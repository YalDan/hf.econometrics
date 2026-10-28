# Adapter to the hf.econometrics method API ----------------------------------
#
# Every call into the jump tests goes through this file, so that a change of
# the method API is a change here and nowhere else. Rows are the package's own
# outputs; the adapter adds identifiers, run status and the few quantities the
# package does not report (marked "not in the package" below).
#
#   lm_test(x, options = lm_options(...))                    -> <LMTest>
#     as.data.frame(): one row per window: method, start, end (first and
#       last observation), n (increments), k, M, C, C_effective, n_blocks,
#       q_hat, iv (sigma^2 T of the window), iv_method, variance, V_n, A_n,
#       B_n (eq. 13), max_chi, statistic (xi_n, eq. 14), p_value
#       (1 - exp(-exp(-xi_n)), Theorem 1), alpha, threshold, reject, n_flagged.
#       alpha, threshold, reject and n_flagged are the package's unadjusted
#       decisions at lm_options(alpha); the pipeline decides in combine.R.
#     @iv: the <IVEstimate> behind `iv`; its as.data.frame() `volatility`,
#       sqrt(sigma^2 T), is written as sigma_hat.
#     lm_blocks(): one row per block difference L(t_j): index (tick j of t_j,
#       from 0), time (t_j), time_end (last observation in L(t_j)), P_hat, L,
#       chi (X(t_j), eq. 10) and jump (-1/0/1 at lm_options(alpha)).
#   lm_holm(p, alpha)  -> data.frame(window, p_value, p_adjusted, reject)
#   ajl_test(x, options = ajl_options(...))                  -> <AJLTest>
#     as.data.frame(): one row per null tested; `null` is "no_jump" (eq. 3.26,
#       rejected in the lower tail, p_value = Phi(z)) or "jump" (eq. 3.27,
#       upper tail, p_value = 1 - Phi(z)). z, critical_value, p_value and
#       reject are NA, with a warning, when the variance estimate is not
#       positive.
#   aj_jump_test(x, options = aj_jump_options(...))          -> <AJJumpTest>
#     as.data.frame(): one row; both nulls in columns (*_no_jumps, *_jumps).
#
# `x` is an hf_series(time, price). Option lists from config.R go to the
# *_options() constructors unchanged (check_config() builds them once when
# _targets.R is loaded, so invalid options fail early); empty lists give the
# papers' defaults. Warnings raised by a test are recorded in its rows
# (column `warning`), not printed.

as_hf_series <- function(series, info) {
  hf.econometrics::hf_series(series$time, series$price, symbol = info$symbol,
                             venue = info$venue)
}

# Label of the no-jump null in as.data.frame(<AJLTest>)$null.
ajl_no_jump <- "no_jump"

# One row per window: as.data.frame(<LMTest>), with sigma_hat after iv.
lm_window_table <- function(test) {
  row <- as.data.frame(test)
  sigma_hat <- if (is.null(test@iv)) NA_real_ else as.data.frame(test@iv)$volatility
  at <- match("iv", names(row))
  cbind(row[seq_len(at)], sigma_hat = sigma_hat, row[-seq_len(at)])
}

# One row per block: lm_blocks(<LMTest>), plus p_block (not in the package):
# the Gumbel p-value of Theorem 1 evaluated at xi_j = (|chi_j| - A_n) / B_n
# with the A_n and B_n of the window, i.e. the window p-value that block j
# would have if its |chi| were the maximum. The block with the largest |chi|
# has p_block = p_value of the window.
lm_block_table <- function(test, row) {
  b <- data.table::as.data.table(hf.econometrics::lm_blocks(test))
  xi <- (abs(b$chi) - row$A_n) / row$B_n
  data.table::set(b, j = "p_block", value = -expm1(-exp(-xi)))
  b
}

# Holm (1979) step-down adjusted p-values (LM 2012, §6.2).
holm_adjust <- function(p, alpha) {
  hf.econometrics::lm_holm(p, alpha)$p_adjusted
}

# Target command: Lee-Mykland test in every window of one unit.
run_lm_unit <- function(prepared, cfg_lm) {
  info <- prepared$info
  series <- prepared$lm_series
  if (is.null(series) || !nrow(series)) return(list(windows = NULL, blocks = NULL))
  day <- day_bounds(info$date, cfg_lm$day_tz)
  w <- hf.econometrics::split_windows(series$time, width = cfg_lm$window, origin = day$start,
                                      closed = prepared$lm_closed %||% "left")
  opts <- do.call(hf.econometrics::lm_options, cfg_lm$options)
  windows <- blocks <- vector("list", nrow(w))
  for (j in seq_len(nrow(w))) {
    rows <- w$from[j]:w$to[j]
    wid <- cbind(unit_ids(info), data.table::data.table(
      window = w$window[j], window_start = w$start[j], window_end = w$end[j], n_obs = w$n[j]))
    res <- attempt(function() {
      test <- hf.econometrics::lm_test(as_hf_series(series[rows], info), options = opts)
      row <- lm_window_table(test)
      list(row = row, blocks = lm_block_table(test, row))
    }, cfg_lm$on_error)
    if (is.null(res$value)) {
      windows[[j]] <- cbind(wid, status = "error", message = res$error, warning = res$warning)
      next
    }
    row <- with_ids(wid, res$value$row)
    data.table::set(row, j = c("status", "message", "warning"),
                    value = list("ok", NA_character_, res$warning))
    windows[[j]] <- row
    b <- res$value$blocks
    if (nrow(b)) {
      b <- with_ids(wid[, c("unit_id", "date", "symbol", "venue", "window", "window_start")], b)
      data.table::set(b, j = "block", value = seq_len(nrow(b)))
      blocks[[j]] <- b
    }
  }
  list(windows = data.table::rbindlist(windows, fill = TRUE),
       blocks = data.table::rbindlist(blocks, fill = TRUE))
}

# Target command: Ait-Sahalia-Jacod-Li test on the day grid of one unit.
run_ajl_unit <- function(prepared, cfg_ajl) {
  info <- prepared$info
  series <- prepared$ajl_series
  if (!isTRUE(cfg_ajl$enabled) || is.null(series)) return(NULL)
  id <- cbind(unit_ids(info), data.table::data.table(frequency = info$frequency,
                                                      n_grid = nrow(series)))
  opts <- do.call(hf.econometrics::ajl_options, cfg_ajl$options)
  res <- attempt(function() {
    as.data.frame(hf.econometrics::ajl_test(as_hf_series(series, info), options = opts))
  }, cfg_ajl$on_error)
  if (is.null(res$value)) {
    return(cbind(id, status = "error", message = res$error, warning = res$warning))
  }
  out <- with_ids(id, res$value)
  data.table::set(out, j = c("no_jump_null", "status", "message", "warning"),
                  value = list(out$null == ajl_no_jump, "ok", NA_character_, res$warning))
  out
}

# Target command: Ait-Sahalia-Jacod jump test on the day grid (optional).
run_aj_jump_unit <- function(prepared, cfg_aj) {
  info <- prepared$info
  series <- prepared$ajl_series
  if (!isTRUE(cfg_aj$enabled) || is.null(series)) return(NULL)
  id <- cbind(unit_ids(info), data.table::data.table(frequency = info$frequency,
                                                      n_grid = nrow(series)))
  opts <- do.call(hf.econometrics::aj_jump_options, cfg_aj$options)
  res <- attempt(function() {
    as.data.frame(hf.econometrics::aj_jump_test(as_hf_series(series, info), options = opts))
  }, cfg_aj$on_error)
  if (is.null(res$value)) {
    return(cbind(id, status = "error", message = res$error, warning = res$warning))
  }
  out <- with_ids(id, res$value)
  data.table::set(out, j = c("status", "message", "warning"),
                  value = list("ok", NA_character_, res$warning))
  out
}
