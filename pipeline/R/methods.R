# Adapter to the hf.econometrics method API ----------------------------------
#
# Every call into the jump tests goes through this file, so that a change of
# the method API is a change here and nowhere else. The pipeline relies on:
#
#   lm_test(x, options = lm_options(...))        -> LMTest
#     as.data.frame(<LMTest>): one row for the window, with at least n, k, M
#       and p_value (the Gumbel p-value of the window maximum, LM 2012,
#       Theorem 1); other columns are passed through.
#     lm_blocks(<LMTest>): one row per block t_j with at least time and chi;
#       a per-block `p_value` or `xi` is used when present.
#   lm_holm(p_values, alpha)                     -> Holm step-down adjusted p-values
#   ajl_test(x, options = ajl_options(...))      -> AJLTest
#     as.data.frame(<AJLTest>): one row per null hypothesis with columns null
#       and p_value; the no-jump null is the row named in ajl_no_jump_labels.
#   aj_jump_test(x, options = aj_jump_options(...)) -> as.data.frame() rows
#
# `x` is always an hf_series(time, price), so results can carry timestamps,
# symbol and venue. Option lists from config.R are passed to the *_options()
# constructors unchanged; empty lists give the papers' defaults.

as_hf_series <- function(series, info) {
  hf.econometrics::hf_series(series$time, series$price, symbol = info$symbol,
                             venue = info$venue)
}

# Labels under which the AJL no-jump null may appear in as.data.frame().
ajl_no_jump_labels <- c("no_jump", "no-jump", "nojump", "no jump", "no jumps", "no_jumps",
                        "continuous")

is_no_jump_null <- function(labels) {
  tolower(trimws(as.character(labels))) %in% ajl_no_jump_labels
}

# Per-block p-values P(xi > xi_j) under the Gumbel limit of the window
# maximum, so that a block can be compared with the same threshold as the
# window statistic (LM 2012, Theorem 1). Without a `p_value` or `xi` column
# in lm_blocks(), xi_j = (|chi_j| - A_n) / B_n is formed with A_n and B_n
# of LM (2012, eq. 13) from floor(n / (k M)) of the window row.
lm_block_pvalues <- function(blocks, window_row) {
  if ("p_value" %in% names(blocks)) return(as.numeric(blocks$p_value))
  if ("xi" %in% names(blocks)) {
    xi <- as.numeric(blocks$xi)
  } else {
    m <- floor(window_row$n / (window_row$k * window_row$M))
    if (!length(m) || !is.finite(m) || m < 2) return(rep(NA_real_, nrow(blocks)))
    l <- 2 * log(m)
    a_n <- sqrt(l) - (log(pi) + log(log(m))) / (2 * sqrt(l))
    xi <- (abs(as.numeric(blocks$chi)) - a_n) * sqrt(l) # B_n = 1 / sqrt(2 log m)
  }
  -expm1(-exp(-xi))
}

# Holm (1979) step-down adjusted p-values via the package (LM 2012, §6.2).
holm_adjust <- function(p, alpha) {
  res <- hf.econometrics::lm_holm(p, alpha)
  if (!is.numeric(res)) {
    res <- as.data.frame(res)
    col <- intersect(c("p_adjusted", "p_adj", "adjusted_p_value", "p_value_adjusted",
                       "adjusted"), names(res))
    if (!length(col)) {
      stop("lm_holm() returned no adjusted p-values (columns: ",
           paste(names(res), collapse = ", "), ").", call. = FALSE)
    }
    res <- res[[col[1L]]]
  }
  if (length(res) != length(p)) {
    stop(sprintf("lm_holm() returned %d values for %d p-values.", length(res), length(p)),
         call. = FALSE)
  }
  as.numeric(res)
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
      list(row = as.data.frame(test), blocks = as.data.frame(hf.econometrics::lm_blocks(test)))
    }, cfg_lm$on_error)
    if (is.null(res$value)) {
      windows[[j]] <- cbind(wid, status = "error", message = res$error)
      next
    }
    b <- data.table::as.data.table(res$value$blocks)
    row <- with_ids(wid, res$value$row)
    data.table::set(row, j = c("n_blocks", "status", "message"),
                    value = list(nrow(b), "ok", NA_character_))
    windows[[j]] <- row
    if (nrow(b)) {
      p_block <- lm_block_pvalues(b, res$value$row)
      b <- with_ids(wid[, c("unit_id", "date", "symbol", "venue", "window", "window_start")], b)
      data.table::set(b, j = c("block", "p_block"), value = list(seq_len(nrow(b)), p_block))
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
  res <- attempt(function() {
    opts <- do.call(hf.econometrics::ajl_options, cfg_ajl$options)
    out <- as.data.frame(hf.econometrics::ajl_test(as_hf_series(series, info), options = opts))
    if (sum(is_no_jump_null(out$null)) != 1L) {
      stop("ajl_test() rows do not contain exactly one no-jump null (nulls: ",
           paste(out$null, collapse = ", "), "); update ajl_no_jump_labels.", call. = FALSE)
    }
    out
  }, cfg_ajl$on_error)
  if (is.null(res$value)) return(cbind(id, status = "error", message = res$error))
  out <- with_ids(id, res$value)
  data.table::set(out, j = c("no_jump_null", "status", "message"),
                  value = list(is_no_jump_null(out$null), "ok", NA_character_))
  out
}

# Target command: Ait-Sahalia-Jacod jump test on the day grid (optional).
run_aj_jump_unit <- function(prepared, cfg_aj) {
  info <- prepared$info
  series <- prepared$ajl_series
  if (!isTRUE(cfg_aj$enabled) || is.null(series)) return(NULL)
  id <- cbind(unit_ids(info), data.table::data.table(frequency = info$frequency,
                                                      n_grid = nrow(series)))
  res <- attempt(function() {
    opts <- do.call(hf.econometrics::aj_jump_options, cfg_aj$options)
    as.data.frame(hf.econometrics::aj_jump_test(as_hf_series(series, info), options = opts))
  }, cfg_aj$on_error)
  if (is.null(res$value)) return(cbind(id, status = "error", message = res$error))
  out <- with_ids(id, res$value)
  data.table::set(out, j = c("status", "message"), value = list("ok", NA_character_))
  out
}
