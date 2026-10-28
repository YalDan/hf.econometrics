# Combination: multiple testing, LM jumps on AJL jump days, consecutive jumps
#
# Decisions are recomputed from p-values here, after all windows and days
# are known, so that changing `testing` or `consecutive` in config.R reruns
# only these targets. Method output columns are carried through unchanged;
# new columns are attached with data.table::set() so that no method column
# can mask a variable of this file.

# Integer family id per row: one family per series (symbol, venue) or one
# family for everything.
family_id <- function(tab, family) {
  if (identical(family, "series")) {
    as.integer(factor(paste(tab$symbol, tab$venue, sep = "|")))
  } else {
    rep(1L, nrow(tab))
  }
}

# Adjusted window decisions. For every window, `multiplier` is the factor c
# with which the window is tested as c * p <= alpha:
#   none: 1; bonferroni: family size m; holm: m - rank + 1 (the step at which
#   Holm's procedure tests it); blocks: number of blocks in the window.
adjust_lm_windows <- function(windows, cfg_test) {
  w <- data.table::as.data.table(windows)
  if (!nrow(w)) return(w)
  alpha <- cfg_test$alpha
  p_all <- suppressWarnings(as.numeric(w$p_value %||% rep(NA_real_, nrow(w))))
  ok <- w$status == "ok" & is.finite(p_all)
  fam <- family_id(w, cfg_test$lm_family)
  size <- mult <- p_adj <- rep(NA_real_, nrow(w))
  for (g in unique(fam[ok])) {
    i <- which(ok & fam == g)
    p <- p_all[i]
    k <- length(i)
    size[i] <- k
    if (cfg_test$lm_adjust == "none") {
      mult[i] <- 1
      p_adj[i] <- p
    } else if (cfg_test$lm_adjust == "bonferroni") {
      mult[i] <- k
      p_adj[i] <- pmin(1, k * p)
    } else if (cfg_test$lm_adjust == "holm") {
      mult[i] <- k - rank(p, ties.method = "first") + 1
      p_adj[i] <- holm_adjust(p, alpha)
    } else if (cfg_test$lm_adjust == "blocks") {
      mult[i] <- w$n_blocks[i]
      p_adj[i] <- pmin(1, w$n_blocks[i] * p)
    }
  }
  data.table::set(w, j = c("adjustment", "family_size", "multiplier", "p_adjusted", "jump_window"),
                  value = list(cfg_test$lm_adjust, size, mult, p_adj, ok & p_adj <= alpha))
  w
}

# Block decisions: block j of window w is a detection when the window is
# rejected and multiplier_w * p_block <= alpha. The block with the largest
# statistic has p_block equal to the window p-value, so a rejected window
# always contains at least one detection.
flag_lm_blocks <- function(blocks, windows, cfg_test) {
  b <- data.table::as.data.table(blocks)
  if (!nrow(b)) return(b)
  key <- paste(b$unit_id, b$window)
  wkey <- paste(windows$unit_id, windows$window)
  m <- match(key, wkey)
  mult <- windows$multiplier[m]
  rejected <- windows$jump_window[m] %in% TRUE
  data.table::set(b, j = c("multiplier", "detected"),
                  value = list(mult, rejected & mult * b$p_block <= cfg_test$alpha))
  b
}

# AJL day decisions on the no-jump null: Bonferroni across the days of a
# family, or none. `jump_day` = the no-jump null is rejected.
adjust_ajl_tests <- function(tests, cfg_test) {
  a <- data.table::as.data.table(tests)
  if (!nrow(a)) return(a)
  alpha <- cfg_test$alpha
  p_all <- suppressWarnings(as.numeric(a$p_value %||% rep(NA_real_, nrow(a))))
  ok <- a$status == "ok" & (a$no_jump_null %||% rep(FALSE, nrow(a))) %in% TRUE & is.finite(p_all)
  fam <- family_id(a, cfg_test$ajl_family)
  size <- p_adj <- rep(NA_real_, nrow(a))
  for (g in unique(fam[ok])) {
    i <- which(ok & fam == g)
    size[i] <- length(i)
    p_adj[i] <- if (cfg_test$ajl_adjust == "bonferroni") pmin(1, length(i) * p_all[i]) else p_all[i]
  }
  data.table::set(a, j = c("adjustment", "family_size", "p_adjusted", "jump_day"),
                  value = list(cfg_test$ajl_adjust, size, p_adj, ok & p_adj <= alpha))
  a
}

# LM detections, the AJL day filter and the removal of consecutive jumps.
combine_detections <- function(blocks, ajl, cfg_test, cfg_cons) {
  b <- data.table::as.data.table(blocks)
  if (!("detected" %in% names(b))) {
    b <- data.table::data.table(unit_id = character(), date = data.table::as.IDate(character()),
                                symbol = character(), venue = character(), window = integer(),
                                window_start = .POSIXct(numeric(), tz = "UTC"),
                                block = integer(), p_block = numeric(), multiplier = numeric(),
                                detected = logical())
  }
  # Locals used inside `[` start with "." so that no method column masks them.
  .sel <- which(b$detected %in% TRUE)
  d <- b[.sel]
  # AJL jump day of the same unit (date, symbol, venue).
  ajl_day <- rep(NA, nrow(d))
  ajl_p <- rep(NA_real_, nrow(d))
  if (!is.null(ajl) && nrow(ajl) && "jump_day" %in% names(ajl)) {
    .null_rows <- which(ajl$no_jump_null %in% TRUE & ajl$status == "ok")
    rows <- ajl[.null_rows]
    m <- match(d$unit_id, rows$unit_id)
    ajl_day <- rows$jump_day[m]
    ajl_p <- rows$p_adjusted[m]
  }
  # Consecutive detections: keep the first of each cluster.
  kept <- rep(TRUE, nrow(d))
  if (isTRUE(cfg_cons$enabled)) {
    by_blocks <- identical(cfg_cons$unit, "blocks")
    grp <- if (by_blocks) paste(d$unit_id, d$window) else d$unit_id
    for (g in unique(grp)) {
      i <- which(grp == g)
      index <- if (by_blocks) d$block[i] else d$time[i]
      kept[i] <- hf.econometrics::drop_consecutive_flags(index, rep(TRUE, length(i)),
                                                         range = cfg_cons$range,
                                                         anchor = cfg_cons$anchor)
    }
  }
  accepted <- kept & (if (isTRUE(cfg_test$require_ajl_day)) ajl_day %in% TRUE else TRUE)
  data.table::set(d, j = c("ajl_jump_day", "ajl_p_adjusted", "consecutive_kept", "accepted"),
                  value = list(ajl_day, ajl_p, kept, accepted))
  d
}

# Counts for a quick look at a run (also written to run_summary.csv).
summarise_run <- function(ingest, units, windows, ajl, detections) {
  n <- function(x) if (is.null(x)) 0L else nrow(x)
  count <- function(x, col) if (is.null(x) || !nrow(x) || !(col %in% names(x))) 0L else sum(x[[col]] %in% TRUE)
  status <- function(x, value) if (is.null(x) || !nrow(x)) 0L else sum(x$status == value)
  .null_rows <- if (n(ajl) && "no_jump_null" %in% names(ajl)) which(ajl$no_jump_null %in% TRUE)
  ajl_null <- if (length(.null_rows)) ajl[.null_rows] else NULL
  data.table::data.table(
    measure = c("files", "trades_read", "trades_kept", "units", "units_tested", "units_skipped",
                "lm_windows", "lm_window_errors", "lm_windows_rejected", "lm_detections",
                "ajl_tests", "ajl_errors", "ajl_jump_days",
                "detections_on_ajl_days", "detections_after_consecutive", "jumps_accepted"),
    value = c(n(ingest), sum(ingest$n_rows), sum(ingest$n_kept), n(units), status(units, "ok"),
              n(units) - status(units, "ok"),
              n(windows), status(windows, "error"), count(windows, "jump_window"), n(detections),
              n(ajl_null), status(ajl, "error"), count(ajl_null, "jump_day"),
              count(detections, "ajl_jump_day"), count(detections, "consecutive_kept"),
              count(detections, "accepted"))
  )
}
