# Preparation of one unit: aggregation, frequency choice, filters, grid -----
#
# Steps, in order (see README.md for the paper references):
#  1. average simultaneous prices                [LM 2012 §6.1; SNSH 2021 §4]
#  2. choose 1/5/10/15 s by grid coverage         [SNSH 2021 §4]
#  3. LM input: the observed series at that frequency, or all ticks
#  4. bounceback filter, then the n-sd filter     [SNSH 2021 §4; LM 2012 §6.1]
#  5. AJL input: previous-tick grid of the cleaned series over the whole day,
#     i.e. last-price imputation                  [SNSH 2021 §5]

prepare_unit <- function(unit, cfg) {
  info <- unit_info_row(unit)
  day <- day_bounds(unit$date[1L], cfg$units$day_tz)
  ticks <- read_unit_ticks(unit)

  pool <- if (isTRUE(cfg$units$pool_venues)) cfg$units$pool else "observations"
  agg <- hf.econometrics::aggregate_simultaneous(ticks$time, ticks$price, venue = ticks$venue,
                                                 pool = pool)
  info[, n_times := nrow(agg)]

  freq <- hf.econometrics::choose_frequency(agg$time, cfg$frequency$candidates,
                                            cfg$frequency$min_coverage,
                                            start = day$start, end = day$end)
  coverage <- attr(freq, "coverage")
  for (f in names(coverage)) info[, (paste0("coverage_", f, "s")) := coverage[[f]]]
  eligible <- !is.na(freq)
  info[, `:=`(eligible = eligible, frequency = as.numeric(freq))]
  if (!eligible && isTRUE(cfg$frequency$skip_ineligible)) {
    info[, status := "skipped: no candidate frequency reaches the coverage"]
    return(list(info = info, lm_series = NULL, ajl_series = NULL, removed = NULL))
  }
  f_used <- if (eligible) as.numeric(freq) else max(cfg$frequency$candidates)
  info[, frequency := f_used]

  ticks_input <- identical(cfg$lm_input, "ticks")
  base <- if (ticks_input) {
    data.table::data.table(time = agg$time, price = agg$price, n_obs = agg$n_obs)
  } else {
    # One observation per occupied interval (g_{j-1}, g_j], j >= 1, labelled
    # by its end g_j; the grid point g_0 = 00:00:00 closes no interval of
    # the day and is left out.
    g <- hf.econometrics::sample_previous_tick(agg$time, agg$price, f_used,
                                               start = day$start, end = day$end)
    g <- g[-1L, ]
    data.table::as.data.table(g[g$n_obs > 0L, ])
  }
  info[, n_lm_input := nrow(base)]

  removed <- list()
  clean <- base
  if (isTRUE(cfg$filters$bounceback$enabled)) {
    threshold <- cfg$filters$bounceback$threshold
    if (!is.null(cfg$filters$bounceback$threshold_sd) && nrow(clean) > 2L) {
      threshold <- cfg$filters$bounceback$threshold_sd * stats::sd(diff(log(clean$price)))
    }
    info[, bounceback_threshold := threshold]
    fb <- hf.econometrics::filter_bouncebacks(clean$time, clean$price, threshold = threshold,
                                              tolerance = cfg$filters$bounceback$tolerance)
    removed$bounceback <- fb$removed
    clean <- clean[fb$keep]
  }
  if (isTRUE(cfg$filters$outliers$enabled)) {
    fo <- hf.econometrics::filter_outliers(clean$time, clean$price,
                                           n_sd = cfg$filters$outliers$n_sd)
    removed$outlier <- fo$removed
    clean <- clean[fo$keep]
  }
  removed <- data.table::rbindlist(removed, use.names = TRUE)
  info[, `:=`(n_bounceback = sum(removed$filter == "bounceback"),
              n_outlier = sum(removed$filter == "outlier"),
              n_lm = nrow(clean))]

  ajl_series <- NULL
  if (isTRUE(cfg$ajl$enabled)) {
    grid <- hf.econometrics::sample_previous_tick(clean$time, clean$price, f_used,
                                                  start = day$start, end = day$end)
    leading <- sum(cumsum(!is.na(grid$price)) == 0L)
    grid$price <- if (identical(cfg$ajl$leading, "first")) {
      hf.econometrics::locf(grid$price, fill_leading = TRUE)
    } else {
      grid$price
    }
    ajl_series <- data.table::as.data.table(grid)[!is.na(price)]
    info[, `:=`(n_ajl = nrow(ajl_series), n_ajl_leading = leading,
                n_ajl_imputed = sum(ajl_series$n_obs == 0L))]
  }
  info[, status := "ok"]
  if (nrow(removed)) removed <- with_ids(unit_ids(info), removed)
  # Ticks fall into windows [start, end) (LM 2012 §6.1); interval-end labels
  # into (start, end].
  list(info = info, lm_series = clean, lm_closed = if (ticks_input) "left" else "right",
       ajl_series = ajl_series, removed = removed)
}
