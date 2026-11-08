#' Average simultaneous observations
#'
#' Replaces all observations that share a timestamp by one observation whose
#' price is the mean of their prices. With `venue`, observations can be
#' averaged within each venue or pooled across venues.
#'
#' @section Paper mapping:
#' * Lee and Mykland (2012, Section 6.1): "For trades that take place at the
#'   same time and hence have multiple prices at any given time, we take the
#'   averaged observed price, which removes all transactions with zero
#'   duration." This is `pool = "observations"` without `venue`.
#' * Saef et al. (2021, Section 4): "we aggregate the prices per symbol over
#'   the different exchanges and take the mean price for each point in time
#'   in case of simultaneous observations". This is `pool = "observations"`
#'   (or `"venues"`) on the trades of all venues of one symbol.
#'
#' @section Implementation choices:
#' * Timestamps are simultaneous only if they are identical. To treat, say,
#'   all trades within the same millisecond as simultaneous, round `time`
#'   before calling. Round up (ceiling), not down: a rounded-down label would
#'   precede the trades it summarises, and previous-tick sampling would then
#'   look ahead.
#' * Prices are averaged arithmetically, as both papers state.
#' * Pooling across venues weights every trade equally by default, as in
#'   version 0.1.0 of the companion code. `pool = "venues"` first averages
#'   within each venue and then weights venues equally, so that a venue that
#'   prints many fills at one timestamp does not dominate the pooled price.
#'
#' @param time Observation times, `POSIXct` or numeric, without missing
#'   values. They need not be sorted.
#' @param price Prices, a finite numeric vector of the same length.
#' @param venue Optional venue labels, a character vector of the same length.
#' @param pool How to combine observations at the same time:
#'   `"observations"` averages all of them; `"venues"` averages within each
#'   venue, then across venues; `"none"` averages within each venue and keeps
#'   venues separate (requires `venue`).
#' @return A data frame sorted by time (by venue, then time, for
#'   `pool = "none"`) with columns `time`, `price`, `n_obs` (number of
#'   observations averaged), plus `n_venues` (number of distinct venues) when
#'   `venue` is given and pooled, or `venue` for `pool = "none"`.
#' @references
#' Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and
#' market microstructure noise. *Journal of Econometrics* 168(2), 396-406.
#'
#' Saef, D., Nagy, O., Sizov, S. and Hardle, W. K. (2021). Understanding
#' jumps in high frequency digital asset markets. arXiv:2110.09429.
#' @examples
#' t <- as.POSIXct("2022-03-04 00:00:00", tz = "UTC") + c(0, 0, 1, 1, 1)
#' aggregate_simultaneous(t, c(100, 102, 101, 103, 105))
#' aggregate_simultaneous(t, c(100, 102, 101, 103, 105),
#'                        venue = c("a", "b", "a", "a", "b"), pool = "venues")
#' @export
aggregate_simultaneous <- function(time, price, venue = NULL,
                                   pool = c("observations", "venues", "none")) {
  pool <- check_choice(pool[1L], c("observations", "venues", "none"), "pool")
  ti <- time_info(time, "time", sorted = FALSE)
  price <- check_numeric(price, "price", min_length = 0L)
  n <- length(ti$value)
  if (length(price) != n) {
    hf_abort("`price` (length %d) must have the same length as `time` (%d).", length(price), n)
  }
  if (!is.null(venue)) {
    if (!is.character(venue) || length(venue) != n || anyNA(venue)) {
      hf_abort("`venue` must be a character vector without NA and of the same length as `time`.")
    }
  } else if (pool != "observations") {
    hf_abort("`pool = \"%s\"` requires `venue`.", pool)
  }
  if (!n) {
    res <- data.frame(time = as_time_like(numeric(), ti), price = numeric(), n_obs = integer())
    if (!is.null(venue)) {
      if (pool == "none") res$venue <- character() else res$n_venues <- integer()
    }
    return(res)
  }

  if (is.null(venue) || pool == "observations") {
    out <- average_runs(ti$value, price)
    res <- data.frame(time = as_time_like(out$time, ti), price = out$price,
                      n_obs = out$n_obs)
    if (!is.null(venue)) res$n_venues <- count_distinct_runs(ti$value, venue)
    return(res)
  }
  # Within-venue averages first.
  ord <- order(venue, ti$value)
  v <- venue[ord]
  tv <- ti$value[ord]
  p <- price[ord]
  first <- c(TRUE, tv[-1L] != tv[-n] | v[-1L] != v[-n])
  g <- cumsum(first)
  cnt <- tabulate(g, nbins = g[n])
  within <- data.frame(venue = v[first], time = tv[first],
                       price = as.double(rowsum(p, g, reorder = FALSE)) / cnt,
                       n_obs = cnt, stringsAsFactors = FALSE)
  if (pool == "none") {
    within$time <- as_time_like(within$time, ti)
    rownames(within) <- NULL
    return(within[c("time", "price", "n_obs", "venue")])
  }
  # pool = "venues": equal weight per venue at each time.
  across <- average_runs(within$time, within$price)
  n_obs <- as.integer(rowsum(within$n_obs, match(within$time, across$time), reorder = TRUE))
  data.frame(time = as_time_like(across$time, ti), price = across$price,
             n_obs = n_obs, n_venues = across$n_obs)
}

# Mean of `p` over runs of identical `t` after sorting by `t`.
average_runs <- function(t, p) {
  n <- length(t)
  if (!n) return(list(time = numeric(), price = numeric(), n_obs = integer()))
  ord <- order(t)
  t <- t[ord]
  p <- p[ord]
  first <- c(TRUE, t[-1L] != t[-n])
  g <- cumsum(first)
  cnt <- tabulate(g, nbins = g[n])
  list(time = t[first], price = as.double(rowsum(p, g, reorder = FALSE)) / cnt,
       n_obs = cnt)
}

# Number of distinct venues at each distinct time (times in sorted order).
count_distinct_runs <- function(t, venue) {
  n <- length(t)
  if (!n) return(integer())
  ord <- order(t, venue)
  t <- t[ord]
  v <- venue[ord]
  new_time <- c(TRUE, t[-1L] != t[-n])
  new_pair <- new_time | c(TRUE, v[-1L] != v[-n])
  as.integer(rowsum(as.integer(new_pair), cumsum(new_time), reorder = FALSE))
}

# Regular grid start, start + interval, ..., end; (end - start) must be a
# whole multiple of the interval.
make_grid <- function(frame, interval) {
  span <- frame[["end"]] - frame[["start"]]
  J <- span / interval
  if (abs(J - round(J)) > 1e-9 * max(1, J)) {
    hf_abort("`end - start` (%s s) must be a whole multiple of `interval` (%s s).",
             format(span), format(interval))
  }
  frame[["start"]] + (0:round(J)) * interval
}

#' Previous-tick sampling on a regular grid
#'
#' Samples an irregularly observed price on the grid
#' \eqn{g_j = \mathrm{start} + j \cdot \mathrm{interval}},
#' \eqn{j = 0, \dots, J}, with \eqn{g_J = \mathrm{end}}. The price at
#' \eqn{g_j} is the last observed price at or before \eqn{g_j}. The grid
#' includes both ends, so a day sampled every second has 86,401 grid points
#' and 86,400 returns, the last of which covers 23:59:59 to 24:00:00.
#'
#' @section Bin convention:
#' Grid point \eqn{g_j} closes the interval \eqn{(g_{j-1}, g_j]}: an
#' observation exactly at \eqn{g_j} belongs to \eqn{g_j}, not to
#' \eqn{g_{j+1}}. `n_obs` counts the observations in that interval. For
#' \eqn{g_0} it counts the observations at or before `start`. The price at a
#' grid point whose interval is empty is the previous price carried forward,
#' that is, last-price imputation. Grid points before the first observation
#' have price `NA`. Observations before `start` therefore feed the price at
#' \eqn{g_0}; observations after `end` are ignored.
#'
#' @section Paper mapping:
#' Saef et al. (2021, Section 5): for the Ait-Sahalia, Jacod and Li (2012)
#' test, "we sample data either at frequencies of 1, 5, 10, or 15 seconds. To
#' maintain a regular structure, we impute missing observations by inserting
#' the last observed price." The rows with `n_obs > 0` form the aggregated
#' series without imputation (the last price in each occupied interval).
#'
#' @section Implementation choices:
#' * The grid covers `[start, end]` completely. Version 0.1.0 aligned
#'   timestamps up to the next bin, dropped the last second(s) of the day and
#'   left the first grid point empty.
#' * When several observations share a timestamp, the last one in input
#'   order is used. Call [aggregate_simultaneous()] first to average them.
#' * Leading grid points without a previous observation stay `NA`. Use
#'   [locf()] with `fill_leading = TRUE` to fill them with the first observed
#'   price.
#'
#' @param time Observation times, `POSIXct` or numeric seconds, sorted.
#' @param price Prices (or log prices), numeric, same length as `time`.
#' @param interval Grid spacing: seconds, a `difftime`, or a string such as
#'   `"5 secs"`.
#' @param start,end Grid bounds: `POSIXct` or ISO-8601 strings (numbers if
#'   `time` is numeric). By default, the calendar day(s) of the observations
#'   in the time zone of `time`: from midnight before the first observation
#'   to midnight after the last. `end - start` must be a whole multiple of
#'   `interval`.
#' @return A data frame with one row per grid point: `time`, `price` and
#'   `n_obs`.
#' @references
#' Saef, D., Nagy, O., Sizov, S. and Hardle, W. K. (2021). Understanding
#' jumps in high frequency digital asset markets. arXiv:2110.09429.
#'
#' Ait-Sahalia, Y., Jacod, J. and Li, J. (2012). Testing for jumps in noisy
#' high frequency data. *Journal of Econometrics* 168(2), 207-222.
#' @examples
#' t0 <- as.POSIXct("2022-03-04", tz = "UTC")
#' g <- sample_previous_tick(t0 + c(0.2, 1, 3.5), c(100, 101, 102), 1,
#'                           start = t0, end = t0 + 5)
#' g
#' @export
sample_previous_tick <- function(time, price, interval, start = NULL, end = NULL) {
  ti <- time_info(time, "time")
  price <- check_numeric(price, "price", min_length = 0L)
  if (length(price) != length(ti$value)) {
    hf_abort("`price` (length %d) must have the same length as `time` (%d).",
             length(price), length(ti$value))
  }
  interval <- as_seconds(interval, "interval")
  grid <- make_grid(resolve_frame(start, end, ti), interval)
  cnt <- findInterval(grid, ti$value)
  p <- rep(NA_real_, length(grid))
  p[cnt > 0L] <- price[cnt[cnt > 0L]]
  data.frame(time = as_time_like(grid, ti), price = p, n_obs = diff(c(0L, cnt)))
}

#' Share of grid intervals that contain an observation
#'
#' Splits `[start, end]` into intervals \eqn{(g_{j-1}, g_j]} of length
#' `interval` (the convention of [sample_previous_tick()]) and returns the
#' share of intervals with at least one observation.
#'
#' @inheritParams sample_previous_tick
#' @return A single number in \eqn{[0, 1]}.
#' @examples
#' t0 <- as.POSIXct("2022-03-04", tz = "UTC")
#' grid_coverage(t0 + c(0.5, 1.5, 1.7), 1, start = t0, end = t0 + 4) # 0.5
#' @export
grid_coverage <- function(time, interval, start = NULL, end = NULL) {
  ti <- time_info(time, "time")
  interval <- as_seconds(interval, "interval")
  coverage_of(ti, interval, resolve_frame(start, end, ti))
}

coverage_of <- function(ti, interval, frame) {
  grid <- make_grid(frame, interval)
  counts <- diff(findInterval(grid, ti$value))
  mean(counts > 0L)
}

#' Choose the sampling frequency by grid coverage
#'
#' Returns the smallest candidate interval at which at least `min_coverage`
#' of the grid intervals contain an observation, following Saef et al.
#' (2021, Section 4).
#'
#' @section Paper mapping:
#' Saef et al. (2021, Section 4): ticks are "aggregated to frequencies of 1,
#' 5, 10, or 15 seconds for testing, depending on whether or not the observed
#' interval contains at least 95% of the observations at these frequencies. A
#' dataset aggregated to 1 second intervals would have to have at least
#' 0.95 * 86,400 = 82,080 observations. If the dataset had a frequency of
#' less than of 15 seconds, we concluded that the respective data is not
#' 'high frequency' and therefore omitted the data." `candidates` and
#' `min_coverage` default to these values. `NA` marks a series that is not
#' high frequency.
#'
#' @section Implementation choices:
#' An observation of a series aggregated to interval \eqn{f} is an occupied
#' interval, so the rule counts occupied intervals ([grid_coverage()]), not
#' raw ticks. Version 0.1.0 compared the raw tick count with
#' \eqn{0.95 \cdot 86400 / f}: 90,000 ticks bunched into 18,000 seconds
#' passed the 1-second rule and produced a grid with 79% zero returns.
#'
#' @inheritParams sample_previous_tick
#' @param candidates Candidate intervals in seconds.
#' @param min_coverage Required share of occupied intervals, in (0, 1].
#' @return The chosen interval in seconds, or `NA` if no candidate qualifies.
#'   The attribute `"coverage"` holds the coverage of every candidate.
#' @references
#' Saef, D., Nagy, O., Sizov, S. and Hardle, W. K. (2021). Understanding
#' jumps in high frequency digital asset markets. arXiv:2110.09429.
#' @examples
#' t0 <- as.POSIXct("2022-03-04", tz = "UTC")
#' # one trade every 2 seconds: 1 s fails, 5 s qualifies
#' choose_frequency(t0 + seq(1, 86399, by = 2))
#' @export
choose_frequency <- function(time, candidates = c(1, 5, 10, 15),
                             min_coverage = 0.95, start = NULL, end = NULL) {
  ti <- time_info(time, "time")
  candidates <- check_numeric(candidates, "candidates")
  if (any(candidates <= 0)) hf_abort("`candidates` must be positive.")
  check_number(min_coverage, "min_coverage", lower = 0, upper = 1, open = c(TRUE, FALSE))
  candidates <- sort(unique(candidates))
  frame <- resolve_frame(start, end, ti)
  cov <- vapply(candidates, function(f) coverage_of(ti, f, frame), numeric(1))
  ok <- cov >= min_coverage - 1e-12
  structure(if (any(ok)) candidates[ok][1L] else NA_real_,
            coverage = stats::setNames(cov, as.character(candidates)))
}
