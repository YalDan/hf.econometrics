#' Log returns aligned with their observations
#'
#' Returns \eqn{r_i = \log p_i - \log p_{i-1}} for \eqn{i = 2, \dots, n} and
#' `NA` for \eqn{i = 1}, so that `r[i]` belongs to observation `i`.
#'
#' @param price Strictly positive prices; `NA` is allowed and propagates.
#' @return A numeric vector of the same length as `price`.
#' @examples
#' log_returns(c(100, 101, 100))
#' @export
log_returns <- function(price) {
  price <- check_numeric(price, "price", min_length = 0L, finite = FALSE)
  if (any(price <= 0, na.rm = TRUE)) hf_abort("`price` must be strictly positive.")
  if (!length(price)) return(numeric())
  c(NA_real_, diff(log(price)))
}

#' Carry the last observed price forward
#'
#' Replaces each missing value with the last preceding non-missing value. Use
#' it on prices (or log prices), never on returns: a carried-forward return
#' is a return that did not happen, whereas a carried-forward price is the
#' last-price imputation of Saef et al. (2021, Section 5) and implies a zero
#' return.
#'
#' @param x A numeric vector of prices with missing values.
#' @param fill_leading Fill leading missing values with the first observed
#'   value? Version 0.1.0 of the companion code did this to keep a full grid.
#' @return A numeric vector of the same length.
#' @references
#' Saef, D., Nagy, O., Sizov, S. and Hardle, W. K. (2021). Understanding
#' jumps in high frequency digital asset markets. arXiv:2110.09429.
#' @examples
#' locf(c(NA, 100, NA, NA, 101, NA))
#' locf(c(NA, 100, NA), fill_leading = TRUE)
#' @export
locf <- function(x, fill_leading = FALSE) {
  x <- check_numeric(x, "x", min_length = 0L, finite = FALSE)
  check_flag(fill_leading, "fill_leading")
  ok <- !is.na(x)
  idx <- cummax(seq_along(x) * ok)
  out <- rep(NA_real_, length(x))
  out[idx > 0L] <- x[idx[idx > 0L]]
  if (fill_leading && any(ok)) {
    first <- which(ok)[1L]
    out[seq_len(first - 1L)] <- x[first]
  }
  out
}

# Shared input checks of the filters.
check_filter_input <- function(time, price) {
  ti <- time_info(time, "time")
  price <- check_numeric(price, "price", min_length = 0L)
  if (length(price) != length(ti$value)) {
    hf_abort("`price` (length %d) must have the same length as `time` (%d).",
             length(price), length(ti$value))
  }
  if (any(price <= 0)) hf_abort("`price` must be strictly positive.")
  list(ti = ti, price = price)
}

# Assembles the common return value of the filters.
filter_result <- function(ti, price, flag, r, statistic, filter) {
  keep <- !flag
  kept <- price[keep]
  list(
    keep = keep,
    series = data.frame(time = as_time_like(ti$value[keep], ti), price = kept,
                        log_return = log_returns(kept)),
    removed = data.frame(index = which(flag), time = as_time_like(ti$value[flag], ti),
                         price = price[flag], log_return = r[flag],
                         statistic = statistic[flag],
                         filter = rep(filter, sum(flag)), stringsAsFactors = FALSE)
  )
}

#' Remove observations whose returns lie outside a symmetric band
#'
#' Flags every log return with
#' \deqn{|r_i - \bar r| > n_{sd} \cdot s_r,}
#' where \eqn{\bar r} and \eqn{s_r} are the mean and standard deviation of all
#' returns, removes the observation \eqn{p_i} at which each flagged return
#' arrives, and recomputes the returns from the prices that remain.
#'
#' @section Paper mapping:
#' * Lee and Mykland (2012, Section 6.1): "we remove obvious outliers and
#'   only keep data with log returns within the range of 7 standard
#'   deviations around its mean" (`n_sd = 7`).
#' * Saef et al. (2021, Section 4): "we remove bounceback outliers and returns
#'   outside of a range of 10 standard deviations" (`n_sd = 10`), with
#'   minima and maxima affected alike.
#'
#' @section Implementation choices:
#' * The band is symmetric around the mean. Version 0.1.0 cut only the upper
#'   tail (`log_ret >= 10 * sd`).
#' * The removed unit is the observation, not the return: "keep data with log
#'   returns within the range" keeps observation \eqn{i} if and only if
#'   \eqn{r_i} lies in the band. Prices and returns stay consistent, because
#'   the returns are recomputed from the kept prices. Version 0.1.0 replaced
#'   a removed return with the previous return and left the price in place.
#' * One pass: mean, standard deviation and flags come from the returns of
#'   the input. Iterating would cascade: after a genuine level shift beyond
#'   the band, every removal hands the shift to the next return. A level
#'   shift is therefore not removed; it moves to the next kept observation,
#'   where the jump tests still see it.
#' * An isolated spike flags its incoming and its outgoing return, so the
#'   spike and the observation after it are removed. Running
#'   [filter_bouncebacks()] first removes the spike alone, which is the order
#'   of Saef et al. (2021, Section 4).
#' * The first observation has no return and is never removed. The moments
#'   include zero returns; neither paper excludes them.
#'
#' @param time Observation times (`POSIXct` or numeric), sorted.
#' @param price Strictly positive prices in time order.
#' @param n_sd Half-width of the band in standard deviations: 7 in Lee and
#'   Mykland (2012), 10 in Saef et al. (2021).
#' @return A list with
#'   * `keep`: logical, `TRUE` for the observations that are kept;
#'   * `series`: data frame of the kept observations (`time`, `price`,
#'     recomputed `log_return`);
#'   * `removed`: data frame of the removed observations (`index` in the
#'     input, `time`, `price`, the flagged `log_return`, its z-score
#'     `statistic`, and `filter = "outlier"`);
#'   * `bounds`: the band `c(lower, upper)` for returns.
#' @references
#' Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and
#' market microstructure noise. *Journal of Econometrics* 168(2), 396-406.
#'
#' Saef, D., Nagy, O., Sizov, S. and Hardle, W. K. (2021). Understanding
#' jumps in high frequency digital asset markets. arXiv:2110.09429.
#' @examples
#' set.seed(1)
#' p <- 100 * exp(cumsum(c(0, rnorm(1999, 0, 1e-4))))
#' p[1200:2000] <- p[1200:2000] * 1.01 # a level shift beyond the band
#' f <- filter_outliers(seq_along(p), p, n_sd = 10)
#' f$removed                        # observation 1200 is removed ...
#' f$series$log_return[1199]        # ... and the shift moves to the next one
#' @export
filter_outliers <- function(time, price, n_sd) {
  input <- check_filter_input(time, price)
  check_number(n_sd, "n_sd", lower = 0, open = c(TRUE, FALSE))
  price <- input$price
  r <- log_returns(price)
  n <- length(price)
  center <- if (n >= 2L) mean(r[-1L]) else NA_real_
  scale <- if (n >= 3L) stats::sd(r[-1L]) else NA_real_
  flag <- if (is.na(scale)) logical(n) else !is.na(r) & abs(r - center) > n_sd * scale
  out <- filter_result(input$ti, price, flag, r, (r - center) / scale, "outlier")
  out$bounds <- c(lower = center - n_sd * scale, upper = center + n_sd * scale)
  out
}

#' Remove bounceback outliers
#'
#' A bounceback is a single observation that jumps away from its neighbours
#' and is immediately reversed. Observation \eqn{i} (\eqn{1 < i < n}) is a
#' bounceback when its incoming return \eqn{a = r_i} and its outgoing return
#' \eqn{b = r_{i+1}} satisfy
#' 1. \eqn{|a| > \mathrm{threshold}} and \eqn{|b| > \mathrm{threshold}}
#'    (both legs are large),
#' 2. \eqn{\mathrm{sign}(b) = -\mathrm{sign}(a)} (the second leg reverses
#'    the first), and
#' 3. \eqn{|a + b| \le \mathrm{tolerance} \cdot \max(|a|, |b|)} (the
#'    reversal brings the price back: the net move from \eqn{p_{i-1}} to
#'    \eqn{p_{i+1}} is at most `tolerance` times the larger leg).
#'
#' Only \eqn{p_i} is removed; the two legs merge into the return
#' \eqn{a + b}.
#'
#' @section Paper mapping:
#' Saef et al. (2021, Section 4) "remove bounceback outliers" without stating
#' a rule; Lee and Mykland (2012, Section 6.1) refer to the bounce-back data
#' errors of Ait-Sahalia, Mykland and Zhang (2011). The rule above is
#' therefore an implementation choice.
#'
#' @section Implementation choices:
#' * The sign condition is required. Version 0.1.0 flagged any pair of large
#'   adjacent returns whose difference exceeded twice the cutoff, so two
#'   large moves in the same direction were deleted as a "bounceback".
#' * `tolerance = 0.5` (default) demands that at least half of the larger leg
#'   is undone, which separates a spike from a jump followed by a partial
#'   retracement. `tolerance = 1` drops condition 3; that is the 0.1.0 pair
#'   rule with the sign check added.
#' * No paper gives a threshold. Version 0.1.0 used 0.001 (a 0.1% move).
#' * One pass on the input returns. Only one-observation spikes are found.
#'   On a grid with last-price imputation a spike can span several grid
#'   points, so filter before imputing.
#' * With prices pooled across venues, a persistent price gap between venues
#'   makes every switch between venues look like a bounceback.
#'
#' @inheritParams filter_outliers
#' @param threshold Minimum absolute log return of both legs, positive.
#' @param tolerance Largest net move as a share of the larger leg, in
#'   \eqn{[0, 1]}.
#' @return A list with `keep`, `series` and `removed` as in
#'   [filter_outliers()]. In `removed`, `log_return` is the incoming leg
#'   \eqn{a} and `statistic` is \eqn{|a + b| / \max(|a|, |b|)}.
#' @references
#' Saef, D., Nagy, O., Sizov, S. and Hardle, W. K. (2021). Understanding
#' jumps in high frequency digital asset markets. arXiv:2110.09429.
#'
#' Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and
#' market microstructure noise. *Journal of Econometrics* 168(2), 396-406.
#'
#' Ait-Sahalia, Y., Mykland, P. A. and Zhang, L. (2011). Ultra high
#' frequency volatility estimation with dependent microstructure noise.
#' *Journal of Econometrics* 160(1), 160-175.
#' @examples
#' p <- c(100, 100.01, 101, 100.02, 100.03, 101, 102, 102.01)
#' filter_bouncebacks(seq_along(p), p, threshold = 0.001)$removed
#' @export
filter_bouncebacks <- function(time, price, threshold, tolerance = 0.5) {
  input <- check_filter_input(time, price)
  check_number(threshold, "threshold", lower = 0, open = c(TRUE, FALSE))
  check_number(tolerance, "tolerance", lower = 0, upper = 1)
  price <- input$price
  n <- length(price)
  r <- log_returns(price)
  flag <- logical(n)
  share <- rep(NA_real_, n)
  if (n >= 3L) {
    i <- 2:(n - 1L)
    a <- r[i]
    b <- r[i + 1L]
    big <- pmax(abs(a), abs(b))
    share[i] <- abs(a + b) / big
    flag[i] <- abs(a) > threshold & abs(b) > threshold & sign(a) == -sign(b) &
      abs(a + b) <= tolerance * big
  }
  filter_result(input$ti, price, flag, r, share, "bounceback")
}
