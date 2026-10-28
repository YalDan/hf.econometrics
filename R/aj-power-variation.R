# Shared building blocks for the Ait-Sahalia and Jacod (2009a, 2009b, 2011,
# 2012) methods: subsampled increments, (truncated) power variations,
# truncation levels, and the moment constants of AJ (2009a).

#' Truncated realized power variation of Ait-Sahalia and Jacod
#'
#' Computes the realized power variation of Ait-Sahalia and Jacod (2012,
#' eq. 7), sampled at \eqn{k\Delta_n},
#' \deqn{B(p, u_n, k\Delta_n) = \sum_{i=1}^{[n/k]} |X_{ik\Delta_n} -
#'       X_{(i-1)k\Delta_n}|^p \, 1\{|X_{ik\Delta_n} - X_{(i-1)k\Delta_n}|
#'       \le u_n\},}
#' or, with `upper = TRUE`, the upward variation
#' \eqn{U(p, u_n, k\Delta_n)} of their eq. (8) that keeps the increments
#' larger than \eqn{u_n}. With `p = 0` and `upper = TRUE` this is the count
#' \eqn{U(0, u_n, \Delta_n)} of their eq. (10), the building block of the
#' activity index of Ait-Sahalia and Jacod (2009b, eq. 14).
#'
#' @param x Log prices observed every \eqn{\Delta_n}: a numeric vector or an
#'   [HFSeries]. Observations are assumed equally spaced.
#' @param p Power, a non-negative number.
#' @param truncation Truncation level \eqn{u_n > 0}. `Inf` (the default)
#'   gives the untruncated variation \eqn{B(p, \infty, k\Delta_n)} of AJ
#'   (2009a, eq. 9). See [aj_truncation()] for the levels used in the papers.
#' @param k Sampling multiple: increments are taken between every `k`-th
#'   price, \eqn{X_{ik\Delta_n} - X_{(i-1)k\Delta_n}}. The increments are
#'   built from the subsampled prices, not by keeping every `k`-th one-step
#'   return.
#' @param upper Keep the increments with \eqn{|\Delta X| > u_n} instead of
#'   \eqn{|\Delta X| \le u_n}?
#' @return A single number.
#' @section Paper mapping:
#' * AJ (2012, eq. 6): increments \eqn{\Delta^n_i X = X_{i\Delta_n} -
#'   X_{(i-1)\Delta_n}}; at the coarser scale, AJ (2009a, Section 3.3) take
#'   "the increments of X between successive multiples of \eqn{k\Delta_n}".
#' * AJ (2012, eq. 7) and AJ (2011, eq. 32): \eqn{B(p, u_n, \Delta_n)},
#'   `upper = FALSE`.
#' * AJ (2012, eqs. 8 and 10): \eqn{U(p, u_n, \Delta_n)}, `upper = TRUE`.
#' @section Implementation choices:
#' * The coarse grid starts at the first observation, as in AJ (2009a,
#'   Section 3.3). When `n` is not a multiple of `k`, the last `n %% k`
#'   one-step increments are not used at scale \eqn{k\Delta_n}, which is the
#'   \eqn{[t/(k\Delta_n)]} upper summation limit of the papers.
#' @references
#' Ait-Sahalia, Y. and Jacod, J. (2009a). Testing for jumps in a discretely
#' observed process. *Annals of Statistics* 37(1), 184-222.
#'
#' Ait-Sahalia, Y. and Jacod, J. (2012). Analyzing the spectrum of asset
#' returns: jump and volatility components in high frequency data.
#' *Journal of Economic Literature* 50(4), 1007-1050. NBER Working Paper
#' 15808 (2010).
#' @seealso [aj_truncation()], [aj_jump_test()], [aj_spectrum()]
#' @examples
#' x <- cumsum(c(0, rnorm(1000, 0, 1e-3)))
#' aj_power_variation(x, p = 4)
#' aj_power_variation(x, p = 4, k = 2) / aj_power_variation(x, p = 4) # ~ 2
#' @export
aj_power_variation <- function(x, p, truncation = Inf, k = 1L, upper = FALSE) {
  y <- log_prices(x)
  check_number(p, "p", lower = 0)
  check_number(truncation, "truncation", lower = 0, open = c(TRUE, FALSE))
  k <- check_count(k, "k", min = 1L, max = length(y) - 1L)
  check_flag(upper, "upper")
  aj_power_sum(abs(aj_increments(y, k)), p, truncation, upper)
}

#' Truncation levels of Ait-Sahalia and Jacod
#'
#' Returns the truncation levels \eqn{u_n} used by the Ait-Sahalia and Jacod
#' statistics, in one of two parametrisations.
#'
#' * `method = "sd"`: the practice of Ait-Sahalia and Jacod (2012, p. 32 of
#'   the working paper) and (2011, Section 5). A pilot volatility is computed
#'   from the increments below \eqn{4\sigma\Delta_n^{1/2}}, where \eqn{\sigma}
#'   is a fixed realistic volatility (they use 0.25 per year),
#'   \deqn{\hat\sigma^2 = B(2, \mathrm{pilot}\cdot\sigma\Delta_n^{1/2},
#'         \Delta_n) / T,}
#'   and the truncation is set at `alpha` standard deviations of the
#'   continuous part, \eqn{u_n = \alpha \hat\sigma \Delta_n^{1/2}}.
#' * `method = "rate"`: the theoretical form \eqn{u_n = \alpha'
#'   \Delta_n^{\varpi}} of AJ (2009a, eq. 21), AJ (2011, eq. 30) and AJ
#'   (2009b, eq. 14), with the constant written as a multiple of a volatility,
#'   \eqn{\alpha' = \alpha\sigma}. For example, the simulations of AJ (2009a,
#'   Table 1) use \eqn{\alpha' = 5\beta^{1/2}} and \eqn{\varpi = 0.47}, that is
#'   `alpha = 5`, `sigma` = \eqn{\beta^{1/2}}, `varpi = 0.47`.
#'
#' @section Time units:
#' `sigma` and \eqn{\alpha'} carry time units. `horizon` is the length of the
#' observation window in the unit of `sigma`, and \eqn{\Delta_n} =
#' `horizon / n` with `n` the number of increments of `x`. The default,
#' `horizon = 1/252` with `sigma = 0.25`, describes one trading day with an
#' annualised volatility, as in the authors' empirical work. For 24-hour
#' markets with a volatility annualised on calendar time, use
#' `horizon = 1/365.25`. With `method = "sd"` the time unit enters only
#' through the pilot cutoff.
#'
#' @inheritParams aj_power_variation
#' @param alpha Positive numeric vector: number of standard deviations
#'   (`method = "sd"`) or multiple of `sigma` (`method = "rate"`).
#' @param method `"sd"` (default) or `"rate"`, see Description.
#' @param varpi Truncation rate \eqn{\varpi \in (0, 1/2]}, used by
#'   `method = "rate"` only.
#' @param sigma Volatility in the time unit of `horizon` (default 0.25, per
#'   year): the pilot guess for `method = "sd"`, the volatility scale for
#'   `method = "rate"`.
#' @param pilot Pilot cutoff in standard deviations of `sigma` (default 4).
#' @param horizon Window length in the time unit of `sigma` (default
#'   `1/252`, one trading day in years).
#' @return A numeric vector of truncation levels, one per element of `alpha`.
#' @section Paper mapping:
#' * AJ (2012, WP p. 32): pilot \eqn{B(2, 4\sigma\Delta_n^{1/2}, \Delta_n)}
#'   with \eqn{\sigma = 0.25}; cutoffs reported "in the form of the
#'   \eqn{\alpha} that would correspond to \eqn{\varpi = 1/2}".
#' * AJ (2011, Section 5): increments smaller than \eqn{\Delta_n^{1/2}}
#'   ("four standard deviations if the annualized volatility of the stock
#'   were 25% per year") give the pilot volatility.
#' * AJ (2009b, eq. 18): the truncated quadratic variation estimates
#'   \eqn{\int_0^t \sigma_s^2 ds}.
#' @section Implementation choices:
#' * \eqn{T} in the pilot is the window length `horizon`, so
#'   \eqn{\hat\sigma^2\Delta_n = B(2, \cdot, \Delta_n)/n}: the truncated
#'   quadratic variation per increment, divided by all `n` increments.
#' * The pilot is computed once per window, at the sampling interval of `x`.
#' @references
#' Ait-Sahalia, Y. and Jacod, J. (2009a). Testing for jumps in a discretely
#' observed process. *Annals of Statistics* 37(1), 184-222.
#'
#' Ait-Sahalia, Y. and Jacod, J. (2009b). Estimating the degree of activity
#' of jumps in high frequency data. *Annals of Statistics* 37(5A),
#' 2202-2244.
#'
#' Ait-Sahalia, Y. and Jacod, J. (2011). Testing whether jumps have finite or
#' infinite activity. *Annals of Statistics* 39(3), 1689-1719.
#'
#' Ait-Sahalia, Y. and Jacod, J. (2012). Analyzing the spectrum of asset
#' returns: jump and volatility components in high frequency data.
#' *Journal of Economic Literature* 50(4), 1007-1050.
#' @examples
#' # one trading day of 1-second log prices, 25% annual volatility
#' n <- 23400
#' x <- cumsum(c(0, rnorm(n, 0, 0.25 * sqrt(1 / (252 * n)))))
#' aj_truncation(x, alpha = c(5, 7, 10))
#' aj_truncation(x, alpha = 5, method = "rate", varpi = 0.47)
#' @export
aj_truncation <- function(x, alpha, method = "sd", varpi = 0.5, sigma = 0.25,
                          pilot = 4, horizon = 1 / 252) {
  y <- log_prices(x)
  alpha <- check_numeric(alpha, "alpha")
  if (any(alpha <= 0)) hf_abort("`alpha` must be positive.")
  check_choice(method, c("sd", "rate"), "method")
  check_number(varpi, "varpi", lower = 0, upper = 0.5, open = c(TRUE, FALSE))
  check_number(sigma, "sigma", lower = 0, open = c(TRUE, FALSE))
  check_number(pilot, "pilot", lower = 0, open = c(TRUE, FALSE))
  check_number(horizon, "horizon", lower = 0, open = c(TRUE, FALSE))
  adx <- abs(diff(y))
  sc <- aj_cutoff_scale(adx, horizon / length(adx), method, varpi, sigma, pilot)
  alpha * sc$scale
}

# Internal helpers ------------------------------------------------------------

# Increments of the k-subsampled prices X_{ik} - X_{(i-1)k}, i = 1..[n/k].
aj_increments <- function(y, k = 1L) {
  if (k == 1L) return(diff(y))
  diff(y[seq.int(1L, length(y), by = k)])
}

# B(p, u) (upper = FALSE) or U(p, u) (upper = TRUE) from absolute increments.
aj_power_sum <- function(adx, p, u = Inf, upper = FALSE) {
  keep <- if (upper) adx > u else adx <= u
  sum(adx[keep]^p)
}

# Pilot volatility (AJ 2012, WP p. 32) and the scale s with u_n = alpha * s.
aj_cutoff_scale <- function(adx, delta, method, varpi, sigma, pilot) {
  n <- length(adx)
  qv <- aj_power_sum(adx, 2, pilot * sigma * sqrt(delta))
  sigma_hat <- sqrt(qv / (n * delta))
  scale <- switch(method,
    sd = sigma_hat * sqrt(delta),
    rate = sigma * delta^varpi
  )
  if (method == "sd" && sigma_hat == 0) {
    hf_warn(paste0("The pilot volatility is zero: no increment lies below ",
                   "`pilot * sigma * sqrt(delta_n)`. Check `sigma` and `horizon`."))
  }
  list(scale = scale, sigma_hat = sigma_hat, delta = delta)
}

# Gauss hypergeometric series 2F1(a, b; c; z), |z| < 1.
aj_hyper_2f1 <- function(a, b, c, z, tol = 1e-16, max_terms = 2000L) {
  term <- 1
  total <- 1
  for (j in 0:(max_terms - 1L)) {
    term <- term * (a + j) * (b + j) / ((c + j) * (j + 1)) * z
    total <- total + term
    if (abs(term) <= tol * abs(total)) break
  }
  total
}

# m_{k,p} = E(|U|^p |U + sqrt(k - 1) V|^p) of AJ (2009a, eq. 17), U, V iid
# N(0, 1). W = U + sqrt(k - 1) V is N(0, k) with corr(U, W) = k^(-1/2), and
# E|Z1|^p |Z2|^p = m_p^2 2F1(-p/2, -p/2; 1/2; rho^2) for standard normals with
# correlation rho.
aj_mkp <- function(p, k) {
  k^(p / 2) * abs_normal_moment(p)^2 * aj_hyper_2f1(-p / 2, -p / 2, 0.5, 1 / k)
}

# Numerator of M(p, k) (AJ 2009a, eq. 20) and N(p, k) (AJ 2011, eq. 38).
aj_mn_core <- function(p, k) {
  k^(p - 2) * (1 + k) * abs_normal_moment(2 * p) +
    k^(p - 2) * (k - 1) * abs_normal_moment(p)^2 -
    2 * k^(p / 2 - 1) * aj_mkp(p, k)
}

# M(p, k) of AJ (2009a, eq. 20): Delta_n^(-1/2) (S - k^(p/2-1)) has variance
# M(p, k) A(2p) / A(p)^2 on continuous paths (eq. 19).
aj_M <- function(p, k) aj_mn_core(p, k) / abs_normal_moment(p)^2

# N(p, k) of AJ (2011, eq. 38) = M(p, k) m_p^2 / m_2p.
aj_N <- function(p, k) aj_mn_core(p, k) / abs_normal_moment(2 * p)

# Truncated A-hat(p, Delta_n) of AJ (2009a, eq. 21).
aj_A_trunc <- function(adx, p, u, delta) {
  delta^(1 - p / 2) / abs_normal_moment(p) * aj_power_sum(adx, p, u)
}

# Multipower A-tilde(r, q, Delta_n) of AJ (2009a, eq. 22):
# Delta_n^(1 - qr/2) / m_r^q sum_{i=1}^{n-q+1} prod_{j=1}^{q} |dX_{i+j-1}|^r.
aj_A_multipower <- function(adx, r, q, delta) {
  n <- length(adx)
  if (n < q) return(NA_real_)
  # log-domain moving sum; zero increments give -Inf and a zero product
  lsum <- stats::filter(r * log(adx), rep(1, q), method = "convolution", sides = 1L)
  prods <- exp(as.numeric(lsum)[q:n])
  delta^(1 - q * r / 2) / abs_normal_moment(r)^q * sum(prods)
}

# D-hat(p, Delta_n) of AJ (2009a, eq. 24): local truncated quadratic
# variation over I(i) = {j != i : |i - j| <= kn} around each increment.
aj_D_hat <- function(adx, p, u, kn, delta) {
  n <- length(adx)
  tr2 <- adx^2 * (adx <= u)
  cs <- c(0, cumsum(tr2))
  i <- seq_len(n)
  lo <- pmax(1L, i - kn)
  hi <- pmin(n, i + kn)
  local <- cs[hi + 1L] - cs[lo] - tr2
  sum(adx^p * local) / (kn * delta)
}

# Shared truncation properties of the AJ options classes.
aj_truncation_properties <- list(
  truncation = class_character,
  alpha = class_double,
  varpi = class_double,
  sigma = class_double,
  pilot = class_double,
  horizon = class_double
)

# Validator messages for the shared truncation properties.
aj_truncation_msgs <- function(self, alpha_scalar = TRUE) {
  collect_msgs(
    check_choice(self@truncation, c("sd", "rate"), "@truncation", abort = FALSE),
    if (alpha_scalar) {
      check_number(self@alpha, "@alpha", lower = 0, open = c(TRUE, FALSE), abort = FALSE)
    } else if (!length(self@alpha) || anyNA(self@alpha) || any(self@alpha <= 0)) {
      "@alpha must be a vector of positive numbers."
    },
    check_number(self@varpi, "@varpi", lower = 0, upper = 0.5, open = c(TRUE, FALSE),
                 abort = FALSE),
    check_number(self@sigma, "@sigma", lower = 0, open = c(TRUE, FALSE), abort = FALSE),
    check_number(self@pilot, "@pilot", lower = 0, open = c(TRUE, FALSE), abort = FALSE),
    check_number(self@horizon, "@horizon", lower = 0, open = c(TRUE, FALSE), abort = FALSE)
  )
}

# Checks the constructor arguments shared by the AJ options.
aj_check_truncation_args <- function(truncation, varpi, sigma, pilot, horizon) {
  check_choice(truncation, c("sd", "rate"), "truncation")
  check_number(varpi, "varpi", lower = 0, upper = 0.5, open = c(TRUE, FALSE))
  check_number(sigma, "sigma", lower = 0, open = c(TRUE, FALSE))
  check_number(pilot, "pilot", lower = 0, open = c(TRUE, FALSE))
  check_number(horizon, "horizon", lower = 0, open = c(TRUE, FALSE))
  invisible(NULL)
}

# Builds an S7 object; validator failures become `hf_input_error`s.
aj_new <- function(class, ...) {
  tryCatch(class(...), error = function(e) hf_abort(conditionMessage(e)))
}

# Truncation scale for a window from an options object.
aj_options_cutoff <- function(adx, options) {
  aj_cutoff_scale(adx, options@horizon / length(adx), options@truncation,
                  options@varpi, options@sigma, options@pilot)
}

# Formats numbers for print methods.
aj_fmt <- function(x, digits = 4) {
  ifelse(is.na(x), "NA", trimws(formatC(x, digits = digits, format = "g")))
}
