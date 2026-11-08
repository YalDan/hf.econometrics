#' Variance of additive microstructure noise
#'
#' Estimates the variance \eqn{\omega^2 = E U_i^2} of the noise in the model
#' \eqn{Y_i = X_{i/n} + U_i} from the log returns \eqn{\Delta_i Y} of one
#' window, with one of the estimators used by the integrated-variance papers
#' of this package.
#'
#' * `"autocovariance"` (default): minus the first-order autocovariance of the
#'   returns, \eqn{\hat\omega^2 = -\frac{1}{n-1}\sum_{i=1}^{n-1}
#'   \Delta_i Y \Delta_{i+1} Y}. With `q > 0` it becomes the estimator of
#'   Hautsch and Podolskij (2013, Lemma 3.2) for \eqn{q}-dependent noise,
#'   which recovers \eqn{\rho(0), \dots, \rho(q)}, \eqn{\rho(k) = cov(U_1,
#'   U_{1+k})}, from the realized autocovariances of lags \eqn{1, \dots, q+1}
#'   by the recursion \eqn{\rho(q) = -\gamma(q+1)},
#'   \eqn{\rho(k-1) = 2\rho(k) - \rho(k+1) - \gamma(k)}. It returns
#'   \eqn{\rho(0)}, or the long-run variance \eqn{\rho^2 = \rho(0) +
#'   2\sum_{k=1}^q \rho(k)} (HP eq. 3.19) when `long_run = TRUE`.
#' * `"pv2009"`: \eqn{\hat\omega^2 = \frac{1}{2n}\sum_{i=1}^n (\Delta_i Y)^2},
#'   Podolskij and Vetter (2009, eq. 3.5), after Zhang, Mykland and
#'   Ait-Sahalia (2005). This is also the noise correction implicit in
#'   Jacod et al. (2009, eq. 3.6) and Hautsch and Podolskij (2013, eqs.
#'   2.11-2.12).
#' * `"hp2013"`: Hautsch and Podolskij (2013, eq. 2.17), after Oomen (2006):
#'   the autocovariance estimator when \eqn{\sum_i \Delta_i Y \Delta_{i-1} Y
#'   < 0}, and \eqn{\sum_i (\Delta_i Y)^2 / (2 N^0)} otherwise, where
#'   \eqn{N^0} is the number of non-zero returns.
#'
#' @section Bias:
#' For i.i.d. noise independent of \eqn{X}, a driftless continuous \eqn{X}
#' with integrated variance \eqn{IV} over the window, and jumps
#' \eqn{\Delta X_s}:
#' * `"pv2009"`: \eqn{E\hat\omega^2 = \omega^2 + (IV + \sum_s \Delta X_s^2)
#'   / (2n)}. The bias is negligible when \eqn{IV/n \ll \omega^2}, but not on
#'   frequently sampled, low-noise data: one day of one-second returns with
#'   \eqn{IV = 9 \times 10^{-4}} and \eqn{\omega = 5 \times 10^{-5}} gives a
#'   bias of \eqn{2\omega^2}, and a 5% jump adds another \eqn{6\omega^2}.
#'   Under \eqn{q}-dependent noise it estimates \eqn{\rho(0) - \rho(1)}.
#' * `"autocovariance"` with `q = 0`: \eqn{E\hat\omega^2 = \omega^2}. A jump
#'   enters only through mean-zero cross products \eqn{\Delta X_s
#'   \Delta_{i \pm 1} Y}, so the estimator is jump-robust to first order.
#'   Under \eqn{q}-dependent noise it estimates \eqn{\rho(0) - 2\rho(1) +
#'   \rho(2)}; with `q` at least the dependence order it estimates
#'   \eqn{\rho(0)} (or \eqn{\rho^2}) without bias. Its variance is larger than
#'   that of `"pv2009"`.
#' * `"hp2013"`: as `"autocovariance"` when the first-order autocovariance
#'   is negative, which is the typical case. Otherwise it falls back to an
#'   estimator with the upward bias of `"pv2009"`.
#'
#' @section Paper mapping:
#' * PV (2009, eq. 3.5): `method = "pv2009"`.
#' * HP (2013, eq. 2.17): `method = "hp2013"`.
#' * HP (2013, eq. 3.20 and Lemma 3.2): realized autocovariances
#'   \eqn{\gamma(k)} and the recursion for \eqn{\rho(0), \dots, \rho(q)};
#'   `method = "autocovariance"`, `q`. HP (2013, eq. 3.19): `long_run = TRUE`.
#'
#' @section Implementation choices:
#' * HP (2013, eq. 3.20) scale the realized autocovariances by
#'   \eqn{\Delta_n = 1/n}; here \eqn{\gamma(k)} is the mean of the
#'   \eqn{n - k} available products, which makes \eqn{-\gamma(1)} exactly
#'   unbiased under i.i.d. noise and matches the \eqn{1/(N-1)} of HP
#'   (2013, eq. 2.17). The difference is of order \eqn{1/n}.
#' * `"autocovariance"` returns \eqn{\max(\hat\omega^2, 0)}: a negative raw
#'   value (positively autocorrelated returns) is inconsistent with additive
#'   noise. `"hp2013"` handles that case with its own fallback instead.
#'
#' @inheritParams iv_realized
#' @param method Estimator: `"autocovariance"`, `"pv2009"` or `"hp2013"`.
#' @param q Dependence order of the noise for `method = "autocovariance"`:
#'   \eqn{U_i} and \eqn{U_j} are independent for \eqn{|i - j| > q}. `q = 0`
#'   (default) is i.i.d. noise.
#' @param long_run Return the long-run variance \eqn{\rho^2} instead of
#'   \eqn{\rho(0)}? Only for `method = "autocovariance"`.
#' @return A single non-negative number: the estimated noise variance (or
#'   long-run variance), in squared log-price units.
#' @references
#' Hautsch, N. and Podolskij, M. (2013). Preaveraging-based estimation of
#' quadratic variation in the presence of noise and jumps: theory,
#' implementation, and empirical evidence. *Journal of Business & Economic
#' Statistics* 31(2), 165-183.
#'
#' Oomen, R. C. A. (2006). Properties of realized variance under alternative
#' sampling schemes. *Journal of Business & Economic Statistics* 24(2),
#' 219-237.
#'
#' Podolskij, M. and Vetter, M. (2009). Estimation of volatility functionals
#' in the simultaneous presence of microstructure noise and jumps.
#' *Bernoulli* 15(3), 634-658.
#'
#' Zhang, L., Mykland, P. A. and Ait-Sahalia, Y. (2005). A tale of two time
#' scales: determining integrated volatility with noisy high-frequency data.
#' *Journal of the American Statistical Association* 100, 1394-1411.
#' @examples
#' set.seed(1)
#' n <- 86400
#' y <- cumsum(c(0, rnorm(n, 0, 0.03 / sqrt(n)))) + rnorm(n + 1, 0, 5e-5)
#' sqrt(noise_variance(y))           # close to 5e-5
#' sqrt(noise_variance(y, "pv2009")) # biased up by IV / (2n)
#' @export
noise_variance <- function(x, method = c("autocovariance", "pv2009", "hp2013"),
                           q = 0L, long_run = FALSE) {
  method <- if (missing(method)) "autocovariance" else
    check_choice(method, c("autocovariance", "pv2009", "hp2013"), "method")
  q <- check_count(q, "q", min = 0L)
  check_flag(long_run, "long_run")
  if (method != "autocovariance" && (q != 0L || long_run)) {
    hf_abort("`q` and `long_run` apply only to `method = \"autocovariance\"`.")
  }
  dy <- iv_returns(x, q + 2L, "noise_variance")
  switch(method,
    autocovariance = noise_autocov(dy, q, long_run),
    pv2009 = sum(dy^2) / (2 * length(dy)),
    hp2013 = noise_hp2013(dy)
  )
}

# Realized autocovariances gamma(k), k = 1..q+1, each the mean of the n - k
# available products (HP 2013, eq. 3.20, with t = 1).
realized_autocov <- function(dy, lags) {
  n <- length(dy)
  vapply(lags, function(k) mean(dy[seq_len(n - k)] * dy[(k + 1L):n]),
         numeric(1))
}

# HP (2013, Lemma 3.2): rho(q) = -gamma(q+1), rho(k-1) = 2 rho(k) - rho(k+1)
# - gamma(k) for k = q, ..., 1, with rho(q+1) = rho(q+2) = 0. Returns rho(0) or
# the long-run variance rho(0) + 2 sum_k rho(k), truncated at zero.
noise_autocov <- function(dy, q, long_run) {
  gam <- realized_autocov(dy, seq_len(q + 1L))
  rho <- numeric(q + 3L) # rho[k + 1] holds rho(k), k = 0..q+2
  for (k in (q + 1L):1L) rho[k] <- 2 * rho[k + 1L] - rho[k + 2L] - gam[k]
  est <- if (long_run) rho[1L] + 2 * sum(rho[seq_len(q) + 1L]) else rho[1L]
  max(est, 0)
}

# HP (2013, eq. 2.17), after Oomen (2006).
noise_hp2013 <- function(dy) {
  n <- length(dy)
  s1 <- sum(dy[-1L] * dy[-n])
  if (s1 < 0) return(-s1 / (n - 1))
  n0 <- sum(dy != 0)
  if (n0 == 0L) return(0)
  sum(dy^2) / (2 * n0)
}
