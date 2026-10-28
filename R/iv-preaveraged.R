# Shared pre-averaging helpers -------------------------------------------------

# `theta` is a positive number or, where an estimator offers a data-driven
# choice, the single string `choice`.
check_theta <- function(theta, choice = NULL) {
  if (!is.null(choice) && is.character(theta)) {
    check_choice(theta, choice, "theta")
  } else {
    check_number(theta, "theta", lower = 0, open = c(TRUE, FALSE))
  }
}

# Window kn from `kn` or `theta` (JLMPV 2009, eq. 3.1: kn sqrt(Delta_n) = theta,
# with Delta_n = 1/n on a unit window). kn = 2 makes every estimator below
# identically zero, so kn >= 3.
preavg_kn <- function(n, kn, theta, max_kn = n) {
  if (!is.null(kn)) {
    kn <- check_count(kn, "kn", min = 3L, max = max_kn)
  } else {
    kn <- max(3L, as.integer(round(theta * sqrt(n))))
    if (kn > max_kn) {
      hf_abort("`theta` = %s gives kn = %d, more than the %d this window allows.",
               format(theta), kn, max_kn)
    }
  }
  kn
}

# Pre-averaged returns Zbar_i = sum_{j=1}^{kn-1} g(j/kn) dZ_{i+j} for every i
# the data determine, i = 0, ..., n - kn + 1 (JLMPV 2009, eqs. 3.5-3.6; HP 2013,
# eq. 4.30). preaverage() stops at i = n - kn because its local noise measure
# also needs dZ_{i+kn}; the weight g(1) = 0 of that increment makes one more
# pre-averaged return available.
preavg_zbar <- function(dy, weight, kn) {
  n <- length(dy)
  g <- weight@fun(seq_len(kn - 1L) / kn)
  as.double(stats::filter(dy, rev(g), sides = 1L))[(kn - 1L):n]
}

# Finite-sample constants psi1^kn, psi2^kn (JLMPV 2009, Sec. 4.2; HP 2013,
# Sec. 2.3). psi1^kn sums all kn squared weight differences, the noise weight
# of A_i^n in JLMPV (2009, eq. 5.4).
preavg_psi <- function(weight, kn) {
  g <- weight@fun((0:kn) / kn)
  c(psi1 = kn * sum(diff(g)^2), psi2 = sum(g[2:kn]^2) / kn)
}

# Limits psi1 = int (g')^2, psi2 = int g^2 (JLMPV 2009, eq. 3.4).
preavg_psi_limit <- function(weight) {
  c(psi1 = weight_integral(weight, 2, derivative = TRUE),
    psi2 = weight_integral(weight, 2))
}

# Sums r(j) = sum_i v_i v_{i+j}, j = 0..m-1, by FFT.
lag_products <- function(v) {
  m <- length(v)
  nfft <- 2^ceiling(log2(2 * m))
  f <- stats::fft(c(v, numeric(nfft - m)))
  Re(stats::fft(Mod(f)^2, inverse = TRUE))[seq_len(m)] / nfft
}

# Finite-sample Phi_ij^kn (JLMPV 2009, Sec. 4.2; HP 2013, Sec. 2.3) from
# phi1^kn(j) = sum_{i>j} (g_{i-1} - g_i)(g_{i-j-1} - g_{i-j}) and
# phi2^kn(j) = sum_{i>j} g_i g_{i-j}, j = 0..kn-1.
preavg_phi <- function(weight, kn) {
  g <- weight@fun((0:kn) / kn)
  phi1 <- lag_products(-diff(g))
  phi2 <- lag_products(g[-1L])
  c(Phi11 = kn * (sum(phi1^2) - phi1[1L]^2 / 2),
    Phi12 = (sum(phi1 * phi2) - phi1[1L] * phi2[1L] / 2) / kn,
    Phi22 = (sum(phi2^2) - phi2[1L]^2 / 2) / kn^3)
}

# Limits Phi_ij = int_0^1 phi_i(s) phi_j(s) ds with phi_1(s) = int_s^1 g'(u)
# g'(u - s) du and phi_2(s) = int_s^1 g(u) g(u - s) du (JLMPV 2009, eq. 3.4).
preavg_phi_limit <- function(weight) {
  bp <- weight@breakpoints
  phi <- function(f, s) {
    vapply(s, function(si) {
      integrate_piecewise(function(u) f(u) * f(u - si), si, 1,
                          breaks = c(bp, bp + si), rel_tol = 1e-8)
    }, numeric(1))
  }
  kinks <- unique(as.vector(abs(outer(bp, bp, "-"))))
  big_phi <- function(fi, fj) {
    integrate_piecewise(function(s) phi(fi, s) * phi(fj, s), 0, 1,
                        breaks = kinks, rel_tol = 1e-8)
  }
  c(Phi11 = big_phi(weight@deriv, weight@deriv),
    Phi12 = big_phi(weight@deriv, weight@fun),
    Phi22 = big_phi(weight@fun, weight@fun))
}

# JLMPV (2009, Remarks 2-3): for constant sigma and noise variance alpha, the
# asymptotic variance (3.9) is minimised by theta = sqrt(alpha / (sigma^2 v)),
# where v solves Phi22 - 2 Phi12 v - 3 Phi11 v^2 = 0 (theta = 4.777
# sqrt(alpha) / sigma for the tent).
preavg_optimal_theta <- function(weight, alpha, iv) {
  ph <- preavg_phi_limit(weight)
  v <- (-ph[["Phi12"]] + sqrt(ph[["Phi12"]]^2 + 3 * ph[["Phi11"]] * ph[["Phi22"]])) /
    (3 * ph[["Phi11"]])
  sqrt(alpha / (iv * v))
}

# Jacod, Li, Mykland, Podolskij and Vetter (2009) ------------------------------

#' Pre-averaging estimator of Jacod et al. (2009)
#'
#' Computes the pre-averaging estimator of integrated variance of Jacod, Li,
#' Mykland, Podolskij and Vetter (2009, eq. 3.6),
#' \deqn{\hat C = \frac{\sqrt{\Delta_n}}{\theta\psi_2}
#'       \sum_{i=0}^{n-k_n+1} (\bar Z_i)^2 - \frac{\psi_1 \Delta_n}{2\theta^2
#'       \psi_2} \sum_{i=1}^{n} (\Delta_i Z)^2, \qquad
#'       \bar Z_i = \sum_{j=1}^{k_n-1} g(j/k_n) \Delta_{i+j} Z,}
#' by default in its finite-sample version \eqn{\hat C^{adj}} (Section 4.2).
#' It is consistent for the integrated variance under additive noise but not
#' robust to jumps: with jumps it estimates the quadratic variation.
#'
#' @section Paper mapping:
#' * Eq. (3.1): \eqn{k_n \sqrt{\Delta_n} = \theta}; `theta` or `kn`. As in
#'   Section 4.2, \eqn{\theta} is then re-defined as \eqn{k_n/\sqrt{n}}, so
#'   that \eqn{\sqrt{\Delta_n}/(\theta\psi_2) = 1/(k_n\psi_2)} and
#'   \eqn{\psi_1\Delta_n/(2\theta^2\psi_2) = \psi_1/(2k_n^2\psi_2)}.
#' * Eqs. (3.2)-(3.5): weight `weight`, pre-averaged returns \eqn{\bar Z_i}.
#' * Eq. (3.6): \eqn{\hat C}; `adjust = FALSE`, with \eqn{\psi_1 = \int
#'   (g')^2} and \eqn{\psi_2 = \int g^2} (eq. 3.4; 1 and 1/12 for the tent,
#'   eqs. 3.11-3.12).
#' * Section 4.2: \eqn{\hat C^{adj} = (1 - \frac{\psi_1^{k_n}\Delta_n}{2
#'   \theta^2\psi_2^{k_n}})^{-1} (\frac{n\sqrt{\Delta_n}}{(n - k_n + 2)
#'   \theta\psi_2^{k_n}} \sum_i (\bar Z_i)^2 - \frac{\psi_1^{k_n}\Delta_n}{2
#'   \theta^2\psi_2^{k_n}} \sum_i (\Delta_i Z)^2)}; `adjust = TRUE`.
#' * Remarks 2-3: the variance-minimising \eqn{\theta = 4.777
#'   \sqrt{\alpha}/\sigma} for the tent; `theta = "optimal"`.
#'
#' @section Implementation choices:
#' * Default \eqn{\theta = 1/3}: the authors' simulation (Section 4.2) uses
#'   \eqn{k_n = 51} with \eqn{n = 23400}, \eqn{\theta \approx 1/3}.
#'   \eqn{k_n = \max(3, \mathrm{round}(\theta\sqrt n))}; \eqn{k_n = 2} would
#'   make the estimator identically zero.
#' * Default `adjust = TRUE`: the authors propose the finite-sample version
#'   after their Table 2, and it removes the small-sample bias in Table 3.
#' * \eqn{\psi_1^{k_n} = k_n \sum_{j=0}^{k_n-1} (g_{j+1} - g_j)^2}, the noise
#'   weight of \eqn{A_i^n} in the proof (eq. 5.4). The display in Section 4.2
#'   starts this sum at \eqn{j = 1}, which drops \eqn{g(1/k_n)^2}. With the
#'   full sum \eqn{\hat C^{adj}} is exactly unbiased for constant volatility
#'   and i.i.d. noise.
#' * `theta = "optimal"` follows Remark 3: pilot \eqn{\hat\sigma^2} from this
#'   estimator with \eqn{\theta = 1/3} and pilot noise variance
#'   \eqn{\hat\alpha} from [noise_variance()] (lag-1 autocovariance); then
#'   \eqn{\theta = \sqrt{\hat\alpha/(\hat\sigma^2 v)}} with \eqn{v} the
#'   positive root of \eqn{\Phi_{22} - 2\Phi_{12}v - 3\Phi_{11}v^2 = 0},
#'   which minimises (3.9) for constant \eqn{\sigma} and \eqn{\alpha}.
#' * `noise_variance` of the result is \eqn{\Delta_n RV/2}, the noise
#'   estimate implicit in (3.6).
#'
#' @inheritParams iv_realized
#' @param kn Pre-averaging window \eqn{k_n \ge 3}. Overrides `theta`.
#' @param theta \eqn{\theta = k_n/\sqrt n}, a positive number, or
#'   `"optimal"` for the plug-in choice of Remarks 2-3.
#' @param weight Pre-averaging [WeightFunction]; defaults to [weight_tent()],
#'   the authors' choice (eq. 3.11).
#' @param adjust Use the finite-sample version of Section 4.2?
#' @return An [IVEstimate] with `jump_robust = FALSE`.
#' @references
#' Jacod, J., Li, Y., Mykland, P. A., Podolskij, M. and Vetter, M. (2009).
#' Microstructure noise in the continuous case: the pre-averaging approach.
#' *Stochastic Processes and their Applications* 119(7), 2249-2276.
#' @seealso [integrated_variance()], [iv_hp2013()]
#' @examples
#' set.seed(1)
#' n <- 23400
#' y <- cumsum(c(0, rnorm(n, 0, 0.2 / sqrt(252 * n)))) + rnorm(n + 1, 0, 5e-4)
#' iv_jlmpv2009(y)                # kn = 51 as in JLMPV (2009, Sec. 4.2)
#' iv_jlmpv2009(y, theta = "optimal")
#' @export
iv_jlmpv2009 <- function(x, kn = NULL, theta = 1 / 3, weight = weight_tent(),
                         adjust = TRUE) {
  dy <- iv_returns(x, 4L, "iv_jlmpv2009")
  n <- length(dy)
  check_theta(theta, "optimal")
  check_class(weight, WeightFunction, "weight")
  check_flag(adjust, "adjust")
  pilot <- NULL
  if (is.null(kn) && is.character(theta)) {
    pilot <- c(
      iv = iv_jlmpv2009_value(dy, preavg_kn(n, NULL, 1 / 3), weight, adjust),
      noise_variance = noise_autocov(dy, 0L, FALSE)
    )
    theta <- if (pilot[["iv"]] > 0) {
      preavg_optimal_theta(weight, pilot[["noise_variance"]], pilot[["iv"]])
    } else {
      hf_warn("The pilot estimate for `theta = \"optimal\"` is not positive; using theta = 1/3.")
      1 / 3
    }
  }
  kn <- preavg_kn(n, kn, theta)
  tuning <- list(kn = kn, theta = kn / sqrt(n), weight = weight@name,
                 adjust = adjust)
  if (!is.null(pilot)) tuning$pilot <- pilot
  new_iv_estimate(
    value = iv_jlmpv2009_value(dy, kn, weight, adjust),
    method = "jlmpv2009",
    reference = if (adjust) "Jacod et al. (2009, Sec. 4.2)" else
      "Jacod et al. (2009, eq. 3.6)",
    n = n, jump_robust = FALSE, noise_variance = sum(dy^2) / (2 * n),
    tuning = tuning
  )
}

iv_jlmpv2009_value <- function(dy, kn, weight, adjust) {
  n <- length(dy)
  zbar <- preavg_zbar(dy, weight, kn) # i = 0..n-kn+1
  rv <- sum(dy^2)
  psi <- if (adjust) preavg_psi(weight, kn) else preavg_psi_limit(weight)
  noise_coef <- psi[["psi1"]] / (2 * kn^2 * psi[["psi2"]])
  if (adjust) {
    if (noise_coef >= 1) {
      hf_abort("kn = %d is too small for this weight: the noise correction is degenerate.", kn)
    }
    (n / length(zbar) * sum(zbar^2) / (kn * psi[["psi2"]]) - noise_coef * rv) /
      (1 - noise_coef)
  } else {
    sum(zbar^2) / (kn * psi[["psi2"]]) - noise_coef * rv
  }
}

# Jacod, Podolskij and Vetter (2010) -------------------------------------------

#' Pre-averaging estimator of Jacod, Podolskij and Vetter (2010)
#'
#' Estimates integrated variance from the law of large numbers of Jacod,
#' Podolskij and Vetter (2010, eq. 3.5),
#' \deqn{\frac{1}{k_n} V(Z, g, 2, 0)^n - \frac{1}{2k_n} V(Z, g, 0, 1)^n
#'       \to \bar g(2) [X, X]_T,}
#' where \eqn{V(Z, g, p, r)^n = \sum_{i=0}^{n-k_n} |\bar Z(g)_i|^p
#' |\hat Z(g)_i|^r} (eq. 2.12) and \eqn{\hat Z(g)_i} is the local noise
#' measure (eq. 2.11). The estimator is \eqn{[X, X]_T} divided by \eqn{\bar
#' g(2) = \int g^2}: consistent for the integrated variance under noise, and
#' for the quadratic variation when there are jumps (it is not jump-robust).
#'
#' @section Paper mapping:
#' * Eq. (2.6): \eqn{k_n\sqrt{\Delta_n} = \theta}; `kn` or `theta`.
#' * Eqs. (2.8), (2.11), (2.12): [preaverage()] and
#'   [preavg_power_variation()].
#' * Eq. (3.5) and (3.10) for \eqn{p = 2}: the estimator, with
#'   \eqn{\bar g(2) = \int_0^1 g(s)^2 ds} (eq. 2.9); `constants =
#'   "asymptotic"`.
#'
#' @section Implementation choices:
#' * JPV (2010) do not tune \eqn{\theta}; the default \eqn{\theta = 1/3} is
#'   the choice of the same authors in Jacod et al. (2009, Sec. 4.2).
#'   \eqn{k_n = \max(3, \mathrm{round}(\theta\sqrt n))}.
#' * Default `constants = "asymptotic"`: the limit (3.5) as stated.
#'   `constants = "finite_sample"` divides instead by the exact expectation
#'   of the statistic for constant volatility and i.i.d. noise,
#'   \eqn{\frac{n - k_n + 1}{n k_n}(\bar g(2)_n - \bar g'(2)_n/2)} with
#'   \eqn{\bar g(2)_n = \sum_{j=1}^{k_n} g(j/k_n)^2} and \eqn{\bar g'(2)_n =
#'   \sum_{j=1}^{k_n} (g(j/k_n) - g((j-1)/k_n))^2} (eq. 2.8). This is the
#'   analogue of the finite-sample adjustment of Jacod et al. (2009, Sec.
#'   4.2); the two coincide up to end effects. The difference from the
#'   asymptotic version is of order \eqn{k_n/n + 1/k_n^2}.
#' * `noise_variance` of the result is \eqn{V(Z, g, 0, 1)^n / (2 (n - k_n +
#'   1) \bar g'(2)_n)}, the noise estimate implicit in (3.5): by Remark 2.3,
#'   \eqn{\hat Z(g)_i \approx 2\bar g'(2)\alpha^2_{i\Delta_n}/k_n}, and
#'   \eqn{\bar g'(2)/k_n \approx \bar g'(2)_n} by (2.10).
#'
#' @inheritParams iv_jlmpv2009
#' @param theta \eqn{\theta = k_n/\sqrt n}, a positive number.
#' @param weight Pre-averaging [WeightFunction]; defaults to [weight_tent()].
#' @param constants `"asymptotic"` (eq. 3.5 as stated) or `"finite_sample"`.
#' @return An [IVEstimate] with `jump_robust = FALSE`.
#' @references
#' Jacod, J., Podolskij, M. and Vetter, M. (2010). Limit theorems for moving
#' averages of discretized processes plus noise. *Annals of Statistics*
#' 38(3), 1478-1545.
#'
#' Jacod, J., Li, Y., Mykland, P. A., Podolskij, M. and Vetter, M. (2009).
#' Microstructure noise in the continuous case: the pre-averaging approach.
#' *Stochastic Processes and their Applications* 119(7), 2249-2276.
#' @seealso [integrated_variance()], [preaverage()]
#' @examples
#' set.seed(1)
#' n <- 23400
#' y <- cumsum(c(0, rnorm(n, 0, 0.01 / sqrt(n)))) + rnorm(n + 1, 0, 1e-4)
#' sqrt(iv_jpv2010(y)@value) # about 0.01
#' @export
iv_jpv2010 <- function(x, kn = NULL, theta = 1 / 3, weight = weight_tent(),
                       constants = c("asymptotic", "finite_sample")) {
  y <- log_prices(x)
  n <- length(y) - 1L
  if (n < 4L) hf_abort("`iv_jpv2010()` needs at least 4 increments, not %d.", n)
  check_theta(theta)
  check_class(weight, WeightFunction, "weight")
  constants <- if (missing(constants)) "asymptotic" else
    check_choice(constants, c("asymptotic", "finite_sample"), "constants")
  kn <- preavg_kn(n, kn, theta)
  pa <- preaverage(y, kn, weight)
  v20 <- preavg_power_variation(pa, 2, 0)
  v01 <- preavg_power_variation(pa, 0, 1)
  stat <- v20 / kn - v01 / (2 * kn) # eq. (3.5): -> gbar(2) [X, X]
  g <- weight@fun((0:kn) / kn)
  gbar2_n <- sum(g[-1L]^2)
  gbar2_prime_n <- sum(diff(g)^2)
  value <- if (constants == "asymptotic") {
    stat / weight_integral(weight, 2)
  } else {
    stat * n * kn / ((n - kn + 1) * (gbar2_n - gbar2_prime_n / 2))
  }
  new_iv_estimate(
    value = value, method = "jpv2010",
    reference = "Jacod, Podolskij and Vetter (2010, eq. 3.5)",
    n = n, jump_robust = FALSE,
    noise_variance = v01 / (2 * (n - kn + 1) * gbar2_prime_n),
    tuning = list(kn = kn, theta = kn / sqrt(n), weight = weight@name,
                  constants = constants)
  )
}
