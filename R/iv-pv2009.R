#' Modulated bipower variation of Podolskij and Vetter (2009)
#'
#' Computes the estimator of integrated variance of Podolskij and Vetter
#' (2009, eq. 3.10), which is robust to both microstructure noise and finitely
#' many jumps:
#' \deqn{MBV(Z)_n = \frac{(c_1 c_2/\mu_1^2) MBV(Z, 1, 1)_n - \nu_2
#'       \hat\omega^2}{\nu_1} \to \int_0^1 \sigma_u^2 du,}
#' where \eqn{MBV(Z, 1, 1)_n = \sum_m |\bar Z_m^{(K)}| |\bar Z_{m+1}^{(K)}|}
#' is the bipower variation of the block averages
#' \deqn{\bar Z_m^{(K)} = \frac{1}{L - K + 1} \sum_{i=(m-1)L}^{mL-K}
#'       (Z_{i+K} - Z_i)}
#' of \eqn{K}-step returns over \eqn{M} non-overlapping blocks of
#' \eqn{L = n/M} returns (eqs. 2.3-2.5). Lee and Mykland (2012, Sec. 4.2 and
#' footnote 12) prescribe this estimator for the volatility in their jump
#' test.
#'
#' @section Paper mapping:
#' * Eq. (2.4): block averages \eqn{\bar Z_m^{(K)}}, computed from cumulative
#'   sums of the \eqn{K}-step returns.
#' * Eq. (2.5): \eqn{K = c_1 n^{1/2}} and block length \eqn{n/M = c_2 K};
#'   arguments `K` and `L`, with \eqn{c_1 = K/\sqrt n} and \eqn{c_2 = L/K}.
#' * Eq. (2.3) with \eqn{r = l = 1} (normalisation \eqn{n^{0} = 1}):
#'   \eqn{MBV(Z, 1, 1)_n}.
#' * Eq. (3.3): \eqn{\nu_1 = c_1(3c_2 - 4 + ((2 - c_2)^3 \vee 0))/(3(c_2 -
#'   1)^2)}, \eqn{\nu_2 = 2((c_2 - 1) \wedge 1)/(c_1(c_2 - 1)^2)};
#'   `constants = "asymptotic"`.
#' * Appendix (first paragraph) and Sec. 4.1: the true variances
#'   \eqn{\nu_i^{(n)}} of \eqn{n^{1/4}\bar W_m^{(K)}} and \eqn{n^{1/4}\bar
#'   U_m^{(K)}}; `constants = "finite_sample"`.
#' * Eq. (3.5): \eqn{\hat\omega^2 = \frac{1}{2n}\sum_i (\Delta_i Z)^2};
#'   `noise = "pv2009"`.
#' * Eq. (3.10): the estimator. Eq. (3.20): \eqn{c_2 = 8/5}, \eqn{c_1 =
#'   \sqrt{18/((c_2 - 1)(4 - c_2))}\,\omega/\sigma}, used when `K` is `NULL`.
#'
#' @section Implementation choices:
#' * **Noise variance (deviation from the paper).** The default `noise =
#'   "autocovariance"` plugs in \eqn{\hat\omega^2 = -\frac{1}{n-1}\sum_i
#'   \Delta_i Z \Delta_{i+1} Z} (see [noise_variance()]) instead of (3.5).
#'   Under i.i.d. noise (3.5) has bias \eqn{(IV + \sum_s \Delta X_s^2)/(2n)},
#'   which is negligible in the authors' simulation design but not on
#'   one-second data with small noise: one day with \eqn{IV = 9 \times
#'   10^{-4}} and \eqn{\omega = 5 \times 10^{-5}} gives a bias of
#'   \eqn{2\omega^2}, and a 5% jump adds \eqn{6\omega^2}. With the
#'   tuning (3.20), the estimator as printed (constants 3.3, noise 3.5) then
#'   underestimates the integrated variance by 34% without a jump and by 42%
#'   with one; with exact constants the errors are 16% and 25%
#'   (`tests/simulations/iv-bias.R`). The autocovariance estimator is
#'   unbiased under i.i.d. noise and robust to jumps to first order, the
#'   property of \eqn{\hat\omega^2} that (3.10) relies on ("since
#'   \eqn{\hat\omega^2} is robust to jumps"). `noise = "pv2009"` with
#'   `constants = "asymptotic"` gives the estimator exactly as printed;
#'   `noise_variance` supplies any other estimate.
#' * **Constants.** The default `constants = "finite_sample"` uses the exact
#'   second moments for the integer \eqn{K} and \eqn{L} actually used, as
#'   the authors recommend for \eqn{\nu_1} in Section 4.1: \eqn{Var(\bar
#'   Z_m) = \frac{IV}{n} \frac{\sum_{j=1}^{L} w_j^2}{(L - K + 1)^2} +
#'   \omega^2 \frac{2\min(K, L - K + 1)}{(L - K + 1)^2}} with \eqn{w_j =
#'   \min(j, K, L - K + 1, L + 1 - j)}. The approximation of
#'   \eqn{\nu_1^{(n)}} printed in Section 4.1 is not used: it does not match
#'   this exact variance (for \eqn{c_2 = 2} the exact correction is
#'   \eqn{-1/(3\sqrt n)}, the printed one \eqn{+1/\sqrt n}). The exact
#'   version also uses the exact Gaussian moment of \eqn{|\bar Z_m||\bar
#'   Z_{m+1}|}: adjacent blocks share the endpoint noise \eqn{U_{mL}}, so
#'   \eqn{Cov(\bar Z_m, \bar Z_{m+1}) = -\omega^2/(L - K + 1)^2} and
#'   \eqn{E|\bar Z_m||\bar Z_{m+1}| = \mu_1^2 V (\sqrt{1 - \rho^2} + \rho
#'   \arcsin\rho)}; \eqn{IV} is the root of this equation. The correction
#'   matters only for very small \eqn{K}. With these constants the estimator
#'   is unbiased for constant volatility and Gaussian i.i.d. noise for any
#'   \eqn{K} and \eqn{L}; with (3.3) it is not, and with the small \eqn{K}
#'   that (3.20) gives on one-second data the bias is large.
#' * **Sum range.** \eqn{MBV(Z, 1, 1)_n} runs over the \eqn{M - 1} adjacent
#'   pairs, as in the definition of modulated multipower variation (sum to
#'   \eqn{M - k + 1} for \eqn{k = 2} factors); eq. (2.3) prints \eqn{M},
#'   which would need \eqn{\bar Z_{M+1}}. The finite-sample version averages
#'   over the \eqn{M - 1} pairs; the asymptotic one uses (3.10) as printed.
#'   \eqn{M = \lfloor n/L \rfloor}: the final incomplete block is not used.
#' * **Tuning.** With `K = NULL`, \eqn{K} and \eqn{L} follow (3.20):
#'   \eqn{c_2 = 8/5}, \eqn{c_1 = \sqrt{18/((c_2 - 1)(4 - c_2))}\,
#'   \hat\omega/\hat\sigma}, \eqn{K = \max(1, \mathrm{round}(c_1\sqrt n))},
#'   \eqn{L = \max(K + 1, \mathrm{round}(c_2 K))}. The pilot \eqn{\hat\sigma^2}
#'   is this estimator with \eqn{c_1 = 1}, \eqn{c_2 = 8/5}, the constants the
#'   authors use for their quarticity estimator in Section 4.1. (In their
#'   simulation for integrated variance they fixed \eqn{c_2 = 2} and
#'   \eqn{c_1 \in \{0.25, 0.125\}} from a first estimate of \eqn{IV}; pass
#'   `K` and `L` to reproduce such choices.) \eqn{K} is capped so that at
#'   least two blocks fit in the window.
#'
#' @inheritParams iv_realized
#' @param K Length \eqn{K \ge 1} of the returns that are averaged within a
#'   block. `NULL` (default) applies the tuning rule (3.20).
#' @param L Block length \eqn{L = n/M > K} in returns. `NULL` gives
#'   \eqn{\max(K + 1, \mathrm{round}(8K/5))}.
#' @param constants `"finite_sample"` (default) or `"asymptotic"` (eq. 3.3).
#' @param noise Noise-variance estimator: `"autocovariance"` (default) or
#'   `"pv2009"` (eq. 3.5). Ignored when `noise_variance` is given.
#' @param noise_variance Optional known or externally estimated noise
#'   variance \eqn{\omega^2 \ge 0}.
#' @return An [IVEstimate] with `jump_robust = TRUE`. `tuning` records `K`,
#'   `L`, the number of blocks `M`, \eqn{c_1}, \eqn{c_2}, the constants and
#'   noise estimator used, and the pilot \eqn{\hat\sigma^2} when (3.20) was
#'   applied.
#' @references
#' Podolskij, M. and Vetter, M. (2009). Estimation of volatility functionals
#' in the simultaneous presence of microstructure noise and jumps.
#' *Bernoulli* 15(3), 634-658.
#'
#' Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and
#' market microstructure noise. *Journal of Econometrics* 168(2), 396-406.
#' @seealso [integrated_variance()], [noise_variance()]
#' @examples
#' set.seed(1)
#' n <- 86400
#' y <- cumsum(c(0, rnorm(n, 0, 0.03 / sqrt(n)))) + rnorm(n + 1, 0, 5e-5)
#' y[50001:(n + 1)] <- y[50001:(n + 1)] + 0.05 # one 5% jump
#' sqrt(iv_pv2009(y)@value)                     # close to 0.03
#' sqrt(iv_realized(y)@value)                   # jump and noise inflate RV
#' @export
iv_pv2009 <- function(x, K = NULL, L = NULL,
                      constants = c("finite_sample", "asymptotic"),
                      noise = c("autocovariance", "pv2009"),
                      noise_variance = NULL) {
  y <- log_prices(x)
  n <- length(y) - 1L
  if (n < 4L) hf_abort("`iv_pv2009()` needs at least 4 increments, not %d.", n)
  constants <- if (missing(constants)) "finite_sample" else
    check_choice(constants, c("finite_sample", "asymptotic"), "constants")
  noise <- if (missing(noise)) "autocovariance" else
    check_choice(noise, c("autocovariance", "pv2009"), "noise")
  dy <- diff(y)
  if (is.null(noise_variance)) {
    omega2 <- if (noise == "autocovariance") noise_autocov(dy, 0L, FALSE) else
      sum(dy^2) / (2 * n)
  } else {
    omega2 <- check_number(noise_variance, "noise_variance", lower = 0)
    noise <- "user"
  }
  K <- check_count(K, "K", min = 1L, allow_null = TRUE)
  L <- check_count(L, "L", min = 2L, allow_null = TRUE)

  pilot_iv <- NULL
  if (is.null(K)) {
    # PV (2009, eq. 3.20) with a pilot sigma^2 from c1 = 1, c2 = 8/5.
    K0 <- max(1L, as.integer(round(sqrt(n))))
    L0 <- pv_block_length(K0)
    if (n %/% L0 < 2L) {
      K0 <- 1L
      L0 <- 2L
    }
    pilot_iv <- pv_mbv(y, K0, L0, omega2, "finite_sample")
    if (is.finite(pilot_iv) && pilot_iv > 0) {
      c1 <- sqrt(18 / ((8 / 5 - 1) * (4 - 8 / 5))) * sqrt(omega2 / pilot_iv)
      K <- max(1L, as.integer(round(c1 * sqrt(n))))
    } else {
      hf_warn("The pilot estimate for the tuning rule (3.20) is not positive; using K = %d.", K0)
      K <- K0
    }
    K <- min(K, max(1L, as.integer(floor(n / 3.2))))
    if (is.null(L)) {
      while (K > 1L && n %/% pv_block_length(K) < 2L) K <- K - 1L
    }
  }
  if (is.null(L)) L <- pv_block_length(K)
  if (L <= K) hf_abort("`L` (%d) must be larger than `K` (%d).", L, K)
  M <- n %/% L
  if (M < 2L) {
    hf_abort("With L = %d only %d block%s of the %d increments fit; at least 2 are needed.",
             L, M, if (M == 1L) "" else "s", n)
  }
  tuning <- list(K = K, L = L, M = M, c1 = K / sqrt(n), c2 = L / K,
                 constants = constants, noise = noise)
  if (!is.null(pilot_iv)) tuning$pilot_iv <- pilot_iv
  new_iv_estimate(
    value = pv_mbv(y, K, L, omega2, constants),
    method = "pv2009", reference = "Podolskij and Vetter (2009, eq. 3.10)",
    n = n, jump_robust = TRUE, noise_variance = omega2, tuning = tuning
  )
}

# Block length L = max(K + 1, round(c2 K)) with c2 = 8/5 (PV 2009, eq. 3.20).
pv_block_length <- function(K) max(K + 1L, as.integer(round(8 * K / 5)))

# Block averages of K-step returns, PV (2009, eq. 2.4): block m = 1..M averages
# Y_{i+K} - Y_i over i = (m-1)L, ..., mL - K.
pv_block_means <- function(y, K, L) {
  n <- length(y) - 1L
  nb <- L - K + 1L
  d <- y[(K + 1L):(n + 1L)] - y[seq_len(n - K + 1L)] # Y_{i+K} - Y_i, i = 0..n-K
  cs <- c(0, cumsum(d))
  start <- (seq_len(n %/% L) - 1L) * L
  (cs[start + nb + 1L] - cs[start + 1L]) / nb
}

# Exact moments of the block averages for integer K, L (T = 1, Delta_n = 1/n):
# Var(Zbar_m) = IV * signal + omega^2 * noise, Cov(Zbar_m, Zbar_{m+1}) =
# omega^2 * cross. The efficient return dX_{(m-1)L+j} enters Zbar_m with
# weight w_j / (L - K + 1).
pv_moments <- function(n, K, L) {
  nb <- L - K + 1L
  j <- seq_len(L)
  w <- pmin(j, K, nb, L + 1L - j)
  c(signal = sum(w^2) / (n * nb^2), noise = 2 * min(K, nb) / nb^2,
    cross = -1 / nb^2)
}

pv_mbv <- function(y, K, L, omega2, constants) {
  n <- length(y) - 1L
  zb <- pv_block_means(y, K, L)
  M <- length(zb)
  mbv11 <- sum(abs(zb[-M]) * abs(zb[-1L])) # eq. (2.3), r = l = 1
  mu1_sq <- abs_normal_moment(1)^2
  if (constants == "asymptotic") {
    c1 <- K / sqrt(n)
    c2 <- L / K
    nu1 <- c1 * (3 * c2 - 4 + max((2 - c2)^3, 0)) / (3 * (c2 - 1)^2) # (3.3)
    nu2 <- 2 * min(c2 - 1, 1) / (c1 * (c2 - 1)^2)
    return(((c1 * c2 / mu1_sq) * mbv11 - nu2 * omega2) / nu1) # (3.10)
  }
  mom <- pv_moments(n, K, L)
  # Solve E|Zbar_m||Zbar_{m+1}| = mu1^2 V h(rho), rho = omega^2 cross / V,
  # h(rho) = sqrt(1 - rho^2) + rho asin(rho), for V; h is increasing in V.
  target <- mbv11 / ((M - 1L) * mu1_sq)
  v <- target
  cov <- omega2 * mom[["cross"]]
  if (target > 0 && cov < 0) {
    for (it in 1:100) {
      rho <- max(cov / v, -1)
      v_new <- target / (sqrt(1 - rho^2) + rho * asin(rho))
      done <- abs(v_new - v) <= 1e-13 * v
      v <- v_new
      if (done) break
    }
  }
  (v - omega2 * mom[["noise"]]) / mom[["signal"]]
}
