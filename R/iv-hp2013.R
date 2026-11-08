#' Pre-averaging estimators of Hautsch and Podolskij (2013)
#'
#' Computes the pre-averaging estimators of Hautsch and Podolskij (2013):
#' the pre-averaged realized variance \eqn{C_t} (eq. 2.12), consistent for
#' the quadratic variation and hence for the integrated variance when there
#' are no jumps; its version \eqn{C_t(q)} for \eqn{q}-dependent noise (eq.
#' 3.21); and the jump-robust pre-averaged bipower variation \eqn{BT_t}
#' (Corollary 4.3),
#' \deqn{C_t = \frac{\sqrt{\Delta_n}}{\theta\psi_2} \sum_{i=0}^{n-k_n}
#'       |\bar Z_i|^2 - \frac{\psi_1\Delta_n}{2\theta^2\psi_2} RV_t, \qquad
#'       BT_t = \frac{\sqrt{\Delta_n}}{\theta m_1^2 \psi_2}
#'       \sum_{i=0}^{n - 2k_n + 1} |\bar Z_i||\bar Z_{i+k_n}| -
#'       \frac{\psi_1\Delta_n}{2\theta^2\psi_2} RV_t,}
#' by default with the finite-sample adjustments of Sections 2.3, 3.1 and
#' 4.1. In the univariate case these are the estimators behind the
#' "modulated realized covariance" of `highfrequency::rMRC()`.
#'
#' @section Paper mapping:
#' * Eq. (2.7): \eqn{k_n\sqrt{\Delta_n} = \theta}; `theta` or `kn`.
#' * Eqs. (2.8)-(2.10): pre-averaged returns \eqn{\bar Z_i}, \eqn{RV_t},
#'   \eqn{V(Z, 2)_t = \sum_{i=0}^{n-k_n} |\bar Z_i|^2}.
#' * Eqs. (2.11)-(2.12): \eqn{C_t}; `estimator = "preaveraged"`,
#'   `noise = "iid"`, `adjust = FALSE`, with \eqn{\psi_1 = \int (g')^2},
#'   \eqn{\psi_2 = \int g^2} (Example 2.2: 1 and 1/12 for the tent).
#' * Section 2.3: \eqn{C_{t,a} = (1 - \frac{\psi_1^{k_n}\Delta_n}{2\theta^2
#'   \psi_2^{k_n}})^{-1}(\frac{n\sqrt{\Delta_n}}{(n - k_n + 1)\theta
#'   \psi_2^{k_n}} V(Z, 2)_t - \frac{\psi_1^{k_n}\Delta_n}{2\theta^2
#'   \psi_2^{k_n}} RV_t)}; `adjust = TRUE`.
#' * Section 2.4 and Section 6: \eqn{\theta} between 0.4 and 0.8 as a rule
#'   of thumb (default 0.6), or \eqn{\theta^* = \arg\min_\theta MSE} against
#'   a sparse realized-variance benchmark over a grid in \eqn{[0.05, 3]};
#'   `theta = "mse"`, `benchmark`, `sparse_lag`, `n_grids`, `theta_grid`.
#'   The MSE uses \eqn{\Gamma_{t,a}} of Section 2.3 (Proposition 2.4 with
#'   finite-sample constants \eqn{\psi^{k_n}}, \eqn{\Phi^{k_n}}).
#' * Lemma 3.1, eqs. (3.19)-(3.21), Lemma 3.2: \eqn{C_t(q) =
#'   \frac{\sqrt{\Delta_n}}{\theta\psi_2} V(Z, 2)_t - \frac{\psi_1 t}{\theta^2
#'   \psi_2}\hat\rho^2} with the long-run noise variance \eqn{\hat\rho^2} of
#'   the autocovariance recursion; `noise = "dependent"`, `q`. Eq. (3.24):
#'   \eqn{C_{t,a}(q)}.
#' * Eq. (4.30) and Corollary 4.3: \eqn{V(Z, 1, 1)_t} and \eqn{BT_t};
#'   `estimator = "bipower"`. Section 4.1: \eqn{BT_{t,a}}.
#' * Appendix: the RV10 benchmark, an average of sparse realized variances
#'   over 100 equidistant starting points, each scaled to the window.
#'
#' @section Implementation choices:
#' * Default \eqn{\theta = 0.6}, the middle of the range 0.4-0.8 that the
#'   authors suggest for a data-independent choice (Section 6). \eqn{k_n =
#'   \max(3, \mathrm{round}(\theta\sqrt n))} and \eqn{\theta} is re-defined
#'   as \eqn{k_n/\sqrt n}. The authors' own empirical choice is the
#'   data-driven `theta = "mse"`, which needs a benchmark.
#' * Number of summands: \eqn{V(Z, 2)_t} runs over \eqn{i = 0, \dots, n -
#'   k_n} (eq. 2.10) and the adjustment divides by that number, \eqn{n - k_n
#'   + 1}, as the text of Section 2.3 and eq. (3.24) say (the display of
#'   \eqn{C_{t,a}} prints \eqn{n - k_n + 2}). \eqn{V(Z, 1, 1)_t} runs over
#'   \eqn{i = 0, \dots, n - 2k_n + 1} (eq. 4.30) and is scaled by \eqn{n/(n -
#'   2k_n + 2)} (Section 4.1).
#' * \eqn{\psi_1^{k_n} = k_n\sum_{j=0}^{k_n-1}(g_{j+1} - g_j)^2}, the full
#'   sum of squared weight differences; the display in Section 2.3 starts at
#'   \eqn{j = 1} (see [iv_jlmpv2009()]).
#' * The factor \eqn{A^{-1} = (1 - \psi_1^{k_n}\Delta_n/(2\theta^2
#'   \psi_2^{k_n}))^{-1}} of \eqn{C_{t,a}} removes the bias
#'   \eqn{-\psi_1\Delta_n IV/(2\theta^2\psi_2)} that the RV-based noise
#'   correction introduces (Section 2.3). `bias_factor = TRUE` (default)
#'   applies it to every adjusted estimator:
#'   * \eqn{BT_{t,a}}: Section 4.1 lists the finite-sample constants and the
#'     summand count and calls the result the "corresponding" estimator; the
#'     factor removes the same bias. With it \eqn{BT_{t,a}} is unbiased for
#'     constant volatility and Gaussian i.i.d. noise; `bias_factor = FALSE`
#'     gives the version with only the listed adjustments.
#'   * \eqn{C_{t,a}(q)}: eq. (3.24) includes the factor. Because
#'     \eqn{\hat\rho^2} carries no bias proportional to \eqn{IV}, it then
#'     over-corrects by about \eqn{\psi_1/(2k_n^2\psi_2)} (\eqn{6/k_n^2} for
#'     the tent; 25% at \eqn{k_n = 5}), the upward bias for small
#'     \eqn{\theta} that the authors observe in Section 3.1. It is negligible
#'     at the default \eqn{\theta} on long windows; `bias_factor = FALSE`
#'     removes it.
#' * `noise = "dependent"` is defined for `estimator = "preaveraged"` only,
#'   as in the paper. Default `q = 1`, the authors' empirical
#'   \eqn{C_{t,a}(1)}.
#' * `theta = "mse"` minimises \eqn{(C_{t,a} - IV_{bench})^2 +
#'   \Delta_n^{1/2}\Gamma_{t,a}} over `theta_grid` (default
#'   \eqn{0.05, 0.10, \dots, 3}). The variance of \eqn{C_{t,a}} is
#'   \eqn{\Delta_n^{1/2}\Gamma_{t,a}} (eq. 2.13); the criterion displayed in
#'   Section 2.4 omits the factor \eqn{\Delta_n^{1/2}}. In
#'   \eqn{\Gamma_{t,a}} the middle sum runs over \eqn{i = 0, \dots, n - 2k_n},
#'   where its inner window fits (the display prints \eqn{n - k_n}), and is
#'   scaled by \eqn{n/(n - 2k_n + 1)}. As in Section 2.4 the criterion uses
#'   the i.i.d.-noise \eqn{C_{t,a}} and \eqn{\Gamma_{t,a}}; the selected
#'   \eqn{\theta} is then used for the requested estimator. Within the
#'   search the pre-averaged returns are computed by FFT convolution (exact
#'   up to rounding); the reported estimate uses the direct sums.
#' * The benchmark is `benchmark` if given; otherwise the RV10-type
#'   average of sparse realized variances with returns over `sparse_lag`
#'   observations (600 for 10 minutes of one-second data) starting at
#'   `n_grids` equidistant offsets, each scaled by \eqn{n} over the number of
#'   increments it covers.
#' * `noise_variance` of the result is \eqn{\Delta_n RV/2} (eq. 2.11) for
#'   `noise = "iid"` and \eqn{\hat\rho(0)} for `noise = "dependent"`; the
#'   long-run \eqn{\hat\rho^2} is in `tuning$long_run_noise_variance`.
#' * Estimates are not truncated at zero (the authors do so in their
#'   application); a negative value raises a warning.
#'
#' @inheritParams iv_jlmpv2009
#' @param theta \eqn{\theta = k_n/\sqrt n}, a positive number, or `"mse"` for
#'   the data-driven choice of Section 2.4.
#' @param estimator `"preaveraged"` (\eqn{C_t}, not jump-robust) or
#'   `"bipower"` (\eqn{BT_t}, jump-robust).
#' @param noise `"iid"` (eq. 2.12) or `"dependent"` (eq. 3.21).
#' @param q Dependence order of the noise for `noise = "dependent"`.
#' @param adjust Use the finite-sample adjustments?
#' @param bias_factor With `adjust = TRUE`, apply the factor
#'   \eqn{(1 - \psi_1^{k_n}\Delta_n/(2\theta^2\psi_2^{k_n}))^{-1}}?
#' @param benchmark For `theta = "mse"`: a benchmark integrated variance. If
#'   `NULL`, the sparse realized variance defined by `sparse_lag` is used.
#' @param sparse_lag For `theta = "mse"` without `benchmark`: the number of
#'   observations spanned by one sparse return.
#' @param n_grids Number of starting points averaged in the sparse benchmark.
#' @param theta_grid Values of \eqn{\theta} searched by `theta = "mse"`.
#' @return An [IVEstimate]. `jump_robust` is `TRUE` for `estimator =
#'   "bipower"`.
#' @references
#' Hautsch, N. and Podolskij, M. (2013). Preaveraging-based estimation of
#' quadratic variation in the presence of noise and jumps: theory,
#' implementation, and empirical evidence. *Journal of Business & Economic
#' Statistics* 31(2), 165-183. Working paper version: CREATES Research Paper
#' 2010-29.
#' @seealso [integrated_variance()], [iv_jlmpv2009()], [noise_variance()]
#' @examples
#' set.seed(1)
#' n <- 23400
#' y <- cumsum(c(0, rnorm(n, 0, 0.02 / sqrt(n)))) + rnorm(n + 1, 0, 3e-4)
#' y[15001:(n + 1)] <- y[15001:(n + 1)] + 0.02 # one jump
#' sqrt(iv_hp2013(y)@value)                       # quadratic variation
#' sqrt(iv_hp2013(y, estimator = "bipower")@value) # about 0.02
#' sqrt(iv_hp2013(y, theta = "mse", sparse_lag = 200)@value)
#' @export
iv_hp2013 <- function(x, theta = 0.6, kn = NULL, weight = weight_tent(),
                      estimator = c("preaveraged", "bipower"),
                      noise = c("iid", "dependent"), q = 1L, adjust = TRUE,
                      bias_factor = TRUE, benchmark = NULL, sparse_lag = NULL,
                      n_grids = 100L, theta_grid = seq(0.05, 3, by = 0.05)) {
  y <- log_prices(x)
  dy <- diff(y)
  n <- length(dy)
  if (n < 8L) hf_abort("`iv_hp2013()` needs at least 8 increments, not %d.", n)
  check_theta(theta, "mse")
  check_class(weight, WeightFunction, "weight")
  estimator <- if (missing(estimator)) "preaveraged" else
    check_choice(estimator, c("preaveraged", "bipower"), "estimator")
  noise <- if (missing(noise)) "iid" else
    check_choice(noise, c("iid", "dependent"), "noise")
  q <- check_count(q, "q", min = 0L, max = n - 2L)
  check_flag(adjust, "adjust")
  check_flag(bias_factor, "bias_factor")
  check_number(benchmark, "benchmark", allow_null = TRUE)
  sparse_lag <- check_count(sparse_lag, "sparse_lag", min = 1L, max = n %/% 2L,
                            allow_null = TRUE)
  n_grids <- check_count(n_grids, "n_grids", min = 1L)
  theta_grid <- check_numeric(theta_grid, "theta_grid")
  if (any(theta_grid <= 0)) hf_abort("`theta_grid` must be positive.")
  if (noise == "dependent" && estimator == "bipower") {
    hf_abort("`noise = \"dependent\"` is defined for `estimator = \"preaveraged\"` only (eq. 3.21).")
  }
  max_kn <- if (estimator == "bipower") (n + 1L) %/% 2L else n

  tuning <- list()
  if (is.null(kn) && is.character(theta)) {
    sel <- hp_select_theta(y, dy, weight, benchmark, sparse_lag, n_grids,
                           theta_grid, max_kn)
    kn <- sel$kn
    tuning <- list(theta_selection = "mse", benchmark = sel$benchmark,
                   mse = sel$mse)
  } else {
    kn <- preavg_kn(n, kn, theta, max_kn)
  }

  zbar <- preavg_zbar(dy, weight, kn) # i = 0..n-kn+1
  psi <- if (adjust) preavg_psi(weight, kn) else preavg_psi_limit(weight)
  noise_coef <- psi[["psi1"]] / (2 * kn^2 * psi[["psi2"]]) # psi1 Dn/(2 theta^2 psi2)
  if (adjust && bias_factor && noise_coef >= 1) {
    hf_abort("kn = %d is too small for this weight: the noise correction is degenerate.", kn)
  }
  if (estimator == "preaveraged") {
    nsum <- n - kn + 1L
    main <- sum(zbar[seq_len(nsum)]^2) / (kn * psi[["psi2"]]) # eq. (2.10)
  } else {
    nsum <- n - 2L * kn + 2L
    v11 <- sum(abs(zbar[seq_len(nsum)]) * abs(zbar[kn + seq_len(nsum)])) # (4.30)
    main <- v11 / (abs_normal_moment(1)^2 * kn * psi[["psi2"]])
  }
  if (adjust) main <- main * n / nsum
  if (noise == "iid") {
    omega2 <- sum(dy^2) / (2 * n)
    correction <- noise_coef * 2 * n * omega2 # psi1 Dn RV / (2 theta^2 psi2)
  } else {
    omega2 <- noise_autocov(dy, q, FALSE)
    rho2 <- noise_autocov(dy, q, TRUE)
    correction <- noise_coef * 2 * n * rho2 # psi1 t rho^2 / (theta^2 psi2)
    tuning$long_run_noise_variance <- rho2
  }
  value <- main - correction
  if (adjust && bias_factor) value <- value / (1 - noise_coef)

  reference <- sprintf("Hautsch and Podolskij (2013, %s)", switch(
    paste(estimator, noise),
    "preaveraged iid" = if (adjust) "Sec. 2.3" else "eq. 2.12",
    "preaveraged dependent" = if (adjust) "eq. 3.24" else "eq. 3.21",
    "bipower iid" = if (adjust) "Sec. 4.1" else "Cor. 4.3"
  ))
  tuning <- c(list(kn = kn, theta = kn / sqrt(n), weight = weight@name,
                   estimator = estimator, noise = noise,
                   q = if (noise == "dependent") q else NULL,
                   adjust = adjust,
                   bias_factor = if (adjust) bias_factor else NULL), tuning)
  new_iv_estimate(
    value = value, method = "hp2013", reference = reference, n = n,
    jump_robust = estimator == "bipower", noise_variance = omega2,
    tuning = tuning[!vapply(tuning, is.null, logical(1))]
  )
}

# C_{t,a} and Gamma_{t,a} of HP (2013, Sec. 2.3) for one window kn, with the
# finite-sample constants psi^kn and Phi^kn; Gamma_{t,a} estimates the
# asymptotic variance of Delta_n^{-1/4} (C_{t,a} - IV). `zbar` holds the
# pre-averaged returns for i = 0..n-kn.
hp_c_gamma <- function(dy, weight, kn, zbar = NULL) {
  n <- length(dy)
  if (is.null(zbar)) zbar <- preavg_zbar(dy, weight, kn)[seq_len(n - kn + 1L)]
  psi <- preavg_psi(weight, kn)
  ph <- preavg_phi(weight, kn)
  psi1 <- psi[["psi1"]]
  psi2 <- psi[["psi2"]]
  theta <- kn / sqrt(n)
  dn <- 1 / n
  a <- 1 - psi1 * dn / (2 * theta^2 * psi2)
  rv <- sum(dy^2)
  c_adj <- (n * sqrt(dn) / ((n - kn + 1) * theta * psi2) * sum(zbar^2) -
              psi1 * dn / (2 * theta^2 * psi2) * rv) / a
  s1 <- sum(zbar^4)
  cs2 <- c(0, cumsum(dy^2))
  i <- 0:(n - 2L * kn) # sum_{j=i+kn+1}^{i+2kn} (dZ_j)^2
  s2 <- sum(zbar[i + 1L]^2 * (cs2[i + 2L * kn + 1L] - cs2[i + kn + 1L]))
  s3 <- sum(dy[seq_len(n - 2L)]^2 * dy[3:n]^2)
  gamma <- (4 * ph[["Phi22"]] * n / (3 * theta * psi2^4 * (n - kn + 1)) * s1 +
    4 * dn * n / (theta^3 * (n - 2 * kn + 1)) *
      (ph[["Phi12"]] / psi2^3 - ph[["Phi22"]] * psi1 / psi2^4) * s2 +
    dn * n / (theta^3 * (n - 2)) *
      (ph[["Phi11"]] / psi2^2 - 2 * ph[["Phi12"]] * psi1 / psi2^3 +
         ph[["Phi22"]] * psi1^2 / psi2^4) * s3) / a^2
  c(c_adj = c_adj, gamma = gamma)
}

# Average of sparse realized variances with returns over `lag` observations,
# starting at `n_grids` equidistant offsets in 0..lag-1, each scaled to the
# window (HP 2013, Appendix: RV10).
hp_sparse_rv <- function(y, lag, n_grids) {
  n <- length(y) - 1L
  offsets <- unique(as.integer(round(seq(0, lag - 1L,
                                         length.out = min(n_grids, lag)))))
  mean(vapply(offsets, function(o) {
    r <- diff(y[seq(o + 1L, n + 1L, by = lag)])
    sum(r^2) * n / (length(r) * lag)
  }, numeric(1)))
}

# Arguments are validated by iv_hp2013().
hp_select_theta <- function(y, dy, weight, benchmark, sparse_lag, n_grids,
                            theta_grid, max_kn) {
  n <- length(dy)
  if (is.null(benchmark)) {
    if (is.null(sparse_lag)) {
      hf_abort("`theta = \"mse\"` needs `benchmark` or `sparse_lag`.")
    }
    benchmark <- hp_sparse_rv(y, sparse_lag, n_grids)
  }
  kn_grid <- unique(pmax(3L, as.integer(round(theta_grid * sqrt(n)))))
  kn_grid <- kn_grid[kn_grid <= min(max_kn, (n - 1L) %/% 2L)]
  if (!length(kn_grid)) hf_abort("No value of `theta_grid` fits this window.")
  # FFT convolution: Zbar_i = sum_{j=1}^{kn-1} g_j dZ_{i+j} for i = 0..n-kn is
  # entry i + kn - 1 of the convolution of dZ with rev(g); the transform of dZ
  # is shared by the whole grid, and the padding avoids wrap-around.
  nfft <- 2^ceiling(log2(n + max(kn_grid)))
  fdy <- stats::fft(c(dy, numeric(nfft - n)))
  mse <- vapply(kn_grid, function(k) {
    g <- weight@fun(seq_len(k - 1L) / k)
    conv <- Re(stats::fft(fdy * stats::fft(c(rev(g), numeric(nfft - k + 1L))),
                          inverse = TRUE)) / nfft
    cg <- hp_c_gamma(dy, weight, k, zbar = conv[(k - 1L):(n - 1L)])
    (cg[["c_adj"]] - benchmark)^2 + cg[["gamma"]] / sqrt(n)
  }, numeric(1))
  best <- which.min(mse)
  list(kn = kn_grid[best], benchmark = benchmark, mse = mse[best])
}
