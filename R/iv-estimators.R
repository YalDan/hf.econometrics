# Shared helpers for the integrated-variance estimators ------------------------

# Log returns of one window. Every estimator reads its input through
# log_prices(), so `x` may be a numeric vector or an HFSeries.
iv_returns <- function(x, min_n, fun) {
  dy <- diff(log_prices(x))
  if (length(dy) < min_n) {
    hf_abort("`%s()` needs at least %d increments, not %d.", fun, min_n,
             length(dy))
  }
  dy
}

# Builds the IVEstimate. A negative estimate is returned as is (it is what the
# formula gives, and truncating it would bias averages across windows), but it
# is flagged: Hautsch and Podolskij (2013, Sec. 2.4) bound their estimates
# below by zero in applications, and sqrt(value) is undefined.
new_iv_estimate <- function(value, method, reference, n, jump_robust,
                            noise_variance = NA_real_, tuning = list()) {
  value <- as.double(value)
  if (is.finite(value) && value < 0) {
    hf_warn(paste0("The %s estimate of integrated variance is negative (%.3g). ",
                   "Hautsch and Podolskij (2013) bound such estimates below ",
                   "by zero."), method, value)
  }
  IVEstimate(
    value = value,
    noise_variance = as.double(noise_variance),
    jump_robust = jump_robust,
    n = as.integer(n),
    tuning = tuning,
    method = method,
    reference = reference
  )
}

# Realized variance and bipower variation --------------------------------------

#' Realized variance
#'
#' Computes the realized variance \eqn{RV = \sum_{i=1}^n (\Delta_i Y)^2} of one
#' window. Without noise it is consistent for the quadratic variation
#' \eqn{[X]_T = \int_0^T \sigma_s^2 ds + \sum_{s \le T} (\Delta X_s)^2}. Under
#' additive i.i.d. noise of variance \eqn{\omega^2} it is dominated by
#' \eqn{2 n \omega^2} and diverges (Podolskij and Vetter 2009, eq. 3.5;
#' Hautsch and Podolskij 2013, eq. 2.11).
#'
#' @section Paper mapping:
#' * Barndorff-Nielsen and Shephard (2002) and Andersen, Bollerslev, Diebold
#'   and Labys (2003): \eqn{RV = \sum_i (\Delta_i Y)^2}, the sum of squared
#'   log returns over the window.
#'
#' @section Implementation choices:
#' None: the estimator has no tuning parameter.
#'
#' @param x Log prices of one window: a numeric vector or an [HFSeries],
#'   assumed equally spaced.
#' @return An [IVEstimate] with `jump_robust = FALSE`.
#' @references
#' Andersen, T. G., Bollerslev, T., Diebold, F. X. and Labys, P. (2003).
#' Modeling and forecasting realized volatility. *Econometrica* 71(2),
#' 579-625.
#'
#' Barndorff-Nielsen, O. E. and Shephard, N. (2002). Econometric analysis of
#' realized volatility and its use in estimating stochastic volatility
#' models. *Journal of the Royal Statistical Society B* 64(2), 253-280.
#' @seealso [integrated_variance()]
#' @examples
#' y <- cumsum(c(0, rnorm(1000, 0, 0.03 / sqrt(1000))))
#' iv_realized(y)
#' @export
iv_realized <- function(x) {
  dy <- iv_returns(x, 1L, "iv_realized")
  new_iv_estimate(
    value = sum(dy^2), method = "rv",
    reference = "Barndorff-Nielsen and Shephard (2002); Andersen et al. (2003)",
    n = length(dy), jump_robust = FALSE
  )
}

#' Realized bipower variation
#'
#' Computes the jump-robust estimator of integrated variance of
#' Barndorff-Nielsen and Shephard (2004),
#' \deqn{BV = \mu_1^{-2} \sum_{j=1}^{n-1} |\Delta_j Y| \, |\Delta_{j+1} Y|,
#'       \qquad \mu_1 = E|U| = \sqrt{2/\pi},}
#' which converges to \eqn{\int_0^T \sigma_s^2 ds} in a stochastic volatility
#' model with finitely many jumps. It is not robust to microstructure noise.
#'
#' @section Paper mapping:
#' * BNS (2004, eq. 3): realised bipower variation
#'   \eqn{\{y^*_M\}^{[1,1]} = \sum_{j=1}^{M-1} |y_j| |y_{j+1}|} over the
#'   \eqn{M} returns \eqn{y_j} of the window (here \eqn{M = n}). The general
#'   definition carries the factor \eqn{(\hbar/M)^{1 - (r+s)/2}}, which equals
#'   1 for \eqn{r = s = 1}.
#' * BNS (2004, Sec. 2.4, after Theorem 2): \eqn{\mu_1^{-2}\{y^*_M\}^{[1,1]}
#'   \to \int \sigma^2(u) du}; jump robustness is their Theorem 5.
#'
#' @section Implementation choices:
#' * BNS (2004) sum the \eqn{n - 1} adjacent products without rescaling, and
#'   that is the default. `adjust = TRUE` multiplies by \eqn{n/(n-1)} to
#'   account for the missing summand, a finite-sample convention used in
#'   later work; it is not part of BNS (2004).
#'
#' @inheritParams iv_realized
#' @param adjust Multiply by \eqn{n / (n - 1)}? Defaults to `FALSE`, the
#'   definition of BNS (2004).
#' @return An [IVEstimate] with `jump_robust = TRUE`.
#' @references
#' Barndorff-Nielsen, O. E. and Shephard, N. (2004). Power and bipower
#' variation with stochastic volatility and jumps. *Journal of Financial
#' Econometrics* 2(1), 1-37.
#' @seealso [integrated_variance()]
#' @examples
#' y <- cumsum(c(0, rnorm(1000, 0, 0.03 / sqrt(1000))))
#' y[600:1001] <- y[600:1001] + 0.05 # a jump
#' iv_realized(y)@value
#' iv_bipower(y)@value
#' @export
iv_bipower <- function(x, adjust = FALSE) {
  check_flag(adjust, "adjust")
  dy <- iv_returns(x, 2L, "iv_bipower")
  n <- length(dy)
  bv <- sum(abs(dy[-n]) * abs(dy[-1L])) / abs_normal_moment(1)^2
  if (adjust) bv <- bv * n / (n - 1)
  new_iv_estimate(
    value = bv, method = "bv",
    reference = "Barndorff-Nielsen and Shephard (2004, eq. 3)",
    n = n, jump_robust = TRUE, tuning = list(adjust = adjust)
  )
}

# Dispatcher -------------------------------------------------------------------

iv_methods <- c("rv", "bv", "jlmpv2009", "jpv2010", "pv2009", "hp2013")

#' Integrated variance of one window
#'
#' Single entry point to the integrated-variance estimators of the package.
#' `method` selects the estimator and `...` is passed on to it unchanged.
#'
#' | `method` | function | source | noise-robust | jump-robust |
#' |---|---|---|---|---|
#' | `"rv"` | [iv_realized()] | BNS (2002), ABDL (2003) | no | no |
#' | `"bv"` | [iv_bipower()] | BNS (2004) | no | yes |
#' | `"jlmpv2009"` | [iv_jlmpv2009()] | JLMPV (2009, eq. 3.6) | yes | no |
#' | `"jpv2010"` | [iv_jpv2010()] | JPV (2010, eq. 3.5) | yes | no |
#' | `"pv2009"` | [iv_pv2009()] | PV (2009, eq. 3.10) | yes | yes |
#' | `"hp2013"` | [iv_hp2013()] | HP (2013) | yes | with `estimator = "bipower"` |
#'
#' The estimators that are not jump-robust estimate the quadratic variation
#' \eqn{\int_0^T \sigma_s^2 ds + \sum_{s \le T} (\Delta X_s)^2}.
#'
#' @section Paper mapping:
#' See the documentation of each estimator.
#'
#' @section Implementation choices:
#' * `method` defaults to `"rv"`, the first entry. Arguments in `...` must be
#'   named arguments of the selected estimator; anything else raises an
#'   `hf_input_error`, so that a misspelled option is not silently dropped.
#' * All estimators treat `x` as one window of equally spaced observations
#'   with \eqn{T = 1} and \eqn{\Delta_n = 1/n}, so the value is the
#'   integrated variance over the window, on the scale of a sum of squared log
#'   returns. To estimate a volatility from k-subsampled prices, pass the
#'   subsampled log prices: the window is the same, only \eqn{n} changes.
#'
#' @inheritParams iv_realized
#' @param method Estimator, one of `"rv"`, `"bv"`, `"jlmpv2009"`, `"jpv2010"`,
#'   `"pv2009"`, `"hp2013"`.
#' @param ... Named arguments passed to the selected estimator.
#' @return An [IVEstimate].
#' @references See the individual estimators.
#' @examples
#' set.seed(1)
#' n <- 23400
#' y <- cumsum(c(0, rnorm(n, 0, 0.03 / sqrt(n)))) + rnorm(n + 1, 0, 2e-4)
#' sqrt(integrated_variance(y, "rv")@value) # inflated by the noise
#' sqrt(integrated_variance(y, "pv2009")@value)
#' sqrt(integrated_variance(y, "hp2013", theta = 0.4)@value)
#' @export
integrated_variance <- function(x, method = c("rv", "bv", "jlmpv2009",
                                              "jpv2010", "pv2009", "hp2013"),
                                ...) {
  method <- if (missing(method)) iv_methods[1L] else
    check_choice(method, iv_methods, "method")
  fun <- switch(method,
    rv = iv_realized, bv = iv_bipower, jlmpv2009 = iv_jlmpv2009,
    jpv2010 = iv_jpv2010, pv2009 = iv_pv2009, hp2013 = iv_hp2013
  )
  dots <- list(...)
  if (length(dots)) {
    nms <- names(dots)
    if (is.null(nms) || any(!nzchar(nms))) {
      hf_abort("Arguments in `...` must be named.")
    }
    bad <- setdiff(nms, setdiff(names(formals(fun)), "x"))
    if (length(bad)) {
      hf_abort("Method \"%s\" has no argument%s %s.", method,
               if (length(bad) > 1L) "s" else "",
               paste0("`", bad, "`", collapse = ", "))
    }
  }
  do.call(fun, c(list(x), dots))
}
