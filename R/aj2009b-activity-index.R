#' @include class-result.R aj-power-variation.R
NULL

#' Options for the Ait-Sahalia and Jacod (2009b) jump activity index
#'
#' Tuning parameters of [aj_activity_index()]. Create them with
#' [aj_activity_index_options()].
#'
#' @param estimator `"truncation"` (\eqn{\hat\beta_n}, two cutoffs, eq. 16)
#'   or `"frequency"` (\eqn{\hat\beta'_n}, two sampling frequencies, eq. 17).
#' @param alpha Cutoff levels \eqn{\alpha} (a vector averages the estimates,
#'   the AVG procedure of Section 7).
#' @param alpha_ratio Ratios \eqn{\alpha'/\alpha > 1} of the second cutoff
#'   (estimator `"truncation"`; a vector is averaged as well).
#' @param k Sampling multiple of estimator `"frequency"` (the paper uses 2).
#' @param bias_correction `"regression"` (eqs. 44-46), `"plugin"`
#'   (eqs. 42-43) or `"none"`.
#' @param bias_grid Cutoff levels, in the units of `alpha`, of the
#'   regression behind `bias_correction = "regression"`; empty means a grid
#'   with unit steps from `min(alpha)` to `max(alpha * alpha_ratio)`.
#' @param level One minus the confidence level of the reported interval.
#' @param truncation,varpi,sigma,pilot,horizon Truncation, see
#'   [aj_truncation()]. `varpi` is also the rate \eqn{\varpi} of the
#'   frequency estimator.
#' @export
AJActivityIndexOptions <- new_class(
  "AJActivityIndexOptions",
  properties = c(list(
    estimator = class_character,
    alpha_ratio = class_double,
    k = class_integer,
    bias_correction = class_character,
    bias_grid = class_double,
    level = class_double
  ), aj_truncation_properties),
  validator = function(self) {
    msgs <- collect_msgs(
      check_choice(self@estimator, c("truncation", "frequency"), "@estimator", abort = FALSE),
      if (!length(self@alpha_ratio) || anyNA(self@alpha_ratio) || any(self@alpha_ratio <= 1)) {
        "@alpha_ratio must be a vector of numbers greater than 1."
      },
      check_number(self@k, "@k", lower = 2, integerish = TRUE, abort = FALSE),
      check_choice(self@bias_correction, c("regression", "plugin", "none"),
                   "@bias_correction", abort = FALSE),
      if (anyNA(self@bias_grid) || any(self@bias_grid <= 0)) {
        "@bias_grid must contain positive numbers."
      },
      check_probability(self@level, "@level", abort = FALSE),
      aj_truncation_msgs(self, alpha_scalar = FALSE)
    )
    if (!is.null(msgs)) return(msgs)
    collect_msgs(
      if (self@estimator == "frequency" && self@bias_correction != "none") {
        "The bias corrections of AJ (2009b, Section 5) apply to the \"truncation\" estimator only."
      },
      if (self@varpi >= 0.5) "@varpi must be below 1/2 (AJ 2009b, eq. 14).",
      if (length(self@bias_grid) && length(unique(self@bias_grid)) < 4L) {
        "@bias_grid needs at least 4 distinct cutoffs (3 regression coefficients)."
      }
    )
  }
)

#' Create options for the Ait-Sahalia and Jacod (2009b) activity index
#'
#' The defaults follow the empirical application of Ait-Sahalia and Jacod
#' (2009b, Table 4): the two-cutoff estimator \eqn{\hat\beta_n} with
#' \eqn{\alpha} = 7 estimated standard deviations of the continuous part,
#' \eqn{\alpha' = 2\alpha}, \eqn{\varpi = 1/5} (Remark 7), and the
#' regression-based bias correction of Section 5.
#'
#' @inheritParams AJActivityIndexOptions
#' @param alpha Cutoff(s) in estimated standard deviations (sd) or multiples
#'   of `sigma` (rate); default 7. Use e.g. `alpha = 7:9` with
#'   `alpha_ratio = seq(1.5, 4, by = 0.5)` for the AVG estimate of Table 4.
#' @param alpha_ratio \eqn{\alpha'/\alpha}; default 2.
#' @param bias_correction Default `"regression"` for the truncation
#'   estimator and `"none"` for the frequency estimator.
#' @param bias_grid Regression cutoffs, or `NULL` for the default grid.
#' @param level One minus the confidence level (default 0.05: a 95%
#'   interval).
#' @param truncation `"sd"` (default) or `"rate"`, see [aj_truncation()].
#' @param varpi Rate \eqn{\varpi}; default 1/5 (Remark 7). It scales the
#'   cutoff across frequencies in `"frequency"` and the level in `"rate"`.
#' @param sigma,pilot,horizon See [aj_truncation()].
#' @return An [AJActivityIndexOptions] object.
#' @examples
#' aj_activity_index_options()
#' aj_activity_index_options(alpha = 7:9, alpha_ratio = seq(1.5, 4, by = 0.5))
#' @export
aj_activity_index_options <- function(estimator = "truncation", alpha = 7,
                                      alpha_ratio = 2, k = 2L,
                                      bias_correction = NULL, bias_grid = NULL,
                                      level = 0.05, truncation = "sd", varpi = 0.2,
                                      sigma = 0.25, pilot = 4, horizon = 1 / 252) {
  check_choice(estimator, c("truncation", "frequency"), "estimator")
  alpha <- check_numeric(alpha, "alpha")
  if (any(alpha <= 0)) hf_abort("`alpha` must be positive.")
  alpha_ratio <- check_numeric(alpha_ratio, "alpha_ratio")
  if (any(alpha_ratio <= 1)) hf_abort("`alpha_ratio` must be greater than 1.")
  k <- check_count(k, "k", min = 2L)
  if (is.null(bias_correction)) {
    bias_correction <- if (estimator == "truncation") "regression" else "none"
  }
  check_choice(bias_correction, c("regression", "plugin", "none"), "bias_correction")
  bias_grid <- if (is.null(bias_grid)) double() else check_numeric(bias_grid, "bias_grid")
  check_probability(level, "level")
  aj_check_truncation_args(truncation, varpi, sigma, pilot, horizon)
  aj_new(AJActivityIndexOptions,
    estimator = estimator, alpha = alpha, alpha_ratio = alpha_ratio, k = k,
    bias_correction = bias_correction, bias_grid = bias_grid,
    level = as.double(level), truncation = truncation, varpi = as.double(varpi),
    sigma = as.double(sigma), pilot = as.double(pilot), horizon = as.double(horizon)
  )
}

#' Estimate of the Ait-Sahalia and Jacod (2009b) jump activity index
#'
#' Returned by [aj_activity_index()].
#'
#' @param estimator `"truncation"` or `"frequency"`.
#' @param estimate \eqn{\hat\beta_n} (eq. 16) or \eqn{\hat\beta'_n} (eq. 17),
#'   averaged over the cutoff grid.
#' @param se Standard error from the standardized CLT (eq. 21 or 22).
#' @param ci Confidence interval `estimate +/- z se`.
#' @param estimate_bc,ci_bc Bias-corrected estimate (Section 5) and the
#'   interval around it with the same `se`; `NA` without correction.
#' @param bias_correction The correction applied.
#' @param level One minus the confidence level.
#' @param n Number of increments at \eqn{\Delta_n}.
#' @param details A data frame with one row per cutoff pair: `alpha`,
#'   `alpha_prime` (truncation estimator), the cutoffs `cutoff` and `cutoff2`
#'   (\eqn{\alpha'}, or the cutoff at \eqn{k\Delta_n}), the counts `count`
#'   \eqn{U(\varpi, \alpha)^n} and `count2`, `estimate`, `se` and the bias
#'   `correction` subtracted.
#' @param sigma_hat Pilot volatility estimate.
#' @param options The [AJActivityIndexOptions] used.
#' @inheritParams HFResult
#' @export
AJActivityIndex <- new_class(
  "AJActivityIndex",
  parent = HFResult,
  properties = list(
    estimator = class_character,
    estimate = class_double,
    se = class_double,
    ci = class_double,
    estimate_bc = class_double,
    ci_bc = class_double,
    bias_correction = class_character,
    level = class_double,
    n = class_integer,
    details = new_property(class_data.frame, default = quote(data.frame())),
    sigma_hat = class_double,
    options = AJActivityIndexOptions
  ),
  validator = function(self) {
    collect_msgs(
      if (length(self@estimate) != 1L || length(self@se) != 1L) {
        "@estimate and @se must be single numbers."
      },
      if (length(self@ci) != 2L || length(self@ci_bc) != 2L) "@ci and @ci_bc must have length 2.",
      if (length(self@estimate_bc) != 1L) "@estimate_bc must be a single number."
    )
  }
)

#' Ait-Sahalia and Jacod (2009b) estimator of the jump activity index
#'
#' Estimates the Blumenthal-Getoor index \eqn{\beta} of the jumps from the
#' number of large increments,
#' \deqn{U(\varpi, \alpha)^n_t = \sum_{i=1}^{[t/\Delta_n]}
#'       1\{|\Delta^n_i X| > \alpha\Delta_n^{\varpi}\}, \qquad
#'       \hat\beta_n(t, \varpi, \alpha, \alpha') =
#'       \frac{\log(U(\varpi, \alpha)^n_t / U(\varpi, \alpha')^n_t)}
#'            {\log(\alpha'/\alpha)}}
#' (eqs. 14, 16), with the standardized CLT of Theorem 3,
#' \deqn{\frac{\log(\alpha'/\alpha)}{\sqrt{1/U(\varpi,\alpha')^n_t -
#'       1/U(\varpi,\alpha)^n_t}} (\hat\beta_n - \beta) \to N(0, 1)}
#' (eq. 21). `estimator = "frequency"` gives \eqn{\hat\beta'_n} of eq. (17),
#' which compares the counts at \eqn{\Delta_n} and \eqn{k\Delta_n}, with the
#' standardization of eq. (22).
#'
#' @param x Log prices observed every \eqn{\Delta_n} over one window: a
#'   numeric vector or an [HFSeries].
#' @param options An [AJActivityIndexOptions] object.
#' @return An [AJActivityIndex] object.
#' @section Paper mapping:
#' Equation numbers refer to Ait-Sahalia and Jacod (2009b).
#' * eq. (14): `count` = \eqn{U(\varpi, \alpha)^n_t}, the number of
#'   increments above the cutoff.
#' * eq. (16) and the convention below it (\eqn{\hat\beta_n = 0} when a count
#'   is 0): `estimate` with `estimator = "truncation"`.
#' * eq. (17): `estimate` with `estimator = "frequency"`; the cutoff at
#'   \eqn{k\Delta_n} is \eqn{\alpha (k\Delta_n)^{\varpi}}, i.e. `k^varpi`
#'   times the cutoff at \eqn{\Delta_n} (\eqn{k = 2} in the paper).
#' * eqs. (21) and (22): `se` and `ci`.
#' * Remark 7: \eqn{\varpi = 1/5}; \eqn{\alpha} set in estimated standard
#'   deviations of the continuous part with eq. (18).
#' * eq. (42): `bias_correction = "plugin"`,
#'   \eqn{\hat\beta_n - \frac{1}{\log(\alpha'/\alpha)}\{\frac{\beta(\beta+1)
#'   \sigma^2}{2}(\alpha^{-2} - \alpha'^{-2})\Delta_n^{1-2\varpi} +
#'   \frac{d_\beta\theta^\beta}{2c_\beta}(\alpha^{-\beta} -
#'   \alpha'^{-\beta})\Delta_n^{1-\varpi\beta}\}} with \eqn{c_\beta},
#'   \eqn{d_\beta} of eq. (38).
#' * eqs. (44)-(46): `bias_correction = "regression"`: regress
#'   \eqn{U(\varpi, \alpha_j)^n} on \eqn{\alpha_j^{-\beta}},
#'   \eqn{\alpha_j^{-2-\beta}} and \eqn{\alpha_j^{-2\beta}} over the cutoffs
#'   `bias_grid` with the first-stage \eqn{\hat\beta_n}, then subtract
#'   \eqn{\frac{1}{\log(\alpha'/\alpha)}\{\frac{a_1}{a_0}(\alpha^{-2} -
#'   \alpha'^{-2}) + \frac{a_2}{a_0}(\alpha^{-\beta} - \alpha'^{-\beta})\}}.
#' @section Implementation choices:
#' * **Cutoffs.** Following Remark 7 and the empirical application
#'   (Section 7), the default cutoffs are \eqn{\alpha} = 7 and
#'   \eqn{\alpha'} = 14 estimated standard deviations (`truncation = "sd"`,
#'   see [aj_truncation()]). Only the cutoff values matter for eq. (16); the
#'   simulations of Section 6 use 5 and 10 standard deviations.
#' * **AVG.** Vectors `alpha` and `alpha_ratio` average the estimates over
#'   all pairs (Section 7: \eqn{\alpha} from 7 to 9, \eqn{\alpha'/\alpha}
#'   from 1.5 to 4). The paper does not give the standard error of the
#'   average; `se` is then the mean of the pairwise standard errors, an
#'   upper bound for the standard deviation of the average.
#' * **Bias correction.** The default is the regression of Section 5 used
#'   for Table 4, run over unit steps of the cutoff from `min(alpha)` to
#'   `max(alpha * alpha_ratio)` (the paper uses "the same range of
#'   truncation levels as for the average"). The regression and the
#'   correction use the first-stage (averaged) \eqn{\hat\beta_n}. The plug-in
#'   correction of eq. (42) assumes \eqn{X = \sigma W + \theta Y} with a
#'   symmetric \eqn{\beta}-stable \eqn{Y}; it uses the pilot
#'   \eqn{\hat\sigma^2\Delta_n} for \eqn{\sigma^2\Delta_n} and
#'   \eqn{U(\varpi,\alpha)^n/n} for the leading tail probability
#'   \eqn{A\Delta_n^{1-\varpi\beta}\alpha^{-\beta}}. Neither correction is
#'   computed when \eqn{\hat\beta_n \notin (0, 2)}.
#' * **Behaviour of the corrections.** In simulations of Brownian motion plus
#'   a symmetric stable process (`tests/simulations/aj-size-power.R`), the
#'   regression moves \eqn{\hat\beta_n} by less than 0.02: its three
#'   regressors are nearly collinear over the cutoff range, and with the
#'   first-stage \eqn{\hat\beta_n} the power law already fits both end
#'   points. The plug-in removes the bias for \eqn{\beta \le 1} but
#'   over-corrects near \eqn{\beta = 2}, where \eqn{c_\beta \to 0} and small
#'   jumps inflate the pilot volatility.
#' * **Not implemented.** AJ (2012, eq. 30) summarise the correction as a
#'   nonlinear regression of \eqn{U} on \eqn{\alpha} with the terms
#'   \eqn{\alpha^{-\beta}} and \eqn{\alpha^{-2\beta}}, estimating \eqn{\beta}
#'   jointly. The first-hand procedure of AJ (2009b, Section 5) is implemented
#'   instead; in our simulations the joint fit had a standard deviation of
#'   0.2-0.35 and was biased downwards.
#' * **Bias-corrected interval.** The corrections are of smaller order than
#'   the CLT rate (Section 5), so `ci_bc` re-centres `ci` at `estimate_bc`.
#' @references
#' Ait-Sahalia, Y. and Jacod, J. (2009b). Estimating the degree of activity
#' of jumps in high frequency data. *Annals of Statistics* 37(5A),
#' 2202-2244. \doi{10.1214/08-AOS640}. (Cited as 2009a in AJ 2011 and
#' AJ 2012.)
#'
#' Ait-Sahalia, Y. and Jacod, J. (2012). Analyzing the spectrum of asset
#' returns: jump and volatility components in high frequency data.
#' *Journal of Economic Literature* 50(4), 1007-1050.
#' @examples
#' set.seed(1)
#' n <- 23400
#' s <- 0.25 * sqrt(1 / (252 * n)) # sd of a 1-second Brownian increment
#' # Brownian motion plus Cauchy jumps (beta = 1), 1% of them above 7 sd
#' dx <- rnorm(n, 0, s) + rcauchy(n, scale = 0.01 * pi * 7 * s / 2)
#' aj_activity_index(cumsum(c(0, dx)))
#' @export
aj_activity_index <- function(x, options = aj_activity_index_options()) {
  check_class(options, AJActivityIndexOptions, "options")
  y <- log_prices(x)
  adx <- abs(diff(y))
  n <- length(adx)
  if (n < 2L * options@k) hf_abort("`x` has %d increments; at least %d are needed.",
                                   n, 2L * options@k)
  cut <- aj_options_cutoff(adx, options)
  s <- cut$scale
  count <- function(a, v) vapply(v, function(vv) sum(a > vv), numeric(1))

  if (options@estimator == "truncation") {
    grid <- expand.grid(alpha = options@alpha, ratio = options@alpha_ratio)
    a1 <- grid$alpha
    a2 <- grid$alpha * grid$ratio
    u1 <- a1 * s
    u2 <- a2 * s
    U1 <- count(adx, u1)
    U2 <- count(adx, u2)
    denom <- log(a2 / a1)
  } else {
    kk <- options@k
    a1 <- options@alpha
    a2 <- rep(NA_real_, length(a1))
    u1 <- a1 * s
    u2 <- u1 * kk^options@varpi
    U1 <- count(adx, u1)
    U2 <- count(abs(aj_increments(y, kk)), u2)
    denom <- rep(options@varpi * log(kk), length(a1))
  }
  # eqs. (16)-(17) with the convention "0 if either count is 0"; eqs. (21)-(22)
  est <- ifelse(U1 == 0 | U2 == 0, 0, log(U1 / U2) / denom)
  se <- ifelse(U1 == 0 | U2 == 0, NA_real_, suppressWarnings(sqrt(1 / U2 - 1 / U1)) / denom)
  if (any(U1 == 0 | U2 == 0)) {
    hf_warn("A cutoff leaves no increment above it; the estimate is set to 0 there (AJ 2009b, eq. 16).")
  }
  estimate <- mean(est)
  se_bar <- mean(se)
  zq <- stats::qnorm(options@level / 2, lower.tail = FALSE)

  correction <- rep(NA_real_, length(est))
  method_bc <- options@bias_correction
  if (method_bc != "none") {
    correction <- aj_beta_bias(method_bc, estimate, adx, s, a1, a2, U1, U2,
                               cut$sigma_hat^2 * cut$delta, options@bias_grid)
  }
  estimate_bc <- if (method_bc == "none") NA_real_ else mean(est - correction)

  details <- data.frame(
    alpha = a1, alpha_prime = a2, cutoff = u1, cutoff2 = u2, count = U1,
    count2 = U2, estimate = est, se = se, correction = correction
  )
  AJActivityIndex(
    method = "aj_activity_index",
    reference = "Ait-Sahalia and Jacod (2009b), Ann. Statist. 37(5A), 2202-2244",
    estimator = options@estimator, estimate = estimate, se = se_bar,
    ci = estimate + c(-1, 1) * zq * se_bar, estimate_bc = estimate_bc,
    ci_bc = estimate_bc + c(-1, 1) * zq * se_bar, bias_correction = method_bc,
    level = options@level, n = as.integer(n), details = details,
    sigma_hat = cut$sigma_hat, options = options
  )
}

# Small-sample bias of beta-hat (AJ 2009b, Section 5) for each cutoff pair
# (a1, a2), evaluated at the first-stage estimate b.
aj_beta_bias <- function(method, b, adx, s, a1, a2, U1, U2, s2c, bias_grid) {
  na <- rep(NA_real_, length(a1))
  if (!is.finite(b) || b <= 0 || b >= 2) {
    hf_warn("The bias correction needs a first-stage estimate in (0, 2), not %s.", format(b))
    return(na)
  }
  denom <- log(a2 / a1)
  if (method == "plugin") {
    # eq. (42) with sigma^2 Delta_n^(1 - 2 varpi) / alpha^2 = s2c / u^2 and
    # (d theta^b / 2c) Delta_n^(1 - varpi b) / alpha^b = (b d / 4c^2) U / n.
    cb <- gamma(b + 1) * sin(pi * b / 2) / (2 * pi)
    db <- -gamma(2 * b + 1) * sin(pi * b) / (8 * pi)
    n <- length(adx)
    return((b * (b + 1) / 2 * s2c * ((a1 * s)^(-2) - (a2 * s)^(-2)) +
              b * db / (4 * cb^2) * (U1 - U2) / n) / denom)
  }
  # eqs. (44)-(46): U(alpha_j) ~ a0 alpha^-b + a1 alpha^-(2+b) + a2 alpha^-2b
  g <- if (length(bias_grid)) sort(unique(bias_grid)) else {
    lo <- min(a1)
    hi <- max(a2)
    seq(lo, hi, length.out = max(4L, floor(hi - lo) + 1L))
  }
  Ug <- vapply(g * s, function(v) sum(adx > v), numeric(1))
  X <- cbind(g^(-b), g^(-2 - b), g^(-2 * b))
  fit <- stats::lm.fit(X, Ug)
  co <- unname(fit$coefficients)
  if (fit$rank < 3L || anyNA(co) || co[1] <= 0) {
    hf_warn("The bias-correction regression (AJ 2009b, eq. 45) is degenerate; no correction.")
    return(na)
  }
  (co[2] / co[1] * (a1^(-2) - a2^(-2)) + co[3] / co[1] * (a1^(-b) - a2^(-b))) / denom
}

method(print, AJActivityIndex) <- function(x, ...) {
  o <- x@options
  cat(sprintf("<AJActivityIndex> %s (%s)\n", x@method, x@reference))
  lab <- if (x@estimator == "truncation") "beta-hat (two cutoffs, eq. 16)" else
    sprintf("beta-hat' (two frequencies, k = %d, eq. 17)", o@k)
  cat(sprintf("  %s = %s   se = %s   %g%% CI [%s, %s]\n", lab, aj_fmt(x@estimate),
              aj_fmt(x@se), 100 * (1 - x@level), aj_fmt(x@ci[1]), aj_fmt(x@ci[2])))
  if (x@bias_correction != "none") {
    cat(sprintf("  bias-corrected (%s) = %s   %g%% CI [%s, %s]\n", x@bias_correction,
                aj_fmt(x@estimate_bc), 100 * (1 - x@level), aj_fmt(x@ci_bc[1]),
                aj_fmt(x@ci_bc[2])))
  }
  d <- x@details
  cat(sprintf("  %d cutoff pair(s) | counts above cutoffs: %s | n = %d | %s truncation\n",
              nrow(d), paste(range(c(d$count, d$count2)), collapse = "-"), x@n, o@truncation))
  invisible(x)
}

method(as.data.frame, AJActivityIndex) <- function(x, ...) {
  data.frame(
    method = x@method, estimator = x@estimator, estimate = x@estimate, se = x@se,
    ci_lower = x@ci[1], ci_upper = x@ci[2], estimate_bc = x@estimate_bc,
    ci_bc_lower = x@ci_bc[1], ci_bc_upper = x@ci_bc[2],
    bias_correction = x@bias_correction, level = x@level, n = x@n,
    pairs = nrow(x@details), sigma_hat = x@sigma_hat, stringsAsFactors = FALSE
  )
}
