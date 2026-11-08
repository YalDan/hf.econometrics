#' @include class-result.R aj-power-variation.R
NULL

#' Options for the Ait-Sahalia and Jacod (2011) activity tests
#'
#' Tuning parameters of [aj_finite_activity_test()]. Create them with
#' [aj_finite_activity_options()].
#'
#' @param null `"finite"` (null of finite jump activity, statistic
#'   \eqn{S_n} of eq. 33) or `"infinite"` (null of infinite activity,
#'   \eqn{S'_n} of eq. 40).
#' @param p Power \eqn{p > 2}: the power of \eqn{S_n}, or the smaller power
#'   of \eqn{S'_n}.
#' @param k Integer sampling multiple \eqn{k \ge 2} of \eqn{S_n}.
#' @param p_prime Larger power \eqn{p' > p} of \eqn{S'_n}.
#' @param gamma Truncation ratio \eqn{\gamma > 1} of \eqn{S'_n}.
#' @param level Asymptotic level \eqn{a} of the test.
#' @param truncation,alpha,varpi,sigma,pilot,horizon Truncation level
#'   \eqn{u_n}, see [aj_truncation()].
#' @export
AJFiniteActivityOptions <- new_class(
  "AJFiniteActivityOptions",
  properties = c(list(
    null = class_character,
    p = class_double,
    k = class_integer,
    p_prime = class_double,
    gamma = class_double,
    level = class_double
  ), aj_truncation_properties),
  validator = function(self) {
    msgs <- collect_msgs(
      check_choice(self@null, c("finite", "infinite"), "@null", abort = FALSE),
      check_number(self@p, "@p", lower = 2, open = c(TRUE, FALSE), abort = FALSE),
      check_number(self@k, "@k", lower = 2, integerish = TRUE, abort = FALSE),
      check_number(self@p_prime, "@p_prime", lower = 2, open = c(TRUE, FALSE), abort = FALSE),
      check_number(self@gamma, "@gamma", lower = 1, open = c(TRUE, FALSE), abort = FALSE),
      check_probability(self@level, "@level", abort = FALSE),
      aj_truncation_msgs(self)
    )
    if (!is.null(msgs)) return(msgs)
    collect_msgs(
      if (self@null == "infinite" && self@p_prime <= self@p) {
        "@p_prime must exceed @p (AJ 2011, eq. 40)."
      },
      if (self@truncation == "rate" && self@varpi >= 0.5) {
        "@varpi must be below 1/2 with truncation = \"rate\" (AJ 2011, eq. 30 with rho < 1/2)."
      }
    )
  }
)

#' Create options for the Ait-Sahalia and Jacod (2011) activity tests
#'
#' The defaults follow the simulations and the empirical application of
#' Ait-Sahalia and Jacod (2011, Sections 4-5): \eqn{p = 4} and \eqn{k = 2}
#' for \eqn{S_n}; \eqn{p = 3}, \eqn{p' = 4} and \eqn{\gamma = 2} for
#' \eqn{S'_n}; the truncation is expressed in estimated standard deviations
#' of the continuous part after a pilot estimate from the increments below
#' \eqn{4 \times 0.25\, \Delta_n^{1/2}}.
#'
#' @inheritParams AJFiniteActivityOptions
#' @param p Power; `NULL` means 4 for `null = "finite"` and 3 for
#'   `null = "infinite"`.
#' @param truncation `"sd"` (default; AJ 2011 Section 5 and AJ 2012 practice)
#'   or `"rate"` (\eqn{u_n = \alpha\sigma\Delta_n^{\varpi}}, eq. 30).
#' @param alpha Number of standard deviations (sd) or multiple of `sigma`
#'   (rate); default 7.
#' @param varpi Truncation rate for `truncation = "rate"`; default 0.2, the
#'   value of the simulations (Section 4).
#' @param sigma Volatility guess in the time unit of `horizon` (default 0.25
#'   per year).
#' @param pilot Pilot cutoff in standard deviations of `sigma` (default 4).
#' @param horizon Window length in the time unit of `sigma` (default `1/252`,
#'   one trading day in years).
#' @return An [AJFiniteActivityOptions] object.
#' @examples
#' aj_finite_activity_options()
#' aj_finite_activity_options(null = "infinite", gamma = 2.5)
#' @export
aj_finite_activity_options <- function(null = "finite", p = NULL, k = 2L,
                                       p_prime = 4, gamma = 2, level = 0.05,
                                       truncation = "sd", alpha = 7, varpi = 0.2,
                                       sigma = 0.25, pilot = 4, horizon = 1 / 252) {
  check_choice(null, c("finite", "infinite"), "null")
  if (is.null(p)) p <- if (null == "finite") 4 else 3
  check_number(p, "p", lower = 2, open = c(TRUE, FALSE))
  k <- check_count(k, "k", min = 2L)
  check_number(p_prime, "p_prime", lower = 2, open = c(TRUE, FALSE))
  check_number(gamma, "gamma", lower = 1, open = c(TRUE, FALSE))
  check_probability(level, "level")
  check_number(alpha, "alpha", lower = 0, open = c(TRUE, FALSE))
  aj_check_truncation_args(truncation, varpi, sigma, pilot, horizon)
  aj_new(AJFiniteActivityOptions,
    null = null, p = as.double(p), k = k, p_prime = as.double(p_prime),
    gamma = as.double(gamma), level = as.double(level), truncation = truncation,
    alpha = as.double(alpha), varpi = as.double(varpi), sigma = as.double(sigma),
    pilot = as.double(pilot), horizon = as.double(horizon)
  )
}

#' Result of the Ait-Sahalia and Jacod (2011) activity tests
#'
#' Returned by [aj_finite_activity_test()].
#'
#' @param null `"finite"` or `"infinite"`: the null hypothesis tested.
#' @param statistic \eqn{S_n} (eq. 33) or \eqn{S'_n} (eq. 40).
#' @param p,k,p_prime,gamma Tuning values used (`k` is used by \eqn{S_n}
#'   only, `p_prime` and `gamma` by \eqn{S'_n} only).
#' @param n Number of increments at \eqn{\Delta_n}.
#' @param level Asymptotic level.
#' @param limits Named probability limits: `finite` (finite activity with a
#'   Brownian part), `infinite` (infinite activity) and `noise` (additive
#'   noise dominates, eq. 48).
#' @param variance \eqn{V_n} (eq. 37) or \eqn{V'_n} (eq. 44).
#' @param z Standardized statistic, centred at the limit under the null
#'   (eqs. 36 and 43).
#' @param p_value Lower-tail p-value \eqn{\Phi(z)}.
#' @param critical_value \eqn{c_n} of eq. (39) or (45).
#' @param reject Is the statistic below `critical_value`?
#' @param cutoff Truncation level \eqn{u_n}.
#' @param sigma_hat Pilot volatility estimate.
#' @param options The [AJFiniteActivityOptions] used.
#' @inheritParams HFResult
#' @export
AJActivityTest <- new_class(
  "AJActivityTest",
  parent = HFResult,
  properties = list(
    null = class_character,
    statistic = class_double,
    p = class_double,
    k = class_integer,
    p_prime = class_double,
    gamma = class_double,
    n = class_integer,
    level = class_double,
    limits = class_double,
    variance = class_double,
    z = class_double,
    p_value = class_double,
    critical_value = class_double,
    reject = class_logical,
    cutoff = class_double,
    sigma_hat = class_double,
    options = AJFiniteActivityOptions
  ),
  validator = function(self) {
    scalars <- c("null", "statistic", "variance", "z", "p_value", "critical_value",
                 "reject", "cutoff", "sigma_hat")
    bad <- scalars[vapply(scalars, function(s) length(S7::prop(self, s)) != 1L, logical(1))]
    collect_msgs(
      if (length(bad)) sprintf("@%s must have length 1.", paste(bad, collapse = ", @")),
      if (!identical(names(self@limits), c("finite", "infinite", "noise"))) {
        "@limits must be named finite, infinite, noise."
      }
    )
  }
)

#' Ait-Sahalia and Jacod (2011) tests of finite or infinite jump activity
#'
#' With `null = "finite"` (the default), computes the truncated statistic
#' \deqn{S_n = \frac{B(p, u_n, k\Delta_n)}{B(p, u_n, \Delta_n)}, \qquad
#'       B(p, u_n, \Delta_n) = \sum_{i=1}^{[T/\Delta_n]} |\Delta^n_i X|^p\,
#'       1\{|\Delta^n_i X| \le u_n\},}
#' which tends to \eqn{k^{p/2-1}} when the jumps have finite activity (the
#' truncation removes them) and to 1 when they have infinite activity
#' (eqs. 34-35). The null of finite activity is rejected in the lower tail,
#' \eqn{\{S_n < k^{p/2-1} - z_a V_n^{1/2}\}} with
#' \eqn{V_n = N(p,k) B(2p, u_n, \Delta_n)/B(p, u_n, \Delta_n)^2}
#' (eqs. 36-39). This is \eqn{S_{FA}} of Ait-Sahalia and Jacod (2012, eq. 16).
#'
#' With `null = "infinite"`, computes
#' \deqn{S'_n = \frac{B(p', \gamma u_n, \Delta_n) B(p, u_n, \Delta_n)}
#'       {B(p', u_n, \Delta_n) B(p, \gamma u_n, \Delta_n)},}
#' which tends to \eqn{\gamma^{p'-p}} under infinite activity and to 1 under
#' finite activity (eqs. 40-42), and rejects in the lower tail
#' \eqn{\{S'_n < \gamma^{p'-p} - z_a V_n'^{1/2}\}} (eqs. 43-45).
#'
#' @param x Log prices observed every \eqn{\Delta_n} over one window: a
#'   numeric vector or an [HFSeries].
#' @param options An [AJFiniteActivityOptions] object.
#' @return An [AJActivityTest] object.
#' @section Paper mapping:
#' Equation numbers refer to Ait-Sahalia and Jacod (2011).
#' * eq. (32): \eqn{B(p, u_n, \Delta_n)}, [aj_power_variation()].
#' * eq. (33): \eqn{S_n} (`null = "finite"`), the same \eqn{u_n} at both
#'   scales; AJ (2012, eq. 16) calls it \eqn{S_{FA}}.
#' * eqs. (34), (35), (48): `@limits`.
#' * eqs. (36)-(38): \eqn{V_n} and \eqn{N(p, k)}, with \eqn{m_{k,p}} of
#'   eq. (38) in closed form (see [aj_jump_test()]).
#' * eq. (39): `@critical_value`, `@reject`.
#' * eqs. (40)-(45): \eqn{S'_n}, \eqn{V'_n} and its critical region
#'   (`null = "infinite"`).
#' @section Implementation choices:
#' * **N(p, k).** It is computed from eq. (38),
#'   \eqn{N(p,k) = M(p,k) m_p^2/m_{2p}} with \eqn{M} of AJ (2009a, eq. 20).
#'   The closed form printed in eq. (49), \eqn{N(4,k) = 16k(2k^2-k-1)/3},
#'   equals \eqn{M(4, k)} instead and is larger by \eqn{m_8/m_4^2 = 35/3};
#'   eq. (38) gives \eqn{N(4, k) = 16k(2k^2-k-1)/35}. Eq. (38) agrees with the
#'   CLT of AJ (2009a, Theorem 3b) and with the nominal sizes of Table 1.
#' * **Truncation.** The default follows the empirical practice of Section 5
#'   (and AJ 2012): a pilot volatility from the increments below
#'   \eqn{\Delta_n^{1/2}} (four standard deviations at 25% annual
#'   volatility), then \eqn{u_n} = `alpha` estimated standard deviations.
#'   `alpha = 7` lies in the ranges used in Table 1 (6-15), Figure 11 (6-12)
#'   and AJ (2012) (5-10). `truncation = "rate"` gives
#'   \eqn{u_n = \alpha\sigma\Delta_n^{\varpi}} with the simulation rate
#'   \eqn{\varpi = 0.2}.
#' * **Finite-sample bias.** \eqn{u_n} is \eqn{\alpha/\sqrt{k}} standard
#'   deviations of the \eqn{k\Delta_n} increments, so a small `alpha`
#'   truncates Brownian increments at the coarse scale and biases \eqn{S_n}
#'   below \eqn{k^{p/2-1}} (Figure 1).
#' * **One window.** The statistic uses one window. AJ (2011, Section 5) sum
#'   daily truncated variations over a year before taking ratios; that
#'   aggregation is left to the caller.
#' * **p-values.** Both tests are one-sided (lower tail): \eqn{\Phi(z)}.
#' @references
#' Ait-Sahalia, Y. and Jacod, J. (2011). Testing whether jumps have finite or
#' infinite activity. *Annals of Statistics* 39(3), 1689-1719.
#' \doi{10.1214/11-AOS873}.
#'
#' Ait-Sahalia, Y. and Jacod, J. (2009a). Testing for jumps in a discretely
#' observed process. *Annals of Statistics* 37(1), 184-222.
#'
#' Ait-Sahalia, Y. and Jacod, J. (2012). Analyzing the spectrum of asset
#' returns: jump and volatility components in high frequency data.
#' *Journal of Economic Literature* 50(4), 1007-1050.
#' @examples
#' n <- 23400
#' x <- cumsum(c(0, rnorm(n, 0, 0.25 * sqrt(1 / (252 * n)))))
#' x[10001:(n + 1)] <- x[10001:(n + 1)] + 0.01 # one large jump
#' aj_finite_activity_test(x)
#' @export
aj_finite_activity_test <- function(x, options = aj_finite_activity_options()) {
  check_class(options, AJFiniteActivityOptions, "options")
  y <- log_prices(x)
  adx <- abs(diff(y))
  n <- length(adx)
  k <- options@k
  if (n < 2L * k) hf_abort("`x` has %d increments; at least %d are needed.", n, 2L * k)
  if (all(adx == 0)) hf_abort("`x` has no non-zero increments.")
  cut <- aj_options_cutoff(adx, options)
  u <- options@alpha * cut$scale
  level <- options@level
  p <- options@p
  B <- function(q, uu) aj_power_sum(adx, q, uu)

  if (options@null == "finite") {
    b1 <- B(p, u)
    stat <- aj_power_sum(abs(aj_increments(y, k)), p, u) / b1 # eq. (33)
    V <- aj_N(p, k) * B(2 * p, u) / b1^2 # eqs. (37)-(38)
    limit0 <- k^(p / 2 - 1)
    limits <- c(finite = limit0, infinite = 1, noise = 1 / k)
  } else {
    g <- options@gamma
    pp <- options@p_prime
    gu <- g * u
    stat <- B(pp, gu) * B(p, u) / (B(pp, u) * B(p, gu)) # eq. (40)
    V <- g^(2 * pp - 2 * p) * ( # eq. (44)
      B(2 * p, u) / B(p, u)^2 + (1 - 2 * g^(-p)) * B(2 * p, gu) / B(p, gu)^2 +
        B(2 * pp, u) / B(pp, u)^2 + (1 - 2 * g^(-pp)) * B(2 * pp, gu) / B(pp, gu)^2 -
        2 * B(p + pp, u) / (B(p, u) * B(pp, u)) -
        2 * (1 - g^(-p) - g^(-pp)) * B(p + pp, gu) / (B(p, gu) * B(pp, gu))
    )
    limit0 <- g^(pp - p)
    limits <- c(finite = 1, infinite = limit0, noise = limit0)
  }
  if (!is.finite(stat)) {
    hf_warn("The truncated power variations vanish at u_n = %s; the statistic is not available.",
            format(u))
    stat <- NA_real_
  }
  res <- if (is.na(stat)) {
    list(variance = NA_real_, z = NA_real_, p_value = NA_real_, critical = NA_real_,
         reject = NA)
  } else {
    aj_one_sided(stat, limit0, V, stats::qnorm(level, lower.tail = FALSE),
                 lower = TRUE, paste(options@null, "activity"))
  }

  AJActivityTest(
    method = "aj_finite_activity_test",
    reference = "Ait-Sahalia and Jacod (2011), Ann. Statist. 39(3), 1689-1719",
    null = options@null, statistic = stat, p = p, k = k, p_prime = options@p_prime,
    gamma = options@gamma, n = as.integer(n), level = level, limits = limits,
    variance = res$variance, z = res$z, p_value = res$p_value,
    critical_value = res$critical, reject = res$reject, cutoff = u,
    sigma_hat = cut$sigma_hat, options = options
  )
}

method(print, AJActivityTest) <- function(x, ...) {
  o <- x@options
  cat(sprintf("<AJActivityTest> %s (%s)\n", x@method, x@reference))
  if (x@null == "finite") {
    cat(sprintf("  H0: finite jump activity | S(p = %s, k = %d, u_n) = %s   [n = %d]\n",
                format(x@p), x@k, aj_fmt(x@statistic, 5), x@n))
  } else {
    cat(sprintf("  H0: infinite jump activity | S'(p = %s, p' = %s, gamma = %s) = %s   [n = %d]\n",
                format(x@p), format(x@p_prime), format(x@gamma), aj_fmt(x@statistic, 5), x@n))
  }
  cat(sprintf("  limits: finite activity %s | infinite activity %s | additive noise %s\n",
              aj_fmt(x@limits[["finite"]]), aj_fmt(x@limits[["infinite"]]),
              aj_fmt(x@limits[["noise"]])))
  cat(sprintf("  z = %s, p-value = %s; reject if S < %s -> %s\n", aj_fmt(x@z),
              aj_fmt(x@p_value), aj_fmt(x@critical_value), aj_decision(x@reject)))
  cat(sprintf("  level %s | u_n = %s (%s, alpha = %s)\n", format(x@level),
              aj_fmt(x@cutoff), o@truncation, format(o@alpha)))
  invisible(x)
}

method(as.data.frame, AJActivityTest) <- function(x, ...) {
  data.frame(
    method = x@method, null = x@null, statistic = x@statistic, p = x@p,
    k = if (x@null == "finite") x@k else NA_integer_,
    p_prime = if (x@null == "infinite") x@p_prime else NA_real_,
    gamma = if (x@null == "infinite") x@gamma else NA_real_,
    n = x@n, level = x@level, limit_finite = x@limits[["finite"]],
    limit_infinite = x@limits[["infinite"]], limit_noise = x@limits[["noise"]],
    variance = x@variance, z = x@z, p_value = x@p_value,
    critical_value = x@critical_value, reject = x@reject, cutoff = x@cutoff,
    sigma_hat = x@sigma_hat, stringsAsFactors = FALSE
  )
}
