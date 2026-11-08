#' @include class-result.R
#' @include weights.R
#' @include ajl2012-constants.R
NULL

# Options ---------------------------------------------------------------------

#' Options of the Ait-Sahalia, Jacod and Li (2012) jump test
#'
#' Typed options of [ajl_test()]. Create them with [ajl_options()], whose
#' defaults reproduce the simulation and empirical settings of Ait-Sahalia,
#' Jacod and Li (2012, Sections 4-5).
#'
#' @param p Even integer power \eqn{p \ge 4}.
#' @param g Weight function \eqn{g}, a [WeightFunction].
#' @param gamma Compression factor \eqn{\gamma > 1} of the second weight
#'   \eqn{h(x) = g(\gamma x)}.
#' @param phi Auxiliary weight \eqn{\phi} of the variance estimators.
#' @param kn,kn_prime Pre-averaging window \eqn{k_n} and the window
#'   \eqn{k'_n} of the local estimators in eq. 3.22.
#' @param trunc_C,trunc_exponent Constants \eqn{C} and \eqn{\varpi'} of the
#'   truncation level \eqn{u_n = C (\bar V(Z, g, 2)^n_T / T)^{1/2}
#'   \Delta_n^{\varpi'}}.
#' @param null Null hypothesis: `"no_jump"`, `"jump"` or `"both"`.
#' @param alpha Asymptotic level of the tests.
#' @param horizon Window length \eqn{T} in the time unit of the truncation
#'   rule; \eqn{\Delta_n = T / n}.
#' @param aprime Source of the \eqn{A'} constants: `"formula"` or `"authors"`.
#' @param eta_truncation,lower_truncation Truncation variants of eqs. 3.22 and
#'   3.23 (Remark 2).
#' @seealso [ajl_options()], [ajl_test()]
#' @export
AJLOptions <- new_class(
  "AJLOptions",
  properties = list(
    p = class_integer,
    g = WeightFunction,
    gamma = class_double,
    phi = WeightFunction,
    kn = class_integer,
    kn_prime = class_integer,
    trunc_C = class_double,
    trunc_exponent = class_double,
    null = class_character,
    alpha = class_double,
    horizon = class_double,
    aprime = class_character,
    eta_truncation = class_logical,
    lower_truncation = class_logical
  ),
  validator = function(self) {
    msgs <- ajl_option_msgs(
      p = self@p, gamma = self@gamma, kn = self@kn, kn_prime = self@kn_prime,
      trunc_C = self@trunc_C, trunc_exponent = self@trunc_exponent,
      null = self@null, alpha = self@alpha, horizon = self@horizon,
      aprime = self@aprime, eta_truncation = self@eta_truncation,
      lower_truncation = self@lower_truncation, abort = FALSE
    )
    if (length(msgs)) return(msgs)
    ajl_option_cross_msgs(self@p, self@g, self@gamma, self@kn, self@kn_prime,
                          self@trunc_C, self@aprime, self@lower_truncation)
  }
)

# Field-wise checks, shared by the validator (abort = FALSE: returns the
# messages) and the constructor (abort = TRUE: raises `hf_input_error`).
ajl_option_msgs <- function(p, gamma, kn, kn_prime, trunc_C, trunc_exponent,
                            null, alpha, horizon, aprime, eta_truncation,
                            lower_truncation, abort) {
  even_msg <- NULL
  if (is.numeric(p) && length(p) == 1L && !is.na(p) && p %% 2 != 0) {
    even_msg <- sprintf("`p` must be an even integer, not %s.", format(p))
  }
  collect_msgs(
    check_count(p, "p", min = 4L, abort = abort),
    finish_check(even_msg, p, abort),
    check_number(gamma, "gamma", lower = 1, open = c(TRUE, TRUE), abort = abort),
    check_count(kn, "kn", min = 2L, abort = abort),
    check_count(kn_prime, "kn_prime", min = 1L, abort = abort),
    check_number(trunc_C, "trunc_C", lower = 0, open = c(TRUE, FALSE), abort = abort),
    check_number(trunc_exponent, "trunc_exponent", lower = 1 / 3, upper = 1 / 2,
                 open = c(TRUE, TRUE), abort = abort),
    check_choice(null, c("no_jump", "jump", "both"), "null", abort = abort),
    check_probability(alpha, "alpha", abort = abort),
    check_number(horizon, "horizon", lower = 0, open = c(TRUE, TRUE), abort = abort),
    check_choice(aprime, c("formula", "authors"), "aprime", abort = abort),
    check_flag(eta_truncation, "eta_truncation", abort = abort),
    check_flag(lower_truncation, "lower_truncation", abort = abort)
  )
}

# Checks across fields.
ajl_option_cross_msgs <- function(p, g, gamma, kn, kn_prime, trunc_C, aprime,
                                  lower_truncation) {
  collect_msgs(
    if (kn_prime < kn) {
      sprintf("`kn_prime` (%d) must be at least `kn` (%d): eq. 3.21 requires kn_prime / kn -> Inf.",
              kn_prime, kn)
    },
    if (lower_truncation && !is.finite(trunc_C)) {
      "`lower_truncation = TRUE` needs a finite truncation constant `trunc_C`."
    },
    if (aprime == "authors" &&
        !(p == 4L && gamma == 2 && ajl_same_weight(g, weight_tent()))) {
      paste0("`aprime = \"authors\"` is only defined for p = 4, g = weight_tent() and ",
             "gamma = 2, the setting of the authors' code.")
    }
  )
}

#' Options for the Ait-Sahalia, Jacod and Li (2012) jump test
#'
#' Builds the [AJLOptions] of [ajl_test()]. The defaults are those of
#' Ait-Sahalia, Jacod and Li (2012, Section 4): \eqn{p = 4},
#' \eqn{g(x) = (0.5 - |x - 0.5|)^+}, \eqn{h(x) = g(2x)}, \eqn{\phi = g},
#' \eqn{k_n = 100}, \eqn{k'_n = 3 k_n} and
#' \eqn{u_n = 5 (\bar V(Z, g, 2)^n_T / T)^{1/2} \Delta_n^{0.49}}.
#'
#' @param p Even integer power \eqn{p \ge 4} of the power variations
#'   (eq. 3.11). Default 4.
#' @param g The weight function \eqn{g} (eq. 3.5), a [WeightFunction].
#'   Default [weight_tent()].
#' @param gamma Compression factor \eqn{\gamma > 1}: the second weight is
#'   \eqn{h(x) = g(\gamma x)} (Section 3.3), so that
#'   \eqn{\gamma'' = \gamma^{p/2 - 1} > 1}. Default 2.
#' @param phi The auxiliary weight \eqn{\phi} of the variance estimators
#'   (eqs. 3.19 and 3.22-3.23). Default `g`; Remark 1 also suggests
#'   `weight_compress(g, gamma)`, that is \eqn{\phi = h}.
#' @param kn Pre-averaging window \eqn{k_n} (eq. 3.4). Default 100.
#' @param kn_prime Window \eqn{k'_n} of eq. 3.21, at least `kn`. Default
#'   `3 * kn`.
#' @param trunc_C Constant \eqn{C > 0} of the truncation level; `Inf`
#'   disables truncation. Default 5.
#' @param trunc_exponent Exponent \eqn{\varpi'} of \eqn{\Delta_n} in the
#'   truncation level, in \eqn{(1/3, 1/2)}. Default 0.49. Because
#'   \eqn{\bar V(Z, g, 2)^n_T} grows like \eqn{k_n \asymp \Delta_n^{-1/2}},
#'   \eqn{u_n \asymp \Delta_n^{\varpi' - 1/4}}, and the range makes
#'   \eqn{\varpi = \varpi' - 1/4} satisfy \eqn{1/12 < \varpi < 1/4} as eq. 3.15
#'   requires.
#' @param null Which null hypothesis to test: `"no_jump"` (eq. 3.26, the
#'   default, which the paper recommends in practice), `"jump"` (eq. 3.27) or
#'   `"both"`.
#' @param alpha Asymptotic level \eqn{\alpha} of the tests. Default 0.05.
#' @param horizon Window length \eqn{T} in the time unit of the truncation
#'   rule; the sampling interval is \eqn{\Delta_n = T / n}. Default 1 (the
#'   window is the time unit). See *Implementation choices* in [ajl_test()].
#' @param aprime `"formula"` (default) computes the constants \eqn{A'} of
#'   eq. 3.17; `"authors"` uses the values hard-coded in the authors' Matlab
#'   code, which disagree with eq. 3.17 (see [ajl_constants()]). Only for
#'   reproducing results computed with that code.
#' @param eta_truncation Truncate the local estimators \eqn{\eta(\phi, m)} of
#'   eq. 3.22 at \eqn{u_n}, as printed? `FALSE` leaves out the truncation
#'   (Remark 2). Default `TRUE`.
#' @param lower_truncation Truncate \eqn{|\bar Z(\phi)^n_i|^{2p-2}} in
#'   eq. 3.23 from below, that is multiply it by
#'   \eqn{1\{|\bar Z(\phi)^n_i| > u_n\}} (Remark 2)? Default `FALSE`.
#' @return An [AJLOptions] object.
#' @references
#' Ait-Sahalia, Y., Jacod, J. and Li, J. (2012). Testing for jumps in noisy
#' high frequency data. *Journal of Econometrics* 168(2), 207-222.
#' @examples
#' ajl_options()
#' ajl_options(kn = 80, null = "both")
#' @export
ajl_options <- function(p = 4L, g = weight_tent(), gamma = 2, phi = g,
                        kn = 100L, kn_prime = 3L * kn, trunc_C = 5,
                        trunc_exponent = 0.49, null = "no_jump", alpha = 0.05,
                        horizon = 1, aprime = "formula", eta_truncation = TRUE,
                        lower_truncation = FALSE) {
  check_class(g, WeightFunction, "g")
  check_class(phi, WeightFunction, "phi")
  ajl_option_msgs(p, gamma, kn, kn_prime, trunc_C, trunc_exponent, null, alpha,
                  horizon, aprime, eta_truncation, lower_truncation, abort = TRUE)
  p <- as.integer(round(p))
  kn <- as.integer(round(kn))
  kn_prime <- as.integer(round(kn_prime))
  msgs <- ajl_option_cross_msgs(p, g, gamma, kn, kn_prime, trunc_C, aprime,
                                lower_truncation)
  if (length(msgs)) hf_abort(paste(msgs, collapse = " "))
  AJLOptions(
    p = p, g = g, gamma = as.double(gamma), phi = phi, kn = kn,
    kn_prime = kn_prime, trunc_C = as.double(trunc_C),
    trunc_exponent = as.double(trunc_exponent), null = null,
    alpha = as.double(alpha), horizon = as.double(horizon), aprime = aprime,
    eta_truncation = eta_truncation, lower_truncation = lower_truncation
  )
}

method(print, AJLOptions) <- function(x, ...) {
  cat("<AJLOptions> Ait-Sahalia, Jacod and Li (2012)\n")
  cat(sprintf("  p = %d, g = %s, h = g(%sx), phi = %s\n", x@p, x@g@name,
              format(x@gamma), x@phi@name))
  cat(sprintf("  kn = %d, kn' = %d, u_n = %s * (Vbar(Z, g, 2) / T)^(1/2) * delta_n^%s, T = %s\n",
              x@kn, x@kn_prime, format(x@trunc_C), format(x@trunc_exponent),
              format(x@horizon)))
  cat(sprintf("  null = %s, alpha = %s, A' = %s\n", x@null, format(x@alpha), x@aprime))
  if (!x@eta_truncation || x@lower_truncation) {
    cat(sprintf("  Remark 2 variants: eta_truncation = %s, lower_truncation = %s\n",
                x@eta_truncation, x@lower_truncation))
  }
  invisible(x)
}

# Result ----------------------------------------------------------------------

#' Result of the Ait-Sahalia, Jacod and Li (2012) jump test
#'
#' Returned by [ajl_test()]. Vectors indexed by null hypothesis are named
#' `no_jump` and/or `jump`.
#'
#' @param statistic \eqn{S_{RJ}(g, h; p)_n} (eq. 3.13).
#' @param statistic_uncorrected The same ratio built from the power variations
#'   \eqn{V(Z, \cdot, p, 0)^n_T} without noise correction.
#' @param vbar \eqn{\bar V(Z, g, p)^n_T} and \eqn{\bar V(Z, h, p)^n_T}
#'   (eq. 3.11), named `g`, `h`.
#' @param null The null hypotheses tested.
#' @param null_limit Limit of the statistic under each null: \eqn{\gamma''}
#'   without jumps, 1 with jumps (eq. 3.14).
#' @param variance \eqn{\Sigma^c_{RJ,n}} (eq. 3.20) and/or
#'   \eqn{\Sigma^j_{RJ,n}} (eq. 3.25).
#' @param std_error \eqn{\Delta_n^{1/4} \sqrt{\Sigma_{RJ,n}}}.
#' @param z Standardized statistics \eqn{(S_{RJ} - \gamma'') / (\Delta_n^{1/4}
#'   \sqrt{\Sigma^c_{RJ,n}})} and \eqn{(S_{RJ} - 1) / (\Delta_n^{1/4}
#'   \sqrt{\Sigma^j_{RJ,n}})}.
#' @param critical_value Boundaries of the critical regions: reject "no jump"
#'   if \eqn{S_{RJ}} is below \eqn{\gamma'' - z_\alpha \Delta_n^{1/4}
#'   \sqrt{\Sigma^c_{RJ,n}}} (eq. 3.26); reject "jumps" if it is above
#'   \eqn{1 + z_\alpha \Delta_n^{1/4} \sqrt{\Sigma^j_{RJ,n}}} (eq. 3.27).
#' @param p_value One-sided p-values, matching the critical regions.
#' @param reject Decisions at level `options@alpha`.
#' @param truncation Truncation level \eqn{u_n}.
#' @param n Number of increments.
#' @param delta_n,theta \eqn{\Delta_n = T / n} and \eqn{\theta = k_n
#'   \sqrt{\Delta_n}}.
#' @param mstar \eqn{M^*(g, g, \phi)}, \eqn{M^*(g, h, \phi)},
#'   \eqn{M^*(h, h, \phi)} of eq. 3.19 (empty if "no jump" is not tested).
#' @param n_jump \eqn{N(\phi, 0, -)}, \eqn{N(\phi, 0, +)}, \eqn{N(\phi, 1, -)},
#'   \eqn{N(\phi, 1, +)} of eq. 3.23 (empty if "jumps" is not tested).
#' @param options The [AJLOptions] used.
#' @param constants The [AJLConstants] used.
#' @inheritParams HFResult
#' @export
AJLTest <- new_class(
  "AJLTest",
  parent = HFResult,
  properties = list(
    statistic = class_double,
    statistic_uncorrected = class_double,
    vbar = class_double,
    null = class_character,
    null_limit = class_double,
    variance = class_double,
    std_error = class_double,
    z = class_double,
    critical_value = class_double,
    p_value = class_double,
    reject = class_logical,
    truncation = class_double,
    n = class_integer,
    delta_n = class_double,
    theta = class_double,
    mstar = class_double,
    n_jump = class_double,
    options = AJLOptions,
    constants = AJLConstants
  ),
  validator = function(self) {
    k <- length(self@null)
    collect_msgs(
      if (length(self@statistic) != 1L) "@statistic must be a single number.",
      if (k < 1L || !all(self@null %in% c("no_jump", "jump"))) "@null must hold \"no_jump\" and/or \"jump\".",
      if (any(lengths(list(self@null_limit, self@variance, self@std_error, self@z,
                           self@critical_value, self@p_value, self@reject)) != k)) {
        "Per-null properties must have one element per null hypothesis."
      }
    )
  }
)

method(print, AJLTest) <- function(x, ...) {
  o <- x@options
  cat(sprintf("<AJLTest> %s\n", x@reference))
  cat(sprintf("  S_RJ(g, h; %d) = %.4f (without noise correction %.4f); limit %.4g without jumps, 1 with jumps\n",
              o@p, x@statistic, x@statistic_uncorrected, x@constants@ratios[["gamma_dprime"]]))
  cat(sprintf("  n = %d, kn = %d, kn' = %d, u_n = %.4g, alpha = %s\n", x@n, o@kn,
              o@kn_prime, x@truncation, format(o@alpha)))
  for (k in seq_along(x@null)) {
    nm <- x@null[k]
    region <- if (nm == "no_jump") "S_RJ < %.4f (eq. 3.26)" else "S_RJ > %.4f (eq. 3.27)"
    decision <- if (is.na(x@reject[k])) "not available" else if (x@reject[k]) "rejected" else "not rejected"
    cat(sprintf(paste0("  H0: %-8s reject if ", region, ": z = %.3f, p-value = %.4g -> %s\n"),
                if (nm == "no_jump") "no jump" else "jumps", x@critical_value[k], x@z[k],
                x@p_value[k], decision))
  }
  invisible(x)
}

method(as.data.frame, AJLTest) <- function(x, ...) {
  o <- x@options
  data.frame(
    method = x@method, null = x@null, statistic = x@statistic,
    statistic_uncorrected = x@statistic_uncorrected, null_limit = x@null_limit,
    variance = x@variance, std_error = x@std_error, z = x@z,
    critical_value = x@critical_value, p_value = x@p_value, reject = x@reject,
    alpha = o@alpha, truncation = x@truncation, n = x@n, p = o@p, kn = o@kn,
    kn_prime = o@kn_prime, aprime = o@aprime,
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# Test --------------------------------------------------------------------------

#' Ait-Sahalia, Jacod and Li (2012) pre-averaged jump test
#'
#' Tests for jumps in a window of noisy high-frequency log prices with the
#' robustified statistic of Ait-Sahalia, Jacod and Li (2012),
#' \deqn{S_{RJ}(g, h; p)_n = \frac{\bar V(Z, g, p)^n_T}{\gamma' \, \bar V(Z, h,
#'       p)^n_T},}
#' the ratio of two noise-corrected pre-averaged power variations with
#' weights \eqn{g} and \eqn{h(x) = g(\gamma x)}. It converges to 1 on paths
#' with jumps and to \eqn{\gamma'' = \gamma^{p/2-1}} (2 with the defaults) on
#' continuous paths, under additive noise. The test of the null "no jump"
#' rejects in the lower tail (Theorem 4); the test of the null "jumps" rejects
#' in the upper tail (Theorem 5).
#'
#' @section Paper mapping:
#' * Eq. 3.7: pre-averaged returns \eqn{\bar Z(\cdot)^n_i} and noise measures
#'   \eqn{\hat Z(\cdot)^n_i} for \eqn{g}, \eqn{h} and \eqn{\phi}, via
#'   [preaverage()].
#' * Eqs. 3.9-3.11: \eqn{\bar V(Z, g, p)^n_T} and \eqn{\bar V(Z, h, p)^n_T}
#'   via [preavg_vbar()]; `@vbar`.
#' * Eq. 3.12: \eqn{\gamma, \gamma', \gamma''} from [ajl_constants()].
#' * Eq. 3.13: `@statistic`. `@statistic_uncorrected` replaces
#'   \eqn{\bar V} by \eqn{V(Z, \cdot, p, 0)^n_T} (the authors'
#'   `S_uncorrected`).
#' * Eq. 3.14: `@null_limit`.
#' * Eq. 3.15 and Section 4: `@truncation`
#'   \eqn{u_n = C (\bar V(Z, g, 2)^n_T / T)^{1/2} \Delta_n^{\varpi'}}
#'   (`trunc_C`, `trunc_exponent`, `horizon`).
#' * Eqs. 3.16-3.17: \eqn{A'(g, g; w)}, \eqn{A'(g, h; w)}, \eqn{A'(h, h; w)},
#'   `@constants@aprime`.
#' * Eq. 3.18: \eqn{V^*(Z, \phi, q, r)^n_T} via [preavg_power_variation()]
#'   with `truncation = u_n`.
#' * Eq. 3.19: \eqn{M^*(g, g, \phi; p)^n_T}, \eqn{M^*(g, h, \phi; p)^n_T},
#'   \eqn{M^*(h, h, \phi; p)^n_T}, `@mstar`.
#' * Eq. 3.20 (Theorem 2): `@variance["no_jump"]` \eqn{= \Sigma^c_{RJ,n}}.
#' * Eqs. 3.21-3.23: \eqn{\eta(\phi, m)^n_i} and \eqn{N(\phi, m, \pm)^n_T},
#'   `@n_jump`.
#' * Eq. 3.24: \eqn{\Psi_\pm}, \eqn{\Psi'_\pm}, `@constants@psi` and
#'   `@constants@psi_prime`.
#' * Eq. 3.25 (Theorem 3): `@variance["jump"]` \eqn{= \Sigma^j_{RJ,n}}.
#' * Eq. 3.26 (Theorem 4): null "no jump" rejected when
#'   \eqn{S_{RJ} < \gamma'' - z_\alpha \Delta_n^{1/4} \sqrt{\Sigma^c_{RJ,n}}};
#'   p-value \eqn{\Phi(z)}.
#' * Eq. 3.27 (Theorem 5): null "jumps" rejected when
#'   \eqn{S_{RJ} > 1 + z_\alpha \Delta_n^{1/4} \sqrt{\Sigma^j_{RJ,n}}};
#'   p-value \eqn{1 - \Phi(z)}.
#' * Section 4: the defaults of [ajl_options()].
#'
#' @section Implementation choices:
#' * **Time unit.** \eqn{\Delta_n = T / n} with \eqn{T} = `horizon`
#'   (default 1). The standardized statistics, critical values and p-values
#'   do not depend on the time unit, because \eqn{\Delta_n^{1/4}
#'   \Sigma_{RJ,n}^{1/2}} is free of \eqn{\Delta_n}. Only \eqn{u_n} does,
#'   through the factor \eqn{T^{-1/2} \Delta_n^{0.49} = T^{-0.01}
#'   n^{-0.49}}. Section 4 states "T = 5 days" without fixing the unit of
#'   the truncation rule; the authors' Matlab code measures time in years
#'   (\eqn{T = 5/250}, \eqn{\Delta_n = 1/(250 \cdot 4680)} for five days of
#'   5-second returns). `horizon = 5/250` reproduces it and raises \eqn{u_n}
#'   by \eqn{(5/250)^{-0.01} \approx 1.04} relative to the default, which is
#'   like using \eqn{C = 5.2} instead of 5.
#' * **Truncation weight.** \eqn{u_n} is computed from \eqn{\bar V(Z, g, 2)},
#'   as printed in Section 4, also when \eqn{\phi \ne g}. It truncates
#'   \eqn{|\bar Z(\phi)^n_i|} in eqs. 3.18 and 3.22. `trunc_C = Inf` gives no
#'   truncation. If \eqn{\bar V(Z, g, 2)^n_T \le 0} (noise-dominated data
#'   with a short window), a warning is given and no truncation is applied.
#' * **Window default.** \eqn{k_n = 100}, the paper's main choice for
#'   \eqn{n = 23400} (5-second returns over five 6.5-hour days); then
#'   \eqn{\theta = k_n \Delta_n^{1/2} \approx 0.65} for \eqn{T = 1}. For other
#'   sample sizes, choose \eqn{k_n \approx \theta \sqrt{n}}.
#' * **Index ranges.** Pre-averaged returns are indexed
#'   \eqn{i = 0, \dots, n - k_n}. The sums in eq. 3.23 run over exactly the
#'   printed ranges, so that \eqn{N(\phi, m, -)} pairs \eqn{\bar Z(\phi)^n_i}
#'   with the \eqn{k'_n} pre-averaged returns ending \eqn{k_n} steps before
#'   \eqn{i}, and \eqn{N(\phi, m, +)} with the \eqn{k'_n} starting \eqn{k_n}
#'   steps after it: the windows never overlap. The jump null therefore needs
#'   \eqn{n \ge 2 k_n + k'_n}.
#' * **Weight integrals.** \eqn{\bar g(2)}, \eqn{\bar g'(2)}, \eqn{\bar g(p)},
#'   \eqn{\bar{\phi}(2)}, \eqn{\bar{\phi}'(2)} and \eqn{\bar{\phi}(2p - 2)} are
#'   the integrals of the continuous weights (eq. 3.6), as in the authors'
#'   code, not Riemann sums over \eqn{j / k_n}.
#' * **A' constants.** Computed from eq. 3.17 (see [ajl_constants()]); the
#'   authors' Matlab values are available as `aprime = "authors"` for
#'   replication only. In the paper's design the \eqn{w = p} term dominates
#'   \eqn{M^*}, and the two sets of constants give variances that differ by
#'   well under 1%.
#' * **Remark 2 variants.** `eta_truncation = FALSE` removes the truncation
#'   from eq. 3.22 and `lower_truncation = TRUE` truncates
#'   \eqn{|\bar Z(\phi)^n_i|^{2p-2}} from below in eq. 3.23. Remark 2's
#'   suggestion to also truncate \eqn{\hat Z(\phi)} "at the level
#'   \eqn{u_n}" is not implemented, because the paper does not say how a
#'   squared quantity is compared with \eqn{u_n}. Remark 1's option of
#'   different windows \eqn{k_n} for the statistic and the variance
#'   estimators is not implemented either; one \eqn{k_n} is used throughout,
#'   as the paper recommends.
#' * **Degenerate estimates.** The noise corrections have coefficients of
#'   both signs, so with a short window and heavy noise \eqn{\bar V(Z, g, p)},
#'   \eqn{\bar V(Z, h, p)} or \eqn{\Sigma_{RJ,n}} can be non-positive. Then
#'   the standardized statistic, critical value, p-value and decision are
#'   `NA`, with a warning; the statistic and variances are still returned.
#' * **Sampling.** Observations are taken as equally spaced in the clock of
#'   the analysis (calendar time in the paper).
#'
#' @param x Log prices: a numeric vector or an [HFSeries], equally spaced.
#' @param options An [AJLOptions] object from [ajl_options()].
#' @return An [AJLTest] object; `as.data.frame()` gives one row per null
#'   hypothesis tested.
#' @references
#' Ait-Sahalia, Y., Jacod, J. and Li, J. (2012). Testing for jumps in noisy
#' high frequency data. *Journal of Econometrics* 168(2), 207-222.
#'
#' Ait-Sahalia, Y. and Jacod, J. (2009). Testing for jumps in a discretely
#' observed process. *Annals of Statistics* 37(1), 184-222.
#'
#' Jacod, J., Li, Y., Mykland, P. A., Podolskij, M. and Vetter, M. (2009).
#' Microstructure noise in the continuous case: the pre-averaging approach.
#' *Stochastic Processes and their Applications* 119(7), 2249-2276.
#'
#' Jacod, J., Podolskij, M. and Vetter, M. (2010). Limit theorems for moving
#' averages of discretized processes plus noise. *Annals of Statistics*
#' 38(3), 1478-1545.
#' @examples
#' set.seed(1)
#' n <- 23400
#' y <- log(25) + cumsum(rnorm(n + 1, 0, 0.4 * sqrt(1 / (250 * 4680)))) +
#'   rnorm(n + 1, 0, 1e-4)
#' res <- ajl_test(y, ajl_options(null = "both"))
#' res
#' as.data.frame(res)
#' @export
ajl_test <- function(x, options = ajl_options()) {
  check_class(options, AJLOptions, "options")
  y <- log_prices(x)
  o <- options
  n <- length(y) - 1L
  p <- o@p
  kn <- o@kn
  knp <- o@kn_prime
  nulls <- if (o@null == "both") c("no_jump", "jump") else o@null
  min_n <- if ("jump" %in% nulls) 2L * kn + knp else kn + 1L
  if (n < min_n) {
    hf_abort("`x` has %d increments, but the test needs at least %d (kn = %d, kn_prime = %d).",
             n, min_n, kn, knp)
  }
  cst <- ajl_constants(o@g, weight_compress(o@g, o@gamma), p, aprime = o@aprime)
  g <- cst@g
  h <- cst@h
  ratios <- cst@ratios
  gam <- ratios[["gamma"]]
  gam_p <- ratios[["gamma_prime"]]
  gam_pp <- ratios[["gamma_dprime"]]
  delta_n <- o@horizon / n
  theta <- kn * sqrt(delta_n)

  # Eqs. 3.7, 3.11 and 3.13.
  pa_g <- preaverage(y, kn, g)
  pa_h <- preaverage(y, kn, h)
  pa_phi <- if (ajl_same_weight(o@phi, g)) {
    pa_g
  } else if (ajl_same_weight(o@phi, h)) {
    pa_h
  } else {
    preaverage(y, kn, o@phi)
  }
  vbar_g <- preavg_vbar(pa_g, p)
  vbar_h <- preavg_vbar(pa_h, p)
  vbar_ok <- vbar_g > 0 && vbar_h > 0
  if (!vbar_ok) {
    hf_warn(paste0("The noise-corrected power variations are not positive, so the test is not ",
                   "available; `kn` may be too small for the noise level."))
  }
  stat <- vbar_g / (gam_p * vbar_h)
  stat_unc <- preavg_power_variation(pa_g, p) / (gam_p * preavg_power_variation(pa_h, p))

  # Truncation level of Section 4 (eq. 3.15).
  vbar2 <- preavg_vbar(pa_g, 2)
  un <- Inf
  if (is.finite(o@trunc_C)) {
    if (vbar2 > 0) {
      un <- o@trunc_C * sqrt(vbar2 / o@horizon) * delta_n^o@trunc_exponent
    } else {
      hf_warn("Vbar(Z, g, 2) is not positive, so u_n is undefined; no truncation is applied.")
    }
  }

  phi_int <- c(
    phi2 = weight_integral(o@phi, 2),
    phi_prime2 = weight_integral(o@phi, 2, derivative = TRUE),
    phi_2p2 = weight_integral(o@phi, 2 * p - 2)
  )

  variance <- c()
  mstar <- numeric()
  n_jump <- numeric()
  if ("no_jump" %in% nulls) {
    # Eqs. 3.18-3.20. V*(Z, phi, 2w - 2l, p + l - w) depends on j = w - l only.
    vstar <- vapply(0:p, function(j) {
      preavg_power_variation(pa_phi, 2 * j, p - j, truncation = un)
    }, numeric(1))
    m <- abs_normal_moment(2 * (0:p))
    bw <- vapply(0:p, function(w) {
      rho <- if (w == 0L) 1 else preavg_rho(2L * w)
      sum(rho * vstar[w - (0:w) + 1L]) /
        (m[w + 1L] * 2^(p - w) * phi_int[["phi2"]]^w * phi_int[["phi_prime2"]]^(p - w))
    }, numeric(1))
    mstar <- delta_n^(1 - p / 2) * theta * colSums(cst@aprime * bw)
    names(mstar) <- c("gg", "gh", "hh")
    variance["no_jump"] <-
      (mstar[["gg"]] - 2 * gam^(p / 2) * mstar[["gh"]] + gam^p * mstar[["hh"]]) /
      (delta_n^(1 - p / 4) * vbar_g / gam_pp)^2
  }
  if ("jump" %in% nulls) {
    # Eqs. 3.22-3.23, with 1-based vectors: element k + 1 holds index k.
    zb <- pa_phi@ybar
    zh <- pa_phi@yhat
    keep <- if (o@eta_truncation) abs(zb) <= un else rep(TRUE, length(zb))
    eta <- function(e) {
      cs <- c(0, cumsum(e * keep))
      i <- 0:(n - kn - knp)
      (cs[i + knp + 2L] - cs[i + 2L]) / (kn * knp * delta_n)
    }
    eta0 <- eta(zb^2 - zh / 2)
    eta1 <- eta(zh)
    pw <- zb^(2 * p - 2)
    if (o@lower_truncation) pw <- pw * (abs(zb) > un)
    i_minus <- (kn + knp):(n - kn)
    i_plus <- 0:(n - 2L * kn - knp + 1L)
    n_jump <- c(
      eta0_minus = sum(pw[i_minus + 1L] * eta0[i_minus - kn - knp + 1L]),
      eta0_plus = sum(pw[i_plus + 1L] * eta0[i_plus + kn]),
      eta1_minus = sum(pw[i_minus + 1L] * eta1[i_minus - kn - knp + 1L]),
      eta1_plus = sum(pw[i_plus + 1L] * eta1[i_plus + kn])
    ) / kn
    psi <- cst@psi[, "combined"]
    psi_p <- cst@psi_prime[, "combined"]
    # Eq. 3.25.
    variance["jump"] <- theta * p^2 * kn^2 / vbar_g^2 * (
      (psi[["minus"]] * n_jump[["eta0_minus"]] + psi[["plus"]] * n_jump[["eta0_plus"]]) /
        (phi_int[["phi2"]] * phi_int[["phi_2p2"]]) +
        (psi_p[["minus"]] * n_jump[["eta1_minus"]] + psi_p[["plus"]] * n_jump[["eta1_plus"]]) /
        (2 * phi_int[["phi_prime2"]] * phi_int[["phi_2p2"]])
    )
  }
  variance <- variance[nulls]

  # Eqs. 3.26-3.27: one-sided critical regions.
  ok <- is.finite(variance) & variance > 0
  if (vbar_ok && !all(ok)) {
    hf_warn("The estimated asymptotic variance is not positive for null %s; the test is not available.",
            paste0("\"", nulls[!ok], "\"", collapse = ", "))
  }
  ok <- ok & vbar_ok
  se <- ifelse(ok, delta_n^(1 / 4) * sqrt(pmax(variance, 0)), NA_real_)
  limit <- c(no_jump = gam_pp, jump = 1)[nulls]
  z <- (stat - limit) / se
  z_alpha <- stats::qnorm(o@alpha, lower.tail = FALSE)
  lower <- nulls == "no_jump"
  crit <- ifelse(lower, limit - z_alpha * se, limit + z_alpha * se)
  p_value <- ifelse(lower, stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE))
  reject <- ifelse(lower, stat < crit, stat > crit)
  names(variance) <- names(limit) <- names(se) <- names(z) <- names(crit) <-
    names(p_value) <- names(reject) <- nulls

  AJLTest(
    method = "ajl2012",
    reference = "Ait-Sahalia, Jacod and Li (2012)",
    statistic = stat, statistic_uncorrected = stat_unc,
    vbar = c(g = vbar_g, h = vbar_h), null = nulls, null_limit = limit,
    variance = variance, std_error = se, z = z,
    critical_value = crit, p_value = p_value, reject = reject,
    truncation = un, n = as.integer(n), delta_n = delta_n, theta = theta,
    mstar = mstar, n_jump = n_jump, options = o, constants = cst
  )
}
