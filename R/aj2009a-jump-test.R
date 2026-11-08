#' @include class-result.R aj-power-variation.R
NULL

#' Options for the Ait-Sahalia and Jacod (2009a) jump test
#'
#' Tuning parameters of [aj_jump_test()]. Create them with
#' [aj_jump_options()], whose defaults reproduce the simulation setting of
#' Ait-Sahalia and Jacod (2009a, Section 6).
#'
#' @param p Power \eqn{p > 3}.
#' @param k Integer sampling multiple \eqn{k \ge 2}.
#' @param level Asymptotic level \eqn{\alpha} of both tests.
#' @param variance Variance estimator under the no-jump null: `"truncated"`
#'   (\eqn{\hat V^c}, eq. 30) or `"multipower"` (\eqn{\tilde V^c}, eq. 31).
#' @param jump_critical Critical value under the jump null: `"normal"`
#'   (eq. 40) or `"chebyshev"` (eq. 39).
#' @param kn Half-width \eqn{k_n} of the local windows of eq. (24); `NA`
#'   means \eqn{[50\Delta_n^{-1/4}]} with \eqn{\Delta_n = 1/n}.
#' @param truncation,alpha,varpi,sigma,pilot,horizon Truncation of eqs. (21)
#'   and (24), see [aj_truncation()].
#' @export
AJJumpOptions <- new_class(
  "AJJumpOptions",
  properties = c(list(
    p = class_double,
    k = class_integer,
    level = class_double,
    variance = class_character,
    jump_critical = class_character,
    kn = class_integer
  ), aj_truncation_properties),
  validator = function(self) {
    msgs <- collect_msgs(
      check_number(self@p, "@p", lower = 3, open = c(TRUE, FALSE), abort = FALSE),
      check_number(self@k, "@k", lower = 2, integerish = TRUE, abort = FALSE),
      check_probability(self@level, "@level", abort = FALSE),
      check_choice(self@variance, c("truncated", "multipower"), "@variance", abort = FALSE),
      check_choice(self@jump_critical, c("normal", "chebyshev"), "@jump_critical",
                   abort = FALSE),
      if (length(self@kn) != 1L || (!is.na(self@kn) && self@kn < 1L)) {
        "@kn must be NA (automatic) or a positive integer."
      },
      aj_truncation_msgs(self)
    )
    if (!is.null(msgs) || self@truncation != "rate") return(msgs)
    collect_msgs(
      if (self@varpi >= 0.5) {
        "@varpi must be below 1/2 with truncation = \"rate\" (AJ 2009a, Theorems 6-7)."
      },
      if (self@variance == "truncated" && self@varpi <= 0.5 - 1 / self@p) {
        sprintf(paste0("@varpi must exceed 1/2 - 1/p = %s for the truncated variance ",
                       "(AJ 2009a, Theorem 6)."), format(0.5 - 1 / self@p))
      }
    )
  }
)

#' Create options for the Ait-Sahalia and Jacod (2009a) jump test
#'
#' The defaults reproduce the simulation design of Ait-Sahalia and Jacod
#' (2009a, Section 6 and Tables 1-3): \eqn{p = 4}, \eqn{k = 2}, the truncated
#' variance (30) with \eqn{\alpha' = 5\sigma} and \eqn{\varpi = 0.47}, and
#' \eqn{k_n = [50\Delta_n^{-1/4}]} in eq. (24) with time in units of the
#' window.
#'
#' @inheritParams AJJumpOptions
#' @param kn Half-width of the local windows of eq. (24), or `NULL` for
#'   \eqn{[50\Delta_n^{-1/4}]} with \eqn{\Delta_n = 1/n}, i.e. time measured
#'   in units of the window (618 for one day of 1-second data).
#' @param truncation `"rate"` (default; \eqn{u_n = \alpha\sigma\Delta_n^{\varpi}},
#'   AJ 2009a eq. 21) or `"sd"` (`alpha` estimated standard deviations,
#'   AJ 2012). See [aj_truncation()].
#' @param alpha Multiple of `sigma` (rate) or number of standard deviations
#'   (sd); default 5.
#' @param varpi Truncation rate; default 0.47. With `truncation = "rate"` it
#'   must lie in \eqn{(1/2 - 1/p, 1/2)} for the truncated variance.
#' @param sigma Volatility guess in the time unit of `horizon` (default 0.25
#'   per year).
#' @param pilot Pilot cutoff for `truncation = "sd"`, in standard deviations
#'   of `sigma`.
#' @param horizon Window length in the time unit of `sigma` (default `1/252`,
#'   one trading day in years).
#' @return An [AJJumpOptions] object.
#' @examples
#' aj_jump_options()
#' aj_jump_options(k = 3, variance = "multipower", jump_critical = "chebyshev")
#' @export
aj_jump_options <- function(p = 4, k = 2L, level = 0.05, variance = "truncated",
                            jump_critical = "normal", truncation = "rate",
                            alpha = 5, varpi = 0.47, sigma = 0.25, pilot = 4,
                            horizon = 1 / 252, kn = NULL) {
  check_number(p, "p", lower = 3, open = c(TRUE, FALSE))
  k <- check_count(k, "k", min = 2L)
  check_probability(level, "level")
  check_choice(variance, c("truncated", "multipower"), "variance")
  check_choice(jump_critical, c("normal", "chebyshev"), "jump_critical")
  check_number(alpha, "alpha", lower = 0, open = c(TRUE, FALSE))
  aj_check_truncation_args(truncation, varpi, sigma, pilot, horizon)
  kn <- if (is.null(kn)) NA_integer_ else check_count(kn, "kn", min = 1L)
  aj_new(AJJumpOptions,
    p = as.double(p), k = k, level = as.double(level), variance = variance,
    jump_critical = jump_critical, kn = kn, truncation = truncation,
    alpha = as.double(alpha), varpi = as.double(varpi), sigma = as.double(sigma),
    pilot = as.double(pilot), horizon = as.double(horizon)
  )
}

#' Result of the Ait-Sahalia and Jacod (2009a) jump test
#'
#' Returned by [aj_jump_test()]. It reports the statistic
#' \eqn{\hat S(p, k, \Delta_n)}, its limits, and the two one-sided tests: the
#' null of no jumps (rejected for small values) and the null of jumps
#' (rejected for large values).
#'
#' @param statistic \eqn{\hat S(p, k, \Delta_n)} (eq. 12).
#' @param p,k Power and sampling multiple.
#' @param n Number of increments at \eqn{\Delta_n}.
#' @param level Asymptotic level of both tests.
#' @param limits Named probability limits: `jumps` (1, eq. 13),
#'   `continuous` (\eqn{k^{p/2-1}}, eq. 13), `noise` (\eqn{1/k}, additive
#'   noise, eq. 43; AJ 2012 eq. 40) and `rounding` (\eqn{k^{-1/2}}, rounding
#'   noise with finite-activity jumps, AJ 2012 eq. 40).
#' @param variance_no_jumps,z_no_jumps,p_value_no_jumps,critical_no_jumps,reject_no_jumps
#'   No-jump null: variance estimate \eqn{\hat V^c_n} (eq. 30) or
#'   \eqn{\tilde V^c_n} (eq. 31), standardized statistic
#'   \eqn{(\hat S - k^{p/2-1})/\sqrt{V}}, lower-tail p-value, critical value
#'   \eqn{c^c_n} (eq. 35) and decision \eqn{\hat S < c^c_n} (eq. 32).
#' @param variance_jumps,z_jumps,p_value_jumps,critical_jumps,reject_jumps
#'   Jump null: \eqn{\hat V^j_n} (eq. 29), \eqn{(\hat S - 1)/\sqrt{\hat V^j_n}},
#'   upper-tail p-value, critical value \eqn{c^j_n} (eq. 40 or 39) and
#'   decision \eqn{\hat S > c^j_n} (eq. 36).
#' @param cutoff Truncation level \eqn{u_n} used in eqs. (21) and (24).
#' @param sigma_hat Pilot volatility estimate (see [aj_truncation()]).
#' @param kn Window half-width used in eq. (24).
#' @param options The [AJJumpOptions] used.
#' @inheritParams HFResult
#' @export
AJJumpTest <- new_class(
  "AJJumpTest",
  parent = HFResult,
  properties = list(
    statistic = class_double,
    p = class_double,
    k = class_integer,
    n = class_integer,
    level = class_double,
    limits = class_double,
    variance_no_jumps = class_double,
    z_no_jumps = class_double,
    p_value_no_jumps = class_double,
    critical_no_jumps = class_double,
    reject_no_jumps = class_logical,
    variance_jumps = class_double,
    z_jumps = class_double,
    p_value_jumps = class_double,
    critical_jumps = class_double,
    reject_jumps = class_logical,
    cutoff = class_double,
    sigma_hat = class_double,
    kn = class_integer,
    options = AJJumpOptions
  ),
  validator = function(self) {
    scalars <- c("statistic", "variance_no_jumps", "z_no_jumps", "p_value_no_jumps",
                 "critical_no_jumps", "reject_no_jumps", "variance_jumps", "z_jumps",
                 "p_value_jumps", "critical_jumps", "reject_jumps", "cutoff", "sigma_hat")
    bad <- scalars[vapply(scalars, function(s) length(S7::prop(self, s)) != 1L, logical(1))]
    collect_msgs(
      if (length(bad)) sprintf("@%s must have length 1.", paste(bad, collapse = ", @")),
      if (!identical(names(self@limits), c("jumps", "continuous", "noise", "rounding"))) {
        "@limits must be named jumps, continuous, noise, rounding."
      }
    )
  }
)

#' Ait-Sahalia and Jacod (2009a) test for jumps
#'
#' Computes the ratio of power variations at two sampling frequencies,
#' \deqn{\hat S(p, k, \Delta_n) = \frac{\hat B(p, k\Delta_n)}{\hat B(p, \Delta_n)},
#'       \qquad \hat B(p, \Delta_n) = \sum_{i=1}^{[t/\Delta_n]} |\Delta^n_i X|^p,}
#' which converges to 1 if the path has jumps and to \eqn{k^{p/2-1}} if it is
#' continuous (Theorem 1). Both one-sided tests of Section 5 are reported.
#' With the null "no jumps", the critical region is
#' \eqn{\{\hat S < k^{p/2-1} - z_\alpha (\hat V^c_n)^{1/2}\}} (eqs. 32, 35).
#' With the null "jumps", it is \eqn{\{\hat S > 1 + z_\alpha (\hat
#' V^j_n)^{1/2}\}} (eqs. 36, 40), or \eqn{\{\hat S > 1 + (\hat V^j_n /
#' \alpha)^{1/2}\}} (eq. 39). Here \eqn{z_\alpha} is the upper
#' \eqn{\alpha}-quantile of \eqn{N(0, 1)}.
#'
#' @param x Log prices observed every \eqn{\Delta_n} over one window: a
#'   numeric vector or an [HFSeries]. To test at a coarser interval, subsample
#'   the prices first, e.g. `x[seq(1, length(x), by = 5)]`.
#' @param options An [AJJumpOptions] object from [aj_jump_options()].
#' @return An [AJJumpTest] object.
#' @section Paper mapping:
#' Equation numbers refer to Ait-Sahalia and Jacod (2009a).
#' * eq. (9): \eqn{\hat B(p, \Delta_n)}; \eqn{\hat B(p, k\Delta_n)} uses the
#'   increments between successive multiples of \eqn{k\Delta_n}
#'   ([aj_power_variation()]).
#' * eq. (12): `@statistic`. eq. (13) and AJ (2012, eqs. 15, 40): `@limits`.
#' * eqs. (17) and (20): \eqn{m_{k,p}} and \eqn{M(p, k)}. \eqn{m_{k,p}} is
#'   computed in closed form, \eqn{m_{k,p} = k^{p/2} m_p^2\,
#'   {}_2F_1(-p/2, -p/2; 1/2; 1/k)}, so non-integer \eqn{p} is allowed; for
#'   \eqn{p = 4}, \eqn{M(4, k) = 16k(2k^2 - k - 1)/3} (Section 5.3).
#' * eqs. (21) and (30): \eqn{\hat A(p, \Delta_n)} and
#'   \eqn{\hat V^c_n = \Delta_n M(p,k) \hat A(2p, \Delta_n)/\hat A(p,
#'   \Delta_n)^2} (`variance = "truncated"`).
#' * eqs. (22) and (31): \eqn{\tilde A(r, q, \Delta_n)} and \eqn{\tilde V^c_n}
#'   with \eqn{r = p/([p]+1)}, \eqn{q = 2[p]+2} and \eqn{[p]+1}
#'   (`variance = "multipower"`).
#' * eqs. (24) and (29): \eqn{\hat D(2p-2, \Delta_n)} over the local windows
#'   \eqn{I_{n,t}(i) = \{j \ne i: |i - j| \le k_n\}} and
#'   \eqn{\hat V^j_n = \Delta_n (k-1) p^2 \hat D(2p-2, \Delta_n) / (2 \hat
#'   B(p, \Delta_n)^2)}.
#' * eqs. (32) and (35): `critical_no_jumps`, `reject_no_jumps`.
#' * eqs. (36), (40) and (39): `critical_jumps`, `reject_jumps`
#'   (`jump_critical = "normal"` or `"chebyshev"`).
#' @section Implementation choices:
#' * **Truncation.** The default is the paper's simulation setting
#'   \eqn{u_n = 5\beta^{1/2}\Delta_n^{0.47}} (Table 1 note), written as
#'   `truncation = "rate"`, `alpha = 5`, `varpi = 0.47`, with
#'   \eqn{\beta^{1/2}} replaced by `sigma = 0.25`, the realistic volatility of
#'   AJ (2012). Set `sigma` to a realistic value for the asset (Section 5.3),
#'   or use `truncation = "sd"`, which scales the cutoff with a pilot
#'   estimate (AJ 2012 practice). The same \eqn{u_n} is used in eqs. (21) and
#'   (24); the notes to Tables 2-3 specify only \eqn{k_n}.
#' * **Time units.** The truncation uses \eqn{\Delta_n} = `horizon / n` in
#'   the unit of `sigma` (years by default), as \eqn{\alpha = 5\beta^{1/2}}
#'   is an annualised volatility. \eqn{k_n = [50\Delta_n^{-1/4}]} (Tables 2-3)
#'   is computed with time in units of the window, \eqn{\Delta_n = 1/n}: the
#'   simulations use \eqn{t = 1} day (Section 6), giving \eqn{k_n} = 618, 437
#'   and 314 at 1, 4 and 15 seconds. With \eqn{\Delta_n} in years the windows
#'   cover most of the day (1251 for 1560 increments at 15 seconds), the
#'   divisor \eqn{k_n\Delta_n} of eq. (24) then biases \eqn{\hat V^j_n}
#'   downwards near the edges, and in our simulations of the Table 2 design
#'   (one jump per day, 15 seconds) the 5% jump-null test rejects 9.5% of
#'   the time instead of 6.8% (Table 2: 5.3%). Pass `kn` to set it.
#' * **Variance and critical values.** The defaults are those of the
#'   simulations: \eqn{\hat V^c_n} of eq. (30) (Table 1) and the normal
#'   critical value (40) (Tables 2-3). Eq. (39) holds also when \eqn{X} and
#'   \eqn{\sigma} jump together, and is conservative.
#' * **p-values.** \eqn{\Phi(z)} for the no-jump null and \eqn{1 - \Phi(z)}
#'   for the jump null with eq. (40). With eq. (39) the test rejects iff
#'   \eqn{z > \alpha^{-1/2}}, so the p-value is \eqn{\min(1, z^{-2})} for
#'   \eqn{z > 0} and 1 otherwise.
#' * **Edges.** Near the ends of the window \eqn{I_{n,t}(i)} has fewer than
#'   \eqn{2k_n} elements; the divisor \eqn{k_n\Delta_n} of eq. (24) is kept.
#' * **Noise.** The test assumes no microstructure noise. Under additive
#'   noise \eqn{\hat S \to 1/k} (Section 5.4), which the no-jump test rejects
#'   and the jump test does not.
#' @references
#' Ait-Sahalia, Y. and Jacod, J. (2009a). Testing for jumps in a discretely
#' observed process. *Annals of Statistics* 37(1), 184-222.
#' \doi{10.1214/07-AOS568}. (Cited as 2009b in AJ 2011 and AJ 2012.)
#'
#' Ait-Sahalia, Y. and Jacod, J. (2012). Analyzing the spectrum of asset
#' returns: jump and volatility components in high frequency data.
#' *Journal of Economic Literature* 50(4), 1007-1050.
#' @seealso [aj_spectrum()] for \eqn{\hat S} over grids of tuning values.
#' @examples
#' n <- 23400 # one trading day of 1-second prices
#' x <- cumsum(c(0, rnorm(n, 0, 0.25 * sqrt(1 / (252 * n)))))
#' aj_jump_test(x)
#' x[15000:(n + 1)] <- x[15000:(n + 1)] + 0.01 # a 1% jump
#' aj_jump_test(x)
#' @export
aj_jump_test <- function(x, options = aj_jump_options()) {
  check_class(options, AJJumpOptions, "options")
  y <- log_prices(x)
  adx <- abs(diff(y))
  n <- length(adx)
  p <- options@p
  k <- options@k
  q_mp <- 2L * as.integer(floor(p)) + 2L
  if (n < max(2L * k, q_mp)) {
    hf_abort("`x` has %d increments; at least %d are needed.", n, max(2L * k, q_mp))
  }
  b1 <- aj_power_sum(adx, p)
  if (b1 == 0) hf_abort("`x` has no non-zero increments.")
  bk <- aj_power_sum(abs(aj_increments(y, k)), p)
  stat <- bk / b1 # eq. (12)

  cut <- aj_options_cutoff(adx, options)
  u <- options@alpha * cut$scale
  delta <- cut$delta
  level <- options@level
  z_level <- stats::qnorm(level, lower.tail = FALSE)

  # Null of no jumps: eqs. (30)/(31), (32), (35).
  limit_c <- k^(p / 2 - 1)
  Vc <- if (options@variance == "truncated") {
    delta * aj_M(p, k) * aj_A_trunc(adx, 2 * p, u, delta) / aj_A_trunc(adx, p, u, delta)^2
  } else {
    r <- p / (floor(p) + 1)
    q <- floor(p) + 1
    delta * aj_M(p, k) * aj_A_multipower(adx, r, 2 * q, delta) /
      aj_A_multipower(adx, r, q, delta)^2
  }
  nj <- aj_one_sided(stat, limit_c, Vc, z_level, lower = TRUE, "no-jump")

  # Null of jumps: eqs. (24), (29), (36), (39)/(40).
  kn <- options@kn
  # [50 Delta_n^(-1/4)] with time in units of the window (t = 1, Section 6)
  if (is.na(kn)) kn <- as.integer(floor(50 * n^(1 / 4)))
  kn <- max(1L, min(kn, n - 1L))
  Vj <- delta * (k - 1) * p^2 * aj_D_hat(adx, 2 * p - 2, u, kn, delta) / (2 * b1^2)
  if (options@jump_critical == "normal") {
    jp <- aj_one_sided(stat, 1, Vj, z_level, lower = FALSE, "jump")
  } else {
    jp <- aj_one_sided(stat, 1, Vj, 1 / sqrt(level), lower = FALSE, "jump")
    jp$p_value <- if (is.na(jp$z)) NA_real_ else if (jp$z > 0) min(1, jp$z^(-2)) else 1
  }

  AJJumpTest(
    method = "aj_jump_test",
    reference = "Ait-Sahalia and Jacod (2009a), Ann. Statist. 37(1), 184-222",
    statistic = stat, p = p, k = k, n = as.integer(n), level = level,
    limits = c(jumps = 1, continuous = limit_c, noise = 1 / k, rounding = k^(-1 / 2)),
    variance_no_jumps = nj$variance, z_no_jumps = nj$z, p_value_no_jumps = nj$p_value,
    critical_no_jumps = nj$critical, reject_no_jumps = nj$reject,
    variance_jumps = jp$variance, z_jumps = jp$z, p_value_jumps = jp$p_value,
    critical_jumps = jp$critical, reject_jumps = jp$reject,
    cutoff = u, sigma_hat = cut$sigma_hat, kn = kn, options = options
  )
}

# One-sided test of `stat` against `limit` with variance `V` and critical
# multiplier `zc`: lower = TRUE rejects for stat < limit - zc sqrt(V), lower =
# FALSE for stat > limit + zc sqrt(V). Normal p-values in the tail tested.
aj_one_sided <- function(stat, limit, V, zc, lower, label) {
  if (!is.finite(V) || V <= 0) {
    hf_warn("The variance estimate under the %s null is not positive (%s); the test is not available.",
            label, format(V))
    return(list(variance = as.double(V), z = NA_real_, p_value = NA_real_,
                critical = NA_real_, reject = NA))
  }
  z <- (stat - limit) / sqrt(V)
  if (lower) {
    critical <- limit - zc * sqrt(V)
    list(variance = V, z = z, p_value = stats::pnorm(z), critical = critical,
         reject = stat < critical)
  } else {
    critical <- limit + zc * sqrt(V)
    list(variance = V, z = z, p_value = stats::pnorm(z, lower.tail = FALSE),
         critical = critical, reject = stat > critical)
  }
}

aj_decision <- function(reject) {
  if (is.na(reject)) "not available" else if (reject) "reject" else "do not reject"
}

method(print, AJJumpTest) <- function(x, ...) {
  o <- x@options
  cat(sprintf("<AJJumpTest> %s (%s)\n", x@method, x@reference))
  cat(sprintf("  S(p = %s, k = %d) = %s   [n = %d]\n", format(x@p), x@k,
              aj_fmt(x@statistic, 5), x@n))
  cat(sprintf("  limits: jumps 1 | continuous %s | additive noise %s\n",
              aj_fmt(x@limits[["continuous"]]), aj_fmt(x@limits[["noise"]])))
  cat(sprintf("  H0 no jumps: z = %s, p-value = %s; reject if S < %s -> %s\n",
              aj_fmt(x@z_no_jumps), aj_fmt(x@p_value_no_jumps),
              aj_fmt(x@critical_no_jumps), aj_decision(x@reject_no_jumps)))
  cat(sprintf("  H0 jumps:    z = %s, p-value = %s; reject if S > %s -> %s\n",
              aj_fmt(x@z_jumps), aj_fmt(x@p_value_jumps), aj_fmt(x@critical_jumps),
              aj_decision(x@reject_jumps)))
  cat(sprintf("  level %s | variance %s | %s critical value | u_n = %s | kn = %d\n",
              format(x@level), o@variance, o@jump_critical, aj_fmt(x@cutoff), x@kn))
  invisible(x)
}

method(as.data.frame, AJJumpTest) <- function(x, ...) {
  data.frame(
    method = x@method, statistic = x@statistic, p = x@p, k = x@k, n = x@n,
    level = x@level, limit_jumps = x@limits[["jumps"]],
    limit_continuous = x@limits[["continuous"]], limit_noise = x@limits[["noise"]],
    variance_no_jumps = x@variance_no_jumps, z_no_jumps = x@z_no_jumps,
    p_value_no_jumps = x@p_value_no_jumps, critical_no_jumps = x@critical_no_jumps,
    reject_no_jumps = x@reject_no_jumps, variance_jumps = x@variance_jumps,
    z_jumps = x@z_jumps, p_value_jumps = x@p_value_jumps,
    critical_jumps = x@critical_jumps, reject_jumps = x@reject_jumps,
    cutoff = x@cutoff, sigma_hat = x@sigma_hat, kn = x@kn,
    stringsAsFactors = FALSE
  )
}
