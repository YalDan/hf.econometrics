#' @include aj-power-variation.R
NULL

#' Options for the Ait-Sahalia and Jacod (2012) spectrum
#'
#' Grids of tuning values for [aj_spectrum()]. Create them with
#' [aj_spectrum_options()], whose defaults are the grids of the empirical
#' section of Ait-Sahalia and Jacod (2012).
#'
#' @param deltas Sampling intervals \eqn{\Delta_n} in seconds.
#' @param p Powers \eqn{p > 2}.
#' @param k Integer sampling multiples \eqn{k \ge 2}.
#' @param alpha Truncation levels of \eqn{S_{FA}}, see [aj_truncation()].
#' @param statistics Statistics to compute: any of `"SJ"` and `"SFA"`.
#' @param truncation,varpi,sigma,pilot,horizon Truncation of \eqn{S_{FA}},
#'   see [aj_truncation()].
#' @export
AJSpectrumOptions <- new_class(
  "AJSpectrumOptions",
  properties = c(list(
    deltas = class_double,
    p = class_double,
    k = class_integer,
    statistics = class_character
  ), aj_truncation_properties),
  validator = function(self) {
    collect_msgs(
      if (!length(self@deltas) || anyNA(self@deltas) || any(self@deltas <= 0)) {
        "@deltas must be positive numbers (seconds)."
      },
      if (!length(self@p) || anyNA(self@p) || any(self@p <= 2)) "@p must be numbers above 2.",
      if (!length(self@k) || anyNA(self@k) || any(self@k < 2L)) "@k must be integers of at least 2.",
      if (!length(self@statistics) || !all(self@statistics %in% c("SJ", "SFA"))) {
        "@statistics must be a subset of \"SJ\", \"SFA\"."
      },
      aj_truncation_msgs(self, alpha_scalar = FALSE)
    )
  }
)

#' Create options for the Ait-Sahalia and Jacod (2012) spectrum
#'
#' The defaults are the grids of Ait-Sahalia and Jacod (2012, Section IX;
#' pp. 30-33 of the working paper): \eqn{p} from 3 to 6 by 0.25,
#' \eqn{\Delta_n} = 5, 15, 30, 60, 90 and 120 seconds, \eqn{k} = 2, 3, and
#' for \eqn{S_{FA}} truncation levels from 5 to 10 standard deviations of the
#' continuous part, after a pilot estimate with \eqn{\sigma = 0.25}.
#'
#' @inheritParams AJSpectrumOptions
#' @param deltas Sampling intervals in seconds; each must be a multiple of
#'   the base interval of the data.
#' @param alpha Truncation levels of \eqn{S_{FA}} in standard deviations
#'   (`truncation = "sd"`) or multiples of `sigma` (`"rate"`).
#' @param truncation `"sd"` (default) or `"rate"`.
#' @param varpi Rate for `truncation = "rate"`.
#' @param sigma,pilot,horizon See [aj_truncation()]; `horizon` is the length
#'   of the window in the time unit of `sigma`.
#' @return An [AJSpectrumOptions] object.
#' @examples
#' aj_spectrum_options()
#' aj_spectrum_options(deltas = c(5, 10), statistics = "SFA", horizon = 1 / 365.25)
#' @export
aj_spectrum_options <- function(deltas = c(5, 15, 30, 60, 90, 120),
                                p = seq(3, 6, by = 0.25), k = 2:3, alpha = 5:10,
                                statistics = c("SJ", "SFA"), truncation = "sd",
                                varpi = 0.5, sigma = 0.25, pilot = 4,
                                horizon = 1 / 252) {
  deltas <- check_numeric(deltas, "deltas")
  if (any(deltas <= 0)) hf_abort("`deltas` must be positive.")
  p <- check_numeric(p, "p")
  if (any(p <= 2)) hf_abort("`p` must be greater than 2.")
  k <- check_numeric(k, "k")
  if (any(k < 2 | abs(k - round(k)) > 1e-8)) hf_abort("`k` must be whole numbers of at least 2.")
  alpha <- check_numeric(alpha, "alpha")
  if (any(alpha <= 0)) hf_abort("`alpha` must be positive.")
  if (!is.character(statistics) || !length(statistics) ||
      !all(statistics %in% c("SJ", "SFA"))) {
    hf_abort("`statistics` must be a subset of \"SJ\", \"SFA\".")
  }
  aj_check_truncation_args(truncation, varpi, sigma, pilot, horizon)
  aj_new(AJSpectrumOptions,
    deltas = deltas, p = p, k = as.integer(round(k)), alpha = alpha,
    statistics = unique(statistics), truncation = truncation,
    varpi = as.double(varpi), sigma = as.double(sigma), pilot = as.double(pilot),
    horizon = as.double(horizon)
  )
}

#' Spectrum of power-variation ratios of Ait-Sahalia and Jacod (2012)
#'
#' Computes the jump statistic
#' \deqn{S_J(p, k, \Delta_n) = \frac{B(p, \infty, k\Delta_n)}{B(p, \infty, \Delta_n)}}
#' and the finite-activity statistic
#' \deqn{S_{FA}(p, u_n, k, \Delta_n) = \frac{B(p, u_n, k\Delta_n)}{B(p, u_n, \Delta_n)}}
#' of Ait-Sahalia and Jacod (2012, eqs. 14 and 16) over grids of the power
#' \eqn{p}, the sampling interval \eqn{\Delta_n}, the multiple \eqn{k} and the
#' truncation level, the data behind their signature plots (Figures 9-10).
#' Each row also carries the possible limits: \eqn{S_J} tends to 1 with
#' jumps, to \eqn{k^{p/2-1}} on continuous paths and to \eqn{1/k} when
#' additive noise dominates (eqs. 15, 40); \eqn{S_{FA}} tends to 1 with
#' infinite-activity jumps, to \eqn{k^{p/2-1}} with finite activity and to
#' \eqn{1/k} under noise (eqs. 17, 41). For tests at one grid point use
#' [aj_jump_test()] and [aj_finite_activity_test()].
#'
#' @param x Log prices of one window at a regular base interval: a numeric
#'   vector or an [HFSeries].
#' @param base_interval Seconds between consecutive observations of `x`. For
#'   an [HFSeries] it defaults to the median spacing of its times; for a
#'   numeric vector it must be given.
#' @param options An [AJSpectrumOptions] object.
#' @return A data frame with one row per statistic and grid point:
#'   `statistic` (`"SJ"` or `"SFA"`), `delta` (seconds), `k`, `p`, `alpha`
#'   (`NA` for \eqn{S_J}), `cutoff` (\eqn{u_n}; `Inf` for \eqn{S_J}),
#'   `sigma_hat` (pilot volatility at that \eqn{\Delta_n}; `NA` for
#'   \eqn{S_J}), `n` (increments at \eqn{\Delta_n}), `value`, and
#'   `limit_jumps` (1: jumps, or infinite-activity jumps for \eqn{S_{FA}}),
#'   `limit_continuous` (\eqn{k^{p/2-1}}: continuous path, or finite activity
#'   for \eqn{S_{FA}}) and `limit_noise` (\eqn{1/k}).
#' @section Paper mapping:
#' Equation and page numbers refer to Ait-Sahalia and Jacod (2012), NBER
#' working paper 15808.
#' * eq. (7): \eqn{B(p, u_n, \Delta_n)}, [aj_power_variation()];
#'   \eqn{k\Delta_n} retains one out of every \eqn{k} prices (p. 8).
#' * eqs. (14)-(15): `statistic = "SJ"` and its limits.
#' * eqs. (16)-(17): `statistic = "SFA"` and its limits, with the same
#'   \eqn{u_n} at both frequencies.
#' * eqs. (40)-(41): `limit_noise`.
#' * p. 30: \eqn{p} from 3 to 6 by 0.25, \eqn{\Delta_n} = 5, 15, 30, 60, 90,
#'   120 s, \eqn{k} = 2, 3.
#' * p. 32: \eqn{u_n} in standard deviations of the continuous part, from the
#'   pilot \eqn{B(2, 4\sigma\Delta_n^{1/2}, \Delta_n)} with \eqn{\sigma = 0.25}
#'   ([aj_truncation()]). p. 33: \eqn{\alpha} from 5 to 10.
#' @section Implementation choices:
#' * **Sampling.** Prices at \eqn{\Delta_n} are every
#'   `delta / base_interval`-th observation of `x`, and increments are
#'   differences of those prices.
#' * **Time units.** `base_interval` only places the grid in seconds.
#'   \eqn{\Delta_n} in the unit of `sigma` is
#'   `horizon * delta / (base_interval * N)`, where `N` is the number of base
#'   increments (`horizon` = 1/252 is one trading day in years). It enters the
#'   pilot cutoff only.
#' * **Pilot.** The pilot volatility is recomputed at each \eqn{\Delta_n}, so
#'   \eqn{\alpha} counts standard deviations of the \eqn{\Delta_n}
#'   increments.
#' * **Grids.** The paper gives no step for \eqn{\alpha}; the default uses
#'   integers, and the \eqn{p} grid of \eqn{S_J} is also used for
#'   \eqn{S_{FA}}.
#' * **Windows.** One call is one window; the paper pools a quarter of
#'   trading days. Intervals that leave fewer than `2 * max(k)` increments
#'   are dropped with a warning.
#' @references
#' Ait-Sahalia, Y. and Jacod, J. (2012). Analyzing the spectrum of asset
#' returns: jump and volatility components in high frequency data.
#' *Journal of Economic Literature* 50(4), 1007-1050. NBER Working Paper
#' 15808 (2010).
#' @examples
#' n <- 23400 # one trading day of 1-second prices
#' x <- cumsum(c(0, rnorm(n, 0, 0.25 * sqrt(1 / (252 * n)))))
#' sp <- aj_spectrum(x, base_interval = 1)
#' head(sp)
#' aggregate(value ~ statistic + delta, data = sp, FUN = median)
#' @export
aj_spectrum <- function(x, base_interval = NULL, options = aj_spectrum_options()) {
  check_class(options, AJSpectrumOptions, "options")
  y <- log_prices(x)
  if (is.null(base_interval)) {
    if (!S7::S7_inherits(x, HFSeries)) {
      hf_abort("`base_interval` (seconds between observations) is required for a numeric `x`.")
    }
    dt <- diff(as.numeric(obs_times(x)))
    base_interval <- stats::median(dt)
    if (!(base_interval > 0)) hf_abort("The observation times of `x` are not increasing.")
    if (any(abs(dt - base_interval) > 1e-6 * base_interval)) {
      hf_warn("`x` is not equally spaced; its median spacing (%s s) is used as the base interval.",
              format(base_interval))
    }
  }
  check_number(base_interval, "base_interval", lower = 0, open = c(TRUE, FALSE))
  m <- options@deltas / base_interval
  if (any(abs(m - round(m)) > 1e-8 * pmax(1, m) | round(m) < 1)) {
    hf_abort("Each of `deltas` must be a multiple of `base_interval` (%s s).",
             format(base_interval))
  }
  m <- as.integer(round(m))
  n_base <- length(y) - 1L
  ks <- options@k
  ps <- options@p
  out <- list()
  for (j in seq_along(m)) {
    yj <- y[seq.int(1L, length(y), by = m[j])]
    adx <- abs(diff(yj))
    nj <- length(adx)
    if (nj < 2L * max(ks)) {
      hf_warn("Delta_n = %s s leaves %d increments; it is skipped.", format(options@deltas[j]), nj)
      next
    }
    adk <- lapply(ks, function(kk) abs(aj_increments(yj, kk)))
    if ("SJ" %in% options@statistics) {
      out[[length(out) + 1L]] <- aj_spectrum_block(
        "SJ", options@deltas[j], adx, adk, ks, ps, NA_real_, Inf, NA_real_
      )
    }
    if ("SFA" %in% options@statistics) {
      sc <- aj_cutoff_scale(adx, options@horizon * m[j] / n_base, options@truncation,
                            options@varpi, options@sigma, options@pilot)
      for (a in options@alpha) {
        out[[length(out) + 1L]] <- aj_spectrum_block(
          "SFA", options@deltas[j], adx, adk, ks, ps, a, a * sc$scale, sc$sigma_hat
        )
      }
    }
  }
  if (!length(out)) {
    hf_warn("No sampling interval leaves enough increments; the spectrum is empty.")
    return(aj_spectrum_frame(character(), double(), integer(), double(), double(),
                             double(), double(), integer(), double()))
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# Ratios B(p, u, k Delta) / B(p, u, Delta) for all p and k at one Delta_n and
# one cutoff u (u = Inf for S_J).
aj_spectrum_block <- function(statistic, delta, adx, adk, ks, ps, alpha, u, sigma_hat) {
  b1 <- vapply(ps, function(p) aj_power_sum(adx, p, u), numeric(1))
  bk <- vapply(adk, function(a) vapply(ps, function(p) aj_power_sum(a, p, u), numeric(1)),
               numeric(length(ps)))
  value <- as.vector(matrix(bk, nrow = length(ps)) / b1)
  nk <- length(ks)
  aj_spectrum_frame(statistic, delta, rep(ks, each = length(ps)), rep(ps, nk), alpha, u,
                    sigma_hat, length(adx), value)
}

aj_spectrum_frame <- function(statistic, delta, k, p, alpha, cutoff, sigma_hat, n, value) {
  data.frame(
    statistic = rep(statistic, length.out = length(k)),
    delta = rep(delta, length.out = length(k)), k = k, p = p,
    alpha = rep(alpha, length.out = length(k)),
    cutoff = rep(cutoff, length.out = length(k)),
    sigma_hat = rep(sigma_hat, length.out = length(k)),
    n = rep(as.integer(n), length.out = length(k)), value = value,
    limit_jumps = rep(1, length(k)), limit_continuous = k^(p / 2 - 1),
    limit_noise = 1 / k, stringsAsFactors = FALSE
  )
}
