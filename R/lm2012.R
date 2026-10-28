#' @include class-result.R class-series.R
NULL

# Options -------------------------------------------------------------------

#' Options for the Lee and Mykland (2012) jump test
#'
#' `lm_options()` builds the typed [LMOptions] object that [lm_test()] takes.
#' The defaults follow the paper: \eqn{k} is selected from the
#' autocorrelation of the returns at the 5% level over lags up to 10
#' (Section 6.2, Fig. 1), \eqn{C} is read from Table 5 at the estimated noise
#' level (Section 5.4), and \eqn{\hat{\sigma}} comes from the Podolskij-Vetter
#' (2009) estimator (Section 4.2). Every argument is checked; invalid values
#' and inconsistent combinations raise an error of class `hf_input_error`.
#'
#' @param k Order of the noise dependence plus one (Remark 1): every
#'   \eqn{k}-th observation is used. `NULL` (default) selects \eqn{k} from
#'   the data with [lm_select_k()].
#' @param C Block-size constant in \eqn{M \sim C \lfloor n/k \rfloor^{1/2}}
#'   (eq. 7). `NULL` (default) reads it from Table 5 at the estimated noise
#'   standard deviation, see [lm_select_C()].
#' @param acf_level,acf_max_lag,acf_band Level, largest lag and band of the
#'   lag rule, passed to [lm_select_k()] as `level`, `max_lag` and `band`.
#'   Used only when `k` is `NULL`.
#' @param iv Integrated variance \eqn{\sigma^2 T} of the window: a
#'   non-negative number or an [IVEstimate]. It replaces the estimate from
#'   `iv_method`, for example to supply the true value in a simulation or an
#'   estimate computed elsewhere.
#' @param iv_method Estimator of the integrated variance, applied to the
#'   \eqn{k}-subsampled log prices: the name of a method of
#'   `integrated_variance()`, or a function `function(x, ...)` that returns an
#'   [IVEstimate] or a single number. The default `"pv2009"` is the
#'   Podolskij and Vetter (2009) estimator that the paper prescribes.
#' @param iv_args Named list of further arguments for the estimator.
#' @param variance `"plugin"` (default) standardises by the plug-in
#'   \eqn{\hat V_n = \frac23 C^2 \hat{\sigma}^2 T + 2\hat q^2} of Section 4.2.
#'   `"empirical"` uses the sample variance of \eqn{\sqrt M \mathcal L(t_j)}
#'   instead. That option is not in the paper; a jump inflates it.
#' @param alpha Significance level of the test of the window. The default
#'   is 1%. The Monte Carlo study of the paper uses 5% (Tables 2-4), and its
#'   empirical study controls the overall error rate across windows at 1%
#'   and 5% (Section 6.2, see [lm_holm()]).
#' @param flag_blocks Add per-block jump flags to the block table? This
#'   extension is described in [lm_test()].
#' @param M Block size, a positive integer that replaces `C`; the test then
#'   reports \eqn{C = M / \lfloor n/k \rfloor^{1/2}}. Not in the paper.
#' @param demean Use the drift-robust statistic of Appendix 8.1, which
#'   subtracts \eqn{\hat{\mu}_{\mathcal L} = \lfloor n/(kM) \rfloor^{-1}
#'   \sum_j \mathcal L(t_j)} from every \eqn{\mathcal L(t_j)}?
#' @param M_rounding How \eqn{C \lfloor n/k \rfloor^{1/2}} becomes an
#'   integer block size: `"floor"` (default), `"round"` or `"ceiling"`.
#'   \eqn{M} is at least 1.
#' @param C_lookup How Table 5 is read between its rows, passed to
#'   [lm_select_C()] as `method`.
#' @return An [LMOptions] object.
#' @inherit lm_select_k references
#' @seealso [lm_test()]
#' @examples
#' lm_options()
#' lm_options(k = 1, C = 1 / 19, iv = 9e-4)
#' @export
lm_options <- function(k = NULL, C = NULL, acf_level = 0.05, acf_max_lag = 10L,
                       iv = NULL, iv_method = "pv2009", iv_args = list(),
                       variance = c("plugin", "empirical"), alpha = 0.01,
                       flag_blocks = TRUE, M = NULL, demean = FALSE,
                       M_rounding = c("floor", "round", "ceiling"),
                       C_lookup = c("nearest", "linear"),
                       acf_band = c("bartlett", "white")) {
  k <- check_count(k, "k", min = 1L, allow_null = TRUE)
  check_number(C, "C", lower = 0, upper = Inf, open = c(TRUE, TRUE),
               allow_null = TRUE)
  M <- check_count(M, "M", min = 1L, allow_null = TRUE)
  if (!is.null(C) && !is.null(M)) hf_abort("Supply at most one of `C` and `M`.")
  check_probability(acf_level, "acf_level")
  acf_max_lag <- check_count(acf_max_lag, "acf_max_lag", min = 2L)
  acf_band <- lm_choice(acf_band, c("bartlett", "white"), "acf_band")
  if (!is.null(iv)) {
    if (S7::S7_inherits(iv, IVEstimate)) {
      if (!is.finite(iv@value)) hf_abort("`iv@value` must be finite.")
    } else {
      check_number(iv, "iv", lower = 0, upper = Inf, open = c(FALSE, TRUE))
      iv <- as.double(iv)
    }
  }
  if (!is.function(iv_method)) check_string(iv_method, "iv_method")
  if (!is.list(iv_args) || is.object(iv_args) ||
      (length(iv_args) && (is.null(names(iv_args)) || !all(nzchar(names(iv_args)))))) {
    hf_abort("`iv_args` must be a named list, not %s.", obj_type(iv_args))
  }
  variance <- lm_choice(variance, c("plugin", "empirical"), "variance")
  if (!is.null(iv) && variance == "empirical") {
    hf_abort("`iv` is only used by the plug-in variance; drop it or set `variance = \"plugin\"`.")
  }
  check_probability(alpha, "alpha")
  check_flag(flag_blocks, "flag_blocks")
  check_flag(demean, "demean")
  M_rounding <- lm_choice(M_rounding, c("floor", "round", "ceiling"), "M_rounding")
  C_lookup <- lm_choice(C_lookup, c("nearest", "linear"), "C_lookup")
  LMOptions(
    k = k, C = if (is.null(C)) NULL else as.double(C), M = M,
    acf_level = as.double(acf_level), acf_max_lag = acf_max_lag,
    acf_band = acf_band, iv = iv, iv_method = iv_method, iv_args = iv_args,
    variance = variance, alpha = as.double(alpha), flag_blocks = flag_blocks,
    demean = demean, M_rounding = M_rounding, C_lookup = C_lookup
  )
}

#' @rdname lm_options
#' @export
LMOptions <- new_class(
  "LMOptions",
  properties = list(
    k = new_property(NULL | class_integer, default = NULL),
    C = new_property(NULL | class_double, default = NULL),
    M = new_property(NULL | class_integer, default = NULL),
    acf_level = new_property(class_double, default = 0.05),
    acf_max_lag = new_property(class_integer, default = 10L),
    acf_band = new_property(class_character, default = "bartlett"),
    iv = new_property(NULL | class_double | IVEstimate, default = NULL),
    iv_method = new_property(class_character | class_function, default = "pv2009"),
    iv_args = new_property(class_list, default = list()),
    variance = new_property(class_character, default = "plugin"),
    alpha = new_property(class_double, default = 0.01),
    flag_blocks = new_property(class_logical, default = TRUE),
    demean = new_property(class_logical, default = FALSE),
    M_rounding = new_property(class_character, default = "floor"),
    C_lookup = new_property(class_character, default = "nearest")
  ),
  validator = function(self) {
    collect_msgs(
      if (!is.null(self@k)) check_count(self@k, "@k", abort = FALSE),
      if (!is.null(self@C)) {
        check_number(self@C, "@C", lower = 0, upper = Inf, open = c(TRUE, TRUE),
                     abort = FALSE)
      },
      if (!is.null(self@M)) check_count(self@M, "@M", abort = FALSE),
      if (!is.null(self@C) && !is.null(self@M)) "Supply at most one of @C and @M.",
      check_probability(self@acf_level, "@acf_level", abort = FALSE),
      check_count(self@acf_max_lag, "@acf_max_lag", min = 2L, abort = FALSE),
      check_choice(self@acf_band, c("bartlett", "white"), "@acf_band", abort = FALSE),
      if (is.double(self@iv)) {
        check_number(self@iv, "@iv", lower = 0, upper = Inf, open = c(FALSE, TRUE),
                     abort = FALSE)
      },
      if (S7::S7_inherits(self@iv, IVEstimate) && !is.finite(self@iv@value)) {
        "@iv@value must be finite."
      },
      if (is.character(self@iv_method)) {
        check_string(self@iv_method, "@iv_method", abort = FALSE)
      },
      if (length(self@iv_args) && (is.null(names(self@iv_args)) ||
                                   !all(nzchar(names(self@iv_args))))) {
        "@iv_args must be a named list."
      },
      check_choice(self@variance, c("plugin", "empirical"), "@variance", abort = FALSE),
      if (!is.null(self@iv) && identical(self@variance, "empirical")) {
        "@iv is only used by the plug-in variance."
      },
      check_probability(self@alpha, "@alpha", abort = FALSE),
      check_flag(self@flag_blocks, "@flag_blocks", abort = FALSE),
      check_flag(self@demean, "@demean", abort = FALSE),
      check_choice(self@M_rounding, c("floor", "round", "ceiling"), "@M_rounding",
                   abort = FALSE),
      check_choice(self@C_lookup, c("nearest", "linear"), "@C_lookup", abort = FALSE)
    )
  }
)

# Result --------------------------------------------------------------------

#' Result of the Lee and Mykland (2012) jump test
#'
#' [lm_test()] returns an `LMTest`. `print()` summarises it,
#' `as.data.frame()` gives the one-row summary of the window and
#' [lm_blocks()] the table of block statistics.
#'
#' @param n Number of increments in the window.
#' @param k Order of the noise dependence plus one; every \eqn{k}-th
#'   observation is used.
#' @param M Block size: the number of subsampled prices averaged per block.
#' @param C Block-size constant: from Table 5, supplied, or implied by a
#'   supplied `M`.
#' @param C_effective \eqn{M / \lfloor n/k \rfloor^{1/2}}, the constant that
#'   enters the plug-in variance.
#' @param n_blocks Number of block differences \eqn{\mathcal L(t_j)} in the
#'   maximum.
#' @param q_hat Noise standard deviation of Proposition 1.
#' @param iv The [IVEstimate] used in the plug-in variance, or `NULL` for the
#'   empirical variance.
#' @param V_n Variance \eqn{\hat V_n} used to standardise.
#' @param A_n,B_n Centring and scaling constants of eq. (13).
#' @param max_chi \eqn{\max_j |\mathcal X(t_j)|}.
#' @param statistic \eqn{\hat{\xi}_n} of eq. (14).
#' @param p_value \eqn{1 - \exp(-e^{-\hat{\xi}_n})}.
#' @param alpha Significance level.
#' @param beta_star Gumbel quantile \eqn{\beta^* = -\log(-\log(1 - \alpha))}.
#' @param threshold Critical value \eqn{A_n + \beta^* B_n} for
#'   \eqn{|\mathcal X(t_j)|}.
#' @param reject Is the null hypothesis of no jump in the window rejected?
#' @param acf Sample autocorrelations of the returns at lags
#'   \eqn{1, \dots,} `acf_max_lag` (empty when `k` was supplied).
#' @param acf_band Half-width of the band of the lag rule (`NA` when `k` was
#'   supplied).
#' @param blocks Data frame with one row per block difference: `index`
#'   (tick index \eqn{j} of \eqn{t_j}, counting from 0), `time`
#'   (\eqn{t_j} from [obs_times()]), `time_end` (time of the last observation
#'   that enters \eqn{\mathcal L(t_j)}), `P_hat` (\eqn{\hat P(t_j)}), `L`
#'   (\eqn{\mathcal L(t_j)}), `chi` (\eqn{\mathcal X(t_j)}) and, if
#'   `flag_blocks`, `jump` (\eqn{-1}, 0 or 1).
#' @param start,end Times of the first and last observation of the window.
#' @param options The [LMOptions] used.
#' @inheritParams HFResult
#' @export
LMTest <- new_class(
  "LMTest",
  parent = HFResult,
  properties = list(
    n = class_integer,
    k = class_integer,
    M = class_integer,
    C = class_double,
    C_effective = class_double,
    n_blocks = class_integer,
    q_hat = class_double,
    iv = new_property(NULL | IVEstimate, default = NULL),
    V_n = class_double,
    A_n = class_double,
    B_n = class_double,
    max_chi = class_double,
    statistic = class_double,
    p_value = class_double,
    alpha = class_double,
    beta_star = class_double,
    threshold = class_double,
    reject = class_logical,
    acf = class_double,
    acf_band = class_double,
    blocks = new_property(class_data.frame, default = quote(data.frame())),
    start = class_any,
    end = class_any,
    options = LMOptions
  ),
  validator = function(self) {
    scalars <- list(self@n, self@k, self@M, self@C, self@C_effective,
                    self@n_blocks, self@q_hat, self@V_n, self@A_n, self@B_n,
                    self@max_chi, self@statistic, self@threshold)
    collect_msgs(
      if (!all(lengths(scalars) == 1L)) "The scalar properties must have length 1.",
      if (length(self@p_value) != 1L || is.na(self@p_value) ||
          self@p_value < 0 || self@p_value > 1) "@p_value must be a probability.",
      if (length(self@reject) != 1L || is.na(self@reject)) "@reject must be TRUE or FALSE.",
      if (nrow(self@blocks) != self@n_blocks) "@blocks must have @n_blocks rows."
    )
  }
)

method(print, LMTest) <- function(x, ...) {
  o <- x@options
  cat(sprintf("<LMTest> %s noise-robust jump test\n", x@reference))
  cat(sprintf("  window:   n = %d increments, %s -> %s\n", x@n,
              format(x@start), format(x@end)))
  k_how <- if (is.null(o@k)) {
    sprintf("largest acf lag in 2..%d outside the %g%% band", o@acf_max_lag,
            100 * (1 - o@acf_level))
  } else {
    "supplied"
  }
  cat(sprintf("  noise:    k = %d (%s), q_hat = %.4g\n", x@k, k_how, x@q_hat))
  C_how <- if (!is.null(o@M)) "implied by M" else if (is.null(o@C)) "Table 5" else "supplied"
  cat(sprintf("  blocks:   C = %.4g (%s), M = %d (effective C = %.4g), %d block differences\n",
              x@C, C_how, x@M, x@C_effective, x@n_blocks))
  if (is.null(x@iv)) {
    cat(sprintf("  variance: empirical, V_n = %.4g\n", x@V_n))
  } else {
    cat(sprintf("  variance: plug-in, V_n = %.4g (integrated variance %.4g, %s)\n",
                x@V_n, x@iv@value, x@iv@method))
  }
  cat(sprintf("  max|chi| = %.4g, A_n = %.4g, B_n = %.4g, xi = %.4g, p-value = %.4g\n",
              x@max_chi, x@A_n, x@B_n, x@statistic, x@p_value))
  cat(sprintf("  at alpha = %g: %s (|chi| threshold %.4g)\n", x@alpha,
              if (x@reject) "jump detected" else "no jump detected", x@threshold))
  if (!is.null(x@blocks$jump)) {
    hit <- which(x@blocks$jump != 0L)
    if (length(hit)) {
      shown <- utils::head(hit, 5L)
      cat(sprintf("  flagged blocks (%d): %s%s\n", length(hit),
                  paste(format(x@blocks$time[shown]), collapse = ", "),
                  if (length(hit) > 5L) ", ..." else ""))
    }
  }
  invisible(x)
}

method(as.data.frame, LMTest) <- function(x, ...) {
  data.frame(
    method = x@method, start = x@start, end = x@end, n = x@n, k = x@k,
    M = x@M, C = x@C, C_effective = x@C_effective, n_blocks = x@n_blocks,
    q_hat = x@q_hat,
    iv = if (is.null(x@iv)) NA_real_ else x@iv@value,
    iv_method = if (is.null(x@iv)) NA_character_ else x@iv@method,
    variance = x@options@variance, V_n = x@V_n, A_n = x@A_n, B_n = x@B_n,
    max_chi = x@max_chi, statistic = x@statistic, p_value = x@p_value,
    alpha = x@alpha, threshold = x@threshold, reject = x@reject,
    n_flagged = if (is.null(x@blocks$jump)) NA_integer_ else sum(x@blocks$jump != 0L),
    stringsAsFactors = FALSE
  )
}

# Test ----------------------------------------------------------------------

#' Lee and Mykland (2012) jump test robust to microstructure noise
#'
#' Tests one window for jumps in the efficient price when the observed log
#' prices \eqn{\tilde P(t_i) = P(t_i) + U(t_i)} carry \eqn{(k-1)}-dependent
#' noise. The prices are subsampled every \eqn{k} ticks and averaged over
#' non-overlapping blocks of \eqn{M} subsampled prices (Definition 1). The
#' differences of consecutive block averages are standardised by the
#' plug-in variance of Section 4.2, and their maximum is compared with its
#' Gumbel limit (Theorem 1):
#' 1. \eqn{k} from the autocorrelation of the returns ([lm_select_k()]);
#' 2. \eqn{\hat q = \widehat Q(k)/\sqrt 2} (Proposition 1, [lm_noise_sd()]);
#' 3. \eqn{C} from Table 5 at \eqn{\hat q} ([lm_select_C()]) and
#'    \eqn{M = \max(1, \lfloor C \lfloor n/k \rfloor^{1/2} \rfloor)};
#' 4. \eqn{\hat P(t_j) = \frac1M \sum_{i=\lfloor j/k \rfloor}^{\lfloor j/k
#'    \rfloor + M - 1} \tilde P(t_{ik})} for \eqn{j = 0, kM, 2kM, \dots} and
#'    \eqn{\mathcal L(t_j) = \hat P(t_{j+kM}) - \hat P(t_j)};
#' 5. \eqn{\hat V_n = \frac23 C^2 \hat{\sigma}^2 T + 2\hat q^2}, where
#'    \eqn{\hat{\sigma}^2 T} is the integrated variance of the window,
#'    estimated from the \eqn{k}-subsampled prices;
#' 6. \eqn{\mathcal X(t_j) = \sqrt M \mathcal L(t_j)/\sqrt{\hat V_n}},
#'    \eqn{\hat{\xi}_n = (\max_j |\mathcal X(t_j)| - A_n)/B_n} with \eqn{A_n,
#'    B_n} from eq. (13), and the p-value \eqn{1 - \exp(-e^{-\hat{\xi}_n})}.
#'
#' The null hypothesis of no jump in the window is rejected at level
#' \eqn{\alpha} when \eqn{\hat{\xi}_n > \beta^* = -\log(-\log(1-\alpha))},
#' that is, when some \eqn{|\mathcal X(t_j)|} exceeds
#' \eqn{A_n + \beta^* B_n}.
#'
#' **Locating jumps (extension).** The paper tests windows. With
#' `flag_blocks = TRUE`, `lm_test()` also flags every block difference with
#' \eqn{|\mathcal X(t_j)| > A_n + \beta^* B_n} by the sign of
#' \eqn{\mathcal X(t_j)}; the window is rejected exactly when at least one
#' block is flagged. A jump at time \eqn{\tau} enters \eqn{\mathcal L(t_j)}
#' with weight (number of prices of the block at \eqn{t_{j+kM}} observed at
#' or after \eqn{\tau}, minus that number for the block at \eqn{t_j}) / M.
#' The weight is positive exactly when `time < tau <= time_end` in the block
#' table and equals 1 when \eqn{\tau} falls between the two blocks. A jump
#' inside a block is shared by two consecutive differences, so both may be
#' flagged; at least one of them carries half of the jump or more.
#'
#' @param x Log prices of one window: a numeric vector, taken as equally
#'   spaced in tick time, or an [HFSeries].
#' @param options An [LMOptions] object from [lm_options()].
#' @return An [LMTest] object.
#'
#' @section Paper mapping:
#' | Paper | Code |
#' |---|---|
#' | Assumption A.1: grid \eqn{t_0 < \dots < t_n} | `n = length(log_prices(x)) - 1` |
#' | Remark 1; Sections 4.2 and 6.2; Fig. 1: order \eqn{k-1} of the noise dependence | `k`, [lm_select_k()] |
#' | Proposition 1, eqs. (16)-(17) | `q_hat`, [lm_noise_sd()] |
#' | Section 5.4, Table 5 | `C`, [lm_select_C()] |
#' | Definition 1, eq. (7): \eqn{M \sim C \lfloor n/k \rfloor^{1/2}} | `M`, `C_effective` |
#' | Definition 1: \eqn{\hat P(t_j)} on \eqn{\mathcal G_n^{kM}} | `blocks$index`, `blocks$P_hat` |
#' | eq. (8): \eqn{\mathcal L(t_j)} | `blocks$L` |
#' | eq. (9), Section 4.2: \eqn{\hat V_n}, \eqn{\hat{\sigma}} from Podolskij and Vetter (2009) | `V_n`, `iv` |
#' | eq. (10): \eqn{\mathcal X(t_j)} | `blocks$chi` |
#' | eq. (13): \eqn{A_n}, \eqn{B_n} with \eqn{\lfloor n/(kM) \rfloor} | `A_n`, `B_n` |
#' | eqs. (12) and (14): \eqn{\hat{\xi}_n} | `max_chi`, `statistic` |
#' | Theorem 1: \eqn{P(\xi \le x) = \exp(-e^{-x})} | `p_value` |
#' | Section 4.1: threshold from the \eqn{1-\alpha} Gumbel quantile | `beta_star`, `threshold`, `reject` |
#' | Appendix 8.1: drift-robust \eqn{\tilde{\mathcal X}(t_j)} | `demean = TRUE` |
#' | Section 6.2: step-down adjustment across windows | [lm_holm()] |
#'
#' @section Implementation choices:
#' * **One window per call, tick time.** \eqn{n} is the number of
#'   increments, so the window has the \eqn{n + 1} observations
#'   \eqn{t_0, \dots, t_n} of Assumption A.1. The spacing may be irregular
#'   (eq. 4); `obs_times(x)` only labels the blocks. The paper cleans its
#'   data first (Section 6.1: trades from 9:30 to 16:00 without the first
#'   trade of the day, prices with equal time stamps averaged, zero prices
#'   and returns beyond 7 standard deviations removed) and tests
#'   30-minute windows for 9:30-10:00 and hourly windows from 10:00 to
#'   16:00. These steps happen before `lm_test()`.
#' * **k** is the largest significant autocorrelation lag of the returns
#'   among lags 2 to 10 at the 5% level, with Bartlett's MA(1) band
#'   (see [lm_select_k()] for the reasoning and the alternatives), or the
#'   supplied `k`.
#' * **M must be an integer.** Definition 1 only requires
#'   \eqn{M \sim C \lfloor n/k \rfloor^{1/2}}. The default rounds down and
#'   keeps \eqn{M \ge 1}; `M_rounding` offers `"round"` and `"ceiling"`, and
#'   `M` fixes the block size. The plug-in variance uses the effective
#'   constant \eqn{C_{\mathrm{eff}} = M/\lfloor n/k \rfloor^{1/2}}, for which
#'   \eqn{\mathrm{Var}[\sqrt M \mathcal L(t_j)] \to \frac23 \sigma^2
#'   C_{\mathrm{eff}}^2 T + 2q^2} holds as in eq. (9); `C` reports the
#'   tabulated (or supplied) constant.
#' * **Blocks.** The blocks start at \eqn{t_j}, \eqn{j = 0, kM, 2kM, \dots},
#'   so \eqn{\hat P(t_j)} averages the prices at ticks
#'   \eqn{j, j + k, \dots, j + (M-1)k}. The first block starts at the first
#'   observation and has \eqn{M} prices. Only full blocks are formed: the
#'   fewer than \eqn{kM} observations after the last full block do not
#'   enter the block statistics (\eqn{\hat q} and the integrated variance
#'   use the whole window). \eqn{\mathcal L(t_j)} is the forward difference
#'   and is labelled \eqn{t_j}, as in eq. (8).
#' * **Volatility.** \eqn{\hat{\sigma}^2 T} is the integrated variance of the
#'   window. `integrated_variance(x_sub, method = iv_method)` estimates it
#'   from the \eqn{k}-subsampled log prices `x_sub`, on which the noise is
#'   independent (Remark 1), so that \eqn{\hat V_n = \frac23
#'   C_{\mathrm{eff}}^2 \mathrm{IV} + 2\hat q^2} needs no time units.
#'   `iv` replaces the estimate. A negative estimate is set to 0 with a
#'   warning.
#' * **Small M.** Eq. (9) is a limit. For equally spaced observations,
#'   constant volatility and independent noise the exact variance is
#'   \eqn{\mathrm{Var}[\sqrt M \mathcal L(t_j)] = \frac{2M^2 + 1}{3}
#'   \mathrm{IV}\, k/n + 2q^2}, while eq. (9) omits the
#'   \eqn{\mathrm{IV}\,k/(3n)} part. In finite samples, however,
#'   \eqn{2\hat q^2} from Proposition 1 exceeds \eqn{2q^2} by about
#'   \eqn{\mathrm{IV}\,k/n}, the efficient-price variance over \eqn{k}
#'   ticks, so the plug-in overstates the variance slightly (by about
#'   \eqn{\frac23 \mathrm{IV}\,k/n}) and the test errs on the conservative
#'   side, even for \eqn{M = 1}.
#' * **Theorem 1** uses \eqn{\lfloor n/(kM) \rfloor} in \eqn{A_n} and
#'   \eqn{B_n}, as in eq. (13). The number of differences in the maximum,
#'   `n_blocks`, is \eqn{\lfloor (\lfloor n/k \rfloor + 1)/M \rfloor - 1},
#'   which differs from it by at most one. The Gumbel limit of the maximum
#'   already accounts for the number of blocks, so no further adjustment
#'   is made within a window; across windows use [lm_holm()].
#' * **Drift.** The main statistic assumes zero drift (Section 2);
#'   `demean = TRUE` applies Appendix 8.1, with \eqn{\hat{\mu}_{\mathcal L}}
#'   computed with the denominator \eqn{\lfloor n/(kM) \rfloor} as printed.
#' * **Empirical variance.** `variance = "empirical"` is not in the paper.
#'   A jump inflates the sample variance of \eqn{\sqrt M \mathcal L(t_j)}
#'   and can mask itself.
#'
#' @references
#' Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and
#' market microstructure noise. *Journal of Econometrics* 168(2), 396-406.
#' \doi{10.1016/j.jeconom.2012.03.001}
#'
#' Podolskij, M. and Vetter, M. (2009). Estimation of volatility functionals
#' in the simultaneous presence of microstructure noise and jumps.
#' *Bernoulli* 15(3), 634-658.
#' @seealso [lm_options()], [lm_blocks()], [lm_holm()]
#' @examples
#' set.seed(1)
#' n <- 23400
#' y <- log(100) + cumsum(c(0, rnorm(n, 0, sqrt(1e-4 / n)))) + rnorm(n + 1, 0, 1e-4)
#' y[15001:(n + 1)] <- y[15001:(n + 1)] + 0.005 # a 0.5% jump
#' res <- lm_test(y, lm_options(iv = 1e-4))     # true integrated variance
#' res
#' subset(lm_blocks(res), jump != 0)
#' @export
lm_test <- function(x, options = lm_options()) {
  y <- log_prices(x)
  check_class(options, LMOptions, "options")
  times <- obs_times(x)
  n <- length(y) - 1L
  r <- diff(y)
  if (all(r == r[1L])) hf_abort("The log returns of `x` are constant; there is nothing to test.")

  # Remark 1, Sections 4.2 and 6.2, Fig. 1: order of the noise dependence.
  if (is.null(options@k)) {
    rule <- lm_acf_rule(r, options@acf_level, options@acf_max_lag, options@acf_band)
    k <- rule$k
    acf <- rule$acf
    band <- rule$band
  } else {
    k <- options@k
    if (k >= n) hf_abort("`k` = %d must be smaller than the number of increments, %d.", k, n)
    acf <- numeric()
    band <- NA_real_
  }

  # Proposition 1, eqs. (16)-(17).
  q_hat <- lm_q_hat(y, k)

  # Definition 1, eq. (7), and Table 5.
  nk <- n %/% k
  if (!is.null(options@M)) {
    M <- options@M
    C <- M / sqrt(nk)
  } else {
    C <- if (is.null(options@C)) lm_select_C(q_hat, options@C_lookup) else options@C
    raw <- C * sqrt(nk)
    M <- switch(options@M_rounding,
                floor = floor(raw + 1e-9), round = round(raw),
                ceiling = ceiling(raw - 1e-9))
    M <- max(1L, as.integer(M))
  }
  C_eff <- M / sqrt(nk)
  N <- n %/% (k * M)
  if (N < 2L) {
    hf_abort("The window is too short for k = %d and M = %d: floor(n / (kM)) = %d, but Theorem 1 needs at least 2.",
             k, M, N)
  }

  # Definition 1 and eq. (8): full blocks only, forward differences.
  b <- lm_block_means(y, k, M)
  n_L <- length(b$P_hat) - 1L
  L <- diff(b$P_hat)
  j <- b$index[seq_len(n_L)]

  # Section 4.2, eq. (9).
  if (options@variance == "plugin") {
    iv <- lm_window_iv(x, y, n, k, options)
    iv_value <- iv@value
    if (iv_value < 0) {
      hf_warn("The integrated variance estimate is negative (%.3g); V_n uses 0 instead.",
              iv_value)
      iv_value <- 0
    }
    V_n <- 2 / 3 * C_eff^2 * iv_value + 2 * q_hat^2
  } else {
    if (n_L < 2L) hf_abort("The empirical variance needs at least 2 block differences.")
    iv <- NULL
    V_n <- M * stats::var(L)
  }
  if (!is.finite(V_n) || V_n <= 0) hf_abort("V_n = %s is not positive.", format(V_n))

  # eq. (10); Appendix 8.1 for the drift-robust version.
  mu <- if (options@demean) sum(L) / N else 0
  chi <- sqrt(M) * (L - mu) / sqrt(V_n)

  # Theorem 1, eqs. (12)-(14).
  log_N <- log(N)
  c_N <- sqrt(2 * log_N)
  A_n <- c_N - (log(pi) + log(log_N)) / (2 * c_N)
  B_n <- 1 / c_N
  max_chi <- max(abs(chi))
  xi <- (max_chi - A_n) / B_n
  beta_star <- gumbel_quantile(1 - options@alpha)
  threshold <- A_n + beta_star * B_n

  blocks <- data.frame(
    index = j,
    time = times[j + 1L],
    time_end = times[j + (2L * M - 1L) * k + 1L],
    P_hat = b$P_hat[seq_len(n_L)],
    L = L,
    chi = chi
  )
  if (options@flag_blocks) blocks$jump <- as.integer(sign(chi) * (abs(chi) > threshold))

  LMTest(
    method = "lm2012", reference = "Lee and Mykland (2012)",
    n = n, k = k, M = M, C = as.double(C), C_effective = C_eff,
    n_blocks = n_L, q_hat = q_hat, iv = iv, V_n = V_n, A_n = A_n, B_n = B_n,
    max_chi = max_chi, statistic = xi, p_value = -expm1(-exp(-xi)),
    alpha = options@alpha, beta_star = beta_star, threshold = threshold,
    reject = xi > beta_star, acf = acf, acf_band = band, blocks = blocks,
    start = times[1L], end = times[n + 1L], options = options
  )
}

# Integrated variance of the window for the plug-in variance: the supplied
# value, or an estimate from the k-subsampled log prices.
lm_window_iv <- function(x, y, n, k, options) {
  iv <- options@iv
  if (S7::S7_inherits(iv, IVEstimate)) return(iv)
  if (!is.null(iv)) {
    return(IVEstimate(
      method = "user", reference = "supplied with lm_options(iv = )",
      value = iv, jump_robust = TRUE, n = n, tuning = list()
    ))
  }
  idx <- seq.int(1L, n + 1L, by = k)
  x_sub <- if (S7::S7_inherits(x, HFSeries)) {
    HFSeries(time = x@time[idx], log_price = x@log_price[idx],
             symbol = x@symbol, venue = x@venue)
  } else {
    y[idx]
  }
  f <- options@iv_method
  if (is.function(f)) {
    est <- do.call(f, c(list(x_sub), options@iv_args))
    label <- "user function"
  } else {
    iv_fun <- get0("integrated_variance", mode = "function")
    if (is.null(iv_fun)) {
      hf_abort(paste(
        "The plug-in variance needs integrated_variance(), which is not available.",
        "Supply the integrated variance with lm_options(iv = ), pass an estimator",
        "function as `iv_method`, or use `variance = \"empirical\"`."
      ))
    }
    est <- do.call(iv_fun, c(list(x_sub, method = f), options@iv_args))
    label <- f
  }
  if (is.numeric(est) && !is.object(est) && length(est) == 1L) {
    est <- IVEstimate(
      method = label, reference = "estimator supplied as `iv_method`",
      value = as.double(est), jump_robust = FALSE,
      n = length(idx) - 1L, tuning = options@iv_args
    )
  }
  if (!S7::S7_inherits(est, IVEstimate)) {
    hf_abort("The integrated-variance estimator must return an <IVEstimate> or a single number, not %s.",
             obj_type(est))
  }
  if (!is.finite(est@value)) hf_abort("The integrated-variance estimate is not finite.")
  est
}

#' Block statistics of a Lee and Mykland (2012) test
#'
#' Returns the table of block differences of an [LMTest]: one row per
#' \eqn{t_j \in \mathcal G_n^{kM}} with `index`, `time`, `time_end`,
#' `P_hat`, `L`, `chi` and, if the test was run with `flag_blocks = TRUE`,
#' the jump flag `jump`. See [LMTest] for the columns and [lm_test()] for
#' how a jump time relates to `time` and `time_end`.
#'
#' @param test An [LMTest] object.
#' @return A data frame.
#' @inherit lm_select_k references
#' @examples
#' y <- cumsum(c(0, rnorm(5000, 0, 1e-4))) + rnorm(5001, 0, 1e-4)
#' head(lm_blocks(lm_test(y, lm_options(iv = 5e-5))))
#' @export
lm_blocks <- function(test) {
  check_class(test, LMTest, "test")
  test@blocks
}

# Multiple testing ----------------------------------------------------------

#' Step-down adjustment across windows (Lee and Mykland 2012, Section 6.2)
#'
#' Controls the overall error rate \eqn{\alpha'} of the tests of \eqn{h}
#' windows with the step-down procedure of Section 6.2. With the p-values
#' ordered as \eqn{p_{O(1)} \le \dots \le p_{O(h)}}, the adjusted p-values
#' are
#' \deqn{\tilde p_{O(h^*)} = \max_{j \le h^*} (h - j + 1)\, p_{O(j)},}
#' and \eqn{H_{O(h^*)}} is rejected when \eqn{\tilde p_{O(h^*)} \le
#' \alpha'}. This is the procedure of Holm (1979).
#'
#' @param x The window tests: a list of [LMTest] objects, or a numeric vector
#'   of their p-values. Names are kept as window labels.
#' @param alpha Overall error rate \eqn{\alpha'}. The paper reports 1% and
#'   5%; the default is 1%.
#' @return A data frame with one row per window: `window` (the names of `x`,
#'   or 1, 2, ...), `p_value`, `p_adjusted` and `reject`.
#'
#' @section Paper mapping:
#' | Paper (Section 6.2, pp. 21-22) | Code |
#' |---|---|
#' | p-values \eqn{p_1, \dots, p_h} of \eqn{\hat{\xi}_{1,n}, \dots, \hat{\xi}_{h,n}} from the Gumbel law | `x` (`LMTest@p_value`) |
#' | ordering \eqn{p_{O(1)} \le \dots \le p_{O(h)}} | `order(p)` |
#' | \eqn{\tilde p_{O(h^*)} = \max_{j \le h^*} (h-j+1) p_{O(j)}} | `p_adjusted` |
#' | reject when \eqn{\tilde p_{O(h^*)} \le \alpha'} | `reject` |
#'
#' @section Implementation choices:
#' * Adjusted p-values are capped at 1, as in
#'   `stats::p.adjust(method = "holm")`. The cap does not change any
#'   decision.
#' * Missing p-values (`NA`), for example of windows that could not be
#'   tested, are not counted in \eqn{h}; their adjusted p-value and decision
#'   are `NA`.
#' * Within a window no adjustment is needed: the Gumbel limit of the
#'   maximum in Theorem 1 already accounts for the number of blocks.
#'
#' @references
#' Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and
#' market microstructure noise. *Journal of Econometrics* 168(2), 396-406.
#' \doi{10.1016/j.jeconom.2012.03.001}
#'
#' Holm, S. (1979). A simple sequentially rejective multiple test procedure.
#' *Scandinavian Journal of Statistics* 6(2), 65-70.
#' @seealso [lm_test()]
#' @examples
#' lm_holm(c(a = 0.01, b = 0.04, c = 0.03, d = 0.005), alpha = 0.05)
#' @export
lm_holm <- function(x, alpha = 0.01) {
  check_probability(alpha, "alpha")
  labels <- names(x)
  if (is.list(x) && !is.object(x)) {
    is_test <- vapply(x, function(t) S7::S7_inherits(t, LMTest), logical(1))
    if (!length(x) || !all(is_test)) {
      hf_abort("`x` must be a list of <LMTest> objects or a numeric vector of p-values.")
    }
    p <- vapply(x, function(t) t@p_value, numeric(1))
  } else {
    p <- check_numeric(x, "x", finite = FALSE)
    if (any(!is.na(p) & (p < 0 | p > 1))) hf_abort("The p-values in `x` must lie in [0, 1].")
  }
  h <- sum(!is.na(p))
  o <- order(p, na.last = NA)
  adjusted <- rep(NA_real_, length(p))
  adjusted[o] <- pmin(1, cummax((h - seq_len(h) + 1) * p[o]))
  data.frame(
    window = if (is.null(labels)) seq_along(p) else labels,
    p_value = unname(p), p_adjusted = adjusted, reject = adjusted <= alpha,
    stringsAsFactors = FALSE
  )
}
