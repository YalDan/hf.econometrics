# Building blocks of the Lee and Mykland (2012) test: selection of the noise
# dependence k, the noise estimator of Proposition 1, Table 5 and the block
# averages of Definition 1. The test itself is in lm2012.R.

# Table 5 of Lee and Mykland (2012, p. 41): optimal C in M ~ C floor(n/k)^(1/2)
# as a function of the noise standard deviation q, given in percent.
lm_table5 <- data.frame(
  q_percent = c(0.01, 0.03, 0.05, 0.07, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                0.7, 0.8, 0.9, 1.0),
  C = 1 / c(19, 19, 19, 18, 18, 18, 16, 16, 9, 9, 9, 9, 9, 8, 8)
)

# match.arg()-style choice that raises `hf_input_error`: the full default
# vector selects its first element.
lm_choice <- function(x, choices, arg, abort = TRUE) {
  if (identical(x, choices)) x <- choices[1L]
  msg <- check_choice(x, choices, arg, abort = FALSE)
  if (!abort) return(msg)
  if (!is.null(msg)) hf_abort(msg)
  x
}

#' Select the noise dependence k of Lee and Mykland (2012)
#'
#' Determines \eqn{k} from the sample autocorrelation of the observed log
#' returns at the highest frequency: \eqn{k} is the largest lag
#' \eqn{l \in \{2, \dots, L\}} whose autocorrelation lies outside the
#' confidence band, and \eqn{k = 1} if there is none. The noise is then
#' treated as \eqn{(k - 1)}-dependent (Remark 1) and every \eqn{k}-th
#' observation is used.
#'
#' **From significant lags to k.** Under \eqn{(k-1)}-dependent noise \eqn{U}
#' (Assumption A.3, Remark 1), the observed returns
#' \eqn{\Delta \tilde P_i = \Delta P_i + U_i - U_{i-1}} have autocovariance
#' \eqn{2\gamma_U(l) - \gamma_U(l-1) - \gamma_U(l+1)} at lag \eqn{l \ge 1}.
#' It vanishes for \eqn{l > k} and equals \eqn{-\gamma_U(k-1) \ne 0} at
#' \eqn{l = k}: the returns are MA(\eqn{k}). The largest significant return
#' lag therefore estimates \eqn{k}, the paper's "significant lag number"
#' (Fig. 1 legend), and \eqn{k - 1} is the order of dependence of the noise
#' (Section 6.2). With i.i.d. noise only lag 1 is significant, so
#' \eqn{k = 1}; with MA(9) noise the largest significant lag is 10, so
#' \eqn{k = 10}. Any noise makes lag 1 significant, so lag 1 never changes
#' \eqn{k}; this is why the paper draws the band only for lags 2 and beyond.
#'
#' @param x Log prices of one window: a numeric vector or an [HFSeries].
#' @param level Significance level of the test of each autocorrelation.
#'   The paper uses the "usual" 5% level.
#' @param max_lag Largest lag \eqn{L} examined, at least 2. Fig. 1 of the
#'   paper shows lags 1 to 10.
#' @param band Standard error behind the band. `"bartlett"` (default) uses
#'   Bartlett's formula under the hypothesis that the returns are MA(1),
#'   \eqn{\sqrt{(1 + 2\hat{\rho}_1^2)/N}}, the band that is drawn only for lags
#'   2 and beyond (Fig. 1). `"white"` uses the white-noise standard error
#'   \eqn{1/\sqrt{N}}.
#' @return The selected \eqn{k}, a single integer.
#'
#' @section Paper mapping:
#' | Paper | Code |
#' |---|---|
#' | Remark 1: \eqn{(k-1)}-dependent noise, subsample every \eqn{k}-th observation | returned `k` |
#' | Section 4.2 (p. 13): serial correlation of the highest-frequency returns, "usual significance test" | `stats::acf()` of `diff(log_prices(x))` |
#' | Section 6.2 (p. 20): significance test at 5% "for every horizon" | `level = 0.05`; called once per window |
#' | Fig. 1 (p. 34): lags 1 to 10, 95% band "for lags of 2 and beyond" | `max_lag = 10`, lags `2:max_lag`, `band = "bartlett"` |
#'
#' @section Implementation choices:
#' * The band is \eqn{\pm z_{1-\mathrm{level}/2}} standard errors. Fig. 1
#'   has the conventions of MATLAB's `autocorr(r, 10, 1)`, which draws the
#'   Bartlett band of an MA(1) process for lags greater than 1 and uses
#'   2 standard errors by default; `level = 2 * pnorm(-2)` (about 0.0455)
#'   reproduces that width exactly. Under i.i.d. noise the returns are
#'   MA(1), so this band has the nominal level at every lag tested. The
#'   white-noise band is available as `band = "white"`; it is too narrow
#'   when the lag-1 autocorrelation is large.
#' * k is the *largest* significant lag, not the number of significant
#'   lags. Both readings agree when the significant lags are contiguous, but
#'   only the largest lag recovers k when the dependence shows up at lag k
#'   alone, as it does for equally weighted MA noise.
#' * Each of the `max_lag - 1` lags is tested at `level` without a
#'   multiplicity correction, as in the paper. With i.i.d. noise a spurious
#'   lag therefore raises k on roughly a third of the windows at the
#'   default settings (Table 1 of the paper also reports an average k above
#'   the truth without jumps). A k that is too large keeps the test valid,
#'   because the noise stays independent after subsampling, but uses fewer
#'   observations. Fix `k` in [lm_options()] to override the rule.
#' * The autocorrelations are the usual demeaned sample autocorrelations
#'   with denominator \eqn{N}, the number of returns in the window. The
#'   returns are taken in tick time, whatever the spacing of `obs_times(x)`.
#'
#' @references
#' Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and
#' market microstructure noise. *Journal of Econometrics* 168(2), 396-406.
#' \doi{10.1016/j.jeconom.2012.03.001}
#' @seealso [lm_test()], [lm_options()]
#' @examples
#' set.seed(4)
#' n <- 23400
#' p <- cumsum(c(0, rnorm(n, 0, 0.01 / sqrt(n)))) # efficient log price
#' lm_select_k(p + rnorm(n + 1, 0, 2e-4))          # i.i.d. noise: k = 1
#' e <- rnorm(n + 10)
#' u <- stats::filter(e, rep(1, 10), sides = 1)[-(1:9)] * 2e-4 / sqrt(10)
#' lm_select_k(p + u)                              # MA(9) noise: k = 10
#' @export
lm_select_k <- function(x, level = 0.05, max_lag = 10L,
                        band = c("bartlett", "white")) {
  y <- log_prices(x)
  check_probability(level, "level")
  max_lag <- check_count(max_lag, "max_lag", min = 2L)
  band <- lm_choice(band, c("bartlett", "white"), "band")
  lm_acf_rule(diff(y), level, max_lag, band)$k
}

# The lag rule on returns `r`; also returns the autocorrelations at lags
# 1..max_lag, the half-width of the band and the significant lags.
lm_acf_rule <- function(r, level, max_lag, band) {
  N <- length(r)
  if (N <= max_lag) {
    hf_abort("The window has %d returns; the lag rule needs more than `acf_max_lag` = %d.",
             N, max_lag)
  }
  if (all(r == r[1L])) hf_abort("The log returns are constant; the lag rule is undefined.")
  rho <- as.double(stats::acf(r, lag.max = max_lag, plot = FALSE,
                              demean = TRUE)$acf)[-1L]
  se <- if (band == "bartlett") sqrt((1 + 2 * rho[1L]^2) / N) else 1 / sqrt(N)
  width <- stats::qnorm(1 - level / 2) * se
  lags <- 2:max_lag
  significant <- lags[abs(rho[lags]) > width]
  list(k = max(1L, significant), acf = rho, band = width,
       significant = significant)
}

#' Noise standard deviation of Lee and Mykland (2012, Proposition 1)
#'
#' Computes
#' \deqn{\widehat{Q}(k) = \Big(\frac{1}{n'} \sum_{m=1}^{n'}
#'   \big(\tilde P(t_m) - \tilde P(t_{m+k})\big)^2\Big)^{1/2}, \qquad
#'   n' = n - k,}
#' (eq. 16) and returns \eqn{\hat q = \widehat Q(k)/\sqrt 2}, which is
#' consistent for the noise standard deviation \eqn{q} under
#' \eqn{(k-1)}-dependent noise, with or without jumps (eq. 17).
#'
#' @param x Log prices of one window: a numeric vector or an [HFSeries].
#'   Observation \eqn{i} is \eqn{\tilde P(t_i)}, \eqn{i = 0, \dots, n}.
#' @param k Lag, at least 1 and less than \eqn{n}; by default selected with
#'   [lm_select_k()].
#' @return \eqn{\hat q}, in log-price units (Table 5 of the paper quotes
#'   \eqn{q} in percent: `1e-4` is 0.01%).
#'
#' @section Paper mapping:
#' | Paper | Code |
#' |---|---|
#' | Proposition 1, eq. (16) | `sum((y[m + k] - y[m])^2) / (n - k)`, `m = 1, ..., n - k` |
#' | eq. (17) and \eqn{\hat q = \widehat Q(k)/\sqrt 2} | returned value |
#'
#' @section Implementation choices:
#' * The grid is \eqn{t_0, \dots, t_n} (Assumption A.1), so the sum runs
#'   over \eqn{m = 1, \dots, n - k} as printed and the pair
#'   \eqn{(t_0, t_k)} is not used. The effect is of order \eqn{1/n}.
#' * The differences are taken on the full grid at lag \eqn{k}, not on the
#'   subsampled grid. In finite samples \eqn{\widehat Q(k)^2} also contains
#'   the efficient-price variance over \eqn{k} ticks, which the paper's
#'   limit (17) neglects.
#'
#' @inherit lm_select_k references
#' @seealso [lm_test()]
#' @examples
#' y <- cumsum(rnorm(20001, 0, 1e-5)) + rnorm(20001, 0, 2e-4)
#' lm_noise_sd(y, k = 1) # about 2e-4
#' @export
lm_noise_sd <- function(x, k = lm_select_k(x)) {
  y <- log_prices(x)
  n <- length(y) - 1L
  k <- check_count(k, "k", min = 1L, max = n - 1L)
  lm_q_hat(y, k)
}

lm_q_hat <- function(y, k) {
  n <- length(y) - 1L
  m <- seq_len(n - k)
  d <- y[m + k + 1L] - y[m + 1L]
  sqrt(sum(d^2) / (n - k) / 2)
}

#' Block-size constant C from Table 5 of Lee and Mykland (2012)
#'
#' Returns the constant \eqn{C} in \eqn{M \sim C \lfloor n/k \rfloor^{1/2}}
#' (Definition 1, eq. 7) that Table 5 of the paper recommends for a noise
#' standard deviation `q_hat`:
#'
#' | \eqn{q} (%) | 0.01 | 0.03 | 0.05 | 0.07 | 0.09 | 0.1 | 0.2 | 0.3 | 0.4 | 0.5 | 0.6 | 0.7 | 0.8 | 0.9 | 1.0 |
#' |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
#' | \eqn{C} | 1/19 | 1/19 | 1/19 | 1/18 | 1/18 | 1/18 | 1/16 | 1/16 | 1/9 | 1/9 | 1/9 | 1/9 | 1/9 | 1/8 | 1/8 |
#'
#' @param q_hat Noise standard deviation in log-price units, for example
#'   from [lm_noise_sd()]: `5e-4` is the table's 0.05%. A vector is allowed.
#' @param method How to read the table between its rows. `"nearest"`
#'   (default) takes the row whose \eqn{q} is nearest to `q_hat`, with ties
#'   going to the smaller \eqn{q}. `"linear"` interpolates \eqn{C} linearly
#'   in \eqn{q}.
#' @return \eqn{C}, a vector of the same length as `q_hat`.
#'
#' @section Paper mapping:
#' | Paper | Code |
#' |---|---|
#' | Section 5.4 (pp. 18-19): estimate \eqn{q} by Proposition 1, then "determine \eqn{C} according to Table 5" | `lm_select_C(lm_noise_sd(x, k))` |
#' | Table 5 (p. 41) | the table above, all 15 rows |
#'
#' @section Implementation choices:
#' * The paper does not say how to read the table between its rows. The
#'   default takes the nearest row on the linear \eqn{q} scale, so the
#'   switching points are the midpoints 0.06%, 0.15%, 0.35% and 0.85% at
#'   which the tabulated \eqn{C} changes. `method = "linear"` interpolates.
#' * Outside the table \eqn{C} is held at the value of the nearest end:
#'   1/19 below 0.01% (the table is flat from 0.01% to 0.05%) and 1/8 above
#'   1%. Above 1% a warning of class `hf_warning` is raised, because the
#'   table gives no guidance there and such noise usually signals a data
#'   problem.
#'
#' @inherit lm_select_k references
#' @seealso [lm_test()], [lm_noise_sd()]
#' @examples
#' lm_select_C(c(1e-4, 6e-4, 2e-3, 5e-3))
#' lm_select_C(3.5e-3, method = "linear")
#' @export
lm_select_C <- function(q_hat, method = c("nearest", "linear")) {
  q_hat <- check_numeric(q_hat, "q_hat")
  if (any(q_hat < 0)) hf_abort("`q_hat` must be non-negative.")
  method <- lm_choice(method, c("nearest", "linear"), "method")
  q_pct <- 100 * q_hat
  tab <- lm_table5
  top <- max(tab$q_percent)
  if (any(q_pct > top)) {
    hf_warn("q_hat = %s%% lies above Table 5 of Lee and Mykland (2012), which ends at %s%%; using C = 1/8.",
            format(max(q_pct), digits = 3), format(top))
  }
  if (method == "nearest") {
    mid <- (tab$q_percent[-1L] + tab$q_percent[-nrow(tab)]) / 2
    tab$C[findInterval(q_pct, mid, left.open = TRUE) + 1L]
  } else {
    stats::approx(tab$q_percent, tab$C, xout = q_pct, rule = 2)$y
  }
}

# Definition 1: averages P_hat(t_j) = (1/M) sum_{i = j/k}^{j/k + M - 1}
# P_tilde(t_{ik}) over the non-overlapping blocks of M subsampled prices that
# start at t_j, j = 0, kM, 2kM, ... Only full blocks are formed. `index` is
# the 0-based tick index j of each block start.
lm_block_means <- function(y, k, M) {
  ys <- y[seq.int(1L, length(y), by = k)]
  nb <- length(ys) %/% M
  list(
    P_hat = colMeans(matrix(ys[seq_len(nb * M)], nrow = M)),
    index = (seq_len(nb) - 1L) * k * M
  )
}
