#' @include weights.R
NULL

#' Pre-averaged increments
#'
#' Holds the pre-averaged returns \eqn{\bar Y(g)^n_i} and the local noise
#' measures \eqn{\hat Y(g)^n_i} of Jacod, Podolskij and Vetter (2010,
#' eq. 2.11) and Ait-Sahalia, Jacod and Li (2012, eq. 3.7), for
#' \eqn{i = 0, \dots, n - k_n}. Create it with [preaverage()].
#'
#' @param ybar Pre-averaged returns.
#' @param yhat Local noise measures.
#' @param kn Pre-averaging window.
#' @param n Number of increments of the underlying series.
#' @param weight The [WeightFunction] used.
#' @export
PreaveragedIncrements <- new_class(
  "PreaveragedIncrements",
  properties = list(
    ybar = class_double,
    yhat = class_double,
    kn = class_integer,
    n = class_integer,
    weight = WeightFunction
  ),
  validator = function(self) {
    collect_msgs(
      if (length(self@ybar) != length(self@yhat)) "@ybar and @yhat must have the same length.",
      if (length(self@kn) != 1L || length(self@n) != 1L) "@kn and @n must be single integers.",
      if (length(self@ybar) != self@n - self@kn + 1L) "@ybar must have n - kn + 1 elements."
    )
  }
)

#' Pre-average a log-price series
#'
#' Computes, for \eqn{i = 0, \dots, n - k_n},
#' \deqn{\bar Y(g)^n_i = \sum_{j=1}^{k_n - 1} g(j/k_n)\, \Delta^n_{i+j} Y, \qquad
#'       \hat Y(g)^n_i = \sum_{j=1}^{k_n} \big(g^n_j - g^n_{j-1}\big)^2
#'       (\Delta^n_{i+j} Y)^2,}
#' following Jacod, Podolskij and Vetter (2010, eq. 2.11) and Ait-Sahalia,
#' Jacod and Li (2012, eq. 3.7). Both sums are computed as linear filters, so
#' the cost is \eqn{O(n k_n)} in compiled code.
#'
#' @param x Log prices: a numeric vector or an [HFSeries]. The method assumes
#'   equally spaced observations.
#' @param kn Pre-averaging window \eqn{k_n}, an integer with
#'   \eqn{2 \le k_n \le n}.
#' @param weight A [WeightFunction]; defaults to [weight_tent()].
#' @return A [PreaveragedIncrements] object.
#' @references
#' Jacod, J., Podolskij, M. and Vetter, M. (2010). Limit theorems for moving
#' averages of discretized processes plus noise. *Annals of Statistics*
#' 38(3), 1478-1545.
#'
#' Ait-Sahalia, Y., Jacod, J. and Li, J. (2012). Testing for jumps in noisy
#' high frequency data. *Journal of Econometrics* 168(2), 207-222.
#' @examples
#' y <- cumsum(rnorm(1000, 0, 1e-3))
#' pa <- preaverage(y, kn = 20)
#' length(pa@ybar)
#' @export
preaverage <- function(x, kn, weight = weight_tent()) {
  y <- log_prices(x)
  dy <- diff(y)
  n <- length(dy)
  kn <- check_count(kn, "kn", min = 2L, max = n)
  check_class(weight, WeightFunction, "weight")
  w <- discrete_weights(weight, kn)
  ybar <- stats::filter(dy, rev(w$g[seq_len(kn - 1L)]), sides = 1L)
  yhat <- stats::filter(dy^2, rev(w$dg2), sides = 1L)
  PreaveragedIncrements(
    ybar = as.double(ybar[(kn - 1L):(n - 1L)]),
    yhat = as.double(yhat[kn:n]),
    kn = kn, n = as.integer(n), weight = weight
  )
}

#' Pre-averaged power variations
#'
#' Computes
#' \deqn{V(Y, g, q, r)^n = \sum_{i=0}^{n - k_n} |\bar Y(g)^n_i|^q \,
#'       |\hat Y(g)^n_i|^r \, 1\{|\bar Y(g)^n_i| \le u_n\},}
#' the power variation of Jacod, Podolskij and Vetter (2010, eq. 2.12) and
#' Ait-Sahalia, Jacod and Li (2012, eq. 3.8). With a finite `truncation`
#' \eqn{u_n} it becomes the truncated version \eqn{V^*} of Ait-Sahalia, Jacod
#' and Li (2012, eq. 3.18).
#'
#' @param pa A [PreaveragedIncrements] object.
#' @param q,r Non-negative powers.
#' @param truncation Truncation level \eqn{u_n} on \eqn{|\bar Y|}; `Inf`
#'   (the default) disables truncation.
#' @return A single number.
#' @export
preavg_power_variation <- function(pa, q, r = 0, truncation = Inf) {
  check_class(pa, PreaveragedIncrements, "pa")
  check_number(q, "q", lower = 0)
  check_number(r, "r", lower = 0)
  check_number(truncation, "truncation", lower = 0, open = c(TRUE, FALSE))
  keep <- abs(pa@ybar) <= truncation
  sum(abs(pa@ybar[keep])^q * abs(pa@yhat[keep])^r)
}

#' Noise-correction coefficients \eqn{\rho(p)_l}
#'
#' For an even integer \eqn{p}, returns \eqn{\rho(p)_0, \dots, \rho(p)_{p/2}}:
#' the unique solution of the triangular system of Ait-Sahalia, Jacod and Li
#' (2012, eq. 3.9), equivalently Jacod, Podolskij and Vetter (2010, eqs.
#' 3.7-3.8):
#' \deqn{\rho(p)_0 = 1, \qquad \sum_{l=0}^{j} 2^l m_{2j-2l}
#'       \binom{p - 2l}{p - 2j} \rho(p)_l = 0, \quad j = 1, \dots, p/2.}
#'
#' @param p Even integer, at least 2.
#' @return Numeric vector of length \eqn{p/2 + 1}. For example,
#'   `preavg_rho(4)` returns `c(1, -3, 0.75)` (eq. 3.10).
#' @examples
#' preavg_rho(4)
#' @export
preavg_rho <- function(p) {
  p <- check_count(p, "p", min = 2L)
  if (p %% 2L != 0L) hf_abort("`p` must be an even integer, not %d.", p)
  h <- p %/% 2L
  rho <- numeric(h + 1L)
  rho[1L] <- 1
  for (j in seq_len(h)) {
    l <- 0:(j - 1L)
    known <- sum(2^l * abs_normal_moment(2 * j - 2 * l) *
                   choose(p - 2 * l, p - 2 * j) * rho[l + 1L])
    rho[j + 1L] <- -known / (2^j * abs_normal_moment(0) * choose(p - 2 * j, p - 2 * j))
  }
  rho
}

#' Noise-corrected pre-averaged power variation
#'
#' Computes
#' \deqn{\bar V(Y, g, p)^n = \sum_{l=0}^{p/2} \rho(p)_l \, V(Y, g, p - 2l, l)^n,}
#' following Ait-Sahalia, Jacod and Li (2012, eq. 3.11) and Jacod, Podolskij
#' and Vetter (2010, eq. 3.9). Without jumps,
#' \eqn{\Delta_n^{1 - p/4} \bar V(Y, g, p)^n \to m_p (\theta \bar g(2))^{p/2}
#' \int_0^T |\sigma_s|^p ds} (JPV 2010, eq. 3.11), where
#' \eqn{\theta = k_n \sqrt{\Delta_n}}. With jumps,
#' \eqn{k_n^{-1} \bar V(Y, g, p)^n \to \bar g(p) \sum_{s \le T} |\Delta X_s|^p}
#' for \eqn{p > 2} (eq. 3.10). It is therefore not a volatility estimator.
#'
#' @inheritParams preavg_power_variation
#' @param p Even integer power, at least 2.
#' @return A single number.
#' @export
preavg_vbar <- function(pa, p, truncation = Inf) {
  rho <- preavg_rho(p)
  l <- seq_along(rho) - 1L
  sum(vapply(l, function(ll) {
    rho[ll + 1L] * preavg_power_variation(pa, p - 2 * ll, ll, truncation)
  }, numeric(1)))
}
