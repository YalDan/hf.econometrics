#' Absolute moments of the standard normal distribution
#'
#' Computes \eqn{m_r = E|U|^r} for \eqn{U \sim N(0, 1)}:
#' \deqn{m_r = \pi^{-1/2} 2^{r/2} \Gamma((r + 1) / 2),}
#' as used in Ait-Sahalia and Jacod (2009a), Ait-Sahalia, Jacod and Li (2012,
#' eq. 3.9) and Podolskij and Vetter (2009).
#'
#' @param r Numeric vector of moment orders, each greater than -1.
#' @return Numeric vector of \eqn{m_r}.
#' @examples
#' abs_normal_moment(c(0, 1, 2, 4)) # 1, sqrt(2/pi), 1, 3
#' @export
abs_normal_moment <- function(r) {
  r <- check_numeric(r, "r")
  if (any(r <= -1)) hf_abort("`r` must be greater than -1.")
  pi^(-1 / 2) * 2^(r / 2) * gamma((r + 1) / 2)
}

#' Quantile function of the standard Gumbel distribution
#'
#' Returns \eqn{\beta^* = -\log(-\log p)}, the \eqn{p}-quantile of the
#' standard Gumbel law \eqn{P(\xi \le x) = \exp(-e^{-x})} used by
#' Lee and Mykland (2012, Theorem 1).
#'
#' @param p Probabilities in (0, 1).
#' @return Numeric vector of quantiles.
#' @examples
#' gumbel_quantile(0.99)
#' @export
gumbel_quantile <- function(p) {
  p <- check_numeric(p, "p")
  if (any(p <= 0 | p >= 1)) hf_abort("`p` must lie in (0, 1).")
  -log(-log(p))
}

# Integrates a piecewise-smooth function exactly enough for the constants in
# the source papers by splitting the domain at its kinks or discontinuities.
integrate_piecewise <- function(f, lower, upper, breaks = numeric(),
                                rel_tol = 1e-10) {
  knots <- sort(unique(c(lower, breaks[breaks > lower & breaks < upper], upper)))
  total <- 0
  for (i in seq_len(length(knots) - 1L)) {
    total <- total + stats::integrate(
      f, knots[i], knots[i + 1L], rel.tol = rel_tol, abs.tol = 0,
      subdivisions = 1000L
    )$value
  }
  total
}
