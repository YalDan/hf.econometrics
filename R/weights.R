#' Pre-averaging weight functions
#'
#' A `WeightFunction` is a weight \eqn{g} on \eqn{[0, 1]} as required by
#' Jacod, Podolskij and Vetter (2010, eq. 2.6) and Ait-Sahalia, Jacod and Li
#' (2012, eq. 3.5): continuous and piecewise \eqn{C^1} with a piecewise
#' Lipschitz derivative, \eqn{g(s) = 0} for \eqn{s \notin (0, 1)}, and
#' \eqn{\int g^2 > 0}.
#'
#' @param name Label, a single string.
#' @param fun Vectorised \eqn{g(x)}; it must return 0 outside \eqn{[0, 1]}.
#' @param deriv Vectorised \eqn{g'(x)}, defined almost everywhere.
#' @param breakpoints Points in \eqn{[0, 1]} where \eqn{g} or \eqn{g'} has a
#'   kink or jump. They are used for accurate numerical integration.
#' @seealso [weight_tent()], [weight_compress()], [weight_integral()]
#' @export
WeightFunction <- new_class(
  "WeightFunction",
  properties = list(
    name = class_character,
    fun = class_function,
    deriv = class_function,
    breakpoints = class_double
  ),
  validator = function(self) {
    collect_msgs(
      if (length(self@name) != 1L) "@name must be a single string.",
      if (is.unsorted(self@breakpoints) || any(self@breakpoints < 0 | self@breakpoints > 1)) {
        "@breakpoints must be sorted and lie in [0, 1]."
      }
    )
  }
)

#' The tent weight \eqn{g(x) = \min(x, 1 - x)}
#'
#' The default weight of Jacod et al. (2009, eq. 3.11), Jacod, Podolskij and
#' Vetter (2010) and Ait-Sahalia, Jacod and Li (2012, Section 4):
#' \eqn{g(x) = (0.5 - |x - 0.5|)^+}. Then \eqn{\bar g(2) = \int g^2 = 1/12}
#' and \eqn{\bar g'(2) = \int (g')^2 = 1}.
#'
#' @return A [WeightFunction].
#' @examples
#' g <- weight_tent()
#' weight_integral(g, 2) # 1/12
#' @export
weight_tent <- function() {
  WeightFunction(
    name = "tent",
    fun = function(x) pmax(0, pmin(x, 1 - x)),
    deriv = function(x) ifelse(x > 0 & x < 0.5, 1, ifelse(x >= 0.5 & x < 1, -1, 0)),
    breakpoints = c(0, 0.5, 1)
  )
}

#' Compressed weight \eqn{h(x) = g(\gamma x)}
#'
#' Ait-Sahalia, Jacod and Li (2012, Section 3.3) build the second weight of
#' their test as \eqn{h(s) = g(\gamma s)} for some \eqn{\gamma > 1}. It is
#' supported on \eqn{[0, 1/\gamma]}, and \eqn{\bar h(q) = \bar g(q) / \gamma}.
#'
#' @param g A [WeightFunction].
#' @param gamma Compression factor, at least 1.
#' @return A [WeightFunction].
#' @examples
#' h <- weight_compress(weight_tent(), 2)
#' weight_integral(h, 2) # 1/24
#' @export
weight_compress <- function(g, gamma) {
  check_class(g, WeightFunction, "g")
  check_number(gamma, "gamma", lower = 1)
  f <- g@fun
  d <- g@deriv
  WeightFunction(
    name = sprintf("%s(%sx)", g@name, format(gamma)),
    fun = function(x) f(gamma * x),
    deriv = function(x) gamma * d(gamma * x),
    breakpoints = sort(unique(c(0, g@breakpoints / gamma, 1)))
  )
}

#' Integrals of powers of a weight function
#'
#' Computes \eqn{\bar g(p) = \int_0^1 |g(s)|^p ds}, or
#' \eqn{\bar g'(p) = \int_0^1 |g'(s)|^p ds} when `derivative = TRUE`
#' (Ait-Sahalia, Jacod and Li 2012, eq. 3.6).
#'
#' @param g A [WeightFunction].
#' @param p Power, a positive number.
#' @param derivative Integrate the derivative instead of the function?
#' @return A single number.
#' @export
weight_integral <- function(g, p, derivative = FALSE) {
  check_class(g, WeightFunction, "g")
  check_number(p, "p", lower = 0, open = c(TRUE, FALSE))
  check_flag(derivative, "derivative")
  f <- if (derivative) g@deriv else g@fun
  integrate_piecewise(function(s) abs(f(s))^p, 0, 1, g@breakpoints)
}

# Discrete weights g_j = g(j / kn), j = 0..kn, and squared differences
# (g'_j)^2 = (g_j - g_{j-1})^2, j = 1..kn (Ait-Sahalia, Jacod and Li 2012, eq. 3.6).
discrete_weights <- function(weight, kn) {
  gj <- weight@fun((0:kn) / kn)
  list(g = gj[-1L], dg2 = diff(gj)^2)
}
