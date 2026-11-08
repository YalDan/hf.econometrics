#' @include weights.R
NULL

# Numerical building blocks ---------------------------------------------------
#
# Every constant of Ait-Sahalia, Jacod and Li (2012, eqs. 3.16, 3.17 and 3.24)
# is an integral of a polynomial in cross-correlations
#   c(f1, f2; tau) = int_0^1 f1(x) f2(x + tau) dx
# of the weight functions and their derivatives. For weights that are
# piecewise polynomial between their break points (the tent and any
# compression of it), these integrands are piecewise polynomial too. They are
# integrated with composite Gauss-Legendre rules whose pieces end at the
# break points, so the constants are exact up to rounding. Smooth weights are
# integrated to spectral accuracy.

# Gauss-Legendre rule with `n` nodes on [-1, 1] (Golub-Welsch algorithm).
ajl_gauss_legendre <- function(n) {
  k <- seq_len(n - 1L)
  off <- k / sqrt(4 * k^2 - 1)
  jac <- matrix(0, n, n)
  jac[cbind(k, k + 1L)] <- off
  jac[cbind(k + 1L, k)] <- off
  e <- eigen(jac, symmetric = TRUE)
  o <- order(e$values)
  list(x = e$values[o], w = 2 * e$vectors[1L, o]^2)
}

# Composite rule. Row k of `knots` holds the sorted break points of case k;
# the rule integrates over [knots[k, 1], knots[k, ncol(knots)]]. Nodes are
# returned with the case index varying fastest, then the piece, then the node.
ajl_composite_rule <- function(knots, rule) {
  if (is.null(dim(knots))) knots <- matrix(knots, nrow = 1L)
  left <- knots[, -ncol(knots), drop = FALSE]
  right <- knots[, -1L, drop = FALSE]
  half <- as.vector(right - left) / 2
  mid <- as.vector(right + left) / 2
  nn <- length(rule$x)
  list(
    x = rep(mid, times = nn) + rep(half, times = nn) * rep(rule$x, each = length(mid)),
    w = rep(half, times = nn) * rep(rule$w, each = length(mid))
  )
}

# c(f1, f2; tau) = int_0^1 f1(x) f2(x + tau) dx for functions vanishing
# outside [0, 1] with break points b1 and b2, vectorised over tau in [-1, 1].
ajl_xcorr <- function(f1, b1, f2, b2, tau, rule) {
  lo <- pmax(0, -tau)
  hi <- pmax(pmin(1, 1 - tau), lo)
  inner <- cbind(matrix(b1, length(tau), length(b1), byrow = TRUE),
                 outer(-tau, b2, "+"))
  inner <- pmin(pmax(inner, lo), hi)
  knots <- cbind(lo, inner, hi)
  knots <- matrix(knots[order(row(knots), knots)], nrow = length(tau),
                  byrow = TRUE)
  q <- ajl_composite_rule(knots, rule)
  shift <- rep(tau, length.out = length(q$x))
  rowSums(matrix(f1(q$x) * f2(q$x + shift) * q$w, nrow = length(tau)))
}

# a'(phi, psi; w, w')_t of eq. 3.16, vectorised over the cross-moment
# `c12` = a(phi, psi)_t; `v1` = a(phi, phi)_1 and `v2` = a(psi, psi)_1. It
# equals E[U^(2w - w') V^w'] for a centred Gaussian pair (U, V) with these
# variances and covariance.
ajl_small_aprime <- function(w, w2, v1, v2, c12, m) {
  out <- 0
  for (r in 0:(w2 %/% 2)) {
    out <- out + choose(w2, 2 * r) * m[2 * r + 1L] * m[2 * w - 2 * r + 1L] *
      v1^(w - w2) * c12^(w2 - 2 * r) * (v1 * v2 - c12^2)^r
  }
  out
}

# A'(phi, psi; w), w = 0..p, of eq. 3.17.
ajl_aprime <- function(phi, psi, p, rule_inner, rule_outer) {
  # a(phi, psi)_t = c(phi, psi; 1 - t); integrate over tau = 1 - t in [-1, 1],
  # split where a break point of phi meets one of psi.
  tau_knots <- c(-1, 1, as.vector(outer(psi@breakpoints, phi@breakpoints, "-")))
  tau_knots <- sort(unique(tau_knots[tau_knots >= -1 & tau_knots <= 1]))
  q <- ajl_composite_rule(tau_knots, rule_outer)
  bp1 <- phi@breakpoints
  bp2 <- psi@breakpoints
  c12 <- ajl_xcorr(phi@fun, bp1, psi@fun, bp2, q$x, rule_inner)       # a(g, h)_t
  d12 <- ajl_xcorr(phi@deriv, bp1, psi@deriv, bp2, q$x, rule_inner)   # a(g', h')_t
  v1 <- ajl_xcorr(phi@fun, bp1, phi@fun, bp1, 0, rule_inner)          # g-bar(2)
  v2 <- ajl_xcorr(psi@fun, bp2, psi@fun, bp2, 0, rule_inner)          # h-bar(2)
  d1 <- ajl_xcorr(phi@deriv, bp1, phi@deriv, bp1, 0, rule_inner)      # g-bar'(2)
  d2 <- ajl_xcorr(psi@deriv, bp2, psi@deriv, bp2, 0, rule_inner)      # h-bar'(2)
  m <- abs_normal_moment(0:(2 * p))
  rho <- preavg_rho(p)
  half <- p %/% 2L
  vapply(0:p, function(w) {
    a_t <- 0
    for (l in 0:half) for (l2 in 0:half) {
      if (l + l2 > p - w) next
      lo <- max(0L, 2L * w - p + 2L * l)
      hi <- min(2L * w, p - 2L * l2)
      if (lo > hi) next
      for (w2 in lo:hi) {
        a_t <- a_t + rho[l + 1L] * rho[l2 + 1L] *
          choose(p - 2 * l, 2 * w - w2) * choose(p - 2 * l2, w2) *
          (2 * d1)^l * (2 * d2)^l2 *
          ajl_small_aprime(w, w2, v1, v2, c12, m) *
          ajl_small_aprime(p - l - l2 - w, p - 2 * l2 - w2, d1, d2, d12, m)
      }
    }
    # The 1{w = p} term of eq. 3.17 is subtracted inside the integral over
    # [0, 2] (an interval of length 2), which avoids cancellation.
    if (w == p) a_t <- a_t - m[p + 1L]^2 * v1^half * v2^half
    sum(a_t * q$w)
  }, numeric(1))
}

# Gamma(+-, phi)_t and Gamma'(+-, phi)_t of eq. 3.24 at the nodes `t`.
ajl_gamma_funs <- function(phi, p, t, rule) {
  bp <- phi@breakpoints
  f <- phi@fun
  fp <- function(x) f(x)^(p - 1L)
  list(
    minus = ajl_xcorr(fp, bp, f, bp, -t, rule),
    plus = ajl_xcorr(fp, bp, f, bp, t, rule),
    minus_prime = ajl_xcorr(fp, bp, phi@deriv, bp, -t, rule),
    plus_prime = ajl_xcorr(fp, bp, phi@deriv, bp, t, rule)
  )
}

# Psi_+-(., .) and Psi'_+-(., .) of eq. 3.24 for the pairs (g, g), (g, h), (h, h).
ajl_psi <- function(g, h, p, rule_inner, rule_outer) {
  bps <- list(g@breakpoints, h@breakpoints)
  t_knots <- c(0, 1, unlist(lapply(bps, function(b) abs(outer(b, b, "-")))))
  t_knots <- sort(unique(t_knots[t_knots >= 0 & t_knots <= 1]))
  q <- ajl_composite_rule(t_knots, rule_outer)
  G <- ajl_gamma_funs(g, p, q$x, rule_inner)
  H <- ajl_gamma_funs(h, p, q$x, rule_inner)
  pair <- function(a, b) sum(a * b * q$w)
  side <- function(key) {
    c(gg = pair(G[[key]], G[[key]]), gh = pair(G[[key]], H[[key]]),
      hh = pair(H[[key]], H[[key]]))
  }
  list(
    psi = rbind(minus = side("minus"), plus = side("plus")),
    psi_prime = rbind(minus = side("minus_prime"), plus = side("plus_prime"))
  )
}

# Constants object ------------------------------------------------------------

#' Constants of the Ait-Sahalia, Jacod and Li (2012) test
#'
#' Holds the weight-function constants that enter the asymptotic variances of
#' the pre-averaged jump test of Ait-Sahalia, Jacod and Li (2012): the ratios
#' \eqn{\gamma, \gamma', \gamma''} of eq. 3.12, the variance constants
#' \eqn{A'(\cdot, \cdot; w)} of eqs. 3.16-3.17 and the constants
#' \eqn{\Psi_\pm, \Psi'_\pm} of eq. 3.24. Create it with [ajl_constants()].
#'
#' @param p The even power \eqn{p}.
#' @param g,h The two [WeightFunction]s of the test statistic.
#' @param ratios Named vector `c(gamma, gamma_prime, gamma_dprime)`:
#'   \eqn{\gamma = \bar g(2) / \bar h(2)}, \eqn{\gamma' = \bar g(p) / \bar h(p)}
#'   and \eqn{\gamma'' = \gamma^{p/2} / \gamma'} (eq. 3.12).
#' @param integrals Named vector of \eqn{\bar g(2), \bar g'(2), \bar g(p),
#'   \bar h(2), \bar h'(2), \bar h(p)} (eq. 3.6).
#' @param aprime Matrix with rows \eqn{w = 0, \dots, p} and columns `gg`, `gh`,
#'   `hh`: \eqn{A'(g, g; w)}, \eqn{A'(g, h; w)} and \eqn{A'(h, h; w)}.
#' @param aprime_source `"formula"` (eq. 3.17) or `"authors"` (the values
#'   hard-coded in the authors' Matlab code; see [ajl_constants()]).
#' @param psi,psi_prime Matrices with rows `minus`, `plus` and columns `gg`,
#'   `gh`, `hh`, `combined`: \eqn{\Psi_\pm(g, g)}, \eqn{\Psi_\pm(g, h)},
#'   \eqn{\Psi_\pm(h, h)} and \eqn{\Psi_\pm = \Psi_\pm(g, g) + \gamma'^2
#'   \Psi_\pm(h, h) - 2 \gamma' \Psi_\pm(g, h)}, and likewise for
#'   \eqn{\Psi'_\pm} (eq. 3.24).
#' @export
AJLConstants <- new_class(
  "AJLConstants",
  properties = list(
    p = class_integer,
    g = WeightFunction,
    h = WeightFunction,
    ratios = class_double,
    integrals = class_double,
    aprime = class_double,
    aprime_source = class_character,
    psi = class_double,
    psi_prime = class_double
  ),
  validator = function(self) {
    p <- self@p
    if (length(p) != 1L || is.na(p) || p < 4L || p %% 2L != 0L) {
      return("@p must be an even integer of at least 4.")
    }
    collect_msgs(
      if (!identical(names(self@ratios), c("gamma", "gamma_prime", "gamma_dprime"))) {
        "@ratios must be named gamma, gamma_prime, gamma_dprime."
      },
      if (!identical(dim(self@aprime), c(p + 1L, 3L))) "@aprime must be a (p + 1) x 3 matrix.",
      if (!identical(dim(self@psi), c(2L, 4L)) || !identical(dim(self@psi_prime), c(2L, 4L))) {
        "@psi and @psi_prime must be 2 x 4 matrices."
      },
      if (length(self@aprime_source) != 1L || !(self@aprime_source %in% c("formula", "authors"))) {
        "@aprime_source must be \"formula\" or \"authors\"."
      }
    )
  }
)

method(print, AJLConstants) <- function(x, ...) {
  cat(sprintf("<AJLConstants> p = %d, g = %s, h = %s\n", x@p, x@g@name, x@h@name))
  r <- x@ratios
  cat(sprintf("  gamma = %.6g, gamma' = %.6g, gamma'' = %.6g (eq. 3.12)\n",
              r[["gamma"]], r[["gamma_prime"]], r[["gamma_dprime"]]))
  cat(sprintf("  A'(., .; w), w = 0..%d (eq. 3.17, %s):\n", x@p,
              if (x@aprime_source == "formula") "from the formula" else "authors' Matlab values"))
  print(signif(x@aprime, 8))
  cat("  Psi and Psi' (eq. 3.24):\n")
  print(signif(rbind(Psi = x@psi[, "combined"], "Psi'" = x@psi_prime[, "combined"]), 8))
  invisible(x)
}

# The values hard-coded in AJLtest1.m of the authors' Matlab code (p = 4,
# g = tent, h = g(2x)). Their A'(g, h; .) and A'(h, h; .) differ from eq. 3.17.
ajl_authors_aprime <- function() {
  cbind(
    gg = c(3.599999960562231, 1.221424577286807, 0.370635624815616,
           0.035039285812380, 0.002315373900701),
    gh = c(2.725769745686566, 0.589334298318853, 0.122686505762394,
           0.007755996403950, 0.000347923390644),
    hh = c(30.217754077981802, 2.535050327092127, 0.188268743583475,
           0.004415379915876, 0.000072391168973)
  )
}

# Constants are memoised by a fingerprint of the weights (their values and
# derivatives on a probe grid), so the default (tent, h = g(2x), p = 4) and any
# other choice is computed once per session.
.ajl_cache <- new.env(parent = emptyenv())

ajl_weight_key <- function(w) {
  x <- c(w@breakpoints, (1:63) / 64 - 1 / (64 * pi))
  paste(c(w@name, sprintf("%.14g", c(w@breakpoints, w@fun(x), w@deriv(x)))),
        collapse = ",")
}

ajl_same_weight <- function(a, b) identical(ajl_weight_key(a), ajl_weight_key(b))

#' Compute the constants of the Ait-Sahalia, Jacod and Li (2012) test
#'
#' Computes, for arbitrary weight functions \eqn{g} and \eqn{h} and an even
#' power \eqn{p \ge 4}, the constants that enter the variance estimators of
#' Ait-Sahalia, Jacod and Li (2012): \eqn{\gamma, \gamma', \gamma''}
#' (eq. 3.12), \eqn{A'(g, g; w)}, \eqn{A'(g, h; w)}, \eqn{A'(h, h; w)} for
#' \eqn{w = 0, \dots, p} (eqs. 3.16-3.17), and \eqn{\Psi_\pm}, \eqn{\Psi'_\pm}
#' (eq. 3.24). Results are cached for the session.
#'
#' @section Paper mapping:
#' * Eq. 3.12: `ratios`.
#' * Eq. 3.16: \eqn{a(\phi, \psi)_t = \int_{1 \vee t}^{1 + 1 \wedge t}
#'   \phi(u - 1) \psi(u - t) du} is the cross-correlation
#'   \eqn{\int_0^1 \phi(x) \psi(x + 1 - t) dx}, evaluated for
#'   \eqn{(\phi, \psi) = (g, h)} and \eqn{(g', h')} at the quadrature nodes in
#'   \eqn{t \in [0, 2]}; \eqn{a'(\phi, \psi; w, w')_t} is the polynomial in
#'   those values given in eq. 3.16.
#' * Eq. 3.17: \eqn{A(g, h; w)_t} is summed over \eqn{l, l'} and \eqn{w'} as
#'   printed, integrated over \eqn{t \in [0, 2]}, and the term
#'   \eqn{2 m_p^2 \bar g(2)^{p/2} \bar h(2)^{p/2}} is subtracted for
#'   \eqn{w = p}. Column `gg` of `aprime` uses \eqn{(g, g)}, `gh` uses
#'   \eqn{(g, h)} and `hh` uses \eqn{(h, h)}.
#' * Eq. 3.24: \eqn{\Gamma(\pm, \phi)_t} and \eqn{\Gamma'(\pm, \phi)_t} for
#'   \eqn{\phi = g, h}, \eqn{t \in [0, 1]}; then \eqn{\Psi_\pm(\cdot, \cdot)},
#'   \eqn{\Psi'_\pm(\cdot, \cdot)} and the combinations \eqn{\Psi_\pm},
#'   \eqn{\Psi'_\pm} (column `combined`).
#'
#' @section Implementation choices:
#' * **Quadrature.** The integrands are piecewise polynomial for piecewise
#'   polynomial weights, with kinks where a break point of one weight meets a
#'   break point of the other. Each integral is split at those points and
#'   computed with a Gauss-Legendre rule of \eqn{2p + 16} nodes per piece
#'   (inner cross-correlations: \eqn{p + 16} nodes). For the tent this is
#'   exact up to rounding, e.g. \eqn{A'(g, g; 0) = 3.6}. The break points
#'   declared in the [WeightFunction]s must therefore be complete.
#' * **Checks.** For \eqn{h(x) = g(\gamma x)} and any \eqn{g},
#'   \eqn{\bar V(Z, h, p)} with window \eqn{k_n} is \eqn{\bar V(Z, g, p)}
#'   with window \eqn{k_n / \gamma}, which implies
#'   \eqn{A'(h, h; w) = \gamma^{p - 2w - 1} A'(g, g; w)}; for the tent and
#'   \eqn{\gamma = 2}, \eqn{A'(h, h; 0) = 8 \times 3.6 = 28.8}. The computed
#'   constants satisfy this to rounding error, and they agree with an
#'   independent evaluation of the definition of the asymptotic variance
#'   (AJL 2012, eq. A.4) by Wick's formula.
#' * **Authors' constants.** `aprime = "authors"` replaces \eqn{A'} by the
#'   values hard-coded in the authors' Matlab code (`AJLtest1.m`), available
#'   only for \eqn{p = 4}, the tent and \eqn{h = g(2x)}. Their
#'   \eqn{A'(g, g; \cdot)} agree with eq. 3.17 to about \eqn{10^{-5}}, but
#'   their \eqn{A'(g, h; \cdot)} (2.7258, 0.5893, 0.1227, 0.0078, 0.000348)
#'   and \eqn{A'(h, h; \cdot)} (30.2178, 2.5351, 0.1883, 0.0044, 0.0000724)
#'   do not: eq. 3.17 gives 2.7, 0.58527, 0.12235, 0.0077453, 0.00034790 and
#'   28.8, 2.44286, 0.18532, 0.0043799, 0.000072356, and the scaling identity
#'   above shows that the authors' \eqn{A'(h, h; 0) = 30.218} cannot be the
#'   asymptotic variance constant. The option exists only to reproduce
#'   results computed with the authors' code; the default is the formula.
#'
#' @param g,h [WeightFunction]s. The default `h` is \eqn{g(2x)}, the choice of
#'   Ait-Sahalia, Jacod and Li (2012, Section 4).
#' @param p Even integer power, at least 4.
#' @param aprime `"formula"` (default) computes \eqn{A'} from eq. 3.17;
#'   `"authors"` uses the authors' hard-coded Matlab values.
#' @return An [AJLConstants] object.
#' @references
#' Ait-Sahalia, Y., Jacod, J. and Li, J. (2012). Testing for jumps in noisy
#' high frequency data. *Journal of Econometrics* 168(2), 207-222.
#' @examples
#' k <- ajl_constants()
#' k@aprime
#' k@psi
#' @export
ajl_constants <- function(g = weight_tent(), h = weight_compress(g, 2), p = 4L,
                          aprime = "formula") {
  check_class(g, WeightFunction, "g")
  check_class(h, WeightFunction, "h")
  p <- check_count(p, "p", min = 4L)
  if (p %% 2L != 0L) hf_abort("`p` must be an even integer, not %d.", p)
  check_choice(aprime, c("formula", "authors"), "aprime")
  key <- paste(ajl_weight_key(g), ajl_weight_key(h), p, sep = "#")
  out <- .ajl_cache[[key]]
  if (is.null(out)) {
    out <- ajl_compute_constants(g, h, p)
    assign(key, out, envir = .ajl_cache)
  }
  if (aprime == "authors") {
    tent <- weight_tent()
    if (p != 4L || !ajl_same_weight(g, tent) ||
        !ajl_same_weight(h, weight_compress(tent, 2))) {
      hf_abort(paste0("`aprime = \"authors\"` is only defined for p = 4, the tent ",
                      "weight g and h(x) = g(2x), the setting of the authors' code."))
    }
    a <- ajl_authors_aprime()
    dimnames(a) <- dimnames(out@aprime)
    out@aprime <- a
    out@aprime_source <- "authors"
  }
  out
}

ajl_compute_constants <- function(g, h, p) {
  rule_inner <- ajl_gauss_legendre(p + 16L)
  rule_outer <- ajl_gauss_legendre(2L * p + 16L)
  wint <- function(w, power, deriv = FALSE) {
    f <- if (deriv) w@deriv else w@fun
    ajl_xcorr(function(x) abs(f(x))^power, w@breakpoints, function(x) rep(1, length(x)),
              c(0, 1), 0, rule_inner)
  }
  integrals <- c(
    g2 = wint(g, 2), g_prime2 = wint(g, 2, TRUE), g_p = wint(g, p),
    h2 = wint(h, 2), h_prime2 = wint(h, 2, TRUE), h_p = wint(h, p)
  )
  gam <- integrals[["g2"]] / integrals[["h2"]]
  gam_p <- integrals[["g_p"]] / integrals[["h_p"]]
  ratios <- c(gamma = gam, gamma_prime = gam_p, gamma_dprime = gam^(p / 2) / gam_p)
  aprime <- cbind(
    gg = ajl_aprime(g, g, p, rule_inner, rule_outer),
    gh = ajl_aprime(g, h, p, rule_inner, rule_outer),
    hh = ajl_aprime(h, h, p, rule_inner, rule_outer)
  )
  dimnames(aprime) <- list(w = as.character(0:p), c("gg", "gh", "hh"))
  ps <- ajl_psi(g, h, p, rule_inner, rule_outer)
  combine <- function(m) {
    cbind(m, combined = m[, "gg"] + gam_p^2 * m[, "hh"] - 2 * gam_p * m[, "gh"])
  }
  AJLConstants(
    p = p, g = g, h = h, ratios = ratios, integrals = integrals,
    aprime = aprime, aprime_source = "formula",
    psi = combine(ps$psi), psi_prime = combine(ps$psi_prime)
  )
}
