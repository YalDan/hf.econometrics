test_that("A' constants follow AJL (2012) eq. 3.17", {
  k <- ajl_constants()
  a <- k@aprime
  # w = 0 reduces to p! int_0^2 a(g', g')_t^p dt (Hermite polynomials); for the
  # tent a(g', g')_t = 1 - 3|1 - t| or |1 - t| - 1, which gives 3.6 for p = 4.
  expect_equal(a[["0", "gg"]], 3.6, tolerance = 1e-12)
  # h(x) = g(2x) is a rescaled tent, so A'(h, h; w) = 2^(p - 2w - 1) A'(g, g; w).
  expect_equal(a[["0", "hh"]], 8 * a[["0", "gg"]], tolerance = 1e-12)
  expect_equal(a[, "hh"], 2^(4 - 2 * (0:4) - 1) * a[, "gg"], tolerance = 1e-10)
  expect_equal(unname(a[, "gh"]),
               c(2.7, 0.5852679, 0.1223493, 0.0077453, 0.0003479), tolerance = 1e-5)
  # Regression (0.1.0): the authors' Matlab A'(g, g) agree with eq. 3.17, but
  # their A'(h, h; 0) = 30.218 violates the scaling identity.
  authors <- ajl_constants(aprime = "authors")@aprime
  expect_equal(unname(authors[, "gg"]), unname(a[, "gg"]), tolerance = 1e-5)
  expect_gt(authors[["0", "hh"]] / a[["0", "hh"]], 1.04)
  # p = 6: 6! * 2 * ((1 + 2^-7) / 21 + 2^-7 / 7) by the same Hermite argument.
  k6 <- ajl_constants(p = 6L)
  expect_equal(k6@aprime[["0", "gg"]], 1440 * ((1 + 2^-7) / 21 + 2^-7 / 7), tolerance = 1e-12)
  expect_equal(unname(k@ratios), c(2, 2, 2), tolerance = 1e-12)
})

test_that("Psi constants match a direct integration of eq. 3.24", {
  g <- weight_tent()
  h <- weight_compress(g, 2)
  k <- ajl_constants()
  split_int <- function(f, lo, hi, br) {
    kn <- sort(unique(c(lo, hi, br[br > lo & br < hi])))
    sum(mapply(function(a, b) stats::integrate(f, a, b, rel.tol = 1e-10, abs.tol = 0)$value,
               utils::head(kn, -1), kn[-1]))
  }
  grid <- (0:8) / 8
  gam_plus <- function(w, t) {
    split_int(function(s) w@fun(s)^3 * w@fun(s + t), 0, 1 - t, c(grid, grid - t))
  }
  gam_prime_minus <- function(w, t) {
    split_int(function(s) w@fun(s)^3 * w@deriv(s - t), t, 1, c(grid, grid + t))
  }
  psi_plus_gh <- split_int(Vectorize(function(t) gam_plus(g, t) * gam_plus(h, t)), 0, 1, grid)
  psi_prime_minus_gh <- split_int(
    Vectorize(function(t) gam_prime_minus(g, t) * gam_prime_minus(h, t)), 0, 1, grid
  )
  expect_equal(k@psi[["plus", "gh"]], psi_plus_gh, tolerance = 1e-9)
  expect_equal(k@psi_prime[["minus", "gh"]], psi_prime_minus_gh, tolerance = 1e-9)
})

test_that("the no-jump test reproduces the authors' Matlab code (AJLtest1.m) with their constants", {
  n <- 3000
  dn <- 1 / (250 * 4680) # the authors measure time in years
  kn <- 20L
  y <- sim_log_prices(n, iv = 0.16 * n * dn, noise_sd = 3e-4, seed = 5)
  r <- ajl_test(y, ajl_options(kn = kn, aprime = "authors", horizon = n * dn))
  # Unvectorised transliteration of YbarYhat.m, Vvec.m, Vvec_trunc.m, Mstar.m.
  dz <- diff(y)
  pre <- function(k) {
    gj <- pmax(0, pmin(k * (0:kn) / kn, 1 - k * (0:kn) / kn))
    idx <- lapply(seq_len(n - kn + 1), function(i) i:(i + kn - 1))
    list(bar = vapply(idx, function(j) sum(gj[-1] * dz[j]), 0),
         hat = vapply(idx, function(j) sum(diff(gj)^2 * dz[j]^2), 0))
  }
  G <- pre(1)
  H <- pre(2)
  vbar4 <- function(P) sum(P$bar^4) - 3 * sum(P$bar^2 * P$hat) + 0.75 * sum(P$hat^2)
  S <- vbar4(G) / vbar4(H) / 2
  sigmabar <- sqrt((sum(G$bar^2) - sum(G$hat) / 2) / kn / (1 / 12) / (n * dn))
  un <- 5 * sqrt(kn / 12) * sigmabar * dn^0.49
  tr <- abs(G$bar) <= un
  v8 <- vapply(0:4, function(j) sum((G$bar^2 * tr)^(4 - j) * (G$hat * tr)^j), 0)
  mstar <- function(a) {
    out <- 0
    for (w in 0:4) {
      rho <- if (w == 0) 1 else preavg_rho(2 * w)
      out <- out + sqrt(dn) * kn * a[w + 1] /
        (abs_normal_moment(2 * w) * 2^(4 - w) * (1 / 12)^w) * sum(rho * v8[4 + (0:w) - w + 1])
    }
    out / dn
  }
  A <- ajl_authors_aprime()
  vhat <- (mstar(A[, 1]) - 8 * mstar(A[, 2]) + 16 * mstar(A[, 3])) / (vbar4(G) / 2)^2
  expect_equal(r@statistic, S, tolerance = 1e-12)
  expect_equal(r@truncation, un, tolerance = 1e-12)
  expect_equal(r@variance[["no_jump"]], vhat, tolerance = 1e-10)
  expect_equal(r@z[["no_jump"]], (S - 2) / dn^0.25 / sqrt(vhat), tolerance = 1e-10)
})

test_that("both variances follow eqs. 3.19-3.25 (direct loops on a tiny input, phi = g and phi = h)", {
  n <- 400L
  kn <- 6L
  knp <- 18L
  p <- 4
  dn <- 1 / n
  theta <- kn * sqrt(dn)
  y <- sim_log_prices(n, iv = 1e-4, noise_sd = 2e-4, jumps = cbind(200, 0.02), seed = 4)
  g <- weight_tent()
  h <- weight_compress(g, 2)
  # phi = g with the printed truncations; phi = h with both Remark 2 variants.
  # Integrals of the tent: g-bar(2) = 1/12, g-bar'(2) = 1, g-bar(6) = 1/448;
  # h-bar(q) = g-bar(q) / 2 and h-bar'(2) = 2.
  cases <- list(
    list(phi = g, phi2 = 1 / 12, phip2 = 1, phi6 = 1 / 448, eta_trunc = TRUE, lower = FALSE),
    list(phi = h, phi2 = 1 / 24, phip2 = 2, phi6 = 1 / 896, eta_trunc = FALSE, lower = TRUE)
  )
  for (cs in cases) {
    r <- ajl_test(y, ajl_options(kn = kn, kn_prime = knp, null = "both", phi = cs$phi,
                                 eta_truncation = cs$eta_trunc, lower_truncation = cs$lower))
    un <- r@truncation
    pa <- preaverage(y, kn, cs$phi)
    zb <- function(i) pa@ybar[i + 1]
    zh <- function(i) pa@yhat[i + 1]
    # Eqs. 3.18-3.20.
    vstar <- function(q, s) sum(abs(pa@ybar)^q * pa@yhat^s * (abs(pa@ybar) <= un))
    a <- r@constants@aprime
    mstar <- vapply(1:3, function(col) {
      out <- 0
      for (w in 0:p) {
        rho <- if (w == 0) 1 else preavg_rho(2 * w)
        inner <- sum(vapply(0:w, function(l) rho[l + 1] * vstar(2 * w - 2 * l, p + l - w), 0))
        out <- out + theta * a[w + 1, col] /
          (abs_normal_moment(2 * w) * 2^(p - w) * cs$phi2^w * cs$phip2^(p - w)) * inner
      }
      dn^(1 - p / 2) * out
    }, 0)
    vbar_g <- preavg_vbar(preaverage(y, kn, g), 4)
    sigma_c <- (mstar[1] - 2 * 2^2 * mstar[2] + 2^4 * mstar[3]) / (vbar_g / 2)^2
    expect_equal(r@variance[["no_jump"]], sigma_c, tolerance = 1e-10)
    # Eqs. 3.22-3.25.
    eta <- function(i, m) {
      s <- 0
      for (j in 1:knp) {
        if (!cs$eta_trunc || abs(zb(i + j)) <= un) {
          s <- s + if (m == 0) zb(i + j)^2 - zh(i + j) / 2 else zh(i + j)
        }
      }
      s / (kn * knp * dn)
    }
    pw <- function(i) zb(i)^(2 * p - 2) * if (cs$lower) abs(zb(i)) > un else 1
    n_minus <- function(m) {
      sum(vapply((kn + knp):(n - kn), function(i) pw(i) * eta(i - kn - knp, m), 0)) / kn
    }
    n_plus <- function(m) {
      sum(vapply(0:(n - 2 * kn - knp + 1), function(i) pw(i) * eta(i + kn - 1, m), 0)) / kn
    }
    N <- c(n_minus(0), n_plus(0), n_minus(1), n_plus(1))
    expect_equal(unname(r@n_jump), N, tolerance = 1e-12)
    psi <- r@constants@psi[, "combined"]
    psip <- r@constants@psi_prime[, "combined"]
    sigma_j <- theta * p^2 * kn^2 / vbar_g^2 *
      ((psi[["minus"]] * N[1] + psi[["plus"]] * N[2]) / (cs$phi2 * cs$phi6) +
         (psip[["minus"]] * N[3] + psip[["plus"]] * N[4]) / (2 * cs$phip2 * cs$phi6))
    expect_equal(r@variance[["jump"]], sigma_j, tolerance = 1e-10)
  }
})

test_that("S_RJ tends to 2 without jumps and to 1 with jumps; the tests are one-sided", {
  n <- 23400
  opts <- ajl_options(null = "both")
  cont <- ajl_test(sim_log_prices(n, iv = 0.0032, noise_sd = 5e-4, seed = 1), opts)
  jump <- ajl_test(sim_log_prices(n, iv = 0.0032, noise_sd = 5e-4, seed = 1,
                                  jumps = cbind(n / 2, 0.05)), opts)
  # (S_RJ - limit) / std_error is asymptotically N(0, 1) under each null.
  expect_lt(abs(cont@statistic - 2), 3 * cont@std_error[["no_jump"]])
  expect_lt(abs(jump@statistic - 1), 3 * jump@std_error[["jump"]])
  expect_identical(cont@reject, c(no_jump = FALSE, jump = TRUE))
  expect_identical(jump@reject, c(no_jump = TRUE, jump = FALSE))
  # Regression (0.1.0 was two-sided): eq. 3.26 rejects only in the lower tail,
  # so a continuous path with S_RJ far above 2 is not a rejection.
  upper <- ajl_test(sim_log_prices(n, iv = 0.0032, noise_sd = 5e-4, seed = 2), opts)
  expect_gt(upper@z[["no_jump"]], stats::qnorm(0.975))
  expect_false(upper@reject[["no_jump"]])
  expect_equal(upper@p_value[["no_jump"]], stats::pnorm(upper@z[["no_jump"]]))
  expect_equal(jump@p_value[["jump"]], stats::pnorm(jump@z[["jump"]], lower.tail = FALSE))
  df <- as.data.frame(jump)
  expect_identical(df$null, c("no_jump", "jump"))
  expect_identical(nrow(as.data.frame(ajl_test(sim_log_prices(n, seed = 3)))), 1L)
})

test_that("invalid options and inputs raise hf_input_error", {
  expect_error(ajl_options(p = 3), class = "hf_input_error")
  expect_error(ajl_options(p = 2), class = "hf_input_error")
  expect_error(ajl_options(gamma = 1), class = "hf_input_error")
  expect_error(ajl_options(kn = 100, kn_prime = 50), class = "hf_input_error")
  expect_error(ajl_options(null = "two_sided"), class = "hf_input_error")
  expect_error(ajl_options(alpha = 1.5), class = "hf_input_error")
  expect_error(ajl_options(trunc_exponent = 0.25), class = "hf_input_error")
  expect_error(ajl_options(trunc_C = 0), class = "hf_input_error")
  expect_error(ajl_options(horizon = -1), class = "hf_input_error")
  expect_error(ajl_options(p = 6, aprime = "authors"), class = "hf_input_error")
  expect_error(ajl_options(trunc_C = Inf, lower_truncation = TRUE), class = "hf_input_error")
  expect_error(ajl_options(phi = "g"), class = "hf_input_error")
  # The jump null needs n >= 2 kn + kn_prime = 500 increments.
  expect_error(ajl_test(sim_log_prices(499), ajl_options(null = "jump")), class = "hf_input_error")
  expect_error(ajl_test(sim_log_prices(500), list()), class = "hf_input_error")
})
