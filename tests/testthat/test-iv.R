# Integrated-variance estimators. Closed forms and direct (unvectorised)
# definitions on tiny inputs, unbiasedness of the finite-sample constants by a
# small Monte Carlo, and limit behaviour with and without a jump.

test_that("RV and BV match their definitions on a tiny example (BNS 2004, eq. 3)", {
  y <- c(0, 0.01, -0.01, 0.02, 0.02) # returns 0.01, -0.02, 0.03, 0
  expect_equal(iv_realized(y)@value, 0.0014)
  expect_equal(iv_bipower(y)@value, pi / 2 * (0.01 * 0.02 + 0.02 * 0.03))
  expect_equal(iv_bipower(y, adjust = TRUE)@value, pi / 2 * 8e-4 * 4 / 3)
  expect_true(iv_bipower(y)@jump_robust)
  expect_false(iv_realized(y)@jump_robust)
})

test_that("noise estimators match PV (3.5), HP (2.17) and the HP Lemma 3.2 recursion", {
  y <- c(0, 0.01, -0.01, 0.02, 0.02)
  expect_equal(noise_variance(y, "pv2009"), 0.0014 / 8)
  expect_equal(noise_variance(y), 8e-4 / 3) # -(sum of lag-1 products) / (n - 1)
  expect_equal(noise_variance(y, "hp2013"), 8e-4 / 3)
  expect_equal(noise_variance(c(0, 0.01, 0.02, 0.03), "hp2013"), 3e-4 / 6) # fallback
  expect_equal(noise_variance(c(0, 0.01, 0.02, 0.03)), 0)                 # truncated
  # The recursion implies rho^2 = -sum_{k=1}^{q+1} k^2 gamma(k).
  set.seed(6)
  e <- rnorm(301)
  x <- e[-1] + e[-301] # MA(1) noise, rho^2 = 4
  dy <- diff(x)
  gam <- vapply(1:4, function(k) mean(dy[1:(299 - k)] * dy[(k + 1):299]), 0)
  rho2 <- -sum((1:4)^2 * gam)
  expect_gt(rho2, 0)
  expect_equal(noise_variance(x, q = 3, long_run = TRUE), rho2)
  # MA(1) noise U_i = e_i + e_{i-1}: rho(0) = 2, rho^2 = 4 (pure noise, n = 1e5).
  e <- rnorm(100001)
  u <- e[-1] + e[-100001]
  expect_equal(noise_variance(u, q = 1), 2, tolerance = 0.05)
  expect_equal(noise_variance(u, q = 1, long_run = TRUE), 4, tolerance = 0.1)
})

test_that("pre-averaging constants match JLMPV (2009, eqs. 3.12 and Remark 2)", {
  g <- weight_tent()
  expect_equal(unname(preavg_psi_limit(g)), c(1, 1 / 12))
  expect_equal(unname(preavg_phi_limit(g)), c(1 / 6, 1 / 96, 151 / 80640),
               tolerance = 1e-8)
  expect_equal(unname(preavg_phi(g, 2000L)), c(1 / 6, 1 / 96, 151 / 80640),
               tolerance = 1e-5)
  expect_equal(preavg_optimal_theta(g, alpha = 1, iv = 1), 4.777, tolerance = 1e-3)
})

test_that("JLMPV (Sec. 4.2) and HP (Secs. 2.3, 4.1) match a direct implementation", {
  set.seed(9)
  n <- 30L
  kn <- 4L
  y <- cumsum(c(0, rnorm(n, 0, 0.01))) + rnorm(n + 1, 0, 0.005)
  dz <- diff(y)
  gj <- pmin((0:kn) / kn, 1 - (0:kn) / kn)
  zbar <- vapply(0:(n - kn + 1), function(i) sum(gj[2:kn] * dz[i + 1:(kn - 1)]), 0)
  psi1 <- kn * sum(diff(gj)^2) # j = 0..kn-1
  psi2 <- sum(gj[2:kn]^2) / kn
  dn <- 1 / n
  theta <- kn * sqrt(dn)
  a <- 1 - psi1 * dn / (2 * theta^2 * psi2)
  rv <- sum(dz^2)
  jlmpv <- (n * sqrt(dn) / ((n - kn + 2) * theta * psi2) * sum(zbar^2) -
              psi1 * dn / (2 * theta^2 * psi2) * rv) / a
  expect_equal(iv_jlmpv2009(y, kn = kn)@value, jlmpv)
  hp_c <- (n * sqrt(dn) / ((n - kn + 1) * theta * psi2) * sum(zbar[1:(n - kn + 1)]^2) -
             psi1 * dn / (2 * theta^2 * psi2) * rv) / a
  expect_equal(iv_hp2013(y, kn = kn)@value, hp_c)
  i <- 0:(n - 2 * kn + 1)
  v11 <- sum(abs(zbar[i + 1]) * abs(zbar[i + kn + 1]))
  hp_bt <- (n * sqrt(dn) / ((n - 2 * kn + 2) * theta * (2 / pi) * psi2) * v11 -
              psi1 * dn / (2 * theta^2 * psi2) * rv) / a
  expect_equal(iv_hp2013(y, kn = kn, estimator = "bipower")@value, hp_bt)
  # JPV (2010, eq. 3.5) with the foundation's V(Z, g, p, r).
  pa <- preaverage(y, kn)
  stat <- (preavg_power_variation(pa, 2) - preavg_power_variation(pa, 0, 1) / 2) / kn
  expect_equal(iv_jpv2010(y, kn = kn)@value, stat * 12)
})

test_that("PV (2009) block means and their exact moments match the definitions", {
  set.seed(3)
  n <- 40L
  y <- cumsum(c(0, rnorm(n)))
  for (kl in list(c(1L, 2L), c(3L, 5L), c(4L, 5L), c(3L, 8L))) {
    K <- kl[1]
    L <- kl[2]
    nb <- L - K + 1
    blocks <- 1:(n %/% L)
    direct <- vapply(blocks, function(m) {
      mean(vapply(((m - 1) * L):(m * L - K), function(i) y[i + K + 1] - y[i + 1], 0))
    }, 0)
    expect_equal(pv_block_means(y, K, L), direct) # eq. (2.4)
    # Coefficients of dX and of U in Zbar_1 and Zbar_2.
    cx <- function(m) {
      co <- numeric(n)
      for (i in ((m - 1) * L):(m * L - K)) co[(i + 1):(i + K)] <- co[(i + 1):(i + K)] + 1
      co / nb
    }
    cu <- function(m) {
      co <- numeric(n + 1)
      for (i in ((m - 1) * L):(m * L - K)) {
        co[i + K + 1] <- co[i + K + 1] + 1
        co[i + 1] <- co[i + 1] - 1
      }
      co / nb
    }
    mom <- pv_moments(n, K, L)
    expect_equal(mom[["signal"]], sum(cx(1)^2) / n)
    expect_equal(mom[["noise"]], sum(cu(1)^2))
    expect_equal(mom[["cross"]], sum(cu(1) * cu(2)))
  }
  # Eq. (3.10) with the constants (3.3) and the noise estimate (3.5).
  K <- 3L
  L <- 5L
  zb <- pv_block_means(y, K, L)
  c1 <- K / sqrt(n)
  c2 <- L / K
  nu1 <- c1 * (3 * c2 - 4 + (2 - c2)^3) / (3 * (c2 - 1)^2)
  nu2 <- 2 * (c2 - 1) / (c1 * (c2 - 1)^2)
  mbv <- ((c1 * c2 / (2 / pi)) * sum(abs(zb[-8]) * abs(zb[-1])) -
            nu2 * sum(diff(y)^2) / (2 * n)) / nu1
  expect_equal(iv_pv2009(y, K = K, L = L, constants = "asymptotic",
                         noise = "pv2009")@value, mbv)
})

test_that("finite-sample constants remove the small-K bias of the asymptotic ones", {
  # n = 1000, IV = 1, noise variance 0.9 IV/n: the regime of small K and kn.
  sims <- lapply(1:300, function(r) {
    set.seed(2000 + r)
    cumsum(c(0, rnorm(1000, 0, sqrt(1 / 1000)))) + rnorm(1001, 0, 0.03)
  })
  mc <- function(f) {
    v <- vapply(sims, function(y) suppressWarnings(f(y)@value), 0)
    c(mean = mean(v), se = sd(v) / sqrt(length(v)))
  }
  for (f in list(function(y) iv_pv2009(y, K = 1, L = 2),
                 function(y) iv_pv2009(y, K = 2, L = 3),
                 function(y) iv_jlmpv2009(y, kn = 3),
                 function(y) iv_jpv2010(y, kn = 5, constants = "finite_sample"),
                 function(y) iv_hp2013(y, kn = 5, estimator = "bipower"))) {
    r <- mc(f)
    expect_lt(abs(r[["mean"]] - 1), 4 * r[["se"]])
  }
  r <- mc(function(y) iv_pv2009(y, K = 2, L = 3, constants = "asymptotic"))
  expect_gt(abs(r[["mean"]] - 1), 10 * r[["se"]])
})

test_that("noise-robust estimators recover IV on a simulated day", {
  n <- 86400L
  iv <- 9e-4
  y <- sim_log_prices(n, iv = iv, noise_sd = 2e-4, seed = 21)
  # RV is dominated by 2 n omega^2 = 6.9e-3.
  expect_gt(iv_realized(y)@value, 5 * iv)
  # Tolerances are 4 standard deviations of IV-hat / IV: the asymptotic sd of
  # JLMPV (3.9) = HP (2.14), 3.5% at theta = 1/3 and 4.7% at theta = 0.6; of
  # PV (3.18) at the K, L chosen, 2.3%; and the Monte Carlo sd of BT at
  # theta = 0.6, 5.1% (tests/simulations/iv-bias.R).
  expect_equal(iv_jlmpv2009(y)@value, iv, tolerance = 0.14)
  expect_equal(iv_jpv2010(y)@value, iv, tolerance = 0.14)
  expect_equal(iv_hp2013(y)@value, iv, tolerance = 0.19)
  expect_equal(iv_hp2013(y, noise = "dependent")@value, iv, tolerance = 0.19)
  expect_equal(iv_pv2009(y)@value, iv, tolerance = 0.094)
  expect_equal(iv_hp2013(y, estimator = "bipower")@value, iv, tolerance = 0.20)
})

test_that("jump-robust estimators stay near IV with a 5% jump", {
  n <- 86400L
  iv <- 9e-4
  y <- sim_log_prices(n, iv = iv, noise_sd = 5e-5, jumps = cbind(43000L, 0.05),
                      seed = 22)
  # Quadratic variation is IV + 0.05^2 = 3.8 IV; the pre-averaged RV finds it
  # (Monte Carlo sd 3.8% of QV, iv-bias.R; tolerance 4 sd).
  expect_equal(iv_hp2013(y)@value, iv + 0.05^2, tolerance = 0.15)
  # Bipower-type estimators keep a finite-sample jump bias of order
  # |dX| n^(-1/4). In iv-bias.R the volatility is biased by +1.0% (sd 1.2%)
  # for PV and by +4.3% (sd 2.5%) for BT at theta = 0.2, so the tolerances
  # are bias + 4 sd: 6% and 15%. Without jump robustness it would be +94%.
  expect_equal(sqrt(iv_pv2009(y)@value), sqrt(iv), tolerance = 0.06)
  expect_equal(sqrt(iv_hp2013(y, estimator = "bipower", theta = 0.2)@value),
               sqrt(iv), tolerance = 0.15)
  # The estimator as printed (3.5 noise, 3.3 constants) collapses here.
  lit <- suppressWarnings(iv_pv2009(y, constants = "asymptotic", noise = "pv2009"))
  expect_lt(lit@value, 0.6 * iv)
})

test_that("integrated_variance() dispatches and returns IVEstimate objects", {
  y <- sim_log_prices(2000, iv = 1e-4, noise_sd = 1e-4, seed = 4)
  for (m in c("rv", "bv", "jlmpv2009", "jpv2010", "pv2009", "hp2013")) {
    e <- integrated_variance(y, m)
    expect_true(S7::S7_inherits(e, IVEstimate))
    expect_identical(e@method, m)
  }
  expect_identical(integrated_variance(y, "pv2009", K = 2L)@tuning$K, 2L)
  expect_identical(integrated_variance(y)@method, "rv")
  s <- hf_series(as.POSIXct("2022-01-01", tz = "UTC") + 0:2000, y, log = TRUE)
  expect_equal(integrated_variance(s, "pv2009")@value, iv_pv2009(y)@value)
})

test_that("bad inputs raise hf_input_error", {
  y <- sim_log_prices(500, noise_sd = 1e-4, seed = 1)
  expect_error(integrated_variance(y, "foo"), class = "hf_input_error")
  expect_error(integrated_variance(y, "pv2009", k = 2), class = "hf_input_error")
  expect_error(integrated_variance(y, "pv2009", 2), class = "hf_input_error")
  expect_error(iv_pv2009(c(1, NA, 2, 3, 4, 5)), class = "hf_input_error")
  expect_error(iv_pv2009(y, K = 0), class = "hf_input_error")
  expect_error(iv_pv2009(y, K = 5, L = 5), class = "hf_input_error")
  expect_error(iv_pv2009(y, K = 100, L = 300), class = "hf_input_error")
  expect_error(iv_pv2009(y, noise_variance = -1), class = "hf_input_error")
  expect_error(iv_jlmpv2009(y, kn = 2), class = "hf_input_error")
  expect_error(iv_jlmpv2009(y, theta = "best"), class = "hf_input_error")
  expect_error(iv_jpv2010(y, theta = -1), class = "hf_input_error")
  expect_error(iv_hp2013(y, estimator = "bipower", noise = "dependent"),
               class = "hf_input_error")
  expect_error(iv_hp2013(y, theta = "mse"), class = "hf_input_error")
  expect_error(iv_hp2013(y, benchmark = "a"), class = "hf_input_error")
  expect_error(iv_jlmpv2009(y, kn = 5, theta = "best"), class = "hf_input_error")
  expect_error(iv_hp2013(y, kn = 400, estimator = "bipower"), class = "hf_input_error")
  expect_error(noise_variance(y, "pv2009", q = 1), class = "hf_input_error")
  expect_error(iv_bipower(y, adjust = NA), class = "hf_input_error")
})
