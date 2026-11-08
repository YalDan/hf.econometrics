# Ait-Sahalia and Jacod (2009a, 2009b, 2011, 2012).

n_day <- 23400 # one trading day of 1-second prices
iv_day <- 0.25^2 / 252 # 25% annual volatility over one day of 1/252 years

test_that("B(p, u, k Delta_n) matches its definition (AJ 2012, eqs. 7-8)", {
  y <- sim_log_prices(25, seed = 5)
  direct <- function(p, u, k, upper = FALSE) {
    idx <- seq(1, length(y), by = k)
    total <- 0
    for (i in 2:length(idx)) {
      d <- abs(y[idx[i]] - y[idx[i - 1]])
      if (if (upper) d > u else d <= u) total <- total + d^p
    }
    total
  }
  u <- stats::median(abs(diff(y)))
  for (k in 1:3) {
    expect_equal(aj_power_variation(y, 4, k = k), direct(4, Inf, k))
    expect_equal(aj_power_variation(y, 3.5, u, k = k), direct(3.5, u, k))
    expect_equal(aj_power_variation(y, 0, u, k = k, upper = TRUE), direct(0, u, k, TRUE))
  }
  # 0.1.0 defect: increments at k Delta_n come from the subsampled prices,
  # not from every k-th one-step return
  expect_false(isTRUE(all.equal(aj_power_variation(y, 2, k = 2),
                                sum(diff(y)[c(TRUE, FALSE)]^2))))
})

test_that("moment constants match AJ (2009a, eq. 20 and Section 5.3)", {
  for (k in 2:4) expect_equal(aj_M(4, k), 16 * k * (2 * k^2 - k - 1) / 3)
  expect_equal(aj_mkp(4, 2), 105 + 6 * 15 + 9) # E U^4 (U + V)^4
  # AJ (2011, eq. 38): N = M m_p^2 / m_2p (the closed form in eq. 49 is M)
  expect_equal(aj_N(4, 2), 16 * 2 * (2 * 4 - 2 - 1) / 35)
  inner <- function(u) {
    vapply(u, function(a) stats::integrate(function(v) abs(a + v)^3.25 * stats::dnorm(v),
                                           -Inf, Inf, rel.tol = 1e-10)$value, numeric(1))
  }
  quad <- stats::integrate(function(u) abs(u)^3.25 * stats::dnorm(u) * inner(u),
                           -Inf, Inf, rel.tol = 1e-9)$value
  expect_equal(aj_mkp(3.25, 2), quad, tolerance = 1e-7)
})

test_that("truncation levels carry explicit time units", {
  x <- sim_log_prices(n_day, iv = iv_day, seed = 7)
  delta <- 1 / (252 * n_day) # Delta_n in years
  expect_equal(aj_truncation(x, 5, method = "rate", varpi = 0.47),
               5 * 0.25 * delta^0.47)
  # the pilot recovers the 25% volatility: u_n / sqrt(Delta_n) = alpha * sigma-hat
  expect_equal(aj_truncation(x, 1) / sqrt(delta), 0.25, tolerance = 0.02)
  expect_equal(aj_truncation(x, c(1, 7)), c(1 / 7, 1) * aj_truncation(x, 7))
})

test_that("S_J has the limits of AJ (2012, eqs. 15 and 40)", {
  cont <- aj_jump_test(sim_log_prices(n_day, iv = iv_day, seed = 1))
  jump <- aj_jump_test(sim_log_prices(n_day, iv = iv_day, seed = 1,
                                      jumps = cbind(12000, 0.01)))
  noisy <- aj_jump_test(sim_log_prices(n_day, iv = iv_day, seed = 1, noise_sd = 1e-3))
  # 0.1.0 defect: the labels of the limits were swapped
  expect_equal(cont@limits, c(jumps = 1, continuous = 2, noise = 0.5, rounding = sqrt(0.5)))
  # continuous: sd of S is sqrt(M(4, 2) / n) (AJ 2009a, eq. 19)
  expect_lt(abs(cont@statistic - 2), 4 * sqrt(aj_M(4, 2) / n_day))
  # one 1% jump: sd of S is about p sigma Delta_n^(1/2) / |J| = 0.04 (eq. 18)
  expect_lt(abs(jump@statistic - 1), 4 * 4 * sqrt(iv_day / n_day) / 0.01)
  # additive noise dominates: S -> 1/k (AJ 2009a, eq. 43)
  expect_lt(abs(noisy@statistic - 0.5), 0.1)
  expect_false(cont@reject_no_jumps)
  expect_true(cont@reject_jumps)
  expect_true(jump@reject_no_jumps)
  expect_false(jump@reject_jumps)
  # the variance estimators match their theoretical values M / n and
  # (k - 1) p^2 sigma^2 Delta_n / J^2 (AJ 2009a, eqs. 19 and 18); the sampling
  # CV of V^c is about 8% (it is driven by sum |dX|^8), that of V^j about 3%
  expect_equal(cont@variance_no_jumps, aj_M(4, 2) / n_day, tolerance = 0.3)
  expect_equal(jump@variance_jumps, 16 * iv_day / n_day / 0.01^2, tolerance = 0.15)
  mp <- aj_jump_test(sim_log_prices(n_day, iv = iv_day, seed = 1),
                     aj_jump_options(variance = "multipower"))
  expect_equal(mp@variance_no_jumps, aj_M(4, 2) / n_day, tolerance = 0.3)
})

test_that("the tests are one-sided (AJ 2009a, eqs. 32, 36; AJ 2011, eq. 39)", {
  # increments a, a, -a, -a, ...: S(4, 2) = 16 / 2 = 8, far above k^(p/2-1) = 2
  a <- 1e-4
  y <- cumsum(c(0, rep(c(a, a, -a, -a), 100)))
  res <- aj_jump_test(y)
  expect_equal(res@statistic, 8)
  expect_gt(res@z_no_jumps, 10)
  expect_false(res@reject_no_jumps) # only small values reject "no jumps"
  expect_true(res@reject_jumps)
  fa <- aj_finite_activity_test(y, aj_finite_activity_options(truncation = "rate", varpi = 0.2))
  expect_equal(fa@statistic, 8)
  expect_false(fa@reject)
  cheb <- aj_jump_test(y, aj_jump_options(jump_critical = "chebyshev"))
  expect_equal(cheb@p_value_jumps, cheb@z_jumps^(-2))
  expect_equal(cheb@critical_jumps, 1 + sqrt(cheb@variance_jumps / 0.05)) # eq. (39)
})

test_that("S_FA separates finite from infinite activity (AJ 2011, eqs. 34-35)", {
  fin <- aj_finite_activity_test(sim_log_prices(
    n_day, iv = iv_day, seed = 2, jumps = cbind(c(3000, 9000, 20000), c(0.005, -0.004, 0.006))
  ))
  # V_n = N(4, 2) B(8) / B(4)^2 is about M(4, 2) / n (AJ 2011, eq. 37)
  expect_lt(abs(fin@statistic - 2), 4 * sqrt(aj_M(4, 2) / n_day))
  expect_false(fin@reject)
  # Brownian motion plus Cauchy jumps, P(|jump increment| > 4 sd) = 5%
  set.seed(3)
  s <- sqrt(iv_day / n_day)
  dx <- stats::rnorm(n_day, 0, s) + stats::rcauchy(n_day, scale = 0.05 * pi * 4 * s / 2)
  inf <- aj_finite_activity_test(cumsum(c(0, dx)))
  # at alpha = 7 the limit is about 1.1-1.3 rather than 1 (AJ 2011, Figure 2)
  expect_lt(inf@statistic, 1.5)
  expect_true(inf@reject)
  expect_equal(inf@limits, c(finite = 2, infinite = 1, noise = 0.5))
})

test_that("beta-hat follows AJ (2009b, eqs. 16 and 21)", {
  # with rate truncation, sigma = 1 and Delta_n = 1 the cutoffs are alpha itself
  set.seed(6)
  dx <- c(rep(0.5, 1000), rep(10, 40), rep(20, 10))
  y <- cumsum(c(0, sample(dx * sample(c(-1, 1), length(dx), TRUE))))
  opt <- aj_activity_index_options(truncation = "rate", varpi = 0.2, sigma = 1,
                                   horizon = length(dx), bias_correction = "none")
  bh <- aj_activity_index(y, opt)
  expect_equal(bh@details$count, 50)
  expect_equal(bh@details$count2, 10)
  expect_equal(bh@estimate, log(50 / 10) / log(2))
  expect_equal(bh@se, sqrt(1 / 10 - 1 / 50) / log(2))
  # Brownian motion plus Cauchy jumps: beta = 1
  set.seed(4)
  s <- sqrt(iv_day / n_day)
  dx <- stats::rnorm(n_day, 0, s) + stats::rcauchy(n_day, scale = 0.01 * pi * 7 * s / 2)
  est <- aj_activity_index(cumsum(c(0, dx)))
  expect_lt(abs(est@estimate - 1), 3 * est@se)
  expect_lt(abs(est@estimate_bc - 1), 3 * est@se)
})

test_that("the spectrum samples prices on the Delta_n grid (AJ 2012, p. 30)", {
  x <- sim_log_prices(4500, iv = iv_day, seed = 8)
  sp <- aj_spectrum(x, base_interval = 1,
                    options = aj_spectrum_options(deltas = c(5, 15), p = c(3, 4)))
  x5 <- x[seq(1, length(x), by = 5)]
  row <- sp[sp$statistic == "SJ" & sp$delta == 5 & sp$k == 2 & sp$p == 4, ]
  expect_equal(row$value, aj_power_variation(x5, 4, k = 2) / aj_power_variation(x5, 4))
  expect_equal(row$limit_continuous, 2)
  expect_equal(nrow(sp), 2 * 2 * 2 * (1 + 6))
  u <- sp$cutoff[sp$statistic == "SFA" & sp$delta == 15 & sp$alpha == 7][1]
  x15 <- x[seq(1, length(x), by = 15)]
  expect_equal(u, aj_truncation(x15, 7)) # one window of 1/252 years
})

test_that("bad inputs raise hf_input_error", {
  x <- sim_log_prices(200, seed = 9)
  expect_error(aj_power_variation(c(1, NA, 2), 4), class = "hf_input_error")
  expect_error(aj_power_variation(x, -1), class = "hf_input_error")
  expect_error(aj_power_variation(x, 2, k = 500), class = "hf_input_error")
  expect_error(aj_truncation(x, -1), class = "hf_input_error")
  expect_error(aj_jump_options(p = 3), class = "hf_input_error")
  expect_error(aj_jump_options(k = 1), class = "hf_input_error")
  expect_error(aj_jump_options(varpi = 0.2), class = "hf_input_error") # AJ 2009a, Thm 6
  expect_error(aj_jump_test(x, options = "default"), class = "hf_input_error")
  expect_error(aj_finite_activity_options(null = "infinite", p = 4, p_prime = 4),
               class = "hf_input_error")
  expect_error(aj_activity_index_options(alpha_ratio = 1), class = "hf_input_error")
  expect_error(aj_activity_index_options(estimator = "frequency", bias_correction = "plugin"),
               class = "hf_input_error")
  expect_error(aj_spectrum(x), class = "hf_input_error") # base interval unknown
  expect_error(aj_spectrum(x, base_interval = 2,
                           options = aj_spectrum_options(deltas = 5)),
               class = "hf_input_error")
})
