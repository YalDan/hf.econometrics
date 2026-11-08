test_that("pre-averaged increments match the definition in JPV (2010, eq. 2.11)", {
  y <- sim_log_prices(60, noise_sd = 1e-4, seed = 3)
  kn <- 7L
  pa <- preaverage(y, kn)
  dy <- diff(y)
  g <- pmin((1:kn) / kn, 1 - (1:kn) / kn)
  dg2 <- diff(c(0, g))^2
  direct_bar <- vapply(0:(60 - kn), function(i) sum(g[1:(kn - 1)] * dy[i + 1:(kn - 1)]), 0)
  direct_hat <- vapply(0:(60 - kn), function(i) sum(dg2 * dy[i + 1:kn]^2), 0)
  expect_equal(pa@ybar, direct_bar, tolerance = 1e-12)
  expect_equal(pa@yhat, direct_hat, tolerance = 1e-12)
})

test_that("rho coefficients solve AJL (2012) eq. 3.9", {
  expect_equal(preavg_rho(2), c(1, -0.5))
  expect_equal(preavg_rho(4), c(1, -3, 0.75)) # eq. 3.10
  expect_equal(preavg_rho(8), c(1, -14, 52.5, -52.5, 6.5625))
  expect_error(preavg_rho(3), class = "hf_input_error")
})

test_that("Vbar(4) matches its continuous limit (JPV 2010, eq. 3.11)", {
  n <- 23400; iv <- 1e-4; kn <- 30L
  y <- sim_log_prices(n, iv = iv, noise_sd = 2e-5, seed = 11)
  pa <- preaverage(y, kn)
  theta <- kn / sqrt(n) # Delta_n = 1/n on a unit window
  limit <- 3 * (theta / 12)^2 * iv^2
  expect_equal(preavg_vbar(pa, 4), limit, tolerance = 0.15)
})

test_that("inputs are type-checked", {
  expect_error(preaverage("a", 5))
  expect_error(preaverage(c(1, NA, 2), 2), class = "hf_input_error")
  expect_error(hf_series(Sys.time() + 2:1, c(1, 2)), "sorted")
})
