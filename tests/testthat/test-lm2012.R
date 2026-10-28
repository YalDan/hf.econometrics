# Equally weighted MA(order) noise with marginal standard deviation `sd`; it
# is `order`-dependent, so Lee and Mykland's k is order + 1.
ma_noise <- function(n, sd, order, seed) {
  set.seed(seed)
  e <- stats::rnorm(n + order)
  as.numeric(stats::filter(e, rep(1, order + 1), sides = 1))[-seq_len(order)] *
    sd / sqrt(order + 1)
}

test_that("blocks follow Definition 1 on a tick-index toy (k = 3, M = 4)", {
  y <- (0:62)^2 / 1e4 # price at tick i is a known, non-linear function of i
  n <- 62; k <- 3; M <- 4
  # Direct Definition 1: t_j for j = 0, kM, 2kM, ..., full blocks only.
  j <- seq(0, n, by = k * M)
  j <- j[floor(j / k) + M - 1 <= n %/% k]
  P_hat <- vapply(j, function(jj) mean(y[k * (floor(jj / k) + 0:(M - 1)) + 1]), 0)

  b <- lm_blocks(lm_test(y, lm_options(k = k, M = M, iv = 1e-4)))
  expect_equal(b$index, c(0, 12, 24, 36))
  expect_equal(b$P_hat, P_hat[1:4])
  expect_equal(b$P_hat[2], mean(y[c(12, 15, 18, 21) + 1])) # not ticks 9-18
  expect_equal(b$P_hat[1], mean(y[c(0, 3, 6, 9) + 1]))     # M points from t_0
  expect_equal(b$L, diff(P_hat))                           # forward, labelled t_j
  expect_equal(b$time_end, b$index + (2 * M - 1) * k)
  # Ticks 60-62 do not complete a block and must not enter the test.
  y[61:63] <- 99
  expect_equal(lm_blocks(lm_test(y, lm_options(k = k, M = M, iv = 1e-4)))$L, b$L)
})

test_that("C follows Table 5 and its documented extension", {
  q <- c(0.01, 0.03, 0.05, 0.07, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1) / 100
  expect_equal(lm_select_C(q), 1 / c(19, 19, 19, 18, 18, 18, 16, 16, 9, 9, 9, 9, 9, 8, 8))
  expect_equal(lm_select_C(c(0.059, 0.061, 0.34, 0.36) / 100), 1 / c(19, 18, 16, 9))
  expect_equal(lm_select_C(0.35 / 100, method = "linear"), (1 / 16 + 1 / 9) / 2)
  expect_equal(lm_select_C(1e-6), 1 / 19)
  expect_warning(C <- lm_select_C(0.02), class = "hf_warning")
  expect_equal(C, 1 / 8)
})

test_that("k is the largest significant acf lag: 1 for iid noise, 10 for MA(9) noise", {
  n <- 23400
  y <- sim_log_prices(n, iv = 1e-4, noise_sd = 2e-4, seed = 4)
  expect_identical(lm_select_k(y), 1L) # 0.1.0 counted lag 0 and returned k >= 2
  x <- sim_log_prices(n, iv = 1e-4, seed = 4) + ma_noise(n + 1, 2e-4, 9, seed = 104)
  expect_identical(lm_select_k(x), 10L)
  expect_lte(lm_select_k(x, max_lag = 9), 9L) # lags beyond max_lag are ignored
})

test_that("q_hat, M, V_n and the Gumbel constants follow Prop. 1, eqs. (7), (9) and (13)", {
  n <- 6000
  y <- sim_log_prices(n, iv = 1e-4, noise_sd = 1e-4, seed = 2)
  seen <- NULL
  stub <- function(x, ...) {
    seen <<- x
    1e-4
  }
  res <- lm_test(y, lm_options(k = 3, iv_method = stub))
  expect_equal(seen, y[seq(1, n + 1, by = 3)]) # sigma-hat from the k-subsampled prices
  expect_equal(res@q_hat, sqrt(mean((y[5:(n + 1)] - y[2:(n - 2)])^2) / 2))
  expect_true(is.integer(res@M))
  expect_equal(res@M, as.integer(floor(lm_select_C(res@q_hat) * sqrt(n %/% 3))))
  expect_equal(res@C_effective, res@M / sqrt(n %/% 3))
  expect_equal(res@V_n, 2 / 3 * res@C_effective^2 * 1e-4 + 2 * res@q_hat^2)
  N <- n %/% (3 * res@M)
  expect_equal(res@A_n, sqrt(2 * log(N)) - (log(pi) + log(log(N))) / (2 * sqrt(2 * log(N))))
  expect_equal(res@B_n, 1 / sqrt(2 * log(N)))
  expect_equal(res@threshold, res@A_n - log(-log(0.99)) * res@B_n) # no Bonferroni
  expect_equal(res@p_value, 1 - exp(-exp(-res@statistic)))
})

test_that("a no-jump day is not rejected and a 1% jump is found where it occurred", {
  n <- 86400
  y <- sim_log_prices(n, iv = 9e-4, noise_sd = 5e-5, seed = 3)
  expect_false(lm_test(y, lm_options(iv = 9e-4))@reject)
  y1 <- sim_log_prices(n, iv = 9e-4, noise_sd = 5e-5, seed = 3, jumps = cbind(50001, 0.01))
  res <- lm_test(y1, lm_options(iv = 9e-4))
  expect_true(res@reject)
  hit <- subset(lm_blocks(res), jump != 0L)
  expect_true(nrow(hit) >= 1L)
  expect_true(all(hit$index < 50001 & 50001 <= hit$time_end & hit$jump == 1L))
})

test_that("the step-down adjustment matches a hand computation (Section 6.2)", {
  p <- c(0.01, 0.04, 0.03, 0.005)
  # Ordered: 0.005, 0.01, 0.03, 0.04 -> 4 * 0.005 = 0.02, max(0.02, 3 * 0.01) =
  # 0.03, max(0.03, 2 * 0.03) = 0.06, max(0.06, 1 * 0.04) = 0.06.
  h <- lm_holm(p, alpha = 0.05)
  expect_equal(h$p_adjusted, c(0.03, 0.06, 0.06, 0.02))
  expect_equal(h$reject, c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(lm_holm(c(p, NA))$p_adjusted, c(0.03, 0.06, 0.06, 0.02, NA))
})

test_that("bad options and inputs raise hf_input_error", {
  expect_error(lm_options(alpha = 1), class = "hf_input_error")
  expect_error(lm_options(k = 0), class = "hf_input_error")
  expect_error(lm_options(C = -1), class = "hf_input_error")
  expect_error(lm_options(C = 0.05, M = 5), class = "hf_input_error")
  expect_error(lm_options(variance = "sample"), class = "hf_input_error")
  expect_error(lm_options(iv = 1e-4, variance = "empirical"), class = "hf_input_error")
  expect_error(lm_options(acf_max_lag = 1), class = "hf_input_error")
  expect_error(lm_options(iv_args = list(1)), class = "hf_input_error")
  expect_error(lm_test(cumsum(rnorm(30)), "options"), class = "hf_input_error")
  expect_error(lm_test(cumsum(rnorm(30)), lm_options(k = 1, M = 20, iv = 1)),
               class = "hf_input_error")
  expect_error(lm_holm(c(0.5, 2)), class = "hf_input_error")
})
