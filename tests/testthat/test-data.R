t0 <- as.POSIXct("2022-03-04", tz = "UTC")

test_that("timestamps: ISO-8601, POSIXct and epochs agree, and numbers need a unit", {
  iso <- parse_timestamps(c("2022-03-04T00:00:00.141Z", "2022-03-04 01:00:00.141+01:00",
                            "2022-03-03T22:30:00.141-01:30"))
  expect_true(all(abs(as.numeric(iso) - 1646352000.141) < 1e-6))
  expect_identical(as.numeric(parse_timestamps(1646352000141, unit = "ms")), as.numeric(iso[1]))
  pt <- as.POSIXct("2022-03-04 10:00:00", tz = "UTC")
  expect_identical(parse_timestamps(pt), pt)
  expect_error(parse_timestamps(1646352000141), "unit", class = "hf_input_error")
  expect_error(parse_timestamps("04/03/2022 10:00"), class = "hf_input_error")
  expect_error(parse_timestamps("2022-02-30"), class = "hf_input_error")
  # 0.1.0 divided parsed seconds by 1000 and moved the sample day to 1970.
  expect_warning(parse_timestamps(1646352000141 / 1e6, unit = "s"), class = "hf_warning")
})

test_that("simultaneous prices are averaged (Lee and Mykland 2012, Section 6.1)", {
  a <- aggregate_simultaneous(t0 + c(1, 0, 1), c(102, 100, 104))
  expect_equal(a$price, c(100, 103))
  expect_identical(a$n_obs, c(1L, 2L))
})

test_that("outlier filter is symmetric, removes observations, and never carries returns", {
  set.seed(42)
  r <- stats::rnorm(2000, 0, 1e-4)
  r[500] <- 0.01   # up-move into observation 501
  r[1500] <- -0.01 # down-move into observation 1501 (0.1.0 cut only the upper tail)
  p <- 100 * exp(cumsum(c(0, r)))
  f <- filter_outliers(seq_along(p), p, n_sd = 10)
  expect_identical(f$removed$index, c(501L, 1501L))
  expect_equal(f$series$price, p[-c(501, 1501)])
  # Prices and returns stay consistent: the removed return merges into the
  # next one instead of being replaced by the previous return.
  expect_equal(f$series$log_return[-1], diff(log(f$series$price)))
  expect_equal(f$series$log_return[501], log(p[502] / p[500]))
})

test_that("a bounceback needs a reversal of opposite sign", {
  p <- c(100, 100.01, 101, 100.02, 100.03, 101, 102, 102.01, 105, 104.5, 104.51)
  f <- filter_bouncebacks(seq_along(p), p, threshold = 0.001)
  # 3 is a spike; 6 starts two large moves in the same direction (deleted by
  # 0.1.0); 9 is a jump with a small retracement.
  expect_identical(f$removed$index, 3L)
  expect_equal(f$series$log_return[-1], diff(log(f$series$price)))
  expect_identical(filter_bouncebacks(seq_along(p), p, 0.001, tolerance = 1)$removed$index,
                   c(3L, 9L))
})

test_that("consecutive flags: the first flag of a chain suppresses the rest", {
  expect_identical(drop_consecutive_flags(c(100, 110, 120), rep(TRUE, 3), range = 30),
                   c(TRUE, FALSE, FALSE))
  expect_identical(drop_consecutive_flags(c(150, 100, 125), rep(TRUE, 3), range = 30),
                   c(TRUE, TRUE, FALSE))
  expect_identical(drop_consecutive_flags(c(100, 125, 150), rep(TRUE, 3), range = 30,
                                          anchor = "previous"), c(TRUE, FALSE, FALSE))
  # Against a direct loop over the flags.
  set.seed(7)
  idx <- sort(sample(1:400, 120))
  flag <- stats::runif(120) < 0.5
  direct <- logical(120)
  anchor <- -Inf
  for (i in which(flag)) {
    if (idx[i] - anchor > 10) {
      direct[i] <- TRUE
      anchor <- idx[i]
    }
  }
  expect_identical(drop_consecutive_flags(idx, flag, 10), direct)
})

test_that("frequency choice counts occupied intervals, not ticks", {
  # 86,400 ticks, five within one second out of every five: enough ticks for
  # the 0.1.0 rule at 1 s (N >= 82,080), yet only 20% of the seconds traded.
  t <- t0 + rep(seq(2.5, 86400, by = 5), each = 5) + rep((0:4) / 10, 17280)
  f <- choose_frequency(t)
  expect_identical(as.numeric(f), 5)
  expect_equal(attr(f, "coverage")[["1"]], 0.2)
  expect_identical(as.numeric(choose_frequency(t0 + seq(2.5, 86400, by = 10))), 10)
  expect_true(is.na(choose_frequency(t0 + seq(2.5, 86400, by = 20))))
})

test_that("previous-tick grid covers the whole day, including the last interval", {
  t <- t0 + c(0.25, 1, 1, 7.5, 86399.5)
  g <- sample_previous_tick(t, c(100, 101, 102, 103, 104), interval = 1)
  expect_identical(nrow(g), 86401L)
  expect_equal(range(g$time), c(t0, t0 + 86400))
  expect_true(is.na(g$price[1]))      # no tick at or before 00:00:00
  expect_equal(g$price[2], 102)       # ticks exactly at 00:00:01 belong to it
  expect_equal(g$price[9], 103)       # 00:00:08 carries the tick at 7.5 s
  expect_equal(g$price[86401], 104)   # (23:59:59, 24:00:00] is not dropped
  expect_identical(g$n_obs[c(2, 3, 9, 86401)], c(3L, 0L, 1L, 1L))
  expect_identical(sum(g$n_obs), 5L)
})

test_that("windows are closed on the left (Lee and Mykland 2012, Section 6.1)", {
  w <- split_windows(t0 + c(10, 3599.9, 3600, 7300), "1 hour")
  expect_identical(w$n, c(2L, 1L, 1L))
  expect_identical(w$from, c(1L, 3L, 4L))
  expect_equal(w$start, t0 + c(0, 3600, 7200))
  # Bin-end labels (sample_previous_tick) keep 24:00:00 in its own day.
  r <- split_windows(t0 + c(5, 86400), "1 day", origin = t0, closed = "right")
  expect_identical(r$n, 2L)
})
