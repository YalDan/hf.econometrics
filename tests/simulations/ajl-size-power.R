# Monte Carlo study of the Ait-Sahalia, Jacod and Li (2012) jump test.
#
# Replicates the design of AJL (2012, Section 4) at reduced scale:
#   Z = X^c + J + eps, X^c Heston with sqrt(beta) = 0.4, gamma = 0.5, kappa = 5,
#   rho = -0.5, X_0 = log(25) (time in years); T = 5 days of 6.5 hours sampled
#   every 5 seconds (n = 23400); noise (4.1): none, Gaussian, t (df = 2.5,
#   truncated at 50 sqrt(df / (df - 2))) or Gaussian-t mixture, with
#   instantaneous sd 2 sigma_t Delta_n^(1/2); optional rounding of the price to
#   a tick; J symmetric alpha-stable, rescaled so that its realized quadratic
#   variation is beta T / 3 (Tables 2, 3A) or 3 beta T (Table 3B). Test tuning:
#   p = 4, tent g, h = g(2x), phi = g, kn' = 3 kn, u_n = C (Vbar(Z, g, 2) / T)^(1/2)
#   Delta_n^0.49 with time in years, as in the authors' code (horizon = 5/250).
#   The initial variance is drawn from the stationary Gamma law of v (the paper
#   does not say how v_0 is chosen).
#
# Usage (from the package root):
#   Rscript tests/simulations/ajl-size-power.R [part] [reps] [seed]
# with part one of "aprime", "size" (no rounding), "rounding" (price rounded to
# 0.03), "paired" (5 x reps paths at kn = 100, C = 5), "power", "jumpnull" or
# "all" (default), and reps the number of replications per cell (default 200).
# Runtime on one free core with reps = 200: about 1 minute for aprime and
# jumpnull, 3-4 minutes for each of size, rounding, paired and power.

args <- commandArgs(trailingOnly = TRUE)
part <- if (length(args) >= 1) args[1] else "all"
reps <- if (length(args) >= 2) as.integer(args[2]) else 200L
seed0 <- if (length(args) >= 3) as.integer(args[3]) else 20120207L

pkg_root <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) normalizePath(file.path(dirname(f), "..", "..")) else "."
})
if (requireNamespace("devtools", quietly = TRUE)) {
  suppressMessages(devtools::load_all(pkg_root, quiet = TRUE))
} else {
  library(hf.econometrics)
}

N_OBS <- 23400L
DAYS <- 5
T_YEARS <- DAYS / 250
DT <- T_YEARS / N_OBS
BETA <- 0.4^2

# Simulation of the Section 4 design --------------------------------------------

# Returns an R x (n + 1) matrix of observed log prices, one path per row.
sim_section4 <- function(R, noise = "none", tick = 0, jump_alpha = NA,
                         jump_qv = 1 / 3, n = N_OBS) {
  kappa <- 5; gam <- 0.5; rho <- -0.5; df <- 2.5
  v <- stats::rgamma(R, shape = 2 * kappa * BETA / gam^2, rate = 2 * kappa / gam^2)
  x <- matrix(0, R, n + 1)
  sig <- matrix(0, R, n + 1)
  xc <- rep(log(25), R)
  x[, 1] <- xc
  sig[, 1] <- sqrt(v)
  sdt <- sqrt(DT)
  for (i in seq_len(n)) { # Euler scheme on the sampling grid
    e1 <- stats::rnorm(R)
    e2 <- rho * e1 + sqrt(1 - rho^2) * stats::rnorm(R)
    sv <- sqrt(v)
    xc <- xc + sv * sdt * e1
    v <- pmax(v + kappa * (BETA - v) * DT + gam * sv * sdt * e2, 0)
    x[, i + 1] <- xc
    sig[, i + 1] <- sqrt(v)
  }
  if (!is.na(jump_alpha)) { # Chambers-Mallows-Stuck, symmetric stable increments
    a <- jump_alpha
    u <- matrix(stats::runif(R * n, -pi / 2, pi / 2), R, n)
    w <- matrix(stats::rexp(R * n), R, n)
    dj <- sin(a * u) / cos(u)^(1 / a) * (cos((1 - a) * u) / w)^((1 - a) / a)
    dj <- dj * sqrt(jump_qv * BETA * T_YEARS / rowSums(dj^2))
    x <- x + cbind(0, t(apply(dj, 1, cumsum)))
  }
  if (noise != "none") {
    tnoise <- function() {
      e <- matrix(stats::rt(R * (n + 1), df), R, n + 1)
      cap <- 50 * sqrt(df / (df - 2))
      pmin(pmax(e, -cap), cap) / sqrt(df / (df - 2))
    }
    eps <- switch(noise,
      gaussian = matrix(stats::rnorm(R * (n + 1)), R, n + 1),
      t = tnoise(),
      mixture = matrix(stats::rnorm(R * (n + 1)), R, n + 1) + tnoise()
    )
    x <- x + 2 * sig * sdt * eps
  }
  if (tick > 0) x <- log(round(exp(x) / tick) * tick)
  x
}

# Runs `fun(y)` on `R` paths of a design, simulated in chunks of 50. With the
# same seed, designs that differ only in noise or rounding share their
# efficient price paths.
mc <- function(design, fun, seed, R = reps) {
  set.seed(seed)
  out <- list()
  left <- R
  while (left > 0) {
    m <- min(50L, left)
    paths <- do.call(sim_section4, c(list(R = m), design))
    out <- c(out, lapply(seq_len(m), function(r) fun(paths[r, ])))
    left <- left - m
  }
  do.call(rbind, out)
}

ajl_one <- function(y, kn = 100L, C = 5, null = "no_jump", aprime = "formula") {
  r <- ajl_test(y, ajl_options(kn = kn, trunc_C = C, null = null, aprime = aprime,
                               horizon = T_YEARS))
  r
}

pct <- function(x) round(100 * mean(x, na.rm = TRUE), 1)

# Part 1: A' constants -----------------------------------------------------------

part_aprime <- function() {
  cat("\n== A' constants: eq. 3.17 versus the authors' Matlab values ==\n")
  k <- ajl_constants()
  ka <- ajl_constants(aprime = "authors")
  comb <- function(a) a[, "gg"] - 2 * 2^2 * a[, "gh"] + 2^4 * a[, "hh"]
  cf <- comb(k@aprime)
  ca <- comb(ka@aprime)
  lam <- c(0, 1, 4, 16, 100, 1250, 2500, Inf)
  ratio <- vapply(lam, function(l) {
    if (is.infinite(l)) return(ca[5] / cf[5])
    sum(ca * l^(0:4)) / sum(cf * l^(0:4))
  }, 0)
  cat("Ratio of the no-jump variance (authors / formula) when (theta sigma)^2 / alpha^2 = lambda:\n")
  print(data.frame(lambda = lam, variance_ratio = round(ratio, 5)), row.names = FALSE)
  cat("(Paper design: lambda = kn^2 / 4 for Gaussian or t noise, kn^2 / 8 for the mixture.)\n")

  # Noisier designs than the paper's (constant volatility, i.i.d. Gaussian
  # noise with sd `mult` times that of the efficient 5-second return), where
  # the small-w terms of M* carry more weight. Much smaller lambda is not
  # usable at n = 23400: the noise-corrected power variations break down.
  source(file.path(pkg_root, "tests", "testthat", "helper-sim.R"))
  cat("\nNoisier designs (sim_log_prices, iv = 0.0032 per window, i.i.d. noise):\n")
  rows <- lapply(list(c(mult = 10, kn = 100), c(mult = 10, kn = 60)), function(d) {
    iv <- 0.0032
    res <- t(vapply(seq_len(reps), function(s) {
      y <- sim_log_prices(N_OBS, iv = iv, noise_sd = d[["mult"]] * sqrt(iv / N_OBS), seed = seed0 + s)
      a <- ajl_test(y, ajl_options(kn = d[["kn"]]))
      b <- ajl_test(y, ajl_options(kn = d[["kn"]], aprime = "authors"))
      c(S = a@statistic, se_f = a@std_error[[1]], se_a = b@std_error[[1]],
        rej_f = a@reject[[1]], rej_a = b@reject[[1]])
    }, numeric(5)))
    data.frame(noise_mult = d[["mult"]], kn = d[["kn"]], lambda = d[["kn"]]^2 / d[["mult"]]^2,
               mean_S = round(mean(res[, "S"]), 3), sd_S = round(stats::sd(res[, "S"]), 4),
               rms_se_formula = round(sqrt(mean(res[, "se_f"]^2)), 4),
               rms_se_authors = round(sqrt(mean(res[, "se_a"]^2)), 4),
               size_formula = pct(res[, "rej_f"]), size_authors = pct(res[, "rej_a"]),
               flips = sum(res[, "rej_f"] != res[, "rej_a"]))
  })
  print(do.call(rbind, rows), row.names = FALSE)
  cat(sprintf("(reps = %d per row)\n", reps))
}

# Part 2: size of the no-jump test (Table 1) ---------------------------------------

part_size <- function(ticks) {
  cat(sprintf("\n== Size (%%) of the 5%% no-jump test, continuous paths, rounding at %s (cf. Table 1) ==\n",
              paste(ticks, collapse = ", ")))
  grid <- expand.grid(noise = c("none", "gaussian", "t", "mixture"), tick = ticks,
                      stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(k) {
    t0 <- proc.time()[["elapsed"]]
    res <- mc(list(noise = grid$noise[k], tick = grid$tick[k]), function(y) {
      base <- ajl_one(y, null = "both")
      auth <- ajl_one(y, aprime = "authors")
      c(kn80 = ajl_one(y, kn = 80L)@reject[[1]],
        kn100 = base@reject[["no_jump"]],
        kn120 = ajl_one(y, kn = 120L)@reject[[1]],
        C4 = ajl_one(y, C = 4)@reject[[1]],
        C6 = ajl_one(y, C = 6)@reject[[1]],
        authors = auth@reject[[1]],
        S = base@statistic, z = base@z[["no_jump"]],
        var_ratio = auth@variance[[1]] / base@variance[["no_jump"]],
        flips = auth@reject[[1]] != base@reject[["no_jump"]],
        jump_null_rej = base@reject[["jump"]])
    }, seed = seed0 + round(1000 * grid$tick[k]) + k)
    data.frame(noise = grid$noise[k], tick = grid$tick[k],
               kn80 = pct(res[, "kn80"]), kn100 = pct(res[, "kn100"]), kn120 = pct(res[, "kn120"]),
               C4 = pct(res[, "C4"]), C6 = pct(res[, "C6"]), authors_Aprime = pct(res[, "authors"]),
               mean_S = round(mean(res[, "S"]), 3), mean_z = round(mean(res[, "z"]), 2),
               sd_z = round(stats::sd(res[, "z"]), 2),
               var_ratio = sprintf("%.5f", mean(res[, "var_ratio"])),
               flips = sum(res[, "flips"]), jump_null_rej = pct(res[, "jump_null_rej"]),
               secs = round(proc.time()[["elapsed"]] - t0))
  })
  out <- do.call(rbind, rows)
  print(out, row.names = FALSE)
  cat(sprintf(paste0("(reps = %d per row; kn80/kn100/kn120 at C = 5, C4/C6 at kn = 100; authors_Aprime and",
                     " var_ratio (authors / formula) at kn = 100, C = 5; flips = paths whose decision changes;",
                     " jump_null_rej = rejection rate of the jump null, eq. 3.27, on these continuous paths)\n"), reps))
  invisible(out)
}

# Part 2b: larger paired size study at the paper's headline tuning -----------------

part_paired <- function() {
  big <- 5L * reps
  cat(sprintf(paste0("\n== Size (%%) of the 5%% no-jump test, kn = 100, C = 5, no rounding: %d paths, ",
                     "the same efficient paths under each noise (cf. Table 1: 5.6, 5.7, 5.2, 5.4) ==\n"), big))
  noises <- c("none", "gaussian", "t", "mixture")
  res <- lapply(noises, function(nz) {
    mc(list(noise = nz), function(y) {
      r <- ajl_one(y)
      c(rej = r@reject[[1]], z = r@z[[1]])
    }, seed = seed0 + 500L, R = big)
  })
  rej <- vapply(res, function(m) m[, "rej"], numeric(big))
  z <- vapply(res, function(m) m[, "z"], numeric(big))
  colnames(rej) <- colnames(z) <- noises
  se <- function(x) sqrt(mean(x) * (1 - mean(x)) / length(x))
  print(data.frame(
    noise = colnames(rej), size = round(100 * colMeans(rej), 1),
    se = round(100 * apply(rej, 2, se), 1), mean_z = round(colMeans(z), 3),
    sd_z = round(apply(z, 2, stats::sd), 3),
    diff_vs_none = round(100 * (colMeans(rej) - mean(rej[, "none"])), 1),
    se_diff = round(100 * apply(rej - rej[, "none"], 2, stats::sd) / sqrt(big), 1)
  ), row.names = FALSE)
}

# Part 3: power (Table 2) and size of the jump-null test (Table 3A) -----------------

part_power <- function() {
  cat("\n== Power (%) of the 5% no-jump test, jump QV = beta T / 3, kn = 100 (cf. Table 2) ==\n")
  cat("   and size (%) of the 5% jump-null test on the same paths, C = 5 (cf. Table 3, Panel A)\n")
  grid <- expand.grid(alpha = c(0.5, 1, 1.5, 1.75), noise = c("none", "mixture"),
                      stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(k) {
    t0 <- proc.time()[["elapsed"]]
    res <- mc(list(noise = grid$noise[k], jump_alpha = grid$alpha[k]), function(y) {
      base <- ajl_one(y, null = "both")
      c(C4 = ajl_one(y, C = 4)@reject[[1]], C5 = base@reject[["no_jump"]],
        C6 = ajl_one(y, C = 6)@reject[[1]], S = base@statistic,
        jump_null_size = base@reject[["jump"]], z_j = base@z[["jump"]])
    }, seed = seed0 + 100 + k)
    data.frame(noise = grid$noise[k], activity = grid$alpha[k],
               power_C4 = pct(res[, "C4"]), power_C5 = pct(res[, "C5"]), power_C6 = pct(res[, "C6"]),
               mean_S = round(mean(res[, "S"]), 3),
               jump_null_size = pct(res[, "jump_null_size"]),
               mean_z_jump = round(mean(res[, "z_j"], na.rm = TRUE), 2),
               secs = round(proc.time()[["elapsed"]] - t0))
  })
  out <- do.call(rbind, rows)
  print(out, row.names = FALSE)
  cat(sprintf("(reps = %d per row)\n", reps))
  invisible(out)
}

# Part 4: size of the jump-null test with stronger jumps (Table 3B) -----------------

part_jumpnull <- function() {
  cat("\n== Size (%) of the 5% jump-null test, jump QV = 3 beta T, mixture noise, kn = 100, C = 5 (cf. Table 3, Panel B) ==\n")
  rows <- lapply(seq_along(c(0.5, 1, 1.5, 1.75)), function(k) {
    a <- c(0.5, 1, 1.5, 1.75)[k]
    t0 <- proc.time()[["elapsed"]]
    res <- mc(list(noise = "mixture", jump_alpha = a, jump_qv = 3), function(y) {
      r <- ajl_one(y, null = "both")
      c(size = r@reject[["jump"]], power = r@reject[["no_jump"]], S = r@statistic,
        z = r@z[["jump"]])
    }, seed = seed0 + 200 + k)
    data.frame(activity = a, jump_null_size = pct(res[, "size"]),
               no_jump_power = pct(res[, "power"]), mean_S = round(mean(res[, "S"]), 3),
               mean_z_jump = round(mean(res[, "z"], na.rm = TRUE), 2),
               secs = round(proc.time()[["elapsed"]] - t0))
  })
  out <- do.call(rbind, rows)
  print(out, row.names = FALSE)
  cat(sprintf("(reps = %d per row)\n", reps))
  invisible(out)
}

parts <- if (part == "all") c("aprime", "size", "rounding", "paired", "power", "jumpnull") else part
for (pt in parts) {
  switch(pt,
    aprime = part_aprime(),
    size = part_size(ticks = 0),
    rounding = part_size(ticks = 0.03),
    paired = part_paired(),
    power = part_power(),
    jumpnull = part_jumpnull(),
    stop("unknown part: ", pt)
  )
}
