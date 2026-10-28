# Monte Carlo size and power of the Ait-Sahalia and Jacod statistics:
# S_J (AJ 2009a), S_FA and S'_n (AJ 2011) and beta-hat (AJ 2009b).
#
# Run from the package root:   Rscript tests/simulations/aj-size-power.R
# Not run by testthat. Each design uses R windows of one trading day (6.5 h,
# 1/252 year) with a 25% annual volatility; R defaults to 200 and can be set
# with the environment variable AJ_SIM_REPS. Runtime: about two minutes.

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(hf.econometrics)
}
source(file.path("tests", "testthat", "helper-sim.R"))

R <- as.integer(Sys.getenv("AJ_SIM_REPS", "200"))
horizon <- 1 / 252
sigma <- 0.25
n1 <- 23400 # 1-second sampling
iv <- sigma^2 * horizon
t_start <- proc.time()

# Simulators -----------------------------------------------------------------

# Symmetric beta-stable increments over dt, standardized as in AJ (2009b,
# Section 5): E exp(iu Y_t) = exp(-t |u|^beta / 2) (Chambers-Mallows-Stuck).
rstable_aj <- function(n, beta, dt) {
  v <- stats::runif(n, -pi / 2, pi / 2)
  w <- stats::rexp(n)
  s <- if (abs(beta - 1) < 1e-12) tan(v) else
    sin(beta * v) / cos(v)^(1 / beta) * (cos((1 - beta) * v) / w)^((1 - beta) / beta)
  2^(-1 / beta) * dt^(1 / beta) * s
}
c_beta <- function(b) gamma(b + 1) * sin(pi * b / 2) / (2 * pi) # AJ 2009b, eq. (38)

# Brownian motion plus theta * Y, with theta set so that the jump increment
# exceeds `alpha_sd` standard deviations of the Brownian increment with
# probability `tail` (leading term of the stable tail, AJ 2009b eq. 37).
sim_stable <- function(n, beta, tail, alpha_sd, seed) {
  set.seed(seed)
  dt <- horizon / n
  x <- alpha_sd * sigma * sqrt(dt)
  theta <- (tail * beta * x^beta / (2 * c_beta(beta) * dt))^(1 / beta)
  c(0, cumsum(stats::rnorm(n, 0, sigma * sqrt(dt)) + theta * rstable_aj(n, beta, dt)))
}

# Compound Poisson jumps as in AJ (2009a, Table 2): lambda jumps per day,
# sizes uniform on [-2, -1] U [1, 2] times J_S, with sigma^2 + (7/3) J_S^2
# lambda = 0.4^2 per year and 25% of the variance from the Brownian part.
# Paths without a jump are redrawn (the null is conditional on jumps).
sim_cp <- function(n, lambda, seed, at_least_one = TRUE) {
  set.seed(seed)
  sig <- 0.2
  js <- sqrt((0.4^2 - sig^2) / ((7 / 3) * lambda / horizon))
  nj <- 0
  while (nj == 0) {
    nj <- stats::rpois(1, lambda)
    if (!at_least_one) break
  }
  jumps <- if (nj > 0) {
    cbind(sample.int(n, nj), js * stats::runif(nj, 1, 2) * sample(c(-1, 1), nj, TRUE))
  }
  sim_log_prices(n, iv = sig^2 * horizon, jumps = jumps, seed = seed + 1e6)
}

# Stochastic volatility without jumps: CIR variance (kappa = 5, long-run
# volatility 0.25, vol-of-vol 0.5, as in AJ 2009a Table 1 but without
# leverage), started from its stationary law, updated every minute.
sim_sv <- function(n, seed) {
  set.seed(seed)
  kappa <- 5
  theta <- sigma^2
  xi <- 0.5
  steps <- 390
  h <- horizon / steps
  v <- numeric(steps)
  v[1] <- stats::rgamma(1, shape = 2 * kappa * theta / xi^2, rate = 2 * kappa / xi^2)
  for (i in seq_len(steps - 1L)) {
    v[i + 1] <- abs(v[i] + kappa * (theta - v[i]) * h + xi * sqrt(v[i] * h) * stats::rnorm(1))
  }
  vol <- sqrt(v[ceiling(seq_len(n) * steps / n)])
  c(0, cumsum(stats::rnorm(n, 0, vol * sqrt(horizon / n))))
}

subsample <- function(x, m) x[seq(1, length(x), by = m)]
rate <- function(x) mean(x, na.rm = TRUE)
line <- function(...) cat(sprintf(...), "\n", sep = "")

# A. AJ (2009a) S_J --------------------------------------------------------------

line("== A. Ait-Sahalia and Jacod (2009a) S_J(p = 4, k = 2), R = %d windows ==", R)
opt_tr <- aj_jump_options()
opt_tr10 <- aj_jump_options(level = 0.10)
opt_mp <- aj_jump_options(variance = "multipower")
opt_sd <- aj_jump_options(truncation = "sd", alpha = 7)
opt_ch <- aj_jump_options(jump_critical = "chebyshev")

# A1. Null of no jumps: Brownian motion and stochastic volatility.
for (design in c("BM", "SV")) {
  for (m in c(1, 5)) {
    res <- t(vapply(seq_len(R), function(r) {
      x <- if (design == "BM") sim_log_prices(n1, iv = iv, seed = r) else sim_sv(n1, r)
      x <- subsample(x, m)
      a <- aj_jump_test(x, opt_tr)
      c(S = a@statistic, rej5 = a@reject_no_jumps,
        rej10 = aj_jump_test(x, opt_tr10)@reject_no_jumps,
        mp = aj_jump_test(x, opt_mp)@reject_no_jumps,
        sd = aj_jump_test(x, opt_sd)@reject_no_jumps,
        pow = a@reject_jumps)
    }, numeric(6)))
    line(paste0("A1 H0 no jumps, %s, Delta = %ds: mean S = %.3f | size 5%%: truncated %.3f, ",
                "multipower %.3f, sd-trunc %.3f | size 10%%: %.3f | power of jump-null test %.3f"),
         design, m, mean(res[, "S"]), rate(res[, "rej5"]), rate(res[, "mp"]),
         rate(res[, "sd"]), rate(res[, "rej10"]), rate(res[, "pow"]))
  }
}

# A2. Null of jumps: compound Poisson, at least one jump (AJ 2009a, Table 2
# reports 5% sizes of 0.056, 0.054, 0.053 for lambda = 1 and 0.057, 0.059,
# 0.071 for lambda = 5 at 1, 4 and 15 seconds).
for (lambda in c(1, 5)) {
  for (m in c(1, 5, 15)) {
    res <- t(vapply(seq_len(R), function(r) {
      x <- subsample(sim_cp(n1, lambda, seed = 1000 + r), m)
      a <- aj_jump_test(x, opt_tr)
      c(S = a@statistic, rej5 = a@reject_jumps,
        rej10 = aj_jump_test(x, opt_tr10)@reject_jumps,
        cheb = aj_jump_test(x, opt_ch)@reject_jumps,
        pow = a@reject_no_jumps, z = a@z_jumps)
    }, numeric(6)))
    line(paste0("A2 H0 jumps, CP lambda = %d/day, Delta = %ds: mean S = %.3f | size 5%%: ",
                "normal (40) %.3f, Chebyshev (39) %.3f | size 10%%: %.3f | sd(z) = %.2f | ",
                "power of no-jump test %.3f"),
         lambda, m, mean(res[, "S"]), rate(res[, "rej5"]), rate(res[, "cheb"]),
         rate(res[, "rej10"]), stats::sd(res[, "z"]), rate(res[, "pow"]))
  }
}

# A3. Power against small jumps: one jump of 4, 8 or 16 standard deviations
# of the 1-second Brownian increment.
for (size_sd in c(4, 8, 16)) {
  rej <- vapply(seq_len(R), function(r) {
    j <- size_sd * sqrt(iv / n1)
    x <- sim_log_prices(n1, iv = iv, seed = 2000 + r, jumps = cbind(n1 %/% 2, j))
    aj_jump_test(x, opt_tr)@reject_no_jumps
  }, logical(1))
  line("A3 one jump of %d sd, Delta = 1s: power of the no-jump test %.3f", size_sd, rate(rej))
}

# A4. Additive noise: S_J -> 1/k (AJ 2009a eq. 43, AJ 2012 eq. 40).
for (noise in c(1e-4, 5e-4)) {
  res <- t(vapply(seq_len(R), function(r) {
    x <- sim_log_prices(n1, iv = iv, seed = 3000 + r, noise_sd = noise)
    c(aj_jump_test(x)@statistic, aj_jump_test(x, aj_jump_options(k = 3))@statistic)
  }, numeric(2)))
  line(paste0("A4 noise sd = %.0e (%.1f x the 1s Brownian sd), continuous X: mean S(k=2) = %.3f ",
              "(1/k = 0.5), mean S(k=3) = %.3f (1/k = 0.333)"),
       noise, noise / sqrt(iv / n1), mean(res[, 1]), mean(res[, 2]))
}

# B. AJ (2011) finite vs infinite activity -------------------------------------

line("\n== B. Ait-Sahalia and Jacod (2011), alpha = 7 sd, R = %d windows ==", R)
fa <- aj_finite_activity_options()
fa10 <- aj_finite_activity_options(level = 0.10)
ia <- aj_finite_activity_options(null = "infinite")
ia10 <- aj_finite_activity_options(null = "infinite", level = 0.10)
run_b <- function(gen, m) {
  t(vapply(seq_len(R), function(r) {
    x <- subsample(gen(r), m)
    s <- aj_finite_activity_test(x, fa)
    s2 <- aj_finite_activity_test(x, ia)
    c(S = s@statistic, rejf5 = s@reject, rejf10 = aj_finite_activity_test(x, fa10)@reject,
      S2 = s2@statistic, reji5 = s2@reject,
      reji10 = aj_finite_activity_test(x, ia10)@reject)
  }, numeric(6)))
}
designs_b <- list(
  "BM only (finite)" = function(r) sim_log_prices(n1, iv = iv, seed = 4000 + r),
  "BM + CP 5/day (finite)" = function(r) sim_cp(n1, 5, seed = 5000 + r),
  "BM + Cauchy, tail 1% (infinite)" = function(r) sim_stable(n1, 1, 0.01, 4, 6000 + r),
  "BM + Cauchy, tail 5% (infinite)" = function(r) sim_stable(n1, 1, 0.05, 4, 7000 + r),
  "BM + Cauchy, tail 10% (infinite)" = function(r) sim_stable(n1, 1, 0.10, 4, 8000 + r),
  "BM + 1.5-stable, tail 5% (infinite)" = function(r) sim_stable(n1, 1.5, 0.05, 4, 9000 + r)
)
for (nm in names(designs_b)) {
  for (m in c(1, 5)) {
    res <- run_b(designs_b[[nm]], m)
    line(paste0("B %-36s Delta = %ds: S_FA mean %.3f, reject H0 finite: 5%% %.3f, 10%% %.3f | ",
                "S' mean %.3f, reject H0 infinite: 5%% %.3f, 10%% %.3f"),
         nm, m, mean(res[, "S"], na.rm = TRUE), rate(res[, "rejf5"]), rate(res[, "rejf10"]),
         mean(res[, "S2"], na.rm = TRUE), rate(res[, "reji5"]), rate(res[, "reji10"]))
  }
}

# C. AJ (2009b) beta-hat -----------------------------------------------------------

line("\n== C. Ait-Sahalia and Jacod (2009b) beta-hat, Delta = 1s, R = %d windows ==", R)
designs_c <- list(
  list(alpha = 5, tail = 0.01, label = "alpha = 5, alpha' = 10 sd (Table 1)"),
  list(alpha = 7, tail = 0.01, label = "alpha = 7, alpha' = 14 sd (default)")
)
for (dc in designs_c) {
  opt <- aj_activity_index_options(alpha = dc$alpha)
  opt_pl <- aj_activity_index_options(alpha = dc$alpha, bias_correction = "plugin")
  opt_fr <- aj_activity_index_options(alpha = dc$alpha, estimator = "frequency")
  for (beta in c(0.5, 1, 1.5)) {
    res <- t(vapply(seq_len(R), function(r) {
      x <- sim_stable(n1, beta, dc$tail, dc$alpha, 10000 + r)
      b <- suppressWarnings(aj_activity_index(x, opt))
      c(est = b@estimate, se = b@se, cover = b@ci[1] <= beta & beta <= b@ci[2],
        bc = b@estimate_bc, pl = suppressWarnings(aj_activity_index(x, opt_pl))@estimate_bc,
        fr = suppressWarnings(aj_activity_index(x, opt_fr))@estimate,
        U = b@details$count)
    }, numeric(7)))
    line(paste0("C %s, tail 1%%, beta = %.2f: beta-hat mean %.3f sd %.3f | mean se %.3f | ",
                "95%% coverage %.3f | bias-corrected: regression %.3f, plug-in %.3f | ",
                "frequency estimator mean %.3f | mean count U(alpha) %.0f"),
         dc$label, beta, mean(res[, "est"]), stats::sd(res[, "est"]), mean(res[, "se"], na.rm = TRUE),
         rate(res[, "cover"]), mean(res[, "bc"], na.rm = TRUE), mean(res[, "pl"], na.rm = TRUE),
         mean(res[, "fr"], na.rm = TRUE), mean(res[, "U"]))
  }
}
res0 <- vapply(seq_len(R), function(r) {
  suppressWarnings(aj_activity_index(sim_cp(n1, 5, seed = 12000 + r)))@estimate
}, numeric(1))
line("C compound Poisson (beta = 0), 5 jumps/day: beta-hat mean %.3f sd %.3f", mean(res0), stats::sd(res0))

# D. One spectrum per regime ---------------------------------------------------------

line("\n== D. Ait-Sahalia and Jacod (2012) spectrum: median S_J and S_FA over p, k, alpha ==")
spec_designs <- list(
  continuous = sim_log_prices(n1, iv = iv, seed = 1),
  jumps = sim_cp(n1, 5, seed = 2),
  noise = sim_log_prices(n1, iv = iv, seed = 3, noise_sd = 5e-4),
  infinite = sim_stable(n1, 1, 0.05, 4, 4)
)
for (nm in names(spec_designs)) {
  sp <- aj_spectrum(spec_designs[[nm]], base_interval = 1)
  med <- stats::aggregate(value ~ statistic + delta, data = sp[sp$k == 2 & sp$p == 4, ],
                          FUN = stats::median)
  line("D %-10s k = 2, p = 4: S_J %s | S_FA %s", nm,
       paste(sprintf("%gs %.2f", med$delta[med$statistic == "SJ"], med$value[med$statistic == "SJ"]),
             collapse = ", "),
       paste(sprintf("%.2f", med$value[med$statistic == "SFA"]), collapse = ", "))
}
line("\nElapsed: %.0f s", (proc.time() - t_start)[["elapsed"]])
