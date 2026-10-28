# Monte Carlo size and power of the Lee and Mykland (2012) jump test.
#
# Run from the package root (about two minutes for 200 replications):
#   Rscript tests/simulations/lm-size-power.R [replications] [seed]
#
# Design: one day of n = 86400 equally spaced observations, integrated
# variance 9e-4 per day (3% daily volatility), and three noise processes:
# i.i.d. normal noise with sd 5e-5 and with sd 2e-4, and equally weighted
# MA(9) noise with sd 1e-4 (9-dependent, so the true k is 10). The plug-in
# variance uses the true integrated variance (oracle), which isolates the lag
# rule, q_hat, the blocks and the Gumbel limit from the volatility estimator.
# Reported per noise process:
#   1. the share of no-jump days rejected at alpha = 0.01, with the plug-in
#      variance and with the (non-paper) empirical variance, and the
#      distribution of the selected k; as diagnostics, the variance of chi
#      (1 by Lemma 1), the mean and sd of xi (Gumbel: 0.58 and 1.28), the
#      size at 5%, and the exact size of the Theorem 1 threshold for the
#      maximum of N independent |N(0, 1)| (the finite-N accuracy of the
#      Gumbel approximation itself);
#   2. the power for one jump of 0.3% and of 1% at a uniform random time with
#      a random sign, how often the block with the largest |chi| brackets the
#      jump (time < tau <= time_end), and how often all flagged blocks do so
#      with the sign of the jump;
#   3. the step-down procedure of Section 6.2 across the 24 hourly windows of
#      each no-jump day (n = 3600 per window): the per-window rejection rate
#      and the share of days with at least one rejection at an overall 1%.
# If integrated_variance() is available, the feasible test (Podolskij-Vetter
# volatility, the paper's default) is run on the no-jump days as well.

suppressMessages(pkgload::load_all(".", quiet = TRUE))
source(file.path("tests", "testthat", "helper-sim.R"))

args <- commandArgs(trailingOnly = TRUE)
R <- if (length(args) >= 1L) as.integer(args[1L]) else 200L
seed0 <- if (length(args) >= 2L) as.integer(args[2L]) else 20120000L

n <- 86400
iv <- 9e-4
alpha <- 0.01
n_hours <- 24L
jump_sizes <- c(0.003, 0.01)
feasible <- exists("integrated_variance", mode = "function")

# Equally weighted MA(order) noise with marginal sd `sd` (continues the
# random-number stream of sim_log_prices()).
ma_noise <- function(n, sd, order) {
  e <- stats::rnorm(n + order)
  as.numeric(stats::filter(e, rep(1, order + 1), sides = 1))[-seq_len(order)] *
    sd / sqrt(order + 1)
}

settings <- list(
  list(label = "iid noise, sd 5e-5", sd = 5e-5, order = 0L),
  list(label = "iid noise, sd 2e-4", sd = 2e-4, order = 0L),
  list(label = "MA(9) noise, sd 1e-4 (true k = 10)", sd = 1e-4, order = 9L)
)

one_day <- function(setting, seed) {
  y <- sim_log_prices(n, iv = iv, noise_sd = if (setting$order == 0L) setting$sd else 0,
                      seed = seed)
  if (setting$order > 0L) y <- y + ma_noise(n + 1, setting$sd, setting$order)
  tau <- sample.int(n, 1L) # the jump occurs between ticks tau - 1 and tau
  sgn <- sample(c(-1, 1), 1L)
  oracle <- lm_options(iv = iv, alpha = alpha)

  null <- lm_test(y, oracle)
  out <- c(k = null@k, M = null@M, N = n %/% (null@k * null@M), q_hat = null@q_hat,
           chi_var = stats::var(lm_blocks(null)$chi), xi = null@statistic,
           p = null@p_value, reject = null@reject,
           reject_emp = lm_test(y, lm_options(variance = "empirical", alpha = alpha))@reject,
           reject_feasible = if (feasible) lm_test(y, lm_options(alpha = alpha))@reject else NA)

  for (s in seq_along(jump_sizes)) {
    yj <- y
    yj[(tau + 1):(n + 1)] <- yj[(tau + 1):(n + 1)] + sgn * jump_sizes[s]
    res <- lm_test(yj, oracle)
    b <- lm_blocks(res)
    inside <- b$index < tau & tau <= b$time_end
    hit <- b$jump != 0L
    out[paste0(c("power", "argmax", "flag_all", "n_flag"), s)] <- c(
      res@reject, inside[which.max(abs(b$chi))],
      if (any(hit)) all(inside[hit] & b$jump[hit] == sgn) else NA, sum(hit)
    )
  }

  len <- n %/% n_hours
  hours <- lapply(seq_len(n_hours), function(h) {
    lm_test(y[((h - 1) * len + 1):(h * len + 1)],
            lm_options(iv = iv / n_hours, alpha = alpha))
  })
  p <- vapply(hours, function(t) t@p_value, numeric(1))
  c(out, hour_reject = mean(p < alpha), holm_any = any(lm_holm(p, alpha)$reject),
    hour_k1 = mean(vapply(hours, function(t) t@k == 1L, logical(1))),
    hour_M1 = mean(vapply(hours, function(t) t@M == 1L, logical(1))))
}

pct <- function(x) sprintf("%.1f%%", 100 * mean(x, na.rm = TRUE))
mc_se <- function(x) sprintf("%.1f%%", 100 * sqrt(mean(x) * (1 - mean(x)) / length(x)))
# Exact size of the Theorem 1 threshold for the maximum of N independent
# |N(0, 1)|: the finite-N benchmark for the Gumbel approximation itself.
iid_size <- function(N, a) {
  c_N <- sqrt(2 * log(N))
  x <- c_N - (log(pi) + log(log(N))) / (2 * c_N) - log(-log(1 - a)) / c_N
  1 - (2 * stats::pnorm(x) - 1)^N
}

cat(sprintf("Lee-Mykland (2012) Monte Carlo: R = %d days, n = %d, IV = %g, alpha = %g, oracle IV\n",
            R, n, iv, alpha))
started <- proc.time()[["elapsed"]]
for (setting in settings) {
  res <- do.call(rbind, lapply(seq_len(R), function(r) one_day(setting, seed0 + r)))
  res <- as.data.frame(res)
  k_tab <- table(res$k)
  cat(sprintf("\n== %s ==\n", setting$label))
  cat(sprintf("  size, daily window: plug-in %s (MC s.e. %s) | empirical variance %s%s\n",
              pct(res$reject), mc_se(res$reject), pct(res$reject_emp),
              if (feasible) sprintf(" | feasible (pv2009) %s", pct(res$reject_feasible)) else ""))
  N_med <- median(res$N)
  cat(sprintf("  null diagnostics: var(chi) %.3f (Lemma 1: 1) | xi mean %.2f, sd %.2f (Gumbel 0.58, 1.28) | size at 5%%: %s | iid-normal benchmark at N = %g: %.2f%% at 1%%, %.2f%% at 5%%\n",
              mean(res$chi_var), mean(res$xi), stats::sd(res$xi), pct(res$p < 0.05), N_med,
              100 * iid_size(N_med, 0.01), 100 * iid_size(N_med, 0.05)))
  cat(sprintf("  k selected: %s\n",
              paste(sprintf("%s: %s", names(k_tab), sprintf("%.0f%%", 100 * k_tab / R)),
                    collapse = ", ")))
  cat(sprintf("  M: median %g [%g, %g] | q_hat: mean %.3g (true %.3g)\n", median(res$M),
              min(res$M), max(res$M), mean(res$q_hat), setting$sd))
  for (s in seq_along(jump_sizes)) {
    cat(sprintf("  jump %.1f%%: power %s | argmax block brackets the jump %s | all flagged blocks bracket it, right sign %s | flagged blocks per day %.2f\n",
                100 * jump_sizes[s], pct(res[[paste0("power", s)]]), pct(res[[paste0("argmax", s)]]),
                pct(res[[paste0("flag_all", s)]]), mean(res[[paste0("n_flag", s)]])))
  }
  cat(sprintf("  24 hourly windows: per-window rejection %s | days with a Holm rejection %s (MC s.e. %s) | windows with k = 1: %s, with M = 1: %s\n",
              pct(res$hour_reject), pct(res$holm_any), mc_se(res$holm_any),
              pct(res$hour_k1), pct(res$hour_M1)))
}
cat(sprintf("\nelapsed: %.0f s\n", proc.time()[["elapsed"]] - started))
