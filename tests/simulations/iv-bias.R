# Monte Carlo study of the integrated-variance estimators (not run by testthat).
#
# One window of n = 86400 one-second log prices, constant volatility with
# IV = 9e-4 (sigma = 3% per window), additive noise with standard deviation
# 5e-5 or 2e-4, with and without one 5% jump at a random time.
# * "iid" cells: i.i.d. Gaussian noise, all estimators on the full series.
# * "ma9" cells: MA(9) noise U_t = (omega / sqrt(10)) sum_{l=0}^{9} e_{t-l}
#   (marginal sd omega), and the estimators applied to every 10th price, where
#   the noise is i.i.d. again (n = 8640). As references, the HP (2013)
#   dependent-noise estimator with q = 9 and PV (2009) are also applied to the
#   full series.
# Reported: mean and sd of sqrt(max(IV-hat, 0)) over the seeds (true 0.0300)
# and the number of negative estimates.
#
# Usage, from the package root:
#   Rscript tests/simulations/iv-bias.R [noise = iid|ma9] [R = 30] [out.csv]

args <- commandArgs(trailingOnly = TRUE)
noise_type <- if (length(args) >= 1L) args[[1L]] else "iid"
R <- if (length(args) >= 2L) as.integer(args[[2L]]) else 30L
out_file <- if (length(args) >= 3L) args[[3L]] else NA_character_
stopifnot(noise_type %in% c("iid", "ma9"), R >= 2L)

suppressMessages(devtools::load_all(".", quiet = TRUE))
source(file.path("tests", "testthat", "helper-sim.R"))

n <- 86400L
iv_true <- 9e-4
noise_sds <- c(5e-5, 2e-4)
jump_size <- 0.05

estimators <- function(sparse_lag) {
  # The theta search of HP (2013, Sec. 2.4) is the slow step: run it once per
  # path. Its criterion does not depend on the estimator computed afterwards,
  # so the bipower estimator reuses the selected kn.
  memo <- new.env()
  mse_fit <- function(y) {
    if (is.null(memo$fit)) memo$fit <- iv_hp2013(y, theta = "mse", sparse_lag = sparse_lag)
    memo$fit
  }
  list(
    "rv" = function(y) iv_realized(y),
    "bv (BNS 2004)" = function(y) iv_bipower(y),
    "jlmpv2009 adj, theta 1/3" = function(y) iv_jlmpv2009(y),
    "jlmpv2009 eq 3.6" = function(y) iv_jlmpv2009(y, adjust = FALSE),
    "jlmpv2009 theta optimal" = function(y) iv_jlmpv2009(y, theta = "optimal"),
    "jpv2010 eq 3.5" = function(y) iv_jpv2010(y),
    "jpv2010 finite sample" = function(y) iv_jpv2010(y, constants = "finite_sample"),
    "pv2009 default (fs, acov)" = function(y) iv_pv2009(y),
    "pv2009 fs, noise 3.5" = function(y) iv_pv2009(y, noise = "pv2009"),
    "pv2009 asym, acov" = function(y) iv_pv2009(y, constants = "asymptotic"),
    "pv2009 literal (asym, 3.5)" = function(y) iv_pv2009(y, constants = "asymptotic", noise = "pv2009"),
    "hp2013 C_ta theta 0.6" = function(y) iv_hp2013(y, theta = 0.6),
    "hp2013 C_t theta 0.6 unadj" = function(y) iv_hp2013(y, theta = 0.6, adjust = FALSE),
    "hp2013 C_ta(1) theta 0.6" = function(y) iv_hp2013(y, theta = 0.6, noise = "dependent", q = 1),
    "hp2013 C_ta theta mse" = function(y) mse_fit(y),
    "hp2013 BT_ta theta 0.6" = function(y) iv_hp2013(y, estimator = "bipower", theta = 0.6),
    "hp2013 BT_ta theta 0.4" = function(y) iv_hp2013(y, estimator = "bipower", theta = 0.4),
    "hp2013 BT_ta theta 0.2" = function(y) iv_hp2013(y, estimator = "bipower", theta = 0.2),
    "hp2013 BT_ta theta mse" = function(y) {
      iv_hp2013(y, estimator = "bipower", kn = min(mse_fit(y)@tuning$kn, length(y) %/% 2L))
    }
  )
}

full_series_refs <- list(
  "[full] hp2013 C_ta(9) theta 0.6" = function(y) iv_hp2013(y, theta = 0.6, noise = "dependent", q = 9),
  "[full] pv2009 default" = function(y) iv_pv2009(y)
)

ma_noise <- function(n_obs, omega, q, seed) {
  set.seed(seed + 7777L)
  e <- stats::rnorm(n_obs + q)
  as.numeric(stats::filter(e, rep(1, q + 1L), sides = 1L))[-seq_len(q)] *
    omega / sqrt(q + 1)
}

one_path <- function(seed, sd, jump) {
  jumps <- if (jump) cbind(1L + (seed * 7919L) %% (n - 2L), jump_size) else NULL
  if (noise_type == "iid") {
    y <- sim_log_prices(n, iv = iv_true, noise_sd = sd, jumps = jumps, seed = seed)
    est <- estimators(sparse_lag = 600L) # 10 minutes of 1-second returns
    ys <- list(sub = y)
  } else {
    y <- sim_log_prices(n, iv = iv_true, noise_sd = 0, jumps = jumps, seed = seed) +
      ma_noise(n + 1L, sd, 9L, seed)
    est <- c(estimators(sparse_lag = 60L), full_series_refs) # 10 min of 10-s returns
    ys <- list(sub = y[seq(1L, n + 1L, by = 10L)], full = y)
  }
  vapply(names(est), function(nm) {
    yy <- if (startsWith(nm, "[full]")) ys$full else ys$sub
    suppressWarnings(est[[nm]](yy)@value)
  }, numeric(1))
}

n_est <- length(if (noise_type == "iid") estimators(600L) else
  c(estimators(60L), full_series_refs))
cells <- expand.grid(sd = noise_sds, jump = c(FALSE, TRUE))
rows <- list()
t_start <- Sys.time()
for (k in seq_len(nrow(cells))) {
  sd <- cells$sd[k]
  jump <- cells$jump[k]
  vals <- vapply(seq_len(R), function(s) one_path(s, sd, jump), numeric(n_est))
  vol <- sqrt(pmax(vals, 0))
  rows[[k]] <- data.frame(
    noise = noise_type, noise_sd = sd, jump = jump, estimator = rownames(vals),
    mean_vol = rowMeans(vol), sd_vol = apply(vol, 1L, stats::sd),
    n_negative = rowSums(vals < 0), R = R, row.names = NULL
  )
  cat(sprintf("cell %d/%d done (%.1f min)\n", k, nrow(cells),
              as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
}
res <- do.call(rbind, rows)

wide <- reshape(
  transform(res, cell = sprintf("sd=%.0e%s", noise_sd, ifelse(jump, " +jump", "")),
            txt = sprintf("%.4f (%.4f)%s", mean_vol, sd_vol,
                          ifelse(n_negative > 0, sprintf(" [%d<0]", n_negative), "")))[
    , c("estimator", "cell", "txt")],
  idvar = "estimator", timevar = "cell", direction = "wide"
)
names(wide) <- sub("^txt\\.", "", names(wide))
options(width = 200)
cat(sprintf("\nNoise: %s; R = %d seeds per cell; true volatility 0.0300; mean (sd) of sqrt(IV-hat)\n",
            noise_type, R))
print(wide, row.names = FALSE, right = FALSE)
if (!is.na(out_file)) utils::write.csv(res, out_file, row.names = FALSE)
