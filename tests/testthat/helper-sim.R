# Simulation helpers shared by the tests.
#
# sim_log_prices() returns n + 1 observed log prices on an equally spaced grid
# over one window: X = log(p0) + Brownian motion with integrated variance `iv`
# over the window, plus permanent jumps, plus i.i.d. N(0, noise_sd^2) noise.
# `jumps` is a two-column matrix (increment index in 1..n, jump size).
sim_log_prices <- function(n, iv = 9e-4, noise_sd = 0, jumps = NULL,
                           seed = 1, p0 = 40000) {
  set.seed(seed)
  x <- log(p0) + c(0, cumsum(stats::rnorm(n, 0, sqrt(iv / n))))
  if (!is.null(jumps)) {
    for (k in seq_len(nrow(jumps))) {
      i <- jumps[k, 1]
      x[(i + 1):(n + 1)] <- x[(i + 1):(n + 1)] + jumps[k, 2]
    }
  }
  x + stats::rnorm(n + 1, 0, noise_sd)
}
