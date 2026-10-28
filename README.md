# hf.econometrics

High-frequency econometrics in R: noise-robust jump tests and volatility
estimators, each implemented from its first-hand source paper. Every
function's help page maps the code to the paper's equations, and lists any
choice the paper leaves open under *Implementation choices*.

Version 0.2.0 is a rewrite of 0.1.0. See [NEWS.md](NEWS.md) for what changed
and why.

## Methods

| Method | Functions | Source |
|---|---|---|
| Lee–Mykland noise-robust jump test | `lm_test()`, `lm_options()`, `lm_blocks()`, `lm_holm()`, `lm_select_k()`, `lm_select_C()` | Lee & Mykland (2012) |
| Jump test S_J, both nulls | `aj_jump_test()`, `aj_jump_options()` | Aït-Sahalia & Jacod (2009a) |
| Finite vs infinite jump activity, S_FA and S′ | `aj_finite_activity_test()` | Aït-Sahalia & Jacod (2011) |
| Jump activity index β̂ | `aj_activity_index()` | Aït-Sahalia & Jacod (2009b) |
| "Spectrum" grids of S_J and S_FA | `aj_spectrum()` | Aït-Sahalia & Jacod (2012) |
| Pre-averaged jump test, both nulls | `ajl_test()`, `ajl_options()`, `ajl_constants()` | Aït-Sahalia, Jacod & Li (2012) |
| Integrated variance | `integrated_variance(method = "rv", "bv", "jlmpv2009", "jpv2010", "pv2009", "hp2013")` and the `iv_*()` functions | BNS (2004), JLMPV (2009), JPV (2010), Podolskij & Vetter (2009), Hautsch & Podolskij (2013) |
| Noise variance | `noise_variance()` | as above |
| Pre-averaging core | `preaverage()`, `preavg_power_variation()`, `preavg_vbar()`, `preavg_rho()`, `weight_tent()`, `weight_compress()` | Jacod, Podolskij & Vetter (2010); AJL (2012) |
| Data utilities | `parse_timestamps()`, `aggregate_simultaneous()`, `sample_previous_tick()`, `choose_frequency()`, `grid_coverage()`, `filter_outliers()`, `filter_bouncebacks()`, `split_windows()`, `drop_consecutive_flags()`, `log_returns()`, `locf()` | LM (2012, §6), companion paper (§4–5) |

All estimators are implemented in this package; nothing is delegated to
other packages. Package dependencies are `S7`, `stats` and `utils`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("YalDan/hf.econometrics")
```

To reproduce the exact environment used for development and the pipeline,
clone the repository and use the pinned lockfile:

```r
# in the repository root; renv activates automatically via .Rprofile
renv::restore()
```

`renv.lock` pins R 4.3.3 and every package, from a dated Posit Package
Manager snapshot (2026-10-28). The pipeline's `targets` needs the system
library GLPK (`libglpk40` on Debian/Ubuntu) for `igraph`.

## Quick start

```r
library(hf.econometrics)

# One window of 23,400 equally spaced observations: Brownian motion with
# integrated variance 1e-4 over the window, i.i.d. noise (sd 1e-4), and a
# 0.5% jump halfway through.
set.seed(1)
n <- 23400
y <- log(100) + cumsum(rnorm(n + 1, 0, 0.01 / sqrt(n))) + rnorm(n + 1, 0, 1e-4)
y[(n / 2):(n + 1)] <- y[(n / 2):(n + 1)] + 0.005

lm_test(y)                                 # Lee-Mykland: jump detected, block located
ajl_test(y, ajl_options(null = "both"))    # AJL: no-jump null rejected (eq. 3.26)
aj_jump_test(y)                            # Ait-Sahalia-Jacod S_J, both nulls
integrated_variance(y, "pv2009")           # jump- and noise-robust IV, ~1e-4

as.data.frame(lm_test(y))                  # every result has a one-row table
lm_blocks(lm_test(y))                      # block-level statistics and flags
```

Inputs are either a numeric vector of log prices or an `HFSeries` built with
`hf_series(time, price)`, which carries timestamps into the results.

## Typed options and results

Options and results are [S7](https://rconsortium.github.io/S7/) objects with
typed, validated properties. Each options object has defaults that reproduce
the paper's own setting. Invalid options fail when you construct them, not
in the middle of a computation:

```r
lm_options(alpha = 2)
#> Error: `alpha` must lie in (0, 1), not 2.
```

Arguments of exported functions are checked; failures are errors of class
`hf_input_error`. Results inherit from `HFResult`, which records the method
and its reference. They print a summary and convert to data frames with
`as.data.frame()`.

## Units

Each call analyses one window, such as a day or an hour. Integrated variances
are per window, and the statistics are unit-free. The Aït-Sahalia–Jacod
truncation defaults reproduce the authors' equity setting: `horizon = 1/252`
(one trading day, in years) and a volatility guess of `sigma = 0.25` per
year. For 24-hour markets, set `horizon = 1/365.25` and a realistic `sigma`.

## Batch pipeline

`pipeline/` holds a [`targets`](https://docs.ropensci.org/targets/) workflow.
It runs the companion paper's procedure, or the Lee–Mykland (2012, §6) data
handling, on raw trade exports, and it lives outside the package build.
Usage, configuration and how each step maps to its source are documented in
[pipeline/README.md](pipeline/README.md).

```sh
R CMD INSTALL .
cd pipeline && Rscript -e 'targets::tar_make()'
```

## Development

[DEVELOPMENT.md](DEVELOPMENT.md) describes the conventions, the testing
policy and the defects of 0.1.0 that must not come back.
- Regenerate the docs with `Rscript -e 'roxygen2::roxygenise()'`.
- Run the tests with `Rscript -e 'testthat::test_local()'`.
- Monte Carlo size and power studies are in `tests/simulations/`. They are
  slow, so testthat does not run them.

## References

- Aït-Sahalia, Y. and Jacod, J. (2009a). Testing for jumps in a discretely
  observed process. *Annals of Statistics* 37(1), 184–222.
- Aït-Sahalia, Y. and Jacod, J. (2009b). Estimating the degree of activity of
  jumps in high frequency data. *Annals of Statistics* 37(5A), 2202–2244.
- Aït-Sahalia, Y. and Jacod, J. (2011). Testing whether jumps have finite or
  infinite activity. *Annals of Statistics* 39(3), 1689–1719.
- Aït-Sahalia, Y. and Jacod, J. (2012). Analyzing the spectrum of asset
  returns: jump and volatility components in high frequency data. *Journal of
  Economic Literature* 50(4), 1007–1050.
- Aït-Sahalia, Y., Jacod, J. and Li, J. (2012). Testing for jumps in noisy
  high frequency data. *Journal of Econometrics* 168(2), 207–222.
- Barndorff-Nielsen, O. E. and Shephard, N. (2004). Power and bipower
  variation with stochastic volatility and jumps. *Journal of Financial
  Econometrics* 2(1), 1–37.
- Hautsch, N. and Podolskij, M. (2013). Preaveraging-based estimation of
  quadratic variation in the presence of noise and jumps: theory,
  implementation, and empirical evidence. *Journal of Business & Economic
  Statistics* 31(2), 165–183.
- Jacod, J., Li, Y., Mykland, P. A., Podolskij, M. and Vetter, M. (2009).
  Microstructure noise in the continuous case: the pre-averaging approach.
  *Stochastic Processes and their Applications* 119(7), 2249–2276.
- Jacod, J., Podolskij, M. and Vetter, M. (2010). Limit theorems for moving
  averages of discretized processes plus noise. *Annals of Statistics* 38(3),
  1478–1545.
- Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and market
  microstructure noise. *Journal of Econometrics* 168(2), 396–406.
- Podolskij, M. and Vetter, M. (2009). Estimation of volatility functionals in
  the simultaneous presence of microstructure noise and jumps. *Bernoulli*
  15(3), 634–658.
- Saef, D., Nagy, O., Sizov, S. and Härdle, W. K. (2021). Understanding jumps
  in high frequency digital asset markets. arXiv:2110.09429.
