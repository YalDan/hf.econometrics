# Development guide

`hf.econometrics` collects high-frequency econometric methods. Each method is
implemented **from its first-hand source paper**, and the code can be traced
back to that paper equation by equation. When a paper offers several
variants, all of them are kept as options.

## Layout

```
R/                      package code: methods + reusable data utilities
  class-*.R             shared S7 classes (HFSeries, HFResult, IVEstimate)
  utils-checks.R        internal argument checks (error class `hf_input_error`)
  utils-math.R          shared maths (abs_normal_moment, gumbel_quantile, ...)
  weights.R             pre-averaging weight functions (S7 WeightFunction)
  preaveraging.R        JPV (2010) / AJL (2012) pre-averaging core
  iv-*.R                integrated-variance estimators -> integrated_variance()
  lm2012.R              Lee & Mykland (2012)
  aj-*.R                Ait-Sahalia & Jacod (2009a, 2009b, 2011, 2012)
  ajl2012*.R            Ait-Sahalia, Jacod & Li (2012)
  data-*.R              reusable data utilities (gridding, filters, windows)
tests/testthat/         sparse unit tests (see "Testing policy")
pipeline/               targets pipeline for batch processing (not in the package build)
renv.lock, Dockerfile   reproducible environment
```

## Conventions

**Inputs.** Every method reads its data through `log_prices(x)` and, if it
needs timestamps, `obs_times(x)`. Both are S7 generics with methods for
numeric vectors and `HFSeries`, so any other input type fails dispatch with
an informative error.

**Validation.** Every argument of every exported function is checked with
the internal helpers in `R/utils-checks.R` (`check_number()`,
`check_count()`, `check_choice()`, `check_class()`, ...). Failures raise
errors of class `hf_input_error`. Warnings use `hf_warn()`, which signals
class `hf_warning`.

**S7 classes.**
- *Options* are typed S7 classes named `<Method>Options`. Each has a
  constructor `<method>_options(...)` whose defaults reproduce the paper's
  own empirical or simulation setting, and a validator that rejects
  inconsistent combinations.
- *Results* inherit from `HFResult`, which records `@method` and a short
  citation in `@reference`. Every result class gets a `print` method and an
  `as.data.frame` method, so that the pipeline can bind results into tables.
- Register methods for base generics with
  `method(print, Class) <- function(x, ...)` and
  `method(as.data.frame, Class) <- function(x, ...)`.
  `.onLoad()` calls `S7::methods_register()`.
- When a file's top-level code uses a class defined in another file, start
  the file with `#' @include other-file.R` followed by `NULL`. After adding
  or renaming files, run `roxygen2::roxygenise()` so that the `Collate` field
  stays complete. Files missing from `Collate` are not installed.

**Time.** A call analyses one window. By default the window length is
`T = 1` and the sampling interval is `delta_n = 1/n`, so integrated
variances are per window and every statistic is unit-free. Some tuning
constants carry time units, for example a volatility guess given per year.
Those functions take an explicit `horizon` (the window length in the unit of
the constant), and their docs say so.

**Numerics.** Vectorise. Never loop in R over the `n` observations; use
`stats::filter()`, `cumsum()` and `findInterval()`. Loops over a handful of
tuning values are fine.

**Documentation.** Write roxygen2 in markdown. Every exported function
documents:
- `@references`: full citations of the source papers;
- a **Paper mapping** section: equation or section -> argument or code step;
- an **Implementation choices** section: every decision the paper does not
  pin down, the default chosen, and the alternatives offered as options.

**Dependencies.** The package imports only `S7`, `stats` and `utils`. Data
handling inside the package uses base R. The pipeline may use `data.table`,
`targets` and `crew`. These are recorded in `renv.lock`; they are not package
dependencies.

## Testing policy

Tests are kept sparse: there is only a test where it would catch a real
error.
1. Closed-form identities and constants from the papers, e.g.
   `preavg_rho(4) == c(1, -3, 0.75)`.
2. Definitions checked against a direct, unvectorised implementation on a
   tiny input.
3. Limit behaviour on simulated data, with a fixed seed and a tolerance
   justified by the theory: a test centred at its null limit, a jump
   detected, an estimator close to the truth with or without jumps.
4. Regression tests for the defects found in the 0.1.0 review (below).

Each test file must run in a few seconds. Monte Carlo studies of size and
power go in `tests/simulations/*.R`; they are scripts that testthat does not
run. Simulate with `sim_log_prices()` from `tests/testthat/helper-sim.R`.

## Defects found in 0.1.0 (do not reintroduce)

- **Lee-Mykland volatility estimate.** The plug-in volatility for Lee-Mykland
  must be the square root of an integrated variance; LM (2012) Section 4.2
  prescribes Podolskij-Vetter (2009). Version 0.1.0 used a 4th-power
  pre-averaged sum, a truncated 4th-power variation, and an unsquare-rooted
  integrated variance.
- **Lee-Mykland lag selection.** The acf position of lag 0 was counted as a
  lag, so k came out 2 larger than in the paper.
- **Lee-Mykland block size.** M was not an integer, and blocks started k ticks
  before Definition 1 says. The final partial block must not be used.
- **Multiple testing.** Bonferroni across the blocks of one window
  double-counts: the Gumbel limit of the maximum already accounts for them.
  LM (2012) Section 6.2 adjusts across windows with a step-down procedure.
- **AJ (2012) S_J limits.** The limit labels were swapped. The correct limits
  are 1 with jumps, k^(p/2-1) on continuous paths, and 1/k under additive
  noise.
- **AJ subsampling.** Subsampling must build increments from the subsampled
  prices, not keep every k-th one-step return.
- **AJL (2012) rejection region.** The no-jump null rejects only in the lower
  tail (eq. 3.26). The jump null rejects only in the upper tail (eq. 3.27).
- **AJL variance constants.** The A' constants of AJL eq. 3.17 must be
  computed from the formula. The values hard-coded from the authors' Matlab
  code differ for pairs involving h: A'(h,h;0) is 28.8 by the formula and
  30.218 in the authors' code.
- **Outlier filtering.** Filters must be symmetric, must remove the offending
  observation, and must keep prices and returns consistent. Never carry the
  last return forward.
- **Bounceback filter.** A bounceback is a jump followed by a reversal of the
  opposite sign. Two large moves in the same direction are not a bounceback.

## Edge cases

When a paper is ambiguous:
1. Default to the choice the authors make in their own empirical or
   simulation section.
2. Expose the alternatives as options.
3. Document the decision under *Implementation choices*.

Hard ambiguities are escalated to an advisor review.
