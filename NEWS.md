# hf.econometrics 0.2.0

This release is a rewrite. It follows an adversarial review of 0.1.0 that
checked every function against its source paper. 0.1.0 functions are
removed; the table below maps each one to its replacement.

## Breaking changes

| 0.1.0 | 0.2.0 |
|---|---|
| `LM_JumpTest()` | `lm_test()` with `lm_options()`; `lm_blocks()`; `lm_holm()` |
| `AJ_JumpTest()` | `aj_jump_test()`, `aj_finite_activity_test()`, `aj_spectrum()` |
| `AJ_09_variation()` | `aj_activity_index()` (the estimator of AJ 2009b) |
| `AJL_JumpTest_2012()` | `ajl_test()` with `ajl_options()` |
| `jacod_preaveraging()` | `integrated_variance(method = "jlmpv2009" / "jpv2010")`; `preavg_vbar()` for the 4th-power statistic it actually returned |
| `rMRC` via `highfrequency` | `integrated_variance(method = "hp2013")`, implemented in the package |
| `make_return_file()`, `remove_bounceback()`, `split_by_id()`, ... | data utilities (`sample_previous_tick()`, `filter_outliers()`, `filter_bouncebacks()`, ...) and the `targets` pipeline in `pipeline/` |

* Options and results are typed S7 objects. Inputs are numeric log prices or
  `hf_series()`.
* Dependencies drop to `S7`, `stats` and `utils`. `xts`, `quantmod`,
  `imputeTS`, `lubridate`, `highfrequency` and `data.table` are no longer
  imported; `data.table` is used only by the pipeline.

## Corrections relative to the source papers

* **Lee–Mykland:**
  * The plug-in variance uses a volatility estimate. In 0.1.0 it used a
    4th-power sum, which flagged about half of all blocks on days without
    jumps.
  * The lag k follows the paper's rule; it was 2 too large.
  * M is an integer.
  * Blocks follow Definition 1. They started k ticks early, and a partial
    block was appended at the end.
  * L is a forward difference.
  * Multiple testing is across windows (§6.2), not across blocks within a
    window.
* **Aït-Sahalia–Jacod:**
  * The S_J limits are labelled correctly: 1 with jumps, k^(p/2-1) when
    continuous, and 1/k under noise.
  * Subsampled increments are built from prices; 0.1.0 used every 5th
    one-second return.
  * The CLT tests of AJ (2009a) and AJ (2011) are added.
  * AJ (2011) eq. (49) misprints N(4,k). The code follows eq. (38).
* **AJL:**
  * Both nulls are tested, one-sided, as in eqs. (3.26) and (3.27).
  * The A' constants are computed from eq. (3.17). The authors' Matlab values
    remain available with `aprime = "authors"`.
  * `Vvec` is corrected for p >= 6.
* **Data handling:**
  * The outlier filter is symmetric and removes the offending observation.
    Returns are never carried forward.
  * Bouncebacks require reversals of opposite sign.
  * Timestamp parsing never silently rescales.
  * The sampling frequency is chosen by interval coverage, as in the
    companion paper.

## Reproducibility

* `renv.lock` pins R 4.3.3 and all package, pipeline and development
  dependencies to a dated Posit Package Manager snapshot.
