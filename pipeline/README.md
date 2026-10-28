# Jump-test pipeline

A [`targets`](https://docs.ropensci.org/targets/) pipeline that runs the
jump-test procedure of Saef, Nagy, Sizov and Hardle (2021), *Understanding
jumps in high frequency digital asset markets* (arXiv:2110.09429), on raw
trade exports, using the methods and data utilities of `hf.econometrics`. It
lives outside the package build. All settings are in `config.R`; a second
profile applies the data handling of Lee and Mykland (2012, Section 6).

## Running

```sh
R CMD INSTALL ..                      # the package, once per change of R/
cd pipeline
Rscript -e 'targets::tar_make()'      # run or update the pipeline
Rscript -e 'targets::tar_read(run_summary)'
```

* Put raw exports into `data-raw/` (`config$input$glob`, default `*.csv`).
  `data-raw/DT_sample.csv` (BTC-USD trades on Coinbase Pro, 2022-03-04) is
  bundled.
* Results are written to `output/` as CSV files (see below). Intermediate
  results live in `_targets/`; `targets::tar_destroy()` removes them.
* Requires `data.table`, `targets` and `hf.econometrics`. `crew` (parallel
  workers, `config$workers`, overridable with `HF_PIPELINE_WORKERS`) and
  `qs2` (faster storage) are used when installed.
* A rerun updates only what changed. Each stage depends on its own slice of
  `config.R`: changing `testing` or `consecutive` reruns only the
  combination step, and changing `lm` reruns only the LM test.
* Choose the preset with `profile` at the top of `config.R`.

## Steps and their sources

| Step | Code | Source |
|---|---|---|
| Read exports; venue from `id`, else from the column signature (as in 0.1.0); an existing `id` is kept | `R/ingest.R` | 0.1.0 `rename_raw_files()` |
| Parse timestamps (ISO-8601, or epochs with an explicit `timestamp_unit`; never guessed) | `parse_timestamps()` | |
| Normalise symbols (off by default; quote aliases such as usdt -> usd only on request) | `normalise_symbols()` | |
| Units: (date, symbol) with venues pooled, or (date, symbol, venue) | `units$pool_venues` | SNSH §4 / §3.1 |
| Average simultaneous prices | `aggregate_simultaneous()` | LM §6.1, SNSH §4 |
| 1/5/10/15 s by coverage >= 95% of intervals; days below are not tested | `choose_frequency()` | SNSH §4 |
| LM input: occupied intervals at that frequency (no imputation), or all ticks | `lm$input` | SNSH §4-5 / LM §6.1 |
| Bounceback filter, then symmetric n-sd filter (10 sd; LM: 7) | `filter_bouncebacks()`, `filter_outliers()` | SNSH §4, LM §6.1 |
| AJL input: previous-tick grid over [00:00, 24:00], last-price imputation | `sample_previous_tick()`, `locf()` | SNSH §5 |
| LM test per window (1 day; LM: 1 hour) | `split_windows()`, `lm_test()` | SNSH §3, LM §6.1 |
| AJL test per day | `ajl_test()` | SNSH §3.4 |
| Multiple testing, alpha = 0.001 (SNSH "alpha = 0.999") | `R/combine.R` | SNSH §2, §5; LM §6.2 |
| Keep LM jumps only on AJL jump days | `combine_detections()` | SNSH §2, §3.1, §5 |
| Drop jumps within 10 blocks after the first of a cluster | `drop_consecutive_flags()` | SNSH §5 |

SNSH = Saef et al. (2021); LM = Lee and Mykland (2012), *Journal of
Econometrics* 168(2), 396-406; 0.1.0 = the companion paper's own code.

## Choices where the companion paper is ambiguous

All of these are settings in `config.R`.

* **Pooled or per exchange.** Section 3.1 tests "each currency on each
  exchange for every day separately"; Section 4 pools prices across
  exchanges, and the published tables count test days per symbol (BTC: 645
  of 669 days). The default follows the empirical section (pooled). Pooling
  venues whose prices differ persistently creates venue-switching
  reversals, which the bounceback filter then removes.
* **LM input.** Section 5 says LM "works best on processing ticks", while
  Section 4 aggregates ticks to the chosen frequency "for testing". The
  default (`"aggregated"`) feeds LM the observed series at that frequency
  without imputation, as the 0.1.0 code did; the block sizes of Table 5
  (mean k 13.5, mean M 4.2) are consistent with this input, not with raw
  ticks. `"ticks"` uses all trades.
* **Filters** run once, on the LM input, before any imputation; the AJL grid
  is built from the cleaned series, so both tests see the same prices.
  0.1.0 filtered the two inputs separately, and its AJL standard deviation
  included imputed zero returns.
* **Bonferroni.** The paper applies "a Bonferroni correction on both tests"
  without naming the family. Default: Bonferroni across the windows (LM) and
  days (AJL) of each series. `lm_adjust = "holm"` is the step-down of LM
  (2012, §6.2). `lm_adjust = "blocks"` reproduces the 0.1.0 code (each block
  at alpha / number of blocks); it double counts, because the Gumbel limit
  of the window maximum already covers the blocks. LM blocks within a
  rejected window are detections when multiplier x p_block <= alpha, with
  the window's own multiplier (m, Holm step or number of blocks).
* **Consecutive jumps.** "Within 10 moments after initial detection" is read
  as 10 LM blocks after the first jump of a cluster
  (`consecutive$anchor = "first"`); `"previous"` chains clusters.
* **Leading grid points** before the first trade of a day are filled with
  the first price (0.1.0), or dropped (`ajl$leading = "drop"`).

## Outputs (`output/`)

| File | One row per | Key columns |
|---|---|---|
| `ingest.csv` | raw file | venue source, rows read, rows dropped (bad time / price / symbol) |
| `units.csv` | unit (date, symbol, venue or "pooled") | trades, distinct times, `coverage_<f>s`, `frequency`, `eligible`, filter counts, sizes of the LM and AJL inputs, `status` |
| `removed_observations.csv` | removed observation | `time`, `price`, `log_return`, `statistic`, `filter` |
| `lm_windows.csv` | LM window | window bounds, `n_obs`, method columns (`n`, `k`, `M`, `statistic`, `p_value`, ...), `family_size`, `multiplier`, `p_adjusted`, `jump_window`, `status`, `message` |
| `lm_detections.csv` | detected LM block | `time`, method block columns (`L`, `chi`, ...), `p_block`, `ajl_jump_day`, `consecutive_kept`, `accepted` |
| `jumps.csv` | accepted jump | as `lm_detections.csv`, `accepted = TRUE` only |
| `ajl_tests.csv` | AJL test and null | method columns (`null`, `statistic`, `z`, `p_value`, ...), `p_adjusted`, `jump_day` (no-jump null) |
| `aj_jump_tests.csv` | AJ test (if enabled) | method columns |
| `lm_blocks.csv` | LM block (if `output$all_blocks`) | as `lm_detections.csv` |
| `run_summary.csv` | measure | counts from files to accepted jumps |

Times are written in ISO-8601 UTC. With `on_error = "record"` a failing test
appears with `status = "error"` and its message, and the run continues.

## Design notes

* Ingestion writes one partition file per (date, venue, symbol) to
  `_targets/user/partitions/`; each unit branch reads only its partitions,
  so no target holds all trades in memory.
* All calls into the jump tests are in `R/methods.R`. A change of the method
  API (`lm_test()`, `lm_blocks()`, `lm_holm()`, `ajl_test()`,
  `aj_jump_test()`) is a change to that file only.
* LM 2012 discards NYSE trades outside 9:30-16:00 and the first trade after
  the open. Crypto markets trade around the clock, so the `lm2012` profile
  keeps all trades; `split_windows(breaks = ...)` reproduces the NYSE
  windows for exchange data.

## Troubleshooting

* `targets` needs `igraph`, which needs the system library GLPK
  (`libglpk.so.40`; Debian/Ubuntu package `libglpk40`).
* With several crew workers, `data.table` threads multiply; set
  `data.table::setDTthreads(1)` in `~/.Rprofile` if the machine is
  oversubscribed.
