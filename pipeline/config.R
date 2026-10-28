# Pipeline configuration ----------------------------------------------------
#
# Every setting of the batch pipeline lives here. `_targets.R` sources this
# file, and each stage of the pipeline depends only on its own section, so
# that changing, for example, the multiple-testing scheme reruns only the
# combination step and not the jump tests.
#
# Tags in the comments name the source of a default:
#   [SNSH 2021 §n]  Saef, Nagy, Sizov and Hardle (2021), "Understanding jumps
#                   in high frequency digital asset markets", arXiv:2110.09429
#                   (the companion paper whose procedure this pipeline runs);
#   [LM 2012 §n]    Lee and Mykland (2012), Journal of Econometrics 168(2);
#   [0.1.0]         the companion paper's own code, hf.econometrics 0.1.0,
#                   used where the paper is silent.
#
# `profile` selects a preset. "companion" reproduces the companion paper;
# "lm2012" applies the data handling and inference of Lee and Mykland (2012,
# Section 6) to the same exports. HF_PIPELINE_PROFILE overrides the value
# without editing this file. Each profile writes to output/<profile>/.

profile <- Sys.getenv("HF_PIPELINE_PROFILE", unset = "companion")
if (!(profile %in% c("companion", "lm2012"))) {
  stop(sprintf("profile must be \"companion\" or \"lm2012\", not \"%s\".", profile),
       call. = FALSE)
}

config <- list(
  input = list(
    dir = "data-raw",
    glob = "*.csv",
    # Raw column -> role. `venue` and `size` are optional in the files.
    columns = c(time = "t", symbol = "s", price = "p", size = "q", venue = "id"),
    # Unit of numeric epoch timestamps ("s", "ms" or "us"); NULL for ISO-8601
    # strings. hf.econometrics::parse_timestamps() never guesses.
    timestamp_unit = NULL,
    # Time zone for timestamps without a zone designator.
    tz = "UTC",
    # Venue per file name; wins over an `id` column and over detection, e.g.
    # c("trades_2020.csv" = "binance").
    venue_override = character()
  ),

  symbols = list(
    # Off by default: symbols are used exactly as recorded. Pooling venues
    # needs equal symbols across venues, so turn this on for raw exports
    # that spell symbols differently ("BTC-USD" vs "btcusd").
    normalise = FALSE,
    lowercase = TRUE,
    strip = "[-_/ ]",                # characters removed from symbols
    rename = character(),            # after lowercase/strip, e.g. c(xbtusd = "btcusd")
    # Per-venue renames; 0.1.0 used e.g. list(hitbtc = c(dashusd = "dshusd")).
    rename_by_venue = list(),
    # Quote-currency aliases MERGE different markets (0.1.0 silently mapped
    # usdt -> usd). Opt in explicitly, e.g. c(usdt = "usd").
    quote_alias = character()
  ),

  units = list(
    # TRUE: one series per (date, symbol), all venues pooled [SNSH 2021 §4,
    # and the published tables count test days per symbol].
    # FALSE: one series per (date, symbol, venue) [SNSH 2021 §3.1: "We test
    # each currency on each exchange for every day separately"].
    pool_venues = TRUE,
    # Averaging at identical timestamps [LM 2012 §6.1; SNSH 2021 §4]:
    # "observations" weights every trade equally [0.1.0]; "venues" averages
    # within each venue first and then weights venues equally.
    pool = "observations",
    # Calendar days, and thus test days, are cut in this time zone.
    day_tz = "UTC"
  ),

  frequency = list(
    candidates = c(1, 5, 10, 15),    # seconds [SNSH 2021 §4]
    min_coverage = 0.95,             # share of occupied intervals [SNSH 2021 §4]
    # Days that reach the coverage at no candidate are not high frequency
    # and are not tested [SNSH 2021 §4]. With FALSE they are processed at
    # the largest candidate and flagged `eligible = FALSE`.
    skip_ineligible = TRUE
  ),

  filters = list(
    # Applied in this order to the series that the LM test sees; the AJL grid
    # is built from the cleaned series [SNSH 2021 §4: "we remove bounceback
    # outliers and returns outside of a range of 10 standard deviations"].
    bounceback = list(
      enabled = TRUE,
      threshold = 0.001,             # both legs above 0.1% [0.1.0]; no paper value
      # Optional threshold in standard deviations of the series' returns; it
      # replaces `threshold` when set. On 5 s BTC returns 0.1% is only about
      # 2 sd, so the absolute default also removes some noise reversals.
      threshold_sd = NULL,
      tolerance = 0.5                # net move at most half the larger leg
    ),
    outliers = list(
      enabled = TRUE,
      n_sd = 10                      # [SNSH 2021 §4]; 7 in [LM 2012 §6.1]
    )
  ),

  lm = list(
    # "aggregated": one observation per occupied interval at the chosen
    # frequency, no imputation [SNSH 2021 §4-5 and 0.1.0; the k and M of
    # SNSH Table 5 match this input]. "ticks": all trades after averaging
    # simultaneous prices [LM 2012 §6.1].
    input = "aggregated",
    # Test window: "1 day" [SNSH 2021 §3.1/§3.4] or "1 hour" [LM 2012 §6.1].
    window = "1 day",
    # Arguments for hf.econometrics::lm_options(); empty = the paper's defaults.
    # Its `alpha` sets only the package's own unadjusted columns (`reject`,
    # `threshold`, `n_flagged`, block `jump`); the pipeline's decisions use
    # `testing` below.
    options = list()
  ),

  ajl = list(
    enabled = TRUE,
    # Grid points before the first trade of the day: "first" fills them with
    # the first price [0.1.0] so that the grid spans the whole day; "drop"
    # starts the grid at the first trade.
    leading = "first",
    # Arguments for hf.econometrics::ajl_options(); otherwise the defaults of
    # AJL 2012 §4. Both nulls are tested: jump days (`testing` below) are
    # decided on the no-jump null (eq. 3.26); the jump null (eq. 3.27) is
    # only reported. As for LM, `alpha` sets only the package's `reject` and
    # `critical_value`.
    options = list(null = "both")
  ),

  # Ait-Sahalia and Jacod jump test on the same grid; not part of the
  # companion procedure.
  aj_jump = list(enabled = FALSE, options = list()),

  testing = list(
    # [SNSH 2021 §5] "We set alpha = 0.999" is the confidence level, so the
    # significance level is 0.001.
    alpha = 0.001,
    # Multiple testing of the LM windows. [SNSH 2021 §2/§3.1/§5] apply "a
    # Bonferroni correction on both tests" without naming the family:
    #   "bonferroni": Bonferroni across the windows of a family (default);
    #   "holm":       Holm step-down across windows [LM 2012 §6.2];
    #   "blocks":     each block at alpha / (number of blocks in its window),
    #                 the companion paper's own 0.1.0 code. This double
    #                 counts: the Gumbel limit of the maximum already covers
    #                 the blocks of a window (see DEVELOPMENT.md);
    #   "none":       every window at alpha.
    lm_adjust = "bonferroni",
    # Family for "bonferroni"/"holm": "series" (all windows of one symbol,
    # or symbol x venue, over the sample, as in LM 2012 §6.2) or "all".
    lm_family = "series",
    # AJL day tests: "bonferroni" across the days of a family
    # [SNSH 2021 §2/§5] or "none".
    ajl_adjust = "bonferroni",
    ajl_family = "series",
    # Keep LM jumps only on days where AJL rejects the no-jump null
    # [SNSH 2021 §2, §3.1, §5].
    require_ajl_day = TRUE
  ),

  consecutive = list(
    # [SNSH 2021 §5] "tagging jumps that were detected within 10 moments
    # after initial detection and remove them".
    enabled = TRUE,
    range = 10,
    # "blocks": distance in LM blocks (test times t_j) within a window;
    # "seconds": distance in seconds within a day.
    unit = "blocks",
    # "first": distance to the first jump of a cluster ("after initial
    # detection"); "previous": distance to the preceding jump (chained).
    anchor = "first"
  ),

  output = list(
    dir = file.path("output", profile),
    all_blocks = FALSE               # also write every LM block (large)
  ),

  # Parallel crew workers (if crew is installed). HF_PIPELINE_WORKERS
  # overrides the value without editing this file.
  workers = as.integer(Sys.getenv("HF_PIPELINE_WORKERS", unset = "2")),

  # "record": a failing test is written to the output with its error
  # message and the pipeline continues; "stop": the error stops tar_make().
  on_error = "record"
)

if (identical(profile, "lm2012")) {
  config <- utils::modifyList(config, list(
    units = list(pool_venues = FALSE),               # one market at a time
    frequency = list(skip_ineligible = FALSE),       # no coverage rule in LM 2012
    filters = list(
      bounceback = list(enabled = FALSE),            # the 7-sd rule handles bouncebacks
      outliers = list(n_sd = 7)                      # [LM 2012 §6.1]
    ),
    lm = list(input = "ticks", window = "1 hour"),   # [LM 2012 §6.1]
    ajl = list(enabled = FALSE),
    testing = list(alpha = 0.01, lm_adjust = "holm", # [LM 2012 §6.2]: 1% and 5%
                   require_ajl_day = FALSE),
    consecutive = list(enabled = FALSE)
  ))
}
