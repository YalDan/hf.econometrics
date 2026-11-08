# Batch pipeline: jump tests on high-frequency trade exports ------------------
#
# Run from this directory:   Rscript -e 'targets::tar_make()'
# Settings:                  config.R
# Functions:                 R/*.R
# Documentation:             README.md

library(targets)

source("config.R")
tar_source("R")
check_config(config)

tar_option_set(
  packages = c("data.table", "hf.econometrics"),
  format = if (requireNamespace("qs2", quietly = TRUE)) "qs" else "rds",
  memory = "transient",
  garbage_collection = 1L,
  error = "stop",
  controller = if (requireNamespace("crew", quietly = TRUE)) {
    crew::crew_controller_local(workers = config$workers, seconds_idle = 30)
  }
)

# Each stage depends only on its own slice of config, so a change reruns
# only the stages it affects.
config_targets <- list(
  tar_target(cfg_input, config$input, deployment = "main"),
  tar_target(cfg_symbols, config$symbols, deployment = "main"),
  tar_target(cfg_units, config$units[c("pool_venues", "day_tz")], deployment = "main"),
  tar_target(
    cfg_prepare,
    list(units = config$units, frequency = config$frequency, filters = config$filters,
         lm_input = config$lm$input, ajl = config$ajl[c("enabled", "leading")]),
    deployment = "main"
  ),
  tar_target(
    cfg_lm,
    list(window = config$lm$window, options = config$lm$options,
         day_tz = config$units$day_tz, on_error = config$on_error),
    deployment = "main"
  ),
  tar_target(
    cfg_ajl,
    list(enabled = config$ajl$enabled, options = config$ajl$options,
         on_error = config$on_error),
    deployment = "main"
  ),
  tar_target(
    cfg_aj,
    list(enabled = config$aj_jump$enabled, options = config$aj_jump$options,
         on_error = config$on_error),
    deployment = "main"
  ),
  tar_target(cfg_test, config$testing, deployment = "main"),
  tar_target(cfg_consecutive, config$consecutive, deployment = "main"),
  tar_target(cfg_output, config$output, deployment = "main")
)

ingest_targets <- list(
  # Re-listed on every run so that new files are picked up.
  tar_target(raw_paths, discover_inputs(cfg_input), cue = tar_cue(mode = "always"),
             deployment = "main"),
  tar_target(raw_files, raw_paths, pattern = map(raw_paths), format = "file",
             deployment = "main"),
  # One branch per raw file: parse, detect the venue, normalise symbols and
  # write one partition per (date, venue, symbol).
  tar_target(partitions, ingest_file(raw_files, cfg_input, cfg_symbols, cfg_units$day_tz),
             pattern = map(raw_files), format = "file"),
  tar_target(ingest_log, read_ingest_logs(partitions), deployment = "main"),
  # One group per unit: (date, symbol, venue), or (date, symbol) when pooled.
  tar_target(unit_index, build_unit_index(partitions, cfg_units), iteration = "group",
             deployment = "main")
)

unit_targets <- list(
  tar_target(prepared, prepare_unit(unit_index, cfg_prepare), pattern = map(unit_index),
             iteration = "list"),
  tar_target(unit_rows, prepared$info, pattern = map(prepared), iteration = "list"),
  tar_target(removed_rows, prepared$removed, pattern = map(prepared), iteration = "list"),
  tar_target(lm_runs, run_lm_unit(prepared, cfg_lm), pattern = map(prepared),
             iteration = "list"),
  tar_target(lm_window_rows, lm_runs$windows, pattern = map(lm_runs), iteration = "list"),
  tar_target(lm_block_rows, lm_runs$blocks, pattern = map(lm_runs), iteration = "list"),
  tar_target(ajl_rows, run_ajl_unit(prepared, cfg_ajl), pattern = map(prepared),
             iteration = "list"),
  tar_target(aj_rows, run_aj_jump_unit(prepared, cfg_aj), pattern = map(prepared),
             iteration = "list")
)

combine_targets <- list(
  tar_target(unit_summary, data.table::rbindlist(unit_rows, fill = TRUE), deployment = "main"),
  tar_target(removed_log, data.table::rbindlist(removed_rows, fill = TRUE), deployment = "main"),
  tar_target(lm_windows, adjust_lm_windows(data.table::rbindlist(lm_window_rows, fill = TRUE),
                                           cfg_test), deployment = "main"),
  tar_target(lm_blocks, flag_lm_blocks(data.table::rbindlist(lm_block_rows, fill = TRUE),
                                       lm_windows, cfg_test), deployment = "main"),
  tar_target(ajl_tests, adjust_ajl_tests(data.table::rbindlist(ajl_rows, fill = TRUE),
                                         cfg_test), deployment = "main"),
  tar_target(aj_tests, data.table::rbindlist(aj_rows, fill = TRUE), deployment = "main"),
  tar_target(detections, combine_detections(lm_blocks, ajl_tests, cfg_test, cfg_consecutive),
             deployment = "main"),
  tar_target(run_summary, summarise_run(ingest_log, unit_summary, lm_windows, ajl_tests,
                                        detections), deployment = "main"),
  tar_target(
    outputs,
    write_outputs(output_tables(ingest_log, unit_summary, removed_log, lm_windows, lm_blocks,
                                detections, ajl_tests, aj_tests, run_summary, cfg_output),
                  cfg_output),
    format = "file", deployment = "main"
  )
)

list(config_targets, ingest_targets, unit_targets, combine_targets)
