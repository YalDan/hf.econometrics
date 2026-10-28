# Output: CSV tables in config$output$dir -----------------------------------

# The tables written by the pipeline; README.md describes their columns.
output_tables <- function(ingest_log, unit_summary, removed_log, lm_windows, lm_blocks,
                          detections, ajl_tests, aj_tests, run_summary, cfg_output) {
  .accepted <- if (nrow(detections)) which(detections$accepted %in% TRUE) else integer()
  tables <- list(
    ingest = ingest_log,
    units = unit_summary,
    removed_observations = removed_log,
    lm_windows = lm_windows,
    lm_blocks = if (isTRUE(cfg_output$all_blocks)) lm_blocks,
    lm_detections = detections,
    ajl_tests = ajl_tests,
    aj_jump_tests = if (nrow(aj_tests)) aj_tests,
    jumps = detections[.accepted],
    run_summary = run_summary
  )
  # Disabled stages (no table, or a table without columns) write no file.
  tables[vapply(tables, function(x) !is.null(x) && ncol(x) > 0L, logical(1))]
}

write_outputs <- function(tables, cfg_output) {
  dir.create(cfg_output$dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(cfg_output$dir, paste0(names(tables), ".csv"))
  for (k in seq_along(tables)) data.table::fwrite(tables[[k]], paths[k])
  paths
}
