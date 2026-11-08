# Output: CSV tables in config$output$dir -----------------------------------

# The tables written by the pipeline; README.md describes their columns. A
# table of a disabled stage is NULL.
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
    aj_jump_tests = aj_tests,
    jumps = detections[.accepted],
    run_summary = run_summary
  )
  # A disabled stage gives no table, or a table without columns.
  lapply(tables, function(x) if (!is.null(x) && ncol(x) > 0L) x)
}

# Writes the tables that exist and removes the file of every table that does
# not, so that a file left by an earlier run with other settings (say,
# ajl_tests.csv after AJL was disabled) is not mistaken for a result.
write_outputs <- function(tables, cfg_output) {
  dir.create(cfg_output$dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(cfg_output$dir, paste0(names(tables), ".csv"))
  written <- !vapply(tables, is.null, logical(1))
  for (k in which(written)) data.table::fwrite(tables[[k]], paths[k])
  stale <- paths[!written & file.exists(paths)]
  if (length(stale)) unlink(stale)
  paths[written]
}
