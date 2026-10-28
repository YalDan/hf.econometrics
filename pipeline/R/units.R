# Units of analysis ------------------------------------------------------------
#
# A unit is one test day of one series: (date, symbol, venue), or
# (date, symbol) with all venues pooled. build_unit_index() returns one row
# per partition file with a `tar_group` column, so that
# `iteration = "group"` creates one branch per unit.

build_unit_index <- function(partition_files, cfg_units) {
  manifests <- partition_files[basename(partition_files) == "_manifest.rds"]
  idx <- data.table::rbindlist(lapply(manifests, function(m) readRDS(m)$index))
  if (!nrow(idx)) stop("No valid observations after ingestion.", call. = FALSE)
  pooled <- isTRUE(cfg_units$pool_venues)
  idx[, unit_venue := if (pooled) "pooled" else venue]
  idx[, unit_id := paste(format(date), symbol, unit_venue, sep = "|")]
  data.table::setorderv(idx, c("date", "symbol", "unit_venue", "venue", "part"))
  idx[, tar_group := data.table::rleid(unit_id)]
  idx[]
}

# One-row summary that every later table of the unit builds on.
unit_info_row <- function(unit) {
  data.table::data.table(
    unit_id = unit$unit_id[1L], date = unit$date[1L], symbol = unit$symbol[1L],
    venue = unit$unit_venue[1L],
    venues = paste(sort(unique(unit$venue)), collapse = ";"),
    n_raw = sum(unit$n)
  )
}

# All trades of a unit, in time order (ties keep the order of the files).
read_unit_ticks <- function(unit) {
  parts <- lapply(seq_len(nrow(unit)), function(i) {
    x <- read_part(unit$part[i])
    data.table::data.table(time = x$time, price = x$price, size = x$size,
                           venue = unit$venue[i])
  })
  ticks <- data.table::rbindlist(parts)
  data.table::setorderv(ticks, "time")
  ticks
}

# The test day [00:00, 24:00) in `day_tz`.
day_bounds <- function(date, day_tz) {
  list(start = as.POSIXct(format(date), tz = day_tz),
       end = as.POSIXct(format(date + 1L), tz = day_tz))
}
