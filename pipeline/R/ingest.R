# Ingestion: raw exports -> per-(date, venue, symbol) partition files -------
#
# Each raw file is read once, its timestamps are parsed with
# hf.econometrics::parse_timestamps(), its venue is taken from an `id` column
# or detected from the column signature, and its rows are written as one
# partition file per (date, venue, symbol) under _targets/user/partitions/.
# The unit branches then read only the partitions they need, so no target
# ever holds all trades in memory.

discover_inputs <- function(cfg_input) {
  paths <- sort(Sys.glob(file.path(cfg_input$dir, cfg_input$glob)))
  if (!length(paths)) {
    stop(sprintf("No input files match \"%s\".", file.path(cfg_input$dir, cfg_input$glob)),
         call. = FALSE)
  }
  keys <- file_key(paths)
  if (anyDuplicated(keys)) {
    stop("Input file names must stay distinct after sanitising: ",
         paste(paths[duplicated(keys) | duplicated(keys, fromLast = TRUE)], collapse = ", "),
         call. = FALSE)
  }
  paths
}

file_key <- function(path) gsub("[^A-Za-z0-9._-]", "_", basename(path))

partition_root <- function() file.path(targets::tar_path_store(), "user", "partitions")

# Column signatures of the Blockchain Research Center exports, as in
# rename_raw_files() of version 0.1.0, compared as sets. HitBTC and OKEx
# share a signature; OKEx symbols contain "-".
venue_signatures <- list(
  binance = c("q", "p", "s", "t", "d", "m", "seller_id", "buyer_id"),
  bitfinex = c("q", "p", "s", "t", "d"),
  bitstamp = c("q", "p", "s", "t", "d", "side", "buy_id", "sell_id"),
  cbpro = c("q", "p", "s", "t", "d", "maker_order_id", "taker_order_id", "side"),
  poloniex = c("p", "s", "t", "d", "lowest_ask", "highest_bid"),
  hitbtc_okex = c("q", "p", "s", "t", "d", "side")
)

detect_venue <- function(columns, first_symbol) {
  for (v in names(venue_signatures)) {
    if (setequal(columns, venue_signatures[[v]])) {
      if (v != "hitbtc_okex") return(v)
      return(if (grepl("-", first_symbol, fixed = TRUE)) "okex" else "hitbtc")
    }
  }
  NA_character_
}

read_raw_file <- function(path, cfg_input) {
  cols <- cfg_input$columns
  header <- names(data.table::fread(path, nrows = 0L))
  missing <- setdiff(cols[c("time", "symbol", "price")], header)
  if (length(missing)) {
    stop(sprintf("%s lacks the column(s) %s (see config$input$columns).", basename(path),
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  has_size <- "size" %in% names(cols) && cols[["size"]] %in% header
  has_venue <- "venue" %in% names(cols) && cols[["venue"]] %in% header
  chr <- c(cols[["time"]], cols[["symbol"]], if (has_venue) cols[["venue"]])
  num <- c(cols[["price"]], if (has_size) cols[["size"]])
  # Timestamps are read as text and parsed by the package, which refuses to
  # guess; fread would otherwise read epochs as integer64.
  dt <- data.table::fread(path, select = c(chr, num), showProgress = FALSE,
                          colClasses = list(character = chr, numeric = num))
  data.table::setnames(dt, c(chr, num),
                       c("time", "symbol", if (has_venue) "venue", "price", if (has_size) "size"))
  if (!has_size) dt[, size := NA_real_]

  # Venue: explicit override, then an existing `id` column (kept as is; 0.1.0
  # overwrote it with NA), then the column signature.
  override <- unname(cfg_input$venue_override[basename(path)])
  detected <- detect_venue(header, dt$symbol[1L])
  if (length(override) == 1L && !is.na(override)) {
    dt[, venue := override]
    source <- "override"
  } else if (has_venue) {
    miss <- is.na(dt$venue) | !nzchar(dt$venue)
    source <- "id column"
    if (any(miss)) {
      if (is.na(detected)) {
        stop(sprintf("%s: %d rows have no venue id and the columns match no known export.",
                     basename(path), sum(miss)), call. = FALSE)
      }
      dt[miss, venue := detected]
      source <- "id column + signature"
    }
  } else if (!is.na(detected)) {
    dt[, venue := detected]
    source <- "signature"
  } else {
    stop(sprintf(paste0("Cannot identify the venue of %s: no venue column and the columns ",
                        "(%s) match no known export. Set config$input$venue_override."),
                 basename(path), paste(header, collapse = ", ")), call. = FALSE)
  }

  n_rows <- nrow(dt)
  dt[, time := hf.econometrics::parse_timestamps(time, unit = cfg_input$timestamp_unit,
                                                 tz = cfg_input$tz)]
  bad_time <- is.na(dt$time)
  bad_price <- !bad_time & !(is.finite(dt$price) & dt$price > 0)
  bad_symbol <- !bad_time & !bad_price & (is.na(dt$symbol) | !nzchar(dt$symbol))
  dt <- dt[!(bad_time | bad_price | bad_symbol)]
  log <- data.table::data.table(
    file = path, venue_source = source,
    venues = paste(sort(unique(dt$venue)), collapse = ";"),
    n_rows = n_rows, n_bad_time = sum(bad_time), n_bad_price = sum(bad_price),
    n_bad_symbol = sum(bad_symbol), n_kept = nrow(dt)
  )
  list(data = dt, log = log)
}

# Symbol normalisation (off unless cfg$normalise), on the distinct
# (symbol, venue) pairs.
normalise_symbols <- function(symbol, venue, cfg) {
  if (!isTRUE(cfg$normalise)) return(symbol)
  key <- unique(data.table::data.table(symbol = symbol, venue = venue))
  s <- key$symbol
  if (isTRUE(cfg$lowercase)) s <- tolower(s)
  if (length(cfg$strip) && nzchar(cfg$strip)) s <- gsub(cfg$strip, "", s)
  for (a in names(cfg$quote_alias)) {
    s <- sub(paste0(a, "$"), cfg$quote_alias[[a]], s)
  }
  if (length(cfg$rename)) {
    m <- match(s, names(cfg$rename))
    s[!is.na(m)] <- unname(cfg$rename[m[!is.na(m)]])
  }
  for (v in names(cfg$rename_by_venue)) {
    map <- cfg$rename_by_venue[[v]]
    sel <- key$venue == v & s %in% names(map)
    s[sel] <- unname(map[s[sel]])
  }
  key[, normalised := s]
  key[data.table::data.table(symbol = symbol, venue = venue), on = c("symbol", "venue"),
      normalised]
}

save_part <- function(x, path) {
  if (grepl("\\.qs2$", path)) qs2::qs_save(x, path) else saveRDS(x, path, compress = FALSE)
}

read_part <- function(path) {
  if (grepl("\\.qs2$", path)) qs2::qs_read(path) else readRDS(path)
}

# Target command: one raw file -> partition files plus a manifest. Returns
# the paths of all files written (a format = "file" target).
ingest_file <- function(path, cfg_input, cfg_symbols, day_tz) {
  raw_path <- path
  raw <- read_raw_file(raw_path, cfg_input)
  dt <- raw$data
  dt[, symbol := normalise_symbols(symbol, venue, cfg_symbols)]
  dt[, date := data.table::as.IDate(as.Date(time, tz = day_tz))]
  data.table::setorderv(dt, c("venue", "symbol", "date", "time")) # stable: ties keep file order
  dir <- file.path(partition_root(), file_key(raw_path))
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  parts <- dt[, list(n = .N, first = .I[1L], last = .I[.N]), by = c("date", "venue", "symbol")]
  ext <- if (has_qs2()) ".qs2" else ".rds"
  parts[, part := file.path(dir, sprintf("part-%05d%s", seq_len(.N), ext))]
  for (g in seq_len(nrow(parts))) {
    rows <- parts$first[g]:parts$last[g]
    save_part(data.frame(time = dt$time[rows], price = dt$price[rows], size = dt$size[rows]),
              parts$part[g])
  }
  manifest <- file.path(dir, "_manifest.rds")
  log <- raw$log
  log[, n_partitions := nrow(parts)]
  index <- parts[, list(file = raw_path, part, date, venue, symbol, n)]
  saveRDS(list(index = index, log = log), manifest)
  c(manifest, parts$part)
}

read_ingest_logs <- function(partition_files) {
  manifests <- partition_files[basename(partition_files) == "_manifest.rds"]
  data.table::rbindlist(lapply(manifests, function(m) readRDS(m)$log), fill = TRUE)
}
