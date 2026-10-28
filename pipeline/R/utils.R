# Helpers shared by the pipeline functions -----------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

# Fails early, when _targets.R is loaded, on settings outside their choices.
check_config <- function(config) {
  choice <- function(x, choices, name) {
    if (!(is.character(x) && length(x) == 1L && x %in% choices)) {
      stop(sprintf("config$%s must be one of %s, not %s.", name,
                   paste0("\"", choices, "\"", collapse = ", "), deparse(x)), call. = FALSE)
    }
  }
  flag <- function(x, name) {
    if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
      stop(sprintf("config$%s must be TRUE or FALSE.", name), call. = FALSE)
    }
  }
  number <- function(x, name, lower, upper) {
    if (!(is.numeric(x) && length(x) == 1L && !is.na(x) && x > lower && x <= upper)) {
      stop(sprintf("config$%s must be a number in (%s, %s].", name, lower, upper), call. = FALSE)
    }
  }
  if (!is.null(config$input$timestamp_unit)) {
    choice(config$input$timestamp_unit, c("s", "ms", "us"), "input$timestamp_unit")
  }
  if (!all(c("time", "symbol", "price") %in% names(config$input$columns))) {
    stop("config$input$columns must name the time, symbol and price columns.", call. = FALSE)
  }
  flag(config$symbols$normalise, "symbols$normalise")
  flag(config$units$pool_venues, "units$pool_venues")
  choice(config$units$pool, c("observations", "venues"), "units$pool")
  if (!(is.numeric(config$frequency$candidates) && length(config$frequency$candidates) &&
        all(config$frequency$candidates > 0))) {
    stop("config$frequency$candidates must be positive numbers of seconds.", call. = FALSE)
  }
  number(config$frequency$min_coverage, "frequency$min_coverage", 0, 1)
  flag(config$frequency$skip_ineligible, "frequency$skip_ineligible")
  flag(config$filters$bounceback$enabled, "filters$bounceback$enabled")
  if (!is.null(config$filters$bounceback$threshold_sd)) {
    number(config$filters$bounceback$threshold_sd, "filters$bounceback$threshold_sd", 0, Inf)
  }
  flag(config$filters$outliers$enabled, "filters$outliers$enabled")
  choice(config$lm$input, c("aggregated", "ticks"), "lm$input")
  flag(config$ajl$enabled, "ajl$enabled")
  choice(config$ajl$leading, c("first", "drop"), "ajl$leading")
  flag(config$aj_jump$enabled, "aj_jump$enabled")
  number(config$testing$alpha, "testing$alpha", 0, 0.5)
  choice(config$testing$lm_adjust, c("bonferroni", "holm", "blocks", "none"), "testing$lm_adjust")
  choice(config$testing$lm_family, c("series", "all"), "testing$lm_family")
  choice(config$testing$ajl_adjust, c("bonferroni", "none"), "testing$ajl_adjust")
  choice(config$testing$ajl_family, c("series", "all"), "testing$ajl_family")
  flag(config$testing$require_ajl_day, "testing$require_ajl_day")
  if (isTRUE(config$testing$require_ajl_day) && !isTRUE(config$ajl$enabled)) {
    stop("config$testing$require_ajl_day needs config$ajl$enabled = TRUE.", call. = FALSE)
  }
  flag(config$consecutive$enabled, "consecutive$enabled")
  choice(config$consecutive$unit, c("blocks", "seconds"), "consecutive$unit")
  choice(config$consecutive$anchor, c("first", "previous"), "consecutive$anchor")
  number(config$workers, "workers", 0, Inf)
  choice(config$on_error, c("record", "stop"), "on_error")
  invisible(config)
}

# Row-binds one field of a list of branch results.
bind_field <- function(results, field) {
  data.table::rbindlist(lapply(results, `[[`, field), use.names = TRUE, fill = TRUE)
}

# Identifier columns that lead every output table.
unit_ids <- function(info) {
  data.table::data.table(unit_id = info$unit_id, date = info$date, symbol = info$symbol,
                         venue = info$venue)
}

# Prepends identifier columns; method columns that clash get a suffix.
with_ids <- function(ids, df) {
  df <- data.table::as.data.table(df)
  clash <- intersect(names(df), names(ids))
  if (length(clash)) data.table::setnames(df, clash, paste0(clash, "_method"))
  if (!nrow(df)) return(cbind(ids[0L], df))
  cbind(ids[rep(1L, nrow(df))], df)
}

# Runs a method call under the configured error policy.
attempt <- function(fun, on_error) {
  if (identical(on_error, "stop")) return(list(value = fun(), error = NA_character_))
  tryCatch(list(value = fun(), error = NA_character_),
           error = function(e) list(value = NULL, error = conditionMessage(e)))
}

has_qs2 <- function() requireNamespace("qs2", quietly = TRUE)
