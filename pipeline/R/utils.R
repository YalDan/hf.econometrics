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

  # Method options: built once here with the package's own constructors, so
  # that invalid options stop tar_make() before any data is read.
  if (utils::packageVersion("hf.econometrics") < "0.2.0") {
    stop("The pipeline needs hf.econometrics >= 0.2.0 (the S7 API); run R CMD INSTALL ..",
         call. = FALSE)
  }
  build <- function(fun, args, name) {
    if (!is.list(args)) stop(sprintf("config$%s must be a list.", name), call. = FALSE)
    tryCatch(do.call(fun, args), error = function(e) {
      stop(sprintf("config$%s: %s", name, conditionMessage(e)), call. = FALSE)
    })
  }
  build(hf.econometrics::lm_options, config$lm$options, "lm$options")
  if (isTRUE(config$ajl$enabled)) {
    ajl <- build(hf.econometrics::ajl_options, config$ajl$options, "ajl$options")
    if (isTRUE(config$testing$require_ajl_day) && !(ajl@null %in% c("no_jump", "both"))) {
      stop(paste0("config$testing$require_ajl_day needs the no-jump null: set ",
                  "config$ajl$options$null to \"no_jump\" or \"both\"."), call. = FALSE)
    }
  }
  if (isTRUE(config$aj_jump$enabled)) {
    build(hf.econometrics::aj_jump_options, config$aj_jump$options, "aj_jump$options")
  }
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

# Runs a method call under the configured error policy. Warnings are
# collected (and muffled) so that they can be written to the rows of the
# call: `warning` holds their distinct messages joined by " | ", or NA.
attempt <- function(fun, on_error) {
  warnings <- character()
  run <- function() {
    withCallingHandlers(fun(), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }
  collected <- function() {
    if (length(warnings)) paste(unique(warnings), collapse = " | ") else NA_character_
  }
  if (identical(on_error, "stop")) {
    value <- run()
    return(list(value = value, error = NA_character_, warning = collected()))
  }
  tryCatch({
    value <- run()
    list(value = value, error = NA_character_, warning = collected())
  }, error = function(e) list(value = NULL, error = conditionMessage(e), warning = collected()))
}

has_qs2 <- function() requireNamespace("qs2", quietly = TRUE)
