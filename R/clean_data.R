#' Data Cleaning
#'
#' Helper function for preprocessing the data for calculating jump tests
#'
#' @import data.table
#' @import lubridate
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param id Optional argument in case Blockchain Research Center data as in the example is used. Defaults to NA.
#'
#' @return Returns the cleaned data

#' @export
clean_data <- function(DATA, id = NA){
  DT_tmp <- copy(DATA) # Ensure to work on a copy of the data

  id <- DT_tmp[1,id]

  print(paste(Sys.time(), "- Starting data cleaning for ID:", id))

  # Preprocessing steps
  print(paste(Sys.time(), "- Preprocessing data..."))

  # Convert 't' from various formats to POSIXct
  print(paste(Sys.time(), "- Converting timestamps to POSIXct."))
  DT_tmp[, t := tryCatch({
    as.POSIXct(as.numeric(t) / 1e3, origin = "1970-01-01", tz = "UTC")
  }, warning = function(w) {
    print(paste("Warning in timestamp conversion:", w))
    NA
  }, error = function(e) {
    print(paste("Error in timestamp conversion:", e))
    NA
  }, finally = {
    print(paste(Sys.time(), "- Timestamp conversion completed."))
  })]

  # Handle symbol adjustments
  print(paste(Sys.time(), " - Adjusting symbol names to lowercase and removing redundant content."))
  DT_tmp[, s := tolower(s)]
  DT_tmp[, s := gsub("-", "", s)]
  DT_tmp[, s := gsub("usdt", "usd", s)]

  # Example of a scalable approach for symbol renaming
  symbol_replacements <- list(
    binance = c(iotausd = "iotusd"),
    hitbtc = c(iotausd = "iotusd", dashusd = "dshusd"),
    poloniex = c(dashusd = "dshusd")
  )

  if (!is.na(id) && id %in% names(symbol_replacements)) {
    replacements <- symbol_replacements[[id]]
    print(paste(Sys.time(), " - Renaming symbols based on exchange ID:", id))
    for (old in names(replacements)) {
      DT_tmp[s == old, s := replacements[[old]]]
    }
  } else if (!is.na(id)) {
    print(paste(Sys.time(), " - ID provided but no specific renaming rules found for:", id))
  }

  print(paste(Sys.time(), " - Data cleaning completed for ID:", id))
  return(DT_tmp)
}

