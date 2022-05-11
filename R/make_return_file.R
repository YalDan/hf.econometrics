#' Make return file
#'
#' Helper function to create a regular return file
#'
#' @import xts
#' @import quantmod
#' @import data.table
#' @import lubridate
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param FREQ The highest possible frequency. Defaults to 1 for a frequency of 1 second.
#' @param IMPUTATION Should missing values be imputed? If TRUE, then `na_locf` will be used. Defaults to FALSE.
#' @param CUTOFF The number of standard deviations of log returns around which data should be kept. Defaults to 10, s.t. observations outside +-10 SD will be omitted.
#' @param ALIGN Should the data be aligned to round timestamps? Defaults to TRUE.
#'
#' @return Outputs a cleaned return file.

#' @export
make_return_file <- function(DATA, FREQ = 1, IMPUTATION = FALSE, CUTOFF = 10, ALIGN = TRUE){
  S <- DATA

  #
  start <- paste(as.Date(first(S[, t])),  " ", first(S[, h]), ":00:00", sep = "")
  start <- lubridate::ymd_hms(start, tz = "UTC")
  end <- paste(as.Date(first(S[, t])),  " ", last(S[, h]), ":59:59", sep = "")
  end <- lubridate::ymd_hms(end, tz = "UTC") - FREQ
  #

  #
  ts_p <- xts(S$p, order.by = S$t, unique = FALSE)
  if (ALIGN) {
    ts_p <- align.time(ts_p, n = FREQ)
  }
  ts_p <- to.period(ts_p, period = "seconds", k = FREQ)

  #
  DT_ts_p <- data.table("index" = seq(from = start, to = end, by = paste(FREQ, " sec", sep = "")),
                        "date" = unique(S$date),
                        "id" = unique(S$id),
                        "s" = unique(S$s)
  )
  DT_ts_p[, index := as.POSIXct(index, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")]
  DT_ts_p[, h := hour(index)]
  DT_ts_p[as.data.table(ts_p), "p" := i.ts_p.Close, on = "index"]

  if (IMPUTATION) {
    DT_ts_p[, p := imputeTS::na_locf(p, na_remaining = "rev")]
    DT_ts_p[, log_ret := quantmod::Delt(p, type = "log")]
    DT_ts_p[1, log_ret := 0]
  } else {
    DT_ts_p <- DT_ts_p[!is.na(p)]
    DT_ts_p[, log_ret := quantmod::Delt(p, type = "log")]
  }

  if (!is.na(CUTOFF)) {
    if (is.numeric(CUTOFF)) {
      DT_ts_p[log_ret >= sd(log_ret, na.rm = TRUE)*CUTOFF, log_ret := NA]
      DT_ts_p[, log_ret := imputeTS::na_locf(log_ret, na_remaining = "rev")]
    } else {
      print("Warning: cutoff not numeric, no action taken")
    }
  }

  names(DT_ts_p)[1] <- "t"
  return(DT_ts_p)
}
