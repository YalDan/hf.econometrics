## install and load packages ##
libraries = c("data.table", "xts", "quantmod", "lubridate")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

make_return_file <- function(DATA, FREQ, IMPUTE = FALSE, CUTOFF = 10, align = TRUE){
  S <- DATA
  
  #
  start <- paste(as.Date(first(S[, t])),  " ", first(S[, h]), ":00:00", sep = "")
  start <- lubridate::ymd_hms(start, tz = "UTC")
  end <- paste(as.Date(first(S[, t])),  " ", last(S[, h]), ":59:59", sep = "")
  end <- lubridate::ymd_hms(end, tz = "UTC") - FREQ + 1 # - FREQ + 1 to make sure it's aligned by the data's frequency 
  #
  
  #
  ts_p <- xts(S$p, order.by = S$t, unique = FALSE)
  if (align) {
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
  
  if (IMPUTE == TRUE) {
    DT_ts_p[, p := na.locf(p, na.rm = F)]
    DT_ts_p[, p := na.locf(p, na.rm = F, fromLast = T)]
    DT_ts_p[, log_ret := quantmod::Delt(p, type = "log")]
    DT_ts_p[1, log_ret := 0]
  } 
  
  if (IMPUTE == FALSE) {
    DT_ts_p <- DT_ts_p[!is.na(p)]
    DT_ts_p[, log_ret := quantmod::Delt(p, type = "log")]
  }
  
  if (!is.na(CUTOFF)) {
    if (is.numeric(CUTOFF)) {
      DT_ts_p[log_ret >= sd(log_ret, na.rm = TRUE)*CUTOFF, log_ret := NA]
      DT_ts_p[, log_ret := na.locf(log_ret, na.rm = F)]
      DT_ts_p[, log_ret := na.locf(log_ret, na.rm = F, fromLast = T)]
    } else {
      print("Warning: cutoff not numeric, no action taken")
    }
  }
  
  
  
  # DT_ts_p <- DT_ts_p[!is.na(log_ret)]
  names(DT_ts_p)[1] <- "t"
  return(DT_ts_p)
}
