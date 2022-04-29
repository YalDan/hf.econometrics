## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

split_by_id <- function(DATA,
                        IMPUTATION = FALSE,
                        T_large = 1,
                        full_day = 0.95*86400,
                        delts = c(5, 10, 15),
                        FREQ = 1,
                        ALIGN = TRUE){
  if (FREQ == 1) {
    print("Warning: data frequency defaulted to 1. Is this intended behavior?")
  }
  ## split data.table ##
  # by id
  if (!"h" %in% names(DATA)) DATA[, "h" := hour(t)] # if no h available
  if (!"q" %in% names(DATA)) DATA[, "q" := NA] # if no q reported

  print(Sys.time());print("Splitting data")
  DT_split <- split(DATA[, .(date, "p" = mean(p), "q" = sum(q)), by = c("t", "s")], by = c("date", "s"))

  # only keep time series that can be aggregated to at least x mins intervals
  DT_split <- DT_split[which(lapply(DT_split,function(x) nrow(x) >= (full_day/max(delts))) == TRUE)]
  ##

  ##
  progress <- round(quantile(1:length(DT_split), probs = seq(0,1,0.05)))
  ##


  ## transform tick data files to regularly spaced return series w.r.t. number of observations##

  print("Making returns")

  DT_split <- lapply(1:length(DT_split), function(x) {
    if (x %in% progress) {print(Sys.time()); print(progress[which(progress == x)])}
    DT_tmp <- DT_split[[x]]
    return(make_regular_return_file(DATA = DT_tmp,
                                    delts = delts,
                                    full_day =  full_day,
                                    IMPUTATION = IMPUTATION,
                                    FREQ = FREQ,
                                    ALIGN = ALIGN))
  })
  ##

  ## only keep non-empty list entries (yes, this discards all time series that are not "high frequency") ##
  # DT_split <- DT_split[which(lapply(DT_split,function(x) nrow(x) > full_day/15) == TRUE)]
  ##

  print(Sys.time());print("Removing bounceback outliers")

  ## remove bouncebacks ##
  DT_split <- split(remove_bounceback(DT_split, IMPUTE = IMPUTATION), by = c("date", "s"))
  return(DT_split)
}
