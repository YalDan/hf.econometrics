#' Split by ID
#'
#' Helper function to split data by ID and make it regular for computing the test statistic.
#'
#' @import data.table
#'
#' @inheritParams make_regular_return_file
#' @inheritParams remove_bounceback

#' @export
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

  print(Sys.time());print("Removing bounceback outliers")

  ## remove bouncebacks ##
  DT_split <- split(remove_bounceback(DT_split, IMPUTATION = IMPUTATION), by = c("date", "s"))
  return(DT_split)
}
