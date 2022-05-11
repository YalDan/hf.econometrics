#' Remove bounceback outliers
#'
#' Helper function to remove bounceback outliers.
#'
#' @import data.table
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param BOUNCE_CUTOFF The threshold of log return size for which bounceback outliers should be removed. Defaults to 0.001, s.t. smaller returns are not checked for bounceback outliers.
#' @param IMPUTATION Should missing values be imputed? If TRUE, then `na.locf` will be used. Defaults to FALSE.
#'
#' @return Returns the data without bounceback outliers.

#' @export
remove_bounceback <- function(DATA, BOUNCE_CUTOFF = 0.001, IMPUTATION = FALSE){

  if(is.data.table(DATA)) {
    DT_tmp <- copy(DATA)
  } else if(is.list(DATA)){
    DT_tmp <- rbindlist(DATA)
  } else {
    message(paste("Error: incorrect filetype:", class(DATA)))
  }

  if(! "id" %in% names(DT_tmp)) DT_tmp[, id := NA] # add unique ID if necessary

  DT_tmp[, unique_id := 1:.N]
  DT_tmp[, daily_id := 1:.N, by = c("s", "id", "date")]

  DT_bounce <- split(DT_tmp[!log_ret %between% c(BOUNCE_CUTOFF*-1, BOUNCE_CUTOFF)], by = c("s", "id", "date"))

  bounce_ids <- lapply(seq_along(DT_bounce), function(x){

    # subset data
    DT_tmp_bounce <- DT_bounce[[x]]

    # check difference between observations
    DT_tmp_bounce[, diff_obs := c(NA, diff(log_ret))]

    # check distance between IDs
    DT_tmp_bounce[, lag_daily_id := shift(daily_id, 1)]

    # difference between returns larger than cutoff*2 and consecutive observations?
    bounce_ids <- DT_tmp_bounce[abs(diff_obs) >= BOUNCE_CUTOFF*2 & (daily_id - lag_daily_id) == 1]

    # extract respective IDs
    bounce_ids <- unlist(bounce_ids[, .(daily_id, lag_daily_id)])

    # returns observations to be removed
    DT_tmp_bounce[daily_id %in% bounce_ids, unique_id]
  })

  bounce_ids <- sort(unlist(bounce_ids))

  DT_tmp[unique_id %in% bounce_ids, log_ret := NA]

  if (IMPUTATION == TRUE){
    DT_tmp[1, log_ret := 0]
    DT_tmp[, log_ret := na.locf(log_ret, na.rm = F)]
    DT_tmp[, log_ret := na.locf(log_ret, na.rm = F, fromLast = T)]
    return(DT_tmp)
  } else {
    return(DT_tmp[!is.na(log_ret)])
  }
}
