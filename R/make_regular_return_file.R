#' Make regular return file
#'
#' Helper function to create a regular return file
#'
#' @import data.table
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param delts Frequencies that should be tested for subsampling the regularized data, where e.g. a value of 5 means that the data is subsampled every 5 seconds. Defaults to `c(5, 10, 15)`.
#' @param full_day How many observations in seconds does a full day have? Defaults to `0.95*86400`.
#' @param IMPUTATION Should missing values be imputed? If TRUE, then `na.locf` will be used. Defaults to FALSE.
#' @param T_large The number of trading days. Defaults to 1 for one observed day.
#' @param ALIGN Should the data be aligned to round timestamps? Defaults to TRUE.
#'
#' @return Outputs a regularized return file with appropriate frequency.

#' @export
make_regular_return_file <- function(DATA, delts = c(5, 10, 15), full_day = 0.95*86400, IMPUTATION = FALSE, T_large = 1, ALIGN = TRUE, FREQ = 1){
  DT_tmp <- DATA
  if (!"h" %in% names(DT_tmp)) DT_tmp[, "h" := hour(t)] # if no h available

  N <- nrow(DT_tmp)
  DT_ret <- data.table()

  if (N >= full_day) {
    DT_ret <- make_return_file(DT_tmp, FREQ = FREQ, IMPUTATION = IMPUTATION, ALIGN = ALIGN)
    DT_ret[, freq := FREQ]
    DT_ret[, T_large := T_large]
    return(DT_ret)

    # if N is greater or equal to 75% of a c(5, 10, 15) interval convert the file to that frequency
  } else {
    for (i in 1:length(delts)) {
      d <- delts[i]
      if (N >= full_day*1/d) {
        DT_ret <-	make_return_file(DT_tmp, FREQ = d, IMPUTATION = IMPUTATION, ALIGN = ALIGN)
        DT_ret[, freq := d]
        DT_ret[, T_large := T_large]
        d <- delts[1]
        return(DT_ret)
      }
    }
  }
}
