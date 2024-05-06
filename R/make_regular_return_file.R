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
#' @param FREQ The highest possible frequency. Defaults to 1 for a frequency of 1 second.
#'
#' @return Outputs a regularized return file with appropriate frequency.

#' @export
make_regular_return_file <- function(DATA, delts = c(1, 5, 10, 15), full_day = 0.95*86400, IMPUTATION = FALSE, T_large = 1, ALIGN = TRUE, FREQ = 1){
  DT_tmp <- DATA

  # Ensure DT_tmp is a data.table
  if (!"h" %in% names(DT_tmp)) {
    DT_tmp[, "h" := hour(t)]
  } # if no 'h' column available

  N <- nrow(DT_tmp)
  DT_ret <- data.table()

  # Check if the number of observations meets or exceeds what is considered a full day
  for (i in 1:length(delts)) {
    delta <- delts[i]
    # Check if the number of observations is enough for the current frequency 'd'
    if (N >= full_day * (1/delta)) {
      DT_ret <- make_return_file(DT_tmp, FREQ = delta, IMPUTATION = IMPUTATION, ALIGN = ALIGN)
      DT_ret[, freq := delta]
      DT_ret[, T_large := T_large]
      return(DT_ret)
    }
  }
  # If no frequency condition is met, possibly return an empty data.table or a message
  warning("No suitable frequency found. Returning empty data.table.")
  return(DT_ret)
}

