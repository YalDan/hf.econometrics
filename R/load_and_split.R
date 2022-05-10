#' Load and split
#'
#' Helper function to process data from the BRC

#' @export
load_and_split <- function(crypto_data_path = NA,
                           clean_data_path = "./data/clean/"){

  ### load aggregate dataset ###
  DT_list <- make_data(crypto_data_path, clean_data_path)
  DT_crypto_split_list <-  list("impute" = split_by_id(DT_list, delts = c(60,60*2,60*5), FREQ = 60, full_day = 60*24*0.9, IMPUTATION = TRUE, ALIGN = TRUE),
                                "no_impute" =  split_by_id(DT_list, delts = c(60,60*2,60*5), FREQ = 60, full_day = 60*24*0.9, IMPUTATION = FALSE, ALIGN = TRUE))

  return(DT_crypto_split_list)
}
