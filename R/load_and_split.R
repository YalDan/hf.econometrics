## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

#### settings ####
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

## install and load packages ##
libraries = c("data.table", "lubridate")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

#### settings ####
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

load_and_split <- function(wti_data_path = "https://box.hu-berlin.de/f/f108288d2ebd451c9018/?dl=1",
                           crypto_data_path = "https://cryptotrades-exports.s3.eu-central-1.amazonaws.com/trade-exports/9b3d6893-26e0-4760-aa34-19bcdc8508e0.csv",
                           clean_data_path = "./data/clean/"){
  
  ### load aggregate dataset ###
  DT_list <- make_data(wti_data_path, crypto_data_path, clean_data_path)
  DT_wti_split_list <- list("impute" = split_by_id(DT_list$WTI, delts = c(60,60*2,60*5), FREQ = 60, full_day = 60*23*0.9, IMPUTATION = TRUE, ALIGN = FALSE),
                            "no_impute" = split_by_id(DT_list$WTI, delts = c(60,60*2,60*5),  FREQ = 60, full_day = 60*23*0.9, IMPUTATION = FALSE, ALIGN = FALSE))

  DT_crypto_split_list <-  list("impute" = split_by_id(DT_list$CRYPTO, delts = c(60,60*2,60*5), FREQ = 60, full_day = 60*24*0.9, IMPUTATION = TRUE, ALIGN = TRUE),
                                "no_impute" =  split_by_id(DT_list$CRYPTO, delts = c(60,60*2,60*5), FREQ = 60, full_day = 60*24*0.9, IMPUTATION = FALSE, ALIGN = TRUE))
  
  
  return(list("DT_wti_split_list" = DT_wti_split_list, "DT_crypto_split_list" = DT_crypto_split_list))
}