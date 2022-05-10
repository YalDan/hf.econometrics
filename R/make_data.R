#' Load and split
#'
#' Helper function to download data from the BRC
#'
#' @import data.table
#' @import lubridate
#'

#' @export
make_data <- function(crypto_data_path = NA,
                      clean_data_path = "./data/clean/"){
  make_clean_data_directory(clean_data_path)

  if (file.exists("./data/clean/DT_crypto_cl.csv")){
    print("File found in './data/clean/DT_crypto_cl.csv'. Loading it now ")
    DT_crypto <- fread("./data/clean/DT_crypto_cl.csv")
  } else if (length(sort(list.files(path = './data/raw/csv_dump/', pattern = "*.csv"))) > 0){
    print("Files found in './data/raw/csv_dump/'. Loading them now ")
    DT_crypto <- load_csv_dump()
    filename_crypto <- "DT_crypto_cl.csv"
    if (!file.exists(filename_crypto)) {
      print(paste("Writing file", filename_crypto, "..."))
      fwrite(DT_crypto, paste(clean_data_path, filename_crypto, sep = ""))
    }
  } else {
    #### preprocess Crypto data ####
    download_crypto_data(crypto_data_path)
    DT_crypto <- load_csv_dump()
    filename_crypto <- "DT_crypto_cl.csv"
    if (!file.exists(filename_crypto)) {
      print(paste("Writing file", filename_crypto, "..."))
      fwrite(DT_crypto, paste(clean_data_path, filename_crypto, sep = ""))
    }
  }
  return(DT_crypto)
}
