## install and load packages ##
libraries = c("data.table", "lubridate") 
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

## make sure data directories exist ##
make_data <- function(wti_data_path = "https://box.hu-berlin.de/f/f108288d2ebd451c9018/?dl=1",
                      crypto_data_path = "https://cryptotrades-exports.s3.eu-central-1.amazonaws.com/trade-exports/cce111bc-6c8b-4412-bde6-bfc238ec9aca.csv",
                      clean_data_path = "./data/clean/"){
  make_clean_data_directory(clean_data_path)
  ## ##
  
  if (file.exists("./data/clean/DT_wti_cl.csv")){
      print("File found in './data/clean/DT_wti_cl.csv'. Loading it now ")
      DT_wti <- fread("./data/clean/DT_wti_cl.csv")
    } else {
    
    #### preprocess WTI data ####
    if (!file.exists("./data/raw/DT_wti.csv")) {
      print(paste("No file found in './data/raw/DT_wti.csv'. Downloading now from specified source", wti_data_path))
      DT_wti <- fread(wti_data_path)
    } else {
      print("File found in './data/raw/DT_wti.csv'. Loading it now. Is this what you wanted? If not, please delete the file and restart.")
      DT_wti <- fread("./data/raw/DT_wti.csv")
    }
    
    DT_wti[, "Timestamp" := lubridate::dmy_hm(Timestamp, tz = "UTC")]
    DT_wti[, "Date" := lubridate::ymd(stringr::str_sub(Timestamp, 1,10))]
    DT_wti <- data.table("s" = "wtiusd", DT_wti[,c("Timestamp", "Date", "Trade Close", "Trade Volume")])
    names(DT_wti) <- c("s", "t", "date", "p", "q")
    DT_wti[, `:=`("id" = "refinitiv", "id_s" = "refinitiv_wtiusd")]
    DT_wti[date <= as.Date("2022-02-04"), t := t - 60] # do this for convenience
    filename_wti <- "DT_wti_cl.csv"
    if (!file.exists(filename_wti)) {
      print(paste("Writing file", filename_wti, "..."))
      fwrite(DT_wti, paste(clean_data_path, filename_wti, sep = ""))
    }
  }
  #### ###
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
  return(list("WTI" = DT_wti, "CRYPTO" = DT_crypto))
}