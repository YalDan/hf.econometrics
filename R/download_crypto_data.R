download_crypto_data <- function(LINK, PATH ="./data/raw/csv_dump/"){
  paste("Opening path", PATH)
  if (length(sort(list.files(path = PATH, pattern = "*.csv"))) == 0) {
      print("Downloading and saving data...")
      DT_CRYPTO <- fread(LINK)
      filename <- gsub("https://cryptotrades-exports.s3.eu-central-1.amazonaws.com/trade-exports/", "", LINK)
      target_path <- paste(PATH, filename, sep = "")
      fwrite(DT_CRYPTO, target_path)
    } else {
      print(paste("Error:", PATH, "not empty. No files downloaded. Do you really want to redownload the data? If that's the case please make sure the target directory is empty."))
    }
}