make_clean_data_directory <- function(PATH = "./data/clean/") {
  dir_name <- PATH
  
  if (!dir.exists(dir_name)) {
    print(paste("Created directory", dir_name))
    dir.create(dir_name, recursive = TRUE)
  }
  
}
