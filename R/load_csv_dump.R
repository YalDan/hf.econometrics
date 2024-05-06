#' Load CSV dump
#'
#' Helper function to load raw data
#'
#' @import data.table
#'

#' @export
load_csv_dump <- function(PATH = "./data/raw/csv_dump/"){

  ## get files ##
  files <- sort(list.files(path = PATH, pattern = "*.csv"))

  ## rename files ##
  file_names_new <- rename_raw_files(files, PATH)

  ## read files into list ##
  DT_collection <- lapply(seq_along(files), function(x) {
    print(Sys.time())
    print(paste("Reading file ", files[x]))
    DT_tmp <- fread(paste(PATH, files[x], sep = ""))
    DT_tmp[, "id" := file_names_new[x]]
  })

  ## clean the files ##
  DT_collection_cl <- lapply(DT_collection, clean_data)
  DT_collection_cl <- rbindlist(DT_collection_cl, fill = TRUE)
  setkey(DT_collection_cl, id, s, t)
  DT_collection_cl[, "id_s" := paste(id, s, sep = "_")]
  DT_collection_cl[, "date" := as.Date(t)]
  return(DT_collection_cl)
}
