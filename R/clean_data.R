## install and load packages ##
libraries = c("data.table", "lubridate")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##


clean_data <- function(DATA, id = NA){
  DT_tmp <- DATA

  id <- DT_tmp[1,id]
  
  print(Sys.time())
  print(paste("processing", id))
  
  if ("q" %in% names(DT_tmp)) {
    DT_tmp <- DT_tmp[, c("t", "s", "p", "q", "id")]
  } else {
    DT_tmp <- DT_tmp[, c("t", "s", "p", "id")]
  }
  
  print(Sys.time())
  print(paste("get symbol to lowercase and remove redundant content"))
  DT_tmp[, "s" := tolower(s)]
  DT_tmp[, "s" := gsub("-", "", s)]
  DT_tmp[, "s" := gsub("usdt", "usd", s)]
  
  print(Sys.time())
  print(paste("adjust timestamp"))
  DT_tmp[, "t" := lubridate::as_datetime(t/1e3,  tz = "UTC")]
  
  # DT_tmp #
  if (!is.na(id)) {
    if (id == "binance") {
      
      print(Sys.time())
      print(paste("rename symbols"))
      
      DT_tmp[s %in% "iotausd", s := gsub("iotausd", "iotusd", s)]
      
    } else if (id == "hitbtc") {
      
      print(Sys.time())
      print(paste("rename symbols"))
      
      DT_tmp[s %in% "iotausd", s := gsub("iotausd", "iotusd", s)]
      DT_tmp[s %in% "dashusd", s := gsub("dashusd", "dshusd", s)]
      
    } else if (id == "poloniex") {
      
      print(Sys.time())
      print(paste("rename symbols"))
      
      DT_tmp[s %in% "dashusd", s := gsub("dashusd", "dshusd", s)]

    } else {
      print(paste("ID",id,"not found, no extra steps taken."))
    }
  }

  return(DT_tmp)
}

