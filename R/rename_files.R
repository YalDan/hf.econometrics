#' Make clean data directory
#'
#' Helper function to create data directories
#'
#' @import data.table

#' @export
rename_raw_files <- function(files, PATH){

  ## categorize files ##
  DT_colnames <- lapply(files, function(x) {
    print(Sys.time())
    print(paste("Reading column names of file ", x))
    names(cbind(fread(paste(PATH, x, sep = ""), nrows = 0)))
  })

  DT_colvals <- lapply(files, function(x) {
    print(Sys.time())
    print(paste("Reading column values of file ", x))
    cbind(fread(paste(PATH, x, sep = ""), nrows = 1))
  })
  ##

  DT_collection <- vector(mode = "list", length = length(files))

  pos_bitfinex <- which(lapply(DT_colnames, identical,  c("q", "p", "s", "t", "d")) == TRUE)
  pos_binance <- which(lapply(DT_colnames, identical,  c("q", "p", "s", "t", "d", "m", "seller_id", "buyer_id")) == TRUE)
  pos_cbpro <- which(lapply(DT_colnames, identical,  c("q", "p", "s", "t", "d", "maker_order_id", "taker_order_id", "side")) == TRUE)
  pos_poloniex <- which(lapply(DT_colnames, identical,  c("p", "s", "t", "d", "lowest_ask", "highest_bid")) == TRUE)
  pos_bitstamp <- which(lapply(DT_colnames, identical,  c("q", "p", "s", "t", "d", "side", "buy_id", "sell_id")) == TRUE)

  pos_hitbtc <- which(lapply(seq_along(DT_colnames), function(x){
    cond_hitbtc_okex <- all.equal(DT_colnames[[x]], c("q", "p", "s", "t", "d", "side")) == TRUE
    cond_okex <- grepl("-", DT_colvals[[x]]$s, fixed =  TRUE) == FALSE
    cond_all <- c(cond_hitbtc_okex, cond_okex)
    all(cond_all)
  }) == TRUE)

  pos_okex <- which(lapply(seq_along(DT_colnames), function(x){
    cond_hitbtc_okex <- all.equal(DT_colnames[[x]], c("q", "p", "s", "t", "d", "side")) == TRUE
    cond_okex <- grepl("-", DT_colvals[[x]]$s, fixed =  TRUE)
    cond_all <- c(cond_hitbtc_okex, cond_okex)
    all(cond_all)
  }) == TRUE)

  names(DT_collection)[pos_binance] <- "binance"
  names(DT_collection)[pos_bitfinex] <- "bitfinex"
  names(DT_collection)[pos_bitstamp] <- "bitstamp"
  names(DT_collection)[pos_cbpro] <- "cbpro"
  names(DT_collection)[pos_hitbtc] <- "hitbtc"
  names(DT_collection)[pos_okex] <- "okex"
  names(DT_collection)[pos_poloniex] <- "poloniex"

  file_names_new <- names(DT_collection)
  return(file_names_new)
}
