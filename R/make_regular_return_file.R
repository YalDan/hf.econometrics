## install and load packages ##
libraries = c("data.table", "xts")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

make_regular_return_file <- function(DATA, delts = c(5, 10, 15), full_day = 0.95*86400, IMPUTATION = FALSE, T_large = 1, ALIGN = TRUE, FREQ){
  DT_tmp <- DATA
  if (!"h" %in% names(DT_tmp)) DT_tmp[, "h" := hour(t)] # if no h available
  
  N <- nrow(DT_tmp)
  DT_ret <- data.table()
  
  if (N >= full_day) {
    DT_ret <- make_return_file(DT_tmp, FREQ = FREQ, IMPUTE = IMPUTATION, align = ALIGN)
    DT_ret[, freq := FREQ] 
    DT_ret[, T_large := T_large]
    return(DT_ret)
    
    # if N is greater or equal to 75% of a c(5, 10, 15) interval convert the file to that frequency
  } else {
    for (i in 1:length(delts)) {
      d <- delts[i]
      if (N >= full_day*1/d) { 
        DT_ret <-	make_return_file(DT_tmp, FREQ = d, IMPUTE = IMPUTATION, align = ALIGN)
        DT_ret[, freq := d] 
        DT_ret[, T_large := T_large]
        d <- delts[1]
        return(DT_ret)
      }  
    }
  }
}