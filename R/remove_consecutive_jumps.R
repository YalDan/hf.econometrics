remove_consecutive_jumps <- function(DATA, RANGE_OBS = 3, split_vars = c("date", "hour", "s")){
  
  #### load data and split it ####
  split_jumps <- split(DATA[Jump_indicator != 0], by = split_vars)
  ###
  
  ###
  split_no_consec_jumps <- lapply(seq_along(split_jumps), function(x)
  {
    ## take subset ##
    DT_tmp <- split_jumps[[x]]
    
    ## reverse dataset to iterate backwards ##
    DT_tmp_rev <- DT_tmp[seq(dim(DT_tmp)[1],1),]
    
    ## preassign iterator ##
    if (nrow(DT_tmp_rev) > 1) {
      n_iter <- 1:(nrow(DT_tmp_rev) - 1)
    } else {
      n_iter <- 1
    }
    
    ## loop over all rows ##
    for (i in n_iter) {
      if(!is.na(DT_tmp_rev[i+1,count])){ # only iterate if last row is not reached
        
        #  remove all rows that contain a consecutive jump #
        # a consecutive jump is measured if jump is in the neighborhood of RANGE_OBS blocks
        if(DT_tmp_rev[i, count]-DT_tmp_rev[i+1, count] <= RANGE_OBS*(DT_tmp_rev[1,kM])){
          DT_tmp_rev <- DT_tmp_rev[!i]
        } 
      }
    }
    
    ## undo reversal ##
    DT_tmp <- DT_tmp_rev[seq(dim(DT_tmp_rev)[1],1),]
  })
  return(rbindlist(split_no_consec_jumps))
}
