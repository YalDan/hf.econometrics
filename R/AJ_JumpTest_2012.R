## install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

AJ_JumpTest <- function(DATA, alpha = 0.1){
  
   # will loop to compute the values of B(p,u,delta) for all those values below
  pvec <- seq(from = 0, to = 6, by = 0.25)
  
  # atrunc is expressed in terms of numbers of stdev of the continuous part;  use 10^10 for no truncation
  atruncvec <- c(2:20, 25, 30, 40, 50, 60, 75, 100, 10^10)
  
  # specify possible gammas
  gammavec <- seq(from = 1, to = 3, by = 0.25)
  
  # specify possible deltas; for simplicity of coding, make sure those are multiples of timeinterval = 5 in the dataset
  deltavec <- c(1, 5, 10, 15, 30, 45, 60, 120, 300, 600, 1800) 
  
  # specify possible ks
  kvec <- 1:3
  
  # read data
  tmp_DT <- DATA
  
  # extract ID if available
  id_dummy <- NA
  if ("id" %in% names(tmp_DT)) {id_dummy <- tmp_DT[,id][1]}
  
  
  if (nrow(tmp_DT) == 86400){
    subs <- seq(0,nrow(tmp_DT), by = 5)
    subs[1] <- 1
    tmp_DT <- tmp_DT[subs]
  }
  
  dX <- tmp_DT[!is.na(log_ret), log_ret] # both X and dX have length n
  x0 <- log(tmp_DT[!is.na(log_ret), p][1]) # initial value
  
  T_large <- 1/365.25
  n <- length(dX)
  
  if(n < 86400/5) {deltavec <- deltavec[2:length(deltavec)]}
  if(n < 86400/10) {deltavec <- deltavec[3:length(deltavec)]}
  if(n < 86400/15) return(data.table())
  
  
  par_grid <- setDT(expand.grid("p" = pvec,
                                "a" = atruncvec,
                                "gamma" = gammavec,
                                "delta" = deltavec,
                                "k" = kvec))
  
  par_grid[, nblag_j := delta/sort(unique(par_grid$delta)[1])]
  par_grid[, delta_j := delta/(6.5*60*60*365.25)]
  
  N_nblagj <- sort(unique(par_grid$nblag_j))
  
  
  for (i in seq_along(N_nblagj)){
    X <- x0 + cumsum(dX) # do this instead of X=log(price) to avoid including the large overnight returns
    nblagj <- unique(par_grid[nblag_j %in% sort(unique(par_grid$nblag_j))[i]]$nblag_j)
    deltaj <-  unique(par_grid[nblag_j %in% sort(unique(par_grid$nblag_j))[i]]$delta_j)
    dXobsj <- sort(abs(X[seq(from = (nblagj+1), to = n, by = nblagj)] - X[seq(from = 1, to = (n-nblagj), by = nblagj)])) # length(dXobsj) is equal to nj-1
    sigma_hat <- sqrt( (1/T_large) * sum( (abs(dXobsj)^2) * ( abs(dXobsj) <= 3 * 0.6 * deltaj^(1/2) )) )
    par_grid[nblag_j %in% nblagj, sigmahat := sigma_hat]
  }
  
  par_grid[, nblag_jk := nblag_j*k]
  par_grid[, thresh := gamma * a * sigmahat * sqrt(delta_j)]
  N_nblagjk <- sort(unique(par_grid$nblag_jk))
  
  list_dXobsjk <- lapply(seq_along(N_nblagjk), function(i){
    nblagjk <- unique(par_grid[nblag_jk %in% sort(unique(par_grid$nblag_jk))[i]]$nblag_jk)
    deltaj <-  unique(par_grid[nblag_jk %in% sort(unique(par_grid$nblag_jk))[i]]$delta_j)
    
    sort(abs(X[seq(from = (nblagjk+1), to = n, by = nblagjk)] - X[seq(from = 1, to = (n-nblagjk), by = nblagjk)]))
  })
  
  result_list <- vector(mode="list", length = length(N_nblagjk))
  
  for (i in seq_along(list_dXobsjk)){
    sa_dX_i <- list_dXobsjk[[i]]
    
    par_grid_tmp <- par_grid[nblag_jk %in% N_nblagjk[i]]
    
    test1 <- rep(NA, nrow(par_grid_tmp))
    loc <- findInterval(par_grid_tmp$thresh, sa_dX_i)
    loc[loc == 0] <- NA  # Handle threshold smaller than everything in dX_i
    
    for (pval in unique(par_grid_tmp$p)) {
      this.p <- par_grid_tmp$p == pval
      cs_dX_i_p <- cumsum(sa_dX_i^pval)
      test1[this.p] <- cs_dX_i_p[loc[this.p]]
    }
    test1[is.na(test1)] <- 0 
    
    par_grid_tmp[,B := test1]
    result_list[[i]] <- par_grid_tmp
  }
  
  ### calculate SJ ###
  pvec_SJ <- pvec[which( (pvec >= 2.5 & pvec <= 6) )]
  deltavec_SJ <- deltavec[which(deltavec <= 120)]
  kvec_SJ <- kvec[which(kvec >= 2)]
  
  SJ <- rbindlist(result_list)[gamma == gammavec[1] & a == max(par_grid$a) & p %in% pvec_SJ & k %in%  kvec_SJ & delta %in% deltavec_SJ]
  B_lower <- rbindlist(result_list)[gamma == gammavec[1] & a == max(par_grid$a)& p %in% pvec_SJ & k == kvec[1] & delta %in% deltavec_SJ]

  for (i in unique(kvec_SJ)){
    SJ[k == i, SJ := B / B_lower$B]
  }
  ###
  
  ### calculate SFA ###
  pvec_SFA <- pvec[which( (pvec >= 2.5 & pvec <= 6) )]
  atruncvec_SFA <- atruncvec[which( (atruncvec > 5 & atruncvec < 10) )]
  deltavec_SFA <- deltavec[which(deltavec <= 120)]
  kvec_SFA <- kvec[which(kvec >= 2)]
  
  # B[[pvec_SFA[pindex],atruncvec_SFA[aindex],1,deltavec_SFA[dindex],kvec_SFA[kindex]]] / B[[pvec_SFA[pindex],atruncvec_SFA[aindex],1,deltavec_SFA[dindex],1]]
  SFA <- rbindlist(result_list)[gamma == gammavec[1] & a %in% atruncvec_SFA  & p %in% pvec_SFA & k %in%  kvec_SFA & delta %in% deltavec_SFA]
  B_lower <- rbindlist(result_list)[gamma == gammavec[1] & a %in% atruncvec_SFA  & p %in% pvec_SFA & k == kvec[1]  & delta %in% deltavec_SFA]
  
  for (i in unique(kvec_SFA)){
    SFA[k == i, SFA := B / B_lower$B]
  }
  ###
  
  ## add limits and indicators ##
  DT_SJ <- data.table("date" =  tmp_DT[,date][1],
                      "id" =  id_dummy,
                      "s" =  tmp_DT[,s][1],
                      SJ)
  DT_SJ[, limit_noise := 1]
  DT_SJ[, limit_jump := k^(p/2-1)]
  ## 
  
  ## add limits and indicators ##
  DT_SFA <- data.table("date" =  tmp_DT[,date][1],
                       "id" =  id_dummy,
                       "s" =  tmp_DT[,s][1],
                       SFA)
  DT_SFA[, limit_infinite_activity := 1]
  DT_SFA[, limit_finite_activity := k^(p/2-1)]
  ##
  
  
  return(rbind(DT_SJ, DT_SFA, fill = TRUE))
}
