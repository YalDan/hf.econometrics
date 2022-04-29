# install and load packages ##
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
invisible(lapply(libraries, library, quietly = TRUE, character.only = TRUE))
# ##

#### Settings ####
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English


AJ_09_variation <- function(DATA, k = 3, p = 4, gamma = 2, a = 10){
  ## Calculate sigma_hat according to Ait-Sahalia & Jacod (2009) "Jump Activity" ##
  
  DT_ts_p <- DATA
  # initialize dX
  dX <- DT_ts_p[!is.na(log_ret), log_ret] # both X and dX have length n
  x0 <- log(DT_ts_p[!is.na(log_ret), p][1]) # initial value
  
  
  # specify parameter
  delta <- 1
  PERIOD <- length(unique(hour(DT_ts_p$t))) / 24
  T_AJ <- 1/(365.25*PERIOD) # 1/365.25 = one calendar day, 1/252 = one exchange trading day, 21/252 = one month, 1/4 = one quarter, 1 = one year
  nblagj <- delta
  deltaj <- delta/(24*60*60*365.25) # measured in years, this is the value of delta corresponding to that jindex
  
  # initialize X
  X <- x0 + cumsum(dX) # do this instead of X=log(price) to avoid including the large overnight returns
  n <- length(X)
  dXobsj <- sort(abs(X[seq(from = (nblagj+1), to = n, by = nblagj)] - X[seq(from = 1, to = (n-nblagj), by = nblagj)])) # length(dXobsj) is equal to nj-1
  
  # calculate sigma_hat (realized bipower variation truncated)
  sigma_hat_as <- sqrt( (1/T_AJ) * sum( ((dXobsj)^2) * ( (dXobsj) <= 3 * 0.60 * deltaj^(1/2) )) )
  sigmahat <- sigma_hat_as
  ## ##
  
  nblagjk <- nblagj * k
  dXobsjk <- sort(abs(X[seq(from = (nblagjk+1), to = n, by = nblagjk)] - X[seq(from = 1, to = (n-nblagjk), by = nblagjk)]))
  
  sigma_hat <- sum( ((dXobsjk)^p) * ( (dXobsjk) < gamma * a * sigmahat * deltaj^(1/2) ))
  ## ##
  
  return(sigma_hat)
}
