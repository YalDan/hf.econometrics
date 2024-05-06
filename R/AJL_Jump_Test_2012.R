#' Ait-Sahalia, Jacod & Li Jump Test
#'
#' Calculates the jump test statistic as per Ait-Sahalia, Jacod & Li (2012)
#'
#' @import data.table
#'
#' @param DATA A data.table with structure as provided in the example.
#' @param deltan The frequency of the data. Defaults to 1/365.25/86400 (i.e. 1 second in a 365.25 trading days interval).
#' @param kn The block size for preaveraging. Defaults to 100.
#' @param T_large The length of T. Defaults to 1/365.25
#' @param trunc_level: Value for the truncation levels for estimating asymptotic variances. Defaults to 5.
#' @param variance_flag Should the variance be estimated? Defaults to TRUE.
#' @param sign_level Significance level for the test statistic. Defaults to 0.05.
#'
#' @return DT_vvec: The V vectors from pre-averaging
#' @return S_stdc: The standardized noise robust test statistic
#' @return sigmabar: The average volatility
#' @return un: The truncation level
#' @return variancehat: The estimated variance
#' @return Mstargg: The Mstar values as in the original paper
#' @return Mstargh: The Mstar values as in the original paper
#' @return Mstarhh: The Mstar values as in the original paper
#' @return sign_level: The signficance level
#' @return pvalue: The p-value
#' @return cv: The critical value
#' @return Jump_indicator: The jump indicator

#' @export
AJL_JumpTest_2012 <- function(DATA, deltan = 1/365.25/86400, kn = 100, T_large = 1/365.25, trunc_level = 5, variance_flag = TRUE, sign_level = 0.05){

  dz <- DATA[!is.na(log_ret), log_ret]
  # Set up constants
  gbar2 = 1/12
  phibar2 = 1/12
  phiprimebar2 = 1
  gamma = 2
  gammap = 2
  gammapp = 2
  Aprimegg = c(
    3.599999960562231,
    1.221424577286807,
    0.370635624815616,
    0.035039285812380,
    0.002315373900701)

  Aprimegh = c(
    2.725769745686566,
    0.589334298318853,
    0.122686505762394,
    0.007755996403950,
    0.000347923390644)

  Aprimehh = c(
    30.217754077981802,
    2.535050327092127,
    0.188268743583475,
    0.004415379915876,
    0.000072391168973)

  S_stdc = c()

  # Compute the ratio stat
  yg = YbarYhat(dz,kn,1)
  ybarg <- yg$Ybar
  yhatg <- yg$Yhat

  yh = YbarYhat(dz,kn,2)
  ybarh <- yh$Ybar
  yhath <- yh$Yhat

  vvec_p4_g = Vvec(ybarg,yhatg,4)
  vvec_p4_h = Vvec(ybarh,yhath,4)
  vbar_p4_g = vvec_p4_g[1] - 3 * vvec_p4_g[2] + 0.75 * vvec_p4_g[3]
  vbar_p4_h = vvec_p4_h[1] - 3 * vvec_p4_h[2] + 0.75 * vvec_p4_h[3]

  S = vbar_p4_g / vbar_p4_h / gammap
  S_uncorrected = vvec_p4_g[1] / vvec_p4_h[1] / gammap

  DT_vvec <- data.table()
  DT_vvec[, vvec_p4_g := vvec_p4_g]
  DT_vvec[, vvec_p4_h := vvec_p4_h]
  DT_vvec[, vbar_p4_g := vbar_p4_g]
  DT_vvec[, vbar_p4_h := vbar_p4_h]

  if ("id" %in% names(DATA)) {
    id <- head(DATA[, id], 1)
  } else {
    id <- NA
  }
  DT_res <- data.table("date" = head(DATA[, d],1), "s" = head(DATA[, s],1), id, S,S_uncorrected)

  # Compute the standard error
  if (variance_flag) {

    # calibrate the average vol
    vvec_p2 = Vvec(ybarg,yhatg,2)
    sigmabar = sqrt((vvec_p2[1] - vvec_p2[2] / 2) / kn / gbar2 /T_large)

    # compute the truncation level
    un = trunc_level * sqrt(gbar2 * kn) * sigmabar * deltan^0.49

    # compute the variance
    vvec_trunc_2p_phi = Vvec_trunc(ybarg,yhatg,8,un)
    Mstargg = Mstar(deltan,kn,4,Aprimegg,phibar2,phiprimebar2,vvec_trunc_2p_phi)
    Mstargh = Mstar(deltan,kn,4,Aprimegh,phibar2,phiprimebar2,vvec_trunc_2p_phi)
    Mstarhh = Mstar(deltan,kn,4,Aprimehh,phibar2,phiprimebar2,vvec_trunc_2p_phi)
    variancehat = (Mstargg - 2 * gamma^2 * Mstargh + gamma^4 * Mstarhh) / (vbar_p4_g/gammapp)^2
    S_stdc = (S - gammapp) / deltan^0.25 / sqrt(variancehat)
    DT_res[, S_stdc := S_stdc]
    DT_res[, sigmabar := sigmabar]
    DT_res[, un := un]
    DT_res[, variancehat := variancehat]
    DT_res[, Mstargg := Mstargg]
    DT_res[, Mstargh := Mstargh]
    DT_res[, Mstarhh := Mstarhh]
    DT_res[, sign_level := sign_level]
    DT_res[, pvalue := 2 * pnorm(-abs(S_stdc))]
    DT_res[, cv := qnorm(1-sign_level/2)]
    DT_res[!S_stdc %between% c(-1*cv,cv), Jump_indicator := 1]
    DT_res[is.na(Jump_indicator), Jump_indicator := 0]
  }

  return(list("vvec" = DT_vvec, "result" = DT_res))
}
