#' hf.econometrics: high-frequency econometrics, implemented from the sources
#'
#' Every estimator and test in this package is implemented from its original
#' paper, and each function's documentation maps the code to that paper's
#' equations. Wherever a paper leaves a choice open, the default reproduces
#' the paper's own empirical or simulation setting, and any deviation is
#' listed under *Implementation choices*.
#'
#' @section Building blocks:
#' * [hf_series()] and [log_prices()]: typed input. Every method accepts
#'   either a numeric vector of log prices or an [HFSeries] object.
#' * [preaverage()], [preavg_power_variation()], [preavg_vbar()] and
#'   [preavg_rho()]: the pre-averaging machinery of Jacod, Podolskij and
#'   Vetter (2010) and Ait-Sahalia, Jacod and Li (2012).
#' * [weight_tent()], [weight_compress()] and [weight_integral()]:
#'   pre-averaging weight functions.
#'
#' @section Conventions:
#' Each call analyses one observation window, for example a day or an hour.
#' Unless a function says otherwise, the window has length `T = 1` and the
#' sampling interval is `delta_n = 1 / n`, so integrated variances are
#' expressed per window. Functions whose tuning constants carry time units
#' take an explicit `horizon` argument, which makes those units visible.
#'
#' @keywords internal
#' @import S7
#' @importFrom stats acf filter integrate median pnorm qnorm sd var
#' @importFrom utils head tail
"_PACKAGE"

# The `@` operator only dispatches on S7 objects from R 4.3.0 onwards.
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
