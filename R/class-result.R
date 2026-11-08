#' Base class for results
#'
#' Every result object in the package inherits from `HFResult`, which records
#' the method that produced it and the source it implements.
#'
#' @param method Identifier of the estimator or test, a single string.
#' @param reference Short citation of the source paper, a single string.
#' @export
HFResult <- new_class(
  "HFResult",
  abstract = TRUE,
  properties = list(
    method = class_character,
    reference = class_character
  ),
  validator = function(self) {
    collect_msgs(
      if (length(self@method) != 1L) "@method must be a single string.",
      if (length(self@reference) != 1L) "@reference must be a single string."
    )
  }
)

#' Integrated-variance estimate
#'
#' Every estimator behind [integrated_variance()] returns an `IVEstimate`.
#' `value` is the integrated variance over the observation window, that is,
#' on the scale of a sum of squared log returns. `sqrt(value)` is therefore
#' the volatility per window.
#'
#' @param value Estimated integrated variance over the window.
#' @param noise_variance Estimated variance of the microstructure noise, or
#'   `NA` when the estimator does not produce one.
#' @param jump_robust Is the estimator consistent for the continuous part of
#'   the quadratic variation in the presence of jumps?
#' @param n Number of increments used.
#' @param tuning Named list of the tuning parameters actually used.
#' @inheritParams HFResult
#' @export
IVEstimate <- new_class(
  "IVEstimate",
  parent = HFResult,
  properties = list(
    value = class_double,
    noise_variance = new_property(class_double, default = NA_real_),
    jump_robust = class_logical,
    n = class_integer,
    tuning = class_list
  ),
  validator = function(self) {
    collect_msgs(
      if (length(self@value) != 1L) "@value must be a single number.",
      if (length(self@noise_variance) != 1L) "@noise_variance must be a single number.",
      if (length(self@jump_robust) != 1L || is.na(self@jump_robust)) "@jump_robust must be TRUE or FALSE.",
      if (length(self@n) != 1L || is.na(self@n) || self@n < 1L) "@n must be a positive integer."
    )
  }
)

method(print, IVEstimate) <- function(x, ...) {
  cat(sprintf("<IVEstimate> %s (%s)\n", x@method, x@reference))
  cat(sprintf("  integrated variance: %.6g  (volatility per window: %.6g)\n",
              x@value, sqrt(max(x@value, 0))))
  if (!is.na(x@noise_variance)) {
    cat(sprintf("  noise variance:      %.6g\n", x@noise_variance))
  }
  cat(sprintf("  jump-robust: %s | n = %d\n", if (x@jump_robust) "yes" else "no", x@n))
  invisible(x)
}

method(as.data.frame, IVEstimate) <- function(x, ...) {
  data.frame(
    method = x@method, value = x@value, volatility = sqrt(max(x@value, 0)),
    noise_variance = x@noise_variance, jump_robust = x@jump_robust, n = x@n,
    stringsAsFactors = FALSE
  )
}
