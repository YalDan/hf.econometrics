#' Drop consecutive detections
#'
#' Thins clusters of flags: the first flag of a cluster is kept, and every
#' later flag whose index lies within `range` of it is dropped. The first
#' flag beyond that range starts the next cluster.
#'
#' @section Paper mapping:
#' Saef et al. (2021, Section 5): "We determine consecutive jump detections
#' by tagging jumps that were detected within 10 moments after initial
#' detection and remove them from the data." With `index` set to the
#' Lee-Mykland block number (the test times \eqn{t_j}), `range = 10` and
#' `anchor = "first"` implement this sentence.
#'
#' @section Implementation choices:
#' * `anchor = "first"` (default) measures distances from the kept flag that
#'   opened the cluster, which is what "after initial detection" says. Flags
#'   at 100, 125 and 150 with `range = 30` keep 100 and 150.
#' * `anchor = "previous"` measures distances from the preceding flag, kept
#'   or not, so clusters chain: the same flags keep only 100. This is what
#'   the 0.1.0 code compared, but it deleted rows while iterating over them
#'   and so skipped comparisons.
#' * A flag at the same index as the anchor counts as within range. The
#'   input need not be sorted; flags are processed in index order.
#' * The computation touches only the flags. With `anchor = "first"` it jumps
#'   from one kept flag to the next with [findInterval()].
#'
#' @param index Positions of the candidate flags: numeric (for example block
#'   numbers or observation counts) or `POSIXct` (then `range` is in
#'   seconds), without missing values.
#' @param flag Logical vector of the same length; `TRUE` marks a detection.
#' @param range Non-negative distance, in the units of `index`, within which
#'   later flags are dropped.
#' @param anchor `"first"` or `"previous"`, see *Implementation choices*.
#' @return A logical vector like `flag`, with the dropped flags set to
#'   `FALSE`.
#' @references
#' Saef, D., Nagy, O., Sizov, S. and Hardle, W. K. (2021). Understanding
#' jumps in high frequency digital asset markets. arXiv:2110.09429.
#' @examples
#' drop_consecutive_flags(c(100, 110, 120, 200), rep(TRUE, 4), range = 30)
#' drop_consecutive_flags(c(100, 125, 150), rep(TRUE, 3), range = 30)
#' drop_consecutive_flags(c(100, 125, 150), rep(TRUE, 3), range = 30,
#'                        anchor = "previous")
#' @export
drop_consecutive_flags <- function(index, flag, range, anchor = c("first", "previous")) {
  anchor <- check_choice(anchor[1L], c("first", "previous"), "anchor")
  ti <- time_info(index, "index", sorted = FALSE)
  if (!is.logical(flag) || length(flag) != length(ti$value) || anyNA(flag)) {
    hf_abort("`flag` must be a logical vector without NA and of the same length as `index`.")
  }
  check_number(range, "range", lower = 0)
  if (!is.finite(range)) hf_abort("`range` must be finite.")
  out <- logical(length(flag))
  pos <- which(flag)
  if (!length(pos)) return(out)
  ord <- pos[order(ti$value[pos])]
  f <- ti$value[ord]
  if (anchor == "previous") {
    keep <- c(TRUE, diff(f) > range)
  } else {
    nxt <- findInterval(f + range, f) + 1L
    keep <- logical(length(f))
    i <- 1L
    while (i <= length(f)) {
      keep[i] <- TRUE
      i <- nxt[i]
    }
  }
  out[ord[keep]] <- TRUE
  out
}
