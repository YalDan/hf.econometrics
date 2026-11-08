#' Split observations into fixed test windows
#'
#' Assigns sorted observation times to consecutive windows
#' \eqn{[b_w, b_{w+1})} (or \eqn{(b_w, b_{w+1}]}) and returns, for each
#' window, the range of rows it contains. Windows are either of fixed length
#' `width`, counted from `origin`, or given explicitly by `breaks`.
#'
#' @section Paper mapping:
#' * Lee and Mykland (2012, Section 6.1) test each hour separately, "for
#'   example 11-12, include information about trades after 11am (inclusive)
#'   and before 12pm (exclusive)": `width = "1 hour"`, `closed = "left"`.
#'   Their opening window 9:30-10:00 is shorter; pass `breaks` at 9:30,
#'   10:00, 11:00, ..., 16:00 to reproduce it.
#' * Saef et al. (2021, Sections 3.1 and 3.4) test every day separately:
#'   `width = "1 day"`.
#'
#' @section Implementation choices:
#' * By default windows are closed on the left and open on the right, as in
#'   Lee and Mykland (2012). Series labelled by the end of their sampling
#'   interval, such as the rows of [sample_previous_tick()], summarise the
#'   interval \eqn{(g_{j-1}, g_j]}; split them with `closed = "right"`, so
#'   that the last interval of a day (labelled 24:00:00) stays in that day.
#' * Fixed-length windows are counted in seconds from `origin`, which by
#'   default is the midnight before the first observation in the time zone
#'   of `time`. Across a daylight-saving change, use `breaks` for windows on
#'   the local clock.
#' * With `breaks`, observations outside the first and last break belong to
#'   no window.
#'
#' @param time Observation times, `POSIXct` or numeric, sorted.
#' @param width Window length: seconds, a `difftime`, or a string such as
#'   `"1 hour"` or `"1 day"`. Ignored when `breaks` is given.
#' @param origin Start of window 1 for fixed-length windows: `POSIXct` or an
#'   ISO-8601 string (a number if `time` is numeric, where it is required).
#' @param breaks Optional strictly increasing window boundaries (`POSIXct`,
#'   ISO-8601 strings, or numbers if `time` is numeric).
#' @param closed Which end of a window is inclusive: `"left"` for
#'   \eqn{[b_w, b_{w+1})} or `"right"` for \eqn{(b_w, b_{w+1}]}.
#' @param drop_empty Drop windows without observations?
#' @return A data frame with one row per window: `window` (1 for the window
#'   that starts at `origin` or at `breaks[1]`; windows before `origin` get
#'   numbers below 1), `start`, `end`, `from` and `to` (first and last row of
#'   `time` in the window, `NA` if empty) and `n` (number of observations).
#' @references
#' Lee, S. S. and Mykland, P. A. (2012). Jumps in equilibrium prices and
#' market microstructure noise. *Journal of Econometrics* 168(2), 396-406.
#'
#' Saef, D., Nagy, O., Sizov, S. and Hardle, W. K. (2021). Understanding
#' jumps in high frequency digital asset markets. arXiv:2110.09429.
#' @examples
#' t0 <- as.POSIXct("2022-03-04", tz = "UTC")
#' t <- t0 + c(10, 3599, 3600, 7300)
#' w <- split_windows(t, "1 hour")
#' w
#' lapply(seq_len(nrow(w)), function(j) t[w$from[j]:w$to[j]])
#' split_windows(t, "1 hour", closed = "right")
#' @export
split_windows <- function(time, width = "1 hour", origin = NULL, breaks = NULL,
                          closed = c("left", "right"), drop_empty = TRUE) {
  ti <- time_info(time, "time")
  closed <- check_choice(closed[1L], c("left", "right"), "closed")
  check_flag(drop_empty, "drop_empty")
  tv <- ti$value
  right <- closed == "right"
  if (!is.null(breaks)) {
    b <- resolve_breaks(breaks, ti)
    first_window <- 1L
  } else {
    width <- as_seconds(width, "width")
    o <- if (is.null(origin)) {
      if (!ti$posix) hf_abort("`origin` is required when `time` is numeric.")
      if (!length(tv)) hf_abort("`origin` is required when `time` is empty.")
      day_start_of(min(tv), ti$tz)
    } else {
      resolve_time_point(origin, ti, "origin")
    }
    if (!length(tv)) {
      b <- c(o, o + width)
      kmin <- 0
    } else {
      # Window k covers [o + k w, o + (k + 1) w), or (o + k w, o + (k + 1) w].
      k_of <- function(x) if (right) ceiling((x - o) / width) - 1 else floor((x - o) / width)
      kmin <- k_of(min(tv))
      b <- o + (kmin:(k_of(max(tv)) + 1)) * width
      # Guard against floating-point rounding at the ends.
      if (if (right) min(tv) <= b[1L] else min(tv) < b[1L]) {
        b <- c(b[1L] - width, b)
        kmin <- kmin - 1
      }
      last <- b[length(b)]
      if (if (right) max(tv) > last else max(tv) >= last) b <- c(b, last + width)
    }
    first_window <- as.integer(kmin) + 1L
  }
  # Number of observations before each break (strictly before for "left",
  # at or before for "right").
  before <- findInterval(b, tv, left.open = !right)
  from <- before[-length(b)] + 1L
  to <- before[-1L]
  n <- pmax(0L, to - from + 1L)
  out <- data.frame(
    window = first_window + seq_len(length(b) - 1L) - 1L,
    start = as_time_like(b[-length(b)], ti),
    end = as_time_like(b[-1L], ti),
    from = ifelse(n > 0L, from, NA_integer_),
    to = ifelse(n > 0L, to, NA_integer_),
    n = as.integer(n)
  )
  if (drop_empty) out <- out[out$n > 0L, , drop = FALSE]
  rownames(out) <- NULL
  out
}

resolve_breaks <- function(breaks, info) {
  if (inherits(breaks, "POSIXct") || is.character(breaks)) {
    if (!info$posix) hf_abort("`breaks` must be numeric when `time` is numeric.")
    breaks <- parse_timestamps(breaks, tz = parsing_tz(info))
  } else if (!(is.numeric(breaks) && !is.object(breaks)) || info$posix) {
    hf_abort("`breaks` must be %s, not %s.",
             if (info$posix) "POSIXct times or ISO-8601 strings" else "numeric", obj_type(breaks))
  }
  b <- as.double(unclass(breaks))
  if (length(b) < 2L || !all(is.finite(b)) || any(diff(b) <= 0)) {
    hf_abort("`breaks` must contain at least two strictly increasing time points.")
  }
  b
}
