#' High-frequency price series
#'
#' `HFSeries` is the typed container that methods accept as an alternative to
#' a bare numeric vector of log prices. Construct it with [hf_series()].
#'
#' @param time Observation times, a `POSIXct` vector in non-decreasing order.
#' @param log_price Log prices, a finite double vector of the same length.
#' @param symbol,venue Optional labels (single strings) carried into results.
#' @return An `HFSeries` object.
#' @seealso [hf_series()], [log_prices()], [obs_times()]
#' @export
HFSeries <- new_class(
  "HFSeries",
  properties = list(
    time = new_property(class_POSIXct, default = quote(.POSIXct(double(), tz = "UTC"))),
    log_price = class_double,
    symbol = new_property(class_character, default = NA_character_),
    venue = new_property(class_character, default = NA_character_)
  ),
  validator = function(self) {
    collect_msgs(
      if (length(self@time) != length(self@log_price)) {
        sprintf("@time (length %d) and @log_price (length %d) must have the same length.",
                length(self@time), length(self@log_price))
      },
      if (length(self@log_price) < 2L) "@log_price needs at least 2 observations.",
      if (anyNA(self@time)) "@time must not contain NA.",
      if (!all(is.finite(self@log_price))) "@log_price must be finite.",
      if (is.unsorted(self@time)) "@time must be sorted in non-decreasing order.",
      if (length(self@symbol) != 1L) "@symbol must be a single string.",
      if (length(self@venue) != 1L) "@venue must be a single string."
    )
  }
)

#' Create a high-frequency price series
#'
#' @param time Observation times: a `POSIXct` vector, or anything
#'   `as.POSIXct()` accepts, with `tz` applied.
#' @param price Prices, or log prices if `log = TRUE`.
#' @param log Is `price` already in logs?
#' @param symbol,venue Optional single-string labels.
#' @param tz Time zone used when `time` is converted to `POSIXct`.
#' @return An [HFSeries] object.
#' @examples
#' t0 <- as.POSIXct("2022-03-04", tz = "UTC")
#' s <- hf_series(t0 + 0:9, 100 * exp(cumsum(rnorm(10, 0, 1e-4))))
#' s
#' @export
hf_series <- function(time, price, log = FALSE, symbol = NA_character_,
                      venue = NA_character_, tz = "UTC") {
  check_flag(log, "log")
  price <- check_numeric(price, "price", min_length = 2L)
  if (!log && any(price <= 0)) hf_abort("`price` must be strictly positive.")
  if (!inherits(time, "POSIXct")) time <- as.POSIXct(time, tz = tz)
  HFSeries(
    time = time,
    log_price = if (log) price else base::log(price),
    symbol = as.character(symbol),
    venue = as.character(venue)
  )
}

#' Extract log prices or observation times
#'
#' `log_prices()` is the single entry point methods use to read their input.
#' It accepts either a numeric vector of log prices, assumed equally spaced in
#' the clock the method works in, or an [HFSeries]. Any other type fails S7
#' dispatch with an informative error.
#'
#' `obs_times()` returns the observation times of an `HFSeries`. For a numeric
#' vector it returns the observation index `0, 1, ..., n`.
#'
#' @param x A numeric vector of log prices or an [HFSeries].
#' @param ... Reserved for methods; currently unused.
#' @return A double vector.
#' @examples
#' log_prices(log(c(100, 101, 100.5)))
#' @export
log_prices <- new_generic("log_prices", "x")

method(log_prices, class_numeric) <- function(x) {
  check_numeric(x, "x", min_length = 2L)
}

method(log_prices, HFSeries) <- function(x) x@log_price

#' @rdname log_prices
#' @export
obs_times <- new_generic("obs_times", "x")

method(obs_times, class_numeric) <- function(x) seq_along(x) - 1

method(obs_times, HFSeries) <- function(x) x@time

method(print, HFSeries) <- function(x, ...) {
  n <- length(x@log_price)
  cat(sprintf("<HFSeries> %d observations", n))
  labels <- c(x@symbol, x@venue)
  labels <- labels[!is.na(labels)]
  if (length(labels)) cat(sprintf(" [%s]", paste(labels, collapse = " @ ")))
  cat(sprintf("\n  %s -> %s\n", format(x@time[1L]), format(x@time[n])))
  invisible(x)
}
