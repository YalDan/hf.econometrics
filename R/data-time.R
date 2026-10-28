#' Parse timestamps without guessing
#'
#' Converts raw timestamps to `POSIXct`. Three input types are accepted, and
#' none of them is interpreted by guesswork:
#' * `POSIXct` (or `POSIXlt`) is returned unchanged (as `POSIXct`); `unit`
#'   and `tz` are not applied.
#' * Character ISO-8601 strings `YYYY-MM-DD`, `YYYY-MM-DDThh:mm:ss` or
#'   `YYYY-MM-DD hh:mm:ss`, with optional fractional seconds of any length and
#'   an optional zone designator `Z`, `+hh`, `+hhmm` or `+hh:mm` (or `-`).
#'   Strings with a designator are converted to the instant they denote;
#'   strings without one are read as wall-clock time in `tz`.
#' * Numeric epochs (seconds, milliseconds or microseconds since
#'   1970-01-01 UTC), or character strings that contain only such numbers.
#'   They need an explicit `unit`.
#'
#' Missing values (`NA`, and `""` for strings) are returned as `NA`. Any other
#' value that does not match the accepted layouts raises an error that shows
#' the offending entries.
#'
#' @section Implementation choices:
#' * Numeric input without `unit` is an error, never a guess. The error
#'   message suggests the unit that the magnitude of the values points to.
#' * After scaling a numeric epoch, times before 1990 or after 2099 trigger a
#'   warning. Version 0.1.0 divided timestamps that were already parsed by
#'   1000 and silently moved the sample day to 1970-01-20; the warning
#'   catches this class of mistake.
#' * Calendar dates are validated (`2022-02-30` is an error), and so are the
#'   ranges `hh <= 23`, `mm <= 59` and `ss < 60`. Leap seconds are rejected,
#'   because `POSIXct` cannot represent them.
#' * The layouts are parsed with vectorised substring arithmetic, once per
#'   distinct date, minute, second and suffix. Strings without a zone
#'   designator and a non-UTC `tz` go through [strptime()], which applies the
#'   daylight-saving rules of `tz`. Wall-clock times that fall into a
#'   daylight-saving gap are an error; ambiguous ones (the repeated hour) are
#'   resolved as [strptime()] resolves them.
#'
#' @param x Timestamps: `POSIXct`, `POSIXlt`, character (or factor) or numeric.
#' @param unit Unit of numeric epochs: `"s"`, `"ms"` or `"us"`. Required for
#'   numeric input, ignored otherwise.
#' @param tz Time zone of the result, and the zone in which strings without a
#'   designator are read. Must be a name from [OlsonNames()].
#' @return A `POSIXct` vector with time zone `tz` (unchanged for `POSIXct`
#'   input).
#' @examples
#' parse_timestamps("2022-03-04T00:00:00.141Z")
#' parse_timestamps("2022-03-04 01:00:00.141+01:00")
#' parse_timestamps(1646352000141, unit = "ms")
#' @export
parse_timestamps <- function(x, unit = NULL, tz = "UTC") {
  check_tz(tz)
  if (!is.null(unit)) check_choice(unit, c("s", "ms", "us"), "unit")
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "POSIXlt")) return(as.POSIXct(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    padded <- !is.na(x) & grepl("^\\s|\\s$", x, perl = TRUE)
    if (any(padded)) x[padded] <- trimws(x[padded])
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    present <- !is.na(x)
    # ISO dates have "-" at position 5; only the other strings can be epochs.
    other <- present & substr(x, 5L, 5L) != "-"
    if (any(other)) {
      numeric_like <- grepl("^[+-]?[0-9]+(\\.[0-9]*)?$", x[other], perl = TRUE)
      if (any(numeric_like)) {
        if (!all(numeric_like) || sum(other) < sum(present)) {
          hf_abort("`x` mixes numeric epochs (e.g. \"%s\") with other strings (e.g. \"%s\").",
                   x[other][numeric_like][1L],
                   c(x[other][!numeric_like], x[present & !other])[1L])
        }
        return(parse_epoch(as.numeric(x), unit, tz))
      }
    }
    return(parse_iso8601(x, tz))
  }
  if (is.numeric(x) && !is.object(x)) return(parse_epoch(as.double(x), unit, tz))
  hf_abort("`x` must be POSIXct, character or numeric timestamps, not %s.", obj_type(x))
}

parse_epoch <- function(x, unit, tz) {
  if (is.null(unit)) {
    mag <- stats::median(abs(x[is.finite(x)]))
    hint <- if (!length(mag) || is.na(mag)) {
      ""
    } else if (mag >= 1e14) {
      " Their magnitude suggests microseconds (\"us\")."
    } else if (mag >= 1e11) {
      " Their magnitude suggests milliseconds (\"ms\")."
    } else {
      " Their magnitude suggests seconds (\"s\")."
    }
    hf_abort("Numeric timestamps need an explicit `unit` (\"s\", \"ms\" or \"us\").%s", hint)
  }
  if (any(is.infinite(x))) hf_abort("Numeric timestamps must be finite.")
  out <- .POSIXct(x / c(s = 1, ms = 1e3, us = 1e6)[[unit]], tz = tz)
  secs <- unclass(out)
  lo <- 631152000   # 1990-01-01 00:00:00 UTC
  hi <- 4102444800  # 2100-01-01 00:00:00 UTC
  if (any(secs < lo | secs >= hi, na.rm = TRUE)) {
    hf_warn(paste0("Timestamps scaled with unit \"%s\" fall outside 1990-2099 ",
                   "(range %s to %s); check `unit`."),
            unit, format(min(out, na.rm = TRUE)), format(max(out, na.rm = TRUE)))
  }
  out
}

# Canonical ISO-8601 layouts (see parse_timestamps()).
iso8601_pattern <- paste0(
  "^[0-9]{4}-[0-9]{2}-[0-9]{2}",
  "(?:[T ][0-9]{2}:[0-9]{2}:[0-9]{2}(?:\\.[0-9]+)?",
  "(?:Z|[+-][0-9]{2}(?::?[0-9]{2})?)?)?$"
)

parse_iso8601 <- function(x, tz) {
  present <- !is.na(x)
  bad <- present & !grepl(iso8601_pattern, x, perl = TRUE)
  if (any(bad)) {
    hf_abort("%d timestamp%s not in a supported ISO-8601 layout, e.g. %s.",
             sum(bad), if (sum(bad) == 1L) " is" else "s are",
             paste0("\"", utils::head(x[bad], 3L), "\"", collapse = ", "))
  }
  out <- rep(NA_real_, length(x))
  if (!any(present)) return(.POSIXct(out, tz = tz))
  s <- x[present]
  # Every component is parsed once per distinct substring (dates, minutes,
  # seconds and fraction-plus-zone suffixes repeat heavily in tick data) and
  # mapped back with match().
  p16 <- substr(s, 1L, 16L)                 # "YYYY-MM-DD" or "YYYY-MM-DDThh:mm"
  u16 <- unique(p16)
  i16 <- match(p16, u16)
  d10 <- substr(u16, 1L, 10L)
  ud <- unique(d10)
  dd <- as.Date(ud, format = "%Y-%m-%d")
  invalid <- is.na(dd) | format(dd) != ud
  if (any(invalid)) {
    hf_abort("Invalid calendar date%s: %s.", if (sum(invalid) == 1L) "" else "s",
             paste0("\"", utils::head(ud[invalid], 3L), "\"", collapse = ", "))
  }
  day <- as.numeric(dd)[match(d10, ud)]
  timed <- nchar(u16) > 10L                 # date-only strings mean midnight
  hh <- ifelse(timed, suppressWarnings(as.numeric(substr(u16, 12L, 13L))), 0)
  mi <- ifelse(timed, suppressWarnings(as.numeric(substr(u16, 15L, 16L))), 0)

  s2 <- substr(s, 18L, 19L)                 # whole seconds ("" for dates)
  u2 <- unique(s2)
  ss <- ifelse(nzchar(u2), suppressWarnings(as.numeric(u2)), 0)
  i2 <- match(s2, u2)

  rest <- substr(s, 20L, nchar(s))          # fraction and zone designator
  ur <- unique(rest)
  ir <- match(rest, ur)
  zpos <- regexpr("[Z+-]", ur)
  flen <- ifelse(zpos > 0L, zpos - 1L, nchar(ur))
  frac_str <- substr(ur, 1L, flen)
  frac <- ifelse(nzchar(frac_str), as.numeric(paste0("0", frac_str)), 0)
  zone <- ifelse(zpos > 0L, substr(ur, zpos, nchar(ur)), "")
  offset <- numeric(length(ur))
  num <- nzchar(zone) & zone != "Z"
  if (any(num)) {
    z <- gsub(":", "", zone[num], fixed = TRUE)
    oh <- as.numeric(substr(z, 2L, 3L))
    om <- ifelse(nchar(z) > 3L, as.numeric(substr(z, 4L, 5L)), 0)
    if (any(oh > 23 | om > 59)) {
      hf_abort("Invalid UTC offset in %s.",
               paste0("\"", utils::head(zone[num][oh > 23 | om > 59], 3L), "\"", collapse = ", "))
    }
    offset[num] <- ifelse(substr(z, 1L, 1L) == "-", -1, 1) * (3600 * oh + 60 * om)
  }
  bad_clock <- (hh > 23 | mi > 59)[i16] | (ss > 59)[i2]
  if (any(bad_clock)) {
    hf_abort("Clock time out of range in %s.",
             paste0("\"", utils::head(s[bad_clock], 3L), "\"", collapse = ", "))
  }

  zoned <- nzchar(zone)[ir]
  direct <- zoned | tz %in% c("UTC", "GMT", "Etc/UTC", "Etc/GMT")
  val <- 86400 * day[i16] + 3600 * hh[i16] + 60 * mi[i16] + ss[i2] + frac[ir] - offset[ir]
  if (any(!direct)) {
    # Wall-clock time in `tz`: strptime() applies its daylight-saving rules.
    wall <- paste(d10[i16], ifelse(timed[i16], substr(s, 12L, 19L), "00:00:00"))[!direct]
    uw <- unique(wall)
    local <- as.numeric(as.POSIXct(strptime(uw, "%Y-%m-%d %H:%M:%S", tz = tz)))
    # Times in a daylight-saving gap do not round-trip.
    local[format(.POSIXct(local, tz = tz), "%Y-%m-%d %H:%M:%S") != uw] <- NA_real_
    if (anyNA(local)) {
      hf_abort("Wall-clock time%s not existing in time zone \"%s\": %s.",
               if (sum(is.na(local)) == 1L) "" else "s", tz,
               paste0("\"", utils::head(uw[is.na(local)], 3L), "\"", collapse = ", "))
    }
    val[!direct] <- local[match(wall, uw)] + frac[ir][!direct]
  }
  out[present] <- val
  .POSIXct(out, tz = tz)
}

check_tz <- function(tz) {
  check_string(tz, "tz")
  if (!(tz %in% c("UTC", "GMT") || tz %in% OlsonNames())) {
    hf_abort("`tz` must be a time zone name from OlsonNames(), not \"%s\".", tz)
  }
  invisible(tz)
}

# Internal helpers shared by the data utilities ------------------------------

# Validates a vector of observation times: POSIXct or numeric (seconds on an
# arbitrary clock), without missing values. Returns the times as plain
# seconds together with what is needed to convert results back.
time_info <- function(x, arg = "time", sorted = TRUE) {
  if (inherits(x, "POSIXct")) {
    tz <- attr(x, "tzone")
    tz <- if (is.null(tz) || !length(tz) || is.na(tz[1L])) "" else tz[1L]
    value <- as.double(unclass(x))
    attributes(value) <- NULL
    posix <- TRUE
  } else if (is.numeric(x) && !is.object(x)) {
    value <- as.double(x)
    tz <- NA_character_
    posix <- FALSE
  } else {
    hf_abort("`%s` must be a POSIXct or numeric vector, not %s.", arg, obj_type(x))
  }
  if (!all(is.finite(value))) {
    hf_abort("`%s` must not contain missing or infinite values (%d found).",
             arg, sum(!is.finite(value)))
  }
  if (sorted && is.unsorted(value)) {
    hf_abort("`%s` must be sorted in non-decreasing order.", arg)
  }
  list(value = value, tz = tz, posix = posix)
}

# Converts seconds back to the type described by `info`.
as_time_like <- function(value, info) {
  if (info$posix) .POSIXct(value, tz = info$tz) else value
}

# Resolves a bound (start, end, origin) to seconds. NULL bounds are resolved
# by the caller.
resolve_time_point <- function(x, info, arg) {
  if (inherits(x, "POSIXct") || is.character(x)) {
    if (!info$posix) hf_abort("`%s` must be numeric when `time` is numeric.", arg)
    x <- parse_timestamps(x, tz = parsing_tz(info))
  } else if (!(is.numeric(x) && !is.object(x)) || info$posix) {
    hf_abort("`%s` must be %s, not %s.", arg,
             if (info$posix) "a POSIXct time or an ISO-8601 string" else "a number",
             obj_type(x))
  }
  v <- as.double(unclass(x))
  if (length(v) != 1L || !is.finite(v)) hf_abort("`%s` must be a single time point.", arg)
  v
}

# Time zone in which bound strings are read: that of `time`, or the session
# time zone for POSIXct in local time (tzone "").
parsing_tz <- function(info) {
  if (nzchar(info$tz)) return(info$tz)
  local <- Sys.timezone()
  if (is.na(local)) "UTC" else local
}

# Midnight at the start of the calendar day of `value` (seconds), in `tz`, and
# midnight at the start of the following day.
day_start_of <- function(value, tz) {
  d <- as.Date(.POSIXct(value, tz = tz), tz = tz)
  as.double(as.POSIXct(format(d), tz = tz))
}
day_end_of <- function(value, tz) {
  d <- as.Date(.POSIXct(value, tz = tz), tz = tz) + 1
  as.double(as.POSIXct(format(d), tz = tz))
}

# Start and end of the sampling frame. Defaults cover the calendar days
# spanned by POSIXct observations; numeric times need explicit bounds.
resolve_frame <- function(start, end, info) {
  if ((is.null(start) || is.null(end)) && !info$posix) {
    hf_abort("`start` and `end` are required when `time` is numeric.")
  }
  if ((is.null(start) || is.null(end)) && !length(info$value)) {
    hf_abort("`start` and `end` are required when `time` is empty.")
  }
  s <- if (is.null(start)) day_start_of(min(info$value), info$tz) else resolve_time_point(start, info, "start")
  e <- if (is.null(end)) day_end_of(max(info$value), info$tz) else resolve_time_point(end, info, "end")
  if (!(e > s)) hf_abort("`end` must be later than `start`.")
  c(start = s, end = e)
}

# Durations: a positive number of seconds, a difftime, or a string such as
# "1 hour", "30 mins", "5 sec" or "day".
duration_units <- c(
  s = 1, sec = 1, secs = 1, second = 1, seconds = 1,
  min = 60, mins = 60, minute = 60, minutes = 60,
  h = 3600, hour = 3600, hours = 3600,
  d = 86400, day = 86400, days = 86400
)

as_seconds <- function(x, arg) {
  if (inherits(x, "difftime")) x <- as.numeric(x, units = "secs")
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    m <- regmatches(x, regexec("^\\s*([0-9]*\\.?[0-9]+)?\\s*([A-Za-z]+)\\s*$", x))[[1L]]
    if (!length(m) || !(tolower(m[3L]) %in% names(duration_units))) {
      hf_abort("`%s` must be a number of seconds or a duration such as \"1 hour\", not \"%s\".",
               arg, x)
    }
    count <- if (nzchar(m[2L])) as.numeric(m[2L]) else 1
    x <- count * duration_units[[tolower(m[3L])]]
  }
  check_number(x, arg, lower = 0, open = c(TRUE, FALSE))
  if (!is.finite(x)) hf_abort("`%s` must be finite.", arg)
  as.double(x)
}
