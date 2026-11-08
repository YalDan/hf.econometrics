# Internal argument checks ---------------------------------------------------
#
# Every exported function validates its inputs with these helpers. Each helper
# returns its (possibly coerced) input invisibly when the check passes. When a
# check fails, it either raises an error of class `hf_input_error`
# (`abort = TRUE`, the default) or returns the message (`abort = FALSE`), the
# form S7 property validators expect.

hf_abort <- function(message, ..., class = character()) {
  msg <- if (...length()) sprintf(message, ...) else message
  cnd <- structure(
    class = c(class, "hf_input_error", "error", "condition"),
    list(message = msg, call = NULL)
  )
  stop(cnd)
}

hf_warn <- function(message, ...) {
  msg <- if (...length()) sprintf(message, ...) else message
  warning(structure(
    class = c("hf_warning", "warning", "condition"),
    list(message = msg, call = NULL)
  ))
}

obj_type <- function(x) {
  if (is.null(x)) return("NULL")
  if (S7::S7_inherits(x)) return(sprintf("a <%s> object", S7::S7_class(x)@name))
  if (is.object(x)) return(sprintf("a <%s> object", class(x)[1L]))
  if (length(x) == 1L) {
    return(sprintf("%s %s", typeof(x), format(x)))
  }
  sprintf("a %s vector of length %d", typeof(x), length(x))
}

finish_check <- function(msg, x, abort) {
  if (!abort) return(msg)
  if (!is.null(msg)) hf_abort(msg)
  invisible(x)
}

check_numeric <- function(x, arg, min_length = 1L, finite = TRUE,
                          abort = TRUE) {
  msg <- NULL
  if (!is.numeric(x) || is.object(x)) {
    msg <- sprintf("`%s` must be a numeric vector, not %s.", arg, obj_type(x))
  } else if (length(x) < min_length) {
    msg <- sprintf("`%s` must have at least %d element%s, not %d.",
                   arg, min_length, if (min_length == 1L) "" else "s",
                   length(x))
  } else if (finite && !all(is.finite(x))) {
    msg <- sprintf("`%s` must not contain NA, NaN or infinite values (%d found).",
                   arg, sum(!is.finite(x)))
  }
  finish_check(msg, if (is.null(msg)) as.double(x) else x, abort)
}

check_number <- function(x, arg, lower = -Inf, upper = Inf,
                         open = c(FALSE, FALSE), integerish = FALSE,
                         allow_null = FALSE, abort = TRUE) {
  msg <- NULL
  if (is.null(x)) {
    if (!allow_null) msg <- sprintf("`%s` must be a number, not NULL.", arg)
  } else if (!is.numeric(x) || is.object(x) || length(x) != 1L || is.na(x)) {
    msg <- sprintf("`%s` must be a single non-missing number, not %s.",
                   arg, obj_type(x))
  } else if (integerish && (!is.finite(x) || abs(x - round(x)) > 1e-8)) {
    msg <- sprintf("`%s` must be a whole number, not %s.", arg, format(x))
  } else if (x < lower || x > upper || (open[1] && x == lower) ||
             (open[2] && x == upper)) {
    msg <- sprintf("`%s` must lie in %s%s, %s%s, not %s.", arg,
                   if (open[1]) "(" else "[", format(lower), format(upper),
                   if (open[2]) ")" else "]", format(x))
  }
  finish_check(msg, x, abort)
}

check_count <- function(x, arg, min = 1L, max = Inf, allow_null = FALSE,
                        abort = TRUE) {
  msg <- check_number(x, arg, lower = min, upper = max, integerish = TRUE,
                      allow_null = allow_null, abort = FALSE)
  finish_check(msg, if (is.null(x)) x else as.integer(round(x)), abort)
}

check_probability <- function(x, arg, abort = TRUE) {
  check_number(x, arg, lower = 0, upper = 1, open = c(TRUE, TRUE),
               abort = abort)
}

check_flag <- function(x, arg, abort = TRUE) {
  msg <- NULL
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    msg <- sprintf("`%s` must be TRUE or FALSE, not %s.", arg, obj_type(x))
  }
  finish_check(msg, x, abort)
}

check_string <- function(x, arg, allow_na = FALSE, abort = TRUE) {
  msg <- NULL
  if (!is.character(x) || length(x) != 1L || (!allow_na && is.na(x))) {
    msg <- sprintf("`%s` must be a single string, not %s.", arg, obj_type(x))
  }
  finish_check(msg, x, abort)
}

check_choice <- function(x, choices, arg, abort = TRUE) {
  msg <- NULL
  if (!is.character(x) || length(x) != 1L || is.na(x) || !(x %in% choices)) {
    msg <- sprintf("`%s` must be one of %s, not %s.", arg,
                   paste0("\"", choices, "\"", collapse = ", "), obj_type(x))
  }
  finish_check(msg, x, abort)
}

check_class <- function(x, class, arg, abort = TRUE) {
  msg <- NULL
  if (!S7::S7_inherits(x, class)) {
    msg <- sprintf("`%s` must be a <%s> object, not %s.", arg, class@name,
                   obj_type(x))
  }
  finish_check(msg, x, abort)
}

# Collapses validator messages (NULLs are dropped) for S7 validators.
collect_msgs <- function(...) {
  msgs <- unlist(list(...), use.names = FALSE)
  if (length(msgs)) msgs else NULL
}
