survival_coerce_duration <- function(values) {
  text <- trimws(as.character(values))
  missing <- is.na(values) | !nzchar(text)
  unsupported_class <- inherits(values, c("Date", "POSIXct", "POSIXlt")) || is.logical(values) || is.list(values)
  numeric_values <- if (unsupported_class) {
    rep(NA_real_, length(values))
  } else if (is.factor(values) || is.character(values)) {
    suppressWarnings(as.numeric(text))
  } else {
    suppressWarnings(as.numeric(values))
  }
  invalid_encoding <- !missing & (unsupported_class | is.na(numeric_values))
  list(values = numeric_values, missing = missing, invalid_encoding = invalid_encoding)
}

