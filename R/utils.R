# Auto-extracted shared functions for EasyFlow Statistics.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

named_value <- function(x, name, default = "") {
  name <- as.character(name %||% "")
  name <- if (length(name) == 0 || is.na(name[[1]])) "" else name[[1]]
  if (is.null(x) || !nzchar(name) || is.null(names(x))) {
    return(default)
  }
  index <- match(name, names(x))
  if (is.na(index) || index < 1 || index > length(x)) {
    return(default)
  }
  value <- x[[index]]
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return(default)
  }
  as.character(value[[1]])
}

named_override_log_text <- function(values = character(0)) {
  values <- values %||% character(0)
  paste(sprintf("%s=%s", names(values), unname(values)), collapse = ", ")
}

format_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < .001) return("< .001")
  sub("^0\\.", ".", sprintf("%.3f", p))
}

format_decimal3 <- function(x) {
  if (is.na(x)) return("")
  text <- sprintf("%.3f", x)
  text <- sub("^-0\\.", "-.", text)
  sub("^0\\.", ".", text)
}

default_seed <- function() {
  as.integer(format(Sys.Date(), "%Y%m%d"))
}

has_request_nonce <- function(request) {
  !is.null(request) && !is.null(request$nonce)
}
