# Auto-extracted shared functions for easyflow_statistics.

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
  if (length(p) == 0 || is.null(p[[1]]) || is.na(p[[1]])) {
    return(NA_character_)
  }
  if (is.character(p[[1]])) {
    text <- trimws(p[[1]])
    if (!nzchar(text)) return("")
    if (startsWith(text, "<")) return(text)
    value <- suppressWarnings(as.numeric(sub("^\\.", "0.", text)))
    if (is.na(value)) return(text)
  } else {
    value <- suppressWarnings(as.numeric(p[[1]]))
  }
  if (is.na(value)) return(NA_character_)
  if (value < .001) return("<.001")
  sub("^0\\.", ".", sprintf("%.3f", value))
}

format_decimal3 <- function(x) {
  if (is.na(x)) return("")
  text <- sprintf("%.3f", x)
  text <- sub("^-0\\.", "-.", text)
  sub("^0\\.", ".", text)
}

format_effect_size <- function(x) {
  format_decimal3(x)
}

register_dual_transfer_drop_observer <- function(
  input,
  session,
  available_id,
  selected_id,
  selected_values,
  all_values_fn,
  active_list = NULL,
  mark_settings_dirty = NULL,
  validate_next = NULL,
  after_change = NULL
) {
  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c(available_id, selected_id)
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) {
      return()
    }

    all_values <- as.character(all_values_fn() %||% character(0))
    current <- intersect(as.character(selected_values() %||% character(0)), all_values)
    if (identical(target, selected_id)) {
      chosen <- intersect(values, all_values)
      next_values <- c(current, setdiff(chosen, current))
    } else {
      chosen <- intersect(values, current)
      next_values <- setdiff(current, chosen)
    }
    if (length(chosen) == 0 || identical(next_values, current)) {
      return()
    }
    if (!is.null(validate_next) && !isTRUE(validate_next(next_values, target, chosen))) {
      return()
    }

    selected_values(next_values)
    if (!is.null(active_list)) active_list(target)
    if (!is.null(after_change)) after_change(target, chosen, next_values)
    if (!is.null(mark_settings_dirty)) mark_settings_dirty()
    if (!is.null(session)) {
      session$sendCustomMessage(
        "easyflow-clear-transfer-selection",
        list(inputIds = ids)
      )
    }
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

stat_chisq_label <- function(with_p = FALSE) {
  if (isTRUE(with_p)) "x\u00B2(p)" else "x\u00B2"
}

default_seed <- function() {
  as.integer(format(Sys.Date(), "%Y%m%d"))
}

has_request_nonce <- function(request) {
  !is.null(request) && !is.null(request$nonce)
}

setup_option_checked <- function(value, default = FALSE) {
  if (is.null(value)) {
    return(isTRUE(default))
  }
  isTRUE(value)
}
