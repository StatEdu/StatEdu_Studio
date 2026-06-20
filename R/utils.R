# Auto-extracted shared functions for StatEdu Studio.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

easyflow_timing_enabled <- function() {
  !identical(tolower(Sys.getenv("EASYFLOW_TIMING", "1")), "0")
}

easyflow_log_timing <- function(label, start, detail = "") {
  if (!isTRUE(easyflow_timing_enabled())) {
    return(invisible(FALSE))
  }
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  suffix <- if (nzchar(as.character(detail %||% ""))) paste0(" ", detail) else ""
  message(sprintf("[StatEdu timing] %s: %.3fs%s", label, elapsed, suffix))
  invisible(TRUE)
}

easyflow_time_expr <- function(label, expr, detail = "") {
  start <- Sys.time()
  on.exit(easyflow_log_timing(label, start, detail), add = TRUE)
  force(expr)
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
    less_than <- startsWith(text, "<")
    value_text <- sub("^<", "", text)
    value <- suppressWarnings(as.numeric(sub("^\\.", "0.", value_text)))
    if (is.na(value)) return(text)
    if (isTRUE(less_than) && value <= .001) {
      return("<.001")
    }
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

format_decimal2 <- function(x) {
  if (is.na(x)) return("")
  text <- sprintf("%.2f", x)
  text <- sub("^-0\\.", "-.", text)
  sub("^0\\.", ".", text)
}

format_effect_size <- function(x) {
  format_decimal3(x)
}

analysis_bind_rows <- function(rows) {
  rows <- Filter(function(row) is.data.frame(row) && nrow(row) > 0, rows %||% list())
  if (length(rows) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    missing_columns <- setdiff(columns, names(row))
    for (column in missing_columns) {
      row[[column]] <- ""
    }
    row <- row[, columns, drop = FALSE]
    rownames(row) <- NULL
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

analysis_message_table <- function(messages, column = "Warning") {
  messages <- unique(as.character(messages %||% character(0)))
  messages <- messages[!is.na(messages) & nzchar(messages)]
  if (length(messages) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  stats::setNames(data.frame(messages, stringsAsFactors = FALSE, check.names = FALSE), column)
}

analysis_warning_table <- function(messages) {
  analysis_message_table(messages, "Warning")
}

analysis_guard_row <- function(type, target, predictors = "", n = NA_integer_, message = "") {
  data.frame(
    Type = as.character(type %||% ""),
    Target = as.character(target %||% ""),
    `Independent variables` = as.character(predictors %||% ""),
    N = if (is.na(n)) "" else as.character(n),
    Message = as.character(message %||% ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

analysis_has_rows <- function(table) {
  is.data.frame(table) && nrow(table) > 0
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

register_dual_transfer_doubleclick_observers <- function(
  input,
  available_id,
  selected_id,
  selected_values,
  all_values_fn,
  active_list = NULL,
  mark_settings_dirty = NULL,
  validate_next = NULL
) {
  observeEvent(input[[paste0(available_id, "_doubleclick")]], {
    event <- input[[paste0(available_id, "_doubleclick")]]
    value <- as.character(event$value %||% "")
    all_values <- as.character(all_values_fn() %||% character(0))
    chosen <- intersect(value, all_values)
    if (length(chosen) == 0) {
      return()
    }
    current <- intersect(as.character(selected_values() %||% character(0)), all_values)
    next_values <- c(current, setdiff(chosen, current))
    if (identical(next_values, current)) {
      return()
    }
    if (!is.null(validate_next) && !isTRUE(validate_next(next_values, selected_id, chosen))) {
      return()
    }
    selected_values(next_values)
    if (!is.null(active_list)) active_list(selected_id)
    if (!is.null(mark_settings_dirty)) mark_settings_dirty()
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
