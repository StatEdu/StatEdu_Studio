# Automatic missing value detection and review.

missing_common_numeric_codes <- function() {
  c(-999, -99, -9, 9, 99, 999, 9999, 777, 888)
}

missing_common_text_codes <- function() {
  unique(c(
    "",
    ".",
    "NA",
    "N/A",
    "NULL",
    "missing",
    "none",
    "not applicable",
    "unknown",
    "don't know",
    "dont know",
    "\ubaa8\ub984",
    "\uc798 \ubaa8\ub984",
    "\ubaa8\ub974\uaca0\uc74c",
    "\ubb34\uc751\ub2f5",
    "\ubbf8\uc751\ub2f5",
    "\uc751\ub2f5\uac70\ubd80",
    "\ud574\ub2f9\uc5c6\uc74c",
    "\ud574\ub2f9 \uc5c6\uc74c",
    "\ube44\ud574\ub2f9",
    "\uacb0\uce21",
    "\uc5c6\uc74c",
    "\uc54c \uc218 \uc5c6\uc74c",
    as.character(setdiff(missing_common_numeric_codes(), 9))
  ))
}

missing_normalize_text_code <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  gsub("[[:space:]]+", "", x)
}

missing_existing_na_summary <- function(data) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(list(total = 0L, variables = character(0)))
  }
  counts <- vapply(data, function(values) sum(is.na(values)), integer(1))
  variables <- names(counts)[counts > 0]
  list(total = sum(counts), variables = variables)
}

missing_display_value <- function(value, value_type = "text") {
  value <- as.character(value %||% "")
  if (identical(value_type, "text") && !nzchar(value)) {
    return("(blank)")
  }
  value
}

missing_value_storage_type <- function(values) {
  if (is.factor(values)) return("factor")
  if (is.numeric(values) || is.integer(values)) return("numeric")
  if (is.logical(values)) return("logical")
  "text"
}

missing_candidate_row <- function(variable, value, value_type, matches, total, reason, confidence, source_index, storage_type = value_type) {
  data.frame(
    variable = variable,
    value = as.character(value),
    value_type = value_type,
    display_value = missing_display_value(value, value_type),
    matches = as.integer(matches),
    total = as.integer(total),
    percent = if (total > 0) round(matches / total * 100, 1) else NA_real_,
    storage_type = storage_type,
    reason = reason,
    confidence = confidence,
    source_index = as.integer(source_index),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

missing_detect_numeric_candidates <- function(values, variable, source_index) {
  if (!(is.numeric(values) || is.integer(values)) || is.logical(values)) {
    return(NULL)
  }
  present <- values[!is.na(values)]
  if (length(present) == 0) {
    return(NULL)
  }
  unique_values <- sort(unique(as.numeric(present)))
  rows <- list()
  non_missing_total <- length(present)
  for (code in missing_common_numeric_codes()) {
    if (!code %in% unique_values) {
      next
    }
    without_code <- present[present != code]
    if (length(without_code) == 0) {
      next
    }
    reason <- "Common numeric missing code"
    confidence <- "medium"
    abs_code <- abs(code)
    without_max <- suppressWarnings(max(abs(without_code), na.rm = TRUE))
    without_range <- range(without_code, na.rm = TRUE)

    if (abs_code == 9) {
      if (code != max(present, na.rm = TRUE) || without_range[[1]] < 0 || without_range[[2]] > 7 || length(unique(without_code)) > 8) {
        next
      }
      reason <- "Possible scale missing code"
      confidence <- "low"
    } else if (abs_code >= 99 && is.finite(without_max) && without_max <= 20) {
      reason <- "Extreme value outside a small response scale"
      confidence <- "high"
    } else if (abs_code >= 99 && is.finite(without_max) && abs_code >= without_max * 2) {
      reason <- "Extreme sentinel-like value"
      confidence <- "medium"
    } else if (abs_code %in% c(777, 888, 999, 9999)) {
      reason <- "Repeated sentinel-style code"
      confidence <- "medium"
    }

    rows[[length(rows) + 1L]] <- missing_candidate_row(
      variable = variable,
      value = code,
      value_type = "numeric",
      matches = sum(present == code),
      total = non_missing_total,
      reason = reason,
      confidence = confidence,
      source_index = source_index
    )
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}

missing_detect_text_candidates <- function(values, variable, source_index) {
  if (is.numeric(values) || is.integer(values) || is.logical(values)) {
    return(NULL)
  }
  text <- trimws(as.character(as.vector(values)))
  text[is.na(values)] <- NA_character_
  present <- text[!is.na(text)]
  if (length(present) == 0) {
    return(NULL)
  }
  normalized <- missing_normalize_text_code(present)
  storage_type <- missing_value_storage_type(values)
  rows <- list()
  seen_codes <- character(0)
  for (code in missing_common_text_codes()) {
    code_text <- as.character(code)
    normalized_code <- missing_normalize_text_code(code_text)
    if (normalized_code %in% seen_codes) {
      next
    }
    seen_codes <- c(seen_codes, normalized_code)
    matched <- if (nzchar(code_text)) {
      normalized == normalized_code
    } else {
      !nzchar(present)
    }
    if (!any(matched)) {
      next
    }
    rows[[length(rows) + 1L]] <- missing_candidate_row(
      variable = variable,
      value = code_text,
      value_type = "text",
      matches = sum(matched),
      total = length(present),
      reason = if (nzchar(code_text)) "Common text missing code" else "Blank text value",
      confidence = if (nzchar(code_text)) "high" else "medium",
      source_index = source_index,
      storage_type = storage_type
    )
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}

detect_missing_value_candidates <- function(data, detect_numeric = TRUE, detect_text = TRUE) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(data.frame(check.names = FALSE))
  }
  rows <- list()
  for (index in seq_along(data)) {
    variable <- names(data)[[index]]
    values <- data[[index]]
    if (isTRUE(detect_numeric)) {
      rows[[length(rows) + 1L]] <- missing_detect_numeric_candidates(values, variable, index)
    }
    if (isTRUE(detect_text)) {
      rows[[length(rows) + 1L]] <- missing_detect_text_candidates(values, variable, index)
    }
  }
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(check.names = FALSE))
  }
  candidates <- do.call(rbind, rows)
  candidates <- candidates[order(candidates$source_index, candidates$value_type, candidates$value), , drop = FALSE]
  rownames(candidates) <- NULL
  candidates
}

missing_candidates_display <- function(candidates) {
  if (is.null(candidates) || !is.data.frame(candidates) || nrow(candidates) == 0) {
    return(data.frame(Message = "No likely missing-value codes were detected.", check.names = FALSE))
  }
  data.frame(
    Variable = candidates$variable,
    Value = candidates$display_value,
    Matches = candidates$matches,
    Percent = sprintf("%.1f%%", candidates$percent),
    Type = candidates$storage_type,
    Confidence = candidates$confidence,
    Reason = candidates$reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

apply_missing_value_rules <- function(values, rules) {
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0) {
    return(values)
  }
  output <- values
  for (index in seq_len(nrow(rules))) {
    value_type <- as.character(rules$value_type[[index]] %||% "text")
    rule_value <- as.character(rules$value[[index]] %||% "")
    if (identical(value_type, "numeric") && (is.numeric(output) || is.integer(output))) {
      numeric_value <- suppressWarnings(as.numeric(rule_value))
      if (!is.na(numeric_value)) {
        output[!is.na(output) & as.numeric(output) == numeric_value] <- NA
      }
      next
    }
    text <- trimws(as.character(as.vector(output)))
    matched <- if (nzchar(rule_value)) {
      !is.na(text) & missing_normalize_text_code(text) == missing_normalize_text_code(rule_value)
    } else {
      !is.na(text) & !nzchar(text)
    }
    output[matched] <- NA
  }
  output
}

data_editor_missing_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Auto Missing Values"),
      div("Detect likely missing-value codes, review them, and convert selected values to NA before analysis.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Auto missing value detection", "missing_values"),
      analysis_workspace_body(
        "missing_values",
        div(
          class = "analysis-action-row recode-same-action-row missing-values-option-row",
          checkboxInput("missing_detect_numeric", "Detect numeric codes", value = TRUE),
          checkboxInput("missing_detect_text", "Detect text codes", value = TRUE)
        ),
        uiOutput("missing_values_status"),
        div(class = "data-editor-result-output", DT::DTOutput("missing_value_candidates")),
        div(
          class = "analysis-action-row recode-same-action-row",
          actionButton("apply_missing_values", "Apply selected", class = "btn btn-primary")
        ),
        uiOutput("missing_values_message")
      )
    )
  )
}

register_missing_value_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  update_existing_variable_fn,
  mark_settings_dirty
) {
  last_message <- reactiveVal(NULL)

  candidates <- reactive({
    file <- current_data_file_fn()
    if (is.null(file)) {
      return(data.frame(check.names = FALSE))
    }
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    detect_missing_value_candidates(
      data,
      detect_numeric = isTRUE(input$missing_detect_numeric %||% TRUE),
      detect_text = isTRUE(input$missing_detect_text %||% TRUE)
    )
  })

  output$missing_values_status <- renderUI({
    file <- current_data_file_fn()
    if (is.null(file)) {
      return(div(class = "empty-message", div("Load a data file before detecting missing values.")))
    }
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    na_summary <- missing_existing_na_summary(data)
    table <- candidates()
    if (is.null(table) || nrow(table) == 0) {
      if (na_summary$total > 0) {
        visible_variables <- head(na_summary$variables, 8)
        variable_text <- paste(visible_variables, collapse = ", ")
        if (length(na_summary$variables) > length(visible_variables)) {
          variable_text <- sprintf("%s, ...", variable_text)
        }
        return(div(
          class = "empty-message",
          div(sprintf(
            "No coded missing values were detected. Existing NA values are already treated as missing: %s value(s) across %s variable(s).",
            na_summary$total,
            length(na_summary$variables)
          )),
          div(class = "small-muted", variable_text)
        ))
      }
      return(div(class = "empty-message", div("No coded missing values or existing NA values were detected in the current data.")))
    }
    status <- sprintf("%s candidate code(s) detected across %s variable(s). Select rows to convert them to NA.", nrow(table), length(unique(table$variable)))
    if (na_summary$total > 0) {
      status <- sprintf("%s Existing NA values are already missing: %s value(s).", status, na_summary$total)
    }
    div(class = "recode-same-status", status)
  })

  output$missing_value_candidates <- DT::renderDT({
    table <- missing_candidates_display(candidates())
    if ("Message" %in% names(table)) {
      return(DT::datatable(table, rownames = FALSE, options = list(dom = "t", ordering = FALSE)))
    }
    DT::datatable(
      table,
      rownames = FALSE,
      selection = list(mode = "multiple", target = "row"),
      class = "compact stripe hover missing-values-table",
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        scrollX = FALSE,
        autoWidth = FALSE,
        searching = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(width = "180px", targets = 0),
          list(width = "110px", targets = 1),
          list(width = "70px", targets = 2, className = "dt-center"),
          list(width = "80px", targets = 3, className = "dt-center"),
          list(width = "86px", targets = 4),
          list(width = "96px", targets = 5),
          list(width = "260px", targets = 6)
        )
      )
    )
  })

  output$missing_values_message <- renderUI({
    message <- last_message()
    if (is.null(message)) {
      return(NULL)
    }
    div(class = "recode-same-status", message)
  })

  observeEvent(input$apply_missing_values, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    table <- candidates()
    selected <- input$missing_value_candidates_rows_selected %||% integer(0)
    selected <- suppressWarnings(as.integer(selected))
    selected <- selected[is.finite(selected) & selected >= 1 & selected <= nrow(table)]
    if (is.null(data)) {
      showNotification("Load a data file before applying missing-value rules.", type = "warning", duration = 5)
      return()
    }
    if (is.null(table) || nrow(table) == 0) {
      showNotification("No missing-value candidates are available.", type = "warning", duration = 5)
      return()
    }
    if (length(selected) == 0) {
      showNotification("Select at least one candidate row to apply.", type = "warning", duration = 5)
      return()
    }

    rules <- table[selected, , drop = FALSE]
    changed_variables <- character(0)
    changed_values <- 0L
    for (variable in unique(as.character(rules$variable))) {
      variable_rules <- rules[rules$variable == variable, , drop = FALSE]
      if (!variable %in% names(data)) {
        next
      }
      before <- data[[variable]]
      after <- apply_missing_value_rules(before, variable_rules)
      changed <- sum(!is.na(before) & is.na(after))
      if (changed == 0) {
        next
      }
      ok <- update_existing_variable_fn(variable, after)
      if (isTRUE(ok)) {
        changed_variables <- c(changed_variables, variable)
        changed_values <- changed_values + changed
      }
    }

    if (length(changed_variables) == 0) {
      last_message("No values were changed.")
      return()
    }
    if (is.function(mark_settings_dirty)) {
      mark_settings_dirty()
    }
    last_message(sprintf("Converted %s value(s) to NA across %s variable(s): %s", changed_values, length(changed_variables), paste(changed_variables, collapse = ", ")))
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
