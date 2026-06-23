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

missing_rule_columns <- function() {
  c("variable", "value", "value_type", "display_value", "matches", "total", "percent", "storage_type", "reason", "confidence", "source_index")
}

missing_empty_rules <- function() {
  data.frame(
    variable = character(0),
    value = character(0),
    value_type = character(0),
    display_value = character(0),
    matches = integer(0),
    total = integer(0),
    percent = numeric(0),
    storage_type = character(0),
    reason = character(0),
    confidence = character(0),
    source_index = integer(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

normalize_missing_rules <- function(rules, variables = NULL) {
  columns <- missing_rule_columns()
  if (is.null(rules) || !is.data.frame(rules) || nrow(rules) == 0) {
    return(missing_empty_rules())
  }
  rules <- as.data.frame(rules, stringsAsFactors = FALSE, check.names = FALSE)
  missing_columns <- setdiff(columns, names(rules))
  for (column in missing_columns) {
    rules[[column]] <- NA
  }
  rules <- rules[, columns, drop = FALSE]
  rules$variable <- as.character(rules$variable %||% "")
  rules$value <- as.character(rules$value %||% "")
  rules$value_type <- as.character(rules$value_type %||% "text")
  rules$display_value <- ifelse(
    !is.na(rules$display_value) & nzchar(as.character(rules$display_value)),
    as.character(rules$display_value),
    mapply(missing_display_value, rules$value, rules$value_type, USE.NAMES = FALSE)
  )
  rules$storage_type <- as.character(rules$storage_type %||% rules$value_type)
  rules$reason <- as.character(rules$reason %||% "")
  rules$confidence <- as.character(rules$confidence %||% "")
  rules$matches <- suppressWarnings(as.integer(rules$matches))
  rules$total <- suppressWarnings(as.integer(rules$total))
  rules$percent <- suppressWarnings(as.numeric(rules$percent))
  rules$source_index <- suppressWarnings(as.integer(rules$source_index))
  rules <- rules[nzchar(rules$variable), , drop = FALSE]
  if (!is.null(variables)) {
    rules <- rules[rules$variable %in% as.character(variables), , drop = FALSE]
  }
  if (nrow(rules) == 0) {
    return(missing_empty_rules())
  }
  key <- paste(rules$variable, rules$value_type, rules$value, sep = "\r")
  rules <- rules[!duplicated(key, fromLast = TRUE), , drop = FALSE]
  rownames(rules) <- NULL
  rules
}

merge_missing_user_rules <- function(existing, added, variables = NULL) {
  existing <- normalize_missing_rules(existing, variables)
  added <- normalize_missing_rules(added, variables)
  normalize_missing_rules(rbind(existing, added), variables)
}

apply_user_missing_rules_to_data <- function(data, rules) {
  if (!is.data.frame(data)) {
    return(data)
  }
  rules <- normalize_missing_rules(rules, names(data))
  if (nrow(rules) == 0) {
    return(data)
  }
  output <- data
  for (variable in unique(as.character(rules$variable))) {
    if (!variable %in% names(output)) {
      next
    }
    variable_rules <- rules[rules$variable == variable, , drop = FALSE]
    output[[variable]] <- apply_missing_value_rules(output[[variable]], variable_rules)
  }
  output
}

rename_missing_user_rules <- function(rules, old_name, new_name) {
  rules <- normalize_missing_rules(rules)
  if (nrow(rules) == 0) {
    return(rules)
  }
  rules$variable[as.character(rules$variable) == old_name] <- new_name
  normalize_missing_rules(rules)
}

missing_manual_codes_from_text <- function(text) {
  lines <- trimws(unlist(strsplit(as.character(text %||% ""), "\\r?\\n")))
  lines <- lines[nzchar(lines)]
  lines[lines %in% c("(blank)", "<blank>")] <- ""
  unique(lines)
}

missing_manual_rules <- function(data, variables, codes) {
  if (is.null(data) || !is.data.frame(data) || length(variables) == 0 || length(codes) == 0) {
    return(data.frame(check.names = FALSE))
  }
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  codes <- as.character(codes %||% character(0))
  rows <- list()
  for (variable in variables) {
    values <- data[[variable]]
    present <- values[!is.na(values)]
    total <- length(present)
    storage_type <- missing_value_storage_type(values)
    for (code in codes) {
      numeric_code <- suppressWarnings(as.numeric(code))
      numeric_rule <- (is.numeric(values) || is.integer(values)) && !is.logical(values) && !is.na(numeric_code)
      if (numeric_rule) {
        matches <- sum(!is.na(values) & as.numeric(values) == numeric_code)
        value_type <- "numeric"
        value <- numeric_code
      } else {
        text <- trimws(as.character(as.vector(values)))
        matched <- if (nzchar(code)) {
          !is.na(text) & missing_normalize_text_code(text) == missing_normalize_text_code(code)
        } else {
          !is.na(text) & !nzchar(text)
        }
        matches <- sum(matched)
        value_type <- "text"
        value <- code
      }
      rows[[length(rows) + 1L]] <- missing_candidate_row(
        variable = variable,
        value = value,
        value_type = value_type,
        matches = matches,
        total = total,
        reason = "Manual missing code",
        confidence = "manual",
        source_index = match(variable, names(data)),
        storage_type = storage_type
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(check.names = FALSE))
  }
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output
}

missing_values_setup_panel <- function(file, data, variable_info = NULL, labels = character(0), selected_variables = character(0), input = NULL) {
  if (is.null(file) || is.null(data)) {
    return(setup_empty_message("Load a data file in the Data tab before converting missing values."))
  }
  variables <- names(data)
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), variables)
  available <- setdiff(variables, selected_variables)
  selected_available <- selected_order_items(isolate(input$missing_values_available) %||% character(0), available)
  selected_selected <- selected_order_items(isolate(input$missing_values_selected) %||% character(0), selected_variables)

  div(
    class = "recode-same-setup-grid missing-values-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input(
        "missing_values_available",
        items = analysis_variable_items(available, variable_info, labels),
        selected = selected_available,
        size = 18
      )
    ),
    div(
      class = "analysis-transfer-controls recode-same-transfer-controls",
      actionButton(
        "missing_values_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (length(available) == 0 && length(selected_variables) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel missing-values-selected-panel",
      analysis_field_label_tag("Variables to convert"),
      analysis_transfer_listbox_input(
        "missing_values_selected",
        items = analysis_variable_items(selected_variables, variable_info, labels),
        selected = selected_selected,
        size = 13
      ),
      div(
        class = "dependent-order-actions",
        actionButton("missing_values_up", "Up", class = "btn-default btn-sm"),
        actionButton("missing_values_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(class = "variable-rename-grid-spacer"),
    div(
      class = "analysis-options-column analysis-options-panel missing-values-options",
      div(
        class = "analysis-option-group",
        div(class = "analysis-option-title", "Detect candidates"),
        div(
          class = "missing-values-option-row",
          checkboxInput("missing_detect_numeric", "Numeric codes", value = TRUE),
          checkboxInput("missing_detect_text", "Text / blank codes", value = TRUE)
        ),
        div(class = "analysis-option-title", "Use codes from"),
        div(
          class = "missing-values-option-row",
          checkboxInput("missing_use_auto", "Selected detected rows", value = TRUE),
          checkboxInput("missing_use_manual", "Manual codes below", value = FALSE)
        ),
        textAreaInput(
          "missing_manual_codes",
          "Manual missing codes",
          value = "",
          placeholder = "-999\n999\nN/A\n(blank)",
          rows = 7,
          width = "100%"
        ),
        div(class = "recode-help-text", "Use one code per line. Use (blank) for empty text values.")
      )
    )
  )
}

data_editor_missing_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Auto Missing Values"),
      div("Detect likely missing-value codes, review them, and mark selected codes as user missing before analysis.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Auto missing value detection", "missing_values"),
      analysis_workspace_body(
        "missing_values",
        uiOutput("missing_values_setup"),
        div(
          class = "analysis-action-row recode-same-action-row missing-values-action-row",
          div(
            class = "missing-values-action-cell",
            actionButton("mark_user_missing_values", "Mark as user missing", class = "btn btn-primary missing-values-apply-button"),
            actionButton("convert_missing_values_to_na", "Convert to NA", class = "btn btn-default missing-values-secondary-button")
          )
        ),
        uiOutput("missing_values_status"),
        div(class = "data-editor-result-output", DT::DTOutput("missing_value_candidates")),
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
  selected_names_fn = NULL,
  variable_info_fn = NULL,
  labels_fn = NULL,
  category_table_fn = NULL,
  user_missing_rules_fn = NULL,
  set_user_missing_rules_fn = NULL,
  update_existing_variable_fn,
  mark_settings_dirty
) {
  selected_variables <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)

  output$missing_values_setup <- renderUI({
    missing_values_setup_panel(
      file = current_data_file_fn(),
      data = tryCatch(dataset_fn(), error = function(e) NULL),
      variable_info = if (is.function(variable_info_fn)) tryCatch(variable_info_fn(), error = function(e) NULL) else NULL,
      labels = if (is.function(labels_fn)) labels_fn() else character(0),
      selected_variables = selected_variables(),
      input = input
    )
  })

  if (is.function(selected_names_fn) && is.function(variable_info_fn)) {
    register_analysis_data_viewer_handlers(
      input = input,
      output = output,
      prefix = "missing_values",
      title = "Auto Missing Values Data Viewer",
      dataset_fn = dataset_fn,
      selected_names_fn = selected_names_fn,
      variables_fn = selected_variables,
      variable_table_fn = variable_info_fn,
      labels_fn = if (is.function(labels_fn)) labels_fn else function() character(0),
      category_table_fn = if (is.function(category_table_fn)) category_table_fn else function() data.frame(check.names = FALSE)
    )
  }

  candidates <- reactive({
    file <- current_data_file_fn()
    if (is.null(file)) {
      return(data.frame(check.names = FALSE))
    }
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variables <- intersect(selected_variables(), names(data %||% data.frame()))
    if (length(variables) == 0) {
      return(data.frame(check.names = FALSE))
    }
    detect_missing_value_candidates(
      data[, variables, drop = FALSE],
      detect_numeric = isTRUE(input$missing_detect_numeric %||% TRUE),
      detect_text = isTRUE(input$missing_detect_text %||% TRUE)
    )
  })

  observe({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      selected_variables(character(0))
      last_message(NULL)
      return()
    }
    current <- selected_variables()
    updated <- intersect(current, names(data))
    if (!identical(updated, current)) {
      selected_variables(updated)
      last_message(NULL)
    }
  })

  observeEvent(input$missing_values_available_active, active_list("missing_values_available"), ignoreInit = TRUE)
  observeEvent(input$missing_values_selected_active, active_list("missing_values_selected"), ignoreInit = TRUE)

  observe({
    if (identical(active_list(), "missing_values_selected") && length(input$missing_values_selected %||% character(0)) > 0) {
      updateActionButton(session, "missing_values_move", label = "<")
    } else {
      updateActionButton(session, "missing_values_move", label = ">")
    }
  })

  observeEvent(input$missing_values_move, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    if (identical(active_list(), "missing_values_selected")) {
      chosen <- intersect(as.character(input$missing_values_selected %||% character(0)), current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("missing_values_available")
      last_message(NULL)
      mark_settings_dirty()
      return()
    }

    chosen <- intersect(as.character(input$missing_values_available %||% character(0)), setdiff(variables, current))
    if (length(chosen) == 0) return()
    selected_variables(c(current, chosen))
    active_list("missing_values_selected")
    last_message(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$missing_values_move_direct, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    payload <- input$missing_values_move_direct %||% list()
    source <- as.character(payload$source %||% "")
    chosen <- intersect(as.character(payload$values %||% character(0)), variables)
    if (identical(source, "selected")) {
      chosen <- intersect(chosen, current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("missing_values_available")
    } else {
      chosen <- intersect(chosen, setdiff(variables, current))
      if (length(chosen) == 0) return()
      selected_variables(c(current, chosen))
      active_list("missing_values_selected")
    }
    last_message(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$missing_values_available_doubleclick, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    chosen <- intersect(as.character(input$missing_values_available_doubleclick$value %||% ""), setdiff(variables, current))
    if (length(chosen) == 0) return()
    selected_variables(c(current, chosen))
    active_list("missing_values_selected")
    last_message(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$missing_values_selected_doubleclick, {
    current <- selected_variables()
    chosen <- intersect(as.character(input$missing_values_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    selected_variables(setdiff(current, chosen))
    active_list("missing_values_available")
    last_message(NULL)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "missing_values_available",
    selected_id = "missing_values_selected",
    selected_values = selected_variables,
    all_values_fn = function() names(tryCatch(dataset_fn(), error = function(e) data.frame()) %||% data.frame()),
    active_list = active_list,
    mark_settings_dirty = mark_settings_dirty,
    after_change = function(target, chosen, next_values) {
      last_message(NULL)
    }
  )

  observeEvent(input$missing_values_up, {
    updated <- move_order_item(selected_variables(), input$missing_values_selected, "up")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$missing_values_down, {
    updated <- move_order_item(selected_variables(), input$missing_values_selected, "down")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  output$missing_values_status <- renderUI({
    file <- current_data_file_fn()
    if (is.null(file)) {
      return(div(class = "empty-message", div("Load a data file before detecting missing values.")))
    }
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variables <- intersect(selected_variables(), names(data %||% data.frame()))
    if (length(variables) == 0) {
      return(div(class = "empty-message", div("Select variables to detect or manually convert missing-value codes.")))
    }
    data <- data[, variables, drop = FALSE]
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

  selected_missing_rules <- reactive({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    table <- candidates()
    use_auto <- isTRUE(input$missing_use_auto %||% TRUE)
    use_manual <- isTRUE(input$missing_use_manual %||% FALSE)
    variables <- intersect(selected_variables(), names(data %||% data.frame()))
    selected <- input$missing_value_candidates_rows_selected %||% integer(0)
    selected <- suppressWarnings(as.integer(selected))
    selected <- selected[is.finite(selected) & selected >= 1 & selected <= nrow(table)]
    if (is.null(data)) {
      stop("Load a data file before applying missing-value rules.", call. = FALSE)
    }
    if (length(variables) == 0) {
      stop("Select at least one variable to convert.", call. = FALSE)
    }
    if (!use_auto && !use_manual) {
      stop("Choose detected rows, manual codes, or both.", call. = FALSE)
    }
    if (use_auto && (is.null(table) || nrow(table) == 0)) {
      stop("No missing-value candidates are available.", call. = FALSE)
    }
    if (use_auto && length(selected) == 0) {
      stop("Select at least one candidate row to apply.", call. = FALSE)
    }

    auto_rules <- if (use_auto && !is.null(table) && nrow(table) > 0 && length(selected) > 0) {
      table[selected, , drop = FALSE]
    } else {
      data.frame(check.names = FALSE)
    }
    manual_rules <- if (use_manual) {
      missing_manual_rules(
        data,
        variables = variables,
        codes = missing_manual_codes_from_text(input$missing_manual_codes)
      )
    } else {
      data.frame(check.names = FALSE)
    }
    rule_sets <- Filter(function(item) is.data.frame(item) && nrow(item) > 0, list(auto_rules, manual_rules))
    if (length(rule_sets) == 0) {
      stop("Select detected rows or enter manual missing-value codes.", call. = FALSE)
    }
    rules <- do.call(rbind, rule_sets)
    rownames(rules) <- NULL
    normalize_missing_rules(rules, variables)
  })

  observeEvent(input$mark_user_missing_values, {
    rules <- tryCatch(
      selected_missing_rules(),
      shiny.silent.error = function(e) NULL,
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 5)
        NULL
      }
    )
    if (is.null(rules) || nrow(rules) == 0) {
      return()
    }
    if (!is.function(set_user_missing_rules_fn)) {
      showNotification("User-missing rules are not available in this session.", type = "warning", duration = 5)
      return()
    }
    existing <- if (is.function(user_missing_rules_fn)) user_missing_rules_fn() else data.frame(check.names = FALSE)
    merged <- merge_missing_user_rules(existing, rules, names(tryCatch(dataset_fn(), error = function(e) data.frame())))
    set_user_missing_rules_fn(merged)
    if (is.function(mark_settings_dirty)) {
      mark_settings_dirty()
    }
    variables <- unique(as.character(rules$variable))
    last_message(sprintf(
      "Marked %s missing-value rule(s) as user missing for analysis. Original data values are preserved: %s",
      nrow(rules),
      paste(variables, collapse = ", ")
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$convert_missing_values_to_na, {
    rules <- tryCatch(
      selected_missing_rules(),
      shiny.silent.error = function(e) NULL,
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 5)
        NULL
      }
    )
    if (is.null(rules) || nrow(rules) == 0) {
      return()
    }
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before applying missing-value rules.", type = "warning", duration = 5)
      return()
    }

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
