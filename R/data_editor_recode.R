# Same-variable recoding for the Data Editor menu.

recode_same_rule_count <- function() 6L

empty_recode_same_rules <- function() {
  data.frame(old = character(0), new = character(0), stringsAsFactors = FALSE)
}

normalize_recode_same_rules <- function(rules) {
  if (is.null(rules) || !is.data.frame(rules) || !all(c("old", "new") %in% names(rules))) {
    return(empty_recode_same_rules())
  }
  rules <- data.frame(
    old = trimws(as.character(rules$old %||% character(0))),
    new = trimws(as.character(rules$new %||% character(0))),
    stringsAsFactors = FALSE
  )
  rules[nzchar(rules$old), , drop = FALSE]
}

recode_same_values <- function(values, rules, keep_unmatched = TRUE) {
  rules <- normalize_recode_same_rules(rules)
  if (nrow(rules) == 0) {
    return(values)
  }

  source <- as.vector(values)
  output <- if (isTRUE(keep_unmatched)) source else rep(NA, length(source))
  source_text <- trimws(as.character(source))
  for (index in seq_len(nrow(rules))) {
    matched <- !is.na(source) & identical(FALSE, is.na(rules$old[[index]])) & source_text == rules$old[[index]]
    output[matched] <- rules$new[[index]]
  }

  numeric_candidate <- suppressWarnings(as.numeric(output))
  non_missing_text <- trimws(as.character(output[!is.na(output)]))
  if (length(non_missing_text) > 0 && all(nzchar(non_missing_text)) && all(!is.na(numeric_candidate[!is.na(output)]))) {
    return(numeric_candidate)
  }
  output
}

recode_same_rules_from_input <- function(input, prefix = "recode_same") {
  count <- recode_same_rule_count()
  data.frame(
    old = vapply(seq_len(count), function(index) as.character(input[[paste0(prefix, "_old_", index)]] %||% ""), character(1)),
    new = vapply(seq_len(count), function(index) as.character(input[[paste0(prefix, "_new_", index)]] %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
}

recode_same_setup_panel <- function(file, data, variable_info, labels = character(0), selected_variables = character(0), input = NULL) {
  if (is.null(file) || is.null(data)) {
    return(setup_empty_message("Load a data file in the Data tab before using same-variable recoding."))
  }

  variables <- names(data)
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), variables)
  available <- setdiff(variables, selected_variables)
  selected_available <- selected_order_items(input$recode_same_available %||% character(0), available)
  selected_selected <- selected_order_items(input$recode_same_selected %||% character(0), selected_variables)
  measurement_choices <- c(
    "Keep current type" = "",
    "Binary" = "binary",
    "Categorical" = "category",
    "Ordered" = "ordered",
    "Continuous" = "continuous"
  )

  div(
    class = "recode-same-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input(
        "recode_same_available",
        items = analysis_variable_items(available, variable_info, labels),
        selected = selected_available,
        size = 18
      )
    ),
    div(
      class = "analysis-transfer-controls recode-same-transfer-controls",
      actionButton(
        "recode_same_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (length(available) == 0 && length(selected_variables) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables to recode", analysis_allowed_measurements_all()),
      analysis_transfer_listbox_input(
        "recode_same_selected",
        items = analysis_variable_items(selected_variables, variable_info, labels),
        selected = selected_selected,
        size = 18
      ),
      div(
        class = "dependent-order-actions",
        actionButton("recode_same_up", "Up", class = "btn-default btn-sm"),
        actionButton("recode_same_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel recode-same-options",
      div(class = "analysis-option-group",
        div(class = "analysis-option-title", "Rules"),
        tags$table(
          class = "recode-rule-table",
          tags$thead(tags$tr(tags$th("Old value"), tags$th("New value"))),
          tags$tbody(
            lapply(seq_len(recode_same_rule_count()), function(index) {
              tags$tr(
                tags$td(textInput(paste0("recode_same_old_", index), NULL, value = "", width = "100%")),
                tags$td(textInput(paste0("recode_same_new_", index), NULL, value = "", width = "100%"))
              )
            })
          )
        )
      ),
      checkboxInput("recode_same_keep_unmatched", "Keep unmatched values", value = TRUE),
      selectInput("recode_same_measurement", "Measurement after recoding", choices = measurement_choices, selected = "", selectize = FALSE)
    )
  )
}

data_editor_same_variable_panel <- function() {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Recode into Same Variables"),
      div("Change coding values in selected variables and keep the same variable names.", class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      analysis_workspace_heading("Same-variable recoding", "recode_same"),
      analysis_workspace_body(
        "recode_same",
        uiOutput("recode_same_setup"),
        div(
          class = "analysis-action-row recode-same-action-row",
          actionButton("apply_recode_same", "Apply recoding", class = "btn btn-primary"),
          uiOutput("recode_same_reset_control")
        ),
        uiOutput("recode_same_message"),
        DT::DTOutput("recode_same_preview")
      )
    )
  )
}

register_recode_same_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  selected_names_fn,
  variable_info_fn,
  labels_fn,
  category_table_fn,
  update_existing_variable_fn,
  mark_settings_dirty
) {
  selected_variables <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  last_message <- reactiveVal(NULL)

  output$recode_same_setup <- renderUI({
    recode_same_setup_panel(
      file = current_data_file_fn(),
      data = tryCatch(dataset_fn(), error = function(e) NULL),
      variable_info = tryCatch(variable_info_fn(), error = function(e) NULL),
      labels = labels_fn(),
      selected_variables = selected_variables(),
      input = input
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "recode_same",
    title = "Same-variable Recoding Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = selected_variables,
    variable_table_fn = variable_info_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observe({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      selected_variables(character(0))
      return()
    }
    current <- selected_variables()
    updated <- intersect(current, names(data))
    if (!identical(updated, current)) {
      selected_variables(updated)
    }
  })

  observeEvent(input$recode_same_available_active, active_list("recode_same_available"), ignoreInit = TRUE)
  observeEvent(input$recode_same_selected_active, active_list("recode_same_selected"), ignoreInit = TRUE)

  observe({
    if (identical(active_list(), "recode_same_selected") && length(input$recode_same_selected %||% character(0)) > 0) {
      updateActionButton(session, "recode_same_move", label = "<")
    } else {
      updateActionButton(session, "recode_same_move", label = ">")
    }
  })

  observeEvent(input$recode_same_move, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) return()
    variables <- names(data)
    current <- intersect(selected_variables(), variables)
    if (identical(active_list(), "recode_same_selected")) {
      chosen <- intersect(as.character(input$recode_same_selected %||% character(0)), current)
      if (length(chosen) == 0) return()
      selected_variables(setdiff(current, chosen))
      active_list("recode_same_available")
      mark_settings_dirty()
      return()
    }

    chosen <- intersect(as.character(input$recode_same_available %||% character(0)), setdiff(variables, current))
    if (length(chosen) == 0) return()
    selected_variables(c(current, chosen))
    active_list("recode_same_selected")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$recode_same_selected_doubleclick, {
    current <- selected_variables()
    chosen <- intersect(as.character(input$recode_same_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    selected_variables(setdiff(current, chosen))
    active_list("recode_same_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$recode_same_up, {
    updated <- move_order_item(selected_variables(), input$recode_same_selected, "up")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  observeEvent(input$recode_same_down, {
    updated <- move_order_item(selected_variables(), input$recode_same_selected, "down")
    if (isTRUE(updated$changed)) {
      selected_variables(updated$order)
      mark_settings_dirty()
    }
  })

  output$recode_same_reset_control <- renderUI({
    analysis_reset_button("reset_recode_same", enabled = length(selected_variables()) > 0)
  })

  observeEvent(input$reset_recode_same, {
    if (length(selected_variables()) == 0) return()
    selected_variables(character(0))
    last_message(NULL)
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("recode_same_available", "recode_same_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  recoded_preview <- reactive({
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    variables <- intersect(selected_variables(), names(data %||% data.frame()))
    if (is.null(data) || length(variables) == 0) {
      return(data.frame())
    }
    rules <- recode_same_rules_from_input(input)
    preview <- as.data.frame(data[seq_len(min(20L, nrow(data))), variables, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)
    for (name in variables) {
      preview[[name]] <- recode_same_values(preview[[name]], rules, keep_unmatched = isTRUE(input$recode_same_keep_unmatched))
    }
    preview
  })

  output$recode_same_preview <- DT::renderDT({
    preview <- recoded_preview()
    if (is.null(preview) || ncol(preview) == 0) {
      return(DT::datatable(data.frame(Message = "Select variables and enter recoding rules."), rownames = FALSE, options = list(dom = "t")))
    }
    DT::datatable(preview, rownames = FALSE, options = list(pageLength = 20, lengthChange = FALSE, scrollX = TRUE))
  })

  output$recode_same_message <- renderUI({
    message <- last_message()
    if (is.null(message)) {
      return(div(class = "empty-message", div("Enter old and new values, then click Apply recoding.")))
    }
    div(class = "recode-same-status", message)
  })

  observeEvent(input$apply_recode_same, {
    data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (is.null(data)) {
      showNotification("Load a data file before recoding.", type = "warning", duration = 5)
      return()
    }
    variables <- intersect(selected_variables(), names(data))
    if (length(variables) == 0) {
      showNotification("Select at least one variable to recode.", type = "warning", duration = 5)
      return()
    }
    rules <- normalize_recode_same_rules(recode_same_rules_from_input(input))
    if (nrow(rules) == 0) {
      showNotification("Enter at least one old value to recode.", type = "warning", duration = 5)
      return()
    }

    measurement <- as.character(input$recode_same_measurement %||% "")
    if (!nzchar(measurement)) measurement <- NULL
    for (name in variables) {
      values <- recode_same_values(data[[name]], rules, keep_unmatched = isTRUE(input$recode_same_keep_unmatched))
      update_existing_variable_fn(name, values, measurement = measurement)
    }
    last_message(sprintf("Recoded %s variable(s): %s", length(variables), paste(variables, collapse = ", ")))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
