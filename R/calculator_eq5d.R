# EQ-5D calculator module.

eq5d_item_specs <- function() {
  data.frame(
    id = paste0("eq5d_item_", 1:5),
    label = c("EQ1 (Mobility)", "EQ2 (Self-care)", "EQ3 (Usual activities)", "EQ4 (Pain/discomfort)", "EQ5 (Anxiety/depression)"),
    stringsAsFactors = FALSE
  )
}

eq5d_reference_values <- function(type = "5L") {
  type <- toupper(as.character(type %||% "5L"))
  if (identical(type, "3L")) {
    return(list(
      constant = 0.050,
      n = 0.050,
      labels = c("M2", "M3", "SC2", "SC3", "UA2", "UA3", "PD2", "PD3", "AD2", "AD3"),
      maps = list(
        c(`1` = 0, `2` = 0.096, `3` = 0.418),
        c(`1` = 0, `2` = 0.046, `3` = 0.136),
        c(`1` = 0, `2` = 0.051, `3` = 0.208),
        c(`1` = 0, `2` = 0.037, `3` = 0.151),
        c(`1` = 0, `2` = 0.043, `3` = 0.158)
      )
    ))
  }
  list(
    constant = 0.096,
    n = 0.078,
    labels = c("M2", "M3", "M4", "M5", "S2", "S3", "S4", "S5", "U2", "U3", "U4", "U5", "P2", "P3", "P4", "P5", "A2", "A3", "A4", "A5"),
    maps = list(
      c(`1` = 0, `2` = 0.046, `3` = 0.058, `4` = 0.133, `5` = 0.251),
      c(`1` = 0, `2` = 0.032, `3` = 0.050, `4` = 0.078, `5` = 0.122),
      c(`1` = 0, `2` = 0.021, `3` = 0.051, `4` = 0.100, `5` = 0.175),
      c(`1` = 0, `2` = 0.042, `3` = 0.053, `4` = 0.166, `5` = 0.207),
      c(`1` = 0, `2` = 0.033, `3` = 0.046, `4` = 0.102, `5` = 0.137)
    )
  )
}

eq5d_ordered_choices <- function(data, variable_info = NULL) {
  hint8_ordered_choices(data, variable_info)
}

eq5d_selected_variables <- function(input) {
  specs <- eq5d_item_specs()
  vapply(specs$id, function(id) as.character(input[[id]] %||% ""), character(1))
}

eq5d_score <- function(items, type = "5L", profile_11111_as_one = TRUE) {
  items <- as.data.frame(items, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(items) != 5) stop("EQ-5D requires exactly 5 item columns.", call. = FALSE)

  reference <- eq5d_reference_values(type)
  max_level <- if (toupper(as.character(type %||% "5L")) == "3L") 3L else 5L
  values <- as.data.frame(lapply(items, function(column) suppressWarnings(as.integer(as.character(column)))), check.names = FALSE)
  penalties <- lapply(seq_len(5), function(index) {
    valid <- !is.na(values[[index]]) & values[[index]] %in% seq_len(max_level)
    out <- rep(NA_real_, nrow(values))
    out[valid] <- unname(reference$maps[[index]][as.character(values[[index]][valid])])
    out
  })
  penalty_data <- as.data.frame(penalties, stringsAsFactors = FALSE)
  complete <- stats::complete.cases(penalty_data)
  score <- rep(NA_real_, nrow(items))
  any_highest <- rowSums(values == max_level, na.rm = TRUE) > 0
  score[complete] <- 1 - (reference$constant + rowSums(penalty_data[complete, , drop = FALSE]) + ifelse(any_highest[complete], reference$n, 0))
  if (isTRUE(profile_11111_as_one)) {
    all_one <- stats::complete.cases(values) & rowSums(values == 1L, na.rm = TRUE) == 5L
    score[all_one] <- 1
  }
  score
}

eq5d_calculator_result <- function(data, selected, type = "5L", ordered_choices = NULL, profile_11111_as_one = TRUE) {
  selected <- as.character(selected)
  selected <- selected[nzchar(selected)]
  if (length(selected) != 5 || anyDuplicated(selected)) stop("Select 5 different EQ-5D item variables.", call. = FALSE)
  if (is.null(data) || !all(selected %in% names(data))) stop("Selected variables are not available in the loaded data.", call. = FALSE)
  if (!is.null(ordered_choices) && !all(selected %in% ordered_choices)) stop("EQ-5D item variables must be ordered variables.", call. = FALSE)
  result <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  result[["EQ5D"]] <- eq5d_score(result[, selected, drop = FALSE], type = type, profile_11111_as_one = profile_11111_as_one)
  result
}

eq5d_calculator_tab_panel <- function() {
  tabPanel(
    "EQ5D",
    value = "calculator_eq5d",
    div(
      class = "page-shell",
      div(class = "app-heading", h1("EQ-5D Calculator"), div("Select the 5 ordered EQ-5D item variables and add EQ5D to the current data.", class = "app-subtitle")),
      div(
        class = "workspace-panel frequencies-workspace-panel hint8-calculator-workspace",
        style = "min-width:980px;overflow-x:auto;",
        h3("EQ-5D"),
        div(class = "load-message", textOutput("eq5d_loaded_message")),
        uiOutput("eq5d_calculator_setup"),
        div(class = "analysis-action-row hint8-action-row", actionButton("run_eq5d_calculator", "Calculate", class = "btn btn-primary"), downloadButton("download_eq5d_calculator", "Download CSV", class = "btn btn-default")),
        uiOutput("eq5d_calculator_summary"),
        DT::DTOutput("eq5d_calculator_preview")
      )
    )
  )
}

eq5d_reference_table <- function(type = "5L") {
  reference <- eq5d_reference_values(type)
  values <- unlist(reference$maps, use.names = FALSE)
  values <- values[values != 0]
  rows <- data.frame(
    Item = c(reference$labels, "constant", if (toupper(type) == "3L") "N3" else "N4"),
    Value = c(values, reference$constant, reference$n),
    stringsAsFactors = FALSE
  )
  tags$table(
    class = "hint8-initial-table",
    tags$thead(tags$tr(tags$th("Reference"), tags$th("Value"))),
    tags$tbody(lapply(seq_len(nrow(rows)), function(index) {
      tags$tr(tags$td(rows$Item[[index]]), tags$td(sprintf("%.3f", rows$Value[[index]])))
    }))
  )
}

eq5d_setup_ui <- function(file, data, variable_info, input) {
  if (is.null(file)) return(setup_empty_message("Load a data file in the Data tab before using the EQ-5D calculator."))
  choices <- eq5d_ordered_choices(data, variable_info)
  available_items <- analysis_variable_items(choices, variable_info, character(0))
  specs <- eq5d_item_specs()
  selected_type <- if (identical(input$eq5d_type, "3L")) "3L" else "5L"
  variable_inputs <- lapply(seq_len(nrow(specs)), function(index) {
    id <- specs$id[[index]]
    hint8_item_select_control(id, specs$label[[index]], choices, selected = input[[id]] %||% "")
  })
  div(
    class = "frequencies-setup-grid metabolic-setup-grid",
    div(class = "analysis-transfer-column analysis-transfer-panel", analysis_field_label_tag("Variables"), analysis_transfer_listbox_input("eq5d_available", available_items, selected = isolate(input$eq5d_available), size = 19)),
    div(class = "analysis-transfer-controls hint8-transfer-spacer"),
    div(
      class = "analysis-transfer-column analysis-transfer-panel metabolic-target-panel",
      analysis_field_label_tag("EQ-5D variables"),
      selectInput("eq5d_type", "Type", choices = c("EQ-5D-5L" = "5L", "EQ-5D-3L" = "3L"), selected = selected_type, width = "100%"),
      div(class = "metabolic-variable-input-grid", variable_inputs),
      checkboxInput("eq5d_profile_11111_as_one", "profile 11111 -> EQ5D = 1.0", value = isTRUE(input$eq5d_profile_11111_as_one %||% TRUE))
    ),
    div(class = "analysis-options-column analysis-options-panel metabolic-reference-panel", div(class = "analysis-option-title", "Initial values"), eq5d_reference_table(selected_type))
  )
}

register_eq5d_calculator_handlers <- function(input, output, session, dataset_fn, current_data_file_fn, variable_info_fn, add_calculated_variable_fn) {
  output$eq5d_loaded_message <- renderText({
    file <- current_data_file_fn()
    hint8_loaded_message_text(file, if (is.null(file)) NULL else dataset_fn())
  })
  output$eq5d_calculator_setup <- renderUI({
    file <- current_data_file_fn()
    data <- if (is.null(file)) NULL else dataset_fn()
    eq5d_setup_ui(file, data, if (is.null(file)) NULL else variable_info_fn(), input)
  })
  observeEvent(input$eq5d_available, {
    picked <- as.character(input$eq5d_available %||% "")
    if (!nzchar(picked)) return()
    selected <- eq5d_selected_variables(input)
    if (picked %in% selected) return()
    empty_index <- which(!nzchar(selected))[1]
    if (!is.na(empty_index)) updateSelectInput(session, eq5d_item_specs()$id[[empty_index]], selected = picked)
  }, ignoreInit = TRUE)
  result <- eventReactive(input$run_eq5d_calculator, {
    if (is.null(current_data_file_fn())) {
      showNotification("Load a data file before calculating EQ-5D.", type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch({
      ordered_choices <- eq5d_ordered_choices(dataset_fn(), variable_info_fn())
      result_data <- eq5d_calculator_result(dataset_fn(), eq5d_selected_variables(input), type = input$eq5d_type %||% "5L", ordered_choices = ordered_choices, profile_11111_as_one = isTRUE(input$eq5d_profile_11111_as_one))
      add_calculated_variable_fn("EQ5D", result_data[["EQ5D"]], var_label = "EQ-5D score", measurement = "continuous")
      showNotification("EQ5D was added to the current data.", type = "message", duration = 5)
      result_data
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "warning", duration = 6)
      NULL
    })
  }, ignoreInit = TRUE)
  output$eq5d_calculator_summary <- renderUI({
    data <- result()
    if (is.null(data)) return(div(class = "empty-message", div("Select variables and click Calculate.")))
    div(class = "empty-message", div(sprintf("Calculated EQ5D for %s rows. The variable is available in analysis menus.", nrow(data))))
  })
  output$eq5d_calculator_preview <- DT::renderDT({
    data <- result()
    if (is.null(data)) return(DT::datatable(data.frame(Message = "No EQ-5D result yet.", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    selected <- eq5d_selected_variables(input)
    preview_names <- intersect(c(selected, "EQ5D"), names(data))
    DT::datatable(utils::head(data[, preview_names, drop = FALSE], 50), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download_eq5d_calculator <- downloadHandler(
    filename = function() paste0("easyflow_eq5d_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      data <- result()
      if (is.null(data)) data <- eq5d_calculator_result(dataset_fn(), eq5d_selected_variables(input), type = input$eq5d_type %||% "5L")
      utils::write.csv(data, file, row.names = FALSE, na = "")
    }
  )
  invisible(TRUE)
}
