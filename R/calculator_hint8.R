# HINT-8 calculator module.

hint8_item_specs <- function() {
  data.frame(
    id = paste0("hint8_item_", 1:8),
    label = c(
      "LQ1 (계단 오르기)",
      "LQ2 (통증)",
      "LQ3 (기운)",
      "LQ4 (일하기)",
      "LQ5 (우울)",
      "LQ6 (기억)",
      "LQ7 (잠자기)",
      "LQ8 (행복)"
    ),
    stringsAsFactors = FALSE
  )
}

hint8_penalty_maps <- function() {
  list(
    c(`1` = 0, `2` = 0.018, `3` = 0, `4` = 0.122),
    c(`1` = 0, `2` = 0.055, `3` = 0.116, `4` = 0.188),
    c(`1` = 0, `2` = 0.019, `3` = 0.019, `4` = 0.070),
    c(`1` = 0, `2` = 0.004, `3` = 0.028, `4` = 0.036),
    c(`1` = 0, `2` = 0.012, `3` = 0.044, `4` = 0.098),
    c(`1` = 0, `2` = 0.014, `3` = 0.058, `4` = 0.109),
    c(`1` = 0, `2` = 0, `3` = 0.020, `4` = 0.090),
    c(`1` = 0, `2` = 0.014, `3` = 0.068, `4` = 0.082)
  )
}

hint8_formula_initial_score <- function() {
  1 - 0.073
}

hint8_score <- function(items, profile_11111111_as_one = TRUE) {
  items <- as.data.frame(items, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(items) != 8) {
    stop("HINT8 requires exactly 8 item columns.", call. = FALSE)
  }

  maps <- hint8_penalty_maps()
  penalties <- lapply(seq_len(8), function(index) {
    values <- suppressWarnings(as.integer(as.character(items[[index]])))
    valid <- !is.na(values) & values %in% 1:4
    out <- rep(NA_real_, length(values))
    out[valid] <- unname(maps[[index]][as.character(values[valid])])
    out
  })

  penalty_data <- as.data.frame(penalties, stringsAsFactors = FALSE)
  complete <- stats::complete.cases(penalty_data)
  score <- rep(NA_real_, nrow(items))
  score[complete] <- 1 - (0.073 + rowSums(penalty_data[complete, , drop = FALSE]))
  if (isTRUE(profile_11111111_as_one)) {
    raw_values <- as.data.frame(lapply(items, function(column) suppressWarnings(as.integer(as.character(column)))), check.names = FALSE)
    all_one <- stats::complete.cases(raw_values) & rowSums(raw_values == 1L, na.rm = TRUE) == 8L
    score[all_one] <- 1
  }
  score
}

hint8_variable_choices <- function(data, variable_info = NULL) {
  if (is.null(data) || ncol(data) == 0) {
    return(character(0))
  }
  names(data)
}

hint8_selected_variables <- function(input) {
  specs <- hint8_item_specs()
  vapply(specs$id, function(id) as.character(input[[id]] %||% ""), character(1))
}

hint8_calculator_result <- function(
  data,
  selected,
  variable_choices = NULL,
  profile_11111111_as_one = TRUE
) {
  selected <- as.character(selected)
  selected <- selected[nzchar(selected)]
  if (length(selected) != 8 || anyDuplicated(selected)) {
    stop("Select 8 different HINT8 item variables.", call. = FALSE)
  }
  if (is.null(data) || !all(selected %in% names(data))) {
    stop("Selected variables are not available in the loaded data.", call. = FALSE)
  }
  if (!is.null(variable_choices) && !all(selected %in% variable_choices)) {
    stop("Selected HINT8 item variables are not available in the loaded data.", call. = FALSE)
  }

  result <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  result[["hint8_score"]] <- hint8_score(
    result[, selected, drop = FALSE],
    profile_11111111_as_one = profile_11111111_as_one
  )
  result
}

hint8_calculated_variable_info_row <- function(name, values, template_info = NULL) {
  values <- as.numeric(values)
  columns <- names(template_info %||% data.frame())
  required <- c("source_order", "name", "var_label", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value")
  columns <- unique(c(columns, required))
  row <- as.data.frame(as.list(stats::setNames(rep("", length(columns)), columns)), stringsAsFactors = FALSE, check.names = FALSE)

  source_order <- 1L
  if (is.data.frame(template_info) && "source_order" %in% names(template_info) && nrow(template_info) > 0) {
    source_order <- suppressWarnings(max(as.integer(template_info$source_order), na.rm = TRUE)) + 1L
    if (!is.finite(source_order)) {
      source_order <- nrow(template_info) + 1L
    }
  }

  present <- stats::na.omit(values)
  row$source_order <- source_order
  row$name <- name
  row$var_label <- "HINT8 score"
  row$measurement <- "continuous"
  row$storage_type <- "numeric"
  row$n_unique <- length(unique(present))
  row$n_missing <- sum(is.na(values))
  row$min_value <- if (length(present) > 0) as.character(min(present)) else ""
  row$max_value <- if (length(present) > 0) as.character(max(present)) else ""
  if ("_row" %in% names(row)) {
    row$`_row` <- name
  }
  row
}

hint8_calculator_tab_panel <- function() {
  tabPanel(
    "HINT8",
    value = "calculator_hint8",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("HINT8 Calculator"),
        div("Select the 8 HINT8 item variables from the current data and calculate hint8_score.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel hint8-calculator-workspace",
        style = "min-width:980px;overflow-x:auto;",
        h3("HINT8"),
        div(class = "load-message", textOutput("hint8_loaded_message")),
        uiOutput("hint8_calculator_setup"),
        div(
          class = "analysis-action-row calculator-action-row",
          div(
            class = "calculator-action-row-controls",
            actionButton("run_hint8_calculator", "Calculate", class = "btn btn-primary"),
            downloadButton("download_hint8_calculator", "Download CSV", class = "btn btn-default")
          )
        ),
        uiOutput("hint8_calculator_summary"),
        DT::DTOutput("hint8_calculator_preview")
      )
    )
  )
}

calculator_tab_panel <- function() {
  navbarMenu(
    "Calculator",
    lazy_tab_panel("HINT8", "calculator_hint8", "lazy_calculator_hint8"),
    lazy_tab_panel("EQ-5D", "calculator_eq5d", "lazy_calculator_eq5d"),
    lazy_tab_panel("Metabolic syndrome", "calculator_metabolic", "lazy_calculator_metabolic"),
    lazy_tab_panel("Framingham risk score", "calculator_frs", "lazy_calculator_frs"),
    lazy_tab_panel("ASCVD10", "calculator_ascvd10", "lazy_calculator_ascvd10"),
    lazy_tab_panel("Metabolic severity", "calculator_metabolic_severity", "lazy_calculator_metabolic_severity")
  )
}

hint8_item_select_control <- function(id, label, choices, selected = "") {
  selectInput(
    id,
    label,
    choices = c("Select variable" = "", choices),
    selected = selected,
    width = "100%"
  )
}

hint8_loaded_message_text <- function(file = NULL, data = NULL) {
  if (is.null(file)) {
    return("No data file is open.")
  }
  sprintf("Loaded %s: %s variables, %s rows.", file$name, ncol(data), nrow(data))
}

hint8_weight_values_table <- function() {
  maps <- hint8_penalty_maps()
  levels <- as.character(1:4)
  dimensions <- paste0("LQ", seq_along(maps))
  tags$table(
    class = "hint8-initial-table hint8-weight-matrix-table",
    tags$thead(tags$tr(
      tags$th(""),
      lapply(levels, tags$th)
    )),
    tags$tbody(
      lapply(seq_along(dimensions), function(index) {
        values <- maps[[index]][levels]
        tags$tr(
          tags$td(dimensions[[index]], class = "hint8-dimension-label"),
          lapply(values, function(value) tags$td(sprintf("%.3f", unname(value))))
        )
      }),
      tags$tr(
        tags$td("constant", class = "hint8-dimension-label"),
        tags$td(sprintf("%.3f", 0.073), colspan = length(levels))
      )
    )
  )
}

hint8_output_table <- function() {
  tags$table(
    class = "hint8-initial-table hint8-output-table",
    tags$tbody(tags$tr(tags$td("Score"), tags$td("hint8_score")))
  )
}

hint8_calculator_setup_ui <- function(file, data, variable_info, input) {
  if (is.null(file)) {
    return(setup_empty_message("Load a data file in the Data tab before using the HINT8 calculator."))
  }

  choices <- hint8_variable_choices(data, variable_info)
  specs <- hint8_item_specs()
  available_items <- analysis_variable_items(choices, variable_info, character(0))
  profile_as_one <- isTRUE(input$hint8_profile_11111111_as_one %||% TRUE)
  initial_score <- if (isTRUE(profile_as_one)) 1 else hint8_formula_initial_score()
  variable_inputs <- lapply(seq_len(nrow(specs)), function(index) {
    hint8_item_select_control(
      specs$id[[index]],
      specs$label[[index]],
      choices,
      selected = isolate(input[[specs$id[[index]]]]) %||% ""
    )
  })

  div(
    class = "frequencies-setup-grid hint8-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("hint8_available", available_items, selected = isolate(input$hint8_available), size = 17)
    ),
    div(class = "analysis-transfer-controls hint8-transfer-spacer"),
    div(
      class = "analysis-transfer-column analysis-transfer-panel hint8-target-panel",
      analysis_field_label_tag("HINT8 variables"),
      div(class = "hint8-variable-input-grid hint8-single-column-input-grid", variable_inputs)
    ),
    div(
      class = "analysis-options-column analysis-options-panel hint8-initial-panel",
      div(class = "analysis-option-title", "Initial values"),
      div(
        class = "hint8-initial-content",
        div(
          class = "step-summary hint8-initial-summary",
          div(sprintf("Initial score: %.3f", initial_score), class = "step-summary-title"),
          div("When checked, profile 11111111 is scored as 1.000. When unchecked, the formula score is 0.927.", class = "step-summary-detail")
        ),
        checkboxInput(
          "hint8_profile_11111111_as_one",
          "profile 11111111 -> HINT8 = 1.0",
          value = profile_as_one
        ),
        hint8_weight_values_table(),
        div(class = "analysis-option-title calculator-output-title", "Output"),
        hint8_output_table()
      )
    )
  )
}

register_hint8_calculator_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  variable_info_fn,
  add_calculated_variable_fn
) {
  output$hint8_loaded_message <- renderText({
    file <- current_data_file_fn()
    hint8_loaded_message_text(file, if (is.null(file)) NULL else dataset_fn())
  })

  output$hint8_calculator_setup <- renderUI({
    file <- current_data_file_fn()
    data <- if (is.null(file)) NULL else dataset_fn()
    variable_info <- if (is.null(file)) NULL else variable_info_fn()
    hint8_calculator_setup_ui(file, data, variable_info, input)
  })

  observeEvent(input$hint8_available, {
    picked <- utils::tail(as.character(input$hint8_available %||% ""), 1)
    picked <- if (length(picked) > 0) picked[[1]] else ""
    if (!nzchar(picked)) {
      return()
    }
    selected <- hint8_selected_variables(input)
    if (picked %in% selected) {
      return()
    }
    empty_index <- which(!nzchar(selected))[1]
    if (is.na(empty_index)) {
      return()
    }
    updateSelectInput(session, hint8_item_specs()$id[[empty_index]], selected = picked)
  }, ignoreInit = TRUE)

  result <- eventReactive(input$run_hint8_calculator, {
    if (is.null(current_data_file_fn())) {
      showNotification("Load a data file before calculating HINT8.", type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch(
      {
        result_data <- hint8_calculator_result(
          dataset_fn(),
          hint8_selected_variables(input),
          variable_choices = hint8_variable_choices(dataset_fn(), variable_info_fn()),
          profile_11111111_as_one = isTRUE(input$hint8_profile_11111111_as_one)
        )
        add_calculated_variable_fn(
          "hint8_score",
          result_data[["hint8_score"]],
          var_label = "HINT8 score",
          measurement = "continuous"
        )
        showNotification("hint8_score was added to the current data.", type = "message", duration = 5)
        result_data
      },
      error = function(error) {
        showNotification(conditionMessage(error), type = "warning", duration = 6)
        NULL
      }
    )
  }, ignoreInit = TRUE)

  output$hint8_calculator_summary <- renderUI({
    data <- result()
    if (is.null(data)) {
      return(NULL)
    }
    score <- data[["hint8_score"]]
    div(
      class = "empty-message",
      div(sprintf(
        "Calculated %s for %s rows. Missing/invalid item rows: %s. The score variable is available in analysis menus.",
        "hint8_score",
        nrow(data),
        sum(is.na(score))
      ))
    )
  })

  output$hint8_calculator_preview <- DT::renderDT({
    data <- result()
    if (is.null(data)) {
      return(NULL)
    }
    selected <- hint8_selected_variables(input)
    preview_names <- intersect(c(selected, "hint8_score"), names(data))
    DT::datatable(
      utils::head(data[, preview_names, drop = FALSE], 50),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$download_hint8_calculator <- downloadHandler(
    filename = function() {
      paste0("StatEdu_Studio_hint8_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      data <- result()
      if (is.null(data)) {
        data <- hint8_calculator_result(
          dataset_fn(),
          hint8_selected_variables(input),
          variable_choices = hint8_variable_choices(dataset_fn(), variable_info_fn()),
          profile_11111111_as_one = isTRUE(input$hint8_profile_11111111_as_one)
        )
      }
      utils::write.csv(data, file, row.names = FALSE, na = "")
    }
  )

  invisible(TRUE)
}
