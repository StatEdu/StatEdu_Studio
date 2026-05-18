# Metabolic syndrome severity score calculator module.

metabolic_severity_variable_specs <- function() {
  data.frame(
    id = c("sex", "age", "glu", "SBP", "wc", "HDLc", "TG"),
    label = c(
      "SEX (Male 1, Female 2)",
      "Age",
      "Glucose",
      "SBP",
      "Waist Circumference",
      "HDL cholesterol",
      "Triglycerides"
    ),
    stringsAsFactors = FALSE
  )
}

metabolic_severity_selected_variables <- function(input) {
  specs <- metabolic_severity_variable_specs()
  stats::setNames(
    vapply(specs$id, function(id) as.character(input[[paste0("mbss_", id)]] %||% ""), character(1)),
    specs$id
  )
}

metabolic_severity_result <- function(data, selected) {
  selected_ids <- names(selected)
  selected <- as.character(selected)
  if (is.null(selected_ids) || length(selected_ids) != length(selected) || any(!nzchar(selected_ids))) {
    selected_ids <- metabolic_severity_variable_specs()$id
  }
  if (any(!nzchar(selected)) || anyDuplicated(selected)) {
    stop("Select 7 different metabolic severity variables.", call. = FALSE)
  }
  if (is.null(data) || !all(selected %in% names(data))) {
    stop("Selected variables are not available in the loaded data.", call. = FALSE)
  }

  source <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  values <- lapply(selected, function(name) metabolic_numeric(source[[name]]))
  names(values) <- selected_ids

  sex <- values$sex
  age <- values$age
  glu <- values$glu
  sbp <- values$SBP
  wc <- values$wc
  hdlc <- values$HDLc
  tg <- values$TG
  ln_tg <- log(tg)
  age_group <- ifelse(age >= 20 & age <= 39, 1, ifelse(age >= 40 & age <= 59, 2, NA_real_))

  mbss_overall <- rep(NA_real_, nrow(source))
  mbss_overall[sex == 1 & age >= 20 & age < 60] <-
    -8.5245 + 0.0156 * glu[sex == 1 & age >= 20 & age < 60] +
    0.0089 * sbp[sex == 1 & age >= 20 & age < 60] +
    0.0371 * wc[sex == 1 & age >= 20 & age < 60] -
    0.0182 * hdlc[sex == 1 & age >= 20 & age < 60] +
    0.7913 * ln_tg[sex == 1 & age >= 20 & age < 60]
  mbss_overall[sex == 2 & age >= 20 & age < 60] <-
    -8.4480 + 0.0223 * glu[sex == 2 & age >= 20 & age < 60] +
    0.0115 * sbp[sex == 2 & age >= 20 & age < 60] +
    0.0403 * wc[sex == 2 & age >= 20 & age < 60] -
    0.0155 * hdlc[sex == 2 & age >= 20 & age < 60] +
    0.6704 * ln_tg[sex == 2 & age >= 20 & age < 60]

  mbss <- rep(NA_real_, nrow(source))
  pick <- sex == 1 & age_group == 1
  mbss[pick] <- -8.7125 + 0.0176 * glu[pick] + 0.0117 * sbp[pick] + 0.0386 * wc[pick] - 0.0172 * hdlc[pick] + 0.7168 * ln_tg[pick]
  pick <- sex == 1 & age_group == 2
  mbss[pick] <- -8.2939 + 0.0126 * glu[pick] + 0.0063 * sbp[pick] + 0.0382 * wc[pick] - 0.0210 * hdlc[pick] + 0.8432 * ln_tg[pick]
  pick <- sex == 2 & age_group == 1
  mbss[pick] <- -8.7795 + 0.0248 * glu[pick] + 0.0130 * sbp[pick] + 0.0472 * wc[pick] - 0.0159 * hdlc[pick] + 0.6131 * ln_tg[pick]
  pick <- sex == 2 & age_group == 2
  mbss[pick] <- -7.5210 + 0.0156 * glu[pick] + 0.0073 * sbp[pick] + 0.0292 * wc[pick] - 0.0207 * hdlc[pick] + 0.9065 * ln_tg[pick]

  result <- source
  result[["MBSS_overall"]] <- mbss_overall
  result[["MBSS"]] <- mbss
  result
}

metabolic_severity_calculator_tab_panel <- function() {
  tabPanel(
    "Metabolic Severity",
    value = "calculator_metabolic_severity",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Metabolic Severity Score Calculator"),
        div("Select variables and add MBSS_overall and MBSS to the current data.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel metabolic-calculator-workspace",
        style = "min-width:980px;overflow-x:auto;",
        h3("Metabolic severity score"),
        div(class = "load-message", textOutput("mbss_loaded_message")),
        uiOutput("mbss_calculator_setup"),
        div(
          class = "analysis-action-row metabolic-action-row",
          actionButton("run_mbss_calculator", "Calculate", class = "btn btn-primary"),
          downloadButton("download_mbss_calculator", "Download CSV", class = "btn btn-default")
        ),
        uiOutput("mbss_calculator_summary"),
        DT::DTOutput("mbss_calculator_preview")
      )
    )
  )
}

metabolic_severity_setup_ui <- function(file, data, variable_info, input) {
  if (is.null(file)) {
    return(setup_empty_message("Load a data file in the Data tab before using the metabolic severity calculator."))
  }
  choices <- names(data %||% data.frame())
  specs <- metabolic_severity_variable_specs()
  available_items <- analysis_variable_items(choices, variable_info, character(0))
  variable_inputs <- lapply(seq_len(nrow(specs)), function(index) {
    id <- specs$id[[index]]
    selectInput(
      paste0("mbss_", id),
      specs$label[[index]],
      choices = c("Select variable" = "", choices),
      selected = input[[paste0("mbss_", id)]] %||% "",
      width = "100%"
    )
  })

  div(
    class = "frequencies-setup-grid metabolic-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("mbss_available", available_items, selected = isolate(input$mbss_available), size = 19)
    ),
    div(class = "analysis-transfer-controls hint8-transfer-spacer"),
    div(
      class = "analysis-transfer-column analysis-transfer-panel metabolic-target-panel",
      analysis_field_label_tag("Severity score variables"),
      div(class = "metabolic-variable-input-grid", variable_inputs)
    ),
    div(
      class = "analysis-options-column analysis-options-panel metabolic-reference-panel",
      div(class = "analysis-option-title", "Formula"),
      div(class = "step-summary", div("Outputs: MBSS_overall, MBSS", class = "step-summary-title")),
      tags$table(
        class = "hint8-initial-table",
        tags$tbody(
          tags$tr(tags$td("Age range"), tags$td("20 to 59")),
          tags$tr(tags$td("Inputs"), tags$td("sex, age, glucose, SBP, WC, HDL-C, TG")),
          tags$tr(tags$td("TG transform"), tags$td("ln(TG)"))
        )
      )
    )
  )
}

register_metabolic_severity_calculator_handlers <- function(input, output, session, dataset_fn, current_data_file_fn, variable_info_fn, add_calculated_variable_fn) {
  output$mbss_loaded_message <- renderText({
    file <- current_data_file_fn()
    metabolic_loaded_message_text(file, if (is.null(file)) NULL else dataset_fn())
  })

  output$mbss_calculator_setup <- renderUI({
    file <- current_data_file_fn()
    data <- if (is.null(file)) NULL else dataset_fn()
    variable_info <- if (is.null(file)) NULL else variable_info_fn()
    metabolic_severity_setup_ui(file, data, variable_info, input)
  })

  observeEvent(input$mbss_available, {
    picked <- as.character(input$mbss_available %||% "")
    if (!nzchar(picked)) return()
    selected <- metabolic_severity_selected_variables(input)
    if (picked %in% selected) return()
    empty_index <- which(!nzchar(selected))[1]
    if (!is.na(empty_index)) {
      updateSelectInput(session, paste0("mbss_", names(selected)[[empty_index]]), selected = picked)
    }
  }, ignoreInit = TRUE)

  result <- eventReactive(input$run_mbss_calculator, {
    if (is.null(current_data_file_fn())) {
      showNotification("Load a data file before calculating metabolic severity.", type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch({
      result_data <- metabolic_severity_result(dataset_fn(), metabolic_severity_selected_variables(input))
      add_calculated_variable_fn("MBSS_overall", result_data[["MBSS_overall"]], var_label = "Metabolic Syndrome severity scores overall", measurement = "continuous")
      add_calculated_variable_fn("MBSS", result_data[["MBSS"]], var_label = "Metabolic Syndrome severity scores", measurement = "continuous")
      showNotification("MBSS_overall and MBSS were added to the current data.", type = "message", duration = 5)
      result_data
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "warning", duration = 6)
      NULL
    })
  }, ignoreInit = TRUE)

  output$mbss_calculator_summary <- renderUI({
    data <- result()
    if (is.null(data)) return(div(class = "empty-message", div("Select variables and click Calculate.")))
    div(class = "empty-message", div(sprintf("Calculated MBSS_overall and MBSS for %s rows. The variables are available in analysis menus.", nrow(data))))
  })

  output$mbss_calculator_preview <- DT::renderDT({
    data <- result()
    if (is.null(data)) {
      return(DT::datatable(data.frame(Message = "No metabolic severity result yet.", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    }
    selected <- metabolic_severity_selected_variables(input)
    preview_names <- intersect(c(unname(selected), "MBSS_overall", "MBSS"), names(data))
    DT::datatable(utils::head(data[, preview_names, drop = FALSE], 50), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_mbss_calculator <- downloadHandler(
    filename = function() paste0("easyflow_metabolic_severity_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      data <- result()
      if (is.null(data)) data <- metabolic_severity_result(dataset_fn(), metabolic_severity_selected_variables(input))
      utils::write.csv(data, file, row.names = FALSE, na = "")
    }
  )

  invisible(TRUE)
}
