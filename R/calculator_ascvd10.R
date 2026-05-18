# ASCVD 10-year risk calculator module.

ascvd10_variable_specs <- function() {
  data.frame(
    id = c("race", "sex", "age", "SMOK", "CHOL", "HDLc", "HPd", "SBP", "DM", "c_history", "c_ldlc"),
    label = c("Race", "Sex", "Age", "Current smoker", "Total cholesterol", "HDL cholesterol", "Hypertension treatment", "SBP", "Diabetes", "ASCVD history", "LDL cholesterol"),
    required = c(rep(TRUE, 10), FALSE),
    stringsAsFactors = FALSE
  )
}

ascvd10_default_coefficients <- function() {
  list(
    male = list(
      lnage = 9.362, lnage2 = 2.425, lnchol = 6.409, lnagechol = -1.430,
      lnhdlc = -3.843, lnagehdlc = 0.810, lnsbp_t = 18.589, lnagesbp_t = -4.116,
      lnsbp_u = 18.541, lnagesbp_u = -4.112, smok = 2.464, lnagesmok = -0.503,
      dm = 0.410, mean = 87.5563, s10 = 0.96427
    ),
    female = list(
      lnage = -9.519, lnage2 = 3.417, lnchol = 0.320, lnagechol = 0,
      lnhdlc = -0.476, lnagehdlc = 0, lnsbp_t = 13.402, lnagesbp_t = -2.889,
      lnsbp_u = 13.291, lnagesbp_u = -2.876, smok = 0.415, lnagesmok = 0,
      dm = 0.424, mean = 24.8813, s10 = 0.96963
    )
  )
}

ascvd10_selected_variables <- function(input) {
  specs <- ascvd10_variable_specs()
  stats::setNames(
    vapply(specs$id, function(id) as.character(input[[paste0("ascvd10_", id)]] %||% ""), character(1)),
    specs$id
  )
}

ascvd10_coefficients_for <- function(sex, race) {
  coeff <- ascvd10_default_coefficients()
  if (sex == 2 && race == 1) {
    return(list(lnage = -29.799, lnage2 = 4.884, lnchol = 13.540, lnagechol = -3.114, lnhdlc = -13.578, lnagehdlc = 3.149, lnsbp_t = 2.019, lnagesbp_t = 0, lnsbp_u = 1.957, lnagesbp_u = 0, smok = 7.574, lnagesmok = -1.665, dm = 0.661, mean = -29.18, s10 = 0.9665))
  }
  if (sex == 2 && race == 2) {
    return(list(lnage = 17.114, lnage2 = 0, lnchol = 0.940, lnagechol = 0, lnhdlc = -18.920, lnagehdlc = 4.475, lnsbp_t = 29.291, lnagesbp_t = -6.432, lnsbp_u = 27.820, lnagesbp_u = -6.087, smok = 0.691, lnagesmok = 0, dm = 0.874, mean = 86.61, s10 = 0.9533))
  }
  if (sex == 1 && race == 1) {
    return(list(lnage = 12.344, lnage2 = 0, lnchol = 11.853, lnagechol = -2.664, lnhdlc = -7.990, lnagehdlc = 1.769, lnsbp_t = 1.797, lnagesbp_t = 0, lnsbp_u = 1.764, lnagesbp_u = 0, smok = 7.837, lnagesmok = -1.795, dm = 0.658, mean = 61.18, s10 = 0.9144))
  }
  if (sex == 1 && race == 2) {
    return(list(lnage = 2.469, lnage2 = 0, lnchol = 0.302, lnagechol = 0, lnhdlc = -0.307, lnagehdlc = 0, lnsbp_t = 1.916, lnagesbp_t = 0, lnsbp_u = 1.809, lnagesbp_u = 0, smok = 0.549, lnagesmok = 0, dm = 0.645, mean = 19.54, s10 = 0.8954))
  }
  if (sex == 1 && race == 3) return(coeff$male)
  if (sex == 2 && race == 3) return(coeff$female)
  NULL
}

ascvd10_score <- function(race, sex, age, smok, chol, hdlc, hpd, sbp, dm, history, ldlc = NULL) {
  n <- length(age)
  out <- rep(NA_real_, n)
  ldlc <- ldlc %||% rep(NA_real_, n)
  values <- data.frame(race, sex, age, smok, chol, hdlc, hpd, sbp, dm, history, ldlc)
  for (index in seq_len(n)) {
    row <- values[index, , drop = FALSE]
    if (!stats::complete.cases(row[, c("race", "sex", "age", "smok", "chol", "hdlc", "hpd", "sbp", "dm", "history")])) next
    if (row$age <= 0 || row$chol <= 0 || row$hdlc <= 0 || row$sbp <= 0) next
    if (row$history == 1 || (!is.na(row$ldlc) && row$ldlc >= 190)) next
    coeff <- ascvd10_coefficients_for(row$sex, row$race)
    if (is.null(coeff)) next
    lnage <- log(row$age)
    lnchol <- log(row$chol)
    lnhdlc <- log(row$hdlc)
    lnsbp <- log(row$sbp)
    treated <- row$hpd == 1
    ind_sum <- (lnage * coeff$lnage) +
      ((lnage * lnage) * coeff$lnage2) +
      (lnchol * coeff$lnchol) +
      ((lnage * lnchol) * coeff$lnagechol) +
      (lnhdlc * coeff$lnhdlc) +
      ((lnage * lnhdlc) * coeff$lnagehdlc) +
      (lnsbp * if (treated) coeff$lnsbp_t else coeff$lnsbp_u) +
      ((lnage * lnsbp) * if (treated) coeff$lnagesbp_t else coeff$lnagesbp_u) +
      (if (row$smok == 1) coeff$smok + lnage * coeff$lnagesmok else 0) +
      (if (row$dm == 1) coeff$dm else 0)
    out[[index]] <- (1 - coeff$s10 ^ exp(ind_sum - coeff$mean)) * 100
  }
  out
}

ascvd10_result <- function(data, selected) {
  selected_ids <- names(selected)
  selected <- as.character(selected)
  specs <- ascvd10_variable_specs()
  if (is.null(selected_ids) || length(selected_ids) != length(selected) || any(!nzchar(selected_ids))) selected_ids <- specs$id
  required_ids <- specs$id[specs$required]
  required_selected <- selected[match(required_ids, selected_ids)]
  if (any(!nzchar(required_selected)) || anyDuplicated(required_selected[nzchar(required_selected)])) stop("Select different required ASCVD10 variables.", call. = FALSE)
  selected_present <- selected[nzchar(selected)]
  if (is.null(data) || !all(selected_present %in% names(data))) stop("Selected variables are not available in the loaded data.", call. = FALSE)
  source <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  values <- lapply(selected_ids, function(id) {
    variable_name <- selected[[match(id, selected_ids)]]
    if (!nzchar(variable_name)) return(rep(NA_real_, nrow(source)))
    metabolic_numeric(source[[variable_name]])
  })
  names(values) <- selected_ids
  source[["ascvd10_score"]] <- ascvd10_score(values$race, values$sex, values$age, values$SMOK, values$CHOL, values$HDLc, values$HPd, values$SBP, values$DM, values$c_history, values$c_ldlc)
  source
}

ascvd10_calculator_tab_panel <- function() {
  tabPanel(
    "ASCVD10",
    value = "calculator_ascvd10",
    div(
      class = "page-shell",
      div(class = "app-heading", h1("ASCVD10 Calculator"), div("Select variables and add ascvd10_score to the current data.", class = "app-subtitle")),
      div(
        class = "workspace-panel frequencies-workspace-panel metabolic-calculator-workspace",
        style = "min-width:980px;overflow-x:auto;",
        h3("ASCVD 10-year risk"),
        div(class = "load-message", textOutput("ascvd10_loaded_message")),
        uiOutput("ascvd10_calculator_setup"),
        div(class = "analysis-action-row metabolic-action-row", actionButton("run_ascvd10_calculator", "Calculate", class = "btn btn-primary"), downloadButton("download_ascvd10_calculator", "Download CSV", class = "btn btn-default")),
        uiOutput("ascvd10_calculator_summary"),
        DT::DTOutput("ascvd10_calculator_preview")
      )
    )
  )
}

ascvd10_reference_table <- function() {
  rows <- data.frame(
    Reference = c("Race", "Sex"),
    Coding = c("White=1; African-American=2; Other=3", "Male=1; Female=2"),
    stringsAsFactors = FALSE
  )
  tags$table(
    class = "hint8-initial-table ascvd10-reference-table",
    tags$thead(tags$tr(tags$th("Reference"), tags$th("Coding"))),
    tags$tbody(lapply(seq_len(nrow(rows)), function(index) {
      tags$tr(tags$td(rows$Reference[[index]]), tags$td(rows$Coding[[index]]))
    }))
  )
}

ascvd10_exclusion_table <- function() {
  rows <- data.frame(
    Variable = c("ASCVD history", "LDL-C"),
    Rule = c("1", ">= 190"),
    Result = c("ascvd10_score missing", "ascvd10_score missing"),
    stringsAsFactors = FALSE
  )
  tags$table(
    class = "hint8-initial-table ascvd10-reference-table ascvd10-exclusion-table",
    tags$thead(tags$tr(tags$th("Variable"), tags$th("Rule"), tags$th("Result"))),
    tags$tbody(lapply(seq_len(nrow(rows)), function(index) {
      tags$tr(tags$td(rows$Variable[[index]]), tags$td(rows$Rule[[index]]), tags$td(rows$Result[[index]]))
    }))
  )
}

ascvd10_output_table <- function() {
  tags$table(
    class = "hint8-initial-table ascvd10-reference-table ascvd10-output-table",
    tags$tbody(tags$tr(tags$td("10-year risk"), tags$td("ascvd10_score")))
  )
}

ascvd10_setup_ui <- function(file, data, variable_info, input) {
  if (is.null(file)) return(setup_empty_message("Load a data file in the Data tab before using the ASCVD10 calculator."))
  choices <- names(data %||% data.frame())
  specs <- ascvd10_variable_specs()
  available_items <- analysis_variable_items(choices, variable_info, character(0))
  variable_inputs <- lapply(seq_len(nrow(specs)), function(index) {
    id <- specs$id[[index]]
    label <- specs$label[[index]]
    if (!isTRUE(specs$required[[index]])) label <- paste0(label, " (optional)")
    selectInput(paste0("ascvd10_", id), label, choices = c("Select variable" = "", choices), selected = input[[paste0("ascvd10_", id)]] %||% "", width = "100%")
  })
  div(
    class = "frequencies-setup-grid metabolic-setup-grid",
    div(class = "analysis-transfer-column analysis-transfer-panel", analysis_field_label_tag("Variables"), analysis_transfer_listbox_input("ascvd10_available", available_items, selected = isolate(input$ascvd10_available), size = 19)),
    div(class = "analysis-transfer-controls hint8-transfer-spacer"),
    div(
      class = "analysis-transfer-column analysis-transfer-panel metabolic-target-panel ascvd10-target-panel",
      analysis_field_label_tag("ASCVD10 variables"),
      div(class = "ascvd10-two-column-row", variable_inputs[[1]], variable_inputs[[2]]),
      div(class = "metabolic-variable-input-grid", variable_inputs[-c(1, 2)])
    ),
    div(
      class = "analysis-options-column analysis-options-panel metabolic-reference-panel ascvd10-reference-panel",
      div(class = "analysis-option-title", "Reference"),
      ascvd10_reference_table(),
      div(class = "analysis-option-title ascvd10-rule-title", "Exclusion rules"),
      ascvd10_exclusion_table(),
      div(class = "analysis-option-title ascvd10-output-title", "Output"),
      ascvd10_output_table()
    )
  )
}

register_ascvd10_calculator_handlers <- function(input, output, session, dataset_fn, current_data_file_fn, variable_info_fn, add_calculated_variable_fn) {
  output$ascvd10_loaded_message <- renderText({
    file <- current_data_file_fn()
    metabolic_loaded_message_text(file, if (is.null(file)) NULL else dataset_fn())
  })
  output$ascvd10_calculator_setup <- renderUI({
    file <- current_data_file_fn()
    data <- if (is.null(file)) NULL else dataset_fn()
    ascvd10_setup_ui(file, data, if (is.null(file)) NULL else variable_info_fn(), input)
  })
  observeEvent(input$ascvd10_available, {
    picked <- as.character(input$ascvd10_available %||% "")
    if (!nzchar(picked)) return()
    selected <- ascvd10_selected_variables(input)
    if (picked %in% selected) return()
    empty_index <- which(!nzchar(selected))[1]
    if (!is.na(empty_index)) updateSelectInput(session, paste0("ascvd10_", names(selected)[[empty_index]]), selected = picked)
  }, ignoreInit = TRUE)
  result <- eventReactive(input$run_ascvd10_calculator, {
    if (is.null(current_data_file_fn())) {
      showNotification("Load a data file before calculating ASCVD10.", type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch({
      result_data <- ascvd10_result(dataset_fn(), ascvd10_selected_variables(input))
      add_calculated_variable_fn("ascvd10_score", result_data[["ascvd10_score"]], var_label = "ASCVD 10-year risk", measurement = "continuous")
      showNotification("ascvd10_score was added to the current data.", type = "message", duration = 5)
      result_data
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "warning", duration = 6)
      NULL
    })
  }, ignoreInit = TRUE)
  output$ascvd10_calculator_summary <- renderUI({
    data <- result()
    if (is.null(data)) return(div(class = "empty-message", div("Select variables and click Calculate.")))
    div(class = "empty-message", div(sprintf("Calculated ascvd10_score for %s rows. The variable is available in analysis menus.", nrow(data))))
  })
  output$ascvd10_calculator_preview <- DT::renderDT({
    data <- result()
    if (is.null(data)) return(DT::datatable(data.frame(Message = "No ASCVD10 result yet.", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE)))
    selected <- ascvd10_selected_variables(input)
    preview_names <- intersect(c(unname(selected[nzchar(selected)]), "ascvd10_score"), names(data))
    DT::datatable(utils::head(data[, preview_names, drop = FALSE], 50), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download_ascvd10_calculator <- downloadHandler(
    filename = function() paste0("easyflow_ascvd10_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      data <- result()
      if (is.null(data)) data <- ascvd10_result(dataset_fn(), ascvd10_selected_variables(input))
      utils::write.csv(data, file, row.names = FALSE, na = "")
    }
  )
  invisible(TRUE)
}
