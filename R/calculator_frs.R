# Framingham risk score calculator module.

frs_variable_specs <- function() {
  data.frame(
    id = c("sex", "age", "Smok", "HDLc", "chol", "HPd", "SBP", "DM"),
    label = c("Sex", "Age", "Current smoker", "HDL cholesterol", "Total cholesterol", "Hypertension treatment", "SBP", "Diabetes"),
    stringsAsFactors = FALSE
  )
}

frs_selected_variables <- function(input) {
  specs <- frs_variable_specs()
  stats::setNames(
    vapply(specs$id, function(id) as.character(input[[paste0("frs_", id)]] %||% ""), character(1)),
    specs$id
  )
}

frs_lipid_unit <- function(input) {
  unit <- as.character(input$frs_lipid_unit %||% "mg_dl")
  if (identical(unit, "mmol_l")) "mmol_l" else "mg_dl"
}

frs_convert_lipids <- function(values, lipid_unit = "mg_dl") {
  if (identical(lipid_unit, "mmol_l")) {
    values$HDLc <- round(values$HDLc * 38.67, 6)
    values$chol <- round(values$chol * 38.67, 6)
  }
  values
}

frs_bucket_score <- function(x, breaks, scores) {
  out <- rep(NA_real_, length(x))
  for (index in seq_along(scores)) {
    range <- breaks[[index]]
    lower_ok <- if (is.null(range$lower)) rep(TRUE, length(x)) else x >= range$lower
    upper_ok <- if (is.null(range$upper)) rep(TRUE, length(x)) else x < range$upper
    matched <- !is.na(x) & !is.na(lower_ok) & !is.na(upper_ok) & lower_ok & upper_ok
    out[matched] <- scores[[index]]
  }
  out
}

frs_age_score <- function(age, sex) {
  male <- list(
    breaks = list(
      list(lower = 30, upper = 35), list(lower = 35, upper = 40), list(lower = 40, upper = 45),
      list(lower = 45, upper = 50), list(lower = 50, upper = 55), list(lower = 55, upper = 60),
      list(lower = 60, upper = 65), list(lower = 65, upper = 70), list(lower = 70, upper = 75),
      list(lower = 75, upper = NULL)
    ),
    scores = c(0, 2, 5, 6, 8, 10, 11, 12, 14, 15)
  )
  female <- list(
    breaks = male$breaks,
    scores = c(0, 2, 4, 5, 7, 8, 9, 10, 11, 12)
  )
  out <- rep(NA_real_, length(age))
  out[sex == 1] <- frs_bucket_score(age[sex == 1], male$breaks, male$scores)
  out[sex == 2] <- frs_bucket_score(age[sex == 2], female$breaks, female$scores)
  out
}

frs_hdl_score <- function(hdlc) {
  frs_bucket_score(
    hdlc,
    list(
      list(lower = 60, upper = NULL),
      list(lower = 50, upper = 60),
      list(lower = 45, upper = 50),
      list(lower = 35, upper = 45),
      list(lower = NULL, upper = 35)
    ),
    c(-2, -1, 0, 1, 2)
  )
}

frs_chol_score <- function(chol, sex) {
  male <- frs_bucket_score(
    chol,
    list(list(lower = 280, upper = NULL), list(lower = 240, upper = 280), list(lower = 200, upper = 240), list(lower = 160, upper = 200), list(lower = NULL, upper = 160)),
    c(4, 3, 2, 1, 0)
  )
  female <- frs_bucket_score(
    chol,
    list(list(lower = 280, upper = NULL), list(lower = 240, upper = 280), list(lower = 200, upper = 240), list(lower = 160, upper = 200), list(lower = NULL, upper = 160)),
    c(5, 4, 3, 1, 0)
  )
  ifelse(sex == 1, male, ifelse(sex == 2, female, NA_real_))
}

frs_bp_score <- function(sbp, hpd, sex) {
  out <- rep(NA_real_, length(sbp))
  male_untreated <- frs_bucket_score(sbp, list(list(lower = NULL, upper = 120), list(lower = 120, upper = 130), list(lower = 130, upper = 140), list(lower = 140, upper = 150), list(lower = 150, upper = 160), list(lower = 160, upper = NULL)), c(-2, 0, 1, 2, 2, 3))
  male_treated <- frs_bucket_score(sbp, list(list(lower = NULL, upper = 120), list(lower = 120, upper = 130), list(lower = 130, upper = 140), list(lower = 140, upper = 150), list(lower = 150, upper = 160), list(lower = 160, upper = NULL)), c(0, 2, 3, 4, 4, 5))
  female_untreated <- frs_bucket_score(sbp, list(list(lower = NULL, upper = 120), list(lower = 120, upper = 130), list(lower = 130, upper = 140), list(lower = 140, upper = 150), list(lower = 150, upper = 160), list(lower = 160, upper = NULL)), c(-3, 0, 1, 2, 4, 5))
  female_treated <- frs_bucket_score(sbp, list(list(lower = NULL, upper = 120), list(lower = 120, upper = 130), list(lower = 130, upper = 140), list(lower = 140, upper = 150), list(lower = 150, upper = 160), list(lower = 160, upper = NULL)), c(-1, 2, 3, 5, 6, 7))
  out[sex == 1 & hpd != 1] <- male_untreated[sex == 1 & hpd != 1]
  out[sex == 1 & hpd == 1] <- male_treated[sex == 1 & hpd == 1]
  out[sex == 2 & hpd != 1] <- female_untreated[sex == 2 & hpd != 1]
  out[sex == 2 & hpd == 1] <- female_treated[sex == 2 & hpd == 1]
  out
}

frs_cvd10 <- function(score, sex) {
  male_values <- c(`-3` = 0.9, `-2` = 1.1, `-1` = 1.4, `0` = 1.6, `1` = 1.9, `2` = 2.3, `3` = 2.8, `4` = 3.3, `5` = 3.9, `6` = 4.7, `7` = 5.6, `8` = 6.7, `9` = 7.9, `10` = 9.4, `11` = 11.2, `12` = 13.2, `13` = 15.6, `14` = 18.4, `15` = 21.6, `16` = 25.3, `17` = 29.4)
  female_values <- c(`-2` = 0.9, `-1` = 1.0, `0` = 1.2, `1` = 1.5, `2` = 1.7, `3` = 2.0, `4` = 2.4, `5` = 2.6, `6` = 3.3, `7` = 3.9, `8` = 4.5, `9` = 5.3, `10` = 6.3, `11` = 7.3, `12` = 8.6, `13` = 10.0, `14` = 11.7, `15` = 13.7, `16` = 15.9, `17` = 18.5, `18` = 21.5, `19` = 24.8, `20` = 28.5)
  out <- rep(NA_real_, length(score))
  male_score <- pmax(score[sex == 1], -3)
  out[sex == 1] <- ifelse(male_score >= 18, 31, unname(male_values[as.character(male_score)]))
  female_score <- pmax(score[sex == 2], -2)
  out[sex == 2] <- ifelse(female_score >= 21, 31, unname(female_values[as.character(female_score)]))
  out
}

frs_heart_age <- function(score, sex) {
  male_values <- c(`0` = 30, `1` = 32, `2` = 34, `3` = 36, `4` = 38, `5` = 40, `6` = 42, `7` = 45, `8` = 48, `9` = 51, `10` = 54, `11` = 57, `12` = 60, `13` = 64, `14` = 68, `15` = 72, `16` = 76)
  female_values <- c(`1` = 31, `2` = 34, `3` = 36, `4` = 39, `5` = 42, `6` = 45, `7` = 48, `8` = 51, `9` = 55, `10` = 59, `11` = 64, `12` = 68, `13` = 73, `14` = 79)
  out <- rep(NA_real_, length(score))
  out[sex == 1] <- ifelse(score[sex == 1] < 0, 29, ifelse(score[sex == 1] >= 17, 81, unname(male_values[as.character(score[sex == 1])])))
  out[sex == 2] <- ifelse(score[sex == 2] < 1, 29, ifelse(score[sex == 2] >= 15, 81, unname(female_values[as.character(score[sex == 2])])))
  out
}

frs_result <- function(data, selected, lipid_unit = "mg_dl") {
  selected_ids <- names(selected)
  selected <- as.character(selected)
  if (is.null(selected_ids) || length(selected_ids) != length(selected) || any(!nzchar(selected_ids))) selected_ids <- frs_variable_specs()$id
  if (any(!nzchar(selected)) || anyDuplicated(selected)) stop("Select 8 different FRS variables.", call. = FALSE)
  if (is.null(data) || !all(selected %in% names(data))) stop("Selected variables are not available in the loaded data.", call. = FALSE)
  source <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  values <- lapply(selected, function(name) metabolic_numeric(source[[name]]))
  names(values) <- selected_ids
  values <- frs_convert_lipids(values, lipid_unit)
  sex <- values$sex
  risk_parts <- data.frame(
    age = frs_age_score(values$age, sex),
    hdl = frs_hdl_score(values$HDLc),
    chol = frs_chol_score(values$chol, sex),
    bp = frs_bp_score(values$SBP, values$HPd, sex),
    smok = ifelse(values$Smok == 1 & sex == 1, 4, ifelse(values$Smok == 1 & sex == 2, 3, ifelse(values$Smok == 0, 0, NA_real_))),
    dm = ifelse(values$DM == 0, 0, ifelse(values$DM == 1 & sex == 1, 3, ifelse(values$DM == 1 & sex == 2, 4, NA_real_)))
  )
  score <- rowSums(risk_parts)
  score[!stats::complete.cases(risk_parts)] <- NA_real_
  cvd10 <- frs_cvd10(score, sex)
  g_cvd10 <- ifelse(is.na(cvd10), NA_real_, ifelse(cvd10 < 10, 1, ifelse(cvd10 <= 20, 2, 3)))
  result <- source
  result[["frs_score"]] <- score
  result[["frs_cvd10"]] <- cvd10
  result[["frs_cvd10_group"]] <- g_cvd10
  result[["frs_heart_age"]] <- frs_heart_age(score, sex)
  result
}

frs_calculator_tab_panel <- function() {
  tabPanel(
    "FRS",
    value = "calculator_frs",
    div(
      class = "page-shell",
      div(class = "app-heading", h1("Framingham Risk Score Calculator"), div("Select variables and add frs_score, frs_cvd10, frs_cvd10_group, and frs_heart_age to the current data.", class = "app-subtitle")),
      div(
        class = "workspace-panel frequencies-workspace-panel metabolic-calculator-workspace",
        style = "min-width:980px;overflow-x:auto;",
        h3("Framingham risk score"),
        div(class = "load-message", textOutput("frs_loaded_message")),
        uiOutput("frs_calculator_setup"),
        div(
          class = "analysis-action-row calculator-action-row",
          div(
            class = "calculator-action-row-controls",
            actionButton("run_frs_calculator", "Calculate", class = "btn btn-primary"),
            downloadButton("download_frs_calculator", "Download CSV", class = "btn btn-default")
          )
        ),
        uiOutput("frs_calculator_summary"),
        DT::DTOutput("frs_calculator_preview")
      )
    )
  )
}

frs_setup_ui <- function(file, data, variable_info, input) {
  if (is.null(file)) return(setup_empty_message("Load a data file in the Data tab before using the FRS calculator."))
  choices <- names(data %||% data.frame())
  specs <- frs_variable_specs()
  available_items <- analysis_variable_items(choices, variable_info, character(0))
  variable_inputs <- lapply(seq_len(nrow(specs)), function(index) {
    id <- specs$id[[index]]
    selectInput(paste0("frs_", id), specs$label[[index]], choices = c("Select variable" = "", choices), selected = isolate(input[[paste0("frs_", id)]]) %||% "", width = "100%")
  })
  div(
    class = "frequencies-setup-grid metabolic-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("frs_available", available_items, selected = isolate(input$frs_available), size = 17)
    ),
    div(class = "analysis-transfer-controls hint8-transfer-spacer"),
      div(class = "analysis-transfer-column analysis-transfer-panel metabolic-target-panel frs-target-panel", analysis_field_label_tag("FRS variables"), div(class = "metabolic-variable-input-grid", variable_inputs)),
    div(
      class = "analysis-options-column analysis-options-panel metabolic-reference-panel frs-reference-panel",
      div(class = "analysis-option-title", "Units"),
      div(
        class = "frs-unit-control",
        selectInput(
          "frs_lipid_unit",
          "Lipid unit",
          choices = c("mg/dL" = "mg_dl", "mmol/L" = "mmol_l"),
          selected = isolate(frs_lipid_unit(input)),
          width = "100%"
        )
      ),
      div(class = "analysis-option-title", "Coding"),
      tags$table(
        class = "hint8-initial-table",
        tags$tbody(
          tags$tr(tags$td("Sex"), tags$td("Male = 1, Female = 2")),
          tags$tr(tags$td("Current smoker"), tags$td("Yes = 1, No = 0")),
          tags$tr(tags$td("Hypertension treatment"), tags$td("Yes = 1, No = 0")),
          tags$tr(tags$td("Diabetes"), tags$td("Yes = 1, No = 0")),
          tags$tr(tags$td("Lipids"), tags$td(if (identical(frs_lipid_unit(input), "mmol_l")) "mmol/L -> mg/dL" else "mg/dL")),
          tags$tr(tags$td("SBP"), tags$td("mmHg"))
        )
      ),
      div(class = "analysis-option-title frs-output-title", "Outputs"),
      tags$table(
        class = "hint8-initial-table",
        tags$tbody(
          tags$tr(tags$td("Score"), tags$td("frs_score")),
          tags$tr(tags$td("10-year risk"), tags$td("frs_cvd10")),
          tags$tr(tags$td("Risk group"), tags$td("frs_cvd10_group")),
          tags$tr(tags$td("Heart age"), tags$td("frs_heart_age"))
        )
      )
    )
  )
}

register_frs_calculator_handlers <- function(input, output, session, dataset_fn, current_data_file_fn, variable_info_fn, add_calculated_variable_fn) {
  output$frs_loaded_message <- renderText({
    file <- current_data_file_fn()
    metabolic_loaded_message_text(file, if (is.null(file)) NULL else dataset_fn())
  })
  output$frs_calculator_setup <- renderUI({
    file <- current_data_file_fn()
    data <- if (is.null(file)) NULL else dataset_fn()
    frs_setup_ui(file, data, if (is.null(file)) NULL else variable_info_fn(), input)
  })
  observeEvent(input$frs_available, {
    picked <- utils::tail(as.character(input$frs_available %||% ""), 1)
    picked <- if (length(picked) > 0) picked[[1]] else ""
    if (!nzchar(picked)) return()
    selected <- frs_selected_variables(input)
    if (picked %in% selected) return()
    empty_index <- which(!nzchar(selected))[1]
    if (!is.na(empty_index)) updateSelectInput(session, paste0("frs_", names(selected)[[empty_index]]), selected = picked)
  }, ignoreInit = TRUE)
  result <- eventReactive(input$run_frs_calculator, {
    if (is.null(current_data_file_fn())) {
      showNotification("Load a data file before calculating FRS.", type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch({
      result_data <- frs_result(dataset_fn(), frs_selected_variables(input), lipid_unit = frs_lipid_unit(input))
      add_calculated_variable_fn("frs_score", result_data[["frs_score"]], var_label = "Framingham Risk Score", measurement = "continuous")
      add_calculated_variable_fn("frs_cvd10", result_data[["frs_cvd10"]], var_label = "Cardiovascular disease 10 year risk", measurement = "continuous")
      add_calculated_variable_fn("frs_cvd10_group", result_data[["frs_cvd10_group"]], var_label = "CVD 10 year risk group", measurement = "category")
      add_calculated_variable_fn("frs_heart_age", result_data[["frs_heart_age"]], var_label = "Heart age", measurement = "continuous")
      showNotification("frs_score, frs_cvd10, frs_cvd10_group, and frs_heart_age were added to the current data.", type = "message", duration = 5)
      result_data
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "warning", duration = 6)
      NULL
    })
  }, ignoreInit = TRUE)
  output$frs_calculator_summary <- renderUI({
    data <- result()
    if (is.null(data)) return(NULL)
    div(class = "empty-message", div(sprintf("Calculated FRS outputs for %s rows. The variables are available in analysis menus.", nrow(data))))
  })
  output$frs_calculator_preview <- DT::renderDT({
    data <- result()
    if (is.null(data)) return(NULL)
    selected <- frs_selected_variables(input)
    preview_names <- intersect(c(unname(selected), "frs_score", "frs_cvd10", "frs_cvd10_group", "frs_heart_age"), names(data))
    DT::datatable(utils::head(data[, preview_names, drop = FALSE], 50), rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download_frs_calculator <- downloadHandler(
    filename = function() paste0("StatEdu_Studio_frs_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      data <- result()
      if (is.null(data)) data <- frs_result(dataset_fn(), frs_selected_variables(input), lipid_unit = frs_lipid_unit(input))
      utils::write.csv(data, file, row.names = FALSE, na = "")
    }
  )
  invisible(TRUE)
}
