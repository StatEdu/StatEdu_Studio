# Metabolic syndrome calculator module.

metabolic_variable_specs <- function() {
  data.frame(
    id = c("sex", "wc", "glu", "DMd", "SBP", "DBP", "HPd", "HDLc", "TG"),
    label = c("Sex", "Waist circumference", "Glucose", "Diabetes treatment", "SBP", "DBP", "Hypertension treatment", "HDL cholesterol", "Triglycerides"),
    stringsAsFactors = FALSE
  )
}

metabolic_default_references <- function() {
  metabolic_reference_sets()[["korea_asian"]]$refs
}

metabolic_reference_sets <- function() {
  list(
    korea_asian = list(label = "Korea / Asian", diagnosis = "count3", refs = list(wc_m = 90, wc_f = 80, glu = 100, sbp = 130, dbp = 85, hdlc_m = 40, hdlc_f = 50, tg = 150)),
    japan = list(label = "Japan", diagnosis = "japan", refs = list(wc_m = 85, wc_f = 90, glu = 110, sbp = 130, dbp = 85, hdlc_m = 40, hdlc_f = 40, tg = 150)),
    ncep_atp3 = list(label = "NCEP ATP III", diagnosis = "count3", refs = list(wc_m = 102, wc_f = 88, glu = 100, sbp = 130, dbp = 85, hdlc_m = 40, hdlc_f = 50, tg = 150)),
    idf_europid = list(label = "IDF Europid", diagnosis = "count3", refs = list(wc_m = 94, wc_f = 80, glu = 100, sbp = 130, dbp = 85, hdlc_m = 40, hdlc_f = 50, tg = 150)),
    custom = list(label = "Custom", diagnosis = "count3", refs = list(
      wc_m = 90,
      wc_f = 80,
      glu = 100,
      sbp = 130,
      dbp = 85,
      hdlc_m = 40,
      hdlc_f = 50,
      tg = 150
    ))
  )
}

metabolic_reference_set_choices <- function() {
  sets <- metabolic_reference_sets()
  stats::setNames(names(sets), vapply(sets, function(set) set$label, character(1)))
}

metabolic_reference_set_id <- function(input) {
  value <- as.character(input$metabolic_reference_set %||% "korea_asian")
  if (value %in% names(metabolic_reference_sets())) value else "korea_asian"
}

metabolic_reference_set_label <- function(input) {
  metabolic_reference_sets()[[metabolic_reference_set_id(input)]]$label
}

metabolic_reference_set_defaults <- function(input) {
  metabolic_reference_sets()[[metabolic_reference_set_id(input)]]$refs
}

metabolic_diagnosis_method <- function(input) {
  metabolic_reference_sets()[[metabolic_reference_set_id(input)]]$diagnosis %||% "count3"
}

metabolic_legacy_default_references <- function() {
  list(
    wc_m = 90,
    wc_f = 80,
    glu = 100,
    sbp = 130,
    dbp = 85,
    hdlc_m = 40,
    hdlc_f = 50,
    tg = 150
  )
}

metabolic_reference_value <- function(input, id) {
  defaults <- metabolic_reference_set_defaults(input)
  value <- suppressWarnings(as.numeric(input[[paste0("metabolic_ref_", id)]] %||% defaults[[id]]))
  if (length(value) == 0 || is.na(value)) {
    value <- defaults[[id]]
  }
  value
}

metabolic_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

metabolic_selected_variables <- function(input) {
  specs <- metabolic_variable_specs()
  stats::setNames(
    vapply(specs$id, function(id) as.character(input[[paste0("metabolic_", id)]] %||% ""), character(1)),
    specs$id
  )
}

metabolic_reference_inputs <- function(input) {
  ids <- names(metabolic_default_references())
  if (!identical(metabolic_reference_set_id(input), "custom")) {
    defaults <- metabolic_reference_set_defaults(input)
    return(stats::setNames(vapply(ids, function(id) as.numeric(defaults[[id]]), numeric(1)), ids))
  }
  stats::setNames(vapply(ids, function(id) metabolic_reference_value(input, id), numeric(1)), ids)
}

metabolic_unit_inputs <- function(input) {
  list(
    wc = if (identical(as.character(input$metabolic_wc_unit %||% "cm"), "inch")) "inch" else "cm",
    lipid = if (identical(as.character(input$metabolic_lipid_unit %||% "mg_dl"), "mmol_l")) "mmol_l" else "mg_dl",
    glucose = if (identical(as.character(input$metabolic_glucose_unit %||% "mg_dl"), "mmol_l")) "mmol_l" else "mg_dl"
  )
}

metabolic_convert_units <- function(values, units) {
  if (identical(units$wc, "inch")) {
    values$wc <- round(values$wc * 2.54, 6)
  }
  if (identical(units$glucose, "mmol_l")) {
    values$glu <- round(values$glu * 18.0182, 6)
  }
  if (identical(units$lipid, "mmol_l")) {
    values$HDLc <- round(values$HDLc * 38.67, 6)
    values$TG <- round(values$TG * 88.57, 6)
  }
  values
}

metabolic_result <- function(data, selected, refs, units = list(wc = "cm", lipid = "mg_dl", glucose = "mg_dl"), diagnosis = "count3") {
  selected_ids <- names(selected)
  selected <- as.character(selected)
  if (is.null(selected_ids) || length(selected_ids) != length(selected) || any(!nzchar(selected_ids))) {
    selected_ids <- metabolic_variable_specs()$id
  }
  if (any(!nzchar(selected)) || anyDuplicated(selected)) {
    stop("Select 9 different metabolic variables.", call. = FALSE)
  }
  if (is.null(data) || !all(selected %in% names(data))) {
    stop("Selected variables are not available in the loaded data.", call. = FALSE)
  }

  source <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  values <- lapply(selected, function(name) metabolic_numeric(source[[name]]))
  names(values) <- selected_ids
  values <- metabolic_convert_units(values, units)

  sex <- values$sex
  wc <- values$wc
  glu <- values$glu
  dmd <- values$DMd
  sbp <- values$SBP
  dbp <- values$DBP
  hpd <- values$HPd
  hdlc <- values$HDLc
  tg <- values$TG

  mb_wc <- ifelse(sex == 1 & wc >= refs[["wc_m"]], 1,
    ifelse(sex == 1 & wc < refs[["wc_m"]], 0,
      ifelse(sex == 2 & wc >= refs[["wc_f"]], 1,
        ifelse(sex == 2 & wc < refs[["wc_f"]], 0, NA_real_)
      )
    )
  )
  mb_dm <- ifelse(glu >= refs[["glu"]] | dmd == 1, 1,
    ifelse(glu < refs[["glu"]] & dmd != 1, 0, NA_real_)
  )
  mb_hp <- ifelse(sbp >= refs[["sbp"]] | dbp >= refs[["dbp"]] | hpd == 1, 1,
    ifelse(sbp < refs[["sbp"]] & dbp < refs[["dbp"]] & hpd != 1, 0, NA_real_)
  )
  mb_hdlc <- ifelse(sex == 1 & hdlc < refs[["hdlc_m"]], 1,
    ifelse(sex == 1 & hdlc >= refs[["hdlc_m"]], 0,
      ifelse(sex == 2 & hdlc < refs[["hdlc_f"]], 1,
        ifelse(sex == 2 & hdlc >= refs[["hdlc_f"]], 0, NA_real_)
      )
    )
  )
  mb_tg <- ifelse(tg >= refs[["tg"]], 1, ifelse(tg < refs[["tg"]], 0, NA_real_))

  components <- data.frame(mb_wc, mb_dm, mb_hp, mb_hdlc, mb_tg)
  mb_5 <- rowSums(components, na.rm = TRUE)
  mb_5[rowSums(!is.na(components)) == 0] <- NA_real_
  if (identical(diagnosis, "japan")) {
    dyslipid <- ifelse(mb_hdlc == 1 | mb_tg == 1, 1, ifelse(mb_hdlc == 0 & mb_tg == 0, 0, NA_real_))
    risk_groups <- data.frame(mb_dm, mb_hp, dyslipid)
    risk_count <- rowSums(risk_groups, na.rm = TRUE)
    risk_count[rowSums(!is.na(risk_groups)) == 0] <- NA_real_
    g_mb <- ifelse(is.na(mb_wc) | is.na(risk_count), NA_real_, ifelse(mb_wc == 1 & risk_count >= 2, 1, 0))
  } else {
    g_mb <- ifelse(is.na(mb_5), NA_real_, ifelse(mb_5 >= 3, 1, 0))
  }

  result <- source
  result[["metabolic_count"]] <- mb_5
  result[["metabolic_syndrome"]] <- g_mb
  result
}

metabolic_calculator_tab_panel <- function() {
  tabPanel(
    "Metabolic Syndrome",
    value = "calculator_metabolic",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Metabolic Syndrome Calculator"),
        div("Select metabolic syndrome variables, adjust reference cutoffs, and add metabolic_count and metabolic_syndrome to the current data.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel metabolic-calculator-workspace",
        style = "min-width:980px;overflow-x:auto;",
        h3("Metabolic Syndrome"),
        div(class = "load-message", textOutput("metabolic_loaded_message")),
        uiOutput("metabolic_calculator_setup"),
        div(
          class = "analysis-action-row metabolic-action-row",
          actionButton("run_metabolic_calculator", "Calculate", class = "btn btn-primary"),
          downloadButton("download_metabolic_calculator", "Download CSV", class = "btn btn-default")
        ),
        uiOutput("metabolic_calculator_summary"),
        DT::DTOutput("metabolic_calculator_preview")
      )
    )
  )
}

metabolic_item_select_control <- function(id, label, choices, selected = "") {
  selectInput(
    paste0("metabolic_", id),
    label,
    choices = c("Select variable" = "", choices),
    selected = selected,
    width = "100%"
  )
}

metabolic_loaded_message_text <- function(file = NULL, data = NULL) {
  if (is.null(file)) {
    return("No data file is open.")
  }
  sprintf("Loaded %s: %s variables, %s rows.", file$name, ncol(data), nrow(data))
}

metabolic_reference_table <- function(input) {
  refs <- metabolic_reference_inputs(input)
  diagnosis <- metabolic_diagnosis_method(input)
  diagnosis_text <- if (identical(diagnosis, "japan")) {
    HTML("WC criterion required<br>and 2+ of glucose/BP/lipid")
  } else {
    HTML("metabolic_syndrome = 1<br>when metabolic_count >= 3")
  }
  rows <- list(
    c("Waist circumference", sprintf("Male >= %s, Female >= %s", refs[["wc_m"]], refs[["wc_f"]])),
    c("Glucose", sprintf(">= %s or treated for diabetes", refs[["glu"]])),
    c("Blood pressure", HTML(sprintf("SBP >= %s or DBP >= %s<br>or treated for hypertension", refs[["sbp"]], refs[["dbp"]]))),
    c("HDL-C", sprintf("Male < %s, Female < %s", refs[["hdlc_m"]], refs[["hdlc_f"]])),
    c("Triglycerides", sprintf(">= %s", refs[["tg"]])),
    c("Diagnosis", diagnosis_text)
  )
  tags$table(
    class = "hint8-initial-table metabolic-reference-table",
    tags$thead(tags$tr(tags$th("Criterion"), tags$th("Default"))),
    tags$tbody(lapply(rows, function(row) tags$tr(tags$td(row[[1]]), tags$td(HTML(as.character(row[[2]]))))))
  )
}

metabolic_reference_controls <- function(input) {
  defaults <- metabolic_reference_set_defaults(input)
  tagList(
    numericInput("metabolic_ref_wc_m", "WC male", value = input$metabolic_ref_wc_m %||% defaults$wc_m, min = 0, width = "100%"),
    numericInput("metabolic_ref_wc_f", "WC female", value = input$metabolic_ref_wc_f %||% defaults$wc_f, min = 0, width = "100%"),
    numericInput("metabolic_ref_glu", "Glucose", value = input$metabolic_ref_glu %||% defaults$glu, min = 0, width = "100%"),
    numericInput("metabolic_ref_sbp", "SBP", value = input$metabolic_ref_sbp %||% defaults$sbp, min = 0, width = "100%"),
    numericInput("metabolic_ref_dbp", "DBP", value = input$metabolic_ref_dbp %||% defaults$dbp, min = 0, width = "100%"),
    numericInput("metabolic_ref_hdlc_m", "HDL-C male", value = input$metabolic_ref_hdlc_m %||% defaults$hdlc_m, min = 0, width = "100%"),
    numericInput("metabolic_ref_hdlc_f", "HDL-C female", value = input$metabolic_ref_hdlc_f %||% defaults$hdlc_f, min = 0, width = "100%"),
    numericInput("metabolic_ref_tg", "TG", value = input$metabolic_ref_tg %||% defaults$tg, min = 0, width = "100%")
  )
}

metabolic_unit_controls <- function(input) {
  units <- metabolic_unit_inputs(input)
  tagList(
    div(
      class = "metabolic-unit-control",
      selectInput("metabolic_wc_unit", "Waist unit", choices = c("cm" = "cm", "inch" = "inch"), selected = units$wc, width = "100%")
    ),
    div(
      class = "metabolic-unit-control",
      selectInput("metabolic_glucose_unit", "Glucose unit", choices = c("mg/dL" = "mg_dl", "mmol/L" = "mmol_l"), selected = units$glucose, width = "100%")
    ),
    div(
      class = "metabolic-unit-control",
      selectInput("metabolic_lipid_unit", "Lipid unit", choices = c("mg/dL" = "mg_dl", "mmol/L" = "mmol_l"), selected = units$lipid, width = "100%")
    )
  )
}

metabolic_reference_set_control <- function(input) {
  div(
    class = "metabolic-reference-set-control",
    selectInput(
      "metabolic_reference_set",
      "Criteria / population",
      choices = metabolic_reference_set_choices(),
      selected = metabolic_reference_set_id(input),
      width = "100%"
    )
  )
}

metabolic_coding_table <- function() {
  tags$table(
    class = "hint8-initial-table metabolic-reference-table metabolic-coding-table",
    tags$tbody(
      tags$tr(tags$td("Sex"), tags$td("Male = 1, Female = 2")),
      tags$tr(tags$td("Diabetes treatment"), tags$td("Yes = 1, No = 0")),
      tags$tr(tags$td("Hypertension treatment"), tags$td("Yes = 1, No = 0"))
    )
  )
}

metabolic_output_table <- function() {
  tags$table(
    class = "hint8-initial-table metabolic-reference-table metabolic-output-table",
    tags$tbody(
      tags$tr(tags$td("Criteria count"), tags$td("metabolic_count")),
      tags$tr(tags$td("Diagnosis"), tags$td("metabolic_syndrome"))
    )
  )
}

metabolic_calculator_setup_ui <- function(file, data, variable_info, input) {
  if (is.null(file)) {
    return(setup_empty_message("Load a data file in the Data tab before using the metabolic calculator."))
  }
  choices <- names(data %||% data.frame())
  if (length(choices) == 0) {
    return(setup_empty_message("The current data file has no variables."))
  }

  specs <- metabolic_variable_specs()
  available_items <- analysis_variable_items(choices, variable_info, character(0))
  variable_inputs <- lapply(seq_len(nrow(specs)), function(index) {
    id <- specs$id[[index]]
    metabolic_item_select_control(id, specs$label[[index]], choices, selected = input[[paste0("metabolic_", id)]] %||% "")
  })

  div(
    class = "frequencies-setup-grid metabolic-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("metabolic_available", available_items, selected = isolate(input$metabolic_available), size = 19)
    ),
    div(class = "analysis-transfer-controls hint8-transfer-spacer"),
    div(
      class = "analysis-transfer-column analysis-transfer-panel metabolic-target-panel",
      analysis_field_label_tag("Metabolic variables"),
      div(class = "metabolic-variable-input-grid", variable_inputs)
    ),
    div(
      class = "analysis-options-column analysis-options-panel metabolic-reference-panel metabolic-syndrome-reference-panel",
      div(class = "analysis-option-title", "Criteria / Unit"),
      metabolic_reference_set_control(input),
      div(class = "metabolic-unit-grid", metabolic_unit_controls(input)),
      div(
        class = if (identical(metabolic_reference_set_id(input), "custom")) "metabolic-reference-custom" else "metabolic-reference-hidden",
        div(class = "analysis-option-title", "Reference cutoffs"),
        div(class = "metabolic-reference-grid", metabolic_reference_controls(input))
      ),
      metabolic_reference_table(input),
      div(class = "analysis-option-title metabolic-coding-title", "Coding"),
      metabolic_coding_table(),
      div(class = "analysis-option-title metabolic-output-title", "Output"),
      metabolic_output_table()
    )
  )
}

register_metabolic_calculator_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  current_data_file_fn,
  variable_info_fn,
  add_calculated_variable_fn
) {
  output$metabolic_loaded_message <- renderText({
    file <- current_data_file_fn()
    metabolic_loaded_message_text(file, if (is.null(file)) NULL else dataset_fn())
  })

  output$metabolic_calculator_setup <- renderUI({
    file <- current_data_file_fn()
    data <- if (is.null(file)) NULL else dataset_fn()
    variable_info <- if (is.null(file)) NULL else variable_info_fn()
    metabolic_calculator_setup_ui(file, data, variable_info, input)
  })

  observeEvent(input$metabolic_available, {
    picked <- as.character(input$metabolic_available %||% "")
    if (!nzchar(picked)) {
      return()
    }
    selected <- metabolic_selected_variables(input)
    if (picked %in% selected) {
      return()
    }
    empty_index <- which(!nzchar(selected))[1]
    if (is.na(empty_index)) {
      return()
    }
    updateSelectInput(session, paste0("metabolic_", names(selected)[[empty_index]]), selected = picked)
  }, ignoreInit = TRUE)

  result <- eventReactive(input$run_metabolic_calculator, {
    if (is.null(current_data_file_fn())) {
      showNotification("Load a data file before calculating metabolic syndrome.", type = "warning", duration = 5)
      return(NULL)
    }
    tryCatch(
      {
        result_data <- metabolic_result(dataset_fn(), metabolic_selected_variables(input), metabolic_reference_inputs(input), metabolic_unit_inputs(input), metabolic_diagnosis_method(input))
        add_calculated_variable_fn("metabolic_count", result_data[["metabolic_count"]], var_label = "Metabolic syndrome criteria count", measurement = "continuous")
        add_calculated_variable_fn("metabolic_syndrome", result_data[["metabolic_syndrome"]], var_label = "Metabolic Syndrome", measurement = "binary")
        showNotification("metabolic_count and metabolic_syndrome were added to the current data.", type = "message", duration = 5)
        result_data
      },
      error = function(error) {
        showNotification(conditionMessage(error), type = "warning", duration = 6)
        NULL
      }
    )
  }, ignoreInit = TRUE)

  output$metabolic_calculator_summary <- renderUI({
    data <- result()
    if (is.null(data)) {
      return(div(class = "empty-message", div("Select variables and click Calculate.")))
    }
    g <- data[["metabolic_syndrome"]]
    div(
      class = "empty-message",
      div(sprintf(
        "Calculated metabolic_count and metabolic_syndrome for %s rows. Metabolic syndrome: %s. Missing diagnosis: %s. The variables are available in analysis menus.",
        nrow(data),
        sum(g == 1, na.rm = TRUE),
        sum(is.na(g))
      ))
    )
  })

  output$metabolic_calculator_preview <- DT::renderDT({
    data <- result()
    if (is.null(data)) {
      return(DT::datatable(
        data.frame(Message = "No metabolic result yet.", stringsAsFactors = FALSE),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    selected <- metabolic_selected_variables(input)
    preview_names <- intersect(c(unname(selected), "metabolic_count", "metabolic_syndrome"), names(data))
    DT::datatable(
      utils::head(data[, preview_names, drop = FALSE], 50),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$download_metabolic_calculator <- downloadHandler(
    filename = function() {
      paste0("easyflow_metabolic_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      data <- result()
      if (is.null(data)) {
        data <- metabolic_result(dataset_fn(), metabolic_selected_variables(input), metabolic_reference_inputs(input), metabolic_unit_inputs(input), metabolic_diagnosis_method(input))
      }
      utils::write.csv(data, file, row.names = FALSE, na = "")
    }
  )

  invisible(TRUE)
}
