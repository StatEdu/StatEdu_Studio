register_structural_equation_canvas_handlers <- function(input, output, session, dataset_fn, selected_names_fn, variable_table_fn, labels_fn, category_table_fn, mark_settings_dirty, app_language_fn = NULL) {
  lapply(c("cfa", "cbsem", "plssem"), function(analysis_type) {
    prefix <- structural_analysis_prefix(analysis_type)
    canvas_input <- paste0(prefix, "_canvas_state")
    canvas_output <- paste0(prefix, "_canvas_setup")
    run_input <- paste0(prefix, "_canvas_run_request")
    confirm_input <- paste0(prefix, "_canvas_run_confirm")
    advanced_input <- paste0(prefix, "_canvas_advanced_request")
    fit_result <- reactiveVal(NULL)
    pending_mi_rows <- reactiveVal(integer(0))
    pending_estimator_snapshot <- reactiveVal(NULL)
    output[[paste0(prefix, "_method_recommendation")]] <- renderUI({
      snapshot <- input[[canvas_input]] %||% list(nodes = list(), edges = list())
      recommendation <- structural_canvas_method_recommendation(
        snapshot, variable_table_fn(), input[[paste0(prefix, "_objective")]] %||% "confirmatory"
      )
      structural_canvas_method_recommendation_ui(
        recommendation,
        structural_canvas_selected_method_label(analysis_type, input[[paste0(prefix, "_estimator")]] %||% if (identical(analysis_type, "plssem")) "PLS" else "ML"),
        statedu_current_language(app_language_fn)
      )
    })
    output[[paste0(prefix, "_download_audit")]] <- downloadHandler(
      filename = function() paste0(analysis_type, "-audit-manifest-", format(Sys.Date(), "%Y%m%d"), ".json"),
      contentType = "application/json; charset=utf-8",
      content = function(file) {
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        structural_canvas_write_audit_manifest(bundle, file, analysis_type)
      }
    )
    if (analysis_type %in% c("cfa", "cbsem", "sem")) output[[paste0(prefix, "_download_reproducibility")]] <- downloadHandler(
      filename = function() paste0(analysis_type, "-analysis-record-", format(Sys.Date(), "%Y%m%d"), ".txt"),
      contentType = "text/plain; charset=utf-8",
      content = function(file) {
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        writeLines(structural_canvas_reproducibility_record(bundle), file, useBytes = TRUE)
      }
    )
    if (analysis_type %in% c("cfa", "cbsem", "sem")) output[[paste0(prefix, "_download_tables")]] <- downloadHandler(
      filename = function() paste0(analysis_type, "-result-tables-", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        shiny::req(requireNamespace("openxlsx", quietly = TRUE))
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        sheets <- structural_canvas_result_workbook_sheets(bundle, result_table)
        structural_canvas_write_result_workbook(sheets, file)
      }
    )
    if (analysis_type %in% c("cfa", "cbsem")) observe({
      data <- dataset_fn()
      choices <- names(data %||% data.frame())
      current <- as.character(input[[paste0(prefix, "_invariance_group")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_invariance_group"), choices = choices, selected = if (current %in% choices) current else "")
    })
    if (identical(analysis_type, "cfa")) observe({
      snapshot <- input[[canvas_input]] %||% list(nodes = list(), edges = list())
      specification <- structural_canvas_construct_specification(snapshot)
      eligible <- specification$name[specification$construct_type == "commonFactor" & specification$measurement_mode == "reflective"]
      current <- as.character(input[[paste0(prefix, "_parcel_construct")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_parcel_construct"), choices = eligible, selected = if (current %in% eligible) current else if (length(eligible)) eligible[[1L]] else "")
    })
    if (identical(analysis_type, "plssem")) observe({
      snapshot <- input[[canvas_input]] %||% list(nodes = list(), edges = list())
      specification <- structural_canvas_construct_specification(snapshot)
      formative <- specification$name[specification$construct_type == "composite" & specification$measurement_mode == "formative"]
      current_construct <- as.character(input[[paste0(prefix, "_redundancy_construct")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_redundancy_construct"), choices = formative, selected = if (current_construct %in% formative) current_construct else if (length(formative)) formative[[1L]] else "")
      data_names <- names(dataset_fn() %||% data.frame())
      current_criterion <- as.character(input[[paste0(prefix, "_redundancy_criterion")]] %||% "")
      criterion_choices <- c(stats::setNames("", if (identical(statedu_current_language(app_language_fn), "ko")) "선택하지 않음" else "Not selected"), stats::setNames(data_names, data_names))
      updateSelectInput(session, paste0(prefix, "_redundancy_criterion"), choices = criterion_choices, selected = if (current_criterion %in% data_names) current_criterion else "")
    })
    result_table <- function(kind) {
      structural_canvas_result_table(kind, fit_result, analysis_type, labels_fn, app_language_fn)
    }
    structural_canvas_register_result_outputs(
      input, output, prefix, canvas_output, analysis_type,
      selected_names_fn, variable_table_fn, dataset_fn, labels_fn, app_language_fn, fit_result, result_table
    )
    execute_analysis <- function(snapshot, settings = NULL) {
      structural_canvas_execute_analysis(
        snapshot, settings, input, session, dataset_fn, variable_table_fn, analysis_type, prefix, fit_result, app_language_fn
      )
    }
    structural_canvas_register_interaction_events(
      input, session, dataset_fn, selected_names_fn, variable_table_fn, app_language_fn,
      analysis_type, prefix, canvas_input, confirm_input, advanced_input,
      fit_result, pending_mi_rows, pending_estimator_snapshot, mark_settings_dirty, execute_analysis
    )
    register_analysis_data_viewer_handlers(
      input = input,
      output = output,
      prefix = prefix,
      title = paste(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), if (identical(statedu_current_language(app_language_fn), "ko")) "데이터 보기" else "Data Viewer"),
      dataset_fn = dataset_fn,
      selected_names_fn = selected_names_fn,
      variables_fn = local({
        state_input <- canvas_input
        function() custom_model_canvas_viewer_variables(input[[state_input]] %||% list())
      }),
      variable_table_fn = variable_table_fn,
      labels_fn = labels_fn,
      category_table_fn = category_table_fn,
      language_fn = app_language_fn
    )
  })
  invisible(TRUE)
}
