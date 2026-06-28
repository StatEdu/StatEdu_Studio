# Server output handlers for the Data tab workspace and tables.

register_data_workspace_outputs <- function(
  input,
  output,
  current_data_file_fn,
  active_data_file_fn = NULL,
  dataset_fn,
  restored_variable_info_fn,
  active_step_fn,
  selection_applied_fn,
  roles_applied_fn,
  active_role_fn,
  restored_data_file_fn,
  selected_names_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn,
  data_view_fn,
  active_role_names_fn,
  available_variable_names_fn,
  calculated_variables_fn = NULL,
  renamed_variables_fn = NULL,
  app_language_fn = NULL
) {
  output$data_steps <- renderUI({
    language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
    file <- current_data_file_fn()
    pending_file <- if (is.function(active_data_file_fn)) active_data_file_fn() else NULL
    excel_sheets <- character(0)
    if (valid_pending_excel_file_value(pending_file)) {
      excel_sheets <- tryCatch(excel_sheet_names(pending_file$path, pending_file$name), error = function(e) character(0))
    }
    calculated <- if (is.function(calculated_variables_fn)) calculated_variables_fn() else NULL
    renamed <- if (is.function(renamed_variables_fn)) renamed_variables_fn() else character(0)
    state <- data_steps_state(
      file = file,
      pending_file = pending_file,
      excel_sheets = excel_sheets,
      open_data = if (!is.null(file)) dataset_fn() else NULL,
      restored_info = restored_variable_info_fn(),
      step = active_step_fn(),
      applied = selection_applied_fn(),
      role_applied = roles_applied_fn(),
      role = active_role_fn(),
      restored_data_file_name = restored_data_file_fn(),
      selected = selected_names_fn(),
      dependent = dependent_names_fn(),
      independent = independent_names_fn(),
      controls = control_names_fn(),
      has_calculated_variables = (is.data.frame(calculated) && ncol(calculated) > 0) || length(renamed) > 0
    )
    do.call(data_steps_panel, c(state, list(language = language)))
  })

  output$excel_import_preview <- DT::renderDT({
    language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
    pending_file <- if (is.function(active_data_file_fn)) active_data_file_fn() else NULL
    if (!valid_pending_excel_file_value(pending_file)) {
      return(DT::datatable(data.frame(Message = statedu_text(language, "No Excel file is pending import.", statedu_utf8("eab080eca0b8ec98ac20457863656c20ed8c8cec9dbcec9db420ec9786ec8ab5eb8b88eb8ba42e"))), rownames = FALSE, options = list(dom = "t")))
    }
    tryCatch({
      preview <- read_excel_preview(
        pending_file$path,
        pending_file$name,
        sheet = as.character(input$excel_import_sheet %||% pending_file$excel_sheet %||% NULL),
        start_cell = normalize_excel_start_cell(input$excel_import_start_cell %||% pending_file$excel_start_cell %||% "A1"),
        col_names = isTRUE(input$excel_import_col_names %||% pending_file$excel_col_names %||% TRUE),
        n_max = 20
      )
      DT::datatable(preview, rownames = FALSE, options = list(dom = "tip", pageLength = 10, scrollX = TRUE))
    }, error = function(error) {
      DT::datatable(data.frame(Message = conditionMessage(error), check.names = FALSE), rownames = FALSE, options = list(dom = "t"))
    })
  })

  output$data_excel_pending <- reactive({
    pending_file <- if (is.function(active_data_file_fn)) active_data_file_fn() else NULL
    valid_pending_excel_file_value(pending_file)
  })
  outputOptions(output, "data_excel_pending", suspendWhenHidden = FALSE)

  output$excel_import_note <- renderUI({
    language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
    pending_file <- if (is.function(active_data_file_fn)) active_data_file_fn() else NULL
    if (!valid_pending_excel_file_value(pending_file)) {
      return(NULL)
    }
    tags$span(sprintf(
      statedu_text(language, "Reviewing %s. Choose sheet and start-cell options on the left, then import.", statedu_utf8("257320eab280ed86a020eca491ec9e85eb8b88eb8ba42e20ec99bcecaabdec9790ec849c20ec8b9ced8ab8ec998020ec8b9cec9e9120ec858020ec98b5ec8598ec9d8420ec84a0ed839ded959c20eb92a420eab080eca0b8ec98a4ec84b8ec9a942e")),
      pending_file$name %||% "Excel file"
    ))
  })

  output$data_loaded_message <- renderUI({
    language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
    pending_file <- if (is.function(active_data_file_fn)) active_data_file_fn() else NULL
    if (valid_pending_excel_file_value(pending_file)) {
      return(tags$span(sprintf(statedu_text(language, "Excel file selected: %s. Review the sheet on the right, then import.", statedu_utf8("457863656c20ed8c8cec9dbc20ec84a0ed839deb90a83a2025732e20ec98a4eba5b8ecaabdec9790ec849c20ec8b9ced8ab8eba5bc20eab280ed86a0ed959c20eb92a420eab080eca0b8ec98a4ec84b8ec9a942e")), pending_file$name %||% "Excel file")))
    }
    file <- current_data_file_fn()
    state <- data_loaded_message_state(
      file = file,
      data = if (!is.null(file)) dataset_fn() else NULL,
      restored_info = restored_variable_info_fn(),
      restored_file_name = restored_data_file_fn()
    )
    tags$span(do.call(data_loaded_message_text, c(state, list(language = language))))
  })

  output$data_view_title <- renderUI({
    language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
    state <- data_view_title_state(
      file = current_data_file_fn(),
      restored_info = restored_variable_info_fn(),
      view = data_view_fn(),
      selection_applied = selection_applied_fn(),
      active_role = active_role_fn(),
      active_role_names = active_role_names_fn(),
      selected = selected_names_fn(),
      available = available_variable_names_fn()
    )
    tags$span(do.call(data_view_title_text, c(state, list(language = language))))
  })

  output$data_view_toggle <- renderUI({
    data_view_toggle_control(data_view_fn(), active_step_fn(), if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language())
  })

  output$data_view <- reactive({
    data_view_fn()
  })
  outputOptions(output, "data_view", suspendWhenHidden = FALSE)

  invisible(TRUE)
}

register_data_table_outputs <- function(
  input,
  output,
  current_data_file_fn,
  dataset_fn,
  selection_applied_fn,
  selected_names_fn,
  variable_info_table_fn,
  category_label_values_fn,
  measurement_overrides_fn,
  category_label_table_data_fn,
  app_language_fn = NULL
) {
  output$selected_variable_edit_table <- DT::renderDT({
    req(identical(input$step3_panel_view %||% "labels", "variables"))
    tryCatch({
      start <- Sys.time()
      language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
      selected <- selected_names_fn()
      table_info <- variable_info_table_fn()
      if (is.null(table_info) || length(selected) == 0) {
        return(empty_variable_table(language))
      }
      table_info <- table_info[table_info$name %in% selected, , drop = FALSE]
      table_state <- variable_table_render_state(
        table_info,
        checked_names = selected,
        selected_names = selected,
        selection_applied = FALSE,
        measurement_overrides = measurement_overrides_fn(),
        language = language
      )
      out <- DT::datatable(
        table_state$table_data,
        rownames = FALSE,
        colnames = data_table_colnames(names(table_state$table_data), language),
        escape = FALSE,
        filter = "top",
        options = variable_table_options(language),
        callback = variable_table_callback(
          selected_names = table_state$checked_names,
          dependent_only = FALSE,
          single_select_role = FALSE,
          script = variable_table_callback_script(language)
        )
      )
      statedu_log_timing("render selected_variable_edit_table", start, sprintf("rows=%s", nrow(table_state$table_data)))
      out
    }, error = function(error) {
      message("Selected variable edit table render error: ", conditionMessage(error))
      empty_variable_table(if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language())
    })
  })

  output$selected_variable_summary_table <- DT::renderDT({
    tryCatch({
      language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
      table_data <- selected_variable_summary_data(
        variable_info_table_fn(),
        selected_names = selected_names_fn(),
        saved_values = category_label_values_fn(),
        measurement_overrides = measurement_overrides_fn()
      )
      if (is.null(table_data)) {
        return(empty_selected_variable_summary_table(language))
      }
      if ("Message" %in% names(table_data)) {
        return(DT::datatable(
          table_data,
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(dom = "t", paging = FALSE, ordering = FALSE)
        ))
      }
      out <- DT::datatable(
        table_data,
        rownames = FALSE,
        colnames = data_table_colnames(names(table_data), language),
        escape = FALSE,
        filter = "top",
        selection = "none",
        options = selected_variable_summary_table_options(language)
      )
      out
    }, error = function(error) {
      message("Selected variable summary render error: ", conditionMessage(error))
      empty_selected_variable_summary_table(if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language())
    })
  })

  output$category_label_table <- DT::renderDT({
    req(identical(input$step3_panel_view %||% "labels", "labels"))
    tryCatch({
      start <- Sys.time()
      language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
      table_data <- category_label_table_data_fn()
      if (is.null(table_data)) {
        return(empty_category_label_table(language))
      }
      if ("Message" %in% names(table_data)) {
        return(DT::datatable(
          table_data,
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(dom = "t", paging = FALSE, ordering = FALSE)
        ))
      }

      column_defs <- category_label_column_defs(
        table_data,
        language = language
      )

      out <- DT::datatable(
        table_data,
        rownames = FALSE,
        colnames = data_table_colnames(names(table_data), language),
        escape = FALSE,
        filter = "top",
        selection = "none",
        options = category_label_table_options(column_defs, language),
        callback = category_label_table_callback(language)
      )
      statedu_log_timing("render category_label_table", start, sprintf("rows=%s cols=%s", nrow(table_data), ncol(table_data)))
      out
    }, error = function(error) {
      message("Category label table render error: ", conditionMessage(error))
      empty_category_label_table(if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language())
    })
  })

  output$data_preview_table <- DT::renderDT({
    start <- Sys.time()
    language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
    if (is.null(current_data_file_fn())) {
      return(empty_data_preview_table(language))
    }
    data <- dataset_fn()
    if (isTRUE(selection_applied_fn())) {
      selected <- selected_names_fn()
      data <- data[, intersect(selected, names(data)), drop = FALSE]
    }
    out <- data_preview_datatable(data)
    statedu_log_timing("render data_preview_table", start, sprintf("vars=%s rows=%s", ncol(data), nrow(data)))
    out
  })

  invisible(TRUE)
}

register_variable_table_output <- function(
  output,
  current_data_file_fn,
  restored_variable_info_fn,
  variable_info_table_fn,
  selection_applied_fn,
  active_role_fn,
  active_role_names_fn,
  selected_names_fn,
  assigned_elsewhere_names_fn,
  dependent_names_fn,
  independent_names_fn,
  control_names_fn,
  measurement_overrides_fn,
  app_language_fn = NULL
) {
  output$variable_table <- DT::renderDT({
    start <- Sys.time()
    if (is.null(current_data_file_fn()) && is.null(restored_variable_info_fn())) {
      language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
      return(empty_variable_table(language))
    }
    if (isTRUE(selection_applied_fn())) {
      active_role_fn()
      checked_names <- isolate(active_role_names_fn())
    } else {
      checked_names <- isolate(selected_names_fn())
    }
    table_state <- variable_table_render_state(
      isolate(variable_info_table_fn(reactive_labels = FALSE)),
      checked_names = checked_names,
      selected_names = isolate(selected_names_fn()),
      assigned_elsewhere = isolate(assigned_elsewhere_names_fn()),
      dependent = isolate(dependent_names_fn()),
      independent = isolate(independent_names_fn()),
      controls = isolate(control_names_fn()),
      selection_applied = isTRUE(selection_applied_fn()),
      active_role = active_role_fn(),
      measurement_overrides = isolate(measurement_overrides_fn()),
      language = if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
    )
    language <- if (is.function(app_language_fn)) app_language_fn() else statedu_initial_language()
    checked_names <- table_state$checked_names
    table_data <- table_state$table_data
    out <- DT::datatable(
      table_data,
      rownames = FALSE,
      colnames = data_table_colnames(names(table_data), language),
      escape = FALSE,
      filter = "top",
      options = variable_table_options(language),
      callback = variable_table_callback(
        selected_names = checked_names,
        dependent_only = isTRUE(selection_applied_fn()) && identical(active_role_fn(), "dependent"),
        single_select_role = FALSE,
        script = variable_table_callback_script(language)
      )
    )
    statedu_log_timing("render variable_table", start, sprintf("rows=%s selection_applied=%s", nrow(table_data), isTRUE(selection_applied_fn())))
    out
  })

  invisible(TRUE)
}

register_data_view_toggle_observers <- function(input, data_view, active_step, selection_applied, go_data_step) {
  observeEvent(input$show_data_preview, {
    go_data_step(if (isTRUE(selection_applied())) active_step() else "step2", "preview")
  })

  observeEvent(input$show_variable_info, {
    if (identical(active_step(), "step3")) {
      go_data_step("step3", "labels")
    } else {
      go_data_step(active_step())
    }
  })

  invisible(TRUE)
}
