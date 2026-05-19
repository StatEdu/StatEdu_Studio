# Server handlers for crosstab analysis.

register_crosstab_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  mark_settings_dirty
) {
  crosstab_result <- reactiveVal(NULL)
  crosstab_row_vars <- reactiveVal(character(0))
  crosstab_col_vars <- reactiveVal(character(0))
  active_crosstab_list <- reactiveVal(NULL)
  crosstab_data_viewer_visible <- reactiveVal(FALSE)
  crosstab_data_viewer_label_mode <- reactiveVal(FALSE)

  current_crosstab_allowed <- function() {
    allowed <- analysis_allowed_variables(selected_names_fn(), variable_table_fn(), crosstab_allowed_measurements())
    row_vars <- crosstab_selected_variables(crosstab_row_vars(), allowed)
    col_vars <- crosstab_selected_variables(crosstab_col_vars(), allowed)
    if (!identical(row_vars, crosstab_row_vars())) crosstab_row_vars(row_vars)
    if (!identical(col_vars, crosstab_col_vars())) crosstab_col_vars(col_vars)
    list(allowed = allowed, row_vars = row_vars, col_vars = col_vars)
  }

  crosstab_state <- reactive({
    current <- current_crosstab_allowed()
    crosstab_setup_state(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_available = isolate(input$crosstab_available),
      selected_row = isolate(input$crosstab_row),
      selected_col = isolate(input$crosstab_col),
      row_var = current$row_vars,
      col_var = current$col_vars,
      design = input$crosstab_design,
      show_row_percent = input$crosstab_row_percent,
      show_column_percent = input$crosstab_column_percent,
      show_total_percent = input$crosstab_total_percent,
      split_count_percent = input$crosstab_split_count_percent,
      trend = input$crosstab_trend
    )
  })

  output$crosstab_setup <- renderUI({
    crosstab_setup_panel(crosstab_state())
  })

  crosstab_viewer_variables <- reactive({
    current <- current_crosstab_allowed()
    if (isTRUE(input$crosstab_viewer_all_step2)) {
      data <- dataset_fn()
      return(intersect(as.character(selected_names_fn() %||% character(0)), names(data %||% data.frame())))
    }
    unique(c(current$col_vars, current$row_vars))
  })

  output$crosstab_view_mode <- reactive({
    if (isTRUE(crosstab_data_viewer_visible())) "viewer" else "analysis"
  })
  outputOptions(output, "crosstab_view_mode", suspendWhenHidden = FALSE)

  observeEvent(input$crosstab_view_data, {
    crosstab_data_viewer_visible(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_back_to_analysis, {
    crosstab_data_viewer_visible(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_value_label_toggle, {
    crosstab_data_viewer_label_mode(!isTRUE(crosstab_data_viewer_label_mode()))
  }, ignoreInit = TRUE)

  output$crosstab_data_viewer <- renderUI({
    analysis_data_viewer_panel(
      title = "Cross-tabulation Data Viewer",
      variables = crosstab_viewer_variables(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      back_button_id = "crosstab_back_to_analysis",
      scope_input_id = "crosstab_viewer_all_step2",
      value_label_button_id = "crosstab_value_label_toggle",
      table_output_id = "crosstab_data_viewer_table",
      all_selected = isTRUE(input$crosstab_viewer_all_step2),
      label_mode = crosstab_data_viewer_label_mode()
    )
  })

  output$crosstab_data_viewer_table <- DT::renderDT({
    analysis_data_viewer_table(
      dataset_fn(),
      crosstab_viewer_variables(),
      category_table = category_table_fn(),
      use_labels = crosstab_data_viewer_label_mode(),
      variable_table = variable_table_fn(),
      labels = labels_fn()
    )
  })

  observeEvent(input$crosstab_available_active, {
    active_crosstab_list("crosstab_available")
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_row_active, {
    active_crosstab_list("crosstab_row")
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_col_active, {
    active_crosstab_list("crosstab_col")
  }, ignoreInit = TRUE)

  observe({
    active <- active_crosstab_list()
    if (identical(active, "crosstab_row") && length(input$crosstab_row %||% character(0)) > 0) {
      updateActionButton(session, "crosstab_assign_row", label = "<")
    } else {
      updateActionButton(session, "crosstab_assign_row", label = ">")
    }
  })

  observe({
    active <- active_crosstab_list()
    if (identical(active, "crosstab_col") && length(input$crosstab_col %||% character(0)) > 0) {
      updateActionButton(session, "crosstab_assign_col", label = "<")
    } else {
      updateActionButton(session, "crosstab_assign_col", label = ">")
    }
  })

  observeEvent(input$crosstab_assign_row, {
    state <- current_crosstab_allowed()
    if (identical(active_crosstab_list(), "crosstab_row")) {
      selected_target <- selected_order_items(input$crosstab_row, state$row_vars)
      if (length(selected_target) == 0) return()
      updated <- remove_order_items(state$row_vars, selected_target)
      crosstab_row_vars(updated$order)
      active_crosstab_list("crosstab_available")
      mark_settings_dirty()
      return()
    }
    available <- setdiff(state$allowed, c(state$row_vars, state$col_vars))
    selected <- selected_order_items(input$crosstab_available, available)
    if (length(selected) == 0) return()
    updated <- append_order_items(state$row_vars, selected)
    if (!updated$changed) return()
    crosstab_row_vars(updated$order)
    active_crosstab_list("crosstab_row")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_assign_col, {
    state <- current_crosstab_allowed()
    if (identical(active_crosstab_list(), "crosstab_col")) {
      selected_target <- selected_order_items(input$crosstab_col, state$col_vars)
      if (length(selected_target) == 0) return()
      updated <- remove_order_items(state$col_vars, selected_target)
      crosstab_col_vars(updated$order)
      active_crosstab_list("crosstab_available")
      mark_settings_dirty()
      return()
    }
    available <- setdiff(state$allowed, c(state$row_vars, state$col_vars))
    selected <- selected_order_items(input$crosstab_available, available)
    if (length(selected) == 0) return()
    updated <- append_order_items(state$col_vars, selected)
    if (!updated$changed) return()
    crosstab_col_vars(updated$order)
    active_crosstab_list("crosstab_col")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_col_doubleclick, {
    state <- current_crosstab_allowed()
    value <- input$crosstab_col_doubleclick$value %||% ""
    selected <- intersect(as.character(value), state$col_vars)
    if (length(selected) == 0) return()
    updated <- remove_order_items(state$col_vars, selected)
    if (!updated$changed) return()
    crosstab_col_vars(updated$order)
    active_crosstab_list("crosstab_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_row_doubleclick, {
    state <- current_crosstab_allowed()
    value <- input$crosstab_row_doubleclick$value %||% ""
    selected <- intersect(as.character(value), state$row_vars)
    if (length(selected) == 0) return()
    updated <- remove_order_items(state$row_vars, selected)
    if (!updated$changed) return()
    crosstab_row_vars(updated$order)
    active_crosstab_list("crosstab_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_row_up, {
    state <- current_crosstab_allowed()
    updated <- move_order_item(state$row_vars, input$crosstab_row, "up")
    if (!updated$changed) return()
    crosstab_row_vars(updated$order)
    active_crosstab_list("crosstab_row")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_row_down, {
    state <- current_crosstab_allowed()
    updated <- move_order_item(state$row_vars, input$crosstab_row, "down")
    if (!updated$changed) return()
    crosstab_row_vars(updated$order)
    active_crosstab_list("crosstab_row")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_col_up, {
    state <- current_crosstab_allowed()
    updated <- move_order_item(state$col_vars, input$crosstab_col, "up")
    if (!updated$changed) return()
    crosstab_col_vars(updated$order)
    active_crosstab_list("crosstab_col")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_col_down, {
    state <- current_crosstab_allowed()
    updated <- move_order_item(state$col_vars, input$crosstab_col, "down")
    if (!updated$changed) return()
    crosstab_col_vars(updated$order)
    active_crosstab_list("crosstab_col")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$crosstab_design, {
    design <- as.character(input$crosstab_design %||% "survey")
    updateCheckboxInput(session, "crosstab_row_percent", value = identical(design, "survey"))
    updateCheckboxInput(session, "crosstab_column_percent", value = identical(design, "experimental"))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$run_crosstab, {
    current <- current_crosstab_allowed()
    shiny::validate(shiny::need(length(current$row_vars) > 0, "Select at least one row variable."))
    shiny::validate(shiny::need(length(current$col_vars) > 0, "Select at least one column variable."))
    options <- list(
      design = input$crosstab_design %||% "survey",
      row_percent = isTRUE(input$crosstab_row_percent),
      column_percent = isTRUE(input$crosstab_column_percent),
      total_percent = isTRUE(input$crosstab_total_percent),
      split_count_percent = isTRUE(input$crosstab_split_count_percent),
      trend = isTRUE(input$crosstab_trend)
    )
    result <- list()
    errors <- character(0)
    for (row_var in current$row_vars) {
      for (col_var in current$col_vars) {
        item <- tryCatch(
          prepare_crosstab_results(
            data = dataset_fn(),
            row_var = row_var,
            col_var = col_var,
            variable_info = variable_table_fn(),
            labels = labels_fn(),
            category_table = category_table_fn(),
            options = options
          ),
          error = function(e) {
            errors <<- c(errors, sprintf("%s x %s: %s", row_var, col_var, conditionMessage(e)))
            NULL
          }
        )
        if (!is.null(item)) result[[length(result) + 1L]] <- item
      }
    }
    if (length(result) == 0) {
      crosstab_result(NULL)
      showNotification(
        if (length(errors) > 0) errors[[1]] else "No valid cross-tabulation result could be prepared.",
        type = "error",
        duration = 8
      )
      return()
    }
    crosstab_result(result)
    if (length(errors) > 0) {
      showNotification(
        sprintf("%s combination(s) were skipped. First issue: %s", length(errors), errors[[1]]),
        type = "warning",
        duration = 8
      )
    }
  })

  output$crosstab_results <- renderUI({
    crosstab_results_ui(crosstab_result())
  })

  output$crosstab_reset_control <- renderUI({
    current <- current_crosstab_allowed()
    analysis_reset_button(
      "reset_crosstab_selection",
      enabled = length(unique(c(current$col_vars, current$row_vars))) > 0
    )
  })

  observeEvent(input$reset_crosstab_selection, {
    current <- current_crosstab_allowed()
    if (length(unique(c(current$col_vars, current$row_vars))) == 0) return()
    crosstab_col_vars(character(0))
    crosstab_row_vars(character(0))
    crosstab_result(NULL)
    active_crosstab_list("crosstab_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("crosstab_available", "crosstab_col", "crosstab_row"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$crosstab_save_control <- renderUI({
    result <- crosstab_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_crosstab_html_dialog",
      pdf_button_id = "save_crosstab_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_crosstab_excel_dialog",
      add_result_button_id = "add_crosstab_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_crosstab_html_dialog, {
    result <- crosstab_result()
    shiny::req(!is.null(result))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".html")
    }
    tryCatch(
      {
        write_crosstab_results_html(result, path)
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_crosstab_pdf_dialog, {
    result <- crosstab_result()
    shiny::req(!is.null(result))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".pdf")
    }
    tryCatch(
      {
        write_crosstab_results_pdf(result, path)
        showNotification(sprintf("PDF results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save PDF results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_crosstab_excel_dialog, {
    result <- crosstab_result()
    shiny::req(!is.null(result))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification("Save dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".xlsx")
    }
    tryCatch(
      {
        save_crosstab_excel_file(result, path)
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  register_add_result_placeholder(input, "add_crosstab_result")

  invisible(TRUE)
}
