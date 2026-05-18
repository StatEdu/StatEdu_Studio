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
    result <- lapply(current$row_vars, function(row_var) {
      lapply(current$col_vars, function(col_var) {
        prepare_crosstab_results(
          data = dataset_fn(),
          row_var = row_var,
          col_var = col_var,
          variable_info = variable_table_fn(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          options = options
        )
      })
    })
    crosstab_result(unlist(result, recursive = FALSE))
  })

  output$crosstab_results <- renderUI({
    crosstab_results_ui(crosstab_result())
  })

  output$crosstab_save_control <- renderUI({
    result <- crosstab_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_crosstab_html_dialog",
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
