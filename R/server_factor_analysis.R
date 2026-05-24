# Server handlers for factor analysis setup.

register_factor_analysis_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  category_table_fn,
  labels_fn,
  mark_settings_dirty
) {
  factor_variables <- reactiveVal(character(0))
  active_factor_list <- reactiveVal(NULL)
  factor_result <- reactiveVal(NULL)

  current_selected <- reactive({
    as.character(selected_names_fn() %||% character(0))
  })

  current_allowed <- reactive({
    analysis_allowed_variables(current_selected(), variable_table_fn(), c("ordered", "continuous"))
  })

  output$factor_analysis_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up factor analysis."))
    }
    factor_analysis_setup_panel(
      factor_analysis_setup_state(
        selected_names = selected,
        factor_variables = factor_variables(),
        variable_table = variable_table_fn(),
        labels = labels_fn(),
        selected_available = isolate(input$factor_available),
        selected_selected = isolate(input$factor_selected),
        normality = input$factor_normality %||% FALSE,
        normality_method = input$factor_normality_method %||% "skew_kurt",
        method = input$factor_method %||% "pa",
        rotation = input$factor_rotation %||% "varimax",
        criterion = input$factor_criterion %||% "eigen",
        n_factors = input$factor_n_factors %||% 1,
        sort_loadings = input$factor_sort_loadings %||% TRUE,
        hide_small_loadings = input$factor_hide_small_loadings %||% TRUE,
        highlight_problem_values = input$factor_highlight_problem_values %||% TRUE,
        subfactor_reliability = input$factor_subfactor_reliability %||% FALSE
      )
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "factor",
    title = "Factor Analysis Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = factor_variables,
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observe({
    allowed <- current_allowed()
    current <- factor_variables()
    updated <- intersect(current, allowed)
    if (!identical(updated, current)) {
      factor_variables(updated)
    }
  })

  observeEvent(input$factor_available_active, {
    active_factor_list("factor_available")
  }, ignoreInit = TRUE)

  observeEvent(input$factor_selected_active, {
    active_factor_list("factor_selected")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_factor_list(), "factor_selected") && length(input$factor_selected %||% character(0)) > 0) {
      updateActionButton(session, "factor_move", label = "<")
    } else {
      updateActionButton(session, "factor_move", label = ">")
    }
  })

  observeEvent(input$factor_move, {
    allowed <- current_allowed()
    available_selected <- intersect(as.character(input$factor_available %||% character(0)), allowed)
    current <- intersect(as.character(factor_variables() %||% character(0)), allowed)
    selected_selected <- intersect(as.character(input$factor_selected %||% character(0)), current)

    if (identical(active_factor_list(), "factor_selected") && length(selected_selected) > 0) {
      factor_variables(setdiff(current, selected_selected))
      active_factor_list("factor_available")
      mark_settings_dirty()
      return()
    }
    if (length(available_selected) > 0) {
      factor_variables(c(current, setdiff(available_selected, current)))
      active_factor_list("factor_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$factor_selected_doubleclick, {
    allowed <- current_allowed()
    current <- intersect(as.character(factor_variables() %||% character(0)), allowed)
    chosen <- intersect(as.character(input$factor_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    factor_variables(setdiff(current, chosen))
    active_factor_list("factor_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "factor_available",
    selected_id = "factor_selected",
    selected_values = factor_variables,
    all_values_fn = current_allowed,
    active_list = active_factor_list,
    mark_settings_dirty = mark_settings_dirty
  )

  observeEvent(input$factor_move_up, {
    updated <- move_order_item(factor_variables(), input$factor_selected, "up")
    if (isTRUE(updated$changed)) {
      factor_variables(updated$order)
      active_factor_list("factor_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$factor_move_down, {
    updated <- move_order_item(factor_variables(), input$factor_selected, "down")
    if (isTRUE(updated$changed)) {
      factor_variables(updated$order)
      active_factor_list("factor_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$run_factor_analysis, {
    if (length(factor_variables()) < 3) {
      showNotification("Select at least three variables for factor analysis.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(
      prepare_factor_analysis_results(
        data = dataset_fn(),
        variables = factor_variables(),
        variable_info = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          normality = isTRUE(input$factor_normality),
          normality_method = input$factor_normality_method %||% "skew_kurt",
          method = input$factor_method %||% "pa",
          rotation = input$factor_rotation %||% "varimax",
          criterion = input$factor_criterion %||% "eigen",
          n_factors = input$factor_n_factors %||% 1,
          sort_loadings = isTRUE(input$factor_sort_loadings %||% TRUE),
          hide_small_loadings = isTRUE(input$factor_hide_small_loadings %||% TRUE),
          highlight_problem_values = isTRUE(input$factor_highlight_problem_values %||% TRUE),
          subfactor_reliability = isTRUE(input$factor_subfactor_reliability %||% FALSE)
        )
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 8)
        NULL
      }
    )
    if (!is.null(result)) {
      factor_result(result)
    }
  })

  output$factor_analysis_results <- renderUI({
    factor_analysis_results_ui(factor_result())
  })

  observe({
    result <- factor_result()
    if (is.null(result)) {
      return()
    }
    output[[factor_analysis_scree_plot_id()]] <- renderPlot({
      draw_factor_analysis_scree_plot(result)
    }, res = 96)
  })

  output$factor_analysis_reset_control <- renderUI({
    analysis_reset_button(
      "reset_factor_analysis_selection",
      enabled = length(as.character(factor_variables() %||% character(0))) > 0
    )
  })

  observeEvent(input$reset_factor_analysis_selection, {
    if (length(as.character(factor_variables() %||% character(0))) == 0) return()
    factor_variables(character(0))
    factor_result(NULL)
    active_factor_list("factor_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("factor_available", "factor_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$factor_analysis_save_control <- renderUI({
    result <- factor_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_factor_analysis_html_dialog",
      figure_button_id = "save_factor_analysis_figures_dialog",
      excel_button_id = "save_factor_analysis_excel_dialog",
      add_result_button_id = NULL,
      has_figures = TRUE
    )
  })

  observeEvent(input$save_factor_analysis_html_dialog, {
    result <- factor_result()
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
        write_factor_analysis_results_html(result, path)
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_factor_analysis_excel_dialog, {
    result <- factor_result()
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
        save_factor_analysis_excel_file(result, path)
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_factor_analysis_figures_dialog, {
    result <- factor_result()
    shiny::req(!is.null(result))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification("Folder selection dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    file <- file.path(directory, "scree_plot_factor_analysis.png")
    tryCatch(
      {
        save_plot_png_file(draw_factor_analysis_scree_plot, result, file, width = 7, height = 4.8)
        showNotification(sprintf("Saved figure file: %s", file), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save figure:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  invisible(TRUE)
}
