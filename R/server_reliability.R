# Server handlers for reliability analysis.

register_reliability_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  reliability_variables,
  mark_settings_dirty
) {
  active_reliability_list <- reactiveVal(NULL)

  reliability_state <- reactive({
    reliability_setup_state(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_variables = reliability_variables(),
      selected_available = isolate(input$reliability_available),
      selected_selected = isolate(input$reliability_selected),
      normality = input$reliability_normality,
      ordinal = input$reliability_ordinal,
      reliability_if_deleted = input$reliability_if_deleted,
      item_total_correlation = input$reliability_item_total_correlation
    )
  })

  output$reliability_setup <- renderUI({
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up reliability analysis."))
    }
    reliability_setup_panel(reliability_state())
  })

  observe({
    selected <- as.character(selected_names_fn() %||% character(0))
    current <- reliability_variables()
    updated <- intersect(current, selected)
    if (!identical(updated, current)) {
      reliability_variables(updated)
    }
  })

  observeEvent(input$reliability_available_active, {
    active_reliability_list("reliability_available")
  }, ignoreInit = TRUE)

  observeEvent(input$reliability_selected_active, {
    active_reliability_list("reliability_selected")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_reliability_list(), "reliability_selected") && length(input$reliability_selected %||% character(0)) > 0) {
      updateActionButton(session, "reliability_move", label = "<")
    } else {
      updateActionButton(session, "reliability_move", label = ">")
    }
  })

  reliability_selection_measurements <- function(names) {
    reliability_measurements_for(names, variable_table_fn())
  }

  reliability_selection_allowed <- function(names) {
    measurements <- reliability_selection_measurements(names)
    allowed <- c("binary", "ordered", "continuous")
    length(names) > 0 &&
      all(measurements %in% allowed) &&
      length(unique(unname(measurements))) == 1
  }

  observeEvent(input$reliability_move, {
    selected <- as.character(selected_names_fn() %||% character(0))
    available_selected <- intersect(as.character(input$reliability_available %||% character(0)), selected)
    current <- intersect(as.character(reliability_variables() %||% character(0)), selected)
    selected_selected <- intersect(as.character(input$reliability_selected %||% character(0)), current)

    if (identical(active_reliability_list(), "reliability_selected") && length(selected_selected) > 0) {
      reliability_variables(setdiff(current, selected_selected))
      active_reliability_list("reliability_available")
      mark_settings_dirty()
      return()
    }
    if (length(available_selected) > 0) {
      next_variables <- c(current, setdiff(available_selected, current))
      if (!isTRUE(reliability_selection_allowed(next_variables))) {
        showNotification(
          "Reliability analysis accepts only one measurement level at a time. Select binary, ordinal, or continuous items only.",
          type = "warning",
          duration = 6
        )
        return()
      }
      reliability_variables(next_variables)
      active_reliability_list("reliability_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$reliability_move_up, {
    updated <- move_order_item(reliability_variables(), input$reliability_selected, "up")
    if (isTRUE(updated$changed)) {
      reliability_variables(updated$order)
      active_reliability_list("reliability_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$reliability_move_down, {
    updated <- move_order_item(reliability_variables(), input$reliability_selected, "down")
    if (isTRUE(updated$changed)) {
      reliability_variables(updated$order)
      active_reliability_list("reliability_selected")
      mark_settings_dirty()
    }
  })

  reliability_result <- reactiveVal(NULL)

  observeEvent(input$run_reliability, {
    if (length(reliability_variables()) < 2) {
      showNotification("Select at least two items for reliability analysis.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(
      prepare_reliability_results(
        data = dataset_fn(),
        variables = reliability_variables(),
        variable_info = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          normality = isTRUE(input$reliability_normality),
          ordinal = isTRUE(input$reliability_ordinal),
          reliability_if_deleted = isTRUE(input$reliability_if_deleted),
          item_total_correlation = isTRUE(input$reliability_item_total_correlation)
        )
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
    if (!is.null(result)) {
      reliability_result(result)
    }
  })

  output$reliability_results <- renderUI({
    reliability_results_ui(reliability_result())
  })

  output$reliability_save_control <- renderUI({
    result <- reliability_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_reliability_html_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_reliability_excel_dialog",
      add_result_button_id = "add_reliability_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_reliability_excel_dialog, {
    result <- reliability_result()
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
        save_reliability_excel_file(result, path)
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_reliability_html_dialog, {
    result <- reliability_result()
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
        write_reliability_results_html(result, path)
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_placeholder(input, "add_reliability_result")

  invisible(TRUE)
}
