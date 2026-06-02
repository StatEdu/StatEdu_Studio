# Server handlers for correlation setup.

register_correlation_handlers <- function(
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
  correlation_variables <- reactiveVal(character(0))
  active_correlation_list <- reactiveVal(NULL)

  current_selected <- reactive({
    as.character(selected_names_fn() %||% character(0))
  })

  current_variable_table <- reactive({
    variable_table_fn()
  })

  output$correlation_setup <- renderUI({
    current_data <- tryCatch(dataset_fn(), error = function(e) NULL)
    if (!is.data.frame(current_data)) {
      return(setup_empty_message("Reconnect the data file in the Data tab before setting up correlation analysis."))
    }
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up correlation analysis."))
    }
    correlation_setup_panel(
      correlation_setup_state(
        selected_names = selected,
        correlation_variables = correlation_variables(),
        variable_table = current_variable_table(),
        labels = labels_fn(),
        selected_available = isolate(input$correlation_available),
        selected_selected = isolate(input$correlation_selected),
        continuous_method = input$correlation_continuous_method %||% "auto",
        normality = input$correlation_normality %||% TRUE,
        latent_correlations = input$correlation_latent_correlations,
        p_ci = input$correlation_p_ci %||% TRUE,
        significance_levels = input$correlation_significance_levels %||% TRUE,
        scatter_plot = input$correlation_scatter_plot %||% TRUE,
        matrix_plot = input$correlation_matrix_plot %||% TRUE
      )
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "correlation",
    title = "Correlation Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = correlation_variables,
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observe({
    selected <- current_selected()
    current <- correlation_variables()
    updated <- intersect(current, selected)
    if (!identical(updated, current)) {
      correlation_variables(updated)
    }
  })

  observeEvent(input$correlation_available_active, {
    active_correlation_list("correlation_available")
  }, ignoreInit = TRUE)

  observeEvent(input$correlation_selected_active, {
    active_correlation_list("correlation_selected")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_correlation_list(), "correlation_selected") && length(input$correlation_selected %||% character(0)) > 0) {
      updateActionButton(session, "correlation_move", label = "<")
    } else {
      updateActionButton(session, "correlation_move", label = ">")
    }
  })

  observeEvent(input$correlation_move, {
    selected <- current_selected()
    available_selected <- intersect(as.character(input$correlation_available %||% character(0)), selected)
    current <- intersect(as.character(correlation_variables() %||% character(0)), selected)
    selected_selected <- intersect(as.character(input$correlation_selected %||% character(0)), current)

    if (identical(active_correlation_list(), "correlation_selected") && length(selected_selected) > 0) {
      correlation_variables(setdiff(current, selected_selected))
      active_correlation_list("correlation_available")
      mark_settings_dirty()
      return()
    }
    if (length(available_selected) > 0) {
      correlation_variables(c(current, setdiff(available_selected, current)))
      active_correlation_list("correlation_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$correlation_selected_doubleclick, {
    selected <- current_selected()
    current <- intersect(as.character(correlation_variables() %||% character(0)), selected)
    chosen <- intersect(as.character(input$correlation_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    correlation_variables(setdiff(current, chosen))
    active_correlation_list("correlation_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "correlation_available",
    selected_id = "correlation_selected",
    selected_values = correlation_variables,
    all_values_fn = current_selected,
    active_list = active_correlation_list,
    mark_settings_dirty = mark_settings_dirty
  )

  register_dual_transfer_doubleclick_observers(
    input = input,
    available_id = "correlation_available",
    selected_id = "correlation_selected",
    selected_values = correlation_variables,
    all_values_fn = current_selected,
    active_list = active_correlation_list,
    mark_settings_dirty = mark_settings_dirty
  )

  observeEvent(input$correlation_move_up, {
    updated <- move_order_item(correlation_variables(), input$correlation_selected, "up")
    if (isTRUE(updated$changed)) {
      correlation_variables(updated$order)
      active_correlation_list("correlation_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$correlation_move_down, {
    updated <- move_order_item(correlation_variables(), input$correlation_selected, "down")
    if (isTRUE(updated$changed)) {
      correlation_variables(updated$order)
      active_correlation_list("correlation_selected")
      mark_settings_dirty()
    }
  })

  correlation_result <- reactiveVal(NULL)

  observeEvent(input$run_correlation, {
    selected_variables <- as.character(correlation_variables() %||% character(0))
    if (length(selected_variables) < 2) {
      showNotification("Select at least two variables for correlation analysis.", type = "warning", duration = 5)
      return()
    }

    current_data <- tryCatch(dataset_fn(), error = function(e) e)
    if (inherits(current_data, "error") || !is.data.frame(current_data)) {
      detail <- conditionMessage(current_data)
      if (!nzchar(detail)) {
        detail <- sprintf(
          "The active data table is not available. Selected variables: %s. Reopen the data file and apply Step 2 variable selection.",
          paste(head(selected_variables, 8), collapse = ", ")
        )
      }
      message("Correlation analysis failed before data preparation: ", detail)
      showNotification(paste("Correlation analysis failed:", detail), type = "error", duration = 8)
      return()
    }
    current_variable_table <- tryCatch(variable_table_fn(), error = function(e) NULL)
    current_labels <- tryCatch(labels_fn(), error = function(e) character(0))
    current_category_table <- tryCatch(category_table_fn(), error = function(e) NULL)

    result <- tryCatch(
      prepare_correlation_results(
        data = current_data,
        variables = selected_variables,
        variable_info = current_variable_table,
        labels = current_labels,
        category_table = current_category_table,
        options = list(
          continuous_method = input$correlation_continuous_method %||% "auto",
          normality = isTRUE(input$correlation_normality),
          latent_correlations = isTRUE(input$correlation_latent_correlations),
          reason = TRUE,
          p_ci = isTRUE(input$correlation_p_ci),
          significance_levels = isTRUE(input$correlation_significance_levels),
          scatter_plot = isTRUE(input$correlation_scatter_plot),
          matrix_plot = isTRUE(input$correlation_matrix_plot)
        )
      ),
      error = function(e) {
        detail <- conditionMessage(e)
        if (!nzchar(detail)) {
          matched <- intersect(selected_variables, names(current_data))
          missing <- setdiff(selected_variables, names(current_data))
          counts <- if (length(matched) > 0) {
            measurements <- stats::setNames(
              vapply(matched, correlation_measurement, character(1), variable_info = current_variable_table),
              matched
            )
            valid <- vapply(matched, function(name) {
              values <- correlation_analysis_vector(current_data[[name]], measurements[[name]])
              sum(!is.na(values))
            }, integer(1))
            unique_values <- vapply(matched, function(name) {
              values <- correlation_analysis_vector(current_data[[name]], measurements[[name]])
              length(unique(values[!is.na(values)]))
            }, integer(1))
            stats::setNames(sprintf("N %s, unique %s", valid, unique_values), matched)
          } else {
            character(0)
          }
          count_text <- if (length(counts) > 0) paste(sprintf("%s=%s", names(counts), counts), collapse = "; ") else "none"
          missing_text <- if (length(missing) > 0) paste(sprintf(" Missing from data: %s.", paste(head(missing, 5), collapse = ", "))) else ""
          detail <- paste0("At least two selected variables must have three or more valid values and at least two unique values. Current counts: ", count_text, ".", missing_text)
        }
        message("Correlation analysis failed: ", detail)
        showNotification(paste("Correlation analysis failed:", detail), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(result)) {
      return()
    }
    correlation_result(result)
  })

  output$correlation_results <- renderUI({
    result <- correlation_result()
    if (is.null(result)) {
      return(NULL)
    }
    correlation_results_ui(result)
  })

  output$correlation_reset_control <- renderUI({
    analysis_reset_button(
      "reset_correlation_selection",
      enabled = length(as.character(correlation_variables() %||% character(0))) > 0
    )
  })

  observeEvent(input$reset_correlation_selection, {
    if (length(as.character(correlation_variables() %||% character(0))) == 0) return()
    correlation_variables(character(0))
    correlation_result(NULL)
    active_correlation_list("correlation_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("correlation_available", "correlation_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$correlation_save_control <- renderUI({
    result <- correlation_result()
    if (is.null(result)) {
      return(NULL)
    }
    has_figures <- isTRUE(result$options$scatter_plot) || isTRUE(result$options$matrix_plot)
    analysis_save_buttons(
      html_button_id = "save_correlation_html_dialog",
      pdf_button_id = "save_correlation_pdf_dialog",
      figure_button_id = if (isTRUE(has_figures)) "save_correlation_figures_dialog" else NULL,
      excel_button_id = "save_correlation_excel_dialog",
      add_result_button_id = "add_correlation_result",
      has_figures = has_figures
    )
  })

  observe({
    result <- correlation_result()
    if (is.null(result)) {
      return()
    }
    if (isTRUE(result$options$scatter_plot)) {
      output[[correlation_scatter_plot_id()]] <- renderPlot({
        draw_correlation_scatter_plot(result)
      })
    }
    if (isTRUE(result$options$matrix_plot)) {
      output[[correlation_heatmap_plot_id()]] <- renderPlot({
        draw_correlation_heatmap(result)
      })
    }
  })

  observeEvent(input$save_correlation_html_dialog, {
    result <- correlation_result()
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
        write_correlation_results_html(result, path)
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_correlation_pdf_dialog, {
    result <- correlation_result()
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
        write_correlation_results_pdf(result, path)
        showNotification(sprintf("PDF results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save PDF results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_correlation_excel_dialog, {
    result <- correlation_result()
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
        save_correlation_excel_file(result, path)
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_correlation_figures_dialog, {
    result <- correlation_result()
    shiny::req(!is.null(result))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification("Folder selection dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- character(0)
        if (isTRUE(result$options$scatter_plot)) {
          file <- file.path(directory, "scatter_plot_correlation.png")
          save_plot_png_file(draw_correlation_scatter_plot, result, file, width = 8.5, height = 8.5)
          saved <- c(saved, file)
        }
        if (isTRUE(result$options$matrix_plot)) {
          file <- file.path(directory, "correlation_matrix_heatmap.png")
          save_plot_png_file(draw_correlation_heatmap, result, file, width = 8, height = 8)
          saved <- c(saved, file)
        }
        if (length(saved) == 0) {
          showNotification("No figures were selected to save.", type = "warning", duration = 5)
          return(invisible(NULL))
        }
        showNotification(sprintf("Saved %s figure file(s): %s", length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save figures:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_snapshot(input, session, "add_correlation_result", "Correlation", function() {
    result <- correlation_result()
    shiny::req(!is.null(result))
    saved_correlation_results_html(result)
  })

  invisible(TRUE)
}
