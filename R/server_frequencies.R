# Server handlers for frequency/descriptive analysis.

register_frequencies_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  frequency_variables,
  mark_settings_dirty
) {
  active_frequency_list <- reactiveVal(NULL)

  frequency_state <- reactive({
    frequencies_setup_state(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_variables = frequency_variables(),
      selected_available = isolate(input$frequency_available),
      selected_selected = isolate(input$frequency_selected),
      table_summary = input$frequency_table_summary,
      stat_min_max = input$frequency_stat_min_max %||% TRUE,
      stat_skew_kurtosis = input$frequency_stat_skew_kurtosis %||% TRUE,
      stat_median_iqr = input$frequency_stat_median_iqr %||% TRUE,
      plot_pie = input$frequency_plot_pie,
      plot_bar = input$frequency_plot_bar,
      plot_histogram = input$frequency_plot_histogram,
      plot_box = input$frequency_plot_box,
      plot_violin = input$frequency_plot_violin
    )
  })

  output$frequencies_setup <- renderUI({
    frequencies_setup_panel(frequency_state())
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "frequencies",
    title = "Frequencies / Descriptives Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = frequency_variables,
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observeEvent(input$frequency_available_active, {
    active_frequency_list("frequency_available")
  }, ignoreInit = TRUE)

  observeEvent(input$frequency_selected_active, {
    active_frequency_list("frequency_selected")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_frequency_list(), "frequency_selected") && length(input$frequency_selected %||% character(0)) > 0) {
      updateActionButton(session, "frequency_move", label = "<")
    } else {
      updateActionButton(session, "frequency_move", label = ">")
    }
  })

  observeEvent(input$frequency_move, {
    available_selected <- intersect(as.character(input$frequency_available %||% character(0)), selected_names_fn())
    current <- intersect(as.character(frequency_variables() %||% character(0)), selected_names_fn())
    selected_selected <- intersect(as.character(input$frequency_selected %||% character(0)), current)

    if (identical(active_frequency_list(), "frequency_selected") && length(selected_selected) > 0) {
      frequency_variables(setdiff(current, selected_selected))
      active_frequency_list("frequency_available")
      mark_settings_dirty()
      return()
    }
    if (length(available_selected) > 0) {
      frequency_variables(c(current, setdiff(available_selected, current)))
      active_frequency_list("frequency_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$frequency_selected_doubleclick, {
    current <- intersect(as.character(frequency_variables() %||% character(0)), selected_names_fn())
    selected <- intersect(as.character(input$frequency_selected_doubleclick$value %||% ""), current)
    if (length(selected) == 0) return()
    frequency_variables(setdiff(current, selected))
    active_frequency_list("frequency_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "frequency_available",
    selected_id = "frequency_selected",
    selected_values = frequency_variables,
    all_values_fn = selected_names_fn,
    active_list = active_frequency_list,
    mark_settings_dirty = mark_settings_dirty
  )

  register_dual_transfer_doubleclick_observers(
    input = input,
    available_id = "frequency_available",
    selected_id = "frequency_selected",
    selected_values = frequency_variables,
    all_values_fn = selected_names_fn,
    active_list = active_frequency_list,
    mark_settings_dirty = mark_settings_dirty
  )

  observeEvent(input$frequency_move_up, {
    updated <- move_order_item(frequency_variables(), input$frequency_selected, "up")
    if (isTRUE(updated$changed)) {
      frequency_variables(updated$order)
      active_frequency_list("frequency_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$frequency_move_down, {
    updated <- move_order_item(frequency_variables(), input$frequency_selected, "down")
    if (isTRUE(updated$changed)) {
      frequency_variables(updated$order)
      active_frequency_list("frequency_selected")
      mark_settings_dirty()
    }
  })

  frequency_result <- reactiveVal(NULL)

  observeEvent(input$run_frequencies, {
    result <- prepare_frequencies_results(
      data = dataset_fn(),
      variables = frequency_variables(),
      variable_info = variable_table_fn(),
      labels = labels_fn(),
      category_table = category_table_fn()
    )
    summary_table <- isTRUE(input$frequency_table_summary)
    result$options <- list(
      n_percent = summary_table,
      mean_sd = summary_table,
      min_max = isTRUE(input$frequency_stat_min_max),
      skew_kurtosis = isTRUE(input$frequency_stat_skew_kurtosis),
      median_iqr = isTRUE(input$frequency_stat_median_iqr),
      pie = isTRUE(input$frequency_plot_pie),
      bar = isTRUE(input$frequency_plot_bar),
      histogram = isTRUE(input$frequency_plot_histogram),
      box = isTRUE(input$frequency_plot_box),
      violin = isTRUE(input$frequency_plot_violin)
    )
    frequency_result(result)
  })

  observe({
    result <- frequency_result()
    if (is.null(result)) {
      return()
    }
    options <- result$options %||% list()
    data <- result$data

    if (isTRUE(options$pie) || isTRUE(options$bar)) {
      lapply(as.character(result$categorical %||% character(0)), function(name) {
        if (isTRUE(options$pie)) {
          output[[frequency_plot_output_id("pie", name)]] <- renderPlot({
            draw_frequency_plot(result, "pie", name)
          })
        }
        if (isTRUE(options$bar)) {
          output[[frequency_plot_output_id("bar", name)]] <- renderPlot({
            draw_frequency_plot(result, "bar", name)
          })
        }
      })
    }

    if (isTRUE(options$histogram) || isTRUE(options$box) || isTRUE(options$violin)) {
      lapply(as.character(result$continuous %||% character(0)), function(name) {
        if (isTRUE(options$histogram)) {
          output[[frequency_plot_output_id("histogram", name)]] <- renderPlot({
            draw_frequency_plot(result, "histogram", name)
          })
        }
        if (isTRUE(options$box)) {
          output[[frequency_plot_output_id("box", name)]] <- renderPlot({
            draw_frequency_plot(result, "box", name)
          })
        }
        if (isTRUE(options$violin)) {
          output[[frequency_plot_output_id("violin", name)]] <- renderPlot({
            draw_frequency_plot(result, "violin", name)
          })
        }
      })
    }
  })

  output$frequencies_results <- renderUI({
    frequencies_results_ui(frequency_result())
  })

  output$frequencies_reset_control <- renderUI({
    analysis_reset_button(
      "reset_frequencies_selection",
      enabled = length(as.character(frequency_variables() %||% character(0))) > 0
    )
  })

  observeEvent(input$reset_frequencies_selection, {
    if (length(as.character(frequency_variables() %||% character(0))) == 0) return()
    frequency_variables(character(0))
    frequency_result(NULL)
    active_frequency_list("frequency_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("frequency_available", "frequency_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$frequencies_save_control <- renderUI({
    result <- frequency_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_frequencies_html_dialog",
      pdf_button_id = "save_frequencies_pdf_dialog",
      figure_button_id = "save_frequencies_figures_dialog",
      excel_button_id = "save_frequencies_excel_dialog",
      add_result_button_id = "add_frequencies_result"
    )
  })

  observeEvent(input$save_frequencies_excel_dialog, {
    result <- frequency_result()
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
        save_frequencies_excel_file(result, path)
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_frequencies_figures_dialog, {
    result <- frequency_result()
    shiny::req(!is.null(result))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification("Folder selection dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_frequency_figures_to_dir(result, directory)
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

  observeEvent(input$save_frequencies_html_dialog, {
    result <- frequency_result()
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
        write_frequencies_results_html(result, path)
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_frequencies_pdf_dialog, {
    result <- frequency_result()
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
        write_frequencies_results_pdf(result, path)
        showNotification(sprintf("PDF results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save PDF results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_snapshot(input, session, "add_frequencies_result", "Frequencies / Descriptives", "frequencies_results")

  invisible(TRUE)
}
