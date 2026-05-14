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
  frequency_state <- reactive({
    frequencies_setup_state(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      selected_variables = frequency_variables()
    )
  })

  output$frequencies_setup <- renderUI({
    frequencies_setup_panel(frequency_state())
  })

  observe({
    if (length(input$frequency_selected %||% character(0)) > 0) {
      updateActionButton(session, "frequency_move", label = "<")
    } else {
      updateActionButton(session, "frequency_move", label = ">")
    }
  })

  observeEvent(input$frequency_move, {
    available_selected <- intersect(as.character(input$frequency_available %||% character(0)), selected_names_fn())
    current <- intersect(as.character(frequency_variables() %||% character(0)), selected_names_fn())
    selected_selected <- intersect(as.character(input$frequency_selected %||% character(0)), current)

    if (length(available_selected) > 0) {
      frequency_variables(c(current, setdiff(available_selected, current)))
      mark_settings_dirty()
      return()
    }
    if (length(selected_selected) > 0) {
      frequency_variables(setdiff(current, selected_selected))
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

  output$frequencies_save_control <- renderUI({
    result <- frequency_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons("save_frequencies_excel_dialog", "save_frequencies_figures_dialog")
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

  invisible(TRUE)
}
