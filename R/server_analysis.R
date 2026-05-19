# Server handlers for regression execution, output, and export.

register_bootstrap_progress_outputs <- function(output, bootstrap_status_fn, bootstrap_stop_visible_fn) {
  output$bootstrap_progress <- renderUI({
    bootstrap_progress_ui(bootstrap_status_fn())
  })
  output$hierarchical_bootstrap_progress <- renderUI({
    bootstrap_progress_ui(bootstrap_status_fn())
  })

  output$bootstrap_stop_control <- renderUI({
    if (!isTRUE(bootstrap_stop_visible_fn())) {
      return(NULL)
    }

    bootstrap_stop_button("stop_bootstrap")
  })
  output$hierarchical_bootstrap_stop_control <- renderUI({
    if (!isTRUE(bootstrap_stop_visible_fn())) {
      return(NULL)
    }

    bootstrap_stop_button("hierarchical_stop_bootstrap")
  })

  invisible(TRUE)
}

register_penalized_regression_handlers <- function(
  input,
  output,
  analysis_result_fn,
  dataset_fn,
  variable_table_fn,
  labels_fn,
  penalized_result,
  seed_fn
) {
  output$penalized_regression_control <- renderUI({
    results <- analysis_result_fn()
    if (!has_severe_vif(results)) {
      return(NULL)
    }
    actionButton("run_penalized_regression", "Run Ridge/LASSO/Elastic Net", class = "btn-warning")
  })

  observeEvent(input$run_penalized_regression, {
    results <- analysis_result_fn()
    shiny::req(has_severe_vif(results))
    tryCatch(
      {
        penalized_result(fit_penalized_models(
          results,
          dataset_fn(),
          variable_table_fn(),
          labels_fn(),
          seed_fn()
        ))
        showNotification("Ridge, LASSO, and Elastic Net finished.", type = "message")
      },
      error = function(e) {
        showNotification(paste("Penalized regression failed:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  invisible(TRUE)
}

regression_plot_result_block <- function(
  output,
  result,
  index,
  variable_table_fn,
  labels_fn,
  id_prefix = "residual"
) {
  qq_id <- paste0(id_prefix, "_qq_plot_", index)
  homo_id <- paste0(id_prefix, "_homoscedasticity_plot_", index)
  variable_table <- variable_table_fn()
  dependent <- all.vars(result$formula)[[1]]
  dependent_label <- display_variable_name_static(dependent, variable_table, labels_fn(), label_only = TRUE)
  local({
    plot_result <- result
    output[[qq_id]] <- renderPlot({
      plot_residual_qq(plot_result)
    }, res = 96)
    output[[homo_id]] <- renderPlot({
      plot_residual_homoscedasticity(plot_result)
    }, res = 96)
  })

  plot_result_panel(dependent_label, qq_id, homo_id, result = result)
}

register_regression_results_output <- function(
  input,
  output,
  analyses_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  penalized_result_fn
) {
  output$regression_results <- renderUI({
    if (is.null(input$run) || input$run == 0) {
      return(div(
        class = "empty-message regression-results-empty",
        "Click Run regression to fit the model."
      ))
    }

    results <- tryCatch(analyses_fn(), error = function(e) NULL)
    if (is.null(results)) {
      return(div(
        class = "empty-message regression-results-empty",
        "Click Run regression to fit the model."
      ))
    }
    tagList(
      regression_results_panel(
        results = results,
        variable_table = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        refs = regression_reference_values_static(category_table_fn()),
        value_labels = category_value_label_lookup_static(category_table_fn()),
        show_sr2 = input$show_sr2,
        show_f2 = input$show_f2,
        show_vif = input$show_vif,
        penalized = penalized_result_fn(),
        plot_blocks = lapply(seq_along(results), function(index) {
          regression_plot_result_block(
            output,
            results[[index]],
            index,
            variable_table_fn = variable_table_fn,
            labels_fn = labels_fn
          )
        })
      )
    )
  })

  invisible(TRUE)
}

register_hierarchical_results_output <- function(
  input,
  output,
  analyses_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn
) {
  output$hierarchical_results <- renderUI({
    if (is.null(input$run_hierarchical) || input$run_hierarchical == 0) {
      return(div(
        class = "empty-message regression-results-empty",
        "Click Run regression to fit the model."
      ))
    }

    results <- tryCatch(analyses_fn(), error = function(e) NULL)
    if (is.null(results)) {
      return(div(
        class = "empty-message regression-results-empty",
        "Click Run regression to fit the model."
      ))
    }
    if (!regression_results_are_hierarchical(results)) {
      return(tagList(
        regression_results_panel(
          results = results,
          variable_table = variable_table_fn(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          refs = regression_reference_values_static(category_table_fn()),
          value_labels = category_value_label_lookup_static(category_table_fn()),
          show_sr2 = input$hierarchical_show_sr2,
          show_f2 = input$hierarchical_show_f2,
          show_vif = input$hierarchical_show_vif,
          plot_blocks = lapply(seq_along(results), function(index) {
            regression_plot_result_block(
              output,
              results[[index]],
              index,
              variable_table_fn = variable_table_fn,
              labels_fn = labels_fn,
              id_prefix = "hierarchical_residual"
            )
          })
        )
      ))
    }
    tagList(
      hierarchical_results_panel(
        results = results,
        variable_table = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        refs = regression_reference_values_static(category_table_fn()),
        value_labels = category_value_label_lookup_static(category_table_fn()),
        show_sr2 = input$hierarchical_show_sr2,
        show_f2 = input$hierarchical_show_f2,
        show_vif = input$hierarchical_show_vif,
        plot_blocks = lapply(seq_along(results), function(index) {
          regression_plot_result_block(
            output,
            results[[index]],
            index,
            variable_table_fn = variable_table_fn,
            labels_fn = labels_fn,
            id_prefix = "hierarchical_residual"
          )
        })
      )
    )
  })

  invisible(TRUE)
}

register_hierarchical_save_handlers <- function(
  input,
  output,
  analyses_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn
) {
  output$hierarchical_save_control <- renderUI({
    if (is.null(input$run_hierarchical) || input$run_hierarchical == 0) {
      return(NULL)
    }
    if (is.null(tryCatch(analyses_fn(), error = function(e) NULL))) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_hierarchical_html_dialog",
      pdf_button_id = "save_hierarchical_pdf_dialog",
      figure_button_id = "save_hierarchical_figures_dialog",
      excel_button_id = "save_hierarchical_excel_dialog",
      add_result_button_id = "add_hierarchical_result"
    )
  })

  observeEvent(input$save_hierarchical_excel_dialog, {
    shiny::req(!is.null(input$run_hierarchical), input$run_hierarchical > 0)
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
        results <- analyses_fn()
        save_fn <- if (regression_results_are_hierarchical(results)) save_hierarchical_excel_file else save_analysis_excel_file
        save_fn(
          results,
          path,
          variable_table_fn(),
          labels_fn(),
          category_table_fn(),
          show_sr2 = input$hierarchical_show_sr2,
          show_f2 = input$hierarchical_show_f2,
          show_vif = input$hierarchical_show_vif
        )
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_hierarchical_figures_dialog, {
    shiny::req(!is.null(input$run_hierarchical), input$run_hierarchical > 0)
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification("Folder selection dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        results <- analyses_fn()
        save_fn <- if (regression_results_are_hierarchical(results)) save_hierarchical_figures_to_dir else save_analysis_figures_to_dir
        saved <- save_fn(
          results,
          directory,
          variable_table_fn(),
          labels_fn()
        )
        showNotification(sprintf("Saved %s figure file(s): %s", length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save figures:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_hierarchical_html_dialog, {
    shiny::req(!is.null(input$run_hierarchical), input$run_hierarchical > 0)
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
        results <- analyses_fn()
        write_fn <- if (regression_results_are_hierarchical(results)) write_hierarchical_results_html else write_analysis_results_html
        write_fn(
          results,
          path,
          variable_table = variable_table_fn(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          show_sr2 = input$hierarchical_show_sr2,
          show_f2 = input$hierarchical_show_f2,
          show_vif = input$hierarchical_show_vif
        )
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_hierarchical_pdf_dialog, {
    shiny::req(!is.null(input$run_hierarchical), input$run_hierarchical > 0)
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
        results <- analyses_fn()
        write_fn <- if (regression_results_are_hierarchical(results)) write_hierarchical_results_pdf else write_analysis_results_pdf
        write_fn(
          results,
          path,
          variable_table = variable_table_fn(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          show_sr2 = input$hierarchical_show_sr2,
          show_f2 = input$hierarchical_show_f2,
          show_vif = input$hierarchical_show_vif
        )
        showNotification(sprintf("PDF results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save PDF results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_placeholder(input, "add_hierarchical_result")

  invisible(TRUE)
}

register_analysis_save_handlers <- function(
  input,
  output,
  analyses_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn
) {
  output$regression_save_control <- renderUI({
    if (is.null(input$run) || input$run == 0) {
      return(NULL)
    }
    if (is.null(tryCatch(analyses_fn(), error = function(e) NULL))) {
      return(NULL)
    }
    div(
      class = "regression-save-action",
      analysis_save_buttons(
        html_button_id = "save_analysis_html_dialog",
        pdf_button_id = "save_analysis_pdf_dialog",
        figure_button_id = "save_analysis_figures_dialog",
        excel_button_id = "save_analysis_excel_dialog",
        add_result_button_id = "add_regression_result"
      )
    )
  })

  observeEvent(input$save_analysis_excel_dialog, {
    shiny::req(!is.null(input$run), input$run > 0)
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
        save_analysis_excel_file(
          analyses_fn(),
          path,
          variable_table_fn(),
          labels_fn(),
          category_table_fn(),
          show_sr2 = input$show_sr2,
          show_f2 = input$show_f2,
          show_vif = input$show_vif
        )
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_analysis_figures_dialog, {
    shiny::req(!is.null(input$run), input$run > 0)
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification("Folder selection dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_analysis_figures_to_dir(
          analyses_fn(),
          directory,
          variable_table_fn(),
          labels_fn()
        )
        showNotification(sprintf("Saved %s figure file(s): %s", length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save figures:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_analysis_html_dialog, {
    shiny::req(!is.null(input$run), input$run > 0)
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
        write_analysis_results_html(
          analyses_fn(),
          path,
          variable_table = variable_table_fn(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          show_sr2 = input$show_sr2,
          show_f2 = input$show_f2,
          show_vif = input$show_vif
        )
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_analysis_pdf_dialog, {
    shiny::req(!is.null(input$run), input$run > 0)
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
        write_analysis_results_pdf(
          analyses_fn(),
          path,
          variable_table = variable_table_fn(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          show_sr2 = input$show_sr2,
          show_f2 = input$show_f2,
          show_vif = input$show_vif
        )
        showNotification(sprintf("PDF results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save PDF results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_placeholder(input, "add_regression_result")

  invisible(TRUE)
}

register_analysis_download_handlers <- function(
  input,
  output,
  analyses_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn
) {
  output$save_coefficients <- downloadHandler(
    filename = coefficients_csv_filename,
    content = function(file) {
      shiny::req(!is.null(input$run), input$run > 0)
      write_coefficients_csv(
        analyses_fn(),
        file,
        variable_table_fn(),
        labels_fn(),
        category_table_fn()
      )
    }
  )

  output$save_analysis_results <- downloadHandler(
    filename = analysis_results_html_filename,
    content = function(file) {
      shiny::req(!is.null(input$run), input$run > 0)
      write_analysis_results_html(
        analyses_fn(),
        file,
        variable_table = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        show_sr2 = input$show_sr2,
        show_f2 = input$show_f2,
        show_vif = input$show_vif
      )
    }
  )

  invisible(TRUE)
}

register_analysis_run_handlers <- function(
  input,
  session,
  prepare_analysis_result_fn,
  penalized_result,
  analysis_result,
  bootstrap_job,
  bootstrap_job_queue,
  bootstrap_cancel_requested,
  bootstrap_status,
  bootstrap_stop_visible,
  bootstrap_manager,
  bootstrap_tick
) {
  observeEvent(input$run, {
    penalized_result(NULL)
    bootstrap_job(NULL)
    bootstrap_job_queue(list())
    bootstrap_cancel_requested(FALSE)
    bootstrap_stop_visible(FALSE)
    bootstrap_status(list(done = 0L, r = 0L, stopping = FALSE, message = "Running regression"))
    prepared <- prepare_analysis_result_fn()
    analysis_result(prepared$results)
    if (length(prepared$jobs) == 0) {
      bootstrap_status(NULL)
      return()
    }
    first_job <- prepared$jobs[[1]]
    remaining_jobs <- if (length(prepared$jobs) > 1) prepared$jobs[-1] else list()
    bootstrap_job_queue(remaining_jobs)
    set.seed(first_job$seed)
    session$onFlushed(function() {
      bootstrap_manager$start(first_job)
    }, once = TRUE)
  })

  observeEvent(input$stop_bootstrap, {
    bootstrap_manager$cancel()
  })

  observeEvent(input$stop_bootstrap_now, {
    bootstrap_manager$cancel()
  }, ignoreInit = TRUE)

  observe({
    bootstrap_tick()
    isolate(bootstrap_manager$poll())
  })

  invisible(TRUE)
}

register_hierarchical_analysis_run_handlers <- function(
  input,
  session,
  prepare_hierarchical_result_fn,
  penalized_result,
  analysis_result,
  bootstrap_job,
  bootstrap_job_queue,
  bootstrap_cancel_requested,
  bootstrap_status,
  bootstrap_stop_visible,
  bootstrap_manager
) {
  observeEvent(input$run_hierarchical, {
    penalized_result(NULL)
    bootstrap_job(NULL)
    bootstrap_job_queue(list())
    bootstrap_cancel_requested(FALSE)
    bootstrap_stop_visible(FALSE)
    bootstrap_status(list(done = 0L, r = 0L, stopping = FALSE, message = "Running hierarchical regression"))
    prepared <- prepare_hierarchical_result_fn()
    analysis_result(prepared$results)
    if (length(prepared$jobs) == 0) {
      bootstrap_status(NULL)
      return()
    }
    first_job <- prepared$jobs[[1]]
    remaining_jobs <- if (length(prepared$jobs) > 1) prepared$jobs[-1] else list()
    bootstrap_job_queue(remaining_jobs)
    set.seed(first_job$seed)
    session$onFlushed(function() {
      bootstrap_manager$start(first_job)
    }, once = TRUE)
  })

  invisible(TRUE)
}

create_analysis_result_views <- function(analysis_result) {
  analysis <- reactive({
    req(analysis_result())
    results <- analysis_result()
    if (is.list(results) && length(results) > 0 && !is.null(results[[1]]$model)) {
      return(results[[1]])
    }
    results
  })

  analyses <- reactive({
    req(analysis_result())
    results <- analysis_result()
    if (is.list(results) && length(results) > 0 && !is.null(results[[1]]$model)) {
      return(results)
    }
    list(results)
  })

  list(
    analysis = analysis,
    analyses = analyses
  )
}
