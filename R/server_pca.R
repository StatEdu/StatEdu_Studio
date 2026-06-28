# Server handlers for principal component analysis setup.

register_pca_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  category_table_fn,
  labels_fn,
  add_calculated_variable_fn = NULL,
  mark_settings_dirty,
  app_language_fn = NULL
) {
  pca_variables <- reactiveVal(character(0))
  active_pca_list <- reactiveVal(NULL)
  pca_result <- reactiveVal(NULL)

  current_selected <- reactive({
    as.character(selected_names_fn() %||% character(0))
  })

  current_allowed <- reactive({
    analysis_allowed_variables(current_selected(), variable_table_fn(), c("ordered", "continuous"))
  })

  output$pca_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message(analysis_ui_text("Complete Step 2 in the Data tab before setting up principal component analysis.", language), language = language))
    }
    pca_setup_panel(
      pca_setup_state(
        selected_names = selected,
        pca_variables = pca_variables(),
        variable_table = variable_table_fn(),
        labels = labels_fn(),
        selected_available = isolate(input$pca_available),
        selected_selected = isolate(input$pca_selected),
        matrix_type = input$pca_matrix_type %||% "correlation",
        rotation = input$pca_rotation %||% "none",
        criterion = input$pca_criterion %||% "eigen",
        n_components = input$pca_n_components %||% 1,
        cumulative_variance = input$pca_cumulative_variance %||% 70,
        sort_loadings = input$pca_sort_loadings %||% TRUE,
        hide_small_loadings = input$pca_hide_small_loadings %||% TRUE,
        highlight_problem_values = input$pca_highlight_problem_values %||% TRUE,
        scree_plot = input$pca_scree_plot %||% TRUE,
        biplot = input$pca_biplot %||% TRUE,
        save_component_scores = input$pca_save_component_scores %||% FALSE,
        save_component_base_name = input$pca_save_component_base_name %||% "PCA",
        options_tab = isolate(input$pca_options_tab) %||% "Model",
        language = language
      )
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "pca",
    title = "Principal Component Analysis Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = pca_variables,
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  observe({
    allowed <- current_allowed()
    current <- pca_variables()
    updated <- intersect(current, allowed)
    if (!identical(updated, current)) {
      pca_variables(updated)
    }
  })

  observeEvent(input$pca_available_active, {
    active_pca_list("pca_available")
  }, ignoreInit = TRUE)

  observeEvent(input$pca_selected_active, {
    active_pca_list("pca_selected")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_pca_list(), "pca_selected") && length(input$pca_selected %||% character(0)) > 0) {
      updateActionButton(session, "pca_move", label = "<")
    } else {
      updateActionButton(session, "pca_move", label = ">")
    }
  })

  observeEvent(input$pca_move, {
    allowed <- current_allowed()
    available_selected <- intersect(as.character(input$pca_available %||% character(0)), allowed)
    current <- intersect(as.character(pca_variables() %||% character(0)), allowed)
    selected_selected <- intersect(as.character(input$pca_selected %||% character(0)), current)

    if (identical(active_pca_list(), "pca_selected") && length(selected_selected) > 0) {
      pca_variables(setdiff(current, selected_selected))
      active_pca_list("pca_available")
      mark_settings_dirty()
      return()
    }
    if (length(available_selected) > 0) {
      pca_variables(c(current, setdiff(available_selected, current)))
      active_pca_list("pca_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$pca_selected_doubleclick, {
    allowed <- current_allowed()
    current <- intersect(as.character(pca_variables() %||% character(0)), allowed)
    chosen <- intersect(as.character(input$pca_selected_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    pca_variables(setdiff(current, chosen))
    active_pca_list("pca_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  register_dual_transfer_drop_observer(
    input = input,
    session = session,
    available_id = "pca_available",
    selected_id = "pca_selected",
    selected_values = pca_variables,
    all_values_fn = current_allowed,
    active_list = active_pca_list,
    mark_settings_dirty = mark_settings_dirty
  )

  register_dual_transfer_doubleclick_observers(
    input = input,
    available_id = "pca_available",
    selected_id = "pca_selected",
    selected_values = pca_variables,
    all_values_fn = current_allowed,
    active_list = active_pca_list,
    mark_settings_dirty = mark_settings_dirty
  )

  observeEvent(input$pca_move_up, {
    updated <- move_order_item(pca_variables(), input$pca_selected, "up")
    if (isTRUE(updated$changed)) {
      pca_variables(updated$order)
      active_pca_list("pca_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$pca_move_down, {
    updated <- move_order_item(pca_variables(), input$pca_selected, "down")
    if (isTRUE(updated$changed)) {
      pca_variables(updated$order)
      active_pca_list("pca_selected")
      mark_settings_dirty()
    }
  })

  observeEvent(input$run_pca, {
    if (length(pca_variables()) < 2) {
      showNotification("Select at least two variables for principal component analysis.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(
      prepare_pca_results(
        data = dataset_fn(),
        variables = pca_variables(),
        variable_info = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          matrix_type = input$pca_matrix_type %||% "correlation",
          rotation = input$pca_rotation %||% "none",
          criterion = input$pca_criterion %||% "eigen",
          n_components = input$pca_n_components %||% 1,
          cumulative_variance = input$pca_cumulative_variance %||% 70,
          sort_loadings = isTRUE(input$pca_sort_loadings %||% TRUE),
          hide_small_loadings = isTRUE(input$pca_hide_small_loadings %||% TRUE),
          highlight_problem_values = isTRUE(input$pca_highlight_problem_values %||% TRUE),
          scree_plot = isTRUE(input$pca_scree_plot %||% TRUE),
          biplot = isTRUE(input$pca_biplot %||% TRUE),
          save_component_scores = isTRUE(input$pca_save_component_scores %||% FALSE),
          save_component_base_name = input$pca_save_component_base_name %||% "PCA"
        )
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 8)
        NULL
      }
    )
    if (!is.null(result)) {
      pca_result(result)
      if (isTRUE(input$pca_save_component_scores %||% FALSE) && is.function(add_calculated_variable_fn)) {
        saved <- tryCatch(
          {
            calculated <- pca_saved_score_outputs(
              result,
              base_name = input$pca_save_component_base_name %||% "PCA"
            )
            created <- character(0)
            score_components <- attr(calculated, "score_components", exact = TRUE) %||% character(0)
            for (name in names(calculated)) {
              component <- as.character(score_components[[name]] %||% name)
              ok <- add_calculated_variable_fn(
                name,
                calculated[[name]],
                var_label = sprintf("Principal component score (%s)", component),
                measurement = "continuous"
              )
              if (isTRUE(ok)) {
                created <- c(created, name)
              }
            }
            created
          },
          error = function(e) {
            showNotification(paste("Failed to save PCA score variables:", conditionMessage(e)), type = "warning", duration = 8)
            character(0)
          }
        )
        if (length(saved) > 0) {
          showNotification(sprintf("Saved %s PCA score variable(s): %s", length(saved), paste(saved, collapse = ", ")), type = "message", duration = 7)
        } else {
          showNotification("No PCA score variables were saved.", type = "warning", duration = 5)
        }
      }
    }
  })

  output$pca_results <- renderUI({
    pca_results_ui(pca_result())
  })

  observe({
    result <- pca_result()
    if (is.null(result)) {
      return()
    }
    if (isTRUE(result$options$scree_plot)) {
      output[[pca_scree_plot_id()]] <- renderPlot({
        draw_pca_scree_plot(result)
      }, res = 96)
    }
    if (isTRUE(result$options$biplot)) {
      output[[pca_component_plot_id()]] <- renderPlot({
        draw_pca_component_plot(result)
      }, res = 96)
    }
  })

  output$pca_reset_control <- renderUI({
    analysis_reset_button(
      "reset_pca_selection",
      enabled = length(as.character(pca_variables() %||% character(0))) > 0
    )
  })

  observeEvent(input$reset_pca_selection, {
    if (length(as.character(pca_variables() %||% character(0))) == 0) return()
    pca_variables(character(0))
    pca_result(NULL)
    active_pca_list("pca_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("pca_available", "pca_selected"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$pca_save_control <- renderUI({
    result <- pca_result()
    if (is.null(result)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_pca_html_dialog",
      pdf_button_id = "save_pca_pdf_dialog",
      figure_button_id = "save_pca_figures_dialog",
      excel_button_id = "save_pca_excel_dialog",
      add_result_button_id = "add_pca_result",
      has_figures = TRUE
    )
  })

  observeEvent(input$save_pca_html_dialog, {
    result <- pca_result()
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
        write_pca_results_html(result, path)
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_pca_pdf_dialog, {
    result <- pca_result()
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
        write_pca_results_pdf(result, path)
        showNotification(sprintf("PDF results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save PDF results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_pca_excel_dialog, {
    result <- pca_result()
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
        save_pca_excel_file(result, path)
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_pca_figures_dialog, {
    result <- pca_result()
    shiny::req(!is.null(result))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification("Folder selection dialog was not available or was canceled.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- character(0)
        if (isTRUE(result$options$scree_plot)) {
          file <- file.path(directory, "scree_plot_pca.png")
          save_plot_png_file(draw_pca_scree_plot, result, file, width = 7, height = 4.8)
          saved <- c(saved, file)
        }
        if (isTRUE(result$options$biplot)) {
          file <- file.path(directory, "biplot_pca.png")
          save_plot_png_file(draw_pca_component_plot, result, file, width = 7, height = 5.6)
          saved <- c(saved, file)
        }
        showNotification(sprintf("Saved %s figure file(s): %s", length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save figures:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_snapshot(input, session, "add_pca_result", "Principal component analysis", "pca_results")

  invisible(TRUE)
}
