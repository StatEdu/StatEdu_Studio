# Server handlers for ANCOVA setup.

register_ancova_handlers <- function(
  input,
  output,
  session,
  selected_names_fn,
  variable_table_fn,
  dataset_fn,
  category_table_fn,
  labels_fn,
  mark_settings_dirty
) {
  dependent_variables <- reactiveVal(character(0))
  factor_variable <- reactiveVal(character(0))
  covariates <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  normality_enabled_value <- reactiveVal(TRUE)
  normality_method_value <- reactiveVal("lillie")
  force_ranked_value <- reactiveVal(FALSE)
  sum_of_squares_value <- reactiveVal("type2")
  ordered_significance_value <- reactiveVal(FALSE)
  posthoc_method_value <- reactiveVal("bonferroni")
  show_df_value <- reactiveVal(FALSE)
  mean_se_value <- reactiveVal(FALSE)
  plot_adjusted_means_value <- reactiveVal(TRUE)
  plot_raw_overlay_value <- reactiveVal(FALSE)
  plot_regression_lines_value <- reactiveVal(FALSE)
  result_value <- reactiveVal(NULL)

  current_selected <- reactive(as.character(selected_names_fn() %||% character(0)))
  current_variable_table <- reactive(variable_table_fn())

  output$ancova_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up ANCOVA."))
    }
    ancova_setup_panel(ancova_setup_state(
      selected_names = selected,
      dependent_variables = dependent_variables(),
      factor_variable = factor_variable(),
      covariates = covariates(),
      variable_table = current_variable_table(),
      labels = labels_fn(),
      selected_available = isolate(input$ancova_available),
      selected_dependent = isolate(input$ancova_dependents),
      selected_factor = isolate(input$ancova_factor),
      selected_covariates = isolate(input$ancova_covariates),
      normality_enabled = isolate(normality_enabled_value()),
      normality_method = isolate(normality_method_value()),
      force_ranked = isolate(force_ranked_value()),
      sum_of_squares = isolate(sum_of_squares_value()),
      ordered_significance = isolate(ordered_significance_value()),
      posthoc_method = isolate(posthoc_method_value()),
      show_df = isolate(show_df_value()),
      mean_se = isolate(mean_se_value()),
      plot_adjusted_means = isolate(plot_adjusted_means_value()),
      plot_raw_overlay = isolate(plot_raw_overlay_value()),
      plot_regression_lines = isolate(plot_regression_lines_value())
    ))
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "ancova",
    title = "ANCOVA Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(dependent_variables(), factor_variable(), covariates())),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observeEvent(input$ancova_force_ranked, {
    force_ranked_value(isTRUE(input$ancova_force_ranked))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_normality_enabled, {
    normality_enabled_value(isTRUE(input$ancova_normality_enabled))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_normality_method, {
    value <- as.character(input$ancova_normality_method %||% "lillie")
    if (!value %in% c("lillie", "shapiro")) value <- "lillie"
    normality_method_value(value)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_sum_of_squares, {
    value <- as.character(input$ancova_sum_of_squares %||% "type2")
    if (!value %in% c("type1", "type2", "type3")) value <- "type2"
    sum_of_squares_value(value)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_ordered_significance, {
    ordered_significance_value(isTRUE(input$ancova_ordered_significance))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_posthoc_method, {
    value <- as.character(input$ancova_posthoc_method %||% "bonferroni")
    if (!value %in% c("bonferroni", "holm")) value <- "bonferroni"
    posthoc_method_value(value)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_show_df, {
    show_df_value(isTRUE(input$ancova_show_df))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_mean_se, {
    mean_se_value(isTRUE(input$ancova_mean_se))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_plot_adjusted_means, {
    plot_adjusted_means_value(isTRUE(input$ancova_plot_adjusted_means))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_plot_raw_overlay, {
    plot_raw_overlay_value(isTRUE(input$ancova_plot_raw_overlay))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_plot_regression_lines, {
    plot_regression_lines_value(isTRUE(input$ancova_plot_regression_lines))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    dependent_variables(intersect(dependent_variables(), selected))
    factor_variable(intersect(factor_variable(), selected))
    covariates(intersect(covariates(), selected))
  })

  observeEvent(input$ancova_available_active, active_list("ancova_available"), ignoreInit = TRUE)
  observeEvent(input$ancova_dependents_active, active_list("ancova_dependents"), ignoreInit = TRUE)
  observeEvent(input$ancova_factor_active, active_list("ancova_factor"), ignoreInit = TRUE)
  observeEvent(input$ancova_covariates_active, active_list("ancova_covariates"), ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    available_selected <- intersect(as.character(input$ancova_available %||% character(0)), selected)
    dependent_selected <- intersect(as.character(input$ancova_dependents %||% character(0)), dependent_variables())
    if (length(dependent_selected) > 0 && (
      identical(active_list(), "ancova_dependents") ||
        length(available_selected) == 0
    )) {
      updateActionButton(session, "ancova_dependent_move", label = "<")
    } else {
      updateActionButton(session, "ancova_dependent_move", label = ">")
    }
  })

  observe({
    selected <- current_selected()
    available_selected <- intersect(as.character(input$ancova_available %||% character(0)), selected)
    factor_selected <- intersect(as.character(input$ancova_factor %||% character(0)), factor_variable())
    if (length(factor_selected) > 0 && (
      identical(active_list(), "ancova_factor") ||
        length(available_selected) == 0
    )) {
      updateActionButton(session, "ancova_factor_move", label = "<")
    } else {
      updateActionButton(session, "ancova_factor_move", label = ">")
    }
  })

  observe({
    selected <- current_selected()
    available_selected <- intersect(as.character(input$ancova_available %||% character(0)), selected)
    covariate_selected <- intersect(as.character(input$ancova_covariates %||% character(0)), covariates())
    if (length(covariate_selected) > 0 && (
      identical(active_list(), "ancova_covariates") ||
        length(available_selected) == 0
    )) {
      updateActionButton(session, "ancova_covariate_move", label = "<")
    } else {
      updateActionButton(session, "ancova_covariate_move", label = ">")
    }
  })

  move_from_target <- function(input_id, current) {
    chosen_target <- intersect(as.character(input[[input_id]] %||% character(0)), current)
    chosen_available <- intersect(as.character(input$ancova_available %||% character(0)), current_selected())
    length(chosen_target) > 0 && (identical(active_list(), input_id) || length(chosen_available) == 0)
  }

  observeEvent(input$ancova_dependent_move, {
    current <- intersect(dependent_variables(), current_selected())
    if (move_from_target("ancova_dependents", current)) {
      dependent_variables(setdiff(current, input$ancova_dependents %||% character(0)))
      active_list("ancova_available")
      mark_settings_dirty()
      return()
    }
    chosen <- intersect(as.character(input$ancova_available %||% character(0)), current_selected())
    allowed <- ancova_continuous_candidates(chosen, current_variable_table())
    if (length(chosen) > 0 && length(allowed) == 0) {
      showNotification("Dependent variables should be continuous or ordinal.", type = "warning")
      return()
    }
    dependent_variables(unique(c(current, allowed)))
    covariates(setdiff(covariates(), allowed))
    factor_variable(setdiff(factor_variable(), allowed))
    active_list("ancova_dependents")
    mark_settings_dirty()
  })

  observeEvent(input$ancova_dependents_doubleclick, {
    current <- intersect(dependent_variables(), current_selected())
    chosen <- intersect(as.character(input$ancova_dependents_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    dependent_variables(setdiff(current, chosen))
    active_list("ancova_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_factor_move, {
    current <- intersect(factor_variable(), current_selected())
    if (move_from_target("ancova_factor", current)) {
      factor_variable(character(0))
      active_list("ancova_available")
      mark_settings_dirty()
      return()
    }
    chosen <- intersect(as.character(input$ancova_available %||% character(0)), current_selected())
    allowed <- ancova_factor_candidates(chosen, current_variable_table())
    if (length(chosen) > 0 && length(allowed) == 0) {
      showNotification("Independent variable should be binary, nominal, or ordinal.", type = "warning")
      return()
    }
    if (length(allowed) > 1L) allowed <- allowed[[1]]
    factor_variable(allowed)
    dependent_variables(setdiff(dependent_variables(), allowed))
    covariates(setdiff(covariates(), allowed))
    active_list("ancova_factor")
    mark_settings_dirty()
  })

  observeEvent(input$ancova_factor_doubleclick, {
    current <- intersect(factor_variable(), current_selected())
    chosen <- intersect(as.character(input$ancova_factor_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    factor_variable(character(0))
    active_list("ancova_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_covariate_move, {
    current <- intersect(covariates(), current_selected())
    if (move_from_target("ancova_covariates", current)) {
      covariates(setdiff(current, input$ancova_covariates %||% character(0)))
      active_list("ancova_available")
      mark_settings_dirty()
      return()
    }
    chosen <- intersect(as.character(input$ancova_available %||% character(0)), current_selected())
    allowed <- ancova_covariate_candidates(chosen, current_variable_table())
    if (length(chosen) > 0 && length(allowed) == 0) {
      showNotification("Covariates should be continuous, binary, nominal, or ordinal.", type = "warning")
      return()
    }
    covariates(unique(c(current, allowed)))
    dependent_variables(setdiff(dependent_variables(), allowed))
    factor_variable(setdiff(factor_variable(), allowed))
    active_list("ancova_covariates")
    mark_settings_dirty()
  })

  observeEvent(input$ancova_covariates_doubleclick, {
    current <- intersect(covariates(), current_selected())
    chosen <- intersect(as.character(input$ancova_covariates_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    covariates(setdiff(current, chosen))
    active_list("ancova_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$ancova_dependent_up, {
    updated <- move_order_item(dependent_variables(), input$ancova_dependents, "up")
    if (isTRUE(updated$changed)) dependent_variables(updated$order)
  })
  observeEvent(input$ancova_dependent_down, {
    updated <- move_order_item(dependent_variables(), input$ancova_dependents, "down")
    if (isTRUE(updated$changed)) dependent_variables(updated$order)
  })
  observeEvent(input$ancova_covariate_up, {
    updated <- move_order_item(covariates(), input$ancova_covariates, "up")
    if (isTRUE(updated$changed)) covariates(updated$order)
  })
  observeEvent(input$ancova_covariate_down, {
    updated <- move_order_item(covariates(), input$ancova_covariates, "down")
    if (isTRUE(updated$changed)) covariates(updated$order)
  })

  observeEvent(input$run_ancova, {
    if (length(dependent_variables()) == 0 || length(factor_variable()) == 0 || length(covariates()) == 0) {
      showNotification("Select dependent variable(s), one independent variable, and at least one covariate.", type = "warning", duration = 5)
      return()
    }
    result <- tryCatch(
      prepare_ancova_results(
        data = dataset_fn(),
        dependents = dependent_variables(),
        factor = factor_variable(),
        covariates = covariates(),
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = list(
          normality_enabled = isTRUE(normality_enabled_value()),
          normality_method = normality_method_value(),
          force_ranked = isTRUE(force_ranked_value()),
          sum_of_squares = sum_of_squares_value(),
          ordered_significance = isTRUE(ordered_significance_value()),
          posthoc_method = posthoc_method_value(),
          show_df = isTRUE(show_df_value()),
          mean_se = isTRUE(mean_se_value()),
          plot_adjusted_means = isTRUE(plot_adjusted_means_value()),
          plot_raw_overlay = isTRUE(plot_raw_overlay_value()),
          plot_regression_lines = isTRUE(plot_regression_lines_value())
        )
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    result_value(result)
  })

  output$ancova_results <- renderUI(ancova_results_ui(result_value(), current_variable_table(), labels_fn()))

  output$ancova_reset_control <- renderUI({
    analysis_reset_button("reset_ancova_selection", enabled = length(unique(c(dependent_variables(), factor_variable(), covariates()))) > 0)
  })

  observeEvent(input$reset_ancova_selection, {
    dependent_variables(character(0))
    factor_variable(character(0))
    covariates(character(0))
    result_value(NULL)
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = c("ancova_available", "ancova_dependents", "ancova_factor", "ancova_covariates")))
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$ancova_save_control <- renderUI({
    result <- result_value()
    if (is.null(result) || !is.null(result$error)) return(NULL)
    has_figures <- any(vapply(result$results %||% list(), ancova_result_has_plots, logical(1)))
    analysis_save_buttons(
      html_button_id = "save_ancova_html_dialog",
      pdf_button_id = "save_ancova_pdf_dialog",
      figure_button_id = if (isTRUE(has_figures)) "save_ancova_figures_dialog" else NULL,
      excel_button_id = "save_ancova_excel_dialog",
      add_result_button_id = "add_ancova_result",
      has_figures = has_figures
    )
  })

  observeEvent(input$save_ancova_html_dialog, {
    result <- result_value()
    shiny::req(!is.null(result), is.null(result$error))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return()
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    write_ancova_results_html(result, path, current_variable_table(), labels_fn())
    showNotification(sprintf("HTML results saved: %s", path), type = "message")
  })

  observeEvent(input$save_ancova_pdf_dialog, {
    result <- result_value()
    shiny::req(!is.null(result), is.null(result$error))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return()
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    write_ancova_results_pdf(result, path, current_variable_table(), labels_fn())
    showNotification(sprintf("PDF results saved: %s", path), type = "message")
  })

  observeEvent(input$save_ancova_excel_dialog, {
    result <- result_value()
    shiny::req(!is.null(result), is.null(result$error))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) return()
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    save_ancova_excel_file(result, path, current_variable_table(), labels_fn())
    showNotification(sprintf("Analysis results saved: %s", path), type = "message")
  })

  observeEvent(input$save_ancova_figures_dialog, {
    result <- result_value()
    shiny::req(!is.null(result), is.null(result$error))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) return()
    tryCatch({
      saved <- save_ancova_figures_to_dir(result, directory, current_variable_table(), labels_fn())
      if (length(saved) == 0) {
        showNotification("No ANCOVA figures were selected to save.", type = "warning", duration = 5)
      } else {
        showNotification(sprintf("Saved %s figure file(s): %s", length(saved), directory), type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Failed to save ANCOVA figures:", conditionMessage(e)), type = "error", duration = 8)
    })
  })

  register_add_result_snapshot(input, session, "add_ancova_result", "ANCOVA", "ancova_results")

  invisible(TRUE)
}
