# Server handlers for nonparametric tests.

register_nonparametric_handlers <- function(
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
  factor_variables <- reactiveVal(character(0))
  active_list <- reactiveVal(NULL)
  post_hoc_method_value <- reactiveVal("bonferroni")
  trend_analysis_value <- reactiveVal(FALSE)
  ordered_significance_value <- reactiveVal(FALSE)
  effect_size_value <- reactiveVal(TRUE)
  median_iqr_value <- reactiveVal(FALSE)
  nonparametric_result <- reactiveVal(NULL)

  current_selected <- reactive({
    as.character(selected_names_fn() %||% character(0))
  })

  current_variable_table <- reactive({
    variable_table_fn()
  })

  output$nonparametric_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up nonparametric tests."))
    }
    nonparametric_setup_panel(
      nonparametric_setup_state(
        selected_names = selected,
        dependent_variables = dependent_variables(),
        factor_variables = factor_variables(),
        variable_table = current_variable_table(),
        labels = labels_fn(),
        selected_available = isolate(input$nonparametric_available),
        selected_dependent = isolate(input$nonparametric_dependents),
        selected_factor = isolate(input$nonparametric_factors),
        trend_analysis = isolate(trend_analysis_value()),
        nonparametric_post_hoc_method = isolate(post_hoc_method_value()),
        ordered_significance = isolate(ordered_significance_value()),
        effect_size = isolate(effect_size_value()),
        median_iqr = isolate(median_iqr_value())
      )
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "nonparametric",
    title = "Nonparametric Tests Data Viewer",
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(dependent_variables(), factor_variables())),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn
  )

  observeEvent(input$nonparametric_post_hoc_method, {
    value <- input$nonparametric_post_hoc_method %||% "bonferroni"
    if (!value %in% c("bonferroni", "holm")) {
      value <- "bonferroni"
    }
    post_hoc_method_value(value)
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_trend_analysis, {
    trend_analysis_value(isTRUE(input$nonparametric_trend_analysis))
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_ordered_significance, {
    ordered_significance_value(isTRUE(input$nonparametric_ordered_significance))
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_effect_size, {
    effect_size_value(isTRUE(input$nonparametric_effect_size))
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_median_iqr, {
    median_iqr_value(isTRUE(input$nonparametric_median_iqr))
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    current_dependents <- dependent_variables()
    updated_dependents <- intersect(current_dependents, selected)
    if (!identical(updated_dependents, current_dependents)) {
      dependent_variables(updated_dependents)
    }
    current_factors <- factor_variables()
    updated_factors <- intersect(current_factors, selected)
    if (!identical(updated_factors, current_factors)) {
      factor_variables(updated_factors)
    }
  })

  observeEvent(input$nonparametric_available_active, {
    active_list("nonparametric_available")
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_dependents_active, {
    active_list("nonparametric_dependents")
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_factors_active, {
    active_list("nonparametric_factors")
  }, ignoreInit = TRUE)

  observe({
    selected <- current_selected()
    available_selected <- intersect(as.character(input$nonparametric_available %||% character(0)), selected)
    dependent_selected <- intersect(as.character(input$nonparametric_dependents %||% character(0)), dependent_variables())
    if (length(dependent_selected) > 0 && (
      identical(active_list(), "nonparametric_dependents") ||
        length(available_selected) == 0
    )) {
      updateActionButton(session, "nonparametric_dependent_move", label = "<")
    } else {
      updateActionButton(session, "nonparametric_dependent_move", label = ">")
    }
  })

  observe({
    selected <- current_selected()
    available_selected <- intersect(as.character(input$nonparametric_available %||% character(0)), selected)
    factor_selected <- intersect(as.character(input$nonparametric_factors %||% character(0)), factor_variables())
    if (length(factor_selected) > 0 && (
      identical(active_list(), "nonparametric_factors") ||
        length(available_selected) == 0
    )) {
      updateActionButton(session, "nonparametric_factor_move", label = "<")
    } else {
      updateActionButton(session, "nonparametric_factor_move", label = ">")
    }
  })

  observeEvent(input$nonparametric_dependent_move, {
    selected <- current_selected()
    current <- intersect(dependent_variables(), selected)
    chosen_target <- intersect(as.character(input$nonparametric_dependents %||% character(0)), current)
    chosen_available <- intersect(as.character(input$nonparametric_available %||% character(0)), selected)
    remove_from_target <- length(chosen_target) > 0 &&
      (identical(active_list(), "nonparametric_dependents") || length(chosen_available) == 0)
    if (isTRUE(remove_from_target)) {
      updated <- setdiff(current, chosen_target)
      if (!identical(updated, current)) {
        dependent_variables(updated)
        active_list("nonparametric_available")
        mark_settings_dirty()
      }
      return()
    }
    allowed <- ttest_anova_continuous_candidates(chosen_available, current_variable_table())
    if (length(chosen_available) > 0 && length(allowed) == 0) {
      showNotification("Dependent variables should be ordinal or continuous.", type = "warning", duration = 4)
      return()
    }
    dependent_variables(c(current, setdiff(allowed, current)))
    active_list("nonparametric_dependents")
    mark_settings_dirty()
  })

  observeEvent(input$nonparametric_dependents_doubleclick, {
    selected <- current_selected()
    current <- intersect(dependent_variables(), selected)
    chosen <- intersect(as.character(input$nonparametric_dependents_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    dependent_variables(setdiff(current, chosen))
    active_list("nonparametric_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_factor_move, {
    selected <- current_selected()
    current <- intersect(factor_variables(), selected)
    chosen_target <- intersect(as.character(input$nonparametric_factors %||% character(0)), current)
    chosen_available <- intersect(as.character(input$nonparametric_available %||% character(0)), selected)
    remove_from_target <- length(chosen_target) > 0 &&
      (identical(active_list(), "nonparametric_factors") || length(chosen_available) == 0)
    if (isTRUE(remove_from_target)) {
      updated <- setdiff(current, chosen_target)
      if (!identical(updated, current)) {
        factor_variables(updated)
        active_list("nonparametric_available")
        mark_settings_dirty()
      }
      return()
    }
    allowed <- ttest_anova_factor_candidates(chosen_available, current_variable_table())
    if (length(chosen_available) > 0 && length(allowed) == 0) {
      showNotification("Grouping variables should be binary, nominal, or ordinal.", type = "warning", duration = 4)
      return()
    }
    factor_variables(c(current, setdiff(allowed, current)))
    active_list("nonparametric_factors")
    mark_settings_dirty()
  })

  observeEvent(input$nonparametric_factors_doubleclick, {
    selected <- current_selected()
    current <- intersect(factor_variables(), selected)
    chosen <- intersect(as.character(input$nonparametric_factors_doubleclick$value %||% ""), current)
    if (length(chosen) == 0) return()
    factor_variables(setdiff(current, chosen))
    active_list("nonparametric_available")
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$nonparametric_dependent_up, {
    updated <- move_order_item(dependent_variables(), input$nonparametric_dependents, "up")
    if (isTRUE(updated$changed)) {
      dependent_variables(updated$order)
      active_list("nonparametric_dependents")
      mark_settings_dirty()
    }
  })

  observeEvent(input$nonparametric_dependent_down, {
    updated <- move_order_item(dependent_variables(), input$nonparametric_dependents, "down")
    if (isTRUE(updated$changed)) {
      dependent_variables(updated$order)
      active_list("nonparametric_dependents")
      mark_settings_dirty()
    }
  })

  observeEvent(input$nonparametric_factor_up, {
    updated <- move_order_item(factor_variables(), input$nonparametric_factors, "up")
    if (isTRUE(updated$changed)) {
      factor_variables(updated$order)
      active_list("nonparametric_factors")
      mark_settings_dirty()
    }
  })

  observeEvent(input$nonparametric_factor_down, {
    updated <- move_order_item(factor_variables(), input$nonparametric_factors, "down")
    if (isTRUE(updated$changed)) {
      factor_variables(updated$order)
      active_list("nonparametric_factors")
      mark_settings_dirty()
    }
  })

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c("nonparametric_available", "nonparametric_dependents", "nonparametric_factors")
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) {
      return()
    }

    selected <- current_selected()
    changed <- FALSE
    if (identical(target, "nonparametric_available")) {
      chosen <- intersect(values, unique(c(dependent_variables(), factor_variables())))
      if (length(chosen) == 0) return()
      next_dependents <- setdiff(intersect(dependent_variables(), selected), chosen)
      next_factors <- setdiff(intersect(factor_variables(), selected), chosen)
      if (!identical(next_dependents, dependent_variables())) {
        dependent_variables(next_dependents)
        changed <- TRUE
      }
      if (!identical(next_factors, factor_variables())) {
        factor_variables(next_factors)
        changed <- TRUE
      }
      active_list("nonparametric_available")
    } else if (identical(target, "nonparametric_dependents")) {
      chosen <- intersect(values, selected)
      allowed <- ttest_anova_continuous_candidates(chosen, current_variable_table())
      if (length(chosen) > 0 && length(allowed) == 0) {
        showNotification("Dependent variables should be ordinal or continuous.", type = "warning", duration = 4)
        return()
      }
      next_factors <- setdiff(intersect(factor_variables(), selected), allowed)
      next_dependents <- c(intersect(dependent_variables(), selected), setdiff(allowed, dependent_variables()))
      if (!identical(next_factors, factor_variables())) {
        factor_variables(next_factors)
        changed <- TRUE
      }
      if (!identical(next_dependents, dependent_variables())) {
        dependent_variables(next_dependents)
        changed <- TRUE
      }
      active_list("nonparametric_dependents")
    } else if (identical(target, "nonparametric_factors")) {
      chosen <- intersect(values, selected)
      allowed <- ttest_anova_factor_candidates(chosen, current_variable_table())
      if (length(chosen) > 0 && length(allowed) == 0) {
        showNotification("Grouping variables should be binary, nominal, or ordinal.", type = "warning", duration = 4)
        return()
      }
      next_dependents <- setdiff(intersect(dependent_variables(), selected), allowed)
      next_factors <- c(intersect(factor_variables(), selected), setdiff(allowed, factor_variables()))
      if (!identical(next_dependents, dependent_variables())) {
        dependent_variables(next_dependents)
        changed <- TRUE
      }
      if (!identical(next_factors, factor_variables())) {
        factor_variables(next_factors)
        changed <- TRUE
      }
      active_list("nonparametric_factors")
    }
    if (changed) mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$run_nonparametric, {
    if (length(dependent_variables()) == 0 || length(factor_variables()) == 0) {
      showNotification("Select at least one dependent variable and one grouping variable.", type = "warning", duration = 5)
      return()
    }
    post_hoc_method <- post_hoc_method_value() %||% "bonferroni"
    if (!post_hoc_method %in% c("bonferroni", "holm")) {
      post_hoc_method <- "bonferroni"
    }
    options <- list(
      force_nonparametric = TRUE,
      normality_enabled = FALSE,
      normality_method = "none",
      trend_analysis = isTRUE(trend_analysis_value()),
      post_hoc = TRUE,
      nonparametric_post_hoc_method = post_hoc_method,
      ordered_significance = isTRUE(ordered_significance_value()),
      effect_size = isTRUE(effect_size_value()),
      median_iqr = isTRUE(median_iqr_value())
    )
    result <- tryCatch(
      {
        out <- prepare_ttest_anova_results(
          data = dataset_fn(),
          dependents = dependent_variables(),
          factors = factor_variables(),
          variable_info = current_variable_table(),
          labels = labels_fn(),
          category_table = category_table_fn(),
          options = options
        )
        out$type <- "nonparametric"
        out
      },
      error = function(e) list(error = conditionMessage(e))
    )
    nonparametric_result(result)
  })

  output$nonparametric_results <- renderUI({
    result <- nonparametric_result()
    if (is.null(result)) {
      return(NULL)
    }
    ttest_anova_results_ui(result)
  })

  output$nonparametric_reset_control <- renderUI({
    analysis_reset_button(
      "reset_nonparametric_selection",
      enabled = length(unique(c(dependent_variables(), factor_variables()))) > 0
    )
  })

  observeEvent(input$reset_nonparametric_selection, {
    if (length(unique(c(dependent_variables(), factor_variables()))) == 0) return()
    dependent_variables(character(0))
    factor_variables(character(0))
    nonparametric_result(NULL)
    active_list("nonparametric_available")
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c("nonparametric_available", "nonparametric_dependents", "nonparametric_factors"))
    )
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  output$nonparametric_save_control <- renderUI({
    result <- nonparametric_result()
    if (is.null(result) || !is.null(result$error)) {
      return(NULL)
    }
    analysis_save_buttons(
      html_button_id = "save_nonparametric_html_dialog",
      pdf_button_id = "save_nonparametric_pdf_dialog",
      figure_button_id = NULL,
      excel_button_id = "save_nonparametric_excel_dialog",
      add_result_button_id = "add_nonparametric_result",
      has_figures = FALSE
    )
  })

  observeEvent(input$save_nonparametric_excel_dialog, {
    result <- nonparametric_result()
    shiny::req(!is.null(result), is.null(result$error))
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
        save_ttest_anova_excel_file(result, path)
        showNotification(sprintf("Analysis results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save analysis results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_nonparametric_html_dialog, {
    result <- nonparametric_result()
    shiny::req(!is.null(result), is.null(result$error))
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
        write_nonparametric_results_html(result, path)
        showNotification(sprintf("HTML results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save HTML results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  observeEvent(input$save_nonparametric_pdf_dialog, {
    result <- nonparametric_result()
    shiny::req(!is.null(result), is.null(result$error))
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
        write_nonparametric_results_pdf(result, path)
        showNotification(sprintf("PDF results saved: %s", path), type = "message")
      },
      error = function(e) {
        showNotification(paste("Failed to save PDF results:", conditionMessage(e)), type = "error", duration = 8)
      }
    )
  })

  register_add_result_snapshot(input, session, "add_nonparametric_result", "Nonparametric Tests", function() {
    result <- nonparametric_result()
    shiny::req(!is.null(result), is.null(result$error))
    saved_nonparametric_results_html(result)
  })

  invisible(TRUE)
}
