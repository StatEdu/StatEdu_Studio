# Server handlers for t-test / ANOVA setup.

ttest_anova_continuous_candidates <- function(variable_names, variable_table) {
  variable_names <- as.character(variable_names %||% character(0))
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(variable_names)
  }
  measurements <- stats::setNames(tolower(as.character(variable_table$measurement)), as.character(variable_table$name))
  measurements[measurements == "ordinal"] <- "ordered"
  variable_names[variable_names %in% names(measurements) & measurements[variable_names] %in% c("ordered", "continuous")]
}

ttest_anova_factor_candidates <- function(variable_names, variable_table) {
  variable_names <- as.character(variable_names %||% character(0))
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(variable_names)
  }
  measurements <- stats::setNames(tolower(as.character(variable_table$measurement)), as.character(variable_table$name))
  variable_names[variable_names %in% names(measurements) & measurements[variable_names] %in% c("binary", "category", "ordered", "ordinal")]
}

register_ttest_anova_handlers <- function(
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
  active_ttest_list <- reactiveVal(NULL)
  normality_enabled_value <- reactiveVal(FALSE)
  normality_study_type_value <- reactiveVal("survey")
  normality_survey_method_value <- reactiveVal("skew_kurtosis")
  normality_experimental_method_value <- reactiveVal("sw")
  normality_method_value <- reactiveVal("skew_kurtosis")
  post_hoc_method_value <- reactiveVal("scheffe")
  trend_analysis_value <- reactiveVal(FALSE)
  ordered_significance_value <- reactiveVal(FALSE)
  effect_size_value <- reactiveVal(FALSE)

  current_selected <- reactive({
    as.character(selected_names_fn() %||% character(0))
  })

  current_variable_table <- reactive({
    variable_table_fn()
  })

  output$ttest_anova_setup <- renderUI({
    selected <- current_selected()
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up t-test / ANOVA."))
    }
    ttest_anova_setup_panel(
      ttest_anova_setup_state(
        selected_names = selected,
        dependent_variables = dependent_variables(),
        factor_variables = factor_variables(),
        variable_table = current_variable_table(),
        labels = labels_fn(),
        method_value = input$ttest_anova_method,
        selected_available = isolate(input$ttest_available),
        selected_dependent = isolate(input$ttest_dependents),
        selected_factor = isolate(input$ttest_factors),
        normality_enabled = isolate(normality_enabled_value()),
        normality_study_type = isolate(normality_study_type_value()),
        normality_method = isolate(normality_method_value()),
        normality_survey_method = isolate(normality_survey_method_value()),
        normality_experimental_method = isolate(normality_experimental_method_value()),
        trend_analysis = isolate(trend_analysis_value()),
        post_hoc_method = isolate(post_hoc_method_value()),
        ordered_significance = isolate(ordered_significance_value()),
        effect_size = isolate(effect_size_value())
      )
    )
  })

  observeEvent(input$ttest_anova_normality_enabled, {
    enabled <- isTRUE(input$ttest_anova_normality_enabled)
    normality_enabled_value(enabled)
    if (enabled) {
      normality_study_type_value("survey")
      normality_survey_method_value("skew_kurtosis")
      normality_method_value("skew_kurtosis")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_anova_normality_study_type, {
    value <- input$ttest_anova_normality_study_type %||% "survey"
    if (!value %in% c("survey", "experimental")) {
      value <- "survey"
    }
    normality_study_type_value(value)
    normality_method_value(
      if (identical(value, "experimental")) {
        normality_experimental_method_value() %||% "sw"
      } else {
        normality_survey_method_value() %||% "skew_kurtosis"
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_anova_survey_normality_method, {
    value <- input$ttest_anova_survey_normality_method %||% "skew_kurtosis"
    if (!value %in% c("skew_kurtosis", "sw", "ks")) {
      value <- "skew_kurtosis"
    }
    normality_survey_method_value(value)
    if (identical(normality_study_type_value(), "survey")) {
      normality_method_value(value)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_anova_experimental_normality_method, {
    value <- input$ttest_anova_experimental_normality_method %||% "sw"
    if (!value %in% c("sw", "ks")) {
      value <- "sw"
    }
    normality_experimental_method_value(value)
    if (identical(normality_study_type_value(), "experimental")) {
      normality_method_value(value)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_anova_post_hoc_method, {
    value <- input$ttest_anova_post_hoc_method %||% "scheffe"
    post_hoc_method_value(value)
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_anova_trend_analysis, {
    trend_analysis_value(isTRUE(input$ttest_anova_trend_analysis))
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_anova_ordered_significance, {
    ordered_significance_value(isTRUE(input$ttest_anova_ordered_significance))
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_anova_effect_size, {
    effect_size_value(isTRUE(input$ttest_anova_effect_size))
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

  observeEvent(input$ttest_available_active, {
    active_ttest_list("ttest_available")
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_dependents_active, {
    active_ttest_list("ttest_dependents")
  }, ignoreInit = TRUE)

  observeEvent(input$ttest_factors_active, {
    active_ttest_list("ttest_factors")
  }, ignoreInit = TRUE)

  observe({
    if (identical(active_ttest_list(), "ttest_dependents") && length(input$ttest_dependents %||% character(0)) > 0) {
      updateActionButton(session, "ttest_dependent_move", label = "<")
    } else {
      updateActionButton(session, "ttest_dependent_move", label = ">")
    }
  })

  observe({
    if (identical(active_ttest_list(), "ttest_factors") && length(input$ttest_factors %||% character(0)) > 0) {
      updateActionButton(session, "ttest_factor_move", label = "<")
    } else {
      updateActionButton(session, "ttest_factor_move", label = ">")
    }
  })

  observeEvent(input$ttest_dependent_move, {
    selected <- current_selected()
    if (identical(active_ttest_list(), "ttest_dependents")) {
      chosen <- intersect(as.character(input$ttest_dependents %||% character(0)), dependent_variables())
      current <- intersect(dependent_variables(), selected)
      updated <- setdiff(current, chosen)
      if (!identical(updated, current)) {
        dependent_variables(updated)
        active_ttest_list("ttest_available")
        mark_settings_dirty()
      }
      return()
    }
    chosen <- intersect(as.character(input$ttest_available %||% character(0)), selected)
    allowed <- ttest_anova_continuous_candidates(chosen, current_variable_table())
    if (length(chosen) > 0 && length(allowed) == 0) {
      showNotification("Dependent variables should be ordinal or continuous.", type = "warning", duration = 4)
      return()
    }
    current <- intersect(dependent_variables(), selected)
    dependent_variables(c(current, setdiff(allowed, current)))
    active_ttest_list("ttest_dependents")
    mark_settings_dirty()
  })

  observeEvent(input$ttest_factor_move, {
    selected <- current_selected()
    if (identical(active_ttest_list(), "ttest_factors")) {
      chosen <- intersect(as.character(input$ttest_factors %||% character(0)), factor_variables())
      current <- intersect(factor_variables(), selected)
      updated <- setdiff(current, chosen)
      if (!identical(updated, current)) {
        factor_variables(updated)
        active_ttest_list("ttest_available")
        mark_settings_dirty()
      }
      return()
    }
    chosen <- intersect(as.character(input$ttest_available %||% character(0)), selected)
    allowed <- ttest_anova_factor_candidates(chosen, current_variable_table())
    if (length(chosen) > 0 && length(allowed) == 0) {
      showNotification("Grouping variables should be binary, nominal, or ordinal.", type = "warning", duration = 4)
      return()
    }
    current <- intersect(factor_variables(), selected)
    factor_variables(c(current, setdiff(allowed, current)))
    active_ttest_list("ttest_factors")
    mark_settings_dirty()
  })

  observeEvent(input$ttest_dependent_up, {
    updated <- move_order_item(dependent_variables(), input$ttest_dependents, "up")
    if (isTRUE(updated$changed)) {
      dependent_variables(updated$order)
      active_ttest_list("ttest_dependents")
      mark_settings_dirty()
    }
  })

  observeEvent(input$ttest_dependent_down, {
    updated <- move_order_item(dependent_variables(), input$ttest_dependents, "down")
    if (isTRUE(updated$changed)) {
      dependent_variables(updated$order)
      active_ttest_list("ttest_dependents")
      mark_settings_dirty()
    }
  })

  observeEvent(input$ttest_factor_up, {
    updated <- move_order_item(factor_variables(), input$ttest_factors, "up")
    if (isTRUE(updated$changed)) {
      factor_variables(updated$order)
      active_ttest_list("ttest_factors")
      mark_settings_dirty()
    }
  })

  observeEvent(input$ttest_factor_down, {
    updated <- move_order_item(factor_variables(), input$ttest_factors, "down")
    if (isTRUE(updated$changed)) {
      factor_variables(updated$order)
      active_ttest_list("ttest_factors")
      mark_settings_dirty()
    }
  })

  ttest_anova_result <- reactiveVal(NULL)

  observeEvent(input$run_ttest_anova, {
    if (length(dependent_variables()) == 0 || length(factor_variables()) == 0) {
      showNotification("Select at least one dependent variable and one grouping variable.", type = "warning", duration = 5)
      return()
    }
    normality_enabled <- isTRUE(normality_enabled_value())
    normality_study_type <- normality_study_type_value() %||% "survey"
    if (!normality_study_type %in% c("survey", "experimental")) {
      normality_study_type <- "survey"
    }
    normality_method <- if (!normality_enabled) {
      "none"
    } else if (identical(normality_study_type, "experimental")) {
      normality_experimental_method_value() %||% "sw"
    } else {
      normality_survey_method_value() %||% "skew_kurtosis"
    }
    if (identical(normality_study_type, "experimental") && !normality_method %in% c("sw", "ks")) {
      normality_method <- "sw"
    }
    post_hoc_method <- post_hoc_method_value() %||% "scheffe"
    options <- list(
        normality_enabled = normality_enabled,
        normality_study_type = normality_study_type,
        normality_method = normality_method,
        normality_skew_kurtosis = normality_enabled && identical(normality_method, "skew_kurtosis"),
        normality_sw = normality_enabled && identical(normality_method, "sw"),
        normality_ks = normality_enabled && identical(normality_method, "ks"),
        trend_analysis = isTRUE(trend_analysis_value()),
        post_hoc = TRUE,
        post_hoc_method = post_hoc_method,
        ordered_significance = isTRUE(ordered_significance_value()),
        effect_size = isTRUE(effect_size_value())
      )
    result <- tryCatch(
      prepare_ttest_anova_results(
        data = dataset_fn(),
        dependents = dependent_variables(),
        factors = factor_variables(),
        variable_info = current_variable_table(),
        labels = labels_fn(),
        category_table = category_table_fn(),
        options = options
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    ttest_anova_result(result)
  })

  output$ttest_anova_results <- renderUI({
    result <- ttest_anova_result()
    if (is.null(result)) {
      return(empty_message("Move variables and click Run analysis."))
    }
    ttest_anova_results_ui(result)
  })

  output$ttest_anova_save_control <- renderUI({
    result <- ttest_anova_result()
    if (is.null(result) || !is.null(result$error)) {
      return(NULL)
    }
    div(
      class = "analysis-save-action",
      actionButton("save_ttest_anova_excel_dialog", "Save tables", class = "btn-primary")
    )
  })

  observeEvent(input$save_ttest_anova_excel_dialog, {
    result <- ttest_anova_result()
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

  invisible(TRUE)
}
