# Server handlers for t-test / ANOVA setup.

ttest_anova_continuous_candidates <- function(variable_names, variable_table) {
  variable_names <- as.character(variable_names %||% character(0))
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(variable_names)
  }
  measurements <- stats::setNames(tolower(as.character(variable_table$measurement)), as.character(variable_table$name))
  variable_names[variable_names %in% names(measurements) & measurements[variable_names] == "continuous"]
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
  labels_fn,
  mark_settings_dirty
) {
  dependent_variables <- reactiveVal(character(0))
  factor_variables <- reactiveVal(character(0))

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
        method_value = input$ttest_anova_method
      )
    )
  })

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

  observeEvent(input$ttest_add_dependent, {
    selected <- current_selected()
    chosen <- intersect(as.character(input$ttest_available %||% character(0)), selected)
    allowed <- ttest_anova_continuous_candidates(chosen, current_variable_table())
    if (length(chosen) > 0 && length(allowed) == 0) {
      showNotification("Dependent variables should be continuous.", type = "warning", duration = 4)
      return()
    }
    current <- intersect(dependent_variables(), selected)
    dependent_variables(c(current, setdiff(allowed, current)))
    mark_settings_dirty()
  })

  observeEvent(input$ttest_remove_dependent, {
    selected <- current_selected()
    chosen <- intersect(as.character(input$ttest_dependents %||% character(0)), selected)
    current <- intersect(dependent_variables(), selected)
    updated <- setdiff(current, chosen)
    if (!identical(updated, current)) {
      dependent_variables(updated)
      mark_settings_dirty()
    }
  })

  observeEvent(input$ttest_add_factor, {
    selected <- current_selected()
    chosen <- intersect(as.character(input$ttest_available %||% character(0)), selected)
    allowed <- ttest_anova_factor_candidates(chosen, current_variable_table())
    if (length(chosen) > 0 && length(allowed) == 0) {
      showNotification("Grouping variables should be binary, nominal, or ordinal.", type = "warning", duration = 4)
      return()
    }
    current <- intersect(factor_variables(), selected)
    factor_variables(c(current, setdiff(allowed, current)))
    mark_settings_dirty()
  })

  observeEvent(input$ttest_remove_factor, {
    selected <- current_selected()
    chosen <- intersect(as.character(input$ttest_factors %||% character(0)), selected)
    current <- intersect(factor_variables(), selected)
    updated <- setdiff(current, chosen)
    if (!identical(updated, current)) {
      factor_variables(updated)
      mark_settings_dirty()
    }
  })

  ttest_anova_result <- reactiveVal(NULL)

  observeEvent(input$run_ttest_anova, {
    if (length(dependent_variables()) == 0 || length(factor_variables()) == 0) {
      showNotification("Select at least one dependent variable and one grouping variable.", type = "warning", duration = 5)
      return()
    }
    ttest_anova_result(list(
      dependents = dependent_variables(),
      factors = factor_variables(),
      method = input$ttest_anova_method,
      options = list(
        descriptives = isTRUE(input$ttest_anova_descriptives),
        assumption_checks = isTRUE(input$ttest_anova_assumption_checks),
        effect_size = isTRUE(input$ttest_anova_effect_size)
      )
    ))
  })

  output$ttest_anova_results <- renderUI({
    result <- ttest_anova_result()
    if (is.null(result)) {
      return(empty_message("Move variables and click Run analysis."))
    }
    empty_message("t-test / ANOVA analysis engine is not implemented yet.")
  })

  invisible(TRUE)
}
