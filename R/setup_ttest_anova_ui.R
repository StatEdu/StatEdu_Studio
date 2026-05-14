# t-test / ANOVA setup UI state and panel.

ttest_anova_method_choices <- function() {
  c(
    "Independent samples t-test" = "independent_t",
    "One-way ANOVA" = "anova",
    "Mann-Whitney U (Wilcoxon rank sum)" = "mann_whitney",
    "Kruskal-Wallis" = "kruskal_wallis"
  )
}

ttest_anova_setup_state <- function(
  selected_names,
  dependent_variables = character(0),
  factor_variables = character(0),
  variable_table = NULL,
  labels = character(0),
  method_value = NULL
) {
  selected <- as.character(selected_names %||% character(0))
  dependent_variables <- intersect(as.character(dependent_variables %||% character(0)), selected)
  factor_variables <- intersect(as.character(factor_variables %||% character(0)), selected)
  assigned <- unique(c(dependent_variables, factor_variables))
  available <- setdiff(selected, assigned)
  method_choices <- ttest_anova_method_choices()
  current_method <- as.character(method_value %||% unname(method_choices)[[1]])
  if (!current_method %in% unname(method_choices)) {
    current_method <- unname(method_choices)[[1]]
  }

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    dependent_variables = dependent_variables,
    dependent_items = analysis_variable_items(dependent_variables, variable_table, labels),
    factor_variables = factor_variables,
    factor_items = analysis_variable_items(factor_variables, variable_table, labels),
    method_choices = method_choices,
    current_method = current_method,
    move_disabled = length(selected) == 0
  )
}

ttest_anova_setup_panel <- function(state) {
  div(
    class = "ttest-anova-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      div(class = "analysis-field-label", "Variables"),
      analysis_transfer_listbox_input("ttest_available", state$available_items, size = 20)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls",
      div(
        class = "ttest-anova-transfer-button-group",
        actionButton(
          "ttest_add_dependent",
          ">",
          class = "btn btn-default analysis-move-button",
          disabled = if (isTRUE(state$move_disabled)) "disabled" else NULL
        ),
        actionButton(
          "ttest_remove_dependent",
          "<",
          class = "btn btn-default analysis-move-button",
          disabled = if (length(state$dependent_variables) == 0) "disabled" else NULL
        )
      ),
      div(
        class = "ttest-anova-transfer-button-group",
        actionButton(
          "ttest_add_factor",
          ">",
          class = "btn btn-default analysis-move-button",
          disabled = if (isTRUE(state$move_disabled)) "disabled" else NULL
        ),
        actionButton(
          "ttest_remove_factor",
          "<",
          class = "btn btn-default analysis-move-button",
          disabled = if (length(state$factor_variables) == 0) "disabled" else NULL
        )
      )
    ),
    div(
      class = "ttest-anova-target-column",
      div(
        class = "analysis-transfer-column analysis-transfer-panel ttest-anova-dependent-panel",
        div(class = "analysis-field-label", "Dependent Variables"),
        analysis_transfer_listbox_input("ttest_dependents", state$dependent_items, size = 8)
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel ttest-anova-factor-panel",
        div(class = "analysis-field-label", "Grouping Variables"),
        analysis_transfer_listbox_input("ttest_factors", state$factor_items, size = 12)
      )
    ),
    div(
      class = "ttest-anova-options-column",
      div(
        class = "analysis-options-panel ttest-anova-method-panel",
        div(
          class = "analysis-option-group",
          div(class = "analysis-option-title", "Analysis"),
          selectInput(
            "ttest_anova_method",
            NULL,
            choices = state$method_choices,
            selected = state$current_method,
            selectize = FALSE
          )
        )
      ),
      div(
        class = "analysis-options-panel ttest-anova-options",
        analysis_option_group(
          "Options",
          list(
            list(id = "ttest_anova_descriptives", label = "Descriptives", value = TRUE),
            list(id = "ttest_anova_assumption_checks", label = "Assumption checks", value = TRUE),
            list(id = "ttest_anova_effect_size", label = "Effect size", value = TRUE)
          )
        )
      )
    )
  )
}
