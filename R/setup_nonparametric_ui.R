# Nonparametric tests setup UI state and panel.

nonparametric_setup_state <- function(
  selected_names,
  dependent_variables = character(0),
  factor_variables = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_dependent = NULL,
  selected_factor = NULL,
  trend_analysis = FALSE,
  nonparametric_post_hoc_method = NULL,
  ordered_significance = FALSE,
  effect_size = TRUE,
  median_iqr = FALSE
) {
  selected <- as.character(selected_names %||% character(0))
  dependent_variables <- intersect(as.character(dependent_variables %||% character(0)), selected)
  factor_variables <- intersect(as.character(factor_variables %||% character(0)), selected)
  assigned <- unique(c(dependent_variables, factor_variables))
  available <- setdiff(selected, assigned)
  dependent_allowed <- analysis_allowed_variables(selected, variable_table, c("ordered", "continuous"))
  factor_allowed <- analysis_allowed_variables(selected, variable_table, c("binary", "category", "ordered"))
  post_hoc_choices <- c(
    "Bonferroni correction" = "bonferroni",
    "Holm Bonferroni" = "holm"
  )
  current_post_hoc <- as.character(nonparametric_post_hoc_method %||% "bonferroni")
  if (!current_post_hoc %in% unname(post_hoc_choices)) {
    current_post_hoc <- "bonferroni"
  }

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    dependent_allowed = dependent_allowed,
    factor_allowed = factor_allowed,
    dependent_variables = dependent_variables,
    dependent_items = analysis_variable_items(dependent_variables, variable_table, labels),
    dependent_selected = selected_order_items(selected_dependent, dependent_variables),
    factor_variables = factor_variables,
    factor_items = analysis_variable_items(factor_variables, variable_table, labels),
    factor_selected = selected_order_items(selected_factor, factor_variables),
    nonparametric_post_hoc_choices = post_hoc_choices,
    nonparametric_post_hoc_method = current_post_hoc,
    trend_analysis = isTRUE(trend_analysis),
    ordered_significance = isTRUE(ordered_significance),
    effect_size = isTRUE(effect_size),
    median_iqr = isTRUE(median_iqr),
    move_disabled = length(selected) == 0
  )
}

nonparametric_setup_panel <- function(state) {
  div(
    class = "ttest-anova-setup-grid nonparametric-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("nonparametric_available", state$available_items, selected = state$available_selected, size = 19)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls",
      actionButton(
        "nonparametric_dependent_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (isTRUE(state$move_disabled) && length(state$dependent_variables) == 0) "disabled" else NULL
      ),
      actionButton(
        "nonparametric_factor_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (isTRUE(state$move_disabled) && length(state$factor_variables) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "ttest-anova-target-column",
      div(
        class = "analysis-transfer-column analysis-transfer-panel ttest-anova-dependent-panel",
        analysis_field_label_tag("Dependent Variables", c("ordered", "continuous")),
        analysis_transfer_listbox_input("nonparametric_dependents", state$dependent_items, selected = state$dependent_selected, size = 4),
        div(
          class = "analysis-order-actions ttest-anova-order-actions",
          actionButton("nonparametric_dependent_up", "Up", class = "btn-default btn-sm"),
          actionButton("nonparametric_dependent_down", "Down", class = "btn-default btn-sm")
        )
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel ttest-anova-factor-panel",
        analysis_field_label_tag("Grouping Variables", c("binary", "category", "ordered")),
        analysis_transfer_listbox_input("nonparametric_factors", state$factor_items, selected = state$factor_selected, size = 10),
        div(
          class = "analysis-order-actions ttest-anova-order-actions",
          actionButton("nonparametric_factor_up", "Up", class = "btn-default btn-sm"),
          actionButton("nonparametric_factor_down", "Down", class = "btn-default btn-sm")
        )
      )
    ),
    div(
      class = "ttest-anova-options-column",
      div(
        class = "analysis-options-panel ttest-anova-options nonparametric-options",
        div(
          class = "analysis-option-group analysis-radio-group",
          div(class = "analysis-option-title", "Post-hoc"),
          div(
            class = "ttest-anova-ordered-significance-option",
            checkboxInput(
              "nonparametric_ordered_significance",
              "Ordered significance notation",
              value = state$ordered_significance
            )
          ),
          radioButtons(
            "nonparametric_post_hoc_method",
            label = NULL,
            choices = state$nonparametric_post_hoc_choices,
            selected = state$nonparametric_post_hoc_method
          )
        ),
        analysis_option_group(
          "Options",
          list(
            list(id = "nonparametric_trend_analysis", label = "Trend analysis", value = state$trend_analysis),
            list(id = "nonparametric_effect_size", label = "Effect size", value = state$effect_size),
            list(id = "nonparametric_median_iqr", label = "Median(Q1~Q3)", value = state$median_iqr)
          )
        )
      )
    )
  )
}
