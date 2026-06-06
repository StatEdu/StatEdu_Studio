# Correlation setup UI state and panel.

correlation_setup_state <- function(
  selected_names,
  correlation_variables = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = character(0),
  selected_selected = character(0),
  continuous_method = "auto",
  normality = TRUE,
  latent_correlations = FALSE,
  p_ci = TRUE,
  significance_levels = TRUE,
  scatter_plot = TRUE,
  matrix_plot = TRUE
) {
  selected <- as.character(selected_names %||% character(0))
  correlation_variables <- intersect(as.character(correlation_variables %||% character(0)), selected)
  available <- setdiff(selected, correlation_variables)

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    correlation_variables = correlation_variables,
    selected_items = analysis_variable_items(correlation_variables, variable_table, labels),
    selected_selected = selected_order_items(selected_selected, correlation_variables),
    move_disabled = length(selected) == 0,
    continuous_method = correlation_continuous_method_value(continuous_method),
    normality = isTRUE(normality),
    latent_correlations = isTRUE(latent_correlations),
    p_ci = isTRUE(p_ci),
    significance_levels = isTRUE(significance_levels),
    scatter_plot = isTRUE(scatter_plot),
    matrix_plot = isTRUE(matrix_plot)
  )
}

correlation_continuous_method_choices <- function() {
  c(
    "Auto" = "auto",
    "Pearson" = "pearson",
    "Spearman" = "spearman",
    "Kendall" = "kendall"
  )
}

correlation_continuous_method_value <- function(value) {
  value <- as.character(value %||% "auto")
  choices <- unname(correlation_continuous_method_choices())
  if (value %in% choices) value else "auto"
}

correlation_setup_panel <- function(state) {
  div(
    class = "correlation-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("correlation_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls correlation-transfer-controls",
      actionButton(
        "correlation_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (isTRUE(state$move_disabled)) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Selected Variables", analysis_allowed_measurements_all()),
      analysis_transfer_listbox_input("correlation_selected", state$selected_items, selected = state$selected_selected, size = 17),
      div(
        class = "analysis-order-actions correlation-order-actions",
        actionButton("correlation_move_up", "Up", class = "btn-default btn-sm"),
        actionButton("correlation_move_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "correlation-options-column",
      div(
        class = "analysis-options-panel correlation-options",
        analysis_option_group(
          "Statistics",
          list(
            list(id = "correlation_p_ci", label = "p-value & 95% CI", value = state$p_ci),
            list(id = "correlation_significance_levels", label = "significance levels", value = state$significance_levels),
            list(id = "correlation_normality", label = "normality diagnostics", value = state$normality)
          )
        ),
        analysis_radio_group(
          "Continuous method",
          "correlation_continuous_method",
          correlation_continuous_method_choices(),
          selected = state$continuous_method
        ),
        div(
          class = "analysis-option-group",
          div(class = "analysis-option-title", "Advanced correlations"),
          checkboxInput(
            "correlation_latent_correlations",
            "Use latent-variable correlations",
            value = state$latent_correlations
          )
        ),
        analysis_option_group(
          "Plot",
          list(
            list(id = "correlation_scatter_plot", label = "scatter plot matrix", value = state$scatter_plot),
            list(id = "correlation_matrix_plot", label = "correlation matrix heatmap", value = state$matrix_plot)
          )
        )
      )
    )
  )
}
