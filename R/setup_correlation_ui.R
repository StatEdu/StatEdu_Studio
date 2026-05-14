# Correlation setup UI state and panel.

correlation_setup_state <- function(
  selected_names,
  correlation_variables = character(0),
  variable_table = NULL,
  labels = character(0)
) {
  selected <- as.character(selected_names %||% character(0))
  correlation_variables <- intersect(as.character(correlation_variables %||% character(0)), selected)
  available <- setdiff(selected, correlation_variables)

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    correlation_variables = correlation_variables,
    selected_items = analysis_variable_items(correlation_variables, variable_table, labels),
    move_disabled = length(selected) == 0
  )
}

correlation_setup_panel <- function(state) {
  div(
    class = "correlation-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      div(class = "analysis-field-label", "Variables"),
      analysis_transfer_listbox_input("correlation_available", state$available_items, size = 20)
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
      div(class = "analysis-field-label", "Selected Variables"),
      analysis_transfer_listbox_input("correlation_selected", state$selected_items, size = 20)
    ),
    div(
      class = "correlation-options-column",
      div(
        class = "analysis-options-panel correlation-method-panel",
        analysis_option_group(
          "Correlation coefficient",
          list(
            list(id = "correlation_pearson", label = "Pearson", value = TRUE),
            list(id = "correlation_spearman", label = "Spearman", value = FALSE),
            list(id = "correlation_kendall", label = "Kendall's tau-b", value = FALSE)
          )
        )
      ),
      div(
        class = "analysis-options-panel correlation-options",
        analysis_option_group(
          "Options",
          list(
            list(id = "correlation_p_value", label = "p-value", value = TRUE),
            list(id = "correlation_n", label = "Sample size (N)", value = TRUE)
          )
        )
      )
    )
  )
}
