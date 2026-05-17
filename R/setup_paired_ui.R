# Paired test setup UI.

paired_setup_state <- function(
  selected_names,
  first_variables = character(0),
  second_variables = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_first = NULL,
  selected_second = NULL,
  assumption_check = FALSE,
  bowker = FALSE
) {
  selected <- as.character(selected_names %||% character(0))
  first_variables <- intersect(as.character(first_variables %||% character(0)), selected)
  second_variables <- intersect(as.character(second_variables %||% character(0)), selected)
  available <- setdiff(selected, unique(c(first_variables, second_variables)))
  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    first_variables = first_variables,
    first_items = analysis_variable_items(first_variables, variable_table, labels),
    first_selected = selected_order_items(selected_first, first_variables),
    second_variables = second_variables,
    second_items = analysis_variable_items(second_variables, variable_table, labels),
    second_selected = selected_order_items(selected_second, second_variables),
    assumption_check = isTRUE(assumption_check),
    bowker = isTRUE(bowker),
    move_disabled = length(selected) == 0
  )
}

paired_setup_panel <- function(state) {
  div(
    class = "ttest-anova-setup-grid paired-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("paired_available", state$available_items, selected = state$available_selected, size = 20)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls paired-transfer-controls",
      actionButton("paired_first_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && length(state$first_variables) == 0) "disabled" else NULL),
      actionButton("paired_second_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && length(state$second_variables) == 0) "disabled" else NULL)
    ),
    div(
      class = "ttest-anova-target-column paired-target-column",
      div(
        class = "analysis-transfer-column analysis-transfer-panel paired-first-panel",
        analysis_field_label_tag("Time 1 Variables", c("binary", "category", "ordered", "continuous")),
        analysis_transfer_listbox_input("paired_first", state$first_items, selected = state$first_selected, size = 8),
        div(class = "analysis-order-actions paired-order-actions", actionButton("paired_first_up", "Up", class = "btn-default btn-sm"), actionButton("paired_first_down", "Down", class = "btn-default btn-sm"))
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel paired-second-panel",
        analysis_field_label_tag("Time 2 Variables", c("binary", "category", "ordered", "continuous")),
        analysis_transfer_listbox_input("paired_second", state$second_items, selected = state$second_selected, size = 8),
        div(class = "analysis-order-actions paired-order-actions", actionButton("paired_second_up", "Up", class = "btn-default btn-sm"), actionButton("paired_second_down", "Down", class = "btn-default btn-sm"))
      )
    ),
    div(
      class = "ttest-anova-options-column",
      div(
        class = "analysis-options-panel ttest-anova-options paired-options",
        analysis_option_group(
          "Assumption",
          list(list(id = "paired_assumption_check", label = "Check assumptions", value = state$assumption_check))
        ),
        analysis_option_group(
          "Categorical",
          list(list(id = "paired_bowker", label = "Bowker symmetry test", value = state$bowker))
        )
      )
    )
  )
}
