# Cross-tabulation setup UI state and panel.

crosstab_selected_variables <- function(value, allowed) {
  value <- as.character(value %||% character(0))
  value <- value[nzchar(value)]
  intersect(value, allowed)
}

crosstab_setup_state <- function(
  selected_names,
  variable_table = NULL,
  labels = character(0),
  selected_available = character(0),
  selected_row = character(0),
  selected_col = character(0),
  row_var = "",
  col_var = "",
  design = "survey",
  show_row_percent = NULL,
  show_column_percent = NULL,
  show_total_percent = FALSE,
  split_count_percent = FALSE,
  trend = FALSE
) {
  allowed <- analysis_allowed_variables(selected_names, variable_table, crosstab_allowed_measurements())
  row_vars <- crosstab_selected_variables(row_var, allowed)
  col_vars <- crosstab_selected_variables(col_var, allowed)
  assigned <- unique(c(row_vars, col_vars))
  assigned <- assigned[nzchar(assigned)]
  available <- setdiff(allowed, assigned)
  available_items <- analysis_variable_items(available, variable_table, labels)
  row_items <- analysis_variable_items(row_vars, variable_table, labels)
  col_items <- analysis_variable_items(col_vars, variable_table, labels)
  design <- as.character(design %||% "survey")
  if (!design %in% c("survey", "experimental")) design <- "survey"
  if (is.null(show_row_percent)) show_row_percent <- identical(design, "survey")
  if (is.null(show_column_percent)) show_column_percent <- identical(design, "experimental")

  list(
    available = available,
    available_items = available_items,
    available_selected = selected_order_items(selected_available, available),
    row_vars = row_vars,
    col_vars = col_vars,
    row_items = row_items,
    col_items = col_items,
    row_selected = selected_order_items(selected_row, row_vars),
    col_selected = selected_order_items(selected_col, col_vars),
    move_disabled = length(allowed) == 0,
    design = design,
    show_row_percent = isTRUE(show_row_percent),
    show_column_percent = isTRUE(show_column_percent),
    show_total_percent = isTRUE(show_total_percent),
    split_count_percent = isTRUE(split_count_percent),
    trend = isTRUE(trend)
  )
}

crosstab_setup_panel <- function(state) {
  div(
    class = "regression-setup-grid crosstab-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel regression-available-panel crosstab-available-panel",
      analysis_field_label_tag("Variables", crosstab_allowed_measurements()),
      analysis_transfer_listbox_input("crosstab_available", state$available_items, selected = state$available_selected, size = 19)
    ),
    div(
      class = "analysis-transfer-controls regression-transfer-controls crosstab-transfer-controls",
      actionButton(
        "crosstab_assign_row",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (state$move_disabled) "disabled" else NULL
      ),
      actionButton(
        "crosstab_assign_col",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (state$move_disabled) "disabled" else NULL
      )
    ),
    div(
      class = "regression-target-column crosstab-target-column",
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-dependent-panel crosstab-row-panel",
        analysis_field_label_tag("Row variable", crosstab_allowed_measurements()),
        analysis_transfer_listbox_input("crosstab_row", state$row_items, selected = state$row_selected, size = 7),
        div(
          class = "analysis-order-actions crosstab-order-actions",
          actionButton("crosstab_row_up", "Up", class = "btn-default btn-sm"),
          actionButton("crosstab_row_down", "Down", class = "btn-default btn-sm")
        )
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-independent-panel crosstab-column-panel",
        analysis_field_label_tag("Column variable", crosstab_allowed_measurements()),
        analysis_transfer_listbox_input("crosstab_col", state$col_items, selected = state$col_selected, size = 7),
        div(
          class = "analysis-order-actions crosstab-order-actions",
          actionButton("crosstab_col_up", "Up", class = "btn-default btn-sm"),
          actionButton("crosstab_col_down", "Down", class = "btn-default btn-sm")
        )
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel regression-options crosstab-options-panel",
      div(class = "analysis-option-title", "Design"),
      radioButtons(
        "crosstab_design",
        label = NULL,
        choices = c("Survey" = "survey", "Experimental" = "experimental"),
        selected = state$design
      ),
      analysis_option_group(
        "Display",
        list(
          list(id = "crosstab_row_percent", label = "row %", value = state$show_row_percent),
          list(id = "crosstab_column_percent", label = "column %", value = state$show_column_percent),
          list(id = "crosstab_total_percent", label = "total %", value = state$show_total_percent),
          list(id = "crosstab_split_count_percent", label = "separate n and %", value = state$split_count_percent)
        )
      ),
      analysis_option_group(
        "Statistics",
        list(
          list(id = "crosstab_trend", label = "Trend analysis", value = state$trend)
        )
      ),
      div(
        class = "step-summary crosstab-rule-summary",
        div("Pearson chi-square is used by default.", class = "step-summary-title"),
        div("If expected counts < 5 in 20% or more cells, Fisher exact or Monte Carlo simulation is used.", class = "step-summary-detail")
      )
    )
  )
}
