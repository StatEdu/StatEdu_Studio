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
  show_total_n = TRUE,
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
    show_total_n = isTRUE(show_total_n),
    split_count_percent = isTRUE(split_count_percent),
    trend = isTRUE(trend)
  )
}

crosstab_setup_panel <- function(state, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  div(
    class = "regression-setup-grid crosstab-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel regression-available-panel crosstab-available-panel",
      analysis_field_label_tag("Variables", crosstab_allowed_measurements(), language = language),
      analysis_transfer_listbox_input("crosstab_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls regression-transfer-controls crosstab-transfer-controls",
      actionButton(
        "crosstab_assign_col",
        ">",
        class = "btn btn-default analysis-move-button crosstab-assign-col-button",
        disabled = if (state$move_disabled) "disabled" else NULL
      ),
      actionButton(
        "crosstab_assign_row",
        ">",
        class = "btn btn-default analysis-move-button crosstab-assign-row-button",
        disabled = if (state$move_disabled) "disabled" else NULL
      )
    ),
    div(
      class = "regression-target-column crosstab-target-column",
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-dependent-panel crosstab-column-panel",
        analysis_field_label_tag("Column variable", crosstab_allowed_measurements(), language = language),
        analysis_transfer_listbox_input("crosstab_col", state$col_items, selected = state$col_selected, size = 3),
        div(
          class = "analysis-order-actions crosstab-order-actions",
          actionButton("crosstab_col_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
          actionButton("crosstab_col_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
        )
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-independent-panel crosstab-row-panel",
        analysis_field_label_tag("Row variable", crosstab_allowed_measurements(), language = language),
        analysis_transfer_listbox_input("crosstab_row", state$row_items, selected = state$row_selected, size = 9),
        div(
          class = "analysis-order-actions crosstab-order-actions",
          actionButton("crosstab_row_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
          actionButton("crosstab_row_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
        )
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel regression-options crosstab-options-panel",
      div(class = "analysis-option-title", analysis_ui_text("Design", language)),
      radioButtons(
        "crosstab_design",
        label = NULL,
        choices = stats::setNames(c("survey", "experimental"), c(analysis_ui_text("Survey study", language), analysis_ui_text("Experimental study", language))),
        selected = state$design
      ),
      div(
        class = "analysis-option-group crosstab-display-options",
        div(class = "analysis-option-title", analysis_ui_text("Display", language)),
        checkboxInput("crosstab_row_percent", statedu_utf8("ed96892025"), value = state$show_row_percent),
        checkboxInput("crosstab_column_percent", statedu_utf8("ec97b42025"), value = state$show_column_percent),
        checkboxInput("crosstab_total_percent", statedu_utf8("eca084ecb2b42025"), value = state$show_total_percent),
        checkboxInput("crosstab_total_n", statedu_utf8("eca084ecb2b4206e"), value = state$show_total_n),
        div(
          class = "crosstab-split-count-option",
          checkboxInput("crosstab_split_count_percent", statedu_utf8("6eeab3bc202520ebb684eba6ac"), value = state$split_count_percent)
        )
      ),
      analysis_option_group(
        "Statistics",
        list(
          list(id = "crosstab_trend", label = "Trend analysis", value = state$trend)
        ),
        language = language
      ),
      div(
        class = "step-summary crosstab-rule-summary",
        div(
          statedu_text(
            language,
            "Default: Pearson chi-square test",
            statedu_utf8("eab8b0ebb3b83a2050656172736f6e20ecb9b4ec9db4eca09ceab3b120eab280eca095")
          ),
          class = "step-summary-title"
        ),
        div(
          statedu_text(
            language,
            "If >= 20% of cells have expected counts < 5, Fisher exact or Monte Carlo is used.",
            statedu_utf8("eab8b0eb8c80eb8f84ec8898203c203520ec8580ec9db42032302520ec9db4ec8381ec9db4eba9b42046697368657220657861637420eb9890eb8a94204d6f6e7465204361726c6feba5bc20ec82acec9aa9ed95a9eb8b88eb8ba42e")
          ),
          class = "step-summary-detail"
        )
      )
    )
  )
}
