variable_table_display_data <- function(
  info,
  checked_names = character(0),
  selected_names = character(0),
  assigned_elsewhere = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  selection_applied = FALSE,
  active_role = "dependent",
  measurement_overrides = character(0),
  language = statedu_initial_language()
) {
  table_data <- apply_measurement_overrides(info, measurement_overrides)
  if (isTRUE(selection_applied)) {
    visible_names <- setdiff(as.character(selected_names), as.character(assigned_elsewhere))
    table_data <- table_data[table_data$name %in% unique(c(checked_names, visible_names)), , drop = FALSE]
  }
  table_data$role <- vapply(
    table_data$name,
    role_for_variable,
    character(1),
    dependent = dependent,
    independent = independent,
    controls = controls
  )
  table_data <- table_data[, c("source_order", "name", "var_label", "role", "measurement", "storage_type", "n_unique", "n_missing", "min_value", "max_value"), drop = FALSE]
  disabled_names <- if (isTRUE(selection_applied) && identical(active_role, "dependent")) {
    setdiff(table_data$name[table_data$measurement != "continuous"], checked_names)
  } else {
    character(0)
  }
  table_data$measurement <- mapply(
    measurement_select_html,
    table_data$name,
    table_data$measurement,
    table_data$source_order,
    MoreArgs = list(language = language),
    USE.NAMES = FALSE
  )
  cbind(
    selected = sprintf(
      '<input type="checkbox" class="variable-select" data-name="%s" %s %s>',
      htmltools::htmlEscape(table_data$name),
      ifelse(table_data$name %in% checked_names, "checked", ""),
      ifelse(table_data$name %in% disabled_names, "disabled title=\"Dependent variable must be continuous\"", "")
    ),
    table_data,
    stringsAsFactors = FALSE
  )
}

variable_table_render_state <- function(
  info,
  checked_names = character(0),
  selected_names = character(0),
  assigned_elsewhere = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  selection_applied = FALSE,
  active_role = "dependent",
  measurement_overrides = character(0),
  language = statedu_initial_language()
) {
  list(
    checked_names = checked_names,
    table_data = variable_table_display_data(
      info,
      checked_names = checked_names,
      selected_names = selected_names,
      assigned_elsewhere = assigned_elsewhere,
      dependent = dependent,
      independent = independent,
      controls = controls,
      selection_applied = selection_applied,
      active_role = active_role,
      measurement_overrides = measurement_overrides,
      language = language
    )
  )
}

