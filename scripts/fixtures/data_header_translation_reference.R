data_table_header_labels <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  labels <- c(
    "selected" = statedu_t("data.table_selected", language),
    "source_order" = statedu_t("data.table_source_order", language),
    "name" = statedu_t("data.table_name", language),
    "var_label" = statedu_t("data.table_var_label", language),
    "role" = statedu_t("data.table_role", language),
    "measurement" = statedu_t("data.table_measurement", language),
    "storage_type" = statedu_t("data.table_storage_type", language),
    "n_unique" = statedu_t("data.table_n_unique", language),
    "n_missing" = statedu_t("data.table_n_missing", language),
    "min_value" = statedu_t("data.table_min_value", language),
    "max_value" = statedu_t("data.table_max_value", language),
    "Variable" = statedu_t("data.table_variable", language),
    "Label" = statedu_t("data.table_label", language),
    "Measurement" = statedu_t("data.table_measurement_title", language),
    "Reference" = statedu_t("data.table_reference_title", language),
    "Min" = statedu_t("data.table_min", language),
    "Max" = statedu_t("data.table_max", language),
    "Missing" = statedu_t("data.table_missing", language),
    "Message" = statedu_t("data.table_message", language),
    "reference" = statedu_t("data.table_reference", language),
    "reference_label" = statedu_t("data.table_reference_label", language)
  )
  for (index in seq_len(11)) {
    labels[[paste0("value_", index)]] <- paste(statedu_t("data.table_value", language), index)
    labels[[paste0("label_", index)]] <- paste(statedu_t("data.table_value_label", language), index)
  }
  labels
}

data_table_colnames <- function(columns, language = statedu_initial_language()) {
  labels <- data_table_header_labels(language)
  vapply(as.character(columns), function(column) {
    if (column %in% names(labels)) labels[[column]] else column
  }, character(1), USE.NAMES = FALSE)
}

