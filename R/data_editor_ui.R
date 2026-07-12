# Data editor menu and command panels.

data_editor_command_panel <- function(title, subtitle, body_title, body_text) {
  div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1(title),
      div(subtitle, class = "app-subtitle")
    ),
    div(
      class = "workspace-panel frequencies-workspace-panel data-editor-workspace",
      h3(body_title),
      div(class = "empty-message", div(body_text))
    )
  )
}

data_editor_same_variable_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.recode_title", language),
    value = "data_editor_recode_same",
    data_editor_same_variable_panel(language)
  )
}

data_editor_likert_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.likert_title", language),
    value = "data_editor_likert",
    data_editor_likert_panel(language)
  )
}

data_editor_different_variable_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.reverse_title", language),
    value = "data_editor_recode_different",
    data_editor_different_variable_panel(language)
  )
}

data_editor_coding_error_check_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.coding_error_title", language),
    value = "data_editor_coding_error_check",
    data_editor_coding_error_check_panel(language)
  )
}

data_editor_variable_calculation_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.calculation_title", language),
    value = "data_editor_variable_calculation",
    data_editor_variable_calculation_panel(language)
  )
}

data_editor_variable_transformation_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.transform_title", language),
    value = "data_editor_variable_transformation",
    data_editor_variable_transformation_panel(language)
  )
}

data_editor_missing_values_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.missing_title", language),
    value = "data_editor_missing_values",
    data_editor_missing_panel(language)
  )
}

data_editor_wide_long_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.wide_long_title", language),
    value = "data_editor_wide_long",
    data_editor_wide_long_panel(language)
  )
}

data_editor_merge_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    merge_text(language, "Merge", statedu_utf8("eb8db0ec9db4ed84b020ebb391ed95a9")),
    value = "data_editor_merge",
    data_editor_merge_panel(language)
  )
}

data_editor_id_aggregate_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    id_aggregate_text(language, "ID aggregation", statedu_utf8("494420eca791eab384")),
    value = "data_editor_id_aggregate",
    data_editor_id_aggregate_panel(language)
  )
}

data_editor_variable_rename_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_t("data_editor.rename_title", language),
    value = "data_editor_variable_rename",
    data_editor_variable_rename_panel(language)
  )
}

data_editor_tab_panel <- function(language = statedu_initial_language()) {
  navbarMenu(
    statedu_ui_label("data_editor", language),
    lazy_tab_panel(statedu_t("data_editor.coding_error_title", language), "data_editor_coding_error_check", "lazy_data_editor_coding_error_check"),
    lazy_tab_panel(statedu_t("data_editor.likert_title", language), "data_editor_likert", "lazy_data_editor_likert"),
    lazy_tab_panel(statedu_t("data_editor.missing_title", language), "data_editor_missing_values", "lazy_data_editor_missing_values"),
    lazy_tab_panel(statedu_t("data_editor.wide_long_title", language), "data_editor_wide_long", "lazy_data_editor_wide_long"),
    lazy_tab_panel(merge_text(language, "Merge", statedu_utf8("eb8db0ec9db4ed84b020ebb391ed95a9")), "data_editor_merge", "lazy_data_editor_merge"),
    lazy_tab_panel(id_aggregate_text(language, "ID aggregation", statedu_utf8("494420eca791eab384")), "data_editor_id_aggregate", "lazy_data_editor_id_aggregate"),
    lazy_tab_panel(statedu_t("data_editor.reverse_title", language), "data_editor_recode_different", "lazy_data_editor_recode_different"),
    lazy_tab_panel(statedu_t("data_editor.calculation_title", language), "data_editor_variable_calculation", "lazy_data_editor_variable_calculation"),
    lazy_tab_panel(statedu_t("data_editor.transform_title", language), "data_editor_variable_transformation", "lazy_data_editor_variable_transformation"),
    lazy_tab_panel(statedu_t("data_editor.recode_title", language), "data_editor_recode_same", "lazy_data_editor_recode_same"),
    lazy_tab_panel(statedu_t("data_editor.rename_title", language), "data_editor_variable_rename", "lazy_data_editor_variable_rename")
  )
}
