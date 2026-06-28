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
    statedu_text(language, "Recode variable", statedu_utf8("ebb380ec889820eba6acecbd94eb94a9")),
    value = "data_editor_recode_same",
    data_editor_same_variable_panel(language)
  )
}

data_editor_likert_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_text(language, "Auto Likert conversion", statedu_utf8("4c696b65727420ec9e90eb8f9920ebb380ed9998")),
    value = "data_editor_likert",
    data_editor_likert_panel(language)
  )
}

data_editor_different_variable_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_text(language, "Auto reverse coding", statedu_utf8("ec97adecbd94eb94a920ec9e90eb8f9920ecb298eba6ac")),
    value = "data_editor_recode_different",
    data_editor_different_variable_panel(language)
  )
}

data_editor_coding_error_check_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_text(language, "Auto coding error check", statedu_utf8("ec9e90eb8f9920ecbd94eb94a920ec98a4eba59820ed9995ec9db8")),
    value = "data_editor_coding_error_check",
    data_editor_coding_error_check_panel(language)
  )
}

data_editor_variable_calculation_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_text(language, "Auto variable calculation", statedu_utf8("ebb380ec889820ec9e90eb8f9920eab384ec82b0")),
    value = "data_editor_variable_calculation",
    data_editor_variable_calculation_panel(language)
  )
}

data_editor_variable_transformation_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_text(language, "Variable transformation", statedu_utf8("ebb380ec889820ebb380ed9998")),
    value = "data_editor_variable_transformation",
    data_editor_variable_transformation_panel(language)
  )
}

data_editor_missing_values_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_text(language, "Auto missing values", statedu_utf8("eab2b0ecb8a1eab09220ec9e90eb8f9920ecb298eba6ac")),
    value = "data_editor_missing_values",
    data_editor_missing_panel(language)
  )
}

data_editor_wide_long_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_text(language, "Wide to Long", statedu_utf8("ec9980ec9db4eb939c2deba1b120ebb380ed9998")),
    value = "data_editor_wide_long",
    data_editor_wide_long_panel(language)
  )
}

data_editor_variable_rename_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_text(language, "Rename variable", statedu_utf8("ebb380ec8898ebaa8520ebb380eab2bd")),
    value = "data_editor_variable_rename",
    data_editor_variable_rename_panel(language)
  )
}

data_editor_tab_panel <- function(language = statedu_initial_language()) {
  h <- statedu_utf8
  navbarMenu(
    statedu_ui_label("data_editor", language),
    lazy_tab_panel(statedu_text(language, "Auto coding error check", h("ec9e90eb8f9920ecbd94eb94a920ec98a4eba59820ed9995ec9db8")), "data_editor_coding_error_check", "lazy_data_editor_coding_error_check"),
    lazy_tab_panel(statedu_text(language, "Auto Likert conversion", h("4c696b65727420ec9e90eb8f9920ebb380ed9998")), "data_editor_likert", "lazy_data_editor_likert"),
    lazy_tab_panel(statedu_text(language, "Auto missing values", h("eab2b0ecb8a1eab09220ec9e90eb8f9920ecb298eba6ac")), "data_editor_missing_values", "lazy_data_editor_missing_values"),
    lazy_tab_panel(statedu_text(language, "Wide to Long", h("ec9980ec9db4eb939c2deba1b120ebb380ed9998")), "data_editor_wide_long", "lazy_data_editor_wide_long"),
    lazy_tab_panel(statedu_text(language, "Auto reverse coding", h("ec97adecbd94eb94a920ec9e90eb8f9920ecb298eba6ac")), "data_editor_recode_different", "lazy_data_editor_recode_different"),
    lazy_tab_panel(statedu_text(language, "Auto variable calculation", h("ebb380ec889820ec9e90eb8f9920eab384ec82b0")), "data_editor_variable_calculation", "lazy_data_editor_variable_calculation"),
    lazy_tab_panel(statedu_text(language, "Variable transformation", h("ebb380ec889820ebb380ed9998")), "data_editor_variable_transformation", "lazy_data_editor_variable_transformation"),
    lazy_tab_panel(statedu_text(language, "Recode variable", h("ebb380ec889820eba6acecbd94eb94a9")), "data_editor_recode_same", "lazy_data_editor_recode_same"),
    lazy_tab_panel(statedu_text(language, "Rename variable", h("ebb380ec8898ebaa8520ebb380eab2bd")), "data_editor_variable_rename", "lazy_data_editor_variable_rename")
  )
}
