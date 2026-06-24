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

data_editor_same_variable_tab_panel <- function() {
  tabPanel(
    "Recode variable",
    value = "data_editor_recode_same",
    data_editor_same_variable_panel()
  )
}

data_editor_likert_tab_panel <- function() {
  tabPanel(
    "Auto Likert conversion",
    value = "data_editor_likert",
    data_editor_likert_panel()
  )
}

data_editor_different_variable_tab_panel <- function() {
  tabPanel(
    "Auto reverse coding",
    value = "data_editor_recode_different",
    data_editor_different_variable_panel()
  )
}

data_editor_coding_error_check_tab_panel <- function() {
  tabPanel(
    "Auto coding error check",
    value = "data_editor_coding_error_check",
    data_editor_coding_error_check_panel()
  )
}

data_editor_variable_calculation_tab_panel <- function() {
  tabPanel(
    "Auto variable calculation",
    value = "data_editor_variable_calculation",
    data_editor_variable_calculation_panel()
  )
}

data_editor_variable_transformation_tab_panel <- function() {
  tabPanel(
    "Variable transformation",
    value = "data_editor_variable_transformation",
    data_editor_variable_transformation_panel()
  )
}

data_editor_missing_values_tab_panel <- function() {
  tabPanel(
    "Auto missing values",
    value = "data_editor_missing_values",
    data_editor_missing_panel()
  )
}

data_editor_wide_long_tab_panel <- function() {
  tabPanel(
    "Wide to Long",
    value = "data_editor_wide_long",
    data_editor_wide_long_panel()
  )
}

data_editor_variable_rename_tab_panel <- function() {
  tabPanel(
    "Rename variable",
    value = "data_editor_variable_rename",
    data_editor_variable_rename_panel()
  )
}

data_editor_tab_panel <- function() {
  navbarMenu(
    "Data Editor",
    lazy_tab_panel("Auto coding error check", "data_editor_coding_error_check", "lazy_data_editor_coding_error_check"),
    lazy_tab_panel("Auto Likert conversion", "data_editor_likert", "lazy_data_editor_likert"),
    lazy_tab_panel("Auto missing values", "data_editor_missing_values", "lazy_data_editor_missing_values"),
    lazy_tab_panel("Wide to Long", "data_editor_wide_long", "lazy_data_editor_wide_long"),
    lazy_tab_panel("Auto reverse coding", "data_editor_recode_different", "lazy_data_editor_recode_different"),
    lazy_tab_panel("Auto variable calculation", "data_editor_variable_calculation", "lazy_data_editor_variable_calculation"),
    lazy_tab_panel("Variable transformation", "data_editor_variable_transformation", "lazy_data_editor_variable_transformation"),
    lazy_tab_panel("Recode variable", "data_editor_recode_same", "lazy_data_editor_recode_same"),
    lazy_tab_panel("Rename variable", "data_editor_variable_rename", "lazy_data_editor_variable_rename")
  )
}
