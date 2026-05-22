# Data editor menu and command placeholders.

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
    "Recode same variable",
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

data_editor_missing_values_tab_panel <- function() {
  tabPanel(
    "Auto missing values",
    value = "data_editor_missing_values",
    data_editor_missing_panel()
  )
}

data_editor_tab_panel <- function() {
  navbarMenu(
    "Data Editor",
    data_editor_likert_tab_panel(),
    data_editor_same_variable_tab_panel(),
    data_editor_coding_error_check_tab_panel(),
    data_editor_different_variable_tab_panel(),
    data_editor_variable_calculation_tab_panel(),
    data_editor_missing_values_tab_panel()
  )
}
