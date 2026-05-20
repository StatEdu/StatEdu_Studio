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

data_editor_different_variable_tab_panel <- function() {
  tabPanel(
    "Recode different variable",
    value = "data_editor_recode_different",
    data_editor_command_panel(
      "Recode into Different Variables",
      "Create new variables from recoded values while preserving the original variables.",
      "Different-variable recoding",
      "This command will create new recoded variables from selected source variables."
    )
  )
}

data_editor_missing_values_tab_panel <- function() {
  tabPanel(
    "Missing values",
    value = "data_editor_missing_values",
    data_editor_command_panel(
      "Missing Values",
      "Define user-missing values for variables before running analyses.",
      "Missing value definition",
      "This command will define values that analyses should treat as missing."
    )
  )
}

data_editor_tab_panel <- function() {
  navbarMenu(
    "Data Editor",
    data_editor_same_variable_tab_panel(),
    data_editor_different_variable_tab_panel(),
    data_editor_missing_values_tab_panel()
  )
}
