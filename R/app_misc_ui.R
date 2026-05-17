# Result and About menu placeholders.

result_tab_panel <- function() {
  tabPanel(
    "Result",
    value = "result",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Result"),
        div("Review and manage saved analysis results.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3("Result"),
        div(class = "empty-message", div("Result management will be collected here."))
      )
    )
  )
}

about_tab_panel <- function() {
  tabPanel(
    "About",
    value = "about",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("About"),
        div("EasyFlow Statistics application information.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3("EasyFlow Statistics"),
        div(class = "empty-message", div("Version, citation, and application information will be shown here."))
      )
    )
  )
}
