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
        div(
          class = "result-toolbar",
          analysis_save_button("save_result_collection_html_dialog", "Save HTML", "html", class = "btn-default"),
          analysis_save_button("save_result_collection_pdf_dialog", "Save PDF", "pdf", class = "btn-default"),
          analysis_save_button("save_result_collection_excel_dialog", "Save Excel", "excel", class = "btn-default"),
          analysis_save_button("save_result_collection_word_dialog", "Save Word", "word", class = "btn-default"),
          actionButton("clear_saved_results", "Clear results", class = "btn-default")
        ),
        uiOutput("saved_results_list")
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
