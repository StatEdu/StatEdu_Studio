data_tab_panel <- function(language = statedu_initial_language()) {
  h <- statedu_utf8
  tabPanel(
    statedu_ui_label("data", language),
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("StatEdu Studio"),
        div(statedu_text(language, "SPSS/SAS/Stata, Excel, CSV, and DAT files can be loaded and summarized before analysis.", h("535053532f5341532f53746174612c20457863656c2c204353562c2044415420ed8c8cec9dbcec9d8420ebb688eb9facec98a4eab3a020ebb684ec849d20eca08420ec9a94ec95bded95a020ec889820ec9e88ec8ab5eb8b88eb8ba42e")), class = "app-subtitle")
      ),
      div(
        class = "data-layout",
        div(
          class = "side-panel",
          uiOutput("data_steps")
        ),
        div(
          class = "workspace-panel",
          div(class = "load-message", uiOutput("data_loaded_message", inline = TRUE)),
          conditionalPanel(
            condition = "output.data_excel_pending === true",
            div(
              class = "excel-import-main-panel",
              div(
                class = "workspace-header",
                h3(statedu_text(language, "Excel Import Review", h("457863656c20ebb688eb9facec98a4eab8b020eab280ed86a0")))
              ),
              div(class = "excel-import-note", uiOutput("excel_import_note", inline = TRUE)),
              div(class = "excel-import-preview-wrap excel-import-preview-main", DTOutput("excel_import_preview"))
            )
          ),
          conditionalPanel(
            condition = "output.data_excel_pending !== true",
            tagList(
              div(
                class = "workspace-header",
                h3(uiOutput("data_view_title", inline = TRUE)),
                uiOutput("data_view_toggle")
              ),
              conditionalPanel(
                condition = "output.data_view === 'info'",
                DTOutput("variable_table")
              ),
              conditionalPanel(
                condition = "output.data_view === 'preview'",
                DTOutput("data_preview_table")
              ),
              conditionalPanel(
                condition = "output.data_view === 'labels'",
                tagList(
                  div(
                    class = "data-table-section step3-labels-section",
                    h4(statedu_text(language, "Categorical value labels", h("ebb294eca3bced989520eab09220eb9dbcebb2a8"))),
                    DTOutput("category_label_table")
                  ),
                  div(
                    class = "data-table-section step3-variables-section",
                    style = "display: none;",
                    h4(statedu_text(language, "Selected variables", h("ec84a0ed839d20ebb380ec8898"))),
                    DTOutput("selected_variable_edit_table")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
