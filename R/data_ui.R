data_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    statedu_ui_label("data", language),
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("StatEdu Studio"),
        div(statedu_t("data.subtitle", language), class = "app-subtitle")
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
                h3(statedu_t("data.excel_import_review", language))
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
                    h4(statedu_t("data.categorical_value_labels", language)),
                    DTOutput("category_label_table")
                  ),
                  div(
                    class = "data-table-section step3-variables-section",
                    style = "display: none;",
                    h4(statedu_t("data.selected_variables", language)),
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
