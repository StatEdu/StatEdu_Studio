data_tab_panel <- function() {
  tabPanel(
    "Data",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("EasyFlow Statistics"),
        div("SPSS SAV, CSV, DAT files can be loaded and summarized before regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "data-layout",
        div(
          class = "side-panel",
          uiOutput("data_steps")
        ),
        div(
          class = "workspace-panel",
          div(class = "load-message", textOutput("data_loaded_message")),
          div(
            class = "workspace-header",
            h3(textOutput("data_view_title")),
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
            condition = "output.data_view === 'labels' && input.step3_label_view === 'variables'",
            div(
              class = "data-table-section",
              h4("Selected variables"),
              DTOutput("selected_variable_edit_table")
            )
          ),
          conditionalPanel(
            condition = "output.data_view === 'labels' && (!input.step3_label_view || input.step3_label_view === 'labels')",
            div(
              class = "data-table-section",
              h4("Categorical value labels"),
              DTOutput("category_label_table")
            )
          )
        )
      )
    )
  )
}

