data_tab_panel <- function() {
  tabPanel(
    "Data",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("easyflow_statistics"),
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
            condition = "output.data_view === 'labels'",
            DTOutput("category_label_table")
          )
        )
      )
    )
  )
}

