# Auto-extracted shared functions for EasyFlow Statistics.

empty_message <- function(text) {
  div(class = "empty-message", text)
}

set_data_step_view <- function(active_step_setter, data_view_setter, step, view = "info") {
  active_step_setter(step)
  data_view_setter(view)
}

app_brand_title <- function(version) {
  div(class = "brand-title", "EasyFlow Statistics", span(class = "version", paste0("v", version)))
}

app_stylesheet_link <- function(version) {
  tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", version, "-overlay"))
}

app_script_link <- function(version) {
  tags$script(src = paste0("easyflow.js?v=", version))
}

app_head_tags <- function(version) {
  tags$head(
    app_stylesheet_link(version),
    app_script_link(version)
  )
}

app_ui <- function(version) {
  navbarPage(
    title = app_brand_title(version),
    id = "main_menu",
    header = app_head_tags(version),

    data_tab_panel(),

    regression_tab_panel(),

    hierarchical_tab_panel(),

    generalized_tab_panel()
  )
}
