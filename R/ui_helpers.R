# Auto-extracted shared functions for easyflow_statistics.

empty_message <- function(text) {
  div(class = "empty-message", text)
}

analysis_save_buttons <- function(table_button_id, figure_button_id, html_button_id = NULL) {
  buttons <- list(
    class = "analysis-save-action",
    actionButton(table_button_id, "Save tables", class = "btn-primary"),
    actionButton(figure_button_id, "Save figures", class = "btn-default")
  )
  if (!is.null(html_button_id) && nzchar(html_button_id)) {
    buttons <- c(buttons, list(actionButton(html_button_id, "Save HTML", class = "btn-default")))
  }
  do.call(div, buttons)
}

set_data_step_view <- function(active_step_setter, data_view_setter, step, view = "info") {
  active_step_setter(step)
  data_view_setter(view)
}

app_brand_title <- function(version) {
  div(
    class = "brand-title",
    tags$img(src = "logo-horizontal.png", class = "brand-logo-horizontal", alt = "Easyflow Statistics logo"),
    span(class = "version", paste0("v", version))
  )
}

app_stylesheet_link <- function(version) {
  tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", version, "-hier-term-1"))
}

app_script_link <- function(version) {
  tags$script(src = paste0("easyflow.js?v=", version, "-selection-controls-7"))
}

app_head_tags <- function(version) {
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo-favicon-32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = "logo-favicon-64.png"),
    app_stylesheet_link(version),
    app_script_link(version)
  )
}

enabled_analysis_tabs <- function() {
  c(
    frequencies = TRUE,
    ttest_anova = TRUE,
    correlation = TRUE,
    regression = TRUE,
    hierarchical = TRUE,
    generalized = FALSE
  )
}

app_ui <- function(version) {
  analysis_tabs <- enabled_analysis_tabs()

  navbarPage(
    title = app_brand_title(version),
    id = "main_menu",
    header = app_head_tags(version),

    data_tab_panel(),

    if (isTRUE(analysis_tabs[["frequencies"]])) frequencies_tab_panel(),

    if (isTRUE(analysis_tabs[["ttest_anova"]])) ttest_anova_tab_panel(),

    if (isTRUE(analysis_tabs[["correlation"]])) correlation_tab_panel(),

    if (isTRUE(analysis_tabs[["regression"]])) regression_tab_panel(),

    if (isTRUE(analysis_tabs[["hierarchical"]])) hierarchical_tab_panel(),

    if (isTRUE(analysis_tabs[["generalized"]])) generalized_tab_panel()
  )
}
