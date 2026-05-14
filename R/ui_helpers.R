# Auto-extracted shared functions for EasyFlow Statistics.

empty_message <- function(text) {
  div(class = "empty-message", text)
}

analysis_save_buttons <- function(table_button_id, figure_button_id) {
  div(
    class = "analysis-save-action",
    actionButton(table_button_id, "Save tables", class = "btn-primary"),
    actionButton(figure_button_id, "Save figures", class = "btn-default")
  )
}

set_data_step_view <- function(active_step_setter, data_view_setter, step, view = "info") {
  active_step_setter(step)
  data_view_setter(view)
}

app_brand_title <- function(version) {
  div(
    class = "brand-title",
    tags$img(src = "logo-mark.svg", class = "brand-logo", alt = "EasyFlow Statistics logo"),
    span(class = "brand-name", "EasyFlow Statistics"),
    span(class = "version", paste0("v", version))
  )
}

app_stylesheet_link <- function(version) {
  tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", version, "-regression-listbox-20-1"))
}

app_script_link <- function(version) {
  tags$script(src = paste0("easyflow.js?v=", version, "-selection-controls-5"))
}

app_head_tags <- function(version) {
  tags$head(
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
