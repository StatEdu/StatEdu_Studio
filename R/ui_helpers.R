# Auto-extracted shared functions for easyflow_statistics.

empty_message <- function(text) {
  div(class = "empty-message", text)
}

analysis_save_edition <- function() {
  edition <- tolower(Sys.getenv("EASYFLOW_EDITION", "free"))
  if (!edition %in% c("free", "development", "personal", "institution")) {
    edition <- "free"
  }
  edition
}

analysis_save_feature_enabled <- function(feature, edition = analysis_save_edition()) {
  if (identical(edition, "development")) {
    return(TRUE)
  }
  if (identical(edition, "personal") || identical(edition, "institution")) {
    return(feature %in% c("html", "pdf", "figure", "excel", "add_result"))
  }
  feature %in% c("html", "figure")
}

analysis_save_button <- function(id, label, feature, class = "btn-default") {
  if (is.null(id) || !nzchar(id)) {
    return(div(class = "analysis-save-slot analysis-save-slot-empty"))
  }
  enabled <- analysis_save_feature_enabled(feature)
  tags$button(
    id = id,
    type = "button",
    class = paste("btn action-button", class, "analysis-save-button"),
    disabled = if (!isTRUE(enabled)) "disabled" else NULL,
    span(class = "action-label", label)
  )
}

analysis_save_buttons <- function(
  html_button_id = NULL,
  pdf_button_id = NULL,
  figure_button_id = NULL,
  excel_button_id = NULL,
  add_result_button_id = NULL,
  has_figures = TRUE
) {
  div(
    class = "analysis-save-action",
    analysis_save_button(html_button_id, "Save HTML", "html", class = "btn-default"),
    if (isTRUE(has_figures)) {
      analysis_save_button(figure_button_id, "Save fig", "figure", class = "btn-default")
    } else {
      div(class = "analysis-save-slot analysis-save-slot-empty")
    },
    analysis_save_button(pdf_button_id, "Save PDF", "pdf", class = "btn-default"),
    analysis_save_button(excel_button_id, "Save Excel", "excel", class = "btn-default"),
    analysis_save_button(add_result_button_id, "Add result", "add_result", class = "btn-primary")
  )
}

register_add_result_placeholder <- function(input, button_id) {
  if (is.null(button_id) || !nzchar(button_id)) {
    return(invisible(FALSE))
  }
  observeEvent(input[[button_id]], {
    showNotification(
      "Add result is reserved for the institutional result accumulation workflow.",
      type = "message",
      duration = 4
    )
  }, ignoreInit = TRUE)
  invisible(TRUE)
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
  tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", version, "-hint8-layout-1"))
}

app_script_link <- function(version) {
  tags$script(src = paste0("easyflow.js?v=", version, "-nested-menu-22"))
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
    reliability = TRUE,
    frequencies = TRUE,
    paired = TRUE,
    paired_rm = TRUE,
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

    data_editor_tab_panel(),

    calculator_tab_panel(),

    analysis_tab_panel(analysis_tabs),

    result_tab_panel(),

    about_tab_panel()
  )
}
