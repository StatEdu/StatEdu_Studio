# Auto-extracted shared functions for easyflow_statistics.

empty_message <- function(text) {
  div(class = "empty-message", text)
}

analysis_save_edition <- function() {
  edition <- tolower(Sys.getenv("EASYFLOW_EDITION", "development"))
  if (!edition %in% c("free", "development", "personal", "institution")) {
    edition <- "development"
  }
  edition
}

analysis_save_feature_enabled <- function(feature, edition = analysis_save_edition()) {
  if (identical(edition, "development")) {
    return(TRUE)
  }
  if (identical(edition, "personal") || identical(edition, "institution")) {
    return(feature %in% c("html", "pdf", "figure", "excel", "word", "add_result"))
  }
  feature %in% c("html", "pdf", "figure")
}

analysis_save_button <- function(id, label, feature, class = "btn-default") {
  if (is.null(id) || !nzchar(id)) {
    if (identical(analysis_save_edition(), "development")) {
      return(tags$button(
        type = "button",
        class = paste("btn action-button", class, "analysis-save-button"),
        disabled = "disabled",
        span(class = "action-label", label)
      ))
    }
    return(div(class = "analysis-save-slot analysis-save-slot-empty"))
  }
  enabled <- analysis_save_feature_enabled(feature)
  tags$button(
    id = id,
    type = "button",
    class = paste("btn action-button", class, "analysis-save-button", paste0("analysis-save-button-", feature)),
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
      analysis_save_button(NULL, "Save fig", "figure", class = "btn-default")
    },
    analysis_save_button(pdf_button_id, "Save PDF", "pdf", class = "btn-default"),
    analysis_save_button(excel_button_id, "Save Excel", "excel", class = "btn-default"),
    analysis_save_button(add_result_button_id, "Add result", "add_result", class = "btn-primary")
  )
}

set_data_step_view <- function(active_step_setter, data_view_setter, step, view = "info") {
  active_step_setter(step)
  data_view_setter(view)
}

app_brand_title <- function(version) {
  div(
    class = "brand-title",
    tags$img(src = paste0("logo-horizontal.png?v=", version, "-concept-02-8"), class = "brand-logo-horizontal", alt = "EasyFlow Statistics logo"),
    span(class = "version", paste0("v", version))
  )
}

app_stylesheet_link <- function(version) {
  tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", version, "-measurement-level-icons"))
}

app_script_link <- function(version) {
  tags$script(src = paste0("easyflow.js?v=", version, "-measurement-level-icons"))
}

app_head_tags <- function(version) {
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = paste0("logo-favicon-32.png?v=", version, "-concept-02-8")),
    tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = paste0("logo-favicon-64.png?v=", version, "-concept-02-8")),
    app_stylesheet_link(version),
    tags$script(HTML(
      "window.MathJax = {
        tex: {
          inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
          displayMath: [['$$', '$$'], ['\\\\[', '\\\\]']],
          processEscapes: true
        },
        options: {
          skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
        }
      };"
    )),
    tags$script(
      id = "MathJax-script",
      defer = "defer",
      onload = "if (window.easyflowMathJaxReady) window.easyflowMathJaxReady();",
      src = paste0("mathjax/tex-svg.js?v=", version, "-local")
    ),
    app_script_link(version)
  )
}

lazy_tab_panel <- function(title, value, output_id) {
  tabPanel(
    title,
    value = value,
    uiOutput(output_id)
  )
}

tab_panel_content <- function(panel) {
  if (inherits(panel, "shiny.tag") && identical(panel$name, "div")) {
    return(tagList(panel$children))
  }
  panel
}

enabled_analysis_tabs <- function() {
  c(
    reliability = TRUE,
    frequencies = TRUE,
    paired = TRUE,
    paired_rm = TRUE,
    ttest_anova = TRUE,
    nonparametric = TRUE,
    nonparametric_paired = TRUE,
    correlation = TRUE,
    factor_analysis = TRUE,
    pca = TRUE,
    regression = FALSE,
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

    sample_size_tab_panel(),

    effect_size_tab_panel(),

    result_tab_panel(),

    about_tab_panel(version)
  )
}
