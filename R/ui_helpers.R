# Auto-extracted shared functions for StatEdu Studio.

empty_message <- function(text) {
  div(class = "empty-message", text)
}

statedu_flag_normalize <- function(value) {
  value <- value[!is.na(value)]
  if (length(value) == 0) {
    return("")
  }
  tolower(trimws(as.character(value[[1]])))
}

statedu_truthy <- function(value) {
  statedu_flag_normalize(value) %in% c("1", "true", "yes", "on", "public")
}

statedu_falsey <- function(value) {
  statedu_flag_normalize(value) %in% c("0", "false", "no", "off", "development")
}

statedu_feature_env_name <- function(prefix, feature) {
  paste0(prefix, "_ENABLE_", toupper(gsub("[^A-Za-z0-9]+", "_", feature)))
}

statedu_public_release <- function() {
  statedu_truthy(Sys.getenv("STATEDU_PUBLIC_RELEASE", "")) ||
    statedu_truthy(Sys.getenv("EASYFLOW_PUBLIC_RELEASE", ""))
}

statedu_feature_enabled <- function(feature, default = TRUE) {
  env_names <- c(
    statedu_feature_env_name("STATEDU", feature),
    statedu_feature_env_name("EASYFLOW", feature)
  )
  for (env_name in env_names) {
    value <- Sys.getenv(env_name, "")
    if (statedu_truthy(value)) {
      return(TRUE)
    }
    if (statedu_falsey(value)) {
      return(FALSE)
    }
  }

  if (statedu_public_release() && feature %in% c("longitudinal", "excel_export", "word_export")) {
    return(FALSE)
  }
  isTRUE(default)
}

analysis_save_edition <- function() {
  edition <- tolower(Sys.getenv("EASYFLOW_EDITION", "development"))
  if (!edition %in% c("free", "development", "personal", "institution")) {
    edition <- "development"
  }
  edition
}

analysis_save_feature_visible <- function(feature) {
  if (identical(feature, "excel")) {
    return(statedu_feature_enabled("excel_export", TRUE))
  }
  if (identical(feature, "word")) {
    return(statedu_feature_enabled("word_export", TRUE))
  }
  TRUE
}

analysis_save_feature_enabled <- function(feature, edition = analysis_save_edition()) {
  if (!isTRUE(analysis_save_feature_visible(feature))) {
    return(FALSE)
  }
  if (identical(edition, "development")) {
    return(TRUE)
  }
  if (identical(edition, "personal") || identical(edition, "institution")) {
    return(feature %in% c("html", "pdf", "figure", "excel", "word", "add_result", "result_history"))
  }
  feature %in% c("html", "pdf", "figure", "word")
}

analysis_save_button <- function(id, label, feature, class = "btn-default") {
  if (!isTRUE(analysis_save_feature_visible(feature))) {
    return(NULL)
  }
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
  has_figures = TRUE,
  language = statedu_initial_language()
) {
  div(
    class = "analysis-save-action",
    analysis_save_button(html_button_id, statedu_ui_label("save_html", language), "html", class = "btn-default"),
    if (isTRUE(has_figures)) {
      analysis_save_button(figure_button_id, statedu_ui_label("save_fig", language), "figure", class = "btn-default")
    } else {
      analysis_save_button(NULL, statedu_ui_label("save_fig", language), "figure", class = "btn-default")
    },
    analysis_save_button(pdf_button_id, statedu_ui_label("save_pdf", language), "pdf", class = "btn-default"),
    analysis_save_button(excel_button_id, statedu_ui_label("save_excel", language), "excel", class = "btn-default"),
    analysis_save_button(add_result_button_id, statedu_ui_label("add_result", language), "add_result", class = "btn-primary")
  )
}

set_data_step_view <- function(active_step_setter, data_view_setter, step, view = "info") {
  active_step_setter(step)
  data_view_setter(view)
}

app_brand_title <- function(version) {
  div(
    class = "brand-title",
    tags$img(src = paste0("logo-horizontal.png?v=", version, "-statedu-studio-final"), class = "brand-logo-horizontal", alt = "StatEdu Studio logo"),
    span(class = "version", paste0("v", version))
  )
}

app_stylesheet_link <- function(version) {
  tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", version, "-language-layout-20260627g"))
}

app_script_link <- function(version) {
  tags$script(src = paste0("easyflow.js?v=", version, "-language-switch-reload-20260628a"))
}

app_language_bootstrap_script <- function(language) {
  language <- normalize_app_language(language)
  tags$script(HTML(sprintf(
    "window.easyflowAppLanguage = '%s'; document.documentElement.lang = '%s';",
    language,
    language
  )))
}

app_head_tags <- function(version) {
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = paste0("logo-favicon-32.png?v=", version, "-statedu-studio-final")),
    tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = paste0("logo-favicon-64.png?v=", version, "-statedu-studio-final")),
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
    ancova = TRUE,
    nonparametric = TRUE,
    nonparametric_paired = TRUE,
    correlation = TRUE,
    factor_analysis = TRUE,
    pca = TRUE,
    regression = FALSE,
    hierarchical = TRUE,
    longitudinal = statedu_feature_enabled("longitudinal", FALSE),
    generalized = TRUE
  )
}

app_ui <- function(version, request = NULL) {
  analysis_tabs <- enabled_analysis_tabs()
  language <- statedu_initial_language(request)

  navbarPage(
    title = app_brand_title(version),
    id = "main_menu",
    header = tagList(
      app_head_tags(version),
      app_language_bootstrap_script(language),
      tags$input(id = "statedu_initial_language", type = "hidden", value = language),
      if (latent_mplus_enabled()) latent_mplus_head_tags(version)
    ),

    data_tab_panel(language),

    data_editor_tab_panel(language),

    calculator_tab_panel(language),

    analysis_tab_panel(analysis_tabs, language),

    sample_size_tab_panel(language),

    effect_size_tab_panel(language),

    if (latent_mplus_enabled()) latent_menu_tab(),

    result_tab_panel(language),

    help_tab_panel(version, language),

    about_tab_panel(version, language)
  )
}
