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
  statedu_truthy(Sys.getenv("STATEDU_PUBLIC_RELEASE", ""))
}

analysis_document_export_available <- function(edition = analysis_save_edition()) {
  version <- tryCatch(read_app_config()$version, error = function(e) "0.0.0")
  public_documents <- statedu_public_release() &&
    utils::compareVersion(sub("-.*$", "", version), "1.3.0") >= 0L
  public_documents || (!statedu_public_release() && identical(edition, "development"))
}

statedu_feature_enabled <- function(feature, default = TRUE) {
  if (feature %in% c("pdf_export", "excel_export", "word_export")) {
    return(analysis_document_export_available())
  }
  env_name <- statedu_feature_env_name("STATEDU", feature)
  value <- Sys.getenv(env_name, "")
  if (statedu_truthy(value)) {
    return(TRUE)
  }
  if (statedu_falsey(value)) {
    return(FALSE)
  }

  isTRUE(default)
}

analysis_save_edition <- function() {
  edition_value <- Sys.getenv("STATEDU_EDITION", "")
  edition <- tolower(edition_value)
  if (!nzchar(edition) && statedu_public_release()) {
    edition <- "free"
  }
  if (!nzchar(edition)) {
    edition <- "development"
  }
  if (!edition %in% c("free", "pro", "development", "personal", "institution")) {
    edition <- "development"
  }
  edition
}

analysis_save_feature_visible <- function(feature, included_features = character(0)) {
  if (feature %in% c("pdf", "excel", "word")) {
    return(analysis_document_export_available())
  }
  if (feature %in% included_features) {
    return(TRUE)
  }
  TRUE
}

analysis_save_feature_enabled <- function(feature, edition = analysis_save_edition(), included_features = character(0)) {
  if (feature %in% c("pdf", "excel", "word")) {
    return(analysis_document_export_available(edition))
  }
  free_all <- "__free_all__" %in% included_features
  public_exception <- "__public_exception__" %in% included_features
  included_features <- setdiff(included_features, "__free_all__")
  included_features <- setdiff(included_features, "__public_exception__")
  if (identical(edition, "free")) {
    return(
      feature %in% c("html", "figure", "add_result", "result_history") ||
        isTRUE(free_all) ||
        (statedu_public_release() && isTRUE(public_exception) && feature %in% included_features)
    )
  }
  if (feature %in% included_features) {
    return(TRUE)
  }
  if (!isTRUE(analysis_save_feature_visible(feature, included_features))) {
    return(FALSE)
  }
  if (identical(edition, "development")) {
    return(TRUE)
  }
  if (identical(edition, "personal") || identical(edition, "institution")) {
    return(feature %in% c("html", "figure", "add_result", "result_history"))
  }
  if (identical(edition, "pro")) {
    return(feature %in% c("html", "figure", "add_result", "result_history"))
  }
  feature %in% c("html", "figure", "add_result", "result_history")
}

analysis_save_button <- function(id, label, feature, class = "btn-default", included_features = character(0)) {
  if (!isTRUE(analysis_save_feature_visible(feature, included_features))) {
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
  enabled <- analysis_save_feature_enabled(feature, included_features = included_features)
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
  language = statedu_initial_language(),
  included_features = character(0),
  hwpx_button_id = NULL
) {
  div(
    class = "analysis-save-action",
    analysis_save_button(html_button_id, statedu_ui_label("save_html", language), "html", class = "btn-default", included_features = included_features),
    if (isTRUE(has_figures)) {
      analysis_save_button(figure_button_id, statedu_ui_label("save_fig", language), "figure", class = "btn-default", included_features = included_features)
    } else {
      analysis_save_button(NULL, statedu_ui_label("save_fig", language), "figure", class = "btn-default", included_features = included_features)
    },
    analysis_save_button(pdf_button_id, statedu_ui_label("save_pdf", language), "pdf", class = "btn-default", included_features = included_features),
    analysis_save_button(excel_button_id, statedu_ui_label("save_excel", language), "excel", class = "btn-default", included_features = included_features),
    analysis_save_button(add_result_button_id, statedu_ui_label("add_result", language), "add_result", class = "btn-primary", included_features = included_features)
  )
}

analysis_three_block_action_row <- function(
  class = "",
  run_button,
  reset_control = NULL,
  save_control = NULL,
  extra_controls = NULL,
  command_control = NULL
) {
  run_id <- run_button$attribs$id %||% ""
  command_button <- command_control
  if (run_id %in% analysis_command_run_ids()) {
    command_button <- tags$button(
      type = "button", class = "btn btn-default",
      onclick = sprintf("window.stateduOpenCommand('%s', this)", run_id),
      statedu_localized_text(statedu_initial_language(), "Analysis Commands", "분석 명령어")
    )
  }
  div(
    class = paste("analysis-action-row analysis-three-block-action-row", class),
    div(class = "analysis-action-cell analysis-run-cell",
      div(class = "analysis-primary-actions", command_button, run_button)),
    div(class = "analysis-action-cell analysis-reset-cell", reset_control),
    if (!is.null(extra_controls)) div(class = "analysis-action-cell analysis-extra-cell", extra_controls),
    div(class = "analysis-action-cell analysis-save-cell", save_control)
  )
}

set_data_step_view <- function(active_step_setter, data_view_setter, step, view = "info") {
  active_step_setter(step)
  data_view_setter(view)
}

app_brand_title <- function(version) {
  div(
    class = "brand-title",
    tags$img(src = paste0("logo-final.png?v=", version, "-statedu-studio-final-logo-ui-20260822"), class = "brand-logo-horizontal", alt = "StatEdu Studio logo"),
    span(class = "version", paste0("v", version))
  )
}

app_stylesheet_link <- function(version) {
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("style.css?v=", version, "-structural-header-wrap-20260911g")),
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("model-canvas/canvas.css?v=", version, "-alignment-reference-icon-20260912b"))
  )
}

app_script_link <- function(version) {
  tagList(
    tags$script(src = paste0("analysis-commands.js?v=", version)),
    tags$script(src = paste0("analysis-scope.js?v=", version, "-canvas-groups-20260916")),
    tags$script(src = paste0("easyflow.js?v=", version, "-model-report-capture-20260911f")),
    tags$script(src = paste0("model-canvas/state.js?v=", version, "-scope-collections-20260916")),
    tags$script(src = paste0("model-canvas/layout.js?v=", version, "-custom-model-canvas-balanced-multi-role-layout-20260729a")),
    tags$script(src = paste0("model-canvas/shiny-bridge.js?v=", version, "-scope-result-groups-20260916")),
    tags$script(src = paste0("model-canvas/edges.js?v=", version, "-ellipse-boundary-anchor-20260825a")),
    tags$script(src = paste0("model-canvas/nodes.js?v=", version, "-full-paper-movement-20260823g")),
    tags$script(src = paste0("model-canvas/dialogs.js?v=", version, "-model-report-dpi-20260911f")),
    tags$script(src = paste0("model-canvas/toolbar.js?v=", version, "-selection-alignment-icons-20260911d")),
    tags$script(src = paste0("model-canvas/canvas.js?v=", version, "-center-model-on-paper-20260825a"))
  )
}

app_language_bootstrap_script <- function(language) {
  language <- normalize_app_language(language)
  tags$script(HTML(sprintf(
    paste0(
      "window.easyflowSupportedLanguages = %s;",
      "window.easyflowAppLanguage = '%s'; document.documentElement.lang = '%s';"
    ),
    jsonlite::toJSON(statedu_supported_languages(), auto_unbox = TRUE),
    language,
    language
  )))
}

app_result_zoom_bootstrap_script <- function(zoom_percent = statedu_initial_result_zoom()) {
  zoom_percent <- normalize_result_zoom_percent(zoom_percent)
  zoom_scale <- sprintf("%.3f", zoom_percent / 100)
  crosstab_scale <- sprintf("%.3f", (zoom_percent / 100) * 0.8)
  landscape_scale <- sprintf("%.3f", (zoom_percent / 100) * (2 / 3))
  tags$script(HTML(sprintf(
    paste0(
      "window.easyflowResultZoomPercent = %d;",
      "document.documentElement.style.setProperty('--statedu-result-zoom', '%s');",
      "document.documentElement.style.setProperty('--statedu-crosstab-result-zoom', '%s');",
      "document.documentElement.style.setProperty('--statedu-landscape-result-zoom', '%s');"
    ),
    zoom_percent,
    zoom_scale,
    crosstab_scale,
    landscape_scale
  )))
}

app_static_language_labels_script <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    keys <- c(
      "data", "data_editor", "calculator", "analysis", "sample_size", "effect_size",
      "latent", "result", "help", "about", "preferences", "bug_report", "feature_request",
      "analysis_request", "qna", "frequencies", "crosstabs", "ttest_anova",
      "paired", "ancova", "one_group_rm_anova", "mixed_rm_anova", "nonparametric", "nonparametric_paired", "correlation",
      "reliability", "interrater_agreement", "factor_analysis", "pca", "regression", "glm", "logistic",
      "longitudinal", "overview", "user_guide", "analyses", "method_notes",
      "validation", "version_history", "source_license", "open_source_licenses"
    )
    labels <- lapply(keys, function(key) {
      statedu_translation_row(paste0("ui.", key)) %||% list(en = statedu_ui_label(key, "en"), ko = statedu_ui_label(key, "ko"))
    })
    static_label_row <- function(en, ko = en) {
      values <- vapply(statedu_supported_languages(), function(language) {
        statedu_localized_text(language, en, ko)
      }, character(1))
      as.list(values)
    }
    method_label_rows <- function(label_fn) {
      languages <- statedu_supported_languages()
      language_labels <- stats::setNames(lapply(languages, label_fn), languages)
      common <- Reduce(intersect, lapply(language_labels, names))
      lapply(common, function(name) {
        values <- vapply(languages, function(language) {
          value <- language_labels[[language]][[name]] %||% ""
          unname(value)
        }, character(1))
        as.list(values)
      })
    }
    extra_labels <- list(
      statedu_translation_row("analysis.ipa.importance_performance_analysis_ipa"),
      statedu_translation_row("analysis.ui.select_cases"),
      statedu_translation_row("analysis.ui.split_file"),
      statedu_translation_row("data_editor.coding_error_title"),
      statedu_translation_row("data_editor.likert_title"),
      statedu_translation_row("data_editor.missing_title"),
      statedu_translation_row("data_editor.wide_long_title"),
      static_label_row("Merge", statedu_utf8("eb8db0ec9db4ed84b020ebb391ed95a9")),
      static_label_row("ID aggregation", statedu_utf8("494420eca791eab384")),
      statedu_translation_row("data_editor.reverse_title"),
      statedu_translation_row("data_editor.calculation_title"),
      statedu_translation_row("data_editor.transform_title"),
      statedu_translation_row("data_editor.recode_title"),
      statedu_translation_row("data_editor.rename_title"),
      static_label_row("HINT8", "HINT8"),
      static_label_row("EQ-5D", "EQ-5D"),
      statedu_translation_row("calc_metabolic_syndrome"),
      statedu_translation_row("calc_framingham_risk"),
      static_label_row("ASCVD10", "ASCVD10"),
      static_label_row("Meta-analysis", "메타분석"),
      statedu_translation_row("calc_metabolic_severity"),
      statedu_translation_row("complex_sample.menu"),
      statedu_translation_row("complex_sample.design_menu"),
      statedu_translation_row("complex_sample.frequencies"),
      statedu_translation_row("complex_sample.crosstabs"),
      statedu_translation_row("complex_sample.ttest_anova"),
      statedu_translation_row("complex_sample.correlation"),
      statedu_translation_row("complex_sample.regression"),
      statedu_translation_row("complex_sample.logistic"),
      statedu_translation_row("analysis.ui.repeated_measures_anova"),
      statedu_translation_row("analysis.ui.inter_rater_agreement"),
      statedu_translation_row("analysis.mediation_moderation"),
      statedu_translation_row("analysis.custom_model_canvas"),
      statedu_translation_row("custom_model_canvas.title"),
      static_label_row("Survival Analysis", "생존분석"),
      static_label_row("Structural Equation Modeling", "구조방정식"),
      static_label_row("Kaplan-Meier", "Kaplan-Meier"),
      static_label_row("Cox Regression", "Cox 회귀분석")
    )
    group_labels <- list(
      statedu_translation_row("group_descriptives"),
      statedu_translation_row("group_comparisons"),
      statedu_translation_row("group_nonparametric"),
      statedu_translation_row("group_association"),
      statedu_translation_row("group_regression"),
      statedu_translation_row("group_longitudinal"),
      statedu_translation_row("group_study_design")
    )
    method_labels <- list()
    if (exists("sample_size_method_labels", mode = "function")) {
      method_labels <- c(method_labels, method_label_rows(sample_size_method_labels))
    }
    if (exists("effect_size_method_labels", mode = "function")) {
      method_labels <- c(method_labels, method_label_rows(effect_size_method_labels))
    }
    keyed_keys <- c("data.categorical_value_labels", "data.selected_variables", "data.subtitle", "data.excel_import_review",
                    "ui.result", "result.subtitle", "result.open", "result.save", "result.clear")
    keyed_labels <- stats::setNames(lapply(keyed_keys, statedu_translation_row), keyed_keys)
    result <- tags$script(HTML(sprintf(
      "window.easyflowStaticLanguageLabels = %s; window.easyflowKeyedLanguageLabels = %s;",
      jsonlite::toJSON(c(labels, extra_labels, group_labels, method_labels), auto_unbox = TRUE),
      jsonlite::toJSON(keyed_labels, auto_unbox = TRUE)
    )))
    cache <<- result
    result
  }
})

app_head_tags <- function(version) {
  build_fingerprint <- trimws(Sys.getenv("STATEDU_BUILD_FINGERPRINT", ""))
  asset_version <- if (nzchar(build_fingerprint)) {
    paste(version, substr(build_fingerprint, 1L, 12L), sep = "-")
  } else {
    version
  }
  tags$head(
    tags$meta(name = "statedu-build-fingerprint", content = build_fingerprint),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = paste0("logo-favicon-32.png?v=", asset_version, "-statedu-studio-final-slanted-bar")),
    tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = paste0("logo-favicon-64.png?v=", asset_version, "-statedu-studio-final-slanted-bar")),
    app_stylesheet_link(asset_version),
    tags$script(HTML(paste0(
      "window.MathJax = {
        tex: {
          inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
          displayMath: [['$$', '$$'], ['\\\\[', '\\\\]']],
          processEscapes: true
        },
        options: {
          skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
        }
      };
      window.easyflowMathJaxSrc = '",
      paste0("mathjax/tex-svg.js?v=", version, "-local"),
      "';"
    ))),
    app_static_language_labels_script(),
    app_script_link(asset_version)
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
    interrater_agreement = TRUE,
    frequencies = TRUE,
    paired = TRUE,
    paired_rm = TRUE,
    ttest_anova = TRUE,
    ancova = TRUE,
    one_group_rm_anova = !statedu_public_release(),
    mixed_rm_anova = TRUE,
    nonparametric = TRUE,
    nonparametric_paired = TRUE,
    correlation = TRUE,
    factor_analysis = TRUE,
    pca = TRUE,
    regression = FALSE,
    hierarchical = TRUE,
    mediation_moderation = FALSE,
    custom_model_canvas = statedu_feature_enabled("custom_model_canvas", TRUE),
    longitudinal = statedu_feature_enabled("longitudinal", TRUE),
    generalized = TRUE,
    meta = !statedu_public_release()
  )
}

app_ui <- function(version, request = NULL) {
  if (!isTRUE(statedu_request_token_authorized(request))) {
    return(statedu_token_rejection_response())
  }
  analysis_tabs <- enabled_analysis_tabs()
  language <- statedu_initial_language(request)

  navbarPage(
    title = app_brand_title(version),
    id = "main_menu",
    header = tagList(
      app_head_tags(version),
      app_language_bootstrap_script(language),
      app_result_zoom_bootstrap_script(statedu_initial_result_zoom()),
      tags$input(id = "statedu_initial_language", type = "hidden", value = language),
      tags$input(id = "statedu_initial_result_zoom", type = "hidden", value = statedu_initial_result_zoom())
    ),

    data_tab_panel(language),

    data_editor_tab_panel(language),

    calculator_tab_panel(language),

    analysis_tab_panel(analysis_tabs, language),

    sample_size_tab_panel(language),

    effect_size_tab_panel(language),

    if (latent_mplus_enabled()) latent_menu_tab(language),

    result_tab_panel(language),

    help_tab_panel(version, language),

    about_tab_panel(version, language)
  )
}
