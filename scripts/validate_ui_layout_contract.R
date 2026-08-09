script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_ui_layout_contract.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_project_file <- function(path) {
  paste(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

count_fixed <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (length(matches) == 1L && matches[[1]] == -1L) 0L else length(matches)
}

assert_contains <- function(text, pattern, label) {
  if (!grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("UI layout contract missing: %s", label), call. = FALSE)
  }
}

assert_not_contains <- function(text, pattern, label) {
  if (grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("UI layout contract should not contain: %s", label), call. = FALSE)
  }
}

assert_count_at_least <- function(text, pattern, minimum, label) {
  actual <- count_fixed(text, pattern)
  if (actual < minimum) {
    stop(sprintf(
      "UI layout contract missing: %s; expected at least %s, found %s",
      label,
      minimum,
      actual
    ), call. = FALSE)
  }
}

assert_count_exact <- function(text, pattern, expected, label) {
  actual <- count_fixed(text, pattern)
  if (actual != expected) {
    stop(sprintf(
      "UI layout contract mismatch: %s; expected %s, found %s",
      label,
      expected,
      actual
    ), call. = FALSE)
  }
}

extract_between <- function(text, start_pattern, end_pattern, label) {
  start <- regexpr(start_pattern, text, fixed = TRUE)[[1]]
  if (start == -1L) {
    stop(sprintf("UI layout contract missing block start: %s", label), call. = FALSE)
  }
  rest <- substr(text, start, nchar(text))
  end <- regexpr(end_pattern, rest, fixed = TRUE)[[1]]
  if (end == -1L) {
    stop(sprintf("UI layout contract missing block end: %s", label), call. = FALSE)
  }
  substr(rest, 1L, end - 1L)
}

css <- read_project_file("www/style.css")
analysis_menu_ui <- read_project_file("R/analysis_menu_ui.R")
analysis_data_viewer_ui <- read_project_file("R/analysis_data_viewer.R")
setup_ui <- read_project_file("R/setup_ui.R")
data_ui_steps <- read_project_file("R/data_ui_steps.R")
sample_size_ui <- read_project_file("R/sample_size_ui.R")
data_editor_ui <- read_project_file("R/data_editor_ui.R")
wide_long_ui <- read_project_file("R/data_editor_wide_long.R")
rename_ui <- read_project_file("R/data_editor_rename.R")
recode_ui <- read_project_file("R/data_editor_recode.R")
likert_ui <- read_project_file("R/data_editor_likert.R")
transform_ui <- read_project_file("R/data_editor_transform.R")
missing_ui <- read_project_file("R/data_editor_missing.R")
easyflow_js <- read_project_file("www/easyflow.js")
app_server <- read_project_file("R/app_server.R")
app_misc_ui <- read_project_file("R/app_misc_ui.R")
ui_helpers <- read_project_file("R/ui_helpers.R")
utils_r <- read_project_file("R/utils.R")
electron_main <- read_project_file("packaging/electron/main.js")
layout_doc <- read_project_file("docs/UI_LAYOUT_CONTRACT.md")
r_text <- paste(vapply(list.files(file.path(repo_root, "R"), pattern = "\\.R$", full.names = TRUE), function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}, character(1)), collapse = "\n")

message("Checking Preferences output zoom contract...")
assert_contains(utils_r, "normalize_result_zoom_percent <- function(value)", "result zoom normalization helper")
assert_contains(utils_r, "max(80, min(200, round(number)))", "result zoom 80-200 clamp")
assert_contains(utils_r, "statedu_write_persisted_result_zoom <- function(value)", "result zoom persistence writer")
assert_contains(ui_helpers, "app_result_zoom_bootstrap_script", "result zoom bootstrap script")
assert_contains(ui_helpers, "--statedu-result-zoom", "result zoom CSS variable bootstrap")
assert_contains(app_misc_ui, "sliderInput(\n              \"result_zoom_percent\"", "Preferences result zoom slider")
assert_contains(app_misc_ui, "min = 80", "Preferences result zoom lower bound")
assert_contains(app_misc_ui, "max = 200", "Preferences result zoom upper bound")
assert_contains(app_server, "observeEvent(input$apply_result_zoom", "Preferences result zoom apply observer")
assert_contains(utils_r, "statedu_default_preferences <- function()", "general app preferences defaults")
assert_contains(utils_r, "normalize_output_decimal_digits <- function(value)", "output decimal digit normalizer")
assert_contains(utils_r, "normalize_p_value_format <- function(value)", "p-value format normalizer")
assert_contains(utils_r, "normalize_multiple_correction_default <- function(value)", "multiple-comparison default normalizer")
assert_contains(utils_r, "normalize_selected_variables_only_default <- function(value)", "selected-only default normalizer")
assert_contains(utils_r, "normalize_default_save_dir <- function(value)", "default save directory normalizer")
assert_contains(app_misc_ui, "\"output_decimal_digits\"", "Preferences output decimal digits control")
assert_contains(app_misc_ui, "\"p_value_format\"", "Preferences p-value format control")
assert_contains(app_misc_ui, "\"multiple_correction_default\"", "Preferences multiple correction control")
assert_contains(app_misc_ui, "\"selected_variables_only_default\"", "Preferences selected-only default control")
assert_contains(app_misc_ui, "\"default_save_dir\"", "Preferences default save location control")
assert_contains(app_server, "observeEvent(input$apply_general_preferences", "Preferences general defaults apply observer")
assert_contains(utils_r, "statedu_supported_languages <- function()", "central supported language list")
assert_contains(utils_r, "statedu_t <- function(key", "translation-key helper")
assert_contains(utils_r, "statedu_t(paste0(\"ui.\", key)", "UI labels use translation table")
labels_r <- read_project_file("R/labels.R")
assert_contains(labels_r, "statedu_translation_table <- local({", "central translation table")
assert_contains(labels_r, "statedu_language_choices <- function", "language selector choices from translation table")
assert_contains(labels_r, "preferences.default_save_dir", "Preferences strings in translation table")
assert_contains(labels_r, "result.open", "Result page strings in translation table")
assert_contains(labels_r, "about.check_updates_title", "About/update strings in translation table")
assert_contains(labels_r, "help.bug_subtitle", "Help request strings in translation table")
assert_contains(app_misc_ui, "statedu_t(\"preferences.subtitle\"", "Preferences page uses translation keys")
assert_contains(app_misc_ui, "statedu_t(\"result.open\"", "Result page uses translation keys")
assert_contains(app_misc_ui, "statedu_t(\"about.info_subtitle\"", "About page uses translation keys")
assert_contains(app_misc_ui, "statedu_t(\"help.bug_subtitle\"", "Help request page uses translation keys")
assert_contains(app_misc_ui, "choices = statedu_language_choices(language)", "Preferences language choices from translation table")
assert_contains(ui_helpers, "window.easyflowSupportedLanguages", "client supported language bootstrap")
assert_contains(easyflow_js, "window.easyflowSupportedLanguages || ['ko', 'en']", "client dynamic supported language fallback")
assert_contains(easyflow_js, "return easyflowNavigateToLanguage(language, true);", "language apply reloads the complete UI")
assert_contains(easyflow_js, "window.location.replace(nextHref);", "language change navigates to the translated session")
assert_contains(easyflow_js, "target && target.id === 'app_language'", "preferences language selector triggers language application")
assert_contains(app_server, 'requested_language <- strsplit(apply_request, ":", fixed = TRUE)[[1]][[1]]', "language apply event carries the authoritative selected language")
assert_contains(
  app_server,
  'active_app_language <- reactiveVal("")',
  "session language starts unset so bootstrap hints can initialize it"
)
assert_contains(
  app_server,
  "selected <- if (nzchar(active_language)) {\n      active_language",
  "confirmed session language takes priority over bootstrap URL and selector hints"
)
assert_contains(easyflow_js, "function easyflowApplyResultZoomValue(value)", "client result zoom application")
assert_contains(easyflow_js, "statedu-apply-result-zoom", "client result zoom custom message")
assert_contains(css, "zoom: var(--statedu-result-zoom, 1.5) !important;", "result panel zoom variable")
assert_contains(css, "zoom: var(--statedu-crosstab-result-zoom, 1.2) !important;", "crosstab zoom variable")
assert_contains(electron_main, "STATEDU_RESULT_ZOOM_FILE: resultZoomFile()", "Electron result zoom setting file")
assert_contains(electron_main, "STATEDU_APP_PREFERENCES_FILE: appPreferencesFile()", "Electron app preferences setting file")
assert_contains(electron_main, "function defaultSaveDirectory()", "Electron default save directory reader")
assert_contains(electron_main, "webContents.session.on(\"will-download\"", "Electron download save path hook")
assert_contains(electron_main, "item.setSavePath", "Electron download save path setter")

message("Checking Data tab step controls contract...")
assert_contains(data_ui_steps, "step2_selection_controls <- function()", "Step 2 selection controls helper")
assert_contains(data_ui_steps, "actionButton(\"apply_variable_selection\"", "Step 2 apply button")
assert_contains(data_ui_steps, "actionButton(\"apply_bulk_measurement_type\"", "Step 2 bulk measurement apply button")
assert_contains(css, "grid-template-columns: minmax(0, 1fr);", "Step 2 selection actions use a language-safe full-width stack")
assert_contains(css, ".step-block .bulk-measurement-action .btn {\n  align-items: center;", "Step 2 selection action buttons share alignment")
assert_contains(css, "overflow-wrap: anywhere;", "Step 2 translated action labels stay inside their buttons")
assert_contains(data_ui_steps, 'selected = "whitespace",\n              width = "100%"', "DAT delimiter control matches the Step 1 control width")
assert_contains(data_ui_steps, "else if (identical(step, \"step2\") || !isTRUE(applied))", "Step 2 controls stay visible before selection is applied")

message("Checking shared Data Editor geometry contract...")
assert_contains(css, "--se-standard-setup-width: 1176px;", "standard setup width")
assert_contains(css, "--se-standard-panel-height: 520px;", "standard panel height")
assert_contains(css, "--se-standard-options-width: 310px;", "standard options width")
assert_contains(css, "--se-standard-options-button-width: 286px;", "standard options footer button width")
assert_not_contains(css, ".data-editor-workspace {\n  --se-standard", "duplicate Data Editor standard variable declarations")
assert_not_contains(css, "var(--se-standard-setup-width, 1140px)", "stale standard setup width fallback")
assert_contains(css, ".recode-same-setup-grid {\n  display: grid;\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "base Data Editor setup grid uses shared variables")
assert_contains(css, ".recode-same-action-row {\n  display: grid;\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "base Data Editor action row uses shared variables")
assert_contains(css, ".recode-builder-action-row {\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "base Data Editor builder action row uses shared variables")
assert_contains(css, ".recode-same-action-row > .btn {\n  grid-column: 1;\n  justify-self: start;\n  margin-left: 30px;\n  width: var(--se-standard-inner-button-width, 300px);", "base Data Editor action buttons use shared width")
assert_contains(css, ".recode-builder-action-row > .btn {\n  grid-column: 1;\n  justify-self: start;\n  margin-left: 30px;\n  width: var(--se-standard-inner-button-width, 300px);", "base Data Editor builder action buttons use shared width")
assert_contains(css, ".missing-values-action-cell {\n  grid-column: 1;\n  justify-self: start;\n  display: flex;\n  flex-direction: column;\n  gap: 8px;\n  width: var(--se-standard-inner-button-width, 300px);", "Missing Values action cell uses shared width")
assert_count_at_least(wide_long_ui, "data-editor-workspace", 1L, "Wide to Long workspace wrapper")
assert_count_at_least(rename_ui, "data-editor-workspace", 1L, "Rename Variable workspace wrapper")
assert_count_at_least(recode_ui, "data-editor-workspace", 4L, "Recode tools workspace wrappers")
assert_count_at_least(likert_ui, "data-editor-workspace", 1L, "Likert workspace wrapper")
assert_count_at_least(transform_ui, "data-editor-workspace", 1L, "Variable Transformation workspace wrapper")
assert_count_at_least(missing_ui, "data-editor-workspace", 1L, "Missing Values workspace wrapper")
assert_contains(css, ".data-editor-workspace .analysis-workspace-heading", "workspace heading width rule")
assert_contains(css, ".data-editor-workspace .recode-same-setup-grid:not(.recode-builder-grid)", "standard setup grid rule")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row)", "standard action row rule")
assert_contains(css, "grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) var(--se-standard-options-width) !important;", "standard frequency-style three-block grid columns")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row) {\n  display: grid !important;\n  grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) var(--se-standard-options-width) !important;\n  gap: var(--se-standard-gap) !important;\n  align-items: start !important;\n  margin-top: 26px !important;\n  margin-bottom: 14px !important;\n}", "standard three-block action row spacing")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row) > .btn,\n.data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row) > .missing-values-action-cell {\n  grid-column: 1 !important;\n  justify-self: start !important;\n  margin-left: 30px !important;\n  width: var(--se-standard-inner-button-width) !important;", "standard Block 1 action placement")
assert_contains(css, ".data-editor-workspace .recode-same-setup-grid:not(.recode-builder-grid) > .analysis-options-panel {\n  grid-column: 4 !important;\n  grid-row: 1 !important;", "standard Data Editor options stay in top row")
assert_contains(css, ".data-editor-workspace .wide-long-options {\n  grid-column: 4 !important;\n  grid-row: 1 !important;", "Wide to Long options stay in top row")
assert_contains(css, ".data-editor-workspace .recode-builder-rule-panel {\n  grid-column: 4 !important;\n  grid-row: 1 !important;", "Recode builder options stay in top row")

message("Checking Data Editor lazy menu wiring...")
data_editor_lazy_contract <- data.frame(
  title = c(
    "Auto coding error check",
    "Auto Likert conversion",
    "Auto missing values",
    "Wide to Long",
    "Auto reverse coding",
    "Auto variable calculation",
    "Variable transformation",
    "Recode variable",
    "Rename variable"
  ),
  title_key = c(
    "data_editor.coding_error_title",
    "data_editor.likert_title",
    "data_editor.missing_title",
    "data_editor.wide_long_title",
    "data_editor.reverse_title",
    "data_editor.calculation_title",
    "data_editor.transform_title",
    "data_editor.recode_title",
    "data_editor.rename_title"
  ),
  value = c(
    "data_editor_coding_error_check",
    "data_editor_likert",
    "data_editor_missing_values",
    "data_editor_wide_long",
    "data_editor_recode_different",
    "data_editor_variable_calculation",
    "data_editor_variable_transformation",
    "data_editor_recode_same",
    "data_editor_variable_rename"
  ),
  output_id = c(
    "lazy_data_editor_coding_error_check",
    "lazy_data_editor_likert",
    "lazy_data_editor_missing_values",
    "lazy_data_editor_wide_long",
    "lazy_data_editor_recode_different",
    "lazy_data_editor_variable_calculation",
    "lazy_data_editor_variable_transformation",
    "lazy_data_editor_recode_same",
    "lazy_data_editor_variable_rename"
  ),
  panel_call = c(
    "data_editor_coding_error_check_panel",
    "data_editor_likert_panel",
    "data_editor_missing_panel",
    "data_editor_wide_long_panel",
    "data_editor_different_variable_panel",
    "data_editor_variable_calculation_panel",
    "data_editor_variable_transformation_panel",
    "data_editor_same_variable_panel",
    "data_editor_variable_rename_panel"
  ),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(data_editor_lazy_contract))) {
  lazy_row <- data_editor_lazy_contract[i, ]
  assert_contains(
    data_editor_ui,
    sprintf('statedu_t("%s", language)', lazy_row$title_key),
    sprintf("Data Editor lazy menu translated label: %s", lazy_row$title)
  )
  assert_contains(
    data_editor_ui,
    sprintf('), "%s", "%s")', lazy_row$value, lazy_row$output_id),
    sprintf("Data Editor lazy menu item: %s", lazy_row$title)
  )
  assert_contains(
    app_server,
    sprintf("output$%s <- renderUI(%s(app_language()))", lazy_row$output_id, lazy_row$panel_call),
    sprintf("Data Editor lazy renderUI target: %s", lazy_row$title)
  )
}

message("Checking shared selected-data viewer button contract...")
assert_contains(analysis_data_viewer_ui, 'analysis_data_viewer_button <- function(id, language = statedu_initial_language())', "selected-data viewer button helper")
assert_contains(analysis_data_viewer_ui, 'statedu_t("ui.view_selected_data", language)', "selected-data viewer button translation key")
assert_contains(analysis_data_viewer_ui, "analysis_workspace_heading <- function(title, prefix, language = statedu_initial_language())", "workspace heading helper")
assert_contains(analysis_data_viewer_ui, 'analysis_data_viewer_button(paste0(prefix, "_view_data"), language)', "workspace heading uses shared viewer button")
button_text <- gsub(read_project_file("R/labels.R"), "", r_text, fixed = TRUE)
if (count_fixed(button_text, 'statedu_t("ui.view_selected_data"') != 1L) {
  stop("UI layout contract missing: ui.view_selected_data must be created only by analysis_data_viewer_button()", call. = FALSE)
}
assert_contains(css, ".analysis-workspace-heading", "workspace heading CSS")
assert_contains(css, "justify-content: space-between;", "workspace heading right-edge alignment")
assert_contains(css, ".analysis-data-viewer-button", "selected-data viewer button CSS")

message("Checking Analysis lazy menu wiring...")
analysis_lazy_contract <- data.frame(
  title = c(
    "Frequencies / Descriptives",
    "Cross-tabulation Analysis",
    "t-test / ANOVA",
    "Paired test",
    "ANCOVA",
    "Nonparametric Tests",
    "Nonparametric Paired",
    "Correlation",
    "Reliability",
    "Factor Analysis",
    "Principal Components",
    "Regression",
    "GLM",
    "Logistic Regression",
    "Longitudinal / Panel Models"
  ),
  value = c(
    "Frequencies / Descriptives",
    "analysis_crosstabs",
    "t-test / ANOVA",
    "Paired test",
    "ANCOVA",
    "Nonparametric Tests",
    "Nonparametric Paired",
    "Correlation",
    "Reliability",
    "Factor Analysis",
    "Principal Components",
    "Regression",
    "Generalized Linear Model (GLM)",
    "analysis_logistic_regression",
    "Longitudinal / Panel Models"
  ),
  output_id = c(
    "lazy_analysis_frequencies",
    "lazy_analysis_crosstabs",
    "lazy_analysis_ttest_anova",
    "lazy_analysis_paired",
    "lazy_analysis_ancova",
    "lazy_analysis_nonparametric",
    "lazy_analysis_nonparametric_paired",
    "lazy_analysis_correlation",
    "lazy_analysis_reliability",
    "lazy_analysis_factor_analysis",
    "lazy_analysis_pca",
    "lazy_analysis_hierarchical",
    "lazy_analysis_generalized",
    "lazy_analysis_logistic",
    "lazy_analysis_longitudinal"
  ),
  panel_call = c(
    "frequencies_tab_panel",
    "crosstab_tab_panel",
    "ttest_anova_tab_panel",
    "paired_tab_panel",
    "ancova_tab_panel",
    "nonparametric_tab_panel",
    "nonparametric_paired_tab_panel",
    "correlation_tab_panel",
    "reliability_tab_panel",
    "factor_analysis_tab_panel",
    "pca_tab_panel",
    "hierarchical_tab_panel",
    "generalized_tab_panel",
    "logistic_regression_tab_panel",
    "longitudinal_tab_panel"
  ),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(analysis_lazy_contract))) {
  lazy_row <- analysis_lazy_contract[i, ]
  assert_contains(
    analysis_menu_ui,
    sprintf('"%s", "%s")', lazy_row$value, lazy_row$output_id),
    sprintf("Analysis lazy menu item: %s", lazy_row$title)
  )
  if (identical(lazy_row$output_id, "lazy_analysis_longitudinal")) {
    assert_contains(
      app_server,
      'output$lazy_analysis_longitudinal <- renderUI({',
      "Analysis lazy renderUI target: Longitudinal / Panel Models guarded output"
    )
    assert_contains(
      app_server,
      'statedu_feature_enabled("longitudinal", TRUE)',
      "Analysis lazy renderUI target: Longitudinal / Panel Models public flag guard"
    )
    assert_contains(
      app_server,
      lazy_row$panel_call,
      "Analysis lazy renderUI target: Longitudinal / Panel Models panel call"
    )
  } else {
    assert_contains(
      app_server,
      sprintf("output$%s <- renderUI(tab_panel_content(%s", lazy_row$output_id, lazy_row$panel_call),
      sprintf("Analysis lazy renderUI target: %s", lazy_row$title)
    )
  }
}

message("Checking Complex-sample navigation stability contract...")
complex_lazy_outputs <- c(
  "lazy_analysis_complex_design",
  "lazy_analysis_complex_frequencies",
  "lazy_analysis_complex_crosstabs",
  "lazy_analysis_complex_ttest_anova",
  "lazy_analysis_complex_correlation",
  "lazy_analysis_complex_regression",
  "lazy_analysis_complex_logistic"
)
for (output_id in complex_lazy_outputs) {
  assert_contains(
    app_server,
    sprintf("output$%s <- renderUI(", output_id),
    sprintf("Complex-sample lazy output is registered: %s", output_id)
  )
}
assert_not_contains(
  app_server,
  "outputOptions(output, complex_lazy_output, suspendWhenHidden = FALSE)",
  "Complex-sample lazy outputs must not all render while hidden"
)
assert_not_contains(
  r_text,
  'outputOptions(output, paste0(prefix, "_setup"), suspendWhenHidden = FALSE)',
  "Complex-sample setup output must not force hidden rendering"
)
assert_not_contains(
  r_text,
  'outputOptions(output, paste0(prefix, "_results"), suspendWhenHidden = FALSE)',
  "Complex-sample results output must not force hidden rendering"
)
assert_contains(
  r_text,
  'identical(analysis_type, "crosstabs") && !identical(input$main_menu %||% "", "analysis_complex_crosstabs")',
  "Complex-sample crosstab result cache clears through the Shiny server cycle"
)
assert_not_contains(
  easyflow_js,
  "function easyflowClearComplexCrosstabResultsForNavigation(navValue)",
  "Client must not directly clear complex crosstab output DOM"
)
assert_not_contains(
  easyflow_js,
  "Shiny.setInputValue('complex_crosstab_clear_results_request'",
  "Client must not use a custom crosstab clear-results request"
)
assert_not_contains(
  easyflow_js,
  "complex_crosstab_results",
  "Client must not manipulate the complex crosstab Shiny output element"
)

message("Checking Sample Size / Effect Size lazy menu wiring...")
assert_contains(
  sample_size_ui,
  "sample_size_tab_panel <- function(language = statedu_initial_language()) {\n  methods <- sample_size_method_labels(language)",
  "Sample Size menu uses language-aware sample_size_method_labels() registry"
)
assert_contains(
  sample_size_ui,
  'lazy_tab_panel(title, paste0("sample_size_", method), paste0("lazy_sample_size_", method))',
  "Sample Size lazy menu item ids are generated from method keys"
)
assert_contains(
  sample_size_ui,
  "for (method in methods) {\n    local({\n      method_local <- method\n      output[[paste0(\"lazy_sample_size_\", method_local)]] <- renderUI({\n        tab_panel_content(sample_size_analysis_panel(method_local, sample_size_language()))",
  "Sample Size lazy renderUI targets are generated from method keys and current language"
)
assert_contains(
  sample_size_ui,
  "effect_size_tab_panel <- function(language = statedu_initial_language()) {\n  methods <- effect_size_method_labels(language)",
  "Effect Size menu uses language-aware effect_size_method_labels() registry"
)
assert_contains(
  sample_size_ui,
  'lazy_tab_panel(title, paste0("effect_size_", method), paste0("lazy_effect_size_", method))',
  "Effect Size lazy menu item ids are generated from method keys"
)
assert_contains(
  sample_size_ui,
  "for (effect_method in names(effect_size_method_labels())) {\n    local({\n      effect_method_local <- effect_method\n      output[[paste0(\"lazy_effect_size_\", effect_method_local)]] <- renderUI({\n        tab_panel_content(effect_size_analysis_panel(effect_method_local, sample_size_language()))",
  "Effect Size lazy renderUI targets are generated from method keys and current language"
)

message("Checking shared analysis menu geometry contract...")
assert_contains(setup_ui, 'analysis_workspace_heading("t-test / ANOVA", "ttest_anova", language)', "t-test / ANOVA baseline heading")
assert_contains(setup_ui, 'class = "analysis-action-row ttest-anova-action-row"', "t-test / ANOVA baseline action row")
assert_contains(css, "body {\n  --se-standard-panel-width: 326px;", "global standard layout variables")
assert_contains(css, "--se-analysis-workspace-width: 1140px;", "standard analysis workspace width variable")
assert_contains(css, "--se-analysis-grid-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) 20px var(--se-standard-options-width);", "standard analysis grid column variable")
assert_contains(css, "--se-standard-four-column-grid-width: calc((var(--se-standard-panel-width) * 2) + var(--se-standard-transfer-width) + var(--se-standard-options-width) + (var(--se-standard-gap) * 3));", "frequency-style four-column grid width variable")
assert_contains(css, "--se-standard-four-column-workspace-width: calc(var(--se-standard-four-column-setup-width) + (var(--se-standard-workspace-padding) * 2));", "frequency-style workspace width variable")
assert_contains(css, ".ttest-anova-setup-grid,", "t-test / ANOVA setup grid shares standard analysis geometry")
assert_contains(css, ".ttest-anova-action-row,", "t-test / ANOVA action row shares standard analysis geometry")
assert_contains(css, ".hierarchical-setup-grid,\n.regression-setup-grid {\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "shared analysis setup grid uses standard variables")
assert_contains(css, ".hierarchical-action-row,\n.regression-action-row {\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "shared analysis action row uses standard variables")
assert_contains(css, ".reliability-action-row > .btn,\n.frequencies-action-row > .btn,", "shared analysis action button selector")
assert_contains(css, "width: var(--se-standard-inner-button-width, 300px);\n  min-width: var(--se-standard-inner-button-width, 300px);", "shared analysis action buttons use standard width")
assert_contains(css, "width: var(--se-analysis-workspace-width) !important;", "standard analysis setup/action width variable")
assert_contains(css, ".analysis-three-block-workspace .analysis-workspace-heading,\n.analysis-three-block-workspace .analysis-data-viewer-panel", "standard analysis heading width selector")
assert_contains(css, ".analysis-three-block-workspace .analysis-workspace-heading,\n.analysis-three-block-workspace .analysis-data-viewer-panel {\n  width: 100% !important;", "final shared analysis heading uses scoped workspace width")
assert_contains(css, ".analysis-three-block-workspace .frequencies-setup-grid,\n.analysis-three-block-workspace .reliability-setup-grid,\n.analysis-three-block-workspace .paired-setup-grid,\n.analysis-three-block-workspace .ttest-anova-setup-grid,\n.analysis-three-block-workspace .correlation-setup-grid,\n.analysis-three-block-workspace .frequencies-action-row,\n.analysis-three-block-workspace .reliability-action-row,\n.analysis-three-block-workspace .paired-action-row,\n.analysis-three-block-workspace .ttest-anova-action-row,\n.analysis-three-block-workspace .correlation-action-row {\n  grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) var(--se-standard-options-width) !important;", "final shared four-column analysis grids use standard variables")
assert_contains(css, ".analysis-three-block-workspace .crosstab-setup-grid,\n.analysis-three-block-workspace .regression-setup-grid,\n.analysis-three-block-workspace .hierarchical-setup-grid,\n.analysis-three-block-workspace .logistic-setup-grid,\n.analysis-three-block-workspace .generalized-setup-grid,\n.analysis-three-block-workspace .crosstab-action-row,\n.analysis-three-block-workspace .regression-action-row,\n.analysis-three-block-workspace .hierarchical-action-row,\n.analysis-three-block-workspace .logistic-action-row,\n.analysis-three-block-workspace .generalized-action-row {\n  grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) var(--se-standard-options-width) !important;", "final shared crosstab/regression analysis grids use frequency-style variables")
assert_contains(css, ".analysis-three-block-workspace .frequencies-setup-grid > .analysis-options-panel,\n.analysis-three-block-workspace .reliability-setup-grid > .analysis-options-panel,\n.analysis-three-block-workspace .paired-setup-grid > .ttest-anova-options-column,\n.analysis-three-block-workspace .ttest-anova-setup-grid > .ttest-anova-options-column,\n.analysis-three-block-workspace .correlation-setup-grid > .correlation-options-column {\n  grid-column: 4 !important;\n  grid-row: 1 !important;", "final shared analysis options stay in top row")
assert_contains(css, ".interrater-agreement-setup-grid > .interrater-options-panel {\n  display: flex !important;\n  flex-direction: column !important;\n  gap: 6px !important;", "Inter-rater options override shared options column gap")
assert_contains(css, ".complex-sample-workspace-panel .complex-sample-setup-grid > .complex-sample-analysis-options-column {\n  grid-column: 4 !important;\n  grid-row: 1 !important;", "complex sample analysis options stay in top row")
assert_contains(css, ".longitudinal-action-row {\n  grid-template-columns: 330px 40px 320px 40px 320px 330px !important;", "Longitudinal four-block action row exception")
assert_contains(css, ".longitudinal-setup-grid {\n  display: grid;\n  grid-template-areas: \"available panel-move panel model-move model options\";", "Longitudinal four-block setup exception")
assert_contains(css, ".ancova-action-row {\n  display: grid !important;\n  grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) var(--se-standard-options-width) !important;\n  gap: var(--se-standard-gap) !important;", "ANCOVA action row uses frequency-style standard variables")
assert_contains(css, "width: var(--se-standard-four-column-setup-width) !important;\n  min-width: var(--se-standard-four-column-setup-width) !important;\n  max-width: var(--se-standard-four-column-setup-width) !important;\n  align-items: start !important;\n}\n\n.ancova-action-row > #run_ancova", "ANCOVA action row uses frequency-style standard width")
assert_count_exact(css, "1138px", 0L, "no legacy hardcoded analysis workspace width")
assert_count_exact(css, "grid-template-columns: 326px 50px 326px 20px 310px;", 1L, "calculator-only hardcoded standard columns")
assert_count_exact(css, "\n  width: 1176px;", 1L, "calculator-only hardcoded standard setup width")
assert_count_exact(css, "\n  min-width: 1176px;", 1L, "calculator-only hardcoded standard setup min-width")
assert_contains(css, ".calculator-action-row {\n  display: grid;\n  grid-template-columns: 326px 50px 326px 20px 310px;\n  gap: 18px;\n  align-items: center;\n  width: 1176px;\n  min-width: 1176px;", "calculator action row owns the only hardcoded standard-width exception")
assert_contains(css, ".sample-size-grid {\n  display: grid;\n  grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) 20px minmax(var(--se-standard-options-width), 1fr);", "Sample Size grid uses shared standard variables")
assert_count_exact(css, "grid-template-columns: 326px 50px 326px 20px minmax(310px, 1fr);", 0L, "no hardcoded sample-size standard grid")
assert_count_exact(css, "grid-template-columns: 326px 50px 326px 310px;", 0L, "no hardcoded standard four-column grid")
assert_count_exact(css, "grid-template-columns: 50px 326px;", 0L, "no hardcoded standard transfer-target row")
assert_count_exact(css, "\n  width: 326px;", 0L, "no hardcoded standard panel width")
assert_count_exact(css, "\n  min-width: 326px;", 0L, "no hardcoded standard panel min-width")
assert_count_exact(css, "\n  max-width: 326px;", 0L, "no hardcoded standard panel max-width")
assert_count_exact(css, "\n  width: 310px;", 0L, "no hardcoded standard options width")
assert_count_exact(css, "\n  min-width: 310px;", 0L, "no hardcoded standard options min-width")
assert_count_exact(css, "\n  max-width: 310px;", 0L, "no hardcoded standard options max-width")
assert_count_exact(css, "\n  height: 520px;", 0L, "no hardcoded standard panel height")
assert_count_exact(css, "\n  min-height: 520px;", 0L, "no hardcoded standard panel min-height")
assert_count_exact(css, "\n  max-height: 520px;", 0L, "no hardcoded standard panel max-height")
assert_count_exact(css, "grid-auto-rows: 520px;", 0L, "no hardcoded standard panel grid row height")
assert_count_exact(css, "\n  height: 520px !important;", 0L, "no hardcoded important standard panel height")
assert_count_exact(css, "\n  min-height: 520px !important;", 0L, "no hardcoded important standard panel min-height")
assert_count_exact(css, "\n  max-height: 520px !important;", 0L, "no hardcoded important standard panel max-height")
assert_count_exact(css, "grid-auto-rows: 520px !important;", 0L, "no hardcoded important standard panel grid row height")
assert_contains(css, ".variable-transform-grid {\n  display: grid;\n  grid-template-columns: var(--se-standard-panel-width) minmax(0, calc(var(--se-analysis-workspace-width) - var(--se-standard-panel-width) - var(--se-standard-gap)));", "Variable transform grid derives columns from shared variables")
assert_count_exact(css, "grid-template-columns: 326px minmax(0, 796px);", 0L, "no hardcoded variable-transform grid columns")
assert_count_exact(css, "\n  width: 1140px;", 0L, "no hardcoded analysis workspace width")
assert_count_exact(css, "\n  min-width: 1140px;", 0L, "no hardcoded analysis workspace min-width")
assert_count_exact(css, "\n  max-width: 1140px;", 0L, "no hardcoded analysis workspace max-width")
assert_count_exact(css, "\n  max-width: 1176px;", 0L, "no hardcoded standard setup max-width")

message("Checking Wide to Long menu contract...")
assert_contains(data_editor_ui, 'statedu_t("data_editor.wide_long_title", language)', "Data Editor menu label")
assert_contains(wide_long_ui, 'h1(statedu_t("data_editor.wide_long_title", language))', "Wide to Long page title")
assert_contains(wide_long_ui, 'analysis_workspace_heading(statedu_t("data_editor.wide_long_heading", language)', "Wide to Long workspace heading")
assert_not_contains(wide_long_ui, "Wide to Long Format", "old Wide to Long Format label")
assert_not_contains(css, ".wide_long-workspace-heading", "Wide to Long-specific workspace heading override")
assert_not_contains(css, ".recode-same-action-row.wide-long-action-row {\n  display: grid;", "legacy unscoped Wide to Long action-row override")
assert_contains(wide_long_ui, 'class = "analysis-action-row recode-same-action-row wide-long-action-row"', "Wide to Long action row")
assert_contains(wide_long_ui, 'actionButton("run_wide_long", analysis_ui_text("Run", language)', "Wide to Long run button")
assert_contains(wide_long_ui, 'actionButton("wide_long_remove_spec", analysis_ui_text("Remove", language)', "Wide to Long remove button")
assert_contains(wide_long_ui, 'actionButton("preview_wide_long", analysis_ui_text("Preview", language)', "Wide to Long preview button")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #run_wide_long", "Wide to Long run placement")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #wide_long_remove_spec", "Wide to Long remove placement")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #preview_wide_long", "Wide to Long preview placement")
assert_contains(css, ".data-editor-workspace .wide-long-set-button", "Wide to Long set button placement")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #run_wide_long {\n  grid-column: 1 !important;\n}", "Wide to Long run button column")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #wide_long_remove_spec {\n  grid-column: 3 !important;\n}", "Wide to Long remove button column")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #preview_wide_long {\n  grid-column: 4 !important;\n}", "Wide to Long preview button column")
assert_contains(css, ".data-editor-workspace .wide-long-set-button {\n  margin-top: auto !important;\n  margin-bottom: 28px !important;", "Wide to Long set button vertical anchor")
assert_contains(css, "width: var(--se-standard-options-button-width) !important;\n  min-width: var(--se-standard-options-button-width) !important;\n  max-width: var(--se-standard-options-button-width) !important;", "Wide to Long set/preview standard button width")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #preview_wide_long {\n  width: var(--se-standard-options-button-width) !important;\n  min-width: var(--se-standard-options-button-width) !important;\n  max-width: var(--se-standard-options-button-width) !important;\n}", "Wide to Long preview matches Block 3 button width")
assert_contains(css, ".data-editor-workspace .wide-long-action-row {\n  grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) var(--se-standard-options-width) !important;", "Wide to Long action row standard columns")

message("Checking Rename Variable menu contract...")
assert_contains(rename_ui, 'class = "analysis-action-row recode-same-action-row variable-rename-action-row"', "Rename action row")
assert_contains(rename_ui, 'actionButton("run_variable_rename", analysis_ui_text("Run", language)', "Rename run button")
assert_contains(rename_ui, 'actionButton("remove_variable_rename", analysis_ui_text("Remove", language)', "Rename remove button")
assert_contains(rename_ui, 'class = "recode-same-setup-grid variable-rename-grid"', "Rename standard grid")
assert_contains(css, ".data-editor-workspace .variable-rename-target-panel .analysis-transfer-listbox", "Rename target list height")
assert_contains(css, ".data-editor-workspace .variable-rename-action-row > #remove_variable_rename", "Rename remove placement")
assert_contains(css, ".data-editor-workspace .variable-rename-action-row > #remove_variable_rename {\n  grid-column: 3 !important;\n  justify-self: start !important;\n  margin-left: 30px !important;\n  width: var(--se-standard-inner-button-width) !important;", "Rename remove uses Block 2 action placement")

message("Checking Recode Variable menu contract...")
assert_contains(recode_ui, 'class = "recode-same-setup-grid recode-builder-grid"', "Recode builder grid")
assert_contains(recode_ui, 'class = "analysis-action-row recode-same-action-row recode-builder-action-row"', "Recode action row")
assert_contains(recode_ui, 'actionButton("apply_recode_same", analysis_ui_text("Apply", language)', "Recode apply button")
assert_contains(recode_ui, 'uiOutput("recode_same_reset_control")', "Recode reset control")
assert_contains(css, ".data-editor-workspace .recode-builder-action-row > #apply_recode_same", "Recode apply placement")
assert_contains(css, ".data-editor-workspace .recode-builder-action-row > #recode_same_reset_control", "Recode reset placement")
assert_contains(css, ".data-editor-workspace .recode-builder-action-row > #recode_same_reset_control {\n  grid-column: 3 !important;\n  justify-self: start !important;\n  margin-left: 30px !important;\n  width: var(--se-standard-inner-button-width) !important;", "Recode reset uses Block 2 action placement")

message("Checking Auto Reverse Coding menu contract...")
assert_contains(data_editor_ui, 'statedu_t("data_editor.reverse_title", language)', "Auto Reverse Coding menu label")
assert_contains(recode_ui, 'analysis_workspace_heading("Auto reverse coding", "recode_different", language = language)', "Auto Reverse Coding workspace heading")
assert_contains(recode_ui, 'class = "recode-same-setup-grid recode-different-setup-grid"', "Auto Reverse Coding standard grid")
assert_contains(recode_ui, '"recode_different_move",', "Auto Reverse Coding transfer button")
assert_contains(recode_ui, 'actionButton("apply_recode_different", analysis_ui_text("Run", language)', "Auto Reverse Coding run button")
assert_contains(recode_ui, 'uiOutput("recode_different_reset_control")', "Auto Reverse Coding reset control")

message("Checking Auto Variable Calculation menu contract...")
assert_contains(data_editor_ui, 'statedu_t("data_editor.calculation_title", language)', "Auto Variable Calculation menu label")
assert_contains(recode_ui, 'analysis_workspace_heading("Auto variable calculation", "variable_calculation", language = language)', "Auto Variable Calculation workspace heading")
assert_contains(recode_ui, 'class = "recode-same-setup-grid recode-different-setup-grid"', "Auto Variable Calculation standard grid")
assert_contains(recode_ui, '"variable_calculation_move",', "Auto Variable Calculation transfer button")
assert_contains(recode_ui, 'actionButton("apply_variable_calculation", analysis_ui_text("Run", language)', "Auto Variable Calculation run button")
assert_contains(recode_ui, 'uiOutput("variable_calculation_reset_control")', "Auto Variable Calculation reset control")

message("Checking Data Editor exception menu contract...")
assert_contains(likert_ui, 'analysis_workspace_heading(statedu_t("data_editor.likert_label_conversion"', "Likert exception uses shared workspace heading")
assert_contains(transform_ui, 'analysis_workspace_heading("Variable transformation", "variable_transform", language = language)', "Variable Transformation exception uses shared workspace heading")
assert_contains(transform_ui, 'class = "variable-transform-grid"', "Variable Transformation exception owns a custom formula-builder grid")
assert_not_contains(likert_ui, 'class = "recode-same-setup-grid', "Likert exception does not masquerade as standard three-block grid")
assert_not_contains(transform_ui, 'class = "recode-same-setup-grid', "Variable Transformation exception does not masquerade as standard three-block grid")

message("Checking Auto Coding Error menu contract...")
assert_contains(recode_ui, 'analysis_workspace_heading("Auto coding error check", "coding_error", language = language)', "Coding error workspace heading")
assert_contains(recode_ui, 'class = "recode-same-setup-grid recode-different-setup-grid"', "Coding error standard grid")
assert_contains(recode_ui, '"coding_error_move",', "Coding error transfer button")
assert_contains(recode_ui, 'class = "analysis-action-row recode-same-action-row"', "Coding error action row")
assert_contains(recode_ui, 'actionButton("apply_coding_error", analysis_ui_text("Run", language)', "Coding error run button")
assert_contains(recode_ui, 'uiOutput("coding_error_reset_control")', "Coding error reset control")

message("Checking Auto Missing Values menu contract...")
assert_contains(missing_ui, 'analysis_workspace_heading("Auto missing value detection", "missing_values", language = language)', "Missing values workspace heading")
assert_contains(missing_ui, 'class = "recode-same-setup-grid missing-values-setup-grid"', "Missing values standard grid")
assert_contains(missing_ui, '"missing_values_move",', "Missing values transfer button")
assert_contains(missing_ui, 'class = "analysis-action-row recode-same-action-row missing-values-action-row"', "Missing values action row")
assert_contains(missing_ui, 'actionButton("mark_user_missing_values", analysis_ui_text("Mark as user missing", language)', "Missing values primary action")
assert_contains(missing_ui, 'actionButton("convert_missing_values_to_na", analysis_ui_text("Convert to NA", language)', "Missing values secondary action")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row) > .btn,", "standard direct action placement")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row) > .missing-values-action-cell", "Missing values action-cell placement")

message("Checking grouped menu navigation contract...")
assert_contains(easyflow_js, "function easyflowGroupedMenuConfigs()", "grouped menu configuration")
assert_contains(easyflow_js, "function markNavbarDropdownActive(link)", "grouped menu top-level active helper")
assert_contains(easyflow_js, "dropdown.closest('.navbar-nav').children('li.active').removeClass('active');", "grouped menu clears stale top-level active state")
assert_contains(easyflow_js, "dropdown.addClass('active');", "grouped menu activates clicked top-level menu")
assert_contains(easyflow_js, "menu: 'Analysis'", "Analysis grouped menu config label")
assert_contains(easyflow_js, "menuLabels: ['Analysis'", "Analysis grouped menu translated labels")
assert_contains(easyflow_js, "marker: 'analysis'", "Analysis grouped menu config marker")
assert_contains(easyflow_js, "analysis_custom_model_canvas: 'Mediation / Moderation Custom Model'", "Analysis grouped menu custom model English label")
assert_contains(easyflow_js, "analysis_custom_model_canvas: '\\uB9E4\\uAC1C\\u00B7\\uC870\\uC808 \\uC0AC\\uC6A9\\uC790 \\uC815\\uC758 \\uBAA8\\uB378'", "Analysis grouped menu custom model Korean label")
assert_contains(easyflow_js, "values: ['Regression', 'analysis_mediation_moderation', 'analysis_custom_model_canvas', 'Generalized Linear Model (GLM)', 'analysis_logistic_regression']", "Analysis grouped menu includes custom model in Regression / Models")
assert_contains(easyflow_js, "menu: 'Sample Size'", "Sample Size grouped menu config label")
assert_contains(easyflow_js, "menuLabels: ['Sample Size'", "Sample Size grouped menu translated labels")
assert_contains(easyflow_js, "marker: 'sample-size'", "Sample Size grouped menu config marker")
assert_contains(easyflow_js, "menu: 'Effect Size'", "Effect Size grouped menu config label")
assert_contains(easyflow_js, "menuLabels: ['Effect Size'", "Effect Size grouped menu translated labels")
assert_contains(easyflow_js, "marker: 'effect-size'", "Effect Size grouped menu config marker")
assert_contains(easyflow_js, "click.easyflowAnalysisSubmenu", "grouped submenu click handler")
assert_contains(easyflow_js, "click.easyflowAnalysisDirectItem", "grouped direct-item click handler")
assert_contains(easyflow_js, "function syncEasyflowTopNavbarActive(link) {", "top navbar active-state sync helper")
assert_contains(easyflow_js, "event.target.closest('.navbar-nav a[data-value]')", "generic navbar data-value click handler")
assert_contains(easyflow_js, "link.tab('show');", "generic navbar click reopens already-active tab")
assert_contains(easyflow_js, "syncEasyflowTopNavbarActive(navLink);", "generic navbar click updates top-level active state")
submenu_click_handler <- extract_between(
  easyflow_js,
  ".on('click.easyflowAnalysisSubmenu', '.analysis-submenu .analysis-menu-section-items a[data-value]'",
  ".on('click.easyflowAnalysisDirectItem'",
  "grouped submenu click handler"
)
direct_click_handler <- extract_between(
  easyflow_js,
  ".on('click.easyflowAnalysisDirectItem', '.analysis-submenu > li.analysis-menu-direct-item > a[data-value]'",
  ".on('hidden.bs.dropdown.easyflowNestedDropdown'",
  "grouped direct item click handler"
)
assert_contains(submenu_click_handler, "link.closest('.analysis-menu-section').addClass('active open');", "grouped submenu active section marker")
assert_contains(submenu_click_handler, "markNavbarDropdownActive(link);", "grouped submenu activates owning top-level menu")
assert_contains(submenu_click_handler, "window.setTimeout(markActive, 0);", "grouped submenu reapplies active state after Shiny navigation")
assert_contains(submenu_click_handler, "link.closest('.navbar-nav > li.dropdown').removeClass('open');", "grouped submenu closes after selection")
assert_contains(direct_click_handler, "link.parent('li').addClass('active');", "grouped direct item active marker")
assert_contains(direct_click_handler, "markNavbarDropdownActive(link);", "grouped direct item activates owning top-level menu")
assert_contains(direct_click_handler, "window.setTimeout(markActive, 0);", "grouped direct item reapplies active state after Shiny navigation")
assert_contains(direct_click_handler, "link.closest('.navbar-nav > li.dropdown').removeClass('open');", "grouped direct item closes after selection")

message("Checking layout documentation...")
assert_contains(layout_doc, "Standard analysis and Data Editor menus use shared geometry variables declared\non `body` in `www/style.css`", "documented global standard geometry variables")
assert_contains(layout_doc, "frequency-style tools use `1138px`", "documented frequency-style workspace width")
assert_contains(layout_doc, "frequency-style tools use `1102px`, including setup-grid padding where\n  relevant", "documented frequency-style setup width")
assert_contains(layout_doc, "Every standard Data Editor tool must render inside the\n  `.data-editor-workspace` wrapper", "documented Data Editor workspace wrapper requirement")
assert_contains(layout_doc, "must reference the standard variables instead of hard-coded copies", "documented standard variable requirement")
assert_contains(layout_doc, "Button row: Block 1 commands use column 1, Block 2 commands use column 3,\n  and Block 3 commands use column 4", "documented button-row column contract")
assert_contains(layout_doc, "Footer button widths: Block 1 and Block 2 commands use\n  `--se-standard-inner-button-width`; Block 3 commands use\n  `--se-standard-options-button-width`.", "documented footer button width contract")
assert_contains(layout_doc, "`Wide to Long` is a standard Data Editor three-block tool, not a layout\nexception.", "documented Wide to Long standard layout classification")
assert_contains(layout_doc, "`Run` sits under Block 1.", "documented Wide to Long run placement")
assert_contains(layout_doc, "`Remove` sits under Block 2.", "documented Wide to Long remove placement")
assert_contains(layout_doc, "`Preview` sits under Block 3.", "documented Wide to Long preview placement")
assert_contains(layout_doc, "`Set variable` stays inside Block 3, uses the standard Block 3 button width,\n  and is anchored to the bottom of the options content", "documented Wide to Long Set variable anchor")
assert_contains(layout_doc, "Longitudinal / Panel Models: four-block analysis structure", "documented four-block exception")
assert_contains(layout_doc, "Auto Likert conversion: detection/review workflow", "documented Likert layout exception")
assert_contains(layout_doc, "Variable transformation: formula-builder workflow", "documented Variable Transformation layout exception")
assert_contains(layout_doc, "The only remaining hard-coded copy of the standard three-block width is\n`.calculator-action-row`", "documented calculator hardcoded-width exception")
assert_contains(layout_doc, "## Navigation Contract", "documented navigation contract")
assert_contains(layout_doc, "Analysis, Sample Size, and Effect Size use grouped dropdown sections.", "documented grouped menu scope")
assert_contains(layout_doc, "Clicking a navbar item that is already marked active must still call the tab\n  navigation path", "documented already-active navbar click recovery")
assert_contains(layout_doc, "Data Editor lazy menu items must map one menu item to one server-side lazy\n  output target.", "documented Data Editor lazy menu mapping")
assert_contains(layout_doc, "scripts/validate_stabilization.ps1", "documented stabilization validation command")

cat("UI layout contract validation passed.\n")
