suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))
invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))

`%||%` <- function(x, y) if (is.null(x)) y else x
statedu_initial_language <- function(...) "ko"
normalize_app_language <- function(language) if (tolower(as.character(language[[1]] %||% "ko")) == "ko") "ko" else "en"
statedu_current_language <- function(language_fn = NULL) if (is.function(language_fn)) language_fn() else "ko"
with_datatable_language <- function(options, language = "ko") options
read_csv_robust <- function(path, csv_header = TRUE) utils::read.csv(path, header = csv_header, check.names = FALSE, stringsAsFactors = FALSE)
analysis_output_table_style <- function(value, default = "standard") as.character(value %||% default)[[1]]
analysis_output_table_style_params <- function(value) list(compact = FALSE, font_size = 12, compact_width = 62, compact_first_width = 118, min_width = 480)
analysis_three_block_action_row <- function(class = "", run_button, reset_control = NULL, save_control = NULL, extra_controls = NULL) {
  div(class = class, run_button, reset_control, extra_controls, save_control)
}

source(file.path("R", "analysis_meta.R"), encoding = "UTF-8")
source(file.path("R", "analysis_scope.R"), encoding = "UTF-8")
source(file.path("R", "analysis_syntax.R"), encoding = "UTF-8")
source(file.path("R", "analysis_commands.R"), encoding = "UTF-8")
source(file.path("R", "result_table_ui.R"), encoding = "UTF-8")
source(file.path("R", "setup_meta_ui.R"), encoding = "UTF-8")
source(file.path("R", "server_meta.R"), encoding = "UTF-8")

meta_css <- paste(readLines(file.path("www", "style.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(grepl(".meta-analysis-input-block > .sample-size-method-note", meta_css, fixed = TRUE))
stopifnot(grepl("font-size: 14px !important", meta_css, fixed = TRUE))
stopifnot(grepl("grid-template-columns: repeat(3, minmax(0, 1fr))", meta_css, fixed = TRUE))
stopifnot(grepl(".analysis-three-block-workspace .meta-analysis-action-row", meta_css, fixed = TRUE))
stopifnot(grepl("padding-right: 19px;\n  padding-left: 19px;", meta_css, fixed = TRUE))
stopifnot(grepl(".analysis-three-block-workspace .meta-analysis-action-row .analysis-extra-cell,", meta_css, fixed = TRUE))
stopifnot(grepl(".meta-analysis-input-block .shiny-input-container {\n  width: 100% !important;", meta_css, fixed = TRUE))
stopifnot(grepl(".meta-download-actions {\n  display: grid;\n  grid-template-columns: repeat(2, minmax(0, 1fr));", meta_css, fixed = TRUE))
stopifnot(grepl(".meta-validation-summary {\n  display: flex;\n  flex-wrap: nowrap;", meta_css, fixed = TRUE))
stopifnot(grepl(".meta-validation-summary > p {\n  margin: 0;\n  white-space: nowrap;", meta_css, fixed = TRUE))

meta_server_source <- paste(readLines(file.path("R", "server_meta.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(grepl('class = "sample-size-result-list meta-validation-summary"', meta_server_source, fixed = TRUE))

ui <- meta_analysis_tab_panel("ko")
stopifnot(inherits(ui, "shiny.tag"))
stopifnot(grepl("메타분석", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta-analysis-setup-grid", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta_run_analysis", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta_funnel_plot_enabled", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta_egger_test_enabled", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta_moderator_selector", as.character(ui), fixed = TRUE))
stopifnot(grepl("RVE, CR2", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta_rve_compare", as.character(ui), fixed = TRUE))
stopifnot(grepl("CR1·CR2·CR3", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta-analysis-input-block", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta-analysis-additional-block", as.character(ui), fixed = TRUE))
stopifnot(grepl("1. 효과크기 입력", as.character(ui), fixed = TRUE))
stopifnot(grepl("2. 기본 분석 설정", as.character(ui), fixed = TRUE))
stopifnot(grepl("3. 추가 분석", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta_download_effects", as.character(ui), fixed = TRUE))
stopifnot(grepl("입력자료 CSV 저장", as.character(ui), fixed = TRUE))
stopifnot(grepl("Excel 템플릿 다운로드", as.character(ui), fixed = TRUE))
stopifnot(grepl("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", as.character(ui), fixed = TRUE))
modal_markup <- as.character(meta_effect_modal("g", NULL, "ko"))
stopifnot(grepl("meta_field_study_name", modal_markup, fixed = TRUE))
stopifnot(grepl("meta_field_publication_year", modal_markup, fixed = TRUE))
stopifnot(grepl("meta_field_predictor", modal_markup, fixed = TRUE))
stopifnot(grepl("meta_group_mode", as.character(ui), fixed = TRUE))
stopifnot(grepl("NEGATIVE", modal_markup, fixed = TRUE))
stopifnot(grepl("meta_dependency_method", as.character(ui), fixed = TRUE))
stopifnot(grepl("meta_trimfill_enabled", as.character(ui), fixed = TRUE))

template_expectations <- list(
  g = c("means", "g_se", "g_ci", "d", "t", "r_pb"),
  r = c("r", "z_se", "t", "partial_r"),
  or = c("2x2", "or_ci", "logor_se", "logistic_b")
)
for (family in names(template_expectations)) {
  template_asset <- meta_excel_template_asset(family)
  stopifnot(file.exists(template_asset))
  workbook_sheets <- readxl::excel_sheets(template_asset)
  workbook_settings <- meta_excel_workbook_settings(template_asset, workbook_sheets)
  stopifnot(identical(workbook_settings$mode, "ENTRY_MODE"), identical(workbook_settings$family, family))
  xlsx_data <- meta_read_effect_input_file(list(name = basename(template_asset), datapath = template_asset), family)
  xlsx_effects <- meta_import_effects(xlsx_data, family)
  stopifnot(
    nrow(xlsx_effects) == length(template_expectations[[family]]),
    identical(xlsx_effects$input_type, unname(template_expectations[[family]])),
    all(xlsx_effects$included),
    all(xlsx_effects$status == "valid")
  )
}
stopifnot(identical(
  gsub("^_+|_+$", "", gsub("[^A-Z0-9]+", "_", toupper("Entry mode"))),
  "ENTRY_MODE"
))
stopifnot(grepl("meta_field_moderator_categorical", modal_markup, fixed = TRUE))
stopifnot(grepl("meta_field_moderator_continuous", modal_markup, fixed = TRUE))
means_fields_markup <- as.character(meta_effect_fields_ui("g", "means", NULL, "ko"))
stopifnot(grepl("집단 1(실험집단) 평균 (M1)", means_fields_markup, fixed = TRUE))
stopifnot(grepl("집단 0(대조집단) 평균 (M0)", means_fields_markup, fixed = TRUE))
stopifnot(length(meta_input_type_choices("g", "ko")) == 6L)
stopifnot(length(meta_input_type_choices("r", "ko")) == 4L)
stopifnot(length(meta_input_type_choices("or", "ko")) == 4L)
stopifnot(identical(names(meta_input_type_choices("g", "ko"))[[1]], "두 집단 평균·SD·표본 수"))

meta_test_server <- function(input, output, session) {
  session$userData$meta_state <- register_meta_server(input, output, session, app_language_fn = function() "ko")
}

shiny::testServer(
  meta_test_server,
  {
    session$setInputs(meta_target_family = "g")
    session$setInputs(meta_add_effect = 1)
    session$setInputs(
      meta_input_type = "means",
      meta_field_study_id = "UI-1",
      meta_field_study_name = "Kim et al.",
      meta_field_publication_year = 2024,
      meta_field_outcome = "Depression",
      meta_field_predictor = "Treatment A",
      meta_field_moderator_categorical = "region=Asia; design=RCT",
      meta_field_moderator_continuous = "mean_age=42.5",
      meta_field_included = TRUE,
      meta_field_direction = "positive",
      meta_field_m1 = 12,
      meta_field_sd1 = 3,
      meta_field_n1 = 40,
      meta_field_m0 = 10,
      meta_field_sd0 = 3,
      meta_field_n0 = 40
    )
    session$setInputs(meta_save_effect = 1)
    current_effects <- session$userData$meta_state$effects()
    stopifnot(nrow(current_effects) == 1L)
    stopifnot(current_effects$study_id[[1]] == "UI-1")
    stopifnot(current_effects$study_name[[1]] == "Kim et al.", current_effects$publication_year[[1]] == 2024)
    stopifnot(nrow(meta_moderators_long(current_effects)) == 3L)
    stopifnot(current_effects$status[[1]] == "valid")
    stopifnot(is.finite(current_effects$yi[[1]]), is.finite(current_effects$vi[[1]]))
    display_table <- meta_effect_display_table(current_effects, "g", "ko")
    stopifnot(identical(display_table$`보고 형식`[[1]], "두 집단 평균·SD·표본 수"))

    session$setInputs(meta_add_effect = 2)
    session$setInputs(
      meta_input_type = "g_se",
      meta_field_study_id = "UI-1B",
      meta_field_study_name = "Lee et al.",
      meta_field_publication_year = 2023,
      meta_field_outcome = "Depression",
      meta_field_predictor = "Treatment B",
      meta_field_moderator_categorical = "region=Europe; design=RCT",
      meta_field_moderator_continuous = "mean_age=39.0",
      meta_field_included = TRUE,
      meta_field_direction = "positive",
      meta_field_g = 0.45,
      meta_field_se = 0.12
    )
    session$setInputs(meta_save_effect = 2)
    current_effects <- session$userData$meta_state$effects()
    stopifnot(nrow(current_effects) == 2L)
    two_row_display <- meta_effect_display_table(current_effects, "g", "ko")
    stopifnot(nrow(two_row_display) == 2L)
    stopifnot(all(nzchar(two_row_display$`Hedges' g`)), all(nzchar(two_row_display$`분석척도 SE`)))
    session$setInputs(
      meta_model = "random",
      meta_tau_method = "REML",
      meta_conf_level = 0.95,
      meta_prediction_interval = TRUE,
      meta_group_mode = "overall",
      meta_dependency_method = "auto",
      meta_funnel_plot_enabled = TRUE,
      meta_egger_test_enabled = TRUE,
      meta_trimfill_enabled = TRUE
    )
    session$setInputs(meta_run_analysis = 1)
    fitted <- session$userData$meta_state$analysis_result()
    stopifnot(inherits(fitted, "statedu_meta_model"), fitted$k == 2L)
    stopifnot(isTRUE(fitted$show_funnel), isTRUE(fitted$show_egger), !isTRUE(fitted$egger$available))
    result_markup <- as.character(meta_analysis_results_ui(fitted, "ko"))
    stopifnot(grepl("Pooled effect", result_markup, fixed = TRUE))
    stopifnot(grepl("Heterogeneity", result_markup, fixed = TRUE))
    stopifnot(grepl("연구별 효과크기", result_markup, fixed = TRUE))
    stopifnot(grepl("Egger 회귀 검정에는 최소 3개 연구가 필요합니다.", result_markup, fixed = TRUE))
    stopifnot(grepl('data-result-table-role="main"', result_markup, fixed = TRUE))
    stopifnot(grepl('data-result-table-role="appendix"', result_markup, fixed = TRUE))
    stopifnot(grepl('data-result-table-language="en"', result_markup, fixed = TRUE))
    stopifnot(grepl('data-result-table-language="ko"', result_markup, fixed = TRUE))
    stopifnot(grepl("result-table-sheet--b5", result_markup, fixed = TRUE))
    stopifnot(grepl("Forest plot", result_markup, fixed = TRUE), grepl("Funnel plot", result_markup, fixed = TRUE))
    stopifnot(!grepl("포리스트 플롯", result_markup, fixed = TRUE), !grepl("퍼널 플롯", result_markup, fixed = TRUE))
    pooled_markup <- as.character(meta_result_section("Pooled effect", meta_model_summary_table(fitted, "en"), "main", "en"))
    stopifnot(grepl("result-table-sheet--portrait", pooled_markup, fixed = TRUE))
    stopifnot(grepl('data-result-table-language="en"', pooled_markup, fixed = TRUE))
    stopifnot(!grepl("모형|연구 수|통합 추정치", pooled_markup))
    forest_labels <- meta_forest_study_labels(fitted$studies)
    stopifnot(identical(forest_labels, c("Kim et al. (2024)", "Lee et al. (2023)")))
    stopifnot(!any(grepl("Treatment|Depression|→", forest_labels)))

    session$setInputs(meta_add_effect = 3)
    session$setInputs(
      meta_input_type = "g_se", meta_field_study_id = "UI-1C", meta_field_study_name = "Choi et al.",
      meta_field_publication_year = 2022, meta_field_outcome = "Depression", meta_field_included = TRUE,
      meta_field_predictor = "Treatment A",
      meta_field_direction = "positive", meta_field_moderator_categorical = "region=Asia; design=RCT",
      meta_field_moderator_continuous = "mean_age=47", meta_field_g = 0.25, meta_field_se = 0.15
    )
    session$setInputs(meta_save_effect = 3)
    session$setInputs(meta_add_effect = 4)
    session$setInputs(
      meta_input_type = "g_se", meta_field_study_id = "UI-1D", meta_field_study_name = "Jung et al.",
      meta_field_publication_year = 2021, meta_field_outcome = "Depression", meta_field_included = TRUE,
      meta_field_predictor = "Treatment B",
      meta_field_direction = "positive", meta_field_moderator_categorical = "region=Europe; design=RCT",
      meta_field_moderator_continuous = "mean_age=51", meta_field_g = 0.60, meta_field_se = 0.18
    )
    session$setInputs(meta_save_effect = 4)
    session$setInputs(meta_moderator_selection = "continuous::mean_age")
    session$setInputs(meta_group_mode = "predictor")
    session$setInputs(meta_run_analysis = 2)
    moderated <- session$userData$meta_state$analysis_result()
    stopifnot(inherits(moderated$moderator, "statedu_meta_moderator"))
    stopifnot(moderated$moderator$type == "continuous", moderated$moderator$k == 4L)
    stopifnot(inherits(moderated$grouped, "statedu_meta_grouped"), length(moderated$grouped$fits) == 2L)
    stopifnot(!is.null(moderated$trimfill), moderated$trimfill$k_observed == 4L)

    session$setInputs(meta_target_family = "or")
    session$setInputs(meta_add_effect = 5)
    session$setInputs(
      meta_input_type = "2x2",
      meta_field_study_id = "UI-2",
      meta_field_study_name = "Park et al.",
      meta_field_publication_year = 2022,
      meta_field_outcome = "Response",
      meta_field_predictor = "Treatment",
      meta_field_moderator_categorical = "region=Asia",
      meta_field_moderator_continuous = "mean_age=50",
      meta_field_included = TRUE,
      meta_field_direction = "positive",
      meta_field_cell_a = 10,
      meta_field_cell_b = 20,
      meta_field_cell_c = 5,
      meta_field_cell_d = 25
    )
    session$setInputs(meta_save_effect = 5)
    current_effects <- session$userData$meta_state$effects()
    stopifnot(nrow(current_effects) == 5L)
    stopifnot(current_effects$family[[5]] == "or")
    stopifnot(current_effects$display_effect[[5]] > 1)
  }
)

shiny::testServer(
  meta_test_server,
  {
    template_asset <- meta_excel_template_asset("g")
    session$setInputs(meta_target_family = "g")
    session$setInputs(meta_import_file = list(
      name = basename(template_asset),
      size = file.info(template_asset)$size,
      type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      datapath = template_asset
    ))
    session$flushReact()
    imported_effects <- session$userData$meta_state$effects()
    stopifnot(nrow(imported_effects) == 6L, all(imported_effects$status == "valid"))
  }
)

message("Meta-analysis input UI validation passed.")
