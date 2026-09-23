# Meta-analysis effect-size input UI.

meta_ui_text <- function(key, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  labels <- list(
    title = c(en = "Meta-analysis", ko = "메타분석"),
    subtitle = c(en = "Enter study-level results in their reported form and standardize them to a common effect size.", ko = "연구별 보고값을 그대로 입력하고 공통 효과크기로 표준화합니다."),
    target = c(en = "1. Effect-size input", ko = "1. 효과크기 입력"),
    target_help = c(en = "All included rows in one analysis must share this target scale.", ko = "한 분석에 포함되는 모든 행은 동일한 목표 척도를 사용해야 합니다."),
    direction = c(en = "Positive effect direction", ko = "양의 효과 방향"),
    direction_help = c(en = "Reverse individual rows when the published coding points in the opposite direction.", ko = "논문의 코딩 방향이 반대인 연구는 개별 행에서 방향을 반전합니다."),
    entry = c(en = "Study-level effects", ko = "연구별 효과크기"),
    add = c(en = "Add effect", ko = "효과크기 추가"),
    edit = c(en = "Edit selected", ko = "선택 행 수정"),
    remove = c(en = "Remove selected", ko = "선택 행 삭제"),
    import = c(en = "Import Excel / CSV template", ko = "Excel / CSV 템플릿 가져오기"),
    template = c(en = "Download Excel template", ko = "Excel 템플릿 다운로드"),
    save_input = c(en = "Save entered data CSV", ko = "입력자료 CSV 저장"),
    review = c(en = "2. Core analysis settings", ko = "2. 기본 분석 설정"),
    additional_analysis = c(en = "3. Additional analyses", ko = "3. 추가 분석"),
    dependency_analysis = c(en = "Dependent effects", ko = "효과 의존성"),
    model = c(en = "Meta-analytic model", ko = "메타분석 모형"),
    random_model = c(en = "Random effects", ko = "랜덤효과"),
    fixed_model = c(en = "Fixed effect", ko = "고정효과"),
    tau_method = c(en = "Between-study variance estimator", ko = "연구 간 분산(τ²) 추정법"),
    confidence = c(en = "Confidence level", ko = "신뢰수준"),
    prediction = c(en = "Calculate prediction interval", ko = "예측구간 계산"),
    publication_bias = c(en = "Publication-bias diagnostics", ko = "출판편향 진단"),
    funnel = c(en = "Funnel plot", ko = "퍼널 플롯"),
    egger = c(en = "Egger regression test", ko = "Egger 회귀 비대칭 검정"),
    trimfill = c(en = "Trim-and-fill sensitivity analysis", ko = "Trim-and-fill 민감도 분석"),
    run = c(en = "Run meta-analysis", ko = "메타분석 실행"),
    validate = c(en = "Validate effects", ko = "효과크기 검증"),
    validate_help = c(
      en = "Recalculate every entered effect and sampling variance, then flag missing, invalid, or assumption-dependent rows.",
      ko = "입력한 모든 효과크기와 표집분산을 다시 계산하고 결측·범위 오류·가정 확인이 필요한 행을 표시합니다."
    ),
    reset = c(en = "Reset", ko = "초기화"),
    no_rows = c(en = "Add an effect or import an Excel / CSV template.", ko = "효과크기를 추가하거나 Excel / CSV 템플릿을 가져오십시오."),
    study_id = c(en = "Study ID", ko = "연구 ID"),
    study_name = c(en = "Study name / citation", ko = "연구명 / 인용명"),
    publication_year = c(en = "Publication year", ko = "출판연도"),
    outcome = c(en = "Dependent variable / outcome", ko = "종속변수 / 결과변수"),
    predictor = c(en = "Independent variable / exposure", ko = "독립변수 / 노출변수"),
    group_mode = c(en = "Analysis grouping", ko = "분석 범위"),
    group_overall = c(en = "Overall", ko = "전체 통합"),
    group_outcome = c(en = "By dependent variable", ko = "종속변수별"),
    group_predictor = c(en = "By independent variable", ko = "독립변수별"),
    group_both = c(en = "By dependent × independent variable", ko = "종속변수 × 독립변수별"),
    dependency_method = c(en = "Multiple effects from one study", ko = "한 논문의 다중 효과 처리"),
    dependency_auto = c(en = "Auto: three-level + RVE sensitivity", ko = "자동: 3수준 + RVE 민감도 비교"),
    dependency_independent = c(en = "Treat as independent (not recommended)", ko = "독립 효과로 처리(권장하지 않음)"),
    dependency_three = c(en = "Three-level multilevel model", ko = "3수준 다층모형"),
    dependency_rve = c(en = "Robust variance estimation (RVE, CR2)", ko = "강건분산추정(RVE, CR2)"),
    dependency_both = c(en = "Compare three-level and RVE", ko = "3수준과 RVE 비교"),
    advanced_settings = c(en = "Advanced settings", ko = "고급 설정"),
    rve_compare = c(en = "Compare CR1, CR2, and CR3", ko = "CR1·CR2·CR3 결과 비교"),
    rve_compare_help = c(en = "CR2 remains the primary result; CR1 and CR3 are added as sensitivity analyses.", ko = "CR2를 주 결과로 유지하고 CR1과 CR3을 민감도 분석으로 함께 제시합니다."),
    moderator_categorical = c(en = "Categorical moderators", ko = "범주형 조절변수"),
    moderator_continuous = c(en = "Continuous moderators", ko = "연속형 조절변수"),
    moderator_help = c(
      en = "For one categorical moderator, a level alone (for example, M1) is allowed. Enter multiple moderators as name=value pairs separated by semicolons. Example: region=Asia; design=RCT / mean_age=42.5; female_percent=60",
      ko = "범주형 조절변수가 하나이면 M1처럼 값만 입력할 수 있습니다. 여러 조절변수는 이름=값을 세미콜론(;)으로 구분합니다. 예: 지역=아시아; 설계=RCT / 평균연령=42.5; 여성비율=60"
    ),
    moderator_analysis = c(en = "Moderator analysis", ko = "조절효과 분석"),
    no_moderator = c(en = "Do not run moderator analysis", ko = "조절효과 분석 안 함"),
    moderator_unavailable = c(en = "Enter moderator values in at least one study to enable this option.", ko = "하나 이상의 연구에 조절변수를 입력하면 선택할 수 있습니다."),
    format = c(en = "Reported-result format", ko = "논문 보고 형식"),
    included = c(en = "Include this effect", ko = "이 효과크기 포함"),
    positive = c(en = "POSITIVE — as entered", ko = "POSITIVE — 입력 방향 유지"),
    reverse = c(en = "NEGATIVE — reverse sign / reciprocal OR", ko = "NEGATIVE — 부호 반전 / OR 역수"),
    save = c(en = "Save effect", ko = "효과크기 저장"),
    cancel = c(en = "Cancel", ko = "취소"),
    add_title = c(en = "Add study effect", ko = "연구 효과크기 추가"),
    edit_title = c(en = "Edit study effect", ko = "연구 효과크기 수정"),
    select_row = c(en = "Select one row first.", ko = "먼저 행 하나를 선택하십시오."),
    imported = c(en = "effect rows were imported.", ko = "개 효과크기 행을 가져왔습니다."),
    invalid_file = c(en = "The Excel / CSV file could not be imported.", ko = "Excel / CSV 파일을 가져올 수 없습니다."),
    reset_done = c(en = "All entered meta-analysis effects were cleared.", ko = "입력한 메타분석 효과크기를 모두 지웠습니다."),
    validation_done = c(en = "Input validation is complete.", ko = "입력 검증을 완료했습니다."),
    supported = c(en = "Supported formats", ko = "지원 입력 형식"),
    analysis_scale = c(en = "Analysis-scale SE", ko = "분석척도 SE")
  )
  values <- labels[[key]] %||% c(en = key, ko = key)
  statedu_localized_text(language, unname(values[["en"]]), unname(values[["ko"]]))
}

meta_table_note <- function(table, language = "en") {
  headers <- attr(table, "meta_note_headers", exact = TRUE) %||% names(table)
  definition <- function(pattern, english, korean) {
    if (any(grepl(pattern, headers, perl = TRUE, ignore.case = TRUE))) statedu_t(paste0("meta.note.", sub(" =.*$", "", english)), language)
  }
  result_sci_note_text(abbreviations = c(
    definition("^SE$", "SE = standard error", "SE = 표준오차"),
    definition("CR2 SE", "CR2 SE = bias-reduced cluster-robust standard error", "CR2 SE = 편향 보정 군집 강건 표준오차"),
    definition("\\bCI\\b|신뢰구간", "CI = confidence interval", "CI = 신뢰구간"),
    definition("\\bPI\\b|예측구간", "PI = prediction interval", "PI = 예측구간"),
    definition("^Q$", "Q = Cochran's heterogeneity statistic", "Q = Cochran의 이질성 검정 통계량"),
    definition("\\bdf\\b|자유도", "df = degrees of freedom", "df = 자유도"),
    definition("Tau squared|τ²", "τ² = between-study variance", "τ² = 연구 간 분산"),
    definition("I squared|I²", "I² = proportion of variability attributed to heterogeneity", "I² = 이질성에 기인하는 변동의 비율"),
    definition("^OR(?: |$)", "OR = odds ratio", "OR = 오즈비"),
    definition("^RR(?: |$)", "RR = risk ratio", "RR = 위험비")
  ))
}

meta_result_table_tag <- function(table, table_role = "main", table_language = NULL) {
  table_role <- result_table_role(table_role, table)
  table_language <- result_table_language(table_role, table_language)
  note_line <- meta_table_note(table, table_language)
  if (identical(table_role, "appendix") && !table_language %in% c("en", "ko")) {
    table <- result_appendix_localize_table(table, table_language)
  }
  attr(table, "result_table_role") <- table_role
  attr(table, "result_table_language") <- table_language
  coefficient_html_table(
    table,
    compact = TRUE,
    compact_font_size = 11,
    compact_width = 56,
    compact_first_width = 100,
    compact_min_width = 460,
    note_line = note_line,
    table_role = table_role,
    table_language = table_language
  )
}

meta_result_section <- function(title, table, table_role = "main", table_language = NULL) {
  if (!is.data.frame(table) || nrow(table) == 0L) return(NULL)
  div(
    class = paste("result-section regression-result-panel meta-result-section", paste0("meta-result-section--", table_role)),
    h3(title),
    meta_result_table_tag(table, table_role = table_role, table_language = table_language)
  )
}

meta_result_warning_text <- function(note, language) {
  keys <- paste0("meta.warning.", c("independent", "small_cr2", "cr_comparison", "trim_interpretation", "funnel_min"))
  index <- match(note, vapply(keys, function(key) statedu_t(key, "en"), character(1)))
  if (!is.na(index)) return(statedu_t(keys[[index]], language))
  patterns <- c(
    multi_count = "^([0-9]+) study/studies contribute multiple effects[.] A dependency-aware model or study-level sensitivity analysis is required[.]$",
    omitted_count = "^([0-9]+) included study row[(]s[)] were omitted from moderator analysis because the selected moderator was missing[.]$")
  for (key in names(patterns)) {
    matched <- regmatches(note, regexec(patterns[[key]], note))[[1]]
    if (length(matched)) return(sprintf(statedu_t(paste0("meta.warning.", key), language), matched[[2]]))
  }
  prefixes <- c(dependency = "Dependency sensitivity analysis was unavailable: ",
    leave_one = "Leave-one-study-out analysis was unavailable: ",
    trimfill = "Trim-and-fill was unavailable: ", cr2 = "Moderator CR2 inference was unavailable: ")
  for (key in names(prefixes)) if (startsWith(note, prefixes[[key]])) {
    detail <- substring(note, nchar(prefixes[[key]]) + 1L)
    detail_keys <- paste0("meta.warning.", c("rho", "leave_min", "trim_min",
      "cr_complete", "cr_tau", "cr_clusters", "cr_parameters", "cr_rank",
      "cr_square", "cr_psd", "cr_type", "cr_lengths", "cr_confidence", "cr_singular", "cr_variance", "cr_df"))
    index <- match(detail, vapply(detail_keys, function(k) statedu_t(k, "en"), character(1)))
    if (!is.na(index)) detail <- statedu_t(detail_keys[[index]], language)
    return(paste0(statedu_t(paste0("meta.warning.", key), language), detail))
  }
  note
}

meta_analysis_results_ui <- function(result, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  appendix_language <- result_appendix_table_language(language)
  appendix_title <- function(en, ko_text) statedu_localized_text(language, en, ko_text)
  notes <- vapply(result$notes, meta_result_warning_text, character(1), language = language)
  div(
    class = "analysis-results meta-analysis-results",
    h2(statedu_t("meta.egger.title", language)),
    meta_result_section(
      if (isTRUE(result$dependency_summary$dependent %||% FALSE)) {
        "Reference pooled effect (independence assumption)"
      } else "Pooled effect",
      meta_model_summary_table(result, "en"),
      table_role = "main",
      table_language = "en"
    ),
    if (!is.null(result$grouped)) meta_result_section("Grouped pooled effects", meta_grouped_results_table(result$grouped, result$family, "en"), "main", "en"),
    if (length(result$dependency_models %||% list()) > 0L) meta_result_section("Dependent-effect analysis", meta_dependency_results_table(result, "en"), "main", "en"),
    meta_result_section("Heterogeneity", meta_heterogeneity_table(result, "en"), "main", "en"),
    if (!is.null(result$moderator)) tagList(
      meta_result_section("Moderator test", meta_moderator_test_table(result$moderator, "en"), "main", "en"),
      if (identical(result$moderator$type, "categorical")) meta_result_section("Subgroup pooled effects", meta_subgroup_results_table(result$moderator, result$family, "en"), "main", "en"),
      meta_result_section("Meta-regression coefficients", meta_moderator_coefficient_table(result$moderator, "en"), "main", "en"),
      if (length(result$moderator$robust_models %||% list()) > 1L) {
        meta_result_section("Cluster-robust meta-regression comparison (CR1, CR2, CR3)", meta_moderator_robust_comparison_table(result$moderator, "en"), "main", "en")
      } else if (!is.null(result$moderator$cr2)) {
        meta_result_section("Meta-regression coefficients (study-cluster CR2)", meta_moderator_cr2_table(result$moderator, "en"), "main", "en")
      },
      if (identical(result$moderator$type, "continuous")) div(
        class = "analysis-note meta-moderator-centering-note",
        sprintf(statedu_t("meta.warning.centered", language), meta_format_number(result$moderator$center))
      )
    ),
    if (!is.null(result$dependency_sensitivity)) meta_result_section(statedu_t("meta.sensitivity.dependency_title", language), meta_dependency_sensitivity_table(result$dependency_sensitivity, result$family, appendix_language), "appendix", appendix_language),
    if (!is.null(result$leave_one_study_out)) meta_result_section(statedu_t("meta.sensitivity.leave_title", language), meta_leave_one_study_out_table(result$leave_one_study_out, result$family, appendix_language), "appendix", appendix_language),
    if (isTRUE(result$show_egger)) meta_result_section(appendix_title("Egger asymmetry test", "Egger 비대칭 검정"), meta_egger_results_table(result$egger, appendix_language), "appendix", appendix_language),
    if (!is.null(result$trimfill)) meta_result_section(statedu_t("meta.sensitivity.trim_title", language), meta_trimfill_results_table(result$trimfill, appendix_language), "appendix", appendix_language),
    meta_result_section(statedu_t("meta.sensitivity.study_title", language), meta_study_results_table(result, appendix_language), "appendix", appendix_language),
    div(
      class = "result-section regression-result-panel meta-forest-section",
      h3("Forest plot"),
      plotOutput("meta_forest_plot", height = paste0(max(430, 190 + result$k * 34), "px"))
    ),
    if (isTRUE(result$show_funnel)) div(
      class = "result-section regression-result-panel meta-funnel-section",
      h3("Funnel plot"),
      plotOutput("meta_funnel_plot", height = "520px")
    ),
    if (length(notes) > 0L) div(
      class = "analysis-warning-panel meta-analysis-notes",
      h4(statedu_t("meta.warning.title", language)),
      tags$ul(lapply(notes, tags$li))
    )
  )
}

meta_family_choices <- function(language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  stats::setNames(
    c("g", "r", "or"),
    statedu_localized_text(language,
      c("Continuous group comparison: Hedges' g", "Variable association: correlation r", "Binary outcome: Odds Ratio"),
      c("연속형 집단 비교: Hedges' g", "변수 간 연관성: 상관계수 r", "이분형 결과: Odds Ratio"))
  )
}

meta_input_type_choices <- function(family, language = statedu_initial_language()) {
  types <- meta_input_types(family)
  if (!identical(normalize_app_language(language), "ko")) {
    return(stats::setNames(names(types), statedu_localized_text(language, unname(types))))
  }
  labels <- switch(
    family,
    g = c(
      means = "두 집단 평균·SD·표본 수",
      g_se = "Hedges' g와 SE",
      g_ci = "Hedges' g와 95% CI",
      d = "Cohen's d와 집단별 표본 수",
      t = "독립표본 t와 집단별 표본 수",
      r_pb = "점이연상관 r과 집단별 표본 수"
    ),
    r = c(
      r = "Pearson r과 표본 수",
      z_se = "Fisher's z와 SE",
      t = "상관검정 t와 표본 수",
      partial_r = "부분상관 r, 표본 수, 통제변수 수"
    ),
    or = c(
      `2x2` = "2×2 셀 빈도",
      or_ci = "OR와 95% CI",
      logor_se = "log(OR)와 SE",
      logistic_b = "로지스틱 회귀계수 B와 SE"
    ),
    types
  )
  stats::setNames(names(labels), unname(labels))
}

meta_record_field <- function(record, name, default = NA_real_) {
  if (is.null(record) || !name %in% names(record) || length(record[[name]]) == 0L) return(default)
  value <- record[[name]][[1]]
  if (is.null(value) || is.na(value)) default else value
}

meta_numeric_input <- function(id, label, record = NULL, field = id, min = NA, max = NA, step = "any") {
  args <- list(inputId = paste0("meta_field_", id), label = label, value = meta_record_field(record, field))
  if (is.finite(min)) args$min <- min
  if (is.finite(max)) args$max <- max
  args$step <- step
  do.call(numericInput, args)
}

meta_effect_fields_ui <- function(family, input_type, record = NULL, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  n1_label <- statedu_localized_text(language, "Group 1 (treatment) sample size (n1)", "집단 1(실험집단) 표본 수 (n1)")
  n0_label <- statedu_localized_text(language, "Group 0 (control) sample size (n0)", "집단 0(대조집단) 표본 수 (n0)")
  se_label <- statedu_localized_text(language, "Standard error (SE)", "표준오차 (SE)")
  ci_low <- statedu_localized_text(language, "95% CI lower", "95% CI 하한")
  ci_high <- statedu_localized_text(language, "95% CI upper", "95% CI 상한")
  row <- function(...) fluidRow(lapply(list(...), function(control) column(4, control)))

  if (identical(family, "g") && identical(input_type, "means")) return(tagList(
    row(
      meta_numeric_input("m1", statedu_localized_text(language, "Group 1 (treatment) mean (M1)", "집단 1(실험집단) 평균 (M1)"), record),
      meta_numeric_input("sd1", statedu_localized_text(language, "Group 1 (treatment) SD (SD1)", "집단 1(실험집단) 표준편차 (SD1)"), record, min = 0),
      meta_numeric_input("n1", n1_label, record, min = 2, step = 1)
    ),
    row(
      meta_numeric_input("m0", statedu_localized_text(language, "Group 0 (control) mean (M0)", "집단 0(대조집단) 평균 (M0)"), record),
      meta_numeric_input("sd0", statedu_localized_text(language, "Group 0 (control) SD (SD0)", "집단 0(대조집단) 표준편차 (SD0)"), record, min = 0),
      meta_numeric_input("n0", n0_label, record, min = 2, step = 1)
    )
  ))
  if (identical(family, "g") && identical(input_type, "g_se")) return(row(
    meta_numeric_input("g", "Hedges' g", record), meta_numeric_input("se", se_label, record, min = 0)
  ))
  if (identical(family, "g") && identical(input_type, "g_ci")) return(row(
    meta_numeric_input("g", "Hedges' g", record), meta_numeric_input("ci_lower", ci_low, record), meta_numeric_input("ci_upper", ci_high, record)
  ))
  if (identical(family, "g") && identical(input_type, "d")) return(row(
    meta_numeric_input("d_value", "Cohen's d", record), meta_numeric_input("n1", n1_label, record, min = 2, step = 1), meta_numeric_input("n0", n0_label, record, min = 2, step = 1)
  ))
  if (identical(family, "g") && identical(input_type, "t")) return(row(
    meta_numeric_input("t_value", statedu_localized_text(language, "Independent-samples t", "독립표본 t"), record), meta_numeric_input("n1", n1_label, record, min = 2, step = 1), meta_numeric_input("n0", n0_label, record, min = 2, step = 1)
  ))
  if (identical(family, "g") && identical(input_type, "r_pb")) return(row(
    meta_numeric_input("r_pb", statedu_localized_text(language, "Point-biserial r", "점이연상관 r"), record, min = -0.999999, max = 0.999999), meta_numeric_input("n1", n1_label, record, min = 2, step = 1), meta_numeric_input("n0", n0_label, record, min = 2, step = 1)
  ))

  if (identical(family, "r") && identical(input_type, "r")) return(row(
    meta_numeric_input("r", "Pearson r", record, min = -0.999999, max = 0.999999), meta_numeric_input("n", statedu_localized_text(language, "Total sample size (n)", "전체 표본 수 (n)"), record, min = 4, step = 1)
  ))
  if (identical(family, "r") && identical(input_type, "z_se")) return(row(
    meta_numeric_input("fisher_z", "Fisher's z", record), meta_numeric_input("se", se_label, record, min = 0)
  ))
  if (identical(family, "r") && identical(input_type, "t")) return(row(
    meta_numeric_input("t_value", statedu_localized_text(language, "Correlation t", "상관검정 t"), record), meta_numeric_input("n", statedu_localized_text(language, "Total sample size (n)", "전체 표본 수 (n)"), record, min = 4, step = 1)
  ))
  if (identical(family, "r") && identical(input_type, "partial_r")) return(row(
    meta_numeric_input("r", statedu_localized_text(language, "Partial r", "부분상관 r"), record, min = -0.999999, max = 0.999999),
    meta_numeric_input("n", statedu_localized_text(language, "Total sample size (n)", "전체 표본 수 (n)"), record, min = 4, step = 1),
    meta_numeric_input("k_controls", statedu_localized_text(language, "Number of controls", "통제변수 수"), record, min = 0, step = 1)
  ))

  if (identical(family, "or") && identical(input_type, "2x2")) return(tagList(
    row(
      meta_numeric_input("cell_a", statedu_localized_text(language, "Group 1 (treatment) event (a)", "집단 1(실험집단) 사건 (a)"), record, min = 0, step = 1),
      meta_numeric_input("cell_b", statedu_localized_text(language, "Group 1 (treatment) non-event (b)", "집단 1(실험집단) 비사건 (b)"), record, min = 0, step = 1),
      meta_numeric_input("cell_c", statedu_localized_text(language, "Group 0 (control) event (c)", "집단 0(대조집단) 사건 (c)"), record, min = 0, step = 1)
    ),
    row(meta_numeric_input("cell_d", statedu_localized_text(language, "Group 0 (control) non-event (d)", "집단 0(대조집단) 비사건 (d)"), record, min = 0, step = 1))
  ))
  if (identical(family, "or") && identical(input_type, "or_ci")) return(row(
    meta_numeric_input("or_value", statedu_t("meta.dialog.odds_ratio", language), record, min = 0), meta_numeric_input("ci_lower", ci_low, record, min = 0), meta_numeric_input("ci_upper", ci_high, record, min = 0)
  ))
  if (identical(family, "or") && identical(input_type, "logor_se")) return(row(
    meta_numeric_input("log_or", "log(OR)", record), meta_numeric_input("se", se_label, record, min = 0)
  ))
  if (identical(family, "or") && identical(input_type, "logistic_b")) return(row(
    meta_numeric_input("logit_b", statedu_localized_text(language, "Logistic coefficient B", "로지스틱 회귀계수 B"), record), meta_numeric_input("se", se_label, record, min = 0)
  ))
  div(class = "empty-message", statedu_localized_text(language, "Choose a reported-result format.", "보고 형식을 선택하십시오."))
}

meta_effect_modal <- function(family, record = NULL, language = statedu_initial_language()) {
  editing <- !is.null(record) && nrow(record) > 0L
  current_type <- if (editing) as.character(record$input_type[[1]]) else names(meta_input_types(family))[[1]]
  current_direction <- if (editing) as.character(record$direction[[1]]) else "positive"
  if (identical(current_direction, "reverse")) current_direction <- "negative"
  modalDialog(
    title = span(id = "meta_effect_modal_title", class = "shiny-text-output", meta_ui_text(if (editing) "edit_title" else "add_title", language)),
    size = "l",
    easyClose = FALSE,
    fluidRow(
      column(4, textInput("meta_field_study_id", meta_ui_text("study_id", language), value = if (editing) record$study_id[[1]] else "")),
      column(4, textInput("meta_field_study_name", meta_ui_text("study_name", language), value = if (editing) record$study_name[[1]] else "")),
      column(4, numericInput("meta_field_publication_year", meta_ui_text("publication_year", language), value = if (editing && is.finite(record$publication_year[[1]])) record$publication_year[[1]] else NA, min = 1800, max = as.integer(format(Sys.Date(), "%Y")) + 1L, step = 1))
    ),
    fluidRow(
      column(4, textInput("meta_field_outcome", meta_ui_text("outcome", language), value = if (editing) record$outcome[[1]] else "")),
      column(4, textInput("meta_field_predictor", meta_ui_text("predictor", language), value = if (editing) record$predictor[[1]] else "")),
      column(4, checkboxInput("meta_field_included", meta_ui_text("included", language), value = if (editing) isTRUE(record$included[[1]]) else TRUE))
    ),
    fluidRow(
      column(6, textInput("meta_field_moderator_categorical", meta_ui_text("moderator_categorical", language), value = if (editing) record$moderator_categorical[[1]] else "", placeholder = statedu_t("meta.dialog.categorical_example", language))),
      column(6, textInput("meta_field_moderator_continuous", meta_ui_text("moderator_continuous", language), value = if (editing) record$moderator_continuous[[1]] else "", placeholder = statedu_t("meta.dialog.continuous_example", language)))
    ),
    div(id = "meta_effect_modal_help", class = "meta-moderator-help shiny-text-output", meta_ui_text("moderator_help", language)),
    fluidRow(
      column(6, selectInput("meta_input_type", meta_ui_text("format", language), choices = meta_input_type_choices(family, language), selected = current_type)),
      column(6, selectInput("meta_field_direction", meta_ui_text("direction", language), choices = stats::setNames(c("positive", "negative"), c(meta_ui_text("positive", language), meta_ui_text("reverse", language))), selected = current_direction))
    ),
    tags$hr(),
    uiOutput("meta_effect_fields"),
    footer = tagList(
      modalButton(span(id = "meta_effect_modal_cancel", class = "shiny-text-output", meta_ui_text("cancel", language))),
      actionButton("meta_save_effect", meta_ui_text("save", language), class = "btn-primary")
    )
  )
}

meta_analysis_tab_panel <- function(language = statedu_initial_language()) {
  tabPanel(
    meta_ui_text("title", language),
    value = "analysis_meta",
    div(
      class = "page-shell",
      div(class = "app-heading", h1(meta_ui_text("title", language)), div(meta_ui_text("subtitle", language), class = "app-subtitle")),
      div(
        class = "workspace-panel frequencies-workspace-panel analysis-three-block-workspace meta-analysis-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        div(
          class = "sample-size-grid meta-analysis-setup-grid",
          div(
            class = "sample-size-block meta-analysis-input-block",
            h3(meta_ui_text("target", language)),
            selectInput("meta_target_family", NULL, choices = meta_family_choices(language), selected = "g"),
            div(class = "sample-size-method-note", meta_ui_text("target_help", language)),
            div(
              class = "meta-input-actions",
              div(class = "analysis-option-title", meta_ui_text("entry", language)),
              div(
                class = "analysis-action-row",
                actionButton("meta_add_effect", meta_ui_text("add", language), class = "btn-primary"),
                actionButton("meta_edit_effect", meta_ui_text("edit", language)),
                actionButton("meta_remove_effect", meta_ui_text("remove", language))
              ),
              fileInput("meta_import_file", meta_ui_text("import", language), accept = c(".xlsx", ".xls", ".csv", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "application/vnd.ms-excel", "text/csv"), width = "100%",
                buttonLabel = statedu_localized_text(language, "Choose file", "파일 선택"),
                placeholder = statedu_localized_text(language, "No file selected", "선택된 파일 없음")),
              div(
                class = "meta-download-actions",
                downloadButton("meta_download_template", meta_ui_text("template", language)),
                downloadButton("meta_download_effects", meta_ui_text("save_input", language), class = "btn-primary")
              )
            ),
            tags$hr(),
            strong(meta_ui_text("supported", language)),
            uiOutput("meta_supported_formats")
          ),
          div(
            class = "sample-size-block meta-analysis-review-block",
            h3(meta_ui_text("review", language)),
            selectInput(
              "meta_model",
              meta_ui_text("model", language),
              choices = stats::setNames(c("random", "fixed"), c(meta_ui_text("random_model", language), meta_ui_text("fixed_model", language))),
              selected = "random"
            ),
            conditionalPanel(
              condition = "input.meta_model === 'random'",
              selectInput(
                "meta_tau_method",
                meta_ui_text("tau_method", language),
                choices = stats::setNames(c("REML", "PM", "DL"), c("REML", "Paule–Mandel", "DerSimonian–Laird")),
                selected = "REML"
              ),
              checkboxInput("meta_prediction_interval", meta_ui_text("prediction", language), value = TRUE)
            ),
            selectInput(
              "meta_conf_level",
              meta_ui_text("confidence", language),
              choices = stats::setNames(c(0.90, 0.95, 0.99), c("90%", "95%", "99%")),
              selected = 0.95
            ),
            selectInput(
              "meta_group_mode",
              meta_ui_text("group_mode", language),
              choices = stats::setNames(
                c("overall", "outcome", "predictor", "outcome_predictor"),
                c(meta_ui_text("group_overall", language), meta_ui_text("group_outcome", language), meta_ui_text("group_predictor", language), meta_ui_text("group_both", language))
              ),
              selected = "overall"
            )
          ),
          div(
            class = "sample-size-block meta-analysis-additional-block",
            h3(meta_ui_text("additional_analysis", language)),
            div(
              class = "meta-additional-card meta-structure-options",
              div(class = "analysis-option-title", meta_ui_text("dependency_analysis", language)),
              selectInput(
                "meta_dependency_method",
                meta_ui_text("dependency_method", language),
                choices = stats::setNames(
                  c("auto", "independent", "three_level", "rve", "both"),
                  c(meta_ui_text("dependency_auto", language), meta_ui_text("dependency_independent", language), meta_ui_text("dependency_three", language), meta_ui_text("dependency_rve", language), meta_ui_text("dependency_both", language))
                ),
                selected = "auto"
              ),
              tags$details(
                class = "meta-advanced-settings",
                tags$summary(meta_ui_text("advanced_settings", language)),
                checkboxInput("meta_rve_compare", meta_ui_text("rve_compare", language), value = FALSE),
                div(class = "sample-size-method-note", meta_ui_text("rve_compare_help", language))
              )
            ),
            div(
              class = "meta-additional-card meta-publication-bias-options",
              div(class = "analysis-option-title", meta_ui_text("publication_bias", language)),
              checkboxInput("meta_funnel_plot_enabled", meta_ui_text("funnel", language), value = TRUE),
              checkboxInput("meta_egger_test_enabled", meta_ui_text("egger", language), value = TRUE),
              checkboxInput("meta_trimfill_enabled", meta_ui_text("trimfill", language), value = TRUE)
            ),
            div(class = "meta-additional-card meta-moderator-card", uiOutput("meta_moderator_selector"))
          )
        ),
        analysis_three_block_action_row(
          class = "meta-analysis-action-row",
          run_button = actionButton("meta_run_analysis", meta_ui_text("run", language), class = "btn btn-primary"),
          reset_control = actionButton("meta_reset_effects", meta_ui_text("reset", language), class = "btn btn-default"),
          save_control = NULL,
          extra_controls = actionButton(
            "meta_validate_effects",
            meta_ui_text("validate", language),
            class = "btn btn-default",
            title = meta_ui_text("validate_help", language)
          )
        ),
        div(
          class = "meta-validation-strip",
          uiOutput("meta_validation_summary"),
          div(class = "sample-size-method-note", meta_ui_text("direction_help", language))
        ),
        tags$hr(),
        DTOutput("meta_effects_table"),
        uiOutput("meta_validation_details"),
        uiOutput("meta_analysis_results")
      )
    )
  )
}
