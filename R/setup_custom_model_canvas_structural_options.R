# Structural equation canvas analysis option controls.

# Restore only analysis controls, never canvas nodes or result content.
structural_canvas_option_ids <- function(ui) {
  ids <- character(0)
  walk <- function(tag) {
    if (inherits(tag, "shiny.tag")) {
      if (tag$name %in% c("input", "select", "textarea")) {
        id <- tag$attribs$name %||% tag$attribs$id %||% ""
        if (nzchar(id)) ids <<- c(ids, id)
      }
      lapply(tag$children, walk)
    } else if (is.list(tag)) lapply(tag, walk)
    invisible(NULL)
  }
  walk(ui)
  unique(ids)
}

structural_canvas_restore_options <- function(ui, values) {
  walk <- function(tag, selected = NULL) {
    if (!inherits(tag, "shiny.tag")) {
      # Shiny stores select options as an HTML string, not individual tags.
      if (inherits(tag, "html") && !is.null(selected) && grepl("<option", tag, fixed = TRUE)) {
        doc <- xml2::read_html(paste0("<select>", tag, "</select>"), encoding = "UTF-8")
        options <- xml2::xml_find_all(doc, "//option")
        for (option in options) {
          attrs <- xml2::xml_attrs(option)
          attrs <- attrs[names(attrs) != "selected"]
          if (xml2::xml_attr(option, "value") %in% selected) attrs <- c(attrs, selected = "selected")
          xml2::xml_attrs(option) <- attrs
        }
        nodes <- xml2::xml_children(xml2::xml_find_first(doc, "//select"))
        return(htmltools::HTML(paste(vapply(nodes, as.character, character(1)), collapse = "\n")))
      }
      if (is.list(tag)) tag[] <- lapply(tag, walk, selected = selected)
      return(tag)
    }
    id <- tag$attribs$name %||% tag$attribs$id %||% ""
    value <- values[[id]]
    if (tag$name == "select" && !is.null(value)) selected <- as.character(value)
    if (tag$name == "option" && !is.null(selected)) {
      tag$attribs$selected <- if (as.character(tag$attribs$value %||% "") %in% selected) "selected" else NULL
    }
    if (tag$name == "input" && !is.null(value)) {
      type <- tag$attribs$type %||% "text"
      if (type %in% c("radio", "checkbox")) {
        checked <- if (type == "checkbox" && is.logical(value)) isTRUE(value) else as.character(tag$attribs$value %||% "") %in% as.character(value)
        tag$attribs$checked <- if (checked) "checked" else NULL
      } else tag$attribs$value <- as.character(value)
    }
    if (tag$name == "textarea" && !is.null(value)) tag$children <- list(as.character(value))
    tag$children <- lapply(tag$children, walk, selected = selected)
    tag
  }
  walk(ui)
}

structural_canvas_grouping_variable_choices <- function(data, variable_table = NULL) {
  data_names <- if (is.character(data)) as.character(data) else names(data %||% data.frame())
  if (!length(data_names) || !is.data.frame(variable_table) ||
      !all(c("name", "measurement") %in% names(variable_table))) {
    return(character(0))
  }
  variable_names <- as.character(variable_table$name)
  measurements <- tolower(trimws(as.character(variable_table$measurement)))
  measurements[measurements %in% c("categorical", "nominal")] <- "category"
  eligible <- variable_names[measurements %in% c("binary", "category")]
  data_names[data_names %in% eligible]
}

structural_canvas_grouping_variable_select_choices <- function(data, variable_table = NULL, labels = character(0)) {
  values <- structural_canvas_grouping_variable_choices(data, variable_table)
  if (!length(values)) return(character(0))
  display <- values
  if (is.data.frame(variable_table) && all(c("name", "var_label") %in% names(variable_table))) {
    index <- match(values, as.character(variable_table$name))
    table_labels <- as.character(variable_table$var_label[index])
    use <- !is.na(table_labels) & nzchar(trimws(table_labels))
    display[use] <- trimws(table_labels[use])
  }
  if (length(labels) && !is.null(names(labels))) {
    override <- as.character(labels[values])
    use <- !is.na(override) & nzchar(trimws(override))
    display[use] <- trimws(override[use])
  }
  display <- ifelse(display == values, values, paste0(display, " [", values, "]"))
  stats::setNames(values, display)
}

structural_canvas_invariance_path_scope <- function(value = "all") {
  value <- tolower(trimws(as.character(value %||% "all")))
  if (length(value) != 1L || is.na(value) || !value %in% c("all", "selected")) "all" else value
}

structural_canvas_invariance_selected_path_ids <- function(value = character(0)) {
  value <- trimws(as.character(value %||% character(0)))
  unique(value[!is.na(value) & nzchar(value)])
}

structural_canvas_multigroup_path_choices <- function(snapshot, display_name = identity) {
  snapshot <- snapshot %||% list()
  nodes <- snapshot$nodes %||% list()
  node_ids <- vapply(nodes, function(node) as.character(node$id %||% ""), character(1))
  node_index <- stats::setNames(nodes, node_ids)
  node_for <- function(id) {
    id <- as.character(id %||% "")
    if (!nzchar(id) || !id %in% names(node_index)) NULL else node_index[[id]]
  }
  structural_edges <- Filter(function(edge) {
    if (identical(as.character(edge$kind %||% ""), "covariance")) return(FALSE)
    if (identical(as.character(edge$pathType %||% "regression"), "higherOrder")) return(FALSE)
    from <- node_for(edge$from)
    to <- node_for(edge$to)
    !is.null(from) && !is.null(to) &&
      identical(as.character(from$role %||% ""), "latent") &&
      identical(as.character(to$role %||% ""), "latent")
  }, snapshot$edges %||% list())
  if (!length(structural_edges)) return(character(0))
  edge_ids <- vapply(structural_edges, function(edge) as.character(edge$id %||% ""), character(1))
  valid <- !is.na(edge_ids) & nzchar(edge_ids) & !duplicated(edge_ids)
  structural_edges <- structural_edges[valid]
  edge_ids <- edge_ids[valid]
  if (!length(edge_ids)) return(character(0))
  labels <- vapply(structural_edges, function(edge) {
    predictor <- structural_canvas_name(node_for(edge$from))
    outcome <- structural_canvas_name(node_for(edge$to))
    paste(display_name(predictor), display_name(outcome), sep = " \u2192 ")
  }, character(1))
  stats::setNames(edge_ids, labels)
}

structural_canvas_result_coefficient_choices <- function(language = statedu_initial_language(), analysis_type = "cbsem") {
  if (identical(analysis_type, "plssem")) {
    return(stats::setNames(c("pls_value", "pls_p"), c("β", "β(p)")))
  }
  stats::setNames(c("b_p", "b_t", "beta_t", "beta_p", "b_beta"), c("B(p)", "B(t)", "beta(t)", "beta(p)", "B(Beta)"))
}

structural_canvas_measurement_coefficient_choices <- function(language = statedu_initial_language()) {
  stats::setNames(c("measurement_value", "measurement_p"), c("loading / weight", "loading(p) / weight(p)"))
}

structural_canvas_bootstrap_replicate_values <- function(include_disabled = TRUE) {
  values <- c(1000L, 5000L, 10000L, 20000L, 50000L)
  if (isTRUE(include_disabled)) c(0L, values) else values
}

structural_analysis_options_panel <- function(analysis_type = "cbsem", language = statedu_initial_language(), grouping_choices = character(0)) {
  prefix <- structural_analysis_prefix(analysis_type)
  input_id <- function(suffix) paste0(prefix, suffix)
  bootstrap_choices <- function(values) {
    values <- as.integer(values)
    labels <- vapply(values, function(value) {
      if (identical(value, 0L)) return(statedu_localized_text(language, "Do not compute", "계산하지 않음"))
      formatted <- format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
      gsub("{count}", formatted, statedu_localized_text(language, "{count} resamples", "{count}회"), fixed = TRUE)
    }, character(1))
    stats::setNames(as.character(values), labels)
  }
  bootstrap_select <- function(suffix, label_ko, label_en, values, selected = "0") {
    values <- as.integer(values)
    selected <- as.character(selected)
    if (!selected %in% as.character(values)) selected <- as.character(values[[1L]])
    selectInput(input_id(suffix), statedu_localized_text(language, label_en, label_ko), choices = bootstrap_choices(values), selected = selected)
  }
  bootstrap_ci_choices <- function(include_bca = FALSE) {
    labels <- c(statedu_localized_text(language, "Bias-corrected (BC)", "편향 보정(BC)"),
                statedu_localized_text(language, "Percentile", "백분위수"))
    values <- c("bias_corrected", "percentile")
    if (include_bca) {
      labels <- c(labels, statedu_localized_text(language, "BCa (slower)", "BCa(느림)"))
      values <- c(values, "bca")
    }
    stats::setNames(values, labels)
  }
  bootstrap_details <- function(suffix, ...) {
    conditionalPanel(sprintf("input['%s'] != '0'", input_id(suffix)), ...)
  }

  estimation_tab <- tabPanel(
    statedu_localized_text(language, "Estimation", "추정"),
    selectInput(
      input_id("_estimator"), statedu_localized_text(language, "Estimator", "추정 방법"),
      choices = if (identical(analysis_type, "plssem")) {
        stats::setNames(
          c("AUTO", "PLS", "PLSC"),
          c(statedu_localized_text(language, "Rule-based recommendation (confirmation required)", "규칙 기반 추천(확인 필요)"), "PLS", "PLSc")
        )
      } else {
        c("ML" = "ML", "MLR" = "MLR", "WLSMV" = "WLSMV")
      },
      selected = if (identical(analysis_type, "plssem")) "AUTO" else "ML"
    ),
    if (identical(analysis_type, "plssem")) checkboxInput(
      input_id("_estimator_recommendation_confirmed"),
      statedu_localized_text(language, "I reviewed and accept the PLS/PLSc recommendation based on the construct specification.", "구성개념 명세에 따른 PLS/PLSc 추천을 검토하고 적용합니다."),
      value = TRUE
    ),
    selectInput(
      input_id("_objective"), statedu_localized_text(language, "Primary analysis objective", "주요 분석 목적"),
      choices = stats::setNames(
        c("confirmatory", "explanatory", "predictive", "scores"),
        c(statedu_localized_text(language, "Confirmatory theory testing", "이론 검증"), statedu_localized_text(language, "Structural explanation", "구조 설명"), statedu_localized_text(language, "Out-of-sample prediction", "표본외 예측"), statedu_localized_text(language, "Construct-score use", "구성개념 점수 활용"))
      ), selected = "confirmatory"
    ),
    selectInput(
      input_id("_analysis_plan_status"), statedu_localized_text(language, "Analysis-plan status", "분석계획 상태"),
      choices = stats::setNames(
        c("not_recorded", "preregistered", "protocol_defined", "exploratory"),
        c(statedu_localized_text(language, "Not recorded", "기록되지 않음"), statedu_localized_text(language, "Preregistered", "사전등록됨"), statedu_localized_text(language, "Defined in an a-priori protocol", "등록 전 프로토콜에 정의됨"), statedu_localized_text(language, "Exploratory analysis", "탐색적 분석"))
      ), selected = "not_recorded"
    ),
    textInput(input_id("_analysis_plan_reference"), statedu_localized_text(language, "Preregistration or protocol reference", "사전등록·프로토콜 참조"), value = "", placeholder = statedu_localized_text(language, "Registration URL/DOI, date, version, or protocol identifier", "등록 URL/DOI, 날짜, 버전 또는 프로토콜 식별자")),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_missing"), statedu_localized_text(language, "Missing data", "결측치 처리"), choices = stats::setNames(c("fiml", "listwise"), c("FIML", statedu_localized_text(language, "Listwise deletion", "목록 삭제")))),
    if (!identical(analysis_type, "plssem")) tags$p(class = "structural-option-note", statedu_localized_text(language, "When ordered indicators or the WLSMV estimator are used, lavaan uses pairwise missing-data handling instead of FIML.", "순서형 지표 또는 WLSMV 추정량을 사용하면 lavaan 제약에 따라 FIML 대신 pairwise 결측 처리가 적용됩니다.")),
    if (identical(analysis_type, "plssem")) tags$p(
      class = "structural-option-note",
      statedu_localized_text(language, "PLS/PLSc uses a fixed indicator-mean replacement policy. The main fit uses means from the full analysis sample; every bootstrap resample recomputes its own indicator means. Replaced rows/cells and indicator-level missingness are recorded in the results and Audit JSON.", "PLS/PLSc는 지표별 평균 대체를 고정적으로 사용합니다. 본 분석은 전체 분석표본의 지표 평균을 사용하고, bootstrap은 각 재표집 안에서 평균을 다시 계산해 대체합니다. 대체 사례·셀 수와 지표별 결측률은 결과와 Audit JSON에 기록됩니다.")
    ),
    uiOutput(input_id("_method_recommendation"))
  )

  bootstrap_tab <- tabPanel(
    statedu_localized_text(language, "Bootstrap", "부트스트랩"),
    tags$p(
      class = "structural-option-note",
      if (analysis_type %in% c("cbsem", "sem")) {
        statedu_localized_text(language, "The base model is fitted first. Path, indirect, and total-effect bootstrap defaults to 5,000 resamples; other resampling analyses run only when selected.", "기본 모형을 먼저 적합하며, 경로·간접·총효과 bootstrap은 기본 5,000회입니다. 그 밖의 재표집 분석은 선택한 경우에만 추가로 실행합니다.")
      } else if (identical(analysis_type, "plssem")) {
        statedu_localized_text(language, "The base model is fitted first, followed by 5,000 PLS/PLSc bootstrap resamples for paths, loadings, weights, indirect effects, and total effects. Choose 1,000, 5,000, 10,000, 20,000, or 50,000 resamples.", "기본 모형을 먼저 적합한 뒤 PLS/PLSc 경로·loading·weight·간접·총효과 bootstrap을 기본 5,000회 실행합니다. 재표집 횟수는 1,000 / 5,000 / 10,000 / 20,000 / 50,000회 중에서 선택합니다.")
      } else {
        statedu_localized_text(language, "The base model is fitted first, and only selected resampling analyses are added. All bootstrap procedures default to 'Do not compute'.", "기본 모형을 먼저 적합하고, 선택한 재표집 분석만 추가로 실행합니다. 모든 부트스트랩의 기본값은 '계산하지 않음'입니다.")
      }
    ),
    if (analysis_type %in% c("cbsem", "sem")) tagList(
      tags$h5(statedu_localized_text(language, "Path, indirect, and total effects", "경로·간접·총효과")),
      bootstrap_select("_effect_bootstrap", "경로·간접·총효과 bootstrap CI/p", "Path, indirect, and total-effect bootstrap CI/p", structural_canvas_bootstrap_replicate_values(), selected = "5000"),
      bootstrap_details(
        "_effect_bootstrap",
        numericInput(input_id("_effect_bootstrap_seed"), statedu_localized_text(language, "Path/indirect/total-effect seed", "경로·간접·총효과 seed"), value = default_seed(), min = 1L, step = 1L),
        selectInput(input_id("_effect_bootstrap_ci_method"), statedu_localized_text(language, "Path/indirect/total-effect CI method", "경로·간접·총효과 CI 방법"), choices = bootstrap_ci_choices(), selected = "bias_corrected"),
        tags$p(class = "structural-option-note", statedu_localized_text(language, "Direct paths, specific and total indirect effects, total effects, and moderated-mediation indices are recomputed in every replicate.", "직접경로, 특정·총 간접효과, 총효과와 조절된 매개효과 index를 매 반복에서 다시 계산합니다."))
      )
    ),
    if (identical(analysis_type, "cfa")) tagList(
      tags$h5(statedu_localized_text(language, "Measurement-model reliability", "측정모형 신뢰도")),
      bootstrap_select("_reliability_bootstrap", "AVE·신뢰도 bootstrap CI", "AVE/reliability bootstrap CI", structural_canvas_bootstrap_replicate_values()),
      bootstrap_details(
        "_reliability_bootstrap",
        numericInput(input_id("_reliability_seed"), statedu_localized_text(language, "AVE/reliability seed", "AVE·신뢰도 seed"), value = default_seed(), min = 1L, step = 1L),
        selectInput(input_id("_reliability_ci_method"), statedu_localized_text(language, "AVE/reliability CI method", "AVE·신뢰도 CI 방법"), choices = bootstrap_ci_choices(TRUE), selected = "bias_corrected")
      ),
      tags$h5(statedu_localized_text(language, "Global model fit", "전체 모형 적합도")),
      bootstrap_select("_bollen_stine_bootstrap", "Bollen-Stine 전체 적합도 bootstrap", "Bollen-Stine global-fit bootstrap", structural_canvas_bootstrap_replicate_values()),
      bootstrap_details(
        "_bollen_stine_bootstrap",
        numericInput(input_id("_bollen_stine_seed"), statedu_localized_text(language, "Bollen-Stine seed", "Bollen-Stine 난수 시드"), value = default_seed(), min = 1L, step = 1L),
        tags$p(class = "structural-option-note", statedu_localized_text(language, "Available only for complete continuous single-group CFA estimated with ML.", "결측이 없는 연속형 단일집단 ML CFA에서만 실행됩니다."))
      )
    ),
    if (!identical(analysis_type, "plssem")) tagList(
      tags$h5(statedu_localized_text(language, "Discriminant validity", "판별타당도")),
      bootstrap_select("_htmt_bootstrap", "HTMT 부트스트랩 CI", "HTMT bootstrap CI", structural_canvas_bootstrap_replicate_values()),
      bootstrap_details(
        "_htmt_bootstrap",
        numericInput(input_id("_htmt_seed"), statedu_localized_text(language, "HTMT seed", "HTMT 난수 시드"), value = default_seed(), min = 1L, step = 1L),
        selectInput(input_id("_htmt_ci_method"), statedu_localized_text(language, "HTMT CI method", "HTMT CI 방법"), choices = bootstrap_ci_choices(), selected = "bias_corrected")
      )
    ),
    if (identical(analysis_type, "plssem")) tagList(
      tags$h5(statedu_localized_text(language, "PLS parameters and structural effects", "PLS 모수 및 구조효과")),
      bootstrap_select("_pls_bootstrap", "PLS 경로·loading·weight·간접·총효과 bootstrap CI/p", "PLS path/loading/weight/indirect/total-effect bootstrap CI/p", structural_canvas_bootstrap_replicate_values(FALSE), selected = "5000"),
      bootstrap_details(
        "_pls_bootstrap",
        numericInput(input_id("_pls_seed"), statedu_localized_text(language, "PLS bootstrap seed", "PLS 부트스트랩 난수 시드"), value = default_seed(), min = 1L, step = 1L),
        tags$p(class = "structural-option-note", statedu_localized_text(language, "When disabled, point estimates remain available but bootstrap CIs and p values are not reported.", "실행하지 않으면 점추정값은 표시하지만 bootstrap CI와 p 값은 표시하지 않습니다."))
      )
    )
  )

  advanced_tab <- tabPanel(
    statedu_localized_text(language, "Advanced Options", "고급 옵션"),
    tags$p(class = "structural-option-note", statedu_localized_text(language, "Reporting, sensitivity, and supplementary-model settings. These are not required to fit the base model.", "보고·민감도·추가 모형 설정입니다. 기본 모형 적합의 필수 항목은 아닙니다.")),
    selectInput(
      input_id("_power_basis"), statedu_localized_text(language, "A-priori sample-size/power basis", "사전 표본크기·검정력 근거"),
      choices = stats::setNames(
        c("not_recorded", "rmsea_power", "model_monte_carlo", "target_effect_precision", "prior_evidence", "other_documented"),
        c(statedu_localized_text(language, "Not recorded", "기록되지 않음"), statedu_localized_text(language, "RMSEA fit-test power", "RMSEA 적합도 검정력"), statedu_localized_text(language, "Model-specific Monte Carlo", "모형별 Monte Carlo"), statedu_localized_text(language, "Target effect/precision", "목표 효과·정밀도"), statedu_localized_text(language, "Prior evidence", "선행연구 근거"), statedu_localized_text(language, "Other documented basis", "기타 문서화된 근거"))
      ), selected = "not_recorded"
    ),
    textAreaInput(input_id("_power_details"), statedu_localized_text(language, "Power-basis details", "검정력 근거 상세"), rows = 2, placeholder = statedu_localized_text(language, "Record assumed effects/parameters, target power, alpha, attrition/missingness allowance, and software or source.", "가정한 효과크기·모수, 목표 검정력, alpha, 결측/탈락, 사용 도구 또는 문헌을 기록하십시오.")),
    tags$p(class = "structural-option-note", statedu_localized_text(language, "N-to-parameter ratios and the PLS 10-times rule are not a-priori power evidence.", "N/모수 비율이나 PLS 10배 규칙은 사전 검정력 근거가 아닙니다.")),
    if (!identical(analysis_type, "plssem")) conditionalPanel(
      sprintf("input['%s'] == 'ML'", input_id("_estimator")),
      selectInput(
        input_id("_ml_likelihood"), statedu_localized_text(language, "ML likelihood convention", "ML likelihood 관례"),
        choices = stats::setNames(
          c("normal", "wishart"),
          c(
            statedu_localized_text(language, "Normal ML (lavaan default; N multiplier)", "Normal ML (lavaan 기본; N 배율)"),
            statedu_localized_text(language, "Wishart ML (AMOS/LISREL/EQS compatible; N-1 multiplier)", "Wishart ML (AMOS/LISREL/EQS 호환; N-1 배율)")
          )
        ),
        selected = "normal"
      ),
      tags$p(
        class = "structural-option-note",
        statedu_localized_text(language, "Normal ML is the lavaan default. Select Wishart ML only for a matched numerical comparison with AMOS or similar software, while also matching the model, data, and missing-data handling. The full model is refitted under that convention; chi-square is not adjusted post hoc.", "기본값은 lavaan의 Normal ML입니다. AMOS 등과 수치 교차검증할 때만 동일 모형·자료·결측 처리와 함께 Wishart ML을 선택하십시오. 카이제곱만 사후 보정하지 않고 전체 모형을 해당 관례로 다시 적합합니다.")
      )
    ),
    if (!identical(analysis_type, "plssem")) selectInput(
      input_id("_missing_sensitivity_method"), statedu_localized_text(language, "Missing-data sensitivity assessment", "결측 민감도 검토"),
      choices = stats::setNames(
        c("not_assessed", "complete_case_comparison", "multiple_imputation", "delta_pattern_mixture", "external_analysis", "other_documented"),
        c(statedu_localized_text(language, "Not assessed", "평가하지 않음"), statedu_localized_text(language, "Complete-case comparison", "완전사례 비교"), statedu_localized_text(language, "Multiple-imputation comparison", "다중대치 비교"), statedu_localized_text(language, "Delta/pattern-mixture", "델타/패턴혼합"), statedu_localized_text(language, "External sensitivity analysis", "외부 민감도 분석"), statedu_localized_text(language, "Other documented assessment", "기타 문서화된 검토"))
      ), selected = "not_assessed"
    ),
    if (!identical(analysis_type, "plssem")) textAreaInput(input_id("_missing_sensitivity_details"), statedu_localized_text(language, "Sensitivity assumptions, results, and conclusion", "민감도 가정·결과·결론"), rows = 3, placeholder = statedu_localized_text(language, "Record the comparison method, MNAR-departure assumptions, changes in key parameters, and robustness of conclusions.", "비교 방법, MNAR 이탈 가정, 주요 모수 변화와 결론의 강건성을 기록하십시오.")),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_scale"), statedu_localized_text(language, "Latent scale", "잠재변수 스케일"), choices = stats::setNames(c("marker", "variance"), c(statedu_localized_text(language, "Marker loading = 1", "첫 지표 부하량 = 1"), statedu_localized_text(language, "Latent variance = 1", "잠재변수 분산 = 1")))),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_rmsea_ci"), statedu_localized_text(language, "RMSEA confidence level", "RMSEA 신뢰수준"), choices = c("90% CI" = "0.90", "95% CI" = "0.95", "99% CI" = "0.99"), selected = "0.90"),
    if (analysis_type %in% c("cbsem", "sem")) selectInput(input_id("_moderation_method"), statedu_localized_text(language, "Latent-moderation product-indicator method", "잠재조절 product-indicator 방법"), choices = stats::setNames(c("all_pairs_dmc", "matched_pair_dmc", "all_pairs_mean_centered"), c(statedu_localized_text(language, "All-pairs + double-mean-centering", "모든 쌍 + 이중 평균중심화"), statedu_localized_text(language, "Matched-pair + double-mean-centering", "대응 쌍 + 이중 평균중심화"), statedu_localized_text(language, "All-pairs mean-centering (legacy)", "모든 쌍 평균중심화(기존)"))), selected = "all_pairs_dmc"),
    if (identical(analysis_type, "plssem")) tagList(
      selectInput(
        input_id("_moderation_method"),
        statedu_localized_text(language, "PLS latent-interaction method", "PLS 잠재조절 상호작용 방법"),
        choices = stats::setNames(
          c("two_stage", "product_indicator", "orthogonal"),
          c(
            statedu_localized_text(language, "Two-stage (default)", "2단계법(기본)"),
            statedu_localized_text(language, "Product indicator", "곱지표법"),
            statedu_localized_text(language, "Orthogonalizing", "직교화법")
          )
        ),
        selected = "two_stage"
      ),
      tags$p(
        class = "structural-option-note",
        statedu_localized_text(language, "Used only when a latent moderator is linked to a structural path. The two-stage method is the default; every bootstrap draw re-estimates both the measurement model and interaction.", "잠재 조절변수를 경로에 연결한 경우에만 적용합니다. 2단계법을 기본으로 하며, 부트스트랩에서는 각 재표집마다 측정모형과 상호작용을 모두 다시 추정합니다.")
      )
    )
  )

  multigroup_tab <- if (analysis_type %in% c("cfa", "cbsem", "sem", "plssem")) tabPanel(
    statedu_localized_text(language, "Multi-group Analysis", "다집단분석"),
    tags$p(
      class = "structural-option-note",
      if (identical(analysis_type, "cfa")) {
        statedu_localized_text(language, "Assess measurement invariance across the selected grouping variable.", "집단변수에 따른 측정불변성을 단계별로 검토합니다.")
      } else if (analysis_type %in% c("cbsem", "sem")) {
        statedu_localized_text(language, "Assess measurement invariance before comparing structural paths across groups.", "측정불변성을 먼저 확인한 뒤 집단 간 구조경로를 비교합니다.")
      } else {
        statedu_localized_text(language, "Assess PLS composite-score invariance with MICOM. Multi-group inference for PLSc common factors is not supported.", "MICOM으로 PLS 합성점수 불변성을 검토합니다. PLSc 공통요인 다집단 추론은 지원하지 않습니다.")
      }
    ),
    if (analysis_type %in% c("cfa", "cbsem", "sem", "plssem")) checkboxInput(
      input_id("_invariance_enabled"),
      if (identical(analysis_type, "cfa")) {
        statedu_localized_text(language, "Measurement invariance analysis", "측정불변성 분석")
      } else if (analysis_type %in% c("cbsem", "sem")) {
        statedu_localized_text(language, "Structural path group comparison", "구조경로 집단비교")
      } else {
        statedu_localized_text(language, "PLS MICOM measurement invariance", "PLS MICOM 측정불변성")
      }, value = FALSE
    ),
    if (analysis_type %in% c("cfa", "cbsem", "sem", "plssem")) selectInput(
      input_id("_invariance_group"),
      statedu_localized_text(language, "Grouping variable", "집단변수"),
      choices = c(stats::setNames("", statedu_localized_text(language, "Select a grouping variable", "집단변수 선택")), grouping_choices),
      selected = ""
    ),
    if (analysis_type %in% c("cbsem", "sem", "plssem")) tagList(
      radioButtons(
        input_id("_invariance_path_scope"),
        statedu_localized_text(language, "Structural-path comparison scope", "구조경로 비교 범위"),
        choices = stats::setNames(
          c("all", "selected"),
          c(statedu_localized_text(language, "All structural paths", "모든 구조경로"), statedu_localized_text(language, "Selected structural paths", "선택한 구조경로"))
        ),
        selected = "all",
        inline = TRUE
      ),
      conditionalPanel(
        sprintf("input['%s'] === 'selected'", input_id("_invariance_path_scope")),
        checkboxGroupInput(
          input_id("_invariance_selected_path_ids"),
          statedu_localized_text(language, "Structural paths to compare", "비교할 구조경로"),
          choices = character(0),
          selected = character(0)
        ),
        tags$p(
          class = "structural-option-note",
          statedu_localized_text(language, "Only structural paths currently connected in the model are listed.", "현재 모형에 실제로 연결된 구조경로만 표시합니다.")
        )
      )
    ),
    if (identical(analysis_type, "plssem")) selectInput(
      input_id("_micom_permutations"),
      statedu_localized_text(language, "MICOM permutations", "MICOM 순열 횟수"),
      choices = c("5,000" = "5000", "10,000" = "10000", "20,000" = "20000", "50,000" = "50000"),
      selected = "5000"
    ),
    if (identical(analysis_type, "plssem")) numericInput(input_id("_micom_seed"), statedu_localized_text(language, "MICOM seed", "MICOM 난수 시드"), value = default_seed(), min = 1L, step = 1L)
  )

  validity_tab <- tabPanel(
    statedu_localized_text(language, "Validity", "타당도"),
    if (!identical(analysis_type, "plssem")) radioButtons(input_id("_validity_formula"), statedu_localized_text(language, "AVE/CR formula", "AVE·CR 계산 방식"), choices = stats::setNames(c("standardized", "model_implied"), c(statedu_localized_text(language, "Standardized loadings (Fornell-Larcker)", "표준화 부하량(Fornell-Larcker)"), statedu_localized_text(language, "Model-implied parameters (Raykov)", "모형모수 방식(Raykov 계열)"))), selected = "standardized", inline = TRUE),
    if (identical(analysis_type, "cfa")) checkboxInput(input_id("_mi_holdout_enabled"), statedu_localized_text(language, "MI exploration/validation split", "MI 탐색·검증 표본분할"), value = FALSE),
    if (identical(analysis_type, "cfa")) selectInput(input_id("_mi_holdout_fraction"), statedu_localized_text(language, "Validation-sample fraction", "검증표본 비율"), choices = c("20%" = "0.20", "30%" = "0.30", "40%" = "0.40"), selected = "0.30"),
    if (identical(analysis_type, "cfa")) numericInput(input_id("_mi_holdout_seed"), statedu_localized_text(language, "Sample-split seed", "표본분할 seed"), value = 13579L, min = 1L, step = 1L),
    if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", statedu_localized_text(language, "MI splitting is for continuous ML/MLR CFA and cannot be combined with measurement invariance or Heywood-constrained reanalysis.", "MI 표본분할은 연속형 ML/MLR CFA 전용이며 측정불변성 또는 Heywood 제약 재분석과 동시에 사용할 수 없습니다.")),
    if (identical(analysis_type, "cfa")) checkboxInput(input_id("_parcel_enabled"), statedu_localized_text(language, "Create parcel item-level model", "Parcel item-level 모형 생성"), value = FALSE),
    if (identical(analysis_type, "cfa")) selectInput(input_id("_parcel_construct"), statedu_localized_text(language, "Target common factor", "대상 공통요인"), choices = character(0)),
    if (identical(analysis_type, "cfa")) selectInput(input_id("_parcel_count"), statedu_localized_text(language, "Number of parcels", "Parcel 수"), choices = c("3" = "3", "4" = "4"), selected = "3"),
    if (identical(analysis_type, "cfa")) textAreaInput(input_id("_parcel_purpose"), statedu_localized_text(language, "Purpose and substantive justification", "적용 목적과 이론적 근거"), rows = 3, placeholder = statedu_localized_text(language, "Document why parceling is being considered instead of retaining item-level analysis.", "문항 수준 분석 대신 parcel을 고려하는 이유를 기록하십시오.")),
    if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", statedu_localized_text(language, "Disabled by default. The item-level model is fitted before creating the parcel-factor model.", "기본값은 비활성화입니다. 문항 수준 모형을 먼저 적합한 후 parcel 하위요인 모형을 생성합니다.")),
    if (identical(analysis_type, "plssem")) selectInput(input_id("_redundancy_construct"), statedu_localized_text(language, "Formative composite", "형성형 합성변수"), choices = character(0)),
    if (identical(analysis_type, "plssem")) selectInput(input_id("_redundancy_criterion"), statedu_localized_text(language, "Global criterion variable", "전역 기준변수"), choices = stats::setNames("", statedu_localized_text(language, "Not selected", "선택하지 않음"))),
    if (identical(analysis_type, "plssem")) tags$p(class = "structural-option-note", statedu_localized_text(language, "Redundancy analysis relates the formative-composite score to a separate global criterion measuring the same concept.", "Redundancy analysis는 형성형 합성변수 점수와 동일한 개념을 측정하는 별도 전역 기준변수의 관계를 평가합니다."))
  )

  diagnostics_tab <- tabPanel(
    statedu_localized_text(language, "Diagnostics", "진단"),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_htmt_threshold"), statedu_localized_text(language, "HTMT threshold", "HTMT 기준"), choices = stats::setNames(c("0.85", "0.90"), c(statedu_localized_text(language, "Strict (.85)", "엄격(.85)"), statedu_localized_text(language, "Lenient (.90)", "완화(.90)"))), selected = "0.85"),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_mi_mode"), statedu_localized_text(language, "MI output method", "MI 출력 기준"), choices = stats::setNames(c("theory", "conventional"), c(statedu_localized_text(language, "Theory-allowed MI with cumulative fit", "이론적 허용 MI + 누적 적합도"), statedu_localized_text(language, "Conventional output (all MI)", "일반 프로그램 방식(전체 MI)"))), selected = "theory"),
    if (identical(analysis_type, "plssem")) tagList(
      tags$p(class = "structural-option-note", statedu_localized_text(language, "PLS diagnostics distinguish repeated PLSpredict out-of-sample assessment while reviewing residual-based approximate fit, collinearity, and construct-level quality indices.", "PLS 진단은 반복 PLSpredict 표본외 예측, 잔차 기반 근사 적합도, 공선성과 구성개념별 품질지표를 구분해 검토합니다.")),
      selectInput(input_id("_pls_predict_folds"), statedu_localized_text(language, "PLSpredict cross-validation", "PLSpredict 교차검증"), choices = stats::setNames(c("0", "5", "10"), c(statedu_localized_text(language, "Do not compute", "계산하지 않음"), statedu_localized_text(language, "5-fold", "5겹"), statedu_localized_text(language, "10-fold", "10겹"))), selected = "0"),
      selectInput(input_id("_pls_predict_reps"), statedu_localized_text(language, "PLSpredict repetitions", "PLSpredict 반복"), choices = c("5" = "5", "10" = "10", "20" = "20"), selected = "10"),
      numericInput(input_id("_pls_predict_seed"), statedu_localized_text(language, "PLSpredict seed", "PLSpredict 난수 시드"), value = default_seed(), min = 1L, step = 1L),
      tags$p(class = "structural-option-note", statedu_localized_text(language, "The Direct Antecedents scheme compares indicator-level out-of-sample RMSE/MAE against PLS and linear-model benchmarks.", "Direct Antecedents 방식으로 지표별 표본 밖 RMSE/MAE를 PLS와 선형모형 기준값에 비교합니다."))
    )
  )

  common_method_tab <- if (!identical(analysis_type, "plssem")) tabPanel(
    statedu_localized_text(language, "Common Method", "동일방법편의"),
    checkboxInput(input_id("_common_method_enabled"), statedu_localized_text(language, "Run common method bias diagnostics", "동일방법편의 진단 실행"), value = FALSE),
    checkboxGroupInput(
      input_id("_common_method_methods"), statedu_localized_text(language, "Common method diagnostics", "진단 방법"),
      choices = stats::setNames(
        c("harman", "single_factor_cfa", "common_latent_factor"),
        c(statedu_localized_text(language, "Harman single-factor screen", "Harman 단일요인 점검"), statedu_localized_text(language, "Single-factor CFA comparison", "단일요인 CFA 비교"), statedu_localized_text(language, "Common latent factor screen", "공통잠재요인 점검"))
      ), selected = c("harman", "single_factor_cfa")
    ),
    textAreaInput(input_id("_common_method_procedural_controls"), statedu_localized_text(language, "Design-stage procedural controls", "설계단계 절차적 통제"), value = "", rows = 3, placeholder = statedu_localized_text(language, "e.g., source/time separation, anonymity, item ordering, scale-format design", "예: 응답원·측정시점 분리, 익명성, 문항 순서·척도 형식 설계")),
    textInput(input_id("_common_method_marker_variable"), statedu_localized_text(language, "Marker variable (record only)", "Marker variable(기록용)"), value = ""),
    textAreaInput(input_id("_common_method_marker_rationale"), statedu_localized_text(language, "Marker selection and validity rationale", "Marker 선정·타당화 근거"), value = "", rows = 2),
    tags$p(class = "structural-option-note", statedu_localized_text(language, "Common method diagnostics are evidence screens, not proof that bias is absent. Marker information is recorded for audit only.", "동일방법편의 진단은 증거 점검용이며, 편의가 없다는 증명이 아닙니다. Marker 정보는 감사기록용입니다."))
  )

  div(
    class = "custom-model-analysis-options structural-run-options-tabs analysis-tabbed-options",
    tabsetPanel(type = "tabs", estimation_tab, bootstrap_tab, advanced_tab, multigroup_tab, validity_tab, diagnostics_tab, common_method_tab)
  )
}
