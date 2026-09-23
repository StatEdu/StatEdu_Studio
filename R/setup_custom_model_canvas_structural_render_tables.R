# Shared structural canvas result table rendering helpers.

# Standalone renderer validators use the same contract as the full application.
if (!exists("result_note_paragraph", mode = "function")) {
  .note_sources <- Filter(function(path) is.character(path) && length(path) == 1L && nzchar(path), lapply(sys.frames(), function(frame) frame$ofile))
  .note_path <- if (length(.note_sources)) file.path(dirname(tail(.note_sources, 1L)[[1]]), "result_table_ui.R") else file.path("R", "result_table_ui.R")
  if (!file.exists(.note_path)) .note_path <- file.path("R", "result_table_ui.R")
  if (!file.exists(.note_path)) .note_path <- "result_table_ui.R"
  source(.note_path, local = TRUE, encoding = "UTF-8")
  rm(.note_sources, .note_path)
}

structural_canvas_numeric_display_cell <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(FALSE)
  grepl("^(?:[<>]=?)?-?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?(?:[%†‡¶*]+)?$", value)
}

structural_canvas_html_cell <- function(value, header = FALSE) {
  value <- as.character(value %||% "")
  if (isTRUE(header)) return(tags$th(class = "structural-table-header-cell", value))
  if (grepl("^\\((?:N/A[†‡¶]?|(?:[<>]=?)?-?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?[†‡¶]?)\\)$", trimws(value))) {
    return(tags$td(class = "text-center structural-parenthetical-cell", value))
  }
  if (structural_canvas_numeric_display_cell(value)) {
    return(tags$td(class = "text-center structural-numeric-cell", tags$span(class = "structural-numeric-value", value)))
  }
  tags$td(value)
}

structural_canvas_table_needs_landscape <- function(table, force = FALSE) {
  if (isTRUE(force)) return(TRUE)
  if (!is.data.frame(table) || !ncol(table)) return(FALSE)
  # Square matrices include one row-label column; long labels wrap in portrait.
  if (ncol(table) == nrow(table) + 1L &&
      identical(as.character(table[[1L]]), names(table)[-1L])) {
    return(nrow(table) > 9L)
  }

  # Use the same intrinsic-width decision as the shared screen-table
  # contract.  Column count alone misses narrow tables whose path names or
  # diagnostic text are too wide for B5 portrait.
  intrinsic_width <- if (exists("result_table_intrinsic_width", mode = "function")) {
    tryCatch(
      result_table_intrinsic_width(table),
      error = function(error) NA_real_
    )
  } else {
    numeric_like <- function(values) {
      values <- trimws(as.character(values))
      values <- values[nzchar(values)]
      length(values) == 0L || mean(grepl(
        "^(?:[-+]?\\d*(?:\\.\\d+)?|<\\.?\\d+|NA|Inf|-Inf)(?:\\s*\\([^)]*\\))?$",
        values,
        perl = TRUE
      )) >= 0.8
    }
    widths <- vapply(seq_len(ncol(table)), function(index) {
      values <- c(names(table)[[index]], as.character(table[[index]] %||% ""))
      lines <- unlist(strsplit(values, "\n", fixed = TRUE), use.names = FALSE)
      longest_line <- if (length(lines)) max(nchar(lines, type = "width"), na.rm = TRUE) else 0L
      base <- if (index == 1L) 118 else 62
      cap <- if (index == 1L || !numeric_like(table[[index]])) 220 else 92
      max(base, min(cap, 18 + longest_line * 6.2))
    }, numeric(1))
    sum(widths)
  }
  portrait_capacity <- if (exists("result_table_portrait_capacity", mode = "function")) {
    result_table_portrait_capacity()
  } else {
    590L
  }
  longest_lines <- vapply(seq_len(ncol(table)), function(index) {
    values <- c(names(table)[[index]], as.character(table[[index]] %||% ""))
    lines <- unlist(strsplit(values, "\n", fixed = TRUE), use.names = FALSE)
    if (length(lines)) max(nchar(lines, type = "width"), na.rm = TRUE) else 0L
  }, numeric(1))
  padding_guard <- if (any(longest_lines > 24L)) 8 * ncol(table) else 0
  is.finite(intrinsic_width) && intrinsic_width + padding_guard > portrait_capacity
}

structural_canvas_appendix_ui_text <- function(value, language = NULL) {
  value <- as.character(value %||% "")
  language <- if (exists("result_appendix_table_language", mode = "function")) {
    result_appendix_table_language(language)
  } else if (exists("normalize_app_language", mode = "function")) {
    normalize_app_language(language %||% getOption("statedu.app_language", "ko"))
  } else {
    "ko"
  }
  if (identical(language, "en") || is.na(value) || !nzchar(value)) return(value)

  common <- if (exists("result_appendix_ui_text", mode = "function")) {
    tryCatch(result_appendix_ui_text(value, language), error = function(error) value)
  } else {
    value
  }
  if (!identical(common, value)) return(as.character(common))
  if (!identical(language, "ko")) return(value)

  korean <- c(
    # Generic structural headers.
    "Factor1" = "요인 1", "Factor2" = "요인 2", "Factor 1" = "요인 1", "Factor 2" = "요인 2",
    "Correlation" = "상관", "Absolute correlation" = "절대 상관", "Severity" = "심각도",
    "Indicator" = "지표", "Indicator 1" = "지표 1", "Indicator 2" = "지표 2",
    "Category" = "범주", "Count" = "빈도", "Percent" = "백분율",
    "Valid pairs" = "유효 쌍", "Cells" = "셀", "Empty cells" = "빈 셀",
    "Sparse nonempty cells" = "희소 비빈 셀", "Minimum nonzero count" = "최소 비영 빈도",
    "Empty %" = "빈 셀 비율", "Missing" = "결측", "Missing cells" = "결측 셀",
    "Replacement mean" = "대체 평균", "Primary missing-data method" = "주 결측자료 처리법",
    "Sensitivity assessment" = "민감도 평가", "Assumptions, results, and conclusion" = "가정·결과·결론",
    "Guidance" = "안내", "Row" = "행", "Mahalanobis" = "Mahalanobis 거리",
    "Determinacy" = "결정성", "Score reliability" = "점수 신뢰도",
    "CI lower" = "신뢰구간 하한", "CI upper" = "신뢰구간 상한", "CI reaches |1|" = "신뢰구간의 |1| 도달",
    "95% CI lower" = "95% CI 하한", "95% CI upper" = "95% CI 상한",
    "Construct" = "구성개념", "Construct type" = "구성개념 유형", "Latent factor" = "잠재요인",
    "Valid replicates" = "유효 반복", "Requested replicates" = "요청 반복", "Valid %" = "유효 비율",
    "CI method" = "신뢰구간 방법", "Residual variance" = "오차분산",
    "Standardized residual" = "표준화 오차", "Observed variance" = "관측분산",
    "Applied %" = "적용 비율", "Fixed value" = "고정값", "Variance" = "분산",
    "Matrix" = "행렬", "Minimum eigenvalue" = "최소 고유값", "Condition number" = "조건수",
    "Step" = "단계", "Skipped unsafe" = "안전성 문제로 제외", "Covariance" = "공분산",
    "MI p" = "MI p", "BH-adjusted p" = "BH 보정 p", "Std. EPC" = "표준화 EPC",
    "Justification" = "근거", "Admissible" = "허용 가능", "Converged" = "수렴",
    "Element" = "요소", "Chisq" = "카이제곱", "DeltaP" = "p 변화", "Comparison" = "비교",
    "Metric" = "지표", "Estimator" = "추정량", "Params" = "모수 수",
    "Close-fit H0" = "근접적합 H0", "Not-close H0" = "비근접적합 H0",
    "Close-fit p" = "근접적합 p", "Not-close p" = "비근접적합 p",
    "Observed chi-square" = "관측 카이제곱", "Bootstrap p" = "부트스트랩 p",
    "Monte Carlo SE" = "Monte Carlo 표준오차", "Monte Carlo 95% lower" = "Monte Carlo 95% 하한",
    "Monte Carlo 95% upper" = "Monte Carlo 95% 상한",
    # Diagnostic values. Identifier columns are deliberately excluded by the
    # table localizer below so user variable, factor, construct, and path names
    # that happen to match one of these words are never rewritten.
    "Mardia skewness" = "Mardia 왜도", "Mardia kurtosis" = "Mardia 첨도",
    "Unavailable" = "산출 불가", "Inadmissible" = "허용 불가", "Severe" = "심각",
    "High" = "높음", "Review" = "검토", "Below correlation review reference" = "상관 검토 기준 미만",
    "Empty" = "빈 범주", "Sparse" = "희소", "Dominant (>=95%)" = "지배적(>=95%)",
    "No sparsity flag" = "희소성 표시 없음", "None" = "없음", "Review complexity" = "복잡성 검토",
    "Limited" = "제한적", "Not recorded" = "기록되지 않음", "Not assessed" = "평가하지 않음",
    "Complete-case comparison" = "완전사례 비교", "Multiple-imputation comparison" = "다중대치 비교",
    "Delta/pattern-mixture" = "델타/패턴혼합", "External sensitivity analysis" = "외부 민감도 분석",
    "Other documented assessment" = "기타 문서화된 평가", "Not required - no incomplete indicator cases" = "필요하지 않음 - 불완전 지표 사례 없음",
    "Documented" = "문서화됨", "Estimated" = "추정", "Fixed" = "고정", "TRUE" = "예", "FALSE" = "아니요",
    "Reference only" = "기준 참고", "Saturated/df <= 0" = "포화모형/df <= 0",
    "Common references: .90/.95" = "일반적 참고값: .90/.95",
    "Common references: .10/.08" = "일반적 참고값: .10/.08",
    "Common references: .08/.06" = "일반적 참고값: .08/.06",
    "Single model; delta not applicable" = "단일 모형; 차이값 해당 없음",
    "Comparable on observations, variables, estimator family, and admissibility" = "관측치·변수·추정량 계열·허용성 기준에서 비교 가능",
    "Not comparable; inadmissible model; delta suppressed" = "비교 불가; 허용 불가 모형으로 차이값 미제시",
    "Not comparable; delta suppressed" = "비교 불가; 차이값 미제시",
    "Research model" = "연구모형", "Modified model" = "수정모형", "Exploratory modified model" = "탐색적 수정모형",
    "At/above descriptive .90 reference" = "기술적 .90 기준 이상",
    "Between descriptive .80 and .90 references" = "기술적 .80~.90 기준 사이",
    "Below descriptive .80; review score use" = "기술적 .80 기준 미만; 점수 사용 검토",
    "Bias-corrected (BC)" = "편향보정(BC)", "BCa unavailable" = "BCa 산출 불가",
    "Percentile" = "백분위수", "Adequate" = "충분", "Unreliable" = "신뢰 불가",
    "Cronbach's α" = "Cronbach α", "McDonald's ωtotal" = "McDonald ω total",
    "Heywood" = "Heywood 사례", "Latent Heywood" = "잠재변수 Heywood 사례",
    "Residual covariance (theta)" = "오차 공분산(theta)", "Latent covariance" = "잠재 공분산",
    "Parameter-estimate covariance (vcov)" = "모수추정치 공분산(vcov)",
    "Not positive semidefinite" = "양의 준정부호 아님", "Near singular / boundary" = "특이/경계에 가까움",
    "Ill-conditioned" = "조건 불량", "Unreliable standard errors" = "표준오차 신뢰 불가",
    "Empirical identification boundary" = "경험적 식별 경계", "Ill-conditioned standard errors" = "표준오차 조건 불량",
    "Latent constructs" = "잠재 구성개념", "Observed indicators" = "관측지표",
    "Structural paths" = "구조경로", "Latent scaling" = "잠재변수 척도화",
    "Model df" = "모형 자유도", "Free parameters" = "자유모수", "A-priori power basis" = "사전 검정력 근거",
    "Latent variance = 1" = "잠재분산 = 1", "Marker loading = 1" = "기준지표 적재량 = 1",
    "Factor-score quality" = "요인점수 품질", "Latent correlation confidence intervals" = "잠재상관 신뢰구간",
    "Modification indices (MI)" = "수정지수(MI)", "Not provided" = "제공되지 않음",
    "Indicator-mean replacement can attenuate variance and distort associations when missingness is material or systematic. Report replaced rows/cells and assess whether key conclusions change under a justified sensitivity analysis." = "지표 평균대체는 결측이 실질적이거나 체계적일 때 분산을 줄이고 연관성을 왜곡할 수 있습니다. 대체한 행·셀 수를 보고하고 정당화된 민감도 분석에서 주요 결론이 달라지는지 평가하십시오.",
    "Sensitivity records are user-supplied evidence; the PLS engine uses indicator-mean replacement and does not perform or validate the stated external analysis." = "민감도 기록은 사용자가 제공한 근거입니다. PLS 엔진은 지표 평균대체를 사용하며 명시된 외부 분석을 수행하거나 검증하지 않습니다.",
    "FIML relies on MAR conditional on modeled variables. Document a sensitivity assessment and whether key conclusions change under plausible departures." = "FIML은 모형에 포함된 변수를 조건으로 한 MAR 가정에 의존합니다. 민감도 평가와 가능한 가정 이탈에서 주요 결론이 달라지는지 기록하십시오.",
    "Sensitivity records are user-supplied evidence and do not cause this engine to perform or validate the stated external analysis." = "민감도 기록은 사용자가 제공한 근거이며, 이 엔진이 명시된 외부 분석을 수행하거나 검증하게 하지는 않습니다."
  )
  if (value %in% names(korean)) unname(korean[[value]]) else value
}

structural_canvas_localize_appendix_table <- function(table, language = NULL) {
  if (!is.data.frame(table)) return(table)
  source_table <- table
  language <- if (exists("result_appendix_table_language", mode = "function")) {
    result_appendix_table_language(language)
  } else if (exists("normalize_app_language", mode = "function")) {
    normalize_app_language(language %||% getOption("statedu.app_language", "ko"))
  } else {
    "ko"
  }
  if (!identical(language, "ko")) return(result_appendix_localize_table(table, language))

  original_names <- names(table)
  protected_identifier_columns <- tolower(c(
    "Variable", "Variables", "Factor", "Factor1", "Factor2", "Factor 1", "Factor 2",
    "Latent", "Latent factor", "Indicator", "Indicator 1", "Indicator 2", "Construct",
    "Outcome", "Predictor", "Path", "lhs", "rhs", "Element", "Category"
  ))
  diagnostic_columns <- tolower(c(
    "Status", "Severity", "Guidance", "Recommendation", "Test", "Type", "Method",
    "Result", "Item", "Metric", "Matrix", "Statistic", "Selection", "Selected",
    "Decision", "Assessment", "Reason", "Priority", "Action", "Admissible", "Converged",
    "Check", "Model", "Value", "CI reaches |1|", "CI method", "Sensitivity assessment",
    "Primary missing-data method", "Assumptions, results, and conclusion", "Inference source",
    "Bootstrap status", "Construct type", "Covariance", "Reference", "Comparison"
  ))
  translate_cell <- function(value) {
    value <- as.character(value %||% "")
    lines <- strsplit(value, "\n", fixed = TRUE)[[1L]]
    paste(vapply(lines, structural_canvas_appendix_ui_text, character(1), language = language), collapse = "\n")
  }
  for (column in original_names) {
    key <- tolower(column)
    if (key %in% protected_identifier_columns || !key %in% diagnostic_columns) next
    if (!is.character(table[[column]]) && !is.factor(table[[column]]) && !is.logical(table[[column]])) next
    table[[column]] <- vapply(as.character(table[[column]]), translate_cell, character(1))
  }
  names(table) <- vapply(original_names, structural_canvas_appendix_ui_text, character(1), language = language)
  attr(table, "result_table_role") <- "appendix"
  attr(table, "result_table_language") <- language
  result_appendix_preserve_data(table, source_table)
}

structural_canvas_table_sheet <- function(content, table = NULL, role = c("appendix", "main"),
                                          orientation = c("auto", "portrait", "landscape"),
                                          language = NULL, note = NULL,
                                          note_class = "structural-result-note structural-main-note",
                                          title = NULL, title_class = "structural-result-table-title") {
  role <- match.arg(role)
  orientation <- match.arg(orientation)
  if (identical(orientation, "auto")) {
    orientation <- if (structural_canvas_table_needs_landscape(table)) "landscape" else "portrait"
  }
  language <- if (exists("result_table_language", mode = "function")) {
    result_table_language(role, language)
  } else if (identical(role, "main")) {
    "en"
  } else if (exists("normalize_app_language", mode = "function")) {
    normalize_app_language(language %||% getOption("statedu.app_language", "ko"))
  } else {
    "ko"
  }
  if (identical(role, "appendix")) {
    title <- structural_canvas_appendix_ui_text(title, language)
  }
  title <- trimws(as.character(title %||% ""))
  note_is_tag <- inherits(note, "shiny.tag") || inherits(note, "shiny.tag.list")
  note_content <- if (note_is_tag) {
    note
  } else {
    if (identical(role, "appendix")) note <- structural_canvas_appendix_ui_text(note, language)
    note <- trimws(as.character(note %||% ""))
    if (nzchar(note)) result_note_paragraph(class = note_class, note) else NULL
  }
  content <- result_ci_header(content)
  if (isTRUE(attr(content, "result_ci_header"))) note_content <- result_ci_note(list(note_content))
  tags$div(
    class = paste(
      "structural-table-sheet structural-table-page-b5 structural-table-font-standard",
      paste0("structural-table-role-", role),
      paste0("structural-table-orientation-", orientation)
    ),
    lang = language,
    `data-result-table-sheet` = "true",
    `data-result-table-role` = role,
    `data-result-table-language` = language,
    `data-result-table-orientation` = orientation,
    `data-table-role` = role,
    `data-paper-size` = "B5",
    `data-orientation` = orientation,
    if (nzchar(title)) tags$h5(class = title_class, title),
    content,
    note_content
  )
}

structural_canvas_basic_html_table <- function(table, class = "table table-striped table-bordered",
                                               role = c("appendix", "main"),
                                               orientation = c("auto", "portrait", "landscape"),
                                               language = NULL, note = NULL,
                                               note_class = "structural-result-note structural-main-note",
                                               title = NULL, title_class = "structural-result-table-title") {
  if (!is.data.frame(table) || !nrow(table) || !ncol(table)) return(NULL)
  table <- result_ci_expand_columns(table, force = any(grepl("bootstrap|부트스트랩", as.character(note %||% ""), ignore.case=TRUE)))
  role <- match.arg(role)
  orientation <- match.arg(orientation)
  language <- language %||% attr(table, "result_table_language", exact = TRUE)
  effective_language <- if (exists("result_table_language", mode = "function")) {
    result_table_language(role, language)
  } else if (identical(role, "main")) {
    "en"
  } else if (exists("normalize_app_language", mode = "function")) {
    normalize_app_language(language %||% getOption("statedu.app_language", "ko"))
  } else {
    "ko"
  }
  if (identical(role, "appendix")) {
    table <- structural_canvas_localize_appendix_table(table, effective_language)
  }
  table_class <- paste(unique(c(strsplit(class, "\\s+")[[1]], "structural-result-table")), collapse = " ")
  structural_canvas_table_sheet(
    tags$div(class = "table-responsive", tags$table(class = table_class,
      tags$thead(tags$tr(lapply(names(table), structural_canvas_html_cell, header = TRUE))),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), structural_canvas_html_cell))))
    )),
    table = table,
    role = role,
    orientation = orientation,
    language = effective_language,
    note = note,
    note_class = note_class,
    title = title,
    title_class = title_class
  )
}

structural_canvas_localize_reporting_metadata <- function(table, language = NULL, rename_headers = TRUE) {
  if (is.data.frame(table) && nrow(table) && !normalize_app_language(language) %in% c("en", "ko")) {
    translate <- function(value) statedu_localized_text(language, value)
    if ("Effect" %in% names(table)) table$Effect <- vapply(as.character(table$Effect), function(value) {
      key <- switch(value, Direct="Direct effect", Indirect="Indirect effect", Total="Total effect", "Specific indirect"="Specific indirect effects", value)
      translate(key)
    }, character(1), USE.NAMES = FALSE)
    for (column in intersect(c("Inference source", "Bootstrap status", "BH family", "B CI source", "beta CI source", "CI source", "Direct CI source", "Indirect CI source", "Total CI source"), names(table))) {
      table[[column]] <- if (identical(column, "BH family")) vapply(as.character(table[[column]]), function(value) {
        if (value %in% c("Direct structural paths", "Specific indirect effects", "Other indirect and total effects", "Fixed parameter (not tested)", "Fixed effect (not tested)")) translate(value) else value
      }, character(1), USE.NAMES = FALSE) else structural_canvas_localize_inference_text(table[[column]], language)
    }
    if (isTRUE(rename_headers)) names(table) <- vapply(names(table), translate, character(1), USE.NAMES = FALSE)
    return(table)
  }
  if (!is.data.frame(table) || !nrow(table) ||
      !identical(normalize_app_language(language), "ko")) return(table)
  replace_exact <- function(values, replacements) {
    values <- as.character(values)
    matched <- match(values, names(replacements))
    replace <- !is.na(matched)
    values[replace] <- unname(replacements[matched[replace]])
    values
  }
  if ("Effect" %in% names(table)) {
    table$Effect <- replace_exact(table$Effect, c(
      "Direct" = "직접효과", "Specific indirect" = "특정 간접효과",
      "Indirect" = "전체 간접효과", "Total" = "총효과"
    ))
  }
  if ("Inference source" %in% names(table)) {
    table[["Inference source"]] <- replace_exact(table[["Inference source"]], c(
      "Model-based normal-theory" = "모형기반 정규이론",
      "Bootstrap (empirical two-sided p)" = "부트스트랩(경험적 양측 p)",
      "Bootstrap requested - inference suppressed" = "부트스트랩 요청됨 - 추론값 억제",
      "Bootstrap pending - inference suppressed" = "부트스트랩 대기 중 - 추론값 억제",
      "Bootstrap canceled - inference suppressed" = "부트스트랩 취소됨 - 추론값 억제",
      "Bootstrap failed - inference suppressed" = "부트스트랩 실패 - 추론값 억제",
      "Bootstrap blocked - original model ineligible" = "부트스트랩 실행 차단 - 원모형 부적격",
      "Bootstrap unavailable - inference suppressed" = "부트스트랩 결과 없음 - 추론값 억제",
      "Fixed parameter - no inferential test" = "고정모수 - 추론검정 없음",
      "Fixed effect - no inferential test" = "고정효과 - 추론검정 없음"
    ))
  }
  localize_ci_source <- function(values) {
    structural_canvas_localize_inference_text(values, language)
  }
  for (column in intersect(c(
    "B CI source", "beta CI source", "CI source",
    "Direct CI source", "Indirect CI source", "Total CI source"
  ), names(table))) {
    table[[column]] <- localize_ci_source(table[[column]])
  }
  if ("Bootstrap status" %in% names(table)) {
    table[["Bootstrap status"]] <- replace_exact(table[["Bootstrap status"]], c(
      "Not requested" = "실행하지 않음", "Adequate" = "충분",
      "Caution" = "주의", "Unreliable" = "신뢰 불가",
      "Bootstrap pending - inference suppressed" = "부트스트랩 대기 중 - 추론값 억제",
      "Bootstrap canceled - inference suppressed" = "부트스트랩 취소됨 - 추론값 억제",
      "Bootstrap failed - inference suppressed" = "부트스트랩 실패 - 추론값 억제",
      "Bootstrap blocked - original model ineligible" = "부트스트랩 실행 차단 - 원모형 부적격",
      "Bootstrap unavailable - inference suppressed" = "부트스트랩 결과 없음 - 추론값 억제",
      "Not applicable - fixed parameter" = "해당 없음 - 고정모수",
      "Fixed effect - no inferential test" = "고정효과 - 추론검정 없음"
    ))
  }
  if ("BH family" %in% names(table)) {
    table[["BH family"]] <- replace_exact(table[["BH family"]], c(
      "Direct structural paths" = "직접 구조경로",
      "Specific indirect effects" = "특정 간접효과",
      "Other indirect and total effects" = "기타 간접·총효과",
      "Fixed parameter (not tested)" = "고정모수(검정 제외)",
      "Fixed effect (not tested)" = "고정효과(검정 제외)"
    ))
  }
  if (isTRUE(rename_headers)) {
    headers <- c(
      "Path" = "경로", "Effect" = "효과", "Outcome" = "결과변수",
      "Predictor" = "예측변수", "BH-adjusted p" = "BH 보정 p",
      "B CI source" = "B CI 산출 근거", "beta CI source" = "beta CI 산출 근거",
      "CI source" = "CI 산출 근거", "Inference source" = "추론 산출 근거",
      "Valid bootstrap" = "유효 부트스트랩", "Bootstrap status" = "부트스트랩 상태",
      "BH family" = "BH 검정군"
    )
    matched <- match(names(table), names(headers))
    replace <- !is.na(matched)
    names(table)[replace] <- unname(headers[matched[replace]])
  }
  table
}

structural_canvas_path_display_table <- function(table, path_label = "Path") {
  if (!is.data.frame(table) || !all(c("Outcome", "Predictor") %in% names(table))) return(table)
  path <- paste(table$Predictor, "→", table$Outcome)
  remaining <- table[, setdiff(names(table), c("Outcome", "Predictor")), drop = FALSE]
  cbind(stats::setNames(data.frame(path, stringsAsFactors = FALSE), path_label), remaining)
}

structural_canvas_localize_inference_text <- function(value, language = NULL) {
  value <- as.character(value %||% "")
  if (!normalize_app_language(language) %in% c("en", "ko")) {
    tr <- function(text) statedu_localized_text(language, text)
    translate <- function(item) {
      # Parse only the program's numeric/status suffix, never arbitrary free text.
      match <- regmatches(item, regexec("^(.*); valid (standardized bootstrap )?([0-9]+/[0-9]+ \\([0-9.]+%\\)); status (Adequate|Caution|Unreliable)$", item))[[1L]]
      if (length(match)) {
        root <- translate(match[[2L]])
        if (!identical(root, match[[2L]])) return(sprintf(tr(if (nzchar(match[[3L]])) "%s; valid standardized bootstrap %s; status %s" else "%s; valid %s; status %s"), root, match[[4L]], tr(match[[5L]])))
        return(item)
      }
      match <- regmatches(item, regexec("^(Bootstrap (bias-corrected and accelerated \\(BCa\\)|bias-corrected \\(BC\\)|BCa|percentile) 95% CI)( \\(R quantile type ([0-9]+)\\))?$", item))[[1L]]
      if (length(match)) {
        root <- if (identical(match[[2L]], "Bootstrap BCa 95% CI")) "Bootstrap bias-corrected and accelerated (BCa) 95% CI" else match[[2L]]
        return(paste0(tr(root), if (nzchar(match[[5L]])) paste0(" ", sprintf(tr("(R quantile type %s)"), match[[5L]])) else ""))
      }
      known <- c("Model-based 95% CI", "Model-based normal-theory", "Bootstrap", "Bootstrap (empirical two-sided p)",
        "Bootstrap requested - inference suppressed", "Bootstrap pending - inference suppressed", "Bootstrap canceled - inference suppressed",
        "Bootstrap failed - inference suppressed", "Bootstrap blocked - original model ineligible", "Bootstrap unavailable - inference suppressed",
        "Not estimated - insufficient valid bootstrap replicates", "Not estimated - insufficient valid standardized bootstrap replicates",
        "Not applicable - fixed parameter", "Fixed parameter - no inferential test", "Fixed effect - no inferential test",
        "Not requested", "Unreliable", "Caution", "Adequate")
      if (item %in% known) tr(item) else item
    }
    return(vapply(value, translate, character(1), USE.NAMES = FALSE))
  }
  if (!identical(normalize_app_language(language), "ko")) return(value)
  vapply(value, function(item) {
    item <- trimws(as.character(item %||% ""))
    if (!nzchar(item)) return("")
    exact <- c(
      "Model-based 95% CI" = "모형기반 95% CI",
      "Model-based normal-theory" = "모형기반 정규이론",
      "Bootstrap" = "부트스트랩",
      "Bootstrap (empirical two-sided p)" = "부트스트랩(경험적 양측 p)",
      "Bootstrap requested - inference suppressed" = "부트스트랩 요청됨 - 추론값 억제",
      "Bootstrap pending - inference suppressed" = "부트스트랩 대기 중 - 추론값 억제",
      "Bootstrap canceled - inference suppressed" = "부트스트랩 취소됨 - 추론값 억제",
      "Bootstrap failed - inference suppressed" = "부트스트랩 실패 - 추론값 억제",
      "Bootstrap blocked - original model ineligible" = "부트스트랩 실행 차단 - 원모형 부적격",
      "Bootstrap unavailable - inference suppressed" = "부트스트랩 결과 없음 - 추론값 억제",
      "Not estimated - insufficient valid bootstrap replicates" = "미산출 - 유효 부트스트랩 반복 부족",
      "Not estimated - insufficient valid standardized bootstrap replicates" = "미산출 - 유효 표준화 부트스트랩 반복 부족",
      "Not applicable - fixed parameter" = "해당 없음 - 고정모수",
      "Fixed parameter - no inferential test" = "고정모수 - 추론검정 없음",
      "Fixed effect - no inferential test" = "고정효과 - 추론검정 없음",
      "Not requested" = "실행하지 않음",
      "Unreliable" = "신뢰 불가",
      "Caution" = "주의",
      "Adequate" = "충분"
    )
    if (item %in% names(exact)) return(unname(exact[[item]]))
    item <- sub("^Bootstrap bias-corrected and accelerated \\(BCa\\) 95% CI", "부트스트랩 편향보정·가속(BCa) 95% CI", item)
    item <- sub("^Bootstrap bias-corrected \\(BC\\) 95% CI", "부트스트랩 편향보정(BC) 95% CI", item)
    item <- sub("^Bootstrap BCa 95% CI", "부트스트랩 편향보정·가속(BCa) 95% CI", item)
    item <- sub("^Bootstrap percentile 95% CI", "부트스트랩 백분위수 95% CI", item)
    item <- sub("^Bootstrap", "부트스트랩", item)
    item <- sub(
      "^Not estimated - insufficient valid standardized bootstrap replicates",
      "미산출 - 유효 표준화 부트스트랩 반복 부족",
      item
    )
    item <- sub(
      "^Not estimated - insufficient valid bootstrap replicates",
      "미산출 - 유효 부트스트랩 반복 부족",
      item
    )
    item <- sub("R quantile type", "R 분위수 유형", item, fixed = TRUE)
    item <- sub("; valid standardized bootstrap ", "; 유효 표준화 부트스트랩 ", item, fixed = TRUE)
    item <- sub("; valid ", "; 유효 ", item, fixed = TRUE)
    item <- sub("; status Adequate", "; 상태 충분", item, fixed = TRUE)
    item <- sub("; status Caution", "; 상태 주의", item, fixed = TRUE)
    item <- sub("; status Unreliable", "; 상태 신뢰 불가", item, fixed = TRUE)
    item
  }, character(1), USE.NAMES = FALSE)
}

structural_canvas_measurement_html_table <- function(table, note = NULL) {
  required <- c("Latent", "Indicator", "B", "SE", "beta", "z", "p", "R²")
  if (!is.data.frame(table) || !all(required %in% names(table))) {
    return(structural_canvas_basic_html_table(table, role = "main", note = note))
  }
  body_values <- table[, required, drop = FALSE]
  structural_canvas_table_sheet(tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-measurement-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", "Latent"),
        tags$th(class = "structural-table-header-cell", "Indicator"),
        tags$th(class = "structural-table-header-cell", "B"),
        tags$th(class = "structural-table-header-cell", "SE"),
        tags$th(class = "structural-table-header-cell", HTML("Std. loading<br>(&lambda;)")),
        tags$th(class = "structural-table-header-cell", "z"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", HTML("R<sup>2</sup>"))
      )
    ),
    tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
      tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
    }))
  )), table = body_values, role = "main", orientation = "portrait", note = note)
}

structural_canvas_measurement_ci_html_table <- function(table, language = statedu_initial_language()) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  column_named <- function(pattern) {
    matches <- names(table)[grepl(pattern, names(table))]
    if (length(matches)) matches[[1L]] else ""
  }
  columns <- c(
    "Latent",
    "Indicator",
    "B 95% CI lower",
    "B 95% CI upper",
    "beta 95% CI lower",
    "beta 95% CI upper",
    column_named("^R.*95% CI lower$"),
    column_named("^R.*95% CI upper$")
  )
  if (any(!nzchar(columns)) || !all(columns %in% names(table))) {
    attr(table, "result_user_columns") <- seq_along(table)
    return(structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-measurement-ci-table", role = "appendix", language = language))
  }
  body_values <- table[, columns, drop = FALSE]
  structural_canvas_table_sheet(tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-measurement-ci-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", rowspan = "2", tr("Latent factor", "잠재요인")),
        tags$th(class = "structural-table-header-cell", rowspan = "2", tr("Indicator", "지표")),
        tags$th(class = "structural-table-header-cell", colspan = "2", "B 95% CI"),
        tags$th(class = "structural-table-header-cell", colspan = "2", tr("Std. loading", "표준화 적재량"), HTML(" (&lambda;)<br>95% CI")),
        tags$th(class = "structural-table-header-cell", colspan = "2", HTML("R<sup>2</sup> 95% CI"))
      ),
      tags$tr(
        tags$th(class = "structural-table-header-cell", tr("lower", "하한")),
        tags$th(class = "structural-table-header-cell", tr("upper", "상한")),
        tags$th(class = "structural-table-header-cell", tr("lower", "하한")),
        tags$th(class = "structural-table-header-cell", tr("upper", "상한")),
        tags$th(class = "structural-table-header-cell", tr("lower", "하한")),
        tags$th(class = "structural-table-header-cell", tr("upper", "상한"))
      )
    ),
    tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
      tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
    }))
  )), table = body_values, role = "appendix", orientation = "auto")
}

structural_canvas_effect_summary_html_table <- function(table, ci = FALSE, language = NULL) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  path_label <- tr("Path", "경로")
  long_required <- c("Outcome", "Predictor", "Effect", "B", "B 95% CI", "beta", "p", "BH-adjusted p", "CI source", "Inference source", "Valid bootstrap", "Bootstrap status", "BH family")
  if (!isTRUE(ci) && all(long_required %in% names(table))) {
    se_column <- intersect(c("Boot SE", "SE"), names(table))
    se_column <- if (length(se_column)) se_column[[1L]] else ""
    columns <- c("Effect", "B", se_column, "B 95% CI", "beta", "p", "BH-adjusted p", "CI source", "Inference source", "Valid bootstrap", "Bootstrap status", "BH family")
    columns <- columns[nzchar(columns) & columns %in% names(table)]
    body_values <- cbind(
      stats::setNames(data.frame(paste(table$Predictor, "→", table$Outcome), stringsAsFactors = FALSE), path_label),
      table[, columns, drop = FALSE]
    )
    body_values <- structural_canvas_localize_reporting_metadata(body_values, language)
    attr(body_values, "result_user_columns") <- seq_along(body_values)
    return(structural_canvas_basic_html_table(body_values, class = "table table-striped table-bordered structural-effect-summary-table", language = language))
  }
  if (isTRUE(ci)) {
    required <- c("Outcome", "Predictor", "Direct beta 95% CI", "Direct CI source", "Indirect beta 95% CI", "Indirect CI source", "Total beta 95% CI", "Total CI source")
    if (!all(required %in% names(table))) return(structural_canvas_basic_html_table(table, language = language))
    ko <- identical(normalize_app_language(language), "ko")
    ci_values <- table[, c("Direct beta 95% CI", "Direct CI source", "Indirect beta 95% CI", "Indirect CI source", "Total beta 95% CI", "Total CI source"), drop = FALSE]
    ci_values <- structural_canvas_localize_reporting_metadata(ci_values, language, rename_headers = FALSE)
    body_values <- cbind(
      stats::setNames(data.frame(paste(table$Predictor, "→", table$Outcome), stringsAsFactors = FALSE), path_label),
      ci_values
    )
    return(structural_canvas_table_sheet(tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-effect-ci-table",
      tags$thead(
        tags$tr(
          tags$th(class = "structural-table-header-cell", rowspan = "2", path_label),
          tags$th(class = "structural-table-header-cell", colspan = "2", tr("Direct effect", "직접효과")),
          tags$th(class = "structural-table-header-cell", colspan = "2", tr("Indirect effect", "간접효과")),
          tags$th(class = "structural-table-header-cell", colspan = "2", tr("Total effect", "총효과"))
        ),
        tags$tr(
          tags$th(class = "structural-table-header-cell", HTML("&beta; 95% CI")), tags$th(class = "structural-table-header-cell", tr("Source", "산출 근거")),
          tags$th(class = "structural-table-header-cell", HTML("&beta; 95% CI")), tags$th(class = "structural-table-header-cell", tr("Source", "산출 근거")),
          tags$th(class = "structural-table-header-cell", HTML("&beta; 95% CI")), tags$th(class = "structural-table-header-cell", tr("Source", "산출 근거"))
        )
      ),
      tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
        tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
      }))
    )), table = body_values, role = "appendix", orientation = "auto"))
  }
  required <- c("Outcome", "Predictor", "Direct beta", "Direct p", "Direct BH-adjusted p", "Indirect beta", "Indirect p", "Indirect BH-adjusted p", "Total beta", "Total p", "Total BH-adjusted p")
  if (!all(required %in% names(table))) return(structural_canvas_basic_html_table(table, language = language))
  body_values <- cbind(
    stats::setNames(data.frame(paste(table$Predictor, "→", table$Outcome), stringsAsFactors = FALSE), path_label),
    table[, required[-c(1L, 2L)], drop = FALSE]
  )
  structural_canvas_table_sheet(tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-effect-summary-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", rowspan = "2", path_label),
        tags$th(class = "structural-table-header-cell", colspan = "3", tr("Direct effect", "직접효과")),
        tags$th(class = "structural-table-header-cell", colspan = "3", tr("Indirect effect", "간접효과")),
        tags$th(class = "structural-table-header-cell", colspan = "3", tr("Total effect", "총효과"))
      ),
      tags$tr(
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", "BH p"),
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", "BH p"),
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", "BH p")
      )
    ),
    tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
      tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
    }))
  )), table = body_values, role = "appendix", orientation = "landscape")
}

structural_canvas_inference_columns <- function(table) {
  intersect(c("B CI source", "beta CI source", "CI source", "Inference source", "Valid bootstrap", "Bootstrap status", "BH family"), names(table))
}

structural_canvas_statistics_only <- function(table) {
  table[, setdiff(names(table), structural_canvas_inference_columns(table)), drop = FALSE]
}

structural_canvas_effect_main_html_table <- function(table, standardized = FALSE, ci = FALSE) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  pairs <- unique(table[, c("Predictor", "Outcome"), drop = FALSE])
  estimate <- if (standardized) "beta" else "B"
  statistic <- if (ci) paste0(estimate, " 95% CI") else if (standardized) "beta p" else "p"
  value <- function(row, effect, column) {
    hit <- which(table$Predictor == pairs$Predictor[[row]] & table$Outcome == pairs$Outcome[[row]] & table$Effect == effect)
    if (!length(hit) || !column %in% names(table)) return("")
    as.character(table[[column]][[hit[[1L]]]])
  }
  body <- data.frame(Path = paste(pairs$Predictor, "→", pairs$Outcome), check.names = FALSE)
  for (effect in c("Direct", "Indirect", "Total")) {
    body[[paste(effect, estimate)]] <- vapply(seq_len(nrow(pairs)), value, character(1), effect = effect, column = estimate)
    body[[paste(effect, if (ci) "95% CI" else "p")]] <- vapply(seq_len(nrow(pairs)), value, character(1), effect = effect, column = statistic)
  }
  if (ci) {
    expanded <- result_ci_expand_columns(body, force=TRUE)
    if (ncol(expanded) == 10L) body <- expanded
  }
  split_ci <- ci && ncol(body) == 10L
  per_effect <- if(split_ci)3L else 2L
  structural_canvas_table_sheet(tags$div(class = "table-responsive", tags$table(
    class = "table structural-result-table structural-effect-main-table",
    tags$colgroup(tags$col(style="width:34%"), lapply(seq_len(per_effect*3L), function(i) tags$col(style=sprintf("width:%s%%",66/(per_effect*3L))))),
    tags$thead(tags$tr(tags$th(rowspan=2,"Path"), lapply(c("Direct effect","Indirect effect","Total effect"), function(label) tags$th(colspan=per_effect,label))),
      tags$tr(lapply(rep(c(if (standardized) "β" else "B", if(split_ci)c("LLCI","ULCI") else if (ci) "95% CI" else "p"),3), tags$th))),
    tags$tbody(lapply(seq_len(nrow(body)),function(i) tags$tr(lapply(as.character(body[i,]),structural_canvas_html_cell)))))),
    table=body,role="main",orientation="portrait",
    note=result_note_paragraph(class="structural-result-note structural-main-note",
      paste0(if (standardized) "β = standardized effect" else "B = unstandardized effect",
        if (ci) "; CI = confidence interval." else "; p values correspond to the reported coefficient scale.",
        " Blank cells indicate effects not defined or inference unavailable; see supplementary inference details.")))
}

structural_canvas_specific_indirect_html_table <- function(table, language = NULL, note = NULL) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  se_column <- intersect(c("Boot SE", "SE"), names(table))
  lower_column <- intersect(c("Boot 95% CI lower", "B 95% CI lower"), names(table))
  upper_column <- intersect(c("Boot 95% CI upper", "B 95% CI upper"), names(table))
  required <- c("Path", "B", "p", "BH-adjusted p")
  if (!all(required %in% names(table)) || !length(se_column) || !length(lower_column) || !length(upper_column)) {
    return(structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-specific-indirect-table", role = "main", note = note))
  }
  se_column <- se_column[[1L]]
  lower_column <- lower_column[[1L]]
  upper_column <- upper_column[[1L]]
  body_values <- data.frame(
    Path = table$Path,
    B = table$B,
    table[[se_column]],
    LLCI = table[[lower_column]],
    ULCI = table[[upper_column]],
    beta = table$beta %||% "",
    z = table$z %||% "",
    p = table$p,
    `BH p` = table[["BH-adjusted p"]],
    `CI source` = table[["CI source"]] %||% "",
    `Inference source` = table[["Inference source"]] %||% "",
    `Valid bootstrap` = table[["Valid bootstrap"]] %||% "",
    `Bootstrap status` = table[["Bootstrap status"]] %||% "",
    `BH family` = table[["BH family"]] %||% "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(body_values)[3L] <- se_column
  body_values <- structural_canvas_statistics_only(body_values)
  structural_canvas_basic_html_table(
    body_values,
    class = "table table-striped table-bordered structural-specific-indirect-table",
    role = "main",
    orientation = "landscape",
    note = note
  )
}

structural_canvas_pls_measurement_main_html_table <- function(table, note = NULL) {
  required <- c("Construct", "Construct type", "Indicator", "loading/weight", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "Boot t", "Boot p", "Boot BH-adjusted p", "Item VIF", "Mode")
  if (!is.data.frame(table) || !nrow(table) || !all(required %in% names(table))) {
    return(structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-pls-measurement-main-table", role = "main", note = note))
  }
  marker <- ifelse(table[["Construct type"]] == "Common factor", "†", ifelse(table[["Construct type"]] == "Composite", "‡", "¶"))
  body <- data.frame(
    Construct = paste0(table$Construct, marker), Indicator = table$Indicator,
    `loading/weight` = table[["loading/weight"]], `Boot SE` = table[["Boot SE"]],
    `Boot 95% CI lower` = table[["Boot 95% CI lower"]], `Boot 95% CI upper` = table[["Boot 95% CI upper"]],
    `Boot t` = table[["Boot t"]], `Boot p` = table[["Boot p"]], `Boot BH-adjusted p` = table[["Boot BH-adjusted p"]], VIF = table[["Item VIF"]],
    check.names = FALSE, stringsAsFactors = FALSE
  )
  body$Construct[duplicated(as.character(table$Construct))] <- ""
  structural_canvas_table_sheet(tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-pls-measurement-main-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Construct"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Indicator"),
        tags$th(class = "structural-table-header-cell structural-loading-weight-header", rowspan = "2", HTML("loading/<br>weight")),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Boot SE"),
        tags$th(class = "structural-table-header-cell", colspan = "2", "Boot 95% CI"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Boot t"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Boot p"),
        tags$th(class = "structural-table-header-cell structural-boot-bh-header", rowspan = "2", HTML("Boot BH<br>adj p")),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "VIF")
      ),
      tags$tr(tags$th(class = "structural-table-header-cell", "Lower"), tags$th(class = "structural-table-header-cell", "Upper"))
    ),
    tags$tbody(lapply(seq_len(nrow(body)), function(index) tags$tr(lapply(as.character(body[index, ]), structural_canvas_html_cell))))
  )), table = body, role = "main", orientation = "landscape", note = note)
}

structural_canvas_effect_ci_source_note <- function(table, language = NULL) {
  source_columns <- c("Direct CI source", "Indirect CI source", "Total CI source")
  if (!is.data.frame(table) || !all(source_columns %in% names(table))) return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  labels <- c("Direct CI source" = tr("Direct effect", "직접효과"), "Indirect CI source" = tr("Indirect effect", "간접효과"), "Total CI source" = tr("Total effect", "총효과"))
  parts <- unlist(lapply(source_columns, function(column) {
    values <- unique(trimws(as.character(table[[column]] %||% "")))
    values <- values[nzchar(values)]
    if (!length(values)) return(character(0))
    values <- structural_canvas_localize_inference_text(values, language)
    paste0(labels[[column]], ": ", paste(values, collapse = "; "))
  }), use.names = FALSE)
  if (!length(parts)) return(NULL)
  result_note_paragraph(class = "structural-result-note structural-effect-ci-source-note", sprintf(tr("Confidence-interval sources: %s.", "신뢰구간 산출 근거: %s."), paste(parts, collapse = "; ")))
}

structural_canvas_abbreviation_footnotes <- function(table, context = "general") {
  if (!is.data.frame(table) || !ncol(table)) return(NULL)
  keys <- tolower(gsub("[^[:alnum:]αβλχω²]", "", names(table)))
  keys <- gsub("²", "2", keys, fixed = TRUE)
  has <- function(pattern) any(grepl(pattern, keys, perl = TRUE))
  definitions <- switch(context,
    fit = c(
      if (has("llci")) "LLCI = lower confidence limit",
      if (has("ulci")) "ULCI = upper confidence limit",
      if (has("chisq|chisquare|χ2")) "χ² = chi-square",
      if (has("df")) "df = degrees of freedom",
      if (has("cfi")) "CFI = Comparative Fit Index",
      if (has("tli")) "TLI = Tucker-Lewis Index",
      if (has("srmr")) "SRMR = Standardized Root Mean Square Residual",
      if (has("rmsea")) "RMSEA = Root Mean Square Error of Approximation",
      if (has("ci|llci|ulci")) "CI = confidence interval"),
    validity = c(
      if (has("ave")) "AVE = average variance extracted",
      if (has("^cr$|compositereliability")) "CR = composite reliability",
      if (has("alpha|α")) "α = Cronbach's alpha",
      if (has("omega|ω")) "ω = McDonald's omega total"),
    measurement = c(
      if (has("^b$")) "B = unstandardized loading",
      if (has("se$")) "SE = standard error",
      if (has("lambda|λ|standardizedloading|stdloading|^beta$")) "λ = standardized loading",
      if (has("r2")) "R² = coefficient of determination",
      if (has("^z$")) "z = test statistic",
      if (has("^p$")) "p = two-sided p value"),
    "pls-path" = c(
      if (has("beta|β")) "β = standardized path coefficient",
      if (has("boot")) "Boot = bootstrap",
      if (has("se$")) "SE = standard error",
      if (has("bh")) "BH = Benjamini-Hochberg",
      if (has("f2")) "f² = local effect size",
      if (has("r2")) "R² = explained variance",
      if (has("adj")) "adj R² = adjusted explained variance",
      if (has("vif")) "VIF = variance inflation factor"),
    character(0))
  if (!length(definitions)) return(NULL)
  result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-1",
    result_publication_note(paste(definitions, collapse = "; ")))
}

structural_canvas_symbol_footnotes <- function(table) {
  values <- as.character(unlist(table, use.names = FALSE))
  values <- values[!is.na(values)]
  notes <- list()
  if (any(grepl("\\*", values))) {
    notes <- c(notes, list(result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-symbol", "* Fixed reference loading.")))
  }
  if (any(grepl("\u2020", values, fixed = TRUE))) {
    notes <- c(notes, list(result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-symbol", "\u2020 Coefficient is outside the conventional admissible range; interpret cautiously.")))
  }
  if (any(grepl("\u2021", values, fixed = TRUE))) {
    notes <- c(notes, list(result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-symbol", "\u2021 Single-indicator construct without a constrained error variance; reliability and AVE are not estimated.")))
  }
  if (any(grepl("\u00b6", values, fixed = TRUE))) {
    notes <- c(notes, list(result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-symbol", "\u00b6 Single-indicator construct with constrained error variance; interpret reliability and validity descriptively.")))
  }
  tagList(notes)
}
