structural_canvas_micom_pair_display <- function(value, language) {
  if (!is.data.frame(value) || !nrow(value)) return(value)
  labels <- c(
    "Every construct passed compositional invariance after the global Holm MICOM adjustment." = "모든 구성개념이 전체 Holm MICOM 보정 후 합성불변성을 통과했습니다.",
    "One or more constructs failed compositional invariance after the global Holm MICOM adjustment." = "하나 이상의 구성개념이 전체 Holm MICOM 보정 후 합성불변성을 통과하지 못했습니다.",
    "Pairwise permutation-validity gate failed." = "집단쌍 순열 유효성 기준을 통과하지 못했습니다.",
    "At least one group has N < 30; review stability" = "하나 이상의 집단이 N < 30이므로 안정성을 검토하십시오.",
    "Pair blocked because the MICOM and MGA group sets do not match." = "MICOM과 MGA의 집단 구성이 일치하지 않아 집단쌍 비교가 차단되었습니다.",
    "Pair blocked because duplicate MICOM pair-gate records were found." = "MICOM 집단쌍 허용 기록이 중복되어 비교가 차단되었습니다.",
    "Pair blocked because its MICOM pair-gate record is missing." = "MICOM 집단쌍 허용 기록이 없어 비교가 차단되었습니다.",
    "Pair blocked because the MICOM pair-gate decision column is missing." = "MICOM 집단쌍 허용 여부 열이 없어 비교가 차단되었습니다.",
    "Pair admitted by the recorded MICOM composite-invariance gate." = "기록된 MICOM 합성불변성 기준에 따라 집단쌍 비교가 허용되었습니다.",
    "Pair blocked because a passing MICOM composite-invariance gate was not recorded." = "MICOM 합성불변성 기준 통과 기록이 없어 집단쌍 비교가 차단되었습니다.",
    "Pairwise inference is blocked because MICOM composite-score invariance was not evaluated." = "MICOM 합성점수 불변성을 평가하지 않아 집단쌍 추론이 차단되었습니다.",
    "MICOM pair gate was unavailable." = "MICOM 집단쌍 허용 여부를 확인할 수 없습니다.",
    "MICOM pair-gate helpers were not loaded; pairwise inference is blocked." = "MICOM 집단쌍 허용 확인 기능을 불러오지 못해 집단쌍 추론이 차단되었습니다.",
    "MICOM was not evaluated." = "MICOM을 평가하지 않았습니다.",
    Adequate = "충분", Insufficient = "불충분", `Blocked by MICOM` = "MICOM으로 차단", None = "없음"
  )
  columns <- intersect(c("Reason", "MICOM reason", "Small-N warning", "Status"), names(value))
  for (column in columns) value[[column]] <- vapply(as.character(value[[column]]), function(text) {
    if (is.na(text) || !text %in% names(labels)) return(text)
    statedu_localized_text(language, text, unname(labels[[text]]))
  }, character(1), USE.NAMES = FALSE)
  attr(value, "result_user_columns") <- unique(c(attr(value, "result_user_columns", exact = TRUE), match(columns, names(value))))
  value
}

structural_canvas_group_diagnostics_display <- function(value, language) {
  if (!is.data.frame(value) || !nrow(value)) return(value)
  labels <- c(
    "Ordered category absent" = "순서형 범주 누락",
    "Very small group (N < 30); invariance estimates may be unstable" = "매우 작은 집단(N < 30): 불변성 추정이 불안정할 수 있습니다.",
    "Severely unbalanced smallest group; review power/stability" = "집단 크기가 크게 불균형합니다. 최소 집단의 검정력과 안정성을 검토하십시오.",
    "Small group; review power/stability" = "작은 집단: 검정력과 안정성을 검토하십시오.",
    "No group-level flag" = "집단별 경고 없음",
    "Small group (N < 30); permutation estimates may be unstable" = "작은 집단(N < 30): 순열 추정이 불안정할 수 있습니다.",
    "No group-size flag" = "집단 크기 경고 없음",
    None = "없음"
  )
  columns <- intersect(c("Status", "N warning", "Absent ordered categories"), names(value))
  for (column in columns) value[[column]] <- vapply(as.character(value[[column]]), function(text) {
    if (is.na(text)) return(text)
    # Category listings contain authored indicator/category labels, not prose.
    if (identical(column, "Absent ordered categories") && text != "None") return(text)
    if (!text %in% names(labels)) return(text)
    statedu_localized_text(language, text, unname(labels[[text]]))
  }, character(1), USE.NAMES = FALSE)
  attr(value, "result_user_columns") <- unique(c(attr(value, "result_user_columns", exact = TRUE), match(columns, names(value))))
  value
}

structural_canvas_micom_audit_text <- function(value, language) {
  labels <- c(
    "Identical indicators and model specification" = "동일한 지표와 모형 명세",
    "Identical missing-data and standardization treatment" = "동일한 결측자료 및 표준화 처리",
    "Identical PLS algorithm and settings" = "동일한 PLS 알고리즘 및 설정",
    "Every pair uses seminr::mean_replacement in group and pooled fits; the pair-pooled mean and SD define the common Step 2/3 score scale." = "모든 집단쌍은 집단별 및 통합 적합에서 seminr::mean_replacement를 사용합니다. 집단쌍 통합 평균과 SD가 2·3단계의 공통 점수 척도를 정의합니다.",
    "Every fit uses run_structural_canvas_analysis(..., analysis_type='plssem', estimator='PLS') with the shared production settings." = "모든 적합은 공통 실행 설정과 함께 run_structural_canvas_analysis(..., analysis_type='plssem', estimator='PLS')를 사용합니다."
  )
  vapply(as.character(value), function(text) {
    if (is.na(text)) return(text)
    if (text %in% names(labels)) return(statedu_localized_text(language, text, unname(labels[[text]])))
    matched <- regmatches(text, regexec("^One shared canvas snapshot supplies ([0-9]+) constructs and ([0-9]+) indicators to every group fit\\.$", text))[[1L]]
    if (length(matched)) return(sprintf(statedu_localized_text(language,
      "One shared canvas snapshot supplies %s constructs and %s indicators to every group fit.",
      "하나의 공통 캔버스 스냅샷이 모든 집단 적합에 구성개념 %s개와 지표 %s개를 제공합니다."), matched[2], matched[3]))
    text
  }, character(1), USE.NAMES = FALSE)
}

structural_canvas_effect_bootstrap_blocked_text <- function(reason, language) {
  labels <- c(
    "Structural-effect bootstrap was not started because the original lavaan fit is unavailable." = "원래 lavaan 적합 결과를 사용할 수 없어 구조효과 부트스트랩을 시작하지 않았습니다.",
    "Structural-effect bootstrap was not started because the original lavaan model did not converge." = "원래 lavaan 모형이 수렴하지 않아 구조효과 부트스트랩을 시작하지 않았습니다.",
    "Structural-effect bootstrap was not started because the original lavaan solution is inadmissible." = "원래 lavaan 모형의 해가 허용 가능하지 않아 구조효과 부트스트랩을 시작하지 않았습니다."
  )
  for (prefix in names(labels)) {
    if (identical(reason, prefix)) return(statedu_localized_text(language, prefix, unname(labels[[prefix]])))
    if (startsWith(reason, paste0(prefix, " Reasons: "))) {
      return(paste0(statedu_localized_text(language, prefix, unname(labels[[prefix]])), " ",
        sprintf(statedu_localized_text(language, "Reasons: %s", "사유: %s"), substring(reason, nchar(prefix) + nchar(" Reasons: ") + 1L))))
    }
  }
  reason
}

structural_canvas_parcel_text <- function(value, language) {
  labels <- c(
    "Parcel planning was not requested." = "Parcel 계획을 요청하지 않았습니다.",
    "Parcel preview currently supports three or four parcels." = "현재 미리보기는 3개 또는 4개 Parcel을 지원합니다.",
    "The selected construct is not present in the model." = "선택한 구성개념이 모형에 없습니다.",
    "Parceling preview is restricted to reflective common-factor constructs." = "Parcel 미리보기는 반영형 공통요인 구성개념에서만 사용할 수 있습니다.",
    "An admissible item-level CFA must be fitted before parcel planning." = "Parcel 계획 전에 문항수준 CFA에서 허용 가능한 해를 얻어야 합니다.",
    "Parcel preview currently requires a single-group item-level CFA." = "현재 Parcel 미리보기에는 단일집단 문항수준 CFA가 필요합니다.",
    "Parcel preview currently requires continuous indicators; ordinal items need an ordinal-specific scoring and sensitivity plan." = "현재 Parcel 미리보기에는 연속형 지표가 필요합니다. 순서형 문항에는 별도의 점수화 및 민감도 검토 계획이 필요합니다.",
    "Item-level parcel-factor model requested" = "문항수준 Parcel 요인 모형 요청됨",
    "Item-level parcel-factor model could not be fitted" = "문항수준 Parcel 요인 모형 적합 불가",
    "Item-level parcel-factor CFA fitted" = "문항수준 Parcel 요인 CFA 적합 완료",
    "Loading-balanced allocation is sample-dependent and can conceal multidimensionality or local dependence." = "적재량 균형 배정은 표본에 의존하며 다차원성이나 국소의존을 가릴 수 있습니다.",
    "No parcel variables were created; the requested parcels are represented as lower-order item-level factors." = "Parcel 변수는 생성하지 않았습니다. 요청한 Parcel은 문항수준 하위요인으로 표현됩니다.",
    "A substantive parceling purpose was not recorded." = "이론적 Parcel 구성 목적이 기록되지 않았습니다."
  )
  if (length(value) != 1L || is.na(value)) return(value)
  if (value %in% names(labels)) return(statedu_localized_text(language, value, unname(labels[[value]])))
  matched <- regmatches(value, regexec("^At least ([0-9]+) indicators are required to preview ([0-9]+) parcels with at least two items each\\.$", value))[[1L]]
  if (length(matched)) return(sprintf(statedu_localized_text(language,
    "At least %s indicators are required to preview %s parcels with at least two items each.",
    "각 Parcel에 문항을 최소 2개씩 배정하는 미리보기에는 지표 %s개 이상이 필요합니다(Parcel %s개)."), matched[2], matched[3]))
  value
}

structural_canvas_parcel_warning <- function(value, language) {
  # Only translate known leading sentences; preserve appended fit errors verbatim.
  remaining <- as.character(value %||% "")
  translated <- ""
  sentences <- c(
    "Loading-balanced allocation is sample-dependent and can conceal multidimensionality or local dependence.",
    "No parcel variables were created; the requested parcels are represented as lower-order item-level factors.",
    "A substantive parceling purpose was not recorded."
  )
  for (sentence in sentences) {
    if (!startsWith(remaining, sentence)) next
    translated <- paste0(translated, structural_canvas_parcel_text(sentence, language))
    remaining <- substring(remaining, nchar(sentence) + 1L)
    spaces <- regmatches(remaining, regexpr("^ *", remaining))
    translated <- paste0(translated, spaces)
    remaining <- substring(remaining, nchar(spaces) + 1L)
  }
  paste0(translated, remaining)
}

structural_canvas_redundancy_text <- function(value, language) {
  labels <- c(
    "Redundancy analysis was not requested." = "중복성 분석을 요청하지 않았습니다.",
    "no global criterion was selected." = "전역 기준변수를 선택하지 않았습니다.",
    "A formative construct and a separate global criterion were not both selected." = "형성형 구성개념과 별도의 전역 기준변수가 모두 선택되지 않았습니다.",
    "Redundancy analysis requires a formative composite." = "중복성 분석에는 형성형 합성변수가 필요합니다.",
    "The global criterion must be separate from the formative indicators." = "전역 기준변수는 형성지표와 별개여야 합니다.",
    "The selected construct score is unavailable." = "선택한 구성개념의 점수를 사용할 수 없습니다.",
    "The global criterion must be a numeric variable available to the PLS model." = "전역 기준변수는 PLS 모형에서 사용할 수 있는 숫자형 변수여야 합니다.",
    "Construct-score and criterion row counts do not match after PLS preprocessing." = "PLS 전처리 후 구성개념 점수와 기준변수의 행 수가 일치하지 않습니다.",
    "At least four complete construct-score/criterion pairs are required." = "구성개념 점수와 기준변수의 완전한 쌍이 최소 4개 필요합니다.",
    "The redundancy relationship could not be estimated because one variable has no variance." = "한 변수에 분산이 없어 중복성 관계를 추정할 수 없습니다.",
    "At/above the descriptive .70 redundancy reference; also verify criterion content validity and independence." = "기술적 중복성 참고값 .70 이상입니다. 기준변수의 내용타당성과 독립성도 확인하십시오.",
    "Redundancy evidence is below the common descriptive .70 loading reference; review criterion quality and construct specification." = "중복성 근거가 일반적인 기술적 적재량 참고값 .70 미만입니다. 기준변수의 품질과 구성개념 명세를 검토하십시오."
  )
  vapply(as.character(value), function(text) {
    if (is.na(text) || !text %in% names(labels)) return(text)
    statedu_localized_text(language, text, unname(labels[[text]]))
  }, character(1), USE.NAMES = FALSE)
}

structural_canvas_higher_order_guidance_text <- function(value, language) {
  labels <- c(
    "Review residual/R² interval" = "잔차/R2 구간 점검",
    "Not assessed" = "평가 안 됨",
    "Weak loading review" = "약한 적재량 점검",
    "At/above descriptive .40 reference" = "기술적 .40 참고값 이상",
    "Review inadmissible coefficient" = "허용 범위 밖 계수 점검",
    "Below common .70 guideline" = "일반적 .70 기준 미만",
    "At/above descriptive .70 reference" = "기술적 .70 참고값 이상"
  )
  if (!value %in% names(labels)) return(value)
  statedu_localized_text(language, value, unname(labels[[value]]))
}

structural_canvas_omega_h_reason_text <- function(reason, language) {
  labels <- c(
    "No higher-order loading paths are specified." = "고차요인 적재 경로가 지정되지 않았습니다.",
    "Omega-h requires exactly one higher-order general factor." = "omega-h는 정확히 하나의 고차 일반요인이 필요합니다.",
    "Omega-h is not reported when a lower-order factor has multiple higher-order loadings." = "하나의 저차요인이 여러 고차요인에 적재되면 omega-h를 보고하지 않습니다.",
    "No observed indicators were found under the lower-order factors." = "저차요인 아래에서 관측 지표를 찾지 못했습니다.",
    "Omega-h is not reported with cross-loaded observed indicators." = "교차적재 관측 지표가 있으면 omega-h를 보고하지 않습니다.",
    "The model-implied indicator covariance matrix is unavailable." = "모형-함의 지표 공분산행렬을 사용할 수 없습니다.",
    "Omega-h denominator is not positive and finite." = "omega-h 분모가 양의 유한값이 아닙니다."
  )
  if (length(reason) != 1L || is.na(reason) || !reason %in% names(labels)) return(reason)
  statedu_localized_text(language, reason, unname(labels[[reason]]))
}

structural_canvas_register_local_fit_outputs <- function(output, prefix, fit_result, app_language_fn = NULL,
                                                         variable_table_fn = function() NULL,
                                                         labels_fn = function() character(0),
                                                         table_number_fn = NULL) {
  display_name_for <- function(bundle) structural_canvas_display_name_resolver(
    snapshot = bundle$snapshot %||% list(),
    variable_table = if (is.function(variable_table_fn)) variable_table_fn() else variable_table_fn,
    labels = if (is.function(labels_fn)) labels_fn() %||% character(0) else labels_fn %||% character(0),
    moderation_definitions = bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list(),
    language = statedu_current_language(app_language_fn)
  )
output[[paste0(prefix, "_result_residuals")]] <- renderUI({
  bundle <- fit_result()
  language <- statedu_current_language(app_language_fn)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  diagnostics <- structural_canvas_display_residual_diagnostics(
    structural_canvas_residual_diagnostics(bundle$fit),
    display_name_for(bundle)
  )
  if (!isTRUE(diagnostics$available)) return(NULL)
  matrix_table <- function(matrix_value, title) {
    values <- matrix("", nrow(matrix_value), ncol(matrix_value) + 1L)
    colnames(values) <- c(tr("Indicator", "지표"), colnames(matrix_value))
    for (row_index in seq_len(nrow(matrix_value))) {
      values[row_index, 1L] <- rownames(matrix_value)[[row_index]]
      for (column_index in seq_len(ncol(matrix_value))) {
        value <- matrix_value[row_index, column_index]
        if (is.finite(value)) values[row_index, column_index + 1L] <- format_decimal3(value)
      }
    }
    values <- as.data.frame(values, check.names = FALSE)
    attr(values, "result_user_columns") <- seq_len(ncol(values))
    attr(values, "result_user_headers") <- seq.int(2L, ncol(values))
    tagList(
      tags$h5(title),
      structural_canvas_basic_html_table(values, language = language, class = "table table-striped table-bordered structural-residual-matrix",
        orientation = if (ncol(matrix_value) <= 9L) "portrait" else "landscape")
    )
  }
  largest <- diagnostics$largest
  if (nrow(largest)) {
    largest[["Standardized residual"]] <- vapply(largest[["Standardized residual"]], format_decimal3, character(1))
    largest[["Correlation residual"]] <- vapply(largest[["Correlation residual"]], format_decimal3, character(1))
    if ("Screening p" %in% names(largest)) largest[["Screening p"]] <- vapply(largest[["Screening p"]], format_p, character(1))
    if ("BH-adjusted screening p" %in% names(largest)) largest[["BH-adjusted screening p"]] <- vapply(largest[["BH-adjusted screening p"]], format_p, character(1))
    {
      largest_names <- c(
        "Indicator1" = "지표 1", "Indicator2" = "지표 2", "Group" = "집단",
        "Residual scale" = "잔차 척도", "Exceeds descriptive cutoff" = "기술적 절단값 초과",
        "Standardized residual" = "표준화 잔차",
        "Correlation residual" = "상관잔차",
        "Screening p" = "선별 p",
        "BH-adjusted screening p" = "BH 보정 선별 p"
      )
      if ("Residual scale" %in% names(largest)) largest[["Residual scale"]] <- vapply(largest[["Residual scale"]], function(x) if (x == "Standardized") tr("Standardized", "표준화") else if (x == "Correlation residual fallback") tr("Correlation residual fallback", "상관잔차 대체") else x, character(1))
      if ("Exceeds descriptive cutoff" %in% names(largest)) largest[["Exceeds descriptive cutoff"]] <- ifelse(largest[["Exceeds descriptive cutoff"]], tr("Yes", "예"), tr("No", "아니요"))
      names(largest) <- vapply(names(largest), function(x) if (x %in% names(largest_names)) tr(x, unname(largest_names[[x]])) else x, character(1), USE.NAMES = FALSE)
      attr(largest, "result_user_columns") <- seq_len(ncol(largest))
    }
  }
  number <- if (is.function(table_number_fn)) table_number_fn("localfit") else NA_character_
  section_title <- tr("Local fit diagnostics", "국소 적합도 진단")
  if (is.character(number) && length(number) && !is.na(number) && nzchar(number)) {
    section_title <- sprintf(tr("Table %s. %s", "표 %s. %s"), number, section_title)
  }
  div(class = "result-section regression-result-panel structural-residual-result",
    h4(section_title),
    matrix_table(diagnostics$standardized, tr("Standardized residual matrix", "표준화 잔차행렬")),
    matrix_table(diagnostics$correlation, tr("Correlation residual matrix", "상관잔차 행렬")),
    tags$h5(sprintf(tr("Large standardized residuals (descriptive |z| >= %s)", "큰 표준화 잔차(기술적 |z| >= %s)"), diagnostics$cutoff)),
    if (!isTRUE(diagnostics$standardized_available)) result_note_paragraph(class = "structural-result-note", tr("Standardized residuals were unavailable. Correlation residuals are displayed without z cutoffs or screening p values because they are not on the same reference scale.", "표준화 잔차를 사용할 수 없습니다. 상관잔차는 같은 기준척도가 아니므로 z 절단값이나 선별 p값 없이 제시합니다.")),
    if (isTRUE(diagnostics$standardized_available) && !nrow(largest)) tags$p(tr("No standardized residuals exceeded the descriptive cutoff.", "기술적 절단값을 넘는 표준화 잔차가 없습니다.")),
    if (nrow(largest)) structural_canvas_basic_html_table(largest, language = language),
    result_note_paragraph(class = "structural-result-note", tr("The |2.58| marker and BH-adjusted two-sided normal-reference screening p values are descriptive localization aids across unique indicator pairs. Residuals are dependent and their reference distribution can vary by estimator; neither an exceedance nor a small adjusted p value is an automatic modification instruction, and no flagged residuals does not establish local fit.", "|2.58| 표시와 BH 보정 양측 정규참조 선별 p값은 고유 지표쌍에서 부적합 위치를 찾기 위한 기술적 보조정보입니다. 잔차는 서로 의존하고 추정량에 따라 참조분포가 달라질 수 있으므로, 절단값 초과나 작은 보정 p값은 자동 수정 지시가 아니며 표시된 잔차가 없다고 국소 적합도가 입증되는 것도 아닙니다."))
  )
})
output[[paste0(prefix, "_result_higher_order")]] <- renderUI({
  bundle <- fit_result()
  language <- statedu_current_language(app_language_fn)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  higher <- structural_canvas_higher_order_results(bundle$snapshot %||% list(), bundle$fit)
  if (!isTRUE(higher$available)) return(NULL)
  table <- higher$table
  display_name <- display_name_for(bundle)
  fixed <- !is.na(table$SE) & table$SE == 0 & is.na(table$z) & is.na(table$p)
  residual_abnormal <- !is.finite(table$ResidualVariance) | table$ResidualVariance < 0 | table$ResidualVariance > 1
  residual_display <- paste0(vapply(table$ResidualVariance, format_decimal3, character(1)), ifelse(residual_abnormal, "†", ""))
  r2_interval_abnormal <- !is.finite(table$R2CILower) | !is.finite(table$R2CIUpper) | table$R2CILower < 0 | table$R2CIUpper > 1
  loading_guidance <- vapply(table$Beta, structural_canvas_higher_order_loading_guidance, character(1))
  display <- data.frame(
    `Higher-order factor` = display_name(table$HigherOrderFactor),
    `Lower-order factor` = display_name(table$LowerOrderFactor),
    B = vapply(table$B, format_decimal3, character(1)),
    `B 95% CI lower` = vapply(table$BCILower, format_decimal3, character(1)),
    `B 95% CI upper` = vapply(table$BCIUpper, format_decimal3, character(1)),
    SE = vapply(table$SE, format_decimal3, character(1)),
    Beta = vapply(table$Beta, format_decimal3, character(1)),
    `β 95% CI lower` = vapply(table$BetaCILower, format_decimal3, character(1)),
    `β 95% CI upper` = vapply(table$BetaCIUpper, format_decimal3, character(1)),
    `R²` = vapply(table$R2, format_decimal3, character(1)),
    `R² 95% CI lower` = paste0(vapply(table$R2CILower, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
    `R² 95% CI upper` = paste0(vapply(table$R2CIUpper, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
    `Residual variance` = residual_display,
    Guidance = ifelse(residual_abnormal | r2_interval_abnormal, "Review residual/R² interval", loading_guidance),
    z = vapply(table$z, format_decimal3, character(1)),
    p = vapply(table$p, format_p, character(1)),
    check.names = FALSE
  )
  display$SE[fixed] <- "—"
  display$z[fixed] <- "—"
  display$p[fixed] <- "—"
  display$Guidance <- vapply(display$Guidance, structural_canvas_higher_order_guidance_text, character(1), language = language)
  ko_headers <- c(
      "고차요인", "저차요인", "B", "B 95% CI 하한", "B 95% CI 상한", "SE", "Beta",
      "Beta 95% CI 하한", "Beta 95% CI 상한", "R2", "R2 95% CI 하한", "R2 95% CI 상한",
      "잔차분산", "해석", "z", "p"
  )
  names(display) <- mapply(function(en, ko) {
    if (startsWith(en, "β ") && !identical(language, "ko")) paste0("β ", tr(substring(en, 3L), ko)) else tr(en, ko)
  }, names(display), ko_headers, USE.NAMES = FALSE)
  attr(display, "result_user_columns") <- seq_len(ncol(display))
  omega_h <- structural_canvas_omega_h(bundle$snapshot %||% list(), bundle$fit)
  omega_h_guidance <- if (isTRUE(omega_h$available)) structural_canvas_omega_h_guidance(omega_h$omega_h) else ""
  omega_h_guidance <- structural_canvas_higher_order_guidance_text(omega_h_guidance, language)
  omega_h_reason <- structural_canvas_omega_h_reason_text(omega_h$reason, statedu_current_language(app_language_fn))
  omega_h_table <- if (isTRUE(omega_h$available)) data.frame(
    stats::setNames(list(display_name(omega_h$higher_order_factor)), tr("Higher-order factor", "고차요인")),
    stats::setNames(list(omega_h$indicators), tr("Indicators", "지표 수")),
    stats::setNames(list(paste0(format_decimal3(omega_h$omega_h), if (!is.finite(omega_h$omega_h) || omega_h$omega_h < 0 || omega_h$omega_h > 1) "†" else "")), tr("Hierarchical omega (ωh)", "위계적 omega (omega-h)")),
    stats::setNames(list(omega_h_guidance), tr("Guidance", "해석")),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ) else data.frame()
  attr(omega_h_table, "result_user_columns") <- seq_len(ncol(omega_h_table))
  div(class = "result-section regression-result-panel structural-appendix-result-panel structural-higher-order-result",
    h4(tr("Higher-order CFA results", "고차요인 CFA 결과")),
    structural_canvas_basic_html_table(display, language = language),
    if (nrow(omega_h_table)) structural_canvas_basic_html_table(omega_h_table, language = language, role = "appendix", orientation = "portrait") else result_note_paragraph(class = "structural-result-note", sprintf(statedu_localized_text(statedu_current_language(app_language_fn), "Hierarchical omega was not reported: %s", "위계적 omega를 보고하지 않았습니다: %s"), omega_h_reason)),
    result_note_paragraph(class = "structural-result-note", tr("Lower-order R² is the variance explained by the higher-order factor. Residual variance is reported on the standardized latent-variable scale.", "저차요인 R2는 고차요인이 설명하는 분산입니다. 잔차분산은 표준화된 잠재변수 척도로 보고됩니다.")),
    result_note_paragraph(class = "structural-result-note", tr("Lower-order R² intervals complement the standardized residual-variance intervals. † also marks an R² interval extending beyond [0, 1].", "저차요인 R2 구간은 표준화 잔차분산 구간의 보수(complement)입니다. † 표시는 R2 구간이 [0, 1] 범위를 벗어난 경우도 나타냅니다.")),
    result_note_paragraph(class = "structural-result-note", tr("Higher-order standardized-loading confidence intervals are 95% delta-method intervals from lavaan.", "고차요인 표준화 적재량 신뢰구간은 lavaan의 95% delta-method 구간입니다.")),
    result_note_paragraph(class = "structural-result-note", tr("B confidence intervals are 95% intervals for unstandardized higher-order loadings; a fixed reference loading has a degenerate interval at its fixed value.", "B 신뢰구간은 비표준화 고차요인 적재량의 95% 구간입니다. 고정된 기준 적재량은 고정값에서 퇴화된 구간을 갖습니다.")),
    result_note_paragraph(class = "structural-result-note", tr("ωh estimates the proportion of unit-weighted total-score variance attributable to one higher-order general factor under the fitted higher-order CFA model.", "omega-h는 적합된 고차요인 CFA 모형에서 단위가중 총점 분산 중 하나의 고차 일반요인에 귀속되는 비율을 추정합니다.")),
    result_note_paragraph(class = "structural-result-note", tr("In a higher-order model, the general factor affects indicators indirectly through lower-order factors. This is not equivalent to a bifactor model, where general and specific factors load directly on indicators; fitting a higher-order factor alone does not establish unidimensionality, superiority of a general score, or a bifactor structure.", "고차요인 모형에서는 일반요인이 문항에 직접 적재하지 않고 저차요인을 통해 간접적으로 영향을 줍니다. 이는 일반요인과 집단요인이 문항에 직접 적재하는 bifactor 모형과 동일하지 않으며, 고차요인 적합만으로 단일차원성·일반점수 우월성 또는 bifactor 구조를 입증하지 않습니다.")),
    result_note_paragraph(class = "structural-result-note", tr("ωh is conditional on the specified unit-weighted total score and fitted higher-order model. Justifying score use also requires content validity, plausible alternative models, factor-score determinacy, and independent replication.", "ωh는 지정된 단위가중 총점과 적합된 고차요인 모형에 조건부인 기술량입니다. 척도 점수 사용을 정당화하려면 내용타당도, 대안모형, 요인점수 결정성 및 독립표본 재현성을 함께 검토하십시오.")),
    result_note_paragraph(class = "structural-result-note", tr("The .40 loading and .70 ωh values are descriptive review guidelines, not universal pass/fail rules. † marks an unavailable value or a coefficient/residual variance outside [0, 1].", ".40 적재량과 .70 omega-h 값은 기술적 검토 기준이며, 보편적 통과/탈락 규칙이 아닙니다. † 표시는 사용할 수 없는 값 또는 [0, 1] 범위를 벗어난 계수/잔차분산을 나타냅니다.")),
    result_note_paragraph(class = "structural-result-note", tr("Fixed reference loadings have no estimated SE, z, or p value.", "고정된 기준 적재량은 SE, z, p를 추정하지 않습니다."))
  )
})
  invisible(TRUE)
}
