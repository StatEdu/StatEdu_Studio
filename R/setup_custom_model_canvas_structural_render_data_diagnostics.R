# Structural data diagnostic result rendering.

structural_canvas_normality_result_ui <- function(bundle, dataset, analysis_type, language) {
  if (analysis_type == "plssem" || length(bundle$ordered %||% character(0))) return(NULL)
  tr <- function(text, values = list()) {
    key <- paste0("analysis.ui.", gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(text))))
    translated <- statedu_t(key, language, text)
    for (name in names(values)) translated <- gsub(paste0("{", name, "}"), as.character(values[[name]]), translated, fixed = TRUE)
    translated
  }
  indicators <- lavaan::lavNames(bundle$fit, "ov")
  diagnosis <- bundle$normality_diagnostics %||% structural_canvas_mardia(dataset, indicators)
  if (!isTRUE(diagnosis$available)) {
    reason <- tr(diagnosis$reason)
    return(div(class = "result-section regression-result-panel structural-normality-result",
      h4(tr("Multivariate normality")),
      result_note_paragraph(class = "structural-result-note", reason)
    ))
  }
  table <- data.frame(
    Test = c("Mardia skewness", "Mardia kurtosis"),
    Estimate = c(format_decimal3(diagnosis$skewness), format_decimal3(diagnosis$kurtosis)),
    Statistic = c(format_decimal3(diagnosis$skew_statistic), format_decimal3(diagnosis$kurtosis_z)),
    df = c(format_decimal3(diagnosis$skew_df), "—"),
    p = c(format_p(diagnosis$skew_p), format_p(diagnosis$kurtosis_p)),
    check.names = FALSE
  )
  recommendation_label <- tr(diagnosis$recommendation)
  div(class = "result-section regression-result-panel structural-normality-result",
    h4(tr("Multivariate normality and estimator guidance")),
    structural_canvas_basic_html_table(table, role = "appendix", orientation = "portrait", language = language),
    result_note_paragraph(class = "structural-result-note", tr("Guidance: {recommendation}. This recommendation is diagnostic and does not automatically change the estimator.", list(recommendation = recommendation_label))),
    result_note_paragraph(class = "structural-result-note", tr("Complete cases used: {n} of {total}; indicators: {p}.", list(n = diagnosis$n, total = diagnosis$original_n, p = diagnosis$p))),
    if (isTRUE(diagnosis$sampled)) result_note_paragraph(class = "structural-result-note", tr("For computational stability, Mardia statistics used a reproducible random subsample of complete cases.")),
    result_note_paragraph(class = "structural-result-note", tr("Mardia tests are sample-size-sensitive omnibus screens. Nonsignificance does not establish multivariate normality, while significance alone does not quantify the substantive size of skew/kurtosis or its impact on estimates. Consider distribution shape, outliers, measurement assumptions, and sensitivity analyses.")),
  )
}

structural_canvas_risk_diagnostics_result_ui <- function(bundle, dataset, analysis_type, language = statedu_initial_language()) {
  if (analysis_type == "plssem") return(NULL)
  tr <- function(en, ko = en) statedu_localized_text(language, en, ko)
  factor_correlations <- structural_canvas_factor_correlation_diagnostics(bundle$fit)
  flagged_correlations <- factor_correlations[factor_correlations$Severity != "Below correlation review reference", , drop = FALSE]
  if (nrow(flagged_correlations)) {
    flagged_correlations$Correlation <- vapply(flagged_correlations$Correlation, format_decimal3, character(1))
    flagged_correlations[["Absolute correlation"]] <- vapply(flagged_correlations[["Absolute correlation"]], format_decimal3, character(1))
  }
  categories <- structural_canvas_ordered_category_diagnostics(dataset, bundle$ordered %||% character(0))
  flagged_categories <- if (nrow(categories)) categories[categories$Status != "No sparsity flag", , drop = FALSE] else categories
  if (nrow(flagged_categories)) flagged_categories$Percent <- paste0(vapply(flagged_categories$Percent, format_decimal3, character(1)), "%")
  ordered_pairs <- structural_canvas_ordered_pair_diagnostics(dataset, bundle$ordered %||% character(0))
  flagged_pairs <- if (nrow(ordered_pairs)) ordered_pairs[ordered_pairs$Status != "No sparsity flag", , drop = FALSE] else ordered_pairs
  if (nrow(flagged_pairs)) flagged_pairs[["Empty %"]] <- paste0(vapply(flagged_pairs[["Empty %"]], format_decimal3, character(1)), "%")
  error_covariances <- structural_canvas_error_covariance_diagnostics(bundle$snapshot %||% list())
  if (!nrow(flagged_correlations) && !nrow(flagged_categories) && !nrow(flagged_pairs) && error_covariances$count == 0L) return(NULL)
  covariance_summary <- tr("{count} of {possible} possible indicator pairs ({percent}%): {status}.", "가능한 지표 쌍 {possible}개 중 {count}개 ({percent}%): {status}.")
  covariance_status <- switch(error_covariances$status,
    "Review complexity" = tr("Review complexity", "복잡성 검토"),
    "Limited" = tr("Limited", "제한적"),
    tr("None", "없음"))
  covariance_values <- list(count = error_covariances$count, possible = error_covariances$possible,
    percent = format_decimal3(100 * error_covariances$ratio), status = covariance_status)
  for (name in names(covariance_values)) covariance_summary <- gsub(paste0("{", name, "}"), as.character(covariance_values[[name]]), covariance_summary, fixed = TRUE)
  render_data_table <- function(table) {
    # Distinct source names avoid the normalized-key collision between Empty and Empty %.
    names(table)[names(table) == "Empty %"] <- tr("Empty cell percentage", "빈 셀 비율")
    names(table)[names(table) == "Correlation"] <- tr("Correlation coefficient", "상관계수")
    structural_canvas_basic_html_table(table, role = "appendix", orientation = "auto", language = language)
  }
  div(class = "result-section regression-result-panel structural-risk-result",
    h4(tr("Data and model risk diagnostics", "데이터 및 모형 위험 진단")),
    if (nrow(flagged_correlations)) tagList(tags$h5(tr("High latent correlations", "높은 잠재변수 상관")), render_data_table(flagged_correlations)),
    if (nrow(flagged_categories)) tagList(tags$h5(tr("Sparse ordered categories", "희소한 순서형 범주")), render_data_table(flagged_categories)),
    if (nrow(flagged_pairs)) tagList(tags$h5(tr("Sparse ordered-indicator cross-tabulations", "희소한 순서형 지표 교차표")), render_data_table(flagged_pairs)),
    if (error_covariances$count > 0L) tagList(
      tags$h5(tr("Correlated measurement errors", "상관된 측정오차")),
      tags$p(covariance_summary)
    ),
    result_note_paragraph(class = "structural-result-note", tr("Latent correlations of .85 or greater warrant discriminant-validity review; .90 or greater are high, and .95 or greater indicate severe construct overlap.", "잠재변수 상관이 .85 이상이면 판별타당도 검토가 필요합니다. .90 이상은 높은 상관, .95 이상은 심각한 구성개념 중복 가능성을 뜻합니다.")),
    if (nrow(flagged_categories)) result_note_paragraph(class = "structural-result-note", tr("A category is flagged when empty, when its count is no greater than max(5, 1% of valid responses), or when it contains at least 95% of valid responses. Sparse or extremely dominant categories can destabilize thresholds and polychoric correlations.", "범주가 비어 있거나, 빈도가 max(5, 유효응답의 1%) 이하이거나, 유효응답의 95% 이상을 차지하면 표시합니다. 희소하거나 지나치게 지배적인 범주는 thresholds와 polychoric 상관을 불안정하게 만들 수 있습니다.")),
    if (nrow(flagged_pairs)) result_note_paragraph(class = "structural-result-note", tr("Each ordered-indicator pair is cross-tabulated over all observed or declared categories. Empty cells or nonempty cells with counts no greater than max(5, 1% of pairwise-valid responses) are flagged because they can destabilize polychoric correlations and the WLSMV weight matrix. Category collapsing requires substantive justification and must preserve order.", "각 순서형 지표 쌍은 관측되었거나 선언된 모든 범주에 대해 교차표를 계산합니다. 빈 셀 또는 빈도가 max(5, pairwise 유효응답의 1%) 이하인 비빈 셀은 polychoric 상관과 WLSMV 가중행렬을 불안정하게 할 수 있어 표시합니다. 범주 병합은 실질적 근거가 있어야 하며 순서를 보존해야 합니다.")),
    if (error_covariances$count > 0L) result_note_paragraph(class = "structural-result-note", tr("Several correlated errors can indicate item redundancy or data-driven overfitting. Each covariance requires substantive justification.", "여러 상관오차는 문항 중복 또는 자료 기반 과적합을 시사할 수 있습니다. 각 공분산에는 실질적 정당화가 필요합니다."))
  )
}

structural_canvas_missing_outliers_result_ui <- function(bundle, dataset, analysis_type, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  tr <- function(en, ko = en, values = list()) {
    text <- statedu_localized_text(language, en, ko)
    for (name in names(values)) text <- gsub(paste0("{", name, "}"), as.character(values[[name]]), text, fixed = TRUE)
    text
  }
  if (analysis_type == "plssem") {
    indicators <- bundle$diagnostics$observed %||% character(0)
    missing <- bundle$missing_diagnostics %||% structural_canvas_pls_missing_diagnostics(dataset, indicators)
    if (!isTRUE(missing$available)) {
      return(div(
        class = "result-section regression-result-panel structural-missing-outlier-result",
        h4(tr("PLS missing-data handling", "PLS 결측자료 처리")),
        result_note_paragraph(class = "structural-result-note", tr("Missing-data diagnostics could not be computed for the model indicators.", "모형 지표의 결측 진단을 계산할 수 없습니다."))
      ))
    }
    variable_table <- missing$variables
    replacement <- missing$replacement_values %||% data.frame()
    if (nrow(variable_table) && nrow(replacement)) {
      variable_table[["Replacement mean"]] <- replacement[["Replacement mean"]][match(variable_table$Variable, replacement$Variable)]
    }
    variable_table$Percent <- paste0(vapply(variable_table$Percent, format_decimal3, character(1)), "%")
    if ("Replacement mean" %in% names(variable_table)) {
      variable_table[["Replacement mean"]] <- vapply(variable_table[["Replacement mean"]], format_decimal3, character(1))
    }
    if (ko) names(variable_table) <- c("변수", "결측 셀", "결측률", if ("Replacement mean" %in% names(variable_table)) "대체 평균")
    render_table <- function(table) structural_canvas_basic_html_table(table, role = "appendix", orientation = "auto", language = language)
    policy <- missing$policy %||% structural_canvas_pls_missing_policy()
    div(
      class = "result-section regression-result-panel structural-missing-outlier-result structural-pls-missing-result",
      h4(tr("PLS missing-data handling", "PLS 결측자료 처리")),
      tags$p(tr("Analysis N: {n}; complete cases: {complete}; rows receiving mean replacement: {rows}; replaced cells: {cells}/{total} ({percent}%).", "분석 N: {n}; 완전 사례: {complete}; 평균 대체 사례: {rows}; 평균 대체 셀: {cells}/{total} ({percent}%).", list(n = missing$effective_n, complete = missing$complete_n, rows = missing$imputed_row_n, cells = missing$imputed_cell_n, total = missing$total_indicator_cells, percent = format_decimal3(missing$missing_cell_percent)))),
      if (nrow(variable_table)) tagList(
        tags$h5(tr("Indicator missingness and replacement values", "지표별 결측 및 대체값")),
        render_table(variable_table)
      ),
      result_note_paragraph(class = "structural-result-note", tr(policy$analysis, "본 분석은 관측된 전체 분석표본에서 계산한 지표별 산술평균으로 결측 셀을 대체합니다. 결측 때문에 행을 삭제하지 않습니다.")),
      result_note_paragraph(class = "structural-result-note", tr(policy$bootstrap, "PLS/PLSc bootstrap은 각 사례 재표집 안에서 지표 평균을 다시 계산한 뒤 결측 셀을 대체하므로 본 분석과 같은 평균대체 원칙을 적용합니다.")),
      if (missing$imputed_cell_n > 0L) result_note_paragraph(
        class = "structural-result-note structural-result-warning",
        tr("Mean replacement can distort variances and associations. Report the amount and likely mechanism of missingness and consider a complete-case or otherwise justified sensitivity analysis when missingness is material.", "평균 대체는 분산과 연관성을 왜곡할 수 있습니다. 결측 원인과 규모를 보고하고, 결측이 실질적이면 완전사례 또는 정당화된 대안 처리와의 민감도 분석을 검토하십시오.")
      )
    )
  } else {
  missing_method_label <- function(method) {
    if (identical(normalize_app_language(language), "en")) return(method)
    switch(method,
      listwise = tr("Listwise deletion", "목록 삭제"),
      pairwise = tr("Pairwise deletion", "쌍별 삭제"),
      fiml = "FIML", unspecified = tr("Not specified", "지정되지 않음"),
      method)
  }
  outlier_reason <- function(reason) {
    korean <- c(
      "Mahalanobis diagnostics are not reported for ordered indicators." = "순서형 지표에는 Mahalanobis 진단을 보고하지 않습니다.",
      "At least two numeric continuous indicators are required." = "숫자형 연속 지표가 2개 이상 필요합니다.",
      "Too few complete cases for multivariate outlier diagnostics." = "다변량 이상치 진단에 필요한 완전 사례가 부족합니다.",
      "The complete-case covariance matrix is singular." = "완전 사례의 공분산 행렬이 특이행렬입니다."
    )
    tr(reason, if (reason %in% names(korean)) unname(korean[[reason]]) else reason)
  }
  indicators <- lavaan::lavNames(bundle$fit, "ov")
  missing <- bundle$missing_diagnostics %||% structural_canvas_missing_diagnostics(dataset, indicators)
  outliers <- if (!length(bundle$ordered %||% character(0))) structural_canvas_mahalanobis_diagnostics(dataset, indicators) else list(available = FALSE, reason = "Mahalanobis diagnostics are not reported for ordered indicators.")
  if (isTRUE(missing$available)) {
    missing_variables <- missing$variables[missing$variables$Missing > 0L, , drop = FALSE]
    if (nrow(missing_variables)) missing_variables$Percent <- paste0(vapply(missing_variables$Percent, format_decimal3, character(1)), "%")
  } else missing_variables <- data.frame()
  if (isTRUE(missing$available) && nrow(missing$patterns)) {
    missing$patterns$Description <- vapply(missing$patterns$Description, function(description) {
      if (identical(description, "Complete")) return(tr("Complete", "완전 사례"))
      if (startsWith(description, "Missing: ")) return(paste0(tr("Missing", "결측"), ": ", substring(description, 10L)))
      description
    }, character(1))
    attr(missing$patterns, "result_user_columns") <- c("Pattern", "Description")
  }
  sensitivity <- structural_canvas_missing_sensitivity_rows(bundle)
  sensitivity[["Primary missing-data method"]] <- vapply(sensitivity[["Primary missing-data method"]], missing_method_label, character(1))
  render_table <- function(table) structural_canvas_basic_html_table(table, role = "appendix", orientation = "auto", language = language)
  div(class = "result-section regression-result-panel structural-missing-outlier-result",
    h4(tr("Missing data and multivariate outliers", "결측 자료 및 다변량 이상치")),
    if (nrow(missing_variables)) tagList(tags$h5(tr("Variable-level missingness", "변수별 결측")), render_table(missing_variables)) else tags$p(tr("No missing indicator values were detected.", "지표 변수에서 결측값이 발견되지 않았습니다.")),
    if (isTRUE(missing$available)) tags$p(tr("Complete cases: {complete} of {n}; incomplete cases: {incomplete}; distinct missingness patterns: {patterns}.", "완전 사례: {complete} / {n}; 불완전 사례: {incomplete}; 서로 다른 결측 패턴: {patterns}.", list(complete = missing$complete_n, n = missing$n, incomplete = missing$incomplete_n, patterns = missing$pattern_count))),
    if (isTRUE(missing$available) && identical(bundle$missing %||% "", "listwise") && missing$incomplete_n > 0L) result_note_paragraph(class = "structural-result-note", tr("Listwise deletion excludes {n} cases ({percent}%) with any missing indicator from model estimation. Examine differences between retained and excluded cases and the potential for selection bias.", "목록 삭제로 지표 중 하나라도 결측인 {n}개 사례({percent}%)가 모형 적합에서 제외됩니다. 완전사례와 제외사례의 차이 및 선택편향 가능성을 검토하십시오.", list(n = missing$incomplete_n, percent = format_decimal3(missing$incomplete_percent)))),
    if (isTRUE(missing$available) && identical(bundle$missing %||% "", "pairwise")) result_note_paragraph(class = "structural-result-note", tr("Minimum indicator-pair available N = {n}. Because covariances can use different case sets, inspect imbalance in pairwise N and positive definiteness.", "지표 쌍별 최소 가용 N = {n}. 서로 다른 공분산이 서로 다른 사례 집합에 근거하므로 쌍별 포함 N의 불균형과 양의 정부호성을 점검하십시오.", list(n = missing$minimum_pairwise_n))),
    tags$h5(tr("Missing-assumption sensitivity record", "결측 가정 민감도 기록")),
    render_table(sensitivity),
    if (identical(sensitivity$Status[[1L]], "Review")) result_note_paragraph(class = "structural-result-note", tr("The MAR assumption underlying FIML is not verified by observed data alone. Assess and document whether key conclusions persist under plausible MNAR departures or alternative missing-data handling.", "FIML의 MAR 가정은 관측자료만으로 검증되지 않습니다. 가능한 MNAR 이탈 또는 대안 결측처리에서 주요 결론이 유지되는지 평가·기록하십시오.")),
    if (isTRUE(missing$available) && missing$pattern_count > 1L) tagList(tags$h5(tr("Missingness patterns", "결측 패턴")), render_table(utils::head(missing$patterns, 20L))),
    result_note_paragraph(class = "structural-result-note", if (length(bundle$ordered %||% character(0))) {
      tr("Ordered-indicator estimation uses {method} missing-data handling. Review sparse pairwise coverage alongside category frequencies.", "순서형 지표 추정은 {method} 결측 처리를 사용합니다. 범주 빈도와 함께 쌍별 포함 범위의 희소성을 검토하십시오.", list(method = missing_method_label(bundle$missing %||% "pairwise")))
    } else if (identical(bundle$missing %||% "", "fiml")) {
      tr("FIML uses all available observations under a missing-at-random (MAR) assumption conditional on variables in the model. MAR cannot be established from the observed data alone; report likely causes of missingness, use of auxiliary variables, and sensitivity analyses. The complete-case count above applies only to diagnostics such as Mardia and Mahalanobis distance.", "FIML은 모형에 포함된 변수들을 조건으로 한 missing-at-random(MAR) 가정하에 사용 가능한 모든 관측치를 사용합니다. MAR은 관측자료만으로 확정할 수 없으므로 결측 원인, 보조변수 포함 여부와 민감도 분석을 보고해야 합니다. 위의 완전 사례 수는 Mardia 및 Mahalanobis 거리 같은 진단에만 적용됩니다.")
    } else {
      tr("The fitted model used missing-data option: {method}.", "적합 모형의 결측 처리 옵션: {method}.", list(method = missing_method_label(bundle$missing %||% "unspecified")))
    }),
    tags$h5(tr("Mahalanobis outlier candidates", "Mahalanobis 이상치 후보")),
    if (!isTRUE(outliers$available)) result_note_paragraph(class = "structural-result-note", outlier_reason(outliers$reason)) else if (!nrow(outliers$table)) tags$p(tr("No complete cases were flagged at p < {alpha}.", "p < {alpha}에서 표시된 완전 사례가 없습니다.", list(alpha = outliers$alpha))) else {
      outlier_table <- outliers$table
      outlier_table$Mahalanobis <- vapply(outlier_table$Mahalanobis, format_decimal3, character(1))
      outlier_table$p <- vapply(outlier_table$p, format_p, character(1))
      tagList(render_table(outlier_table), tags$p(tr("{flagged} of {n} complete cases flagged at p < {alpha}.", "{n}개 완전 사례 중 {flagged}개가 p < {alpha}에서 표시되었습니다.", list(flagged = outliers$flagged_n, n = outliers$n, alpha = outliers$alpha))))
    },
    result_note_paragraph(class = "structural-result-note", tr("Mahalanobis candidates should be investigated for data errors, unusual but valid cases, and influence. They are not removed automatically; use robust estimation or a documented sensitivity analysis when appropriate.", "Mahalanobis 후보는 자료 오류, 특이하지만 유효한 사례, 영향점을 검토해야 합니다. 자동으로 제거하지 않으며, 필요하면 robust 추정 또는 문서화된 민감도 분석을 사용하십시오."))
  )
  }
}
