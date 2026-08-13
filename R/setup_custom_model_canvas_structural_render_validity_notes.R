# Structural equation canvas validity-note render outputs.

structural_canvas_register_validity_note_outputs <- function(output, prefix, fit_result, result_table, app_language_fn = NULL) {
output[[paste0(prefix, "_result_validity_note")]] <- renderUI({
  bundle <- fit_result()
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  validity_values <- result_table("validity")
  abnormal_reliability <- any(grepl("†", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
  single_indicator <- any(grepl("‡", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
  constrained_single_indicator <- any(grepl("¶", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
  orthogonal_not_assessed <- any(grepl("§", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
  theta <- as.matrix(lavaan::lavInspect(bundle$fit, "theta"))
  correlated_errors <- length(theta) > 1L && any(abs(theta[row(theta) != col(theta)]) > sqrt(.Machine$double.eps), na.rm = TRUE)
  snapshot <- bundle$snapshot %||% list()
  missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
  has_higher_order <- any(vapply(snapshot$edges %||% list(), function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), logical(1)))
  validity_loadings <- lavaan::standardizedSolution(bundle$fit)
  validity_loadings <- validity_loadings[validity_loadings$op == "=~" & validity_loadings$rhs %in% lavaan::lavNames(bundle$fit, "ov"), c("lhs", "rhs"), drop = FALSE]
  cross_loaded_indicators <- unique(validity_loadings$rhs[duplicated(validity_loadings$rhs) | duplicated(validity_loadings$rhs, fromLast = TRUE)])
  tagList(
    tags$p(class = "structural-result-note", if (ko) "괄호 안 대각값은 sqrt(AVE)이고, 하삼각 값은 잠재변수 상관입니다. Max |r|은 sqrt(AVE)와 비교합니다. 나머지 열은 지표 수(k), AVE, CR, Cronbach's alpha, McDonald's omega total을 보고합니다." else "Diagonal values in parentheses are sqrt(AVE); lower-triangle values are latent correlations. Max |r| is compared with sqrt(AVE). The remaining columns report the number of indicators (k), AVE, CR, Cronbach's alpha, and McDonald's omega total."),
    tags$p(class = "structural-result-note", if (ko) "Fornell-Larcker는 sqrt(AVE)가 해당 요인의 최대 절대 상관보다 클 때 'Criterion met'으로, 그렇지 않으면 'Review needed'로 표시합니다." else "Fornell-Larcker is marked 'Criterion met' when sqrt(AVE) is greater than the factor's largest absolute correlation; otherwise it is marked 'Review needed'."),
    tags$p(class = "structural-result-note", if (ko) "해석 안내는 자주 인용되는 기술적 기준(AVE ≥ .50, CR/Cronbach's alpha/omega total ≥ .70)을 사용합니다. 이는 보편적 합격/불합격 규칙이 아니며 구성개념 범위, 문항 수, 모형 허용성, 연구 목적과 함께 해석해야 합니다." else "Guidance uses commonly cited descriptive cutoffs (AVE ≥ .50; CR, Cronbach's alpha, and omega total ≥ .70). These are heuristics rather than universal pass/fail rules and should be interpreted with construct breadth, item count, model admissibility, and study purpose."),
    if (length(missing_covariances)) tags$p(class = "structural-result-note", paste0(if (ko) "주의: 누락된 외생 잠재변수 공분산 경로(" else "Caution: missing exogenous latent covariance paths (", paste(missing_covariances, collapse = ", "), if (ko) ")는 0으로 고정됩니다." else ") are fixed to zero.")),
    if (correlated_errors) tags$p(class = "structural-result-note", if (ko) "CR은 잔차 공분산 행렬에 추정된 측정오차 공분산을 반영합니다." else "CR incorporates the estimated measurement-error covariances in the residual covariance matrix."),
    if (abnormal_reliability) tags$p(class = "structural-result-note", if (ko) "† AVE 또는 신뢰도 계수가 산출되지 않았거나 [0, 1] 범위를 벗어났습니다. 비정상 문항 공분산, 허용 불가능한 해, 또는 식별되지 않은 계산을 점검하십시오." else "† AVE or a reliability coefficient is unavailable or outside [0, 1], indicating unusual item covariances, an inadmissible solution, or an unidentified calculation."),
    if (single_indicator) tags$p(class = "structural-result-note", if (ko) "‡ 외부 근거가 있는 신뢰도 제약이 없는 단일지표 요인에는 AVE와 CR을 보고하지 않습니다." else "‡ AVE and CR are not reported for a single-indicator factor without externally justified reliability constraints."),
    if (constrained_single_indicator) tags$p(class = "structural-result-note", if (ko) "¶ 단일지표 요인은 외부 근거가 있는 고정 잔차분산을 사용합니다. AVE, CR, omega total은 독립적으로 추정된 내적일관성이 아니라 부여한 신뢰도 제약을 반영하며, Cronbach's alpha와 Fornell-Larcker는 평가하지 않습니다." else "¶ The single-indicator factor uses an externally justified fixed residual variance. AVE, CR, and omega total reflect that imposed reliability constraint rather than independently estimated internal consistency; Cronbach's alpha and Fornell-Larcker are not assessed."),
    if (orthogonal_not_assessed) tags$p(class = "structural-result-note", if (ko) "§ 하나 이상의 외생 잠재변수 공분산 경로가 생략되어 0으로 고정되었기 때문에 Fornell-Larcker를 평가하지 않았습니다." else "§ Fornell-Larcker was not assessed because one or more exogenous latent covariances were fixed to zero by omitted covariance paths."),
    if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", if (ko) "순서형 지표에서는 AVE, CR, omega가 표준화된 잠재반응변수 해를 사용합니다. Cronbach's alpha는 lavaan의 polychoric 상관행렬을 사용하는 ordinal alpha입니다." else "For ordered indicators, AVE, CR, and omega use the standardized latent-response solution; Cronbach's alpha uses lavaan's polychoric correlation matrix (ordinal alpha)."),
    if (!length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", if (ko) "Cronbach's alpha는 분석에 사용된 지표의 표본 공분산 행렬을 사용합니다. Omega는 모형 기반이며 추정된 잔차 공분산을 반영합니다." else "Cronbach's alpha uses the analyzed indicators' sample covariance matrix. Omega is model-based and incorporates estimated residual covariances."),
    tags$p(class = "structural-result-note", if (ko) "이 표에서 omega total은 CR과 같은 모형함의 적재량 및 잔차 공분산 행렬을 사용합니다. 현재 congeneric scoring 명세에서는 두 값이 같으므로, 보고 명확성을 위해 두 라벨을 모두 유지합니다." else "In this table, omega total uses the same model-implied loadings and residual covariance matrix as CR; under the current congeneric scoring specification it therefore equals CR. Both labels are retained for reporting clarity."),
    if (has_higher_order) tags$p(class = "structural-result-note", if (ko) "AVE, CR, Fornell-Larcker, HTMT는 관측지표가 있는 1차 요인에 대해 보고합니다. 고차요인은 이러한 신뢰도 및 판별타당도 계산에서 제외됩니다." else "AVE, CR, Fornell-Larcker, and HTMT are reported for first-order factors with observed indicators; higher-order factors are excluded from these reliability and discriminant-validity calculations."),
    if (length(cross_loaded_indicators)) tags$p(class = "structural-result-note", paste0(if (ko) "주의: 교차적재 지표(" else "Caution: cross-loaded indicators (", paste(cross_loaded_indicators, collapse = ", "), if (ko) ")가 둘 이상의 요인에 포함되어 있습니다. 요인별 AVE/CR은 기술적 지표로만 보며, 단순구조 기반 판별타당도 해석에는 특별한 주의가 필요합니다." else ") appear in more than one factor. Factor-specific AVE/CR remain descriptive, while simple-structure discriminant-validity interpretations require particular caution.")),
    if (!isTRUE(bundle$diagnostics$admissible %||% TRUE)) tags$p(class = "structural-result-note", if (ko) "주의: 적합된 해가 하나 이상의 허용성 점검을 통과하지 못했습니다. AVE, CR, 판별타당도 결과를 최종 결과로 해석하지 마십시오." else "Caution: the fitted solution failed one or more admissibility checks; AVE, CR, and discriminant-validity results should not be interpreted as final.")
  )
})
  invisible(TRUE)
}
