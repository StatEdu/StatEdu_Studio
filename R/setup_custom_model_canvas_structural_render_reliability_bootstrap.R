# Structural equation canvas reliability bootstrap render outputs.

structural_canvas_register_reliability_bootstrap_outputs <- function(output, prefix, fit_result, app_language_fn = NULL) {
output[[paste0(prefix, "_result_reliability_bootstrap")]] <- renderUI({
  bundle <- fit_result()
  language <- statedu_current_language(app_language_fn)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  values <- bundle$reliability_bootstrap_result %||% NULL
  requested <- as.integer(bundle$reliability_bootstrap %||% 0L)
  if (requested <= 0L) return(result_note_paragraph(class = "structural-result-note", tr("AVE, CR, Cronbach's alpha, and omega are point estimates. Select AVE/reliability bootstrap CI in the analysis options when interval estimates are required.", "AVE, CR, Cronbach's alpha, omega는 점추정값입니다. 구간추정이 필요하면 분석 옵션에서 AVE/신뢰도 부트스트랩 CI를 선택하십시오.")))
  if (isTRUE(bundle$cfa_bootstrap_pending) && is.null(values)) return(result_note_paragraph(class = "structural-result-note", tr("AVE/reliability bootstrap intervals are being computed in the background. This result table will update automatically when complete.", "AVE·신뢰도 부트스트랩을 백그라운드에서 계산하고 있습니다. 완료되면 이 결과표가 자동으로 갱신됩니다.")))
  if (isTRUE(bundle$cfa_bootstrap_canceled) && is.null(values)) return(result_note_paragraph(class = "structural-result-note", tr("The AVE/reliability bootstrap was stopped by the user. Point estimates and base-model results remain available.", "AVE·신뢰도 부트스트랩이 사용자 요청으로 중단되었습니다. 점추정값과 기본 분석 결과는 유지됩니다.")))
  if (is.null(values) || !nrow(values)) return(result_note_paragraph(class = "structural-result-note", tr("AVE/reliability bootstrap intervals could not be estimated because no resample produced usable estimates.", "사용 가능한 추정값을 산출한 재표집이 없어 AVE/신뢰도 부트스트랩 구간을 계산하지 못했습니다.")))
  reliability_ci_method <- structural_canvas_bootstrap_ci_method(bundle$reliability_ci_method %||% "bias_corrected")
  reliability_ci_label <- if (identical(reliability_ci_method, "bca")) "BCa" else if (identical(reliability_ci_method, "bias_corrected")) tr("Bias-corrected (BC)", "편향보정 (BC)") else tr("Percentile", "백분위수")
  incomplete <- values[["Valid replicates"]] < values[["Requested replicates"]]
  caution_intervals <- values$Status == "Caution"
  unreliable_intervals <- values$Status == "Unreliable"
  boundary_interval <- !is.finite(values$Lower) | !is.finite(values$Upper) | values$Lower < 0 | values$Upper > 1
  if (!"CI method" %in% names(values)) values[["CI method"]] <- if (identical(reliability_ci_method, "bias_corrected")) "Bias-corrected (BC)" else if (identical(reliability_ci_method, "bca")) "BCa" else "Percentile"
  bca_unavailable <- "CI method" %in% names(values) && any(values[["CI method"]] == "BCa unavailable")
  values$Statistic[values$Statistic == "Alpha"] <- "Cronbach's α"
  values$Statistic[values$Statistic == "Omega"] <- "McDonald's ωtotal"
  values$Estimate <- paste0(vapply(values$Estimate, format_decimal3, character(1)), ifelse(!is.finite(values$Estimate) | values$Estimate < 0 | values$Estimate > 1, "†", ""))
  values$Lower <- paste0(vapply(values$Lower, format_decimal3, character(1)), ifelse(boundary_interval, "†", ""))
  values$Upper <- paste0(vapply(values$Upper, format_decimal3, character(1)), ifelse(boundary_interval, "†", ""))
  values[["Valid %"]] <- paste0(vapply(values[["Valid %"]], format_decimal3, character(1)), "%")
  names(values)[names(values) == "Lower"] <- "95% CI lower"
  names(values)[names(values) == "Upper"] <- "95% CI upper"
  labels <- c("Adequate"="충분", "Caution"="주의", "Unreliable"="신뢰 불가", "BCa unavailable"="BCa 사용 불가", "Percentile"="백분위수", "Bias-corrected (BC)"="편향보정 (BC)")
  for (column in intersect(c("Status", "CI method"), names(values))) values[[column]] <- vapply(as.character(values[[column]]), function(x) if(x %in% names(labels)) tr(x, unname(labels[[x]])) else x, character(1), USE.NAMES=FALSE)
  names(values)[names(values)=="CI method"] <- tr("CI method", "CI 산출법")
  names(values)[names(values)=="Estimate"] <- tr("Estimate", "추정값")
  attr(values, "result_user_columns") <- seq_len(ncol(values))
  tagList(
    tags$h5(sprintf(tr("AVE and reliability %s bootstrap intervals (%s resamples; seed = %s)", "AVE 및 신뢰도 %s 부트스트랩 구간 (%s회 재표집; seed = %s)"), reliability_ci_label, requested, bundle$reliability_seed)),
    structural_canvas_basic_html_table(values, role = "appendix", orientation = "auto", language = statedu_current_language(app_language_fn)),
    result_note_paragraph(class = "structural-result-note", tr("Intervals use case resampling. McDonald's omega total follows the same fitted congeneric scoring formula as CR in this output.", "구간은 사례 재표집을 사용합니다. 이 출력에서 McDonald's omega total은 CR과 같은 적합된 congeneric scoring 공식에 따릅니다.")),
    result_note_paragraph(class = "structural-result-note", if (identical(reliability_ci_method, "bca")) tr("BCa uses leave-one-out jackknife acceleration.", "BCa는 leave-one-out 잭나이프 가속도를 사용합니다.") else if (identical(reliability_ci_method, "bias_corrected")) tr("Intervals use bias-corrected (BC) quantiles.", "구간은 편향보정(BC) 분위수를 사용합니다.") else tr("Intervals use percentile bootstrap quantiles.", "구간은 백분위수 부트스트랩 분위수를 사용합니다.")),
    result_note_paragraph(class = "structural-result-note", if (identical(bundle$validity_formula, "model_implied")) tr("The selected AVE/CR formula uses model-implied parameters.", "선택한 AVE/CR 공식은 모형-함의 모수를 사용합니다.") else tr("The selected AVE/CR formula uses standardized loadings.", "선택한 AVE/CR 공식은 표준화 적재량을 사용합니다.")),
    if (bca_unavailable) result_note_paragraph(class = "structural-result-note", tr("BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that statistic; increase valid replicates or use percentile CI for reporting.", "BCa unavailable은 해당 통계량의 편향보정 또는 잭나이프 가속도를 계산하지 못했다는 뜻입니다. 유효 반복 수를 늘리거나 보고에는 percentile CI 사용을 검토하십시오.")),
    if (any(boundary_interval)) result_note_paragraph(class = "structural-result-note", tr("† marks an estimate or interval extending outside the admissible [0, 1] coefficient range. Do not truncate the interval for reporting; investigate model admissibility, sample instability, item covariance structure, and failed resamples.", "† 표시는 추정값 또는 구간이 허용 가능한 [0, 1] 계수 범위를 벗어났음을 뜻합니다. 보고를 위해 구간을 임의 절단하지 말고 모형 admissibility, 표본 불안정성, 문항 공분산 구조, 실패한 재표집을 점검하십시오.")),
    if (any(incomplete)) result_note_paragraph(class = "structural-result-note", tr("Some resamples failed the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA, or yielded unavailable statistics. Interpret intervals cautiously when the valid-replicate count is materially below the requested count.", "일부 재표집은 주 CFA와 같은 수렴, 분산, 공분산행렬, 자유도, 잠재상관 admissibility 점검을 통과하지 못했거나 통계량을 산출하지 못했습니다. 유효 반복 수가 요청 반복 수보다 크게 낮으면 구간을 신중하게 해석하십시오.")),
    if (any(caution_intervals)) result_note_paragraph(class = "structural-result-note", tr("Caution indicates that 50% to less than 80% of requested resamples yielded the statistic. Treat the percentile limits as unstable and report the valid-replicate count.", "Caution은 요청 재표집의 50% 이상 80% 미만만 해당 통계량을 산출했다는 뜻입니다. 백분위수 한계값은 불안정할 수 있으므로 유효 반복 수를 함께 보고하십시오.")),
    if (any(unreliable_intervals)) result_note_paragraph(class = "structural-result-note", tr("Unreliable indicates that fewer than 50% of requested resamples yielded the statistic. The displayed quantiles are diagnostic only and should not be reported as a defensible confidence interval; resolve convergence, admissibility, sparse-category, or specification problems first.", "Unreliable은 요청 재표집의 50% 미만만 해당 통계량을 산출했다는 뜻입니다. 표시된 분위수는 진단용일 뿐 방어 가능한 신뢰구간으로 보고하지 말고, 먼저 수렴, admissibility, 희소 범주 또는 모형 지정 문제를 해결하십시오."))
  )
})
  invisible(TRUE)
}
