# Structural equation canvas reliability bootstrap render outputs.

structural_canvas_register_reliability_bootstrap_outputs <- function(output, prefix, fit_result, app_language_fn = NULL) {
output[[paste0(prefix, "_result_reliability_bootstrap")]] <- renderUI({
  bundle <- fit_result()
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  values <- bundle$reliability_bootstrap_result %||% NULL
  requested <- as.integer(bundle$reliability_bootstrap %||% 0L)
  if (requested <= 0L) return(tags$p(class = "structural-result-note", if (ko) "AVE, CR, Cronbach's alpha, omega는 점추정값입니다. 구간추정이 필요하면 분석 옵션에서 AVE/신뢰도 부트스트랩 CI를 선택하십시오." else "AVE, CR, Cronbach's alpha, and omega are point estimates. Select AVE/reliability bootstrap CI in the analysis options when interval estimates are required."))
  if (is.null(values) || !nrow(values)) return(tags$p(class = "structural-result-note", if (ko) "사용 가능한 추정값을 산출한 재표집이 없어 AVE/신뢰도 부트스트랩 구간을 계산하지 못했습니다." else "AVE/reliability bootstrap intervals could not be estimated because no resample produced usable estimates."))
  reliability_ci_method <- structural_canvas_bootstrap_ci_method(bundle$reliability_ci_method %||% "bias_corrected")
  reliability_ci_label <- if (identical(reliability_ci_method, "bca")) "BCa" else if (identical(reliability_ci_method, "bias_corrected")) "bias-corrected (BC)" else "percentile"
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
  tagList(
    tags$h5(if (ko) paste0("AVE 및 신뢰도 ", reliability_ci_label, " 부트스트랩 구간 (", requested, "회 재표집; seed = ", bundle$reliability_seed, ")") else paste0("AVE and reliability ", reliability_ci_label, " bootstrap intervals (", requested, " resamples; seed = ", bundle$reliability_seed, ")")),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) paste0("구간은 사례 재표집", if (identical(reliability_ci_method, "bca")) "과 leave-one-out 잭나이프 가속도" else if (identical(reliability_ci_method, "bias_corrected")) "과 편향보정(BC) 분위수" else " 백분위수 부트스트랩", "을 사용하고, 선택된 ", if (identical(bundle$validity_formula, "model_implied")) "모형-함의 모수" else "표준화 적재량", " AVE/CR 공식을 적용합니다. 이 출력에서 McDonald's omega total은 CR과 같은 적합된 congeneric scoring 공식에 따릅니다.") else paste0("Intervals use case resampling", if (identical(reliability_ci_method, "bca")) " with leave-one-out jackknife acceleration" else if (identical(reliability_ci_method, "bias_corrected")) " with bias-corrected (BC) quantiles" else " percentile bootstrap", " and the selected ", if (identical(bundle$validity_formula, "model_implied")) "model-implied parameter" else "standardized-loading", " AVE/CR formula. McDonald's omega total follows the same fitted congeneric scoring formula as CR in this output.")),
    if (bca_unavailable) tags$p(class = "structural-result-note", if (ko) "BCa unavailable은 해당 통계량의 편향보정 또는 잭나이프 가속도를 계산하지 못했다는 뜻입니다. 유효 반복 수를 늘리거나 보고에는 percentile CI 사용을 검토하십시오." else "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that statistic; increase valid replicates or use percentile CI for reporting."),
    if (any(boundary_interval)) tags$p(class = "structural-result-note", if (ko) "† 표시는 추정값 또는 구간이 허용 가능한 [0, 1] 계수 범위를 벗어났음을 뜻합니다. 보고를 위해 구간을 임의 절단하지 말고 모형 admissibility, 표본 불안정성, 문항 공분산 구조, 실패한 재표집을 점검하십시오." else "† marks an estimate or interval extending outside the admissible [0, 1] coefficient range. Do not truncate the interval for reporting; investigate model admissibility, sample instability, item covariance structure, and failed resamples."),
    if (any(incomplete)) tags$p(class = "structural-result-note", if (ko) "일부 재표집은 주 CFA와 같은 수렴, 분산, 공분산행렬, 자유도, 잠재상관 admissibility 점검을 통과하지 못했거나 통계량을 산출하지 못했습니다. 유효 반복 수가 요청 반복 수보다 크게 낮으면 구간을 신중하게 해석하십시오." else "Some resamples failed the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA, or yielded unavailable statistics. Interpret intervals cautiously when the valid-replicate count is materially below the requested count."),
    if (any(caution_intervals)) tags$p(class = "structural-result-note", if (ko) "Caution은 요청 재표집의 50% 이상 80% 미만만 해당 통계량을 산출했다는 뜻입니다. 백분위수 한계값은 불안정할 수 있으므로 유효 반복 수를 함께 보고하십시오." else "Caution indicates that 50% to less than 80% of requested resamples yielded the statistic. Treat the percentile limits as unstable and report the valid-replicate count."),
    if (any(unreliable_intervals)) tags$p(class = "structural-result-note", if (ko) "Unreliable은 요청 재표집의 50% 미만만 해당 통계량을 산출했다는 뜻입니다. 표시된 분위수는 진단용일 뿐 방어 가능한 신뢰구간으로 보고하지 말고, 먼저 수렴, admissibility, 희소 범주 또는 모형 지정 문제를 해결하십시오." else "Unreliable indicates that fewer than 50% of requested resamples yielded the statistic. The displayed quantiles are diagnostic only and should not be reported as a defensible confidence interval; resolve convergence, admissibility, sparse-category, or specification problems first.")
  )
})
  invisible(TRUE)
}
