structural_canvas_register_validity_outputs <- function(output, prefix, analysis_type, fit_result, result_table, app_language_fn = NULL) {
  if (identical(analysis_type, "plssem")) {
    output[[paste0(prefix, "_result_htmt")]] <- renderUI({
      table <- result_table("pls_htmt")
      if (!is.data.frame(table) || !nrow(table)) return(NULL)
      tagList(
        tags$h5("HTMT matrix"),
        structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-htmt-matrix")
      )
    })
    output[[paste0(prefix, "_result_validity_note")]] <- renderUI({
      bundle <- fit_result()
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      tagList(
        tags$p(class = "structural-result-note", if (ko) "PLS-SEM 타당도 출력은 seminr의 신뢰도, Fornell-Larcker, HTMT 요약에 기반합니다. lavaan 전용 잠재공분산, 요인점수, delta-method 진단은 PLS 모형 화면에서는 표시하지 않습니다." else "PLS-SEM validity output is based on seminr reliability, Fornell-Larcker, and HTMT summaries. Lavaan-specific latent covariance, factor-score, and delta-method diagnostics are not displayed for PLS models in this view."),
        if (as.integer(bundle$pls_bootstrap %||% 0L) > 0L) tags$p(class = "structural-result-note", if (ko) "PLS bootstrap 열은 seminr가 해당 bootstrap 요약을 반환할 때 직접경로, 총효과와 간접효과, HTMT, outer loading, outer weight에 추가됩니다." else "PLS bootstrap columns are added for direct paths, total and indirect effects, HTMT, outer loadings, and outer weights when seminr returns the corresponding bootstrap summaries.")
      )
    })
    output[[paste0(prefix, "_result_validity_guide")]] <- renderUI({
      table <- result_table("validity_guide")
      if (!is.data.frame(table) || !nrow(table)) return(NULL)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      tagList(
        tags$h5(if (ko) "표 3 보조: 판별타당도 가이드" else "Supplementary Table 3: Discriminant-validity guide"),
        structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-pls-validity-guide-table"),
        tags$p(class = "structural-result-note", if (ko) "HTMT 부트스트랩 추론과 Fornell-Larcker 판정은 본표의 신뢰도/타당도 핵심 지표를 해석하기 위한 보조 정보입니다." else "HTMT bootstrap inference and Fornell-Larcker decisions are supplementary interpretation aids for the main reliability and validity table.")
      )
    })
    return(invisible(TRUE))
  }
  structural_canvas_register_latent_correlation_outputs(output, prefix, fit_result, app_language_fn)
  structural_canvas_register_validity_note_outputs(output, prefix, fit_result, result_table, app_language_fn)
  structural_canvas_register_factor_score_outputs(output, prefix, fit_result, app_language_fn)
  structural_canvas_register_reliability_bootstrap_outputs(output, prefix, fit_result, app_language_fn)
  structural_canvas_register_htmt_outputs(output, prefix, fit_result, result_table, app_language_fn)
  invisible(TRUE)
}
