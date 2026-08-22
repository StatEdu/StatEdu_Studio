structural_canvas_register_validity_outputs <- function(output, prefix, analysis_type, fit_result, result_table,
                                                        app_language_fn = NULL,
                                                        variable_table_fn = function() NULL,
                                                        labels_fn = function() character(0)) {
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
        tags$p(class = "structural-result-note", if (ko) "구성개념 유형, 측정 모드와 각 지표의 증거 역할은 본표의 수치 해석을 방해하지 않도록 아래 보조표에 제시합니다." else "Construct type, measurement mode, and evidence role are reported in the supplementary table below so the main numeric table remains concise."),
        tags$p(class = "structural-result-note", if (ko) "반영형 공통요인의 표준 PLS 신뢰도·AVE·HTMT는 Mode A 점수 대리변수의 진단이며 공분산 기반 요인모형의 증거가 아닙니다. 반영형 합성변수에서는 합성점수 평가로만 해석하고, 형성형 합성변수에는 내적일관성·AVE·HTMT를 적용하지 않습니다." else "For a reflective common factor, standard-PLS reliability, AVE, and HTMT diagnose a Mode A score proxy rather than provide covariance-based factor-model evidence. For a reflective composite, interpret them only as composite-score diagnostics. Internal consistency, AVE, and HTMT are not applied to formative composites."),
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
        tags$p(class = "structural-result-note", if (ko) "HTMT와 Fornell-Larcker는 반영형 구성개념끼리만 계산합니다. 해당 결과는 보조 정보이며 단독 합격판정이 아니므로 교차적재, 구성개념 상관, 이론과 경쟁 측정모형을 함께 검토하십시오." else "HTMT and Fornell-Larcker are computed only between reflective constructs. They are supplementary rather than standalone pass decisions; also review cross-loadings, construct correlations, theory, and competing measurement models.")
      )
    })
    return(invisible(TRUE))
  }
  display_name_for <- function(bundle) structural_canvas_display_name_resolver(
    snapshot = bundle$snapshot %||% list(),
    variable_table = if (is.function(variable_table_fn)) variable_table_fn() else variable_table_fn,
    labels = if (is.function(labels_fn)) labels_fn() %||% character(0) else labels_fn %||% character(0),
    moderation_definitions = bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list(),
    language = statedu_current_language(app_language_fn)
  )
  structural_canvas_register_latent_correlation_outputs(output, prefix, fit_result, app_language_fn, display_name_for)
  structural_canvas_register_validity_note_outputs(output, prefix, fit_result, result_table, app_language_fn)
  structural_canvas_register_factor_score_outputs(output, prefix, fit_result, app_language_fn, display_name_for)
  structural_canvas_register_reliability_bootstrap_outputs(output, prefix, fit_result, app_language_fn)
  structural_canvas_register_htmt_outputs(output, prefix, fit_result, result_table, app_language_fn)
  invisible(TRUE)
}
