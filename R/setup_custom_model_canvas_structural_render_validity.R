structural_canvas_register_validity_outputs <- function(output, prefix, analysis_type, fit_result, result_table,
                                                        app_language_fn = NULL,
                                                        variable_table_fn = function() NULL,
                                                        labels_fn = function() character(0),
                                                        table_number_fn = NULL,
                                                        appendix_result_table = result_table) {
  if (identical(analysis_type, "plssem")) {
    output[[paste0(prefix, "_result_htmt")]] <- renderUI({
      table <- result_table("pls_htmt")
      if (!is.data.frame(table) || !nrow(table)) return(NULL)
      tagList(
        tags$h5("HTMT matrix"),
        structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-htmt-matrix", role = "main", orientation = if (ncol(table) - 1L <= 9L) "portrait" else "landscape")
      )
    })
    output[[paste0(prefix, "_result_validity_note")]] <- renderUI({
      bundle <- fit_result()
      language <- statedu_current_language(app_language_fn)
      tr <- function(en, ko) statedu_localized_text(language, en, ko)
      bootstrap <- bundle$pls_bootstrap_result %||% list()
      valid_n <- suppressWarnings(as.integer(bootstrap$nboot %||% 0L))
      requested_n <- suppressWarnings(as.integer(bootstrap$requested_nboot %||% bundle$pls_bootstrap %||% 0L))
      minimum_ratio <- suppressWarnings(as.numeric(bootstrap$minimum_valid_ratio %||% .80))
      bootstrap_status <- as.character(bootstrap$bootstrap_status %||% "Not recorded")[[1L]]
      failure_message <- as.character(bootstrap$failure_message %||% "")
      failure_message <- if (length(failure_message)) trimws(failure_message[[1L]]) else ""
      status_labels <- c("Adequate"="충분", "Insufficient"="불충분", "Pending"="진행 중", "Failed"="실패", "Canceled"="중단", "Not recorded"="기록 없음")
      if (bootstrap_status %in% names(status_labels)) bootstrap_status <- tr(bootstrap_status, unname(status_labels[[bootstrap_status]]))
      tagList(
        result_note_paragraph(class = "structural-result-note", tr("Construct type, measurement mode, and evidence role are reported in the supplementary table below so the main numeric table remains concise.", "구성개념 유형, 측정 모드와 각 지표의 증거 역할은 본표의 수치 해석을 방해하지 않도록 아래 보조표에 제시합니다.")),
        result_note_paragraph(class = "structural-result-note", tr("For a reflective common factor, standard-PLS reliability, AVE, and HTMT diagnose a Mode A score proxy rather than provide covariance-based factor-model evidence. For a reflective composite, interpret them only as composite-score diagnostics. Internal consistency, AVE, and HTMT are not applied to formative composites.", "반영형 공통요인의 표준 PLS 신뢰도·AVE·HTMT는 Mode A 점수 대리변수의 진단이며 공분산 기반 요인모형의 증거가 아닙니다. 반영형 합성변수에서는 합성점수 평가로만 해석하고, 형성형 합성변수에는 내적일관성·AVE·HTMT를 적용하지 않습니다.")),
        if (as.integer(bundle$pls_bootstrap %||% 0L) > 0L) result_note_paragraph(class = "structural-result-note", tr("PLS bootstrap columns cover direct paths, total and indirect effects, HTMT, outer loadings, and outer weights. A resample is valid only when this complete statistic set is finite and structurally consistent.", "PLS bootstrap 열은 직접경로, 총효과와 간접효과, HTMT, outer loading, outer weight에 추가됩니다. 이 통계량 전체가 유한하고 구조가 일치하는 반복만 유효합니다.")),
        if (requested_n > 0L) tags$p(
          class = paste("structural-result-note", if (!isTRUE(bootstrap$inference_available)) "structural-result-warning" else ""),
          sprintf(tr("PLS bootstrap status: %s; valid resamples: %s/%s. The minimum reporting ratio is %s%%; CIs and test statistics are suppressed below it.", "PLS bootstrap 상태: %s; 유효 재표집: %s/%s회. 최소 보고 기준은 %s%%이며, 미달 시 CI와 검정값을 표시하지 않습니다."), bootstrap_status, valid_n, requested_n, formatC(100 * minimum_ratio, format = "fg", digits = 3)),
          if (nzchar(failure_message)) paste0(" ", sprintf(tr("Detail: %s", "상세: %s"), failure_message))
        )
      )
    })
    output[[paste0(prefix, "_result_validity_guide")]] <- renderUI({
      table <- appendix_result_table("validity_guide")
      if (!is.data.frame(table) || !nrow(table)) return(NULL)
      language <- statedu_current_language(app_language_fn)
      tr <- function(en, ko) statedu_localized_text(language, en, ko)
      labels <- c(
        "Construct" = "구성개념",
        "Construct type" = "구성개념 유형",
        "Mode" = "측정 모드",
        "Evidence role" = "증거 역할",
        "Max HTMT CI lower" = "최대 HTMT CI 하한",
        "Max HTMT CI upper" = "최대 HTMT CI 상한",
        "Max HTMT p" = "최대 HTMT p값",
        "Common factor" = "공통요인",
        "Composite" = "합성변수",
        "Unspecified" = "미지정",
        "Reflective" = "반영형",
        "Formative" = "형성형",
        "N/A - formative" = "해당 없음 - 형성형",
        "Weights, collinearity, content coverage, and redundancy; internal consistency/AVE/HTMT not applicable" = "가중치, 공선성, 내용 포괄성과 중복성; 내적일관성/AVE/HTMT는 해당 없음",
        "PLSc common-factor diagnostics; interpretation depends on consistency-correction assumptions" = "PLSc 공통요인 진단; 해석은 일치성 보정 가정에 의존",
        "Mode A score-proxy diagnostics; not covariance-based factor-model evidence" = "Mode A 점수 대리변수 진단; 공분산 기반 요인모형의 증거가 아님",
        "Reflective-composite diagnostics; do not infer a latent common cause or explicit measurement-error separation" = "반영형 합성변수 진단; 잠재 공통원인이나 명시적인 측정오차 분리를 추론하지 않음",
        "Below reference" = "참고값 미만",
        "Review needed" = "검토 필요")
      translate <- function(value) {
        if (value %in% names(labels)) tr(value, unname(labels[[value]])) else value
      }
      # Translate program metadata only; construct names are user data.
      for (column in intersect(c("Construct type", "Mode", "Evidence role", "Fornell-Larcker"), names(table))) {
        table[[column]] <- vapply(as.character(table[[column]]), translate, character(1), USE.NAMES = FALSE)
      }
      construct_index <- if ("Construct" %in% names(table)) match("Construct", names(table)) else 1L
      names(table) <- vapply(names(table), translate, character(1), USE.NAMES = FALSE)
      compact <- structural_canvas_compact_common_display_columns(table, names(table)[[construct_index]])
      attr(compact$table, "result_user_columns") <- seq_along(compact$table)
      common_note <- if (length(compact$common)) {
        sprintf(tr("Shared specification for all constructs: %s.", "모든 구성개념의 공통 명세: %s."),
          paste0(names(compact$common), " = ", unname(compact$common), collapse = "; "))
      } else ""
      tagList(
        tags$h5(tr("Validity supplement: Discriminant-validity guide", "타당도 보조표: 판별타당도 가이드")),
        structural_canvas_basic_html_table(compact$table, class = "table table-striped table-bordered structural-pls-validity-guide-table", language = language),
        if (nzchar(common_note)) result_note_paragraph(class = "structural-result-note", common_note),
        result_note_paragraph(class = "structural-result-note", tr("HTMT and Fornell-Larcker are computed only between reflective constructs. They are supplementary rather than standalone pass decisions; also review cross-loadings, construct correlations, theory, and competing measurement models.", "HTMT와 Fornell-Larcker는 반영형 구성개념끼리만 계산합니다. 해당 결과는 보조 정보이며 단독 합격판정이 아니므로 교차적재, 구성개념 상관, 이론과 경쟁 측정모형을 함께 검토하십시오."))
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
  structural_canvas_register_htmt_outputs(
    output, prefix, fit_result, result_table, app_language_fn,
    table_number_fn = table_number_fn
  )
  invisible(TRUE)
}
