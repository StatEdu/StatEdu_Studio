structural_canvas_mi_validation_label <- function(gate, language) {
  korean <- c(not_applicable = "해당 없음", unvalidated = "탐색적 - 독립 검증 미실시",
    reserved = "탐색적 - 예약 홀드아웃 평가 전", holdout_inadmissible = "탐색적 - 홀드아웃 평가 불확정(허용 불가능한 모형)",
    holdout_evaluated = "탐색적 - 사전 지정한 예약 홀드아웃에서 1회 평가됨; 모든 변화를 검토하십시오")
  if (is.null(gate$code) || !gate$code %in% names(korean)) return(gate$label)
  statedu_localized_text(language, gate$label, unname(korean[[gate$code]]))
}

structural_canvas_register_mi_render_outputs <- function(output, prefix, fit_result, result_table, app_language_fn) {
output[[paste0(prefix, "_result_mi")]] <- renderUI({
  table <- result_table("mi")
  if (!nrow(table)) return(NULL)
  language <- statedu_current_language(app_language_fn)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  theory_mi <- identical(fit_result()$mi_mode %||% "theory", "theory")
  skipped_details <- attr(table, "skipped_details", exact = TRUE)
  skipped_source <- attr(table, "skipped_source", exact = TRUE)
  if (is.data.frame(skipped_source) && is.data.frame(skipped_details) && nrow(skipped_details)) {
    translated <- structural_canvas_mi_skipped_text(skipped_source, language)
    legacy <- as.character(skipped_source$skipped_details %||% rep("", nrow(skipped_source)))
    skipped_details[["Skipped details"]] <- translated[!is.na(legacy) & nzchar(trimws(legacy))]
    attr(skipped_details, "result_user_columns") <- names(skipped_details)
  }
  display_columns <- intersect(c(
    "Step", "Skipped unsafe", "Covariance", "MI", "MI p", "BH-adjusted p", "EPC", "Std. EPC", "CFI", "TLI", "SRMR", "RMSEA"
  ), names(table))
  display <- table[, display_columns, drop = FALSE]
  attr(display, "result_user_columns") <- names(display)
  display <- structural_canvas_localize_appendix_table(display, language)
  tagList(
  structural_canvas_table_sheet(tags$div(class = "table-responsive structural-landscape-table-wrap structural-mi-table-scroll", tags$table(
      class = paste("table table-striped table-bordered structural-result-table structural-landscape-table structural-mi-table", if (theory_mi) "structural-mi-theory-table" else "structural-mi-free-table"),
      tags$thead(tags$tr(
        lapply(names(display), tags$th),
        if (theory_mi) tags$th(tr("Select", "선택"))
      )),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) {
        tags$tr(
          lapply(as.character(display[index, , drop = TRUE]), structural_canvas_html_cell),
          if (theory_mi) tags$td(actionButton(
            paste0(prefix, "_mi_select_", index),
            tr("Select", "선택"),
            class = "btn-sm structural-mi-select-button"
          ))
        )
      }))
    )), table = display, role = "appendix", orientation = "landscape", language = language),
  if (theory_mi && is.data.frame(skipped_details) && nrow(skipped_details)) tagList(
    tags$h5(statedu_localized_text(language, "Skipped MI candidate details", "건너뛴 MI 후보 상세")),
    structural_canvas_basic_html_table(skipped_details, class = "table table-striped table-bordered structural-mi-skipped-details-table", language = language)
  ),
  result_note_paragraph(class = "structural-result-note", tr("MI p treats each modification index as an unscaled asymptotic 1-df chi-square test. BH-adjusted p controls the false-discovery rate across all finite lavaan candidate modifications before the displayed MI and theory filters.", "MI p는 각 수정지수를 비척도화된 점근적 1 자유도 카이제곱 검정으로 취급합니다. BH-adjusted p는 화면에 표시된 MI 및 이론 필터를 적용하기 전 lavaan 후보 수정 전체의 false-discovery rate를 조절합니다.")),
  result_note_paragraph(class = "structural-result-note", tr("For MLR or WLSMV, these derived p values are not a separate robust/scaled score-test correction and should be treated as exploratory reference values.", "MLR 또는 WLSMV에서 이 p값은 별도의 robust/scaled score-test 보정이 아니며 탐색적 참고값으로 보아야 합니다.")),
  result_note_paragraph(class = "structural-result-note", tr("EPC is the expected unstandardized parameter change if the fixed parameter is freed; Std. EPC is lavaan's fully standardized expected change (sepc.all). Consider direction and magnitude rather than MI rank alone.", "EPC는 고정 모수를 자유화했을 때 기대되는 비표준화 모수 변화입니다. Std. EPC는 lavaan의 완전표준화 기대 변화(sepc.all)입니다. MI 순위만 보지 말고 방향과 크기를 함께 검토하십시오.")),
  if (theory_mi) result_note_paragraph(class = "structural-result-note", tr("Each Step is sequential: the displayed path is added, the model is refitted, and MI, multiplicity family, EPC, and cumulative fit for the next row are recomputed from that updated model. Rows are not simultaneous candidates from one unchanged model.", "각 Step은 순차적입니다. 표시된 경로를 추가하고 모형을 다시 적합한 뒤, 다음 행의 MI, 다중검정 계열, EPC, 누적 적합도를 그 갱신된 모형에서 다시 계산합니다. 행들은 변경되지 않은 하나의 모형에서 나온 동시 후보가 아닙니다.")),
  if (theory_mi) result_note_paragraph(class = "structural-result-note", tr("Skipped unsafe counts higher-ranked candidates rejected for nonconvergence, post.check failure, negative variance, a non-positive-definite or boundary residual/latent/parameter covariance matrix, invalid df, or |latent correlation| >= 1. Skipped details records each rejected path and diagnostic reason; a skipped candidate is not offered for automatic application.", "Skipped unsafe는 더 높은 순위였지만 수렴 실패, post.check 실패, 음의 분산, 비양정 또는 경계 잔차/잠재/모수 공분산행렬, 잘못된 자유도, 또는 |잠재상관| >= 1 때문에 거부된 후보 수입니다. Skipped details는 거부된 각 경로와 진단 사유를 기록하며, 거부된 후보는 자동 적용 대상으로 제공하지 않습니다.")),
  result_note_paragraph(class = "structural-result-note", tr("Neither an unadjusted nor adjusted p value justifies a modification. Use effect size (EPC/standardized EPC), residual diagnostics, admissibility, theory, and preferably independent validation.", "비보정 p값이나 보정 p값만으로는 수정을 정당화할 수 없습니다. 효과크기(EPC/표준화 EPC), 잔차 진단, 허용성, 이론, 가능하면 독립 검증을 함께 사용하십시오.")))
})
output[[paste0(prefix, "_result_mi_history")]] <- renderUI({
  bundle <- fit_result()
  history <- bundle$mi_history %||% data.frame()
  if (!nrow(history)) return(NULL)
  language <- statedu_current_language(app_language_fn)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  display <- history[, setdiff(names(history), "Signature"), drop = FALSE]
  for (name in intersect(c("MI", "EPC", "CFI", "TLI", "RMSEA", "SRMR"), names(display))) {
    display[[name]] <- vapply(display[[name]], format_decimal3, character(1))
  }
  display$Justification[!nzchar(display$Justification)] <- tr("Not provided", "제공되지 않음")
  names(display)[names(display) == "Justification"] <- tr("Justification", "수정 근거")
  attr(display, "result_user_columns") <- names(display)
  gate <- bundle$mi_validation_gate %||% structural_canvas_mi_validation_gate(TRUE, bundle$mi_holdout_enabled, bundle$holdout_comparison)
  div(class = "result-section regression-result-panel structural-mi-history-result",
    h4(tr("MI modification history", "MI 수정 이력")),
    result_note_paragraph(class = "structural-result-note", paste0(tr("Validation status: ", "검증 상태: "), structural_canvas_mi_validation_label(gate, language))),
    structural_canvas_basic_html_table(display, role = "appendix", orientation = "auto", language = language),
    result_note_paragraph(class = "structural-result-note", tr("MI, EPC, and cumulative fit values are those available when the path was selected. The justification should document the substantive reason for freeing each parameter.", "MI, EPC, 누적 적합도 값은 해당 경로를 선택한 시점의 값입니다. 근거란에는 각 모수를 자유화한 실질적 이유를 기록해야 합니다.")),
    result_note_paragraph(class = "structural-result-note", tr("MI-driven modifications are exploratory and should be cross-validated in an independent sample.", "MI 기반 수정은 탐색적 수정 모형이며 독립 표본에서 교차검증해야 합니다."))
  )
})
output[[paste0(prefix, "_result_mi_holdout")]] <- renderUI({
  bundle <- fit_result()
  if (!isTRUE(bundle$mi_holdout_enabled)) return(NULL)
  language <- statedu_current_language(app_language_fn)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  comparison <- bundle$holdout_comparison %||% NULL
  if (is.null(comparison)) return(tags$div(class = "result-section regression-result-panel",
    tags$h4(tr("MI holdout validation", "MI 홀드아웃 검증")),
    tags$p(sprintf(tr("Exploration N = %s; reserved validation N = %s.", "탐색 표본 N = %s; 예약 검증 표본 N = %s."), nrow(bundle$analysis_data), nrow(bundle$validation_data))),
    result_note_paragraph(class = "structural-result-note", tr("MI candidates and all currently displayed CFA estimates are based only on the exploration sample. Validation results will appear after an MI path is applied.", "MI 후보와 현재 표시된 CFA 추정값은 탐색 표본만 사용한 결과입니다. MI 경로를 적용한 뒤 검증 결과가 표시됩니다."))
  ))
  table <- comparison$table
  for (name in c("Chisq", "df", "CFI", "TLI", "SRMR", "RMSEA")) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
  table$p <- vapply(table$p, format_p, character(1))
  changes <- comparison$changes
  for (name in names(changes)[vapply(changes, is.numeric, logical(1)) & names(changes) != "DeltaP"]) changes[[name]] <- vapply(changes[[name]], format_decimal3, character(1))
  changes$DeltaP <- vapply(changes$DeltaP, format_p, character(1))
  table[["Admissibility reasons"]] <- structural_canvas_holdout_reason_text(comparison, language)
  names(table)[names(table) == "N used"] <- tr("N used", "사용 N")
  names(table)[names(table) == "Admissibility reasons"] <- tr("Admissibility reasons", "허용성 사유")
  attr(table, "result_user_columns") <- tr("Admissibility reasons", "허용성 사유")
  if ("Comparison status" %in% names(changes)) {
    statuses <- c("Both validation models admissible" = "두 검증 모형 모두 허용 가능",
      "Suppressed because one or both validation models are inadmissible" = "한 모형 또는 두 검증 모형이 허용 불가능하여 표시하지 않음")
    changes[["Comparison status"]] <- vapply(changes[["Comparison status"]], function(value)
      if (value %in% names(statuses)) tr(value, unname(statuses[[value]])) else value, character(1))
  }
  div(class = "result-section regression-result-panel structural-mi-holdout-result",
    h4(tr("MI holdout validation", "MI 홀드아웃 검증")),
    result_note_paragraph(class = "structural-result-note", paste0(tr("Validation status: ", "검증 상태: "), structural_canvas_mi_validation_label(bundle$mi_validation_gate %||% structural_canvas_mi_validation_gate(TRUE, TRUE, comparison), language))),
    tags$p(sprintf(tr("Exploration rows = %s; reserved validation rows = %s; validation N used = %s; split seed = %s.", "탐색 행 수 = %s; 예약 검증 행 수 = %s; 사용된 검증 N = %s; 분할 seed = %s."), nrow(bundle$analysis_data), comparison$validation_n_raw, paste(unique(comparison$validation_n_used), collapse = ", "), bundle$mi_holdout_seed)),
    structural_canvas_basic_html_table(table, role = "appendix", orientation = "auto", language = language),
    tags$h5(tr("Validation-sample change: modified minus original", "검증표본 변화: 수정 모형 - 기존 모형")),
    structural_canvas_basic_html_table(changes[1L, , drop = FALSE], role = "appendix", orientation = "auto", language = language),
    if (any(!comparison$table$Admissible)) result_note_paragraph(class = "structural-result-note", tr("One or both validation-sample models failed the same full admissibility checks as the main CFA. Validation-sample change statistics and the formal difference test are suppressed; the modification must not be treated as replicated.", "검증표본의 한 모형 또는 두 모형이 주 CFA와 동일한 전체 admissibility 점검을 통과하지 못했습니다. 검증표본 변화 통계와 공식 차이 검정은 표시하지 않으며, 이 수정은 반복검증된 것으로 해석하면 안 됩니다.")),
    result_note_paragraph(class = "structural-result-note", tr("The MI path was selected only in the exploration sample. The table above refits both models independently in the reserved validation sample. Replication of improved fit supports stability but does not replace substantive justification; failure to replicate indicates likely sample-specific modification.", "MI 경로는 탐색 표본에서만 선택되었습니다. 위 표는 예약된 검증 표본에서 두 모형을 독립적으로 다시 적합한 결과입니다. 적합도 개선의 반복은 안정성을 뒷받침하지만 실질적 근거를 대체하지 않으며, 반복되지 않으면 표본 특이적 수정일 가능성이 큽니다.")),
    result_note_paragraph(class = "structural-result-note", tr("The validation sample is now unblinded and locked. Further MI changes are disabled for this split; start a new analysis with a newly chosen split seed to evaluate a different modified model.", "검증 표본은 이제 공개되어 잠겼습니다. 이 분할에서는 추가 MI 변경을 비활성화합니다. 다른 수정 모형을 평가하려면 새 분할 seed로 새 분석을 시작하십시오."))
  )
})
  invisible(TRUE)
}
