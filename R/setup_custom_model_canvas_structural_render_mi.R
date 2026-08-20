structural_canvas_register_mi_render_outputs <- function(output, prefix, fit_result, result_table, app_language_fn) {
output[[paste0(prefix, "_result_mi")]] <- renderUI({
  table <- result_table("mi")
  if (!nrow(table)) return(NULL)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  theory_mi <- identical(fit_result()$mi_mode %||% "theory", "theory")
  skipped_details <- attr(table, "skipped_details", exact = TRUE)
  display_columns <- intersect(c(
    "Step", "Skipped unsafe", "Covariance", "MI", "MI p", "BH-adjusted p", "EPC", "Std. EPC", "CFI", "TLI", "SRMR", "RMSEA"
  ), names(table))
  display <- table[, display_columns, drop = FALSE]
  tagList(
  tags$div(class = "table-responsive structural-landscape-table-wrap structural-mi-table-scroll", tags$table(
      class = paste("table table-striped table-bordered structural-result-table structural-landscape-table structural-mi-table", if (theory_mi) "structural-mi-theory-table" else "structural-mi-free-table"),
      tags$thead(tags$tr(
        lapply(names(display), tags$th),
        if (theory_mi) tags$th(if (ko) "선택" else "Select")
      )),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) {
        tags$tr(
          lapply(as.character(display[index, , drop = TRUE]), structural_canvas_html_cell),
          if (theory_mi) tags$td(actionButton(
            paste0(prefix, "_mi_select_", index),
            if (ko) "선택" else "Select",
            class = "btn-sm structural-mi-select-button"
          ))
        )
      }))
    )),
  if (theory_mi && is.data.frame(skipped_details) && nrow(skipped_details)) tagList(
    tags$h5(if (ko) "건너뛴 MI 후보 상세" else "Skipped MI candidate details"),
    structural_canvas_basic_html_table(skipped_details, class = "table table-striped table-bordered structural-mi-skipped-details-table")
  ),
  tags$p(class = "structural-result-note", if (ko) "MI p는 각 수정지수를 비척도화된 점근적 1 자유도 카이제곱 검정으로 취급합니다. BH-adjusted p는 화면에 표시된 MI 및 이론 필터를 적용하기 전 lavaan 후보 수정 전체의 false-discovery rate를 조절합니다." else "MI p treats each modification index as an unscaled asymptotic 1-df chi-square test. BH-adjusted p controls the false-discovery rate across all finite lavaan candidate modifications before the displayed MI and theory filters."),
  tags$p(class = "structural-result-note", if (ko) "MLR 또는 WLSMV에서 이 p값은 별도의 robust/scaled score-test 보정이 아니며 탐색적 참고값으로 보아야 합니다." else "For MLR or WLSMV, these derived p values are not a separate robust/scaled score-test correction and should be treated as exploratory reference values."),
  tags$p(class = "structural-result-note", if (ko) "EPC는 고정 모수를 자유화했을 때 기대되는 비표준화 모수 변화입니다. Std. EPC는 lavaan의 완전표준화 기대 변화(sepc.all)입니다. MI 순위만 보지 말고 방향과 크기를 함께 검토하십시오." else "EPC is the expected unstandardized parameter change if the fixed parameter is freed; Std. EPC is lavaan's fully standardized expected change (sepc.all). Consider direction and magnitude rather than MI rank alone."),
  if (theory_mi) tags$p(class = "structural-result-note", if (ko) "각 Step은 순차적입니다. 표시된 경로를 추가하고 모형을 다시 적합한 뒤, 다음 행의 MI, 다중검정 계열, EPC, 누적 적합도를 그 갱신된 모형에서 다시 계산합니다. 행들은 변경되지 않은 하나의 모형에서 나온 동시 후보가 아닙니다." else "Each Step is sequential: the displayed path is added, the model is refitted, and MI, multiplicity family, EPC, and cumulative fit for the next row are recomputed from that updated model. Rows are not simultaneous candidates from one unchanged model."),
  if (theory_mi) tags$p(class = "structural-result-note", if (ko) "Skipped unsafe는 더 높은 순위였지만 수렴 실패, post.check 실패, 음의 분산, 비양정 또는 경계 잔차/잠재/모수 공분산행렬, 잘못된 자유도, 또는 |잠재상관| >= 1 때문에 거부된 후보 수입니다. Skipped details는 거부된 각 경로와 진단 사유를 기록하며, 거부된 후보는 자동 적용 대상으로 제공하지 않습니다." else "Skipped unsafe counts higher-ranked candidates rejected for nonconvergence, post.check failure, negative variance, a non-positive-definite or boundary residual/latent/parameter covariance matrix, invalid df, or |latent correlation| >= 1. Skipped details records each rejected path and diagnostic reason; a skipped candidate is not offered for automatic application."),
  tags$p(class = "structural-result-note", if (ko) "비보정 p값이나 보정 p값만으로는 수정을 정당화할 수 없습니다. 효과크기(EPC/표준화 EPC), 잔차 진단, 허용성, 이론, 가능하면 독립 검증을 함께 사용하십시오." else "Neither an unadjusted nor adjusted p value justifies a modification. Use effect size (EPC/standardized EPC), residual diagnostics, admissibility, theory, and preferably independent validation."))
})
output[[paste0(prefix, "_result_mi_history")]] <- renderUI({
  bundle <- fit_result()
  history <- bundle$mi_history %||% data.frame()
  if (!nrow(history)) return(NULL)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  display <- history[, setdiff(names(history), "Signature"), drop = FALSE]
  for (name in intersect(c("MI", "EPC", "CFI", "TLI", "RMSEA", "SRMR"), names(display))) {
    display[[name]] <- vapply(display[[name]], format_decimal3, character(1))
  }
  display$Justification[!nzchar(display$Justification)] <- if (ko) "제공되지 않음" else "Not provided"
  gate <- bundle$mi_validation_gate %||% structural_canvas_mi_validation_gate(TRUE, bundle$mi_holdout_enabled, bundle$holdout_comparison)
  div(class = "result-section regression-result-panel structural-mi-history-result",
    h4(if (ko) "MI 수정 이력" else "MI modification history"),
    tags$p(class = "structural-result-note", paste0(if (ko) "검증 상태: " else "Validation status: ", gate$label)),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    ),
    tags$p(class = "structural-result-note", if (ko) "MI, EPC, 누적 적합도 값은 해당 경로를 선택한 시점의 값입니다. 근거란에는 각 모수를 자유화한 실질적 이유를 기록해야 합니다." else "MI, EPC, and cumulative fit values are those available when the path was selected. The justification should document the substantive reason for freeing each parameter."),
    tags$p(class = "structural-result-note", if (ko) "MI 기반 수정은 탐색적 수정 모형이며 독립 표본에서 교차검증해야 합니다." else "MI-driven modifications are exploratory and should be cross-validated in an independent sample.")
  )
})
output[[paste0(prefix, "_result_mi_holdout")]] <- renderUI({
  bundle <- fit_result()
  if (!isTRUE(bundle$mi_holdout_enabled)) return(NULL)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  comparison <- bundle$holdout_comparison %||% NULL
  if (is.null(comparison)) return(tags$div(class = "result-section regression-result-panel",
    tags$h4(if (ko) "MI 홀드아웃 검증" else "MI holdout validation"),
    tags$p(paste0(if (ko) "탐색 표본 N = " else "Exploration N = ", nrow(bundle$analysis_data), if (ko) "; 예약 검증 표본 N = " else "; reserved validation N = ", nrow(bundle$validation_data), ".")),
    tags$p(class = "structural-result-note", if (ko) "MI 후보와 현재 표시된 CFA 추정값은 탐색 표본만 사용한 결과입니다. MI 경로를 적용한 뒤 검증 결과가 표시됩니다." else "MI candidates and all currently displayed CFA estimates are based only on the exploration sample. Validation results will appear after an MI path is applied.")
  ))
  table <- comparison$table
  for (name in c("Chisq", "df", "CFI", "TLI", "SRMR", "RMSEA")) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
  table$p <- vapply(table$p, format_p, character(1))
  changes <- comparison$changes
  for (name in names(changes)[vapply(changes, is.numeric, logical(1)) & names(changes) != "DeltaP"]) changes[[name]] <- vapply(changes[[name]], format_decimal3, character(1))
  changes$DeltaP <- vapply(changes$DeltaP, format_p, character(1))
  div(class = "result-section regression-result-panel structural-mi-holdout-result",
    h4(if (ko) "MI 홀드아웃 검증" else "MI holdout validation"),
    tags$p(class = "structural-result-note", paste0(if (ko) "검증 상태: " else "Validation status: ", (bundle$mi_validation_gate %||% structural_canvas_mi_validation_gate(TRUE, TRUE, comparison))$label)),
    tags$p(paste0(if (ko) "탐색 행 수 = " else "Exploration rows = ", nrow(bundle$analysis_data), if (ko) "; 예약 검증 행 수 = " else "; reserved validation rows = ", comparison$validation_n_raw, if (ko) "; 사용된 검증 N = " else "; validation N used = ", paste(unique(comparison$validation_n_used), collapse = ", "), if (ko) "; 분할 seed = " else "; split seed = ", bundle$mi_holdout_seed, ".")),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
    )),
    tags$h5(if (ko) "검증표본 변화: 수정 모형 - 기존 모형" else "Validation-sample change: modified minus original"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(changes), tags$th))),
      tags$tbody(tags$tr(lapply(as.character(changes[1L, ]), tags$td)))
    )),
    if (any(!comparison$table$Admissible)) tags$p(class = "structural-result-note", if (ko) "검증표본의 한 모형 또는 두 모형이 주 CFA와 동일한 전체 admissibility 점검을 통과하지 못했습니다. 검증표본 변화 통계와 공식 차이 검정은 표시하지 않으며, 이 수정은 반복검증된 것으로 해석하면 안 됩니다." else "One or both validation-sample models failed the same full admissibility checks as the main CFA. Validation-sample change statistics and the formal difference test are suppressed; the modification must not be treated as replicated."),
    tags$p(class = "structural-result-note", if (ko) "MI 경로는 탐색 표본에서만 선택되었습니다. 위 표는 예약된 검증 표본에서 두 모형을 독립적으로 다시 적합한 결과입니다. 적합도 개선의 반복은 안정성을 뒷받침하지만 실질적 근거를 대체하지 않으며, 반복되지 않으면 표본 특이적 수정일 가능성이 큽니다." else "The MI path was selected only in the exploration sample. The table above refits both models independently in the reserved validation sample. Replication of improved fit supports stability but does not replace substantive justification; failure to replicate indicates likely sample-specific modification."),
    tags$p(class = "structural-result-note", if (ko) "검증 표본은 이제 공개되어 잠겼습니다. 이 분할에서는 추가 MI 변경을 비활성화합니다. 다른 수정 모형을 평가하려면 새 분할 seed로 새 분석을 시작하십시오." else "The validation sample is now unblinded and locked. Further MI changes are disabled for this split; start a new analysis with a newly chosen split seed to evaluate a different modified model.")
  )
})
  invisible(TRUE)
}
