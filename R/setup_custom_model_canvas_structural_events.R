structural_canvas_register_interaction_events <- function(input, session, dataset_fn, selected_names_fn, variable_table_fn, app_language_fn, analysis_type, prefix, canvas_input, run_input, confirm_input, fit_result, pending_mi_rows, pending_estimator_snapshot, mark_settings_dirty, execute_analysis) {
tr <- function(en, ko) statedu_localized_text(statedu_current_language(app_language_fn), en, ko)
pending_run_snapshot <- shiny::reactiveVal(NULL)
run_confirmed_analysis <- function(snapshot, settings = list(), scope_dispatch = TRUE) {
  force(snapshot); force(settings)
  if (scope_dispatch) return(analysis_scope_run(session, prefix,
    function() run_confirmed_analysis(snapshot, settings, FALSE), paste0(prefix, "_results"), paste0(prefix, "-canvas-root")))
  snapshot <- analysis_scope_model_snapshot(snapshot, dataset_fn())
  result <- execute_analysis(snapshot, settings)
  showNotification(
    sprintf(tr("%s analysis completed.", "%s 분석이 완료되었습니다."), structural_analysis_title(analysis_type, statedu_current_language(app_language_fn))),
    type = if (isTRUE(result$converged)) "message" else "warning"
  )
  result
}
observeEvent(input[[canvas_input]], mark_settings_dirty(), ignoreInit = TRUE)
observeEvent(input[[run_input]], {
  snapshot <- input[[run_input]]
  pending_run_snapshot(snapshot)
}, ignoreInit = TRUE)
observeEvent(list(input[[paste0(prefix, "_result_coefficient")]], input[[paste0(prefix, "_result_measurement_coefficient")]]), {
  bundle <- fit_result()
  shiny::req(!is.null(bundle), !is.null(bundle$fit), !is.null(bundle$snapshot))
  coefficient <- structural_canvas_result_coefficient_mode(input[[paste0(prefix, "_result_coefficient")]] %||% bundle$result_coefficient %||% "beta_p")
  measurement_coefficient <- as.character(input[[paste0(prefix, "_result_measurement_coefficient")]] %||% bundle$result_measurement_coefficient %||% "measurement_p")
  bundle$result_coefficient <- coefficient
  bundle$result_measurement_coefficient <- measurement_coefficient
  fit_result(bundle)
  session$sendCustomMessage(
    "custom-model-canvas-result",
    local({
      result_snapshots <- structural_canvas_group_result_snapshots(
        bundle$snapshot, bundle$fit, coefficient, bundle$pls_bootstrap_result %||% NULL,
        measurement_coefficient, bundle$invariance_result %||% NULL,
        statedu_current_language(app_language_fn),
        effect_bootstrap_state = structural_canvas_effect_bootstrap_snapshot_state_from_bundle(bundle)
      )
      list(
      rootId = paste0(prefix, "-canvas-root"),
      source = bundle$snapshot,
      result = result_snapshots[[1L]]$result,
      results = result_snapshots,
      show = TRUE
      )
    })
  )
}, ignoreInit = TRUE)
request_analysis <- function(snapshot) {
  snapshot <- analysis_scope_model_snapshot(snapshot,dataset_fn())
  package <- structural_analysis_package(analysis_type)
  if (!requireNamespace(package, quietly = TRUE)) {
    showNotification(sprintf(tr("%s package is required.", "%s 패키지가 필요합니다."), package), type = "error")
  } else {
    tryCatch({
      pending_run_snapshot(snapshot)
      shiny::req(!is.null(snapshot))
      recommendation <- structural_canvas_estimator_recommendation(
        snapshot, dataset_fn(), variable_table_fn(), analysis_type,
        input[[paste0(prefix, "_estimator")]] %||% "ML"
      )
      if (isTRUE(recommendation$recommend)) {
        pending_estimator_snapshot(snapshot)
        bollen_requested <- identical(analysis_type, "cfa") && as.integer(input[[paste0(prefix, "_bollen_stine_bootstrap")]] %||% 0L) > 0L
        showModal(structural_canvas_estimator_recommendation_modal(
          recommendation, analysis_type, prefix, bollen_requested,
          statedu_current_language(app_language_fn)
        ))
        return()
      }
      run_confirmed_analysis(snapshot)
      return()
      result <- execute_analysis(snapshot)
      showNotification(
        sprintf(tr("%s analysis completed.", "%s 분석이 완료되었습니다."), structural_analysis_title(analysis_type, statedu_current_language(app_language_fn))),
        type = if (isTRUE(result$converged)) "message" else "warning"
      )
    }, error = function(error) {
      showNotification(structural_canvas_error_message(error, statedu_current_language(app_language_fn)), type = "error", duration = 8)
    })
  }
}
observeEvent(input[[confirm_input]], request_analysis(input[[confirm_input]] %||% pending_run_snapshot()), ignoreInit = TRUE)
observeEvent(input[[paste0(prefix, "_run_with_ml")]], {
  snapshot <- pending_estimator_snapshot()
  removeModal()
  shiny::req(!is.null(snapshot))
  tryCatch({
    run_confirmed_analysis(snapshot, list(estimator = "ML"))
  }, error = function(error) {
    showNotification(structural_canvas_error_message(error, statedu_current_language(app_language_fn)), type = "error", duration = 8)
  })
}, ignoreInit = TRUE)
observeEvent(input[[paste0(prefix, "_run_with_mlr")]], {
  snapshot <- pending_estimator_snapshot()
  removeModal()
  shiny::req(!is.null(snapshot))
  tryCatch({
    settings <- list(estimator = "MLR")
    if (identical(analysis_type, "cfa")) settings$bollen_stine_bootstrap <- 0L
    run_confirmed_analysis(snapshot, settings)
  }, error = function(error) {
    showNotification(structural_canvas_error_message(error, statedu_current_language(app_language_fn)), type = "error", duration = 8)
  })
}, ignoreInit = TRUE)
lapply(seq_len(100L), function(index) local({
  row_index <- index
  observeEvent(input[[paste0(prefix, "_mi_select_", row_index)]], {
    bundle <- fit_result()
    shiny::req(!is.null(bundle), !is.null(bundle$mi), nrow(bundle$mi) >= row_index)
    reuse_error <- tryCatch({
      structural_canvas_validate_holdout_reuse(bundle$mi_holdout_enabled, !is.null(bundle$holdout_comparison))
      NULL
    }, error = identity)
    if (!is.null(reuse_error)) {
      showNotification(structural_canvas_error_message(reuse_error, statedu_current_language(app_language_fn)), type = "error", duration = 12)
      return()
    }
    selected_rows <- if (identical(bundle$mi_mode %||% "theory", "theory")) seq_len(row_index) else row_index
    existing <- bundle$mi_history %||% data.frame()
    existing_signatures <- if (nrow(existing)) existing$Signature else character(0)
    selected_rows <- selected_rows[!vapply(selected_rows, function(index) structural_canvas_mi_signature(bundle$mi$lhs[[index]], bundle$mi$op[[index]], bundle$mi$rhs[[index]]) %in% existing_signatures, logical(1))]
    if (!length(selected_rows)) {
      showNotification(tr("All selected MI paths have already been applied.", "선택한 MI 경로는 모두 이미 적용되었습니다."), type = "warning")
      return()
    }
    pending_mi_rows(selected_rows)
    parameters <- vapply(selected_rows, function(index) paste(bundle$mi$lhs[[index]], bundle$mi$op[[index]], bundle$mi$rhs[[index]]), character(1))
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    skipped_details <- if ("skipped_details" %in% names(bundle$mi)) {
      unique(trimws(structural_canvas_mi_skipped_text(bundle$mi, statedu_current_language(app_language_fn))[selected_rows]))
    } else character(0)
    skipped_details <- skipped_details[!is.na(skipped_details) & nzchar(skipped_details)]
    showModal(modalDialog(
      title = tr("Document MI modification", "MI 수정 기록"),
      tags$p(paste0(tr("Paths to add: ", "추가할 경로: "), paste(parameters, collapse = ", "))),
      if (length(skipped_details)) tags$div(
        class = "alert alert-warning structural-mi-skipped-reasons",
        tags$strong(tr("Skipped candidates and reasons", "건너뛴 후보 및 사유")),
        tags$ul(lapply(skipped_details, tags$li)),
        result_note_paragraph(class = "structural-result-note", tr("These candidates were excluded from application because they failed safety diagnostics.", "이 후보들은 안전성 진단을 통과하지 못해 적용 대상에서 제외되었습니다.") )
      ),
      textAreaInput(paste0(prefix, "_mi_justification"), tr("Substantive justification", "실질적 근거"), rows = 4, placeholder = tr("Explain why freeing these parameters is theoretically defensible.", "이 모수를 자유화하는 것이 이론적으로 방어 가능한 이유를 기록하십시오.")),
      footer = tagList(modalButton(tr("Cancel", "취소")), actionButton(paste0(prefix, "_mi_confirm_apply"), tr("Apply and reanalyze", "적용 후 재분석"), class = "btn-primary")),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)
}))
observeEvent(input[[paste0(prefix, "_mi_confirm_apply")]], {
  bundle <- fit_result()
  selected_rows <- pending_mi_rows()
  shiny::req(!is.null(bundle), length(selected_rows))
  tryCatch({
    structural_canvas_validate_holdout_reuse(bundle$mi_holdout_enabled, !is.null(bundle$holdout_comparison))
    justification <- structural_canvas_validate_mi_justification(input[[paste0(prefix, "_mi_justification")]])
    snapshot <- bundle$snapshot
    for (selected_row in selected_rows) snapshot <- structural_canvas_apply_mi(snapshot, bundle$mi[selected_row, , drop = FALSE])
    settings <- bundle
    settings$comparison_type <- "mi"
    settings$comparison_label <- "Modified model"
    settings$mi_history <- structural_canvas_mi_history_rows(bundle$mi, selected_rows, bundle$mi_history %||% data.frame(), justification)
    removeModal()
    pending_mi_rows(integer(0))
    result <- execute_analysis(snapshot, settings)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    showNotification(tr("The selected MI paths were added, documented, and reanalyzed.", "선택한 MI 경로를 추가하고 기록한 뒤 재분석했습니다."), type = if (isTRUE(result$converged)) "message" else "warning")
  }, error = function(error) showNotification(structural_canvas_error_message(error, statedu_current_language(app_language_fn)), type = "error", duration = 8))
}, ignoreInit = TRUE)
observeEvent(input[[paste0(prefix, "_heywood_refit")]], {
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  showModal(modalDialog(
    title = tr("Heywood-constrained reanalysis", "Heywood 제약 재분석"),
    tags$p(tr("Fix each negative residual variance to a small positive percentage of that variable's observed variance.", "각 음수 잔차분산을 해당 변수 관측분산의 작은 양수 비율로 고정합니다.")),
    numericInput(paste0(prefix, "_heywood_percent"), tr("Observed-variance percentage", "관측분산 비율"), value = 0.1, min = 0.01, max = 5, step = 0.01),
    result_note_paragraph(class = "structural-result-note", tr("Recommended starting value: 0.1%. This is a sensitivity analysis, not an automatic correction of model misspecification.", "권장 시작값: 0.1%. 이는 민감도 분석이며 모형 부적합을 자동으로 수정하는 절차가 아닙니다.")),
    footer = tagList(modalButton(tr("Cancel", "취소")), actionButton(paste0(prefix, "_heywood_confirm"), tr("Run constrained model", "제약 모형 실행"), class = "btn-warning")),
    easyClose = TRUE
  ))
}, ignoreInit = TRUE)
observeEvent(input[[paste0(prefix, "_heywood_confirm")]], {
  bundle <- fit_result()
  shiny::req(!is.null(bundle))
  tryCatch({
    percent <- as.numeric(input[[paste0(prefix, "_heywood_percent")]] %||% 0.1)
    constraint <- structural_canvas_heywood_constraint_settings(bundle, dataset_fn(), percent)
    removeModal()
    result <- execute_analysis(bundle$snapshot, constraint$settings)
    showNotification(sprintf(tr("The constrained model fixed %s to %s%% of observed variance.", "제약 모형에서 %s의 잔차분산을 관측분산의 %s%%로 고정했습니다."), paste(constraint$variables, collapse = ", "), format(constraint$percent, trim = TRUE)), type = if (isTRUE(result$admissible)) "message" else "warning", duration = 10)
  }, error = function(error) {
    showNotification(structural_canvas_error_message(error, statedu_current_language(app_language_fn)), type = "error", duration = 10)
  })
}, ignoreInit = TRUE)
  invisible(request_analysis)
}
