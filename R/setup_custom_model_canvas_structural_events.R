structural_canvas_register_interaction_events <- function(input, session, dataset_fn, selected_names_fn, variable_table_fn, app_language_fn, analysis_type, prefix, canvas_input, confirm_input, advanced_input, fit_result, pending_mi_rows, pending_estimator_snapshot, mark_settings_dirty, execute_analysis) {
run_confirmed_analysis <- function(snapshot, settings = list()) {
  result <- execute_analysis(snapshot, settings)
  showNotification(
    if (identical(statedu_current_language(app_language_fn), "ko")) paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " 분석이 완료되었습니다.") else paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " analysis completed."),
    type = if (isTRUE(result$converged)) "message" else "warning"
  )
  result
}
observeEvent(input[[canvas_input]], mark_settings_dirty(), ignoreInit = TRUE)
observeEvent(input[[paste0(prefix, "_result_coefficient")]], {
  bundle <- fit_result()
  shiny::req(!is.null(bundle), !is.null(bundle$fit), !is.null(bundle$snapshot))
  coefficient <- structural_canvas_result_coefficient_mode(input[[paste0(prefix, "_result_coefficient")]] %||% bundle$result_coefficient %||% "beta_p")
  bundle$result_coefficient <- coefficient
  fit_result(bundle)
  session$sendCustomMessage(
    "custom-model-canvas-result",
    list(
      rootId = paste0(prefix, "-canvas-root"),
      source = bundle$snapshot,
      result = structural_canvas_result_snapshot(bundle$snapshot, bundle$fit, coefficient),
      show = TRUE
    )
  )
}, ignoreInit = TRUE)
observeEvent(input[[confirm_input]], {
  package <- structural_analysis_package(analysis_type)
  if (!requireNamespace(package, quietly = TRUE)) {
    showNotification(sprintf("%s package is required.", package), type = "error")
  } else {
    tryCatch({
      snapshot <- input[[confirm_input]]
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
        if (identical(statedu_current_language(app_language_fn), "ko")) paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " 분석이 완료되었습니다.") else paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " analysis completed."),
        type = if (isTRUE(result$converged)) "message" else "warning"
      )
    }, error = function(error) {
      showNotification(structural_canvas_error_message(error, statedu_current_language(app_language_fn)), type = "error", duration = 8)
    })
  }
}, ignoreInit = TRUE)
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
      showNotification("All selected MI paths have already been applied.", type = "warning")
      return()
    }
    pending_mi_rows(selected_rows)
    parameters <- vapply(selected_rows, function(index) paste(bundle$mi$lhs[[index]], bundle$mi$op[[index]], bundle$mi$rhs[[index]]), character(1))
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    showModal(modalDialog(
      title = if (ko) "MI 수정 기록" else "Document MI modification",
      tags$p(paste0(if (ko) "추가할 경로: " else "Paths to add: ", paste(parameters, collapse = ", "))),
      textAreaInput(paste0(prefix, "_mi_justification"), if (ko) "실질적 근거" else "Substantive justification", rows = 4, placeholder = if (ko) "이 모수를 자유화하는 것이 이론적으로 방어 가능한 이유를 기록하십시오." else "Explain why freeing these parameters is theoretically defensible."),
      footer = tagList(modalButton(if (ko) "취소" else "Cancel"), actionButton(paste0(prefix, "_mi_confirm_apply"), if (ko) "적용 후 재분석" else "Apply and reanalyze", class = "btn-primary")),
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
    showNotification(if (ko) "선택한 MI 경로를 추가하고 기록한 뒤 재분석했습니다." else "The selected MI paths were added, documented, and reanalyzed.", type = if (isTRUE(result$converged)) "message" else "warning")
  }, error = function(error) showNotification(structural_canvas_error_message(error, statedu_current_language(app_language_fn)), type = "error", duration = 8))
}, ignoreInit = TRUE)
observeEvent(input[[paste0(prefix, "_heywood_refit")]], {
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  showModal(modalDialog(
    title = if (ko) "Heywood 제약 재분석" else "Heywood-constrained reanalysis",
    tags$p(if (ko) "각 음수 잔차분산을 해당 변수 관측분산의 작은 양수 비율로 고정합니다." else "Fix each negative residual variance to a small positive percentage of that variable's observed variance."),
    numericInput(paste0(prefix, "_heywood_percent"), if (ko) "관측분산 비율" else "Observed-variance percentage", value = 0.1, min = 0.01, max = 5, step = 0.01),
    tags$p(class = "structural-result-note", if (ko) "권장 시작값: 0.1%. 이는 민감도 분석이며 모형 부적합을 자동으로 수정하는 절차가 아닙니다." else "Recommended starting value: 0.1%. This is a sensitivity analysis, not an automatic correction of model misspecification."),
    footer = tagList(modalButton(if (ko) "취소" else "Cancel"), actionButton(paste0(prefix, "_heywood_confirm"), if (ko) "제약 모형 실행" else "Run constrained model", class = "btn-warning")),
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
    showNotification(paste0("The constrained model fixed ", paste(constraint$variables, collapse = ", "), " to ", format(constraint$percent, trim = TRUE), "% of observed variance."), type = if (isTRUE(result$admissible)) "message" else "warning", duration = 10)
  }, error = function(error) {
    showNotification(structural_canvas_error_message(error, statedu_current_language(app_language_fn)), type = "error", duration = 10)
  })
}, ignoreInit = TRUE)
observeEvent(input[[advanced_input]], {
  request <- input[[advanced_input]] %||% list()
  action <- as.character(request$action %||% "")
  candidates <- as.character(selected_names_fn() %||% character(0))
  ko <- identical(statedu_current_language(app_language_fn), "ko")
  if (identical(action, "multiGroup")) {
    showModal(modalDialog(
      title = if (ko) "다집단 분석 설정" else "Multigroup Analysis",
      selectInput(paste0(prefix, "_group_variable"), if (ko) "집단변수" else "Grouping variable", choices = candidates),
      helpText(if (ko) "집단 간 측정모형과 구조경로의 차이를 검정합니다." else "Compare measurement models and structural paths across groups."),
      footer = modalButton(if (ko) "닫기" else "Close"),
      easyClose = TRUE
    ))
  } else if (identical(action, "moderator")) {
    showModal(modalDialog(
      title = if (ko) "조절효과 설정" else "Moderation Settings",
      selectInput(paste0(prefix, "_moderator_variable"), if (ko) "조절변수" else "Moderator", choices = candidates),
      selectInput(paste0(prefix, "_moderated_predictor"), if (ko) "독립변수" else "Predictor", choices = candidates),
      selectInput(paste0(prefix, "_moderated_outcome"), if (ko) "종속변수" else "Outcome", choices = candidates),
      helpText(if (ko) "선택한 경로에 조절효과를 지정합니다." else "Assign a moderation effect to the selected path."),
      footer = modalButton(if (ko) "닫기" else "Close"),
      easyClose = TRUE
    ))
  }
}, ignoreInit = TRUE)
  invisible(TRUE)
}
