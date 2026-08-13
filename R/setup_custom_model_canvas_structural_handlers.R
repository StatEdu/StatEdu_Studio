register_structural_equation_canvas_handlers <- function(input, output, session, dataset_fn, selected_names_fn, variable_table_fn, labels_fn, category_table_fn, mark_settings_dirty, app_language_fn = NULL) {
  lapply(c("cfa", "cbsem", "plssem"), function(analysis_type) {
    prefix <- structural_analysis_prefix(analysis_type)
    canvas_input <- paste0(prefix, "_canvas_state")
    canvas_output <- paste0(prefix, "_canvas_setup")
    run_input <- paste0(prefix, "_canvas_run_request")
    confirm_input <- paste0(prefix, "_canvas_run_confirm")
    advanced_input <- paste0(prefix, "_canvas_advanced_request")
    fit_result <- reactiveVal(NULL)
    pending_mi_rows <- reactiveVal(integer(0))
    pending_estimator_snapshot <- reactiveVal(NULL)
    if (identical(analysis_type, "cfa")) output[[paste0(prefix, "_download_reproducibility")]] <- downloadHandler(
      filename = function() paste0("cfa-analysis-record-", format(Sys.Date(), "%Y%m%d"), ".txt"),
      contentType = "text/plain; charset=utf-8",
      content = function(file) {
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        writeLines(structural_canvas_reproducibility_record(bundle), file, useBytes = TRUE)
      }
    )
    if (identical(analysis_type, "cfa")) output[[paste0(prefix, "_download_tables")]] <- downloadHandler(
      filename = function() paste0("cfa-result-tables-", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        shiny::req(requireNamespace("openxlsx", quietly = TRUE))
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        sheets <- structural_canvas_result_workbook_sheets(bundle, result_table)
        structural_canvas_write_result_workbook(sheets, file)
      }
    )
    if (identical(analysis_type, "cfa")) observe({
      data <- dataset_fn()
      choices <- names(data %||% data.frame())
      current <- as.character(input[[paste0(prefix, "_invariance_group")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_invariance_group"), choices = choices, selected = if (current %in% choices) current else "")
    })
    result_table <- function(kind) {
      structural_canvas_result_table(kind, fit_result, analysis_type, labels_fn, app_language_fn)
    }
    structural_canvas_register_result_outputs(
      input, output, prefix, canvas_output, analysis_type,
      selected_names_fn, variable_table_fn, labels_fn, app_language_fn, fit_result, result_table
    )
    execute_analysis <- function(snapshot, settings = NULL) {
      structural_canvas_execute_analysis(
        snapshot, settings, input, session, dataset_fn, variable_table_fn, analysis_type, prefix, fit_result
      )
    }
    run_confirmed_analysis <- function(snapshot, settings = list()) {
      result <- execute_analysis(snapshot, settings)
      showNotification(
        if (identical(statedu_current_language(app_language_fn), "ko")) paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " 遺꾩꽍???꾨즺?섏뿀?듬땲??") else paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " analysis completed."),
        type = if (isTRUE(result$converged)) "message" else "warning"
      )
      result
    }
    observeEvent(input[[canvas_input]], mark_settings_dirty(), ignoreInit = TRUE)
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
            diagnosis <- recommendation$diagnosis
            bollen_requested <- identical(analysis_type, "cfa") && as.integer(input[[paste0(prefix, "_bollen_stine_bootstrap")]] %||% 0L) > 0L
            ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
            showModal(modalDialog(
              title = if (ko) "추정량 권고" else "Estimator recommendation",
              tags$p(if (ko) "Mardia 진단에서 연속형 지표의 비정규성이 확인되었습니다. 이 모형을 적합하기 전에 강건 MLR 사용을 권장합니다." else "Mardia diagnostics flagged nonnormal continuous indicators. Robust MLR is recommended before fitting this model."),
              tags$p(paste0(
                if (ko) "Mardia 왜도 p = " else "Mardia skewness p = ", format_p(diagnosis$skew_p),
                if (ko) "; 첨도 p = " else "; kurtosis p = ", format_p(diagnosis$kurtosis_p),
                if (ko) "; 완전 사례 = " else "; complete cases = ", diagnosis$n, if (ko) " / " else " of ", diagnosis$original_n, "."
              )),
              if (bollen_requested) tags$p(class = "structural-result-note", if (ko) "Bollen-Stine 부트스트랩은 ML에서만 사용할 수 있습니다. MLR을 선택하면 Bollen-Stine 부트스트랩 없이 모형을 실행합니다." else "Bollen-Stine bootstrap is available only for ML; choosing MLR will run the model without Bollen-Stine bootstrap."),
              footer = tagList(
                modalButton(if (ko) "취소" else "Cancel"),
                actionButton(paste0(prefix, "_run_with_ml"), if (ko) "ML로 실행" else "Run with ML", class = "btn-default"),
                actionButton(paste0(prefix, "_run_with_mlr"), if (ko) "MLR로 실행" else "Run with MLR", class = "btn-primary")
              ),
              easyClose = TRUE
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
          showNotification(conditionMessage(error), type = "error", duration = 8)
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
        showNotification(conditionMessage(error), type = "error", duration = 8)
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
        showNotification(conditionMessage(error), type = "error", duration = 8)
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
          showNotification(conditionMessage(reuse_error), type = "error", duration = 12)
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
        snapshot <- bundle$snapshot
        for (selected_row in selected_rows) snapshot <- structural_canvas_apply_mi(snapshot, bundle$mi[selected_row, , drop = FALSE])
        settings <- bundle
        settings$comparison_type <- "mi"
        settings$comparison_label <- "Modified model"
        settings$mi_history <- structural_canvas_mi_history_rows(bundle$mi, selected_rows, bundle$mi_history %||% data.frame(), input[[paste0(prefix, "_mi_justification")]] %||% "")
        removeModal()
        pending_mi_rows(integer(0))
        result <- execute_analysis(snapshot, settings)
        ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
        showNotification(if (ko) "선택한 MI 경로를 추가하고 기록한 뒤 재분석했습니다." else "The selected MI paths were added, documented, and reanalyzed.", type = if (isTRUE(result$converged)) "message" else "warning")
      }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = 8))
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
        if (!toupper(as.character(bundle$estimator %||% "ML")) %in% c("ML", "MLR") || length(bundle$ordered %||% character(0))) {
          stop("Heywood-constrained reanalysis is available only for continuous indicators estimated with ML or MLR.")
        }
        variables <- as.character((bundle$baseline_diagnostics %||% bundle$diagnostics)$negative_residuals %||% character(0))
        if (!length(variables)) stop("No negative residual variances were found in the original model.")
        percent <- as.numeric(input[[paste0(prefix, "_heywood_percent")]] %||% 0.1)
        if (!is.finite(percent) || percent < 0.01 || percent > 5) stop("Enter a percentage between 0.01 and 5.")
        data <- dataset_fn()
        observed_variances <- vapply(variables, function(name) stats::var(data[[name]], na.rm = TRUE), numeric(1))
        if (any(!is.finite(observed_variances) | observed_variances <= 0)) stop("A positive observed variance is required for every Heywood indicator.")
        fixes <- observed_variances * percent / 100
        names(fixes) <- variables
        settings <- bundle
        settings$residual_variance_fixes <- fixes
        settings$comparison_label <- "Heywood-constrained model"
        settings$comparison_type <- "heywood"
        removeModal()
        result <- execute_analysis(bundle$snapshot, settings)
        showNotification(paste0("The constrained model fixed ", paste(variables, collapse = ", "), " to ", format(percent, trim = TRUE), "% of observed variance."), type = if (isTRUE(result$admissible)) "message" else "warning", duration = 10)
      }, error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 10)
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
    register_analysis_data_viewer_handlers(
      input = input,
      output = output,
      prefix = prefix,
      title = paste(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), if (identical(statedu_current_language(app_language_fn), "ko")) "데이터 보기" else "Data Viewer"),
      dataset_fn = dataset_fn,
      selected_names_fn = selected_names_fn,
      variables_fn = local({
        state_input <- canvas_input
        function() custom_model_canvas_viewer_variables(input[[state_input]] %||% list())
      }),
      variable_table_fn = variable_table_fn,
      labels_fn = labels_fn,
      category_table_fn = category_table_fn,
      language_fn = app_language_fn
    )
  })
  invisible(TRUE)
}
