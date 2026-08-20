register_structural_equation_canvas_handlers <- function(input, output, session, dataset_fn, selected_names_fn, variable_table_fn, labels_fn, category_table_fn, mark_settings_dirty, app_language_fn = NULL) {
  lapply(c("cfa", "cbsem", "plssem"), function(analysis_type) {
    prefix <- structural_analysis_prefix(analysis_type)
    bootstrap_stop_button <- function(suffix, label) {
      input_id <- paste0(prefix, suffix)
      shiny::actionButton(
        input_id, label, class = "btn btn-sm btn-danger",
        onclick = sprintf("if (window.Shiny) Shiny.setInputValue('%s', Date.now(), {priority: 'event'});", input_id)
      )
    }
    canvas_input <- paste0(prefix, "_canvas_state")
    canvas_output <- paste0(prefix, "_canvas_setup")
    run_input <- paste0(prefix, "_canvas_run_request")
    confirm_input <- paste0(prefix, "_canvas_run_confirm")
    advanced_input <- paste0(prefix, "_canvas_advanced_request")
    fit_result <- reactiveVal(NULL)
    pls_bootstrap_job <- reactiveVal(NULL)
    pls_bootstrap_progress_mtime <- reactiveVal(NA_real_)
    pls_bootstrap_progress_cache <- reactiveVal(NULL)
    effect_bootstrap_job <- reactiveVal(NULL)
    cfa_bootstrap_job <- reactiveVal(NULL)
    pending_mi_rows <- reactiveVal(integer(0))
    pending_estimator_snapshot <- reactiveVal(NULL)
    output[[paste0(prefix, "_method_recommendation")]] <- renderUI({
      snapshot <- input[[canvas_input]] %||% list(nodes = list(), edges = list())
      recommendation <- structural_canvas_method_recommendation(
        snapshot, variable_table_fn(), input[[paste0(prefix, "_objective")]] %||% "confirmatory"
      )
      structural_canvas_method_recommendation_ui(
        recommendation,
        structural_canvas_selected_method_label(analysis_type, input[[paste0(prefix, "_estimator")]] %||% if (identical(analysis_type, "plssem")) "PLS" else "ML"),
        statedu_current_language(app_language_fn)
      )
    })
    output[[paste0(prefix, "_download_audit")]] <- downloadHandler(
      filename = function() paste0(analysis_type, "-audit-manifest-", format(Sys.Date(), "%Y%m%d"), ".json"),
      contentType = "application/json; charset=utf-8",
      content = function(file) {
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        structural_canvas_write_audit_manifest(bundle, file, analysis_type)
      }
    )
    if (analysis_type %in% c("cfa", "cbsem", "sem")) output[[paste0(prefix, "_download_reproducibility")]] <- downloadHandler(
      filename = function() paste0(analysis_type, "-analysis-record-", format(Sys.Date(), "%Y%m%d"), ".txt"),
      contentType = "text/plain; charset=utf-8",
      content = function(file) {
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        writeLines(structural_canvas_reproducibility_record(bundle), file, useBytes = TRUE)
      }
    )
    if (analysis_type %in% c("cfa", "cbsem", "sem")) output[[paste0(prefix, "_download_tables")]] <- downloadHandler(
      filename = function() paste0(analysis_type, "-result-tables-", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        shiny::req(requireNamespace("openxlsx", quietly = TRUE))
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        sheets <- structural_canvas_result_workbook_sheets(bundle, result_table)
        structural_canvas_write_result_workbook(sheets, file)
      }
    )
    if (analysis_type %in% c("cfa", "cbsem", "plssem")) observe({
      data <- dataset_fn()
      choices <- names(data %||% data.frame())
      current <- as.character(input[[paste0(prefix, "_invariance_group")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_invariance_group"), choices = choices, selected = if (current %in% choices) current else "")
    })
    if (identical(analysis_type, "cfa")) observe({
      snapshot <- input[[canvas_input]] %||% list(nodes = list(), edges = list())
      specification <- structural_canvas_construct_specification(snapshot)
      eligible <- specification$name[specification$construct_type == "commonFactor" & specification$measurement_mode == "reflective"]
      current <- as.character(input[[paste0(prefix, "_parcel_construct")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_parcel_construct"), choices = eligible, selected = if (current %in% eligible) current else if (length(eligible)) eligible[[1L]] else "")
    })
    if (identical(analysis_type, "plssem")) observe({
      snapshot <- input[[canvas_input]] %||% list(nodes = list(), edges = list())
      specification <- structural_canvas_construct_specification(snapshot)
      formative <- specification$name[specification$construct_type == "composite" & specification$measurement_mode == "formative"]
      current_construct <- as.character(input[[paste0(prefix, "_redundancy_construct")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_redundancy_construct"), choices = formative, selected = if (current_construct %in% formative) current_construct else if (length(formative)) formative[[1L]] else "")
      data_names <- names(dataset_fn() %||% data.frame())
      current_criterion <- as.character(input[[paste0(prefix, "_redundancy_criterion")]] %||% "")
      criterion_choices <- c(stats::setNames("", if (identical(statedu_current_language(app_language_fn), "ko")) "선택하지 않음" else "Not selected"), stats::setNames(data_names, data_names))
      updateSelectInput(session, paste0(prefix, "_redundancy_criterion"), choices = criterion_choices, selected = if (current_criterion %in% data_names) current_criterion else "")
    })
    result_table_cache <- new.env(parent = emptyenv())
    result_table_cache_bundle <- NULL
    result_table <- function(kind, language_override = NULL) {
      bundle <- fit_result()
      shiny::req(!is.null(bundle))
      if (!identical(bundle, result_table_cache_bundle)) {
        rm(list = ls(result_table_cache, all.names = TRUE), envir = result_table_cache)
        result_table_cache_bundle <<- bundle
      }
      language <- language_override %||% statedu_current_language(app_language_fn)
      labels <- labels_fn() %||% character(0)
      label_signature <- paste(names(labels), as.character(labels), sep = "=", collapse = "\r")
      cache_key <- paste(normalize_app_language(language), as.character(kind), label_signature, sep = "::")
      if (exists(cache_key, envir = result_table_cache, inherits = FALSE)) {
        return(get(cache_key, envir = result_table_cache, inherits = FALSE))
      }
      value <- structural_canvas_result_table(
        kind,
        function() bundle,
        analysis_type,
        function() labels,
        function() language
      )
      assign(cache_key, value, envir = result_table_cache)
      value
    }
    structural_canvas_register_result_outputs(
      input, output, prefix, canvas_output, analysis_type,
      selected_names_fn, variable_table_fn, dataset_fn, labels_fn, app_language_fn, fit_result, result_table
    )
    execute_analysis <- function(snapshot, settings = NULL) {
      value <- structural_canvas_execute_analysis(
        snapshot, settings, input, session, dataset_fn, variable_table_fn, analysis_type, prefix, fit_result, app_language_fn,
        defer_pls_bootstrap = identical(analysis_type, "plssem"),
        defer_cfa_bootstrap = analysis_type %in% c("cfa", "cbsem", "sem")
      )
      if (identical(analysis_type, "plssem")) {
        bundle <- fit_result()
        nboot <- suppressWarnings(as.integer(bundle$pls_bootstrap %||% 0L))
        if (is.finite(nboot) && nboot > 0L) {
          old_job <- pls_bootstrap_job()
          if (!is.null(old_job)) {
            if (!is.null(old_job$process) && old_job$process$is_alive()) old_job$process$kill()
            structural_canvas_cleanup_pls_bootstrap_job(old_job)
          }
          job <- structural_canvas_start_pls_bootstrap_job(bundle$diagnostics, nboot, bundle$pls_seed)
          pls_bootstrap_job(job)
          pls_bootstrap_progress_mtime(NA_real_)
          pls_bootstrap_progress_cache(NULL)
          ko <- identical(statedu_current_language(app_language_fn), "ko")
          structural_canvas_show_notification(
            shiny::tagList(
              shiny::tags$strong(if (ko) "PLS/PLSc 부트스트랩을 계산하고 있습니다." else "Computing the PLS/PLSc bootstrap."),
              shiny::tags$div(paste0(format(nboot, big.mark = ","), if (ko) "회 재표집 · 기본 분석 결과는 지금 확인할 수 있습니다." else " resamples · Base-model results are available now.")),
              shiny::tags$div(class = "progress structural-bootstrap-progress", shiny::tags$div(class = "progress-bar progress-bar-striped active", role = "progressbar", style = "width: 100%;", if (ko) "준비 중" else "Starting")),
              bootstrap_stop_button("_pls_bootstrap_stop", if (ko) "부트스트랩 중단" else "Stop bootstrap")
            ),
            type = "message", duration = NULL, id = paste0(prefix, "-pls-bootstrap-progress")
          )
        }
      }
      if (analysis_type %in% c("cbsem", "sem")) {
        bundle <- fit_result()
        if (!is.null(bundle) && isTRUE(bundle$effect_bootstrap_pending)) {
          old_job <- effect_bootstrap_job()
          if (!is.null(old_job)) {
            if (!is.null(old_job$process) && old_job$process$is_alive()) old_job$process$kill()
            structural_canvas_cleanup_effect_bootstrap_job(old_job)
          }
          job <- structural_canvas_start_effect_bootstrap_job(
            bundle$snapshot, bundle$analysis_data, analysis_type, bundle$estimator,
            bundle$missing, bundle$std_lv, bundle$ordered, character(0),
            bundle$residual_variance_fixes, bundle$effect_bootstrap, bundle$effect_bootstrap_seed,
            bundle$effect_bootstrap_ci_method %||% "bias_corrected"
          )
          effect_bootstrap_job(job)
          ko <- identical(statedu_current_language(app_language_fn), "ko")
          structural_canvas_show_notification(
            shiny::tagList(
              shiny::tags$strong(if (ko) "SEM 경로·간접·총효과 부트스트랩을 계산하고 있습니다." else "Computing SEM path, indirect, and total-effect bootstrap inference."),
              shiny::tags$div(paste0(format(job$total, big.mark = ","), if (ko) "회 재표집 · 기본 결과표는 지금 확인할 수 있습니다." else " resamples · Base result tables are available now.")),
              shiny::tags$div(class = "progress structural-bootstrap-progress", shiny::tags$div(class = "progress-bar progress-bar-striped active", role = "progressbar", style = "width: 100%;", if (ko) "준비 중" else "Starting")),
              bootstrap_stop_button("_effect_bootstrap_stop", if (ko) "경로·간접·총효과 부트스트랩 중단" else "Stop path/effect bootstrap")
            ),
            type = "message", duration = NULL, id = paste0(prefix, "-effect-bootstrap-progress")
          )
        }
      }
      if (analysis_type %in% c("cfa", "cbsem", "sem")) {
        bundle <- fit_result()
        if (!is.null(bundle) && isTRUE(bundle$cfa_bootstrap_pending)) {
          old_job <- cfa_bootstrap_job()
          if (!is.null(old_job)) {
            if (!is.null(old_job$process) && old_job$process$is_alive()) old_job$process$kill()
            structural_canvas_cleanup_cfa_bootstrap_job(old_job)
          }
          job <- structural_canvas_start_cfa_bootstrap_job(bundle)
          cfa_bootstrap_job(job)
          ko <- identical(statedu_current_language(app_language_fn), "ko")
          model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
          structural_canvas_show_notification(
            shiny::tagList(
              shiny::tags$strong(if (ko) paste0(model_label, " 부트스트랩을 계산하고 있습니다.") else paste0("Computing ", model_label, " bootstrap intervals.")),
              shiny::tags$div(if (ko) "기본 분석 결과는 지금 확인할 수 있습니다." else "Base-model results are available now."),
              shiny::tags$div(class = "progress structural-bootstrap-progress", shiny::tags$div(class = "progress-bar progress-bar-striped active", role = "progressbar", style = "width: 100%;", if (ko) "준비 중" else "Starting")),
              bootstrap_stop_button("_cfa_bootstrap_stop", if (ko) "부트스트랩 중단" else "Stop bootstrap")
            ),
            type = "message", duration = NULL, id = paste0(prefix, "-cfa-bootstrap-progress")
          )
        }
      }
      value
    }
    if (analysis_type %in% c("cfa", "cbsem", "sem")) {
      observeEvent(input[[paste0(prefix, "_cfa_bootstrap_stop")]], {
        job <- cfa_bootstrap_job()
        if (is.null(job)) return()
        if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
        structural_canvas_cleanup_cfa_bootstrap_job(job)
        cfa_bootstrap_job(NULL)
        bundle <- fit_result()
        if (!is.null(bundle)) {
          bundle$cfa_bootstrap_pending <- FALSE
          bundle$cfa_bootstrap_canceled <- TRUE
          bundle$cfa_bootstrap_error <- "Canceled by user"
          fit_result(bundle)
        }
        shiny::removeNotification(paste0(prefix, "-cfa-bootstrap-progress"))
        model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
        structural_canvas_show_notification(if (identical(statedu_current_language(app_language_fn), "ko")) paste0(model_label, " 부트스트랩을 중단했습니다. 기본 분석 결과는 유지됩니다.") else paste0("The ", model_label, " bootstrap was stopped. Base-model results remain available."), type = "warning", duration = 8)
      }, ignoreInit = TRUE)
      observe({
        job <- cfa_bootstrap_job()
        if (is.null(job) || is.null(job$process)) return()
        if (job$process$is_alive()) {
          shiny::invalidateLater(500, session)
          progress <- tryCatch(if (file.exists(job$progress_file)) readRDS(job$progress_file) else NULL, error = function(error) NULL)
          if (!is.null(progress)) {
            completed <- as.integer(progress$completed %||% 0L)
            total <- as.integer(progress$total %||% job$total %||% 0L)
            percent <- if (total > 0L) max(0, min(100, round(100 * completed / total))) else 0L
            phase <- as.character(progress$phase %||% "starting")
            ko <- identical(statedu_current_language(app_language_fn), "ko")
            phase_label <- if (ko) switch(phase, reliability = "AVE·신뢰도", bollen_stine = "Bollen-Stine", htmt = "HTMT", complete = "완료", "준비 중") else switch(phase, reliability = "AVE/reliability", bollen_stine = "Bollen-Stine", htmt = "HTMT", complete = "Complete", "Starting")
            detail <- paste0(phase_label, " · ", percent, "% · ", format(completed, big.mark = ","), "/", format(total, big.mark = ","))
            model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
            structural_canvas_show_notification(
              shiny::tagList(
                shiny::tags$strong(if (ko) paste0(model_label, " 부트스트랩 진행 상태") else paste0(model_label, " bootstrap progress")),
                shiny::tags$div(detail),
                shiny::tags$div(class = "progress structural-bootstrap-progress", shiny::tags$div(class = "progress-bar progress-bar-striped", role = "progressbar", style = paste0("width: ", percent, "%;"), paste0(percent, "%"))),
                bootstrap_stop_button("_cfa_bootstrap_stop", if (ko) "부트스트랩 중단" else "Stop bootstrap")
              ), type = "message", duration = NULL, id = paste0(prefix, "-cfa-bootstrap-progress")
            )
          }
          return()
        }
        on.exit(structural_canvas_cleanup_cfa_bootstrap_job(job), add = TRUE)
        shiny::removeNotification(paste0(prefix, "-cfa-bootstrap-progress"))
        status <- job$process$get_exit_status()
        bundle <- fit_result()
        if (!is.null(bundle)) {
          bundle$cfa_bootstrap_pending <- FALSE
          if (identical(status, 0L) && file.exists(job$result_file)) {
            value <- readRDS(job$result_file)
            bundle$reliability_bootstrap_result <- value$reliability_bootstrap_result
            bundle$bollen_stine_result <- value$bollen_stine_result
            bundle$htmt_bootstrap_result <- value$htmt_bootstrap_result
            bundle$cfa_bootstrap_canceled <- FALSE
            bundle$cfa_bootstrap_error <- NULL
            fit_result(bundle)
            model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
            structural_canvas_show_notification(if (identical(statedu_current_language(app_language_fn), "ko")) paste0(model_label, " 부트스트랩이 완료되어 결과표를 갱신했습니다.") else paste0("The ", model_label, " bootstrap is complete and result tables were updated."), type = "message", duration = 8)
          } else {
            model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
            error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else paste0("Background ", model_label, " bootstrap did not complete.")
            bundle$cfa_bootstrap_canceled <- FALSE
            bundle$cfa_bootstrap_error <- error_text
            fit_result(bundle)
            structural_canvas_show_notification(paste0(if (identical(statedu_current_language(app_language_fn), "ko")) paste0(model_label, " 부트스트랩을 완료하지 못했습니다. ") else paste0("The ", model_label, " bootstrap did not complete. "), error_text), type = "error", duration = 12)
          }
        }
        cfa_bootstrap_job(NULL)
      })
      session$onSessionEnded(function() {
        job <- shiny::isolate(cfa_bootstrap_job())
        if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
        structural_canvas_cleanup_cfa_bootstrap_job(job)
      })
    }
    if (analysis_type %in% c("cbsem", "sem")) {
      observeEvent(input[[paste0(prefix, "_effect_bootstrap_stop")]], {
        job <- effect_bootstrap_job()
        if (is.null(job)) return()
        if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
        structural_canvas_cleanup_effect_bootstrap_job(job)
        effect_bootstrap_job(NULL)
        bundle <- fit_result()
        if (!is.null(bundle)) {
          bundle$effect_bootstrap_pending <- FALSE
          bundle$effect_bootstrap_result <- NULL
          bundle$effect_bootstrap_canceled <- TRUE
          bundle$effect_bootstrap_error <- "Canceled by user"
          fit_result(bundle)
        }
        shiny::removeNotification(paste0(prefix, "-effect-bootstrap-progress"))
        structural_canvas_show_notification(
          if (identical(statedu_current_language(app_language_fn), "ko")) "SEM 경로·간접·총효과 부트스트랩을 중단했습니다. 기본 분석 결과는 유지됩니다." else "The SEM path, indirect, and total-effect bootstrap was stopped. Base-model results remain available.",
          type = "warning", duration = 8
        )
      }, ignoreInit = TRUE)
      observe({
        job <- effect_bootstrap_job()
        if (is.null(job) || is.null(job$process)) return()
        if (job$process$is_alive()) {
          shiny::invalidateLater(500, session)
          progress <- tryCatch(if (file.exists(job$progress_file)) readRDS(job$progress_file) else NULL, error = function(error) NULL)
          if (!is.null(progress)) {
            completed <- as.integer(progress$completed %||% 0L)
            total <- as.integer(progress$total %||% job$total %||% 0L)
            valid <- as.integer(progress$valid %||% 0L)
            percent <- if (total > 0L) max(0, min(100, round(100 * completed / total))) else 0L
            ko <- identical(statedu_current_language(app_language_fn), "ko")
            detail <- if (ko) {
              paste0(percent, "% · ", format(completed, big.mark = ","), "/", format(total, big.mark = ","), "회 · 유효 모형 ", format(valid, big.mark = ","))
            } else {
              paste0(percent, "% · ", format(completed, big.mark = ","), "/", format(total, big.mark = ","), " resamples · valid models ", format(valid, big.mark = ","))
            }
            structural_canvas_show_notification(
              shiny::tagList(
                shiny::tags$strong(if (ko) "SEM 경로·간접·총효과 부트스트랩 진행 상태" else "SEM path, indirect, and total-effect bootstrap progress"),
                shiny::tags$div(detail),
                shiny::tags$div(class = "progress structural-bootstrap-progress", shiny::tags$div(class = "progress-bar progress-bar-striped", role = "progressbar", style = paste0("width: ", percent, "%;"), paste0(percent, "%"))),
                bootstrap_stop_button("_effect_bootstrap_stop", if (ko) "경로·간접·총효과 부트스트랩 중단" else "Stop path/effect bootstrap")
              ), type = "message", duration = NULL, id = paste0(prefix, "-effect-bootstrap-progress")
            )
          }
          return()
        }
        on.exit(structural_canvas_cleanup_effect_bootstrap_job(job), add = TRUE)
        shiny::removeNotification(paste0(prefix, "-effect-bootstrap-progress"))
        status <- job$process$get_exit_status()
        bundle <- fit_result()
        if (!is.null(bundle)) {
          bundle$effect_bootstrap_pending <- FALSE
          if (identical(status, 0L) && file.exists(job$result_file)) {
            bundle$effect_bootstrap_result <- readRDS(job$result_file)
            bundle$effect_bootstrap_canceled <- FALSE
            bundle$effect_bootstrap_error <- NULL
            fit_result(bundle)
            structural_canvas_show_notification(
              if (identical(statedu_current_language(app_language_fn), "ko")) "경로·간접·총효과 bootstrap CI/p 계산이 완료되어 결과표를 갱신했습니다." else "Path, indirect, and total-effect bootstrap inference is complete and result tables were updated.",
              type = "message", duration = 6
            )
          } else {
            error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
            bundle$effect_bootstrap_result <- NULL
            bundle$effect_bootstrap_canceled <- FALSE
            bundle$effect_bootstrap_error <- if (nzchar(error_text)) error_text else "Background bootstrap did not complete."
            fit_result(bundle)
            structural_canvas_show_notification(
              if (identical(statedu_current_language(app_language_fn), "ko")) paste0("경로·간접·총효과 bootstrap CI/p 계산을 완료하지 못했습니다.", if (nzchar(error_text)) paste0(" ", error_text) else "") else paste0("Path, indirect, and total-effect bootstrap inference did not complete.", if (nzchar(error_text)) paste0(" ", error_text) else ""),
              type = "warning", duration = 10
            )
          }
        }
        effect_bootstrap_job(NULL)
      })
      session$onSessionEnded(function() {
        job <- shiny::isolate(effect_bootstrap_job())
        if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
        structural_canvas_cleanup_effect_bootstrap_job(job)
      })
    }
    if (identical(analysis_type, "plssem")) {
      observeEvent(input[[paste0(prefix, "_pls_bootstrap_stop")]], {
        job <- pls_bootstrap_job()
        if (is.null(job)) return()
        if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
        structural_canvas_cleanup_pls_bootstrap_job(job)
        pls_bootstrap_job(NULL)
        shiny::removeNotification(paste0(prefix, "-pls-bootstrap-progress"))
        structural_canvas_show_notification(
          if (identical(statedu_current_language(app_language_fn), "ko")) "PLS/PLSc 부트스트랩을 중단했습니다. 기본 분석 결과는 유지됩니다." else "The PLS/PLSc bootstrap was stopped. Base-model results remain available.",
          type = "warning", duration = 8
        )
      }, ignoreInit = TRUE)
      observe({
        job <- pls_bootstrap_job()
        if (is.null(job) || is.null(job$process)) return()
        if (job$process$is_alive()) {
          shiny::invalidateLater(400, session)
          current_mtime <- tryCatch(as.numeric(file.info(job$progress_file)$mtime), error = function(error) NA_real_)
          if (is.finite(current_mtime) && !identical(current_mtime, pls_bootstrap_progress_mtime())) {
            pls_bootstrap_progress_cache(tryCatch(readRDS(job$progress_file), error = function(error) pls_bootstrap_progress_cache()))
            pls_bootstrap_progress_mtime(current_mtime)
          }
          progress <- pls_bootstrap_progress_cache()
          elapsed <- max(0L, as.integer(difftime(Sys.time(), job$started_at, units = "secs")))
          ko <- identical(statedu_current_language(app_language_fn), "ko")
          determinate <- isTRUE(progress$determinate)
          completed <- suppressWarnings(as.integer(progress$completed %||% 0L))
          total <- suppressWarnings(as.integer(progress$total %||% job$nboot %||% 0L))
          percentage <- if (determinate && total > 0L) max(0, min(100, round(100 * completed / total))) else NA_real_
          progress_age <- suppressWarnings(as.numeric(Sys.time()) - as.numeric(progress$updated_at %||% Sys.time()))
          stalled <- identical(as.character(progress$phase %||% ""), "resampling") && is.finite(progress_age) && progress_age >= 10
          rate <- if (!stalled && elapsed > 0L && completed > 0L) completed / elapsed else NA_real_
          remaining <- if (!stalled && is.finite(rate) && rate > 0 && total > completed) ceiling((total - completed) / rate) else NA_real_
          phase <- as.character(progress$phase %||% "starting")
          phase_label <- if (ko) switch(phase, starting = "준비 중", resampling = "재표집 중", summarizing = "결과 정리 중", complete = "완료", "실행 중") else switch(phase, starting = "Starting", resampling = "Resampling", summarizing = "Preparing summaries", complete = "Complete", "Running")
          detail <- if (is.finite(percentage)) {
            paste0(
              phase_label, " ", percentage, "% · ", format(completed, big.mark = ","), "/", format(total, big.mark = ","),
              if (is.finite(rate)) paste0(" · ", format(round(rate, 1), nsmall = 1), if (ko) "회/초" else "/s") else "",
              if (ko) " · 경과 " else " · elapsed ", elapsed, if (ko) "초" else " s",
              if (stalled) if (ko) " · 현재 재표집 묶음이 오래 걸려 ETA 계산을 일시 중지했습니다." else " · current resample batch is slow; ETA paused."
              else if (is.finite(remaining)) paste0(if (ko) " · 예상 잔여 " else " · about ", remaining, if (ko) "초" else " s remaining") else ""
            )
          } else {
            paste0(phase_label, " · ", format(job$nboot, big.mark = ","), if (ko) "회 요청 · 경과 " else " requested · elapsed ", elapsed, if (ko) "초" else " s")
          }
          bar_class <- paste("progress-bar progress-bar-striped", if (!is.finite(percentage)) "active" else "")
          bar_width <- if (is.finite(percentage)) paste0(percentage, "%") else "100%"
          structural_canvas_show_notification(
            shiny::tagList(
              shiny::tags$strong(if (ko) "PLS/PLSc 부트스트랩 진행 상태" else "PLS/PLSc bootstrap progress"),
              shiny::tags$div(detail),
              shiny::tags$div(class = "progress structural-bootstrap-progress", shiny::tags$div(class = bar_class, role = "progressbar", style = paste0("width: ", bar_width, ";"), if (is.finite(percentage)) paste0(percentage, "%") else phase_label)),
              bootstrap_stop_button("_pls_bootstrap_stop", if (ko) "부트스트랩 중단" else "Stop bootstrap")
            ),
            type = "message", duration = NULL, id = paste0(prefix, "-pls-bootstrap-progress")
          )
          return()
        }
        on.exit(structural_canvas_cleanup_pls_bootstrap_job(job), add = TRUE)
        shiny::removeNotification(paste0(prefix, "-pls-bootstrap-progress"))
        status <- job$process$get_exit_status()
        if (identical(status, 0L) && file.exists(job$result_file)) {
          value <- readRDS(job$result_file)
          bundle <- fit_result()
          if (!is.null(bundle)) {
            bundle$pls_bootstrap_result <- value
            fit_result(bundle)
            session$sendCustomMessage(
              "custom-model-canvas-result",
              list(
                rootId = paste0(prefix, "-canvas-root"), source = bundle$snapshot,
                result = structural_canvas_result_snapshot(
                  bundle$snapshot, bundle$fit,
                  bundle$result_coefficient %||% "pls_p", value,
                  bundle$result_measurement_coefficient %||% "measurement_p"
                ),
                show = TRUE
              )
            )
          }
          valid_n <- suppressWarnings(as.integer(value$nboot %||% 0L))
          requested_n <- suppressWarnings(as.integer(value$requested_nboot %||% job$nboot %||% 0L))
          timeout_n <- suppressWarnings(as.integer(value$timeout_failures %||% 0L))
          estimation_n <- suppressWarnings(as.integer(value$estimation_failures %||% max(0L, requested_n - valid_n - timeout_n)))
          structural_canvas_show_notification(
            if (identical(statedu_current_language(app_language_fn), "ko")) paste0("PLS/PLSc 부트스트랩이 완료되어 결과표를 갱신했습니다. 유효 재표집 ", format(valid_n, big.mark = ","), "/", format(requested_n, big.mark = ","), "회 (시간 제한 ", timeout_n, ", 추정 실패 ", estimation_n, ").") else paste0("The PLS/PLSc bootstrap is complete and result tables were updated. Valid resamples: ", format(valid_n, big.mark = ","), "/", format(requested_n, big.mark = ","), " (timeouts ", timeout_n, ", estimation failures ", estimation_n, ")."),
            type = "message", duration = 8
          )
        } else {
          error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
          structural_canvas_show_notification(
            if (identical(statedu_current_language(app_language_fn), "ko")) paste0("PLS/PLSc 부트스트랩을 완료하지 못했습니다.", if (nzchar(error_text)) paste0(" ", error_text) else "") else paste0("The PLS/PLSc bootstrap did not complete.", if (nzchar(error_text)) paste0(" ", error_text) else ""),
            type = "error", duration = 12
          )
        }
        pls_bootstrap_job(NULL)
      })
      session$onSessionEnded(function() {
        job <- shiny::isolate(pls_bootstrap_job())
        if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
        structural_canvas_cleanup_pls_bootstrap_job(job)
      })
    }
    structural_canvas_register_interaction_events(
      input, session, dataset_fn, selected_names_fn, variable_table_fn, app_language_fn,
      analysis_type, prefix, canvas_input, run_input, confirm_input, advanced_input,
      fit_result, pending_mi_rows, pending_estimator_snapshot, mark_settings_dirty, execute_analysis
    )
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
