register_custom_model_canvas_handlers <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn = function() NULL,
  mark_settings_dirty,
  app_language_fn = NULL,
  analysis_reset_epoch_fn = NULL
) {
  custom_model_canvas_snapshot <- reactiveVal(NULL)
  custom_model_canvas_pending_snapshot <- reactiveVal(NULL)
  custom_model_canvas_result <- analysis_scope_result_val(NULL)
  custom_model_canvas_bootstrap_job <- analysis_scope_result_val(NULL, job = TRUE)

  output$custom_model_canvas_setup <- renderUI({
    result <- custom_model_canvas_result()
    result_snapshot <- result$custom_model_canvas_result_snapshot %||% NULL
    custom_model_canvas_workspace(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      input = input,
      language = statedu_current_language(app_language_fn),
      initial_snapshot = shiny::isolate(custom_model_canvas_snapshot()),
      initial_result_snapshot = result_snapshot,
      initial_view = if (is.list(result_snapshot)) "result" else "source"
    )
  })

  observeEvent(input$custom_model_canvas_state, {
    custom_model_canvas_snapshot(input$custom_model_canvas_state)
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  lapply(c("custom_mm_analysis_method", "custom_mm_residual_diagnostics", "custom_mm_auto_method", "custom_mm_effect_size_y", "custom_mm_effect_size_m", "custom_mm_covariate_control_y", "custom_mm_covariate_control_m", "custom_mm_boot_r", "custom_mm_seed", "custom_mm_ci_method", "custom_mm_options_tab", "custom_mm_output_table_style"), function(input_id) {
    observeEvent(input[[input_id]], {
      mark_settings_dirty()
    }, ignoreInit = TRUE)
  })
  observeEvent(input$custom_mm_residual_diagnostics, {
    if (!isTRUE(input$custom_mm_residual_diagnostics)) {
      updateCheckboxInput(session, "custom_mm_auto_method", value = FALSE)
    }
  }, ignoreInit = TRUE)

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "custom_model_canvas",
    title = custom_model_canvas_text(statedu_current_language(app_language_fn), "Custom Model Canvas Data Viewer", "\uc0ac\uc6a9\uc790 \ubaa8\ub378 \ub370\uc774\ud130 \ubcf4\uae30"),
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() custom_model_canvas_viewer_variables(custom_model_canvas_snapshot() %||% list()),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  cancel_custom_model_canvas_bootstrap <- function() {
    job <- shiny::isolate(custom_model_canvas_bootstrap_job())
    statedu_stop_background_process_tree(job$process)
    mediation_moderation_cleanup_bootstrap_job(job)
    custom_model_canvas_bootstrap_job(NULL)
    shiny::removeNotification("custom-model-canvas-bootstrap-progress")
    invisible(!is.null(job))
  }
  if (is.function(analysis_reset_epoch_fn)) {
    observeEvent(analysis_reset_epoch_fn(), {
      cancel_custom_model_canvas_bootstrap()
      custom_model_canvas_snapshot(NULL)
      custom_model_canvas_pending_snapshot(NULL)
      custom_model_canvas_result(NULL)
    }, ignoreInit = TRUE)
  }
  output$custom_model_canvas_results <- renderUI({
    mediation_moderation_result_ui(
      custom_model_canvas_result(),
      statedu_current_language(app_language_fn),
      dash_nonsignificant = TRUE,
      output_table_style = analysis_output_table_style(input$custom_mm_output_table_style)
    )
  })
  output$custom_model_canvas_save_control <- renderUI({
    if (is.null(custom_model_canvas_result())) {
      return(NULL)
    }
    controls <- analysis_save_buttons(
        html_button_id = "save_custom_model_canvas_html_dialog",
        pdf_button_id = "save_custom_model_canvas_pdf_dialog",
        figure_button_id = "save_custom_model_canvas_figures_dialog",
        excel_button_id = "save_custom_model_canvas_excel_dialog",
        add_result_button_id = "add_custom_model_canvas_result",
        language = statedu_current_language(app_language_fn)
      )
    for (i in seq_along(controls$children)) {
      button <- controls$children[[i]]
      if (identical(button$attribs$id, "save_custom_model_canvas_figures_dialog")) {
        button$attribs$id <- NULL
        button$attribs$class <- gsub("action-button", "", button$attribs$class, fixed = TRUE)
        button$attribs$onclick <- "this.closest('.custom-model-canvas-root').querySelector('[data-action=export]').click()"
        controls$children[[i]] <- button
      }
    }
    div(class = "mm-save-control", controls)
  })

  apply_custom_model_canvas_result <- function(result, snapshot) {
    language <- statedu_current_language(app_language_fn)
    if (is.data.frame(result$overview) && all(c("Item", "Value") %in% names(result$overview))) {
      result$overview$Value[result$overview$Item == "Model"] <- custom_model_canvas_text(
        language,
        "User-defined mediation / moderation model",
        "\uc0ac\uc6a9\uc790\uc815\uc758 \ub9e4\uac1c\u00b7\uc870\uc808 \ubaa8\ud615"
      )
    }
    source_snapshot <- snapshot
    source_snapshot$nonce <- NULL
    result_snapshot <- custom_model_canvas_result_snapshot(source_snapshot, result)
    result$custom_model_canvas <- TRUE
    result$custom_model_canvas_snapshot <- source_snapshot
    result$custom_model_canvas_result_snapshot <- result_snapshot
    custom_model_canvas_result(result)
    session$sendCustomMessage(
      "custom-model-canvas-result",
      list(
        rootId = "custom-model-canvas-root",
        source = source_snapshot,
        result = result_snapshot,
        show = TRUE
      )
    )
  }

  observeEvent(input$custom_model_canvas_model_replaced, {
    cancel_custom_model_canvas_bootstrap()
    custom_model_canvas_result(NULL)
    custom_model_canvas_pending_snapshot(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$custom_model_canvas_result_save_request, {
    shiny::req(!is.null(custom_model_canvas_result()))
    language <- statedu_current_language(app_language_fn)
    tryCatch({
      path <- canvas_analysis_result_save_request(
        input$custom_model_canvas_result_save_request,
        "custom_mm",
        custom_model_canvas_result(),
        dataset_fn(),
        language
      )
      if (nzchar(path)) showNotification(if (identical(normalize_app_language(language), "ko")) paste0("분석 결과를 저장했습니다: ", path) else paste0("Analysis result saved: ", path), type = "message")
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$custom_model_canvas_result_load_request, {
    language <- statedu_current_language(app_language_fn)
    tryCatch({
      loaded <- canvas_analysis_result_load_request(input$custom_model_canvas_result_load_request, "custom_mm", language, dataset_fn())
      if (is.null(loaded)) return()
      package <- loaded$package
      result <- package$result
      source_snapshot <- package$source_snapshot %||% result$custom_model_canvas_snapshot %||% NULL
      result_snapshot <- package$result_snapshot %||% result$custom_model_canvas_result_snapshot %||% NULL
      result$custom_model_canvas <- TRUE
      result$custom_model_canvas_snapshot <- source_snapshot
      result$custom_model_canvas_result_snapshot <- result_snapshot
      custom_model_canvas_snapshot(source_snapshot)
      custom_model_canvas_pending_snapshot(NULL)
      custom_model_canvas_result(result)
      session$sendCustomMessage("custom-model-canvas-result", list(
        rootId = "custom-model-canvas-root",
        source = source_snapshot,
        result = result_snapshot,
        results = package$result_snapshots %||% NULL,
        activeResultGroupKey = package$active_result_group_key %||% "overall",
        show = TRUE
      ))
      showNotification(if (identical(normalize_app_language(language), "ko")) paste0("분석 결과를 불러왔습니다: ", loaded$path) else paste0("Analysis result loaded: ", loaded$path), type = "message")
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)

  run_custom_model_canvas_analysis <- function(snapshot, scope_dispatch = TRUE) {
    force(snapshot)
    if (scope_dispatch) return(analysis_scope_run(session, "custom_model_canvas",
      function() run_custom_model_canvas_analysis(snapshot, FALSE), "custom_model_canvas_results", "custom-model-canvas-root"))
    language <- statedu_current_language(app_language_fn)
    snapshot <- analysis_scope_model_snapshot(snapshot, dataset_fn())
    spec <- tryCatch(custom_model_canvas_snapshot_spec(snapshot, selected_names_fn(), language, two_moderator_model = "3"), error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = 8)
      NULL
    })
    if (is.null(spec)) return(invisible(NULL))
    cancel_custom_model_canvas_bootstrap()
    job <- tryCatch(
      mediation_moderation_start_bootstrap_job(list(
        data = dataset_fn(),
        roles = spec$roles,
        mediator_arrangement = spec$mediator_arrangement,
        moderated_paths = spec$moderated_paths,
        boot_r = as.integer(input$custom_mm_boot_r %||% 5000L),
        seed = as.integer(input$custom_mm_seed %||% default_seed()),
        mean_center = FALSE,
        simple_slopes = TRUE,
        johnson_neyman = TRUE,
        analysis_method = input$custom_mm_analysis_method %||% "statedu",
        ci_method = input$custom_mm_ci_method %||% "bias_corrected",
        residual_diagnostics = input$custom_mm_residual_diagnostics %||% TRUE,
        auto_method = isTRUE(input$custom_mm_residual_diagnostics %||% TRUE) && isTRUE(input$custom_mm_auto_method %||% TRUE),
        direct_x = spec$direct_x,
        x_to_m = spec$x_to_m,
        m_to_y = spec$m_to_y,
        m_to_m = spec$m_to_m,
        moderated_x_to_m = spec$moderated_x_to_m,
        moderated_m_to_y = spec$moderated_m_to_y,
        moderation_map = spec$moderation_map,
        two_moderator_model = "3",
        custom_path_model = TRUE,
        effect_size_models = c(
          if (isTRUE(input$custom_mm_effect_size_y %||% TRUE)) "y" else character(0),
          if (isTRUE(input$custom_mm_effect_size_m %||% FALSE)) "m" else character(0)
        ),
        covariate_control = c(
          if (isTRUE(input$custom_mm_covariate_control_y %||% TRUE)) "y" else character(0),
          if (isTRUE(input$custom_mm_covariate_control_m %||% TRUE)) "m" else character(0)
        ),
        language = language,
        variable_info = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn()
      )),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
    if (is.null(job)) return()
    mediation_moderation_claim_bootstrap(
      session,
      "custom_model_canvas",
      cancel_custom_model_canvas_bootstrap
    )
    job$source_snapshot <- snapshot
    custom_model_canvas_bootstrap_job(job)
    ko <- identical(normalize_app_language(language), "ko")
    structural_canvas_show_notification(
      statedu_bootstrap_status_ui(
        statedu_localized_text(language, "Custom mediation / moderation bootstrap progress", "사용자 매개·조절 모형 부트스트랩 진행 상태"),
        sprintf(statedu_localized_text(language, "Starting the bootstrap worker; %s resamples planned", "부트스트랩 작업 프로세스를 시작하는 중 · 예정 %s회"), format(job$requested_total, big.mark = ",")),
        percent = NA_real_,
        stop_input_id = "custom_model_canvas_bootstrap_stop",
        stop_label = statedu_localized_text(language, "Stop bootstrap", "부트스트랩 중단"),
        phase_label = statedu_localized_text(language, "Starting worker", "작업 시작 중")
      ),
      type = "message", duration = NULL, id = "custom-model-canvas-bootstrap-progress"
    )
  }

  observeEvent(input$custom_model_canvas_bootstrap_stop, {
    job <- custom_model_canvas_bootstrap_job()
    if (is.null(job)) return()
    cancel_custom_model_canvas_bootstrap()
    mediation_moderation_release_bootstrap(session, "custom_model_canvas")
    language <- statedu_current_language(app_language_fn)
    structural_canvas_show_notification(
      custom_model_canvas_text(language, "The custom-model bootstrap was stopped.", "사용자 모형 부트스트랩을 중단했습니다."),
      type = "warning", duration = 8
    )
  }, ignoreInit = TRUE)

  observe({
    job <- custom_model_canvas_bootstrap_job()
    if (is.null(job) || is.null(job$process)) return()
    if (job$process$is_alive()) {
      shiny::invalidateLater(400, session)
      language <- statedu_current_language(app_language_fn)
      progress <- mediation_moderation_bootstrap_job_progress(job, language)
      ko <- identical(normalize_app_language(language), "ko")
      structural_canvas_show_notification(
        statedu_bootstrap_status_ui(
          statedu_localized_text(language, "Custom mediation / moderation bootstrap progress", "사용자 매개·조절 모형 부트스트랩 진행 상태"),
          progress$detail,
          percent = progress$percent,
          stop_input_id = "custom_model_canvas_bootstrap_stop",
          stop_label = statedu_localized_text(language, "Stop bootstrap", "부트스트랩 중단"),
          phase_label = progress$phase_label
        ),
        type = "message", duration = NULL, id = "custom-model-canvas-bootstrap-progress"
      )
      return()
    }
    status <- job$process$get_exit_status()
    language <- statedu_current_language(app_language_fn)
    ko <- identical(normalize_app_language(language), "ko")
    if (identical(status, 0L) && file.exists(job$result_file)) {
      structural_canvas_show_notification(
        statedu_bootstrap_status_ui(
          statedu_localized_text(language, "Custom mediation / moderation bootstrap progress", "사용자 매개·조절 모형 부트스트랩 진행 상태"),
          statedu_localized_text(language, "Loading the saved analysis result", "저장된 분석 결과를 불러오는 중"),
          percent = NA_real_,
          stop_input_id = "custom_model_canvas_bootstrap_stop",
          stop_label = statedu_localized_text(language, "Stop bootstrap", "부트스트랩 중단"),
          phase_label = statedu_localized_text(language, "Loading results", "결과 불러오는 중")
        ),
        type = "message", duration = NULL, id = "custom-model-canvas-bootstrap-progress"
      )
      result_read_started <- proc.time()[["elapsed"]]
      result <- readRDS(job$result_file)
      result_read_elapsed <- proc.time()[["elapsed"]] - result_read_started
      result_render_started <- proc.time()[["elapsed"]]
      apply_custom_model_canvas_result(result, job$source_snapshot)
      structural_canvas_show_notification(
        statedu_bootstrap_status_ui(
          statedu_localized_text(language, "Custom mediation / moderation bootstrap progress", "사용자 매개·조절 모형 부트스트랩 진행 상태"),
          statedu_localized_text(language, "Rendering result tables, figures, and canvas", "결과 표·그림과 캔버스를 화면에 구성하는 중"),
          percent = NA_real_,
          stop_input_id = "custom_model_canvas_bootstrap_stop",
          stop_label = statedu_localized_text(language, "Stop bootstrap", "부트스트랩 중단"),
          phase_label = statedu_localized_text(language, "Rendering results", "결과 화면 구성 중")
        ),
        type = "message", duration = NULL, id = "custom-model-canvas-bootstrap-progress"
      )
      session$onFlushed(function() {
        render_elapsed <- proc.time()[["elapsed"]] - result_render_started
        shiny::removeNotification("custom-model-canvas-bootstrap-progress", session = session)
        message(sprintf(
          "[StatEdu timing] custom mediation/moderation result read %.3fs; server UI flush %.3fs",
          result_read_elapsed,
          render_elapsed
        ))
        shiny::showNotification(
          custom_model_canvas_text(language, "Custom model analysis finished.", "사용자 모형 분석이 완료되었습니다."),
          type = "message", duration = 4, session = session
        )
      }, once = TRUE)
    } else {
      shiny::removeNotification("custom-model-canvas-bootstrap-progress")
      error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
      structural_canvas_show_notification(
        paste0(
          custom_model_canvas_text(language, "The custom-model bootstrap did not complete.", "사용자 모형 부트스트랩을 완료하지 못했습니다."),
          if (nzchar(error_text)) paste0(" ", error_text) else ""
        ),
        type = "error", duration = 10
      )
    }
    mediation_moderation_cleanup_bootstrap_job(job)
    custom_model_canvas_bootstrap_job(NULL)
    mediation_moderation_release_bootstrap(session, "custom_model_canvas")
  })

  session$onSessionEnded(function() {
    cancel_custom_model_canvas_bootstrap()
    mediation_moderation_release_bootstrap(session, "custom_model_canvas")
  })

  register_canvas_analysis_commands(
    input, output, session, "custom_model_canvas_state", "custom-model-canvas-root",
    dataset_fn, function() list(selected = selected_names_fn(), variables = variable_table_fn(), labels = labels_fn(), categories = category_table_fn()),
    run_fn = function(snapshot) {
      custom_model_canvas_snapshot(snapshot)
      run_custom_model_canvas_analysis(snapshot)
    }
  )

  observeEvent(input$custom_model_canvas_run_request, {
    snapshot <- input$custom_model_canvas_run_request
    custom_model_canvas_pending_snapshot(snapshot)
    custom_model_canvas_snapshot(snapshot)
  }, ignoreInit = TRUE)

  observeEvent(input$custom_model_canvas_run_confirm, {
    snapshot <- input$custom_model_canvas_run_confirm %||% custom_model_canvas_pending_snapshot()
    custom_model_canvas_pending_snapshot(snapshot)
    custom_model_canvas_snapshot(snapshot)
    shiny::req(!is.null(snapshot))
    run_custom_model_canvas_analysis(snapshot)
  }, ignoreInit = TRUE)

  register_canvas_report_exports(input, session,
    "save_custom_model_canvas_html_dialog", "save_custom_model_canvas_pdf_dialog",
    "custom_model_canvas_results", "custom-model-canvas-root",
    function() custom_model_canvas_title(statedu_current_language(app_language_fn)),
    custom_model_canvas_result, app_language_fn, excel_id = "save_custom_model_canvas_excel_dialog", hwpx_id = "save_custom_model_canvas_hwpx_dialog")

  register_add_result_snapshot(
    input,
    session,
    "add_custom_model_canvas_result",
    function() if (identical(statedu_current_language(app_language_fn), "ko")) "매개·조절효과" else "Mediation / Moderation Effects",
    output_id = "custom_model_canvas_results",
    app_language_fn = app_language_fn,
    canvas_root_id = "custom-model-canvas-root"
  )

  invisible(TRUE)
}
