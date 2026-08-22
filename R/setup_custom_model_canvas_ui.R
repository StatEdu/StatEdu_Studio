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
  app_language_fn = NULL
) {
  custom_model_canvas_snapshot <- reactiveVal(NULL)
  custom_model_canvas_pending_snapshot <- reactiveVal(NULL)

  output$custom_model_canvas_setup <- renderUI({
    custom_model_canvas_workspace(
      selected_names = selected_names_fn(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      input = input,
      language = statedu_current_language(app_language_fn)
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

  custom_model_canvas_result <- reactiveVal(NULL)
  custom_model_canvas_bootstrap_job <- reactiveVal(NULL)
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
    div(
      class = "mm-save-control",
      analysis_save_buttons(
        html_button_id = "save_custom_model_canvas_html_dialog",
        pdf_button_id = "save_custom_model_canvas_pdf_dialog",
        figure_button_id = "save_custom_model_canvas_figures_dialog",
        excel_button_id = "save_custom_model_canvas_excel_dialog",
        add_result_button_id = "add_custom_model_canvas_result",
        language = statedu_current_language(app_language_fn)
      )
    )
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
      list(source = source_snapshot, result = result_snapshot, show = TRUE)
    )
    showNotification(custom_model_canvas_text(language, "Custom model analysis finished.", "\uc0ac\uc6a9\uc790 \ubaa8\ud615 \ubd84\uc11d\uc774 \uc644\ub8cc\ub418\uc5c8\uc2b5\ub2c8\ub2e4."), type = "message", duration = 4)
  }

  run_custom_model_canvas_analysis <- function(snapshot) {
    language <- statedu_current_language(app_language_fn)
    spec <- custom_model_canvas_snapshot_spec(snapshot, selected_names_fn(), language, two_moderator_model = "3")
    old_job <- custom_model_canvas_bootstrap_job()
    if (!is.null(old_job)) {
      if (!is.null(old_job$process) && old_job$process$is_alive()) old_job$process$kill()
      mediation_moderation_cleanup_bootstrap_job(old_job)
    }
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
    job$source_snapshot <- snapshot
    custom_model_canvas_bootstrap_job(job)
    ko <- identical(normalize_app_language(language), "ko")
    structural_canvas_show_notification(
      statedu_bootstrap_status_ui(
        if (ko) "사용자 매개·조절 모형 부트스트랩 진행 상태" else "Custom mediation / moderation bootstrap progress",
        if (ko) paste0("준비 중 · 전체 ", format(job$requested_total, big.mark = ","), "회") else paste0("Starting · ", format(job$requested_total, big.mark = ","), " total resamples"),
        percent = NA_real_,
        stop_input_id = "custom_model_canvas_bootstrap_stop",
        stop_label = if (ko) "부트스트랩 중단" else "Stop bootstrap",
        phase_label = if (ko) "준비 중" else "Starting"
      ),
      type = "message", duration = NULL, id = "custom-model-canvas-bootstrap-progress"
    )
  }

  observeEvent(input$custom_model_canvas_bootstrap_stop, {
    job <- custom_model_canvas_bootstrap_job()
    if (is.null(job)) return()
    if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
    mediation_moderation_cleanup_bootstrap_job(job)
    custom_model_canvas_bootstrap_job(NULL)
    shiny::removeNotification("custom-model-canvas-bootstrap-progress")
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
          if (ko) "사용자 매개·조절 모형 부트스트랩 진행 상태" else "Custom mediation / moderation bootstrap progress",
          progress$detail,
          percent = progress$percent,
          stop_input_id = "custom_model_canvas_bootstrap_stop",
          stop_label = if (ko) "부트스트랩 중단" else "Stop bootstrap",
          phase_label = progress$phase_label
        ),
        type = "message", duration = NULL, id = "custom-model-canvas-bootstrap-progress"
      )
      return()
    }
    status <- job$process$get_exit_status()
    shiny::removeNotification("custom-model-canvas-bootstrap-progress")
    language <- statedu_current_language(app_language_fn)
    if (identical(status, 0L) && file.exists(job$result_file)) {
      apply_custom_model_canvas_result(readRDS(job$result_file), job$source_snapshot)
    } else {
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
  })

  session$onSessionEnded(function() {
    job <- shiny::isolate(custom_model_canvas_bootstrap_job())
    if (!is.null(job$process) && job$process$is_alive()) job$process$kill()
    mediation_moderation_cleanup_bootstrap_job(job)
  })

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

  observeEvent(input$save_custom_model_canvas_html_dialog, {
    shiny::req(!is.null(custom_model_canvas_result()))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) path <- paste0(path, ".html")
    tryCatch(
      {
        write_mediation_moderation_results_html(custom_model_canvas_result(), path, statedu_current_language(app_language_fn), dash_nonsignificant = TRUE, output_table_style = analysis_output_table_style(input$custom_mm_output_table_style))
        showNotification(sprintf(statedu_t("result.html_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) showNotification(paste(statedu_t("result.html_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_custom_model_canvas_pdf_dialog, {
    shiny::req(!is.null(custom_model_canvas_result()))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) path <- paste0(path, ".pdf")
    tryCatch(
      {
        write_mediation_moderation_results_pdf(custom_model_canvas_result(), path, statedu_current_language(app_language_fn), dash_nonsignificant = TRUE, output_table_style = analysis_output_table_style(input$custom_mm_output_table_style))
        showNotification(sprintf(statedu_t("result.pdf_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) showNotification(paste(statedu_t("result.pdf_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_custom_model_canvas_excel_dialog, {
    shiny::req(!is.null(custom_model_canvas_result()))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
    tryCatch(
      {
        save_mediation_moderation_excel_file(custom_model_canvas_result(), path)
        showNotification(sprintf(statedu_t("result.excel_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) showNotification(paste(statedu_t("result.excel_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_custom_model_canvas_figures_dialog, {
    shiny::req(!is.null(custom_model_canvas_result()))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification(statedu_t("result.folder_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_mediation_moderation_figures_to_dir(custom_model_canvas_result(), directory, statedu_current_language(app_language_fn), dash_nonsignificant = TRUE)
        showNotification(sprintf(statedu_t("result.figures_saved", statedu_current_language(app_language_fn)), length(saved), directory), type = "message")
      },
      error = function(e) showNotification(paste(statedu_t("result.figures_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
    )
  }, ignoreInit = TRUE)

  register_add_result_snapshot(
    input,
    session,
    "add_custom_model_canvas_result",
    "Custom mediation / moderation model",
    html_fn = function() {
      mediation_moderation_saved_results_html(
        custom_model_canvas_result(),
        statedu_current_language(app_language_fn),
        dash_nonsignificant = TRUE,
        output_table_style = analysis_output_table_style(input$custom_mm_output_table_style)
      )
    }
  )

  invisible(TRUE)
}
