# Terminal notification formatting is UI-only; engine result/error records stay unchanged.
structural_canvas_bootstrap_terminal_text <- function(state, model, language, errors = character()) {
  tr <- function(text) structural_canvas_reporting_text(text, language)
  message <- if (identical(state, "partial")) tr("Only part of the bootstrap calculation completed; successful results were retained.") else sprintf(tr(switch(state,
    stopped = "The %s bootstrap was stopped. Base-model results remain available.",
    complete = "The %s bootstrap is complete and result tables were updated.",
    "The %s bootstrap did not complete.")), tr(model))
  errors <- errors[nzchar(errors)]
  paste0(message, if (length(errors)) paste0(" ", paste(errors, collapse = " | ")) else "")
}

structural_canvas_pls_bootstrap_completion_text <- function(valid, requested, counts, inference_available, language) {
  tr <- function(text) structural_canvas_reporting_text(text, language)
  paste0(tr("The PLS/PLSc bootstrap completed."), " ", tr("Valid resamples"), ": ",
    format(valid, big.mark = ","), "/", format(requested, big.mark = ","), " (",
    paste(vapply(names(counts), function(label) paste0(tr(label), " ", counts[[label]]), character(1)), collapse = ", "), "). ",
    tr(if (inference_available) "Result tables were updated." else "Inference is suppressed because the valid ratio is below 80%."))
}

structural_canvas_read_bootstrap_progress_snapshot <- function(
  progress_file, attempts = 5L, retry_seconds = 0.005
) {
  progress_file <- as.character(progress_file %||% "")
  attempts <- suppressWarnings(as.integer(attempts))
  retry_seconds <- suppressWarnings(as.numeric(retry_seconds))
  if (length(progress_file) != 1L || !nzchar(progress_file) ||
      !is.finite(attempts) || attempts < 1L ||
      !is.finite(retry_seconds) || retry_seconds < 0) return(NULL)
  for (attempt in seq_len(attempts)) {
    candidate <- suppressWarnings(tryCatch(
      if (file.exists(progress_file)) readRDS(progress_file) else NULL,
      error = function(error) NULL
    ))
    if (is.list(candidate)) return(candidate)
    if (attempt < attempts && retry_seconds > 0) Sys.sleep(retry_seconds)
  }
  NULL
}

register_structural_equation_canvas_handlers <- function(input, output, session, dataset_fn, selected_names_fn, variable_table_fn, labels_fn, category_table_fn, mark_settings_dirty, app_language_fn = NULL, analysis_reset_epoch_fn = NULL, analysis_types = c("cfa", "cbsem", "plssem")) {
  analysis_types <- intersect(c("cfa", "cbsem", "plssem"), unique(tolower(as.character(analysis_types %||% character(0)))))
  if (length(analysis_types) == 0L) return(invisible(list()))
  lapply(analysis_types, function(analysis_type) {
    prefix <- structural_analysis_prefix(analysis_type)
    canvas_input <- paste0(prefix, "_canvas_state")
    canvas_output <- paste0(prefix, "_canvas_setup")
    run_input <- paste0(prefix, "_canvas_run_request")
    confirm_input <- paste0(prefix, "_canvas_run_confirm")
    fit_result <- analysis_scope_result_val(NULL)
    pls_bootstrap_job <- analysis_scope_result_val(NULL, job = TRUE)
    pls_bootstrap_progress_mtime <- reactiveVal(NA_real_)
    pls_bootstrap_progress_cache <- reactiveVal(NULL)
    effect_bootstrap_job <- analysis_scope_result_val(NULL, job = TRUE)
    effect_bootstrap_progress_cache <- reactiveVal(NULL)
    cfa_bootstrap_job <- analysis_scope_result_val(NULL, job = TRUE)
    cfa_bootstrap_progress_cache <- reactiveVal(NULL)
    pending_mi_rows <- reactiveVal(integer(0))
    pending_estimator_snapshot <- reactiveVal(NULL)
    supports_invariance_paths <- analysis_type %in% c("cbsem", "sem", "plssem")
    invariance_path_selection_cache <- reactiveVal(character(0))
    invariance_path_choices <- function(snapshot) {
      display_name <- structural_canvas_display_name_resolver(
        snapshot = snapshot,
        variable_table = variable_table_fn(),
        labels = labels_fn() %||% character(0),
        language = statedu_current_language(app_language_fn)
      )
      structural_canvas_multigroup_path_choices(snapshot, display_name)
    }
    update_invariance_path_inputs <- function(snapshot, scope = NULL, selected = NULL) {
      if (!supports_invariance_paths) return(invisible(FALSE))
      choices <- invariance_path_choices(snapshot %||% list(nodes = list(), edges = list()))
      selected <- structural_canvas_invariance_selected_path_ids(
        selected %||% shiny::isolate(invariance_path_selection_cache())
      )
      selected <- intersect(selected, unname(choices))
      invariance_path_selection_cache(selected)
      if (!is.null(scope)) {
        updateRadioButtons(
          session, paste0(prefix, "_invariance_path_scope"),
          selected = structural_canvas_invariance_path_scope(scope)
        )
      }
      updateCheckboxGroupInput(
        session, paste0(prefix, "_invariance_selected_path_ids"),
        choices = choices, selected = selected
      )
      invisible(TRUE)
    }
    finalize_pls_bootstrap_bundle <- function(bundle, bootstrap) {
      if (is.null(bundle) || !is.list(bootstrap)) return(bundle)
      definitions <- bundle$diagnostics$moderation_definitions %||%
        bundle$fit$statedu_moderation_definitions %||% list()
      bundle$pls_modmed_result <- NULL
      bundle$pls_modmed_error <- NULL
      if (length(definitions) && !is.null(bootstrap$statedu_boot_paths)) {
        modmed_error <- NULL
        bundle$pls_modmed_result <- tryCatch(
          structural_canvas_pls_modmed_compact(
            structural_canvas_pls_modmed_from_bootstrap(
              bundle$diagnostics %||% list(fit = bundle$fit),
              bootstrap,
              estimator = bundle$estimator %||% "PLS",
              moderation_definitions = definitions
            )
          ),
          error = function(error) {
            modmed_error <<- conditionMessage(error)
            NULL
          }
        )
        bundle$pls_modmed_error <- modmed_error
      }
      # This full path array is an internal hand-off only.  Persist the compact
      # draw registries and result tables, not a potentially very large 3-D
      # object for 10,000–50,000 requested resamples.
      bootstrap$statedu_boot_paths <- NULL
      bootstrap$statedu_moderation_draws <- NULL
      bootstrap$statedu_effect_draws <- NULL
      bundle$pls_bootstrap_result <- bootstrap
      bundle
    }
    send_effect_bootstrap_result_snapshot <- function(bundle, show = TRUE) {
      if (is.null(bundle) || is.null(bundle$fit) || is.null(bundle$snapshot)) return(invisible(FALSE))
      result_snapshots <- structural_canvas_group_result_snapshots(
        bundle$snapshot, bundle$fit,
        bundle$result_coefficient %||% "beta_p",
        bundle$pls_bootstrap_result %||% NULL,
        bundle$result_measurement_coefficient %||% "measurement_p",
        bundle$invariance_result %||% NULL,
        statedu_current_language(app_language_fn),
        effect_bootstrap_state = structural_canvas_effect_bootstrap_snapshot_state_from_bundle(bundle)
      )
      session$sendCustomMessage("custom-model-canvas-result", list(
        rootId = paste0(prefix, "-canvas-root"), source = bundle$snapshot,
        result = result_snapshots[[1L]]$result, results = result_snapshots,
        show = isTRUE(show)
      ))
      invisible(TRUE)
    }
    clear_analysis_state <- function() {
      jobs <- list(
        list(value = shiny::isolate(cfa_bootstrap_job()), cleanup = structural_canvas_cleanup_cfa_bootstrap_job),
        list(value = shiny::isolate(effect_bootstrap_job()), cleanup = structural_canvas_cleanup_effect_bootstrap_job),
        list(value = shiny::isolate(pls_bootstrap_job()), cleanup = structural_canvas_cleanup_pls_bootstrap_job)
      )
      for (job_info in jobs) {
        job <- job_info$value
        if (!is.null(job)) {
          statedu_stop_background_process_tree(job$process)
          try(job_info$cleanup(job), silent = TRUE)
        }
      }
      cfa_bootstrap_job(NULL)
      effect_bootstrap_job(NULL)
      pls_bootstrap_job(NULL)
      pls_bootstrap_progress_mtime(NA_real_)
      pls_bootstrap_progress_cache(NULL)
      effect_bootstrap_progress_cache(NULL)
      cfa_bootstrap_progress_cache(NULL)
      pending_mi_rows(integer(0))
      pending_estimator_snapshot(NULL)
      if (supports_invariance_paths) {
        invariance_path_selection_cache(character(0))
        update_invariance_path_inputs(
          shiny::isolate(input[[canvas_input]]) %||% list(nodes = list(), edges = list()),
          scope = "all", selected = character(0)
        )
      }
      fit_result(NULL)
      invisible(TRUE)
    }
    if (is.function(analysis_reset_epoch_fn)) {
      observeEvent(analysis_reset_epoch_fn(), clear_analysis_state(), ignoreInit = TRUE)
    }
    observeEvent(input[[paste0(prefix, "_canvas_model_replaced")]], clear_analysis_state(), ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_canvas_result_save_request")]], {
      bundle <- fit_result()
      shiny::req(!is.null(bundle))
      language <- statedu_current_language(app_language_fn)
      tryCatch({
        path <- canvas_analysis_result_save_request(
          input[[paste0(prefix, "_canvas_result_save_request")]],
          analysis_type,
          bundle,
          dataset_fn(),
          language
        )
        if (nzchar(path)) showNotification(if (identical(normalize_app_language(language), "ko")) paste0("분석 결과를 저장했습니다: ", path) else paste0("Analysis result saved: ", path), type = "message")
      }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = 8))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_canvas_result_load_request")]], {
      language <- statedu_current_language(app_language_fn)
      tryCatch({
        loaded <- canvas_analysis_result_load_request(input[[paste0(prefix, "_canvas_result_load_request")]], analysis_type, language, dataset_fn())
        if (is.null(loaded)) return()
        package <- loaded$package
        clear_analysis_state()
        bundle <- package$result
        source_snapshot <- package$source_snapshot %||% bundle$snapshot %||% NULL
        result_snapshot <- package$result_snapshot %||% NULL
        result_snapshots <- package$result_snapshots %||% NULL
        bundle$snapshot <- source_snapshot
        fit_result(bundle)
        update_invariance_path_inputs(
          source_snapshot,
          scope = bundle$invariance_path_scope %||%
            bundle$invariance_result$path_scope %||% "all",
          selected = bundle$invariance_selected_path_ids %||%
            bundle$invariance_result$requested_path_ids %||% character(0)
        )
        session$sendCustomMessage("custom-model-canvas-result", list(
          rootId = paste0(prefix, "-canvas-root"),
          source = source_snapshot,
          result = result_snapshot,
          results = result_snapshots,
          activeResultGroupKey = package$active_result_group_key %||% "overall",
          show = TRUE
        ))
        showNotification(if (identical(normalize_app_language(language), "ko")) paste0("분석 결과를 불러왔습니다: ", loaded$path) else paste0("Analysis result loaded: ", loaded$path), type = "message")
      }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = 8))
    }, ignoreInit = TRUE)
    if (supports_invariance_paths) {
      observeEvent(input[[paste0(prefix, "_invariance_selected_path_ids")]], {
        invariance_path_selection_cache(
          structural_canvas_invariance_selected_path_ids(
            input[[paste0(prefix, "_invariance_selected_path_ids")]]
          )
        )
      }, ignoreInit = TRUE)
      observe({
        snapshot <- input[[canvas_input]] %||% list(nodes = list(), edges = list())
        update_invariance_path_inputs(snapshot)
      })
    }
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
    if (analysis_type %in% c("cfa", "cbsem", "sem", "plssem")) output[[paste0(prefix, "_download_tables")]] <- downloadHandler(
      filename = function() paste0(analysis_type, "-result-tables-", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        shiny::req(requireNamespace("openxlsx", quietly = TRUE))
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        display_name <- structural_canvas_display_name_resolver(
          snapshot = bundle$snapshot %||% list(),
          variable_table = variable_table_fn(),
          labels = labels_fn() %||% character(0),
          moderation_definitions = bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list(),
          language = statedu_current_language(app_language_fn)
        )
        sheets <- structural_canvas_result_workbook_sheets(bundle, result_table, display_name)
        structural_canvas_write_result_workbook(sheets, file)
      }
    )
    register_add_result_snapshot(input, session, paste0(prefix, "_add_result"),
      title = function() structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)),
      output_id = paste0(prefix, "_results"), app_language_fn = app_language_fn,
      canvas_root_id = paste0(prefix, "-canvas-root"))
    register_canvas_report_exports(input, session,
      paste0(prefix, "_save_html"), paste0(prefix, "_save_pdf"),
      paste0(prefix, "_results"), paste0(prefix, "-canvas-root"),
      function() structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)),
      fit_result, app_language_fn, excel_id = paste0(prefix, "_save_excel"), hwpx_id = paste0(prefix, "_save_hwpx"),
      canvas_input_prefix = paste0(prefix, "_canvas"))
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
      criterion_choices <- c(stats::setNames("", statedu_localized_text(statedu_current_language(app_language_fn), "Not selected", "선택하지 않음")), stats::setNames(data_names, data_names))
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
      variable_table <- variable_table_fn()
      label_signature <- paste(names(labels), as.character(labels), sep = "=", collapse = "\r")
      variable_label_signature <- if (is.data.frame(variable_table) && all(c("name", "var_label") %in% names(variable_table))) {
        paste(as.character(variable_table$name), as.character(variable_table$var_label), sep = "=", collapse = "\r")
      } else ""
      cache_key <- paste(normalize_app_language(language), as.character(kind), label_signature, variable_label_signature, sep = "::")
      if (exists(cache_key, envir = result_table_cache, inherits = FALSE)) {
        return(get(cache_key, envir = result_table_cache, inherits = FALSE))
      }
      value <- structural_canvas_result_table(
        kind,
        function() bundle,
        analysis_type,
        function() labels,
        function() language,
        function() variable_table
      )
      if (is.data.frame(value)) {
        attr(value, "result_table_language") <- normalize_app_language(language)
      }
      assign(cache_key, value, envir = result_table_cache)
      value
    }
    structural_canvas_register_result_outputs(
      input, output, prefix, canvas_output, analysis_type,
      selected_names_fn, variable_table_fn, dataset_fn, labels_fn, app_language_fn, fit_result, result_table
    )
    start_cfa_bootstrap <- function(bundle) {
      if (is.null(bundle) || !isTRUE(bundle$cfa_bootstrap_pending)) return(invisible(FALSE))
      old_job <- cfa_bootstrap_job()
      if (!is.null(old_job)) {
        statedu_stop_background_process_tree(old_job$process)
        structural_canvas_cleanup_cfa_bootstrap_job(old_job)
      }
      job <- structural_canvas_start_cfa_bootstrap_job(bundle)
      cfa_bootstrap_job(job)
      cfa_bootstrap_progress_cache(NULL)
      tr <- function(text) structural_canvas_reporting_text(text, statedu_current_language(app_language_fn))
      model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
      structural_canvas_show_notification(
        statedu_bootstrap_status_ui(
          sprintf(tr("%s bootstrap progress"), model_label),
          tr("Base-model results are available now."),
          percent = NA_real_, stop_input_id = paste0(prefix, "_cfa_bootstrap_stop"),
          stop_label = tr("Stop bootstrap"),
          phase_label = tr("Starting")
        ),
        type = "message", duration = NULL, id = paste0(prefix, "-cfa-bootstrap-progress")
      )
      invisible(TRUE)
    }
    execute_analysis <- function(snapshot, settings = NULL) {
      value <- structural_canvas_execute_analysis(
        snapshot, settings, input, session, dataset_fn, variable_table_fn, analysis_type, prefix, fit_result, app_language_fn,
        defer_pls_bootstrap = identical(analysis_type, "plssem"),
        defer_cfa_bootstrap = analysis_type %in% c("cfa", "cbsem", "sem")
      )
      if (identical(analysis_type, "plssem")) {
        bundle <- fit_result()
        old_job <- pls_bootstrap_job()
        if (!is.null(old_job)) {
          statedu_stop_background_process_tree(old_job$process)
          structural_canvas_cleanup_pls_bootstrap_job(old_job)
          shiny::removeNotification(paste0(prefix, "-pls-bootstrap-progress"))
        }
        pls_bootstrap_job(NULL)
        pls_bootstrap_progress_mtime(NA_real_)
        pls_bootstrap_progress_cache(NULL)
        nboot <- suppressWarnings(as.integer(bundle$pls_bootstrap %||% 0L))
        if (is.finite(nboot) && nboot > 0L) {
          bundle$pls_bootstrap_result <- structural_canvas_pls_bootstrap_unavailable_result(
            nboot, bundle$pls_seed, status = "Pending",
            failure_message = "Background PLS/PLSc bootstrap is running."
          )
          fit_result(bundle)
          ko <- identical(statedu_current_language(app_language_fn), "ko")
          job <- tryCatch(
            structural_canvas_start_pls_bootstrap_job(bundle$diagnostics, nboot, bundle$pls_seed),
            error = function(error) {
              error_text <- conditionMessage(error)
              bundle$pls_bootstrap_result <- structural_canvas_pls_bootstrap_unavailable_result(
                nboot, bundle$pls_seed, status = "Failed", failure_message = error_text
              )
              fit_result(bundle)
              structural_canvas_show_notification(
                paste0(structural_canvas_reporting_text("The PLS/PLSc bootstrap could not start.", statedu_current_language(app_language_fn)), " ", error_text),
                type = "error", duration = 12
              )
              NULL
            }
          )
          if (!is.null(job)) {
            pls_bootstrap_job(job)
            pls_bootstrap_progress_mtime(NA_real_)
            pls_bootstrap_progress_cache(NULL)
            structural_canvas_show_notification(
              statedu_bootstrap_status_ui(
                sprintf(structural_canvas_reporting_text("%s bootstrap progress", statedu_current_language(app_language_fn)), "PLS/PLSc"),
                sprintf(structural_canvas_reporting_text("%s resamples; base-model results are available now.", statedu_current_language(app_language_fn)), format(nboot, big.mark = ",")),
                percent = NA_real_, stop_input_id = paste0(prefix, "_pls_bootstrap_stop"),
                stop_label = structural_canvas_reporting_text("Stop bootstrap", statedu_current_language(app_language_fn)),
                phase_label = structural_canvas_reporting_text("Starting", statedu_current_language(app_language_fn))
              ),
              type = "message", duration = NULL, id = paste0(prefix, "-pls-bootstrap-progress")
            )
          }
        }
      }
      if (analysis_type %in% c("cbsem", "sem")) {
        bundle <- fit_result()
        if (!is.null(bundle) && (
          isTRUE(bundle$effect_bootstrap_pending) ||
            isTRUE(bundle$multigroup_moderation_bootstrap_pending)
        )) {
          # SEM effect and CFA-family bootstraps share one visible queue. Stop
          # any job from a previous model and run CFA-family work only after
          # the effect bootstrap finishes, so two progress cards never race.
          old_cfa_job <- cfa_bootstrap_job()
          if (!is.null(old_cfa_job)) {
            statedu_stop_background_process_tree(old_cfa_job$process)
            structural_canvas_cleanup_cfa_bootstrap_job(old_cfa_job)
            cfa_bootstrap_job(NULL)
            cfa_bootstrap_progress_cache(NULL)
            shiny::removeNotification(paste0(prefix, "-cfa-bootstrap-progress"))
          }
          old_job <- effect_bootstrap_job()
          if (!is.null(old_job)) {
            structural_canvas_stop_effect_bootstrap_job(old_job)
            structural_canvas_cleanup_effect_bootstrap_job(old_job)
          }
          multigroup_moderation <- if (
            isTRUE(bundle$multigroup_moderation_bootstrap_pending) &&
              isTRUE(bundle$invariance_enabled) &&
              identical(as.character(bundle$invariance_result$subtype %||% ""), "latent_product_indicator") &&
              length(bundle$diagnostics$moderation_definitions %||% list()) > 0L
          ) {
            list(
              syntax = bundle$syntax,
              raw_data = bundle$analysis_data,
              group = bundle$invariance_group,
              moderation_definitions = bundle$diagnostics$moderation_definitions %||% list(),
              effect_definitions = bundle$diagnostics$effect_definitions %||% list(),
              estimator = bundle$estimator,
              missing = bundle$missing,
              std_lv = bundle$std_lv,
              ml_likelihood = bundle$ml_likelihood %||% "normal"
            )
          } else NULL
          job <- structural_canvas_start_effect_bootstrap_job(
            bundle$snapshot, bundle$analysis_data, analysis_type, bundle$estimator,
            bundle$missing, bundle$std_lv, bundle$ordered, character(0),
            bundle$residual_variance_fixes, bundle$effect_bootstrap, bundle$effect_bootstrap_seed,
            bundle$effect_bootstrap_ci_method %||% "bias_corrected",
            bundle$ml_likelihood %||% "normal",
            original_result = bundle$diagnostics,
            multigroup_moderation = multigroup_moderation,
            run_pooled_effect = isTRUE(bundle$effect_bootstrap_pooled_eligible)
          )
          effect_bootstrap_job(job)
          effect_bootstrap_progress_cache(NULL)
          ko <- identical(statedu_current_language(app_language_fn), "ko")
          structural_canvas_show_notification(
            statedu_bootstrap_status_ui(
              sprintf(structural_canvas_reporting_text("%s bootstrap progress", statedu_current_language(app_language_fn)), "SEM"),
              sprintf(structural_canvas_reporting_text("%s resamples; base-model results are available now.", statedu_current_language(app_language_fn)), format(job$reps, big.mark = ",")),
              percent = NA_real_, stop_input_id = paste0(prefix, "_effect_bootstrap_stop"),
              stop_label = structural_canvas_reporting_text("Stop bootstrap", statedu_current_language(app_language_fn)),
              phase_label = structural_canvas_reporting_text("Starting", statedu_current_language(app_language_fn))
            ),
            type = "message", duration = NULL, id = paste0(prefix, "-effect-bootstrap-progress")
          )
        }
      }
      if (analysis_type %in% c("cfa", "cbsem", "sem")) {
        bundle <- fit_result()
        effect_is_queued <- analysis_type %in% c("cbsem", "sem") &&
          !is.null(bundle) && (
            isTRUE(bundle$effect_bootstrap_pending) ||
              isTRUE(bundle$multigroup_moderation_bootstrap_pending)
          )
        if (!effect_is_queued) {
          start_cfa_bootstrap(bundle)
        }
      }
      value
    }
    if (analysis_type %in% c("cfa", "cbsem", "sem")) {
      cfa_tr <- function(text) structural_canvas_reporting_text(text, statedu_current_language(app_language_fn))
      observeEvent(input[[paste0(prefix, "_cfa_bootstrap_stop")]], {
        job <- cfa_bootstrap_job()
        if (is.null(job)) return()
        statedu_stop_background_process_tree(job$process)
        structural_canvas_cleanup_cfa_bootstrap_job(job)
        cfa_bootstrap_job(NULL)
        cfa_bootstrap_progress_cache(NULL)
        bundle <- fit_result()
        if (!is.null(bundle)) {
          bundle$cfa_bootstrap_pending <- FALSE
          bundle$cfa_bootstrap_canceled <- TRUE
          bundle$cfa_bootstrap_error <- "Canceled by user"
          fit_result(bundle)
          send_effect_bootstrap_result_snapshot(bundle)
        }
        shiny::removeNotification(paste0(prefix, "-cfa-bootstrap-progress"))
        model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
        structural_canvas_show_notification(sprintf(cfa_tr("The %s bootstrap was stopped. Base-model results remain available."), model_label), type = "warning", duration = 8)
      }, ignoreInit = TRUE)
      observe({
        job <- cfa_bootstrap_job()
        if (is.null(job) || is.null(job$process)) return()
        if (job$process$is_alive()) {
          shiny::invalidateLater(500, session)
          candidate_progress <- structural_canvas_read_bootstrap_progress_snapshot(job$progress_file)
          progress <- structural_canvas_cfa_bootstrap_progress_merge(
            cfa_bootstrap_progress_cache(), candidate_progress
          )
          if (!is.null(progress)) {
            cfa_bootstrap_progress_cache(progress)
            completed <- as.integer(progress$completed %||% 0L)
            total <- as.integer(progress$total %||% job$total %||% 0L)
            percent <- if (total > 0L) max(0, min(100, round(100 * completed / total))) else 0L
            phase <- as.character(progress$phase %||% "starting")
            ko <- identical(statedu_current_language(app_language_fn), "ko")
            phase_label <- cfa_tr(switch(phase, reliability = "AVE/reliability", bollen_stine = "Bollen-Stine", htmt = "HTMT", complete = "Complete", "Starting"))
            detail <- paste0(phase_label, " · ", percent, "% · ", format(completed, big.mark = ","), "/", format(total, big.mark = ","))
            model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
            structural_canvas_show_notification(
              statedu_bootstrap_status_ui(
                sprintf(cfa_tr("%s bootstrap progress"), model_label),
                detail, percent = percent, stop_input_id = paste0(prefix, "_cfa_bootstrap_stop"),
                stop_label = cfa_tr("Stop bootstrap"), phase_label = phase_label
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
            structural_canvas_show_notification(sprintf(cfa_tr("The %s bootstrap is complete and result tables were updated."), model_label), type = "message", duration = 8)
          } else {
            model_label <- if (identical(analysis_type, "cfa")) "CFA" else "SEM"
            error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else paste0("Background ", model_label, " bootstrap did not complete.")
            bundle$cfa_bootstrap_canceled <- FALSE
            bundle$cfa_bootstrap_error <- error_text
            fit_result(bundle)
            structural_canvas_show_notification(paste0(sprintf(cfa_tr("The %s bootstrap did not complete."), model_label), " ", error_text), type = "error", duration = 12)
          }
        }
        cfa_bootstrap_job(NULL)
        cfa_bootstrap_progress_cache(NULL)
      })
      session$onSessionEnded(function() {
        job <- shiny::isolate(cfa_bootstrap_job())
        statedu_stop_background_process_tree(job$process)
        structural_canvas_cleanup_cfa_bootstrap_job(job)
      })
    }
    if (analysis_type %in% c("cbsem", "sem")) {
      observeEvent(input[[paste0(prefix, "_effect_bootstrap_stop")]], {
        job <- effect_bootstrap_job()
        if (is.null(job)) return()
        structural_canvas_stop_effect_bootstrap_job(job)
        structural_canvas_cleanup_effect_bootstrap_job(job)
        effect_bootstrap_job(NULL)
        effect_bootstrap_progress_cache(NULL)
        bundle <- fit_result()
        if (!is.null(bundle)) {
          bundle$effect_bootstrap_pending <- FALSE
          bundle$multigroup_moderation_bootstrap_pending <- FALSE
          if (isTRUE(job$run_pooled_effect)) {
            bundle$effect_bootstrap_result <- NULL
            bundle$effect_bootstrap_canceled <- TRUE
            bundle$effect_bootstrap_error <- "Canceled by user"
          }
          if (isTRUE(job$run_multigroup_moderation)) {
            bundle$multigroup_moderation_bootstrap_canceled <- TRUE
            bundle$multigroup_moderation_bootstrap_error <- "Canceled by user"
          }
          # The Stop action applies to the one queued SEM bootstrap operation,
          # including any CFA-family work that has not started yet.
          bundle$cfa_bootstrap_pending <- FALSE
          bundle$cfa_bootstrap_canceled <- TRUE
          bundle$cfa_bootstrap_error <- "Canceled by user"
          fit_result(bundle)
          send_effect_bootstrap_result_snapshot(bundle)
        }
        shiny::removeNotification(paste0(prefix, "-effect-bootstrap-progress"))
        shiny::removeNotification(paste0(prefix, "-cfa-bootstrap-progress"))
        structural_canvas_show_notification(
          structural_canvas_bootstrap_terminal_text("stopped", if (isTRUE(job$run_multigroup_moderation)) "SEM and multi-group latent-moderation" else "SEM path, indirect, and total-effect", statedu_current_language(app_language_fn)),
          type = "warning", duration = 8
        )
      }, ignoreInit = TRUE)
      observe({
        job <- effect_bootstrap_job()
        if (is.null(job) || is.null(job$process)) return()
        if (job$process$is_alive()) {
          shiny::invalidateLater(500, session)
          candidate_progress <- structural_canvas_read_bootstrap_progress_snapshot(job$progress_file)
          progress <- structural_canvas_effect_bootstrap_progress_merge(
            effect_bootstrap_progress_cache(), candidate_progress
          )
          if (!is.null(progress)) {
            effect_bootstrap_progress_cache(progress)
            completed <- as.integer(progress$completed %||% 0L)
            total <- as.integer(progress$total %||% job$total %||% 0L)
            valid <- as.integer(progress$valid %||% 0L)
            percent <- if (total > 0L) max(0, min(100, round(100 * completed / total))) else 0L
            ko <- identical(statedu_current_language(app_language_fn), "ko")
            phase <- as.character(progress$phase %||% "starting")
            tr <- function(text) structural_canvas_reporting_text(text, statedu_current_language(app_language_fn))
            phase_label <- tr(switch(phase,
              loading_engine = "Loading engine", starting_workers = "Starting workers",
              resampling = "Resampling", validating = "Validating screened models",
              summarizing = "Summarizing", multigroup_resampling = "Stratified multi-group latent-moderation resampling",
              complete = "Complete", "Starting"))
            elapsed <- suppressWarnings(as.numeric(progress$elapsed %||% 0))
            # Throughput and ETA based on completed resamples are meaningful
            # only during resampling. The guarded full-SE validation is a
            # distinct phase and should not misleadingly display ETA 0.
            rate <- if (identical(phase, "resampling") && completed > 0L &&
                        is.finite(elapsed) && elapsed > 0) completed / elapsed else NA_real_
            eta <- if (is.finite(rate) && rate > 0 && total >= completed) (total - completed) / rate else NA_real_
            detail <- paste0(percent, "% · ",
              sprintf(tr("%s resamples; valid models %s"), paste0(format(completed, big.mark = ","), "/", format(total, big.mark = ",")), format(valid, big.mark = ",")),
              if (is.finite(rate)) paste0(" · ", sprintf(tr("%s/sec"), format(round(rate, 1), nsmall = 1))) else "",
              if (is.finite(eta)) paste0(" · ", sprintf(tr("ETA %s sec"), format(round(eta), big.mark = ","))) else "")
            structural_canvas_show_notification(
              statedu_bootstrap_status_ui(
                sprintf(structural_canvas_reporting_text("%s bootstrap progress", statedu_current_language(app_language_fn)), "SEM"),
                detail, percent = percent, stop_input_id = paste0(prefix, "_effect_bootstrap_stop"),
                stop_label = structural_canvas_reporting_text("Stop bootstrap", statedu_current_language(app_language_fn)),
                phase_label = phase_label
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
          bundle$multigroup_moderation_bootstrap_pending <- FALSE
          if (identical(status, 0L) && file.exists(job$result_file)) {
            bootstrap_value <- readRDS(job$result_file)
            component_status <- attr(bootstrap_value, "bootstrap_component_status") %||% list()
            pooled_status <- component_status$pooled_effect %||% list(
              requested = isTRUE(job$run_pooled_effect), succeeded = TRUE, error = NULL
            )
            multigroup_status <- component_status$multigroup_moderation %||% list(
              requested = isTRUE(job$run_multigroup_moderation),
              succeeded = !is.null(attr(bootstrap_value, "multigroup_moderation")), error = NULL
            )
            bundle$effect_bootstrap_result <- if (
              isTRUE(pooled_status$requested) && isTRUE(pooled_status$succeeded)
            ) bootstrap_value else NULL
            multigroup_bootstrap <- attr(bootstrap_value, "multigroup_moderation")
            if (is.list(multigroup_bootstrap) && is.list(bundle$invariance_result)) {
              bundle$invariance_result <- structural_canvas_apply_multigroup_moderation_bootstrap(
                bundle$invariance_result, multigroup_bootstrap
              )
            }
            bundle$effect_bootstrap_canceled <- FALSE
            bundle$multigroup_moderation_bootstrap_canceled <- FALSE
            pooled_error <- if (isTRUE(pooled_status$requested) && !isTRUE(pooled_status$succeeded)) {
              as.character(pooled_status$error %||% "Pooled structural-effect bootstrap failed.")
            } else ""
            multigroup_error <- if (isTRUE(multigroup_status$requested) && !isTRUE(multigroup_status$succeeded)) {
              as.character(multigroup_status$error %||% "Multi-group latent-moderation bootstrap failed.")
            } else ""
            component_errors <- Filter(nzchar, c(pooled_error, multigroup_error))
            bundle$effect_bootstrap_error <- if (nzchar(pooled_error)) pooled_error else NULL
            bundle$multigroup_moderation_bootstrap_error <- if (nzchar(multigroup_error)) multigroup_error else NULL
            fit_result(bundle)
            send_effect_bootstrap_result_snapshot(bundle)
            structural_canvas_show_notification(
              structural_canvas_bootstrap_terminal_text(if (length(component_errors)) "partial" else "complete", if (isTRUE(job$run_multigroup_moderation)) "SEM and multi-group latent-moderation" else "SEM path, indirect, and total-effect", statedu_current_language(app_language_fn), component_errors),
              type = if (length(component_errors)) "warning" else "message", duration = if (length(component_errors)) 10 else 6
            )
          } else {
            error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
            component_error <- if (nzchar(error_text)) error_text else "Background bootstrap did not complete."
            if (isTRUE(job$run_pooled_effect)) {
              bundle$effect_bootstrap_result <- NULL
              bundle$effect_bootstrap_canceled <- FALSE
              bundle$effect_bootstrap_error <- component_error
            }
            if (isTRUE(job$run_multigroup_moderation)) {
              bundle$multigroup_moderation_bootstrap_canceled <- FALSE
              bundle$multigroup_moderation_bootstrap_error <- component_error
            }
            fit_result(bundle)
            send_effect_bootstrap_result_snapshot(bundle)
            structural_canvas_show_notification(
              structural_canvas_bootstrap_terminal_text("failed", if (isTRUE(job$run_multigroup_moderation)) "SEM and multi-group latent-moderation" else "SEM path, indirect, and total-effect", statedu_current_language(app_language_fn), error_text),
              type = "warning", duration = 10
            )
          }
        }
        effect_bootstrap_job(NULL)
        effect_bootstrap_progress_cache(NULL)
        # Continue the single visible queue only after the effect worker and
        # its progress card are fully gone.
        queued_bundle <- fit_result()
        if (!is.null(queued_bundle) && isTRUE(queued_bundle$cfa_bootstrap_pending)) {
          start_cfa_bootstrap(queued_bundle)
        }
      })
      session$onSessionEnded(function() {
        job <- shiny::isolate(effect_bootstrap_job())
        structural_canvas_stop_effect_bootstrap_job(job)
        structural_canvas_cleanup_effect_bootstrap_job(job)
      })
    }
    if (identical(analysis_type, "plssem")) {
      observeEvent(input[[paste0(prefix, "_pls_bootstrap_stop")]], {
        job <- pls_bootstrap_job()
        if (is.null(job)) return()
        statedu_stop_background_process_tree(job$process)
        structural_canvas_cleanup_pls_bootstrap_job(job)
        pls_bootstrap_job(NULL)
        bundle <- fit_result()
        if (!is.null(bundle)) {
          bundle$pls_bootstrap_result <- structural_canvas_pls_bootstrap_unavailable_result(
            job$nboot, bundle$pls_seed, status = "Canceled",
            failure_message = "Canceled by user"
          )
          fit_result(bundle)
        }
        shiny::removeNotification(paste0(prefix, "-pls-bootstrap-progress"))
        structural_canvas_show_notification(
          structural_canvas_bootstrap_terminal_text("stopped", "PLS/PLSc", statedu_current_language(app_language_fn)),
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
            candidate_progress <- structural_canvas_read_bootstrap_progress_snapshot(job$progress_file)
            if (is.list(candidate_progress)) {
              pls_bootstrap_progress_cache(candidate_progress)
              pls_bootstrap_progress_mtime(current_mtime)
            }
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
          tr <- function(text) structural_canvas_reporting_text(text, statedu_current_language(app_language_fn))
          phase_label <- tr(switch(phase, starting = "Starting", resampling = "Resampling", summarizing = "Preparing summaries", complete = "Complete", "Running"))
          detail <- if (is.finite(percentage)) {
            paste0(phase_label, " ", percentage, "% · ", format(completed, big.mark = ","), "/", format(total, big.mark = ","),
              if (is.finite(rate)) paste0(" · ", sprintf(tr("%s/sec"), format(round(rate, 1), nsmall = 1))) else "",
              " · ", sprintf(tr("elapsed %s s"), elapsed),
              if (stalled) paste0(" · ", tr("Current resample batch is slow; ETA paused."))
              else if (is.finite(remaining)) paste0(" · ", sprintf(tr("ETA %s sec"), remaining)) else "")
          } else {
            paste0(phase_label, " · ", sprintf(tr("%s requested"), format(job$nboot, big.mark = ",")), " · ", sprintf(tr("elapsed %s s"), elapsed))
          }
          structural_canvas_show_notification(
            statedu_bootstrap_status_ui(
              sprintf(structural_canvas_reporting_text("%s bootstrap progress", statedu_current_language(app_language_fn)), "PLS/PLSc"),
              detail, percent = percentage, stop_input_id = paste0(prefix, "_pls_bootstrap_stop"),
              stop_label = structural_canvas_reporting_text("Stop bootstrap", statedu_current_language(app_language_fn)), phase_label = phase_label
            ),
            type = "message", duration = NULL, id = paste0(prefix, "-pls-bootstrap-progress")
          )
          return()
        }
        on.exit(structural_canvas_cleanup_pls_bootstrap_job(job), add = TRUE)
        shiny::removeNotification(paste0(prefix, "-pls-bootstrap-progress"))
        status <- job$process$get_exit_status()
        read_error <- ""
        value <- if (identical(status, 0L) && file.exists(job$result_file)) {
          tryCatch(
            readRDS(job$result_file),
            error = function(error) {
              read_error <<- conditionMessage(error)
              NULL
            }
          )
        } else NULL
        result_contract_ok <- is.list(value) && all(c(
          "nboot", "requested_nboot", "inference_available", "bootstrap_status", "failure_counts"
        ) %in% names(value))
        if (identical(status, 0L) && isTRUE(result_contract_ok)) {
          bundle <- fit_result()
          if (!is.null(bundle)) {
            bundle <- finalize_pls_bootstrap_bundle(bundle, value)
            value <- bundle$pls_bootstrap_result
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
          nonconvergence_n <- suppressWarnings(as.integer(value$nonconvergence_failures %||% 0L))
          inadmissible_n <- suppressWarnings(as.integer(value$inadmissible_failures %||% 0L))
          invalid_n <- suppressWarnings(as.integer(value$invalid_statistic_failures %||% 0L))
          execution_n <- suppressWarnings(as.integer(value$execution_failures %||% 0L))
          canceled_n <- suppressWarnings(as.integer(value$canceled_failures %||% 0L))
          inference_available <- isTRUE(value$inference_available)
          structural_canvas_show_notification(
            structural_canvas_pls_bootstrap_completion_text(valid_n, requested_n,
              c("Timeouts" = timeout_n, "Estimation failures" = estimation_n, "Nonconvergence" = nonconvergence_n, "Inadmissible solutions" = inadmissible_n,
                "Statistic-contract failures" = invalid_n, "Execution failures" = execution_n, "Cancellations" = canceled_n), inference_available, statedu_current_language(app_language_fn)),
            type = if (inference_available) "message" else "warning", duration = 10
          )
        } else {
          error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else read_error
          if (!nzchar(error_text)) {
            status_label <- if (length(status)) as.character(status[[1L]]) else "not recorded"
            error_text <- if (identical(status, 0L)) "The background bootstrap returned no contract-valid result." else paste0("Background bootstrap process exited with status ", status_label, ".")
          }
          bundle <- fit_result()
          if (!is.null(bundle)) {
            failed_value <- structural_canvas_pls_bootstrap_unavailable_result(
              job$nboot, bundle$pls_seed, status = "Failed", failure_message = error_text
            )
            bundle$pls_bootstrap_result <- failed_value
            fit_result(bundle)
            session$sendCustomMessage(
              "custom-model-canvas-result",
              list(
                rootId = paste0(prefix, "-canvas-root"), source = bundle$snapshot,
                result = structural_canvas_result_snapshot(
                  bundle$snapshot, bundle$fit,
                  bundle$result_coefficient %||% "pls_p", failed_value,
                  bundle$result_measurement_coefficient %||% "measurement_p"
                ),
                show = TRUE
              )
            )
          }
          structural_canvas_show_notification(
            structural_canvas_bootstrap_terminal_text("failed", "PLS/PLSc", statedu_current_language(app_language_fn), error_text),
            type = "error", duration = 12
          )
        }
        pls_bootstrap_job(NULL)
      })
      session$onSessionEnded(function() {
        job <- shiny::isolate(pls_bootstrap_job())
        statedu_stop_background_process_tree(job$process)
        structural_canvas_cleanup_pls_bootstrap_job(job)
      })
    }
    command_execute <- structural_canvas_register_interaction_events(
      input, session, dataset_fn, selected_names_fn, variable_table_fn, app_language_fn,
      analysis_type, prefix, canvas_input, run_input, confirm_input,
      fit_result, pending_mi_rows, pending_estimator_snapshot, mark_settings_dirty, execute_analysis
    )
    register_canvas_analysis_commands(
      input, output, session, canvas_input, paste0(prefix, "-canvas-root"),
      dataset_fn, function() list(selected = selected_names_fn(), variables = variable_table_fn(), labels = labels_fn(), categories = category_table_fn()),
      run_fn = command_execute
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
