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
      is_mi_refit <- !is.null(settings) && !is.null(settings$fit)
      settings <- settings %||% list()
      identification <- structural_canvas_identification_diagnostics(snapshot)
      identification_errors <- identification[identification$Severity == "Error", , drop = FALSE]
      if (nrow(identification_errors)) {
        stop(paste0("Model identification check failed: ", paste(paste0(identification_errors$Element, " — ", identification_errors$Message), collapse = "; ")))
      }
      identification_warnings <- identification[identification$Severity == "Warning", , drop = FALSE]
      if (nrow(identification_warnings)) {
        showNotification(paste0("Identification warning: ", paste(paste0(identification_warnings$Element, " — ", identification_warnings$Message), collapse = "; ")), type = "warning", duration = 12)
      }
      estimator <- settings$estimator %||% input[[paste0(prefix, "_estimator")]] %||% "ML"
      missing <- settings$missing %||% input[[paste0(prefix, "_missing")]] %||% "fiml"
      std_lv <- settings$std_lv %||% identical(input[[paste0(prefix, "_scale")]], "variance")
      mi_mode <- settings$mi_mode %||% input[[paste0(prefix, "_mi_mode")]] %||% "theory"
      rmsea_ci <- settings$rmsea_ci %||% as.numeric(input[[paste0(prefix, "_rmsea_ci")]] %||% .90)
      validity_formula <- settings$validity_formula %||% input[[paste0(prefix, "_validity_formula")]] %||% "standardized"
      reliability_bootstrap <- suppressWarnings(as.integer(settings$reliability_bootstrap %||% input[[paste0(prefix, "_reliability_bootstrap")]] %||% 0L))
      if (is.na(reliability_bootstrap) || !reliability_bootstrap %in% c(0L, 500L, 1000L, 2000L)) reliability_bootstrap <- 0L
      reliability_seed <- suppressWarnings(as.integer(settings$reliability_seed %||% input[[paste0(prefix, "_reliability_seed")]] %||% 24680L))
      if (is.na(reliability_seed) || reliability_seed < 1L) reliability_seed <- 24680L
      reliability_ci_method <- structural_canvas_bootstrap_ci_method(settings$reliability_ci_method %||% input[[paste0(prefix, "_reliability_ci_method")]] %||% "percentile")
      bollen_stine_bootstrap <- suppressWarnings(as.integer(settings$bollen_stine_bootstrap %||% input[[paste0(prefix, "_bollen_stine_bootstrap")]] %||% 0L))
      if (is.na(bollen_stine_bootstrap) || !bollen_stine_bootstrap %in% c(0L, 500L, 1000L, 2000L)) bollen_stine_bootstrap <- 0L
      bollen_stine_seed <- suppressWarnings(as.integer(settings$bollen_stine_seed %||% input[[paste0(prefix, "_bollen_stine_seed")]] %||% 97531L))
      if (is.na(bollen_stine_seed) || bollen_stine_seed < 1L) bollen_stine_seed <- 97531L
      htmt_threshold <- as.numeric(settings$htmt_threshold %||% input[[paste0(prefix, "_htmt_threshold")]] %||% .85)
      if (!is.finite(htmt_threshold) || !htmt_threshold %in% c(.85, .90)) htmt_threshold <- .85
      htmt_bootstrap <- suppressWarnings(as.integer(settings$htmt_bootstrap %||% input[[paste0(prefix, "_htmt_bootstrap")]] %||% 0L))
      if (is.na(htmt_bootstrap) || !htmt_bootstrap %in% c(0L, 500L, 1000L, 2000L)) htmt_bootstrap <- 0L
      htmt_seed <- suppressWarnings(as.integer(settings$htmt_seed %||% input[[paste0(prefix, "_htmt_seed")]] %||% 12345L))
      if (is.na(htmt_seed) || htmt_seed < 1L) htmt_seed <- 12345L
      htmt_ci_method <- structural_canvas_bootstrap_ci_method(settings$htmt_ci_method %||% input[[paste0(prefix, "_htmt_ci_method")]] %||% "percentile")
      invariance_enabled <- isTRUE(settings$invariance_enabled %||% input[[paste0(prefix, "_invariance_enabled")]] %||% FALSE)
      invariance_group <- as.character(settings$invariance_group %||% input[[paste0(prefix, "_invariance_group")]] %||% "")
      mi_holdout_enabled <- isTRUE(settings$mi_holdout_enabled %||% input[[paste0(prefix, "_mi_holdout_enabled")]] %||% FALSE)
      mi_holdout_fraction <- as.numeric(settings$mi_holdout_fraction %||% input[[paste0(prefix, "_mi_holdout_fraction")]] %||% .30)
      mi_holdout_seed <- suppressWarnings(as.integer(settings$mi_holdout_seed %||% input[[paste0(prefix, "_mi_holdout_seed")]] %||% 13579L))
      if (is.na(mi_holdout_seed) || mi_holdout_seed < 1L) mi_holdout_seed <- 13579L
      result_coefficient <- settings$result_coefficient %||% input[[paste0(prefix, "_result_coefficient")]] %||% "beta"
      residual_variance_fixes <- settings$residual_variance_fixes %||% numeric(0)
      full_data <- dataset_fn()
      variable_table <- variable_table_fn()
      nominal <- structural_canvas_nominal_indicators(snapshot, variable_table)
      if (length(nominal)) {
        stop(sprintf(
          "Nominal indicators are not supported by standard CFA/SEM: %s. Reclassify them as ordered only when their categories have a meaningful order.",
          paste(nominal, collapse = ", ")
        ))
      }
      ordered <- structural_canvas_ordered_indicators(snapshot, variable_table)
      if (length(ordered) > 0 || identical(toupper(estimator), "WLSMV")) {
        if (toupper(estimator) %in% c("ML", "MLR")) estimator <- "WLSMV"
        if (identical(missing, "fiml")) missing <- "pairwise"
      }
      structural_canvas_validate_holdout_options(
        mi_holdout_enabled, analysis_type, estimator, ordered,
        invariance_enabled, residual_variance_fixes
      )
      if (mi_holdout_enabled) {
        if (!is.null(settings$analysis_data) && !is.null(settings$validation_data)) {
          data <- settings$analysis_data
          validation_data <- settings$validation_data
          holdout_rows <- settings$holdout_rows %||% list()
        } else {
          split <- structural_canvas_holdout_split(full_data, mi_holdout_fraction, mi_holdout_seed)
          data <- split$exploration
          validation_data <- split$validation
          holdout_rows <- list(exploration = split$exploration_rows, validation = split$validation_rows)
        }
      } else {
        data <- full_data
        validation_data <- NULL
        holdout_rows <- list()
      }
      missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
      if (analysis_type %in% c("cfa", "cbsem") && length(missing_covariances)) {
        showNotification(paste0("Missing covariance paths between exogenous latent variables: ", paste(missing_covariances, collapse = ", "), ". These covariances will be fixed to zero."), type = "warning", duration = 10)
      }
      result <- run_structural_canvas_analysis(snapshot, data, analysis_type, estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, nominal = nominal, residual_variance_fixes = residual_variance_fixes)
      if (!isTRUE(result$admissible)) {
        details <- c(
          if (!isTRUE(result$converged)) "the model did not converge",
          if (!isTRUE(result$post_check)) "lavaan post-estimation checks failed",
          if (!isTRUE(result$identified)) paste0("model degrees of freedom are invalid (df = ", format_decimal3(result$df), ")"),
          if (length(result$negative_residuals)) paste0("negative residual variances: ", paste(result$negative_residuals, collapse = ", ")),
          if (length(result$negative_latent_variances)) paste0("negative latent variances: ", paste(result$negative_latent_variances, collapse = ", ")),
          if (isTRUE(result$non_psd_theta)) paste0("residual covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
          if (isTRUE(result$non_psd_latent_covariance)) paste0("latent covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
          if (isTRUE(result$near_singular_theta)) paste0("residual covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
          if (isTRUE(result$near_singular_latent_covariance)) paste0("latent covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
          if (isTRUE(result$non_psd_parameter_covariance)) paste0("parameter-estimate covariance matrix is not positive semidefinite (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
          if (isTRUE(result$near_singular_parameter_covariance)) paste0("parameter-estimate covariance matrix is near singular (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
          if (isTRUE(result$invalid_correlations)) "one or more absolute latent correlations are at least 1"
        )
        showNotification(paste0("Potentially inadmissible solution: ", paste(details, collapse = "; "), ". Interpret fit, AVE, CR, and validity results with caution."), type = "error", duration = NULL)
      }
      conditioning_details <- c(
        if (isTRUE(result$ill_conditioned_theta)) paste0("residual covariance condition number = ", format(result$theta_condition_number, scientific = TRUE, digits = 3)),
        if (isTRUE(result$ill_conditioned_latent_covariance)) paste0("latent covariance condition number = ", format(result$latent_condition_number, scientific = TRUE, digits = 3)),
        if (isTRUE(result$ill_conditioned_parameter_covariance)) paste0("parameter-estimate covariance condition number = ", format(result$parameter_condition_number, scientific = TRUE, digits = 3))
      )
      if (length(conditioning_details)) {
        showNotification(paste0("Numerically ill-conditioned solution: ", paste(conditioning_details, collapse = "; "), ". Small data or specification changes may produce unstable estimates."), type = "warning", duration = 12)
      }
      if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
        if (invariance_enabled) stop("Bollen-Stine bootstrap cannot be combined with measurement-invariance analysis; assess global fit within the appropriate group model instead of the pooled CFA.")
        eligibility <- structural_canvas_bollen_stine_eligibility(result$fit)
        if (!isTRUE(eligibility$available)) stop(eligibility$reason)
      }
      invariance_result <- NULL
      if (identical(analysis_type, "cfa") && invariance_enabled) {
        if (!length(ordered) && !toupper(estimator) %in% c("ML", "MLR")) stop("Continuous-indicator measurement invariance requires ML or MLR.")
        if (length(ordered) && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop("Ordered-indicator measurement invariance requires WLSMV or DWLS.")
        if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop("Select a valid grouping variable for measurement invariance analysis.")
        if (invariance_group %in% lavaan::lavNames(result$fit, "ov")) stop("The grouping variable cannot also be an indicator in the CFA model.")
        group_count <- length(unique(data[[invariance_group]][!is.na(data[[invariance_group]])]))
        if (group_count < 2L || group_count > 20L) stop("The grouping variable must contain between 2 and 20 non-empty groups.")
        invariance_result <- shiny::withProgress(message = "Estimating measurement-invariance models", value = 0, {
          shiny::incProgress(.15, detail = "Configural, metric, scalar, and strict models")
          value <- structural_canvas_measurement_invariance(result$syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered)
          shiny::incProgress(.85, detail = "Preparing robust comparisons")
          value
        })
      }
      reliability_bootstrap_result <- NULL
      if (identical(analysis_type, "cfa") && reliability_bootstrap > 0L) {
        structural_canvas_validate_model_based_bootstrap(result$fit, "AVE/reliability bootstrap")
        reliability_bootstrap_result <- shiny::withProgress(message = "Estimating AVE and reliability confidence intervals", value = 0, {
          shiny::incProgress(.05, detail = paste0(reliability_bootstrap, " case-resampling replicates"))
          value <- structural_canvas_reliability_bootstrap(
            result$syntax, data, reps = reliability_bootstrap, confidence = .95, seed = reliability_seed,
            estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, formula_mode = validity_formula,
            original_fit = result$fit,
            ci_method = reliability_ci_method,
            progress = function(done, total, valid) {
              shiny::setProgress(
                value = .05 + .90 * (as.numeric(done) / max(1, as.numeric(total))),
                detail = sprintf("Reliability bootstrap %s/%s; valid replicates %s", done, total, valid)
              )
            }
          )
          if (nrow(value)) {
            point <- structural_canvas_reliability_estimates(result$fit, validity_formula)
            statistic_column <- c(AVE = "AVE", CR = "CR", Alpha = "Alpha", Omega = "Omega")
            value$Estimate <- vapply(seq_len(nrow(value)), function(index) {
              row <- point[point$Factor == value$Factor[[index]], , drop = FALSE]
              column <- statistic_column[[value$Statistic[[index]]]]
              if (nrow(row) && column %in% names(row)) as.numeric(row[[column]][[1L]]) else NA_real_
            }, numeric(1))
            value$`Valid %` <- 100 * value[["Valid replicates"]] / value[["Requested replicates"]]
            value <- value[, c("Factor", "Statistic", "Estimate", "Lower", "Upper", "CI method", "Valid replicates", "Requested replicates", "Valid %", "Status"), drop = FALSE]
          }
          shiny::incProgress(.95, detail = paste0("Preparing ", if (identical(reliability_ci_method, "bca")) "BCa" else "percentile", " intervals"))
          value
        })
      }
      bollen_stine_result <- NULL
      if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
        bollen_stine_result <- shiny::withProgress(message = "Estimating Bollen-Stine global-fit p value", value = 0, {
          shiny::incProgress(.05, detail = paste0(bollen_stine_bootstrap, " transformed-data bootstrap replicates"))
          value <- structural_canvas_bollen_stine(result$fit, bollen_stine_bootstrap, bollen_stine_seed)
          shiny::incProgress(.95, detail = "Preparing bootstrap goodness-of-fit result")
          value
        })
      }
      mi <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_mi_refits(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = mi_mode, ordered = ordered) else NULL
      htmt_bootstrap_result <- NULL
      if (analysis_type %in% c("cfa", "cbsem") && htmt_bootstrap > 0L) {
        standardized_for_htmt <- lavaan::standardizedSolution(result$fit)
        observed_for_htmt <- lavaan::lavNames(result$fit, "ov")
        loadings_for_htmt <- standardized_for_htmt[
          standardized_for_htmt$op == "=~" & standardized_for_htmt$rhs %in% observed_for_htmt,
          c("lhs", "rhs"), drop = FALSE
        ]
        factor_names_for_htmt <- unique(loadings_for_htmt$lhs)
        if (length(factor_names_for_htmt) >= 2L) {
          indicators_for_htmt <- stats::setNames(lapply(factor_names_for_htmt, function(name) {
            unique(loadings_for_htmt$rhs[loadings_for_htmt$lhs == name])
          }), factor_names_for_htmt)
          htmt_bootstrap_result <- shiny::withProgress(
            message = "Estimating HTMT bootstrap confidence intervals",
            value = 0,
            {
              shiny::incProgress(.1, detail = paste0(htmt_bootstrap, " case-resampling replicates"))
              value <- structural_canvas_htmt_bootstrap(
                data, indicators_for_htmt, reps = htmt_bootstrap, confidence = .95,
                seed = htmt_seed, ordered = ordered, threshold = htmt_threshold,
                ci_method = htmt_ci_method,
                progress = function(done, total, valid) {
                  shiny::setProgress(
                    value = .10 + .80 * (as.numeric(done) / max(1, as.numeric(total))),
                    detail = sprintf("HTMT bootstrap %s/%s; valid replicates %s", done, total, valid)
                  )
                }
              )
              shiny::incProgress(.9, detail = "Preparing interval estimates")
              value
            }
          )
        }
      }
      baseline_fit <- if (is_mi_refit) settings$baseline_fit %||% settings$fit else result$fit
      baseline_diagnostics <- if (is_mi_refit) settings$baseline_diagnostics %||% settings$diagnostics else result
      baseline_syntax <- if (is_mi_refit) settings$baseline_syntax %||% settings$syntax else result$syntax
      holdout_comparison <- NULL
      if (mi_holdout_enabled && identical(settings$comparison_type %||% "", "mi") && !is.null(validation_data)) {
        holdout_comparison <- structural_canvas_holdout_model_comparison(
          baseline_syntax, result$syntax, validation_data,
          estimator = estimator, missing = missing, std_lv = std_lv, ci_level = rmsea_ci
        )
      }
      fit_result(list(
        fit = result$fit, syntax = result$syntax, snapshot = snapshot, mi = mi, mi_mode = mi_mode,
        rmsea_ci = rmsea_ci, validity_formula = validity_formula,
        reliability_bootstrap = reliability_bootstrap, reliability_seed = reliability_seed,
        reliability_ci_method = reliability_ci_method,
        reliability_bootstrap_result = reliability_bootstrap_result,
        bollen_stine_bootstrap = bollen_stine_bootstrap, bollen_stine_seed = bollen_stine_seed,
        bollen_stine_result = bollen_stine_result,
        htmt_threshold = htmt_threshold, htmt_bootstrap = htmt_bootstrap, htmt_seed = htmt_seed,
        htmt_ci_method = htmt_ci_method,
        htmt_bootstrap_result = htmt_bootstrap_result,
        invariance_enabled = invariance_enabled, invariance_group = invariance_group, invariance_result = invariance_result,
        mi_holdout_enabled = mi_holdout_enabled, mi_holdout_fraction = mi_holdout_fraction, mi_holdout_seed = mi_holdout_seed,
        analysis_data = data, validation_data = validation_data, holdout_rows = holdout_rows, holdout_comparison = holdout_comparison,
        estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered,
        result_coefficient = result_coefficient, diagnostics = result,
        baseline_fit = baseline_fit, modified_from_baseline = is_mi_refit || isTRUE(settings$modified_from_baseline),
        baseline_syntax = baseline_syntax,
        baseline_diagnostics = baseline_diagnostics,
        comparison_label = settings$comparison_label %||% NULL,
        comparison_type = settings$comparison_type %||% NULL,
        residual_variance_fixes = residual_variance_fixes,
        identification = identification,
        mi_history = settings$mi_history %||% data.frame()
      ))
      session$sendCustomMessage(
        "custom-model-canvas-result",
        list(
          rootId = paste0(prefix, "-canvas-root"),
          source = snapshot,
          result = structural_canvas_result_snapshot(snapshot, result$fit, result_coefficient),
          show = TRUE
        )
      )
      result
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
