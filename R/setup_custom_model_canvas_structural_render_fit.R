structural_canvas_pls_quality_number <- function(values, direction = "single") {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (!length(values)) return("")
  value <- switch(
    direction,
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    mean = mean(values, na.rm = TRUE),
    values[[1L]]
  )
  format_decimal3(value)
}

structural_canvas_pls_quality_predictive_label <- function(bundle) {
  if (is.null(bundle$pls_predict_result)) return("Not executed")
  tables <- structural_canvas_pls_predict_tables(bundle$pls_predict_result)
  items <- tables$items
  if (!nrow(items) || !"Assessment" %in% names(items)) return("Executed; no comparable indicator metrics")
  assessments <- as.character(items$Assessment %||% character(0))
  paste0(
    sum(assessments == "PLS lower error", na.rm = TRUE),
    "/",
    length(assessments),
    " indicator metrics favor PLS over LM"
  )
}

structural_canvas_pls_quality_rows <- function(bundle) {
  if (is.null(bundle) || is.null(bundle$fit) || !inherits(bundle$fit, "pls_model")) {
    return(data.frame(Item = character(0), Value = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  summary_fit <- tryCatch(summary(bundle$fit), error = function(error) NULL)
  if (is.null(summary_fit)) {
    return(data.frame(Item = character(0), Value = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  reliability <- as.data.frame(summary_fit$reliability %||% data.frame(), check.names = FALSE)
  loadings <- suppressWarnings(abs(as.matrix(summary_fit$loadings %||% matrix(numeric(0), 0L, 0L))))
  if (length(loadings)) loadings[loadings == 0] <- NA_real_
  assigned_loadings <- if (length(loadings)) apply(loadings, 1L, function(row) {
    row <- row[is.finite(row)]
    if (!length(row)) NA_real_ else max(row, na.rm = TRUE)
  }) else numeric(0)
  item_vif <- unlist(summary_fit$validity$vif_items %||% list(), use.names = FALSE)
  inner_vif <- unlist(summary_fit$vif_antecedents %||% list(), use.names = FALSE)
  htmt <- suppressWarnings(as.numeric(as.matrix(summary_fit$validity$htmt %||% matrix(numeric(0), 0L, 0L))))
  htmt <- htmt[is.finite(htmt)]
  paths <- suppressWarnings(as.matrix(summary_fit$paths %||% matrix(numeric(0), 0L, 0L)))
  r2 <- if (length(paths) && "R^2" %in% rownames(paths)) suppressWarnings(as.numeric(paths["R^2", ])) else numeric(0)
  f_square <- suppressWarnings(as.numeric(as.matrix(summary_fit$fSquare %||% matrix(numeric(0), 0L, 0L))))
  f_square <- f_square[is.finite(f_square) & f_square > 0]
  missing <- summary_fit$missing_data$method %||% "not recorded"
  weight_diff <- bundle$fit$weightDiff %||% NA_real_
  data.frame(
    Item = c(
      "PLS algorithm iterations",
      "Final weight difference",
      "Missing-data method",
      "Min outer loading",
      "Min rhoC",
      "Min AVE",
      "Max HTMT",
      "Max item VIF",
      "Max inner VIF",
      "Min endogenous R2",
      "Max f2",
      "PLSpredict summary"
    ),
    Value = c(
      as.character(summary_fit$iterations %||% bundle$fit$iterations %||% ""),
      structural_canvas_pls_quality_number(weight_diff),
      as.character(missing),
      structural_canvas_pls_quality_number(assigned_loadings, "min"),
      structural_canvas_pls_quality_number(reliability$rhoC, "min"),
      structural_canvas_pls_quality_number(reliability$AVE, "min"),
      structural_canvas_pls_quality_number(htmt, "max"),
      structural_canvas_pls_quality_number(item_vif, "max"),
      structural_canvas_pls_quality_number(inner_vif, "max"),
      structural_canvas_pls_quality_number(r2, "min"),
      structural_canvas_pls_quality_number(f_square, "max"),
      structural_canvas_pls_quality_predictive_label(bundle)
    ),
    Guidance = c(
      "Algorithm diagnostic; inspect unusually high iteration counts.",
      "Smaller values indicate stable outer-weight convergence.",
      "Report this because PLS does not use lavaan FIML/pairwise options.",
      "Review reflective indicators below .70; retain lower loadings only with theory.",
      "Review construct reliability below .70.",
      "Review convergent validity below .50.",
      "Review discriminant validity near or above .85/.90.",
      "Review indicator collinearity above 3.3 or 5.",
      "Review structural predictor collinearity above 3.3 or 5.",
      "Report explanatory power for endogenous constructs.",
      "Interpret as structural effect size; .02/.15/.35 are descriptive anchors.",
      "Out-of-sample prediction is strongest when PLS error is lower than LM."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

structural_canvas_pls_quality_result_ui <- function(bundle, language = statedu_initial_language()) {
  rows <- structural_canvas_pls_quality_rows(bundle)
  if (!nrow(rows)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  div(
    class = "result-section regression-result-panel structural-pls-quality-result",
    h4(if (ko) "PLS-SEM quality checklist" else "PLS-SEM quality checklist"),
    structural_canvas_basic_html_table(rows),
    tags$p(
      class = "structural-result-note",
      if (ko) {
        "This checklist summarizes PLS-SEM measurement, collinearity, explanatory-power, and predictive-quality boundary conditions for reporting and review."
      } else {
        "This checklist summarizes PLS-SEM measurement, collinearity, explanatory-power, and predictive-quality boundary conditions for reporting and review."
      }
    )
  )
}

structural_canvas_pls_predict_result_ui <- function(bundle, language = statedu_initial_language()) {
  if (is.null(bundle) || is.null(bundle$pls_predict_result)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  tables <- structural_canvas_pls_predict_tables(bundle$pls_predict_result)
  item_table <- tables$items
  construct_table <- tables$constructs
  for (table_name in c("item_table", "construct_table")) {
    table <- get(table_name)
    if (nrow(table)) {
      for (name in names(table)) {
        if (is.numeric(table[[name]])) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
      }
      assign(table_name, table)
    }
  }
  div(class = "result-section regression-result-panel structural-pls-predict-result",
    h4(if (ko) "PLSpredict 예측 진단" else "PLSpredict predictive assessment"),
    tags$p(class = "structural-result-note", if (ko) {
      paste0("Direct Antecedents 방식, ", bundle$pls_predict_result$folds, "-fold, 반복 ", bundle$pls_predict_result$reps, "회 기준입니다. PLS - LM 값이 음수이면 PLS의 out-of-sample 예측오차가 선형모형 기준값보다 작습니다.")
    } else {
      paste0("Direct Antecedents scheme, ", bundle$pls_predict_result$folds, "-fold, ", bundle$pls_predict_result$reps, " repetition(s). Negative PLS - LM values indicate lower out-of-sample prediction error for PLS than for the linear-model benchmark.")
    }),
    if (nrow(item_table)) tagList(
      tags$h5(if (ko) "Indicator별 out-of-sample 예측오차" else "Indicator-level out-of-sample prediction error"),
      structural_canvas_basic_html_table(item_table)
    ),
    if (nrow(construct_table)) tagList(
      tags$h5(if (ko) "Construct-level prediction error" else "Construct-level prediction error"),
      structural_canvas_basic_html_table(construct_table)
    )
  )
}

structural_canvas_register_fit_diagnostic_outputs <- function(output, prefix, analysis_type, fit_result, result_table, dataset_fn, app_language_fn) {
output[[paste0(prefix, "_result_fit")]] <- renderUI({
  structural_canvas_fit_table_result_ui(fit_result(), result_table("fit"))
})
if (identical(analysis_type, "plssem")) {
  output[[paste0(prefix, "_result_pls_quality")]] <- renderUI({
    structural_canvas_pls_quality_result_ui(fit_result(), statedu_current_language(app_language_fn))
  })
  output[[paste0(prefix, "_result_pls_predict")]] <- renderUI({
    structural_canvas_pls_predict_result_ui(fit_result(), statedu_current_language(app_language_fn))
  })
  return(invisible(TRUE))
}
output[[paste0(prefix, "_result_lavaan_quality")]] <- renderUI({
  structural_canvas_lavaan_quality_result_ui(fit_result(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_identification")]] <- renderUI({
  structural_canvas_identification_result_ui(fit_result())
})
output[[paste0(prefix, "_result_normality")]] <- renderUI({
  structural_canvas_normality_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_risk_diagnostics")]] <- renderUI({
  structural_canvas_risk_diagnostics_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_missing_outliers")]] <- renderUI({
  structural_canvas_missing_outliers_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_fit_difference")]] <- renderUI({
  structural_canvas_fit_difference_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_invariance")]] <- renderUI({
  structural_canvas_invariance_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_fit_guidance")]] <- renderUI({
  structural_canvas_fit_guidance_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_rmsea_tests")]] <- renderUI({
  structural_canvas_rmsea_tests_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_information_criteria")]] <- renderUI({
  structural_canvas_information_criteria_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_bollen_stine")]] <- renderUI({
  structural_canvas_bollen_stine_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_heywood")]] <- renderUI({
  structural_canvas_heywood_result_ui(fit_result(), dataset_fn(), prefix, analysis_type)
})
  invisible(TRUE)
}
