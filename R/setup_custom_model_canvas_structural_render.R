structural_canvas_register_result_outputs <- function(input, output, prefix, canvas_output, analysis_type, selected_names_fn, variable_table_fn, labels_fn, app_language_fn, fit_result, result_table) {
output[[canvas_output]] <- renderUI({
  structural_equation_workspace(selected_names_fn(), variable_table_fn(), labels_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_results")]] <- renderUI({
  shiny::req(!is.null(fit_result()))
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  div(
    class = "structural-analysis-results regression-results",
    h3(if (ko) "분석 결과" else "Analysis Results"),
    if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_reproducibility"), if (ko) "분석 기록 다운로드" else "Download analysis record", class = "btn btn-default btn-sm"),
    if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_tables"), if (ko) "결과표 Excel 다운로드" else "Download result tables", class = "btn btn-default btn-sm"),
    div(class = "result-section regression-result-panel", h4(if (ko) "1. 모형 개요" else "1. Model overview"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview")))),
    uiOutput(paste0(prefix, "_result_identification")),
    uiOutput(paste0(prefix, "_result_normality")),
    uiOutput(paste0(prefix, "_result_missing_outliers")),
    uiOutput(paste0(prefix, "_result_risk_diagnostics")),
    uiOutput(paste0(prefix, "_result_heywood")),
    div(class = "result-section regression-result-panel", h4(if (ko) "2. 모형 적합도" else "2. Model fit"), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit"))), uiOutput(paste0(prefix, "_result_fit_guidance")), uiOutput(paste0(prefix, "_result_rmsea_tests")), uiOutput(paste0(prefix, "_result_information_criteria")), uiOutput(paste0(prefix, "_result_bollen_stine")), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit_difference")))),
    uiOutput(paste0(prefix, "_result_invariance")),
    div(class = "result-section regression-result-panel", h4(if (ko) "3. 잠재 구성개념 상관, 신뢰도 및 수렴·판별타당도" else "3. Latent construct correlations, reliability, and convergent/discriminant validity"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_validity"))), uiOutput(paste0(prefix, "_result_latent_correlation_ci")), uiOutput(paste0(prefix, "_result_validity_note")), uiOutput(paste0(prefix, "_result_reliability_bootstrap")), uiOutput(paste0(prefix, "_result_factor_scores")), uiOutput(paste0(prefix, "_result_htmt"))),
    div(
      class = "result-section regression-result-panel structural-measurement-result",
      h4(if (ko) "4. 측정모형" else "4. Measurement model"),
      div(class = "table-responsive", tableOutput(paste0(prefix, "_result_measurement"))),
      tags$p(class = "structural-result-note", "* Fixed reference loading; its unstandardized SE, z, and p are not estimated. The standardized loading remains a derived estimate and therefore has a confidence interval."),
      tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized loadings. A fixed reference loading has a degenerate B interval at its fixed value."),
      tags$p(class = "structural-result-note", "Standardized-loading confidence intervals are 95% delta-method intervals from lavaan; robust fitted models use the fitted model's robust covariance information."),
      tags$p(class = "structural-result-note", "R² confidence intervals are obtained by complementing the standardized residual-variance interval (lower = 1 − residual upper; upper = 1 − residual lower)."),
      tags$p(class = "structural-result-note", "Std. residual variance is the standardized diagonal residual variance for each indicator. † marks an unavailable residual value, a residual outside [0, 1], or an R² interval extending beyond [0, 1], including a negative-residual Heywood case."),
      tags$p(class = "structural-result-note", "Guidance prioritizes inadmissible residual variance, then cross-loading, a standardized-loading CI containing 0, and |β| < .40. The .40 value is a descriptive review guideline rather than a universal item-retention rule."),
      tags$p(class = "structural-result-note", "Cross-loaded indicators require theory-based interpretation; simple-structure reliability and discriminant-validity summaries, especially HTMT, may be unavailable or require caution.")
    ),
    uiOutput(paste0(prefix, "_result_higher_order")),
    uiOutput(paste0(prefix, "_result_residuals")),
    uiOutput(paste0(prefix, "_result_mi_holdout")),
    uiOutput(paste0(prefix, "_result_mi_history")),
    if (analysis_type != "plssem") div(class = "result-section regression-result-panel structural-mi-result", h4(if (ko) "6. 수정지수(MI)" else "6. Modification indices (MI)"), uiOutput(paste0(prefix, "_result_mi")))
  )
})
  structural_canvas_register_fit_diagnostic_outputs(
    output, prefix, analysis_type, fit_result, result_table, dataset_fn, app_language_fn
  )
output[[paste0(prefix, "_result_latent_correlation_ci")]] <- renderUI({
  bundle <- fit_result()
  values <- structural_canvas_latent_correlation_intervals(bundle$fit, level = .95)
  if (!nrow(values)) return(NULL)
  values$r <- vapply(values$r, format_decimal3, character(1))
  values[["CI lower"]] <- vapply(values[["CI lower"]], format_decimal3, character(1))
  values[["CI upper"]] <- vapply(values[["CI upper"]], format_decimal3, character(1))
  values$p <- vapply(values$p, format_p, character(1))
  values$p[values$Type == "Fixed"] <- "—"
  tags$div(
    class = "structural-latent-correlation-ci",
    tags$h5("Latent correlation confidence intervals"),
    tags$div(class = "table-responsive", tags$table(
      class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", "Intervals are 95% delta-method intervals for explicitly estimated or fixed latent covariance paths. 'CI reaches |1|' flags an interval touching an inadmissible correlation boundary; implied correlations without an explicit covariance parameter are not assigned a delta-method interval here.")
  )
})
output[[paste0(prefix, "_result_validity_note")]] <- renderUI({
  bundle <- fit_result()
  validity_values <- result_table("validity")
  abnormal_reliability <- any(grepl("†", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
  single_indicator <- any(grepl("‡", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
  constrained_single_indicator <- any(grepl("¶", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
  orthogonal_not_assessed <- any(grepl("§", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
  theta <- as.matrix(lavaan::lavInspect(bundle$fit, "theta"))
  correlated_errors <- length(theta) > 1L && any(abs(theta[row(theta) != col(theta)]) > sqrt(.Machine$double.eps), na.rm = TRUE)
  snapshot <- bundle$snapshot %||% list()
  missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
  has_higher_order <- any(vapply(snapshot$edges %||% list(), function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), logical(1)))
  validity_loadings <- lavaan::standardizedSolution(bundle$fit)
  validity_loadings <- validity_loadings[validity_loadings$op == "=~" & validity_loadings$rhs %in% lavaan::lavNames(bundle$fit, "ov"), c("lhs", "rhs"), drop = FALSE]
  cross_loaded_indicators <- unique(validity_loadings$rhs[duplicated(validity_loadings$rhs) | duplicated(validity_loadings$rhs, fromLast = TRUE)])
  tagList(
    tags$p(class = "structural-result-note", "Diagonal values in parentheses are sqrt(AVE); lower-triangle values are latent correlations. Max |r| is compared with sqrt(AVE). The remaining columns report the number of indicators (k), AVE, CR, Cronbach's alpha, and McDonald's omega total."),
    tags$p(class = "structural-result-note", "Fornell-Larcker is marked 'Criterion met' when sqrt(AVE) is greater than the factor's largest absolute correlation; otherwise it is marked 'Review needed'."),
    tags$p(class = "structural-result-note", "Guidance uses commonly cited descriptive cutoffs (AVE ≥ .50; CR, Cronbach's alpha, and omega total ≥ .70). These are heuristics rather than universal pass/fail rules and should be interpreted with construct breadth, item count, model admissibility, and study purpose."),
    if (length(missing_covariances)) tags$p(class = "structural-result-note", paste0("Caution: missing exogenous latent covariance paths (", paste(missing_covariances, collapse = ", "), ") are fixed to zero.")),
    if (correlated_errors) tags$p(class = "structural-result-note", "CR incorporates the estimated measurement-error covariances in the residual covariance matrix."),
    if (abnormal_reliability) tags$p(class = "structural-result-note", "† AVE or a reliability coefficient is unavailable or outside [0, 1], indicating unusual item covariances, an inadmissible solution, or an unidentified calculation."),
    if (single_indicator) tags$p(class = "structural-result-note", "‡ AVE and CR are not reported for a single-indicator factor without externally justified reliability constraints."),
    if (constrained_single_indicator) tags$p(class = "structural-result-note", "¶ The single-indicator factor uses an externally justified fixed residual variance. AVE, CR, and omega total reflect that imposed reliability constraint rather than independently estimated internal consistency; Cronbach's alpha and Fornell-Larcker are not assessed."),
    if (orthogonal_not_assessed) tags$p(class = "structural-result-note", "§ Fornell-Larcker was not assessed because one or more exogenous latent covariances were fixed to zero by omitted covariance paths."),
    if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, AVE, CR, and omega use the standardized latent-response solution; Cronbach's alpha uses lavaan's polychoric correlation matrix (ordinal alpha)."),
    if (!length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "Cronbach's alpha uses the analyzed indicators' sample covariance matrix. Omega is model-based and incorporates estimated residual covariances."),
    tags$p(class = "structural-result-note", "In this table, omega total uses the same model-implied loadings and residual covariance matrix as CR; under the current congeneric scoring specification it therefore equals CR. Both labels are retained for reporting clarity."),
    if (has_higher_order) tags$p(class = "structural-result-note", "AVE, CR, Fornell-Larcker, and HTMT are reported for first-order factors with observed indicators; higher-order factors are excluded from these reliability and discriminant-validity calculations."),
    if (length(cross_loaded_indicators)) tags$p(class = "structural-result-note", paste0("Caution: cross-loaded indicators (", paste(cross_loaded_indicators, collapse = ", "), ") appear in more than one factor. Factor-specific AVE/CR remain descriptive, while simple-structure discriminant-validity interpretations require particular caution.")),
    if (!isTRUE(bundle$diagnostics$admissible %||% TRUE)) tags$p(class = "structural-result-note", "Caution: the fitted solution failed one or more admissibility checks; AVE, CR, and discriminant-validity results should not be interpreted as final.")
  )
})
output[[paste0(prefix, "_result_factor_scores")]] <- renderUI({
  bundle <- fit_result()
  values <- structural_canvas_factor_score_quality(bundle$fit)
  if (!nrow(values)) return(NULL)
  values$Determinacy <- vapply(values$Determinacy, format_decimal3, character(1))
  values[["Score reliability"]] <- vapply(values[["Score reliability"]], format_decimal3, character(1))
  tagList(
    tags$h5("Factor-score quality"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", "Determinacy is the correlation between regression factor scores and the latent factor; score reliability is its square. Descriptive guidance uses .90 as strong and .80 as a cautious-use threshold. These indices concern estimated factor scores for downstream or individual-level use and are not substitutes for CR, omega, validity evidence, or model admissibility."),
    if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, factor-score quality is conditional on the fitted latent-response WLSMV model and category thresholds.")
  )
})
output[[paste0(prefix, "_result_reliability_bootstrap")]] <- renderUI({
  bundle <- fit_result()
  values <- bundle$reliability_bootstrap_result %||% NULL
  requested <- as.integer(bundle$reliability_bootstrap %||% 0L)
  if (requested <= 0L) return(tags$p(class = "structural-result-note", "AVE, CR, Cronbach's alpha, and omega are point estimates. Select AVE/reliability bootstrap CI in the analysis options when interval estimates are required."))
  if (is.null(values) || !nrow(values)) return(tags$p(class = "structural-result-note", "AVE/reliability bootstrap intervals could not be estimated because no resample produced usable estimates."))
  reliability_ci_method <- structural_canvas_bootstrap_ci_method(bundle$reliability_ci_method %||% "percentile")
  reliability_ci_label <- if (identical(reliability_ci_method, "bca")) "BCa" else "percentile"
  incomplete <- values[["Valid replicates"]] < values[["Requested replicates"]]
  caution_intervals <- values$Status == "Caution"
  unreliable_intervals <- values$Status == "Unreliable"
  boundary_interval <- !is.finite(values$Lower) | !is.finite(values$Upper) | values$Lower < 0 | values$Upper > 1
  if (!"CI method" %in% names(values)) values[["CI method"]] <- "Percentile"
  bca_unavailable <- "CI method" %in% names(values) && any(values[["CI method"]] == "BCa unavailable")
  values$Statistic[values$Statistic == "Alpha"] <- "Cronbach's α"
  values$Statistic[values$Statistic == "Omega"] <- "McDonald's ωtotal"
  values$Estimate <- paste0(vapply(values$Estimate, format_decimal3, character(1)), ifelse(!is.finite(values$Estimate) | values$Estimate < 0 | values$Estimate > 1, "†", ""))
  values$Lower <- paste0(vapply(values$Lower, format_decimal3, character(1)), ifelse(boundary_interval, "†", ""))
  values$Upper <- paste0(vapply(values$Upper, format_decimal3, character(1)), ifelse(boundary_interval, "†", ""))
  values[["Valid %"]] <- paste0(vapply(values[["Valid %"]], format_decimal3, character(1)), "%")
  names(values)[names(values) == "Lower"] <- "95% CI lower"
  names(values)[names(values) == "Upper"] <- "95% CI upper"
  tagList(
    tags$h5(paste0("AVE and reliability ", reliability_ci_label, " bootstrap intervals (", requested, " resamples; seed = ", bundle$reliability_seed, ")")),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", paste0("Intervals use case resampling", if (identical(reliability_ci_method, "bca")) " with leave-one-out jackknife acceleration" else " percentile bootstrap", " and the selected ", if (identical(bundle$validity_formula, "model_implied")) "model-implied parameter" else "standardized-loading", " AVE/CR formula. McDonald's omega total follows the same fitted congeneric scoring formula as CR in this output.")),
    if (bca_unavailable) tags$p(class = "structural-result-note", "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that statistic; increase valid replicates or use percentile CI for reporting."),
    if (any(boundary_interval)) tags$p(class = "structural-result-note", "† marks an estimate or interval extending outside the admissible [0, 1] coefficient range. Do not truncate the interval for reporting; investigate model admissibility, sample instability, item covariance structure, and failed resamples."),
    if (any(incomplete)) tags$p(class = "structural-result-note", "Some resamples failed the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA, or yielded unavailable statistics. Interpret intervals cautiously when the valid-replicate count is materially below the requested count."),
    if (any(caution_intervals)) tags$p(class = "structural-result-note", "Caution indicates that 50% to less than 80% of requested resamples yielded the statistic. Treat the percentile limits as unstable and report the valid-replicate count."),
    if (any(unreliable_intervals)) tags$p(class = "structural-result-note", "Unreliable indicates that fewer than 50% of requested resamples yielded the statistic. The displayed quantiles are diagnostic only and should not be reported as a defensible confidence interval; resolve convergence, admissibility, sparse-category, or specification problems first.")
  )
})
output[[paste0(prefix, "_result_htmt")]] <- renderUI({
  bundle <- fit_result()
  fit <- bundle$fit
  standardized <- lavaan::standardizedSolution(fit)
  loadings <- standardized[standardized$op == "=~", c("lhs", "rhs"), drop = FALSE]
  loadings <- loadings[loadings$rhs %in% lavaan::lavNames(fit, "ov"), , drop = FALSE]
  factor_names <- unique(loadings$lhs)
  if (length(factor_names) < 2L) return(NULL)
  indicators_by_factor <- stats::setNames(lapply(factor_names, function(name) unique(loadings$rhs[loadings$lhs == name])), factor_names)
  sample_statistics <- lavaan::lavInspect(fit, "sampstat")
  sample_covariance <- sample_statistics$cov %||% NULL
  if (is.null(sample_covariance)) return(tags$p(class = "structural-result-note", "HTMT is not currently displayed for multigroup sample statistics."))
  sample_correlations <- stats::cov2cor(as.matrix(sample_covariance))
  threshold <- as.numeric(bundle$htmt_threshold %||% .85)
  htmt <- structural_canvas_htmt(sample_correlations, indicators_by_factor, threshold)
  matrix_values <- matrix("", nrow = length(factor_names), ncol = length(factor_names) + 1L)
  colnames(matrix_values) <- c("Factor", factor_names)
  for (row in seq_along(factor_names)) {
    matrix_values[row, 1L] <- factor_names[[row]]
    for (column in seq_along(factor_names)) {
      if (row == column) matrix_values[row, column + 1L] <- "—"
      else if (row > column) matrix_values[row, column + 1L] <- format_decimal3(htmt$matrix[row, column])
    }
  }
  pair_table <- htmt$pairs
  pair_table$HTMT <- vapply(pair_table$HTMT, format_decimal3, character(1))
  pair_table$Reason[!nzchar(pair_table$Reason)] <- "—"
  bootstrap_reps <- as.integer(bundle$htmt_bootstrap %||% 0L)
  bootstrap_seed <- as.integer(bundle$htmt_seed %||% 12345L)
  htmt_ci_method <- structural_canvas_bootstrap_ci_method(bundle$htmt_ci_method %||% "percentile")
  htmt_ci_label <- if (identical(htmt_ci_method, "bca")) "BCa" else "percentile"
  bootstrap_table <- NULL
  bootstrap_incomplete <- FALSE
  bootstrap_caution <- FALSE
  bootstrap_unreliable <- FALSE
  bootstrap_bca_unavailable <- FALSE
  if (bootstrap_reps > 0L) {
    bootstrap_table <- bundle$htmt_bootstrap_result %||% NULL
    if (!is.null(bootstrap_table)) {
      bootstrap_incomplete <- any(bootstrap_table[["Valid replicates"]] < bootstrap_reps)
      bootstrap_caution <- any(bootstrap_table$Status == "Caution")
      bootstrap_unreliable <- any(bootstrap_table$Status == "Unreliable")
      bootstrap_bca_unavailable <- "CI method" %in% names(bootstrap_table) && any(bootstrap_table[["CI method"]] == "BCa unavailable")
      names(bootstrap_table)[names(bootstrap_table) == "Lower"] <- "95% CI lower"
      names(bootstrap_table)[names(bootstrap_table) == "Upper"] <- "95% CI upper"
      names(bootstrap_table)[names(bootstrap_table) == "One-sided upper"] <- "One-sided 95% upper"
      bootstrap_table[["95% CI lower"]] <- vapply(bootstrap_table[["95% CI lower"]], format_decimal3, character(1))
      bootstrap_table[["95% CI upper"]] <- vapply(bootstrap_table[["95% CI upper"]], format_decimal3, character(1))
      bootstrap_table[["One-sided 95% upper"]] <- vapply(bootstrap_table[["One-sided 95% upper"]], format_decimal3, character(1))
      bootstrap_table[["Valid %"]] <- paste0(vapply(bootstrap_table[["Valid %"]], format_decimal3, character(1)), "%")
    }
  }
  tagList(
    tags$h5(paste0("HTMT (threshold = ", format(threshold, nsmall = 2L), ")")),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-matrix",
      tags$thead(tags$tr(lapply(colnames(matrix_values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(matrix_values)), function(index) tags$tr(lapply(as.character(matrix_values[index, ]), tags$td))))
    )),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-criterion",
      tags$thead(tags$tr(lapply(c("Factor 1", "Factor 2", "HTMT", "Criterion", "Reason"), tags$th))),
      tags$tbody(lapply(seq_len(nrow(pair_table)), function(index) tags$tr(lapply(as.character(pair_table[index, ]), tags$td))))
    )),
    if (!is.null(bootstrap_table)) tagList(
      tags$h5(paste0("HTMT ", htmt_ci_label, " bootstrap confidence intervals (", bootstrap_reps, " resamples; seed = ", bootstrap_seed, ")")),
      tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-bootstrap",
        tags$thead(tags$tr(lapply(names(bootstrap_table), tags$th))),
        tags$tbody(lapply(seq_len(nrow(bootstrap_table)), function(index) tags$tr(lapply(as.character(bootstrap_table[index, ]), tags$td))))
      ))
    ),
    tags$p(class = "structural-result-note", "HTMT uses absolute item correlations. Values below the selected threshold are marked 'Criterion met'."),
    if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, HTMT uses lavaan's polychoric latent-response correlations."),
    if (bootstrap_reps <= 0L) tags$p(class = "structural-result-note", "HTMT point estimates are descriptive. Select HTMT bootstrap CI in the analysis options when interval estimates are required."),
    if (bootstrap_reps > 0L && is.null(bootstrap_table)) tags$p(class = "structural-result-note", "HTMT bootstrap confidence intervals could not be estimated from the selected indicators."),
    if (bootstrap_incomplete) tags$p(class = "structural-result-note", "Some bootstrap resamples could not produce an admissible correlation matrix, commonly because an ordered category was absent or sparse. Interpret intervals with reduced valid-replicate counts cautiously."),
    if (bootstrap_bca_unavailable) tags$p(class = "structural-result-note", "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that pair; increase valid replicates or use percentile CI for reporting."),
    if (bootstrap_caution) tags$p(class = "structural-result-note", "HTMT bootstrap status Caution means that 50% to less than 80% of requested resamples were valid. Treat the interval as unstable and report the valid-replicate count."),
    if (bootstrap_unreliable) tags$p(class = "structural-result-note", "HTMT bootstrap status Unreliable means that fewer than 50% of requested resamples were valid. Confidence limits and threshold decisions are not assessed and should not be used as discriminant-validity evidence."),
    if (!is.null(bootstrap_table)) tags$p(class = "structural-result-note", paste0(
      "The ", htmt_ci_label, " interval is based on case resampling", if (identical(htmt_ci_method, "bca")) " with leave-one-out jackknife acceleration" else "", if (length(bundle$ordered %||% character(0))) " with polychoric correlations re-estimated in each resample" else "",
      ". 'Upper < threshold' uses the one-sided 95% upper confidence limit for the selected .85/.90 criterion. 'Upper < 1' indicates whether the two-sided 95% interval excludes 1. These are intentionally reported separately from the point-estimate criterion."
    ))
  )
})
output[[paste0(prefix, "_result_residuals")]] <- renderUI({
  bundle <- fit_result()
  diagnostics <- structural_canvas_residual_diagnostics(bundle$fit)
  if (!isTRUE(diagnostics$available)) return(NULL)
  matrix_table <- function(matrix_value, title) {
    values <- matrix("", nrow(matrix_value), ncol(matrix_value) + 1L)
    colnames(values) <- c("Indicator", colnames(matrix_value))
    for (row_index in seq_len(nrow(matrix_value))) {
      values[row_index, 1L] <- rownames(matrix_value)[[row_index]]
      for (column_index in seq_len(ncol(matrix_value))) {
        value <- matrix_value[row_index, column_index]
        if (is.finite(value)) values[row_index, column_index + 1L] <- format_decimal3(value)
      }
    }
    tagList(tags$h5(title), tags$table(class = "table table-striped table-bordered structural-residual-matrix",
      tags$thead(tags$tr(lapply(colnames(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    ))
  }
  largest <- diagnostics$largest
  if (nrow(largest)) {
    largest[["Standardized residual"]] <- vapply(largest[["Standardized residual"]], format_decimal3, character(1))
    largest[["Correlation residual"]] <- vapply(largest[["Correlation residual"]], format_decimal3, character(1))
  }
  div(class = "result-section regression-result-panel structural-residual-result",
    h4("5. Local fit diagnostics"),
    matrix_table(diagnostics$standardized, "Standardized residual matrix"),
    matrix_table(diagnostics$correlation, "Correlation residual matrix"),
    tags$h5(paste0("Large standardized residuals (|z| >= ", diagnostics$cutoff, ")")),
    if (!nrow(largest)) tags$p("No residuals exceeded the cutoff.") else tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(largest), tags$th))),
      tags$tbody(lapply(seq_len(nrow(largest)), function(index) tags$tr(lapply(as.character(largest[index, ]), tags$td))))
    ),
    tags$p(class = "structural-result-note", "Large standardized residuals identify local areas of model misfit and should be interpreted with theory rather than used as automatic modification instructions.")
  )
})
output[[paste0(prefix, "_result_higher_order")]] <- renderUI({
  bundle <- fit_result()
  higher <- structural_canvas_higher_order_results(bundle$snapshot %||% list(), bundle$fit)
  if (!isTRUE(higher$available)) return(NULL)
  table <- higher$table
  fixed <- !is.na(table$SE) & table$SE == 0 & is.na(table$z) & is.na(table$p)
  residual_abnormal <- !is.finite(table$ResidualVariance) | table$ResidualVariance < 0 | table$ResidualVariance > 1
  residual_display <- paste0(vapply(table$ResidualVariance, format_decimal3, character(1)), ifelse(residual_abnormal, "†", ""))
  r2_interval_abnormal <- !is.finite(table$R2CILower) | !is.finite(table$R2CIUpper) | table$R2CILower < 0 | table$R2CIUpper > 1
  loading_guidance <- vapply(table$Beta, structural_canvas_higher_order_loading_guidance, character(1))
  display <- data.frame(
    `Higher-order factor` = table$HigherOrderFactor,
    `Lower-order factor` = table$LowerOrderFactor,
    B = vapply(table$B, format_decimal3, character(1)),
    `B 95% CI lower` = vapply(table$BCILower, format_decimal3, character(1)),
    `B 95% CI upper` = vapply(table$BCIUpper, format_decimal3, character(1)),
    SE = vapply(table$SE, format_decimal3, character(1)),
    Beta = vapply(table$Beta, format_decimal3, character(1)),
    `β 95% CI lower` = vapply(table$BetaCILower, format_decimal3, character(1)),
    `β 95% CI upper` = vapply(table$BetaCIUpper, format_decimal3, character(1)),
    `R²` = vapply(table$R2, format_decimal3, character(1)),
    `R² 95% CI lower` = paste0(vapply(table$R2CILower, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
    `R² 95% CI upper` = paste0(vapply(table$R2CIUpper, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
    `Residual variance` = residual_display,
    Guidance = ifelse(residual_abnormal | r2_interval_abnormal, "Review residual/R² interval", loading_guidance),
    z = vapply(table$z, format_decimal3, character(1)),
    p = vapply(table$p, format_p, character(1)),
    check.names = FALSE
  )
  display$SE[fixed] <- "Fixed*"
  display$z[fixed] <- "—"
  display$p[fixed] <- "—"
  omega_h <- structural_canvas_omega_h(bundle$snapshot %||% list(), bundle$fit)
  div(class = "result-section regression-result-panel structural-higher-order-result",
    h4("Higher-order CFA results"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    if (isTRUE(omega_h$available)) tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th("Higher-order factor"), tags$th("Indicators"), tags$th("Hierarchical omega (ωh)"), tags$th("Guidance"))),
      tags$tbody(tags$tr(
        tags$td(omega_h$higher_order_factor), tags$td(omega_h$indicators),
        tags$td(paste0(format_decimal3(omega_h$omega_h), if (!is.finite(omega_h$omega_h) || omega_h$omega_h < 0 || omega_h$omega_h > 1) "†" else "")),
        tags$td(structural_canvas_omega_h_guidance(omega_h$omega_h))
      ))
    )) else tags$p(class = "structural-result-note", paste0("Hierarchical omega was not reported: ", omega_h$reason)),
    tags$p(class = "structural-result-note", "Lower-order R² is the variance explained by the higher-order factor. Residual variance is reported on the standardized latent-variable scale."),
    tags$p(class = "structural-result-note", "Lower-order R² intervals complement the standardized residual-variance intervals. † also marks an R² interval extending beyond [0, 1]."),
    tags$p(class = "structural-result-note", "Higher-order standardized-loading confidence intervals are 95% delta-method intervals from lavaan."),
    tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized higher-order loadings; a fixed reference loading has a degenerate interval at its fixed value."),
    tags$p(class = "structural-result-note", "ωh estimates the proportion of unit-weighted total-score variance attributable to one higher-order general factor under the fitted higher-order CFA model."),
    tags$p(class = "structural-result-note", "The .40 loading and .70 ωh values are descriptive review guidelines, not universal pass/fail rules. † marks an unavailable value or a coefficient/residual variance outside [0, 1]."),
    tags$p(class = "structural-result-note", "* Fixed reference loading; SE, z, and p are not estimated.")
  )
})
for (kind in c("overview", "validity", "measurement")) local({
  result_kind <- kind
  output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
})
output[[paste0(prefix, "_result_mi")]] <- renderUI({
  table <- result_table("mi")
  if (!nrow(table)) return(NULL)
  theory_mi <- identical(fit_result()$mi_mode %||% "theory", "theory")
  tagList(tags$table(
    class = "table table-striped table-bordered structural-mi-table",
    tags$thead(tags$tr(
      lapply(names(table), tags$th),
      if (theory_mi) tags$th("Select")
    )),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) {
      tags$tr(
        lapply(as.character(table[index, , drop = TRUE]), tags$td),
        if (theory_mi) tags$td(actionButton(
          paste0(prefix, "_mi_select_", index),
          "Select",
          class = "btn-sm structural-mi-select-button"
        ))
      )
    }))
  ),
  tags$p(class = "structural-result-note", "MI p treats each modification index as an unscaled asymptotic 1-df chi-square test. BH-adjusted p controls the false-discovery rate across all finite lavaan candidate modifications before the displayed MI and theory filters; MI tests reports that multiplicity-family size."),
  tags$p(class = "structural-result-note", "For MLR or WLSMV, these derived p values are not a separate robust/scaled score-test correction and should be treated as exploratory reference values."),
  tags$p(class = "structural-result-note", "EPC is the expected unstandardized parameter change if the fixed parameter is freed; Std. EPC is lavaan's fully standardized expected change (sepc.all). Consider direction and magnitude rather than MI rank alone."),
  if (theory_mi) tags$p(class = "structural-result-note", "Each Step is sequential: the displayed path is added, the model is refitted, and MI, multiplicity family, EPC, and cumulative fit for the next row are recomputed from that updated model. Rows are not simultaneous candidates from one unchanged model."),
  if (theory_mi) tags$p(class = "structural-result-note", "Skipped unsafe counts higher-ranked candidates rejected for nonconvergence, post.check failure, negative variance, a non-positive-definite or boundary residual/latent/parameter covariance matrix, invalid df, or |latent correlation| >= 1. Skipped details records each rejected path and diagnostic reason; a skipped candidate is not offered for automatic application."),
  tags$p(class = "structural-result-note", "Neither an unadjusted nor adjusted p value justifies a modification. Use effect size (EPC/standardized EPC), residual diagnostics, admissibility, theory, and preferably independent validation."))
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
  div(class = "result-section regression-result-panel structural-mi-history-result",
    h4(if (ko) "MI 수정 이력" else "MI modification history"),
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
