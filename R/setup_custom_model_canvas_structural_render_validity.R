structural_canvas_register_validity_outputs <- function(output, prefix, analysis_type, fit_result, result_table) {
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
  structural_canvas_register_htmt_outputs(output, prefix, fit_result)
  invisible(TRUE)
}
