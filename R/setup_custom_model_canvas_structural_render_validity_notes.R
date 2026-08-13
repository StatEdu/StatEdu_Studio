# Structural equation canvas validity-note render outputs.

structural_canvas_register_validity_note_outputs <- function(output, prefix, fit_result, result_table) {
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
  invisible(TRUE)
}
