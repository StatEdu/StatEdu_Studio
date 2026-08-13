# Structural equation canvas HTMT render outputs.

structural_canvas_register_htmt_outputs <- function(output, prefix, fit_result) {
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
  invisible(TRUE)
}
