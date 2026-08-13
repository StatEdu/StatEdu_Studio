# Structural equation canvas reliability bootstrap render outputs.

structural_canvas_register_reliability_bootstrap_outputs <- function(output, prefix, fit_result) {
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
  invisible(TRUE)
}
