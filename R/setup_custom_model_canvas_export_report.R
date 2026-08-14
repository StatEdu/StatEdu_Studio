# CFA/SEM canvas report and reproducibility export helpers.

structural_canvas_analysis_context <- function(bundle) {
  if (isTRUE(bundle$modified_from_baseline)) {
    return("Exploratory modified model; selected using the analyzed data and requiring independent validation.")
  }
  "Prespecified/original model."
}

structural_canvas_reproducibility_record <- function(bundle, generated_at = Sys.time()) {
  fit <- bundle$fit
  options <- lavaan::lavInspect(fit, "options")
  lines <- c(
    "CFA analysis reproducibility record",
    paste0("Generated: ", format(generated_at, "%Y-%m-%d %H:%M:%S %Z")),
    paste0("R version: ", paste(R.version$major, R.version$minor, sep = ".")),
    paste0("lavaan version: ", as.character(utils::packageVersion("lavaan"))),
    paste0("Analysis context: ", structural_canvas_analysis_context(bundle)),
    paste0("Estimator: ", bundle$estimator %||% options$estimator %||% ""),
    paste0("Missing-data option: ", bundle$missing %||% options$missing %||% ""),
    paste0("Latent scaling: ", if (isTRUE(bundle$std_lv)) "latent variance fixed to 1" else "marker loading fixed to 1"),
    paste0("N used: ", lavaan::lavInspect(fit, "ntotal")),
    paste0("Ordered indicators: ", if (length(bundle$ordered %||% character(0))) paste(bundle$ordered, collapse = ", ") else "none"),
    paste0("AVE/CR formula: ", bundle$validity_formula %||% "standardized"),
    paste0("RMSEA CI level: ", bundle$rmsea_ci %||% .90),
    paste0("HTMT threshold: ", bundle$htmt_threshold %||% .85),
    paste0("HTMT bootstrap: ", bundle$htmt_bootstrap %||% 0L, "; seed: ", bundle$htmt_seed %||% "not used", "; CI method: ", bundle$htmt_ci_method %||% "percentile"),
    paste0("AVE/reliability bootstrap: ", bundle$reliability_bootstrap %||% 0L, "; seed: ", bundle$reliability_seed %||% "not used", "; CI method: ", bundle$reliability_ci_method %||% "percentile"),
    paste0("Bollen-Stine bootstrap: ", bundle$bollen_stine_bootstrap %||% 0L, "; seed: ", bundle$bollen_stine_seed %||% "not used"),
    paste0("Measurement invariance: ", if (isTRUE(bundle$invariance_enabled)) paste0("enabled; group = ", bundle$invariance_group) else "disabled"),
    paste0("Common method bias diagnostics: ", if (isTRUE(bundle$common_method_enabled)) paste0("enabled; methods = ", paste(bundle$common_method_methods %||% character(0), collapse = ", ")) else "disabled"),
    paste0("MI holdout validation: ", if (isTRUE(bundle$mi_holdout_enabled)) paste0("enabled; validation fraction = ", bundle$mi_holdout_fraction, "; seed = ", bundle$mi_holdout_seed, "; exploration N = ", nrow(bundle$analysis_data), "; validation N = ", nrow(bundle$validation_data)) else "disabled"),
    if (!is.null(bundle$holdout_comparison)) paste0("MI holdout N used after missing-data handling: ", paste(bundle$holdout_comparison$validation_n_used, collapse = ", ")),
    paste0("MI output mode: ", bundle$mi_mode %||% "theory"),
    paste0("Admissible solution: ", isTRUE(bundle$diagnostics$admissible)),
    paste0("Admissibility reasons: ", if (length(bundle$diagnostics$admissibility_reasons %||% character(0))) paste(bundle$diagnostics$admissibility_reasons, collapse = "; ") else "none"),
    "",
    "lavaan model syntax",
    "-------------------",
    as.character(bundle$syntax %||% "Syntax unavailable")
  )
  paste(lines, collapse = "\n")
}

structural_canvas_export_notes <- function(bundle) {
  ordered <- length(bundle$ordered %||% character(0)) > 0L
  admissible <- isTRUE(bundle$diagnostics$admissible %||% FALSE)
  missing_covariances <- structural_canvas_missing_exogenous_covariances(bundle$snapshot %||% list())
  notes <- data.frame(
    Section = c("Analysis context", "Fit", "Fit", "RMSEA tests", "Information criteria", "Validity", "Reliability", "Measurement", "Modification indices", "Admissibility"),
    Note = c(
      structural_canvas_analysis_context(bundle),
      "Robust/scaled fit statistics are reported when available for the fitted estimator.",
      "Descriptive guidance uses CFI/TLI >= .95 (good) and >= .90 (marginal), RMSEA <= .06 (good) and <= .08 (marginal), and SRMR <= .08 (good) and <= .10 (marginal). These are not universal acceptance rules.",
      "Close-fit tests H0: RMSEA <= .05; not-close tests H0: RMSEA >= .08. Estimator-matched robust/scaled p values are used when available, and neither test is a standalone acceptance rule.",
      "AIC, BIC, and adjusted BIC are relative criteria for models fitted to the same observations and variables with the same likelihood and estimator family; lower values are preferred, but they do not establish absolute fit.",
      "Fornell-Larcker diagonal entries are sqrt(AVE); lower-triangle entries are latent correlations. AVE is reported separately.",
      "AVE >= .50 and CR, Cronbach's alpha, and omega >= .70 are descriptive guidelines that require substantive and model-based interpretation.",
      "Fixed reference loadings have no estimated unstandardized SE, z, or p value. R-squared and residual diagnostics should be considered alongside loadings.",
      "MI p values use the unscaled asymptotic 1-df chi-square reference from each modification index and BH adjustment across all finite lavaan candidates before display filters. In sequential output, each step refits the preceding model, skips candidates that fail convergence or post-estimation admissibility, and recomputes its own MI family and EPC values.",
      if (admissible) "The fitted solution passed the implemented admissibility checks." else "The fitted solution failed or did not complete one or more admissibility checks; inferential and validity results require caution."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (ordered) notes <- rbind(notes, data.frame(
    Section = "Ordered indicators",
    Note = "Ordered-indicator results use the fitted latent-response model; alpha uses the polychoric correlation matrix and AVE/CR/omega use standardized latent-response parameters.",
    stringsAsFactors = FALSE
  ))
  if (!is.null(bundle$bollen_stine_result) && nrow(bundle$bollen_stine_result)) notes <- rbind(notes, data.frame(
    Section = "Bollen-Stine",
    Note = paste0(
      "Model-based AVE/reliability and Bollen-Stine bootstraps require an admissible original CFA and use only replicates that pass the same full admissibility checks; Bollen-Stine additionally uses a plus-one correction and reports finite-simulation error.",
      if (isTRUE(bundle$modified_from_baseline)) " Because the model was modified using the analyzed data, this result is exploratory rather than confirmatory." else ""
    ),
    stringsAsFactors = FALSE
  ))
  if (!is.null(bundle$common_method_result)) notes <- rbind(notes, data.frame(
    Section = "Common method bias",
    Note = "Common method diagnostics are screening evidence only. Report them as indicating whether serious common-method concentration was detected, not as proof that common method bias is absent.",
    stringsAsFactors = FALSE
  ))
  if (length(missing_covariances)) notes <- rbind(notes, data.frame(
    Section = "Latent covariances",
    Note = paste0("Omitted exogenous latent covariance paths were fixed to zero: ", paste(missing_covariances, collapse = ", "), "."),
    stringsAsFactors = FALSE
  ))
  rownames(notes) <- NULL
  notes
}

structural_canvas_report_summary <- function(bundle) {
  fit <- bundle$fit
  estimator <- bundle$estimator %||% lavaan::lavInspect(fit, "options")$estimator %||% ""
  fit_values <- structural_canvas_fit_measures(fit, estimator, bundle$rmsea_ci %||% .90)$values
  data.frame(
    Section = c(rep("Analysis", 6L), rep("Model fit", 8L), "Interpretation"),
    Item = c(
      "Analysis context", "Estimator", "N used", "Observed variables", "Latent variables", "Free parameters",
      "Chi-square", "df", "p", "CFI", "TLI", "SRMR", "RMSEA", paste0(round(100 * as.numeric(bundle$rmsea_ci %||% .90)), "% RMSEA CI"),
      "Reporting caution"
    ),
    Value = c(
      structural_canvas_analysis_context(bundle),
      as.character(estimator),
      as.character(lavaan::lavInspect(fit, "ntotal")),
      as.character(length(lavaan::lavNames(fit, "ov"))),
      as.character(length(lavaan::lavNames(fit, "lv"))),
      as.character(lavaan::lavInspect(fit, "npar")),
      format_decimal3(fit_values[[1L]]),
      format_decimal3(fit_values[[2L]]),
      format_p(fit_values[[3L]]),
      format_decimal3(fit_values[[5L]]),
      format_decimal3(fit_values[[6L]]),
      format_decimal3(fit_values[[7L]]),
      format_decimal3(fit_values[[8L]]),
      paste0(format_decimal3(fit_values[[9L]]), ", ", format_decimal3(fit_values[[10L]])),
      if (isTRUE(bundle$modified_from_baseline)) "Label the model as exploratory in manuscripts and reports." else "Report as prespecified only if the model was specified before inspecting these data."
    ),
    check.names = FALSE
  )
}
