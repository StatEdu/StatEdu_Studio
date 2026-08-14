run_structural_canvas_analysis <- function(snapshot, data, analysis_type, estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0), nominal = character(0), residual_variance_fixes = numeric(0)) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  residual_constraint_diagnostics <- if (analysis_type %in% c("cfa", "cbsem", "sem")) structural_canvas_identification_diagnostics(snapshot) else data.frame()
  residual_constraint_errors <- if (nrow(residual_constraint_diagnostics)) residual_constraint_diagnostics[
    residual_constraint_diagnostics$Severity == "Error" & residual_constraint_diagnostics$Code %in% c("single_indicator", "invalid_fixed_residual", "negative_fixed_residual"),
    , drop = FALSE
  ] else data.frame()
  if (nrow(residual_constraint_errors)) {
    stop(paste(residual_constraint_errors$Message, collapse = " "))
  }
  residual_scale <- if (analysis_type %in% c("cfa", "cbsem", "sem")) structural_canvas_fixed_residual_scale_diagnostics(snapshot, data, ordered) else data.frame()
  invalid_residual_scale <- if (nrow(residual_scale)) residual_scale[
    residual_scale$Status != "Within observed variance" & residual_scale[["Single-indicator factor"]], , drop = FALSE
  ] else data.frame()
  if (nrow(invalid_residual_scale)) {
    details <- vapply(seq_len(nrow(invalid_residual_scale)), function(index) paste0(
      invalid_residual_scale$Indicator[[index]], ": fixed residual = ", format_decimal3(invalid_residual_scale[["Fixed residual variance"]][[index]]),
      ", observed variance = ", format_decimal3(invalid_residual_scale[["Observed variance"]][[index]])
    ), character(1))
    stop(paste0("For a continuous single-indicator factor, the fixed residual variance must be smaller than the observed variance; otherwise nonpositive common variance is imposed by the single-indicator decomposition. ", paste(details, collapse = "; "), "."))
  }
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  lavaan_syntax <- structural_canvas_lavaan_syntax(
    snapshot, data, analysis_type, latents, edges, ordered, residual_variance_fixes
  )
  data <- lavaan_syntax$data %||% data
  residual_variance_fixes <- lavaan_syntax$residual_variance_fixes
  if (analysis_type %in% c("cfa", "cbsem", "sem")) {
    syntax <- lavaan_syntax$syntax
    if (!nzchar(syntax)) stop("The model does not contain estimable paths.")
    estimator <- toupper(as.character(estimator %||% "ML"))
    missing <- as.character(missing %||% "fiml")
    if (identical(estimator, "WLSMV") && identical(missing, "fiml")) missing <- "pairwise"
    ordered <- intersect(unique(as.character(ordered %||% character(0))), names(data))
    nominal <- intersect(unique(as.character(nominal %||% character(0))), names(data))
    if (length(nominal)) {
      stop(sprintf("Nominal indicators are not supported by standard CFA/SEM: %s.", paste(nominal, collapse = ", ")))
    }
    if (length(residual_variance_fixes) && (length(ordered) || estimator %in% c("WLSMV", "DWLS"))) {
      stop("Fixed residual-variance sensitivity analysis is supported only for continuous indicators estimated with ML or MLR.")
    }
    if (length(residual_variance_fixes)) {
      fix_observed_variances <- vapply(names(residual_variance_fixes), function(name) {
        if (!is.numeric(data[[name]])) return(NA_real_)
        stats::var(data[[name]], na.rm = TRUE)
      }, numeric(1))
      if (any(!is.finite(fix_observed_variances) | fix_observed_variances <= 0)) stop("Residual-variance sensitivity analysis requires a positive observed variance for every continuous indicator.")
      if (any(residual_variance_fixes >= fix_observed_variances)) stop("Each residual-variance sensitivity value must be smaller than its indicator's observed variance.")
    }
    if (identical(estimator, "WLSMV") && !length(ordered)) {
      stop("WLSMV requires at least one binary, categorical, or ordinal indicator.")
    }
    fit <- if (identical(analysis_type, "cfa")) {
      lavaan::cfa(syntax, data = data, estimator = estimator, missing = missing, std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE)
    } else {
      lavaan::sem(syntax, data = data, estimator = estimator, missing = missing, std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE)
    }
    converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
    post_check <- isTRUE(lavaan::lavInspect(fit, "post.check"))
    model_df <- suppressWarnings(as.numeric(lavaan::fitMeasures(fit, "df")[[1L]]))
    theta <- as.matrix(lavaan::lavInspect(fit, "theta"))
    negative_residuals <- if (length(theta)) rownames(theta)[diag(theta) < 0] else character(0)
    latent_covariance <- as.matrix(lavaan::lavInspect(fit, "cov.lv"))
    parameter_covariance <- tryCatch(as.matrix(lavaan::lavInspect(fit, "vcov")), error = function(error) matrix(numeric(0), 0L, 0L))
    negative_latent_variances <- if (length(latent_covariance)) rownames(latent_covariance)[diag(latent_covariance) < 0] else character(0)
    theta_min_eigenvalue <- structural_canvas_minimum_eigenvalue(theta)
    latent_min_eigenvalue <- structural_canvas_minimum_eigenvalue(latent_covariance)
    parameter_min_eigenvalue <- structural_canvas_minimum_eigenvalue(parameter_covariance)
    theta_tolerance <- sqrt(.Machine$double.eps) * max(1, if (length(theta)) max(abs(diag(theta)), na.rm = TRUE) else 1)
    latent_tolerance <- sqrt(.Machine$double.eps) * max(1, if (length(latent_covariance)) max(abs(diag(latent_covariance)), na.rm = TRUE) else 1)
    parameter_scale <- if (length(parameter_covariance)) max(abs(diag(parameter_covariance)), na.rm = TRUE) else NA_real_
    parameter_tolerance <- if (is.finite(parameter_scale)) sqrt(.Machine$double.eps) * parameter_scale else NA_real_
    non_psd_theta <- is.finite(theta_min_eigenvalue) && theta_min_eigenvalue < -theta_tolerance
    non_psd_latent_covariance <- is.finite(latent_min_eigenvalue) && latent_min_eigenvalue < -latent_tolerance
    near_singular_theta <- is.finite(theta_min_eigenvalue) && !non_psd_theta && theta_min_eigenvalue <= theta_tolerance
    near_singular_latent_covariance <- is.finite(latent_min_eigenvalue) && !non_psd_latent_covariance && latent_min_eigenvalue <= latent_tolerance
    non_psd_parameter_covariance <- is.finite(parameter_min_eigenvalue) && parameter_min_eigenvalue < -parameter_tolerance
    near_singular_parameter_covariance <- is.finite(parameter_min_eigenvalue) && !non_psd_parameter_covariance && parameter_min_eigenvalue <= parameter_tolerance
    theta_condition_number <- structural_canvas_symmetric_condition_number(theta)
    latent_condition_number <- structural_canvas_symmetric_condition_number(latent_covariance)
    parameter_condition_number <- structural_canvas_symmetric_condition_number(parameter_covariance)
    ill_conditioned_theta <- is.finite(theta_condition_number) && theta_condition_number > 1e8
    ill_conditioned_latent_covariance <- is.finite(latent_condition_number) && latent_condition_number > 1e8
    ill_conditioned_parameter_covariance <- is.finite(parameter_condition_number) && parameter_condition_number > 1e8
    latent_correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
    invalid_correlations <- length(latent_correlations) > 1L && any(abs(latent_correlations[row(latent_correlations) != col(latent_correlations)]) >= 1, na.rm = TRUE)
    shared_admissibility <- structural_canvas_fit_admissibility(fit)
    return(list(
      fit = fit, syntax = syntax, converged = converged, post_check = post_check,
      effect_definitions = lavaan_syntax$effect_definitions %||% list(),
      moderation_definitions = lavaan_syntax$moderation_definitions %||% list(),
      identified = is.finite(model_df) && model_df >= 0,
      df = model_df,
      admissible = isTRUE(shared_admissibility$admissible),
      admissibility_reasons = shared_admissibility$reasons,
      negative_residuals = negative_residuals, negative_latent_variances = negative_latent_variances,
      theta_min_eigenvalue = theta_min_eigenvalue, latent_min_eigenvalue = latent_min_eigenvalue,
      non_psd_theta = non_psd_theta, non_psd_latent_covariance = non_psd_latent_covariance,
      near_singular_theta = near_singular_theta, near_singular_latent_covariance = near_singular_latent_covariance,
      parameter_min_eigenvalue = parameter_min_eigenvalue,
      non_psd_parameter_covariance = non_psd_parameter_covariance, near_singular_parameter_covariance = near_singular_parameter_covariance,
      theta_condition_number = theta_condition_number, latent_condition_number = latent_condition_number,
      ill_conditioned_theta = ill_conditioned_theta, ill_conditioned_latent_covariance = ill_conditioned_latent_covariance,
      parameter_condition_number = parameter_condition_number, ill_conditioned_parameter_covariance = ill_conditioned_parameter_covariance,
      invalid_correlations = invalid_correlations
    ))
  }

  structural_canvas_run_pls_analysis(snapshot, data, latents, edges)
}
