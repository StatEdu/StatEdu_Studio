structural_canvas_construct_specification <- function(snapshot) {
  latents <- Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list())
  rows <- lapply(latents, function(latent) {
    measurement_mode <- as.character(latent$measurementMode %||% "reflective")
    construct_type <- as.character(latent$constructType %||% if (identical(measurement_mode, "formative")) "composite" else "commonFactor")
    data.frame(
      id = as.character(latent$id %||% ""),
      name = structural_canvas_name(latent),
      construct_type = construct_type,
      measurement_mode = measurement_mode,
      weighting_mode = as.character(latent$weightingMode %||% "auto"),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) return(data.frame(id = character(0), name = character(0), construct_type = character(0), measurement_mode = character(0), weighting_mode = character(0)))
  do.call(rbind, rows)
}

structural_canvas_validate_construct_specification <- function(snapshot, analysis_type) {
  specification <- structural_canvas_construct_specification(snapshot)
  if (!nrow(specification)) return(invisible(specification))
  invalid_factor <- specification$construct_type == "commonFactor" & specification$measurement_mode == "formative"
  if (any(invalid_factor)) {
    stop(sprintf(
      "Formative indicator relationships are incompatible with common-factor specifications: %s.",
      paste(specification$name[invalid_factor], collapse = ", ")
    ))
  }
  unsupported_composite <- analysis_type %in% c("cfa", "cbsem", "sem") & specification$construct_type == "composite"
  if (any(unsupported_composite)) {
    stop(sprintf(
      "The current CFA/CB-SEM engine does not estimate composite constructs: %s. Use a supported composite engine or revise the theoretical specification.",
      paste(specification$name[unsupported_composite], collapse = ", ")
    ))
  }
  invisible(specification)
}

structural_canvas_structural_effect_plan <- function(snapshot, analysis_type, estimator = "ML") {
  moderations <- snapshot$moderations %||% list()
  engine <- if (identical(analysis_type, "plssem")) toupper(as.character(estimator %||% "PLS")) else "CB-SEM"
  if (!engine %in% c("PLS", "PLSC", "CB-SEM")) engine <- "CB-SEM"
  rows <- list(
    data.frame(Effect = "Direct effects", Status = "Supported", Method = if (engine == "CB-SEM") "lavaan structural regression" else "PLS path coefficients", Limitation = "Interpret only theory-specified directed paths.", stringsAsFactors = FALSE),
    data.frame(Effect = "Mediation", Status = "Supported", Method = if (engine == "CB-SEM") "Defined indirect and total effects" else "Bootstrap indirect and total effects", Limitation = if (engine == "CB-SEM") "Use bootstrap confidence intervals when requested; significance of component paths alone is not a mediation test." else "Requires PLS bootstrap inference; no covariance-model global fit claim.", stringsAsFactors = FALSE),
    data.frame(Effect = "Moderation", Status = if (length(moderations) && engine != "CB-SEM") "Blocked" else if (length(moderations)) "Supported" else "Not requested", Method = if (engine == "CB-SEM") "Mean-centered product indicators" else "Not implemented in the current PLS/PLSc engine", Limitation = if (engine == "CB-SEM") "Johnson-Neyman regions require a continuous observed moderator or a latent moderator represented on its factor-score scale." else "Canvas moderation edges must not be silently omitted.", stringsAsFactors = FALSE),
    data.frame(Effect = "Moderated mediation", Status = if (length(moderations) && engine != "CB-SEM") "Blocked" else if (length(moderations)) "Available when an indirect chain contains the moderated path" else "Not requested", Method = if (engine == "CB-SEM") "Conditional indirect effect and index of moderated mediation" else "Not implemented in the current PLS/PLSc engine", Limitation = if (engine == "CB-SEM") "Inference uses the fitted product-indicator parameterization and the observed moderator range." else "Use a validated two-stage or product-indicator PLS implementation outside the current engine.", stringsAsFactors = FALSE)
  )
  do.call(rbind, rows)
}

structural_canvas_validate_structural_effects <- function(snapshot, analysis_type, estimator = "ML") {
  plan <- structural_canvas_structural_effect_plan(snapshot, analysis_type, estimator)
  blocked <- plan$Status == "Blocked"
  if (any(blocked)) {
    stop(paste0(
      "The current PLS/PLSc engine does not estimate canvas moderation or moderated-mediation edges. ",
      "These edges were not fitted. Use CB-SEM product-indicator moderation or a separately validated PLS interaction workflow."
    ))
  }
  invisible(plan)
}

run_structural_canvas_analysis <- function(snapshot, data, analysis_type, estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0), nominal = character(0), residual_variance_fixes = numeric(0)) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  structural_canvas_validate_construct_specification(snapshot, analysis_type)
  structural_effect_plan <- structural_canvas_validate_structural_effects(snapshot, analysis_type, estimator)
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
    model_df <- tryCatch(
      suppressWarnings(as.numeric(lavaan::fitMeasures(fit, "df")[[1L]])),
      error = function(error) NA_real_
    )
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
      structural_effect_plan = structural_effect_plan,
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

  result <- structural_canvas_run_pls_analysis(snapshot, data, latents, edges, estimator = estimator)
  result$structural_effect_plan <- structural_effect_plan
  result
}
