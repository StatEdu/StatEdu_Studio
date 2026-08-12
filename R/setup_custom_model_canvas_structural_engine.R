run_structural_canvas_analysis <- function(snapshot, data, analysis_type, estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0), nominal = character(0), residual_variance_fixes = numeric(0)) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  residual_constraint_diagnostics <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_identification_diagnostics(snapshot) else data.frame()
  residual_constraint_errors <- if (nrow(residual_constraint_diagnostics)) residual_constraint_diagnostics[
    residual_constraint_diagnostics$Severity == "Error" & residual_constraint_diagnostics$Code %in% c("single_indicator", "invalid_fixed_residual", "negative_fixed_residual"),
    , drop = FALSE
  ] else data.frame()
  if (nrow(residual_constraint_errors)) {
    stop(paste(residual_constraint_errors$Message, collapse = " "))
  }
  residual_scale <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_fixed_residual_scale_diagnostics(snapshot, data, ordered) else data.frame()
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
  measurement_lines <- vapply(latents, function(latent) {
    indicator_edges <- Filter(function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(from$id, latent$id) && identical(to$role, "indicator")) ||
        (identical(to$id, latent$id) && identical(from$role, "indicator"))
    }, edges)
    indicators <- vapply(indicator_edges, function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      indicator_name <- structural_canvas_name(if (identical(from$role, "indicator")) from else to)
      structural_canvas_parameter_term(edge, indicator_name)
    }, character(1))
    paste(structural_canvas_name(latent), "=~", paste(indicators, collapse = " + "))
  }, character(1))
  measurement_lines <- measurement_lines[grepl("\\S+\\s*=~\\s*\\S+", measurement_lines)]
  higher_order_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance") || !identical(as.character(edge$pathType %||% "regression"), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")
  }, edges)
  higher_order_groups <- split(higher_order_edges, vapply(higher_order_edges, function(edge) as.character(edge$from), character(1)))
  higher_order_lines <- vapply(higher_order_groups, function(group) {
    parent <- structural_canvas_node(snapshot, group[[1L]]$from)
    children <- vapply(group, function(edge) {
      child_name <- structural_canvas_name(structural_canvas_node(snapshot, edge$to))
      structural_canvas_parameter_term(edge, child_name)
    }, character(1))
    paste(structural_canvas_name(parent), "=~", paste(children, collapse = " + "))
  }, character(1))
  structural_lines <- vapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    if (identical(as.character(edge$pathType %||% "regression"), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) {
    predictor_name <- structural_canvas_name(structural_canvas_node(snapshot, edge$from))
    paste(structural_canvas_name(structural_canvas_node(snapshot, edge$to)), "~", structural_canvas_parameter_term(edge, predictor_name))
  }, character(1))
  covariance_target_name <- function(node) {
    if (is.null(node)) return("")
    if (identical(node$role, "latent") || identical(node$role, "indicator")) return(structural_canvas_name(node))
    if (node$role %in% c("error", "disturbance")) {
      target_edge <- Filter(function(edge) !identical(edge$kind, "covariance") && identical(as.character(edge$from), as.character(node$id)), edges)
      if (length(target_edge)) return(structural_canvas_name(structural_canvas_node(snapshot, target_edge[[1]]$to)))
    }
    ""
  }
  covariance_lines <- vapply(Filter(function(edge) identical(edge$kind, "covariance"), edges), function(edge) {
    from_name <- covariance_target_name(structural_canvas_node(snapshot, edge$from))
    to_name <- covariance_target_name(structural_canvas_node(snapshot, edge$to))
    if (!nzchar(from_name) || !nzchar(to_name)) "" else paste(from_name, "~~", structural_canvas_parameter_term(edge, to_name))
  }, character(1))
  covariance_lines <- covariance_lines[nzchar(covariance_lines)]
  residual_parameter_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance") || !structural_canvas_has_parameter_modifier(edge)) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && from$role %in% c("error", "disturbance") && to$role %in% c("indicator", "latent")
  }, edges)
  residual_parameter_lines <- vapply(residual_parameter_edges, function(edge) {
    target_name <- structural_canvas_name(structural_canvas_node(snapshot, edge$to))
    paste(target_name, "~~", structural_canvas_parameter_term(edge, target_name))
  }, character(1))
  residual_fix_input <- residual_variance_fixes %||% numeric(0)
  residual_variance_fixes <- as.numeric(residual_fix_input)
  names(residual_variance_fixes) <- names(residual_fix_input)
  if (length(residual_variance_fixes)) {
    fix_names <- names(residual_variance_fixes)
    if (is.null(fix_names) || any(is.na(fix_names) | !nzchar(fix_names))) stop("Every residual-variance sensitivity value must have an indicator name.")
    if (anyDuplicated(fix_names)) stop("Residual-variance sensitivity values contain duplicate indicator names.")
    unknown_fix_names <- setdiff(fix_names, names(data))
    if (length(unknown_fix_names)) stop(paste0("Residual-variance sensitivity indicators were not found in the data: ", paste(unknown_fix_names, collapse = ", "), "."))
    if (any(!is.finite(residual_variance_fixes) | residual_variance_fixes <= 0)) stop("Residual-variance sensitivity values must be finite and greater than zero.")
    existing_residual_targets <- unique(vapply(residual_parameter_edges, function(edge) structural_canvas_name(structural_canvas_node(snapshot, edge$to)), character(1)))
    conflicting_fix_names <- intersect(fix_names, existing_residual_targets)
    if (length(conflicting_fix_names)) stop(paste0("Residual-variance sensitivity values conflict with existing canvas residual constraints for: ", paste(conflicting_fix_names, collapse = ", "), ". Remove the existing fixed/start/labeled constraint or do not apply the sensitivity fix."))
  }
  residual_fix_lines <- if (length(residual_variance_fixes)) vapply(names(residual_variance_fixes), function(name) {
    paste(structural_canvas_name(list(name = name)), "~~", paste0(format(residual_variance_fixes[[name]], scientific = FALSE, digits = 15, trim = TRUE), "*", structural_canvas_name(list(name = name))))
  }, character(1)) else character(0)

  if (analysis_type %in% c("cfa", "cbsem")) {
    syntax <- paste(c(measurement_lines, higher_order_lines, structural_lines, covariance_lines, residual_parameter_lines, residual_fix_lines), collapse = "\n")
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

  constructs <- lapply(latents, function(latent) {
    indicator_names <- vapply(Filter(function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(from$id, latent$id) && identical(to$role, "indicator")) ||
        (identical(to$id, latent$id) && identical(from$role, "indicator"))
    }, edges), function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "indicator")) from else to)
    }, character(1))
    if (identical(latent$measurementMode %||% "reflective", "formative")) {
      seminr::composite(structural_canvas_name(latent), indicator_names)
    } else {
      seminr::reflective(structural_canvas_name(latent), indicator_names)
    }
  })
  path_specs <- lapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) {
    seminr::paths(
      from = structural_canvas_name(structural_canvas_node(snapshot, edge$from)),
      to = structural_canvas_name(structural_canvas_node(snapshot, edge$to))
    )
  })
  fit <- seminr::estimate_pls(
    data = data,
    measurement_model = do.call(seminr::constructs, constructs),
    structural_model = if (length(path_specs)) do.call(seminr::relationships, path_specs) else NULL
  )
  list(fit = fit, converged = TRUE)
}
