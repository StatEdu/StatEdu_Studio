structural_canvas_construct_specification <- function(snapshot) {
  latents <- Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list())
  rows <- lapply(latents, function(latent) {
    measurement_mode <- as.character(latent$measurementMode %||% "reflective")
    construct_type <- as.character(latent$constructType %||% if (identical(measurement_mode, "formative")) "composite" else "commonFactor")
    if (identical(construct_type, "higherOrder")) construct_type <- "commonFactor"
    data.frame(
      id = as.character(latent$id %||% ""),
      name = structural_canvas_name(latent),
      construct_type = construct_type,
      measurement_mode = measurement_mode,
      weighting_mode = as.character(latent$weightingMode %||% "auto"),
      specification_migration = as.character(latent$constructSpecificationMigration %||% ""),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) return(data.frame(id = character(0), name = character(0), construct_type = character(0), measurement_mode = character(0), weighting_mode = character(0), specification_migration = character(0)))
  do.call(rbind, rows)
}

structural_canvas_formative_content_validity_rows <- function(snapshot, redundancy_result = NULL, redundancy_construct = NULL) {
  specification <- structural_canvas_construct_specification(snapshot)
  formative <- specification[specification$construct_type == "composite" & specification$measurement_mode == "formative", , drop = FALSE]
  if (!nrow(formative)) return(data.frame(
    Construct = character(0), `Domain definition` = character(0), `Indicator inclusion rationale` = character(0),
    `Content-validity procedure/source` = character(0), `Redundancy evidence` = character(0), Status = character(0), Guidance = character(0), check.names = FALSE
  ))
  nodes <- stats::setNames(Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list()), vapply(Filter(function(node) identical(node$role, "latent"), snapshot$nodes %||% list()), structural_canvas_name, character(1)))
  rows <- lapply(formative$name, function(name) {
    node <- nodes[[name]] %||% list()
    domain <- trimws(as.character(node$compositeDomainDefinition %||% ""))
    rationale <- trimws(as.character(node$compositeIndicatorRationale %||% ""))
    evidence <- trimws(as.character(node$compositeContentValidityEvidence %||% ""))
    redundancy <- if (identical(as.character(redundancy_construct %||% ""), name) && isTRUE(redundancy_result$available)) "Available" else "Not documented"
    complete <- all(nzchar(c(domain, rationale, evidence))) && identical(redundancy, "Available")
    data.frame(
      Construct = name, `Domain definition` = domain, `Indicator inclusion rationale` = rationale,
      `Content-validity procedure/source` = evidence, `Redundancy evidence` = redundancy,
      Status = if (complete) "Documented" else "Review",
      Guidance = if (complete) "Report these design-based grounds with weight, collinearity, and redundancy results." else "Document construct-domain coverage, indicator inclusion grounds, content-validation procedure/source, and available redundancy evidence before confirmatory reporting.",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_resolve_construct_specification <- function(snapshot, analysis_type, estimator = NULL) {
  specification <- structural_canvas_construct_specification(snapshot)
  if (!nrow(specification)) return(transform(
    specification, effective_weighting = character(0), engine_representation = character(0),
    estimand = character(0), supported = logical(0), reason = character(0)
  ))
  estimator <- toupper(as.character(estimator %||% if (identical(analysis_type, "plssem")) "PLS" else "ML"))
  covariance_engine <- analysis_type %in% c("cfa", "cbsem", "sem")
  rows <- lapply(seq_len(nrow(specification)), function(index) {
    row <- specification[index, , drop = FALSE]
    construct_type <- row$construct_type[[1L]]
    measurement_mode <- row$measurement_mode[[1L]]
    requested_weighting <- row$weighting_mode[[1L]]
    supported <- TRUE
    reason <- ""
    effective_weighting <- "Not applicable"
    engine_representation <- ""
    estimand <- ""
    if (identical(construct_type, "unspecified")) {
      supported <- FALSE
      reason <- "Construct ontology is unspecified. Declare a common factor or composite before estimation."
    } else if (identical(construct_type, "commonFactor") && identical(measurement_mode, "formative")) {
      supported <- FALSE
      reason <- "A formative indicator relation is incompatible with the common-factor ontology."
    } else if (covariance_engine) {
      engine_representation <- "lavaan reflective latent factor"
      estimand <- "Common factor with explicit measurement error"
      if (!identical(construct_type, "commonFactor") || !identical(measurement_mode, "reflective")) {
        supported <- FALSE
        reason <- "The current CFA/CB-SEM engine supports reflective common factors only."
      } else if (!requested_weighting %in% c("", "auto")) {
        supported <- FALSE
        reason <- "PLS weighting choices do not apply to covariance-based common-factor estimation."
      }
    } else if (identical(analysis_type, "plssem")) {
      effective_weighting <- if (identical(requested_weighting, "auto")) {
        if (identical(measurement_mode, "formative")) "Mode B" else "Mode A"
      } else switch(requested_weighting, modeA = "Mode A", modeB = "Mode B", sum = "Equal weights", predefined = "Predefined weights", requested_weighting)
      if (requested_weighting %in% c("sum", "predefined")) {
        supported <- FALSE
        reason <- "Equal and predefined weights are exposed in the legacy UI but are not implemented by the current SEM engine."
      } else if (identical(measurement_mode, "reflective") && identical(effective_weighting, "Mode B")) {
        supported <- FALSE
        reason <- "Reflective blocks require Mode A in the current PLS implementation."
      } else if (identical(measurement_mode, "formative") && identical(effective_weighting, "Mode A")) {
        supported <- FALSE
        reason <- "Formative composite blocks require Mode B in the current PLS implementation."
      }
      if (identical(measurement_mode, "reflective")) {
        engine_representation <- if (identical(estimator, "PLSC")) "seminr reflective block with PLSc correction" else "seminr reflective Mode A score proxy"
        estimand <- if (identical(construct_type, "commonFactor")) {
          if (identical(estimator, "PLSC")) "Consistency-corrected reflective common factor" else "Common-factor construct represented by a Mode A composite score proxy"
        } else "Reflective Mode A composite"
      } else {
        engine_representation <- "seminr Mode B composite"
        estimand <- "Formative composite"
      }
      if (identical(estimator, "PLSC") && !(identical(construct_type, "commonFactor") && identical(measurement_mode, "reflective") && identical(effective_weighting, "Mode A"))) {
        supported <- FALSE
        reason <- "PLSc is restricted to reflective common factors initialized with Mode A weights."
      }
    } else {
      supported <- FALSE
      reason <- paste0("No construct-resolution rule is defined for analysis type: ", analysis_type, ".")
    }
    data.frame(row, effective_weighting = effective_weighting, engine_representation = engine_representation, estimand = estimand, supported = supported, reason = reason, check.names = FALSE)
  })
  do.call(rbind, rows)
}

structural_canvas_validate_construct_specification <- function(snapshot, analysis_type) {
  specification <- structural_canvas_construct_specification(snapshot)
  if (!nrow(specification)) return(invisible(specification))
  unspecified <- specification$construct_type == "unspecified"
  if (any(unspecified)) {
    stop(sprintf(
      "Every latent construct must be specified before estimation. Choose common factor or composite for: %s.",
      paste(specification$name[unspecified], collapse = ", ")
    ))
  }
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
  resolved <- structural_canvas_resolve_construct_specification(snapshot, analysis_type, if (identical(analysis_type, "plssem")) "PLS" else NULL)
  unsupported <- !resolved$supported
  if (any(unsupported)) stop(paste(unique(resolved$reason[unsupported]), collapse = " "))
  invisible(specification)
}

structural_canvas_validate_plsc_specification <- function(snapshot, analysis_type, estimator) {
  if (!identical(analysis_type, "plssem") || !identical(toupper(as.character(estimator %||% "PLS")), "PLSC")) return(invisible(NULL))
  specification <- structural_canvas_construct_specification(snapshot)
  if (!nrow(specification)) stop("PLSc requires at least one explicitly specified reflective common factor.")
  resolved <- structural_canvas_resolve_construct_specification(snapshot, analysis_type, "PLSC")
  eligible <- resolved$supported
  if (!all(eligible)) {
    invalid <- paste0(
      specification$name[!eligible], " [", specification$construct_type[!eligible], "/",
      specification$measurement_mode[!eligible], "]"
    )
    stop(paste0(
      "PLSc consistency correction is available only when every construct is explicitly specified as a reflective common factor. ",
      "Unsupported constructs: ", paste(invalid, collapse = ", "), ". ", paste(unique(resolved$reason[!eligible]), collapse = " "), " Use standard PLS for composite models or revise the theoretical construct specification."
    ))
  }
  invisible(specification)
}

structural_canvas_structural_effect_plan <- function(snapshot, analysis_type, estimator = "ML") {
  moderations <- snapshot$moderations %||% list()
  moderation_method <- as.character(snapshot$moderationMethod %||% "all_pairs_dmc")
  moderation_method_label <- switch(
    moderation_method,
    matched_pair_dmc = "Unconstrained matched-pair product indicators with double-mean-centering",
    all_pairs_mean_centered = "Unconstrained all-pairs product indicators with indicator mean-centering (legacy)",
    "Unconstrained all-pairs product indicators with double-mean-centering"
  )
  engine <- if (identical(analysis_type, "plssem")) toupper(as.character(estimator %||% "PLS")) else "CB-SEM"
  if (!engine %in% c("PLS", "PLSC", "CB-SEM")) engine <- "CB-SEM"
  rows <- list(
    data.frame(Effect = "Direct effects", Status = "Supported", Method = if (engine == "CB-SEM") "lavaan structural regression" else "PLS path coefficients", Limitation = "Interpret only theory-specified directed paths.", stringsAsFactors = FALSE),
    data.frame(Effect = "Mediation", Status = "Supported", Method = if (engine == "CB-SEM") "Defined indirect and total effects" else "Bootstrap indirect and total effects", Limitation = if (engine == "CB-SEM") "Use bootstrap confidence intervals when requested; significance of component paths alone is not a mediation test." else "Requires PLS bootstrap inference; no covariance-model global fit claim.", stringsAsFactors = FALSE),
    data.frame(Effect = "Moderation", Status = if (length(moderations) && engine != "CB-SEM") "Blocked" else if (length(moderations)) "Supported" else "Not requested", Method = if (engine == "CB-SEM") moderation_method_label else "Not implemented in the current PLS/PLSc engine", Limitation = if (engine == "CB-SEM") "Johnson-Neyman regions require a continuous observed moderator or a latent moderator represented on its factor-score scale." else "Canvas moderation edges must not be silently omitted.", stringsAsFactors = FALSE),
    data.frame(Effect = "Moderated mediation", Status = if (length(moderations) && engine != "CB-SEM") "Blocked" else if (length(moderations)) "Available when an indirect chain contains the moderated path" else "Not requested", Method = if (engine == "CB-SEM") "Conditional indirect effect and index of moderated mediation" else "Not implemented in the current PLS/PLSc engine", Limitation = if (engine == "CB-SEM") "Inference uses the fitted product-indicator parameterization and the observed moderator range." else "Use a validated two-stage or product-indicator PLS implementation outside the current engine.", stringsAsFactors = FALSE)
  )
  do.call(rbind, rows)
}

structural_canvas_causal_interpretation <- function(snapshot, analysis_type) {
  structural_analysis <- analysis_type %in% c("cbsem", "sem", "plssem")
  if (!structural_analysis) return(list(applicable = FALSE, status = "Not applicable", rows = data.frame()))
  nodes <- snapshot$nodes %||% list()
  node_ids <- vapply(nodes, function(node) as.character(node$id %||% ""), character(1))
  latent_ids <- node_ids[vapply(nodes, function(node) identical(node$role, "latent"), logical(1))]
  directed <- Filter(function(edge) {
    !identical(edge$kind %||% "", "covariance") &&
      as.character(edge$from %||% "") %in% latent_ids &&
      as.character(edge$to %||% "") %in% latent_ids
  }, snapshot$edges %||% list())
  from <- vapply(directed, function(edge) as.character(edge$from %||% ""), character(1))
  to <- vapply(directed, function(edge) as.character(edge$to %||% ""), character(1))
  has_indirect_chain <- length(from) > 1L && any(to %in% from)
  rows <- data.frame(
    Assumption = c(
      "Temporal ordering / study design",
      "No unmeasured exposure-outcome confounding",
      "Causal treatment/exposure identification",
      "Sequential ignorability for mediation"
    ),
    Recorded = c(
      "Not collected by this workflow",
      "Not established by SEM fit",
      "Not established by directed paths",
      if (has_indirect_chain) "Not established; indirect chain detected" else "Not assessed; no indirect chain detected"
    ),
    Consequence = c(
      "Do not infer temporal or causal direction from path arrows alone.",
      "Fit indices, bootstrap intervals, and significant coefficients do not remove confounding bias.",
      "Report coefficients as theory-directed associations unless identification is justified externally.",
      if (has_indirect_chain) "Describe the indirect effect as an associational decomposition unless temporal order and mediator/outcome confounding assumptions are justified." else "No mediation-specific claim is indicated by the current path graph."
    ),
    stringsAsFactors = FALSE
  )
  list(
    applicable = TRUE,
    status = "Causal identification not established",
    interpretation = "Associational structural parameters",
    indirect_chain_detected = has_indirect_chain,
    rows = rows
  )
}

structural_canvas_sampling_design_gate <- function(sampling_design) {
  sampling_design <- as.character(sampling_design %||% "not_declared")
  labels <- c(
    not_declared = "Not declared",
    independent_cross_sectional = "Independent cross-sectional observations",
    clustered = "Clustered or multilevel data",
    complex_survey = "Complex survey design",
    longitudinal_repeated = "Longitudinal or repeated measures"
  )
  if (!sampling_design %in% names(labels)) sampling_design <- "not_declared"
  supported <- identical(sampling_design, "independent_cross_sectional")
  reason <- switch(
    sampling_design,
    independent_cross_sectional = "The declared structure is supported by the current single-level independent-observation engine.",
    clustered = "Cluster-robust or multilevel SEM is required; the current canvas engine does not model cluster dependence.",
    complex_survey = "Survey weights, strata, and primary sampling units require a survey-aware SEM workflow that is not implemented here.",
    longitudinal_repeated = "Within-person dependence and longitudinal measurement structure require a repeated-measures or longitudinal SEM workflow that is not implemented here.",
    "Observation independence and sampling structure must be declared before estimation."
  )
  result <- list(design = sampling_design, label = unname(labels[[sampling_design]]), supported = supported, reason = reason)
  if (!supported) stop(paste0("Sampling-design gate blocked estimation. ", reason))
  result
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
  structural_canvas_validate_plsc_specification(snapshot, analysis_type, estimator)
  resolved_specification <- structural_canvas_resolve_construct_specification(snapshot, analysis_type, estimator)
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
      resolved_construct_specification = resolved_specification,
      single_indicator_auto_residuals = lavaan_syntax$single_indicator_auto_residuals %||% character(0),
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
  result$resolved_construct_specification <- resolved_specification
  result
}
