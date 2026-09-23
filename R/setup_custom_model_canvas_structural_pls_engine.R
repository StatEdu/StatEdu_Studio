# Structural equation canvas PLS engine helpers.

structural_canvas_pls_path_specs_from_strings <- function(structural_paths) {
  structural_paths <- as.character(structural_paths %||% character(0))
  rows <- lapply(structural_paths, function(spec) {
    parts <- strsplit(spec, "~", fixed = TRUE)[[1L]]
    if (length(parts) != 2L) return(NULL)
    data.frame(
      outcome = trimws(parts[[1L]]),
      predictor = trimws(parts[[2L]]),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(outcome = character(0), predictor = character(0)))
  unique(do.call(rbind, rows))
}

structural_canvas_pls_redundancy_analysis <- function(result, snapshot, data, construct = "", criterion = "") {
  construct <- as.character(construct %||% "")
  criterion <- as.character(criterion %||% "")
  if (!nzchar(construct) || !nzchar(criterion)) return(list(available = FALSE, reason = "A formative construct and a separate global criterion were not both selected."))
  specification <- structural_canvas_construct_specification(snapshot)
  selected <- specification[specification$name == construct, , drop = FALSE]
  if (!nrow(selected) || !identical(selected$construct_type[[1L]], "composite") || !identical(selected$measurement_mode[[1L]], "formative")) {
    return(list(available = FALSE, reason = "Redundancy analysis requires a formative composite."))
  }
  latent <- Filter(function(node) identical(node$role, "latent") && identical(structural_canvas_name(node), construct), snapshot$nodes %||% list())
  indicator_names <- if (length(latent)) unique(vapply(Filter(function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    (identical(as.character(from$id %||% ""), as.character(latent[[1L]]$id)) && identical(to$role, "indicator")) ||
      (identical(as.character(to$id %||% ""), as.character(latent[[1L]]$id)) && identical(from$role, "indicator"))
  }, snapshot$edges %||% list()), function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    structural_canvas_name(if (identical(from$role, "indicator")) from else to)
  }, character(1))) else character(0)
  if (criterion %in% indicator_names) return(list(available = FALSE, reason = "The global criterion must be separate from the formative indicators."))
  scores <- result$fit$construct_scores %||% NULL
  if (is.null(scores) || !construct %in% colnames(scores)) return(list(available = FALSE, reason = "The selected construct score is unavailable."))
  rawdata <- as.data.frame(result$fit$rawdata %||% data, check.names = FALSE)
  if (!criterion %in% names(rawdata) || !is.numeric(rawdata[[criterion]])) return(list(available = FALSE, reason = "The global criterion must be a numeric variable available to the PLS model."))
  score <- as.numeric(scores[, construct])
  criterion_values <- as.numeric(rawdata[[criterion]])
  if (length(score) != length(criterion_values)) return(list(available = FALSE, reason = "Construct-score and criterion row counts do not match after PLS preprocessing."))
  complete <- is.finite(score) & is.finite(criterion_values)
  n <- sum(complete)
  if (n < 4L) return(list(available = FALSE, reason = "At least four complete construct-score/criterion pairs are required."))
  correlation <- suppressWarnings(stats::cor(score[complete], criterion_values[complete]))
  if (!is.finite(correlation)) return(list(available = FALSE, reason = "The redundancy relationship could not be estimated because one variable has no variance."))
  bounded <- max(-.999999, min(.999999, correlation))
  fisher_se <- 1 / sqrt(n - 3)
  ci <- tanh(atanh(bounded) + c(-1, 1) * stats::qnorm(.975) * fisher_se)
  list(
    available = TRUE, construct = construct, criterion = criterion, n = n,
    loading = correlation, r2 = correlation^2, ci_lower = ci[[1L]], ci_upper = ci[[2L]],
    guidance = if (abs(correlation) >= .70) "At/above the descriptive .70 redundancy reference; also verify criterion content validity and independence." else "Redundancy evidence is below the common descriptive .70 loading reference; review criterion quality and construct specification."
  )
}

structural_canvas_pls_max_iterations <- function() 300L

structural_canvas_pls_stop_criterion <- function() 7L

structural_canvas_pls_r_squared_values <- function(fit) {
  values <- as.matrix(fit$rSquared %||% matrix(numeric(0), 0L, 0L))
  if (!length(values)) return(numeric(0))
  rows <- rownames(values) %||% character(0)
  position <- which(tolower(rows) %in% c("rsq", "r2", "r_squared"))
  if (!length(position)) position <- 1L
  suppressWarnings(as.numeric(values[position[[1L]], , drop = TRUE]))
}

structural_canvas_pls_positive_definite <- function(value, tolerance = 1e-8) {
  value <- as.matrix(value)
  if (!length(value) || nrow(value) != ncol(value) || any(!is.finite(value))) {
    return(list(ok = FALSE, minimum_eigenvalue = NA_real_))
  }
  symmetric <- (value + t(value)) / 2
  eigenvalues <- tryCatch(
    eigen(symmetric, symmetric = TRUE, only.values = TRUE)$values,
    error = function(error) numeric(0)
  )
  minimum <- if (length(eigenvalues)) min(eigenvalues) else NA_real_
  list(ok = is.finite(minimum) && minimum > tolerance, minimum_eigenvalue = minimum)
}

structural_canvas_pls_fit_diagnostics <- function(
  fit, estimator = "PLS", common_factor_constructs = character(0),
  max_it = structural_canvas_pls_max_iterations(),
  stop_criterion = structural_canvas_pls_stop_criterion()
) {
  estimator <- toupper(as.character(estimator %||% "PLS"))
  max_it <- suppressWarnings(as.integer(max_it %||% structural_canvas_pls_max_iterations()))
  stop_criterion <- suppressWarnings(as.integer(stop_criterion %||% structural_canvas_pls_stop_criterion()))
  if (!is.finite(max_it) || max_it < 1L) max_it <- structural_canvas_pls_max_iterations()
  if (!is.finite(stop_criterion) || stop_criterion < 1L) stop_criterion <- structural_canvas_pls_stop_criterion()
  iterations <- suppressWarnings(as.integer(fit$iterations %||% NA_integer_))
  weight_difference <- suppressWarnings(as.numeric(fit$weightDiff %||% NA_real_))
  threshold <- 10^(-stop_criterion)
  converged <- length(iterations) == 1L && is.finite(iterations) && iterations <= max_it &&
    length(weight_difference) == 1L && is.finite(weight_difference) && weight_difference < threshold
  convergence_issue <- if (converged) character(0) else paste0(
    "outer-weight iteration did not meet the ", format(threshold, scientific = TRUE),
    " stopping threshold within ", max_it, " iterations (iterations=",
    ifelse(is.finite(iterations), iterations, "NA"), ", weightDiff=",
    ifelse(is.finite(weight_difference), format(weight_difference, scientific = TRUE), "NA"), ")."
  )

  issues <- character(0)
  warnings <- character(0)
  required_components <- list(
    path_coefficients = fit$path_coef,
    outer_loadings = fit$outer_loadings,
    outer_weights = fit$outer_weights,
    construct_scores = fit$construct_scores,
    r_squared = fit$rSquared
  )
  for (name in names(required_components)) {
    component <- required_components[[name]]
    if (!is.numeric(component) || (length(component) && any(!is.finite(component)))) {
      issues <- c(issues, paste0(name, " contains non-finite values."))
    }
  }
  outer_loadings <- as.matrix(fit$outer_loadings %||% matrix(numeric(0), 0L, 0L))
  loading_constructs <- colnames(outer_loadings) %||% character(0)
  ordinary_loading_columns <- if (length(loading_constructs)) {
    !seminr:::is_interaction(loading_constructs)
  } else {
    rep(TRUE, ncol(outer_loadings))
  }
  ordinary_loadings <- if (length(outer_loadings) && any(ordinary_loading_columns)) {
    outer_loadings[, ordinary_loading_columns, drop = FALSE]
  } else {
    matrix(numeric(0), 0L, 0L)
  }
  # seminr represents a two-stage interaction with a generated single score
  # indicator.  Its stored outer loading is the generated score's scale, not a
  # standardized reflective loading, and therefore is not subject to [-1, 1].
  if (length(ordinary_loadings) && any(abs(ordinary_loadings) > 1 + 1e-8, na.rm = TRUE)) {
    issues <- c(issues, "standardized outer loadings exceed the admissible [-1, 1] range.")
  }
  r_squared <- structural_canvas_pls_r_squared_values(fit)
  if (length(r_squared) && any(!is.finite(r_squared) | r_squared < -1e-8 | r_squared > 1 + 1e-8)) {
    issues <- c(issues, "R-squared is outside the admissible [0, 1] range.")
  }

  rho_a <- as.matrix(fit$statedu_plsc_rho_a %||% matrix(numeric(0), 0L, 0L))
  adjusted_correlations <- as.matrix(fit$statedu_plsc_adjusted_correlations %||% matrix(numeric(0), 0L, 0L))
  minimum_eigenvalue <- NA_real_
  common_factor_constructs <- unique(as.character(common_factor_constructs %||% character(0)))
  if (!length(common_factor_constructs)) {
    common_factor_constructs <- unique(as.character(fit$statedu_common_factor_constructs %||% character(0)))
  }
  if (identical(estimator, "PLSC")) {
    available_rho <- intersect(common_factor_constructs, rownames(rho_a) %||% character(0))
    if (!length(common_factor_constructs) || length(available_rho) != length(common_factor_constructs)) {
      issues <- c(issues, "PLSc rho_A diagnostics are missing for one or more common-factor constructs.")
    } else {
      rho_values <- suppressWarnings(as.numeric(rho_a[available_rho, 1L, drop = TRUE]))
      if (any(!is.finite(rho_values) | rho_values <= 1e-8 | rho_values > 1 + 1e-8)) {
        issues <- c(issues, "PLSc rho_A must be finite and in the interval (0, 1].")
      }
    }
    if (!length(adjusted_correlations) || nrow(adjusted_correlations) != ncol(adjusted_correlations) ||
        any(!is.finite(adjusted_correlations))) {
      issues <- c(issues, "PLSc disattenuated construct correlations are unavailable or non-finite.")
    } else {
      off_diagonal <- adjusted_correlations[row(adjusted_correlations) != col(adjusted_correlations)]
      if (length(off_diagonal) && any(abs(off_diagonal) > 1 + 1e-8)) {
        issues <- c(issues, "PLSc disattenuated construct correlations exceed the [-1, 1] range.")
      }
      pd <- structural_canvas_pls_positive_definite(adjusted_correlations)
      minimum_eigenvalue <- pd$minimum_eigenvalue
      if (!isTRUE(pd$ok)) {
        if (isTRUE(fit$statedu_plsc_bootstrap_resample)) {
          warnings <- c(warnings, "PLSc bootstrap resample has a non-positive-definite global disattenuated correlation matrix; local structural equations remain subject to finite/admissible checks.")
        } else {
          issues <- c(issues, "PLSc disattenuated construct-correlation matrix is not positive definite.")
        }
      }
    }
    if (!identical(as.character(fit$statedu_plsc_correction_status %||% ""), "complete")) {
      issues <- c(issues, "PLSc correction did not complete.")
    }
  }
  issues <- unique(issues)
  list(
    converged = converged,
    admissible = !length(issues),
    ok = converged && !length(issues),
    iterations = iterations,
    max_iterations = max_it,
    weight_difference = weight_difference,
    stop_criterion = stop_criterion,
    convergence_threshold = threshold,
    estimator = if (identical(estimator, "PLSC")) "PLSc" else "PLS",
    rho_a = rho_a,
    adjusted_correlations = adjusted_correlations,
    minimum_adjusted_correlation_eigenvalue = minimum_eigenvalue,
    convergence_issue = convergence_issue,
    admissibility_issues = issues,
    diagnostic_warnings = unique(warnings),
    message = paste(c(convergence_issue, issues), collapse = " ")
  )
}

structural_canvas_pls_assert_fit <- function(fit, estimator = "PLS", common_factor_constructs = character(0), stage = "model") {
  diagnostics <- structural_canvas_pls_fit_diagnostics(fit, estimator, common_factor_constructs)
  if (!isTRUE(diagnostics$converged)) {
    stop(paste0(diagnostics$estimator, " ", stage, " did not converge: ", diagnostics$convergence_issue), call. = FALSE)
  }
  if (!isTRUE(diagnostics$admissible)) {
    stop(paste0(diagnostics$estimator, " ", stage, " is inadmissible: ", paste(diagnostics$admissibility_issues, collapse = " ")), call. = FALSE)
  }
  diagnostics
}

structural_canvas_pls_bootstrap_draw_gate <- function(fit, estimator = "PLS", common_factor_constructs = character(0)) {
  diagnostics <- structural_canvas_pls_fit_diagnostics(fit, estimator, common_factor_constructs)
  reason <- if (!isTRUE(diagnostics$converged)) "nonconvergence" else if (!isTRUE(diagnostics$admissible)) "inadmissible" else ""
  list(valid = !nzchar(reason), reason = reason, diagnostics = diagnostics)
}

structural_canvas_apply_plsc <- function(fit, common_factor_constructs = character(0), bootstrap_resample = FALSE) {
  score_names <- if (is.null(fit$construct_scores)) character(0) else colnames(fit$construct_scores)
  common_factor_constructs <- intersect(as.character(common_factor_constructs), score_names)
  if (!length(common_factor_constructs)) stop("PLSc requires at least one reflective common-factor construct.")
  all_constructs <- seminr:::constructs_in_model(fit)$construct_names
  sm_matrix <- fit$smMatrix
  mm_matrix <- fit$mmMatrix
  path_coef <- fit$path_coef
  loadings <- fit$outer_loadings
  construct_scores <- fit$construct_scores
  rho <- seminr:::rho_A(fit, all_constructs)
  rho[!rownames(rho) %in% common_factor_constructs, ] <- 1
  rho[seminr:::is_interaction(rownames(rho)), ] <- 1
  common_rho <- suppressWarnings(as.numeric(rho[common_factor_constructs, 1L, drop = TRUE]))
  if (any(!is.finite(common_rho) | common_rho <= 1e-8 | common_rho > 1 + 1e-8)) {
    stop("PLSc correction failed because common-factor rho_A must be finite and in the interval (0, 1].", call. = FALSE)
  }
  adjustment <- sqrt(rho %*% t(rho))
  diag(adjustment) <- 1
  adjusted_correlations <- stats::cor(construct_scores, use = "pairwise.complete.obs") / adjustment
  if (any(!is.finite(adjusted_correlations))) {
    stop("PLSc correction failed because the disattenuated construct-correlation matrix contains non-finite values.")
  }
  off_diagonal <- adjusted_correlations[row(adjusted_correlations) != col(adjusted_correlations)]
  if (length(off_diagonal) && any(abs(off_diagonal) > 1 + 1e-8)) {
    stop("PLSc correction failed because a disattenuated construct correlation is outside [-1, 1].", call. = FALSE)
  }
  pd <- structural_canvas_pls_positive_definite(adjusted_correlations)
  if (!isTRUE(pd$ok) && !isTRUE(bootstrap_resample)) {
    stop(paste0(
      "PLSc correction failed because the disattenuated construct-correlation matrix is not positive definite",
      if (is.finite(pd$minimum_eigenvalue)) paste0(" (minimum eigenvalue=", format(pd$minimum_eigenvalue, scientific = TRUE), ")") else "",
      "."
    ), call. = FALSE)
  }
  corrected_endogenous <- character(0)
  for (endogenous in seminr:::all_endogenous(sm_matrix)) {
    antecedents <- seminr:::construct_antecedents(sm_matrix, endogenous)
    if (!length(antecedents)) next
    coefficients <- tryCatch(
      solve(adjusted_correlations[antecedents, antecedents, drop = FALSE], adjusted_correlations[antecedents, endogenous, drop = FALSE]),
      error = function(error) stop(
        paste0("PLSc correction failed for endogenous construct '", endogenous, "': ", conditionMessage(error)),
        call. = FALSE
      )
    )
    if (any(!is.finite(coefficients))) {
      stop(paste0("PLSc correction failed for endogenous construct '", endogenous, "': corrected path coefficients are non-finite."), call. = FALSE)
    }
    path_coef[antecedents, endogenous] <- coefficients
    corrected_endogenous <- c(corrected_endogenous, endogenous)
  }
  for (construct in common_factor_constructs) {
    indicators <- seminr:::construct_items(mm_matrix, construct)
    available <- intersect(indicators, rownames(loadings))
    if (length(available)) {
      weights <- as.matrix(fit$outer_weights[available, construct])
      loadings[available, construct] <- weights %*% (sqrt(rho[construct, 1L]) / (t(weights) %*% weights))
    }
  }
  fit$path_coef <- path_coef
  fit$outer_loadings <- loadings
  fit$rSquared <- seminr:::metrics_insample(
    fit$data, construct_scores, sm_matrix, seminr:::all_endogenous(sm_matrix), adjusted_correlations
  )
  fit$statedu_common_factor_constructs <- common_factor_constructs
  fit$statedu_plsc_mode <- if (setequal(common_factor_constructs, all_constructs)) "all_common_factors" else "mixed_common_factors_and_composites"
  fit$statedu_plsc_correction_status <- "complete"
  fit$statedu_plsc_corrected_endogenous <- unique(corrected_endogenous)
  fit$statedu_plsc_rho_a <- rho
  fit$statedu_plsc_adjusted_correlations <- adjusted_correlations
  fit$statedu_plsc_minimum_eigenvalue <- pd$minimum_eigenvalue
  fit$statedu_plsc_bootstrap_resample <- isTRUE(bootstrap_resample)
  fit$statedu_plsc_nonpositive_definite_resample <- isTRUE(bootstrap_resample) && !isTRUE(pd$ok)
  fit$statedu_fit_diagnostics <- structural_canvas_pls_assert_fit(
    fit, "PLSC", common_factor_constructs, stage = "corrected solution"
  )
  fit
}

structural_canvas_pls_moderation_method <- function(snapshot) {
  requested <- as.character(snapshot$moderationMethod %||% "two_stage")
  if (!length(requested) || is.na(requested[[1L]]) || !nzchar(trimws(requested[[1L]]))) requested <- "two_stage"
  requested <- tolower(trimws(requested[[1L]]))
  aliases <- c(
    "two-stage" = "two_stage",
    "twostage" = "two_stage",
    "product-indicator" = "product_indicator",
    "productindicator" = "product_indicator",
    "orthogonalized" = "orthogonal"
  )
  if (requested %in% names(aliases)) requested <- unname(aliases[[requested]])
  supported <- c("two_stage", "product_indicator", "orthogonal")
  effective <- if (requested %in% supported) requested else "two_stage"
  method_function <- switch(
    effective,
    product_indicator = seminr::product_indicator,
    orthogonal = seminr::orthogonal,
    seminr::two_stage
  )
  list(
    requested = requested,
    effective = effective,
    normalized = !identical(requested, effective),
    normalization_reason = if (!identical(requested, effective)) {
      "A saved CB-SEM or legacy moderation method was normalized to the supported PLS two-stage interaction."
    } else "",
    seminr_method = method_function
  )
}

structural_canvas_pls_moderation_specification <- function(snapshot, edges) {
  moderations <- snapshot$moderations %||% list()
  method <- structural_canvas_pls_moderation_method(snapshot)
  if (!length(moderations)) {
    return(list(
      method = method, definitions = list(), interaction_pairs = data.frame(),
      added_paths = list(), interaction_constructs = character(0)
    ))
  }
  nodes <- snapshot$nodes %||% list()
  node_ids <- vapply(nodes, function(node) as.character(node$id %||% ""), character(1))
  node_by_id <- stats::setNames(nodes, node_ids)
  node_for <- function(id) node_by_id[[as.character(id %||% "")]] %||% NULL
  edge_ids <- vapply(edges, function(edge) as.character(edge$id %||% ""), character(1))
  edge_by_id <- stats::setNames(edges, edge_ids)
  edge_for <- function(id) edge_by_id[[as.character(id %||% "")]] %||% NULL
  latent_name <- function(node, element) {
    if (is.null(node) || !identical(as.character(node$role %||% ""), "latent")) {
      stop(paste0("PLS latent moderation requires ", element, " to be a latent construct."), call. = FALSE)
    }
    value <- trimws(as.character(structural_canvas_name(node) %||% ""))
    if (!nzchar(value)) stop(paste0("PLS latent moderation found an unnamed ", element, "."), call. = FALSE)
    value
  }
  base_structural <- Filter(function(edge) {
    if (identical(as.character(edge$kind %||% ""), "covariance")) return(FALSE)
    from <- node_for(edge$from)
    to <- node_for(edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")
  }, edges)
  existing_pairs <- unique(vapply(base_structural, function(edge) {
    paste(as.character(edge$from %||% ""), as.character(edge$to %||% ""), sep = "\r")
  }, character(1)))
  definitions <- list()
  interaction_rows <- list()
  added_paths <- list()
  definition_keys <- character(0)
  pair_keys <- character(0)
  for (index in seq_along(moderations)) {
    moderation <- moderations[[index]]
    target_edge_id <- as.character(moderation$toEdge %||% "")
    target <- edge_for(target_edge_id)
    if (!nzchar(target_edge_id) || is.null(target)) {
      stop("PLS latent moderation references a structural path that no longer exists.", call. = FALSE)
    }
    if (identical(as.character(target$kind %||% ""), "covariance")) {
      stop("PLS latent moderation requires a directed structural path, not a covariance.", call. = FALSE)
    }
    predictor_node <- node_for(target$from)
    outcome_node <- node_for(target$to)
    moderator_node <- node_for(moderation$from)
    predictor <- latent_name(predictor_node, "moderated predictor")
    outcome <- latent_name(outcome_node, "moderated outcome")
    moderator <- latent_name(moderator_node, "moderator")
    if (identical(predictor, moderator)) {
      stop("PLS latent moderation requires different predictor and moderator constructs.", call. = FALSE)
    }
    if (identical(outcome, moderator)) {
      stop("PLS latent moderation cannot use the outcome construct as its own moderator.", call. = FALSE)
    }
    interaction <- paste(predictor, moderator, sep = "*")
    definition_key <- paste(predictor, moderator, outcome, sep = "\r")
    if (definition_key %in% definition_keys) {
      stop(paste0("Duplicate PLS latent-moderation edge: ", predictor, " x ", moderator, " -> ", outcome, "."), call. = FALSE)
    }
    definition_keys <- c(definition_keys, definition_key)
    pair_key <- paste(predictor, moderator, sep = "\r")
    if (!pair_key %in% pair_keys) {
      pair_keys <- c(pair_keys, pair_key)
      interaction_rows[[length(interaction_rows) + 1L]] <- data.frame(
        predictor = predictor, moderator = moderator, interaction = interaction,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
    moderator_path_key <- paste(as.character(moderator_node$id), as.character(outcome_node$id), sep = "\r")
    main_effect_added <- !moderator_path_key %in% existing_pairs
    if (main_effect_added) {
      existing_pairs <- c(existing_pairs, moderator_path_key)
      added_paths[[length(added_paths) + 1L]] <- list(
        from = moderator, to = outcome, type = "hierarchy_main_effect",
        source_node_id = as.character(moderator_node$id), target_node_id = as.character(outcome_node$id)
      )
    }
    added_paths[[length(added_paths) + 1L]] <- list(
      from = interaction, to = outcome, type = "interaction",
      source_node_id = "", target_node_id = as.character(outcome_node$id)
    )
    definitions[[length(definitions) + 1L]] <- list(
      id = as.character(moderation$id %||% paste0("pls_moderation_", index)),
      target_edge_id = target_edge_id,
      predictor = predictor,
      outcome = outcome,
      moderator = moderator,
      interaction = interaction,
      interaction_factor = interaction,
      interaction_path = paste0(interaction, " -> ", outcome),
      direct_path = paste0(predictor, " -> ", outcome),
      moderator_main_path = paste0(moderator, " -> ", outcome),
      moderator_main_effect_auto_added = main_effect_added,
      hierarchy = "Strong hierarchy: predictor and moderator main effects included",
      method_requested = method$requested,
      method = method$effective,
      method_normalized = method$normalized,
      method_normalization_reason = method$normalization_reason,
      product_indicator_method = method$effective,
      moderator_role = "latent",
      moderator_mean = 0,
      moderator_sd = 1,
      moderator_min = -1,
      moderator_max = 1,
      scale = "Standardized seminr construct-score scale",
      plsc_interaction_correction = "Uncorrected composite interaction (rho_A fixed to 1); base common-factor constructs retain selective PLSc correction"
    )
  }
  interaction_pairs <- if (length(interaction_rows)) {
    do.call(rbind, interaction_rows)
  } else {
    data.frame(predictor = character(0), moderator = character(0), interaction = character(0), stringsAsFactors = FALSE)
  }

  # The hierarchy-generated moderator paths must not introduce a feedback loop.
  graph_edges <- c(
    lapply(base_structural, function(edge) list(
      from = latent_name(node_for(edge$from), "structural predictor"),
      to = latent_name(node_for(edge$to), "structural outcome")
    )),
    Filter(function(path) identical(path$type, "hierarchy_main_effect"), added_paths)
  )
  adjacency <- split(
    vapply(graph_edges, function(path) as.character(path$to), character(1)),
    vapply(graph_edges, function(path) as.character(path$from), character(1))
  )
  visiting <- character(0)
  visited <- character(0)
  visit <- function(name) {
    if (name %in% visiting) return(TRUE)
    if (name %in% visited) return(FALSE)
    visiting <<- c(visiting, name)
    cyclic <- any(vapply(adjacency[[name]] %||% character(0), visit, logical(1)))
    visiting <<- setdiff(visiting, name)
    visited <<- c(visited, name)
    cyclic
  }
  graph_nodes <- unique(c(
    vapply(graph_edges, function(path) as.character(path$from), character(1)),
    vapply(graph_edges, function(path) as.character(path$to), character(1))
  ))
  if (length(graph_nodes) && any(vapply(graph_nodes, visit, logical(1)))) {
    stop("PLS hierarchy enforcement for latent moderation would create a directed feedback loop.", call. = FALSE)
  }
  list(
    method = method,
    definitions = definitions,
    interaction_pairs = interaction_pairs,
    added_paths = added_paths,
    interaction_constructs = unique(interaction_pairs$interaction)
  )
}

structural_canvas_pls_moderation_point_tables <- function(path_coef, definitions, estimator = "PLS") {
  definitions <- definitions %||% list()
  estimator <- toupper(as.character(estimator %||% "PLS"))
  correction <- if (identical(estimator, "PLSC")) {
    "Uncorrected composite interaction (rho_A fixed to 1)"
  } else {
    "Not applicable (PLS composite interaction)"
  }
  effect_rows <- list()
  slope_rows <- list()
  for (definition in definitions) {
    predictor <- as.character(definition$predictor %||% "")
    moderator <- as.character(definition$moderator %||% "")
    outcome <- as.character(definition$outcome %||% "")
    interaction <- as.character(definition$interaction %||% definition$interaction_factor %||% "")
    required <- c(predictor, moderator, interaction)
    if (!all(required %in% rownames(path_coef)) || !outcome %in% colnames(path_coef)) {
      stop(paste0("PLS moderation path contract is incomplete for ", predictor, " x ", moderator, " -> ", outcome, "."), call. = FALSE)
    }
    direct <- suppressWarnings(as.numeric(path_coef[predictor, outcome]))
    moderator_effect <- suppressWarnings(as.numeric(path_coef[moderator, outcome]))
    interaction_effect <- suppressWarnings(as.numeric(path_coef[interaction, outcome]))
    if (any(!is.finite(c(direct, moderator_effect, interaction_effect)))) {
      stop("PLS moderation point estimates contain non-finite values.", call. = FALSE)
    }
    effect_rows[[length(effect_rows) + 1L]] <- data.frame(
      Predictor = predictor,
      Moderator = moderator,
      Outcome = outcome,
      Interaction = interaction,
      Method = as.character(definition$method %||% "two_stage"),
      Estimate = interaction_effect,
      `Predictor main effect` = direct,
      `Moderator main effect` = moderator_effect,
      `Moderator main effect auto-added` = isTRUE(definition$moderator_main_effect_auto_added),
      `PLSc interaction correction` = correction,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    levels <- c(`-1 SD` = -1, Mean = 0, `+1 SD` = 1)
    for (level in names(levels)) {
      value <- unname(levels[[level]])
      slope_rows[[length(slope_rows) + 1L]] <- data.frame(
        Predictor = predictor,
        Moderator = moderator,
        Outcome = outcome,
        Interaction = interaction,
        Method = as.character(definition$method %||% "two_stage"),
        `Moderator level` = level,
        `Moderator value` = value,
        `Direct effect` = direct,
        `Interaction effect` = interaction_effect,
        `Simple slope` = direct + value * interaction_effect,
        `PLSc interaction correction` = correction,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  empty_effect <- data.frame(
    Predictor = character(0), Moderator = character(0), Outcome = character(0),
    Interaction = character(0), Method = character(0), Estimate = numeric(0),
    `Predictor main effect` = numeric(0), `Moderator main effect` = numeric(0),
    `Moderator main effect auto-added` = logical(0),
    `PLSc interaction correction` = character(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  empty_slope <- data.frame(
    Predictor = character(0), Moderator = character(0), Outcome = character(0),
    Interaction = character(0), Method = character(0), `Moderator level` = character(0),
    `Moderator value` = numeric(0), `Direct effect` = numeric(0),
    `Interaction effect` = numeric(0), `Simple slope` = numeric(0),
    `PLSc interaction correction` = character(0),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  list(
    effects = if (length(effect_rows)) do.call(rbind, effect_rows) else empty_effect,
    simple_slopes = if (length(slope_rows)) do.call(rbind, slope_rows) else empty_slope
  )
}

structural_canvas_run_pls_analysis <- function(snapshot, data, latents, edges, estimator = "PLS") {
  structural_canvas_validate_pls_model_contract(snapshot, "plssem")
  selection <- structural_canvas_select_pls_estimator(snapshot, estimator)
  estimator <- selection$selected
  resolved_specification <- structural_canvas_resolve_construct_specification(snapshot, "plssem", estimator)
  unsupported <- !resolved_specification$supported
  if (any(unsupported)) stop(paste(unique(resolved_specification$reason[unsupported]), collapse = " "))
  moderation_specification <- structural_canvas_pls_moderation_specification(snapshot, edges)
  latent_indicators <- function(latent) {
    vapply(Filter(function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(from$id, latent$id) && identical(to$role, "indicator")) ||
        (identical(to$id, latent$id) && identical(from$role, "indicator"))
    }, edges), function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "indicator")) from else to)
    }, character(1))
  }
  indicator_names <- unique(unlist(lapply(latents, latent_indicators), use.names = FALSE))
  missing_indicators <- setdiff(indicator_names, names(data))
  if (length(missing_indicators)) {
    stop(paste0(
      "PLS model indicators missing from the current data: ",
      paste(missing_indicators, collapse = ", "),
      ". Reassign the highlighted measurement variables to columns in the current data."
    ))
  }
  constructs <- lapply(latents, function(latent) {
    indicator_names <- latent_indicators(latent)
    latent_name <- structural_canvas_name(latent)
    resolved <- resolved_specification[resolved_specification$name == latent_name, , drop = FALSE]
    if (!nrow(resolved)) stop(paste0("Resolved PLS construct specification is missing for: ", latent_name, "."))
    if (identical(resolved$effective_weighting[[1L]], "Mode B")) {
      seminr::composite(latent_name, indicator_names, weights = seminr::mode_B)
    } else {
      # seminr::reflective() triggers PLSc automatically inside estimate_pls().
      # Build the uncorrected Mode A score model first so PLS and PLSc remain
      # distinct estimators; declared common factors are corrected explicitly
      # and selectively below only when PLSc is requested.
      seminr::composite(latent_name, indicator_names, weights = seminr::mode_A)
    }
  })
  construct_names <- vapply(latents, structural_canvas_name, character(1))
  if (any(moderation_specification$interaction_constructs %in% construct_names)) {
    stop("A generated PLS interaction construct conflicts with an existing construct name.", call. = FALSE)
  }
  if (nrow(moderation_specification$interaction_pairs)) {
    interaction_constructs <- lapply(seq_len(nrow(moderation_specification$interaction_pairs)), function(index) {
      seminr::interaction_term(
        iv = moderation_specification$interaction_pairs$predictor[[index]],
        moderator = moderation_specification$interaction_pairs$moderator[[index]],
        method = moderation_specification$method$seminr_method,
        weights = seminr::mode_A
      )
    })
    constructs <- c(constructs, interaction_constructs)
  }
  base_path_records <- lapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) {
    list(
      from = structural_canvas_name(structural_canvas_node(snapshot, edge$from)),
      to = structural_canvas_name(structural_canvas_node(snapshot, edge$to)),
      type = "specified"
    )
  })
  path_records <- c(base_path_records, moderation_specification$added_paths)
  path_keys <- vapply(path_records, function(path) paste(path$from, path$to, sep = "\r"), character(1))
  path_records <- path_records[!duplicated(path_keys)]
  path_specs <- lapply(path_records, function(path) seminr::paths(from = path$from, to = path$to))
  structural_paths <- vapply(path_records, function(path) paste0(path$to, " ~ ", path$from), character(1))
  ignored_covariances <- vapply(Filter(function(edge) identical(edge$kind, "covariance"), edges), function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    from_name <- if (is.null(from)) "" else structural_canvas_name(from)
    to_name <- if (is.null(to)) "" else structural_canvas_name(to)
    label <- paste(c(from_name, to_name)[nzchar(c(from_name, to_name))], collapse = " ~~ ")
    if (nzchar(label)) label else as.character(edge$id %||% "covariance")
  }, character(1))
  measurement_lines <- vapply(latents, function(latent) {
    latent_name <- structural_canvas_name(latent)
    indicators <- latent_indicators(latent)
    operator <- if (identical(latent$measurementMode %||% "reflective", "formative")) "<~" else "=~"
    paste(latent_name, operator, paste(indicators, collapse = " + "))
  }, character(1))
  estimator <- toupper(as.character(estimator %||% "PLS"))
  if (!estimator %in% c("PLS", "PLSC")) estimator <- "PLS"
  fit <- seminr::estimate_pls(
    data = data,
    measurement_model = do.call(seminr::constructs, constructs),
    structural_model = if (length(path_specs)) do.call(seminr::relationships, path_specs) else NULL,
    missing = seminr::mean_replacement,
    maxIt = structural_canvas_pls_max_iterations(),
    stopCriterion = structural_canvas_pls_stop_criterion(),
    assess_syntax = FALSE
  )
  fit$statedu_common_factor_constructs <- selection$common_factors
  fit$statedu_composite_constructs <- selection$composites
  fit$statedu_fit_diagnostics <- structural_canvas_pls_assert_fit(
    fit, "PLS", character(0), stage = "base solution"
  )
  if (identical(estimator, "PLSC")) {
    fit <- structural_canvas_apply_plsc(fit, selection$common_factors)
  }
  fit_diagnostics <- fit$statedu_fit_diagnostics %||% structural_canvas_pls_assert_fit(
    fit, estimator, selection$common_factors, stage = "solution"
  )
  effect_points <- structural_canvas_pls_effect_point_tables(fit$path_coef, fit$smMatrix)
  effect_contract <- structural_canvas_pls_effect_finiteness_contract(fit$path_coef, effect_points$paths)
  if (!isTRUE(effect_contract$valid)) {
    stop(
      "PLS fitted direct, indirect, or total effects contain non-finite values; the analysis was stopped.",
      call. = FALSE
    )
  }
  fit$statedu_direct_paths <- effect_points$direct
  fit$statedu_specific_indirect_paths <- effect_points$specific
  fit$statedu_total_indirect_paths <- effect_points$total_indirect
  fit$statedu_total_paths <- effect_points$total
  moderation_points <- structural_canvas_pls_moderation_point_tables(
    fit$path_coef, moderation_specification$definitions, estimator
  )
  fit$statedu_moderation_definitions <- moderation_specification$definitions
  fit$statedu_moderation_effects <- moderation_points$effects
  fit$statedu_moderation_simple_slopes <- moderation_points$simple_slopes
  fit$statedu_interaction_constructs <- moderation_specification$interaction_constructs
  fit$statedu_moderation_method <- moderation_specification$method
  fit$statedu_plsc_uncorrected_interactions <- if (identical(estimator, "PLSC")) {
    moderation_specification$interaction_constructs
  } else character(0)
  list(
    fit = fit,
    estimator = if (identical(estimator, "PLSC")) "PLSc" else "PLS",
    estimator_requested = selection$requested,
    estimator_selection_mode = selection$mode,
    estimator_selection_reason = selection$reason,
    plsc_corrected_constructs = selection$common_factors,
    plsc_uncorrected_composites = unique(c(selection$composites, moderation_specification$interaction_constructs)),
    plsc_uncorrected_interactions = if (identical(estimator, "PLSC")) moderation_specification$interaction_constructs else character(0),
    plsc_correction_status = as.character(fit$statedu_plsc_correction_status %||% if (identical(estimator, "PLSC")) "not recorded" else "not applicable"),
    plsc_corrected_endogenous = as.character(fit$statedu_plsc_corrected_endogenous %||% character(0)),
    syntax = paste(c(measurement_lines, structural_paths), collapse = "\n"),
    converged = isTRUE(fit_diagnostics$converged),
    convergence_diagnostics = fit_diagnostics,
    n = nrow(data),
    observed = indicator_names,
    constructs = construct_names,
    interaction_constructs = moderation_specification$interaction_constructs,
    moderation_method = moderation_specification$method,
    moderation_definitions = moderation_specification$definitions,
    moderation_effects = moderation_points$effects,
    moderation_simple_slopes = moderation_points$simple_slopes,
    structural_paths = structural_paths,
    ignored_covariances = unique(ignored_covariances),
    resolved_construct_specification = resolved_specification,
    admissible = isTRUE(fit_diagnostics$admissible),
    admissibility_diagnostics = fit_diagnostics
  )
}

structural_canvas_write_bootstrap_progress <- function(progress_file, completed, total, phase = "bootstrap", determinate = TRUE) {
  progress_file <- as.character(progress_file %||% "")
  if (!nzchar(progress_file)) return(invisible(FALSE))
  payload <- list(
    completed = as.integer(completed %||% 0L), total = as.integer(total %||% 0L),
    phase = as.character(phase %||% "bootstrap"), determinate = isTRUE(determinate),
    updated_at = as.numeric(Sys.time())
  )
  temporary <- paste0(progress_file, ".tmp")
  tryCatch({
    saveRDS(payload, temporary)
    file.rename(temporary, progress_file)
  }, error = function(error) FALSE)
  invisible(TRUE)
}

structural_canvas_rng_streams <- function(n, seed = default_seed()) {
  n <- suppressWarnings(as.integer(n))
  seed <- suppressWarnings(as.integer(seed))
  if (!is.finite(n) || n < 1L) return(list())
  if (!is.finite(seed) || seed < 1L) seed <- default_seed()
  old_kind <- RNGkind()
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  streams <- vector("list", n)
  stream <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  for (index in seq_len(n)) {
    streams[[index]] <- stream
    stream <- parallel::nextRNGStream(stream)
  }
  streams
}

structural_canvas_pls_bootstrap_min_valid_ratio <- function() .80

structural_canvas_pls_bootstrap_validity <- function(valid, requested) {
  valid <- suppressWarnings(as.integer(valid %||% 0L))
  requested <- suppressWarnings(as.integer(requested %||% 0L))
  if (!is.finite(valid) || valid < 0L) valid <- 0L
  if (!is.finite(requested) || requested < 1L) requested <- 0L
  ratio <- if (requested > 0L) valid / requested else NA_real_
  minimum_ratio <- structural_canvas_pls_bootstrap_min_valid_ratio()
  minimum_valid <- if (requested > 0L) as.integer(max(2L, ceiling(minimum_ratio * requested))) else NA_integer_
  adequate <- requested > 0L && valid >= minimum_valid && is.finite(ratio) && ratio >= minimum_ratio
  list(
    valid = valid,
    requested = requested,
    ratio = ratio,
    minimum_ratio = minimum_ratio,
    minimum_valid = minimum_valid,
    adequate = adequate,
    status = if (adequate) "Adequate" else "Insufficient"
  )
}

structural_canvas_pls_bootstrap_required_masks <- function(reference_components) {
  masks <- lapply(names(reference_components), function(name) {
    reference <- reference_components[[name]]
    if (identical(name, "htmt")) {
      if (length(dim(reference)) != 2L || nrow(reference) != ncol(reference)) {
        return(array(FALSE, dim = dim(reference), dimnames = dimnames(reference)))
      }
      return(upper.tri(reference, diag = FALSE))
    }
    array(TRUE, dim = dim(reference), dimnames = dimnames(reference))
  })
  names(masks) <- names(reference_components)
  masks
}

structural_canvas_pls_bootstrap_components_contract <- function(components, reference_components) {
  expected_names <- names(reference_components)
  if (!is.list(components) || !identical(names(components), expected_names)) {
    return(list(valid = FALSE, reason = "component_names"))
  }
  required_masks <- structural_canvas_pls_bootstrap_required_masks(reference_components)
  for (name in expected_names) {
    value <- components[[name]]
    reference <- reference_components[[name]]
    if (!is.numeric(value) || !identical(dim(value), dim(reference)) ||
        !identical(dimnames(value), dimnames(reference))) {
      return(list(valid = FALSE, reason = paste0("component_shape:", name)))
    }
    required <- required_masks[[name]]
    if (length(value) != length(reference) || length(required) != length(reference) ||
        any(!is.finite(reference[required])) || any(!is.finite(value[required])) ||
        any(is.finite(value[!required]))) {
      return(list(valid = FALSE, reason = paste0("nonfinite_statistics:", name)))
    }
  }
  list(valid = TRUE, reason = "")
}

structural_canvas_pls_bootstrap_select_draws <- function(values, expected_length, required_mask = NULL) {
  expected_length <- suppressWarnings(as.integer(expected_length))
  if (is.null(required_mask)) required_mask <- rep(TRUE, expected_length)
  required_mask <- as.logical(required_mask)
  reasons <- vapply(values, function(value) as.character(attr(value, "failure_reason") %||% ""), character(1))
  valid <- vapply(values, function(value) {
    is.numeric(value) && length(value) == expected_length && length(required_mask) == expected_length &&
      identical(is.finite(value), required_mask)
  }, logical(1))
  reasons[!valid & !nzchar(reasons)] <- "invalid_statistics"
  list(valid = valid, values = values[valid], failure_reasons = reasons)
}

structural_canvas_pls_bootstrap_suppress_inference <- function(value) {
  if (!is.matrix(value) && !is.data.frame(value)) return(value)
  columns <- intersect(
    c("Bootstrap SD", "T Stat.", "2.5% CI", "97.5% CI", "Bootstrap P Val"),
    colnames(value) %||% character(0)
  )
  if (length(columns)) {
    if (is.data.frame(value)) {
      for (column in columns) value[[column]] <- rep(NA_real_, nrow(value))
    } else if (nrow(value)) {
      value[, columns] <- NA_real_
    }
  }
  attr(value, "inference_suppressed") <- TRUE
  value
}

structural_canvas_pls_effect_path_key <- function(path, type = c("direct", "specific", "total_indirect", "total")) {
  type <- match.arg(type)
  path <- as.character(path %||% character(0))
  if (!length(path)) return("")
  if (identical(type, "specific")) {
    paste(c(type, path), collapse = "|")
  } else {
    paste(c(type, path[[1L]], path[[length(path)]]), collapse = "|")
  }
}

structural_canvas_pls_effect_path_label <- function(path) {
  paste(as.character(path %||% character(0)), collapse = " -> ")
}

structural_canvas_pls_structural_edges <- function(path_coef, structural_model = NULL) {
  path_coef <- as.matrix(path_coef)
  row_names <- rownames(path_coef) %||% character(0)
  column_names <- colnames(path_coef) %||% character(0)
  if (anyDuplicated(row_names) || anyDuplicated(column_names)) {
    return(data.frame(source = character(0), target = character(0)))
  }
  structure <- as.matrix(structural_model %||% matrix(character(0), 0L, 2L))
  if (nrow(structure) && all(c("source", "target") %in% colnames(structure))) {
    edges <- data.frame(
      source = trimws(as.character(structure[, "source"])),
      target = trimws(as.character(structure[, "target"])),
      stringsAsFactors = FALSE
    )
    edges <- edges[nzchar(edges$source) & nzchar(edges$target) & edges$source != edges$target, , drop = FALSE]
  } else {
    positions <- which(is.finite(path_coef) & path_coef != 0, arr.ind = TRUE)
    edges <- if (nrow(positions)) data.frame(
      source = row_names[positions[, 1L]], target = column_names[positions[, 2L]], stringsAsFactors = FALSE
    ) else data.frame(source = character(0), target = character(0))
  }
  edges <- unique(edges)
  edges[order(edges$source, edges$target, method = "radix"), , drop = FALSE]
}

structural_canvas_pls_effect_paths <- function(path_coef, structural_model = NULL) {
  path_coef <- as.matrix(path_coef)
  edges <- structural_canvas_pls_structural_edges(path_coef, structural_model)
  nodes <- sort(unique(c(rownames(path_coef) %||% character(0), colnames(path_coef) %||% character(0), edges$source, edges$target)), method = "radix")
  if (!length(nodes)) return(list(nodes = character(0), direct = list(), all = list(), specific = list()))
  direct_paths <- lapply(seq_len(nrow(edges)), function(index) c(edges$source[[index]], edges$target[[index]]))
  adjacency <- stats::setNames(lapply(nodes, function(node) {
    sort(unique(edges$target[edges$source == node]), method = "radix")
  }), nodes)
  paths <- list()
  append_descendants <- function(current, path) {
    neighbors <- adjacency[[current]] %||% character(0)
    for (neighbor in neighbors) {
      if (neighbor %in% path) next
      next_path <- c(path, neighbor)
      paths[[length(paths) + 1L]] <<- next_path
      append_descendants(neighbor, next_path)
    }
    invisible(NULL)
  }
  for (predictor in nodes) append_descendants(predictor, predictor)
  if (length(paths)) {
    labels <- vapply(paths, structural_canvas_pls_effect_path_label, character(1))
    paths <- paths[order(labels, method = "radix")]
    labels <- labels[order(labels, method = "radix")]
    paths <- paths[!duplicated(labels)]
  }
  list(
    nodes = nodes,
    direct = direct_paths,
    all = paths,
    specific = Filter(function(path) length(path) >= 3L, paths)
  )
}

structural_canvas_pls_effect_path_product <- function(path_coef, path) {
  path_coef <- as.matrix(path_coef)
  path <- as.character(path %||% character(0))
  if (length(path) < 2L) return(NA_real_)
  edges <- vapply(seq_len(length(path) - 1L), function(index) {
    suppressWarnings(as.numeric(path_coef[path[[index]], path[[index + 1L]]]))
  }, numeric(1))
  if (any(!is.finite(edges))) NA_real_ else prod(edges)
}

structural_canvas_pls_effect_point_table <- function(path_coef, paths, type = c("direct", "specific", "total_indirect", "total")) {
  type <- match.arg(type)
  paths <- paths %||% list()
  if (!length(paths)) {
    return(data.frame(
      `Estimand Key` = character(0), Path = character(0), Predictor = character(0),
      Outcome = character(0), Mediators = character(0), `Original Est.` = numeric(0),
      `Bootstrap Status` = character(0), `Inference Source` = character(0),
      `Valid N` = integer(0), `Requested N` = integer(0), check.names = FALSE
    ))
  }
  if (identical(type, "specific")) {
    selected_paths <- paths
    estimates <- vapply(selected_paths, structural_canvas_pls_effect_path_product, numeric(1), path_coef = path_coef)
  } else {
    pair_keys <- vapply(paths, function(path) paste(path[[1L]], path[[length(path)]], sep = "\r"), character(1))
    unique_pairs <- sort(unique(pair_keys), method = "radix")
    selected_paths <- lapply(unique_pairs, function(key) strsplit(key, "\r", fixed = TRUE)[[1L]])
    estimates <- vapply(unique_pairs, function(key) {
      sum(vapply(paths[pair_keys == key], structural_canvas_pls_effect_path_product, numeric(1), path_coef = path_coef))
    }, numeric(1))
  }
  labels <- vapply(selected_paths, structural_canvas_pls_effect_path_label, character(1))
  output <- data.frame(
    `Estimand Key` = vapply(selected_paths, structural_canvas_pls_effect_path_key, character(1), type = type),
    Path = labels,
    Predictor = vapply(selected_paths, `[[`, character(1), 1L),
    Outcome = vapply(selected_paths, function(path) path[[length(path)]], character(1)),
    Mediators = if (identical(type, "specific")) {
      vapply(selected_paths, function(path) paste(path[-c(1L, length(path))], collapse = " -> "), character(1))
    } else rep("", length(selected_paths)),
    `Original Est.` = as.numeric(estimates),
    `Bootstrap Status` = rep("Not requested", length(selected_paths)),
    `Inference Source` = rep("StatEdu fitted-path product point estimate", length(selected_paths)),
    `Valid N` = rep.int(0L, length(selected_paths)),
    `Requested N` = rep.int(0L, length(selected_paths)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(output) <- labels
  output
}

structural_canvas_pls_effect_point_tables <- function(path_coef, structural_model = NULL) {
  paths <- structural_canvas_pls_effect_paths(path_coef, structural_model)
  indirect_pair_keys <- unique(vapply(paths$specific, function(path) paste(path[[1L]], path[[length(path)]], sep = "\r"), character(1)))
  total_paths <- Filter(function(path) paste(path[[1L]], path[[length(path)]], sep = "\r") %in% indirect_pair_keys, paths$all)
  list(
    direct = structural_canvas_pls_effect_point_table(path_coef, paths$direct, "direct"),
    specific = structural_canvas_pls_effect_point_table(path_coef, paths$specific, "specific"),
    total_indirect = structural_canvas_pls_effect_point_table(path_coef, paths$specific, "total_indirect"),
    total = structural_canvas_pls_effect_point_table(path_coef, total_paths, "total"),
    paths = c(paths, list(total = total_paths))
  )
}

structural_canvas_pls_effect_finiteness_contract <- function(path_coef, effect_paths) {
  path_coef <- as.matrix(path_coef)
  effect_paths <- effect_paths %||% list()
  product_values <- function(paths) {
    paths <- paths %||% list()
    if (!length(paths)) return(numeric(0))
    vapply(paths, function(path) {
      path <- as.character(path %||% character(0))
      if (length(path) < 2L) return(NA_real_)
      edges <- vapply(seq_len(length(path) - 1L), function(index) {
        suppressWarnings(as.numeric(path_coef[path[[index]], path[[index + 1L]]]))
      }, numeric(1))
      if (any(!is.finite(edges))) return(NA_real_)
      prod(edges)
    }, numeric(1))
  }
  aggregate_pairs <- function(paths, values) {
    paths <- paths %||% list()
    if (!length(paths)) return(numeric(0))
    pair_keys <- vapply(paths, function(path) {
      paste(path[[1L]], path[[length(path)]], sep = "\r")
    }, character(1))
    vapply(sort(unique(pair_keys), method = "radix"), function(key) {
      sum(values[pair_keys == key])
    }, numeric(1))
  }
  direct <- product_values(effect_paths$direct)
  specific <- product_values(effect_paths$specific)
  total_path_products <- product_values(effect_paths$total)
  values <- c(
    direct = direct,
    specific = specific,
    total_indirect = aggregate_pairs(effect_paths$specific, specific),
    total = aggregate_pairs(effect_paths$total, total_path_products)
  )
  valid <- all(is.finite(values))
  list(
    valid = isTRUE(valid),
    reason = if (isTRUE(valid)) "" else "invalid_statistics:effects",
    values = values
  )
}

structural_canvas_pls_effect_draw_products <- function(paths, boot_paths) {
  boot_paths <- as.array(boot_paths)
  paths <- paths %||% list()
  draw_count <- if (length(dim(boot_paths)) == 3L) dim(boot_paths)[[3L]] else 0L
  if (!length(paths)) return(matrix(numeric(0), 0L, draw_count))
  output <- matrix(NA_real_, length(paths), draw_count)
  for (path_index in seq_along(paths)) {
    path <- paths[[path_index]]
    effect <- rep(1, draw_count)
    for (edge_index in seq_len(length(path) - 1L)) {
      effect <- effect * as.numeric(boot_paths[path[[edge_index]], path[[edge_index + 1L]], ])
    }
    output[path_index, ] <- effect
  }
  rownames(output) <- vapply(paths, structural_canvas_pls_effect_path_label, character(1))
  colnames(output) <- dimnames(boot_paths)[[3L]] %||% as.character(seq_len(draw_count))
  output
}

structural_canvas_pls_effect_bootstrap_p <- function(draws) {
  draws <- as.numeric(draws)
  draws <- draws[is.finite(draws)]
  if (!length(draws)) return(NA_real_)
  lower_tail <- sum(draws <= 0)
  upper_tail <- sum(draws >= 0)
  min(1, 2 * (min(lower_tail, upper_tail) + 1) / (length(draws) + 1))
}

structural_canvas_pls_moderation_bootstrap_tables <- function(
  path_coef, boot_paths, definitions, requested_nboot, estimator = "PLS"
) {
  points <- structural_canvas_pls_moderation_point_tables(path_coef, definitions, estimator)
  valid_n <- if (length(dim(boot_paths)) == 3L) dim(boot_paths)[[3L]] else 0L
  requested_nboot <- suppressWarnings(as.integer(requested_nboot %||% 0L))
  validity <- structural_canvas_pls_bootstrap_validity(valid_n, requested_nboot)
  draw_names <- if (valid_n) dimnames(boot_paths)[[3L]] %||% as.character(seq_len(valid_n)) else character(0)

  effect_draws <- matrix(
    numeric(nrow(points$effects) * valid_n), nrow = nrow(points$effects), ncol = valid_n,
    dimnames = list(
      if (nrow(points$effects)) paste(points$effects$Interaction, points$effects$Outcome, sep = " -> ") else character(0),
      draw_names
    )
  )
  if (nrow(points$effects) && valid_n) {
    for (index in seq_len(nrow(points$effects))) {
      effect_draws[index, ] <- as.numeric(boot_paths[
        points$effects$Interaction[[index]], points$effects$Outcome[[index]],
      ])
    }
  }

  slope_draws <- matrix(
    numeric(nrow(points$simple_slopes) * valid_n), nrow = nrow(points$simple_slopes), ncol = valid_n,
    dimnames = list(
      if (nrow(points$simple_slopes)) paste(
        points$simple_slopes$Predictor, points$simple_slopes$Moderator,
        points$simple_slopes$Outcome,
        points$simple_slopes[["Moderator level"]], sep = " | "
      ) else character(0),
      draw_names
    )
  )
  if (nrow(points$simple_slopes) && valid_n) {
    for (index in seq_len(nrow(points$simple_slopes))) {
      direct_draw <- as.numeric(boot_paths[
        points$simple_slopes$Predictor[[index]], points$simple_slopes$Outcome[[index]],
      ])
      interaction_draw <- as.numeric(boot_paths[
        points$simple_slopes$Interaction[[index]], points$simple_slopes$Outcome[[index]],
      ])
      slope_draws[index, ] <- direct_draw +
        points$simple_slopes[["Moderator value"]][[index]] * interaction_draw
    }
  }
  if ((length(effect_draws) && any(!is.finite(effect_draws))) ||
      (length(slope_draws) && any(!is.finite(slope_draws)))) {
    stop("An accepted PLS bootstrap draw produced a non-finite moderation statistic.", call. = FALSE)
  }

  append_inference <- function(table, draws, point_column) {
    table <- as.data.frame(table, stringsAsFactors = FALSE, check.names = FALSE)
    row_count <- nrow(table)
    if (!row_count) {
      table$`Bootstrap mean` <- numeric(0)
      table$`Bootstrap SE` <- numeric(0)
      table$`95% CI lower` <- numeric(0)
      table$`95% CI upper` <- numeric(0)
      table$p <- numeric(0)
      table$`BH-adjusted p` <- numeric(0)
      table$`Valid replicates` <- integer(0)
      table$`Requested replicates` <- integer(0)
      table$`Valid ratio` <- numeric(0)
      table$`Inference available` <- logical(0)
      table$`Bootstrap Status` <- character(0)
      table$`Inference Source` <- character(0)
      return(table)
    }
    bootstrap_mean <- if (valid_n) rowMeans(draws) else rep(NA_real_, row_count)
    bootstrap_se <- if (valid_n >= 2L) apply(draws, 1L, stats::sd) else rep(NA_real_, row_count)
    intervals <- if (valid_n >= 2L) {
      t(apply(draws, 1L, stats::quantile, probs = c(.025, .975), names = FALSE, type = 7))
    } else {
      matrix(NA_real_, row_count, 2L)
    }
    p_values <- if (valid_n >= 2L) {
      apply(draws, 1L, structural_canvas_pls_effect_bootstrap_p)
    } else rep(NA_real_, row_count)
    if (!isTRUE(validity$adequate)) {
      bootstrap_se[] <- NA_real_
      intervals[,] <- NA_real_
      p_values[] <- NA_real_
    }
    table$`Bootstrap mean` <- bootstrap_mean
    table$`Bootstrap SE` <- bootstrap_se
    table$`95% CI lower` <- intervals[, 1L]
    table$`95% CI upper` <- intervals[, 2L]
    table$p <- p_values
    table$`BH-adjusted p` <- if (any(is.finite(p_values))) {
      adjusted <- rep(NA_real_, length(p_values))
      finite <- which(is.finite(p_values))
      adjusted[finite] <- stats::p.adjust(p_values[finite], method = "BH")
      adjusted
    } else rep(NA_real_, length(p_values))
    table$`Valid replicates` <- rep.int(valid_n, row_count)
    table$`Requested replicates` <- rep.int(requested_nboot, row_count)
    table$`Valid ratio` <- rep(validity$ratio, row_count)
    table$`Inference available` <- rep(isTRUE(validity$adequate), row_count)
    table$`Bootstrap Status` <- rep(as.character(validity$status), row_count)
    table$`Inference Source` <- rep(
      if (isTRUE(validity$adequate)) {
        "Whole-draw PLS path bootstrap; percentile 95% CI; plus-one two-sided empirical sign p"
      } else {
        "Point estimate retained; bootstrap inference suppressed by the 80% whole-draw validity gate"
      },
      row_count
    )
    attr(table, "point_column") <- point_column
    table
  }
  list(
    effects = append_inference(points$effects, effect_draws, "Estimate"),
    simple_slopes = append_inference(points$simple_slopes, slope_draws, "Simple slope"),
    draws = list(interaction = effect_draws, simple_slopes = slope_draws),
    validity = validity
  )
}

structural_canvas_pls_effect_bootstrap_table <- function(point_table, draws, valid_n, requested_n) {
  if (!is.data.frame(point_table) || !nrow(point_table)) {
    empty <- point_table
    empty$`Bootstrap Mean` <- numeric(0)
    empty$`Bootstrap SD` <- numeric(0)
    empty$`T Stat.` <- numeric(0)
    empty$`2.5% CI` <- numeric(0)
    empty$`97.5% CI` <- numeric(0)
    empty$`Bootstrap P Val` <- numeric(0)
    return(empty[, c(
      "Estimand Key", "Path", "Predictor", "Outcome", "Mediators", "Original Est.",
      "Bootstrap Mean", "Bootstrap SD", "T Stat.", "2.5% CI", "97.5% CI",
      "Bootstrap P Val", "Bootstrap Status", "Inference Source", "Valid N", "Requested N"
    ), drop = FALSE])
  }
  draws <- as.matrix(draws)
  if (nrow(draws) != nrow(point_table)) stop("PLS effect bootstrap rows do not match the point-estimate contract.", call. = FALSE)
  original <- suppressWarnings(as.numeric(point_table[["Original Est."]]))
  if (any(!is.finite(original))) {
    stop("PLS fitted effect point estimates contain non-finite values.", call. = FALSE)
  }
  if (length(draws) && any(!is.finite(draws))) {
    stop("An accepted PLS bootstrap draw produced a non-finite path effect.", call. = FALSE)
  }
  if (ncol(draws) < 2L) {
    output <- point_table
    output$`Bootstrap Mean` <- if (ncol(draws) == 1L) as.numeric(draws[, 1L]) else rep(NA_real_, nrow(output))
    output$`Bootstrap SD` <- NA_real_
    output$`T Stat.` <- NA_real_
    output$`2.5% CI` <- NA_real_
    output$`97.5% CI` <- NA_real_
    output$`Bootstrap P Val` <- NA_real_
    validity <- structural_canvas_pls_bootstrap_validity(valid_n, requested_n)
    output$`Bootstrap Status` <- rep(validity$status, nrow(output))
    output$`Inference Source` <- rep(
      "Point estimate retained; fewer than two valid bootstrap draws",
      nrow(output)
    )
    output$`Valid N` <- rep.int(as.integer(valid_n), nrow(output))
    output$`Requested N` <- rep.int(as.integer(requested_n), nrow(output))
    return(output[, c(
      "Estimand Key", "Path", "Predictor", "Outcome", "Mediators", "Original Est.",
      "Bootstrap Mean", "Bootstrap SD", "T Stat.", "2.5% CI", "97.5% CI",
      "Bootstrap P Val", "Bootstrap Status", "Inference Source", "Valid N", "Requested N"
    ), drop = FALSE])
  }
  standard_errors <- apply(draws, 1L, stats::sd)
  t_statistic <- ifelse(is.finite(standard_errors) & standard_errors > sqrt(.Machine$double.eps), original / standard_errors, NA_real_)
  intervals <- t(apply(draws, 1L, stats::quantile, probs = c(.025, .975), names = FALSE, type = 7))
  output <- point_table
  output$`Bootstrap Mean` <- rowMeans(draws)
  output$`Bootstrap SD` <- standard_errors
  output$`T Stat.` <- t_statistic
  output$`2.5% CI` <- intervals[, 1L]
  output$`97.5% CI` <- intervals[, 2L]
  output$`Bootstrap P Val` <- apply(draws, 1L, structural_canvas_pls_effect_bootstrap_p)
  validity <- structural_canvas_pls_bootstrap_validity(valid_n, requested_n)
  output$`Bootstrap Status` <- rep(validity$status, nrow(output))
  output$`Inference Source` <- rep(
    if (isTRUE(validity$adequate)) {
      "StatEdu fitted-path product bootstrap; percentile 95% CI; plus-one two-sided empirical sign p"
    } else {
      "Point estimate retained; bootstrap inference suppressed by the whole-draw validity gate"
    },
    nrow(output)
  )
  output$`Valid N` <- rep.int(as.integer(valid_n), nrow(output))
  output$`Requested N` <- rep.int(as.integer(requested_n), nrow(output))
  output[, c(
    "Estimand Key", "Path", "Predictor", "Outcome", "Mediators", "Original Est.",
    "Bootstrap Mean", "Bootstrap SD", "T Stat.", "2.5% CI", "97.5% CI",
    "Bootstrap P Val", "Bootstrap Status", "Inference Source", "Valid N", "Requested N"
  ), drop = FALSE]
}

structural_canvas_pls_effect_bootstrap_tables <- function(path_coef, boot_paths, requested_nboot, structural_model = NULL) {
  point <- structural_canvas_pls_effect_point_tables(path_coef, structural_model)
  valid_n <- if (length(dim(boot_paths)) == 3L) dim(boot_paths)[[3L]] else 0L
  direct_draws <- structural_canvas_pls_effect_draw_products(point$paths$direct, boot_paths)
  specific_draws <- structural_canvas_pls_effect_draw_products(point$paths$specific, boot_paths)
  total_draws_by_path <- structural_canvas_pls_effect_draw_products(point$paths$total, boot_paths)
  aggregate_draws <- function(paths, draws, point_table) {
    if (!nrow(point_table)) return(matrix(numeric(0), 0L, valid_n))
    pair_keys <- vapply(paths, function(path) paste(path[[1L]], path[[length(path)]], sep = "\r"), character(1))
    output <- matrix(0, nrow(point_table), valid_n, dimnames = list(rownames(point_table), colnames(draws)))
    for (index in seq_len(nrow(point_table))) {
      key <- paste(point_table$Predictor[[index]], point_table$Outcome[[index]], sep = "\r")
      output[index, ] <- colSums(draws[pair_keys == key, , drop = FALSE])
    }
    output
  }
  total_indirect_draws <- aggregate_draws(point$paths$specific, specific_draws, point$total_indirect)
  total_draws <- aggregate_draws(point$paths$total, total_draws_by_path, point$total)
  list(
    direct = structural_canvas_pls_effect_bootstrap_table(point$direct, direct_draws, valid_n, requested_nboot),
    specific = structural_canvas_pls_effect_bootstrap_table(point$specific, specific_draws, valid_n, requested_nboot),
    total_indirect = structural_canvas_pls_effect_bootstrap_table(point$total_indirect, total_indirect_draws, valid_n, requested_nboot),
    total = structural_canvas_pls_effect_bootstrap_table(point$total, total_draws, valid_n, requested_nboot),
    point = point,
    draws = list(direct = direct_draws, specific = specific_draws, total_indirect = total_indirect_draws, total = total_draws)
  )
}

structural_canvas_pls_bootstrap_contract_metadata <- function(summary, validity, failure_reasons, valid_positions = integer(0), seed = NA_integer_) {
  failure_reasons <- as.character(failure_reasons %||% character(0))
  summary$nboot <- validity$valid
  summary$requested_nboot <- validity$requested
  summary$valid_ratio <- validity$ratio
  summary$minimum_valid_ratio <- validity$minimum_ratio
  summary$minimum_valid_n <- validity$minimum_valid
  summary$inference_available <- isTRUE(validity$adequate)
  summary$bootstrap_status <- validity$status
  summary$timeout_failures <- sum(failure_reasons == "timeout")
  summary$estimation_failures <- sum(failure_reasons == "estimation")
  summary$nonconvergence_failures <- sum(failure_reasons == "nonconvergence")
  summary$inadmissible_failures <- sum(failure_reasons == "inadmissible")
  summary$invalid_statistic_failures <- sum(startsWith(failure_reasons, "invalid_statistics") |
    startsWith(failure_reasons, "nonfinite_statistics") |
    startsWith(failure_reasons, "component_"))
  summary$execution_failures <- sum(failure_reasons == "execution")
  summary$canceled_failures <- sum(failure_reasons == "canceled")
  summary$failure_counts <- list(
    timeout = summary$timeout_failures,
    estimation = summary$estimation_failures,
    nonconvergence = summary$nonconvergence_failures,
    inadmissible = summary$inadmissible_failures,
    invalid_statistics = summary$invalid_statistic_failures,
    execution = summary$execution_failures,
    canceled = summary$canceled_failures
  )
  summary$validity_contract <- paste(
    "PLS outer-weight iteration converged; PLS/PLSc local structural equations and reported statistics admissible;",
    "a globally non-positive-definite PLSc disattenuated correlation matrix may be retained for a bootstrap resample only when every local equation remains solvable and all downstream checks pass;",
    "all path/loading/weight/HTMT statistics finite with matching dimensions and names;",
    "all direct, specific-indirect, total-indirect, and total effects finite before whole-draw acceptance"
  )
  summary$valid_positions <- as.integer(valid_positions)
  summary$seed <- suppressWarnings(as.integer(seed))
  summary$rng <- "L'Ecuyer-CMRG independent stream per requested position"
  summary$draw_order <- "requested-position order; invalid draws removed without reordering valid draws"
  summary
}

structural_canvas_pls_bootstrap_unavailable_result <- function(
  requested_nboot, seed = default_seed(), status = c("Pending", "Failed", "Canceled"),
  failure_message = ""
) {
  status <- match.arg(status)
  requested_nboot <- suppressWarnings(as.integer(requested_nboot %||% 0L))
  if (!is.finite(requested_nboot) || requested_nboot < 0L) requested_nboot <- 0L
  empty_summary <- list(
    bootstrapped_paths = NULL,
    bootstrapped_weights = NULL,
    bootstrapped_loadings = NULL,
    bootstrapped_HTMT = NULL,
    bootstrapped_total_paths = NULL,
    bootstrapped_total_indirect_paths = NULL,
    bootstrapped_specific_indirect_paths = NULL,
    bootstrapped_moderation_effects = NULL,
    bootstrapped_moderation_simple_slopes = NULL,
    statedu_boot_paths = NULL,
    statedu_moderation_draws = NULL,
    statedu_effect_draws = NULL,
    statedu_effect_registry = NULL
  )
  result <- structural_canvas_pls_bootstrap_contract_metadata(
    empty_summary,
    structural_canvas_pls_bootstrap_validity(0L, requested_nboot),
    character(0), integer(0), seed
  )
  result$bootstrap_status <- status
  result$inference_available <- FALSE
  result$execution_failures <- as.integer(identical(status, "Failed"))
  result$canceled_failures <- as.integer(identical(status, "Canceled"))
  result$failure_counts$execution <- result$execution_failures
  result$failure_counts$canceled <- result$canceled_failures
  failure_message <- as.character(failure_message %||% "")
  result$failure_message <- if (length(failure_message)) trimws(failure_message[[1L]]) else ""
  class(result) <- "summary.boot_seminr_model"
  result
}

structural_canvas_pls_bootstrap_workers <- function(nboot) {
  nboot <- suppressWarnings(as.integer(nboot))
  if (length(nboot) != 1L || !is.finite(nboot) || nboot < 1L) nboot <- 1L
  physical_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  logical_cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
  detected_cores <- if (length(physical_cores) == 1L && is.finite(physical_cores) && physical_cores >= 1L) {
    as.integer(physical_cores)
  } else if (length(logical_cores) == 1L && is.finite(logical_cores) && logical_cores >= 1L) {
    as.integer(logical_cores)
  } else {
    1L
  }
  maximum_workers <- max(1L, min(detected_cores, nboot))
  configured_workers <- suppressWarnings(as.integer(getOption("statedu.pls.bootstrap.workers", NA_integer_)))
  if (length(configured_workers) == 1L && is.finite(configured_workers) && configured_workers >= 1L) {
    return(max(1L, min(configured_workers, maximum_workers)))
  }
  if (nboot < 250L || detected_cores < 3L) return(1L)
  reserved_cores <- max(1L, as.integer(ceiling(detected_cores * 0.25)))
  max(1L, min(8L, detected_cores - reserved_cores, nboot))
}

structural_canvas_run_plsc_bootstrap <- function(seminr_model, nboot = 5000L, seed = default_seed(), progress_file = NULL, apply_plsc = TRUE) {
  nboot <- suppressWarnings(as.integer(nboot %||% 5000L))
  seed <- suppressWarnings(as.integer(seed %||% default_seed()))
  if (!is.finite(nboot) || nboot < 1L) nboot <- 5000L
  if (!is.finite(seed) || seed < 1L) seed <- default_seed()
  d <- seminr_model$rawdata
  measurement_model <- seminr_model$measurement_model
  structural_model <- seminr_model$smMatrix
  inner_weights <- seminr_model$inner_weights
  moderation_definitions <- seminr_model$statedu_moderation_definitions %||% list()
  moderation_bootstrap_contract <- list(
    requested = length(moderation_definitions) > 0L,
    interaction_reestimated_each_draw = length(moderation_definitions) > 0L,
    resampling_unit = "row",
    estimation = "Full seminr measurement, interaction, and structural model re-estimated in each bootstrap draw",
    whole_draw_gate = structural_canvas_pls_bootstrap_min_valid_ratio(),
    multiplicity = "BH adjustment is computed separately for latent-interaction effects and simple-slope probes",
    transient_path_draws = "statedu_boot_paths is an internal transient array and must not be persisted in saved result bundles",
    methods = unique(vapply(moderation_definitions, function(definition) as.character(definition$method %||% "two_stage"), character(1)))
  )
  reference_effect_paths <- structural_canvas_pls_effect_point_tables(
    seminr_model$path_coef, structural_model
  )$paths
  reference_effect_contract <- structural_canvas_pls_effect_finiteness_contract(
    seminr_model$path_coef, reference_effect_paths
  )
  if (!isTRUE(reference_effect_contract$valid)) {
    stop(
      "PLS fitted direct, indirect, or total effects contain non-finite values; bootstrap inference was not run.",
      call. = FALSE
    )
  }
  original_htmt <- seminr:::HTMT(seminr_model)
  reference_components <- list(
    paths = as.matrix(seminr_model$path_coef),
    loadings = as.matrix(seminr_model$outer_loadings),
    weights = as.matrix(seminr_model$outer_weights),
    htmt = as.matrix(original_htmt)
  )
  boot_vec_len <- sum(vapply(reference_components, length, integer(1)))
  required_statistic_mask <- unlist(structural_canvas_pls_bootstrap_required_masks(reference_components), recursive = FALSE, use.names = FALSE)
  workers <- structural_canvas_pls_bootstrap_workers(nboot)
  batch_size <- max(100L, workers * 50L)
  boot_values <- vector("list", nboot)
  align_bootstrap_signs <- function(boot_model, reference_loadings) {
    boot_loadings <- as.matrix(boot_model$outer_loadings)
    reference_loadings <- as.matrix(reference_loadings)
    constructs <- intersect(colnames(boot_loadings), colnames(reference_loadings))
    signs <- stats::setNames(rep(1, length(constructs)), constructs)
    for (construct in constructs) {
      shared <- intersect(rownames(boot_loadings), rownames(reference_loadings))
      current <- suppressWarnings(as.numeric(boot_loadings[shared, construct]))
      reference <- suppressWarnings(as.numeric(reference_loadings[shared, construct]))
      usable <- is.finite(current) & is.finite(reference) & (abs(current) + abs(reference) > 0)
      if (any(usable) && sum(current[usable] * reference[usable]) < 0) signs[[construct]] <- -1
    }
    for (construct in names(signs)[signs < 0]) {
      boot_model$outer_loadings[, construct] <- -boot_model$outer_loadings[, construct]
      if (!is.null(boot_model$outer_weights) && construct %in% colnames(boot_model$outer_weights)) boot_model$outer_weights[, construct] <- -boot_model$outer_weights[, construct]
      if (!is.null(boot_model$construct_scores) && construct %in% colnames(boot_model$construct_scores)) boot_model$construct_scores[, construct] <- -boot_model$construct_scores[, construct]
    }
    path_rows <- intersect(rownames(boot_model$path_coef), names(signs))
    path_cols <- intersect(colnames(boot_model$path_coef), names(signs))
    if (length(path_rows) && length(path_cols)) {
      boot_model$path_coef[path_rows, path_cols] <- boot_model$path_coef[path_rows, path_cols, drop = FALSE] * outer(signs[path_rows], signs[path_cols])
    }
    boot_model
  }
  reference_loadings <- seminr_model$outer_loadings
  common_factor_constructs <- as.character(seminr_model$statedu_common_factor_constructs %||% character(0))
  rng_streams <- structural_canvas_rng_streams(nboot, seed)
  estimate_one <- function(index, d, measurement_model, structural_model, inner_weights, rng_stream, apply_plsc, common_factor_constructs, boot_vec_len, reference_loadings, reference_components, reference_effect_paths) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    assign(".Random.seed", rng_stream, envir = .GlobalEnv)
    tryCatch({
      setTimeLimit(cpu = Inf, elapsed = 60, transient = TRUE)
      suppressWarnings({
        sampled <- d[sample.int(nrow(d), replace = TRUE), , drop = FALSE]
        boot_model <- seminr::estimate_pls(
          data = sampled,
          measurement_model = measurement_model,
          structural_model = structural_model,
          inner_weights = inner_weights,
          missing = seminr::mean_replacement,
          maxIt = structural_canvas_pls_max_iterations(),
          stopCriterion = structural_canvas_pls_stop_criterion(),
          assess_syntax = FALSE
        )
        base_gate <- structural_canvas_pls_bootstrap_draw_gate(boot_model, "PLS")
        if (!isTRUE(base_gate$valid)) {
          failed <- rep(NA_real_, boot_vec_len)
          attr(failed, "failure_reason") <- base_gate$reason
          return(failed)
        }
        if (isTRUE(apply_plsc)) boot_model <- structural_canvas_apply_plsc(
          boot_model, common_factor_constructs, bootstrap_resample = TRUE
        )
        final_gate <- structural_canvas_pls_bootstrap_draw_gate(
          boot_model,
          if (isTRUE(apply_plsc)) "PLSC" else "PLS",
          if (isTRUE(apply_plsc)) common_factor_constructs else character(0)
        )
        if (!isTRUE(final_gate$valid)) {
          failed <- rep(NA_real_, boot_vec_len)
          attr(failed, "failure_reason") <- final_gate$reason
          return(failed)
        }
        boot_model <- align_bootstrap_signs(boot_model, reference_loadings)
        components <- list(
          paths = as.matrix(boot_model$path_coef),
          loadings = as.matrix(boot_model$outer_loadings),
          weights = as.matrix(boot_model$outer_weights),
          htmt = as.matrix(seminr:::HTMT(boot_model))
        )
        contract <- structural_canvas_pls_bootstrap_components_contract(components, reference_components)
        if (!isTRUE(contract$valid)) {
          failed <- rep(NA_real_, boot_vec_len)
          attr(failed, "failure_reason") <- paste0("invalid_statistics:", contract$reason)
          return(failed)
        }
        effect_contract <- structural_canvas_pls_effect_finiteness_contract(
          components$paths, reference_effect_paths
        )
        if (!isTRUE(effect_contract$valid)) {
          failed <- rep(NA_real_, boot_vec_len)
          attr(failed, "failure_reason") <- "invalid_statistics:effects"
          return(failed)
        }
        draw <- unlist(components, recursive = FALSE, use.names = FALSE)
        attr(draw, "retained_nonpositive_definite_plsc") <- isTRUE(
          boot_model$statedu_plsc_nonpositive_definite_resample
        )
        draw
      })
    }, error = function(error) {
      failed <- rep(NA_real_, boot_vec_len)
      message <- conditionMessage(error)
      attr(failed, "failure_reason") <- if (grepl("time limit|elapsed time", message, ignore.case = TRUE)) {
        "timeout"
      } else if (grepl("did not converge|outer-weight iteration", message, ignore.case = TRUE)) {
        "nonconvergence"
      } else if (grepl("inadmissible|PLSc correction failed|positive definite|rho_A", message, ignore.case = TRUE)) {
        "inadmissible"
      } else {
        "estimation"
      }
      failed
    }, finally = {
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    })
  }
  cluster <- if (workers > 1L) parallel::makePSOCKcluster(workers) else NULL
  if (!is.null(cluster)) {
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    worker_estimate_one <- estimate_one
    environment(worker_estimate_one) <- .GlobalEnv
    environment(align_bootstrap_signs) <- .GlobalEnv
    worker_dispatch <- function(index) {
      worker_estimate_one(
        index, d, measurement_model, structural_model, inner_weights,
        rng_streams[[index]], apply_plsc, common_factor_constructs,
        boot_vec_len, reference_loadings, reference_components,
        reference_effect_paths
      )
    }
    environment(worker_dispatch) <- .GlobalEnv
    parallel::clusterExport(
      cluster,
      c(
        "worker_estimate_one", "align_bootstrap_signs", "structural_canvas_apply_plsc",
        "structural_canvas_pls_bootstrap_components_contract", "structural_canvas_pls_bootstrap_required_masks",
        "structural_canvas_pls_effect_finiteness_contract",
        "structural_canvas_pls_bootstrap_draw_gate", "structural_canvas_pls_fit_diagnostics",
        "structural_canvas_pls_r_squared_values", "structural_canvas_pls_positive_definite",
        "structural_canvas_pls_assert_fit", "structural_canvas_pls_max_iterations",
        "structural_canvas_pls_stop_criterion", "%||%", "common_factor_constructs", "d", "measurement_model",
        "structural_model", "inner_weights", "rng_streams", "apply_plsc", "boot_vec_len",
        "reference_loadings", "reference_components", "reference_effect_paths"
      ),
      envir = environment()
    )
  }
  structural_canvas_write_bootstrap_progress(progress_file, 0L, nboot, "resampling", TRUE)
  starts <- seq.int(1L, nboot, by = batch_size)
  for (start in starts) {
    indices <- seq.int(start, min(nboot, start + batch_size - 1L))
    values <- if (is.null(cluster)) {
      lapply(indices, function(index) estimate_one(index, d, measurement_model, structural_model, inner_weights, rng_streams[[index]], apply_plsc, common_factor_constructs, boot_vec_len, reference_loadings, reference_components, reference_effect_paths))
    } else {
      parallel::parLapplyLB(cluster, indices, worker_dispatch)
    }
    boot_values[indices] <- values
    structural_canvas_write_bootstrap_progress(progress_file, max(indices), nboot, "resampling", TRUE)
  }
  structural_canvas_write_bootstrap_progress(progress_file, nboot, nboot, "summarizing", TRUE)
  selection <- structural_canvas_pls_bootstrap_select_draws(boot_values, boot_vec_len, required_statistic_mask)
  retained_nonpositive_definite_plsc <- sum(vapply(boot_values, function(value) {
    isTRUE(attr(value, "retained_nonpositive_definite_plsc"))
  }, logical(1)))
  failure_reasons <- selection$failure_reasons
  valid <- selection$valid
  bootmatrix <- if (length(selection$values)) do.call(cbind, selection$values) else matrix(numeric(0), nrow = boot_vec_len, ncol = 0L)
  valid_n <- ncol(bootmatrix)
  validity <- structural_canvas_pls_bootstrap_validity(valid_n, nboot)
  if (valid_n < 2L) {
    path_rows <- nrow(seminr_model$path_coef)
    path_cols <- ncol(seminr_model$path_coef)
    path_end <- path_rows * path_cols
    valid_position_names <- as.character(which(valid))
    boot_paths <- array(
      bootmatrix[seq_len(path_end), , drop = FALSE],
      dim = c(path_rows, path_cols, valid_n),
      dimnames = list(rownames(seminr_model$path_coef), colnames(seminr_model$path_coef), valid_position_names)
    )
    effect_bootstrap <- structural_canvas_pls_effect_bootstrap_tables(
      seminr_model$path_coef, boot_paths, requested_nboot = nboot,
      structural_model = seminr_model$smMatrix
    )
    moderation_bootstrap <- structural_canvas_pls_moderation_bootstrap_tables(
      seminr_model$path_coef, boot_paths,
      moderation_definitions,
      requested_nboot = nboot,
      estimator = if (isTRUE(apply_plsc)) "PLSC" else "PLS"
    )
    empty_summary <- list(
      bootstrapped_paths = effect_bootstrap$direct,
      bootstrapped_weights = NULL,
      bootstrapped_loadings = NULL,
      bootstrapped_HTMT = NULL,
      bootstrapped_total_paths = effect_bootstrap$total,
      bootstrapped_total_indirect_paths = effect_bootstrap$total_indirect,
      bootstrapped_specific_indirect_paths = effect_bootstrap$specific,
      bootstrapped_moderation_effects = moderation_bootstrap$effects,
      bootstrapped_moderation_simple_slopes = moderation_bootstrap$simple_slopes,
      statedu_boot_paths = boot_paths,
      statedu_moderation_draws = moderation_bootstrap$draws,
      statedu_moderation_bootstrap_contract = moderation_bootstrap_contract,
      statedu_effect_draws = effect_bootstrap$draws,
      statedu_effect_registry = effect_bootstrap$point
    )
    empty_summary <- structural_canvas_pls_bootstrap_contract_metadata(empty_summary, validity, failure_reasons, which(valid), seed)
    empty_summary$retained_nonpositive_definite_plsc_draws <- retained_nonpositive_definite_plsc
    class(empty_summary) <- "summary.boot_seminr_model"
    return(empty_summary)
  }

  path_rows <- nrow(seminr_model$path_coef)
  path_cols <- ncol(seminr_model$path_coef)
  mm_rows <- nrow(seminr_model$outer_loadings)
  mm_cols <- ncol(seminr_model$outer_loadings)

  start <- 1L
  end <- path_rows * path_cols
  valid_position_names <- as.character(which(valid))
  boot_paths <- array(bootmatrix[start:end, , drop = FALSE], dim = c(path_rows, path_cols, valid_n), dimnames = list(rownames(seminr_model$path_coef), colnames(seminr_model$path_coef), valid_position_names))

  start <- end + 1L
  end <- start + (mm_rows * mm_cols) - 1L
  boot_loadings <- array(bootmatrix[start:end, , drop = FALSE], dim = c(mm_rows, mm_cols, valid_n), dimnames = list(rownames(seminr_model$outer_loadings), colnames(seminr_model$outer_loadings), valid_position_names))

  start <- end + 1L
  end <- start + (mm_rows * mm_cols) - 1L
  boot_weights <- array(bootmatrix[start:end, , drop = FALSE], dim = c(mm_rows, mm_cols, valid_n), dimnames = list(rownames(seminr_model$outer_weights), colnames(seminr_model$outer_weights), valid_position_names))

  htmt_rows <- nrow(original_htmt)
  htmt_cols <- ncol(original_htmt)
  start <- end + 1L
  end <- start + (htmt_rows * htmt_cols) - 1L
  boot_htmt <- array(bootmatrix[start:end, , drop = FALSE], dim = c(htmt_rows, htmt_cols, valid_n), dimnames = list(rownames(original_htmt), colnames(original_htmt), valid_position_names))

  summarize_component <- function(label, expression) {
    tryCatch(
      force(expression),
      error = function(error) {
        stop(
          paste0("PLS bootstrap failed while summarizing ", label, ": ", conditionMessage(error)),
          call. = FALSE
        )
      }
    )
  }
  effect_bootstrap <- summarize_component("structural effects", {
    structural_canvas_pls_effect_bootstrap_tables(
      seminr_model$path_coef, boot_paths, requested_nboot = nboot,
      structural_model = seminr_model$smMatrix
    )
  })
  moderation_bootstrap <- summarize_component("latent-moderation effects", {
    structural_canvas_pls_moderation_bootstrap_tables(
      seminr_model$path_coef, boot_paths,
      moderation_definitions,
      requested_nboot = nboot,
      estimator = if (isTRUE(apply_plsc)) "PLSC" else "PLS"
    )
  })
  parsed_weights <- summarize_component("outer weights", {
    seminr:::parse_boot_array(seminr_model$outer_weights, boot_weights, alpha = .05)
  })
  parsed_loadings <- summarize_component("outer loadings", {
    seminr:::parse_boot_array(seminr_model$outer_loadings, boot_loadings, alpha = .05)
  })
  parsed_htmt <- summarize_component("HTMT", {
    seminr:::parse_boot_array_htmt(original_htmt, boot_htmt, alpha = .05)
  })
  boot_summary <- list(
    bootstrapped_paths = effect_bootstrap$direct,
    bootstrapped_weights = parsed_weights,
    bootstrapped_loadings = parsed_loadings,
    bootstrapped_HTMT = parsed_htmt,
    bootstrapped_total_paths = effect_bootstrap$total,
    bootstrapped_total_indirect_paths = effect_bootstrap$total_indirect,
    bootstrapped_specific_indirect_paths = effect_bootstrap$specific,
    bootstrapped_moderation_effects = moderation_bootstrap$effects,
    bootstrapped_moderation_simple_slopes = moderation_bootstrap$simple_slopes,
    statedu_boot_paths = boot_paths,
    statedu_moderation_draws = moderation_bootstrap$draws,
    statedu_moderation_bootstrap_contract = moderation_bootstrap_contract,
    statedu_effect_draws = effect_bootstrap$draws,
    statedu_effect_registry = effect_bootstrap$point
  )
  boot_summary <- summarize_component("contract metadata", {
    structural_canvas_pls_bootstrap_contract_metadata(
      boot_summary, validity, failure_reasons, which(valid), seed
    )
  })
  boot_summary$retained_nonpositive_definite_plsc_draws <- retained_nonpositive_definite_plsc
  if (!isTRUE(validity$adequate)) {
    inferential_tables <- c(
      "bootstrapped_paths", "bootstrapped_weights", "bootstrapped_loadings",
      "bootstrapped_HTMT", "bootstrapped_total_paths", "bootstrapped_total_indirect_paths",
      "bootstrapped_specific_indirect_paths", "bootstrapped_moderation_effects",
      "bootstrapped_moderation_simple_slopes"
    )
    for (name in inferential_tables) {
      boot_summary[[name]] <- summarize_component(paste0("suppressed inference table '", name, "'"), {
        structural_canvas_pls_bootstrap_suppress_inference(boot_summary[[name]])
      })
    }
  }
  class(boot_summary) <- "summary.boot_seminr_model"
  boot_summary
}

structural_canvas_run_pls_bootstrap <- function(analysis_type, pls_bootstrap, result, pls_seed) {
  pls_bootstrap <- suppressWarnings(as.integer(pls_bootstrap %||% 0L))
  if (!identical(analysis_type, "plssem") || pls_bootstrap <= 0L) return(NULL)
  if (is.null(result$fit) || !inherits(result$fit, "pls_model")) return(NULL)
  use_plsc <- identical(toupper(as.character(result$estimator %||% "PLS")), "PLSC")
  structural_canvas_with_progress(message = "Estimating PLS-SEM bootstrap intervals", value = 0, {
    structural_canvas_set_progress(
      value = .05,
      detail = paste0(pls_bootstrap, " seminr bootstrap resamples", if (use_plsc) " with PLSc correction" else "")
    )
    boot <- structural_canvas_run_plsc_bootstrap(
      result$fit, pls_bootstrap, pls_seed, apply_plsc = use_plsc
    )
    structural_canvas_set_progress(
      value = .90,
      detail = "Preparing PLS-SEM bootstrap summaries"
    )
    if (is.null(boot)) return(NULL)
    value <- if (inherits(boot, "summary.boot_seminr_model")) boot else summary(boot)
    structural_canvas_set_progress(value = 1, detail = "PLS-SEM bootstrap complete")
    value
  })
}

structural_canvas_pls_predict_mean_matrix <- function(summaries, field) {
  matrices <- lapply(summaries, function(value) as.matrix(value[[field]] %||% matrix(numeric(0), 0L, 0L)))
  matrices <- Filter(function(value) length(value) && nrow(value) && ncol(value), matrices)
  if (!length(matrices)) return(matrix(numeric(0), 0L, 0L))
  reference <- matrices[[1L]]
  compatible <- vapply(matrices, function(value) identical(dim(value), dim(reference)) && identical(dimnames(value), dimnames(reference)), logical(1))
  matrices <- matrices[compatible]
  values <- simplify2array(matrices)
  if (length(matrices) == 1L) return(reference)
  apply(values, c(1L, 2L), mean, na.rm = TRUE)
}

structural_canvas_pls_predict_fold_data <- function(training_data, testing_data, variables, missing_value = NA) {
  variables <- unique(as.character(variables %||% character(0)))
  if (!length(variables)) stop("PLSpredict requires at least one measurement variable.", call. = FALSE)
  missing_variables <- setdiff(variables, intersect(names(training_data), names(testing_data)))
  if (length(missing_variables)) {
    stop(paste0("PLSpredict fold data are missing measurement variables: ", paste(missing_variables, collapse = ", "), "."), call. = FALSE)
  }
  training <- as.data.frame(training_data[, variables, drop = FALSE], check.names = FALSE)
  testing <- as.data.frame(testing_data[, variables, drop = FALSE], check.names = FALSE)
  for (variable in variables) {
    if (!is.numeric(training[[variable]]) || !is.numeric(testing[[variable]])) {
      stop(paste0("PLSpredict requires numeric measurement data; '", variable, "' is not numeric."), call. = FALSE)
    }
    if (length(missing_value) == 1L && !is.na(missing_value)) {
      training[[variable]][training[[variable]] == missing_value] <- NA_real_
      testing[[variable]][testing[[variable]] == missing_value] <- NA_real_
    }
    if (any(is.infinite(training[[variable]])) || any(is.infinite(testing[[variable]]))) {
      stop(paste0("PLSpredict fold contains infinite values for '", variable, "'."), call. = FALSE)
    }
  }
  training_missing <- vapply(training, function(value) sum(is.na(value)), integer(1))
  testing_missing <- vapply(testing, function(value) sum(is.na(value)), integer(1))
  training_means <- vapply(training, function(value) {
    available <- value[!is.na(value)]
    if (!length(available)) return(NA_real_)
    mean(available)
  }, numeric(1))
  invalid_means <- names(training_means)[!is.finite(training_means)]
  if (length(invalid_means)) {
    stop(paste0(
      "PLSpredict fold has no finite training observations for: ",
      paste(invalid_means, collapse = ", "), "."
    ), call. = FALSE)
  }
  impute <- function(value, means) {
    for (variable in names(means)) {
      missing <- is.na(value[[variable]])
      if (any(missing)) value[[variable]][missing] <- means[[variable]]
    }
    value
  }
  training <- impute(training, training_means)
  testing <- impute(testing, training_means)
  list(
    training = training, testing = testing, training_means = training_means,
    training_missing = training_missing, testing_missing = testing_missing
  )
}

structural_canvas_pls_predict_estimate_fold <- function(model, training_data, estimator = "PLS", common_factor_constructs = character(0)) {
  estimator <- toupper(as.character(estimator %||% "PLS"))
  fit <- suppressMessages(seminr::estimate_pls(
    data = training_data,
    measurement_model = model$measurement_model,
    structural_model = model$structural_model %||% model$smMatrix,
    inner_weights = model$inner_weights,
    missing = seminr::mean_replacement,
    missing_value = NA,
    maxIt = structural_canvas_pls_max_iterations(),
    stopCriterion = structural_canvas_pls_stop_criterion(),
    assess_syntax = FALSE
  ))
  fit$statedu_common_factor_constructs <- unique(as.character(common_factor_constructs %||% character(0)))
  structural_canvas_pls_assert_fit(fit, "PLS", stage = "PLSpredict training-fold base solution")
  if (identical(estimator, "PLSC")) {
    fit <- structural_canvas_apply_plsc(fit, common_factor_constructs)
  }
  fit$statedu_fit_diagnostics <- structural_canvas_pls_assert_fit(
    fit, estimator, if (identical(estimator, "PLSC")) common_factor_constructs else character(0),
    stage = "PLSpredict training-fold solution"
  )
  fit
}

structural_canvas_pls_predict_standardized_scores <- function(fit, data) {
  variables <- as.character(fit$mmVariables %||% character(0))
  means <- suppressWarnings(as.numeric(fit$meanData[variables]))
  standard_deviations <- suppressWarnings(as.numeric(fit$sdData[variables]))
  if (!length(variables) || length(means) != length(variables) || length(standard_deviations) != length(variables) ||
      any(!is.finite(means)) || any(!is.finite(standard_deviations) | standard_deviations <= 0)) {
    stop("PLSpredict training-fold standardization parameters are unavailable or degenerate.", call. = FALSE)
  }
  matrix_data <- as.matrix(data[, variables, drop = FALSE])
  scaled <- sweep(sweep(matrix_data, 2L, means, FUN = "-"), 2L, standard_deviations, FUN = "/")
  weights <- as.matrix(fit$outer_weights[variables, , drop = FALSE])
  if (any(!is.finite(scaled)) || any(!is.finite(weights))) {
    stop("PLSpredict fold contains non-finite standardized data or outer weights.", call. = FALSE)
  }
  scores <- scaled %*% weights
  if (any(!is.finite(scores))) stop("PLSpredict fold construct scores are non-finite.", call. = FALSE)
  scores
}

structural_canvas_pls_predict_from_fold <- function(fit, data) {
  variables <- as.character(fit$mmVariables %||% character(0))
  no_interaction_variables <- variables[!seminr:::is_interaction(variables)]
  actual_scores <- structural_canvas_pls_predict_standardized_scores(fit, data)
  predicted_scores <- seminr::predict_DA(fit$smMatrix, fit$path_coef, actual_scores)
  loadings <- as.matrix(fit$outer_loadings[no_interaction_variables, , drop = FALSE])
  predicted_standardized_items <- predicted_scores %*% t(loadings)
  item_means <- suppressWarnings(as.numeric(fit$meanData[no_interaction_variables]))
  item_sd <- suppressWarnings(as.numeric(fit$sdData[no_interaction_variables]))
  predicted_items <- sweep(sweep(predicted_standardized_items, 2L, item_sd, FUN = "*"), 2L, item_means, FUN = "+")
  colnames(predicted_items) <- no_interaction_variables
  if (any(!is.finite(predicted_scores)) || any(!is.finite(predicted_items))) {
    stop("PLSpredict fold produced non-finite PLS indicator predictions.", call. = FALSE)
  }
  list(actual_scores = actual_scores, predicted_scores = predicted_scores, predicted_items = predicted_items)
}

structural_canvas_pls_predict_lm_fold <- function(fit, training_data, testing_data) {
  endogenous_constructs <- seminr:::all_endogenous(fit$smMatrix)
  endogenous_items <- unlist(lapply(endogenous_constructs, function(construct) {
    seminr:::construct_items(fit$mmMatrix, construct)
  }), use.names = FALSE)
  training_predictions <- matrix(
    NA_real_, nrow(training_data), length(endogenous_items),
    dimnames = list(rownames(training_data), endogenous_items)
  )
  testing_predictions <- matrix(
    NA_real_, nrow(testing_data), length(endogenous_items),
    dimnames = list(rownames(testing_data), endogenous_items)
  )
  for (construct in endogenous_constructs) {
    outcomes <- seminr:::construct_items(fit$mmMatrix, construct)
    antecedents <- seminr:::construct_antecedents(fit$smMatrix, construct)
    predictors <- unique(unlist(lapply(antecedents, function(antecedent) {
      seminr:::construct_items(fit$mmMatrix, antecedent)
    }), use.names = FALSE))
    if (!length(predictors)) {
      stop(paste0("PLSpredict LM benchmark has no direct-antecedent indicators for '", construct, "'."), call. = FALSE)
    }
    design_training <- cbind(`(Intercept)` = 1, as.matrix(training_data[, predictors, drop = FALSE]))
    design_testing <- cbind(`(Intercept)` = 1, as.matrix(testing_data[, predictors, drop = FALSE]))
    for (outcome in outcomes) {
      lm_fit <- stats::lm.fit(design_training, training_data[[outcome]])
      coefficients <- as.numeric(lm_fit$coefficients)
      if (length(coefficients) != ncol(design_training) || any(!is.finite(coefficients))) {
        stop(paste0("PLSpredict LM benchmark is rank deficient for indicator '", outcome, "'."), call. = FALSE)
      }
      training_predictions[, outcome] <- as.vector(design_training %*% coefficients)
      testing_predictions[, outcome] <- as.vector(design_testing %*% coefficients)
    }
  }
  if (any(!is.finite(training_predictions)) || any(!is.finite(testing_predictions))) {
    stop("PLSpredict fold produced non-finite LM benchmark predictions.", call. = FALSE)
  }
  list(training = training_predictions, testing = testing_predictions)
}

structural_canvas_pls_predict_summary <- function(prediction) {
  residual_metrics <- function(residuals, label) {
    residuals <- as.matrix(residuals)
    if (!ncol(residuals)) return(matrix(numeric(0), 2L, 0L, dimnames = list(c("RMSE", "MAE"), character(0))))
    values <- vapply(seq_len(ncol(residuals)), function(index) {
      available <- residuals[, index]
      available <- available[is.finite(available)]
      if (!length(available)) {
        stop(paste0("PLSpredict has no observed outcome values for ", label, " indicator '", colnames(residuals)[[index]], "'."), call. = FALSE)
      }
      c(RMSE = sqrt(mean(available^2)), MAE = mean(abs(available)))
    }, numeric(2))
    rownames(values) <- c("RMSE", "MAE")
    colnames(values) <- colnames(residuals)
    class(values) <- append(class(values), "table_output")
    values
  }
  model <- prediction$model
  construct_error <- do.call(cbind, lapply(seminr:::all_endogenous(model$smMatrix), function(construct) {
    actual <- as.numeric(prediction$composites$actuals_star[, construct])
    in_sample <- as.numeric(prediction$composites$composite_in_sample[, construct])
    out_of_sample <- as.numeric(prediction$composites$composite_out_of_sample[, construct])
    in_valid <- is.finite(actual) & is.finite(in_sample)
    out_valid <- is.finite(actual) & is.finite(out_of_sample)
    if (!any(in_valid) || !any(out_valid)) {
      stop(paste0("PLSpredict has no observed construct-score outcomes for '", construct, "'."), call. = FALSE)
    }
    is_mse <- mean((actual[in_valid] - in_sample[in_valid])^2)
    c(
      IS_MSE = is_mse,
      IS_MAE = mean(abs(actual[in_valid] - in_sample[in_valid])),
      OOS_MSE = mean((actual[out_valid] - out_of_sample[out_valid])^2),
      OOS_MAE = mean(abs(actual[out_valid] - out_of_sample[out_valid])),
      overfit = if (is.finite(is_mse) && is_mse > 0) {
        (mean((actual[out_valid] - out_of_sample[out_valid])^2) - is_mse) / is_mse
      } else NA_real_
    )
  }))
  if (length(construct_error)) colnames(construct_error) <- seminr:::all_endogenous(model$smMatrix)
  value <- list(
    PLS_in_sample = residual_metrics(prediction$items$PLS_in_sample_residuals, "PLS in-sample"),
    PLS_out_of_sample = residual_metrics(prediction$items$PLS_out_of_sample_residuals, "PLS out-of-sample"),
    LM_in_sample = residual_metrics(prediction$items$lm_in_sample_residuals, "LM in-sample"),
    LM_out_of_sample = residual_metrics(prediction$items$lm_out_of_sample_residuals, "LM out-of-sample"),
    construct_error = construct_error,
    prediction_error = prediction$items$PLS_out_of_sample_residuals
  )
  value$effective_n <- list(
    PLS_in_sample = colSums(is.finite(prediction$items$PLS_in_sample_residuals)),
    PLS_out_of_sample = colSums(is.finite(prediction$items$PLS_out_of_sample_residuals)),
    LM_in_sample = colSums(is.finite(prediction$items$lm_in_sample_residuals)),
    LM_out_of_sample = colSums(is.finite(prediction$items$lm_out_of_sample_residuals)),
    construct = vapply(seminr:::all_endogenous(model$smMatrix), function(construct) {
      sum(is.finite(prediction$composites$actuals_star[, construct]))
    }, integer(1))
  )
  class(value) <- "summary.predict_pls_model"
  value
}

structural_canvas_pls_predict_repetition <- function(model, no_folds, estimator = "PLS", common_factor_constructs = character(0)) {
  if (!is.null(model$hoc)) {
    stop("PLSpredict does not support higher-order construct models in this release.", call. = FALSE)
  }
  if (any(seminr:::is_interaction(as.character(model$mmVariables %||% character(0))))) {
    stop("PLSpredict does not support interaction constructs in this release.", call. = FALSE)
  }
  raw_data <- as.data.frame(model$rawdata %||% model$data, check.names = FALSE)
  variables <- as.character(model$mmVariables %||% character(0))
  raw_data <- raw_data[, variables, drop = FALSE]
  n <- nrow(raw_data)
  no_folds <- suppressWarnings(as.integer(no_folds))
  if (!is.finite(no_folds) || no_folds < 2L || no_folds > n) {
    stop(paste0("PLSpredict folds must be between 2 and the analysis sample size (", n, ")."), call. = FALSE)
  }
  rownames(raw_data) <- as.character(seq_len(n))
  raw_missing <- vapply(raw_data, function(value) {
    missing <- is.na(value)
    missing_value <- model$settings$missing_value %||% NA
    if (length(missing_value) == 1L && !is.na(missing_value)) missing <- missing | value == missing_value
    missing
  }, logical(n))
  raw_missing <- matrix(raw_missing, nrow = n, dimnames = list(rownames(raw_data), variables))
  order <- sample(seq_len(n), n, replace = FALSE)
  fold_order <- cut(seq_len(n), breaks = no_folds, labels = FALSE)
  folds <- integer(n)
  folds[order] <- fold_order
  constructs <- as.character(model$constructs %||% colnames(model$construct_scores))
  no_interaction_variables <- variables[!seminr:::is_interaction(variables)]
  endogenous_constructs <- seminr:::all_endogenous(model$smMatrix)
  endogenous_items <- unique(unlist(lapply(endogenous_constructs, function(construct) {
    seminr:::construct_items(model$mmMatrix, construct)
  }), use.names = FALSE))
  empty <- function(columns) matrix(0, n, length(columns), dimnames = list(rownames(raw_data), columns))
  oos_construct <- empty(constructs)
  oos_items <- empty(no_interaction_variables)
  oos_lm <- empty(endogenous_items)
  oos_actual_scores <- empty(constructs)
  oos_actual_items <- empty(no_interaction_variables)
  is_construct_sum <- empty(constructs)
  is_items_sum <- empty(no_interaction_variables)
  is_lm_sum <- empty(endogenous_items)
  is_actual_items_sum <- empty(no_interaction_variables)
  is_counts <- numeric(n)
  fold_diagnostics <- vector("list", no_folds)
  missing_value <- model$settings$missing_value %||% NA

  for (fold in seq_len(no_folds)) {
    testing_index <- which(folds == fold)
    training_index <- which(folds != fold)
    fold_value <- tryCatch({
      prepared <- structural_canvas_pls_predict_fold_data(
        raw_data[training_index, , drop = FALSE], raw_data[testing_index, , drop = FALSE],
        variables, missing_value
      )
      fit <- structural_canvas_pls_predict_estimate_fold(
        model, prepared$training, estimator, common_factor_constructs
      )
      training_prediction <- structural_canvas_pls_predict_from_fold(fit, prepared$training)
      testing_prediction <- structural_canvas_pls_predict_from_fold(fit, prepared$testing)
      lm_prediction <- structural_canvas_pls_predict_lm_fold(fit, prepared$training, prepared$testing)
      list(
        prepared = prepared, fit = fit, training = training_prediction,
        testing = testing_prediction, lm = lm_prediction
      )
    }, error = function(error) {
      stop(paste0("PLSpredict fold ", fold, " failed closed: ", conditionMessage(error)), call. = FALSE)
    })
    oos_construct[testing_index, ] <- fold_value$testing$predicted_scores[, constructs, drop = FALSE]
    oos_items[testing_index, ] <- fold_value$testing$predicted_items[, no_interaction_variables, drop = FALSE]
    oos_lm[testing_index, ] <- fold_value$lm$testing[, endogenous_items, drop = FALSE]
    oos_actual_scores[testing_index, ] <- fold_value$testing$actual_scores[, constructs, drop = FALSE]
    oos_actual_items[testing_index, ] <- as.matrix(fold_value$prepared$testing[, no_interaction_variables, drop = FALSE])
    is_construct_sum[training_index, ] <- is_construct_sum[training_index, , drop = FALSE] +
      fold_value$training$predicted_scores[, constructs, drop = FALSE]
    is_items_sum[training_index, ] <- is_items_sum[training_index, , drop = FALSE] +
      fold_value$training$predicted_items[, no_interaction_variables, drop = FALSE]
    is_lm_sum[training_index, ] <- is_lm_sum[training_index, , drop = FALSE] +
      fold_value$lm$training[, endogenous_items, drop = FALSE]
    is_actual_items_sum[training_index, ] <- is_actual_items_sum[training_index, , drop = FALSE] +
      as.matrix(fold_value$prepared$training[, no_interaction_variables, drop = FALSE])
    is_counts[training_index] <- is_counts[training_index] + 1
    fold_diagnostics[[fold]] <- list(
      fold = fold, training_n = length(training_index), testing_n = length(testing_index),
      estimator = if (identical(toupper(estimator), "PLSC")) "PLSc" else "PLS",
      converged = isTRUE(fold_value$fit$statedu_fit_diagnostics$converged),
      admissible = isTRUE(fold_value$fit$statedu_fit_diagnostics$admissible),
      training_means = fold_value$prepared$training_means,
      training_imputations = fold_value$prepared$training_missing,
      holdout_imputations = fold_value$prepared$testing_missing
    )
  }
  if (any(is_counts <= 0)) stop("PLSpredict did not produce in-sample predictions for every row.", call. = FALSE)
  divide_rows <- function(value) value / is_counts
  is_construct <- divide_rows(is_construct_sum)
  is_items <- divide_rows(is_items_sum)
  is_lm <- divide_rows(is_lm_sum)
  is_actual_items <- divide_rows(is_actual_items_sum)
  prediction <- list(
    composites = list(
      composite_out_of_sample = oos_construct,
      composite_in_sample = is_construct,
      actuals_star = oos_actual_scores
    ),
    items = list(
      PLS_out_of_sample = oos_items[, endogenous_items, drop = FALSE],
      PLS_in_sample = is_items[, endogenous_items, drop = FALSE],
      lm_out_of_sample = oos_lm,
      lm_in_sample = is_lm,
      item_actuals = oos_actual_items,
      PLS_out_of_sample_residuals = oos_actual_items[, endogenous_items, drop = FALSE] - oos_items[, endogenous_items, drop = FALSE],
      PLS_in_sample_residuals = is_actual_items[, endogenous_items, drop = FALSE] - is_items[, endogenous_items, drop = FALSE],
      lm_out_of_sample_residuals = oos_actual_items[, endogenous_items, drop = FALSE] - oos_lm,
      lm_in_sample_residuals = is_actual_items[, endogenous_items, drop = FALSE] - is_lm
    ),
    model = model,
    statedu_fold_diagnostics = fold_diagnostics
  )
  prediction$items$PLS_out_of_sample_residuals[raw_missing[, endogenous_items, drop = FALSE]] <- NA_real_
  prediction$items$PLS_in_sample_residuals[raw_missing[, endogenous_items, drop = FALSE]] <- NA_real_
  prediction$items$lm_out_of_sample_residuals[raw_missing[, endogenous_items, drop = FALSE]] <- NA_real_
  prediction$items$lm_in_sample_residuals[raw_missing[, endogenous_items, drop = FALSE]] <- NA_real_
  for (construct in constructs) {
    indicators <- seminr:::construct_items(model$mmMatrix, construct)
    unavailable <- rowSums(raw_missing[, indicators, drop = FALSE]) > 0L
    prediction$composites$actuals_star[unavailable, construct] <- NA_real_
  }
  class(prediction) <- "predict_pls_model"
  summary_value <- structural_canvas_pls_predict_summary(prediction)
  attr(summary_value, "statedu_fold_diagnostics") <- fold_diagnostics
  summary_value
}

structural_canvas_start_pls_bootstrap_job <- function(result, nboot, seed = default_seed()) {
  stopifnot(requireNamespace("callr", quietly = TRUE))
  job_dir <- tempfile("statedu-pls-bootstrap-")
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  model_file <- file.path(job_dir, "model.rds")
  result_file <- file.path(job_dir, "result.rds")
  error_file <- file.path(job_dir, "error.txt")
  progress_file <- file.path(job_dir, "progress.rds")
  saveRDS(list(fit = result$fit, estimator = result$estimator), model_file)
  structural_canvas_write_bootstrap_progress(progress_file, 0L, nboot, "starting", TRUE)
  process <- callr::r_bg(
    func = function(model_file, result_file, error_file, progress_file, nboot, seed, source_file) {
      tryCatch({
        `%||%` <- function(x, y) if (is.null(x)) y else x
        default_seed <- function() 24680L
        source(source_file, local = environment(), encoding = "UTF-8")
        model <- readRDS(model_file)
        use_plsc <- identical(toupper(as.character(model$estimator %||% "PLS")), "PLSC")
        boot <- structural_canvas_run_plsc_bootstrap(model$fit, nboot, seed, progress_file, apply_plsc = use_plsc)
        structural_canvas_write_bootstrap_progress(progress_file, nboot, nboot, "summarizing", TRUE)
        value <- if (is.null(boot) || inherits(boot, "summary.boot_seminr_model")) boot else summary(boot)
        saveRDS(value, result_file)
        structural_canvas_write_bootstrap_progress(progress_file, nboot, nboot, "complete", TRUE)
      }, error = function(error) {
        writeLines(conditionMessage(error), error_file, useBytes = TRUE)
        quit(status = 1L, save = "no")
      })
      invisible(TRUE)
    },
    args = list(
      model_file = model_file, result_file = result_file, error_file = error_file, progress_file = progress_file,
      nboot = as.integer(nboot), seed = as.integer(seed),
      source_file = normalizePath("R/setup_custom_model_canvas_structural_pls_engine.R", winslash = "/", mustWork = TRUE)
    ),
    supervise = TRUE
  )
  list(
    process = process, directory = job_dir, result_file = result_file, error_file = error_file,
    progress_file = progress_file, started_at = Sys.time(), nboot = as.integer(nboot),
    estimator = as.character(result$estimator %||% "PLS")
  )
}

structural_canvas_cleanup_pls_bootstrap_job <- function(job) {
  if (is.null(job)) return(invisible(FALSE))
  directory <- as.character(job$directory %||% "")
  if (nzchar(directory) && dir.exists(directory)) unlink(directory, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

structural_canvas_run_pls_predict <- function(analysis_type, pls_predict_folds, pls_predict_reps, result, pls_predict_seed = default_seed()) {
  pls_predict_folds <- suppressWarnings(as.integer(pls_predict_folds %||% 0L))
  pls_predict_reps <- suppressWarnings(as.integer(pls_predict_reps %||% 1L))
  if (!identical(analysis_type, "plssem") || pls_predict_folds <= 0L) return(NULL)
  if (is.null(result$fit) || !inherits(result$fit, "pls_model")) return(NULL)
  if (is.na(pls_predict_reps) || pls_predict_reps < 1L) pls_predict_reps <- 1L
  structural_canvas_with_progress(message = "Estimating PLSpredict cross-validation", value = 0, {
    structural_canvas_set_progress(
      value = .05,
      detail = paste0(pls_predict_folds, "-fold cross-validation")
    )
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_kind <- RNGkind()
    on.exit({
      do.call(RNGkind, as.list(old_kind))
      if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    rng_streams <- structural_canvas_rng_streams(pls_predict_reps, pls_predict_seed)
    estimator <- toupper(as.character(result$estimator %||% "PLS"))
    common_factor_constructs <- unique(as.character(
      result$common_factor_constructs %||% result$fit$statedu_common_factor_constructs %||% character(0)
    ))
    repetition_summaries <- lapply(seq_len(pls_predict_reps), function(index) {
      assign(".Random.seed", rng_streams[[index]], envir = .GlobalEnv)
      # This fold-native path replaces seminr::predict_pls so PLSc uses the
      # same correction and fail-closed gate as the primary analysis and so
      # holdout values never contribute to missing-value preprocessing.
      structural_canvas_pls_predict_repetition(
        model = result$fit,
        no_folds = pls_predict_folds,
        estimator = estimator,
        common_factor_constructs = common_factor_constructs
      )
    })
    summary_value <- repetition_summaries[[1L]]
    for (field in c("PLS_out_of_sample", "LM_out_of_sample", "construct_error")) {
      summary_value[[field]] <- structural_canvas_pls_predict_mean_matrix(repetition_summaries, field)
    }
    structural_canvas_set_progress(value = .90, detail = "Preparing PLSpredict summaries")
    value <- list(
      folds = pls_predict_folds,
      reps = pls_predict_reps,
      seed = as.integer(pls_predict_seed),
      rng = "L'Ecuyer-CMRG independent streams",
      technique = "Direct antecedents",
      estimator = if (identical(estimator, "PLSC")) "PLSc" else "PLS",
      missing_preprocessing = "Training-fold means applied to training and holdout rows",
      summary = summary_value,
      repetition_summaries = repetition_summaries,
      repetition_diagnostics = lapply(repetition_summaries, function(value) {
        attr(value, "statedu_fold_diagnostics") %||% list()
      }),
      effective_n = lapply(repetition_summaries, function(value) value$effective_n %||% list())
    )
    structural_canvas_set_progress(value = 1, detail = "PLSpredict complete")
    value
  })
}
