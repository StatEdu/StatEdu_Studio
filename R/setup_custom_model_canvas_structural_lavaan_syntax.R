# Structural equation canvas lavaan syntax helpers.

structural_canvas_lavaan_safe_label <- function(value, prefix = "statedu") {
  value <- gsub("[^A-Za-z0-9_.]+", "_", as.character(value %||% ""))
  value <- gsub("_+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value) || !grepl("^[A-Za-z]", value)) value <- paste0(prefix, "_", value)
  value
}

structural_canvas_structural_effect_label <- function(prefix, predictor, outcome) {
  paste(
    structural_canvas_lavaan_safe_label(prefix, "statedu"),
    structural_canvas_lavaan_safe_label(predictor, "from"),
    structural_canvas_lavaan_safe_label(outcome, "to"),
    sep = "_"
  )
}

structural_canvas_moderation_product_name <- function(predictor, moderator, indicator, existing_names) {
  base <- structural_canvas_lavaan_safe_label(paste("statedu_pi", predictor, moderator, indicator, sep = "_"), "statedu_pi")
  name <- base
  suffix <- 1L
  while (name %in% existing_names) {
    suffix <- suffix + 1L
    name <- paste0(base, "_", suffix)
  }
  name
}

structural_canvas_latent_indicators <- function(snapshot, edges, latent_id) {
  indicator_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (is.null(from) || is.null(to)) return(FALSE)
    (identical(as.character(from$id %||% ""), as.character(latent_id)) && identical(to$role, "indicator")) ||
      (identical(as.character(to$id %||% ""), as.character(latent_id)) && identical(from$role, "indicator"))
  }, edges)
  unique(vapply(indicator_edges, function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    structural_canvas_name(if (identical(from$role, "indicator")) from else to)
  }, character(1)))
}

structural_canvas_structural_edge_label <- function(edge, index) {
  equality_label <- trimws(as.character(edge$equalityLabel %||% ""))
  parameter_name <- trimws(as.character(edge$parameterName %||% ""))
  label <- if (nzchar(equality_label)) equality_label else parameter_name
  if (nzchar(label) && !grepl("^[A-Za-z][A-Za-z0-9_.]*$", label)) {
    stop(sprintf("Invalid lavaan parameter label '%s'. Use a letter first, followed by letters, numbers, underscores, or periods.", label))
  }
  if (nzchar(label)) return(label)
  fixed_value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
  if (identical(edge$free, FALSE)) {
    if (!is.finite(fixed_value)) stop("A finite fixed value is required for every fixed structural path.")
    return(format(fixed_value, scientific = FALSE, digits = 15, trim = TRUE))
  }
  paste0("statedu_b", as.integer(index))
}

structural_canvas_structural_moderation_terms <- function(snapshot, data, edges, structural_edge_info) {
  moderations <- snapshot$moderations %||% list()
  if (!length(moderations)) return(list(data = data, measurement_lines = character(0), structural_lines = character(0), definitions = list()))
  edge_info_by_id <- stats::setNames(structural_edge_info, vapply(structural_edge_info, function(item) as.character(item$edge$id %||% ""), character(1)))
  existing_names <- names(data)
  measurement_lines <- character(0)
  structural_lines <- character(0)
  definitions <- list()
  for (moderation in moderations) {
    target_edge_id <- as.character(moderation$toEdge %||% "")
    if (!nzchar(target_edge_id) || !target_edge_id %in% names(edge_info_by_id)) next
    source <- structural_canvas_node(snapshot, moderation$from)
    if (is.null(source) || !identical(source$role, "moderator")) next
    moderator <- structural_canvas_name(source)
    if (!moderator %in% names(data)) stop(paste0("The moderator variable is not in the data: ", moderator, "."))
    if (!is.numeric(data[[moderator]])) stop(paste0("Johnson-Neyman SEM moderation currently requires a continuous numeric observed moderator: ", moderator, "."))
    edge_info <- edge_info_by_id[[target_edge_id]]
    predictor_node <- structural_canvas_node(snapshot, edge_info$edge$from)
    predictor_indicators <- structural_canvas_latent_indicators(snapshot, edges, predictor_node$id)
    predictor_indicators <- predictor_indicators[predictor_indicators %in% names(data)]
    if (length(predictor_indicators) < 2L) {
      stop(paste0("Latent SEM moderation requires at least two observed indicators for the moderated predictor: ", edge_info$predictor, "."))
    }
    moderator_mean <- mean(data[[moderator]], na.rm = TRUE)
    moderator_centered <- data[[moderator]] - moderator_mean
    product_names <- character(0)
    for (indicator in predictor_indicators) {
      if (!is.numeric(data[[indicator]])) stop(paste0("Product-indicator SEM moderation requires numeric indicators. Check: ", indicator, "."))
      product_name <- structural_canvas_moderation_product_name(edge_info$predictor, moderator, indicator, existing_names)
      existing_names <- c(existing_names, product_name)
      data[[product_name]] <- (data[[indicator]] - mean(data[[indicator]], na.rm = TRUE)) * moderator_centered
      product_names <- c(product_names, product_name)
    }
    interaction_factor <- structural_canvas_structural_effect_label("statedu_int", edge_info$predictor, moderator)
    moderator_label <- structural_canvas_structural_effect_label("statedu_mod", moderator, edge_info$outcome)
    interaction_label <- structural_canvas_structural_effect_label("statedu_jn", edge_info$predictor, moderator)
    measurement_lines <- c(measurement_lines, paste(interaction_factor, "=~", paste(product_names, collapse = " + ")))
    structural_lines <- c(structural_lines, paste(edge_info$outcome, "~", paste0(moderator_label, "*", moderator), "+", paste0(interaction_label, "*", interaction_factor)))
    definitions[[length(definitions) + 1L]] <- list(
      predictor = edge_info$predictor,
      outcome = edge_info$outcome,
      moderator = moderator,
      direct_label = edge_info$label,
      moderator_label = moderator_label,
      interaction_label = interaction_label,
      interaction_factor = interaction_factor,
      product_indicators = product_names,
      moderator_mean = moderator_mean,
      moderator_min = min(data[[moderator]], na.rm = TRUE),
      moderator_max = max(data[[moderator]], na.rm = TRUE)
    )
  }
  list(data = data, measurement_lines = measurement_lines, structural_lines = structural_lines, definitions = definitions)
}

structural_canvas_structural_edge_term <- function(edge, predictor_name, label) {
  if (identical(edge$free, FALSE)) return(structural_canvas_parameter_term(edge, predictor_name))
  labeled_edge <- edge
  if (!nzchar(trimws(as.character(labeled_edge$equalityLabel %||% ""))) &&
      !nzchar(trimws(as.character(labeled_edge$parameterName %||% "")))) {
    labeled_edge$parameterName <- label
  }
  structural_canvas_parameter_term(labeled_edge, predictor_name)
}

structural_canvas_find_structural_paths <- function(adjacency, from, to, visited = character(0)) {
  from <- as.character(from)
  to <- as.character(to)
  if (identical(from, to)) return(list())
  next_nodes <- adjacency[[from]] %||% character(0)
  paths <- list()
  for (next_node in next_nodes) {
    if (next_node %in% visited) next
    if (identical(next_node, to)) {
      paths[[length(paths) + 1L]] <- c(from, to)
    } else {
      child_paths <- structural_canvas_find_structural_paths(adjacency, next_node, to, c(visited, from))
      paths <- c(paths, lapply(child_paths, function(path) c(from, path)))
    }
  }
  paths
}

structural_canvas_structural_has_cycle <- function(adjacency) {
  visiting <- character(0)
  visited <- character(0)
  visit <- function(node) {
    if (node %in% visiting) return(TRUE)
    if (node %in% visited) return(FALSE)
    visiting <<- c(visiting, node)
    for (next_node in adjacency[[node]] %||% character(0)) {
      if (isTRUE(visit(next_node))) return(TRUE)
    }
    visiting <<- setdiff(visiting, node)
    visited <<- c(visited, node)
    FALSE
  }
  any(vapply(names(adjacency), visit, logical(1)))
}

structural_canvas_structural_effect_definitions <- function(structural_edge_info) {
  if (!length(structural_edge_info)) return(list(lines = character(0), effects = list()))
  nodes <- sort(unique(c(
    vapply(structural_edge_info, function(item) item$predictor, character(1)),
    vapply(structural_edge_info, function(item) item$outcome, character(1))
  )))
  edge_expression <- list()
  adjacency <- setNames(vector("list", length(nodes)), nodes)
  for (item in structural_edge_info) {
    adjacency[[item$predictor]] <- unique(c(adjacency[[item$predictor]] %||% character(0), item$outcome))
    edge_expression[[paste(item$predictor, item$outcome, sep = "\r")]] <- item$label
  }
  if (structural_canvas_structural_has_cycle(adjacency)) return(list(lines = character(0), effects = list()))
  lines <- character(0)
  effects <- list()
  for (predictor in nodes) {
    for (outcome in setdiff(nodes, predictor)) {
      paths <- structural_canvas_find_structural_paths(adjacency, predictor, outcome)
      if (!length(paths)) next
      direct_key <- paste(predictor, outcome, sep = "\r")
      indirect_paths <- Filter(function(path) length(path) > 2L, paths)
      if (!length(indirect_paths)) next
      path_labels <- lapply(indirect_paths, function(path) {
        terms <- vapply(seq_len(length(path) - 1L), function(index) {
          edge_expression[[paste(path[[index]], path[[index + 1L]], sep = "\r")]]
        }, character(1))
        terms
      })
      path_expressions <- vapply(path_labels, function(terms) {
        paste(terms, collapse = " * ")
      }, character(1))
      indirect_label <- structural_canvas_structural_effect_label("statedu_ind", predictor, outcome)
      total_label <- structural_canvas_structural_effect_label("statedu_tot", predictor, outcome)
      indirect_expression <- paste(path_expressions, collapse = " + ")
      total_terms <- c(edge_expression[[direct_key]] %||% character(0), path_expressions)
      total_expression <- paste(total_terms[nzchar(total_terms)], collapse = " + ")
      lines <- c(lines, paste(indirect_label, ":=", indirect_expression), paste(total_label, ":=", total_expression))
      effects[[length(effects) + 1L]] <- list(
        label = indirect_label, type = "Indirect", predictor = predictor, outcome = outcome, paths = indirect_paths, path_labels = path_labels
      )
      effects[[length(effects) + 1L]] <- list(
        label = total_label, type = "Total", predictor = predictor, outcome = outcome, paths = paths
      )
    }
  }
  list(lines = lines, effects = effects)
}

structural_canvas_lavaan_syntax <- function(snapshot, data, analysis_type, latents, edges, ordered, residual_variance_fixes = numeric(0)) {
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
  structural_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    if (identical(as.character(edge$pathType %||% "regression"), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges)
  structural_edge_info <- Map(function(edge, index) {
    predictor_name <- structural_canvas_name(structural_canvas_node(snapshot, edge$from))
    outcome_name <- structural_canvas_name(structural_canvas_node(snapshot, edge$to))
    list(
      edge = edge,
      predictor = predictor_name,
      outcome = outcome_name,
      label = structural_canvas_structural_edge_label(edge, index)
    )
  }, structural_edges, seq_along(structural_edges))
  structural_lines <- vapply(structural_edge_info, function(item) {
    paste(item$outcome, "~", structural_canvas_structural_edge_term(item$edge, item$predictor, item$label))
  }, character(1))
  moderation_terms <- structural_canvas_structural_moderation_terms(snapshot, data, edges, structural_edge_info)
  data <- moderation_terms$data
  measurement_lines <- c(measurement_lines, moderation_terms$measurement_lines)
  structural_lines <- c(structural_lines, moderation_terms$structural_lines)
  effect_definitions <- if (analysis_type %in% c("cbsem", "sem")) {
    structural_canvas_structural_effect_definitions(structural_edge_info)
  } else {
    list(lines = character(0), effects = list())
  }
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

  list(
    syntax = paste(c(measurement_lines, higher_order_lines, structural_lines, effect_definitions$lines, covariance_lines, residual_parameter_lines, residual_fix_lines), collapse = "\n"),
    data = data,
    residual_variance_fixes = residual_variance_fixes,
    effect_definitions = effect_definitions$effects,
    moderation_definitions = moderation_terms$definitions
  )
}
