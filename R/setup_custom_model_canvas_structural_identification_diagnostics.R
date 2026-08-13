# Structural identification diagnostic helpers.

structural_canvas_missing_exogenous_covariances <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  endogenous_ids <- unique(vapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) as.character(edge$to), character(1)))
  exogenous <- Filter(function(node) !as.character(node$id) %in% endogenous_ids, latents)
  if (length(exogenous) < 2L) return(character(0))
  pairs <- utils::combn(exogenous, 2L, simplify = FALSE)
  missing <- Filter(function(pair) {
    ids <- vapply(pair, function(node) as.character(node$id), character(1))
    !any(vapply(edges, function(edge) {
      if (!identical(edge$kind, "covariance")) return(FALSE)
      endpoints <- c(as.character(edge$from), as.character(edge$to))
      setequal(endpoints, ids)
    }, logical(1)))
  }, pairs)
  vapply(missing, function(pair) paste(vapply(pair, structural_canvas_name, character(1)), collapse = " ↔ "), character(1))
}

structural_canvas_identification_diagnostics <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  issues <- list()
  add <- function(severity, element, code, message) {
    issues[[length(issues) + 1L]] <<- data.frame(Severity = severity, Element = element, Code = code, Message = message, stringsAsFactors = FALSE)
  }
  higher_edges <- Filter(function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), edges)
  for (latent in latents) {
    latent_id <- as.character(latent$id)
    latent_name <- structural_canvas_name(latent)
    observed <- unique(vapply(Filter(function(edge) {
      if (identical(edge$kind, "covariance")) return(FALSE)
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(as.character(from$id %||% ""), latent_id) && identical(to$role, "indicator")) ||
        (identical(as.character(to$id %||% ""), latent_id) && identical(from$role, "indicator"))
    }, edges), function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "indicator")) from else to)
    }, character(1)))
    children <- unique(vapply(Filter(function(edge) identical(as.character(edge$from), latent_id), higher_edges), function(edge) structural_canvas_name(structural_canvas_node(snapshot, edge$to)), character(1)))
    if (!length(observed) && !length(children)) add("Error", latent_name, "unmeasured_latent", "The latent variable has neither observed indicators nor lower-order factors.")
    single_indicator_constrained <- FALSE
    if (length(observed) == 1L && !length(children)) {
      indicator_node <- Filter(function(node) identical(node$role, "indicator") && identical(structural_canvas_name(node), observed[[1L]]), nodes)
      indicator_id <- if (length(indicator_node)) as.character(indicator_node[[1L]]$id) else ""
      single_indicator_constrained <- any(vapply(edges, function(edge) {
        from <- structural_canvas_node(snapshot, edge$from)
        identical(as.character(edge$to), indicator_id) && !is.null(from) && identical(from$role, "error") &&
          identical(edge$free, FALSE) && is.finite(suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))) &&
          suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_)) > 0
      }, logical(1)))
      if (!single_indicator_constrained) add("Error", latent_name, "single_indicator", "A single-indicator factor requires an externally justified fixed residual variance on its error path.")
      else add("Warning", latent_name, "single_indicator_constrained", "The single-indicator factor is identified using a fixed residual variance; document the external reliability basis for this constraint.")
    }
    if (length(observed) == 2L && !length(children)) add("Warning", latent_name, "two_indicators", "A two-indicator factor may require additional constraints or structural information for stable identification.")
    if (length(children) > 0L && length(children) < 3L) add("Error", latent_name, "few_lower_order_factors", "A higher-order factor requires at least three lower-order factors in the current automatic identification scheme.")
    if (length(observed) && length(children)) add("Warning", latent_name, "mixed_measurement_level", "The factor has both observed indicators and lower-order factors; verify that this hybrid measurement specification is intentional.")
  }
  if (length(higher_edges)) {
    child_ids <- vapply(higher_edges, function(edge) as.character(edge$to), character(1))
    for (child_id in unique(child_ids[duplicated(child_ids)])) {
      child <- structural_canvas_node(snapshot, child_id)
      add("Warning", structural_canvas_name(child), "multiple_higher_order_parents", "The lower-order factor loads on more than one higher-order factor; standard higher-order reliability summaries may not apply.")
    }
  }
  measurement_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance") || identical(as.character(edge$pathType %||% ""), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) &&
      ((identical(from$role, "latent") && identical(to$role, "indicator")) ||
        (identical(to$role, "latent") && identical(from$role, "indicator")))
  }, edges)
  indicator_parents <- split(measurement_edges, vapply(measurement_edges, function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    structural_canvas_name(if (identical(from$role, "indicator")) from else to)
  }, character(1)))
  for (indicator_name in names(indicator_parents)) {
    parents <- unique(vapply(indicator_parents[[indicator_name]], function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "latent")) from else to)
    }, character(1)))
    if (length(parents) > 1L) add("Warning", indicator_name, "cross_loading", paste0("The indicator loads on multiple factors: ", paste(parents, collapse = ", "), ". Review simple-structure assumptions and reliability/validity summaries."))
  }
  fixed_residual_edges <- Filter(function(edge) {
    if (!identical(edge$free, FALSE) || identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && from$role %in% c("error", "disturbance") && to$role %in% c("indicator", "latent")
  }, edges)
  for (edge in fixed_residual_edges) {
    target <- structural_canvas_node(snapshot, edge$to)
    value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
    if (!is.finite(value)) {
      add("Error", structural_canvas_name(target), "invalid_fixed_residual", "A fixed residual variance must be a finite nonnegative number.")
    } else if (value < 0) {
      add("Error", structural_canvas_name(target), "negative_fixed_residual", "A residual variance cannot be fixed to a negative value.")
    } else if (value == 0) {
      add("Warning", structural_canvas_name(target), "boundary_fixed_residual", "A zero fixed residual variance is a boundary constraint implying perfect residual-free measurement; provide strong substantive justification.")
    }
  }
  directed <- Filter(function(edge) !identical(edge$kind, "covariance"), edges)
  signatures <- vapply(directed, function(edge) paste(edge$from, edge$to, edge$pathType %||% "regression", sep = "|"), character(1))
  if (anyDuplicated(signatures)) add("Error", "Model", "duplicate_path", "Duplicate directed paths were found between the same endpoints.")
  result <- if (length(issues)) do.call(rbind, issues) else data.frame(Severity = character(0), Element = character(0), Code = character(0), Message = character(0), stringsAsFactors = FALSE)
  rownames(result) <- NULL
  result
}
