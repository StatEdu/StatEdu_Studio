# Structural identification residual-constraint helpers.

structural_canvas_constrained_single_indicators <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  result <- character(0)
  for (latent in latents) {
    latent_id <- as.character(latent$id %||% "")
    measurement_edges <- Filter(function(edge) {
      if (identical(edge$kind, "covariance")) return(FALSE)
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(as.character(from$id %||% ""), latent_id) && identical(to$role, "indicator")) ||
        (identical(as.character(to$id %||% ""), latent_id) && identical(from$role, "indicator"))
    }, edges)
    indicators <- unique(vapply(measurement_edges, function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      as.character(if (identical(from$role, "indicator")) from$id else to$id)
    }, character(1)))
    if (length(indicators) != 1L) next
    constrained <- any(vapply(edges, function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      identical(as.character(edge$to %||% ""), indicators[[1L]]) && !is.null(from) && identical(from$role, "error") &&
        identical(edge$free, FALSE) && is.finite(suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))) &&
        suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_)) > 0
    }, logical(1)))
    if (constrained) result <- c(result, structural_canvas_name(latent))
  }
  unique(result)
}

structural_canvas_fixed_residual_scale_diagnostics <- function(snapshot, data, ordered = character(0)) {
  if (!is.data.frame(data)) return(data.frame())
  ordered <- unique(as.character(ordered))
  rows <- list()
  for (edge in snapshot$edges %||% list()) {
    if (!identical(edge$free, FALSE) || identical(edge$kind, "covariance")) next
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (is.null(from) || is.null(to) || !from$role %in% c("error", "disturbance") || !identical(to$role, "indicator")) next
    indicator <- structural_canvas_name(to)
    if (!indicator %in% names(data) || indicator %in% ordered || !is.numeric(data[[indicator]])) next
    fixed_value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
    observed_variance <- stats::var(data[[indicator]], na.rm = TRUE)
    if (!is.finite(fixed_value) || !is.finite(observed_variance)) next
    indicator_id <- as.character(to$id %||% "")
    parent_latents <- unique(vapply(Filter(function(measurement_edge) {
      if (identical(measurement_edge$kind, "covariance")) return(FALSE)
      measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
      measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
      (identical(as.character(measurement_from$id %||% ""), indicator_id) && identical(measurement_to$role, "latent")) ||
        (identical(as.character(measurement_to$id %||% ""), indicator_id) && identical(measurement_from$role, "latent"))
    }, snapshot$edges %||% list()), function(measurement_edge) {
      measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
      measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
      as.character(if (identical(measurement_from$role, "latent")) measurement_from$id else measurement_to$id)
    }, character(1)))
    parent_indicator_counts <- vapply(parent_latents, function(parent_id) {
      length(unique(vapply(Filter(function(measurement_edge) {
        if (identical(measurement_edge$kind, "covariance")) return(FALSE)
        measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
        measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
        (identical(as.character(measurement_from$id %||% ""), parent_id) && identical(measurement_to$role, "indicator")) ||
          (identical(as.character(measurement_to$id %||% ""), parent_id) && identical(measurement_from$role, "indicator"))
      }, snapshot$edges %||% list()), function(measurement_edge) {
        measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
        measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
        as.character(if (identical(measurement_from$role, "indicator")) measurement_from$id else measurement_to$id)
      }, character(1))))
    }, integer(1))
    single_indicator_factor <- length(parent_indicator_counts) > 0L && any(parent_indicator_counts == 1L)
    status <- if (fixed_value > observed_variance) "Exceeds observed variance" else if (fixed_value == observed_variance) "Equals observed variance" else "Within observed variance"
    rows[[length(rows) + 1L]] <- data.frame(
      Indicator = indicator, `Fixed residual variance` = fixed_value,
      `Observed variance` = observed_variance,
      `Implied maximum common variance` = observed_variance - fixed_value,
      `Single-indicator factor` = single_indicator_factor,
      Status = status, check.names = FALSE
    )
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}
