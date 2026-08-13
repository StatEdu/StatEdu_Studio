# Structural equation canvas lavaan syntax helpers.

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

  list(
    syntax = paste(c(measurement_lines, higher_order_lines, structural_lines, covariance_lines, residual_parameter_lines, residual_fix_lines), collapse = "\n"),
    residual_variance_fixes = residual_variance_fixes
  )
}
