# Structural equation canvas PLS engine helpers.

structural_canvas_run_pls_analysis <- function(snapshot, data, latents, edges) {
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
  constructs <- lapply(latents, function(latent) {
    indicator_names <- latent_indicators(latent)
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
  construct_names <- vapply(latents, structural_canvas_name, character(1))
  indicator_names <- unique(unlist(lapply(latents, latent_indicators), use.names = FALSE))
  structural_paths <- vapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) {
    paste0(
      structural_canvas_name(structural_canvas_node(snapshot, edge$to)),
      " ~ ",
      structural_canvas_name(structural_canvas_node(snapshot, edge$from))
    )
  }, character(1))
  measurement_lines <- vapply(latents, function(latent) {
    latent_name <- structural_canvas_name(latent)
    indicators <- latent_indicators(latent)
    operator <- if (identical(latent$measurementMode %||% "reflective", "formative")) "<~" else "=~"
    paste(latent_name, operator, paste(indicators, collapse = " + "))
  }, character(1))
  fit <- seminr::estimate_pls(
    data = data,
    measurement_model = do.call(seminr::constructs, constructs),
    structural_model = if (length(path_specs)) do.call(seminr::relationships, path_specs) else NULL
  )
  list(
    fit = fit,
    syntax = paste(c(measurement_lines, structural_paths), collapse = "\n"),
    converged = TRUE,
    n = nrow(data),
    observed = indicator_names,
    constructs = construct_names,
    structural_paths = structural_paths,
    admissible = TRUE
  )
}
