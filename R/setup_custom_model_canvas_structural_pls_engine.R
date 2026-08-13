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
    ignored_covariances = unique(ignored_covariances),
    admissible = TRUE
  )
}

structural_canvas_run_pls_bootstrap <- function(analysis_type, pls_bootstrap, result, pls_seed) {
  pls_bootstrap <- suppressWarnings(as.integer(pls_bootstrap %||% 0L))
  if (!identical(analysis_type, "plssem") || pls_bootstrap <= 0L) return(NULL)
  if (is.null(result$fit) || !inherits(result$fit, "pls_model")) return(NULL)
  structural_canvas_with_progress(message = "Estimating PLS-SEM bootstrap intervals", value = 0, {
    structural_canvas_set_progress(
      value = .05,
      detail = paste0(pls_bootstrap, " seminr bootstrap resamples")
    )
    boot <- seminr::bootstrap_model(
      seminr_model = result$fit,
      nboot = pls_bootstrap,
      cores = 1,
      seed = as.integer(pls_seed %||% 24680L)
    )
    structural_canvas_set_progress(
      value = .90,
      detail = "Preparing PLS-SEM bootstrap summaries"
    )
    value <- summary(boot)
    structural_canvas_set_progress(value = 1, detail = "PLS-SEM bootstrap complete")
    value
  })
}

structural_canvas_run_pls_predict <- function(analysis_type, pls_predict_folds, pls_predict_reps, result) {
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
    prediction <- seminr::predict_pls(
      model = result$fit,
      technique = seminr::predict_DA,
      noFolds = pls_predict_folds,
      reps = pls_predict_reps,
      cores = NULL
    )
    structural_canvas_set_progress(value = .90, detail = "Preparing PLSpredict summaries")
    value <- list(
      folds = pls_predict_folds,
      reps = pls_predict_reps,
      technique = "Direct antecedents",
      summary = summary(prediction)
    )
    structural_canvas_set_progress(value = 1, detail = "PLSpredict complete")
    value
  })
}
