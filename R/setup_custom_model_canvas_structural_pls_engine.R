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

structural_canvas_pls_cv_q2_value <- function(scores, outcome, predictors, folds = 7L, excluded = character(0)) {
  predictors <- setdiff(unique(as.character(predictors %||% character(0))), as.character(excluded %||% character(0)))
  if (is.null(scores) || !outcome %in% colnames(scores)) return(list(q2 = NA_real_, press = NA_real_, tss = NA_real_, folds = 0L, n = 0L))
  columns <- unique(c(outcome, predictors))
  available <- intersect(columns, colnames(scores))
  if (!identical(sort(columns), sort(available))) return(list(q2 = NA_real_, press = NA_real_, tss = NA_real_, folds = 0L, n = 0L))
  values <- as.data.frame(scores[, columns, drop = FALSE], check.names = FALSE)
  finite_rows <- stats::complete.cases(values)
  values <- values[finite_rows, , drop = FALSE]
  n <- nrow(values)
  folds <- suppressWarnings(as.integer(folds %||% 7L))
  if (!is.finite(folds) || folds < 2L) folds <- 2L
  folds <- min(folds, max(2L, floor(n / 2L)))
  if (n < 6L || folds < 2L) return(list(q2 = NA_real_, press = NA_real_, tss = NA_real_, folds = 0L, n = n))
  fold_id <- ((seq_len(n) - 1L) %% folds) + 1L
  press <- 0
  tss <- 0
  used_folds <- 0L
  for (fold in seq_len(folds)) {
    test <- fold_id == fold
    train <- !test
    if (sum(test) < 1L || sum(train) <= length(predictors) + 1L) next
    y_train <- values[[outcome]][train]
    y_test <- values[[outcome]][test]
    x_train <- if (length(predictors)) as.matrix(values[train, predictors, drop = FALSE]) else matrix(numeric(0), sum(train), 0L)
    x_test <- if (length(predictors)) as.matrix(values[test, predictors, drop = FALSE]) else matrix(numeric(0), sum(test), 0L)
    design_train <- cbind(`(Intercept)` = 1, x_train)
    design_test <- cbind(`(Intercept)` = 1, x_test)
    coefficients <- tryCatch(stats::lm.fit(design_train, y_train)$coefficients, error = function(error) rep(NA_real_, ncol(design_train)))
    coefficients[!is.finite(coefficients)] <- 0
    predicted <- as.vector(design_test %*% coefficients)
    fold_tss <- sum((y_test - mean(y_train, na.rm = TRUE))^2, na.rm = TRUE)
    if (!is.finite(fold_tss) || fold_tss <= 0) next
    press <- press + sum((y_test - predicted)^2, na.rm = TRUE)
    tss <- tss + fold_tss
    used_folds <- used_folds + 1L
  }
  if (!used_folds || !is.finite(tss) || tss <= 0) return(list(q2 = NA_real_, press = NA_real_, tss = NA_real_, folds = used_folds, n = n))
  list(q2 = 1 - press / tss, press = press, tss = tss, folds = used_folds, n = n)
}

structural_canvas_pls_predictive_relevance <- function(fit, structural_paths, folds = 7L) {
  if (is.null(fit) || is.null(fit$construct_scores)) {
    return(list(q2 = data.frame(), q2_effects = data.frame(), folds = 0L))
  }
  scores <- as.matrix(fit$construct_scores)
  path_specs <- structural_canvas_pls_path_specs_from_strings(structural_paths)
  if (!nrow(path_specs)) return(list(q2 = data.frame(), q2_effects = data.frame(), folds = 0L))
  outcomes <- unique(path_specs$outcome)
  q2_rows <- list()
  effect_rows <- list()
  for (outcome in outcomes) {
    predictors <- unique(path_specs$predictor[path_specs$outcome == outcome])
    included <- structural_canvas_pls_cv_q2_value(scores, outcome, predictors, folds = folds)
    q2_rows[[length(q2_rows) + 1L]] <- data.frame(
      Outcome = outcome,
      Predictors = paste(predictors, collapse = ", "),
      Q2 = included$q2,
      PRESS = included$press,
      TSS = included$tss,
      Folds = included$folds,
      N = included$n,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    for (predictor in predictors) {
      excluded <- structural_canvas_pls_cv_q2_value(scores, outcome, predictors, folds = folds, excluded = predictor)
      effect <- if (is.finite(included$q2) && is.finite(excluded$q2) && is.finite(1 - included$q2) && (1 - included$q2) > 0) {
        (included$q2 - excluded$q2) / (1 - included$q2)
      } else {
        NA_real_
      }
      effect_rows[[length(effect_rows) + 1L]] <- data.frame(
        Outcome = outcome,
        Predictor = predictor,
        `Q2 included` = included$q2,
        `Q2 excluded` = excluded$q2,
        q2 = effect,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  list(
    q2 = if (length(q2_rows)) do.call(rbind, q2_rows) else data.frame(),
    q2_effects = if (length(effect_rows)) do.call(rbind, effect_rows) else data.frame(),
    folds = folds
  )
}

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
  predictive_relevance <- structural_canvas_pls_predictive_relevance(fit, structural_paths, folds = 7L)
  list(
    fit = fit,
    syntax = paste(c(measurement_lines, structural_paths), collapse = "\n"),
    converged = TRUE,
    n = nrow(data),
    observed = indicator_names,
    constructs = construct_names,
    structural_paths = structural_paths,
    q2 = predictive_relevance$q2,
    q2_effects = predictive_relevance$q2_effects,
    q2_folds = predictive_relevance$folds,
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
