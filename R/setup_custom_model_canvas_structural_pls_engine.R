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

structural_canvas_run_pls_analysis <- function(snapshot, data, latents, edges, estimator = "PLS") {
  resolved_specification <- structural_canvas_resolve_construct_specification(snapshot, "plssem", estimator)
  unsupported <- !resolved_specification$supported
  if (any(unsupported)) stop(paste(unique(resolved_specification$reason[unsupported]), collapse = " "))
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
    latent_name <- structural_canvas_name(latent)
    resolved <- resolved_specification[resolved_specification$name == latent_name, , drop = FALSE]
    if (!nrow(resolved)) stop(paste0("Resolved PLS construct specification is missing for: ", latent_name, "."))
    if (identical(resolved$effective_weighting[[1L]], "Mode B")) {
      seminr::composite(latent_name, indicator_names, weights = seminr::mode_B)
    } else {
      seminr::reflective(latent_name, indicator_names)
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
  estimator <- toupper(as.character(estimator %||% "PLS"))
  if (!estimator %in% c("PLS", "PLSC")) estimator <- "PLS"
  fit <- seminr::estimate_pls(
    data = data,
    measurement_model = do.call(seminr::constructs, constructs),
    structural_model = if (length(path_specs)) do.call(seminr::relationships, path_specs) else NULL
  )
  if (identical(estimator, "PLSC")) {
    fit <- seminr::PLSc(fit)
  }
  predictive_relevance <- structural_canvas_pls_predictive_relevance(fit, structural_paths, folds = 7L)
  list(
    fit = fit,
    estimator = if (identical(estimator, "PLSC")) "PLSc" else "PLS",
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
    resolved_construct_specification = resolved_specification,
    admissible = TRUE
  )
}

structural_canvas_run_plsc_bootstrap <- function(seminr_model, nboot = 5000L, seed = default_seed()) {
  nboot <- suppressWarnings(as.integer(nboot %||% 5000L))
  seed <- suppressWarnings(as.integer(seed %||% default_seed()))
  if (!is.finite(nboot) || nboot < 1L) nboot <- 5000L
  if (!is.finite(seed) || seed < 1L) seed <- default_seed()
  d <- seminr_model$rawdata
  measurement_model <- seminr_model$measurement_model
  structural_model <- seminr_model$smMatrix
  inner_weights <- seminr_model$inner_weights
  original_htmt <- seminr:::HTMT(seminr_model)
  original_total <- seminr:::total_effects(seminr_model$path_coef)
  boot_vec_len <- length(seminr_model$path_coef) + length(seminr_model$outer_loadings) +
    length(seminr_model$outer_weights) + length(original_htmt) + length(original_total)
  boot_values <- lapply(seq_len(nboot), function(index) {
    set.seed(seed + index)
    tryCatch(suppressWarnings({
      sampled <- d[sample.int(nrow(d), replace = TRUE), , drop = FALSE]
      boot_model <- seminr::estimate_pls(
        data = sampled,
        measurement_model = measurement_model,
        structural_model = structural_model,
        inner_weights = inner_weights,
        assess_syntax = FALSE
      )
      boot_model <- seminr::PLSc(boot_model)
      c(
        c(boot_model$path_coef),
        c(boot_model$outer_loadings),
        c(boot_model$outer_weights),
        c(seminr:::HTMT(boot_model)),
        c(seminr:::total_effects(boot_model$path_coef))
      )
    }), error = function(error) rep(NA_real_, boot_vec_len))
  })
  bootmatrix <- do.call(cbind, boot_values)
  valid <- !is.na(bootmatrix[1L, ])
  bootmatrix <- bootmatrix[, valid, drop = FALSE]
  valid_n <- ncol(bootmatrix)
  if (!valid_n) return(NULL)

  path_rows <- nrow(seminr_model$path_coef)
  path_cols <- ncol(seminr_model$path_coef)
  mm_rows <- nrow(seminr_model$outer_loadings)
  mm_cols <- ncol(seminr_model$outer_loadings)

  start <- 1L
  end <- path_rows * path_cols
  boot_paths <- array(bootmatrix[start:end, , drop = FALSE], dim = c(path_rows, path_cols, valid_n), dimnames = list(rownames(seminr_model$path_coef), colnames(seminr_model$path_coef), seq_len(valid_n)))

  start <- end + 1L
  end <- start + (mm_rows * mm_cols) - 1L
  boot_loadings <- array(bootmatrix[start:end, , drop = FALSE], dim = c(mm_rows, mm_cols, valid_n), dimnames = list(rownames(seminr_model$outer_loadings), colnames(seminr_model$outer_loadings), seq_len(valid_n)))

  start <- end + 1L
  end <- start + (mm_rows * mm_cols) - 1L
  boot_weights <- array(bootmatrix[start:end, , drop = FALSE], dim = c(mm_rows, mm_cols, valid_n), dimnames = list(rownames(seminr_model$outer_weights), colnames(seminr_model$outer_weights), seq_len(valid_n)))

  htmt_rows <- nrow(original_htmt)
  htmt_cols <- ncol(original_htmt)
  start <- end + 1L
  end <- start + (htmt_rows * htmt_cols) - 1L
  boot_htmt <- array(bootmatrix[start:end, , drop = FALSE], dim = c(htmt_rows, htmt_cols, valid_n), dimnames = list(rownames(original_htmt), colnames(original_htmt), seq_len(valid_n)))

  start <- end + 1L
  end <- start + (path_rows * path_cols) - 1L
  boot_total_paths <- array(bootmatrix[start:end, , drop = FALSE], dim = c(path_rows, path_cols, valid_n), dimnames = list(rownames(original_total), colnames(original_total), seq_len(valid_n)))

  boot_summary <- list(
    nboot = valid_n,
    requested_nboot = nboot,
    bootstrapped_paths = seminr:::parse_boot_array(seminr_model$path_coef, boot_paths, alpha = .05),
    bootstrapped_weights = seminr:::parse_boot_array(seminr_model$outer_weights, boot_weights, alpha = .05),
    bootstrapped_loadings = seminr:::parse_boot_array(seminr_model$outer_loadings, boot_loadings, alpha = .05),
    bootstrapped_HTMT = seminr:::parse_boot_array_htmt(original_htmt, boot_htmt, alpha = .05),
    bootstrapped_total_paths = seminr:::parse_boot_array(original_total, boot_total_paths, alpha = .05),
    bootstrapped_total_indirect_paths = seminr:::parse_boot_array_total_indirect(
      original_matrix = seminr:::total_indirect_effects(seminr_model$path_coef),
      tp = boot_total_paths,
      rp = boot_paths,
      alpha = .05
    )
  )
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
    boot <- if (use_plsc) {
      structural_canvas_run_plsc_bootstrap(result$fit, pls_bootstrap, pls_seed)
    } else {
      seminr::bootstrap_model(
        seminr_model = result$fit,
        nboot = pls_bootstrap,
        cores = 1,
        seed = as.integer(pls_seed %||% default_seed())
      )
    }
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
    on.exit({
      if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    repetition_summaries <- lapply(seq_len(pls_predict_reps), function(index) {
      set.seed(as.integer(pls_predict_seed) + index - 1L)
      prediction <- seminr::predict_pls(
        model = result$fit,
        technique = seminr::predict_DA,
        noFolds = pls_predict_folds,
        reps = 1L,
        cores = NULL
      )
      summary(prediction)
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
      technique = "Direct antecedents",
      summary = summary_value,
      repetition_summaries = repetition_summaries
    )
    structural_canvas_set_progress(value = 1, detail = "PLSpredict complete")
    value
  })
}
