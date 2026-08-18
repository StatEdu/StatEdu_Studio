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

structural_canvas_apply_plsc <- function(fit, common_factor_constructs = character(0)) {
  score_names <- if (is.null(fit$construct_scores)) character(0) else colnames(fit$construct_scores)
  common_factor_constructs <- intersect(as.character(common_factor_constructs), score_names)
  if (!length(common_factor_constructs)) stop("PLSc requires at least one reflective common-factor construct.")
  all_constructs <- seminr:::constructs_in_model(fit)$construct_names
  if (setequal(common_factor_constructs, all_constructs)) {
    corrected <- seminr::PLSc(fit)
    corrected$statedu_common_factor_constructs <- common_factor_constructs
    corrected$statedu_plsc_mode <- "all_common_factors"
    corrected$statedu_plsc_correction_status <- "complete"
    corrected$statedu_plsc_corrected_endogenous <- seminr:::all_endogenous(corrected$smMatrix)
    return(corrected)
  }
  sm_matrix <- fit$smMatrix
  mm_matrix <- fit$mmMatrix
  path_coef <- fit$path_coef
  loadings <- fit$outer_loadings
  construct_scores <- fit$construct_scores
  rho <- seminr:::rho_A(fit, all_constructs)
  rho[!rownames(rho) %in% common_factor_constructs, ] <- 1
  rho[seminr:::is_interaction(rownames(rho)), ] <- 1
  adjustment <- sqrt(rho %*% t(rho))
  diag(adjustment) <- 1
  adjusted_correlations <- stats::cor(construct_scores, use = "pairwise.complete.obs") / adjustment
  if (any(!is.finite(adjusted_correlations))) {
    stop("PLSc correction failed because the disattenuated construct-correlation matrix contains non-finite values.")
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
  reflectives <- intersect(seminr:::all_reflective(mm_matrix), common_factor_constructs)
  for (construct in reflectives) {
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
  fit$statedu_plsc_mode <- "mixed_common_factors_and_composites"
  fit$statedu_plsc_correction_status <- "complete"
  fit$statedu_plsc_corrected_endogenous <- unique(corrected_endogenous)
  fit
}

structural_canvas_run_pls_analysis <- function(snapshot, data, latents, edges, estimator = "PLS") {
  selection <- structural_canvas_select_pls_estimator(snapshot, estimator)
  estimator <- selection$selected
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
  fit$statedu_common_factor_constructs <- selection$common_factors
  fit$statedu_composite_constructs <- selection$composites
  if (identical(estimator, "PLSC")) {
    fit <- structural_canvas_apply_plsc(fit, selection$common_factors)
  }
  predictive_relevance <- structural_canvas_pls_predictive_relevance(fit, structural_paths, folds = 7L)
  list(
    fit = fit,
    estimator = if (identical(estimator, "PLSC")) "PLSc" else "PLS",
    estimator_requested = selection$requested,
    estimator_selection_mode = selection$mode,
    estimator_selection_reason = selection$reason,
    plsc_corrected_constructs = selection$common_factors,
    plsc_uncorrected_composites = selection$composites,
    plsc_correction_status = as.character(fit$statedu_plsc_correction_status %||% if (identical(estimator, "PLSC")) "not recorded" else "not applicable"),
    plsc_corrected_endogenous = as.character(fit$statedu_plsc_corrected_endogenous %||% character(0)),
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

structural_canvas_run_plsc_bootstrap <- function(seminr_model, nboot = 5000L, seed = default_seed(), progress_file = NULL, apply_plsc = TRUE) {
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
  detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (!is.finite(detected_cores) || detected_cores < 2L) detected_cores <- 2L
  workers <- max(1L, min(4L, as.integer(detected_cores) - 1L, nboot))
  batch_size <- max(50L, workers * 25L)
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
  estimate_one <- function(index, d, measurement_model, structural_model, inner_weights, seed, apply_plsc, common_factor_constructs, boot_vec_len, reference_loadings) {
    set.seed(seed + index)
    tryCatch({
      setTimeLimit(cpu = Inf, elapsed = 60, transient = TRUE)
      suppressWarnings({
        sampled <- d[sample.int(nrow(d), replace = TRUE), , drop = FALSE]
        boot_model <- seminr::estimate_pls(
          data = sampled,
          measurement_model = measurement_model,
          structural_model = structural_model,
          inner_weights = inner_weights,
          maxIt = 300,
          stopCriterion = 7,
          assess_syntax = FALSE
        )
        if (isTRUE(apply_plsc)) boot_model <- structural_canvas_apply_plsc(boot_model, common_factor_constructs)
        boot_model <- align_bootstrap_signs(boot_model, reference_loadings)
        c(
          c(boot_model$path_coef),
          c(boot_model$outer_loadings),
          c(boot_model$outer_weights),
          c(seminr:::HTMT(boot_model)),
          c(seminr:::total_effects(boot_model$path_coef))
        )
      })
    }, error = function(error) {
      failed <- rep(NA_real_, boot_vec_len)
      attr(failed, "failure_reason") <- if (grepl("time limit|elapsed time", conditionMessage(error), ignore.case = TRUE)) "timeout" else "estimation"
      failed
    }, finally = {
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    })
  }
  cluster <- if (workers > 1L) parallel::makePSOCKcluster(workers) else NULL
  if (!is.null(cluster)) {
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    parallel::clusterExport(
      cluster,
      c("estimate_one", "align_bootstrap_signs", "structural_canvas_apply_plsc", "common_factor_constructs", "d", "measurement_model", "structural_model", "inner_weights", "seed", "apply_plsc", "boot_vec_len", "reference_loadings"),
      envir = environment()
    )
  }
  structural_canvas_write_bootstrap_progress(progress_file, 0L, nboot, "resampling", TRUE)
  starts <- seq.int(1L, nboot, by = batch_size)
  for (start in starts) {
    indices <- seq.int(start, min(nboot, start + batch_size - 1L))
    values <- if (is.null(cluster)) {
      lapply(indices, estimate_one, d, measurement_model, structural_model, inner_weights, seed, apply_plsc, common_factor_constructs, boot_vec_len, reference_loadings)
    } else {
      parallel::parLapplyLB(cluster, indices, function(index) {
        estimate_one(index, d, measurement_model, structural_model, inner_weights, seed, apply_plsc, common_factor_constructs, boot_vec_len, reference_loadings)
      })
    }
    boot_values[indices] <- values
    structural_canvas_write_bootstrap_progress(progress_file, max(indices), nboot, "resampling", TRUE)
  }
  structural_canvas_write_bootstrap_progress(progress_file, nboot, nboot, "summarizing", TRUE)
  failure_reasons <- vapply(boot_values, function(value) as.character(attr(value, "failure_reason") %||% ""), character(1))
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
    timeout_failures = sum(failure_reasons == "timeout"),
    estimation_failures = sum(failure_reasons == "estimation"),
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
