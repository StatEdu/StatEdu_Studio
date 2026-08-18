# Structural equation canvas bootstrap helpers.

structural_canvas_htmt_bootstrap <- function(data, indicators_by_factor, reps = 0L, confidence = .95, seed = default_seed(), ordered = character(0), threshold = .85, ci_method = "percentile", progress = NULL, cancel = NULL) {
  reps <- suppressWarnings(as.integer(reps))
  confidence <- suppressWarnings(as.numeric(confidence))
  threshold <- suppressWarnings(as.numeric(threshold))
  ci_method <- structural_canvas_bootstrap_ci_method(ci_method)
  variables <- unique(unlist(indicators_by_factor, use.names = FALSE))
  if (!is.data.frame(data) || reps < 2L || length(indicators_by_factor) < 2L ||
      !all(variables %in% names(data)) ||
      !is.finite(confidence) || confidence <= 0 || confidence >= 1 || !is.finite(threshold) || threshold <= 0) {
    return(NULL)
  }
  values <- data[variables]
  ordered <- intersect(as.character(ordered), variables)
  continuous <- setdiff(variables, ordered)
  if (length(continuous) && !all(vapply(values[continuous], is.numeric, logical(1)))) return(NULL)
  n <- nrow(values)
  if (n < 3L) return(NULL)
  factor_names <- names(indicators_by_factor)
  pairs <- utils::combn(factor_names, 2L, simplify = FALSE)
  estimates <- matrix(NA_real_, nrow = reps, ncol = length(pairs))
  compute_correlations <- function(frame) {
    if (length(ordered)) {
      suppressWarnings(tryCatch(
        as.matrix(lavaan::lavCor(frame, ordered = ordered, missing = "pairwise", estimator = "two.step", se = "none", test = "none", output = "cor", cor.smooth = TRUE)),
        error = function(error) NULL
      ))
    } else {
      suppressWarnings(stats::cor(frame, use = "pairwise.complete.obs"))
    }
  }
  compute_pair_values <- function(frame) {
    correlations <- compute_correlations(frame)
    if (is.null(correlations) || !all(variables %in% rownames(correlations))) return(rep(NA_real_, length(pairs)))
    htmt <- structural_canvas_htmt(correlations, indicators_by_factor, threshold = 1)
    vapply(seq_along(pairs), function(pair_index) {
      pair <- pairs[[pair_index]]
      as.numeric(htmt$matrix[pair[[1L]], pair[[2L]]])
    }, numeric(1))
  }
  original_values <- if (identical(ci_method, "bca")) compute_pair_values(values) else rep(NA_real_, length(pairs))
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  total_iterations <- reps + if (identical(ci_method, "bca")) n else 0L
  progress_step <- max(1L, floor(total_iterations / 100L))
  if (is.function(progress)) progress(0L, total_iterations, 0L)
  for (index in seq_len(reps)) {
    if (is.function(cancel) && isTRUE(cancel())) stop("HTMT bootstrap canceled.")
    sampled <- values[sample.int(n, n, replace = TRUE), , drop = FALSE]
    estimates[index, ] <- compute_pair_values(sampled)
    if (is.function(progress) && (index == 1L || index == total_iterations || index %% progress_step == 0L)) {
      valid_counts <- colSums(is.finite(estimates[seq_len(index), , drop = FALSE]))
      progress(index, total_iterations, if (length(valid_counts)) min(valid_counts) else 0L)
    }
  }
  jackknife <- NULL
  if (identical(ci_method, "bca")) {
    jackknife <- matrix(NA_real_, nrow = n, ncol = length(pairs))
    for (index in seq_len(n)) {
      if (is.function(cancel) && isTRUE(cancel())) stop("HTMT bootstrap canceled.")
      jackknife[index, ] <- compute_pair_values(values[-index, , drop = FALSE])
      completed <- reps + index
      if (is.function(progress) && (completed == total_iterations || completed %% progress_step == 0L)) {
        valid_counts <- colSums(is.finite(estimates))
        progress(completed, total_iterations, if (length(valid_counts)) min(valid_counts) else 0L)
      }
    }
  }
  alpha <- (1 - confidence) / 2
  rows <- lapply(seq_along(pairs), function(pair_index) {
    pair_values <- estimates[, pair_index]
    pair_values <- pair_values[is.finite(pair_values)]
    interval <- if (identical(ci_method, "bca")) {
      structural_canvas_bca_interval(pair_values, original_values[[pair_index]], jackknife[, pair_index], confidence)
    } else if (length(pair_values) >= max(20L, ceiling(.5 * reps))) {
      as.numeric(stats::quantile(pair_values, probs = c(alpha, 1 - alpha), names = FALSE, type = 6, na.rm = TRUE))
    } else c(NA_real_, NA_real_)
    upper_one_sided <- if (length(pair_values) >= max(20L, ceiling(.5 * reps))) {
      as.numeric(stats::quantile(pair_values, probs = confidence, names = FALSE, type = 6, na.rm = TRUE))
    } else NA_real_
    data.frame(
      `Factor 1` = pairs[[pair_index]][[1L]], `Factor 2` = pairs[[pair_index]][[2L]],
      Lower = interval[[1L]], Upper = interval[[2L]],
      `One-sided upper` = upper_one_sided,
      `Upper < threshold` = if (is.finite(upper_one_sided)) if (upper_one_sided < threshold) "Yes" else "No" else "Not assessed",
      `Upper < 1` = if (is.finite(interval[[2L]])) if (interval[[2L]] < 1) "Yes" else "No" else "Not assessed",
      `CI method` = if (identical(ci_method, "bca")) {
        if (all(is.finite(interval))) "BCa" else "BCa unavailable"
      } else "Percentile",
      `Valid replicates` = length(pair_values), `Requested replicates` = reps,
      `Valid %` = 100 * length(pair_values) / reps,
      Status = structural_canvas_bootstrap_status(length(pair_values), reps), check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_bootstrap_status <- function(valid, requested) {
  ratio <- as.numeric(valid) / as.numeric(requested)
  ifelse(!is.finite(ratio) | ratio < .50, "Unreliable", ifelse(ratio < .80, "Caution", "Adequate"))
}

structural_canvas_bootstrap_ci_method <- function(value) {
  value <- tolower(trimws(as.character(value %||% "percentile")))
  if (grepl("^bca", value)) return("bca")
  if (value %in% c("bca", "bc_a", "bias-corrected accelerated", "bias corrected accelerated")) return("bca")
  "percentile"
}

structural_canvas_bca_interval <- function(bootstrap_values, original_value, jackknife_values, confidence = .95) {
  bootstrap_values <- as.numeric(bootstrap_values)
  bootstrap_values <- bootstrap_values[is.finite(bootstrap_values)]
  jackknife_values <- as.numeric(jackknife_values)
  jackknife_values <- jackknife_values[is.finite(jackknife_values)]
  confidence <- as.numeric(confidence)
  if (length(bootstrap_values) < 20L || length(jackknife_values) < 10L ||
      !is.finite(original_value) || !is.finite(confidence) || confidence <= 0 || confidence >= 1) {
    return(c(NA_real_, NA_real_))
  }
  alpha <- (1 - confidence) / 2
  prop_less <- (sum(bootstrap_values < original_value) + .5) / (length(bootstrap_values) + 1)
  z0 <- stats::qnorm(prop_less)
  jackknife_mean <- mean(jackknife_values)
  jackknife_delta <- jackknife_mean - jackknife_values
  denominator <- 6 * (sum(jackknife_delta^2)^(3 / 2))
  acceleration <- if (is.finite(denominator) && denominator > 0) sum(jackknife_delta^3) / denominator else 0
  adjusted <- vapply(c(alpha, 1 - alpha), function(probability) {
    z_alpha <- stats::qnorm(probability)
    denominator <- 1 - acceleration * (z0 + z_alpha)
    if (!is.finite(denominator) || abs(denominator) < .Machine$double.eps) return(NA_real_)
    stats::pnorm(z0 + (z0 + z_alpha) / denominator)
  }, numeric(1))
  if (any(!is.finite(adjusted)) || any(adjusted <= 0 | adjusted >= 1)) return(c(NA_real_, NA_real_))
  as.numeric(stats::quantile(bootstrap_values, probs = adjusted, names = FALSE, type = 6, na.rm = TRUE))
}

structural_canvas_moderated_mediation_indices <- function(result) {
  effects <- result$effect_definitions %||% list()
  moderations <- result$moderation_definitions %||% list()
  if (!length(effects) || !length(moderations) || is.null(result$fit)) return(data.frame())
  parameters <- lavaan::parameterEstimates(result$fit)
  labeled <- parameters[nzchar(parameters$label %||% ""), c("label", "est"), drop = FALSE]
  coefficients <- stats::setNames(as.numeric(labeled$est), as.character(labeled$label))
  rows <- list()
  for (effect in effects) {
    if (!identical(as.character(effect$type %||% ""), "Indirect")) next
    paths <- effect$paths %||% list()
    path_labels <- effect$path_labels %||% list()
    for (path_index in seq_along(paths)) {
      path <- paths[[path_index]]
      labels <- path_labels[[path_index]] %||% character(0)
      if (length(path) < 3L || length(labels) != length(path) - 1L) next
      for (definition in moderations) {
        edge_position <- which(
          path[-length(path)] == as.character(definition$predictor %||% "") &
            path[-1L] == as.character(definition$outcome %||% "")
        )
        if (!length(edge_position)) next
        edge_position <- edge_position[[1L]]
        interaction_label <- as.character(definition$interaction_label %||% "")
        other_labels <- labels[-edge_position]
        required <- c(interaction_label, other_labels)
        if (!length(interaction_label) || !all(required %in% names(coefficients))) next
        values <- unname(coefficients[required])
        if (any(!is.finite(values))) next
        rows[[length(rows) + 1L]] <- data.frame(
          lhs = paste(path, collapse = " -> "), op = "modmed",
          rhs = as.character(definition$moderator %||% ""),
          est = unname(coefficients[[interaction_label]]) * if (length(other_labels)) prod(unname(coefficients[other_labels])) else 1,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(data.frame())
  unique(do.call(rbind, rows))
}

structural_canvas_effect_bootstrap <- function(snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal, residual_variance_fixes, reps = 0L, seed = default_seed()) {
  reps <- suppressWarnings(as.integer(reps))
  if (!analysis_type %in% c("cbsem", "sem") || !is.data.frame(data) || nrow(data) < 3L || !is.finite(reps) || reps < 2L) return(NULL)
  original <- run_structural_canvas_analysis(snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal, residual_variance_fixes)
  raw_original <- lavaan::parameterEstimates(original$fit)
  raw_original <- raw_original[raw_original$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE]
  moderated_original <- structural_canvas_moderated_mediation_indices(original)
  if (nrow(moderated_original)) raw_original <- rbind(raw_original, moderated_original)
  keys <- paste(raw_original$lhs, raw_original$op, raw_original$rhs, sep = "\r")
  draws <- matrix(NA_real_, nrow = reps, ncol = length(keys), dimnames = list(NULL, keys))
  standardized_original <- tryCatch(lavaan::standardizedSolution(original$fit, ci = FALSE), error = function(error) data.frame())
  standardized_original_values <- rep(NA_real_, length(keys))
  if (all(c("lhs", "op", "rhs", "est.std") %in% names(standardized_original))) {
    standardized_keys <- paste(standardized_original$lhs, standardized_original$op, standardized_original$rhs, sep = "\r")
    standardized_match <- match(keys, standardized_keys)
    standardized_original_values[!is.na(standardized_match)] <- standardized_original$est.std[standardized_match[!is.na(standardized_match)]]
  }
  standardized_draws <- matrix(NA_real_, nrow = reps, ncol = length(keys), dimnames = list(NULL, keys))
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  for (index in seq_len(reps)) {
    sampled <- data[sample.int(nrow(data), nrow(data), replace = TRUE), , drop = FALSE]
    fit <- suppressWarnings(tryCatch(
      run_structural_canvas_analysis(snapshot, sampled, analysis_type, estimator, missing, std_lv, ordered, nominal, residual_variance_fixes),
      error = function(error) NULL
    ))
    if (is.null(fit) || !isTRUE(fit$converged) || !isTRUE(fit$admissible)) next
    estimates <- lavaan::parameterEstimates(fit$fit)
    estimates <- estimates[estimates$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE]
    moderated <- structural_canvas_moderated_mediation_indices(fit)
    if (nrow(moderated)) estimates <- rbind(estimates, moderated)
    estimate_keys <- paste(estimates$lhs, estimates$op, estimates$rhs, sep = "\r")
    matched <- match(keys, estimate_keys)
    draws[index, !is.na(matched)] <- estimates$est[matched[!is.na(matched)]]
    standardized <- tryCatch(lavaan::standardizedSolution(fit$fit, ci = FALSE), error = function(error) data.frame())
    if (all(c("lhs", "op", "rhs", "est.std") %in% names(standardized))) {
      standardized_keys <- paste(standardized$lhs, standardized$op, standardized$rhs, sep = "\r")
      standardized_match <- match(keys, standardized_keys)
      standardized_draws[index, !is.na(standardized_match)] <- standardized$est.std[standardized_match[!is.na(standardized_match)]]
    }
  }
  rows <- lapply(seq_along(keys), function(column) {
    values <- draws[, column]
    values <- values[is.finite(values)]
    valid <- length(values)
    interval <- if (valid >= max(20L, ceiling(.5 * reps))) as.numeric(stats::quantile(values, c(.025, .975), names = FALSE, type = 6)) else c(NA_real_, NA_real_)
    p_value <- if (valid) min(1, 2 * min((sum(values <= 0) + 1) / (valid + 1), (sum(values >= 0) + 1) / (valid + 1))) else NA_real_
    standardized_values <- standardized_draws[, column]
    standardized_values <- standardized_values[is.finite(standardized_values)]
    standardized_valid <- length(standardized_values)
    standardized_interval <- if (standardized_valid >= max(20L, ceiling(.5 * reps))) as.numeric(stats::quantile(standardized_values, c(.025, .975), names = FALSE, type = 6)) else c(NA_real_, NA_real_)
    standardized_p <- if (standardized_valid) min(1, 2 * min((sum(standardized_values <= 0) + 1) / (standardized_valid + 1), (sum(standardized_values >= 0) + 1) / (standardized_valid + 1))) else NA_real_
    data.frame(lhs = raw_original$lhs[[column]], op = raw_original$op[[column]], rhs = raw_original$rhs[[column]], estimate = raw_original$est[[column]], se = if (valid > 1L) stats::sd(values) else NA_real_, lower = interval[[1L]], upper = interval[[2L]], p = p_value, beta_estimate = standardized_original_values[[column]], beta_se = if (standardized_valid > 1L) stats::sd(standardized_values) else NA_real_, beta_lower = standardized_interval[[1L]], beta_upper = standardized_interval[[2L]], beta_p = standardized_p, beta_valid = standardized_valid, valid = valid, requested = reps, `valid_percent` = 100 * valid / reps, status = structural_canvas_bootstrap_status(valid, reps), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

structural_canvas_start_effect_bootstrap_job <- function(snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal, residual_variance_fixes, reps = 0L, seed = default_seed()) {
  stopifnot(requireNamespace("callr", quietly = TRUE))
  job_dir <- tempfile("statedu-effect-bootstrap-")
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  input_file <- file.path(job_dir, "input.rds")
  result_file <- file.path(job_dir, "result.rds")
  error_file <- file.path(job_dir, "error.txt")
  saveRDS(
    list(
      snapshot = snapshot, data = data, analysis_type = analysis_type,
      estimator = estimator, missing = missing, std_lv = std_lv,
      ordered = ordered, nominal = nominal,
      residual_variance_fixes = residual_variance_fixes,
      reps = as.integer(reps), seed = as.integer(seed)
    ),
    input_file
  )
  process <- callr::r_bg(
    func = function(input_file, result_file, error_file, project_dir) {
      tryCatch({
        setwd(project_dir)
        source(file.path("R", "app_bootstrap.R"), encoding = "UTF-8")
        load_app_packages()
        source_app_modules()
        args <- readRDS(input_file)
        value <- structural_canvas_effect_bootstrap(
          args$snapshot, args$data, args$analysis_type, args$estimator, args$missing,
          args$std_lv, args$ordered, args$nominal, args$residual_variance_fixes,
          args$reps, args$seed
        )
        saveRDS(value, result_file)
      }, error = function(error) {
        writeLines(conditionMessage(error), error_file, useBytes = TRUE)
        quit(status = 1L, save = "no")
      })
      invisible(TRUE)
    },
    args = list(
      input_file = input_file,
      result_file = result_file,
      error_file = error_file,
      project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)
    ),
    supervise = TRUE
  )
  list(
    process = process, directory = job_dir, result_file = result_file,
    error_file = error_file, started_at = Sys.time(), reps = as.integer(reps)
  )
}

structural_canvas_cleanup_effect_bootstrap_job <- function(job) {
  if (is.null(job)) return(invisible(FALSE))
  directory <- as.character(job$directory %||% "")
  if (nzchar(directory) && dir.exists(directory)) unlink(directory, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}
