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
  original_values <- if (ci_method %in% c("bca", "bias_corrected")) compute_pair_values(values) else rep(NA_real_, length(pairs))
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
    } else if (identical(ci_method, "bias_corrected") && length(pair_values) >= max(20L, ceiling(.5 * reps))) {
      bootstrap_ci(original_values[[pair_index]], pair_values, conf = confidence, method = "bias_corrected")
    } else if (length(pair_values) >= max(20L, ceiling(.5 * reps))) {
      as.numeric(stats::quantile(pair_values, probs = c(alpha, 1 - alpha), names = FALSE, type = 6, na.rm = TRUE))
    } else c(NA_real_, NA_real_)
    upper_one_sided <- if (identical(ci_method, "bca")) {
      structural_canvas_bca_quantile(pair_values, original_values[[pair_index]], jackknife[, pair_index], confidence)
    } else if (identical(ci_method, "bias_corrected") && length(pair_values) >= max(20L, ceiling(.5 * reps))) {
      structural_canvas_bias_corrected_quantile(pair_values, original_values[[pair_index]], confidence)
    } else if (length(pair_values) >= max(20L, ceiling(.5 * reps))) {
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
      } else if (identical(ci_method, "bias_corrected")) "Bias-corrected (BC)" else "Percentile",
      `Quantile type` = paste0("R type ", structural_canvas_bootstrap_quantile_type(ci_method, "htmt")),
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
  if (value %in% c("bc", "bias_corrected", "bias-corrected", "bias corrected", "bias-corrected (bc)", "bias corrected (bc)")) return("bias_corrected")
  "percentile"
}

structural_canvas_bootstrap_quantile_type <- function(ci_method = "percentile", procedure = "structural_effects") {
  ci_method <- structural_canvas_bootstrap_ci_method(ci_method)
  procedure <- tolower(trimws(as.character(procedure %||% "structural_effects")[[1L]]))
  # Preserve the released numerical contracts: structural effects and HTMT,
  # including BC/BCa adjusted probabilities, use R quantile type 6. The
  # reliability/AVE percentile branch predates that shared helper and retains
  # R's default type 7; its BC/BCa branches use type 6.
  if (procedure %in% c("reliability", "ave_reliability") && identical(ci_method, "percentile")) 7L else 6L
}

structural_canvas_bias_corrected_quantile <- function(bootstrap_values, original_value, probability) {
  bootstrap_values <- as.numeric(bootstrap_values)
  bootstrap_values <- bootstrap_values[is.finite(bootstrap_values)]
  probability <- as.numeric(probability)
  if (!length(bootstrap_values) || !is.finite(original_value) || !is.finite(probability) || probability <= 0 || probability >= 1) return(NA_real_)
  prop_less <- mean(bootstrap_values < original_value)
  prop_less <- min(max(prop_less, 0.5 / length(bootstrap_values)), 1 - 0.5 / length(bootstrap_values))
  adjusted_probability <- stats::pnorm(2 * stats::qnorm(prop_less) + stats::qnorm(probability))
  if (!is.finite(adjusted_probability) || adjusted_probability <= 0 || adjusted_probability >= 1) return(NA_real_)
  as.numeric(stats::quantile(bootstrap_values, probs = adjusted_probability, names = FALSE, type = 6, na.rm = TRUE))
}

structural_canvas_bca_interval <- function(bootstrap_values, original_value, jackknife_values, confidence = .95) {
  confidence <- as.numeric(confidence)
  if (!is.finite(confidence) || confidence <= 0 || confidence >= 1) return(c(NA_real_, NA_real_))
  alpha <- (1 - confidence) / 2
  vapply(
    c(alpha, 1 - alpha),
    function(probability) structural_canvas_bca_quantile(bootstrap_values, original_value, jackknife_values, probability),
    numeric(1)
  )
}

structural_canvas_bca_quantile <- function(bootstrap_values, original_value, jackknife_values, probability) {
  bootstrap_values <- as.numeric(bootstrap_values)
  bootstrap_values <- bootstrap_values[is.finite(bootstrap_values)]
  jackknife_values <- as.numeric(jackknife_values)
  jackknife_values <- jackknife_values[is.finite(jackknife_values)]
  probability <- as.numeric(probability)
  if (length(bootstrap_values) < 20L || length(jackknife_values) < 10L ||
      !is.finite(original_value) || !is.finite(probability) || probability <= 0 || probability >= 1) {
    return(NA_real_)
  }
  prop_less <- (sum(bootstrap_values < original_value) + .5) / (length(bootstrap_values) + 1)
  z0 <- stats::qnorm(prop_less)
  jackknife_mean <- mean(jackknife_values)
  jackknife_delta <- jackknife_mean - jackknife_values
  denominator <- 6 * (sum(jackknife_delta^2)^(3 / 2))
  acceleration <- if (is.finite(denominator) && denominator > 0) sum(jackknife_delta^3) / denominator else 0
  z_alpha <- stats::qnorm(probability)
  adjusted_denominator <- 1 - acceleration * (z0 + z_alpha)
  if (!is.finite(adjusted_denominator) || abs(adjusted_denominator) < .Machine$double.eps) return(NA_real_)
  adjusted <- stats::pnorm(z0 + (z0 + z_alpha) / adjusted_denominator)
  if (!is.finite(adjusted) || adjusted <= 0 || adjusted >= 1) return(NA_real_)
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

structural_canvas_effect_bootstrap <- function(snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal, residual_variance_fixes, reps = 0L, seed = default_seed(), ci_method = "bias_corrected", progress = NULL, cancel = NULL, ml_likelihood = "normal") {
  reps <- suppressWarnings(as.integer(reps))
  ci_method <- if (identical(as.character(ci_method %||% "bias_corrected"), "percentile")) "percentile" else "bias_corrected"
  if (!analysis_type %in% c("cbsem", "sem") || !is.data.frame(data) || nrow(data) < 3L || !is.finite(reps) || reps < 2L) return(NULL)
  original <- run_structural_canvas_analysis(snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal, residual_variance_fixes, ml_likelihood)
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
  progress_step <- max(1L, floor(reps / 100L))
  valid_fits <- 0L
  if (is.function(progress)) progress(0L, reps, valid_fits)
  for (index in seq_len(reps)) {
    if (is.function(cancel) && isTRUE(cancel())) stop("Structural-effect bootstrap canceled.")
    sampled <- data[sample.int(nrow(data), nrow(data), replace = TRUE), , drop = FALSE]
    fit <- suppressWarnings(tryCatch(
      run_structural_canvas_analysis(snapshot, sampled, analysis_type, estimator, missing, std_lv, ordered, nominal, residual_variance_fixes, ml_likelihood),
      error = function(error) NULL
    ))
    if (is.null(fit) || !isTRUE(fit$converged) || !isTRUE(fit$admissible)) {
      if (is.function(progress) && (index == 1L || index == reps || index %% progress_step == 0L)) progress(index, reps, valid_fits)
      next
    }
    valid_fits <- valid_fits + 1L
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
    if (is.function(progress) && (index == 1L || index == reps || index %% progress_step == 0L)) progress(index, reps, valid_fits)
  }
  rows <- lapply(seq_along(keys), function(column) {
    values <- draws[, column]
    values <- values[is.finite(values)]
    valid <- length(values)
    interval <- if (valid >= max(20L, ceiling(.5 * reps))) bootstrap_ci(raw_original$est[[column]], values, method = ci_method) else c(NA_real_, NA_real_)
    p_value <- if (valid) min(1, 2 * min((sum(values <= 0) + 1) / (valid + 1), (sum(values >= 0) + 1) / (valid + 1))) else NA_real_
    standardized_values <- standardized_draws[, column]
    standardized_values <- standardized_values[is.finite(standardized_values)]
    standardized_valid <- length(standardized_values)
    standardized_interval <- if (standardized_valid >= max(20L, ceiling(.5 * reps))) bootstrap_ci(standardized_original_values[[column]], standardized_values, method = ci_method) else c(NA_real_, NA_real_)
    standardized_p <- if (standardized_valid) min(1, 2 * min((sum(standardized_values <= 0) + 1) / (standardized_valid + 1), (sum(standardized_values >= 0) + 1) / (standardized_valid + 1))) else NA_real_
    data.frame(lhs = raw_original$lhs[[column]], op = raw_original$op[[column]], rhs = raw_original$rhs[[column]], estimate = raw_original$est[[column]], se = if (valid > 1L) stats::sd(values) else NA_real_, lower = interval[[1L]], upper = interval[[2L]], p = p_value, beta_estimate = standardized_original_values[[column]], beta_se = if (standardized_valid > 1L) stats::sd(standardized_values) else NA_real_, beta_lower = standardized_interval[[1L]], beta_upper = standardized_interval[[2L]], beta_p = standardized_p, beta_valid = standardized_valid, beta_status = if (identical(raw_original$op[[column]], "modmed")) "Not reported: product-indicator index is scale-dependent" else if (standardized_valid) "Estimated" else "Not available", valid = valid, requested = reps, `valid_percent` = 100 * valid / reps, ci_method = ci_method, quantile_type = structural_canvas_bootstrap_quantile_type(ci_method, "structural_effects"), status = structural_canvas_bootstrap_status(valid, reps), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# The interactive SEM effect bootstrap uses a prepared lavaan template rather
# than rebuilding and re-diagnosing the complete canvas model for every draw.
# Product indicators are deliberately not cached: they are reconstructed from
# every resampled data set so that mean centering and double-mean-centering have
# exactly the same sampling semantics as structural_canvas_lavaan_syntax().
structural_canvas_moderated_mediation_bootstrap_specs <- function(result) {
  effects <- result$effect_definitions %||% list()
  moderations <- result$moderation_definitions %||% list()
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
        required <- c(interaction_label, labels[-edge_position])
        if (!nzchar(interaction_label) || !length(required)) next
        rows[[length(rows) + 1L]] <- list(
          lhs = paste(path, collapse = " -> "), op = "modmed",
          rhs = as.character(definition$moderator %||% ""),
          required_labels = required
        )
      }
    }
  }
  if (!length(rows)) return(list())
  keys <- vapply(rows, function(item) paste(item$lhs, item$op, item$rhs, sep = "\r"), character(1))
  rows[!duplicated(keys)]
}

structural_canvas_prepare_effect_bootstrap <- function(
  snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal,
  residual_variance_fixes, ml_likelihood = "normal", original_result = NULL
) {
  started_at <- Sys.time()
  if (is.null(original_result) || is.null(original_result$fit)) {
    original_result <- run_structural_canvas_analysis(
      snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal,
      residual_variance_fixes, ml_likelihood
    )
  }
  raw_original <- lavaan::parameterEstimates(original_result$fit)
  raw_original <- raw_original[
    raw_original$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE
  ]
  moderated_original <- structural_canvas_moderated_mediation_indices(original_result)
  if (nrow(moderated_original)) raw_original <- rbind(raw_original, moderated_original)
  keys <- paste(raw_original$lhs, raw_original$op, raw_original$rhs, sep = "\r")
  standardized_original <- tryCatch(
    lavaan::standardizedSolution(original_result$fit, ci = FALSE),
    error = function(error) data.frame()
  )
  standardized_original_values <- rep(NA_real_, length(keys))
  if (all(c("lhs", "op", "rhs", "est.std") %in% names(standardized_original))) {
    standardized_keys <- paste(
      standardized_original$lhs, standardized_original$op, standardized_original$rhs,
      sep = "\r"
    )
    standardized_match <- match(keys, standardized_keys)
    standardized_original_values[!is.na(standardized_match)] <-
      standardized_original$est.std[standardized_match[!is.na(standardized_match)]]
  }
  product_specs <- lapply(original_result$moderation_definitions %||% list(), function(definition) {
    pairs <- definition$product_indicator_pairs %||% data.frame()
    if (!is.data.frame(pairs) || !all(c("name", "predictor_indicator", "moderator_indicator") %in% names(pairs))) {
      pairs <- data.frame()
    }
    list(
      pairs = pairs,
      method = as.character(definition$product_indicator_method %||% "all_pairs_dmc")
    )
  })
  product_specs <- Filter(function(item) nrow(item$pairs) > 0L, product_specs)
  fit_template <- original_result$fit
  for (option_name in intersect(c("baseline", "h1", "loglik", "implied"), names(fit_template@Options))) {
    fit_template@Options[[option_name]] <- FALSE
  }
  if ("test" %in% names(fit_template@Options)) fit_template@Options$test <- "none"
  list(
    data = data,
    fit_template = fit_template,
    model_df = suppressWarnings(as.numeric(original_result$df %||% tryCatch(lavaan::fitMeasures(original_result$fit, "df")[[1L]], error = function(error) NA_real_))),
    raw_original = raw_original,
    raw_keys = keys,
    standardized_original_values = standardized_original_values,
    moderated_specs = structural_canvas_moderated_mediation_bootstrap_specs(original_result),
    product_specs = product_specs,
    preparation_seconds = as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  )
}

structural_canvas_effect_bootstrap_workers <- function(value = NULL) {
  if (is.null(value)) value <- Sys.getenv("STATEDU_SEM_BOOTSTRAP_WORKERS", "")
  requested <- suppressWarnings(as.integer(value))
  available <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (!is.finite(available) || available < 1L) available <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (!is.finite(available) || available < 1L) available <- 1L
  # Leave capacity for the Shiny/Electron process while allowing large desktop
  # CPUs to shorten the long product-indicator bootstrap materially.
  if (!is.finite(requested) || requested < 1L) requested <- min(12L, max(1L, available - 1L))
  max(1L, min(as.integer(requested), as.integer(available)))
}

# lavaan 0.7-2 validates every newly-created object against the installed
# DESCRIPTION file.  On Windows this means repeated system.file()/read.dcf()
# calls from lavInspect() plus packageDescription() calls while every bootstrap
# fit is assembled.  Concurrent workers can spend substantially more wall time
# waiting on those metadata reads than fitting a small model.
#
# This optimization is deliberately installed only by isolated callr/PSOCK
# bootstrap workers.  It does not alter the Shiny process, model options,
# resamples, estimates, admissibility gates, or CI calculations.  The current
# package version and the two lavaan source contracts must match exactly; any
# mismatch returns an unapplied state and leaves the public fallback untouched.
structural_canvas_lavaan_worker_metadata_fast_path_install <- function() {
  no_op_state <- function(reason) list(
    applied = FALSE, owned = FALSE, reason = as.character(reason),
    restore = function() invisible(FALSE)
  )
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    return(no_op_state("lavaan is unavailable"))
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(no_op_state("digest is unavailable for the lavaan body fingerprint"))
  }
  installed_version <- tryCatch(
    as.character(utils::packageVersion("lavaan")),
    error = function(error) ""
  )
  if (!identical(installed_version, "0.7.2")) {
    return(no_op_state(sprintf("unsupported lavaan version: %s", installed_version)))
  }
  namespace <- asNamespace("lavaan")
  imports <- parent.env(namespace)
  current_check <- tryCatch(
    get("lav_object_check_version", namespace, inherits = FALSE),
    error = function(error) NULL
  )
  current_package_description <- tryCatch(
    get("packageDescription", imports, inherits = FALSE),
    error = function(error) NULL
  )
  if (!is.function(current_check) || !is.function(current_package_description)) {
    return(no_op_state("lavaan metadata functions are unavailable"))
  }
  marker <- "statedu_lavaan_worker_metadata_fast_path_0_7_2"
  state_option <- "statedu.internal.lavaan_worker_metadata_fast_path_state"
  check_lock_state <- bindingIsLocked("lav_object_check_version", namespace)
  description_lock_state <- bindingIsLocked("packageDescription", imports)
  replace_binding <- function(name, envir, value, lock_after) {
    if (bindingIsLocked(name, envir)) unlockBinding(name, envir)
    on.exit({
      active_lock <- bindingIsLocked(name, envir)
      if (isTRUE(lock_after) && !active_lock) lockBinding(name, envir)
      if (!isTRUE(lock_after) && active_lock) unlockBinding(name, envir)
    }, add = TRUE)
    assign(name, value, envir = envir)
    invisible(TRUE)
  }
  release_lease <- function(shared_state) {
    released <- FALSE
    function() {
      if (released) return(invisible(FALSE))
      released <<- TRUE
      if (!is.environment(shared_state) || !isTRUE(shared_state$active)) {
        return(invisible(FALSE))
      }
      leases <- suppressWarnings(as.integer(shared_state$leases))
      if (!is.finite(leases) || leases < 1L) leases <- 1L
      shared_state$leases <- leases - 1L
      if (shared_state$leases > 0L) return(invisible(TRUE))
      active_check <- tryCatch(
        get("lav_object_check_version", namespace, inherits = FALSE),
        error = function(error) NULL
      )
      if (identical(active_check, shared_state$fast_check)) {
        try(replace_binding(
          "lav_object_check_version", namespace, shared_state$original_check,
          shared_state$check_lock_state
        ), silent = TRUE)
      }
      active_description <- tryCatch(
        get("packageDescription", imports, inherits = FALSE),
        error = function(error) NULL
      )
      if (identical(active_description, shared_state$fast_package_description)) {
        try(replace_binding(
          "packageDescription", imports, shared_state$original_package_description,
          shared_state$description_lock_state
        ), silent = TRUE)
      }
      shared_state$active <- FALSE
      if (identical(getOption(state_option), shared_state)) {
        options(structure(list(NULL), names = state_option))
      }
      invisible(TRUE)
    }
  }
  shared_state <- getOption(state_option)
  if (is.environment(shared_state) && isTRUE(shared_state$active) &&
      identical(current_check, shared_state$fast_check) &&
      identical(current_package_description, shared_state$fast_package_description)) {
    leases <- suppressWarnings(as.integer(shared_state$leases))
    if (!is.finite(leases) || leases < 1L) leases <- 1L
    shared_state$leases <- leases + 1L
    return(list(
      applied = TRUE, owned = FALSE, reason = "existing worker-local lease",
      restore = release_lease(shared_state)
    ))
  }
  if (identical(attr(current_check, marker, exact = TRUE), TRUE) ||
      identical(attr(current_package_description, marker, exact = TRUE), TRUE) ||
      !is.null(shared_state)) {
    return(no_op_state("inconsistent pre-existing lavaan metadata fast-path state"))
  }
  step17 <- tryCatch(
    get("lav_step17_lavaan", namespace, inherits = FALSE),
    error = function(error) NULL
  )
  list_builder <- tryCatch(
    get("lavaanList", namespace, inherits = FALSE),
    error = function(error) NULL
  )
  body_digest <- function(fun) tryCatch(
    if (is.function(fun)) {
      digest::digest(body(fun), algo = "sha256", serialize = TRUE)
    } else "",
    error = function(error) ""
  )
  expected_body_digests <- c(
    lav_object_check_version = "edcad4ef5169a36c8dbfc0bbafcea87218cef760d6f230e0add34066119761b8",
    lav_step17_lavaan = "3c1428c1f82c453cbc342f464c0a3fee4b0f337d1e48f567bac9a08ff212eff8",
    lavaanList = "a613bda6aeef261393a4e52d05492f47fc7b774967412e463d2f8d6fdaaeeebe"
  )
  actual_body_digests <- c(
    lav_object_check_version = body_digest(current_check),
    lav_step17_lavaan = body_digest(step17),
    lavaanList = body_digest(list_builder)
  )
  if (!identical(actual_body_digests, expected_body_digests)) {
    return(no_op_state("lavaan metadata body fingerprint changed"))
  }
  cached_object_version <- tryCatch(
    as.character(current_package_description("lavaan", fields = "Version")),
    error = function(error) ""
  )
  normalized_cached_version <- tryCatch(
    as.character(package_version(cached_object_version)),
    error = function(error) ""
  )
  if (!identical(normalized_cached_version, installed_version)) {
    return(no_op_state("lavaan package and DESCRIPTION versions differ"))
  }
  original_check <- current_check
  original_package_description <- current_package_description
  fast_check <- local({
    expected_version <- cached_object_version
    fallback <- original_check
    function(object = NULL) {
      object_version <- tryCatch({
        supported_object <- inherits(object, "lavaan") || inherits(object, "lavaanList")
        if (supported_object && methods::.hasSlot(object, "version")) {
          as.character(methods::slot(object, "version")[[1L]])
        } else NA_character_
      }, error = function(error) NA_character_)
      if (length(object_version) == 1L && !is.na(object_version) &&
          identical(object_version, expected_version)) {
        return(object)
      }
      fallback(object)
    }
  })
  fast_package_description <- local({
    expected_version <- cached_object_version
    fallback <- original_package_description
    function(pkg, lib.loc = NULL, fields = NULL, drop = TRUE, encoding = "") {
      if (identical(pkg, "lavaan") && is.null(lib.loc) &&
          identical(fields, "Version") && isTRUE(drop)) {
        return(expected_version)
      }
      fallback(
        pkg, lib.loc = lib.loc, fields = fields, drop = drop,
        encoding = encoding
      )
    }
  })
  attr(fast_check, marker) <- TRUE
  attr(fast_package_description, marker) <- TRUE
  check_applied <- FALSE
  description_applied <- FALSE
  restore_partial <- function() {
    if (check_applied) try(replace_binding(
      "lav_object_check_version", namespace, original_check, check_lock_state
    ), silent = TRUE)
    if (description_applied) try(replace_binding(
      "packageDescription", imports, original_package_description,
      description_lock_state
    ), silent = TRUE)
    invisible(TRUE)
  }
  installation_error <- NULL
  installed <- tryCatch({
    replace_binding(
      "lav_object_check_version", namespace, fast_check, check_lock_state
    )
    check_applied <- TRUE
    replace_binding(
      "packageDescription", imports, fast_package_description,
      description_lock_state
    )
    description_applied <- TRUE
    TRUE
  }, error = function(error) {
    installation_error <<- conditionMessage(error)
    FALSE
  })
  if (!isTRUE(installed)) {
    restore_partial()
    error_text <- if (is.null(installation_error)) "unknown error" else installation_error
    return(no_op_state(sprintf("lavaan metadata fast path was not installed: %s", error_text)))
  }
  shared_state <- new.env(parent = emptyenv())
  shared_state$active <- TRUE
  shared_state$leases <- 1L
  shared_state$original_check <- original_check
  shared_state$original_package_description <- original_package_description
  shared_state$fast_check <- fast_check
  shared_state$fast_package_description <- fast_package_description
  shared_state$check_lock_state <- check_lock_state
  shared_state$description_lock_state <- description_lock_state
  state_registered <- tryCatch({
    options(structure(list(shared_state), names = state_option))
    identical(getOption(state_option), shared_state)
  }, error = function(error) FALSE)
  if (!isTRUE(state_registered)) {
    shared_state$active <- FALSE
    restore_partial()
    return(no_op_state("lavaan metadata lease state could not be registered"))
  }
  list(
    applied = TRUE, owned = TRUE, reason = "lavaan 0.7-2 metadata contract",
    restore = release_lease(shared_state)
  )
}

structural_canvas_isolated_lavaan_bootstrap_fast_path_enabled <- function() {
  isTRUE(getOption("statedu.isolated_lavaan_bootstrap_worker", FALSE))
}

structural_canvas_effect_bootstrap_resample_data <- function(data, indices, product_specs) {
  sampled <- data[indices, , drop = FALSE]
  for (specification in product_specs) {
    pairs <- specification$pairs
    double_mean_center <- specification$method %in% c("all_pairs_dmc", "matched_pair_dmc")
    for (pair_index in seq_len(nrow(pairs))) {
      predictor <- as.character(pairs$predictor_indicator[[pair_index]])
      moderator <- as.character(pairs$moderator_indicator[[pair_index]])
      product_name <- as.character(pairs$name[[pair_index]])
      predictor_values <- sampled[[predictor]]
      moderator_values <- sampled[[moderator]]
      product_values <-
        (predictor_values - mean(predictor_values, na.rm = TRUE)) *
        (moderator_values - mean(moderator_values, na.rm = TRUE))
      if (double_mean_center) product_values <- product_values - mean(product_values, na.rm = TRUE)
      sampled[[product_name]] <- product_values
    }
  }
  sampled
}

# A no-SE screen is allowed to discard a draw only when the guarded lavaan
# 0.7-2 fused gate explicitly completed and rejected it. Missing/short
# lavaanList funList entries, malformed callback values, and every fallback or
# error path are deliberately fail-open so the unchanged full-SE fit remains
# the final scientific authority.
structural_canvas_effect_bootstrap_screen_explicit_reject <- function(item) {
  if (!is.list(item) || !isFALSE(item$valid)) return(FALSE)
  path <- tryCatch(as.character(item$screening_path), error = function(error) character(0))
  length(path) == 1L && !is.na(path) && identical(path, "fused_0_7_2_screen")
}

# Execute one no-SE lavaanList screen as a fail-open operation. A call-level
# error, a wrong return class, an inaccessible/malformed funList slot, or a
# short result is never evidence that a resample is inadmissible. The caller
# receives one unknown (NULL) item per requested draw and must run the unchanged
# full-SE path for every such item. Full-SE/legacy calls deliberately do not use
# this helper and therefore remain authoritative failures rather than silently
# accepting an incomplete result.
structural_canvas_effect_bootstrap_screen_call <- function(call, expected) {
  expected <- suppressWarnings(as.integer(expected))
  if (!is.function(call) || !is.finite(expected) || expected < 0L) {
    stop("SEM no-SE screening requires a callable and a non-negative expected result count.")
  }
  unknown <- rep(list(NULL), expected)
  fail_open <- function(reason) list(
    values = unknown, complete = FALSE, reason = as.character(reason %||% "screen call failed")
  )
  fit_list <- tryCatch(call(), error = function(error) error)
  if (inherits(fit_list, "error")) return(fail_open(conditionMessage(fit_list)))
  if (!inherits(fit_list, "lavaanList")) return(fail_open("wrong lavaanList result class"))
  slot_names <- tryCatch(methods::slotNames(fit_list), error = function(error) character(0))
  if (!"funList" %in% slot_names) return(fail_open("lavaanList funList slot unavailable"))
  values <- tryCatch(methods::slot(fit_list, "funList"), error = function(error) error)
  if (inherits(values, "error") || !is.list(values)) {
    return(fail_open("lavaanList funList slot unreadable"))
  }
  if (length(values) < expected) length(values) <- expected
  if (length(values) > expected) values <- values[seq_len(expected)]
  list(values = values, complete = TRUE, reason = "")
}

structural_canvas_effect_bootstrap_lavaan_seed <- function(seed, position = 0L) {
  seed_value <- suppressWarnings(as.numeric(seed)[[1L]])
  position_value <- suppressWarnings(as.numeric(position)[[1L]])
  if (!is.finite(seed_value)) seed_value <- 1
  if (!is.finite(position_value)) position_value <- 0
  value <- (abs(seed_value) + max(0, position_value)) %% .Machine$integer.max
  if (!is.finite(value) || value < 1) value <- 1
  as.integer(value)
}

# This callback is intentionally self-contained because lavaanList serializes it
# to its reusable PSOCK workers. It applies the same admissibility gates used by
# structural_canvas_fit_admissibility(), then extracts aligned raw and
# standardized effects from the fitted lavaan object. `screen_only` stops after
# the fit/post/theta/cov.lv/latent-correlation/df gates.  It is used only for a
# guarded no-SE first pass; a screen pass can nominate a draw for the unchanged
# full-SE refit but can never supply a reported estimate. A guarded public-API
# fallback preserves compatibility if a future lavaan version changes slots.
structural_canvas_effect_bootstrap_extract_fit <- function(
  fit, raw_keys, moderated_specs, model_df, screen_only = FALSE
) {
  result <- tryCatch({
    lavaan_namespace <- asNamespace("lavaan")
    internal_function <- function(name) tryCatch(
      get(name, envir = lavaan_namespace, inherits = FALSE),
      error = function(error) NULL
    )
    theta_inspector <- internal_function("lav_inspect_theta")
    latent_inspector <- internal_function("lav_inspect_cov_lv")
    vcov_inspector <- internal_function("lav_inspect_vcov")
    post_checker <- internal_function("lav_object_post_check")
    object_vnames <- internal_function("lav_object_vnames")
    as_matrix_list <- function(value) {
      if (is.list(value) && !is.matrix(value)) lapply(value, as.matrix) else list(as.matrix(value))
    }
    matrix_status <- function(values, floor_scale = TRUE) {
      statuses <- lapply(values, function(value) {
        eigenvalues <- if (length(value) && nrow(value) == ncol(value) && all(is.finite(value))) {
          tryCatch(eigen((value + t(value)) / 2, symmetric = TRUE, only.values = TRUE)$values, error = function(error) numeric(0))
        } else numeric(0)
        minimum <- if (length(eigenvalues)) min(eigenvalues) else NA_real_
        scale <- if (length(value)) suppressWarnings(max(abs(diag(value)), na.rm = TRUE)) else NA_real_
        tolerance <- if (is.finite(scale)) sqrt(.Machine$double.eps) * if (floor_scale) max(1, scale) else scale else NA_real_
        list(
          eigenvalues = eigenvalues,
          non_psd = is.finite(minimum) && is.finite(tolerance) && minimum < -tolerance,
          boundary = is.finite(minimum) && is.finite(tolerance) && minimum >= -tolerance && minimum <= tolerance,
          boundary_count = if (length(eigenvalues) && is.finite(tolerance)) sum(abs(eigenvalues) <= tolerance) else 0L
        )
      })
      list(
        items = statuses,
        non_psd = any(vapply(statuses, function(item) item$non_psd, logical(1))),
        boundary = any(vapply(statuses, function(item) item$boundary, logical(1))),
        boundary_count = sum(vapply(statuses, function(item) as.integer(item$boundary_count), integer(1)))
      )
    }
    converged <- if (inherits(fit, "lavaan") && is.list(fit@optim)) {
      isTRUE(fit@optim$converged)
    } else isTRUE(lavaan::lavInspect(fit, "converged"))
    negative_diagonal <- function(values) any(vapply(values, function(value) {
      length(value) && any(diag(value) < 0, na.rm = TRUE)
    }, logical(1)))
    invalid_latent_correlations <- function(values) any(vapply(values, function(value) {
      length(value) > 1L && any(abs(value[row(value) != col(value)]) >= 1, na.rm = TRUE)
    }, logical(1)))

    # lavaan 0.7-2's public post.check repeats cov.lv/theta extraction and their
    # eigen decompositions.  The exact source contract is stable in the bundled
    # runtime, so fuse that check with the stricter StatEdu matrix gate and
    # reject an invalid draw before inspecting vcov or standardizing it.  A
    # future lavaan version, multilevel object, changed slot contract, or any
    # internal error falls back to the unchanged public-compatible path below.
    parameter_table <- fit@ParTable
    object_version <- tryCatch(as.character(fit@version[[1L]]), error = function(error) "")
    n_groups <- tryCatch(as.integer(fit@Data@ngroups), error = function(error) NA_integer_)
    n_levels <- tryCatch(as.integer(fit@Data@nlevels), error = function(error) NA_integer_)
    fused_contract <- identical(object_version, "0.7-2") &&
      is.function(theta_inspector) && is.function(latent_inspector) &&
      is.function(vcov_inspector) && is.function(object_vnames) &&
      is.list(parameter_table) && all(c("lhs", "op", "rhs", "est") %in% names(parameter_table)) &&
      is.finite(n_groups) && n_groups >= 1L && identical(n_levels, 1L) &&
      is.list(fit@Model@num.idx) && length(fit@Model@num.idx) >= n_groups
    fused_gate <- if (fused_contract) tryCatch({
      theta <- as_matrix_list(theta_inspector(
        fit, correlation_metric = FALSE, add_labels = FALSE,
        add_class = FALSE, drop_list_single_group = TRUE
      ))
      latent_covariance <- as_matrix_list(latent_inspector(
        fit, correlation_metric = FALSE, add_labels = FALSE,
        add_class = FALSE, drop_list_single_group = TRUE
      ))
      if (length(theta) < n_groups || length(latent_covariance) < n_groups) {
        stop("lavaan block contract changed")
      }
      theta_status <- matrix_status(theta)
      latent_status <- matrix_status(latent_covariance)

      # Exact, warning-free reproduction of lavaan 0.7-2
      # lav_object_post_check(). Passing ParTable (rather than fit) to
      # lav_object_vnames avoids the public object-version metadata check.
      observed_names <- object_vnames(parameter_table, type = "ov")
      latent_names <- object_vnames(parameter_table, type = "lv")
      regular_latent_names <- object_vnames(parameter_table, type = "lv.regular")
      observed_variance_indices <- which(
        parameter_table$op == "~~" & parameter_table$lhs %in% observed_names &
          parameter_table$lhs == parameter_table$rhs
      )
      latent_variance_indices <- which(
        parameter_table$op == "~~" & parameter_table$lhs %in% latent_names &
          parameter_table$lhs == parameter_table$rhs
      )
      variance_na <- FALSE
      observed_variance_ok <- TRUE
      latent_variance_ok <- TRUE
      post_check <- TRUE
      if (any(is.na(parameter_table$est[observed_variance_indices]))) {
        variance_na <- TRUE
      } else if (length(observed_variance_indices) &&
                 any(parameter_table$est[observed_variance_indices] < 0)) {
        observed_variance_ok <- FALSE
        post_check <- FALSE
      }
      if (any(is.na(parameter_table$est[latent_variance_indices]))) {
        variance_na <- TRUE
      } else if (length(latent_variance_indices) &&
                 any(parameter_table$est[latent_variance_indices] < 0)) {
        latent_variance_ok <- FALSE
        post_check <- FALSE
      }
      post_tolerance <- .Machine$double.eps^(3 / 4)
      if (!variance_na && latent_variance_ok && length(regular_latent_names)) {
        for (group_index in seq_len(n_groups)) {
          eigenvalues <- latent_status$items[[group_index]]$eigenvalues
          if (length(eigenvalues) && any(eigenvalues < -post_tolerance)) post_check <- FALSE
        }
      }
      if (!variance_na && observed_variance_ok) {
        for (group_index in seq_len(n_groups)) {
          numeric_indices <- fit@Model@num.idx[[group_index]]
          if (!length(numeric_indices)) next
          eigenvalues <- if (identical(numeric_indices, seq_len(nrow(theta[[group_index]])))) {
            theta_status$items[[group_index]]$eigenvalues
          } else {
            eigen(
              theta[[group_index]][numeric_indices, numeric_indices, drop = FALSE],
              symmetric = TRUE, only.values = TRUE
            )$values
          }
          if (any(eigenvalues < -post_tolerance)) post_check <- FALSE
        }
      }
      latent_correlations <- lapply(latent_covariance, function(value) {
        if (nrow(value) > 1L) suppressWarnings(stats::cov2cor(value)) else value
      })
      early_admissible <- converged && post_check && is.finite(model_df) && model_df >= 0 &&
        !negative_diagonal(theta) && !negative_diagonal(latent_covariance) &&
        !theta_status$non_psd && !theta_status$boundary &&
        !latent_status$non_psd && !latent_status$boundary &&
        !invalid_latent_correlations(latent_correlations)
      if (!early_admissible) {
        list(admissible = FALSE)
      } else if (isTRUE(screen_only)) {
        list(admissible = TRUE)
      } else {
        parameter_covariance <- tryCatch(
          as_matrix_list(vcov_inspector(
            fit, standardized = FALSE, free_only = TRUE,
            add_labels = FALSE, add_class = FALSE
          )),
          error = function(error) list(matrix(numeric(0), 0L, 0L))
        )
        parameter_status <- matrix_status(parameter_covariance, floor_scale = FALSE)
        equality_constraint_count <- sum(parameter_table$op == "==")
        list(admissible = !parameter_status$non_psd &&
          parameter_status$boundary_count <= equality_constraint_count)
      }
    }, error = function(error) NULL) else NULL

    fused_gate_used <- is.list(fused_gate) && length(fused_gate$admissible) == 1L
    screening_path <- if (fused_gate_used) "fused_0_7_2" else "public_fallback"
    if (isTRUE(screen_only)) {
      # Screening is fail-open when the exact fused contract is unavailable:
      # every such draw proceeds to the legacy full-SE fit and strict gate.
      return(list(
        valid = if (fused_gate_used) isTRUE(fused_gate$admissible) else TRUE,
        raw = NULL, standardized = NULL,
        screening_path = if (fused_gate_used) "fused_0_7_2_screen" else "screen_fail_open"
      ))
    }
    if (fused_gate_used) {
      admissible <- isTRUE(fused_gate$admissible)
    } else {
      post_check <- if (is.function(post_checker)) {
        isTRUE(post_checker(fit))
      } else isTRUE(lavaan::lavInspect(fit, "post.check"))
      theta <- as_matrix_list(if (is.function(theta_inspector)) {
        theta_inspector(
          fit, correlation_metric = FALSE, add_labels = FALSE,
          add_class = FALSE, drop_list_single_group = TRUE
        )
      } else lavaan::lavInspect(fit, "theta"))
      latent_covariance <- as_matrix_list(if (is.function(latent_inspector)) {
        latent_inspector(
          fit, correlation_metric = FALSE, add_labels = FALSE,
          add_class = FALSE, drop_list_single_group = TRUE
        )
      } else lavaan::lavInspect(fit, "cov.lv"))
      parameter_covariance <- tryCatch(
        as_matrix_list(if (is.function(vcov_inspector)) {
          vcov_inspector(
            fit, standardized = FALSE, free_only = TRUE,
            add_labels = FALSE, add_class = FALSE
          )
        } else lavaan::lavInspect(fit, "vcov")),
        error = function(error) list(matrix(numeric(0), 0L, 0L))
      )
      theta_status <- matrix_status(theta)
      latent_status <- matrix_status(latent_covariance)
      parameter_status <- matrix_status(parameter_covariance, floor_scale = FALSE)
      equality_constraint_count <- sum(parameter_table$op == "==")
      latent_correlations <- as_matrix_list(if (is.function(latent_inspector)) {
        latent_inspector(
          fit, correlation_metric = TRUE, add_labels = FALSE,
          add_class = FALSE, drop_list_single_group = TRUE
        )
      } else lavaan::lavInspect(fit, "cor.lv"))
      admissible <- converged && post_check && is.finite(model_df) && model_df >= 0 &&
        !negative_diagonal(theta) && !negative_diagonal(latent_covariance) &&
        !theta_status$non_psd && !theta_status$boundary &&
        !latent_status$non_psd && !latent_status$boundary &&
        !parameter_status$non_psd && parameter_status$boundary_count <= equality_constraint_count &&
        !invalid_latent_correlations(latent_correlations)
    }
    if (!admissible) return(list(
      valid = FALSE, raw = NULL, standardized = NULL,
      screening_path = screening_path
    ))
    # parameterEstimates(standardized = TRUE) performs repeated public-object
    # version and package-description checks.  The bootstrap worker has already
    # produced a current lavaan object, and both vectors below are aligned to
    # fit@ParTable.  Fall back to the public API if a future lavaan version no
    # longer satisfies that alignment contract.
    standardize_all <- internal_function("lav_standardize_all")
    standardized_all <- if (is.function(standardize_all)) {
      tryCatch(as.numeric(standardize_all(fit)), error = function(error) numeric(0))
    } else numeric(0)
    slot_contract <- is.list(parameter_table) &&
      all(c("lhs", "op", "rhs", "label", "est") %in% names(parameter_table)) &&
      length(parameter_table$lhs) == length(parameter_table$est) &&
      length(standardized_all) == length(parameter_table$est)
    if (slot_contract) {
      estimates <- list(
        lhs = as.character(parameter_table$lhs),
        op = as.character(parameter_table$op),
        rhs = as.character(parameter_table$rhs),
        label = as.character(parameter_table$label),
        est = as.numeric(parameter_table$est),
        std.all = standardized_all
      )
    } else {
      estimates <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = FALSE)
    }
    estimate_keys <- paste(estimates$lhs, estimates$op, estimates$rhs, sep = "\r")
    raw <- rep(NA_real_, length(raw_keys))
    standardized <- rep(NA_real_, length(raw_keys))
    matched <- match(raw_keys, estimate_keys)
    raw[!is.na(matched)] <- estimates$est[matched[!is.na(matched)]]
    if ("std.all" %in% names(estimates)) {
      standardized[!is.na(matched)] <- estimates$std.all[matched[!is.na(matched)]]
    }
    if (length(moderated_specs)) {
      labeled_rows <- nzchar(as.character(estimates$label))
      coefficients <- stats::setNames(as.numeric(estimates$est[labeled_rows]), as.character(estimates$label[labeled_rows]))
      for (specification in moderated_specs) {
        key <- paste(specification$lhs, specification$op, specification$rhs, sep = "\r")
        position <- match(key, raw_keys)
        required <- specification$required_labels
        if (!is.na(position) && length(required) && all(required %in% names(coefficients))) {
          values <- unname(coefficients[required])
          if (all(is.finite(values))) raw[[position]] <- prod(values)
        }
      }
    }
    list(
      valid = TRUE, raw = raw, standardized = standardized,
      screening_path = screening_path
    )
  }, error = function(error) NULL)
  if (is.null(result)) {
    list(
      valid = isTRUE(screen_only), raw = NULL, standardized = NULL,
      screening_path = if (isTRUE(screen_only)) "screen_error_fail_open" else "error"
    )
  } else result
}

structural_canvas_effect_bootstrap_worker_cleanup <- function() {
  if (exists(
    ".statedu_sem_fixed_index_bootstrap_context",
    envir = .GlobalEnv, inherits = FALSE
  )) {
    rm(".statedu_sem_fixed_index_bootstrap_context", envir = .GlobalEnv)
  }
  if (exists(
    ".statedu_lavaan_metadata_fast_path_state",
    envir = .GlobalEnv, inherits = FALSE
  )) {
    state <- get(
      ".statedu_lavaan_metadata_fast_path_state",
      envir = .GlobalEnv, inherits = FALSE
    )
    if (is.function(state$restore)) state$restore()
    rm(".statedu_lavaan_metadata_fast_path_state", envir = .GlobalEnv)
  }
  TRUE
}

structural_canvas_effect_bootstrap_worker_install_metadata <- function(install) {
  state <- install()
  assign(".statedu_lavaan_metadata_fast_path_state", state, envir = .GlobalEnv)
  list(applied = isTRUE(state$applied), reason = as.character(state$reason))
}

structural_canvas_effect_bootstrap_worker_install_context <- function(context) {
  assign(".statedu_sem_fixed_index_bootstrap_context", context, envir = .GlobalEnv)
  list(installed = TRUE, rows = nrow(context$data), columns = ncol(context$data))
}

structural_canvas_effect_bootstrap_fixed_index_worker <- function(block) {
  context <- get(
    ".statedu_sem_fixed_index_bootstrap_context",
    envir = .GlobalEnv, inherits = FALSE
  )
  failure_mode <- if (is.null(context$test_failure) || !length(context$test_failure)) {
    ""
  } else {
    as.character(context$test_failure[[1L]])
  }
  if (identical(failure_mode, "block")) {
    return(list(
      positions = block$positions, items = NULL, failed = TRUE,
      error = "injected fixed-index worker failure"
    ))
  }
  block_mode <- tryCatch(as.character(block$mode), error = function(error) character(0))
  if (length(block_mode) != 1L || is.na(block_mode) ||
      !block_mode %in% c("screen", "full")) {
    block_mode <- "full"
  }
  screen_only <- identical(block_mode, "screen")
  fit_options <- if (screen_only) context$screen_options else context$options
  if (!is.list(fit_options)) {
    return(list(
      positions = block$positions, items = NULL, failed = TRUE,
      error = sprintf("fixed-index %s options are unavailable", block_mode)
    ))
  }
  items <- vector("list", ncol(block$indices))
  for (column in seq_len(ncol(block$indices))) {
    frame <- context$data[block$indices[, column], , drop = FALSE]
    for (specification in context$product_specs) {
      pairs <- specification$pairs
      double_mean_center <- specification$method %in% c(
        "all_pairs_dmc", "matched_pair_dmc"
      )
      for (pair_index in seq_len(nrow(pairs))) {
        predictor <- as.character(pairs$predictor_indicator[[pair_index]])
        moderator <- as.character(pairs$moderator_indicator[[pair_index]])
        product_name <- as.character(pairs$name[[pair_index]])
        predictor_values <- frame[[predictor]]
        moderator_values <- frame[[moderator]]
        product_values <-
          (predictor_values - mean(predictor_values, na.rm = TRUE)) *
          (moderator_values - mean(moderator_values, na.rm = TRUE))
        if (double_mean_center) {
          product_values <- product_values - mean(product_values, na.rm = TRUE)
        }
        frame[[product_name]] <- product_values
      }
    }
    candidate <- if (identical(failure_mode, "item") && column == 1L) {
      simpleError("injected fixed-index item failure")
    } else {
      suppressWarnings(tryCatch(
        lavaan::lavaan(
          slot_options = fit_options,
          slot_par_table = context$partable,
          data = frame
        ),
        error = function(error) error
      ))
    }
    if (inherits(candidate, "error") || !inherits(candidate, "lavaan")) {
      return(list(
        positions = block$positions, items = NULL, failed = TRUE,
        error = if (inherits(candidate, "error")) {
          conditionMessage(candidate)
        } else "fixed-index fit returned a non-lavaan object"
      ))
    }
    item <- context$extract_fit(
      candidate, context$raw_keys, context$moderated_specs, context$model_df,
      screen_only = screen_only
    )
    if (!is.list(item) || length(item$valid) != 1L) {
      return(list(
        positions = block$positions, items = NULL, failed = TRUE,
        error = "fixed-index extractor returned a malformed item"
      ))
    }
    items[[column]] <- item
  }
  list(positions = block$positions, items = items, failed = FALSE, error = "")
}

structural_canvas_effect_bootstrap_prepared <- function(
  prepared, reps = 0L, seed = default_seed(), ci_method = "bias_corrected",
  progress = NULL, cancel = NULL, workers = NULL, chunk_size = NULL,
  phase = NULL, return_draws = FALSE
) {
  reps <- suppressWarnings(as.integer(reps))
  ci_method <- if (identical(as.character(ci_method %||% "bias_corrected"), "percentile")) "percentile" else "bias_corrected"
  if (!is.list(prepared) || !is.data.frame(prepared$data) || nrow(prepared$data) < 3L ||
      !is.finite(reps) || reps < 2L || is.null(prepared$fit_template)) return(NULL)
  workers <- structural_canvas_effect_bootstrap_workers(workers)
  if (is.null(chunk_size)) chunk_size <- max(workers * 4L, min(250L, ceiling(reps / 20L)))
  chunk_size <- max(workers, suppressWarnings(as.integer(chunk_size)))
  keys <- prepared$raw_keys
  draws <- matrix(NA_real_, nrow = reps, ncol = length(keys), dimnames = list(NULL, keys))
  standardized_draws <- matrix(NA_real_, nrow = reps, ncol = length(keys), dimnames = list(NULL, keys))
  fit_valid_mask <- rep(FALSE, reps)
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  sample_indices <- replicate(
    reps, sample.int(nrow(prepared$data), nrow(prepared$data), replace = TRUE),
    simplify = FALSE
  )
  metadata_fast_path_enabled <- structural_canvas_isolated_lavaan_bootstrap_fast_path_enabled()
  metadata_fast_path_state <- if (metadata_fast_path_enabled) {
    structural_canvas_lavaan_worker_metadata_fast_path_install()
  } else {
    list(applied = FALSE, owned = FALSE, reason = "not an isolated bootstrap worker", restore = function() invisible(FALSE))
  }
  on.exit(try(metadata_fast_path_state$restore(), silent = TRUE), add = TRUE)
  cluster <- NULL
  cluster_metadata_fast_path <- list()
  worker_startup_started <- Sys.time()
  if (workers > 1L) {
    if (is.function(phase)) phase("starting_workers", 0L, reps, 0L, workers)
    cluster <- parallel::makePSOCKcluster(rep("localhost", workers))
    on.exit({
      # Restore each worker namespace while its process is still reachable,
      # then stop the cluster. Keep both cleanup attempts independent.
      try(parallel::clusterCall(
        cluster, structural_canvas_effect_bootstrap_worker_cleanup
      ), silent = TRUE)
      try(parallel::stopCluster(cluster), silent = TRUE)
    }, add = TRUE)
    parallel::clusterEvalQ(cluster, suppressPackageStartupMessages(requireNamespace("lavaan", quietly = TRUE)))
    if (metadata_fast_path_enabled) {
      installer <- structural_canvas_lavaan_worker_metadata_fast_path_install
      cluster_metadata_fast_path <- parallel::clusterCall(
        cluster, structural_canvas_effect_bootstrap_worker_install_metadata,
        installer
      )
    }
  }
  worker_startup_seconds <- as.numeric(difftime(Sys.time(), worker_startup_started, units = "secs"))
  all_worker_fast_paths_applied <- workers <= 1L || (
    length(cluster_metadata_fast_path) == workers &&
      all(vapply(cluster_metadata_fast_path, function(item) isTRUE(item$applied), logical(1)))
  )
  fit_template <- prepared$fit_template
  template_version <- tryCatch(as.character(fit_template@version[[1L]]), error = function(error) "")
  template_se <- tryCatch(as.character(fit_template@Options$se[[1L]]), error = function(error) "")
  template_categorical <- tryCatch(isTRUE(fit_template@Model@categorical), error = function(error) TRUE)
  template_groups <- tryCatch(as.integer(fit_template@Data@ngroups), error = function(error) NA_integer_)
  template_levels <- tryCatch(as.integer(fit_template@Data@nlevels), error = function(error) NA_integer_)
  template_random_starts <- tryCatch(
    suppressWarnings(as.integer(fit_template@Options$rstarts %||% 0L)),
    error = function(error) NA_integer_
  )
  two_stage_supported <- isTRUE(metadata_fast_path_state$applied) &&
    all_worker_fast_paths_applied && identical(template_version, "0.7-2") &&
    identical(template_se, "standard") && !template_categorical &&
    identical(template_groups, 1L) && identical(template_levels, 1L) &&
    is.finite(template_random_starts) && template_random_starts == 0L &&
    !isTRUE(getOption("statedu.internal.disable_sem_bootstrap_two_stage", FALSE))
  screen_template <- NULL
  if (two_stage_supported) {
    screen_template <- fit_template
    screen_template@Options$se <- "none"
    if ("se.def" %in% names(screen_template@Options)) screen_template@Options$se.def <- "none"
  }
  two_stage_active <- two_stage_supported && length(prepared$product_specs %||% list()) > 0L
  template_estimator <- tryCatch(
    toupper(as.character(fit_template@Options$estimator[[1L]])),
    error = function(error) ""
  )
  template_likelihood <- tryCatch(
    tolower(as.character(fit_template@Options$likelihood[[1L]])),
    error = function(error) ""
  )
  template_missing <- tryCatch(
    tolower(as.character(fit_template@Options$missing[[1L]])),
    error = function(error) ""
  )
  template_meanstructure <- tryCatch(
    isTRUE(fit_template@Options$meanstructure),
    error = function(error) TRUE
  )
  fixed_index_common_supported <- workers > 1L && isTRUE(metadata_fast_path_state$applied) &&
    all_worker_fast_paths_applied && identical(template_version, "0.7-2") &&
    identical(template_se, "standard") && identical(template_estimator, "ML") &&
    template_likelihood %in% c("normal", "wishart") && !template_categorical &&
    identical(template_groups, 1L) && identical(template_levels, 1L) &&
    is.finite(template_random_starts) && template_random_starts == 0L &&
    all(vapply(prepared$data, is.numeric, logical(1))) &&
    !isTRUE(getOption("statedu.internal.disable_sem_bootstrap_fixed_index", FALSE))
  fixed_index_product_aware <- fixed_index_common_supported &&
    length(prepared$product_specs %||% list()) > 0L &&
    template_missing %in% c("listwise", "ml", "fiml") &&
    !anyNA(prepared$data)
  fixed_index_nonproduct <- fixed_index_common_supported &&
    length(prepared$product_specs %||% list()) == 0L &&
    identical(template_missing, "listwise") && !template_meanstructure &&
    !anyNA(prepared$data)
  fixed_index_supported <- fixed_index_product_aware || fixed_index_nonproduct
  fixed_index_active <- FALSE
  fixed_index_worker_context <- list()
  fixed_index_seconds <- 0
  fixed_index_blocks <- 0L
  fixed_index_fallbacks <- 0L
  fixed_index_screen_seconds <- 0
  fixed_index_full_seconds <- 0
  fixed_index_screen_blocks <- 0L
  fixed_index_full_blocks <- 0L
  fixed_index_screen_fallbacks <- 0L
  fixed_index_full_fallbacks <- 0L
  if (fixed_index_supported) {
    fixed_partable <- fit_template@ParTable
    fixed_partable$start <- fixed_partable$est <- fixed_partable$se <- NULL
    fixed_options <- fit_template@Options
    fixed_options$fit.by.level <- FALSE
    fixed_screen_options <- fixed_options
    fixed_screen_options$se <- "none"
    if ("se.def" %in% names(fixed_screen_options)) {
      fixed_screen_options$se.def <- "none"
    }
    fixed_failure_mode <- tryCatch(
      as.character(getOption(
        "statedu.internal.sem_bootstrap_fixed_index_test_failure", ""
      ))[[1L]],
      error = function(error) ""
    )
    if (!fixed_failure_mode %in% c("block", "item")) fixed_failure_mode <- ""
    fixed_context <- list(
      data = prepared$data,
      options = fixed_options,
      screen_options = fixed_screen_options,
      partable = fixed_partable,
      product_specs = prepared$product_specs %||% list(),
      extract_fit = structural_canvas_effect_bootstrap_extract_fit,
      raw_keys = prepared$raw_keys,
      moderated_specs = prepared$moderated_specs,
      model_df = prepared$model_df,
      test_failure = fixed_failure_mode
    )
    fixed_index_worker_context <- tryCatch(
      parallel::clusterCall(
        cluster, structural_canvas_effect_bootstrap_worker_install_context,
        fixed_context
      ),
      error = function(error) list()
    )
    fixed_index_active <- length(fixed_index_worker_context) == workers &&
      all(vapply(fixed_index_worker_context, function(item) isTRUE(item$installed), logical(1)))
    rm(fixed_context)
  }
  two_stage_ever_used <- FALSE
  two_stage_screened <- 0L
  two_stage_rejected <- 0L
  two_stage_refit <- 0L
  two_stage_screening_seconds <- 0
  two_stage_refit_seconds <- 0
  legacy_fit_seconds <- 0
  chunk_seconds <- numeric(0)
  screen_candidate_ratios <- numeric(0)
  pending_refit_positions <- integer(0)
  pending_refit_data <- list()
  refit_batches <- list()
  resampling_started <- Sys.time()
  valid_fits <- 0L
  if (is.function(progress)) progress(0L, reps, valid_fits)
  # Use one short warm-up chunk so the UI leaves "preparing" promptly; the
  # remaining chunks stay large enough to amortize lavaanList setup costs.
  first_chunk_size <- min(reps, chunk_size, max(workers, workers * 2L))
  chunks <- list(seq_len(first_chunk_size))
  if (first_chunk_size < reps) {
    chunk_starts <- seq.int(first_chunk_size + 1L, reps, by = chunk_size)
    chunks <- c(chunks, lapply(chunk_starts, function(chunk_start) {
      seq.int(chunk_start, min(reps, chunk_start + chunk_size - 1L))
    }))
  }
  call_fit_list <- function(template, datasets, callback, chunk_start) {
    suppressWarnings(lavaan::lavaanList(
      model = template,
      data_list = datasets,
      cmd = "sem",
      store_slots = character(0),
      fun = callback,
      parallel = if (workers > 1L) "snow" else "no",
      ncpus = workers,
      cl = cluster,
      iseed = structural_canvas_effect_bootstrap_lavaan_seed(seed, chunk_start)
    ))
  }
  run_fit_list <- function(template, datasets, callback, chunk_start) {
    fit_list <- call_fit_list(template, datasets, callback, chunk_start)
    if (!inherits(fit_list, "lavaanList")) {
      stop("Authoritative SEM bootstrap fit did not return a lavaanList object.")
    }
    slot_names <- tryCatch(methods::slotNames(fit_list), error = function(error) character(0))
    if (!"funList" %in% slot_names) {
      stop("Authoritative SEM bootstrap lavaanList has no readable funList slot.")
    }
    values <- tryCatch(
      methods::slot(fit_list, "funList"),
      error = function(error) stop(
        paste0("Authoritative SEM bootstrap could not read funList: ", conditionMessage(error))
      )
    )
    if (!is.list(values)) stop("Authoritative SEM bootstrap funList is malformed.")
    if (length(values) < length(datasets)) length(values) <- length(datasets)
    values[seq_along(datasets)]
  }
  run_fixed_index_blocks <- function(positions, mode = "full") {
    if (!fixed_index_active || !length(positions)) return(NULL)
    mode <- as.character(mode[[1L]])
    if (!mode %in% c("screen", "full")) return(NULL)
    block_count <- min(length(positions), max(workers, workers * 4L))
    local_positions <- if (block_count == 1L) {
      list(seq_along(positions))
    } else {
      split(
        seq_along(positions),
        cut(seq_along(positions), breaks = block_count, labels = FALSE)
      )
    }
    blocks <- lapply(local_positions, function(local_index) {
      requested_positions <- positions[local_index]
      list(
        positions = as.integer(requested_positions),
        indices = do.call(cbind, sample_indices[requested_positions]),
        mode = mode
      )
    })
    pieces <- tryCatch(
      parallel::clusterApplyLB(
        cluster, blocks, structural_canvas_effect_bootstrap_fixed_index_worker
      ),
      error = function(error) NULL
    )
    if (!is.list(pieces) || length(pieces) != length(blocks)) return(NULL)
    values <- vector("list", length(positions))
    names(values) <- as.character(positions)
    for (piece in pieces) {
      if (!is.list(piece) || isTRUE(piece$failed) || !is.list(piece$items) ||
          length(piece$positions) != length(piece$items) ||
          !all(piece$positions %in% positions)) return(NULL)
      values[as.character(piece$positions)] <- piece$items
    }
    if (any(vapply(values, is.null, logical(1)))) return(NULL)
    unname(values)
  }
  for (positions in chunks) {
    if (is.function(cancel) && isTRUE(cancel())) stop("Structural-effect bootstrap canceled.")
    chunk_started <- Sys.time()
    chunk_start <- positions[[1L]]
    data_list <- if (!fixed_index_active) {
      lapply(positions, function(index) {
        structural_canvas_effect_bootstrap_resample_data(
          prepared$data, sample_indices[[index]], prepared$product_specs
        )
      })
    } else NULL
    extract_fit <- structural_canvas_effect_bootstrap_extract_fit
    raw_keys <- prepared$raw_keys
    moderated_specs <- prepared$moderated_specs
    model_df <- prepared$model_df
    if (two_stage_active) {
      two_stage_ever_used <- TRUE
      screening_started <- Sys.time()
      direct_screen <- NULL
      if (fixed_index_active) {
        fixed_started <- Sys.time()
        direct_screen <- run_fixed_index_blocks(positions, mode = "screen")
        fixed_elapsed <- as.numeric(difftime(Sys.time(), fixed_started, units = "secs"))
        fixed_index_seconds <- fixed_index_seconds + fixed_elapsed
        fixed_index_screen_seconds <- fixed_index_screen_seconds + fixed_elapsed
        fixed_index_blocks <- fixed_index_blocks + 1L
        fixed_index_screen_blocks <- fixed_index_screen_blocks + 1L
      }
      screen_call <- if (!is.null(direct_screen)) {
        list(values = direct_screen, complete = TRUE, reason = "")
      } else {
        if (fixed_index_active) {
          fixed_index_fallbacks <- fixed_index_fallbacks + 1L
          fixed_index_screen_fallbacks <- fixed_index_screen_fallbacks + 1L
        }
        if (is.null(data_list)) {
          data_list <- lapply(positions, function(index) {
            structural_canvas_effect_bootstrap_resample_data(
              prepared$data, sample_indices[[index]], prepared$product_specs
            )
          })
        }
        structural_canvas_effect_bootstrap_screen_call(
          function() call_fit_list(
            screen_template, data_list,
            function(fit) extract_fit(
              fit, raw_keys, moderated_specs, model_df, screen_only = TRUE
            ),
            chunk_start
          ),
          length(positions)
        )
      }
      chunk_results <- screen_call$values
      # A whole-call failure has no trustworthy screening evidence. Nominate
      # the entire chunk for the full-SE refit and disable further two-stage
      # screening; the legacy/full-SE result remains the sole authority.
      if (!isTRUE(screen_call$complete)) two_stage_active <- FALSE
      two_stage_screening_seconds <- two_stage_screening_seconds +
        as.numeric(difftime(Sys.time(), screening_started, units = "secs"))
      explicit_rejections <- vapply(
        chunk_results,
        structural_canvas_effect_bootstrap_screen_explicit_reject,
        logical(1)
      )
      candidate_indices <- which(!explicit_rejections)
      candidate_ratio <- length(candidate_indices) / length(positions)
      screen_candidate_ratios <- c(screen_candidate_ratios, candidate_ratio)
      two_stage_screened <- two_stage_screened + length(positions)
      two_stage_rejected <- two_stage_rejected + sum(explicit_rejections)
      if (length(candidate_indices)) {
        # Defer the unchanged full-SE refits and batch candidates across screen
        # chunks. The index-only path retains positions and reconstructs DMC
        # products inside its workers; the public lavaanList fallback retains
        # frames. Both preserve requested-replicate order.
        pending_refit_positions <- c(
          pending_refit_positions, positions[candidate_indices]
        )
        if (!fixed_index_active) {
          pending_refit_data <- c(
            pending_refit_data, data_list[candidate_indices]
          )
        }
        two_stage_refit <- two_stage_refit + length(candidate_indices)
      }
      # A screen result can never supply a reported estimate. Candidates are
      # populated only after their original full-SE refit below.
      chunk_results <- lapply(chunk_results, function(item) list(
        valid = FALSE, raw = NULL, standardized = NULL
      ))
      # A stable model gains nothing from a no-SE pass followed by refitting
      # almost every draw. Keep the first observed chunk exact, then return to
      # the legacy full-SE path for subsequent chunks.
      if (candidate_ratio >= .75) two_stage_active <- FALSE
    } else {
      chunk_results <- if (fixed_index_active) {
        fixed_started <- Sys.time()
        direct_results <- run_fixed_index_blocks(positions, mode = "full")
        fixed_elapsed <- as.numeric(difftime(Sys.time(), fixed_started, units = "secs"))
        fixed_index_seconds <- fixed_index_seconds + fixed_elapsed
        fixed_index_full_seconds <- fixed_index_full_seconds + fixed_elapsed
        fixed_index_blocks <- fixed_index_blocks + 1L
        fixed_index_full_blocks <- fixed_index_full_blocks + 1L
        direct_results
      } else NULL
      if (is.null(chunk_results)) {
        if (fixed_index_active) {
          fixed_index_fallbacks <- fixed_index_fallbacks + 1L
          fixed_index_full_fallbacks <- fixed_index_full_fallbacks + 1L
        }
        if (is.null(data_list)) {
          data_list <- lapply(positions, function(index) {
            structural_canvas_effect_bootstrap_resample_data(
              prepared$data, sample_indices[[index]], prepared$product_specs
            )
          })
        }
        legacy_started <- Sys.time()
        chunk_results <- run_fit_list(
          fit_template, data_list,
          function(fit) extract_fit(fit, raw_keys, moderated_specs, model_df),
          chunk_start
        )
        legacy_fit_seconds <- legacy_fit_seconds +
          as.numeric(difftime(Sys.time(), legacy_started, units = "secs"))
      }
    }
    chunk_valid <- 0L
    for (local_index in seq_along(positions)) {
      item <- chunk_results[[local_index]]
      if (!is.list(item) || !isTRUE(item$valid)) next
      position <- positions[[local_index]]
      draws[position, ] <- item$raw
      standardized_draws[position, ] <- item$standardized
      fit_valid_mask[[position]] <- TRUE
      valid_fits <- valid_fits + 1L
      chunk_valid <- chunk_valid + 1L
    }
    if (!two_stage_ever_used && two_stage_supported && identical(positions, chunks[[1L]]) &&
        chunk_valid / length(positions) < .50) {
      two_stage_active <- TRUE
    }
    chunk_seconds <- c(
      chunk_seconds,
      as.numeric(difftime(Sys.time(), chunk_started, units = "secs"))
    )
    if (is.function(progress)) progress(max(positions), reps, valid_fits)
  }
  # All requested resamples have completed their first pass. Keep that timing
  # separate from the deferred full-SE validation so a 100% progress snapshot
  # does not hide where the remaining wall time is being spent.
  resampling_seconds <- as.numeric(difftime(Sys.time(), resampling_started, units = "secs"))
  validating_seconds <- 0
  if (length(pending_refit_positions)) {
    if (is.function(phase)) phase("validating", reps, reps, valid_fits, workers)
    validating_started <- Sys.time()
    refit_batches <- split(
      seq_along(pending_refit_positions),
      ceiling(seq_along(pending_refit_positions) / chunk_size)
    )
    for (refit_indices in refit_batches) {
      if (is.function(cancel) && isTRUE(cancel())) stop("Structural-effect bootstrap canceled.")
      refit_started <- Sys.time()
      refit_positions <- pending_refit_positions[refit_indices]
      refit_results <- NULL
      if (fixed_index_active) {
        fixed_started <- Sys.time()
        refit_results <- run_fixed_index_blocks(refit_positions, mode = "full")
        fixed_elapsed <- as.numeric(difftime(Sys.time(), fixed_started, units = "secs"))
        fixed_index_seconds <- fixed_index_seconds + fixed_elapsed
        fixed_index_full_seconds <- fixed_index_full_seconds + fixed_elapsed
        fixed_index_blocks <- fixed_index_blocks + 1L
        fixed_index_full_blocks <- fixed_index_full_blocks + 1L
      }
      if (is.null(refit_results)) {
        if (fixed_index_active) {
          fixed_index_fallbacks <- fixed_index_fallbacks + 1L
          fixed_index_full_fallbacks <- fixed_index_full_fallbacks + 1L
        }
        refit_data <- if (length(pending_refit_data) == length(pending_refit_positions)) {
          pending_refit_data[refit_indices]
        } else {
          lapply(refit_positions, function(position) {
            structural_canvas_effect_bootstrap_resample_data(
              prepared$data, sample_indices[[position]], prepared$product_specs
            )
          })
        }
        refit_results <- run_fit_list(
          fit_template, refit_data,
          function(fit) extract_fit(fit, raw_keys, moderated_specs, model_df),
          refit_positions[[1L]]
        )
      }
      two_stage_refit_seconds <- two_stage_refit_seconds +
        as.numeric(difftime(Sys.time(), refit_started, units = "secs"))
      for (local_index in seq_along(refit_indices)) {
        item <- refit_results[[local_index]]
        if (!is.list(item) || !isTRUE(item$valid)) next
        position <- pending_refit_positions[refit_indices[[local_index]]]
        draws[position, ] <- item$raw
        standardized_draws[position, ] <- item$standardized
        fit_valid_mask[[position]] <- TRUE
        valid_fits <- valid_fits + 1L
      }
      # Screening progress is expressed against requested replicates. During
      # the deferred validation pass completed remains at reps and only the
      # final valid count advances, preserving monotonic UI state.
      if (is.function(phase)) phase("validating", reps, reps, valid_fits, workers)
    }
    validating_seconds <- as.numeric(difftime(Sys.time(), validating_started, units = "secs"))
  }
  if (is.function(phase)) phase("summarizing", reps, reps, valid_fits, workers)
  summarizing_started <- Sys.time()
  raw_original <- prepared$raw_original
  standardized_original_values <- prepared$standardized_original_values
  rows <- lapply(seq_along(keys), function(column) {
    values <- draws[, column]
    values <- values[is.finite(values)]
    valid <- length(values)
    interval <- if (valid >= max(20L, ceiling(.5 * reps))) bootstrap_ci(raw_original$est[[column]], values, method = ci_method) else c(NA_real_, NA_real_)
    p_value <- if (valid) min(1, 2 * min((sum(values <= 0) + 1) / (valid + 1), (sum(values >= 0) + 1) / (valid + 1))) else NA_real_
    standardized_values <- standardized_draws[, column]
    standardized_values <- standardized_values[is.finite(standardized_values)]
    standardized_valid <- length(standardized_values)
    standardized_interval <- if (standardized_valid >= max(20L, ceiling(.5 * reps))) bootstrap_ci(standardized_original_values[[column]], standardized_values, method = ci_method) else c(NA_real_, NA_real_)
    standardized_p <- if (standardized_valid) min(1, 2 * min((sum(standardized_values <= 0) + 1) / (standardized_valid + 1), (sum(standardized_values >= 0) + 1) / (standardized_valid + 1))) else NA_real_
    data.frame(
      lhs = raw_original$lhs[[column]], op = raw_original$op[[column]], rhs = raw_original$rhs[[column]],
      estimate = raw_original$est[[column]], se = if (valid > 1L) stats::sd(values) else NA_real_,
      lower = interval[[1L]], upper = interval[[2L]], p = p_value,
      beta_estimate = standardized_original_values[[column]],
      beta_se = if (standardized_valid > 1L) stats::sd(standardized_values) else NA_real_,
      beta_lower = standardized_interval[[1L]], beta_upper = standardized_interval[[2L]], beta_p = standardized_p,
      beta_valid = standardized_valid,
      beta_status = if (identical(raw_original$op[[column]], "modmed")) "Not reported: product-indicator index is scale-dependent" else if (standardized_valid) "Estimated" else "Not available",
      valid = valid, requested = reps, `valid_percent` = 100 * valid / reps,
      ci_method = ci_method,
      quantile_type = structural_canvas_bootstrap_quantile_type(ci_method, "structural_effects"),
      status = structural_canvas_bootstrap_status(valid, reps),
      stringsAsFactors = FALSE
    )
  })
  value <- do.call(rbind, rows)
  if (isTRUE(return_draws)) {
    attr(value, "bootstrap_draws") <- list(
      sample_indices = sample_indices,
      valid_mask = fit_valid_mask,
      raw = draws,
      standardized = standardized_draws
    )
  }
  attr(value, "timings") <- list(
    preparation = as.numeric(prepared$preparation_seconds %||% NA_real_),
    worker_startup = worker_startup_seconds,
    resampling = resampling_seconds,
    validating = validating_seconds,
    summarizing = as.numeric(difftime(Sys.time(), summarizing_started, units = "secs")),
    workers = workers,
    chunk_size = chunk_size,
    chunks = length(chunks),
    chunk_seconds = chunk_seconds,
    two_stage = list(
      supported = two_stage_supported,
      used = two_stage_ever_used,
      screened = two_stage_screened,
      screen_rejected = two_stage_rejected,
      refit = two_stage_refit,
      refit_batches = length(refit_batches),
      candidate_ratios = screen_candidate_ratios,
      screening_seconds = two_stage_screening_seconds,
      refit_seconds = two_stage_refit_seconds,
      legacy_fit_seconds = legacy_fit_seconds
    ),
    fixed_index = list(
      supported = fixed_index_supported,
      active = fixed_index_active,
      product_aware = fixed_index_product_aware,
      worker_context = fixed_index_worker_context,
      batches = fixed_index_blocks,
      seconds = fixed_index_seconds,
      fallbacks = fixed_index_fallbacks,
      screen = list(
        batches = fixed_index_screen_blocks,
        seconds = fixed_index_screen_seconds,
        fallbacks = fixed_index_screen_fallbacks
      ),
      full = list(
        batches = fixed_index_full_blocks,
        seconds = fixed_index_full_seconds,
        fallbacks = fixed_index_full_fallbacks
      )
    ),
    lavaan_metadata_fast_path = list(
      enabled = metadata_fast_path_enabled,
      main = list(
        applied = isTRUE(metadata_fast_path_state$applied),
        reason = as.character(metadata_fast_path_state$reason)
      ),
      workers = cluster_metadata_fast_path
    )
  )
  value
}

structural_canvas_effect_bootstrap_progress_merge <- function(previous, current) {
  if (!is.list(current)) return(previous)
  required <- c("phase", "completed", "total", "valid")
  if (!all(required %in% names(current))) return(previous)
  current_completed <- suppressWarnings(as.integer(current$completed))
  current_total <- suppressWarnings(as.integer(current$total))
  current_valid <- suppressWarnings(as.integer(current$valid))
  if (length(current_completed) != 1L || length(current_total) != 1L || length(current_valid) != 1L ||
      anyNA(c(current_completed, current_total, current_valid)) ||
      current_completed < 0L || current_total < 0L || current_completed > current_total ||
      current_valid < 0L || current_valid > current_completed) return(previous)
  phase_order <- c(
    starting = 0L, loading_engine = 1L, starting_workers = 2L,
    resampling = 3L, validating = 4L, summarizing = 5L, complete = 6L
  )
  current_phase <- as.character(current$phase %||% character(0))
  if (length(current_phase) != 1L) return(previous)
  if (!current_phase %in% names(phase_order)) return(previous)
  if (is.list(previous)) {
    previous_completed <- suppressWarnings(as.integer(previous$completed %||% 0L))
    previous_total <- suppressWarnings(as.integer(previous$total %||% current_total))
    previous_valid <- suppressWarnings(as.integer(previous$valid %||% 0L))
    previous_phase <- as.character(previous$phase %||% character(0))
    phase_regressed <- length(previous_phase) == 1L &&
      previous_phase %in% names(phase_order) &&
      phase_order[[current_phase]] < phase_order[[previous_phase]]
    if ((length(previous_completed) == 1L && is.finite(previous_completed) && current_completed < previous_completed) ||
        (length(previous_total) == 1L && is.finite(previous_total) && current_total != previous_total) ||
        (length(previous_valid) == 1L && is.finite(previous_valid) && current_valid < previous_valid) ||
        phase_regressed) {
      return(previous)
    }
  }
  current$completed <- current_completed
  current$total <- current_total
  current$valid <- current_valid
  current
}

structural_canvas_write_effect_bootstrap_progress <- function(progress_file, completed, total, valid = 0L, phase = "resampling", workers = 1L, started_at = NULL) {
  if (is.null(started_at)) started_at <- Sys.time()
  value <- list(
    phase = as.character(phase), completed = as.integer(completed), total = as.integer(total),
    valid = as.integer(valid), workers = as.integer(workers), started_at = started_at,
    elapsed = as.numeric(difftime(Sys.time(), started_at, units = "secs")), updated_at = Sys.time()
  )
  temporary_file <- tempfile(
    pattern = paste0(basename(progress_file), "."),
    tmpdir = dirname(progress_file)
  )
  on.exit(if (file.exists(temporary_file)) unlink(temporary_file, force = TRUE), add = TRUE)
  saveRDS(value, temporary_file)
  # Same-directory rename is atomic on the supported Windows runtime.  A
  # polling reader can briefly hold the destination open, so retry the atomic
  # replacement without ever falling back to an in-place (partial) write.
  replaced <- FALSE
  for (attempt in seq_len(20L)) {
    replaced <- isTRUE(suppressWarnings(file.rename(temporary_file, progress_file)))
    if (replaced) break
    Sys.sleep(0.005)
  }
  invisible(replaced)
}

structural_canvas_start_effect_bootstrap_job <- function(
  snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal,
  residual_variance_fixes, reps = 0L, seed = default_seed(),
  ci_method = "bias_corrected", ml_likelihood = "normal",
  original_result = NULL, workers = NULL, chunk_size = NULL
) {
  stopifnot(requireNamespace("callr", quietly = TRUE))
  preparation_started <- Sys.time()
  prepared <- structural_canvas_prepare_effect_bootstrap(
    snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal,
    residual_variance_fixes, ml_likelihood, original_result = original_result
  )
  prepared$preparation_seconds <- as.numeric(difftime(Sys.time(), preparation_started, units = "secs"))
  workers <- structural_canvas_effect_bootstrap_workers(workers)
  job_dir <- tempfile("statedu-effect-bootstrap-")
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  input_file <- file.path(job_dir, "input.rds")
  result_file <- file.path(job_dir, "result.rds")
  progress_file <- file.path(job_dir, "progress.rds")
  error_file <- file.path(job_dir, "error.txt")
  saveRDS(
    list(
      prepared = prepared, reps = as.integer(reps), seed = as.integer(seed),
      ci_method = ci_method, workers = workers, chunk_size = chunk_size
    ),
    input_file
  )
  job_started_at <- Sys.time()
  structural_canvas_write_effect_bootstrap_progress(
    progress_file, 0L, as.integer(reps), 0L, "starting", workers, job_started_at
  )
  process <- callr::r_bg(
    func = function(input_file, result_file, progress_file, error_file, project_dir) {
      tryCatch({
        setwd(project_dir)
        if (.Platform$OS.type == "windows") suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE))
        options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
        source(file.path("R", "utils.R"), encoding = "UTF-8")
        source(file.path("R", "setup_custom_model_canvas_structural_bootstrap.R"), encoding = "UTF-8")
        args <- readRDS(input_file)
        started_at <- Sys.time()
        structural_canvas_write_effect_bootstrap_progress(
          progress_file, 0L, args$reps, 0L, "loading_engine", args$workers, started_at
        )
        if (!requireNamespace("lavaan", quietly = TRUE)) stop("The lavaan package is required for SEM bootstrap.")
        value <- structural_canvas_effect_bootstrap_prepared(
          args$prepared, args$reps, args$seed, args$ci_method,
          progress = function(done, total, valid) structural_canvas_write_effect_bootstrap_progress(
            progress_file, done, total, valid, "resampling", args$workers, started_at
          ),
          workers = args$workers, chunk_size = args$chunk_size,
          phase = function(name, done, total, valid, workers) structural_canvas_write_effect_bootstrap_progress(
            progress_file, done, total, valid, name, workers, started_at
          )
        )
        saveRDS(value, result_file)
        valid_values <- if (is.data.frame(value) && "valid" %in% names(value)) suppressWarnings(as.integer(value$valid)) else integer(0)
        valid_values <- valid_values[is.finite(valid_values)]
        valid <- if (length(valid_values)) min(valid_values) else 0L
        structural_canvas_write_effect_bootstrap_progress(
          progress_file, args$reps, args$reps, valid, "complete", args$workers, started_at
        )
      }, error = function(error) {
        writeLines(conditionMessage(error), error_file, useBytes = TRUE)
        quit(status = 1L, save = "no")
      })
      invisible(TRUE)
    },
    args = list(
      input_file = input_file,
      result_file = result_file,
      progress_file = progress_file,
      error_file = error_file,
      project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)
    ),
    supervise = TRUE
  )
  list(
    process = process, directory = job_dir, result_file = result_file, progress_file = progress_file,
    error_file = error_file, started_at = job_started_at, reps = as.integer(reps), total = as.integer(reps),
    workers = workers, preparation_seconds = prepared$preparation_seconds
  )
}

structural_canvas_cleanup_effect_bootstrap_job <- function(job) {
  if (is.null(job)) return(invisible(FALSE))
  directory <- as.character(job$directory %||% "")
  if (!nzchar(directory)) return(invisible(FALSE))
  # A just-terminated Windows worker can retain an input/progress handle for a
  # few scheduler ticks. Retry the same resolved job directory so cancellation
  # does not leave StatEdu-owned bootstrap artifacts behind.
  for (attempt in seq_len(20L)) {
    if (!dir.exists(directory)) return(invisible(TRUE))
    unlink(directory, recursive = TRUE, force = TRUE)
    if (!dir.exists(directory)) return(invisible(TRUE))
    Sys.sleep(0.025)
  }
  invisible(!dir.exists(directory))
}

structural_canvas_stop_effect_bootstrap_job <- function(job) {
  if (is.null(job) || is.null(job$process) || !isTRUE(job$process$is_alive())) {
    return(invisible(FALSE))
  }
  # The optimized worker owns a reusable PSOCK cluster. Kill the complete
  # process tree so cancellation cannot leave R worker processes behind.
  kill_tree <- tryCatch(job$process$kill_tree, error = function(error) NULL)
  if (is.function(kill_tree)) kill_tree() else job$process$kill()
  invisible(TRUE)
}
