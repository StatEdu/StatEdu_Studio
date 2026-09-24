# Structural equation canvas bootstrap helpers.

structural_canvas_htmt_bootstrap <- function(data, indicators_by_factor, reps = 0L, confidence = .95, seed = default_seed(), ordered = character(0), threshold = .85, ci_method = "percentile", progress = NULL, cancel = NULL, strict_correlations = FALSE) {
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
    htmt <- structural_canvas_htmt(correlations, indicators_by_factor, threshold = 1, include_pairs = FALSE)
    vapply(seq_along(pairs), function(pair_index) {
      pair <- pairs[[pair_index]]
      if (isTRUE(strict_correlations)) {
        selected <- unique(c(indicators_by_factor[[pair[[1L]]]], indicators_by_factor[[pair[[2L]]]]))
        if (any(!is.finite(correlations[selected, selected, drop = FALSE]))) return(NA_real_)
      }
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

structural_canvas_bootstrap_inference_usable <- function(valid, requested) {
  valid <- suppressWarnings(as.integer(valid))
  requested <- suppressWarnings(as.integer(requested))
  length(valid) == 1L && length(requested) == 1L &&
    is.finite(valid) && is.finite(requested) && requested > 0L &&
    valid >= 2L && valid >= ceiling(.50 * requested)
}

structural_canvas_bootstrap_token_values <- function(tokens, coefficients) {
  tokens <- trimws(as.character(tokens %||% character(0)))
  if (!length(tokens)) return(list(values = numeric(0), complete = TRUE))
  coefficient_names <- names(coefficients) %||% character(0)
  values <- vapply(tokens, function(token) {
    if (nzchar(token) && token %in% coefficient_names) {
      return(suppressWarnings(as.numeric(coefficients[[token]])[[1L]]))
    }
    numeric_value <- suppressWarnings(as.numeric(token))
    if (length(numeric_value) == 1L && is.finite(numeric_value)) numeric_value else NA_real_
  }, numeric(1))
  list(values = unname(values), complete = all(is.finite(values)))
}

structural_canvas_effect_bootstrap_original_fit_gate <- function(result) {
  fit <- result$fit %||% NULL
  if (!inherits(fit, "lavaan")) {
    return(list(
      eligible = FALSE, state = "original_fit_unavailable",
      reason = "Structural-effect bootstrap was not started because the original lavaan fit is unavailable."
    ))
  }
  converged <- if (!is.null(result$converged)) {
    isTRUE(result$converged)
  } else {
    isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"), error = function(error) FALSE))
  }
  admissibility <- if (!is.null(result$admissible)) {
    list(admissible = isTRUE(result$admissible), reasons = result$admissibility_reasons %||% character(0))
  } else if (exists("structural_canvas_fit_admissibility", mode = "function")) {
    tryCatch(
      structural_canvas_fit_admissibility(fit),
      error = function(error) list(admissible = FALSE, reasons = conditionMessage(error))
    )
  } else {
    list(
      admissible = isTRUE(tryCatch(lavaan::lavInspect(fit, "post.check"), error = function(error) FALSE)),
      reasons = character(0)
    )
  }
  if (!converged) {
    return(list(
      eligible = FALSE, state = "original_fit_nonconverged",
      reason = "Structural-effect bootstrap was not started because the original lavaan model did not converge."
    ))
  }
  if (!isTRUE(admissibility$admissible)) {
    reasons <- trimws(as.character(admissibility$reasons %||% character(0)))
    reasons <- unique(reasons[nzchar(reasons)])
    detail <- if (length(reasons)) paste0(" Reasons: ", paste(reasons, collapse = "; ")) else ""
    return(list(
      eligible = FALSE, state = "original_fit_inadmissible",
      reason = paste0(
        "Structural-effect bootstrap was not started because the original lavaan solution is inadmissible.",
        detail
      )
    ))
  }
  list(eligible = TRUE, state = "eligible", reason = "")
}

structural_canvas_require_effect_bootstrap_original_fit <- function(result) {
  gate <- structural_canvas_effect_bootstrap_original_fit_gate(result)
  if (!isTRUE(gate$eligible)) stop(gate$reason, call. = FALSE)
  gate
}

structural_canvas_lavaan_effect_is_constant <- function(fit, effect, tolerance = sqrt(.Machine$double.eps)) {
  paths <- effect$paths %||% list()
  if (!length(paths)) {
    path <- as.character(effect$path %||% character(0))
    if (length(path)) paths <- list(path)
  }
  if (!length(paths)) return(FALSE)
  parameters <- tryCatch(lavaan::parameterTable(fit), error = function(error) data.frame())
  required <- c("lhs", "op", "rhs", "free", "est")
  if (!is.data.frame(parameters) || !all(required %in% names(parameters))) return(FALSE)
  regressions <- parameters[parameters$op == "~", required, drop = FALSE]
  if (!nrow(regressions)) return(FALSE)
  edge_state <- function(predictor, outcome) {
    rows <- regressions[regressions$lhs == outcome & regressions$rhs == predictor, , drop = FALSE]
    if (!nrow(rows)) return(c(found = FALSE, fixed = FALSE, zero = FALSE))
    free <- suppressWarnings(as.integer(rows$free))
    estimates <- suppressWarnings(as.numeric(rows$est))
    fixed <- all(is.finite(free) & free == 0L)
    # Only an explicitly fixed exact zero makes a coefficient product
    # deterministically zero.  A small nonzero fixed constant (for example
    # 1e-9) still scales the remaining free coefficients and must retain its
    # inferential uncertainty.
    zero <- fixed && all(is.finite(estimates) & estimates == 0)
    c(found = TRUE, fixed = fixed, zero = zero)
  }
  path_is_constant <- function(path) {
    path <- as.character(path %||% character(0))
    if (length(path) < 2L) return(FALSE)
    states <- vapply(seq_len(length(path) - 1L), function(index) {
      edge_state(path[[index]], path[[index + 1L]])
    }, logical(3L))
    if (!all(states["found", ])) return(FALSE)
    all(states["fixed", ]) || any(states["zero", ])
  }
  all(vapply(paths, path_is_constant, logical(1)))
}

structural_canvas_effect_bootstrap_fixed_sources <- function(fit, raw, effect_definitions = list(),
                                                             moderation_definitions = list()) {
  if (!is.data.frame(raw) || !nrow(raw) || !all(c("lhs", "op", "rhs") %in% names(raw))) return(character(0))
  sources <- rep("", nrow(raw))
  parameters <- tryCatch(lavaan::parameterTable(fit), error = function(error) data.frame())
  if (is.data.frame(parameters) && all(c("lhs", "op", "rhs", "free") %in% names(parameters))) {
    regressions <- parameters[parameters$op == "~", c("lhs", "rhs", "free"), drop = FALSE]
    direct_indices <- which(raw$op == "~")
    for (index in direct_indices) {
      rows <- regressions[regressions$lhs == raw$lhs[[index]] & regressions$rhs == raw$rhs[[index]], , drop = FALSE]
      free <- suppressWarnings(as.integer(rows$free))
      if (nrow(rows) && all(is.finite(free) & free == 0L)) sources[[index]] <- "Fixed parameter - no inferential test"
    }
  }
  if (length(effect_definitions)) {
    definitions <- stats::setNames(effect_definitions, vapply(effect_definitions, function(effect) as.character(effect$label %||% ""), character(1)))
    defined_indices <- which(raw$op == ":=")
    for (index in defined_indices) {
      effect <- definitions[[as.character(raw$lhs[[index]])]]
      if (!is.null(effect) && structural_canvas_lavaan_effect_is_constant(fit, effect)) {
        sources[[index]] <- "Fixed effect - no inferential test"
      }
    }
  }
  if (length(effect_definitions) && length(moderation_definitions)) {
    specifications <- structural_canvas_moderated_mediation_bootstrap_specs(list(
      fit = fit,
      effect_definitions = effect_definitions,
      moderation_definitions = moderation_definitions
    ))
    specification_keys <- if (length(specifications)) {
      vapply(specifications, function(item) paste(item$lhs, item$op, item$rhs, sep = "\r"), character(1))
    } else {
      character(0)
    }
    parameter_labels <- trimws(as.character(parameters$label %||% character(0)))
    token_state <- function(token, tolerance = sqrt(.Machine$double.eps)) {
      token <- trimws(as.character(token %||% "")[[1L]])
      numeric_value <- suppressWarnings(as.numeric(token))
      if (nzchar(token) && length(numeric_value) == 1L && is.finite(numeric_value)) {
        return(c(found = TRUE, fixed = TRUE, zero = numeric_value == 0))
      }
      rows <- parameters[nzchar(parameter_labels) & parameter_labels == token, , drop = FALSE]
      if (!nrow(rows) || !all(c("free", "est") %in% names(rows))) {
        return(c(found = FALSE, fixed = FALSE, zero = FALSE))
      }
      free <- suppressWarnings(as.integer(rows$free))
      estimates <- suppressWarnings(as.numeric(rows$est))
      fixed <- all(is.finite(free) & free == 0L)
      zero <- fixed && all(is.finite(estimates) & estimates == 0)
      c(found = TRUE, fixed = fixed, zero = zero)
    }
    moderated_indices <- which(raw$op == "modmed")
    for (index in moderated_indices) {
      key <- paste(raw$lhs[[index]], raw$op[[index]], raw$rhs[[index]], sep = "\r")
      specification_index <- match(key, specification_keys)
      if (is.na(specification_index)) next
      tokens <- specifications[[specification_index]]$required_tokens %||% character(0)
      if (!length(tokens)) next
      states <- vapply(tokens, token_state, logical(3L))
      constant <- all(states["found", ]) &&
        (all(states["fixed", ]) || any(states["fixed", ] & states["zero", ]))
      if (constant) sources[[index]] <- "Fixed effect - no inferential test"
    }
  }
  sources
}

structural_canvas_suppress_fixed_bootstrap_inference <- function(table, fixed_sources) {
  if (!is.data.frame(table) || !nrow(table)) return(table)
  fixed_sources <- as.character(fixed_sources %||% character(0))
  if (length(fixed_sources) != nrow(table)) fixed_sources <- rep("", nrow(table))
  fixed <- nzchar(fixed_sources)
  if (!"inference_source" %in% names(table)) {
    status <- as.character(table$status %||% rep("", nrow(table)))
    table$inference_source <- ifelse(
      status %in% c("Adequate", "Caution"),
      "Bootstrap (empirical two-sided p)",
      "Bootstrap requested - inference suppressed"
    )
  }
  if (!any(fixed)) return(table)
  inference_columns <- intersect(c("se", "lower", "upper", "p", "beta_se", "beta_lower", "beta_upper", "beta_p"), names(table))
  for (column in inference_columns) table[[column]][fixed] <- NA_real_
  if ("beta_status" %in% names(table)) table$beta_status[fixed] <- fixed_sources[fixed]
  if ("status" %in% names(table)) table$status[fixed] <- fixed_sources[fixed]
  table$inference_source[fixed] <- fixed_sources[fixed]
  table
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
        if (!length(interaction_label)) next
        resolved <- structural_canvas_bootstrap_token_values(required, coefficients)
        if (!isTRUE(resolved$complete)) next
        values <- resolved$values
        rows[[length(rows) + 1L]] <- data.frame(
          lhs = paste(path, collapse = " -> "), op = "modmed",
          rhs = as.character(definition$moderator %||% ""),
          est = prod(resolved$values),
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
  structural_canvas_require_effect_bootstrap_original_fit(original)
  raw_original <- lavaan::parameterEstimates(original$fit)
  raw_original <- raw_original[raw_original$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE]
  moderated_original <- structural_canvas_moderated_mediation_indices(original)
  if (nrow(moderated_original)) raw_original <- rbind(raw_original, moderated_original)
  fixed_inference_source <- structural_canvas_effect_bootstrap_fixed_sources(
    original$fit, raw_original, original$effect_definitions %||% list(),
    original$moderation_definitions %||% list()
  )
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
    inference_usable <- structural_canvas_bootstrap_inference_usable(valid, reps)
    interval <- if (inference_usable) bootstrap_ci(raw_original$est[[column]], values, method = ci_method) else c(NA_real_, NA_real_)
    p_value <- if (inference_usable) min(1, 2 * min((sum(values <= 0) + 1) / (valid + 1), (sum(values >= 0) + 1) / (valid + 1))) else NA_real_
    standardized_values <- standardized_draws[, column]
    standardized_values <- standardized_values[is.finite(standardized_values)]
    standardized_valid <- length(standardized_values)
    standardized_usable <- structural_canvas_bootstrap_inference_usable(standardized_valid, reps)
    standardized_interval <- if (standardized_usable) bootstrap_ci(standardized_original_values[[column]], standardized_values, method = ci_method) else c(NA_real_, NA_real_)
    standardized_p <- if (standardized_usable) min(1, 2 * min((sum(standardized_values <= 0) + 1) / (standardized_valid + 1), (sum(standardized_values >= 0) + 1) / (standardized_valid + 1))) else NA_real_
    data.frame(lhs = raw_original$lhs[[column]], op = raw_original$op[[column]], rhs = raw_original$rhs[[column]], estimate = raw_original$est[[column]], se = if (inference_usable && valid > 1L) stats::sd(values) else NA_real_, lower = interval[[1L]], upper = interval[[2L]], p = p_value, beta_estimate = standardized_original_values[[column]], beta_se = if (standardized_usable && standardized_valid > 1L) stats::sd(standardized_values) else NA_real_, beta_lower = standardized_interval[[1L]], beta_upper = standardized_interval[[2L]], beta_p = standardized_p, beta_valid = standardized_valid, beta_status = if (identical(raw_original$op[[column]], "modmed")) "Not reported: product-indicator index is scale-dependent" else if (standardized_usable) "Estimated" else "Not available - insufficient valid bootstrap replicates", valid = valid, requested = reps, `valid_percent` = 100 * valid / reps, ci_method = ci_method, quantile_type = structural_canvas_bootstrap_quantile_type(ci_method, "structural_effects"), status = structural_canvas_bootstrap_status(valid, reps), stringsAsFactors = FALSE)
  })
  structural_canvas_suppress_fixed_bootstrap_inference(do.call(rbind, rows), fixed_inference_source)
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
          required_tokens = required,
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
  original_fit_gate <- structural_canvas_require_effect_bootstrap_original_fit(original_result)
  raw_original <- lavaan::parameterEstimates(original_result$fit)
  raw_original <- raw_original[
    raw_original$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE
  ]
  moderated_original <- structural_canvas_moderated_mediation_indices(original_result)
  if (nrow(moderated_original)) raw_original <- rbind(raw_original, moderated_original)
  fixed_inference_source <- structural_canvas_effect_bootstrap_fixed_sources(
    original_result$fit, raw_original, original_result$effect_definitions %||% list(),
    original_result$moderation_definitions %||% list()
  )
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
    fixed_inference_source = fixed_inference_source,
    standardized_original_values = standardized_original_values,
    moderated_specs = structural_canvas_moderated_mediation_bootstrap_specs(original_result),
    product_specs = product_specs,
    original_fit_gate = original_fit_gate,
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

# Supported lavaan versions validate newly-created objects against the installed
# DESCRIPTION file.  On Windows this means repeated system.file()/read.dcf()
# calls from lavInspect() plus packageDescription() calls while every bootstrap
# fit is assembled.  Concurrent workers can spend substantially more wall time
# waiting on those metadata reads than fitting a small model.
#
# This optimization is deliberately installed only by isolated callr/PSOCK
# bootstrap workers.  It does not alter the Shiny process, model options,
# resamples, estimates, admissibility gates, or CI calculations.  The current
# package version and the audited lavaan source contracts must match exactly; any
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
  if (!installed_version %in% c("0.6.21", "0.7.2") ||
      (identical(installed_version, "0.6.21") &&
       !isTRUE(getOption("statedu.internal.sem_metadata_0621", TRUE)))) {
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
  marker <- paste0("statedu_lavaan_worker_metadata_fast_path_", gsub(".", "_", installed_version, fixed = TRUE))
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
    get(if (identical(installed_version, "0.6.21")) "lav_lavaan_step17_lavaan" else "lav_step17_lavaan", namespace, inherits = FALSE),
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
  expected_body_digests <- if (identical(installed_version, "0.6.21")) c(
    lav_object_check_version = "98e1db0df4f86bf0eba010eb178a61f40abefd155892112d800969c2e0c9eb07",
    lav_step17_lavaan = "01488fe9d53c02319298e6a828900404008af2e2ed14567b431b7c8ed6fd2e6f",
    lavaanList = "935712acf18237d126d4ebb311b01d29ae757deb2450e858c65cd54aa55c0ed3"
  ) else c(
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
    applied = TRUE, owned = TRUE, reason = paste("lavaan", installed_version, "metadata contract"),
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
        required <- specification$required_tokens %||% specification$required_labels %||% character(0)
        coefficient_names <- names(coefficients) %||% character(0)
        resolved_values <- vapply(trimws(as.character(required)), function(token) {
          if (nzchar(token) && token %in% coefficient_names) {
            return(suppressWarnings(as.numeric(coefficients[[token]])[[1L]]))
          }
          numeric_value <- suppressWarnings(as.numeric(token))
          if (length(numeric_value) == 1L && is.finite(numeric_value)) numeric_value else NA_real_
        }, numeric(1))
        resolved <- list(values = unname(resolved_values), complete = all(is.finite(resolved_values)))
        if (!is.na(position) && length(required) && isTRUE(resolved$complete)) {
          raw[[position]] <- prod(resolved$values)
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
  if (exists(".statedu_sem_shared_inverse_state", envir = .GlobalEnv, inherits = FALSE)) {
    state <- get(".statedu_sem_shared_inverse_state", envir = .GlobalEnv, inherits = FALSE)
    state$restore()
    rm(".statedu_sem_shared_inverse_state", envir = .GlobalEnv)
  }
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

structural_canvas_effect_bootstrap_worker_install_metadata <- function(install, legacy_enabled = NULL) {
  if (!is.null(legacy_enabled)) options(statedu.internal.sem_metadata_0621 = isTRUE(legacy_enabled))
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
        if (isTRUE(context$legacy_slot_names)) {
          lavaan::lavaan(slotOptions = fit_options, slotParTable = context$partable, data = frame)
        } else {
          lavaan::lavaan(slot_options = fit_options, slot_par_table = context$partable, data = frame)
        },
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

structural_canvas_effect_bootstrap_index_memory_diagnostics <- function(
  reps, observations, chunk_size, return_draws = FALSE,
  replay_chunks = 0L, replay_state_bytes = 0
) {
  scalar_count <- function(value, fallback, minimum) {
    value <- suppressWarnings(as.integer(value %||% fallback))
    if (length(value) != 1L || !is.finite(value)) value <- fallback
    as.integer(max(minimum, value))
  }
  reps <- scalar_count(reps, 0L, 0L)
  observations <- scalar_count(observations, 0L, 0L)
  chunk_size <- scalar_count(chunk_size, 1L, 1L)
  peak_chunk_replicates <- min(reps, chunk_size)
  integer_bytes <- 4
  full_materialization_bytes <- as.double(reps) * as.double(observations) * integer_bytes
  peak_chunk_bytes <- as.double(peak_chunk_replicates) * as.double(observations) * integer_bytes
  list(
    strategy = if (isTRUE(return_draws)) {
      "chunked generation with explicit full draw retention"
    } else {
      "chunked generation with compact RNG-state replay for deferred refits"
    },
    requested_replicates = reps,
    observations = observations,
    peak_chunk_replicates = peak_chunk_replicates,
    retained_replicates = if (isTRUE(return_draws)) reps else 0L,
    replay_chunks = suppressWarnings(as.integer(replay_chunks %||% 0L)),
    replay_state_bytes = as.double(replay_state_bytes %||% 0),
    estimated_full_materialization_bytes = full_materialization_bytes,
    estimated_peak_sample_index_bytes = if (isTRUE(return_draws)) {
      full_materialization_bytes
    } else {
      peak_chunk_bytes
    },
    full_materialization_avoided = !isTRUE(return_draws)
  )
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
  original_fit_gate <- prepared$original_fit_gate %||%
    structural_canvas_effect_bootstrap_original_fit_gate(list(fit = prepared$fit_template))
  if (!isTRUE(original_fit_gate$eligible)) stop(
    as.character(original_fit_gate$reason %||% "Structural-effect bootstrap requires a converged, admissible original lavaan model."),
    call. = FALSE
  )
  workers <- structural_canvas_effect_bootstrap_workers(workers)
  if (is.null(chunk_size)) chunk_size <- max(workers * 4L, min(250L, ceiling(reps / 20L)))
  chunk_size <- max(workers, suppressWarnings(as.integer(chunk_size)))
  keys <- prepared$raw_keys
  draws <- matrix(NA_real_, nrow = reps, ncol = length(keys), dimnames = list(NULL, keys))
  standardized_draws <- matrix(NA_real_, nrow = reps, ncol = length(keys), dimnames = list(NULL, keys))
  fit_valid_mask <- rep(FALSE, reps)
  old_rng_kind <- RNGkind()
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_rng_kind))
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  bootstrap_rng_kind <- c(
    kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection"
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
        installer, legacy_enabled = isTRUE(getOption("statedu.internal.sem_metadata_0621", TRUE))
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
  # MLR retains its full robust covariance and observed-information gate. Only
  # scheduling changes: send indices to persistent workers instead of serializing
  # every replicated data frame through lavaanList's static batches.
  mlr_variables <- tryCatch(lavaan::lavNames(fit_template, "ov"), error = function(error) character(0))
  fixed_index_mlr <- isTRUE(getOption("statedu.internal.sem_mlr_fixed_index", TRUE)) &&
    workers > 1L && template_version %in% c("0.6-21", "0.7-2") &&
    identical(gsub("-", ".", template_version, fixed = TRUE), as.character(utils::packageVersion("lavaan"))) &&
    identical(template_se, "robust.huber.white") && identical(template_estimator, "ML") &&
    template_likelihood %in% c("normal", "wishart") && !template_categorical &&
    identical(template_groups, 1L) && identical(template_levels, 1L) &&
    is.finite(template_random_starts) && template_random_starts == 0L &&
    length(prepared$product_specs %||% list()) == 0L &&
    template_missing %in% c("listwise", "ml", "fiml") &&
    length(mlr_variables) > 0L && all(mlr_variables %in% names(prepared$data)) &&
    all(vapply(prepared$data[mlr_variables], is.numeric, logical(1))) &&
    !isTRUE(getOption("statedu.internal.disable_sem_bootstrap_fixed_index", FALSE))
  fixed_index_common_supported <- fixed_index_mlr || (workers > 1L && isTRUE(metadata_fast_path_state$applied) &&
    all_worker_fast_paths_applied && identical(template_version, "0.7-2") &&
    identical(template_se, "standard") && identical(template_estimator, "ML") &&
    template_likelihood %in% c("normal", "wishart") && !template_categorical &&
    identical(template_groups, 1L) && identical(template_levels, 1L) &&
    is.finite(template_random_starts) && template_random_starts == 0L &&
    all(vapply(prepared$data, is.numeric, logical(1))) &&
    !isTRUE(getOption("statedu.internal.disable_sem_bootstrap_fixed_index", FALSE)))
  fixed_index_product_aware <- fixed_index_common_supported &&
    length(prepared$product_specs %||% list()) > 0L &&
    template_missing %in% c("listwise", "ml", "fiml") &&
    !anyNA(prepared$data)
  # Actual-missing FIML is numerically exact on the non-product worker path.
  # Keep the optimization within the validated UI range (the 5,000 default);
  # larger custom jobs retain the authoritative lavaanList fallback until they
  # receive an independent scaling gate.
  fixed_index_nonproduct <- fixed_index_common_supported &&
    length(prepared$product_specs %||% list()) == 0L &&
    (
      (!anyNA(prepared$data) && template_missing %in% c("listwise", "ml", "fiml")) ||
        (anyNA(prepared$data) && template_missing %in% c("ml", "fiml") &&
          reps <= 5000L)
    )
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
  fixed_information_mode <- tryCatch(
    tolower(trimws(as.character(getOption(
      "statedu.internal.sem_bootstrap_information_mode", "expected"
    ))[[1L]])),
    error = function(error) "observed"
  )
  if (!fixed_information_mode %in% c("observed", "expected")) {
    fixed_information_mode <- "observed"
  }
  fixed_expected_information_active <- FALSE
  if (fixed_index_supported) {
    fixed_partable <- fit_template@ParTable
    fixed_partable$start <- fixed_partable$est <- fixed_partable$se <- NULL
    fixed_options <- fit_template@Options
    fixed_options$fit.by.level <- FALSE
    fixed_expected_information_active <- fixed_index_nonproduct && !fixed_index_mlr &&
      anyNA(prepared$data) && identical(fixed_information_mode, "expected")
    if (fixed_expected_information_active) {
      # Bootstrap inference below uses the replicate estimates, not each
      # replicate's model-based standard errors. Expected information preserves
      # the ML estimates and standardized estimates while avoiding the much
      # slower numerical observed Hessian. The parameter-vcov admissibility gate
      # remains active, now against the expected-information covariance.
      fixed_options$information <- rep(
        "expected", max(1L, length(fixed_options$information))
      )
    }
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
      legacy_slot_names = fixed_index_mlr && identical(template_version, "0.6-21"),
      data = if (fixed_index_mlr) prepared$data[mlr_variables] else prepared$data,
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
  shared_inverse_workers <- list()
  if (fixed_index_active && metadata_fast_path_enabled &&
      !isTRUE(getOption("statedu.internal.disable_sem_shared_inverse", FALSE)) &&
      structural_canvas_sem_shared_inverse_eligible(prepared, reps, workers)) {
    shared_inverse_workers <- parallel::clusterCall(
      cluster, structural_canvas_sem_shared_inverse_install,
      structural_canvas_sem_shared_inverse_gradient
    )
  }
  two_stage_screened <- 0L
  two_stage_rejected <- 0L
  two_stage_refit <- 0L
  two_stage_screening_seconds <- 0
  two_stage_refit_seconds <- 0
  legacy_fit_seconds <- 0
  chunk_seconds <- numeric(0)
  screen_candidate_ratios <- numeric(0)
  pending_refit_positions <- integer(0)
  pending_refit_replay <- list()
  refit_batches <- list()
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
  retained_sample_indices <- if (isTRUE(return_draws)) vector("list", reps) else NULL
  do.call(RNGkind, as.list(bootstrap_rng_kind))
  set.seed(as.integer(seed))
  resampling_rng_state <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  resampling_started <- Sys.time()
  valid_fits <- 0L
  if (is.function(progress)) progress(0L, reps, valid_fits)
  call_fit_list <- function(template, datasets, callback, chunk_start) {
    suppressWarnings(lavaan::lavaanList(
      model = template,
      dataList = datasets,
      cmd = "sem",
      store.slots = character(0),
      FUN = callback,
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
  replay_sample_indices <- function(requested_positions) {
    requested_positions <- as.integer(requested_positions)
    if (!length(requested_positions)) return(list())
    if (isTRUE(return_draws)) {
      values <- retained_sample_indices[requested_positions]
      if (any(vapply(values, is.null, logical(1)))) {
        stop("Retained SEM bootstrap sample indices are incomplete.")
      }
      return(values)
    }
    current_rng_kind <- RNGkind()
    current_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (current_seed_exists) {
      current_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit({
      do.call(RNGkind, as.list(current_rng_kind))
      if (current_seed_exists) {
        assign(".Random.seed", current_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    values <- vector("list", length(requested_positions))
    for (record in pending_refit_replay) {
      matched <- match(record$positions, requested_positions, nomatch = 0L)
      if (!any(matched > 0L)) next
      do.call(RNGkind, as.list(bootstrap_rng_kind))
      assign(".Random.seed", record$rng_state, envir = .GlobalEnv)
      generated <- lapply(record$positions, function(position) {
        sample.int(nrow(prepared$data), nrow(prepared$data), replace = TRUE)
      })
      selected <- which(matched > 0L)
      values[matched[selected]] <- generated[selected]
    }
    if (any(vapply(values, is.null, logical(1)))) {
      stop("Deferred SEM bootstrap sample-index replay is incomplete.")
    }
    values
  }
  run_fixed_index_blocks <- function(positions, sample_indices, mode = "full") {
    if (!fixed_index_active || !length(positions) ||
        length(sample_indices) != length(positions)) return(NULL)
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
        indices = do.call(cbind, sample_indices[local_index]),
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
    # Fitting and progress callbacks may consume or even replace the process RNG
    # state. Resume the dedicated controller stream explicitly so worker count,
    # callback behavior, and chunk boundaries cannot change later resamples.
    chunk_rng_state <- resampling_rng_state
    do.call(RNGkind, as.list(bootstrap_rng_kind))
    assign(".Random.seed", chunk_rng_state, envir = .GlobalEnv)
    chunk_sample_indices <- lapply(positions, function(position) {
      sample.int(nrow(prepared$data), nrow(prepared$data), replace = TRUE)
    })
    resampling_rng_state <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (isTRUE(return_draws)) {
      retained_sample_indices[positions] <- chunk_sample_indices
    }
    data_list <- if (!fixed_index_active) {
      lapply(seq_along(positions), function(local_index) {
        structural_canvas_effect_bootstrap_resample_data(
          prepared$data, chunk_sample_indices[[local_index]], prepared$product_specs
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
        direct_screen <- run_fixed_index_blocks(
          positions, chunk_sample_indices, mode = "screen"
        )
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
          data_list <- lapply(seq_along(positions), function(local_index) {
            structural_canvas_effect_bootstrap_resample_data(
              prepared$data, chunk_sample_indices[[local_index]], prepared$product_specs
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
        # chunks. Only positions plus one compact controller RNG state per
        # candidate-bearing chunk are retained; both the index-only worker path
        # and the public lavaanList fallback replay the exact requested draws.
        pending_refit_positions <- c(
          pending_refit_positions, positions[candidate_indices]
        )
        if (!isTRUE(return_draws)) {
          pending_refit_replay[[length(pending_refit_replay) + 1L]] <- list(
            positions = positions,
            rng_state = chunk_rng_state
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
        direct_results <- run_fixed_index_blocks(
          positions, chunk_sample_indices, mode = "full"
        )
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
          data_list <- lapply(seq_along(positions), function(local_index) {
            structural_canvas_effect_bootstrap_resample_data(
              prepared$data, chunk_sample_indices[[local_index]], prepared$product_specs
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
    rm(chunk_sample_indices, data_list)
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
      refit_sample_indices <- replay_sample_indices(refit_positions)
      refit_results <- NULL
      if (fixed_index_active) {
        fixed_started <- Sys.time()
        refit_results <- run_fixed_index_blocks(
          refit_positions, refit_sample_indices, mode = "full"
        )
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
        refit_data <- lapply(refit_sample_indices, function(sample_index) {
          structural_canvas_effect_bootstrap_resample_data(
            prepared$data, sample_index, prepared$product_specs
          )
        })
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
      rm(refit_sample_indices)
      if (exists("refit_data", inherits = FALSE)) rm(refit_data)
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
    inference_usable <- structural_canvas_bootstrap_inference_usable(valid, reps)
    interval <- if (inference_usable) bootstrap_ci(raw_original$est[[column]], values, method = ci_method) else c(NA_real_, NA_real_)
    p_value <- if (inference_usable) min(1, 2 * min((sum(values <= 0) + 1) / (valid + 1), (sum(values >= 0) + 1) / (valid + 1))) else NA_real_
    standardized_values <- standardized_draws[, column]
    standardized_values <- standardized_values[is.finite(standardized_values)]
    standardized_valid <- length(standardized_values)
    standardized_usable <- structural_canvas_bootstrap_inference_usable(standardized_valid, reps)
    standardized_interval <- if (standardized_usable) bootstrap_ci(standardized_original_values[[column]], standardized_values, method = ci_method) else c(NA_real_, NA_real_)
    standardized_p <- if (standardized_usable) min(1, 2 * min((sum(standardized_values <= 0) + 1) / (standardized_valid + 1), (sum(standardized_values >= 0) + 1) / (standardized_valid + 1))) else NA_real_
    data.frame(
      lhs = raw_original$lhs[[column]], op = raw_original$op[[column]], rhs = raw_original$rhs[[column]],
      estimate = raw_original$est[[column]], se = if (inference_usable && valid > 1L) stats::sd(values) else NA_real_,
      lower = interval[[1L]], upper = interval[[2L]], p = p_value,
      beta_estimate = standardized_original_values[[column]],
      beta_se = if (standardized_usable && standardized_valid > 1L) stats::sd(standardized_values) else NA_real_,
      beta_lower = standardized_interval[[1L]], beta_upper = standardized_interval[[2L]], beta_p = standardized_p,
      beta_valid = standardized_valid,
      beta_status = if (identical(raw_original$op[[column]], "modmed")) "Not reported: product-indicator index is scale-dependent" else if (standardized_usable) "Estimated" else "Not available - insufficient valid bootstrap replicates",
      valid = valid, requested = reps, `valid_percent` = 100 * valid / reps,
      ci_method = ci_method,
      quantile_type = structural_canvas_bootstrap_quantile_type(ci_method, "structural_effects"),
      status = structural_canvas_bootstrap_status(valid, reps),
      stringsAsFactors = FALSE
    )
  })
  value <- structural_canvas_suppress_fixed_bootstrap_inference(
    do.call(rbind, rows), prepared$fixed_inference_source %||% character(0)
  )
  if (isTRUE(return_draws)) {
    attr(value, "bootstrap_draws") <- list(
      sample_indices = retained_sample_indices,
      valid_mask = fit_valid_mask,
      raw = draws,
      standardized = standardized_draws
    )
  }
  replay_state_bytes <- sum(vapply(
    pending_refit_replay,
    function(record) as.double(utils::object.size(record$rng_state)),
    numeric(1)
  ))
  sample_index_memory <- structural_canvas_effect_bootstrap_index_memory_diagnostics(
    reps = reps,
    observations = nrow(prepared$data),
    chunk_size = max(vapply(chunks, length, integer(1))),
    return_draws = return_draws,
    replay_chunks = length(pending_refit_replay),
    replay_state_bytes = replay_state_bytes
  )
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
    rng = list(
      policy = "RNGkind('Mersenne-Twister', 'Inversion', 'Rejection'); set.seed(seed); controller-only sequential case-resampling stream restored before every chunk",
      seed = as.integer(seed),
      kind = bootstrap_rng_kind,
      r_version = as.character(getRversion())
    ),
    sample_index_memory = sample_index_memory,
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
      mlr = fixed_index_mlr,
      supported = fixed_index_supported,
      active = fixed_index_active,
      product_aware = fixed_index_product_aware,
      actual_missing = anyNA(prepared$data),
      missing = template_missing,
      information = list(
        mode = fixed_information_mode,
        expected_active = fixed_expected_information_active
      ),
      worker_context = fixed_index_worker_context,
      shared_inverse_workers = shared_inverse_workers,
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
    resampling = 3L, validating = 4L, summarizing = 5L,
    multigroup_resampling = 6L, complete = 7L
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

# Execute the pooled structural-effect and multi-group latent-moderation passes
# independently.  A failure in one component must never discard a valid result
# from the other component; only an all-requested-components failure is fatal.
# The small pure wrapper also makes this preservation contract directly
# testable without starting a background R process.
structural_canvas_run_bootstrap_components <- function(
  run_pooled, run_multigroup, pooled_call = NULL, multigroup_call = NULL
) {
  run_pooled <- isTRUE(run_pooled)
  run_multigroup <- isTRUE(run_multigroup)
  if (!run_pooled && !run_multigroup) stop("No bootstrap component was requested.")
  if (run_pooled && !is.function(pooled_call)) stop("The pooled bootstrap callback is unavailable.")
  if (run_multigroup && !is.function(multigroup_call)) stop("The multi-group bootstrap callback is unavailable.")

  pooled_attempt <- if (run_pooled) {
    tryCatch({
      pooled_value <- pooled_call()
      if (!is.data.frame(pooled_value)) {
        stop("The pooled bootstrap callback returned an invalid result contract.")
      }
      list(succeeded = TRUE, value = pooled_value, error = NULL)
    }, error = function(error) list(
      succeeded = FALSE, value = data.frame(), error = conditionMessage(error)
    ))
  } else {
    list(succeeded = FALSE, value = data.frame(), error = NULL)
  }
  value <- pooled_attempt$value
  if (!is.data.frame(value)) value <- data.frame()
  multigroup_attempt <- if (run_multigroup) {
    tryCatch({
      multigroup_value <- multigroup_call(value)
      if (!is.list(multigroup_value) || !is.list(multigroup_value$diagnostics)) {
        stop("The multi-group bootstrap callback returned an invalid result contract.")
      }
      list(succeeded = TRUE, value = multigroup_value, error = NULL)
    }, error = function(error) {
      list(succeeded = FALSE, value = NULL, error = conditionMessage(error))
    })
  } else {
    list(succeeded = FALSE, value = NULL, error = NULL)
  }
  if (isTRUE(multigroup_attempt$succeeded)) {
    attr(value, "multigroup_moderation") <- multigroup_attempt$value
  }
  component_status <- list(
    pooled_effect = list(
      requested = run_pooled, succeeded = isTRUE(pooled_attempt$succeeded),
      error = pooled_attempt$error
    ),
    multigroup_moderation = list(
      requested = run_multigroup, succeeded = isTRUE(multigroup_attempt$succeeded),
      error = multigroup_attempt$error
    )
  )
  attr(value, "bootstrap_component_status") <- component_status
  requested_success <- c(
    if (run_pooled) component_status$pooled_effect$succeeded,
    if (run_multigroup) component_status$multigroup_moderation$succeeded
  )
  if (!any(requested_success)) {
    component_errors <- Filter(nzchar, c(
      as.character(component_status$pooled_effect$error %||% ""),
      as.character(component_status$multigroup_moderation$error %||% "")
    ))
    stop(if (length(component_errors)) paste(component_errors, collapse = " | ") else "No requested bootstrap component completed.")
  }
  value
}

structural_canvas_start_effect_bootstrap_job <- function(
  snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal,
  residual_variance_fixes, reps = 0L, seed = default_seed(),
  ci_method = "bias_corrected", ml_likelihood = "normal",
  original_result = NULL, workers = NULL, chunk_size = NULL,
  multigroup_moderation = NULL, run_pooled_effect = TRUE
) {
  stopifnot(requireNamespace("callr", quietly = TRUE))
  preparation_started <- Sys.time()
  run_pooled_effect <- isTRUE(run_pooled_effect)
  prepared <- if (run_pooled_effect) {
    structural_canvas_prepare_effect_bootstrap(
      snapshot, data, analysis_type, estimator, missing, std_lv, ordered, nominal,
      residual_variance_fixes, ml_likelihood, original_result = original_result
    )
  } else {
    NULL
  }
  preparation_seconds <- if (run_pooled_effect) {
    as.numeric(difftime(Sys.time(), preparation_started, units = "secs"))
  } else {
    0
  }
  if (is.list(prepared)) prepared$preparation_seconds <- preparation_seconds
  workers <- structural_canvas_effect_bootstrap_workers(workers)
  job_dir <- tempfile("statedu-effect-bootstrap-")
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  input_file <- file.path(job_dir, "input.rds")
  result_file <- file.path(job_dir, "result.rds")
  progress_file <- file.path(job_dir, "progress.rds")
  error_file <- file.path(job_dir, "error.txt")
  has_multigroup_moderation <- is.list(multigroup_moderation) &&
    nzchar(as.character(multigroup_moderation$group %||% "")) &&
    length(multigroup_moderation$moderation_definitions %||% list()) > 0L
  work_passes <- as.integer(run_pooled_effect) + as.integer(has_multigroup_moderation)
  if (work_passes < 1L) stop("No eligible structural-effect or multi-group latent-moderation bootstrap was available.")
  total_work <- as.integer(reps) * work_passes
  saveRDS(
    list(
      prepared = prepared, reps = as.integer(reps), seed = as.integer(seed),
      ci_method = ci_method, workers = workers, chunk_size = chunk_size,
      disable_fixed_index = isTRUE(getOption(
        "statedu.internal.disable_sem_bootstrap_fixed_index", FALSE
      )),
      disable_shared_inverse = isTRUE(getOption(
        "statedu.internal.disable_sem_shared_inverse", FALSE
      )),
      information_mode = as.character(getOption(
        "statedu.internal.sem_bootstrap_information_mode", "expected"
      )),
      multigroup_moderation = if (has_multigroup_moderation) multigroup_moderation else NULL,
      run_pooled_effect = run_pooled_effect, total_work = total_work
    ),
    input_file
  )
  job_started_at <- Sys.time()
  structural_canvas_write_effect_bootstrap_progress(
    progress_file, 0L, total_work, 0L, "starting", workers, job_started_at
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
        options(
          statedu.internal.disable_sem_shared_inverse = isTRUE(args$disable_shared_inverse),
          statedu.internal.disable_sem_bootstrap_fixed_index =
            isTRUE(args$disable_fixed_index),
          statedu.internal.sem_bootstrap_information_mode =
            as.character(args$information_mode %||% "expected")
        )
        if (is.list(args$multigroup_moderation)) {
          source(file.path("R", "setup_custom_model_canvas_structural_core.R"), encoding = "UTF-8")
          source(file.path("R", "setup_custom_model_canvas_structural_evaluation.R"), encoding = "UTF-8")
          source(file.path("R", "setup_custom_model_canvas_structural_invariance_evaluation.R"), encoding = "UTF-8")
          source(file.path("R", "setup_custom_model_canvas_structural_lavaan_syntax.R"), encoding = "UTF-8")
        }
        started_at <- Sys.time()
        structural_canvas_write_effect_bootstrap_progress(
          progress_file, 0L, args$total_work, 0L, "loading_engine", args$workers, started_at
        )
        if (!requireNamespace("lavaan", quietly = TRUE)) stop("The lavaan package is required for SEM bootstrap.")
        value <- structural_canvas_run_bootstrap_components(
          run_pooled = isTRUE(args$run_pooled_effect),
          run_multigroup = is.list(args$multigroup_moderation),
          pooled_call = function() {
            structural_canvas_effect_bootstrap_prepared(
                args$prepared, args$reps, args$seed, args$ci_method,
                progress = function(done, total, valid) structural_canvas_write_effect_bootstrap_progress(
                  progress_file, done, args$total_work, valid, "resampling", args$workers, started_at
                ),
                workers = args$workers, chunk_size = args$chunk_size,
                phase = function(name, done, total, valid, workers) structural_canvas_write_effect_bootstrap_progress(
                  progress_file, done, args$total_work, valid, name, workers, started_at
                )
            )
          },
          multigroup_call = function(pooled_value) {
            specification <- args$multigroup_moderation
            pooled_offset <- if (isTRUE(args$run_pooled_effect)) args$reps else 0L
            effect_valid_values <- if (is.data.frame(pooled_value) && "valid" %in% names(pooled_value)) {
              suppressWarnings(as.integer(pooled_value$valid))
            } else integer(0)
            effect_valid_values <- effect_valid_values[is.finite(effect_valid_values)]
            effect_valid <- if (length(effect_valid_values)) min(effect_valid_values) else 0L
            structural_canvas_multigroup_moderation_bootstrap(
              syntax = specification$syntax,
              raw_data = specification$raw_data,
              group = specification$group,
              moderation_definitions = specification$moderation_definitions,
              effect_definitions = specification$effect_definitions,
              estimator = specification$estimator,
              missing = specification$missing,
              std_lv = specification$std_lv,
              reps = args$reps,
              seed = args$seed,
              ci_method = args$ci_method,
              ml_likelihood = specification$ml_likelihood,
              progress = function(done, total, valid) structural_canvas_write_effect_bootstrap_progress(
                progress_file, pooled_offset + done, args$total_work,
                effect_valid + valid, "multigroup_resampling", args$workers, started_at
              ),
              workers = args$workers, chunk_size = args$chunk_size
            )
          }
        )
        effect_valid_values <- if (is.data.frame(value) && "valid" %in% names(value)) {
          suppressWarnings(as.integer(value$valid))
        } else integer(0)
        effect_valid_values <- effect_valid_values[is.finite(effect_valid_values)]
        effect_valid <- if (length(effect_valid_values)) min(effect_valid_values) else 0L
        saveRDS(value, result_file)
        multigroup_diagnostics <- attr(value, "multigroup_moderation")$diagnostics %||% list()
        multigroup_valid <- suppressWarnings(as.integer(multigroup_diagnostics$joint_valid_replicates %||% 0L))
        if (!is.finite(multigroup_valid)) multigroup_valid <- 0L
        valid <- effect_valid + multigroup_valid
        structural_canvas_write_effect_bootstrap_progress(
          progress_file, args$total_work, args$total_work, valid, "complete", args$workers, started_at
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
    error_file = error_file, started_at = job_started_at, reps = as.integer(reps), total = total_work,
    workers = workers, preparation_seconds = preparation_seconds,
    run_pooled_effect = run_pooled_effect,
    run_multigroup_moderation = has_multigroup_moderation
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

structural_canvas_multigroup_moderation_bootstrap_worker <- function(sampled_indices, config) {
  sampled <- config$analysis_data[sampled_indices, , drop = FALSE]
  rownames(sampled) <- NULL
  prepared <- tryCatch(
    structural_canvas_prepare_group_product_indicators(
      sampled, config$group, config$moderation_definitions
    ),
    error = identity
  )
  if (inherits(prepared, "error")) {
    return(list(fit_valid = FALSE, joint_valid = FALSE, state = "product_preparation"))
  }
  arguments <- list(
    model = config$group_syntax,
    data = prepared$data,
    group = config$group,
    group.label = config$group_labels,
    group.equal = "loadings",
    estimator = config$estimator,
    missing = config$missing,
    std.lv = isTRUE(config$std_lv),
    auto.cov.lv.x = FALSE
  )
  if (identical(config$estimator, "ML")) arguments$likelihood <- config$ml_likelihood
  fit <- tryCatch(suppressWarnings(do.call(lavaan::sem, arguments)), error = identity)
  if (inherits(fit, "error")) {
    return(list(fit_valid = FALSE, joint_valid = FALSE, state = "fit_error"))
  }
  converged <- isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"), error = function(error) FALSE))
  if (!converged) {
    return(list(fit_valid = FALSE, joint_valid = FALSE, state = "nonconverged"))
  }
  admissibility <- tryCatch(
    structural_canvas_fit_admissibility(fit),
    error = function(error) list(admissible = FALSE)
  )
  if (!isTRUE(admissibility$admissible)) {
    return(list(fit_valid = FALSE, joint_valid = FALSE, state = "inadmissible"))
  }
  parameters <- tryCatch(lavaan::parameterTable(fit), error = function(error) data.frame())
  required_columns <- c("lhs", "op", "rhs", "group", "free", "est")
  if (!is.data.frame(parameters) || !all(required_columns %in% names(parameters))) {
    return(list(fit_valid = TRUE, joint_valid = FALSE, state = "target_extraction"))
  }
  regressions <- parameters[parameters$op == "~", required_columns, drop = FALSE]
  edge_row <- function(lhs, rhs, group_index) {
    rows <- regressions[
      regressions$lhs == lhs & regressions$rhs == rhs & regressions$group == group_index,
      , drop = FALSE
    ]
    if (nrow(rows) != 1L) return(NULL)
    rows[1L, , drop = FALSE]
  }
  group_count <- length(config$group_labels)
  interaction_values <- matrix(
    NA_real_, nrow = length(config$interaction_specs), ncol = group_count
  )
  for (specification_index in seq_along(config$interaction_specs)) {
    specification <- config$interaction_specs[[specification_index]]
    for (group_index in seq_len(group_count)) {
      row <- edge_row(specification$outcome, specification$interaction_factor, group_index)
      if (!is.null(row)) {
        interaction_values[specification_index, group_index] <-
          suppressWarnings(as.numeric(row$est[[1L]]))
      }
    }
  }
  index_values <- matrix(
    NA_real_, nrow = length(config$moderated_specs), ncol = group_count
  )
  if (length(config$moderated_specs)) {
    for (specification_index in seq_along(config$moderated_specs)) {
      specification <- config$moderated_specs[[specification_index]]
      for (group_index in seq_len(group_count)) {
        rows <- lapply(seq_len(nrow(specification$required_edges)), function(edge_index) {
          edge_row(
            specification$required_edges$lhs[[edge_index]],
            specification$required_edges$rhs[[edge_index]],
            group_index
          )
        })
        if (any(vapply(rows, is.null, logical(1)))) next
        estimates <- vapply(rows, function(row) {
          suppressWarnings(as.numeric(row$est[[1L]]))
        }, numeric(1))
        if (all(is.finite(estimates))) {
          index_values[specification_index, group_index] <- prod(estimates)
        }
      }
    }
  }
  complete_targets <- c(as.numeric(interaction_values), as.numeric(index_values))
  joint_valid <- length(complete_targets) > 0L && all(is.finite(complete_targets))
  list(
    fit_valid = TRUE,
    joint_valid = joint_valid,
    state = if (joint_valid) "valid" else "target_extraction",
    interaction_values = interaction_values,
    index_values = index_values
  )
}

# Stratified case-resampling inference for a jointly fitted multi-group latent
# product-indicator model.  This core deliberately rebuilds products after each
# within-group resample; pooled products would retain the wrong centering origin
# and an ordinary (unstratified) bootstrap would randomize the observed group
# sizes.  The fitted model keeps all loadings equal so that unstandardized
# interaction coefficients and moderated-mediation indices share a group scale.
structural_canvas_multigroup_bootstrap_draw_memory_plan <- function(
  reps, group_count, interaction_target_count,
  moderated_mediation_target_count,
  max_bytes = getOption(
    "statedu.multigroup_bootstrap.max_draw_bytes",
    512 * 1024^2
  )
) {
  count_value <- function(value, name, positive = FALSE) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) != 1L || !is.finite(value) || value != floor(value) ||
        value < if (isTRUE(positive)) 1 else 0) {
      qualifier <- if (isTRUE(positive)) "positive" else "non-negative"
      stop(sprintf("%s must be a finite %s integer.", name, qualifier), call. = FALSE)
    }
    value
  }
  reps <- count_value(reps, "Bootstrap replicate count", positive = TRUE)
  group_count <- count_value(group_count, "Multi-group count", positive = TRUE)
  interaction_target_count <- count_value(
    interaction_target_count, "Latent-interaction target count"
  )
  moderated_mediation_target_count <- count_value(
    moderated_mediation_target_count, "Moderated-mediation target count"
  )
  max_bytes <- suppressWarnings(as.numeric(max_bytes))
  if (length(max_bytes) != 1L || !is.finite(max_bytes) || max_bytes <= 0) {
    stop(
      "Option 'statedu.multigroup_bootstrap.max_draw_bytes' must be one positive finite byte count.",
      call. = FALSE
    )
  }

  target_count <- interaction_target_count + moderated_mediation_target_count
  estimated_cells <- reps * group_count * target_count
  estimated_bytes <- estimated_cells * 8
  list(
    allowed = is.finite(estimated_bytes) && estimated_bytes <= max_bytes,
    estimated_bytes = estimated_bytes,
    max_bytes = max_bytes,
    estimated_mib = estimated_bytes / 1024^2,
    max_mib = max_bytes / 1024^2,
    cells = estimated_cells,
    reps = reps,
    group_count = group_count,
    interaction_target_count = interaction_target_count,
    moderated_mediation_target_count = moderated_mediation_target_count,
    target_count = target_count,
    bytes_per_draw = 8
  )
}

structural_canvas_multigroup_bootstrap_assert_draw_memory <- function(plan) {
  if (!is.list(plan) || !isTRUE(plan$allowed)) {
    estimated_mib <- suppressWarnings(as.numeric(plan$estimated_mib %||% Inf))
    max_mib <- suppressWarnings(as.numeric(plan$max_mib %||% NA_real_))
    reps <- suppressWarnings(as.numeric(plan$reps %||% NA_real_))
    group_count <- suppressWarnings(as.numeric(plan$group_count %||% NA_real_))
    target_count <- suppressWarnings(as.numeric(plan$target_count %||% NA_real_))
    stop(sprintf(
      paste0(
        "Multi-group latent-moderation bootstrap target draws would require approximately ",
        "%.1f MiB (%s replicates x %s groups x %s targets), exceeding the configured ",
        "%.1f MiB limit. Reduce bootstrap replicates or requested moderation effects, ",
        "or raise options(statedu.multigroup_bootstrap.max_draw_bytes = ...) only when ",
        "sufficient memory is available."
      ),
      estimated_mib,
      format(reps, scientific = FALSE, big.mark = ",", trim = TRUE),
      format(group_count, scientific = FALSE, big.mark = ",", trim = TRUE),
      format(target_count, scientific = FALSE, big.mark = ",", trim = TRUE),
      max_mib
    ), call. = FALSE)
  }
  plan
}

structural_canvas_multigroup_moderation_bootstrap <- function(
  syntax, raw_data, group, moderation_definitions, effect_definitions,
  estimator = "MLR", missing = "fiml", std_lv = FALSE, reps, seed,
  ci_method, ml_likelihood = "normal", progress = NULL, cancel = NULL,
  workers = NULL, chunk_size = NULL
) {
  scalar_character <- function(value, default = "") {
    value <- as.character(value %||% default)
    if (length(value) && !is.na(value[[1L]])) value[[1L]] else default
  }
  estimator <- toupper(trimws(scalar_character(estimator, "MLR")))
  if (!estimator %in% c("ML", "MLR")) {
    stop("Multi-group latent-moderation bootstrap requires ML or MLR.")
  }
  ml_likelihood <- tolower(trimws(scalar_character(ml_likelihood, "normal")))
  if (!ml_likelihood %in% c("normal", "wishart")) {
    stop("ML likelihood convention must be 'normal' or 'wishart'.")
  }
  if (!identical(estimator, "ML") && !identical(ml_likelihood, "normal")) {
    stop("Wishart likelihood compatibility mode is available only with conventional ML estimation.")
  }
  missing <- trimws(scalar_character(missing, "fiml"))
  if (!nzchar(missing)) missing <- "fiml"
  reps <- suppressWarnings(as.integer(reps))
  seed <- suppressWarnings(as.integer(seed))
  if (!is.finite(reps) || reps < 2L) stop("Multi-group latent-moderation bootstrap requires at least two replicates.")
  if (!is.finite(seed) || seed < 1L) stop("Multi-group latent-moderation bootstrap requires a positive integer seed.")
  ci_method <- structural_canvas_bootstrap_ci_method(ci_method)
  if (!ci_method %in% c("percentile", "bias_corrected")) {
    stop("Multi-group latent-moderation bootstrap supports percentile or bias-corrected confidence intervals.")
  }
  if (!is.data.frame(raw_data) || nrow(raw_data) < 3L) {
    stop("Multi-group latent-moderation bootstrap requires a non-empty data frame.")
  }
  group <- trimws(scalar_character(group))
  if (!nzchar(group) || !group %in% names(raw_data)) {
    stop("A valid grouping variable is required for multi-group latent-moderation bootstrap.")
  }
  if (!length(moderation_definitions %||% list())) {
    stop("At least one latent product-indicator moderation definition is required.")
  }
  if (!exists("structural_canvas_prepare_group_product_indicators", mode = "function")) {
    stop("The within-group product-indicator preparation helper is unavailable.")
  }
  if (!exists("structural_canvas_sanitize_multigroup_syntax", mode = "function")) {
    stop("The multi-group syntax sanitizer is unavailable.")
  }
  if (!exists("bootstrap_ci", mode = "function")) {
    stop("The shared bootstrap confidence-interval helper is unavailable.")
  }

  group_missing <- is.na(raw_data[[group]])
  analysis_data <- raw_data[!group_missing, , drop = FALSE]
  observed_values <- analysis_data[[group]]
  observed_character <- as.character(observed_values)
  group_labels <- if (is.factor(raw_data[[group]])) {
    levels(raw_data[[group]])[levels(raw_data[[group]]) %in% unique(observed_character)]
  } else {
    unique(observed_character)
  }
  if (!length(group_labels)) group_labels <- unique(observed_character)
  if (length(group_labels) < 2L || length(group_labels) > 20L) {
    stop("Multi-group latent-moderation bootstrap requires between two and twenty non-empty groups.")
  }
  analysis_data[[group]] <- factor(observed_character, levels = group_labels)
  group_rows <- lapply(group_labels, function(label) {
    which(as.character(analysis_data[[group]]) == label)
  })
  names(group_rows) <- group_labels
  group_sizes <- vapply(group_rows, length, integer(1))
  if (any(group_sizes < 2L)) {
    stop("Every group requires at least two observations for stratified bootstrap resampling.")
  }

  constraint_audit <- if (exists("structural_canvas_multigroup_constraint_audit", mode = "function")) {
    structural_canvas_multigroup_constraint_audit(
      syntax, context = "Multi-group latent-moderation bootstrap"
    )
  } else {
    NULL
  }
  group_syntax <- structural_canvas_sanitize_multigroup_syntax(
    syntax,
    constraint_audit = constraint_audit,
    context = "Multi-group latent-moderation bootstrap"
  )

  interaction_specs <- lapply(moderation_definitions %||% list(), function(definition) {
    predictor <- scalar_character(definition$predictor)
    moderator <- scalar_character(definition$moderator)
    outcome <- scalar_character(definition$outcome)
    interaction_factor <- scalar_character(definition$interaction_factor)
    if (any(!nzchar(c(predictor, moderator, outcome, interaction_factor)))) {
      stop("A multi-group moderation definition is missing its predictor, moderator, outcome, or interaction factor.")
    }
    list(
      interaction_path = paste0(predictor, " × ", moderator, " → ", outcome),
      predictor = predictor, moderator = moderator, outcome = outcome,
      interaction_factor = interaction_factor,
      edge = data.frame(lhs = outcome, rhs = interaction_factor, stringsAsFactors = FALSE)
    )
  })
  interaction_keys <- vapply(interaction_specs, function(specification) {
    paste(specification$outcome, specification$interaction_factor, sep = "\r")
  }, character(1))
  interaction_specs <- interaction_specs[!duplicated(interaction_keys)]

  moderated_specs <- list()
  for (effect in effect_definitions %||% list()) {
    if (!identical(scalar_character(effect$type), "Indirect")) next
    for (path in effect$paths %||% list()) {
      path <- as.character(path %||% character(0))
      if (length(path) < 3L) next
      candidates <- list()
      for (interaction in interaction_specs) {
        moderated_positions <- which(
          path[-length(path)] == interaction$predictor &
            path[-1L] == interaction$outcome
        )
        if (length(moderated_positions) != 1L) next
        candidates[[length(candidates) + 1L]] <- list(
          interaction = interaction, position = moderated_positions[[1L]],
          key = paste(moderated_positions[[1L]], interaction$interaction_factor, sep = "\r")
        )
      }
      if (length(candidates)) {
        candidate_keys <- vapply(candidates, function(item) item$key, character(1))
        candidates <- candidates[!duplicated(candidate_keys)]
      }
      # Match the analytic estimand boundary: the implemented index supports
      # exactly one moderated stage in an indirect chain.  Multiple moderated
      # stages require a W-specific conditional derivative with cross-products.
      if (length(candidates) != 1L) next
      interaction <- candidates[[1L]]$interaction
      moderated_position <- candidates[[1L]]$position
      required_edges <- do.call(rbind, lapply(seq_len(length(path) - 1L), function(edge_position) {
        if (identical(edge_position, moderated_position)) {
          data.frame(
            lhs = interaction$outcome,
            rhs = interaction$interaction_factor,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            lhs = path[[edge_position + 1L]],
            rhs = path[[edge_position]],
            stringsAsFactors = FALSE
          )
        }
      }))
      moderated_specs[[length(moderated_specs) + 1L]] <- list(
        indirect_path = paste(path, collapse = " → "),
        moderated_path = paste0(
          interaction$predictor, " × ", interaction$moderator,
          " → ", interaction$outcome
        ),
        predictor = path[[1L]],
        outcome = path[[length(path)]],
        moderator = interaction$moderator,
        interaction_factor = interaction$interaction_factor,
        required_edges = required_edges
      )
    }
  }
  if (length(moderated_specs)) {
    moderated_keys <- vapply(moderated_specs, function(specification) {
      paste(
        specification$indirect_path, specification$moderated_path,
        specification$moderator, specification$interaction_factor,
        sep = "\r"
      )
    }, character(1))
    moderated_specs <- moderated_specs[!duplicated(moderated_keys)]
  }

  group_count <- length(group_labels)
  draw_memory_plan <- structural_canvas_multigroup_bootstrap_assert_draw_memory(
    structural_canvas_multigroup_bootstrap_draw_memory_plan(
      reps = reps,
      group_count = group_count,
      interaction_target_count = length(interaction_specs),
      moderated_mediation_target_count = length(moderated_specs)
    )
  )

  fit_model <- function(data) {
    arguments <- list(
      model = group_syntax,
      data = data,
      group = group,
      group.label = group_labels,
      group.equal = "loadings",
      estimator = estimator,
      missing = missing,
      std.lv = isTRUE(std_lv),
      auto.cov.lv.x = FALSE
    )
    if (identical(estimator, "ML")) arguments$likelihood <- ml_likelihood
    suppressWarnings(do.call(lavaan::sem, arguments))
  }
  fit_state <- function(fit) {
    if (!inherits(fit, "lavaan")) return(list(valid = FALSE, state = "fit_error", reasons = "No lavaan fit was returned."))
    converged <- isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"), error = function(error) FALSE))
    if (!converged) return(list(valid = FALSE, state = "nonconverged", reasons = "The model did not converge."))
    admissibility <- if (exists("structural_canvas_fit_admissibility", mode = "function")) {
      tryCatch(
        structural_canvas_fit_admissibility(fit),
        error = function(error) list(admissible = FALSE, reasons = conditionMessage(error))
      )
    } else {
      list(
        admissible = isTRUE(tryCatch(lavaan::lavInspect(fit, "post.check"), error = function(error) FALSE)),
        reasons = character(0)
      )
    }
    list(
      valid = isTRUE(admissibility$admissible),
      state = if (isTRUE(admissibility$admissible)) "valid" else "inadmissible",
      reasons = as.character(admissibility$reasons %||% character(0))
    )
  }
  extract_targets <- function(fit) {
    parameters <- tryCatch(lavaan::parameterTable(fit), error = function(error) data.frame())
    required_columns <- c("lhs", "op", "rhs", "group", "free", "est")
    if (!is.data.frame(parameters) || !all(required_columns %in% names(parameters))) return(NULL)
    regressions <- parameters[parameters$op == "~", required_columns, drop = FALSE]
    edge_row <- function(lhs, rhs, group_index) {
      rows <- regressions[
        regressions$lhs == lhs & regressions$rhs == rhs & regressions$group == group_index,
        , drop = FALSE
      ]
      if (nrow(rows) != 1L) return(NULL)
      rows[1L, , drop = FALSE]
    }
    interaction_values <- matrix(
      NA_real_, nrow = length(interaction_specs), ncol = length(group_labels),
      dimnames = list(NULL, group_labels)
    )
    interaction_fixed <- matrix(
      FALSE, nrow = length(interaction_specs), ncol = length(group_labels),
      dimnames = list(NULL, group_labels)
    )
    for (specification_index in seq_along(interaction_specs)) {
      specification <- interaction_specs[[specification_index]]
      for (group_index in seq_along(group_labels)) {
        row <- edge_row(specification$outcome, specification$interaction_factor, group_index)
        if (is.null(row)) next
        interaction_values[specification_index, group_index] <- suppressWarnings(as.numeric(row$est[[1L]]))
        free <- suppressWarnings(as.integer(row$free[[1L]]))
        interaction_fixed[specification_index, group_index] <- is.finite(free) && free == 0L
      }
    }
    index_values <- matrix(
      NA_real_, nrow = length(moderated_specs), ncol = length(group_labels),
      dimnames = list(NULL, group_labels)
    )
    index_fixed <- matrix(
      FALSE, nrow = length(moderated_specs), ncol = length(group_labels),
      dimnames = list(NULL, group_labels)
    )
    for (specification_index in seq_along(moderated_specs)) {
      specification <- moderated_specs[[specification_index]]
      for (group_index in seq_along(group_labels)) {
        rows <- lapply(seq_len(nrow(specification$required_edges)), function(edge_index) {
          edge_row(
            specification$required_edges$lhs[[edge_index]],
            specification$required_edges$rhs[[edge_index]],
            group_index
          )
        })
        if (any(vapply(rows, is.null, logical(1)))) next
        estimates <- vapply(rows, function(row) suppressWarnings(as.numeric(row$est[[1L]])), numeric(1))
        free <- vapply(rows, function(row) suppressWarnings(as.integer(row$free[[1L]])), integer(1))
        if (!all(is.finite(estimates))) next
        index_values[specification_index, group_index] <- prod(estimates)
        fixed <- is.finite(free) & free == 0L
        index_fixed[specification_index, group_index] <-
          all(fixed) || any(fixed & estimates == 0)
      }
    }
    list(
      interaction_values = interaction_values,
      interaction_fixed = interaction_fixed,
      index_values = index_values,
      index_fixed = index_fixed
    )
  }

  original_prepared <- structural_canvas_prepare_group_product_indicators(
    analysis_data, group, moderation_definitions
  )
  original_fit <- tryCatch(fit_model(original_prepared$data), error = identity)
  if (inherits(original_fit, "error")) {
    stop(paste0("The original multi-group latent-moderation model could not be fitted: ", conditionMessage(original_fit)))
  }
  original_state <- fit_state(original_fit)
  if (!isTRUE(original_state$valid)) {
    reason <- paste(unique(original_state$reasons[nzchar(original_state$reasons)]), collapse = "; ")
    if (nzchar(reason)) reason <- paste0(" ", reason)
    stop(paste0(
      "The original multi-group latent-moderation model was ", original_state$state,
      "; bootstrap inference was not started.", reason
    ))
  }
  fitted_group_labels <- as.character(lavaan::lavInspect(original_fit, "group.label") %||% character(0))
  if (!identical(fitted_group_labels, group_labels)) {
    stop("The fitted lavaan group order did not match the stratified bootstrap group order.")
  }
  original_targets <- extract_targets(original_fit)
  if (is.null(original_targets) || any(!is.finite(original_targets$interaction_values))) {
    stop("The original model did not provide one finite latent-interaction coefficient in every group.")
  }
  if (length(moderated_specs) && any(!is.finite(original_targets$index_values))) {
    stop("The original model did not provide every group-specific moderated-mediation index required by the indirect paths.")
  }

  interaction_draws <- matrix(
    NA_real_, nrow = reps, ncol = length(interaction_specs) * group_count
  )
  index_draws <- matrix(
    NA_real_, nrow = reps, ncol = length(moderated_specs) * group_count
  )
  target_position <- function(specification_index, group_index) {
    (specification_index - 1L) * group_count + group_index
  }
  failure_counts <- c(
    product_preparation = 0L, fit_error = 0L, nonconverged = 0L,
    inadmissible = 0L, target_extraction = 0L
  )
  fit_valid_mask <- rep(FALSE, reps)
  joint_valid_mask <- rep(FALSE, reps)
  valid_fits <- 0L
  old_rng_kind <- RNGkind()
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_rng_kind))
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  set.seed(seed)
  if (is.function(progress)) progress(0L, reps, 0L)
  workers <- if (is.null(workers)) 1L else structural_canvas_effect_bootstrap_workers(workers)
  workers <- max(1L, min(as.integer(workers), reps))
  if (is.null(chunk_size)) chunk_size <- max(workers * 2L, min(250L, ceiling(reps / 100L)))
  chunk_size <- max(workers, suppressWarnings(as.integer(chunk_size)))
  worker_config <- list(
    analysis_data = analysis_data,
    group = group,
    group_labels = group_labels,
    group_syntax = group_syntax,
    moderation_definitions = moderation_definitions,
    interaction_specs = interaction_specs,
    moderated_specs = moderated_specs,
    estimator = estimator,
    missing = missing,
    std_lv = isTRUE(std_lv),
    ml_likelihood = ml_likelihood
  )
  cluster <- NULL
  if (workers > 1L) {
    cluster <- parallel::makePSOCKcluster(workers)
    on.exit(try(parallel::stopCluster(cluster), silent = TRUE), add = TRUE)
    project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
    initialized <- parallel::clusterCall(cluster, function(project_dir) {
      setwd(project_dir)
      if (.Platform$OS.type == "windows") {
        suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE))
      }
      options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
      source(file.path("R", "utils.R"), encoding = "UTF-8")
      source(file.path("R", "setup_custom_model_canvas_structural_core.R"), encoding = "UTF-8")
      source(file.path("R", "setup_custom_model_canvas_structural_evaluation.R"), encoding = "UTF-8")
      source(file.path("R", "setup_custom_model_canvas_structural_invariance_evaluation.R"), encoding = "UTF-8")
      source(file.path("R", "setup_custom_model_canvas_structural_lavaan_syntax.R"), encoding = "UTF-8")
      source(file.path("R", "setup_custom_model_canvas_structural_bootstrap.R"), encoding = "UTF-8")
      if (!requireNamespace("lavaan", quietly = TRUE)) stop("The lavaan package is required.")
      try(structural_canvas_lavaan_worker_metadata_fast_path_install(), silent = TRUE)
      TRUE
    }, project_dir)
    if (!all(vapply(initialized, isTRUE, logical(1)))) {
      stop("One or more multi-group latent-moderation bootstrap workers could not be initialized.")
    }
    parallel::clusterExport(cluster, "worker_config", envir = environment())
  }
  chunks <- split(seq_len(reps), ceiling(seq_len(reps) / chunk_size))
  for (positions in chunks) {
    if (is.function(cancel) && isTRUE(cancel())) stop("Multi-group latent-moderation bootstrap canceled.")
    # Generate only the current chunk's resamples.  Keeping the RNG stream in
    # the controller preserves the established serial draw order and gives
    # bit-for-bit identical samples to every worker count, without retaining a
    # reps-by-N index matrix (which is prohibitive at 50,000 replicates).
    chunk_sample_indices <- lapply(positions, function(replicate_index) {
      unlist(lapply(group_rows, function(rows) {
        sample(rows, length(rows), replace = TRUE)
      }), use.names = FALSE)
    })
    items <- if (is.null(cluster)) {
      lapply(chunk_sample_indices, function(indices) {
        structural_canvas_multigroup_moderation_bootstrap_worker(indices, worker_config)
      })
    } else {
      parallel::parLapplyLB(cluster, chunk_sample_indices, function(indices) {
        structural_canvas_multigroup_moderation_bootstrap_worker(indices, worker_config)
      })
    }
    for (local_index in seq_along(positions)) {
      replicate_index <- positions[[local_index]]
      item <- items[[local_index]]
      state <- as.character(item$state %||% "fit_error")
      if (!state %in% names(failure_counts) && !identical(state, "valid")) state <- "fit_error"
      if (!isTRUE(item$fit_valid)) {
        failure_counts[[state]] <- failure_counts[[state]] + 1L
        next
      }
      fit_valid_mask[[replicate_index]] <- TRUE
      valid_fits <- valid_fits + 1L
      if (!isTRUE(item$joint_valid)) {
        failure_counts[["target_extraction"]] <- failure_counts[["target_extraction"]] + 1L
        next
      }
      joint_valid_mask[[replicate_index]] <- TRUE
      for (specification_index in seq_along(interaction_specs)) {
        for (group_index in seq_len(group_count)) {
          interaction_draws[
            replicate_index,
            target_position(specification_index, group_index)
          ] <- item$interaction_values[specification_index, group_index]
        }
      }
      if (length(moderated_specs)) {
        for (specification_index in seq_along(moderated_specs)) {
          for (group_index in seq_len(group_count)) {
            index_draws[
              replicate_index,
              target_position(specification_index, group_index)
            ] <- item$index_values[specification_index, group_index]
          }
        }
      }
    }
    if (is.function(progress)) progress(max(positions), reps, valid_fits)
  }

  summarize_draw <- function(point, values, fixed = FALSE) {
    # All reported statistics use the same set of jointly valid replicates.
    # This makes the global valid-rate diagnostic an actual inferential gate
    # rather than a warning that can disagree with statistic-specific output.
    values <- as.numeric(values)[joint_valid_mask]
    values <- values[is.finite(values)]
    valid <- length(values)
    usable <- structural_canvas_bootstrap_inference_usable(valid, reps)
    status <- scalar_character(structural_canvas_bootstrap_status(valid, reps), "Unreliable")
    interval <- c(NA_real_, NA_real_)
    p_value <- NA_real_
    standard_error <- NA_real_
    inference_source <- "Bootstrap requested - inference suppressed"
    if (isTRUE(fixed)) {
      status <- "Fixed effect - no inferential test"
      inference_source <- status
    } else if (usable) {
      interval <- bootstrap_ci(point, values, method = ci_method)
      p_value <- min(
        1,
        2 * min(
          (sum(values <= 0) + 1) / (valid + 1),
          (sum(values >= 0) + 1) / (valid + 1)
        )
      )
      if (valid > 1L) standard_error <- stats::sd(values)
      inference_source <- "Bootstrap (empirical two-sided p)"
    }
    list(
      estimate = as.numeric(point), se = standard_error,
      lower = interval[[1L]], upper = interval[[2L]], p = p_value,
      valid = valid, requested = reps, valid_percent = 100 * valid / reps,
      ci_method = ci_method,
      quantile_type = structural_canvas_bootstrap_quantile_type(ci_method, "structural_effects"),
      status = status, inference_source = inference_source
    )
  }
  add_bh <- function(table) {
    if (!is.data.frame(table)) return(table)
    if (!"bh_adjusted_p" %in% names(table)) table$bh_adjusted_p <- NA_real_
    finite <- is.finite(table$p)
    if (any(finite)) table$bh_adjusted_p[finite] <- stats::p.adjust(table$p[finite], method = "BH")
    table
  }
  bind_rows <- function(rows, columns) {
    if (length(rows)) {
      result <- do.call(rbind, rows)
      rownames(result) <- NULL
      return(result)
    }
    result <- as.data.frame(stats::setNames(replicate(length(columns), logical(0), simplify = FALSE), columns))
    result
  }
  summary_columns <- c(
    "estimate", "se", "lower", "upper", "p", "bh_adjusted_p", "valid",
    "requested", "valid_percent", "ci_method", "quantile_type", "status",
    "inference_source"
  )

  interaction_rows <- list()
  for (specification_index in seq_along(interaction_specs)) {
    specification <- interaction_specs[[specification_index]]
    for (group_index in seq_len(group_count)) {
      summary <- summarize_draw(
        original_targets$interaction_values[specification_index, group_index],
        interaction_draws[, target_position(specification_index, group_index)],
        original_targets$interaction_fixed[specification_index, group_index]
      )
      interaction_rows[[length(interaction_rows) + 1L]] <- data.frame(
        interaction_path = specification$interaction_path,
        predictor = specification$predictor,
        moderator = specification$moderator,
        outcome = specification$outcome,
        group = group_labels[[group_index]],
        estimate = summary$estimate, se = summary$se,
        lower = summary$lower, upper = summary$upper, p = summary$p,
        bh_adjusted_p = NA_real_, valid = summary$valid,
        requested = summary$requested, valid_percent = summary$valid_percent,
        ci_method = summary$ci_method, quantile_type = summary$quantile_type,
        status = summary$status, inference_source = summary$inference_source,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  group_interactions <- add_bh(bind_rows(
    interaction_rows,
    c("interaction_path", "predictor", "moderator", "outcome", "group", summary_columns)
  ))

  group_pairs <- utils::combn(seq_len(group_count), 2L, simplify = FALSE)
  interaction_difference_rows <- list()
  for (specification_index in seq_along(interaction_specs)) {
    specification <- interaction_specs[[specification_index]]
    for (pair in group_pairs) {
      first <- pair[[1L]]
      second <- pair[[2L]]
      first_point <- original_targets$interaction_values[specification_index, first]
      second_point <- original_targets$interaction_values[specification_index, second]
      difference_draws <-
        interaction_draws[, target_position(specification_index, first)] -
        interaction_draws[, target_position(specification_index, second)]
      summary <- summarize_draw(
        first_point - second_point,
        difference_draws,
        original_targets$interaction_fixed[specification_index, first] &&
          original_targets$interaction_fixed[specification_index, second]
      )
      interaction_difference_rows[[length(interaction_difference_rows) + 1L]] <- data.frame(
        interaction_path = specification$interaction_path,
        predictor = specification$predictor,
        moderator = specification$moderator,
        outcome = specification$outcome,
        group_1 = group_labels[[first]], group_2 = group_labels[[second]],
        estimate_group_1 = first_point, estimate_group_2 = second_point,
        difference = summary$estimate, se = summary$se,
        lower = summary$lower, upper = summary$upper, p = summary$p,
        bh_adjusted_p = NA_real_, valid = summary$valid,
        requested = summary$requested, valid_percent = summary$valid_percent,
        ci_method = summary$ci_method, quantile_type = summary$quantile_type,
        status = summary$status, inference_source = summary$inference_source,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  interaction_differences <- add_bh(bind_rows(
    interaction_difference_rows,
    c(
      "interaction_path", "predictor", "moderator", "outcome", "group_1", "group_2",
      "estimate_group_1", "estimate_group_2", "difference", setdiff(summary_columns, "estimate")
    )
  ))

  index_rows <- list()
  for (specification_index in seq_along(moderated_specs)) {
    specification <- moderated_specs[[specification_index]]
    for (group_index in seq_len(group_count)) {
      summary <- summarize_draw(
        original_targets$index_values[specification_index, group_index],
        index_draws[, target_position(specification_index, group_index)],
        original_targets$index_fixed[specification_index, group_index]
      )
      index_rows[[length(index_rows) + 1L]] <- data.frame(
        indirect_path = specification$indirect_path,
        moderated_path = specification$moderated_path,
        predictor = specification$predictor,
        outcome = specification$outcome,
        moderator = specification$moderator,
        group = group_labels[[group_index]],
        estimate = summary$estimate, se = summary$se,
        lower = summary$lower, upper = summary$upper, p = summary$p,
        bh_adjusted_p = NA_real_, valid = summary$valid,
        requested = summary$requested, valid_percent = summary$valid_percent,
        ci_method = summary$ci_method, quantile_type = summary$quantile_type,
        status = summary$status, inference_source = summary$inference_source,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  group_indices <- add_bh(bind_rows(
    index_rows,
    c("indirect_path", "moderated_path", "predictor", "outcome", "moderator", "group", summary_columns)
  ))

  index_difference_rows <- list()
  for (specification_index in seq_along(moderated_specs)) {
    specification <- moderated_specs[[specification_index]]
    for (pair in group_pairs) {
      first <- pair[[1L]]
      second <- pair[[2L]]
      first_point <- original_targets$index_values[specification_index, first]
      second_point <- original_targets$index_values[specification_index, second]
      difference_draws <-
        index_draws[, target_position(specification_index, first)] -
        index_draws[, target_position(specification_index, second)]
      summary <- summarize_draw(
        first_point - second_point,
        difference_draws,
        original_targets$index_fixed[specification_index, first] &&
          original_targets$index_fixed[specification_index, second]
      )
      index_difference_rows[[length(index_difference_rows) + 1L]] <- data.frame(
        indirect_path = specification$indirect_path,
        moderated_path = specification$moderated_path,
        predictor = specification$predictor,
        outcome = specification$outcome,
        moderator = specification$moderator,
        group_1 = group_labels[[first]], group_2 = group_labels[[second]],
        estimate_group_1 = first_point, estimate_group_2 = second_point,
        difference = summary$estimate, se = summary$se,
        lower = summary$lower, upper = summary$upper, p = summary$p,
        bh_adjusted_p = NA_real_, valid = summary$valid,
        requested = summary$requested, valid_percent = summary$valid_percent,
        ci_method = summary$ci_method, quantile_type = summary$quantile_type,
        status = summary$status, inference_source = summary$inference_source,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }
  pairwise_differences <- add_bh(bind_rows(
    index_difference_rows,
    c(
      "indirect_path", "moderated_path", "predictor", "outcome", "moderator",
      "group_1", "group_2", "estimate_group_1", "estimate_group_2", "difference",
      setdiff(summary_columns, "estimate")
    )
  ))

  joint_valid <- sum(joint_valid_mask)
  diagnostics <- list(
    requested = reps,
    fit_valid_replicates = sum(fit_valid_mask),
    fit_valid_percent = 100 * sum(fit_valid_mask) / reps,
    joint_valid_replicates = joint_valid,
    joint_valid_percent = 100 * joint_valid / reps,
    inference_usable = structural_canvas_bootstrap_inference_usable(joint_valid, reps),
    status = scalar_character(structural_canvas_bootstrap_status(joint_valid, reps), "Unreliable"),
    estimator = estimator,
    missing = scalar_character(missing, "fiml"),
    std_lv = isTRUE(std_lv),
    ml_likelihood = if (identical(estimator, "ML")) ml_likelihood else "normal",
    ci_method = ci_method,
    quantile_type = structural_canvas_bootstrap_quantile_type(ci_method, "structural_effects"),
    seed = seed,
    group = group,
    groups = group_labels,
    group_sizes = group_sizes,
    excluded_missing_group_rows = sum(group_missing),
    stratified_resampling = TRUE,
    rng_policy = "RNGkind('Mersenne-Twister', 'Inversion', 'Rejection'); set.seed(seed); for each replicate, sample each group in stored group order with replacement at its observed n_g",
    rng_kind = c(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection"),
    r_version = as.character(getRversion()),
    workers = workers,
    chunk_size = chunk_size,
    products_recomputed_within_group_each_replicate = TRUE,
    product_indicator_policy = original_prepared$policy %||% list(),
    product_indicator_audit = original_prepared$audit %||% data.frame(),
    failure_counts = failure_counts,
    interaction_statistics = nrow(group_interactions),
    moderated_mediation_statistics = nrow(group_indices),
    estimated_draw_storage_bytes = draw_memory_plan$estimated_bytes,
    draw_storage_limit_bytes = draw_memory_plan$max_bytes,
    estimated_draw_storage_mib = draw_memory_plan$estimated_mib,
    draw_storage_limit_mib = draw_memory_plan$max_mib,
    draws_exposed = FALSE,
    constraint_audit = constraint_audit
  )
  list(
    group_indices = group_indices,
    pairwise_differences = pairwise_differences,
    group_interactions = group_interactions,
    interaction_differences = interaction_differences,
    diagnostics = diagnostics
  )
}

structural_canvas_apply_multigroup_moderation_bootstrap <- function(result, bootstrap) {
  if (!is.list(result) || !is.list(bootstrap)) return(result)
  if (exists("structural_canvas_enforce_product_factor_joint_gate", mode = "function")) {
    result <- structural_canvas_enforce_product_factor_joint_gate(result)
  }
  if (identical(as.character(result$subtype %||% ""), "latent_product_indicator") &&
      !isTRUE((result$product_factor_joint_gate %||% list())$passed)) return(result)
  composite_key <- function(data, columns) {
    if (!is.data.frame(data) || !all(columns %in% names(data))) return(character(0))
    do.call(paste, c(lapply(data[columns], as.character), list(sep = "\r")))
  }
  merge_columns <- function(target, source, keys, mapping) {
    if (!is.data.frame(target) || !nrow(target) || !is.data.frame(source) || !nrow(source)) return(target)
    target_columns <- names(keys)
    if (is.null(target_columns) || any(!nzchar(target_columns))) target_columns <- unname(keys)
    target_key <- composite_key(target, target_columns)
    source_key <- composite_key(source, unname(keys))
    if (!length(target_key) || !length(source_key)) return(target)
    matched <- match(target_key, source_key)
    for (target_name in names(mapping)) {
      source_name <- mapping[[target_name]]
      if (!source_name %in% names(source)) next
      if (!target_name %in% names(target)) target[[target_name]] <- NA
      available <- !is.na(matched)
      target[[target_name]][available] <- source[[source_name]][matched[available]]
    }
    target
  }
  group_interactions <- bootstrap$group_interactions %||% data.frame()
  if (is.data.frame(group_interactions) && nrow(group_interactions)) {
    result$interaction_group_estimates <- merge_columns(
      result$interaction_group_estimates %||% data.frame(), group_interactions,
      c(`Interaction path` = "interaction_path", Group = "group"),
      c(
        `Bootstrap SE` = "se", `Bootstrap CI lower` = "lower",
        `Bootstrap CI upper` = "upper", `Bootstrap p` = "p",
        `Bootstrap BH-adjusted p` = "bh_adjusted_p",
        `Valid replicates` = "valid", `Requested replicates` = "requested",
        `Valid %` = "valid_percent", `CI method` = "ci_method",
        `Quantile type` = "quantile_type", `Bootstrap status` = "status",
        `Bootstrap inference source` = "inference_source"
      )
    )
  }
  interaction_differences <- bootstrap$interaction_differences %||% data.frame()
  if (is.data.frame(interaction_differences) && nrow(interaction_differences)) {
    result$interaction_pairwise_differences <- merge_columns(
      result$interaction_pairwise_differences %||% data.frame(), interaction_differences,
      c(`Interaction path` = "interaction_path", `Group 1` = "group_1", `Group 2` = "group_2"),
      c(
        `B group 1` = "estimate_group_1", `B group 2` = "estimate_group_2",
        `Bootstrap B difference` = "difference", `Bootstrap SE` = "se",
        `Bootstrap CI lower` = "lower", `Bootstrap CI upper` = "upper",
        `Bootstrap p` = "p", `Bootstrap BH-adjusted p` = "bh_adjusted_p",
        `Valid replicates` = "valid", `Requested replicates` = "requested",
        `Valid %` = "valid_percent", `CI method` = "ci_method",
        `Quantile type` = "quantile_type", `Bootstrap status` = "status",
        `Bootstrap inference source` = "inference_source"
      )
    )
  }
  group_indices <- bootstrap$group_indices %||% data.frame()
  if (is.data.frame(group_indices) && nrow(group_indices)) {
    result$moderated_mediation_group_indices <- merge_columns(
      result$moderated_mediation_group_indices %||% data.frame(), group_indices,
      c(
        `Indirect path` = "indirect_path", `Moderated path` = "moderated_path",
        Moderator = "moderator", Group = "group"
      ),
      c(
        Index = "estimate", `Bootstrap SE` = "se",
        `Bootstrap CI lower` = "lower", `Bootstrap CI upper` = "upper",
        `Bootstrap p` = "p", `Bootstrap BH-adjusted p` = "bh_adjusted_p",
        `Valid replicates` = "valid", `Requested replicates` = "requested",
        `Valid %` = "valid_percent", `CI method` = "ci_method",
        `Quantile type` = "quantile_type",
        `Bootstrap inference source` = "inference_source",
        `Bootstrap status` = "status"
      )
    )
  }
  pairwise <- bootstrap$pairwise_differences %||% data.frame()
  if (is.data.frame(pairwise) && nrow(pairwise)) {
    result$moderated_mediation_pairwise_differences <- merge_columns(
      result$moderated_mediation_pairwise_differences %||% data.frame(), pairwise,
      c(
        `Indirect path` = "indirect_path", `Moderated path` = "moderated_path",
        Moderator = "moderator", `Group 1` = "group_1", `Group 2` = "group_2"
      ),
      c(
        `Index group 1` = "estimate_group_1", `Index group 2` = "estimate_group_2",
        `Index difference` = "difference", `Bootstrap SE` = "se",
        `Bootstrap CI lower` = "lower", `Bootstrap CI upper` = "upper",
        `Bootstrap p` = "p", `Bootstrap BH-adjusted p` = "bh_adjusted_p",
        `Valid replicates` = "valid", `Requested replicates` = "requested",
        `Valid %` = "valid_percent", `CI method` = "ci_method",
        `Quantile type` = "quantile_type",
        `Bootstrap inference source` = "inference_source",
        `Bootstrap status` = "status"
      )
    )
  }
  diagnostics <- bootstrap$diagnostics %||% list()
  if (is.list(diagnostics) && length(diagnostics)) {
    failures <- diagnostics$failure_counts %||% integer(0)
    failures <- failures[is.finite(failures) & failures > 0L]
    result$moderated_mediation_bootstrap_diagnostics <- data.frame(
      Requested = as.integer(diagnostics$requested %||% NA_integer_),
      `Fit-valid` = as.integer(diagnostics$fit_valid_replicates %||% NA_integer_),
      `Fit-valid %` = as.numeric(diagnostics$fit_valid_percent %||% NA_real_),
      `Joint-valid` = as.integer(diagnostics$joint_valid_replicates %||% NA_integer_),
      `Joint-valid %` = as.numeric(diagnostics$joint_valid_percent %||% NA_real_),
      `Inference usable` = isTRUE(diagnostics$inference_usable),
      Seed = as.integer(diagnostics$seed %||% NA_integer_),
      RNG = paste(as.character(diagnostics$rng_kind %||% character(0)), collapse = "/"),
      `R version` = as.character(diagnostics$r_version %||% ""),
      `CI method` = as.character(diagnostics$ci_method %||% ""),
      `Quantile type` = as.character(diagnostics$quantile_type %||% ""),
      `Centering scope` = "Within group; product indicators regenerated after every stratified resample",
      `Failure counts` = if (length(failures)) paste(names(failures), failures, sep = "=", collapse = "; ") else "None",
      Status = as.character(diagnostics$status %||% ""),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    result$multigroup_moderation_bootstrap <- bootstrap
    result$product_indicator_policy$bootstrap <- paste(
      "Within-group stratified case resampling; products regenerated in every replicate;",
      paste0("seed=", diagnostics$seed %||% NA_integer_),
      paste0("; requested=", diagnostics$requested %||% NA_integer_),
      paste0("; CI=", diagnostics$ci_method %||% "")
    )
  }
  result
}

structural_canvas_sem_shared_inverse_install <- function(factory) {
  key <- ".statedu_sem_shared_inverse_state"
  if (exists(key, envir = .GlobalEnv, inherits = FALSE)) {
    return(list(applied = FALSE, reason = "already installed"))
  }
  # Construct and compile before changing any binding. Unsupported libraries
  # simply keep their original implementation.
  shared <- tryCatch(factory(), error = function(e) {
    list(available = FALSE, reason = conditionMessage(e))
  })
  if (!isTRUE(shared$available)) return(list(applied = FALSE, reason = shared$reason))
  gradient <- tryCatch(compiler::cmpfun(shared$gradient), error = function(e) NULL)
  if (is.null(gradient)) return(list(applied = FALSE, reason = "compilation unavailable"))
  ns <- asNamespace("lavaan")
  original <- get("lav_model_grad", ns, inherits = FALSE)
  locked <- bindingIsLocked("lav_model_grad", ns)
  restored <- FALSE
  restore <- function() {
    if (restored) return(invisible(FALSE))
    if (bindingIsLocked("lav_model_grad", ns)) unlockBinding("lav_model_grad", ns)
    assign("lav_model_grad", original, ns)
    if (locked) lockBinding("lav_model_grad", ns)
    shared$state$key <- NULL
    shared$state$value <- NULL
    restored <<- TRUE
    invisible(TRUE)
  }
  installed <- FALSE
  on.exit(if (!installed) restore(), add = TRUE)
  if (locked) unlockBinding("lav_model_grad", ns)
  assign("lav_model_grad", gradient, ns)
  if (locked) lockBinding("lav_model_grad", ns)
  assign(key, list(restore = restore), envir = .GlobalEnv)
  installed <- TRUE
  list(applied = TRUE, reason = shared$reason)
}

# Conservative job scope; numerical options and draw generation are unchanged.
structural_canvas_sem_shared_inverse_eligible <- function(prepared, reps, workers) {
 isTRUE(tryCatch({
  if(length(reps)!=1L || !is.numeric(reps) || !is.finite(reps) || reps<200 || reps!=trunc(reps) ||
     length(workers)!=1L || !is.numeric(workers) || !is.finite(workers) || workers<2 || workers!=trunc(workers))return(FALSE)
  if(!is.list(prepared) || !is.data.frame(prepared$data) || anyNA(prepared$data) ||
     !all(vapply(prepared$data,function(x)is.numeric(x)&&is.null(attributes(x))&&all(is.finite(x)),logical(1))) ||
     length(prepared$product_specs)!=1L)return(FALSE)
  spec<-prepared$product_specs[[1L]]
  if(!spec$method %in% c('all_pairs_dmc','matched_pair_dmc') || !is.data.frame(spec$pairs) || !nrow(spec$pairs))return(FALSE)
  fit<-prepared$fit_template;model<-fit@Model
  inherits(fit,'lavaan') && model@estimator=='ML' && model@representation=='LISREL' &&
   model@nblocks==1L && fit@Data@nlevels==1L && isFALSE(fit@Options$std.lv) &&
   !model@ceq.simple.only && !model@categorical && !model@composites && !model@conditional.x &&
   !model@group.w.free && !length(model@rv.ov) && !length(model@rv.lv)
 },error=function(e)FALSE))
}

structural_canvas_sem_shared_inverse_gradient <- function(count_calls = FALSE) {
  make_shared_inverse_gradient <- function(count_calls = FALSE) {
   ns <- asNamespace('lavaan')
   scope <- new.env(parent = ns)
   state <- new.env(parent = emptyenv())
   state$key <- NULL; state$value <- NULL; state$calls <- 0L; state$hits <- 0L
   original_inverse <- get('lav_lisrel_ibinv', ns)
   scope$lav_lisrel_ibinv <- function(mlist = NULL) {
    if (count_calls) state$calls <- state$calls + 1L
    if (!is.null(state$key) && identical(mlist, state$key, num.eq = FALSE)) {
     if (count_calls) state$hits <- state$hits + 1L
     return(state$value)
    }
    diagnostic <- FALSE
    value <- withCallingHandlers(original_inverse(mlist),
     warning = function(w) diagnostic <<- TRUE, message = function(m) diagnostic <<- TRUE)
    if (!diagnostic) { state$key <- mlist; state$value <- value }
    value
   }
   for (name in c('lav_model_sigma', 'lav_lisrel_sigma', 'lav_model_mu',
                  'lav_lisrel_mu', 'lav_lisrel_df_dmlist')) {
    fun <- get(name, ns); environment(fun) <- scope; scope[[name]] <- fun
   }
   scope$.inverse_state <- state
   gradient <- get('lav_model_grad', ns)
   previous_body <- body(gradient)
   body(gradient) <- bquote({
    .inverse_state$key <- NULL
    .inverse_state$value <- NULL
    .(previous_body)
   })
   environment(gradient) <- scope
   list(gradient = gradient, state = state)
  }

 ns <- asNamespace('lavaan'); original <- get('lav_model_grad', ns)
 fallback <- function(reason) list(gradient = original, available = FALSE, reason = reason)
 if (!identical(as.character(utils::packageVersion('lavaan')), '0.7.2')) return(fallback('version'))
 expected <- c(
  lav_model_grad='329157f0aeaf6c966820d33df0d014a57aef4a560b73b591717a19867a298d27',
  lav_lisrel_ibinv='0d02d68248f0c741c0a655f4030e74a2cff9f5234130c93631fd91fbdc5562fa',
  lav_model_sigma='8a965a57b1e877e22872a30c28d8b27e385571533672d125b13e61877490c019',
  lav_lisrel_sigma='e3512a23e4211a69c0d1c8e232c1af17ab0fc2fae5020b7d7cdddf6c9af4fbc7',
  lav_model_mu='e5692d124050b7d43b76352f353471069920ef229b166f50e019b468740e2476',
  lav_lisrel_mu='8798194608f810eb2d29dfffcc63efedd6d56c2214695c11243a172be626280a',
  lav_lisrel_df_dmlist='2f65a50726954246c57ee692e417106542b4aad0b9a689a1a6206891165e7faa')
 compatible <- vapply(names(expected), function(name) {
  if (bindingIsActive(name, ns)) return(FALSE)
  fun <- get(name, ns, inherits = FALSE)
  is.function(fun) && identical(environment(fun), ns) &&
   identical(digest::digest(list(formals(fun), body(fun)), algo = 'sha256'), expected[[name]])
 }, logical(1))
 if (!all(compatible)) return(fallback(paste(names(expected)[!compatible], collapse = ',')))
 shared <- make_shared_inverse_gradient(count_calls)
 scope <- environment(shared$gradient)
 scope$.original_gradient <- original
 old_body <- body(shared$gradient)
 original_call <- as.call(c(list(as.name('.original_gradient')),
  setNames(lapply(names(formals(original)), as.name), names(formals(original)))))
 body(shared$gradient) <- bquote({
  .supported <- tryCatch(isS4(lavmodel) && lavmodel@estimator == 'ML' &&
   lavmodel@representation == 'LISREL' && type == 'free' && !ceq_simple &&
   !lavmodel@ceq.simple.only && !lavmodel@categorical && !lavmodel@composites &&
   !lavmodel@conditional.x && !lavmodel@group.w.free &&
   length(lavmodel@rv.ov) == 0L && length(lavmodel@rv.lv) == 0L &&
   isS4(lavdata) && lavdata@nlevels == 1L &&
   (!lavsamplestats@missing.flag ||
    (length(lavdata@Mp) == lavmodel@nblocks && all(vapply(lavdata@Mp, function(p)
     identical(p$npatterns, 1L) && is.matrix(p$pat) && is.logical(p$pat) &&
      all(p$pat) && !length(p$empty.idx), logical(1))))),
   error = function(e) FALSE)
  if (!isTRUE(.supported)) return(.(original_call))
  .(old_body)
 })
 shared$available <- TRUE; shared$reason <- 'compatible complete single-level ML/LISREL'
 shared
}
