# Structural equation canvas bootstrap helpers.

structural_canvas_htmt_bootstrap <- function(data, indicators_by_factor, reps = 0L, confidence = .95, seed = 20260812L, ordered = character(0), threshold = .85, ci_method = "percentile", progress = NULL, cancel = NULL) {
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

structural_canvas_bollen_stine <- function(fit, reps = 500L, seed = 97531L) {
  reps <- as.integer(reps)
  if (!is.finite(reps) || reps < 1L) stop("Bollen-Stine bootstrap requires at least one resample.")
  eligibility <- structural_canvas_bollen_stine_eligibility(fit)
  if (!isTRUE(eligibility$available)) stop(eligibility$reason)
  observed <- unname(lavaan::fitMeasures(fit, "chisq"))
  draws <- suppressWarnings(lavaan::bootstrapLavaan(
    fit, r = reps, type = "bollen.stine", iseed = as.integer(seed),
    fun = function(candidate) {
      if (!isTRUE(structural_canvas_fit_admissibility(candidate)$admissible)) return(NA_real_)
      unname(lavaan::fitMeasures(candidate, "chisq"))
    }
  ))
  draws <- as.numeric(draws)
  valid_draws <- draws[is.finite(draws)]
  valid <- length(valid_draws)
  exceedances <- if (valid) sum(valid_draws >= observed) else 0L
  trials <- valid + 1L
  successes <- exceedances + 1L
  p_bootstrap <- if (valid) successes / trials else NA_real_
  mcse <- if (valid) sqrt(p_bootstrap * (1 - p_bootstrap) / trials) else NA_real_
  z_95 <- stats::qnorm(.975)
  wilson_denominator <- 1 + z_95^2 / trials
  wilson_center <- if (valid) (p_bootstrap + z_95^2 / (2 * trials)) / wilson_denominator else NA_real_
  wilson_margin <- if (valid) z_95 * sqrt(p_bootstrap * (1 - p_bootstrap) / trials + z_95^2 / (4 * trials^2)) / wilson_denominator else NA_real_
  data.frame(
    `Observed chi-square` = observed,
    `Bootstrap p` = p_bootstrap,
    `Monte Carlo SE` = mcse,
    `Monte Carlo 95% lower` = if (valid) max(0, wilson_center - wilson_margin) else NA_real_,
    `Monte Carlo 95% upper` = if (valid) min(1, wilson_center + wilson_margin) else NA_real_,
    `Valid replicates` = valid,
    `Requested replicates` = reps,
    `Valid %` = 100 * valid / reps,
    Status = structural_canvas_bootstrap_status(valid, reps),
    Seed = as.integer(seed),
    check.names = FALSE
  )
}

structural_canvas_bollen_stine_eligibility <- function(fit) {
  estimator <- toupper(as.character(lavaan::lavInspect(fit, "options")$estimator %||% ""))
  if (!identical(estimator, "ML")) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is available only for ML estimation."))
  if (length(lavaan::lavNames(fit, "ov.ord"))) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is not available for ordered indicators."))
  if (as.integer(lavaan::lavInspect(fit, "ngroups")) != 1L) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is currently available only for single-group CFA."))
  admissibility <- structural_canvas_fit_admissibility(fit)
  if (!isTRUE(admissibility$admissible)) return(list(
    available = FALSE,
    reason = paste0("Bollen-Stine bootstrap requires an admissible fitted model: ", paste(admissibility$reasons, collapse = "; "), ".")
  ))
  degrees_of_freedom <- unname(lavaan::fitMeasures(fit, "df"))
  if (!is.finite(degrees_of_freedom) || degrees_of_freedom <= 0) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is not informative for a saturated model with df = 0."))
  analyzed_data <- as.matrix(lavaan::lavInspect(fit, "data"))
  if (anyNA(analyzed_data)) return(list(available = FALSE, reason = "Bollen-Stine bootstrap requires complete analyzed data in this implementation."))
  list(available = TRUE, reason = "Eligible complete continuous single-group ML model with positive degrees of freedom.")
}

structural_canvas_normalize_missing_option <- function(value) {
  value <- tolower(trimws(as.character(value %||% "")))
  if (value %in% c("fiml", "ml", "direct")) return("ml")
  if (value %in% c("fiml.x", "ml.x")) return("ml.x")
  if (value %in% c("default", "listwise", "")) return("listwise")
  value
}

structural_canvas_reliability_bootstrap <- function(syntax, data, reps = 500L, confidence = .95, seed = 12345L, estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0), formula_mode = "standardized", original_fit = NULL, ci_method = "percentile", progress = NULL, cancel = NULL) {
  reps <- as.integer(reps)
  ci_method <- structural_canvas_bootstrap_ci_method(ci_method)
  if (!is.finite(reps) || reps < 1L) stop("Reliability bootstrap requires at least one resample.")
  if (is.null(original_fit)) original_fit <- tryCatch(lavaan::cfa(
      syntax, data = data, estimator = estimator, missing = missing,
      std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE
    ), error = function(error) stop(paste0("AVE/reliability bootstrap could not fit the original CFA model: ", conditionMessage(error))))
  if (!inherits(original_fit, "lavaan")) stop("AVE/reliability bootstrap original_fit must be a fitted lavaan object.")
  if (as.integer(lavaan::lavInspect(original_fit, "ngroups")) != 1L) stop("AVE/reliability bootstrap currently supports only single-group CFA models.")
  original_options <- lavaan::lavInspect(original_fit, "options")
  original_estimator <- original_options$estimator.orig %||% original_options$estimator
  if (!identical(toupper(as.character(original_estimator)), toupper(as.character(estimator)))) stop("AVE/reliability bootstrap original_fit estimator does not match the requested estimator.")
  fitted_missing <- structural_canvas_normalize_missing_option(original_options$missing)
  requested_missing <- structural_canvas_normalize_missing_option(missing)
  if (!identical(fitted_missing, requested_missing)) stop("AVE/reliability bootstrap original_fit missing-data option does not match the requested missing-data option.")
  if (!identical(isTRUE(original_options$std.lv), isTRUE(std_lv))) stop("AVE/reliability bootstrap original_fit latent-scaling option does not match std_lv.")
  fitted_ordered <- sort(lavaan::lavNames(original_fit, "ov.ord"))
  requested_ordered <- sort(unique(as.character(ordered %||% character(0))))
  if (!identical(fitted_ordered, requested_ordered)) stop("AVE/reliability bootstrap original_fit ordered-indicator specification does not match the requested ordered variables.")
  original_parameterization <- tolower(as.character(original_options$parameterization %||% "delta"))
  if (!original_parameterization %in% c("delta", "theta")) stop("AVE/reliability bootstrap original_fit uses an unsupported parameterization.")
  if (isTRUE(original_options$auto.cov.lv.x) && length(lavaan::lavNames(original_fit, "lv")) > 1L) stop("AVE/reliability bootstrap original_fit enables automatic exogenous latent covariances, but resamples use the explicit canvas covariance specification.")
  fitted_observed <- sort(lavaan::lavNames(original_fit, "ov"))
  syntax_observed <- sort(unique(unlist(regmatches(syntax, gregexpr("[[:alnum:]_.]+", syntax)), use.names = FALSE)))
  if (!all(fitted_observed %in% syntax_observed)) stop("AVE/reliability bootstrap original_fit observed variables do not match the supplied syntax.")
  normalize_user_parameters <- function(parameters) {
    parameters <- parameters[parameters$user == 1L, c("lhs", "op", "rhs", "ustart", "label"), drop = FALSE]
    covariance <- parameters$op == "~~" & parameters$lhs > parameters$rhs
    lhs <- ifelse(covariance, parameters$rhs, parameters$lhs)
    rhs <- ifelse(covariance, parameters$lhs, parameters$rhs)
    fixed <- ifelse(is.finite(parameters$ustart), format(parameters$ustart, digits = 15, scientific = FALSE, trim = TRUE), "free")
    sort(paste(lhs, parameters$op, rhs, fixed, as.character(parameters$label %||% ""), sep = "\r"))
  }
  fitted_structure <- normalize_user_parameters(lavaan::parameterTable(original_fit))
  supplied_parameters <- tryCatch(lavaan::lavaanify(
    syntax, model.type = "cfa", auto = TRUE, std.lv = isTRUE(std_lv),
    auto.fix.first = !isTRUE(std_lv), auto.fix.single = TRUE,
    auto.var = TRUE, auto.cov.lv.x = FALSE, auto.th = TRUE,
    auto.delta = identical(original_parameterization, "delta"),
    parameterization = original_parameterization
  ), error = function(error) stop(paste0("AVE/reliability bootstrap could not parse the supplied syntax: ", conditionMessage(error))))
  supplied_structure <- normalize_user_parameters(supplied_parameters)
  if (!identical(fitted_structure, supplied_structure)) stop("AVE/reliability bootstrap original_fit parameter structure does not match the supplied syntax.")
  fitted_data <- as.matrix(lavaan::lavInspect(original_fit, "data"))
  if (!is.null(colnames(fitted_data))) fitted_data <- fitted_data[, fitted_observed, drop = FALSE]
  supplied_data <- data[, fitted_observed, drop = FALSE]
  supplied_data <- as.data.frame(lapply(supplied_data, function(values) {
    if (is.factor(values)) as.numeric(values) else as.numeric(values)
  }), check.names = FALSE)
  supplied_matrix <- as.matrix(supplied_data)
  colnames(supplied_matrix) <- fitted_observed
  missing_option <- fitted_missing
  if (missing_option %in% c("listwise", "default") && nrow(fitted_data) != nrow(supplied_matrix)) supplied_matrix <- supplied_matrix[stats::complete.cases(supplied_matrix), , drop = FALSE]
  if (!isTRUE(all.equal(fitted_data, supplied_matrix, check.attributes = FALSE))) stop("AVE/reliability bootstrap original_fit does not use the same analyzed observations and values as the supplied data.")
  structural_canvas_validate_model_based_bootstrap(original_fit, "AVE/reliability bootstrap")
  fit_reliability <- function(frame) {
    fit <- tryCatch(lavaan::cfa(
      syntax, data = frame, estimator = estimator, missing = missing,
      std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE,
      parameterization = original_parameterization
    ), error = function(error) NULL)
    if (is.null(fit) || !isTRUE(structural_canvas_fit_admissibility(fit)$admissible)) return(NULL)
    tryCatch(structural_canvas_reliability_estimates(fit, formula_mode), error = function(error) NULL)
  }
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  n <- nrow(data)
  estimates <- vector("list", reps)
  total_iterations <- reps + if (identical(ci_method, "bca")) n else 0L
  progress_step <- max(1L, floor(total_iterations / 100L))
  if (is.function(progress)) progress(0L, total_iterations, 0L)
  for (index in seq_len(reps)) {
    if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
    sampled <- data[sample.int(n, n, replace = TRUE), , drop = FALSE]
    estimates[[index]] <- fit_reliability(sampled)
    if (is.function(progress) && (index == 1L || index == total_iterations || index %% progress_step == 0L)) {
      progress(index, total_iterations, length(Filter(function(value) !is.null(value) && nrow(value), estimates[seq_len(index)])))
    }
  }
  valid <- Filter(function(value) !is.null(value) && nrow(value), estimates)
  if (!length(valid)) return(data.frame())
  combined <- do.call(rbind, lapply(seq_along(valid), function(index) transform(valid[[index]], Replicate = index)))
  original_estimates <- structural_canvas_reliability_estimates(original_fit, formula_mode)
  jackknife <- data.frame()
  if (identical(ci_method, "bca")) {
    jackknife_values <- vector("list", n)
    for (index in seq_len(n)) {
      if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
      jackknife_values[[index]] <- fit_reliability(data[-index, , drop = FALSE])
      completed <- reps + index
      if (is.function(progress) && (completed == total_iterations || completed %% progress_step == 0L)) {
        progress(completed, total_iterations, length(valid))
      }
    }
    jackknife_values <- Filter(function(value) !is.null(value) && nrow(value), jackknife_values)
    if (length(jackknife_values)) jackknife <- do.call(rbind, lapply(seq_along(jackknife_values), function(index) transform(jackknife_values[[index]], Replicate = index)))
  }
  alpha <- (1 - confidence) / 2
  factors <- unique(combined$Factor)
  do.call(rbind, lapply(factors, function(factor) {
    values <- combined[combined$Factor == factor, , drop = FALSE]
    do.call(rbind, lapply(c("AVE", "CR", "Alpha", "Omega"), function(statistic) {
      finite <- values[[statistic]][is.finite(values[[statistic]])]
      original <- original_estimates[original_estimates$Factor == factor, statistic, drop = TRUE]
      original_value <- if (length(original)) as.numeric(original[[1L]]) else NA_real_
      jackknife_finite <- if (nrow(jackknife)) jackknife[jackknife$Factor == factor, statistic, drop = TRUE] else numeric(0)
      interval <- if (identical(ci_method, "bca")) {
        structural_canvas_bca_interval(finite, original_value, jackknife_finite, confidence)
      } else if (length(finite)) {
        c(
          unname(stats::quantile(finite, alpha, names = FALSE)),
          unname(stats::quantile(finite, 1 - alpha, names = FALSE))
        )
      } else c(NA_real_, NA_real_)
      data.frame(Factor = factor, Statistic = statistic,
        Lower = interval[[1L]], Upper = interval[[2L]],
        `CI method` = if (identical(ci_method, "bca")) {
          if (all(is.finite(interval))) "BCa" else "BCa unavailable"
        } else "Percentile",
        `Valid replicates` = length(finite), `Requested replicates` = reps,
        `Valid %` = 100 * length(finite) / reps,
        Status = structural_canvas_bootstrap_status(length(finite), reps),
        check.names = FALSE)
    }))
  }))
}

structural_canvas_validate_model_based_bootstrap <- function(fit, label = "Model-based bootstrap") {
  if (is.null(fit) || !inherits(fit, "lavaan")) stop(paste0(label, " requires a fitted lavaan CFA model."))
  diagnostics <- structural_canvas_fit_admissibility(fit)
  if (!isTRUE(diagnostics$admissible)) stop(paste0(
    label, " requires an admissible original CFA model: ",
    paste(diagnostics$reasons, collapse = "; "), "."
  ))
  invisible(TRUE)
}
