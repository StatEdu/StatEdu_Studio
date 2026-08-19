# Structural equation canvas AVE/reliability bootstrap helpers.

structural_canvas_normalize_missing_option <- function(value) {
  value <- tolower(trimws(as.character(value %||% "")))
  if (value %in% c("fiml", "ml", "direct")) return("ml")
  if (value %in% c("fiml.x", "ml.x")) return("ml.x")
  if (value %in% c("default", "listwise", "")) return("listwise")
  value
}

structural_canvas_reliability_bootstrap <- function(syntax, data, reps = 500L, confidence = .95, seed = default_seed(), estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0), formula_mode = "standardized", original_fit = NULL, ci_method = "bias_corrected", progress = NULL, cancel = NULL) {
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
      } else if (identical(ci_method, "bias_corrected") && length(finite)) {
        c(
          structural_canvas_bias_corrected_quantile(finite, original_value, alpha),
          structural_canvas_bias_corrected_quantile(finite, original_value, 1 - alpha)
        )
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
        } else if (identical(ci_method, "bias_corrected")) "Bias-corrected (BC)" else "Percentile",
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
