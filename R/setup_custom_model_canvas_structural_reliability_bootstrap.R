# Structural equation canvas AVE/reliability bootstrap helpers.

structural_canvas_normalize_missing_option <- function(value) {
  value <- tolower(trimws(as.character(value %||% "")))
  if (value %in% c("fiml", "ml", "direct")) return("ml")
  if (value %in% c("fiml.x", "ml.x")) return("ml.x")
  if (value %in% c("default", "listwise", "")) return("listwise")
  value
}

# Reliability bootstraps run in the isolated CFA callr worker.  Keep a single
# PSOCK pool alive for all resampling (and BCa jackknife) chunks, and generate
# every row index in the master process so worker count and scheduling cannot
# change the statistical sample or the seed contract.
structural_canvas_reliability_bootstrap_workers <- function(value = NULL, reps = 0L) {
  if (is.null(value)) value <- Sys.getenv("STATEDU_CFA_BOOTSTRAP_WORKERS", "")
  text <- trimws(as.character(value %||% ""))
  automatic <- !length(text) || !nzchar(text[[1L]])
  requested <- suppressWarnings(as.integer(if (automatic) NA_character_ else text[[1L]]))
  available <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (!is.finite(available) || available < 1L) {
    available <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }
  if (!is.finite(available) || available < 1L) available <- 1L
  reps <- suppressWarnings(as.integer(reps))
  if (!is.finite(reps) || reps < 1L) reps <- 1L
  if (automatic) {
    # Small validation and interactive jobs finish sooner without Windows
    # PSOCK startup.  The normal 1,000+ release profiles use the reusable pool.
    requested <- if (available <= 2L || reps < 250L) {
      1L
    } else {
      min(8L, max(1L, available - 1L), max(2L, ceiling(reps / 100L)))
    }
  } else if (!is.finite(requested) || requested < 1L) {
    stop("STATEDU_CFA_BOOTSTRAP_WORKERS/workers must be a positive integer.")
  }
  max(1L, min(as.integer(requested), as.integer(available), reps))
}

structural_canvas_reliability_bootstrap_chunk_size <- function(value = NULL, reps, workers) {
  reps <- suppressWarnings(as.integer(reps))
  workers <- suppressWarnings(as.integer(workers))
  if (is.null(value)) {
    value <- if (workers <= 1L) {
      min(250L, max(20L, ceiling(reps / 20L)))
    } else {
      max(workers * 4L, min(250L, ceiling(reps / 40L)))
    }
  }
  value <- suppressWarnings(as.integer(value))
  if (!is.finite(value) || value < 1L) {
    stop("CFA reliability bootstrap chunk_size must be a positive integer.")
  }
  max(1L, min(value, reps))
}

# Classify one reusable-pool callback without conflating a scientifically
# rejected fit with an unreadable worker result. Only an explicit rejection
# from the full fused/public strict gate is known-invalid. Callback exceptions,
# malformed results, and extractor error paths are unknown and must be retried
# by the unchanged serial full-SE path on the same master-generated sample.
structural_canvas_reliability_bootstrap_callback_result <- function(screening, value_call) {
  unknown <- function() list(status = "unknown", value = NULL)
  if (!is.list(screening)) return(unknown())
  valid <- tryCatch(screening$valid, error = function(error) NULL)
  path <- tryCatch(as.character(screening$screening_path), error = function(error) character(0))
  trusted_path <- length(path) == 1L && !is.na(path) &&
    path %in% c("fused_0_7_2", "public_fallback")
  if (isFALSE(valid)) {
    if (trusted_path) return(list(status = "known_invalid", value = NULL))
    return(unknown())
  }
  if (!isTRUE(valid) || !is.function(value_call)) return(unknown())
  value <- tryCatch(value_call(), error = function(error) NULL)
  if (!is.data.frame(value)) return(unknown())
  list(status = "valid", value = value)
}

# A lavaanList call that fails as a whole remains distinguishable from a valid
# call whose individual callback entries are NULL. NULL here tells the caller
# to fall back for the entire chunk; short funList results are padded with NULL
# so only the missing positions are retried.
structural_canvas_reliability_bootstrap_fast_call <- function(call, expected) {
  expected <- suppressWarnings(as.integer(expected))
  if (!is.function(call) || !is.finite(expected) || expected < 0L) {
    stop("CFA reliability fast call requires a callable and a non-negative expected result count.")
  }
  fit_list <- tryCatch(call(), error = function(error) NULL)
  if (is.null(fit_list) || !inherits(fit_list, "lavaanList")) return(NULL)
  slot_names <- tryCatch(methods::slotNames(fit_list), error = function(error) character(0))
  if (!"funList" %in% slot_names) return(NULL)
  values <- tryCatch(methods::slot(fit_list, "funList"), error = function(error) NULL)
  if (!is.list(values)) return(NULL)
  if (length(values) < expected) length(values) <- expected
  if (length(values) > expected) values <- values[seq_len(expected)]
  values
}

# Preserve replicate order while retrying unknown callback positions only.
# `frames[[i]]` is passed through unchanged, so the retry uses the exact same
# master sample indices and never resamples or advances the seed contract.
structural_canvas_reliability_bootstrap_resolve_fast_items <- function(items, frames, retry) {
  if (!is.list(frames) || !is.function(retry)) {
    stop("CFA reliability fast-item resolution requires frames and a retry function.")
  }
  count <- length(frames)
  if (!is.list(items)) items <- list()
  if (length(items) < count) length(items) <- count
  if (length(items) > count) items <- items[seq_len(count)]
  values <- vector("list", count)
  states <- rep("unknown", count)
  for (index in seq_len(count)) {
    item <- items[[index]]
    status <- if (is.list(item)) {
      tryCatch(as.character(item$status), error = function(error) character(0))
    } else character(0)
    status <- if (length(status) == 1L && !is.na(status)) status else "unknown"
    if (identical(status, "known_invalid")) {
      states[[index]] <- "known_invalid"
      next
    }
    value <- if (identical(status, "valid") && is.data.frame(item$value)) item$value else NULL
    if (!is.null(value)) {
      states[[index]] <- "valid"
      values[index] <- list(value)
      next
    }
    values[index] <- list(retry(frames[[index]], index))
  }
  list(values = values, states = states, retry_indices = which(states == "unknown"))
}

structural_canvas_reliability_bootstrap <- function(syntax, data, reps = 500L, confidence = .95, seed = default_seed(), estimator = "ML", missing = "fiml", std_lv = FALSE, ordered = character(0), formula_mode = "standardized", original_fit = NULL, ci_method = "bias_corrected", progress = NULL, cancel = NULL, ml_likelihood = "normal", workers = NULL, chunk_size = NULL, return_draws = FALSE) {
  reps <- as.integer(reps)
  ci_method <- structural_canvas_bootstrap_ci_method(ci_method)
  if (!is.finite(reps) || reps < 1L) stop("Reliability bootstrap requires at least one resample.")
  parameterization <- if (length(ordered)) "theta" else "delta"
  fit_arguments <- function(model, frame, parameterization_value) {
    arguments <- list(
      model = model, data = frame, estimator = estimator, missing = missing,
      std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE,
      parameterization = parameterization_value
    )
    if (identical(toupper(as.character(estimator)), "ML")) arguments$likelihood <- ml_likelihood
    arguments
  }
  if (is.null(original_fit)) original_fit <- tryCatch(
    do.call(lavaan::cfa, fit_arguments(syntax, data, parameterization)),
    error = function(error) stop(paste0("AVE/reliability bootstrap could not fit the original CFA model: ", conditionMessage(error)))
  )
  if (!inherits(original_fit, "lavaan")) stop("AVE/reliability bootstrap original_fit must be a fitted lavaan object.")
  if (as.integer(lavaan::lavInspect(original_fit, "ngroups")) != 1L) stop("AVE/reliability bootstrap currently supports only single-group CFA models.")
  original_options <- lavaan::lavInspect(original_fit, "options")
  original_estimator <- original_options$estimator.orig %||% original_options$estimator
  if (!identical(toupper(as.character(original_estimator)), toupper(as.character(estimator)))) stop("AVE/reliability bootstrap original_fit estimator does not match the requested estimator.")
  fitted_likelihood <- tolower(as.character(original_options$likelihood %||% "normal"))
  if (identical(toupper(as.character(estimator)), "ML") && !identical(fitted_likelihood, tolower(as.character(ml_likelihood)))) stop("AVE/reliability bootstrap original_fit likelihood convention does not match the requested ML likelihood convention.")
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
  bootstrap_model_df <- tryCatch(
    suppressWarnings(as.numeric(lavaan::fitMeasures(original_fit, "df")[[1L]])),
    error = function(error) NA_real_
  )
  use_isolated_fast_screen <-
    isTRUE(getOption("statedu.isolated_lavaan_bootstrap_worker", FALSE)) &&
    is.function(get0("structural_canvas_effect_bootstrap_extract_fit", mode = "function")) &&
    is.function(get0("structural_canvas_reliability_estimates_bootstrap_fast", mode = "function")) &&
    is.finite(bootstrap_model_df) && bootstrap_model_df >= 0
  fit_reliability <- function(frame) {
    fit <- tryCatch(do.call(lavaan::cfa, fit_arguments(syntax, frame, original_parameterization)), error = function(error) NULL)
    if (is.null(fit)) return(NULL)
    admissible <- if (use_isolated_fast_screen) {
      isTRUE(structural_canvas_effect_bootstrap_extract_fit(
        fit, character(0), list(), bootstrap_model_df
      )$valid)
    } else {
      isTRUE(structural_canvas_fit_admissibility(fit)$admissible)
    }
    if (!admissible) return(NULL)
    tryCatch(
      if (use_isolated_fast_screen) {
        structural_canvas_reliability_estimates_bootstrap_fast(fit, formula_mode)
      } else {
        structural_canvas_reliability_estimates(fit, formula_mode)
      },
      error = function(error) NULL
    )
  }
  total_iterations <- reps + if (identical(ci_method, "bca")) nrow(data) else 0L
  selected_workers <- if (use_isolated_fast_screen) {
    structural_canvas_reliability_bootstrap_workers(workers, reps)
  } else 1L
  selected_chunk_size <- structural_canvas_reliability_bootstrap_chunk_size(
    chunk_size, max(reps, if (identical(ci_method, "bca")) nrow(data) else 1L),
    selected_workers
  )
  make_chunks <- function(count, first = 1L) {
    count <- suppressWarnings(as.integer(count))
    first <- suppressWarnings(as.integer(first))
    if (!is.finite(count) || !is.finite(first) || count < first || first < 1L) return(list())
    starts <- seq.int(first, count, by = selected_chunk_size)
    lapply(starts, function(start) {
      seq.int(start, min(count, start + selected_chunk_size - 1L))
    })
  }
  fit_template <- original_fit
  # Keep the original full-SE template. The unchanged strict CFA gate includes
  # the free-parameter vcov PSD/boundary check; a lean se="none" fit would omit
  # that scientific acceptance criterion even though AVE/CR themselves do not
  # consume standard errors.
  fast_callback <- if (use_isolated_fast_screen) local({
    extract_fit <- structural_canvas_effect_bootstrap_extract_fit
    reliability_fast <- structural_canvas_reliability_estimates_bootstrap_fast
    classify_result <- structural_canvas_reliability_bootstrap_callback_result
    model_df <- bootstrap_model_df
    formula <- formula_mode
    function(fit) {
      screening <- tryCatch(
        extract_fit(fit, character(0), list(), model_df),
        error = function(error) error
      )
      classify_result(screening, function() reliability_fast(fit, formula))
    }
  }) else NULL
  cluster <- NULL
  cluster_metadata_fast_path <- list()
  worker_startup_seconds <- 0
  fast_chunk_failures <- 0L
  fast_item_retries <- 0L
  fit_fast_frames <- function(frames, first_position) {
    seed_value <- as.integer((abs(as.numeric(seed)) + as.numeric(first_position)) %% .Machine$integer.max)
    if (!is.finite(seed_value) || seed_value < 1L) seed_value <- 1L
    values <- structural_canvas_reliability_bootstrap_fast_call(
      function() suppressWarnings(lavaan::lavaanList(
        model = fit_template, data_list = frames, cmd = "cfa",
        store_slots = character(0), fun = fast_callback,
        parallel = if (selected_workers > 1L) "snow" else "no",
        ncpus = selected_workers, cl = cluster, iseed = seed_value
      )),
      length(frames)
    )
    if (is.null(values)) {
      fast_chunk_failures <<- fast_chunk_failures + 1L
      return(NULL)
    }
    values
  }
  fit_frames <- function(frames, first_position) {
    # A one-worker lavaanList adds more setup than it saves for small jobs. The
    # low-core path deliberately retains the legacy full-SE cfa-per-draw fit.
    values <- if (use_isolated_fast_screen && selected_workers > 1L) {
      fit_fast_frames(frames, first_position)
    } else NULL
    if (is.null(values)) {
      lapply(frames, function(frame) {
        if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
        fit_reliability(frame)
      })
    } else {
      resolved <- structural_canvas_reliability_bootstrap_resolve_fast_items(
        values, frames,
        function(frame, index) {
          if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
          fit_reliability(frame)
        }
      )
      fast_item_retries <<- fast_item_retries + length(resolved$retry_indices)
      resolved$values
    }
  }
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  n <- nrow(data)
  sample_indices <- replicate(
    reps, sample.int(n, n, replace = TRUE), simplify = FALSE
  )
  estimates <- vector("list", reps)
  if (is.function(progress)) progress(0L, total_iterations, 0L)
  if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
  # Fit the first deterministic draw in the callr process before starting the
  # PSOCK pool. This is a real full-SE/strict result, not synthetic progress,
  # and removes worker startup from the user's time-to-first-completion.
  warmup_started <- Sys.time()
  first_value <- fit_reliability(data[sample_indices[[1L]], , drop = FALSE])
  estimates[[1L]] <- first_value
  valid_count <- as.integer(!is.null(first_value) && is.data.frame(first_value) && nrow(first_value) > 0L)
  warmup_seconds <- as.numeric(difftime(Sys.time(), warmup_started, units = "secs"))
  if (is.function(progress)) progress(1L, total_iterations, valid_count)

  worker_startup_started <- Sys.time()
  if (use_isolated_fast_screen && selected_workers > 1L && reps > 1L) {
    if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
    cluster <- parallel::makePSOCKcluster(rep("localhost", selected_workers))
    on.exit({
      try(parallel::clusterCall(cluster, function() {
        if (exists(".statedu_cfa_lavaan_metadata_fast_path_state", envir = .GlobalEnv, inherits = FALSE)) {
          state <- get(".statedu_cfa_lavaan_metadata_fast_path_state", envir = .GlobalEnv, inherits = FALSE)
          if (is.function(state$restore)) state$restore()
          rm(".statedu_cfa_lavaan_metadata_fast_path_state", envir = .GlobalEnv)
        }
        TRUE
      }), silent = TRUE)
      try(parallel::stopCluster(cluster), silent = TRUE)
    }, add = TRUE)
    installer <- get0("structural_canvas_lavaan_worker_metadata_fast_path_install", mode = "function")
    cluster_metadata_fast_path <- parallel::clusterCall(
      cluster,
      function(extract_fit, reliability_fast, reliability_public, alpha_function, install_fast_path) {
        options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
        suppressPackageStartupMessages(requireNamespace("lavaan", quietly = TRUE))
        assign("structural_canvas_effect_bootstrap_extract_fit", extract_fit, envir = .GlobalEnv)
        assign("structural_canvas_reliability_estimates_bootstrap_fast", reliability_fast, envir = .GlobalEnv)
        assign("structural_canvas_reliability_estimates", reliability_public, envir = .GlobalEnv)
        assign("structural_canvas_cronbach_alpha", alpha_function, envir = .GlobalEnv)
        state <- if (is.function(install_fast_path)) install_fast_path() else list(
          applied = FALSE, reason = "metadata fast path unavailable",
          restore = function() invisible(FALSE)
        )
        assign(".statedu_cfa_lavaan_metadata_fast_path_state", state, envir = .GlobalEnv)
        reason <- if (is.null(state$reason)) "" else as.character(state$reason)
        list(applied = isTRUE(state$applied), reason = reason)
      },
      structural_canvas_effect_bootstrap_extract_fit,
      structural_canvas_reliability_estimates_bootstrap_fast,
      structural_canvas_reliability_estimates,
      structural_canvas_cronbach_alpha,
      installer
    )
  }
  worker_startup_seconds <- as.numeric(difftime(Sys.time(), worker_startup_started, units = "secs"))

  remaining_started <- Sys.time()
  for (positions in make_chunks(reps, first = 2L)) {
    if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
    frames <- lapply(positions, function(index) data[sample_indices[[index]], , drop = FALSE])
    values <- fit_frames(frames, positions[[1L]])
    estimates[positions] <- values
    valid_count <- valid_count + sum(vapply(values, function(value) {
      !is.null(value) && is.data.frame(value) && nrow(value) > 0L
    }, logical(1)))
    completed <- max(positions)
    if (is.function(progress)) progress(completed, total_iterations, valid_count)
  }
  remaining_seconds <- as.numeric(difftime(Sys.time(), remaining_started, units = "secs"))
  resampling_seconds <- warmup_seconds + remaining_seconds
  valid <- Filter(function(value) !is.null(value) && nrow(value), estimates)
  combined <- if (length(valid)) {
    do.call(rbind, lapply(seq_along(valid), function(index) transform(valid[[index]], Replicate = index)))
  } else data.frame()
  original_estimates <- if (use_isolated_fast_screen) {
    structural_canvas_reliability_estimates_bootstrap_fast(original_fit, formula_mode)
  } else structural_canvas_reliability_estimates(original_fit, formula_mode)
  jackknife <- data.frame()
  jackknife_values <- list()
  jackknife_seconds <- 0
  if (identical(ci_method, "bca")) {
    jackknife_values <- vector("list", n)
    jackknife_started <- Sys.time()
    for (positions in make_chunks(n)) {
      if (is.function(cancel) && isTRUE(cancel())) stop("AVE/reliability bootstrap canceled.")
      frames <- lapply(positions, function(index) data[-index, , drop = FALSE])
      jackknife_values[positions] <- fit_frames(frames, reps + positions[[1L]])
      completed <- reps + max(positions)
      if (is.function(progress)) progress(completed, total_iterations, length(valid))
    }
    jackknife_seconds <- as.numeric(difftime(Sys.time(), jackknife_started, units = "secs"))
    valid_jackknife <- Filter(function(value) !is.null(value) && nrow(value), jackknife_values)
    if (length(valid_jackknife)) jackknife <- do.call(rbind, lapply(seq_along(valid_jackknife), function(index) transform(valid_jackknife[[index]], Replicate = index)))
  }
  alpha <- (1 - confidence) / 2
  factors <- unique(combined$Factor)
  summary <- if (length(factors)) do.call(rbind, lapply(factors, function(factor) {
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
  })) else data.frame()
  timing <- list(
    worker_startup = worker_startup_seconds,
    first_resample = warmup_seconds,
    resampling = resampling_seconds,
    jackknife = jackknife_seconds,
    workers = selected_workers,
    chunk_size = selected_chunk_size,
    fast_path = use_isolated_fast_screen,
    fast_chunk_fallbacks = fast_chunk_failures,
    fast_item_retries = fast_item_retries,
    worker_metadata_fast_path = cluster_metadata_fast_path
  )
  attr(summary, "timings") <- timing
  if (isTRUE(return_draws)) {
    return(list(
      summary = summary, estimates = estimates,
      jackknife = jackknife_values, sample_indices = sample_indices,
      timings = timing
    ))
  }
  summary
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
