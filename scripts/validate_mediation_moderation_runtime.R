script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0L) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/validate_mediation_moderation_runtime.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)
if (identical(.Platform$OS.type, "windows")) {
  # callr inherits these values. Setting them here keeps direct invocation as
  # reliable as the PowerShell runners when UTF-8 identifiers are sourced.
  Sys.setenv(
    LC_ALL = "English_United States.utf8",
    LANG = "English_United States.utf8"
  )
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE)))
}

suppressPackageStartupMessages(library(shiny))
tags <- htmltools::tags
tagList <- htmltools::tagList
source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "result_labels.R"))
source(file.path(repo_root, "R", "setup_analysis_ui.R"))
source(file.path(repo_root, "R", "result_table_ui.R"))
source(file.path(repo_root, "R", "result_coefficients.R"))
source(file.path(repo_root, "R", "result_panels_ui.R"))
source(file.path(repo_root, "R", "analysis_regression.R"))
source(file.path(repo_root, "R", "setup_mediation_moderation_ui.R"))

# The app applies preferences once when a session starts. Mirror that contract
# here so result formatting does not repeatedly read the preferences file while
# measuring the post-bootstrap render path.
statedu_apply_preferences(statedu_default_preferences())

runtime_budget <- function(name, default) {
  raw <- Sys.getenv(name, unset = "")
  if (!nzchar(raw)) return(as.numeric(default))
  value <- suppressWarnings(as.numeric(raw))
  if (length(value) != 1L || !is.finite(value) || value <= 0) {
    stop(sprintf("%s must be one finite positive number; received '%s'.", name, raw), call. = FALSE)
  }
  value
}

elapsed_seconds <- function(started_at) {
  unname(proc.time()[["elapsed"]] - started_at)
}

record_phase <- function(samples, progress, elapsed) {
  if (!is.list(progress)) return(samples)
  phase <- as.character(progress$phase %||% "")
  if (length(phase) != 1L || !nzchar(phase)) return(samples)
  done <- max(0L, as.integer(progress$done %||% 0L))
  total <- max(1L, as.integer(progress$total %||% 1L))
  if (nrow(samples) > 0L && identical(tail(samples$phase, 1L), phase)) {
    samples$last_seen[[nrow(samples)]] <- elapsed
    samples$done[[nrow(samples)]] <- max(samples$done[[nrow(samples)]], done)
    samples$total[[nrow(samples)]] <- max(samples$total[[nrow(samples)]], total)
    return(samples)
  }
  rbind(
    samples,
    data.frame(
      phase = phase,
      first_seen = elapsed,
      last_seen = elapsed,
      done = done,
      total = total,
      stringsAsFactors = FALSE
    )
  )
}

read_progress <- function(path) {
  tryCatch(if (file.exists(path)) suppressWarnings(readRDS(path)) else NULL, error = function(error) NULL)
}

runtime_phase_first_seen <- function(phase_samples, phase) {
  values <- phase_samples$first_seen[phase_samples$phase == phase]
  if (length(values) != 1L || !is.finite(values[[1L]])) return(NA_real_)
  unname(values[[1L]])
}

runtime_finalize_elapsed <- function(phase_samples) {
  finalizing_first_seen <- runtime_phase_first_seen(phase_samples, "finalizing")
  complete_first_seen <- runtime_phase_first_seen(phase_samples, "complete")
  unname(complete_first_seen - finalizing_first_seen)
}

runtime_finalize_within_budget <- function(phase_samples, budget) {
  value <- runtime_finalize_elapsed(phase_samples)
  is.finite(value) && value >= 0 && value <= budget
}

# A worker may remain alive briefly after publishing complete. That exit delay
# belongs to total worker latency, not to finalizing + serializing. Conversely,
# a genuinely long finalizing -> complete interval must fail the 5-second gate.
synthetic_delayed_exit_phases <- data.frame(
  phase = c("finalizing", "complete"),
  first_seen = c(4, 5),
  stringsAsFactors = FALSE
)
synthetic_delayed_worker_exit <- 20
stopifnot(
  identical(runtime_finalize_elapsed(synthetic_delayed_exit_phases), 1),
  synthetic_delayed_worker_exit - runtime_phase_first_seen(synthetic_delayed_exit_phases, "complete") > 5,
  runtime_finalize_within_budget(synthetic_delayed_exit_phases, 5)
)
synthetic_slow_finalize_phases <- data.frame(
  phase = c("finalizing", "complete"),
  first_seen = c(4, 9.001),
  stringsAsFactors = FALSE
)
stopifnot(!runtime_finalize_within_budget(synthetic_slow_finalize_phases, 5))

worker_budget <- runtime_budget("STATEDU_RUNTIME_BOOTSTRAP_MAX_SECONDS", 20)
if (worker_budget > 20) {
  stop("STATEDU_RUNTIME_BOOTSTRAP_MAX_SECONDS cannot exceed the historical 20-second target.", call. = FALSE)
}
worker_hard_max <- 25
runtime_sample_count <- 3L
read_budget <- runtime_budget("STATEDU_RUNTIME_RESULT_READ_MAX_SECONDS", 1)
render_budget <- runtime_budget("STATEDU_RUNTIME_RESULT_RENDER_MAX_SECONDS", 5)
finalize_budget <- runtime_budget("STATEDU_RUNTIME_FINALIZE_MAX_SECONDS", 5)

message(sprintf(
  paste0(
    "Runtime budgets: %s independent workers; worker median %.3fs; worker hard max %.3fs; ",
    "result read %.3fs; result HTML render %.3fs; finalizing + serializing %.3fs."
  ),
  runtime_sample_count,
  worker_budget,
  worker_hard_max,
  read_budget,
  render_budget,
  finalize_budget
))

message("Building the exact 75-row custom mediation/moderation runtime fixture...")
set.seed(20260822)
n <- 75L
fixture <- data.frame(
  X1 = stats::rnorm(n),
  X2 = stats::rnorm(n),
  C = stats::rnorm(n)
)
fixture$M1 <- 0.45 * fixture$X1 + 0.20 * fixture$X2 + 0.15 * fixture$C + stats::rnorm(n, sd = 0.72)
fixture$M2 <- 0.25 * fixture$X1 + 0.50 * fixture$X2 + 0.25 * fixture$C + stats::rnorm(n, sd = 0.76)
fixture$Y <- 0.18 * fixture$X1 + 0.12 * fixture$X2 + 0.48 * fixture$M1 +
  0.36 * fixture$M2 + 0.10 * fixture$C + stats::rnorm(n, sd = 0.80)

roles <- list(
  y = "Y",
  x = c("X1", "X2"),
  mediators = c("M1", "M2"),
  w = character(0),
  covariates = "C"
)
variable_info <- data.frame(
  name = names(fixture),
  var_label = names(fixture),
  role = "",
  measurement = "continuous",
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(fixture) == 75L,
  length(roles$x) == 2L,
  length(roles$mediators) == 2L,
  length(roles$y) == 1L,
  length(roles$covariates) == 1L
)

runtime_seed <- 20260822L
runtime_args <- list(
  data = fixture,
  roles = roles,
  mediator_arrangement = "parallel",
  moderated_paths = character(0),
  boot_r = 5000L,
  seed = runtime_seed,
  mean_center = FALSE,
  simple_slopes = FALSE,
  johnson_neyman = FALSE,
  analysis_method = "statedu",
  ci_method = "bias_corrected",
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  direct_x = c("X1", "X2"),
  x_to_m = list(M1 = c("X1", "X2"), M2 = c("X1", "X2")),
  m_to_y = list(Y = c("M1", "M2")),
  m_to_m = list(M1 = character(0), M2 = character(0)),
  moderated_x_to_m = list(),
  moderated_m_to_y = list(),
  moderation_map = list(),
  two_moderator_model = "3",
  custom_path_model = TRUE,
  effect_size_models = "y",
  covariate_control = c("y", "m"),
  language = "en",
  variable_info = variable_info,
  labels = character(0),
  category_table = NULL
)

runtime_canonical_result <- function(value) {
  # lm/formula/terms objects retain the worker-local formula environment after
  # RDS serialization. That environment identity is process metadata, not an
  # analysis result. Normalize only environment references on comparison copies;
  # preserve every value, storage mode, class, and other attribute unchanged.
  if (is.environment(value)) return(emptyenv())
  if (is.function(value) && !is.primitive(value)) environment(value) <- emptyenv()
  if (is.recursive(value)) {
    for (index in seq_along(value)) {
      canonical_child <- runtime_canonical_result(value[[index]])
      if (is.null(canonical_child) && is.list(value)) {
        value[index] <- list(NULL)
      } else {
        value[[index]] <- canonical_child
      }
    }
  }
  value_attributes <- attributes(value)
  if (!is.null(value_attributes)) {
    for (index in seq_along(value_attributes)) {
      if (identical(names(value_attributes)[[index]], ".Environment")) {
        value_attributes[[index]] <- emptyenv()
      } else {
        value_attributes[[index]] <- runtime_canonical_result(value_attributes[[index]])
      }
    }
    attributes(value) <- value_attributes
  }
  value
}

runtime_results_identical <- function(current, reference) {
  current <- runtime_canonical_result(current)
  reference <- runtime_canonical_result(reference)
  identical(
    current,
    reference,
    num.eq = FALSE,
    single.NA = FALSE,
    attrib.as.set = FALSE,
    ignore.bytecode = TRUE,
    ignore.environment = FALSE,
    ignore.srcref = TRUE,
    extptr.as.ref = FALSE
  )
}

runtime_seed_verified <- function(result, expected_seed) {
  if (!is.data.frame(result$overview) || !all(c("Item", "Value") %in% names(result$overview))) {
    return(FALSE)
  }
  seed_values <- as.character(result$overview$Value[result$overview$Item == "Seed"])
  identical(seed_values, as.character(expected_seed))
}

run_runtime_sample <- function(sample_index, reference_result = NULL) {
  job <- NULL
  on.exit({
    if (!is.null(job)) {
      if (!is.null(job$process) && isTRUE(tryCatch(job$process$is_alive(), error = function(error) FALSE))) {
        try(statedu_stop_background_process_tree(job$process), silent = TRUE)
      }
      mediation_moderation_cleanup_bootstrap_job(job)
    }
  }, add = TRUE)

  sample_label <- if (identical(sample_index, 1L)) "fresh-worker first-run" else "independent repeat"
  message(sprintf(
    "Runtime sample %s/%s (%s): 5,000 bootstrap samples for each focal X (10,000 total)...",
    sample_index, runtime_sample_count, sample_label
  ))
  parent_rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  worker_started <- proc.time()[["elapsed"]]
  job <- mediation_moderation_start_bootstrap_job(runtime_args)
  stopifnot(job$requested_total == 10000L, job$boot_r == 5000L, dir.exists(job$directory))

  phase_samples <- data.frame(
    phase = character(0), first_seen = numeric(0), last_seen = numeric(0),
    done = integer(0), total = integer(0), stringsAsFactors = FALSE
  )
  phase_samples <- record_phase(
    phase_samples,
    list(phase = "starting", done = 0L, total = job$requested_total),
    elapsed_seconds(worker_started)
  )

  repeat {
    worker_elapsed <- elapsed_seconds(worker_started)
    phase_samples <- record_phase(phase_samples, read_progress(job$progress_file), worker_elapsed)
    if (!isTRUE(job$process$is_alive())) break
    if (worker_elapsed > worker_hard_max) {
      try(statedu_stop_background_process_tree(job$process), silent = TRUE)
      stop(sprintf(
        "Runtime sample %s exceeded the %.3fs hard maximum (observed %.3fs).",
        sample_index, worker_hard_max, worker_elapsed
      ), call. = FALSE)
    }
    Sys.sleep(0.01)
  }

  worker_elapsed <- elapsed_seconds(worker_started)
  phase_samples <- record_phase(phase_samples, read_progress(job$progress_file), worker_elapsed)
  exit_status <- job$process$get_exit_status()
  if (!identical(exit_status, 0L)) {
    worker_error <- if (file.exists(job$error_file)) {
      paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    } else {
      "No worker error file was produced."
    }
    stop(sprintf(
      "Runtime sample %s failed with exit code %s: %s",
      sample_index, exit_status, worker_error
    ), call. = FALSE)
  }
  if (!is.finite(worker_elapsed) || worker_elapsed > worker_hard_max) {
    stop(sprintf(
      "Runtime sample %s exceeded the %.3fs hard maximum (observed %.3fs).",
      sample_index, worker_hard_max, worker_elapsed
    ), call. = FALSE)
  }

  phase_order <- c(starting = 1L, preparing = 2L, resampling = 3L, finalizing = 4L, serializing = 5L, complete = 6L)
  observed_ranks <- unname(phase_order[phase_samples$phase])
  if (anyNA(observed_ranks) || is.unsorted(observed_ranks, strictly = FALSE)) {
    stop(sprintf(
      "Runtime sample %s phases were invalid or regressed: %s",
      sample_index, paste(phase_samples$phase, collapse = " -> ")
    ), call. = FALSE)
  }
  required_phases <- c("starting", "preparing", "resampling", "finalizing", "complete")
  missing_phases <- setdiff(required_phases, phase_samples$phase)
  if (length(missing_phases) > 0L) {
    stop(sprintf(
      "Runtime sample %s did not observe phase(s): %s",
      sample_index, paste(missing_phases, collapse = ", ")
    ), call. = FALSE)
  }
  complete_started <- runtime_phase_first_seen(phase_samples, "complete")
  finalize_elapsed <- runtime_finalize_elapsed(phase_samples)
  post_complete_exit_elapsed <- worker_elapsed - complete_started
  if (!is.finite(finalize_elapsed) || finalize_elapsed < 0 || finalize_elapsed > finalize_budget) {
    stop(sprintf(
      "Runtime sample %s finalizing + serializing exceeded %.3fs (observed %.3fs).",
      sample_index, finalize_budget, finalize_elapsed
    ), call. = FALSE)
  }
  if (!is.finite(post_complete_exit_elapsed) || post_complete_exit_elapsed < 0) {
    stop(sprintf(
      "Runtime sample %s had an invalid post-complete exit interval (observed %.3fs).",
      sample_index, post_complete_exit_elapsed
    ), call. = FALSE)
  }

  message(sprintf("Runtime sample %s worker phases:", sample_index))
  for (phase_index in seq_len(nrow(phase_samples))) {
    message(sprintf(
      "  %-11s first=%7.3fs last=%7.3fs progress=%s/%s",
      phase_samples$phase[[phase_index]],
      phase_samples$first_seen[[phase_index]],
      phase_samples$last_seen[[phase_index]],
      phase_samples$done[[phase_index]],
      phase_samples$total[[phase_index]]
    ))
  }

  if (!file.exists(job$result_file)) {
    stop(sprintf("Runtime sample %s completed without a result file.", sample_index), call. = FALSE)
  }
  result_read_started <- proc.time()[["elapsed"]]
  result <- readRDS(job$result_file)
  result_read_elapsed <- elapsed_seconds(result_read_started)
  if (!is.finite(result_read_elapsed) || result_read_elapsed > read_budget) {
    stop(sprintf(
      "Runtime sample %s result read exceeded %.3fs (observed %.3fs).",
      sample_index, read_budget, result_read_elapsed
    ), call. = FALSE)
  }

  stopifnot(
    is.list(result),
    is.list(result$path_results),
    identical(sort(unique(vapply(
      result$path_results,
      function(path_result) as.character(path_result$focal %||% ""),
      character(1)
    ))), c("X1", "X2")),
    is.data.frame(result$effect_bootstrap_diagnostics),
    all(result$effect_bootstrap_diagnostics$Requested == 5000L),
    runtime_seed_verified(result, runtime_seed)
  )
  parent_rng_after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (!identical(parent_rng_after, parent_rng_before, num.eq = FALSE, single.NA = FALSE)) {
    stop(sprintf("Runtime sample %s changed the parent RNG state.", sample_index), call. = FALSE)
  }
  if (!is.null(reference_result) && !runtime_results_identical(result, reference_result)) {
    canonical_result <- runtime_canonical_result(result)
    canonical_reference <- runtime_canonical_result(reference_result)
    difference <- tryCatch(
      utils::head(all.equal(canonical_result, canonical_reference, tolerance = 0, check.attributes = TRUE), 3L),
      error = function(error) conditionMessage(error)
    )
    stop(sprintf(
      "Runtime sample %s did not reproduce the first sample's exact numeric/result object: %s",
      sample_index, paste(difference, collapse = " | ")
    ), call. = FALSE)
  }

  job_directory <- job$directory
  process_stopped <- !isTRUE(tryCatch(job$process$is_alive(), error = function(error) TRUE))
  mediation_moderation_cleanup_bootstrap_job(job)
  cleanup_verified <- isTRUE(process_stopped) && !dir.exists(job_directory)
  if (!cleanup_verified) {
    stop(sprintf(
      "Runtime sample %s left a worker process or recursive job directory active: %s",
      sample_index, job_directory
    ), call. = FALSE)
  }
  job <- NULL

  message(sprintf(
    paste0(
      "Runtime sample %s: worker %.3fs; finalizing + serializing %.3fs; ",
      "post-complete exit %.3fs; read %.3fs; exact seed/result=%s; process/directory cleanup=%s."
    ),
    sample_index,
    worker_elapsed,
    finalize_elapsed,
    post_complete_exit_elapsed,
    result_read_elapsed,
    TRUE,
    cleanup_verified
  ))
  list(
    result = result,
    worker_elapsed = worker_elapsed,
    finalize_elapsed = finalize_elapsed,
    post_complete_exit_elapsed = post_complete_exit_elapsed,
    result_read_elapsed = result_read_elapsed,
    cleanup_verified = cleanup_verified
  )
}

message(sprintf(
  "Running %s independent fresh worker processes; the first sample represents user-visible first-run startup.",
  runtime_sample_count
))
runtime_samples <- vector("list", runtime_sample_count)
reference_result <- NULL
for (sample_index in seq_len(runtime_sample_count)) {
  sample <- run_runtime_sample(sample_index, reference_result)
  if (is.null(reference_result)) reference_result <- sample$result
  sample$result <- NULL
  runtime_samples[[sample_index]] <- sample
}

worker_elapsed_values <- vapply(runtime_samples, `[[`, numeric(1), "worker_elapsed")
if (length(worker_elapsed_values) != runtime_sample_count || any(!is.finite(worker_elapsed_values))) {
  stop("Runtime gate did not collect three finite independent worker measurements.", call. = FALSE)
}
worker_median <- stats::median(worker_elapsed_values)
worker_max <- max(worker_elapsed_values)
if (!is.finite(worker_median) || worker_median > worker_budget) {
  stop(sprintf(
    "Independent worker median exceeded the historical %.3fs target (observed %.3fs; samples: %s).",
    worker_budget,
    worker_median,
    paste(format(worker_elapsed_values, digits = 5, nsmall = 3), collapse = ", ")
  ), call. = FALSE)
}
if (!is.finite(worker_max) || worker_max > worker_hard_max) {
  stop(sprintf(
    "Independent worker hard maximum exceeded %.3fs (observed %.3fs).",
    worker_hard_max, worker_max
  ), call. = FALSE)
}
if (!all(vapply(runtime_samples, `[[`, logical(1), "cleanup_verified"))) {
  stop("At least one independent runtime sample did not verify process/directory cleanup.", call. = FALSE)
}

# Render the first fresh worker's exact result once. Re-rendering the same
# deterministic result after every timing sample would only lengthen this gate;
# one uncached render preserves the user-visible result latency contract.
render_result <- reference_result
render_result$custom_model_canvas <- TRUE
result_render_started <- proc.time()[["elapsed"]]
result_html <- htmltools::renderTags(
  mediation_moderation_result_ui(render_result, language = "en", dash_nonsignificant = TRUE)
)$html
result_render_elapsed <- elapsed_seconds(result_render_started)
if (!is.finite(result_render_elapsed) || result_render_elapsed > render_budget) {
  stop(sprintf(
    "Fresh result HTML render exceeded %.3fs (observed %.3fs).",
    render_budget, result_render_elapsed
  ), call. = FALSE)
}
stopifnot(nchar(result_html, type = "bytes") > 1000L)

message(sprintf(
  paste0(
    "Runtime aggregate: worker samples=%s; median %.3fs / %.3fs historical target; ",
    "max %.3fs / %.3fs hard ceiling; max finalize-to-complete %.3fs / %.3fs; ",
    "max post-complete exit %.3fs; max read %.3fs / %.3fs; ",
    "fresh render %.3fs / %.3fs; all exact seed/results and recursive process cleanup verified."
  ),
  paste(format(worker_elapsed_values, digits = 5, nsmall = 3), collapse = ", "),
  worker_median,
  worker_budget,
  worker_max,
  worker_hard_max,
  max(vapply(runtime_samples, `[[`, numeric(1), "finalize_elapsed")),
  finalize_budget,
  max(vapply(runtime_samples, `[[`, numeric(1), "post_complete_exit_elapsed")),
  max(vapply(runtime_samples, `[[`, numeric(1), "result_read_elapsed")),
  read_budget,
  result_render_elapsed,
  render_budget
))
message("Mediation/moderation robust runtime regression passed.")
