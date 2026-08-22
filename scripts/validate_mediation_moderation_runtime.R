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
  tryCatch(if (file.exists(path)) readRDS(path) else NULL, error = function(error) NULL)
}

worker_budget <- runtime_budget("STATEDU_RUNTIME_BOOTSTRAP_MAX_SECONDS", 20)
read_budget <- runtime_budget("STATEDU_RUNTIME_RESULT_READ_MAX_SECONDS", 1)
render_budget <- runtime_budget("STATEDU_RUNTIME_RESULT_RENDER_MAX_SECONDS", 5)
finalize_budget <- runtime_budget("STATEDU_RUNTIME_FINALIZE_MAX_SECONDS", 5)

message(sprintf(
  paste0(
    "Runtime budgets: worker %.3fs; result read %.3fs; result HTML render %.3fs; ",
    "finalizing + serializing %.3fs."
  ),
  worker_budget,
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

job <- NULL
cleanup_verified <- FALSE
on.exit({
  if (!is.null(job)) {
    if (!is.null(job$process) && isTRUE(job$process$is_alive())) {
      try(job$process$kill(), silent = TRUE)
    }
    mediation_moderation_cleanup_bootstrap_job(job)
  }
}, add = TRUE)

message("Running 5,000 bootstrap samples for each of two focal X models (10,000 total)...")
worker_started <- proc.time()[["elapsed"]]
job <- mediation_moderation_start_bootstrap_job(list(
  data = fixture,
  roles = roles,
  mediator_arrangement = "parallel",
  moderated_paths = character(0),
  boot_r = 5000L,
  seed = 20260822L,
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
))

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
  if (worker_elapsed > worker_budget) {
    try(job$process$kill(), silent = TRUE)
    stop(sprintf(
      "Custom bootstrap worker exceeded its %.3fs budget (observed %.3fs).",
      worker_budget,
      worker_elapsed
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
  stop(sprintf("Custom bootstrap worker failed with exit code %s: %s", exit_status, worker_error), call. = FALSE)
}
if (worker_elapsed > worker_budget) {
  stop(sprintf(
    "Custom bootstrap worker exceeded its %.3fs budget (observed %.3fs).",
    worker_budget,
    worker_elapsed
  ), call. = FALSE)
}

phase_order <- c(starting = 1L, preparing = 2L, resampling = 3L, finalizing = 4L, serializing = 5L, complete = 6L)
observed_ranks <- unname(phase_order[phase_samples$phase])
if (anyNA(observed_ranks) || is.unsorted(observed_ranks, strictly = FALSE)) {
  stop(sprintf("Bootstrap phases were invalid or regressed: %s", paste(phase_samples$phase, collapse = " -> ")), call. = FALSE)
}
required_phases <- c("starting", "preparing", "resampling", "finalizing", "complete")
missing_phases <- setdiff(required_phases, phase_samples$phase)
if (length(missing_phases) > 0L) {
  stop(sprintf("Bootstrap runtime test did not observe phase(s): %s", paste(missing_phases, collapse = ", ")), call. = FALSE)
}
finalizing_started <- phase_samples$first_seen[match("finalizing", phase_samples$phase)]
finalize_elapsed <- worker_elapsed - finalizing_started
if (!is.finite(finalize_elapsed) || finalize_elapsed > finalize_budget) {
  stop(sprintf(
    "Finalizing + serializing exceeded its %.3fs budget (observed %.3fs).",
    finalize_budget,
    finalize_elapsed
  ), call. = FALSE)
}

message("Observed worker phases:")
for (index in seq_len(nrow(phase_samples))) {
  message(sprintf(
    "  %-11s first=%7.3fs last=%7.3fs progress=%s/%s",
    phase_samples$phase[[index]],
    phase_samples$first_seen[[index]],
    phase_samples$last_seen[[index]],
    phase_samples$done[[index]],
    phase_samples$total[[index]]
  ))
}

if (!file.exists(job$result_file)) {
  stop("Custom bootstrap worker completed without a result file.", call. = FALSE)
}
result_read_started <- proc.time()[["elapsed"]]
result <- readRDS(job$result_file)
result_read_elapsed <- elapsed_seconds(result_read_started)
if (result_read_elapsed > read_budget) {
  stop(sprintf(
    "Bootstrap result read exceeded its %.3fs budget (observed %.3fs).",
    read_budget,
    result_read_elapsed
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
  all(result$effect_bootstrap_diagnostics$Requested == 5000L)
)

# The custom-canvas server marks the result before render so the result panel
# reuses the live canvas instead of generating a second base64 diagram.
result$custom_model_canvas <- TRUE
result_render_started <- proc.time()[["elapsed"]]
result_html <- htmltools::renderTags(
  mediation_moderation_result_ui(result, language = "en", dash_nonsignificant = TRUE)
)$html
result_render_elapsed <- elapsed_seconds(result_render_started)
if (result_render_elapsed > render_budget) {
  stop(sprintf(
    "Bootstrap result HTML render exceeded its %.3fs budget (observed %.3fs).",
    render_budget,
    result_render_elapsed
  ), call. = FALSE)
}
stopifnot(nchar(result_html, type = "bytes") > 1000L)

job_directory <- job$directory
mediation_moderation_cleanup_bootstrap_job(job)
cleanup_verified <- !dir.exists(job_directory)
if (!cleanup_verified) {
  stop(sprintf("Bootstrap job directory was not removed: %s", job_directory), call. = FALSE)
}

end_to_end_elapsed <- worker_elapsed + result_read_elapsed + result_render_elapsed
message(sprintf(
  paste0(
    "Runtime result: worker %.3fs / %.3fs; finalizing + serializing %.3fs / %.3fs; ",
    "result read %.3fs / %.3fs; result HTML render %.3fs / %.3fs; end-to-end %.3fs; cleanup verified=%s."
  ),
  worker_elapsed,
  worker_budget,
  finalize_elapsed,
  finalize_budget,
  result_read_elapsed,
  read_budget,
  result_render_elapsed,
  render_budget,
  end_to_end_elapsed,
  cleanup_verified
))
message("Mediation/moderation runtime regression passed.")
