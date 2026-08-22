# Fail-closed CFA, SEM, and PLS-SEM bootstrap regression validation.
#
# The default `core` mode uses small deterministic repetitions to protect the
# sampling, progress, and reproducibility contracts.  The installer gate sets
# STATEDU_STRUCTURAL_BOOTSTRAP_MODE=installer and runs the real release counts:
# CFA 1,000; SEM 5,000; PLS-SEM 1,000.

options(warn = 1)
source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

for (package in c("lavaan", "seminr", "jsonlite", "callr", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("%s is required for structural bootstrap validation.", package), call. = FALSE)
  }
}

`%named_or%` <- function(value, fallback) {
  if (is.null(value) || !length(value) || is.na(value[[1L]])) fallback else value[[1L]]
}

bootstrap_validation_mode <- tolower(trimws(Sys.getenv("STATEDU_STRUCTURAL_BOOTSTRAP_MODE", "core")))
if (!bootstrap_validation_mode %in% c("core", "installer")) {
  stop(
    "STATEDU_STRUCTURAL_BOOTSTRAP_MODE must be exactly 'core' or 'installer'.",
    call. = FALSE
  )
}
installer_mode <- identical(bootstrap_validation_mode, "installer")
validation_started_at <- Sys.time()
validation_run_id <- trimws(Sys.getenv("STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID", ""))

read_budget <- function(name, default, maximum) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) return(as.numeric(default))
  value <- suppressWarnings(as.numeric(raw))
  if (length(value) != 1L || !is.finite(value) || value <= 0 || value > maximum) {
    stop(
      sprintf(
        "%s must be one finite positive number no greater than %s seconds; received '%s'.",
        name, format(maximum, scientific = FALSE), raw
      ),
      call. = FALSE
    )
  }
  value
}

budgets <- list(
  core_total = read_budget("STATEDU_STRUCTURAL_BOOTSTRAP_CORE_MAX_SECONDS", 120, 600),
  sem_total = read_budget("STATEDU_STRUCTURAL_SEM_MAX_SECONDS", 360, 900),
  cfa_total = read_budget("STATEDU_STRUCTURAL_CFA_MAX_SECONDS", 180, 600),
  pls_total = read_budget("STATEDU_STRUCTURAL_PLS_MAX_SECONDS", 240, 600),
  prepare = read_budget("STATEDU_STRUCTURAL_PREPARE_MAX_SECONDS", 3, 60),
  first_completion = read_budget("STATEDU_STRUCTURAL_FIRST_COMPLETION_MAX_SECONDS", 15, 120),
  summarize = read_budget("STATEDU_STRUCTURAL_SUMMARIZE_MAX_SECONDS", 2, 60)
)

report_path <- trimws(Sys.getenv("STATEDU_STRUCTURAL_BOOTSTRAP_REPORT", ""))
if (installer_mode && !nzchar(report_path)) {
  stop(
    "STATEDU_STRUCTURAL_BOOTSTRAP_REPORT is required in installer mode so measured timings cannot be omitted.",
    call. = FALSE
  )
}
if (nzchar(report_path) && !identical(tolower(tools::file_ext(report_path)), "json")) {
  stop("STATEDU_STRUCTURAL_BOOTSTRAP_REPORT must name a .json file.", call. = FALSE)
}
if (installer_mode && !nzchar(validation_run_id)) {
  stop(
    "STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID is required in installer mode so stale evidence cannot be reused.",
    call. = FALSE
  )
}
if (nzchar(validation_run_id) && (nchar(validation_run_id) > 128L ||
    !grepl("^[A-Za-z0-9._-]+$", validation_run_id))) {
  stop("STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID must be a short filesystem-safe identifier.", call. = FALSE)
}
if (installer_mode && nzchar(report_path) && file.exists(report_path)) {
  unlink(report_path, force = TRUE)
  if (file.exists(report_path)) {
    stop("The previous structural bootstrap report could not be invalidated before validation.", call. = FALSE)
  }
}

elapsed_seconds <- function(started_at) {
  as.numeric(difftime(Sys.time(), started_at, units = "secs"))
}

assert_within_budget <- function(label, actual, budget) {
  if (!is.finite(actual) || actual > budget) {
    stop(
      sprintf("%s took %.3f seconds; fail-closed budget is %.3f seconds.", label, actual, budget),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_measured <- function(label, values, required_length = length(values)) {
  numeric_values <- suppressWarnings(as.numeric(unlist(values, use.names = FALSE)))
  if (length(numeric_values) != required_length || any(!is.finite(numeric_values)) ||
      any(numeric_values < 0)) {
    stop(sprintf("%s contains a missing or invalid required measurement.", label), call. = FALSE)
  }
  invisible(TRUE)
}

installer_exactness_verified <- function(recorded) {
  exactness <- recorded$metrics$exactness
  is.list(exactness) && all(vapply(
    exactness[c(
      "passed", "sem_two_stage_vs_full_se", "sem_seed_reproducible",
      "sem_product_index_vs_legacy", "sem_product_index_fail_open",
      "sem_product_index_missing_guard", "sem_product_index_single_position",
      "sem_fixed_index_vs_legacy", "sem_fixed_index_fail_open",
      "sem_fixed_index_normal_default", "sem_fixed_index_worker_payload_small",
      "cfa_legacy_vs_fast_serial", "cfa_fast_serial_vs_psock", "metadata_restore"
    )], isTRUE, logical(1)
  ))
}

write_verified_structural_report <- function(report, path, run_id) {
  report_directory <- dirname(path)
  if (!dir.exists(report_directory) &&
      !dir.create(report_directory, recursive = TRUE, showWarnings = FALSE)) {
    stop(sprintf("Could not create structural bootstrap report directory: %s", report_directory), call. = FALSE)
  }
  temporary_path <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = report_directory, fileext = ".tmp"
  )
  committed <- FALSE
  on.exit({
    unlink(temporary_path, force = TRUE)
    if (!committed && file.exists(path)) unlink(path, force = TRUE)
  }, add = TRUE)

  jsonlite::write_json(
    report, temporary_path, auto_unbox = TRUE, pretty = TRUE,
    null = "null", digits = 10
  )
  staged <- tryCatch(
    jsonlite::read_json(temporary_path, simplifyVector = TRUE),
    error = function(error) NULL
  )
  if (is.null(staged) || !isTRUE(staged$passed) ||
      !identical(staged$mode, "installer") ||
      !identical(as.character(staged$run_id), run_id) ||
      !installer_exactness_verified(staged)) {
    stop("Structural bootstrap timing report failed staged verification.", call. = FALSE)
  }
  if (file.exists(path)) {
    stop("A structural bootstrap report unexpectedly appeared before atomic publication.", call. = FALSE)
  }
  if (!file.rename(temporary_path, path)) {
    stop("Structural bootstrap timing report could not be atomically published.", call. = FALSE)
  }
  recorded <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(error) NULL
  )
  if (is.null(recorded) || !isTRUE(recorded$passed) ||
      !identical(recorded$mode, "installer") ||
      !identical(as.character(recorded$run_id), run_id) ||
      !installer_exactness_verified(recorded)) {
    stop("Structural bootstrap timing report could not be verified after atomic publication.", call. = FALSE)
  }
  committed <- TRUE
  recorded
}

phase_transition_seconds <- function(events) {
  if (!length(events)) return(list())
  phases <- unique(vapply(events, function(event) as.character(event$phase %||% ""), character(1)))
  phases <- phases[nzchar(phases)]
  stats::setNames(lapply(phases, function(phase) {
    phase_events <- Filter(function(event) identical(as.character(event$phase %||% ""), phase), events)
    min(vapply(phase_events, function(event) as.numeric(event$elapsed %||% NA_real_), numeric(1)), na.rm = TRUE)
  }), phases)
}

assert_progress <- function(events, expected_total, allowed_phases, label) {
  if (!length(events)) stop(sprintf("%s produced no progress events.", label), call. = FALSE)
  completed <- vapply(events, function(event) as.integer(event$completed %||% NA_integer_), integer(1))
  totals <- vapply(events, function(event) as.integer(event$total %||% NA_integer_), integer(1))
  phases <- vapply(events, function(event) as.character(event$phase %||% ""), character(1))
  if (any(!is.finite(completed)) || any(diff(completed) < 0L)) {
    stop(sprintf("%s progress completed count regressed.", label), call. = FALSE)
  }
  if (any(totals != as.integer(expected_total))) {
    stop(sprintf("%s progress total changed during the job.", label), call. = FALSE)
  }
  unknown <- setdiff(unique(phases), allowed_phases)
  if (length(unknown)) {
    stop(sprintf("%s emitted unknown phase(s): %s.", label, paste(unknown, collapse = ", ")), call. = FALSE)
  }
  observed_order <- unique(match(phases, allowed_phases))
  observed_order <- observed_order[is.finite(observed_order)]
  if (length(observed_order) > 1L && any(diff(observed_order) < 0L)) {
    stop(sprintf("%s progress phase regressed.", label), call. = FALSE)
  }
  if (tail(completed, 1L) != as.integer(expected_total)) {
    stop(sprintf("%s did not finish at the requested repetition count.", label), call. = FALSE)
  }
  invisible(TRUE)
}

assert_progress_merge_contract <- function() {
  previous <- list(
    phase = "resampling", completed = 10L, total = 100L,
    valid = 3L, workers = 2L
  )
  rejected <- list(
    NULL,
    "unreadable candidate",
    list(phase = "resampling", completed = 11L, total = 100L),
    list(phase = "resampling", completed = 9L, total = 100L, valid = 3L),
    list(phase = "resampling", completed = 11L, total = 101L, valid = 4L),
    list(phase = "resampling", completed = 11L, total = 100L, valid = 2L),
    list(phase = "resampling", completed = c(11L, 12L), total = 100L, valid = 4L),
    list(phase = "starting_workers", completed = 10L, total = 100L, valid = 3L),
    list(phase = "resampling", completed = 101L, total = 100L, valid = 3L),
    list(phase = "unknown", completed = 11L, total = 100L, valid = 3L)
  )
  for (candidate in rejected) {
    merged <- structural_canvas_effect_bootstrap_progress_merge(previous, candidate)
    if (!identical(merged, previous)) {
      stop(
        "SEM progress cache replaced a valid snapshot with a partial, malformed, or regressive candidate.",
        call. = FALSE
      )
    }
  }
  advanced <- structural_canvas_effect_bootstrap_progress_merge(
    previous,
    list(phase = "resampling", completed = 11L, total = 100L, valid = 4L)
  )
  if (!is.list(advanced) || !identical(advanced$phase, "resampling") ||
      !identical(advanced$completed, 11L) || !identical(advanced$valid, 4L)) {
    stop("SEM progress cache rejected a valid monotonic candidate.", call. = FALSE)
  }
  validating <- structural_canvas_effect_bootstrap_progress_merge(
    advanced,
    list(phase = "validating", completed = 100L, total = 100L, valid = 4L)
  )
  if (!is.list(validating) || !identical(validating$phase, "validating") ||
      !identical(validating$completed, 100L)) {
    stop("SEM progress cache rejected the deferred validation phase.", call. = FALSE)
  }
  if (!identical(
    structural_canvas_effect_bootstrap_progress_merge(
      validating,
      list(phase = "resampling", completed = 100L, total = 100L, valid = 4L)
    ),
    validating
  )) {
    stop("SEM progress cache allowed validation to regress to resampling.", call. = FALSE)
  }

  progress_file <- tempfile("statedu-structural-progress-", fileext = ".rds")
  corrupted_file <- tempfile("statedu-structural-progress-corrupt-", fileext = ".rds")
  on.exit(unlink(c(
    progress_file, paste0(progress_file, ".tmp"), corrupted_file,
    paste0(corrupted_file, ".tmp")
  ), force = TRUE), add = TRUE)
  cached <- NULL
  for (completed in 0:20) {
    structural_canvas_write_effect_bootstrap_progress(
      progress_file, completed = completed, total = 100L,
      valid = min(completed, 4L), phase = "resampling", workers = 2L
    )
    decoded <- structural_canvas_read_bootstrap_progress_snapshot(progress_file)
    if (!is.list(decoded) || !all(c("phase", "completed", "total", "valid") %in% names(decoded))) {
      stop("Repeated atomic SEM progress writes exposed a partial RDS snapshot.", call. = FALSE)
    }
    cached <- structural_canvas_effect_bootstrap_progress_merge(cached, decoded)
  }
  if (!is.list(cached) || !identical(cached$completed, 20L)) {
    stop("Repeated atomic SEM progress writes did not preserve a valid cache.", call. = FALSE)
  }

  # On Windows, a rename/read race can briefly report "Permission denied".
  # The reader must absorb that transient condition (and other unreadable
  # snapshots) without warning or replacing the last monotonic cache value.
  unreadable_progress <- tempfile("statedu-structural-progress-unreadable-")
  dir.create(unreadable_progress)
  on.exit(unlink(unreadable_progress, recursive = TRUE, force = TRUE), add = TRUE)
  progress_warnings <- character(0)
  unreadable_candidate <- withCallingHandlers(
    structural_canvas_read_bootstrap_progress_snapshot(
      unreadable_progress, attempts = 2L, retry_seconds = 0
    ),
    warning = function(warning) {
      progress_warnings <<- c(progress_warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  if (!is.null(unreadable_candidate) || length(progress_warnings) || !identical(
    structural_canvas_effect_bootstrap_progress_merge(previous, unreadable_candidate),
    previous
  )) {
    stop("An unreadable SEM progress snapshot escaped the retrying reader or replaced its cache.", call. = FALSE)
  }

  writeBin(as.raw(c(0x52, 0x44, 0x58, 0x32, 0x0a, 0xff)), corrupted_file)
  corrupted_candidate <- tryCatch(readRDS(corrupted_file), error = function(error) NULL)
  if (!is.null(corrupted_candidate) || !identical(
    structural_canvas_effect_bootstrap_progress_merge(previous, corrupted_candidate),
    previous
  )) {
    stop("A corrupt SEM progress RDS replaced the last valid cached snapshot.", call. = FALSE)
  }
  invisible(TRUE)
}

assert_progress_merge_contract()

assert_sem_screen_fail_open_contract <- function() {
  short_fun_list <- list(list(
    valid = FALSE, raw = NULL, standardized = NULL,
    screening_path = "fused_0_7_2_screen"
  ))
  # Reproduce lavaanList's defensive short-result padding. The new entries are
  # NULL and must proceed to the unchanged full-SE refit, never be discarded.
  length(short_fun_list) <- 3L
  candidates <- c(short_fun_list, list(
    "malformed callback value",
    list(valid = FALSE, screening_path = "screen_error_fail_open"),
    list(valid = FALSE),
    list(valid = NA, screening_path = "fused_0_7_2_screen"),
    list(valid = TRUE, screening_path = "fused_0_7_2_screen")
  ))
  rejected <- vapply(
    candidates,
    structural_canvas_effect_bootstrap_screen_explicit_reject,
    logical(1)
  )
  expected <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  if (!identical(rejected, expected)) {
    stop(
      "SEM no-SE screening did not fail open for a short, NULL, malformed, or fallback callback result.",
      call. = FALSE
    )
  }
  whole_error <- structural_canvas_effect_bootstrap_screen_call(
    function() stop("injected whole-call screening failure"), 4L
  )
  wrong_class <- structural_canvas_effect_bootstrap_screen_call(
    function() structure(list(funList = short_fun_list), class = "lavaanList"), 4L
  )
  for (failure in list(whole_error, wrong_class)) {
    failure_rejections <- vapply(
      failure$values,
      structural_canvas_effect_bootstrap_screen_explicit_reject,
      logical(1)
    )
    if (isTRUE(failure$complete) || length(failure$values) != 4L || any(failure_rejections)) {
      stop(
        "A whole-call or wrong-class SEM screen failure did not nominate every draw for full-SE validation.",
        call. = FALSE
      )
    }
  }
  boundary_seed <- structural_canvas_effect_bootstrap_lavaan_seed(
    .Machine$integer.max - 2L, 10L
  )
  if (!is.integer(boundary_seed) || length(boundary_seed) != 1L ||
      !is.finite(boundary_seed) || boundary_seed < 1L) {
    stop("SEM lavaanList iseed overflow guard returned an invalid seed.", call. = FALSE)
  }
  invisible(TRUE)
}

assert_sem_screen_fail_open_contract()

assert_ci_method_contract <- function() {
  bootstrap_values <- c(
    seq(-2, 0, length.out = 20L),
    exp(seq(-1, 2, length.out = 40L))
  )
  original_value <- 0.7
  confidence <- 0.95
  alpha <- (1 - confidence) / 2
  proportion_less <- mean(bootstrap_values < original_value)
  proportion_less <- min(
    max(proportion_less, 0.5 / length(bootstrap_values)),
    1 - 0.5 / length(bootstrap_values)
  )
  manual_bc_probabilities <- stats::pnorm(
    2 * stats::qnorm(proportion_less) +
      stats::qnorm(c(alpha, 1 - alpha))
  )
  manual_bc <- as.numeric(stats::quantile(
    bootstrap_values, probs = manual_bc_probabilities,
    names = FALSE, type = 6
  ))
  implemented_bc <- bootstrap_ci(
    original_value, bootstrap_values,
    conf = confidence, method = "bias_corrected"
  )
  if (!isTRUE(all.equal(
    implemented_bc, manual_bc,
    tolerance = 0, check.attributes = FALSE
  ))) {
    stop("Bias-corrected CI no longer implements BC percentile (z0 only).", call. = FALSE)
  }
  jackknife_skewed <- (seq_len(30L)^1.7) / 10
  jackknife_symmetric <- seq(-1, 1, length.out = 30L)
  bca_skewed <- structural_canvas_bca_interval(
    bootstrap_values, original_value, jackknife_skewed, confidence
  )
  bca_symmetric <- structural_canvas_bca_interval(
    bootstrap_values, original_value, jackknife_symmetric, confidence
  )
  if (any(!is.finite(c(bca_skewed, bca_symmetric))) ||
      isTRUE(all.equal(bca_skewed, implemented_bc, tolerance = 1e-10)) ||
      isTRUE(all.equal(bca_skewed, bca_symmetric, tolerance = 1e-10))) {
    stop("BCa no longer applies jackknife acceleration separately from BC percentile.", call. = FALSE)
  }
  if (!identical(structural_canvas_bootstrap_ci_method("BC"), "bias_corrected") ||
      !identical(structural_canvas_bootstrap_ci_method("BCa"), "bca") ||
      !identical(structural_canvas_bootstrap_ci_method("percentile"), "percentile")) {
    stop("Bootstrap CI method normalization conflated BC percentile and BCa.", call. = FALSE)
  }
  invisible(TRUE)
}

assert_ci_method_contract()

strip_timing_attributes <- function(value) {
  attr(value, "timings") <- NULL
  value
}

assert_reproducible <- function(first, second, label, tolerance = 1e-12) {
  comparison <- all.equal(first, second, tolerance = tolerance, check.attributes = TRUE)
  if (!isTRUE(comparison)) {
    stop(sprintf("%s is not seed-reproducible: %s", label, paste(comparison, collapse = "; ")), call. = FALSE)
  }
  invisible(TRUE)
}

node <- function(id, role, name) {
  list(
    id = id, role = role, name = name,
    variableId = if (identical(role, "indicator")) name else NULL,
    canvasLabel = name,
    measurementMode = if (identical(role, "latent")) "reflective" else NULL
  )
}

edge <- function(id, from, to) list(id = id, from = from, to = to)

sem_fixture_path <- file.path("sample", "PoliticalDemocracy.csv")
if (!file.exists(sem_fixture_path)) {
  stop(sprintf("The structural bootstrap fixture is missing: %s", sem_fixture_path), call. = FALSE)
}
sem_data <- utils::read.csv(sem_fixture_path, check.names = FALSE, stringsAsFactors = FALSE)
if (!identical(dim(sem_data), c(75L, 11L))) {
  stop("The structural bootstrap fixture must contain exactly 75 rows and 11 variables.", call. = FALSE)
}
sem_snapshot <- list(
  moderationMethod = "all_pairs_dmc",
  nodes = list(
    node("lx", "latent", "dem60"), node("lm", "latent", "dem65"),
    node("ly", "latent", "ind60"), node("lw", "latent", "demW"),
    node("x1", "indicator", "x1"), node("x2", "indicator", "x2"), node("x3", "indicator", "x3"),
    node("y1", "indicator", "y1"), node("y2", "indicator", "y2"), node("y3", "indicator", "y3"),
    node("y4", "indicator", "y4"), node("y5", "indicator", "y5"),
    node("y6", "indicator", "y6"), node("y7", "indicator", "y7"), node("y8", "indicator", "y8")
  ),
  edges = list(
    edge("mx1", "lx", "x1"), edge("mx2", "lx", "x2"), edge("mx3", "lx", "x3"),
    edge("mm1", "lm", "y1"), edge("mm2", "lm", "y2"), edge("mm3", "lm", "y3"),
    edge("mw1", "lw", "y4"), edge("mw2", "lw", "y5"),
    edge("my1", "ly", "y6"), edge("my2", "ly", "y7"), edge("my3", "ly", "y8"),
    edge("p1", "lx", "lm"), edge("p2", "lm", "ly"), edge("p3", "lx", "ly")
  ),
  moderations = list(list(id = "latent_product_moderation", from = "lw", toEdge = "p1"))
)

pls_snapshot <- list(
  nodes = list(
    node("px", "latent", "dem60"), node("py", "latent", "dem65"),
    node("px1", "indicator", "x1"), node("px2", "indicator", "x2"), node("px3", "indicator", "x3"),
    node("py1", "indicator", "y1"), node("py2", "indicator", "y2"), node("py3", "indicator", "y3")
  ),
  edges = list(
    edge("pmx1", "px", "px1"), edge("pmx2", "px", "px2"), edge("pmx3", "px", "px3"),
    edge("pmy1", "py", "py1"), edge("pmy2", "py", "py2"), edge("pmy3", "py", "py3"),
    edge("pp1", "px", "py")
  )
)

message("Preparing representative CFA, SEM, and PLS-SEM fixtures...")
fixture_started <- Sys.time()
sem_original <- suppressWarnings(run_structural_canvas_analysis(
  sem_snapshot, sem_data, "sem", estimator = "ML", missing = "fiml", std_lv = FALSE,
  ordered = character(0), nominal = character(0), residual_variance_fixes = numeric(0)
))
if (!inherits(sem_original$fit, "lavaan") || !isTRUE(sem_original$converged) ||
    length(sem_original$moderation_definitions) != 1L ||
    !identical(sem_original$moderation_definitions[[1L]]$product_indicator_method, "all_pairs_dmc") ||
    sem_original$moderation_definitions[[1L]]$product_indicator_count != 6L) {
  stop("The representative SEM all-pairs DMC fixture was not prepared as specified.", call. = FALSE)
}

cfa_syntax <- "dem60 =~ x1 + x2 + x3"
cfa_fit <- suppressWarnings(lavaan::cfa(
  cfa_syntax, data = sem_data, estimator = "ML", missing = "listwise", auto.cov.lv.x = FALSE
))
if (!isTRUE(lavaan::lavInspect(cfa_fit, "converged"))) {
  stop("The representative CFA fixture did not converge.", call. = FALSE)
}

pls_data <- sem_data[c("x1", "x2", "x3", "y1", "y2", "y3")]
pls_original <- suppressWarnings(run_structural_canvas_analysis(
  pls_snapshot, pls_data, "plssem", estimator = "PLS"
))
if (!inherits(pls_original$fit, "pls_model") || !isTRUE(pls_original$converged)) {
  stop("The representative PLS-SEM fixture did not converge.", call. = FALSE)
}
fixture_seconds <- elapsed_seconds(fixture_started)

prepared_started <- Sys.time()
sem_prepared <- suppressWarnings(structural_canvas_prepare_effect_bootstrap(
  sem_snapshot, sem_data, "sem", "ML", "fiml", FALSE,
  character(0), character(0), numeric(0), original_result = sem_original
))
prepare_seconds <- elapsed_seconds(prepared_started)
if (length(sem_prepared$product_specs) != 1L || nrow(sem_prepared$product_specs[[1L]]$pairs) != 6L) {
  stop("Prepared SEM bootstrap lost the six all-pairs DMC product indicators.", call. = FALSE)
}
sem_template_missing <- tryCatch(
  tolower(as.character(sem_prepared$fit_template@Options$missing[[1L]])),
  error = function(error) ""
)
if (!sem_template_missing %in% c("ml", "fiml") ||
    !isTRUE(sem_prepared$fit_template@Options$meanstructure) || anyNA(sem_prepared$data)) {
  stop(
    paste0(
      "The representative DMC fixture must prove the complete-data FIML/meanstructure ",
      "product-index contract (missing='ml' alias, meanstructure=TRUE, no actual NA)."
    ),
    call. = FALSE
  )
}
assert_within_budget("SEM bootstrap synchronous preparation", prepare_seconds, budgets$prepare)

if (!installer_mode) {
  message("Checking SEM background bootstrap cancellation and cleanup...")
  cancellation_job <- structural_canvas_start_effect_bootstrap_job(
    sem_snapshot, sem_data, "sem", "ML", "fiml", FALSE,
    character(0), character(0), numeric(0), reps = 5000L, seed = 20260825L,
    ci_method = "bias_corrected", ml_likelihood = "normal",
    original_result = sem_original, workers = 2L, chunk_size = 250L
  )
  cancellation_directory <- cancellation_job$directory
  cancellation_stopped <- structural_canvas_stop_effect_bootstrap_job(cancellation_job)
  cancellation_job$process$wait(timeout = 5000L)
  if (!isTRUE(cancellation_stopped) || isTRUE(cancellation_job$process$is_alive())) {
    structural_canvas_stop_effect_bootstrap_job(cancellation_job)
    structural_canvas_cleanup_effect_bootstrap_job(cancellation_job)
    stop("SEM bootstrap cancellation did not stop the complete worker process tree.", call. = FALSE)
  }
  structural_canvas_cleanup_effect_bootstrap_job(cancellation_job)
  if (dir.exists(cancellation_directory)) {
    stop("SEM bootstrap cancellation left its temporary job directory behind.", call. = FALSE)
  }
}

message("Checking per-resample DMC equivalence and strict admissibility agreement...")
set.seed(20260822L)
fixed_indices <- sample.int(nrow(sem_data), nrow(sem_data), replace = TRUE)
sampled_base <- sem_data[fixed_indices, , drop = FALSE]
prepared_sample <- structural_canvas_effect_bootstrap_resample_data(
  sem_data, fixed_indices, sem_prepared$product_specs
)
latents <- Filter(function(item) identical(item$role, "latent"), sem_snapshot$nodes)
full_syntax <- structural_canvas_lavaan_syntax(
  sem_snapshot, sampled_base, "sem", latents, sem_snapshot$edges,
  character(0), numeric(0)
)
product_names <- as.character(sem_prepared$product_specs[[1L]]$pairs$name)
if (!all(product_names %in% names(full_syntax$data)) || !all(product_names %in% names(prepared_sample))) {
  stop("DMC equivalence check could not find every generated product indicator.", call. = FALSE)
}
for (product_name in product_names) {
  if (!isTRUE(all.equal(
    prepared_sample[[product_name]], full_syntax$data[[product_name]],
    tolerance = 1e-12, check.attributes = FALSE
  ))) {
    stop(sprintf("Per-resample DMC values differ for %s.", product_name), call. = FALSE)
  }
}

full_sample_fit <- suppressWarnings(run_structural_canvas_analysis(
  sem_snapshot, sampled_base, "sem", estimator = "ML", missing = "fiml", std_lv = FALSE,
  ordered = character(0), nominal = character(0), residual_variance_fixes = numeric(0)
))
fast_fit_list <- suppressWarnings(lavaan::lavaanList(
  model = sem_prepared$fit_template,
  data_list = list(prepared_sample), cmd = "sem", store_slots = character(0),
  fun = function(fit) {
    estimates <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = FALSE)
    list(
      keys = paste(estimates$lhs, estimates$op, estimates$rhs, sep = "\r"),
      raw = estimates$est,
      standardized = estimates$std.all,
      extracted = structural_canvas_effect_bootstrap_extract_fit(
        fit, sem_prepared$raw_keys, sem_prepared$moderated_specs, sem_prepared$model_df
      )
    )
  },
  parallel = "no", ncpus = 1L, iseed = 20260822L
))
fast_sample <- fast_fit_list@funList[[1L]]
full_estimates <- lavaan::parameterEstimates(full_sample_fit$fit, standardized = TRUE, ci = FALSE)
full_keys <- paste(full_estimates$lhs, full_estimates$op, full_estimates$rhs, sep = "\r")
if (!identical(isTRUE(fast_sample$extracted$valid), isTRUE(full_sample_fit$admissible))) {
  stop("Prepared SEM strict admissibility disagrees with the full canvas analysis.", call. = FALSE)
}
extracted_positions <- match(sem_prepared$raw_keys, fast_sample$keys)
extractable_positions <- which(!is.na(extracted_positions))
if (isTRUE(fast_sample$extracted$valid)) {
  if (!length(extractable_positions) || !isTRUE(all.equal(
    fast_sample$extracted$raw[extractable_positions],
    fast_sample$raw[extracted_positions[extractable_positions]],
    tolerance = 0, check.attributes = FALSE
  ))) {
    stop("SEM slot-based raw extraction differs from public parameterEstimates().", call. = FALSE)
  }
  standardized_extractable <- extractable_positions[
    is.finite(fast_sample$extracted$standardized[extractable_positions]) &
      is.finite(fast_sample$standardized[extracted_positions[extractable_positions]])
  ]
  if (!length(standardized_extractable) || !isTRUE(all.equal(
    fast_sample$extracted$standardized[standardized_extractable],
    fast_sample$standardized[extracted_positions[standardized_extractable]],
    tolerance = 0, check.attributes = FALSE
  ))) {
    stop("SEM slot-based standardized extraction differs from public parameterEstimates().", call. = FALSE)
  }
}
shared_keys <- intersect(fast_sample$keys, full_keys)
if (!length(shared_keys)) stop("SEM equivalence check found no shared parameters.", call. = FALSE)
fast_positions <- match(shared_keys, fast_sample$keys)
full_positions <- match(shared_keys, full_keys)
if (!isTRUE(all.equal(
  fast_sample$raw[fast_positions], full_estimates$est[full_positions],
  tolerance = 1e-8, check.attributes = FALSE
))) {
  stop("Prepared SEM raw estimates differ from a full canvas refit.", call. = FALSE)
}
usable_standardized <- is.finite(fast_sample$standardized[fast_positions]) &
  is.finite(full_estimates$std.all[full_positions])
if (!any(usable_standardized) || !isTRUE(all.equal(
  fast_sample$standardized[fast_positions][usable_standardized],
  full_estimates$std.all[full_positions][usable_standardized],
  tolerance = 1e-8, check.attributes = FALSE
))) {
  stop("Prepared SEM standardized estimates differ from a full canvas refit.", call. = FALSE)
}

# Keep one admissible fit in the core gate so slot/internal extraction is
# compared exactly even when the deterministic SEM resample is intentionally
# rejected by the strict Heywood/PSD checks above.
cfa_public <- lavaan::parameterEstimates(cfa_fit, standardized = TRUE, ci = FALSE)
cfa_keys <- paste(cfa_public$lhs, cfa_public$op, cfa_public$rhs, sep = "\r")
cfa_extracted <- structural_canvas_effect_bootstrap_extract_fit(
  cfa_fit, cfa_keys, moderated_specs = list(),
  model_df = as.numeric(lavaan::fitMeasures(cfa_fit, "df")[[1L]])
)
cfa_admissibility <- structural_canvas_fit_admissibility(cfa_fit)
if (!identical(isTRUE(cfa_extracted$valid), isTRUE(cfa_admissibility$admissible))) {
  stop("CFA slot/internal extractor changed the shared strict admissibility decision.", call. = FALSE)
}
if (!isTRUE(cfa_extracted$valid) || !isTRUE(all.equal(
  cfa_extracted$raw, cfa_public$est,
  tolerance = 0, check.attributes = FALSE
)) || !isTRUE(all.equal(
  cfa_extracted$standardized, cfa_public$std.all,
  tolerance = 0, check.attributes = FALSE
))) {
  stop("CFA slot/internal raw or standardized extraction differs from public parameterEstimates().", call. = FALSE)
}

assert_lavaan_metadata_fast_path_contract <- function(fit) {
  namespace <- asNamespace("lavaan")
  imports <- parent.env(namespace)
  original_check <- get("lav_object_check_version", namespace, inherits = FALSE)
  original_description <- get("packageDescription", imports, inherits = FALSE)
  original_check_lock <- bindingIsLocked("lav_object_check_version", namespace)
  original_description_lock <- bindingIsLocked("packageDescription", imports)
  public_before <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = FALSE)

  first <- structural_canvas_lavaan_worker_metadata_fast_path_install()
  if (!isTRUE(first$applied)) {
    if (installer_mode) {
      stop(sprintf(
        "The packaged lavaan metadata fast path was not applied: %s",
        as.character(first$reason %||% "unknown reason")
      ), call. = FALSE)
    }
    message(sprintf("Skipping bundled lavaan metadata fast-path contract: %s", first$reason))
    return(invisible(FALSE))
  }
  second <- structural_canvas_lavaan_worker_metadata_fast_path_install()
  if (!isTRUE(second$applied) || isTRUE(second$owned)) {
    first$restore()
    stop("The nested lavaan metadata fast-path lease was not acquired.", call. = FALSE)
  }
  active_check <- get("lav_object_check_version", namespace, inherits = FALSE)
  active_description <- get("packageDescription", imports, inherits = FALSE)
  first$restore()
  if (!identical(get("lav_object_check_version", namespace, inherits = FALSE), active_check) ||
      !identical(get("packageDescription", imports, inherits = FALSE), active_description)) {
    second$restore()
    stop("Owner-first restore released a live nested lavaan metadata lease.", call. = FALSE)
  }
  public_after <- lavaan::parameterEstimates(fit, standardized = TRUE, ci = FALSE)
  second$restore()
  if (!isTRUE(all.equal(public_before, public_after, tolerance = 0, check.attributes = TRUE)) ||
      !identical(get("lav_object_check_version", namespace, inherits = FALSE), original_check) ||
      !identical(get("packageDescription", imports, inherits = FALSE), original_description) ||
      !identical(bindingIsLocked("lav_object_check_version", namespace), original_check_lock) ||
      !identical(bindingIsLocked("packageDescription", imports), original_description_lock)) {
    stop("The lavaan metadata fast path changed output or failed exact binding restoration.", call. = FALSE)
  }

  original_step17 <- get("lav_step17_lavaan", namespace, inherits = FALSE)
  original_step17_lock <- bindingIsLocked("lav_step17_lavaan", namespace)
  restore_step17 <- function() {
    if (bindingIsLocked("lav_step17_lavaan", namespace)) {
      unlockBinding("lav_step17_lavaan", namespace)
    }
    assign("lav_step17_lavaan", original_step17, envir = namespace)
    if (original_step17_lock) lockBinding("lav_step17_lavaan", namespace)
    invisible(TRUE)
  }
  on.exit(restore_step17(), add = TRUE)
  if (original_step17_lock) unlockBinding("lav_step17_lavaan", namespace)
  assign("lav_step17_lavaan", function(...) NULL, envir = namespace)
  if (original_step17_lock) lockBinding("lav_step17_lavaan", namespace)
  mismatch <- structural_canvas_lavaan_worker_metadata_fast_path_install()
  restore_step17()
  if (isTRUE(mismatch$applied) || !grepl("fingerprint", mismatch$reason, fixed = TRUE) ||
      !identical(get("lav_object_check_version", namespace, inherits = FALSE), original_check) ||
      !identical(get("packageDescription", imports, inherits = FALSE), original_description)) {
    stop("A changed lavaan body fingerprint did not fail closed.", call. = FALSE)
  }
  invisible(TRUE)
}

metadata_restore_passed <- isTRUE(assert_lavaan_metadata_fast_path_contract(cfa_fit))
if (installer_mode && !metadata_restore_passed) {
  stop("The installer gate could not prove exact lavaan metadata fast-path restoration.", call. = FALSE)
}

# Defined effects are included in the aligned ParTable vectors but require an
# explicit regression fixture: ordinary CFA coefficients alone do not prove
# that `:=` rows retain their raw and standardized values.
defined_effect_fit <- suppressWarnings(lavaan::sem(
  paste(
    "y1 ~ a*x1",
    "y2 ~ b*y1 + c*x1",
    "ind := a*b",
    sep = "\n"
  ),
  data = sem_data, estimator = "ML"
))
defined_effect_public <- lavaan::parameterEstimates(
  defined_effect_fit, standardized = TRUE, ci = FALSE
)
defined_effect_keys <- paste(
  defined_effect_public$lhs, defined_effect_public$op,
  defined_effect_public$rhs, sep = "\r"
)
defined_effect_extracted <- structural_canvas_effect_bootstrap_extract_fit(
  defined_effect_fit, defined_effect_keys, moderated_specs = list(),
  model_df = as.numeric(lavaan::fitMeasures(defined_effect_fit, "df")[[1L]])
)
defined_effect_admissibility <- structural_canvas_fit_admissibility(defined_effect_fit)
defined_effect_rows <- which(defined_effect_public$op == ":=")
if (!length(defined_effect_rows) || !identical(
  isTRUE(defined_effect_extracted$valid),
  isTRUE(defined_effect_admissibility$admissible)
)) {
  stop("Defined-effect extractor changed strict admissibility or lost the := row.", call. = FALSE)
}
if (!isTRUE(defined_effect_extracted$valid) || !isTRUE(all.equal(
  defined_effect_extracted$raw, defined_effect_public$est,
  tolerance = 0, check.attributes = FALSE
)) || !isTRUE(all.equal(
  defined_effect_extracted$standardized, defined_effect_public$std.all,
  tolerance = 0, check.attributes = FALSE
)) || !all(is.finite(defined_effect_extracted$raw[defined_effect_rows])) ||
    !all(is.finite(defined_effect_extracted$standardized[defined_effect_rows]))) {
  stop("Defined-effect slot/internal extraction differs from public parameterEstimates().", call. = FALSE)
}

# The fused lavaan 0.7-2 bootstrap gate must retain the complete public strict
# validity mask, including both clean CFA draws and deliberately fragile DMC
# product-indicator draws. Keep the repetitions small so this exact reference
# comparison remains suitable for the everyday core gate.
assert_strict_mask_equivalence <- function(
  fit_template, data_list, raw_keys, model_df, label,
  require_valid = FALSE, require_invalid = FALSE
) {
  compared <- suppressWarnings(lavaan::lavaanList(
    model = fit_template, data_list = data_list, cmd = "sem",
    store_slots = character(0),
    fun = function(fit) {
      extracted_result <- structural_canvas_effect_bootstrap_extract_fit(
        fit, raw_keys, moderated_specs = list(), model_df = model_df
      )
      list(
        extracted = isTRUE(extracted_result$valid),
        reference = isTRUE(structural_canvas_fit_admissibility(fit)$admissible),
        screening_path = as.character(extracted_result$screening_path %||% "missing")
      )
    },
    parallel = "no", ncpus = 1L, iseed = 20260822L
  ))
  items <- compared@funList
  extracted <- vapply(items, function(item) is.list(item) && isTRUE(item$extracted), logical(1))
  reference <- vapply(items, function(item) is.list(item) && isTRUE(item$reference), logical(1))
  if (length(extracted) != length(data_list) || !identical(extracted, reference)) {
    stop(sprintf("%s fused/public strict admissibility masks differ.", label), call. = FALSE)
  }
  screening_paths <- vapply(
    items,
    function(item) if (is.list(item) && length(item$screening_path)) item$screening_path[[1L]] else "missing",
    character(1)
  )
  if (identical(as.character(fit_template@version[[1L]]), "0.7-2") &&
      !all(screening_paths == "fused_0_7_2")) {
    stop(sprintf(
      "%s did not execute the fused lavaan 0.7-2 screening path: %s.",
      label, paste(unique(screening_paths), collapse = ", ")
    ), call. = FALSE)
  }
  if (require_valid && !any(reference)) {
    stop(sprintf("%s strict-mask fixture produced no admissible draw.", label), call. = FALSE)
  }
  if (require_invalid && !any(!reference)) {
    stop(sprintf("%s strict-mask fixture produced no rejected draw.", label), call. = FALSE)
  }
  invisible(reference)
}

hs_strict_model <- paste(
  "visual =~ x1 + x2 + x3",
  "textual =~ x4 + x5 + x6",
  "speed =~ x7 + x8 + x9",
  sep = "\n"
)
hs_strict_fixture_path <- file.path("sample", "HolzingerSwineford1939.csv")
if (!file.exists(hs_strict_fixture_path)) {
  stop(sprintf("The strict CFA bootstrap fixture is missing: %s", hs_strict_fixture_path), call. = FALSE)
}
hs_strict_data <- utils::read.csv(
  hs_strict_fixture_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
if (nrow(hs_strict_data) != 301L || !all(paste0("x", 1:9) %in% names(hs_strict_data))) {
  stop("The strict CFA bootstrap fixture must contain 301 rows and indicators x1 through x9.", call. = FALSE)
}
hs_strict_fit <- suppressWarnings(lavaan::cfa(
  hs_strict_model, data = hs_strict_data, missing = "fiml",
  auto.cov.lv.x = FALSE
))
hs_strict_parameters <- lavaan::parameterEstimates(hs_strict_fit, ci = FALSE)
hs_strict_keys <- paste(
  hs_strict_parameters$lhs, hs_strict_parameters$op,
  hs_strict_parameters$rhs, sep = "\r"
)
set.seed(20260822L)
hs_strict_data_list <- replicate(
  12L,
  hs_strict_data[sample.int(nrow(hs_strict_data), nrow(hs_strict_data), replace = TRUE), , drop = FALSE],
  simplify = FALSE
)
assert_strict_mask_equivalence(
  hs_strict_fit, hs_strict_data_list, hs_strict_keys,
  as.numeric(lavaan::fitMeasures(hs_strict_fit, "df")[[1L]]),
  "Holzinger-Swineford CFA (12 draws)", require_valid = TRUE
)

set.seed(20260822L)
dmc_strict_indices <- replicate(
  24L, sample.int(nrow(sem_data), nrow(sem_data), replace = TRUE),
  simplify = FALSE
)
dmc_strict_data_list <- lapply(dmc_strict_indices, function(indices) {
  structural_canvas_effect_bootstrap_resample_data(
    sem_data, indices, sem_prepared$product_specs
  )
})
assert_strict_mask_equivalence(
  sem_prepared$fit_template, dmc_strict_data_list, sem_prepared$raw_keys,
  sem_prepared$model_df, "DMC SEM (24 draws)", require_invalid = TRUE
)

run_sem_core <- function(
  seed, disable_two_stage = FALSE, disable_fixed_index = FALSE,
  workers = 2L, repetitions = 24L, failure_mode = "", chunk_size = NULL
) {
  old_fast_path_option <- getOption("statedu.isolated_lavaan_bootstrap_worker")
  old_two_stage_option <- getOption("statedu.internal.disable_sem_bootstrap_two_stage")
  old_fixed_index_option <- getOption("statedu.internal.disable_sem_bootstrap_fixed_index")
  old_failure_option <- getOption("statedu.internal.sem_bootstrap_fixed_index_test_failure")
  options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
  on.exit(options(statedu.isolated_lavaan_bootstrap_worker = old_fast_path_option), add = TRUE)
  options(statedu.internal.disable_sem_bootstrap_two_stage = isTRUE(disable_two_stage))
  on.exit(options(statedu.internal.disable_sem_bootstrap_two_stage = old_two_stage_option), add = TRUE)
  options(statedu.internal.disable_sem_bootstrap_fixed_index = isTRUE(disable_fixed_index))
  on.exit(options(statedu.internal.disable_sem_bootstrap_fixed_index = old_fixed_index_option), add = TRUE)
  options(statedu.internal.sem_bootstrap_fixed_index_test_failure = failure_mode)
  on.exit(options(statedu.internal.sem_bootstrap_fixed_index_test_failure = old_failure_option), add = TRUE)
  events <- list()
  started <- Sys.time()
  if (is.null(chunk_size)) chunk_size <- min(repetitions, max(workers, 12L))
  value <- suppressWarnings(structural_canvas_effect_bootstrap_prepared(
    sem_prepared, reps = repetitions, seed = seed, workers = workers,
    chunk_size = chunk_size,
    return_draws = TRUE,
    progress = function(done, total, valid) {
      events[[length(events) + 1L]] <<- list(
        phase = "resampling", completed = done, total = total, valid = valid,
        elapsed = elapsed_seconds(started)
      )
    },
    phase = function(name, done, total, valid, workers) {
      events[[length(events) + 1L]] <<- list(
        phase = name, completed = done, total = total, valid = valid,
        workers = workers, elapsed = elapsed_seconds(started)
      )
    }
  ))
  list(value = value, events = events, seconds = elapsed_seconds(started))
}

run_cfa_core <- function(seed) {
  events <- list()
  started <- Sys.time()
  value <- suppressWarnings(structural_canvas_reliability_bootstrap(
    cfa_syntax, sem_data, reps = 24L, seed = seed, estimator = "ML",
    missing = "listwise", original_fit = cfa_fit,
    progress = function(done, total, valid) {
      events[[length(events) + 1L]] <<- list(
        phase = "reliability", completed = done, total = total, valid = valid,
        elapsed = elapsed_seconds(started)
      )
    }
  ))
  list(value = value, events = events, seconds = elapsed_seconds(started))
}

reliability_draw_mask <- function(value) {
  vapply(value$estimates, function(item) {
    is.data.frame(item) && nrow(item) > 0L
  }, logical(1))
}

reliability_draws_equal <- function(left, right, tolerance = 1e-12) {
  if (length(left$estimates) != length(right$estimates)) return(FALSE)
  all(vapply(seq_along(left$estimates), function(index) {
    lhs <- left$estimates[[index]]
    rhs <- right$estimates[[index]]
    if (is.null(lhs) || is.null(rhs)) return(identical(is.null(lhs), is.null(rhs)))
    isTRUE(all.equal(lhs, rhs, tolerance = tolerance, check.attributes = TRUE))
  }, logical(1)))
}

run_cfa_release_exactness <- function(seed = 20260823L, repetitions = 6L) {
  old_isolated_worker <- getOption("statedu.isolated_lavaan_bootstrap_worker")
  metadata_state <- NULL
  on.exit({
    if (is.list(metadata_state) && is.function(metadata_state$restore)) {
      try(metadata_state$restore(), silent = TRUE)
    }
    options(statedu.isolated_lavaan_bootstrap_worker = old_isolated_worker)
  }, add = TRUE)

  call_bootstrap <- function(workers, progress = NULL) {
    suppressWarnings(structural_canvas_reliability_bootstrap(
      cfa_syntax, sem_data, reps = repetitions, seed = seed,
      estimator = "ML", missing = "listwise", original_fit = cfa_fit,
      workers = workers, chunk_size = 4L, return_draws = TRUE,
      progress = progress
    ))
  }

  options(statedu.isolated_lavaan_bootstrap_worker = FALSE)
  legacy_started <- Sys.time()
  legacy <- call_bootstrap(1L)
  legacy_seconds <- elapsed_seconds(legacy_started)

  options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
  metadata_state <- structural_canvas_lavaan_worker_metadata_fast_path_install()
  if (installer_mode && !isTRUE(metadata_state$applied)) {
    stop(sprintf(
      "CFA exactness gate could not apply the guarded lavaan metadata fast path: %s",
      as.character(metadata_state$reason %||% "unknown reason")
    ), call. = FALSE)
  }
  serial_started <- Sys.time()
  fast_serial <- call_bootstrap(1L)
  serial_seconds <- elapsed_seconds(serial_started)

  parallel_progress <- list()
  parallel_started <- Sys.time()
  fast_parallel <- call_bootstrap(2L, progress = function(done, total, valid) {
    parallel_progress[[length(parallel_progress) + 1L]] <<- c(
      completed = done, total = total, valid = valid
    )
  })
  parallel_seconds <- elapsed_seconds(parallel_started)
  metadata_state$restore()
  metadata_state <- NULL
  options(statedu.isolated_lavaan_bootstrap_worker = old_isolated_worker)

  progress_matrix <- do.call(rbind, parallel_progress)
  if (is.null(progress_matrix) || !nrow(progress_matrix) ||
      any(diff(progress_matrix[, "completed"]) < 0L) ||
      any(diff(progress_matrix[, "valid"]) < 0L) ||
      tail(progress_matrix[, "completed"], 1L) != repetitions ||
      tail(progress_matrix[, "valid"], 1L) != sum(reliability_draw_mask(fast_parallel))) {
    stop("CFA exactness gate observed invalid reusable-PSOCK progress.", call. = FALSE)
  }

  same_indices_legacy_serial <- identical(legacy$sample_indices, fast_serial$sample_indices)
  same_indices_serial_psock <- identical(fast_serial$sample_indices, fast_parallel$sample_indices)
  same_mask_legacy_serial <- identical(reliability_draw_mask(legacy), reliability_draw_mask(fast_serial))
  same_mask_serial_psock <- identical(reliability_draw_mask(fast_serial), reliability_draw_mask(fast_parallel))
  same_draws_legacy_serial <- reliability_draws_equal(legacy, fast_serial)
  same_draws_serial_psock <- reliability_draws_equal(fast_serial, fast_parallel)
  same_summary_legacy_serial <- isTRUE(all.equal(
    legacy$summary, fast_serial$summary, tolerance = 1e-12, check.attributes = FALSE
  ))
  same_summary_serial_psock <- isTRUE(all.equal(
    fast_serial$summary, fast_parallel$summary, tolerance = 1e-12, check.attributes = FALSE
  ))
  worker_metadata_applied <- length(fast_parallel$timings$worker_metadata_fast_path) == 2L &&
    all(vapply(
      fast_parallel$timings$worker_metadata_fast_path,
      function(item) isTRUE(item$applied), logical(1)
    ))
  compared_valid_draws <- sum(reliability_draw_mask(legacy))

  if (!all(c(
    same_indices_legacy_serial, same_indices_serial_psock,
    same_mask_legacy_serial, same_mask_serial_psock,
    same_draws_legacy_serial, same_draws_serial_psock,
    same_summary_legacy_serial, same_summary_serial_psock,
    identical(fast_serial$timings$workers, 1L),
    identical(fast_parallel$timings$workers, 2L),
    worker_metadata_applied, compared_valid_draws > 0L
  ))) {
    stop(
      "CFA legacy, fused serial, and reusable-PSOCK bootstrap paths are not numerically exact.",
      call. = FALSE
    )
  }

  list(
    legacy_vs_fast_serial = TRUE,
    fast_serial_vs_psock = TRUE,
    sample_indices_exact = TRUE,
    admissibility_mask_exact = TRUE,
    draw_order_exact = TRUE,
    summary_exact = TRUE,
    worker_metadata_applied = TRUE,
    compared_valid_draws = as.integer(compared_valid_draws),
    legacy_seconds = legacy_seconds,
    fast_serial_seconds = serial_seconds,
    psock_seconds = parallel_seconds
  )
}

run_sem_fixed_index_exactness <- function(seed = 20260824L, repetitions = 8L) {
  transport_functions <- list(
    fit_block = structural_canvas_effect_bootstrap_fixed_index_worker,
    cleanup = structural_canvas_effect_bootstrap_worker_cleanup,
    install_metadata = structural_canvas_effect_bootstrap_worker_install_metadata,
    install_context = structural_canvas_effect_bootstrap_worker_install_context
  )
  worker_payload_bytes <- vapply(
    transport_functions, function(value) length(serialize(value, NULL)), integer(1)
  )
  if (any(!is.finite(worker_payload_bytes)) || any(worker_payload_bytes >= 10000L)) {
    stop(sprintf(
      paste0(
        "A SEM fixed-index worker callback serialized to %d bytes; this suggests that it ",
        "captured controller data instead of reading one-time worker context."
      ),
      max(worker_payload_bytes)
    ), call. = FALSE)
  }

  fixed_data <- hs_strict_data[, paste0("x", 1:9), drop = FALSE]
  if (anyNA(fixed_data) || !all(vapply(fixed_data, is.numeric, logical(1)))) {
    stop("The fixed-index SEM fixture must be complete and numeric.", call. = FALSE)
  }
  fixed_fit <- suppressWarnings(lavaan::cfa(
    hs_strict_model, data = fixed_data, estimator = "ML",
    missing = "listwise", meanstructure = FALSE, se = "standard",
    test = "standard", baseline = FALSE, h1 = FALSE
  ))
  if (!isTRUE(lavaan::lavInspect(fixed_fit, "converged"))) {
    stop("The fixed-index SEM fixture did not converge.", call. = FALSE)
  }
  fixed_likelihood <- tryCatch(
    tolower(as.character(fixed_fit@Options$likelihood[[1L]])),
    error = function(error) ""
  )
  if (!identical(fixed_likelihood, "normal")) {
    stop(sprintf(
      "The default complete-data ML fixture resolved to likelihood '%s', not normal.",
      fixed_likelihood
    ), call. = FALSE)
  }
  fixed_model_df <- as.numeric(lavaan::fitMeasures(fixed_fit, "df")[[1L]])
  fixed_estimates <- lavaan::parameterEstimates(
    fixed_fit, standardized = TRUE, ci = FALSE
  )
  fixed_fit@Options$baseline <- FALSE
  fixed_fit@Options$h1 <- FALSE
  fixed_fit@Options$loglik <- FALSE
  fixed_fit@Options$implied <- FALSE
  fixed_fit@Options$test <- "none"
  fixed_prepared <- list(
    data = fixed_data,
    fit_template = fixed_fit,
    model_df = fixed_model_df,
    raw_original = fixed_estimates[, c("lhs", "op", "rhs", "est"), drop = FALSE],
    raw_keys = paste(
      fixed_estimates$lhs, fixed_estimates$op, fixed_estimates$rhs, sep = "\r"
    ),
    standardized_original_values = fixed_estimates$std.all,
    moderated_specs = list(), product_specs = list(), preparation_seconds = 0
  )

  run_path <- function(workers, disable_fixed = FALSE, failure_mode = "") {
    old_options <- options(
      statedu.isolated_lavaan_bootstrap_worker = TRUE,
      statedu.internal.disable_sem_bootstrap_two_stage = TRUE,
      statedu.internal.disable_sem_bootstrap_fixed_index = isTRUE(disable_fixed),
      statedu.internal.sem_bootstrap_fixed_index_test_failure = failure_mode
    )
    on.exit(options(old_options), add = TRUE)
    started <- Sys.time()
    value <- suppressWarnings(structural_canvas_effect_bootstrap_prepared(
      fixed_prepared, reps = repetitions, seed = seed,
      workers = workers, chunk_size = repetitions
    ))
    list(
      value = value,
      timings = attr(value, "timings") %||% list(),
      seconds = elapsed_seconds(started)
    )
  }

  context_existed <- exists(
    ".statedu_sem_fixed_index_bootstrap_context",
    envir = .GlobalEnv, inherits = FALSE
  )
  if (context_existed) {
    old_context <- get(
      ".statedu_sem_fixed_index_bootstrap_context",
      envir = .GlobalEnv, inherits = FALSE
    )
  }
  on.exit({
    if (context_existed) {
      assign(
        ".statedu_sem_fixed_index_bootstrap_context", old_context,
        envir = .GlobalEnv
      )
    } else if (exists(
      ".statedu_sem_fixed_index_bootstrap_context",
      envir = .GlobalEnv, inherits = FALSE
    )) {
      rm(".statedu_sem_fixed_index_bootstrap_context", envir = .GlobalEnv)
    }
  }, add = TRUE)
  protocol_block <- list(
    positions = 1L,
    indices = matrix(seq_len(nrow(fixed_data)), ncol = 1L)
  )
  assign(
    ".statedu_sem_fixed_index_bootstrap_context",
    list(test_failure = "block"), envir = .GlobalEnv
  )
  block_protocol <- structural_canvas_effect_bootstrap_fixed_index_worker(protocol_block)
  assign(
    ".statedu_sem_fixed_index_bootstrap_context",
    list(test_failure = "item", data = fixed_data), envir = .GlobalEnv
  )
  item_protocol <- structural_canvas_effect_bootstrap_fixed_index_worker(protocol_block)
  if (!isTRUE(block_protocol$failed) || !isTRUE(item_protocol$failed)) {
    stop("SEM fixed-index block/item failure protocol did not return a fallback marker.", call. = FALSE)
  }

  legacy <- run_path(1L)
  fast_two <- run_path(2L)
  fast_four <- run_path(4L)
  item_failure <- run_path(2L, failure_mode = "item")

  reference <- strip_timing_attributes(legacy$value)
  for (candidate in list(fast_two, fast_four, item_failure)) {
    assert_reproducible(
      reference, strip_timing_attributes(candidate$value),
      "SEM fixed-index versus authoritative lavaanList bootstrap", tolerance = 0
    )
  }
  for (candidate in list(fast_two, fast_four)) {
    state <- candidate$timings$fixed_index %||% list()
    if (!isTRUE(state$supported) || !isTRUE(state$active) ||
        !identical(as.integer(state$fallbacks), 0L) ||
        length(state$worker_context %||% list()) != candidate$timings$workers ||
        !all(vapply(
          state$worker_context, function(item) isTRUE(item$installed), logical(1)
        ))) {
      stop("SEM fixed-index exactness gate did not exercise the direct worker path.", call. = FALSE)
    }
  }
  legacy_state <- legacy$timings$fixed_index %||% list()
  if (isTRUE(legacy_state$supported) || isTRUE(legacy_state$active)) {
    stop("SEM fixed-index guard activated for a disabled or single-worker run.", call. = FALSE)
  }
  for (candidate in list(item_failure)) {
    state <- candidate$timings$fixed_index %||% list()
    two_stage <- candidate$timings$two_stage %||% list()
    if (!isTRUE(state$supported) || !isTRUE(state$active) ||
        as.integer(state$fallbacks %||% 0L) < 1L ||
        !is.finite(two_stage$legacy_fit_seconds %||% NA_real_) ||
        two_stage$legacy_fit_seconds <= 0) {
      stop("An injected SEM fixed-index worker failure did not fail open to lavaanList.", call. = FALSE)
    }
  }

  list(
    legacy_vs_fast = TRUE,
    worker_count_exact = TRUE,
    normal_default_activated = TRUE,
    fail_open = TRUE,
    worker_payload_small = TRUE,
    worker_payload_bytes = as.list(worker_payload_bytes),
    repetitions = as.integer(repetitions),
    seed = as.integer(seed),
    tolerance = 0,
    seconds = list(
      legacy_one_worker = legacy$seconds,
      fast_two_workers = fast_two$seconds,
      fast_four_workers = fast_four$seconds,
      injected_item_fallback = item_failure$seconds
    )
  )
}

run_sem_product_index_exactness <- function(seed = 20260826L, repetitions = 5L) {
  # A well-conditioned latent product fixture makes every draw a full-SE
  # candidate. With repetitions=5 and chunk_size=12, the deterministic chunk
  # layout is screen(1:4), singleton full(5), then deferred full(1:4).
  # This covers deferred integration and the one-position block without making
  # the intentionally fragile PoliticalDemocracy fixture pay for repeated full
  # fits in the core release gate.
  set.seed(1031L)
  rows <- 240L
  latent_x <- stats::rnorm(rows)
  latent_w <- 0.15 * latent_x + sqrt(1 - 0.15^2) * stats::rnorm(rows)
  latent_product <- latent_x * latent_w
  latent_y <- 0.35 * latent_x + 0.25 * latent_w +
    0.50 * latent_product + stats::rnorm(rows, sd = 0.55)
  product_data <- data.frame(
    x1 = 0.95 * latent_x + stats::rnorm(rows, sd = 0.18),
    x2 = 1.05 * latent_x + stats::rnorm(rows, sd = 0.18),
    x3 = 0.90 * latent_x + stats::rnorm(rows, sd = 0.18),
    w1 = 0.95 * latent_w + stats::rnorm(rows, sd = 0.18),
    w2 = 1.05 * latent_w + stats::rnorm(rows, sd = 0.18),
    w3 = 0.90 * latent_w + stats::rnorm(rows, sd = 0.18),
    y1 = 0.95 * latent_y + stats::rnorm(rows, sd = 0.18),
    y2 = 1.05 * latent_y + stats::rnorm(rows, sd = 0.18),
    y3 = 0.90 * latent_y + stats::rnorm(rows, sd = 0.18)
  )
  product_spec <- list(
    method = "matched_pair_dmc",
    pairs = data.frame(
      name = paste0("p", 1:3),
      predictor_indicator = paste0("x", 1:3),
      moderator_indicator = paste0("w", 1:3),
      stringsAsFactors = FALSE
    )
  )
  fitted_data <- structural_canvas_effect_bootstrap_resample_data(
    product_data, seq_len(rows), list(product_spec)
  )
  product_model <- paste(
    "X =~ x1 + x2 + x3",
    "W =~ w1 + w2 + w3",
    "XW =~ p1 + p2 + p3",
    "Y =~ y1 + y2 + y3",
    "Y ~ X + W + XW",
    sep = "\n"
  )
  product_fit <- suppressWarnings(lavaan::sem(
    product_model, data = fitted_data, estimator = "ML", missing = "fiml",
    se = "standard", test = "standard", baseline = FALSE, h1 = FALSE
  ))
  product_missing <- tryCatch(
    tolower(as.character(product_fit@Options$missing[[1L]])),
    error = function(error) ""
  )
  if (!isTRUE(lavaan::lavInspect(product_fit, "converged")) ||
      !isTRUE(lavaan::lavInspect(product_fit, "post.check")) ||
      !product_missing %in% c("ml", "fiml") ||
      !isTRUE(product_fit@Options$meanstructure) || anyNA(product_data)) {
    stop(
      "The stable latent-product fixture lost its complete FIML/meanstructure contract.",
      call. = FALSE
    )
  }
  product_estimates <- lavaan::parameterEstimates(
    product_fit, standardized = TRUE, ci = FALSE
  )
  product_raw <- product_estimates[
    product_estimates$op %in% c("~", ":="),
    c("lhs", "op", "rhs", "est"), drop = FALSE
  ]
  product_keys <- paste(
    product_raw$lhs, product_raw$op, product_raw$rhs, sep = "\r"
  )
  all_keys <- paste(
    product_estimates$lhs, product_estimates$op, product_estimates$rhs,
    sep = "\r"
  )
  product_standardized <- product_estimates$std.all[match(product_keys, all_keys)]
  product_fit@Options$baseline <- FALSE
  product_fit@Options$h1 <- FALSE
  product_fit@Options$loglik <- FALSE
  product_fit@Options$implied <- FALSE
  product_fit@Options$test <- "none"
  product_prepared <- list(
    data = product_data,
    fit_template = product_fit,
    model_df = as.numeric(lavaan::fitMeasures(product_fit, "df")[[1L]]),
    raw_original = product_raw,
    raw_keys = product_keys,
    standardized_original_values = product_standardized,
    moderated_specs = list(),
    product_specs = list(product_spec),
    preparation_seconds = 0
  )

  run_path <- function(
    workers, chunk_size, disable_two_stage = FALSE,
    disable_fixed_index = FALSE, failure_mode = "", prepared = product_prepared,
    path_repetitions = repetitions
  ) {
    old_options <- options(
      statedu.isolated_lavaan_bootstrap_worker = TRUE,
      statedu.internal.disable_sem_bootstrap_two_stage = isTRUE(disable_two_stage),
      statedu.internal.disable_sem_bootstrap_fixed_index = isTRUE(disable_fixed_index),
      statedu.internal.sem_bootstrap_fixed_index_test_failure = failure_mode
    )
    on.exit(options(old_options), add = TRUE)
    started <- Sys.time()
    value <- suppressWarnings(structural_canvas_effect_bootstrap_prepared(
      prepared, reps = path_repetitions, seed = seed, workers = workers,
      chunk_size = chunk_size, return_draws = TRUE
    ))
    list(
      value = value,
      timings = attr(value, "timings") %||% list(),
      seconds = elapsed_seconds(started)
    )
  }

  legacy <- run_path(
    2L, 12L, disable_two_stage = TRUE, disable_fixed_index = TRUE
  )
  fast_two <- run_path(2L, 12L)
  fast_four <- run_path(4L, 9L)
  item_failure <- run_path(2L, 12L, failure_mode = "item")
  reference <- strip_timing_attributes(legacy$value)
  for (candidate in list(fast_two, fast_four, item_failure)) {
    assert_reproducible(
      reference, strip_timing_attributes(candidate$value),
      "SEM two-stage versus unchanged full-SE bootstrap (stable latent product)",
      tolerance = 0
    )
    draw_state <- attr(candidate$value, "bootstrap_draws")
    if (!is.list(draw_state) ||
        !identical(names(draw_state), c(
          "sample_indices", "valid_mask", "raw", "standardized"
        ))) {
      stop("Latent-product exactness evidence omitted position-aligned raw draws.", call. = FALSE)
    }
  }

  assert_clean_direct <- function(candidate, expected_workers) {
    state <- candidate$timings$fixed_index %||% list()
    if (!isTRUE(state$supported) || !isTRUE(state$active) ||
        !isTRUE(state$product_aware) ||
        !identical(as.integer(candidate$timings$workers), as.integer(expected_workers)) ||
        as.integer(state$fallbacks %||% 0L) != 0L ||
        length(state$worker_context %||% list()) != expected_workers ||
        !all(vapply(
          state$worker_context, function(item) isTRUE(item$installed), logical(1)
        ))) {
      stop(
        sprintf("The %d-worker latent-product direct path did not execute cleanly.", expected_workers),
        call. = FALSE
      )
    }
    invisible(state)
  }
  fast_two_state <- assert_clean_direct(fast_two, 2L)
  fast_four_state <- assert_clean_direct(fast_four, 4L)
  fast_two_stage <- fast_two$timings$two_stage %||% list()
  if (!isTRUE(fast_two_stage$used) ||
      as.integer(fast_two_stage$refit %||% 0L) < 1L ||
      as.integer(fast_two_stage$refit_batches %||% 0L) < 1L ||
      !identical(as.integer(fast_two$timings$chunks), 2L) ||
      !identical(as.integer(fast_two$timings$chunk_size), 12L) ||
      as.integer(fast_two_state$full$batches %||% 0L) < 2L) {
    stop(
      "The latent-product gate did not exercise deferred and one-position full-SE refits.",
      call. = FALSE
    )
  }
  if (as.integer(fast_four_state$screen$batches %||% 0L) < 1L ||
      as.integer(fast_four_state$full$batches %||% 0L) < 1L) {
    stop("The four-worker latent-product gate skipped a direct screen/full phase.", call. = FALSE)
  }
  fallback_state <- item_failure$timings$fixed_index %||% list()
  fallback_two_stage <- item_failure$timings$two_stage %||% list()
  if (!isTRUE(fallback_state$supported) || !isTRUE(fallback_state$active) ||
      !isTRUE(fallback_state$product_aware) ||
      as.integer(fallback_state$screen$fallbacks %||% 0L) < 1L ||
      as.integer(fallback_state$full$fallbacks %||% 0L) < 1L ||
      !is.finite(fallback_two_stage$legacy_fit_seconds %||% NA_real_) ||
      fallback_two_stage$legacy_fit_seconds <= 0) {
    stop("Latent-product screen/full worker failures did not fail open by whole batch.", call. = FALSE)
  }

  missing_prepared <- product_prepared
  missing_prepared$data[[1L]][[1L]] <- NA_real_
  missing <- run_path(
    2L, 2L, disable_two_stage = TRUE, prepared = missing_prepared,
    path_repetitions = 2L
  )
  missing_state <- missing$timings$fixed_index %||% list()
  if (isTRUE(missing_state$supported) || isTRUE(missing_state$active) ||
      isTRUE(missing_state$product_aware)) {
    stop("Latent-product index path activated for actual missing data.", call. = FALSE)
  }

  list(
    legacy_vs_fast = TRUE,
    worker_count_exact = TRUE,
    draw_order_exact = TRUE,
    strict_mask_exact = TRUE,
    deferred_refit_exercised = TRUE,
    singleton_full_exercised = TRUE,
    fail_open = TRUE,
    actual_missing_guard = TRUE,
    complete_fiml_meanstructure = TRUE,
    repetitions = as.integer(repetitions),
    seed = as.integer(seed),
    tolerance = 0,
    seconds = list(
      legacy = legacy$seconds,
      fast_two_workers = fast_two$seconds,
      fast_four_workers = fast_four$seconds,
      injected_item_fallback = item_failure$seconds,
      actual_missing_guard = missing$seconds
    ),
    two_stage = fast_two_stage,
    fixed_index = fast_two_state
  )
}

run_structural_exactness_gate <- function() {
  message("Running deterministic SEM/CFA release exactness checks...")
  sem_first <- run_sem_core(20260822L)
  sem_fast_path <- attr(sem_first$value, "timings")$lavaan_metadata_fast_path
  if (!isTRUE(sem_fast_path$enabled) || !isTRUE(sem_fast_path$main$applied) ||
      length(sem_fast_path$workers) != 2L ||
      !all(vapply(sem_fast_path$workers, function(item) isTRUE(item$applied), logical(1)))) {
    stop("SEM exactness gate did not apply the guarded worker-local lavaan metadata fast path.", call. = FALSE)
  }
  assert_progress(
    sem_first$events, 24L,
    c("starting_workers", "resampling", "validating", "summarizing"),
    "SEM release exactness bootstrap"
  )
  sem_two_stage <- attr(sem_first$value, "timings")$two_stage %||% list()
  if (!isTRUE(sem_two_stage$supported) || !isTRUE(sem_two_stage$used) ||
      !identical(as.integer(sem_two_stage$screened), 24L)) {
    stop("SEM exactness gate did not exercise the guarded two-stage screening path.", call. = FALSE)
  }
  sem_product_fixed_index <- attr(sem_first$value, "timings")$fixed_index %||% list()
  if (!isTRUE(sem_product_fixed_index$supported) ||
      !isTRUE(sem_product_fixed_index$active) ||
      !isTRUE(sem_product_fixed_index$product_aware) ||
      as.integer(sem_product_fixed_index$fallbacks %||% 0L) != 0L ||
      as.integer(sem_product_fixed_index$screen$batches %||% 0L) < 1L) {
    stop("SEM product-aware index-only path was not exercised cleanly.", call. = FALSE)
  }

  sem_product_index <- run_sem_product_index_exactness()
  sem_full_se <- list(seconds = sem_product_index$seconds$legacy)
  sem_fixed_index <- run_sem_fixed_index_exactness()
  cfa <- run_cfa_release_exactness()
  evidence <- list(
    passed = TRUE,
    sem_two_stage_vs_full_se = isTRUE(sem_product_index$legacy_vs_fast) &&
      isTRUE(sem_product_index$strict_mask_exact),
    sem_seed_reproducible = isTRUE(sem_product_index$draw_order_exact) &&
      isTRUE(sem_product_index$worker_count_exact),
    sem_product_index_vs_legacy = isTRUE(sem_product_index$legacy_vs_fast) &&
      isTRUE(sem_product_index$worker_count_exact) &&
      isTRUE(sem_product_index$draw_order_exact) &&
      isTRUE(sem_product_index$strict_mask_exact),
    sem_product_index_fail_open = isTRUE(sem_product_index$fail_open),
    sem_product_index_missing_guard = isTRUE(sem_product_index$actual_missing_guard),
    sem_product_index_single_position = isTRUE(sem_product_index$singleton_full_exercised) &&
      isTRUE(sem_product_index$deferred_refit_exercised),
    sem_fixed_index_vs_legacy = isTRUE(sem_fixed_index$legacy_vs_fast) &&
      isTRUE(sem_fixed_index$worker_count_exact),
    sem_fixed_index_fail_open = isTRUE(sem_fixed_index$fail_open),
    sem_fixed_index_normal_default = isTRUE(sem_fixed_index$normal_default_activated),
    sem_fixed_index_worker_payload_small = isTRUE(sem_fixed_index$worker_payload_small),
    cfa_legacy_vs_fast_serial = isTRUE(cfa$legacy_vs_fast_serial),
    cfa_fast_serial_vs_psock = isTRUE(cfa$fast_serial_vs_psock),
    metadata_restore = isTRUE(metadata_restore_passed),
    sem = list(
      repetitions = 24L, seed = 20260822L, tolerance = 0,
      two_stage = sem_two_stage, product_index = sem_product_fixed_index,
      product_index_exactness = sem_product_index,
      fixed_index = sem_fixed_index
    ),
    cfa = c(list(repetitions = 6L, seed = 20260823L, tolerance = 1e-12), cfa)
  )
  if (installer_mode && !all(vapply(
    evidence[c(
      "passed", "sem_two_stage_vs_full_se", "sem_seed_reproducible",
      "sem_product_index_vs_legacy", "sem_product_index_fail_open",
      "sem_product_index_missing_guard", "sem_product_index_single_position",
      "sem_fixed_index_vs_legacy", "sem_fixed_index_fail_open",
      "sem_fixed_index_normal_default", "sem_fixed_index_worker_payload_small",
      "cfa_legacy_vs_fast_serial", "cfa_fast_serial_vs_psock", "metadata_restore"
    )], isTRUE, logical(1)
  ))) {
    stop("Installer structural bootstrap exactness evidence is incomplete.", call. = FALSE)
  }
  list(
    evidence = evidence,
    sem_first = sem_first,
    sem_full_se = sem_full_se,
    cfa_parallel_seconds = cfa$psock_seconds
  )
}

run_pls_core <- function(seed) {
  events <- list()
  original_writer <- structural_canvas_write_bootstrap_progress
  assign(
    "structural_canvas_write_bootstrap_progress",
    function(progress_file, completed, total, phase = "bootstrap", determinate = TRUE) {
      events[[length(events) + 1L]] <<- list(
        phase = phase, completed = completed, total = total, valid = completed
      )
      original_writer(progress_file, completed, total, phase, determinate)
    },
    envir = .GlobalEnv
  )
  on.exit(assign("structural_canvas_write_bootstrap_progress", original_writer, envir = .GlobalEnv), add = TRUE)
  progress_file <- tempfile("statedu-pls-core-progress-", fileext = ".rds")
  on.exit(unlink(c(progress_file, paste0(progress_file, ".tmp")), force = TRUE), add = TRUE)
  started <- Sys.time()
  value <- suppressWarnings(structural_canvas_run_plsc_bootstrap(
    pls_original$fit, nboot = 24L, seed = seed,
    progress_file = progress_file, apply_plsc = FALSE
  ))
  list(value = value, events = events, seconds = elapsed_seconds(started))
}

metrics <- list(
  fixture = list(
    rows = nrow(sem_data), observed_variables = ncol(sem_data),
    product_indicators = length(product_names), preparation_seconds = prepare_seconds,
    fixture_seconds = fixture_seconds
  )
)

# This deterministic numerical gate deliberately runs before the mode-specific
# timing branch. Consequently both direct installer builds and the core suite
# must prove the optimized SEM/CFA paths exact before any timing evidence can
# be accepted.
core_started <- if (!installer_mode) Sys.time() else NULL
exactness_run <- run_structural_exactness_gate()
metrics$exactness <- exactness_run$evidence

if (!installer_mode) {
  message("Running fast core structural bootstrap progress and reproducibility checks...")
  sem_first <- exactness_run$sem_first
  sem_full_se <- exactness_run$sem_full_se
  sem_two_stage <- attr(sem_first$value, "timings")$two_stage %||% list()

  pls_first <- run_pls_core(20260824L)
  pls_second <- run_pls_core(20260824L)
  assert_progress(
    pls_first$events, 24L, c("resampling", "summarizing"), "PLS-SEM core bootstrap"
  )
  assert_reproducible(pls_first$value, pls_second$value, "PLS-SEM core bootstrap")

  core_seconds <- elapsed_seconds(core_started)
  assert_within_budget("Structural bootstrap core validation", core_seconds, budgets$core_total)
  metrics$core <- list(
    repetitions = 24L, total_seconds = core_seconds,
    sem_seconds = sem_first$seconds,
    cfa_seconds = exactness_run$cfa_parallel_seconds,
    pls_seconds = pls_first$seconds, sem_legacy_seconds = sem_full_se$seconds,
    sem_two_stage = sem_two_stage
  )
  cat(sprintf(
    paste0(
      "Structural bootstrap core validation passed: total %.3fs; ",
      "SEM %.3fs; CFA %.3fs; PLS-SEM %.3fs.\n"
    ),
    core_seconds, sem_first$seconds, exactness_run$cfa_parallel_seconds, pls_first$seconds
  ))
} else {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("callr is required for installer-mode background bootstrap validation.", call. = FALSE)
  }

  poll_background_job <- function(job, cleanup, expected_total, allowed_phases, label, budget) {
    events <- list()
    first_phase_seconds <- NA_real_
    first_completion_seconds <- NA_real_
    started <- Sys.time()
    on.exit({
      statedu_stop_background_process_tree(job$process)
      cleanup(job)
    }, add = TRUE)
    repeat {
      elapsed <- elapsed_seconds(started)
      if (elapsed > budget) {
        stop(sprintf("%s exceeded its %.3f-second fail-closed budget.", label, budget), call. = FALSE)
      }
      progress <- structural_canvas_read_bootstrap_progress_snapshot(job$progress_file)
      if (is.list(progress)) {
        event <- list(
          phase = as.character(progress$phase %||% ""),
          completed = as.integer(progress$completed %||% 0L),
          total = as.integer(progress$total %||% expected_total),
          valid = as.integer(progress$valid %||% 0L),
          elapsed = elapsed
        )
        signature <- paste(event$phase, event$completed, event$total, event$valid, sep = "|")
        previous_signature <- if (length(events)) events[[length(events)]]$signature else ""
        if (!identical(signature, previous_signature)) {
          event$signature <- signature
          events[[length(events) + 1L]] <- event
        }
        if (!is.finite(first_phase_seconds) && !event$phase %in% c("", "starting")) {
          first_phase_seconds <- elapsed
        }
        if (!is.finite(first_completion_seconds) && event$completed > 0L) {
          first_completion_seconds <- elapsed
        }
      }
      if (!job$process$is_alive()) break
      Sys.sleep(.1)
    }
    total_seconds <- elapsed_seconds(started)
    exit_status <- job$process$get_exit_status()
    if (!identical(exit_status, 0L)) {
      error_text <- if (file.exists(job$error_file)) {
        paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      } else "Background worker did not write an error file."
      stop(sprintf("%s failed: %s", label, error_text), call. = FALSE)
    }
    final_progress <- structural_canvas_read_bootstrap_progress_snapshot(job$progress_file)
    if (is.list(final_progress)) {
      signature <- paste(
        final_progress$phase %||% "", final_progress$completed %||% 0L,
        final_progress$total %||% expected_total, final_progress$valid %||% 0L,
        sep = "|"
      )
      previous_signature <- if (length(events)) events[[length(events)]]$signature else ""
      if (!identical(signature, previous_signature)) {
        events[[length(events) + 1L]] <- list(
          phase = as.character(final_progress$phase %||% ""),
          completed = as.integer(final_progress$completed %||% 0L),
          total = as.integer(final_progress$total %||% expected_total),
          valid = as.integer(final_progress$valid %||% 0L),
          elapsed = total_seconds, signature = signature
        )
      }
    }
    assert_progress(events, expected_total, allowed_phases, label)
    if (!file.exists(job$result_file)) {
      stop(sprintf("%s completed without a result artifact.", label), call. = FALSE)
    }
    value <- readRDS(job$result_file)
    list(
      value = value, events = events, total_seconds = total_seconds,
      first_phase_seconds = first_phase_seconds,
      first_completion_seconds = if (is.finite(first_completion_seconds)) first_completion_seconds else total_seconds
    )
  }

  message("Running installer CFA bootstrap at 1,000 repetitions...")
  cfa_job <- structural_canvas_start_cfa_bootstrap_job(list(
    fit = cfa_fit, syntax = cfa_syntax, analysis_data = sem_data,
    estimator = "ML", missing = "listwise", std_lv = FALSE,
    ml_likelihood = "normal", ordered = character(0), validity_formula = "standardized",
    reliability_bootstrap = 1000L, reliability_seed = 20260823L,
    reliability_ci_method = "bias_corrected",
    bollen_stine_bootstrap = 0L, bollen_stine_seed = 20260823L,
    htmt_bootstrap = 0L, htmt_seed = 20260823L, htmt_threshold = .85,
    htmt_ci_method = "bias_corrected"
  ))
  cfa_run <- poll_background_job(
    cfa_job, structural_canvas_cleanup_cfa_bootstrap_job, 1000L,
    c("starting", "reliability", "complete"), "CFA installer bootstrap", budgets$cfa_total
  )
  if (!is.list(cfa_run$value) || !is.data.frame(cfa_run$value$reliability_bootstrap_result) ||
      !all(cfa_run$value$reliability_bootstrap_result[["Requested replicates"]] == 1000L)) {
    stop("CFA installer bootstrap result did not retain all 1,000 requested repetitions.", call. = FALSE)
  }

  message("Running installer SEM effect bootstrap at the 5,000 default...")
  sem_job <- structural_canvas_start_effect_bootstrap_job(
    sem_snapshot, sem_data, "sem", "ML", "fiml", FALSE,
    character(0), character(0), numeric(0), reps = 5000L, seed = 20260822L,
    ci_method = "bias_corrected", ml_likelihood = "normal",
    original_result = sem_original, workers = NULL, chunk_size = NULL
  )
  if (!is.finite(sem_job$preparation_seconds) || sem_job$preparation_seconds > budgets$prepare) {
    stop(sprintf(
      "SEM installer bootstrap preparation took %.3f seconds; budget is %.3f seconds.",
      sem_job$preparation_seconds, budgets$prepare
    ), call. = FALSE)
  }
  sem_run <- poll_background_job(
    sem_job, structural_canvas_cleanup_effect_bootstrap_job, 5000L,
    c("starting", "loading_engine", "starting_workers", "resampling", "validating", "summarizing", "complete"),
    "SEM installer bootstrap", budgets$sem_total
  )
  if (!is.data.frame(sem_run$value) || !nrow(sem_run$value) ||
      !all(sem_run$value$requested == 5000L) || !any(sem_run$value$valid > 0L)) {
    stop("SEM installer bootstrap did not return a valid 5,000-repetition result.", call. = FALSE)
  }
  sem_timings <- attr(sem_run$value, "timings") %||% list()
  sem_validation_seconds <- as.numeric(sem_timings$validating %||% NA_real_)
  sem_summary_seconds <- as.numeric(sem_timings$summarizing %||% NA_real_)
  assert_within_budget("SEM installer bootstrap summarization", sem_summary_seconds, budgets$summarize)
  assert_within_budget(
    "SEM installer bootstrap first completed resample",
    sem_run$first_completion_seconds, budgets$first_completion
  )

  message("Running installer PLS-SEM bootstrap at 1,000 repetitions...")
  pls_job <- structural_canvas_start_pls_bootstrap_job(pls_original, 1000L, 20260824L)
  pls_run <- poll_background_job(
    pls_job, structural_canvas_cleanup_pls_bootstrap_job, 1000L,
    c("starting", "resampling", "summarizing", "complete"),
    "PLS-SEM installer bootstrap", budgets$pls_total
  )
  requested_pls <- suppressWarnings(as.integer(pls_run$value$requested_nboot %||% NA_integer_))
  if (!inherits(pls_run$value, "summary.boot_seminr_model") ||
      !is.finite(requested_pls) || requested_pls != 1000L ||
      suppressWarnings(as.integer(pls_run$value$nboot %||% 0L)) < 1L) {
    stop("PLS-SEM installer bootstrap did not return a valid 1,000-repetition result.", call. = FALSE)
  }
  assert_within_budget(
    "CFA installer bootstrap first completed resample",
    cfa_run$first_completion_seconds, budgets$first_completion
  )
  assert_within_budget(
    "PLS-SEM installer bootstrap first completed resample",
    pls_run$first_completion_seconds, budgets$first_completion
  )
  assert_measured("CFA installer bootstrap timings", list(
    cfa_run$first_phase_seconds, cfa_run$first_completion_seconds, cfa_run$total_seconds
  ), 3L)
  assert_measured("SEM installer bootstrap timings", list(
    sem_job$preparation_seconds, sem_run$first_phase_seconds,
    sem_run$first_completion_seconds, sem_timings$worker_startup,
    sem_timings$resampling, sem_validation_seconds,
    sem_summary_seconds, sem_run$total_seconds
  ), 8L)
  assert_measured("PLS-SEM installer bootstrap timings", list(
    pls_run$first_phase_seconds, pls_run$first_completion_seconds, pls_run$total_seconds
  ), 3L)

  metrics$installer <- list(
    cfa = list(
      repetitions = 1000L,
      first_phase_seconds = cfa_run$first_phase_seconds,
      first_completion_seconds = cfa_run$first_completion_seconds,
      phase_transition_seconds = phase_transition_seconds(cfa_run$events),
      total_seconds = cfa_run$total_seconds
    ),
    sem = list(
      repetitions = 5000L, workers = as.integer(sem_job$workers %||% 1L),
      synchronous_preparation_seconds = sem_job$preparation_seconds,
      first_phase_seconds = sem_run$first_phase_seconds,
      first_completion_seconds = sem_run$first_completion_seconds,
      phase_transition_seconds = phase_transition_seconds(sem_run$events),
      worker_startup_seconds = as.numeric(sem_timings$worker_startup %||% NA_real_),
      resampling_seconds = as.numeric(sem_timings$resampling %||% NA_real_),
      validating_seconds = sem_validation_seconds,
      summarizing_seconds = sem_summary_seconds,
      two_stage = sem_timings$two_stage %||% list(),
      total_seconds = sem_run$total_seconds,
      minimum_valid_repetitions = min(sem_run$value$valid, na.rm = TRUE)
    ),
    pls_sem = list(
      repetitions = 1000L,
      first_phase_seconds = pls_run$first_phase_seconds,
      first_completion_seconds = pls_run$first_completion_seconds,
      phase_transition_seconds = phase_transition_seconds(pls_run$events),
      total_seconds = pls_run$total_seconds,
      valid_repetitions = as.integer(pls_run$value$nboot)
    )
  )

  report <- list(
    schema_version = 2L,
    run_id = validation_run_id,
    validation = "CFA/SEM/PLS-SEM structural bootstrap installer regression",
    mode = bootstrap_validation_mode,
    passed = TRUE,
    started_at_utc = format(validation_started_at, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    completed_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    measured_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    R_version = R.version.string,
    package_versions = list(
      lavaan = as.character(utils::packageVersion("lavaan")),
      seminr = as.character(utils::packageVersion("seminr"))
    ),
    budgets_seconds = budgets,
    metrics = metrics
  )
  recorded <- write_verified_structural_report(report, report_path, validation_run_id)
  cat(sprintf(
    paste0(
      "Structural bootstrap installer validation passed. ",
      "CFA 1,000: %.3fs; SEM 5,000: %.3fs; PLS-SEM 1,000: %.3fs.\n",
      "Measured timing record: %s\n"
    ),
    cfa_run$total_seconds, sem_run$total_seconds, pls_run$total_seconds,
    normalizePath(report_path, winslash = "/", mustWork = TRUE)
  ))
}
