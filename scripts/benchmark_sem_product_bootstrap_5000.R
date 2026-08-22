# Opt-in, long-running benchmark for the StatEdu-owned SEM bootstrap controller.
#
# This is deliberately not a validate_* release gate.  It runs three 5,000-
# replicate latent-product fixtures sequentially and compares the new direct
# product-index path with the immediately preceding two-stage lavaanList path.
# Both paths retain public, full-SE lavaan fits as the reporting authority.
#
# Run only when explicitly requested:
#   $env:STATEDU_RUN_5000_BENCHMARK = "1"
#   packaging/electron/runtime/R-4.5.3/bin/Rscript.exe `
#     scripts/benchmark_sem_product_bootstrap_5000.R
# Resume only from the latest atomic checkpoint emitted by that run:
#   $env:STATEDU_SEM_PRODUCT_5000_RESUME = "C:/.../checkpoint-XX-label.json"

options(warn = 1)

if (!identical(trimws(Sys.getenv("STATEDU_RUN_5000_BENCHMARK", "")), "1")) {
  stop(
    paste0(
      "This long-running benchmark is opt-in. Set ",
      "STATEDU_RUN_5000_BENCHMARK=1 to run it."
    ),
    call. = FALSE
  )
}

source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

for (package in c("lavaan", "jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("%s is required for the 5,000-replicate SEM benchmark.", package), call. = FALSE)
  }
}

`%or%` <- function(value, fallback) {
  if (is.null(value) || !length(value)) fallback else value
}

timestamp <- function(value = Sys.time()) {
  format(value, "%Y-%m-%dT%H:%M:%OS3%z")
}

elapsed_seconds <- function(started_at) {
  as.numeric(difftime(Sys.time(), started_at, units = "secs"))
}

node <- function(id, role, name) {
  list(
    id = id,
    role = role,
    name = name,
    variableId = if (identical(role, "indicator")) name else NULL,
    canvasLabel = name,
    measurementMode = if (identical(role, "latent")) "reflective" else NULL
  )
}

edge <- function(id, from, to) list(id = id, from = from, to = to)

political_fixture <- function() {
  data_path <- file.path("sample", "PoliticalDemocracy.csv")
  data <- utils::read.csv(
    data_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  snapshot <- list(
    moderationMethod = "all_pairs_dmc",
    nodes = list(
      node("lx", "latent", "dem60"), node("lm", "latent", "dem65"),
      node("ly", "latent", "ind60"), node("lw", "latent", "demW"),
      node("x1", "indicator", "x1"), node("x2", "indicator", "x2"),
      node("x3", "indicator", "x3"), node("y1", "indicator", "y1"),
      node("y2", "indicator", "y2"), node("y3", "indicator", "y3"),
      node("y4", "indicator", "y4"), node("y5", "indicator", "y5"),
      node("y6", "indicator", "y6"), node("y7", "indicator", "y7"),
      node("y8", "indicator", "y8")
    ),
    edges = list(
      edge("mx1", "lx", "x1"), edge("mx2", "lx", "x2"),
      edge("mx3", "lx", "x3"), edge("mm1", "lm", "y1"),
      edge("mm2", "lm", "y2"), edge("mm3", "lm", "y3"),
      edge("mw1", "lw", "y4"), edge("mw2", "lw", "y5"),
      edge("my1", "ly", "y6"), edge("my2", "ly", "y7"),
      edge("my3", "ly", "y8"), edge("p1", "lx", "lm"),
      edge("p2", "lm", "ly"), edge("p3", "lx", "ly")
    ),
    moderations = list(list(
      id = "latent_product_moderation", from = "lw", toEdge = "p1"
    ))
  )
  list(
    id = "political_all_pairs_dmc",
    label = "PoliticalDemocracy all-pairs DMC",
    data = data,
    snapshot = snapshot,
    expected_rows = 75L,
    expected_products = 6L,
    expected_method = "all_pairs_dmc",
    bootstrap_seed = 20260831L,
    source = gsub("\\\\", "/", data_path)
  )
}

stable_data <- function() {
  set.seed(1031L)
  rows <- 240L
  latent_x <- stats::rnorm(rows)
  latent_w <- 0.15 * latent_x + sqrt(1 - 0.15^2) * stats::rnorm(rows)
  latent_product <- latent_x * latent_w
  latent_y <- 0.35 * latent_x + 0.25 * latent_w +
    0.50 * latent_product + stats::rnorm(rows, sd = 0.55)
  data.frame(
    x1 = 0.95 * latent_x + stats::rnorm(rows, sd = 0.18),
    x2 = 1.05 * latent_x + stats::rnorm(rows, sd = 0.18),
    x3 = 0.90 * latent_x + stats::rnorm(rows, sd = 0.18),
    w1 = 0.95 * latent_w + stats::rnorm(rows, sd = 0.18),
    w2 = 1.05 * latent_w + stats::rnorm(rows, sd = 0.18),
    w3 = 0.90 * latent_w + stats::rnorm(rows, sd = 0.18),
    z1 = 0.95 * latent_y + stats::rnorm(rows, sd = 0.18),
    z2 = 1.05 * latent_y + stats::rnorm(rows, sd = 0.18),
    z3 = 0.90 * latent_y + stats::rnorm(rows, sd = 0.18),
    check.names = FALSE
  )
}

stable_snapshot <- function(method) {
  list(
    moderationMethod = method,
    nodes = list(
      node("latent_x", "latent", "X"),
      node("latent_w", "latent", "W"),
      node("latent_y", "latent", "Y"),
      node("x1", "indicator", "x1"), node("x2", "indicator", "x2"),
      node("x3", "indicator", "x3"), node("w1", "indicator", "w1"),
      node("w2", "indicator", "w2"), node("w3", "indicator", "w3"),
      node("z1", "indicator", "z1"), node("z2", "indicator", "z2"),
      node("z3", "indicator", "z3")
    ),
    edges = list(
      edge("mx1", "latent_x", "x1"), edge("mx2", "latent_x", "x2"),
      edge("mx3", "latent_x", "x3"), edge("mw1", "latent_w", "w1"),
      edge("mw2", "latent_w", "w2"), edge("mw3", "latent_w", "w3"),
      edge("my1", "latent_y", "z1"), edge("my2", "latent_y", "z2"),
      edge("my3", "latent_y", "z3"),
      edge("path_x_y", "latent_x", "latent_y"),
      edge("path_w_y", "latent_w", "latent_y")
    ),
    moderations = list(list(
      id = paste0("moderation_", method),
      from = "latent_w",
      toEdge = "path_x_y"
    ))
  )
}

stable_fixture <- function(method, expected_products, bootstrap_seed) {
  list(
    id = paste0("stable_", method),
    label = sprintf("Deterministic stable %s", gsub("_", "-", method)),
    data = stable_data(),
    snapshot = stable_snapshot(method),
    expected_rows = 240L,
    expected_products = as.integer(expected_products),
    expected_method = method,
    bootstrap_seed = as.integer(bootstrap_seed),
    source = "deterministic synthetic seed 1031"
  )
}

fixture_factories <- list(
  political_fixture,
  function() stable_fixture("matched_pair_dmc", 3L, 20260832L),
  function() stable_fixture("all_pairs_dmc", 9L, 20260833L)
)
expected_fixture_ids <- c(
  "political_all_pairs_dmc",
  "stable_matched_pair_dmc",
  "stable_all_pairs_dmc"
)

requested_repetitions <- 5000L
requested_workers <- 12L
expected_chunk_size <- 250L
expected_path_order <- c(
  "fast_product_index", "prior_two_stage_lavaanList"
)

resume_checkpoint <- trimws(Sys.getenv(
  "STATEDU_SEM_PRODUCT_5000_RESUME", ""
))
if (nzchar(resume_checkpoint)) {
  resume_checkpoint <- normalizePath(
    resume_checkpoint, winslash = "/", mustWork = TRUE
  )
}

requested_report_path <- trimws(Sys.getenv(
  "STATEDU_SEM_PRODUCT_5000_REPORT", ""
))
resume_report <- NULL
if (nzchar(resume_checkpoint)) {
  resume_report <- jsonlite::read_json(
    resume_checkpoint, simplifyVector = FALSE
  )
  report_path <- as.character(resume_report$output_path %or% "")
  if (!nzchar(report_path)) {
    stop("Resume checkpoint does not identify its final output path.", call. = FALSE)
  }
  report_path <- normalizePath(report_path, winslash = "/", mustWork = FALSE)
  if (nzchar(requested_report_path) && !identical(
      normalizePath(requested_report_path, winslash = "/", mustWork = FALSE),
      report_path
  )) {
    stop("Resume checkpoint and requested report paths disagree.", call. = FALSE)
  }
} else if (nzchar(requested_report_path)) {
  report_path <- requested_report_path
} else {
  report_path <- file.path(
    "tmp",
    sprintf("sem_product_bootstrap_5000_%s.json", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
}
report_path <- normalizePath(
  report_path, winslash = "/", mustWork = FALSE
)
report_directory <- dirname(report_path)
if (!dir.exists(report_directory) &&
    !dir.create(report_directory, recursive = TRUE, showWarnings = FALSE)) {
  stop(sprintf("Could not create report directory: %s", report_directory), call. = FALSE)
}
if (file.exists(report_path)) {
  stop(sprintf("Refusing to overwrite an existing benchmark report: %s", report_path), call. = FALSE)
}

checkpoint_number <- if (is.null(resume_report)) {
  0L
} else {
  as.integer(resume_report$checkpoint$sequence %or% 0L)
}
checkpoint_paths <- if (is.null(resume_report)) {
  character(0)
} else {
  as.character(unlist(resume_report$checkpoints %or% list(), use.names = FALSE))
}
atomic_write_new_json <- function(value, path) {
  if (file.exists(path)) {
    stop(sprintf("Atomic checkpoint target already exists: %s", path), call. = FALSE)
  }
  temporary <- tempfile(
    pattern = paste0(basename(path), "."), tmpdir = dirname(path)
  )
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    value, temporary, auto_unbox = TRUE, pretty = TRUE,
    null = "null", na = "null", digits = 15
  )
  if (!isTRUE(file.rename(temporary, path))) {
    stop(sprintf("Could not atomically publish benchmark checkpoint: %s", path), call. = FALSE)
  }
  invisible(path)
}

atomic_write_new_rds <- function(value, path) {
  if (file.exists(path)) {
    stop(sprintf("Atomic result artifact already exists: %s", path), call. = FALSE)
  }
  temporary <- tempfile(
    pattern = paste0(basename(path), "."), tmpdir = dirname(path)
  )
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary, compress = FALSE)
  if (!isTRUE(file.rename(temporary, path))) {
    stop(sprintf("Could not atomically publish result artifact: %s", path), call. = FALSE)
  }
  invisible(path)
}

new_artifact_path <- function(fixture_id, path_id) {
  tempfile(
    pattern = sprintf(
      "%s.%s-%s-", basename(tools::file_path_sans_ext(report_path)),
      fixture_id, path_id
    ),
    tmpdir = report_directory,
    fileext = ".rds"
  )
}

file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

write_checkpoint <- function(report, label) {
  checkpoint_number <<- checkpoint_number + 1L
  stem <- tools::file_path_sans_ext(report_path)
  safe_label <- gsub("[^A-Za-z0-9_-]+", "-", label)
  path <- sprintf("%s.checkpoint-%02d-%s.json", stem, checkpoint_number, safe_label)
  checkpoint_paths <<- c(checkpoint_paths, path)
  report$checkpoints <- checkpoint_paths
  report$checkpoint <- list(
    sequence = checkpoint_number,
    label = label,
    written_at = timestamp()
  )
  current_session_seconds <- elapsed_seconds(benchmark_started)
  report$session_wall_seconds <- c(
    previous_session_wall_seconds, current_session_seconds
  )
  report$current_session_wall_seconds <- current_session_seconds
  report$total_wall_seconds <- sum(report$session_wall_seconds)
  atomic_write_new_json(report, path)
  message(sprintf("Checkpoint %02d: %s", checkpoint_number, path))
  invisible(path)
}

compact_timings <- function(timings, wall_seconds) {
  timings <- timings %or% list()
  fixed <- timings$fixed_index %or% list()
  two_stage <- timings$two_stage %or% list()
  additive <- c(
    timings$worker_startup %or% 0,
    timings$resampling %or% 0,
    timings$validating %or% 0,
    timings$summarizing %or% 0
  )
  additive[!is.finite(additive)] <- 0
  list(
    wall = as.numeric(wall_seconds),
    preparation_reported = as.numeric(timings$preparation %or% NA_real_),
    worker_startup = as.numeric(timings$worker_startup %or% NA_real_),
    resampling = as.numeric(timings$resampling %or% NA_real_),
    validating = as.numeric(timings$validating %or% NA_real_),
    summarizing = as.numeric(timings$summarizing %or% NA_real_),
    controller_and_context_overhead = as.numeric(wall_seconds - sum(additive)),
    workers = as.integer(timings$workers %or% NA_integer_),
    chunk_size = as.integer(timings$chunk_size %or% NA_integer_),
    chunks = as.integer(timings$chunks %or% NA_integer_),
    chunk_seconds = as.numeric(timings$chunk_seconds %or% numeric(0)),
    fixed_index = list(
      supported = isTRUE(fixed$supported),
      active = isTRUE(fixed$active),
      product_aware = isTRUE(fixed$product_aware),
      worker_context_count = length(fixed$worker_context %or% list()),
      batches = as.integer(fixed$batches %or% 0L),
      seconds = as.numeric(fixed$seconds %or% 0),
      fallbacks = as.integer(fixed$fallbacks %or% 0L),
      screen = fixed$screen %or% list(),
      full = fixed$full %or% list()
    ),
    two_stage = list(
      supported = isTRUE(two_stage$supported),
      used = isTRUE(two_stage$used),
      screened = as.integer(two_stage$screened %or% 0L),
      screen_rejected = as.integer(two_stage$screen_rejected %or% 0L),
      refit = as.integer(two_stage$refit %or% 0L),
      refit_batches = as.integer(two_stage$refit_batches %or% 0L),
      candidate_ratios = as.numeric(two_stage$candidate_ratios %or% numeric(0)),
      screening_seconds = as.numeric(two_stage$screening_seconds %or% 0),
      refit_seconds = as.numeric(two_stage$refit_seconds %or% 0),
      legacy_fit_seconds = as.numeric(two_stage$legacy_fit_seconds %or% 0)
    )
  )
}

strip_runtime_attributes <- function(value) {
  attr(value, "timings") <- NULL
  attr(value, "bootstrap_draws") <- NULL
  value
}

path_result_hash <- function(value) {
  draws <- attr(value, "bootstrap_draws")
  digest::digest(
    list(
      result = strip_runtime_attributes(value),
      sample_indices = draws$sample_indices,
      valid_mask = draws$valid_mask,
      raw = draws$raw,
      standardized = draws$standardized
    ),
    algo = "sha256", serialize = TRUE
  )
}

validate_path_value <- function(value, fixture_id, path_id) {
  is_prior <- identical(path_id, "prior_two_stage_lavaanList")
  if (!is.data.frame(value)) {
    stop(sprintf("%s/%s did not return a result table.", fixture_id, path_id), call. = FALSE)
  }
  draws <- attr(value, "bootstrap_draws")
  timings <- attr(value, "timings") %or% list()
  if (!is.list(draws) ||
      !identical(names(draws), c("sample_indices", "valid_mask", "raw", "standardized")) ||
      length(draws$sample_indices) != requested_repetitions ||
      length(draws$valid_mask) != requested_repetitions ||
      nrow(draws$raw) != requested_repetitions ||
      nrow(draws$standardized) != requested_repetitions) {
    stop(sprintf("%s/%s returned malformed draw evidence.", fixture_id, path_id), call. = FALSE)
  }
  if (!identical(as.integer(timings$workers), requested_workers) ||
      !identical(as.integer(timings$chunk_size), expected_chunk_size)) {
    stop(sprintf(
      "%s/%s did not run the requested 12-worker/default-250 contract.",
      fixture_id, path_id
    ), call. = FALSE)
  }
  fixed <- timings$fixed_index %or% list()
  if (!is_prior && (!isTRUE(fixed$supported) || !isTRUE(fixed$active) ||
      !isTRUE(fixed$product_aware) || as.integer(fixed$fallbacks %or% 0L) != 0L)) {
    stop(sprintf("%s fast product-index path was not cleanly active.", fixture_id), call. = FALSE)
  }
  if (is_prior && (isTRUE(fixed$supported) || isTRUE(fixed$active))) {
    stop(sprintf("%s prior two-stage reference activated the new index path.", fixture_id), call. = FALSE)
  }
  invisible(list(draws = draws, timings = timings))
}

persist_path_result <- function(path_run, fixture_id, path_id) {
  artifact_path <- new_artifact_path(fixture_id, path_id)
  artifact_started <- Sys.time()
  atomic_write_new_rds(path_run$value, artifact_path)
  path_run$report$artifact_rds <- normalizePath(
    artifact_path, winslash = "/", mustWork = TRUE
  )
  path_run$report$artifact_hash_sha256 <- file_sha256(artifact_path)
  path_run$report$artifact_write_seconds <- elapsed_seconds(artifact_started)
  path_run
}

load_persisted_path <- function(path_report, fixture_id, path_id) {
  artifact_path <- as.character(path_report$artifact_rds %or% "")
  expected_hash <- as.character(path_report$artifact_hash_sha256 %or% "")
  if (!nzchar(artifact_path) || !file.exists(artifact_path) || !nzchar(expected_hash)) {
    stop(sprintf(
      "%s/%s checkpoint is not backed by a complete RDS artifact.",
      fixture_id, path_id
    ), call. = FALSE)
  }
  if (!identical(file_sha256(artifact_path), expected_hash)) {
    stop(sprintf("%s/%s artifact hash mismatch.", fixture_id, path_id), call. = FALSE)
  }
  value <- readRDS(artifact_path)
  validate_path_value(value, fixture_id, path_id)
  if (!identical(
      path_result_hash(value),
      as.character(path_report$result_hash_sha256 %or% "")
  )) {
    stop(sprintf("%s/%s result hash mismatch.", fixture_id, path_id), call. = FALSE)
  }
  draws <- attr(value, "bootstrap_draws")
  if (!identical(
      as.integer(sum(draws$valid_mask)),
      as.integer(path_report$valid_draws %or% NA_integer_)
  )) {
    stop(sprintf("%s/%s persisted valid-count mismatch.", fixture_id, path_id), call. = FALSE)
  }
  message(sprintf("[%s/%s] resumed from %s", fixture_id, path_id, artifact_path))
  list(value = value, report = path_report)
}

run_path <- function(prepared, fixture_id, path_id, seed) {
  is_prior <- identical(path_id, "prior_two_stage_lavaanList")
  old_options <- options(
    statedu.isolated_lavaan_bootstrap_worker = TRUE,
    statedu.internal.disable_sem_bootstrap_two_stage = FALSE,
    statedu.internal.disable_sem_bootstrap_fixed_index = is_prior,
    statedu.internal.sem_bootstrap_fixed_index_test_failure = ""
  )
  on.exit(options(old_options), add = TRUE)
  last_completed <- -1L
  progress <- function(completed, total, valid) {
    completed <- as.integer(completed)
    if (completed != last_completed) {
      last_completed <<- completed
      message(sprintf(
        "[%s/%s] %d/%d first-pass positions; valid so far %d",
        fixture_id, path_id, completed, total, valid
      ))
    }
  }
  phase <- function(name, completed, total, valid, workers) {
    message(sprintf(
      "[%s/%s] phase=%s completed=%d/%d valid=%d workers=%d",
      fixture_id, path_id, name, completed, total, valid, workers
    ))
  }
  started_at <- Sys.time()
  value <- suppressWarnings(structural_canvas_effect_bootstrap_prepared(
    prepared,
    reps = requested_repetitions,
    seed = seed,
    ci_method = "bias_corrected",
    workers = requested_workers,
    chunk_size = NULL,
    return_draws = TRUE,
    progress = progress,
    phase = phase
  ))
  wall_seconds <- elapsed_seconds(started_at)
  validated <- validate_path_value(value, fixture_id, path_id)
  draws <- validated$draws
  timings <- validated$timings
  list(
    value = value,
    report = list(
      id = path_id,
      completed_at = timestamp(),
      valid_draws = as.integer(sum(draws$valid_mask)),
      invalid_draws = as.integer(sum(!draws$valid_mask)),
      result_hash_sha256 = path_result_hash(value),
      timings = compact_timings(timings, wall_seconds)
    )
  )
}

exact_comparison <- function(fast, prior) {
  fast_draws <- attr(fast, "bootstrap_draws")
  prior_draws <- attr(prior, "bootstrap_draws")
  checks <- list(
    sample_indices = identical(fast_draws$sample_indices, prior_draws$sample_indices),
    valid_mask = identical(fast_draws$valid_mask, prior_draws$valid_mask),
    raw_draws = identical(fast_draws$raw, prior_draws$raw),
    standardized_draws = identical(
      fast_draws$standardized, prior_draws$standardized
    ),
    result_table = identical(
      strip_runtime_attributes(fast), strip_runtime_attributes(prior)
    )
  )
  checks$passed <- all(vapply(checks, isTRUE, logical(1)))
  checks$tolerance <- 0
  checks
}

code_files <- c(
  benchmark_runner = file.path("scripts", "benchmark_sem_product_bootstrap_5000.R"),
  validation_loader = file.path("scripts", "validate_cfa_common.R"),
  structural_bootstrap = file.path("R", "setup_custom_model_canvas_structural_bootstrap.R"),
  structural_engine = file.path("R", "setup_custom_model_canvas_structural_engine.R"),
  lavaan_syntax = file.path("R", "setup_custom_model_canvas_structural_lavaan_syntax.R")
)
current_code_hashes <- as.list(vapply(code_files, file_sha256, character(1)))
current_runtime <- list(
  R = R.version.string,
  platform = R.version$platform,
  executable = normalizePath(
    file.path(R.home("bin"), "Rscript.exe"), winslash = "/", mustWork = FALSE
  ),
  lavaan = as.character(utils::packageVersion("lavaan")),
  timezone = Sys.timezone()
)
current_config <- list(
  repetitions = requested_repetitions,
  workers = requested_workers,
  chunk_size = expected_chunk_size,
  path_order = expected_path_order,
  authority_note = paste0(
    "The prior reference is the immediately preceding two-stage lavaanList ",
    "path. Both paths use public full-SE lavaan fits for reported draws; this ",
    "benchmark intentionally does not repeat a third 5,000-draw all-full-SE run."
  )
)

validate_resume_manifest <- function(value) {
  if (!identical(as.integer(value$schema_version %or% NA_integer_), 1L) ||
      !identical(as.character(value$benchmark %or% ""),
                 "SEM latent-product bootstrap 5,000 new-versus-prior comparison") ||
      !identical(as.character(value$status %or% ""), "running")) {
    stop("Resume checkpoint has an incompatible schema or status.", call. = FALSE)
  }
  config <- value$config %or% list()
  if (!identical(as.integer(config$repetitions %or% NA_integer_), requested_repetitions) ||
      !identical(as.integer(config$workers %or% NA_integer_), requested_workers) ||
      !identical(as.integer(config$chunk_size %or% NA_integer_), expected_chunk_size) ||
      !identical(
        as.character(unlist(config$path_order %or% list(), use.names = FALSE)),
        expected_path_order
      )) {
    stop("Resume checkpoint run configuration does not match this benchmark.", call. = FALSE)
  }
  runtime_fields <- c("R", "platform", "executable", "lavaan", "timezone")
  for (field in runtime_fields) {
    if (!identical(
        as.character((value$runtime %or% list())[[field]] %or% ""),
        as.character(current_runtime[[field]])
    )) {
      stop(sprintf("Resume runtime mismatch: %s.", field), call. = FALSE)
    }
  }
  for (field in names(current_code_hashes)) {
    if (!identical(
        as.character((value$code_hashes %or% list())[[field]] %or% ""),
        as.character(current_code_hashes[[field]])
    )) {
      stop(sprintf("Resume implementation hash mismatch: %s.", field), call. = FALSE)
    }
  }
  recorded_fixture_ids <- names(value$fixtures %or% list())
  if (length(recorded_fixture_ids) > length(expected_fixture_ids) ||
      !identical(
        recorded_fixture_ids,
        head(expected_fixture_ids, length(recorded_fixture_ids))
      )) {
    stop("Resume checkpoint fixture sequence is not a valid prefix.", call. = FALSE)
  }
  recorded_paths <- as.character(unlist(
    value$checkpoints %or% list(), use.names = FALSE
  ))
  if (!length(recorded_paths) || !identical(
      normalizePath(tail(recorded_paths, 1L), winslash = "/", mustWork = TRUE),
      resume_checkpoint
  ) || !identical(
      as.integer(value$checkpoint$sequence %or% NA_integer_),
      as.integer(length(recorded_paths))
  )) {
    stop("Resume checkpoint chain is incomplete or not the latest checkpoint.", call. = FALSE)
  }
  invisible(TRUE)
}

if (is.null(resume_report)) {
  report <- list(
    schema_version = 1L,
    benchmark = "SEM latent-product bootstrap 5,000 new-versus-prior comparison",
    status = "running",
    started_at = timestamp(),
    output_path = report_path,
    config = current_config,
    runtime = current_runtime,
    code_hashes = current_code_hashes,
    fixtures = list(),
    checkpoints = character(0)
  )
} else {
  validate_resume_manifest(resume_report)
  report <- resume_report
  report$resumed_at <- c(
    as.character(unlist(report$resumed_at %or% list(), use.names = FALSE)),
    timestamp()
  )
  message(sprintf("Resuming benchmark from: %s", resume_checkpoint))
}

benchmark_started <- Sys.time()
previous_session_wall_seconds <- as.numeric(unlist(
  report$session_wall_seconds %or% list(), use.names = FALSE
))
for (fixture_index in seq_along(fixture_factories)) {
  fixture <- fixture_factories[[fixture_index]]()
  message(sprintf(
    "Preparing fixture %d/%d: %s",
    fixture_index, length(fixture_factories), fixture$label
  ))
  if (!identical(nrow(fixture$data), fixture$expected_rows) || anyNA(fixture$data) ||
      !all(vapply(fixture$data, is.numeric, logical(1)))) {
    stop(sprintf("%s lost its complete numeric data contract.", fixture$id), call. = FALSE)
  }
  original_started <- Sys.time()
  original <- suppressWarnings(run_structural_canvas_analysis(
    fixture$snapshot, fixture$data, "sem",
    estimator = "ML", missing = "fiml", std_lv = FALSE,
    ordered = character(0), nominal = character(0),
    residual_variance_fixes = numeric(0)
  ))
  original_seconds <- elapsed_seconds(original_started)
  if (!inherits(original$fit, "lavaan") || !isTRUE(original$converged) ||
      length(original$moderation_definitions %or% list()) != 1L) {
    stop(sprintf("%s original latent-moderation model did not converge.", fixture$id), call. = FALSE)
  }
  definition <- original$moderation_definitions[[1L]]
  if (!identical(definition$product_indicator_method, fixture$expected_method) ||
      !identical(as.integer(definition$product_indicator_count), fixture$expected_products)) {
    stop(sprintf("%s lost its product-indicator contract.", fixture$id), call. = FALSE)
  }
  prepare_started <- Sys.time()
  prepared <- suppressWarnings(structural_canvas_prepare_effect_bootstrap(
    fixture$snapshot, fixture$data, "sem", "ML", "fiml", FALSE,
    character(0), character(0), numeric(0),
    original_result = original
  ))
  prepare_seconds <- elapsed_seconds(prepare_started)
  template_missing <- tryCatch(
    tolower(as.character(prepared$fit_template@Options$missing[[1L]])),
    error = function(error) ""
  )
  if (length(prepared$product_specs %or% list()) != 1L ||
      nrow(prepared$product_specs[[1L]]$pairs) != fixture$expected_products ||
      !template_missing %in% c("ml", "fiml") ||
      !isTRUE(prepared$fit_template@Options$meanstructure) || anyNA(prepared$data)) {
    stop(sprintf("%s lost its complete-FIML direct-path contract.", fixture$id), call. = FALSE)
  }
  fixture_report <- list(
    id = fixture$id,
    label = fixture$label,
    source = fixture$source,
    rows = nrow(fixture$data),
    columns = ncol(fixture$data),
    moderation_method = fixture$expected_method,
    product_indicators = fixture$expected_products,
    bootstrap_seed = fixture$bootstrap_seed,
    data_hash_sha256 = digest::digest(
      fixture$data, algo = "sha256", serialize = TRUE
    ),
    model_hash_sha256 = digest::digest(
      fixture$snapshot, algo = "sha256", serialize = TRUE
    ),
    original_fit_seconds = original_seconds,
    preparation_wall_seconds = prepare_seconds,
    paths = list(),
    exactness = NULL,
    status = "prepared"
  )
  existing_fixture <- report$fixtures[[fixture$id]]
  if (!is.null(existing_fixture)) {
    immutable_fields <- c(
      "id", "label", "source", "rows", "columns", "moderation_method",
      "product_indicators", "bootstrap_seed", "data_hash_sha256",
      "model_hash_sha256"
    )
    for (field in immutable_fields) {
      expected <- fixture_report[[field]]
      actual <- existing_fixture[[field]]
      if (is.numeric(expected)) {
        matches <- identical(as.numeric(actual), as.numeric(expected))
      } else {
        matches <- identical(as.character(actual), as.character(expected))
      }
      if (!isTRUE(matches)) {
        stop(sprintf(
          "%s resume fixture mismatch: %s.", fixture$id, field
        ), call. = FALSE)
      }
    }
    existing_path_names <- names(existing_fixture$paths %or% list())
    existing_status <- as.character(existing_fixture$status %or% "")
    if (!all(existing_path_names %in% expected_path_order) ||
        ("prior_two_stage_lavaanList" %in% existing_path_names &&
         !"fast_product_index" %in% existing_path_names) ||
        (existing_status == "fast_complete" &&
         !identical(existing_path_names, "fast_product_index")) ||
        (existing_status %in% c("prior_complete", "complete", "exactness_failed") &&
         !identical(existing_path_names, expected_path_order))) {
      stop(sprintf(
        "%s resume status/path checkpoint is inconsistent.", fixture$id
      ), call. = FALSE)
    }
    fixture_report <- existing_fixture
  }
  report$fixtures[[fixture$id]] <- fixture_report
  resumed_fixture_complete <- identical(
    as.character(fixture_report$status %or% ""), "complete"
  )

  fast_report <- (fixture_report$paths %or% list())$fast_product_index
  if (is.null(fast_report)) {
    fast <- persist_path_result(
      run_path(prepared, fixture$id, "fast_product_index", fixture$bootstrap_seed),
      fixture$id, "fast_product_index"
    )
    report$fixtures[[fixture$id]]$paths$fast_product_index <- fast$report
    report$fixtures[[fixture$id]]$status <- "fast_complete"
    write_checkpoint(report, paste0(fixture$id, "-fast"))
  } else {
    fast <- load_persisted_path(
      fast_report, fixture$id, "fast_product_index"
    )
  }

  prior_report <- (report$fixtures[[fixture$id]]$paths %or% list())$prior_two_stage_lavaanList
  if (is.null(prior_report)) {
    prior <- persist_path_result(
      run_path(
        prepared, fixture$id, "prior_two_stage_lavaanList",
        fixture$bootstrap_seed
      ),
      fixture$id, "prior_two_stage_lavaanList"
    )
    report$fixtures[[fixture$id]]$paths$prior_two_stage_lavaanList <- prior$report
    report$fixtures[[fixture$id]]$status <- "prior_complete"
    write_checkpoint(report, paste0(fixture$id, "-prior"))
  } else {
    prior <- load_persisted_path(
      prior_report, fixture$id, "prior_two_stage_lavaanList"
    )
  }

  exactness <- exact_comparison(fast$value, prior$value)
  if (!isTRUE(exactness$passed)) {
    report$fixtures[[fixture$id]]$exactness <- exactness
    report$fixtures[[fixture$id]]$status <- "exactness_failed"
    write_checkpoint(report, paste0(fixture$id, "-exactness-failed"))
    stop(sprintf(
      "%s fast and prior paths are not exactly equal: %s",
      fixture$id,
      paste(
        names(exactness)[names(exactness) %in% c(
          "sample_indices", "valid_mask", "raw_draws",
          "standardized_draws", "result_table"
        ) & !vapply(exactness, isTRUE, logical(1))],
        collapse = ", "
      )
    ), call. = FALSE)
  }
  fast_seconds <- fast$report$timings$wall
  prior_seconds <- prior$report$timings$wall
  report$fixtures[[fixture$id]]$exactness <- exactness
  report$fixtures[[fixture$id]]$comparison <- list(
    fast_seconds = fast_seconds,
    prior_seconds = prior_seconds,
    speedup = prior_seconds / fast_seconds,
    seconds_saved = prior_seconds - fast_seconds,
    reduction_percent = 100 * (prior_seconds - fast_seconds) / prior_seconds
  )
  report$fixtures[[fixture$id]]$status <- "complete"
  if (!isTRUE(resumed_fixture_complete)) {
    write_checkpoint(report, paste0(fixture$id, "-complete"))
  }
  rm(fast, prior, prepared, original, fixture)
  invisible(gc())
}

report$status <- "passed"
report$completed_at <- timestamp()
report$current_session_wall_seconds <- elapsed_seconds(benchmark_started)
report$session_wall_seconds <- c(
  previous_session_wall_seconds, report$current_session_wall_seconds
)
report$total_wall_seconds <- sum(report$session_wall_seconds)
report$all_exact_tolerance_zero <- all(vapply(
  report$fixtures,
  function(item) isTRUE(item$exactness$passed) && identical(item$exactness$tolerance, 0),
  logical(1)
))
report$checkpoints <- checkpoint_paths
if (!isTRUE(report$all_exact_tolerance_zero)) {
  stop("One or more 5,000-replicate fixture comparisons did not pass exactly.", call. = FALSE)
}
atomic_write_new_json(report, report_path)
cat(sprintf(
  "All three 5,000-replicate SEM product benchmarks passed exactly.\nReport: %s\n",
  report_path
))
