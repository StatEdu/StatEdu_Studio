# Focused, bounded validation for the latent DMC public-lavaan index path.
#
# This script deliberately accepts only 24, 100, or 500 repetitions. It never
# exercises the installer/default 5,000-repetition branch. The 24-draw run uses
# unchanged full-SE lavaanList as its authority; 100/500 compare performance
# with the prior two-stage lavaanList execution path while retaining exact
# position-aligned draw checks.

options(warn = 1)
source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

for (package in c("lavaan", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("%s is required for the focused SEM product-index gate.", package), call. = FALSE)
  }
}

read_integer <- function(name, default) {
  raw <- trimws(Sys.getenv(name, ""))
  if (!nzchar(raw)) return(as.integer(default))
  value <- suppressWarnings(as.integer(raw))
  if (length(value) != 1L || !is.finite(value)) {
    stop(sprintf("%s must be one finite integer.", name), call. = FALSE)
  }
  value
}

repetitions <- read_integer("STATEDU_SEM_PRODUCT_INDEX_REPS", 24L)
if (!repetitions %in% c(24L, 100L, 500L)) {
  stop(
    "STATEDU_SEM_PRODUCT_INDEX_REPS must be exactly 24, 100, or 500; 5,000 is intentionally disallowed.",
    call. = FALSE
  )
}
workers <- read_integer(
  "STATEDU_SEM_PRODUCT_INDEX_WORKERS",
  if (repetitions == 24L) 4L else 12L
)
if (workers < 2L || workers > 12L) {
  stop("STATEDU_SEM_PRODUCT_INDEX_WORKERS must be between 2 and 12.", call. = FALSE)
}
chunk_size <- read_integer(
  "STATEDU_SEM_PRODUCT_INDEX_CHUNK_SIZE",
  max(workers * 4L, min(250L, ceiling(repetitions / 20L)))
)
chunk_size <- max(workers, chunk_size)
seed <- 20260822L
report_path <- trimws(Sys.getenv(
  "STATEDU_SEM_PRODUCT_INDEX_REPORT",
  file.path("tmp", sprintf("sem_product_index_%d.json", repetitions))
))

node <- function(id, role, name) {
  list(
    id = id, role = role, name = name,
    variableId = if (identical(role, "indicator")) name else NULL,
    canvasLabel = name,
    measurementMode = if (identical(role, "latent")) "reflective" else NULL
  )
}
edge <- function(id, from, to) list(id = id, from = from, to = to)

fixture_path <- file.path("sample", "PoliticalDemocracy.csv")
fixture_data <- utils::read.csv(
  fixture_path, check.names = FALSE, stringsAsFactors = FALSE
)
if (!identical(dim(fixture_data), c(75L, 11L)) || anyNA(fixture_data)) {
  stop("PoliticalDemocracy must be a complete 75-by-11 fixture.", call. = FALSE)
}
fixture_snapshot <- list(
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
    edge("mx1", "lx", "x1"), edge("mx2", "lx", "x2"), edge("mx3", "lx", "x3"),
    edge("mm1", "lm", "y1"), edge("mm2", "lm", "y2"), edge("mm3", "lm", "y3"),
    edge("mw1", "lw", "y4"), edge("mw2", "lw", "y5"),
    edge("my1", "ly", "y6"), edge("my2", "ly", "y7"), edge("my3", "ly", "y8"),
    edge("p1", "lx", "lm"), edge("p2", "lm", "ly"), edge("p3", "lx", "ly")
  ),
  moderations = list(list(id = "latent_product_moderation", from = "lw", toEdge = "p1"))
)

original <- suppressWarnings(run_structural_canvas_analysis(
  fixture_snapshot, fixture_data, "sem", estimator = "ML", missing = "fiml",
  std_lv = FALSE, ordered = character(0), nominal = character(0),
  residual_variance_fixes = numeric(0)
))
if (!inherits(original$fit, "lavaan") || !isTRUE(original$converged)) {
  stop("The PoliticalDemocracy latent DMC fixture did not converge.", call. = FALSE)
}
prepared <- suppressWarnings(structural_canvas_prepare_effect_bootstrap(
  fixture_snapshot, fixture_data, "sem", "ML", "fiml", FALSE,
  character(0), character(0), numeric(0), original_result = original
))
template_missing <- tryCatch(
  tolower(as.character(prepared$fit_template@Options$missing[[1L]])),
  error = function(error) ""
)
if (length(prepared$product_specs) != 1L ||
    nrow(prepared$product_specs[[1L]]$pairs) != 6L ||
    !template_missing %in% c("ml", "fiml") ||
    !isTRUE(prepared$fit_template@Options$meanstructure) || anyNA(prepared$data)) {
  stop(
    "The focused fixture lost its complete FIML/meanstructure six-product contract.",
    call. = FALSE
  )
}

run_path <- function(kind) {
  authoritative <- identical(kind, "legacy") && repetitions == 24L
  old_options <- options(
    statedu.isolated_lavaan_bootstrap_worker = TRUE,
    statedu.internal.disable_sem_bootstrap_two_stage = authoritative,
    statedu.internal.disable_sem_bootstrap_fixed_index = identical(kind, "legacy"),
    statedu.internal.sem_bootstrap_fixed_index_test_failure = ""
  )
  on.exit(options(old_options), add = TRUE)
  started <- Sys.time()
  value <- suppressWarnings(structural_canvas_effect_bootstrap_prepared(
    prepared, reps = repetitions, seed = seed, workers = workers,
    chunk_size = chunk_size, return_draws = TRUE
  ))
  list(
    kind = kind,
    authority = if (authoritative) "unchanged_full_se_lavaanList" else if (
      identical(kind, "legacy")
    ) "prior_two_stage_lavaanList" else "product_index_public_lavaan",
    value = value,
    seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    timings = attr(value, "timings") %||% list()
  )
}

message(sprintf(
  "Running product-index path: %d repetitions, %d workers, chunk %d...",
  repetitions, workers, chunk_size
))
fast <- run_path("fast")
message(sprintf(
  "Running one legacy reference: %s...",
  if (repetitions == 24L) "unchanged full-SE lavaanList" else "prior two-stage lavaanList"
))
legacy <- run_path("legacy")

strip_timing <- function(value) {
  attr(value, "timings") <- NULL
  value
}
comparison <- all.equal(
  strip_timing(fast$value), strip_timing(legacy$value),
  tolerance = 0, check.attributes = TRUE
)
if (!isTRUE(comparison)) {
  stop(sprintf(
    "Product-index path differs from its same-index legacy reference: %s",
    paste(comparison, collapse = "; ")
  ), call. = FALSE)
}
fast_draws <- attr(fast$value, "bootstrap_draws")
legacy_draws <- attr(legacy$value, "bootstrap_draws")
exactness <- list(
  passed = TRUE,
  tolerance = 0,
  sample_indices = identical(fast_draws$sample_indices, legacy_draws$sample_indices),
  valid_mask = identical(fast_draws$valid_mask, legacy_draws$valid_mask),
  raw_draws = identical(fast_draws$raw, legacy_draws$raw),
  standardized_draws = identical(fast_draws$standardized, legacy_draws$standardized),
  result_table = TRUE
)
if (!all(vapply(exactness[c(
  "passed", "sample_indices", "valid_mask", "raw_draws",
  "standardized_draws", "result_table"
)], isTRUE, logical(1)))) {
  stop("Position-aligned product-index draw evidence is not exactly equal.", call. = FALSE)
}

fast_fixed <- fast$timings$fixed_index %||% list()
legacy_fixed <- legacy$timings$fixed_index %||% list()
if (!isTRUE(fast_fixed$supported) || !isTRUE(fast_fixed$active) ||
    !isTRUE(fast_fixed$product_aware) ||
    as.integer(fast_fixed$fallbacks %||% 0L) != 0L ||
    length(fast_fixed$worker_context %||% list()) != workers) {
  stop("The focused product-index run did not stay on its direct path.", call. = FALSE)
}
if (isTRUE(legacy_fixed$supported) || isTRUE(legacy_fixed$active)) {
  stop("The focused legacy reference unexpectedly activated the index path.", call. = FALSE)
}

valid_draws <- sum(fast_draws$valid_mask)
report <- list(
  schema_version = 1L,
  passed = TRUE,
  fixture = "PoliticalDemocracy latent all-pairs DMC",
  repetitions = repetitions,
  seed = seed,
  workers = workers,
  chunk_size = chunk_size,
  complete_fiml_meanstructure = TRUE,
  exactness = exactness,
  performance = list(
    fast_path = fast$authority,
    legacy_reference = legacy$authority,
    fast_seconds = fast$seconds,
    legacy_seconds = legacy$seconds,
    speedup = legacy$seconds / fast$seconds,
    seconds_saved = legacy$seconds - fast$seconds
  ),
  execution = list(
    valid_draws = as.integer(valid_draws),
    fast_two_stage = fast$timings$two_stage %||% list(),
    fast_fixed_index = list(
      supported = isTRUE(fast_fixed$supported),
      active = isTRUE(fast_fixed$active),
      product_aware = isTRUE(fast_fixed$product_aware),
      batches = as.integer(fast_fixed$batches %||% 0L),
      fallbacks = as.integer(fast_fixed$fallbacks %||% 0L),
      screen = fast_fixed$screen %||% list(),
      full = fast_fixed$full %||% list()
    ),
    legacy_two_stage = legacy$timings$two_stage %||% list()
  )
)
report_directory <- dirname(report_path)
if (!dir.exists(report_directory) &&
    !dir.create(report_directory, recursive = TRUE, showWarnings = FALSE)) {
  stop(sprintf("Could not create report directory: %s", report_directory), call. = FALSE)
}
jsonlite::write_json(
  report, report_path, auto_unbox = TRUE, pretty = TRUE,
  null = "null", digits = 10
)
cat(sprintf(
  paste0(
    "SEM product-index focused validation passed (%d draws, %d valid). ",
    "Fast %.3fs; legacy %.3fs; speedup %.2fx.\nReport: %s\n"
  ),
  repetitions, valid_draws, fast$seconds, legacy$seconds,
  legacy$seconds / fast$seconds,
  normalizePath(report_path, winslash = "/", mustWork = TRUE)
))
