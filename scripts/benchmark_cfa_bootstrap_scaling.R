script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/benchmark_cfa_bootstrap_scaling.R"
}
default_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(
  Sys.getenv("STATEDU_BENCHMARK_REPO", default_root),
  winslash = "/", mustWork = TRUE
)
setwd(repo_root)
if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))

reps <- suppressWarnings(as.integer(Sys.getenv("STATEDU_CFA_SCALING_REPS", "32")))
if (!is.finite(reps) || reps < 2L) stop("STATEDU_CFA_SCALING_REPS must be at least 2.")
worker_text <- strsplit(Sys.getenv("STATEDU_CFA_SCALING_WORKERS", "2,4,8,12"), ",", fixed = TRUE)[[1L]]
worker_counts <- unique(suppressWarnings(as.integer(trimws(worker_text))))
worker_counts <- worker_counts[is.finite(worker_counts) & worker_counts >= 1L]
if (!length(worker_counts)) stop("STATEDU_CFA_SCALING_WORKERS has no positive worker count.")
chunk_size <- suppressWarnings(as.integer(Sys.getenv(
  "STATEDU_CFA_SCALING_CHUNK_SIZE", as.character(reps)
)))
if (!is.finite(chunk_size) || chunk_size < 1L) {
  stop("STATEDU_CFA_SCALING_CHUNK_SIZE must be a positive integer.")
}

set.seed(20260814L)
n <- 180L
factor_score <- stats::rnorm(n)
data <- data.frame(
  x1 = .80 * factor_score + stats::rnorm(n, sd = .60),
  x2 = .70 * factor_score + stats::rnorm(n, sd = .70),
  x3 = .90 * factor_score + stats::rnorm(n, sd = .50)
)
syntax <- "eta1 =~ x1 + x2 + x3"
fit <- suppressWarnings(lavaan::cfa(
  syntax, data = data, estimator = "ML", missing = "listwise",
  auto.cov.lv.x = FALSE
))
if (!isTRUE(lavaan::lavInspect(fit, "converged"))) stop("CFA scaling fixture did not converge.")

old_isolated <- getOption("statedu.isolated_lavaan_bootstrap_worker")
old_disable_fixed_index <- getOption("statedu.internal.disable_cfa_reliability_fixed_index")
disable_fixed_index <- tolower(trimws(Sys.getenv(
  "STATEDU_CFA_SCALING_DISABLE_FIXED_INDEX", "false"
))) %in% c("1", "true", "yes", "on")
options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
options(statedu.internal.disable_cfa_reliability_fixed_index = disable_fixed_index)
metadata_state <- structural_canvas_lavaan_worker_metadata_fast_path_install()
on.exit({
  try(metadata_state$restore(), silent = TRUE)
  options(statedu.isolated_lavaan_bootstrap_worker = old_isolated)
  options(statedu.internal.disable_cfa_reliability_fixed_index = old_disable_fixed_index)
}, add = TRUE)

results <- lapply(worker_counts, function(workers) {
  gc(FALSE)
  started <- proc.time()[["elapsed"]]
  value <- suppressWarnings(structural_canvas_reliability_bootstrap(
    syntax, data, reps = reps, seed = 20260826L,
    estimator = "ML", missing = "listwise", original_fit = fit,
    workers = workers, chunk_size = chunk_size, return_draws = TRUE
  ))
  elapsed <- unname(proc.time()[["elapsed"]] - started)
  timing <- value$timings
  data.frame(
    workers = workers,
    reps = reps,
    fixed_index_requested = !disable_fixed_index,
    elapsed_seconds = elapsed,
    worker_startup_seconds = timing$worker_startup,
    first_resample_seconds = timing$first_resample,
    resampling_seconds = timing$resampling,
    chunk_size = timing$chunk_size,
    fast_chunk_fallbacks = timing$fast_chunk_fallbacks,
    fast_item_retries = timing$fast_item_retries,
    fixed_index_active = isTRUE(timing$fixed_index$active),
    fixed_index_seconds = timing$fixed_index$seconds %||% 0,
    fixed_index_fallbacks = timing$fixed_index$fallbacks %||% 0L,
    valid = sum(vapply(value$estimates, function(item) is.data.frame(item) && nrow(item), logical(1))),
    stringsAsFactors = FALSE
  )
})

result <- do.call(rbind, results)
print(result, row.names = FALSE)
output_path <- trimws(Sys.getenv("STATEDU_CFA_SCALING_OUTPUT", ""))
if (nzchar(output_path)) saveRDS(result, output_path)
