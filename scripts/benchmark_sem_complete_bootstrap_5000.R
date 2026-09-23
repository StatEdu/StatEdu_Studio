script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/benchmark_sem_complete_bootstrap_5000.R"
}
default_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(Sys.getenv("STATEDU_BENCHMARK_REPO", default_root), winslash = "/", mustWork = TRUE)
setwd(repo_root)
if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))

node <- function(id, role, name) {
  list(
    id = id,
    role = role,
    name = name,
    variableId = if (identical(role, "indicator")) name else NULL,
    canvasLabel = name
  )
}
edge <- function(id, from, to) list(id = id, from = from, to = to)

set.seed(20260814L)
n <- 240L
eta_a <- stats::rnorm(n)
eta_b <- .55 * eta_a + stats::rnorm(n, sd = .80)
eta_c <- .35 * eta_a + .50 * eta_b + stats::rnorm(n, sd = .75)
data <- data.frame(
  a1 = .82 * eta_a + stats::rnorm(n, sd = .42),
  a2 = .75 * eta_a + stats::rnorm(n, sd = .50),
  a3 = .70 * eta_a + stats::rnorm(n, sd = .55),
  b1 = .80 * eta_b + stats::rnorm(n, sd = .45),
  b2 = .73 * eta_b + stats::rnorm(n, sd = .52),
  b3 = .68 * eta_b + stats::rnorm(n, sd = .58),
  c1 = .84 * eta_c + stats::rnorm(n, sd = .40),
  c2 = .77 * eta_c + stats::rnorm(n, sd = .48),
  c3 = .71 * eta_c + stats::rnorm(n, sd = .54)
)
missing_rate <- suppressWarnings(as.numeric(Sys.getenv("STATEDU_BENCHMARK_MISSING_RATE", "0")))
if (!is.finite(missing_rate) || missing_rate < 0 || missing_rate >= .5) {
  stop("STATEDU_BENCHMARK_MISSING_RATE must be in [0, .5).", call. = FALSE)
}
repetitions <- suppressWarnings(as.integer(Sys.getenv("STATEDU_BENCHMARK_REPS", "5000")))
workers <- suppressWarnings(as.integer(Sys.getenv("STATEDU_BENCHMARK_WORKERS", "12")))
chunk_size <- suppressWarnings(as.integer(Sys.getenv("STATEDU_BENCHMARK_CHUNK_SIZE", "250")))
if (!is.finite(repetitions) || repetitions < 2L) repetitions <- 5000L
if (!is.finite(workers) || workers < 1L) workers <- 12L
if (!is.finite(chunk_size) || chunk_size < workers) chunk_size <- max(workers, 250L)
disable_fixed_index <- identical(
  tolower(trimws(Sys.getenv("STATEDU_BENCHMARK_DISABLE_FIXED_INDEX", "false"))),
  "true"
)
information_mode <- tolower(trimws(Sys.getenv(
  "STATEDU_BENCHMARK_INFORMATION_MODE", "expected"
)))
if (!information_mode %in% c("observed", "expected")) information_mode <- "observed"
options(
  statedu.internal.disable_sem_bootstrap_fixed_index = disable_fixed_index,
  statedu.internal.sem_bootstrap_information_mode = information_mode
)
if (missing_rate > 0) {
  set.seed(20260827L)
  missing_per_variable <- max(1L, floor(nrow(data) * missing_rate))
  for (variable in names(data)) {
    data[sample.int(nrow(data), missing_per_variable), variable] <- NA_real_
  }
}

snapshot <- list(
  nodes = c(
    list(node("eta_a", "latent", "etaA"), node("eta_b", "latent", "etaB"), node("eta_c", "latent", "etaC")),
    lapply(paste0("a", 1:3), function(name) node(name, "indicator", name)),
    lapply(paste0("b", 1:3), function(name) node(name, "indicator", name)),
    lapply(paste0("c", 1:3), function(name) node(name, "indicator", name))
  ),
  edges = c(
    lapply(paste0("a", 1:3), function(name) edge(paste0("m", name), "eta_a", name)),
    lapply(paste0("b", 1:3), function(name) edge(paste0("m", name), "eta_b", name)),
    lapply(paste0("c", 1:3), function(name) edge(paste0("m", name), "eta_c", name)),
    list(edge("ab", "eta_a", "eta_b"), edge("bc", "eta_b", "eta_c"), edge("ac", "eta_a", "eta_c"))
  )
)

original <- suppressWarnings(run_structural_canvas_analysis(
  snapshot, data, "sem", estimator = "ML", missing = "fiml", std_lv = FALSE,
  ordered = character(0), nominal = character(0), residual_variance_fixes = numeric(0)
))
if (!isTRUE(original$converged) || !isTRUE(original$admissible)) {
  stop("The SEM benchmark fixture did not converge admissibly.", call. = FALSE)
}

started <- Sys.time()
job <- structural_canvas_start_effect_bootstrap_job(
  snapshot, data, "sem", "ML", "fiml", FALSE,
  character(0), character(0), numeric(0),
  reps = repetitions,
  seed = 20260826L,
  ci_method = "bias_corrected",
  original_result = original,
  workers = workers,
  chunk_size = chunk_size
)
on.exit({
  statedu_stop_background_process_tree(job$process)
  structural_canvas_cleanup_effect_bootstrap_job(job)
}, add = TRUE)

last_signature <- ""
repeat {
  progress <- structural_canvas_read_bootstrap_progress_snapshot(job$progress_file)
  if (is.list(progress)) {
    signature <- paste(progress$phase, progress$completed, progress$total, progress$valid, sep = "|")
    if (!identical(signature, last_signature)) {
      cat(sprintf("%s %d/%d valid=%d\n", progress$phase, progress$completed, progress$total, progress$valid))
      last_signature <- signature
    }
  }
  if (!job$process$is_alive()) break
  Sys.sleep(.2)
}

if (!identical(job$process$get_exit_status(), 0L) || !file.exists(job$result_file)) {
  error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE), collapse = "\n") else ""
  stop(sprintf("SEM bootstrap failed: %s", error_text), call. = FALSE)
}
result <- readRDS(job$result_file)
timings <- attr(result, "timings") %||% list()
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
valid <- if (is.data.frame(result) && "valid" %in% names(result)) min(result$valid, na.rm = TRUE) else NA_integer_
summary <- data.frame(
  repo = repo_root,
  repetitions = repetitions,
  workers = workers,
  missing_rate = missing_rate,
  missing_cells = sum(is.na(data)),
  elapsed_seconds = elapsed,
  resampling_seconds = as.numeric(timings$resampling %||% NA_real_),
  validating_seconds = as.numeric(timings$validating %||% NA_real_),
  summarizing_seconds = as.numeric(timings$summarizing %||% NA_real_),
  fixed_index_active = isTRUE((timings$fixed_index %||% list())$active),
  fixed_index_disabled = disable_fixed_index,
  information_mode = information_mode,
  expected_information_active = isTRUE(
    ((timings$fixed_index %||% list())$information %||% list())$expected_active
  ),
  fixed_index_fallbacks = as.integer((timings$fixed_index %||% list())$fallbacks %||% NA_integer_),
  valid = as.integer(valid),
  stringsAsFactors = FALSE
)
print(summary, row.names = FALSE)

output_path <- trimws(Sys.getenv("STATEDU_BENCHMARK_OUTPUT", ""))
if (nzchar(output_path)) saveRDS(summary, output_path)
