script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/benchmark_structural_aux_bootstraps.R"
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

measure <- function(analysis, repetitions, expression) {
  gc(FALSE)
  started <- proc.time()[["elapsed"]]
  value <- force(expression)
  elapsed <- unname(proc.time()[["elapsed"]] - started)
  if (is.null(value)) stop(sprintf("%s returned NULL.", analysis), call. = FALSE)
  data.frame(
    analysis = analysis,
    repetitions = as.integer(repetitions),
    elapsed_seconds = elapsed,
    repetitions_per_second = repetitions / elapsed,
    stringsAsFactors = FALSE
  )
}

set.seed(20260814L)
n <- 180L
eta1 <- stats::rnorm(n)
eta2 <- .35 * eta1 + sqrt(1 - .35^2) * stats::rnorm(n)
data <- data.frame(
  x1 = .80 * eta1 + stats::rnorm(n, sd = .60),
  x2 = .70 * eta1 + stats::rnorm(n, sd = .70),
  x3 = .90 * eta1 + stats::rnorm(n, sd = .50),
  y1 = .75 * eta2 + stats::rnorm(n, sd = .65),
  y2 = .70 * eta2 + stats::rnorm(n, sd = .70),
  y3 = .85 * eta2 + stats::rnorm(n, sd = .55)
)

cfa_syntax <- "eta1 =~ x1 + x2 + x3"
cfa_fit <- suppressWarnings(lavaan::cfa(
  cfa_syntax,
  data = data,
  estimator = "ML",
  missing = "listwise",
  auto.cov.lv.x = FALSE
))
if (!isTRUE(lavaan::lavInspect(cfa_fit, "converged"))) {
  stop("The CFA auxiliary-bootstrap fixture did not converge.", call. = FALSE)
}
cfa <- measure(
  "CFA reliability bootstrap",
  1000L,
  local({
    old_isolated <- getOption("statedu.isolated_lavaan_bootstrap_worker")
    options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
    metadata_state <- structural_canvas_lavaan_worker_metadata_fast_path_install()
    on.exit({
      try(metadata_state$restore(), silent = TRUE)
      options(statedu.isolated_lavaan_bootstrap_worker = old_isolated)
    }, add = TRUE)
    structural_canvas_reliability_bootstrap(
      cfa_syntax,
      data,
      reps = 1000L,
      seed = 20260826L,
      estimator = "ML",
      missing = "listwise",
      original_fit = cfa_fit,
      workers = 8L,
      chunk_size = 1000L
    )
  })
)

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
pls_snapshot <- list(
  nodes = list(
    node("px", "latent", "eta1"), node("py", "latent", "eta2"),
    node("px1", "indicator", "x1"), node("px2", "indicator", "x2"), node("px3", "indicator", "x3"),
    node("py1", "indicator", "y1"), node("py2", "indicator", "y2"), node("py3", "indicator", "y3")
  ),
  edges = list(
    edge("pmx1", "px", "px1"), edge("pmx2", "px", "px2"), edge("pmx3", "px", "px3"),
    edge("pmy1", "py", "py1"), edge("pmy2", "py", "py2"), edge("pmy3", "py", "py3"),
    edge("pp1", "px", "py")
  )
)
pls_original <- suppressWarnings(run_structural_canvas_analysis(
  pls_snapshot,
  data,
  "plssem",
  estimator = "PLS"
))
if (!inherits(pls_original$fit, "pls_model") || !isTRUE(pls_original$converged)) {
  stop("The PLS-SEM auxiliary-bootstrap fixture did not converge.", call. = FALSE)
}
progress_file <- tempfile("statedu-pls-audit-", fileext = ".rds")
on.exit(unlink(progress_file, force = TRUE), add = TRUE)
pls <- measure(
  "PLS-SEM bootstrap",
  5000L,
  structural_canvas_run_plsc_bootstrap(
    pls_original$fit,
    nboot = 5000L,
    seed = 20260826L,
    progress_file = progress_file,
    apply_plsc = FALSE
  )
)

result <- rbind(cfa, pls)
result$repo <- repo_root
print(result, row.names = FALSE)
output_path <- trimws(Sys.getenv("STATEDU_BENCHMARK_OUTPUT", ""))
if (nzchar(output_path)) saveRDS(result, output_path)
