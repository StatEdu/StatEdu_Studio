script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/validate_sem_missing_fiml_fixed_index.R"
}
repo_root <- normalizePath(
  file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE
)
setwd(repo_root)
if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(
    Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE
  )))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))

node <- function(id, role, name) {
  list(
    id = id, role = role, name = name,
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
set.seed(20260827L)
for (variable in names(data)) {
  data[sample.int(nrow(data), floor(nrow(data) * .1)), variable] <- NA_real_
}

snapshot <- list(
  nodes = c(
    list(
      node("eta_a", "latent", "etaA"),
      node("eta_b", "latent", "etaB"),
      node("eta_c", "latent", "etaC")
    ),
    lapply(paste0("a", 1:3), function(name) node(name, "indicator", name)),
    lapply(paste0("b", 1:3), function(name) node(name, "indicator", name)),
    lapply(paste0("c", 1:3), function(name) node(name, "indicator", name))
  ),
  edges = c(
    lapply(paste0("a", 1:3), function(name) edge(paste0("m", name), "eta_a", name)),
    lapply(paste0("b", 1:3), function(name) edge(paste0("m", name), "eta_b", name)),
    lapply(paste0("c", 1:3), function(name) edge(paste0("m", name), "eta_c", name)),
    list(
      edge("ab", "eta_a", "eta_b"),
      edge("bc", "eta_b", "eta_c"),
      edge("ac", "eta_a", "eta_c")
    )
  )
)

original <- suppressWarnings(run_structural_canvas_analysis(
  snapshot, data, "sem", estimator = "ML", missing = "fiml", std_lv = FALSE,
  ordered = character(0), nominal = character(0),
  residual_variance_fixes = numeric(0)
))
if (!isTRUE(original$converged) || !isTRUE(original$admissible)) {
  stop("The actual-missing FIML SEM fixture did not converge admissibly.", call. = FALSE)
}
prepared <- structural_canvas_prepare_effect_bootstrap(
  snapshot, data, "sem", "ML", "fiml", FALSE,
  character(0), character(0), numeric(0), original_result = original
)

repetitions <- suppressWarnings(as.integer(Sys.getenv("STATEDU_TEST_REPS", "100")))
workers <- suppressWarnings(as.integer(Sys.getenv("STATEDU_TEST_WORKERS", "2")))
information_mode <- tolower(trimws(Sys.getenv(
  "STATEDU_TEST_INFORMATION_MODE", "expected"
)))
if (!is.finite(repetitions) || repetitions < 2L) repetitions <- 100L
if (!is.finite(workers) || workers < 2L) workers <- 2L
if (!information_mode %in% c("observed", "expected")) information_mode <- "observed"

run_path <- function(disable_fixed_index) {
  old_options <- options(
    statedu.isolated_lavaan_bootstrap_worker = TRUE,
    statedu.internal.disable_sem_bootstrap_two_stage = TRUE,
    statedu.internal.disable_sem_bootstrap_fixed_index = isTRUE(disable_fixed_index),
    statedu.internal.sem_bootstrap_information_mode = information_mode
  )
  on.exit(options(old_options), add = TRUE)
  started <- Sys.time()
  value <- suppressWarnings(structural_canvas_effect_bootstrap_prepared(
    prepared, reps = repetitions, seed = 20260826L,
    workers = workers, chunk_size = repetitions, return_draws = TRUE
  ))
  list(
    value = value,
    seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    timings = attr(value, "timings") %||% list()
  )
}

legacy <- run_path(TRUE)
fast <- run_path(FALSE)
legacy_value <- legacy$value
fast_value <- fast$value
attr(legacy_value, "timings") <- NULL
attr(fast_value, "timings") <- NULL
comparison <- all.equal(
  legacy_value, fast_value, tolerance = 0, check.attributes = TRUE
)
if (!isTRUE(comparison)) {
  stop(sprintf(
    "Actual-missing FIML fixed-index output differs from lavaanList: %s",
    paste(comparison, collapse = "; ")
  ), call. = FALSE)
}
state <- fast$timings$fixed_index %||% list()
if (!isTRUE(state$supported) || !isTRUE(state$active) ||
    isTRUE(state$product_aware) || as.integer(state$fallbacks %||% 0L) != 0L) {
  stop(sprintf(
    paste0(
      "Actual-missing FIML fixed-index path did not execute cleanly: ",
      "supported=%s, active=%s, product_aware=%s, fallbacks=%s, reason=%s"
    ),
    isTRUE(state$supported), isTRUE(state$active), isTRUE(state$product_aware),
    as.integer(state$fallbacks %||% 0L), as.character(state$reason %||% "")
  ), call. = FALSE)
}
information_state <- state$information %||% list()
if (identical(information_mode, "expected") &&
    !isTRUE(information_state$expected_active)) {
  stop("Actual-missing FIML expected-information path was not active.", call. = FALSE)
}
draws <- attr(fast$value, "bootstrap_draws")
if (!is.list(draws) || !identical(
  names(draws), c("sample_indices", "valid_mask", "raw", "standardized")
)) {
  stop("Actual-missing FIML exactness evidence omitted aligned bootstrap draws.", call. = FALSE)
}

cat(sprintf(
  paste0(
    "Actual-missing FIML SEM exactness passed: %d repetitions, %d workers; ",
    "lavaanList %.3fs, fixed-index %.3fs, speedup %.2fx, valid %d/%d, information=%s.\n"
  ),
  repetitions, workers, legacy$seconds, fast$seconds,
  legacy$seconds / fast$seconds, sum(draws$valid_mask), repetitions,
  information_mode
))
