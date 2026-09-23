#!/usr/bin/env Rscript

# Run the focused PLS/PLSc release regressions with the packaged R runtime only.
#
# Usage from the repository or staged Electron app root:
#   Rscript --vanilla scripts/run_bundled_pls_focused_regressions.R \
#     --repo-root=<app-root> --runtime-root=<app-root>/../runtime/R-4.5.3
#
# The driver deliberately starts a fresh R process for every validator. Both
# this process and every child must resolve all R libraries from runtime-root;
# a developer or user library in .libPaths() is a fatal isolation violation.

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  matches <- args[startsWith(args, prefix)]
  if (length(matches) == 0L) {
    return(default)
  }
  if (length(matches) != 1L) {
    stop("Argument --", name, " must be supplied at most once.", call. = FALSE)
  }
  sub(prefix, "", matches[[1L]], fixed = TRUE)
}

has_flag <- function(name) {
  count <- sum(args == paste0("--", name))
  if (count > 1L) {
    stop("Flag --", name, " must be supplied at most once.", call. = FALSE)
  }
  identical(count, 1L)
}

if (has_flag("help")) {
  cat(
    "Bundled-runtime-only PLS/PLSc focused regression driver\n",
    "\n",
    "Options:\n",
    "  --repo-root=PATH       Repository or staged app root (default: working directory)\n",
    "  --runtime-root=PATH    Bundled R root (default: the current R.home())\n",
    "  --timeout-seconds=N    Per-validator timeout (default: 900)\n",
    sep = ""
  )
  quit(save = "no", status = 0L)
}

known_arguments <- c("--help")
unknown_arguments <- args[
  !args %in% known_arguments &
    !startsWith(args, "--repo-root=") &
    !startsWith(args, "--runtime-root=") &
    !startsWith(args, "--timeout-seconds=")
]
if (length(unknown_arguments) > 0L) {
  stop("Unknown argument(s): ", paste(unknown_arguments, collapse = ", "), call. = FALSE)
}

canonical_path <- function(path, must_work = TRUE) {
  normalized <- normalizePath(path, winslash = "/", mustWork = must_work)
  normalized <- sub("/+$", "", normalized)
  if (.Platform$OS.type == "windows") {
    normalized <- tolower(normalized)
  }
  normalized
}

path_is_within <- function(path, root) {
  path <- canonical_path(path, must_work = TRUE)
  root <- canonical_path(root, must_work = TRUE)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

repo_root <- canonical_path(arg_value("repo-root", getwd()), must_work = TRUE)
runtime_root_value <- arg_value("runtime-root", R.home())
runtime_root <- canonical_path(runtime_root_value, must_work = TRUE)
current_r_home <- canonical_path(R.home(), must_work = TRUE)

if (!identical(current_r_home, runtime_root)) {
  stop(
    "Bundled-runtime isolation failed: current R.home() is ", current_r_home,
    ", but --runtime-root resolves to ", runtime_root, ".",
    call. = FALSE
  )
}
if (!identical(as.character(getRversion()), "4.5.3")) {
  stop(
    "Bundled-runtime isolation failed: expected R 4.5.3, found R ",
    as.character(getRversion()), ".",
    call. = FALSE
  )
}

runtime_library <- canonical_path(file.path(runtime_root, "library"), must_work = TRUE)
library_paths <- vapply(.libPaths(), canonical_path, character(1L), must_work = TRUE)
external_library_paths <- library_paths[
  !vapply(library_paths, path_is_within, logical(1L), root = runtime_root)
]
if (length(external_library_paths) > 0L) {
  stop(
    "Bundled-runtime isolation failed: .libPaths() contains non-runtime path(s): ",
    paste(external_library_paths, collapse = "; "),
    call. = FALSE
  )
}
if (length(library_paths) == 0L || !identical(library_paths[[1L]], runtime_library)) {
  stop(
    "Bundled-runtime isolation failed: the runtime library must be first in .libPaths(). ",
    "Expected ", runtime_library, "; found ", paste(library_paths, collapse = "; "),
    call. = FALSE
  )
}

rscript_candidates <- c(
  file.path(R.home("bin"), "Rscript.exe"),
  file.path(R.home("bin"), "Rscript")
)
rscript_candidates <- rscript_candidates[file.exists(rscript_candidates)]
if (length(rscript_candidates) == 0L) {
  stop("Bundled Rscript executable was not found under R.home().", call. = FALSE)
}
rscript <- canonical_path(rscript_candidates[[1L]], must_work = TRUE)
if (!path_is_within(rscript, runtime_root)) {
  stop("Bundled Rscript executable resolved outside runtime-root: ", rscript, call. = FALSE)
}

timeout_text <- arg_value("timeout-seconds", "900")
timeout_seconds <- suppressWarnings(as.numeric(timeout_text))
if (
  length(timeout_seconds) != 1L || !is.finite(timeout_seconds) ||
    timeout_seconds < 1 || timeout_seconds > .Machine$integer.max ||
    timeout_seconds != floor(timeout_seconds)
) {
  stop(
    "--timeout-seconds must be a positive whole number no greater than ",
    .Machine$integer.max, ".",
    call. = FALSE
  )
}
timeout_seconds <- as.integer(timeout_seconds)

validations <- list(
  list(
    label = "bundled runtime version contract",
    script = "scripts/validate_bundled_runtime_package_versions.R",
    args = c(paste0("--repo-root=", repo_root), paste0("--runtime-root=", runtime_root))
  ),
  list(
    label = "bundled validation dependency closure",
    script = "scripts/validate_bundled_validation_packages.R",
    args = c(paste0("--repo-root=", repo_root), paste0("--runtime-root=", runtime_root))
  ),
  list(label = "PLS/PLSc cSEM numerical oracle", script = "scripts/validate_pls_fit_csem.R"),
  list(label = "PLS public/synthetic external-comparator contract", script = "scripts/validate_pls_external_comparator.R"),
  list(label = "PLS model contract", script = "scripts/validate_pls_model_contract.R"),
  list(label = "PLS fail-closed core", script = "scripts/validate_pls_failclosed_core.R"),
  list(label = "PLS structural effect tables", script = "scripts/validate_pls_effect_tables.R"),
  list(label = "PLSc f-squared consistency", script = "scripts/validate_plsc_f2_consistency.R"),
  list(label = "PLS missing-data policy", script = "scripts/validate_pls_missing_policy.R"),
  list(label = "PLSc PLSpredict consistency", script = "scripts/validate_plsc_predict_consistency.R"),
  list(label = "PLS whole-draw bootstrap contract", script = "scripts/validate_pls_bootstrap_contract.R"),
  list(label = "PLS bootstrap parallel reproducibility", script = "scripts/validate_pls_bootstrap_parallel_reproducibility.R"),
  list(label = "PLS specific indirect effects", script = "scripts/validate_pls_specific_indirect_engine.R"),
  list(label = "PLS multi-group effects", script = "scripts/validate_pls_mga_effect_engine.R"),
  list(label = "PLS latent moderation", script = "scripts/validate_pls_latent_moderation_core.R"),
  list(label = "PLS moderated mediation", script = "scripts/validate_pls_modmed_engine.R"),
  list(label = "PLS moderated-mediation audit export", script = "scripts/validate_pls_modmed_audit_export.R"),
  list(label = "PLS workbook export", script = "scripts/validate_pls_workbook_export.R"),
  list(label = "PLS analysis-result roundtrip", script = "scripts/validate_pls_analysis_result_roundtrip.R")
)

validation_paths <- vapply(
  validations,
  function(validation) canonical_path(file.path(repo_root, validation$script), must_work = TRUE),
  character(1L)
)
outside_repo <- validation_paths[
  !vapply(validation_paths, path_is_within, logical(1L), root = repo_root)
]
if (length(outside_repo) > 0L) {
  stop(
    "Focused regression script resolved outside repo-root: ",
    paste(outside_repo, collapse = "; "),
    call. = FALSE
  )
}

# Set all R library selectors before starting children. --vanilla prevents
# profiles and Renviron files from adding user or developer libraries.
library_environment <- c(
  R_LIBS = runtime_library,
  R_LIBS_USER = runtime_library,
  R_LIBS_SITE = runtime_library,
  STATEDU_CSEM_VALIDATION_MODE = "required"
)
previous_environment <- Sys.getenv(names(library_environment), unset = NA_character_)
on.exit({
  present <- !is.na(previous_environment)
  if (any(present)) {
    do.call(Sys.setenv, as.list(previous_environment[present]))
  }
  if (any(!present)) {
    Sys.unsetenv(names(previous_environment)[!present])
  }
}, add = TRUE)
do.call(Sys.setenv, as.list(library_environment))

probe_expression <- paste0(
  "root <- ", deparse(runtime_root), "; ",
  "canon <- function(x) { x <- sub('/+$', '', normalizePath(x, winslash='/', mustWork=TRUE)); ",
  if (.Platform$OS.type == "windows") "tolower(x)" else "x",
  " }; paths <- vapply(.libPaths(), canon, character(1)); ",
  "inside <- paths == root | startsWith(paths, paste0(root, '/')); ",
  "if (!all(inside)) stop('child .libPaths isolation violation: ', paste(paths[!inside], collapse='; '), call.=FALSE); ",
  "expected <- canon(file.path(root, 'library')); ",
  "if (!identical(paths[[1]], expected)) stop('child runtime library is not first in .libPaths()', call.=FALSE); ",
  "cat('Bundled child library isolation PASS\\n')"
)
probe_status <- suppressWarnings(system2(
  rscript,
  c("--vanilla", "-e", shQuote(probe_expression)),
  stdout = "",
  stderr = "",
  wait = TRUE,
  timeout = min(timeout_seconds, 60L)
))
if (!identical(as.integer(probe_status), 0L)) {
  stop(
    "Bundled child-process library isolation probe failed with exit code ",
    as.integer(probe_status), ".",
    call. = FALSE
  )
}

old_working_directory <- getwd()
on.exit(setwd(old_working_directory), add = TRUE)
setwd(repo_root)

cat("Bundled PLS/PLSc focused regression driver\n")
cat("R home: ", current_r_home, "\n", sep = "")
cat("Library paths: ", paste(library_paths, collapse = "; "), "\n", sep = "")
cat("Validators: ", length(validations), "\n", sep = "")

started <- proc.time()[["elapsed"]]
results <- vector("list", length(validations))
for (index in seq_along(validations)) {
  validation <- validations[[index]]
  validation_args <- validation$args
  if (is.null(validation_args)) {
    validation_args <- character()
  }
  child_args <- c(
    "--vanilla",
    shQuote(validation_paths[[index]]),
    vapply(validation_args, shQuote, character(1L))
  )
  cat(sprintf("[%02d/%02d] %s\n", index, length(validations), validation$label))
  step_started <- proc.time()[["elapsed"]]
  status <- suppressWarnings(system2(
    rscript,
    child_args,
    stdout = "",
    stderr = "",
    wait = TRUE,
    timeout = timeout_seconds
  ))
  elapsed <- proc.time()[["elapsed"]] - step_started
  results[[index]] <- data.frame(
    Validator = validation$script,
    Status = as.integer(status),
    Seconds = as.numeric(elapsed),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!identical(as.integer(status), 0L)) {
    stop(
      "Bundled PLS/PLSc focused regression failed: ", validation$label,
      " (", validation$script, ") exited with status ", as.integer(status),
      " after ", sprintf("%.2f", elapsed), " seconds.",
      call. = FALSE
    )
  }
  cat(sprintf("         PASS (%.2fs)\n", elapsed))
}

results <- do.call(rbind, results)
total_elapsed <- proc.time()[["elapsed"]] - started
if (
  nrow(results) != length(validations) ||
    any(!is.finite(results$Seconds)) ||
    any(results$Status != 0L)
) {
  stop("Bundled PLS/PLSc focused regression summary is incomplete.", call. = FALSE)
}

cat(
  "Bundled PLS/PLSc focused regression PASS: ", nrow(results),
  " validators in ", sprintf("%.2f", total_elapsed), " seconds.\n",
  sep = ""
)
