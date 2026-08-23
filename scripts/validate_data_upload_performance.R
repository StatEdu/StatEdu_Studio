script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_data_upload_performance.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

suppressPackageStartupMessages(library(shiny))

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_io.R"))
source(file.path(repo_root, "R", "server_data_state.R"))
source(file.path(repo_root, "R", "server_settings.R"))

latent_env <- new.env(parent = globalenv())
sys.source(
  file.path(repo_root, "modules", "latent_mplus", "app", "R", "app_server.R"),
  envir = latent_env
)

message("Checking that a Shiny temp upload never starts a recursive data-root scan...")
upload_path <- tempfile(pattern = "statedu-shiny-upload-")
writeLines(c("x,y", "1,2"), upload_path, useBytes = TRUE)
uploaded_file <- list(
  path = upload_path,
  name = "tiny-upload.csv",
  original_path = "",
  restored = FALSE
)

recursive_scan_calls <- 0L
latent_env$list.files <- function(path, ..., recursive = FALSE) {
  if (isTRUE(recursive)) {
    recursive_scan_calls <<- recursive_scan_calls + 1L
    return(character(0))
  }
  base::list.files(path, ..., recursive = recursive)
}

iterations <- 100L
exercise_temp_upload_resolution <- function() {
  for (i in seq_len(iterations)) {
    resolved <- latent_env$latent_resolved_data_file_path(uploaded_file, app_root = repo_root)
    dataset_id <- latent_env$dataset_id_from_data_file(uploaded_file)
    output_root <- latent_env$latent_output_root_from_data_file(uploaded_file, app_root = repo_root)
  }
  list(
    resolved = resolved,
    dataset_id = dataset_id,
    output_root = output_root
  )
}

validate_temp_upload_resolution <- function(result, scans, label) {
  stopifnot(
    identical(result$resolved, normalizePath(upload_path, winslash = "/", mustWork = TRUE)),
    identical(result$dataset_id, "tiny-upload"),
    identical(result$output_root, latent_env$latent_default_output_root(repo_root))
  )
  if (!identical(scans, 0L)) {
    stop(sprintf("%s started %d recursive data-root scan(s).", label, scans), call. = FALSE)
  }
}

run_temp_upload_resolution <- function(label, timed = FALSE) {
  scans_before <- recursive_scan_calls
  if (isTRUE(timed)) {
    result <- NULL
    elapsed <- system.time({
      result <- exercise_temp_upload_resolution()
    })[["elapsed"]]
  } else {
    result <- exercise_temp_upload_resolution()
    elapsed <- NA_real_
  }
  scans <- as.integer(recursive_scan_calls - scans_before)
  validate_temp_upload_resolution(result, scans, label)
  list(elapsed = elapsed, scans = scans)
}

warmup <- run_temp_upload_resolution("Temp upload cold-control sample", timed = TRUE)
timed_runs <- lapply(seq_len(9L), function(sample_index) {
  run_temp_upload_resolution(
    sprintf("Temp upload timed sample %d", sample_index),
    timed = TRUE
  )
})
timed_samples <- vapply(timed_runs, `[[`, numeric(1), "elapsed")
timed_scans <- vapply(timed_runs, `[[`, integer(1), "scans")
median_elapsed <- stats::median(timed_samples)
all_samples <- c(warmup$elapsed, timed_samples)
maximum_elapsed <- max(all_samples)
diagnostic_tail_seconds <- 10
operational_ceiling_seconds <- 30

if (!all(is.finite(all_samples)) || !is.finite(median_elapsed) || !is.finite(maximum_elapsed)) {
  stop(sprintf(
    "Temp upload path resolution produced a non-finite timing: %s.",
    paste(sprintf("%.3fs", all_samples), collapse = ", ")
  ), call. = FALSE)
}
if (any(all_samples >= operational_ceiling_seconds)) {
  stop(sprintf(
    "Temp upload path resolution exceeded the %.3fs operational runtime ceiling: %d iterations sampled at %s.",
    operational_ceiling_seconds,
    iterations,
    paste(sprintf("%.3fs", all_samples), collapse = ", ")
  ), call. = FALSE)
}
if (any(all_samples >= diagnostic_tail_seconds)) {
  message(sprintf(
    "DIAGNOSTIC WARNING: Temp upload path resolution reached the %.3fs diagnostic tail: %d iterations sampled at %s.",
    diagnostic_tail_seconds,
    iterations,
    paste(sprintf("%.3fs", all_samples), collapse = ", ")
  ))
}
if (median_elapsed >= 2) {
  stop(sprintf(
    "Temp upload path resolution median exceeded 2.000s: %d iterations sampled at %s; median %.3fs.",
    iterations,
    paste(sprintf("%.3fs", timed_samples), collapse = ", "),
    median_elapsed
  ), call. = FALSE)
}
message(sprintf(
  "Temp upload path resolution: %d-iteration cold-control %.3fs; timed samples %s; timed median %.3fs; maximum %.3fs; diagnostic tail %.3fs; operational runtime ceiling %.3fs; recursive scans: cold-control %d, timed %s",
  iterations,
  warmup$elapsed,
  paste(sprintf("%.3fs", timed_samples), collapse = ", "),
  median_elapsed,
  maximum_elapsed,
  diagnostic_tail_seconds,
  operational_ceiling_seconds,
  warmup$scans,
  paste(timed_scans, collapse = ", ")
))

expired_upload <- uploaded_file
expired_upload$path <- file.path(tempdir(), "statedu-expired-shiny-upload")
expired_resolved <- latent_env$latent_resolved_data_file_path(expired_upload, app_root = repo_root)
stopifnot(
  identical(expired_resolved, normalizePath(expired_upload$path, winslash = "/", mustWork = FALSE)),
  identical(recursive_scan_calls, 0L)
)

message("Checking that the legacy name-only fallback is cached...")
search_root <- tempfile(pattern = "statedu-latent-search-")
dir.create(search_root)
latent_env$latent_original_data_path_cache <- new.env(parent = emptyenv())
latent_env$latent_data_search_roots <- function(app_root = getwd()) search_root
recursive_scan_calls <- 0L
first_missing <- latent_env$latent_original_data_path_from_name("not-present.csv", app_root = repo_root)
second_missing <- latent_env$latent_original_data_path_from_name("not-present.csv", app_root = repo_root)
stopifnot(
  identical(first_missing, ""),
  identical(second_missing, ""),
  identical(recursive_scan_calls, 1L)
)

message("Checking that CSV import options are captured before the first read...")
upload_input <- data.frame(
  name = "tiny-upload.csv",
  size = file.info(upload_path)$size,
  type = "text/csv",
  datapath = upload_path,
  stringsAsFactors = FALSE
)

upload_observer_server <- function(input, output, session) {
  state <- new.env(parent = emptyenv())
  state$value <- NULL
  state$assignments <- 0L
  active_data_file <- function(value) {
    if (missing(value)) {
      return(state$value)
    }
    state$assignments <- state$assignments + 1L
    state$value <- value
    invisible(value)
  }
  register_data_input_observers(
    input = input,
    active_data_file = active_data_file,
    reset_on_dataset_load = function(value) invisible(value),
    mark_settings_dirty = function() invisible(TRUE)
  )
  session$userData$upload_state <- state
}

shiny::testServer(upload_observer_server, {
  session$setInputs(file = upload_input)
  session$setInputs(header = TRUE)
  state <- session$userData$upload_state
  stopifnot(
    identical(state$assignments, 1L),
    is.list(state$value),
    isTRUE(state$value$csv_header)
  )
})

message("Checking that one CSV upload causes exactly one source-data read...")
read_count <- 0L
original_read_current_data_file <- read_current_data_file
read_current_data_file <- function(file, input) {
  read_count <<- read_count + 1L
  original_read_current_data_file(file, input)
}

single_read_server <- function(input, output, session) {
  active_data_file <- reactiveVal(NULL)
  data_reactives <- create_data_reactives(input, active_data_file)
  register_data_input_observers(
    input = input,
    active_data_file = active_data_file,
    reset_on_dataset_load = function(value) invisible(value),
    mark_settings_dirty = function() invisible(TRUE)
  )
  observe(data_reactives$source_dataset())
  session$userData$data_reactives <- data_reactives
}

shiny::testServer(single_read_server, {
  session$setInputs(file = upload_input)
  session$setInputs(header = TRUE)
  loaded <- session$userData$data_reactives$source_dataset()
  stopifnot(
    identical(read_count, 1L),
    identical(names(loaded), c("x", "y")),
    nrow(loaded) == 1L
  )
})
read_current_data_file <- original_read_current_data_file

unlink(c(upload_path, search_root), recursive = TRUE, force = TRUE)
message("All data upload performance validations passed.")
