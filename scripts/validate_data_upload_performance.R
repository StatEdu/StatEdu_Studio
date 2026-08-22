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
elapsed <- system.time({
  for (i in seq_len(iterations)) {
    resolved <- latent_env$latent_resolved_data_file_path(uploaded_file, app_root = repo_root)
    dataset_id <- latent_env$dataset_id_from_data_file(uploaded_file)
    output_root <- latent_env$latent_output_root_from_data_file(uploaded_file, app_root = repo_root)
  }
})[["elapsed"]]

stopifnot(
  identical(resolved, normalizePath(upload_path, winslash = "/", mustWork = TRUE)),
  identical(dataset_id, "tiny-upload"),
  identical(output_root, latent_env$latent_default_output_root(repo_root)),
  identical(recursive_scan_calls, 0L)
)
if (!is.finite(elapsed) || elapsed >= 2) {
  stop(sprintf(
    "Temp upload path resolution exceeded 2.000s: %d iterations took %.3fs.",
    iterations, elapsed
  ), call. = FALSE)
}
message(sprintf("Temp upload path resolution: %d iterations in %.3fs; recursive scans: %d", iterations, elapsed, recursive_scan_calls))

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
