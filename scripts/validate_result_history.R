source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "result_saved_ui.R"), encoding = "UTF-8")
library(shiny)

entry <- list(
  id = "saved_result_test",
  title = "Stored result",
  saved_at = "2026-06-09 12:00:00",
  html = "<html><body><p>result</p></body></html>"
)

store_file <- tempfile("statedu_result_history_", fileext = ".json")
old_store <- Sys.getenv("STATEDU_RESULT_STORE", unset = NA_character_)
Sys.setenv(STATEDU_RESULT_STORE = store_file)
on.exit({
  if (is.na(old_store)) {
    Sys.unsetenv("STATEDU_RESULT_STORE")
  } else {
    Sys.setenv(STATEDU_RESULT_STORE = old_store)
  }
  unlink(store_file)
}, add = TRUE)

stopifnot(isTRUE(write_result_snapshot_store(list(entry), store_file)))
stopifnot(length(read_result_snapshot_store(store_file)) == 1L)

session <- new.env(parent = emptyenv())
session$userData <- new.env(parent = emptyenv())
store <- result_accumulator_store(session)
stopifnot(length(shiny::isolate(store())) == 0L)

store(list(entry))
stopifnot(length(shiny::isolate(store())) == 1L)
clear_result_accumulator_store(session)
stopifnot(length(shiny::isolate(store())) == 0L)
stopifnot(length(read_result_snapshot_store(store_file)) == 0L)

message("Result history validation passed.")
