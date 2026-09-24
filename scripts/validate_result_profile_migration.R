source('R/utils.R', encoding='UTF-8')
source('R/result_saved_ui.R', encoding='UTF-8')
local({
  original_dir <- getwd()
  original_env <- Sys.getenv(c('STATEDU_USER_DATA_DIR', 'STATEDU_RESULT_STORE'), unset=NA_character_)
  on.exit({
    setwd(original_dir)
    for (key in names(original_env)) {
      if (is.na(original_env[[key]])) Sys.unsetenv(key)
      else do.call(Sys.setenv, setNames(list(original_env[[key]]), key))
    }
  })
  root <- tempfile('history-profile-')
  dir.create(root)
  setwd(root)
  dir.create('data')
  Sys.unsetenv('STATEDU_RESULT_STORE')
  Sys.setenv(STATEDU_USER_DATA_DIR=file.path(root, 'profile'))
  target <- result_snapshot_store_path()
  stopifnot(identical(target, file.path(root, 'profile', 'data', 'StatEdu_Studio_results.json')))
  old <- file.path('data', 'StatEdu_Studio_results.json')
  entry <- list(id='legacy-1', title='Stored', saved_at='2026-09-15', html='<p>0.123</p>')
  stopifnot(write_result_snapshot_store(list(entry), old))
  bytes <- function(path) readBin(path, 'raw', n=file.info(path)$size)
  original <- bytes(old)
  stopifnot(identical(read_result_snapshot_store(), list(entry)), identical(bytes(target), original), identical(bytes(old), original))
  # Clearing the new store must not resurrect the retained original.
  stopifnot(write_result_snapshot_store(list()), identical(read_result_snapshot_store(), list()), identical(bytes(old), original))
  # An explicit store is isolated, even when the application has a legacy file.
  Sys.setenv(STATEDU_RESULT_STORE=file.path(root, 'explicit.json'))
  stopifnot(identical(read_result_snapshot_store(), list()))
  Sys.unsetenv('STATEDU_RESULT_STORE')
  Sys.setenv(STATEDU_USER_DATA_DIR=file.path(root, 'corrupt-profile'))
  writeLines('{broken', old)
  corrupt <- bytes(old)
  stopifnot(inherits(try(read_result_snapshot_store(), silent=TRUE), 'try-error'),
            !file.exists(result_snapshot_store_path()), identical(bytes(old), corrupt))
  # A later save writes the profile copy without replacing the corrupt original.
  stopifnot(write_result_snapshot_store(list(entry)), identical(bytes(old), corrupt),
            identical(read_result_snapshot_store(), list(entry)))
})
cat('PASS: profile path, byte-preserving migration, clear persistence, explicit isolation, corrupt original preservation\n')
