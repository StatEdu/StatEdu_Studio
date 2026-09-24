Sys.setlocale('LC_CTYPE', 'English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE = 'false')
.libPaths(R.home('library'))
source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE); source_app_modules()
root <- tempfile('settings-reconnect-'); dir.create(root)
source_path <- file.path(root, 'uploaded.csv')
data <- data.frame(group = rep(1:2, 20), x = 1:40, y = 1:40 + sin(1:40))
write.csv(data, source_path, row.names = FALSE)
original <- readBin(source_path, 'raw', n = file.info(source_path)$size)
settings <- list(data_file = 'uploaded.csv', data_file_path = source_path,
  data_file_options = list(csv_header = TRUE), selected_variables = names(data),
  measurement_overrides = list(group = 'binary', x = 'continuous', y = 'continuous'))
settings_path <- file.path(root, 'saved.studio')
write_settings_json_file(settings, settings_path)
saved <- read_settings_json_file(settings_path)
stopifnot(identical(readBin(source_path, 'raw', n = length(original)), original))
# Simulate the prior Shiny session's temporary upload disappearing.
unlink(source_path)
restored <- settings_restored_data_file(saved, settings_path)
stopifnot(valid_data_file_value(restored), isTRUE(restored$csv_header),
  identical(readBin(restored$path, 'raw', n = length(original)), original))
loaded <- read_current_data_file(restored, list())
stopifnot(identical(names(loaded), names(data)), nrow(loaded) == nrow(data))
info <- data.frame(name = names(data), measurement = c('binary', 'continuous', 'continuous'))
stopifnot(length(prepare_frequencies_results(loaded, names(data), info)$variables) == 3L,
  !is.null(prepare_correlation_results(loaded, c('x', 'y'), info)))
# A second save from the extracted temporary file must also be portable.
saved$data_file_path <- restored$path
write_settings_json_file(saved, file.path(root, 'saved-again.studio'))
unlink(restored$path)
again <- settings_restored_data_file(read_settings_json_file(file.path(root, 'saved-again.studio')))
stopifnot(valid_data_file_value(again))
legacy <- settings; legacy$data_file_content_base64 <- NULL
stopifnot(is.null(settings_restored_data_file(legacy)))
register_regression_syntax <- function(...) invisible(NULL)
for (run_id in c('run_frequencies', 'run_ttest_anova', 'run_correlation', 'run_ipa')) {
  shiny::testServer(function(input, output, session) {
    data_state <- reactiveVal(NULL); runs <- reactiveVal(0L); notifications <- reactiveVal(list())
    session$sendNotification <- function(type, message) { notifications(c(notifications(), list(message))) }
    register_analysis_command_handler(run_id, input, output, session, states = list(),
      dataset_fn = function() { req(data_state()); data_state() }, context_fn = function() list(),
      run_fn = function() { runs(runs() + 1L) })
  }, {
    session$flushReact()
    do.call(session$setInputs, setNames(list(1L), run_id))
    stopifnot(runs() == 0L, length(notifications()) == 1L)
    data_state(loaded); session$flushReact()
    do.call(session$setInputs, setNames(list(2L), run_id))
    do.call(session$setInputs, setNames(list(3L), run_id))
    stopifnot(runs() == 2L)
  })
}
cat('PASS: portable settings after upload deletion, second save, preserved bytes/options, missing-data notice, repeated runs after reconnect\n')
