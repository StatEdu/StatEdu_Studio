Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE_DIR = file.path(tempdir(), "syntax-module-cache"))
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
set.seed(182)
data <- data.frame(x = rnorm(90), z = rnorm(90), group = rep(1:3, 30))
data$y <- 2 * data$x - data$z + rnorm(90)
data$y2 <- data$x + rnorm(90)
data$x[1:3] <- NA
info <- data.frame(name = names(data), var_label = names(data),
                   measurement = ifelse(names(data) == "group", "category", "continuous"))
spec <- list(VERSION = 1L, STUDIO_VERSION = "test", DATA = "설문.csv",
             DATA_HASH = regression_syntax_data_hash(data), DEPENDENTS = c("y", "y2"),
             PREDICTORS = c("x", "group"), MEASUREMENTS = regression_syntax_measurements(info),
             REFERENCES = list(group = "2"), MISSING = "LISTWISE", CI_METHOD = "bias_corrected",
             BOOTSTRAP = 1000L, SEED = 728L, RESIDUAL_DIAGNOSTICS = FALSE, AUTO_METHOD = FALSE,
             SHOW_SR2 = TRUE, SHOW_F2 = FALSE, SHOW_VIF = TRUE, OUTPUT_STYLE = "wide")
text <- regression_syntax_text(spec)
parsed <- parse_regression_syntax(text)
stopifnot(identical(spec, parsed))
validate_regression_syntax_context(parsed, data, info, c(group = "2"))
menu <- prepare_regression_analysis_results(data, c("y", "y2"), c("x", "group"),
          variable_info = info, reference_values = c(group = "2"), boot_r = 1000L,
          seed = 728L, residual_diagnostics = FALSE, auto_method = FALSE)
command <- prepare_regression_syntax(parsed, data, info)
for (i in 1:2) {
  stopifnot(identical(menu$results[[i]]$coef_table, command$results[[i]]$coef_table),
            command$results[[i]]$n == 87L)
}
changed <- parsed
changed$PREDICTORS <- "z"
stopifnot(!identical(coef(prepare_regression_syntax(changed, data, info)$results[[1]]$model),
                     coef(command$results[[1]]$model)))
fails <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))
fails(parse_regression_syntax(sub("END", "SEED = 9\nEND", text, fixed = TRUE)))
fails(parse_regression_syntax(sub("END", "SYSTEM = \"whoami\"\nEND", text, fixed = TRUE)))
fails(parse_regression_syntax(sub("728", "system('whoami')", text, fixed = TRUE)))
fails(parse_regression_syntax(sub("728", "1.5", text, fixed = TRUE)))
fails(parse_regression_syntax(sub("VERSION[ ]+= 1", "VERSION = 2", text)))
fails(parse_regression_syntax(sub('"x","group"', '"x","x"', text, fixed = TRUE)))
fails(parse_regression_syntax(paste(text, text, sep = "\n")))
changed_data <- data
changed_data$x[4] <- 999
fails(validate_regression_syntax_context(parsed, changed_data, info, c(group = "2")))
validate_regression_syntax_context(parsed, changed_data, info, c(group = "2"), TRUE)
fails(validate_regression_syntax_context(parsed, data, info, c(group = "1")))
fails(validate_regression_syntax_context(parsed, data[-1], info, c(group = "2"), TRUE))
changed_info <- info
changed_info$measurement[1] <- "category"
fails(validate_regression_syntax_context(parsed, data, changed_info, c(group = "2")))
unicode <- parsed
unicode$DEPENDENTS <- '만족도 "점수"'
unicode$PREDICTORS <- c("신뢰,관계", "서비스 품질")
unicode$REFERENCES <- list()
stopifnot(identical(unicode, parse_regression_syntax(regression_syntax_text(unicode))))
path <- tempfile(fileext = ".stcmd")
writeLines(enc2utf8(text), path, useBytes = TRUE)
stopifnot(identical(parsed, parse_regression_syntax(paste(readLines(path, encoding = "UTF-8"), collapse = "\n"))))

# Exercise the actual Shiny command handlers and shared execution callback.
hierarchical <- spec
hierarchical$BLOCK1 <- "x"
hierarchical$BLOCK2 <- "group"
hierarchical$BLOCK3 <- character(0)
stopifnot(identical(hierarchical, parse_regression_syntax(regression_syntax_text(hierarchical))))
hm <- prepare_hierarchical_analysis_results(data, c("y", "y2"), "x", "group", character(0),
  variable_info = info, reference_values = c(group = "2"), boot_r = 1000L, seed = 728L,
  residual_diagnostics = FALSE, auto_method = FALSE)
hs <- prepare_regression_syntax(hierarchical, data, info)
stopifnot(isTRUE(all.equal(hm, hs)), regression_results_are_hierarchical(hs$results))
broken_blocks <- hierarchical
broken_blocks$BLOCK2 <- "x"
fails(parse_regression_syntax(regression_syntax_text(broken_blocks)))
# Automatic bootstrap must preserve seeds, sample counts, model matrices and queue order.
boot_data <- data
boot_data$y <- exp(seq_len(nrow(data)) / 10)
boot_spec <- hierarchical
boot_spec$RESIDUAL_DIAGNOSTICS <- boot_spec$AUTO_METHOD <- TRUE
bm <- prepare_hierarchical_analysis_results(boot_data, c("y", "y2"), "x", "group", character(0),
  variable_info = info, reference_values = c(group = "2"), boot_r = 1000L, seed = 728L)
bs <- prepare_regression_syntax(boot_spec, boot_data, info)
stopifnot(length(bs$jobs) > 0, length(bs$jobs) == length(bm$jobs))
for (i in seq_along(bs$jobs)) for (field in c("model_matrix", "outcome", "r", "seed", "ci_method", "result_index"))
  stopifnot(identical(bs$jobs[[i]][[field]], bm$jobs[[i]][[field]]))
shiny::testServer(function(input, output, session) {
  state <- create_analysis_state()
  manager <- list(start = function(job) NULL, cancel = function() NULL, poll = function() NULL)
  runner <- register_analysis_run_handlers(input, session, function() menu,
    state$penalized_result, state$analysis_result, state$bootstrap_job, state$bootstrap_job_queue,
    state$bootstrap_cancel_requested, state$bootstrap_status, state$bootstrap_stop_visible, manager)
  applied <- reactiveVal(NULL)
  draft <- register_regression_syntax(input, output, session, function() spec,
    function(s, allow) validate_regression_syntax_context(s, data, info, c(group = "2"), allow),
    function(s) applied(s), function(s) prepare_regression_syntax(s, data, info), runner, "test")
}, {
  session$setInputs(generate_regression_syntax = 1)
  stopifnot(identical(parse_regression_syntax(draft()), spec))
  session$setInputs(regression_syntax_text = text)
  session$setInputs(regression_syntax_request = list(text = regression_syntax_text(changed), action = "save", allow = FALSE))
  stopifnot(identical(parse_regression_syntax(draft()), changed))
  session$setInputs(regression_syntax_request = list(text = text, action = "run", allow = FALSE))
  stopifnot(identical(applied(), spec), length(state$analysis_result()) == 2L)
  session$setInputs(regression_syntax_request = list(text = regression_syntax_text(changed), action = "run", allow = FALSE))
  stopifnot(identical(applied()$PREDICTORS, "z"),
            identical(state$analysis_result()[[1]]$predictors, "z"))
  before <- state$analysis_result()
  session$setInputs(regression_syntax_request = list(text = "invalid", action = "run", allow = FALSE))
  stopifnot(identical(before, state$analysis_result()))
  session$setInputs(load_regression_syntax = list(size = file.info(path)$size, datapath = path))
  stopifnot(identical(parse_regression_syntax(draft()), spec))
  session$setInputs(regression_syntax_request = list(text = text, action = "apply", allow = FALSE))
  stopifnot(identical(applied(), spec), identical(before, state$analysis_result()))
  state$bootstrap_job(list(active = TRUE))
  fails(runner(menu))
  state$bootstrap_job(NULL)
  session$setInputs(run = 1)
  stopifnot(identical(state$analysis_result()[[1]]$coef_table, menu$results[[1]]$coef_table))
})
unlink(path)
cat("Regression syntax: parser, UTF-8 round trip, context guards, statistical parity and Shiny execution passed.\n")
