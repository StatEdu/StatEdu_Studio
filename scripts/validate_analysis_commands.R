Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE_DIR = file.path(tempdir(), "analysis-command-check"))
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
iris <- data.frame(Sepal.Length = c(5, 6, 4, 7, 5.5), Petal.Length = c(1, 3, 2, 5, 2.5))

spec <- list(VERSION = 1L, STUDIO_VERSION = readLines("VERSION")[1], ANALYSIS = "run_pca",
             DATA_HASH = regression_syntax_data_hash(iris), CONTEXT_HASH = regression_syntax_data_hash(names(iris)),
             STATE = list(variables = c("Sepal.Length", "Petal.Length"), groups = list(c("x", "y"), c("z", "w"))),
             OPTIONS = list(pca_rotation = "varimax", pca_n_components = 2L))
text <- analysis_command_text(spec)
parsed <- parse_analysis_command(text, "run_pca", names(spec$STATE), names(spec$OPTIONS))
stopifnot(identical(analysis_command_restore_value(parsed$STATE$groups, list()), spec$STATE$groups))
positions <- regexpr("=", strsplit(text, "\n")[[1]], fixed = TRUE)
stopifnot(length(unique(positions[positions > 0])) == 1L)
fails <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))
fails(parse_analysis_command(text, "run_correlation", names(spec$STATE), names(spec$OPTIONS)))
fails(parse_analysis_command(sub("STATE_VARIABLES", "STATE_UNKNOWN", text), "run_pca", names(spec$STATE), names(spec$OPTIONS)))
fails(parse_analysis_command(sub("END", "  VERSION = 1\nEND", text), "run_pca", names(spec$STATE), names(spec$OPTIONS)))
fails(parse_analysis_command(sub('"varimax"', 'system("bad")', text, fixed = TRUE), "run_pca", names(spec$STATE), names(spec$OPTIONS)))
empty_spec <- spec
empty_spec$STATE <- list()
empty_spec$OPTIONS <- list()
stopifnot(length(parse_analysis_command(analysis_command_text(empty_spec), "run_pca", character(), character())$OPTIONS) == 0L)

# Meta-analysis columns must keep missing cells and their row positions.
rows <- data.frame(study = c("a", "b"), estimate = c(NA_real_, 0.4), included = c(TRUE, FALSE))
decoded <- jsonlite::fromJSON(analysis_command_json(rows), simplifyVector = FALSE)
stopifnot(identical(rows, analysis_command_restore_value(decoded, rows[FALSE, ])))
model <- list(nodes = list(list(id = "a", x = 10, selected = FALSE), list(id = "b", x = 30)),
              edges = list(list(from = "a", to = "b")), covariates = list())
decoded <- jsonlite::fromJSON(analysis_command_json(model), simplifyVector = FALSE)
stopifnot(isTRUE(all.equal(model, analysis_command_restore_value(decoded, NULL))))

# Replaying a command must wait for the actual UI options, including a changed variable order.
shiny::testServer(function(input, output, session) {
  vars <- reactiveVal(c("Sepal.Length", "Petal.Length"))
  result <- reactiveVal(NULL)
  runs <- reactiveVal(0L)
  register_analysis_command_handler("run_pca", input, output, session,
    states = list(variables = vars), dataset_fn = function() iris,
    context_fn = function() names(iris), run_fn = function() {
      result(stats::prcomp(iris[vars()], scale. = isTRUE(input$pca_scale)))
      runs(runs() + 1L)
    })
}, {
  session$setInputs(pca_scale = FALSE, run_pca_open_regression_syntax = list(options = list("pca_scale")))
  command <- list(VERSION = 1L, STUDIO_VERSION = readLines("VERSION")[1], ANALYSIS = "run_pca",
                  DATA_HASH = regression_syntax_data_hash(iris), CONTEXT_HASH = regression_syntax_data_hash(names(iris)),
                  STATE = list(variables = c("Petal.Length", "Sepal.Length")), OPTIONS = list(pca_scale = TRUE))
  session$setInputs(run_pca_regression_syntax_request = list(text = analysis_command_text(command), allow = FALSE, action = "run"))
  stopifnot(runs() == 0L)
  session$setInputs(run_pca_command_ack = list(token = 99L))
  stopifnot(runs() == 0L)
  session$setInputs(pca_scale = TRUE, run_pca_command_ack = list(token = 1L))
  stopifnot(runs() == 1L, identical(result(), stats::prcomp(iris[c("Petal.Length", "Sepal.Length")], scale. = TRUE)))
  command$DATA_HASH <- "different"
  session$setInputs(run_pca_regression_syntax_request = list(text = analysis_command_text(command), allow = FALSE, action = "run"))
  stopifnot(runs() == 1L)
})

stopifnot(grepl("stateduOpenCommand", as.character(frequencies_tab_panel()), fixed = TRUE))
stopifnot(grepl("stateduOpenCommand", as.character(structural_equation_toolbar("cfa")), fixed = TRUE))
cat("Analysis command validation passed.\n")
