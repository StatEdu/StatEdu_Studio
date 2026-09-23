script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/benchmark_complex_sample_setup_render.R"
}
default_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(
  Sys.getenv("STATEDU_BENCHMARK_REPO", default_root),
  winslash = "/", mustWork = TRUE
)
setwd(repo_root)
if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))
statedu_apply_preferences(statedu_default_preferences())

variable_count <- suppressWarnings(as.integer(Sys.getenv("STATEDU_COMPLEX_SETUP_VARIABLES", "200")))
if (!is.finite(variable_count) || variable_count < 2L) {
  stop("STATEDU_COMPLEX_SETUP_VARIABLES must be at least 2.")
}
variables <- sprintf("variable_%04d", seq_len(variable_count))
variable_table <- data.frame(
  name = variables,
  var_label = paste("Variable", seq_len(variable_count)),
  measurement = rep(c("continuous", "nominal", "ordered"), length.out = variable_count),
  stringsAsFactors = FALSE
)
target_specs <- complex_sample_target_specs("regression")
selected <- stats::setNames(vector("list", 40L), character(40L))
repetitions <- suppressWarnings(as.integer(Sys.getenv("STATEDU_COMPLEX_SETUP_REPETITIONS", "5")))
if (!is.finite(repetitions) || repetitions < 1L) stop("STATEDU_COMPLEX_SETUP_REPETITIONS must be positive.")

samples <- lapply(seq_len(repetitions), function(index) {
  gc(FALSE)
  started <- proc.time()[["elapsed"]]
  ui <- complex_sample_setup_panel(
    prefix = "complex_regression",
    selected_names = variables,
    all_names = variables,
    target_specs = target_specs,
    target_values = list(outcome = character(0), predictors = character(0)),
    variable_table = variable_table,
    labels = character(0),
    language = "en",
    selected = selected,
    analysis_type = "regression",
    show_design_tabs = FALSE
  )
  build_elapsed <- unname(proc.time()[["elapsed"]] - started)
  render_started <- proc.time()[["elapsed"]]
  html <- htmltools::renderTags(ui)$html
  render_elapsed <- unname(proc.time()[["elapsed"]] - render_started)
  list(build = build_elapsed, render = render_elapsed, html = html)
})
build_values <- vapply(samples, `[[`, numeric(1), "build")
render_values <- vapply(samples, `[[`, numeric(1), "render")
html <- samples[[length(samples)]]$html
cat(sprintf(
  paste0(
    "variables=%d repetitions=%d build_median_seconds=%.4f render_median_seconds=%.4f ",
    "build_samples=%s render_samples=%s html_bytes=%d hidden_design=%s\n"
  ),
  variable_count, repetitions, stats::median(build_values), stats::median(render_values),
  paste(format(build_values, digits = 4, nsmall = 3), collapse = ","),
  paste(format(render_values, digits = 4, nsmall = 3), collapse = ","),
  nchar(html, type = "bytes"),
  grepl("complex-sample-hidden-design-inputs", html, fixed = TRUE)
))
