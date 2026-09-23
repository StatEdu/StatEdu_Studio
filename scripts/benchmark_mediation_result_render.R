script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/benchmark_mediation_result_render.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)
if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "English_United States.utf8", LANG = "English_United States.utf8")
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE)))
}

suppressPackageStartupMessages(library(shiny))
tags <- htmltools::tags
tagList <- htmltools::tagList
source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "result_labels.R"))
source(file.path(repo_root, "R", "setup_analysis_ui.R"))
source(file.path(repo_root, "R", "result_table_ui.R"))
source(file.path(repo_root, "R", "result_coefficients.R"))
source(file.path(repo_root, "R", "result_panels_ui.R"))
source(file.path(repo_root, "R", "analysis_regression.R"))
source(file.path(repo_root, "R", "setup_mediation_moderation_ui.R"))
statedu_apply_preferences(statedu_default_preferences())

boot_r <- suppressWarnings(as.integer(Sys.getenv("STATEDU_MEDIATION_RENDER_BOOT_R", "5000")))
if (!is.finite(boot_r) || boot_r < 1L) stop("STATEDU_MEDIATION_RENDER_BOOT_R must be positive.")

set.seed(20260822L)
n <- 75L
fixture <- data.frame(
  X1 = stats::rnorm(n),
  X2 = stats::rnorm(n),
  C = stats::rnorm(n)
)
fixture$M1 <- .45 * fixture$X1 + .20 * fixture$X2 + .15 * fixture$C + stats::rnorm(n, sd = .72)
fixture$M2 <- .25 * fixture$X1 + .50 * fixture$X2 + .25 * fixture$C + stats::rnorm(n, sd = .76)
fixture$Y <- .18 * fixture$X1 + .12 * fixture$X2 + .48 * fixture$M1 +
  .36 * fixture$M2 + .10 * fixture$C + stats::rnorm(n, sd = .80)
roles <- list(
  y = "Y", x = c("X1", "X2"), mediators = c("M1", "M2"),
  w = character(0), covariates = "C"
)
variable_info <- data.frame(
  name = names(fixture), var_label = names(fixture), role = "",
  measurement = "continuous", stringsAsFactors = FALSE
)

old_worker <- getOption("statedu.mediation_moderation_worker")
options(statedu.mediation_moderation_worker = TRUE)
on.exit(options(statedu.mediation_moderation_worker = old_worker), add = TRUE)
analysis_started <- proc.time()[["elapsed"]]
result <- run_mediation_moderation_analysis(
  data = fixture,
  roles = roles,
  mediator_arrangement = "parallel",
  moderated_paths = character(0),
  boot_r = boot_r,
  seed = 20260822L,
  mean_center = FALSE,
  simple_slopes = FALSE,
  johnson_neyman = FALSE,
  analysis_method = "statedu",
  ci_method = "bias_corrected",
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  direct_x = c("X1", "X2"),
  x_to_m = list(M1 = c("X1", "X2"), M2 = c("X1", "X2")),
  m_to_y = list(Y = c("M1", "M2")),
  m_to_m = list(M1 = character(0), M2 = character(0)),
  moderated_x_to_m = list(),
  moderated_m_to_y = list(),
  moderation_map = list(),
  two_moderator_model = "3",
  custom_path_model = TRUE,
  effect_size_models = "y",
  covariate_control = c("y", "m"),
  language = "en",
  variable_info = variable_info,
  labels = character(0),
  category_table = NULL
)
analysis_elapsed <- unname(proc.time()[["elapsed"]] - analysis_started)
result$custom_model_canvas <- TRUE

ui_started <- proc.time()[["elapsed"]]
ui <- mediation_moderation_result_ui(
  result, language = "en", dash_nonsignificant = TRUE
)
ui_elapsed <- unname(proc.time()[["elapsed"]] - ui_started)
html_started <- proc.time()[["elapsed"]]
html <- htmltools::renderTags(ui)$html
html_elapsed <- unname(proc.time()[["elapsed"]] - html_started)

cat(sprintf(
  paste0(
    "analysis_seconds=%.3f ui_build_seconds=%.3f html_serialize_seconds=%.3f ",
    "render_seconds=%.3f html_bytes=%d boot_r=%d\n"
  ),
  analysis_elapsed, ui_elapsed, html_elapsed, ui_elapsed + html_elapsed,
  nchar(html, type = "bytes"), boot_r
))
