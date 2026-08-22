script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) "scripts/validate_custom_model_canvas.R"
)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)
options(statedu.output_decimal_digits = 3L)

suppressPackageStartupMessages(library(shiny))
tags <- htmltools::tags
div <- htmltools::tags$div
tagList <- htmltools::tagList
source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "result_labels.R"))
source(file.path(repo_root, "R", "setup_analysis_ui.R"))
source(file.path(repo_root, "R", "result_table_ui.R"))
source(file.path(repo_root, "R", "result_coefficients.R"))
source(file.path(repo_root, "R", "result_panels_ui.R"))
source(file.path(repo_root, "R", "analysis_regression.R"))
source(file.path(repo_root, "R", "setup_ui.R"))
source(file.path(repo_root, "R", "setup_mediation_moderation_ui.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_snapshot.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_i18n.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_variables.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_components.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_options.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_toolbar.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_structural_toolbar_icons.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_result_snapshot.R"))
source(file.path(repo_root, "R", "setup_custom_model_canvas_ui.R"))

bridge_source <- paste(
  readLines(file.path(repo_root, "www", "model-canvas", "shiny-bridge.js"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(
  grepl('root.querySelector(\'.custom-model-toolbar-panel[data-toolbar-panel="result"]\')', bridge_source, fixed = TRUE),
  grepl('window.StatEduModelCanvas.canvas.showResult(instance);', bridge_source, fixed = TRUE),
  grepl("cachedStateForRoot", bridge_source, fixed = TRUE),
  grepl("StatEduModelCanvasMountCache", bridge_source, fixed = TRUE)
)
canvas_source <- paste(
  readLines(file.path(repo_root, "www", "model-canvas", "canvas.js"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(
  grepl("cachedCanvas && cachedCanvas.source", canvas_source, fixed = TRUE),
  grepl("bridge.cacheInstance(instance, instance.sourceSnapshot)", canvas_source, fixed = TRUE)
)
custom_canvas_server_source <- paste(
  readLines(file.path(repo_root, "R", "setup_custom_model_canvas_ui.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(grepl('rootId = "custom-model-canvas-root"', custom_canvas_server_source, fixed = TRUE))

message("Checking custom model canvas snapshot-to-analysis maps...")

node <- function(id, variable, role, x, y) {
  list(id = id, variableId = variable, role = role, x = x, y = y)
}

edge <- function(id, from, to) {
  list(id = id, from = from, to = to)
}

moderation <- function(id, from, to_edge) {
  list(id = id, from = from, toEdge = to_edge)
}

snapshot <- list(
  nodes = list(
    node("x1", "X1", "independent", 80, 120),
    node("x2", "X2", "independent", 80, 220),
    node("m1", "M1", "mediator", 260, 120),
    node("m2", "M2", "mediator", 440, 120),
    node("y", "Y", "dependent", 620, 120),
    node("w", "W", "moderator", 260, 20),
    node("z", "Z", "moderator", 440, 20),
    node("unused_w", "UnusedW", "moderator", 620, 20)
  ),
  edges = list(
    edge("e_x1_m1", "x1", "m1"),
    edge("e_x2_m1", "x2", "m1"),
    edge("e_m1_m2", "m1", "m2"),
    edge("e_m2_y", "m2", "y"),
    edge("e_x1_y", "x1", "y")
  ),
  moderations = list(
    moderation("mod_xm", "w", "e_x1_m1"),
    moderation("mod_my", "z", "e_m2_y"),
    moderation("mod_xy", "w", "e_x1_y")
  ),
  covariates = c("C")
)

message("Checking custom model canvas snapshots survive reactive UI rebuilds...")
workspace_html <- htmltools::renderTags(custom_model_canvas_workspace(
  selected_names = c("X1", "X2", "M1", "M2", "Y", "W", "Z", "UnusedW", "C"),
  initial_snapshot = snapshot
))$html
stopifnot(
  grepl("data-initial-snapshot", workspace_html, fixed = TRUE),
  grepl("e_x1_m1", workspace_html, fixed = TRUE),
  grepl("custom-model-canvas-root", workspace_html, fixed = TRUE)
)
result_workspace_html <- htmltools::renderTags(custom_model_canvas_workspace(
  selected_names = c("X1", "X2", "M1", "M2", "Y", "W", "Z", "UnusedW", "C"),
  initial_snapshot = snapshot,
  initial_result_snapshot = snapshot,
  initial_view = "result"
))$html
stopifnot(
  grepl("data-result-snapshot", result_workspace_html, fixed = TRUE),
  grepl('data-initial-view="result"', result_workspace_html, fixed = TRUE),
  grepl("parseResultSnapshot", canvas_source, fixed = TRUE)
)

message("Checking bootstrap progress remains monotonic and ETA waits for a stable rate...")
progress_dir <- tempfile("statedu-progress-validation-")
dir.create(progress_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(progress_dir, recursive = TRUE, force = TRUE), add = TRUE)
progress_file <- file.path(progress_dir, "progress.rds")
progress_started_at <- Sys.time() - 20
progress_state <- new.env(parent = emptyenv())
progress_state$progress <- list(
  phase = "starting", done = 0L, total = 1000L, focal = "X1", boot_r = 1000L,
  updated_at = progress_started_at
)
progress_state$last_sample_done <- 0L
progress_state$last_sample_at <- as.numeric(progress_started_at)
progress_state$rate_samples <- numeric(0)
progress_job <- list(
  progress_file = progress_file,
  requested_total = 1000L,
  boot_r = 1000L,
  started_at = progress_started_at,
  progress_state = progress_state
)
write_progress_sample <- function(done, elapsed, phase = "resampling") {
  saveRDS(list(
    phase = phase, done = as.integer(done), total = 1000L, focal = "X1", boot_r = 1000L,
    updated_at = progress_started_at + elapsed
  ), progress_file)
}
progress_samples <- lapply(seq_along(c(100L, 200L, 300L, 400L)), function(index) {
  write_progress_sample(c(100L, 200L, 300L, 400L)[[index]], 10 + index)
  mediation_moderation_bootstrap_job_progress(progress_job, "ko")
})
stopifnot(
  is.na(progress_samples[[1L]]$rate),
  grepl("재표집 잔여 계산 중", progress_samples[[1L]]$detail, fixed = TRUE),
  isTRUE(all.equal(progress_samples[[4L]]$rate, 100, tolerance = 0.01)),
  isTRUE(all.equal(progress_samples[[4L]]$remaining, 6, tolerance = 0.01))
)
write_progress_sample(200L, 15)
regressed_progress <- mediation_moderation_bootstrap_job_progress(progress_job, "ko")
writeLines("temporarily incomplete", progress_file, useBytes = TRUE)
unreadable_progress <- mediation_moderation_bootstrap_job_progress(progress_job, "ko")
stopifnot(
  identical(regressed_progress$done, 400L),
  identical(unreadable_progress$done, 400L),
  identical(unreadable_progress$percent, 40)
)
mediation_moderation_write_bootstrap_progress(progress_file, 500L, 1000L, "X1", 1000L)
stopifnot(identical(readRDS(progress_file)$done, 500L))
write_progress_sample(1000L, 20, phase = "finalizing")
finalizing_progress <- mediation_moderation_bootstrap_job_progress(progress_job, "ko")
stopifnot(
  identical(finalizing_progress$phase_label, "부트스트랩 통계 계산 중"),
  is.na(finalizing_progress$percent),
  is.na(finalizing_progress$rate),
  is.na(finalizing_progress$remaining)
)
stage_progress_state <- new.env(parent = emptyenv())
stage_progress_state$progress <- NULL
stage_progress_state$last_sample_done <- 0L
stage_progress_state$last_sample_at <- as.numeric(progress_started_at)
stage_progress_state$rate_samples <- numeric(0)
stage_progress_job <- progress_job
stage_progress_job$progress_state <- stage_progress_state
write_progress_sample(0L, 21, phase = "preparing")
preparing_progress <- mediation_moderation_bootstrap_job_progress(stage_progress_job, "ko")
stopifnot(
  identical(preparing_progress$phase_label, "모형 준비 중"),
  grepl("모형 행렬과 진단 통계를 준비", preparing_progress$detail, fixed = TRUE),
  is.na(preparing_progress$percent)
)
write_progress_sample(1000L, 22, phase = "serializing")
serializing_progress <- mediation_moderation_bootstrap_job_progress(stage_progress_job, "ko")
stopifnot(
  identical(serializing_progress$phase_label, "결과 저장 중"),
  grepl("분석 결과를 저장", serializing_progress$detail, fixed = TRUE),
  is.na(serializing_progress$percent)
)
write_progress_sample(1000L, 22.5, phase = "finalizing")
stage_regressed_progress <- mediation_moderation_bootstrap_job_progress(stage_progress_job, "ko")
stopifnot(identical(stage_regressed_progress$phase, "serializing"))
write_progress_sample(1000L, 23, phase = "complete")
complete_progress <- mediation_moderation_bootstrap_job_progress(stage_progress_job, "ko")
stopifnot(identical(complete_progress$percent, 100))

message("Checking mediation/moderation postprocessing caches repeated disk reads...")
rm(list = ls(envir = mediation_moderation_dw_critical_cache, all.names = TRUE), envir = mediation_moderation_dw_critical_cache)
original_dw_table_reader <- mediation_moderation_read_dw_critical_table
dw_table_read_count <- 0L
mediation_moderation_read_dw_critical_table <- function(...) {
  dw_table_read_count <<- dw_table_read_count + 1L
  original_dw_table_reader(...)
}
cached_dw_first <- mediation_moderation_cached_dw_critical(75L, 3L)
cached_dw_second <- mediation_moderation_cached_dw_critical(75L, 3L)
cached_dw_other_p <- mediation_moderation_cached_dw_critical(75L, 4L)
mediation_moderation_read_dw_critical_table <- original_dw_table_reader
stopifnot(
  identical(cached_dw_first, cached_dw_second),
  is.list(cached_dw_other_p),
  identical(dw_table_read_count, 1L)
)
bootstrap_worker_source <- paste(
  readLines(file.path(repo_root, "R", "setup_mediation_moderation_ui.R"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(
  grepl('"result_panels_ui.R"', bootstrap_worker_source, fixed = TRUE),
  grepl("worker_preferences <- args$worker_preferences", bootstrap_worker_source, fixed = TRUE),
  grepl("args$worker_preferences <- NULL", bootstrap_worker_source, fixed = TRUE),
  grepl("statedu_apply_preferences(worker_preferences)", bootstrap_worker_source, fixed = TRUE),
  grepl('boot_r, "serializing"', bootstrap_worker_source, fixed = TRUE)
)

message("Checking standard and custom bootstrap jobs share one session coordinator...")
coordinator_session <- new.env(parent = emptyenv())
coordinator_session$userData <- new.env(parent = emptyenv())
cancelled_owner <- character(0)
mediation_moderation_claim_bootstrap(
  coordinator_session,
  "mediation_moderation",
  function() cancelled_owner <<- c(cancelled_owner, "mediation_moderation")
)
mediation_moderation_claim_bootstrap(
  coordinator_session,
  "custom_model_canvas",
  function() cancelled_owner <<- c(cancelled_owner, "custom_model_canvas")
)
coordinator <- mediation_moderation_bootstrap_coordinator(coordinator_session)
stopifnot(
  identical(cancelled_owner, "mediation_moderation"),
  identical(coordinator$owner, "custom_model_canvas")
)
mediation_moderation_release_bootstrap(coordinator_session, "mediation_moderation")
stopifnot(identical(coordinator$owner, "custom_model_canvas"))
mediation_moderation_release_bootstrap(coordinator_session, "custom_model_canvas")
stopifnot(is.null(coordinator$owner), is.null(coordinator$cancel))

selected_names <- c("X1", "X2", "M1", "M2", "Y", "W", "Z", "UnusedW", "C")
spec <- custom_model_canvas_snapshot_spec(snapshot, selected_names)

stopifnot(identical(spec$roles$y, "Y"))
stopifnot(identical(spec$roles$x, c("X1", "X2")))
stopifnot(identical(spec$roles$mediators, c("M1", "M2")))
stopifnot(identical(spec$roles$w, c("W", "Z")))
stopifnot(identical(spec$roles$covariates, "C"))
stopifnot(identical(spec$mediator_arrangement, "serial"))

stopifnot(identical(spec$x_to_m$M1, c("X1", "X2")))
stopifnot(identical(spec$x_to_m$M2, character(0)))
stopifnot(identical(spec$m_to_m$M1, character(0)))
stopifnot(identical(spec$m_to_m$M2, "M1"))
stopifnot(identical(spec$m_to_y$Y, "M2"))
stopifnot(identical(spec$direct_x_to_y$Y, "X1"))

stopifnot(identical(spec$moderated_paths, c("xm", "my", "xy")))
stopifnot(identical(spec$moderated_x_to_m$M1, "X1"))
stopifnot(identical(spec$moderated_x_to_m$M2, character(0)))
stopifnot(identical(spec$moderated_m_to_y, "M2"))

expected_map <- data.frame(
  path_type = c("xm", "my", "xy"),
  moderator = c("W", "Z", "W"),
  x = c("X1", "", "X1"),
  mediator = c("M1", "M2", ""),
  y = c("", "Y", "Y"),
  stringsAsFactors = FALSE
)
stopifnot(identical(spec$moderation_map, expected_map))

message("Checking disconnected or invalid canvas records are ignored...")
invalid_snapshot <- snapshot
invalid_snapshot$edges <- c(
  invalid_snapshot$edges,
  list(edge("e_bad_xm", "x1", "missing_node"))
)
invalid_snapshot$moderations <- c(
  invalid_snapshot$moderations,
  list(moderation("mod_bad", "unused_w", "e_bad_xm"))
)
invalid_spec <- custom_model_canvas_snapshot_spec(invalid_snapshot, selected_names)
stopifnot(identical(invalid_spec$x_to_m, spec$x_to_m))
stopifnot(identical(invalid_spec$moderation_map, spec$moderation_map))
stopifnot(identical(invalid_spec$roles$w, c("W", "Z")))

message("Checking custom model Durbin-Watson summary labels...")
dw_diagnostics <- mediation_moderation_dw_summary_value(list(
  residual_diagnostics = TRUE,
  dw_d = 1.79,
  dw_crit = list(dU = 1.89)
))
stopifnot(identical(dw_diagnostics, "1.790 (1.890~2.110)"))
dw_plain <- mediation_moderation_dw_summary_value(list(
  residual_diagnostics = FALSE,
  dw_d = 1.79,
  dw_crit = list(dU = 1.89)
))
stopifnot(identical(dw_plain, "1.790"))

message("Checking numeric-label factor responses are converted before lm...")
numeric_factor_data <- data.frame(
  Y = factor(as.character(seq_len(12))),
  X = c(1, 2, 1, 3, 2, 4, 3, 5, 4, 6, 5, 7),
  M = factor(as.character(c(2, 2, 3, 4, 4, 5, 6, 6, 7, 8, 8, 9)))
)
numeric_factor_info <- data.frame(
  name = c("Y", "X", "M"),
  var_label = c("Y", "X", "M"),
  role = c("", "", ""),
  measurement = c("continuous", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
var_factor_warning <- FALSE
numeric_factor_result <- withCallingHandlers(
  mediation_moderation_fit_focal(
    numeric_factor_data,
    roles = list(y = "Y", x = "X", mediators = "M", w = character(0), covariates = character(0)),
    focal = "X",
    structure = "parallel",
    model = "4",
    variable_info = numeric_factor_info,
    boot_r = 10L,
    residual_diagnostics = FALSE
  ),
  warning = function(w) {
    if (grepl("Calling var\\(x\\) on a factor", conditionMessage(w), fixed = FALSE)) {
      var_factor_warning <<- TRUE
    }
    invokeRestart("muffleWarning")
  }
)
stopifnot(!isTRUE(var_factor_warning))
stopifnot(is.numeric(numeric_factor_result$data$Y))
stopifnot(is.numeric(numeric_factor_result$data$M))

message("Checking categorical moderators produce conditional and relative indirect effects...")
set.seed(120)
categorical_n <- 72L
categorical_data <- data.frame(
  X = rep(seq(-1.5, 1.5, length.out = 24L), 3L),
  W = factor(rep(c("control", "low", "high"), each = 24L), levels = c("control", "low", "high"))
)
categorical_data$M <- 0.30 +
  (0.40 + ifelse(categorical_data$W == "low", 0.25, ifelse(categorical_data$W == "high", 0.55, 0))) * categorical_data$X +
  ifelse(categorical_data$W == "low", 0.10, ifelse(categorical_data$W == "high", 0.20, 0)) +
  stats::rnorm(categorical_n, sd = 0.03)
categorical_data$Y <- 0.20 + 0.70 * categorical_data$M + 0.15 * categorical_data$X + stats::rnorm(categorical_n, sd = 0.03)
categorical_info <- data.frame(
  name = c("Y", "X", "M", "W"),
  var_label = c("Y", "X", "M", "W"),
  role = c("", "", "", ""),
  measurement = c("continuous", "continuous", "continuous", "category"),
  stringsAsFactors = FALSE
)
categorical_result <- mediation_moderation_boot_effects(
  categorical_data,
  roles = list(y = "Y", x = "X", mediators = "M", w = "W", covariates = character(0)),
  focal = "X",
  structure = "single",
  model = "7",
  mean_center = FALSE,
  variable_info = categorical_info,
  boot_r = 20L,
  seed = 120L,
  residual_diagnostics = FALSE,
  moderated_paths = "xm"
)
categorical_effects <- as.character(categorical_result$effect_table$Effect)
stopifnot(any(startsWith(categorical_effects, "Conditional indirect effect")))
stopifnot(any(startsWith(categorical_effects, "Relative indirect effect")))
stopifnot(is.data.frame(categorical_result$simple_slopes_table) || is.null(categorical_result$simple_slopes_table))
categorical_simple <- mediation_moderation_simple_slopes_table(categorical_result$path_results)
stopifnot(is.data.frame(categorical_simple), nrow(categorical_simple) >= 3L)
stopifnot(any(as.character(categorical_simple$Level) == "high"))
categorical_moderation_result <- mediation_moderation_boot_effects(
  transform(categorical_data, Y = 0.10 + (0.35 + ifelse(W == "low", 0.20, ifelse(W == "high", 0.45, 0))) * X + stats::rnorm(categorical_n, sd = 0.03)),
  roles = list(y = "Y", x = "X", mediators = character(0), w = "W", covariates = character(0)),
  focal = "X",
  structure = "none",
  model = "1",
  mean_center = FALSE,
  variable_info = categorical_info[c(1, 2, 4), , drop = FALSE],
  boot_r = 20L,
  seed = 121L,
  residual_diagnostics = FALSE,
  moderated_paths = "xy"
)
categorical_moderation_simple <- mediation_moderation_simple_slopes_table(categorical_moderation_result$path_results)
stopifnot(is.data.frame(categorical_moderation_simple), nrow(categorical_moderation_simple) >= 3L)
stopifnot(!any(!nzchar(trimws(as.character(categorical_moderation_result$effect_table$Estimate %||% "")))))

message("Checking B5 portrait width metadata survives summary rows...")
width_table <- data.frame(
  Term = c("(Intercept)", "X"),
  B = c("1.00", ".20"),
  `Boot SE` = c(".10", ".03"),
  LLCI = c(".80", ".14"),
  ULCI = c("1.20", ".26"),
  `Boot p` = c("<.001", ".002"),
  check.names = FALSE
)
width_table <- mediation_moderation_combined_table_widths(width_table)
summary_width_table <- hierarchical_standard_summary_table(
  width_table,
  summary = list(f = "1.00(.002)", r2 = ".10 (.08)", dw = "1.90", normality = ".10(.200)", homogeneity = ".50(.480)"),
  model_index = 1L,
  summary_values = structure(list(list()), any_residual_diagnostics = TRUE),
  include_delta = FALSE
)
stopifnot(is.numeric(attr(summary_width_table, "compact_column_widths", exact = TRUE)))
stopifnot(identical(
  length(attr(summary_width_table, "compact_column_widths", exact = TRUE)),
  ncol(summary_width_table)
))

message("Checking Delta R-squared rows follow the value instead of the model position...")
delta_summary_values <- structure(
  list(
    list(f = "1.00(.002)", r2 = ".20 (.18)", delta = ".125 (.001)", dw = "1.90"),
    list(f = "2.00(.010)", r2 = ".30 (.28)", delta = NULL, dw = "1.80")
  ),
  delta_label = "Delta R\u00B2(p)"
)
delta_first_table <- hierarchical_standard_summary_table(
  width_table,
  summary = delta_summary_values[[1L]],
  model_index = 1L,
  summary_values = delta_summary_values,
  include_delta = TRUE
)
delta_second_table <- hierarchical_standard_summary_table(
  width_table,
  summary = delta_summary_values[[2L]],
  model_index = 2L,
  summary_values = delta_summary_values,
  include_delta = TRUE
)
stopifnot(
  sum(as.character(delta_first_table$Term) == "Delta R\u00B2(p)") == 1L,
  identical(
    as.character(delta_first_table$B[as.character(delta_first_table$Term) == "Delta R\u00B2(p)"][[1L]]),
    ".125 (.001)"
  ),
  !any(as.character(delta_second_table$Term) == "Delta R\u00B2(p)")
)

delta_wide_html <- htmltools::renderTags(hierarchical_coefficient_html_table(
  list(width_table, width_table),
  list("Model 1", "Model 2"),
  delta_summary_values,
  include_delta = TRUE,
  output_table_style = "wide"
))$html
stopifnot(
  grepl("Delta R\u00B2(p)", delta_wide_html, fixed = TRUE),
  grepl(".125 (.001)", delta_wide_html, fixed = TRUE)
)
no_delta_values <- delta_summary_values
no_delta_values[[1L]]$delta <- ""
no_delta_html <- htmltools::renderTags(hierarchical_coefficient_html_table(
  list(width_table, width_table),
  list("Model 1", "Model 2"),
  no_delta_values,
  include_delta = TRUE,
  output_table_style = "wide"
))$html
stopifnot(!grepl("Delta R\u00B2(p)", no_delta_html, fixed = TRUE))

message("Checking the combined-path Y model keeps its hierarchical Delta R-squared value...")
set.seed(20260822)
combined_n <- 120L
combined_data <- data.frame(
  X = stats::rnorm(combined_n),
  W = stats::rnorm(combined_n)
)
combined_data$M <- .4 * combined_data$X + .2 * combined_data$W +
  .5 * combined_data$X * combined_data$W + stats::rnorm(combined_n, sd = .7)
combined_data$Y <- .2 * combined_data$X + .6 * combined_data$M + .15 * combined_data$W +
  .8 * combined_data$M * combined_data$W + stats::rnorm(combined_n, sd = .8)
combined_info <- data.frame(
  name = names(combined_data),
  var_label = names(combined_data),
  role = "",
  measurement = "continuous",
  stringsAsFactors = FALSE
)
combined_path_result <- function(model, equation, mediators = character(0)) {
  mediation_moderation_path_result(
    model,
    focal = "X",
    equation = equation,
    w = "W",
    mediators = mediators,
    variable_info = combined_info,
    analysis_method = "process_ols",
    residual_diagnostics = FALSE,
    auto_method = FALSE,
    all_x = "X",
    show_f2 = FALSE
  )
}
m_base_result <- combined_path_result(stats::lm(M ~ X + W, data = combined_data), "M model: M")
m_full_result <- combined_path_result(stats::lm(M ~ X * W, data = combined_data), "M model: M")
m_full_result$hierarchical_base <- m_base_result
y_base_result <- combined_path_result(stats::lm(Y ~ X + M + W, data = combined_data), "Y model", "M")
y_full_result <- combined_path_result(stats::lm(Y ~ X + M * W, data = combined_data), "Y model", "M")
y_full_result$hierarchical_base <- y_base_result
expected_y_delta <- hierarchical_delta_line(y_base_result, y_full_result)
stopifnot(hierarchical_summary_value_available(expected_y_delta))
combined_path_html <- htmltools::renderTags(mediation_moderation_combined_path_table_ui(
  list(m_full_result, y_full_result),
  result = list(roles = list(mediators = "M")),
  output_table_style = "standard"
))$html
stopifnot(
  grepl("Model 2", combined_path_html, fixed = TRUE),
  grepl(as.character(htmltools::htmlEscape(expected_y_delta)), combined_path_html, fixed = TRUE)
)
combined_path_wide_html <- htmltools::renderTags(mediation_moderation_combined_path_table_ui(
  list(m_full_result, y_full_result),
  result = list(roles = list(mediators = "M")),
  output_table_style = "wide"
))$html
stopifnot(grepl(as.character(htmltools::htmlEscape(expected_y_delta)), combined_path_wide_html, fixed = TRUE))
combined_no_delta_standard <- htmltools::renderTags(mediation_moderation_combined_path_table_ui(
  list(m_base_result, y_base_result),
  result = list(roles = list(mediators = "M")),
  output_table_style = "standard"
))$html
combined_no_delta_wide <- htmltools::renderTags(mediation_moderation_combined_path_table_ui(
  list(m_base_result, y_base_result),
  result = list(roles = list(mediators = "M")),
  output_table_style = "wide"
))$html
stopifnot(
  !grepl("Delta R²", combined_no_delta_standard, fixed = TRUE),
  !grepl("Delta R²", combined_no_delta_wide, fixed = TRUE)
)

message("Checking compact path table consolidates SE and p columns...")
compact_boot_table <- data.frame(
  Term = c("(Intercept)", "X"),
  B = c("1.00", ".20"),
  `Boot SE` = c(".10", ".03"),
  LLCI = c(".80", ".14"),
  ULCI = c("1.20", ".26"),
  `Boot p` = c("<.001", ".002"),
  check.names = FALSE
)
compact_hc3_table <- data.frame(
  Term = c("(Intercept)", "M"),
  B = c("1.10", ".30"),
  `HC3 SE` = c(".11", ".04"),
  t = c("10.00", "2.50"),
  p = c("<.001", ".013"),
  check.names = FALSE
)
compact_values <- structure(
  list(
    list(f = "1.00(.002)", r2 = ".10 (.08)", dw = "1.90", normality = ".10(.200)", homogeneity = ".50(.480)"),
    list(f = "2.00(.010)", r2 = ".20 (.18)", dw = "1.80", normality = ".12(.180)", homogeneity = ".60(.440)")
  ),
  any_residual_diagnostics = TRUE
)
compact_table <- hierarchical_compact_coefficient_table(
  list(compact_boot_table, compact_hc3_table),
  list("Model 1", "Model 2"),
  compact_values
)
stopifnot("SE" %in% names(compact_table))
stopifnot("p" %in% names(compact_table))
stopifnot(!any(c("HC3 SE", "Boot SE", "Boot p", "t(p)") %in% names(compact_table)))
stopifnot(is.data.frame(attr(compact_table, "note_markers", exact = TRUE)))
stopifnot(nzchar(attr(compact_table, "compact_method_notes", exact = TRUE)))
stopifnot(identical(hierarchical_compact_summary_cell("11.56 (<.001)"), "11.56\n(<.001)"))
stopifnot(identical(as.character(compact_table[["F(p)"]][[1L]]), "1.00"))
stopifnot(identical(as.character(compact_table[["F(p)"]][[2L]]), "(.002)"))
compact_xm_table <- hierarchical_compact_coefficient_table(
  list(compact_boot_table, compact_hc3_table),
  list("Model 1", "Model 2"),
  compact_values,
  output_table_style = "compact_xm"
)
stopifnot(identical(as.character(compact_xm_table[["F(p)"]][[1L]]), "1.00\n(.002)"))
compact_html <- htmltools::renderTags(coefficient_html_table(compact_xm_table, output_table_style = "compact_xm"))$html
stopifnot(grepl("coefficient-cell-break", compact_html, fixed = TRUE))

message("Checking duplicate custom model Y tables are collapsed...")
collapse_data <- data.frame(
  Y = c(1, 2, 3, 4, 5, 6),
  Y2 = c(2, 1, 4, 3, 6, 5),
  X1 = c(1, 1, 2, 2, 3, 3),
  X2 = c(2, 1, 3, 2, 4, 3),
  M = c(1, 2, 2, 3, 3, 4)
)
collapse_model <- stats::lm(Y ~ X1 + M, data = collapse_data)
collapse_model_alt <- stats::lm(Y ~ X2 + M, data = collapse_data)
collapse_model_y2 <- stats::lm(Y2 ~ X1 + M, data = collapse_data)
collapse_coef <- data.frame(
  Term = names(stats::coef(collapse_model)),
  B = unname(stats::coef(collapse_model)),
  SE = rep(0.1, length(stats::coef(collapse_model))),
  t = rep(1, length(stats::coef(collapse_model))),
  p = rep(0.1, length(stats::coef(collapse_model))),
  check.names = FALSE
)
collapsed_y <- mediation_moderation_collapse_duplicate_y_models(list(
  list(model = collapse_model, n = stats::nobs(collapse_model), coef_table = collapse_coef, focal = "X1"),
  list(model = collapse_model, n = stats::nobs(collapse_model), coef_table = collapse_coef, focal = "X2")
))
stopifnot(length(collapsed_y) == 1L)
stopifnot(identical(as.character(attr(collapsed_y[[1L]], "collapsed_focals")), c("X1", "X2")))
collapsed_y_by_outcome <- mediation_moderation_collapse_duplicate_y_models(list(
  list(model = collapse_model, n = stats::nobs(collapse_model), coef_table = collapse_coef, focal = "X1"),
  list(model = collapse_model_alt, n = stats::nobs(collapse_model_alt), coef_table = collapse_coef, focal = "X2")
))
stopifnot(length(collapsed_y_by_outcome) == 1L)
stopifnot(identical(as.character(attr(collapsed_y_by_outcome[[1L]], "collapsed_focals")), c("X1", "X2")))
collapsed_y_multiple_outcomes <- mediation_moderation_collapse_duplicate_y_models(list(
  list(model = collapse_model, n = stats::nobs(collapse_model), coef_table = collapse_coef, focal = "X1"),
  list(model = collapse_model_alt, n = stats::nobs(collapse_model_alt), coef_table = collapse_coef, focal = "X2"),
  list(model = collapse_model_y2, n = stats::nobs(collapse_model_y2), coef_table = collapse_coef, focal = "X1")
))
stopifnot(length(collapsed_y_multiple_outcomes) == 2L)

message("Checking custom Y model and diagram use only drawn canvas paths...")
drawn_snapshot <- list(
  nodes = list(
    node("x1", "X1", "independent", 80, 120),
    node("x2", "X2", "independent", 80, 220),
    node("m1", "M1", "mediator", 300, 120),
    node("m2", "M2", "mediator", 300, 220),
    node("y", "Y", "dependent", 620, 170),
    node("w", "W", "moderator", 420, 40)
  ),
  edges = list(
    edge("e_x1_m1", "x1", "m1"),
    edge("e_x2_m2", "x2", "m2"),
    edge("e_m1_y", "m1", "y"),
    edge("e_m2_y", "m2", "y")
  ),
  moderations = list(
    moderation("mod_my", "w", "e_m1_y")
  ),
  covariates = character(0)
)
drawn_spec <- custom_model_canvas_snapshot_spec(drawn_snapshot, c("X1", "X2", "M1", "M2", "Y", "W"))
drawn_data <- data.frame(
  Y = seq_len(40) / 10,
  X1 = rep(c(0, 1), 20),
  X2 = rep(c(1, 2, 3, 4), 10),
  M1 = rep(c(2, 3, 4, 5), 10),
  M2 = rep(c(1, 3, 2, 4, 5), 8),
  W = rep(c(0, 1), 20)
)
drawn_info <- data.frame(
  name = names(drawn_data),
  var_label = names(drawn_data),
  role = "",
  measurement = "continuous",
  stringsAsFactors = FALSE
)
drawn_result <- run_mediation_moderation_analysis(
  data = drawn_data,
  roles = drawn_spec$roles,
  mediator_arrangement = drawn_spec$mediator_arrangement,
  moderated_paths = drawn_spec$moderated_paths,
  boot_r = 5L,
  seed = 1L,
  simple_slopes = FALSE,
  johnson_neyman = FALSE,
  residual_diagnostics = FALSE,
  auto_method = FALSE,
  direct_x = drawn_spec$direct_x,
  direct_x_to_y = drawn_spec$direct_x_to_y,
  x_to_m = drawn_spec$x_to_m,
  m_to_y = drawn_spec$m_to_y,
  m_to_m = drawn_spec$m_to_m,
  moderated_x_to_m = drawn_spec$moderated_x_to_m,
  moderated_m_to_y = drawn_spec$moderated_m_to_y,
  moderation_map = drawn_spec$moderation_map,
  custom_path_model = TRUE,
  variable_info = drawn_info
)
drawn_y_path <- Filter(function(path_result) identical(path_result$equation, "Y model"), drawn_result$path_results)[[1L]]
stopifnot("M2" %in% as.character(drawn_y_path$coef_table$Term))
stopifnot(!"X2" %in% as.character(drawn_y_path$coef_table$Term))
drawn_result$custom_model_canvas <- TRUE
drawn_result$custom_model_canvas_snapshot <- drawn_snapshot
drawn_diagram <- mediation_moderation_result_diagram_data(drawn_result)
drawn_keys <- vapply(drawn_diagram$spec$paths, mediation_moderation_path_key, character(1))
stopifnot(all(c("x1->m1", "x2->m2", "m1->y", "m2->y", "w->my_m1") %in% drawn_keys))
stopifnot(!"x1->m2" %in% drawn_keys)
stopifnot(!"x2->y" %in% drawn_keys)

message("Checking moderated mediation index path labels...")
index_effects <- c(
  `Index of moderated mediation: X -> M1 -> Y` = -0.01,
  `Index of moderated mediation: X -> M2 -> Y` = -0.02
)
index_boot <- matrix(
  c(-0.02, -0.03, -0.01, -0.02, 0.00, -0.01),
  nrow = 3,
  dimnames = list(NULL, names(index_effects))
)
index_table <- mediation_moderation_effect_table(
  model = "custom",
  focal = "X1",
  effects = index_effects,
  boot_matrix = index_boot,
  ci_method = "percentile",
  y = "Y",
  mediators = c("M1", "M2"),
  w = "W",
  model_label = "Custom"
)
stopifnot("Path" %in% names(index_table))
stopifnot(identical(as.character(index_table$Effect), rep("Index of moderated mediation", 2L)))
stopifnot(length(unique(as.character(index_table$Path))) == 2L)
stopifnot(all(grepl("X1-->M[12]-->Y", as.character(index_table$Path))))

message("Checking custom model canvas file extension policy...")
dialogs_js <- paste(readLines(file.path(repo_root, "www", "model-canvas", "dialogs.js"), warn = FALSE), collapse = "\n")
stopifnot(grepl('timestampName\\("model-canvas", "stmodel"\\)', dialogs_js))
stopifnot(grepl('"\\.stmodel", "\\.studio", "\\.json"', dialogs_js))
stopifnot(grepl('input\\.accept = "\\.stmodel,\\.studio,\\.json,application/json"', dialogs_js))

message("All custom model canvas validations passed.")
