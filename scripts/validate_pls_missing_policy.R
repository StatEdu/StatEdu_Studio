`%||%` <- function(x, y) if (is.null(x)) y else x

source(
  file.path("R", "setup_custom_model_canvas_structural_distribution_diagnostics.R"),
  local = TRUE,
  encoding = "UTF-8"
)

data <- data.frame(
  x1 = c(1, NA, 3, 5),
  x2 = c(NA, 4, 6, 8),
  unused = c(NA, NA, 1, 1)
)
diagnostics <- structural_canvas_pls_missing_diagnostics(data, c("x1", "x2"))
replacement <- stats::setNames(
  diagnostics$replacement_values[["Replacement mean"]],
  diagnostics$replacement_values$Variable
)

stopifnot(
  identical(diagnostics$method, "mean_replacement"),
  identical(diagnostics$effective_n, 4L),
  identical(diagnostics$excluded_n, 0L),
  identical(diagnostics$complete_n, 2L),
  identical(diagnostics$imputed_row_n, 2L),
  identical(diagnostics$imputed_cell_n, 2L),
  identical(diagnostics$total_indicator_cells, 8L),
  isTRUE(all.equal(diagnostics$missing_cell_percent, 25)),
  isTRUE(all.equal(unname(replacement[c("x1", "x2")]), c(3, 6))),
  grepl("recomputes", diagnostics$policy$bootstrap, fixed = TRUE)
)

if (!requireNamespace("seminr", quietly = TRUE)) {
  stop("The PLS missing-data policy validation requires the bundled seminr package.")
}
seminr_data <- data.frame(
  x1 = c(1, NA, 3, 5, 7), x2 = c(2, 4, NA, 8, 10),
  y1 = c(1, 2, 3, 4, 5), y2 = c(2, 3, 4, 5, 6)
)
measurement_model <- seminr::constructs(
  seminr::composite("X", seminr::multi_items("x", 1:2)),
  seminr::composite("Y", seminr::multi_items("y", 1:2))
)
structural_model <- seminr::relationships(seminr::paths(from = "X", to = "Y"))
fit_main <- suppressWarnings(seminr::estimate_pls(
  data = seminr_data,
  measurement_model = measurement_model,
  structural_model = structural_model,
  missing = seminr::mean_replacement,
  assess_syntax = FALSE
))
bootstrap_indices <- c(1L, 1L, 2L, 4L, 5L)
fit_resample <- suppressWarnings(seminr::estimate_pls(
  data = seminr_data[bootstrap_indices, , drop = FALSE],
  measurement_model = measurement_model,
  structural_model = structural_model,
  missing = seminr::mean_replacement,
  assess_syntax = FALSE
))
stopifnot(
  is.na(fit_main$rawdata$x1[[2L]]),
  isTRUE(all.equal(fit_main$data$x1[[2L]], mean(seminr_data$x1, na.rm = TRUE))),
  isTRUE(all.equal(fit_main$data$x2[[3L]], mean(seminr_data$x2, na.rm = TRUE))),
  isTRUE(all.equal(
    fit_resample$data$x1[[3L]],
    mean(seminr_data$x1[bootstrap_indices], na.rm = TRUE)
  )),
  !isTRUE(all.equal(fit_resample$data$x1[[3L]], fit_main$data$x1[[2L]]))
)

review <- structural_canvas_missing_sensitivity_rows(list(
  missing = "mean_replacement",
  missing_diagnostics = diagnostics
))
complete <- structural_canvas_missing_sensitivity_rows(list(
  missing = "mean_replacement",
  missing_diagnostics = structural_canvas_pls_missing_diagnostics(
    data.frame(x1 = 1:4, x2 = 5:8), c("x1", "x2")
  )
))
stopifnot(
  identical(review$Status, "Review"),
  grepl("mean replacement", review$Guidance, ignore.case = TRUE),
  grepl("Not required", complete$Status, fixed = TRUE)
)

read_source <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
engine_source <- read_source(file.path("R", "setup_custom_model_canvas_structural_pls_engine.R"))
execute_source <- read_source(file.path("R", "setup_custom_model_canvas_structural_execute.R"))
settings_source <- read_source(file.path("R", "setup_custom_model_canvas_structural_execute_settings.R"))
render_source <- read_source(file.path("R", "setup_custom_model_canvas_structural_render.R"))
options_source <- read_source(file.path("R", "setup_custom_model_canvas_structural_options.R"))
audit_source <- read_source(file.path("R", "setup_custom_model_canvas_export_report.R"))
ko_docs <- read_source(file.path("docs", "ANALYSIS_METHODS_KO.md"))
en_docs <- read_source(file.path("docs", "ANALYSIS_METHODS_EN.md"))

explicit_calls <- gregexpr("missing = seminr::mean_replacement", engine_source, fixed = TRUE)[[1L]]
explicit_call_n <- if (identical(explicit_calls[[1L]], -1L)) 0L else length(explicit_calls)
stopifnot(
  explicit_call_n >= 2L,
  grepl('if (identical(analysis_type, "plssem")) missing <- "mean_replacement"', execute_source, fixed = TRUE),
  grepl('identical(prefix, "structural_plssem")', settings_source, fixed = TRUE),
  grepl("structural_canvas_pls_missing_diagnostics", execute_source, fixed = TRUE),
  grepl("Indicator mean replacement (seminr-compatible)", render_source, fixed = TRUE),
  !grepl("Not applicable to the current complete-row PLS workflow", render_source, fixed = TRUE),
  grepl("bootstrap resample recomputes", options_source, fixed = TRUE),
  grepl("missing_data_policy", audit_source, fixed = TRUE),
  grepl("nonconvergence_failures", audit_source, fixed = TRUE),
  grepl("inadmissible_failures", audit_source, fixed = TRUE),
  grepl("seminr::mean_replacement", ko_docs, fixed = TRUE),
  grepl("PLS/PLSc Missing-Data Handling", en_docs, fixed = TRUE),
  grepl("Bootstrap defaults to 5,000 resamples", en_docs, fixed = TRUE),
  !grepl("Bootstrap is off by default", en_docs, fixed = TRUE)
)

message("PLS missing-data policy validation passed.")
