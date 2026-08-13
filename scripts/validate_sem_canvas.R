source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

if (!requireNamespace("lavaan", quietly = TRUE)) {
  stop("lavaan is required for CB-SEM validation.")
}
if (!requireNamespace("seminr", quietly = TRUE)) {
  stop("seminr is required for PLS-SEM validation.")
}

set.seed(20260813)
n <- 180L
eta1 <- stats::rnorm(n)
eta2 <- 0.62 * eta1 + stats::rnorm(n, sd = 0.78)
data <- data.frame(
  x1 = 0.78 * eta1 + stats::rnorm(n, sd = 0.45),
  x2 = 0.72 * eta1 + stats::rnorm(n, sd = 0.50),
  x3 = 0.69 * eta1 + stats::rnorm(n, sd = 0.52),
  y1 = 0.82 * eta2 + stats::rnorm(n, sd = 0.42),
  y2 = 0.76 * eta2 + stats::rnorm(n, sd = 0.48),
  y3 = 0.70 * eta2 + stats::rnorm(n, sd = 0.55)
)

snapshot <- list(
  nodes = list(
    list(id = "lv1", role = "latent", name = "eta1", canvasLabel = "eta1", x = 120, y = 120, measurementMode = "reflective"),
    list(id = "lv2", role = "latent", name = "eta2", canvasLabel = "eta2", x = 420, y = 120, measurementMode = "reflective"),
    list(id = "x1", role = "indicator", name = "x1", variableId = "x1", canvasLabel = "x1", x = 120, y = 260),
    list(id = "x2", role = "indicator", name = "x2", variableId = "x2", canvasLabel = "x2", x = 120, y = 340),
    list(id = "x3", role = "indicator", name = "x3", variableId = "x3", canvasLabel = "x3", x = 120, y = 420),
    list(id = "y1", role = "indicator", name = "y1", variableId = "y1", canvasLabel = "y1", x = 420, y = 260),
    list(id = "y2", role = "indicator", name = "y2", variableId = "y2", canvasLabel = "y2", x = 420, y = 340),
    list(id = "y3", role = "indicator", name = "y3", variableId = "y3", canvasLabel = "y3", x = 420, y = 420)
  ),
  edges = list(
    list(id = "e1", from = "lv1", to = "x1"),
    list(id = "e2", from = "lv1", to = "x2"),
    list(id = "e3", from = "lv1", to = "x3"),
    list(id = "e4", from = "lv2", to = "y1"),
    list(id = "e5", from = "lv2", to = "y2"),
    list(id = "e6", from = "lv2", to = "y3"),
    list(id = "p1", from = "lv1", to = "lv2")
  )
)

labels_fn <- function() character(0)
language_fn <- function() "en"

cbsem <- run_structural_canvas_analysis(snapshot, data, "cbsem", estimator = "ML", missing = "fiml")
stopifnot(inherits(cbsem$fit, "lavaan"))
stopifnot(isTRUE(cbsem$converged))
stopifnot(grepl("eta2 ~ eta1", cbsem$syntax, fixed = TRUE))
stopifnot(is.finite(cbsem$df))

cbsem_bundle <- list(
  fit = cbsem$fit,
  syntax = cbsem$syntax,
  snapshot = snapshot,
  diagnostics = cbsem,
  estimator = "ML",
  rmsea_ci = 0.90,
  validity_formula = "standardized"
)
cbsem_result <- function() cbsem_bundle
stopifnot(nrow(structural_canvas_result_table("overview", cbsem_result, "cbsem", labels_fn, language_fn)) > 0L)
stopifnot(nrow(structural_canvas_result_table("fit", cbsem_result, "cbsem", labels_fn, language_fn)) > 0L)
stopifnot(nrow(structural_canvas_result_table("validity", cbsem_result, "cbsem", labels_fn, language_fn)) > 0L)
stopifnot(nrow(structural_canvas_result_table("measurement", cbsem_result, "cbsem", labels_fn, language_fn)) > 0L)
cbsem_structural <- structural_canvas_result_table("structural", cbsem_result, "cbsem", labels_fn, language_fn)
stopifnot(nrow(cbsem_structural) == 1L)
stopifnot(all(c("Outcome", "Predictor", "B", "beta", "R²", "z", "p") %in% names(cbsem_structural)))
stopifnot(cbsem_structural$Outcome[[1L]] == "eta2")
stopifnot(cbsem_structural$Predictor[[1L]] == "eta1")

cbsem_snapshot <- structural_canvas_result_snapshot(snapshot, cbsem$fit, "beta")
cbsem_labels <- vapply(cbsem_snapshot$edges, function(edge) as.character(edge$label %||% ""), character(1))
stopifnot(any(nzchar(cbsem_labels)))

pls <- run_structural_canvas_analysis(snapshot, data, "plssem", estimator = "PLS")
stopifnot(inherits(pls$fit, "pls_model"))
stopifnot(isTRUE(pls$converged))
stopifnot(grepl("eta2 ~ eta1", pls$syntax, fixed = TRUE))
stopifnot(length(pls$constructs) == 2L)
stopifnot(length(pls$observed) == 6L)

pls_bundle <- list(
  fit = pls$fit,
  syntax = pls$syntax,
  snapshot = snapshot,
  diagnostics = pls,
  estimator = "PLS"
)
pls_result <- function() pls_bundle
pls_overview <- structural_canvas_result_table("overview", pls_result, "plssem", labels_fn, language_fn)
pls_fit <- structural_canvas_result_table("fit", pls_result, "plssem", labels_fn, language_fn)
pls_validity <- structural_canvas_result_table("validity", pls_result, "plssem", labels_fn, language_fn)
pls_measurement <- structural_canvas_result_table("measurement", pls_result, "plssem", labels_fn, language_fn)
pls_mi <- structural_canvas_result_table("mi", pls_result, "plssem", labels_fn, language_fn)
stopifnot(nrow(pls_overview) == 7L)
stopifnot("Item" %in% names(pls_overview))
stopifnot(all(c("Outcome", "Predictor", "Coefficient", "R2", "AdjR2", "f2", "Total effect") %in% names(pls_fit)))
stopifnot(any(pls_fit$Predictor == "eta1"))
stopifnot(any(pls_fit$Outcome == "eta2"))
stopifnot(nzchar(pls_fit$f2[[1L]]))
stopifnot(nzchar(pls_fit[["Total effect"]][[1L]]))
stopifnot(all(c("Construct", "alpha", "rhoA", "rhoC", "AVE", "sqrt(AVE)", "Max HTMT", "Fornell-Larcker") %in% names(pls_validity)))
stopifnot(nrow(pls_validity) >= 2L)
stopifnot(all(c("Construct", "Indicator", "Loading", "Weight", "Item VIF", "Max cross-loading", "Mode") %in% names(pls_measurement)))
stopifnot(nrow(pls_measurement) >= 6L)
stopifnot(any(nzchar(pls_measurement[["Item VIF"]])))
stopifnot(nrow(pls_mi) == 0L)

pls_snapshot <- structural_canvas_result_snapshot(snapshot, pls$fit, "beta")
pls_labels <- vapply(pls_snapshot$edges, function(edge) as.character(edge$label %||% ""), character(1))
stopifnot(any(nzchar(pls_labels)))

pls_options <- structural_canvas_execute_settings(
  settings = list(pls_bootstrap = 500L, pls_seed = 13579L),
  input = list(),
  prefix = "structural_plssem"
)
stopifnot(pls_options$pls_bootstrap == 500L)
stopifnot(pls_options$pls_seed == 13579L)

pls_bootstrap <- structural_canvas_run_pls_bootstrap("plssem", 30L, pls, 24680L)
stopifnot(is.list(pls_bootstrap))
stopifnot(length(pls_bootstrap$bootstrapped_paths) > 0L)
stopifnot(length(pls_bootstrap$bootstrapped_loadings) > 0L)
stopifnot(length(pls_bootstrap$bootstrapped_weights) > 0L)
pls_boot_bundle <- pls_bundle
pls_boot_bundle$pls_bootstrap <- 30L
pls_boot_bundle$pls_bootstrap_result <- pls_bootstrap
pls_boot_result <- function() pls_boot_bundle
pls_boot_fit <- structural_canvas_result_table("fit", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_validity <- structural_canvas_result_table("validity", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_measurement <- structural_canvas_result_table("measurement", pls_boot_result, "plssem", labels_fn, language_fn)
stopifnot(all(c("Total effect CI lower", "Total effect CI upper", "Total effect p", "Boot CI lower", "Boot CI upper", "Boot p") %in% names(pls_boot_fit)))
stopifnot(any(nzchar(pls_boot_fit[["Total effect p"]])))
stopifnot(any(nzchar(pls_boot_fit[["Boot p"]])))
stopifnot(all(c("Max HTMT CI lower", "Max HTMT CI upper", "Max HTMT p") %in% names(pls_boot_validity)))
stopifnot(any(nzchar(pls_boot_validity[["Max HTMT p"]])))
stopifnot(all(c("Loading CI lower", "Loading CI upper", "Loading p", "Weight CI lower", "Weight CI upper", "Weight p") %in% names(pls_boot_measurement)))
stopifnot(any(nzchar(pls_boot_measurement[["Loading p"]])))

execution_state <- new.env(parent = emptyenv())
execution_state$value <- NULL
execution_state$message <- NULL
fit_result_state <- function(value) {
  if (missing(value)) return(execution_state$value)
  execution_state$value <- value
}
session <- list(sendCustomMessage = function(type, message) {
  execution_state$message <- list(type = type, message = message)
})
variable_table <- data.frame(name = names(data), measurement = "scale", stringsAsFactors = FALSE)
executed <- structural_canvas_execute_analysis(
  snapshot,
  settings = list(estimator = "PLS"),
  input = list(),
  session = session,
  dataset_fn = function() data,
  variable_table_fn = function() variable_table,
  analysis_type = "plssem",
  prefix = "structural_plssem",
  fit_result = fit_result_state
)
stopifnot(inherits(executed$fit, "pls_model"))
stopifnot(inherits(fit_result_state()$fit, "pls_model"))
stopifnot(identical(execution_state$message$type, "custom-model-canvas-result"))
stopifnot(any(nzchar(vapply(execution_state$message$message$result$edges, function(edge) as.character(edge$label %||% ""), character(1)))))

formative_snapshot <- snapshot
formative_snapshot$nodes[[1]]$measurementMode <- "formative"
pls_formative <- run_structural_canvas_analysis(formative_snapshot, data, "plssem", estimator = "PLS")
stopifnot(inherits(pls_formative$fit, "pls_model"))
stopifnot(grepl("eta1 <~", pls_formative$syntax, fixed = TRUE))

cat("SEM canvas validations passed.\n")
