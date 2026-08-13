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
stopifnot(
  grepl("PLS structural model effects", ui_source, fixed = TRUE),
  grepl("PLS 구조모형 효과", ui_source, fixed = TRUE),
  grepl("total and indirect effects", ui_source, fixed = TRUE),
  grepl("path coefficients, R2, adjusted R2, f2, inner VIF", ui_source, fixed = TRUE),
  grepl("Inner VIF is reported for direct structural paths", ui_source, fixed = TRUE),
  grepl("PLS-SEM 구조모형 출력", ui_source, fixed = TRUE),
  grepl("PLS-SEM 측정모형 출력", ui_source, fixed = TRUE),
  grepl("PLS-SEM 타당도 출력", ui_source, fixed = TRUE),
  grepl("rather than covariance-based global fit indices", ui_source, fixed = TRUE),
  grepl("PLS-SEM measurement output reports outer loadings, outer weights, item VIF", ui_source, fixed = TRUE),
  grepl("Fornell-Larcker, and HTMT summaries", ui_source, fixed = TRUE),
  grepl("indirect and total effect rows are included", ui_source, fixed = TRUE),
  grepl("표준화 효과의 95% 신뢰구간", ui_source, fixed = TRUE),
  grepl("PLS-SEM does not estimate covariance paths", ui_source, fixed = TRUE),
  grepl("PLS-SEM does not estimate covariance paths; excluded:", ui_source, fixed = TRUE),
  grepl("structural_canvas_notify_ignored_pls_covariances(result, analysis_type)", ui_source, fixed = TRUE),
  grepl("PLS-SEM에서는 공분산 경로를 추정하지", ui_source, fixed = TRUE),
  !grepl("Latent covariance, factor-score, HTMT, and lavaan delta-method diagnostics are not displayed", ui_source, fixed = TRUE)
)

cbsem <- run_structural_canvas_analysis(snapshot, data, "cbsem", estimator = "ML", missing = "fiml")
stopifnot(inherits(cbsem$fit, "lavaan"))
stopifnot(isTRUE(cbsem$converged))
stopifnot(grepl("eta2 ~", cbsem$syntax, fixed = TRUE), grepl("*eta1", cbsem$syntax, fixed = TRUE))
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
stopifnot("Effect" %in% names(cbsem_structural), cbsem_structural$Effect[[1L]] == "Direct")
stopifnot(all(c("Outcome", "Predictor", "B", "beta", "R²", "z", "p") %in% names(cbsem_structural)))
stopifnot(cbsem_structural$Outcome[[1L]] == "eta2")
stopifnot(cbsem_structural$Predictor[[1L]] == "eta1")

cbsem_snapshot <- structural_canvas_result_snapshot(snapshot, cbsem$fit, "beta")
cbsem_labels <- vapply(cbsem_snapshot$edges, function(edge) as.character(edge$label %||% ""), character(1))
stopifnot(any(nzchar(cbsem_labels)))

set.seed(20260814)
n_mediation <- 240L
eta_a <- stats::rnorm(n_mediation)
eta_b <- 0.55 * eta_a + stats::rnorm(n_mediation, sd = 0.80)
eta_c <- 0.35 * eta_a + 0.50 * eta_b + stats::rnorm(n_mediation, sd = 0.75)
mediation_data <- data.frame(
  a1 = 0.82 * eta_a + stats::rnorm(n_mediation, sd = 0.42),
  a2 = 0.75 * eta_a + stats::rnorm(n_mediation, sd = 0.50),
  a3 = 0.70 * eta_a + stats::rnorm(n_mediation, sd = 0.55),
  b1 = 0.80 * eta_b + stats::rnorm(n_mediation, sd = 0.45),
  b2 = 0.73 * eta_b + stats::rnorm(n_mediation, sd = 0.52),
  b3 = 0.68 * eta_b + stats::rnorm(n_mediation, sd = 0.58),
  c1 = 0.84 * eta_c + stats::rnorm(n_mediation, sd = 0.40),
  c2 = 0.77 * eta_c + stats::rnorm(n_mediation, sd = 0.48),
  c3 = 0.71 * eta_c + stats::rnorm(n_mediation, sd = 0.54)
)
mediation_snapshot <- list(
  nodes = c(
    list(
      list(id = "eta_a", role = "latent", name = "etaA", canvasLabel = "etaA", x = 100, y = 100),
      list(id = "eta_b", role = "latent", name = "etaB", canvasLabel = "etaB", x = 360, y = 100),
      list(id = "eta_c", role = "latent", name = "etaC", canvasLabel = "etaC", x = 620, y = 100)
    ),
    lapply(seq_len(3L), function(index) list(id = paste0("a", index), role = "indicator", name = paste0("a", index), variableId = paste0("a", index), canvasLabel = paste0("a", index), x = 100, y = 210 + index * 60)),
    lapply(seq_len(3L), function(index) list(id = paste0("b", index), role = "indicator", name = paste0("b", index), variableId = paste0("b", index), canvasLabel = paste0("b", index), x = 360, y = 210 + index * 60)),
    lapply(seq_len(3L), function(index) list(id = paste0("c", index), role = "indicator", name = paste0("c", index), variableId = paste0("c", index), canvasLabel = paste0("c", index), x = 620, y = 210 + index * 60))
  ),
  edges = c(
    lapply(seq_len(3L), function(index) list(id = paste0("ea", index), from = "eta_a", to = paste0("a", index))),
    lapply(seq_len(3L), function(index) list(id = paste0("eb", index), from = "eta_b", to = paste0("b", index))),
    lapply(seq_len(3L), function(index) list(id = paste0("ec", index), from = "eta_c", to = paste0("c", index))),
    list(
      list(id = "ab", from = "eta_a", to = "eta_b"),
      list(id = "bc", from = "eta_b", to = "eta_c"),
      list(id = "ac", from = "eta_a", to = "eta_c")
    )
  )
)
cbsem_mediation <- run_structural_canvas_analysis(mediation_snapshot, mediation_data, "cbsem", estimator = "ML", missing = "fiml")
stopifnot(isTRUE(cbsem_mediation$converged))
stopifnot(grepl(":=", cbsem_mediation$syntax, fixed = TRUE))
stopifnot(length(cbsem_mediation$effect_definitions) == 2L)
cbsem_mediation_bundle <- list(
  fit = cbsem_mediation$fit,
  syntax = cbsem_mediation$syntax,
  snapshot = mediation_snapshot,
  diagnostics = cbsem_mediation,
  estimator = "ML",
  rmsea_ci = 0.90,
  validity_formula = "standardized"
)
cbsem_mediation_result <- function() cbsem_mediation_bundle
cbsem_mediation_structural <- structural_canvas_result_table("structural", cbsem_mediation_result, "cbsem", labels_fn, language_fn)
stopifnot(all(c("Direct", "Indirect", "Total") %in% cbsem_mediation_structural$Effect))
stopifnot(sum(cbsem_mediation_structural$Effect == "Direct") == 3L)
stopifnot(any(cbsem_mediation_structural$Effect == "Indirect" & cbsem_mediation_structural$Outcome == "etaC" & cbsem_mediation_structural$Predictor == "etaA"))
stopifnot(any(cbsem_mediation_structural$Effect == "Total" & cbsem_mediation_structural$Outcome == "etaC" & cbsem_mediation_structural$Predictor == "etaA"))
stopifnot(all(nzchar(cbsem_mediation_structural$B[cbsem_mediation_structural$Effect %in% c("Indirect", "Total")])))
stopifnot(all(nzchar(cbsem_mediation_structural$beta[cbsem_mediation_structural$Effect %in% c("Direct", "Indirect", "Total")])))
stopifnot(all(nzchar(cbsem_mediation_structural$p[cbsem_mediation_structural$Effect %in% c("Direct", "Indirect", "Total")])))
stopifnot(any(nzchar(cbsem_mediation_structural[["R²"]][cbsem_mediation_structural$Effect == "Direct" & cbsem_mediation_structural$Outcome == "etaC"])))
stopifnot(any(nzchar(cbsem_mediation_structural[["beta 95% CI lower"]][cbsem_mediation_structural$Effect == "Direct"])))
stopifnot(all(nzchar(cbsem_mediation_structural[["beta 95% CI lower"]][cbsem_mediation_structural$Effect %in% c("Indirect", "Total")])))
stopifnot(all(nzchar(cbsem_mediation_structural[["beta 95% CI upper"]][cbsem_mediation_structural$Effect %in% c("Indirect", "Total")])))

parallel_snapshot <- mediation_snapshot
parallel_snapshot$edges <- c(
  parallel_snapshot$edges[seq_len(9L)],
  list(
    list(id = "ac", from = "eta_a", to = "eta_c"),
    list(id = "bc", from = "eta_b", to = "eta_c"),
    list(id = "cov_ab", from = "eta_a", to = "eta_b", kind = "covariance")
  )
)
cbsem_parallel <- run_structural_canvas_analysis(parallel_snapshot, mediation_data, "cbsem", estimator = "ML", missing = "fiml")
stopifnot(isTRUE(cbsem_parallel$converged))
stopifnot(grepl("etaA ~~ etaB", cbsem_parallel$syntax, fixed = TRUE))
stopifnot(!length(structural_canvas_missing_exogenous_covariances(parallel_snapshot)))
cbsem_parallel_bundle <- list(
  fit = cbsem_parallel$fit,
  syntax = cbsem_parallel$syntax,
  snapshot = parallel_snapshot,
  diagnostics = cbsem_parallel,
  estimator = "ML",
  rmsea_ci = 0.90,
  validity_formula = "standardized"
)
cbsem_parallel_result <- function() cbsem_parallel_bundle
cbsem_parallel_structural <- structural_canvas_result_table("structural", cbsem_parallel_result, "cbsem", labels_fn, language_fn)
stopifnot(sum(cbsem_parallel_structural$Effect == "Direct") == 2L)
stopifnot(all(c("etaA", "etaB") %in% cbsem_parallel_structural$Predictor[cbsem_parallel_structural$Outcome == "etaC"]))

cycle_snapshot <- snapshot
cycle_snapshot$edges <- c(cycle_snapshot$edges, list(list(id = "p2", from = "lv2", to = "lv1")))
cycle_syntax <- structural_canvas_lavaan_syntax(
  cycle_snapshot, data, "cbsem",
  Filter(function(node) identical(node$role, "latent"), cycle_snapshot$nodes),
  cycle_snapshot$edges,
  character(0)
)
stopifnot(!grepl(":=", cycle_syntax$syntax, fixed = TRUE), !length(cycle_syntax$effect_definitions))

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
stopifnot(all(c("Effect", "Outcome", "Predictor", "Coefficient", "R2", "AdjR2", "f2", "Inner VIF", "Indirect effect", "Indirect effect CI lower", "Indirect effect CI upper", "Indirect effect p", "Total effect") %in% names(pls_fit)))
stopifnot(pls_fit$Effect[[1L]] == "Direct")
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

pls_covariance_snapshot <- snapshot
pls_covariance_snapshot$edges <- c(pls_covariance_snapshot$edges, list(list(id = "cov12", from = "lv1", to = "lv2", kind = "covariance")))
pls_covariance <- run_structural_canvas_analysis(pls_covariance_snapshot, data, "plssem", estimator = "PLS")
stopifnot(inherits(pls_covariance$fit, "pls_model"))
stopifnot(!grepl("~~", pls_covariance$syntax, fixed = TRUE))
stopifnot(identical(pls_covariance$ignored_covariances, "eta1 ~~ eta2"))
pls_covariance_bundle <- list(
  fit = pls_covariance$fit,
  syntax = pls_covariance$syntax,
  snapshot = pls_covariance_snapshot,
  diagnostics = pls_covariance,
  estimator = "PLS"
)
pls_covariance_result <- function() pls_covariance_bundle
pls_covariance_overview <- structural_canvas_result_table("overview", pls_covariance_result, "plssem", labels_fn, language_fn)
stopifnot(nrow(pls_covariance_overview) == 8L)
stopifnot(pls_covariance_overview$Value[pls_covariance_overview$Item == "Ignored covariance paths"] == "eta1 ~~ eta2")

pls_mediation <- run_structural_canvas_analysis(mediation_snapshot, mediation_data, "plssem", estimator = "PLS")
stopifnot(inherits(pls_mediation$fit, "pls_model"))
pls_mediation_bundle <- list(
  fit = pls_mediation$fit,
  syntax = pls_mediation$syntax,
  snapshot = mediation_snapshot,
  diagnostics = pls_mediation,
  estimator = "PLS"
)
pls_mediation_result <- function() pls_mediation_bundle
pls_mediation_fit <- structural_canvas_result_table("fit", pls_mediation_result, "plssem", labels_fn, language_fn)
stopifnot(any(pls_mediation_fit$Effect == "Indirect" & pls_mediation_fit$Outcome == "etaC" & pls_mediation_fit$Predictor == "etaA"))
stopifnot(any(nzchar(pls_mediation_fit[["Indirect effect"]][pls_mediation_fit$Effect == "Indirect"])))
stopifnot(any(nzchar(pls_mediation_fit[["Total effect"]][pls_mediation_fit$Effect == "Indirect"])))
stopifnot(any(nzchar(pls_mediation_fit[["Inner VIF"]][pls_mediation_fit$Effect == "Direct" & pls_mediation_fit$Outcome == "etaC"])))

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
pls_mediation_bootstrap <- structural_canvas_run_pls_bootstrap("plssem", 20L, pls_mediation, 13579L)
stopifnot("bootstrapped_total_indirect_paths" %in% names(pls_mediation_bootstrap))
pls_boot_bundle <- pls_bundle
pls_boot_bundle$pls_bootstrap <- 30L
pls_boot_bundle$pls_bootstrap_result <- pls_bootstrap
pls_boot_result <- function() pls_boot_bundle
pls_boot_fit <- structural_canvas_result_table("fit", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_validity <- structural_canvas_result_table("validity", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_measurement <- structural_canvas_result_table("measurement", pls_boot_result, "plssem", labels_fn, language_fn)
stopifnot(all(c("Indirect effect CI lower", "Indirect effect CI upper", "Indirect effect p", "Total effect CI lower", "Total effect CI upper", "Total effect p", "Boot CI lower", "Boot CI upper", "Boot p") %in% names(pls_boot_fit)))
stopifnot(any(nzchar(pls_boot_fit[["Total effect p"]])))
stopifnot(any(nzchar(pls_boot_fit[["Boot p"]])))
pls_mediation_boot_bundle <- pls_mediation_bundle
pls_mediation_boot_bundle$pls_bootstrap_result <- pls_mediation_bootstrap
pls_mediation_boot_result <- function() pls_mediation_boot_bundle
pls_mediation_boot_fit <- structural_canvas_result_table("fit", pls_mediation_boot_result, "plssem", labels_fn, language_fn)
pls_mediation_indirect_row <- pls_mediation_boot_fit[pls_mediation_boot_fit$Effect == "Indirect" & pls_mediation_boot_fit$Outcome == "etaC" & pls_mediation_boot_fit$Predictor == "etaA", , drop = FALSE]
stopifnot(nrow(pls_mediation_indirect_row) == 1L)
stopifnot(nzchar(pls_mediation_indirect_row[["Indirect effect CI lower"]][[1L]]), nzchar(pls_mediation_indirect_row[["Indirect effect CI upper"]][[1L]]), nzchar(pls_mediation_indirect_row[["Indirect effect p"]][[1L]]))
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
execution_state$value <- NULL
execution_state$message <- NULL
executed_covariance <- structural_canvas_execute_analysis(
  pls_covariance_snapshot,
  settings = list(estimator = "PLS"),
  input = list(),
  session = session,
  dataset_fn = function() data,
  variable_table_fn = function() variable_table,
  analysis_type = "plssem",
  prefix = "structural_plssem",
  fit_result = fit_result_state
)
stopifnot(inherits(executed_covariance$fit, "pls_model"))
stopifnot(identical(fit_result_state()$diagnostics$ignored_covariances, "eta1 ~~ eta2"))
stopifnot(identical(execution_state$message$type, "custom-model-canvas-result"))

formative_snapshot <- snapshot
formative_snapshot$nodes[[1]]$measurementMode <- "formative"
pls_formative <- run_structural_canvas_analysis(formative_snapshot, data, "plssem", estimator = "PLS")
stopifnot(inherits(pls_formative$fit, "pls_model"))
stopifnot(grepl("eta1 <~", pls_formative$syntax, fixed = TRUE))
pls_formative_bundle <- list(
  fit = pls_formative$fit,
  syntax = pls_formative$syntax,
  snapshot = formative_snapshot,
  diagnostics = pls_formative,
  estimator = "PLS"
)
pls_formative_result <- function() pls_formative_bundle
pls_formative_measurement <- structural_canvas_result_table("measurement", pls_formative_result, "plssem", labels_fn, language_fn)
stopifnot(any(pls_formative_measurement$Construct == "eta1" & pls_formative_measurement$Mode == "Formative"))
stopifnot(any(pls_formative_measurement$Construct == "eta2" & pls_formative_measurement$Mode == "Reflective"))
stopifnot(any(nzchar(pls_formative_measurement$Weight[pls_formative_measurement$Mode == "Formative"])))
stopifnot(any(nzchar(pls_formative_measurement[["Item VIF"]][pls_formative_measurement$Mode == "Formative"])))

cat("SEM canvas validations passed.\n")
