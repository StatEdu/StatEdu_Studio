source(file.path("R", "utils.R"), encoding = "UTF-8")
library(shiny)
source(file.path("R", "setup_custom_model_canvas_ui.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))

latent <- list(id = "latent_1", role = "latent", name = "eta1", x = 200, y = 200)
indicators <- lapply(seq_len(3L), function(index) {
  list(id = paste0("indicator_", index), role = "indicator", name = paste0("x", index), x = 400, y = index * 100)
})
snapshot <- list(
  nodes = c(list(latent), indicators),
  edges = lapply(seq_len(3L), function(index) list(from = "latent_1", to = paste0("indicator_", index)))
)

set.seed(20260812)
n <- 400L
factor_score <- stats::rnorm(n)
continuous <- data.frame(
  x1 = .8 * factor_score + stats::rnorm(n, sd = .6),
  x2 = .7 * factor_score + stats::rnorm(n, sd = .7),
  x3 = .9 * factor_score + stats::rnorm(n, sd = .5)
)

ml <- run_structural_canvas_analysis(snapshot, continuous, "cfa")
stopifnot(isTRUE(ml$converged))
correlations <- as.matrix(lavaan::lavInspect(ml$fit, "cor.lv"))
stopifnot(identical(dim(correlations), c(1L, 1L)), identical(rownames(correlations), "eta1"))

variable_table <- data.frame(
  name = c("x1", "x2", "x3"),
  measurement = c("ordered", "ordinal", "continuous"),
  stringsAsFactors = FALSE
)
ordered_names <- structural_canvas_ordered_indicators(snapshot, variable_table)
stopifnot(identical(ordered_names, c("x1", "x2")))

ordinal <- as.data.frame(lapply(continuous, function(value) {
  as.integer(cut(value, breaks = stats::quantile(value, probs = seq(0, 1, .2)), include.lowest = TRUE))
}))
wlsmv <- run_structural_canvas_analysis(snapshot, ordinal, "cfa", estimator = "WLSMV", missing = "pairwise", ordered = ordered_names)
stopifnot(isTRUE(wlsmv$converged))
stopifnot(identical(sort(lavaan::lavNames(wlsmv$fit, "ov.ord")), sort(ordered_names)))

missing_ordered_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, ordinal, "cfa", estimator = "WLSMV", missing = "pairwise")
  ""
}, error = conditionMessage)
stopifnot(grepl("requires at least one", missing_ordered_error, fixed = TRUE))

cfa_toolbar <- htmltools::renderTags(structural_equation_toolbar("cfa", "en"))$html
stopifnot(!grepl("structural-covariate-toolbar-button", cfa_toolbar, fixed = TRUE))
stopifnot(!grepl("data-action=\"structuralCovariateTargets\"", cfa_toolbar, fixed = TRUE))

cat("CFA canvas validations passed.\n")
