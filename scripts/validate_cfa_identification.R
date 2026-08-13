source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

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

loading_table <- lavaan::parameterTable(ml$fit)
loading_table <- loading_table[loading_table$op == "=~", , drop = FALSE]
fixed_value <- stats::var(continuous$x1) * .001
constrained <- suppressWarnings(run_structural_canvas_analysis(
  snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = fixed_value)
))
negative_sensitivity_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = -.001))
  ""
}, error = conditionMessage)
unknown_sensitivity_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, continuous, "cfa", residual_variance_fixes = c(not_in_data = .001))
  ""
}, error = conditionMessage)
excessive_sensitivity_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = stats::var(continuous$x1)))
  ""
}, error = conditionMessage)
conflicting_residual_snapshot <- snapshot
conflicting_residual_snapshot$nodes <- c(conflicting_residual_snapshot$nodes, list(list(id = "e_conflict", role = "error", name = "e_conflict")))
conflicting_residual_snapshot$edges <- c(conflicting_residual_snapshot$edges, list(list(from = "e_conflict", to = "indicator_1", free = FALSE, fixedValue = .10)))
conflicting_sensitivity_error <- tryCatch({
  run_structural_canvas_analysis(conflicting_residual_snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = fixed_value))
  ""
}, error = conditionMessage)
stopifnot(
  loading_table$free[[1L]] == 0L,
  loading_table$free[[2L]] > 0L,
  grepl("x1 ~~", constrained$syntax, fixed = TRUE),
  grepl("*x1", constrained$syntax, fixed = TRUE),
  identical(constrained$admissible, structural_canvas_fit_admissibility(constrained$fit)$admissible),
  grepl("finite and greater than zero", negative_sensitivity_error, fixed = TRUE),
  grepl("not found in the data", unknown_sensitivity_error, fixed = TRUE),
  grepl("smaller than its indicator's observed variance", excessive_sensitivity_error, fixed = TRUE),
  grepl("conflict with existing canvas residual constraints", conflicting_sensitivity_error, fixed = TRUE)
)

second_latent <- list(id = "latent_2", role = "latent", name = "eta2", x = 700, y = 200)
two_factor_snapshot <- snapshot
two_factor_snapshot$nodes <- c(two_factor_snapshot$nodes, list(second_latent))
missing_covariances <- structural_canvas_missing_exogenous_covariances(two_factor_snapshot)
two_factor_snapshot$edges <- c(two_factor_snapshot$edges, list(list(from = "latent_1", to = "latent_2", kind = "covariance")))
cross_loading_snapshot <- two_factor_snapshot
cross_loading_snapshot$edges <- c(cross_loading_snapshot$edges, list(list(from = "latent_2", to = "indicator_1")))
cross_loading_diagnostics <- structural_canvas_identification_diagnostics(cross_loading_snapshot)
error_snapshot <- snapshot
error_snapshot$nodes <- c(error_snapshot$nodes, list(list(id = "e1", role = "error"), list(id = "e2", role = "error")))
error_snapshot$edges <- c(error_snapshot$edges, list(list(from = "e1", to = "e2", kind = "covariance")))
error_diagnostics <- structural_canvas_error_covariance_diagnostics(error_snapshot)
stopifnot(
  length(missing_covariances) == 1L,
  grepl("eta1", missing_covariances, fixed = TRUE),
  grepl("eta2", missing_covariances, fixed = TRUE),
  length(structural_canvas_missing_exogenous_covariances(two_factor_snapshot)) == 0L,
  any(cross_loading_diagnostics$Code == "cross_loading" & cross_loading_diagnostics$Element == "x1" & cross_loading_diagnostics$Severity == "Warning"),
  error_diagnostics$count == 1L,
  error_diagnostics$possible == 3L,
  identical(error_diagnostics$status, "Review complexity")
)

set.seed(20260813)
second_order_score <- stats::rnorm(n)
first_scores <- lapply(c(.8, .7, .9), function(loading) loading * second_order_score + stats::rnorm(n, sd = sqrt(1 - loading^2)))
second_order_data <- data.frame(
  a1 = .8 * first_scores[[1L]] + stats::rnorm(n, sd = .6), a2 = .7 * first_scores[[1L]] + stats::rnorm(n, sd = .7), a3 = .9 * first_scores[[1L]] + stats::rnorm(n, sd = .5),
  b1 = .8 * first_scores[[2L]] + stats::rnorm(n, sd = .6), b2 = .7 * first_scores[[2L]] + stats::rnorm(n, sd = .7), b3 = .9 * first_scores[[2L]] + stats::rnorm(n, sd = .5),
  c1 = .8 * first_scores[[3L]] + stats::rnorm(n, sd = .6), c2 = .7 * first_scores[[3L]] + stats::rnorm(n, sd = .7), c3 = .9 * first_scores[[3L]] + stats::rnorm(n, sd = .5)
)
second_order_nodes <- list(list(id = "G", role = "latent", name = "G"))
second_order_edges <- list()
for (factor_index in seq_len(3L)) {
  factor_id <- paste0("F", factor_index)
  second_order_nodes <- c(second_order_nodes, list(list(id = factor_id, role = "latent", name = factor_id)))
  second_order_edges <- c(second_order_edges, list(list(from = "G", to = factor_id, pathType = "higherOrder")))
  for (item_index in seq_len(3L)) {
    indicator_name <- paste0(letters[[factor_index]], item_index)
    indicator_id <- paste0("i_", indicator_name)
    second_order_nodes <- c(second_order_nodes, list(list(id = indicator_id, role = "indicator", name = indicator_name)))
    second_order_edges <- c(second_order_edges, list(list(from = factor_id, to = indicator_id)))
  }
}
second_order_snapshot <- list(nodes = second_order_nodes, edges = second_order_edges)
second_order_fit <- run_structural_canvas_analysis(second_order_snapshot, second_order_data, "cfa")
second_order_results <- structural_canvas_higher_order_results(second_order_snapshot, second_order_fit$fit)
omega_h <- structural_canvas_omega_h(second_order_snapshot, second_order_fit$fit)
second_order_identification <- structural_canvas_identification_diagnostics(second_order_snapshot)
factor_correlation_diagnostics <- structural_canvas_factor_correlation_diagnostics(second_order_fit$fit)
stopifnot(
  isTRUE(second_order_fit$converged),
  !length(second_order_fit$negative_latent_variances),
  grepl("G =~ F1 + F2 + F3", second_order_fit$syntax, fixed = TRUE),
  !grepl("F1 ~ G", second_order_fit$syntax, fixed = TRUE),
  isTRUE(second_order_results$available),
  nrow(second_order_results$table) == 3L,
  all(is.finite(second_order_results$table$Beta)),
  all(is.finite(second_order_results$table$BCILower)),
  all(is.finite(second_order_results$table$BCIUpper)),
  all(second_order_results$table$BCILower <= second_order_results$table$B),
  all(second_order_results$table$B <= second_order_results$table$BCIUpper),
  all(is.finite(second_order_results$table$BetaCILower)),
  all(is.finite(second_order_results$table$BetaCIUpper)),
  all(second_order_results$table$BetaCILower <= second_order_results$table$Beta),
  all(second_order_results$table$Beta <= second_order_results$table$BetaCIUpper),
  all(is.finite(second_order_results$table$R2)),
  all(is.finite(second_order_results$table$R2CILower)),
  all(is.finite(second_order_results$table$R2CIUpper)),
  all(second_order_results$table$R2CILower <= second_order_results$table$R2),
  all(second_order_results$table$R2 <= second_order_results$table$R2CIUpper),
  isTRUE(omega_h$available),
  is.finite(omega_h$omega_h),
  omega_h$omega_h > 0,
  omega_h$omega_h <= 1,
  omega_h$indicators == 9L,
  identical(structural_canvas_higher_order_loading_guidance(.30), "Weak loading review"),
  identical(structural_canvas_higher_order_loading_guidance(-.60), "No loading flag"),
  identical(structural_canvas_omega_h_guidance(.65), "Below common .70 guideline"),
  identical(structural_canvas_omega_h_guidance(.80), "Meets common .70 guideline"),
  identical(structural_canvas_omega_h_guidance(1.05), "Review inadmissible coefficient"),
  !any(second_order_identification$Severity == "Error"),
  nrow(factor_correlation_diagnostics) > 0L,
  all(factor_correlation_diagnostics$Severity %in% c("Acceptable", "Review", "High", "Severe", "Inadmissible", "Unavailable"))
)

single_indicator_snapshot <- list(
  nodes = list(list(id = "F", role = "latent", name = "F"), list(id = "x", role = "indicator", name = "x")),
  edges = list(list(from = "F", to = "x"))
)
single_indicator_diagnostics <- structural_canvas_identification_diagnostics(single_indicator_snapshot)
two_indicator_snapshot <- single_indicator_snapshot
two_indicator_snapshot$nodes <- c(two_indicator_snapshot$nodes, list(list(id = "y", role = "indicator", name = "y")))
two_indicator_snapshot$edges <- c(two_indicator_snapshot$edges, list(list(from = "F", to = "y")))
two_indicator_diagnostics <- structural_canvas_identification_diagnostics(two_indicator_snapshot)
duplicate_snapshot <- snapshot
duplicate_snapshot$edges <- c(duplicate_snapshot$edges, list(duplicate_snapshot$edges[[1L]]))
stopifnot(
  any(single_indicator_diagnostics$Code == "single_indicator" & single_indicator_diagnostics$Severity == "Error"),
  any(two_indicator_diagnostics$Code == "two_indicators" & two_indicator_diagnostics$Severity == "Warning"),
  !any(two_indicator_diagnostics$Severity == "Error"),
  any(structural_canvas_identification_diagnostics(duplicate_snapshot)$Code == "duplicate_path")
)

stopifnot(identical(structural_canvas_parameter_term(list(free = FALSE, fixedValue = .8), "x1"), "0.8*x1"))
stopifnot(identical(structural_canvas_parameter_term(list(startValue = .7, parameterName = "loading2"), "x2"), "start(0.7)*loading2*x2"))
invalid_label_error <- tryCatch({
  structural_canvas_parameter_term(list(parameterName = "2bad label"), "x1")
  ""
}, error = conditionMessage)
modified_snapshot <- snapshot
modified_snapshot$edges[[2L]]$free <- FALSE
modified_snapshot$edges[[2L]]$fixedValue <- .75
modified_snapshot$edges[[3L]]$startValue <- .9
modified_snapshot$edges[[3L]]$parameterName <- "lambda3"
modified_fit <- run_structural_canvas_analysis(modified_snapshot, continuous, "cfa")
stopifnot(
  grepl("Invalid lavaan parameter label", invalid_label_error, fixed = TRUE),
  grepl("0.75*x2", modified_fit$syntax, fixed = TRUE),
  grepl("start(0.9)*lambda3*x3", modified_fit$syntax, fixed = TRUE)
)

single_constrained_snapshot <- single_indicator_snapshot
single_constrained_snapshot$nodes <- c(single_constrained_snapshot$nodes, list(list(id = "e", role = "error", name = "e")))
single_constrained_snapshot$edges <- c(single_constrained_snapshot$edges, list(list(from = "e", to = "x", free = FALSE, fixedValue = .20)))
single_constrained_diagnostics <- structural_canvas_identification_diagnostics(single_constrained_snapshot)
negative_residual_snapshot <- single_constrained_snapshot
negative_residual_snapshot$edges[[2L]]$fixedValue <- -.10
negative_residual_diagnostics <- structural_canvas_identification_diagnostics(negative_residual_snapshot)
zero_residual_snapshot <- single_constrained_snapshot
zero_residual_snapshot$edges[[2L]]$fixedValue <- 0
zero_residual_diagnostics <- structural_canvas_identification_diagnostics(zero_residual_snapshot)
single_constrained_data <- data.frame(x = stats::rnorm(300))
residual_scale_ok <- structural_canvas_fixed_residual_scale_diagnostics(single_constrained_snapshot, single_constrained_data)
excessive_residual_snapshot <- single_constrained_snapshot
excessive_residual_snapshot$edges[[2L]]$fixedValue <- 2 * stats::var(single_constrained_data$x)
residual_scale_bad <- structural_canvas_fixed_residual_scale_diagnostics(excessive_residual_snapshot, single_constrained_data)
multi_indicator_residual_snapshot <- snapshot
multi_indicator_residual_snapshot$nodes <- c(multi_indicator_residual_snapshot$nodes, list(list(id = "e_multi", role = "error", name = "e_multi")))
multi_indicator_residual_snapshot$edges <- c(multi_indicator_residual_snapshot$edges, list(list(from = "e_multi", to = "indicator_1", free = FALSE, fixedValue = 2 * stats::var(continuous$x1))))
multi_indicator_scale <- structural_canvas_fixed_residual_scale_diagnostics(multi_indicator_residual_snapshot, continuous)
single_constrained_fit <- run_structural_canvas_analysis(single_constrained_snapshot, single_constrained_data, "cfa")
negative_residual_error <- tryCatch({
  run_structural_canvas_analysis(negative_residual_snapshot, single_constrained_data, "cfa")
  ""
}, error = conditionMessage)
excessive_residual_error <- tryCatch({
  run_structural_canvas_analysis(excessive_residual_snapshot, single_constrained_data, "cfa")
  ""
}, error = conditionMessage)
stopifnot(
  !any(single_constrained_diagnostics$Severity == "Error"),
  any(single_constrained_diagnostics$Code == "single_indicator_constrained"),
  length(structural_canvas_constrained_single_indicators(single_indicator_snapshot)) == 0L,
  identical(structural_canvas_constrained_single_indicators(single_constrained_snapshot), "F"),
  any(negative_residual_diagnostics$Code == "negative_fixed_residual" & negative_residual_diagnostics$Severity == "Error"),
  any(zero_residual_diagnostics$Code == "boundary_fixed_residual" & zero_residual_diagnostics$Severity == "Warning"),
  any(zero_residual_diagnostics$Code == "single_indicator" & zero_residual_diagnostics$Severity == "Error"),
  length(structural_canvas_constrained_single_indicators(zero_residual_snapshot)) == 0L,
  nrow(residual_scale_ok) == 1L,
  isTRUE(residual_scale_ok[["Single-indicator factor"]][[1L]]),
  identical(residual_scale_ok$Status[[1L]], "Within observed variance"),
  nrow(residual_scale_bad) == 1L,
  identical(residual_scale_bad$Status[[1L]], "Exceeds observed variance"),
  nrow(multi_indicator_scale) == 1L,
  identical(multi_indicator_scale$Status[[1L]], "Exceeds observed variance"),
  identical(multi_indicator_scale[["Single-indicator factor"]][[1L]], FALSE),
  grepl("x ~~ 0.2*x", single_constrained_fit$syntax, fixed = TRUE),
  isTRUE(single_constrained_fit$converged),
  grepl("cannot be fixed to a negative value", negative_residual_error, fixed = TRUE),
  grepl("must be smaller than the observed variance", excessive_residual_error, fixed = TRUE)
)

round_trip_snapshot <- list(
  modelSchemaVersion = 3L,
  nodes = list(list(id = "G", role = "latent", name = "G"), list(id = "F1", role = "latent", name = "F1")),
  edges = list(list(from = "G", to = "F1", pathType = "higherOrder", free = FALSE, fixedValue = .8, startValue = .7, parameterName = "gamma1", equalityLabel = "equal_general"))
)
round_trip_json <- jsonlite::toJSON(round_trip_snapshot, auto_unbox = TRUE, null = "null")
round_trip_restored <- jsonlite::fromJSON(round_trip_json, simplifyVector = FALSE)
restored_edge <- round_trip_restored$edges[[1L]]
stopifnot(
  identical(restored_edge$pathType, "higherOrder"),
  identical(restored_edge$free, FALSE),
  identical(restored_edge$fixedValue, .8),
  identical(restored_edge$startValue, .7),
  identical(restored_edge$parameterName, "gamma1"),
  identical(restored_edge$equalityLabel, "equal_general")
)

cat("CFA identification validations passed.\n")
