source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))

set.seed(20260814)
n <- 240L
factor_score <- stats::rnorm(n)
continuous <- data.frame(
  x1 = .8 * factor_score + stats::rnorm(n, sd = .6),
  x2 = .7 * factor_score + stats::rnorm(n, sd = .7),
  x3 = .9 * factor_score + stats::rnorm(n, sd = .5),
  group = rep(c("A", "B"), each = n / 2L)
)

unbalanced <- continuous
unbalanced$group <- c(rep("A", 210L), rep("B", 30L))
unbalanced_diagnostics <- structural_canvas_invariance_group_diagnostics(
  unbalanced, "group", c("x1", "x2", "x3")
)
very_small <- continuous
very_small$group <- c(rep("A", 220L), rep("B", 20L))
very_small_diagnostics <- structural_canvas_invariance_group_diagnostics(
  very_small, "group", c("x1", "x2", "x3")
)
stopifnot(
  any(unbalanced_diagnostics$Status == "Severely unbalanced smallest group; review power/stability"),
  any(very_small_diagnostics$Status == "Very small group (N < 30); invariance estimates may be unstable")
)

invariance <- structural_canvas_measurement_invariance(
  "eta1 =~ x1 + x2 + x3", continuous, "group", estimator = "MLR"
)
score_column <- grep("^Score", names(invariance$score_diagnostics$Metric), value = TRUE)[1L]
stopifnot(
  identical(invariance$table$Model, c("Configural", "Metric", "Scalar", "Strict")),
  nrow(invariance$table) == 4L,
  length(invariance$fits) == 4L,
  all(invariance$table$Converged),
  all(invariance$table$Admissible),
  all(invariance$table[["Admissibility reasons"]] == "None"),
  all(invariance$table[["Parameter boundary dimensions"]] <= invariance$table[["Explicit equality constraints"]]),
  all(vapply(invariance$table[c("Residual min eigenvalue", "Latent min eigenvalue", "Parameter min eigenvalue")], function(values) all(is.finite(values)), logical(1))),
  all(vapply(invariance$table[c("Residual condition number", "Latent condition number", "Parameter condition number")], function(values) all(is.finite(values) | is.infinite(values)), logical(1))),
  is.logical(invariance$table[["Ill-conditioned warning"]]),
  all(vapply(invariance$fits, function(fit) structural_canvas_fit_admissibility(fit)$admissible, logical(1))),
  all(vapply(invariance$fits, function(fit) identical(structural_canvas_fit_admissibility(fit)$group_labels, c("A", "B")), logical(1))),
  nrow(invariance$group_reliability) == 2L,
  identical(invariance$group_reliability$Group, c("A", "B")),
  nrow(invariance$group_htmt) == 0L,
  identical(names(invariance$score_diagnostics), c("Configural", "Metric", "Scalar", "Strict")),
  nrow(invariance$score_diagnostics$Configural) == 0L,
  all(vapply(invariance$score_diagnostics[-1L], nrow, integer(1)) > 0L),
  !is.na(score_column),
  all(vapply(invariance$score_diagnostics[-1L], function(value) all(diff(value[[score_column]]) <= 0), logical(1))),
  all(vapply(invariance$score_diagnostics[-1L], function(value) all(value[["BH-adjusted p"]] >= value$p, na.rm = TRUE), logical(1)))
)

two_factor_invariance <- structural_canvas_measurement_invariance(
  "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6\neta1 ~~ eta2",
  lavaan::HolzingerSwineford1939, "school", estimator = "MLR"
)
stopifnot(
  nrow(two_factor_invariance$group_reliability) == 4L,
  all(c("Group", "Factor", "AVE", "CR", "Cronbach's alpha", "Omega total") %in% names(two_factor_invariance$group_reliability)),
  identical(sort(unique(two_factor_invariance$group_reliability$Factor)), c("eta1", "eta2")),
  all(is.finite(two_factor_invariance$group_reliability$AVE)),
  nrow(two_factor_invariance$group_htmt) == 2L,
  all(c("Group", "Factor1", "Factor2", "HTMT", "Criterion") %in% names(two_factor_invariance$group_htmt)),
  all(is.finite(two_factor_invariance$group_htmt$HTMT)),
  isTRUE(two_factor_invariance$group_residuals$available),
  nrow(two_factor_invariance$group_residuals$group_summary) == 2L,
  nrow(two_factor_invariance$group_residuals$group_pairs) > 0L,
  all(c("Group", "Indicator1", "Indicator2", "Standardized residual", "Correlation residual", "Screening p", "BH-adjusted screening p", "Exceeds descriptive cutoff") %in% names(two_factor_invariance$group_residuals$group_pairs))
)

ordinal <- as.data.frame(lapply(continuous[c("x1", "x2", "x3")], function(value) {
  as.integer(cut(value, breaks = stats::quantile(value, probs = seq(0, 1, .2)), include.lowest = TRUE))
}))
ordinal$group <- continuous$group
ordered_names <- c("x1", "x2", "x3")
ordinal_invariance <- structural_canvas_measurement_invariance(
  "eta1 =~ x1 + x2 + x3", ordinal, "group",
  estimator = "WLSMV", missing = "pairwise", ordered = ordered_names
)
stopifnot(
  isTRUE(ordinal_invariance$ordinal),
  identical(ordinal_invariance$table$Model, c("Configural", "Thresholds", "Scalar (thresholds + loadings)", "Strict")),
  all(ordinal_invariance$table$Converged),
  all(ordinal_invariance$table$Admissible),
  all(vapply(ordinal_invariance$fits, function(fit) identical(lavaan::lavInspect(fit, "options")$parameterization, "theta"), logical(1))),
  nrow(ordinal_invariance$group_diagnostics) == 2L,
  all(ordinal_invariance$group_diagnostics[["Absent ordered categories"]] == "None"),
  isTRUE(ordinal_invariance$group_residuals$available),
  nrow(ordinal_invariance$group_residuals$group_summary) == 2L,
  nrow(ordinal_invariance$group_residuals$group_pairs) > 0L,
  identical(names(ordinal_invariance$score_diagnostics), names(ordinal_invariance$fits))
)

absent_category_data <- ordinal
absent_category_data$x1[absent_category_data$group == "A" & absent_category_data$x1 == 5L] <- 4L
absent_category_error <- tryCatch({
  structural_canvas_measurement_invariance(
    "eta1 =~ x1 + x2 + x3", absent_category_data, "group",
    estimator = "WLSMV", missing = "pairwise", ordered = ordered_names
  )
  ""
}, error = conditionMessage)
stopifnot(
  inherits(try(structural_canvas_measurement_invariance("eta1 =~ x1 + x2 + x3", continuous, "missing_group"), silent = TRUE), "try-error"),
  grepl("categories are absent within group", absent_category_error, fixed = TRUE)
)

cat("CFA invariance validations passed.\n")
