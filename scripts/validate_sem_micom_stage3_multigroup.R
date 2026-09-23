source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

if (!requireNamespace("seminr", quietly = TRUE)) stop("seminr is required for MICOM validation.")
stopifnot(identical(formals(structural_canvas_micom)$permutations, 5000L))

expect_equal <- function(actual, expected, tolerance = 1e-12, message = "values differ") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(message, call. = FALSE)
  }
}

# Step 2 must retain the signed correlation. Treating c = -1 as |c| = 1 would
# silently turn opposite composite directions into compositional invariance.
expect_equal(
  structural_canvas_micom_signed_c(c(-2, -1, 0, 1, 2), c(2, 1, 0, -1, -2)),
  -1,
  message = "MICOM compositional correlation is not signed."
)

# Step 3 uses one fixed pooled-score matrix. The variance estimand is the
# Henseler-Ringle-Sarstedt log variance ratio, not a raw variance difference.
stage3_scores <- matrix(c(-1, 1, 0, 2, -2, 2), ncol = 1L, dimnames = list(NULL, "eta"))
stage3_labels <- rep(c("A", "B", "C"), each = 2L)
stage3_pairs <- utils::combn(seq_along(c("A", "B", "C")), 2L)
stage3 <- structural_canvas_micom_stage3_pair_statistics(stage3_scores, stage3_labels, c("A", "B", "C"), stage3_pairs)
expect_equal(stage3$mean_difference, c(-1, 0, 1), message = "Pooled-score mean differences are incorrect.")
expect_equal(stage3$variance_difference, c(0, -6, -6), message = "Raw pooled-score variance differences are incorrect.")
expect_equal(stage3$log_variance_ratio, c(0, log(.25), log(.25)), message = "MICOM log variance ratios are incorrect.")

# Whole-permutation inference is available only when at least 80% of requested
# positions, and no fewer than 19 positions, are jointly finite.
valid_80 <- structural_canvas_micom_permutation_validity(c(rep(TRUE, 80L), rep(FALSE, 20L)), 100L)
invalid_79 <- structural_canvas_micom_permutation_validity(c(rep(TRUE, 79L), rep(FALSE, 21L)), 100L)
stopifnot(isTRUE(valid_80$adequate), identical(valid_80$required, 80L), !isTRUE(invalid_79$adequate))

# Equal-tail probabilities, unlike |T|-tail counting, remain valid for the
# asymmetric permutation nulls produced by unequal group sizes and log ratios.
asymmetric_null <- c(-4, -2, -1, -.5, 0, .1, .2, .3, .4, .5)
equal_tail_p <- structural_canvas_micom_permutation_p(asymmetric_null, .45, "two.sided")
absolute_tail_p <- (1 + sum(abs(asymmetric_null) >= abs(.45))) / (length(asymmetric_null) + 1)
expect_equal(equal_tail_p, 4 / 11, message = "MICOM Step 3 does not use an equal-tail two-sided permutation p-value.")
stopifnot(!isTRUE(all.equal(equal_tail_p, absolute_tail_p)))

# The 2016 MICOM contract is a PLS composite-score procedure. PLSc/common-factor
# invariance is a separate, unvalidated estimand and must fail closed.
plsc_error <- tryCatch(
  {
    structural_canvas_micom(list(), data.frame(group = c("A", "B")), "group", estimator = "PLSC", permutations = 19L)
    ""
  },
  error = function(error) conditionMessage(error)
)
stopifnot(grepl("PLS composite-score estimator", plsc_error, fixed = TRUE), grepl("PLSc", plsc_error, fixed = TRUE))

set.seed(20260825)
n_a <- 24L
n_b <- 20L
n_c <- 22L
n_max <- max(n_a, n_b, n_c)
eta1 <- stats::rnorm(n_max)
eta2 <- .58 * eta1 + stats::rnorm(n_max, sd = .75)
base_data <- data.frame(
  x1 = .82 * eta1 + stats::rnorm(n_max, sd = .40),
  x2 = .76 * eta1 + stats::rnorm(n_max, sd = .48),
  x3 = .70 * eta1 + stats::rnorm(n_max, sd = .55),
  y1 = .84 * eta2 + stats::rnorm(n_max, sd = .38),
  y2 = .77 * eta2 + stats::rnorm(n_max, sd = .46),
  y3 = .72 * eta2 + stats::rnorm(n_max, sd = .52)
)
data <- rbind(base_data[seq_len(n_a), ], base_data[seq_len(n_b), ], base_data[seq_len(n_c), ])
data$group <- rep(c("A", "B", "C"), times = c(n_a, n_b, n_c))
# Missing indicator values must be retained and handled by the same PLS mean-
# replacement policy used by the production estimator.
data$x1[c(2L, n_a + 3L, n_a + n_b + 4L)] <- NA_real_
data$y2[c(5L, n_a + 6L, n_a + n_b + 7L)] <- NA_real_

snapshot <- list(nodes = list(
  list(id = "f1", role = "latent", name = "eta1", constructType = "composite", measurementMode = "reflective"),
  list(id = "f2", role = "latent", name = "eta2", constructType = "composite", measurementMode = "reflective"),
  list(id = "x1", role = "indicator", name = "x1", variableId = "x1"),
  list(id = "x2", role = "indicator", name = "x2", variableId = "x2"),
  list(id = "x3", role = "indicator", name = "x3", variableId = "x3"),
  list(id = "y1", role = "indicator", name = "y1", variableId = "y1"),
  list(id = "y2", role = "indicator", name = "y2", variableId = "y2"),
  list(id = "y3", role = "indicator", name = "y3", variableId = "y3")
), edges = list(
  list(from = "f1", to = "x1"), list(from = "f1", to = "x2"), list(from = "f1", to = "x3"),
  list(from = "f2", to = "y1"), list(from = "f2", to = "y2"), list(from = "f2", to = "y3"),
  list(from = "f1", to = "f2")
))

set.seed(9182)
rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
micom <- structural_canvas_micom(snapshot, data, "group", "PLS", permutations = 19L, seed = 86420L)
rng_after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

required_columns <- c(
  "Group 1", "Group 2", "Construct", "Observed c", "5% permutation c",
  "Compositional permutation p", "Compositional Holm p", "Compositional invariance",
  "Mean difference", "Mean permutation p", "Mean Holm p", "Mean equality",
  "Variance difference", "Log variance ratio", "Variance permutation p", "Variance Holm p",
  "Variance equality", "Invariance level"
)
stopifnot(
  identical(micom$type, "pls_micom"),
  identical(micom$estimator, "PLS"),
  identical(micom$estimand, "composite-score invariance"),
  nrow(micom$table) == choose(3L, 2L) * 2L,
  all(required_columns %in% names(micom$table)),
  nrow(unique(micom$table[c("Group 1", "Group 2")])) == choose(3L, 2L),
  nrow(micom$pairwise_gate) == choose(3L, 2L),
  all(micom$table$`Observed c` >= -1 & micom$table$`Observed c` <= 1),
  micom$observations_used == n_a + n_b + n_c,
  micom$observations_excluded_missing_group == 0L,
  isTRUE(micom$configural_invariance),
  nrow(micom$configural_audit) == 3L,
  all(micom$configural_audit$Passed),
  nrow(micom$group_diagnostics) == 3L,
  all(micom$group_diagnostics$N < 30L),
  all(micom$group_diagnostics$`N warning` != "None"),
  grepl("mean replacement", micom$missing_data_policy, fixed = TRUE),
  grepl("pooled PLS fit", micom$stage3_score_source, fixed = TRUE),
  grepl("unordered pair", micom$permutation_design, fixed = TRUE),
  micom$permutations_valid >= 19L,
  micom$permutation_valid_ratio >= .80,
  length(micom$permutations_valid_by_pair) == choose(3L, 2L),
  length(micom$permutation_valid_ratio_by_pair) == choose(3L, 2L),
  all(micom$pairwise_gate$`Valid permutations` >= 19L),
  all(micom$pairwise_gate$`Valid ratio` >= .80),
  all(micom$pairwise_gate$`Small-N warning` != "None"),
  identical(micom$minimum_valid_ratio, .80),
  identical(micom$multiple_testing$micom_step2$method, "Holm"),
  identical(micom$multiple_testing$micom_step3_mean$method, "Holm"),
  identical(micom$multiple_testing$micom_step3_variance$method, "Holm"),
  identical(micom$multiple_testing$direct_path_permutation_sensitivity$method, "Benjamini-Hochberg"),
  identical(rng_before, rng_after)
)
expect_equal(micom$table$`Compositional Holm p`, stats::p.adjust(micom$table$`Compositional permutation p`, method = "holm"), message = "Step 2 Holm family is incorrect.")
expect_equal(micom$table$`Mean Holm p`, stats::p.adjust(micom$table$`Mean permutation p`, method = "holm"), message = "Step 3 mean Holm family is incorrect.")
expect_equal(micom$table$`Variance Holm p`, stats::p.adjust(micom$table$`Variance permutation p`, method = "holm"), message = "Step 3 variance Holm family is incorrect.")

# Every identical-data group pair passes the composite-score gate, so each pair
# contributes one structural path to the separate exploratory BH family.
stopifnot(
  isTRUE(micom$measurement_gate$passed),
  all(micom$pairwise_gate$`Composite-score invariance gate`),
  nrow(micom$mga_table) == choose(3L, 2L),
  identical(micom$mga_status, "Permutation PLS-MGA completed"),
  all(micom$mga_table$`BH-adjusted p` >= micom$mga_table$`Permutation p`)
)

# A third group's distribution must never enter the A-B pooled fit or label
# permutations. Only multiplicity-adjusted columns can legitimately change
# through the other pairwise families.
extreme_data <- data
c_rows <- extreme_data$group == "C"
for (indicator in c("x1", "x2", "x3", "y1", "y2", "y3")) {
  extreme_data[[indicator]][c_rows] <- 75 * extreme_data[[indicator]][c_rows] + 500
}
micom_extreme <- structural_canvas_micom(snapshot, extreme_data, "group", "PLS", permutations = 19L, seed = 86420L)
ab <- micom$table$`Group 1` == "A" & micom$table$`Group 2` == "B"
ab_extreme <- micom_extreme$table$`Group 1` == "A" & micom_extreme$table$`Group 2` == "B"
pair_native_columns <- c(
  "Observed c", "5% permutation c", "Compositional permutation p",
  "Mean difference", "2.5% permutation mean difference", "97.5% permutation mean difference", "Mean permutation p",
  "Variance difference", "Log variance ratio", "2.5% permutation log variance ratio",
  "97.5% permutation log variance ratio", "Variance permutation p"
)
expect_equal(
  as.matrix(micom$table[ab, pair_native_columns]),
  as.matrix(micom_extreme$table[ab_extreme, pair_native_columns]),
  tolerance = 1e-12,
  message = "The A-B MICOM distribution changed when only group C changed."
)
ab_mga <- micom$mga_table$`Group 1` == "A" & micom$mga_table$`Group 2` == "B"
ab_mga_extreme <- micom_extreme$mga_table$`Group 1` == "A" & micom_extreme$mga_table$`Group 2` == "B"
expect_equal(
  as.matrix(micom$mga_table[ab_mga, c("Path difference", "Permutation p")]),
  as.matrix(micom_extreme$mga_table[ab_mga_extreme, c("Path difference", "Permutation p")]),
  tolerance = 1e-12,
  message = "The A-B PLS-MGA permutation result changed when only group C changed."
)

# Group direction and deterministic pair streams must not depend on the input
# row order. Factor levels are honored; character labels use radix order.
set.seed(3817)
shuffled_data <- data[sample.int(nrow(data)), , drop = FALSE]
micom_shuffled <- structural_canvas_micom(snapshot, shuffled_data, "group", "PLS", permutations = 19L, seed = 86420L)
stopifnot(
  identical(micom$groups, c("A", "B", "C")),
  identical(micom_shuffled$groups, micom$groups),
  identical(micom_shuffled$table[c("Group 1", "Group 2", "Construct")], micom$table[c("Group 1", "Group 2", "Construct")])
)
numeric_table_columns <- names(micom$table)[vapply(micom$table, is.numeric, logical(1))]
expect_equal(
  as.matrix(micom_shuffled$table[numeric_table_columns]),
  as.matrix(micom$table[numeric_table_columns]),
  tolerance = 1e-12,
  message = "MICOM numeric results changed after shuffling input rows."
)
expect_equal(
  as.matrix(micom_shuffled$mga_table[c("Path difference", "Permutation p", "BH-adjusted p")]),
  as.matrix(micom$mga_table[c("Path difference", "Permutation p", "BH-adjusted p")]),
  tolerance = 1e-12,
  message = "PLS-MGA results changed after shuffling input rows."
)

message("SEM MICOM Step 3 and multigroup validation passed.")
