all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_linear_regression_sci.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(repo_root)
options(statedu.output_decimal_digits = 3L)

source(file.path(repo_root, "R", "app_bootstrap.R"))
source_app_modules(dir = file.path(repo_root, "R"))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(htmltools))

assert_close <- function(actual, expected, tolerance = 1e-9, label = "value") {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (length(actual) != length(expected) || any(!is.finite(actual)) ||
      any(!is.finite(expected)) || any(abs(actual - expected) > tolerance)) {
    stop(sprintf("%s mismatch: actual=%s expected=%s", label,
                 paste(signif(actual, 12), collapse = ", "),
                 paste(signif(expected, 12), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

manual_bc_ci <- function(point, values, conf = .95) {
  values <- values[is.finite(values)]
  alpha <- (1 - conf) / 2
  prop_less <- mean(values < point)
  prop_less <- min(max(prop_less, .5 / length(values)), 1 - .5 / length(values))
  probabilities <- stats::pnorm(2 * stats::qnorm(prop_less) + stats::qnorm(c(alpha, 1 - alpha)))
  as.numeric(stats::quantile(values, probabilities, names = FALSE, type = 6))
}

manual_boot_p <- function(values) {
  values <- values[is.finite(values)]
  min(1, 2 * min((sum(values <= 0) + 1) / (length(values) + 1),
                 (sum(values >= 0) + 1) / (length(values) + 1)))
}

variable_info_for <- function(data) {
  data.frame(
    name = names(data), var_label = names(data), role = "",
    measurement = "continuous", stringsAsFactors = FALSE
  )
}

message("Checking OLS coefficients, fit, standardized coefficients, unique effects, VIF, and complete cases...")
set.seed(20260821)
n <- 230L
data <- data.frame(X1 = stats::rnorm(n), X2 = stats::rnorm(n), C = stats::rnorm(n))
data$Y <- .45 + .72 * data$X1 - .38 * data$X2 + .21 * data$C + stats::rnorm(n, sd = .85)
data$X2[c(13L, 61L)] <- NA_real_
data$Y[[117L]] <- NA_real_
complete <- data[stats::complete.cases(data[c("Y", "X1", "X2", "C")]), , drop = FALSE]
reference <- stats::lm(Y ~ X1 + X2 + C, data = complete)
prepared <- prepare_single_regression_result(
  dependent = "Y", data = data, predictors = c("X1", "X2", "C"),
  variable_info = variable_info_for(data), residual_diagnostics = FALSE, auto_method = FALSE
)
result <- prepared$result
stopifnot(is.null(prepared$job), result$n == nrow(complete), identical(result$model_test_label, "F"))
assert_close(stats::coef(result$model), stats::coef(reference), label = "OLS coefficients")
assert_close(result$coef_table$SE, summary(reference)$coefficients[, "Std. Error"], label = "OLS SE")
assert_close(result$r_squared, summary(reference)$r.squared, label = "R squared")
assert_close(result$adjusted_r_squared, summary(reference)$adj.r.squared, label = "adjusted R squared")
assert_close(result$f_statistic, unname(summary(reference)$fstatistic[["value"]]), label = "OLS F")
assert_close(result$f_p, stats::pf(result$f_statistic, result$f_df1, result$f_df2, lower.tail = FALSE), label = "OLS F p")

matrix <- stats::model.matrix(reference)
outcome <- stats::model.response(stats::model.frame(reference))
expected_beta <- stats::coef(reference) * apply(matrix, 2L, stats::sd) / stats::sd(outcome)
expected_beta[["(Intercept)"]] <- NA_real_
assert_close(result$coef_table$beta[-1L], expected_beta[-1L], label = "standardized coefficients")
for (term in c("X1", "X2", "C")) {
  reduced <- stats::lm.fit(matrix[, setdiff(colnames(matrix), term), drop = FALSE], outcome)
  reduced_r2 <- 1 - sum(reduced$residuals^2) / sum((outcome - mean(outcome))^2)
  sr2 <- max(0, summary(reference)$r.squared - reduced_r2)
  assert_close(result$coef_table$sr2[result$coef_table$Term == term], sr2, label = paste("sr2", term))
  assert_close(result$coef_table$f2[result$coef_table$Term == term], sr2 / (1 - summary(reference)$r.squared), label = paste("f2", term))
  auxiliary <- stats::lm(matrix[, term] ~ matrix[, setdiff(c("(Intercept)", "X1", "X2", "C"), term), drop = FALSE])
  expected_vif <- 1 / (1 - summary(auxiliary)$r.squared)
  assert_close(result$coef_table$VIF[result$coef_table$Term == term], expected_vif, label = paste("VIF", term))
}
assert_close(result$dw_d, sum(diff(stats::residuals(reference))^2) / sum(stats::residuals(reference)^2), label = "Durbin-Watson d")

message("Checking HC3 coefficient inference and robust omnibus Wald F...")
set.seed(442)
hetero_n <- 320L
hetero <- data.frame(X1 = stats::rnorm(hetero_n), X2 = stats::rnorm(hetero_n))
hetero$Y <- .2 + .55 * hetero$X1 - .3 * hetero$X2 +
  stats::rnorm(hetero_n, sd = exp(.85 * hetero$X1))
hetero_prepared <- prepare_single_regression_result(
  dependent = "Y", data = hetero, predictors = c("X1", "X2"),
  variable_info = variable_info_for(hetero), residual_diagnostics = TRUE, auto_method = TRUE,
  boot_r = 80L, seed = 44L
)
hetero_result <- hetero_prepared$result
stopifnot(isTRUE(hetero_result$use_hc3), identical(hetero_result$model_test_label, "Robust Wald F"))
hetero_fit <- stats::lm(Y ~ X1 + X2, hetero)
hc3 <- sandwich::vcovHC(hetero_fit, type = "HC3")
hc3_test <- lmtest::coeftest(hetero_fit, vcov. = hc3)
assert_close(hetero_result$coef_table[["HC3 SE"]], hc3_test[, 2L], label = "HC3 SE")
beta <- stats::coef(hetero_fit)[c("X1", "X2")]
covariance <- hc3[c("X1", "X2"), c("X1", "X2")]
robust_f <- as.numeric(t(beta) %*% solve(covariance) %*% beta / length(beta))
assert_close(hetero_result$f_statistic, robust_f, label = "robust omnibus F")
assert_close(hetero_result$f_p, stats::pf(robust_f, length(beta), stats::df.residual(hetero_fit), lower.tail = FALSE), label = "robust omnibus p")
stopifnot(grepl("Robust Wald F", coefficient_fit_line(hetero_result), fixed = TRUE))
hetero_summary <- hierarchical_summary_values(list(hetero_result))
stopifnot(identical(attr(hetero_summary, "f_label", exact = TRUE), "Robust Wald F(p)"))
hetero_table <- hierarchical_compact_coefficient_table(
  list(hetero_result$coef_table), "Model 1", hetero_summary, output_table_style = "compact"
)
stopifnot("Robust Wald F(p)" %in% names(hetero_table))

message("Checking case-resampling BC and percentile bootstrap independently...")
boot_r <- 260L
boot_seed <- 792L
formula <- Y ~ X1 + X2 + C
boot_table <- bootstrap_coef_table(complete, formula, r = boot_r, seed = boot_seed, ci_method = "bias_corrected")
design <- stats::model.matrix(formula, complete)
response <- complete$Y
set.seed(boot_seed)
manual_samples <- matrix(NA_real_, boot_r, ncol(design), dimnames = list(NULL, colnames(design)))
for (index in seq_len(boot_r)) {
  rows <- sample.int(nrow(design), nrow(design), replace = TRUE)
  manual_samples[index, ] <- stats::lm.fit(design[rows, , drop = FALSE], response[rows])$coefficients
}
point <- stats::coef(reference)
for (term in names(point)) {
  row <- boot_table[boot_table$Term == term, , drop = FALSE]
  values <- manual_samples[, term]
  ci <- manual_bc_ci(point[[term]], values)
  assert_close(row$Boot_SE, stats::sd(values), label = paste("bootstrap SE", term))
  assert_close(c(row$Boot_LLCI, row$Boot_ULCI), ci, label = paste("BC CI", term))
  assert_close(row$Boot_p, manual_boot_p(values), label = paste("bootstrap p", term))
  stopifnot(row$Requested == boot_r, row$Valid == boot_r, row[["Valid %"]] == 100,
            identical(row$Status, "Adequate"))
}
percentile <- bootstrap_summary_table(manual_samples, reference, ci_method = "percentile")
for (term in names(point)) {
  row <- percentile[percentile$Term == term, , drop = FALSE]
  expected <- as.numeric(stats::quantile(manual_samples[, term], c(.025, .975), names = FALSE, type = 6))
  assert_close(c(row$Boot_LLCI, row$Boot_ULCI), expected, label = paste("percentile CI", term))
}
stopifnot(identical(boot_table, bootstrap_coef_table(complete, formula, r = boot_r, seed = boot_seed, ci_method = "bias_corrected")))

message("Checking valid-replicate gates and suppression...")
synthetic <- matrix(NA_real_, nrow = 100L, ncol = 4L,
                    dimnames = list(NULL, c("(Intercept)", "X1", "X2", "C")))
synthetic[seq_len(19L), "(Intercept)"] <- seq(.1, .3, length.out = 19L)
synthetic[seq_len(60L), "X1"] <- seq(.1, .5, length.out = 60L)
synthetic[seq_len(80L), "X2"] <- seq(-.1, .6, length.out = 80L)
synthetic[, "C"] <- seq(-.2, .4, length.out = 100L)
gated <- bootstrap_summary_table(synthetic, reference)
stopifnot(identical(gated$Status, c("Unreliable", "Caution", "Adequate", "Adequate")))
stopifnot(all(is.na(gated[1L, c("Boot_LLCI", "Boot_ULCI", "Boot_p")])))
stopifnot(all(is.finite(as.numeric(unlist(gated[2L:3L, c("Boot_LLCI", "Boot_ULCI", "Boot_p")], use.names = FALSE)))))

message("Checking hierarchical common sample, Delta R2, F change, and paired bootstrap CI...")
hier_data <- data
hier <- prepare_hierarchical_analysis_results(
  hier_data, dependents = "Y", block1 = c("C"), block2 = c("X1", "X2"),
  variable_info = variable_info_for(hier_data), residual_diagnostics = FALSE, auto_method = FALSE
)$results
stopifnot(length(hier) == 2L, all(vapply(hier, `[[`, numeric(1), "n") == nrow(complete)))
ref1 <- stats::lm(Y ~ C, complete)
ref2 <- stats::lm(Y ~ C + X1 + X2, complete)
delta <- summary(ref2)$r.squared - summary(ref1)$r.squared
f_change <- (delta / 2) / ((1 - summary(ref2)$r.squared) / stats::df.residual(ref2))
assert_close(hier[[2L]]$r_squared - hier[[1L]]$r_squared, delta, label = "Delta R squared")
line <- hierarchical_delta_line(hier[[1L]], hier[[2L]])
stopifnot(grepl(format_decimal3(delta), line, fixed = TRUE), grepl(format_p(stats::pf(f_change, 2, stats::df.residual(ref2), lower.tail = FALSE)), line, fixed = TRUE))

set.seed(91L)
paired_r <- 120L
r2_1 <- r2_2 <- rep(NA_real_, paired_r)
for (index in seq_len(paired_r)) {
  rows <- sample.int(nrow(complete), nrow(complete), replace = TRUE)
  sample_data <- complete[rows, , drop = FALSE]
  r2_1[[index]] <- summary(stats::lm(Y ~ C, sample_data))$r.squared
  r2_2[[index]] <- summary(stats::lm(Y ~ C + X1 + X2, sample_data))$r.squared
}
hier[[1L]]$use_bootstrap <- TRUE
hier[[2L]]$use_bootstrap <- TRUE
hier[[1L]]$bootstrap_r_squared <- r2_1
hier[[2L]]$bootstrap_r_squared <- r2_2
hier[[1L]]$bootstrap_ci_method <- "bias_corrected"
hier[[2L]]$bootstrap_ci_method <- "bias_corrected"
delta_ci <- hierarchical_bootstrap_delta_r2_ci(hier[[1L]], hier[[2L]])
assert_close(delta_ci, manual_bc_ci(delta, r2_2 - r2_1), label = "paired Delta R squared BC CI")
stopifnot(identical(attr(delta_ci, "status", exact = TRUE), "Adequate"))

message("Checking rank-deficient model gatekeeping...")
rank_data <- data.frame(Y = seq_len(30L), X1 = seq_len(30L), X2 = 2 * seq_len(30L))
rank_prepared <- prepare_regression_analysis_results(
  rank_data, dependents = "Y", predictors = c("X1", "X2"),
  variable_info = variable_info_for(rank_data), residual_diagnostics = FALSE, auto_method = FALSE
)
stopifnot(length(rank_prepared$results) == 0L)
rank_skipped <- attr(rank_prepared$results, "skipped")
stopifnot(is.data.frame(rank_skipped), any(grepl("not uniquely estimable", rank_skipped$Message, fixed = TRUE)))

message("Checking unused factor levels after complete-case filtering...")
factor_data <- data.frame(
  Y = c(2, 3, 5, 4, 8, 7, 9, 12, NA_real_, NA_real_),
  X = seq_len(10L),
  Group = factor(c(rep(c("A", "B"), 4L), "C", "C"), levels = c("A", "B", "C"))
)
factor_prepared <- prepare_regression_analysis_results(
  factor_data, dependents = "Y", predictors = c("X", "Group"),
  variable_info = variable_info_for(factor_data), residual_diagnostics = FALSE, auto_method = FALSE
)
stopifnot(length(factor_prepared$results) == 1L)
stopifnot(!any(grepl("GroupC", factor_prepared$results[[1L]]$coef_table$Term, fixed = TRUE)))

message("Checking bootstrap diagnostics in UI and Excel exports...")
result$use_bootstrap <- TRUE
result$bootstrap_ci_method <- "bias_corrected"
result$boot_table <- boot_table
result_list <- list(result)
html <- htmltools::renderTags(regression_results_panel(result_list, variable_table = variable_info_for(data)))$html
stopifnot(grepl("Bootstrap diagnostics", html, fixed = TRUE), grepl("Adequate", html, fixed = TRUE))
xlsx <- tempfile(fileext = ".xlsx")
save_analysis_excel_file(result_list, xlsx, variable_table = variable_info_for(data))
stopifnot("Bootstrap diagnostics" %in% openxlsx::getSheetNames(xlsx))

hier[[1L]]$boot_table <- bootstrap_summary_table(manual_samples[, c("(Intercept)", "C"), drop = FALSE], ref1)
hier[[2L]]$boot_table <- bootstrap_summary_table(manual_samples, ref2)
hier_xlsx <- tempfile(fileext = ".xlsx")
save_hierarchical_excel_file(hier, hier_xlsx, variable_table = variable_info_for(data))
stopifnot("Bootstrap diagnostics" %in% openxlsx::getSheetNames(hier_xlsx))

message("Linear/hierarchical regression SCI validation passed.")
