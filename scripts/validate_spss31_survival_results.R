all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_spss31_survival_results.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules()

reference_path <- file.path(repo_root, "sample", "spss31_survival_results.csv")
fixture_path <- file.path(repo_root, "scripts", "fixtures", "survival_validation.csv")
stopifnot(file.exists(reference_path), file.exists(fixture_path))
reference <- utils::read.csv(reference_path, check.names = FALSE, stringsAsFactors = FALSE)
data <- utils::read.csv(fixture_path, check.names = FALSE, stringsAsFactors = FALSE)

ref_value <- function(section, group = "", term = "", metric) {
  selected <- reference$Section == section & reference$Group == group &
    reference$Term == term & reference$Metric == metric
  if (sum(selected) != 1L) stop("Reference key is missing or duplicated: ", paste(section, group, term, metric, sep = " / "))
  reference$Value[selected]
}
expect_close <- function(actual, expected, tolerance = 1e-10, label = "value") {
  error <- abs(as.numeric(actual) - as.numeric(expected))
  if (!is.finite(error) || error > tolerance) {
    stop(sprintf("%s mismatch: actual=%.16g expected=%.16g error=%.3g", label, actual, expected, error), call. = FALSE)
  }
  invisible(error)
}

km <- prepare_km_analysis_result(
  data = data,
  time = "time",
  event = "status",
  group = "sex",
  event_value = "1",
  rate_times = "100, 200, 400",
  test_method = "logrank"
)
cox <- prepare_cox_analysis_result(
  data = data,
  time = "time",
  event = "status",
  covariates = c("age", "sex"),
  event_value = "1",
  ties_method = "efron"
)

# Case counts and the full event-time Kaplan-Meier curve must agree exactly.
for (group in c("1", "2")) {
  row <- km$median_table[km$median_table$Strata == paste0("sex=", group), , drop = FALSE]
  stopifnot(nrow(row) == 1L)
  expect_close(row$Records, ref_value("km_cases", group, metric = "N"), 0, paste("KM", group, "N"))
  expect_close(row$Events, ref_value("km_cases", group, metric = "Events"), 0, paste("KM", group, "events"))
}
expect_close(km$n, ref_value("km_cases", "Overall", metric = "N"), 0, "KM overall N")
expect_close(km$events, ref_value("km_cases", "Overall", metric = "Events"), 0, "KM overall events")

km_summary <- summary(km$fit)
km_curve <- data.frame(
  Group = sub("^.*=", "", as.character(km_summary$strata)),
  Term = format(km_summary$time, scientific = FALSE, trim = TRUE),
  Event = km_summary$n.event,
  Estimate = km_summary$surv,
  SE = km_summary$std.err,
  stringsAsFactors = FALSE
)
km_curve <- km_curve[km_curve$Event > 0, , drop = FALSE]
sp_curve <- reference[reference$Section == "km_survival", c("Group", "Term", "Metric", "Value")]
sp_estimate <- sp_curve[sp_curve$Metric == "Estimate", c("Group", "Term", "Value")]
sp_se <- sp_curve[sp_curve$Metric == "SE", c("Group", "Term", "Value")]
names(sp_estimate)[[3]] <- "SPSS_Estimate"
names(sp_se)[[3]] <- "SPSS_SE"
curve_comparison <- merge(km_curve, merge(sp_estimate, sp_se, by = c("Group", "Term")), by = c("Group", "Term"), all = TRUE)
stopifnot(nrow(curve_comparison) == 50L, !anyNA(curve_comparison))
km_curve_max_error <- max(abs(c(
  curve_comparison$Estimate - curve_comparison$SPSS_Estimate,
  curve_comparison$SE - curve_comparison$SPSS_SE
)))
stopifnot(km_curve_max_error < 1e-12)

expect_close(km$logrank$chisq, ref_value("km_logrank", "Overall", metric = "Chi-square"), 1e-12, "log-rank chi-square")
expect_close(km$logrank$p, ref_value("km_logrank", "Overall", metric = "p"), 1e-12, "log-rank p")
expect_close(km$logrank$df, ref_value("km_logrank", "Overall", metric = "df"), 0, "log-rank df")

# Median point estimates agree. Interval methods are intentionally documented as different.
for (group in c("1", "2")) {
  row <- km$median_table[km$median_table$Strata == paste0("sex=", group), , drop = FALSE]
  expect_close(row$Median, ref_value("km_time", group, "Median", "Estimate"), 0, paste("KM", group, "median"))
}
stopifnot(
  identical(as.numeric(km$median_table[["Median 95% LCL"]]), c(315, 222)),
  identical(as.numeric(km$median_table[["Median 95% UCL"]]), c(481, 426)),
  ref_value("km_time", "1", "Median", "Lower") != 315,
  ref_value("km_time", "2", "Median", "Upper") != 426
)

# SPSS and survival::survfit use a slightly different restricted-mean endpoint
# convention when the last observation is censored. Preserve and bound that known difference.
mean_rows <- km$median_table[match(c("sex=1", "sex=2"), km$median_table$Strata), , drop = FALSE]
spss_means <- vapply(c("1", "2"), function(group) ref_value("km_time", group, "Mean", "Estimate"), numeric(1))
mean_errors <- abs(as.numeric(mean_rows$Mean) - spss_means)
stopifnot(mean_errors[[1]] < 1e-10, mean_errors[[2]] < 0.05)

# Cox coefficients, model tests, and likelihoods should be numerically identical.
cox_max_error <- 0
for (term in c("age", "sex")) {
  row <- cox$coef_table[cox$coef_table$Term == term, , drop = FALSE]
  stopifnot(nrow(row) == 1L)
  actual <- c(B = row$B, SE = row$SE, Wald = row$z^2, df = 1, p = row$p, HR = row$HR, Lower = row$LLCI, Upper = row$ULCI)
  expected <- vapply(names(actual), function(metric) ref_value("cox_coefficient", term = term, metric = metric), numeric(1))
  cox_max_error <- max(cox_max_error, abs(actual - expected))
}
for (test in c("Likelihood-ratio", "Score")) {
  row <- cox$model_tests[cox$model_tests$Test == test, , drop = FALSE]
  stopifnot(nrow(row) == 1L)
  actual <- c(`Chi-square` = row$Statistic, df = row$df, p = row$p)
  expected <- vapply(names(actual), function(metric) ref_value("cox_model", term = test, metric = metric), numeric(1))
  cox_max_error <- max(cox_max_error, abs(actual - expected))
}
cox_max_error <- max(cox_max_error, abs(c(
  -2 * cox$fit$loglik[[1]] - ref_value("cox_model", term = "Null", metric = "-2 Log Likelihood"),
  -2 * cox$fit$loglik[[2]] - ref_value("cox_model", term = "Fitted", metric = "-2 Log Likelihood")
)))
stopifnot(cox_max_error < 1e-10)

cat(sprintf(
  paste0(
    "SPSS 31 survival external validation passed: ",
    "50/50 event-time KM estimates and SEs agree (max error %.3g); ",
    "log-rank and Cox agree (max Cox error %.3g); ",
    "known KM mean endpoint difference %.6f and median-CI convention difference are documented.\n"
  ),
  km_curve_max_error,
  cox_max_error,
  mean_errors[[2]]
))
