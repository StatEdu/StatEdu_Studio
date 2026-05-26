all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_correlation_auto.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))
library(shiny)

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

set.seed(42)
n <- 120
normal_x <- rnorm(n)
normal_y <- normal_x + rnorm(n, 0, 0.25)
skew_x <- stats::rlnorm(n, meanlog = 0, sdlog = 1.5)
skew_y <- skew_x + stats::rlnorm(n, meanlog = 0, sdlog = 1.2)
ordinal_x <- sample(1:5, n, replace = TRUE)
ordinal_text <- sample(c("low", "middle", "high"), n, replace = TRUE)
binary_x <- sample(c("no", "yes"), n, replace = TRUE)
nominal_x <- sample(c("A", "B", "C"), n, replace = TRUE)
blank_ordinal <- rep("", n)
blank_ordinal[seq(1, n, by = 20)] <- "low"

data <- data.frame(
  normal_x = normal_x,
  normal_y = normal_y,
  skew_x = skew_x,
  skew_y = skew_y,
  ordinal_x = ordinal_x,
  ordinal_text = ordinal_text,
  binary_x = binary_x,
  nominal_x = nominal_x,
  blank_ordinal = blank_ordinal,
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(data),
  measurement = c("continuous", "continuous", "continuous", "continuous", "ordinal", "ordinal", "binary", "category", "ordinal"),
  stringsAsFactors = FALSE
)

message("Checking correlation auto method selection...")
normal_result <- prepare_correlation_results(
  data,
  variables = c("normal_x", "normal_y"),
  variable_info = variable_info,
  options = list(continuous_method = "auto", normality = TRUE, reason = TRUE)
)
expect_true(identical(normal_result$pairwise_table$Method[[1]], "Pearson"), "Expected auto to select Pearson for normal continuous variables")
expect_true(grepl("both continuous variables satisfied normality", normal_result$pairwise_table$Reason[[1]], fixed = TRUE), "Expected Pearson auto-selection reason")

skew_result <- prepare_correlation_results(
  data,
  variables = c("skew_x", "skew_y"),
  variable_info = variable_info,
  options = list(continuous_method = "auto", normality = TRUE, reason = TRUE)
)
expect_true(identical(skew_result$pairwise_table$Method[[1]], "Spearman"), "Expected auto to select Spearman for non-normal continuous variables")
expect_true(grepl("did not satisfy normality", skew_result$pairwise_table$Reason[[1]], fixed = TRUE), "Expected Spearman auto-selection reason")

manual_result <- prepare_correlation_results(
  data,
  variables = c("skew_x", "skew_y"),
  variable_info = variable_info,
  options = list(continuous_method = "pearson", normality = TRUE, reason = TRUE)
)
expect_true(identical(manual_result$pairwise_table$Method[[1]], "Pearson"), "Expected manual Pearson selection to be respected")

ordinal_result <- prepare_correlation_results(
  data,
  variables = c("normal_x", "ordinal_x"),
  variable_info = variable_info,
  options = list(continuous_method = "auto", normality = TRUE, reason = TRUE)
)
expect_true(identical(ordinal_result$pairwise_table$Method[[1]], "Spearman"), "Expected ordinal-involved pair to use Spearman")

ordinal_text_result <- prepare_correlation_results(
  data,
  variables = c("ordinal_x", "ordinal_text"),
  variable_info = variable_info,
  options = list(continuous_method = "auto", normality = TRUE, reason = TRUE)
)
expect_true(identical(ordinal_text_result$pairwise_table$Method[[1]], "Spearman"), "Expected text ordinal variables to be scored and analyzed")
expect_true(isTRUE(ordinal_text_result$pairwise_table$N[[1]] >= 3), "Expected text ordinal variable to retain valid cases")

mixed_result <- prepare_correlation_results(
  data,
  variables = c("normal_x", "binary_x", "nominal_x", "ordinal_text", "blank_ordinal"),
  variable_info = variable_info,
  options = list(continuous_method = "auto", normality = FALSE, reason = TRUE, latent_correlations = TRUE)
)
expect_true(any(mixed_result$pairwise_table$Method == "Point-biserial"), "Expected continuous-binary pair to use point-biserial")
expect_true(any(mixed_result$pairwise_table$Method == "Eta"), "Expected continuous-nominal pair to use Eta")
expect_true(any(mixed_result$pairwise_table$Method == "Cramer's V"), "Expected categorical pairs to use Cramer's V")
expect_true(is.data.frame(mixed_result$omitted_table) && any(mixed_result$omitted_table$Variable == "blank_ordinal"), "Expected sparse ordinal variable to be reported as omitted")
expect_true(isTRUE(mixed_result$options$normality), "Expected auto method to retain normality diagnostics for traceability")
expect_true(is.list(mixed_result$latent) && nrow(mixed_result$latent$pairwise_table) > 0, "Expected latent correlation result set")

message("All correlation auto validations passed.")
