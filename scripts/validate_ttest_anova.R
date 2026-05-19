all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_ttest_anova.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
source_app_modules(dir = file.path(repo_root, "R"))
library(shiny)
library(htmltools)

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

message("Checking t-test / ANOVA numbered notes...")
data <- data.frame(
  y = c(1:20, 1:20),
  g2 = rep(c("A", "B"), each = 20),
  g3 = rep(c("A", "B", "C", "A"), each = 10),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = c("y", "g2", "g3"),
  measurement = c("continuous", "binary", "category"),
  stringsAsFactors = FALSE
)

result <- prepare_ttest_anova_results(
  data,
  dependents = "y",
  factors = c("g2", "g3"),
  variable_info = variable_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE)
)

table <- result$results[[1]]$table
note <- result$results[[1]]$note

expect_true(grepl("[0-9]$", table[["Effect size"]][[1]]), "Expected first effect-size value to include a numbered marker")
expect_true(grepl("[0-9]$", table[["p"]][[3]]), "Expected Welch ANOVA p-value to include a numbered marker")
expect_true(grepl("[0-9]$", table[["Effect size"]][[3]]), "Expected second effect-size value to include a numbered marker")
expect_true(grepl("1\\. Effect size = Hedges' g\\.", note), "Expected numbered Hedges' g note")
expect_true(grepl("2\\. Welch test was used because homogeneity of variance was not satisfied\\.", note), "Expected numbered Welch note")
expect_true(grepl("3\\. Effect size = omega squared\\.", note), "Expected numbered omega squared note")

html <- as.character(tags_to_html(ttest_anova_results_ui(result)))
expect_true(grepl('class="coefficient-footnote-marker">1</sup>', html, fixed = TRUE), "Expected Hedges' g marker to render as superscript")
expect_true(grepl('class="coefficient-footnote-marker">2</sup>', html, fixed = TRUE), "Expected p-value marker to render as superscript")
expect_true(grepl('class="coefficient-footnote-marker">3</sup>', html, fixed = TRUE), "Expected omega squared marker to render as superscript")

message("All t-test / ANOVA validations passed.")
