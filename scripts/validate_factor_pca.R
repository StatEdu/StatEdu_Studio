all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_factor_pca.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

set.seed(42)
n <- 180
latent1 <- rnorm(n)
latent2 <- rnorm(n)
data <- data.frame(
  x1 = latent1 + rnorm(n, 0, 0.35),
  x2 = latent1 + rnorm(n, 0, 0.35),
  x3 = latent1 + rnorm(n, 0, 0.35),
  x4 = latent2 + rnorm(n, 0, 0.35),
  x5 = latent2 + rnorm(n, 0, 0.35),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(data),
  measurement = "continuous",
  stringsAsFactors = FALSE
)

message("Checking factor analysis defaults and normality-driven method selection...")
factor_result <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = FALSE,
    method = "pa",
    rotation = "varimax",
    criterion = "eigen",
    n_factors = 1
  )
)
expect_true(identical(factor_result$method, "pa"), "Expected default factor method to remain principal axis factoring")
expect_true(is.data.frame(factor_result$loadings_table) && nrow(factor_result$loadings_table) == ncol(data), "Expected factor loading table")
expect_true(is.data.frame(factor_result$eigen_table) && nrow(factor_result$eigen_table) == ncol(data), "Expected factor eigenvalue table")

factor_ml <- prepare_factor_analysis_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    normality = TRUE,
    normality_method = "skew_kurt",
    method = "pa",
    rotation = "varimax",
    criterion = "fixed",
    n_factors = 1
  )
)
expect_true(identical(factor_ml$method, "ml"), "Expected normal data to use maximum likelihood when normality is checked")

skewed_data <- data
skewed_data$x5 <- exp(rnorm(n, 0, 1.8))
factor_pa <- prepare_factor_analysis_results(
  skewed_data,
  variables = names(skewed_data),
  variable_info = variable_info,
  options = list(
    normality = TRUE,
    normality_method = "mardia",
    method = "ml",
    rotation = "varimax",
    criterion = "fixed",
    n_factors = 1
  )
)
expect_true(identical(factor_pa$method, "pa"), "Expected non-normal data to use principal axis factoring when normality is checked")
expect_true(is.data.frame(factor_pa$normality_table) && nrow(factor_pa$normality_table) == 2, "Expected Mardia normality table")

message("Checking PCA options, plots, and exports...")
pca_result <- prepare_pca_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    matrix_type = "correlation",
    rotation = "none",
    criterion = "eigen",
    n_components = 1,
    cumulative_variance = 70,
    scree_plot = TRUE,
    component_plot = TRUE
  )
)
expect_true(pca_result$n_components >= 1, "Expected at least one PCA component")
expect_true(is.data.frame(pca_result$loadings_table) && nrow(pca_result$loadings_table) == ncol(data), "Expected PCA loading table")
expect_true(is.data.frame(pca_result$variance_table) && nrow(pca_result$variance_table) > 0, "Expected PCA variance table")
expect_true(is.data.frame(pca_result$eigen_table) && nrow(pca_result$eigen_table) == ncol(data), "Expected PCA eigenvalue table")

pca_cumulative <- prepare_pca_results(
  data,
  variables = names(data),
  variable_info = variable_info,
  options = list(
    matrix_type = "correlation",
    rotation = "varimax",
    criterion = "cumulative",
    n_components = 1,
    cumulative_variance = 70,
    scree_plot = TRUE,
    component_plot = TRUE
  )
)
expect_true(identical(pca_cumulative$criterion, "cumulative"), "Expected cumulative PCA criterion")

factor_html <- tempfile(fileext = ".html")
factor_xlsx <- tempfile(fileext = ".xlsx")
pca_html <- tempfile(fileext = ".html")
pca_xlsx <- tempfile(fileext = ".xlsx")
write_factor_analysis_results_html(factor_result, factor_html)
save_factor_analysis_excel_file(factor_result, factor_xlsx)
write_pca_results_html(pca_result, pca_html)
save_pca_excel_file(pca_result, pca_xlsx)
expect_true(file.exists(factor_html) && file.info(factor_html)$size > 0, "Expected factor analysis HTML export")
expect_true(file.exists(factor_xlsx) && file.info(factor_xlsx)$size > 0, "Expected factor analysis Excel export")
expect_true(file.exists(pca_html) && file.info(pca_html)$size > 0, "Expected PCA HTML export")
expect_true(file.exists(pca_xlsx) && file.info(pca_xlsx)$size > 0, "Expected PCA Excel export")

message("Factor analysis and PCA validations passed.")
