all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_penalized.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))

assert_close <- function(actual, expected, tolerance = 1e-10, label = "value") {
  diff <- max(abs(as.numeric(actual) - as.numeric(expected)), na.rm = TRUE)
  if (!is.finite(diff) || diff > tolerance) {
    stop(sprintf("%s mismatch: max diff=%0.12f", label, diff), call. = FALSE)
  }
}

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

assert_identical <- function(actual, expected, label) {
  if (!identical(as.character(actual), as.character(expected))) {
    stop(sprintf("%s mismatch: actual=%s expected=%s", label, actual, expected), call. = FALSE)
  }
}

set.seed(20260730)
n <- 90
data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  group = factor(rep(c("A", "B", "C"), length.out = n))
)
data$y <- 0.7 + 0.9 * data$x1 - 0.45 * data$x2 + 0.25 * (data$group == "B") - 0.15 * (data$group == "C") + rnorm(n, sd = 0.65)
data$x3[seq(5, n, by = 19)] <- NA

formula <- y ~ x1 + x2 + x3 + group
fit <- stats::lm(formula, data = data)
coef_table <- data.frame(
  Term = rownames(coef(summary(fit))),
  B = as.numeric(stats::coef(fit)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
regression_result <- list(formula = formula, coef_table = coef_table)

seed <- 7731L
alpha_grid <- c(0.25, 0.5, 0.75)
message("Checking penalized regression against direct glmnet calls...")
app <- fit_penalized_models(
  list(regression_result),
  data,
  seed = seed,
  alpha_grid = alpha_grid,
  selection_bootstrap_resamples = 2L
)

raw_model_frame <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
complete_data <- raw_model_frame[stats::complete.cases(raw_model_frame), , drop = FALSE]
x <- stats::model.matrix(formula, data = complete_data)
x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
y <- stats::model.response(complete_data)
nfolds <- min(10L, length(y))

fit_cv <- function(alpha, seed_offset = 0L) {
  set.seed(seed + seed_offset)
  glmnet::cv.glmnet(
    x,
    y,
    alpha = alpha,
    family = "gaussian",
    standardize = TRUE,
    nfolds = nfolds,
    keep = TRUE
  )
}

direct_fits <- list(
  Ridge = fit_cv(0),
  LASSO = fit_cv(1)
)
elastic_candidates <- lapply(alpha_grid, function(alpha) {
  fit <- fit_cv(alpha)
  list(alpha = alpha, fit = fit, cv_mse = min(fit$cvm, na.rm = TRUE))
})
best_elastic <- elastic_candidates[[which.min(vapply(elastic_candidates, function(candidate) candidate$cv_mse, numeric(1)))]]
direct_fits[["Elastic Net"]] <- best_elastic$fit

for (method in names(direct_fits)) {
  direct <- direct_fits[[method]]
  curve <- app$cv_curves[app$cv_curves$Method == method, , drop = FALSE]
  assert_true(nrow(curve) == length(direct$lambda), sprintf("%s CV curve length should match glmnet", method))
  assert_close(curve$lambda, direct$lambda, label = sprintf("%s lambda path", method))
  assert_close(curve[["CV MSE"]], direct$cvm, label = sprintf("%s CV MSE", method))
  assert_close(curve[["CV SE"]], direct$cvsd, label = sprintf("%s CV SE", method))

  for (rule in c("lambda.min", "lambda.1se")) {
    app_method <- paste(method, rule)
    app_coef <- app$coefficients[app$coefficients$Method == app_method, "Coefficient"]
    direct_coef <- as.numeric(as.matrix(stats::coef(direct, s = direct[[rule]]))[, 1])
    assert_true(length(app_coef) == length(direct_coef), sprintf("%s coefficient length should match glmnet", app_method))
    assert_close(app_coef, direct_coef, tolerance = 1e-9, label = sprintf("%s coefficients", app_method))

    index <- which.min(abs(direct$lambda - direct[[rule]]))
    app_summary <- app$summary[app$summary$Method == method & app$summary[["lambda rule"]] == rule, , drop = FALSE]
    assert_true(nrow(app_summary) == 1L, sprintf("%s summary row should exist", app_method))
    assert_identical(app_summary[["lambda"]], format_decimal3(direct[[rule]]), sprintf("%s displayed lambda", app_method))
    assert_identical(app_summary[["CV MSE"]], format_decimal3(direct$cvm[[index]]), sprintf("%s displayed CV MSE", app_method))
  }
}

settings_en <- app$cv_settings[app$cv_settings$Method == "Elastic Net", , drop = FALSE]
assert_true(nrow(settings_en) == 1L, "Elastic Net settings row should exist")
assert_identical(settings_en[["Selected alpha"]], format_decimal3(best_elastic$alpha), "Elastic Net selected alpha")

message("Penalized regression validation passed.")
