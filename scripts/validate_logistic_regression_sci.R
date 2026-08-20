all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_logistic_regression_sci.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source("R/app_bootstrap.R")
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

expect_close <- function(actual, expected, tolerance = 1e-7, label = "value") {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (length(actual) != length(expected) || any(!is.finite(actual) != !is.finite(expected)) ||
      any(abs(actual[is.finite(actual)] - expected[is.finite(expected)]) > tolerance)) {
    stop(sprintf("%s mismatch. Actual: %s; expected: %s", label, paste(actual, collapse = ", "), paste(expected, collapse = ", ")), call. = FALSE)
  }
}

variable_info_for <- function(data, outcome = "y", outcome_measurement = "binary") {
  measurements <- rep("continuous", ncol(data))
  measurements[names(data) == outcome] <- outcome_measurement
  measurements[vapply(data, is.factor, logical(1)) & names(data) != outcome] <- "category"
  data.frame(name = names(data), measurement = measurements, stringsAsFactors = FALSE)
}

message("Checking binary logistic coefficients, likelihood fit, pseudo-R2, EPV, and apparent performance...")
set.seed(7301)
n <- 700L
binary <- data.frame(
  x = stats::rnorm(n),
  z = stats::rnorm(n),
  group = factor(sample(c("A", "B", "C"), n, replace = TRUE))
)
linear <- -0.45 + 0.8 * binary$x - 0.5 * binary$z + 0.35 * (binary$group == "B") - 0.25 * (binary$group == "C")
binary$y <- factor(stats::rbinom(n, 1, stats::plogis(linear)), levels = 0:1)
binary_info <- variable_info_for(binary)
binary_result <- prepare_logistic_analysis_results(binary, "y", c("x", "z", "group"), variable_info = binary_info)[[1L]]
binary_reference <- stats::glm(y ~ x + z + group, data = binary, family = stats::binomial())
binary_null <- stats::glm(y ~ 1, data = binary, family = stats::binomial())
reference_coef <- summary(binary_reference)$coefficients
matched <- match(rownames(reference_coef), binary_result$coef_table$Term)
stopifnot(all(!is.na(matched)), identical(binary_result$method, "Binary logistic regression"), isTRUE(binary_result$convergence$ok))
expect_close(binary_result$coef_table$B[matched], reference_coef[, 1L], label = "binary B")
expect_close(binary_result$coef_table$SE[matched], reference_coef[, 2L], label = "binary SE")
expect_close(binary_result$coef_table$p[matched], reference_coef[, 4L], label = "binary Wald p")
critical <- stats::qnorm(.975)
expect_close(binary_result$coef_table$LLCI[matched], exp(reference_coef[, 1L] - critical * reference_coef[, 2L]), label = "binary OR LLCI")
expect_close(binary_result$coef_table$ULCI[matched], exp(reference_coef[, 1L] + critical * reference_coef[, 2L]), label = "binary OR ULCI")
ll_full <- as.numeric(stats::logLik(binary_reference))
ll_null <- as.numeric(stats::logLik(binary_null))
lr <- 2 * (ll_full - ll_null)
expect_close(binary_result$fit$chisq, lr, label = "binary LR chi-square")
expect_close(binary_result$fit$p, stats::pchisq(lr, binary_result$fit$df, lower.tail = FALSE), label = "binary LR p")
cox <- 1 - exp((2 / n) * (ll_null - ll_full))
max_cox <- 1 - exp((2 / n) * ll_null)
expect_close(binary_result$fit$r2[c("cox_snell", "nagelkerke", "mcfadden")], c(cox, cox / max_cox, 1 - ll_full / ll_null), label = "binary pseudo R2")
parameter_df <- qr(stats::model.matrix(binary_reference))$rank - 1L
expect_close(binary_result$epv$epv, min(table(binary$y)) / parameter_df, label = "binary class-per-parameter")
probability <- stats::fitted(binary_reference)
event <- as.integer(binary$y == "1")
n1 <- sum(event == 1L)
n0 <- sum(event == 0L)
auc <- (sum(rank(probability, ties.method = "average")[event == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
expect_close(binary_result$performance[["AUC (apparent)"]], auc, label = "binary AUC")
expect_close(binary_result$performance[["Brier score (apparent)"]], mean((event - probability)^2), label = "binary Brier")
expect_close(binary_result$performance[["Tjur R² (apparent)"]], mean(probability[event == 1L]) - mean(probability[event == 0L]), label = "binary Tjur R2")
stopifnot(any(grepl("large-sample Wald", binary_result$notes, fixed = TRUE)))

message("Checking observed two-level routing and final-model complete-case hierarchy...")
nominal_two_info <- binary_info
nominal_two_info$measurement[nominal_two_info$name == "y"] <- "category"
nominal_two <- prepare_logistic_analysis_results(binary, "y", "x", variable_info = nominal_two_info)[[1L]]
stopifnot(identical(nominal_two$method, "Binary logistic regression"), any(grepl("Two outcome levels remained", nominal_two$notes, fixed = TRUE)))

hierarchical <- binary
hierarchical$z[c(4, 18, 55, 211, 390)] <- NA_real_
hierarchical_info <- variable_info_for(hierarchical)
hierarchical_result <- prepare_logistic_analysis_results(hierarchical, "y", "x", "z", variable_info = hierarchical_info)
complete <- hierarchical[stats::complete.cases(hierarchical[, c("y", "x", "z")]), , drop = FALSE]
reference_step1 <- stats::glm(y ~ x, complete, family = stats::binomial())
reference_step2 <- stats::glm(y ~ x + z, complete, family = stats::binomial())
expected_delta <- 2 * (as.numeric(stats::logLik(reference_step2)) - as.numeric(stats::logLik(reference_step1)))
expected_df <- attr(stats::logLik(reference_step2), "df") - attr(stats::logLik(reference_step1), "df")
stopifnot(length(hierarchical_result) == 2L, all(vapply(hierarchical_result, `[[`, numeric(1), "n") == nrow(complete)))
expect_close(hierarchical_result[[2L]]$delta_chisq, expected_delta, label = "hierarchical LR delta")
expect_close(hierarchical_result[[2L]]$delta_p, stats::pchisq(expected_delta, expected_df, lower.tail = FALSE), label = "hierarchical LR delta p")

message("Checking rank-deficiency gate and unused factor levels...")
rank_data <- binary[, c("y", "x")]
rank_data$x_duplicate <- 2 * rank_data$x
rank_info <- variable_info_for(rank_data)
rank_result <- prepare_logistic_analysis_results(rank_data, "y", c("x", "x_duplicate"), variable_info = rank_info)
stopifnot(length(rank_result) == 0L, any(grepl("not uniquely estimable", attr(rank_result, "skipped")$Message, fixed = TRUE)))

factor_data <- data.frame(
  y = factor(c(0, 1, 0, 1, 0, 1, 0, 1, NA, NA), levels = 0:1),
  x = c(-1.3, -.8, -.4, -.1, .2, .5, .9, 1.4, 1.8, 2.1),
  group = factor(c(rep(c("A", "B"), 4L), "C", "C"), levels = c("A", "B", "C"))
)
factor_info <- variable_info_for(factor_data)
factor_result <- prepare_logistic_analysis_results(factor_data, "y", c("x", "group"), variable_info = factor_info)[[1L]]
stopifnot(!any(grepl("groupC", factor_result$coef_table$Term, fixed = TRUE)))

message("Checking cumulative-logit coefficients and nested nominal-effects proportional-odds test...")
set.seed(7302)
ordinal_n <- 1200L
ordinal_data <- data.frame(x = stats::rnorm(ordinal_n), z = stats::rnorm(ordinal_n))
latent <- .75 * ordinal_data$x - .45 * ordinal_data$z + stats::rlogis(ordinal_n)
ordinal_data$y <- ordered(cut(latent, breaks = c(-Inf, -.7, .35, Inf), labels = c("Low", "Mid", "High")))
ordinal_info <- variable_info_for(ordinal_data, outcome_measurement = "ordered")
ordinal_result <- prepare_logistic_analysis_results(ordinal_data, "y", "x", "z", variable_info = ordinal_info)
stopifnot(length(ordinal_result) == 2L, all(vapply(ordinal_result, function(result) identical(result$method, "Ordinal logistic regression"), logical(1))))
ordinal_reference <- ordinal::clm(y ~ x + z, data = ordinal_data, link = "logit", Hess = TRUE)
ordinal_alternative <- ordinal::clm(y ~ x + z, nominal = ~ x + z, data = ordinal_data, link = "logit", Hess = TRUE)
ordinal_lr <- 2 * (as.numeric(stats::logLik(ordinal_alternative)) - as.numeric(stats::logLik(ordinal_reference)))
ordinal_df <- attr(stats::logLik(ordinal_alternative), "df") - attr(stats::logLik(ordinal_reference), "df")
final_ordinal <- ordinal_result[[2L]]
expect_close(final_ordinal$coef_table$B[match(names(ordinal_reference$beta), final_ordinal$coef_table$Term)], ordinal_reference$beta, label = "ordinal B")
expect_close(final_ordinal$coef_table$SE[match(names(ordinal_reference$beta), final_ordinal$coef_table$Term)], sqrt(diag(stats::vcov(ordinal_reference)))[names(ordinal_reference$beta)], label = "ordinal SE")
expect_close(final_ordinal$parallel$chisq, ordinal_lr, label = "proportional odds LR")
expect_close(final_ordinal$parallel$p, stats::pchisq(ordinal_lr, ordinal_df, lower.tail = FALSE), label = "proportional odds p")
stopifnot(identical(final_ordinal$parallel$basis, "Final hierarchical model"), grepl("ordinal", logistic_package_label(final_ordinal), fixed = TRUE))

message("Checking category-table order for character ordinal outcomes...")
character_ordinal <- ordinal_data
character_ordinal$y <- as.character(character_ordinal$y)
character_table <- data.frame(
  name = "y", var_label = "Severity", reference = "",
  value_1 = "Low", label_1 = "Low",
  value_2 = "Mid", label_2 = "Mid",
  value_3 = "High", label_3 = "High",
  stringsAsFactors = FALSE, check.names = FALSE
)
character_result <- prepare_logistic_analysis_results(
  character_ordinal, "y", c("x", "z"), variable_info = ordinal_info, category_table = character_table
)[[1L]]
stopifnot(identical(character_result$dependent_levels, c("Low", "Mid", "High")))

message("Checking non-proportional-odds fallback and hierarchical family consistency...")
set.seed(7303)
nonpo_n <- 1600L
nonpo <- data.frame(x = stats::rnorm(nonpo_n), z = stats::rnorm(nonpo_n))
eta_mid <- 1.8 * nonpo$x - 1.3 * nonpo$z
eta_high <- -1.5 * nonpo$x + 1.6 * nonpo$z
denominator <- 1 + exp(eta_mid) + exp(eta_high)
probabilities <- cbind(1 / denominator, exp(eta_mid) / denominator, exp(eta_high) / denominator)
draw <- vapply(seq_len(nonpo_n), function(index) sample.int(3L, 1L, prob = probabilities[index, ]), integer(1))
nonpo$y <- ordered(c("Low", "Mid", "High")[draw], levels = c("Low", "Mid", "High"))
nonpo_info <- variable_info_for(nonpo, outcome_measurement = "ordered")
nonpo_result <- prepare_logistic_analysis_results(nonpo, "y", "x", "z", variable_info = nonpo_info)
stopifnot(length(nonpo_result) == 2L, nonpo_result[[2L]]$parallel$p < .05)
stopifnot(all(vapply(nonpo_result, function(result) identical(result$method, "Multinomial logistic regression"), logical(1))))
stopifnot(all(vapply(nonpo_result, function(result) isTRUE(result$ordinal_fallback), logical(1))))
nonpo_reference <- nnet::multinom(y ~ x + z, data = nonpo, trace = FALSE)
reference_matrix <- summary(nonpo_reference)$coefficients
final_table <- nonpo_result[[2L]]$coef_table
for (outcome in rownames(reference_matrix)) {
  for (term in colnames(reference_matrix)) {
    row <- final_table$Outcome == outcome & final_table$Term == term
    expect_close(final_table$B[row], reference_matrix[outcome, term], tolerance = 1e-6, label = paste("multinomial B", outcome, term))
  }
}

message("Checking convergence contracts, UI visibility, and Excel export...")
stopifnot(!isTRUE(logistic_model_convergence(structure(list(converged = FALSE), class = "glm"))$ok))
stopifnot(!isTRUE(logistic_model_convergence(structure(list(convergence = 1L), class = "multinom"))$ok))
html <- htmltools::renderTags(logistic_results_panel(ordinal_result, variable_table = ordinal_info))$html
stopifnot(grepl("Apparent model performance", html, fixed = TRUE), grepl("Convergence", html, fixed = TRUE), grepl("Final hierarchical model", html, fixed = TRUE))
xlsx <- tempfile(fileext = ".xlsx")
save_logistic_excel_file(binary_result |> list(), xlsx, variable_table = binary_info)
stopifnot(all(c("Model overview", "Assumption review", "Apparent performance") %in% openxlsx::getSheetNames(xlsx)))

message("Logistic regression SCI validation passed.")
