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
binary_y <- sample(c("0", "1"), n, replace = TRUE)
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
  binary_y = binary_y,
  nominal_x = nominal_x,
  blank_ordinal = blank_ordinal,
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(data),
  measurement = c("continuous", "continuous", "continuous", "continuous", "ordinal", "ordinal", "binary", "binary", "category", "ordinal"),
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

binary_result <- prepare_correlation_results(
  data,
  variables = c("binary_x", "binary_y"),
  variable_info = variable_info,
  options = list(continuous_method = "auto", normality = TRUE, reason = TRUE)
)
expect_true(identical(binary_result$pairwise_table$Method[[1]], "Phi"), "Expected two binary variables to be labelled as Phi")
expect_true(grepl("two binary variables", binary_result$pairwise_table$Reason[[1]], fixed = TRUE), "Expected binary-binary reason to mention Phi selection")

kendall_result <- correlation_test_result(normal_x, normal_y, "kendall", "Kendall")
kendall_expected_ci <- {
  z <- atanh(kendall_result$coefficient)
  se <- sqrt(0.437 / (kendall_result$n - 4))
  critical <- stats::qnorm(0.975)
  tanh(c(z - critical * se, z + critical * se))
}
expect_true(
  isTRUE(all.equal(kendall_result$ci, kendall_expected_ci, tolerance = 1e-12)),
  "Expected Kendall tau CI to use Fieller z-transform standard error"
)
expect_true(
  all(is.na(correlation_ci(0.2, 4, method = "kendall"))),
  "Expected Kendall tau CI to be unavailable for n < 5"
)

mixed_result <- prepare_correlation_results(
  data,
  variables = c("normal_x", "binary_x", "nominal_x", "ordinal_text", "blank_ordinal"),
  variable_info = variable_info,
  options = list(continuous_method = "auto", normality = FALSE, reason = TRUE, latent_correlations = TRUE, p_ci = TRUE)
)
expect_true(any(mixed_result$pairwise_table$Method == "Polyserial"), "Expected latent continuous-binary/ordinal pairs to use polyserial")
expect_true(any(mixed_result$pairwise_table$Method == "Polychoric"), "Expected latent ordinal/binary pairs to use polychoric")
expect_true(any(mixed_result$pairwise_table$Method == "Eta"), "Expected continuous-nominal pair to retain Eta")
expect_true(any(mixed_result$pairwise_table$Method == "Cramer's V"), "Expected nominal categorical pairs to retain Cramer's V")
expect_true(is.data.frame(mixed_result$omitted_table) && any(mixed_result$omitted_table$Variable == "blank_ordinal"), "Expected sparse ordinal variable to be reported as omitted")
expect_true(isTRUE(mixed_result$options$normality), "Expected auto method to retain normality diagnostics for traceability")
expect_true(is.null(mixed_result$latent), "Expected latent correlations to replace the primary method set instead of rendering as a duplicate result set")

message("Checking latent-response correlation inference...")
set.seed(31415)
latent_n <- 500
latent_x <- stats::rnorm(latent_n)
latent_y <- 0.65 * latent_x + stats::rnorm(latent_n, sd = 0.75)
latent_y2 <- latent_x + stats::rnorm(latent_n, sd = 0.8)
latent_ord_4 <- ordered(cut(latent_y, breaks = stats::quantile(latent_y, probs = 0:4 / 4), include.lowest = TRUE))
latent_ord_3 <- ordered(cut(latent_y2, breaks = stats::quantile(latent_y2, probs = 0:3 / 3), include.lowest = TRUE))

polyserial_result <- correlation_polyserial_result(latent_x, latent_ord_4)
polyserial_oracle <- polycor::polyserial(latent_x, latent_ord_4, ML = FALSE, std.err = TRUE)
expect_true(is.finite(polyserial_result$coefficient), "Expected a finite polyserial estimate")
expect_true(isTRUE(all.equal(polyserial_result$coefficient, as.numeric(polyserial_oracle$rho), tolerance = 1e-12)), "Expected the reported polyserial estimate to match polycor")
expect_true(isTRUE(all.equal(polyserial_result$se, sqrt(as.numeric(polyserial_oracle$var[1, 1])), tolerance = 1e-12)), "Expected the reported polyserial SE to match polycor")
expect_true(is.finite(polyserial_result$se) && polyserial_result$se > 0, "Expected polyserial SE from the polycor estimator")
expect_true(all(is.finite(polyserial_result$ci)), "Expected a finite estimator-based polyserial CI")
expect_true(polyserial_result$ci[[1]] <= polyserial_result$coefficient && polyserial_result$coefficient <= polyserial_result$ci[[2]], "Expected polyserial estimate inside its CI")
expect_true(is.finite(polyserial_result$p), "Expected a polyserial Wald p-value from the estimator SE")
expect_true(identical(polyserial_result$ci_method, "polycor two-step asymptotic SE (thresholds treated as fixed); Fisher-z delta Wald CI and p"), "Expected polyserial CI method metadata")
expect_true(!isTRUE(all.equal(polyserial_result$ci, correlation_ci(polyserial_result$coefficient, polyserial_result$n), tolerance = 1e-10)), "Expected polyserial CI not to reuse the ordinary Pearson Fisher-z SE")
polyserial_expected_statistic <- atanh(polyserial_result$coefficient) / (polyserial_result$se / (1 - polyserial_result$coefficient^2))
expect_true(isTRUE(all.equal(polyserial_result$statistic, polyserial_expected_statistic, tolerance = 1e-12)), "Expected polyserial Wald statistic on the Fisher-z delta scale")
expect_true(isTRUE((polyserial_result$p < .05) == !(polyserial_result$ci[[1]] <= 0 && polyserial_result$ci[[2]] >= 0)), "Expected polyserial p-value and CI conclusion to agree")

polychoric_result <- correlation_polychoric_result(latent_ord_3, latent_ord_4)
polychoric_oracle <- polycor::polychor(latent_ord_3, latent_ord_4, ML = FALSE, std.err = TRUE)
expect_true(is.finite(polychoric_result$coefficient), "Expected a finite polychoric estimate")
expect_true(isTRUE(all.equal(polychoric_result$coefficient, as.numeric(polychoric_oracle$rho), tolerance = 1e-12)), "Expected the reported polychoric estimate to match polycor")
expect_true(isTRUE(all.equal(polychoric_result$se, sqrt(as.numeric(polychoric_oracle$var[1, 1])), tolerance = 1e-12)), "Expected the reported polychoric SE to match polycor")
expect_true(is.finite(polychoric_result$se) && polychoric_result$se > 0, "Expected polychoric SE from the polycor estimator")
expect_true(all(is.finite(polychoric_result$ci)), "Expected a finite estimator-based polychoric CI")
expect_true(polychoric_result$ci[[1]] <= polychoric_result$coefficient && polychoric_result$coefficient <= polychoric_result$ci[[2]], "Expected polychoric estimate inside its CI")
expect_true(is.finite(polychoric_result$p), "Expected a polychoric Wald p-value from the estimator SE")
expect_true(!isTRUE(all.equal(polychoric_result$ci, correlation_ci(polychoric_result$coefficient, polychoric_result$n), tolerance = 1e-10)), "Expected polychoric CI not to reuse the ordinary Pearson Fisher-z SE")
expect_true(isTRUE((polychoric_result$p < .05) == !(polychoric_result$ci[[1]] <= 0 && polychoric_result$ci[[2]] >= 0)), "Expected polychoric p-value and CI conclusion to agree")

latent_binary_1 <- ordered(ifelse(latent_x > stats::median(latent_x), "1", "0"), levels = c("0", "1"))
latent_binary_2 <- ordered(ifelse(latent_y2 > stats::median(latent_y2), "1", "0"), levels = c("0", "1"))
tetrachoric_result <- correlation_polychoric_result(latent_binary_1, latent_binary_2, method = "tetrachoric", label = "Tetrachoric")
tetrachoric_oracle <- polycor::polychor(latent_binary_1, latent_binary_2, ML = FALSE, std.err = TRUE)
expect_true(isTRUE(all.equal(tetrachoric_result$coefficient, as.numeric(tetrachoric_oracle$rho), tolerance = 1e-12)), "Expected the tetrachoric estimate to match polycor")
expect_true(isTRUE(all.equal(tetrachoric_result$se, sqrt(as.numeric(tetrachoric_oracle$var[1, 1])), tolerance = 1e-12)), "Expected the tetrachoric SE to match polycor")
expect_true(isTRUE((tetrachoric_result$p < .05) == !(tetrachoric_result$ci[[1]] <= 0 && tetrachoric_result$ci[[2]] >= 0)), "Expected tetrachoric p-value and CI conclusion to agree")

consistency_probe <- correlation_polycor_inference(list(rho = 0.5, var = matrix(0.0576, 1, 1)))
expect_true(consistency_probe$p >= .05 && consistency_probe$ci[[1]] <= 0 && consistency_probe$ci[[2]] >= 0, "Expected Fisher-z delta p-value and CI to agree for the consistency probe")
invalid_probes <- list(
  list(rho = 0.9999, var = matrix(Inf, 1, 1)),
  list(rho = 0.5, var = matrix(0, 1, 1)),
  list(rho = 0.5, var = matrix(-0.01, 1, 1)),
  list(rho = 0.5, var = matrix(NA_real_, 1, 1)),
  list(rho = numeric(0), var = matrix(0.01, 1, 1))
)
for (probe in invalid_probes) {
  inference <- correlation_polycor_inference(probe)
  expect_true(identical(inference$inference_status, "unavailable"), "Expected malformed/boundary polycor inference to fail closed")
  expect_true(is.na(inference$p) && all(is.na(inference$ci)), "Expected no generic Fisher fallback for malformed/boundary polycor inference")
  expect_true(nzchar(inference$inference_reason), "Expected an explicit unavailable-inference reason")
}
latent_ui_html <- paste(as.character(correlation_results_ui(mixed_result)), collapse = "")
expect_true(
  grepl("고정 임계값", latent_ui_html, fixed = TRUE) &&
    grepl('data-result-table-role="appendix"', latent_ui_html, fixed = TRUE),
  "Expected the UI-language appendix note to identify the fixed-threshold two-step inference limitation"
)

localized_appendix <- correlation_appendix_localize_table(
  data.frame(
    Normality = c("satisfied", "not satisfied"),
    Reason = c(
      "Omitted because fewer than three valid values were available.",
      "Omitted because fewer than two unique values were available."
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ),
  "ko"
)
expect_true(identical(localized_appendix$정규성, c("충족", "미충족")), "Expected correlation normality appendix values to follow the Korean UI language")
localized_normality_headers <- correlation_appendix_localize_table(
  data.frame(
    Variable = "x1",
    N = 10,
    Skewness = 0,
    Kurtosis = 0,
    Normality = "satisfied",
    check.names = FALSE
  ),
  "ko"
)
expect_true(
  identical(names(localized_normality_headers), c("변수", "N", "왜도", "첨도", "정규성")),
  "Expected correlation normality appendix headers to follow the Korean UI language"
)
method_note_ko <- correlation_method_abbreviation_note(
  data.frame(Variable = c("x1", "x2"), x1 = c("", "r"), x2 = c("", ""), check.names = FALSE),
  "ko"
)
expect_true(
  identical(method_note_ko, "r = Pearson 상관"),
  "Expected correlation appendix method note to follow the Korean UI language"
)
expect_true(
  identical(localized_appendix$사유, c("유효값이 3개 미만이어서 제외했습니다.", "서로 다른 값이 2개 미만이어서 제외했습니다.")),
  "Expected correlation omitted-variable appendix reasons to follow the Korean UI language"
)

message("All correlation auto validations passed.")
