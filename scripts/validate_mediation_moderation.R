script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) "scripts/validate_mediation_moderation.R"
)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)
options(statedu.output_decimal_digits = 3L)

suppressPackageStartupMessages(library(shiny))
tags <- htmltools::tags
tagList <- htmltools::tagList
source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "result_labels.R"))
source(file.path(repo_root, "R", "setup_analysis_ui.R"))
source(file.path(repo_root, "R", "result_table_ui.R"))
source(file.path(repo_root, "R", "result_panels_ui.R"))
source(file.path(repo_root, "R", "result_coefficients.R"))
source(file.path(repo_root, "R", "analysis_regression.R"))
source(file.path(repo_root, "R", "setup_mediation_moderation_ui.R"))

assert_close <- function(actual, expected, tolerance = 1e-8, label = "value") {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (length(actual) != length(expected) || any(!is.finite(actual)) ||
      any(!is.finite(expected)) || any(abs(actual - expected) > tolerance)) {
    stop(sprintf(
      "%s mismatch: actual=%s, expected=%s",
      label,
      paste(signif(actual, 12), collapse = ", "),
      paste(signif(expected, 12), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

numeric_cell <- function(value) {
  value <- trimws(as.character(value %||% ""))
  value <- sub("^<\\s*", "", value)
  value <- sub("^\\.", "0.", value)
  suppressWarnings(as.numeric(value))
}

effect_row <- function(table, effect, path = NULL) {
  keep <- as.character(table$Effect) == effect
  if (!is.null(path)) keep <- keep & as.character(table$Path) == path
  rows <- table[keep, , drop = FALSE]
  if (nrow(rows) != 1L) {
    stop(sprintf("Expected one effect row for '%s' (%s); found %s.", effect, path %||% "", nrow(rows)), call. = FALSE)
  }
  rows
}

manual_bc_ci <- function(point, values, conf = .95) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  alpha <- (1 - conf) / 2
  prop_less <- mean(values < point)
  prop_less <- min(max(prop_less, .5 / length(values)), 1 - .5 / length(values))
  z0 <- stats::qnorm(prop_less)
  probs <- stats::pnorm(2 * z0 + stats::qnorm(c(alpha, 1 - alpha)))
  as.numeric(stats::quantile(values, probs = probs, names = FALSE, type = 6))
}

manual_percentile_ci <- function(values, conf = .95) {
  alpha <- (1 - conf) / 2
  as.numeric(stats::quantile(values, probs = c(alpha, 1 - alpha), names = FALSE, type = 6))
}

manual_boot_p <- function(values) {
  values <- values[is.finite(values)]
  min(1, 2 * min((sum(values <= 0) + 1) / (length(values) + 1),
                 (sum(values >= 0) + 1) / (length(values) + 1)))
}

variable_info_for <- function(data) {
  data.frame(
    name = names(data),
    var_label = names(data),
    role = "",
    measurement = "continuous",
    stringsAsFactors = FALSE
  )
}

message("Checking simple mediation against independent OLS equations...")
set.seed(20260820)
n <- 180L
mediation_data <- data.frame(
  X = stats::rnorm(n),
  C = stats::rnorm(n)
)
mediation_data$M <- .35 + .62 * mediation_data$X + .28 * mediation_data$C + stats::rnorm(n, sd = .72)
mediation_data$Y <- -.15 + .24 * mediation_data$X + .71 * mediation_data$M - .19 * mediation_data$C + stats::rnorm(n, sd = .81)
mediation_data$M[c(7L, 29L)] <- NA_real_
mediation_data$Y[[91L]] <- NA_real_

roles <- list(y = "Y", x = "X", mediators = "M", w = character(0), covariates = "C")
complete_data <- mediation_data[stats::complete.cases(mediation_data[c("Y", "X", "M", "C")]), , drop = FALSE]
fit_m <- stats::lm(M ~ X + C, data = complete_data)
fit_y <- stats::lm(Y ~ X + M + C, data = complete_data)
expected_effects <- c(
  Direct = stats::coef(fit_y)[["X"]],
  `Indirect: X -> M -> Y` = stats::coef(fit_m)[["X"]] * stats::coef(fit_y)[["M"]]
)
expected_effects <- c(
  expected_effects,
  `Total indirect` = expected_effects[["Indirect: X -> M -> Y"]],
  Total = sum(expected_effects)
)

base <- mediation_moderation_fit_focal(
  mediation_data, roles, focal = "X", structure = "single", model = "4",
  variable_info = variable_info_for(mediation_data), boot_r = 200L,
  analysis_method = "process_ols", residual_diagnostics = FALSE, auto_method = FALSE
)
stopifnot(identical(base$n, nrow(complete_data)))
assert_close(base$effects[names(expected_effects)], expected_effects, label = "simple mediation effects")
assert_close(stats::coef(base$models[["m_M"]]), stats::coef(fit_m), label = "mediator model coefficients")
assert_close(stats::coef(base$models$y), stats::coef(fit_y), label = "outcome model coefficients")

message("Checking BC bootstrap estimates, confidence limits, p values, and replicate counts...")
boot_r <- 240L
boot_seed <- 8491L
boot_result <- mediation_moderation_boot_effects(
  mediation_data, roles, focal = "X", structure = "single", model = "4",
  mean_center = FALSE, boot_r = boot_r, seed = boot_seed,
  variable_info = variable_info_for(mediation_data), analysis_method = "process_ols",
  ci_method = "bias_corrected", residual_diagnostics = FALSE, auto_method = FALSE
)

set.seed(boot_seed)
manual_boot <- matrix(NA_real_, nrow = boot_r, ncol = 4L,
                      dimnames = list(NULL, names(expected_effects)))
for (index in seq_len(boot_r)) {
  sample_rows <- sample.int(nrow(complete_data), nrow(complete_data), replace = TRUE)
  sample_data <- complete_data[sample_rows, , drop = FALSE]
  boot_m <- stats::lm.fit(stats::model.matrix(~ X + C, sample_data), sample_data$M)
  boot_y <- stats::lm.fit(stats::model.matrix(~ X + M + C, sample_data), sample_data$Y)
  direct <- boot_y$coefficients[["X"]]
  indirect <- boot_m$coefficients[["X"]] * boot_y$coefficients[["M"]]
  manual_boot[index, ] <- c(direct, indirect, indirect, direct + indirect)
}

indirect_path <- "X-->M-->Y"
indirect_row <- effect_row(boot_result$effect_table, "Indirect effect", indirect_path)
manual_indirect <- manual_boot[, "Indirect: X -> M -> Y"]
manual_ci <- manual_bc_ci(expected_effects[["Indirect: X -> M -> Y"]], manual_indirect)
assert_close(numeric_cell(indirect_row$Estimate), expected_effects[["Indirect: X -> M -> Y"]], .00051, "displayed indirect estimate")
assert_close(numeric_cell(indirect_row$`Boot SE`), stats::sd(manual_indirect), .00051, "BC bootstrap SE")
assert_close(numeric_cell(indirect_row$LLCI), manual_ci[[1L]], .00051, "BC lower CI")
assert_close(numeric_cell(indirect_row$ULCI), manual_ci[[2L]], .00051, "BC upper CI")
assert_close(numeric_cell(indirect_row$`Boot p`), manual_boot_p(manual_indirect), .00051, "bootstrap p")
diagnostic <- boot_result$effect_bootstrap_diagnostics[
  boot_result$effect_bootstrap_diagnostics$Effect == "Indirect effect" &
    boot_result$effect_bootstrap_diagnostics$Path == indirect_path, , drop = FALSE
]
stopifnot(nrow(diagnostic) == 1L, diagnostic$Requested[[1L]] == boot_r,
          diagnostic$Valid[[1L]] == boot_r, identical(diagnostic$Status[[1L]], "Adequate"))

message("Checking percentile bootstrap option independently...")
percentile_table <- mediation_moderation_effect_table(
  "4", "X", expected_effects, manual_boot, ci_method = "percentile",
  y = "Y", mediators = "M", variable_info = variable_info_for(mediation_data)
)
percentile_row <- effect_row(percentile_table, "Indirect effect", indirect_path)
percentile_ci <- manual_percentile_ci(manual_indirect)
assert_close(numeric_cell(percentile_row$LLCI), percentile_ci[[1L]], .00051, "percentile lower CI")
assert_close(numeric_cell(percentile_row$ULCI), percentile_ci[[2L]], .00051, "percentile upper CI")

message("Checking bootstrap reliability gate and non-estimable effect propagation...")
stopifnot(is.na(mediation_moderation_effect_sum(c(.2, NA_real_))))
stopifnot(identical(mediation_moderation_boot_status(19L, 100L), "Unreliable"))
stopifnot(identical(mediation_moderation_boot_status(60L, 100L), "Caution"))
stopifnot(identical(mediation_moderation_boot_status(80L, 100L), "Adequate"))
unreliable <- mediation_moderation_boot_summary(.2, c(rep(.2, 19L), rep(NA_real_, 81L)))
caution <- mediation_moderation_boot_summary(.2, c(rep(.2, 60L), rep(NA_real_, 40L)))
reliable <- mediation_moderation_boot_summary(.2, c(rep(.2, 80L), rep(NA_real_, 20L)))
stopifnot(all(is.na(unreliable[c("LLCI", "ULCI", "Boot p")])))
stopifnot(all(is.finite(caution[c("LLCI", "ULCI", "Boot p")])))
stopifnot(all(is.finite(reliable[c("LLCI", "ULCI", "Boot p")])))

message("Checking simple moderation, conditional slopes, and covariance algebra...")
set.seed(731)
mod_n <- 210L
moderation_data <- data.frame(X = stats::rnorm(mod_n), W = stats::rnorm(mod_n), C = stats::rnorm(mod_n))
moderation_data$Y <- .3 + .42 * moderation_data$X - .25 * moderation_data$W +
  .56 * moderation_data$X * moderation_data$W + .18 * moderation_data$C + stats::rnorm(mod_n, sd = .7)
moderation_roles <- list(y = "Y", x = "X", mediators = character(0), w = "W", covariates = "C")
moderation_base <- mediation_moderation_fit_focal(
  moderation_data, moderation_roles, focal = "X", structure = "none", model = "1",
  variable_info = variable_info_for(moderation_data), boot_r = 50L,
  analysis_method = "process_ols", residual_diagnostics = FALSE, auto_method = FALSE
)
moderation_lm <- stats::lm(Y ~ X * W + C, data = moderation_data)
assert_close(stats::coef(moderation_base$models$y), stats::coef(moderation_lm), label = "moderation coefficients")
path_result <- moderation_base$path_results[[1L]]
slopes <- mediation_moderation_simple_slopes_table(list(path_result))
stopifnot(is.data.frame(slopes), nrow(slopes) >= 3L)
w_value_column <- intersect(c("Moderator value", "Moderator.value", "W"), names(slopes))
stopifnot(length(w_value_column) == 1L)
w_values <- numeric_cell(slopes[[w_value_column[[1L]]]])
expected_slopes <- stats::coef(moderation_lm)[["X"]] + stats::coef(moderation_lm)[["X:W"]] * w_values
assert_close(numeric_cell(slopes$Effect), expected_slopes, .00051, "conditional slopes")
v <- stats::vcov(moderation_lm)
expected_se <- sqrt(v["X", "X"] + 2 * w_values * v["X", "X:W"] + w_values^2 * v["X:W", "X:W"])
assert_close(numeric_cell(slopes$SE), expected_se, .00051, "conditional slope SE")

message("Checking serial mediation path decomposition...")
set.seed(982)
serial_n <- 190L
serial_data <- data.frame(X = stats::rnorm(serial_n), C = stats::rnorm(serial_n))
serial_data$M1 <- .4 * serial_data$X + .2 * serial_data$C + stats::rnorm(serial_n, sd = .7)
serial_data$M2 <- .3 * serial_data$X + .5 * serial_data$M1 - .1 * serial_data$C + stats::rnorm(serial_n, sd = .75)
serial_data$Y <- .2 * serial_data$X + .35 * serial_data$M1 + .6 * serial_data$M2 + .15 * serial_data$C + stats::rnorm(serial_n, sd = .8)
serial_roles <- list(y = "Y", x = "X", mediators = c("M1", "M2"), w = character(0), covariates = "C")
serial_base <- mediation_moderation_fit_focal(
  serial_data, serial_roles, focal = "X", structure = "serial", model = "6",
  variable_info = variable_info_for(serial_data), boot_r = 50L,
  analysis_method = "process_ols", residual_diagnostics = FALSE, auto_method = FALSE
)
serial_m1 <- stats::lm(M1 ~ X + C, serial_data)
serial_m2 <- stats::lm(M2 ~ X + M1 + C, serial_data)
serial_y <- stats::lm(Y ~ X + M1 + M2 + C, serial_data)
serial_expected <- c(
  `Indirect: X -> M1 -> Y` = coef(serial_m1)[["X"]] * coef(serial_y)[["M1"]],
  `Indirect: X -> M2 -> Y` = coef(serial_m2)[["X"]] * coef(serial_y)[["M2"]],
  `Indirect: X -> M1 -> M2 -> Y` = coef(serial_m1)[["X"]] * coef(serial_m2)[["M1"]] * coef(serial_y)[["M2"]]
)
assert_close(serial_base$effects[names(serial_expected)], serial_expected, label = "serial indirect effects")
assert_close(serial_base$effects[["Total indirect"]], sum(serial_expected), label = "serial total indirect")

message("Checking reproducibility and result/export visibility...")
repeat_result <- mediation_moderation_boot_effects(
  mediation_data, roles, focal = "X", structure = "single", model = "4",
  mean_center = FALSE, boot_r = boot_r, seed = boot_seed,
  variable_info = variable_info_for(mediation_data), analysis_method = "process_ols",
  ci_method = "bias_corrected", residual_diagnostics = FALSE, auto_method = FALSE
)
stopifnot(identical(boot_result$effect_table, repeat_result$effect_table))
stopifnot(identical(boot_result$effect_bootstrap_diagnostics, repeat_result$effect_bootstrap_diagnostics))

full_result <- run_mediation_moderation_analysis(
  data = mediation_data, roles = roles, mediator_arrangement = "parallel",
  moderated_paths = character(0), boot_r = 80L, seed = 2026L,
  analysis_method = "process_ols", ci_method = "bias_corrected",
  residual_diagnostics = FALSE, auto_method = FALSE,
  variable_info = variable_info_for(mediation_data), language = "en"
)
rendered <- htmltools::renderTags(mediation_moderation_result_ui(full_result, language = "en"))$html
stopifnot(grepl("Bootstrap diagnostics", rendered, fixed = TRUE))
stopifnot(grepl(">p</th>", rendered, fixed = TRUE))
export_tables <- mediation_moderation_export_tables(full_result)
stopifnot("Bootstrap diagnostics" %in% names(export_tables))
stopifnot("Boot p" %in% names(export_tables[["Bootstrap effects"]]))

message("Mediation/moderation validation passed.")
