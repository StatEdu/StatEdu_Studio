script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/benchmark_analysis_pipeline_all.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))
statedu_apply_preferences(statedu_initial_preferences())
options(statedu.app_language = normalize_app_language(Sys.getenv("STATEDU_APP_LANGUAGE", "ko")))

repetitions <- max(1L, suppressWarnings(as.integer(Sys.getenv("STATEDU_BENCHMARK_REPETITIONS", "3"))))
if (is.na(repetitions)) repetitions <- 3L

make_info <- function(names, measurements) {
  data.frame(name = names, measurement = measurements, stringsAsFactors = FALSE)
}

elapsed <- function(expression) {
  started <- proc.time()[["elapsed"]]
  value <- force(expression)
  list(value = value, seconds = unname(proc.time()[["elapsed"]] - started))
}

object_md5 <- function(value) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(value, path, version = 3)
  unname(tools::md5sum(path)[[1L]])
}

render_result <- function(ui) {
  tags <- htmltools::renderTags(ui)
  nchar(tags$html, type = "bytes") + nchar(tags$head, type = "bytes")
}

benchmark_case <- function(name, calculate, make_ui) {
  gc(FALSE)
  cold <- elapsed(calculate())
  warm_seconds <- numeric(repetitions)
  reference <- NULL
  for (index in seq_len(repetitions)) {
    timed <- elapsed(calculate())
    warm_seconds[[index]] <- timed$seconds
    reference <- timed$value
  }
  cold_seconds <- cold$seconds
  cold$value <- NULL
  result_md5 <- object_md5(reference)
  gc(FALSE)
  ui_build <- elapsed(make_ui(reference))
  ui_render <- elapsed(render_result(ui_build$value))
  warm_ui_build_seconds <- numeric(repetitions)
  warm_ui_render_seconds <- numeric(repetitions)
  warm_html_bytes <- integer(repetitions)
  for (index in seq_len(repetitions)) {
    warm_ui <- elapsed(make_ui(reference))
    warm_render <- elapsed(render_result(warm_ui$value))
    warm_ui_build_seconds[[index]] <- warm_ui$seconds
    warm_ui_render_seconds[[index]] <- warm_render$seconds
    warm_html_bytes[[index]] <- as.integer(warm_render$value)
  }
  data.frame(
    analysis = name,
    cold_analysis_seconds = cold_seconds,
    warm_analysis_median_seconds = stats::median(warm_seconds),
    warm_analysis_min_seconds = min(warm_seconds),
    table_ui_build_seconds = ui_build$seconds,
    table_html_render_seconds = ui_render$seconds,
    table_total_seconds = ui_build$seconds + ui_render$seconds,
    warm_table_ui_build_median_seconds = stats::median(warm_ui_build_seconds),
    warm_table_html_render_median_seconds = stats::median(warm_ui_render_seconds),
    warm_table_total_median_seconds = stats::median(warm_ui_build_seconds + warm_ui_render_seconds),
    html_bytes = as.integer(stats::median(warm_html_bytes)),
    result_md5 = result_md5,
    stringsAsFactors = FALSE
  )
}

set.seed(20260827L)
n <- 180L
group2 <- rep(c("Control", "Treatment"), each = n / 2L)
group3 <- rep(c("A", "B", "C"), length.out = n)
x1 <- stats::rnorm(n)
x2 <- stats::rnorm(n)
y <- 0.6 * x1 - 0.3 * x2 + ifelse(group2 == "Treatment", 0.45, 0) + stats::rnorm(n)

freq_data <- data.frame(group = group3, score = y, stringsAsFactors = FALSE)
freq_info <- make_info(names(freq_data), c("category", "continuous"))
ct_data <- data.frame(row = factor(group2), col = factor(ifelse(y > stats::median(y), "High", "Low")))
ct_info <- make_info(names(ct_data), c("binary", "binary"))
tt_data <- data.frame(y = y, group = group2, stringsAsFactors = FALSE)
tt_info <- make_info(names(tt_data), c("continuous", "binary"))
anc_data <- data.frame(y = y, group = group3, x = x1, stringsAsFactors = FALSE)
anc_info <- make_info(names(anc_data), c("continuous", "category", "continuous"))
paired_data <- data.frame(pre = x1, post = x1 + 0.35 + stats::rnorm(n, sd = 0.8))
paired_info <- make_info(names(paired_data), rep("continuous", 2L))
mixed_data <- data.frame(
  group = group2,
  pre = 50 + 5 * x1 + stats::rnorm(n),
  post = 50 + 5 * x1 + ifelse(group2 == "Treatment", 3, 1) + stats::rnorm(n),
  post2 = 50 + 5 * x1 + ifelse(group2 == "Treatment", 5, 1.5) + stats::rnorm(n),
  stringsAsFactors = FALSE
)
mixed_info <- make_info(names(mixed_data), c("binary", "continuous", "continuous", "continuous"))
cor_data <- data.frame(x1 = x1, x2 = x2, y = y)
cor_info <- make_info(names(cor_data), rep("continuous", 3L))
item_data <- as.data.frame(matrix(stats::rnorm(n * 6L), ncol = 6L))
names(item_data) <- paste0("item", seq_len(ncol(item_data)))
item_info <- make_info(names(item_data), rep("continuous", ncol(item_data)))
rater_base <- stats::rnorm(n)
rater_data <- data.frame(
  r1 = rater_base + stats::rnorm(n, sd = .5),
  r2 = rater_base + stats::rnorm(n, sd = .55),
  r3 = rater_base + stats::rnorm(n, sd = .6)
)
rater_info <- make_info(names(rater_data), rep("continuous", 3L))
reg_data <- data.frame(y = y, x1 = x1, x2 = x2)
reg_info <- make_info(names(reg_data), rep("continuous", 3L))
log_data <- data.frame(
  outcome = factor(stats::rbinom(n, 1L, stats::plogis(-.3 + .7 * x1 - .25 * x2))),
  x1 = x1,
  x2 = x2
)
log_info <- make_info(names(log_data), c("binary", "continuous", "continuous"))

cases <- list(
  list("Frequencies", function() prepare_frequencies_results(freq_data, names(freq_data), freq_info), frequencies_results_ui),
  list("Crosstabs", function() prepare_crosstab_results(ct_data, "row", "col", ct_info), crosstab_results_ui),
  list("t-test / ANOVA", function() prepare_ttest_anova_results(tt_data, "y", "group", tt_info, options = list(effect_size = TRUE, normality_enabled = FALSE, show_df = TRUE)), ttest_anova_results_ui),
  list("ANCOVA", function() prepare_ancova_results(anc_data, "y", "group", "x", anc_info, options = list(show_df = TRUE)), ancova_results_ui),
  list("Paired test", function() prepare_paired_results(paired_data, "pre", "post", paired_info, options = list(effect_size = TRUE)), paired_results_ui),
  list("Repeated-measures ANOVA", function() prepare_mixed_rm_anova_results(mixed_data, "group", c("pre", "post", "post2"), variable_info = mixed_info, options = list(assumption_check = TRUE, posthoc = TRUE, posthoc_adjustment = "holm", time_labels = c("Pre", "Post", "Post 2"))), mixed_rm_anova_results_ui),
  list("Nonparametric paired", function() prepare_nonparametric_paired_results(paired_data, "pre", "post", paired_info, options = list(effect_size = TRUE)), nonparametric_paired_results_ui),
  list("Correlation", function() prepare_correlation_results(cor_data, names(cor_data), cor_info, options = list(continuous_method = "pearson")), correlation_results_ui),
  list("Reliability", function() prepare_reliability_results(item_data, names(item_data), item_info), reliability_results_ui),
  list("Inter-rater agreement", function() prepare_interrater_agreement_results(rater_data, names(rater_data), rater_info, options = list(method = "icc", icc_model = "icc2")), interrater_agreement_results_ui),
  list("Factor analysis", function() prepare_factor_analysis_results(item_data, names(item_data), item_info, options = list(criterion = "fixed", n_factors = 2L, method = "pa", rotation = "varimax")), factor_analysis_results_ui),
  list("Principal components", function() prepare_pca_results(item_data, names(item_data), item_info, options = list(criterion = "eigen", rotation = "varimax")), pca_results_ui),
  list("Linear regression", function() prepare_regression_analysis_results(reg_data, "y", c("x1", "x2"), variable_info = reg_info), function(value) regression_results_panel(value$results)),
  list("Logistic regression", function() prepare_logistic_analysis_results(log_data, "outcome", c("x1", "x2"), variable_info = log_info), logistic_results_panel),
  list("Generalized linear model", function() prepare_generalized_analysis_result(reg_data, "y", c("x1", "x2"), family = "gaussian", se_type = "model", robust = FALSE, variable_info = reg_info), generalized_results_panel)
)

results <- lapply(cases, function(case) {
  message(sprintf("Benchmarking %s ...", case[[1L]]))
  tryCatch(
    benchmark_case(case[[1L]], case[[2L]], case[[3L]]),
    error = function(error) data.frame(
      analysis = case[[1L]],
      cold_analysis_seconds = NA_real_, warm_analysis_median_seconds = NA_real_, warm_analysis_min_seconds = NA_real_,
      table_ui_build_seconds = NA_real_, table_html_render_seconds = NA_real_, table_total_seconds = NA_real_,
      warm_table_ui_build_median_seconds = NA_real_, warm_table_html_render_median_seconds = NA_real_,
      warm_table_total_median_seconds = NA_real_,
      html_bytes = NA_integer_, result_md5 = NA_character_, error = conditionMessage(error), stringsAsFactors = FALSE
    )
  )
})

all_names <- unique(unlist(lapply(results, names), use.names = FALSE))
results <- lapply(results, function(result) {
  missing <- setdiff(all_names, names(result))
  for (name in missing) result[[name]] <- NA_character_
  result[all_names]
})
result <- do.call(rbind, results)
result$repetitions <- repetitions
result$commit <- tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)[[1L]], error = function(error) "")
print(result, row.names = FALSE)

output_path <- trimws(Sys.getenv("STATEDU_BENCHMARK_OUTPUT", ""))
if (nzchar(output_path)) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, output_path)
}
