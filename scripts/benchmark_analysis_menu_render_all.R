script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/benchmark_analysis_menu_render_all.R"
}
repo_root <- normalizePath(
  file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE
)
setwd(repo_root)
if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(
    Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE
  )))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))
statedu_apply_preferences(statedu_default_preferences())

language <- normalize_app_language(Sys.getenv("STATEDU_BENCHMARK_LANGUAGE", "ko"))
repetitions <- suppressWarnings(as.integer(Sys.getenv(
  "STATEDU_BENCHMARK_REPETITIONS", "4"
)))
if (!is.finite(repetitions) || repetitions < 2L) repetitions <- 4L

wrap <- function(value) tab_panel_content(value)
specifications <- list(
  list("Frequencies / Descriptives", function() wrap(frequencies_tab_panel(statedu_ui_label("frequencies", language), language))),
  list("Crosstabs", function() wrap(crosstab_tab_panel(language))),
  list("t-test / ANOVA", function() wrap(ttest_anova_tab_panel(statedu_ui_label("ttest_anova", language), language))),
  list("ANCOVA", function() wrap(ancova_tab_panel(statedu_ui_label("ancova", language), language))),
  list("Repeated-measures ANOVA", function() wrap(mixed_rm_anova_tab_panel(statedu_ui_label("mixed_rm_anova", language), language))),
  list("Nonparametric Tests", function() wrap(nonparametric_tab_panel(statedu_ui_label("nonparametric", language), language))),
  list("Paired test", function() wrap(paired_tab_panel(statedu_ui_label("paired", language), language))),
  list("Nonparametric Paired", function() wrap(nonparametric_paired_tab_panel(statedu_ui_label("nonparametric_paired", language), language))),
  list("Correlation", function() wrap(correlation_tab_panel(statedu_ui_label("correlation", language), language))),
  list("Factor Analysis", function() wrap(factor_analysis_tab_panel(statedu_ui_label("factor_analysis", language), language))),
  list("Principal Components", function() wrap(pca_tab_panel(statedu_ui_label("pca", language), language))),
  list("Reliability", function() wrap(reliability_tab_panel(statedu_ui_label("reliability", language), language))),
  list("Inter-rater Agreement", function() wrap(interrater_agreement_tab_panel(statedu_ui_label("interrater_agreement", language), language))),
  list("Regression", function() wrap(hierarchical_tab_panel(statedu_ui_label("regression", language), language))),
  list("Mediation / Moderation", function() wrap(mediation_moderation_tab_panel(mediation_moderation_title(language), language))),
  list("Mediation / Moderation Custom Model", function() wrap(custom_model_canvas_tab_panel(custom_model_canvas_title(language), language))),
  list("CFA", function() wrap(structural_equation_tab_panel("cfa", language))),
  list("SEM", function() wrap(structural_equation_tab_panel("cbsem", language))),
  list("PLS-SEM", function() wrap(structural_equation_tab_panel("plssem", language))),
  list("SEM analysis recommendation", function() wrap(structural_automation_tab_panel(language))),
  list("Longitudinal / Panel Models", function() wrap(longitudinal_tab_panel(statedu_ui_label("longitudinal", language), language))),
  list("Generalized Linear Model (GLM)", function() wrap(generalized_tab_panel(statedu_ui_label("glm", language), language))),
  list("Logistic Regression", function() wrap(logistic_regression_tab_panel(language))),
  list("Complex-sample design", function() wrap(complex_sample_design_tab_panel(language))),
  list("Complex-sample frequencies", function() wrap(complex_sample_frequencies_tab_panel(language))),
  list("Complex-sample crosstabs", function() wrap(complex_sample_crosstabs_tab_panel(language))),
  list("Complex-sample t-test / ANOVA", function() wrap(complex_sample_ttest_anova_tab_panel(language))),
  list("Complex-sample correlation", function() wrap(complex_sample_correlation_tab_panel(language))),
  list("Complex-sample regression", function() wrap(complex_sample_regression_tab_panel(language))),
  list("Complex-sample logistic", function() wrap(complex_sample_logistic_tab_panel(language))),
  list("Complex-sample mediation / moderation", function() wrap(complex_sample_custom_model_tab_panel(language))),
  list("Survival setup", function() wrap(survival_setup_tab_panel(language))),
  list("Kaplan-Meier", function() wrap(survival_km_tab_panel(language))),
  list("Cox regression", function() wrap(survival_cox_tab_panel(language))),
  list("Competing risks", function() wrap(survival_competing_tab_panel(language)))
)

measure_one <- function(specification) {
  name <- specification[[1L]]
  builder <- specification[[2L]]
  samples <- lapply(seq_len(repetitions), function(index) {
    gc(FALSE)
    build_started <- proc.time()[["elapsed"]]
    ui <- builder()
    build_seconds <- unname(proc.time()[["elapsed"]] - build_started)
    render_started <- proc.time()[["elapsed"]]
    html <- htmltools::renderTags(ui)$html
    render_seconds <- unname(proc.time()[["elapsed"]] - render_started)
    list(
      build = build_seconds,
      render = render_seconds,
      bytes = nchar(html, type = "bytes")
    )
  })
  build <- vapply(samples, `[[`, numeric(1), "build")
  render <- vapply(samples, `[[`, numeric(1), "render")
  warm_index <- if (length(build) > 1L) 2:length(build) else 1L
  data.frame(
    analysis = name,
    first_build_seconds = build[[1L]],
    warm_build_median_seconds = stats::median(build[warm_index]),
    first_render_seconds = render[[1L]],
    warm_render_median_seconds = stats::median(render[warm_index]),
    warm_total_median_seconds = stats::median(build[warm_index] + render[warm_index]),
    html_bytes = samples[[length(samples)]]$bytes,
    repetitions = repetitions,
    stringsAsFactors = FALSE
  )
}

result <- do.call(rbind, lapply(specifications, function(specification) {
  tryCatch(
    measure_one(specification),
    error = function(error) data.frame(
      analysis = specification[[1L]],
      first_build_seconds = NA_real_, warm_build_median_seconds = NA_real_,
      first_render_seconds = NA_real_, warm_render_median_seconds = NA_real_,
      warm_total_median_seconds = NA_real_, html_bytes = NA_integer_,
      repetitions = repetitions, error = conditionMessage(error),
      stringsAsFactors = FALSE
    )
  )
}))
result <- result[order(result$warm_total_median_seconds, decreasing = TRUE, na.last = TRUE), ]
rownames(result) <- NULL
print(result, row.names = FALSE, digits = 4)

output_path <- trimws(Sys.getenv("STATEDU_BENCHMARK_OUTPUT", ""))
if (nzchar(output_path)) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, output_path)
}
