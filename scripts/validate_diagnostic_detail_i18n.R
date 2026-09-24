Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
sentences <- c("Cell-level Shapiro-Wilk checks did not flag p < .05.",
  "The repeated outcome is treated as continuous Gaussian; mixed modeling handles unbalanced repeated records.")
diagnostics <- c("Sphericity met", "Sphericity not met", "not checked Satisfied",
  "No clear slope heterogeneity", "no clear nonlinearity",
  "Acceptable (max VIF=1.03)", "Flagged cases=2; max Cook's D=.07",
  "Outcome normality is descriptive; residual normality drives ANCOVA method selection.",
  sentences, paste(sentences, collapse = " "),
  "Crossing survival curves: 사용자집단=1 vs sex=2", "At risk near follow-up tail = 8",
  "residual normality, homogeneity of variance, and homogeneity of regression slopes satisfied")
probe <- data.frame(Variable = diagnostics, Details = diagnostics, check.names = FALSE)
for (language in c("ja", "zh", "es", "fr", "de", "vi")) {
  localized <- result_appendix_localize_table(probe, language)
  stopifnot(identical(localized[[1]], diagnostics), all(localized[[2]] != diagnostics))
  stopifnot(grepl("1.03", localized[[2]][6], fixed = TRUE),
    grepl("2", localized[[2]][7], fixed = TRUE), grepl(".07", localized[[2]][7], fixed = TRUE))
  stopifnot(!grepl("Sphericity|not checked|Satisfied|Cell-level|The repeated", paste(localized[[2]], collapse = " ")))
  stopifnot(grepl("사용자집단=1 vs sex=2", localized[[2]][12], fixed = TRUE),
    grepl("8", localized[[2]][13], fixed = TRUE))
  for (title in c("Analysis overview", "Event-code mapping", "Data inclusion audit", "Survival-curve crossing screen"))
    stopifnot(survival_appendix_title(title, language) != title)
  cat("PASS:", language, "diagnostic translations, numeric captures, and original user labels\n")
}
# Include these exact changed outputs in both current and accumulated export tests.
out <- "tmp/diagnostic-detail-i18n"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
options(statedu.app_language = "ja")
fixture <- tagList(
  analysis_result_table_section("Publication table", regression_main_table(probe)),
  analysis_result_table_section(result_appendix_ui_text("Assumption review", "ja"), result_appendix_localize_table(probe, "ja")),
  analysis_result_table_section(survival_appendix_title("Analysis overview", "ja"),
    survival_appendix_localize_table(data.frame(Item = "Survival probability", Value = "Crossing detected"), "ja")))
saveRDS(list(list(id = "diagnostic-details", title = "Language contract", html = as.character(fixture))), file.path(out, "entries.rds"))
