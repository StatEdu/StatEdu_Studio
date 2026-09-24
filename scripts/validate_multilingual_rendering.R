Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
out <- "tmp/multilingual"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
has_korean <- function(x) grepl("[\uac00-\ud7a3]", x, perl = TRUE)
text_functions <- c("result_appendix_ui_text", "regression_appendix_text", "ancova_appendix_text", "generalized_appendix_text", "generalized_appendix_value_text", "logistic_appendix_text", "longitudinal_appendix_text")
for (language in setdiff(statedu_supported_languages(), "ko")) {
  for (fn in text_functions) for (text in c("Model overview", "Requested", "Status", "Residual diagnostics", "Bootstrap regression with HC3 robust standard errors")) {
    value <- do.call(fn, list(text, language))
    stopifnot(!has_korean(value))
  }
  for (fn in c("ancova_appendix_table", "generalized_appendix_table", "logistic_appendix_table", "longitudinal_appendix_table", "regression_appendix_table")) {
    input <- data.frame(Variable = "사용자 변수", Status = "Adequate", Requested = 5000, check.names = FALSE)
    value <- do.call(fn, list(input, language))
    stopifnot(!any(has_korean(names(value))), identical(value[[1]], input[[1]]), identical(value[[3]], input[[3]]))
  }
}
stopifnot(identical(regression_appendix_text("Requested", "ko"), "요청"),
          identical(result_appendix_ui_text("Model overview", "ja"), "モデルの概要"),
          !identical(result_appendix_ui_text("Valid", "ja"), result_appendix_ui_text("Valid %", "ja")))
for (language in statedu_supported_languages()) {
  options(statedu.app_language = language)
  htmltools::save_html(app_ui("multilingual-check"), file.path(out, paste0(language, ".html")))
}
options(statedu.app_language = "ja")
mm <- custom_model_canvas_workspace(c("x", "m", "y"), language = "ja")
writeLines(as.character(mm), file.path(out, "mm-ja.html"), useBytes = TRUE)
writeLines(system.file("www/shared/bootstrap/css/bootstrap.min.css", package = "shiny"), file.path(out, "bootstrap-path.txt"))
models <- readRDS("outputs/spss_phase32_20260906/three_blocks/analysis.rds")
regression_html <- as.character(htmltools::renderTags(hierarchical_results_panel(models))$html)
writeLines(regression_html, file.path(out, "regression-ja.html"), useBytes = TRUE)
doc <- xml2::read_html(regression_html)
texts <- xml2::xml_text(xml2::xml_find_all(doc, "//th|//td|//h2|//h3|//h4|//h5|//p"))
cat("Korean strings in regression fixture (may include user labels):\n")
print(unique(texts[has_korean(texts)]))
overview <- data.frame(Item = c("Model", "Outcome", "Focal X analyses", "Residual diagnostics", "Automatic method selection", "Missing data"),
                       Value = c("User-defined mediation / moderation model", "사용자 변수", "x", "Run", "On", "Complete cases for each focal-X model"))
overview <- mediation_moderation_appendix_overview_table(overview, "ja")
stopifnot(!any(has_korean(names(overview))), identical(overview[[2]][[2]], "사용자 변수"))
html <- as.character(tags$div(class = "regression-results",
  analysis_result_table_section(result_appendix_ui_text("Model overview", "ja"), overview, table_fn = coefficient_html_table),
  analysis_result_table_section(result_appendix_ui_text("Bootstrap diagnostics", "ja"), regression_appendix_table(data.frame(Model = "Model 1", Requested = 5000L, Valid = 5000L, Status = "Adequate"), "ja"), table_fn = coefficient_html_table),
  analysis_result_table_section(result_appendix_ui_text("Recommended interpretation", "ja"),
    mixed_rm_appendix_table(data.frame(Item=c("Analysis", "Subjects", "Sphericity"),
      Value=c("Covariate-adjusted mixed repeated-measures ANOVA", "사용자 변수", "Not satisfied (W=.40, p<.001); adjusted residuals (group + covariates)."), N=c(589L,589L,589L))), table_fn=coefficient_html_table)))
saveRDS(list(list(id = "ja-overview", title = "日本語の結果", html = html)), file.path(out, "entries.rds"))
cat("PASS: eight-language fallback, Japanese diagnostics, preserved user labels and numeric columns\n")
