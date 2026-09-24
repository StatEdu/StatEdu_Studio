Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
out <- "tmp/multilingual-audit"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
languages <- c("en", "ko", "ja", "zh", "es", "fr", "de", "vi")
localizers <- c("result_appendix_localize_table", "regression_appendix_table", "ancova_appendix_table",
  "generalized_appendix_table", "logistic_appendix_table", "longitudinal_appendix_table",
  "paired_appendix_table", "mixed_rm_appendix_table", "correlation_appendix_localize_table",
  "interrater_agreement_appendix_table", "survival_appendix_localize_table",
  "structural_canvas_localize_appendix_table", "mediation_moderation_appendix_overview_table",
  "mediation_moderation_appendix_bootstrap_table", "pca_appendix_table", "factor_analysis_appendix_table")
probe <- data.frame(Variable = c("Normality", "Yes", "Status", "사용자 변수"),
  Group = c("Adequate", "No", "Satisfied", "사용자 범주"), Status = rep("Adequate", 4), N = c(17L, 18L, 19L, 20L),
  p = c(.001, .05, NA_real_, .938), check.names = FALSE)
overview <- data.frame(Item = c("Outcome", "Predictor", "Mediators", "Residual diagnostics"),
  Value = c("Normality", "Yes", "Status", "Run"))
for (language in languages) {
  options(statedu.app_language = language)
  for (name in localizers) {
    fn <- get(name)
    apply_table <- function(table) do.call(fn, c(list(table), if ("language" %in% names(formals(fn))) list(language = language)))
    value <- apply_table(probe)
    for (i in c(1L, 2L, 4L, 5L)) if (!identical(value[[i]], probe[[i]])) stop(name, "/", language, ": user/numeric column changed: ", names(probe)[i])
    stopifnot(all(value[[3]] == result_appendix_ui_text("Adequate", language)))
    value <- apply_table(overview)
    if (!identical(value[[2]][1:3], overview[[2]][1:3])) stop(name, "/", language, ": overview variable labels changed")
    stopifnot(identical(attr(value, "result_table_language"), language))
  }
  matrix <- data.frame(Variable = c("Normality", "Status"), Normality = c(1, .5), Status = c(.5, 1))
  value <- result_appendix_localize_table(matrix, language)
  stopifnot(identical(names(value)[-1L], names(matrix)[-1L]), identical(value[[1]], matrix[[1]]))
  stopifnot(identical(structural_canvas_appendix_ui_text("Assumption review", language), result_appendix_ui_text("Assumption review", language)))
  structural_ui <- structural_canvas_basic_html_table(probe, role = "appendix", language = language, title = "Assumption review")
  structural_doc <- xml2::read_html(as.character(structural_ui))
  stopifnot(xml2::xml_text(xml2::xml_find_first(structural_doc, "//h5")) == result_appendix_ui_text("Assumption review", language))
  cat("PASS:", language, "16 appendix localizers preserve conflicting variable/category labels and numeric values\n")
}

options(statedu.app_language = "en")
set.seed(916)
d <- data.frame(group = factor(rep(c("Yes", "Normality"), each = 30)), x = rnorm(60), pre = rnorm(60), post = rnorm(60), post2 = rnorm(60))
info <- data.frame(name = names(d), measurement = c("category", rep("continuous", 4)), var_label = c("사용자 집단", "Status", "Normality", "Yes", "사용자 결과"))
freq <- prepare_frequencies_results(d, names(d), info)
paired <- prepare_paired_results(d, "pre", "post", info, options = list(mean_sd = TRUE, effect_size = TRUE))
rm3 <- prepare_paired_rm_results(d, variables = c("pre", "post", "post2"), variable_info = info, options = list(mean_sd = TRUE))
ancova <- prepare_ancova_results(d, "post", "group", "x", info, options = list(normality_enabled = FALSE))
mixed <- prepare_mixed_rm_anova_results(d, "group", c("pre", "post", "post2"), covariates = "x", variable_info = info, options = list(assumption_check = TRUE))
km <- prepare_km_analysis_result(read.csv("scripts/fixtures/survival_validation.csv"), "time", "status", "sex", rate_times = "100, 200, 400")
effects <- do.call(rbind, lapply(1:4, function(i) meta_normalize_effect(list(included = TRUE, study_id = paste0("S", i), study_name = "사용자 연구", outcome = "Normality", predictor = "Yes", family = "g", input_type = "g_se", direction = "positive", g = i / 10, se = .1), row_id = i)))
meta <- meta_fit_model(effects, "g", model = "fixed")
regression <- readRDS("outputs/spss_phase32_20260906/three_blocks/analysis.rds")
builders <- list(frequencies = function() frequencies_results_ui(freq), paired = function() paired_results_ui(paired),
  repeated = function() paired_rm_results_ui(rm3), ancova = function() ancova_results_ui(ancova, info),
  mixed = function() mixed_rm_anova_results_ui(mixed), survival = function() survival_km_result_panel(km),
  meta = function() meta_analysis_results_ui(meta), regression = function() hierarchical_results_panel(regression))
baseline <- list(); report <- list(); entries <- list(); appendix_candidates <- list()
for (language in languages) {
  options(statedu.app_language = language)
  for (name in names(builders)) {
    html <- as.character(htmltools::renderTags(builders[[name]]())$html)
    doc <- xml2::read_html(html)
    main <- xml2::xml_find_all(doc, "//table[@data-result-table-role='main']")
    appendix <- xml2::xml_find_all(doc, "//table[@data-result-table-role='appendix']")
    stopifnot(length(main) > 0L, all(xml2::xml_attr(main, "data-result-table-language") == "en"))
    cells <- lapply(main, function(table) list(
      cells = xml2::xml_text(xml2::xml_find_all(table, ".//th|.//td")),
      title = xml2::xml_text(xml2::xml_find_all(table, "preceding::*[self::h3 or self::h4][1]")),
      notes = xml2::xml_text(xml2::xml_find_all(table, "ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
    if (language == "en") baseline[[name]] <- cells
    if (!identical(cells, baseline[[name]])) stop(name, "/", language, ": main-table cells/title/notes differ from English")
    if (length(appendix)) stopifnot(all(xml2::xml_attr(appendix, "data-result-table-language") == language))
    if (language != "en") {
      headers <- unique(xml2::xml_text(xml2::xml_find_all(appendix, ".//th")))
      candidates <- headers[grepl("^[A-Za-z][A-Za-z -]{2,}", headers)]
      if (length(candidates)) appendix_candidates[[length(appendix_candidates) + 1L]] <- data.frame(analysis = name, language, header = candidates)
    }
    report[[length(report) + 1L]] <- data.frame(analysis = name, language, main_tables = length(main), appendix_tables = length(appendix), main_content_matches_English = TRUE)
    if (language == "ja") entries[[length(entries) + 1L]] <- list(id = name, title = name, html = html)
  }
  cat("PASS:", language, "eight analysis renderers: main-table cells match English; appendix language metadata follows UI\n")
}
write.csv(do.call(rbind, report), file.path(out, "runtime-table-roles.csv"), row.names = FALSE)
write.csv(unique(do.call(rbind, appendix_candidates)), file.path(out, "appendix-headers-to-review.csv"), row.names = FALSE, fileEncoding = "UTF-8")
# Small export fixture deliberately puts the same words in user and system cells.
options(statedu.app_language = "ja")
fixture <- tagList(analysis_result_table_section("Publication table", regression_main_table(probe)),
  analysis_result_table_section(result_appendix_ui_text("Assumption review", "ja"), regression_appendix_table(probe, "ja")),
  structural_canvas_basic_html_table(probe, role = "appendix", language = "ja", title = "Assumption review"),
  meta_result_section("Study effects", meta_study_results_table(meta, "en"), "appendix", "ja"))
saveRDS(list(list(id = "label-collisions", title = "Language contract", html = as.character(fixture))), file.path(out, "entries.rds"))
saveRDS(entries, file.path(out, "analysis-entries.rds"))
