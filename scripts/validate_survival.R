script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_survival.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules()

message("Checking Kaplan-Meier analysis...")
fixture_path <- file.path(repo_root, "scripts", "fixtures", "survival_validation.csv")
if (!file.exists(fixture_path)) stop("Required survival validation fixture is missing: ", fixture_path)
survival_fixture <- read.csv(fixture_path, check.names = FALSE)
required_fixture_columns <- c("id", "time", "status", "sex", "ph.ecog", "age")
stopifnot(
  identical(names(survival_fixture), required_fixture_columns),
  nrow(survival_fixture) == 72L,
  identical(sort(unique(survival_fixture$status)), c(0L, 1L)),
  identical(sort(unique(survival_fixture$sex)), c(1L, 2L)),
  identical(sort(unique(survival_fixture$ph.ecog)), 0:2),
  identical(survival_fixture$id, seq_len(72L)),
  sum(survival_fixture$time) == 19176,
  sum(survival_fixture$status) == 50,
  sum(survival_fixture$age) == 4318,
  max(survival_fixture$time) > 400
)
km <- prepare_km_analysis_result(
  data = survival_fixture,
  time = "time",
  event = "status",
  group = c("sex", "ph.ecog"),
  event_value = "1",
  rate_times = "100, 200, 400",
  test_method = "logrank",
  output_tables = c("survival_table", "survival_time"),
  plot_types = c("survival", "event"),
  plot_versions = c("color", "bw")
)
stopifnot(identical(km$type, "km_multi"))
stopifnot(length(km$analyses) == 2)
stopifnot(all(vapply(km$analyses, function(item) !is.null(item$fit), logical(1))))

km_table <- survival_km_summary_table(km)
stopifnot(nrow(km_table) > 0)
stopifnot("M ± SE" %in% names(km_table))
stopifnot("Median (95% CI)" %in% names(km_table))
stopifnot("Chi-square (df)" %in% names(km_table))
stopifnot(!("df" %in% names(km_table)))
stopifnot(!any(grepl("=", km_table$Level, fixed = TRUE)))
stopifnot(any(grepl(paste0(intToUtf8(160), "("), km_table[["Median (95% CI)"]], fixed = TRUE)))

km_html <- htmltools::renderTags(survival_simple_table(km_table))$html
stopifnot(grepl("<br", km_html, fixed = TRUE))
stopifnot(grepl("&chi;", km_html, fixed = TRUE))
stopifnot(grepl("<sup>2</sup>", km_html, fixed = TRUE))

km_panel <- htmltools::renderTags(survival_km_results_panel(km, plot_output_ids = list(character(0), character(0)), language = "en"))$html
stopifnot(grepl("1. Analysis overview", km_panel, fixed = TRUE))
stopifnot(grepl("2. Kaplan-Meier survival time summary", km_panel, fixed = TRUE))
stopifnot(grepl("3. Survival probabilities at selected time points", km_panel, fixed = TRUE))
stopifnot(grepl("M = restricted mean over the default fitted follow-up horizon", km_panel, fixed = TRUE))
stopifnot(regexpr("1. Analysis overview", km_panel, fixed = TRUE) < regexpr("2. Kaplan-Meier survival time summary", km_panel, fixed = TRUE))
stopifnot(regexpr("2. Kaplan-Meier survival time summary", km_panel, fixed = TRUE) < regexpr("3. Survival probabilities at selected time points", km_panel, fixed = TRUE))
stopifnot(grepl("text-align:center !important", km_panel, fixed = TRUE))

message("Checking Kaplan-Meier empty table/plot options...")
km_empty_options <- prepare_km_analysis_result(
  data = survival_fixture,
  time = "time",
  event = "status",
  group = "sex",
  event_value = "1",
  output_tables = character(0),
  plot_types = character(0),
  plot_versions = character(0)
)
stopifnot(length(km_empty_options$output_tables) == 0)
stopifnot(length(km_empty_options$plot_types) == 0)
stopifnot(identical(km_empty_options$plot_versions, "color"))

message("Checking Kaplan-Meier plots...")
plot_color <- survival_km_ggplot(km$analyses[[1]], "survival", "color")
plot_bw <- survival_km_ggplot(km$analyses[[1]], "event", "bw")
stopifnot(inherits(plot_color, "ggplot"))
stopifnot(inherits(plot_bw, "ggplot"))

message("Checking Cox regression...")
event_note_html <- htmltools::renderTags(survival_event_variable_note("ko"))$html
stopifnot(
  grepl("이분형 또는 범주형 사건코드 변수", event_note_html, fixed = TRUE),
  grepl("사건/비사건으로 매핑", event_note_html, fixed = TRUE)
)
cox <- prepare_cox_analysis_result(
  data = survival_fixture,
  time = "time",
  event = "status",
  covariates = c("age", "sex"),
  event_value = "1"
)
stopifnot(identical(cox$type, "cox"))
stopifnot(nrow(cox$coef_table) > 0)
cox_hr_table <- survival_cox_coef_table(cox)
cox_statistic_table <- survival_cox_statistic_table(cox)
stopifnot(
  is.data.frame(cox_hr_table),
  identical(names(cox_hr_table), c("Term", "HR", "95% CI", "p")),
  is.data.frame(cox_statistic_table),
  identical(names(cox_statistic_table), c("Term", "B", "SE", "z", "p"))
)
stopifnot(is.data.frame(survival_ph_table(cox)))
factor_cox_data <- survival_fixture
factor_cox_data$sex <- factor(factor_cox_data$sex)
factor_cox <- prepare_cox_analysis_result(
  data = factor_cox_data,
  time = "time",
  event = "status",
  covariates = c("sex", "age"),
  event_value = "1",
  reference_values = stats::setNames(levels(factor_cox_data$sex)[[2]], "sex")
)
factor_cox_table <- survival_cox_coef_table(factor_cox)
stopifnot(
  any(grepl(paste0("sex=", levels(factor_cox_data$sex)[[2]], " \\(reference\\)"), factor_cox_table$Term)),
  any(grepl("sex=.*\\(reference\\)", factor_cox_table$Term)),
  any(factor_cox_table$`95% CI` == "reference"),
  any(factor_cox_table$HR == "1.000")
)
cox_html <- htmltools::renderTags(survival_cox_results_panel(cox, language = "ko"))$html
cox_table_html <- htmltools::renderTags(survival_cox_result_html_table(factor_cox))$html
stopifnot(
  grepl("survival-cox-result-table", cox_table_html, fixed = TRUE),
  grepl("coefficient-ci-group-header", cox_table_html, fixed = TRUE),
  grepl('colspan="2"', cox_table_html, fixed = TRUE),
  grepl("95% CI", cox_table_html, fixed = TRUE),
  grepl("LLCI", cox_table_html, fixed = TRUE),
  grepl("ULCI", cox_table_html, fixed = TRUE),
  grepl(">1.00<", cox_table_html, fixed = TRUE),
  grepl("survival-cox-summary-row", cox_table_html, fixed = TRUE),
  grepl("Likelihood-ratio χ²(p)", cox_table_html, fixed = TRUE),
  grepl("PH GLOBAL χ²(p)", cox_table_html, fixed = TRUE),
  !grepl("Wald χ²(p)", cox_table_html, fixed = TRUE),
  !grepl("Score χ²(p)", cox_table_html, fixed = TRUE),
  !grepl("PH age χ²(p)", cox_table_html, fixed = TRUE),
  grepl("text-align:center", cox_table_html, fixed = TRUE)
)
ph_first <- factor_cox$ph_table[toupper(factor_cox$ph_table$Term) == "GLOBAL", , drop = FALSE]
stopifnot(grepl(
  paste0(
    "PH ", ph_first[["Term"]], " χ²(p) = ",
    survival_format_number(ph_first[["chisq"]]), " (",
    survival_p(ph_first[["p"]]), ")"
  ),
  cox_table_html,
  fixed = TRUE
))
stopifnot(grepl("1. Analysis overview", cox_html, fixed = TRUE))
stopifnot(grepl("2. Cox proportional hazards model", cox_html, fixed = TRUE))
stopifnot(grepl("3. Supplementary statistics and diagnostics", cox_html, fixed = TRUE))
stopifnot(grepl("HR = hazard ratio", cox_html, fixed = TRUE))
stopifnot(grepl("괄호 안은 p값", cox_html, fixed = TRUE))
stopifnot(grepl("Schoenfeld 잔차", cox_html, fixed = TRUE))
stopifnot(grepl("비례위험 가정 상세 진단", cox_html, fixed = TRUE))
stopifnot(grepl("4. Residual distribution review", cox_html, fixed = TRUE))
stopifnot(grepl("5. Influence review", cox_html, fixed = TRUE))
stopifnot(grepl("logistic-result-table", cox_html, fixed = TRUE))
stopifnot(!grepl("survival-cox-model-subsection", cox_html, fixed = TRUE))
stopifnot(regexpr("2. Cox proportional hazards model", cox_html, fixed = TRUE) < regexpr("Likelihood-ratio χ²(p)", cox_html, fixed = TRUE))
stopifnot(regexpr("Likelihood-ratio χ²(p)", cox_html, fixed = TRUE) < regexpr("3. Supplementary statistics and diagnostics", cox_html, fixed = TRUE))

message("Checking survival HTML, PDF, Excel, and Result-collection exports...")
saved_cox_html <- saved_survival_results_html(cox, language = "en")
stopifnot(grepl("StatEdu Studio Cox Regression Results", saved_cox_html, fixed = TRUE))
stopifnot(grepl("data:image/png;base64,", saved_cox_html, fixed = TRUE))
saved_cox_document <- xml2::read_html(saved_cox_html)
stopifnot(length(xml2::xml_find_all(saved_cox_document, "//*[contains(concat(' ', normalize-space(@class), ' '), ' shiny-plot-output ')]")) == 0L)

survival_export_dir <- tempfile("survival-result-exports-")
dir.create(survival_export_dir)
survival_html_file <- file.path(survival_export_dir, "cox-results.html")
survival_pdf_file <- file.path(survival_export_dir, "cox-results.pdf")
survival_excel_file <- file.path(survival_export_dir, "cox-results.xlsx")
write_survival_results_html(cox, survival_html_file, language = "en")
write_survival_results_pdf(cox, survival_pdf_file, language = "en")
save_survival_excel_file(cox, survival_excel_file, language = "en")
stopifnot(all(file.exists(c(survival_html_file, survival_pdf_file, survival_excel_file))))
stopifnot(all(file.info(c(survival_html_file, survival_pdf_file, survival_excel_file))$size > 1000))
stopifnot(identical(readBin(survival_pdf_file, "raw", n = 4L), charToRaw("%PDF")))
survival_excel_sheets <- openxlsx::getSheetNames(survival_excel_file)
stopifnot("Report metadata" %in% survival_excel_sheets)
stopifnot(any(grepl("Cox", survival_excel_sheets, ignore.case = TRUE)))

survival_result_entry <- list(title = "Cox Regression", saved_at = "2026-08-20 12:00:00", html = saved_cox_html)
stopifnot(length(result_entry_tables(survival_result_entry, 1L)) >= 5L)
survival_result_images <- result_entry_images(survival_result_entry)
stopifnot(length(survival_result_images) >= 2L, all(file.exists(vapply(survival_result_images, `[[`, character(1), "path"))))
unlink(vapply(survival_result_images, `[[`, character(1), "path"))

message("Checking figure DPI policy...")
stopifnot(analysis_figure_dpi(edition = "pro", public_release = FALSE) == 600L)
stopifnot(analysis_figure_dpi(edition = "free", public_release = FALSE) == 300L)
stopifnot(analysis_figure_dpi(edition = "development", public_release = TRUE) == 300L)
stopifnot(analysis_figure_dpi(edition = "development", public_release = FALSE) == 600L)

message("All survival validations passed.")
