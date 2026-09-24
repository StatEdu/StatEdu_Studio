Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
options(statedu.app_language = "ja")
out <- "tmp/survival-multilingual"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
data <- read.csv("scripts/fixtures/survival_validation.csv")
result <- prepare_km_analysis_result(data, "time", "status", "sex", rate_times = "100, 200, 400")
result$plot_types <- c("survival", "event", "cumhaz", "log_survival")
result$plot_versions <- "color"
ids <- paste0("i18n_survival_plot_", seq_along(result$plot_types))
panel <- xml2::read_html(as.character(survival_km_result_panel(result, plot_output_ids = ids, language = "ja")))
for (i in seq_along(ids)) {
  file <- file.path(out, paste0(ids[[i]], ".png"))
  png(file, width = 1000, height = 760, res = 120)
  survival_draw_plot_with_risk_table(survival_km_ggplot(result, result$plot_types[[i]], "color"), survival_km_risk_table_plot(result, "color"))
  dev.off()
  node <- xml2::xml_find_first(panel, paste0("//*[@id='", ids[[i]], "']"))
  xml2::xml_replace(node, xml2::read_xml(paste0('<img style="max-width:100%" src="data:image/png;base64,', base64enc::base64encode(file), '"/>')))
}
html <- as.character(xml2::xml_find_first(panel, "//body/div"))
entries <- list(list(id = "survival-ja", title = "生存時間分析", html = html))
normalize <- function(x) gsub("[[:space:]\u00a0]+", "", paste(x, collapse = ""), perl = TRUE)
expected <- xml2::xml_text(xml2::xml_find_all(panel, "//th|//td|//h3|//h4|//h5"))
for (type in result$plot_types) stopifnot(any(grepl(survival_plot_type_label(type, "ja"), expected, fixed = TRUE)))
for (mode in c("current", "accumulated")) {
  selected <- if (mode == "current") entries else c(entries, list(list(id = "survival-ja-second", title = "追加した結果", html = html)))
  stem <- file.path(out, paste0("ja-", mode))
  write_result_collection_html(selected, paste0(stem, ".html"))
  write_result_collection_docx(selected, paste0(stem, ".docx"))
  save_result_collection_excel_file(selected, paste0(stem, ".xlsx"))
  write_result_collection_pdf(selected, paste0(stem, ".pdf"))
  write_result_collection_hwpx(selected, paste0(stem, ".hwpx"))
  word <- normalize(xml2::xml_text(xml2::read_xml(unz(paste0(stem, ".docx"), "word/document.xml"))))
  members <- unzip(paste0(stem, ".hwpx"), list = TRUE)$Name
  hwpx <- normalize(vapply(members[grepl("Contents/section[0-9]+[.]xml$", members)], function(s) xml2::xml_text(xml2::read_xml(unz(paste0(stem, ".hwpx"), s))), character(1)))
  workbook <- openxlsx::loadWorkbook(paste0(stem, ".xlsx"))
  excel <- normalize(unlist(lapply(seq_along(names(workbook)), function(i) as.matrix(openxlsx::read.xlsx(workbook, sheet = i, colNames = FALSE)))))
  saved_html <- normalize(xml2::xml_text(xml2::read_html(paste0(stem, ".html"))))
  for (value in expected[nzchar(trimws(expected))]) for (actual in list(word, hwpx, excel, saved_html)) stopifnot(grepl(normalize(value), actual, fixed = TRUE))
  for (format in c("docx", "hwpx", "xlsx")) {
    files <- unzip(paste0(stem, ".", format), list = TRUE)$Name
    stopifnot(sum(grepl("[.](png|jpg|jpeg)$", files, ignore.case = TRUE)) >= 4L)
  }
  jsonlite::write_json(expected, paste0(stem, "-expected.json"), auto_unbox = FALSE)
  cat("PASS:", mode, "survival HTML/Word/HWPX/Excel labels, values and four plots; PDF generated\n")
}
