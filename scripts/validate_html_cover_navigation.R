if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
out <- "tmp/html-cover-navigation"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
entries <- read_result_snapshot_store("sample/StatEdu_Studio_result_history_20260917_154949.efs-result")
check <- function(original, exported) {
  before <- xml2::read_html(original); after <- xml2::read_html(exported)
  cells <- function(doc) xml2::xml_text(xml2::xml_find_all(doc, ".//table//th | .//table//td"))
  stopifnot(identical(cells(before), cells(after)))
  count <- length(xml2::xml_find_all(before, ".//table"))
  links <- xml2::xml_find_all(after, ".//*[@class='html-table-contents']//a")
  back <- xml2::xml_find_all(after, ".//*[@class='html-table-navigation']/a")
  stopifnot(length(links) == count, length(back) == count)
  ids <- xml2::xml_attr(xml2::xml_find_all(after, ".//*[@id]"), "id")
  for (href in c(xml2::xml_attr(links, "href"), xml2::xml_attr(back, "href"))) stopifnot(sum(ids == substring(href, 2)) == 1L)
  stopifnot(length(xml2::xml_find_all(after, ".//*[@data-statedu-html-navigation='cover']")) == 1L)
  cover <- xml2::xml_find_first(after, ".//*[@data-statedu-html-navigation='cover']")
  stopifnot(length(xml2::xml_find_all(cover, "preceding-sibling::*")) == 0L)
  stopifnot(length(xml2::xml_find_all(cover, ".//ol")) == 0L)
  if (count > 0L) {
    contents <- xml2::xml_find_first(cover, "following-sibling::*[1]")
    stopifnot(xml2::xml_attr(contents, "class") == "html-table-contents")
    jump <- xml2::xml_attr(xml2::xml_find_first(cover, ".//*[@class='html-cover-contents-link']/a"), "href")
    stopifnot(jump == paste0("#", xml2::xml_attr(contents, "id")))
  }
  # Removing our additive navigation must leave every original body element intact.
  xml2::xml_remove(xml2::xml_find_all(after, "//*[@data-statedu-html-navigation]"))
  body_text <- function(doc) {
    values <- trimws(xml2::xml_text(xml2::xml_find_all(doc, ".//body//text()")))
    values[nzchar(values)]
  }
  stopifnot(identical(body_text(before), body_text(after)))
  cat("PASS:", count, "tables, original cells/body, links and return links\n")
}
for (name in c("current", "accumulated")) {
  selected <- if (name == "current") entries[1] else entries
  original <- saved_result_collection_html(selected)
  file <- file.path(out, paste0(name, ".html"))
  write_result_collection_html(selected, file)
  exported <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  check(original, exported)
  check(original, result_html_export_document(exported))
  save_result_collection_excel_file(selected, file.path(out, paste0(name, ".xlsx")))
  wb <- openxlsx::loadWorkbook(file.path(out, paste0(name, ".xlsx")))
  cover <- openxlsx::read.xlsx(wb, sheet = 1, colNames = FALSE)
  stopifnot(paste0("StatEdu Studio v", saved_results_app_version()) %in% cover[[1]])
  members <- unzip(file.path(out, paste0(name, ".xlsx")), list = TRUE)$Name
  # Descriptive results contain no figures: these two media files must be
  # the shared application and developer logos on the cover drawing.
  stopifnot(sum(grepl("^xl/media/", members)) >= 2L)
}
tiny <- '<html><head><title>Test</title></head><body><div id="statedu-html-cover"></div><h2>A &amp; B</h2><table id="statedu-html-table-1"><tr><td>1.230</td></tr></table><h2>A &amp; B</h2><table><tr><td>한글</td></tr></table></body></html>'
for (language in c("ko", "en", "ja", "zh", "es", "fr", "de", "vi")) {
  check(tiny, result_html_export_document(tiny, language))
  stopifnot(grepl(statedu_t("report.cover.back_to_cover", language), result_html_export_document(tiny, language), fixed = TRUE))
}
check("<html><body><p>No tables</p></body></html>", result_html_export_document("<html><body><p>No tables</p></body></html>"))
cat("PASS: Excel cover version, eight languages, duplicate titles/IDs and empty reports\n")

