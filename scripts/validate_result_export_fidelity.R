Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
out <- "tmp/result-fidelity"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
html <- paste0('<div class="regression-results"><h2>RESULT_TITLE</h2><h3>TABLE_ONE_TITLE</h3><p>TABLE_ONE_INTRO</p>',
 '<div data-result-table-sheet="true" data-result-table-orientation="portrait"><h5>TABLE_ONE_SUBTITLE</h5>',
 '<table data-result-column-widths="[0.7,0.3]"><thead><tr><th>Variable</th><th>p</th></tr></thead><tbody><tr><td>LONG_LABEL_ONE</td><td>&lt;.001</td></tr></tbody></table>',
 '<p>TABLE_ONE_NOTE</p></div><p>TABLE_ONE_EXPLANATION</p><h6>TABLE_TWO_TITLE</h6>',
 '<div data-result-table-sheet="true" data-result-table-orientation="landscape"><table><thead><tr><th>Effect</th><th>CI</th></tr></thead>',
 '<tbody><tr><td>.210</td><td>[-.100, .500]</td></tr></tbody></table><p>TABLE_TWO_NOTE</p></div><p>TABLE_TWO_EXPLANATION</p></div>')
entry <- list(id = "fidelity", title = "Fidelity", saved_at = "today", html = html)
sentinels <- c("RESULT_TITLE", "TABLE_ONE_TITLE", "TABLE_ONE_INTRO", "TABLE_ONE_SUBTITLE", "LONG_LABEL_ONE", "TABLE_ONE_NOTE", "TABLE_ONE_EXPLANATION", "TABLE_TWO_TITLE", "TABLE_TWO_NOTE", "TABLE_TWO_EXPLANATION")
check_text <- function(text) {
  positions <- vapply(sentinels, function(value) regexpr(value, text, fixed = TRUE)[[1]], integer(1))
  stopifnot(all(positions > 0), !is.unsorted(positions))
  stopifnot(all(vapply(sentinels, function(value) length(gregexpr(value, text, fixed = TRUE)[[1]]) == 1L, logical(1))))
}
write_result_collection_docx(list(entry), file.path(out, "results.docx"))
unzip(file.path(out, "results.docx"), files = "word/document.xml", exdir = out)
doc <- xml2::read_xml(file.path(out, "word/document.xml"))
check_text(paste(xml2::xml_text(xml2::xml_find_all(doc, ".//w:t", xml2::xml_ns(doc))), collapse = " "))
stopifnot(length(xml2::xml_find_all(doc, ".//w:pgSz[@w:orient='landscape']", xml2::xml_ns(doc))) > 0L)
pdf_html <- saved_result_sheet_document("Fidelity", htmltools::HTML(html))
check_text(xml2::xml_text(xml2::xml_find_first(xml2::read_html(pdf_html), ".//body")))
writeLines(pdf_html, file.path(out, "results-print.html"), useBytes = TRUE)
write_result_collection_html(list(entry), file.path(out, "results.html"))
save_result_collection_excel_file(list(entry), file.path(out, "results.xlsx"))
workbook <- openxlsx::loadWorkbook(file.path(out, "results.xlsx"))
stopifnot(length(names(workbook)) == 3L)
one <- openxlsx::read.xlsx(workbook, sheet = 2, colNames = FALSE)
two <- openxlsx::read.xlsx(workbook, sheet = 3, colNames = FALSE)
one_text <- paste(as.matrix(one), collapse = " ")
two_text <- paste(as.matrix(two), collapse = " ")
stopifnot(all(vapply(sentinels[1:7], grepl, logical(1), x = one_text, fixed = TRUE)))
stopifnot(all(vapply(sentinels[8:10], grepl, logical(1), x = two_text, fixed = TRUE)))
stopifnot(grepl("<.001", one_text, fixed = TRUE), grepl(".210", two_text, fixed = TRUE))
message("PASS: all titles, h6, introductions, notes, explanations, order, exact displayed cells, and Word/PDF orientation; one table per Excel sheet")
