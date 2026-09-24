Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
out <- "tmp/structural-screen-contract"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
matrix_table <- function(n) {
  labels <- paste0("긴 변수 이름 ", seq_len(n))
  table <- data.frame(Indicator = labels, matrix(".12", n, n), check.names = FALSE)
  names(table) <- c("Indicator", labels)
  table
}
stopifnot(!structural_canvas_table_needs_landscape(matrix_table(9)))
stopifnot(structural_canvas_table_needs_landscape(matrix_table(10)))
additional <- data.frame(Model = "Research model", Metric = c("cfi", "rmsea", "srmr", "aic"), Value = c(".95", ".05", ".04", "123"))
content <- tags$div(class = "structural-analysis-results regression-results",
  structural_canvas_basic_html_table(matrix_table(9), class = "structural-residual-matrix", title = "Nine variables"),
  structural_canvas_basic_html_table(matrix_table(10), title = "Ten variables"),
  structural_canvas_additional_fit_indices_ui(additional),
  tags$p("Displayed interpretation and caution must survive every export."))
html <- result_snapshot_document_html("Structural results", tags_to_html(content))
entry <- list(title = "Structural results", html = html)
tables <- result_entry_tables(entry)
expected <- c("portrait", "landscape", "landscape", "landscape", "landscape", "portrait")
stopifnot(identical(vapply(tables, `[[`, character(1), "orientation"), expected))
stopifnot(identical(vapply(tables, result_docx_wide_table, logical(1)), expected == "landscape"))
pdf_html <- saved_result_sheet_document("Structural results", content)
pages <- xml2::xml_find_all(xml2::read_html(pdf_html), ".//section[@data-orientation][.//table]")
stopifnot(identical(xml2::xml_attr(pages, "data-orientation"), expected))
stopifnot(grepl("Displayed interpretation and caution", pdf_html, fixed = TRUE))
writeLines(html, file.path(out, "screen.html"), useBytes = TRUE)
writeLines(pdf_html, file.path(out, "print.html"), useBytes = TRUE)
save_screen_excel_file(html, file.path(out, "screen.xlsx"))
write_result_collection_docx(list(entry), file.path(out, "screen.docx"))
zipdir <- file.path(out, "docx")
unzip(file.path(out, "screen.docx"), files = "word/document.xml", exdir = zipdir)
doc <- xml2::read_xml(file.path(zipdir, "word/document.xml"))
stopifnot(length(xml2::xml_find_all(doc, ".//w:tbl", xml2::xml_ns(doc))) == length(expected))
stopifnot(grepl("Displayed interpretation and caution", xml2::xml_text(doc), fixed = TRUE))
workbook <- openxlsx::loadWorkbook(file.path(out, "screen.xlsx"))
stopifnot(length(names(workbook)) == length(expected) + 1L)
save_screen_excel_file(result_snapshot_document_html("Accumulated", tags_to_html(content)), file.path(out, "accumulated.xlsx"))
message("PASS: 9/10-variable matrices, selected landscape fit families, HTML/PDF page contracts, Word and Excel exports")
