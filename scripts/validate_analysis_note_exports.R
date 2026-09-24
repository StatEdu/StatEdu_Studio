Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
root <- "tmp/all-analysis-notes"
entries <- readRDS(file.path(root, "entries.rds"))
groups <- split(entries, sub("-.*", "", names(entries)))
# Family comes from the audit, including names that contain hyphens.
inventory <- read.csv(file.path(root, "tables.csv"), check.names = FALSE)
groups <- lapply(unique(inventory$family), function(family) entries[startsWith(names(entries), paste0(family, "-"))])
names(groups) <- unique(inventory$family)
extra <- c("validate_penalized", "validate_meta_analysis_ui", "validate_complex_sample_analysis", "structural")
for (group in extra) {
  html <- readRDS(file.path(root, group, "rendered.rds"))
  if (group == "structural") {
    for (type in c("cfa", "sem", "plssem")) groups[[type]] <- lapply(html[startsWith(names(html), paste0(type, "_"))], function(h) list(html = paste0('<div class="structural-analysis-results regression-results">', h, '</div>')))
  } else groups[[group]] <- lapply(html, function(h) list(html = h))
}
fixtures <- list()
for (group in names(groups)) {
  for (entry in groups[[group]]) {
    doc <- xml2::read_html(entry$html)
    candidates <- xml2::xml_find_all(doc, ".//*[@data-result-table-sheet='true'][descendant::table and descendant::*[contains(@class,'note')]]")
    if (!length(candidates)) next
    fragment <- as.character(candidates[[1]])
    parent <- xml2::xml_parent(candidates[[1]])
    while (!xml2::xml_name(parent) %in% c("body", "html")) {
      fragment <- as.character(htmltools::tag(xml2::xml_name(parent), c(as.list(xml2::xml_attrs(parent)), list(htmltools::HTML(fragment)))))
      parent <- xml2::xml_parent(parent)
    }
    fixtures[[group]] <- list(id = group, title = group, html = fragment)
    break
  }
}
stopifnot(length(fixtures) == length(groups))
out <- file.path(root, "exports")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
normalize <- function(x) gsub("[[:space:]]+", "", x, perl = TRUE)
note_text <- function(entry) {
 d <- xml2::read_html(entry$html)
 n <- xml2::xml_find_all(d, ".//*[contains(@class,'note') and not(descendant::table) and not(descendant::*[contains(@class,'note')])]")
 vapply(n, result_html_text, character(1))
}
for (mode in c("current", "accumulated")) {
  selected <- if (mode == "current") fixtures[1] else fixtures
  base <- file.path(out, mode)
  expected <- unlist(lapply(selected, note_text), use.names = FALSE)
  check <- function(text) stopifnot(all(vapply(normalize(expected), grepl, logical(1), x = normalize(text), fixed = TRUE)))
  write_result_collection_html(selected, paste0(base, ".html"))
  check(xml2::xml_text(xml2::read_html(paste0(base, ".html"))))
  write_result_collection_docx(selected, paste0(base, ".docx"))
  doc <- xml2::read_xml(unz(paste0(base, ".docx"), "word/document.xml"))
  check(paste(xml2::xml_text(xml2::xml_find_all(doc, ".//w:t", xml2::xml_ns(doc))), collapse = " "))
  save_result_collection_excel_file(selected, paste0(base, ".xlsx"))
  wb <- openxlsx::loadWorkbook(paste0(base, ".xlsx"))
  stopifnot(length(names(wb)) == length(selected) + 1L)
  for (i in seq_along(selected)) {
    text <- paste(as.matrix(openxlsx::read.xlsx(wb, sheet = i + 1L, colNames = FALSE)), collapse = " ")
    stopifnot(all(vapply(normalize(note_text(selected[[i]])), grepl, logical(1), x = normalize(text), fixed = TRUE)))
  }
  write_result_collection_pdf(selected, paste0(base, ".pdf"))
  python <- file.path(Sys.getenv("USERPROFILE"), ".cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe")
  status <- system2(python, c("scripts/extract_note_test_pdf.py", shQuote(normalizePath(paste0(base, ".pdf"))), shQuote(file.path(normalizePath(out), paste0(mode, "-pdf.txt")))))
  stopifnot(status == 0L)
  check(paste(readLines(paste0(base, "-pdf.txt"), encoding = "UTF-8"), collapse = " "))
  write_result_collection_hwpx(selected, paste0(base, ".hwpx"))
  members <- unzip(paste0(base, ".hwpx"), list = TRUE)$Name
  sections <- members[grepl("Contents/section[0-9]+[.]xml$", members)]
  text <- paste(vapply(sections, function(path) xml2::xml_text(xml2::read_xml(unz(paste0(base, ".hwpx"), path))), character(1)), collapse = " ")
  check(text)
  message("PASS: ", mode, "; ", length(selected), " analysis fixtures, ", length(expected), " notes, five formats")
}
write.csv(data.frame(family = names(fixtures)), file.path(out, "coverage.csv"), row.names = FALSE)
