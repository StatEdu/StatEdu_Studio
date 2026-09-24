Sys.setlocale("LC_CTYPE", "English_United States.utf8")
out <- "tmp/multilingual-audit"
entries <- readRDS(file.path(out, "analysis-entries.rds"))
rows <- list()
for (entry in entries) {
  doc <- xml2::read_html(entry$html)
  tables <- xml2::xml_find_all(doc, "//table[@data-result-table-role='appendix']")
  for (table in tables) {
    text <- xml2::xml_text(xml2::xml_find_all(table, ".//td|.//th|preceding::*[self::h3 or self::h4 or self::h5][1]"))
    text <- unique(trimws(text))
    # Deliberately a candidate report, not a pass/fail: proper names and
    # user-defined labels may contain English and require contextual review.
    text <- text[nchar(text) >= 15L & grepl("[A-Za-z]{3,}[ -]+[A-Za-z]{3,}", text)]
    if (length(text)) rows[[length(rows) + 1L]] <- data.frame(analysis = entry$id, language = "ja", text)
  }
}
candidates <- unique(do.call(rbind, rows))
write.csv(candidates, file.path(out, "rendered-appendix-text-to-review.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat(nrow(candidates), "rendered appendix text candidates; inspect context before translating.\n")
print(utils::head(candidates, 12L), row.names = FALSE)
