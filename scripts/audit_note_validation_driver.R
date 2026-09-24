Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
note_capture_state <- new.env(parent = emptyenv())
note_capture_state$html <- list()
note_capture_state$busy <- FALSE
note_capture_render <- function(value) {
  if (isTRUE(note_capture_state$busy) || !is.list(value)) return(invisible(NULL))
  html <- as.character(value$html %||% "")
  if (length(html) != 1 || !grepl("<table", html, fixed = TRUE)) return(invisible(NULL))
  note_capture_state$busy <- TRUE
  on.exit(note_capture_state$busy <- FALSE)
  key <- digest::digest(html)
  note_capture_state$html[[key]] <- html
}
trace("renderTags", where = asNamespace("htmltools"), exit = quote(note_capture_render(returnValue())), print = FALSE)
target <- commandArgs(trailingOnly = TRUE)[[1]]
failure <- tryCatch({source(target, encoding = "UTF-8"); NULL}, error = function(e) conditionMessage(e))
untrace("renderTags", where = asNamespace("htmltools"))
folder <- file.path("tmp", "all-analysis-notes", tools::file_path_sans_ext(basename(target)))
dir.create(folder, recursive = TRUE, showWarnings = FALSE)
saveRDS(note_capture_state$html, file.path(folder, "rendered.rds"))
rows <- lapply(seq_along(note_capture_state$html), function(i) {
  html <- note_capture_state$html[[i]]
  doc <- xml2::read_html(html)
  tables <- xml2::xml_find_all(doc, ".//table")
  notes <- xml2::xml_find_all(doc, ".//*[self::p or self::div][contains(@class,'note') and not(descendant::table)]")
  note_text <- vapply(notes, result_html_text, character(1))
  prefixes <- sum(grepl("^(Notes?|주석|주)[.:]", trimws(note_text), perl = TRUE))
  data.frame(render = i, tables = length(tables), notes = length(notes), redundant_prefixes = prefixes)
})
write.csv(if (length(rows)) do.call(rbind, rows) else data.frame(), file.path(folder, "audit.csv"), row.names = FALSE)
writeLines(failure %||% "PASS", file.path(folder, "status.txt"))
message("Captured ", length(rows), " distinct table renderings from ", target)
if (!is.null(failure)) stop(failure, call. = FALSE)
