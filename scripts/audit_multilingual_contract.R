Sys.setlocale("LC_CTYPE", "English_United States.utf8")
source("R/utils.R", encoding = "UTF-8")
source("R/labels.R", encoding = "UTF-8")
out <- "tmp/multilingual-audit"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
catalog <- statedu_translation_table()
locales <- c("ja", "zh", "es", "fr", "de", "vi")
english_index <- split(names(catalog), vapply(catalog, function(row) {
  value <- row["en"]
  if (is.na(value)) "" else unname(value)
}, character(1)))
literal <- function(x) {
  if (is.character(x)) return(x)
  if (is.call(x) && is.symbol(x[[1]]) && as.character(x[[1]]) %in% c("statedu_utf8", "h", "merge_ko") && is.character(x[[2]])) return(statedu_utf8(x[[2]]))
  if (is.call(x) && identical(x[[1]], as.name("c"))) {
    values <- lapply(as.list(x)[-1L], literal)
    if (all(vapply(values, is.character, logical(1)))) return(unlist(values, use.names = FALSE))
  }
  NULL
}
records <- list()
add <- function(en, ko, file, fn, kind) {
  if (is.null(en) || is.null(ko) || length(en) != length(ko)) return()
  for (i in seq_along(en)) {
    if (!nzchar(en[[i]]) || !grepl("[A-Za-z]", en[[i]]) || grepl("[\uac00-\ud7a3]", en[[i]], perl = TRUE) || !grepl("[\uac00-\ud7a3]", ko[[i]], perl = TRUE)) next
    key <- paste0("analysis.ui.", gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(en[[i]])))))
    keys <- unique(c(key, english_index[[en[[i]]]]))
    missing <- locales[!vapply(locales, function(lang) any(vapply(keys, function(k) {
      value <- catalog[[k]][lang]
      length(value) == 1L && !is.na(value) && nzchar(value)
    }, logical(1))), logical(1))]
    records[[length(records) + 1L]] <<- data.frame(file, function_name = fn, kind, english = en[[i]], korean = ko[[i]], missing_languages = paste(missing, collapse = ","), stringsAsFactors = FALSE)
  }
}
walk <- function(x, file, fn = "<top level>") {
  if (!is.call(x) && !is.expression(x)) return()
  if (is.call(x) && is.symbol(x[[1]])) {
    name <- as.character(x[[1]])
    if (name %in% c("<-", "=") && length(x) == 3L && is.symbol(x[[2]]) && is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function"))) {
      walk(x[[3]], file, as.character(x[[2]])); return()
    }
    if (name == "if" && length(x) == 4L) add(literal(x[[4]]), literal(x[[3]]), file, fn, "bilingual_branch")
    if (name %in% c("statedu_localized_text", "mediation_moderation_text", "sample_size_text", "complex_sample_text_pair", "merge_text", "id_aggregate_text", "structural_canvas_invariance_text", "custom_model_canvas_text") && length(x) >= 4L) add(literal(x[[3]]), literal(x[[4]]), file, fn, "translation_call")
    if (name %in% c("appendix_text", "appendix_title") && length(x) >= 3L) add(literal(x[[2]]), literal(x[[3]]), file, fn, "appendix_call")
    if (name == "c" && length(x) > 1L) {
      keys <- names(x)
      if (all(c("en", "ko") %in% keys)) add(literal(x[["en"]]), literal(x[["ko"]]), file, fn, "bilingual_dictionary")
      else if (!is.null(keys)) for (i in seq_along(x)[-1L]) if (!is.na(keys[[i]]) && nzchar(keys[[i]])) add(keys[[i]], literal(x[[i]]), file, fn, "named_dictionary")
    }
  }
  for (i in seq_along(x)) {
    if (identical(x[[i]], quote(expr = ))) next
    if (!is.symbol(x[[i]])) walk(x[[i]], file, fn)
  }
}
files <- c(list.files("R", pattern = "[.]R$", full.names = TRUE), list.files("modules", pattern = "[.]R$", full.names = TRUE, recursive = TRUE))
for (file in files) walk(parse(file, encoding = "UTF-8"), file)
table <- unique(do.call(rbind, records))
write.csv(table, file.path(out, "source-phrases.csv"), row.names = FALSE, fileEncoding = "UTF-8")
missing <- table[nzchar(table$missing_languages), ]
write.csv(missing, file.path(out, "missing-translations.csv"), row.names = FALSE, fileEncoding = "UTF-8")
summary <- aggregate(english ~ file, missing, function(x) length(unique(x)))
summary <- summary[order(-summary$english), ]
names(summary)[[2]] <- "candidate_phrases_missing_translation"
write.csv(summary, file.path(out, "by-file.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("Files scanned:", length(files), "\nSource occurrences:", nrow(table), "\nUnique phrases with a missing locale:", length(unique(missing$english)), "\n")
print(utils::head(summary, 20L), row.names = FALSE)
cat("Candidate audit only: includes main-table English exceptions and dormant branches; dynamic messages require runtime checks.\n")
