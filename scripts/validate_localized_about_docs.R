script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
setwd(normalizePath(file.path(dirname(script), ".."), winslash = "/", mustWork = TRUE))
library(shiny)
source("R/utils.R", encoding = "UTF-8")
source("R/labels.R", encoding = "UTF-8")
source("R/app_misc_ui.R", encoding = "UTF-8")
languages <- c("ko", "en", "ja", "zh", "es", "fr", "de", "vi")
keys <- c("analysis_methods", "method_notes", "version_history")
expected_ids <- c("scope", "data", "descriptive", "group", "paired", "correlation", "factor", "ipa",
                  "regression", "mediation", "generalized", "longitudinal", "survival", "cfa",
                  "sem", "pls", "planning", "reporting", "validation")
paths <- character()
release_headings <- function(text) regmatches(text, gregexpr("(?m)^## v[^\r\n]+", text, perl = TRUE))[[1L]]
expected_releases <- release_headings(about_read_utf8_text("CHANGELOG.md"))
for (language in languages) {
  specs <- about_document_specs(language)
  for (key in keys) {
    spec <- specs[[key]]
    stopifnot(file.exists(spec$path), nzchar(spec$title), nzchar(spec$subtitle))
    paths <- c(paths, spec$path)
    text <- about_read_utf8_text(spec$path)
    stopifnot(!is.na(iconv(text, from = "UTF-8", to = "UTF-8")), !grepl("\uFFFD", text, fixed = TRUE))
    stopifnot(grepl("1.3.0", text, fixed = TRUE))
    html <- as.character(about_markdown_document(spec$path))
    stopifnot(grepl("<h", html, fixed = TRUE), !grepl("<U+", html, fixed = TRUE))
    if (key != "version_history") {
      stopifnot(!grepl("1\\.2\\.[0-9]|-dev|Unreleased", text))
      anchors <- regmatches(text, gregexpr('<a id="[a-z0-9-]+"></a>', text))[[1]]
      ids <- sub('<a id="([a-z0-9-]+)"></a>', '\\1', anchors)
      stopifnot(identical(ids, if (key == "method_notes") paste0("method-", 1:28) else expected_ids))
      for (id in ids) stopifnot(grepl(paste0('href="#', id, '"'), html, fixed = TRUE), grepl(paste0('id="', id, '"'), html, fixed = TRUE))
      stopifnot(grepl("CFA", text, fixed = TRUE), grepl("SEM", text, fixed = TRUE), grepl("PLS", text, fixed = TRUE))
    }
    if (key == "analysis_methods") stopifnot(all(vapply(c("300", "600", "PDF", "Word", "Excel", "seminr::mean_replacement"), grepl, logical(1), text, fixed = TRUE)))
    if (key == "method_notes") stopifnot(grepl("80%", text, fixed = TRUE), grepl("$$", text, fixed = TRUE),
                                        length(gregexpr("$$", text, fixed = TRUE)[[1]]) >= 40L)
    if (key != "version_history") stopifnot(all(vapply(c("SPSS", "AMOS", "SmartPLS"), grepl, logical(1), text, fixed = TRUE)))
    if (key == "version_history") {
      stopifnot(identical(release_headings(text), expected_releases), !grepl("Unreleased|-dev", text))
      rendered <- xml2::read_html(html)
      headings <- xml2::xml_text(xml2::xml_find_all(rendered, "//h2"))
      stopifnot(identical(headings, sub("^## ", "", expected_releases)))
    }
  }
}
stopifnot(length(unique(paths)) == 24L)
for (language in languages) {
  specs <- about_document_specs(language)
  for (key in c("overview", "user_guide", "validation")) {
    spec <- specs[[key]]
    stopifnot(file.exists(spec$path), nzchar(spec$title), nzchar(spec$subtitle))
    text <- about_read_utf8_text(spec$path)
    html <- as.character(about_markdown_document(spec$path))
    stopifnot(grepl("1.3.0", text, fixed = TRUE),
              !grepl("\uFFFD", text, fixed = TRUE),
              grepl("<h", html, fixed = TRUE), !grepl("<U+", html, fixed = TRUE))
    stopifnot(all(vapply(c("CFA", "SEM", "PLS", "RMST", "Cox"), grepl, logical(1), text, fixed = TRUE)))
    if (key != "validation") stopifnot(all(vapply(c("HWPX", "300", "600"), grepl, logical(1), text, fixed = TRUE)))
    if (key == "validation") stopifnot(all(vapply(c("49/50", "7/9", "6/6", "66/66", "584", "SHA-256"), grepl, logical(1), text, fixed = TRUE)))
    paths <- c(paths, spec$path)
  }
}
stopifnot(length(unique(paths)) == 48L)
keys <- c("overview", "user_guide", keys, "validation")
# Manifest lookup must also work from a different working directory (installed app root).
root <- getwd()
Sys.setenv(STATEDU_APP_DIR = root)
setwd(tempdir())
for (language in languages) {
  specs <- about_document_specs(language)
  stopifnot(all(vapply(specs[keys], function(x) nzchar(about_resolve_document_path(x$path)), logical(1))))
}
setwd(root)
for (path in c("docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC.md", "docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC_KO.md")) {
  text <- about_read_utf8_text(path)
  stopifnot(!grepl("1\\.2\\.[0-9]-dev|Detailed validation materials|상세 검증 자료|evidence/release_", text))
}
message("PASS: 8 languages, 48 distinct documents, ", length(expected_releases), " rendered releases per language, 19 analysis / 28 method topics, equations, comparison scope, rendered anchors and app-root resolution.")
