script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_document_encoding.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

message("Checking documentation encoding...")

read_bytes_text <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size == 0) {
    return("")
  }
  text <- readChar(path, nchars = size, useBytes = TRUE)
  Encoding(text) <- "bytes"
  text
}

assert_valid_utf8 <- function(path) {
  text <- read_bytes_text(path)
  converted <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA)
  if (is.na(converted)) {
    stop(sprintf("Documentation file is not valid UTF-8: %s", path), call. = FALSE)
  }
  if (grepl("\uFFFD", converted, fixed = TRUE)) {
    stop(sprintf("Documentation file contains replacement characters: %s", path), call. = FALSE)
  }
  TRUE
}

tracked_document_patterns <- c("\\.md$", "\\.txt$", "\\.cff$")
tracked_files <- system2("git", c("ls-files"), stdout = TRUE)
doc_files <- tracked_files[grepl(paste(tracked_document_patterns, collapse = "|"), tracked_files, ignore.case = TRUE)]
if (length(doc_files) == 0) {
  stop("No tracked documentation files found.", call. = FALSE)
}

invisible(vapply(doc_files, assert_valid_utf8, logical(1)))

current_version <- "0.9.42"
current_version_docs <- c(
  "docs/ANALYSIS_METHODS_KO.md",
  "docs/METHOD_NOTES_KO.md",
  "docs/USER_GUIDE_KO.md"
)
stale_current_version_pattern <- "0\\.9\\.(3[0-9]|40|41)"

assert_current_doc_version <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl(current_version, text, fixed = TRUE)) {
    stop(sprintf("Current documentation file does not mention %s: %s", current_version, path), call. = FALSE)
  }
  stale_matches <- regmatches(text, gregexpr(stale_current_version_pattern, text, perl = TRUE))[[1]]
  stale_matches <- unique(stale_matches[stale_matches != "-1"])
  if (length(stale_matches) > 0) {
    stop(
      sprintf(
        "Current documentation file contains stale version reference(s): %s (%s)",
        path,
        paste(stale_matches, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  TRUE
}

invisible(vapply(current_version_docs, assert_current_doc_version, logical(1)))

product_plan <- paste(readLines("docs/PRODUCT_PLAN_KO.md", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
required_product_plan_terms <- c(
  "StatEdu Studio",
  "1.0",
  "UI",
  "validate_stabilization.ps1 -Full",
  "RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md"
)
missing_terms <- required_product_plan_terms[!vapply(required_product_plan_terms, grepl, logical(1), product_plan, fixed = TRUE)]
if (length(missing_terms) > 0) {
  stop(sprintf("PRODUCT_PLAN_KO.md missing expected term(s): %s", paste(missing_terms, collapse = ", ")), call. = FALSE)
}

cat(sprintf("Documentation encoding validation passed: %d tracked documentation files\n", length(doc_files)))
