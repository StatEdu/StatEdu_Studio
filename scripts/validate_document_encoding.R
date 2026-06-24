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
