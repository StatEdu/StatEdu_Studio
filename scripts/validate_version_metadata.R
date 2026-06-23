script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_version_metadata.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_text <- function(path) {
  paste(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

read_first_line <- function(path) {
  trimws(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8")[[1]])
}

extract_match <- function(text, pattern, label) {
  match <- regexec(pattern, text, perl = TRUE)
  parts <- regmatches(text, match)[[1]]
  if (length(parts) < 2) {
    stop(sprintf("Could not find %s", label), call. = FALSE)
  }
  parts[[2]]
}

assert_equal <- function(actual, expected, label) {
  if (is.null(actual) || length(actual) == 0) {
    stop(sprintf("%s missing", label), call. = FALSE)
  }
  if (!identical(actual, expected)) {
    stop(sprintf("%s mismatch: expected %s, found %s", label, expected, paste(actual, collapse = ", ")), call. = FALSE)
  }
}

message("Checking version metadata...")

version <- read_first_line("VERSION")
if (!grepl("^\\d+\\.\\d+\\.\\d+$", version)) {
  stop(sprintf("VERSION must use semantic version format, found: %s", version), call. = FALSE)
}

read_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required for version metadata validation.", call. = FALSE)
  }
  jsonlite::fromJSON(file.path(repo_root, path), simplifyVector = FALSE)
}

electron_package <- read_json("packaging/electron/package.json")
electron_lock <- read_json("packaging/electron/package-lock.json")

assert_equal(electron_package$version, version, "packaging/electron/package.json version")
assert_equal(electron_lock$version, version, "packaging/electron/package-lock.json root version")
root_package <- electron_lock$packages[[which(names(electron_lock$packages) == "")[[1]]]]
assert_equal(root_package$version, version, "packaging/electron/package-lock.json package version")

readme <- read_text("README.md")
readme_current <- extract_match(readme, "Current development version: `([^`]+)`", "README current development version")
assert_equal(readme_current, version, "README current development version")

readme_citation <- extract_match(readme, "\\(Version ([0-9]+\\.[0-9]+\\.[0-9]+)\\) \\[Computer software\\]", "README citation version")
assert_equal(readme_citation, version, "README citation version")

citation <- read_text("CITATION.cff")
citation_version <- extract_match(citation, '(?m)^version: "([^"]+)"', "CITATION.cff version")
assert_equal(citation_version, version, "CITATION.cff version")

cat(sprintf("Version metadata validation passed: %s\n", version))
