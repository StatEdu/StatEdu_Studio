script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_brand_metadata.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_text <- function(path) {
  paste(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

assert_contains <- function(path, pattern, label, fixed = TRUE) {
  text <- read_text(path)
  found <- if (fixed) {
    grepl(pattern, text, fixed = TRUE)
  } else {
    grepl(pattern, text, perl = TRUE)
  }
  if (!found) {
    stop(sprintf("%s missing from %s", label, path), call. = FALSE)
  }
}

assert_not_contains <- function(path, pattern, label, fixed = TRUE) {
  text <- read_text(path)
  found <- if (fixed) {
    grepl(pattern, text, fixed = TRUE)
  } else {
    grepl(pattern, text, perl = TRUE)
  }
  if (found) {
    stop(sprintf("%s found in %s", label, path), call. = FALSE)
  }
}

read_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required for brand metadata validation.", call. = FALSE)
  }
  jsonlite::fromJSON(file.path(repo_root, path), simplifyVector = FALSE)
}

message("Checking brand metadata...")

visible_product_files <- c(
  "README.md",
  "CITATION.cff",
  "SOURCE-OFFER.txt",
  "R/app_misc_ui.R",
  "packaging/electron/main.js",
  "packaging/electron/package.json",
  "packaging/electron/scripts/afterPack.js"
)

for (path in visible_product_files) {
  assert_not_contains(path, "EasyFlow Statistics", "legacy visible product name")
  assert_not_contains(path, "EasyFlow_Statistics_Beta", "legacy installer product name")
}

assert_contains("README.md", "# **StatEdu Studio**", "README product title")
assert_contains("CITATION.cff", 'title: "StatEdu Studio"', "CITATION product title")
assert_contains("SOURCE-OFFER.txt", "StatEdu Studio Source Code and License Notice", "source offer product title")
assert_contains("R/app_misc_ui.R", 'h2("StatEdu Studio")', "About dialog product title")
assert_contains("R/app_misc_ui.R", "StatEdu Studio source availability and application license.", "About source/license product text")
assert_contains("packaging/electron/main.js", 'app.setName("StatEdu Studio Beta")', "Electron app display name")
assert_contains("packaging/electron/scripts/afterPack.js", '"ProductName", "StatEdu Studio Beta"', "Windows executable product name")

electron_package <- read_json("packaging/electron/package.json")

assert_equal <- function(actual, expected, label) {
  if (is.null(actual) || length(actual) == 0) {
    stop(sprintf("%s missing", label), call. = FALSE)
  }
  if (!identical(actual, expected)) {
    stop(sprintf("%s mismatch: expected %s, found %s", label, expected, paste(actual, collapse = ", ")), call. = FALSE)
  }
}

assert_equal(
  electron_package$build$productName,
  "StatEdu Studio Beta",
  "Electron build productName"
)
assert_equal(
  electron_package$build$nsis$shortcutName,
  "StatEdu Studio Beta",
  "Electron NSIS shortcutName"
)

assert_contains("packaging/electron/main.js", "EASYFLOW_TOKEN", "legacy token compatibility identifier")
assert_contains("docs/RELEASE_CHECKLIST.md", "Keep compatibility identifiers", "compatibility policy")
assert_contains("docs/RELEASE_CHECKLIST.md", ".efs-settings", "legacy settings compatibility note")

cat("Brand metadata validation passed.\n")
