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
assert_contains("SOURCE-OFFER.txt", "GPL-2.0-or-later", "source offer GPL license identifier")
assert_contains("SOURCE-OFFER.txt", "THIRD-PARTY-NOTICES.txt", "source offer third-party notices reference")
assert_contains("SOURCE-OFFER.txt", "license_report.csv", "source offer license report reference")
assert_contains("SOURCE-OFFER.txt", "LICENSES directory", "source offer license texts reference")
assert_contains("R/app_misc_ui.R", 'h2("StatEdu Studio")', "About dialog product title")
assert_contains("R/app_misc_ui.R", "StatEdu Studio source availability and application license.", "About source/license product text")
assert_contains("packaging/electron/main.js", "function appDisplayName()", "Electron dynamic app display name helper")
assert_contains("packaging/electron/main.js", "app.setName(appDisplayName())", "Electron app display name")
assert_contains("packaging/electron/scripts/afterPack.js", "context.packager.appInfo.productName", "Windows executable dynamic product name")
assert_contains("packaging/electron/scripts/afterPack.js", '"OriginalFilename", exeName', "Windows executable dynamic original filename")

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
  "StatEdu Studio",
  "Electron build productName"
)
assert_equal(
  electron_package$build$nsis$shortcutName,
  "StatEdu Studio",
  "Electron NSIS shortcutName"
)
assert_equal(
  electron_package$build$win$artifactName,
  'StatEdu_Studio_Setup_${version}.${ext}',
  "Electron final installer artifactName"
)
assert_contains("scripts/build_electron_beta.ps1", "Sync-ElectronPackageMetadata", "Electron package metadata sync")
assert_contains("scripts/build_electron_beta.ps1", "StatEdu_Studio_Setup", "final 1.0 installer artifact prefix")
assert_contains("scripts/build_electron_beta.ps1", "com.statedu.studio", "final 1.0 app id")
assert_contains("scripts/build_electron_release.ps1", "build_electron_beta.ps1", "release build wrapper delegates to compatibility build script")

assert_contains("packaging/electron/main.js", "STATEDU_TOKEN", "Electron token handoff")
assert_contains("docs/RELEASE_CHECKLIST.md", "Keep backward-compatible internal identifiers", "compatibility policy")
assert_contains("docs/RELEASE_CHECKLIST.md", "Do not expose legacy `.efs-settings` or `.json` settings filters", "legacy settings dialog non-exposure note")
assert_contains("scripts/generate_oss_notices.R", "THIRD-PARTY-NOTICES.txt", "OSS notice output file")
assert_contains("scripts/generate_oss_notices.R", "license_report.csv", "OSS license report output")
assert_contains("scripts/generate_oss_notices.R", "public releases must include source code, documentation, example data, and validation notes", "OSS notice public source release policy")

cat("Brand metadata validation passed.\n")
