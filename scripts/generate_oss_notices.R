# Generate third-party license notices for the bundled EasyFlow runtime.

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  match <- args[startsWith(args, prefix)]
  if (length(match) == 0) {
    return(default)
  }
  sub(prefix, "", match[[1]], fixed = TRUE)
}

repo_root <- normalizePath(arg_value("repo-root", getwd()), winslash = "/", mustWork = TRUE)
runtime_root <- normalizePath(arg_value("runtime-root", ""), winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(arg_value("output-dir", repo_root), winslash = "/", mustWork = FALSE)

runtime_library <- file.path(runtime_root, "library")
if (!dir.exists(runtime_library)) {
  stop("Runtime R library was not found: ", runtime_library, call. = FALSE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"), local = TRUE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
licenses_dir <- file.path(output_dir, "LICENSES")
if (dir.exists(licenses_dir)) {
  unlink(licenses_dir, recursive = TRUE, force = TRUE)
}
dir.create(licenses_dir, recursive = TRUE, showWarnings = FALSE)

risk_label <- function(license) {
  ifelse(grepl("AGPL", license, ignore.case = TRUE), "AGPL",
    ifelse(grepl("LGPL", license, ignore.case = TRUE), "LGPL",
      ifelse(grepl("GPL", license, ignore.case = TRUE), "GPL", "permissive-or-other")
    )
  )
}

safe_file_name <- function(value) {
  value <- gsub("[^A-Za-z0-9._-]+", "_", as.character(value))
  value <- gsub("_+", "_", value)
  trimws(value, whitespace = "_")
}

package_url <- function(description) {
  url <- description_field(description, "URL")
  if (!is.na(url) && nzchar(url)) {
    return(url)
  }
  package <- description_field(description, "Package")
  if (!is.na(package) && nzchar(package)) {
    return(paste0("https://cran.r-project.org/package=", package))
  }
  ""
}

description_field <- function(description, name, default = "") {
  if (!name %in% names(description)) {
    return(default)
  }
  value <- description[[name]]
  if (is.na(value)) default else value
}

package_license_files <- function(package_dir) {
  candidates <- c(
    "LICENSE", "LICENCE", "LICENSE.md", "LICENCE.md", "LICENSE.txt", "LICENCE.txt",
    "COPYING", "COPYING.md", "COPYING.txt", "NOTICE", "NOTICE.md", "NOTICE.txt"
  )
  paths <- file.path(package_dir, candidates)
  paths[file.exists(paths)]
}

copy_package_licenses <- function(package, version, package_dir) {
  files <- package_license_files(package_dir)
  if (length(files) == 0) {
    return("")
  }
  copied <- character(0)
  for (file in files) {
    target <- file.path(
      licenses_dir,
      paste0("R-", safe_file_name(package), "-", safe_file_name(version), "-", safe_file_name(basename(file)))
    )
    file.copy(file, target, overwrite = TRUE)
    copied <- c(copied, basename(target))
  }
  paste(copied, collapse = "; ")
}

copy_runtime_license_files <- function() {
  copied <- character(0)
  runtime_candidates <- c(
    file.path(runtime_root, "COPYING"),
    file.path(runtime_root, "COPYING.LIB"),
    file.path(runtime_root, "doc", "COPYING"),
    file.path(runtime_root, "doc", "COPYING.LIB")
  )
  for (file in runtime_candidates[file.exists(runtime_candidates)]) {
    target <- file.path(licenses_dir, paste0("R-runtime-", safe_file_name(basename(file))))
    file.copy(file, target, overwrite = TRUE)
    copied <- c(copied, basename(target))
  }
  copied
}

db <- installed.packages(lib.loc = runtime_library)
packages <- sort(rownames(db))

package_scope <- function(package, description) {
  priority <- description_field(description, "Priority")
  if (package %in% required_packages) {
    return("Direct EFS package")
  }
  if (!is.na(priority) && priority %in% c("base", "recommended")) {
    return("R base/recommended")
  }
  "Bundled dependency"
}

rows <- lapply(packages, function(package) {
  package_dir <- file.path(runtime_library, package)
  description <- read.dcf(file.path(package_dir, "DESCRIPTION"))[1, ]
  license <- description_field(description, "License")
  version <- description_field(description, "Version")
  data.frame(
    Component = paste0("R package: ", package),
    Scope = package_scope(package, description),
    Package = package,
    Version = version,
    License = license,
    Risk = risk_label(license),
    URL = package_url(description),
    LicenseFiles = copy_package_licenses(package, version, package_dir),
    Notes = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

report <- do.call(rbind, rows)

runtime_license_files <- copy_runtime_license_files()
runtime_row <- data.frame(
  Component = "R runtime",
  Scope = "R runtime",
  Package = "R",
  Version = paste(R.version$major, R.version$minor, sep = "."),
  License = "GPL-2 | GPL-3; LGPL applies to selected R libraries where stated",
  Risk = "GPL",
  URL = "https://www.r-project.org/Licenses/",
  LicenseFiles = paste(runtime_license_files, collapse = "; "),
  Notes = "Bundled runtime. Public releases should provide corresponding EasyFlow source and preserve R license notices.",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

report <- rbind(runtime_row, report)
scope_order <- c("R runtime", "Direct EFS package", "Bundled dependency", "R base/recommended")
report$Scope <- factor(report$Scope, levels = scope_order)
report <- report[order(report$Scope, report$Risk, report$Component), ]
report$Scope <- as.character(report$Scope)

write.csv(
  report,
  file.path(output_dir, "license_report.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

notice_lines <- c(
  "Third-Party Notices",
  "===================",
  "",
  "EasyFlow Statistics bundles third-party software components, including Electron, Chromium, R, and CRAN packages.",
  "This notice is generated from the bundled R runtime at build time.",
  "",
  "License summary:",
  paste0("- ", names(sort(table(report$Scope))), ": ", as.integer(sort(table(report$Scope)))),
  "",
  "License risk summary:",
  paste0("- ", names(sort(table(report$Risk))), ": ", as.integer(sort(table(report$Risk)))),
  "",
  "R runtime:",
  sprintf("- %s %s -- %s", runtime_row$Package, runtime_row$Version, runtime_row$License),
  "",
  "Direct EFS R packages:",
  sprintf("- %s %s -- %s", report$Package[report$Scope == "Direct EFS package"], report$Version[report$Scope == "Direct EFS package"], report$License[report$Scope == "Direct EFS package"]),
  "",
  "Bundled R package dependencies:",
  sprintf("- %s %s -- %s", report$Package[report$Scope == "Bundled dependency"], report$Version[report$Scope == "Bundled dependency"], report$License[report$Scope == "Bundled dependency"]),
  "",
  "R base/recommended packages bundled with the runtime:",
  sprintf("- %s %s -- %s", report$Package[report$Scope == "R base/recommended"], report$Version[report$Scope == "R base/recommended"], report$License[report$Scope == "R base/recommended"]),
  "",
  "Full package license metadata is available in license_report.csv.",
  "License text files copied from bundled packages are available in LICENSES/.",
  "Electron and Chromium license files are generated by electron-builder in the application output directory.",
  "",
  "Source availability:",
  "EasyFlow Statistics public releases are intended to include source code, documentation, example data, and validation notes.",
  "GPL/LGPL components remain under their original licenses. Modifications to third-party components, if any, must follow those licenses.",
  ""
)

writeLines(notice_lines, file.path(output_dir, "THIRD-PARTY-NOTICES.txt"), useBytes = TRUE)

cat("Generated OSS notices in ", output_dir, "\n", sep = "")
cat("Packages scanned: ", length(packages), "\n", sep = "")
cat("Risk counts:\n")
print(table(report$Risk), quote = FALSE)
