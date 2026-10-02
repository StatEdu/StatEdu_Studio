# Run with --vanilla and the staged application as working directory.
stopifnot(Sys.info()[["sysname"]] == "Darwin",
          grepl("aarch64|arm64", R.version$arch),
          as.character(getRversion()) == "4.5.3")
expected_home <- normalizePath(Sys.getenv("R_HOME"), mustWork = TRUE)
stopifnot(identical(normalizePath(R.home()), expected_home))
initial_libraries <- normalizePath(.libPaths(), mustWork = TRUE)
stopifnot(length(initial_libraries) > 0L,
          all(startsWith(initial_libraries, paste0(expected_home, "/"))))
.libPaths(file.path(expected_home, "library"))
source("R/app_bootstrap.R", local = TRUE)
for (package in required_packages) {
  location <- find.package(package)
  stopifnot(startsWith(normalizePath(location), paste0(expected_home, "/")))
  loadNamespace(package)
}
for (package in names(bundled_runtime_package_versions)) {
  stopifnot(packageDescription(package, fields = "Version") == bundled_runtime_package_versions[[package]])
}
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_arg) == 1L)
tool_directory <- dirname(normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE))
validation_lock <- read.csv(file.path(tool_directory, "bundled_validation_packages.expected.csv"),
                            colClasses = "character", check.names = FALSE)
stopifnot(identical(names(validation_lock), c("Package", "Version")),
          nrow(validation_lock) == 72L, !anyNA(validation_lock),
          !anyDuplicated(validation_lock$Package))
for (i in seq_len(nrow(validation_lock))) {
  package <- validation_lock$Package[[i]]
  stopifnot(packageDescription(package, fields = "Version") == validation_lock$Version[[i]])
  location <- normalizePath(find.package(package), mustWork = TRUE)
  stopifnot(startsWith(location, paste0(expected_home, "/")))
  loadNamespace(package)
}
full_lock_path <- file.path(tool_directory, "../runtime-packages.lock.csv")
if (!file.exists(full_lock_path)) full_lock_path <- file.path(tool_directory, "../packaging/macos/runtime-packages.lock.csv")
full_lock <- read.csv(full_lock_path, colClasses = "character")
installed <- installed.packages(lib.loc = file.path(expected_home, "library"), noCache = TRUE)
stopifnot(setequal(full_lock$Package, installed[, "Package"]),
          all(full_lock$Version == installed[match(full_lock$Package, installed[, "Package"]), "Version"]))
for (package in loadedNamespaces()) {
  # R's base namespace has no package path metadata; R.home() is checked above.
  if (identical(package, "base")) next
  location <- normalizePath(getNamespaceInfo(asNamespace(package), "path"), mustWork = TRUE)
  stopifnot(startsWith(location, paste0(expected_home, "/")))
}
cat("PASS: staged R 4.5.3 arm64, required namespaces, runtime pins and 72 validation package versions\n")
cat("PASS: complete package inventory matches the runtime lock\n")
cat("This does not certify relocation, signing, numerical parity or export behavior.\n")
