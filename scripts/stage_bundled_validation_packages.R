# Stage pinned release-validation packages and their recursive dependency
# closure into the bundled Electron R runtime. No packages are downloaded.

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  match <- args[startsWith(args, prefix)]
  if (length(match) == 0L) {
    return(default)
  }
  sub(prefix, "", match[[1L]], fixed = TRUE)
}

has_flag <- function(name) {
  paste0("--", name) %in% args
}

repo_root <- normalizePath(arg_value("repo-root", getwd()), winslash = "/", mustWork = TRUE)
runtime_root <- normalizePath(arg_value("runtime-root", ""), winslash = "/", mustWork = TRUE)
source_library <- normalizePath(arg_value("source-library", ""), winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(arg_value("output-dir", repo_root), winslash = "/", mustWork = FALSE)
execute <- has_flag("execute")
runtime_library <- normalizePath(file.path(runtime_root, "library"), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "app_bootstrap.R"), local = TRUE)
source(file.path(repo_root, "scripts", "validate_bundled_validation_packages.R"), local = TRUE)

validation_roots <- names(bundled_validation_packages)
if (length(validation_roots) == 0L || any(!nzchar(validation_roots))) {
  stop("No bundled validation package contract is defined.", call. = FALSE)
}
expected_contract <- bundled_validation_read_expected_contract(repo_root)
bundled_validation_assert_root_contract(
  expected_contract, validation_roots, bundled_validation_packages
)

library_paths <- unique(c(runtime_library, source_library, .libPaths()))
library_paths <- library_paths[dir.exists(library_paths)]
package_databases <- lapply(library_paths, function(path) {
  installed.packages(lib.loc = path, noCache = TRUE)
})
package_database <- do.call(rbind, package_databases)
package_database <- package_database[!duplicated(rownames(package_database)), , drop = FALSE]

# The pinned root must come from the explicit validation library, while shared
# dependencies prefer the freshly-copied runtime. In particular, staging cSEM
# must never downgrade the runtime's separately governed lavaan version.
source_database <- installed.packages(lib.loc = source_library, noCache = TRUE)
source_roots <- intersect(validation_roots, rownames(source_database))
if (length(source_roots) > 0L) {
  package_database <- rbind(
    source_database[source_roots, , drop = FALSE],
    package_database[setdiff(rownames(package_database), source_roots), , drop = FALSE]
  )
}

missing_roots <- setdiff(validation_roots, rownames(package_database))
if (length(missing_roots) > 0L) {
  stop(
    "Bundled validation package(s) missing from the selected source libraries: ",
    paste(missing_roots, collapse = ", "),
    call. = FALSE
  )
}

actual_root_versions <- package_database[validation_roots, "Version"]
expected_root_versions <- unname(bundled_validation_packages[validation_roots])
version_mismatch <- validation_roots[actual_root_versions != expected_root_versions]
if (length(version_mismatch) > 0L) {
  details <- paste0(
    version_mismatch, " expected ", bundled_validation_packages[version_mismatch],
    ", found ", actual_root_versions[match(version_mismatch, validation_roots)]
  )
  stop("Bundled validation package version mismatch: ", paste(details, collapse = "; "), call. = FALSE)
}

dependency_map <- tools::package_dependencies(
  validation_roots,
  db = package_database,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)
closure <- sort(unique(c(validation_roots, unlist(dependency_map, use.names = FALSE))))
base_packages <- rownames(package_database)[
  !is.na(package_database[, "Priority"]) &
    package_database[, "Priority"] %in% c("base", "recommended")
]
required_closure <- setdiff(closure, base_packages)
missing_closure <- setdiff(required_closure, rownames(package_database))
if (length(missing_closure) > 0L) {
  stop(
    "Bundled validation dependency closure is incomplete: ",
    paste(missing_closure, collapse = ", "),
    call. = FALSE
  )
}
source_contract <- bundled_validation_canonical_records(data.frame(
  Package = required_closure,
  Version = unname(package_database[required_closure, "Version"]),
  stringsAsFactors = FALSE,
  check.names = FALSE
))
bundled_validation_assert_no_base_or_recommended(expected_contract, package_database)
bundled_validation_assert_exact_contract(
  expected_contract,
  source_contract,
  missing_message = "Bundled validation dependency closure is incomplete",
  unexpected_message = "Bundled validation dependency closure has unexpected package(s)",
  version_message = "Bundled validation dependency version mismatch"
)
required_closure <- source_contract$Package

runtime_prefix <- paste0(runtime_library, "/")
rows <- lapply(required_closure, function(package) {
  source_path <- normalizePath(
    file.path(package_database[package, "LibPath"], package),
    winslash = "/",
    mustWork = TRUE
  )
  target_path <- file.path(runtime_library, package)
  target_exists <- dir.exists(target_path)
  target_version <- if (target_exists) {
    description <- read.dcf(file.path(target_path, "DESCRIPTION"))[1L, , drop = TRUE]
    unname(description[["Version"]])
  } else {
    ""
  }
  source_version <- unname(package_database[package, "Version"])
  same_path <- identical(tolower(source_path), tolower(normalizePath(target_path, winslash = "/", mustWork = FALSE)))
  action <- if (same_path || (target_exists && identical(target_version, source_version))) {
    "keep"
  } else if (target_exists) {
    if (execute) "replace" else "would-replace"
  } else {
    if (execute) "copy" else "would-copy"
  }

  if (execute && action %in% c("replace", "copy")) {
    normalized_target <- normalizePath(target_path, winslash = "/", mustWork = FALSE)
    if (!startsWith(normalized_target, runtime_prefix)) {
      stop("Refusing to modify a package outside the runtime library: ", target_path, call. = FALSE)
    }
    if (dir.exists(target_path)) {
      unlink(target_path, recursive = TRUE, force = TRUE)
    }
    copied <- file.copy(source_path, runtime_library, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
    if (!isTRUE(copied) || !dir.exists(target_path)) {
      stop("Failed to stage bundled validation package: ", package, call. = FALSE)
    }
  }

  data.frame(
    Package = package,
    Version = source_version,
    License = unname(package_database[package, "License"]),
    SourceLibrary = dirname(source_path),
    Action = action,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

report <- do.call(rbind, rows)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  report,
  file.path(output_dir, "bundled_validation_packages.lock.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (execute) {
  target_database <- installed.packages(lib.loc = runtime_library, noCache = TRUE)
  target_contract <- bundled_validation_closure_records(
    target_database,
    validation_roots,
    incomplete_message = "Staged runtime validation closure is incomplete"
  )
  bundled_validation_assert_no_base_or_recommended(expected_contract, target_database)
  bundled_validation_assert_exact_contract(
    expected_contract,
    target_contract,
    missing_message = "Staged runtime validation closure is incomplete",
    unexpected_message = "Staged runtime validation dependency closure has unexpected package(s)",
    version_message = "Staged runtime validation dependency version mismatch"
  )
}

cat("Bundled validation package staging ", if (execute) "completed" else "dry-run completed", ".\n", sep = "")
cat("Pinned roots: ", paste(paste0(validation_roots, " ", bundled_validation_packages), collapse = ", "), "\n", sep = "")
cat("Recursive non-base closure: ", length(required_closure), " package(s).\n", sep = "")
cat("Expected lock SHA-256: ", attr(expected_contract, "sha256"), "\n", sep = "")
cat("Lock report: ", file.path(output_dir, "bundled_validation_packages.lock.csv"), "\n", sep = "")
