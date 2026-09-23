# Validate exact package versions governed by bundled runtime implementation
# contracts. With --runtime-root, only that runtime library is inspected;
# otherwise the selected .libPaths() are checked for the pre-build gate.

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  match <- args[startsWith(args, prefix)]
  if (length(match) == 0L) {
    return(default)
  }
  sub(prefix, "", match[[1L]], fixed = TRUE)
}

repo_root <- normalizePath(arg_value("repo-root", getwd()), winslash = "/", mustWork = TRUE)
runtime_root_value <- arg_value("runtime-root", "")
source(file.path(repo_root, "R", "app_bootstrap.R"), local = TRUE)

if (length(bundled_runtime_package_versions) == 0L || any(!nzchar(names(bundled_runtime_package_versions)))) {
  stop("No bundled runtime package version contract is defined.", call. = FALSE)
}

if (nzchar(runtime_root_value)) {
  runtime_root <- normalizePath(runtime_root_value, winslash = "/", mustWork = TRUE)
  runtime_library <- normalizePath(file.path(runtime_root, "library"), winslash = "/", mustWork = TRUE)
  database <- installed.packages(lib.loc = runtime_library, noCache = TRUE)
  scope <- paste0("bundled runtime library ", runtime_library)
} else {
  database <- installed.packages(noCache = TRUE)
  database <- database[!duplicated(rownames(database)), , drop = FALSE]
  scope <- paste0("selected R library paths ", paste(.libPaths(), collapse = "; "))
}

packages <- names(bundled_runtime_package_versions)
missing <- setdiff(packages, rownames(database))
if (length(missing) > 0L) {
  stop(
    "Bundled runtime package version contract failed in ", scope,
    ": missing ", paste(missing, collapse = ", "),
    call. = FALSE
  )
}

actual <- unname(database[packages, "Version"])
expected <- unname(bundled_runtime_package_versions[packages])
mismatch <- packages[actual != expected]
if (length(mismatch) > 0L) {
  details <- paste0(
    mismatch,
    " expected ", bundled_runtime_package_versions[mismatch],
    ", found ", actual[match(mismatch, packages)]
  )
  stop(
    "Bundled runtime package version contract failed in ", scope,
    ": ", paste(details, collapse = "; "),
    call. = FALSE
  )
}

cat(
  "Bundled runtime package version contract passed: ",
  paste(paste0(packages, " ", expected), collapse = ", "),
  ".\n",
  sep = ""
)
