# Prune bundled R library to the packages required by EasyFlow Statistics.
# Dry-run is the default. Pass --execute to remove extra package directories.

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = "") {
  prefix <- paste0("--", name, "=")
  match <- args[startsWith(args, prefix)]
  if (length(match) == 0) {
    return(default)
  }
  sub(prefix, "", match[[1]], fixed = TRUE)
}

has_flag <- function(name) {
  paste0("--", name) %in% args
}

repo_root <- normalizePath(arg_value("repo-root", getwd()), winslash = "/", mustWork = TRUE)
runtime_root <- normalizePath(arg_value("runtime-root", ""), winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(arg_value("output-dir", repo_root), winslash = "/", mustWork = FALSE)
execute <- has_flag("execute")

runtime_library <- normalizePath(file.path(runtime_root, "library"), winslash = "/", mustWork = TRUE)
if (!dir.exists(runtime_library)) {
  stop("Runtime R library was not found: ", runtime_library, call. = FALSE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"), local = TRUE)

db <- installed.packages(lib.loc = runtime_library)
dependency_map <- tools::package_dependencies(
  required_packages,
  db = db,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)

priority_packages <- rownames(db)[db[, "Priority"] %in% c("base", "recommended")]
protected_packages <- c("translations")
keep_packages <- sort(unique(c(
  required_packages,
  unlist(dependency_map, use.names = FALSE),
  priority_packages,
  protected_packages
)))

installed_packages <- sort(rownames(db))
extra_packages <- setdiff(installed_packages, keep_packages)
missing_packages <- setdiff(keep_packages, installed_packages)

report <- data.frame(
  Package = installed_packages,
  Version = db[installed_packages, "Version"],
  License = db[installed_packages, "License"],
  Priority = ifelse(is.na(db[installed_packages, "Priority"]), "", db[installed_packages, "Priority"]),
  Action = ifelse(installed_packages %in% extra_packages, if (execute) "removed" else "would-remove", "keep"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  report,
  file.path(output_dir, "runtime_prune_report.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (length(missing_packages) > 0) {
  stop(
    "Required R package(s) missing from runtime library: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

remove_package_dir <- function(package) {
  path <- normalizePath(file.path(runtime_library, package), winslash = "/", mustWork = TRUE)
  library_prefix <- paste0(runtime_library, "/")
  if (!startsWith(path, library_prefix)) {
    stop("Refusing to remove path outside runtime library: ", path, call. = FALSE)
  }
  unlink(path, recursive = TRUE, force = TRUE)
}

if (execute && length(extra_packages) > 0) {
  vapply(extra_packages, remove_package_dir, logical(1))
}

cat("R runtime prune ", if (execute) "execute" else "dry-run", "\n", sep = "")
cat("Installed packages: ", length(installed_packages), "\n", sep = "")
cat("Kept packages: ", length(intersect(installed_packages, keep_packages)), "\n", sep = "")
cat("Extra packages: ", length(extra_packages), "\n", sep = "")
if (length(extra_packages) > 0) {
  cat("Extra package names: ", paste(extra_packages, collapse = ", "), "\n", sep = "")
}
cat("Report: ", file.path(output_dir, "runtime_prune_report.csv"), "\n", sep = "")
