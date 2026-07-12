# Remove non-runtime documentation and test payloads from the bundled R library.
# Package directories are pruned first by prune_r_runtime.R; this script keeps
# executable/runtime payloads such as R, libs, extdata, and www.

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

runtime_root <- normalizePath(arg_value("runtime-root", ""), winslash = "/", mustWork = TRUE)
output_dir <- normalizePath(arg_value("output-dir", getwd()), winslash = "/", mustWork = FALSE)
execute <- has_flag("execute")

runtime_library <- normalizePath(file.path(runtime_root, "library"), winslash = "/", mustWork = TRUE)
if (!dir.exists(runtime_library)) {
  stop("Runtime R library was not found: ", runtime_library, call. = FALSE)
}

prune_dir_names <- c(
  "doc",
  "docs",
  "html",
  "help",
  "demo",
  "demos",
  "examples",
  "tests",
  "testdata",
  "testthat",
  "unitTests",
  "vignettes",
  "include",
  "src",
  "data"
)

library_prefix <- paste0(runtime_library, "/")

package_dirs <- list.dirs(runtime_library, full.names = TRUE, recursive = FALSE)
targets <- list()

for (package_dir in package_dirs) {
  for (dir_name in prune_dir_names) {
    candidate <- file.path(package_dir, dir_name)
    if (dir.exists(candidate)) {
      path <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
      if (!startsWith(path, library_prefix)) {
        stop("Refusing to prune path outside runtime library: ", path, call. = FALSE)
      }
      files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      file_info <- file.info(files)
      file_count <- sum(!is.na(file_info$isdir) & !file_info$isdir)
      byte_count <- sum(file_info$size[!is.na(file_info$isdir) & !file_info$isdir], na.rm = TRUE)
      targets[[length(targets) + 1L]] <- data.frame(
        Package = basename(package_dir),
        Path = substring(path, nchar(library_prefix) + 1L),
        Files = file_count,
        Bytes = byte_count,
        Action = if (execute) "removed" else "would-remove",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
}

report <- if (length(targets) > 0) {
  do.call(rbind, targets)
} else {
  data.frame(Package = character(0), Path = character(0), Files = integer(0), Bytes = numeric(0), Action = character(0), stringsAsFactors = FALSE, check.names = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  report,
  file.path(output_dir, "runtime_content_prune_report.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (execute && nrow(report) > 0) {
  for (relative_path in report$Path) {
    path <- normalizePath(file.path(runtime_library, relative_path), winslash = "/", mustWork = TRUE)
    if (!startsWith(path, library_prefix)) {
      stop("Refusing to remove path outside runtime library: ", path, call. = FALSE)
    }
    unlink(path, recursive = TRUE, force = TRUE)
  }
}

cat("R runtime content prune ", if (execute) "execute" else "dry-run", "\n", sep = "")
cat("Directories: ", nrow(report), "\n", sep = "")
cat("Files: ", sum(report$Files), "\n", sep = "")
cat("Bytes: ", sum(report$Bytes), "\n", sep = "")
cat("Report: ", file.path(output_dir, "runtime_content_prune_report.csv"), "\n", sep = "")
