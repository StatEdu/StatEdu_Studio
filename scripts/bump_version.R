args <- commandArgs(trailingOnly = TRUE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) y else x
}

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript scripts/bump_version.R patch|minor|major [--no-changelog]",
      "",
      "Examples:",
      "  Rscript scripts/bump_version.R patch  # 0.2.0 -> 0.2.1",
      "  Rscript scripts/bump_version.R minor  # 0.2.0 -> 0.3.0",
      "  Rscript scripts/bump_version.R major  # 0.2.0 -> 1.0.0",
      sep = "\n"
    ),
    "\n"
  )
}

if (length(args) < 1 || args[[1]] %in% c("-h", "--help", "help")) {
  usage()
  quit(status = if (length(args) < 1) 1 else 0)
}

bump_type <- args[[1]]
if (!bump_type %in% c("patch", "minor", "major")) {
  usage()
  stop("bump type must be one of: patch, minor, major", call. = FALSE)
}

update_changelog <- !"--no-changelog" %in% args

all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  ""
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = FALSE)
repo_root <- if (nzchar(script_path) && file.exists(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

version_path <- file.path(repo_root, "VERSION")
changelog_path <- file.path(repo_root, "CHANGELOG.md")

if (!file.exists(version_path)) {
  stop("VERSION file was not found: ", version_path, call. = FALSE)
}

current <- trimws(readLines(version_path, warn = FALSE)[[1]])
parts <- strsplit(current, ".", fixed = TRUE)[[1]]
if (length(parts) != 3 || any(!grepl("^[0-9]+$", parts))) {
  stop("VERSION must use semantic version format MAJOR.MINOR.PATCH, found: ", current, call. = FALSE)
}

version <- as.integer(parts)
names(version) <- c("major", "minor", "patch")

if (identical(bump_type, "major")) {
  version[["major"]] <- version[["major"]] + 1L
  version[["minor"]] <- 0L
  version[["patch"]] <- 0L
} else if (identical(bump_type, "minor")) {
  version[["minor"]] <- version[["minor"]] + 1L
  version[["patch"]] <- 0L
} else {
  version[["patch"]] <- version[["patch"]] + 1L
}

next_version <- paste(version, collapse = ".")
writeLines(next_version, version_path, useBytes = TRUE)

if (isTRUE(update_changelog)) {
  today <- format(Sys.Date(), "%Y-%m-%d")
  heading <- sprintf("## v%s - %s", next_version, today)
  entry <- c(
    heading,
    "",
    "### Changed",
    "",
    "- TBD",
    ""
  )

  if (file.exists(changelog_path)) {
    lines <- readLines(changelog_path, warn = FALSE)
    if (length(lines) > 0 && grepl("^# Changelog\\s*$", lines[[1]])) {
      lines <- c(lines[[1]], "", entry, lines[-1])
    } else {
      lines <- c("# Changelog", "", entry, lines)
    }
  } else {
    lines <- c("# Changelog", "", entry)
  }
  writeLines(lines, changelog_path, useBytes = TRUE)
}

cat(sprintf("Version bumped: %s -> %s\n", current, next_version))
