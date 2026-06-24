script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_release_hygiene.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

message("Checking release hygiene...")

tracked <- system2("git", c("ls-files"), stdout = TRUE, stderr = TRUE)
status <- attr(tracked, "status")
if (!is.null(status) && status != 0) {
  stop("git ls-files failed while checking release hygiene.", call. = FALSE)
}

blocked_prefixes <- c(
  "dist",
  "packaging/electron/app",
  "packaging/electron/runtime",
  "packaging/electron/node_modules",
  "output",
  "outputs",
  "scratch",
  "modules/latent_mplus/app/output",
  "modules/latent_mplus/app/outputs",
  "modules/latent_mplus/app/mplus_tmp",
  "modules/latent_mplus/app/settings"
)

has_blocked_prefix <- function(path) {
  any(path == blocked_prefixes | startsWith(path, paste0(blocked_prefixes, "/")))
}

blocked <- tracked[vapply(tracked, function(path) {
  has_blocked_prefix(path) ||
    grepl("(^|/)\\.Rhistory$", path, perl = TRUE) ||
    grepl("(^|/)\\.RData$", path, perl = TRUE) ||
    grepl("(^|/)\\.Ruserdata$", path, perl = TRUE) ||
    grepl("(^|/)[^/]+\\.(log|tmp)$", path, perl = TRUE) ||
    grepl("^settings/[^/]+\\.local\\.json$", path, perl = TRUE)
}, logical(1))]

if (length(blocked) > 0) {
  stop(
    sprintf(
      "Generated or local-only artifact path(s) are tracked by git: %s",
      paste(blocked, collapse = ", ")
    ),
    call. = FALSE
  )
}

cat("Release hygiene validation passed.\n")
