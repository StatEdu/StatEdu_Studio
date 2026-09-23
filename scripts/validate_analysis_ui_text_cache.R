script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)
if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))
statedu_apply_preferences(statedu_initial_preferences())
options(statedu.app_language = "ko")

cached_function <- analysis_ui_text
invisible(cached_function("__cache_warm__", "ko"))
cache_environment <- environment(cached_function)
stopifnot(exists("labels_cache", envir = cache_environment, inherits = FALSE))
label_keys <- names(get("labels_cache", envir = cache_environment, inherits = FALSE))
stopifnot(length(label_keys) > 300L)

old_source <- system2("git", c("show", "HEAD:R/setup_analysis_ui.R"), stdout = TRUE, stderr = TRUE)
stopifnot(length(old_source) > 0L)
old_path <- tempfile(fileext = ".R")
on.exit(unlink(old_path), add = TRUE)
writeLines(old_source, old_path, useBytes = TRUE)
old_environment <- new.env(parent = globalenv())
sys.source(old_path, envir = old_environment)
uncached_function <- get("analysis_ui_text", envir = old_environment, inherits = FALSE)

inputs <- unique(c(label_keys, "  VARIABLES  ", "Mean +/- SD", "Mean ± SD", "unknown label"))
for (language in c("ko", "en")) {
  before <- vapply(inputs, uncached_function, character(1), language = language)
  after <- vapply(inputs, cached_function, character(1), language = language)
  stopifnot(identical(before, after))
}

cat(sprintf("analysis_ui_text cache validation passed: %d labels x 2 languages.\n", length(inputs)))
