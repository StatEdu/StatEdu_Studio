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
old_source <- system2("git", c("show", "HEAD:R/setup_analysis_ui.R"), stdout = TRUE, stderr = TRUE)
old_path <- tempfile(fileext = ".R")
on.exit(unlink(old_path), add = TRUE)
writeLines(old_source, old_path, useBytes = TRUE)
old_environment <- new.env(parent = globalenv())
sys.source(old_path, envir = old_environment)
uncached_function <- get("analysis_ui_text", envir = old_environment, inherits = FALSE)

make_info <- function(names, measurements) data.frame(name = names, measurement = measurements, stringsAsFactors = FALSE)
html_md5 <- function(html) {
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw(enc2utf8(html)), path)
  unname(tools::md5sum(path)[[1L]])
}
measure_build <- function(text_function, build, repetitions = 3L) {
  assign("analysis_ui_text", text_function, envir = .GlobalEnv)
  seconds <- numeric(repetitions)
  html <- ""
  for (index in seq_len(repetitions)) {
    gc(FALSE)
    started <- proc.time()[["elapsed"]]
    ui <- build()
    html <- htmltools::renderTags(ui)$html
    seconds[[index]] <- unname(proc.time()[["elapsed"]] - started)
  }
  list(seconds = seconds, html = html, md5 = html_md5(html), bytes = nchar(html, type = "bytes"))
}

set.seed(20260827L)
n <- 180L
group2 <- rep(c("Control", "Treatment"), each = n / 2L)
group3 <- rep(c("A", "B", "C"), length.out = n)
x1 <- stats::rnorm(n)
x2 <- stats::rnorm(n)
y <- .6 * x1 - .3 * x2 + ifelse(group2 == "Treatment", .45, 0) + stats::rnorm(n)

anc_data <- data.frame(y = y, group = group3, x = x1, stringsAsFactors = FALSE)
anc_result <- prepare_ancova_results(anc_data, "y", "group", "x", make_info(names(anc_data), c("continuous", "category", "continuous")), options = list(show_df = TRUE))
mixed_data <- data.frame(group = group2, pre = 50 + x1, post = 51 + x1 + ifelse(group2 == "Treatment", 2, 0), post2 = 52 + x1 + ifelse(group2 == "Treatment", 3, 0), stringsAsFactors = FALSE)
mixed_result <- prepare_mixed_rm_anova_results(mixed_data, "group", c("pre", "post", "post2"), variable_info = make_info(names(mixed_data), c("binary", "continuous", "continuous", "continuous")), options = list(assumption_check = TRUE, posthoc = TRUE, posthoc_adjustment = "holm", time_labels = c("Pre", "Post", "Post 2")))
log_data <- data.frame(outcome = factor(stats::rbinom(n, 1L, stats::plogis(-.3 + .7 * x1 - .25 * x2))), x1 = x1, x2 = x2)
log_result <- prepare_logistic_analysis_results(log_data, "outcome", c("x1", "x2"), variable_info = make_info(names(log_data), c("binary", "continuous", "continuous")))
reg_data <- data.frame(y = y, x1 = x1, x2 = x2)
glm_result <- prepare_generalized_analysis_result(reg_data, "y", c("x1", "x2"), family = "gaussian", se_type = "model", robust = FALSE, variable_info = make_info(names(reg_data), rep("continuous", 3L)))

cases <- list(
  ANCOVA = function() ancova_results_ui(anc_result),
  `Repeated-measures ANOVA` = function() mixed_rm_anova_results_ui(mixed_result),
  `Logistic regression` = function() logistic_results_panel(log_result),
  GLM = function() generalized_results_panel(glm_result)
)

rows <- lapply(names(cases), function(name) {
  before <- measure_build(uncached_function, cases[[name]])
  after <- measure_build(cached_function, cases[[name]])
  stopifnot(identical(before$html, after$html), identical(before$md5, after$md5), identical(before$bytes, after$bytes))
  data.frame(
    analysis = name,
    before_median_seconds = stats::median(before$seconds),
    after_median_seconds = stats::median(after$seconds),
    reduction_percent = 100 * (1 - stats::median(after$seconds) / stats::median(before$seconds)),
    html_bytes = before$bytes,
    html_md5 = before$md5,
    stringsAsFactors = FALSE
  )
})
assign("analysis_ui_text", cached_function, envir = .GlobalEnv)
result <- do.call(rbind, rows)
print(result, row.names = FALSE)

output_path <- trimws(Sys.getenv("STATEDU_BENCHMARK_OUTPUT", ""))
if (nzchar(output_path)) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, output_path)
}
