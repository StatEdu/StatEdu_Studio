repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])), ".."), winslash = "/", mustWork = TRUE)
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

make_info <- function(names, measurements) data.frame(name = names, measurement = measurements, stringsAsFactors = FALSE)
profile_ui <- function(name, expression) {
  path <- tempfile(fileext = ".out")
  Rprof(path, interval = 0.001)
  value <- force(expression)
  Rprof(NULL)
  profile <- summaryRprof(path)
  unlink(path)
  cat("\n==", name, "==\n")
  print(utils::head(profile$by.total, 30L))
  invisible(value)
}

set.seed(20260827L)
n <- 180L
group <- rep(c("A", "B", "C"), length.out = n)
x1 <- stats::rnorm(n)
x2 <- stats::rnorm(n)
y <- 0.6 * x1 - 0.3 * x2 + ifelse(group == "B", .3, ifelse(group == "C", .6, 0)) + stats::rnorm(n)

tt_data <- data.frame(y = y, group = rep(c("Control", "Treatment"), each = n / 2L), stringsAsFactors = FALSE)
tt_info <- make_info(names(tt_data), c("continuous", "binary"))
tt_result <- prepare_ttest_anova_results(tt_data, "y", "group", tt_info, options = list(effect_size = TRUE, normality_enabled = FALSE, show_df = TRUE))
profile_ui("t-test results UI cold", ttest_anova_results_ui(tt_result))
profile_ui("t-test results UI warm", ttest_anova_results_ui(tt_result))

anc_data <- data.frame(y = y, group = group, x = x1, stringsAsFactors = FALSE)
anc_info <- make_info(names(anc_data), c("continuous", "category", "continuous"))
anc_result <- prepare_ancova_results(anc_data, "y", "group", "x", anc_info, options = list(show_df = TRUE))
invisible(ancova_results_ui(anc_result))
profile_ui("ANCOVA results UI", ancova_results_ui(anc_result))

reg_data <- data.frame(y = y, x1 = x1, x2 = x2)
reg_info <- make_info(names(reg_data), rep("continuous", 3L))
glm_result <- prepare_generalized_analysis_result(reg_data, "y", c("x1", "x2"), family = "gaussian", se_type = "model", robust = FALSE, variable_info = reg_info)
invisible(generalized_results_panel(glm_result))
profile_ui("GLM results UI", generalized_results_panel(glm_result))
