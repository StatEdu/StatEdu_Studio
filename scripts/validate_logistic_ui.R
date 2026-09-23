all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_logistic_ui.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(repo_root)

source("R/app_bootstrap.R")
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

variable_info <- data.frame(
  name = c("change", "satisfaction", "group", "age"),
  measurement = c("binary", "ordered", "category", "continuous"),
  stringsAsFactors = FALSE
)

dependent_candidates <- logistic_dependent_candidates(variable_info$name, variable_info)
stopifnot(identical(dependent_candidates, c("change", "satisfaction", "group")))
stopifnot(identical(logistic_model_family_label("change", variable_info), "Binary logistic regression"))
stopifnot(identical(logistic_model_family_label("satisfaction", variable_info), "Ordinal logistic regression"))
stopifnot(identical(logistic_model_family_label("group", variable_info), "Multinomial logistic regression"))

setup <- logistic_setup_state(
  selected_names = variable_info$name,
  dependents = c("change", "satisfaction"),
  block1 = "age",
  block2 = "satisfaction",
  block3 = "group",
  variable_table = variable_info,
  labels = c(change = "Changed", satisfaction = "Satisfaction"),
  active_block = "block1",
  language = "en"
)
stopifnot(identical(setup$dependents, c("change", "satisfaction")))

html <- as.character(htmltools::renderTags(logistic_setup_panel(setup, NULL))$html)
required_fragments <- c(
  "logistic_available",
  "logistic_y",
  "logistic_block1",
  "Dependent variables (2)",
  "B, SE",
  "95% CI (LLCI, ULCI)",
  "McFadden R",
  "Cox &amp; Snell R",
  "Run logistic",
  "logistic_save_control"
)
for (fragment in required_fragments) {
  if (!grepl(fragment, html, fixed = TRUE)) {
    stop(sprintf("Expected logistic UI fragment not found: %s", fragment), call. = FALSE)
  }
}
if (grepl("Dependent Variable (1/1)", html, fixed = TRUE)) {
  stop("Logistic dependent label should not be capped at one variable.", call. = FALSE)
}
for (removed in c("Method is selected by dependent variable type.", "Complete case analysis is used by default.", "Model</div>")) {
  if (grepl(removed, html, fixed = TRUE)) {
    stop(sprintf("Removed logistic UI fragment is still present: %s", removed), call. = FALSE)
  }
}

probe_result <- list(
  dependent = "satisfaction",
  predictors = "age",
  method = "Ordinal logistic regression",
  n = 100L,
  coef_table = data.frame(OR = 1.2, LLCI = 0.9, ULCI = 1.7, SE = 0.2),
  notes = c(
    "Zero cell found for satisfaction by age; separation is possible.",
    "Reference for sex was not set; minimum value 1 was used.",
    "Performance statistics are apparent (in-sample) diagnostics and do not establish external predictive validity."
  ),
  parallel = list(chisq = 2.4, p = .21, basis = "Final hierarchical model"),
  convergence = list(ok = TRUE, message = "Converged"),
  max_vif = 6.4,
  performance = c(`AUC (apparent)` = .72)
)
options(statedu.app_language = "ko")
ko_assumption_html <- as.character(htmltools::renderTags(logistic_assumption_review_block(list(probe_result), variable_info))$html)
ko_performance_html <- as.character(htmltools::renderTags(logistic_performance_block(list(probe_result), variable_info))$html)
ko_diagnostics_html <- as.character(htmltools::renderTags(logistic_model_notes_block(list(probe_result), variable_info))$html)
ko_assumption_table <- logistic_appendix_table(logistic_assumption_review_data_frame(list(probe_result), variable_info), "ko")
stopifnot(
  identical(
    unname(ko_assumption_table[["항목"]]),
    c("수렴", "비례오즈", "EPV / 희소 셀", "분리", "VIF", "함수 형태", "다항 로짓 IIA", "패키지")
  ),
  grepl("수렴함", ko_assumption_html, fixed = TRUE),
  grepl("비례오즈 가정 충족", ko_assumption_html, fixed = TRUE),
  grepl("최종 위계적 모형", ko_assumption_html, fixed = TRUE),
  grepl("다중공선성 주의", ko_assumption_html, fixed = TRUE),
  grepl("교차표에서 빈 셀", ko_assumption_html, fixed = TRUE),
  grepl("satisfaction", ko_assumption_html, fixed = TRUE),
  grepl("age", ko_assumption_html, fixed = TRUE),
  grepl("표본 내 모형 성능", ko_performance_html, fixed = TRUE),
  grepl("AUC (표본 내)", ko_performance_html, fixed = TRUE),
  grepl(".72", ko_performance_html, fixed = TRUE),
  grepl("표본 내 성능(기술통계 용도)", ko_performance_html, fixed = TRUE),
  grepl("모형 진단", ko_diagnostics_html, fixed = TRUE),
  grepl("sex의 기준 범주가 지정되지 않아 최솟값 1을(를) 사용했습니다.", ko_diagnostics_html, fixed = TRUE),
  !grepl("Converged", ko_assumption_html, fixed = TRUE),
  !grepl("Final hierarchical model", ko_assumption_html, fixed = TRUE),
  !grepl("Apparent model performance", ko_performance_html, fixed = TRUE),
  !grepl("Model diagnostics", ko_diagnostics_html, fixed = TRUE),
  !grepl("Reference for sex was not set", ko_diagnostics_html, fixed = TRUE)
)
options(statedu.app_language = "en")
en_assumption_html <- as.character(htmltools::renderTags(logistic_assumption_review_block(list(probe_result), variable_info))$html)
en_performance_html <- as.character(htmltools::renderTags(logistic_performance_block(list(probe_result), variable_info))$html)
en_diagnostics_html <- as.character(htmltools::renderTags(logistic_model_notes_block(list(probe_result), variable_info))$html)
stopifnot(
  grepl("Converged", en_assumption_html, fixed = TRUE),
  grepl("Proportional odds assumption met", en_assumption_html, fixed = TRUE),
  grepl("Apparent model performance", en_performance_html, fixed = TRUE),
  grepl("Model diagnostics", en_diagnostics_html, fixed = TRUE),
  grepl("Reference for sex was not set; minimum value 1 was used.", en_diagnostics_html, fixed = TRUE),
  grepl("AUC (apparent)", en_performance_html, fixed = TRUE),
  grepl("OR = odds ratio", logistic_main_note("Ordinal logistic regression"), fixed = TRUE)
)

cat("Logistic UI validation passed.\n")
