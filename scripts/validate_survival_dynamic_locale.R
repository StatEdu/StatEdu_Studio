all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_survival_dynamic_locale.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source("R/app_bootstrap.R")
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))

render_html <- function(tag) paste(as.character(htmltools::renderTags(tag)$html), collapse = "\n")
expect_true <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
expect_contains <- function(text, pattern, message) expect_true(grepl(pattern, text, fixed = TRUE), message)
expect_not_contains <- function(text, pattern, message) expect_true(!grepl(pattern, text, fixed = TRUE), message)
count_fixed <- function(text, pattern) {
  hits <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  if (length(hits) == 1L && identical(hits[[1L]], -1L)) 0L else length(hits)
}

message("Checking Cox appendix dynamic localization...")
fixture <- utils::read.csv(file.path("scripts", "fixtures", "survival_validation.csv"), check.names = FALSE)
cox <- prepare_cox_analysis_result(
  fixture,
  time = "time",
  event = "status",
  covariates = c("age", "sex"),
  event_value = "1"
)
cox_ko <- render_html(survival_cox_results_panel(cox, language = "ko"))
cox_en <- render_html(survival_cox_results_panel(cox, language = "en"))
for (phrase in c(
  "Martingale 잔차는 함수형태 검토를",
  "표준화 DFBETAS는 경험적 2/sqrt(N)",
  "VIF는 Cox 설계행렬의 각 열에 대해 계산",
  "B = 로그 위험계수",
  "시간 원점", "시간 단위", "진입 시간 변수", "구간 시작", "구간 종료",
  "대상자 ID", "층화 변수", "군집 변수", "모수당 사건 수",
  "고유 사건시점 수", "동률 사건시점 수", "동률 시점 사건 비율", "주제"
)) expect_contains(cox_ko, phrase, paste("Korean Cox appendix must contain:", phrase))
for (phrase in c(
  "Martingale residuals support functional-form review",
  "Standardized DFBETAS are compared",
  "VIF is computed for each Cox design-matrix column",
  ">Time origin<", ">Entry variable<", ">Distinct event times<",
  ">Proportion of events at tied times<", ">Topic<"
)) {
  expect_not_contains(cox_ko, phrase, paste("Korean Cox appendix must not leak:", phrase))
  expect_contains(cox_en, phrase, paste("English Cox appendix must retain:", phrase))
}
expect_contains(cox_ko, "values in parentheses are p values", "The Cox journal-table note must remain English under the Korean UI.")
expect_contains(cox_ko, "The likelihood-ratio test evaluates the overall model; PH GLOBAL evaluates the proportional-hazards assumption", "The Cox model-test explanation must remain inside the journal-table note.")
expect_true(
  count_fixed(cox_ko, "values in parentheses are p values") == 1L,
  "The Cox journal table must have one concise, non-duplicated note."
)

stability_probe <- cox
stability_probe$collinearity_table$VIF[[1]] <- 12
stability_probe$condition_number <- 40
stability_probe$influence_table$`Review signal`[[1]] <- TRUE
stability_ko <- render_html(survival_simple_table(survival_stability_review(stability_probe, "ko"), table_language = "ko"))
stability_en <- render_html(survival_simple_table(survival_stability_review(stability_probe, "en"), table_language = "en"))
for (phrase in c("수준", "코드", "근거", "안내", "설계행렬 열의 최대 VIF", "설계행렬 조건수", "영향력 검토 계수 행 수")) {
  expect_contains(stability_ko, phrase, paste("Korean stability HTML must contain:", phrase))
}
for (phrase in c("Maximum design-column VIF", "Design condition number", "Influential coefficient rows")) {
  expect_not_contains(stability_ko, phrase, paste("Korean stability HTML must not leak:", phrase))
  expect_contains(stability_en, phrase, paste("English stability HTML must retain:", phrase))
}

message("Checking time-varying-coefficient appendix localization...")
time_varying <- prepare_cox_analysis_result(
  fixture,
  time = "time",
  event = "status",
  covariates = c("age", "sex"),
  event_value = "1",
  time_varying_covariate = "age",
  time_varying_times = "100, 250, 500"
)
time_varying_ko <- render_html(survival_cox_results_panel(time_varying, language = "ko"))
time_varying_en <- render_html(survival_cox_results_panel(time_varying, language = "en"))
for (phrase in c("시간가변 계수 분석", "시점별 위험비", "시간가변 계수 분석의 Schoenfeld 검정", "모형은 β(t) = β + γ log(1 + time)")) {
  expect_contains(time_varying_ko, phrase, paste("Korean time-varying appendix must contain:", phrase))
}
for (phrase in c("For this time-varying-coefficient analysis", "The model is β(t) = β + γ log(1 + time)")) {
  expect_not_contains(time_varying_ko, phrase, paste("Korean time-varying appendix must not leak:", phrase))
  expect_contains(time_varying_en, phrase, paste("English time-varying appendix must retain:", phrase))
}

message("Checking Fine-Gray censoring and stability localization...")
set.seed(20260826)
n <- 180L
competing_data <- data.frame(
  time = stats::rexp(n, rate = .07),
  status = sample(c(0L, 1L, 2L), n, replace = TRUE, prob = c(.30, .45, .25)),
  group = factor(rep(c("A", "B"), each = n / 2L)),
  censor_stratum = factor(rep(c("Site A", "Site B"), length.out = n)),
  age = stats::rnorm(n, 60, 9)
)
competing <- prepare_competing_risk_result(
  competing_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  group = "group",
  covariates = "age",
  regression = "fine_gray",
  censoring_group = "censor_stratum"
)
competing_ko <- render_html(survival_competing_results_panel(competing, language = "ko"))
competing_en <- render_html(survival_competing_results_panel(competing, language = "en"))
for (phrase in c(
  "검열분포 추정",
  "검열분포는 crr(cengroup=...)을 사용하여 censor_stratum의 층별로 추정",
  "검열분포를 층별로 추정: censor_stratum",
  "Fine-Gray 수치 안정성",
  "설계행렬 조건수:",
  "crr Schoenfeld 유사 잔차도"
)) expect_contains(competing_ko, phrase, paste("Korean Fine-Gray appendix must contain:", phrase))
for (phrase in c(
  "The censoring distribution was estimated separately within",
  "Separate censoring distributions within censor_stratum",
  "Design-matrix condition number:",
  "The crr Schoenfeld-like residual plot"
)) {
  expect_not_contains(competing_ko, phrase, paste("Korean Fine-Gray appendix must not leak:", phrase))
  expect_contains(competing_en, phrase, paste("English Fine-Gray appendix must retain:", phrase))
}
expect_contains(competing_ko, 'data-result-table-role="main"', "Competing-risk main tables must be identifiable.")
expect_contains(competing_ko, 'data-result-table-language="en"', "Competing-risk journal tables must remain English.")
expect_contains(competing_ko, 'data-result-table-role="appendix"', "Competing-risk appendix tables must be identifiable.")
expect_contains(competing_ko, 'data-result-table-language="ko"', "Competing-risk appendix tables must follow the Korean UI language.")

competing_sparse <- competing
competing_sparse$censoring_group_table$`Review signal`[[1]] <- TRUE
competing_sparse$fine_gray$collinearity_table$VIF[[1]] <- 12
competing_sparse$fine_gray$condition_number <- 40
competing_sparse$fine_gray$optimizer_table$`Standardized information condition number`[[1]] <- 40
if (nrow(competing_sparse$fine_gray$residual_review)) competing_sparse$fine_gray$residual_review$`Review signal`[[1]] <- TRUE
sparse_ko <- render_html(survival_simple_table(survival_stability_review(competing_sparse, "ko"), table_language = "ko"))
sparse_en <- render_html(survival_simple_table(survival_stability_review(competing_sparse, "en"), table_language = "en"))
for (phrase in c("희소 검열분포 층:", "Fine-Gray 최대 VIF =", "Fine-Gray 설계행렬 조건수 =", "표준화 Fine-Gray 정보행렬 조건수 =", "Schoenfeld 유사 잔차 시간패턴 신호:")) {
  expect_contains(sparse_ko, phrase, paste("Korean Fine-Gray stability evidence must contain:", phrase))
}
for (phrase in c("Sparse censoring-distribution strata:", "Maximum Fine-Gray VIF =", "Fine-Gray design condition number =", "Standardized Fine-Gray information condition number =", "Schoenfeld-like residual time-pattern signals:")) {
  expect_not_contains(sparse_ko, phrase, paste("Korean Fine-Gray stability evidence must not leak:", phrase))
  expect_contains(sparse_en, phrase, paste("English Fine-Gray stability evidence must retain:", phrase))
}

message("Checking survival note font contract...")
css <- paste(readLines(file.path("www", "style.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
note_rule <- regmatches(css, regexpr("\\.survival-table-note \\{[^}]+\\}", css, perl = TRUE))
expect_contains(note_rule, 'font-family: Arial, "Noto Sans KR", "Malgun Gothic", sans-serif;', "Survival notes must use the shared font stack.")
expect_contains(note_rule, "font-size: 11px;", "Survival notes must use the shared 11 px note size.")
expect_contains(note_rule, "line-height: 1.4;", "Survival notes must use the shared note line height.")

message("Survival dynamic locale validations passed.")

message("Checking repeated language changes against the same fitted results...")
old_options <- options()
models <- list(cox = cox, time_varying = time_varying, competing = competing)
before <- serialize(models, NULL)
for (kind in names(models)) {
  baseline <- NULL
  for (language in c("en", "ko", "ja", "zh", "es", "fr", "de", "vi", "ko", "en")) {
    options(statedu.app_language = language)
    panel <- if (kind == "competing") survival_competing_results_panel(models[[kind]], language) else survival_cox_results_panel(models[[kind]], language)
    doc <- xml2::read_html(render_html(panel))
    main <- xml2::xml_find_all(doc, "//table[@data-result-table-role='main']")
    appendix <- xml2::xml_find_all(doc, "//table[@data-result-table-role='appendix']")
    expect_true(length(main) > 0L && length(appendix) > 0L, "Both table roles must be present.")
    expect_true(all(xml2::xml_attr(main, "data-result-table-language") == "en"), "Main table language must remain English.")
    expect_true(all(xml2::xml_attr(appendix, "data-result-table-language") == language), "Appendix metadata must follow language changes.")
    main_text <- xml2::xml_text(main)
    if (is.null(baseline)) baseline <- main_text else expect_true(identical(main_text, baseline), "Main content must remain unchanged across language changes.")
    expect_contains(xml2::xml_text(doc), survival_appendix_title("Analysis overview", language), "Overview title must follow language changes.")
    expect_true(identical(before, serialize(models, NULL)), "Rendering must preserve fitted results.")
  }
}
options(old_options)
message("PASS: three fitted models, eight languages with ko/en return visits; main tables and fitted results preserved.")
