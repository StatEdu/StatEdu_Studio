source(file.path("R", "app_bootstrap.R"))
load_app_packages()
source_app_modules()

expect_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

fixed_count <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  if (length(matches) == 1L && matches[[1L]] < 0L) 0L else length(matches)
}

result <- list(
  method = "Linear mixed model",
  model_type = "lmm",
  family = "gaussian",
  requested_family = "gaussian",
  outcome = "y",
  id = "subject_id",
  time = "visit",
  offset_variable = character(0),
  weight = character(0),
  predictors = "x",
  covariates = character(0),
  terms = "x",
  include_time = FALSE,
  corstr = "exchangeable",
  random_slope = FALSE,
  exponentiate = FALSE,
  formula = y ~ x,
  n = 80L,
  clusters = 20L,
  time_points = 4L,
  aic = 201.2,
  bic = 214.6,
  coef_table = data.frame(
    Term = c("(Intercept)", "x"),
    B = c(1.20, 0.42),
    SE = c(0.20, 0.10),
    Statistic = c(6.0, 4.2),
    p = c(0.0001, 0.002),
    LLCI = c(0.81, 0.22),
    ULCI = c(1.59, 0.62),
    check.names = FALSE
  ),
  publication_notes = data.frame(
    Note = c(
      "Estimates represent subject-specific effects from the selected linear mixed model.",
      "CI = confidence interval; SE = standard error.",
      "Mixed-model fixed-effect standard errors are reported on the model scale.",
      "B is reported on the model scale.",
      "The repeated-measures structure used 20 subject/cluster units and 4 observed time point(s).",
      "Missing data were handled using complete-case analysis.",
      "No analysis weights were applied.",
      "Missing-data sensitivity analysis with MI/IPW/WGEE was not selected.",
      "Sensitivity analyses should be reported when feasible."
    ),
    check.names = FALSE
  ),
  model_rationale = "Use LMM when the target is subject-specific change in a continuous outcome with cluster-level random intercepts.",
  data_structure = data.frame(
    Item = c("Raw observations", "Analyzed observations", "Balanced panel"),
    Value = c("82", "80", "Yes"),
    check.names = FALSE
  ),
  weight_summary = data.frame(
    Item = c("Weight type", "Normalization", "Note"),
    Value = c("No weights", "Not applied", "No analysis weights were applied."),
    check.names = FALSE
  ),
  missing_table = data.frame(
    Variable = c("y", "x"), Missing = c(1L, 1L), MissingPercent = c(1.2, 1.2),
    check.names = FALSE
  ),
  missing_pattern = data.frame(
    Item = c("Distinct missingness patterns", "Most common missingness pattern"),
    Value = c("2", "Complete (n=80)"),
    check.names = FALSE
  ),
  missing_by_time = data.frame(
    Time = c("1", "2"), Rows = c(40L, 42L), `Complete rows` = c(39L, 41L),
    `Missing dependent variable` = c(1L, 0L),
    `Any missing selected variable` = c(1L, 1L), `Any missing %` = c(2.5, 2.4),
    check.names = FALSE
  ),
  missing_sensitivity_results = data.frame(
    Strategy = "Multiple imputation (MI)", Term = "x", B = 0.41, SE = 0.11,
    Statistic = 3.73, p = 0.003, LLCI = 0.19, ULCI = 0.63,
    Status = "Fitted", Note = "No analysis weights were applied.",
    check.names = FALSE
  ),
  fit_details = data.frame(Item = "Singular fit", Value = "No", check.names = FALSE),
  assumption_checks = data.frame(
    Check = "Random-effect normality", Result = "No evidence of violation",
    Statistic = 0.98, p = 0.41,
    Interpretation = "Random-effect normality screening was not significant.",
    Recommendation = "Random-effect normality looks acceptable by this screening test.",
    Issue = FALSE, check.names = FALSE
  ),
  recommendations = "No major assumption issue was detected by the selected screening checks. Continue with the selected model and report the repeated-measures structure.",
  sensitivity_recommendations = c(
    "Compare random-intercept and random-slope specifications when time trends may differ by subject.",
    "Repeat key conclusions with GEE if population-averaged inference is also relevant."
  ),
  sensitivity_results = data.frame(
    Analysis = "Random-effects sensitivity", Comparison = "Random intercept only",
    Status = "Fitted (selected)", Metric = "AIC / BIC", Value = "201.2 / 214.6",
    Note = "Sensitivity analyses should be reported when feasible.",
    check.names = FALSE
  ),
  manuscript_text = data.frame(
    Section = c("Methods", "Results", "Assumptions", "Software"),
    SuggestedText = c(
      "Use LMM when the target is subject-specific change in a continuous outcome with cluster-level random intercepts. No analysis weights were applied.",
      "Fixed-effect estimates were reported with standard errors, p-values, and 95% confidence intervals.",
      "Assumption screening did not flag a major issue among the selected checks.",
      "Analyses were performed using R 4.5.3."
    ),
    check.names = FALSE
  ),
  reporting_checklist = data.frame(
    Item = c("Model rationale", "Data structure summarized", "Software/package version reported"),
    Status = c("Ready", "Ready", "Ready"),
    Details = c(
      "Use LMM when the target is subject-specific change in a continuous outcome with cluster-level random intercepts.",
      "Subject count, time points, balanced/unbalanced status, and cluster size are summarized.",
      "R 4.5.3"
    ),
    check.names = FALSE
  ),
  software_versions = data.frame(Software = c("R", "lme4"), Version = c("4.5.3", "1.1"), check.names = FALSE),
  notes = "No analysis weights were applied."
)

results <- list(result)
variable_info <- data.frame(
  name = c("y", "x", "subject_id", "visit"),
  var_label = c("Outcome Y", "Predictor X", "Subject ID", "Visit"),
  measurement = c("continuous", "continuous", "category", "continuous"),
  stringsAsFactors = FALSE
)

render_panel <- function(language) {
  old <- options(statedu.app_language = language)
  on.exit(options(old), add = TRUE)
  htmltools::renderTags(longitudinal_results_panel(results, variable_info))$html
}

ko_html <- render_panel("ko")
expect_true(grepl('data-result-table-role="main"', ko_html, fixed = TRUE), "Longitudinal publication output must contain a main table.")
expect_true(grepl('data-result-table-role="main" data-result-table-language="en"', ko_html, fixed = TRUE), "Longitudinal main tables must remain English under Korean UI.")
expect_true(grepl('data-result-table-role="appendix" data-result-table-language="ko"', ko_html, fixed = TRUE), "Longitudinal appendix tables must follow the Korean UI language.")
expect_true(grepl("Publication-ready estimates", ko_html, fixed = TRUE), "The longitudinal journal-table title must remain English.")
expect_true(grepl("<h3>자료 구조</h3>", ko_html, fixed = TRUE), "The longitudinal data-structure appendix title must be Korean.")
expect_true(grepl("<h3>결측자료 패턴</h3>", ko_html, fixed = TRUE), "The missing-pattern appendix title must be Korean.")
expect_true(grepl("원자료 관측치", ko_html, fixed = TRUE) && grepl("완전관측 (n=80)", ko_html, fixed = TRUE), "The Korean appendix must localize actual missing-data body values.")
expect_true(grepl("모형 선택 근거", ko_html, fixed = TRUE) && grepl("군집수준 확률절편", ko_html, fixed = TRUE), "The Korean appendix must localize the dynamic model rationale.")
expect_true(grepl("준비됨", ko_html, fixed = TRUE) && grepl("원고 문장 제안", ko_html, fixed = TRUE), "The Korean appendix must localize checklist values and manuscript headings.")
expect_true(grepl("분석에는 R 4.5.3를 사용했습니다.", ko_html, fixed = TRUE), "The Korean appendix must localize dynamic software manuscript text.")
expect_true(!grepl("Raw observations", ko_html, fixed = TRUE) && !grepl("Most common missingness pattern", ko_html, fixed = TRUE), "Korean appendix body sentinels must not remain English.")
expect_true(fixed_count(ko_html, 'data-result-table-sheet="true"') == fixed_count(ko_html, "<table"), "Each longitudinal result-table sheet must contain exactly one table.")
expect_true(grepl('data-result-table-orientation="portrait"', ko_html, fixed = TRUE), "Compact longitudinal tables must use B5 portrait.")
expect_true(grepl("font-size:12px", ko_html, fixed = TRUE), "Longitudinal table body text must retain the common 12px size.")
expect_true(grepl("SE = standard error; CI = confidence interval;", ko_html, fixed = TRUE), "The main table must place SE and CI definitions before explanations.")
expect_true(!grepl("Note. (SE|CI) =", ko_html), "The main-table note must omit the obsolete Note. prefix.")
expect_true(!grepl("The repeated-measures structure used 20", ko_html, fixed = TRUE), "Verbose diagnostic prose must not be copied into the journal-table note.")

en_html <- render_panel("en")
expect_true(grepl('data-result-table-role="appendix" data-result-table-language="en"', en_html, fixed = TRUE), "English UI appendix tables must be marked English.")
expect_true(grepl("<h3>Data structure</h3>", en_html, fixed = TRUE) && grepl("Raw observations", en_html, fixed = TRUE), "English UI appendix titles and body must remain English.")
expect_true(grepl("Use LMM when the target is subject-specific change", en_html, fixed = TRUE), "English UI dynamic appendix prose must remain English.")
expect_true(fixed_count(en_html, 'data-result-table-sheet="true"') == fixed_count(en_html, "<table"), "English longitudinal result sheets must also contain one table each.")

set.seed(20260826)
actual_data <- data.frame(
  subject_id = rep(seq_len(16L), each = 3L),
  visit = rep(0:2, times = 16L),
  x = stats::rnorm(48L),
  y = stats::rnorm(48L),
  stringsAsFactors = FALSE
)
actual_info <- data.frame(
  name = names(actual_data), var_label = names(actual_data), role = "",
  measurement = c("category", "continuous", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
actual_results <- prepare_longitudinal_analysis_result(
  data = actual_data,
  outcome = "y",
  id = "subject_id",
  time = "visit",
  predictors = "x",
  model_type = "gee",
  family = "gaussian",
  variable_info = actual_info
)
expect_true(is.list(actual_results) && length(actual_results) == 1L, "The actual GEE sentinel model must fit.")
old <- options(statedu.app_language = "ko")
actual_ko_html <- htmltools::renderTags(longitudinal_results_panel(actual_results, actual_info))$html
options(old)
expect_true(grepl("모집단 평균 종단효과", actual_ko_html, fixed = TRUE), "Actual GEE model-rationale prose must be Korean in the appendix.")
expect_true(grepl(sprintf("선택한 작업상관은 %s입니다.", statedu_t("longitudinal.identifier.exchangeable", "ko")), actual_ko_html, fixed = TRUE), "Actual GEE assumption interpretation must include the localized correlation identifier.")
expect_true(grepl("원자료 관측치", actual_ko_html, fixed = TRUE) && grepl("시점별 결측자료", actual_ko_html, fixed = TRUE), "Actual GEE data and missing-by-time sections must be Korean.")
expect_true(!grepl("Use GEE when the target is a population-averaged", actual_ko_html, fixed = TRUE), "Actual Korean GEE appendix rationale must not retain the checked English sentinel.")
expect_true(!grepl("The selected working correlation is", actual_ko_html, fixed = TRUE), "Actual Korean GEE assumption text must not retain the checked English sentinel.")
expect_true(fixed_count(actual_ko_html, 'data-result-table-sheet="true"') == fixed_count(actual_ko_html, "<table"), "Actual GEE output must keep one table per sheet.")

cat("Longitudinal result-table language and sheet contract passed.\n")
