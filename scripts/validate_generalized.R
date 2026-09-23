source("R/app_bootstrap.R")
invisible(lapply(file.path("R", app_module_files), source, chdir = TRUE))
suppressPackageStartupMessages(library(shiny))
options(statedu.app_language = "en")

runtime_required <- c("car", "MASS", "sandwich", "lmtest", "mice", "geepack", "lme4", "lmerTest", "plm")
stopifnot(all(runtime_required %in% required_packages))
run_app_text <- paste(readLines("run_app.R", warn = FALSE), collapse = "\n")
stopifnot(grepl('source(file.path("R", "app_bootstrap.R"), local = TRUE)', run_app_text, fixed = TRUE))
server_generalized_text <- paste(readLines(file.path("R", "server_generalized.R"), warn = FALSE), collapse = "\n")
stopifnot(grepl("observeEvent(input$analysis_transfer_drop", server_generalized_text, fixed = TRUE))
stopifnot(grepl("generalized_predictors", server_generalized_text, fixed = TRUE))

set.seed(1)
data <- data.frame(
  y = rnorm(80),
  x1 = rnorm(80),
  x2 = factor(sample(c("A", "B"), 80, TRUE)),
  aux = rnorm(80),
  exposure = runif(80, 0.5, 2)
)
data$y[c(3, 9)] <- NA
variable_info <- data.frame(
  name = c("y", "x1", "x2", "aux", "exposure", "bin", "count"),
  var_label = c("Outcome", "Predictor 1", "Group", "Auxiliary", "Exposure", "Binary outcome", "Count outcome"),
  role = "",
  measurement = c("continuous", "continuous", "category", "continuous", "continuous", "binary", "continuous"),
  stringsAsFactors = FALSE
)
category_table <- data.frame(
  name = "x2",
  var_label = "Group",
  reference = "A",
  value_1 = "A",
  label_1 = "Control",
  value_2 = "B",
  label_2 = "Treatment",
  stringsAsFactors = FALSE
)

gaussian <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  robust = TRUE,
  show_vif = TRUE,
  variable_info = variable_info,
  category_table = category_table
)
stopifnot(is.data.frame(gaussian$coef_table))
stopifnot(nrow(gaussian$coef_table) > 0)
stopifnot(identical(gaussian$family, "gaussian"))
stopifnot(is.data.frame(gaussian$missing_table))
stopifnot(is.data.frame(gaussian$missing_pattern))
stopifnot(any(gaussian$missing_pattern$Item == "Distinct missingness patterns"))
stopifnot(gaussian$excluded_n == 2)
stopifnot(any(gaussian$assumption_checks$Check == "Independent observations"))
stopifnot(identical(gaussian$missing_strategy, "complete"))
stopifnot(identical(gaussian$se_type_requested, "HC1"))
stopifnot(any(gaussian$fit_stats$Item == "Standard errors"))
stopifnot(any(gaussian$fit_stats$Item == "Log likelihood"))
stopifnot(any(gaussian$fit_stats$Item == "AIC"))
stopifnot(any(gaussian$fit_stats$Item == "BIC"))
stopifnot(any(gaussian$fit_stats$Item == "R\u00B2"))
stopifnot(any(gaussian$fit_stats$Item == "Adjusted R\u00B2"))
stopifnot(is.data.frame(gaussian$decision_summary))
stopifnot(any(gaussian$decision_summary$Item == "Family selection"))
stopifnot(any(gaussian$decision_summary$Item == "Recommended reporting"))
stopifnot(is.data.frame(gaussian$coding_summary))
stopifnot(any(grepl("reference = Control", gaussian$coding_summary$Coding, fixed = TRUE)))
stopifnot(is.data.frame(gaussian$publication_notes))
stopifnot(nrow(gaussian$publication_notes) > 0)
stopifnot(is.data.frame(gaussian$reporting_checklist))
stopifnot(any(gaussian$reporting_checklist$Item == "Family and link reported"))
stopifnot(is.data.frame(gaussian$manuscript_text))
stopifnot(any(gaussian$manuscript_text$Section == "Methods"))
stopifnot(is.data.frame(gaussian$software_versions))
stopifnot(any(gaussian$software_versions$Software == "R"))
stopifnot(any(gaussian$software_versions$Software == "stats"))
stopifnot(all(c("MASS", "sandwich", "lmtest", "mice", "openxlsx") %in% required_packages))
display_coef <- generalized_display_coef_table(
  gaussian,
  variable_table = variable_info,
  category_table = category_table
)
stopifnot(any(display_coef$Term == "Group:Control" & display_coef$B == "reference"))
stopifnot(any(grepl("Group:Treatment", display_coef$Term, fixed = TRUE)))
saved_html <- saved_generalized_results_html(
  gaussian,
  variable_table = variable_info,
  category_table = category_table
)
screen_html <- as.character(generalized_results_panel(
  gaussian,
  variable_table = variable_info,
  category_table = category_table
))
html_count <- function(text, pattern) {
  hits <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (length(hits) == 1L && hits[[1]] < 0L) 0L else length(hits)
}
generalized_sheet_count <- html_count(screen_html, 'data-result-table-sheet="true"')
stopifnot(generalized_sheet_count > 1L)
stopifnot(generalized_sheet_count == html_count(screen_html, "<table"))
stopifnot(generalized_sheet_count == html_count(screen_html, "generalized-result-panel"))
stopifnot(grepl('data-result-table-role="main"', screen_html, fixed = TRUE))
stopifnot(grepl('data-result-table-role="appendix"', screen_html, fixed = TRUE))
stopifnot(grepl('data-result-table-language="en"', screen_html, fixed = TRUE))
stopifnot(grepl('data-result-table-orientation="portrait"', screen_html, fixed = TRUE))
stopifnot(grepl('data-result-table-orientation="landscape"', screen_html, fixed = TRUE))
options(statedu.app_language = "ko")
saved_html_ko <- saved_generalized_results_html(
  gaussian,
  variable_table = variable_info,
  category_table = category_table
)
options(statedu.app_language = "en")
stopifnot(grepl('data-result-table-role="main" data-result-table-language="en"', saved_html_ko, fixed = TRUE))
stopifnot(grepl('data-result-table-role="appendix" data-result-table-language="ko"', saved_html_ko, fixed = TRUE))
stopifnot(grepl("\ubaa8\ud615 \uac1c\uc694", saved_html_ko, fixed = TRUE))
stopifnot(grepl("완전사례 GLM에서", saved_html_ko, fixed = TRUE))
stopifnot(grepl("모형 선택 근거", saved_html_ko, fixed = TRUE))
stopifnot(grepl("원고 작성을 위한 방법, 결과, 가정 및 소프트웨어 문장", saved_html_ko, fixed = TRUE))
stopifnot(grepl("계수의 표준오차는", saved_html_ko, fixed = TRUE))
stopifnot(grepl("적합 분포: 가우시안, 링크: 항등.", saved_html_ko, fixed = TRUE))
stopifnot(grepl("표준 GLM은 예측변수를 조건화한 뒤", saved_html_ko, fixed = TRUE))
stopifnot(grepl("편차 잔차에 Shapiro-Wilk 선별검정을 적용", saved_html_ko, fixed = TRUE))
stopifnot(grepl("최대 Cook's D", saved_html_ko, fixed = TRUE))
stopifnot(grepl("연속형 결과변수; 항등 링크.", saved_html_ko, fixed = TRUE))
stopifnot(grepl("원자료 행", saved_html_ko, fixed = TRUE))
stopifnot(grepl("완전 모형 행", saved_html_ko, fixed = TRUE))
stopifnot(grepl("완전사례 (n=", saved_html_ko, fixed = TRUE))
stopifnot(grepl("x1", saved_html_ko, fixed = TRUE), grepl("x2", saved_html_ko, fixed = TRUE))
remaining_english_appendix <- c(
  "item(s) flagged/review:", "Fitted family:", "Standard GLM assumes independent observations",
  "Shapiro-Wilk screening was applied", "Max Cook's D =", "Continuous outcome; identity link.",
  ">Raw rows<", ">Complete model rows<", "Rows excluded by complete-case screen",
  "Most common missingness pattern", "Complete (n="
)
stopifnot(!any(vapply(remaining_english_appendix, grepl, logical(1), x = saved_html_ko, fixed = TRUE)))
main_note <- generalized_coefficient_note(gaussian)
stopifnot(startsWith(main_note, "SE = standard error; B = unstandardized coefficient; CI = confidence interval"))
main_note_order <- vapply(
  c("B = unstandardized coefficient", "A Gaussian identity-link model was fitted", "Standard errors were computed", "Missing data were handled"),
  function(value) regexpr(value, main_note, fixed = TRUE)[[1]],
  integer(1)
)
stopifnot(all(main_note_order > 0L), all(diff(main_note_order) > 0L))
stopifnot(!grepl("reported in the appendix", main_note, fixed = TRUE))
stopifnot(!grepl("model link scale", main_note, fixed = TRUE))
stopifnot(!grepl("Complete-case GLM excluded", saved_html_ko, fixed = TRUE))
stopifnot(!grepl(">Model rationale<", saved_html_ko, fixed = TRUE))
stopifnot(!grepl("Suggested Methods, Results, Assumptions, and Software text", saved_html_ko, fixed = TRUE))
stopifnot(grepl("A Gaussian identity-link model was fitted", saved_html_ko, fixed = TRUE))
stopifnot(grepl("Model overview", saved_html, fixed = TRUE))
stopifnot(!grepl("Model decision summary", saved_html, fixed = TRUE))
major_section_order <- c("Coefficients", "Model overview", "Assumption checks", "SCI reporting checklist")
major_section_positions <- vapply(major_section_order, function(label) regexpr(label, saved_html, fixed = TRUE)[[1]], numeric(1))
stopifnot(all(major_section_positions > 0))
stopifnot(all(diff(major_section_positions) > 0))
stopifnot(grepl("Variable coding", saved_html, fixed = TRUE))
stopifnot(regexpr("Variable coding", saved_html, fixed = TRUE)[[1]] > major_section_positions[[length(major_section_positions)]])
stopifnot(!grepl("SCI model statistics", saved_html, fixed = TRUE))
stopifnot(!any(grepl("^N=", generalized_fit_summary_lines(gaussian))))
stopifnot(!any(grepl("^standard errors=", generalized_fit_summary_lines(gaussian), ignore.case = TRUE)))
stopifnot(grepl("AIC=", saved_html, fixed = TRUE))
stopifnot(grepl("BIC=", saved_html, fixed = TRUE))
stopifnot(grepl("adjusted R", saved_html, fixed = TRUE))
stopifnot(grepl("Distinct missingness patterns", saved_html, fixed = TRUE))
stopifnot(grepl("Group:Control", saved_html, fixed = TRUE))
stopifnot(grepl("reference", saved_html, fixed = TRUE))
stopifnot(grepl("Group:Treatment", saved_html, fixed = TRUE))
stopifnot(!grepl(">Publication table notes<", saved_html, fixed = TRUE))
stopifnot(!grepl("Note.", saved_html, fixed = TRUE))
stopifnot(grepl("SCI reporting checklist", saved_html, fixed = TRUE))
stopifnot(grepl("Suggested manuscript text", saved_html, fixed = TRUE))
stopifnot(grepl("Software versions", saved_html, fixed = TRUE))
if (requireNamespace("openxlsx", quietly = TRUE)) {
  excel_path <- tempfile(fileext = ".xlsx")
  save_generalized_excel_file(
    gaussian,
    excel_path,
    variable_table = variable_info,
    category_table = category_table
  )
  stopifnot(file.exists(excel_path))
  stopifnot(file.info(excel_path)$size > 0)
  excel_sheets <- openxlsx::getSheetNames(excel_path)
  stopifnot(!"SCI model stats" %in% excel_sheets)
  screen_tables <- result_entry_tables(list(html = saved_html))
  stopifnot(length(excel_sheets) == length(screen_tables))
  for (i in seq_along(screen_tables)) {
    table <- screen_tables[[i]]
    exported <- openxlsx::read.xlsx(excel_path, sheet = i, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, na.strings = NULL)
    header <- table$screen$values[1L, ]
    header_rows <- which(vapply(seq_len(nrow(exported)), function(row) {
      all(vapply(which(nzchar(header)), function(col) identical(as.character(exported[row, col]), header[[col]]), logical(1)))
    }, logical(1)))
    stopifnot(length(header_rows) == 1L)
    offset <- header_rows[[1L]] - 1L
    for (cell in table$screen$cells) {
      expected <- table$screen$values[cell$row, cell$col]
      if (nzchar(expected) && !identical(as.character(exported[cell$row + offset, cell$col]), expected)) stop(sprintf("Excel mismatch table %s row %s col %s", i, cell$row, cell$col))
    }
  }
  coefficient_index <- which(vapply(screen_tables, function(table) any(grepl("AIC=", c(table$notes, as.vector(table$screen$values)), fixed = TRUE)), logical(1)))[1L]
  stopifnot(!is.na(coefficient_index))
  coefficient_sheet <- openxlsx::read.xlsx(excel_path, sheet = coefficient_index, colNames = FALSE)
  coefficient_text <- paste(unlist(coefficient_sheet, use.names = FALSE), collapse = " ")
  stopifnot(grepl("AIC=", coefficient_text, fixed = TRUE))
  stopifnot(grepl("BIC=", coefficient_text, fixed = TRUE))
}

hc3 <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  se_type = "HC3"
)
stopifnot(identical(hc3$se_type_requested, "HC3"))
stopifnot(hc3$se_type_used %in% c("HC3", "model"))

model_based <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  se_type = "model"
)
stopifnot(identical(model_based$se_type_used, "model"))

pool_tables <- lapply(seq_len(3L), function(index) {
  table <- data.frame(
    Term = c("(Intercept)", "x"),
    B = c(c(0.8, 1.0, 1.1)[[index]], c(0.25, 0.30, 0.28)[[index]]),
    SE = c(c(0.20, 0.25, 0.22)[[index]], c(0.10, 0.12, 0.11)[[index]]),
    stringsAsFactors = FALSE
  )
  attr(table, "robust_used") <- FALSE
  attr(table, "se_type_requested") <- "model"
  attr(table, "se_type_used") <- "model"
  table
})
pooled_probe <- generalized_pool_coef_tables(pool_tables, expected_terms = c("(Intercept)", "x"), m_expected = 3L, dfcom = 90)
probe_estimates <- c(0.8, 1.0, 1.1)
probe_variances <- c(0.20, 0.25, 0.22)^2
probe_qbar <- mean(probe_estimates)
probe_ubar <- mean(probe_variances)
probe_b <- stats::var(probe_estimates)
probe_total <- probe_ubar + (1 + 1 / 3) * probe_b
probe_riv <- ((1 + 1 / 3) * probe_b) / probe_ubar
probe_lambda <- ((1 + 1 / 3) * probe_b) / probe_total
probe_old_df <- (3 - 1) * (1 + 1 / probe_riv)^2
probe_observed_df <- ((90 + 1) / (90 + 3)) * 90 * (1 - probe_lambda)
probe_df <- (probe_old_df * probe_observed_df) / (probe_old_df + probe_observed_df)
stopifnot(isTRUE(all.equal(pooled_probe$B[[1]], probe_qbar, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(pooled_probe$SE[[1]], sqrt(probe_total), tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(pooled_probe$df[[1]], probe_df, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(pooled_probe$p[[1]], 2 * stats::pt(abs(probe_qbar / sqrt(probe_total)), df = probe_df, lower.tail = FALSE), tolerance = 1e-12)))
pool_diagnostics_probe <- attr(pooled_probe, "mi_pooling_diagnostics")
stopifnot(is.data.frame(pool_diagnostics_probe), all(c("m", "df", "RIV", "FMI") %in% names(pool_diagnostics_probe)))
mismatched_pool_tables <- pool_tables
mismatched_pool_tables[[2]] <- mismatched_pool_tables[[2]][1, , drop = FALSE]
mismatch_error <- tryCatch(
  generalized_pool_coef_tables(mismatched_pool_tables, expected_terms = c("(Intercept)", "x"), m_expected = 3L, dfcom = 90),
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("intersection pooling is not allowed", mismatch_error, fixed = TRUE))

if (requireNamespace("mice", quietly = TRUE)) {
  mi_result <- prepare_generalized_analysis_result(
    data,
    "y",
    c("x1", "x2"),
    family = "gaussian",
    robust = TRUE,
    missing_strategy = "mi",
    missing_imputations = 3L,
    missing_iterations = 2L
  )
  stopifnot(identical(mi_result$missing_strategy, "mi"))
  stopifnot(mi_result$n == sum(!is.na(data$y)))
  stopifnot(is.data.frame(mi_result$missing_details))
  stopifnot(any(mi_result$missing_details$Item == "MI datasets"))
  stopifnot(any(mi_result$missing_details$Item == "Dependent-variable handling"))
  stopifnot(any(grepl("originally missing dependent-variable", mi_result$missing_details$Value, fixed = TRUE)))
  stopifnot(any(grepl("Standard mice-based multiple imputation", mi_result$notes, fixed = TRUE)))
  stopifnot(is.data.frame(mi_result$mi_pooling_diagnostics), nrow(mi_result$mi_pooling_diagnostics) == nrow(mi_result$coef_table))
  stopifnot(all(mi_result$mi_pooling_diagnostics$m == 3L))
  stopifnot(all(is.finite(mi_result$mi_pooling_diagnostics$df) & mi_result$mi_pooling_diagnostics$df > 0))
  stopifnot(all(mi_result$mi_pooling_diagnostics$RIV >= 0))
  stopifnot(all(mi_result$mi_pooling_diagnostics$FMI >= 0 & mi_result$mi_pooling_diagnostics$FMI <= 1))
  stopifnot(is.data.frame(mi_result$mi_fit_diagnostics), nrow(mi_result$mi_fit_diagnostics) == 3L)
  stopifnot(length(unique(mi_result$mi_fit_diagnostics$Family)) == 1L)
  stopifnot(length(unique(mi_result$mi_fit_diagnostics$Link)) == 1L)
  stopifnot(length(unique(mi_result$mi_fit_diagnostics$`Residual df`)) == 1L)
  stopifnot(length(unique(mi_result$mi_fit_diagnostics$`Term signature`)) == 1L)
  mi_html <- saved_generalized_results_html(mi_result)
  stopifnot(grepl("Multiple-imputation pooling diagnostics", mi_html, fixed = TRUE))
  stopifnot(grepl("Barnard-Rubin", mi_html, fixed = TRUE))
  options(statedu.app_language = "ko")
  mi_html_ko <- saved_generalized_results_html(mi_result)
  options(statedu.app_language = "en")
  stopifnot(grepl("표준 mice 기반 다중대치", mi_html_ko, fixed = TRUE))
  stopifnot(grepl("대치 후 분석 행", mi_html_ko, fixed = TRUE))
  stopifnot(grepl("종속변수 처리", mi_html_ko, fixed = TRUE))
  stopifnot(!grepl("Standard mice-based multiple imputation was selected", mi_html_ko, fixed = TRUE))
  stopifnot(!grepl("Rows with originally missing dependent-variable values are excluded", mi_html_ko, fixed = TRUE))
  mi_imputed_outcome <- prepare_generalized_analysis_result(
    data,
    "y",
    c("x1", "x2"),
    family = "gaussian",
    robust = TRUE,
    missing_strategy = "mi",
    missing_imputations = 3L,
    missing_iterations = 2L,
    mi_outcome = "impute"
  )
  stopifnot(mi_imputed_outcome$n == nrow(data))
  stopifnot(any(grepl("imputed dependent-variable", mi_imputed_outcome$missing_details$Value, fixed = TRUE)))
}

ipw_result <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  robust = TRUE,
  missing_strategy = "ipw",
  ipw_auxiliary = "aux"
)
stopifnot(identical(ipw_result$missing_strategy, "ipw"))
stopifnot(is.data.frame(ipw_result$missing_details))
stopifnot(any(ipw_result$missing_details$Item == "IPW summary"))
stopifnot(any(ipw_result$missing_details$Item == "Selected auxiliary variables"))
stopifnot(any(grepl("aux", ipw_result$missing_details$Value, fixed = TRUE)))
stopifnot(any(grepl("positivity/weight stability", ipw_result$missing_details$Value, fixed = TRUE)))
stopifnot(all(c(
  "Predicted observation probability: min",
  "Final weight summary",
  "Effective sample size",
  "Weight clipping count"
) %in% ipw_result$missing_details$Item))
options(statedu.app_language = "ko")
ipw_html_ko <- saved_generalized_results_html(ipw_result)
options(statedu.app_language = "en")
stopifnot(grepl("역확률가중(IPW)", ipw_html_ko, fixed = TRUE))
stopifnot(grepl("관측모형", ipw_html_ko, fixed = TRUE))
stopifnot(grepl("양성성/가중치 안정성", ipw_html_ko, fixed = TRUE))
stopifnot(grepl("aux", ipw_html_ko, fixed = TRUE))
stopifnot(!grepl("Inverse-probability weighting was selected", ipw_html_ko, fixed = TRUE))
stopifnot(!grepl("Observation model: aux", ipw_html_ko, fixed = TRUE))

data$bin <- rbinom(80, 1, plogis(-0.2 + 0.5 * data$x1))
logistic <- prepare_generalized_analysis_result(
  data,
  "bin",
  c("x1", "x2"),
  family = "binomial",
  robust = TRUE,
  variable_info = variable_info,
  category_table = category_table
)
stopifnot(isTRUE(logistic$exponentiate))
stopifnot(identical(logistic$family, "binomial"))
stopifnot(any(logistic$assumption_checks$Check == "Events per variable"))
stopifnot(any(logistic$assumption_checks$Check == "Separation risk"))
stopifnot(any(grepl("Binary outcome coded as event", logistic$coding_summary$Coding, fixed = TRUE)))
logistic_display <- generalized_display_coef_table(
  logistic,
  variable_table = variable_info,
  category_table = category_table
)
stopifnot("OR" %in% names(logistic_display))
stopifnot(!"B" %in% names(logistic_display))
stopifnot(any(logistic_display$Term == "Group:Control" & logistic_display$OR == "reference"))
logistic_b_se <- prepare_generalized_analysis_result(
  data,
  "bin",
  c("x1", "x2"),
  family = "binomial",
  robust = TRUE,
  exponentiate = FALSE,
  variable_info = variable_info,
  category_table = category_table
)
logistic_b_se_display <- generalized_display_coef_table(
  logistic_b_se,
  variable_table = variable_info,
  category_table = category_table
)
stopifnot(all(c("B", "SE") %in% names(logistic_b_se_display)))
stopifnot(!"OR" %in% names(logistic_b_se_display))
stopifnot(!any(c("LLCI", "ULCI") %in% names(logistic_b_se_display)))

data$count <- rnbinom(80, mu = exp(0.3 + 0.4 * data$x1), size = 0.7)
count <- prepare_generalized_analysis_result(
  data,
  "count",
  c("x1", "x2"),
  exposure = "exposure",
  family = "count",
  robust = TRUE,
  overdispersion = TRUE
)
stopifnot(count$family %in% c("count", "negative_binomial"))
stopifnot(is.data.frame(count$count_details))
stopifnot(nrow(count$count_details) > 0)
stopifnot(any(count$count_details$Item == "Poisson dispersion ratio"))
stopifnot(any(count$count_details$Item == "Selection rule"))
stopifnot(any(count$count_details$Item == "Poisson logLik"))
stopifnot(any(count$count_details$Item == "Negative binomial logLik"))
stopifnot(any(count$count_details$Item == "Poisson vs NB LR statistic"))
stopifnot(any(count$count_details$Item == "LL comparison note"))
stopifnot(any(grepl("AIC/BIC are reported as supplementary", count$count_details$Value, fixed = TRUE)))
stopifnot(any(count$decision_summary$Item == "Recommended reporting"))
stopifnot(identical(count$exposure, "exposure"))
stopifnot(grepl("offset(log(exposure))", paste(deparse(count$formula), collapse = " "), fixed = TRUE))
count_display <- generalized_display_coef_table(count)
stopifnot("RR" %in% names(count_display))
count_html <- saved_generalized_results_html(count)
stopifnot(grepl("Count-family / overdispersion screening", count_html, fixed = TRUE))
stopifnot(grepl("Poisson overdispersion screen", count_html, fixed = TRUE))
stopifnot(grepl("LogLik=", count_html, fixed = TRUE))
stopifnot(grepl("RR = rate ratio", count_html, fixed = TRUE))

if (requireNamespace("mice", quietly = TRUE)) {
  set.seed(77)
  mi_count_n <- 160L
  mi_count_x <- stats::rnorm(mi_count_n)
  mi_count_data <- data.frame(
    count = stats::rnbinom(mi_count_n, mu = exp(0.4 + 0.5 * mi_count_x), size = 0.35),
    x = mi_count_x
  )
  mi_count_data$x[sample.int(mi_count_n, 25L)] <- NA_real_
  mi_count_info <- data.frame(name = c("count", "x"), measurement = c("continuous", "continuous"), stringsAsFactors = FALSE)
  mi_count <- prepare_generalized_analysis_result(
    mi_count_data,
    "count",
    "x",
    family = "count",
    robust = TRUE,
    missing_strategy = "mi",
    missing_imputations = 3L,
    missing_iterations = 2L,
    variable_info = mi_count_info
  )
  stopifnot(identical(mi_count$family, "negative_binomial"))
  stopifnot(nrow(mi_count$mi_fit_diagnostics) == 3L)
  stopifnot(all(mi_count$mi_fit_diagnostics$Family == "negative_binomial"))
  stopifnot(all(mi_count$mi_fit_diagnostics$Link == "log"))
  stopifnot(length(unique(mi_count$mi_fit_diagnostics$`Term signature`)) == 1L)
  stopifnot(all(is.finite(mi_count$mi_fit_diagnostics$`Poisson dispersion`)))
  stopifnot(any(grepl("selected once before pooling", mi_count$notes, fixed = TRUE)))
}

categorical_info <- data.frame(
  name = c("group", "x1"),
  measurement = c("category", "continuous"),
  stringsAsFactors = FALSE
)
categorical_error <- tryCatch(
  prepare_generalized_analysis_result(
    data.frame(group = factor(sample(c("A", "B", "C"), 80, TRUE)), x1 = data$x1),
    "group",
    "x1",
    family = "auto",
    variable_info = categorical_info
  ),
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("logistic regression menu", categorical_error, fixed = TRUE))
stopifnot(identical(unname(generalized_link_choices("binomial")), c("default", "logit")))
stopifnot(identical(generalized_resolve_link("binomial", "log"), "default"))

unchecked <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  assumption_checks = FALSE
)
stopifnot(is.data.frame(unchecked$assumption_checks))
stopifnot(nrow(unchecked$assumption_checks) == 0)

setup_state <- generalized_setup_state(
  selected_names = names(data),
  outcome = "y",
  exposure = "exposure",
  predictors = c("x1", "x2"),
  variable_table = data.frame(name = names(data), measurement = "continuous", stringsAsFactors = FALSE),
  missing_strategy = "mi",
  se_type = "HC3",
  language = "en"
)
setup_html <- as.character(generalized_setup_panel(setup_state, NULL))
if (!grepl("id=\"generalized_options_tab\"", setup_html, fixed = TRUE) ||
    !grepl("id=\"generalized_missing_strategy\"", setup_html, fixed = TRUE) ||
    !grepl("id=\"generalized_se_type\"", setup_html, fixed = TRUE) ||
    !grepl("id=\"generalized_report_b_se\"", setup_html, fixed = TRUE) ||
    !grepl("Robust sandwich HC3", setup_html, fixed = TRUE) ||
    !grepl("Multiple imputation settings", setup_html, fixed = TRUE) ||
    !grepl("Dependent variable", setup_html, fixed = TRUE) ||
    !grepl("Exposure / offset (optional)", setup_html, fixed = TRUE) ||
    !grepl("Independent variables", setup_html, fixed = TRUE)) {
  stop("GLM setup UI is missing expected tabbed missing-data controls.")
}

cat(
  "generalized validation passed:",
  gaussian$method,
  "/",
  logistic$method,
  "/",
  count$method,
  "\n"
)
