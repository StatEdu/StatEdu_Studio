all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_general_result_table_contract.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

if (.Platform$OS.type == "windows") {
  invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

render_html <- function(tag) {
  as.character(htmltools::renderTags(tag)$html)
}

contains_in_order <- function(text, values) {
  positions <- vapply(values, function(value) regexpr(value, text, fixed = TRUE)[[1]], integer(1))
  all(positions > 0L) && identical(order(positions), seq_along(positions))
}

old_language <- getOption("statedu.app_language")
on.exit(options(statedu.app_language = old_language), add = TRUE)
options(statedu.app_language = "ko")

message("Checking shared B5 role, language, orientation, and note order...")
narrow <- data.frame(Term = "x", B = ".10", p = ".040", check.names = FALSE)
narrow_html <- render_html(coefficient_html_table(narrow, table_role = "main"))
expect_true(grepl('data-result-table-role="main"', narrow_html, fixed = TRUE), "Main table role was not emitted")
expect_true(grepl('data-result-table-language="en"', narrow_html, fixed = TRUE), "Main table language must be English")
expect_true(grepl('result-table-sheet--b5', narrow_html, fixed = TRUE), "Main table must use a B5 sheet")
expect_true(grepl('result-table-sheet--portrait', narrow_html, fixed = TRUE), "Narrow table must default to B5 portrait")

wide <- as.data.frame(setNames(replicate(12L, "1", simplify = FALSE), paste0("Statistic ", seq_len(12L))), check.names = FALSE)
wide_html <- render_html(coefficient_html_table(wide, table_role = "main"))
expect_true(grepl('result-table-sheet--landscape', wide_html, fixed = TRUE), "Overflowing table must switch to B5 landscape")

appendix <- result_appendix_localize_table(data.frame(Status = "Pass", Message = "Available", check.names = FALSE))
appendix_html <- render_html(coefficient_html_table(appendix, table_role = "appendix"))
expect_true(grepl('data-result-table-role="appendix"', appendix_html, fixed = TRUE), "Appendix table role was not emitted")
expect_true(grepl('data-result-table-language="ko"', appendix_html, fixed = TRUE), "Appendix table must follow the UI language")
expect_true(grepl("상태", appendix_html, fixed = TRUE) && grepl("통과", appendix_html, fixed = TRUE), "Appendix headers and diagnostic values were not localized")

ordered_note <- result_sci_note_text(
  format = "Formatting",
  abbreviations = "Abbreviations",
  estimation = "Estimation",
  reference = "Reference",
  multiplicity = "Multiplicity",
  symbol = "Symbols"
)
expect_true(
  contains_in_order(ordered_note, c("Formatting", "Abbreviations", "Estimation", "Reference", "Multiplicity", "Symbols")),
  "SCI note categories were not emitted in the required order"
)

message("Checking repeated-measures, paired, and nonparametric contracts...")
mixed_html <- render_html(mixed_rm_anova_results_ui(list(
  overview = data.frame(
    Item = c("Analysis", "Analysis population", "Time points"),
    Value = c("Mixed repeated-measures ANOVA", "PP / complete-case repeated-measures ANOVA", "3"),
    check.names = FALSE
  ),
  recommendation = data.frame(
    Item = c("Recommended model", "Data condition"),
    Recommendation = c("Use mixed repeated-measures ANOVA.", "Listwise deletion was applied."),
    Reason = c("One independent variable was selected.", "Excluded cases: 2."),
    check.names = FALSE
  ),
  anova = data.frame(Effect = "Time", F = "4.20", p = ".020", check.names = FALSE),
  method_note = "Greenhouse-Geisser correction was applied",
  assumption = data.frame(
    Item = c("Total cases", "Excluded cases", "Complete cases", "Groups", "Time points", "Sphericity", "GG epsilon", "Covariates", "Levene homogeneity"),
    Result = c("30", "2", "28", "2", "3", "Not satisfied", ".72", "1", "Potential violation"),
    Detail = c(
      "",
      "Rows with missing values in selected variables are excluded listwise.",
      "",
      "A, B",
      "",
      "W=.740; p=.020",
      "Used for corrected within-subject p values.",
      "age",
      "T1=.010; T2=.200; raw values by group."
    ),
    check.names = FALSE
  )
)))
expect_true(grepl('data-result-table-role="main"', mixed_html, fixed = TRUE), "Repeated-measures ANOVA main table contract is missing")
expect_true(grepl('data-result-table-role="appendix"', mixed_html, fixed = TRUE), "Repeated-measures ANOVA appendix table contract is missing")
expect_true(grepl("Greenhouse-Geisser correction", mixed_html, fixed = TRUE), "Repeated-measures ANOVA main note is not SCI formatted")
expect_true(
  all(vapply(
    c("혼합 반복측정 분산분석", "분석 대상", "권장 모형", "목록별 제외를 적용했습니다.", "전체 사례", "제외 사례", "완전 사례", "시점 수", "구형성", "GG 엡실론", "공변량", "Levene 등분산성", "불충족", "위반 가능성", "선택한 변수에 결측값이 있는 행은 목록별로 제외합니다.", "집단별 원자료 값."),
    grepl,
    logical(1),
    x = mixed_html,
    fixed = TRUE
  )),
  "Repeated-measures appendix content was not fully localized to the Korean UI language"
)
expect_true(
  !any(vapply(
    c("Total cases", "Excluded cases", "Complete cases", "Time points", "Sphericity", "GG epsilon", "Potential violation", "Rows with missing values in selected variables are excluded listwise.", "raw values by group."),
    grepl,
    logical(1),
    x = mixed_html,
    fixed = TRUE
  )),
  "Repeated-measures appendix retained known English diagnostic text in the Korean UI language"
)
options(statedu.app_language = "en")
mixed_english <- mixed_rm_appendix_table(data.frame(
  Item = "Total cases",
  Result = "Satisfied",
  Detail = "Rows with missing values in selected variables are excluded listwise.",
  check.names = FALSE
))
expect_true(
  identical(as.character(mixed_english$Item[[1]]), "Total cases") &&
    identical(as.character(mixed_english$Result[[1]]), "Satisfied") &&
    identical(attr(mixed_english, "result_table_language", exact = TRUE), "en"),
  "Repeated-measures appendix must retain English content in the English UI language"
)
options(statedu.app_language = "ko")

paired_table <- data.frame(
  Variable = "Outcome",
  Pre_M = "2.00", Pre_SD = "0.50", Post_M = "2.50", Post_SD = "0.60",
  Statistic = "2.10", p = ".040", StatisticLabel = "t", Method = "Paired t-test",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
paired_html <- render_html(result_table_with_notes(
  paired_grouped_table(paired_main_table(paired_table), "scale", table_role = "main"),
  result_note_tag(paired_main_note("Paired t-test"))
))
expect_true(grepl('data-result-table-role="main"', paired_html, fixed = TRUE), "Paired main table contract is missing")
expect_true(grepl('font-size:12px', paired_html, fixed = TRUE), "Paired table must use the shared 12 px font")

rm_table <- data.frame(
  `Repeated variables` = "Outcome",
  N = "30",
  Time1_label = "Pre", Time1_marker = "a", Time1_M = "2.00", Time1_SD = "0.50",
  Time2_label = "Post", Time2_marker = "b", Time2_M = "2.50", Time2_SD = "0.60",
  Statistic = "4.20", p = ".020", `Post-hoc` = "b>a",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
rm_html <- render_html(result_table_with_notes(paired_rm_grouped_table(rm_table, "scale", table_role = "main")))
expect_true(grepl('data-result-table-role="main"', rm_html, fixed = TRUE), "Paired repeated-measures main table contract is missing")

rm_fallback_table <- data.frame(
  `Repeated variables` = "Outcome",
  N = "30",
  Statistic = "4.20",
  p = ".020",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
rm_fallback_html <- render_html(result_table_with_notes(paired_rm_grouped_table(rm_fallback_table, "scale", table_role = "main")))
fallback_sheet_hits <- gregexpr('data-result-table-sheet="true"', rm_fallback_html, fixed = TRUE)[[1]]
fallback_sheet_count <- if (length(fallback_sheet_hits) == 1L && fallback_sheet_hits[[1]] == -1L) 0L else length(fallback_sheet_hits)
expect_true(fallback_sheet_count == 1L, "Paired repeated-measures fallback must not nest two B5 sheet wrappers around one table")

message("Checking factor-analysis and PCA classifications...")
factor_main_html <- render_html(coefficient_html_table(factor_analysis_main_table(data.frame(Variable = "x1", F1 = ".80")), table_role = "main"))
factor_appendix_html <- render_html(coefficient_html_table(factor_analysis_appendix_table(data.frame(Status = "Pass")), table_role = "appendix"))
pca_main_html <- render_html(coefficient_html_table(pca_main_table(data.frame(Variable = "x1", PC1 = ".80")), table_role = "main"))
pca_appendix_html <- render_html(coefficient_html_table(pca_appendix_table(data.frame(Status = "Pass")), table_role = "appendix"))
expect_true(all(vapply(list(factor_main_html, pca_main_html), grepl, logical(1), pattern = 'data-result-table-language="en"', fixed = TRUE)), "Factor/PCA main tables must be English")
expect_true(all(vapply(list(factor_appendix_html, pca_appendix_html), grepl, logical(1), pattern = 'data-result-table-language="ko"', fixed = TRUE)), "Factor/PCA appendix tables must follow the UI language")

message("Checking logistic, longitudinal, and regression contracts...")
logistic_result <- list(
  dependent = "y",
  dependent_levels = c("0", "1"),
  predictors = "x",
  predictor_levels = list(),
  coef_table = data.frame(Term = "x", B = .2, SE = .1, OR = 1.22, LLCI = 1.01, ULCI = 1.48, p = .04, check.names = FALSE),
  method = "Binary logistic regression",
  fit = list(chisq = 4.2, p = .04, r2 = c(nagelkerke = .12, mcfadden = .08, cox_snell = .09), aic = 120, bic = 128)
)
logistic_html <- render_html(logistic_result_block(logistic_result))
expect_true(grepl('data-result-table-role="main"', logistic_html, fixed = TRUE), "Logistic coefficient table contract is missing")
expect_true(!grepl("Note.", logistic_html, fixed = TRUE), "Logistic main table note is not SCI formatted")

longitudinal_main <- longitudinal_table_section(
  "Publication-ready estimates",
  data.frame(Term = "Time", `B (95% CI)` = ".20 (.05 to .35)", p = ".010", check.names = FALSE),
  role = "main",
  table_fn = coefficient_html_table,
  note_line = result_sci_note_text(abbreviations = "CI = confidence interval")
)
longitudinal_appendix <- longitudinal_table_section(
  "Assumption checks",
  data.frame(Status = "Pass", Message = "Available", check.names = FALSE),
  role = "appendix"
)
longitudinal_html <- render_html(tagList(longitudinal_main, longitudinal_appendix))
expect_true(grepl('data-result-table-role="main"', longitudinal_html, fixed = TRUE) && grepl('data-result-table-role="appendix"', longitudinal_html, fixed = TRUE), "Longitudinal main/appendix separation is missing")
expect_true(length(gregexpr('data-result-table-sheet="true"', longitudinal_html, fixed = TRUE)[[1]]) == 2L, "Longitudinal tables must be emitted as separate sheets")

regression_main_html <- render_html(coefficient_html_table(regression_main_table(data.frame(Term = "x", B = ".20", p = ".010")), table_role = "main"))
regression_appendix_html <- render_html(model_overview_html_table(regression_appendix_table(data.frame(Item = "N", Value = "30"))))
expect_true(grepl('data-result-table-role="main"', regression_main_html, fixed = TRUE), "Regression main table contract is missing")
expect_true(grepl('data-result-table-role="appendix"', regression_appendix_html, fixed = TRUE), "Regression appendix table contract is missing")

regression_diagnostics <- regression_appendix_table(data.frame(
  Item = c("Analysis", "Reason", "Residual normality", "Residual homogeneity", "Autocorrelation"),
  Outcome = c(
    "Bootstrap Regression",
    "Normality not met\nHomogeneity met\nBootstrap used",
    "Normality not met",
    "Homogeneity met",
    "Independent"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
))
regression_diagnostics_html <- render_html(model_overview_html_table(regression_diagnostics))
expect_true(
  all(vapply(
    c("부트스트랩 회귀분석", "정규성 미충족", "등분산성 충족", "부트스트랩 사용", "잔차 정규성", "잔차 등분산성", "자기상관", "독립"),
    grepl,
    logical(1),
    x = regression_diagnostics_html,
    fixed = TRUE
  )),
  "Regression diagnostic appendix content must follow the Korean UI language"
)
effect_guideline_html <- render_html(effect_size_reference_panel(show_f2 = TRUE))
expect_true(
  grepl("효과크기 해석 기준", effect_guideline_html, fixed = TRUE) &&
    grepl("기준", effect_guideline_html, fixed = TRUE) &&
    grepl("작음", effect_guideline_html, fixed = TRUE) &&
    !grepl("Effect Size Guidelines", effect_guideline_html, fixed = TRUE),
  "Regression effect-size guideline appendix must follow the Korean UI language"
)

message("General result screen-table contract validation passed.")
