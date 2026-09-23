all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_result_table_contract.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

if (.Platform$OS.type == "windows") {
  invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))
}

suppressPackageStartupMessages(library(shiny))
source("R/utils.R")
source("R/setup_analysis_ui.R")
source("R/result_table_ui.R")

render_html <- function(tag) {
  as.character(htmltools::renderTags(tag)$html)
}

assert_contains <- function(text, pattern, label) {
  if (!grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("Result table contract missing: %s", label), call. = FALSE)
  }
}

count_fixed <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (length(matches) == 1L && matches[[1]] == -1L) 0L else length(matches)
}

stopifnot(
  identical(result_main_table_language("ko"), "en"),
  identical(result_main_table_language("en"), "en"),
  identical(result_appendix_table_language("korean"), "ko"),
  identical(result_appendix_table_language("english"), "en"),
  identical(result_table_language("main", "ko"), "en"),
  identical(result_table_language("appendix", "en"), "en"),
  identical(result_table_role("diagnostic"), "appendix"),
  identical(result_table_role("journal"), "main")
)

localized_appendix <- result_appendix_localize_table(
  data.frame(
    Variable = "x",
    Message = "Check coding",
    Status = "Warning",
    Reason = "Sparse cells",
    N = "12",
    Method = "Robust",
    Result = "Not assessed",
    check.names = FALSE
  ),
  "ko"
)
normality_note_ko <- result_appendix_ui_text(
  "Normality is treated as satisfied when each variable has |skewness| < 2 and |kurtosis| < 7.",
  "ko"
)
stopifnot(
  identical(normality_note_ko, "각 변수의 |왜도| < 2 및 |첨도| < 7이면 정규성을 충족한 것으로 판단합니다."),
  !identical(normality_note_ko, "< 7.")
)
same_utf8_bytes <- function(x, y) {
  identical(lapply(as.character(x), charToRaw), lapply(as.character(y), charToRaw))
}
stopifnot(
  same_utf8_bytes(names(localized_appendix), c("변수", "메시지", "상태", "사유", "N", "방법", "결과")),
  same_utf8_bytes(localized_appendix[[3]][[1]], "경고"),
  same_utf8_bytes(localized_appendix[[7]][[1]], "평가하지 않음"),
  identical(attr(localized_appendix, "result_table_role", exact = TRUE), "appendix"),
  identical(attr(localized_appendix, "result_table_language", exact = TRUE), "ko")
)

ordered_notes <- result_sci_notes(
  symbol = c("* p < .05", "* p < .05"),
  reference = "Reference group: Control",
  format = "Values are estimates",
  multiplicity = "p values are BH adjusted",
  abbreviations = "CI = confidence interval",
  estimation = "SEs are robust"
)
stopifnot(identical(
  ordered_notes,
  c(
    "Values are estimates",
    "CI = confidence interval",
    "SEs are robust",
    "Reference group: Control",
    "p values are BH adjusted",
    "* p < .05"
  )
))
note_text <- result_sci_note_text(
  symbol = "* p < .05",
  abbreviations = "CI = confidence interval",
  format = "Values are estimates"
)
stopifnot(identical(
  note_text,
  "Note. Values are estimates. CI = confidence interval. * p < .05."
))

portrait_table <- data.frame(
  Variable = c("Age", "Score"),
  M = c("42.1", "7.2"),
  SD = c("8.4", "1.1"),
  p = c(".120", ".004"),
  check.names = FALSE
)
portrait_html <- render_html(coefficient_html_table(portrait_table, table_language = "ko"))
assert_contains(portrait_html, "result-table-sheet--b5", "B5 sheet class")
assert_contains(portrait_html, "result-table-sheet--main", "main-table role class")
assert_contains(portrait_html, "result-table-sheet--portrait", "portrait default for a narrow table")
assert_contains(portrait_html, "data-result-table-language=\"en\"", "main table language is English")

frequency_table <- as.data.frame(
  stats::setNames(
    replicate(12L, c("1.00", "2.00"), simplify = FALSE),
    c("Variable", "N", "Missing", "Mean", "SD", "Min", "Max", "Median", "IQR", "Skewness", "SE skewness", "Kurtosis")
  ),
  check.names = FALSE
)
frequency_html <- render_html(coefficient_html_table(frequency_table))
assert_contains(frequency_html, "result-table-sheet--landscape", "12-column frequency table auto landscape")
assert_contains(frequency_html, "data-result-table-orientation=\"landscape\"", "landscape orientation metadata")
if (result_table_intrinsic_width(frequency_table) <= result_table_portrait_capacity()) {
  stop("Twelve-column frequency table must exceed B5 portrait capacity.", call. = FALSE)
}

appendix_table <- portrait_table
attr(appendix_table, "result_table_role") <- "appendix"
appendix_html <- render_html(coefficient_html_table(appendix_table, table_language = "ko"))
assert_contains(appendix_html, "result-table-sheet--appendix", "appendix role class")
assert_contains(appendix_html, "data-result-table-language=\"ko\"", "appendix follows UI language")

two_sheet_html <- render_html(tagList(
  coefficient_html_table(portrait_table),
  coefficient_html_table(appendix_table, table_language = "ko")
))
if (count_fixed(two_sheet_html, "data-result-table-sheet=\"true\"") != 2L) {
  stop("Every rendered table must have its own independent sheet wrapper.", call. = FALSE)
}

header_table <- data.frame(
  Path = "X -> Y",
  `Robust\n95% CI` = "[.10, .30]",
  check.names = FALSE
)
header_html <- render_html(coefficient_html_table(header_table))
assert_contains(header_html, "coefficient-header-break", "deliberate header line-break markup")

css <- paste(readLines("www/style.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (fragment in c(
  ".result-table-sheet--portrait",
  ".result-table-sheet--landscape",
  "font-family: Arial, \"Noto Sans KR\", \"Malgun Gothic\", sans-serif;",
  "font-size: var(--result-sheet-table-font-size) !important;",
  "font-size: var(--result-sheet-header-font-size) !important;",
  "font-size: var(--result-sheet-note-font-size);",
  "min-width: var(--result-table-intrinsic-width, 480px) !important;",
  "overflow-wrap: anywhere !important;",
  ".result-table-with-note.result-table-sheet[data-result-table-sheet=\"true\"][data-result-table-orientation]",
  "page: statedu-b5-portrait;",
  "page: statedu-b5-landscape;"
)) {
  assert_contains(css, fragment, fragment)
}

required_trailing_formals <- c("table_role", "table_language", "sheet_orientation")
if (!all(required_trailing_formals %in% names(formals(coefficient_html_table)))) {
  stop("coefficient_html_table is missing common contract arguments.", call. = FALSE)
}

cat("Result table B5 screen contract validation passed.\n")
