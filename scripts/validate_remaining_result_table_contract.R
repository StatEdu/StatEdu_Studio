all_args <- commandArgs(trailingOnly = FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_remaining_result_table_contract.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

if (.Platform$OS.type == "windows") {
  invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))
}

source(file.path(repo_root, "R", "utils.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "labels.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "sample_size_ui.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "result_table_ui.R"), encoding = "UTF-8")
suppressPackageStartupMessages(library(shiny))

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

count_fixed <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (identical(matches[[1]], -1L)) 0L else length(matches)
}

render_html <- function(tag) htmltools::renderTags(tag)$html

message("Checking sample-size/effect-size screen result-table contract...")
fixture <- list(
  design_label = "Two independent groups",
  group1 = 64L,
  group2 = 64L,
  total = 128L,
  total_label = "Participants",
  method_note = "Two-sided test with equal allocation",
  formula_note = "Normal approximation was used",
  references = "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences."
)
html <- render_html(sample_size_results_ui(fixture, language = "ko"))

expect_true(count_fixed(html, 'data-result-table-sheet="true"') == 1L, "Sample-size output must emit exactly one independent table sheet")
expect_true(grepl('data-result-table-role="main"', html, fixed = TRUE), "Sample-size result must be a main table")
expect_true(grepl('data-result-table-language="en"', html, fixed = TRUE), "Sample-size main table must remain English in a localized UI")
expect_true(grepl("result-table-sheet--b5", html, fixed = TRUE), "Sample-size result must use the shared B5 sheet")
expect_true(grepl("result-table-sheet--portrait", html, fixed = TRUE), "Narrow sample-size result must default to B5 portrait")
expect_true(grepl("result-table-contract-table", html, fixed = TRUE), "Sample-size table must use the common table contract")
expect_true(grepl("Calculated sample size", html, fixed = TRUE), "Sample-size result labels must be English")
expect_true(grepl("Note. Two-sided test with equal allocation. Normal approximation was used.", html, fixed = TRUE), "Sample-size result must emit a concise SCI note")
expect_true(grepl('class="sample-size-references coefficient-note"', html, fixed = TRUE), "References must inherit the common 11 px note typography")
expect_true(grepl(">References<", html, fixed = TRUE), "Main-table reference heading must be English")

expect_true(
  identical(result_table_orientation(result_table_portrait_capacity() + 1L), "landscape"),
  "A table wider than B5 portrait capacity must switch to landscape"
)

message("Checking effect-size early-return branch...")
effect_html <- render_html(sample_size_results_ui(list(
  result_type = "effect_size",
  primary_effect_size = 0.42,
  primary_effect_size_label = "Hedges' g",
  point_biserial_r = 0.21
), language = "ko"))
expect_true(count_fixed(effect_html, 'data-result-table-sheet="true"') == 1L, "Effect-size output must emit exactly one independent table sheet")
expect_true(grepl('data-result-table-language="en"', effect_html, fixed = TRUE), "Effect-size main table must remain English")
expect_true(grepl("Point-biserial r", effect_html, fixed = TRUE), "Localized UI must not localize main-table effect labels")

appendix_contract <- result_table_contract(role = "appendix", language = "ko", intrinsic_width = 480L)
expect_true(identical(appendix_contract$language, "ko"), "Appendix tables must follow the UI language")

css <- paste(readLines(file.path(repo_root, "www", "style.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
expect_true(grepl("--result-sheet-table-font-size: 12px;", css, fixed = TRUE), "Shared result-table body font must be 12 px")
expect_true(grepl("--result-sheet-header-font-size: 11px;", css, fixed = TRUE), "Shared result-table header font must be 11 px")
expect_true(grepl("--result-sheet-note-font-size: 11px;", css, fixed = TRUE), "Shared result-table note font must be 11 px")

message("Auditing calculator table exclusions...")
calculator_files <- Sys.glob(file.path(repo_root, "R", "calculator_*.R"))
expect_true(length(calculator_files) > 0L, "Calculator source files were not found")
for (path in calculator_files) {
  source_text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  render_ids <- regmatches(source_text, gregexpr("output\\$[[:alnum:]_]+[[:space:]]*<-[[:space:]]*DT::renderDT", source_text, perl = TRUE))[[1]]
  if (length(render_ids) > 0L && !identical(render_ids[[1]], "")) {
    expect_true(all(grepl("_preview[[:space:]]*<-", render_ids, perl = TRUE)), paste(basename(path), "contains a DT output that is not a row-level preview"))
  }
  expect_true(!grepl("renderTable[[:space:]]*\\(", source_text, perl = TRUE), paste(basename(path), "contains an unclassified rendered result table"))
}

message("Remaining screen result-table contract validation passed (sample/effect size included; calculator setup and row previews excluded).")
