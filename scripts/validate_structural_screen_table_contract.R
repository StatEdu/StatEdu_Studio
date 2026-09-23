if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  invisible(try(Sys.setlocale("LC_ALL", "Korean_Korea.utf8"), silent = TRUE))
}

suppressPackageStartupMessages(library(shiny))

`%||%` <- function(value, fallback) {
  if (is.null(value) || !length(value)) fallback else value
}
normalize_app_language <- function(value) {
  value <- tolower(trimws(as.character(value %||% "en")[[1L]]))
  if (startsWith(value, "ko")) "ko" else "en"
}
analysis_has_rows <- function(table) is.data.frame(table) && nrow(table) > 0L
analysis_output_table_style <- function(value, default = "standard") {
  value <- as.character(value %||% default)[[1L]]
  if (value %in% c("standard", "wide", "compact", "compact_xm")) value else default
}
analysis_output_table_style_params <- function(value) {
  list(compact = FALSE, font_size = 12, compact_width = 62, compact_first_width = 118, min_width = 480, hierarchical_min_width = 0)
}

source(file.path("R", "result_table_ui.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_tables.R"), encoding = "UTF-8")

render_html <- function(value) as.character(htmltools::renderTags(value)$html)
count_literal <- function(text, pattern) {
  match <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  if (length(match) == 1L && identical(match[[1L]], -1L)) 0L else length(match)
}
read_source <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
normalized_source <- function(path) gsub("\\s+", " ", read_source(path), perl = TRUE)

portrait_probe <- data.frame(A = "1", B = "2", C = "3", check.names = FALSE)
portrait_html <- render_html(structural_canvas_basic_html_table(portrait_probe))
stopifnot(
  count_literal(portrait_html, "<table") == 1L,
  grepl("structural-table-sheet", portrait_html, fixed = TRUE),
  grepl("structural-table-role-appendix", portrait_html, fixed = TRUE),
  grepl("structural-table-orientation-portrait", portrait_html, fixed = TRUE),
  grepl('data-paper-size="B5"', portrait_html, fixed = TRUE),
  grepl('data-orientation="portrait"', portrait_html, fixed = TRUE),
  grepl('data-result-table-sheet="true"', portrait_html, fixed = TRUE),
  grepl('data-result-table-role="appendix"', portrait_html, fixed = TRUE),
  grepl('data-result-table-language="ko"', portrait_html, fixed = TRUE),
  grepl('data-result-table-orientation="portrait"', portrait_html, fixed = TRUE),
  grepl("structural-table-font-standard", portrait_html, fixed = TRUE)
)

# Model overview is rendered as a complete table sheet by
# structural_canvas_basic_html_table().  The result panel must therefore use a
# plain UI output placeholder; wrapping that placeholder in another sheet
# produces one table nested inside two B5 sheets after Shiny binds the output.
overview_panel_probe <- tags$div(
  class = "result-section regression-result-panel structural-main-result-panel",
  tags$h4("Table 1. Model overview"),
  structural_canvas_basic_html_table(
    data.frame(Item = "Analysis", Value = "SEM", stringsAsFactors = FALSE),
    class = "table table-striped table-bordered structural-overview-table",
    role = "main",
    orientation = "portrait"
  )
)
overview_panel_html <- render_html(overview_panel_probe)
stopifnot(
  count_literal(overview_panel_html, "<table") == 1L,
  count_literal(overview_panel_html, 'data-result-table-sheet="true"') == 1L,
  count_literal(overview_panel_html, 'data-result-table-role="main"') == 1L,
  count_literal(overview_panel_html, 'data-result-table-language="en"') == 1L,
  count_literal(overview_panel_html, 'data-result-table-orientation="portrait"') == 1L,
  grepl("Table 1. Model overview", overview_panel_html, fixed = TRUE),
  grepl("structural-overview-table", overview_panel_html, fixed = TRUE)
)

appendix_body_probe <- data.frame(
  Test = "Mardia skewness",
  Status = "Review",
  Guidance = "Below descriptive .80; review score use",
  Type = "Estimated",
  Matrix = "Residual covariance (theta)",
  `CI method` = "Bias-corrected (BC)",
  Factor = "Review",
  Variable = "Fixed",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
appendix_body_html <- render_html(structural_canvas_basic_html_table(
  appendix_body_probe,
  role = "appendix",
  language = "ko",
  title = "Factor-score quality",
  note = "Not assessed"
))
stopifnot(
  grepl('data-result-table-language="ko"', appendix_body_html, fixed = TRUE),
  grepl("요인점수 품질", appendix_body_html, fixed = TRUE),
  grepl("검정", appendix_body_html, fixed = TRUE),
  grepl("Mardia 왜도", appendix_body_html, fixed = TRUE),
  grepl("검토", appendix_body_html, fixed = TRUE),
  grepl("기술적 .80 기준 미만; 점수 사용 검토", appendix_body_html, fixed = TRUE),
  grepl("추정", appendix_body_html, fixed = TRUE),
  grepl("오차 공분산(theta)", appendix_body_html, fixed = TRUE),
  grepl("편향보정(BC)", appendix_body_html, fixed = TRUE),
  grepl("평가하지 않음", appendix_body_html, fixed = TRUE),
  count_literal(appendix_body_html, ">Review</td>") == 1L,
  count_literal(appendix_body_html, ">Fixed</td>") == 1L,
  !grepl(">Mardia skewness</td>", appendix_body_html, fixed = TRUE)
)

appendix_english_html <- render_html(structural_canvas_basic_html_table(
  appendix_body_probe,
  role = "appendix",
  language = "en",
  title = "Factor-score quality",
  note = "Not assessed"
))
stopifnot(
  grepl('data-result-table-language="en"', appendix_english_html, fixed = TRUE),
  grepl("Factor-score quality", appendix_english_html, fixed = TRUE),
  grepl(">Mardia skewness</td>", appendix_english_html, fixed = TRUE),
  grepl(">Review</td>", appendix_english_html, fixed = TRUE),
  grepl("Not assessed", appendix_english_html, fixed = TRUE),
  !grepl("Mardia 왜도", appendix_english_html, fixed = TRUE)
)

main_probe <- as.data.frame(
  stats::setNames(as.list(rep(".100", 8L)), LETTERS[seq_len(8L)]),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
main_html <- render_html(structural_canvas_basic_html_table(
  main_probe,
  role = "main",
  orientation = "auto",
  language = "ko",
  note = result_sci_note_text(abbreviations = "SE = standard error", estimation = "Model-based inference")
))
stopifnot(
  count_literal(main_html, "<table") == 1L,
  grepl("structural-table-role-main", main_html, fixed = TRUE),
  grepl("structural-table-orientation-portrait", main_html, fixed = TRUE),
  grepl('data-result-table-role="main"', main_html, fixed = TRUE),
  grepl('data-result-table-language="en"', main_html, fixed = TRUE),
  grepl('data-result-table-orientation="portrait"', main_html, fixed = TRUE),
  grepl("<table", main_html, fixed = TRUE),
  grepl("Note. SE = standard error. Model-based inference.", main_html, fixed = TRUE),
  regexpr("<table", main_html, fixed = TRUE)[[1L]] < regexpr("Note. SE = standard error", main_html, fixed = TRUE)[[1L]]
)

main_body_html <- render_html(structural_canvas_basic_html_table(
  appendix_body_probe,
  role = "main",
  language = "ko",
  title = "Factor-score quality",
  note = "Not assessed"
))
stopifnot(
  grepl('data-result-table-language="en"', main_body_html, fixed = TRUE),
  grepl("Factor-score quality", main_body_html, fixed = TRUE),
  grepl(">Mardia skewness</td>", main_body_html, fixed = TRUE),
  grepl("Not assessed", main_body_html, fixed = TRUE),
  !grepl("Mardia 왜도", main_body_html, fixed = TRUE),
  !grepl("요인점수 품질", main_body_html, fixed = TRUE)
)

multi_note_html <- render_html(structural_canvas_basic_html_table(
  portrait_probe,
  role = "main",
  note = tagList(
    tags$p(class = "structural-result-note structural-main-note structural-main-note-1", "Note. First note."),
    tags$p(class = "structural-result-note structural-main-note structural-main-note-2", "Second note.")
  )
))
stopifnot(
  count_literal(multi_note_html, "<table") == 1L,
  count_literal(multi_note_html, 'data-result-table-sheet="true"') == 1L,
  grepl("Note. First note.", multi_note_html, fixed = TRUE),
  grepl("Second note.", multi_note_html, fixed = TRUE),
  regexpr("<table", multi_note_html, fixed = TRUE)[[1L]] < regexpr("Note. First note.", multi_note_html, fixed = TRUE)[[1L]],
  regexpr("Note. First note.", multi_note_html, fixed = TRUE)[[1L]] < regexpr("Second note.", multi_note_html, fixed = TRUE)[[1L]]
)

wide_probe <- as.data.frame(
  stats::setNames(as.list(rep(".100", 10L)), paste0("Statistic ", seq_len(10L))),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
wide_html <- render_html(structural_canvas_basic_html_table(wide_probe, role = "main", orientation = "auto"))
stopifnot(
  count_literal(wide_html, "<table") == 1L,
  grepl("structural-table-orientation-landscape", wide_html, fixed = TRUE),
  grepl('data-orientation="landscape"', wide_html, fixed = TRUE),
  structural_canvas_table_needs_landscape(wide_probe),
  !structural_canvas_table_needs_landscape(portrait_probe)
)

long_header_probe <- as.data.frame(
  stats::setNames(
    as.list(rep(".100", 7L)),
    paste("Bootstrap confidence interval statistic", seq_len(7L))
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
stopifnot(structural_canvas_table_needs_landscape(long_header_probe))

long_cell_probe <- data.frame(
  Path = "A very long latent-variable path label that cannot fit safely in a B5 portrait table",
  Decision = "Review the residual-covariance diagnostics and the full sensitivity-analysis conclusion",
  Estimate = ".100",
  p = ".020",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
stopifnot(structural_canvas_table_needs_landscape(long_cell_probe))

render_files <- Sys.glob(file.path("R", "setup_custom_model_canvas_structural_render*.R"))
invisible(lapply(render_files, parse))

raw_table_files <- render_files[vapply(render_files, function(path) {
  grepl("tags$table(", read_source(path), fixed = TRUE)
}, logical(1))]
allowed_raw_table_files <- file.path("R", c(
  "setup_custom_model_canvas_structural_render_fit_core.R",
  "setup_custom_model_canvas_structural_render_mi.R",
  "setup_custom_model_canvas_structural_render_tables.R"
))
stopifnot(all(raw_table_files %in% allowed_raw_table_files))
for (path in raw_table_files) {
  stopifnot(grepl("structural_canvas_table_sheet", read_source(path), fixed = TRUE))
}

render_source <- normalized_source(file.path("R", "setup_custom_model_canvas_structural_render.R"))
fit_source <- normalized_source(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"))
fit_core_source <- normalized_source(file.path("R", "setup_custom_model_canvas_structural_render_fit_core.R"))
moderation_source <- normalized_source(file.path("R", "setup_custom_model_canvas_structural_render_moderation.R"))
invariance_source <- normalized_source(file.path("R", "setup_custom_model_canvas_structural_render_invariance.R"))
table_source <- normalized_source(file.path("R", "setup_custom_model_canvas_structural_render_tables.R"))
complex_custom_source <- normalized_source(file.path("R", "setup_complex_sample_custom_model_ui.R"))
css_source <- read_source(file.path("www", "model-canvas", "canvas.css"))

stopifnot(
  grepl('result_table(kind, "en")', render_source, fixed = TRUE),
  grepl('appendix_result_table <- function(kind) result_table(kind, ui_language())', render_source, fixed = TRUE),
  grepl('paste0("Table ", number, ". ", title)', render_source, fixed = TRUE),
  grepl("structural-main-result-panel", render_source, fixed = TRUE),
  grepl("structural-appendix-result-panel", render_source, fixed = TRUE),
  !grepl("renderTable(", render_source, fixed = TRUE),
  grepl("structural-overview-table", render_source, fixed = TRUE),
  grepl('h4(table_heading("overview", "모형 개요", "Model overview")), uiOutput(paste0(prefix, "_result_overview"))', render_source, fixed = TRUE),
  !grepl('structural_canvas_table_sheet( div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview")))', render_source, fixed = TRUE),
  grepl("_result_invariance_appendix", render_source, fixed = TRUE),
  !grepl("landscape-table-panel structural-supplementary-result", render_source, fixed = TRUE),
  grepl('structural_canvas_invariance_result_ui( fit_result(), "en"', fit_source, fixed = TRUE),
  grepl("structural_canvas_invariance_appendix_ui( fit_result(), statedu_current_language(app_language_fn)", fit_source, fixed = TRUE),
  grepl('language = "en"', moderation_source, fixed = TRUE),
  grepl('role = "main"', moderation_source, fixed = TRUE),
  grepl("structural_canvas_invariance_appendix_ui <- function", invariance_source, fixed = TRUE),
  grepl("role = \"appendix\"", invariance_source, fixed = TRUE),
  grepl("note = result_sci_note_text", invariance_source, fixed = TRUE),
  grepl('appendix_table(result$overview)', complex_custom_source, fixed = TRUE),
  grepl('appendix_table(result$syntax)', complex_custom_source, fixed = TRUE),
  grepl('main_table(result$coefficients)', complex_custom_source, fixed = TRUE),
  grepl('main_table(result$effects)', complex_custom_source, fixed = TRUE),
  grepl('table_language = "en"', complex_custom_source, fixed = TRUE),
  grepl("result_sci_note_text", complex_custom_source, fixed = TRUE)
)

stopifnot(
  grepl('class = "table table-striped table-bordered structural-path-table", role = "main", orientation = "landscape", note = tags$p(', render_source, fixed = TRUE),
  grepl('class = "table table-striped table-bordered structural-covariate-effect-table", role = "main", orientation = "auto", note = "Note. Robust rows', render_source, fixed = TRUE),
  grepl('class = "table table-striped table-bordered structural-covariate-fit-table", role = "main", orientation = "auto", note = "Note. Model differences', render_source, fixed = TRUE),
  grepl('class = "table table-striped table-bordered structural-pls-fit-diagnostics-table", role = "main", orientation = "portrait", note = "Note. SRMR', fit_source, fixed = TRUE),
  grepl('role = "main", orientation = "auto", note = "Note. Regions are shown only', moderation_source, fixed = TRUE),
  !grepl('structural-path-table", role = "main", orientation = "landscape"), tags$p(', render_source, fixed = TRUE),
  !grepl('structural-pls-fit-diagnostics-table", role = "main", orientation = "portrait"), tags$p(', fit_source, fixed = TRUE)
)

stopifnot(
  grepl('structural_canvas_specific_indirect_html_table( table, "en", note = tags$p(', render_source, fixed = TRUE),
  grepl('structural-validity-table", role = "main", orientation = "auto", note = tagList(', render_source, fixed = TRUE),
  grepl('structural_canvas_pls_measurement_main_html_table( table, note = tags$p(', render_source, fixed = TRUE),
  grepl('structural_canvas_measurement_html_table( table, note = tagList(', render_source, fixed = TRUE),
  grepl('orientation = "landscape", note = tagList(', invariance_source, fixed = TRUE),
  grepl('Indirect and total effects and extended diagnostics are reported in the supplementary tables.', fit_core_source, fixed = TRUE),
  !grepl('if (identical(analysis_type, "plssem")) tagList( tags$p(class = "structural-result-note structural-main-note structural-main-note-3"', render_source, fixed = TRUE),
  grepl('note_is_tag <- inherits(note, "shiny.tag") || inherits(note, "shiny.tag.list")', table_source, fixed = TRUE)
)

stopifnot(
  grepl("structural-table-page-b5", css_source, fixed = TRUE),
  grepl("structural-table-font-standard", css_source, fixed = TRUE),
  grepl("structural-table-orientation-portrait", css_source, fixed = TRUE),
  grepl("structural-table-orientation-landscape", css_source, fixed = TRUE),
  grepl("size: B5 portrait", css_source, fixed = TRUE),
  grepl("size: B5 landscape", css_source, fixed = TRUE),
  grepl('--structural-result-font-family: Arial, "Noto Sans KR", "Malgun Gothic", sans-serif', css_source, fixed = TRUE),
  grepl("structural-table-orientation-landscape \\{[^}]*width: min\\(100%, 890px\\)", css_source, perl = TRUE),
  grepl("\\.structural-table-font-standard table \\{[^}]*font-size: 12px !important", css_source, perl = TRUE),
  grepl("\\.structural-table-font-standard th \\{[^}]*font-size: 11px !important", css_source, perl = TRUE),
  grepl("\\.structural-table-font-standard td \\{[^}]*font-size: 12px !important", css_source, perl = TRUE),
  grepl("\\.structural-main-note,[^}]*font-size: 11px", css_source, perl = TRUE),
  !grepl("min-width: 1120px", css_source, fixed = TRUE),
  !grepl("min-width: 1080px", css_source, fixed = TRUE),
  !grepl("min-width: 1040px", css_source, fixed = TRUE),
  grepl("table.structural-group-gate-table,[^}]+width: 100% !important;[^}]+min-width: 0 !important;[^}]+max-width: 100% !important;[^}]+table-layout: fixed !important;", css_source, perl = TRUE),
  grepl("structural-multigroup-path-table th,[^}]+white-space: normal !important;[^}]+overflow-wrap: anywhere !important;", css_source, perl = TRUE),
  grepl("td.structural-numeric-cell,[^}]+white-space: nowrap !important;", css_source, perl = TRUE)
)

custom_model_canvas_text <- function(language, english, korean) {
  if (identical(normalize_app_language(language), "ko")) korean else english
}
statedu_initial_language <- function() "ko"
source(file.path("R", "setup_complex_sample_ui.R"), encoding = "UTF-8")
source(file.path("R", "setup_complex_sample_custom_model_ui.R"), encoding = "UTF-8")
custom_result <- list(
  overview = data.frame(Item = c("Analysis", "Analysis N"), Value = c("Complex Samples Mediation / Moderation", "120"), stringsAsFactors = FALSE),
  syntax = data.frame(Item = "Equation", Value = "Y ~ X", stringsAsFactors = FALSE),
  coefficients = data.frame(Equation = "Y", Term = "X", Estimate = ".250", SE = ".050", Statistic = "5.000", `p-value` = "< .001", check.names = FALSE),
  effects = data.frame(Effect = "Indirect", Estimate = ".100", SE = ".030", `p-value` = ".001", check.names = FALSE),
  design = list(meta = list(design_type = "taylor", lonely_psu = "adjust"))
)
collect_contract_sheets <- function(value) {
  sheets <- list()
  visit <- function(node) {
    if (inherits(node, "shiny.tag")) {
      if (identical(node$attribs$`data-result-table-sheet` %||% "", "true")) {
        sheets[[length(sheets) + 1L]] <<- node
      }
      invisible(lapply(node$children %||% list(), visit))
    } else if (is.list(node)) {
      invisible(lapply(node, visit))
    }
    invisible(NULL)
  }
  visit(value)
  sheets
}
custom_ui <- complex_sample_custom_model_result_ui(custom_result, "ko")
custom_sheets <- collect_contract_sheets(custom_ui)
custom_html <- render_html(custom_ui)
custom_sheet_tags <- regmatches(
  custom_html,
  gregexpr('<div[^>]*data-result-table-sheet="true"[^>]*>', custom_html, perl = TRUE)
)[[1L]]
stopifnot(
  length(custom_sheets) == 5L,
  sum(vapply(custom_sheets, function(sheet) identical(sheet$attribs$`data-result-table-role`, "appendix"), logical(1))) == 3L,
  sum(vapply(custom_sheets, function(sheet) identical(sheet$attribs$`data-result-table-language`, "ko"), logical(1))) == 3L,
  sum(vapply(custom_sheets, function(sheet) identical(sheet$attribs$`data-result-table-role`, "main"), logical(1))) == 2L,
  sum(vapply(custom_sheets, function(sheet) identical(sheet$attribs$`data-result-table-language`, "en"), logical(1))) == 2L,
  all(vapply(
    custom_sheets[vapply(custom_sheets, function(sheet) identical(sheet$attribs$`data-result-table-role`, "main"), logical(1))],
    function(sheet) grepl("Note. SE = standard error; CI = confidence interval.", render_html(sheet), fixed = TRUE),
    logical(1)
  )),
  length(custom_sheet_tags) == 5L,
  sum(grepl('data-result-table-role="appendix"', custom_sheet_tags, fixed = TRUE)) == 3L,
  sum(grepl('data-result-table-language="ko"', custom_sheet_tags, fixed = TRUE)) == 3L,
  sum(grepl('data-result-table-role="main"', custom_sheet_tags, fixed = TRUE)) == 2L,
  sum(grepl('data-result-table-language="en"', custom_sheet_tags, fixed = TRUE)) == 2L,
  grepl("모형 개요", custom_html, fixed = TRUE),
  grepl("분석 구문", custom_html, fixed = TRUE),
  grepl("복합표본 설계", custom_html, fixed = TRUE),
  grepl("Survey-weighted path coefficients", custom_html, fixed = TRUE),
  grepl("Direct, indirect, and conditional effects", custom_html, fixed = TRUE),
  count_literal(custom_html, "Note. SE = standard error; CI = confidence interval.") == 2L
)

cat("Structural screen-table contract validation passed.\n")
