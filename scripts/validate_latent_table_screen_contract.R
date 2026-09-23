options(stringsAsFactors = FALSE)

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_path <- if (length(file_arg) > 0L) {
  normalizePath(file_arg[[1L]], winslash = "/", mustWork = TRUE)
} else {
  normalizePath("scripts/validate_latent_table_screen_contract.R", winslash = "/", mustWork = TRUE)
}
repo_root <- dirname(dirname(script_path))
server_path <- file.path(repo_root, "modules", "latent_mplus", "app", "R", "app_server.R")
css_path <- file.path(repo_root, "modules", "latent_mplus", "app", "www", "style.css")
module_path <- file.path(repo_root, "R", "latent_mplus_module.R")

for (path in c(server_path, css_path, module_path)) {
  assert_true(file.exists(path), paste0("Required latent table file is missing: ", path))
}

server_expr <- parse(file = server_path)
invisible(parse(file = module_path))

helper_names <- c(
  "latent_result_table_role",
  "latent_result_ui_language",
  "latent_result_table_language",
  "latent_result_role_label",
  "latent_result_orientation_label",
  "latent_result_table_intrinsic_width",
  "latent_result_table_orientation",
  "latent_result_table_section_class",
  "latent_result_table_section_style",
  "latent_result_table_wrap_style",
  "latent_result_description_map",
  "table_description",
  "latent_result_table_caption",
  "latent_result_localize_value",
  "latent_result_main_note",
  "latent_prepare_result_table_for_screen"
)
helper_env <- new.env(parent = baseenv())
helper_env$`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}
helper_env$assert_true <- assert_true

for (helper_name in helper_names) {
  matched <- FALSE
  for (expression in server_expr) {
    if (
      is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      identical(as.character(expression[[2L]]), helper_name)
    ) {
      eval(expression, envir = helper_env)
      matched <- TRUE
      break
    }
  }
  assert_true(matched, paste0("Latent screen helper is missing: ", helper_name))
}

with(helper_env, {
  assert_true(identical(latent_result_table_role("T5C.csv"), "main"), "T* must be classified as a main table.")
  assert_true(identical(latent_result_table_role("A5.csv"), "appendix"), "A* must be classified as an appendix table.")
  assert_true(identical(latent_result_table_role("S5.csv"), "appendix"), "S* must be classified as an appendix table.")
  assert_true(identical(latent_result_table_role("estimation_registry.csv"), "appendix"), "Estimation diagnostics must be appendix tables.")
  assert_true(identical(latent_result_table_role("bch_posthoc.csv"), "appendix"), "BCH diagnostics must be appendix tables.")
  assert_true(identical(latent_result_table_language("T6", language = "ko"), "en"), "Main latent tables must remain English.")
  assert_true(identical(latent_result_table_language("A5", language = "ko"), "ko"), "Appendix latent tables must follow the UI language.")

  compact_eight <- data.frame(
    Path = c("A -> B", "B -> C"),
    B = c(".12", ".24"), SE = c(".03", ".04"), z = c("4.0", "6.0"),
    p = c("<.001", "<.001"), LLCI = c(".06", ".16"), ULCI = c(".18", ".32"),
    beta = c(".21", ".35"),
    check.names = FALSE
  )
  long_text_seven <- data.frame(
    Path = c(
      "Predictor -> outcome",
      "A very long structural path or diagnostic description that requires substantially more horizontal space"
    ),
    B = c(".12", ".24"), SE = c(".03", ".04"), z = c("4.0", "6.0"),
    p = c("<.001", "<.001"), LLCI = c(".06", ".16"), ULCI = c(".18", ".32"),
    check.names = FALSE
  )
  compact_width <- latent_result_table_intrinsic_width(compact_eight)
  long_width <- latent_result_table_intrinsic_width(long_text_seven)
  assert_true(compact_width <= 590L, "The compact eight-column probe must fit the B5 portrait content width.")
  assert_true(long_width > 590L, "The long-text seven-column probe must exceed the B5 portrait content width.")
  assert_true(identical(latent_result_table_orientation("T3", column_count = 4L), "portrait"), "A narrow T* table must default to B5 portrait.")
  assert_true(identical(latent_result_table_orientation("A5", column_count = 7L), "portrait"), "A compact seven-column fallback must remain B5 portrait.")
  assert_true(identical(latent_result_table_orientation("T2", table_data = compact_eight), "portrait"), "A compact eight-column table must remain B5 portrait.")
  assert_true(identical(latent_result_table_orientation("S5", table_data = long_text_seven), "landscape"), "A long-text seven-column table must use B5 landscape.")
  assert_true(identical(latent_result_table_orientation("T7"), "portrait"), "B5 portrait must be the fallback orientation.")

  main_class <- latent_result_table_section_class("T4", table_data = compact_eight)
  appendix_class <- latent_result_table_section_class("A4", table_data = long_text_seven)
  assert_true(grepl("latent-result-table-main", main_class, fixed = TRUE), "The main-table role class is missing.")
  assert_true(grepl("latent-result-table-language-en", main_class, fixed = TRUE), "The main-table English class is missing.")
  assert_true(grepl("latent-result-table-b5-portrait", main_class, fixed = TRUE), "The portrait B5 class is missing.")
  assert_true(grepl("latent-result-table-appendix", appendix_class, fixed = TRUE), "The appendix-table role class is missing.")
  assert_true(grepl("latent-result-table-language-ui", appendix_class, fixed = TRUE), "The appendix UI-language class is missing.")
  assert_true(grepl("latent-result-table-b5-landscape", appendix_class, fixed = TRUE), "The landscape B5 class is missing.")
  assert_true(
    grepl("margin-left: auto", latent_result_table_section_style("T3", column_count = 4L), fixed = TRUE),
    "Each B5 table sheet must be centered as a separate panel."
  )
  assert_true(
    grepl("--latent-b5-portrait-width", latent_result_table_wrap_style("T3", column_count = 4L), fixed = TRUE),
    "A standalone portrait table wrapper must retain the B5 portrait width."
  )
  assert_true(
    grepl("--latent-b5-landscape-width", latent_result_table_wrap_style("T2", table_data = long_text_seven), fixed = TRUE),
    "A standalone landscape table wrapper must retain the B5 landscape width."
  )

  fallback_manifest <- data.frame(
    table = "custom_diagnostic",
    description = "Custom convergence diagnostic",
    stringsAsFactors = FALSE
  )
  assert_true(
    identical(table_description("custom_diagnostic", fallback_manifest, language = "ko"), "Custom convergence diagnostic"),
    "An untranslated appendix description must fall back to English."
  )

  main_data <- data.frame(
    V1 = c(
      "\uAE30\uC874 \uD55C\uAE00 \uC81C\uBAA9",
      "",
      "Predictor",
      "X1",
      "Note. Values are relative risk ratios. Reference profile = Profile 2. Significance: *p < .05."
    ),
    V2 = c("", "", "RRR", "1.24*", ""),
    V3 = c("", "", "SE", "0.12", ""),
    check.names = FALSE
  )
  prepared_main <- latent_prepare_result_table_for_screen(main_data, "T5", language = "ko")
  assert_true(grepl("^Table 5\\.", prepared_main[[1L]][[1L]]), "A main-table caption must be English even when the UI is Korean.")
  assert_true(identical(prepared_main[[1L]][[3L]], "Predictor"), "Main-table headers must remain English.")
  note_rows <- grepl("^Note\\.", trimws(prepared_main[[1L]]), ignore.case = TRUE)
  assert_true(sum(note_rows) == 1L, "A main table must contain exactly one concise note row.")
  main_note <- prepared_main[[1L]][note_rows][[1L]]
  assert_true(grepl("RRR = relative risk ratio", main_note, fixed = TRUE), "The SCI note must define RRR.")
  assert_true(grepl("SE = standard error", main_note, fixed = TRUE), "The SCI note must define SE.")
  assert_true(grepl("Reference class/profile: Profile 2", main_note, fixed = TRUE), "The reference profile must be retained in the SCI note.")
  assert_true(grepl("*p < .05; **p < .01; ***p < .001.", main_note, fixed = TRUE), "Significance notes must use the ordered SCI format.")
  assert_true(!grepl("Values are", main_note, fixed = TRUE), "Verbose legacy prose must not remain in a main-table note.")

  for (table_key in c("T6", "T6D", "T6E")) {
    ordered_note <- latent_result_main_note(
      table_key,
      data.frame(
        Statistic = "Post-hoc",
        Result = "Profile 1 > Profile 2*",
        check.names = FALSE
      ),
      existing_notes = "Reference profile = Profile 2."
    )
    reference_position <- regexpr("Reference class/profile:", ordered_note, fixed = TRUE)[[1L]]
    multiplicity_position <- regexpr("Post-hoc entries report", ordered_note, fixed = TRUE)[[1L]]
    symbol_position <- regexpr("*p < .05", ordered_note, fixed = TRUE)[[1L]]
    assert_true(startsWith(ordered_note, "Note. "), paste0(table_key, " must use the SCI Note. prefix."))
    assert_true(
      all(c(reference_position, multiplicity_position, symbol_position) > 0L) &&
        reference_position < multiplicity_position && multiplicity_position < symbol_position,
      paste0(table_key, " SCI notes must follow reference -> multiplicity/post-hoc -> symbol order.")
    )
  }

  appendix_data <- data.frame(
    V1 = c("Classification summary", "", "Variable", "Profile 1", "Note. Diagnostic output."),
    V2 = c("", "", "Value", "Yes", ""),
    check.names = FALSE
  )
  prepared_appendix <- latent_prepare_result_table_for_screen(appendix_data, "A5", language = "ko")
  assert_true(grepl("^\uBD80\uB85D\uD45C A5\\.", prepared_appendix[[1L]][[1L]]), "An appendix caption must follow the Korean UI language.")
  assert_true(identical(prepared_appendix[[1L]][[3L]], "\uBCC0\uC218"), "Appendix headers must follow the UI language when a translation exists.")
  assert_true(identical(prepared_appendix[[2L]][[3L]], "\uAC12"), "Appendix value headers must follow the UI language.")
  assert_true(identical(prepared_appendix[[1L]][[4L]], "\uD504\uB85C\uD30C\uC77C 1"), "Appendix class/profile labels must follow the UI language.")
  assert_true(grepl("^\uC8FC\\.", prepared_appendix[[1L]][[5L]]), "Appendix notes must follow the UI language.")
})

server_text <- paste(readLines(server_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
css_text <- paste(readLines(css_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
module_text <- paste(readLines(module_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

for (token in c(
  "table_blocks <- lapply",
  "latent-result-table-meta",
  "latent_result_table_column_count_for_path",
  "latent_result_table_intrinsic_width_for_path",
  "latent_prepare_result_table_for_screen"
)) {
  assert_true(grepl(token, server_text, fixed = TRUE), paste0("The one-table screen renderer is missing: ", token))
}

wrapper_expression <- NULL
for (expression in server_expr) {
  if (
    is.call(expression) &&
    identical(expression[[1L]], as.name("<-")) &&
    identical(as.character(expression[[2L]]), "latent_excel_like_table_ui")
  ) {
    wrapper_expression <- expression
    break
  }
}
assert_true(!is.null(wrapper_expression), "The latent Excel-like table wrapper is missing.")
wrapper_text <- paste(deparse(wrapper_expression, width.cutoff = 500L), collapse = "\n")
for (token in c(
  "data-result-table-sheet",
  "data-result-table-role",
  "data-result-table-language",
  "data-result-table-orientation",
  "intrinsic_width <- latent_result_table_intrinsic_width(data)",
  "table_language <- latent_result_table_language",
  "table_orientation <- latent_result_table_orientation"
)) {
  assert_true(grepl(token, wrapper_text, fixed = TRUE), paste0("The actual latent table wrapper is missing metadata: ", token))
}

final_contract_position <- regexpr("Final screen contract for latent LCA/LPA/mixture tables", css_text, fixed = TRUE)[[1L]]
legacy_small_font_position <- max(gregexpr("font-size: 8.2px", css_text, fixed = TRUE)[[1L]])
assert_true(final_contract_position > 0L, "The final latent screen CSS contract is missing.")
assert_true(final_contract_position > legacy_small_font_position, "The final font contract must override legacy latent table sizing.")

normalized_css <- gsub("\\s+", " ", substr(css_text, final_contract_position, nchar(css_text)), perl = TRUE)
module_contract_position <- regexpr("Final screen contract for latent LCA/LPA/mixture tables", module_text, fixed = TRUE)[[1L]]
assert_true(module_contract_position > 0L, "The mirrored final latent screen CSS contract is missing.")
normalized_module_css <- gsub("\\s+", " ", substr(module_text, module_contract_position, nchar(module_text)), perl = TRUE)
for (contract_text in list(normalized_css, normalized_module_css)) {
  assert_true(
    grepl(".latent-excel-table-wrap.latent-result-table-b5-page .latent-excel-table td { font-size: 12px !important;", contract_text, fixed = TRUE),
    "Latent table body cells must use the 12px screen contract."
  )
  assert_true(
    grepl(".latent-excel-table-wrap.latent-result-table-b5-page .latent-excel-table th { background: #f4f7fa !important; font-size: 11px !important;", contract_text, fixed = TRUE),
    "Latent table headers must use the 11px screen contract."
  )
  assert_true(
    grepl(".latent-excel-table-wrap.latent-result-table-b5-page .latent-excel-note-row td { background: #ffffff !important; border-bottom: 0 !important; border-top: 1px solid #1f2937 !important; font-size: 11px !important;", contract_text, fixed = TRUE),
    "Latent table notes must use the 11px screen contract."
  )
}

for (token in c(
  "break-after: page !important",
  "--latent-b5-portrait-width",
  "--latent-b5-landscape-width",
  "590px",
  "890px",
  "overflow-x: auto !important",
  "font-family: Arial",
  "font-size: 12px !important",
  "font-size: 11px !important",
  "white-space: normal !important",
  "latent-result-table-main",
  "latent-result-table-appendix"
)) {
  assert_true(grepl(token, css_text, fixed = TRUE), paste0("The latent CSS screen contract is missing: ", token))
  assert_true(grepl(token, module_text, fixed = TRUE), paste0("The mirrored latent CSS contract is missing: ", token))
}

cat("latent table screen contract validation PASS\n")
