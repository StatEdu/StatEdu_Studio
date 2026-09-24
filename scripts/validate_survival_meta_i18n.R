Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()

# Inspect the actual helper dictionaries, so newly added labels cannot silently
# fall back to English without extending the six locale dictionaries.
dictionary <- function(fn) {
  statements <- as.list(body(fn))
  assignment <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name("labels")), statements)
  stopifnot(length(assignment) == 1L)
  eval(assignment[[1]][[3]])
}
labels <- list(survival = dictionary(survival_ui_text), meta = dictionary(meta_ui_text))
key_for <- function(en) paste0("analysis.ui.", gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(en)))))
parse_html <- function(ui) xml2::read_html(as.character(htmltools::renderTags(ui)$html))
controls <- function(ui) {
  doc <- parse_html(ui)
  nodes <- xml2::xml_find_all(doc, "//input|//select|//option")
  lapply(nodes, function(node) xml2::xml_attrs(node)[intersect(c("id", "name", "type", "value", "min", "max", "step", "selected", "multiple", "checked"), names(xml2::xml_attrs(node)))])
}
out <- "tmp/multilingual"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
catalog <- statedu_translation_table()
# Confirm every newly routed literal, including labels inside option vectors,
# has a translation. This excludes user-supplied variable names by construction.
literal_pairs <- list()
collect <- function(expr) {
  if (!is.call(expr)) return(invisible(NULL))
  if (identical(expr[[1]], as.name("statedu_localized_text")) && length(expr) >= 4L) {
    literal_pairs[[length(literal_pairs) + 1L]] <<- list(en = eval(expr[[3]]), ko = eval(expr[[4]]))
  } else for (child in as.list(expr)[-1L]) collect(child)
}
for (fn in list(survival_setup_tab_panel, survival_contract_setup_panel, meta_family_choices, meta_effect_fields_ui)) collect(body(fn))
for (language in c("en", "ko", "ja", "zh", "es", "fr", "de", "vi")) {
  options(statedu.app_language = language)
  for (pair in literal_pairs) {
    values <- statedu_localized_text(language, pair$en, pair$ko)
    expected <- if (language %in% c("en", "ko")) pair[[language]] else vapply(pair$en, function(en) unname(catalog[[key_for(en)]][language]), character(1), USE.NAMES = FALSE)
    stopifnot(!anyNA(expected), identical(values, expected))
  }
  for (kind in names(labels)) for (key in names(labels[[kind]])) {
    pair <- labels[[kind]][[key]]
    actual <- if (kind == "survival") survival_ui_text(key, language) else meta_ui_text(key, language)
    expected <- if (language %in% c("en", "ko")) unname(pair[[language]]) else unname(catalog[[key_for(pair[["en"]])]][language])
    stopifnot(length(expected) == 1L, !is.na(expected), nzchar(expected), identical(actual, expected))
  }
  stopifnot(identical(unname(meta_family_choices(language)), c("g", "r", "or")))
  for (family in c("g", "r", "or")) {
    types <- meta_input_types(family)
    stopifnot(identical(unname(meta_input_type_choices(family, language)), names(types)))
    for (type in names(types)) {
      record <- as.list(setNames(rep(12.5, length(meta_input_field_map(family)[[type]])), meta_input_field_map(family)[[type]]))
      ui <- meta_effect_fields_ui(family, type, record, language)
      baseline <- meta_effect_fields_ui(family, type, record, "en")
      stopifnot(identical(controls(ui), controls(baseline)))
      if (!language %in% c("en", "ko")) {
        doc <- parse_html(ui)
        field_labels <- xml2::xml_text(xml2::xml_find_all(doc, "//label"))
        stopifnot(!any(grepl("Group [01]|sample size|Standard error|coefficient|Number of controls|[\uac00-\ud7a3]", field_labels, perl = TRUE)))
      }
    }
  }
  stopifnot(identical(controls(meta_analysis_tab_panel(language)), controls(meta_analysis_tab_panel("en"))))
  stopifnot(identical(controls(survival_setup_tab_panel(language)), controls(survival_setup_tab_panel("en"))))
  for (shape in c("single_record", "entry_exit", "start_stop", "interval_censored")) {
    args <- list(selected_names = c("사용자 변수", "time", "status"), shape = shape)
    ui <- do.call(survival_contract_setup_panel, c(args, list(language = language)))
    baseline <- do.call(survival_contract_setup_panel, c(args, list(language = "en")))
    stopifnot(identical(controls(ui), controls(baseline)))
  }
  htmltools::save_html(tagList(meta_analysis_tab_panel(language), meta_effect_fields_ui("g", "means", language = language)), file.path(out, paste0("meta-", language, ".html")))
  htmltools::save_html(tagList(survival_setup_tab_panel(language), survival_contract_setup_panel(c("사용자 변수", "time", "status"), language = language)), file.path(out, paste0("survival-guide-", language, ".html")))
  for (type in c("survival", "event", "cumhaz", "log_survival")) {
    label <- survival_plot_type_label(type, language)
    if (!language %in% c("en", "ko")) stopifnot(!grepl("Survival|Cumulative|Log survival", label))
  }
  cat("PASS:", language, "survival/meta dictionaries, all 14 input formats and unchanged control values\n")
}
