Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
options(statedu.app_language = "ko")
out <- "tmp/all-analysis-notes"
dir.create(file.path(out, "rendered"), recursive = TRUE, showWarnings = FALSE)
renderers <- list(
  `19` = frequencies_results_ui, `20` = reliability_results_ui,
  `21` = correlation_results_ui, `22` = crosstab_results_ui,
  `23` = ttest_anova_results_ui, `24` = ancova_results_ui,
  `25` = paired_results_ui, `26` = paired_rm_results_ui,
  `27` = one_group_rm_anova_results_ui, `28` = mixed_rm_anova_results_ui,
  `29` = ttest_anova_results_ui, `30` = nonparametric_paired_results_ui,
  `31` = function(r) regression_results_panel(r, show_vif = TRUE, show_sr2 = TRUE, show_f2 = TRUE),
  `32` = function(r) hierarchical_results_panel(r, show_vif = TRUE, show_sr2 = TRUE, show_f2 = TRUE),
  `33` = logistic_results_panel, `34` = factor_analysis_results_ui,
  `35` = pca_results_ui, `36` = mediation_moderation_result_ui,
  `37` = generalized_results_panel, `38` = longitudinal_results_panel,
  `39` = function(r) if (r$type %in% c("km", "km_multi")) survival_km_results_panel(r, survival_saved_km_plot_ids(r), language = "ko") else if (r$type == "cox") survival_cox_results_panel(r, language = "ko") else survival_competing_results_panel(r, language = "ko"),
  `40` = interrater_agreement_results_ui
)
families <- c("frequencies", "reliability", "correlation", "crosstabs", "ttest-anova", "ancova", "paired", "paired-rm", "one-group-rm", "mixed-rm", "nonparametric", "nonparametric-paired", "regression", "hierarchical", "logistic", "efa", "pca", "mediation", "generalized", "longitudinal", "survival", "interrater")
names(families) <- names(renderers)
audit <- list(); failures <- list(); entries <- list(); note_audit <- list()
canonical <- function(x) gsub("[[:space:];.]", "", x, perl = TRUE)
audit_html <- function(html, family, case) {
  id <- paste(family, case, sep = "-")
  doc <- xml2::read_html(html)
  entry <- list(id = id, title = id, html = html)
  tables <- result_entry_tables(entry)
  notes <- xml2::xml_find_all(doc, ".//*[self::p or self::div][contains(@class,'note') and not(descendant::table)]")
  notes <- notes[!vapply(notes, function(n) length(xml2::xml_find_all(n, ".//*[self::p or self::div][contains(@class,'note')]")) > 0L, logical(1))]
  for (i in seq_along(notes)) {
    value <- result_html_text(notes[[i]])
    class <- xml2::xml_attr(notes[[i]], "class")
    # Model labels are not statistical table notes.
    if (grepl("hierarchical-model-note", class, fixed = TRUE) || !nzchar(value)) next
    normalized <- result_publication_note(value)
    note_audit[[length(note_audit) + 1L]] <<- data.frame(family, case, note = i, class, text = value, normalized, valid = identical(canonical(value), canonical(normalized)))
  }
  for (i in seq_along(tables)) {
    table <- tables[[i]]
    audit[[length(audit) + 1L]] <<- data.frame(family, case, table = i, title = table$title %||% "", orientation = table$orientation %||% "", notes = paste(table$after_text %||% table$notes, collapse = " | "), cells = length(table$screen$cells))
  }
  entries[[id]] <<- entry
  writeLines(html, file.path(out, "rendered", paste0(id, ".html")), useBytes = TRUE)
}
for (phase in names(renderers)) {
  folders <- list.dirs("outputs", recursive = FALSE, full.names = TRUE)
  folder <- folders[grepl(paste0("^spss_phase", phase, "_"), basename(folders))][1]
  files <- list.files(folder, pattern = "^analysis[.]rds$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) {
    cases <- if (phase == "19") {
      fixture <- data.frame(group = factor(rep(c("A", "B"), 20)), score = seq_len(40), value = seq_len(40) / 3)
      info <- data.frame(name = names(fixture), measurement = c("category", "continuous", "continuous"))
      list(mixed = prepare_frequencies_results(fixture, names(fixture), variable_info = info),
           categorical = prepare_frequencies_results(fixture, "group", variable_info = info),
           continuous = prepare_frequencies_results(fixture, c("score", "value"), variable_info = info))
    } else {
      candidates <- list.files("scripts", pattern = "_four_formats[.]R$", full.names = TRUE)
      setup_file <- candidates[vapply(candidates, function(path) any(grepl(paste0("spss_phase", phase, "_"), readLines(path, warn = FALSE), fixed = TRUE)), logical(1))][1]
      if (is.na(setup_file)) stop("Missing fixtures for ", families[[phase]])
      setup <- new.env(parent = .GlobalEnv)
      for (expression in parse(setup_file, encoding = "UTF-8")) {
        if (is.call(expression) && identical(expression[[1]], as.name("for")) && identical(expression[[2]], as.name("name"))) break
        head <- if (is.call(expression)) as.character(expression[[1]]) else ""
        if (head %in% c("source", "load_app_packages", "source_app_modules", "options", "Sys.setlocale", "dir.create")) next
        eval(expression, setup)
      }
      setup$cases
    }
    for (case in names(cases)) audit_html(as.character(htmltools::renderTags(renderers[[phase]](cases[[case]]))$html), families[[phase]], case)
    message(families[[phase]], ": ", length(cases), " generated cases inspected")
    next
  }
  for (file in files) {
    case <- basename(dirname(file))
    tryCatch({
      result <- readRDS(file)
      html <- as.character(htmltools::renderTags(renderers[[phase]](result))$html)
      audit_html(html, families[[phase]], case)
    }, error = function(e) failures[[length(failures) + 1L]] <<- data.frame(family = families[[phase]], case, error = conditionMessage(e)))
  }
  message(families[[phase]], ": ", length(files), " cases inspected")
}
saveRDS(entries, file.path(out, "entries.rds"))
write.csv(do.call(rbind, audit), file.path(out, "tables.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(do.call(rbind, note_audit), file.path(out, "notes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (length(failures)) write.csv(do.call(rbind, failures), file.path(out, "render-errors.csv"), row.names = FALSE, fileEncoding = "UTF-8")
message("Audited ", length(entries), " cases, ", length(audit), " tables, ", length(note_audit), " notes; rendering failures: ", length(failures))
stopifnot(length(failures) == 0L, all(do.call(rbind, note_audit)$valid),
          setequal(unique(do.call(rbind, audit)$family), unname(families)))
