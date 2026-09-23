# Within-subject treatment repeated-measures ANOVA result UI.

one_group_rm_anova_results_ui <- function(result) {
  if (is.null(result)) return(NULL)
  if (is.list(result) && !is.null(result$error)) return(empty_message(result$error))
  keys <- attr(result$anova, "one_group_effect_keys", exact = TRUE)
  if (is.data.frame(result$anova) && length(keys) == nrow(result$anova)) {
    covariate_labels <- attr(result$anova, "one_group_covariate_labels", exact = TRUE)
    language <- result_appendix_table_language()
    attr(result$anova, "result_appendix_effects") <- vapply(strsplit(keys, ":", fixed = TRUE), function(parts) {
      paste(vapply(parts, function(part) {
        if (part %in% names(covariate_labels)) return(unname(covariate_labels[[part]]))
        if (part %in% c("Treatment", "Time")) return(statedu_t(paste0("analysis.one_group.", tolower(part)), language))
        result_appendix_ui_text(part, language)
      }, character(1)), collapse = " x ")
    }, character(1))
  }
  if (is.data.frame(result$overview) && all(c("Item", "Value") %in% names(result$overview))) {
    language <- result_appendix_table_language()
    tr <- function(key) statedu_t(paste0("analysis.one_group.", key), language)
    overview <- result$overview
    fields <- attr(overview, "one_group_input_fields", exact = TRUE)
    if (isTRUE(result$input_format %in% c("wide", "long"))) overview$Value[overview$Item == "Input format"] <- tr(paste0(result$input_format, "_format"))
    if (length(fields) == 4L) overview$Value[overview$Item == "Input fields"] <- paste(paste0(vapply(c("subject_id", "treatment_group", "time", "outcome"), tr, character(1)), ": ", fields), collapse = "; ")
    overview$Value[overview$Item == "Independent variables"] <- tr("within_factors")
    count <- overview$Value[overview$Item == "Complete N"]
    if (length(count) == 1L && length(result$treatment_labels) > 0L) overview$Value[overview$Item == "Groups"] <- paste(sprintf(tr("paired_group"), result$treatment_labels, count), collapse = ", ")
    rows <- which(overview$Item %in% c("Input format", "Input fields", "Independent variables", "Groups"))
    attr(overview, "result_user_cells") <- cbind(rows, rep(match("Value", names(overview)), length(rows)))
    result$overview <- overview
  }
  if (is.data.frame(result$recommendation) && "Reason" %in% names(result$recommendation)) {
    result$recommendation$Reason <- vapply(as.character(result$recommendation$Reason), function(text) {
      count <- regmatches(text, regexec("^Excluded subjects: ([0-9]+)\\.$", text))[[1L]]
      if (length(count) == 2L) sprintf(statedu_t("analysis.one_group.excluded_subjects", result_appendix_table_language()), count[[2L]]) else text
    }, character(1))
  }
  if (is.data.frame(result$normality) && ncol(result$normality) > 1L) {
    attr(result$normality, "result_user_columns") <- 1L
    attr(result$normality, "result_user_headers") <- seq.int(2L, ncol(result$normality))
  }
  if (is.data.frame(result$assumption) && all(c("Item", "Detail") %in% names(result$assumption))) {
    rows <- which(result$assumption$Item %in% c("Treatment levels", "Time points"))
    attr(result$assumption, "result_user_cells") <- cbind(rows, rep(match("Detail", names(result$assumption)), length(rows)))
  }
  tags$div(
    class = "one-group-rm-anova-results",
    mixed_rm_anova_results_ui(result)
  )
}

saved_one_group_rm_anova_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "StatEdu Studio Within-subject Treatment Repeated-measures ANOVA Results",
    tags$div(class = "regression-results", one_group_rm_anova_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

write_one_group_rm_anova_results_html <- function(result, file) {
  write_result_html_document(saved_one_group_rm_anova_results_html(result), file, useBytes = TRUE)
}

write_one_group_rm_anova_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_one_group_rm_anova_results_html(result, report_mode = TRUE), file)
}
