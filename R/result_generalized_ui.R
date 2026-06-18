# Generalized linear model result UI.

generalized_display_coef_table <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  variables <- unique(c(result$outcome, result$exposure, result$predictors))
  fallback_labels <- stats::setNames(
    vapply(variables, display_variable_name_static, character(1), table = variable_table, labels = labels, label_only = TRUE),
    variables
  )
  value_labels <- category_value_label_lookup_static(category_table)
  output <- data.frame(
    Term = vapply(
      table$Term,
      display_term_name_with_variables_static,
      character(1),
      variable_names = variables,
      labels = labels,
      value_labels = value_labels,
      fallback_labels = fallback_labels
    ),
    B = vapply(table$B, longitudinal_format_number, character(1)),
    SE = vapply(table$SE, longitudinal_format_number, character(1)),
    Statistic = vapply(table$Statistic, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    LLCI = vapply(table$LLCI, longitudinal_format_number, character(1)),
    ULCI = vapply(table$ULCI, longitudinal_format_number, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (isTRUE(result$exponentiate) && all(c("exp(B)", "exp(LLCI)", "exp(ULCI)") %in% names(table))) {
    output$`exp(B)` <- vapply(table$`exp(B)`, longitudinal_format_number, character(1))
    output$`exp(LLCI)` <- vapply(table$`exp(LLCI)`, longitudinal_format_number, character(1))
    output$`exp(ULCI)` <- vapply(table$`exp(ULCI)`, longitudinal_format_number, character(1))
  }
  output
}

generalized_display_assumption_table <- function(result) {
  table <- result$assumption_checks
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Check = as.character(table$Check),
    Result = as.character(table$Result),
    Statistic = vapply(table$Statistic, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    Interpretation = as.character(table$Interpretation),
    Recommendation = as.character(table$Recommendation),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_missing_table <- function(result) {
  table <- result$missing_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Variable = as.character(table$Variable),
    Missing = as.integer(table$Missing),
    `Missing %` = vapply(table$`Missing %`, longitudinal_format_number, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_count_details <- function(result) {
  table <- result$count_details
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_missing_details <- function(result) {
  table <- result$missing_details
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_decision_summary <- function(result) {
  table <- result$decision_summary
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_coding_summary <- function(result) {
  table <- result$coding_summary
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Variable = as.character(table$Variable),
    Role = as.character(table$Role),
    Measurement = as.character(table$Measurement),
    Coding = as.character(table$Coding),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_display_vif_table <- function(result) {
  table <- result$vif_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = as.character(table$Term),
    VIF = vapply(table$VIF, longitudinal_format_number, character(1)),
    Tolerance = vapply(table$Tolerance, longitudinal_format_number, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_result_block <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  coef_table <- generalized_display_coef_table(result, variable_table, labels, category_table)
  missing_table <- generalized_display_missing_table(result)
  missing_details <- generalized_display_missing_details(result)
  decision_summary <- generalized_display_decision_summary(result)
  coding_summary <- generalized_display_coding_summary(result)
  count_details <- generalized_display_count_details(result)
  assumption_table <- generalized_display_assumption_table(result)
  vif_table <- generalized_display_vif_table(result)
  software_versions <- result$software_versions
  if (!is.data.frame(software_versions)) software_versions <- data.frame()
  publication_notes <- result$publication_notes
  if (!is.data.frame(publication_notes)) publication_notes <- data.frame()
  reporting_checklist <- result$reporting_checklist
  if (!is.data.frame(reporting_checklist)) reporting_checklist <- data.frame()
  manuscript_text <- result$manuscript_text
  if (!is.data.frame(manuscript_text)) manuscript_text <- data.frame()
  notes <- as.character(result$notes %||% character(0))
  notes <- notes[nzchar(notes)]
  outcome <- display_variable_name_static(result$outcome, variable_table, labels, label_only = TRUE)
  missing_summary <- switch(
    as.character(result$missing_strategy %||% "complete")[[1]],
    mi = sprintf(
      "%s; analyzed rows after imputation: %s of %s; complete-case rows before MI: %s of %s.",
      result$missing_method %||% "Multiple imputation",
      as.character(result$n %||% ""),
      as.character(result$raw_n %||% ""),
      as.character(result$complete_case_n %||% ""),
      as.character(result$raw_n %||% "")
    ),
    ipw = sprintf(
      "%s; weighted complete-case rows: %s of %s.",
      result$missing_method %||% "Inverse probability weighting",
      as.character(result$n %||% result$complete_case_n %||% ""),
      as.character(result$raw_n %||% "")
    ),
    sprintf(
      "%s; complete cases used: %s of %s.",
      result$missing_method %||% "Complete-case",
      as.character(result$complete_case_n %||% result$n %||% ""),
      as.character(result$raw_n %||% result$n %||% "")
    )
  )

  div(
    class = "result-section regression-result-panel generalized-result-panel",
    h3(sprintf("%s: %s", result$method, outcome)),
    if (nzchar(result$model_rationale %||% "")) {
      div(result$model_rationale, class = "result-note coefficient-warning")
    },
    if (nrow(decision_summary) > 0) {
      tagList(
        h4("Model decision summary"),
        longitudinal_coef_html_table(decision_summary)
      )
    },
    if (nrow(coding_summary) > 0) {
      tagList(
        h4("Variable coding"),
        longitudinal_coef_html_table(coding_summary)
      )
    },
    h4("Model fit"),
    longitudinal_coef_html_table(result$fit_stats),
    h4("Missing data"),
    div(missing_summary, class = "result-note"),
    if (nrow(missing_details) > 0) {
      longitudinal_coef_html_table(missing_details)
    },
    if (nrow(missing_table) > 0) {
      longitudinal_coef_html_table(missing_table)
    },
    if (nrow(count_details) > 0) {
      tagList(
        h4("Count-family screening"),
        longitudinal_coef_html_table(count_details)
      )
    },
    h4("Coefficients"),
    longitudinal_coef_html_table(coef_table),
    if (nrow(assumption_table) > 0) {
      tagList(
        h4("Assumption checks"),
        longitudinal_coef_html_table(assumption_table)
      )
    },
    if (nrow(vif_table) > 0) {
      tagList(
        h4("Collinearity diagnostics"),
        longitudinal_coef_html_table(vif_table)
      )
    },
    if (nrow(publication_notes) > 0) {
      tagList(
        h4("Publication table notes"),
        tags$ol(class = "longitudinal-publication-notes", lapply(publication_notes$Note, tags$li))
      )
    },
    if (nrow(reporting_checklist) > 0) {
      tagList(
        h4("SCI reporting checklist"),
        longitudinal_coef_html_table(reporting_checklist)
      )
    },
    if (nrow(manuscript_text) > 0) {
      tagList(
        h4("Suggested manuscript text"),
        longitudinal_coef_html_table(manuscript_text)
      )
    },
    if (nrow(software_versions) > 0) {
      tagList(
        h4("Software versions"),
        longitudinal_coef_html_table(software_versions)
      )
    },
    if (length(notes) > 0) {
      tagList(
        h4("Notes"),
        tags$ul(lapply(notes, tags$li))
      )
    }
  )
}

generalized_results_panel <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  if (is.null(result)) return(NULL)
  div(
    class = "regression-results generalized-results",
    generalized_result_block(result, variable_table, labels, category_table)
  )
}

saved_generalized_results_html <- function(
  result,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  css_path = file.path("www", "style.css"),
  report_mode = FALSE
) {
  content <- div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Generalized Linear Model (GLM)"),
      div("Gaussian, logistic, gamma, Poisson, and negative-binomial generalized linear model results.", class = "app-subtitle")
    ),
    generalized_results_panel(result, variable_table, labels, category_table)
  )
  saved_results_document(
    title = "Generalized Linear Model (GLM)",
    content = content,
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

write_generalized_results_html <- function(
  result,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL
) {
  writeLines(
    saved_generalized_results_html(result, variable_table, labels, category_table),
    file,
    useBytes = TRUE
  )
  invisible(file)
}

write_generalized_results_pdf <- function(
  result,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL
) {
  write_pdf_from_html(
    saved_generalized_results_html(result, variable_table, labels, category_table, report_mode = TRUE),
    file
  )
}

save_generalized_excel_file <- function(
  result,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL
) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)

  add_table <- function(sheet, table, title = sheet) {
    if (!is.data.frame(table) || nrow(table) == 0) return(invisible(FALSE))
    used_sheets <<- add_excel_table_sheet(workbook, sheet, table, used_sheets, title = title)
    invisible(TRUE)
  }

  add_table("Decision summary", generalized_display_decision_summary(result), "Model decision summary")
  add_table("Variable coding", generalized_display_coding_summary(result), "Variable coding")
  add_table("Model fit", result$fit_stats, "Model fit")
  add_table("Missing details", generalized_display_missing_details(result), "Missing data handling")
  add_table("Missing values", generalized_display_missing_table(result), "Missing values")
  add_table("Count screening", generalized_display_count_details(result), "Count-family screening")
  add_table(
    "Coefficients",
    generalized_display_coef_table(result, variable_table, labels, category_table),
    "Coefficients"
  )
  add_table("Assumptions", generalized_display_assumption_table(result), "Assumption checks")
  add_table("Collinearity", generalized_display_vif_table(result), "Collinearity diagnostics")
  add_table("Table notes", result$publication_notes, "Publication table notes")
  add_table("SCI checklist", result$reporting_checklist, "SCI reporting checklist")
  add_table("Manuscript text", result$manuscript_text, "Suggested manuscript text")
  add_table("Software", result$software_versions, "Software versions")

  notes <- as.character(result$notes %||% character(0))
  notes <- notes[nzchar(notes)]
  if (length(notes) > 0) {
    add_table("Notes", data.frame(Note = notes, stringsAsFactors = FALSE, check.names = FALSE), "Notes")
  }

  if (length(used_sheets) == 0) {
    used_sheets <- add_excel_table_sheet(
      workbook,
      "Results",
      data.frame(Message = "No GLM result tables are available.", stringsAsFactors = FALSE),
      used_sheets,
      title = "Generalized Linear Model (GLM)"
    )
  }

  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}
