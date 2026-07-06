# Generalized linear model result UI.

generalized_exponentiated_estimate_label <- function(result) {
  switch(
    as.character(result$family %||% "")[[1]],
    binomial = "OR",
    gamma = "Mean ratio",
    count = "RR",
    negative_binomial = "RR",
    "exp(B)"
  )
}

generalized_coefficient_display_input <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  exp_columns <- c("exp(B)", "exp(LLCI)", "exp(ULCI)")
  if (isTRUE(result$exponentiate) && all(exp_columns %in% names(table))) {
    estimate_label <- generalized_exponentiated_estimate_label(result)
    display <- data.frame(Term = as.character(table$Term), stringsAsFactors = FALSE, check.names = FALSE)
    display[[estimate_label]] <- table[["exp(B)"]]
    display$LLCI <- table[["exp(LLCI)"]]
    display$ULCI <- table[["exp(ULCI)"]]
    if ("p" %in% names(table)) display$p <- table$p
    return(display)
  }
  raw_columns <- if (as.character(result$family %||% "")[[1]] %in% c("binomial", "gamma", "count", "negative_binomial")) {
    c("Term", "B", "SE", "Statistic", "p")
  } else {
    c("Term", "B", "SE", "Statistic", "p", "LLCI", "ULCI")
  }
  keep <- intersect(raw_columns, names(table))
  table[, keep, drop = FALSE]
}

generalized_display_coef_table <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  table <- generalized_coefficient_display_input(result)
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  coefficient_output_table_with_context(
    table,
    predictors = as.character(result$predictors %||% character(0)),
    include_references = TRUE,
    variable_info = variable_table,
    refs = regression_reference_values_static(category_table),
    value_labels = category_value_label_lookup_static(category_table),
    labels = labels,
    category_table = category_table
  )
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

generalized_display_missing_pattern <- function(result) {
  table <- result$missing_pattern
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

generalized_fit_summary_lines <- function(result) {
  table <- result$fit_stats
  if (!is.data.frame(table) || nrow(table) == 0 || !all(c("Item", "Value") %in% names(table))) {
    return(character(0))
  }
  values <- stats::setNames(as.character(table$Value), as.character(table$Item))
  get_value <- function(name) {
    value <- trimws(named_value(values, name, ""))
    if (nzchar(value)) value else NA_character_
  }
  has_value <- function(value) {
    isTRUE(!is.na(value) && nzchar(value))
  }
  line <- function(...) {
    parts <- Filter(function(value) !is.na(value) && nzchar(value), c(...))
    if (length(parts) == 0) "" else paste(parts, collapse = "; ")
  }
  count_values <- if (is.data.frame(result$count_details) && nrow(result$count_details) > 0 && all(c("Item", "Value") %in% names(result$count_details))) {
    stats::setNames(as.character(result$count_details$Value), as.character(result$count_details$Item))
  } else {
    character(0)
  }
  count_value <- function(name) {
    value <- trimws(named_value(count_values, name, ""))
    if (nzchar(value)) value else NA_character_
  }
  aic_value <- get_value("AIC")
  bic_value <- get_value("BIC")
  loglik_value <- get_value("Log likelihood")
  dispersion_value <- get_value("Dispersion")
  residual_df_value <- get_value("Residual df")
  r2_value <- get_value("R2")
  adjusted_r2_value <- get_value("Adjusted R2")
  mcfadden_value <- get_value("McFadden pseudo R2")
  r2_parts <- if (has_value(r2_value)) {
    c(
      sprintf("R%s=%s", "\u00b2", r2_value),
      if (has_value(adjusted_r2_value)) sprintf("adjusted R%s=%s", "\u00b2", adjusted_r2_value) else NA_character_
    )
  } else if (has_value(mcfadden_value)) {
    sprintf("McFadden pseudo R%s=%s", "\u00b2", mcfadden_value)
  } else {
    character(0)
  }
  lines <- c(
    line(
      if (has_value(loglik_value)) sprintf("LogLik=%s", loglik_value) else NA_character_,
      if (has_value(aic_value)) sprintf("AIC=%s", aic_value) else NA_character_,
      if (has_value(bic_value)) sprintf("BIC=%s", bic_value) else NA_character_
    ),
    line(
      r2_parts,
      if (has_value(dispersion_value)) sprintf("dispersion=%s", dispersion_value) else NA_character_,
      if (has_value(residual_df_value)) sprintf("residual df=%s", residual_df_value) else NA_character_
    ),
    line(
      if (has_value(count_value("Poisson dispersion ratio"))) {
        sprintf("Poisson overdispersion screen: ratio=%s", count_value("Poisson dispersion ratio"))
      } else {
        NA_character_
      },
      if (has_value(count_value("Overdispersion threshold"))) {
        sprintf("threshold=%s", count_value("Overdispersion threshold"))
      } else {
        NA_character_
      },
      if (has_value(count_value("Selected family"))) {
        sprintf("selected=%s", count_value("Selected family"))
      } else {
        NA_character_
      }
    )
  )
  lines[nzchar(lines)]
}

generalized_coefficient_note <- function(result) {
  se_label <- generalized_se_type_label(result$se_type_used %||% "model")
  if (isTRUE(result$exponentiate)) {
    ratio_note <- if (as.character(result$family %||% "")[[1]] %in% c("count", "negative_binomial")) {
      " RR = rate ratio."
    } else {
      ""
    }
    sprintf("Note. Estimates and 95%% CIs are exponentiated coefficients.%s Standard errors were computed using %s.", ratio_note, se_label)
  } else {
    sprintf("Note. B, SE, and 95%% CIs are on the model link scale; standard errors were computed using %s.", se_label)
  }
}

generalized_display_coef_table_with_fit_rows <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  table <- generalized_display_coef_table(result, variable_table, labels, category_table)
  if (!is.data.frame(table) || nrow(table) == 0) return(table)
  footer_lines <- generalized_fit_summary_lines(result)
  if (length(footer_lines) == 0) return(table)
  footer <- as.data.frame(stats::setNames(as.list(rep("", ncol(table))), names(table)), stringsAsFactors = FALSE, check.names = FALSE)
  footer <- footer[rep(1L, length(footer_lines)), , drop = FALSE]
  footer$Term <- footer_lines
  rbind(table, footer)
}

generalized_coefficient_html_table <- function(table, result) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(div(class = "empty-message", "No coefficient table was returned."))
  }
  footer_lines <- generalized_fit_summary_lines(result)
  tags$table(
    class = "coefficient-table generalized-coefficient-table",
    style = paste0(result_table_style(font_size = 12, min_width = 0), "width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;"),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = result_header_cell_style(first = index == 1L), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(table), function(col_index) {
        tags$td(style = result_body_cell_style(first = col_index == 1L, last = length(footer_lines) == 0 && row_index == nrow(table)), as.character(table[[col_index]][[row_index]] %||% ""))
      }))
    })),
    if (length(footer_lines) > 0) {
      tags$tfoot(lapply(seq_along(footer_lines), function(index) {
        tags$tr(
          class = "coefficient-fit-row generalized-fit-row",
          tags$td(
            colspan = ncol(table),
            style = paste0(
              "padding:5px 10px;line-height:1.35;border-left:0;border-right:0;",
              "border-top:", if (index == 1L) "2px solid #1f2937" else "1px solid #d7dde5", ";",
              "border-bottom:", if (index == length(footer_lines)) "0" else "1px solid #d7dde5", ";",
              "text-align:center !important;white-space:normal;font-size:12px;"
            ),
            footer_lines[[index]]
          )
        )
      }))
    }
  )
}

generalized_html_table <- function(table, widths = NULL, align = NULL, font_size = 12) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(div(class = "empty-message", "No table was returned."))
  }
  columns <- names(table)
  column_count <- length(columns)
  if (is.null(widths) || length(widths) != column_count) {
    widths <- rep(sprintf("%.3f%%", 100 / column_count), column_count)
  }
  if (is.null(align) || length(align) != column_count) {
    align <- c("left", rep("right", max(0, column_count - 1L)))
  }
  cell_style <- function(index, last = FALSE, header = FALSE) {
    paste0(
      "padding:5px 10px;line-height:1.35;border-left:0;border-right:0;",
      "border-top:0;border-bottom:", if (isTRUE(header)) "2px solid #1f2937" else if (isTRUE(last)) "0" else "1px solid #d7dde5", ";",
      "vertical-align:middle;background:transparent;white-space:normal;",
      "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
      "overflow-wrap:break-word;word-break:normal;",
      "font-weight:", if (isTRUE(header)) "700" else "400", ";",
      "font-size:", if (isTRUE(header)) max(8, font_size - 1) else font_size, "px;",
      "width:", widths[[index]], " !important;",
      "min-width:0 !important;max-width:none !important;",
      "text-align:", align[[index]], " !important;"
    )
  }
  tags$table(
    class = "coefficient-table generalized-result-table",
    style = paste0(result_table_style(font_size = font_size, min_width = 0), "width:100% !important;min-width:0 !important;table-layout:fixed;"),
    tags$colgroup(lapply(widths, function(width) tags$col(style = paste0("width:", width, ";")))),
    tags$thead(tags$tr(lapply(seq_along(columns), function(index) {
      tags$th(style = cell_style(index, header = TRUE), columns[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(table), function(col_index) {
        tags$td(style = cell_style(col_index, last = row_index == nrow(table)), as.character(table[[col_index]][[row_index]] %||% ""))
      }))
    }))
  )
}

generalized_result_block <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  coef_table <- generalized_display_coef_table(result, variable_table, labels, category_table)
  missing_table <- generalized_display_missing_table(result)
  missing_details <- generalized_display_missing_details(result)
  missing_pattern <- generalized_display_missing_pattern(result)
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
    if (nrow(decision_summary) > 0) {
      tagList(
        h4("Model overview"),
        generalized_html_table(decision_summary, widths = c("34%", "66%"), align = c("left", "right"), font_size = 12)
      )
    },
    h4("Coefficients"),
    generalized_coefficient_html_table(coef_table, result),
    div(generalized_coefficient_note(result), class = "coefficient-note"),
    if (nrow(count_details) > 0) {
      tagList(
        h4("Count-family / overdispersion screening"),
        generalized_html_table(count_details, widths = c("36%", "64%"), align = c("left", "right"), font_size = 12)
      )
    },
    if (nrow(assumption_table) > 0) {
      tagList(
        h4("Assumption checks"),
        generalized_html_table(
          assumption_table,
          widths = c("17%", "9%", "9%", "7%", "29%", "29%"),
          align = c("left", "center", "right", "right", "left", "left"),
          font_size = 11
        )
      )
    },
    if (nrow(reporting_checklist) > 0) {
      tagList(
        h4("SCI reporting checklist"),
        generalized_html_table(
          reporting_checklist,
          widths = c("24%", "12%", "64%"),
          align = c("left", "center", "left"),
          font_size = 11
        )
      )
    },
    if (nrow(coding_summary) > 0) {
      tagList(
        h4("Variable coding"),
        generalized_html_table(
          coding_summary,
          widths = c("18%", "18%", "14%", "50%"),
          align = c("left", "center", "center", "left"),
          font_size = 11
        )
      )
    },
    if (nrow(vif_table) > 0) {
      tagList(
        h4("Collinearity diagnostics"),
        generalized_html_table(vif_table, widths = c("50%", "25%", "25%"), align = c("left", "right", "right"), font_size = 12)
      )
    },
    if (nrow(publication_notes) > 0) {
      tagList(
        h4("Publication table notes"),
        tags$ol(class = "longitudinal-publication-notes", lapply(publication_notes$Note, tags$li))
      )
    },
    h4("Missing data"),
    div(missing_summary, class = "result-note"),
    if (nrow(missing_details) > 0) {
      generalized_html_table(missing_details, widths = c("36%", "64%"), align = c("left", "right"), font_size = 12)
    },
    if (nrow(missing_pattern) > 0) {
      generalized_html_table(missing_pattern, widths = c("42%", "58%"), align = c("left", "right"), font_size = 12)
    },
    if (nrow(missing_table) > 0) {
      generalized_html_table(missing_table, widths = c("50%", "25%", "25%"), align = c("left", "right", "right"), font_size = 12)
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
    max_width = 688,
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

  add_table("Model overview", generalized_display_decision_summary(result), "Model overview")
  add_table(
    "Coefficients",
    generalized_display_coef_table_with_fit_rows(result, variable_table, labels, category_table),
    "Coefficients"
  )
  add_table("Count screening", generalized_display_count_details(result), "Count-family / overdispersion screening")
  add_table("Assumptions", generalized_display_assumption_table(result), "Assumption checks")
  add_table("SCI checklist", result$reporting_checklist, "SCI reporting checklist")
  add_table("Variable coding", generalized_display_coding_summary(result), "Variable coding")
  add_table("Collinearity", generalized_display_vif_table(result), "Collinearity diagnostics")
  add_table("Table notes", result$publication_notes, "Publication table notes")
  add_table("Missing details", generalized_display_missing_details(result), "Missing data handling")
  add_table("Missing pattern", generalized_display_missing_pattern(result), "Missing-data pattern")
  add_table("Missing values", generalized_display_missing_table(result), "Missing values")
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
