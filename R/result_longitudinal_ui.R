# Longitudinal / panel model result UI and export helpers.

longitudinal_format_number <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0 || is.na(value)) return("")
  if (!is.finite(value)) return(as.character(value))
  format_decimal3(value)
}

longitudinal_display_coef_table <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  output <- data.frame(
    Term = as.character(table$Term),
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

longitudinal_publication_estimate_table <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  estimate_ci <- sprintf(
    "%s (%s to %s)",
    vapply(table$B, longitudinal_format_number, character(1)),
    vapply(table$LLCI, longitudinal_format_number, character(1)),
    vapply(table$ULCI, longitudinal_format_number, character(1))
  )
  direction <- ifelse(
    suppressWarnings(as.numeric(table$B)) > 0,
    "Positive",
    ifelse(suppressWarnings(as.numeric(table$B)) < 0, "Negative", "Neutral")
  )
  output <- data.frame(
    Term = as.character(table$Term),
    `B (95% CI)` = estimate_ci,
    SE = vapply(table$SE, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    Direction = direction,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (isTRUE(result$exponentiate) && all(c("exp(B)", "exp(LLCI)", "exp(ULCI)") %in% names(table))) {
    output$`exp(B) (95% CI)` <- sprintf(
      "%s (%s to %s)",
      vapply(table$`exp(B)`, longitudinal_format_number, character(1)),
      vapply(table$`exp(LLCI)`, longitudinal_format_number, character(1)),
      vapply(table$`exp(ULCI)`, longitudinal_format_number, character(1))
    )
  }
  output
}

longitudinal_result_title <- function(result, variable_table = NULL, labels = character(0)) {
  outcome <- display_variable_name_static(result$outcome, variable_table, labels, label_only = TRUE)
  sprintf("%s: %s", result$method, outcome)
}

longitudinal_display_assumption_table <- function(result) {
  table <- result$assumption_checks
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  output <- data.frame(
    Check = as.character(table$Check),
    Result = as.character(table$Result),
    Statistic = vapply(table$Statistic, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    Interpretation = as.character(table$Interpretation),
    Recommendation = as.character(table$Recommendation),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  output
}

longitudinal_display_missing_table <- function(result) {
  table <- result$missing_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  data.frame(
    Variable = as.character(table$Variable),
    Missing = as.character(table$Missing),
    `Missing %` = vapply(table$MissingPercent, longitudinal_format_number, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_display_missing_sensitivity_table <- function(result) {
  table <- result$missing_sensitivity_results
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  output <- data.frame(
    Strategy = as.character(table$Strategy),
    Term = as.character(table$Term),
    B = vapply(table$B, longitudinal_format_number, character(1)),
    SE = vapply(table$SE, longitudinal_format_number, character(1)),
    Statistic = vapply(table$Statistic, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    LLCI = vapply(table$LLCI, longitudinal_format_number, character(1)),
    ULCI = vapply(table$ULCI, longitudinal_format_number, character(1)),
    Status = as.character(table$Status),
    Note = as.character(table$Note),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (all(c("exp(B)", "exp(LLCI)", "exp(ULCI)") %in% names(table))) {
    output$`exp(B)` <- vapply(table$`exp(B)`, longitudinal_format_number, character(1))
    output$`exp(LLCI)` <- vapply(table$`exp(LLCI)`, longitudinal_format_number, character(1))
    output$`exp(ULCI)` <- vapply(table$`exp(ULCI)`, longitudinal_format_number, character(1))
  }
  output
}

longitudinal_display_weight_summary_table <- function(result) {
  table <- result$weight_summary
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_model_overview_table <- function(results, variable_table = NULL, labels = character(0)) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  rows <- c("N", "Clusters", "Time points", "Outcome", "ID", "Time", "Exposure / offset", "Weight", "Method", "Requested family", "Fitted family", "Formula", "AIC", "BIC")
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(results)) {
    result <- results[[index]]
    outcome <- display_variable_name_static(result$outcome, variable_table, labels, label_only = TRUE)
    id <- display_variable_name_static(result$id, variable_table, labels, label_only = TRUE)
    time <- display_variable_name_static(result$time, variable_table, labels, label_only = TRUE)
    offset <- display_variable_name_static(result$offset_variable %||% character(0), variable_table, labels, label_only = TRUE)
    column <- outcome
    if (column %in% names(table)) {
      column <- paste(column, index)
    }
    table[[column]] <- c(
      as.character(result$n),
      as.character(result$clusters),
      as.character(result$time_points),
      outcome,
      id,
      time,
      offset,
      paste(as.character(result$weight %||% character(0)), collapse = ", "),
      result$method,
      result$requested_family %||% result$family,
      result$family,
      paste(deparse(result$formula), collapse = " "),
      longitudinal_format_number(result$aic),
      longitudinal_format_number(result$bic)
    )
  }
  table
}

longitudinal_assumption_review_table <- function(results) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  rows <- c("Estimand", "Correlation / random effect", "Package")
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(results)) {
    result <- results[[index]]
    estimand <- if (identical(result$model_type, "gee")) {
      "Population-averaged"
    } else if (result$model_type %in% c("lmm", "glmm")) {
      "Subject-specific"
    } else {
      "Panel unit effect"
    }
    structure <- if (identical(result$model_type, "gee")) {
      sprintf("Working correlation: %s", result$corstr)
    } else if (result$model_type %in% c("lmm", "glmm")) {
      id_label <- display_variable_name_static(result$id, NULL, character(0), label_only = TRUE)
      time_label <- display_variable_name_static(result$time, NULL, character(0), label_only = TRUE)
      if (isTRUE(result$random_slope)) {
        sprintf("Random intercept by %s; random slope for %s", id_label, time_label)
      } else {
        sprintf("Random intercept by %s", id_label)
      }
    } else {
      if (identical(result$model_type, "panel_fe")) "Fixed effects" else "Random effects"
    }
    package <- longitudinal_required_package(result$model_type)
    table[[sprintf("Model %s", index)]] <- c(
      estimand,
      structure,
      if (nzchar(package)) package_version_label(package) else ""
    )
  }
  table
}

longitudinal_coef_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(div(class = "empty-message", "No coefficient table was returned."))
  }
  tags$table(
    class = "coefficient-table longitudinal-coefficient-table",
    style = paste0(result_table_style(font_size = 12, min_width = 0), "width:100% !important;table-layout:fixed;"),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = result_header_cell_style(first = index == 1L), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(table), function(col_index) {
        tags$td(style = result_body_cell_style(first = col_index == 1L, last = row_index == nrow(table)), as.character(table[[col_index]][[row_index]] %||% ""))
      }))
    }))
  )
}

longitudinal_result_block <- function(result, variable_table = NULL, labels = character(0)) {
  assumption_table <- longitudinal_display_assumption_table(result)
  publication_table <- longitudinal_publication_estimate_table(result)
  recommendations <- as.character(result$recommendations %||% character(0))
  recommendations <- recommendations[nzchar(recommendations)]
  sensitivity <- as.character(result$sensitivity_recommendations %||% character(0))
  sensitivity <- sensitivity[nzchar(sensitivity)]
  div(
    class = "result-section regression-result-panel longitudinal-result-panel",
    h3(longitudinal_result_title(result, variable_table, labels)),
    if (nzchar(result$model_rationale %||% "")) {
      tagList(
        h4("Model rationale"),
        div(result$model_rationale, class = "result-note coefficient-warning")
      )
    },
    if (is.data.frame(result$data_structure) && nrow(result$data_structure) > 0) {
      tagList(
        h4("Data structure"),
        model_overview_html_table(result$data_structure)
      )
    },
    if (is.data.frame(result$weight_summary) && nrow(result$weight_summary) > 0) {
      tagList(
        h4("Analysis weights"),
        model_overview_html_table(longitudinal_display_weight_summary_table(result))
      )
    },
    if (is.data.frame(result$missing_table) && nrow(result$missing_table) > 0) {
      tagList(
        h4("Missing data"),
        model_overview_html_table(longitudinal_display_missing_table(result))
      )
    },
    if (is.data.frame(result$missing_sensitivity_results) && nrow(result$missing_sensitivity_results) > 0) {
      tagList(
        h4("Missing-data sensitivity results"),
        model_overview_html_table(longitudinal_display_missing_sensitivity_table(result))
      )
    },
    if (is.data.frame(publication_table) && nrow(publication_table) > 0) {
      tagList(
        h4("Publication-ready estimates"),
        model_overview_html_table(publication_table),
        if (is.data.frame(result$publication_notes) && nrow(result$publication_notes) > 0) {
          tagList(
            h4("Publication table notes"),
            tags$ol(class = "longitudinal-publication-notes", lapply(result$publication_notes$Note, tags$li))
          )
        }
      )
    },
    h4("Detailed coefficient table"),
    longitudinal_coef_html_table(longitudinal_display_coef_table(result)),
    if (is.data.frame(result$fit_details) && nrow(result$fit_details) > 0) {
      tagList(
        h4("Model fit details"),
        model_overview_html_table(result$fit_details)
      )
    },
    if (is.data.frame(assumption_table) && nrow(assumption_table) > 0) {
      tagList(
        h4("Assumption checks"),
        model_overview_html_table(assumption_table)
      )
    },
    if (length(recommendations) > 0) {
      tagList(
        h4("Recommended analysis"),
        tags$ul(class = "longitudinal-recommendation-list", lapply(recommendations, tags$li))
      )
    },
    if (length(sensitivity) > 0) {
      tagList(
        h4("Sensitivity analysis suggestions"),
        tags$ul(class = "longitudinal-recommendation-list", lapply(sensitivity, tags$li))
      )
    },
    if (is.data.frame(result$sensitivity_results) && nrow(result$sensitivity_results) > 0) {
      tagList(
        h4("Sensitivity analysis results"),
        model_overview_html_table(result$sensitivity_results)
      )
    },
    if (is.data.frame(result$manuscript_text) && nrow(result$manuscript_text) > 0) {
      tagList(
        h4("Suggested manuscript text"),
        model_overview_html_table(result$manuscript_text)
      )
    },
    if (is.data.frame(result$reporting_checklist) && nrow(result$reporting_checklist) > 0) {
      tagList(
        h4("SCI reporting checklist"),
        model_overview_html_table(result$reporting_checklist)
      )
    },
    if (is.data.frame(result$software_versions) && nrow(result$software_versions) > 0) {
      tagList(
        h4("Software versions"),
        model_overview_html_table(result$software_versions)
      )
    },
    lapply(result$notes %||% character(0), function(note) div(note, class = "result-note coefficient-warning"))
  )
}

longitudinal_results_panel <- function(results, variable_table = NULL, labels = character(0)) {
  warnings <- attr(results, "warnings")
  skipped <- attr(results, "skipped")
  div(
    class = "regression-results longitudinal-results",
    if (is.list(results) && length(results) > 0) {
      div(
        class = "regression-result-panel model-overview-panel",
        h3("Model overview"),
        model_overview_html_table(longitudinal_model_overview_table(results, variable_table, labels))
      )
    },
    lapply(results, longitudinal_result_block, variable_table = variable_table, labels = labels),
    if (is.list(results) && length(results) > 0) {
      div(
        class = "regression-result-panel assumption-review-panel",
        h3("Model interpretation guide"),
        model_overview_html_table(longitudinal_assumption_review_table(results))
      )
    },
    analysis_diagnostics_section(warnings, skipped, title = "Warnings / skipped models", class = "regression-result-panel longitudinal-diagnostics-panel")
  )
}

saved_longitudinal_results_html <- function(results, variable_table = NULL, labels = character(0), css_path = file.path("www", "style.css"), report_mode = FALSE) {
  content <- div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Longitudinal / Panel Models"),
      div("Longitudinal, clustered, and panel model results.", class = "app-subtitle")
    ),
    longitudinal_results_panel(results, variable_table, labels)
  )
  saved_results_document(
    title = "Longitudinal / Panel Models",
    content = content,
    css_path = css_path,
    report_mode = report_mode
  )
}

write_longitudinal_results_html <- function(results, file, variable_table = NULL, labels = character(0)) {
  writeLines(saved_longitudinal_results_html(results, variable_table, labels), file, useBytes = TRUE)
  invisible(file)
}

write_longitudinal_results_pdf <- function(results, file, variable_table = NULL, labels = character(0)) {
  write_pdf_from_html(saved_longitudinal_results_html(results, variable_table, labels, report_mode = TRUE), file)
}

save_longitudinal_excel_file <- function(results, file, variable_table = NULL, labels = character(0)) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  overview <- longitudinal_model_overview_table(results, variable_table, labels)
  if (is.data.frame(overview) && nrow(overview) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Model overview", overview, used_sheets, title = "Model overview")
  }
  for (index in seq_along(results)) {
    result <- results[[index]]
    table <- longitudinal_display_coef_table(result)
    publication_table <- longitudinal_publication_estimate_table(result)
    title <- longitudinal_result_title(result, variable_table, labels)
    if (is.data.frame(result$data_structure) && nrow(result$data_structure) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Data structure %s", index), result$data_structure, used_sheets, title = sprintf("Data structure - Model %s", index))
    }
    weight_summary <- longitudinal_display_weight_summary_table(result)
    if (is.data.frame(weight_summary) && nrow(weight_summary) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Weights %s", index), weight_summary, used_sheets, title = sprintf("Analysis weights - Model %s", index))
    }
    missing <- longitudinal_display_missing_table(result)
    if (is.data.frame(missing) && nrow(missing) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Missing data %s", index), missing, used_sheets, title = sprintf("Missing data - Model %s", index))
    }
    missing_sensitivity <- longitudinal_display_missing_sensitivity_table(result)
    if (is.data.frame(missing_sensitivity) && nrow(missing_sensitivity) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Missing sensitivity %s", index), missing_sensitivity, used_sheets, title = sprintf("Missing-data sensitivity results - Model %s", index))
    }
    if (is.data.frame(publication_table) && nrow(publication_table) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Publication table %s", index), publication_table, used_sheets, title = sprintf("Publication-ready estimates - Model %s", index))
    }
    if (is.data.frame(result$publication_notes) && nrow(result$publication_notes) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Table notes %s", index), result$publication_notes, used_sheets, title = sprintf("Publication table notes - Model %s", index))
    }
    used_sheets <- add_excel_table_sheet(workbook, sprintf("Model %s", index), table, used_sheets, title = title)
    if (is.data.frame(result$fit_details) && nrow(result$fit_details) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Fit details %s", index), result$fit_details, used_sheets, title = sprintf("Model fit details - Model %s", index))
    }
    assumption <- longitudinal_display_assumption_table(result)
    if (is.data.frame(assumption) && nrow(assumption) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Assumptions %s", index), assumption, used_sheets, title = sprintf("Assumption checks - Model %s", index))
    }
    recommendations <- as.character(result$recommendations %||% character(0))
    recommendations <- recommendations[nzchar(recommendations)]
    if (length(recommendations) > 0) {
      recommendation_table <- data.frame(Recommendation = recommendations, stringsAsFactors = FALSE, check.names = FALSE)
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Recommendations %s", index), recommendation_table, used_sheets, title = sprintf("Recommended analysis - Model %s", index))
    }
    sensitivity <- as.character(result$sensitivity_recommendations %||% character(0))
    sensitivity <- sensitivity[nzchar(sensitivity)]
    if (length(sensitivity) > 0) {
      sensitivity_table <- data.frame(Sensitivity = sensitivity, stringsAsFactors = FALSE, check.names = FALSE)
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Sensitivity %s", index), sensitivity_table, used_sheets, title = sprintf("Sensitivity analysis suggestions - Model %s", index))
    }
    if (is.data.frame(result$sensitivity_results) && nrow(result$sensitivity_results) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Sensitivity results %s", index), result$sensitivity_results, used_sheets, title = sprintf("Sensitivity analysis results - Model %s", index))
    }
    if (is.data.frame(result$manuscript_text) && nrow(result$manuscript_text) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Manuscript text %s", index), result$manuscript_text, used_sheets, title = sprintf("Suggested manuscript text - Model %s", index))
    }
    if (is.data.frame(result$reporting_checklist) && nrow(result$reporting_checklist) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("SCI checklist %s", index), result$reporting_checklist, used_sheets, title = sprintf("SCI reporting checklist - Model %s", index))
    }
    if (is.data.frame(result$software_versions) && nrow(result$software_versions) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, sprintf("Software %s", index), result$software_versions, used_sheets, title = sprintf("Software versions - Model %s", index))
    }
  }
  assumption <- longitudinal_assumption_review_table(results)
  if (is.data.frame(assumption) && nrow(assumption) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Model guide", assumption, used_sheets, title = "Model interpretation guide")
  }
  used_sheets <- add_analysis_warning_skipped_sheets(workbook, used_sheets, attr(results, "warnings"), attr(results, "skipped"), skipped_title = "Skipped models")
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}
