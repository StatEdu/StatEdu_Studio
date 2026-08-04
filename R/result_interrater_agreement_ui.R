# Inter-rater agreement result UI and exports.

interrater_agreement_note <- function(result) {
  result$method_note %||% ""
}

interrater_agreement_table <- function(result) {
  table <- result$overview
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  preferred <- c("Method", "Estimate", "95% CI", "N", "Raters", "Note")
  table[, intersect(preferred, names(table)), drop = FALSE]
}

interrater_primary_agreement_table <- function(result) {
  table <- result$primary %||% NULL
  if (!is.data.frame(table) || nrow(table) == 0) {
    table <- interrater_agreement_table(result)
    if (is.data.frame(table) && nrow(table) > 1L) {
      table <- table[1L, , drop = FALSE]
    }
  }
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  preferred <- c("Method", "Estimate", "95% CI", "N", "Raters", "Reason", "Note")
  table[, intersect(preferred, names(table)), drop = FALSE]
}

interrater_auxiliary_agreement_table <- function(result) {
  table <- result$auxiliary %||% NULL
  if (!is.data.frame(table)) {
    table <- interrater_agreement_table(result)
    primary_method <- result$primary$Method[[1]] %||% ""
    if (is.data.frame(table) && nzchar(primary_method)) {
      table <- table[as.character(table$Method) != primary_method, , drop = FALSE]
    }
  }
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  preferred <- c("Method", "Estimate", "95% CI", "N", "Raters", "Note")
  table[, intersect(preferred, names(table)), drop = FALSE]
}

interrater_agreement_column_widths <- function(columns) {
  weights <- vapply(columns, function(column) {
    switch(
      column,
      Method = 22,
      Estimate = 10,
      `95% CI` = 16,
      N = 8,
      Raters = 8,
      Reason = 38,
      Note = 28,
      10
    )
  }, numeric(1))
  weights / sum(weights) * 100
}

interrater_agreement_cell_style <- function(column, header = FALSE, last = FALSE, width = NULL) {
  center_columns <- c("Estimate", "95% CI", "N", "Raters")
  left_aligned <- !isTRUE(header) && column %in% c("Method", "Reason", "Note")
  normal_space <- column %in% c("Method", "Reason", "Note") || isTRUE(header)
  paste0(
    "padding:", if (isTRUE(header)) "4px 5px" else "5px 5px", ";",
    "line-height:", if (isTRUE(header)) "1.12" else "1.25", ";",
    "border-left:0;border-right:0;border-top:0;",
    "border-bottom:", if (isTRUE(last)) "0" else if (isTRUE(header)) "2px solid #1f2937" else "1px solid #d7dde5", ";",
    "vertical-align:middle;background:transparent;box-sizing:border-box;",
    "font-weight:", if (isTRUE(header)) "700" else "400", ";",
    "font-size:", if (isTRUE(header)) "11px" else "12px", ";",
    "white-space:", if (isTRUE(normal_space)) "normal" else "nowrap", ";",
    "overflow-wrap:", if (column %in% c("Reason", "Note")) "anywhere" else if (isTRUE(normal_space)) "break-word" else "normal", ";",
    "word-break:normal;",
    "width:", if (!is.null(width)) sprintf("%.4f%%", width) else "auto", ";",
    "min-width:0;max-width:none;",
    "text-align:", if (isTRUE(header) || column %in% center_columns) "center" else if (isTRUE(left_aligned)) "left" else "right", ";"
  )
}

interrater_agreement_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  columns <- names(table)
  widths <- interrater_agreement_column_widths(columns)
  tags$table(
    class = "coefficient-table reliability-table interrater-agreement-table",
    style = paste0(
      result_table_style(font_size = 12, min_width = 0),
      "width:100%;min-width:0;max-width:100%;table-layout:fixed;box-sizing:border-box;"
    ),
    tags$colgroup(lapply(widths, function(width) {
      tags$col(style = sprintf("width:%.4f%%;", width))
    })),
    tags$thead(
      tags$tr(lapply(seq_along(columns), function(index) {
        column <- columns[[index]]
        tags$th(
          style = interrater_agreement_cell_style(column, header = TRUE, width = widths[[index]]),
          column
        )
      }))
    ),
    tags$tbody(
      lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(seq_along(columns), function(index) {
          column <- columns[[index]]
          tags$td(
            style = interrater_agreement_cell_style(column, last = row_index == nrow(table), width = widths[[index]]),
            table[[column]][[row_index]] %||% ""
          )
        }))
      })
    )
  )
}

interrater_agreement_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  primary <- interrater_primary_agreement_table(result)
  auxiliary <- interrater_auxiliary_agreement_table(result)
  normality <- result$normality_table
  tagList(
    div(
      class = "reliability-results regression-results interrater-agreement-results",
      div(
        class = "result-section reliability-result-section regression-result-panel",
        style = "width:min(100%,920px);max-width:920px;overflow-x:visible;box-sizing:border-box;",
        h3("Recommended analysis"),
        result_table_with_notes(
          interrater_agreement_html_table(primary),
          reliability_note_tag(interrater_agreement_note(result), width = 920)
        )
      ),
      if (is.data.frame(auxiliary) && nrow(auxiliary) > 0) {
        div(
          class = "result-section reliability-result-section regression-result-panel",
          style = "width:min(100%,920px);max-width:920px;overflow-x:visible;box-sizing:border-box;",
          h3("Auxiliary agreement indices"),
          interrater_agreement_html_table(auxiliary)
        )
      },
      if (is.data.frame(normality) && nrow(normality) > 0) {
        div(
          class = "result-section reliability-result-section regression-result-panel",
          style = "width:min(100%,920px);max-width:920px;overflow-x:visible;box-sizing:border-box;",
          h3("Normality diagnostics"),
          result_table_with_notes(
            reliability_html_table(normality),
            reliability_note_tag("Normality is flagged as satisfied when absolute skewness is less than 2 and absolute kurtosis is less than 7 for every rater variable.", width = 920)
          )
        )
      },
      NULL
    )
  )
}

saved_interrater_agreement_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    "StatEdu Studio Inter-rater Agreement Results",
    tags$div(class = "regression-results", interrater_agreement_results_ui(result)),
    max_width = 1500,
    css_path = css_path,
    report_mode = report_mode
  )
}

write_interrater_agreement_results_html <- function(result, file) {
  writeLines(saved_interrater_agreement_results_html(result), file, useBytes = TRUE)
}

write_interrater_agreement_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_interrater_agreement_results_html(result, report_mode = TRUE), file)
}

save_interrater_agreement_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  used_sheets <- add_excel_table_sheet(workbook, "Recommended", interrater_primary_agreement_table(result), used_sheets, title = "Recommended analysis")
  auxiliary <- interrater_auxiliary_agreement_table(result)
  if (is.data.frame(auxiliary) && nrow(auxiliary) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Auxiliary", auxiliary, used_sheets, title = "Auxiliary agreement indices")
  }
  used_sheets <- add_excel_table_sheet(workbook, "Agreement", interrater_agreement_table(result), used_sheets, title = "All agreement indices")
  if (is.data.frame(result$normality_table) && nrow(result$normality_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Normality", result$normality_table, used_sheets, title = "Normality diagnostics")
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}
