# HTML table builders for result output.

result_table_style <- function(font_size = 15, min_width = 480) {
  paste(
    sprintf("width:auto;min-width:%dpx;border-collapse:collapse;border-spacing:0;", min_width),
    "border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
    sprintf("color:#2f3a46;font-size:%dpx;background:transparent;", font_size)
  )
}

result_header_cell_style <- function(first = FALSE, compact = FALSE, compact_font_size = 12, compact_width = 62, compact_first_width = 118) {
  padding <- if (isTRUE(compact)) "7px 12px" else "9px 18px"
  width <- if (isTRUE(first)) {
    if (isTRUE(compact)) paste0(compact_first_width, "px") else "150px"
  } else if (isTRUE(compact)) {
    paste0(compact_width, "px")
  } else {
    "86px"
  }
  paste0(
    "padding:", padding, ";line-height:1.35;border-left:0;border-right:0;",
    "border-top:0;border-bottom:2px solid #1f2937;vertical-align:middle;",
    "font-weight:700;background:transparent;white-space:nowrap;",
    "min-width:", width, ";",
    "text-align:", if (isTRUE(first)) "left" else "right", ";"
  )
}

result_body_cell_style <- function(first = FALSE, last = FALSE, compact = FALSE, compact_font_size = 12, compact_width = 62, compact_first_width = 118) {
  padding <- if (isTRUE(compact)) "7px 12px" else "9px 18px"
  width <- if (isTRUE(first)) {
    if (isTRUE(compact)) paste0(compact_first_width, "px") else "150px"
  } else if (isTRUE(compact)) {
    paste0(compact_width, "px")
  } else {
    "86px"
  }
  paste0(
    "padding:", padding, ";line-height:1.35;border-left:0;border-right:0;",
    "border-top:0;border-bottom:", if (isTRUE(last)) "0" else "1px solid #d7dde5", ";",
    "vertical-align:middle;background:transparent;white-space:nowrap;",
    "min-width:", width, ";",
    "white-space:pre-line;",
    "text-align:", if (isTRUE(first)) "left" else "right", ";"
  )
}

result_note_tag <- function(text, class = "coefficient-note") {
  if (length(text) == 0 || !nzchar(text[[1]] %||% "")) {
    return(NULL)
  }
  tags$div(class = class, text)
}

result_table_with_notes <- function(table_tag, ..., class = "result-table-with-note") {
  notes <- list(...)
  notes <- Filter(Negate(is.null), notes)
  if (is.null(table_tag)) {
    return(NULL)
  }
  do.call(
    tags$div,
    c(
      list(class = class),
      list(table_tag),
      notes
    )
  )
}

result_cell_note_marker <- function(table, row_index, column) {
  markers <- attr(table, "note_markers", exact = TRUE)
  if (!is.data.frame(markers) || nrow(markers) == 0) {
    return("")
  }
  matched <- markers[markers$row == row_index & markers$column == column, , drop = FALSE]
  if (nrow(matched) == 0) "" else as.character(matched$marker[[1]])
}

result_cell_content <- function(value, marker = "") {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value) || !nzchar(marker)) {
    return(value)
  }
  base <- sub(paste0(marker, "$"), "", value)
  tags$span(
    class = "coefficient-footnote-value",
    base,
    tags$sup(class = "coefficient-footnote-marker", marker)
  )
}

coefficient_column_class <- function(name) {
  normalized <- gsub("[^[:alnum:]]+", "", tolower(as.character(name %||% "")))
  switch(
    normalized,
    term = "coefficient-col-term",
    b = "coefficient-col-b",
    reference = "coefficient-col-reference",
    t = "coefficient-col-compact",
    p = "coefficient-col-compact",
    sr2 = "coefficient-col-compact",
    f2 = "coefficient-col-compact",
    vif = "coefficient-col-compact",
    tolerance = "coefficient-col-tolerance",
    "coefficient-col-stat"
  )
}

result_column_key <- function(name) {
  name <- gsub("\u00B2", "2", as.character(name %||% ""), fixed = TRUE)
  gsub("[^[:alnum:]]+", "", tolower(name))
}

hierarchical_compact_stat_column <- function(name) {
  result_column_key(name) %in% c("llci", "ulci", "p", "bootp", "sr2", "f2", "vif")
}

hierarchical_stat_column_class <- function(name) {
  if (isTRUE(hierarchical_compact_stat_column(name))) {
    return("hierarchical-stat-col hierarchical-stat-col-narrow")
  }
  "hierarchical-stat-col"
}

hierarchical_stat_column_width <- function(name) {
  key <- result_column_key(name)
  if (key %in% c("bootp")) {
    return(54L)
  }
  if (key %in% c("llci", "ulci", "p", "sr2", "f2", "vif")) {
    return(48L)
  }
  70L
}

hierarchical_stat_cell_style <- function(column, last = FALSE, header = FALSE) {
  width <- hierarchical_stat_column_width(column)
  padding <- if (isTRUE(hierarchical_compact_stat_column(column))) "9px 4px" else "9px 7px"
  paste0(
    "padding:", padding, ";line-height:1.45;border-left:0;border-right:0;",
    "border-top:0;border-bottom:",
    if (isTRUE(header)) "2px solid #1f2937" else if (isTRUE(last)) "0" else "1px solid #d7dde5",
    ";vertical-align:middle;background:transparent;",
    "width:", width, "px;min-width:", width, "px;max-width:", width, "px;",
    "text-align:right;white-space:nowrap;overflow-wrap:normal;"
  )
}

coefficient_html_table <- function(
  table,
  fit_line = NULL,
  stat_lines = character(0),
  warning_line = NULL,
  note_line = NULL,
  compact = FALSE,
  compact_font_size = 12,
  compact_width = 62,
  compact_first_width = 118,
  compact_min_width = 330
) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  columns <- names(table)
  table_tag <- tags$table(
      class = "coefficient-table",
      style = result_table_style(font_size = if (isTRUE(compact)) compact_font_size else 15, min_width = if (isTRUE(compact)) compact_min_width else 480),
      tags$colgroup(lapply(columns, function(column) {
        tags$col(class = coefficient_column_class(column))
      })),
      tags$thead(
        tags$tr(lapply(seq_along(columns), function(index) {
          tags$th(
            style = result_header_cell_style(
              index == 1,
              compact = compact,
              compact_font_size = compact_font_size,
              compact_width = compact_width,
              compact_first_width = compact_first_width
            ),
            columns[[index]]
          )
        }))
      ),
      tags$tbody(
        lapply(seq_len(nrow(table)), function(row_index) {
          tags$tr(lapply(seq_along(columns), function(column_index) {
            column <- columns[[column_index]]
            marker <- result_cell_note_marker(table, row_index, column)
            tags$td(
              style = result_body_cell_style(
                column_index == 1,
                row_index == nrow(table),
                compact = compact,
                compact_font_size = compact_font_size,
                compact_width = compact_width,
                compact_first_width = compact_first_width
              ),
              result_cell_content(table[[column]][[row_index]] %||% "", marker)
            )
          }))
        })
      ),
      if (!is.null(fit_line) && nzchar(fit_line)) {
        tags$tfoot(
          tags$tr(
            class = "coefficient-fit-row",
            tags$td(
              colspan = length(columns),
              style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:2px solid #1f2937;border-bottom:0;text-align:center;font-weight:500;",
              fit_line
            )
          ),
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(
                colspan = length(columns),
                style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:1px solid #d7dde5;border-bottom:0;text-align:center;font-weight:500;",
                line
              )
            )
          })
        )
      } else if (length(stat_lines) > 0) {
        tags$tfoot(
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(
                colspan = length(columns),
                style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:1px solid #d7dde5;border-bottom:0;text-align:center;font-weight:500;",
                line
              )
            )
          })
        )
      }
  )
  result_table_with_notes(
    table_tag,
    result_note_tag(note_line),
    result_note_tag(warning_line, class = "coefficient-warning")
  )
}


model_overview_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table_tag <- tags$table(
    class = "table shiny-table combined-model-overview-table",
    style = result_table_style(),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = result_header_cell_style(index == 1), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      values <- table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = result_body_cell_style(index == 1, row_index == nrow(table)), values[[index]])
      }))
    }))
  )
  result_table_with_notes(table_tag)
}

combined_dw_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table_tag <- tags$table(
    class = "table shiny-table combined-dw-table",
    style = result_table_style(),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = result_header_cell_style(index == 1), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      values <- table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = result_body_cell_style(index == 1, row_index == nrow(table)), values[[index]])
      }))
    }))
  )
  result_table_with_notes(table_tag)
}

