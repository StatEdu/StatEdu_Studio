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
    "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
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

result_cell_bold <- function(table, row_index, column) {
  bold_cells <- attr(table, "bold_cells", exact = TRUE)
  if (!is.data.frame(bold_cells) || nrow(bold_cells) == 0) {
    return(FALSE)
  }
  any(bold_cells$row == row_index & bold_cells$column == column)
}

result_cell_style_extra <- function(table, row_index, column) {
  cell_styles <- attr(table, "cell_styles", exact = TRUE)
  if (!is.data.frame(cell_styles) || nrow(cell_styles) == 0) {
    return("")
  }
  matched <- cell_styles[cell_styles$row == row_index & cell_styles$column == column, , drop = FALSE]
  if (nrow(matched) == 0 || !"style" %in% names(matched)) {
    return("")
  }
  paste(as.character(matched$style), collapse = "")
}

result_cell_span_start <- function(table, row_index, column) {
  spans <- attr(table, "spanning_cells", exact = TRUE)
  if (!is.data.frame(spans) || nrow(spans) == 0) {
    return(NULL)
  }
  matched <- spans[spans$row == row_index & spans$start_column == column, , drop = FALSE]
  if (nrow(matched) == 0) NULL else matched[1, , drop = FALSE]
}

result_cell_covered_by_span <- function(table, row_index, column, columns) {
  spans <- attr(table, "spanning_cells", exact = TRUE)
  if (!is.data.frame(spans) || nrow(spans) == 0) {
    return(FALSE)
  }
  column_index <- match(column, columns)
  if (!is.finite(column_index)) {
    return(FALSE)
  }
  any(vapply(seq_len(nrow(spans)), function(index) {
    if (spans$row[[index]] != row_index || spans$start_column[[index]] == column) {
      return(FALSE)
    }
    start_index <- match(spans$start_column[[index]], columns)
    end_index <- match(spans$end_column[[index]], columns)
    is.finite(start_index) && is.finite(end_index) && column_index >= start_index && column_index <= end_index
  }, logical(1)))
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

result_cell_value_without_marker <- function(value, marker = "") {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value) || !nzchar(marker)) {
    return(value)
  }
  sub(paste0(marker, "$"), "", value)
}

coefficient_display_columns <- function(table) {
  columns <- names(table)
  markers <- attr(table, "note_markers", exact = TRUE)
  if (!is.data.frame(markers) || nrow(markers) == 0 || !"column" %in% names(markers)) {
    return(data.frame(
      source = columns,
      label = columns,
      marker = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  marker_columns <- intersect(columns, unique(as.character(markers$column)))
  rows <- list()
  for (column in columns) {
    rows[[length(rows) + 1L]] <- data.frame(source = column, label = column, marker = FALSE, stringsAsFactors = FALSE)
    if (column %in% marker_columns) {
      rows[[length(rows) + 1L]] <- data.frame(source = column, label = "", marker = TRUE, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

coefficient_display_cell_style <- function(table, row_index, column, display_index, display_meta, compact, compact_font_size, compact_width, compact_first_width) {
  marker_column <- isTRUE(display_meta$marker[[display_index]])
  source_columns <- display_meta$source[!display_meta$marker]
  source_index <- match(column, source_columns)
  style <- result_body_cell_style(
    display_index == 1,
    row_index == nrow(table),
    compact = compact,
    compact_font_size = compact_font_size,
    compact_width = compact_width,
    compact_first_width = compact_first_width
  )
  if (isTRUE(marker_column)) {
    return(paste0(
      style,
      "padding-left:2px;padding-right:8px;padding-top:0;min-width:16px;width:16px;text-align:left;vertical-align:top;line-height:1;"
    ))
  }
  next_is_marker <- display_index < nrow(display_meta) &&
    isTRUE(display_meta$marker[[display_index + 1L]]) &&
    identical(display_meta$source[[display_index + 1L]], column)
  if (isTRUE(next_is_marker)) {
    style <- paste0(style, "padding-right:2px;")
  }
  if (is.finite(source_index) && source_index == 1 && display_index != 1) {
    style <- paste0(style, "text-align:left;")
  }
  style
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
  display_meta <- coefficient_display_columns(table)
  table_tag <- tags$table(
      class = "coefficient-table",
      style = result_table_style(font_size = if (isTRUE(compact)) compact_font_size else 15, min_width = if (isTRUE(compact)) compact_min_width else 480),
      tags$colgroup(lapply(seq_len(nrow(display_meta)), function(index) {
        tags$col(class = if (isTRUE(display_meta$marker[[index]])) "coefficient-col-note-marker" else coefficient_column_class(display_meta$source[[index]]))
      })),
      tags$thead(
        tags$tr(lapply(seq_len(nrow(display_meta)), function(index) {
          tags$th(
            style = paste0(
              result_header_cell_style(
                index == 1,
                compact = compact,
                compact_font_size = compact_font_size,
                compact_width = compact_width,
                compact_first_width = compact_first_width
              ),
              if (isTRUE(display_meta$marker[[index]])) "padding-left:2px;padding-right:8px;min-width:16px;width:16px;text-align:left;" else "",
              if (!isTRUE(display_meta$marker[[index]]) && index < nrow(display_meta) && isTRUE(display_meta$marker[[index + 1L]])) "padding-right:2px;" else ""
            ),
            display_meta$label[[index]]
          )
        }))
      ),
      tags$tbody(
        lapply(seq_len(nrow(table)), function(row_index) {
          tags$tr(lapply(seq_len(nrow(display_meta)), function(column_index) {
            column <- display_meta$source[[column_index]]
            marker_column <- isTRUE(display_meta$marker[[column_index]])
            if (isTRUE(result_cell_covered_by_span(table, row_index, column, columns))) {
              return(NULL)
            }
            span <- result_cell_span_start(table, row_index, column)
            marker <- result_cell_note_marker(table, row_index, column)
            value <- if (!is.null(span) && "value" %in% names(span)) span$value[[1]] else table[[column]][[row_index]] %||% ""
            content <- if (isTRUE(marker_column)) {
              if (nzchar(marker)) tags$sup(class = "coefficient-note-cell-marker", marker) else ""
            } else {
              result_cell_value_without_marker(value, marker)
            }
            bold_style <- if (isTRUE(result_cell_bold(table, row_index, column)) && nzchar(as.character(table[[column]][[row_index]] %||% ""))) "font-weight:700;" else ""
            colspan <- if (!is.null(span)) {
              start_index <- match(span$start_column[[1]], columns)
              end_index <- match(span$end_column[[1]], columns)
              max(1L, end_index - start_index + 1L)
            } else {
              NULL
            }
            span_style <- if (!is.null(span) && "style" %in% names(span)) as.character(span$style[[1]] %||% "") else ""
            tags$td(
              colspan = colspan,
              style = paste0(
                coefficient_display_cell_style(
                  table,
                  row_index,
                  column,
                  column_index,
                  display_meta,
                  compact = compact,
                  compact_font_size = compact_font_size,
                  compact_width = compact_width,
                  compact_first_width = compact_first_width
                ),
                bold_style,
                result_cell_style_extra(table, row_index, column),
                span_style
              ),
              content
            )
          }))
        })
      ),
      if (!is.null(fit_line) && nzchar(fit_line)) {
        tags$tfoot(
          tags$tr(
            class = "coefficient-fit-row",
            tags$td(
              colspan = nrow(display_meta),
              style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:2px solid #1f2937;border-bottom:0;text-align:center;font-weight:500;",
              fit_line
            )
          ),
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(
                colspan = nrow(display_meta),
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
                colspan = nrow(display_meta),
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

