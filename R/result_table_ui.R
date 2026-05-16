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
  tagList(
    tags$table(
      class = "coefficient-table",
      style = result_table_style(font_size = if (isTRUE(compact)) compact_font_size else 15, min_width = if (isTRUE(compact)) compact_min_width else 480),
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
            tags$td(
              style = result_body_cell_style(
                column_index == 1,
                row_index == nrow(table),
                compact = compact,
                compact_font_size = compact_font_size,
                compact_width = compact_width,
                compact_first_width = compact_first_width
              ),
              table[[column]][[row_index]] %||% ""
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
    ),
    if (length(note_line) > 0 && nzchar(note_line[[1]])) {
      tags$div(class = "coefficient-note", note_line)
    },
    if (length(warning_line) > 0 && nzchar(warning_line[[1]])) {
      tags$div(class = "coefficient-warning", warning_line)
    }
  )
}


model_overview_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  tags$table(
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
}

combined_dw_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  tags$table(
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
}

