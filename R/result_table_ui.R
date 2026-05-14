# HTML table builders for result output.

coefficient_html_table <- function(table, fit_line = NULL, stat_lines = character(0), warning_line = NULL, note_line = NULL) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  columns <- names(table)
  tagList(
    tags$table(
      class = "coefficient-table",
      if (length(note_line) > 0 && nzchar(note_line[[1]])) {
        tags$caption(class = "coefficient-note", note_line)
      },
      tags$thead(
        tags$tr(lapply(columns, function(column) tags$th(column)))
      ),
      tags$tbody(
        lapply(seq_len(nrow(table)), function(row_index) {
          tags$tr(lapply(columns, function(column) tags$td(table[[column]][[row_index]] %||% "")))
        })
      ),
      if (!is.null(fit_line) && nzchar(fit_line)) {
        tags$tfoot(
          tags$tr(
            class = "coefficient-fit-row",
            tags$td(colspan = length(columns), fit_line)
          ),
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(colspan = length(columns), line)
            )
          })
        )
      } else if (length(stat_lines) > 0) {
        tags$tfoot(
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(colspan = length(columns), line)
            )
          })
        )
      }
    ),
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
    tags$thead(tags$tr(lapply(names(table), tags$th))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(table[row_index, , drop = TRUE], tags$td))
    }))
  )
}

combined_dw_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  tags$table(
    class = "table shiny-table combined-dw-table",
    tags$thead(tags$tr(lapply(names(table), tags$th))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(table[row_index, , drop = TRUE], tags$td))
    }))
  )
}

