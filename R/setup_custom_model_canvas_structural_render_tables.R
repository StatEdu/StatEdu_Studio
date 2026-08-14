# Shared structural canvas result table rendering helpers.

structural_canvas_numeric_display_cell <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(FALSE)
  grepl("^(?:[<>]=?)?-?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?(?:[%†*]+)?$", value)
}

structural_canvas_html_cell <- function(value, header = FALSE) {
  value <- as.character(value %||% "")
  if (isTRUE(header)) return(tags$th(value))
  if (structural_canvas_numeric_display_cell(value)) {
    return(tags$td(class = "text-right structural-numeric-cell", style = "text-align: right;", value))
  }
  tags$td(value)
}

structural_canvas_basic_html_table <- function(table, class = "table table-striped table-bordered") {
  tags$div(class = "table-responsive", tags$table(class = class,
    tags$thead(tags$tr(lapply(names(table), structural_canvas_html_cell, header = TRUE))),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), structural_canvas_html_cell))))
  ))
}
