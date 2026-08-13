# Shared structural canvas result table rendering helpers.

structural_canvas_basic_html_table <- function(table, class = "table table-striped table-bordered") {
  tags$div(class = "table-responsive", tags$table(class = class,
    tags$thead(tags$tr(lapply(names(table), tags$th))),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
  ))
}
