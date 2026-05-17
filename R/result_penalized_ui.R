# Penalized regression result UI.

penalized_result_block <- function(result) {
  if (!is.list(result) || !is.data.frame(result$summary)) {
    return(NULL)
  }
  render_simple_table <- function(table, class_name, note = NULL) {
    if (!is.data.frame(table) || nrow(table) == 0) {
      return(NULL)
    }
    table_tag <- tags$table(
      class = paste("table shiny-table penalized-journal-table", class_name),
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(table[row_index, , drop = TRUE], tags$td))
      }))
    )
    result_table_with_notes(
      table_tag,
      result_note_tag(note, class = "coefficient-note penalized-table-note")
    )
  }
  div(
    class = "regression-result-panel penalized-result-panel",
    h3("Ridge/LASSO/Elastic Net"),
    h4("Table 1. Penalized regression model performance"),
    render_simple_table(
      result$summary,
      "penalized-summary-table",
      "\u03BBmin was selected by cross-validation. Apparent R\u00B2 is reported for descriptive model performance; conventional p-values are not reported for penalized regression models."
    ),
    h4("Table 2. Penalized regression coefficients"),
    render_simple_table(
      result$coefficient_comparison,
      "penalized-coefficient-comparison-table",
      "Coefficients are estimated at \u03BBmin. A coefficient of 0 in LASSO or Elastic Net indicates that the predictor was not selected."
    ),
    h4("Table 3. Predictors retained by penalized regression"),
    render_simple_table(result$selected_predictors, "penalized-selected-table")
  )
}

