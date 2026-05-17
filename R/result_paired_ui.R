# Paired test result UI.

paired_results_ui <- function(result) {
  if (is.null(result)) {
    return(empty_message("Move paired variables and click Run analysis."))
  }
  if (!is.null(result$error)) {
    return(empty_message(result$error))
  }
  tags$div(
    class = "regression-results paired-results",
    tags$div(
      class = "result-section paired-result-section regression-result-panel",
      tags$h3("Paired test"),
      coefficient_html_table(result$table)
    ),
    if (isTRUE(result$options$assumption_check) && is.data.frame(result$checks) && nrow(result$checks) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Assumption check"),
        coefficient_html_table(result$checks)
      )
    }
  )
}
