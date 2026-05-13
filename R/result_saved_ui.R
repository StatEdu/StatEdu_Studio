# Saved analysis HTML output.

saved_analysis_results_html <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  css_path = file.path("www", "style.css")
) {
  css <- if (file.exists(css_path)) paste(readLines(css_path, warn = FALSE), collapse = "\n") else ""
  document <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title("EasyFlow Statistics Results"),
      tags$style(htmltools::HTML(css)),
      tags$style(htmltools::HTML(
        paste(
          "body { background: #ffffff; }",
          ".page-shell { max-width: 1280px; margin: 24px auto; }",
          ".saved-results-meta { color: #52606d; margin: 4px 0 18px; font-size: 13px; }",
          ".regression-results { border-top: 0; padding-top: 0; }",
          ".regression-result-panel { break-inside: avoid; page-break-inside: avoid; }",
          ".residual-diagnostic-plots img { display: block; width: 420px; height: 420px; }",
          sep = "\n"
        )
      ))
    ),
    tags$body(
      div(
        class = "page-shell",
        h1("EasyFlow Statistics Results"),
        div(
          class = "saved-results-meta",
          sprintf("Saved: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        ),
        div(
          class = "regression-results",
          div(
            class = "regression-result-panel model-overview-panel",
            h3("Model overview"),
            model_overview_html_table(model_overview_data_frame(results, variable_table, labels))
          ),
          lapply(seq_along(results), function(index) {
            regression_coefficient_result_block(
              results[[index]],
              variable_table,
              labels,
              category_table,
              refs,
              value_labels,
              show_sr2,
              show_f2,
              show_vif
            )
          }),
          effect_size_reference_panel(show_sr2, show_f2),
          lapply(seq_along(results), function(index) {
            result <- results[[index]]
            dependent <- all.vars(result$formula)[[1]]
            dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
            saved_plot_result_block(result, dependent_label)
          }),
          regression_durbin_watson_result_block(results, variable_table, labels)
        )
      )
    )
  )
  paste0("<!DOCTYPE html>\n", tags_to_html(document))
}

