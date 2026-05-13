# Result panel UI builders.

coefficient_result_ui <- function(table, result, show_sr2 = FALSE, show_f2 = FALSE, show_vif = FALSE) {
  table <- filter_coefficient_export_table(table, show_sr2, show_f2, show_vif)
  fit_line <- coefficient_fit_line(result)
  stat_lines <- coefficient_stat_lines(result)
  warning_line <- coefficient_vif_warning_line(result)
  note_line <- coefficient_note_line(result, show_vif, show_sr2, show_f2)
  coefficient_html_table(table, fit_line, stat_lines, warning_line, note_line)
}

coefficient_result_block <- function(title, content) {
  div(
    class = "regression-result-panel",
    h3(title),
    content
  )
}

effect_size_reference_panel <- function(show_sr2 = FALSE, show_f2 = FALSE) {
  if (!isTRUE(show_sr2) && !isTRUE(show_f2)) {
    return(NULL)
  }
  rows <- list()
  if (isTRUE(show_sr2)) {
    rows <- c(rows, list(tags$tr(
      tags$td(tags$span("sr", tags$sup("2"))),
      tags$td("Cohen et al. (2003); Pedhazur (1997)"),
      tags$td(".01"),
      tags$td(".09"),
      tags$td(".25")
    )))
  }
  if (isTRUE(show_f2)) {
    rows <- c(rows, list(tags$tr(
      tags$td(tags$span("Cohen's f", tags$sup("2"))),
      tags$td("Cohen et al. (2003)"),
      tags$td(".02"),
      tags$td(".15"),
      tags$td(".35")
    )))
  }
  tagList(
    div(
      class = "effect-size-reference-panel",
      h4("Effect Size Guidelines"),
      tags$table(
        class = "effect-size-reference-table",
        tags$thead(tags$tr(
          tags$th("Effect size"),
          tags$th("Reference"),
          tags$th("Small"),
          tags$th("Medium"),
          tags$th("Large")
        )),
        tags$tbody(rows)
      ),
      if (isTRUE(show_sr2)) {
        p(
          class = "effect-size-reference-note",
          tags$span("Squared semi-partial correlations (sr", tags$sup("2"), ") were examined to estimate the unique variance explained by each predictor (Cohen et al., 2003; Pedhazur, 1997). Values of .01, .09, and .25 were interpreted as small, medium, and large effects, respectively.")
        )
      },
      p(
        class = "effect-size-reference-citation",
        "Cohen, J., Cohen, P., West, S. G., & Leona S. Aiken (2003). Applied multiple regression/correlation analysis for the behavioral sciences (3rd ed.). Lawrence Erlbaum Associates."
      ),
      if (isTRUE(show_sr2)) {
        p(
          class = "effect-size-reference-citation",
          "Elazar J. Pedhazur (1997). Multiple regression in behavioral research: Explanation and prediction (3rd ed.). Harcourt Brace."
        )
      }
    )
  )
}

saved_plot_result_block <- function(result, dependent_label) {
  div(
    class = "regression-result-panel",
    h3(sprintf("Diagnostic plots(%s)", dependent_label)),
    div(
      class = "residual-diagnostic-plots",
      div(
        class = "residual-plot-card",
        h4("Q-Q plot"),
        tags$img(
          src = plot_data_uri(plot_residual_qq, result),
          width = "420",
          height = "420",
          alt = sprintf("Q-Q plot(%s)", dependent_label)
        )
      ),
      div(
        class = "residual-plot-card",
        h4("Residual homoscedasticity"),
        tags$img(
          src = plot_data_uri(plot_residual_homoscedasticity, result),
          width = "420",
          height = "420",
          alt = sprintf("Residual homoscedasticity(%s)", dependent_label)
        )
      )
    )
  )
}

plot_result_panel <- function(dependent_label, qq_output_id, homoscedasticity_output_id) {
  div(
    class = "regression-result-panel",
    h3(sprintf("Diagnostic plots(%s)", dependent_label)),
    div(
      class = "residual-diagnostic-plots",
      div(
        class = "residual-plot-card",
        h4("Q-Q plot"),
        plotOutput(qq_output_id, height = "420px")
      ),
      div(
        class = "residual-plot-card",
        h4("Residual homoscedasticity"),
        plotOutput(homoscedasticity_output_id, height = "420px")
      )
    )
  )
}

durbin_watson_result_block <- function(table) {
  div(
    class = "regression-result-panel",
    h3("Durbin-Watson"),
    combined_dw_html_table(table)
  )
}

regression_coefficient_result_block <- function(
  result,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  coefficient_result_block(
    coefficient_panel_title_static(result, variable_table, labels),
    coefficient_result_ui(
      coefficient_output_table_with_context(
        coefficient_display_table(result),
        result$predictors,
        include_references = TRUE,
        variable_info = variable_table,
        refs = refs,
        value_labels = value_labels,
        labels = labels,
        category_table = category_table
      ),
      result,
      show_sr2,
      show_f2,
      show_vif
    )
  )
}

regression_durbin_watson_result_block <- function(results, variable_table = NULL, labels = character(0)) {
  durbin_watson_result_block(combined_dw_data_frame(results, variable_table, labels))
}

regression_results_panel <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  penalized = NULL,
  plot_blocks = NULL
) {
  show_penalized <- is.list(penalized)
  div(
    class = "regression-results",
    div(
      class = "regression-result-panel model-overview-panel",
      h3("Model overview"),
      model_overview_html_table(model_overview_data_frame(results, variable_table, labels))
    ),
    penalized_result_block(penalized),
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
    if (!isTRUE(show_penalized)) {
      tagList(
        plot_blocks,
        regression_durbin_watson_result_block(results, variable_table, labels)
      )
    }
  )
}

