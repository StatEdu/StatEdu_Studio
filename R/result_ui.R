coefficient_html_table <- function(table, fit_line = NULL, stat_lines = character(0), warning_line = NULL, note_line = NULL) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  columns <- names(table)
  tagList(
    tags$table(
      class = "coefficient-table",
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
    },
    if (length(note_line) > 0 && nzchar(note_line[[1]])) {
      tags$div(class = "coefficient-note", note_line)
    }
  )
}

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

penalized_result_block <- function(result) {
  if (!is.list(result) || !is.data.frame(result$summary)) {
    return(NULL)
  }
  render_simple_table <- function(table, class_name) {
    if (!is.data.frame(table) || nrow(table) == 0) {
      return(NULL)
    }
    tags$table(
      class = paste("table shiny-table penalized-journal-table", class_name),
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(table[row_index, , drop = TRUE], tags$td))
      }))
    )
  }
  div(
    class = "regression-result-panel penalized-result-panel",
    h3("Ridge/LASSO/Elastic Net"),
    h4("Table 1. Penalized regression model performance"),
    render_simple_table(result$summary, "penalized-summary-table"),
    div(
      class = "penalized-table-note",
      "\u03BBmin was selected by cross-validation. Apparent R\u00B2 is reported for descriptive model performance; conventional p-values are not reported for penalized regression models."
    ),
    h4("Table 2. Penalized regression coefficients"),
    render_simple_table(result$coefficient_comparison, "penalized-coefficient-comparison-table"),
    div(
      class = "penalized-table-note",
      "Coefficients are estimated at \u03BBmin. A coefficient of 0 in LASSO or Elastic Net indicates that the predictor was not selected."
    ),
    h4("Table 3. Predictors retained by penalized regression"),
    render_simple_table(result$selected_predictors, "penalized-selected-table")
  )
}

bootstrap_progress_ui <- function(status) {
  if (!is.list(status)) {
    return(NULL)
  }
  done <- as.integer(status$done %||% 0L)
  total <- as.integer(status$r %||% 0L)
  percent <- if (total > 0) min(100, max(0, round(done / total * 100))) else 0
  message <- status$message %||% "Running regression"
  label <- if (total > 0) {
    sprintf("%s Bootstrap %s / %s", message, done, total)
  } else {
    message
  }

  div(
    class = "bootstrap-progress-row bootstrap-progress-only",
    div(
      class = "bootstrap-progress-box",
      div(
        class = "bootstrap-progress-track",
        div(class = "bootstrap-progress-bar", style = sprintf("width:%s%%;", percent))
      ),
      div(class = "bootstrap-progress-label", label)
    )
  )
}

bootstrap_stop_button <- function() {
  tags$button(
    "Stop bootstrap",
    id = "stop_bootstrap",
    type = "button",
    class = "btn btn-default btn-sm bootstrap-stop-button",
    onmousedown = paste(
      "this.disabled = true;",
      "this.innerText = 'Stopping...';",
      "if (window.Shiny) Shiny.setInputValue('stop_bootstrap_now', Date.now() + Math.random(), {priority: 'event'});"
    ),
    onclick = paste(
      "if (window.Shiny) Shiny.setInputValue('stop_bootstrap_now', Date.now() + Math.random(), {priority: 'event'});"
    )
  )
}
