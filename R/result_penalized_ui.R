# Penalized regression result UI.

penalized_cv_plot_output_id <- function() "penalized_cv_curve_plot"

penalized_path_plot_output_id <- function() "penalized_coefficient_path_plot"

penalized_has_plot_data <- function(result) {
  is.list(result) &&
    is.data.frame(result$cv_curves) &&
    nrow(result$cv_curves) > 0 &&
    is.data.frame(result$coefficient_paths) &&
    nrow(result$coefficient_paths) > 0
}

penalized_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 13)
    )
}

plot_penalized_cv_curve <- function(result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Package 'ggplot2' is required for penalized regression plots.")
    return(invisible(NULL))
  }
  curve <- result$cv_curves
  if (!is.data.frame(curve) || nrow(curve) == 0) {
    plot.new()
    text(0.5, 0.5, "No cross-validation curve data.")
    return(invisible(NULL))
  }
  reference <- unique(curve[, c("Outcome", "Method", "lambda_min", "lambda_1se"), drop = FALSE])
  reference$log_lambda_min <- log(reference$lambda_min)
  reference$log_lambda_1se <- log(reference$lambda_1se)
  p <- ggplot2::ggplot(curve, ggplot2::aes(x = log_lambda, y = `CV MSE`, color = Method)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = `CV MSE` - `CV SE`, ymax = `CV MSE` + `CV SE`, fill = Method),
      alpha = 0.12,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(size = 1.1, alpha = 0.7) +
    ggplot2::geom_vline(
      data = reference,
      ggplot2::aes(xintercept = log_lambda_min, color = Method),
      linetype = "dashed",
      linewidth = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      data = reference,
      ggplot2::aes(xintercept = log_lambda_1se, color = Method),
      linetype = "dotted",
      linewidth = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::facet_wrap(stats::as.formula("~ Outcome"), scales = "free_y") +
    ggplot2::labs(
      title = "Cross-validation curve",
      x = "log(lambda)",
      y = "Cross-validated MSE",
      color = "Method",
      fill = "Method"
    ) +
    penalized_plot_theme()
  print(p)
}

plot_penalized_coefficient_path <- function(result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Package 'ggplot2' is required for penalized regression plots.")
    return(invisible(NULL))
  }
  path <- result$coefficient_paths
  if (!is.data.frame(path) || nrow(path) == 0) {
    plot.new()
    text(0.5, 0.5, "No coefficient path data.")
    return(invisible(NULL))
  }
  reference <- unique(path[, c("Outcome", "Method", "lambda_min", "lambda_1se"), drop = FALSE])
  reference$log_lambda_min <- log(reference$lambda_min)
  reference$log_lambda_1se <- log(reference$lambda_1se)
  p <- ggplot2::ggplot(path, ggplot2::aes(x = log_lambda, y = Coefficient, group = Predictor, color = Predictor)) +
    ggplot2::geom_hline(yintercept = 0, color = "#888888", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.85) +
    ggplot2::geom_vline(
      data = reference,
      ggplot2::aes(xintercept = log_lambda_min),
      linetype = "dashed",
      color = "#333333",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = reference,
      ggplot2::aes(xintercept = log_lambda_1se),
      linetype = "dotted",
      color = "#333333",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(Outcome ~ Method, scales = "free_y") +
    ggplot2::labs(
      title = "Coefficient path",
      x = "log(lambda)",
      y = "Coefficient",
      color = "Predictor"
    ) +
    penalized_plot_theme()
  print(p)
}

penalized_plot_block <- function(result) {
  if (!penalized_has_plot_data(result)) {
    return(NULL)
  }
  div(
    class = "penalized-plot-section",
    h4("Figure 1. Cross-validation curve"),
    div(
      class = "penalized-plot-card",
      plotOutput(penalized_cv_plot_output_id(), height = "460px")
    ),
    h4("Figure 2. Coefficient path"),
    div(
      class = "penalized-plot-card",
      plotOutput(penalized_path_plot_output_id(), height = "520px")
    ),
    result_note_tag(
      "Dashed vertical lines indicate lambda.min; dotted vertical lines indicate lambda.1se.",
      class = "coefficient-note penalized-plot-note"
    )
  )
}

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
    h3("Penalized Regression: Ridge, LASSO, and Elastic Net"),
    h4("Table 1. Publication summary using lambda.1se"),
    render_simple_table(
      result$publication_summary,
      "penalized-publication-summary-table",
      "The publication summary reports the more parsimonious lambda.1se solution. CV RMSE, CV MAE, and CV R\u00B2 are based on out-of-fold predictions."
    ),
    h4("Table 2. Predictors retained in the publication model"),
    render_simple_table(
      result$publication_selected_predictors,
      "penalized-publication-selected-table",
      "Ridge regression shrinks coefficients but does not perform variable selection; therefore all predictors are retained."
    ),
    h4("Table 3. Selection stability of the publication model"),
    render_simple_table(
      result$publication_stability,
      "penalized-publication-stability-table",
      "This table reports bootstrap selection stability for predictors retained in the lambda.1se publication model. Stability is classified as High (>= .80), Moderate (.50-.79), or Low (< .50). Ridge is omitted because it does not perform variable selection."
    ),
    h4("Appendix Table A1. Cross-validated tuning and model performance"),
    render_simple_table(
      result$summary,
      "penalized-summary-table",
      "lambda.min minimizes cross-validated mean squared error. lambda.1se is the largest lambda within one standard error of the minimum and is typically more parsimonious. CV RMSE, CV MAE, and CV R\u00B2 are based on out-of-fold predictions; apparent indices are descriptive in-sample summaries. Conventional p-values are not reported for penalized regression models."
    ),
    h4("Appendix Table A2. OLS and penalized regression coefficients"),
    render_simple_table(
      result$coefficient_comparison,
      "penalized-coefficient-comparison-table",
      "Penalized coefficients are estimated after internal predictor standardization and returned on the original response scale. A coefficient of 0 in LASSO or Elastic Net indicates that the predictor was not selected."
    ),
    h4("Appendix Table A3. Independent variables retained by penalized regression"),
    render_simple_table(
      result$selected_predictors,
      "penalized-selected-table",
      "Ridge regression shrinks coefficients but does not perform variable selection; therefore all predictors are retained."
    ),
    h4("Appendix Table A4. Cross-validation settings"),
    render_simple_table(
      result$cv_settings,
      "penalized-settings-table",
      "Elastic Net selects the alpha value with the lowest cross-validated mean squared error from the reported alpha grid. For bootstrap selection stability, the selected alpha is fixed and lambda is re-estimated within each bootstrap resample."
    ),
    h4("Appendix Table A5. Bootstrap selection stability"),
    render_simple_table(
      result$selection_stability,
      "penalized-selection-stability-table",
      "Selection frequency is the proportion of successful bootstrap resamples in which each predictor was retained by the specified penalized method and lambda rule. For Elastic Net, alpha is fixed at the selected full-sample value during bootstrap stability estimation. Ridge is omitted because it shrinks coefficients but does not perform variable selection."
    ),
    penalized_plot_block(result)
  )
}
