# Factor analysis result UI and plots.

factor_analysis_scree_plot_id <- function() {
  "factor_scree_plot_output"
}

factor_analysis_plot_size <- function(result, base = 640, per_variable = 18, max_size = 980) {
  count <- length(result$variables %||% character(0))
  paste0(min(max_size, max(base, 260 + count * per_variable)), "px")
}

factor_analysis_note <- function(result) {
  paste(
    "Loadings with absolute values below .30 are hidden.",
    "h2 is communality, u2 is uniqueness, and complexity summarizes cross-loading pattern."
  )
}

factor_analysis_suitability_note <- function(result) {
  "KMO values of .60 or higher and a significant Bartlett test are commonly treated as evidence that factor analysis is appropriate."
}

factor_analysis_normality_note <- function(result) {
  if (identical(result$normality_method %||% "skew_kurt", "mardia")) {
    return("Mardia normality is treated as satisfied when both skewness and kurtosis tests have p >= .05.")
  }
  "Normality is treated as satisfied when each variable has |skewness| < 2 and |kurtosis| < 7."
}

factor_analysis_results_ui <- function(result, report_mode = FALSE) {
  if (is.null(result)) {
    return(NULL)
  }
  tagList(
    div(
      class = "factor-analysis-results regression-results",
      div(
        class = "result-section factor-analysis-result-section regression-result-panel",
        h3("Factor analysis"),
        coefficient_html_table(result$overview)
      ),
      div(
        class = "result-section factor-analysis-result-section regression-result-panel",
        h3("Suitability"),
        coefficient_html_table(result$suitability$overview, note_line = factor_analysis_suitability_note(result))
      ),
      if (is.data.frame(result$normality_table) && nrow(result$normality_table) > 0) {
        div(
          class = "result-section factor-analysis-result-section regression-result-panel",
          h3("Normality"),
          coefficient_html_table(result$normality_table, note_line = factor_analysis_normality_note(result))
        )
      },
      div(
        class = "result-section factor-analysis-result-section regression-result-panel landscape-table-panel",
        h3("Pattern / loading matrix"),
        coefficient_html_table(
          result$loadings_table,
          compact = TRUE,
          compact_font_size = 13,
          compact_width = 70,
          compact_first_width = 150,
          compact_min_width = 520,
          note_line = factor_analysis_note(result)
        )
      ),
      if (is.data.frame(result$variance_table) && nrow(result$variance_table) > 0) {
        div(
          class = "result-section factor-analysis-result-section regression-result-panel landscape-table-panel",
          h3("Variance explained"),
          coefficient_html_table(
            result$variance_table,
            compact = TRUE,
            compact_font_size = 13,
            compact_width = 86,
            compact_first_width = 130,
            compact_min_width = 520
          )
        )
      },
      if (is.data.frame(result$factor_correlation_table) && nrow(result$factor_correlation_table) > 0) {
        div(
          class = "result-section factor-analysis-result-section regression-result-panel landscape-table-panel",
          h3("Factor correlations"),
          coefficient_html_table(
            result$factor_correlation_table,
            compact = TRUE,
            compact_font_size = 13,
            compact_width = 76,
            compact_first_width = 100,
            compact_min_width = 420
          )
        )
      },
      div(
        class = "result-section factor-analysis-result-section factor-analysis-plot-section regression-result-panel",
        h3("Scree plot"),
        if (isTRUE(report_mode)) {
          tags$img(
            src = plot_data_uri(draw_factor_analysis_scree_plot, result, width = 900, height = 620, res = 120),
            style = "max-width:900px;width:100%;height:auto;"
          )
        } else {
          plotOutput(
            factor_analysis_scree_plot_id(),
            width = factor_analysis_plot_size(result),
            height = "520px"
          )
        }
      ),
      div(
        class = "result-section factor-analysis-result-section regression-result-panel landscape-table-panel",
        h3("Eigenvalues"),
        coefficient_html_table(
          result$eigen_table,
          compact = TRUE,
          compact_font_size = 13,
          compact_width = 86,
          compact_first_width = 90,
          compact_min_width = 360
        )
      )
    )
  )
}

draw_factor_analysis_scree_plot <- function(result) {
  eigenvalues <- as.numeric(result$eigenvalues %||% numeric(0))
  if (length(eigenvalues) == 0 || all(!is.finite(eigenvalues))) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No eigenvalues")
    return(invisible(NULL))
  }
  factors <- seq_along(eigenvalues)
  selected <- factors <= as.integer(result$n_factors %||% 1L)
  y_max <- max(1.2, eigenvalues, na.rm = TRUE) * 1.08
  graphics::par(mar = c(4.5, 4.5, 2.5, 1.5), cex = 1.12)
  graphics::plot(
    factors,
    eigenvalues,
    type = "b",
    pch = 16,
    lwd = 1.6,
    col = "#1f6fa8",
    xlab = "Factor number",
    ylab = "Eigenvalue",
    ylim = c(0, y_max),
    xaxt = "n",
    main = ""
  )
  graphics::axis(1, at = factors)
  graphics::abline(h = 1, lty = 2, col = "#9a3412", lwd = 1.2)
  graphics::points(factors[selected], eigenvalues[selected], pch = 16, cex = 1.35, col = "#c2410c")
  graphics::text(
    x = max(factors),
    y = 1,
    labels = " eigenvalue = 1.0",
    pos = 3,
    cex = 0.9,
    col = "#7c2d12"
  )
  graphics::box(col = "#1f2937")
  invisible(NULL)
}
