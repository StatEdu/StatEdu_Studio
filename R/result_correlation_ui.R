# Correlation result UI and plots.

correlation_display_table <- function(result) {
  table <- result$pairwise_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(table)
  }
  options <- result$options %||% list()
  columns <- c("Variable1", "Variable2", "N", "Method", "r")
  if (isTRUE(options$p_ci)) {
    columns <- c(columns, "p", "95% CI")
  }
  if (isTRUE(options$significance_levels)) {
    columns <- c(columns, "Sig")
  }
  table[, intersect(columns, names(table)), drop = FALSE]
}

correlation_normality_display_table <- function(result) {
  table <- result$normality_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table[, intersect(c("Variable", "N", "Skewness", "Kurtosis", "Normality"), names(table)), drop = FALSE]
}

correlation_matrix_display_table <- function(result) {
  matrix <- result$correlation_matrix
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    return(NULL)
  }
  table <- as.data.frame(matrix, check.names = FALSE)
  table[] <- lapply(table, function(column) vapply(column, format_decimal3, character(1)))
  data.frame(Variable = rownames(matrix), table, check.names = FALSE)
}

correlation_scatter_plot_id <- function() {
  "correlation_scatter_plot_output"
}

correlation_heatmap_plot_id <- function() {
  "correlation_heatmap_plot_output"
}

correlation_results_ui <- function(result) {
  if (is.null(result)) {
    return(empty_message("Move variables and click Run analysis."))
  }
  main_table <- correlation_display_table(result)
  if (!is.data.frame(main_table) || nrow(main_table) == 0) {
    return(empty_message("No correlation results to show."))
  }
  options <- result$options %||% list()
  tagList(
    div(
      class = "correlation-results regression-results",
      div(
        class = "result-section correlation-result-section regression-result-panel",
        h3("Correlation"),
        coefficient_html_table(main_table),
        if (isTRUE(options$significance_levels)) {
          div(class = "coefficient-note", "* p < .05; ** p < .01; *** p < .001")
        }
      ),
      if (isTRUE(options$normality)) {
        div(
          class = "result-section correlation-result-section regression-result-panel",
          h3("Normality"),
          coefficient_html_table(correlation_normality_display_table(result))
        )
      },
      div(
        class = "result-section correlation-result-section regression-result-panel",
        h3(sprintf("Correlation matrix (%s)", if (identical(result$matrix_method, "spearman")) "Spearman" else "Pearson")),
        coefficient_html_table(correlation_matrix_display_table(result))
      ),
      if (isTRUE(options$scatter_plot)) {
        div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          h3("Scatter plot matrix"),
          plotOutput(correlation_scatter_plot_id(), height = "520px")
        )
      },
      if (isTRUE(options$matrix_plot)) {
        div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          h3("Correlation matrix heatmap"),
          plotOutput(correlation_heatmap_plot_id(), height = "520px")
        )
      }
    )
  )
}

draw_correlation_scatter_plot <- function(result) {
  data <- result$data
  if (!is.data.frame(data) || ncol(data) < 2) {
    plot.new()
    text(0.5, 0.5, "No plot data")
    return(invisible(NULL))
  }
  labels <- unname(result$labels[colnames(data)])
  old_names <- names(data)
  names(data) <- labels
  on.exit(names(data) <- old_names, add = TRUE)
  graphics::pairs(data, pch = 19, col = grDevices::adjustcolor("#2f6f9f", alpha.f = 0.55), gap = 0.35)
  invisible(NULL)
}

draw_correlation_heatmap <- function(result) {
  matrix <- result$correlation_matrix
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    plot.new()
    text(0.5, 0.5, "No matrix data")
    return(invisible(NULL))
  }
  values <- matrix[nrow(matrix):1, , drop = FALSE]
  graphics::par(mar = c(7, 8, 2, 2))
  graphics::image(
    x = seq_len(ncol(values)),
    y = seq_len(nrow(values)),
    z = t(values),
    zlim = c(-1, 1),
    col = grDevices::colorRampPalette(c("#2b6cb0", "#ffffff", "#c2410c"))(101),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  graphics::axis(1, at = seq_len(ncol(values)), labels = colnames(values), las = 2, cex.axis = 0.8)
  graphics::axis(2, at = seq_len(nrow(values)), labels = rownames(values), las = 2, cex.axis = 0.8)
  for (row in seq_len(nrow(values))) {
    for (col in seq_len(ncol(values))) {
      label <- format_decimal3(values[row, col])
      graphics::text(col, row, label, cex = 0.8)
    }
  }
  invisible(NULL)
}
