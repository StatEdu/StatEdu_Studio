# Principal component analysis result UI and plots.

pca_scree_plot_id <- function() {
  "pca_scree_plot_output"
}

pca_component_plot_id <- function() {
  "pca_component_plot_output"
}

pca_plot_size <- function(result, base = 640, per_variable = 18, max_size = 980) {
  count <- length(result$variables %||% character(0))
  paste0(min(max_size, max(base, 260 + count * per_variable)), "px")
}

pca_loading_note <- function(result) {
  if (isTRUE(result$options$hide_small_loadings %||% TRUE)) {
    "Loadings with absolute values below .30 are hidden. Communality is the variance explained by the retained components."
  } else {
    "All loadings are shown; loadings with absolute values of .30 or higher are bold. Communality is the variance explained by the retained components."
  }
}

pca_suitability_note <- function(result) {
  "KMO and Bartlett's test are reported as descriptive diagnostics for whether the variable set has enough shared association for dimension reduction."
}

pca_results_ui <- function(result, report_mode = FALSE) {
  if (is.null(result)) {
    return(NULL)
  }
  options <- result$options %||% list()
  tagList(
    div(
      class = "pca-results regression-results",
      div(
        class = "result-section pca-result-section regression-result-panel",
        h3("Principal component analysis"),
        coefficient_html_table(result$overview)
      ),
      div(
        class = "result-section pca-result-section regression-result-panel",
        h3("Suitability"),
        coefficient_html_table(result$suitability$overview, note_line = pca_suitability_note(result))
      ),
      div(
        class = "result-section pca-result-section regression-result-panel landscape-table-panel",
        h3("Component loadings"),
        coefficient_html_table(
          result$loadings_table,
          compact = TRUE,
          compact_font_size = 13,
          compact_width = 70,
          compact_first_width = 150,
          compact_min_width = 520,
          note_line = pca_loading_note(result)
        )
      ),
      if (is.data.frame(result$variance_table) && nrow(result$variance_table) > 0) {
        div(
          class = "result-section pca-result-section regression-result-panel landscape-table-panel",
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
      if (is.data.frame(result$component_correlation_table) && nrow(result$component_correlation_table) > 0) {
        div(
          class = "result-section pca-result-section regression-result-panel landscape-table-panel",
          h3("Component correlations"),
          coefficient_html_table(
            result$component_correlation_table,
            compact = TRUE,
            compact_font_size = 13,
            compact_width = 76,
            compact_first_width = 100,
            compact_min_width = 420
          )
        )
      },
      if (isTRUE(options$scree_plot)) {
        div(
          class = "result-section pca-result-section pca-plot-section regression-result-panel",
          h3("Scree plot"),
          if (isTRUE(report_mode)) {
            tags$img(
              src = plot_data_uri(draw_pca_scree_plot, result, width = 900, height = 620, res = 120),
              style = "max-width:900px;width:100%;height:auto;"
            )
          } else {
            plotOutput(pca_scree_plot_id(), width = pca_plot_size(result), height = "520px")
          }
        )
      },
      if (isTRUE(options$biplot)) {
        div(
          class = "result-section pca-result-section pca-plot-section regression-result-panel",
          h3("Biplot"),
          if (isTRUE(report_mode)) {
            tags$img(
              src = plot_data_uri(draw_pca_component_plot, result, width = 900, height = 720, res = 120),
              style = "max-width:900px;width:100%;height:auto;"
            )
          } else {
            plotOutput(pca_component_plot_id(), width = pca_plot_size(result), height = "620px")
          }
        )
      },
      div(
        class = "result-section pca-result-section regression-result-panel landscape-table-panel",
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

draw_pca_scree_plot <- function(result) {
  eigenvalues <- as.numeric(result$eigenvalues %||% numeric(0))
  if (length(eigenvalues) == 0 || all(!is.finite(eigenvalues))) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No eigenvalues")
    return(invisible(NULL))
  }
  components <- seq_along(eigenvalues)
  selected <- components <= as.integer(result$n_components %||% 1L)
  y_max <- max(1.2, eigenvalues, na.rm = TRUE) * 1.08
  graphics::par(mar = c(4.5, 4.5, 2.5, 1.5), cex = 1.12)
  graphics::plot(
    components,
    eigenvalues,
    type = "b",
    pch = 16,
    lwd = 1.6,
    col = "#1f6fa8",
    xlab = "Component number",
    ylab = "Eigenvalue",
    ylim = c(0, y_max),
    xaxt = "n",
    main = ""
  )
  graphics::axis(1, at = components)
  graphics::abline(h = 1, lty = 2, col = "#9a3412", lwd = 1.2)
  graphics::points(components[selected], eigenvalues[selected], pch = 16, cex = 1.35, col = "#c2410c")
  graphics::box(col = "#1f2937")
  invisible(NULL)
}

draw_pca_component_plot <- function(result) {
  loadings <- result$loadings
  if (!is.matrix(loadings) || ncol(loadings) < 2) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Biplot requires at least two retained components.", cex = 0.95)
    return(invisible(NULL))
  }
  scores <- result$scores
  score_x <- numeric(0)
  score_y <- numeric(0)
  if (is.data.frame(scores) && ncol(scores) >= 2) {
    score_x <- suppressWarnings(as.numeric(scores[[1]]))
    score_y <- suppressWarnings(as.numeric(scores[[2]]))
  }
  x <- loadings[, 1]
  y <- loadings[, 2]
  labels <- result$display_names[rownames(loadings)]
  max_abs <- max(abs(c(x, y)), na.rm = TRUE)
  if (!is.finite(max_abs) || max_abs <= 0) {
    max_abs <- 1
  }
  score_limit <- max(abs(c(score_x, score_y)), na.rm = TRUE)
  if (!is.finite(score_limit) || score_limit <= 0) {
    score_limit <- max_abs
  }
  limit <- max(1, score_limit * 1.1)
  arrow_scale <- limit / max_abs * 0.78
  graphics::par(mar = c(4.8, 4.8, 2.5, 1.5), cex = 1.08)
  graphics::plot(
    score_x,
    score_y,
    xlim = c(-limit, limit),
    ylim = c(-limit, limit),
    xlab = colnames(loadings)[[1]],
    ylab = colnames(loadings)[[2]],
    pch = 16,
    cex = 0.72,
    col = adjustcolor("#607d9b", alpha.f = 0.45),
    main = "",
    asp = 1
  )
  graphics::abline(h = 0, v = 0, col = "#94a3b8", lty = 2)
  graphics::arrows(0, 0, x * arrow_scale, y * arrow_scale, length = 0.08, lwd = 1.4, col = "#0fa3a3")
  graphics::points(x * arrow_scale, y * arrow_scale, pch = 16, cex = 0.9, col = "#0b7285")
  graphics::text(x * arrow_scale, y * arrow_scale, labels = labels, pos = 3, cex = 0.82, col = "#15233a")
  graphics::box(col = "#1f2937")
  invisible(NULL)
}
