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
  display_note <- if (isTRUE(result$options$hide_small_loadings %||% TRUE)) {
    "Absolute loadings below .30 are suppressed"
  } else {
    "Absolute loadings of .30 or greater are shown in bold"
  }
  if (identical(result$matrix_type, "covariance")) {
    display_note <- paste(display_note,
      "using standardized loadings; ordering and diagnostic colours also use standardized values, while displayed loadings and h² retain covariance units")
  }
  result_sci_note_text(
    format = c(display_note, "Items are ordered by primary component and absolute loading"),
    abbreviations = "h² = communality; complexity = cross-loading complexity"
  )
}

pca_suitability_note <- function(result) {
  "KMO and Bartlett's test are reported as descriptive diagnostics for whether the variable set has enough shared association for dimension reduction."
}

pca_b5_panel <- function(..., class = "") {
  div(
    class = paste("result-section pca-result-section regression-result-panel", class),
    ...
  )
}

pca_main_table <- function(table) {
  if (is.data.frame(table)) {
    attr(table, "result_table_role") <- "main"
    attr(table, "result_table_language") <- result_main_table_language()
  }
  table
}

pca_appendix_table <- function(table) {
  localized <- result_appendix_localize_table(table)
  if (!is.data.frame(table)) return(localized)
  language <- result_appendix_table_language()
  headers <- c(Check = "check", Component = "component", Eigenvalue = "eigenvalue",
    `Variance %` = "variance_percent", `Cumulative %` = "cumulative_percent", Selected = "selected")
  for (column in intersect(names(table), names(headers))) {
    names(localized)[match(column, names(table))] <- statedu_t(paste0("analysis.pca.", headers[[column]]), language)
  }
  if ("Check" %in% names(table)) {
    selected <- as.character(table$Check) == "Bartlett's test of sphericity"
    localized[[match("Check", names(table))]][selected] <- statedu_t("analysis.pca.bartlett", language)
  }
  if ("Selected" %in% names(table)) {
    selected <- as.character(table$Selected) == "Yes"
    localized[[match("Selected", names(table))]][selected] <- statedu_t("analysis.pca.yes", language)
  }
  result_appendix_preserve_data(localized, table)
}

pca_apply_column_widths <- function(table, widths = NULL) {
  factor_analysis_apply_column_widths(table, widths)
}

pca_overview_table_ui <- function(result) {
  table <- result$overview
  if (!is.data.frame(table) || ncol(table) == 0) return(NULL)
  base_widths <- c(
    N = 9,
    Variables = 13,
    Components = 13,
    Matrix = 17,
    Rotation = 13,
    Criterion = 22
  )
  widths <- unname(base_widths[names(table)])
  widths[!is.finite(widths)] <- 100 / ncol(table)
  widths <- widths / sum(widths) * 100
  pca_apply_column_widths(table, widths)
}

pca_loading_table_ui <- function(table, result) {
  if (!is.data.frame(table) || ncol(table) == 0) return(table)
  component_names <- factor_analysis_order_factor_names(colnames(result$loadings %||% matrix(nrow = 0, ncol = 0)))
  columns <- names(table)
  first_width <- if (ncol(table) >= 7L) 28 else 32
  fixed <- rep(NA_real_, length(columns))
  fixed[columns == "Variable"] <- first_width
  fixed[factor_analysis_column_key(columns) %in% c("h2", "communality") | grepl("^h", tolower(columns))] <- 8
  fixed[factor_analysis_column_key(columns) == "complexity"] <- 10
  component_indices <- which(columns %in% component_names)
  remaining <- 100 - sum(fixed[is.finite(fixed)])
  if (length(component_indices) > 0) {
    fixed[component_indices] <- remaining / length(component_indices)
  }
  fixed[!is.finite(fixed)] <- max(7, remaining / max(1, sum(!is.finite(fixed))))
  pca_apply_column_widths(table, fixed / sum(fixed) * 100)
}

pca_results_ui <- function(result, report_mode = FALSE, plot_renderer = plot_data_uri) {
  if (is.null(result)) {
    return(NULL)
  }
  options <- result$options %||% list()
  tagList(
    div(
      class = "pca-results regression-results",
      pca_b5_panel(
        h3("Principal component analysis"),
        coefficient_html_table(
          pca_main_table(pca_overview_table_ui(result)),
          compact = TRUE,
          compact_font_size = 12,
          compact_width = 62,
          compact_first_width = 44,
          compact_min_width = 320,
          table_role = "main"
        )
      ),
      pca_b5_panel(
        h3("Component loadings"),
        coefficient_html_table(
          pca_main_table(pca_loading_table_ui(result$loadings_table, result)),
          compact = TRUE,
          compact_font_size = 12,
          compact_width = 48,
          compact_first_width = 138,
          compact_min_width = 320,
          note_line = pca_loading_note(result),
          table_role = "main"
        )
      ),
      analysis_warning_section(result$warnings, class = "result-section pca-result-section regression-result-panel"),
      pca_b5_panel(
        h3(result_appendix_ui_text("Suitability")),
        coefficient_html_table(
          pca_appendix_table(result$suitability$overview),
          note_line = result_appendix_ui_text(pca_suitability_note(result)),
          table_role = "appendix"
        )
      ),
      if (is.data.frame(result$variance_table) && nrow(result$variance_table) > 0) {
        pca_b5_panel(
          h3("Variance explained"),
          coefficient_html_table(
            pca_main_table(pca_apply_column_widths(result$variance_table)),
            compact = TRUE,
            compact_font_size = 12,
            compact_width = 58,
            compact_first_width = 104,
            compact_min_width = 320,
            table_role = "main"
          )
        )
      },
      if (is.data.frame(result$component_correlation_table) && nrow(result$component_correlation_table) > 0) {
        pca_b5_panel(
          h3("Component correlations"),
          coefficient_html_table(
            pca_main_table(pca_apply_column_widths(result$component_correlation_table)),
            compact = TRUE,
            compact_font_size = 12,
            compact_width = 54,
            compact_first_width = 82,
            compact_min_width = 320,
            table_role = "main"
          )
        )
      },
      if (isTRUE(options$scree_plot)) {
        pca_b5_panel(
          class = "pca-plot-section",
          h3("Scree plot"),
          if (isTRUE(report_mode)) {
            tags$img(
              class = "analysis-plot-image", alt = "Scree plot", width = 900, height = 620,
              src = plot_renderer(draw_pca_scree_plot, result, width = 900, height = 620, res = 120),
              style = "max-width:900px;width:100%;height:auto;"
            )
          } else {
            plotOutput(pca_scree_plot_id(), width = pca_plot_size(result), height = "520px")
          }
        )
      },
      if (isTRUE(options$biplot)) {
        pca_b5_panel(
          class = "pca-plot-section",
          h3("Biplot"),
          if (isTRUE(report_mode)) {
            tags$img(
              class = "analysis-plot-image", alt = "Biplot", width = 900, height = 720,
              src = plot_renderer(draw_pca_component_plot, result, width = 900, height = 720, res = 120),
              style = "max-width:900px;width:100%;height:auto;"
            )
          } else {
            plotOutput(pca_component_plot_id(), width = pca_plot_size(result), height = "620px")
          }
        )
      },
      pca_b5_panel(
        h3(result_appendix_ui_text("Eigenvalues")),
        coefficient_html_table(
          pca_appendix_table(pca_apply_column_widths(result$eigen_table)),
          compact = TRUE,
          compact_font_size = 12,
          compact_width = 56,
          compact_first_width = 74,
          compact_min_width = 320,
          table_role = "appendix"
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

pca_biplot_label_positions <- function(x, y, labels, cex = 0.82) {
  bounds <- graphics::par("usr")
  dx <- diff(bounds[1:2]); dy <- diff(bounds[3:4])
  widths <- graphics::strwidth(labels, cex = cex) + dx * 0.012
  heights <- graphics::strheight(labels, cex = cex) + dy * 0.016
  placed <- data.frame(x = x, y = y, width = widths, height = heights)
  # Search nearest free positions in device-aware text boxes. Stable ordering
  # keeps redraws deterministic, and leaders preserve each arrow association.
  for (i in seq_along(labels)) {
    xs <- seq(bounds[1] + widths[i] / 2, bounds[2] - widths[i] / 2, length.out = 60)
    ys <- seq(bounds[3] + heights[i] / 2, bounds[4] - heights[i] / 2, length.out = 80)
    candidates <- expand.grid(x = xs, y = ys)
    distance <- ((candidates$x - x[i]) / dx)^2 + ((candidates$y - y[i]) / dy)^2
    overlap <- integer(nrow(candidates))
    for (j in seq_along(labels)) {
      overlap <- overlap + (
        abs(candidates$x - x[j]) < widths[i] / 2 &
        abs(candidates$y - y[j]) < heights[i] / 2
      )
    }
    if (i > 1L) {
      for (j in seq_len(i - 1L)) {
        overlap <- overlap + (
          abs(candidates$x - placed$x[j]) < (widths[i] + widths[j]) / 2 &
          abs(candidates$y - placed$y[j]) < (heights[i] + heights[j]) / 2
        )
      }
    }
    best <- order(overlap, distance)[1L]
    placed$x[i] <- candidates$x[best]
    placed$y[i] <- candidates$y[best]
  }
  placed
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
  finite_scores <- abs(c(score_x, score_y))
  finite_scores <- finite_scores[is.finite(finite_scores)]
  score_limit <- if (length(finite_scores) > 0) max(finite_scores) else max_abs
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
  positions <- pca_biplot_label_positions(x * arrow_scale, y * arrow_scale, labels)
  graphics::segments(x * arrow_scale, y * arrow_scale, positions$x, positions$y, col = "#94a3b8", lwd = 0.7)
  graphics::text(positions$x, positions$y, labels = labels, cex = 0.82, col = "#15233a")
  graphics::box(col = "#1f2937")
  invisible(positions)
}
