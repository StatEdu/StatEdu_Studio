# Correlation result UI and plots.

correlation_normality_display_table <- function(result) {
  table <- result$normality_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table[, intersect(c("Variable", "N", "Skewness", "Kurtosis", "Normality"), names(table)), drop = FALSE]
}

correlation_lower_matrix_display_table <- function(
  matrix,
  formatter = format_decimal3,
  p_matrix = NULL,
  significance_levels = FALSE
) {
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    return(NULL)
  }
  out <- matrix("", nrow = nrow(matrix), ncol = ncol(matrix))
  for (row in seq_len(nrow(matrix))) {
    for (col in seq_len(ncol(matrix))) {
      if (row > col && is.finite(matrix[row, col])) {
        stars <- if (
          isTRUE(significance_levels) &&
            is.matrix(p_matrix) &&
            row <= nrow(p_matrix) &&
            col <= ncol(p_matrix)
        ) {
          correlation_sig(p_matrix[row, col])
        } else {
          ""
        }
        out[row, col] <- paste0(formatter(matrix[row, col]), stars)
      }
    }
  }
  table <- as.data.frame(out, check.names = FALSE)
  names(table) <- colnames(matrix)
  data.frame(Variable = rownames(matrix), table, check.names = FALSE)
}

correlation_matrix_display_table <- function(result, source = NULL) {
  source <- source %||% result
  options <- result$options %||% list()
  correlation_lower_matrix_display_table(
    source$correlation_matrix,
    format_decimal3,
    p_matrix = source$p_matrix,
    significance_levels = isTRUE(options$significance_levels)
  )
}

correlation_p_matrix_display_table <- function(result, source = NULL) {
  source <- source %||% result
  matrix <- source$p_matrix
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    return(NULL)
  }
  ci_matrix <- source$ci_matrix
  out <- matrix("", nrow = nrow(matrix), ncol = ncol(matrix))
  for (row in seq_len(nrow(matrix))) {
    for (col in seq_len(ncol(matrix))) {
      if (row > col) {
        ci_text <- if (is.matrix(ci_matrix)) ci_matrix[row, col] %||% "" else ""
        p_text <- if (is.finite(matrix[row, col])) format_p(matrix[row, col]) else ""
        out[row, col] <- paste(ci_text, p_text, sep = "\n")
      }
    }
  }
  table <- as.data.frame(out, check.names = FALSE)
  names(table) <- colnames(matrix)
  data.frame(Variable = rownames(matrix), table, check.names = FALSE)
}

correlation_method_matrix_display_table <- function(result, source = NULL) {
  source <- source %||% result
  matrix <- source$method_matrix
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    return(NULL)
  }
  out <- matrix("", nrow = nrow(matrix), ncol = ncol(matrix))
  for (row in seq_len(nrow(matrix))) {
    for (col in seq_len(ncol(matrix))) {
      if (row > col) {
        out[row, col] <- matrix[row, col] %||% ""
      }
    }
  }
  table <- as.data.frame(out, check.names = FALSE)
  names(table) <- colnames(matrix)
  data.frame(Variable = rownames(matrix), table, check.names = FALSE)
}

correlation_reason_display_table <- function(source) {
  table <- source$pairwise_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table[, intersect(c("Variable1", "Variable2", "Method", "Reason"), names(table)), drop = FALSE]
}

correlation_matrix_set_ui <- function(result, source = NULL, title_prefix = "") {
  source <- source %||% result
  options <- result$options %||% list()
  main_table <- correlation_matrix_display_table(result, source)
  p_table <- correlation_p_matrix_display_table(result, source)
  method_table <- correlation_method_matrix_display_table(result, source)
  reason_table <- correlation_reason_display_table(source)
  tagList(
    div(
      class = "result-section correlation-result-section regression-result-panel",
      h3(paste0(title_prefix, "Correlation / association coefficients")),
      coefficient_html_table(main_table, compact = TRUE, compact_font_size = 13),
      if (!nzchar(title_prefix) && isTRUE(options$significance_levels)) {
        div(class = "coefficient-note", "* p < .05; ** p < .01; *** p < .001")
      }
    ),
    if (isTRUE(options$p_ci) && is.data.frame(p_table) && nrow(p_table) > 0) {
      div(
        class = "result-section correlation-result-section regression-result-panel",
        h3(paste0(title_prefix, "p-value & 95% CI")),
        coefficient_html_table(
          p_table,
          compact = TRUE,
          compact_font_size = 12,
          compact_width = 50,
          compact_first_width = 104,
          compact_min_width = 280
        )
      )
    },
    if (is.data.frame(method_table) && nrow(method_table) > 0) {
      div(
        class = "result-section correlation-result-section regression-result-panel",
        h3(paste0(title_prefix, "Methods")),
        coefficient_html_table(
          method_table,
          compact = TRUE,
          compact_font_size = 12,
          compact_width = 50,
          compact_first_width = 104,
          compact_min_width = 280
        )
      )
    },
    if (isTRUE(options$reason) && is.data.frame(reason_table) && nrow(reason_table) > 0) {
      div(
        class = "result-section correlation-result-section regression-result-panel",
        h3(paste0(title_prefix, "Reason")),
        coefficient_html_table(reason_table)
      )
    }
  )
}

correlation_scatter_plot_id <- function() {
  "correlation_scatter_plot_output"
}

correlation_heatmap_plot_id <- function() {
  "correlation_heatmap_plot_output"
}

correlation_plot_height <- function(result, base = 560, per_variable = 28, max_height = 900) {
  count <- length(result$variables %||% character(0))
  paste0(min(max_height, max(base, 180 + count * per_variable)), "px")
}

correlation_square_plot_size <- function(result, base = 640, per_variable = 26, max_size = 860) {
  count <- length(result$variables %||% character(0))
  paste0(min(max_size, max(base, 180 + count * per_variable)), "px")
}

correlation_results_ui <- function(result) {
  if (is.null(result)) {
    return(empty_message("Move variables and click Run analysis."))
  }
  main_table <- correlation_matrix_display_table(result)
  if (!is.data.frame(main_table) || nrow(main_table) == 0) {
    return(empty_message("No correlation results to show."))
  }
  options <- result$options %||% list()
  tagList(
    div(
      class = "correlation-results regression-results",
      correlation_matrix_set_ui(result),
      if (is.list(result$latent)) {
        correlation_matrix_set_ui(result, source = result$latent, title_prefix = "Latent-variable ")
      },
      if (isTRUE(options$normality)) {
        div(
          class = "result-section correlation-result-section regression-result-panel",
          h3("Normality"),
          coefficient_html_table(correlation_normality_display_table(result))
        )
      },
      if (isTRUE(options$scatter_plot)) {
        div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          h3("Scatter plot matrix"),
          plotOutput(
            correlation_scatter_plot_id(),
            width = correlation_square_plot_size(result, base = 1160, per_variable = 46, max_size = 1480),
            height = correlation_square_plot_size(result, base = 1160, per_variable = 46, max_size = 1480)
          )
        )
      },
      if (isTRUE(options$matrix_plot)) {
        div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          h3("Correlation matrix heatmap"),
          plotOutput(
            correlation_heatmap_plot_id(),
            width = correlation_square_plot_size(result, base = 980, per_variable = 46, max_size = 1360),
            height = correlation_square_plot_size(result, base = 980, per_variable = 46, max_size = 1360)
          )
        )
      }
    )
  )
}

draw_correlation_scatter_plot <- function(result) {
  data <- result$data
  measurements <- result$measurements %||% character(0)
  plot_vars <- names(measurements)[measurements %in% c("continuous")]
  plot_vars <- intersect(plot_vars, names(data))
  if (!is.data.frame(data) || length(plot_vars) < 2) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Scatter plot requires at least two continuous variables.", cex = 0.9)
    return(invisible(NULL))
  }
  original_count <- length(plot_vars)
  plot_vars <- head(plot_vars, 12)
  plot_data <- data.frame(lapply(plot_vars, function(name) {
    correlation_analysis_vector(data[[name]], measurements[[name]])
  }), check.names = FALSE)
  names(plot_data) <- unname(result$labels[plot_vars])
  plot_data <- plot_data[, vapply(plot_data, function(values) sum(!is.na(values)) >= 3 && stats::sd(values, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
  if (ncol(plot_data) < 2) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Not enough non-constant variables for a scatter plot matrix.", cex = 0.9)
    return(invisible(NULL))
  }
  n <- ncol(plot_data)
  ranges <- lapply(plot_data, function(values) {
    range(values, na.rm = TRUE)
  })
  pad_range <- function(range_value) {
    span <- diff(range_value)
    if (!is.finite(span) || span == 0) {
      span <- 1
    }
    range_value + c(-1, 1) * span * 0.06
  }
  ranges <- lapply(ranges, pad_range)
  layout_matrix <- matrix(seq_len(n * n), nrow = n, byrow = TRUE)
  graphics::layout(layout_matrix)
  graphics::par(
    oma = c(3, 3, if (original_count > length(plot_vars)) 2.5 else 1, 1),
    mar = c(0.35, 0.35, 0.35, 0.35),
    mgp = c(1.4, 0.35, 0),
    tck = -0.02,
    cex = 1.12
  )
  for (row in seq_len(n)) {
    for (col in seq_len(n)) {
      x <- plot_data[[col]]
      y <- plot_data[[row]]
      if (row < col) {
        graphics::plot.new()
        next
      }
      if (row == col) {
        h <- graphics::hist(x, plot = FALSE)
        graphics::plot(
          NA,
          xlim = ranges[[col]],
          ylim = c(0, max(h$counts, 1)),
          axes = FALSE,
          xlab = "",
          ylab = "",
          frame.plot = TRUE
        )
        graphics::rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], h$counts, col = "#dbeafe", border = "#8aa4c2")
        graphics::text(mean(ranges[[col]]), max(h$counts, 1) * 0.84, names(plot_data)[[col]], cex = 1.22, font = 2, col = "#15233a")
      } else {
        graphics::plot(
          x,
          y,
          xlim = ranges[[col]],
          ylim = ranges[[row]],
          axes = FALSE,
          xlab = "",
          ylab = "",
          pch = 16,
          cex = 0.86,
          col = grDevices::adjustcolor("#1f6fa8", alpha.f = 0.62),
          frame.plot = TRUE
        )
        ok <- stats::complete.cases(x, y)
        if (sum(ok) >= 6 && length(unique(x[ok])) > 2 && length(unique(y[ok])) > 2) {
          fit <- stats::lowess(x[ok], y[ok], f = 0.8)
          graphics::lines(fit, col = "#c2410c", lwd = 1.25)
        }
      }
      if (row == n) {
        graphics::axis(1, labels = FALSE)
      }
      if (col == 1 && row > 1) {
        graphics::axis(2, labels = FALSE)
      }
    }
  }
  if (original_count > length(plot_vars)) {
    graphics::mtext(sprintf("Showing first %s of %s plottable variables.", length(plot_vars), original_count), outer = TRUE, side = 3, cex = 1.18, col = "#52606d")
  }
  invisible(NULL)
}

draw_correlation_heatmap <- function(result) {
  matrix <- result$correlation_matrix
  if (!is.matrix(matrix) || nrow(matrix) == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No matrix data")
    return(invisible(NULL))
  }
  values <- matrix
  diag(values) <- NA_real_
  values <- values[nrow(values):1, , drop = FALSE]
  labels_x <- colnames(matrix)
  labels_y <- rev(rownames(matrix))
  n <- ncol(values)
  graphics::par(mar = c(11, 12, 3, 7), xpd = NA, cex = 1.22)
  palette <- grDevices::colorRampPalette(c("#2b6cb0", "#f7fafc", "#c2410c"))(121)
  graphics::image(
    x = seq_len(ncol(values)),
    y = seq_len(nrow(values)),
    z = t(values),
    zlim = c(-1, 1),
    col = palette,
    axes = FALSE,
    xlab = "",
    ylab = "",
    asp = 1
  )
  graphics::axis(1, at = seq_len(ncol(values)), labels = labels_x, las = 2, cex.axis = if (n > 12) 0.92 else 1.08, tick = FALSE)
  graphics::axis(2, at = seq_len(nrow(values)), labels = labels_y, las = 1, cex.axis = if (n > 12) 0.92 else 1.08, tick = FALSE)
  graphics::box(col = "#1f2937")
  show_values <- n <= 10
  if (show_values) {
    for (row in seq_len(nrow(values))) {
      for (col in seq_len(ncol(values))) {
        if (is.finite(values[row, col])) {
          graphics::text(col, row, format_decimal3(values[row, col]), cex = 0.96, col = "#111827")
        }
      }
    }
  }
  legend_y <- seq(1, nrow(values), length.out = length(palette))
  legend_x <- ncol(values) + 1.1
  graphics::rect(legend_x, legend_y[-length(legend_y)], legend_x + 0.38, legend_y[-1], col = palette[-length(palette)], border = NA)
  graphics::text(legend_x + 0.7, c(1, nrow(values) / 2, nrow(values)), c("-1", "0", "1"), cex = 0.98, adj = 0)
  invisible(NULL)
}
