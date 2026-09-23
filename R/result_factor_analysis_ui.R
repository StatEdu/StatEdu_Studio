# Factor analysis result UI and plots.

factor_analysis_scree_plot_id <- function() {
  "factor_scree_plot_output"
}

factor_analysis_plot_size <- function(result, base = 640, per_variable = 18, max_size = 980) {
  count <- length(result$variables %||% character(0))
  paste0(min(max_size, max(base, 260 + count * per_variable)), "px")
}

factor_analysis_has_ordered_variables <- function(result) {
  measurements <- factor_analysis_measurements_for(result$variables %||% character(0), result$variable_info)
  any(measurements == "ordered", na.rm = TRUE)
}

factor_analysis_negative_primary_note <- function(result, cutoff = 0.30) {
  loadings <- result$loadings
  if (!is.matrix(loadings) || nrow(loadings) == 0) {
    return("")
  }
  loading_abs <- abs(loadings)
  primary_factor <- max.col(loading_abs, ties.method = "first")
  primary_loading <- loadings[cbind(seq_len(nrow(loadings)), primary_factor)]
  rows <- which(is.finite(primary_loading) & primary_loading <= -cutoff)
  if (length(rows) == 0) {
    return("")
  }
  factors <- colnames(loadings)
  items <- vapply(rows, function(row_index) {
    variable <- rownames(loadings)[[row_index]]
    sprintf(
      "%s (%s=%s)",
      result$display_names[[variable]] %||% variable,
      factors[[primary_factor[[row_index]]]],
      format_decimal3(primary_loading[[row_index]])
    )
  }, character(1))
  paste0("Potential reverse-keyed items based on negative primary loadings: ", paste(items, collapse = ", "), ".")
}

factor_analysis_factor_selection_note <- function(result) {
  if (identical(result$criterion %||% "", "eigen")) {
    return("Eigenvalue >= 1.0 and the scree plot are screening aids; consider parallel analysis or theory when deciding the final number of factors.")
  }
  "The fixed factor count should be checked against the scree plot, interpretability, and theory; parallel analysis can be useful as an additional check."
}

factor_analysis_note <- function(result) {
  loading_filter_note <- if (isTRUE(result$options$hide_small_loadings %||% TRUE)) {
    "Absolute loadings below .30 are suppressed"
  } else {
    "Absolute loadings of .30 or greater are shown in bold"
  }
  sort_note <- if (isTRUE(result$options$sort_loadings %||% TRUE)) {
    "Items are ordered by primary factor and absolute loading"
  } else {
    NULL
  }
  ordinal_note <- if (isTRUE(factor_analysis_has_ordered_variables(result))) {
    if (identical(result$matrix_type %||% "pearson", "polychoric")) {
      "Ordinal variables were analyzed with a polychoric correlation matrix"
    } else {
      "Ordinal variables were analyzed with Pearson correlations"
    }
  } else {
    ""
  }
  oblique_note <- if (is.matrix(result$fit$Phi)) {
    "Pattern coefficients are reported for the oblique rotation"
  } else {
    ""
  }
  result_sci_note_text(
    format = c(loading_filter_note, sort_note),
    abbreviations = "h² = communality; complexity = cross-loading complexity",
    estimation = c(ordinal_note, oblique_note),
    symbol = c(factor_analysis_negative_primary_note(result), factor_analysis_reliability_note(result))
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

factor_analysis_structure_note <- function(result) {
  result_sci_note_text(
    abbreviations = "Structure coefficients are item-factor correlations",
    estimation = "The pattern matrix shows unique factor contributions"
  )
}

factor_analysis_reliability_note <- function(result) {
  reliability <- result$subfactor_reliability
  if (is.null(reliability)) {
    return("")
  }
  reliability_n <- c(
    if (is.list(reliability$total)) sprintf("Total N=%s", reliability$total$overview$N[[1]] %||% ""),
    vapply(reliability$factors %||% list(), function(item) {
      sprintf("%s N=%s", item$subfactor %||% "", item$overview$N[[1]] %||% "")
    }, character(1))
  )
  reliability_n <- reliability_n[nzchar(reliability_n)]
  n_note <- if (length(reliability_n) > 0) {
    paste0(" Sample sizes: ", paste(reliability_n, collapse = "; "), ".")
  } else {
    ""
  }
  skipped <- reliability$skipped
  skipped_note <- if (is.data.frame(skipped) && nrow(skipped) > 0) {
    paste0(
      " Skipped subfactors: ",
      paste(sprintf("%s (%s)", skipped$Subfactor, skipped$Reason), collapse = "; "),
      "."
    )
  } else {
    ""
  }
  item_issues <- reliability$item_issues
  issue_note <- if (is.data.frame(item_issues) && nrow(item_issues) > 0) {
    paste0(
      " Item issues: ",
      paste(sprintf("%s/%s: %s", item_issues$Subfactor, item_issues$Item, item_issues$Problem), collapse = "; "),
      "."
    )
  } else {
    ""
  }
  paste0(
    "Subfactor reliability used items with absolute primary loadings >= .30 and complete cases.",
    n_note,
    skipped_note,
    issue_note
  )
}

factor_analysis_b5_panel <- function(..., class = "") {
  div(
    class = paste("result-section factor-analysis-result-section regression-result-panel", class),
    ...
  )
}

factor_analysis_main_table <- function(table) {
  if (is.data.frame(table)) {
    attr(table, "result_table_role") <- "main"
    attr(table, "result_table_language") <- result_main_table_language()
  }
  table
}

factor_analysis_appendix_table <- function(table) {
  result_appendix_localize_table(table)
}

factor_analysis_apply_column_widths <- function(table, widths = NULL) {
  if (!is.data.frame(table) || ncol(table) == 0) {
    return(table)
  }
  if (is.null(widths)) {
    widths <- rep(100 / ncol(table), ncol(table))
  }
  if (length(widths) != ncol(table)) {
    widths <- rep(100 / ncol(table), ncol(table))
  }
  attr(table, "compact_column_widths") <- as.numeric(widths)
  table
}

factor_analysis_overview_table_ui <- function(result) {
  table <- result$overview
  if (!is.data.frame(table) || ncol(table) == 0) return(NULL)
  base_widths <- c(
    N = 8,
    Variables = 12,
    Factors = 10,
    Matrix = 14,
    Method = 16,
    Rotation = 12,
    Criterion = 16,
    `Normality check` = 16,
    Normality = 12
  )
  widths <- unname(base_widths[names(table)])
  widths[!is.finite(widths)] <- 100 / ncol(table)
  widths <- widths / sum(widths) * 100
  factor_analysis_apply_column_widths(table, widths)
}

factor_analysis_loading_table_ui <- function(table, result) {
  if (!is.data.frame(table) || ncol(table) == 0) return(table)
  factor_names <- colnames(result$loadings %||% matrix(nrow = 0, ncol = 0))
  rename_map <- c(
    Reliability = "Rel.",
    `Reliability if deleted` = "Rel. if deleted",
    `Item-total` = "Item-total r"
  )
  cell_styles <- attr(table, "cell_styles", exact = TRUE)
  if (is.data.frame(cell_styles) && "column" %in% names(cell_styles)) {
    matched <- match(cell_styles$column, names(rename_map))
    cell_styles$column[!is.na(matched)] <- unname(rename_map[matched[!is.na(matched)]])
    attr(table, "cell_styles") <- cell_styles
  }
  bold_cells <- attr(table, "bold_cells", exact = TRUE)
  if (is.data.frame(bold_cells) && "column" %in% names(bold_cells)) {
    matched <- match(bold_cells$column, names(rename_map))
    bold_cells$column[!is.na(matched)] <- unname(rename_map[matched[!is.na(matched)]])
    attr(table, "bold_cells") <- bold_cells
  }
  names(table)[names(table) == "Reliability"] <- "Rel."
  names(table)[names(table) == "Reliability if deleted"] <- "Rel. if deleted"
  names(table)[names(table) == "Item-total"] <- "Item-total r"
  columns <- names(table)
  first_width <- if (ncol(table) >= 8L) 24 else 30
  fixed <- rep(NA_real_, length(columns))
  fixed[columns == "Variable"] <- first_width
  fixed[factor_analysis_column_key(columns) %in% c("h2", "communality") | grepl("^h", tolower(columns))] <- 7
  fixed[factor_analysis_column_key(columns) == "complexity"] <- 9
  fixed[columns == "Rel."] <- 8
  fixed[columns == "Rel. if deleted"] <- 13
  fixed[columns == "Item-total r"] <- 11
  factor_indices <- which(columns %in% factor_names)
  remaining <- 100 - sum(fixed[is.finite(fixed)])
  if (length(factor_indices) > 0) {
    fixed[factor_indices] <- remaining / length(factor_indices)
  }
  fixed[!is.finite(fixed)] <- max(6, remaining / max(1, sum(!is.finite(fixed))))
  factor_analysis_apply_column_widths(table, fixed / sum(fixed) * 100)
}

factor_analysis_results_ui <- function(result, report_mode = FALSE, plot_renderer = plot_data_uri) {
  if (is.null(result)) {
    return(NULL)
  }
  tagList(
    div(
      class = "factor-analysis-results regression-results",
      factor_analysis_b5_panel(
        h3("Factor analysis"),
        coefficient_html_table(
          factor_analysis_main_table(factor_analysis_overview_table_ui(result)),
          compact = TRUE,
          compact_font_size = 12,
          compact_width = 62,
          compact_first_width = 44,
          compact_min_width = 320,
          table_role = "main"
        )
      ),
      factor_analysis_b5_panel(
        h3("Pattern / loading matrix"),
        coefficient_html_table(
          factor_analysis_main_table(factor_analysis_loading_table_ui(result$loadings_table, result)),
          compact = TRUE,
          compact_font_size = 12,
          compact_width = 48,
          compact_first_width = 138,
          compact_min_width = 320,
          note_line = factor_analysis_note(result),
          table_role = "main"
        )
      ),
      if (is.data.frame(result$structure_table) && nrow(result$structure_table) > 0) {
        factor_analysis_b5_panel(
          h3("Structure matrix"),
          coefficient_html_table(
            factor_analysis_main_table(factor_analysis_loading_table_ui(result$structure_table, result)),
            compact = TRUE,
            compact_font_size = 12,
            compact_width = 48,
            compact_first_width = 138,
            compact_min_width = 320,
            note_line = factor_analysis_structure_note(result),
            table_role = "main"
          )
        )
      },
      analysis_warning_section(result$warnings, class = "result-section factor-analysis-result-section regression-result-panel"),
      factor_analysis_b5_panel(
        h3(result_appendix_ui_text("Suitability")),
        coefficient_html_table(
          factor_analysis_appendix_table(result$suitability$overview),
          note_line = result_appendix_ui_text(factor_analysis_suitability_note(result)),
          table_role = "appendix"
        )
      ),
      if (is.data.frame(result$normality_table) && nrow(result$normality_table) > 0) {
        factor_analysis_b5_panel(
          h3(result_appendix_ui_text("Normality")),
          coefficient_html_table(
            factor_analysis_appendix_table(result$normality_table),
            note_line = result_appendix_ui_text(factor_analysis_normality_note(result)),
            table_role = "appendix"
          )
        )
      },
      if (is.data.frame(result$variance_table) && nrow(result$variance_table) > 0) {
        factor_analysis_b5_panel(
          h3("Variance explained"),
          coefficient_html_table(
            factor_analysis_main_table(factor_analysis_apply_column_widths(result$variance_table)),
            compact = TRUE,
            compact_font_size = 12,
            compact_width = 58,
            compact_first_width = 104,
            compact_min_width = 320,
            table_role = "main"
          )
        )
      },
      if (is.data.frame(result$factor_correlation_table) && nrow(result$factor_correlation_table) > 0) {
        factor_analysis_b5_panel(
          h3("Factor correlations"),
          coefficient_html_table(
            factor_analysis_main_table(factor_analysis_apply_column_widths(result$factor_correlation_table)),
            compact = TRUE,
            compact_font_size = 12,
            compact_width = 54,
            compact_first_width = 82,
            compact_min_width = 320,
            table_role = "main"
          )
        )
      },
      factor_analysis_b5_panel(
        class = "factor-analysis-plot-section",
        h3("Scree plot"),
        if (isTRUE(report_mode)) {
          tags$img(
            src = plot_renderer(draw_factor_analysis_scree_plot, result, width = 900, height = 620, res = 120),
            class = "analysis-plot-image", alt = "Scree plot", width = 900, height = 620,
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
      factor_analysis_b5_panel(
        h3(result_appendix_ui_text("Eigenvalues")),
        coefficient_html_table(
          factor_analysis_appendix_table(factor_analysis_apply_column_widths(result$eigen_table)),
          compact = TRUE,
          compact_font_size = 12,
          compact_width = 56,
          compact_first_width = 74,
          compact_min_width = 320,
          note_line = result_appendix_ui_text(factor_analysis_factor_selection_note(result)),
          table_role = "appendix"
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
