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
    "Loadings with absolute values below .30 are hidden."
  } else {
    "All loadings are shown; loadings with absolute values of .30 or higher are bold."
  }
  highlight_note <- if (isTRUE(result$options$highlight_problem_values %||% TRUE)) {
    "Problem values are highlighted with a red background: primary loading < .30, cross-loading >= .30, h² < .30, h² > .90, or Complexity >= 2."
  } else {
    "Problem value highlighting is off."
  }
  sort_note <- if (isTRUE(result$options$sort_loadings %||% TRUE)) {
    "Variables are sorted by primary factor and descending absolute loading."
  } else {
    "Variables are shown in the selected input order."
  }
  ordinal_note <- if (isTRUE(factor_analysis_has_ordered_variables(result))) {
    if (identical(result$matrix_type %||% "pearson", "polychoric")) {
      "Ordinal variables were analyzed with a polychoric correlation matrix."
    } else {
      "Ordinal variables were analyzed with Pearson correlations; polychoric correlation is recommended for ordinal item sets."
    }
  } else {
    ""
  }
  oblique_note <- if (is.matrix(result$fit$Phi)) {
    "For oblique rotation, the pattern matrix shows unique factor contributions after accounting for factor correlations; the structure matrix shows variable-factor correlations."
  } else {
    ""
  }
  notes <- c(
    loading_filter_note,
    sort_note,
    "h² is communality, and complexity summarizes cross-loading pattern. Eigenvalue, variance %, and cumulative variance % are shown at the bottom of the loading matrix.",
    ordinal_note,
    factor_analysis_factor_selection_note(result),
    factor_analysis_negative_primary_note(result),
    oblique_note,
    factor_analysis_reliability_note(result),
    highlight_note
  )
  paste(notes[nzchar(notes)], collapse = " ")
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
  "Structure coefficients are variable-factor correlations. They are shown for oblique rotation because factors are allowed to correlate; compare them with the pattern matrix when interpreting cross-loadings."
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
    paste0(" Reliability coefficients use complete cases within each item set (", paste(reliability_n, collapse = "; "), ").")
  } else {
    " Reliability coefficients use complete cases within each item set."
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
    "Items are assigned to the subfactor with the largest absolute loading; items with primary loading below .30 are not included in subfactor reliability.",
    n_note,
    skipped_note,
    issue_note
  )
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
      analysis_warning_section(result$warnings, class = "result-section factor-analysis-result-section regression-result-panel"),
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
      if (is.data.frame(result$structure_table) && nrow(result$structure_table) > 0) {
        div(
          class = "result-section factor-analysis-result-section regression-result-panel landscape-table-panel",
          h3("Structure matrix"),
          coefficient_html_table(
            result$structure_table,
            compact = TRUE,
            compact_font_size = 13,
            compact_width = 70,
            compact_first_width = 150,
            compact_min_width = 520,
            note_line = factor_analysis_structure_note(result)
          )
        )
      },
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
