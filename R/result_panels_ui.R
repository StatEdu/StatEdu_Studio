# Result panel UI builders.

coefficient_result_ui <- function(table, result, show_sr2 = FALSE, show_f2 = FALSE, show_vif = FALSE, output_table_style = "standard") {
  table <- filter_coefficient_export_table(table, show_sr2, show_f2, show_vif)
  if (isTRUE(result$use_bootstrap)) {
    attr(table, "bootstrap_regression") <- TRUE
    keys <- result_column_key(names(table))
    widths <- rep(8, length(keys))
    widths[keys == "term"] <- 36
    widths[keys == "b"] <- 10
    widths[keys %in% c("bootse", "hc3se")] <- 12
    widths[keys %in% c("llci", "ulci")] <- 10
    widths[keys == "bootp"] <- 10
    widths[keys %in% c("sr2", "f2")] <- 8
    widths[keys == "tolerance"] <- 11
    widths[keys == "vif"] <- 8
    attr(table, "compact_column_widths") <- widths / sum(widths, na.rm = TRUE) * 100
  }
  fit_line <- coefficient_fit_line(result)
  stat_lines <- coefficient_stat_lines(result)
  warning_line <- coefficient_vif_warning_line(result)
  note_line <- coefficient_note_line(result, show_vif, show_sr2, show_f2)
  coefficient_html_table(table, fit_line, stat_lines, warning_line, note_line, output_table_style = output_table_style)
}

coefficient_result_block <- function(title, content, landscape = FALSE) {
  div(
    class = paste("regression-result-panel", if (isTRUE(landscape)) "landscape-table-panel" else ""),
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
  table_tag <- tags$table(
    class = "effect-size-reference-table",
    tags$thead(tags$tr(
      tags$th("ES"),
      tags$th("Reference"),
      tags$th("Small"),
      tags$th("Medium"),
      tags$th("Large")
    )),
    tags$tbody(rows)
  )
  note_tag <- if (isTRUE(show_sr2)) {
    tags$div(
      class = "coefficient-note effect-size-reference-note",
      tags$span("Squared semi-partial correlations (sr", tags$sup("2"), ") were examined to estimate the unique variance explained by each predictor (Cohen et al., 2003; Pedhazur, 1997). Values of .01, .09, and .25 were interpreted as small, medium, and large effects, respectively.")
    )
  } else {
    NULL
  }

  tagList(
    div(
      class = "effect-size-reference-panel",
      h4("Effect Size Guidelines"),
      result_table_with_notes(table_tag, note_tag),
      p(
        class = "effect-size-reference-citation",
        "Cohen, J., Cohen, P., West, S. G., & Leona S. Aiken (2003). Applied multiple regression/correlation analysis for the behavioral sciences (3rd ed.). Lawrence Erlbaum Associates."
      ),
      if (isTRUE(show_sr2)) p(
        class = "effect-size-reference-citation",
        "Elazar J. Pedhazur (1997). Multiple regression in behavioral research: Explanation and prediction (3rd ed.). Harcourt Brace."
      )
    )
  )
}

diagnostic_plot_title <- function(dependent_label, result = NULL) {
  title <- sprintf("Diagnostic plots(%s)", dependent_label)
  if (!is.null(result) && isTRUE(result$hierarchical)) {
    step <- result$hierarchical_step %||% ""
    if (!nzchar(step)) {
      step_index <- suppressWarnings(as.integer(result$hierarchical_step_index %||% NA_integer_))
      if (!is.na(step_index)) {
        step <- sprintf("Model %s", step_index)
      }
    }
    if (nzchar(step)) {
      title <- sprintf("%s - %s", title, step)
    }
  }
  title
}

saved_plot_result_block <- function(result, dependent_label) {
  div(
    class = "regression-result-panel diagnostic-plots-section",
    h3(diagnostic_plot_title(dependent_label, result)),
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

plot_result_panel <- function(dependent_label, qq_output_id, homoscedasticity_output_id, result = NULL) {
  div(
    class = "regression-result-panel diagnostic-plots-section",
    h3(diagnostic_plot_title(dependent_label, result)),
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
    class = "regression-result-panel durbin-watson-panel",
    h3("Durbin-Watson"),
    combined_dw_html_table(table)
  )
}

hierarchical_result_dependent_name <- function(result) {
  variables <- all.vars(result$formula)
  if (length(variables) == 0) {
    return("")
  }
  variables[[1]]
}

hierarchical_result_groups <- function(results) {
  if (!is.list(results) || length(results) == 0) {
    return(list())
  }
  keys <- vapply(results, hierarchical_result_dependent_name, character(1))
  groups <- split(results, keys)
  lapply(groups, function(group) {
    order_index <- vapply(group, function(result) {
      as.integer(result$hierarchical_step_index %||% 999L)
    }, integer(1))
    group[order(order_index)]
  })
}

hierarchical_step_label <- function(result, index) {
  step <- result$hierarchical_step %||% ""
  if (nzchar(step)) {
    return(step)
  }
  sprintf("Model %s", index)
}

hierarchical_model_method_label <- function(result) {
  regression_method_label(result)
}

hierarchical_step_header_label <- function(result, index, group = NULL) {
  label <- hierarchical_step_label(result, index)
  if (!is.list(group) || length(group) <= 1L) {
    return(label)
  }
  methods <- vapply(group, hierarchical_model_method_label, character(1))
  if (length(unique(methods)) <= 1L) {
    return(label)
  }
  tags$span(
    style = "white-space:nowrap;",
    label,
    tags$sup(class = "coefficient-footnote-marker", as.character(index))
  )
}

hierarchical_bootstrap_delta_r2_ci <- function(previous, current, conf = .95) {
  previous_r2 <- as.numeric(previous$bootstrap_r_squared %||% numeric(0))
  current_r2 <- as.numeric(current$bootstrap_r_squared %||% numeric(0))
  count <- min(length(previous_r2), length(current_r2))
  if (count == 0) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  delta <- current_r2[seq_len(count)] - previous_r2[seq_len(count)]
  delta <- delta[is.finite(delta)]
  if (length(delta) == 0) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  point <- current$r_squared - previous$r_squared
  ci_method <- current$bootstrap_ci_method %||% previous$bootstrap_ci_method %||% "bias_corrected"
  bootstrap_ci(point, delta, conf = conf, method = ci_method)
}

hierarchical_robust_wald_f_p <- function(previous, current) {
  if (is.null(previous$model) || is.null(current$model)) {
    return(NA_real_)
  }
  current_terms <- setdiff(colnames(stats::model.matrix(current$model)), "(Intercept)")
  previous_terms <- setdiff(colnames(stats::model.matrix(previous$model)), "(Intercept)")
  added_terms <- setdiff(current_terms, previous_terms)
  if (length(added_terms) == 0) {
    return(NA_real_)
  }
  coefficients <- stats::coef(current$model)
  term_index <- match(added_terms, names(coefficients))
  term_index <- term_index[!is.na(term_index)]
  if (length(term_index) == 0) {
    return(NA_real_)
  }
  vcov_matrix <- tryCatch(sandwich::vcovHC(current$model, type = "HC3"), error = function(e) NULL)
  if (is.null(vcov_matrix)) {
    return(NA_real_)
  }
  beta <- coefficients[term_index]
  covariance <- vcov_matrix[term_index, term_index, drop = FALSE]
  inverse_covariance <- tryCatch(solve(covariance), error = function(e) NULL)
  if (is.null(inverse_covariance)) {
    return(NA_real_)
  }
  df1 <- length(beta)
  df2 <- stats::df.residual(current$model)
  statistic <- as.numeric(t(beta) %*% inverse_covariance %*% beta / df1)
  if (!is.finite(statistic) || !is.finite(df1) || !is.finite(df2) || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  stats::pf(statistic, df1, df2, lower.tail = FALSE)
}

hierarchical_delta_line <- function(previous, current) {
  if (is.null(previous) || is.null(current)) {
    return("")
  }
  delta_r2 <- current$r_squared - previous$r_squared
  if (isTRUE(previous$use_bootstrap) || isTRUE(current$use_bootstrap)) {
    ci <- hierarchical_bootstrap_delta_r2_ci(previous, current)
    if (all(is.finite(ci))) {
      return(sprintf(
        "\u0394R\u00B2[95%% CI]=%s[%s, %s]",
        format_decimal3(delta_r2),
        format_decimal3(ci[[1]]),
        format_decimal3(ci[[2]])
      ))
    }
    return(sprintf("\u0394R\u00B2[95%% CI]=%s[pending]", format_decimal3(delta_r2)))
  }
  if (isTRUE(previous$use_hc3) || isTRUE(current$use_hc3)) {
    robust_p <- hierarchical_robust_wald_f_p(previous, current)
    return(sprintf(
      "\u0394R\u00B2(Robust Wald F p)=%s(%s)",
      format_decimal3(delta_r2),
      format_p(robust_p)
    ))
  }
  df1 <- current$f_df1 - previous$f_df1
  df2 <- current$f_df2
  if (!is.finite(delta_r2) || !is.finite(df1) || !is.finite(df2) || df1 <= 0 || df2 <= 0) {
    return(sprintf("\u0394R\u00B2(p)=%s", format_decimal3(delta_r2)))
  }
  f_change <- (delta_r2 / df1) / ((1 - current$r_squared) / df2)
  p_change <- stats::pf(f_change, df1, df2, lower.tail = FALSE)
  sprintf(
    "\u0394R\u00B2(p)=%s(%s)",
    format_decimal3(delta_r2),
    format_p(p_change)
  )
}

hierarchical_delta_footer_label <- function(group) {
  if (any(vapply(group, function(result) isTRUE(result$use_bootstrap), logical(1)))) {
    return("\u0394R\u00B2(95% CI)")
  }
  if (any(vapply(group, function(result) isTRUE(result$use_hc3), logical(1)))) {
    return("\u0394R\u00B2(p)")
  }
  "\u0394R\u00B2(p)"
}

hierarchical_summary_values <- function(group) {
  values <- lapply(seq_along(group), function(index) {
    result <- group[[index]]
    previous <- if (index > 1) group[[index - 1]] else NULL
    residual_diagnostics <- isTRUE(result$residual_diagnostics)
    list(
      f = sprintf("%s(%s)", format_decimal3(result$f_statistic), format_p(result$f_p)),
      r2 = sprintf("%s (%s)", format_decimal3(result$r_squared), format_decimal3(result$adjusted_r_squared)),
      delta = hierarchical_delta_line(previous, result),
      dw = if (residual_diagnostics) sprintf(
        "%s (%s~%s)",
        format_decimal3(result$dw_d),
        format_decimal3(result$dw_crit$dU),
        format_decimal3(4 - result$dw_crit$dU)
      ) else format_decimal3(result$dw_d),
      normality = if (residual_diagnostics) sprintf(
        "%s (%s)",
        format_decimal3(result$normality_statistic),
        format_p(result$normality_p)
      ) else "",
      homogeneity = if (residual_diagnostics) sprintf(
        "%s (%s)",
        format_decimal3(result$homogeneity_statistic),
        format_p(result$homogeneity_p)
      ) else ""
    )
  })
  attr(values, "delta_label") <- hierarchical_delta_footer_label(group)
  attr(values, "any_residual_diagnostics") <- any(vapply(group, function(result) isTRUE(result$residual_diagnostics), logical(1)))
  values
}

hierarchical_coefficient_note_line <- function(result, show_vif = FALSE, show_sr2 = FALSE, show_f2 = FALSE) {
  ci_label <- bootstrap_ci_method_label(result$bootstrap_ci_method %||% "bias_corrected")
  paste(
    if (isTRUE(show_vif)) "Tolerance = 1 - R\u00B2 for each predictor;" else NULL,
    if (isTRUE(show_vif)) "VIF = Variance Inflation Factor;" else NULL,
    if (isTRUE(result$use_hc3)) "HC3 SE = heteroskedasticity-consistent standard error type 3;" else NULL,
    if (isTRUE(result$use_bootstrap)) sprintf("Boot SE is the bootstrap standard error; LLCI and ULCI are %s bootstrap confidence limits based on the selected bootstrap resamples and seed number;", ci_label) else NULL,
    if (isTRUE(show_sr2)) "sr\u00B2 = squared semi-partial correlation, unique R\u00B2 contribution for each coefficient;" else NULL,
    if (isTRUE(show_f2)) "f\u00B2 = sr\u00B2 / (1 - model R\u00B2);" else NULL,
    "\u0394R\u00B2(F change p) is shown when OLS assumptions are met; \u0394R\u00B2(Robust Wald F p) is shown for HC3 models; \u0394R\u00B2[95% CI] is shown for bootstrap models;",
    if (isTRUE(result$residual_diagnostics)) "d(d\u1D64~4-d\u1D64) = Durbin-Watson statistic (upper critical value~4-upper critical value);" else "d = Durbin-Watson statistic;",
    if (isTRUE(result$residual_diagnostics)) "z(p) = Lilliefors corrected Kolmogorov-Smirnov residual normality test statistic (p-value);" else NULL,
    if (isTRUE(result$residual_diagnostics)) sprintf("%s = Breusch-Pagan residual homoscedasticity test statistic (p-value)", stat_chisq_label(with_p = TRUE)) else NULL
  )
}

hierarchical_model_table <- function(
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
  table <- coefficient_output_table_with_context(
    coefficient_display_table(result),
    result$predictors,
    include_references = TRUE,
    variable_info = variable_table,
    refs = refs,
    value_labels = value_labels,
    labels = labels,
    category_table = category_table
  )
  filter_coefficient_export_table(table, show_sr2, show_f2, show_vif)
}

hierarchical_separator_cell <- function(border_top = "0", border_bottom = "1px solid #d7dde5") {
  tags$td(
    class = "hierarchical-model-separator",
    style = paste0(
      "width:10px;min-width:10px;max-width:10px;padding:0;border-left:0;border-right:0;",
      "border-top:", border_top, ";border-bottom:", border_bottom, ";background:transparent;"
    ),
    ""
  )
}

hierarchical_term_cell_style <- function(last = FALSE) {
  paste0(
    "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;",
    "border-top:0;border-bottom:", if (isTRUE(last)) "0" else "1px solid #d7dde5", ";",
    "vertical-align:middle;background:transparent;",
    "width:auto;min-width:0;max-width:none;",
    "text-align:left;white-space:normal;overflow-wrap:break-word;word-break:keep-all;"
  )
}

hierarchical_header_separator_cell <- function(class = "hierarchical-model-header-separator") {
  is_subheader <- identical(class, "hierarchical-model-subheader-separator")
  tags$th(
    class = paste("hierarchical-model-separator", class),
    style = paste0(
      "width:10px;min-width:10px;max-width:10px;padding:0;border-left:0;border-right:0;",
      "border-top:", if (isTRUE(is_subheader)) "0" else "2px solid #1f2937", ";",
      "border-bottom:", if (isTRUE(is_subheader)) "2px solid #1f2937" else "0", ";",
      "background:transparent;"
    ),
    ""
  )
}

hierarchical_footer_row <- function(label, values, model_columns, first = FALSE) {
  top_border <- if (isTRUE(first)) "2px solid #1f2937" else "1px solid #d7dde5"
  cells <- list(tags$td(
    class = "coefficient-summary-label",
    style = paste0(
      "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;",
      "border-top:", top_border, ";border-bottom:0;text-align:left;",
      "width:auto;min-width:0;max-width:none;white-space:normal;overflow-wrap:break-word;"
    ),
    label
  ))
  for (index in seq_along(values)) {
    cells <- c(cells, list(tags$td(
      colspan = length(model_columns[[index]]),
      style = paste0(
        "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;",
        "border-top:", top_border, ";border-bottom:0;text-align:center;font-weight:500;"
      ),
      values[[index]]
    )))
    if (index < length(values)) {
      cells <- c(cells, list(hierarchical_separator_cell(border_top = top_border, border_bottom = "0")))
    }
  }
  do.call(tags$tr, c(list(class = "coefficient-fit-row"), cells))
}

hierarchical_stat_column_weight <- function(column) {
  key <- result_column_key(column)
  if (key %in% c("bootp")) return(0.82)
  if (key %in% c("llci", "ulci", "p", "sr2", "f2", "tolerance", "vif")) return(0.75)
  if (key %in% c("bootse", "hc3se")) return(1.05)
  1
}

hierarchical_stat_header_label <- function(column) {
  key <- result_column_key(column)
  switch(
    key,
    bootse = "Boot\nSE",
    hc3se = "HC3\nSE",
    bootp = "Boot\np",
    tolerance = "Tol",
    column
  )
}

hierarchical_table_column_percentages <- function(model_columns) {
  model_count <- length(model_columns)
  separator_count <- max(model_count - 1L, 0L)
  term_percent <- if (model_count <= 2L) 22 else 14
  separator_percent <- if (separator_count > 0L) 0.8 else 0
  weights <- unlist(lapply(model_columns, function(columns) {
    vapply(columns, hierarchical_stat_column_weight, numeric(1))
  }), use.names = FALSE)
  available <- max(10, 100 - term_percent - separator_count * separator_percent)
  stat_percents <- if (length(weights) > 0 && sum(weights, na.rm = TRUE) > 0) {
    weights / sum(weights, na.rm = TRUE) * available
  } else {
    numeric(0)
  }
  list(
    term = term_percent,
    separator = separator_percent,
    stats = stat_percents
  )
}

hierarchical_table_colgroup <- function(model_columns) {
  percentages <- hierarchical_table_column_percentages(model_columns)
  stat_index <- 0L
  cols <- list(tags$col(class = "hierarchical-term-col", style = sprintf("width:%.4f%%;", percentages$term)))
  for (index in seq_along(model_columns)) {
    cols <- c(
      cols,
      lapply(model_columns[[index]], function(column) {
        stat_index <<- stat_index + 1L
        tags$col(
          class = hierarchical_stat_column_class(column),
          style = sprintf("width:%.4f%%;", percentages$stats[[stat_index]] %||% 0)
        )
      })
    )
    if (index < length(model_columns)) {
      cols <- c(cols, list(tags$col(class = "hierarchical-separator-col", style = sprintf("width:%.4f%%;", percentages$separator))))
    }
  }
  do.call(tags$colgroup, cols)
}

hierarchical_table_width <- function(model_columns) {
  term_width <- 232
  separator_width <- 10
  model_count <- length(model_columns)
  stat_width <- sum(unlist(lapply(model_columns, function(columns) {
    vapply(columns, hierarchical_stat_column_width, integer(1))
  }), use.names = FALSE))
  term_width + stat_width + (max(model_count - 1, 0) * separator_width)
}

hierarchical_model_note_lines <- function(group, variable_table = NULL, labels = character(0)) {
  if (!is.list(group) || length(group) == 0) {
    return(character(0))
  }
  hierarchical_notes <- unique(vapply(group, function(result) {
    as.character(result$hierarchical_note %||% "")
  }, character(1)))
  hierarchical_notes <- hierarchical_notes[nzchar(hierarchical_notes)]
  model_lines <- vapply(seq_along(group), function(index) {
    result <- group[[index]]
    predictors <- result$predictors %||% character(0)
  predictor_labels <- vapply(
    predictors,
    display_variable_name_static,
    character(1),
    table = variable_table,
    labels = labels,
    label_only = TRUE
  )
    predictor_text <- if (length(predictor_labels) > 0) {
      paste(predictor_labels, collapse = " + ")
    } else {
      "No predictors"
    }
    method_text <- if (length(unique(vapply(group, hierarchical_model_method_label, character(1)))) > 1L) {
      sprintf(" [%s]", hierarchical_model_method_label(result))
    } else {
      ""
    }
    sprintf("%s%s: %s", hierarchical_step_label(result, index), method_text, predictor_text)
  }, character(1))
  c(hierarchical_notes, model_lines)
}

hierarchical_standard_summary_table <- function(table, summary, model_index, summary_values, include_delta = TRUE) {
  columns <- names(table)
  if (length(columns) == 0) {
    return(table)
  }
  stat_columns <- setdiff(columns, "Term")
  if (length(stat_columns) == 0) {
    stat_columns <- columns[-1]
  }
  if (length(stat_columns) == 0) {
    return(table)
  }
  output <- as.data.frame(lapply(table, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  names(output) <- columns
  summary_items <- list(
    list(label = "F(p)", value = summary$f %||% ""),
    list(label = "R\u00B2(adj. R\u00B2)", value = summary$r2 %||% "")
  )
  if (length(summary_values) > 1L && isTRUE(include_delta) && model_index > 1L) {
    summary_items <- c(summary_items, list(list(
      label = attr(summary_values, "delta_label", exact = TRUE) %||% "\u0394R\u00B2(F change p)",
      value = summary$delta %||% ""
    )))
  }
  if (isTRUE(attr(summary_values, "any_residual_diagnostics", exact = TRUE))) {
    summary_items <- c(summary_items, list(
      list(label = "d(d\u1D64~4-d\u1D64)", value = summary$dw %||% ""),
      list(label = "z(p)", value = summary$normality %||% ""),
      list(label = stat_chisq_label(with_p = TRUE), value = summary$homogeneity %||% "")
    ))
  } else {
    summary_items <- c(summary_items, list(list(label = "d", value = summary$dw %||% "")))
  }

  start_row <- nrow(output) + 1L
  summary_rows <- lapply(summary_items, function(item) {
    row <- output[1L, , drop = FALSE]
    row[1L, ] <- ""
    row[[columns[[1]]]][[1L]] <- item$label
    row[[stat_columns[[1]]]][[1L]] <- item$value
    row
  })
  output <- do.call(rbind, c(list(output), summary_rows))
  attr(output, "bootstrap_regression") <- attr(table, "bootstrap_regression", exact = TRUE)
  attr(output, "show_df") <- attr(table, "show_df", exact = TRUE)
  attr(output, "spanning_cells") <- data.frame(
    row = seq.int(start_row, length.out = length(summary_items)),
    start_column = stat_columns[[1]],
    end_column = stat_columns[[length(stat_columns)]],
    value = vapply(summary_items, function(item) as.character(item$value %||% ""), character(1)),
    style = "text-align:center !important;font-weight:500;",
    stringsAsFactors = FALSE
  )
  output
}

hierarchical_standard_coefficient_html_table <- function(
  model_tables,
  model_labels,
  summary_values,
  note_line = NULL,
  model_note_lines = character(0),
  include_delta = TRUE
) {
  model_blocks <- lapply(seq_along(model_tables), function(index) {
    model_table <- hierarchical_standard_summary_table(
      model_tables[[index]],
      summary_values[[index]],
      index,
      summary_values,
      include_delta = include_delta
    )
    tags$div(
      class = "hierarchical-standard-model-block",
      tags$h4(class = "hierarchical-standard-model-title", model_labels[[index]]),
      coefficient_html_table(model_table, output_table_style = "standard")
    )
  })
  notes <- list()
  clean_model_notes <- model_note_lines[nzchar(model_note_lines %||% "")]
  if (length(clean_model_notes) > 0) {
    notes <- c(notes, list(tags$div(
      class = "coefficient-note hierarchical-model-notes",
      lapply(clean_model_notes, function(line) tags$div(class = "hierarchical-model-note-line", line))
    )))
  }
  if (!is.null(note_line) && nzchar(note_line)) {
    notes <- c(notes, list(tags$div(class = "coefficient-note hierarchical-coefficient-note", note_line)))
  }
  do.call(
    tags$div,
    c(
      list(class = "hierarchical-standard-table-wrap"),
      model_blocks,
      notes
    )
  )
}

hierarchical_compact_method_columns <- function(model_tables) {
  table_names <- unique(unlist(lapply(model_tables, names), use.names = FALSE))
  has_bootstrap <- any(c("Boot SE", "LLCI", "ULCI", "Boot p") %in% table_names)
  has_hc3 <- "HC3 SE" %in% table_names
  has_se <- "SE" %in% table_names
  has_beta <- "beta" %in% table_names
  has_t <- "t" %in% table_names || "p" %in% table_names
  c(
    "B",
    if (isTRUE(has_se)) "SE",
    if (isTRUE(has_hc3)) "HC3 SE",
    if (isTRUE(has_bootstrap)) "Boot SE",
    if (isTRUE(has_beta)) "beta",
    if (isTRUE(has_t)) "t(p)",
    if (isTRUE(has_bootstrap)) c("LLCI", "ULCI", "Boot p")
  )
}

hierarchical_compact_cell <- function(table, row_index, column) {
  if (!column %in% names(table)) {
    return("")
  }
  as.character(table[[column]][[row_index]] %||% "")
}

hierarchical_compact_tp_cell <- function(table, row_index) {
  t_value <- hierarchical_compact_cell(table, row_index, "t")
  p_value <- hierarchical_compact_cell(table, row_index, "p")
  if (!nzchar(t_value) && !nzchar(p_value)) {
    return("")
  }
  if (!nzchar(t_value)) {
    return(sprintf("(%s)", p_value))
  }
  if (!nzchar(p_value)) {
    return(t_value)
  }
  sprintf("%s(%s)", t_value, p_value)
}

hierarchical_compact_summary_cell <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value) || grepl("\n", value, fixed = TRUE)) {
    return(value)
  }
  matched <- regexec("^([^()]+?)\\s*\\((.*)\\)$", value, perl = TRUE)
  parts <- regmatches(value, matched)[[1]]
  if (length(parts) == 3L) {
    return(sprintf("%s\n(%s)", trimws(parts[[2]]), parts[[3]]))
  }
  value
}

hierarchical_compact_row <- function(model_label, variable, method_values, summary_values, columns) {
  row <- stats::setNames(as.list(rep("", length(columns))), columns)
  row[["Model"]] <- model_label
  row[["Variable"]] <- variable
  for (column in intersect(names(method_values), columns)) {
    row[[column]] <- method_values[[column]]
  }
  for (column in intersect(names(summary_values), columns)) {
    row[[column]] <- summary_values[[column]]
  }
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

hierarchical_compact_model_label <- function(label, fallback = "") {
  if (inherits(label, "shiny.tag") || inherits(label, "shiny.tag.list")) {
    html <- tryCatch(htmltools::renderTags(label)[["html"]], error = function(e) "")
    html <- gsub("<br\\s*/?>", "\n", html, ignore.case = TRUE, perl = TRUE)
    html <- gsub("<[^>]+>", "", html, perl = TRUE)
    html <- gsub("&nbsp;", " ", html, fixed = TRUE)
    html <- gsub("&amp;", "&", html, fixed = TRUE)
    html <- gsub("&lt;", "<", html, fixed = TRUE)
    html <- gsub("&gt;", ">", html, fixed = TRUE)
    html <- gsub("[ \t]*\n[ \t]*", "\n", html, perl = TRUE)
    html <- gsub("\n{2,}", "\n", html, perl = TRUE)
    html <- trimws(html)
    if (nzchar(html)) {
      return(html)
    }
  }
  value <- as.character(label %||% fallback)
  value <- value[[1]] %||% fallback
  if (nzchar(value)) value else fallback
}

hierarchical_compact_coefficient_table <- function(model_tables, model_labels, summary_values) {
  method_columns <- hierarchical_compact_method_columns(model_tables)
  residual_columns <- if (isTRUE(attr(summary_values, "any_residual_diagnostics", exact = TRUE))) {
    c("d", "z(p)", "chi^2(p)")
  } else {
    "d"
  }
  summary_columns <- c("F(p)", "R^2(adj R^2)", residual_columns)
  columns <- c("Model", "Variable", method_columns, summary_columns)
  rows <- list()
  for (model_index in seq_along(model_tables)) {
    table <- model_tables[[model_index]]
    if (!is.data.frame(table) || nrow(table) == 0) {
      next
    }
    model_label <- hierarchical_compact_model_label(
      model_labels[[model_index]] %||% sprintf("Model %s", model_index),
      sprintf("Model %s", model_index)
    )
    summary <- summary_values[[model_index]]
    for (row_index in seq_len(nrow(table))) {
      method_values <- list(
        B = hierarchical_compact_cell(table, row_index, "B"),
        SE = hierarchical_compact_cell(table, row_index, "SE"),
        `HC3 SE` = hierarchical_compact_cell(table, row_index, "HC3 SE"),
        `Boot SE` = hierarchical_compact_cell(table, row_index, "Boot SE"),
        beta = hierarchical_compact_cell(table, row_index, "beta"),
        `t(p)` = hierarchical_compact_tp_cell(table, row_index),
        LLCI = hierarchical_compact_cell(table, row_index, "LLCI"),
        ULCI = hierarchical_compact_cell(table, row_index, "ULCI"),
        `Boot p` = hierarchical_compact_cell(table, row_index, "Boot p")
      )
      row_summary <- if (row_index == 1L) {
        list(
          `F(p)` = hierarchical_compact_summary_cell(summary$f),
          `R^2(adj R^2)` = hierarchical_compact_summary_cell(summary$r2),
          d = hierarchical_compact_summary_cell(summary$dw),
          `z(p)` = hierarchical_compact_summary_cell(summary$normality),
          `chi^2(p)` = hierarchical_compact_summary_cell(summary$homogeneity)
        )
      } else {
        list()
      }
      rows[[length(rows) + 1L]] <- hierarchical_compact_row(
        if (row_index == 1L) model_label else "",
        hierarchical_compact_cell(table, row_index, "Term"),
        method_values,
        row_summary,
        columns
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame())
  }
  output <- do.call(rbind, rows)
  widths <- rep(7, length(columns))
  names(widths) <- columns
  widths["Model"] <- 7
  widths["Variable"] <- 16
  width_overrides <- c(
    `F(p)` = 8,
    `R^2(adj R^2)` = 10,
    d = 10,
    `z(p)` = 8,
    `chi^2(p)` = 8
  )
  matched_widths <- intersect(names(width_overrides), names(widths))
  widths[matched_widths] <- width_overrides[matched_widths]
  attr(output, "compact_column_widths") <- widths / sum(widths, na.rm = TRUE) * 100
  attr(output, "column_display_labels") <- c(
    `F(p)` = "F\n(p)",
    `R^2(adj R^2)` = "R^2\n(adj R^2)",
    `z(p)` = "z\n(p)",
    `chi^2(p)` = "chi^2\n(p)"
  )
  output
}

hierarchical_compact_coefficient_html_table <- function(
  model_tables,
  model_labels,
  summary_values,
  note_line = NULL,
  model_note_lines = character(0),
  output_table_style = "compact"
) {
  table <- hierarchical_compact_coefficient_table(model_tables, model_labels, summary_values)
  notes <- list()
  clean_model_notes <- model_note_lines[nzchar(model_note_lines %||% "")]
  if (length(clean_model_notes) > 0) {
    notes <- c(notes, list(tags$div(
      class = "coefficient-note hierarchical-model-notes",
      lapply(clean_model_notes, function(line) tags$div(class = "hierarchical-model-note-line", line))
    )))
  }
  if (!is.null(note_line) && nzchar(note_line)) {
    notes <- c(notes, list(tags$div(class = "coefficient-note hierarchical-coefficient-note", note_line)))
  }
  do.call(
    tags$div,
    c(
      list(class = "hierarchical-standard-table-wrap hierarchical-compact-table-wrap"),
      list(coefficient_html_table(table, output_table_style = output_table_style)),
      notes
    )
  )
}

hierarchical_coefficient_html_table <- function(
  model_tables,
  model_labels,
  summary_values,
  note_line = NULL,
  model_note_lines = character(0),
  include_delta = TRUE,
  extra_footer_rows = list(),
  output_table_style = "standard"
) {
  if (length(model_tables) == 0) {
    return(NULL)
  }
  output_table_style <- analysis_output_table_style(output_table_style)
  if (identical(output_table_style, "standard")) {
    return(hierarchical_standard_coefficient_html_table(
      model_tables,
      model_labels,
      summary_values,
      note_line = note_line,
      model_note_lines = model_note_lines,
      include_delta = include_delta
    ))
  }
  if (output_table_style %in% c("compact", "compact_xm")) {
    return(hierarchical_compact_coefficient_html_table(
      model_tables,
      model_labels,
      summary_values,
      note_line = note_line,
      model_note_lines = model_note_lines,
      output_table_style = output_table_style
    ))
  }
  style_params <- analysis_output_table_style_params(output_table_style)
  model_columns <- lapply(model_tables, function(table) setdiff(names(table), "Term"))
  terms <- unique(unlist(lapply(model_tables, function(table) as.character(table$Term)), use.names = FALSE))

  header_groups <- list(tags$th(
    rowspan = 2,
    style = paste0(
      "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;",
      "border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
      "text-align:left;font-weight:700;width:auto;min-width:0;max-width:none;white-space:nowrap;"
    ),
    "Variable"
  ))
  for (index in seq_along(model_tables)) {
    header_groups <- c(header_groups, list(tags$th(
      class = "hierarchical-model-header",
      style = paste0(
        "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;",
        "border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
        "text-align:center;font-weight:700;white-space:nowrap;"
      ),
      colspan = length(model_columns[[index]]),
      model_labels[[index]]
    )))
    if (index < length(model_tables)) {
      header_groups <- c(header_groups, list(hierarchical_header_separator_cell()))
    }
  }
  sub_headers <- list()
  for (index in seq_along(model_columns)) {
    columns <- model_columns[[index]]
    sub_headers <- c(sub_headers, lapply(columns, function(column) {
      tags$th(
        style = paste0(hierarchical_stat_cell_style(column, header = TRUE), "font-weight:400;"),
        result_header_content(hierarchical_stat_header_label(column))
      )
    }))
    if (index < length(model_columns)) {
      sub_headers <- c(sub_headers, list(hierarchical_header_separator_cell("hierarchical-model-subheader-separator")))
    }
  }

  body_rows <- lapply(terms, function(term) {
    term_index <- match(term, terms)
    is_last <- identical(term_index, length(terms))
    cells <- list(tags$td(
      style = hierarchical_term_cell_style(is_last),
      term
    ))
    for (model_index in seq_along(model_tables)) {
      table <- model_tables[[model_index]]
      columns <- model_columns[[model_index]]
      row_index <- match(term, as.character(table$Term))
      if (is.na(row_index)) {
        cells <- c(cells, lapply(columns, function(column) tags$td(style = hierarchical_stat_cell_style(column, is_last), "")))
      } else {
        cells <- c(cells, lapply(columns, function(column) {
          tags$td(style = hierarchical_stat_cell_style(column, is_last), as.character(table[[column]][[row_index]] %||% ""))
        }))
      }
      if (model_index < length(model_tables)) {
        cells <- c(cells, list(hierarchical_separator_cell()))
      }
    }
    do.call(tags$tr, cells)
  })

  footer_rows <- list(
    hierarchical_footer_row("F(p)", lapply(summary_values, `[[`, "f"), model_columns, first = TRUE),
    hierarchical_footer_row("R\u00B2(adj. R\u00B2)", lapply(summary_values, `[[`, "r2"), model_columns)
  )
  if (length(model_tables) > 1 && isTRUE(include_delta)) {
    footer_rows <- c(footer_rows, list(
      hierarchical_footer_row(attr(summary_values, "delta_label", exact = TRUE) %||% "\u0394R\u00B2(F change p)", lapply(summary_values, `[[`, "delta"), model_columns)
    ))
  }
  if (isTRUE(attr(summary_values, "any_residual_diagnostics", exact = TRUE))) {
    footer_rows <- c(footer_rows, list(
      hierarchical_footer_row("d(d\u1D64~4-d\u1D64)", lapply(summary_values, `[[`, "dw"), model_columns),
      hierarchical_footer_row("z(p)", lapply(summary_values, `[[`, "normality"), model_columns),
      hierarchical_footer_row(stat_chisq_label(with_p = TRUE), lapply(summary_values, `[[`, "homogeneity"), model_columns)
    ))
  } else {
    footer_rows <- c(footer_rows, list(
      hierarchical_footer_row("d", lapply(summary_values, `[[`, "dw"), model_columns)
    ))
  }
  if (length(extra_footer_rows) > 0L) {
    footer_rows <- c(footer_rows, extra_footer_rows)
  }

  table <- tags$table(
    class = paste("coefficient-table hierarchical-coefficient-table", paste0("output-table-style-", output_table_style)),
    style = paste0(
      result_table_style(
        font_size = if (isTRUE(style_params$compact)) style_params$font_size else 12,
        min_width = 0
      ),
      "width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;"
    ),
    hierarchical_table_colgroup(model_columns),
    tags$thead(
      do.call(tags$tr, header_groups),
      do.call(tags$tr, sub_headers)
    ),
    tags$tbody(body_rows),
    tags$tfoot(footer_rows)
  )

  notes <- list()
  clean_model_notes <- model_note_lines[nzchar(model_note_lines %||% "")]
  if (length(clean_model_notes) > 0) {
    notes <- c(notes, list(tags$div(
      class = "coefficient-note hierarchical-model-notes",
      lapply(clean_model_notes, function(line) tags$div(class = "hierarchical-model-note-line", line))
    )))
  }
  if (!is.null(note_line) && nzchar(note_line)) {
    notes <- c(notes, list(tags$div(class = "coefficient-note hierarchical-coefficient-note", note_line)))
  }

  do.call(
    tags$div,
    c(
      list(class = "result-table-with-note hierarchical-table-wrap"),
      list(div(class = "hierarchical-table-scroll", table)),
      notes
    )
  )
}

hierarchical_coefficient_result_block <- function(
  group,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  output_table_style = "standard"
) {
  if (!is.list(group) || length(group) == 0) {
    return(NULL)
  }
  final_index <- length(group)
  model_tables <- lapply(seq_along(group), function(index) {
    hierarchical_model_table(
      group[[index]],
      variable_table,
      labels,
      category_table,
      refs,
      value_labels,
      show_sr2 = index == final_index && isTRUE(show_sr2),
      show_f2 = index == final_index && isTRUE(show_f2),
      show_vif = index == final_index && isTRUE(show_vif)
    )
  })
  model_labels <- mapply(hierarchical_step_header_label, group, seq_along(group), MoreArgs = list(group = group), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  dependent <- hierarchical_result_dependent_name(group[[1]])
  dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
  coefficient_result_block(
    sprintf("Hierarchical Regression(%s)", dependent_label),
    hierarchical_coefficient_html_table(
      model_tables,
      model_labels,
      hierarchical_summary_values(group),
      hierarchical_coefficient_note_line(group[[final_index]], show_vif, show_sr2, show_f2),
      hierarchical_model_note_lines(group, variable_table, labels),
      output_table_style = output_table_style
    ),
    landscape = identical(analysis_output_table_style(output_table_style), "wide") && length(group) >= 3L
  )
}

hierarchical_results_panel <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  output_table_style = "standard",
  plot_blocks = NULL
) {
  groups <- hierarchical_result_groups(results)
  warnings <- attr(results, "warnings")
  skipped <- attr(results, "skipped")
  div(
    class = "regression-results hierarchical-results",
    div(
      class = "regression-result-panel model-overview-panel",
      h3("Model overview"),
      model_overview_html_table(model_overview_data_frame(results, variable_table, labels))
    ),
    lapply(groups, function(group) {
      hierarchical_coefficient_result_block(
        group,
        variable_table,
        labels,
        category_table,
        refs,
        value_labels,
        show_sr2,
        show_f2,
        show_vif,
        output_table_style
      )
    }),
    regression_reference_summary_block(results, variable_table, labels, show_sr2, show_f2),
    regression_assumption_review_block(results, variable_table, labels),
    analysis_diagnostics_section(warnings, skipped, title = "Warnings / skipped models", class = "regression-result-panel"),
    plot_blocks
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
  show_vif = FALSE,
  output_table_style = "standard"
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
      show_vif,
      output_table_style
    )
  )
}

regression_durbin_watson_result_block <- function(results, variable_table = NULL, labels = character(0)) {
  durbin_watson_result_block(combined_dw_data_frame(results, variable_table, labels))
}

regression_reference_summary_block <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  show_sr2 = FALSE,
  show_f2 = FALSE
) {
  effect_panel <- effect_size_reference_panel(show_sr2, show_f2)
  has_effect <- !is.null(effect_panel)
  if (!has_effect) {
    return(NULL)
  }

  div(
    class = "regression-result-panel reference-summary-panel",
    effect_panel
  )
}

regression_assumption_review_block <- function(results, variable_table = NULL, labels = character(0)) {
  table <- regression_assumption_review_data_frame(results, variable_table, labels)
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  div(
    class = "regression-result-panel assumption-review-panel",
    h3("\uac00\uc815 \uac80\ud1a0"),
    model_overview_html_table(table)
  )
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
  output_table_style = "standard",
  plot_blocks = NULL
) {
  if (regression_results_are_hierarchical(results)) {
    return(hierarchical_results_panel(
      results,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table,
      refs = refs,
      value_labels = value_labels,
      show_sr2 = show_sr2,
      show_f2 = show_f2,
      show_vif = show_vif,
      output_table_style = output_table_style,
      plot_blocks = plot_blocks
    ))
  }
  show_penalized <- is.list(penalized)
  warnings <- attr(results, "warnings")
  skipped <- attr(results, "skipped")
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
        show_vif,
        output_table_style
      )
    }),
    regression_reference_summary_block(results, variable_table, labels, show_sr2, show_f2),
    regression_assumption_review_block(results, variable_table, labels),
    analysis_diagnostics_section(warnings, skipped, title = "Warnings / skipped models", class = "regression-result-panel"),
    if (!isTRUE(show_penalized)) {
      plot_blocks
    }
  )
}

