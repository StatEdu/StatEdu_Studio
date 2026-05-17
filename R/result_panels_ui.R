# Result panel UI builders.

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
  table_tag <- tags$table(
    class = "effect-size-reference-table",
    tags$thead(tags$tr(
      tags$th("Effect size"),
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
    class = "regression-result-panel",
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
    class = "regression-result-panel",
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
    class = "regression-result-panel",
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

hierarchical_delta_line <- function(previous, current) {
  if (is.null(previous) || is.null(current)) {
    return("")
  }
  delta_r2 <- current$r_squared - previous$r_squared
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

hierarchical_summary_values <- function(group) {
  lapply(seq_along(group), function(index) {
    result <- group[[index]]
    previous <- if (index > 1) group[[index - 1]] else NULL
    list(
      f = sprintf("%s(%s)", format_decimal3(result$f_statistic), format_p(result$f_p)),
      r2 = sprintf("%s (%s)", format_decimal3(result$r_squared), format_decimal3(result$adjusted_r_squared)),
      delta = hierarchical_delta_line(previous, result),
      dw = sprintf(
        "%s (%s~%s)",
        format_decimal3(result$dw_d),
        format_decimal3(result$dw_crit$dU),
        format_decimal3(4 - result$dw_crit$dU)
      ),
      normality = sprintf(
        "%s (%s)",
        format_decimal3(result$normality_statistic),
        format_p(result$normality_p)
      ),
      homogeneity = sprintf(
        "%s (%s)",
        format_decimal3(result$homogeneity_statistic),
        format_p(result$homogeneity_p)
      )
    )
  })
}

hierarchical_coefficient_note_line <- function(result, show_vif = FALSE, show_sr2 = FALSE, show_f2 = FALSE) {
  paste(
    if (isTRUE(show_vif)) "Tolerance = 1 - R\u00B2 for each predictor;" else NULL,
    if (isTRUE(show_vif)) "VIF = Variance Inflation Factor;" else NULL,
    if (isTRUE(result$use_hc3)) "HC3 SE = heteroskedasticity-consistent standard error type 3;" else NULL,
    if (isTRUE(result$use_bootstrap)) "Boot SE, LLCI, ULCI, and Boot p are bootstrap estimates based on the selected bootstrap resamples and seed number;" else NULL,
    if (isTRUE(show_sr2)) "sr\u00B2 = squared semi-partial correlation, unique R\u00B2 contribution for each coefficient;" else NULL,
    if (isTRUE(show_f2)) "f\u00B2 = sr\u00B2 / (1 - model R\u00B2);" else NULL,
    "\u0394R\u00B2(F change p) = change in R\u00B2 from the previous model (nested model comparison p-value);",
    "d(d\u1D64~4-d\u1D64) = Durbin-Watson statistic (upper critical value~4-upper critical value);",
    "z(p) = Lilliefors corrected Kolmogorov-Smirnov residual normality test statistic (p-value);",
    sprintf("%s = Breusch-Pagan homoscedasticity test statistic (p-value)", stat_chisq_label(with_p = TRUE))
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
    "width:232px;min-width:232px;max-width:232px;",
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
      "width:232px;min-width:232px;max-width:232px;white-space:normal;overflow-wrap:break-word;"
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

hierarchical_table_colgroup <- function(model_columns) {
  cols <- list(tags$col(class = "hierarchical-term-col"))
  for (index in seq_along(model_columns)) {
    cols <- c(
      cols,
      lapply(model_columns[[index]], function(column) {
        tags$col(class = "hierarchical-stat-col")
      })
    )
    if (index < length(model_columns)) {
      cols <- c(cols, list(tags$col(class = "hierarchical-separator-col")))
    }
  }
  do.call(tags$colgroup, cols)
}

hierarchical_table_width <- function(model_columns) {
  term_width <- 232
  stat_width <- 70
  separator_width <- 10
  model_count <- length(model_columns)
  stat_count <- sum(lengths(model_columns))
  term_width + (stat_count * stat_width) + (max(model_count - 1, 0) * separator_width)
}

hierarchical_model_note_lines <- function(group, variable_table = NULL, labels = character(0)) {
  if (!is.list(group) || length(group) == 0) {
    return(character(0))
  }
  vapply(seq_along(group), function(index) {
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
    sprintf("%s: %s", hierarchical_step_label(result, index), predictor_text)
  }, character(1))
}

hierarchical_coefficient_html_table <- function(
  model_tables,
  model_labels,
  summary_values,
  note_line = NULL,
  model_note_lines = character(0)
) {
  if (length(model_tables) == 0) {
    return(NULL)
  }
  model_columns <- lapply(model_tables, function(table) setdiff(names(table), "Term"))
  terms <- unique(unlist(lapply(rev(model_tables), function(table) as.character(table$Term)), use.names = FALSE))

  header_groups <- list(tags$th(
    rowspan = 2,
    style = paste0(
      "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;",
      "border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
      "text-align:left;font-weight:700;width:232px;min-width:232px;max-width:232px;white-space:nowrap;"
    ),
    "Term"
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
        style = paste0(
          "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;",
          "border-top:0;border-bottom:2px solid #1f2937;",
          "text-align:right;font-weight:700;min-width:65px;white-space:nowrap;"
        ),
        column
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
        cells <- c(cells, lapply(columns, function(column) tags$td(style = result_body_cell_style(FALSE, is_last), "")))
      } else {
        cells <- c(cells, lapply(columns, function(column) {
          tags$td(style = result_body_cell_style(FALSE, is_last), as.character(table[[column]][[row_index]] %||% ""))
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
  if (length(model_tables) > 1) {
    footer_rows <- c(footer_rows, list(
      hierarchical_footer_row("\u0394R\u00B2(F change p)", lapply(summary_values, `[[`, "delta"), model_columns)
    ))
  }
  footer_rows <- c(footer_rows, list(
    hierarchical_footer_row("d(d\u1D64~4-d\u1D64)", lapply(summary_values, `[[`, "dw"), model_columns),
    hierarchical_footer_row("z(p)", lapply(summary_values, `[[`, "normality"), model_columns),
    hierarchical_footer_row(stat_chisq_label(with_p = TRUE), lapply(summary_values, `[[`, "homogeneity"), model_columns)
  ))

  table <- tags$table(
    class = "coefficient-table hierarchical-coefficient-table",
    style = sprintf(
      "%s width: %dpx; min-width: %dpx; table-layout: fixed;",
      result_table_style(),
      hierarchical_table_width(model_columns),
      hierarchical_table_width(model_columns)
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
  show_vif = FALSE
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
  model_labels <- mapply(hierarchical_step_label, group, seq_along(group), USE.NAMES = FALSE)
  dependent <- hierarchical_result_dependent_name(group[[1]])
  dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
  coefficient_result_block(
    sprintf("Hierarchical Regression(%s)", dependent_label),
    hierarchical_coefficient_html_table(
      model_tables,
      model_labels,
      hierarchical_summary_values(group),
      hierarchical_coefficient_note_line(group[[final_index]], show_vif, show_sr2, show_f2),
      hierarchical_model_note_lines(group, variable_table, labels)
    )
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
  plot_blocks = NULL
) {
  groups <- hierarchical_result_groups(results)
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
        show_vif
      )
    }),
    effect_size_reference_panel(show_sr2, show_f2),
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

