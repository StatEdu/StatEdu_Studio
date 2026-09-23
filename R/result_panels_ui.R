# Result panel UI builders.

regression_main_table <- function(table) {
  if (is.data.frame(table)) {
    attr(table, "result_table_role") <- "main"
    attr(table, "result_table_language") <- result_main_table_language()
    attr(table, "regression_publication_style") <- TRUE
    keys <- result_column_key(names(table))
    variable <- which(keys %in% c("term", "variable"))
    if (length(variable) == 1L && "b" %in% keys && !"model" %in% keys) {
      # Reserve label space before distributing the remaining width to statistics.
      label_width <- max(28, min(42, 100 - 10 * (ncol(table) - 1L)))
      weights <- rep(1, ncol(table))
      if (any(grepl("reference", as.character(table[[which(keys == "b")[[1L]]]]), fixed = TRUE), na.rm = TRUE)) weights[keys == "b"] <- 1.2
      weights[keys %in% c("se", "hc3se", "bootse")] <- 1.1
      weights[keys %in% c("p", "bootp")] <- .95
      weights[variable] <- 0
      widths <- weights / sum(weights) * (100 - label_width)
      widths[variable] <- label_width
      attr(table, "compact_column_widths") <- widths
    }
  }
  table
}

regression_appendix_text <- function(text, language = NULL) {
  language <- result_appendix_table_language(language)
  text <- as.character(text %||% "")
  if (identical(language, "en") || !nzchar(text)) {
    return(text)
  }
  if (!identical(language, "ko")) return(result_appendix_ui_text(text, language))
  korean <- c(
    "Requested" = "요청",
    "Valid" = "유효",
    "Valid %" = "유효 비율(%)",
    "Adequate" = "충분",
    "Unreliable" = "신뢰 불가",
    "Pending" = "대기",
    "Regression" = "회귀분석",
    "OLS regression" = "OLS 회귀분석",
    "OLS regression with HC3 robust standard errors" = "HC3 강건 표준오차를 사용한 OLS 회귀분석",
    "Bootstrap regression" = "부트스트랩 회귀분석",
    "Bootstrap regression with HC3 robust standard errors" = "HC3 강건 표준오차를 사용한 부트스트랩 회귀분석",
    "OLS Regression" = "OLS 회귀분석",
    "HC3 Regression" = "HC3 회귀분석",
    "Bootstrap Regression" = "부트스트랩 회귀분석",
    "Bootstrap + HC3 Regression" = "부트스트랩 + HC3 회귀분석",
    "Not rejected" = "기각되지 않음",
    "Violated" = "위반",
    "Diagnostic plots" = "진단 도표",
    "Q-Q plot" = "Q-Q 도표",
    "Residual homoscedasticity" = "잔차 등분산성",
    "Warnings / skipped models" = "경고 / 제외된 모형"
  )
  if (text %in% names(korean)) {
    return(unname(korean[[text]]))
  }
  result_appendix_ui_text(text, language)
}

regression_appendix_table <- function(table, language = NULL) {
  source_table <- table
  if (!is.data.frame(table)) {
    return(table)
  }
  language <- result_appendix_table_language(language)
  if (identical(language, "en")) {
    attr(table, "result_table_role") <- "appendix"
    attr(table, "result_table_language") <- "en"
    return(table)
  }
  translate_cell <- function(value) {
    lines <- strsplit(as.character(value %||% ""), "\n", fixed = TRUE)[[1L]]
    paste(vapply(lines, regression_appendix_text, character(1), language = language), collapse = "\n")
  }
  for (column in names(table)) {
    if (!is.character(table[[column]]) && !is.factor(table[[column]])) next
    table[[column]] <- vapply(as.character(table[[column]]), translate_cell, character(1))
  }
  names(table) <- vapply(names(table), regression_appendix_text, character(1), language = language)
  attr(table, "result_table_role") <- "appendix"
  attr(table, "result_table_language") <- language
  result_appendix_preserve_data(table, source_table)
}

regression_sci_note <- function(result, show_vif = FALSE, show_sr2 = FALSE, show_f2 = FALSE, reference = NULL) {
  notes <- c(coefficient_note_line(result, show_vif, show_sr2, show_f2), reference)
  notes <- notes[!is.na(notes) & nzchar(trimws(notes))]
  paste(notes, collapse = "\n")
}

regression_group_sci_note <- function(group, show_vif = FALSE, show_sr2 = FALSE, show_f2 = FALSE, reference = NULL) {
  if (!length(group)) return("")
  combined <- group[[length(group)]]
  for (field in c("use_hc3", "use_bootstrap", "residual_diagnostics")) {
    combined[[field]] <- any(vapply(group, function(result) isTRUE(result[[field]]), logical(1)))
  }
  # Include every SE type used by the models, then the shared ordered definitions.
  se_notes <- unique(vapply(group, function(result) {
    strsplit(coefficient_note_line(result), ";", fixed = TRUE)[[1L]][[1L]]
  }, character(1)))
  note <- coefficient_note_line(combined, show_vif, show_sr2, show_f2)
  note <- paste(paste(se_notes, collapse = "; "), sub("^[^;]+; *", "", note), sep = "; ")
  notes <- c(note, reference)
  paste(unique(notes[!is.na(notes) & nzchar(trimws(notes))]), collapse = "\n")
}

coefficient_result_ui <- function(table, result, show_sr2 = FALSE, show_f2 = FALSE, show_vif = FALSE, output_table_style = "standard") {
  table <- filter_coefficient_export_table(table, show_sr2, show_f2, show_vif)
  if (isTRUE(result$use_bootstrap)) {
    attr(table, "bootstrap_regression") <- TRUE
  }
  fit_line <- coefficient_fit_line(result)
  stat_lines <- coefficient_stat_lines(result)
  warning_line <- coefficient_vif_warning_line(result)
  note_line <- coefficient_note_line(result, show_vif, show_sr2, show_f2)
  coefficient_html_table(
    regression_main_table(table),
    fit_line,
    stat_lines,
    warning_line,
    regression_sci_note(result, show_vif, show_sr2, show_f2),
    output_table_style = output_table_style,
    sheet_orientation = if (identical(output_table_style, "standard") && nrow(coefficient_display_columns(table)) <= 9L) "portrait" else "auto",
    table_role = "main"
  )
}

coefficient_result_block <- function(title, content, landscape = FALSE) {
  div(
    class = "result-section regression-result-panel",
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
  appendix_language <- result_appendix_table_language()
  appendix_text <- function(text) result_appendix_ui_text(text, appendix_language)
  table_tag <- tags$table(
    class = "effect-size-reference-table",
    tags$thead(tags$tr(
      tags$th("ES"),
      tags$th(appendix_text("Reference")),
      tags$th(appendix_text("Small")),
      tags$th(appendix_text("Medium")),
      tags$th(appendix_text("Large"))
    )),
    tags$tbody(rows)
  )
  note_tag <- if (isTRUE(show_sr2)) {
    result_note_div(
      class = "coefficient-note effect-size-reference-note",
      if (identical(appendix_language, "ko")) {
        tags$span("제곱 준부분상관(sr", tags$sup("2"), ")은 각 예측변수의 고유 설명분산을 나타냅니다. .01, .09, .25를 각각 작은, 중간, 큰 효과의 기준으로 사용했습니다.")
      } else {
        tags$span("Squared semi-partial correlations (sr", tags$sup("2"), ") estimate the unique variance explained by each predictor; .01, .09, and .25 indicate small, medium, and large effects, respectively.")
      }
    )
  } else {
    NULL
  }

  tagList(
    div(
      class = "result-section effect-size-reference-panel",
      h4(appendix_text("Effect Size Guidelines")),
      result_table_with_notes(
        result_table_apply_contract(
          table_tag,
          result_table_contract(role = "appendix", language = appendix_language, intrinsic_width = 520L)
        ),
        note_tag
      ),
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

diagnostic_plot_title <- function(dependent_label, result = NULL, language = NULL) {
  language <- result_appendix_table_language(language)
  title <- sprintf("%s(%s)", regression_appendix_text("Diagnostic plots", language), dependent_label)
  if (!is.null(result) && isTRUE(result$hierarchical)) {
    step <- result$hierarchical_step %||% ""
    if (!nzchar(step)) {
      step_index <- suppressWarnings(as.integer(result$hierarchical_step_index %||% NA_integer_))
      if (!is.na(step_index)) {
        step <- sprintf("%s %s", regression_appendix_text("Model", language), step_index)
      }
    }
    if (nzchar(step)) {
      title <- sprintf("%s - %s", title, step)
    }
  }
  title
}

saved_plot_result_block <- function(result, dependent_label, plot_renderer = plot_data_uri) {
  appendix_language <- result_appendix_table_language()
  div(
    class = "regression-result-panel diagnostic-plots-section",
    lang = appendix_language,
    h3(diagnostic_plot_title(dependent_label, result, appendix_language)),
    div(
      class = "residual-diagnostic-plots",
      div(
        class = "residual-plot-card",
        h4(regression_appendix_text("Q-Q plot", appendix_language)),
        tags$img(
          src = plot_renderer(plot_residual_qq, result),
          width = "420",
          height = "420",
          alt = sprintf("%s(%s)", regression_appendix_text("Q-Q plot", appendix_language), dependent_label)
        )
      ),
      div(
        class = "residual-plot-card",
        h4(regression_appendix_text("Residual homoscedasticity", appendix_language)),
        tags$img(
          src = plot_renderer(plot_residual_homoscedasticity, result),
          width = "420",
          height = "420",
          alt = sprintf("%s(%s)", regression_appendix_text("Residual homoscedasticity", appendix_language), dependent_label)
        )
      )
    )
  )
}

plot_result_panel <- function(dependent_label, qq_output_id, homoscedasticity_output_id, result = NULL) {
  appendix_language <- result_appendix_table_language()
  div(
    class = "regression-result-panel diagnostic-plots-section",
    lang = appendix_language,
    h3(diagnostic_plot_title(dependent_label, result, appendix_language)),
    div(
      class = "residual-diagnostic-plots",
      div(
        class = "residual-plot-card",
        h4(regression_appendix_text("Q-Q plot", appendix_language)),
        plotOutput(qq_output_id, height = "420px")
      ),
      div(
        class = "residual-plot-card",
        h4(regression_appendix_text("Residual homoscedasticity", appendix_language)),
        plotOutput(homoscedasticity_output_id, height = "420px")
      )
    )
  )
}

durbin_watson_result_block <- function(table) {
  table <- regression_appendix_table(table)
  div(
    class = "result-section regression-result-panel durbin-watson-panel",
    h3(result_appendix_ui_text("Durbin-Watson")),
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
    out <- c(lower = NA_real_, upper = NA_real_)
    attr(out, "status") <- "Pending"
    attr(out, "requested") <- 0L
    attr(out, "valid") <- 0L
    return(out)
  }
  delta <- current_r2[seq_len(count)] - previous_r2[seq_len(count)]
  delta <- delta[is.finite(delta)]
  status <- regression_bootstrap_status(length(delta), count)
  point <- current$r_squared - previous$r_squared
  ci_method <- current$bootstrap_ci_method %||% previous$bootstrap_ci_method %||% "bias_corrected"
  out <- if (identical(status, "Unreliable") || length(delta) == 0L) {
    c(lower = NA_real_, upper = NA_real_)
  } else {
    stats::setNames(bootstrap_ci(point, delta, conf = conf, method = ci_method), c("lower", "upper"))
  }
  attr(out, "status") <- status
  attr(out, "requested") <- count
  attr(out, "valid") <- length(delta)
  out
}

hierarchical_delta_line <- function(previous, current) {
  if (is.null(previous) || is.null(current)) {
    return("")
  }
  delta_r2 <- current$r_squared - previous$r_squared
  if (isTRUE(previous$use_bootstrap) || isTRUE(current$use_bootstrap)) {
    ci <- hierarchical_bootstrap_delta_r2_ci(previous, current)
    status <- as.character(attr(ci, "status", exact = TRUE) %||% "Pending")
    valid <- as.integer(attr(ci, "valid", exact = TRUE) %||% 0L)
    requested <- as.integer(attr(ci, "requested", exact = TRUE) %||% 0L)
    if (all(is.finite(ci))) {
      return(sprintf(
        "\u0394 R\u00B2[95%% CI]=%s[%s, %s]%s",
        format_decimal3(delta_r2),
        format_decimal3(ci[[1]]),
        format_decimal3(ci[[2]]),
        if (identical(status, "Caution")) sprintf("; Caution %s/%s valid", valid, requested) else ""
      ))
    }
    if (identical(status, "Unreliable")) {
      return(sprintf("\u0394 R\u00B2[95%% CI]=%s[unreliable: %s/%s valid]", format_decimal3(delta_r2), valid, requested))
    }
    return(sprintf("\u0394 R\u00B2[95%% CI]=%s[pending]", format_decimal3(delta_r2)))
  }
  if (isTRUE(previous$use_hc3) || isTRUE(current$use_hc3)) {
    robust_p <- hierarchical_robust_wald_f_p(previous, current)
    return(sprintf(
      "\u0394 R\u00B2(Robust Wald F p)=%s(%s)",
      format_decimal3(delta_r2),
      format_p(robust_p)
    ))
  }
  df1 <- current$f_df1 - previous$f_df1
  df2 <- current$f_df2
  if (!is.finite(delta_r2) || !is.finite(df1) || !is.finite(df2) || df1 <= 0 || df2 <= 0) {
    return(sprintf("\u0394 R\u00B2(p)=%s", format_decimal3(delta_r2)))
  }
  f_change <- (delta_r2 / df1) / ((1 - current$r_squared) / df2)
  p_change <- stats::pf(f_change, df1, df2, lower.tail = FALSE)
  sprintf(
    "\u0394 R\u00B2(p)=%s(%s)",
    format_decimal3(delta_r2),
    format_p(p_change)
  )
}

hierarchical_delta_footer_label <- function(group) {
  if (any(vapply(group, function(result) isTRUE(result$use_bootstrap), logical(1)))) {
    return("\u0394 R\u00B2(95% CI)")
  }
  if (any(vapply(group, function(result) isTRUE(result$use_hc3), logical(1)))) {
    return("\u0394 R\u00B2(p)")
  }
  "\u0394 R\u00B2(p)"
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
  model_test_labels <- unique(vapply(group, function(result) as.character(result$model_test_label %||% "F")[[1L]], character(1)))
  attr(values, "f_label") <- if (length(model_test_labels) == 1L) paste0(model_test_labels[[1L]], "(p)") else "F(p) / Robust Wald F(p)"
  attr(values, "delta_label") <- hierarchical_delta_footer_label(group)
  attr(values, "any_residual_diagnostics") <- any(vapply(group, function(result) isTRUE(result$residual_diagnostics), logical(1)))
  values
}

hierarchical_coefficient_note_line <- function(result, show_vif = FALSE, show_sr2 = FALSE, show_f2 = FALSE) {
  paste(coefficient_note_line(result, show_vif, show_sr2, show_f2),
    "\u0394 R²(F change p) is shown for OLS models; \u0394 R²(Robust Wald F p) for HC3 models; \u0394 R²[95% CI] for bootstrap models.")
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
  regression_main_table(filter_coefficient_export_table(table, show_sr2, show_f2, show_vif))
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
    "border-top:0;border-bottom:", if (isTRUE(last)) "2px solid #1f2937 !important" else "1px solid #d7dde5", ";",
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
  top_border <- if (isTRUE(first)) "0" else "1px solid #d7dde5"
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
        "border-top:", top_border, ";border-bottom:0;text-align:right;font-weight:500;"
      ),
      values[[index]]
    )))
    if (index < length(values)) {
      cells <- c(cells, list(hierarchical_separator_cell(border_top = top_border, border_bottom = "0")))
    }
  }
  do.call(tags$tr, c(list(class = "coefficient-fit-row"), cells))
}

hierarchical_footer_separator_row <- function(model_columns) {
  total_columns <- 1L + sum(vapply(model_columns, length, integer(1))) + max(0L, length(model_columns) - 1L)
  tags$tr(
    class = "coefficient-fit-separator-row",
    tags$td(
      colspan = total_columns,
      style = "padding:0 !important;height:2px !important;line-height:0;border:0 !important;background:#1f2937 !important;",
      ""
    )
  )
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
    beta = "\u03B2",
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
    sprintf("%s (%s)", hierarchical_step_label(result, index), hierarchical_model_method_label(result))
  }, character(1))
  c(hierarchical_notes, model_lines)
}

hierarchical_summary_value_available <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return(FALSE)
  }
  text <- tryCatch(trimws(as.character(value)), error = function(e) character(0))
  any(!is.na(text) & nzchar(text) & !tolower(text) %in% c("na", "nan", "null"))
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
    list(label = attr(summary_values, "f_label", exact = TRUE) %||% "F(p)", value = summary$f %||% ""),
    list(label = "R\u00B2(adj. R\u00B2)", value = summary$r2 %||% "")
  )
  if (isTRUE(include_delta) && hierarchical_summary_value_available(summary$delta)) {
    summary_items <- c(summary_items, list(list(
      label = attr(summary_values, "delta_label", exact = TRUE) %||% "\u0394 R\u00B2(F change p)",
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
  attr(output, "compact_column_widths") <- attr(table, "compact_column_widths", exact = TRUE)
  attr(output, "column_display_labels") <- attr(table, "column_display_labels", exact = TRUE)
  attr(output, "bootstrap_regression") <- attr(table, "bootstrap_regression", exact = TRUE)
  attr(output, "show_df") <- attr(table, "show_df", exact = TRUE)
  existing_cell_styles <- attr(table, "cell_styles", exact = TRUE)
  separator_styles <- data.frame(
    row = rep(start_row, length(columns)),
    column = columns,
    style = "border-top:1px solid #1f2937 !important;",
    stringsAsFactors = FALSE
  )
  attr(output, "cell_styles") <- if (is.data.frame(existing_cell_styles) && nrow(existing_cell_styles) > 0L) {
    rbind(existing_cell_styles, separator_styles)
  } else {
    separator_styles
  }
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
  if (any(vapply(model_tables,function(table)any(grepl("LLCI",names(table),fixed=TRUE)),logical(1)))) {
    definitions <- c("95% CI = 95% confidence interval", "LLCI = lower confidence limit", "ULCI = upper confidence limit")
    definitions <- definitions[!vapply(c("95% CI", "LLCI", "ULCI"), function(key) grepl(paste0(key,"\\s*="), note_line %||% ""), logical(1))]
    note_line <- result_publication_note(paste(paste(definitions,collapse="; "),note_line %||% "",sep="; "))
  }
  model_blocks <- lapply(seq_along(model_tables), function(index) {
    model_table <- hierarchical_standard_summary_table(
      model_tables[[index]],
      summary_values[[index]],
      index,
      summary_values,
      include_delta = include_delta
    )
    tags$div(
      class = "result-section hierarchical-standard-model-block",
      tags$h4(class = "hierarchical-standard-model-title", model_labels[[index]]),
      coefficient_html_table(
        regression_main_table(model_table),
        note_line = if (index == length(model_tables)) note_line else "",
        ci_note_deferred = index < length(model_tables),
        output_table_style = "standard",
        sheet_orientation = if (nrow(coefficient_display_columns(model_table)) <= 9L) "portrait" else "auto",
        table_role = "main"
      )
    )
  })
  tagList(model_blocks)
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
    if (isTRUE(has_se) || isTRUE(has_hc3) || isTRUE(has_bootstrap)) "SE",
    if (isTRUE(has_beta)) "beta",
    if (isTRUE(has_t)) "t",
    if (isTRUE(has_t) || isTRUE(has_bootstrap)) "p",
    if (isTRUE(has_bootstrap)) c("LLCI", "ULCI")
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

hierarchical_compact_first_cell <- function(table, row_index, columns) {
  columns <- intersect(columns, names(table))
  for (column in columns) {
    value <- hierarchical_compact_cell(table, row_index, column)
    if (nzchar(trimws(value))) {
      return(list(value = value, source = column))
    }
  }
  list(value = "", source = "")
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

hierarchical_compact_summary_parts <- function(value) {
  value <- hierarchical_compact_summary_cell(value)
  parts <- strsplit(value, "\n", fixed = TRUE)[[1]]
  if (length(parts) <= 1L) {
    return(list(primary = value, secondary = ""))
  }
  list(
    primary = parts[[1L]],
    secondary = paste(parts[-1L], collapse = "\n")
  )
}

hierarchical_compact_summary_row_values <- function(summary, split_rows = FALSE, f_label = "F(p)") {
  values <- list(
    summary$f,
    `R²(adj R²)` = summary$r2,
    d = summary$dw,
    `z(p)` = summary$normality,
    `x²(p)` = summary$homogeneity
  )
  names(values)[[1L]] <- f_label
  if (!isTRUE(split_rows)) {
    return(lapply(values, hierarchical_compact_summary_cell))
  }
  parts <- lapply(values, hierarchical_compact_summary_parts)
  list(
    primary = lapply(parts, `[[`, "primary"),
    secondary = lapply(parts, `[[`, "secondary")
  )
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

hierarchical_compact_coefficient_table <- function(model_tables, model_labels, summary_values, output_table_style = "compact") {
  output_table_style <- analysis_output_table_style(output_table_style)
  split_summary_rows <- identical(output_table_style, "compact")
  method_columns <- hierarchical_compact_method_columns(model_tables)
  residual_columns <- if (isTRUE(attr(summary_values, "any_residual_diagnostics", exact = TRUE))) {
    c("d", "z(p)", "x²(p)")
  } else {
    "d"
  }
  f_label <- attr(summary_values, "f_label", exact = TRUE) %||% "F(p)"
  summary_columns <- c(f_label, "R²(adj R²)", residual_columns)
  columns <- c("Model", "Variable", method_columns, summary_columns)
  rows <- list()
  marker_rows <- list()
  marker_notes <- character(0)
  marker_for <- function(note) {
    matched <- match(note, marker_notes)
    if (is.na(matched)) {
      marker_notes <<- c(marker_notes, note)
      matched <- length(marker_notes)
    }
    as.character(matched)
  }
  add_marker <- function(row, column, note) {
    note <- as.character(note %||% "")[[1L]]
    if (!nzchar(note) || !column %in% columns) {
      return(NULL)
    }
    marker_rows[[length(marker_rows) + 1L]] <<- data.frame(
      row = row,
      column = column,
      marker = marker_for(note),
      stringsAsFactors = FALSE
    )
    NULL
  }
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
    compact_summary <- hierarchical_compact_summary_row_values(summary, split_rows = split_summary_rows, f_label = f_label)
    for (row_index in seq_len(nrow(table))) {
      se_cell <- hierarchical_compact_first_cell(table, row_index, c("Boot SE", "HC3 SE", "SE"))
      p_cell <- hierarchical_compact_first_cell(table, row_index, c("Boot p", "p"))
      method_values <- list(
        B = hierarchical_compact_cell(table, row_index, "B"),
        SE = se_cell$value,
        beta = hierarchical_compact_cell(table, row_index, "beta"),
        t = hierarchical_compact_cell(table, row_index, "t"),
        p = p_cell$value,
        LLCI = hierarchical_compact_cell(table, row_index, "LLCI"),
        ULCI = hierarchical_compact_cell(table, row_index, "ULCI")
      )
      row_summary <- if (isTRUE(split_summary_rows) && row_index == 1L) {
        compact_summary$primary
      } else if (isTRUE(split_summary_rows) && row_index == 2L) {
        compact_summary$secondary
      } else if (!isTRUE(split_summary_rows) && row_index == 1L) {
        compact_summary
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
      output_row <- length(rows)
      se_note <- switch(
        se_cell$source,
        "Boot SE" = "SE = bootstrap standard error.",
        "HC3 SE" = "SE = HC3 robust standard error.",
        "SE" = "SE = ordinary least squares standard error.",
        ""
      )
      p_note <- switch(
        p_cell$source,
        "Boot p" = "p = bootstrap p-value.",
        "p" = "p = t-test p-value using the displayed standard error.",
        ""
      )
      add_marker(output_row, "SE", se_note)
      add_marker(output_row, "p", p_note)
    }
    if (isTRUE(split_summary_rows) && nrow(table) < 2L) {
      secondary_values <- unlist(compact_summary$secondary, use.names = FALSE)
      if (any(nzchar(trimws(secondary_values %||% "")))) {
        rows[[length(rows) + 1L]] <- hierarchical_compact_row(
          "",
          "",
          list(),
          compact_summary$secondary,
          columns
        )
      }
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
    `R²(adj R²)` = 10,
    d = 10,
    `z(p)` = 8,
    `x²(p)` = 8
  )
  matched_widths <- intersect(names(width_overrides), names(widths))
  widths[matched_widths] <- width_overrides[matched_widths]
  widths[[f_label]] <- if (identical(f_label, "F(p)")) 8 else 14
  attr(output, "compact_column_widths") <- widths / sum(widths, na.rm = TRUE) * 100
  if (length(marker_rows) > 0L) {
    attr(output, "note_markers") <- do.call(rbind, marker_rows)
  }
  if (length(marker_notes) > 0L) {
    attr(output, "compact_method_notes") <- paste(
      sprintf("%s = %s", seq_along(marker_notes), marker_notes),
      collapse = "\n"
    )
  }
  attr(output, "column_display_labels") <- c(
    stats::setNames(sub("\\(p\\)$", "\n(p)", f_label), f_label),
    `R²(adj R²)` = "R\u00B2\n(adj R\u00B2)",
    `z(p)` = "z\n(p)",
    `x²(p)` = "x\u00B2\n(p)"
  )
  model_rows <- which(nzchar(trimws(as.character(output[["Model"]] %||% ""))))
  model_rows <- model_rows[model_rows > 1L]
  if (length(model_rows) > 0L) {
    attr(output, "cell_styles") <- data.frame(
      row = rep(model_rows, each = length(columns)),
      column = rep(columns, times = length(model_rows)),
      style = "border-top:2px solid #1f2937 !important;",
      stringsAsFactors = FALSE
    )
  }
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
  table <- hierarchical_compact_coefficient_table(
    model_tables,
    model_labels,
    summary_values,
    output_table_style = output_table_style
  )
  coefficient_html_table(
    regression_main_table(table),
    note_line = note_line,
    output_table_style = output_table_style,
    table_role = "main"
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
        cells <- c(cells, list(hierarchical_separator_cell(border_bottom = if (isTRUE(is_last)) "2px solid #1f2937 !important" else "1px solid #d7dde5")))
      }
    }
    do.call(tags$tr, cells)
  })

  footer_rows <- list(
    hierarchical_footer_row(attr(summary_values, "f_label", exact = TRUE) %||% "F(p)", lapply(summary_values, `[[`, "f"), model_columns, first = TRUE),
    hierarchical_footer_row("R\u00B2(adj. R\u00B2)", lapply(summary_values, `[[`, "r2"), model_columns)
  )
  delta_values <- lapply(summary_values, `[[`, "delta")
  if (isTRUE(include_delta) && any(vapply(delta_values, hierarchical_summary_value_available, logical(1)))) {
    footer_rows <- c(footer_rows, list(
      hierarchical_footer_row(attr(summary_values, "delta_label", exact = TRUE) %||% "\u0394 R\u00B2(F change p)", delta_values, model_columns)
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
    tags$tbody(c(body_rows, list(hierarchical_footer_separator_row(model_columns)))),
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
    notes <- c(notes, list(result_note_div(class = "coefficient-note hierarchical-coefficient-note", note_line)))
  }
  table <- result_table_apply_contract(
    table,
    result_table_contract(
      role = "main",
      language = result_main_table_language(),
      intrinsic_width = max(480L, hierarchical_table_width(model_columns))
    )
  )
  do.call(
    result_table_with_notes,
    c(
      list(table, class = "result-table-with-note hierarchical-table-wrap hierarchical-table-scroll"),
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
  if (identical(analysis_output_table_style(output_table_style), "standard")) {
    summary_values <- hierarchical_summary_values(group)
    publication_labels <- lapply(seq_along(group), function(index) tagList(
      hierarchical_step_label(group[[index]], index),
      tags$span(class = "mm-combined-sublabel", sprintf("(%s; %s)", dependent_label, hierarchical_model_method_label(group[[index]])))
    ))
    return(tags$div(class = "regression-publication-panel",
      hierarchical_standard_coefficient_html_table(
        model_tables, publication_labels, summary_values,
        note_line = regression_group_sci_note(group, show_vif, show_sr2, show_f2,
          reference = vapply(group, function(model) model$hierarchical_note %||% "", character(1))),
        include_delta = TRUE
      )
    ))
  }
  coefficient_result_block(
    sprintf("Hierarchical Regression(%s)", dependent_label),
    hierarchical_coefficient_html_table(
      model_tables,
      model_labels,
      hierarchical_summary_values(group),
      regression_group_sci_note(
        group,
        show_vif,
        show_sr2,
        show_f2,
        reference = hierarchical_model_note_lines(group, variable_table, labels)
      ),
      character(0),
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
      class = "result-section regression-result-panel model-overview-panel",
      h3(result_appendix_ui_text("Model overview")),
      model_overview_html_table(regression_appendix_table(model_overview_data_frame(results, variable_table, labels)))
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
    regression_bootstrap_diagnostics_block(results, variable_table, labels),
    regression_assumption_review_block(results, variable_table, labels),
    analysis_diagnostics_section(
      warnings,
      skipped,
      title = regression_appendix_text("Warnings / skipped models"),
      class = "regression-result-panel"
    ),
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
    class = "result-section regression-result-panel reference-summary-panel",
    effect_panel
  )
}

regression_assumption_review_block <- function(results, variable_table = NULL, labels = character(0)) {
  table <- regression_assumption_review_data_frame(results, variable_table, labels)
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  div(
    class = "result-section regression-result-panel assumption-review-panel",
    h3(result_appendix_ui_text("Assumption review")),
    model_overview_html_table(regression_appendix_table(table))
  )
}

regression_bootstrap_diagnostics_data_frame <- function(results, variable_table = NULL, labels = character(0)) {
  rows <- lapply(seq_along(results %||% list()), function(index) {
    result <- results[[index]]
    table <- result$boot_table
    required <- c("Term", "Requested", "Valid", "Valid %", "Status")
    if (!is.data.frame(table) || nrow(table) == 0L || !all(required %in% names(table))) return(NULL)
    dependent <- hierarchical_result_dependent_name(result)
    data.frame(
      Dependent = display_variable_name_static(dependent, variable_table, labels, label_only = TRUE),
      Model = as.character(result$hierarchical_step %||% "Regression"),
      Statistic = as.character(table$Term),
      Requested = as.integer(table$Requested),
      Valid = as.integer(table$Valid),
      `Valid %` = vapply(as.numeric(table[["Valid %"]]), format_decimal3, character(1)),
      Status = as.character(table$Status),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  if (regression_results_are_hierarchical(results)) {
    for (group in hierarchical_result_groups(results)) {
      if (length(group) < 2L) next
      dependent <- hierarchical_result_dependent_name(group[[1L]])
      dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
      for (index in 2:length(group)) {
        if (!isTRUE(group[[index - 1L]]$use_bootstrap) && !isTRUE(group[[index]]$use_bootstrap)) next
        interval <- hierarchical_bootstrap_delta_r2_ci(group[[index - 1L]], group[[index]])
        status <- as.character(attr(interval, "status", exact = TRUE) %||% "Pending")
        if (identical(status, "Pending")) next
        requested <- as.integer(attr(interval, "requested", exact = TRUE) %||% 0L)
        valid <- as.integer(attr(interval, "valid", exact = TRUE) %||% 0L)
        rows[[length(rows) + 1L]] <- data.frame(
          Dependent = dependent_label,
          Model = hierarchical_step_label(group[[index]], index),
          Statistic = sprintf("\u0394 R²: %s vs %s", hierarchical_step_label(group[[index]], index), hierarchical_step_label(group[[index - 1L]], index - 1L)),
          Requested = requested,
          Valid = valid,
          `Valid %` = format_decimal3(if (requested > 0L) 100 * valid / requested else NA_real_),
          Status = status,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
  }
  analysis_bind_rows(rows)
}

regression_bootstrap_diagnostics_block <- function(results, variable_table = NULL, labels = character(0)) {
  table <- regression_bootstrap_diagnostics_data_frame(results, variable_table, labels)
  if (!is.data.frame(table) || nrow(table) == 0L) return(NULL)
  div(
    class = "result-section regression-result-panel bootstrap-diagnostics-panel",
    h3(result_appendix_ui_text("Bootstrap diagnostics")),
    model_overview_html_table(regression_appendix_table(table))
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
      class = "result-section regression-result-panel model-overview-panel",
      h3(result_appendix_ui_text("Model overview")),
      model_overview_html_table(regression_appendix_table(model_overview_data_frame(results, variable_table, labels)))
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
    regression_bootstrap_diagnostics_block(results, variable_table, labels),
    regression_assumption_review_block(results, variable_table, labels),
    analysis_diagnostics_section(
      warnings,
      skipped,
      title = regression_appendix_text("Warnings / skipped models"),
      class = "regression-result-panel"
    ),
    if (!isTRUE(show_penalized)) {
      plot_blocks
    }
  )
}
