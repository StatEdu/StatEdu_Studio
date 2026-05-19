# Cross-tabulation result UI.

crosstab_test_table <- function(result) {
  association <- result$association
  rows <- list(data.frame(
    Test = association$method,
    Statistic = crosstab_format_number(association$statistic),
    df = crosstab_format_number(association$df, digits = 0),
    p = crosstab_format_p(association$p),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
  trend <- result$trend
  if (!is.null(trend)) {
    rows[[length(rows) + 1]] <- data.frame(
      Test = trend$method,
      Statistic = crosstab_format_number(trend$statistic),
      df = crosstab_format_number(trend$df, digits = 0),
      p = crosstab_format_p(trend$p),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  do.call(rbind, rows)
}

crosstab_notes_ui <- function(result) {
  notes <- c(result$association$note)
  if (!is.null(result$trend)) {
    notes <- c(notes, "Trend analysis was added as Armitage trend analysis.")
  }
  if (nzchar(as.character(result$trend_note %||% ""))) {
    notes <- c(notes, result$trend_note)
  }
  effect <- crosstab_general_effect_info(result)
  if (nzchar(effect$note)) {
    notes <- c(notes, effect$note)
  }
  div(
    class = "analysis-result-notes crosstab-notes",
    lapply(notes, function(note) div(note))
  )
}

crosstab_expected_table <- function(result) {
  expected <- result$expected_table
  expected <- cbind(Row = rownames(expected), expected, stringsAsFactors = FALSE)
  rownames(expected) <- NULL
  expected
}

crosstab_primary_percent_matrix <- function(result) {
  options <- result$options %||% list()
  if (isTRUE(options$row_percent)) {
    return(crosstab_percent_matrix(result$table, "row"))
  }
  if (isTRUE(options$column_percent)) {
    return(crosstab_percent_matrix(result$table, "column"))
  }
  if (isTRUE(options$total_percent)) {
    return(crosstab_percent_matrix(result$table, "total"))
  }
  NULL
}

crosstab_cell_text <- function(count, percent = NULL) {
  if (is.null(percent) || length(percent) == 0 || is.na(percent)) {
    return(as.character(count))
  }
  percent_text <- crosstab_format_number(percent, 1)
  if (!is.na(percent) && percent < 10) {
    percent_text <- paste0("\u00A0", percent_text)
  }
  sprintf("%s(%s)", count, percent_text)
}

crosstab_split_count_percent <- function(result) {
  isTRUE((result$options %||% list())$split_count_percent)
}

crosstab_split_percent_text <- function(percent) {
  if (is.null(percent) || length(percent) == 0 || is.na(percent)) return("")
  crosstab_format_number(percent, 1)
}

crosstab_total_percent_value <- function(tab, row_index) {
  total <- sum(tab)
  if (total == 0) return(NA_real_)
  sum(tab[row_index, ]) / total * 100
}

crosstab_value_cells <- function(count, percent, split = FALSE) {
  if (isTRUE(split)) {
    return(list(
      tags$td(class = "crosstab-count-cell crosstab-count-only-cell", as.character(count)),
      tags$td(class = "crosstab-count-cell crosstab-percent-only-cell", crosstab_split_percent_text(percent))
    ))
  }
  tags$td(class = "crosstab-count-cell", crosstab_cell_text(count, percent))
}

crosstab_total_cells <- function(tab, row_index, split = FALSE) {
  count <- rowSums(tab)[[row_index]]
  percent <- crosstab_total_percent_value(tab, row_index)
  if (isTRUE(split)) {
    return(list(
      tags$td(class = "crosstab-n-cell crosstab-count-only-cell", as.character(count)),
      tags$td(class = "crosstab-n-cell crosstab-percent-only-cell", crosstab_split_percent_text(percent))
    ))
  }
  tags$td(class = "crosstab-n-cell", crosstab_cell_text(count, percent))
}

crosstab_show_trend_p <- function(results) {
  results <- crosstab_result_list(results)
  any(vapply(results, function(result) isTRUE(result$trend_requested), logical(1)))
}

crosstab_trend_p_text <- function(result) {
  if (is.null(result$trend)) return("")
  crosstab_format_p(result$trend$p)
}

crosstab_colgroup_tags <- function(level_count, split = FALSE, show_trend_p = FALSE) {
  value_cols <- if (isTRUE(split)) level_count * 2 else level_count
  list(
    tags$col(class = "crosstab-row-variable-col"),
    tags$col(class = "crosstab-row-label-col"),
    if (isTRUE(split)) {
      list(tags$col(class = "crosstab-n-col"), tags$col(class = "crosstab-percent-col"))
    } else {
      tags$col(class = "crosstab-n-col")
    },
    lapply(seq_len(value_cols), function(col_index) tags$col(class = "crosstab-count-col")),
    tags$col(class = "crosstab-stat-col"),
    tags$col(class = "crosstab-stat-col"),
    tags$col(class = "crosstab-stat-col"),
    if (isTRUE(show_trend_p)) tags$col(class = "crosstab-trend-p-col")
  )
}

crosstab_header_tags <- function(col_label, col_labels, split = FALSE, show_trend_p = FALSE) {
  if (!isTRUE(split)) {
    return(tags$thead(
      tags$tr(
        tags$th(class = "crosstab-row-head", rowspan = 2, ""),
        tags$th(class = "crosstab-level-label-head", rowspan = 2, ""),
        tags$th(class = "crosstab-n-head", rowspan = 2, "n"),
        tags$th(class = "crosstab-col-head", colspan = length(col_labels), col_label),
        tags$th(class = "crosstab-stat-head", rowspan = 2, HTML("x<sup>2</sup>")),
        tags$th(class = "crosstab-stat-head", rowspan = 2, "p"),
        tags$th(class = "crosstab-stat-head", rowspan = 2, "ES"),
        if (isTRUE(show_trend_p)) tags$th(class = "crosstab-stat-head crosstab-trend-p-head", rowspan = 2, "p for trend")
      ),
      tags$tr(
        lapply(col_labels, function(label) {
          tags$th(class = "crosstab-level-head", label)
        })
      )
    ))
  }
  tags$thead(
    tags$tr(
      tags$th(class = "crosstab-row-head", rowspan = 3, ""),
      tags$th(class = "crosstab-level-label-head", rowspan = 3, ""),
      tags$th(class = "crosstab-total-head", colspan = 2, "Total"),
      tags$th(class = "crosstab-col-head", colspan = length(col_labels) * 2, col_label),
      tags$th(class = "crosstab-stat-head", rowspan = 3, HTML("x<sup>2</sup>")),
      tags$th(class = "crosstab-stat-head", rowspan = 3, "p"),
      tags$th(class = "crosstab-stat-head", rowspan = 3, "ES"),
      if (isTRUE(show_trend_p)) tags$th(class = "crosstab-stat-head crosstab-trend-p-head", rowspan = 3, "p for trend")
    ),
    tags$tr(
      tags$th(class = "crosstab-level-head crosstab-total-level-head", colspan = 2, ""),
      lapply(col_labels, function(label) tags$th(class = "crosstab-level-head", colspan = 2, label))
    ),
    tags$tr(
      tags$th(class = "crosstab-sub-head crosstab-n-sub-head", "n"),
      tags$th(class = "crosstab-sub-head", "%"),
      lapply(col_labels, function(label) {
        list(tags$th(class = "crosstab-sub-head crosstab-n-sub-head", "n"), tags$th(class = "crosstab-sub-head", "%"))
      })
    )
  )
}

crosstab_general_effect_info <- function(result) {
  table <- result$effect_sizes
  empty <- list(value = "", key = "", note = "")
  if (!is.data.frame(table) || nrow(table) == 0) return(empty)
  preferred <- if (any(table$Effect == "Odds ratio")) "Odds ratio" else "Cramer's V"
  matched <- table[table$Effect == preferred, , drop = FALSE]
  if (nrow(matched) == 0) return(empty)
  note <- if (identical(preferred, "Odds ratio")) {
    "ES = odds ratio."
  } else {
    "ES = Cramer's V."
  }
  list(value = matched$Estimate[[1]], key = preferred, note = note)
}

crosstab_effect_note_text <- function(key) {
  key <- as.character(key %||% "")
  switch(
    key,
    "Odds ratio" = "ES = odds ratio.",
    "Cramer's V" = "ES = Cramer's V.",
    if (nzchar(key)) paste0("ES = ", key, ".") else ""
  )
}

crosstab_method_note_text <- function(method) {
  method <- as.character(method %||% "")
  switch(
    method,
    "Pearson chi-square test" = "Pearson chi-square test was used.",
    "Fisher's exact test" = "Fisher's exact test was used because expected counts were low.",
    "Fisher's exact test with Monte Carlo simulation" = "Fisher's exact test with Monte Carlo simulation was used because expected counts were low.",
    if (nzchar(method)) paste0(method, " was used.") else ""
  )
}

crosstab_trend_note_key <- function(result) {
  trend <- result$trend
  if (is.null(trend)) return("")
  detail <- as.character(trend$detail %||% "")
  if (!nzchar(detail)) detail <- as.character(trend$method %||% "trend")
  paste("trend", detail, sep = ":")
}

crosstab_trend_note_text <- function(key) {
  key <- as.character(key %||% "")
  switch(
    key,
    "trend:cochran_armitage" = "Cochran-Armitage trend test was used for p for trend.",
    "trend:ordered_score" = "Score-based ordered-by-ordered trend association was used for p for trend.",
    "Armitage trend analysis was used for p for trend."
  )
}

crosstab_method_footnotes <- function(results) {
  results <- crosstab_result_list(results)
  rows <- list()
  methods <- unique(vapply(results, function(result) result$association$method, character(1)))
  methods <- methods[nzchar(methods)]
  for (method in methods) {
    rows[[length(rows) + 1L]] <- data.frame(
      type = "association",
      key = method,
      note = crosstab_method_note_text(method),
      stringsAsFactors = FALSE
    )
  }
  effect_keys <- unique(vapply(results, function(result) crosstab_general_effect_info(result)$key, character(1)))
  effect_keys <- effect_keys[nzchar(effect_keys)]
  for (key in effect_keys) {
    rows[[length(rows) + 1L]] <- data.frame(
      type = "effect",
      key = key,
      note = crosstab_effect_note_text(key),
      stringsAsFactors = FALSE
    )
  }
  trend_keys <- unique(vapply(results, crosstab_trend_note_key, character(1)))
  trend_keys <- trend_keys[nzchar(trend_keys)]
  for (key in trend_keys) {
    rows[[length(rows) + 1L]] <- data.frame(
      type = "trend",
      key = key,
      note = crosstab_trend_note_text(key),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(marker = character(0), type = character(0), key = character(0), note = character(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out$marker <- as.character(seq_len(nrow(out)))
  out[, c("marker", "type", "key", "note")]
}

crosstab_method_marker <- function(result, method_notes) {
  if (!is.data.frame(method_notes) || nrow(method_notes) == 0) return("")
  matched <- method_notes$marker[method_notes$type == "association" & method_notes$key == result$association$method]
  if (length(matched) == 0) "" else matched[[1]]
}

crosstab_trend_marker <- function(result, method_notes) {
  if (!is.data.frame(method_notes) || nrow(method_notes) == 0 || is.null(result$trend)) return("")
  matched <- method_notes$marker[method_notes$type == "trend" & method_notes$key == crosstab_trend_note_key(result)]
  if (length(matched) == 0) "" else matched[[1]]
}

crosstab_effect_marker <- function(result, method_notes) {
  if (!is.data.frame(method_notes) || nrow(method_notes) == 0) return("")
  effect <- crosstab_general_effect_info(result)
  matched <- method_notes$marker[method_notes$type == "effect" & method_notes$key == effect$key]
  if (length(matched) == 0) "" else matched[[1]]
}

crosstab_p_with_marker <- function(p_value, marker) {
  if (!nzchar(as.character(marker %||% ""))) return(p_value)
  tagList(p_value, tags$sup(class = "crosstab-footnote-marker", marker))
}

crosstab_main_table_ui <- function(result, method_notes = crosstab_method_footnotes(result)) {
  tab <- result$table
  percent <- crosstab_primary_percent_matrix(result)
  row_labels <- crosstab_value_labels(result$row_var, rownames(tab), result$category_table)
  col_labels <- crosstab_value_labels(result$col_var, colnames(tab), result$category_table)
  association <- result$association
  statistic <- crosstab_format_number(association$statistic)
  p_value <- crosstab_format_p(association$p)
  p_marker <- crosstab_method_marker(result, method_notes)
  trend_marker <- crosstab_trend_marker(result, method_notes)
  effect_marker <- crosstab_effect_marker(result, method_notes)
  effect <- crosstab_general_effect_info(result)$value
  stat_rowspan <- nrow(tab)
  split <- crosstab_split_count_percent(result)
  show_trend_p <- crosstab_show_trend_p(result)
  value_width <- if (isTRUE(split)) 50 * (ncol(tab) * 2 + 2) else 86 * (ncol(tab) + 1)
  min_width <- 80 + 86 + value_width + (72 * 3) + if (isTRUE(show_trend_p)) 92 else 0

  tags$table(
    class = "coefficient-table crosstab-main-table",
    style = result_table_style(font_size = 15, min_width = max(820, min_width)),
    tags$colgroup(crosstab_colgroup_tags(ncol(tab), split, show_trend_p)),
    crosstab_header_tags(result$col_label, col_labels, split, show_trend_p),
    tags$tbody(
      lapply(seq_len(nrow(tab)), function(row_index) {
        tags$tr(
          tags$td(class = "crosstab-row-variable", if (row_index == 1) result$row_label else ""),
          tags$td(class = "crosstab-row-label", row_labels[[row_index]]),
          crosstab_total_cells(tab, row_index, split),
          lapply(seq_len(ncol(tab)), function(col_index) {
            crosstab_value_cells(tab[row_index, col_index], if (is.null(percent)) NULL else percent[row_index, col_index], split)
          }),
          if (row_index == 1) {
            list(
              tags$td(class = "crosstab-stat-cell", rowspan = stat_rowspan, statistic),
              tags$td(class = "crosstab-stat-cell", rowspan = stat_rowspan, crosstab_p_with_marker(p_value, p_marker)),
              tags$td(class = "crosstab-stat-cell", rowspan = stat_rowspan, crosstab_p_with_marker(effect, effect_marker)),
              if (isTRUE(show_trend_p)) tags$td(class = "crosstab-stat-cell crosstab-trend-p-cell", rowspan = stat_rowspan, crosstab_p_with_marker(crosstab_trend_p_text(result), trend_marker))
            )
          }
        )
      })
    )
  )
}

crosstab_column_group_table_ui <- function(results, method_notes = crosstab_method_footnotes(results)) {
  first <- results[[1]]
  col_labels <- crosstab_value_labels(first$col_var, colnames(first$table), first$category_table)
  split <- crosstab_split_count_percent(first)
  show_trend_p <- crosstab_show_trend_p(results)
  value_width <- if (isTRUE(split)) 50 * (ncol(first$table) * 2 + 2) else 86 * (ncol(first$table) + 1)
  min_width <- 80 + 86 + value_width + (72 * 3) + if (isTRUE(show_trend_p)) 92 else 0

  tags$table(
    class = "coefficient-table crosstab-main-table",
    style = result_table_style(font_size = 15, min_width = max(820, min_width)),
    tags$colgroup(crosstab_colgroup_tags(ncol(first$table), split, show_trend_p)),
    crosstab_header_tags(first$col_label, col_labels, split, show_trend_p),
    tags$tbody(
      lapply(results, function(result) {
        tab <- result$table
        percent <- crosstab_primary_percent_matrix(result)
        row_labels <- crosstab_value_labels(result$row_var, rownames(tab), result$category_table)
        statistic <- crosstab_format_number(result$association$statistic)
        p_value <- crosstab_format_p(result$association$p)
        p_marker <- crosstab_method_marker(result, method_notes)
        trend_marker <- crosstab_trend_marker(result, method_notes)
        effect_marker <- crosstab_effect_marker(result, method_notes)
        effect <- crosstab_general_effect_info(result)$value
        lapply(seq_len(nrow(tab)), function(row_index) {
          tags$tr(
            class = if (row_index == 1) "crosstab-row-block-start" else NULL,
            if (row_index == 1) {
              tags$td(class = "crosstab-row-variable", rowspan = nrow(tab), result$row_label)
            },
            tags$td(class = "crosstab-row-label", row_labels[[row_index]]),
            crosstab_total_cells(tab, row_index, split),
            lapply(seq_len(ncol(tab)), function(col_index) {
              crosstab_value_cells(tab[row_index, col_index], if (is.null(percent)) NULL else percent[row_index, col_index], split)
            }),
            if (row_index == 1) {
              list(
                tags$td(class = "crosstab-stat-cell", rowspan = nrow(tab), statistic),
                tags$td(class = "crosstab-stat-cell", rowspan = nrow(tab), crosstab_p_with_marker(p_value, p_marker)),
                tags$td(class = "crosstab-stat-cell", rowspan = nrow(tab), crosstab_p_with_marker(effect, effect_marker)),
                if (isTRUE(show_trend_p)) tags$td(class = "crosstab-stat-cell crosstab-trend-p-cell", rowspan = nrow(tab), crosstab_p_with_marker(crosstab_trend_p_text(result), trend_marker))
              )
            }
          )
        })
      })
    )
  )
}

crosstab_column_group_notes_ui <- function(results, method_notes = crosstab_method_footnotes(results)) {
  method_lines <- if (is.data.frame(method_notes) && nrow(method_notes) > 0) {
    sprintf("%s. %s", method_notes$marker, method_notes$note)
  } else {
    character(0)
  }
  trend_lines <- unique(unlist(lapply(results, function(result) {
    if (is.null(result$trend) && nzchar(as.character(result$trend_note %||% ""))) {
      result$trend_note
    } else {
      character(0)
    }
  }), use.names = FALSE))
  notes <- c(method_lines, trend_lines)
  div(
    class = "analysis-result-notes crosstab-notes",
    lapply(notes, function(note) div(note))
  )
}

crosstab_column_group_test_table <- function(results) {
  do.call(rbind, lapply(results, function(result) {
    table <- crosstab_test_table(result)
    out <- data.frame(`Row variable` = result$row_label, stringsAsFactors = FALSE, check.names = FALSE)
    cbind(out, table)
  }))
}

crosstab_column_group_effect_table <- function(results) {
  do.call(rbind, lapply(results, function(result) {
    out <- data.frame(`Row variable` = result$row_label, stringsAsFactors = FALSE, check.names = FALSE)
    cbind(out, result$effect_sizes)
  }))
}

crosstab_column_group_ui <- function(results) {
  first <- results[[1]]
  method_notes <- crosstab_method_footnotes(results)
  div(
    class = "result-section crosstab-result-section regression-result-panel",
    h3(sprintf("Cross-tabulation: %s", first$col_label)),
    div(class = "frequency-table-wrap crosstab-table-wrap", crosstab_column_group_table_ui(results, method_notes)),
    crosstab_column_group_notes_ui(results, method_notes)
  )
}

crosstab_single_result_ui <- function(result) {
  method_notes <- crosstab_method_footnotes(result)
  tagList(
    div(
      class = "result-section crosstab-result-section regression-result-panel",
      h3(sprintf("Cross-tabulation: %s x %s", result$row_label, result$col_label)),
      div(class = "frequency-table-wrap crosstab-table-wrap", crosstab_main_table_ui(result, method_notes)),
      crosstab_column_group_notes_ui(list(result), method_notes)
    ),
    div(
      class = "result-section crosstab-result-section regression-result-panel",
      h3(sprintf("Expected counts: %s x %s", result$row_label, result$col_label)),
      div(class = "frequency-table-wrap", coefficient_html_table(crosstab_expected_table(result)))
    )
  )
}

crosstab_result_list <- function(result) {
  if (is.null(result)) return(list())
  if (is.list(result) && is.null(result$table)) return(result)
  list(result)
}

crosstab_results_by_column <- function(results) {
  column_order <- unique(vapply(results, function(result) result$col_var, character(1)))
  lapply(column_order, function(col_var) {
    results[vapply(results, function(result) identical(result$col_var, col_var), logical(1))]
  })
}

crosstab_results_ui <- function(result) {
  if (is.null(result)) {
    return(div(class = "empty-message regression-results-empty", "Select row and column variables, then click Run analysis."))
  }
  results <- crosstab_result_list(result)
  if (length(results) == 0) {
    return(div(class = "empty-message regression-results-empty", "Select row and column variables, then click Run analysis."))
  }

  div(
    class = "crosstab-results regression-results",
    lapply(crosstab_results_by_column(results), crosstab_column_group_ui)
  )
}
