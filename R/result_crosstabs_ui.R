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
    notes <- c(notes, crosstab_trend_note_text(crosstab_trend_note_key(result)))
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

crosstab_primary_percent_matrix <- function(result, tab = result$table) {
  options <- result$options %||% list()
  if (isTRUE(options$row_percent)) {
    return(crosstab_percent_matrix(tab, "row"))
  }
  if (isTRUE(options$column_percent)) {
    return(crosstab_percent_matrix(tab, "column"))
  }
  if (isTRUE(options$total_percent)) {
    return(crosstab_percent_matrix(tab, "total"))
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

crosstab_show_total_n <- function(result) {
  !identical((result$options %||% list())$total_n, FALSE)
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

crosstab_total_cells <- function(tab, row_index, split = FALSE, show_total_n = TRUE) {
  if (!isTRUE(show_total_n)) return(NULL)
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

crosstab_colgroup_tags <- function(level_count, split = FALSE, show_trend_p = FALSE, show_total_n = TRUE) {
  value_cols <- if (isTRUE(split)) level_count * 2 else level_count
  total_cols <- if (!isTRUE(show_total_n)) {
    NULL
  } else if (isTRUE(split)) {
    list(tags$col(class = "crosstab-n-col"), tags$col(class = "crosstab-percent-col"))
  } else {
    tags$col(class = "crosstab-n-col")
  }
  list(
    tags$col(class = "crosstab-row-variable-col"),
    tags$col(class = "crosstab-row-label-col"),
    total_cols,
    lapply(seq_len(value_cols), function(col_index) tags$col(class = "crosstab-count-col")),
    tags$col(class = "crosstab-stat-col"),
    tags$col(class = "crosstab-stat-col"),
    tags$col(class = "crosstab-stat-col"),
    if (isTRUE(show_trend_p)) tags$col(class = "crosstab-trend-p-col")
  )
}

crosstab_primary_table_width <- function(result, show_trend_p = crosstab_show_trend_p(result)) {
  level_count <- ncol(result$table)
  split <- crosstab_split_count_percent(result)
  show_total_n <- crosstab_show_total_n(result)
  total_width <- if (!isTRUE(show_total_n)) 0 else if (isTRUE(split)) 100 else 86
  value_width <- if (isTRUE(split)) 50 * (level_count * 2) else 86 * level_count
  80 + 86 + total_width + value_width + (72 * 3) + if (isTRUE(show_trend_p)) 92 else 0
}

crosstab_primary_table_column_count <- function(result, show_trend_p = crosstab_show_trend_p(result)) {
  tab <- result$table
  split <- crosstab_split_count_percent(result)
  total_cols <- if (!isTRUE(crosstab_show_total_n(result))) 0L else if (isTRUE(split)) 2L else 1L
  value_cols <- if (isTRUE(split)) ncol(tab) * 2L else ncol(tab)
  2L + total_cols + value_cols + 3L + if (isTRUE(show_trend_p)) 1L else 0L
}

crosstab_primary_table_min_width <- function(result, show_trend_p = crosstab_show_trend_p(result)) {
  base_width <- if (isTRUE(crosstab_show_total_n(result))) 760 else 680
  max(base_width, crosstab_primary_table_width(result, show_trend_p))
}

crosstab_primary_table_landscape <- function(
  result,
  threshold = 760,
  column_threshold = 5,
  display_column_threshold = 10,
  show_trend_p = crosstab_show_trend_p(result)
) {
  crosstab_primary_table_width(result, show_trend_p) > threshold ||
    ncol(result$table) >= column_threshold ||
    crosstab_primary_table_column_count(result, show_trend_p) >= display_column_threshold
}

crosstab_results_landscape <- function(result, threshold = 760) {
  results <- crosstab_result_list(result)
  any(vapply(crosstab_results_by_column(results), function(group) {
    crosstab_column_group_landscape(group, threshold = threshold)
  }, logical(1)))
}

crosstab_column_group_colnames <- function(results) {
  if (length(results) == 0) return(character(0))
  values <- unique(unlist(lapply(results, function(result) colnames(result$table)), use.names = FALSE))
  crosstab_order_values(values, results[[1]]$col_measure %||% "")
}

crosstab_align_table_columns <- function(tab, col_names) {
  col_names <- as.character(col_names %||% character(0))
  if (length(col_names) == 0 || identical(colnames(tab), col_names)) {
    return(tab)
  }
  aligned <- matrix(
    0L,
    nrow = nrow(tab),
    ncol = length(col_names),
    dimnames = list(rownames(tab), col_names)
  )
  matched <- intersect(colnames(tab), col_names)
  if (length(matched) > 0) {
    aligned[, matched] <- tab[, matched, drop = FALSE]
  }
  as.table(aligned)
}

crosstab_column_split_limit <- function(split = FALSE) {
  if (isTRUE(split)) 4L else 7L
}

crosstab_split_colnames <- function(col_names, split = FALSE, limit = crosstab_column_split_limit(split)) {
  col_names <- as.character(col_names %||% character(0))
  if (length(col_names) <= limit) {
    return(list(col_names))
  }
  split(col_names, ceiling(seq_along(col_names) / limit))
}

crosstab_column_group_split_colnames <- function(results) {
  first <- results[[1]]
  crosstab_split_colnames(crosstab_column_group_colnames(results), crosstab_split_count_percent(first))
}

crosstab_primary_split_colnames <- function(result) {
  crosstab_split_colnames(colnames(result$table), crosstab_split_count_percent(result))
}

crosstab_column_split_label <- function(index, total) {
  if (total <= 1L) "" else sprintf(" (%d/%d)", index, total)
}

crosstab_column_group_width <- function(results, show_trend_p = crosstab_show_trend_p(results)) {
  first <- results[[1]]
  level_count <- length(crosstab_column_group_colnames(results))
  split <- crosstab_split_count_percent(first)
  total_width <- if (!isTRUE(crosstab_show_total_n(first))) 0 else if (isTRUE(split)) 100 else 86
  value_width <- if (isTRUE(split)) 50 * (level_count * 2) else 86 * level_count
  80 + 86 + total_width + value_width + (72 * 3) + if (isTRUE(show_trend_p)) 92 else 0
}

crosstab_column_group_column_count <- function(results, show_trend_p = crosstab_show_trend_p(results)) {
  first <- results[[1]]
  split <- crosstab_split_count_percent(first)
  total_cols <- if (!isTRUE(crosstab_show_total_n(first))) 0L else if (isTRUE(split)) 2L else 1L
  value_cols <- if (isTRUE(split)) length(crosstab_column_group_colnames(results)) * 2L else length(crosstab_column_group_colnames(results))
  2L + total_cols + value_cols + 3L + if (isTRUE(show_trend_p)) 1L else 0L
}

crosstab_column_group_min_width <- function(results, show_trend_p = crosstab_show_trend_p(results)) {
  first <- results[[1]]
  base_width <- if (isTRUE(crosstab_show_total_n(first))) 760 else 680
  max(base_width, crosstab_column_group_width(results, show_trend_p))
}

crosstab_column_group_landscape <- function(results, threshold = 760, column_threshold = 5, display_column_threshold = 10) {
  crosstab_column_group_width(results) > threshold ||
  length(crosstab_column_group_colnames(results)) >= column_threshold ||
    crosstab_column_group_column_count(results) >= display_column_threshold
}

crosstab_header_tags <- function(col_label, col_labels, split = FALSE, show_trend_p = FALSE, show_total_n = TRUE) {
  if (!isTRUE(split)) {
    return(tags$thead(
      tags$tr(
        tags$th(class = "crosstab-row-head", rowspan = 2, ""),
        tags$th(class = "crosstab-level-label-head", rowspan = 2, ""),
        if (isTRUE(show_total_n)) tags$th(class = "crosstab-n-head", rowspan = 2, "n"),
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
      if (isTRUE(show_total_n)) tags$th(class = "crosstab-total-head", colspan = 2, "Total"),
      tags$th(class = "crosstab-col-head", colspan = length(col_labels) * 2, col_label),
      tags$th(class = "crosstab-stat-head", rowspan = 3, HTML("x<sup>2</sup>")),
      tags$th(class = "crosstab-stat-head", rowspan = 3, "p"),
      tags$th(class = "crosstab-stat-head", rowspan = 3, "ES"),
      if (isTRUE(show_trend_p)) tags$th(class = "crosstab-stat-head crosstab-trend-p-head", rowspan = 3, "p for trend")
    ),
    tags$tr(
      if (isTRUE(show_total_n)) tags$th(class = "crosstab-level-head crosstab-total-level-head", colspan = 2, ""),
      lapply(col_labels, function(label) tags$th(class = "crosstab-level-head", colspan = 2, label))
    ),
    tags$tr(
      if (isTRUE(show_total_n)) {
        list(tags$th(class = "crosstab-sub-head crosstab-n-sub-head", "n"), tags$th(class = "crosstab-sub-head", "%"))
      },
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
    "ES = effect size (odds ratio)."
  } else {
    "ES = effect size (Cramer's V)."
  }
  list(value = matched$Estimate[[1]], key = preferred, note = note)
}

crosstab_effect_note_text <- function(key) {
  key <- as.character(key %||% "")
  switch(
    key,
    "Odds ratio" = "ES = effect size (odds ratio).",
    "Cramer's V" = "ES = effect size (Cramer's V).",
    if (nzchar(key)) paste0("ES = effect size (", key, ").") else ""
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
    "Trend analysis was used for p for trend."
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
  out$marker <- ""
  marker_index <- 0L
  for (type in unique(out$type)) {
    rows_for_type <- which(out$type == type)
    if (length(rows_for_type) <= 1L) next
    for (row_index in rows_for_type) {
      marker_index <- marker_index + 1L
      out$marker[[row_index]] <- as.character(marker_index)
    }
  }
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

crosstab_main_table_ui <- function(result, method_notes = crosstab_method_footnotes(result), col_names = NULL) {
  tab <- if (is.null(col_names)) result$table else crosstab_align_table_columns(result$table, col_names)
  percent <- crosstab_primary_percent_matrix(result, tab)
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
  show_total_n <- crosstab_show_total_n(result)
  min_width <- if (is.null(col_names)) {
    crosstab_primary_table_min_width(result, show_trend_p)
  } else {
    max(if (isTRUE(show_total_n)) 760 else 680, crosstab_primary_table_width(result, show_trend_p))
  }

  tags$table(
    class = "coefficient-table crosstab-main-table",
    style = result_table_style(font_size = 15, min_width = min_width),
    tags$colgroup(crosstab_colgroup_tags(ncol(tab), split, show_trend_p, show_total_n)),
    crosstab_header_tags(result$col_label, col_labels, split, show_trend_p, show_total_n),
    tags$tbody(
      lapply(seq_len(nrow(tab)), function(row_index) {
        tags$tr(
          tags$td(class = "crosstab-row-variable", if (row_index == 1) result$row_label else ""),
          tags$td(class = "crosstab-row-label", row_labels[[row_index]]),
          crosstab_total_cells(tab, row_index, split, show_total_n),
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

crosstab_column_group_table_ui <- function(results, method_notes = crosstab_method_footnotes(results), col_names = NULL) {
  first <- results[[1]]
  col_names <- col_names %||% crosstab_column_group_colnames(results)
  col_labels <- crosstab_value_labels(first$col_var, col_names, first$category_table)
  split <- crosstab_split_count_percent(first)
  show_trend_p <- crosstab_show_trend_p(results)
  show_total_n <- crosstab_show_total_n(first)
  min_width <- crosstab_column_group_min_width(results, show_trend_p)

  tags$table(
    class = "coefficient-table crosstab-main-table",
    style = result_table_style(font_size = 15, min_width = min_width),
    tags$colgroup(crosstab_colgroup_tags(length(col_names), split, show_trend_p, show_total_n)),
    crosstab_header_tags(first$col_label, col_labels, split, show_trend_p, show_total_n),
    tags$tbody(
      lapply(results, function(result) {
        tab <- crosstab_align_table_columns(result$table, col_names)
        percent <- crosstab_primary_percent_matrix(result, tab)
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
            crosstab_total_cells(tab, row_index, split, crosstab_show_total_n(result)),
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
    ifelse(nzchar(method_notes$marker), sprintf("%s. %s", method_notes$marker, method_notes$note), method_notes$note)
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
  col_chunks <- crosstab_column_group_split_colnames(results)
  landscape_class <- if (isTRUE(crosstab_column_group_landscape(results))) {
    " landscape-table-panel"
  } else {
    ""
  }
  tagList(lapply(seq_along(col_chunks), function(chunk_index) {
    div(
      class = paste0("result-section crosstab-result-section regression-result-panel", landscape_class),
      h3(sprintf("Cross-tabulation: %s%s", first$col_label, crosstab_column_split_label(chunk_index, length(col_chunks)))),
      div(class = "frequency-table-wrap crosstab-table-wrap", crosstab_column_group_table_ui(results, method_notes, col_chunks[[chunk_index]])),
      crosstab_column_group_notes_ui(results, method_notes)
    )
  }))
}

crosstab_single_result_ui <- function(result) {
  method_notes <- crosstab_method_footnotes(result)
  col_chunks <- crosstab_primary_split_colnames(result)
  landscape_class <- if (isTRUE(crosstab_primary_table_landscape(result))) {
    " landscape-table-panel"
  } else {
    ""
  }
  tagList(
    lapply(seq_along(col_chunks), function(chunk_index) {
      div(
        class = paste0("result-section crosstab-result-section regression-result-panel", landscape_class),
        h3(sprintf("Cross-tabulation: %s x %s%s", result$row_label, result$col_label, crosstab_column_split_label(chunk_index, length(col_chunks)))),
        div(class = "frequency-table-wrap crosstab-table-wrap", crosstab_main_table_ui(result, method_notes, col_chunks[[chunk_index]])),
        crosstab_column_group_notes_ui(list(result), method_notes)
      )
    }),
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
    return(NULL)
  }
  results <- crosstab_result_list(result)
  if (length(results) == 0) {
    return(NULL)
  }

  div(
    class = "crosstab-results regression-results",
    lapply(crosstab_results_by_column(results), crosstab_column_group_ui)
  )
}
