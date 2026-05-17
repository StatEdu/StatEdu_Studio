# Paired test (three or more repeated measurements) result UI.

paired_rm_statistic_label <- function(table) {
  labels <- unique(as.character(table$StatisticLabel %||% ""))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 1L) labels[[1]] else "Statistic"
}

paired_rm_method_note <- function(result) {
  table <- result$display_table %||% result$count_table %||% result$table
  methods <- unique(as.character(table$Method %||% ""))
  methods <- methods[nzchar(methods)]
  if (length(methods) == 0) return("")
  paste0("Analysis method: ", paste(methods, collapse = ", "), ".")
}

paired_rm_table_method_note <- function(table) {
  methods <- unique(as.character(table$Method %||% ""))
  methods <- methods[nzchar(methods)]
  parts <- character(0)
  if (length(methods) > 0) {
    parts <- c(parts, paste0("Analysis method: ", paste(methods, collapse = ", "), "."))
  }
  effect_methods <- unique(as.character(table$EffectSizeLabel %||% ""))
  effect_methods <- effect_methods[nzchar(effect_methods)]
  if (length(effect_methods) > 0) {
    parts <- c(parts, paste0("Effect size: ", paste(effect_methods, collapse = ", "), "."))
  }
  paste(parts, collapse = " ")
}

paired_rm_posthoc_note <- function(result) {
  adjustment <- if (identical(result$options$posthoc_adjustment %||% "holm", "bonferroni")) "Bonferroni correction" else "Holm Bonferroni"
  methods <- unique(as.character(result$posthoc$Method %||% ""))
  methods <- methods[nzchar(methods)]
  effect_methods <- character(0)
  if (any(identical(methods, "Paired t-test"))) effect_methods <- c(effect_methods, "Hedges' g")
  if (any(identical(methods, "Wilcoxon signed-rank test"))) effect_methods <- c(effect_methods, "r")
  parts <- c(paste0("P values were adjusted using ", adjustment, "."))
  if (length(effect_methods) > 0) {
    parts <- c(parts, paste0("Effect size: ", paste(effect_methods, collapse = ", "), "."))
  }
  paste(parts, collapse = " ")
}

paired_rm_grouped_table <- function(table, type = c("scale", "count")) {
  type <- match.arg(type)
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  time_label_columns <- grep("^Time[0-9]+_label$", names(table), value = TRUE)
  if (length(time_label_columns) == 0) {
    return(coefficient_html_table(table))
  }
  time_indices <- as.integer(sub("^Time([0-9]+)_label$", "\\1", time_label_columns))
  time_indices <- sort(time_indices)
  statistic_label <- paired_rm_statistic_label(table)
  body_columns <- c(
    "Repeated variables",
    "N",
    if (identical(type, "count")) {
      as.vector(rbind(paste0("Time", time_indices, "_0"), paste0("Time", time_indices, "_1")))
    } else {
      as.vector(rbind(paste0("Time", time_indices, "_M"), paste0("Time", time_indices, "_SD")))
    },
    "Statistic",
    "p",
    "ES",
    "Post-hoc"
  )
  body_columns <- body_columns[body_columns %in% names(table)]
  time_header <- lapply(time_indices, function(index) {
    labels <- as.character(table[[paste0("Time", index, "_label")]] %||% "")
    labels <- labels[nzchar(labels)]
    label <- if (length(labels) > 0) labels[[1]] else paste0("Time ", index)
    tags$th(colspan = 2, style = result_header_cell_style(FALSE), label)
  })
  tags$table(
    class = "coefficient-table paired-grouped-table paired-rm-grouped-table",
    style = result_table_style(font_size = 15, min_width = 760),
    tags$thead(
      tags$tr(
        tags$th(rowspan = 2, style = result_header_cell_style(TRUE), "Repeated variables"),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "N"),
        time_header,
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), statistic_label),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "p"),
        if ("ES" %in% names(table)) tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "ES"),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "Post-hoc")
      ),
      tags$tr(
        lapply(time_indices, function(index) {
          tagList(
            tags$th(style = result_header_cell_style(FALSE), if (identical(type, "count")) "0" else "M"),
            tags$th(style = result_header_cell_style(FALSE), if (identical(type, "count")) "1" else "SD")
          )
        })
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(seq_along(body_columns), function(column_index) {
          column <- body_columns[[column_index]]
          tags$td(
            style = result_body_cell_style(column_index == 1, row_index == nrow(table)),
            table[[column]][[row_index]] %||% ""
          )
        }))
      })
    )
  )
}

paired_rm_results_ui <- function(result) {
  if (is.null(result)) {
    return(empty_message("Move three or more repeated-measures variables and click Run analysis."))
  }
  if (is.list(result) && !is.null(result$error)) {
    return(empty_message(result$error))
  }
  tags$div(
    class = "regression-results paired-results paired-rm-results",
    if (is.data.frame(result$display_table) && nrow(result$display_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Repeated-measures test: continuous / ordinal"),
        paired_rm_grouped_table(result$display_table, "scale"),
        tags$div(class = "coefficient-note", paired_rm_table_method_note(result$display_table))
      )
    },
    if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Repeated-measures test: binary"),
        paired_rm_grouped_table(result$count_table, "count"),
        tags$div(class = "coefficient-note", paired_rm_table_method_note(result$count_table))
      )
    },
    if ((!is.data.frame(result$display_table) || nrow(result$display_table) == 0) && (!is.data.frame(result$count_table) || nrow(result$count_table) == 0)) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Repeated-measures test"),
        coefficient_html_table(result$table),
        tags$div(class = "coefficient-note", paired_rm_method_note(result))
      )
    },
    if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Post-hoc pairwise comparisons"),
        coefficient_html_table(result$posthoc),
        tags$div(class = "coefficient-note", paired_rm_posthoc_note(result))
      )
    },
    if (isTRUE(result$options$assumption_check) && is.data.frame(result$assumption) && nrow(result$assumption) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Assumption check"),
        coefficient_html_table(result$assumption)
      )
    }
  )
}
