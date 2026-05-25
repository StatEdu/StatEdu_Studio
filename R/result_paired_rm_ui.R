# Paired test (three or more repeated measurements) result UI.

paired_rm_statistic_label <- function(table) {
  labels <- unique(as.character(table$StatisticLabel %||% ""))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 1L) labels[[1]] else "Statistic"
}

paired_rm_method_marker_map <- function(table) {
  if (!is.data.frame(table) || !"Method" %in% names(table)) return(character(0))
  methods <- unique(as.character(table$Method))
  methods <- methods[nzchar(methods)]
  if (length(methods) <= 1L) return(character(0))
  stats::setNames(letters[seq_along(methods)], methods)
}

paired_rm_method_marker_for_row <- function(table, row_index) {
  marker_map <- paired_rm_method_marker_map(table)
  if (length(marker_map) == 0 || !"Method" %in% names(table)) return("")
  method <- as.character(table$Method[[row_index]] %||% "")
  named_value(marker_map, method, "")
}

paired_rm_p_value_cell <- function(value, marker) {
  if (!nzchar(marker %||% "")) return(value %||% "")
  tags$span(
    style = "white-space:nowrap;",
    value %||% "",
    tags$sup(style = "margin-left:2px;font-size:75%;vertical-align:super;", marker)
  )
}

paired_rm_sup_header <- function(label, marker) {
  tags$span(
    style = "white-space:nowrap;",
    label,
    tags$sup(style = "margin-left:2px;font-size:75%;vertical-align:super;", marker)
  )
}

paired_rm_method_note <- function(result) {
  table <- result$display_table %||% result$count_table %||% result$table
  methods <- unique(as.character(table$Method %||% ""))
  methods <- methods[nzchar(methods)]
  if (length(methods) == 0) return("")
  paste0("Analysis method: ", paste(methods, collapse = ", "), ".")
}

paired_rm_table_method_note <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) return("")
  marker_map <- paired_rm_method_marker_map(table)
  methods <- unique(as.character(table$Method %||% ""))
  methods <- methods[nzchar(methods)]
  method_note <- if (length(marker_map) > 0) {
    paste0("Analysis method: ", paste(sprintf("%s %s", unname(marker_map), names(marker_map)), collapse = "; "), ".")
  } else if (length(methods) == 1L) {
    if (
      identical(methods[[1]], "Standard RM ANOVA") &&
        "Sphericity" %in% names(table) &&
        "Sphericity p" %in% names(table) &&
        any(as.character(table$Sphericity %||% "") == "Satisfied", na.rm = TRUE)
    ) {
      p_values <- unique(as.character(table$`Sphericity p` %||% ""))
      p_values <- p_values[nzchar(p_values)]
      p_note <- if (length(p_values) == 1L) paste0("(p=", p_values[[1]], ") ") else ""
      paste0("Analysis method: Sphericity ", p_note, "satisfied; RM ANOVA.")
    } else {
      paste0("Analysis method: ", methods[[1]], ".")
    }
  } else {
    ""
  }
  overall_effect_methods <- unique(as.character(table$EffectSizeLabel %||% ""))
  overall_effect_methods <- overall_effect_methods[nzchar(overall_effect_methods)]
  pairwise_effect_methods <- unique(as.character(table$PairwiseEffectSizeLabel %||% ""))
  pairwise_effect_methods <- pairwise_effect_methods[nzchar(pairwise_effect_methods)]
  effect_parts <- character(0)
  if (length(overall_effect_methods) > 0) {
    effect_parts <- c(effect_parts, paste0("a overall = ", paste(overall_effect_methods, collapse = ", ")))
  }
  if (length(pairwise_effect_methods) > 0) {
    effect_parts <- c(effect_parts, paste0("b pairwise = ", paste(pairwise_effect_methods, collapse = ", ")))
  }
  effect_note <- if (length(effect_parts) > 0) paste0("Effect size: ", paste(effect_parts, collapse = "; "), ".") else ""
  posthoc_methods <- unique(as.character(table$PosthocMethodLabel %||% ""))
  posthoc_methods <- posthoc_methods[nzchar(posthoc_methods)]
  posthoc_adjustments <- unique(as.character(table$PosthocAdjustmentLabel %||% ""))
  posthoc_adjustments <- posthoc_adjustments[nzchar(posthoc_adjustments)]
  posthoc_note <- if (length(posthoc_methods) > 0) {
    adjustment_text <- if (length(posthoc_adjustments) == 1L) paste0(" with ", posthoc_adjustments[[1]], " adjustment") else ""
    paste0("Post-hoc: c ", paste(posthoc_methods, collapse = ", "), adjustment_text, ".")
  } else {
    ""
  }
  correction_notes <- vapply(seq_len(nrow(table)), function(index) {
    group <- as.character(table$`Repeated variables`[[index]] %||% "")
    details <- character(0)
    if ("Wilks' lambda" %in% names(table) && nzchar(as.character(table$`Wilks' lambda`[[index]] %||% ""))) {
      details <- c(details, paste0("Wilks' lambda = ", table$`Wilks' lambda`[[index]]))
    }
    if ("GG epsilon" %in% names(table) && nzchar(as.character(table$`GG epsilon`[[index]] %||% ""))) {
      details <- c(details, paste0("GG epsilon = ", table$`GG epsilon`[[index]]))
    }
    if ("GG p" %in% names(table) && nzchar(as.character(table$`GG p`[[index]] %||% ""))) {
      details <- c(details, paste0("GG p = ", table$`GG p`[[index]]))
    }
    if (length(details) == 0) return("")
    paste0(group, ": ", paste(details, collapse = "; "), ".")
  }, character(1))
  paste(c(method_note, effect_note, posthoc_note, correction_notes[nzchar(correction_notes)]), collapse = " ")
}

paired_rm_posthoc_note <- function(result) {
  adjustment <- if (identical(result$options$posthoc_adjustment %||% "bonferroni", "holm")) "Holm Bonferroni" else "Bonferroni correction"
  methods <- unique(as.character(result$posthoc$Method %||% ""))
  methods <- methods[nzchar(methods)]
  effect_methods <- character(0)
  if (any(methods == "Paired t-test")) effect_methods <- c(effect_methods, "Hedges' g")
  if (any(methods == "Wilcoxon signed-rank test")) effect_methods <- c(effect_methods, "r")
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
  es_columns <- grep("^ES_[0-9]+_[0-9]+$", names(table), value = TRUE)
  es_label_columns <- paste0(es_columns, "_label")
  es_labels <- vapply(seq_along(es_columns), function(index) {
    label_column <- es_label_columns[[index]]
    labels <- as.character(table[[label_column]] %||% "")
    labels <- labels[nzchar(labels)]
    if (length(labels) > 0) labels[[1]] else sub("^ES_", "", es_columns[[index]])
  }, character(1))
  include_es <- "ES_overall" %in% names(table) || length(es_columns) > 0
  center_label <- if (isTRUE(attr(table, "median_iqr", exact = TRUE))) "Median" else "M"
  spread_label <- if (isTRUE(attr(table, "median_iqr", exact = TRUE))) "Q1~Q3" else "SD"
  header_style <- function(first = FALSE) {
    paste0(result_header_cell_style(first, compact = TRUE, compact_width = 44, compact_first_width = 148), if (!isTRUE(first)) "text-align:center;" else "")
  }
  body_style <- function(first = FALSE, last = FALSE) {
    result_body_cell_style(first, last, compact = TRUE, compact_width = 44, compact_first_width = 148)
  }
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
    if (include_es) "ES_overall",
    es_columns,
    "Post-hoc"
  )
  body_columns <- body_columns[body_columns %in% names(table)]
  time_header <- lapply(time_indices, function(index) {
    labels <- as.character(table[[paste0("Time", index, "_label")]] %||% "")
    labels <- labels[nzchar(labels)]
    label <- if (length(labels) > 0) labels[[1]] else paste0("Time ", index)
    tags$th(colspan = 2, style = header_style(FALSE), label)
  })
  tags$table(
    class = "coefficient-table paired-grouped-table paired-rm-grouped-table",
    style = result_table_style(font_size = 13, min_width = 520),
    tags$thead(
      tags$tr(
        tags$th(rowspan = 2, style = header_style(TRUE), "Repeated variables"),
        tags$th(rowspan = 2, style = header_style(FALSE), "N"),
        time_header,
        tags$th(rowspan = 2, style = header_style(FALSE), statistic_label),
        tags$th(rowspan = 2, style = header_style(FALSE), "p"),
        if (include_es) tags$th(colspan = 1L + length(es_columns), style = header_style(FALSE), "ES"),
        tags$th(rowspan = 2, style = header_style(FALSE), paired_rm_sup_header("Post-hoc", "c"))
      ),
      tags$tr(
        lapply(time_indices, function(index) {
          tagList(
            tags$th(style = header_style(FALSE), if (identical(type, "count")) "0" else center_label),
            tags$th(style = header_style(FALSE), if (identical(type, "count")) "1" else spread_label)
          )
        }),
        if (include_es) tags$th(style = header_style(FALSE), paired_rm_sup_header(as.character(table$ES_overall_label[[1]] %||% "overall"), "a")),
        lapply(es_labels, function(label) tags$th(style = header_style(FALSE), paired_rm_sup_header(label, "b")))
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(seq_along(body_columns), function(column_index) {
          column <- body_columns[[column_index]]
          tags$td(
            style = body_style(column_index == 1, row_index == nrow(table)),
            if (identical(column, "p")) {
              paired_rm_p_value_cell(table[[column]][[row_index]], paired_rm_method_marker_for_row(table, row_index))
            } else {
              table[[column]][[row_index]] %||% ""
            }
          )
        }))
      })
    )
  )
}

paired_rm_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
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
        result_table_with_notes(
          paired_rm_grouped_table(result$display_table, "scale"),
          result_note_tag(paired_rm_table_method_note(result$display_table))
        )
      )
    },
    if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Repeated-measures test: binary"),
        result_table_with_notes(
          paired_rm_grouped_table(result$count_table, "count"),
          result_note_tag(paired_rm_table_method_note(result$count_table))
        )
      )
    },
    if ((!is.data.frame(result$display_table) || nrow(result$display_table) == 0) && (!is.data.frame(result$count_table) || nrow(result$count_table) == 0)) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Repeated-measures test"),
        coefficient_html_table(result$table, note_line = paired_rm_method_note(result))
      )
    },
    if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Post-hoc pairwise comparisons"),
        coefficient_html_table(result$posthoc, note_line = paired_rm_posthoc_note(result))
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
