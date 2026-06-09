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

paired_rm_short_method <- function(value) {
  value <- as.character(value %||% "")
  switch(
    value,
    "Standard RM ANOVA" = "RM ANOVA",
    "RM ANOVA + Greenhouse-Geisser correction" = "RM ANOVA + GG",
    "RM ANOVA + Wilks' lambda / GG correction" = "RM ANOVA + Wilks",
    "Friedman test" = "Friedman",
    "Cochran's Q test" = "Cochran Q",
    value
  )
}

paired_rm_assumption_value <- function(table, group, statistic, column = "Result") {
  if (!is.data.frame(table) || nrow(table) == 0) return("")
  matched <- table
  if ("Repeated variables" %in% names(matched)) {
    matched <- matched[as.character(matched$`Repeated variables`) == group, , drop = FALSE]
  }
  if (!"Statistics" %in% names(matched)) return("")
  matched <- matched[as.character(matched$Statistics) == statistic, , drop = FALSE]
  if (nrow(matched) == 0 || !column %in% names(matched)) return("")
  as.character(matched[[column]][[1]] %||% "")
}

paired_rm_normality_summary <- function(result, group) {
  method <- paired_rm_assumption_value(result$assumption, group, "Normality method", "Value")
  normality <- paired_rm_assumption_value(result$assumption, group, "Normality", "Result")
  normality <- switch(normality, Satisfied = "\ub9cc\uc871", `Not satisfied` = "\ubd88\ub9cc\uc871", normality)
  if (!nzchar(method) && !nzchar(normality)) return("")
  if (!nzchar(normality)) return(method)
  paste(method, normality)
}

paired_rm_sphericity_summary <- function(result, group) {
  p_value <- paired_rm_assumption_value(result$assumption, group, "Sphericity p", "Value")
  sphericity <- paired_rm_assumption_value(result$assumption, group, "Sphericity", "Result")
  sphericity <- switch(sphericity, Satisfied = "\ub9cc\uc871", `Not satisfied` = "\ubd88\ub9cc\uc871", sphericity)
  if (!nzchar(p_value) && !nzchar(sphericity)) return("")
  parts <- c(if (nzchar(p_value)) paste0("p=", p_value) else "", sphericity)
  paste(parts[nzchar(parts)], collapse = " ")
}

paired_rm_reason_summary <- function(result, group, row) {
  normality <- paired_rm_assumption_value(result$assumption, group, "Normality", "Result")
  sphericity <- paired_rm_assumption_value(result$assumption, group, "Sphericity", "Result")
  posthoc <- as.character(row$`Post-hoc`[[1]] %||% "")
  parts <- c(
    if (identical(normality, "Satisfied")) "\uc815\uaddc\uc131 \ub9cc\uc871" else if (identical(normality, "Not satisfied")) "\uc815\uaddc\uc131 \ubd88\ub9cc\uc871" else "",
    if (identical(sphericity, "Satisfied")) "\uad6c\ud615\uc131 \ub9cc\uc871" else if (identical(sphericity, "Not satisfied")) "\uad6c\ud615\uc131 \ubd88\ub9cc\uc871" else "",
    if (nzchar(posthoc)) "\uc0ac\ud6c4\ubd84\uc11d \uc788\uc74c" else ""
  )
  paste(parts[nzchar(parts)], collapse = "\n")
}

paired_rm_overview_source <- function(result) {
  if (is.data.frame(result$display_table) && nrow(result$display_table) > 0) return(result$display_table)
  if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) return(result$count_table)
  result$table
}

paired_rm_model_overview_table <- function(result) {
  table <- paired_rm_overview_source(result)
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  group_column <- if ("Repeated variables" %in% names(table)) "Repeated variables" else names(table)[[1]]
  rows <- list()
  for (index in seq_len(nrow(table))) {
    item <- table[index, , drop = FALSE]
    group <- as.character(item[[group_column]][[1]] %||% "")
    row <- data.frame(
      `Repeated variables` = group,
      N = as.character(item$N[[1]] %||% ""),
      Analysis = paired_rm_short_method(item$Method[[1]] %||% ""),
      Reason = paired_rm_reason_summary(result, group, item),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(row) <- c("Repeated variables", "N", "\ubd84\uc11d \ubc29\ubc95", "\uc774\uc720")
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

paired_rm_assumption_review_table <- function(result) {
  table <- paired_rm_overview_source(result)
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  group_column <- if ("Repeated variables" %in% names(table)) "Repeated variables" else names(table)[[1]]
  rows <- list()
  for (index in seq_len(nrow(table))) {
    item <- table[index, , drop = FALSE]
    group <- as.character(item[[group_column]][[1]] %||% "")
    values <- stats::setNames(
      list(
        paired_rm_normality_summary(result, group),
        paired_rm_sphericity_summary(result, group),
        as.character(item$`Post-hoc`[[1]] %||% ""),
        "stats"
      ),
      c("\uc815\uaddc\uc131", "\uad6c\ud615\uc131", "\uc0ac\ud6c4\ubd84\uc11d", "\ud328\ud0a4\uc9c0")
    )
    metric_index <- 0L
    for (metric in names(values)) {
      metric_index <- metric_index + 1L
      rows[[length(rows) + 1L]] <- data.frame(
        `Repeated variables` = if (metric_index == 1L) group else "",
        Item = metric,
        Result = values[[metric]],
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  if (length(rows) == 0) NULL else do.call(rbind, rows)
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
    effect_parts <- c(effect_parts, paste0("overall = ", paste(overall_effect_methods, collapse = ", ")))
  }
  if (length(pairwise_effect_methods) > 0) {
    effect_parts <- c(effect_parts, paste0("pairwise = ", paste(pairwise_effect_methods, collapse = ", ")))
  }
  effect_note <- if (length(effect_parts) > 0) paste0("ES = effect size (", paste(effect_parts, collapse = "; "), ").") else ""
  posthoc_methods <- unique(as.character(table$PosthocMethodLabel %||% ""))
  posthoc_methods <- posthoc_methods[nzchar(posthoc_methods)]
  posthoc_adjustments <- unique(as.character(table$PosthocAdjustmentLabel %||% ""))
  posthoc_adjustments <- posthoc_adjustments[nzchar(posthoc_adjustments)]
  posthoc_note <- if (length(posthoc_methods) > 0) {
    adjustment_text <- if (length(posthoc_adjustments) == 1L) paste0(" with ", posthoc_adjustments[[1]], " adjustment") else ""
    paste0("Post-hoc: ", paste(posthoc_methods, collapse = ", "), adjustment_text, ".")
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
    parts <- c(parts, paste0("ES = effect size (", paste(effect_methods, collapse = ", "), ")."))
  }
  paste(parts, collapse = " ")
}

paired_rm_grouped_column_class <- function(column) {
  if (identical(column, "Repeated variables")) return("paired-rm-col-variable")
  if (identical(column, "N")) return("paired-rm-col-n")
  if (grepl("^Time[0-9]+_(M|SD|0|1)$", column)) return("paired-rm-col-time")
  if (identical(column, "Statistic")) return("paired-rm-col-stat")
  if (identical(column, "p")) return("paired-rm-col-p")
  if (identical(column, "ES_overall") || grepl("^ES_[0-9]+_[0-9]+$", column)) return("paired-rm-col-es")
  if (identical(column, "Post-hoc")) return("paired-rm-col-posthoc")
  "paired-rm-col-default"
}

paired_rm_grouped_column_widths <- function(body_columns) {
  widths <- rep(0, length(body_columns))
  names(widths) <- body_columns
  has_posthoc <- "Post-hoc" %in% body_columns
  es_columns <- body_columns[body_columns == "ES_overall" | grepl("^ES_[0-9]+_[0-9]+$", body_columns)]
  time_columns <- body_columns[grepl("^Time[0-9]+_(M|SD|0|1)$", body_columns)]

  widths[body_columns == "Repeated variables"] <- 15
  widths[body_columns == "N"] <- 5
  widths[body_columns == "Statistic"] <- 6
  widths[body_columns == "p"] <- 5
  widths[body_columns == "Post-hoc"] <- if (has_posthoc) 15 else 0
  widths[body_columns %in% es_columns] <- if (length(es_columns) > 0) 6 else 0

  fixed_width <- sum(widths)
  time_width <- if (length(time_columns) > 0) {
    max(3, (100 - fixed_width) / length(time_columns))
  } else {
    0
  }
  widths[body_columns %in% time_columns] <- time_width
  total <- sum(widths)
  if (is.finite(total) && total > 100) {
    widths <- widths / total * 100
  }
  widths
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
    paste0(
      result_body_cell_style(first, last, compact = TRUE, compact_width = 44, compact_first_width = 148),
      if (isTRUE(first)) "white-space:normal;" else "white-space:nowrap;"
    )
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
  column_widths <- paired_rm_grouped_column_widths(body_columns)
  time_header <- lapply(time_indices, function(index) {
    labels <- as.character(table[[paste0("Time", index, "_label")]] %||% "")
    labels <- labels[nzchar(labels)]
    label <- if (length(labels) > 0) labels[[1]] else paste0("Time ", index)
    markers <- as.character(table[[paste0("Time", index, "_marker")]] %||% "")
    markers <- markers[nzchar(markers)]
    marker <- if (length(markers) > 0) markers[[1]] else letters[[index]]
    tags$th(colspan = 2, style = header_style(FALSE), paired_rm_sup_header(label, marker))
  })
  tags$table(
    class = "coefficient-table paired-grouped-table paired-rm-grouped-table",
    style = paste0(
      result_table_style(font_size = 12, min_width = 520),
      "table-layout:fixed;width:100%;min-width:0;max-width:100%;"
    ),
    tags$colgroup(lapply(body_columns, function(column) {
      tags$col(
        class = paired_rm_grouped_column_class(column),
        style = sprintf("width:%.3f%% !important;", column_widths[[column]])
      )
    })),
    tags$thead(
      tags$tr(
        tags$th(rowspan = 2, style = header_style(TRUE), "Repeated variables"),
        tags$th(rowspan = 2, style = header_style(FALSE), "N"),
        time_header,
        tags$th(rowspan = 2, style = header_style(FALSE), statistic_label),
        tags$th(rowspan = 2, style = header_style(FALSE), "p"),
        if (include_es) tags$th(colspan = 1L + length(es_columns), style = header_style(FALSE), "ES"),
        tags$th(rowspan = 2, style = header_style(FALSE), "Post-hoc")
      ),
      tags$tr(
        lapply(time_indices, function(index) {
          tagList(
            tags$th(style = header_style(FALSE), if (identical(type, "count")) "0" else center_label),
            tags$th(style = header_style(FALSE), if (identical(type, "count")) "1" else spread_label)
          )
        }),
        if (include_es) tags$th(style = header_style(FALSE), as.character(table$ES_overall_label[[1]] %||% "overall")),
        lapply(es_labels, function(label) tags$th(style = header_style(FALSE), label))
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
    if (is.data.frame(paired_rm_model_overview_table(result)) && nrow(paired_rm_model_overview_table(result)) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Model overview"),
        model_overview_html_table(paired_rm_model_overview_table(result))
      )
    },
    if (is.data.frame(result$display_table) && nrow(result$display_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel landscape-table-panel",
        tags$h3("Repeated-measures test: continuous / ordinal"),
        result_table_with_notes(
          paired_rm_grouped_table(result$display_table, "scale"),
          result_note_tag(paired_rm_table_method_note(result$display_table)),
          class = "result-table-with-note paired-fit-table-wrap"
        )
      )
    },
    if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel landscape-table-panel",
        tags$h3("Repeated-measures test: binary"),
        result_table_with_notes(
          paired_rm_grouped_table(result$count_table, "count"),
          result_note_tag(paired_rm_table_method_note(result$count_table)),
          class = "result-table-with-note paired-fit-table-wrap"
        )
      )
    },
    if (
      (!is.data.frame(result$display_table) || nrow(result$display_table) == 0) &&
        (!is.data.frame(result$count_table) || nrow(result$count_table) == 0) &&
        is.data.frame(result$table) &&
        nrow(result$table) > 0
    ) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel landscape-table-panel",
        tags$h3("Repeated-measures test"),
        coefficient_html_table(result$table, note_line = paired_rm_method_note(result))
      )
    },
    if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel landscape-table-panel",
        tags$h3("Post-hoc pairwise comparisons"),
        coefficient_html_table(result$posthoc, note_line = paired_rm_posthoc_note(result))
      )
    },
    if (is.data.frame(paired_rm_assumption_review_table(result)) && nrow(paired_rm_assumption_review_table(result)) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("\uac00\uc815 \uac80\ud1a0"),
        model_overview_html_table(paired_rm_assumption_review_table(result))
      )
    },
    analysis_diagnostics_section(
      NULL,
      result$skipped,
      title = "Warnings / skipped repeated-measures rows",
      class = "result-section paired-result-section regression-result-panel paired-diagnostics-panel"
    )
  )
}
