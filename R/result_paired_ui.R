# Paired test result UI.

paired_scale_statistic_label <- function(table) {
  labels <- unique(as.character(table$StatisticLabel %||% ""))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 1L) labels[[1]] else "Statistic"
}

paired_effect_header_label <- function(table) {
  labels <- unique(as.character(table$EffectLabel %||% ""))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 1L) labels[[1]] else "Effect size"
}

paired_effect_labels <- function(table) {
  if (!is.data.frame(table) || !"EffectLabel" %in% names(table)) return(character(0))
  labels <- unique(unlist(strsplit(as.character(table$EffectLabel %||% ""), ";", fixed = TRUE), use.names = FALSE))
  labels <- trimws(labels)
  labels[nzchar(labels)]
}

paired_effect_header_text <- function(label, label_count = 1L) {
  if (label_count == 1L) "Effect size" else label
}

paired_count_statistic_label <- function(table) {
  labels <- unique(as.character(table$StatisticLabel %||% ""))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 1L) labels[[1]] else "Statistic"
}

paired_has_statistic <- function(table) {
  if (!is.data.frame(table) || !"Statistic" %in% names(table)) return(FALSE)
  any(nzchar(as.character(table$Statistic %||% "")))
}

paired_has_effect <- function(table) {
  if (!is.data.frame(table) || !"Effect" %in% names(table)) return(FALSE)
  any(nzchar(as.character(table$Effect %||% "")))
}

paired_effect_note <- function(table, show_effect_size = TRUE) {
  if (!isTRUE(show_effect_size)) return("")
  labels <- paired_effect_labels(table)
  if (length(labels) == 0) return("")
  if (length(labels) == 1L) {
    paste0("Effect size = ", labels[[1]], ".")
  } else {
    paste0("Effect sizes = ", paste(labels, collapse = ", "), ".")
  }
}

paired_method_note <- function(table, show_effect_size = TRUE) {
  if (!is.data.frame(table) || nrow(table) == 0 || !"Method" %in% names(table)) return("")
  methods <- unique(as.character(table$Method))
  methods <- methods[nzchar(methods)]
  if (length(methods) == 0) return("")
  marker_map <- paired_method_marker_map(table)
  method_note <- if (length(marker_map) > 1) {
    marker_notes <- paste(sprintf("%s %s", unname(marker_map), names(marker_map)), collapse = "; ")
    paste0("Analysis method: ", marker_notes, ".")
  } else {
    paste0("Analysis method: ", methods[[1]], ".")
  }
  paste(c(method_note, paired_effect_note(table, show_effect_size)), collapse = " ")
}

paired_count_method_note <- function(result, show_effect_size = TRUE) {
  paired_method_note(result$count_table, show_effect_size = show_effect_size)
}

paired_method_marker_map <- function(table) {
  if (!is.data.frame(table) || !"Method" %in% names(table)) return(character(0))
  methods <- unique(as.character(table$Method))
  methods <- methods[nzchar(methods)]
  if (length(methods) <= 1L) return(character(0))
  stats::setNames(letters[seq_along(methods)], methods)
}

paired_method_marker_for_row <- function(table, row_index) {
  marker_map <- paired_method_marker_map(table)
  if (length(marker_map) == 0 || !"Method" %in% names(table)) return("")
  method <- as.character(table$Method[[row_index]] %||% "")
  named_value(marker_map, method, "")
}

paired_p_value_cell <- function(value, marker) {
  if (!nzchar(marker %||% "")) return(value %||% "")
  tags$span(
    style = "white-space:nowrap;",
    value %||% "",
    tags$sup(style = "margin-left:2px;font-size:75%;vertical-align:super;", marker)
  )
}

paired_effect_value_for_label <- function(table, row_index, effect_label) {
  value <- as.character(table$Effect[[row_index]] %||% "")
  if (!nzchar(value)) return("")
  labels <- trimws(strsplit(as.character(table$EffectLabel[[row_index]] %||% ""), ";", fixed = TRUE)[[1]])
  values <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
  index <- match(effect_label, labels)
  if (is.na(index) || index < 1L || index > length(values)) return("")
  values[[index]]
}

paired_grouped_table <- function(table, type = c("scale", "count"), show_effect_size = FALSE) {
  type <- match.arg(type)
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  show_effect_size <- isTRUE(show_effect_size) && paired_has_effect(table)
  effect_labels <- if (show_effect_size) paired_effect_labels(table) else character(0)
  center_label <- if (isTRUE(attr(table, "median_iqr", exact = TRUE))) "Median" else "M"
  spread_label <- if (isTRUE(attr(table, "median_iqr", exact = TRUE))) "Q1~Q3" else "SD"
  if (identical(type, "scale")) {
    body_columns <- c("Variable", "Pre_M", "Pre_SD", "Post_M", "Post_SD", "Statistic", "p")
    body_columns <- c(body_columns, paste0("Effect:", effect_labels))
    statistic_label <- paired_scale_statistic_label(table)
    headers <- list(
      tags$tr(
        tags$th(rowspan = 2, style = result_header_cell_style(TRUE), "Variable"),
        tags$th(colspan = 2, style = result_header_cell_style(FALSE), "Pre"),
        tags$th(colspan = 2, style = result_header_cell_style(FALSE), "Post"),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), statistic_label),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "p"),
        lapply(effect_labels, function(label) tags$th(rowspan = 2, style = result_header_cell_style(FALSE), paired_effect_header_text(label, length(effect_labels))))
      ),
      tags$tr(
        tags$th(style = result_header_cell_style(FALSE), center_label),
        tags$th(style = result_header_cell_style(FALSE), spread_label),
        tags$th(style = result_header_cell_style(FALSE), center_label),
        tags$th(style = result_header_cell_style(FALSE), spread_label)
      )
    )
  } else {
    post_columns <- grep("^Post_", names(table), value = TRUE)
    post_labels <- sub("^Post_", "", post_columns)
    include_statistic <- paired_has_statistic(table)
    body_columns <- c("Variable", "Pre", post_columns, if (include_statistic) "Statistic", "p", paste0("Effect:", effect_labels))
    statistic_label <- paired_count_statistic_label(table)
    headers <- list(
      tags$tr(
        tags$th(rowspan = 2, style = result_header_cell_style(TRUE), "Variable"),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "Pre"),
        tags$th(colspan = length(post_columns), style = result_header_cell_style(FALSE), "Post"),
        if (include_statistic) tags$th(rowspan = 2, style = result_header_cell_style(FALSE), statistic_label),
        tags$th(rowspan = 2, style = result_header_cell_style(FALSE), "p"),
        lapply(effect_labels, function(label) tags$th(rowspan = 2, style = result_header_cell_style(FALSE), paired_effect_header_text(label, length(effect_labels))))
      ),
      tags$tr(lapply(post_labels, function(label) tags$th(style = result_header_cell_style(FALSE), label)))
    )
  }
  tags$table(
    class = "coefficient-table paired-grouped-table",
    style = result_table_style(font_size = 15, min_width = 640),
    tags$thead(headers),
    tags$tbody(
      lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(seq_along(body_columns), function(column_index) {
          column <- body_columns[[column_index]]
          marker <- if (identical(column, "p")) paired_method_marker_for_row(table, row_index) else ""
          tags$td(
            style = result_body_cell_style(column_index == 1, row_index == nrow(table)),
            if (identical(column, "p")) {
              paired_p_value_cell(table[[column]][[row_index]], marker)
            } else if (startsWith(column, "Effect:")) {
              paired_effect_value_for_label(table, row_index, sub("^Effect:", "", column))
            } else {
              table[[column]][[row_index]] %||% ""
            }
          )
        }))
      })
    )
  )
}

paired_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  if (!is.null(result$error)) {
    return(empty_message(result$error))
  }
  if (identical(result$type, "paired_rm")) {
    return(paired_rm_results_ui(result))
  }
  if (identical(result$type, "paired_combined")) {
    return(tags$div(
      class = "regression-results paired-results paired-combined-results",
      paired_results_ui(result$paired),
      paired_rm_results_ui(result$paired_rm)
    ))
  }
  tags$div(
    class = "regression-results paired-results",
    if (is.data.frame(result$scale_table) && nrow(result$scale_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Paired test: continuous / ordinal"),
        result_table_with_notes(
          paired_grouped_table(result$scale_table, "scale", show_effect_size = isTRUE(result$options$effect_size)),
          result_note_tag(paired_method_note(result$scale_table, show_effect_size = isTRUE(result$options$effect_size)))
        )
      )
    },
    if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Paired test: binary / categorical"),
        result_table_with_notes(
          paired_grouped_table(result$count_table, "count", show_effect_size = isTRUE(result$options$effect_size)),
          result_note_tag(paired_count_method_note(result, show_effect_size = isTRUE(result$options$effect_size)))
        )
      )
    },
    if (isTRUE(result$options$assumption_check) && is.data.frame(result$checks) && nrow(result$checks) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Assumption check"),
        coefficient_html_table(result$checks)
      )
    },
    if (is.data.frame(result$warnings) && nrow(result$warnings) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Warnings"),
        coefficient_html_table(result$warnings)
      )
    },
    if (is.data.frame(result$skipped) && nrow(result$skipped) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Skipped pairs"),
        coefficient_html_table(result$skipped)
      )
    }
  )
}
