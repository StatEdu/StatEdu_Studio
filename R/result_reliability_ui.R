# Reliability result UI.

reliability_item_analysis_table <- function(result) {
  options <- result$options %||% list()
  show_item_analysis <- isTRUE(options$normality) ||
    isTRUE(options$reliability_if_deleted) ||
    isTRUE(options$item_total_correlation)
  if (!isTRUE(show_item_analysis)) {
    return(NULL)
  }
  descriptives <- result$item_descriptives
  diagnostics <- result$item_diagnostics
  if (!is.data.frame(descriptives) || nrow(descriptives) == 0) {
    return(NULL)
  }
  descriptive_columns <- c("Item", "N", "Missing", "Min", "Max", "M", "SD")
  if (isTRUE(options$normality)) {
    descriptive_columns <- c(descriptive_columns, "Skewness", "Kurtosis")
  }
  table <- descriptives[, intersect(descriptive_columns, names(descriptives)), drop = FALSE]
  if (!is.data.frame(diagnostics) || nrow(diagnostics) == 0) {
    return(table)
  }
  merge(table, diagnostics, by = "Item", all.x = TRUE, sort = FALSE)
}

reliability_drop_empty_metric_columns <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(table)
  }
  metric_columns <- intersect(c("Pearson omega", "Ordinal omega", "Reliability"), names(table))
  for (column in metric_columns) {
    values <- trimws(as.character(table[[column]] %||% ""))
    if (length(values) > 0 && all(values %in% c("", "-"))) {
      table[[column]] <- NULL
    }
  }
  table
}

reliability_overview_table <- function(result) {
  table <- reliability_drop_empty_metric_columns(result$overview)
  if (is.data.frame(table) && nrow(table) > 0 && isTRUE(result$options$ordinal) && !("Ordinal" %in% names(table))) {
    insert_after <- match("Measurement level", names(table), nomatch = 0)
    ordinal_column <- data.frame(Ordinal = "ON", check.names = FALSE)
    if (insert_after > 0) {
      table <- data.frame(table[seq_len(insert_after)], ordinal_column, table[-seq_len(insert_after)], check.names = FALSE)
    } else {
      table <- data.frame(table, ordinal_column, check.names = FALSE)
    }
  }
  if (is.data.frame(table) && nrow(table) > 0 && "Method" %in% names(table)) {
    if (!("Pearson omega" %in% names(table)) && identical(result$method, "pearson")) {
      table$Method <- "Cronbach's alpha"
    }
    if (!("Ordinal omega" %in% names(table)) && identical(result$method, "ordinal")) {
      table$Method <- "Ordinal alpha"
    }
  }
  table
}

reliability_method_note <- function(result) {
  method <- result$method %||% ""
  recommended <- result$recommended %||% switch(method, pearson = "Pearson Omega", ordinal = "Ordinal Omega", kr20 = "KR-20", "")
  reason <- switch(
    method,
    pearson = if (isTRUE(result$options$ordinal)) {
      "Items were treated as approximately continuous because the response scale had six or more categories and distributional assumptions were acceptable."
    } else {
      "Pearson reliability coefficients were estimated because items were treated as continuous."
    },
    ordinal = "Ordinal reliability coefficients based on polychoric correlations were estimated due to the ordinal response format and/or non-normal item distributions.",
    kr20 = "KR-20 was used for binary items.",
    ""
  )
  normality_note <- if (is.data.frame(result$normality_table) && nrow(result$normality_table) > 0) {
    "Normality was considered satisfied when the absolute skewness was less than 2 and the absolute kurtosis was less than 7 for every item."
  } else {
    ""
  }
  paste(c(sprintf("Recommended reliability method: %s.", recommended), reason, normality_note), collapse = " ")
}

reliability_item_analysis_note <- function(result) {
  options <- result$options %||% list()
  notes <- character(0)
  if (isTRUE(options$normality)) {
    notes <- c(notes, "Skewness and kurtosis are reported for the normality option.")
  }
  if (isTRUE(options$reliability_if_deleted)) {
    if (identical(result$method, "ordinal")) {
      notes <- c(notes, "Item-deleted columns show Ordinal alpha and Ordinal omega after removing each item.")
    } else if (identical(result$method, "pearson")) {
      notes <- c(notes, "Item-deleted columns show Cronbach's alpha and Pearson omega after removing each item.")
    } else {
      notes <- c(notes, "Reliability if item deleted shows the selected reliability coefficient after removing each item.")
    }
  }
  if (isTRUE(options$item_total_correlation)) {
    notes <- c(notes, "Corrected item-total correlation uses the total score excluding the item; item-total correlation uses the full total score.")
  }
  if (isTRUE(options$reliability_if_deleted)) {
    notes <- c(notes, "A dash (-) indicates that the coefficient could not be estimated for that item-deleted model.")
  }
  paste(notes, collapse = " ")
}

reliability_note_tag <- function(text, width = 620) {
  if (length(text) == 0 || !nzchar(text[[1]])) {
    return(NULL)
  }
  div(
    class = "coefficient-note reliability-note",
    style = sprintf("width:%dpx;max-width:100%%;overflow-wrap:break-word;word-break:normal;", as.integer(width)),
    text
  )
}

reliability_header_label <- function(column) {
  switch(
    column,
    `Reliability if item deleted` = htmltools::HTML("Reliability if<br>item deleted"),
    `Corrected item-total correlation` = htmltools::HTML("Corrected<br>item-total<br>correlation"),
    `Item-total correlation` = htmltools::HTML("Item-total<br>correlation"),
    `Cronbach's alpha if item deleted` = htmltools::HTML("Cronbach's alpha if<br>item deleted"),
    `Pearson omega if item deleted` = htmltools::HTML("Pearson omega if<br>item deleted"),
    `Ordinal alpha if item deleted` = htmltools::HTML("Ordinal alpha if<br>item deleted"),
    `Ordinal omega if item deleted` = htmltools::HTML("Ordinal omega if<br>item deleted"),
    column
  )
}

reliability_column_width <- function(column, first = FALSE) {
  if (isTRUE(first)) {
    return("120px")
  }
  if (column %in% c("Reliability if item deleted", "Corrected item-total correlation", "Item-total correlation")) {
    return("136px")
  }
  if (column %in% c("Cronbach's alpha if item deleted", "Pearson omega if item deleted", "Ordinal alpha if item deleted", "Ordinal omega if item deleted")) {
    return(if (identical(column, "Cronbach's alpha if item deleted")) "164px" else "148px")
  }
  if (identical(column, "Method")) {
    return("240px")
  }
  if (identical(column, "Measurement level")) {
    return("132px")
  }
  if (column %in% c("Cronbach's alpha", "Pearson omega", "Ordinal alpha", "Ordinal omega", "Reliability")) {
    return(if (identical(column, "Cronbach's alpha")) "124px" else "104px")
  }
  if (column %in% c("Item")) {
    return("120px")
  }
  "76px"
}

reliability_table_width <- function(table, min_width = 360) {
  if (!is.data.frame(table) || ncol(table) == 0) {
    return(min_width)
  }
  column_widths <- vapply(seq_along(table), function(index) {
    column <- names(table)[[index]]
    width <- reliability_column_width(column, first = index == 1)
    suppressWarnings(as.numeric(gsub("[^0-9.]", "", width)))
  }, numeric(1))
  total <- sum(column_widths[is.finite(column_widths)], na.rm = TRUE)
  max(as.integer(min_width), as.integer(total))
}

reliability_cell_style <- function(column, first = FALSE, header = FALSE, last = FALSE) {
  left_aligned <- isTRUE(first) || identical(column, "Method")
  normal_space <- isTRUE(header) || identical(column, "Method")
  paste0(
    "padding:", if (isTRUE(header)) "7px 10px" else "6px 10px", ";",
    "line-height:1.25;border-left:0;border-right:0;",
    "border-top:0;border-bottom:", if (isTRUE(last)) "0" else if (isTRUE(header)) "2px solid #1f2937" else "1px solid #d7dde5", ";",
    "vertical-align:middle;background:transparent;",
    "box-sizing:border-box;",
    "font-weight:", if (isTRUE(header)) "700" else "400", ";",
    "white-space:", if (isTRUE(normal_space)) "normal" else "nowrap", ";",
    "min-width:", reliability_column_width(column, first), ";",
    "max-width:", reliability_column_width(column, first), ";",
    "text-align:", if (isTRUE(left_aligned)) "left" else "right", ";"
  )
}

reliability_html_table <- function(table, min_width = 360) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  columns <- names(table)
  tags$table(
    class = "coefficient-table reliability-table",
    style = result_table_style(font_size = 14, min_width = min_width),
    tags$thead(
      tags$tr(lapply(seq_along(columns), function(index) {
        column <- columns[[index]]
        tags$th(
          style = reliability_cell_style(column, first = index == 1, header = TRUE),
          reliability_header_label(column)
        )
      }))
    ),
    tags$tbody(
      lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(seq_along(columns), function(column_index) {
          column <- columns[[column_index]]
          tags$td(
            style = reliability_cell_style(column, first = column_index == 1, last = row_index == nrow(table)),
            table[[column]][[row_index]] %||% ""
          )
        }))
      })
    )
  )
}

reliability_results_ui <- function(result) {
  if (is.null(result)) {
    return(empty_message("Move items and click Run analysis."))
  }
  item_analysis <- reliability_item_analysis_table(result)
  overview <- reliability_overview_table(result)
  overview_width <- reliability_table_width(overview, min_width = 620)
  item_analysis_width <- reliability_table_width(item_analysis, min_width = 760)
  tagList(
    div(
      class = "reliability-results regression-results",
      div(
        class = "result-section reliability-result-section regression-result-panel",
        h3("Reliability"),
        reliability_html_table(overview, min_width = overview_width),
        reliability_note_tag(reliability_method_note(result), width = overview_width)
      ),
      if (is.data.frame(item_analysis) && nrow(item_analysis) > 0) {
        div(
          class = "result-section reliability-result-section regression-result-panel",
          h3("Item analysis"),
          reliability_html_table(item_analysis, min_width = item_analysis_width),
          reliability_note_tag(reliability_item_analysis_note(result), width = item_analysis_width)
        )
      },
      NULL
    )
  )
}
