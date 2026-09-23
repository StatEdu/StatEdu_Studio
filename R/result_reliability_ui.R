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
  descriptive_columns <- c("Item", "N", "Min", "Max", "M", "SD")
  if (isTRUE(options$normality)) {
    descriptive_columns <- c(descriptive_columns, "Skewness", "Kurtosis")
  }
  table <- descriptives[, intersect(descriptive_columns, names(descriptives)), drop = FALSE]
  if (!is.data.frame(diagnostics) || nrow(diagnostics) == 0) {
    return(table)
  }
  merge(table, diagnostics, by = "Item", all.x = TRUE, sort = FALSE)
}

reliability_primary_item_deleted_column <- function(result, table) {
  if (!is.data.frame(table) || nrow(table) == 0) return(character(0))
  candidates <- switch(
    result$method %||% "",
    ordinal = c("Ordinal alpha if item deleted", "Reliability if item deleted"),
    pearson = c("Cronbach's alpha if item deleted", "Reliability if item deleted"),
    kr20 = c("Reliability if item deleted"),
    c("Reliability if item deleted")
  )
  intersect(candidates, names(table))[1] %||% character(0)
}

reliability_factor_item_analysis_table <- function(result) {
  factors <- result$factors %||% list()
  tables <- lapply(factors, function(item) {
    table <- reliability_item_analysis_table(item)
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    data.frame(Subfactor = item$subfactor %||% "", table, check.names = FALSE)
  })
  tables <- Filter(function(table) is.data.frame(table) && nrow(table) > 0, tables)
  if (length(tables) == 0) return(NULL)

  total_table <- reliability_item_analysis_table(result$total)
  total_column <- reliability_primary_item_deleted_column(result$total, total_table)
  if (is.data.frame(total_table) && length(total_column) == 1L && nzchar(total_column)) {
    total_values <- total_table[, c("Item", total_column), drop = FALSE]
    names(total_values)[2] <- "Total items if item deleted"
    tables <- lapply(tables, function(table) merge(table, total_values, by = "Item", all.x = TRUE, sort = FALSE))
  }

  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  preferred <- c("Subfactor", "Item")
  columns <- c(intersect(preferred, columns), setdiff(columns, preferred))
  tables <- lapply(tables, function(table) {
    missing <- setdiff(columns, names(table))
    for (column in missing) table[[column]] <- ""
    table[, columns, drop = FALSE]
  })
  do.call(rbind, tables)
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
  if (is.data.frame(table) && nrow(table) > 0 && nzchar(result$subfactor %||% "")) {
    table <- data.frame(`Subfactor` = result$subfactor, table, check.names = FALSE)
  }
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

reliability_factor_overview_table <- function(result) {
  factors <- result$factors %||% list()
  rows <- c(
    if (!is.null(result$total)) list(result$total) else list(),
    factors
  )
  tables <- lapply(rows, reliability_overview_table)
  tables <- Filter(function(table) is.data.frame(table) && nrow(table) > 0, tables)
  if (length(tables) == 0) return(NULL)
  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  tables <- lapply(tables, function(table) {
    missing <- setdiff(columns, names(table))
    for (column in missing) table[[column]] <- ""
    table[, columns, drop = FALSE]
  })
  do.call(rbind, tables)
}

reliability_method_note <- function(result) {
  method <- result$method %||% ""
  reason <- switch(
    method,
    pearson = if (isTRUE(result$options$ordinal)) {
      "Pearson correlations were used because items had at least six response categories and met the distributional criteria."
    } else {
      "Pearson correlations were used for continuous items."
    },
    ordinal = "Polychoric correlations were used for ordinal items.",
    kr20 = "KR-20 was used for binary items.",
    ""
  )
  normality_note <- if (is.data.frame(result$normality_table) && nrow(result$normality_table) > 0) {
    "Item normality was defined as |skewness| < 2 and |kurtosis| < 7."
  } else {
    ""
  }
  displayed <- names(reliability_overview_table(result))
  abbreviation_note <- paste(c(
    if (any(grepl("alpha", displayed, ignore.case = TRUE))) "alpha = Cronbach's or ordinal alpha",
    if (any(grepl("omega", displayed, ignore.case = TRUE))) "omega = Pearson or ordinal omega"
  ), collapse = "; ")
  # Journal notes use a stable order: abbreviations first, followed by the
  # estimation rule and its diagnostic threshold.
  result_sci_note_text(abbreviations = abbreviation_note, estimation = c(reason, normality_note))
}

reliability_item_analysis_note <- function(result, language = "en") {
  options <- result$options %||% list()
  if (identical(language, "ko")) {
    notes <- character(0)
    if (isTRUE(options$normality)) {
      notes <- c(notes, "정규성 진단을 위해 왜도와 첨도를 제시했습니다.")
    }
    if (isTRUE(options$reliability_if_deleted)) {
      notes <- c(notes, "문항 제거 시 신뢰도는 각 문항을 제외한 뒤 다시 산출했습니다.")
    }
    if (isTRUE(options$item_total_correlation)) {
      notes <- c(notes, "수정 문항-총점 상관은 해당 문항을 제외한 총점을 사용합니다.")
    }
    if (isTRUE(options$reliability_if_deleted)) {
      notes <- c(notes, "대시(-)는 추정할 수 없음을 뜻합니다.")
    }
    return(paste(notes, collapse = " "))
  }
  notes <- character(0)
  if (isTRUE(options$normality)) {
    notes <- c(notes, "Skewness and kurtosis are reported for the normality option.")
  }
  if (isTRUE(options$reliability_if_deleted)) {
    diagnostics <- result$item_diagnostics
    has_omega_deleted <- is.data.frame(diagnostics) && any(c("Ordinal omega if item deleted", "Pearson omega if item deleted") %in% names(diagnostics))
    if (identical(result$method, "ordinal") && isTRUE(has_omega_deleted)) {
      notes <- c(notes, "Item-deleted columns show Ordinal alpha and Ordinal omega after removing each item.")
    } else if (identical(result$method, "ordinal")) {
      notes <- c(notes, "Item-deleted columns show Ordinal alpha after removing each item.")
    } else if (identical(result$method, "pearson") && isTRUE(has_omega_deleted)) {
      notes <- c(notes, "Item-deleted columns show Cronbach's alpha and Pearson omega after removing each item.")
    } else if (identical(result$method, "pearson")) {
      notes <- c(notes, "Item-deleted columns show Cronbach's alpha after removing each item.")
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
  displayed <- names(reliability_item_analysis_table(result))
  definitions <- c(
    if (any(grepl("alpha", displayed, ignore.case = TRUE))) "alpha = Cronbach's alpha or ordinal alpha",
    if (any(grepl("omega", displayed, ignore.case = TRUE))) "omega = Pearson omega or ordinal omega",
    if (any(grepl("correlation", displayed, ignore.case = TRUE))) "r = correlation"
  )
  if (!identical(language, "en")) {
    definitions <- vapply(definitions, result_appendix_ui_text, character(1), language = language)
    notes <- vapply(notes, result_appendix_ui_text, character(1), language = language)
  }
  result_publication_note(c(paste(definitions, collapse = "; "), notes))
}

reliability_note_tag <- function(text, width = 688) {
  if (length(text) == 0 || !nzchar(text[[1]])) {
    return(NULL)
  }
  result_note_div(
    class = "coefficient-note reliability-note",
    style = sprintf("width:min(100%%,%dpx);max-width:%dpx;overflow-wrap:break-word;word-break:normal;", as.integer(width), as.integer(width)),
    text
  )
}

reliability_header_label <- function(column, language = "en") {
  if (!language %in% c("en", "ko")) return(result_appendix_ui_text(column, language))
  if (identical(language, "ko")) {
    return(switch(
      column,
      `Total items if item deleted` = htmltools::HTML("전체 문항<br>제거 시"),
      `Reliability if item deleted` = htmltools::HTML("신뢰도<br>문항 제거 시"),
      `Corrected item-total correlation` = htmltools::HTML("수정 문항-총점<br>상관"),
      `Item-total correlation` = htmltools::HTML("문항-총점<br>상관"),
      `Cronbach's alpha if item deleted` = htmltools::HTML("alpha<br>문항 제거 시"),
      `Pearson omega if item deleted` = htmltools::HTML("omega<br>문항 제거 시"),
      `Ordinal alpha if item deleted` = htmltools::HTML("alpha<br>문항 제거 시"),
      `Ordinal omega if item deleted` = htmltools::HTML("omega<br>문항 제거 시"),
      htmltools::HTML(result_appendix_ui_text(column, language))
    ))
  }
  if (identical(column, "Total items if item deleted")) {
    return(htmltools::HTML("Total<br>if deleted"))
  }
  switch(
    column,
    `Measurement level` = htmltools::HTML("Measure"),
    `Cronbach's alpha` = htmltools::HTML("alpha"),
    `Pearson omega` = htmltools::HTML("omega"),
    `Ordinal alpha` = htmltools::HTML("alpha"),
    `Ordinal omega` = htmltools::HTML("omega"),
    `Reliability` = htmltools::HTML("Rel."),
    `Reliability if item deleted` = htmltools::HTML("Rel.<br>if deleted"),
    `Corrected item-total correlation` = htmltools::HTML("Corrected r"),
    `Item-total correlation` = htmltools::HTML("Item-total<br>r"),
    `Cronbach's alpha if item deleted` = htmltools::HTML("alpha<br>if deleted"),
    `Pearson omega if item deleted` = htmltools::HTML("omega<br>if deleted"),
    `Ordinal alpha if item deleted` = htmltools::HTML("alpha<br>if deleted"),
    `Ordinal omega if item deleted` = htmltools::HTML("omega<br>if deleted"),
    `Skewness` = htmltools::HTML("Skew"),
    `Kurtosis` = htmltools::HTML("Kurt"),
    column
  )
}

reliability_display_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(table)
  }
  table
}

reliability_column_weight <- function(column, first = FALSE) {
  if (isTRUE(first)) {
    return(16)
  }
  if (column %in% c("Reliability if item deleted", "Total items if item deleted")) {
    return(8)
  }
  if (column %in% c("Corrected item-total correlation", "Item-total correlation")) {
    return(if (identical(column, "Corrected item-total correlation")) 10 else 8)
  }
  if (column %in% c("Cronbach's alpha if item deleted", "Pearson omega if item deleted", "Ordinal alpha if item deleted", "Ordinal omega if item deleted")) {
    return(8)
  }
  if (identical(column, "Method")) {
    return(18)
  }
  if (identical(column, "Measurement level")) {
    return(10)
  }
  if (column %in% c("Cronbach's alpha", "Pearson omega", "Ordinal alpha", "Ordinal omega", "Reliability")) {
    return(9)
  }
  if (column %in% c("Skewness", "Kurtosis")) {
    return(6)
  }
  if (column %in% c("Min", "Max", "M", "SD")) {
    return(5)
  }
  if (identical(column, "Missing")) {
    return(5)
  }
  if (column %in% c("Item", "Subfactor")) {
    return(13)
  }
  6
}

reliability_column_width <- function(column, first = FALSE) {
  paste0(reliability_column_weight(column, first), "%")
}

reliability_column_widths <- function(columns) {
  weights <- vapply(seq_along(columns), function(index) {
    reliability_column_weight(columns[[index]], first = index == 1L)
  }, numeric(1))
  if (!any(is.finite(weights)) || sum(weights, na.rm = TRUE) <= 0) {
    return(rep(100 / length(columns), length(columns)))
  }
  weights / sum(weights, na.rm = TRUE) * 100
}

reliability_table_width <- function(table, min_width = 360) {
  688L
}

reliability_cell_style <- function(column, first = FALSE, header = FALSE, last = FALSE, group_start = FALSE, width = NULL) {
  left_aligned <- !isTRUE(header) && (isTRUE(first) || identical(column, "Method"))
  no_wrap_header <- column %in% c("Corrected item-total correlation")
  normal_space <- (isTRUE(header) && !isTRUE(no_wrap_header)) || identical(column, "Method") || isTRUE(first)
  paste0(
    "padding:", if (isTRUE(header)) "4px 3px" else "4px 3px", ";",
    "line-height:", if (isTRUE(header)) "1.12" else "1.2", ";border-left:0;border-right:0;",
    "border-top:", if (isTRUE(group_start)) "2px solid #1f2937" else "0", ";",
    "border-bottom:", if (isTRUE(last)) "0" else if (isTRUE(header)) "2px solid #1f2937" else "1px solid #d7dde5", ";",
    "vertical-align:middle;background:transparent;",
    "box-sizing:border-box;",
    "font-weight:", if (isTRUE(header)) "700" else "400", ";",
    "font-size:", if (isTRUE(header)) "11px" else "12px", ";",
    "white-space:", if (isTRUE(normal_space)) "normal" else "nowrap", ";",
    "overflow-wrap:", if (isTRUE(normal_space)) "break-word" else "normal", ";",
    "word-break:normal;",
    "width:", if (!is.null(width)) sprintf("%.4f%%", width) else reliability_column_width(column, first), ";",
    "min-width:0;max-width:none;",
    "text-align:", if (isTRUE(header)) "center" else if (isTRUE(left_aligned)) "left" else "right", ";"
  )
}

reliability_html_table <- function(table, min_width = 360, table_role = NULL, table_language = NULL) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table <- reliability_display_table(table)
  columns <- names(table)
  widths <- reliability_column_widths(columns)
  table_tag <- tags$table(
    class = "coefficient-table reliability-table",
    style = paste0(
      result_table_style(font_size = 12, min_width = 0),
      "width:100%;min-width:0;max-width:100%;table-layout:fixed;box-sizing:border-box;"
    ),
    tags$colgroup(lapply(widths, function(width) {
      tags$col(style = sprintf("width:%.4f%%;", width))
    })),
    tags$thead(
      tags$tr(lapply(seq_along(columns), function(index) {
        column <- columns[[index]]
        tags$th(
          style = reliability_cell_style(column, first = index == 1, header = TRUE, width = widths[[index]]),
          reliability_header_label(column, table_language %||% "en")
        )
      }))
    ),
    tags$tbody(
      lapply(seq_len(nrow(table)), function(row_index) {
        group_start <- "Subfactor" %in% columns &&
          row_index > 1L &&
          !identical(as.character(table$Subfactor[[row_index]]), as.character(table$Subfactor[[row_index - 1L]]))
        tags$tr(lapply(seq_along(columns), function(column_index) {
          column <- columns[[column_index]]
          tags$td(
            style = reliability_cell_style(
              column,
              first = column_index == 1,
              last = row_index == nrow(table),
              group_start = group_start,
              width = widths[[column_index]]
            ),
            table[[column]][[row_index]] %||% ""
          )
        }))
      })
    )
  )
  intrinsic_width <- result_table_intrinsic_width(
    table,
    first_width = 120,
    default_width = 70,
    min_width = min(590, as.numeric(min_width %||% 360))
  )
  contract <- result_table_contract(
    table,
    role = table_role,
    language = table_language,
    intrinsic_width = intrinsic_width
  )
  result_table_apply_contract(table_tag, contract)
}

reliability_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  if (identical(result$type %||% "", "reliability_factors")) {
    appendix_language <- result_appendix_table_language()
    overview <- reliability_factor_overview_table(result)
    overview_width <- reliability_table_width(overview, min_width = 688)
    item_analysis <- reliability_factor_item_analysis_table(result)
    item_analysis_width <- reliability_table_width(item_analysis, min_width = 688)
    item_note <- reliability_item_analysis_note((result$factors %||% list(result$total))[[1]], appendix_language)
    if (is.data.frame(item_analysis) && nrow(item_analysis) > 0 && "Total items if item deleted" %in% names(item_analysis)) {
      item_note <- paste(
        item_note,
        if (identical(appendix_language, "ko")) {
          "전체 문항 제거 시 신뢰도는 모든 하위요인의 문항에서 해당 문항을 제외한 뒤 산출했습니다."
        } else {
          result_appendix_ui_text("Total items if item deleted is calculated from all items across subfactors after removing each item.", appendix_language)
        }
      )
    }
    return(tagList(
      div(
        class = "reliability-results regression-results",
        div(
          class = "result-section reliability-result-section regression-result-panel",
          style = "width:min(100%,688px);max-width:688px;overflow-x:hidden;box-sizing:border-box;",
          h3("Reliability by subfactor"),
          result_table_with_notes(
            reliability_html_table(overview, min_width = overview_width, table_role = "main", table_language = "en")
          )
        ),
        if (is.data.frame(item_analysis) && nrow(item_analysis) > 0) {
          div(
            class = "result-section reliability-result-section regression-result-panel",
            style = "width:min(100%,688px);max-width:688px;overflow-x:hidden;box-sizing:border-box;",
            h3(result_appendix_ui_text("Item analysis", appendix_language)),
            result_table_with_notes(
              reliability_html_table(item_analysis, min_width = item_analysis_width, table_role = "appendix", table_language = appendix_language),
              reliability_note_tag(item_note, width = item_analysis_width)
            )
          )
        }
      )
    ))
  }
  item_analysis <- reliability_item_analysis_table(result)
  overview <- reliability_overview_table(result)
  appendix_language <- result_appendix_table_language()
  overview_width <- reliability_table_width(overview, min_width = 688)
  item_analysis_width <- reliability_table_width(item_analysis, min_width = 688)
  tagList(
    div(
      class = "reliability-results regression-results",
      div(
        class = "result-section reliability-result-section regression-result-panel",
        style = "width:min(100%,688px);max-width:688px;overflow-x:hidden;box-sizing:border-box;",
        h3("Reliability"),
        result_table_with_notes(
          reliability_html_table(overview, min_width = overview_width, table_role = "main", table_language = "en"),
          reliability_note_tag(result_sci_note_text(estimation = reliability_method_note(result)), width = overview_width)
        )
      ),
      if (is.data.frame(item_analysis) && nrow(item_analysis) > 0) {
        div(
          class = "result-section reliability-result-section regression-result-panel",
          style = "width:min(100%,688px);max-width:688px;overflow-x:hidden;box-sizing:border-box;",
          h3(result_appendix_ui_text("Item analysis", appendix_language)),
          result_table_with_notes(
            reliability_html_table(item_analysis, min_width = item_analysis_width, table_role = "appendix", table_language = appendix_language),
            reliability_note_tag(reliability_item_analysis_note(result, appendix_language), width = item_analysis_width)
          )
        )
      },
      NULL
    )
  )
}
