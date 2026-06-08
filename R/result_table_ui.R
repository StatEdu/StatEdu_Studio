# HTML table builders for result output.

analysis_result_table_section <- function(title, table, class = "result-section regression-result-panel", table_fn = coefficient_html_table) {
  if (!analysis_has_rows(table)) {
    return(NULL)
  }
  tags$div(
    class = class,
    tags$h3(title),
    table_fn(table)
  )
}

analysis_warning_section <- function(table, class = "result-section regression-result-panel") {
  analysis_result_table_section("Warnings", table, class = class)
}

analysis_skipped_section <- function(table, title = "Skipped analyses", class = "result-section regression-result-panel") {
  analysis_result_table_section(title, table, class = class)
}

analysis_diagnostics_row <- function(table, type) {
  if (!analysis_has_rows(table)) {
    return(NULL)
  }
  pick <- function(candidates) {
    matched <- intersect(candidates, names(table))
    if (length(matched) == 0) {
      return(rep("", nrow(table)))
    }
    as.character(table[[matched[[1]]]])
  }
  data.frame(
    Type = rep(type, nrow(table)),
    `Dependent variable` = pick(c("Dependent variable", "Dependent", "Outcome")),
    `Independent variable` = pick(c("Independent variable", "Independent", "Predictor", "Variable")),
    N = pick(c("N", "n")),
    Message = pick(c("Warning", "Reason", "Message")),
    check.names = FALSE
  )
}

analysis_diagnostics_section <- function(warnings, skipped, title = "Warnings / skipped analyses", class = "result-section regression-result-panel") {
  rows <- Filter(Negate(is.null), list(
    analysis_diagnostics_row(warnings, "Warning"),
    analysis_diagnostics_row(skipped, "Skipped")
  ))
  if (length(rows) == 0) {
    return(NULL)
  }
  table <- do.call(rbind, rows)
  if (all(!nzchar(table$N))) {
    table$N <- NULL
  }
  analysis_result_table_section(title, table, class = class)
}

result_table_style <- function(font_size = 12, min_width = 480) {
  paste(
    sprintf("width:auto;min-width:%dpx;border-collapse:collapse;border-spacing:0;", min_width),
    "border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
    sprintf("color:#2f3a46;font-size:%spx;background:transparent;", as.character(font_size))
  )
}

result_header_cell_style <- function(first = FALSE, compact = FALSE, compact_font_size = 12, compact_width = 62, compact_first_width = 118) {
  padding <- if (isTRUE(compact)) "5px 7px" else "5px 7px"
  width <- if (isTRUE(first)) {
    if (isTRUE(compact)) paste0(compact_first_width, "px") else "90px"
  } else if (isTRUE(compact)) {
    paste0(compact_width, "px")
  } else {
    "58px"
  }
  paste0(
    "padding:", padding, ";line-height:1.35;border-left:0;border-right:0;",
    "border-top:0;border-bottom:2px solid #1f2937;vertical-align:middle;",
    "font-weight:700;background:transparent;white-space:nowrap;",
    "min-width:", width, ";",
    "text-align:", if (isTRUE(first)) "left" else "right", ";"
  )
}

result_body_cell_style <- function(first = FALSE, last = FALSE, compact = FALSE, compact_font_size = 12, compact_width = 62, compact_first_width = 118) {
  padding <- if (isTRUE(compact)) "5px 7px" else "5px 7px"
  width <- if (isTRUE(first)) {
    if (isTRUE(compact)) paste0(compact_first_width, "px") else "90px"
  } else if (isTRUE(compact)) {
    paste0(compact_width, "px")
  } else {
    "58px"
  }
  paste0(
    "padding:", padding, ";line-height:1.35;border-left:0;border-right:0;",
    "border-top:0;border-bottom:", if (isTRUE(last)) "0" else "1px solid #d7dde5", ";",
    "vertical-align:middle;background:transparent;white-space:normal;",
    "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
    "min-width:", width, ";",
    "white-space:pre-line;",
    "text-align:", if (isTRUE(first)) "left" else "right", ";"
  )
}

result_note_tag <- function(text, class = "coefficient-note") {
  if (length(text) == 0 || !nzchar(text[[1]] %||% "")) {
    return(NULL)
  }
  tags$div(class = class, text)
}

result_table_with_notes <- function(table_tag, ..., class = "result-table-with-note") {
  notes <- list(...)
  notes <- Filter(Negate(is.null), notes)
  if (is.null(table_tag)) {
    return(NULL)
  }
  do.call(
    tags$div,
    c(
      list(class = class),
      list(table_tag),
      notes
    )
  )
}

result_cell_note_marker <- function(table, row_index, column) {
  markers <- attr(table, "note_markers", exact = TRUE)
  if (!is.data.frame(markers) || nrow(markers) == 0) {
    return("")
  }
  matched <- markers[markers$row == row_index & markers$column == column, , drop = FALSE]
  if (nrow(matched) == 0) "" else as.character(matched$marker[[1]])
}

result_note_marker_column <- function(column) {
  key <- result_column_key(column)
  key %in% c("p", "pfortrend", "effectsize") ||
    grepl("effect|hedges|cohen|eta|omega|epsilon|cliff|bootp", key)
}

result_split_inline_marker <- function(value, marker = "", column = "") {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value)) {
    return(c(value = value, marker = ""))
  }
  if (nzchar(marker)) {
    spaced_pattern <- paste0("\\s+", marker, "$")
    if (grepl(spaced_pattern, value, perl = TRUE)) {
      return(c(value = trimws(sub(spaced_pattern, "", value, perl = TRUE)), marker = marker))
    }
    compact_pattern <- paste0(marker, "$")
    compact_value <- sub(compact_pattern, "", value, perl = TRUE)
    compact_marker_appended <- !identical(compact_value, value) && (
      grepl("^-?\\.[0-9]{4,}$", value, perl = TRUE) ||
        grepl("^<\\.001[1-9][0-9]?$", value, perl = TRUE)
    )
    if (isTRUE(compact_marker_appended)) {
      return(c(value = compact_value, marker = marker))
    }
    return(c(value = value, marker = marker))
  }
  if (!result_note_marker_column(column)) {
    return(c(value = value, marker = ""))
  }
  spaced <- regexec("^(.+?)\\s+([1-9][0-9]?)$", value, perl = TRUE)
  spaced_match <- regmatches(value, spaced)[[1]]
  if (length(spaced_match) == 3L) {
    return(c(value = trimws(spaced_match[[2]]), marker = spaced_match[[3]]))
  }
  compact <- regexec("^((?:<\\.001)|(?:-?(?:0)?\\.[0-9]{3,}))([1-9][0-9]?)$", value, perl = TRUE)
  compact_match <- regmatches(value, compact)[[1]]
  if (length(compact_match) == 3L) {
    return(c(value = compact_match[[2]], marker = compact_match[[3]]))
  }
  c(value = value, marker = "")
}

result_cell_bold <- function(table, row_index, column) {
  bold_cells <- attr(table, "bold_cells", exact = TRUE)
  if (!is.data.frame(bold_cells) || nrow(bold_cells) == 0) {
    return(FALSE)
  }
  any(bold_cells$row == row_index & bold_cells$column == column)
}

result_cell_style_extra <- function(table, row_index, column) {
  cell_styles <- attr(table, "cell_styles", exact = TRUE)
  if (!is.data.frame(cell_styles) || nrow(cell_styles) == 0) {
    return("")
  }
  matched <- cell_styles[cell_styles$row == row_index & cell_styles$column == column, , drop = FALSE]
  if (nrow(matched) == 0 || !"style" %in% names(matched)) {
    return("")
  }
  paste(as.character(matched$style), collapse = "")
}

result_cell_span_start <- function(table, row_index, column) {
  spans <- attr(table, "spanning_cells", exact = TRUE)
  if (!is.data.frame(spans) || nrow(spans) == 0) {
    return(NULL)
  }
  matched <- spans[spans$row == row_index & spans$start_column == column, , drop = FALSE]
  if (nrow(matched) == 0) NULL else matched[1, , drop = FALSE]
}

result_cell_covered_by_span <- function(table, row_index, column, columns) {
  spans <- attr(table, "spanning_cells", exact = TRUE)
  if (!is.data.frame(spans) || nrow(spans) == 0) {
    return(FALSE)
  }
  column_index <- match(column, columns)
  if (!is.finite(column_index)) {
    return(FALSE)
  }
  any(vapply(seq_len(nrow(spans)), function(index) {
    if (spans$row[[index]] != row_index || spans$start_column[[index]] == column) {
      return(FALSE)
    }
    start_index <- match(spans$start_column[[index]], columns)
    end_index <- match(spans$end_column[[index]], columns)
    is.finite(start_index) && is.finite(end_index) && column_index >= start_index && column_index <= end_index
  }, logical(1)))
}

result_cell_content <- function(value, marker = "", column = "") {
  split <- result_split_inline_marker(value, marker, column)
  value <- split[["value"]]
  marker <- split[["marker"]]
  if (!nzchar(value) || !nzchar(marker)) {
    return(value)
  }
  tags$span(
    class = "coefficient-footnote-value",
    value,
    tags$sup(class = "coefficient-footnote-marker", marker)
  )
}

result_cell_value_without_marker <- function(value, marker = "") {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value) || !nzchar(marker)) {
    return(value)
  }
  sub(paste0(marker, "$"), "", value)
}

coefficient_display_columns <- function(table) {
  columns <- names(table)
  labels <- columns
  labels[result_column_key(labels) == "term"] <- "Variable"
  labels[result_column_key(labels) == "effectsize"] <- "ES"
  labels[result_column_key(labels) == "posthoc"] <- "post\n-hoc"
  data.frame(
    source = columns,
    label = labels,
    marker = FALSE,
    stringsAsFactors = FALSE
  )
}

result_header_content <- function(label) {
  label <- as.character(label %||% "")
  if (!grepl("\n", label, fixed = TRUE)) {
    return(label)
  }
  parts <- strsplit(label, "\n", fixed = TRUE)[[1]]
  tags$span(
    class = "coefficient-header-break",
    lapply(parts, function(part) {
      tags$span(part)
    })
  )
}

coefficient_show_df_column_width <- function(table, column) {
  column_key <- result_column_key(column)
  if (isTRUE(attr(table, "bootstrap_regression", exact = TRUE))) {
    if (column_key == "term") return(330L)
    if (column_key == "b") return(56L)
    if (column_key %in% c("bootse", "hc3se")) return(78L)
    if (column_key %in% c("llci", "ulci")) return(68L)
    if (column_key == "bootp") return(72L)
    if (column_key %in% c("sr2", "f2")) return(48L)
    if (column_key == "tolerance") return(92L)
    if (column_key == "vif") return(60L)
  }
  if (!isTRUE(attr(table, "show_df", exact = TRUE))) {
    return(NA_integer_)
  }
  mean_sd <- isTRUE(attr(table, "mean_sd", exact = TRUE))
  trend_analysis <- isTRUE(attr(table, "trend_analysis", exact = TRUE))
  if (isTRUE(trend_analysis)) {
    if (column_key == "variable") {
      return(110L)
    }
    if (column_key == "value") {
      return(if (isTRUE(mean_sd)) 135L else 130L)
    }
    if (column_key %in% c("msd", "mse", "rankmse")) {
      return(128L)
    }
    if (column_key %in% c("p", "effectsize")) {
      return(if (isTRUE(mean_sd)) 54L else 54L)
    }
    if (column_key == "pfortrend") {
      return(if (isTRUE(mean_sd)) 94L else 100L)
    }
    if (column_key == "posthoc") {
      return(76L)
    }
  }
  if (column_key %in% c("m", "sd") && !isTRUE(mean_sd)) {
    return(if (isTRUE(trend_analysis)) 50L else 44L)
  }
  if (column_key %in% c("statistic", "t", "f", "tf", "fstatistic")) {
    if (isTRUE(trend_analysis)) {
      return(if (isTRUE(mean_sd)) 158L else 170L)
    }
    return(if (isTRUE(mean_sd)) 112L else 144L)
  }
  NA_integer_
}

coefficient_show_df_width_style <- function(table, column) {
  width <- coefficient_show_df_column_width(table, column)
  if (!is.finite(width)) {
    return("")
  }
  paste0("width:", width, "px !important;min-width:", width, "px !important;max-width:", width, "px !important;")
}

coefficient_display_cell_style <- function(table, row_index, column, display_index, display_meta, compact, compact_font_size, compact_width, compact_first_width) {
  marker_column <- isTRUE(display_meta$marker[[display_index]])
  source_columns <- display_meta$source[!display_meta$marker]
  source_index <- match(column, source_columns)
  column_key <- result_column_key(column)
  style <- result_body_cell_style(
    display_index == 1,
    row_index == nrow(table),
    compact = compact,
    compact_font_size = compact_font_size,
    compact_width = compact_width,
    compact_first_width = compact_first_width
  )
  if (isTRUE(marker_column)) {
    return(paste0(
      style,
      "padding-left:2px;padding-right:8px;padding-top:0;min-width:16px;width:16px;text-align:left;vertical-align:top;line-height:1;"
    ))
  }
  next_is_marker <- display_index < nrow(display_meta) &&
    isTRUE(display_meta$marker[[display_index + 1L]]) &&
    identical(display_meta$source[[display_index + 1L]], column)
  if (isTRUE(next_is_marker)) {
    style <- paste0(style, "padding-right:2px;")
  }
  if (is.finite(source_index) && source_index == 1 && display_index != 1) {
    style <- paste0(style, "text-align:left;")
  }
  if (result_note_marker_column(column) || column_key %in% c("msd", "mse", "rankmse")) {
    style <- paste0(style, "white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  if (nzchar(result_cell_note_marker(table, row_index, column))) {
    style <- paste0(style, "white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  style <- paste0(style, coefficient_show_df_width_style(table, column))
  if (isTRUE(attr(table, "show_df", exact = TRUE)) && column_key %in% c("statistic", "t", "f", "tf", "fstatistic")) {
    style <- paste0(style, "padding-right:12px !important;")
  }
  if (isTRUE(attr(table, "show_df", exact = TRUE)) && column_key == "p") {
    style <- paste0(style, "padding-left:12px !important;")
  }
  if (isTRUE(attr(table, "trend_analysis", exact = TRUE)) && column_key == "pfortrend") {
    style <- paste0(style, "white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  if (column_key %in% c("statistic", "t", "f", "tf", "fstatistic")) {
    style <- paste0(style, "text-align:right;white-space:nowrap;overflow-wrap:normal;word-break:normal;")
  }
  style
}

coefficient_column_class <- function(name) {
  normalized <- gsub("[^[:alnum:]]+", "", tolower(as.character(name %||% "")))
  switch(
    normalized,
    term = "coefficient-col-term",
    variable = "coefficient-col-term",
    value = "coefficient-col-value",
    label = "coefficient-col-reference",
    statistic = "coefficient-col-statistic",
    tf = "coefficient-col-statistic",
    f = "coefficient-col-f",
    msd = "coefficient-col-mse",
    mse = "coefficient-col-mse",
    rankmse = "coefficient-col-mse",
    b = "coefficient-col-b",
    bootse = "coefficient-col-boot-se",
    hc3se = "coefficient-col-boot-se",
    llci = "coefficient-col-ci",
    ulci = "coefficient-col-ci",
    bootp = "coefficient-col-boot-p",
    reference = "coefficient-col-reference",
    t = "coefficient-col-compact",
    p = "coefficient-col-p",
    pfortrend = "coefficient-col-p-trend",
    effectsize = "coefficient-col-effect-size",
    posthoc = "coefficient-col-posthoc",
    sr2 = "coefficient-col-compact",
    f2 = "coefficient-col-compact",
    vif = "coefficient-col-vif",
    tolerance = "coefficient-col-tolerance",
    "coefficient-col-stat"
  )
}

result_column_key <- function(name) {
  name <- gsub("\u00B2", "2", as.character(name %||% ""), fixed = TRUE)
  gsub("[^[:alnum:]]+", "", tolower(name))
}

hierarchical_compact_stat_column <- function(name) {
  result_column_key(name) %in% c("llci", "ulci", "p", "bootp", "sr2", "f2", "vif")
}

hierarchical_stat_column_class <- function(name) {
  if (isTRUE(hierarchical_compact_stat_column(name))) {
    return("hierarchical-stat-col hierarchical-stat-col-narrow")
  }
  "hierarchical-stat-col"
}

hierarchical_stat_column_width <- function(name) {
  key <- result_column_key(name)
  if (key %in% c("bootp")) {
    return(54L)
  }
  if (key %in% c("llci", "ulci", "p", "sr2", "f2", "vif")) {
    return(48L)
  }
  70L
}

hierarchical_stat_cell_style <- function(column, last = FALSE, header = FALSE) {
  width <- hierarchical_stat_column_width(column)
  padding <- if (isTRUE(hierarchical_compact_stat_column(column))) "9px 4px" else "9px 7px"
  paste0(
    "padding:", padding, ";line-height:1.45;border-left:0;border-right:0;",
    "border-top:0;border-bottom:",
    if (isTRUE(header)) "2px solid #1f2937" else if (isTRUE(last)) "0" else "1px solid #d7dde5",
    ";vertical-align:middle;background:transparent;",
    "width:", width, "px;min-width:", width, "px;max-width:", width, "px;",
    "text-align:right;white-space:nowrap;overflow-wrap:normal;"
  )
}

coefficient_html_table <- function(
  table,
  fit_line = NULL,
  stat_lines = character(0),
  warning_line = NULL,
  note_line = NULL,
  compact = FALSE,
  compact_font_size = 12,
  compact_width = 62,
  compact_first_width = 118,
  compact_min_width = 330
) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  columns <- names(table)
  display_meta <- coefficient_display_columns(table)
  table_class <- paste(
    "coefficient-table",
    if (isTRUE(attr(table, "show_df", exact = TRUE))) "coefficient-table-show-df" else "",
    if (isTRUE(attr(table, "mean_sd", exact = TRUE))) "coefficient-table-mean-sd" else "",
    if (isTRUE(attr(table, "trend_analysis", exact = TRUE))) "coefficient-table-trend-analysis" else "",
    if (isTRUE(attr(table, "bootstrap_regression", exact = TRUE))) "coefficient-table-bootstrap-regression" else ""
  )
  table_tag <- tags$table(
      class = table_class,
      style = result_table_style(font_size = if (isTRUE(compact)) compact_font_size else 12, min_width = if (isTRUE(compact)) compact_min_width else 480),
      tags$colgroup(lapply(seq_len(nrow(display_meta)), function(index) {
        tags$col(
          class = if (isTRUE(display_meta$marker[[index]])) "coefficient-col-note-marker" else coefficient_column_class(display_meta$source[[index]]),
          style = if (isTRUE(display_meta$marker[[index]])) "" else coefficient_show_df_width_style(table, display_meta$source[[index]])
        )
      })),
      tags$thead(
        tags$tr(lapply(seq_len(nrow(display_meta)), function(index) {
          tags$th(
            class = if (isTRUE(display_meta$marker[[index]])) {
              "coefficient-note-marker-cell"
            } else {
              coefficient_column_class(display_meta$source[[index]])
            },
            style = paste0(
              result_header_cell_style(
                index == 1,
                compact = compact,
                compact_font_size = compact_font_size,
                compact_width = compact_width,
                compact_first_width = compact_first_width
              ),
              if (isTRUE(display_meta$marker[[index]])) "" else coefficient_show_df_width_style(table, display_meta$source[[index]]),
              if (isTRUE(display_meta$marker[[index]])) "padding-left:2px;padding-right:8px;min-width:16px;width:16px;text-align:left;" else "",
              if (!isTRUE(display_meta$marker[[index]]) && index < nrow(display_meta) && isTRUE(display_meta$marker[[index + 1L]])) "padding-right:2px;" else ""
            ),
            result_header_content(display_meta$label[[index]])
          )
        }))
      ),
      tags$tbody(
        lapply(seq_len(nrow(table)), function(row_index) {
          tags$tr(lapply(seq_len(nrow(display_meta)), function(column_index) {
            column <- display_meta$source[[column_index]]
            marker_column <- isTRUE(display_meta$marker[[column_index]])
            if (isTRUE(result_cell_covered_by_span(table, row_index, column, columns))) {
              return(NULL)
            }
            span <- result_cell_span_start(table, row_index, column)
            marker <- result_cell_note_marker(table, row_index, column)
            value <- if (!is.null(span) && "value" %in% names(span)) span$value[[1]] else table[[column]][[row_index]] %||% ""
            content <- if (isTRUE(marker_column)) {
              if (nzchar(marker)) tags$sup(class = "coefficient-note-cell-marker", marker) else ""
            } else {
              result_cell_content(value, marker, column)
            }
            bold_style <- if (isTRUE(result_cell_bold(table, row_index, column)) && nzchar(as.character(table[[column]][[row_index]] %||% ""))) "font-weight:700;" else ""
            colspan <- if (!is.null(span)) {
              start_index <- match(span$start_column[[1]], columns)
              end_index <- match(span$end_column[[1]], columns)
              max(1L, end_index - start_index + 1L)
            } else {
              NULL
            }
            span_style <- if (!is.null(span) && "style" %in% names(span)) as.character(span$style[[1]] %||% "") else ""
            tags$td(
              class = if (isTRUE(marker_column)) "coefficient-note-marker-cell" else coefficient_column_class(column),
              colspan = colspan,
              style = paste0(
                coefficient_display_cell_style(
                  table,
                  row_index,
                  column,
                  column_index,
                  display_meta,
                  compact = compact,
                  compact_font_size = compact_font_size,
                  compact_width = compact_width,
                  compact_first_width = compact_first_width
                ),
                bold_style,
                result_cell_style_extra(table, row_index, column),
                span_style
              ),
              content
            )
          }))
        })
      ),
      if (!is.null(fit_line) && nzchar(fit_line)) {
        tags$tfoot(
          tags$tr(
            class = "coefficient-fit-row",
            tags$td(
              colspan = nrow(display_meta),
              style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:2px solid #1f2937;border-bottom:0;text-align:center;font-weight:500;",
              fit_line
            )
          ),
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(
                colspan = nrow(display_meta),
                style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:1px solid #d7dde5;border-bottom:0;text-align:center;font-weight:500;",
                line
              )
            )
          })
        )
      } else if (length(stat_lines) > 0) {
        tags$tfoot(
          lapply(as.character(stat_lines), function(line) {
            if (!nzchar(line)) return(NULL)
            tags$tr(
              class = "coefficient-fit-row coefficient-dw-row",
              tags$td(
                colspan = nrow(display_meta),
                style = "padding:9px 18px;line-height:1.45;border-left:0;border-right:0;border-top:1px solid #d7dde5;border-bottom:0;text-align:center;font-weight:500;",
                line
              )
            )
          })
        )
      }
  )
  result_table_with_notes(
    table_tag,
    result_note_tag(note_line),
    result_note_tag(warning_line, class = "coefficient-warning")
  )
}


model_overview_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  left_columns <- if ("Item" %in% names(table)) {
    if (identical(names(table)[[1]], "Item")) 1L else 2L
  } else {
    1L
  }
  compact_overview <- "Item" %in% names(table)
  if (isTRUE(compact_overview)) {
    n_cols <- ncol(table)
    value_cols <- max(1L, n_cols - left_columns)
    left_width <- if (left_columns == 1L) 96L else 160L
    target_width <- if (value_cols >= 5L) 890L else 590L
    first_width_pct <- 96 / target_width * 100
    item_width_pct <- if (left_columns >= 2L) 64 / target_width * 100 else 0
    value_width_pct <- max(6, (100 - first_width_pct - item_width_pct) / value_cols)
    value_names <- names(table)[seq.int(left_columns + 1L, n_cols)]
    model_header_match <- regexec("^(.*)\\s+(Model\\s+[0-9]+)$", value_names)
    model_header_parts <- regmatches(value_names, model_header_match)
    use_model_header <- length(value_names) > 1L && all(vapply(model_header_parts, length, integer(1)) == 3L)
    header_tag <- if (isTRUE(use_model_header)) {
      group_names <- vapply(model_header_parts, `[[`, character(1), 2L)
      model_names <- vapply(model_header_parts, `[[`, character(1), 3L)
      grouped_headers <- list()
      for (left_index in seq_len(left_columns)) {
        grouped_headers <- c(grouped_headers, list(tags$th(
          rowspan = 2,
          style = paste(
            "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
            "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
            "text-align:left;"
          ),
          names(table)[[left_index]]
        )))
      }
      for (group in unique(group_names)) {
        grouped_headers <- c(grouped_headers, list(tags$th(
          colspan = sum(group_names == group),
          style = paste(
            "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
            "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
            "text-align:center;"
          ),
          group
        )))
      }
      tags$thead(
        do.call(tags$tr, grouped_headers),
        tags$tr(lapply(model_names, function(name) {
          tags$th(
            style = paste(
              "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
              "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
              "text-align:center;"
            ),
            name
          )
        }))
      )
    } else {
      tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
        tags$th(
          style = paste(
            "padding:6px 8px;line-height:1.3;border-left:0;border-right:0;border-bottom:2px solid #1f2937;",
            "vertical-align:middle;font-weight:700;background:transparent;white-space:normal;overflow-wrap:anywhere;",
            "text-align:", if (index <= left_columns) "left" else "center", ";"
          ),
          names(table)[[index]]
        )
      })))
    }
    table_tag <- tags$table(
      class = "table shiny-table combined-model-overview-table compact-model-overview-table",
      style = paste(
        "width:100%;max-width:100%;min-width:0;table-layout:fixed;",
        "border-collapse:collapse;border-spacing:0;border-top:2px solid #1f2937;border-bottom:2px solid #1f2937;",
        "color:#2f3a46;font-size:12px;background:transparent;"
      ),
      tags$colgroup(
        tags$col(style = sprintf("width:%.4f%%;", first_width_pct)),
        if (left_columns >= 2L) tags$col(style = sprintf("width:%.4f%%;", item_width_pct)),
        lapply(seq_len(value_cols), function(unused) {
          tags$col(style = sprintf("width:%.4f%%;", value_width_pct))
        })
      ),
      header_tag,
      tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
        values <- table[row_index, , drop = TRUE]
        tags$tr(lapply(seq_along(values), function(index) {
          tags$td(
            style = paste(
              "padding:6px 8px;line-height:1.35;border-left:0;border-right:0;",
              "border-bottom:", if (row_index == nrow(table)) "0" else "1px solid #d7dde5", ";",
              "vertical-align:top;background:transparent;white-space:pre-line;overflow-wrap:anywhere;word-break:normal;",
              "font-variant-numeric:tabular-nums lining-nums;font-feature-settings:'tnum' 1,'lnum' 1;",
              "text-align:", if (index <= left_columns) "left" else "center", ";"
            ),
            values[[index]]
          )
        }))
      }))
    )
    return(result_table_with_notes(table_tag))
  }
  overview_header_style <- function(index) {
    style <- result_header_cell_style(index <= left_columns, compact = compact_overview, compact_font_size = 12, compact_width = 88, compact_first_width = 118)
    if (isTRUE(compact_overview)) {
      style <- paste0(style, "min-width:0;white-space:normal;overflow-wrap:anywhere;")
    }
    style
  }
  overview_body_style <- function(index, last) {
    style <- result_body_cell_style(index <= left_columns, last, compact = compact_overview, compact_font_size = 12, compact_width = 88, compact_first_width = 118)
    if (isTRUE(compact_overview)) {
      style <- paste0(style, "min-width:0;white-space:pre-line;overflow-wrap:anywhere;word-break:normal;")
    }
    style
  }
  table_tag <- tags$table(
    class = "table shiny-table combined-model-overview-table",
    style = if (isTRUE(compact_overview)) {
      paste0(result_table_style(font_size = 12, min_width = 360), "width:100%;min-width:0;max-width:100%;table-layout:fixed;")
    } else {
      result_table_style()
    },
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = overview_header_style(index), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      values <- table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = overview_body_style(index, row_index == nrow(table)), values[[index]])
      }))
    }))
  )
  result_table_with_notes(table_tag)
}

combined_dw_html_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  table_tag <- tags$table(
    class = "table shiny-table combined-dw-table",
    style = result_table_style(),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = result_header_cell_style(index == 1), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      values <- table[row_index, , drop = TRUE]
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = result_body_cell_style(index == 1, row_index == nrow(table)), values[[index]])
      }))
    }))
  )
  result_table_with_notes(table_tag)
}

