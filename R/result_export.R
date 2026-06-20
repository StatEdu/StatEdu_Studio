# Result export helpers for StatEdu Studio.
# All analysis table exports should use these helpers so workbook layout,
# table lines, title rows, column widths, and alignment stay consistent.

excel_sheet_name <- function(name, used = character(0)) {
  name <- gsub("[\\\\/:*?\\[\\]]", " ", as.character(name %||% "Sheet"), perl = TRUE)
  name <- trimws(gsub("\\s+", " ", name))
  if (!nzchar(name)) {
    name <- "Sheet"
  }
  base <- substr(name, 1, 31)
  candidate <- base
  counter <- 1L
  while (tolower(candidate) %in% tolower(used)) {
    suffix <- paste0("_", counter)
    candidate <- paste0(substr(base, 1, 31 - nchar(suffix)), suffix)
    counter <- counter + 1L
  }
  candidate
}

analysis_excel_styles <- function() {
  list(
    title = openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left"),
    header = openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom", borderStyle = "thin"),
    group_header = openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center"),
    right_header = openxlsx::createStyle(textDecoration = "bold", halign = "right", valign = "center", border = "bottom", borderStyle = "thin"),
    body = openxlsx::createStyle(halign = "center", valign = "center"),
    left = openxlsx::createStyle(halign = "left", valign = "center", wrapText = TRUE),
    right = openxlsx::createStyle(halign = "right", valign = "center"),
    wrap = openxlsx::createStyle(halign = "center", valign = "center", wrapText = TRUE),
    summary = openxlsx::createStyle(halign = "center", valign = "center"),
    warning = openxlsx::createStyle(halign = "center", valign = "center", wrapText = TRUE, fontColour = "#9A3412"),
    note = openxlsx::createStyle(halign = "left", valign = "top", wrapText = TRUE),
    top = openxlsx::createStyle(border = "top", borderStyle = "thin"),
    bottom = openxlsx::createStyle(border = "bottom", borderStyle = "thin")
  )
}

regression_excel_styles <- analysis_excel_styles

excel_p_value_column <- function(column) {
  key <- tolower(gsub("[^a-z0-9]+", "", as.character(column %||% "")))
  key %in% c("p", "pvalue", "padjusted", "pfortrend", "ggp") ||
    grepl("(normality|homogeneity|sphericity|shapirowilk|bootstrap|boot).*p$", key)
}

excel_split_p_marker <- function(value) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text)) {
    return(list(value = text, marker = ""))
  }
  spaced_marker <- regexpr("\\s+[0-9]+$", text, perl = TRUE)
  if (spaced_marker[[1]] > 0) {
    marker <- regmatches(text, spaced_marker)
    text <- sub("\\s+[0-9]+$", "", text, perl = TRUE)
    return(list(value = trimws(text), marker = marker))
  }
  compact_marker <- regexec("^((?:<\\.001)|(?:-?\\.[0-9]{3,}))([1-9][0-9]?)$", text, perl = TRUE)
  compact_parts <- regmatches(text, compact_marker)[[1]]
  if (length(compact_parts) == 3L) {
    return(list(value = compact_parts[[2]], marker = compact_parts[[3]]))
  }
  list(value = text, marker = "")
}

excel_format_p_value <- function(value) {
  if (length(value) == 0 || is.na(value)) {
    return("")
  }
  if (is.numeric(value)) {
    formatted <- format_p(value)
    return(if (is.na(formatted)) "" else formatted)
  }
  parts <- excel_split_p_marker(value)
  formatted <- format_p(parts$value)
  if (is.na(formatted)) {
    return(trimws(as.character(value %||% "")))
  }
  paste0(formatted, parts$marker)
}

excel_normalize_p_columns <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0 || ncol(table) == 0) {
    return(table)
  }
  p_columns <- names(table)[vapply(names(table), excel_p_value_column, logical(1))]
  for (column in p_columns) {
    table[[column]] <- vapply(table[[column]], excel_format_p_value, character(1))
  }
  table
}

excel_display_column_names <- function(table) {
  if (!is.data.frame(table) || ncol(table) == 0) {
    return(table)
  }
  names(table)[tolower(gsub("[^a-z0-9]+", "", names(table))) == "effectsize"] <- "ES"
  table
}

excel_apply_title_row <- function(workbook, sheet_name, title, n_cols, styles, row = 1L) {
  openxlsx::writeData(workbook, sheet_name, title, startRow = row, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(workbook, sheet_name, cols = seq_len(n_cols), rows = row)
  openxlsx::addStyle(workbook, sheet_name, styles$title, rows = row, cols = 1, gridExpand = TRUE, stack = TRUE)
}

excel_apply_note_row <- function(workbook, sheet_name, note, row, n_cols, widths, styles, min_height = 42) {
  if (length(note) == 0 || !nzchar(note[[1]] %||% "")) {
    return(invisible(FALSE))
  }
  openxlsx::writeData(workbook, sheet_name, note[[1]], startRow = row, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(workbook, sheet_name, cols = seq_len(n_cols), rows = row)
  openxlsx::addStyle(workbook, sheet_name, styles$note, rows = row, cols = 1, gridExpand = TRUE, stack = TRUE)
  openxlsx::setRowHeights(workbook, sheet_name, rows = row, heights = excel_note_row_height(note[[1]], widths, min_height = min_height))
  invisible(TRUE)
}

excel_apply_two_tier_header_style <- function(workbook, sheet_name, header_row, subheader_row, n_cols, styles) {
  openxlsx::addStyle(workbook, sheet_name, styles$top, rows = header_row, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(workbook, sheet_name, styles$group_header, rows = header_row, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(workbook, sheet_name, styles$header, rows = subheader_row, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
}

excel_note_row_height <- function(text, widths, min_height = 48, line_height = 15, padding = 10) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  widths <- as.numeric(widths %||% numeric(0))
  total_width <- sum(widths[is.finite(widths) & widths > 0], na.rm = TRUE)
  if (!is.finite(total_width) || total_width <= 0) {
    total_width <- 60
  }
  chars_per_line <- max(24L, floor(total_width * 1.15))
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) {
    lines <- ""
  }
  wrapped_lines <- sum(pmax(1L, ceiling(nchar(lines, type = "width") / chars_per_line)))
  max(min_height, wrapped_lines * line_height + padding)
}

excel_table_column_widths <- function(table) {
  widths <- rep(12, ncol(table))
  names(widths) <- names(table)
  if (length(widths) == 0) {
    return(widths)
  }
  widths[1] <- 14
  if (ncol(table) >= 2) {
    widths[2] <- 18
  }
  widths[names(widths) %in% c("Item")] <- 22
  widths[names(widths) %in% c("Measurement level")] <- 18
  widths[names(widths) %in% c("Method")] <- 34
  widths[names(widths) %in% c("Ordinal")] <- 10
  widths[names(widths) %in% c("Cronbach's alpha", "Pearson omega", "Ordinal alpha", "Ordinal omega", "Reliability")] <- 16
  widths[names(widths) %in% c("Cronbach's alpha if item deleted", "Pearson omega if item deleted", "Ordinal alpha if item deleted", "Ordinal omega if item deleted", "Reliability if item deleted")] <- 24
  widths[names(widths) %in% c("Corrected item-total correlation", "Item-total correlation")] <- 24
  widths[names(widths) %in% c("Skewness", "Kurtosis")] <- 14
  widths[names(widths) %in% c("Min", "Max", "Median", "M", "SD", "n", "%", "N", "Missing")] <- 12
  widths[names(widths) %in% c("IQR(Q1~Q3)")] <- 18
  widths[names(widths) %in% c("n(%) or M \u00b1 SD")] <- 16
  widths
}

add_excel_table_sheet <- function(workbook, sheet_name, table, used_sheets, merge_shared_independent = FALSE, title = NULL) {
  sheet_name <- excel_sheet_name(sheet_name, used_sheets)
  styles <- analysis_excel_styles()
  title <- title %||% sheet_name
  openxlsx::addWorksheet(workbook, sheet_name)
  if (is.data.frame(table) && nrow(table) > 0) {
    table <- excel_normalize_p_columns(table)
    table <- excel_display_column_names(table)
    n_cols <- ncol(table)
    title_row <- 1L
    header_row <- 3L
    body_rows <- (header_row + 1):(header_row + nrow(table))

    openxlsx::writeData(workbook, sheet_name, title, startRow = title_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, sheet_name, cols = seq_len(n_cols), rows = title_row)
    openxlsx::addStyle(workbook, sheet_name, styles$title, rows = title_row, cols = 1, gridExpand = TRUE, stack = TRUE)

    openxlsx::writeData(workbook, sheet_name, table, startRow = header_row, startCol = 1, withFilter = FALSE)
    openxlsx::addStyle(workbook, sheet_name, styles$top, rows = header_row, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$header, rows = header_row, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$body, rows = body_rows, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE)
    if (n_cols >= 2) {
      openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 2, gridExpand = TRUE, stack = TRUE)
    }
    if (identical(sheet_name, "Model overview") && ncol(table) > 1) {
      openxlsx::addStyle(workbook, sheet_name, styles$wrap, rows = body_rows, cols = 2:ncol(table), gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::addStyle(workbook, sheet_name, styles$bottom, rows = header_row + nrow(table), cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    if (isTRUE(merge_shared_independent) && ncol(table) > 2) {
      independent_row <- which(as.character(table[[1]]) == "Independent variables")
      independent_values <- as.character(unlist(table[independent_row, -1, drop = TRUE], use.names = FALSE))
      if (
        length(independent_row) == 1 &&
        length(independent_values) == ncol(table) - 1 &&
        all(!is.na(independent_values)) &&
        all(nzchar(trimws(independent_values))) &&
        length(unique(independent_values)) == 1
      ) {
        excel_row <- independent_row + header_row
        openxlsx::mergeCells(workbook, sheet_name, cols = 2:ncol(table), rows = excel_row)
        openxlsx::addStyle(workbook, sheet_name, styles$body, rows = excel_row, cols = 2, gridExpand = TRUE, stack = TRUE)
      }
    }
    if (identical(sheet_name, "Model overview")) {
      overview_widths <- c(28, rep(24, max(0, ncol(table) - 1)))
      openxlsx::setColWidths(workbook, sheet_name, cols = seq_along(table), widths = overview_widths[seq_along(table)])
    } else {
      widths <- excel_table_column_widths(table)
      openxlsx::setColWidths(workbook, sheet_name, cols = seq_along(table), widths = widths)
    }
  } else {
    openxlsx::writeData(workbook, sheet_name, "No data")
  }
  c(used_sheets, sheet_name)
}

add_optional_excel_table_sheet <- function(workbook, sheet_name, table, used_sheets, title = NULL, ...) {
  if (!analysis_has_rows(table)) {
    return(used_sheets)
  }
  add_excel_table_sheet(workbook, sheet_name, table, used_sheets, title = title %||% sheet_name, ...)
}

add_analysis_warning_skipped_sheets <- function(workbook, used_sheets, warnings = NULL, skipped = NULL, skipped_title = "Skipped analyses") {
  used_sheets <- add_optional_excel_table_sheet(workbook, "Warnings", warnings, used_sheets, title = "Warnings")
  used_sheets <- add_optional_excel_table_sheet(workbook, skipped_title, skipped, used_sheets, title = skipped_title)
  used_sheets
}

add_regression_result_sheet <- function(
  workbook,
  sheet_name,
  result,
  table,
  used_sheets,
  show_vif = FALSE,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  title = NULL
) {
  sheet_name <- excel_sheet_name(sheet_name, used_sheets)
  styles <- analysis_excel_styles()
  n_cols <- max(1, ncol(table))
  data_start_row <- 3
  header_row <- data_start_row
  title <- title %||% coefficient_fit_line(result)

  openxlsx::addWorksheet(workbook, sheet_name)
  openxlsx::writeData(workbook, sheet_name, title, startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(workbook, sheet_name, cols = 1:n_cols, rows = 1)
  openxlsx::addStyle(workbook, sheet_name, styles$title, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  if (is.data.frame(table) && nrow(table) > 0 && ncol(table) > 0) {
    table <- excel_normalize_p_columns(table)
    table <- excel_display_column_names(table)
    openxlsx::writeData(workbook, sheet_name, table, startRow = data_start_row, startCol = 1, withFilter = FALSE)
    openxlsx::addStyle(workbook, sheet_name, styles$top, rows = header_row, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$header, rows = header_row, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)

    body_rows <- (data_start_row + 1):(data_start_row + nrow(table))
    openxlsx::addStyle(workbook, sheet_name, styles$body, rows = body_rows, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE)

    summary_lines <- c(coefficient_fit_line(result), coefficient_stat_lines(result))
    summary_start <- data_start_row + nrow(table) + 1
    for (index in seq_along(summary_lines)) {
      row <- summary_start + index - 1
      openxlsx::writeData(workbook, sheet_name, summary_lines[[index]], startRow = row, startCol = 1, colNames = FALSE)
      openxlsx::mergeCells(workbook, sheet_name, cols = 1:ncol(table), rows = row)
      openxlsx::addStyle(workbook, sheet_name, styles$summary, rows = row, cols = 1, gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::addStyle(workbook, sheet_name, styles$top, rows = summary_start, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)

    warning_line <- coefficient_vif_warning_line(result)
    warning_row_count <- if (length(warning_line) > 0 && nzchar(warning_line[[1]])) 1L else 0L
    if (warning_row_count > 0) {
      warning_row <- summary_start + length(summary_lines)
      openxlsx::writeData(workbook, sheet_name, warning_line[[1]], startRow = warning_row, startCol = 1, colNames = FALSE)
      openxlsx::mergeCells(workbook, sheet_name, cols = 1:ncol(table), rows = warning_row)
      openxlsx::addStyle(workbook, sheet_name, styles$warning, rows = warning_row, cols = 1, gridExpand = TRUE, stack = TRUE)
      openxlsx::setRowHeights(workbook, sheet_name, rows = warning_row, heights = 28)
    }
    summary_end <- summary_start + length(summary_lines) + warning_row_count - 1
    openxlsx::addStyle(workbook, sheet_name, styles$bottom, rows = summary_end, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)

    widths <- c(20, rep(10, max(0, ncol(table) - 1)))
    note_row <- summary_end + 2
    note <- coefficient_note_line(result, show_vif, show_sr2, show_f2)
    openxlsx::writeData(workbook, sheet_name, note, startRow = note_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, sheet_name, cols = 1:ncol(table), rows = note_row)
    openxlsx::addStyle(workbook, sheet_name, styles$note, rows = note_row, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(workbook, sheet_name, rows = note_row, heights = excel_note_row_height(note, widths, min_height = 48))

    openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(ncol(table)), widths = widths[seq_len(ncol(table))])
    openxlsx::freezePane(workbook, sheet_name, firstActiveRow = data_start_row + 1, firstActiveCol = 2)
  } else {
    openxlsx::writeData(workbook, sheet_name, "No data", startRow = data_start_row, startCol = 1, colNames = FALSE)
  }

  c(used_sheets, sheet_name)
}

save_analysis_excel_workbook <- function(
  results,
  file,
  model_overview_table,
  coefficient_tables,
  sheet_names,
  titles,
  assumption_review_table = NULL,
  show_vif = FALSE,
  show_sr2 = FALSE,
  show_f2 = FALSE
) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)

  used_sheets <- add_excel_table_sheet(
    workbook,
    "Model overview",
    model_overview_table,
    used_sheets,
    merge_shared_independent = TRUE
  )

  for (index in seq_along(results)) {
    used_sheets <- add_regression_result_sheet(
      workbook,
      sheet_names[[index]],
      results[[index]],
      coefficient_tables[[index]],
      used_sheets,
      show_vif = show_vif,
      show_sr2 = show_sr2,
      show_f2 = show_f2,
      title = titles[[index]]
    )
  }
  if (is.data.frame(assumption_review_table) && nrow(assumption_review_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Assumption review", assumption_review_table, used_sheets, title = "Assumption review")
  }
  warnings <- attr(results, "warnings")
  skipped <- attr(results, "skipped")
  used_sheets <- add_analysis_warning_skipped_sheets(workbook, used_sheets, warnings, skipped, skipped_title = "Skipped models")

  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
}

saved_coefficients_table <- function(results, variable_table = NULL, labels = character(0), category_table = NULL) {
  refs <- regression_reference_values_static(category_table)
  value_labels <- category_value_label_lookup_static(category_table)
  tables <- lapply(results, function(result) {
    dependent <- all.vars(result$formula)[[1]]
    dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
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
    if (!is.data.frame(table)) {
      table <- data.frame()
    }
    data.frame(Dependent = dependent_label, table, check.names = FALSE)
  })
  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  tables <- lapply(tables, function(table) {
    missing_columns <- setdiff(columns, names(table))
    for (column in missing_columns) {
      table[[column]] <- ""
    }
    table[, columns, drop = FALSE]
  })
  do.call(rbind, tables)
}

coefficients_csv_filename <- function() {
  "EFS_coefficients.csv"
}

analysis_results_html_filename <- function() {
  sprintf("EFS_results_%s.html", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

write_coefficients_csv <- function(results, file, variable_table = NULL, labels = character(0), category_table = NULL) {
  write.csv(
    saved_coefficients_table(results, variable_table, labels, category_table),
    file,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

write_analysis_results_html <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  writeLines(
    saved_analysis_results_html(
      results,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table,
      refs = regression_reference_values_static(category_table),
      value_labels = category_value_label_lookup_static(category_table),
      show_sr2 = show_sr2,
      show_f2 = show_f2,
      show_vif = show_vif
    ),
    file,
    useBytes = TRUE
  )
}

write_analysis_results_pdf <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  html <- saved_analysis_results_html(
    results,
    variable_table = variable_table,
    labels = labels,
    category_table = category_table,
    refs = regression_reference_values_static(category_table),
    value_labels = category_value_label_lookup_static(category_table),
    show_sr2 = show_sr2,
    show_f2 = show_f2,
    show_vif = show_vif,
    report_mode = TRUE
  )
  write_pdf_from_html(html, file)
}

write_hierarchical_results_html <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  writeLines(
    saved_hierarchical_results_html(
      results,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table,
      refs = regression_reference_values_static(category_table),
      value_labels = category_value_label_lookup_static(category_table),
      show_sr2 = show_sr2,
      show_f2 = show_f2,
      show_vif = show_vif
    ),
    file,
    useBytes = TRUE
  )
}

write_hierarchical_results_pdf <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  html <- saved_hierarchical_results_html(
    results,
    variable_table = variable_table,
    labels = labels,
    category_table = category_table,
    refs = regression_reference_values_static(category_table),
    value_labels = category_value_label_lookup_static(category_table),
    show_sr2 = show_sr2,
    show_f2 = show_f2,
    show_vif = show_vif,
    report_mode = TRUE
  )
  write_pdf_from_html(html, file)
}

write_frequencies_results_html <- function(result, file) {
  writeLines(
    saved_frequencies_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_reliability_results_html <- function(result, file) {
  writeLines(
    saved_reliability_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_ttest_anova_results_html <- function(result, file) {
  writeLines(
    saved_ttest_anova_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_ancova_results_html <- function(result, file, variable_table = NULL, labels = character(0)) {
  writeLines(
    saved_ancova_results_html(result, variable_table, labels),
    file,
    useBytes = TRUE
  )
}

write_nonparametric_results_html <- function(result, file) {
  writeLines(
    saved_nonparametric_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_nonparametric_paired_results_html <- function(result, file) {
  writeLines(
    saved_nonparametric_paired_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_paired_results_html <- function(result, file) {
  writeLines(
    saved_paired_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_paired_rm_results_html <- function(result, file) {
  writeLines(
    saved_paired_rm_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_correlation_results_html <- function(result, file) {
  writeLines(
    saved_correlation_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_logistic_results_html <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_b = FALSE,
  show_se = FALSE,
  show_mcfadden = FALSE,
  show_cox_snell = FALSE,
  split_ci = TRUE
) {
  writeLines(
    saved_logistic_results_html(
      results,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table,
      show_b = show_b,
      show_se = show_se,
      show_mcfadden = show_mcfadden,
      show_cox_snell = show_cox_snell,
      split_ci = split_ci
    ),
    file,
    useBytes = TRUE
  )
}

write_frequencies_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_frequencies_results_html(result, report_mode = TRUE), file)
}

write_reliability_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_reliability_results_html(result, report_mode = TRUE), file)
}

write_ttest_anova_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_ttest_anova_results_html(result, report_mode = TRUE), file)
}

write_ancova_results_pdf <- function(result, file, variable_table = NULL, labels = character(0)) {
  write_pdf_from_html(saved_ancova_results_html(result, variable_table, labels, report_mode = TRUE), file)
}

write_nonparametric_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_nonparametric_results_html(result, report_mode = TRUE), file)
}

write_nonparametric_paired_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_nonparametric_paired_results_html(result, report_mode = TRUE), file)
}

write_paired_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_paired_results_html(result, report_mode = TRUE), file)
}

write_paired_rm_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_paired_rm_results_html(result, report_mode = TRUE), file)
}

write_correlation_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_correlation_results_html(result, report_mode = TRUE), file)
}

write_logistic_results_pdf <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_b = FALSE,
  show_se = FALSE,
  show_mcfadden = FALSE,
  show_cox_snell = FALSE,
  split_ci = TRUE
) {
  write_pdf_from_html(
    saved_logistic_results_html(
      results,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table,
      show_b = show_b,
      show_se = show_se,
      show_mcfadden = show_mcfadden,
      show_cox_snell = show_cox_snell,
      split_ci = split_ci,
      report_mode = TRUE
    ),
    file
  )
}

write_factor_analysis_results_html <- function(result, file) {
  writeLines(
    saved_factor_analysis_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_pca_results_html <- function(result, file) {
  writeLines(
    saved_pca_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_factor_analysis_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_factor_analysis_results_html(result, report_mode = TRUE), file)
}

write_pca_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_pca_results_html(result, report_mode = TRUE), file)
}

write_crosstab_results_html <- function(result, file) {
  writeLines(
    saved_crosstab_results_html(result),
    file,
    useBytes = TRUE
  )
}

write_crosstab_results_pdf <- function(result, file) {
  write_pdf_from_html(
    saved_crosstab_results_html(result, report_mode = TRUE),
    file
  )
}

coefficient_export_table <- function(
  result,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL
) {
  table <- coefficient_output_table_with_context(
    coefficient_display_table(result),
    result$predictors,
    include_references = TRUE,
    variable_info = variable_table,
    refs = regression_reference_values_static(category_table),
    value_labels = category_value_label_lookup_static(category_table),
    labels = labels,
    category_table = category_table
  )
  if (!is.data.frame(table)) {
    return(data.frame())
  }
  filter_coefficient_export_table(table, show_sr2, show_f2, show_vif)
}

regression_excel_export_payload <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  sheet_names <- vapply(results, function(result) {
    dependent <- all.vars(result$formula)[[1]]
    display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
  }, character(1))
  list(
    model_overview = model_overview_data_frame(results, variable_table, labels),
    coefficient_tables = lapply(
      results,
      coefficient_export_table,
      show_sr2 = show_sr2,
      show_f2 = show_f2,
      show_vif = show_vif,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table
    ),
    sheet_names = sheet_names,
    titles = vapply(results, coefficient_panel_title_static, character(1), variable_table = variable_table, labels = labels)
  )
}

save_analysis_excel_file <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  payload <- regression_excel_export_payload(
    results,
    variable_table,
    labels,
    category_table,
    show_sr2 = show_sr2,
    show_f2 = show_f2,
    show_vif = show_vif
  )
  save_analysis_excel_workbook(
    results,
    file,
    payload$model_overview,
    payload$coefficient_tables,
    payload$sheet_names,
    payload$titles,
    assumption_review_table = regression_assumption_review_data_frame(results, variable_table, labels),
    show_vif = show_vif,
    show_sr2 = show_sr2,
    show_f2 = show_f2
  )
}

hierarchical_export_table <- function(
  group,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  refs <- regression_reference_values_static(category_table)
  value_labels <- category_value_label_lookup_static(category_table)
  model_tables <- lapply(
    group,
    hierarchical_model_table,
    variable_table = variable_table,
    labels = labels,
    category_table = category_table,
    refs = refs,
    value_labels = value_labels,
    show_sr2 = show_sr2,
    show_f2 = show_f2,
    show_vif = show_vif
  )
  model_labels <- mapply(hierarchical_step_label, group, seq_along(group), USE.NAMES = FALSE)
  model_columns <- lapply(model_tables, function(table) setdiff(names(table), "Term"))
  terms <- unique(unlist(lapply(rev(model_tables), function(table) as.character(table$Term)), use.names = FALSE))
  out <- data.frame(Term = terms, stringsAsFactors = FALSE, check.names = FALSE)
  for (model_index in seq_along(model_tables)) {
    table <- model_tables[[model_index]]
    columns <- setdiff(names(table), "Term")
    row_match <- match(terms, as.character(table$Term))
    for (column in columns) {
      values <- rep("", length(terms))
      matched <- !is.na(row_match)
      values[matched] <- as.character(table[[column]][row_match[matched]] %||% "")
      out[[paste(model_labels[[model_index]], column)]] <- values
    }
  }

  summary_values <- hierarchical_summary_values(group)
  summary_labels <- c("F(p)", "R\u00B2(adj. R\u00B2)")
  summary_keys <- c("f", "r2")
  if (length(group) > 1) {
    summary_labels <- c(summary_labels, attr(summary_values, "delta_label", exact = TRUE) %||% "\u0394R\u00B2(F change p)")
    summary_keys <- c(summary_keys, "delta")
  }
  summary_labels <- c(summary_labels, "d(d\u1D64~4-d\u1D64)", "z(p)", stat_chisq_label(with_p = TRUE))
  summary_keys <- c(summary_keys, "dw", "normality", "homogeneity")

  for (summary_index in seq_along(summary_labels)) {
    row <- as.list(rep("", ncol(out)))
    names(row) <- names(out)
    row[["Term"]] <- summary_labels[[summary_index]]
    key <- summary_keys[[summary_index]]
    for (model_index in seq_along(model_tables)) {
      first_model_column <- setdiff(names(model_tables[[model_index]]), "Term")[[1]]
      target <- paste(model_labels[[model_index]], first_model_column)
      row[[target]] <- summary_values[[model_index]][[key]] %||% ""
    }
    out <- rbind(out, as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE))
  }
  attr(out, "model_labels") <- model_labels
  attr(out, "model_columns") <- model_columns
  attr(out, "summary_start") <- length(terms) + 1L
  out
}

add_hierarchical_result_sheet <- function(workbook, sheet_name, table, note, model_notes, used_sheets, title = NULL) {
  sheet_name <- excel_sheet_name(sheet_name, used_sheets)
  styles <- analysis_excel_styles()
  title <- title %||% sheet_name
  openxlsx::addWorksheet(workbook, sheet_name)
  if (is.data.frame(table) && nrow(table) > 0 && ncol(table) > 0) {
    table <- excel_normalize_p_columns(table)
    table <- excel_display_column_names(table)
    n_cols <- ncol(table)
    title_row <- 1L
    header_top_row <- 3L
    header_bottom_row <- 4L
    data_start_row <- 5L
    body_rows <- data_start_row:(data_start_row + nrow(table) - 1L)
    model_labels <- attr(table, "model_labels", exact = TRUE)
    model_columns <- attr(table, "model_columns", exact = TRUE)
    summary_start <- as.integer(attr(table, "summary_start", exact = TRUE) %||% NA_integer_)
    if (is.null(model_labels) || is.null(model_columns)) {
      model_labels <- unique(sub("^(Model [0-9]+) .*", "\\1", names(table)[-1]))
      model_columns <- lapply(model_labels, function(label) sub(paste0("^", label, " "), "", grep(paste0("^", label, " "), names(table), value = TRUE)))
    }
    model_widths <- lengths(model_columns)
    openxlsx::writeData(workbook, sheet_name, title, startRow = title_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, sheet_name, cols = seq_len(n_cols), rows = title_row)
    openxlsx::addStyle(workbook, sheet_name, styles$title, rows = title_row, cols = 1, gridExpand = TRUE, stack = TRUE)

    openxlsx::writeData(workbook, sheet_name, "Term", startRow = header_top_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, sheet_name, cols = 1, rows = header_top_row:header_bottom_row)
    current_col <- 2L
    for (model_index in seq_along(model_labels)) {
      span <- model_widths[[model_index]]
      if (span <= 0) next
      openxlsx::writeData(workbook, sheet_name, model_labels[[model_index]], startRow = header_top_row, startCol = current_col, colNames = FALSE)
      if (span > 1) {
        openxlsx::mergeCells(workbook, sheet_name, cols = current_col:(current_col + span - 1L), rows = header_top_row)
      }
      for (column_index in seq_len(span)) {
        openxlsx::writeData(
          workbook,
          sheet_name,
          as.character(model_columns[[model_index]][[column_index]]),
          startRow = header_bottom_row,
          startCol = current_col + column_index - 1L,
          colNames = FALSE
        )
      }
      current_col <- current_col + span
    }
    openxlsx::addStyle(workbook, sheet_name, styles$top, rows = header_top_row, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    if (n_cols > 1) {
      openxlsx::addStyle(workbook, sheet_name, styles$group_header, rows = header_top_row, cols = 2:n_cols, gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(workbook, sheet_name, styles$right_header, rows = header_bottom_row, cols = 2:n_cols, gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::addStyle(workbook, sheet_name, styles$header, rows = header_top_row:header_bottom_row, cols = 1, gridExpand = TRUE, stack = TRUE)

    openxlsx::writeData(workbook, sheet_name, table, startRow = data_start_row, startCol = 1, colNames = FALSE, withFilter = FALSE)
    openxlsx::addStyle(workbook, sheet_name, styles$body, rows = body_rows, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE)
    if (n_cols > 1) {
      openxlsx::addStyle(workbook, sheet_name, styles$right, rows = body_rows, cols = 2:n_cols, gridExpand = TRUE, stack = TRUE)
    }
    if (!is.na(summary_start) && summary_start <= nrow(table)) {
      summary_rows <- (data_start_row + summary_start - 1L):(data_start_row + nrow(table) - 1L)
      openxlsx::addStyle(workbook, sheet_name, styles$summary, rows = summary_rows, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
      for (row in summary_rows) {
        current_col <- 2L
        for (span in model_widths) {
          if (span <= 0) next
          if (span > 1) {
            openxlsx::mergeCells(workbook, sheet_name, cols = current_col:(current_col + span - 1L), rows = row)
          }
          current_col <- current_col + span
        }
      }
      openxlsx::addStyle(workbook, sheet_name, styles$top, rows = summary_rows[[1]], cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::addStyle(workbook, sheet_name, styles$bottom, rows = data_start_row + nrow(table) - 1L, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)

    widths <- rep(12, n_cols)
    widths[[1]] <- 20
    if (n_cols > 1) {
      column_names <- names(table)
      widths[2:n_cols] <- vapply(column_names[2:n_cols], function(name) {
        column <- sub("^Model [0-9]+ ", "", as.character(name))
        if (isTRUE(hierarchical_compact_stat_column(column))) 7 else 10
      }, numeric(1))
    }
    notes <- c(model_notes, note)
    notes <- notes[nzchar(notes %||% "")]
    if (length(notes) > 0) {
      note_row <- data_start_row + nrow(table) + 1L
      note_text <- paste(notes, collapse = "\n")
      openxlsx::writeData(workbook, sheet_name, note_text, startRow = note_row, startCol = 1, colNames = FALSE)
      openxlsx::mergeCells(workbook, sheet_name, cols = seq_len(n_cols), rows = note_row)
      openxlsx::addStyle(workbook, sheet_name, styles$note, rows = note_row, cols = 1, gridExpand = TRUE, stack = TRUE)
      openxlsx::setRowHeights(workbook, sheet_name, rows = note_row, heights = excel_note_row_height(note_text, widths, min_height = 48))
    }
    openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(n_cols), widths = widths)
    openxlsx::freezePane(workbook, sheet_name, firstActiveRow = data_start_row, firstActiveCol = 2)
  } else {
    openxlsx::writeData(workbook, sheet_name, "No data")
  }
  c(used_sheets, sheet_name)
}

save_hierarchical_excel_file <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  used_sheets <- add_excel_table_sheet(
    workbook,
    "Model overview",
    model_overview_data_frame(results, variable_table, labels),
    used_sheets,
    merge_shared_independent = TRUE
  )
  groups <- hierarchical_result_groups(results)
  for (group in groups) {
    final_index <- length(group)
    dependent <- hierarchical_result_dependent_name(group[[1]])
    dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
    used_sheets <- add_hierarchical_result_sheet(
      workbook,
      dependent_label,
      hierarchical_export_table(
        group,
        variable_table = variable_table,
        labels = labels,
        category_table = category_table,
        show_sr2 = show_sr2,
        show_f2 = show_f2,
        show_vif = show_vif
      ),
      hierarchical_coefficient_note_line(group[[final_index]], show_vif, show_sr2, show_f2),
      hierarchical_model_note_lines(group, variable_table, labels),
      used_sheets,
      title = sprintf("Hierarchical Regression(%s)", dependent_label)
    )
  }
  assumption_review <- regression_assumption_review_data_frame(results, variable_table, labels)
  if (is.data.frame(assumption_review) && nrow(assumption_review) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Assumption review", assumption_review, used_sheets, title = "Assumption review")
  }
  warnings <- attr(results, "warnings")
  skipped <- attr(results, "skipped")
  used_sheets <- add_analysis_warning_skipped_sheets(workbook, used_sheets, warnings, skipped, skipped_title = "Skipped models")
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

logistic_export_table <- function(
  result,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_b = FALSE,
  show_se = FALSE,
  show_mcfadden = FALSE,
  show_cox_snell = FALSE,
  split_ci = TRUE
) {
  headers <- c("Variable", "Value", logistic_coef_headers(show_b, show_se, split_ci))
  rows <- logistic_coefficient_rows(
    result,
    variable_table = variable_table,
    labels = labels,
    category_table = category_table,
    show_b = show_b,
    show_se = show_se,
    split_ci = split_ci
  )
  table_rows <- lapply(rows, function(row) {
    values <- as.character(row$values %||% character(0))
    length(values) <- length(headers)
    values[is.na(values)] <- ""
    stats::setNames(as.list(values), headers)
  })
  summaries <- logistic_fit_summary_values(result, show_mcfadden = show_mcfadden, show_cox_snell = show_cox_snell)
  summary_labels <- c(
    x2 = "x2(p)",
    r2 = "R2",
    delta_r2 = "Delta R2",
    delta_x2 = "Delta x2(p)",
    aic = "AIC, BIC",
    parallel = "Parallel lines x2(p)",
    status = "Status"
  )
  for (key in names(summaries)) {
    values <- rep("", length(headers))
    values[[1]] <- summary_labels[[key]] %||% key
    values[[2]] <- as.character(summaries[[key]] %||% "")
    table_rows[[length(table_rows) + 1L]] <- stats::setNames(as.list(values), headers)
  }
  if (length(table_rows) == 0) {
    return(data.frame())
  }
  as.data.frame(do.call(rbind, table_rows), stringsAsFactors = FALSE, check.names = FALSE)
}

save_logistic_excel_file <- function(
  results,
  file,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  show_b = FALSE,
  show_se = FALSE,
  show_mcfadden = FALSE,
  show_cox_snell = FALSE,
  split_ci = TRUE
) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  overview <- logistic_model_overview_data_frame(results, variable_table, labels, category_table)
  if (is.data.frame(overview) && nrow(overview) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Model overview", overview, used_sheets, title = "Model overview")
  }
  for (index in seq_along(results %||% list())) {
    result <- results[[index]]
    title <- logistic_result_title(result, variable_table, labels, category_table)
    if (!is.null(result$hierarchical_step) && nzchar(result$hierarchical_step %||% "")) {
      title <- sprintf("%s - %s", title, result$hierarchical_step)
    }
    sheet_name <- sprintf("%s %s", logistic_dependent_title_label(result$dependent, variable_table, labels, category_table), logistic_model_label(result, index))
    table <- logistic_export_table(
      result,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table,
      show_b = show_b,
      show_se = show_se,
      show_mcfadden = show_mcfadden,
      show_cox_snell = show_cox_snell,
      split_ci = split_ci
    )
    used_sheets <- add_excel_table_sheet(workbook, sheet_name, table, used_sheets, title = title)
  }
  assumption_review <- logistic_assumption_review_data_frame(results, variable_table, labels, category_table)
  if (is.data.frame(assumption_review) && nrow(assumption_review) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Assumption review", assumption_review, used_sheets, title = "Assumption review")
  }
  used_sheets <- add_analysis_warning_skipped_sheets(
    workbook,
    used_sheets,
    attr(results, "warnings"),
    attr(results, "skipped"),
    skipped_title = "Skipped models"
  )
  if (length(used_sheets) == 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Logistic regression", data.frame(Message = "No logistic regression results.", stringsAsFactors = FALSE), used_sheets)
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_frequencies_excel_file <- function(result, file) {
  table <- frequency_combined_table(result, result$options %||% list(n_percent = TRUE, mean_sd = TRUE))
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  used_sheets <- add_excel_table_sheet(
    workbook,
    "Frequencies Descriptives",
    table,
    used_sheets
  )
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

add_reliability_excel_sheet <- function(workbook, sheet_name, table, note, used_sheets) {
  used_sheets <- add_excel_table_sheet(workbook, sheet_name, table, used_sheets)
  actual_sheet <- utils::tail(used_sheets, 1)
  if (is.data.frame(table) && nrow(table) > 0 && ncol(table) > 0 && length(note) > 0 && nzchar(note[[1]] %||% "")) {
    styles <- analysis_excel_styles()
    note_row <- 3L + nrow(table) + 2L
    n_cols <- ncol(table)
    widths <- excel_table_column_widths(table)
    openxlsx::writeData(workbook, actual_sheet, note[[1]], startRow = note_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, actual_sheet, cols = seq_len(n_cols), rows = note_row)
    openxlsx::addStyle(workbook, actual_sheet, styles$note, rows = note_row, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(workbook, actual_sheet, rows = note_row, heights = excel_note_row_height(note[[1]], widths, min_height = 42))
  }
  used_sheets
}

add_excel_table_sheet_with_note <- function(workbook, sheet_name, table, note, used_sheets, title = NULL) {
  used_sheets <- add_excel_table_sheet(workbook, sheet_name, table, used_sheets, title = title)
  actual_sheet <- utils::tail(used_sheets, 1)
  if (is.data.frame(table) && nrow(table) > 0 && ncol(table) > 0 && length(note) > 0 && nzchar(note[[1]] %||% "")) {
    styles <- analysis_excel_styles()
    note_row <- 3L + nrow(table) + 2L
    widths <- excel_table_column_widths(table)
    openxlsx::writeData(workbook, actual_sheet, note[[1]], startRow = note_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, actual_sheet, cols = seq_len(ncol(table)), rows = note_row)
    openxlsx::addStyle(workbook, actual_sheet, styles$note, rows = note_row, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(workbook, actual_sheet, rows = note_row, heights = excel_note_row_height(note[[1]], widths, min_height = 42))
  }
  used_sheets
}

save_reliability_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  if (identical(result$type %||% "", "reliability_factors")) {
    used_sheets <- add_reliability_excel_sheet(
      workbook,
      "Reliability",
      reliability_factor_overview_table(result),
      "",
      used_sheets
    )
    for (item in result$factors %||% list()) {
      if (is.data.frame(item$normality_table) && nrow(item$normality_table) > 0) {
        normality <- data.frame(Subfactor = item$subfactor %||% "", item$normality_table, check.names = FALSE)
        used_sheets <- add_excel_table_sheet(workbook, paste0(item$subfactor, " Normality"), normality, used_sheets)
      }
    }
    item_analysis <- reliability_factor_item_analysis_table(result)
    if (is.data.frame(item_analysis) && nrow(item_analysis) > 0) {
      item_note <- reliability_item_analysis_note((result$factors %||% list(result$total))[[1]])
      if ("Total items if item deleted" %in% names(item_analysis)) {
        item_note <- paste(
          item_note,
          "Total items if item deleted is calculated from all items across subfactors after removing each item."
        )
      }
      used_sheets <- add_reliability_excel_sheet(
        workbook,
        "Item analysis",
        item_analysis,
        item_note,
        used_sheets
      )
    }
    openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    return(invisible(file))
  }
  used_sheets <- add_reliability_excel_sheet(
    workbook,
    "Reliability",
    reliability_overview_table(result),
    reliability_method_note(result),
    used_sheets
  )
  if (is.data.frame(result$normality_table) && nrow(result$normality_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Normality", result$normality_table, used_sheets)
  }
  item_analysis <- reliability_item_analysis_table(result)
  if (is.data.frame(item_analysis) && nrow(item_analysis) > 0) {
    used_sheets <- add_reliability_excel_sheet(
      workbook,
      "Item analysis",
      item_analysis,
      reliability_item_analysis_note(result),
      used_sheets
    )
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_correlation_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  significance_note <- if (isTRUE(result$options$significance_levels)) "* p < .05; ** p < .01; *** p < .001" else ""
  used_sheets <- add_excel_table_sheet_with_note(
    workbook,
    "Correlations",
    correlation_matrix_display_table(result),
    significance_note,
    used_sheets
  )
  if (isTRUE(result$options$p_ci)) {
    used_sheets <- add_excel_table_sheet(
      workbook,
      "p-value CI",
      correlation_p_matrix_display_table(result),
      used_sheets
    )
  }
  overview_table <- correlation_model_overview_matrix_display_table(result)
  if (is.data.frame(overview_table) && nrow(overview_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Model overview", overview_table, used_sheets)
  }
  normality_table <- correlation_normality_display_table(result)
  if (isTRUE(result$options$normality) && is.data.frame(normality_table) && nrow(normality_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Normality", normality_table, used_sheets)
  }
  omitted_table <- correlation_omitted_display_table(result)
  if (is.data.frame(omitted_table) && nrow(omitted_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Omitted variables", omitted_table, used_sheets)
  }
  if (is.list(result$latent)) {
    used_sheets <- add_excel_table_sheet(
      workbook,
      "Latent correlations",
      correlation_matrix_display_table(result, result$latent),
      used_sheets
    )
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_factor_analysis_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  used_sheets <- add_excel_table_sheet(workbook, "Overview", result$overview, used_sheets, title = "Factor analysis")
  used_sheets <- add_excel_table_sheet(workbook, "Loadings", result$loadings_table, used_sheets, title = "Pattern / loading matrix")
  if (is.data.frame(result$structure_table) && nrow(result$structure_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Structure", result$structure_table, used_sheets, title = "Structure matrix")
  }
  used_sheets <- add_optional_excel_table_sheet(workbook, "Warnings", result$warnings, used_sheets)
  used_sheets <- add_excel_table_sheet(workbook, "Suitability", result$suitability$overview, used_sheets)
  if (is.data.frame(result$normality_table) && nrow(result$normality_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Normality", result$normality_table, used_sheets)
  }
  if (is.data.frame(result$variance_table) && nrow(result$variance_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Variance", result$variance_table, used_sheets, title = "Variance explained")
  }
  if (is.data.frame(result$factor_correlation_table) && nrow(result$factor_correlation_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Factor correlations", result$factor_correlation_table, used_sheets)
  }
  used_sheets <- add_excel_table_sheet(workbook, "Eigenvalues", result$eigen_table, used_sheets)
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_pca_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  used_sheets <- add_excel_table_sheet(workbook, "Overview", result$overview, used_sheets, title = "Principal component analysis")
  used_sheets <- add_excel_table_sheet(workbook, "Loadings", result$loadings_table, used_sheets, title = "Component loadings")
  used_sheets <- add_optional_excel_table_sheet(workbook, "Warnings", result$warnings, used_sheets)
  used_sheets <- add_excel_table_sheet(workbook, "Suitability", result$suitability$overview, used_sheets)
  if (is.data.frame(result$variance_table) && nrow(result$variance_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Variance", result$variance_table, used_sheets, title = "Variance explained")
  }
  if (is.data.frame(result$component_correlation_table) && nrow(result$component_correlation_table) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Component correlations", result$component_correlation_table, used_sheets)
  }
  used_sheets <- add_excel_table_sheet(workbook, "Eigenvalues", result$eigen_table, used_sheets)
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

crosstab_excel_group_table <- function(results) {
  first <- results[[1]]
  col_labels <- crosstab_value_labels(first$col_var, colnames(first$table), first$category_table)
  split <- crosstab_split_count_percent(first)
  show_trend_p <- crosstab_show_trend_p(results)
  method_notes <- crosstab_method_footnotes(results)
  rows <- list()

  for (result in results) {
    tab <- result$table
    percent <- crosstab_primary_percent_matrix(result)
    row_labels <- crosstab_value_labels(result$row_var, rownames(tab), result$category_table)
    statistic <- crosstab_format_number(result$association$statistic)
    p_marker <- crosstab_method_marker(result, method_notes)
    p_value <- crosstab_format_p(result$association$p)
    if (nzchar(p_marker)) p_value <- paste0(p_value, p_marker)
    trend_marker <- crosstab_trend_marker(result, method_notes)
    effect <- crosstab_general_effect_info(result)$value

    for (row_index in seq_len(nrow(tab))) {
      base <- list(
        `Row variable` = if (row_index == 1) result$row_label else "",
        Value = row_labels[[row_index]]
      )
      if (isTRUE(split)) {
        base[["Total n"]] <- rowSums(tab)[[row_index]]
        base[["Total %"]] <- crosstab_split_percent_text(crosstab_total_percent_value(tab, row_index))
        for (col_index in seq_len(ncol(tab))) {
          label <- col_labels[[col_index]]
          base[[paste(label, "n")]] <- tab[row_index, col_index]
          base[[paste(label, "%")]] <- crosstab_split_percent_text(if (is.null(percent)) NULL else percent[row_index, col_index])
        }
      } else {
        base[["n"]] <- crosstab_cell_text(rowSums(tab)[[row_index]], crosstab_total_percent_value(tab, row_index))
        for (col_index in seq_len(ncol(tab))) {
          base[[col_labels[[col_index]]]] <- crosstab_cell_text(tab[row_index, col_index], if (is.null(percent)) NULL else percent[row_index, col_index])
        }
      }
      base[["x2"]] <- if (row_index == 1) statistic else ""
      base[["p"]] <- if (row_index == 1) p_value else ""
      base[["ES"]] <- if (row_index == 1) effect else ""
      if (isTRUE(show_trend_p)) {
        trend_p <- crosstab_trend_p_text(result)
        if (nzchar(trend_p) && nzchar(trend_marker)) trend_p <- paste0(trend_p, trend_marker)
        base[["p for trend"]] <- if (row_index == 1) trend_p else ""
      }
      rows[[length(rows) + 1L]] <- base
    }
  }

  data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE, check.names = FALSE)), check.names = FALSE)
}

crosstab_excel_group_note <- function(results) {
  method_notes <- crosstab_method_footnotes(results)
  method_lines <- if (is.data.frame(method_notes) && nrow(method_notes) > 0) {
    ifelse(nzchar(method_notes$marker), sprintf("%s. %s", method_notes$marker, method_notes$note), method_notes$note)
  } else {
    character(0)
  }
  effect_lines <- unique(unlist(lapply(results, function(result) {
    effect <- crosstab_general_effect_info(result)
    if (nzchar(effect$note)) effect$note else character(0)
  }), use.names = FALSE))
  trend_lines <- unique(unlist(lapply(results, function(result) {
    if (is.null(result$trend) && nzchar(as.character(result$trend_note %||% ""))) {
      result$trend_note
    } else {
      character(0)
    }
  }), use.names = FALSE))
  paste(c(method_lines, effect_lines, trend_lines), collapse = "\n")
}

save_crosstab_excel_file <- function(result, file) {
  results <- crosstab_result_list(result)
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  for (group in crosstab_results_by_column(results)) {
    first <- group[[1]]
    used_sheets <- add_excel_table_sheet_with_note(
      workbook,
      first$col_label,
      crosstab_excel_group_table(group),
      crosstab_excel_group_note(group),
      used_sheets,
      title = sprintf("Cross-tabulation: %s", first$col_label)
    )
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

add_ttest_anova_result_sheet <- function(workbook, sheet_name, table, note, used_sheets, title = NULL) {
  sheet_name <- excel_sheet_name(sheet_name, used_sheets)
  styles <- analysis_excel_styles()
  title <- title %||% sheet_name
  openxlsx::addWorksheet(workbook, sheet_name)

  if (is.data.frame(table) && nrow(table) > 0 && ncol(table) > 0) {
    table <- excel_normalize_p_columns(table)
    table <- excel_display_column_names(table)
    n_cols <- ncol(table)
    title_row <- 1L
    header_row <- 3L
    body_rows <- (header_row + 1):(header_row + nrow(table))

    openxlsx::writeData(workbook, sheet_name, title, startRow = title_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, sheet_name, cols = seq_len(n_cols), rows = title_row)
    openxlsx::addStyle(workbook, sheet_name, styles$title, rows = title_row, cols = 1, gridExpand = TRUE, stack = TRUE)

    openxlsx::writeData(workbook, sheet_name, table, startRow = header_row, startCol = 1, withFilter = FALSE)
    openxlsx::addStyle(workbook, sheet_name, styles$top, rows = header_row, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$header, rows = header_row, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$body, rows = body_rows, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE)
    if (n_cols >= 2) {
      openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 2, gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::addStyle(workbook, sheet_name, styles$bottom, rows = header_row + nrow(table), cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)

    if (length(note) > 0 && nzchar(note[[1]])) {
      note_row <- header_row + nrow(table) + 2L
      openxlsx::writeData(workbook, sheet_name, note[[1]], startRow = note_row, startCol = 1, colNames = FALSE)
      openxlsx::mergeCells(workbook, sheet_name, cols = seq_len(n_cols), rows = note_row)
      openxlsx::addStyle(workbook, sheet_name, styles$note, rows = note_row, cols = 1, gridExpand = TRUE, stack = TRUE)
      openxlsx::setRowHeights(workbook, sheet_name, rows = note_row, heights = excel_note_row_height(note[[1]], excel_table_column_widths(table), min_height = 42))
    }

    widths <- rep(12, n_cols)
    names(widths) <- names(table)
    widths[names(widths) %in% c("Variable", "Value", "Dependent variable", "Independent variable", "Normality", "Analysis", "Post-hoc")] <- 20
    widths[names(widths) %in% c("Package")] <- 14
    openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(n_cols), widths = widths)
    openxlsx::freezePane(workbook, sheet_name, firstActiveRow = header_row + 1)
  } else {
    openxlsx::writeData(workbook, sheet_name, "No data")
  }

  c(used_sheets, sheet_name)
}

add_paired_grouped_excel_sheet <- function(workbook, sheet_name, table, used_sheets, title = NULL, type = c("scale", "count"), note = "", show_effect_size = FALSE) {
  type <- match.arg(type)
  sheet_name <- excel_sheet_name(sheet_name, used_sheets)
  styles <- analysis_excel_styles()
  title <- title %||% sheet_name
  openxlsx::addWorksheet(workbook, sheet_name)
  if (!is.data.frame(table) || nrow(table) == 0) {
    openxlsx::writeData(workbook, sheet_name, "No data")
    return(c(used_sheets, sheet_name))
  }

  show_effect_size <- isTRUE(show_effect_size) && paired_has_effect(table)
  effect_labels <- if (show_effect_size) paired_effect_labels(table) else character(0)
  if (identical(type, "scale")) {
    summary_labels <- paired_summary_header_labels(table)
    mean_sd <- isTRUE(attr(table, "mean_sd", exact = TRUE))
    export_columns <- if (isTRUE(mean_sd)) {
      c("Variable", "Pre_MS", "Post_MS", "Statistic", "p")
    } else {
      c("Variable", "Pre_M", "Pre_SD", "Post_M", "Post_SD", "Statistic", "p")
    }
    export <- table[, export_columns, drop = FALSE]
    for (label in effect_labels) {
      export[[label]] <- vapply(seq_len(nrow(table)), function(index) paired_effect_value_for_label(table, index, label), character(1))
    }
    statistic_label <- paired_scale_statistic_label(table)
    effect_header_labels <- vapply(effect_labels, paired_effect_header_text, character(1), label_count = length(effect_labels))
    if (isTRUE(mean_sd)) {
      header_top <- c("Variable", "Pre", "Post", statistic_label, "p", effect_header_labels)
      header_bottom <- c("", summary_labels$combined, summary_labels$combined, "", "", rep("", length(effect_labels)))
      merge_cols <- c(list(1, 2, 3, 4, 5), as.list(seq_along(effect_labels) + 5L))
      widths <- c(28, 18, 18, 14, 12, rep(14, length(effect_labels)))
    } else {
      header_top <- c("Variable", "Pre", "", "Post", "", statistic_label, "p", effect_header_labels)
      header_bottom <- c("", summary_labels$center, summary_labels$spread, summary_labels$center, summary_labels$spread, "", "", rep("", length(effect_labels)))
      merge_cols <- c(list(1, 2:3, 4:5, 6, 7), as.list(seq_along(effect_labels) + 7L))
      widths <- c(28, 12, 12, 12, 12, 14, 12, rep(14, length(effect_labels)))
    }
  } else {
    post_columns <- grep("^Post_", names(table), value = TRUE)
    post_labels <- sub("^Post_", "", post_columns)
    include_statistic <- paired_has_statistic(table)
    export_columns <- c("Variable", "Pre", post_columns)
    if (include_statistic) export_columns <- c(export_columns, "Statistic")
    export_columns <- c(export_columns, "p")
    export <- table[, export_columns, drop = FALSE]
    for (label in effect_labels) {
      export[[label]] <- vapply(seq_len(nrow(table)), function(index) paired_effect_value_for_label(table, index, label), character(1))
    }
    statistic_label <- paired_count_statistic_label(table)
    effect_header_labels <- vapply(effect_labels, paired_effect_header_text, character(1), label_count = length(effect_labels))
    header_top <- c("Variable", "Pre", "Post", rep("", max(0, length(post_columns) - 1L)), if (include_statistic) statistic_label, "p", effect_header_labels)
    header_bottom <- c("", "", post_labels, if (include_statistic) "", "", rep("", length(effect_labels)))
    merge_cols <- c(
      list(1, 2),
      if (length(post_columns) > 1L) list(3:(2 + length(post_columns))) else list(),
      if (include_statistic) list(match("Statistic", export_columns)) else list(),
      list(match("p", export_columns)),
      as.list(match(effect_labels, names(export)))
    )
    widths <- c(28, 16, rep(12, length(post_columns)), if (include_statistic) 14, 12, rep(14, length(effect_labels)))
  }
  method_markers <- vapply(seq_len(nrow(table)), function(index) paired_method_marker_for_row(table, index), character(1))
  if (any(nzchar(method_markers)) && "p" %in% names(export)) {
    export$p <- ifelse(nzchar(as.character(export$p)), paste0(export$p, method_markers), export$p)
  }
  export <- excel_normalize_p_columns(export)

  title_row <- 1L
  header_row <- 3L
  subheader_row <- 4L
  body_start <- 5L
  body_rows <- body_start:(body_start + nrow(export) - 1L)
  n_cols <- ncol(export)

  excel_apply_title_row(workbook, sheet_name, title, n_cols, styles, row = title_row)

  openxlsx::writeData(workbook, sheet_name, t(header_top), startRow = header_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(workbook, sheet_name, t(header_bottom), startRow = subheader_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(workbook, sheet_name, export, startRow = body_start, startCol = 1, colNames = FALSE, withFilter = FALSE)
  for (cols in merge_cols) {
    if (length(cols) == 1L && !nzchar(header_bottom[[cols]])) {
      openxlsx::mergeCells(workbook, sheet_name, cols = cols, rows = header_row:subheader_row)
    } else if (length(cols) > 1L) {
      openxlsx::mergeCells(workbook, sheet_name, cols = cols, rows = header_row)
    }
  }
  excel_apply_two_tier_header_style(workbook, sheet_name, header_row, subheader_row, n_cols, styles)
  openxlsx::addStyle(workbook, sheet_name, styles$body, rows = body_rows, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE)
  if (identical(type, "count")) {
    openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 2, gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::addStyle(workbook, sheet_name, styles$bottom, rows = body_start + nrow(export) - 1L, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
  if (length(note) > 0 && nzchar(note[[1]])) {
    note_row <- body_start + nrow(export) + 1L
    excel_apply_note_row(workbook, sheet_name, note, note_row, n_cols, widths, styles, min_height = 36)
  }
  openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(n_cols), widths = widths)
  openxlsx::freezePane(workbook, sheet_name, firstActiveRow = body_start)
  c(used_sheets, sheet_name)
}

add_paired_rm_grouped_excel_sheet <- function(workbook, sheet_name, table, used_sheets, title = NULL, note = "", type = c("scale", "count")) {
  type <- match.arg(type)
  sheet_name <- excel_sheet_name(sheet_name, used_sheets)
  styles <- analysis_excel_styles()
  title <- title %||% sheet_name
  openxlsx::addWorksheet(workbook, sheet_name)
  if (!is.data.frame(table) || nrow(table) == 0) {
    openxlsx::writeData(workbook, sheet_name, "No data")
    return(c(used_sheets, sheet_name))
  }
  time_label_columns <- grep("^Time[0-9]+_label$", names(table), value = TRUE)
  if (length(time_label_columns) == 0) {
    return(add_ttest_anova_result_sheet(workbook, sheet_name, table, note, used_sheets, title = title))
  }
  time_indices <- sort(as.integer(sub("^Time([0-9]+)_label$", "\\1", time_label_columns)))
  table <- paired_rm_fill_summary_columns(table, time_indices)
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
  export_columns <- c(
    "Repeated variables",
    "N",
    if (identical(type, "count")) {
      as.vector(rbind(paste0("Time", time_indices, "_0"), paste0("Time", time_indices, "_1")))
    } else if (isTRUE(attr(table, "mean_sd", exact = TRUE)) || isTRUE(attr(table, "median_iqr", exact = TRUE))) {
      if (isTRUE(attr(table, "mean_sd", exact = TRUE)) && isTRUE(attr(table, "median_iqr", exact = TRUE))) {
        paste0("Time", time_indices, "_Summary")
      } else {
      columns <- character(0)
      for (index in time_indices) {
        if (isTRUE(attr(table, "mean_sd", exact = TRUE))) columns <- c(columns, paste0("Time", index, "_MS"))
        if (isTRUE(attr(table, "median_iqr", exact = TRUE))) columns <- c(columns, paste0("Time", index, "_MedianIQR"))
      }
      columns
      }
    } else {
      as.vector(rbind(paste0("Time", time_indices, "_M"), paste0("Time", time_indices, "_SD")))
    },
    "Statistic",
    "p",
    if (include_es) "ES_overall",
    es_columns,
    "Post-hoc"
  )
  export_columns <- export_columns[export_columns %in% names(table)]
  export <- table[, export_columns, drop = FALSE]
  export <- excel_normalize_p_columns(export)
  time_labels <- vapply(time_indices, function(index) {
    labels <- as.character(table[[paste0("Time", index, "_label")]] %||% "")
    labels <- labels[nzchar(labels)]
    if (length(labels) > 0) labels[[1]] else paste0("Time ", index)
  }, character(1))
  time_markers <- vapply(time_indices, function(index) {
    markers <- as.character(table[[paste0("Time", index, "_marker")]] %||% "")
    markers <- markers[nzchar(markers)]
    if (length(markers) > 0) markers[[1]] else letters[[index]]
  }, character(1))
  time_header_labels <- sprintf("%s (%s)", time_labels, time_markers)
  summary_labels <- if (identical(type, "count")) {
    c("0", "1")
  } else if (isTRUE(attr(table, "mean_sd", exact = TRUE)) || isTRUE(attr(table, "median_iqr", exact = TRUE))) {
    if (isTRUE(attr(table, "mean_sd", exact = TRUE)) && isTRUE(attr(table, "median_iqr", exact = TRUE))) {
      "M \u00B1 SD/\nMedian(Q1~Q3)"
    } else {
    c(if (isTRUE(attr(table, "mean_sd", exact = TRUE))) "M \u00B1 SD", if (isTRUE(attr(table, "median_iqr", exact = TRUE))) "Median(Q1~Q3)")
    }
  } else {
    c("M", "SD")
  }
  columns_per_time <- length(summary_labels)
  time_top <- unlist(lapply(time_header_labels, function(label) c(label, rep("", columns_per_time - 1L))), use.names = FALSE)
  header_top <- c("Repeated variables", "N", time_top, statistic_label, "p", if (include_es) c("ES", rep("", length(es_columns))), "Post-hoc")
  header_bottom <- c(
    "",
    "",
    rep(summary_labels, length(time_labels)),
    "",
    "",
    if (include_es) c(as.character(table$ES_overall_label[[1]] %||% "overall"), es_labels),
    ""
  )
  merge_cols <- c(list(1, 2), lapply(seq_along(time_indices), function(index) {
    start <- 3 + (index - 1L) * columns_per_time
    start:(start + columns_per_time - 1L)
  }))
  if (include_es) {
    es_start <- 3 + length(time_indices) * columns_per_time + 2L
    merge_cols <- c(merge_cols, list(es_start:(es_start + length(es_columns))))
    tail_start <- es_start + length(es_columns) + 1L
  } else {
    tail_start <- 3 + length(time_indices) * columns_per_time
  }
  merge_cols <- c(merge_cols, as.list(seq.int(tail_start, ncol(export))))
  widths <- c(16, 6, rep(if (columns_per_time == 1L) 12 else 10, length(time_indices) * columns_per_time), 8, 8, if (include_es) rep(8, 1L + length(es_columns)), 16)

  title_row <- 1L
  header_row <- 3L
  subheader_row <- 4L
  body_start <- 5L
  body_rows <- body_start:(body_start + nrow(export) - 1L)
  n_cols <- ncol(export)

  excel_apply_title_row(workbook, sheet_name, title, n_cols, styles, row = title_row)
  openxlsx::writeData(workbook, sheet_name, t(header_top), startRow = header_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(workbook, sheet_name, t(header_bottom), startRow = subheader_row, startCol = 1, colNames = FALSE)
  openxlsx::writeData(workbook, sheet_name, export, startRow = body_start, startCol = 1, colNames = FALSE, withFilter = FALSE)
  for (cols in merge_cols) {
    if (length(cols) == 1L && cols >= 1L && cols <= n_cols) {
      openxlsx::mergeCells(workbook, sheet_name, cols = cols, rows = header_row:subheader_row)
    } else if (length(cols) > 1L && all(cols <= n_cols)) {
      openxlsx::mergeCells(workbook, sheet_name, cols = cols, rows = header_row)
    }
  }
  excel_apply_two_tier_header_style(workbook, sheet_name, header_row, subheader_row, n_cols, styles)
  openxlsx::addStyle(workbook, sheet_name, styles$body, rows = body_rows, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(workbook, sheet_name, styles$left, rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(workbook, sheet_name, styles$bottom, rows = body_start + nrow(export) - 1L, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)
  if (length(note) > 0 && nzchar(note[[1]])) {
    note_row <- body_start + nrow(export) + 1L
    excel_apply_note_row(workbook, sheet_name, note, note_row, n_cols, widths, styles, min_height = 36)
  }
  openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(n_cols), widths = widths[seq_len(n_cols)])
  openxlsx::freezePane(workbook, sheet_name, firstActiveRow = body_start)
  c(used_sheets, sheet_name)
}

save_ttest_anova_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  if (is.data.frame(result$assumption_review) && nrow(result$assumption_review) > 0) {
    used_sheets <- add_excel_table_sheet(
      workbook,
      "Assumption review",
      result$assumption_review,
      used_sheets,
      title = "Assumption review"
    )
  }
  used_sheets <- add_excel_table_sheet(
    workbook,
    "Model overview",
    result$overview,
    used_sheets,
    title = "Model overview"
  )
  used_sheets <- add_analysis_warning_skipped_sheets(workbook, used_sheets, result$warnings, result$skipped, skipped_title = "Skipped analyses")
  for (item in result$results %||% list()) {
    used_sheets <- add_ttest_anova_result_sheet(
      workbook,
      item$title %||% "Result",
      item$table,
      item$note %||% "",
      used_sheets,
      title = item$title %||% "Result"
    )
    if (is.data.frame(item$posthoc) && nrow(item$posthoc) > 0) {
      used_sheets <- add_ttest_anova_result_sheet(
        workbook,
        paste(item$title %||% "Result", "posthoc"),
        item$posthoc,
        "",
        used_sheets,
        title = paste(item$title %||% "Result", "Post-hoc")
      )
    }
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_ancova_excel_file <- function(result, file, variable_table = NULL, labels = character(0)) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  overview <- ancova_model_overview_table(result, variable_table, labels)
  if (is.data.frame(overview) && nrow(overview) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Model overview", overview, used_sheets, title = "Model overview")
  }
  combined <- ancova_combined_result_table(result, variable_table, labels)
  if (is.data.frame(combined) && nrow(combined) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(
      workbook,
      "ANCOVA table",
      combined,
      ancova_combined_note(result, variable_table, labels),
      used_sheets,
      title = "ANCOVA table"
    )
  }
  assumption <- ancova_assumption_review_table(result, variable_table, labels)
  if (is.data.frame(assumption) && nrow(assumption) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Assumption summary", assumption, used_sheets, title = "Assumption summary")
  }
  normality <- ancova_normality_review_table(result, variable_table, labels)
  if (is.data.frame(normality) && nrow(normality) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Normality diagnostics", normality, used_sheets, title = "Normality diagnostics")
  }
  interaction_terms <- ancova_interaction_terms_review_table(result, variable_table, labels)
  if (is.data.frame(interaction_terms) && nrow(interaction_terms) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Interaction terms", interaction_terms, used_sheets, title = "Interaction terms")
  }
  slope_homogeneity <- ancova_slope_homogeneity_review_table(result, variable_table, labels)
  if (is.data.frame(slope_homogeneity) && nrow(slope_homogeneity) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Slope homogeneity", slope_homogeneity, used_sheets, title = "Regression slope homogeneity")
  }
  simple_effects <- ancova_simple_effects_review_table(result, variable_table, labels)
  if (is.data.frame(simple_effects) && nrow(simple_effects) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Simple effects", simple_effects, used_sheets, title = "Simple group effects")
  }
  linearity <- ancova_linearity_review_table(result, variable_table, labels)
  if (is.data.frame(linearity) && nrow(linearity) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Linearity diagnostics", linearity, used_sheets, title = "Covariate linearity check")
  }
  collinearity <- ancova_collinearity_review_table(result, variable_table, labels)
  if (is.data.frame(collinearity) && nrow(collinearity) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Collinearity diagnostics", collinearity, used_sheets, title = "Collinearity diagnostics")
  }
  influence <- ancova_influence_review_table(result, variable_table, labels)
  if (is.data.frame(influence) && nrow(influence) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Influence diagnostics", influence, used_sheets, title = "Influence diagnostics")
  }
  influence_sensitivity <- ancova_influence_sensitivity_review_table(result, variable_table, labels)
  if (is.data.frame(influence_sensitivity) && nrow(influence_sensitivity) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Influence sensitivity", influence_sensitivity, used_sheets, title = "Influence sensitivity analysis")
  }
  used_sheets <- add_analysis_warning_skipped_sheets(workbook, used_sheets, NULL, result$skipped, skipped_title = "Skipped analyses")
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_nonparametric_paired_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  write_paired_guard_sheets <- function(part, prefix = "") {
    if (is.data.frame(part$warnings) && nrow(part$warnings) > 0) {
      used_sheets <<- add_ttest_anova_result_sheet(workbook, paste0(prefix, "Warnings"), part$warnings, "", used_sheets, title = "Warnings")
    }
    if (is.data.frame(part$skipped) && nrow(part$skipped) > 0) {
      used_sheets <<- add_ttest_anova_result_sheet(workbook, paste0(prefix, "Skipped"), part$skipped, "", used_sheets, title = "Skipped pairs")
    }
  }
  write_part <- function(part, prefix = "") {
    if (identical(part$type, "nonparametric_paired_rm")) {
      if (is.data.frame(part$display_table) && nrow(part$display_table) > 0) {
        table <- part$display_table
        attr(table, "median_iqr") <- isTRUE(part$options$median_iqr)
        if (!isTRUE(part$options$effect_size)) {
          drop_columns <- c("ES_overall", "ES_overall_label", "PairwiseEffectSizeLabel", "EffectSizeLabel", grep("^ES_[0-9]+_[0-9]+(_label)?$", names(table), value = TRUE))
          table <- table[, setdiff(names(table), drop_columns), drop = FALSE]
        }
        used_sheets <<- add_paired_rm_grouped_excel_sheet(
          workbook,
          paste0(prefix, "Repeated summary"),
          table,
          used_sheets,
          note = paired_rm_table_method_note(table),
          title = "Nonparametric paired test: continuous / ordinal",
          type = "scale"
        )
      }
      if (is.data.frame(part$count_table) && nrow(part$count_table) > 0) {
        used_sheets <<- add_paired_rm_grouped_excel_sheet(
          workbook,
          paste0(prefix, "Repeated n"),
          part$count_table,
          used_sheets,
          note = paired_rm_table_method_note(part$count_table),
          title = "Nonparametric paired test: binary",
          type = "count"
        )
      }
      if (is.data.frame(part$posthoc) && nrow(part$posthoc) > 0) {
        used_sheets <<- add_ttest_anova_result_sheet(
          workbook,
          paste0(prefix, "Posthoc"),
          nonparametric_paired_posthoc_display_table(part),
          paired_rm_posthoc_note(part),
          used_sheets,
          title = "Post-hoc pairwise comparisons"
        )
      }
      write_paired_guard_sheets(part, prefix)
    } else {
      if (is.data.frame(part$scale_table) && nrow(part$scale_table) > 0) {
        table <- part$scale_table
        attr(table, "median_iqr") <- isTRUE(part$options$median_iqr)
        used_sheets <<- add_paired_grouped_excel_sheet(
          workbook,
          paste0(prefix, "Paired summary"),
          table,
          used_sheets,
          title = "Nonparametric paired test: summary",
          type = "scale",
          note = paired_method_note(table, show_effect_size = isTRUE(part$options$effect_size)),
          show_effect_size = isTRUE(part$options$effect_size)
        )
      }
      if (is.data.frame(part$count_table) && nrow(part$count_table) > 0) {
        used_sheets <<- add_paired_grouped_excel_sheet(
          workbook,
          paste0(prefix, "Paired n"),
          part$count_table,
          used_sheets,
          title = "Nonparametric paired test: n by level",
          type = "count",
          note = paired_count_method_note(part, show_effect_size = isTRUE(part$options$effect_size)),
          show_effect_size = isTRUE(part$options$effect_size)
        )
      }
      write_paired_guard_sheets(part, prefix)
    }
  }
  if (identical(result$type, "nonparametric_paired_combined")) {
    write_part(result$paired, "Two ")
    write_part(result$paired_rm, "Repeated ")
  } else {
    write_part(result)
  }
  if (length(used_sheets) == 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Nonparametric paired", result$table, "", used_sheets, title = "Nonparametric paired test")
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_paired_excel_file <- function(result, file) {
  if (identical(result$type, "paired_rm")) {
    return(save_paired_rm_excel_file(result, file))
  }
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  write_paired_guard_sheets <- function(part, prefix = "") {
    if (is.data.frame(part$warnings) && nrow(part$warnings) > 0) {
      used_sheets <<- add_ttest_anova_result_sheet(workbook, paste0(prefix, "Warnings"), part$warnings, "", used_sheets, title = "Warnings")
    }
    if (is.data.frame(part$skipped) && nrow(part$skipped) > 0) {
      used_sheets <<- add_ttest_anova_result_sheet(workbook, paste0(prefix, "Skipped"), part$skipped, "", used_sheets, title = "Skipped pairs")
    }
  }
  if (identical(result$type, "paired_combined")) {
    paired_part <- result$paired
    rm_part <- result$paired_rm
    paired_overview <- paired_model_overview_table(paired_part)
    paired_assumption_review <- paired_assumption_review_table(paired_part)
    rm_overview <- paired_rm_model_overview_table(rm_part)
    rm_assumption_review <- paired_rm_assumption_review_table(rm_part)
    if (is.data.frame(paired_overview) && nrow(paired_overview) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, "Paired overview", paired_overview, used_sheets, title = "Model overview")
    }
    if (is.data.frame(rm_overview) && nrow(rm_overview) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, "Repeated overview", rm_overview, used_sheets, title = "Model overview")
    }
    if (is.data.frame(paired_part$scale_table) && nrow(paired_part$scale_table) > 0) {
      used_sheets <- add_paired_grouped_excel_sheet(
        workbook,
        "Paired M SD",
        paired_scale_display_table(paired_part),
        used_sheets,
        title = "Paired test: M and SD",
        type = "scale",
        note = paired_method_note(paired_scale_display_table(paired_part), show_effect_size = isTRUE(paired_part$options$effect_size)),
        show_effect_size = isTRUE(paired_part$options$effect_size)
      )
    }
    if (is.data.frame(paired_part$count_table) && nrow(paired_part$count_table) > 0) {
      used_sheets <- add_paired_grouped_excel_sheet(
        workbook,
        "Paired n",
        paired_part$count_table,
        used_sheets,
        title = "Paired test: n by level",
        type = "count",
        note = paired_count_method_note(paired_part, show_effect_size = isTRUE(paired_part$options$effect_size)),
        show_effect_size = isTRUE(paired_part$options$effect_size)
      )
    }
    if (isTRUE(paired_part$options$assumption_check) && is.data.frame(paired_part$checks) && nrow(paired_part$checks) > 0) {
      used_sheets <- add_ttest_anova_result_sheet(
        workbook,
        "Paired assumptions",
        paired_part$checks,
        "Outliers were evaluated using values beyond 3*IQR from the paired difference distribution.",
        used_sheets,
        title = "Paired assumption check"
      )
    }
    if (is.data.frame(paired_assumption_review) && nrow(paired_assumption_review) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, "Paired assumption review", paired_assumption_review, used_sheets, title = "Assumption review")
    }
    write_paired_guard_sheets(paired_part, "Paired ")
    if (is.data.frame(rm_part$display_table) && nrow(rm_part$display_table) > 0) {
      table <- paired_rm_table_with_options(rm_part$display_table, rm_part$options)
      used_sheets <- add_paired_rm_grouped_excel_sheet(
        workbook,
        "Repeated M SD",
        table,
        used_sheets,
        note = paired_rm_table_method_note(table),
        title = "Repeated-measures test: continuous / ordinal",
        type = "scale"
      )
    }
    if (is.data.frame(rm_part$count_table) && nrow(rm_part$count_table) > 0) {
      used_sheets <- add_paired_rm_grouped_excel_sheet(
        workbook,
        "Repeated n",
        rm_part$count_table,
        used_sheets,
        note = paired_rm_table_method_note(rm_part$count_table),
        title = "Repeated-measures test: binary",
        type = "count"
      )
    }
    if (is.data.frame(rm_part$posthoc) && nrow(rm_part$posthoc) > 0) {
      used_sheets <- add_ttest_anova_result_sheet(
        workbook,
        "Repeated posthoc",
        rm_part$posthoc,
        paired_rm_posthoc_note(rm_part),
        used_sheets,
        title = "Post-hoc pairwise comparisons"
      )
    }
    if (isTRUE(rm_part$options$assumption_check) && is.data.frame(rm_part$assumption) && nrow(rm_part$assumption) > 0) {
      used_sheets <- add_ttest_anova_result_sheet(
        workbook,
        "Repeated assumptions",
        rm_part$assumption,
        "",
        used_sheets,
        title = "Repeated-measures assumption check"
      )
    }
    if (is.data.frame(rm_assumption_review) && nrow(rm_assumption_review) > 0) {
      used_sheets <- add_excel_table_sheet(workbook, "Repeated assumption review", rm_assumption_review, used_sheets, title = "Assumption review")
    }
    if (is.data.frame(rm_part$skipped) && nrow(rm_part$skipped) > 0) {
      used_sheets <- add_ttest_anova_result_sheet(workbook, "Repeated skipped", rm_part$skipped, "", used_sheets, title = "Skipped repeated-measures rows")
    }
    openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    return(invisible(file))
  }
  overview <- paired_model_overview_table(result)
  assumption_review <- paired_assumption_review_table(result)
  if (is.data.frame(overview) && nrow(overview) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Model overview", overview, used_sheets, title = "Model overview")
  }
  if (is.data.frame(result$scale_table) && nrow(result$scale_table) > 0) {
    used_sheets <- add_paired_grouped_excel_sheet(
      workbook,
      "Paired M SD",
      paired_scale_display_table(result),
      used_sheets,
      title = "Paired test: M and SD",
      type = "scale",
      note = paired_method_note(paired_scale_display_table(result), show_effect_size = isTRUE(result$options$effect_size)),
      show_effect_size = isTRUE(result$options$effect_size)
    )
  }
  if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
    used_sheets <- add_paired_grouped_excel_sheet(
      workbook,
      "Paired n",
      result$count_table,
      used_sheets,
      title = "Paired test: n by level",
      type = "count",
      note = paired_count_method_note(result, show_effect_size = isTRUE(result$options$effect_size)),
      show_effect_size = isTRUE(result$options$effect_size)
    )
  }
  if (length(used_sheets) == 0) {
    used_sheets <- add_ttest_anova_result_sheet(
      workbook,
      "Paired test",
      result$table,
      "",
      used_sheets,
      title = "Paired test"
    )
  }
  if (isTRUE(result$options$assumption_check) && is.data.frame(result$checks) && nrow(result$checks) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(
      workbook,
      "Assumption check",
      result$checks,
      "Outliers were evaluated using values beyond 3*IQR from the paired difference distribution.",
      used_sheets,
      title = "Assumption check"
    )
  }
  if (is.data.frame(assumption_review) && nrow(assumption_review) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Assumption review", assumption_review, used_sheets, title = "Assumption review")
  }
  write_paired_guard_sheets(result)
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

save_paired_rm_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  overview <- paired_rm_model_overview_table(result)
  assumption_review <- paired_rm_assumption_review_table(result)
  if (is.data.frame(overview) && nrow(overview) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Model overview", overview, used_sheets, title = "Model overview")
  }
  if (is.data.frame(result$display_table) && nrow(result$display_table) > 0) {
    table <- paired_rm_table_with_options(result$display_table, result$options)
    used_sheets <- add_paired_rm_grouped_excel_sheet(
      workbook,
      "Repeated M SD",
      table,
      used_sheets,
      note = paired_rm_table_method_note(table),
      title = "Repeated-measures test: continuous / ordinal",
      type = "scale"
    )
  }
  if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
    used_sheets <- add_paired_rm_grouped_excel_sheet(
      workbook,
      "Repeated n",
      result$count_table,
      used_sheets,
      note = paired_rm_table_method_note(result$count_table),
      title = "Repeated-measures test: binary",
      type = "count"
    )
  }
  if (length(used_sheets) == 0) {
    used_sheets <- add_ttest_anova_result_sheet(
      workbook,
      "Repeated test",
      result$table,
      paired_rm_method_note(result),
      used_sheets,
      title = "Repeated-measures test"
    )
  }
  if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(
      workbook,
      "Posthoc",
      result$posthoc,
      paired_rm_posthoc_note(result),
      used_sheets,
      title = "Post-hoc pairwise comparisons"
    )
  }
  if (isTRUE(result$options$assumption_check) && is.data.frame(result$assumption) && nrow(result$assumption) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(
      workbook,
      "Assumptions",
      result$assumption,
      "",
      used_sheets,
      title = "Assumption check"
    )
  }
  if (is.data.frame(assumption_review) && nrow(assumption_review) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Assumption review", assumption_review, used_sheets, title = "Assumption review")
  }
  if (is.data.frame(result$skipped) && nrow(result$skipped) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Skipped", result$skipped, "", used_sheets, title = "Skipped repeated-measures rows")
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}
