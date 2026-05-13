# Result export helpers for EasyFlow Statistics.

excel_sheet_name <- function(name, used = character(0)) {
  name <- gsub("[\\\\/\\?\\*\\[\\]:]", " ", as.character(name %||% "Sheet"))
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

regression_excel_styles <- function() {
  list(
    title = openxlsx::createStyle(textDecoration = "bold", fontSize = 12, halign = "left"),
    header = openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom", borderStyle = "thick"),
    body = openxlsx::createStyle(halign = "center", valign = "center"),
    left = openxlsx::createStyle(halign = "left", valign = "center", wrapText = TRUE),
    wrap = openxlsx::createStyle(halign = "center", valign = "center", wrapText = TRUE),
    summary = openxlsx::createStyle(halign = "center", valign = "center"),
    warning = openxlsx::createStyle(halign = "center", valign = "center", wrapText = TRUE, fontColour = "#9A3412"),
    note = openxlsx::createStyle(halign = "left", valign = "top", wrapText = TRUE),
    top = openxlsx::createStyle(border = "top", borderStyle = "thick"),
    bottom = openxlsx::createStyle(border = "bottom", borderStyle = "thick")
  )
}

add_excel_table_sheet <- function(workbook, sheet_name, table, used_sheets, merge_shared_independent = FALSE) {
  sheet_name <- excel_sheet_name(sheet_name, used_sheets)
  styles <- regression_excel_styles()
  openxlsx::addWorksheet(workbook, sheet_name)
  if (is.data.frame(table) && nrow(table) > 0) {
    openxlsx::writeData(workbook, sheet_name, table, startRow = 1, startCol = 1, withFilter = FALSE)
    openxlsx::addStyle(workbook, sheet_name, styles$header, rows = 1, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$body, rows = 2:(nrow(table) + 1), cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(workbook, sheet_name, styles$left, rows = 2:(nrow(table) + 1), cols = 1, gridExpand = TRUE, stack = TRUE)
    if (identical(sheet_name, "Model overview") && ncol(table) > 1) {
      openxlsx::addStyle(workbook, sheet_name, styles$wrap, rows = 2:(nrow(table) + 1), cols = 2:ncol(table), gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::addStyle(workbook, sheet_name, styles$bottom, rows = nrow(table) + 1, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
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
        excel_row <- independent_row + 1
        openxlsx::mergeCells(workbook, sheet_name, cols = 2:ncol(table), rows = excel_row)
        openxlsx::addStyle(workbook, sheet_name, styles$body, rows = excel_row, cols = 2, gridExpand = TRUE, stack = TRUE)
      }
    }
    if (identical(sheet_name, "Model overview")) {
      overview_widths <- c(28, rep(24, max(0, ncol(table) - 1)))
      openxlsx::setColWidths(workbook, sheet_name, cols = seq_along(table), widths = overview_widths[seq_along(table)])
    } else {
      openxlsx::setColWidths(workbook, sheet_name, cols = seq_along(table), widths = "auto")
    }
  } else {
    openxlsx::writeData(workbook, sheet_name, "No data")
  }
  c(used_sheets, sheet_name)
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
  styles <- regression_excel_styles()
  n_cols <- max(1, ncol(table))
  data_start_row <- 3
  header_row <- data_start_row
  title <- title %||% coefficient_fit_line(result)

  openxlsx::addWorksheet(workbook, sheet_name)
  openxlsx::writeData(workbook, sheet_name, title, startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::mergeCells(workbook, sheet_name, cols = 1:n_cols, rows = 1)
  openxlsx::addStyle(workbook, sheet_name, styles$title, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  if (is.data.frame(table) && nrow(table) > 0 && ncol(table) > 0) {
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

    note_row <- summary_end + 2
    note <- coefficient_note_line(result, show_vif, show_sr2, show_f2)
    openxlsx::writeData(workbook, sheet_name, note, startRow = note_row, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, sheet_name, cols = 1:ncol(table), rows = note_row)
    openxlsx::addStyle(workbook, sheet_name, styles$note, rows = note_row, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(workbook, sheet_name, rows = note_row, heights = 48)

    widths <- c(20, rep(10, max(0, ncol(table) - 1)))
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
  "EasyFlow_Statistics_Coefficients.csv"
}

analysis_results_html_filename <- function() {
  sprintf("EasyFlow_Statistics_Results_%s.html", format(Sys.time(), "%Y%m%d_%H%M%S"))
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
    show_vif = show_vif,
    show_sr2 = show_sr2,
    show_f2 = show_f2
  )
}

plot_png_file <- function(plot_function, result, dpi = 600, width = 4.375, height = 4.375) {
  path <- tempfile("easyflow_plot_", fileext = ".png")
  grDevices::png(path, width = width, height = height, units = "in", res = dpi)
  closed <- FALSE
  on.exit({
    if (!closed) {
      grDevices::dev.off()
    }
  }, add = TRUE)
  plot_function(result)
  grDevices::dev.off()
  closed <- TRUE
  path
}

ps_quote <- function(value) {
  paste0("'", gsub("'", "''", enc2utf8(as.character(value %||% "")), fixed = TRUE), "'")
}

run_windows_dialog_script <- function(script) {
  powershell <- Sys.which("powershell.exe")
  if (!nzchar(powershell)) {
    powershell <- Sys.which("powershell")
  }
  if (!nzchar(powershell)) {
    return(character(0))
  }
  output <- tryCatch(
    system2(
      powershell,
      c("-NoProfile", "-Sta", "-ExecutionPolicy", "Bypass", "-Command", script),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) character(0)
  )
  output <- trimws(output[nzchar(output)])
  if (length(output) > 0) output[[1]] else character(0)
}

choose_windows_save_file <- function(default_name, title) {
  if (.Platform$OS.type != "windows") {
    return(character(0))
  }
  script <- paste(
    "Add-Type -AssemblyName System.Windows.Forms;",
    "$dialog = New-Object System.Windows.Forms.SaveFileDialog;",
    "$dialog.Title =", ps_quote(title), ";",
    "$dialog.FileName =", ps_quote(default_name), ";",
    "$dialog.Filter = 'Excel Workbook (*.xlsx)|*.xlsx|All Files (*.*)|*.*';",
    "$dialog.DefaultExt = 'xlsx';",
    "$dialog.AddExtension = $true;",
    "$dialog.OverwritePrompt = $true;",
    "if ($dialog.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) {",
    "[Console]::Out.WriteLine($dialog.FileName)",
    "}"
  )
  run_windows_dialog_script(script)
}

choose_windows_directory <- function(caption) {
  if (.Platform$OS.type != "windows") {
    return(character(0))
  }
  script <- paste(
    "Add-Type -AssemblyName System.Windows.Forms;",
    "$dialog = New-Object System.Windows.Forms.OpenFileDialog;",
    "$dialog.Title =", ps_quote(caption), ";",
    "$dialog.CheckFileExists = $false;",
    "$dialog.CheckPathExists = $true;",
    "$dialog.ValidateNames = $false;",
    "$dialog.FileName = 'Select this folder';",
    "if ($dialog.ShowDialog() -eq [System.Windows.Forms.DialogResult]::OK) {",
    "[Console]::Out.WriteLine([System.IO.Path]::GetDirectoryName($dialog.FileName))",
    "}"
  )
  run_windows_dialog_script(script)
}

choose_tk_save_file <- function(default_name, title, extension, filetypes) {
  if (requireNamespace("tcltk", quietly = TRUE)) {
    path <- tryCatch(
      as.character(tcltk::tkgetSaveFile(
        initialfile = default_name,
        defaultextension = extension,
        filetypes = filetypes,
        title = title
      )),
      error = function(e) character(0)
    )
    if (length(path) > 0 && nzchar(path[[1]])) {
      return(path[[1]])
    }
  }
  character(0)
}

choose_rstudio_save_file <- function(default_name, caption, filter) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && isTRUE(rstudioapi::isAvailable())) {
    path <- tryCatch(
      rstudioapi::selectFile(
        caption = caption,
        label = "Save",
        path = file.path(getwd(), default_name),
        filter = filter,
        existing = FALSE
      ),
      error = function(e) character(0)
    )
    if (length(path) > 0 && nzchar(path[[1]])) {
      return(path[[1]])
    }
  }
  character(0)
}

choose_rstudio_directory <- function(caption) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && isTRUE(rstudioapi::isAvailable())) {
    path <- tryCatch(
      rstudioapi::selectDirectory(caption = caption, label = "Select", path = getwd()),
      error = function(e) character(0)
    )
    if (length(path) > 0 && nzchar(path[[1]])) {
      return(path[[1]])
    }
  }
  character(0)
}

safe_file_stem <- function(name) {
  name <- gsub("[\\\\/:*?\"<>|]", "_", as.character(name %||% "variable"))
  name <- trimws(gsub("\\s+", " ", name))
  if (!nzchar(name)) "variable" else name
}

save_plot_png_file <- function(plot_function, result, file, dpi = 600, width = 4.375, height = 4.375) {
  grDevices::png(file, width = width, height = height, units = "in", res = dpi)
  closed <- FALSE
  on.exit({
    if (!closed) {
      grDevices::dev.off()
    }
  }, add = TRUE)
  plot_function(result)
  grDevices::dev.off()
  closed <- TRUE
}

tags_to_html <- function(content) {
  paste(htmltools::renderTags(content)$html, collapse = "\n")
}

plot_data_uri <- function(plot_function, result, width = 420, height = 420, res = 96) {
  path <- tempfile("easyflow_plot_", fileext = ".png")
  grDevices::png(path, width = width, height = height, res = res)
  closed <- FALSE
  on.exit({
    if (!closed) {
      grDevices::dev.off()
    }
    unlink(path)
  }, add = TRUE)
  plot_function(result)
  grDevices::dev.off()
  closed <- TRUE
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

choose_excel_save_path <- function() {
  default_name <- sprintf("EasyFlow_Statistics_Results_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- "Save EasyFlow Statistics Results"
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title)
    if (length(path) > 0 && nzchar(path[[1]])) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".xlsx", "{{Excel Workbook} {.xlsx}} {{All Files} {*}}")
    if (length(path) > 0 && nzchar(path[[1]])) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    filters <- matrix(c("Excel Workbook", "*.xlsx", "All Files", "*.*"), ncol = 2, byrow = TRUE)
    path <- utils::choose.files(default = default_name, caption = title, multi = FALSE, filters = filters, index = 1)
    if (length(path) > 0 && nzchar(path[[1]])) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, "Excel Workbook (*.xlsx)")
  if (length(path) > 0 && nzchar(path[[1]])) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".xlsx", "{{Excel Workbook} {.xlsx}} {{All Files} {*}}")
}

choose_figure_save_dir <- function() {
  caption <- "Choose folder for EasyFlow Statistics figures"
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_directory(caption)
    if (length(path) > 0 && nzchar(path[[1]]) && dir.exists(path[[1]])) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- utils::choose.dir(default = getwd(), caption = caption)
    if (length(path) > 0 && nzchar(path[[1]]) && dir.exists(path[[1]])) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_directory(caption)
  if (length(path) > 0 && nzchar(path[[1]]) && dir.exists(path[[1]])) {
    return(path[[1]])
  }
  if (requireNamespace("tcltk", quietly = TRUE)) {
    path <- tryCatch(
      as.character(tcltk::tk_choose.dir(default = getwd(), caption = caption)),
      error = function(e) character(0)
    )
    if (length(path) > 0 && nzchar(path[[1]]) && dir.exists(path[[1]])) {
      return(path[[1]])
    }
  }
  character(0)
}

save_analysis_figure_files <- function(results, directory, variable_table = NULL, labels = character(0)) {
  saved <- character(0)
  for (result in results) {
    dependent <- all.vars(result$formula)[[1]]
    dependent_label <- safe_file_stem(display_variable_name_static(dependent, variable_table, labels, label_only = TRUE))
    qq_file <- file.path(directory, sprintf("qqplot(%s).png", dependent_label))
    residual_file <- file.path(directory, sprintf("residual(%s).png", dependent_label))
    save_plot_png_file(plot_residual_qq, result, qq_file, dpi = 600)
    save_plot_png_file(plot_residual_homoscedasticity, result, residual_file, dpi = 600)
    saved <- c(saved, qq_file, residual_file)
  }
  saved
}

save_analysis_figures_to_dir <- function(results, directory, variable_table = NULL, labels = character(0)) {
  save_analysis_figure_files(results, directory, variable_table, labels)
}

