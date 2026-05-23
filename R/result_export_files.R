# File dialog and figure export helpers for analysis results.
# All analysis figure exports should use these helpers so folder selection,
# file naming, and PNG resolution stay consistent across modules.

analysis_figure_dpi <- 600
analysis_figure_width <- 4.375
analysis_figure_height <- 4.375

plot_png_file <- function(plot_function, result, dpi = analysis_figure_dpi, width = analysis_figure_width, height = analysis_figure_height) {
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

windows_dialog_cancel_marker <- "__EASYFLOW_DIALOG_CANCEL__"

is_windows_dialog_cancel <- function(path) {
  length(path) > 0 && identical(path[[1]], windows_dialog_cancel_marker)
}

is_dialog_path <- function(path) {
  length(path) > 0 && !is.na(path[[1]]) && nzchar(path[[1]])
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

choose_windows_save_file <- function(
  default_name,
  title,
  filter = "Excel Workbook (*.xlsx)|*.xlsx|All Files (*.*)|*.*",
  default_ext = "xlsx"
) {
  if (.Platform$OS.type != "windows") {
    return(character(0))
  }
  script <- paste(
    "Add-Type -AssemblyName System.Windows.Forms;",
    "Add-Type -AssemblyName System.Drawing;",
    "$owner = New-Object System.Windows.Forms.Form;",
    "$owner.TopMost = $true;",
    "$owner.ShowInTaskbar = $false;",
    "$owner.StartPosition = 'CenterScreen';",
    "$owner.Size = New-Object System.Drawing.Size(1,1);",
    "$owner.Opacity = 0;",
    "$dialog = New-Object System.Windows.Forms.SaveFileDialog;",
    "$dialog.Title =", ps_quote(title), ";",
    "$dialog.FileName =", ps_quote(default_name), ";",
    "$dialog.Filter =", ps_quote(filter), ";",
    "$dialog.DefaultExt =", ps_quote(default_ext), ";",
    "$dialog.AddExtension = $true;",
    "$dialog.OverwritePrompt = $true;",
    "$owner.Show();",
    "$owner.Activate();",
    "if ($dialog.ShowDialog($owner) -eq [System.Windows.Forms.DialogResult]::OK) {",
    "[Console]::Out.WriteLine($dialog.FileName)",
    "} else {",
    "[Console]::Out.WriteLine(", ps_quote(windows_dialog_cancel_marker), ")",
    "}",
    "$dialog.Dispose();",
    "$owner.Close();",
    "$owner.Dispose();"
  )
  run_windows_dialog_script(script)
}

choose_windows_directory <- function(caption) {
  if (.Platform$OS.type != "windows") {
    return(character(0))
  }
  script <- paste(
    "Add-Type -AssemblyName System.Windows.Forms;",
    "Add-Type -AssemblyName System.Drawing;",
    "$owner = New-Object System.Windows.Forms.Form;",
    "$owner.TopMost = $true;",
    "$owner.ShowInTaskbar = $false;",
    "$owner.StartPosition = 'CenterScreen';",
    "$owner.Size = New-Object System.Drawing.Size(1,1);",
    "$owner.Opacity = 0;",
    "$dialog = New-Object System.Windows.Forms.OpenFileDialog;",
    "$dialog.Title =", ps_quote(caption), ";",
    "$dialog.CheckFileExists = $false;",
    "$dialog.CheckPathExists = $true;",
    "$dialog.ValidateNames = $false;",
    "$dialog.FileName = 'Select this folder';",
    "$owner.Show();",
    "$owner.Activate();",
    "if ($dialog.ShowDialog($owner) -eq [System.Windows.Forms.DialogResult]::OK) {",
    "[Console]::Out.WriteLine([System.IO.Path]::GetDirectoryName($dialog.FileName))",
    "} else {",
    "[Console]::Out.WriteLine(", ps_quote(windows_dialog_cancel_marker), ")",
    "}",
    "$dialog.Dispose();",
    "$owner.Close();",
    "$owner.Dispose();"
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
    if (is_dialog_path(path)) {
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
    if (is_dialog_path(path)) {
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
    if (is_dialog_path(path)) {
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

save_plot_png_file <- function(plot_function, result, file, dpi = analysis_figure_dpi, width = analysis_figure_width, height = analysis_figure_height) {
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
  rendered <- htmltools::renderTags(content)
  html <- paste(rendered$html, collapse = "\n")
  head <- paste(rendered$head, collapse = "\n")

  if (nzchar(head)) {
    html_tag <- regexpr("<html[^>]*>", html, perl = TRUE)
    if (html_tag[[1]] != -1) {
      insert_at <- html_tag[[1]] + attr(html_tag, "match.length") - 1
      html <- paste0(
        substr(html, 1, insert_at),
        "\n  <head>\n",
        head,
        "\n  </head>",
        substr(html, insert_at + 1, nchar(html))
      )
    } else {
      html <- paste(head, html, sep = "\n")
    }
  }

  html
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
  default_name <- sprintf("easyflow_statistics_results_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- "Save easyflow_statistics Results"
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title)
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".xlsx", "{{Excel Workbook} {.xlsx}} {{All Files} {*}}")
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    filters <- matrix(c("Excel Workbook", "*.xlsx", "All Files", "*.*"), ncol = 2, byrow = TRUE)
    path <- utils::choose.files(default = default_name, caption = title, multi = FALSE, filters = filters, index = 1)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, "Excel Workbook (*.xlsx)")
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".xlsx", "{{Excel Workbook} {.xlsx}} {{All Files} {*}}")
}

choose_data_csv_save_path <- function() {
  default_name <- sprintf("easyflow_statistics_data_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- "Save EasyFlow Statistics Data"
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, "CSV File (*.csv)|*.csv|All Files (*.*)|*.*", "csv")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".csv", "{{CSV File} {.csv}} {{All Files} {*}}")
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    filters <- matrix(c("CSV File", "*.csv", "All Files", "*.*"), ncol = 2, byrow = TRUE)
    path <- utils::choose.files(default = default_name, caption = title, multi = FALSE, filters = filters, index = 1)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, "CSV File (*.csv)")
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".csv", "{{CSV File} {.csv}} {{All Files} {*}}")
}

choose_html_save_path <- function() {
  default_name <- sprintf("easyflow_statistics_results_%s.html", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- "Save easyflow_statistics HTML Results"
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, "HTML File (*.html)|*.html|All Files (*.*)|*.*", "html")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".html", "{{HTML File} {.html}} {{All Files} {*}}")
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    filters <- matrix(c("HTML File", "*.html", "All Files", "*.*"), ncol = 2, byrow = TRUE)
    path <- utils::choose.files(default = default_name, caption = title, multi = FALSE, filters = filters, index = 1)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, "HTML File (*.html)")
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".html", "{{HTML File} {.html}} {{All Files} {*}}")
}

choose_pdf_save_path <- function() {
  default_name <- sprintf("easyflow_statistics_results_%s.pdf", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- "Save EasyFlow Statistics PDF Results"
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, "PDF File (*.pdf)|*.pdf|All Files (*.*)|*.*", "pdf")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".pdf", "{{PDF File} {.pdf}} {{All Files} {*}}")
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, "PDF File (*.pdf)")
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".pdf", "{{PDF File} {.pdf}} {{All Files} {*}}")
}

choose_word_save_path <- function() {
  default_name <- sprintf("easyflow_statistics_results_%s.docx", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- "Save EasyFlow Statistics Word Results"
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, "Word Document (*.docx)|*.docx|All Files (*.*)|*.*", "docx")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".docx", "{{Word Document} {.docx}} {{All Files} {*}}")
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, "Word Document (*.docx)")
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".docx", "{{Word Document} {.docx}} {{All Files} {*}}")
}

find_pdf_chromium <- function() {
  candidates <- c(
    Sys.getenv("EASYFLOW_CHROME", ""),
    Sys.which("chrome"),
    Sys.which("google-chrome"),
    Sys.which("chromium"),
    Sys.which("msedge"),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) "" else candidates[[1]]
}

write_pdf_from_html <- function(html, file) {
  browser <- find_pdf_chromium()
  if (!nzchar(browser)) {
    stop("Chrome or Edge was not found. Install Chrome/Edge or set EASYFLOW_CHROME.")
  }
  html_file <- tempfile("easyflow_pdf_", fileext = ".html")
  writeLines(html, html_file, useBytes = TRUE)
  on.exit(unlink(html_file), add = TRUE)
  args <- c(
    "--headless=new",
    "--disable-gpu",
    "--no-pdf-header-footer",
    paste0("--print-to-pdf=", normalizePath(file, winslash = "/", mustWork = FALSE)),
    normalizePath(html_file, winslash = "/", mustWork = TRUE)
  )
  status <- system2(browser, args = args, stdout = TRUE, stderr = TRUE)
  if (!file.exists(file) || file.info(file)$size <= 0) {
    stop(paste(c("PDF export failed.", status), collapse = "\n"))
  }
  invisible(file)
}

choose_figure_save_dir <- function() {
  caption <- "Choose folder for easyflow_statistics figures"
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_directory(caption)
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path) && dir.exists(path[[1]])) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- utils::choose.dir(default = getwd(), caption = caption)
    if (is_dialog_path(path) && dir.exists(path[[1]])) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_directory(caption)
  if (is_dialog_path(path) && dir.exists(path[[1]])) {
    return(path[[1]])
  }
  if (requireNamespace("tcltk", quietly = TRUE)) {
    path <- tryCatch(
      as.character(tcltk::tk_choose.dir(default = getwd(), caption = caption)),
      error = function(e) character(0)
    )
    if (is_dialog_path(path) && dir.exists(path[[1]])) {
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
    save_plot_png_file(plot_residual_qq, result, qq_file)
    save_plot_png_file(plot_residual_homoscedasticity, result, residual_file)
    saved <- c(saved, qq_file, residual_file)
  }
  saved
}

save_analysis_figures_to_dir <- function(results, directory, variable_table = NULL, labels = character(0)) {
  save_analysis_figure_files(results, directory, variable_table, labels)
}

save_hierarchical_figure_files <- function(results, directory, variable_table = NULL, labels = character(0)) {
  saved <- character(0)
  for (result in results) {
    dependent <- all.vars(result$formula)[[1]]
    dependent_label <- safe_file_stem(display_variable_name_static(dependent, variable_table, labels, label_only = TRUE))
    step_label <- safe_file_stem(result$hierarchical_step %||% sprintf("Model %s", result$hierarchical_step_index %||% ""))
    suffix <- sprintf("%s_%s", dependent_label, step_label)
    qq_file <- file.path(directory, sprintf("qqplot(%s).png", suffix))
    residual_file <- file.path(directory, sprintf("residual(%s).png", suffix))
    save_plot_png_file(plot_residual_qq, result, qq_file)
    save_plot_png_file(plot_residual_homoscedasticity, result, residual_file)
    saved <- c(saved, qq_file, residual_file)
  }
  saved
}

save_hierarchical_figures_to_dir <- function(results, directory, variable_table = NULL, labels = character(0)) {
  save_hierarchical_figure_files(results, directory, variable_table, labels)
}

frequency_plot_label <- function(type) {
  switch(
    type,
    pie = "Pie chart",
    bar = "Bar chart",
    histogram = "Histogram",
    box = "Box plot",
    violin = "Violin plot",
    type
  )
}

frequency_plot_counts <- function(result, name) {
  values <- result$data[[name]]
  values_chr <- as.character(values)
  values_chr[is.na(values)] <- "(Missing)"
  counts <- table(values_chr, useNA = "no")
  ordered_values <- frequency_value_order(names(counts))
  counts <- counts[ordered_values]
  names(counts) <- frequency_value_display_labels(name, names(counts), result$category_table)
  counts
}

draw_frequency_plot <- function(result, type, name) {
  label <- frequency_variable_display_name(name, result$variable_info, result$labels, result$category_table)
  if (identical(type, "pie")) {
    counts <- frequency_plot_counts(result, name)
    graphics::par(mar = c(2, 2, 2, 1))
    graphics::pie(counts, main = label, col = grDevices::hcl.colors(length(counts), "Set 3"))
    return(invisible(NULL))
  }
  if (identical(type, "bar")) {
    counts <- frequency_plot_counts(result, name)
    graphics::par(mar = c(5, 4, 2, 1))
    graphics::barplot(
      counts,
      main = label,
      ylab = "n",
      col = "#73a7e8",
      border = "#0f3d56",
      las = 1,
      axes = TRUE,
      xaxs = "i"
    )
    graphics::abline(h = 0, col = "#1f2937", lwd = 1)
    return(invisible(NULL))
  }

  values <- suppressWarnings(as.numeric(result$data[[name]]))
  values <- values[!is.na(values)]
  if (identical(type, "histogram")) {
    graphics::par(mar = c(4, 4, 2, 1))
    graphics::hist(
      values,
      main = label,
      xlab = label,
      ylab = "n",
      col = "#73a7e8",
      border = "#0f3d56"
    )
    graphics::abline(h = 0, col = "#1f2937", lwd = 1)
    return(invisible(NULL))
  }
  if (identical(type, "box")) {
    graphics::par(mar = c(3, 4, 2, 1))
    graphics::boxplot(values, main = label, ylab = label, col = "#bfe3f1", border = "#0f3d56")
    return(invisible(NULL))
  }
  if (identical(type, "violin")) {
    graphics::par(mar = c(3, 4, 2, 1))
    if (length(unique(values)) < 2) {
      graphics::plot.new()
      graphics::title(main = label)
      graphics::text(0.5, 0.5, "Not enough unique values")
      return(invisible(NULL))
    }
    density_values <- stats::density(values, na.rm = TRUE)
    width <- density_values$y / max(density_values$y) * 0.35
    graphics::plot(
      c(0.5, 1.5),
      range(density_values$x),
      type = "n",
      xaxt = "n",
      xlab = "",
      ylab = label,
      main = label
    )
    graphics::polygon(
      c(1 - width, rev(1 + width)),
      c(density_values$x, rev(density_values$x)),
      col = "#9fc7e8",
      border = "#0f3d56"
    )
    graphics::points(rep(1, length(values)), values, pch = 16, cex = 0.45, col = grDevices::adjustcolor("#102a43", alpha.f = 0.4))
    return(invisible(NULL))
  }
  stop("Unknown frequency plot type: ", type)
}

save_frequency_figure_files <- function(result, directory) {
  options <- result$options %||% list()
  saved <- character(0)
  plot_jobs <- list()

  if (isTRUE(options$pie)) {
    plot_jobs <- c(plot_jobs, lapply(as.character(result$categorical %||% character(0)), function(name) list(type = "pie", name = name)))
  }
  if (isTRUE(options$bar)) {
    plot_jobs <- c(plot_jobs, lapply(as.character(result$categorical %||% character(0)), function(name) list(type = "bar", name = name)))
  }
  if (isTRUE(options$histogram)) {
    plot_jobs <- c(plot_jobs, lapply(as.character(result$continuous %||% character(0)), function(name) list(type = "histogram", name = name)))
  }
  if (isTRUE(options$box)) {
    plot_jobs <- c(plot_jobs, lapply(as.character(result$continuous %||% character(0)), function(name) list(type = "box", name = name)))
  }
  if (isTRUE(options$violin)) {
    plot_jobs <- c(plot_jobs, lapply(as.character(result$continuous %||% character(0)), function(name) list(type = "violin", name = name)))
  }

  for (job in plot_jobs) {
    variable_label <- frequency_variable_display_name(job$name, result$variable_info, result$labels, result$category_table)
    file <- file.path(directory, sprintf("%s(%s).png", safe_file_stem(frequency_plot_label(job$type)), safe_file_stem(variable_label)))
    local({
      plot_type <- job$type
      plot_name <- job$name
      save_plot_png_file(function(plot_result) draw_frequency_plot(plot_result, plot_type, plot_name), result, file)
    })
    saved <- c(saved, file)
  }

  saved
}

save_frequency_figures_to_dir <- function(result, directory) {
  save_frequency_figure_files(result, directory)
}

