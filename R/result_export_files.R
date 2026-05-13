# File dialog and figure export helpers for analysis results.

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

