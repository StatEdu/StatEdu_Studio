# File dialog and figure export helpers for analysis results.
data_file_source_directory <- function(file) {
  directory <- file$source_directory %||% ""
  if (nzchar(directory) && !data_path_is_session_temporary(directory) && dir.exists(directory)) return(normalizePath(directory, winslash = "/"))
  candidates <- c(file$original_path %||% "", file$path %||% "")
  for (path in candidates) {
    if (is.na(path) || !nzchar(path)) next
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    # Browser upload copies are not the user's source folder.
    if (data_path_is_session_temporary(path)) next
    directory <- dirname(path)
    if (dir.exists(directory)) return(directory)
  }
  ""
}

result_save_directory <- function(session = shiny::getDefaultReactiveDomain()) {
  file <- if (!is.null(session) && is.function(session$userData$result_data_file))
    shiny::isolate(session$userData$result_data_file()) else NULL
  directory <- data_file_source_directory(file)
  if (nzchar(directory)) return(directory)
  configured <- statedu_read_persisted_preferences()$default_save_dir %||% ""
  if (nzchar(configured) && !data_path_is_session_temporary(configured) && dir.exists(configured)) return(normalizePath(configured,winslash="/"))
  getwd()
}

save_canvas_figure_snapshots <- function(files, directory, prefix = "mediation_moderation") {
  if (length(prefix) != 1L || !grepl("^[A-Za-z0-9_-]+$", prefix)) stop("Invalid figure folder prefix.")
  if (!length(files)) stop("No figure snapshots are available.")
  decoded <- lapply(seq_along(files), function(index) {
    item <- files[[index]]
    name <- as.character(item$name %||% "")
    if (length(name) != 1L || !grepl("^[A-Za-z0-9_-]+[.]png$", name)) stop("Invalid figure filename.")
    data <- as.character(item$data %||% "")
    if (length(data) != 1L || !startsWith(data, "data:image/png;base64,")) stop("Invalid PNG snapshot.")
    # Browser image URLs can percent-encode the line breaks from base64_enc.
    # substring() defaults to last = 1e6, truncating high-resolution PNGs.
    encoded <- substring(data, nchar("data:image/png;base64,") + 1L, nchar(data))
    # Decode escapes in one pass; URLdecode repeatedly copies multi-megabyte plots.
    escapes <- gregexpr("%[0-9A-Fa-f]{2}", encoded, perl = TRUE)
    matches <- regmatches(encoded, escapes)[[1]]
    if (length(matches)) {
      codes <- strtoi(substring(matches, 2L), base = 16L)
      if (any(codes == 0L | codes > 127L)) stop(sprintf("Invalid PNG snapshot: %s", name))
      regmatches(encoded, escapes) <- list(intToUtf8(codes, multiple = TRUE))
    }
    encoded <- gsub("[\t\r\n\f ]", "", encoded)
    bytes <- tryCatch(base64enc::base64decode(encoded), error = function(error) {
      stop(sprintf("Could not decode PNG snapshot: %s", name))
    })
    # Compare canonical encodings instead of rejecting valid unpadded base64.
    canonical <- base64enc::base64encode(bytes)
    if (!identical(sub("=+$", "", encoded), sub("=+$", "", canonical))) {
      stop(sprintf("Invalid PNG snapshot: %s (encoded length %d)", name, nchar(encoded)))
    }
    if (length(bytes) < 8L || !identical(bytes[1:8], as.raw(c(137,80,78,71,13,10,26,10)))) stop("Invalid PNG image.")
    list(name = name, bytes = bytes)
  })
  folder <- file.path(directory, paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  base <- folder; suffix <- 1L
  while (file.exists(folder)) {folder <- paste0(base, "_", suffix);suffix <- suffix + 1L}
  if (!dir.create(folder, recursive = TRUE)) stop("Could not create the figure folder.")
  vapply(seq_along(decoded), function(index) {
    item <- decoded[[index]]
    file <- file.path(folder, sprintf("%02d_%s", index, item$name))
    writeBin(item$bytes, file)
    file
  }, character(1))
}

# All analysis figure exports should use these helpers so folder selection,
# file naming, and PNG resolution stay consistent across modules.

analysis_figure_dpi <- function(
  edition = analysis_save_edition(),
  public_release = statedu_public_release(),
  requested_dpi = NULL
) {
  edition <- tolower(as.character(edition %||% "development")[[1]])
  if (edition %in% c("development", "pro")) 600L else 300L
}

analysis_figure_width <- 4.375
analysis_figure_height <- 4.375

plot_png_file <- function(plot_function, result, dpi = analysis_figure_dpi(), width = analysis_figure_width, height = analysis_figure_height) {
  path <- tempfile("statedu_plot_", fileext = ".png")
  grDevices::png(path, width = width, height = height, units = "in", res = dpi, bg = "transparent")
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

windows_dialog_cancel_marker <- "__STATEDU_DIALOG_CANCEL__"

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
    "$dialog.InitialDirectory =", ps_quote(result_save_directory()), ";",
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

choose_windows_open_file <- function(
  title,
  filter = "StatEdu Studio Result (*.efs-result)|*.efs-result|JSON File (*.json)|*.json|All Files (*.*)|*.*"
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
    "$dialog = New-Object System.Windows.Forms.OpenFileDialog;",
    "$dialog.Title =", ps_quote(title), ";",
    "$dialog.Filter =", ps_quote(filter), ";",
    "$dialog.FilterIndex = 1;",
    "$dialog.Multiselect = $false;",
    "$dialog.CheckFileExists = $true;",
    "$dialog.CheckPathExists = $true;",
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
    "$dialog = New-Object System.Windows.Forms.FolderBrowserDialog;",
    "$dialog.Description =", ps_quote(caption), ";",
    "$dialog.ShowNewFolderButton = $true;",
    "$dialog.SelectedPath =", ps_quote(normalizePath(result_save_directory(), winslash = "\\", mustWork = FALSE)), ";",
    "$owner.Show();",
    "$owner.Activate();",
    "if ($dialog.ShowDialog($owner) -eq [System.Windows.Forms.DialogResult]::OK) {",
    "[Console]::Out.WriteLine($dialog.SelectedPath)",
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
        initialdir = result_save_directory(),
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

choose_tk_open_file <- function(title, filetypes) {
  if (requireNamespace("tcltk", quietly = TRUE)) {
    path <- tryCatch(
      as.character(tcltk::tkgetOpenFile(
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
        path = file.path(result_save_directory(), default_name),
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

choose_rstudio_open_file <- function(caption, filter) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && isTRUE(rstudioapi::isAvailable())) {
    path <- tryCatch(
      rstudioapi::selectFile(
        caption = caption,
        label = "Open",
        path = getwd(),
        filter = filter,
        existing = TRUE
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
      rstudioapi::selectDirectory(caption = caption, label = "Select", path = result_save_directory()),
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

save_plot_png_file <- function(plot_function, result, file, dpi = analysis_figure_dpi(), width = analysis_figure_width, height = analysis_figure_height) {
  grDevices::png(file, width = width, height = height, units = "in", res = dpi, bg = "transparent")
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
  export_res <- max(res, analysis_figure_dpi())
  width <- round(width * export_res / res)
  height <- round(height * export_res / res)
  res <- export_res
  path <- tempfile("statedu_plot_", fileext = ".png")
  grDevices::png(path, width = width, height = height, res = res, bg = "transparent")
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

choose_excel_save_path <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  if (!exists("analysis_save_feature_enabled", mode = "function", inherits = TRUE) ||
      !isTRUE(analysis_save_feature_enabled("excel"))) {
    return(character(0))
  }
  default_name <- sprintf("StatEdu_Studio_results_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- sprintf(statedu_t("file_dialog.save_results_format", language), "Excel")
  file_label <- sprintf(statedu_t("file_dialog.format_file", language), "Excel")
  all_label <- statedu_t("file_dialog.all_files", language)
  tk_types <- sprintf("{{%s} {.xlsx}} {{%s} {*}}", file_label, all_label)
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, sprintf("%s (*.xlsx)|*.xlsx|%s (*.*)|*.*", file_label, all_label), "xlsx")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".xlsx", tk_types)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    filters <- matrix(c(file_label, "*.xlsx", all_label, "*.*"), ncol = 2, byrow = TRUE)
    path <- utils::choose.files(default = file.path(result_save_directory(), default_name), caption = title, multi = FALSE, filters = filters, index = 1)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, sprintf("%s (*.xlsx)", file_label))
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".xlsx", tk_types)
}

choose_data_csv_save_path <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  default_name <- sprintf("StatEdu_Studio_data_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- statedu_t("file_dialog.save_data", language)
  csv_label <- statedu_t("file_dialog.csv_file", language)
  all_label <- statedu_t("file_dialog.all_files", language)
  tk_types <- sprintf("{{%s} {.csv}} {{%s} {*}}", csv_label, all_label)
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, sprintf("%s (*.csv)|*.csv|%s (*.*)|*.*", csv_label, all_label), "csv")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".csv", tk_types)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    filters <- matrix(c(csv_label, "*.csv", all_label, "*.*"), ncol = 2, byrow = TRUE)
    path <- utils::choose.files(default = file.path(result_save_directory(), default_name), caption = title, multi = FALSE, filters = filters, index = 1)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, sprintf("%s (*.csv)", csv_label))
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".csv", tk_types)
}

choose_html_save_path <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  default_name <- sprintf("StatEdu_Studio_results_%s.html", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- sprintf(statedu_t("file_dialog.save_results_format", language), "HTML")
  file_label <- sprintf(statedu_t("file_dialog.format_file", language), "HTML")
  all_label <- statedu_t("file_dialog.all_files", language)
  tk_types <- sprintf("{{%s} {.html}} {{%s} {*}}", file_label, all_label)
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, sprintf("%s (*.html)|*.html|%s (*.*)|*.*", file_label, all_label), "html")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".html", tk_types)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    filters <- matrix(c(file_label, "*.html", all_label, "*.*"), ncol = 2, byrow = TRUE)
    path <- utils::choose.files(default = file.path(result_save_directory(), default_name), caption = title, multi = FALSE, filters = filters, index = 1)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, sprintf("%s (*.html)", file_label))
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".html", tk_types)
}

choose_result_history_save_path <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  default_name <- sprintf("StatEdu_Studio_result_history_%s.efs-result", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- statedu_t("file_dialog.save_history", language)
  history_label <- statedu_t("file_dialog.history_file", language)
  json_label <- sprintf(statedu_t("file_dialog.format_file", language), "JSON")
  all_label <- statedu_t("file_dialog.all_files", language)
  tk_types <- sprintf("{{%s} {.efs-result}} {{%s} {.json}} {{%s} {*}}", history_label, json_label, all_label)
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, sprintf("%s (*.efs-result)|*.efs-result|%s (*.json)|*.json|%s (*.*)|*.*", history_label, json_label, all_label), "efs-result")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".efs-result", tk_types)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, sprintf("%s (*.efs-result)", history_label))
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".efs-result", tk_types)
}

choose_result_history_open_path <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  title <- statedu_t("file_dialog.open_history", language)
  history_label <- statedu_t("file_dialog.history_file", language)
  json_label <- sprintf(statedu_t("file_dialog.format_file", language), "JSON")
  all_label <- statedu_t("file_dialog.all_files", language)
  tk_types <- sprintf("{{%s} {.efs-result}} {{%s} {.json}} {{%s} {*}}", history_label, json_label, all_label)
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_open_file(title, sprintf("%s (*.efs-result)|*.efs-result|%s (*.json)|*.json|%s (*.*)|*.*", history_label, json_label, all_label))
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_open_file(title, tk_types)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    filters <- matrix(c(history_label, "*.efs-result", json_label, "*.json", all_label, "*.*"), ncol = 2, byrow = TRUE)
    path <- utils::choose.files(caption = title, multi = FALSE, filters = filters, index = 1)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_open_file(title, sprintf("%s (*.efs-result)", history_label))
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_open_file(title, tk_types)
}

choose_pdf_save_path <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  if (!exists("analysis_save_feature_enabled", mode = "function", inherits = TRUE) ||
      !isTRUE(analysis_save_feature_enabled("pdf"))) {
    return(character(0))
  }
  default_name <- sprintf("StatEdu_Studio_results_%s.pdf", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- sprintf(statedu_t("file_dialog.save_results_format", language), "PDF")
  file_label <- sprintf(statedu_t("file_dialog.format_file", language), "PDF")
  all_label <- statedu_t("file_dialog.all_files", language)
  tk_types <- sprintf("{{%s} {.pdf}} {{%s} {*}}", file_label, all_label)
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, sprintf("%s (*.pdf)|*.pdf|%s (*.*)|*.*", file_label, all_label), "pdf")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".pdf", tk_types)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, sprintf("%s (*.pdf)", file_label))
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".pdf", tk_types)
}

choose_word_save_path <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  if (!exists("analysis_save_feature_enabled", mode = "function", inherits = TRUE) ||
      !isTRUE(analysis_save_feature_enabled("word"))) {
    return(character(0))
  }
  default_name <- sprintf("StatEdu_Studio_results_%s.docx", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- sprintf(statedu_t("file_dialog.save_results_format", language), "Word")
  file_label <- sprintf(statedu_t("file_dialog.format_file", language), "Word")
  all_label <- statedu_t("file_dialog.all_files", language)
  tk_types <- sprintf("{{%s} {.docx}} {{%s} {*}}", file_label, all_label)
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, sprintf("%s (*.docx)|*.docx|%s (*.*)|*.*", file_label, all_label), "docx")
    if (is_windows_dialog_cancel(path)) {
      return(character(0))
    }
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  if (.Platform$OS.type == "windows") {
    path <- choose_tk_save_file(default_name, title, ".docx", tk_types)
    if (is_dialog_path(path)) {
      return(path[[1]])
    }
  }
  path <- choose_rstudio_save_file(default_name, title, sprintf("%s (*.docx)", file_label))
  if (is_dialog_path(path)) {
    return(path[[1]])
  }
  choose_tk_save_file(default_name, title, ".docx", tk_types)
}

choose_hwpx_save_path <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  default_name <- sprintf("StatEdu_Studio_results_%s.hwpx", format(Sys.time(), "%Y%m%d_%H%M%S"))
  title <- sprintf(statedu_t("file_dialog.save_results_format", language), "HWPX")
  file_label <- sprintf(statedu_t("file_dialog.format_file", language), "HWPX")
  all_label <- statedu_t("file_dialog.all_files", language)
  tk_types <- sprintf("{{%s} {.hwpx}} {{%s} {*}}", file_label, all_label)
  if (.Platform$OS.type == "windows") {
    path <- choose_windows_save_file(default_name, title, sprintf("%s (*.hwpx)|*.hwpx|%s (*.*)|*.*", file_label, all_label), "hwpx")
    if (is_windows_dialog_cancel(path)) return(character(0))
    if (is_dialog_path(path)) return(path[[1L]])
  }
  choose_tk_save_file(default_name, title, ".hwpx", tk_types)
}

write_result_collection_hwpx <- function(entries, file, contents = NULL) {
  write_result_collection_hwpx_native(entries,file,contents)
}

find_pdf_chromium <- function() {
  candidates <- c(
    Sys.getenv("STATEDU_CHROME", ""),
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
    stop("Chrome or Edge was not found. Install Chrome/Edge or set STATEDU_CHROME.")
  }
  html_file <- tempfile("statedu_pdf_", fileext = ".html")
  pdf_file <- tempfile("statedu_pdf_", fileext = ".pdf")
  profile_dir <- tempfile("statedu_pdf_profile_")
  writeLines(html, html_file, useBytes = TRUE)
  on.exit(unlink(c(html_file, pdf_file)), add = TRUE)
  on.exit(unlink(profile_dir, recursive = TRUE), add = TRUE)
  args <- c(
    "--headless=new",
    "--disable-gpu",
    "--no-pdf-header-footer",
    "--no-first-run",
    paste0("--user-data-dir=", normalizePath(profile_dir, winslash = "/", mustWork = FALSE)),
    paste0("--print-to-pdf=", normalizePath(pdf_file, winslash = "/", mustWork = FALSE)),
    normalizePath(html_file, winslash = "/", mustWork = TRUE)
  )
  status <- system2(browser, args = vapply(args, shQuote, character(1)), stdout = TRUE, stderr = TRUE)
  if (!identical(as.integer(attr(status, "status") %||% 0L), 0L) ||
      !file.exists(pdf_file) || file.info(pdf_file)$size <= 5 ||
      !identical(rawToChar(readBin(pdf_file, "raw", n = 5L)), "%PDF-")) {
    stop(paste(c("PDF export failed.", status), collapse = "\n"))
  }
  if (!file.copy(pdf_file, file, overwrite = TRUE)) stop("The generated PDF could not be saved to the selected path.")
  invisible(file)
}

choose_figure_save_dir <- function(language = getOption("statedu.app_language", statedu_initial_language())) {
  caption <- statedu_t("file_dialog.figure_folder", language)
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
    path <- utils::choose.dir(default = result_save_directory(), caption = caption)
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
      as.character(tcltk::tk_choose.dir(default = result_save_directory(), caption = caption)),
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

save_ancova_figure_files <- function(result, directory, variable_table = NULL, labels = character(0)) {
  saved <- character(0)
  for (item in result$results %||% list()) {
    options <- item$options %||% list()
    dependent_label <- safe_file_stem(display_variable_name_static(item$dependent, variable_table, labels, label_only = TRUE))
    jobs <- list()
    if (isTRUE(options$plot_adjusted_means)) {
      jobs <- c(jobs, list(list(name = "adjusted_mean", label = "Adjusted mean", fn = draw_ancova_adjusted_mean_plot)))
    }
    if (isTRUE(options$plot_raw_overlay)) {
      jobs <- c(jobs, list(list(name = "raw_overlay", label = "Raw overlay", fn = draw_ancova_raw_overlay_plot)))
    }
    if (isTRUE(options$plot_regression_lines)) {
      jobs <- c(jobs, list(list(name = "regression_lines", label = "Regression lines", fn = draw_ancova_regression_lines_plot)))
    }
    if (isTRUE(options$plot_linearity_diagnostics)) {
      for (covariate in ancova_numeric_covariates(item)) {
        plot_item <- item
        plot_item$linearity_covariate <- covariate
        plot_function <- local({
          local_item <- plot_item
          function(unused) draw_ancova_linearity_diagnostic_plot(local_item)
        })
        jobs <- c(jobs, list(list(
          name = sprintf("linearity_%s", safe_file_stem(covariate)),
          label = sprintf("Linearity diagnostic %s", covariate),
          fn = plot_function
        )))
      }
    }
    for (job in jobs) {
      file <- file.path(directory, sprintf("ANCOVA_%s(%s).png", job$name, dependent_label))
      save_plot_png_file(job$fn, item, file, width = 6.4, height = 4.6)
      saved <- c(saved, file)
    }
  }
  saved
}

save_ancova_figures_to_dir <- function(result, directory, variable_table = NULL, labels = character(0)) {
  save_ancova_figure_files(result, directory, variable_table, labels)
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

save_survival_km_figure_files <- function(result, directory, dpi = analysis_figure_dpi()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for survival figure export.")
  }
  items <- survival_km_result_items(result)
  saved <- character(0)
  for (item_index in seq_along(items)) {
    item <- items[[item_index]]
    group_label <- if (nzchar(as.character(item$group %||% ""))) safe_file_stem(item$group) else "All"
    plot_types <- as.character(item$plot_types %||% "survival")
    plot_versions <- as.character(item$plot_versions %||% "color")
    for (plot_type in plot_types) {
      plot_label <- safe_file_stem(survival_plot_type_label(plot_type, "en"))
      for (plot_version in plot_versions) {
        version_label <- safe_file_stem(survival_plot_version_label(plot_version, "en"))
        file <- file.path(directory, sprintf("Kaplan-Meier_%s_%s_%s_%sdpi.png", group_label, plot_label, version_label, dpi))
        grDevices::png(file, width = 6.4, height = 6.2, units = "in", res = dpi, bg = "transparent")
        tryCatch(
          survival_draw_plot_with_risk_table(survival_km_ggplot(item, plot_type, plot_version), survival_km_risk_table_plot(item, plot_version)),
          finally = grDevices::dev.off()
        )
        saved <- c(saved, file)
      }
    }
  }
  saved
}

save_survival_km_figures_to_dir <- function(result, directory) {
  save_survival_km_figure_files(result, directory)
}

save_survival_cox_figure_files <- function(result, directory, dpi = analysis_figure_dpi(), plot_versions = c("color", "bw")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for Cox figure export.")
  }
  saved <- character(0)
  plot_versions <- unique(as.character(plot_versions %||% "color"))
  plot_versions <- plot_versions[plot_versions %in% c("color", "bw")]
  if (length(plot_versions) == 0) {
    plot_versions <- "color"
  }
  for (plot_version in plot_versions) {
    plot <- survival_cox_ggplot(result, plot_version)
    if (is.null(plot)) next
    version_label <- safe_file_stem(survival_plot_version_label(plot_version, "en"))
    file <- file.path(directory, sprintf("Cox_Hazard_Ratio_Forest_Plot_%s_%sdpi.png", version_label, dpi))
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      width = 6.4,
      height = 4.8,
      units = "in",
      dpi = dpi,
      bg = "transparent"
    )
    saved <- c(saved, file)
  }
  saved
}

save_survival_cox_figures_to_dir <- function(result, directory) {
  save_survival_cox_figure_files(result, directory)
}

save_survival_competing_figure_files <- function(result, directory, dpi = analysis_figure_dpi()) {
  file <- file.path(directory, sprintf("Competing-risks_CIF_number-at-risk_%sdpi.png", dpi))
  grDevices::png(file, width = 6.4, height = 6.2, units = "in", res = dpi, bg = "transparent")
  tryCatch(
    survival_draw_plot_with_risk_table(survival_competing_ggplot(result), survival_competing_risk_table_plot(result)),
    finally = grDevices::dev.off()
  )
  saved <- file
  if (is.list(result$cause_specific) && !is.null(result$cause_specific$ph)) {
    ph_file <- file.path(directory, sprintf("Cause-specific-Cox_Schoenfeld-diagnostics_%sdpi.png", dpi))
    grDevices::png(ph_file, width = 7.2, height = survival_cox_ph_plot_height(result$cause_specific, 6.2 * 110) / 110, units = "in", res = dpi, bg = "transparent")
    tryCatch(
      survival_cox_ph_plot(result$cause_specific),
      finally = grDevices::dev.off()
    )
    saved <- c(saved, ph_file)
  }
  functional_plot <- if (is.list(result$cause_specific)) survival_cox_functional_form_ggplot(result$cause_specific) else NULL
  if (!is.null(functional_plot)) {
    functional_file <- file.path(directory, sprintf("Cause-specific-Cox_Martingale-functional-form_%sdpi.png", dpi))
    ggplot2::ggsave(functional_file, functional_plot, width = 7.2, height = 5.8, units = "in", dpi = dpi, bg = "transparent")
    saved <- c(saved, functional_file)
  }
  residual_plot <- survival_fine_gray_residual_ggplot(result)
  if (!is.null(residual_plot)) {
    residual_file <- file.path(directory, sprintf("Fine-Gray_Schoenfeld-like-residuals_%sdpi.png", dpi))
    ggplot2::ggsave(residual_file, residual_plot, width = 7.2, height = 5.8, units = "in", dpi = dpi, bg = "transparent")
    saved <- c(saved, residual_file)
  }
  saved
}

save_survival_reporting_files <- function(result, directory, language = statedu_initial_language()) {
  if (!dir.exists(directory)) stop("The selected export directory does not exist.")
  bundle <- survival_reporting_bundle(result, language)
  files <- character(0)
  method_file <- file.path(directory, "Survival_methods.txt")
  writeLines(enc2utf8(bundle$method), method_file, useBytes = TRUE)
  files <- c(files, method_file)
  tables <- list(Data_inclusion_audit = bundle$data_flow, Event_code_mapping = bundle$event_map, Excluded_rows_by_reason = bundle$exclusions, Censoring_followup_diagnostics = bundle$followup, Analysis_stability_review = bundle$stability, Reporting_checklist = bundle$checklist, Interpretation_guide = bundle$interpretation)
  if (length(bundle$diagnostics %||% list())) tables <- c(tables, bundle$diagnostics)
  for (name in names(tables)) {
    table <- tables[[name]]
    if (!is.data.frame(table) || !nrow(table)) next
    path <- file.path(directory, paste0(name, ".csv"))
    utils::write.csv(table, path, row.names = FALSE, fileEncoding = "UTF-8")
    files <- c(files, path)
  }
  files
}

# Capture only the displayed diagram for Office, retaining HTML/CSS node placement.
result_diagram_image_uri <- function(diagram) {
  browser <- find_pdf_chromium()
  if (!nzchar(browser)) stop("Chrome or Edge is required to export model diagrams to Office.")
  folder <- tempfile("statedu_diagram_"); dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  source <- file.path(folder, "diagram.html"); target <- file.path(folder, "diagram.png")
  css <- paste(readLines(file.path("www", "style.css"), warn = FALSE), collapse = "\n")
  html <- paste0('<!doctype html><meta charset="utf-8"><style>', css,
    '*{--statedu-result-zoom:1;--statedu-landscape-result-zoom:1;}html,body{margin:0!important;padding:0!important;width:860px!important;height:608px!important;overflow:hidden!important;background:white!important;}',
    '.mm-result-diagram-section{margin:0!important;padding:0!important;border:0!important;width:860px!important;}',
    '.mm-result-diagram-section>h3{display:none!important}.mm-result-diagram-section .mm-diagram-panel{width:860px!important;height:608px!important;margin:0!important;box-sizing:border-box!important;}',
    '</style>', as.character(diagram))
  writeLines(html, source, useBytes = TRUE)
  args <- c('--headless=new', '--disable-gpu', '--no-first-run', '--hide-scrollbars', '--force-device-scale-factor=1',
    '--window-size=860,608', paste0('--user-data-dir=', file.path(folder, 'profile')),
    paste0('--screenshot=', target), source)
  status <- system2(browser, vapply(args, shQuote, character(1)), stdout = TRUE, stderr = TRUE)
  if (!identical(as.integer(attr(status, 'status') %||% 0L), 0L) || !file.exists(target)) stop('Model diagram capture failed.')
  paste0('data:image/png;base64,', jsonlite::base64_enc(readBin(target, 'raw', n = file.info(target)$size)))
}
