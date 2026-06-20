# File dialog helpers for data and settings files.

open_dialog_cancel_marker <- "__EASYFLOW_OPEN_DIALOG_CANCEL__"

open_dialog_ps_quote <- function(value) {
  paste0("'", gsub("'", "''", enc2utf8(as.character(value %||% "")), fixed = TRUE), "'")
}

is_open_dialog_path <- function(path) {
  length(path) > 0 && !is.na(path[[1]]) && nzchar(path[[1]])
}

windows_filter_string <- function(filters) {
  if (is.null(filters) || !is.matrix(filters) || ncol(filters) != 2 || nrow(filters) == 0) {
    return("")
  }
  entries <- apply(filters, 1, function(row) {
    label <- as.character(row[[1]] %||% "")
    pattern <- as.character(row[[2]] %||% "")
    if (!nzchar(label) || !nzchar(pattern)) {
      return("")
    }
    paste0(label, " (", pattern, ")|", pattern)
  })
  paste(entries[nzchar(entries)], collapse = "|")
}

run_windows_open_dialog_script <- function(script) {
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

topmost_tk_parent <- function() {
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    return(NULL)
  }

  parent <- tcltk::tktoplevel()
  try(tcltk::tkwm.withdraw(parent), silent = TRUE)
  try(tcltk::tcl("wm", "attributes", parent, "-topmost", 1), silent = TRUE)
  try(tcltk::tcl("wm", "attributes", parent, "-toolwindow", 1), silent = TRUE)
  try(tcltk::tkfocus(parent), silent = TRUE)
  parent
}

windows_open_file_dialog <- function(title, filters) {
  if (!identical(.Platform$OS.type, "windows")) {
    return(list(attempted = FALSE, path = NULL))
  }
  filter <- windows_filter_string(filters)
  if (!nzchar(filter)) {
    return(list(attempted = FALSE, path = NULL))
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
    "$dialog.Title =", open_dialog_ps_quote(title), ";",
    "$dialog.Filter =", open_dialog_ps_quote(filter), ";",
    "$dialog.FilterIndex = 1;",
    "$dialog.Multiselect = $false;",
    "$dialog.CheckFileExists = $true;",
    "$dialog.CheckPathExists = $true;",
    "$owner.Show();",
    "$owner.Activate();",
    "if ($dialog.ShowDialog($owner) -eq [System.Windows.Forms.DialogResult]::OK) {",
    "[Console]::Out.WriteLine($dialog.FileName)",
    "} else {",
    "[Console]::Out.WriteLine(", open_dialog_ps_quote(open_dialog_cancel_marker), ")",
    "}",
    "$dialog.Dispose();",
    "$owner.Close();",
    "$owner.Dispose();"
  )
  output <- run_windows_open_dialog_script(script)
  if (!is_open_dialog_path(output)) {
    return(list(attempted = FALSE, path = NULL))
  }
  if (identical(output[[1]], open_dialog_cancel_marker)) {
    return(list(attempted = TRUE, path = NULL))
  }
  list(attempted = TRUE, path = output[[1]])
}

windows_choose_file_dialog <- function(title, filters) {
  if (!identical(.Platform$OS.type, "windows") || !exists("choose.files", envir = asNamespace("utils"))) {
    return(list(attempted = FALSE, path = NULL))
  }
  path <- tryCatch(
    utils::choose.files(caption = title, multi = FALSE, filters = filters, index = 1),
    error = function(e) {
      message("Windows choose.files dialog failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(path)) {
    return(list(attempted = FALSE, path = NULL))
  }
  if (!is_open_dialog_path(path)) {
    return(list(attempted = TRUE, path = NULL))
  }
  list(attempted = TRUE, path = path[[1]])
}

open_file_dialog <- function(title, filetypes) {
  windows_filters <- attr(filetypes, "windows_filters", exact = TRUE)
  if (!is.null(windows_filters)) {
    windows_result <- windows_open_file_dialog(title, windows_filters)
    if (isTRUE(windows_result$attempted)) {
      windows_path <- windows_result$path
      if (!is.null(windows_path) && nzchar(windows_path) && !dir.exists(windows_path)) {
        return(windows_path)
      }
      return(NULL)
    }

    choose_result <- windows_choose_file_dialog(title, windows_filters)
    if (isTRUE(choose_result$attempted)) {
      choose_path <- choose_result$path
      if (!is.null(choose_path) && nzchar(choose_path) && !dir.exists(choose_path)) {
        return(choose_path)
      }
      return(NULL)
    }
  }

  path <- tryCatch(
    {
      if (requireNamespace("tcltk", quietly = TRUE)) {
        parent <- topmost_tk_parent()
        on.exit(try(tcltk::tkdestroy(parent), silent = TRUE), add = TRUE)
        as.character(tcltk::tkgetOpenFile(parent = parent, title = title, filetypes = filetypes))
      } else {
        file.choose()
      }
    },
    error = function(e) character(0)
  )

  if (length(path) == 0 || !nzchar(path[[1]])) {
    return(NULL)
  }
  path <- path[[1]]
  if (dir.exists(path)) {
    return(NULL)
  }
  path
}

open_settings_file <- function() {
  filetypes <- "{{StatEdu Studio Settings} {.efs-settings}} {{JSON settings} {.json}} {{All files} *}"
  attr(filetypes, "windows_filters") <- matrix(
    c(
      "StatEdu Studio Settings", "*.efs-settings",
      "JSON settings", "*.json",
      "All files", "*.*"
    ),
    ncol = 2,
    byrow = TRUE
  )
  open_file_dialog("Open StatEdu Studio Settings", filetypes)
}

open_data_file <- function() {
  filetypes <- "{{Data files} {.sav .sas7bdat .xpt .dta .xlsx .xls .csv .dat}} {{SPSS SAV} {.sav}} {{SAS} {.sas7bdat .xpt}} {{Stata} {.dta}} {{Excel} {.xlsx .xls}} {{CSV} {.csv}} {{DAT} {.dat}} {{All files} *}"
  attr(filetypes, "windows_filters") <- matrix(
    c(
      "Data files", "*.sav;*.sas7bdat;*.xpt;*.dta;*.xlsx;*.xls;*.csv;*.dat",
      "SPSS SAV", "*.sav",
      "SAS", "*.sas7bdat;*.xpt",
      "Stata", "*.dta",
      "Excel", "*.xlsx;*.xls",
      "CSV", "*.csv",
      "DAT", "*.dat",
      "All files", "*.*"
    ),
    ncol = 2,
    byrow = TRUE
  )
  path <- open_file_dialog(
    "Open StatEdu Studio Data",
    filetypes
  )
  if (is.null(path) || !supported_data_file_extension(path)) {
    return(NULL)
  }
  path
}

save_settings_file <- function() {
  path <- tryCatch(
    {
      if (requireNamespace("tcltk", quietly = TRUE)) {
        parent <- topmost_tk_parent()
        on.exit(try(tcltk::tkdestroy(parent), silent = TRUE), add = TRUE)
        as.character(tcltk::tkgetSaveFile(
          parent = parent,
          title = "Save StatEdu Studio Settings",
          initialfile = "EFS_settings.efs-settings",
          defaultextension = ".efs-settings",
          filetypes = "{{StatEdu Studio Settings} {.efs-settings}} {{JSON settings} {.json}} {{All files} *}"
        ))
      } else {
        folder <- utils::choose.dir(caption = "Choose a folder for StatEdu Studio Settings")
        if (is.na(folder) || !nzchar(folder)) {
          character(0)
        } else {
          file.path(folder, "EFS_settings.efs-settings")
        }
      }
    },
    error = function(e) character(0)
  )

  if (length(path) == 0 || !nzchar(path[[1]])) {
    return(NULL)
  }

  path <- path[[1]]
  if (!nzchar(tools::file_ext(path))) {
    path <- paste0(path, ".efs-settings")
  }
  path
}
