# File dialog helpers for data and settings files.

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

windows_open_file_dialog <- function(title, filter) {
  if (!identical(.Platform$OS.type, "windows")) {
    return(NULL)
  }
  powershell <- Sys.which("powershell.exe")
  if (!nzchar(powershell)) {
    powershell <- Sys.which("powershell")
  }
  if (!nzchar(powershell)) {
    return(NULL)
  }

  script <- paste(
    "Add-Type -AssemblyName System.Windows.Forms;",
    "Add-Type -AssemblyName System.Drawing;",
    "$form = New-Object System.Windows.Forms.Form;",
    "$form.TopMost = $true;",
    "$form.StartPosition = 'CenterScreen';",
    "$form.Width = 1; $form.Height = 1;",
    "$dialog = New-Object System.Windows.Forms.OpenFileDialog;",
    sprintf("$dialog.Title = %s;", shQuote(title, type = "cmd")),
    sprintf("$dialog.Filter = %s;", shQuote(filter, type = "cmd")),
    "$dialog.RestoreDirectory = $true;",
    "$result = $dialog.ShowDialog($form);",
    "if ($result -eq [System.Windows.Forms.DialogResult]::OK) { [Console]::OutputEncoding = [System.Text.Encoding]::UTF8; Write-Output $dialog.FileName }",
    "$form.Dispose();",
    sep = " "
  )
  output <- tryCatch(
    system2(
      powershell,
      args = c("-NoProfile", "-STA", "-ExecutionPolicy", "Bypass", "-Command", script),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) character(0)
  )
  output <- output[nzchar(output)]
  if (length(output) == 0) {
    return(NULL)
  }
  output[[1]]
}

open_file_dialog <- function(title, filetypes) {
  windows_filter <- attr(filetypes, "windows_filter", exact = TRUE)
  if (!is.null(windows_filter)) {
    windows_path <- windows_open_file_dialog(title, windows_filter)
    if (!is.null(windows_path) && nzchar(windows_path) && !dir.exists(windows_path)) {
      return(windows_path)
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
  open_file_dialog(
    "Open EasyFlow Statistics Settings",
    "{{JSON settings} {.json}} {{All files} *}"
  )
}

open_data_file <- function() {
  filetypes <- "{{Data files} {.sav .sas7bdat .xpt .dta .xlsx .xls .csv .dat}} {{SPSS SAV} {.sav}} {{SAS} {.sas7bdat .xpt}} {{Stata} {.dta}} {{Excel} {.xlsx .xls}} {{CSV} {.csv}} {{DAT} {.dat}} {{All files} *}"
  attr(filetypes, "windows_filter") <- paste(
    "Data files (*.sav;*.sas7bdat;*.xpt;*.dta;*.xlsx;*.xls;*.csv;*.dat)|*.sav;*.sas7bdat;*.xpt;*.dta;*.xlsx;*.xls;*.csv;*.dat",
    "SPSS SAV (*.sav)|*.sav",
    "SAS (*.sas7bdat;*.xpt)|*.sas7bdat;*.xpt",
    "Stata (*.dta)|*.dta",
    "Excel (*.xlsx;*.xls)|*.xlsx;*.xls",
    "CSV (*.csv)|*.csv",
    "DAT (*.dat)|*.dat",
    "All files (*.*)|*.*",
    sep = "|"
  )
  path <- open_file_dialog(
    "Open EasyFlow Statistics Data",
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
          title = "Save EasyFlow Statistics Settings",
          initialfile = "EasyFlow_Statistics_settings.json",
          filetypes = "{{JSON settings} {.json}} {{All files} *}"
        ))
      } else {
        folder <- utils::choose.dir(caption = "Choose a folder for EasyFlow Statistics Settings")
        if (is.na(folder) || !nzchar(folder)) {
          character(0)
        } else {
          file.path(folder, "EasyFlow_Statistics_settings.json")
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
    path <- paste0(path, ".json")
  }
  path
}

