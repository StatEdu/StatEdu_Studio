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

windows_open_file_dialog <- function(title, filters) {
  if (!identical(.Platform$OS.type, "windows")) {
    return(list(attempted = FALSE, path = NULL))
  }
  if (!exists("choose.files", envir = asNamespace("utils"), mode = "function")) {
    return(list(attempted = FALSE, path = NULL))
  }
  output <- tryCatch(
    utils::choose.files(
      default = "",
      caption = title,
      multi = FALSE,
      filters = filters,
      index = 1
    ),
    error = function(e) NULL
  )
  if (is.null(output)) {
    return(list(attempted = FALSE, path = NULL))
  }
  output <- output[nzchar(output)]
  list(attempted = TRUE, path = if (length(output) > 0) output[[1]] else NULL)
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

