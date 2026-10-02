# The launcher creates an isolated request directory with private permissions.
macos_desktop_available <- function() {
  identical(Sys.info()[['sysname']], 'Darwin') && nzchar(Sys.getenv('STATEDU_DESKTOP_REQUEST_DIR', ''))
}

macos_desktop_request <- function(operation, ..., timeout = 300) {
  if (!macos_desktop_available()) stop('The macOS desktop bridge is unavailable.')
  directory <- Sys.getenv('STATEDU_DESKTOP_REQUEST_DIR')
  if (!dir.exists(directory)) stop('The macOS desktop session has ended.')
  id <- gsub('[^a-f0-9]', '', basename(tempfile(pattern = '')))
  request <- file.path(directory, paste0(id, '.request.json'))
  response <- file.path(directory, paste0(id, '.response.json'))
  temporary <- paste0(request, '.tmp')
  on.exit(unlink(c(request, response, temporary)), add = TRUE)
  jsonlite::write_json(c(list(operation = operation), list(...)), temporary, auto_unbox = TRUE)
  if (!file.rename(temporary, request)) stop('Could not submit the macOS desktop request.')
  deadline <- Sys.time() + timeout
  while (!file.exists(response)) {
    if (Sys.time() > deadline || !dir.exists(directory)) stop('The macOS desktop request timed out.')
    Sys.sleep(0.08)
  }
  result <- jsonlite::fromJSON(response)
  if (!is.null(result$error)) stop(result$error, call. = FALSE)
  if (isTRUE(result$canceled)) return(character(0))
  result$filePath
}

macos_dialog_extensions <- function(filetypes) {
  text <- paste(as.character(filetypes), collapse = ' ')
  matches <- regmatches(text, gregexpr('\\.[A-Za-z0-9]+', text))[[1]]
  extensions <- unique(tolower(sub('^\\.', '', matches)))
  if (!length(extensions)) '*' else extensions
}

macos_file_dialog <- function(operation, title, default_path = '', filetypes = '') {
  macos_desktop_request(operation, title = title, defaultPath = default_path,
    extensions = as.list(macos_dialog_extensions(filetypes)))
}
