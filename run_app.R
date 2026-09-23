source(file.path("R", "app_bootstrap.R"), local = TRUE)

run_app_log <- function(message) {
  path <- Sys.getenv("STATEDU_STARTUP_LOG", "")
  if (!nzchar(path)) {
    return(invisible(FALSE))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  cat(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"), message, "\n", file = path, append = TRUE)
  invisible(TRUE)
}

run_app_log("run_app.R begin")
no_package_install <- identical(tolower(Sys.getenv("STATEDU_NO_PACKAGE_INSTALL", "false")), "true")
missing_packages <- character(0)

if (!isTRUE(no_package_install)) {
  available_package_names <- .packages(all.available = TRUE)
  missing_packages <- setdiff(required_packages, available_package_names)
}
run_app_log("run_app.R dependency check complete")

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
run_app_log("run_app.R dependencies ready")

port <- suppressWarnings(as.integer(Sys.getenv("STATEDU_PORT", "7894")))
if (is.na(port) || port <= 0) {
  port <- 7894
}

append_statedu_query <- function(url) {
  token <- Sys.getenv("STATEDU_TOKEN", "")
  params <- c(
    if (nzchar(token)) paste0("token=", utils::URLencode(token, reserved = TRUE)) else character(0),
    paste0("t=", as.integer(Sys.time()))
  )
  separator <- if (grepl("\\?", url, fixed = FALSE)) "&" else "?"
  paste0(url, separator, paste(params, collapse = "&"))
}

run_app_log("run_app.R loading Shiny")
shiny::runApp(
  appDir = ".",
  host = "127.0.0.1",
  port = port,
  launch.browser = if (identical(tolower(Sys.getenv("STATEDU_LAUNCH_BROWSER", "true")), "false")) {
    FALSE
  } else {
    function(url) {
      utils::browseURL(append_statedu_query(url))
    }
  }
)
