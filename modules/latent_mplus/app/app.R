startup_log <- function(message) {
  path <- Sys.getenv("STATEDU_STARTUP_LOG", "")
  if (!nzchar(path)) {
    return(invisible(FALSE))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  cat(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"), message, "\n", file = path, append = TRUE)
  invisible(TRUE)
}

startup_time <- function(label, expr) {
  started <- Sys.time()
  value <- force(expr)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  startup_log(sprintf("%s %.3fs", label, elapsed))
  value
}

startup_log("app.R begin")
startup_time("source app_bootstrap", source(file.path("R", "app_bootstrap.R"), local = FALSE))
startup_time("load packages", load_app_packages())

app_config <- read_app_config()
app_version <- app_config$version

startup_time("source modules", source_app_modules())

ui <- startup_time("build ui", app_ui(app_version))
server <- startup_time("build server", create_app_server(app_version))
startup_log("app.R ready")

shinyApp(ui, server)
