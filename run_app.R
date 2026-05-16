required_packages <- c(
  "shiny",
  "DT",
  "lmtest",
  "sandwich",
  "nortest",
  "boot",
  "jsonlite",
  "haven",
  "readr",
  "htmltools",
  "openxlsx",
  "callr",
  "glmnet",
  "agricolae"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

port <- suppressWarnings(as.integer(Sys.getenv("EASYFLOW_PORT", "7894")))
if (is.na(port) || port <= 0) {
  port <- 7894
}

shiny::runApp(
  appDir = ".",
  host = "127.0.0.1",
  port = port,
  launch.browser = function(url) {
    utils::browseURL(sprintf("%s?t=%s", url, as.integer(Sys.time())))
  }
)
