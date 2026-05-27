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
  "markdown",
  "openxlsx",
  "officer",
  "flextable",
  "xml2",
  "rvest",
  "callr",
  "glmnet",
  "agricolae",
  "psych",
  "polycor"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  if (identical(tolower(Sys.getenv("EASYFLOW_NO_PACKAGE_INSTALL", "false")), "true")) {
    stop(
      sprintf("Required R packages are missing from the bundled runtime: %s", paste(missing_packages, collapse = ", ")),
      call. = FALSE
    )
  }
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
  launch.browser = if (identical(tolower(Sys.getenv("EASYFLOW_LAUNCH_BROWSER", "true")), "false")) {
    FALSE
  } else {
    function(url) {
      utils::browseURL(sprintf("%s?t=%s", url, as.integer(Sys.time())))
    }
  }
)
