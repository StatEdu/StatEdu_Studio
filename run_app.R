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
  "readxl",
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
  "polycor",
  "MASS",
  "nnet"
)

no_package_install <- identical(tolower(Sys.getenv("EASYFLOW_NO_PACKAGE_INSTALL", "false")), "true")
missing_packages <- character(0)

if (!isTRUE(no_package_install)) {
  installed_package_names <- rownames(utils::installed.packages())
  missing_packages <- setdiff(required_packages, installed_package_names)
}

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

port <- suppressWarnings(as.integer(Sys.getenv("EASYFLOW_PORT", "7894")))
if (is.na(port) || port <= 0) {
  port <- 7894
}

append_easyflow_query <- function(url) {
  token <- Sys.getenv("EASYFLOW_TOKEN", "")
  params <- c(
    if (nzchar(token)) paste0("token=", utils::URLencode(token, reserved = TRUE)) else character(0),
    paste0("t=", as.integer(Sys.time()))
  )
  separator <- if (grepl("\\?", url, fixed = FALSE)) "&" else "?"
  paste0(url, separator, paste(params, collapse = "&"))
}

shiny::runApp(
  appDir = ".",
  host = "127.0.0.1",
  port = port,
  launch.browser = if (identical(tolower(Sys.getenv("EASYFLOW_LAUNCH_BROWSER", "true")), "false")) {
    FALSE
  } else {
    function(url) {
      utils::browseURL(append_easyflow_query(url))
    }
  }
)
