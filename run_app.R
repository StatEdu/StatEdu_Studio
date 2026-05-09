required_packages <- c(
  "shiny",
  "DT",
  "lmtest",
  "sandwich",
  "nortest",
  "boot",
  "readxl",
  "jsonlite",
  "haven",
  "readr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

shiny::runApp(
  appDir = ".",
  host = "127.0.0.1",
  launch.browser = TRUE
)
