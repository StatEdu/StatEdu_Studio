all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_logistic_ui.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(repo_root)

source("R/app_bootstrap.R")
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

variable_info <- data.frame(
  name = c("change", "satisfaction", "group", "age"),
  measurement = c("binary", "ordered", "category", "continuous"),
  stringsAsFactors = FALSE
)

dependent_candidates <- logistic_dependent_candidates(variable_info$name, variable_info)
stopifnot(identical(dependent_candidates, c("change", "satisfaction", "group")))
stopifnot(identical(logistic_model_family_label("change", variable_info), "Binary logistic regression"))
stopifnot(identical(logistic_model_family_label("satisfaction", variable_info), "Ordinal logistic regression"))
stopifnot(identical(logistic_model_family_label("group", variable_info), "Multinomial logistic regression"))

setup <- logistic_setup_state(
  selected_names = variable_info$name,
  dependents = c("change", "satisfaction"),
  block1 = "age",
  block2 = "satisfaction",
  block3 = "group",
  variable_table = variable_info,
  labels = c(change = "Changed", satisfaction = "Satisfaction"),
  active_block = "block1"
)
stopifnot(identical(setup$dependents, c("change", "satisfaction")))

html <- as.character(htmltools::renderTags(logistic_setup_panel(setup, NULL))$html)
required_fragments <- c(
  "logistic_available",
  "logistic_y",
  "logistic_block1",
  "Dependent variables (2)",
  "B, SE",
  "95% CI (LLCI, ULCI)",
  "McFadden R",
  "Cox &amp; Snell R",
  "Run logistic",
  "logistic_save_control"
)
for (fragment in required_fragments) {
  if (!grepl(fragment, html, fixed = TRUE)) {
    stop(sprintf("Expected logistic UI fragment not found: %s", fragment), call. = FALSE)
  }
}
if (grepl("Dependent Variable (1/1)", html, fixed = TRUE)) {
  stop("Logistic dependent label should not be capped at one variable.", call. = FALSE)
}
for (removed in c("Method is selected by dependent variable type.", "Complete case analysis is used by default.", "Model</div>")) {
  if (grepl(removed, html, fixed = TRUE)) {
    stop(sprintf("Removed logistic UI fragment is still present: %s", removed), call. = FALSE)
  }
}

cat("Logistic UI validation passed.\n")
