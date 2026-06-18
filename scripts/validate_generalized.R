source("R/app_bootstrap.R")
invisible(lapply(file.path("R", app_module_files), source, chdir = TRUE))
suppressPackageStartupMessages(library(shiny))

runtime_required <- c("car", "MASS", "sandwich", "lmtest", "mice", "geepack", "lme4", "lmerTest", "plm")
stopifnot(all(runtime_required %in% required_packages))
run_app_text <- paste(readLines("run_app.R", warn = FALSE), collapse = "\n")
stopifnot(grepl('source(file.path("R", "app_bootstrap.R"), local = TRUE)', run_app_text, fixed = TRUE))

set.seed(1)
data <- data.frame(
  y = rnorm(80),
  x1 = rnorm(80),
  x2 = factor(sample(c("A", "B"), 80, TRUE)),
  exposure = runif(80, 0.5, 2)
)
data$y[c(3, 9)] <- NA
variable_info <- data.frame(
  name = c("y", "x1", "x2", "exposure", "bin", "count"),
  var_label = c("Outcome", "Predictor 1", "Group", "Exposure", "Binary outcome", "Count outcome"),
  role = "",
  measurement = c("continuous", "continuous", "category", "continuous", "binary", "continuous"),
  stringsAsFactors = FALSE
)
category_table <- data.frame(
  name = "x2",
  var_label = "Group",
  reference = "A",
  value_1 = "A",
  label_1 = "Control",
  value_2 = "B",
  label_2 = "Treatment",
  stringsAsFactors = FALSE
)

gaussian <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  robust = TRUE,
  show_vif = TRUE,
  variable_info = variable_info,
  category_table = category_table
)
stopifnot(is.data.frame(gaussian$coef_table))
stopifnot(nrow(gaussian$coef_table) > 0)
stopifnot(identical(gaussian$family, "gaussian"))
stopifnot(is.data.frame(gaussian$missing_table))
stopifnot(gaussian$excluded_n == 2)
stopifnot(any(gaussian$assumption_checks$Check == "Independent observations"))
stopifnot(identical(gaussian$missing_strategy, "complete"))
stopifnot(identical(gaussian$se_type_requested, "HC1"))
stopifnot(any(gaussian$fit_stats$Item == "Standard errors"))
stopifnot(is.data.frame(gaussian$decision_summary))
stopifnot(any(gaussian$decision_summary$Item == "Family selection"))
stopifnot(any(gaussian$decision_summary$Item == "Recommended reporting"))
stopifnot(is.data.frame(gaussian$coding_summary))
stopifnot(any(grepl("reference = Control", gaussian$coding_summary$Coding, fixed = TRUE)))
stopifnot(is.data.frame(gaussian$publication_notes))
stopifnot(nrow(gaussian$publication_notes) > 0)
stopifnot(is.data.frame(gaussian$reporting_checklist))
stopifnot(any(gaussian$reporting_checklist$Item == "Family and link reported"))
stopifnot(is.data.frame(gaussian$manuscript_text))
stopifnot(any(gaussian$manuscript_text$Section == "Methods"))
stopifnot(is.data.frame(gaussian$software_versions))
stopifnot(any(gaussian$software_versions$Software == "R"))
stopifnot(any(gaussian$software_versions$Software == "stats"))
stopifnot(all(c("MASS", "sandwich", "lmtest", "mice", "openxlsx") %in% required_packages))
display_coef <- generalized_display_coef_table(
  gaussian,
  variable_table = variable_info,
  category_table = category_table
)
stopifnot(any(grepl("Group:Treatment", display_coef$Term, fixed = TRUE)))
saved_html <- saved_generalized_results_html(
  gaussian,
  variable_table = variable_info,
  category_table = category_table
)
stopifnot(grepl("Model decision summary", saved_html, fixed = TRUE))
stopifnot(grepl("Variable coding", saved_html, fixed = TRUE))
stopifnot(grepl("Group:Treatment", saved_html, fixed = TRUE))
stopifnot(grepl("Publication table notes", saved_html, fixed = TRUE))
stopifnot(grepl("SCI reporting checklist", saved_html, fixed = TRUE))
stopifnot(grepl("Suggested manuscript text", saved_html, fixed = TRUE))
stopifnot(grepl("Software versions", saved_html, fixed = TRUE))
if (requireNamespace("openxlsx", quietly = TRUE)) {
  excel_path <- tempfile(fileext = ".xlsx")
  save_generalized_excel_file(
    gaussian,
    excel_path,
    variable_table = variable_info,
    category_table = category_table
  )
  stopifnot(file.exists(excel_path))
  stopifnot(file.info(excel_path)$size > 0)
  excel_sheets <- openxlsx::getSheetNames(excel_path)
  stopifnot(all(c("Table notes", "SCI checklist", "Manuscript text", "Software") %in% excel_sheets))
}

hc3 <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  se_type = "HC3"
)
stopifnot(identical(hc3$se_type_requested, "HC3"))
stopifnot(hc3$se_type_used %in% c("HC3", "model"))

model_based <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  se_type = "model"
)
stopifnot(identical(model_based$se_type_used, "model"))

if (requireNamespace("mice", quietly = TRUE)) {
  mi_result <- prepare_generalized_analysis_result(
    data,
    "y",
    c("x1", "x2"),
    family = "gaussian",
    robust = TRUE,
    missing_strategy = "mi",
    missing_imputations = 3L,
    missing_iterations = 2L
  )
  stopifnot(identical(mi_result$missing_strategy, "mi"))
  stopifnot(mi_result$n == nrow(data))
  stopifnot(is.data.frame(mi_result$missing_details))
  stopifnot(any(mi_result$missing_details$Item == "MI datasets"))
  stopifnot(any(grepl("Standard mice-based multiple imputation", mi_result$notes, fixed = TRUE)))
}

ipw_result <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  robust = TRUE,
  missing_strategy = "ipw"
)
stopifnot(identical(ipw_result$missing_strategy, "ipw"))
stopifnot(is.data.frame(ipw_result$missing_details))
stopifnot(any(ipw_result$missing_details$Item == "IPW summary"))
stopifnot(any(grepl("positivity/weight stability", ipw_result$missing_details$Value, fixed = TRUE)))
stopifnot(all(c(
  "Predicted observation probability: min",
  "Final weight summary",
  "Effective sample size",
  "Weight clipping count"
) %in% ipw_result$missing_details$Item))

data$bin <- rbinom(80, 1, plogis(-0.2 + 0.5 * data$x1))
logistic <- prepare_generalized_analysis_result(
  data,
  "bin",
  c("x1", "x2"),
  family = "binomial",
  robust = TRUE,
  variable_info = variable_info,
  category_table = category_table
)
stopifnot(isTRUE(logistic$exponentiate))
stopifnot(identical(logistic$family, "binomial"))
stopifnot(any(logistic$assumption_checks$Check == "Events per variable"))
stopifnot(any(logistic$assumption_checks$Check == "Separation risk"))
stopifnot(any(grepl("Binary outcome coded as event", logistic$coding_summary$Coding, fixed = TRUE)))

data$count <- rnbinom(80, mu = exp(0.3 + 0.4 * data$x1), size = 0.7)
count <- prepare_generalized_analysis_result(
  data,
  "count",
  c("x1", "x2"),
  exposure = "exposure",
  family = "count",
  robust = TRUE,
  overdispersion = TRUE
)
stopifnot(count$family %in% c("count", "negative_binomial"))
stopifnot(is.data.frame(count$count_details))
stopifnot(nrow(count$count_details) > 0)
stopifnot(any(count$count_details$Item == "Poisson dispersion ratio"))
stopifnot(any(count$count_details$Item == "Selection rule"))
stopifnot(any(grepl("AIC/BIC are reported as supplementary", count$count_details$Value, fixed = TRUE)))
stopifnot(any(count$decision_summary$Item == "Recommended reporting"))
stopifnot(identical(count$exposure, "exposure"))
stopifnot(grepl("offset(log(exposure))", paste(deparse(count$formula), collapse = " "), fixed = TRUE))

categorical_info <- data.frame(
  name = c("group", "x1"),
  measurement = c("category", "continuous"),
  stringsAsFactors = FALSE
)
categorical_error <- tryCatch(
  prepare_generalized_analysis_result(
    data.frame(group = factor(sample(c("A", "B", "C"), 80, TRUE)), x1 = data$x1),
    "group",
    "x1",
    family = "auto",
    variable_info = categorical_info
  ),
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("logistic regression menu", categorical_error, fixed = TRUE))
stopifnot(identical(unname(generalized_link_choices("binomial")), c("default", "logit")))
stopifnot(identical(generalized_resolve_link("binomial", "log"), "default"))

unchecked <- prepare_generalized_analysis_result(
  data,
  "y",
  c("x1", "x2"),
  family = "gaussian",
  assumption_checks = FALSE
)
stopifnot(is.data.frame(unchecked$assumption_checks))
stopifnot(nrow(unchecked$assumption_checks) == 0)

setup_state <- generalized_setup_state(
  selected_names = names(data),
  outcome = "y",
  exposure = "exposure",
  predictors = c("x1", "x2"),
  variable_table = data.frame(name = names(data), measurement = "continuous", stringsAsFactors = FALSE),
  missing_strategy = "mi",
  se_type = "HC3"
)
setup_html <- as.character(generalized_setup_panel(setup_state, NULL))
if (!grepl("id=\"generalized_options_tab\"", setup_html, fixed = TRUE) ||
    !grepl("id=\"generalized_missing_strategy\"", setup_html, fixed = TRUE) ||
    !grepl("id=\"generalized_se_type\"", setup_html, fixed = TRUE) ||
    !grepl("Robust sandwich HC3", setup_html, fixed = TRUE) ||
    !grepl("Multiple imputation settings", setup_html, fixed = TRUE) ||
    !grepl("Dependent variable", setup_html, fixed = TRUE) ||
    !grepl("Exposure / offset (optional)", setup_html, fixed = TRUE) ||
    !grepl("Independent variables", setup_html, fixed = TRUE)) {
  stop("GLM setup UI is missing expected tabbed missing-data controls.")
}

cat(
  "generalized validation passed:",
  gaussian$method,
  "/",
  logistic$method,
  "/",
  count$method,
  "\n"
)
