source(file.path("R", "app_bootstrap.R"))
load_app_packages()
source_app_modules()
options(statedu.app_language = "en")

required <- c("geepack", "mice", "lme4", "lmerTest", "plm")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing longitudinal model package(s): ", paste(missing, collapse = ", "))
}

set.seed(20260618)
subject_id <- rep(seq_len(40), each = 4)
site_id <- rep(rep(seq_len(8), each = 5), each = 4)
time <- rep(0:3, times = 40)
x <- stats::rnorm(length(subject_id))
group <- rep(sample(c("A", "B"), 40, replace = TRUE), each = 4)
random_intercept <- rep(stats::rnorm(40, 0, 0.8), each = 4)
continuous_y <- 2 + 0.4 * time + 0.7 * x + ifelse(group == "B", 0.5, 0) + random_intercept + stats::rnorm(length(subject_id))
integer_score_y <- pmin(5, pmax(1, round(continuous_y)))
binary_y <- stats::rbinom(length(subject_id), 1, stats::plogis(-1 + 0.2 * time + 0.5 * x + random_intercept * 0.4))
count_mu <- exp(1 + 0.12 * time + 0.25 * x + ifelse(group == "B", 0.2, 0) + random_intercept * 0.25)
count_y <- stats::rnbinom(length(subject_id), mu = count_mu, size = 1.6)
gamma_mu <- exp(0.6 + 0.08 * time + 0.18 * x + ifelse(group == "B", 0.15, 0) + random_intercept * 0.2)
gamma_y <- stats::rgamma(length(subject_id), shape = 3, rate = 3 / gamma_mu)
person_time <- runif(length(subject_id), 0.6, 1.8)

data <- data.frame(
  subject_id = subject_id,
  site_id = site_id,
  time = time,
  continuous_y = continuous_y,
  integer_score_y = integer_score_y,
  binary_y = factor(binary_y),
  count_y = count_y,
  gamma_y = gamma_y,
  person_time = person_time,
  x = x,
  aux_history = stats::rnorm(length(subject_id)),
  group = group,
  long_weight = runif(length(subject_id), 0.45, 2.2),
  stringsAsFactors = FALSE
)

missing_data <- data
missing_data$continuous_y[c(5, 18, 47, 92)] <- NA_real_
missing_data$x[c(11, 38, 119)] <- NA_real_

variable_info <- data.frame(
  name = names(data),
  var_label = names(data),
  role = "",
  measurement = c("category", "category", "continuous", "continuous", "continuous", "binary", "continuous", "continuous", "continuous", "continuous", "continuous", "category", "continuous"),
  stringsAsFactors = FALSE
)

validate_model <- function(model_type, outcome = "continuous_y", family = "auto", cluster = character(0), exposure = character(0), missing_method = "row_complete", missing_strategies = character(0), missing_imputations = 5L, missing_iterations = 5L, mi_outcome = "observed", ipw_auxiliary = character(0), data_input = data) {
  results <- prepare_longitudinal_analysis_result(
    data = data_input,
    outcome = outcome,
    id = "subject_id",
    cluster = cluster,
    time = "time",
    exposure = exposure,
    predictors = "x",
    covariates = "group",
    model_type = model_type,
    family = family,
    missing_method = missing_method,
    missing_strategies = missing_strategies,
    missing_imputations = missing_imputations,
    missing_iterations = missing_iterations,
    mi_outcome = mi_outcome,
    ipw_auxiliary = ipw_auxiliary,
    variable_info = variable_info
  )
  if (!is.list(results) || length(results) != 1) {
    print(attr(results, "skipped"))
    stop("Expected one fitted model for ", model_type)
  }
  table <- results[[1]]$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    stop("Expected a non-empty coefficient table for ", model_type)
  }
  assumption_checks <- results[[1]]$assumption_checks
  if (!is.data.frame(assumption_checks) || nrow(assumption_checks) == 0) {
    stop("Expected a non-empty assumption check table for ", model_type)
  }
  recommendations <- results[[1]]$recommendations
  if (length(recommendations) == 0 || !any(nzchar(recommendations))) {
    stop("Expected at least one recommended analysis message for ", model_type)
  }
  required_result_fields <- c(
    "data_structure",
    "missing_table",
    "missing_pattern",
    "missing_by_time",
    "missing_method",
    "missing_method_label",
    "missing_strategy_notes",
    "missing_sensitivity_results",
    "missing_imputations",
    "missing_iterations",
    "fit_details",
    "model_rationale",
    "sensitivity_recommendations",
    "sensitivity_results",
    "software_versions",
    "manuscript_text",
    "publication_notes",
    "reporting_checklist"
  )
  missing_fields <- setdiff(required_result_fields, names(results[[1]]))
  if (length(missing_fields) > 0) {
    stop("Missing SCI reporting result field(s): ", paste(missing_fields, collapse = ", "))
  }
  if (!is.data.frame(results[[1]]$data_structure) || nrow(results[[1]]$data_structure) == 0) {
    stop("Expected data structure summary for ", model_type)
  }
  if (!is.data.frame(results[[1]]$missing_pattern) || !any(results[[1]]$missing_pattern$Item == "Distinct missingness patterns")) {
    stop("Expected missing-data pattern summary for ", model_type)
  }
  if (!is.data.frame(results[[1]]$missing_by_time) || nrow(results[[1]]$missing_by_time) == 0) {
    stop("Expected time-specific missing-data summary for ", model_type)
  }
  if (!is.data.frame(results[[1]]$reporting_checklist) || nrow(results[[1]]$reporting_checklist) == 0) {
    stop("Expected SCI reporting checklist for ", model_type)
  }
  if (length(results[[1]]$sensitivity_recommendations) == 0) {
    stop("Expected sensitivity analysis suggestions for ", model_type)
  }
  if (!is.data.frame(results[[1]]$sensitivity_results) || nrow(results[[1]]$sensitivity_results) == 0) {
    stop("Expected automated sensitivity analysis results for ", model_type)
  }
  if (!is.data.frame(results[[1]]$manuscript_text) || nrow(results[[1]]$manuscript_text) == 0) {
    stop("Expected suggested manuscript text for ", model_type)
  }
  if (!is.data.frame(results[[1]]$publication_notes) || nrow(results[[1]]$publication_notes) == 0) {
    stop("Expected publication table notes for ", model_type)
  }
  invisible(results)
}

validate_model("gee")

ohio_binary_path <- file.path("sample", "longitudinal_examples", "longitudinal_gee_ohio_binary.csv")
if (file.exists(ohio_binary_path)) {
  ohio_binary <- utils::read.csv(ohio_binary_path, stringsAsFactors = FALSE)
  ohio_variable_info <- data.frame(
    name = names(ohio_binary),
    var_label = names(ohio_binary),
    role = "",
    measurement = c("binary", "category", "ordered", "binary"),
    stringsAsFactors = FALSE
  )
  ohio_results <- prepare_longitudinal_analysis_result(
    data = ohio_binary,
    outcome = "resp",
    id = "id",
    time = "age",
    predictors = "smoke",
    model_type = "gee",
    family = "auto",
    variable_info = ohio_variable_info
  )
  if (!is.list(ohio_results) || length(ohio_results) != 1) {
    print(attr(ohio_results, "skipped"))
    stop("Expected the Ohio binary GEE example to fit one model.")
  }
  if (!identical(ohio_results[[1]]$family, "binomial")) {
    stop("Expected the Ohio binary GEE example to resolve to a binomial family.")
  }
}

validate_model("lmm")
validate_model("lmm", cluster = "site_id")
validate_model("glmm", outcome = "binary_y", family = "binomial")
validate_model("glmm", outcome = "binary_y", family = "binomial", cluster = "site_id")
gee_count_results <- validate_model("gee", outcome = "count_y", family = "count")
glmm_count_results <- validate_model("glmm", outcome = "count_y", family = "count")
gee_count_offset_results <- validate_model("gee", outcome = "count_y", family = "count", exposure = "person_time")
validate_model("gee", outcome = "gamma_y", family = "gamma")
validate_model("glmm", outcome = "gamma_y", family = "gamma")
panel_fe_results <- validate_model("panel_fe")
panel_re_results <- validate_model("panel_re")

if (!any(panel_fe_results[[1]]$sensitivity_results$Analysis %in% "Panel covariance sensitivity") ||
    !any(grepl("Driscoll-Kraay SE", panel_fe_results[[1]]$sensitivity_results$Comparison, fixed = TRUE))) {
  stop("Panel FE sensitivity results should include Driscoll-Kraay covariance screening.")
}
if (!any(panel_re_results[[1]]$sensitivity_results$Analysis %in% "Panel covariance sensitivity") ||
    !any(grepl("Driscoll-Kraay SE", panel_re_results[[1]]$sensitivity_results$Comparison, fixed = TRUE))) {
  stop("Panel RE sensitivity results should include Driscoll-Kraay covariance screening.")
}

if (!identical(gee_count_results[[1]]$requested_family, "count") || !gee_count_results[[1]]$family %in% c("poisson", "negative_binomial")) {
  stop("GEE count family should resolve to Poisson or negative binomial.")
}
if (!identical(glmm_count_results[[1]]$requested_family, "count") || !glmm_count_results[[1]]$family %in% c("poisson", "negative_binomial")) {
  stop("GLMM count family should resolve to Poisson or negative binomial.")
}
if (!is.data.frame(gee_count_results[[1]]$fit_details) || !"Poisson dispersion ratio" %in% gee_count_results[[1]]$fit_details$Item) {
  stop("Count family results should report Poisson dispersion screening.")
}
if (!"Selection rule" %in% gee_count_results[[1]]$fit_details$Item ||
    !any(grepl("AIC/BIC are reported as supplementary", gee_count_results[[1]]$fit_details$Value, fixed = TRUE))) {
  stop("Count family results should report dispersion-threshold selection and supplementary AIC/BIC interpretation.")
}
if (!"Zero-inflation ratio" %in% gee_count_results[[1]]$fit_details$Item) {
  stop("Count family results should report zero-inflation screening.")
}
if (!identical(gee_count_offset_results[[1]]$offset_variable, "person_time") || !grepl("offset\\(log\\(person_time\\)\\)", paste(deparse(gee_count_offset_results[[1]]$formula), collapse = " "))) {
  stop("Count family results should apply the selected exposure as a log offset.")
}
offset_overview <- longitudinal_model_overview_table(gee_count_offset_results, variable_info)
if (!"Exposure / offset" %in% offset_overview$Item || !any(offset_overview[offset_overview$Item == "Exposure / offset", -1, drop = TRUE] == "person_time")) {
  stop("Model overview should report the applied exposure offset.")
}
if (any(c("Poisson / log", "Negative binomial / log") %in% names(longitudinal_family_choices()))) {
  stop("Poisson and negative binomial should not be separate primary family choices.")
}

auto_family_results <- validate_model("gee", outcome = "integer_score_y")
if (!identical(auto_family_results[[1]]$family, "gaussian")) {
  stop("Auto family should respect non-binary variable type metadata and keep integer score outcomes Gaussian.")
}
auto_count_results <- validate_model("gee", outcome = "count_y")
if (!identical(auto_count_results[[1]]$requested_family, "count") || !auto_count_results[[1]]$family %in% c("poisson", "negative_binomial")) {
  stop("Auto family should classify count-like non-negative integer outcomes as count, then resolve Poisson versus negative binomial.")
}

publication_results <- validate_model("lmm")
available_results <- validate_model("lmm", missing_method = "available", missing_strategies = c("mi", "ipw"))
if (!identical(available_results[[1]]$missing_method, "available") ||
    !grepl("available repeated measures", available_results[[1]]$missing_method_label, fixed = TRUE) ||
    !grepl("MAR", available_results[[1]]$missing_method_label, fixed = TRUE)) {
  stop("Expected likelihood-based mixed-model MAR missing-data handling to be retained in results.")
}
if (!any(grepl("subjects with other observed visits remained", available_results[[1]]$manuscript_text$SuggestedText, fixed = TRUE))) {
  stop("Expected LMM/GLMM manuscript text to distinguish available repeated measures from subject-level complete-case deletion.")
}
if (!all(c("mi", "ipw") %in% available_results[[1]]$missing_strategies) || length(available_results[[1]]$missing_strategy_notes) != 2) {
  stop("Expected MI/IPW missing-data sensitivity strategy notes.")
}
if (!any(grepl("standard mice-based", available_results[[1]]$missing_strategy_notes, fixed = TRUE)) ||
    !any(grepl("Positivity and weight stability", available_results[[1]]$missing_strategy_notes, fixed = TRUE))) {
  stop("Expected SCI-oriented MI/IPW sensitivity limitations in missing-data notes.")
}
missing_engine_results <- validate_model("lmm", missing_method = "available", missing_strategies = c("mi", "ipw"), missing_imputations = 3L, missing_iterations = 3L, ipw_auxiliary = "aux_history", data_input = missing_data)
missing_sensitivity <- missing_engine_results[[1]]$missing_sensitivity_results
if (!is.data.frame(missing_sensitivity) || nrow(missing_sensitivity) == 0 || !any(missing_sensitivity$Strategy %in% "Multiple imputation (MI)") || !any(missing_sensitivity$Strategy %in% "Inverse probability weighting (IPW)")) {
  stop("Expected actual MI and IPW missing-data sensitivity results.")
}
if (!any(missing_sensitivity$Status %in% "Fitted")) {
  stop("Expected at least one fitted missing-data sensitivity result.")
}
if (!any(grepl("originally missing dependent-variable", missing_sensitivity$Note, fixed = TRUE), na.rm = TRUE) ||
    !any(grepl("not a dedicated multilevel MI engine", missing_sensitivity$Note, fixed = TRUE), na.rm = TRUE) ||
    !any(grepl("positivity/weight stability", missing_sensitivity$Note, fixed = TRUE), na.rm = TRUE)) {
  stop("Expected MI/IPW sensitivity result notes to describe multilevel-MI and positivity limitations.")
}
if (!any(grepl("aux_history", missing_sensitivity$Note, fixed = TRUE), na.rm = TRUE)) {
  stop("Expected selected IPW auxiliary variable to be reported in missing-data sensitivity notes.")
}
gee_missing_engine_results <- validate_model("gee", missing_method = "row_complete", missing_strategies = c("mi", "wgee"), data_input = missing_data)
gee_missing_sensitivity <- gee_missing_engine_results[[1]]$missing_sensitivity_results
if (!is.data.frame(gee_missing_sensitivity) || !any(gee_missing_sensitivity$Strategy %in% "Weighted GEE (WGEE)")) {
  stop("Expected WGEE missing-data sensitivity results for GEE.")
}
weighted_results <- prepare_longitudinal_analysis_result(
  data = missing_data,
  outcome = "continuous_y",
  id = "subject_id",
  cluster = "site_id",
  time = "time",
  predictors = "x",
  covariates = "group",
  weight = "long_weight",
  model_type = "gee",
  family = "auto",
  missing_method = "row_complete",
  missing_strategies = "wgee",
  weight_type = "combined",
  weight_trim = "p01_99",
  variable_info = variable_info
)
if (!is.list(weighted_results) || length(weighted_results) != 1) {
  stop("Expected weighted longitudinal model to fit.")
}
weighted_result <- weighted_results[[1]]
if (!identical(weighted_result$weight_type, "combined") || !"long_weight" %in% weighted_result$weight || !is.data.frame(weighted_result$weight_summary) || nrow(weighted_result$weight_summary) == 0) {
  stop("Expected selected longitudinal weight to be retained and summarized.")
}
if (is.null(weighted_result$analysis_weights) || length(weighted_result$analysis_weights) != weighted_result$n || !all(is.finite(weighted_result$analysis_weights))) {
  stop("Expected final analysis weights for each analyzed row.")
}
if (!all(c(
  "Predicted observation probability: min",
  "Generated IPW summary",
  "Generated IPW effective sample size",
  "Weight clipping count"
) %in% weighted_result$weight_summary$Item)) {
  stop("Expected longitudinal IPW positivity and weight-stability diagnostics in the weight summary.")
}
if (!is.data.frame(weighted_result$missing_sensitivity_results) || !any(weighted_result$missing_sensitivity_results$Strategy %in% "Weighted GEE (WGEE)")) {
  stop("Expected weighted GEE sensitivity results with selected analysis weights.")
}
lmm_ignored_weight_results <- prepare_longitudinal_analysis_result(
  data = missing_data,
  outcome = "continuous_y",
  id = "subject_id",
  cluster = "site_id",
  time = "time",
  predictors = c("x", "long_weight"),
  covariates = "group",
  weight = "long_weight",
  model_type = "lmm",
  family = "auto",
  missing_method = "available",
  missing_strategies = character(0),
  weight_type = "combined",
  weight_trim = "p01_99",
  variable_info = variable_info
)
if (!is.list(lmm_ignored_weight_results) || length(lmm_ignored_weight_results) != 1) {
  stop("Expected LMM with ignored weight request to fit.")
}
lmm_ignored_weight_result <- lmm_ignored_weight_results[[1]]
if (!identical(lmm_ignored_weight_result$weight_type, "none") || !is.null(lmm_ignored_weight_result$analysis_weights)) {
  stop("LMM should force requested primary-fit weights to no weights.")
}
if (!"long_weight" %in% lmm_ignored_weight_result$predictors) {
  stop("LMM should not remove a selected predictor just because the same variable was displayed as an ignored weight candidate.")
}
publication_table <- longitudinal_publication_estimate_table(publication_results[[1]])
if (!is.data.frame(publication_table) || nrow(publication_table) == 0 || !"B (95% CI)" %in% names(publication_table)) {
  stop("Expected publication-ready estimate table.")
}
publication_html <- as.character(longitudinal_results_panel(publication_results, variable_info))
if (!grepl("Publication-ready estimates", publication_html, fixed = TRUE)) {
  stop("Expected publication-ready estimates section in result HTML.")
}
if (!grepl("Publication table notes", publication_html, fixed = TRUE)) {
  stop("Expected publication table notes section in result HTML.")
}

setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = c("x", "group"),
  model_type = "lmm",
  options_tab = "Checks"
)
setup_html <- as.character(longitudinal_setup_panel(setup_state, setup_status_message(TRUE, TRUE)))
if (!identical(setup_state$missing_method, "available")) {
  stop("LMM setup should default missing-data handling to available model rows under a MAR assumption.")
}
required_setup_fragments <- c(
  "longitudinal-setup-grid",
  "longitudinal-available-panel",
  "longitudinal-transfer-controls",
  "longitudinal-core-block",
  "longitudinal-predictors-block",
  "longitudinal-options",
  "id=\"longitudinal_outcome\"",
  "id=\"longitudinal_id\"",
  "id=\"longitudinal_cluster\"",
  "id=\"longitudinal_time\"",
  "id=\"longitudinal_exposure\"",
  "id=\"longitudinal_predictors\"",
  "id=\"longitudinal_outcome_move\"",
  "id=\"longitudinal_id_move\"",
  "id=\"longitudinal_cluster_move\"",
  "id=\"longitudinal_time_move\"",
  "id=\"longitudinal_exposure_move\"",
  "id=\"longitudinal_predictors_move\"",
  "Panel structure",
  "Model variables",
  "Dependent variable",
  "Independent variables",
  "Subject ID",
  "Cluster ID (optional)",
  "Exposure / offset (optional)",
  "Checks for selected model",
  "longitudinal-checks-options",
  "Convergence / singular fit",
  "Random-effect normality",
  "Residual normality",
  "Within-subject serial correlation"
)
missing_setup_fragments <- required_setup_fragments[!vapply(required_setup_fragments, grepl, logical(1), x = setup_html, fixed = TRUE)]
if (length(missing_setup_fragments) > 0) {
  stop("Longitudinal setup UI is missing expected fragment(s): ", paste(missing_setup_fragments, collapse = ", "))
}
if (grepl("GEE correlation", setup_html, fixed = TRUE)) {
  stop("LMM setup should not show GEE correlation options.")
}
if (grepl("Outcome family", setup_html, fixed = TRUE)) {
  stop("LMM setup should not show outcome family options.")
}
if (!grepl("Random slope for selected time variable", setup_html, fixed = TRUE)) {
  stop("LMM model tab should show random slope options.")
}

server_code <- paste(readLines(file.path("R", "server_longitudinal.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
required_drop_fragments <- c(
  "observeEvent(input$analysis_transfer_drop",
  "all_transfer_ids <- c(\"longitudinal_available\", target_ids)",
  "\"longitudinal_outcome\"",
  "\"longitudinal_id\"",
  "\"longitudinal_cluster\"",
  "\"longitudinal_time\"",
  "\"longitudinal_exposure\"",
  "\"longitudinal_predictors\"",
  "target %in% c(\"longitudinal_outcome\", \"longitudinal_id\", \"longitudinal_cluster\", \"longitudinal_time\", \"longitudinal_exposure\")",
  "identical(target, \"longitudinal_predictors\")"
)
missing_drop_fragments <- required_drop_fragments[!vapply(required_drop_fragments, grepl, logical(1), x = server_code, fixed = TRUE)]
if (length(missing_drop_fragments) > 0) {
  stop("Longitudinal drag/drop server handling is missing expected fragment(s): ", paste(missing_drop_fragments, collapse = ", "))
}

weight_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  weight = "long_weight",
  model_type = "gee",
  options_tab = "Weights",
  weight_type = "combined",
  weight_trim = "p01_99"
)
weight_setup_html <- as.character(longitudinal_setup_panel(weight_setup_state, NULL))
if (!grepl("id=\"longitudinal_weight_choice\"", weight_setup_html, fixed = TRUE) || !grepl("id=\"longitudinal_weight_type\"", weight_setup_html, fixed = TRUE) || !grepl("Analysis weight x generated IPW", weight_setup_html, fixed = TRUE) || !grepl("Trim extreme weights", weight_setup_html, fixed = TRUE)) {
  stop("Weights tab should show weight variable, weight type, and trimming controls.")
}
if (!grepl("Multiplies the selected analysis weight by generated inverse-probability weights", weight_setup_html, fixed = TRUE) || !grepl("Winsorizes extreme final weights below the 1st percentile", weight_setup_html, fixed = TRUE)) {
  stop("Weights tab should explain the selected weight type and trimming option.")
}
if (grepl("No weights", weight_setup_html, fixed = TRUE)) {
  stop("GEE weight options should hide No weights when a weight variable is selected.")
}
if (grepl("id=\"longitudinal_weight_move\"", weight_setup_html, fixed = TRUE)) {
  stop("Weights tab should use a dropdown weight selector instead of a transfer button.")
}

no_weight_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  model_type = "gee",
  options_tab = "Weights"
)
no_weight_setup_html <- as.character(longitudinal_setup_panel(no_weight_setup_state, NULL))
if (!identical(no_weight_setup_state$weight_type, "none") || !grepl("No weights", no_weight_setup_html, fixed = TRUE) || grepl("Trim extreme weights", no_weight_setup_html, fixed = TRUE)) {
  stop("No selected weight variable should force unweighted analysis and hide trimming.")
}
if (!grepl("No analysis weights are applied", no_weight_setup_html, fixed = TRUE)) {
  stop("No-weight setup should explain that no analysis weights are applied.")
}
if (!identical(unname(no_weight_setup_state$weight_choices)[[1]], "") || !identical(unname(no_weight_setup_state$weight_choices)[[2]], "long_weight")) {
  stop("Likely weight variables should be sorted to the top of the weight selector after the no-weight option.")
}
if (!all(c("continuous_y", "time", "x") %in% unname(no_weight_setup_state$weight_choices))) {
  stop("Weight selector should show likely weight variables first and keep the remaining continuous variables available in data order.")
}

lmm_weight_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  weight = "long_weight",
  model_type = "lmm",
  options_tab = "Weights"
)
if (!identical(lmm_weight_setup_state$weight_type, "none")) {
  stop("LMM should default to no weights even when a weight variable is selected.")
}
lmm_weight_setup_html <- as.character(longitudinal_setup_panel(lmm_weight_setup_state, NULL))
if (!grepl("Weights are disabled for primary LMM / GLMM.", lmm_weight_setup_html, fixed = TRUE)) {
  stop("LMM weights tab should explain that primary weighted mixed-model fits are not recommended.")
}
if (!grepl("id=\"longitudinal_weight_choice\"", lmm_weight_setup_html, fixed = TRUE) || !grepl("disabled=\"disabled\"", lmm_weight_setup_html, fixed = TRUE)) {
  stop("LMM weights tab should keep the weight controls visible but disabled.")
}
if (grepl("No analysis weights are applied", lmm_weight_setup_html, fixed = TRUE) || grepl("The selected LMM / GLMM model will be run", lmm_weight_setup_html, fixed = TRUE)) {
  stop("LMM weights tab should not repeat weight-type detail below disabled controls.")
}
if (grepl("Time-varying longitudinal weight", lmm_weight_setup_html, fixed = TRUE) || grepl("Analysis weight x generated IPW", lmm_weight_setup_html, fixed = TRUE) || grepl("Trim extreme weights", lmm_weight_setup_html, fixed = TRUE)) {
  stop("LMM weights tab should not expose weighted primary-fit options.")
}
glmm_weight_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "binary_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  weight = "long_weight",
  model_type = "glmm",
  options_tab = "Weights",
  weight_type = "combined"
)
glmm_weight_setup_html <- as.character(longitudinal_setup_panel(glmm_weight_setup_state, NULL))
if (!identical(glmm_weight_setup_state$weight_type, "none") || !grepl("Weights are disabled for primary LMM / GLMM.", glmm_weight_setup_html, fixed = TRUE)) {
  stop("GLMM weights tab should force no weights and show the mixed-model weight warning.")
}

gee_weight_default_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  weight = "long_weight",
  model_type = "gee",
  options_tab = "Weights"
)
if (!identical(gee_weight_default_state$weight_type, "longitudinal")) {
  stop("GEE should default selected weight variables to longitudinal weights.")
}
panel_weight_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  weight = "long_weight",
  model_type = "panel_fe",
  options_tab = "Weights",
  weight_type = "longitudinal"
)
panel_weight_setup_html <- as.character(longitudinal_setup_panel(panel_weight_setup_state, NULL))
if (grepl("GEE uses the selected weight directly", panel_weight_setup_html, fixed = TRUE) || !grepl("Panel models use weighted panel estimation", panel_weight_setup_html, fixed = TRUE)) {
  stop("Panel weights tab should show panel-specific guidance without GEE guidance.")
}

gee_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  model_type = "gee",
  options_tab = "Model"
)
gee_setup_html <- as.character(longitudinal_setup_panel(gee_setup_state, NULL))
if (!grepl("GEE correlation", gee_setup_html, fixed = TRUE) || !grepl("Outcome family", gee_setup_html, fixed = TRUE)) {
  stop("GEE setup should show outcome family and GEE correlation options.")
}
if (!grepl("Exposure / offset (optional)", gee_setup_html, fixed = TRUE)) {
  stop("Longitudinal setup should show the optional exposure / offset field.")
}
if (!identical(gee_setup_state$corstr, "exchangeable") || !grepl("<option value=\"exchangeable\" selected>Exchangeable</option>", gee_setup_html)) {
  stop("GEE setup should default the working correlation to Exchangeable.")
}
if (!grepl("Count: Poisson or negative binomial / log", gee_setup_html, fixed = TRUE) || !grepl("Gamma: positive skewed continuous / log", gee_setup_html, fixed = TRUE)) {
  stop("GEE/GLMM family choices should include unified count and Gamma log-link options.")
}
if (grepl("<option value=\"poisson\">", gee_setup_html, fixed = TRUE) || grepl("<option value=\"negative_binomial\">", gee_setup_html, fixed = TRUE)) {
  stop("GEE/GLMM setup should not expose Poisson and negative binomial as separate primary choices.")
}
if (!grepl("Include time as fixed effect", gee_setup_html, fixed = TRUE)) {
  stop("Model tab should include term options.")
}
if (!grepl("Report exp(B) for logit / log models", gee_setup_html, fixed = TRUE)) {
  stop("GEE model tab should include exponentiated reporting options.")
}

missing_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  model_type = "gee",
  options_tab = "Missing",
  missing_strategy = "mi"
)
missing_setup_html <- as.character(longitudinal_setup_panel(missing_setup_state, NULL))
if (!grepl("id=\"longitudinal_missing_strategy\"", missing_setup_html, fixed = TRUE) || !grepl("Complete-case: row-wise", missing_setup_html, fixed = TRUE) || !grepl("Weighted GEE (WGEE)", missing_setup_html, fixed = TRUE)) {
  stop("Missing tab should show a single model-specific missing-data strategy selector.")
}
if (grepl("Likelihood-based MAR", missing_setup_html, fixed = TRUE)) {
  stop("GEE missing-data selector should not offer mixed-model likelihood-based MAR handling.")
}
if (!grepl("Rubin", missing_setup_html, fixed = TRUE) || !grepl("id=\"longitudinal_missing_imputations\"", missing_setup_html, fixed = TRUE) || !grepl("id=\"longitudinal_missing_iterations\"", missing_setup_html, fixed = TRUE)) {
  stop("Missing tab should show selected MI explanation and MI settings.")
}
if (grepl("longitudinal_missing_strategies", missing_setup_html, fixed = TRUE)) {
  stop("Missing-data strategy UI should not allow multiple engine selection.")
}

lmm_missing_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  model_type = "lmm",
  options_tab = "Missing"
)
lmm_missing_setup_html <- as.character(longitudinal_setup_panel(lmm_missing_setup_state, NULL))
if (!grepl("Likelihood-based MAR: available repeated measures", lmm_missing_setup_html, fixed = TRUE) ||
    !grepl("A subject is not removed only because an outcome is missing at another visit", lmm_missing_setup_html, fixed = TRUE) ||
    grepl("Weighted GEE (WGEE)", lmm_missing_setup_html, fixed = TRUE)) {
  stop("LMM missing-data selector should offer likelihood MAR handling but not WGEE.")
}

panel_missing_setup_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  model_type = "panel_fe",
  options_tab = "Missing"
)
panel_missing_setup_html <- as.character(longitudinal_setup_panel(panel_missing_setup_state, NULL))
if (grepl("Likelihood-based MAR", panel_missing_setup_html, fixed = TRUE) || grepl("Weighted GEE (WGEE)", panel_missing_setup_html, fixed = TRUE)) {
  stop("Panel missing-data selector should not offer likelihood MAR handling or WGEE.")
}

legacy_output_state <- longitudinal_setup_state(
  selected_names = names(data),
  variable_table = variable_info,
  labels = stats::setNames(variable_info$var_label, variable_info$name),
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  model_type = "panel_fe",
  options_tab = "Output"
)
if (!identical(legacy_output_state$options_tab, "Model")) {
  stop("Legacy Output tab selection should resolve to the Model tab.")
}
legacy_output_html <- as.character(longitudinal_setup_panel(legacy_output_state, NULL))
if (grepl("Report exp(B)", legacy_output_html, fixed = TRUE) || grepl("No additional output options", legacy_output_html, fixed = TRUE)) {
  stop("Panel model tab should not show removed output-tab content.")
}
if (!grepl("Model", legacy_output_html, fixed = TRUE) || !grepl("Weights", legacy_output_html, fixed = TRUE) || !grepl("Missing", legacy_output_html, fixed = TRUE) || !grepl("Checks", legacy_output_html, fixed = TRUE)) {
  stop("Longitudinal options should expose Model, Weights, Missing, and Checks tabs.")
}

expected_check_counts <- c(gee = 4L, lmm = 6L, glmm = 5L, panel_fe = 5L, panel_re = 5L)
for (model_type in names(expected_check_counts)) {
  catalog <- longitudinal_check_catalog(model_type)
  if (!is.data.frame(catalog) || nrow(catalog) != expected_check_counts[[model_type]]) {
    stop("Unexpected assumption-check catalog for ", model_type)
  }
}

html_file <- tempfile(fileext = ".html")
write_longitudinal_results_html(publication_results, html_file, variable_info)
saved_html <- paste(readLines(html_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (!grepl("SCI reporting checklist", saved_html, fixed = TRUE) || !grepl("Suggested manuscript text", saved_html, fixed = TRUE)) {
  stop("Saved longitudinal HTML should include SCI reporting and manuscript sections.")
}
model_overview_position <- regexpr("Model overview", saved_html, fixed = TRUE)[[1]]
coefficients_position <- regexpr("Coefficients", saved_html, fixed = TRUE)[[1]]
if (model_overview_position < 0 || coefficients_position < 0 || coefficients_position < model_overview_position) {
  stop("Saved longitudinal HTML should show Model overview before Coefficients.")
}

excel_file <- tempfile(fileext = ".xlsx")
save_longitudinal_excel_file(publication_results, excel_file, variable_info)
sheet_names <- openxlsx::getSheetNames(excel_file)
required_sheets <- c("Model overview", "Coefficients 1", "Weights 1", "Missing pattern 1", "Missing by time 1", "Publication table 1", "Table notes 1", "Manuscript text 1", "SCI checklist 1", "Software 1", "Model guide")
missing_sheets <- setdiff(required_sheets, sheet_names)
if (length(missing_sheets) > 0) {
  stop("Longitudinal Excel export is missing expected sheet(s): ", paste(missing_sheets, collapse = ", "))
}
if (!identical(sheet_names[seq_len(2)], c("Model overview", "Coefficients 1"))) {
  stop("Longitudinal Excel export should start with Model overview followed by Coefficients.")
}

selective_results <- prepare_longitudinal_analysis_result(
  data = data,
  outcome = "continuous_y",
  id = "subject_id",
  time = "time",
  predictors = "x",
  model_type = "panel_fe",
  variable_info = variable_info,
  check_options = list(exogeneity = FALSE, hausman = FALSE)
)
selective_checks <- selective_results[[1]]$assumption_checks$Check
if ("Strict exogeneity / omitted confounding" %in% selective_checks || "FE vs RE assumption" %in% selective_checks) {
  stop("Disabled panel checks should not be included in assumption check output.")
}

cat("Longitudinal / panel validation passed.\n")
