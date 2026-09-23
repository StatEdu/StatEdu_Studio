all_args <- commandArgs(trailingOnly = TRUE)
value_arg <- function(prefix, default = "") {
  hit <- all_args[startsWith(all_args, prefix)]
  if (length(hit) == 0L) return(default)
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "scripts/import_spss_survival_output.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
input_path <- value_arg("--input=", file.path(repo_root, "outputs", "spss_survival_validation.xml"))
output_path <- value_arg("--output=", file.path(repo_root, "sample", "spss31_survival_results.csv"))

if (!requireNamespace("xml2", quietly = TRUE)) stop("The xml2 package is required.")
if (!file.exists(input_path)) stop("SPSS OXML output was not found: ", input_path)

doc <- xml2::read_xml(input_path)
xml2::xml_ns_strip(doc)

number_at <- function(node, xpath) {
  cell <- xml2::xml_find_first(node, xpath)
  if (inherits(cell, "xml_missing")) return(NA_real_)
  suppressWarnings(as.numeric(xml2::xml_attr(cell, "number")))
}

rows <- list()
add_value <- function(section, group = "", term = "", metric, value) {
  rows[[length(rows) + 1L]] <<- data.frame(
    Section = section,
    Group = as.character(group),
    Term = as.character(term),
    Metric = metric,
    Value = as.numeric(value),
    stringsAsFactors = FALSE
  )
}

km_command <- xml2::xml_find_first(doc, ".//command[@command='Kaplan-Meier']")
cox_command <- xml2::xml_find_first(doc, ".//command[@command='Cox Regression']")
if (inherits(km_command, "xml_missing") || inherits(cox_command, "xml_missing")) {
  stop("The OXML file does not contain both Kaplan-Meier and Cox Regression output.")
}

km_cases <- xml2::xml_find_first(km_command, ".//pivotTable[@subType='Case Processing Summary']")
km_case_nodes <- xml2::xml_find_all(km_cases, "./dimension[@axis='row']/group/category")
for (node in km_case_nodes) {
  group <- xml2::xml_attr(node, "text")
  add_value("km_cases", group, metric = "N", value = number_at(node, ".//category[@text='Total N']/cell"))
  add_value("km_cases", group, metric = "Events", value = number_at(node, ".//category[@text='N of Events']/cell"))
}
km_overall_cases <- xml2::xml_find_first(km_cases, "./dimension[@axis='row']/category[@text='Overall']")
add_value("km_cases", "Overall", metric = "N", value = number_at(km_overall_cases, ".//category[@text='Total N']/cell"))
add_value("km_cases", "Overall", metric = "Events", value = number_at(km_overall_cases, ".//category[@text='N of Events']/cell"))

km_survival <- xml2::xml_find_first(km_command, ".//pivotTable[@subType='Survival Table']")
km_group_nodes <- xml2::xml_find_all(km_survival, "./dimension[@axis='row']/category")
for (group_node in km_group_nodes) {
  group <- xml2::xml_attr(group_node, "text")
  record_nodes <- xml2::xml_find_all(group_node, "./dimension[@axis='row']/category")
  for (record in record_nodes) {
    status <- number_at(record, ".//category[@text='Status']/cell")
    estimate <- number_at(record, ".//group[@text='Cumulative Proportion Surviving at the Time']/category[@text='Estimate']/cell")
    if (isTRUE(status == 1) && is.finite(estimate)) {
      time <- number_at(record, ".//category[@text='Time']/cell")
      add_value("km_survival", group, format(time, scientific = FALSE, trim = TRUE), "Estimate", estimate)
      add_value("km_survival", group, format(time, scientific = FALSE, trim = TRUE), "SE", number_at(record, ".//group[@text='Cumulative Proportion Surviving at the Time']/category[@text='Std. Error']/cell"))
    }
  }
}

km_times <- xml2::xml_find_first(km_command, ".//pivotTable[@subType='Means and Medians for Survival Time']")
km_time_nodes <- xml2::xml_find_all(km_times, "./dimension[@axis='row']/category")
for (node in km_time_nodes) {
  group <- xml2::xml_attr(node, "text")
  for (statistic in c("Mean", "Median")) {
    base <- sprintf("./dimension[@axis='column']/category[@text='%s']/dimension[@axis='column']", statistic)
    add_value("km_time", group, statistic, "Estimate", number_at(node, paste0(base, "/category[@text='Estimate']/cell")))
    add_value("km_time", group, statistic, "SE", number_at(node, paste0(base, "/category[@text='Std. Error']/cell")))
    add_value("km_time", group, statistic, "Lower", number_at(node, paste0(base, "/group/category[@text='Lower Bound']/cell")))
    add_value("km_time", group, statistic, "Upper", number_at(node, paste0(base, "/group/category[@text='Upper Bound']/cell")))
  }
}

logrank <- xml2::xml_find_first(km_command, ".//pivotTable[@subType='Overall Comparisons']//dimension[@axis='row']/category")
add_value("km_logrank", "Overall", metric = "Chi-square", value = number_at(logrank, ".//category[@text='Chi-Square']/cell"))
add_value("km_logrank", "Overall", metric = "df", value = number_at(logrank, ".//category[@text='df']/cell"))
add_value("km_logrank", "Overall", metric = "p", value = number_at(logrank, ".//category[@text='Sig.']/cell"))

cox_tables <- xml2::xml_find_all(cox_command, ".//pivotTable[@subType='Omnibus Tests of Model Coefficients']")
if (length(cox_tables) != 2L) stop("Expected the null and fitted Cox omnibus tables.")
add_value("cox_model", "", "Null", "-2 Log Likelihood", number_at(cox_tables[[1]], ".//category[@text='-2 Log Likelihood']/cell"))
add_value("cox_model", "", "Fitted", "-2 Log Likelihood", number_at(cox_tables[[2]], ".//category[@text='-2 Log Likelihood']/cell"))
add_value("cox_model", "", "Score", "Chi-square", number_at(cox_tables[[2]], ".//group[@text='Overall (score)']/category[@text='Chi-square']/cell"))
add_value("cox_model", "", "Score", "df", number_at(cox_tables[[2]], ".//group[@text='Overall (score)']/category[@text='df']/cell"))
add_value("cox_model", "", "Score", "p", number_at(cox_tables[[2]], ".//group[@text='Overall (score)']/category[@text='Sig.']/cell"))
add_value("cox_model", "", "Likelihood-ratio", "Chi-square", number_at(cox_tables[[2]], ".//group[@text='Change From Previous Block']/category[@text='Chi-square']/cell"))
add_value("cox_model", "", "Likelihood-ratio", "df", number_at(cox_tables[[2]], ".//group[@text='Change From Previous Block']/category[@text='df']/cell"))
add_value("cox_model", "", "Likelihood-ratio", "p", number_at(cox_tables[[2]], ".//group[@text='Change From Previous Block']/category[@text='Sig.']/cell"))

cox_coefficients <- xml2::xml_find_first(cox_command, ".//pivotTable[@subType='Variables in the Equation']")
cox_term_nodes <- xml2::xml_find_all(cox_coefficients, "./dimension[@axis='row']/category")
for (node in cox_term_nodes) {
  term <- xml2::xml_attr(node, "text")
  metric_paths <- c(
    B = ".//category[@text='B']/cell",
    SE = ".//category[@text='SE']/cell",
    Wald = ".//category[@text='Wald']/cell",
    df = ".//category[@text='df']/cell",
    p = ".//category[@text='Sig.']/cell",
    HR = ".//category[@text='Exp(B)']/cell",
    Lower = ".//group[@text='95.0% CI for Exp(B)']/category[@text='Lower']/cell",
    Upper = ".//group[@text='95.0% CI for Exp(B)']/category[@text='Upper']/cell"
  )
  for (metric in names(metric_paths)) add_value("cox_coefficient", "", term, metric, number_at(node, metric_paths[[metric]]))
}

result <- do.call(rbind, rows)
if (any(!is.finite(result$Value))) stop("At least one expected SPSS output value could not be extracted.")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(result, output_path, row.names = FALSE, na = "")
cat(sprintf("Imported %d SPSS survival reference values to %s\n", nrow(result), normalizePath(output_path, winslash = "/", mustWork = TRUE)))
