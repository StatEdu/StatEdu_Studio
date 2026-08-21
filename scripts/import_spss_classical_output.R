all_args <- commandArgs(trailingOnly = TRUE)
value_arg <- function(prefix, default = "") {
  hit <- all_args[startsWith(all_args, prefix)]
  if (length(hit) == 0L) return(default)
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "scripts/import_spss_classical_output.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
input_path <- value_arg("--input=", file.path(repo_root, "outputs", "spss_classical_validation.xml"))
output_path <- value_arg("--output=", file.path(repo_root, "sample", "spss31_classical_results.csv"))

if (!requireNamespace("xml2", quietly = TRUE)) stop("The xml2 package is required.")
if (!file.exists(input_path)) stop("SPSS OXML output was not found: ", input_path)

doc <- xml2::read_xml(input_path)
xml2::xml_ns_strip(doc)
commands <- xml2::xml_find_all(doc, ".//command[@command='Frequencies']")
if (length(commands) != 2L) stop("Expected two SPSS FREQUENCIES commands in the OXML output.")

number_at <- function(node, xpath = "./cell") {
  cell <- xml2::xml_find_first(node, xpath)
  if (inherits(cell, "xml_missing")) return(NA_real_)
  suppressWarnings(as.numeric(xml2::xml_attr(cell, "number")))
}
clean_level <- function(node) {
  number <- suppressWarnings(as.numeric(xml2::xml_attr(node, "number")))
  if (is.finite(number)) return(format(number, scientific = FALSE, trim = TRUE))
  xml2::xml_attr(node, "text")
}

rows <- list()
add_value <- function(section, variable, level = "", metric, value) {
  rows[[length(rows) + 1L]] <<- data.frame(
    Section = section,
    Variable = variable,
    Level = level,
    Metric = metric,
    Value = as.numeric(value),
    stringsAsFactors = FALSE
  )
}

statistics <- xml2::xml_find_first(commands[[1]], ".//pivotTable[@subType='Statistics']")
statistics_rows <- xml2::xml_find_first(statistics, "./dimension[@axis='row']")

for (metric in c("Valid", "Missing")) {
  variable_nodes <- xml2::xml_find_all(statistics_rows, sprintf("./group[@text='N']/category[@text='%s']/dimension/category", metric))
  for (node in variable_nodes) add_value("descriptive", xml2::xml_attr(node, "varName"), metric = metric, value = number_at(node))
}

direct_metrics <- c("Mean", "Median", "Std. Deviation", "Skewness", "Kurtosis", "Minimum", "Maximum")
for (metric in direct_metrics) {
  variable_nodes <- xml2::xml_find_all(statistics_rows, sprintf("./category[@text='%s']/dimension/category", metric))
  for (node in variable_nodes) add_value("descriptive", xml2::xml_attr(node, "varName"), metric = metric, value = number_at(node))
}

for (percentile in c("25", "75")) {
  variable_nodes <- xml2::xml_find_all(statistics_rows, sprintf("./group[@text='Percentiles']/category[@text='%s']/dimension/category", percentile))
  for (node in variable_nodes) add_value("descriptive", xml2::xml_attr(node, "varName"), metric = paste0("P", percentile), value = number_at(node))
}

frequency_tables <- xml2::xml_find_all(commands[[2]], ".//pivotTable[@subType='Frequencies']")
for (table in frequency_tables) {
  variable <- xml2::xml_attr(table, "varName")
  valid_nodes <- xml2::xml_find_all(table, sprintf(".//group[@text='Valid']//category[@varName='%s']", variable))
  for (node in valid_nodes) {
    level <- clean_level(node)
    for (metric in c("Frequency", "Percent", "Valid Percent")) {
      add_value("frequency", variable, level, metric, number_at(node, sprintf("./dimension/category[@text='%s']/cell", metric)))
    }
  }
  missing_node <- xml2::xml_find_first(table, ".//group[@text='Missing']/category[@text='System']")
  if (!inherits(missing_node, "xml_missing")) {
    add_value("frequency", variable, "(Missing)", "Frequency", number_at(missing_node, "./dimension/category[@text='Frequency']/cell"))
    add_value("frequency", variable, "(Missing)", "Percent", number_at(missing_node, "./dimension/category[@text='Percent']/cell"))
  }
}

result <- do.call(rbind, rows)
if (any(!is.finite(result$Value))) stop("At least one expected SPSS output value could not be extracted.")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(result, output_path, row.names = FALSE, na = "")
cat(sprintf("Imported %d SPSS classical-statistics reference values to %s\n", nrow(result), normalizePath(output_path, winslash = "/", mustWork = TRUE)))
