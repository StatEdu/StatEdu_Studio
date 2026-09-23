all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_spss31_classical_results.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules()

fixture_path <- file.path(repo_root, "scripts", "fixtures", "survival_validation.csv")
reference_path <- file.path(repo_root, "sample", "spss31_classical_results.csv")
stopifnot(file.exists(fixture_path), file.exists(reference_path))
data <- utils::read.csv(fixture_path, check.names = FALSE, stringsAsFactors = FALSE)
reference <- utils::read.csv(reference_path, check.names = FALSE, stringsAsFactors = FALSE)

data$age_m <- data$age
data$age_m[data$id <= 2] <- NA_real_
data$status_m <- data$status
data$status_m[data$id == 1] <- NA_integer_

variable_info <- data.frame(
  name = c("time", "age", "age_m", "status", "sex", "ph.ecog", "status_m"),
  measurement = c("continuous", "continuous", "continuous", "binary", "binary", "category", "binary"),
  stringsAsFactors = FALSE
)
result <- prepare_frequencies_results(data, variable_info$name, variable_info = variable_info)

reference_name <- function(name) if (identical(name, "ph.ecog")) "ph_ecog" else name
ref_value <- function(section, variable, level = "", metric) {
  selected <- reference$Section == section & reference$Variable == reference_name(variable) &
    reference$Level == level & reference$Metric == metric
  if (sum(selected) != 1L) stop("Reference key is missing or duplicated: ", paste(section, variable, level, metric, sep = " / "))
  reference$Value[selected]
}

raw_descriptive <- function(values) {
  valid <- as.numeric(values[!is.na(values)])
  c(
    Valid = length(valid),
    Missing = sum(is.na(values)),
    Mean = mean(valid),
    Median = stats::median(valid),
    `Std. Deviation` = stats::sd(valid),
    Skewness = sample_skewness(valid),
    Kurtosis = sample_excess_kurtosis(valid),
    Minimum = min(valid),
    Maximum = max(valid)
  )
}

descriptive_max_error <- 0
quartile_convention_max_error <- 0
quartile_method_differences <- numeric(0)
for (variable in c("time", "age", "age_m")) {
  actual <- raw_descriptive(data[[variable]])
  expected <- vapply(names(actual), function(metric) ref_value("descriptive", variable, metric = metric), numeric(1))
  descriptive_max_error <- max(descriptive_max_error, abs(actual - expected))

  display <- result$descriptive_table[result$descriptive_table$Name == variable, , drop = FALSE]
  stopifnot(nrow(display) == 1L)
  stopifnot(
    display$N == expected[["Valid"]],
    display$Missing == expected[["Missing"]],
    identical(display$Mean, formatC(expected[["Mean"]], format = "f", digits = 2)),
    identical(display$SD, formatC(expected[["Std. Deviation"]], format = "f", digits = 2)),
    identical(display$Median, formatC(expected[["Median"]], format = "f", digits = 2)),
    identical(display$Min, formatC(expected[["Minimum"]], format = "f", digits = 2)),
    identical(display$Max, formatC(expected[["Maximum"]], format = "f", digits = 2)),
    identical(display$Skewness, formatC(expected[["Skewness"]], format = "f", digits = 3)),
    identical(display$Kurtosis, formatC(expected[["Kurtosis"]], format = "f", digits = 3))
  )
  valid_values <- as.numeric(data[[variable]][!is.na(data[[variable]])])
  app_quartiles <- stats::quantile(valid_values, c(.25, .75), names = FALSE, type = 7)
  spss_quartiles <- vapply(c("P25", "P75"), function(metric) ref_value("descriptive", variable, metric = metric), numeric(1))
  spss_type6 <- stats::quantile(valid_values, c(.25, .75), names = FALSE, type = 6)
  quartile_convention_max_error <- max(quartile_convention_max_error, abs(spss_quartiles - spss_type6))
  quartile_method_differences <- c(quartile_method_differences, abs(app_quartiles - spss_quartiles))
  expected_iqr <- app_quartiles[[2]] - app_quartiles[[1]]
  stopifnot(
    identical(display$IQR, formatC(expected_iqr, format = "f", digits = 2)),
    grepl(formatC(app_quartiles[[1]], format = "f", digits = 2), display[["IQR(Q1~Q3)"]], fixed = TRUE),
    grepl(formatC(app_quartiles[[2]], format = "f", digits = 2), display[["IQR(Q1~Q3)"]], fixed = TRUE)
  )
}
stopifnot(
  descriptive_max_error < 1e-10,
  quartile_convention_max_error < 1e-10,
  max(quartile_method_differences) > 0
)

frequency_checks <- 0L
frequency_percent_max_display_error <- 0
for (table in result$categorical_tables) {
  variable <- table$Name[[1]]
  for (index in seq_len(nrow(table))) {
    level <- as.character(table$Value[[index]])
    spss_n <- ref_value("frequency", variable, level, "Frequency")
    spss_percent <- ref_value("frequency", variable, level, "Percent")
    stopifnot(table$N[[index]] == spss_n)
    display_percent <- as.numeric(gsub("\u00a0", "", table$Percent[[index]], fixed = TRUE))
    frequency_percent_max_display_error <- max(frequency_percent_max_display_error, abs(display_percent - round(spss_percent, 1)))
    frequency_checks <- frequency_checks + 2L
  }
}
stopifnot(frequency_checks == 20L, frequency_percent_max_display_error < 1e-12)

# StatEdu's Percent includes missing observations in the denominator and therefore
# aligns with SPSS Percent, not SPSS Valid Percent, when system missing data exist.
status_missing <- result$categorical_tables[[which(vapply(result$categorical_tables, function(x) x$Name[[1]] == "status_m", logical(1)))]]
status_zero <- status_missing[status_missing$Value == "0", , drop = FALSE]
spss_percent <- ref_value("frequency", "status_m", "0", "Percent")
spss_valid_percent <- ref_value("frequency", "status_m", "0", "Valid Percent")
stopifnot(
  identical(status_zero$Percent, formatC(spss_percent, format = "f", digits = 1)),
  !identical(status_zero$Percent, formatC(spss_valid_percent, format = "f", digits = 1))
)

cat(sprintf(
  paste0(
    "SPSS 31 descriptive/frequency external validation passed: ",
    "27 core descriptive values agree (max raw error %.3g); ",
    "six SPSS percentile values reproduce R type=6 (max error %.3g), while StatEdu uses type=7; ",
    "20 frequency count/display-percent checks agree; missing-denominator semantics are explicit.\n"
  ),
  descriptive_max_error,
  quartile_convention_max_error
))
