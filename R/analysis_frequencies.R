# Frequency and descriptive statistics helpers.

format_frequency_percent <- function(value, pad_under_10 = FALSE) {
  if (length(value) == 0 || is.na(value)) {
    return(NA_character_)
  }
  formatted <- formatC(value, format = "f", digits = 1)
  if (isTRUE(pad_under_10) && value < 10) {
    formatted <- paste0(" ", formatted)
  }
  formatted
}

frequency_variable_display_name <- function(name, variable_info = NULL, labels = character(0), category_table = NULL) {
  display_labels <- labels
  category_labels <- category_var_label_lookup_static(category_table)
  if (length(category_labels) > 0) {
    display_labels[names(category_labels)] <- category_labels
  }
  display_variable_name_static(name, variable_info, display_labels, label_only = TRUE)
}

frequency_value_display_labels <- function(name, values, category_table = NULL) {
  labels <- category_value_label_lookup_static(category_table)[[name]]
  values <- as.character(values)
  if (is.null(labels) || length(labels) == 0) {
    return(values)
  }
  vapply(values, function(value) {
    label <- named_value(labels, value, "")
    if (nzchar(label)) label else value
  }, character(1))
}

frequency_value_order <- function(values) {
  values <- as.character(values)
  non_missing <- values[!is.na(values)]
  unique_values <- unique(non_missing)
  numeric_values <- suppressWarnings(as.numeric(unique_values))
  if (length(unique_values) > 0 && all(!is.na(numeric_values))) {
    return(unique_values[order(numeric_values)])
  }
  sort(unique_values)
}

frequency_table_for_variable <- function(data, name, variable_info = NULL, labels = character(0), category_table = NULL) {
  values <- data[[name]]
  values_chr <- as.character(values)
  values_chr[is.na(values)] <- "(Missing)"
  tab <- table(values_chr, useNA = "no")
  ordered_values <- frequency_value_order(names(tab))
  tab <- tab[ordered_values]
  total <- sum(tab)
  counts <- as.integer(tab)
  percent <- if (total > 0) counts / total * 100 else rep(NA_real_, length(tab))
  percent_display <- vapply(percent, format_frequency_percent, character(1), pad_under_10 = FALSE)
  percent_summary_display <- vapply(percent, format_frequency_percent, character(1), pad_under_10 = TRUE)
  data.frame(
    Name = name,
    Variable = frequency_variable_display_name(name, variable_info, labels, category_table),
    Value = frequency_value_display_labels(name, names(tab), category_table),
    N = counts,
    Percent = percent_display,
    `n (%)` = paste0(counts, "(", percent_summary_display, ")"),
    check.names = FALSE
  )
}

sample_skewness <- function(values) {
  n <- length(values)
  if (n < 3) {
    return(NA_real_)
  }
  centered <- values - mean(values)
  s <- stats::sd(values)
  if (is.na(s) || s == 0) {
    return(NA_real_)
  }
  n / ((n - 1) * (n - 2)) * sum((centered / s)^3)
}

sample_excess_kurtosis <- function(values) {
  n <- length(values)
  if (n < 4) {
    return(NA_real_)
  }
  centered <- values - mean(values)
  s <- stats::sd(values)
  if (is.na(s) || s == 0) {
    return(NA_real_)
  }
  term1 <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum((centered / s)^4)
  term2 <- 3 * (n - 1)^2 / ((n - 2) * (n - 3))
  term1 - term2
}

format_frequency_decimal <- function(value, digits = 2) {
  if (length(value) == 0 || is.na(value)) {
    return(NA_character_)
  }
  formatC(value, format = "f", digits = digits)
}

descriptive_table_for_variable <- function(data, name, variable_info = NULL, labels = character(0), category_table = NULL) {
  values <- suppressWarnings(as.numeric(data[[name]]))
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    empty_table <- data.frame(
      Name = name,
      Variable = frequency_variable_display_name(name, variable_info, labels, category_table),
      N = 0L,
      Missing = sum(is.na(data[[name]])),
      Mean = NA_character_,
      SD = NA_character_,
      M_SD = NA_character_,
      Median = NA_character_,
      IQR = NA_character_,
      IQR_Q1_Q3 = NA_character_,
      Min = NA_character_,
      Max = NA_character_,
      Skewness = NA_character_,
      Kurtosis = NA_character_,
      check.names = FALSE
    )
    names(empty_table)[names(empty_table) == "M_SD"] <- "M \u00b1 SD"
    names(empty_table)[names(empty_table) == "IQR_Q1_Q3"] <- "IQR(Q1~Q3)"
    return(empty_table)
  }
  mean_value <- mean(values)
  sd_value <- stats::sd(values)
  q1_value <- stats::quantile(values, 0.25, names = FALSE, type = 7)
  q3_value <- stats::quantile(values, 0.75, names = FALSE, type = 7)
  iqr_value <- stats::IQR(values)
  result <- data.frame(
    Name = name,
    Variable = frequency_variable_display_name(name, variable_info, labels, category_table),
    N = length(values),
    Missing = sum(is.na(data[[name]])),
    Mean = format_frequency_decimal(mean_value),
    SD = format_frequency_decimal(sd_value),
    M_SD = paste0(format_frequency_decimal(mean_value), " \u00b1 ", format_frequency_decimal(sd_value)),
    Median = format_frequency_decimal(stats::median(values)),
    IQR = format_frequency_decimal(iqr_value),
    IQR_Q1_Q3 = paste0(
      format_frequency_decimal(iqr_value),
      " (",
      format_frequency_decimal(q1_value),
      "~",
      format_frequency_decimal(q3_value),
      ")"
    ),
    Min = format_frequency_decimal(min(values)),
    Max = format_frequency_decimal(max(values)),
    Skewness = format_frequency_decimal(sample_skewness(values), digits = 3),
    Kurtosis = format_frequency_decimal(sample_excess_kurtosis(values), digits = 3),
    check.names = FALSE
  )
  names(result)[names(result) == "M_SD"] <- "M \u00b1 SD"
  names(result)[names(result) == "IQR_Q1_Q3"] <- "IQR(Q1~Q3)"
  result
}

prepare_frequencies_results <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  shiny::validate(shiny::need(length(variables) > 0, "Move at least one variable into Variables."))

  measurements <- character(0)
  if (!is.null(variable_info) && all(c("name", "measurement") %in% names(variable_info))) {
    measurements <- stats::setNames(as.character(variable_info$measurement), as.character(variable_info$name))
  }
  variable_measurements <- vapply(variables, function(name) named_value(measurements, name, ""), character(1))
  continuous <- variables[variable_measurements == "continuous"]
  categorical <- setdiff(variables, continuous)

  list(
    variables = variables,
    continuous = continuous,
    categorical = categorical,
    categorical_tables = lapply(categorical, frequency_table_for_variable, data = data, variable_info = variable_info, labels = labels, category_table = category_table),
    descriptive_table = if (length(continuous) > 0) {
      do.call(rbind, lapply(continuous, descriptive_table_for_variable, data = data, variable_info = variable_info, labels = labels, category_table = category_table))
    } else {
      NULL
    },
    data = data,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table
  )
}
