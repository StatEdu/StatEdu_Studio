# Correlation analysis helpers.

correlation_variable_display_name <- function(name, variable_info = NULL, labels = character(0), category_table = NULL) {
  frequency_variable_display_name(name, variable_info, labels, category_table)
}

correlation_numeric_data <- function(data, variables) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  out <- data.frame(lapply(variables, function(name) suppressWarnings(as.numeric(data[[name]]))), check.names = FALSE)
  names(out) <- variables
  out
}

correlation_normality_summary <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL) {
  numeric_data <- correlation_numeric_data(data, variables)
  rows <- lapply(names(numeric_data), function(name) {
    values <- numeric_data[[name]]
    values <- values[!is.na(values)]
    skew <- sample_skewness(values)
    kurtosis <- sample_excess_kurtosis(values)
    satisfied <- is.finite(skew) && is.finite(kurtosis) && abs(skew) <= 2 && abs(kurtosis) <= 7
    data.frame(
      Name = name,
      Variable = correlation_variable_display_name(name, variable_info, labels, category_table),
      N = length(values),
      Skewness = format_decimal3(skew),
      Kurtosis = format_decimal3(kurtosis),
      Normality = if (isTRUE(satisfied)) "satisfied" else "not satisfied",
      normal = isTRUE(satisfied),
      check.names = FALSE
    )
  })
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

correlation_select_method <- function(x_name, y_name, normality_table = NULL, normality_checked = FALSE) {
  if (!isTRUE(normality_checked) || !is.data.frame(normality_table) || nrow(normality_table) == 0) {
    return("pearson")
  }
  normal_map <- stats::setNames(as.logical(normality_table$normal), as.character(normality_table$Name))
  if (isTRUE(normal_map[[x_name]]) && isTRUE(normal_map[[y_name]])) {
    "pearson"
  } else {
    "spearman"
  }
}

correlation_ci <- function(r, n, level = 0.95) {
  if (!is.finite(r) || n < 4 || abs(r) >= 1) {
    return(c(NA_real_, NA_real_))
  }
  z <- stats::atanh(r)
  se <- 1 / sqrt(n - 3)
  critical <- stats::qnorm(1 - (1 - level) / 2)
  stats::tanh(c(z - critical * se, z + critical * se))
}

correlation_sig <- function(p) {
  if (!is.finite(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

correlation_pair_result <- function(x, y, x_name, y_name, method) {
  complete <- stats::complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)
  if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(list(n = n, r = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = method))
  }
  test <- try(stats::cor.test(x, y, method = method, exact = FALSE), silent = TRUE)
  if (inherits(test, "try-error")) {
    return(list(n = n, r = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = method))
  }
  r <- unname(as.numeric(test$estimate[[1]]))
  ci <- if (!is.null(test$conf.int)) {
    as.numeric(test$conf.int[1:2])
  } else {
    correlation_ci(r, n)
  }
  list(n = n, r = r, p = as.numeric(test$p.value), ci = ci, method = method)
}

correlation_matrix_for_result <- function(numeric_data, method = "pearson") {
  if (ncol(numeric_data) < 2) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }
  suppressWarnings(stats::cor(numeric_data, use = "pairwise.complete.obs", method = method))
}

prepare_correlation_results <- function(
  data,
  variables,
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  options = list()
) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  shiny::validate(shiny::need(length(variables) >= 2, "Select at least two variables for correlation analysis."))

  numeric_data <- correlation_numeric_data(data, variables)
  numeric_counts <- vapply(numeric_data, function(values) sum(!is.na(values)), integer(1))
  variables <- names(numeric_counts)[numeric_counts >= 3]
  shiny::validate(shiny::need(length(variables) >= 2, "At least two selected variables must have three or more numeric values."))
  numeric_data <- numeric_data[, variables, drop = FALSE]

  normality_checked <- isTRUE(options$normality)
  normality_table <- if (isTRUE(normality_checked)) {
    correlation_normality_summary(data, variables, variable_info, labels, category_table)
  } else {
    data.frame()
  }
  display_names <- stats::setNames(
    vapply(variables, correlation_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    variables
  )

  rows <- list()
  for (i in seq_len(length(variables) - 1)) {
    for (j in seq.int(i + 1, length(variables))) {
      x_name <- variables[[i]]
      y_name <- variables[[j]]
      method <- correlation_select_method(x_name, y_name, normality_table, normality_checked)
      pair <- correlation_pair_result(numeric_data[[x_name]], numeric_data[[y_name]], x_name, y_name, method)
      rows[[length(rows) + 1]] <- data.frame(
        Variable1 = display_names[[x_name]],
        Variable2 = display_names[[y_name]],
        N = pair$n,
        Method = if (identical(pair$method, "pearson")) "Pearson" else "Spearman",
        r = format_decimal3(pair$r),
        p = format_p(pair$p),
        `95% CI` = if (all(is.finite(pair$ci))) {
          sprintf("%s~%s", format_decimal3(pair$ci[[1]]), format_decimal3(pair$ci[[2]]))
        } else {
          ""
        },
        Sig = correlation_sig(pair$p),
        check.names = FALSE
      )
    }
  }

  pairwise_table <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  matrix_method <- if (isTRUE(normality_checked) && is.data.frame(normality_table) && any(!normality_table$normal)) {
    "spearman"
  } else {
    "pearson"
  }
  correlation_matrix <- correlation_matrix_for_result(numeric_data, matrix_method)
  dimnames(correlation_matrix) <- list(unname(display_names[colnames(correlation_matrix)]), unname(display_names[colnames(correlation_matrix)]))

  list(
    variables = variables,
    labels = display_names,
    data = numeric_data,
    options = options,
    normality_table = normality_table,
    pairwise_table = pairwise_table,
    correlation_matrix = correlation_matrix,
    matrix_method = matrix_method
  )
}
