# Factor analysis helpers.

factor_analysis_method_choices <- function() {
  c(
    "Principal axis factoring" = "pa",
    "Maximum likelihood" = "ml"
  )
}

factor_analysis_rotation_choices <- function() {
  c(
    "None" = "none",
    "Varimax" = "varimax",
    "Oblimin" = "oblimin"
  )
}

factor_analysis_criterion_choices <- function() {
  c(
    "Eigenvalue >= 1.0" = "eigen",
    "Fixed number of factors" = "fixed"
  )
}

factor_analysis_matrix_choices <- function() {
  c(
    "Pearson correlation" = "pearson",
    "Polychoric correlation" = "polychoric"
  )
}

factor_analysis_normality_method_choices <- function() {
  c(
    "Skewness / kurtosis" = "skew_kurt",
    "Mardia test" = "mardia"
  )
}

factor_analysis_assumption_choices <- function() {
  c(
    "None" = "none",
    factor_analysis_normality_method_choices()
  )
}

factor_analysis_method_label <- function(method) {
  choices <- factor_analysis_method_choices()
  names(choices)[match(method, choices)] %||% method
}

factor_analysis_rotation_label <- function(rotation) {
  choices <- factor_analysis_rotation_choices()
  names(choices)[match(rotation, choices)] %||% rotation
}

factor_analysis_criterion_label <- function(criterion, n_factors = NULL) {
  if (identical(criterion, "fixed")) {
    return(sprintf("Fixed number of factors: %s", as.integer(n_factors %||% 1L)))
  }
  "Eigenvalue >= 1.0"
}

factor_analysis_matrix_label <- function(matrix_type) {
  choices <- factor_analysis_matrix_choices()
  names(choices)[match(matrix_type, choices)] %||% matrix_type
}

factor_analysis_normality_method_label <- function(method) {
  choices <- factor_analysis_normality_method_choices()
  names(choices)[match(method, choices)] %||% method
}

factor_analysis_measurements_for <- function(variables, variable_info = NULL) {
  measurements <- character(0)
  if (!is.null(variable_info) && all(c("name", "measurement") %in% names(variable_info))) {
    measurements <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  }
  out <- vapply(as.character(variables), function(name) named_value(measurements, name, ""), character(1))
  out[out == "ordinal"] <- "ordered"
  out[out == "nominal"] <- "category"
  out
}

factor_analysis_numeric_matrix <- function(data, variables) {
  frame <- data[, variables, drop = FALSE]
  matrix <- as.data.frame(lapply(frame, function(values) suppressWarnings(as.numeric(values))), check.names = FALSE)
  names(matrix) <- variables
  matrix
}

factor_analysis_complete_matrix <- function(matrix) {
  if (!is.data.frame(matrix) || ncol(matrix) == 0) {
    return(matrix[0, , drop = FALSE])
  }
  matrix[stats::complete.cases(matrix), , drop = FALSE]
}

factor_analysis_warning_table <- function(messages) {
  analysis_warning_table(messages)
}

factor_analysis_sample_warnings <- function(n_obs, n_variables, analysis = "analysis") {
  messages <- character(0)
  if (is.finite(n_obs) && n_obs < 100) {
    messages <- c(messages, sprintf("Sample size is N=%d. A common rule of thumb recommends N >= 100 for %s.", n_obs, analysis))
  }
  ratio <- if (is.finite(n_variables) && n_variables > 0) n_obs / n_variables else NA_real_
  if (is.finite(ratio) && ratio < 5) {
    messages <- c(messages, sprintf("The subject-to-variable ratio is %.1f:1. A common rule of thumb recommends at least 5:1, preferably 10:1.", ratio))
  } else if (is.finite(ratio) && ratio < 10) {
    messages <- c(messages, sprintf("The subject-to-variable ratio is %.1f:1. Interpret the solution cautiously; 10:1 is often recommended.", ratio))
  }
  messages
}

factor_analysis_correlation_matrix <- function(complete, measurements, matrix_type = "pearson") {
  complete <- as.data.frame(complete, check.names = FALSE)
  measurements <- as.character(measurements %||% character(0))
  matrix_type <- as.character(matrix_type %||% "pearson")
  warnings <- character(0)
  has_ordered <- any(measurements == "ordered", na.rm = TRUE)
  has_continuous <- any(measurements == "continuous", na.rm = TRUE)

  pearson_matrix <- function() {
    corr <- stats::cor(complete, use = "pairwise.complete.obs")
    diag(corr) <- 1
    corr
  }

  if (identical(matrix_type, "polychoric")) {
    if (has_continuous) {
      warnings <- c(warnings, "Polychoric correlation was requested, but continuous variables are included. Pearson correlation was used for this mixed variable set.")
      return(list(matrix = pearson_matrix(), matrix_type = "pearson", requested_matrix_type = "polychoric", warnings = warnings))
    }
    ordered_complete <- as.data.frame(lapply(complete, function(values) ordered(values)), check.names = FALSE)
    corr <- tryCatch(
      suppressWarnings(suppressMessages(psych::polychoric(ordered_complete, correct = 0)$rho)),
      error = function(e) {
        warnings <<- c(warnings, sprintf("Polychoric correlation could not be estimated (%s). Pearson correlation was used instead.", conditionMessage(e)))
        NULL
      }
    )
    if (is.matrix(corr) && all(is.finite(corr))) {
      diag(corr) <- 1
      return(list(matrix = corr, matrix_type = "polychoric", requested_matrix_type = "polychoric", warnings = warnings))
    }
    return(list(matrix = pearson_matrix(), matrix_type = "pearson", requested_matrix_type = "polychoric", warnings = warnings))
  }

  if (isTRUE(has_ordered)) {
    warnings <- c(warnings, "Ordinal variables are included. Pearson correlation is allowed, but polychoric correlation is recommended for ordinal item sets.")
  }
  list(matrix = pearson_matrix(), matrix_type = "pearson", requested_matrix_type = "pearson", warnings = warnings)
}

factor_analysis_display_names <- function(variables, variable_info = NULL, labels = character(0), category_table = NULL) {
  stats::setNames(
    vapply(variables, correlation_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    variables
  )
}

factor_analysis_normality_table <- function(matrix, variables, display_names) {
  rows <- lapply(variables, function(name) {
    values <- suppressWarnings(as.numeric(matrix[[name]]))
    values <- values[!is.na(values)]
    skewness <- sample_skewness(values)
    kurtosis <- sample_excess_kurtosis(values)
    normal <- is.finite(skewness) && is.finite(kurtosis) && abs(skewness) < 2 && abs(kurtosis) < 7
    data.frame(
      Variable = display_names[[name]] %||% name,
      N = length(values),
      Skewness = format_decimal3(skewness),
      Kurtosis = format_decimal3(kurtosis),
      Normality = if (is.finite(skewness) && is.finite(kurtosis)) {
        if (isTRUE(normal)) "Satisfied" else "Not satisfied"
      } else {
        "Not tested"
      },
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

factor_analysis_normality_satisfied <- function(normality_table) {
  is.data.frame(normality_table) &&
    nrow(normality_table) > 0 &&
    "Normality" %in% names(normality_table) &&
    all(as.character(normality_table$Normality) == "Satisfied")
}

factor_analysis_mardia_table <- function(complete) {
  complete <- as.data.frame(complete, check.names = FALSE)
  n_obs <- nrow(complete)
  n_vars <- ncol(complete)
  if (n_obs < 5 || n_vars < 2) {
    return(data.frame(
      Check = "Mardia test",
      Statistic = "",
      df = "",
      p = "",
      Normality = "Not tested",
      check.names = FALSE
    ))
  }
  centered <- scale(as.matrix(complete), center = TRUE, scale = FALSE)
  cov_matrix <- stats::cov(centered)
  inv_cov <- tryCatch(solve(cov_matrix), error = function(e) NULL)
  if (is.null(inv_cov) || any(!is.finite(inv_cov))) {
    return(data.frame(
      Check = "Mardia test",
      Statistic = "",
      df = "",
      p = "",
      Normality = "Not tested",
      check.names = FALSE
    ))
  }

  distances <- centered %*% inv_cov %*% t(centered)
  skewness <- mean(distances^3)
  skewness_df <- n_vars * (n_vars + 1) * (n_vars + 2) / 6
  skewness_chisq <- n_obs * skewness / 6
  skewness_p <- stats::pchisq(skewness_chisq, df = skewness_df, lower.tail = FALSE)

  squared_distances <- diag(distances)
  kurtosis <- mean(squared_distances^2)
  kurtosis_z <- (kurtosis - n_vars * (n_vars + 2)) / sqrt(8 * n_vars * (n_vars + 2) / n_obs)
  kurtosis_p <- 2 * stats::pnorm(abs(kurtosis_z), lower.tail = FALSE)

  normal <- is.finite(skewness_p) && is.finite(kurtosis_p) && skewness_p >= 0.05 && kurtosis_p >= 0.05
  data.frame(
    Check = c("Mardia skewness", "Mardia kurtosis"),
    Statistic = c(format_decimal3(skewness_chisq), format_decimal3(kurtosis_z)),
    df = c(format_decimal3(skewness_df), ""),
    p = c(format_p(skewness_p), format_p(kurtosis_p)),
    Normality = if (isTRUE(normal)) "Satisfied" else "Not satisfied",
    check.names = FALSE
  )
}

factor_analysis_suitability_tables <- function(corr, n_obs) {
  kmo <- suppressWarnings(suppressMessages(tryCatch(psych::KMO(corr), error = function(e) NULL)))
  bartlett <- suppressWarnings(suppressMessages(tryCatch(psych::cortest.bartlett(corr, n = n_obs), error = function(e) NULL)))
  overview <- data.frame(
    Check = c("KMO", "Bartlett's test of sphericity"),
    Value = c(
      if (!is.null(kmo) && is.finite(kmo$MSA)) format_decimal3(kmo$MSA) else "",
      if (!is.null(bartlett) && is.finite(bartlett$chisq)) format_decimal3(bartlett$chisq) else ""
    ),
    df = c("", if (!is.null(bartlett) && is.finite(bartlett$df)) format_decimal3(bartlett$df) else ""),
    p = c("", if (!is.null(bartlett) && is.finite(bartlett$p.value)) format_p(bartlett$p.value) else ""),
    check.names = FALSE
  )
  list(overview = overview, kmo = kmo, bartlett = bartlett)
}

factor_analysis_model_df <- function(n_variables, n_factors) {
  ((n_variables - n_factors)^2 - n_variables - n_factors) / 2
}

factor_analysis_max_factor_count <- function(n_variables) {
  n_variables <- as.integer(n_variables %||% 0L)
  if (n_variables < 3) {
    return(1L)
  }
  candidates <- seq_len(max(1L, n_variables - 1L))
  identified <- candidates[vapply(candidates, function(n_factors) {
    factor_analysis_model_df(n_variables, n_factors) >= 0
  }, logical(1))]
  if (length(identified) == 0) {
    return(1L)
  }
  max(1L, max(identified))
}

factor_analysis_select_factor_count <- function(eigenvalues, criterion, requested_n = 1L) {
  max_factors <- factor_analysis_max_factor_count(length(eigenvalues))
  if (identical(criterion, "fixed")) {
    return(min(max(1L, as.integer(requested_n %||% 1L)), max_factors))
  }
  selected <- sum(is.finite(eigenvalues) & eigenvalues >= 1)
  min(max(1L, selected), max_factors)
}

factor_analysis_high_correlation_pairs <- function(corr, display_names, threshold = 0.98, max_pairs = 5L) {
  if (!is.matrix(corr) || ncol(corr) < 2) {
    return(character(0))
  }
  pairs <- which(abs(corr) >= threshold & upper.tri(corr), arr.ind = TRUE)
  if (!is.matrix(pairs) || nrow(pairs) == 0) {
    return(character(0))
  }
  values <- abs(corr[pairs])
  pairs <- pairs[order(values, decreasing = TRUE), , drop = FALSE]
  pairs <- pairs[seq_len(min(nrow(pairs), max_pairs)), , drop = FALSE]
  vapply(seq_len(nrow(pairs)), function(index) {
    row_name <- rownames(corr)[pairs[index, "row"]]
    col_name <- colnames(corr)[pairs[index, "col"]]
    sprintf(
      "%s-%s (r=%s)",
      display_names[[row_name]] %||% row_name,
      display_names[[col_name]] %||% col_name,
      format_decimal3(corr[pairs[index, "row"], pairs[index, "col"]])
    )
  }, character(1))
}

factor_analysis_fit_error_message <- function(error, n_factors, n_variables, n_obs, method, rotation, eigenvalues, corr, display_names) {
  min_eigen <- suppressWarnings(min(eigenvalues, na.rm = TRUE))
  small_eigen_count <- sum(is.finite(eigenvalues) & eigenvalues < 1e-6)
  high_pairs <- factor_analysis_high_correlation_pairs(corr, display_names)
  details <- c(
    sprintf("Requested factors: %d; variables: %d; complete cases: %d.", n_factors, n_variables, n_obs),
    sprintf("Method: %s; rotation: %s.", factor_analysis_method_label(method), factor_analysis_rotation_label(rotation)),
    sprintf("Model df: %s.", format_decimal3(factor_analysis_model_df(n_variables, n_factors))),
    if (is.finite(min_eigen)) sprintf("Smallest eigenvalue: %s.", format_decimal3(min_eigen)) else "",
    if (small_eigen_count > 0) sprintf("%d eigenvalue(s) are near zero, suggesting a singular or unstable correlation matrix.", small_eigen_count) else "",
    if (length(high_pairs) > 0) sprintf("Very highly correlated variable pairs: %s.", paste(high_pairs, collapse = "; ")) else ""
  )
  details <- details[nzchar(details)]
  paste(
    "Factor analysis could not be estimated. The requested number of factors may be too large, or the correlation matrix may be unstable.",
    paste(details, collapse = " "),
    sprintf("Original error: %s", conditionMessage(error))
  )
}

factor_analysis_overview_table <- function(result) {
  table <- data.frame(
    N = result$n_obs,
    Variables = length(result$variables),
    Factors = result$n_factors,
    Matrix = factor_analysis_matrix_label(result$matrix_type %||% "pearson"),
    Method = factor_analysis_method_label(result$method),
    Rotation = factor_analysis_rotation_label(result$rotation),
    Criterion = factor_analysis_criterion_label(result$criterion, result$n_factors),
    check.names = FALSE
  )
  if (isTRUE(result$options$normality)) {
    table$`Normality check` <- factor_analysis_normality_method_label(result$normality_method)
    table$Normality <- if (isTRUE(result$normality_decision)) "Satisfied" else "Not satisfied"
  }
  table
}

factor_analysis_factor_order <- function(names) {
  names <- as.character(names %||% character(0))
  if (length(names) == 0) return(integer(0))
  numeric_suffix <- suppressWarnings(as.integer(sub("^.*?(\\d+)$", "\\1", names)))
  prefix <- sub("\\d+$", "", names)
  if (all(is.finite(numeric_suffix))) {
    return(order(prefix, numeric_suffix, names, na.last = TRUE))
  }
  order(names, na.last = TRUE)
}

factor_analysis_order_factor_names <- function(names) {
  names <- as.character(names %||% character(0))
  names[factor_analysis_factor_order(names)]
}

factor_analysis_reorder_factor_matrix <- function(matrix) {
  if (!is.matrix(matrix) || ncol(matrix) == 0) return(matrix)
  order_index <- factor_analysis_factor_order(colnames(matrix))
  matrix[, order_index, drop = FALSE]
}

factor_analysis_loading_table <- function(result, cutoff = 0.30) {
  loadings <- factor_analysis_reorder_factor_matrix(result$loadings)
  if (!is.matrix(loadings) || nrow(loadings) == 0) {
    return(NULL)
  }
  hide_small_loadings <- isTRUE(result$options$hide_small_loadings %||% TRUE)
  highlight_problem_values <- isTRUE(result$options$highlight_problem_values %||% TRUE)
  sort_loadings <- isTRUE(result$options$sort_loadings %||% TRUE)
  loading_abs <- abs(loadings)
  primary_factor <- max.col(loading_abs, ties.method = "first")
  primary_loading <- loading_abs[cbind(seq_len(nrow(loading_abs)), primary_factor)]
  if (isTRUE(sort_loadings)) {
    row_order <- order(primary_factor, -primary_loading, rownames(loadings), na.last = TRUE)
    loadings <- loadings[row_order, , drop = FALSE]
    loading_abs <- abs(loadings)
    primary_factor <- max.col(loading_abs, ties.method = "first")
    primary_loading <- loading_abs[cbind(seq_len(nrow(loading_abs)), primary_factor)]
  }
  factors <- colnames(loadings)
  table <- data.frame(Variable = result$display_names[rownames(loadings)], check.names = FALSE)
  for (factor_index in seq_along(factors)) {
    factor <- factors[[factor_index]]
    table[[factor]] <- vapply(seq_len(nrow(loadings)), function(row_index) {
      value <- loadings[row_index, factor_index]
      low_primary <- isTRUE(highlight_problem_values) &&
        primary_factor[[row_index]] == factor_index &&
        is.finite(primary_loading[[row_index]]) &&
        primary_loading[[row_index]] < cutoff
      if (!is.finite(value) || (isTRUE(hide_small_loadings) && abs(value) < cutoff && !isTRUE(low_primary))) "" else format_decimal3(value)
    }, character(1))
  }
  table$`h²` <- vapply(result$communality[rownames(loadings)], format_decimal3, character(1))
  table$Complexity <- vapply(result$complexity[rownames(loadings)], format_decimal3, character(1))
  table <- factor_analysis_loading_reliability_columns(result, table, rownames(loadings), primary_factor, primary_loading, cutoff = cutoff)
  cell_styles <- factor_analysis_problem_cell_styles(result, rownames(loadings), table, loadings, cutoff = cutoff)
  if (!isTRUE(hide_small_loadings)) {
    bold_cells <- do.call(rbind, lapply(seq_along(factors), function(column_index) {
      rows <- which(abs(loadings[, factors[[column_index]]]) >= cutoff)
      if (length(rows) == 0) {
        return(NULL)
      }
      data.frame(row = rows, column = factors[[column_index]], stringsAsFactors = FALSE)
    }))
    attr(table, "bold_cells") <- bold_cells
  }
  table <- factor_analysis_append_loading_summary_rows(result, table, factors)
  summary_styles <- factor_analysis_loading_summary_styles(table)
  attr(table, "cell_styles") <- rbind(cell_styles, summary_styles)
  table
}

factor_analysis_variance_value <- function(accounted, row_name, factor) {
  if (!is.matrix(accounted) && !is.data.frame(accounted)) {
    return("")
  }
  accounted <- as.data.frame(accounted, check.names = FALSE)
  if (!row_name %in% rownames(accounted) || !factor %in% names(accounted)) {
    return("")
  }
  value <- suppressWarnings(as.numeric(accounted[row_name, factor]))
  if (length(value) == 0 || !is.finite(value)) "" else format_decimal3(value)
}

factor_analysis_append_loading_summary_rows <- function(result, table, factors) {
  accounted <- result$fit$Vaccounted
  if ((!is.matrix(accounted) && !is.data.frame(accounted)) || length(factors) == 0) {
    return(table)
  }
  summary_specs <- list(
    list(label = "Eigenvalue", row = "SS loadings", multiplier = 1),
    list(label = "Variance %", row = "Proportion Var", multiplier = 100),
    list(label = "Cumulative variance %", row = "Cumulative Var", multiplier = 100)
  )
  rows <- lapply(summary_specs, function(spec) {
    row <- as.list(stats::setNames(rep("", ncol(table)), names(table)))
    row$Variable <- spec$label
    for (factor in factors) {
      value <- factor_analysis_variance_value(accounted, spec$row, factor)
      if (nzchar(value) && !identical(spec$multiplier, 1)) {
        numeric_value <- suppressWarnings(as.numeric(value))
        value <- if (is.finite(numeric_value)) format_decimal3(numeric_value * spec$multiplier) else ""
      }
      row[[factor]] <- value
    }
    if (identical(spec$label, "Eigenvalue") && "Reliability" %in% names(table)) {
      row$Reliability <- factor_analysis_reliability_overview_value(result$subfactor_reliability$total)
    }
    as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  })
  suitability_row <- factor_analysis_loading_suitability_row(result, table)
  if (!is.null(suitability_row)) {
    rows <- c(rows, list(suitability_row))
  }
  out <- rbind(table, do.call(rbind, rows))
  attr(out, "factor_loading_summary_start") <- nrow(table) + 1L
  if (!is.null(suitability_row)) {
    span <- attr(suitability_row, "spanning_cells", exact = TRUE)
    if (is.data.frame(span) && nrow(span) > 0) {
      span$row <- nrow(out)
      attr(out, "spanning_cells") <- span
    }
  }
  out
}

factor_analysis_column_key <- function(name) {
  name <- gsub("\u00B2", "2", as.character(name %||% ""), fixed = TRUE)
  gsub("[^[:alnum:]]+", "", tolower(name))
}

factor_analysis_loading_suitability_row <- function(result, table) {
  columns <- names(table)
  factors <- intersect(factor_analysis_order_factor_names(colnames(result$loadings)), columns)
  start_column <- if (length(factors) > 0) factors[[1]] else ""
  h2_index <- match("h2", vapply(columns, factor_analysis_column_key, character(1)))
  end_column <- if (!is.na(h2_index)) columns[[h2_index]] else if (length(factors) > 0) factors[[1]] else start_column
  if (!nzchar(start_column) || !start_column %in% columns || !end_column %in% columns) {
    return(NULL)
  }
  suitability <- result$suitability %||% list()
  kmo <- suitability$kmo
  bartlett <- suitability$bartlett
  kmo_value <- if (!is.null(kmo) && is.finite(kmo$MSA)) format_decimal3(kmo$MSA) else ""
  bartlett_value <- if (!is.null(bartlett) && is.finite(bartlett$chisq)) format_decimal3(bartlett$chisq) else ""
  bartlett_p <- if (!is.null(bartlett) && is.finite(bartlett$p.value)) format_p(bartlett$p.value) else ""
  if (!nzchar(kmo_value) && !nzchar(bartlett_value)) {
    return(NULL)
  }
  row <- as.list(stats::setNames(rep("", length(columns)), columns))
  row[[start_column]] <- if (nzchar(bartlett_p)) {
    sprintf("KMO= %s     Bartlett's x2 (p)= %s (%s)", kmo_value, bartlett_value, bartlett_p)
  } else {
    sprintf("KMO= %s     Bartlett's x2 (p)= %s", kmo_value, bartlett_value)
  }
  out <- as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  attr(out, "spanning_cells") <- data.frame(
    row = 1L,
    start_column = start_column,
    end_column = end_column,
    value = row[[start_column]],
    style = "border-top:2px solid #1f2937;text-align:center;",
    stringsAsFactors = FALSE
  )
  out
}

factor_analysis_loading_summary_styles <- function(table) {
  start <- attr(table, "factor_loading_summary_start", exact = TRUE)
  if (length(start) == 0 || is.null(start) || !is.finite(start) || start > nrow(table)) {
    return(data.frame(row = integer(0), column = character(0), style = character(0), stringsAsFactors = FALSE))
  }
  spans <- attr(table, "spanning_cells", exact = TRUE)
  spanning_rows <- if (is.data.frame(spans) && "row" %in% names(spans)) as.integer(spans$row) else integer(0)
  rows <- seq.int(start, nrow(table))
  do.call(rbind, lapply(rows, function(row_index) {
    data.frame(
      row = row_index,
      column = names(table),
      style = paste0(
        if (identical(row_index, start)) {
          "border-top:2px solid #1f2937;"
        } else if (row_index %in% spanning_rows) {
          "border-top:2px solid #1f2937;"
        } else {
          ""
        },
        "font-weight:600;background:#f8fafc;"
      ),
      stringsAsFactors = FALSE
    )
  }))
}

factor_analysis_structure_table <- function(result, cutoff = 0.30) {
  loadings <- factor_analysis_reorder_factor_matrix(result$loadings)
  phi <- result$fit$Phi
  if (!is.matrix(loadings) || nrow(loadings) == 0 || !is.matrix(phi) || nrow(phi) == 0) {
    return(NULL)
  }
  if (ncol(loadings) != nrow(phi)) {
    return(NULL)
  }
  factor_names <- colnames(loadings)
  if (!is.null(rownames(phi)) && !is.null(colnames(phi)) && all(factor_names %in% rownames(phi)) && all(factor_names %in% colnames(phi))) {
    phi <- phi[factor_names, factor_names, drop = FALSE]
  }
  structure <- loadings %*% phi
  rownames(structure) <- rownames(loadings)
  colnames(structure) <- colnames(loadings)

  loading_abs <- abs(loadings)
  primary_factor <- max.col(loading_abs, ties.method = "first")
  primary_loading <- loading_abs[cbind(seq_len(nrow(loading_abs)), primary_factor)]
  if (isTRUE(result$options$sort_loadings %||% TRUE)) {
    row_order <- order(primary_factor, -primary_loading, rownames(loadings), na.last = TRUE)
    structure <- structure[row_order, , drop = FALSE]
  }

  hide_small_loadings <- isTRUE(result$options$hide_small_loadings %||% TRUE)
  factors <- colnames(structure)
  table <- data.frame(Variable = result$display_names[rownames(structure)], check.names = FALSE)
  for (factor in factors) {
    table[[factor]] <- vapply(structure[, factor], function(value) {
      if (!is.finite(value) || (isTRUE(hide_small_loadings) && abs(value) < cutoff)) "" else format_decimal3(value)
    }, character(1))
  }
  if (!isTRUE(hide_small_loadings)) {
    bold_cells <- do.call(rbind, lapply(factors, function(factor) {
      rows <- which(abs(structure[, factor]) >= cutoff)
      if (length(rows) == 0) {
        return(NULL)
      }
      data.frame(row = rows, column = factor, stringsAsFactors = FALSE)
    }))
    attr(table, "bold_cells") <- bold_cells
  }
  table
}

factor_analysis_reliability_overview_value <- function(result) {
  table <- result$overview
  if (!is.data.frame(table) || nrow(table) == 0) {
    return("")
  }
  columns <- c("Cronbach's alpha", "Ordinal alpha", "Reliability")
  column <- intersect(columns, names(table))[1] %||% character(0)
  if (!length(column) || !nzchar(column)) {
    return("")
  }
  as.character(table[[column]][[1]] %||% "")
}

factor_analysis_reliability_deleted_column <- function(table) {
  columns <- c("Cronbach's alpha if item deleted", "Ordinal alpha if item deleted", "Reliability if item deleted")
  intersect(columns, names(table))[1] %||% character(0)
}

factor_analysis_loading_reliability_columns <- function(result, table, row_names, primary_factor, primary_loading, cutoff = 0.30) {
  if (!isTRUE(result$options$subfactor_reliability %||% FALSE)) {
    return(table)
  }
  table$Reliability <- ""
  table$`Reliability if deleted` <- ""
  table$`Item-total r` <- ""

  reliability <- result$subfactor_reliability
  factors <- reliability$factors %||% list()
  if (length(factors) == 0) {
    return(table)
  }
  factor_map <- stats::setNames(factors, vapply(factors, function(item) item$subfactor %||% "", character(1)))
  shown_reliability <- character(0)
  factor_names <- colnames(result$loadings)
  for (row_index in seq_along(row_names)) {
    subfactor <- factor_names[[primary_factor[[row_index]]]]
    item <- factor_map[[subfactor]]
    if (is.null(item) || !is.finite(primary_loading[[row_index]]) || primary_loading[[row_index]] < cutoff) {
      next
    }
    if (!subfactor %in% shown_reliability) {
      table$Reliability[[row_index]] <- factor_analysis_reliability_overview_value(item)
      shown_reliability <- c(shown_reliability, subfactor)
    }
    diagnostics <- item$item_diagnostics
    item_row <- match(row_names[[row_index]], item$variables %||% character(0))
    if (!is.data.frame(diagnostics) || is.na(item_row) || item_row > nrow(diagnostics)) {
      next
    }
    deleted_column <- factor_analysis_reliability_deleted_column(diagnostics)
    if (length(deleted_column) == 1L && nzchar(deleted_column)) {
      table$`Reliability if deleted`[[row_index]] <- as.character(diagnostics[[deleted_column]][[item_row]] %||% "")
    }
    if ("Corrected item-total correlation" %in% names(diagnostics)) {
      table$`Item-total r`[[row_index]] <- as.character(diagnostics$`Corrected item-total correlation`[[item_row]] %||% "")
    }
  }
  table
}

factor_analysis_problem_cell_styles <- function(result, row_names, table, loadings = NULL, cutoff = 0.30) {
  if (length(row_names) == 0 || !is.data.frame(table) || nrow(table) == 0) {
    return(data.frame(row = integer(0), column = character(0), style = character(0), stringsAsFactors = FALSE))
  }
  if (!isTRUE(result$options$highlight_problem_values %||% TRUE)) {
    return(data.frame(row = integer(0), column = character(0), style = character(0), stringsAsFactors = FALSE))
  }
  problem_style <- "color:#991b1b;font-weight:700;background:#fee2e2;"
  h2_values <- suppressWarnings(as.numeric(result$communality[row_names]))
  complexity_values <- suppressWarnings(as.numeric(result$complexity[row_names]))
  rows <- list()
  if (is.matrix(loadings) && nrow(loadings) == nrow(table) && ncol(loadings) > 0) {
    loading_abs <- abs(loadings)
    primary_factor <- max.col(loading_abs, ties.method = "first")
    primary_loading <- loading_abs[cbind(seq_len(nrow(loading_abs)), primary_factor)]
    factor_names <- colnames(loadings)
    low_primary_rows <- which(is.finite(primary_loading) & primary_loading < cutoff)
    if (length(low_primary_rows) > 0) {
      rows <- c(rows, list(data.frame(
        row = low_primary_rows,
        column = factor_names[primary_factor[low_primary_rows]],
        style = problem_style,
        stringsAsFactors = FALSE
      )))
    }
    cross_loading_rows <- do.call(rbind, lapply(seq_len(nrow(loadings)), function(row_index) {
      columns <- which(is.finite(loading_abs[row_index, ]) & loading_abs[row_index, ] >= cutoff & seq_len(ncol(loadings)) != primary_factor[[row_index]])
      if (length(columns) == 0) {
        return(NULL)
      }
      data.frame(row = row_index, column = factor_names[columns], style = problem_style, stringsAsFactors = FALSE)
    }))
    if (is.data.frame(cross_loading_rows) && nrow(cross_loading_rows) > 0) {
      rows <- c(rows, list(cross_loading_rows))
    }
  }
  h2_problem_rows <- which(is.finite(h2_values) & (h2_values < 0.30 | h2_values > 0.90 | h2_values > 1))
  if (length(h2_problem_rows) > 0 && "h²" %in% names(table)) {
    rows <- c(rows, list(data.frame(row = h2_problem_rows, column = "h²", style = problem_style, stringsAsFactors = FALSE)))
  }
  complexity_problem_rows <- which(is.finite(complexity_values) & complexity_values >= 2)
  if (length(complexity_problem_rows) > 0 && "Complexity" %in% names(table)) {
    rows <- c(rows, list(data.frame(row = complexity_problem_rows, column = "Complexity", style = problem_style, stringsAsFactors = FALSE)))
  }
  if (length(rows) == 0) {
    return(data.frame(row = integer(0), column = character(0), style = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

factor_analysis_variance_table <- function(result) {
  accounted <- result$fit$Vaccounted
  if (is.null(accounted)) {
    return(NULL)
  }
  table <- as.data.frame(accounted, check.names = FALSE)
  factor_columns <- factor_analysis_order_factor_names(names(table))
  table <- table[, factor_columns, drop = FALSE]
  table <- data.frame(Index = rownames(table), table, check.names = FALSE)
  for (column in setdiff(names(table), "Index")) {
    table[[column]] <- vapply(table[[column]], format_decimal3, character(1))
  }
  rownames(table) <- NULL
  table
}

factor_analysis_factor_correlation_table <- function(result) {
  phi <- result$fit$Phi
  if (!is.matrix(phi) || nrow(phi) == 0) {
    return(NULL)
  }
  factor_names <- factor_analysis_order_factor_names(colnames(phi))
  if (length(factor_names) > 0 && !is.null(rownames(phi)) && all(factor_names %in% rownames(phi))) {
    phi <- phi[factor_names, factor_names, drop = FALSE]
  }
  table <- as.data.frame(phi, check.names = FALSE)
  table <- data.frame(Factor = rownames(phi), table, check.names = FALSE)
  for (column in setdiff(names(table), "Factor")) {
    table[[column]] <- vapply(table[[column]], format_decimal3, character(1))
  }
  rownames(table) <- NULL
  table
}

factor_analysis_eigen_table <- function(result) {
  data.frame(
    Factor = seq_along(result$eigenvalues),
    Eigenvalue = vapply(result$eigenvalues, format_decimal3, character(1)),
    Selected = ifelse(seq_along(result$eigenvalues) <= result$n_factors, "Yes", ""),
    check.names = FALSE
  )
}

factor_analysis_display_variable <- function(result, variable) {
  result$display_names[[variable]] %||% variable
}

factor_analysis_reliability_item_issues <- function(result, subfactor, variables) {
  rows <- list()
  if (length(variables) == 0) {
    return(NULL)
  }
  for (variable in variables) {
    raw <- result$matrix[[variable]]
    numeric <- suppressWarnings(as.numeric(raw))
    missing_count <- sum(is.na(raw))
    numeric_missing_count <- sum(is.na(numeric))
    conversion_count <- max(0L, numeric_missing_count - missing_count)
    infinite_count <- sum(is.infinite(numeric), na.rm = TRUE)
    finite <- numeric[is.finite(numeric)]
    variance <- if (length(finite) >= 2) stats::var(finite) else NA_real_
    problems <- character(0)
    if (missing_count > 0) {
      problems <- c(problems, sprintf("%d missing value(s)", missing_count))
    }
    if (conversion_count > 0) {
      problems <- c(problems, sprintf("%d value(s) could not be converted to numeric", conversion_count))
    }
    if (infinite_count > 0) {
      problems <- c(problems, sprintf("%d infinite value(s)", infinite_count))
    }
    if (length(finite) < 2) {
      problems <- c(problems, "fewer than two finite numeric values")
    } else if (!is.finite(variance) || variance <= 0) {
      problems <- c(problems, "zero variance / constant item")
    }
    if (length(problems) > 0) {
      rows <- c(rows, list(data.frame(
        Subfactor = subfactor,
        Item = factor_analysis_display_variable(result, variable),
        Variable = variable,
        Problem = paste(unique(problems), collapse = "; "),
        check.names = FALSE
      )))
    }
  }
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

factor_analysis_reliability_group_reason <- function(result, subfactor, variables, error_message = NULL) {
  issue_table <- factor_analysis_reliability_item_issues(result, subfactor, variables)
  item_names <- vapply(variables, factor_analysis_display_variable, character(1), result = result)
  item_text <- paste(item_names, collapse = ", ")
  issue_text <- if (is.data.frame(issue_table) && nrow(issue_table) > 0) {
    paste(sprintf("%s: %s", issue_table$Item, issue_table$Problem), collapse = "; ")
  } else {
    ""
  }
  pieces <- c(
    if (nzchar(item_text)) sprintf("Items: %s", item_text) else "",
    issue_text,
    if (!is.null(error_message) && nzchar(error_message)) sprintf("Calculation error: %s", error_message) else ""
  )
  paste(pieces[nzchar(pieces)], collapse = " | ")
}

factor_analysis_subfactor_reliability <- function(result, cutoff = 0.30) {
  if (!isTRUE(result$options$subfactor_reliability %||% FALSE)) {
    return(NULL)
  }
  loadings <- result$loadings
  if (!is.matrix(loadings) || nrow(loadings) == 0 || ncol(loadings) == 0) {
    return(NULL)
  }
  loading_abs <- abs(loadings)
  primary_factor <- max.col(loading_abs, ties.method = "first")
  primary_loading <- loading_abs[cbind(seq_len(nrow(loading_abs)), primary_factor)]
  factor_names <- colnames(loadings)
  assignments <- data.frame(
    variable = rownames(loadings),
    subfactor = factor_names[primary_factor],
    primary_loading = primary_loading,
    stringsAsFactors = FALSE
  )
  assignments <- assignments[is.finite(assignments$primary_loading) & assignments$primary_loading >= cutoff, , drop = FALSE]
  groups <- split(assignments$variable, assignments$subfactor)
  reliability_options <- list(
    normality = FALSE,
    ordinal = FALSE,
    reliability_if_deleted = TRUE,
    item_total_correlation = TRUE
  )
  skipped <- list()
  item_issues <- list()
  factors <- list()
  total_variables <- result$variables[result$variables %in% assignments$variable]
  total <- NULL
  if (length(total_variables) >= 2) {
    total <- tryCatch(
      prepare_reliability_results(
        data = result$matrix,
        variables = total_variables,
        variable_info = result$variable_info,
        labels = result$labels,
        category_table = result$category_table,
        options = reliability_options
      ),
      error = function(e) {
        skipped <<- c(skipped, list(data.frame(
          Subfactor = "Total",
          Items = length(total_variables),
          Reason = factor_analysis_reliability_group_reason(result, "Total", total_variables, conditionMessage(e)),
          check.names = FALSE
        )))
        NULL
      }
    )
    if (!is.null(total)) {
      total$subfactor <- "Total"
      total$assigned_variables <- total_variables
    }
  } else {
    skipped <- c(skipped, list(data.frame(
      Subfactor = "Total",
      Items = length(total_variables),
      Reason = factor_analysis_reliability_group_reason(result, "Total", total_variables, "Fewer than two items with primary loading >= .30"),
      check.names = FALSE
    )))
  }
  for (subfactor in factor_names) {
    variables <- as.character(groups[[subfactor]] %||% character(0))
    issue_table <- factor_analysis_reliability_item_issues(result, subfactor, variables)
    if (is.data.frame(issue_table) && nrow(issue_table) > 0) {
      item_issues <- c(item_issues, list(issue_table))
    }
    if (length(variables) < 2) {
      skipped <- c(skipped, list(data.frame(
        Subfactor = subfactor,
        Items = length(variables),
        Reason = factor_analysis_reliability_group_reason(result, subfactor, variables, "Fewer than two items with primary loading >= .30"),
        check.names = FALSE
      )))
      next
    }
    item <- tryCatch(
      prepare_reliability_results(
        data = result$matrix,
        variables = variables,
        variable_info = result$variable_info,
        labels = result$labels,
        category_table = result$category_table,
        options = reliability_options
      ),
      error = function(e) {
        skipped <<- c(skipped, list(data.frame(
          Subfactor = subfactor,
          Items = length(variables),
          Reason = factor_analysis_reliability_group_reason(result, subfactor, variables, conditionMessage(e)),
          check.names = FALSE
        )))
        NULL
      }
    )
    if (!is.null(item)) {
      item$subfactor <- subfactor
      item$assigned_variables <- variables
      factors <- c(factors, list(item))
    }
  }
  list(
    type = "reliability_factors",
    total = total,
    factors = factors,
    skipped = if (length(skipped) > 0) do.call(rbind, skipped) else NULL,
    item_issues = if (length(item_issues) > 0) do.call(rbind, item_issues) else NULL,
    options = reliability_options,
    source = "factor_analysis",
    cutoff = cutoff
  )
}

factor_analysis_primary_assignments <- function(result, cutoff = 0.30) {
  loadings <- result$loadings
  if (!is.matrix(loadings) || nrow(loadings) == 0 || ncol(loadings) == 0) {
    return(data.frame(variable = character(0), factor = character(0), factor_index = integer(0), loading = numeric(0), stringsAsFactors = FALSE))
  }

  loading_abs <- abs(loadings)
  primary_factor <- max.col(loading_abs, ties.method = "first")
  primary_loading <- loading_abs[cbind(seq_len(nrow(loadings)), primary_factor)]
  keep <- is.finite(primary_loading) & primary_loading >= cutoff
  if (!any(keep)) {
    return(data.frame(variable = character(0), factor = character(0), factor_index = integer(0), loading = numeric(0), stringsAsFactors = FALSE))
  }

  data.frame(
    variable = rownames(loadings)[keep],
    factor = colnames(loadings)[primary_factor[keep]],
    factor_index = primary_factor[keep],
    loading = loadings[cbind(which(keep), primary_factor[keep])],
    stringsAsFactors = FALSE
  )
}

factor_analysis_row_mean <- function(values) {
  count <- rowSums(!is.na(values))
  out <- rowMeans(values, na.rm = TRUE)
  out[count == 0] <- NA_real_
  out
}

factor_analysis_row_sum <- function(values) {
  count <- rowSums(!is.na(values))
  out <- rowSums(values, na.rm = TRUE)
  out[count == 0] <- NA_real_
  out
}

factor_analysis_saved_score_name <- function(factor_index, kind, base_name = "FA") {
  prefix <- switch(
    as.character(kind),
    mean = "MF",
    sum = "SF",
    score = "FS",
    "FA"
  )
  base_name <- trimws(as.character(base_name %||% "FA"))
  if (!nzchar(base_name)) {
    base_name <- "FA"
  }
  base_name <- make.names(base_name)
  paste0(prefix, "_", base_name, as.integer(factor_index))
}

factor_analysis_factor_score_matrix <- function(result) {
  factors <- colnames(result$loadings)
  matrix <- result$matrix
  out <- matrix(NA_real_, nrow = nrow(matrix), ncol = length(factors))
  colnames(out) <- factors
  if (nrow(matrix) == 0 || length(factors) == 0) {
    return(out)
  }

  complete_rows <- stats::complete.cases(matrix[, result$variables, drop = FALSE])
  if (!any(complete_rows)) {
    return(out)
  }

  scores <- suppressWarnings(psych::factor.scores(
    x = as.matrix(matrix[complete_rows, result$variables, drop = FALSE]),
    f = result$fit,
    method = "Thurstone"
  )$scores)
  scores <- as.matrix(scores)
  if (ncol(scores) != length(factors)) {
    stop("Factor scores could not be matched to the estimated factors.", call. = FALSE)
  }
  colnames(scores) <- factors
  out[complete_rows, factors] <- scores[, factors, drop = FALSE]
  out
}

factor_analysis_saved_score_outputs <- function(
  result,
  include_means = FALSE,
  include_sums = FALSE,
  include_scores = FALSE,
  base_name = "FA",
  cutoff = 0.30
) {
  if (!isTRUE(include_means) && !isTRUE(include_sums) && !isTRUE(include_scores)) {
    return(data.frame(check.names = FALSE))
  }

  factors <- colnames(result$loadings)
  multiple <- length(factors) > 1
  out <- data.frame(row_id = seq_len(nrow(result$matrix)), check.names = FALSE)
  out$row_id <- NULL
  score_kinds <- character(0)
  append_score_output <- function(name, values, kind) {
    name <- make.unique(c(names(out), name), sep = "_")[[length(names(out)) + 1L]]
    out[[name]] <<- values
    score_kinds[[name]] <<- kind
  }

  if (isTRUE(include_means) || isTRUE(include_sums)) {
    assignments <- factor_analysis_primary_assignments(result, cutoff = cutoff)
    for (factor in factors) {
      variables <- assignments$variable[assignments$factor == factor]
      if (length(variables) == 0) {
        next
      }
      values <- as.matrix(result$matrix[, variables, drop = FALSE])
      if (isTRUE(include_means)) {
        append_score_output(
          factor_analysis_saved_score_name(match(factor, factors), "mean", base_name),
          factor_analysis_row_mean(values),
          "mean"
        )
      }
      if (isTRUE(include_sums)) {
        append_score_output(
          factor_analysis_saved_score_name(match(factor, factors), "sum", base_name),
          factor_analysis_row_sum(values),
          "sum"
        )
      }
    }
  }

  if (isTRUE(include_scores)) {
    scores <- factor_analysis_factor_score_matrix(result)
    for (factor in factors) {
      append_score_output(
        factor_analysis_saved_score_name(match(factor, factors), "score", base_name),
        scores[, factor],
        "score"
      )
    }
  }

  attr(out, "score_kinds") <- score_kinds
  out
}

prepare_factor_analysis_results <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  shiny::validate(shiny::need(length(variables) >= 3, "Select at least three variables for factor analysis."))

  measurements <- factor_analysis_measurements_for(variables, variable_info)
  allowed <- c("ordered", "continuous")
  shiny::validate(shiny::need(all(measurements %in% allowed), "Factor analysis accepts only ordinal or continuous variables."))

  matrix <- factor_analysis_numeric_matrix(data, variables)
  complete <- factor_analysis_complete_matrix(matrix)
  shiny::validate(shiny::need(nrow(complete) >= 5, "Not enough complete cases for factor analysis."))

  variable_sd <- vapply(complete, stats::sd, numeric(1), na.rm = TRUE)
  non_constant <- names(variable_sd)[is.finite(variable_sd) & variable_sd > 0]
  shiny::validate(shiny::need(length(non_constant) >= 3, "At least three non-constant variables are required."))
  variables <- intersect(variables, non_constant)
  matrix <- matrix[, variables, drop = FALSE]
  complete <- complete[, variables, drop = FALSE]

  matrix_type <- as.character(options$matrix_type %||% "pearson")
  if (!matrix_type %in% unname(factor_analysis_matrix_choices())) matrix_type <- "pearson"
  matrix_result <- factor_analysis_correlation_matrix(complete, measurements[variables], matrix_type)
  corr <- matrix_result$matrix
  shiny::validate(shiny::need(is.matrix(corr) && all(is.finite(corr)), "The correlation matrix could not be estimated."))
  diag(corr) <- 1
  eigenvalues <- eigen(corr, symmetric = TRUE, only.values = TRUE)$values
  warnings <- factor_analysis_warning_table(c(
    matrix_result$warnings,
    factor_analysis_sample_warnings(nrow(complete), length(variables), "factor analysis")
  ))

  display_names <- factor_analysis_display_names(variables, variable_info, labels, category_table)
  normality_method <- as.character(options$normality_method %||% "skew_kurt")
  if (!normality_method %in% unname(factor_analysis_normality_method_choices())) normality_method <- "skew_kurt"
  normality <- if (isTRUE(options$normality)) {
    if (identical(normality_method, "mardia")) {
      factor_analysis_mardia_table(complete)
    } else {
      factor_analysis_normality_table(matrix, variables, display_names)
    }
  } else {
    NULL
  }
  normality_decision <- if (isTRUE(options$normality)) factor_analysis_normality_satisfied(normality) else NULL

  requested_method <- as.character(options$method %||% "pa")
  if (!requested_method %in% unname(factor_analysis_method_choices())) requested_method <- "pa"
  method <- if (isTRUE(options$normality)) {
    if (isTRUE(normality_decision)) "ml" else "pa"
  } else {
    requested_method
  }
  rotation <- as.character(options$rotation %||% "varimax")
  if (!rotation %in% unname(factor_analysis_rotation_choices())) rotation <- "varimax"
  criterion <- as.character(options$criterion %||% "eigen")
  if (!criterion %in% unname(factor_analysis_criterion_choices())) criterion <- "eigen"
  requested_n_factors <- max(1L, as.integer(options$n_factors %||% 1L))
  max_factors <- factor_analysis_max_factor_count(length(eigenvalues))
  shiny::validate(shiny::need(
    !identical(criterion, "fixed") || requested_n_factors <= max_factors,
    sprintf(
      "Requested factor count (%d) is too high for %d variables. Use %d or fewer factors, or select more variables.",
      requested_n_factors,
      length(eigenvalues),
      max_factors
    )
  ))
  n_factors <- factor_analysis_select_factor_count(eigenvalues, criterion, requested_n_factors)

  fit <- tryCatch(
    suppressWarnings(suppressMessages(psych::fa(
      r = corr,
      nfactors = n_factors,
      n.obs = nrow(complete),
      rotate = rotation,
      fm = method,
      warnings = FALSE
    ))),
    error = function(e) {
      stop(
        factor_analysis_fit_error_message(e, n_factors, length(variables), nrow(complete), method, rotation, eigenvalues, corr, display_names),
        call. = FALSE
      )
    }
  )

  loadings <- as.matrix(unclass(fit$loadings))
  rownames(loadings) <- variables

  suitability <- factor_analysis_suitability_tables(corr, nrow(complete))

  result <- list(
    type = "factor_analysis",
    variables = variables,
    display_names = display_names,
    matrix = matrix,
    complete = complete,
    n_obs = nrow(complete),
    matrix_type = matrix_result$matrix_type,
    requested_matrix_type = matrix_result$requested_matrix_type,
    method = method,
    requested_method = requested_method,
    rotation = rotation,
    criterion = criterion,
    n_factors = n_factors,
    eigenvalues = eigenvalues,
    corr = corr,
    fit = fit,
    loadings = loadings,
    communality = fit$communality,
    uniqueness = fit$uniquenesses,
    complexity = fit$complexity,
    suitability = suitability,
    warnings = warnings,
    normality_table = normality,
    normality_method = normality_method,
    normality_decision = normality_decision,
    options = options,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table
  )
  result$overview <- factor_analysis_overview_table(result)
  result$subfactor_reliability <- factor_analysis_subfactor_reliability(result)
  result$loadings_table <- factor_analysis_loading_table(result)
  result$structure_table <- factor_analysis_structure_table(result)
  result$variance_table <- factor_analysis_variance_table(result)
  result$factor_correlation_table <- factor_analysis_factor_correlation_table(result)
  result$eigen_table <- factor_analysis_eigen_table(result)
  result
}
