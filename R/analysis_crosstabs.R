# Cross-tabulation helpers.

crosstab_allowed_measurements <- function() {
  c("binary", "ordered", "category")
}

crosstab_measurement_lookup <- function(variable_info = NULL) {
  if (is.null(variable_info) || !all(c("name", "measurement") %in% names(variable_info))) {
    return(character(0))
  }
  measurements <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  measurements[measurements == "ordinal"] <- "ordered"
  measurements[measurements == "nominal"] <- "category"
  measurements
}

crosstab_measurement <- function(name, variable_info = NULL) {
  lookup <- crosstab_measurement_lookup(variable_info)
  value <- named_value(lookup, name, "")
  if (value %in% crosstab_allowed_measurements()) value else ""
}

crosstab_variable_display_name <- function(name, variable_info = NULL, labels = character(0), category_table = NULL) {
  frequency_variable_display_name(name, variable_info, labels, category_table)
}

crosstab_value_labels <- function(name, values, category_table = NULL) {
  frequency_value_display_labels(name, values, category_table)
}

crosstab_order_values <- function(values, measurement = "", name = NULL, category_table = NULL) {
  values <- as.character(values)
  values <- values[!is.na(values)]
  unique_values <- unique(values)
  category_order <- frequency_category_value_order(name, category_table)
  category_order <- category_order[category_order %in% unique_values]
  if (length(category_order) > 0) {
    remaining <- unique_values[!unique_values %in% category_order]
    return(c(category_order, frequency_value_order(remaining)))
  }
  numeric_values <- suppressWarnings(as.numeric(unique_values))
  if (length(unique_values) > 0 && all(!is.na(numeric_values))) {
    return(unique_values[order(numeric_values)])
  }
  if (identical(measurement, "ordered")) {
    return(unique_values)
  }
  sort(unique_values)
}

crosstab_complete_table <- function(data, row_var, col_var, row_measure = "", col_measure = "", category_table = NULL) {
  row_values <- as.character(data[[row_var]])
  col_values <- as.character(data[[col_var]])
  complete <- !is.na(row_values) & !is.na(col_values)
  row_values <- row_values[complete]
  col_values <- col_values[complete]
  row_levels <- crosstab_order_values(row_values, row_measure, name = row_var, category_table = category_table)
  col_levels <- crosstab_order_values(col_values, col_measure, name = col_var, category_table = category_table)
  table(
    factor(row_values, levels = row_levels),
    factor(col_values, levels = col_levels),
    useNA = "no"
  )
}

crosstab_format_number <- function(value, digits = 3) {
  if (length(value) == 0 || is.na(value)) return("")
  formatC(as.numeric(value), format = "f", digits = digits)
}

crosstab_format_p <- function(value) {
  format_p(value)
}

crosstab_format_effect_size <- function(value) {
  format_effect_size(value)
}

crosstab_percent_matrix <- function(tab, margin = c("row", "column", "total")) {
  margin <- match.arg(margin)
  if (identical(margin, "row")) {
    denom <- rowSums(tab)
    return(sweep(tab, 1, ifelse(denom == 0, NA_real_, denom), "/") * 100)
  }
  if (identical(margin, "column")) {
    denom <- colSums(tab)
    return(sweep(tab, 2, ifelse(denom == 0, NA_real_, denom), "/") * 100)
  }
  tab / sum(tab) * 100
}

crosstab_display_table <- function(tab, row_var, col_var, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  row_labels <- crosstab_value_labels(row_var, rownames(tab), category_table)
  col_labels <- crosstab_value_labels(col_var, colnames(tab), category_table)
  row_percent <- crosstab_percent_matrix(tab, "row")
  col_percent <- crosstab_percent_matrix(tab, "column")
  total_percent <- crosstab_percent_matrix(tab, "total")
  show_total_n <- !identical(options$total_n, FALSE)
  format_percent <- function(values) {
    out <- rep("", length(values))
    present <- !is.na(values)
    out[present] <- formatC(as.numeric(values[present]), format = "f", digits = 1)
    matrix(out, nrow(values), ncol(values))
  }
  if (isTRUE(options$row_percent)) row_percent <- format_percent(row_percent)
  if (isTRUE(options$column_percent)) col_percent <- format_percent(col_percent)
  if (isTRUE(options$total_percent)) total_percent <- format_percent(total_percent)

  rows <- list()
  for (row_index in seq_len(nrow(tab))) {
    out <- list(Row = row_labels[[row_index]])
    for (col_index in seq_len(ncol(tab))) {
      pieces <- as.character(tab[row_index, col_index])
      if (isTRUE(options$row_percent)) {
        pieces <- c(pieces, paste0("row ", row_percent[row_index, col_index], "%"))
      }
      if (isTRUE(options$column_percent)) {
        pieces <- c(pieces, paste0("col ", col_percent[row_index, col_index], "%"))
      }
      if (isTRUE(options$total_percent)) {
        pieces <- c(pieces, paste0("total ", total_percent[row_index, col_index], "%"))
      }
      out[[col_labels[[col_index]]]] <- paste(pieces, collapse = "\n")
    }
    if (isTRUE(show_total_n)) {
      out[["Total"]] <- as.character(rowSums(tab)[[row_index]])
    }
    rows[[length(rows) + 1]] <- out
  }
  total_row <- c(list(Row = "Total"), stats::setNames(as.list(as.character(colSums(tab))), col_labels))
  if (isTRUE(show_total_n)) {
    total_row <- c(total_row, list(Total = as.character(sum(tab))))
  }
  rows[[length(rows) + 1]] <- total_row
  row_frame <- function(row) {
    # Values are already scalar strings. Preserve the general converter's
    # name handling for unusual labels instead of repairing them differently.
    if (anyNA(names(row)) || any(!nzchar(names(row))) || anyDuplicated(names(row))) {
      return(as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE))
    }
    list2DF(row)
  }
  do.call(rbind, lapply(rows, row_frame))
}

crosstab_chisq_result <- function(tab) {
  suppressWarnings(stats::chisq.test(tab, correct = FALSE))
}

crosstab_exact_selection <- function(expected, cell_count, threshold = 0.20, large_cell_count = 20) {
  low_count <- sum(expected < 5)
  low_percent <- if (length(expected) == 0) 0 else low_count / length(expected)
  use_exact <- low_percent >= threshold
  list(
    low_count = low_count,
    low_percent = low_percent,
    use_exact = use_exact,
    monte_carlo = isTRUE(use_exact) && cell_count > large_cell_count
  )
}

crosstab_association_test <- function(tab) {
  chisq <- crosstab_chisq_result(tab)
  exact <- crosstab_exact_selection(chisq$expected, length(tab))
  if (isTRUE(exact$use_exact)) {
    monte_carlo <- isTRUE(exact$monte_carlo)
    fisher <- tryCatch(
      stats::fisher.test(tab, simulate.p.value = monte_carlo, B = if (isTRUE(monte_carlo)) 10000 else 2000),
      error = function(e) {
        stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000)
      }
    )
    monte_carlo <- isTRUE(monte_carlo) || grepl("simulated", fisher$method, ignore.case = TRUE)
    method <- if (isTRUE(monte_carlo)) "Fisher's exact test with Monte Carlo simulation" else "Fisher's exact test"
    note <- sprintf(
      "Expected counts < 5 in %.1f%% of cells; %s was used.",
      exact$low_percent * 100,
      method
    )
    return(list(method = method, statistic = NA_real_, df = NA_real_, p = fisher$p.value, expected = chisq$expected, note = note))
  }
  list(
    method = "Pearson chi-square test",
    statistic = unname(chisq$statistic),
    df = unname(chisq$parameter),
    p = chisq$p.value,
    expected = chisq$expected,
    note = "Pearson chi-square test was used."
  )
}

crosstab_odds_ratio <- function(tab) {
  if (!all(dim(tab) == c(2, 2))) return(NA_real_)
  (tab[1, 1] * tab[2, 2]) / (tab[1, 2] * tab[2, 1])
}

crosstab_cramers_v <- function(tab) {
  chisq <- crosstab_chisq_result(tab)
  n <- sum(tab)
  denom <- n * (min(dim(tab)) - 1)
  if (denom <= 0) return(NA_real_)
  sqrt(unname(chisq$statistic) / denom)
}

crosstab_gamma <- function(tab) {
  lower_sums <- NULL
  # Integer counts make these cumulative sums exact. Preserve the original
  # arithmetic for custom/fractional inputs and possible integer overflow.
  if (is.matrix(tab) && is.numeric(tab) &&
      (identical(class(tab), "table") || identical(class(tab), c("matrix", "array"))) &&
      length(tab) >= 64L && length(tab) <= 1000000L &&
      all(is.finite(tab)) && all(tab >= 0) && all(tab == floor(tab))) {
    total <- sum(tab)
    if (total <= 94906265 && (!is.integer(tab) || max(tab) * as.double(total) <= .Machine$integer.max)) {
      lower_sums <- matrix(0, nrow(tab), ncol(tab))
      below <- numeric(ncol(tab))
      for (row in rev(seq_len(nrow(tab)))) {
        lower_sums[row, ] <- cumsum(below)
        below <- below + tab[row, ]
      }
    }
  }
  concordant <- 0
  discordant <- 0
  for (i in seq_len(nrow(tab))) {
    for (j in seq_len(ncol(tab))) {
      n_ij <- tab[i, j]
      if (n_ij == 0) next
      lower_rows <- if (i < nrow(tab)) seq.int(i + 1, nrow(tab)) else integer(0)
      higher_cols <- if (j < ncol(tab)) seq.int(j + 1, ncol(tab)) else integer(0)
      lower_cols <- if (j > 1) seq_len(j - 1) else integer(0)
      if (length(lower_rows) > 0 && length(higher_cols) > 0) {
        concordant <- concordant + n_ij * if (is.null(lower_sums)) {
          sum(tab[lower_rows, higher_cols, drop = FALSE])
        } else lower_sums[i, ncol(tab)] - lower_sums[i, j]
      }
      if (length(lower_rows) > 0 && length(lower_cols) > 0) {
        discordant <- discordant + n_ij * if (is.null(lower_sums)) {
          sum(tab[lower_rows, lower_cols, drop = FALSE])
        } else lower_sums[i, j - 1L]
      }
    }
  }
  denom <- concordant + discordant
  if (denom == 0) return(NA_real_)
  (concordant - discordant) / denom
}

crosstab_binary_row_index <- function(tab) {
  if (nrow(tab) != 2) return(NA_integer_)
  values <- suppressWarnings(as.numeric(rownames(tab)))
  if (all(!is.na(values))) {
    return(order(values, decreasing = TRUE)[[1]])
  }
  2L
}

crosstab_trend_or <- function(tab) {
  if (nrow(tab) == 2 && ncol(tab) >= 2) {
    event_row <- crosstab_binary_row_index(tab)
    scores <- seq_len(ncol(tab))
    events <- as.numeric(tab[event_row, ])
    totals <- colSums(tab)
  } else if (ncol(tab) == 2 && nrow(tab) >= 2) {
    event_col <- 2L
    scores <- seq_len(nrow(tab))
    events <- as.numeric(tab[, event_col])
    totals <- rowSums(tab)
  } else {
    return(NA_real_)
  }
  fit <- tryCatch(stats::glm(cbind(events, totals - events) ~ scores, family = stats::binomial()), error = function(e) NULL)
  if (is.null(fit) || length(stats::coef(fit)) < 2 || is.na(stats::coef(fit)[[2]])) return(NA_real_)
  unname(exp(stats::coef(fit)[[2]]))
}

crosstab_prop_trend_test <- function(x, n, score) {
  suppressWarnings(stats::prop.trend.test(x, n, score = score))
}

crosstab_trend_analysis <- function(tab, row_measure = "", col_measure = "") {
  if (nrow(tab) == 2 && ncol(tab) >= 2) {
    event_row <- crosstab_binary_row_index(tab)
    test <- crosstab_prop_trend_test(as.numeric(tab[event_row, ]), colSums(tab), score = seq_len(ncol(tab)))
    return(list(method = "Cochran-Armitage trend test", detail = "cochran_armitage", statistic = unname(test$statistic), df = unname(test$parameter), p = test$p.value, odds_ratio = crosstab_trend_or(tab), gamma = NA_real_))
  }
  if (ncol(tab) == 2 && nrow(tab) >= 2) {
    event_col <- 2L
    test <- crosstab_prop_trend_test(as.numeric(tab[, event_col]), rowSums(tab), score = seq_len(nrow(tab)))
    return(list(method = "Cochran-Armitage trend test", detail = "cochran_armitage", statistic = unname(test$statistic), df = unname(test$parameter), p = test$p.value, odds_ratio = crosstab_trend_or(tab), gamma = NA_real_))
  }
  if (identical(row_measure, "ordered") && identical(col_measure, "ordered")) {
    row_scores <- seq_len(nrow(tab))
    col_scores <- seq_len(ncol(tab))
    expanded <- as.data.frame(as.table(tab), stringsAsFactors = FALSE)
    expanded$row_score <- row_scores[match(expanded$Var1, rownames(tab))]
    expanded$col_score <- col_scores[match(expanded$Var2, colnames(tab))]
    # Keep cor() inputs in the original observation order without constructing
    # a repeated data frame and its unused unique row names.
    observation_index <- rep(seq_len(nrow(expanded)), expanded$Freq)
    row_values <- expanded$row_score[observation_index]
    col_values <- expanded$col_score[observation_index]
    observation_count <- length(observation_index)
    if (observation_count < 3) {
      return(list(method = "Score-based ordered-by-ordered trend association", detail = "ordered_score", statistic = NA_real_, df = 1, p = NA_real_, odds_ratio = NA_real_, gamma = crosstab_gamma(tab)))
    }
    r <- suppressWarnings(stats::cor(row_values, col_values))
    statistic <- (observation_count - 1) * r^2
    p <- stats::pchisq(statistic, df = 1, lower.tail = FALSE)
    return(list(method = "Score-based ordered-by-ordered trend association", detail = "ordered_score", statistic = statistic, df = 1, p = p, odds_ratio = NA_real_, gamma = crosstab_gamma(tab)))
  }
  NULL
}

crosstab_effect_size_table <- function(tab, trend = NULL) {
  rows <- list()
  if (all(dim(tab) == c(2, 2))) {
    rows[[length(rows) + 1]] <- data.frame(Effect = "Odds ratio", Estimate = crosstab_format_effect_size(crosstab_odds_ratio(tab)), stringsAsFactors = FALSE)
  }
  rows[[length(rows) + 1]] <- data.frame(Effect = "Cramer's V", Estimate = crosstab_format_effect_size(crosstab_cramers_v(tab)), stringsAsFactors = FALSE)
  if (!is.null(trend)) {
    if (!is.na(trend$odds_ratio)) {
      rows[[length(rows) + 1]] <- data.frame(Effect = "Trend odds ratio", Estimate = crosstab_format_effect_size(trend$odds_ratio), stringsAsFactors = FALSE)
    }
    if (!is.na(trend$gamma)) {
      rows[[length(rows) + 1]] <- data.frame(Effect = "Gamma", Estimate = crosstab_format_effect_size(trend$gamma), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

prepare_crosstab_results <- function(data, row_var, col_var, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  if (length(attr(data, "statedu_scope_excluded"))) analysis_scope_prepare_variables(data, environment(), c("row_var", "col_var"))
  row_var <- as.character(row_var %||% "")
  col_var <- as.character(col_var %||% "")
  shiny::validate(shiny::need(nzchar(row_var), "Select a row variable."))
  shiny::validate(shiny::need(nzchar(col_var), "Select a column variable."))
  shiny::validate(shiny::need(!identical(row_var, col_var), "Select different row and column variables."))
  shiny::validate(shiny::need(all(c(row_var, col_var) %in% names(data)), "Selected variables are not available in the loaded data."))

  row_measure <- crosstab_measurement(row_var, variable_info)
  col_measure <- crosstab_measurement(col_var, variable_info)
  shiny::validate(shiny::need(row_measure %in% crosstab_allowed_measurements(), "Row variable must be binary, ordered, or categorical."))
  shiny::validate(shiny::need(col_measure %in% crosstab_allowed_measurements(), "Column variable must be binary, ordered, or categorical."))

  tab <- crosstab_complete_table(data, row_var, col_var, row_measure, col_measure, category_table)
  shiny::validate(shiny::need(nrow(tab) >= 2 && ncol(tab) >= 2, "The crosstab must have at least 2 row levels and 2 column levels after excluding missing values."))

  association <- crosstab_association_test(tab)
  trend_requested <- isTRUE(options$trend)
  trend <- if (isTRUE(trend_requested)) crosstab_trend_analysis(tab, row_measure, col_measure) else NULL
  trend_note <- if (isTRUE(trend_requested) && is.null(trend)) {
    "Trend analysis was requested but was not available for this variable combination."
  } else {
    ""
  }

  list(
    row_var = row_var,
    col_var = col_var,
    row_label = crosstab_variable_display_name(row_var, variable_info, labels, category_table),
    col_label = crosstab_variable_display_name(col_var, variable_info, labels, category_table),
    row_measure = row_measure,
    col_measure = col_measure,
    table = tab,
    display_table = crosstab_display_table(tab, row_var, col_var, variable_info, labels, category_table, options),
    expected_table = as.data.frame.matrix(round(association$expected, 3), stringsAsFactors = FALSE),
    association = association,
    trend = trend,
    trend_requested = trend_requested,
    trend_note = trend_note,
    effect_sizes = crosstab_effect_size_table(tab, trend),
    options = options,
    data = data,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table
  )
}
