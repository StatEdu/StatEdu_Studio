# Paired/repeated-measures test analysis.

paired_measurement_lookup <- function(variable_info = NULL) {
  if (is.null(variable_info) || !all(c("name", "measurement") %in% names(variable_info))) {
    return(character(0))
  }
  values <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  values[values == "ordinal"] <- "ordered"
  values
}

paired_variable_measurement <- function(name, variable_info = NULL) {
  measurements <- paired_measurement_lookup(variable_info)
  value <- named_value(measurements, name, "continuous")
  if (value %in% c("binary", "category", "ordered", "continuous")) value else "continuous"
}

paired_display_name <- function(name, variable_info = NULL, labels = character(0), category_table = NULL) {
  correlation_variable_display_name(name, variable_info, labels, category_table)
}

paired_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

paired_complete_data <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  list(x = x[keep], y = y[keep], n = sum(keep))
}

paired_outlier_summary <- function(diff) {
  diff <- diff[is.finite(diff)]
  if (length(diff) < 4) {
    return("None detected")
  }
  q <- stats::quantile(diff, probs = c(.25, .75), na.rm = TRUE, names = FALSE)
  iqr <- q[[2]] - q[[1]]
  if (!is.finite(iqr) || iqr <= 0) {
    return("None detected")
  }
  lower <- q[[1]] - 3 * iqr
  upper <- q[[2]] + 3 * iqr
  count <- sum(diff < lower | diff > upper, na.rm = TRUE)
  if (count > 0) sprintf("%s detected", count) else "None detected"
}

paired_diff_assumption <- function(diff) {
  diff <- diff[is.finite(diff)]
  n <- length(diff)
  if (n == 0) {
    return(list(n = 0L, skewness = NA_real_, kurtosis = NA_real_, shapiro_p = NA_real_, normal = FALSE, outliers = "None detected", method = "none"))
  }
  skew <- if (n >= 3) as.numeric(psych::skew(diff, na.rm = TRUE, type = 2)) else NA_real_
  kurt <- if (n >= 4) as.numeric(psych::kurtosi(diff, na.rm = TRUE, type = 2)) else NA_real_
  sw <- if (n >= 3 && n <= 5000 && stats::sd(diff, na.rm = TRUE) > 0) {
    suppressWarnings(stats::shapiro.test(diff)$p.value)
  } else {
    NA_real_
  }
  if (n >= 30) {
    normal <- is.finite(skew) && is.finite(kurt) && abs(skew) < 2 && abs(kurt) < 7
    method <- "Skewness/Kurtosis"
  } else {
    normal <- is.finite(sw) && sw >= .05
    method <- "Shapiro-Wilk"
  }
  list(
    n = n,
    skewness = skew,
    kurtosis = kurt,
    shapiro_p = sw,
    normal = normal,
    outliers = paired_outlier_summary(diff),
    method = method
  )
}

paired_check_table <- function(pair_label, check) {
  if (is.null(check) || identical(check$method, "none")) return(NULL)
  data.frame(
    Pair = pair_label,
    `Check Result` = check$method,
    `Shapiro-Wilk p` = if (is.finite(check$shapiro_p)) format_p(check$shapiro_p) else "",
    Skewness = format_decimal3(check$skewness),
    Kurtosis = format_decimal3(check$kurtosis),
    Outliers = check$outliers,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

paired_mcnemar_exact <- function(table) {
  b <- as.numeric(table[1, 2])
  c <- as.numeric(table[2, 1])
  total <- b + c
  p <- if (total > 0) stats::binom.test(min(b, c), total, p = .5)$p.value else NA_real_
  list(statistic = NA_real_, df = NA_real_, p = p, b = b, c = c, discordant = total)
}

paired_mcnemar_asymptotic <- function(table) {
  b <- as.numeric(table[1, 2])
  c <- as.numeric(table[2, 1])
  test <- suppressWarnings(stats::mcnemar.test(table, correct = FALSE))
  list(statistic = unname(as.numeric(test$statistic)), df = 1, p = as.numeric(test$p.value), b = b, c = c, discordant = b + c)
}

paired_bowker_test <- function(table) {
  stat <- 0
  df <- 0
  k <- nrow(table)
  for (i in seq_len(k - 1L)) {
    for (j in seq.int(i + 1L, k)) {
      denom <- table[i, j] + table[j, i]
      if (denom > 0) {
        stat <- stat + (table[i, j] - table[j, i])^2 / denom
        df <- df + 1L
      }
    }
  }
  p <- if (df > 0) stats::pchisq(stat, df = df, lower.tail = FALSE) else NA_real_
  list(statistic = stat, df = df, p = p)
}

paired_stuart_maxwell_test <- function(table) {
  k <- nrow(table)
  if (k < 2) return(list(statistic = NA_real_, df = NA_real_, p = NA_real_))
  use <- seq_len(k - 1L)
  d <- rowSums(table)[use] - colSums(table)[use]
  v <- matrix(0, nrow = length(use), ncol = length(use))
  for (a in seq_along(use)) {
    i <- use[[a]]
    for (b in seq_along(use)) {
      j <- use[[b]]
      if (i == j) {
        v[a, b] <- rowSums(table)[i] + colSums(table)[i] - 2 * table[i, i]
      } else {
        v[a, b] <- -(table[i, j] + table[j, i])
      }
    }
  }
  inv <- tryCatch(solve(v), error = function(e) qr.solve(v))
  stat <- as.numeric(t(d) %*% inv %*% d)
  df <- k - 1L
  p <- stats::pchisq(stat, df = df, lower.tail = FALSE)
  list(statistic = stat, df = df, p = p)
}

paired_category_table <- function(x, y) {
  levels <- sort(unique(c(as.character(x), as.character(y))))
  x <- factor(as.character(x), levels = levels)
  y <- factor(as.character(y), levels = levels)
  table(x, y)
}

paired_analyze_pair <- function(data, first, second, measurement, variable_info, labels, category_table, options) {
  x_raw <- data[[first]]
  y_raw <- data[[second]]
  first_label <- paired_display_name(first, variable_info, labels, category_table)
  second_label <- paired_display_name(second, variable_info, labels, category_table)
  pair_label <- sprintf("%s - %s", second_label, first_label)

  if (identical(measurement, "continuous")) {
    pair <- paired_complete_data(paired_numeric(x_raw), paired_numeric(y_raw))
    diff <- pair$y - pair$x
    check <- paired_diff_assumption(diff)
    use_t <- !isTRUE(options$assumption_check) || isTRUE(check$normal)
    if (isTRUE(use_t)) {
      test <- suppressWarnings(stats::t.test(pair$y, pair$x, paired = TRUE))
      method <- "Paired t-test"
      statistic <- unname(as.numeric(test$statistic))
      df <- unname(as.numeric(test$parameter))
      p <- as.numeric(test$p.value)
    } else {
      test <- suppressWarnings(stats::wilcox.test(pair$y, pair$x, paired = TRUE, exact = FALSE))
      method <- "Wilcoxon signed-rank test"
      statistic <- unname(as.numeric(test$statistic))
      df <- NA_real_
      p <- as.numeric(test$p.value)
    }
    return(list(
      result = data.frame(
        Pair = pair_label,
        Level = "Continuous",
        Method = method,
        N = pair$n,
        Statistic = format_decimal3(statistic),
        df = if (is.finite(df)) format_decimal3(df) else "",
        p = format_p(p),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      check = paired_check_table(pair_label, check)
    ))
  }

  if (identical(measurement, "ordered")) {
    pair <- paired_complete_data(paired_numeric(x_raw), paired_numeric(y_raw))
    test <- suppressWarnings(stats::wilcox.test(pair$y, pair$x, paired = TRUE, exact = FALSE))
    return(list(
      result = data.frame(Pair = pair_label, Level = "Ordinal", Method = "Wilcoxon signed-rank test", N = pair$n, Statistic = format_decimal3(unname(as.numeric(test$statistic))), df = "", p = format_p(test$p.value), stringsAsFactors = FALSE, check.names = FALSE),
      check = NULL
    ))
  }

  pair <- paired_complete_data(as.character(x_raw), as.character(y_raw))
  tab <- paired_category_table(pair$x, pair$y)
  if (identical(measurement, "binary") && nrow(tab) == 2 && ncol(tab) == 2) {
    b <- as.numeric(tab[1, 2])
    c <- as.numeric(tab[2, 1])
    use_asymptotic <- (b + c) >= 25 && b > 5 && c > 5
    res <- if (use_asymptotic) paired_mcnemar_asymptotic(tab) else paired_mcnemar_exact(tab)
    method <- if (use_asymptotic) "McNemar test" else "Exact McNemar test"
    return(list(
      result = data.frame(Pair = pair_label, Level = "Binary", Method = method, N = pair$n, Statistic = format_decimal3(res$statistic), df = if (is.finite(res$df)) format_decimal3(res$df) else "", p = format_p(res$p), b = b, c = c, stringsAsFactors = FALSE, check.names = FALSE),
      check = NULL
    ))
  }

  res <- if (isTRUE(options$bowker)) paired_bowker_test(tab) else paired_stuart_maxwell_test(tab)
  method <- if (isTRUE(options$bowker)) "Bowker symmetry test" else "Stuart-Maxwell test"
  list(
    result = data.frame(Pair = pair_label, Level = "Categorical", Method = method, N = pair$n, Statistic = format_decimal3(res$statistic), df = if (is.finite(res$df)) format_decimal3(res$df) else "", p = format_p(res$p), stringsAsFactors = FALSE, check.names = FALSE),
    check = NULL
  )
}

prepare_paired_results <- function(data, first, second, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  first <- as.character(first %||% character(0))
  second <- as.character(second %||% character(0))
  shiny::validate(shiny::need(length(first) > 0 && length(second) > 0, "Select paired variables for both repeated measurements."))
  shiny::validate(shiny::need(length(first) == length(second), "Time 1 and Time 2 lists must have the same number of variables."))
  measurements <- paired_measurement_lookup(variable_info)
  items <- lapply(seq_along(first), function(index) {
    x <- first[[index]]
    y <- second[[index]]
    m1 <- named_value(measurements, x, "continuous")
    m2 <- named_value(measurements, y, "continuous")
    shiny::validate(shiny::need(identical(m1, m2), sprintf("Paired variables must have the same measurement level: %s and %s.", x, y)))
    paired_analyze_pair(data, x, y, m1, variable_info, labels, category_table, options)
  })
  result_tables <- lapply(items, `[[`, "result")
  all_columns <- unique(unlist(lapply(result_tables, names), use.names = FALSE))
  result_tables <- lapply(result_tables, function(table) {
    missing <- setdiff(all_columns, names(table))
    for (column in missing) table[[column]] <- ""
    table[, all_columns, drop = FALSE]
  })
  result_table <- do.call(rbind, result_tables)
  checks <- Filter(Negate(is.null), lapply(items, `[[`, "check"))
  check_table <- if (length(checks) > 0) do.call(rbind, checks) else NULL
  list(
    type = "paired",
    table = result_table,
    checks = check_table,
    options = options
  )
}
