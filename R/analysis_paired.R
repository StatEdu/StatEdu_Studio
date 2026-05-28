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

paired_measurement_label <- function(measurement) {
  switch(
    measurement,
    continuous = "Continuous",
    ordered = "Ordinal",
    binary = "Binary",
    category = "Categorical",
    measurement
  )
}

paired_display_name <- function(name, variable_info = NULL, labels = character(0), category_table = NULL) {
  correlation_variable_display_name(name, variable_info, labels, category_table)
}

paired_numeric <- function(x) {
  if (is.character(x) || is.factor(x)) {
    values <- as.character(x)
    values[!nzchar(trimws(values))] <- NA_character_
    numeric <- suppressWarnings(as.numeric(values))
    if (sum(!is.na(numeric)) >= 2) {
      return(numeric)
    }
    ordered_values <- frequency_value_order(values[!is.na(values)])
    return(as.numeric(match(values, ordered_values)))
  }
  suppressWarnings(as.numeric(x))
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
  sw_test <- if (n >= 3 && n <= 5000 && stats::sd(diff, na.rm = TRUE) > 0) {
    suppressWarnings(stats::shapiro.test(diff))
  } else {
    NULL
  }
  sw <- if (is.null(sw_test)) NA_real_ else as.numeric(sw_test$p.value)
  sw_w <- if (is.null(sw_test)) NA_real_ else unname(as.numeric(sw_test$statistic))
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
    shapiro_w = sw_w,
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
    `Shapiro-Wilk W` = if (is.finite(check$shapiro_w)) format_decimal3(check$shapiro_w) else "",
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

paired_effect_value <- function(value) {
  if (!is.finite(value)) {
    return(if (identical(value, Inf)) "Inf" else "")
  }
  format_effect_size(value)
}

paired_hedges_correction <- function(n) {
  if (!is.finite(n) || n <= 1) return(NA_real_)
  df <- n - 1
  if (df <= 1) return(1)
  1 - (3 / (4 * df - 1))
}

paired_t_effects <- function(diff) {
  diff <- diff[is.finite(diff)]
  n <- length(diff)
  sd_diff <- stats::sd(diff, na.rm = TRUE)
  d <- if (n > 1 && is.finite(sd_diff) && sd_diff > 0) mean(diff, na.rm = TRUE) / sd_diff else NA_real_
  g <- d * paired_hedges_correction(n)
  list(d = d, g = g)
}

paired_wilcoxon_r <- function(p, diff) {
  diff <- diff[is.finite(diff)]
  n <- length(diff)
  if (!is.finite(p) || p <= 0 || n <= 0) return(NA_real_)
  z <- stats::qnorm(p / 2, lower.tail = FALSE)
  direction <- sign(stats::median(diff, na.rm = TRUE))
  if (!is.finite(direction) || direction == 0) direction <- sign(mean(diff, na.rm = TRUE))
  if (!is.finite(direction) || direction == 0) direction <- 1
  direction * z / sqrt(n)
}

paired_diff_guard <- function(diff, require_variance = FALSE) {
  diff <- diff[is.finite(diff)]
  n <- length(diff)
  if (n < 2) {
    return("At least two complete paired cases are required.")
  }
  nonzero <- diff[diff != 0]
  if (length(nonzero) == 0) {
    return("The paired differences are all zero; no paired test was performed.")
  }
  if (isTRUE(require_variance)) {
    sd_diff <- stats::sd(diff)
    if (!is.finite(sd_diff) || sd_diff <= 0) {
      return("The paired differences have zero variance; paired t-test was not performed.")
    }
  }
  ""
}

paired_wilcoxon_note <- function(diff) {
  diff <- diff[is.finite(diff)]
  if (length(diff) == 0) return("")
  notes <- character(0)
  zero_count <- sum(diff == 0)
  if (zero_count > 0) {
    notes <- c(notes, sprintf("%d zero difference(s) were omitted from the Wilcoxon signed-rank calculation.", zero_count))
  }
  nonzero_abs <- abs(diff[diff != 0])
  if (length(nonzero_abs) > length(unique(nonzero_abs))) {
    notes <- c(notes, "Tied absolute differences were present; the large-sample Wilcoxon approximation was used.")
  }
  paste(notes, collapse = " ")
}

paired_skipped_item <- function(pair_label, level, method, n, reason) {
  data.frame(
    Pair = pair_label,
    Level = level,
    Method = method,
    N = n,
    Reason = reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

paired_safe_pair_label <- function(first, second, variable_info = NULL, labels = character(0), category_table = NULL) {
  first_label <- if (length(first) > 0 && nzchar(first[[1]] %||% "")) paired_display_name(first[[1]], variable_info, labels, category_table) else ""
  second_label <- if (length(second) > 0 && nzchar(second[[1]] %||% "")) paired_display_name(second[[1]], variable_info, labels, category_table) else ""
  sprintf("%s - %s", first_label %||% first, second_label %||% second)
}

paired_odds_ratio <- function(table) {
  if (nrow(table) != 2 || ncol(table) != 2) return(NA_real_)
  b <- as.numeric(table[1, 2])
  c <- as.numeric(table[2, 1])
  if (c == 0 && b == 0) return(NA_real_)
  if (c == 0) return(Inf)
  b / c
}

paired_count_rows <- function(pair_label, method, table, statistic_label, statistic, p, effect_label = "", effect = "", level_variable = NULL, category_table = NULL) {
  levels <- rownames(table)
  display_levels <- if (length(level_variable) > 0 && nzchar(level_variable[[1]] %||% "")) {
    frequency_value_display_labels(level_variable[[1]], levels, category_table)
  } else {
    levels
  }
  rows <- lapply(seq_along(levels), function(index) {
    row <- as.list(rep("", 8L + length(levels)))
    names(row) <- c("Variable", "Pre", paste0("Post_", display_levels), "StatisticLabel", "Statistic", "p", "EffectLabel", "Effect", "Method")
    row[["Variable"]] <- if (index == 1L) pair_label else ""
    row[["Pre"]] <- display_levels[[index]]
    for (post_index in seq_along(levels)) {
      row[[paste0("Post_", display_levels[[post_index]])]] <- as.integer(table[levels[[index]], levels[[post_index]]])
    }
    row[["StatisticLabel"]] <- if (index == 1L) statistic_label else ""
    row[["Statistic"]] <- if (index == 1L) statistic else ""
    row[["p"]] <- if (index == 1L) p else ""
    row[["EffectLabel"]] <- if (index == 1L) effect_label else ""
    row[["Effect"]] <- if (index == 1L) effect else ""
    row[["Method"]] <- if (index == 1L) method else ""
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  })
  out <- do.call(rbind, rows)
  attr(out, "post_levels") <- display_levels
  out
}

paired_analyze_pair <- function(data, first, second, measurement, variable_info, labels, category_table, options) {
  x_raw <- data[[first]]
  y_raw <- data[[second]]
  first_label <- paired_display_name(first, variable_info, labels, category_table)
  second_label <- paired_display_name(second, variable_info, labels, category_table)
  pair_label <- sprintf("%s - %s", first_label, second_label)

  if (identical(measurement, "continuous")) {
    pair <- paired_complete_data(paired_numeric(x_raw), paired_numeric(y_raw))
    diff <- pair$y - pair$x
    check <- paired_diff_assumption(diff)
    use_t <- !isTRUE(options$assumption_check) || isTRUE(check$normal)
    if (isTRUE(use_t)) {
      guard <- paired_diff_guard(diff, require_variance = TRUE)
      if (nzchar(guard)) {
        return(list(result = paired_skipped_item(pair_label, "Continuous", "Paired t-test", pair$n, guard), scale = NULL, count = NULL, check = paired_check_table(pair_label, check), skipped = paired_skipped_item(pair_label, "Continuous", "Paired t-test", pair$n, guard)))
      }
      test <- suppressWarnings(stats::t.test(pair$y, pair$x, paired = TRUE))
      method <- "Paired t-test"
      statistic <- unname(as.numeric(test$statistic))
      df <- unname(as.numeric(test$parameter))
      p <- as.numeric(test$p.value)
      effects <- paired_t_effects(diff)
      effect_label <- if (isTRUE(options$cohen_d)) "Hedges' g; Cohen's d" else "Hedges' g"
      effect <- if (isTRUE(options$cohen_d)) {
        paste(paired_effect_value(effects$g), paired_effect_value(effects$d), sep = "; ")
      } else {
        paired_effect_value(effects$g)
      }
    } else {
      guard <- paired_diff_guard(diff, require_variance = FALSE)
      if (nzchar(guard)) {
        return(list(result = paired_skipped_item(pair_label, "Continuous", "Wilcoxon signed-rank test", pair$n, guard), scale = NULL, count = NULL, check = paired_check_table(pair_label, check), skipped = paired_skipped_item(pair_label, "Continuous", "Wilcoxon signed-rank test", pair$n, guard)))
      }
      test <- suppressWarnings(stats::wilcox.test(pair$y, pair$x, paired = TRUE, exact = FALSE))
      method <- "Wilcoxon signed-rank test"
      statistic <- unname(as.numeric(test$statistic))
      df <- NA_real_
      p <- as.numeric(test$p.value)
      effect_label <- "r"
      effect <- paired_effect_value(paired_wilcoxon_r(p, diff))
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
      scale = data.frame(
        Variable = pair_label,
        Pre_M = format_decimal2(mean(pair$x, na.rm = TRUE)),
        Pre_SD = format_decimal2(stats::sd(pair$x, na.rm = TRUE)),
        Post_M = format_decimal2(mean(pair$y, na.rm = TRUE)),
        Post_SD = format_decimal2(stats::sd(pair$y, na.rm = TRUE)),
        Method = method,
        StatisticLabel = if (identical(method, "Paired t-test")) "t" else "W",
        Statistic = format_decimal3(statistic),
        p = format_p(p),
        EffectLabel = effect_label,
        Effect = effect,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      count = NULL,
      check = paired_check_table(pair_label, check),
      skipped = NULL,
      warning = paired_wilcoxon_note(diff)
    ))
  }

  if (identical(measurement, "ordered")) {
    pair <- paired_complete_data(paired_numeric(x_raw), paired_numeric(y_raw))
    diff <- pair$y - pair$x
    guard <- paired_diff_guard(diff, require_variance = FALSE)
    if (nzchar(guard)) {
      return(list(result = paired_skipped_item(pair_label, "Ordinal", "Wilcoxon signed-rank test", pair$n, guard), scale = NULL, count = NULL, check = NULL, skipped = paired_skipped_item(pair_label, "Ordinal", "Wilcoxon signed-rank test", pair$n, guard)))
    }
    test <- suppressWarnings(stats::wilcox.test(pair$y, pair$x, paired = TRUE, exact = FALSE))
    p <- as.numeric(test$p.value)
    return(list(
      result = data.frame(Pair = pair_label, Level = "Ordinal", Method = "Wilcoxon signed-rank test", N = pair$n, Statistic = format_decimal3(unname(as.numeric(test$statistic))), df = "", p = format_p(p), stringsAsFactors = FALSE, check.names = FALSE),
      scale = data.frame(
        Variable = pair_label,
        Pre_M = format_decimal2(mean(pair$x, na.rm = TRUE)),
        Pre_SD = format_decimal2(stats::sd(pair$x, na.rm = TRUE)),
        Post_M = format_decimal2(mean(pair$y, na.rm = TRUE)),
        Post_SD = format_decimal2(stats::sd(pair$y, na.rm = TRUE)),
        Method = "Wilcoxon signed-rank test",
        StatisticLabel = "W",
        Statistic = format_decimal3(unname(as.numeric(test$statistic))),
        p = format_p(p),
        EffectLabel = "r",
        Effect = paired_effect_value(paired_wilcoxon_r(p, diff)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      count = NULL,
      check = NULL,
      skipped = NULL,
      warning = paired_wilcoxon_note(diff)
    ))
  }

  pair <- paired_complete_data(as.character(x_raw), as.character(y_raw))
  tab <- paired_category_table(pair$x, pair$y)
  if (pair$n < 2 || nrow(tab) < 2 || ncol(tab) < 2) {
    reason <- "At least two complete paired cases with at least two observed categories are required."
    return(list(result = paired_skipped_item(pair_label, if (identical(measurement, "binary")) "Binary" else "Categorical", "Paired categorical test", pair$n, reason), scale = NULL, count = NULL, count_method = "", check = NULL, skipped = paired_skipped_item(pair_label, if (identical(measurement, "binary")) "Binary" else "Categorical", "Paired categorical test", pair$n, reason)))
  }
  if (identical(measurement, "binary") && nrow(tab) == 2 && ncol(tab) == 2) {
    b <- as.numeric(tab[1, 2])
    c <- as.numeric(tab[2, 1])
    use_asymptotic <- (b + c) >= 25 && b > 5 && c > 5
    res <- if (use_asymptotic) paired_mcnemar_asymptotic(tab) else paired_mcnemar_exact(tab)
    method <- if (use_asymptotic) "McNemar test" else "Exact McNemar test"
    statistic_label <- if (use_asymptotic) stat_chisq_label(FALSE) else ""
    effect_label <- "OR"
    effect <- paired_effect_value(paired_odds_ratio(tab))
    return(list(
      result = data.frame(Pair = pair_label, Level = "Binary", Method = method, N = pair$n, Statistic = format_decimal3(res$statistic), df = if (is.finite(res$df)) format_decimal3(res$df) else "", p = format_p(res$p), b = b, c = c, stringsAsFactors = FALSE, check.names = FALSE),
      scale = NULL,
      count = paired_count_rows(pair_label, method, tab, statistic_label, format_decimal3(res$statistic), format_p(res$p), effect_label, effect, first, category_table),
      count_method = method,
      check = NULL,
      skipped = NULL
    ))
  }

  res <- if (isTRUE(options$bowker)) paired_bowker_test(tab) else paired_stuart_maxwell_test(tab)
  method <- if (isTRUE(options$bowker)) "Bowker symmetry test" else "Stuart-Maxwell test"
  list(
    result = data.frame(Pair = pair_label, Level = "Categorical", Method = method, N = pair$n, Statistic = format_decimal3(res$statistic), df = if (is.finite(res$df)) format_decimal3(res$df) else "", p = format_p(res$p), stringsAsFactors = FALSE, check.names = FALSE),
    scale = NULL,
    count = paired_count_rows(pair_label, method, tab, stat_chisq_label(FALSE), format_decimal3(res$statistic), format_p(res$p), "", "", first, category_table),
    count_method = method,
    check = NULL,
    skipped = NULL
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
    pair_label <- paired_safe_pair_label(x, y, variable_info, labels, category_table)
    if (!all(c(x, y) %in% names(data))) {
      missing <- setdiff(c(x, y), names(data))
      reason <- sprintf("Skipped because variable(s) were not found in the active data: %s.", paste(missing, collapse = ", "))
      skipped <- paired_skipped_item(pair_label, "Unknown", "Paired test", 0L, reason)
      return(list(result = skipped, scale = NULL, count = NULL, count_method = "", check = NULL, skipped = skipped))
    }
    m1 <- named_value(measurements, x, "continuous")
    m2 <- named_value(measurements, y, "continuous")
    if (!identical(m1, m2)) {
      reason <- sprintf("Skipped because paired variables have different measurement levels (%s vs %s).", m1, m2)
      skipped <- paired_skipped_item(pair_label, sprintf("%s/%s", paired_measurement_label(m1), paired_measurement_label(m2)), "Paired test", 0L, reason)
      return(list(result = skipped, scale = NULL, count = NULL, count_method = "", check = NULL, skipped = skipped))
    }
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
  scale_tables <- Filter(Negate(is.null), lapply(items, `[[`, "scale"))
  scale_table <- if (length(scale_tables) > 0) do.call(rbind, scale_tables) else NULL
  count_tables <- Filter(Negate(is.null), lapply(items, `[[`, "count"))
  count_table <- if (length(count_tables) > 0) {
    all_columns <- unique(unlist(lapply(count_tables, names), use.names = FALSE))
    count_tables <- lapply(count_tables, function(table) {
      missing <- setdiff(all_columns, names(table))
      for (column in missing) table[[column]] <- ""
      table[, all_columns, drop = FALSE]
    })
    out <- do.call(rbind, count_tables)
    attr(out, "post_levels") <- unique(unlist(lapply(count_tables, function(table) attr(table, "post_levels", exact = TRUE)), use.names = FALSE))
    out
  } else {
    NULL
  }
  count_methods <- unique(as.character(unlist(lapply(items, function(item) item$count_method %||% character(0)), use.names = FALSE)))
  count_methods <- count_methods[nzchar(count_methods)]
  checks <- Filter(Negate(is.null), lapply(items, `[[`, "check"))
  check_table <- if (length(checks) > 0) do.call(rbind, checks) else NULL
  skipped <- Filter(Negate(is.null), lapply(items, `[[`, "skipped"))
  skipped_table <- if (length(skipped) > 0) do.call(rbind, skipped) else NULL
  warnings <- lapply(items, function(item) {
    note <- as.character(item$warning %||% "")
    if (!nzchar(note)) return(NULL)
    data.frame(Pair = item$scale$Variable[[1]] %||% "", Warning = note, stringsAsFactors = FALSE, check.names = FALSE)
  })
  warnings <- Filter(Negate(is.null), warnings)
  warning_table <- if (length(warnings) > 0) do.call(rbind, warnings) else NULL
  list(
    type = "paired",
    table = result_table,
    scale_table = scale_table,
    count_table = count_table,
    count_methods = count_methods,
    checks = check_table,
    skipped = skipped_table,
    warnings = warning_table,
    options = options
  )
}

prepare_paired_unified_results <- function(data, variable_groups, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  groups <- lapply(variable_groups %||% list(), as.character)
  groups <- groups[lengths(groups) >= 2L]
  shiny::validate(shiny::need(length(groups) > 0, "Select one or more repeated-measures rows."))

  two_groups <- groups[lengths(groups) == 2L]
  rm_groups <- groups[lengths(groups) >= 3L]
  paired_result <- NULL
  paired_rm_result <- NULL

  if (length(two_groups) > 0) {
    paired_result <- prepare_paired_results(
      data = data,
      first = vapply(two_groups, `[[`, character(1), 1L),
      second = vapply(two_groups, `[[`, character(1), 2L),
      variable_info = variable_info,
      labels = labels,
      category_table = category_table,
      options = options
    )
  }

  if (length(rm_groups) > 0) {
    paired_rm_result <- prepare_paired_rm_results(
      data = data,
      variable_groups = rm_groups,
      variable_info = variable_info,
      labels = labels,
      category_table = category_table,
      options = options
    )
  }

  if (!is.null(paired_result) && !is.null(paired_rm_result)) {
    return(list(
      type = "paired_combined",
      paired = paired_result,
      paired_rm = paired_rm_result,
      options = options
    ))
  }
  paired_result %||% paired_rm_result
}
