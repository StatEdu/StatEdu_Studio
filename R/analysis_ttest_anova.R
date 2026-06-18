# t-test / ANOVA analysis helpers.

ttest_measurement_lookup <- function(variable_info = NULL) {
  if (!is.data.frame(variable_info) || !all(c("name", "measurement") %in% names(variable_info))) {
    return(character(0))
  }
  values <- tolower(as.character(variable_info$measurement))
  values[values == "ordinal"] <- "ordered"
  stats::setNames(values, as.character(variable_info$name))
}

ttest_measurement <- function(name, variable_info = NULL) {
  measurements <- ttest_measurement_lookup(variable_info)
  named_value(measurements, name, "continuous")
}

ttest_display_variable <- function(name, variable_info = NULL, labels = character(0), category_table = NULL) {
  frequency_variable_display_name(name, variable_info, labels, category_table)
}

ttest_display_levels <- function(name, values, category_table = NULL) {
  frequency_value_display_labels(name, values, category_table)
}

ttest_level_order <- function(values) {
  frequency_value_order(values)
}

ttest_format_decimal <- function(value, digits = 2) {
  if (length(value) == 0 || is.na(value[[1]])) {
    return("")
  }
  sprintf(paste0("%.", digits, "f"), as.numeric(value[[1]]))
}

ttest_sample_sd <- function(values) {
  values <- as.numeric(values)
  values <- values[!is.na(values)]
  if (length(values) < 2) return(NA_real_)
  stats::sd(values)
}

ttest_skipped_item <- function(dependent, factor, reason, n = NA_integer_, variable_info = NULL, labels = character(0), category_table = NULL) {
  data.frame(
    `Dependent variable` = ttest_display_variable(dependent, variable_info, labels, category_table),
    `Independent variable` = ttest_display_variable(factor, variable_info, labels, category_table),
    N = if (is.na(n)) "" else as.character(n),
    Reason = reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

ttest_warning_item <- function(dependent, factor, message, variable_info = NULL, labels = character(0), category_table = NULL) {
  data.frame(
    `Dependent variable` = ttest_display_variable(dependent, variable_info, labels, category_table),
    `Independent variable` = ttest_display_variable(factor, variable_info, labels, category_table),
    Warning = message,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

ttest_safe_call <- function(expr, fallback = NULL) {
  suppressWarnings(tryCatch(expr, error = function(e) fallback))
}

ttest_group_counts <- function(groups, levels = NULL) {
  groups <- as.character(groups)
  if (is.null(levels)) {
    levels <- ttest_level_order(unique(groups))
  }
  stats::setNames(vapply(levels, function(level) sum(groups == level, na.rm = TRUE), integer(1)), levels)
}

ttest_zero_sd_groups <- function(values, groups, levels = NULL) {
  values <- as.numeric(values)
  groups <- as.character(groups)
  if (is.null(levels)) {
    levels <- ttest_level_order(unique(groups))
  }
  levels[vapply(levels, function(level) {
    group_values <- values[groups == level]
    group_values <- group_values[!is.na(group_values)]
    length(group_values) >= 2 && isTRUE(ttest_sample_sd(group_values) == 0)
  }, logical(1))]
}

ttest_normality_skew_kurtosis <- function(values) {
  values <- as.numeric(values)
  values <- values[!is.na(values)]
  skew <- sample_skewness(values)
  kurtosis <- sample_excess_kurtosis(values)
  normal <- is.finite(skew) && is.finite(kurtosis) && abs(skew) <= 2 && abs(kurtosis) <= 7
  list(
    method = "Skewness/Kurtosis",
    normal = normal,
    detail = sprintf("skew=%s, kurtosis=%s", format_decimal3(skew), format_decimal3(kurtosis))
  )
}

ttest_normality_ks <- function(values, groups) {
  values <- as.numeric(values)
  groups <- as.character(groups)
  keep <- !is.na(values) & !is.na(groups) & nzchar(groups)
  values <- values[keep]
  groups <- groups[keep]

  levels <- ttest_level_order(unique(groups))
  p_values <- stats::setNames(rep(NA_real_, length(levels)), levels)
  statistics <- stats::setNames(rep(NA_real_, length(levels)), levels)
  for (level in levels) {
    group_values <- values[groups == level]
    group_values <- group_values[!is.na(group_values)]
    if (length(group_values) < 5 || isTRUE(stats::sd(group_values) == 0)) {
      next
    }
    standardized <- as.numeric(scale(group_values))
    test <- suppressWarnings(stats::ks.test(standardized, "pnorm"))
    statistics[[level]] <- unname(test$statistic)
    p_values[[level]] <- test$p.value
  }
  available <- p_values[!is.na(p_values)]
  normal <- length(available) > 0 && all(available >= .05)
  available_statistics <- statistics[names(available)]
  list(
    method = "Kolmogorov-Smirnov by group",
    normal = normal,
    detail = paste(sprintf(
      "%s: K-S D=%s(%s)",
      names(available),
      vapply(available_statistics, format_decimal3, character(1)),
      vapply(available, format_p, character(1))
    ), collapse = "\n")
  )
}

ttest_normality_ks_grouped <- ttest_normality_ks

ttest_normality_ks_overall <- function(values) {
  values <- as.numeric(values)
  values <- values[!is.na(values)]
  if (length(values) < 5 || isTRUE(stats::sd(values) == 0)) {
    return(list(
      method = "Kolmogorov-Smirnov",
      normal = FALSE,
      detail = "No valid sample for Kolmogorov-Smirnov test"
    ))
  }
  standardized <- as.numeric(scale(values))
  test <- suppressWarnings(stats::ks.test(standardized, "pnorm"))
  p_value <- test$p.value
  list(
    method = "Kolmogorov-Smirnov",
    normal = !is.na(p_value) && p_value >= .05,
    detail = sprintf("K-S D=%s(%s)", format_decimal3(unname(test$statistic)), format_p(p_value))
  )
}

ttest_normality_shapiro <- function(values, groups) {
  values <- as.numeric(values)
  groups <- as.character(groups)
  keep <- !is.na(values) & !is.na(groups) & nzchar(groups)
  values <- values[keep]
  groups <- groups[keep]

  levels <- ttest_level_order(unique(groups))
  p_values <- stats::setNames(rep(NA_real_, length(levels)), levels)
  statistics <- stats::setNames(rep(NA_real_, length(levels)), levels)
  for (level in levels) {
    group_values <- values[groups == level]
    group_values <- group_values[!is.na(group_values)]
    if (length(group_values) < 3 || length(group_values) > 5000 || isTRUE(stats::sd(group_values) == 0)) {
      next
    }
    test <- suppressWarnings(stats::shapiro.test(group_values))
    statistics[[level]] <- unname(test$statistic)
    p_values[[level]] <- test$p.value
  }
  available <- p_values[!is.na(p_values)]
  normal <- length(available) > 0 && all(available >= .05)
  detail <- if (length(available) > 0) {
    available_statistics <- statistics[names(available)]
    paste(sprintf(
      "%s: S-W W=%s(%s)",
      names(available),
      vapply(available_statistics, format_decimal3, character(1)),
      vapply(available, format_p, character(1))
    ), collapse = "\n")
  } else {
    "No valid group for Shapiro-Wilk test"
  }
  list(
    method = "Shapiro-Wilk by group",
    normal = normal,
    detail = detail
  )
}

ttest_normality_shapiro_grouped <- ttest_normality_shapiro

ttest_normality_shapiro_overall <- function(values) {
  values <- as.numeric(values)
  values <- values[!is.na(values)]
  if (length(values) < 3 || length(values) > 5000 || isTRUE(stats::sd(values) == 0)) {
    return(list(
      method = "Shapiro-Wilk",
      normal = FALSE,
      detail = "No valid sample for Shapiro-Wilk test"
    ))
  }
  test <- suppressWarnings(stats::shapiro.test(values))
  p_value <- test$p.value
  list(
    method = "Shapiro-Wilk",
    normal = !is.na(p_value) && p_value >= .05,
    detail = sprintf("S-W W=%s(%s)", format_decimal3(unname(test$statistic)), format_p(p_value))
  )
}

ttest_levene_p <- function(values, groups) {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data) < 3 || length(unique(data$g)) < 2) {
    return(NA_real_)
  }
  centers <- stats::ave(data$y, data$g, FUN = stats::median)
  data$z <- abs(data$y - centers)
  fit <- try(stats::aov(z ~ g, data = data), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(NA_real_)
  }
  summary_fit <- summary(fit)[[1]]
  as.numeric(summary_fit[["Pr(>F)"]][[1]])
}

ttest_parametric_anova_pairwise <- function(values, groups, method = "scheffe") {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  levels <- levels(data$g)
  k <- length(levels)
  matrix_out <- matrix(1, nrow = k, ncol = k, dimnames = list(levels, levels))
  if (k < 2) return(matrix_out)

  method <- tolower(method %||% "scheffe")
  if (identical(method, "tukey")) {
    fit <- stats::aov(y ~ g, data = data)
    tukey <- try(stats::TukeyHSD(fit, "g")$g, silent = TRUE)
    if (!inherits(tukey, "try-error")) {
      for (row_name in rownames(tukey)) {
        pair <- strsplit(row_name, "-", fixed = TRUE)[[1]]
        if (length(pair) == 2 && all(pair %in% levels)) {
          matrix_out[pair[[1]], pair[[2]]] <- matrix_out[pair[[2]], pair[[1]]] <- as.numeric(tukey[row_name, "p adj"])
        }
      }
    }
    return(matrix_out)
  }

  if (identical(method, "duncan")) {
    if (!requireNamespace("agricolae", quietly = TRUE)) {
      stop("Package 'agricolae' is required for Duncan multiple range test.", call. = FALSE)
    }
    fit <- stats::aov(y ~ g, data = data)
    duncan <- try(agricolae::duncan.test(fit, "g", group = FALSE, console = FALSE), silent = TRUE)
    if (!inherits(duncan, "try-error") && is.data.frame(duncan[["comparison"]])) {
      comparison <- duncan[["comparison"]]
      for (row_name in rownames(comparison)) {
        pair <- strsplit(row_name, " - ", fixed = TRUE)[[1]]
        if (length(pair) == 2 && all(pair %in% levels) && "pvalue" %in% names(comparison)) {
          p <- as.numeric(comparison[row_name, "pvalue"])
          if (!is.na(p)) {
            matrix_out[pair[[1]], pair[[2]]] <- matrix_out[pair[[2]], pair[[1]]] <- p
          }
        }
      }
    }
    return(matrix_out)
  }

  pairwise <- stats::pairwise.t.test(data$y, data$g, p.adjust.method = if (identical(method, "bonferroni")) "bonferroni" else "none")
  if (is.matrix(pairwise$p.value)) {
    for (row in rownames(pairwise$p.value)) {
      for (col in colnames(pairwise$p.value)) {
        p <- pairwise$p.value[row, col]
        if (!is.na(p)) {
          matrix_out[row, col] <- matrix_out[col, row] <- p
        }
      }
    }
  }

  if (identical(method, "scheffe")) {
    fit <- stats::aov(y ~ g, data = data)
    summary_fit <- summary(fit)[[1]]
    mse <- as.numeric(summary_fit[["Mean Sq"]][[2]])
    df_error <- as.numeric(summary_fit[["Df"]][[2]])
    means <- tapply(data$y, data$g, mean)
    counts <- table(data$g)
    for (i in seq_len(k - 1)) {
      for (j in seq.int(i + 1, k)) {
        diff <- means[[i]] - means[[j]]
        f_value <- (diff^2 / (mse * (1 / counts[[i]] + 1 / counts[[j]]))) / (k - 1)
        p <- stats::pf(f_value, df1 = k - 1, df2 = df_error, lower.tail = FALSE)
        matrix_out[i, j] <- matrix_out[j, i] <- p
      }
    }
  }
  matrix_out
}

ttest_games_howell_pairwise <- function(values, groups) {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  levels <- levels(data$g)
  k <- length(levels)
  matrix_out <- matrix(1, nrow = k, ncol = k, dimnames = list(levels, levels))
  if (k < 2) return(matrix_out)

  split_values <- split(data$y, data$g)
  means <- vapply(split_values, mean, numeric(1), na.rm = TRUE)
  variances <- vapply(split_values, stats::var, numeric(1), na.rm = TRUE)
  counts <- vapply(split_values, function(x) sum(!is.na(x)), integer(1))

  for (i in seq_len(k - 1)) {
    for (j in seq.int(i + 1, k)) {
      ni <- counts[[i]]
      nj <- counts[[j]]
      vi <- variances[[i]]
      vj <- variances[[j]]
      if (ni < 2 || nj < 2 || !is.finite(vi) || !is.finite(vj)) next
      se2 <- vi / ni + vj / nj
      if (!is.finite(se2) || se2 <= 0) next
      t_value <- abs(means[[i]] - means[[j]]) / sqrt(se2)
      df_num <- se2^2
      df_den <- ((vi / ni)^2 / (ni - 1)) + ((vj / nj)^2 / (nj - 1))
      df <- df_num / df_den
      p <- stats::ptukey(t_value * sqrt(2), nmeans = k, df = df, lower.tail = FALSE)
      matrix_out[i, j] <- matrix_out[j, i] <- p
    }
  }
  matrix_out
}

ttest_nonparametric_pairwise <- function(values, groups, method = "bonferroni") {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  levels <- levels(data$g)
  k <- length(levels)
  matrix_out <- matrix(1, nrow = k, ncol = k, dimnames = list(levels, levels))
  if (k < 2) return(matrix_out)
  method <- tolower(method %||% "bonferroni")
  if (!method %in% c("bonferroni", "holm")) {
    method <- "bonferroni"
  }
  pairwise <- stats::pairwise.wilcox.test(data$y, data$g, p.adjust.method = method, exact = FALSE)
  if (is.matrix(pairwise$p.value)) {
    for (row in rownames(pairwise$p.value)) {
      for (col in colnames(pairwise$p.value)) {
        p <- pairwise$p.value[row, col]
        if (!is.na(p)) {
          matrix_out[row, col] <- matrix_out[col, row] <- p
        }
      }
    }
  }
  matrix_out
}

ttest_group_letters <- function(values, groups, p_matrix, ordered = FALSE) {
  levels <- ttest_level_order(unique(as.character(groups)))
  means <- tapply(as.numeric(values), as.character(groups), mean, na.rm = TRUE)
  levels <- levels[levels %in% names(means)]
  if (!isTRUE(ordered)) {
    levels <- names(sort(means[levels], decreasing = TRUE))
  }
  letters_pool <- letters
  assigned <- stats::setNames(rep("", length(levels)), levels)
  letter_groups <- list()
  processed <- character(0)
  pair_is_shared <- function(first, second) {
    first <- as.character(first %||% "")
    second <- as.character(second %||% "")
    if (!nzchar(first) || !nzchar(second)) return(FALSE)
    if (identical(first, second)) return(TRUE)
    if (is.null(p_matrix) || !all(c(first, second) %in% rownames(p_matrix)) || !all(c(first, second) %in% colnames(p_matrix))) {
      return(FALSE)
    }
    p_value <- suppressWarnings(as.numeric(p_matrix[first, second]))
    !is.na(p_value) && p_value >= .05
  }
  for (level in levels) {
    placed <- FALSE
    if (length(letter_groups) > 0) {
      for (letter in names(letter_groups)) {
        members <- letter_groups[[letter]]
        shared <- vapply(members, function(member) pair_is_shared(level, member), logical(1))
        if (all(shared)) {
          letter_groups[[letter]] <- c(members, level)
          assigned[[level]] <- paste0(assigned[[level]], letter)
          placed <- TRUE
        }
      }
    }
    if (!placed) {
      letter <- letters_pool[[length(letter_groups) + 1]]
      shared_members <- character(0)
      for (member in processed) {
        compatible_with_level <- pair_is_shared(level, member)
        compatible_with_group <- length(shared_members) == 0 ||
          all(vapply(shared_members, function(existing) pair_is_shared(member, existing), logical(1)))
        if (isTRUE(compatible_with_level) && isTRUE(compatible_with_group)) {
          shared_members <- c(shared_members, member)
        }
      }
      new_members <- c(shared_members, level)
      letter_groups[[letter]] <- new_members
      assigned[new_members] <- paste0(assigned[new_members], letter)
    }
    processed <- c(processed, level)
  }
  assigned[ttest_level_order(names(assigned))]
}

ttest_lookup_letters <- function(letter_map, values) {
  out <- rep("", length(values))
  if (is.null(letter_map) || length(letter_map) == 0) {
    return(out)
  }
  keys <- as.character(values)
  hit <- keys %in% names(letter_map)
  out[hit] <- unname(letter_map[keys[hit]])
  out[is.na(out)] <- ""
  out
}

ttest_posthoc_table <- function(factor, levels, p_matrix, method, variable_info = NULL, labels = character(0), category_table = NULL) {
  method <- as.character(method %||% "")
  if (!nzchar(method) || is.null(p_matrix) || length(p_matrix) == 0) {
    return(data.frame())
  }
  levels <- as.character(levels %||% character(0))
  levels <- levels[levels %in% rownames(p_matrix) & levels %in% colnames(p_matrix)]
  if (length(levels) < 2) {
    return(data.frame())
  }

  display_levels <- stats::setNames(ttest_display_levels(factor, levels, category_table), levels)
  rows <- list()
  for (i in seq_len(length(levels) - 1L)) {
    for (j in seq.int(i + 1L, length(levels))) {
      first <- levels[[i]]
      second <- levels[[j]]
      p_value <- suppressWarnings(as.numeric(p_matrix[first, second]))
      rows[[length(rows) + 1L]] <- data.frame(
        Variable = ttest_display_variable(factor, variable_info, labels, category_table),
        Method = method,
        Comparison = sprintf("%s - %s", display_levels[[first]], display_levels[[second]]),
        p = if (is.na(p_value)) "" else format_p(p_value),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame())
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

ttest_analysis_data <- function(values, groups) {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data[stats::complete.cases(data), , drop = FALSE]
}

ttest_hedges_correction <- function(df) {
  if (!is.finite(df) || df <= 1) return(NA_real_)
  1 - (3 / (4 * df - 1))
}

ttest_hedges_g <- function(values, groups) {
  data <- ttest_analysis_data(values, groups)
  split_values <- split(data$y, data$g)
  if (length(split_values) != 2) return(NA_real_)
  n1 <- length(split_values[[1]])
  n2 <- length(split_values[[2]])
  if (n1 < 2 || n2 < 2) return(NA_real_)
  df <- n1 + n2 - 2
  pooled <- sqrt(((n1 - 1) * stats::var(split_values[[1]]) + (n2 - 1) * stats::var(split_values[[2]])) / df)
  if (!is.finite(pooled) || pooled == 0) return(NA_real_)
  d <- (mean(split_values[[1]]) - mean(split_values[[2]])) / pooled
  d * ttest_hedges_correction(df)
}

ttest_paired_dz <- function(before, after) {
  data <- data.frame(before = as.numeric(before), after = as.numeric(after))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data) < 2) return(NA_real_)
  differences <- data$after - data$before
  sd_diff <- stats::sd(differences)
  if (!is.finite(sd_diff) || sd_diff == 0) return(NA_real_)
  mean(differences) / sd_diff
}

ttest_paired_gav <- function(before, after) {
  data <- data.frame(before = as.numeric(before), after = as.numeric(after))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data) < 2) return(NA_real_)
  average_sd <- (stats::sd(data$before) + stats::sd(data$after)) / 2
  if (!is.finite(average_sd) || average_sd == 0) return(NA_real_)
  d_av <- mean(data$after - data$before) / average_sd
  d_av * ttest_hedges_correction(nrow(data) - 1)
}

ttest_anova_sums <- function(values, groups) {
  data <- ttest_analysis_data(values, groups)
  if (nrow(data) == 0 || length(unique(data$g)) < 2) return(NULL)
  fit <- stats::aov(y ~ g, data = data)
  summary_fit <- summary(fit)[[1]]
  list(
    ss_between = as.numeric(summary_fit[["Sum Sq"]][[1]]),
    ss_error = as.numeric(summary_fit[["Sum Sq"]][[2]]),
    ss_total = sum(summary_fit[["Sum Sq"]], na.rm = TRUE),
    df_between = as.numeric(summary_fit[["Df"]][[1]]),
    df_error = as.numeric(summary_fit[["Df"]][[2]]),
    ms_error = as.numeric(summary_fit[["Mean Sq"]][[2]])
  )
}

ttest_omega_squared <- function(values, groups) {
  sums <- ttest_anova_sums(values, groups)
  if (is.null(sums) || !is.finite(sums$ss_total) || sums$ss_total <= 0 || !is.finite(sums$ms_error)) {
    return(NA_real_)
  }
  omega <- (sums$ss_between - sums$df_between * sums$ms_error) / (sums$ss_total + sums$ms_error)
  if (!is.finite(omega)) NA_real_ else max(0, omega)
}

ttest_partial_eta_squared <- function(ss_effect, ss_error) {
  denominator <- ss_effect + ss_error
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  ss_effect / denominator
}

ttest_kruskal_epsilon_squared <- function(values, groups) {
  data <- ttest_analysis_data(values, groups)
  if (nrow(data) == 0 || length(unique(data$g)) < 2) return(NA_real_)
  kw <- stats::kruskal.test(y ~ g, data = data)
  k <- length(unique(data$g))
  denominator <- nrow(data) - k
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  epsilon <- (as.numeric(kw$statistic) - k + 1) / denominator
  if (!is.finite(epsilon)) NA_real_ else max(0, epsilon)
}

ttest_mann_whitney_z <- function(values, groups) {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  split_values <- split(data$y, data$g)
  if (length(split_values) != 2) return(NA_real_)
  n1 <- length(split_values[[1]])
  n2 <- length(split_values[[2]])
  n <- n1 + n2
  if (n1 < 1 || n2 < 1 || n < 3) return(NA_real_)
  ranks <- rank(data$y, ties.method = "average")
  rank_sum_1 <- sum(ranks[data$g == levels(data$g)[[1]]])
  u1 <- rank_sum_1 - n1 * (n1 + 1) / 2
  mean_u <- n1 * n2 / 2
  ties <- table(data$y)
  tie_correction <- sum(ties^3 - ties) / (n * (n - 1))
  variance_u <- n1 * n2 * (n + 1 - tie_correction) / 12
  if (!is.finite(variance_u) || variance_u <= 0) return(NA_real_)
  (u1 - mean_u) / sqrt(variance_u)
}

ttest_cliffs_delta <- function(values, groups) {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  split_values <- split(data$y, data$g)
  if (length(split_values) != 2) return(NA_real_)
  n1 <- length(split_values[[1]])
  n2 <- length(split_values[[2]])
  if (n1 < 1 || n2 < 1) return(NA_real_)
  ranks <- rank(data$y, ties.method = "average")
  rank_sum_1 <- sum(ranks[data$g == levels(data$g)[[1]]])
  u1 <- rank_sum_1 - n1 * (n1 + 1) / 2
  delta <- (2 * u1) / (n1 * n2) - 1
  if (!is.finite(delta)) NA_real_ else delta
}

ttest_jt_test <- function(values, groups, alternative = "two.sided") {
  data <- data.frame(y = as.numeric(values), g = as.character(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  levels <- ttest_level_order(unique(data$g))
  data <- data[data$g %in% levels, , drop = FALSE]
  k <- length(levels)
  n <- nrow(data)
  if (k < 2 || n < 3) {
    return(list(statistic = NA_real_, z = NA_real_, p.value = NA_real_, r = NA_real_))
  }

  split_values <- lapply(levels, function(level) data$y[data$g == level])
  jt <- 0
  for (i in seq_len(k - 1)) {
    for (j in seq.int(i + 1, k)) {
      comparisons <- outer(split_values[[j]], split_values[[i]], "-")
      jt <- jt + sum(comparisons > 0, na.rm = TRUE) + 0.5 * sum(comparisons == 0, na.rm = TRUE)
    }
  }

  counts <- vapply(split_values, length, integer(1))
  mean_jt <- (n^2 - sum(counts^2)) / 4
  var_jt <- (n^2 * (2 * n + 3) - sum(counts^2 * (2 * counts + 3))) / 72
  if (!is.finite(var_jt) || var_jt <= 0) {
    return(list(statistic = jt, z = NA_real_, p.value = NA_real_, r = NA_real_))
  }

  z <- (jt - mean_jt) / sqrt(var_jt)
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))
  p_value <- switch(
    alternative,
    greater = stats::pnorm(z, lower.tail = FALSE),
    less = stats::pnorm(z, lower.tail = TRUE),
    two.sided = 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  )
  list(statistic = jt, z = z, p.value = p_value, r = abs(z) / sqrt(n))
}

ttest_polynomial_trend <- function(values, groups, degree = 1, weights = NULL) {
  data <- data.frame(y = as.numeric(values), g = as.character(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data) < 3) {
    return(list(method = "Polynomial trend", p.value = NA_real_, partial_eta2 = NA_real_))
  }
  levels <- ttest_level_order(unique(data$g))
  data$score <- match(data$g, levels)
  degree <- min(as.integer(degree %||% 1), length(levels) - 1)
  if (!is.finite(degree) || degree < 1) {
    return(list(method = "Polynomial trend", p.value = NA_real_, partial_eta2 = NA_real_))
  }
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
    if (length(weights) != nrow(data)) {
      weights <- NULL
    }
  }
  fit <- stats::lm(y ~ stats::poly(score, degree = degree), data = data, weights = weights)
  anova_fit <- stats::anova(fit)
  ss_effect <- sum(as.numeric(anova_fit[["Sum Sq"]][-nrow(anova_fit)]), na.rm = TRUE)
  ss_error <- as.numeric(anova_fit[["Sum Sq"]][nrow(anova_fit)])
  p_value <- as.numeric(anova_fit[["Pr(>F)"]][[1]])
  list(
    method = "Polynomial trend",
    p.value = p_value,
    partial_eta2 = ttest_partial_eta_squared(ss_effect, ss_error)
  )
}

ttest_welch_polynomial_trend <- function(values, groups, degree = 1) {
  data <- data.frame(y = as.numeric(values), g = as.character(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  levels <- ttest_level_order(unique(data$g))
  group_variance <- tapply(data$y, data$g, stats::var, na.rm = TRUE)
  weights <- 1 / group_variance[data$g]
  weights[!is.finite(weights)] <- 1
  out <- ttest_polynomial_trend(data$y, data$g, degree = degree, weights = weights)
  out$method <- "Welch + polynomial"
  out
}

ttest_select_trend_method <- function(normal = FALSE, variance = c("equal", "mild_heterogeneous", "heterogeneous"), ordinal = FALSE, robust = FALSE) {
  variance <- match.arg(variance)
  if (isTRUE(robust) || isTRUE(ordinal) || !isTRUE(normal)) return("JT")
  if (identical(variance, "mild_heterogeneous")) return("Welch + polynomial")
  "Polynomial trend"
}

ttest_trend_analysis <- function(values, groups, normal = FALSE, equal_variance = TRUE, ordinal = FALSE, robust = FALSE) {
  variance <- if (isTRUE(equal_variance)) "equal" else "mild_heterogeneous"
  method <- ttest_select_trend_method(normal = normal, variance = variance, ordinal = ordinal, robust = robust)
  if (identical(method, "JT")) {
    jt <- ttest_jt_test(values, groups)
    return(list(method = "JT", p.value = jt$p.value, effect_size = jt$r, effect_size_name = "r"))
  }
  if (identical(method, "Welch + polynomial")) {
    trend <- ttest_welch_polynomial_trend(values, groups)
    return(list(method = trend$method, p.value = trend$p.value, effect_size = trend$partial_eta2, effect_size_name = "partial eta2"))
  }
  trend <- ttest_polynomial_trend(values, groups)
  list(method = trend$method, p.value = trend$p.value, effect_size = trend$partial_eta2, effect_size_name = "partial eta2")
}

ttest_ordered_significance_notation <- function(values, groups, p_matrix, labels = NULL, alpha = .05) {
  data <- data.frame(y = as.numeric(values), g = as.character(groups))
  data <- data[stats::complete.cases(data) & nzchar(data$g), , drop = FALSE]
  if (nrow(data) == 0 || is.null(p_matrix) || length(p_matrix) == 0) return("")
  levels <- ttest_level_order(unique(data$g))
  data$g <- factor(data$g, levels = levels)
  means <- tapply(data$y, data$g, mean, na.rm = TRUE)
  levels <- levels[levels %in% names(means)]
  ordered_levels <- names(sort(means[levels], decreasing = TRUE))
  if (!is.null(labels)) {
    labels <- stats::setNames(as.character(labels), names(labels))
  }
  display <- function(level) {
    label <- named_value(labels, level, level)
    if (nzchar(label)) label else level
  }
  statements <- list()
  for (i in seq_along(ordered_levels)) {
    higher <- ordered_levels[[i]]
    lower <- character(0)
    if (i >= length(ordered_levels)) next
    for (j in seq.int(i + 1, length(ordered_levels))) {
      candidate <- ordered_levels[[j]]
      if (!all(c(higher, candidate) %in% rownames(p_matrix)) || !all(c(higher, candidate) %in% colnames(p_matrix))) next
      p_value <- p_matrix[higher, candidate] %||% NA_real_
      if (!is.na(p_value) && p_value < alpha && means[[higher]] > means[[candidate]]) {
        lower <- c(lower, display(candidate))
      }
    }
    if (length(lower) > 0) {
      key <- paste(lower, collapse = "\r")
      if (is.null(statements[[key]])) {
        statements[[key]] <- list(higher = character(0), lower = lower)
      }
      statements[[key]]$higher <- c(statements[[key]]$higher, higher)
    }
  }
  if (length(statements) == 0) {
    return("")
  }
  notation <- vapply(statements, function(statement) {
    higher <- vapply(statement$higher, display, character(1))
    lower <- vapply(statement$lower, display, character(1))
    sprintf("%s>%s", paste(higher, collapse = ", "), paste(lower, collapse = ", "))
  }, character(1))
  paste(notation, collapse = "; ")
}

ttest_distribute_ordered_posthoc <- function(rows, notation) {
  if (!is.data.frame(rows) || !"post-hoc" %in% names(rows)) {
    return(rows)
  }
  notation <- as.character(notation %||% "")
  statements <- trimws(unlist(strsplit(notation, "\\s*;\\s*|\\n+", perl = TRUE), use.names = FALSE))
  statements <- statements[nzchar(statements)]
  if (length(statements) == 0) {
    return(rows)
  }
  rows[["post-hoc"]] <- ""
  target_rows <- seq_len(min(nrow(rows), length(statements)))
  rows[["post-hoc"]][target_rows] <- statements[target_rows]
  rows
}

analysis_apply_ordered_posthoc_markers <- function(rows, estimates, levels, p_matrix, label_column = NULL, alpha = .05) {
  if (!is.data.frame(rows) || !"post-hoc" %in% names(rows)) {
    return(rows)
  }
  if (is.null(label_column)) {
    label_column <- intersect(c("Label", "Value"), names(rows))[[1]] %||% ""
  }
  if (!nzchar(label_column) || !label_column %in% names(rows)) {
    return(rows)
  }
  all_estimates <- suppressWarnings(as.numeric(estimates))
  all_levels <- as.character(levels)
  valid <- is.finite(all_estimates) & nzchar(all_levels)
  if (!any(valid) || is.null(p_matrix) || length(p_matrix) == 0) {
    return(rows)
  }
  estimates <- all_estimates[valid]
  levels <- all_levels[valid]
  if (length(levels) < 2L) {
    return(rows)
  }

  marker_sequence <- c(letters, paste0(rep(letters, each = length(letters)), letters))
  marker_map <- stats::setNames(marker_sequence[seq_along(levels)], levels)
  ordered_low_to_high <- levels[order(estimates, decreasing = FALSE, na.last = NA)]
  estimate_map <- stats::setNames(estimates, levels)

  ordered_high_to_low <- rev(ordered_low_to_high)
  statements <- character(0)
  for (higher in ordered_high_to_low) {
    lower_markers <- character(0)
    for (candidate in ordered_low_to_high) {
      if (identical(higher, candidate)) next
      if (!all(c(higher, candidate) %in% rownames(p_matrix)) || !all(c(higher, candidate) %in% colnames(p_matrix))) next
      if (!is.finite(estimate_map[[higher]]) || !is.finite(estimate_map[[candidate]]) || estimate_map[[higher]] <= estimate_map[[candidate]]) next
      p_value <- suppressWarnings(as.numeric(p_matrix[higher, candidate] %||% NA_real_))
      if (is.finite(p_value) && p_value < alpha) {
        lower_markers <- c(lower_markers, named_value(marker_map, candidate, ""))
      }
    }
    lower_markers <- unique(lower_markers[nzchar(lower_markers)])
    higher_marker <- named_value(marker_map, higher, "")
    if (length(lower_markers) > 0L && nzchar(higher_marker)) {
      statements <- c(statements, sprintf("%s>%s", higher_marker, paste(lower_markers, collapse = ",")))
    }
  }
  if (length(statements) == 0L) {
    return(rows)
  }

  marker_rows <- list()
  for (row_index in seq_len(min(nrow(rows), length(all_levels)))) {
    level <- all_levels[[row_index]] %||% ""
    marker <- named_value(marker_map, level, "")
    if (nzchar(marker) && nzchar(as.character(rows[[label_column]][[row_index]] %||% ""))) {
      marker_rows[[length(marker_rows) + 1L]] <- data.frame(
        row = row_index,
        column = label_column,
        marker = marker,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(marker_rows) > 0L) {
    existing <- attr(rows, "note_markers", exact = TRUE)
    marker_table <- do.call(rbind, marker_rows)
    attr(rows, "note_markers") <- if (is.data.frame(existing) && nrow(existing) > 0) {
      rbind(existing, marker_table)
    } else {
      marker_table
    }
  }
  ttest_distribute_ordered_posthoc(rows, paste(unique(statements), collapse = "; "))
}

ttest_effect_size <- function(values, groups, test_type) {
  data <- ttest_analysis_data(values, groups)
  if (nrow(data) == 0) return("")
  if (identical(test_type, "t")) {
    return(format_effect_size(ttest_hedges_g(values, groups)))
  }
  if (identical(test_type, "anova")) {
    return(format_effect_size(ttest_omega_squared(values, groups)))
  }
  if (identical(test_type, "mw")) {
    return(format_effect_size(ttest_cliffs_delta(values, groups)))
  }
  if (identical(test_type, "kw")) {
    return(format_effect_size(ttest_kruskal_epsilon_squared(values, groups)))
  }
  if (identical(test_type, "jt")) {
    return(format_effect_size(ttest_jt_test(values, groups)$r))
  }
  ""
}

ttest_trend_p <- function(values, groups, normal = FALSE, equal_variance = TRUE, ordinal = FALSE, robust = FALSE) {
  trend <- ttest_trend_analysis(values, groups, normal = normal, equal_variance = equal_variance, ordinal = ordinal, robust = robust)
  format_p(trend$p.value)
}

ttest_group_summary <- function(values, groups, levels, median_iqr = FALSE) {
  lapply(levels, function(level) {
    group_values <- as.numeric(values[as.character(groups) == level])
    if (isTRUE(median_iqr)) {
      q <- stats::quantile(group_values, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE, type = 2)
      return(data.frame(
        Value = level,
        M = ttest_format_decimal(q[[2]], 2),
        SD = sprintf("%s~%s", ttest_format_decimal(q[[1]], 2), ttest_format_decimal(q[[3]], 2)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    data.frame(
      Value = level,
      M = ttest_format_decimal(mean(group_values, na.rm = TRUE), 2),
      SD = ttest_format_decimal(ttest_sample_sd(group_values), 2),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
}

ttest_result_table_columns <- c("Variable", "Value", "M", "SD", "Statistic", "p", "Effect size", "p for trend", "post-hoc")

ttest_statistic_df_text <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) return("")
  if (abs(value - round(value)) < 1e-8) {
    return(as.character(as.integer(round(value))))
  }
  format_decimal3(value)
}

ttest_format_statistic <- function(statistic, df = numeric(0)) {
  stat_text <- format_decimal3(statistic)
  if (!nzchar(stat_text)) return("")
  df <- suppressWarnings(as.numeric(df))
  df <- df[is.finite(df)]
  if (length(df) == 0L) return(stat_text)
  paste0(stat_text, "(", paste(vapply(df, ttest_statistic_df_text, character(1)), collapse = ","), ")")
}

ttest_statistic_heading <- function(labels) {
  labels <- unique(as.character(labels %||% character(0)))
  labels <- labels[nzchar(labels)]
  if (length(labels) == 0) {
    return("Statistic")
  }
  paste(labels, collapse = "/")
}

ttest_apply_statistic_heading <- function(table, labels) {
  if (!is.data.frame(table) || !"Statistic" %in% names(table)) {
    return(table)
  }
  names(table)[names(table) == "Statistic"] <- ttest_statistic_heading(labels)
  table
}

ttest_effect_size_name <- function(test_type) {
  switch(
    as.character(test_type %||% ""),
    t = "Hedges' g",
    anova = "omega squared",
    mw = "Cliff's delta",
    kw = "epsilon squared",
    jt = "r",
    ""
  )
}

ttest_p_note <- function(test_type, analysis) {
  analysis <- as.character(analysis %||% "")
  test_type <- as.character(test_type %||% "")
  if (grepl("^Welch", analysis)) {
    return(list(key = "welch", symbol = "", note = "Welch test was used because homogeneity of variance was not satisfied."))
  }
  if (identical(test_type, "mw")) {
    return(list(key = "mann_whitney", symbol = "", note = "Mann-Whitney U test."))
  }
  if (identical(test_type, "kw")) {
    return(list(key = "kruskal_wallis", symbol = "", note = "Kruskal-Wallis test."))
  }
  list(key = "", symbol = "", note = "")
}

ttest_trend_note <- function(method) {
  method <- as.character(method %||% "")
  switch(
    method,
    `Polynomial trend` = list(key = "polynomial", symbol = "", note = "Polynomial trend analysis."),
    `Welch + polynomial` = list(key = "welch_polynomial", symbol = "", note = "Welch + polynomial trend analysis."),
    JT = list(key = "jt", symbol = "", note = "Jonckheere-Terpstra trend test."),
    list(key = "", symbol = "", note = "")
  )
}

ttest_note_key <- function(type, key) {
  paste(as.character(type %||% ""), as.character(key %||% ""), sep = ":")
}

ttest_numbered_notes <- function(items) {
  rows <- list()
  add_note <- function(type, key, note) {
    key <- as.character(key %||% "")
    note <- as.character(note %||% "")
    if (!nzchar(key) || !nzchar(note)) return()
    rows[[length(rows) + 1L]] <<- data.frame(type = type, key = key, note = note, stringsAsFactors = FALSE)
  }
  for (item in items %||% list()) {
    notes <- item$notes %||% list()
    add_note("method", notes$p_key %||% "", notes$p_note %||% "")
    effect <- notes$effect_size %||% ""
    if (nzchar(effect)) add_note("effect", effect, paste0("ES = effect size (", effect, ")."))
    add_note("trend", notes$trend_key %||% "", notes$trend_note %||% "")
  }
  if (length(rows) == 0) {
    return(data.frame(marker = character(0), type = character(0), key = character(0), note = character(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out <- out[!duplicated(ttest_note_key(out$type, out$key)), , drop = FALSE]
  out$marker <- ""
  marker_index <- 0L
  note_type_order <- c("method", "trend", "effect")
  for (type in note_type_order[note_type_order %in% unique(out$type)]) {
    rows_for_type <- which(out$type == type)
    if (length(rows_for_type) <= 1L) next
    for (row_index in rows_for_type) {
      marker_index <- marker_index + 1L
      out$marker[[row_index]] <- as.character(marker_index)
    }
  }
  out[, c("marker", "type", "key", "note")]
}

ttest_note_marker <- function(notes, type, key) {
  if (!is.data.frame(notes) || nrow(notes) == 0) return("")
  matched <- notes$marker[notes$type == type & notes$key == key]
  if (length(matched) == 0) "" else matched[[1]]
}

ttest_append_marker <- function(value, marker) {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value) || !nzchar(marker)) return(value)
  paste0(value, marker)
}

ttest_preserve_note_markers <- function(source, target) {
  markers <- attr(source, "note_markers", exact = TRUE)
  if (is.data.frame(markers) && nrow(markers) > 0) {
    attr(target, "note_markers") <- markers
  }
  target
}

ttest_apply_numbered_notes <- function(table, items) {
  notes <- ttest_numbered_notes(items)
  if (!is.data.frame(table) || nrow(table) == 0 || nrow(notes) == 0) {
    return(list(table = table, notes = notes))
  }

  cell_markers <- list()
  existing_markers <- attr(table, "note_markers", exact = TRUE)
  if (is.data.frame(existing_markers) && nrow(existing_markers) > 0) {
    cell_markers <- lapply(seq_len(nrow(existing_markers)), function(index) existing_markers[index, , drop = FALSE])
  }
  add_cell_marker <- function(row, column, marker) {
    marker <- as.character(marker %||% "")
    if (!nzchar(marker)) return()
    cell_markers[[length(cell_markers) + 1L]] <<- data.frame(
      row = as.integer(row),
      column = as.character(column),
      marker = marker,
      stringsAsFactors = FALSE
    )
  }

  row_start <- 1L
  for (item in items %||% list()) {
    n_rows <- if (is.data.frame(item$table)) nrow(item$table) else 0L
    item_notes <- item$notes %||% list()
    if (n_rows > 0) {
      p_marker <- ttest_note_marker(notes, "method", item_notes$p_key %||% "")
      if ("p" %in% names(table)) {
        table$p[[row_start]] <- ttest_append_marker(table$p[[row_start]], p_marker)
        add_cell_marker(row_start, "p", p_marker)
      }
      effect_marker <- ttest_note_marker(notes, "effect", item_notes$effect_size %||% "")
      if ("Effect size" %in% names(table)) {
        table[["Effect size"]][[row_start]] <- ttest_append_marker(table[["Effect size"]][[row_start]], effect_marker)
        add_cell_marker(row_start, "Effect size", effect_marker)
      }
      trend_marker <- ttest_note_marker(notes, "trend", item_notes$trend_key %||% "")
      if ("p for trend" %in% names(table)) {
        table[["p for trend"]][[row_start]] <- ttest_append_marker(table[["p for trend"]][[row_start]], trend_marker)
        add_cell_marker(row_start, "p for trend", trend_marker)
      }
    }
    row_start <- row_start + n_rows
  }
  if (length(cell_markers) > 0) {
    attr(table, "note_markers") <- ttest_bind_result_rows(cell_markers)
  }
  list(table = table, notes = notes)
}

ttest_analysis_note_line <- function(items) {
  items <- Filter(function(item) !is.null(item) && !is.null(item$notes), items %||% list())
  if (length(items) == 0) return("")

  posthoc_notes <- vapply(items, function(item) {
    posthoc <- item$notes$posthoc %||% ""
    if (!nzchar(posthoc)) return("")
    posthoc
  }, character(1))
  posthoc_notes <- unique(posthoc_notes[nzchar(posthoc_notes)])
  notes <- ttest_numbered_notes(items)
  format_note_rows <- function(note_rows) {
    if (!is.data.frame(note_rows) || nrow(note_rows) == 0) return(character(0))
    ifelse(nzchar(note_rows$marker), sprintf("%s. %s", note_rows$marker, note_rows$note), note_rows$note)
  }
  marker_parts <- if (is.data.frame(notes) && nrow(notes) > 0) {
    marker_notes <- notes[nzchar(notes$marker), , drop = FALSE]
    if (nrow(marker_notes) > 0) {
      marker_notes <- marker_notes[order(suppressWarnings(as.integer(marker_notes$marker))), , drop = FALSE]
    }
    format_note_rows(marker_notes)
  } else {
    character(0)
  }
  unmarked_parts <- if (is.data.frame(notes) && nrow(notes) > 0) {
    format_note_rows(notes[!nzchar(notes$marker), , drop = FALSE])
  } else {
    character(0)
  }
  parts <- c(marker_parts, unmarked_parts)
  if (length(posthoc_notes) > 0) {
    parts <- c(parts, sprintf("Post-hoc: %s.", paste(posthoc_notes, collapse = ", ")))
  }
  if (any(vapply(items, function(item) isTRUE(item$notes$mean_sd), logical(1)))) {
    parts <- c(parts, "M \u00B1 SD = mean \u00B1 standard deviation.")
  }
  paste(parts, collapse = " ")
}

ttest_bind_result_rows <- function(rows) {
  rows <- Filter(function(row) is.data.frame(row) && nrow(row) > 0, rows %||% list())
  if (length(rows) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  marker_rows <- list()
  row_offset <- 0L
  for (row in rows) {
    markers <- attr(row, "note_markers", exact = TRUE)
    if (is.data.frame(markers) && nrow(markers) > 0) {
      markers <- markers[nzchar(as.character(markers$marker %||% "")), , drop = FALSE]
      if (nrow(markers) > 0) {
        markers$row <- as.integer(markers$row) + row_offset
        marker_rows[[length(marker_rows) + 1L]] <- markers
      }
    }
    row_offset <- row_offset + nrow(row)
  }
  out <- analysis_bind_rows(rows)
  if (length(marker_rows) > 0) {
    markers <- analysis_bind_rows(marker_rows)
    attr(out, "note_markers") <- markers
  }
  out
}

ttest_single_result <- function(data, dependent, factor, variable_info, labels, category_table, options) {
  values <- as.numeric(data[[dependent]])
  groups <- as.character(data[[factor]])
  keep <- !is.na(values) & !is.na(groups) & nzchar(groups)
  values <- values[keep]
  groups <- groups[keep]
  levels <- ttest_level_order(unique(groups))
  if (length(levels) < 2 || length(values) < 3) {
    return(list(
      skipped = ttest_skipped_item(
        dependent,
        factor,
        "At least two groups and three complete observations are required.",
        length(values),
        variable_info,
        labels,
        category_table
      )
    ))
  }

  dependent_measure <- ttest_measurement(dependent, variable_info)
  factor_measure <- ttest_measurement(factor, variable_info)
  group_count <- length(levels)
  force_nonparametric <- dependent_measure == "ordered" || isTRUE(options$force_nonparametric)
  counts <- ttest_group_counts(groups, levels)
  small_groups <- names(counts)[counts < 2]
  if (length(small_groups) > 0) {
    return(list(
      skipped = ttest_skipped_item(
        dependent,
        factor,
        sprintf("Each group must have at least 2 valid observations. Insufficient group(s): %s.", paste(small_groups, collapse = ", ")),
        length(values),
        variable_info,
        labels,
        category_table
      )
    ))
  }
  if (length(unique(values)) < 2) {
    return(list(
      skipped = ttest_skipped_item(
        dependent,
        factor,
        "The dependent variable has no variance after complete-case filtering.",
        length(values),
        variable_info,
        labels,
        category_table
      )
    ))
  }
  warning_rows <- list()
  zero_sd_groups <- ttest_zero_sd_groups(values, groups, levels)
  if (length(zero_sd_groups) > 0) {
    warning_rows[[length(warning_rows) + 1]] <- ttest_warning_item(
      dependent,
      factor,
      sprintf("Zero standard deviation in group(s): %s. Variance-sensitive statistics may be unavailable or interpreted cautiously.", paste(zero_sd_groups, collapse = ", ")),
      variable_info,
      labels,
      category_table
    )
  }

  normality_method <- tolower(as.character(options$normality_method %||% "skew_kurtosis"))
  normality_study_type <- tolower(as.character(options$normality_study_type %||% "survey"))
  if (!normality_study_type %in% c("survey", "experimental")) {
    normality_study_type <- "survey"
  }
  normality_enabled <- isTRUE(options$normality_enabled %||% TRUE)
  normality <- if (isTRUE(options$force_nonparametric)) {
    list(method = "Nonparametric test", normal = FALSE, detail = "Selected by analysis menu")
  } else if (isTRUE(force_nonparametric)) {
    list(method = "Ordinal dependent variable", normal = FALSE, detail = "Nonparametric test used")
  } else if (!normality_enabled || identical(normality_method, "none")) {
    list(method = "Normality not checked", normal = TRUE, detail = "Parametric test selected by option")
  } else if (identical(normality_method, "ks")) {
    if (identical(normality_study_type, "experimental")) {
      ttest_normality_ks_grouped(values, groups)
    } else {
      ttest_normality_ks_overall(values)
    }
  } else if (identical(normality_method, "sw")) {
    if (identical(normality_study_type, "experimental")) {
      ttest_normality_shapiro_grouped(values, groups)
    } else {
      ttest_normality_shapiro_overall(values)
    }
  } else {
    ttest_normality_skew_kurtosis(values)
  }

  parametric <- isTRUE(normality$normal) && !isTRUE(force_nonparametric)
  levene_p <- NA_real_
  equal_variance <- NA
  analysis <- ""
  statistic <- NA_real_
  statistic_df <- numeric(0)
  statistic_label <- ""
  p_value <- NA_real_
  test_type <- ""
  posthoc_label <- ""
  letters <- NULL
  p_matrix <- NULL

  if (group_count == 2) {
    if (parametric) {
      levene_p <- ttest_levene_p(values, groups)
      equal_variance <- is.na(levene_p) || levene_p >= .05
      fit <- ttest_safe_call(stats::t.test(values ~ as.factor(groups), var.equal = isTRUE(equal_variance)))
      if (is.null(fit)) {
        return(list(
          skipped = ttest_skipped_item(
            dependent,
            factor,
            "Independent samples t-test could not be computed, commonly because one or more groups have zero variance.",
            length(values),
            variable_info,
            labels,
            category_table
          ),
          warnings = ttest_bind_result_rows(warning_rows)
        ))
      }
      statistic <- unname(fit$statistic)
      statistic_df <- unname(fit$parameter)
      statistic_label <- "t"
      p_value <- fit$p.value
      analysis <- if (isTRUE(equal_variance)) "Independent samples t-test" else "Welch t-test"
      test_type <- "t"
    } else {
      fit <- ttest_safe_call(stats::wilcox.test(values ~ as.factor(groups), exact = FALSE))
      if (is.null(fit)) {
        return(list(
          skipped = ttest_skipped_item(
            dependent,
            factor,
            "Mann-Whitney U test could not be computed for this variable-group combination.",
            length(values),
            variable_info,
            labels,
            category_table
          ),
          warnings = ttest_bind_result_rows(warning_rows)
        ))
      }
      statistic <- ttest_mann_whitney_z(values, groups)
      statistic_label <- "z"
      p_value <- fit$p.value
      analysis <- "Mann-Whitney U test (Wilcoxon rank-sum test)"
      test_type <- "mw"
    }
  } else if (parametric) {
    levene_p <- ttest_levene_p(values, groups)
    equal_variance <- is.na(levene_p) || levene_p >= .05
    if (isTRUE(equal_variance)) {
      fit <- ttest_safe_call(stats::aov(values ~ as.factor(groups)))
      fit_summary <- if (is.null(fit)) NULL else ttest_safe_call(summary(fit)[[1]])
      if (is.null(fit_summary)) {
        return(list(
          skipped = ttest_skipped_item(
            dependent,
            factor,
            "One-way ANOVA could not be computed for this variable-group combination.",
            length(values),
            variable_info,
            labels,
            category_table
          ),
          warnings = ttest_bind_result_rows(warning_rows)
        ))
      }
      statistic <- as.numeric(fit_summary[["F value"]][[1]])
      statistic_df <- c(as.numeric(fit_summary[["Df"]][[1]]), as.numeric(fit_summary[["Df"]][[2]]))
      statistic_label <- "F"
      p_value <- as.numeric(fit_summary[["Pr(>F)"]][[1]])
      analysis <- "One-way ANOVA"
      test_type <- "anova"
      if (!is.na(p_value) && p_value < .05) {
        method <- tolower(options$post_hoc_method %||% "scheffe")
        posthoc_label <- switch(method, tukey = "Tukey HSD", duncan = "Duncan multiple range test", bonferroni = "Bonferroni post-hoc test", "Scheffe post-hoc test")
        p_matrix <- ttest_safe_call(ttest_parametric_anova_pairwise(values, groups, method))
        if (!is.null(p_matrix)) {
          letters <- ttest_safe_call(ttest_group_letters(values, groups, p_matrix, ordered = FALSE))
        } else {
          posthoc_label <- ""
        }
      }
    } else {
      if (length(zero_sd_groups) > 0) {
        return(list(
          skipped = ttest_skipped_item(
            dependent,
            factor,
            "Welch ANOVA was not performed because one or more groups have zero variance.",
            length(values),
            variable_info,
            labels,
            category_table
          ),
          warnings = ttest_bind_result_rows(warning_rows)
        ))
      }
      fit <- ttest_safe_call(stats::oneway.test(values ~ as.factor(groups), var.equal = FALSE))
      if (is.null(fit)) {
        return(list(
          skipped = ttest_skipped_item(
            dependent,
            factor,
            "Welch ANOVA could not be computed for this variable-group combination.",
            length(values),
            variable_info,
            labels,
            category_table
          ),
          warnings = ttest_bind_result_rows(warning_rows)
        ))
      }
      statistic <- unname(fit$statistic)
      statistic_df <- unname(fit$parameter)
      statistic_label <- "F"
      p_value <- fit$p.value
      analysis <- "Welch ANOVA"
      test_type <- "anova"
      if (!is.na(p_value) && p_value < .05) {
        posthoc_label <- "Games-Howell"
        p_matrix <- ttest_safe_call(ttest_games_howell_pairwise(values, groups))
        if (!is.null(p_matrix)) {
          letters <- ttest_safe_call(ttest_group_letters(values, groups, p_matrix, ordered = FALSE))
        } else {
          posthoc_label <- ""
        }
      }
    }
  } else {
    fit <- ttest_safe_call(stats::kruskal.test(values ~ as.factor(groups)))
    if (is.null(fit)) {
      return(list(
        skipped = ttest_skipped_item(
          dependent,
          factor,
          "Kruskal-Wallis test could not be computed for this variable-group combination.",
          length(values),
          variable_info,
          labels,
          category_table
        ),
        warnings = ttest_bind_result_rows(warning_rows)
      ))
    }
    statistic <- unname(fit$statistic)
    statistic_df <- unname(fit$parameter)
    statistic_label <- stat_chisq_label()
    p_value <- fit$p.value
    analysis <- "Kruskal-Wallis test"
    test_type <- "kw"
    if (!is.na(p_value) && p_value < .05) {
      method <- tolower(options$nonparametric_post_hoc_method %||% "bonferroni")
      if (!method %in% c("bonferroni", "holm")) {
        method <- "bonferroni"
      }
      correction_label <- if (identical(method, "holm")) "Holm Bonferroni" else "Bonferroni correction"
      posthoc_label <- sprintf("Pairwise Wilcoxon rank-sum test with %s", correction_label)
      p_matrix <- ttest_safe_call(ttest_nonparametric_pairwise(values, groups, method))
      if (!is.null(p_matrix)) {
        letters <- ttest_safe_call(ttest_group_letters(values, groups, p_matrix, ordered = FALSE))
      } else {
        posthoc_label <- ""
      }
    }
  }

  summaries <- do.call(rbind, ttest_group_summary(values, groups, levels, median_iqr = isTRUE(options$median_iqr)))
  display_values <- ttest_display_levels(factor, summaries$Value, category_table)
  trend_p <- ""
  trend_method <- ""
  if (isTRUE(options$trend_analysis) && factor_measure == "ordered") {
    trend_result <- ttest_safe_call(ttest_trend_analysis(
      values,
      groups,
      normal = parametric,
      equal_variance = isTRUE(equal_variance),
      ordinal = dependent_measure == "ordered",
      robust = isTRUE(options$robust_trend)
    ))
    if (!is.null(trend_result)) {
      trend_p <- format_p(trend_result$p.value)
      trend_method <- trend_result$method %||% ""
    }
  }
  p_note <- ttest_p_note(test_type, analysis)
  trend_note <- ttest_trend_note(trend_method)
  effect_size_text <- if (isTRUE(options$effect_size)) {
    ttest_safe_call(ttest_effect_size(values, groups, test_type), "")
  } else {
    ""
  }
  rows <- data.frame(
    Variable = "",
    Value = display_values,
    M = summaries$M,
    SD = summaries$SD,
    Statistic = "",
    p = "",
    `Effect size` = "",
    `p for trend` = "",
    `post-hoc` = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rows$Variable[[1]] <- ttest_display_variable(factor, variable_info, labels, category_table)
  rows$Statistic[[1]] <- if (isTRUE(options$show_df)) {
    ttest_format_statistic(statistic, statistic_df)
  } else {
    format_decimal3(statistic)
  }
  p_text <- format_p(p_value)
  rows$p[[1]] <- if (is.na(p_text)) "" else paste0(p_text, p_note$symbol %||% "")
  rows[["Effect size"]][[1]] <- effect_size_text
  rows[["p for trend"]][[1]] <- if (is.na(trend_p) || !nzchar(trend_p)) "" else paste0(trend_p, trend_note$symbol %||% "")
  if (isTRUE(options$mean_sd)) {
    rows[["M \u00B1 SD"]] <- ifelse(
      nzchar(rows$M) & nzchar(rows$SD),
      paste0(rows$M, "\u00A0\u00B1\u00A0", rows$SD),
      ""
    )
    rows <- rows[, c("Variable", "Value", "M \u00B1 SD", "Statistic", "p", "Effect size", "p for trend", "post-hoc"), drop = FALSE]
  }

  if (nzchar(posthoc_label) && isTRUE(options$ordered_significance) && !is.null(p_matrix)) {
    rows <- analysis_apply_ordered_posthoc_markers(
      rows,
      estimates = suppressWarnings(as.numeric(summaries$M)),
      levels = summaries$Value,
      p_matrix = p_matrix,
      label_column = "Value"
    )
  } else if (nzchar(posthoc_label)) {
    rows[["post-hoc"]] <- ttest_lookup_letters(letters, summaries$Value)
  }
  if (!isTRUE(options$mean_sd)) {
    rows <- ttest_preserve_note_markers(rows, rows[, ttest_result_table_columns, drop = FALSE])
  }
  posthoc_table <- ttest_safe_call(ttest_posthoc_table(factor, levels, p_matrix, posthoc_label, variable_info, labels, category_table), data.frame(stringsAsFactors = FALSE))

  normality_text <- if ((isTRUE(normality_enabled) || isTRUE(options$force_nonparametric)) && nzchar(normality$detail %||% "")) {
    normality$detail
  } else {
    normality$method
  }

  overview <- data.frame(
    `Dependent variable` = ttest_display_variable(dependent, variable_info, labels, category_table),
    `Independent variable` = ttest_display_variable(factor, variable_info, labels, category_table),
    N = as.character(length(values)),
    Normality = normality_text,
    `Homogeneity p` = if (is.na(levene_p)) "" else format_p(levene_p),
    Analysis = analysis,
    `Post-hoc` = posthoc_label,
    Package = "stats",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  list(
    title = overview$`Dependent variable`[[1]],
    dependent = dependent,
    factor = factor,
    table = rows,
    posthoc = posthoc_table,
    overview = overview,
    statistic_label = statistic_label,
    notes = list(
      factor = overview$`Independent variable`[[1]],
      analysis = analysis,
      p_key = p_note$key %||% "",
      p_symbol = p_note$symbol %||% "",
      p_note = p_note$note %||% "",
      effect_size = if (isTRUE(options$effect_size)) ttest_effect_size_name(test_type) else "",
      posthoc = posthoc_label,
      trend = trend_method,
      trend_key = trend_note$key %||% "",
      trend_symbol = trend_note$symbol %||% "",
      trend_note = trend_note$note %||% "",
      mean_sd = isTRUE(options$mean_sd)
    ),
    warnings = ttest_bind_result_rows(warning_rows)
  )
}

ttest_model_overview_wide <- function(overview, dependents = NULL, variable_info = NULL, labels = character(0), category_table = NULL) {
  if (!is.data.frame(overview) || nrow(overview) == 0 || "Message" %in% names(overview)) {
    return(overview)
  }
  if (!all(c("Dependent variable", "Independent variable") %in% names(overview))) {
    return(overview)
  }

  dependents <- as.character(dependents %||% character(0))
  dependent_labels <- if (length(dependents) > 0) {
    vapply(dependents, ttest_display_variable, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  } else {
    unique(as.character(overview$`Dependent variable`))
  }
  dependent_labels <- unique(dependent_labels[nzchar(dependent_labels)])
  factors <- unique(as.character(overview$`Independent variable`))

  short_analysis <- function(value) {
    value <- as.character(value %||% "")
    switch(
      value,
      "Independent samples t-test" = "t-test",
      "Welch t-test" = "Welch t",
      "One-way ANOVA" = "ANOVA",
      "Welch ANOVA" = "Welch",
      "Kruskal-Wallis test" = "K-W",
      "Mann-Whitney U test" = "M-W",
      value
    )
  }
  normality_satisfied <- function(value) {
    value <- as.character(value %||% "")
    if (!nzchar(value)) return("")
    p_values <- suppressWarnings(as.numeric(unlist(regmatches(value, gregexpr("(?<=\\()[<>.]?[0-9.]+(?=\\))", value, perl = TRUE)))))
    if (length(p_values) > 0 && any(!is.na(p_values))) {
      return(if (all(p_values[!is.na(p_values)] >= .05)) "\uc815\uaddc\uc131 \ub9cc\uc871" else "\uc815\uaddc\uc131 \ubd88\ub9cc\uc871")
    }
    numeric_values <- suppressWarnings(as.numeric(unlist(regmatches(value, gregexpr("-?[0-9.]+", value)))))
    if (grepl("skew\\s*=|kurtosis\\s*=", value, ignore.case = TRUE) && length(numeric_values) >= 2) {
      skew <- numeric_values[[1]]
      kurtosis <- numeric_values[[2]]
      return(if (!is.na(skew) && !is.na(kurtosis) && abs(skew) <= 2 && abs(kurtosis) <= 7) "\uc815\uaddc\uc131 \ub9cc\uc871" else "\uc815\uaddc\uc131 \ubd88\ub9cc\uc871")
    }
    ""
  }
  homogeneity_satisfied <- function(value) {
    value <- as.character(value %||% "")
    if (!nzchar(value)) return("")
    if (identical(value, "<.001")) return("\ub4f1\ubd84\uc0b0\uc131 \ubd88\ub9cc\uc871")
    p_value <- suppressWarnings(as.numeric(sub("^<", "", value)))
    if (is.na(p_value)) return("")
    if (p_value >= .05) "\ub4f1\ubd84\uc0b0\uc131 \ub9cc\uc871" else "\ub4f1\ubd84\uc0b0\uc131 \ubd88\ub9cc\uc871"
  }
  metric_values <- function(row, metric) {
    if (identical(metric, "Reason")) {
      parts <- c(
        if ("Normality" %in% names(row)) normality_satisfied(row$Normality) else "",
        if ("Homogeneity p" %in% names(row)) homogeneity_satisfied(row$`Homogeneity p`) else "",
        if ("Post-hoc" %in% names(row) && nzchar(row$`Post-hoc` %||% "")) "\uc0ac\ud6c4\ubd84\uc11d \uc788\uc74c" else ""
      )
      return(paste(parts[nzchar(parts)], collapse = "\n"))
    }
    if (identical(metric, "Analysis")) {
      return(short_analysis(row[[metric]][[1]]))
    }
    as.character(row[[metric]][[1]] %||% "")
  }

  metrics <- c(intersect(c("N", "Analysis"), names(overview)), "Reason")
  metric_labels <- c(N = "N", Analysis = "\ubd84\uc11d", Reason = "\uc0ac\uc720")
  rows <- list()
  for (factor in factors) {
    for (metric_index in seq_along(metrics)) {
      metric <- metrics[[metric_index]]
      row <- c(
        `Independent variable` = if (metric_index == 1) factor else "",
        Item = metric_labels[[metric]]
      )
      for (dependent in dependent_labels) {
        matched <- overview[
          as.character(overview$`Dependent variable`) == dependent &
            as.character(overview$`Independent variable`) == factor,
          ,
          drop = FALSE
        ]
        row[[dependent]] <- if (nrow(matched) > 0) metric_values(matched[1, , drop = FALSE], metric) else ""
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  if (length(rows) == 0) {
    return(overview)
  }
  output <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE, check.names = FALSE)
  names(output) <- c("Independent variable", "Item", dependent_labels)
  attr(output, "dependent_count") <- length(dependent_labels)
  output
}

ttest_model_overview_landscape <- function(table) {
  count <- attr(table, "dependent_count", exact = TRUE)
  if (is.null(count)) {
    left_columns <- if ("Item" %in% names(table)) {
      if (identical(names(table)[[1]], "Item")) 1L else 2L
    } else {
      1L
    }
    count <- max(0L, ncol(table) - left_columns)
  }
  is.finite(count) && as.integer(count) >= 5L
}

ttest_assumption_review_wide <- function(overview, dependents = NULL, variable_info = NULL, labels = character(0), category_table = NULL) {
  if (!is.data.frame(overview) || nrow(overview) == 0 || "Message" %in% names(overview)) {
    return(NULL)
  }
  if (!all(c("Dependent variable", "Independent variable") %in% names(overview))) {
    return(NULL)
  }

  dependents <- as.character(dependents %||% character(0))
  dependent_labels <- if (length(dependents) > 0) {
    vapply(dependents, ttest_display_variable, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  } else {
    unique(as.character(overview$`Dependent variable`))
  }
  dependent_labels <- unique(dependent_labels[nzchar(dependent_labels)])
  factors <- unique(as.character(overview$`Independent variable`))
  metrics <- c("Normality", "Homogeneity p", "Post-hoc", "Package")
  metric_labels <- c(
    Normality = "\uc815\uaddc\uc131",
    `Homogeneity p` = "\ub4f1\ubd84\uc0b0\uc131",
    `Post-hoc` = "\uc0ac\ud6c4\ubd84\uc11d",
    Package = "\ud328\ud0a4\uc9c0"
  )
  rows <- list()

  for (factor in factors) {
    for (metric_index in seq_along(metrics)) {
      metric <- metrics[[metric_index]]
      row <- c(
        `Independent variable` = if (metric_index == 1) factor else "",
        Item = unname(metric_labels[[metric]] %||% metric)
      )
      for (dependent in dependent_labels) {
        matched <- overview[
          as.character(overview$`Dependent variable`) == dependent &
            as.character(overview$`Independent variable`) == factor,
          ,
          drop = FALSE
        ]
        row[[dependent]] <- if (nrow(matched) > 0 && metric %in% names(matched)) {
          as.character(matched[[metric]][[1]] %||% "")
        } else {
          ""
        }
      }
      rows[[length(rows) + 1]] <- row
    }
  }

  output <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE, check.names = FALSE)
  names(output) <- c("Independent variable", "Item", dependent_labels)
  attr(output, "dependent_count") <- length(dependent_labels)
  output
}

ttest_result_flat_overview <- function(result) {
  rows <- lapply(result$results %||% list(), function(item) item$overview)
  rows <- rows[vapply(rows, is.data.frame, logical(1))]
  if (length(rows) == 0) {
    return(NULL)
  }
  ttest_bind_result_rows(rows)
}

ttest_result_overview_tables <- function(result) {
  overview <- result$overview
  assumption_review <- result$assumption_review
  if (is.data.frame(overview) && nrow(overview) > 0) {
    return(list(
      assumption_review = assumption_review,
      overview = overview
    ))
  }

  flat <- ttest_result_flat_overview(result)
  if (!is.data.frame(flat) || nrow(flat) == 0) {
    return(list(
      assumption_review = assumption_review,
      overview = overview
    ))
  }

  list(
    assumption_review = if (is.data.frame(assumption_review) && nrow(assumption_review) > 0) {
      assumption_review
    } else {
      ttest_assumption_review_wide(flat, result$dependents %||% character(0))
    },
    overview = ttest_model_overview_wide(flat, result$dependents %||% character(0))
  )
}

prepare_ttest_anova_results <- function(
  data,
  dependents,
  factors,
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  options = list()
) {
  if (!is.data.frame(data)) {
    stop("No data frame is available for t-test / ANOVA.")
  }
  dependents <- intersect(as.character(dependents %||% character(0)), names(data))
  factors <- intersect(as.character(factors %||% character(0)), names(data))
  results <- list()
  overview_rows <- list()
  warning_rows <- list()
  skipped_rows <- list()
  for (dependent in dependents) {
    dependent_items <- list()
    for (factor in factors) {
      item <- tryCatch(
        ttest_single_result(data, dependent, factor, variable_info, labels, category_table, options),
        error = function(e) {
          list(skipped = ttest_skipped_item(dependent, factor, conditionMessage(e), NA_integer_, variable_info, labels, category_table))
        }
      )
      if (is.data.frame(item$warnings) && nrow(item$warnings) > 0) {
        warning_rows[[length(warning_rows) + 1]] <- item$warnings
      }
      if (is.data.frame(item$skipped) && nrow(item$skipped) > 0) {
        skipped_rows[[length(skipped_rows) + 1]] <- item$skipped
        next
      }
      if (is.null(item) || !is.data.frame(item$table)) next
      dependent_items[[length(dependent_items) + 1]] <- item
      overview_rows[[length(overview_rows) + 1]] <- item$overview
    }
    if (length(dependent_items) > 0) {
      result_columns <- if (isTRUE(options$mean_sd)) {
        c("Variable", "Value", "M \u00B1 SD", "Statistic", "p", "Effect size", "p for trend", "post-hoc")
      } else {
        ttest_result_table_columns
      }
      combined_table <- ttest_bind_result_rows(lapply(dependent_items, function(item) item$table))
      combined_table <- ttest_preserve_note_markers(combined_table, combined_table[, result_columns, drop = FALSE])
      statistic_labels <- vapply(dependent_items, function(item) item$statistic_label %||% "", character(1))
      combined_table <- ttest_apply_statistic_heading(combined_table, statistic_labels)
      if (isTRUE(options$median_iqr) && !isTRUE(options$mean_sd)) {
        names(combined_table)[names(combined_table) == "M"] <- "Median"
        names(combined_table)[names(combined_table) == "SD"] <- "Q1~Q3"
      }
      has_trend_result <- any(vapply(dependent_items, function(item) {
        nzchar(as.character(item$notes$trend %||% ""))
      }, logical(1)))
      if (!isTRUE(has_trend_result) && "p for trend" %in% names(combined_table)) {
        combined_table[["p for trend"]] <- NULL
      }
      note_result <- ttest_apply_numbered_notes(combined_table, dependent_items)
      combined_table <- note_result$table
      if (isTRUE(options$show_df)) {
        attr(combined_table, "show_df") <- TRUE
      }
      if (isTRUE(options$mean_sd)) {
        attr(combined_table, "mean_sd") <- TRUE
      }
      if (isTRUE(has_trend_result)) {
        attr(combined_table, "trend_analysis") <- TRUE
      }
      note_line <- ttest_analysis_note_line(dependent_items)
      posthoc_table <- ttest_bind_result_rows(lapply(dependent_items, function(item) item$posthoc))
      results[[length(results) + 1]] <- list(
        title = ttest_display_variable(dependent, variable_info, labels, category_table),
        dependent = dependent,
        table = combined_table,
        posthoc = posthoc_table,
        note = note_line,
        overview = ttest_bind_result_rows(lapply(dependent_items, function(item) item$overview)),
        factors = lapply(dependent_items, function(item) item$factor)
      )
    }
  }
  flat_overview <- if (length(overview_rows) > 0) {
    ttest_bind_result_rows(overview_rows)
  } else {
    NULL
  }
  overview <- if (is.data.frame(flat_overview) && nrow(flat_overview) > 0) {
    ttest_model_overview_wide(flat_overview, dependents, variable_info, labels, category_table)
  } else {
    data.frame(Message = "No valid t-test / ANOVA result.", stringsAsFactors = FALSE)
  }
  assumption_review <- if (is.data.frame(flat_overview) && nrow(flat_overview) > 0) {
    ttest_assumption_review_wide(flat_overview, dependents, variable_info, labels, category_table)
  } else {
    NULL
  }
  list(
    type = "ttest_anova",
    dependents = dependents,
    factors = factors,
    assumption_review = assumption_review,
    overview = overview,
    results = results,
    warnings = ttest_bind_result_rows(warning_rows),
    skipped = ttest_bind_result_rows(skipped_rows),
    options = options
  )
}

ttest_anova_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  if (!is.null(result$error)) {
    return(tags$div(class = "analysis-error", result$error))
  }

  overview_tables <- ttest_result_overview_tables(result)
  overview_landscape_class <- if (isTRUE(ttest_model_overview_landscape(overview_tables$overview))) " landscape-table-panel" else ""
  assumption_landscape_class <- if (isTRUE(ttest_model_overview_landscape(overview_tables$assumption_review))) " landscape-table-panel" else ""

  sections <- list(
    tags$div(
      class = paste0("result-section regression-result-panel ttest-anova-overview-panel", overview_landscape_class),
      tags$h3("Model overview"),
      model_overview_html_table(overview_tables$overview)
    )
  )

  for (item in result$results %||% list()) {
    sections[[length(sections) + 1]] <- tags$div(
      class = "result-section regression-result-panel ttest-anova-result-panel",
      tags$h3(item$title),
      coefficient_html_table(item$table, note_line = item$note %||% ""),
      if (is.data.frame(item$posthoc) && nrow(item$posthoc) > 0) {
        tags$div(
          class = "ttest-anova-posthoc-section",
          tags$h4("Post-hoc"),
          coefficient_html_table(item$posthoc)
        )
      }
    )
  }

  if (is.data.frame(overview_tables$assumption_review) && nrow(overview_tables$assumption_review) > 0) {
    sections[[length(sections) + 1]] <- tags$div(
      class = paste0("result-section regression-result-panel ttest-anova-assumption-review-panel", assumption_landscape_class),
      tags$h3("가정 검토"),
      model_overview_html_table(overview_tables$assumption_review)
    )
  }

  diagnostics_section <- analysis_diagnostics_section(result$warnings, result$skipped)
  if (!is.null(diagnostics_section)) sections[[length(sections) + 1]] <- diagnostics_section

  do.call(tagList, sections)
}
