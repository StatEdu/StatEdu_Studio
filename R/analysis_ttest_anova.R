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
  for (level in levels) {
    group_values <- values[groups == level]
    group_values <- group_values[!is.na(group_values)]
    if (length(group_values) < 5 || isTRUE(stats::sd(group_values) == 0)) {
      next
    }
    standardized <- as.numeric(scale(group_values))
    p_values[[level]] <- suppressWarnings(stats::ks.test(standardized, "pnorm")$p.value)
  }
  available <- p_values[!is.na(p_values)]
  normal <- length(available) > 0 && all(available >= .05)
  list(
    method = "Kolmogorov-Smirnov by group",
    normal = normal,
    detail = paste(sprintf("%s=%s", names(available), vapply(available, format_p, character(1))), collapse = ", ")
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
  p_value <- suppressWarnings(stats::ks.test(standardized, "pnorm")$p.value)
  list(
    method = "Kolmogorov-Smirnov",
    normal = !is.na(p_value) && p_value >= .05,
    detail = sprintf("p=%s", format_p(p_value))
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
  for (level in levels) {
    group_values <- values[groups == level]
    group_values <- group_values[!is.na(group_values)]
    if (length(group_values) < 3 || length(group_values) > 5000 || isTRUE(stats::sd(group_values) == 0)) {
      next
    }
    p_values[[level]] <- suppressWarnings(stats::shapiro.test(group_values)$p.value)
  }
  available <- p_values[!is.na(p_values)]
  normal <- length(available) > 0 && all(available >= .05)
  detail <- if (length(available) > 0) {
    paste(sprintf("%s=%s", names(available), vapply(available, format_p, character(1))), collapse = ", ")
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
  p_value <- suppressWarnings(stats::shapiro.test(values)$p.value)
  list(
    method = "Shapiro-Wilk",
    normal = !is.na(p_value) && p_value >= .05,
    detail = sprintf("p=%s", format_p(p_value))
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
    fit <- stats::aov(y ~ g, data = data)
    summary_fit <- summary(fit)[[1]]
    mse <- as.numeric(summary_fit[["Mean Sq"]][[2]])
    df_error <- as.numeric(summary_fit[["Df"]][[2]])
    means <- tapply(data$y, data$g, mean)
    counts <- table(data$g)
    ordered_levels <- names(sort(means))
    ranks <- stats::setNames(seq_along(ordered_levels), ordered_levels)
    for (i in seq_len(k - 1)) {
      for (j in seq.int(i + 1, k)) {
        ni <- as.numeric(counts[[i]])
        nj <- as.numeric(counts[[j]])
        se <- sqrt(mse * (1 / ni + 1 / nj) / 2)
        range_size <- abs(ranks[[levels[[i]]]] - ranks[[levels[[j]]]]) + 1
        if (!is.finite(se) || se <= 0 || !is.finite(range_size)) next
        q_value <- abs(means[[i]] - means[[j]]) / se
        p <- stats::ptukey(q_value, nmeans = range_size, df = df_error, lower.tail = FALSE)
        matrix_out[i, j] <- matrix_out[j, i] <- p
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

ttest_nonparametric_pairwise <- function(values, groups) {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  levels <- levels(data$g)
  k <- length(levels)
  matrix_out <- matrix(1, nrow = k, ncol = k, dimnames = list(levels, levels))
  if (k < 2) return(matrix_out)
  pairwise <- stats::pairwise.wilcox.test(data$y, data$g, p.adjust.method = "bonferroni", exact = FALSE)
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
  for (level in levels) {
    placed <- FALSE
    if (length(letter_groups) > 0) {
      for (letter in names(letter_groups)) {
        members <- letter_groups[[letter]]
        p_values <- vapply(members, function(member) p_matrix[level, member] %||% 0, numeric(1))
        if (all(!is.na(p_values) & p_values >= .05)) {
          letter_groups[[letter]] <- c(members, level)
          assigned[[level]] <- paste0(assigned[[level]], letter)
          placed <- TRUE
        }
      }
    }
    if (!placed) {
      letter <- letters_pool[[length(letter_groups) + 1]]
      letter_groups[[letter]] <- level
      assigned[[level]] <- paste0(assigned[[level]], letter)
    }
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

ttest_effect_size <- function(values, groups, test_type) {
  data <- data.frame(y = as.numeric(values), g = as.factor(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data) == 0) return("")
  if (identical(test_type, "t")) {
    split_values <- split(data$y, data$g)
    if (length(split_values) != 2) return("")
    n1 <- length(split_values[[1]])
    n2 <- length(split_values[[2]])
    pooled <- sqrt(((n1 - 1) * stats::var(split_values[[1]]) + (n2 - 1) * stats::var(split_values[[2]])) / (n1 + n2 - 2))
    if (!is.finite(pooled) || pooled == 0) return("")
    return(format_decimal3((mean(split_values[[1]]) - mean(split_values[[2]])) / pooled))
  }
  if (identical(test_type, "anova")) {
    fit <- stats::aov(y ~ g, data = data)
    summary_fit <- summary(fit)[[1]]
    ss_between <- as.numeric(summary_fit[["Sum Sq"]][[1]])
    ss_total <- sum(summary_fit[["Sum Sq"]], na.rm = TRUE)
    if (!is.finite(ss_total) || ss_total == 0) return("")
    return(format_decimal3(ss_between / ss_total))
  }
  if (identical(test_type, "kw")) {
    kw <- stats::kruskal.test(y ~ g, data = data)
    k <- length(unique(data$g))
    epsilon <- (as.numeric(kw$statistic) - k + 1) / (nrow(data) - k)
    return(format_decimal3(epsilon))
  }
  ""
}

ttest_trend_p <- function(values, groups, normal = FALSE) {
  data <- data.frame(y = as.numeric(values), g = as.character(groups))
  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data) < 3) return("")
  levels <- ttest_level_order(unique(data$g))
  data$score <- match(data$g, levels)
  if (isTRUE(normal)) {
    fit <- stats::lm(y ~ score, data = data)
    return(format_p(summary(fit)$coefficients["score", "Pr(>|t|)"]))
  }
  test <- suppressWarnings(stats::cor.test(data$score, data$y, method = "spearman", exact = FALSE))
  format_p(test$p.value)
}

ttest_group_summary <- function(values, groups, levels) {
  lapply(levels, function(level) {
    group_values <- as.numeric(values[as.character(groups) == level])
    data.frame(
      Value = level,
      M = ttest_format_decimal(mean(group_values, na.rm = TRUE), 2),
      SD = ttest_format_decimal(ttest_sample_sd(group_values), 2),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
}

ttest_result_table_columns <- c("Variable", "Value", "M", "SD", "t or F", "p", "post-hoc")

ttest_bind_result_rows <- function(rows) {
  rows <- Filter(function(row) is.data.frame(row) && nrow(row) > 0, rows)
  if (length(rows) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    missing_columns <- setdiff(columns, names(row))
    for (column in missing_columns) {
      row[[column]] <- ""
    }
    row <- row[, columns, drop = FALSE]
    rownames(row) <- NULL
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
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
    return(NULL)
  }

  dependent_measure <- ttest_measurement(dependent, variable_info)
  factor_measure <- ttest_measurement(factor, variable_info)
  group_count <- length(levels)
  force_nonparametric <- dependent_measure == "ordered"

  normality_method <- tolower(as.character(options$normality_method %||% "skew_kurtosis"))
  normality_study_type <- tolower(as.character(options$normality_study_type %||% "survey"))
  if (!normality_study_type %in% c("survey", "experimental")) {
    normality_study_type <- "survey"
  }
  normality_enabled <- isTRUE(options$normality_enabled %||% TRUE)
  normality <- if (isTRUE(force_nonparametric)) {
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
  p_value <- NA_real_
  test_type <- ""
  posthoc_label <- ""
  letters <- NULL
  ordered_letters <- NULL

  if (group_count == 2) {
    if (parametric) {
      levene_p <- ttest_levene_p(values, groups)
      equal_variance <- is.na(levene_p) || levene_p >= .05
      fit <- stats::t.test(values ~ as.factor(groups), var.equal = isTRUE(equal_variance))
      statistic <- unname(fit$statistic)
      p_value <- fit$p.value
      analysis <- if (isTRUE(equal_variance)) "Independent samples t-test" else "Welch t-test"
      test_type <- "t"
    } else {
      fit <- suppressWarnings(stats::wilcox.test(values ~ as.factor(groups), exact = FALSE))
      statistic <- unname(fit$statistic)
      p_value <- fit$p.value
      analysis <- "Mann-Whitney U test (Wilcoxon rank-sum test)"
      test_type <- "mw"
    }
  } else if (parametric) {
    levene_p <- ttest_levene_p(values, groups)
    equal_variance <- is.na(levene_p) || levene_p >= .05
    if (isTRUE(equal_variance)) {
      fit <- stats::aov(values ~ as.factor(groups))
      fit_summary <- summary(fit)[[1]]
      statistic <- as.numeric(fit_summary[["F value"]][[1]])
      p_value <- as.numeric(fit_summary[["Pr(>F)"]][[1]])
      analysis <- "One-way ANOVA"
      test_type <- "anova"
      if (!is.na(p_value) && p_value < .05) {
        method <- tolower(options$post_hoc_method %||% "scheffe")
        posthoc_label <- switch(method, tukey = "Tukey", duncan = "Duncan", bonferroni = "Bonferroni", "Scheffe")
        p_matrix <- ttest_parametric_anova_pairwise(values, groups, method)
        letters <- ttest_group_letters(values, groups, p_matrix, ordered = FALSE)
        if (isTRUE(options$ordered_significance)) {
          ordered_letters <- ttest_group_letters(values, groups, p_matrix, ordered = TRUE)
        }
      }
    } else {
      fit <- stats::oneway.test(values ~ as.factor(groups), var.equal = FALSE)
      statistic <- unname(fit$statistic)
      p_value <- fit$p.value
      analysis <- "Welch ANOVA"
      test_type <- "anova"
      if (!is.na(p_value) && p_value < .05) {
        posthoc_label <- "Games-Howell"
        p_matrix <- ttest_games_howell_pairwise(values, groups)
        letters <- ttest_group_letters(values, groups, p_matrix, ordered = FALSE)
        if (isTRUE(options$ordered_significance)) {
          ordered_letters <- ttest_group_letters(values, groups, p_matrix, ordered = TRUE)
        }
      }
    }
  } else {
    fit <- stats::kruskal.test(values ~ as.factor(groups))
    statistic <- unname(fit$statistic)
    p_value <- fit$p.value
    analysis <- "Kruskal-Wallis test"
    test_type <- "kw"
    if (!is.na(p_value) && p_value < .05) {
      posthoc_label <- "Bonferroni"
      p_matrix <- ttest_nonparametric_pairwise(values, groups)
      letters <- ttest_group_letters(values, groups, p_matrix, ordered = FALSE)
      if (isTRUE(options$ordered_significance)) {
        ordered_letters <- ttest_group_letters(values, groups, p_matrix, ordered = TRUE)
      }
    }
  }

  summaries <- do.call(rbind, ttest_group_summary(values, groups, levels))
  display_values <- ttest_display_levels(factor, summaries$Value, category_table)
  rows <- data.frame(
    Variable = "",
    Value = display_values,
    M = summaries$M,
    SD = summaries$SD,
    `t or F` = "",
    p = "",
    `post-hoc` = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rows$Variable[[1]] <- ttest_display_variable(factor, variable_info, labels, category_table)
  rows$`t or F`[[1]] <- format_decimal3(statistic)
  p_text <- format_p(p_value)
  rows$p[[1]] <- if (is.na(p_text)) "" else p_text

  if (nzchar(posthoc_label)) {
    rows[["post-hoc"]] <- ttest_lookup_letters(letters, summaries$Value)
  }
  if (!is.null(ordered_letters)) {
    rows[["post-hoc"]] <- ttest_lookup_letters(ordered_letters, summaries$Value)
  }
  rows <- rows[, ttest_result_table_columns, drop = FALSE]

  overview <- data.frame(
    `Dependent variable` = ttest_display_variable(dependent, variable_info, labels, category_table),
    `Independent variable` = ttest_display_variable(factor, variable_info, labels, category_table),
    N = as.character(length(values)),
    Normality = sprintf("%s: %s", normality$method, if (isTRUE(normality$normal)) "satisfied" else "not satisfied"),
    `Levene p` = if (is.na(levene_p)) "" else format_p(levene_p),
    Analysis = analysis,
    `Post-hoc` = posthoc_label,
    Package = "stats",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (isTRUE(options$trend_analysis) && factor_measure == "ordered") {
    overview[["p for trend"]] <- ttest_trend_p(values, groups, parametric)
  }
  if (isTRUE(options$effect_size)) {
    overview[["Effect size"]] <- ttest_effect_size(values, groups, test_type)
  }

  list(
    title = overview$`Dependent variable`[[1]],
    dependent = dependent,
    factor = factor,
    table = rows,
    overview = overview
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
  for (dependent in dependents) {
    dependent_items <- list()
    for (factor in factors) {
      item <- ttest_single_result(data, dependent, factor, variable_info, labels, category_table, options)
      if (is.null(item)) next
      dependent_items[[length(dependent_items) + 1]] <- item
      overview_rows[[length(overview_rows) + 1]] <- item$overview
    }
    if (length(dependent_items) > 0) {
      results[[length(results) + 1]] <- list(
        title = ttest_display_variable(dependent, variable_info, labels, category_table),
        dependent = dependent,
        table = ttest_bind_result_rows(lapply(dependent_items, function(item) item$table))[, ttest_result_table_columns, drop = FALSE],
        overview = ttest_bind_result_rows(lapply(dependent_items, function(item) item$overview)),
        factors = lapply(dependent_items, function(item) item$factor)
      )
    }
  }
  overview <- if (length(overview_rows) > 0) {
    ttest_bind_result_rows(overview_rows)
  } else {
    data.frame(Message = "No valid t-test / ANOVA result.", stringsAsFactors = FALSE)
  }
  list(
    type = "ttest_anova",
    dependents = dependents,
    factors = factors,
    overview = overview,
    results = results,
    options = options
  )
}

ttest_anova_results_ui <- function(result) {
  if (is.null(result)) {
    return(empty_message("Move variables and click Run analysis."))
  }
  if (!is.null(result$error)) {
    return(tags$div(class = "analysis-error", result$error))
  }

  sections <- list(
    tags$div(
      class = "result-section",
      tags$h3("Model overview"),
      model_overview_html_table(result$overview)
    )
  )

  for (item in result$results %||% list()) {
    sections[[length(sections) + 1]] <- tags$div(
      class = "result-section",
      tags$h3(item$title),
      coefficient_html_table(item$table)
    )
  }

  do.call(tagList, sections)
}
