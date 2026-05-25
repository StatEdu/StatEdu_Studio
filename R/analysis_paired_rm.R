# Paired test for three or more repeated measurements.

paired_rm_complete_matrix <- function(data, variables, measurement) {
  values <- data[, variables, drop = FALSE]
  if (measurement %in% c("continuous", "ordered")) {
    values[] <- lapply(values, paired_numeric)
  } else {
    values[] <- lapply(values, as.character)
  }
  keep <- stats::complete.cases(values)
  values[keep, , drop = FALSE]
}

paired_rm_method_label <- function(method) {
  switch(
    method,
    rm_anova = "Standard RM ANOVA",
    rm_anova_gg = "RM ANOVA + Greenhouse-Geisser correction",
    rm_anova_wilks = "RM ANOVA + Wilks' lambda / GG correction",
    friedman = "Friedman test",
    cochran = "Cochran's Q test",
    method
  )
}

paired_rm_sphericity <- function(y) {
  n <- nrow(y)
  k <- ncol(y)
  if (n < 3 || k < 3) {
    return(list(w = NA_real_, p = NA_real_, satisfied = NA, epsilon = NA_real_))
  }
  diffs <- sweep(y[, -1, drop = FALSE], 1, y[, 1], "-")
  cov_matrix <- stats::cov(diffs)
  p <- ncol(diffs)
  det_value <- det(cov_matrix)
  trace_value <- sum(diag(cov_matrix))
  w <- if (is.finite(det_value) && det_value > 0 && is.finite(trace_value) && trace_value > 0) {
    det_value / ((trace_value / p) ^ p)
  } else {
    NA_real_
  }
  df <- p * (p + 1) / 2 - 1
  chi <- if (is.finite(w) && w > 0) -(n - 1 - (2 * p + 1) / 6) * log(w) else NA_real_
  p_value <- if (is.finite(chi) && df > 0) stats::pchisq(chi, df = df, lower.tail = FALSE) else NA_real_
  center <- diag(k) - matrix(1 / k, nrow = k, ncol = k)
  centered_cov <- center %*% stats::cov(y) %*% center
  epsilon <- (sum(diag(centered_cov)) ^ 2) / ((k - 1) * sum(centered_cov ^ 2))
  epsilon <- max(1 / (k - 1), min(1, epsilon))
  list(w = w, p = p_value, satisfied = is.finite(p_value) && p_value >= .05, epsilon = epsilon)
}

paired_rm_normality <- function(y, use_assumption = FALSE) {
  y <- as.matrix(y)
  n <- nrow(y)
  skew_values <- apply(y, 2, function(x) if (length(x) >= 3) as.numeric(psych::skew(x, na.rm = TRUE, type = 2)) else NA_real_)
  kurt_values <- apply(y, 2, function(x) if (length(x) >= 4) as.numeric(psych::kurtosi(x, na.rm = TRUE, type = 2)) else NA_real_)
  shapiro_values <- apply(y, 2, function(x) {
    if (length(x) >= 3 && length(x) <= 5000 && stats::sd(x, na.rm = TRUE) > 0) {
      suppressWarnings(stats::shapiro.test(x)$p.value)
    } else {
      NA_real_
    }
  })
  if (!isTRUE(use_assumption)) {
    return(list(method = "not checked", satisfied = TRUE, skewness = skew_values, kurtosis = kurt_values, shapiro_p = shapiro_values))
  }
  if (n >= 30) {
    satisfied <- all(abs(skew_values) < 2, na.rm = TRUE) && all(abs(kurt_values) < 7, na.rm = TRUE)
    method <- "Skewness and kurtosis"
  } else if (n >= 20) {
    satisfied <- all(shapiro_values >= .05, na.rm = TRUE)
    method <- "Shapiro-Wilk"
  } else {
    satisfied <- FALSE
    method <- "Shapiro-Wilk"
  }
  list(method = method, satisfied = isTRUE(satisfied), skewness = skew_values, kurtosis = kurt_values, shapiro_p = shapiro_values)
}

paired_rm_anova <- function(y) {
  y <- as.matrix(y)
  n <- nrow(y)
  k <- ncol(y)
  grand <- mean(y)
  time_means <- colMeans(y)
  subject_means <- rowMeans(y)
  ss_time <- n * sum((time_means - grand) ^ 2)
  ss_subject <- k * sum((subject_means - grand) ^ 2)
  ss_total <- sum((y - grand) ^ 2)
  ss_error <- ss_total - ss_time - ss_subject
  df_time <- k - 1
  df_error <- (n - 1) * (k - 1)
  ms_time <- ss_time / df_time
  ms_error <- ss_error / df_error
  f_value <- ms_time / ms_error
  p_value <- stats::pf(f_value, df_time, df_error, lower.tail = FALSE)
  list(f = f_value, df1 = df_time, df2 = df_error, p = p_value, ss_time = ss_time, ss_error = ss_error)
}

paired_rm_wilks <- function(y) {
  y <- as.matrix(y)
  n <- nrow(y)
  diffs <- sweep(y[, -1, drop = FALSE], 1, y[, 1], "-")
  p <- ncol(diffs)
  mean_diff <- colMeans(diffs)
  cov_diff <- stats::cov(diffs)
  inv_cov <- tryCatch(solve(cov_diff), error = function(e) NULL)
  if (is.null(inv_cov) || n <= p) {
    return(list(lambda = NA_real_, f = NA_real_, df1 = p, df2 = n - p, p = NA_real_))
  }
  t2 <- as.numeric(n * t(mean_diff) %*% inv_cov %*% mean_diff)
  f_value <- ((n - p) / ((n - 1) * p)) * t2
  lambda <- 1 / (1 + t2 / (n - 1))
  p_value <- stats::pf(f_value, p, n - p, lower.tail = FALSE)
  list(lambda = lambda, f = f_value, df1 = p, df2 = n - p, p = p_value)
}

paired_rm_cochran_q <- function(y) {
  y <- as.matrix(y)
  y <- matrix(ifelse(y %in% c("1", 1, TRUE), 1, 0), nrow = nrow(y), ncol = ncol(y), dimnames = dimnames(y))
  k <- ncol(y)
  col_totals <- colSums(y)
  row_totals <- rowSums(y)
  total <- sum(col_totals)
  denominator <- k * total - sum(row_totals ^ 2)
  q <- if (denominator > 0) (k - 1) * (k * sum(col_totals ^ 2) - total ^ 2) / denominator else NA_real_
  p <- if (is.finite(q)) stats::pchisq(q, df = k - 1, lower.tail = FALSE) else NA_real_
  list(q = q, df = k - 1, p = p)
}

paired_rm_partial_eta_squared <- function(anova) {
  if (!is.list(anova)) return(NA_real_)
  ss_time <- anova$ss_time %||% NA_real_
  ss_error <- anova$ss_error %||% NA_real_
  if (!is.finite(ss_time) || !is.finite(ss_error) || (ss_time + ss_error) <= 0) return(NA_real_)
  ss_time / (ss_time + ss_error)
}

paired_rm_kendalls_w <- function(statistic, n, k) {
  if (!is.finite(statistic) || !is.finite(n) || !is.finite(k) || n <= 0 || k <= 1) return(NA_real_)
  statistic / (n * (k - 1))
}

paired_rm_summary_table <- function(y, variable_info, labels, category_table) {
  data.frame(
    Time = vapply(colnames(y), paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    N = nrow(y),
    M = apply(y, 2, function(x) format_decimal3(mean(x, na.rm = TRUE))),
    SD = apply(y, 2, function(x) format_decimal3(stats::sd(x, na.rm = TRUE))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

paired_rm_group_label <- function(variables, variable_info, labels, category_table) {
  paste(
    vapply(variables, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    collapse = " - "
  )
}

paired_rm_pair_label <- function(a, b, variable_info, labels, category_table) {
  sprintf(
    "%s - %s",
    paired_display_name(a, variable_info, labels, category_table),
    paired_display_name(b, variable_info, labels, category_table)
  )
}

paired_rm_posthoc_scale <- function(y, method, adjustment, variable_info, labels, category_table) {
  pairs <- utils::combn(colnames(y), 2, simplify = FALSE)
  raw <- lapply(pairs, function(pair) {
    x <- y[[pair[[1]]]]
    z <- y[[pair[[2]]]]
    if (identical(method, "Paired t-test")) {
      test <- tryCatch(suppressWarnings(stats::t.test(z, x, paired = TRUE)), error = function(e) NULL)
      statistic <- if (is.null(test)) NA_real_ else unname(as.numeric(test$statistic))
      p <- if (is.null(test)) NA_real_ else as.numeric(test$p.value)
      label <- "t"
      effects <- paired_t_effects(z - x)
      effect_label <- "Hedges' g"
      effect <- paired_effect_value(effects$g)
    } else {
      test <- tryCatch(suppressWarnings(stats::wilcox.test(z, x, paired = TRUE, exact = FALSE)), error = function(e) NULL)
      statistic <- if (is.null(test)) NA_real_ else unname(as.numeric(test$statistic))
      p <- if (is.null(test)) NA_real_ else as.numeric(test$p.value)
      label <- "W"
      effect_label <- "r"
      effect <- paired_effect_value(paired_wilcoxon_r(p, z - x))
    }
    list(pair = pair, label = label, statistic = statistic, p = p, effect_label = effect_label, effect = effect)
  })
  p_adjusted <- stats::p.adjust(vapply(raw, `[[`, numeric(1), "p"), method = adjustment)
  do.call(rbind, lapply(seq_along(raw), function(index) {
    item <- raw[[index]]
    data.frame(
      Contrast = paired_rm_pair_label(item$pair[[1]], item$pair[[2]], variable_info, labels, category_table),
      Method = method,
      Statistic = item$label,
      Value = format_decimal3(item$statistic),
      p = format_p(item$p),
      `p adjusted` = format_p(p_adjusted[[index]]),
      `Effect size` = item$effect_label,
      ES = item$effect,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
}

paired_rm_posthoc_binary <- function(y, adjustment, variable_info, labels, category_table) {
  pairs <- utils::combn(colnames(y), 2, simplify = FALSE)
  raw <- lapply(pairs, function(pair) {
    tab <- paired_category_table(as.character(y[[pair[[1]]]]), as.character(y[[pair[[2]]]]))
    if (all(dim(tab) == c(2, 2))) {
      res <- paired_mcnemar_asymptotic(tab)
    } else {
      res <- list(statistic = NA_real_, p = NA_real_)
    }
    list(pair = pair, statistic = res$statistic, p = as.numeric(res$p))
  })
  p_adjusted <- stats::p.adjust(vapply(raw, `[[`, numeric(1), "p"), method = adjustment)
  do.call(rbind, lapply(seq_along(raw), function(index) {
    item <- raw[[index]]
    data.frame(
      Contrast = paired_rm_pair_label(item$pair[[1]], item$pair[[2]], variable_info, labels, category_table),
      Method = "McNemar test",
      Statistic = stat_chisq_label(FALSE),
      Value = format_decimal3(item$statistic),
      p = format_p(item$p),
      `p adjusted` = format_p(p_adjusted[[index]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
}

paired_rm_posthoc_notation <- function(variables, posthoc, means = NULL, labels = NULL, contrast_labels = NULL, alpha = .05) {
  if (!is.data.frame(posthoc) || nrow(posthoc) == 0) return("")
  display <- stats::setNames(as.character(labels %||% variables), variables)
  contrast_display <- stats::setNames(as.character(contrast_labels %||% variables), variables)
  sig <- matrix(FALSE, nrow = length(variables), ncol = length(variables), dimnames = list(variables, variables))
  for (i in seq_along(variables)) {
    if (i >= length(variables)) next
    for (j in seq.int(i + 1L, length(variables))) {
      first <- variables[[i]]
      second <- variables[[j]]
      contrast <- sprintf("%s - %s", contrast_display[[first]], contrast_display[[second]])
      reverse <- sprintf("%s - %s", contrast_display[[second]], contrast_display[[first]])
      row <- posthoc[posthoc$Contrast %in% c(contrast, reverse), , drop = FALSE]
      if (nrow(row) == 0) next
      p_text <- row$`p adjusted`[[1]]
      if (is.na(p_text) || !nzchar(p_text)) next
      p_value <- suppressWarnings(as.numeric(sub("^\\.", "0.", p_text)))
      if (startsWith(p_text, "<")) p_value <- 0
      if (!is.finite(p_value) || p_value >= alpha) next
      if (!is.null(means) && all(c(first, second) %in% names(means))) {
        left <- if (means[[first]] >= means[[second]]) first else second
        right <- if (identical(left, first)) second else first
      } else {
        left <- first
        right <- second
      }
      sig[left, right] <- TRUE
    }
  }
  statements <- character(0)
  for (start in variables) {
    lower <- variables[sig[start, variables]]
    if (length(lower) == 0) next
    chain <- start
    current <- start
    remaining <- lower
    while (length(remaining) > 0) {
      next_values <- remaining[sig[current, remaining]]
      if (length(next_values) == 0) break
      next_value <- next_values[[1]]
      chain <- c(chain, next_value)
      current <- next_value
      remaining <- setdiff(remaining, next_value)
    }
    rest <- setdiff(lower, chain[-1])
    if (length(chain) > 1 && length(rest) == 0) {
      statements <- c(statements, paste(display[chain], collapse = ">"))
    } else if (length(chain) > 1 && length(rest) > 0) {
      statements <- c(statements, paste0(paste(display[chain], collapse = ">"), ",", paste(display[rest], collapse = ",")))
    } else {
      statements <- c(statements, sprintf("%s>%s", display[[start]], paste(display[lower], collapse = ",")))
    }
  }
  paste(statements, collapse = "; ")
}

paired_rm_time_header_labels <- function(n) {
  if (n <= 0) return(character(0))
  c("pre", if (n > 1) paste0("post", seq_len(n - 1L)) else character(0))
}

paired_rm_option_time_labels <- function(options, n) {
  defaults <- paired_rm_time_header_labels(n)
  labels <- trimws(as.character(options$time_labels %||% defaults))
  if (length(labels) < n) {
    labels <- c(labels, defaults[seq.int(length(labels) + 1L, n)])
  }
  labels <- labels[seq_len(n)]
  ifelse(nzchar(labels), labels, defaults)
}

paired_rm_pairwise_effects <- function(result, variables, time_labels, contrast_labels) {
  posthoc <- result$posthoc
  if (!is.data.frame(posthoc) || nrow(posthoc) == 0 || !"ES" %in% names(posthoc)) {
    return(list(columns = character(0), labels = character(0), values = character(0)))
  }
  columns <- character(0)
  labels <- character(0)
  values <- character(0)
  if (length(variables) < 2L) {
    return(list(columns = columns, labels = labels, values = values))
  }
  i <- 1L
  for (j in seq.int(2L, length(variables))) {
    first <- variables[[i]]
    second <- variables[[j]]
    contrast <- sprintf("%s - %s", contrast_labels[[first]], contrast_labels[[second]])
    reverse <- sprintf("%s - %s", contrast_labels[[second]], contrast_labels[[first]])
    row <- posthoc[posthoc$Contrast %in% c(contrast, reverse), , drop = FALSE]
    column <- sprintf("ES_%d_%d", i, j)
    columns <- c(columns, column)
    labels <- c(labels, sprintf("%s-%s", time_labels[[i]], time_labels[[j]]))
    values <- c(values, if (nrow(row) > 0) as.character(row$ES[[1]] %||% "") else "")
  }
  list(columns = columns, labels = labels, values = values)
}

paired_rm_display_table <- function(result) {
  variables <- as.character(result$variables %||% character(0))
  if (length(variables) == 0 || !is.data.frame(result$summary) || nrow(result$summary) == 0) {
    return(result$table)
  }
  time_labels <- paired_rm_option_time_labels(result$options %||% list(), nrow(result$summary))
  contrast_labels <- stats::setNames(as.character(result$summary$Time %||% variables), variables)
  means <- stats::setNames(suppressWarnings(as.numeric(sub("^\\.", "0.", result$summary$M))), variables)
  row <- data.frame(
    `Repeated variables` = result$group_label,
    N = result$table$N[[1]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (index in seq_along(time_labels)) {
    row[[paste0("Time", index, "_label")]] <- time_labels[[index]]
    row[[paste0("Time", index, "_M")]] <- result$summary$M[[index]]
    row[[paste0("Time", index, "_SD")]] <- result$summary$SD[[index]]
  }
  statistic_label <- as.character(result$table$Statistic[[1]] %||% "Statistic")
  row[["StatisticLabel"]] <- statistic_label
  row[["Statistic"]] <- result$table$Value[[1]]
  row[["p"]] <- result$table$p[[1]]
  row[["ES_overall_label"]] <- "overall"
  row[["ES_overall"]] <- result$table$ES[[1]] %||% ""
  pairwise_effect_labels <- unique(as.character(result$posthoc$`Effect size` %||% ""))
  pairwise_effect_labels <- pairwise_effect_labels[nzchar(pairwise_effect_labels)]
  row[["PairwiseEffectSizeLabel"]] <- if (length(pairwise_effect_labels) > 0) paste(pairwise_effect_labels, collapse = ", ") else ""
  if (!nzchar(row[["PairwiseEffectSizeLabel"]])) {
    methods <- unique(as.character(result$posthoc$Method %||% ""))
    methods <- methods[nzchar(methods)]
    if (any(methods == "Paired t-test")) row[["PairwiseEffectSizeLabel"]] <- "Hedges' g"
    if (any(methods == "Wilcoxon signed-rank test")) row[["PairwiseEffectSizeLabel"]] <- "r"
  }
  pairwise_effects <- paired_rm_pairwise_effects(result, variables, time_labels, contrast_labels)
  for (index in seq_along(pairwise_effects$columns)) {
    row[[paste0(pairwise_effects$columns[[index]], "_label")]] <- pairwise_effects$labels[[index]]
    row[[pairwise_effects$columns[[index]]]] <- pairwise_effects$values[[index]]
  }
  row[["Post-hoc"]] <- paired_rm_posthoc_notation(variables, result$posthoc, means, stats::setNames(time_labels, variables), contrast_labels)
  posthoc_methods <- unique(as.character(result$posthoc$Method %||% ""))
  posthoc_methods <- posthoc_methods[nzchar(posthoc_methods)]
  row[["PosthocMethodLabel"]] <- paste(posthoc_methods, collapse = ", ")
  row[["PosthocAdjustmentLabel"]] <- if (identical(result$options$posthoc_adjustment %||% "bonferroni", "holm")) "Holm Bonferroni" else "Bonferroni correction"
  row[["Method"]] <- result$table$Method[[1]]
  row[["EffectSizeLabel"]] <- result$table$`Effect size`[[1]] %||% ""
  for (column in c("Wilks' lambda", "GG epsilon", "GG p")) {
    if (column %in% names(result$table)) {
      row[[column]] <- result$table[[column]][[1]] %||% ""
    }
  }
  if (is.data.frame(result$assumption) && nrow(result$assumption) > 0) {
    sphericity_row <- result$assumption[result$assumption$Statistics == "Sphericity", , drop = FALSE]
    sphericity_p_row <- result$assumption[result$assumption$Statistics == "Sphericity p", , drop = FALSE]
    if (nrow(sphericity_row) > 0) {
      row[["Sphericity"]] <- sphericity_row$Result[[1]] %||% ""
    }
    if (nrow(sphericity_p_row) > 0) {
      row[["Sphericity p"]] <- sphericity_p_row$Value[[1]] %||% ""
    }
  }
  row
}

paired_rm_binary_display_table <- function(result, values, variable_info, labels, category_table) {
  variables <- as.character(result$variables %||% character(0))
  if (length(variables) == 0) return(result$table)
  time_labels <- paired_rm_option_time_labels(result$options %||% list(), length(variables))
  contrast_labels <- stats::setNames(
    vapply(variables, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    variables
  )
  row <- data.frame(
    `Repeated variables` = result$group_label,
    N = result$table$N[[1]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (index in seq_along(variables)) {
    value <- as.character(values[[variables[[index]]]])
    row[[paste0("Time", index, "_label")]] <- time_labels[[index]]
    row[[paste0("Time", index, "_0")]] <- sum(value == "0", na.rm = TRUE)
    row[[paste0("Time", index, "_1")]] <- sum(value == "1", na.rm = TRUE)
  }
  row[["StatisticLabel"]] <- result$table$Statistic[[1]]
  row[["Statistic"]] <- result$table$Value[[1]]
  row[["p"]] <- result$table$p[[1]]
  row[["Post-hoc"]] <- paired_rm_posthoc_notation(variables, result$posthoc, NULL, stats::setNames(time_labels, variables), contrast_labels)
  posthoc_methods <- unique(as.character(result$posthoc$Method %||% ""))
  posthoc_methods <- posthoc_methods[nzchar(posthoc_methods)]
  row[["PosthocMethodLabel"]] <- paste(posthoc_methods, collapse = ", ")
  row[["PosthocAdjustmentLabel"]] <- if (identical(result$options$posthoc_adjustment %||% "bonferroni", "holm")) "Holm Bonferroni" else "Bonferroni correction"
  row[["Method"]] <- result$table$Method[[1]]
  row
}

prepare_paired_rm_single_result <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  variables <- as.character(variables %||% character(0))
  shiny::validate(shiny::need(length(variables) >= 3, "Select three or more repeated-measures variables."))
  measurements <- paired_measurement_lookup(variable_info)
  levels <- vapply(variables, function(name) named_value(measurements, name, "continuous"), character(1))
  shiny::validate(shiny::need(length(unique(levels)) == 1, "Repeated-measures variables must have the same measurement level."))
  measurement <- levels[[1]]
  shiny::validate(shiny::need(measurement %in% c("continuous", "ordered", "binary"), "Paired test (3+) supports continuous, ordinal, or binary variables."))
  values <- paired_rm_complete_matrix(data, variables, measurement)
  shiny::validate(shiny::need(nrow(values) > 1, "Not enough complete repeated-measures cases."))
  adjustment <- if (identical(options$posthoc_adjustment %||% "bonferroni", "holm")) "holm" else "bonferroni"

  if (identical(measurement, "continuous")) {
    y <- as.matrix(values)
    normality <- paired_rm_normality(y, isTRUE(options$assumption_check))
    sphericity <- paired_rm_sphericity(y)
    anova <- paired_rm_anova(y)
    wilks <- paired_rm_wilks(y)
    method_key <- if (isTRUE(options$assumption_check) && !isTRUE(normality$satisfied)) {
      "friedman"
    } else if (isTRUE(options$assumption_check) && identical(sphericity$satisfied, FALSE)) {
      "rm_anova_wilks"
    } else {
      "rm_anova"
    }
    if (identical(method_key, "friedman")) {
      test <- stats::friedman.test(y)
      main <- data.frame(Method = paired_rm_method_label(method_key), N = nrow(y), Statistic = stat_chisq_label(FALSE), Value = format_decimal3(unname(as.numeric(test$statistic))), df1 = format_decimal3(unname(as.numeric(test$parameter))), df2 = "", p = format_p(test$p.value), stringsAsFactors = FALSE, check.names = FALSE)
      main[["Effect size"]] <- "Kendall's W"
      main[["ES"]] <- paired_effect_value(paired_rm_kendalls_w(unname(as.numeric(test$statistic)), nrow(y), ncol(y)))
      posthoc <- paired_rm_posthoc_scale(as.data.frame(y), "Wilcoxon signed-rank test", adjustment, variable_info, labels, category_table)
    } else if (identical(method_key, "rm_anova_wilks")) {
      gg_df1 <- sphericity$epsilon * anova$df1
      gg_df2 <- sphericity$epsilon * anova$df2
      gg_p <- stats::pf(anova$f, gg_df1, gg_df2, lower.tail = FALSE)
      main <- data.frame(Method = paired_rm_method_label(method_key), N = nrow(y), Statistic = "F", Value = format_decimal3(wilks$f), df1 = format_decimal3(wilks$df1), df2 = format_decimal3(wilks$df2), p = format_p(wilks$p), `Wilks' lambda` = format_decimal3(wilks$lambda), `GG epsilon` = format_decimal3(sphericity$epsilon), `GG p` = format_p(gg_p), stringsAsFactors = FALSE, check.names = FALSE)
      main[["Effect size"]] <- "partial eta squared"
      main[["ES"]] <- paired_effect_value(paired_rm_partial_eta_squared(anova))
      posthoc <- paired_rm_posthoc_scale(as.data.frame(y), "Paired t-test", adjustment, variable_info, labels, category_table)
    } else {
      main <- data.frame(Method = paired_rm_method_label(method_key), N = nrow(y), Statistic = "F", Value = format_decimal3(anova$f), df1 = format_decimal3(anova$df1), df2 = format_decimal3(anova$df2), p = format_p(anova$p), stringsAsFactors = FALSE, check.names = FALSE)
      main[["Effect size"]] <- "partial eta squared"
      main[["ES"]] <- paired_effect_value(paired_rm_partial_eta_squared(anova))
      posthoc <- paired_rm_posthoc_scale(as.data.frame(y), "Paired t-test", adjustment, variable_info, labels, category_table)
    }
    assumption <- data.frame(
      Statistics = c("Normality method", "Normality", "Mauchly W", "Sphericity p", "Sphericity", "GG epsilon"),
      Value = c(normality$method, "", format_decimal3(sphericity$w), format_p(sphericity$p), "", format_decimal3(sphericity$epsilon)),
      Result = c("", if (isTRUE(normality$satisfied)) "Satisfied" else "Not satisfied", "", "", if (isTRUE(sphericity$satisfied)) "Satisfied" else "Not satisfied", ""),
      stringsAsFactors = FALSE
    )
    result <- list(type = "paired_rm", measurement = "Continuous", variables = variables, group_label = paired_rm_group_label(variables, variable_info, labels, category_table), table = main, summary = paired_rm_summary_table(y, variable_info, labels, category_table), posthoc = posthoc, assumption = assumption, options = options)
    result$display_table <- paired_rm_display_table(result)
    return(result)
  }

  if (identical(measurement, "ordered")) {
    y <- as.matrix(values)
    test <- stats::friedman.test(y)
    main <- data.frame(Method = paired_rm_method_label("friedman"), N = nrow(y), Statistic = stat_chisq_label(FALSE), Value = format_decimal3(unname(as.numeric(test$statistic))), df1 = format_decimal3(unname(as.numeric(test$parameter))), df2 = "", p = format_p(test$p.value), stringsAsFactors = FALSE, check.names = FALSE)
    main[["Effect size"]] <- "Kendall's W"
    main[["ES"]] <- paired_effect_value(paired_rm_kendalls_w(unname(as.numeric(test$statistic)), nrow(y), ncol(y)))
    posthoc <- paired_rm_posthoc_scale(as.data.frame(y), "Wilcoxon signed-rank test", adjustment, variable_info, labels, category_table)
    result <- list(type = "paired_rm", measurement = "Ordinal", variables = variables, group_label = paired_rm_group_label(variables, variable_info, labels, category_table), table = main, summary = paired_rm_summary_table(y, variable_info, labels, category_table), posthoc = posthoc, assumption = NULL, options = options)
    result$display_table <- paired_rm_display_table(result)
    return(result)
  }

  y <- values
  test <- paired_rm_cochran_q(y)
  main <- data.frame(Method = paired_rm_method_label("cochran"), N = nrow(y), Statistic = "Q", Value = format_decimal3(test$q), df1 = format_decimal3(test$df), df2 = "", p = format_p(test$p), stringsAsFactors = FALSE, check.names = FALSE)
  posthoc <- paired_rm_posthoc_binary(y, adjustment, variable_info, labels, category_table)
  result <- list(type = "paired_rm", measurement = "Binary", variables = variables, group_label = paired_rm_group_label(variables, variable_info, labels, category_table), table = main, summary = NULL, posthoc = posthoc, assumption = NULL, options = options)
  result$count_table <- paired_rm_binary_display_table(result, y, variable_info, labels, category_table)
  result
}

paired_rm_tag_table <- function(table, group_label) {
  if (!is.data.frame(table) || nrow(table) == 0) return(table)
  cbind(`Repeated variables` = group_label, table, stringsAsFactors = FALSE)
}

paired_rm_bind_rows <- function(tables) {
  tables <- Filter(function(table) is.data.frame(table) && nrow(table) > 0, tables)
  if (length(tables) == 0) return(NULL)
  columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  tables <- lapply(tables, function(table) {
    missing <- setdiff(columns, names(table))
    for (column in missing) table[[column]] <- ""
    table[, columns, drop = FALSE]
  })
  do.call(rbind, tables)
}

prepare_paired_rm_results <- function(data, variables = NULL, variable_groups = NULL, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  groups <- variable_groups %||% list()
  if (length(groups) == 0 && length(variables %||% character(0)) > 0) {
    groups <- list(as.character(variables))
  }
  groups <- lapply(groups, as.character)
  shiny::validate(shiny::need(length(groups) > 0, "Select one or more repeated-measures rows."))
  results <- lapply(groups, function(group) {
    prepare_paired_rm_single_result(data, group, variable_info, labels, category_table, options)
  })
  if (length(results) == 1L) return(results[[1]])

  table <- paired_rm_bind_rows(lapply(results, function(result) paired_rm_tag_table(result$table, result$group_label)))
  display_table <- paired_rm_bind_rows(lapply(results, function(result) result$display_table))
  count_table <- paired_rm_bind_rows(lapply(results, function(result) result$count_table))
  summary <- paired_rm_bind_rows(lapply(results, function(result) paired_rm_tag_table(result$summary, result$group_label)))
  posthoc <- paired_rm_bind_rows(lapply(results, function(result) paired_rm_tag_table(result$posthoc, result$group_label)))
  assumptions <- Filter(Negate(is.null), lapply(results, function(result) paired_rm_tag_table(result$assumption, result$group_label)))
  assumption <- if (length(assumptions) > 0) do.call(rbind, assumptions) else NULL
  list(
    type = "paired_rm",
    measurement = paste(unique(vapply(results, `[[`, character(1), "measurement")), collapse = ", "),
    variables = lapply(results, `[[`, "variables"),
    group_label = paste(vapply(results, `[[`, character(1), "group_label"), collapse = "; "),
    table = table,
    display_table = display_table,
    count_table = count_table,
    summary = summary,
    posthoc = posthoc,
    assumption = assumption,
    options = options
  )
}
