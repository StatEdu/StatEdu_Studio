# Nonparametric paired/repeated-measures test analysis.

nonparametric_paired_format_decimal2 <- function(value) {
  if (length(value) == 0 || is.na(value[[1]])) return("")
  sprintf("%.2f", as.numeric(value[[1]]))
}

nonparametric_paired_summary_values <- function(values, median_iqr = FALSE) {
  values <- as.numeric(values)
  if (isTRUE(median_iqr)) {
    q <- stats::quantile(values, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE, type = 2)
    return(list(center = nonparametric_paired_format_decimal2(q[[2]]), spread = sprintf("%s~%s", nonparametric_paired_format_decimal2(q[[1]]), nonparametric_paired_format_decimal2(q[[3]]))))
  }
  list(
    center = nonparametric_paired_format_decimal2(mean(values, na.rm = TRUE)),
    spread = nonparametric_paired_format_decimal2(stats::sd(values, na.rm = TRUE))
  )
}

nonparametric_paired_summary_table <- function(y, variable_info, labels, category_table, median_iqr = FALSE) {
  summaries <- lapply(as.data.frame(y), nonparametric_paired_summary_values, median_iqr = isTRUE(median_iqr))
  data.frame(
    Time = vapply(colnames(y), paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    N = nrow(y),
    M = vapply(summaries, `[[`, character(1), "center"),
    SD = vapply(summaries, `[[`, character(1), "spread"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

nonparametric_paired_analyze_pair <- function(data, first, second, measurement, variable_info, labels, category_table, options) {
  x_raw <- data[[first]]
  y_raw <- data[[second]]
  pair_label <- sprintf(
    "%s - %s",
    paired_display_name(first, variable_info, labels, category_table),
    paired_display_name(second, variable_info, labels, category_table)
  )

  if (measurement %in% c("continuous", "ordered")) {
    pair <- paired_complete_data(paired_numeric(x_raw), paired_numeric(y_raw))
    diff <- pair$y - pair$x
    guard <- paired_diff_guard(diff, require_variance = FALSE)
    if (nzchar(guard)) {
      level <- if (identical(measurement, "ordered")) "Ordinal" else "Continuous"
      skipped <- paired_skipped_item(pair_label, level, "Wilcoxon signed-rank test", pair$n, guard)
      return(list(result = skipped, scale = NULL, count = NULL, check = NULL, skipped = skipped))
    }
    test <- tryCatch(suppressWarnings(stats::wilcox.test(pair$y, pair$x, paired = TRUE, exact = FALSE)), error = function(e) NULL)
    statistic <- if (is.null(test)) NA_real_ else unname(as.numeric(test$statistic))
    p <- if (is.null(test)) NA_real_ else as.numeric(test$p.value)
    pre_summary <- nonparametric_paired_summary_values(pair$x, median_iqr = isTRUE(options$median_iqr))
    post_summary <- nonparametric_paired_summary_values(pair$y, median_iqr = isTRUE(options$median_iqr))
    return(list(
      result = data.frame(
        Pair = pair_label,
        Level = if (identical(measurement, "ordered")) "Ordinal" else "Continuous",
        Method = "Wilcoxon signed-rank test",
        N = pair$n,
        Statistic = format_decimal3(statistic),
        df = "",
        p = format_p(p),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      scale = data.frame(
        Variable = pair_label,
        Pre_M = pre_summary$center,
        Pre_SD = pre_summary$spread,
        Post_M = post_summary$center,
        Post_SD = post_summary$spread,
        Method = "Wilcoxon signed-rank test",
        StatisticLabel = "W",
        Statistic = format_decimal3(statistic),
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
    level <- if (identical(measurement, "binary")) "Binary" else "Categorical"
    skipped <- paired_skipped_item(pair_label, level, "Paired categorical test", pair$n, reason)
    return(list(result = skipped, scale = NULL, count = NULL, count_method = "", check = NULL, skipped = skipped))
  }
  if (identical(measurement, "binary") && nrow(tab) == 2 && ncol(tab) == 2) {
    b <- as.numeric(tab[1, 2])
    c <- as.numeric(tab[2, 1])
    use_asymptotic <- (b + c) >= 25 && b > 5 && c > 5
    res <- if (use_asymptotic) paired_mcnemar_asymptotic(tab) else paired_mcnemar_exact(tab)
    method <- if (use_asymptotic) "McNemar test" else "Exact McNemar test"
    statistic_label <- if (use_asymptotic) stat_chisq_label(FALSE) else ""
    return(list(
      result = data.frame(Pair = pair_label, Level = "Binary", Method = method, N = pair$n, Statistic = format_decimal3(res$statistic), df = if (is.finite(res$df)) format_decimal3(res$df) else "", p = format_p(res$p), b = b, c = c, stringsAsFactors = FALSE, check.names = FALSE),
      scale = NULL,
      count = paired_count_rows(pair_label, method, tab, statistic_label, format_decimal3(res$statistic), format_p(res$p), "OR", paired_effect_value(paired_odds_ratio(tab)), first, category_table),
      count_method = method,
      check = NULL,
      skipped = NULL
    ))
  }

  res <- paired_bowker_test(tab)
  method <- "Bowker symmetry test"
  list(
    result = data.frame(Pair = pair_label, Level = "Categorical", Method = method, N = pair$n, Statistic = format_decimal3(res$statistic), df = if (is.finite(res$df)) format_decimal3(res$df) else "", p = format_p(res$p), stringsAsFactors = FALSE, check.names = FALSE),
    scale = NULL,
    count = paired_count_rows(pair_label, method, tab, stat_chisq_label(FALSE), format_decimal3(res$statistic), format_p(res$p), "", "", first, category_table),
    count_method = method,
    check = NULL,
    skipped = NULL
  )
}

prepare_nonparametric_paired_results <- function(data, first, second, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
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
      skipped <- paired_skipped_item(pair_label, "Unknown", "Nonparametric paired test", 0L, reason)
      return(list(result = skipped, scale = NULL, count = NULL, count_method = "", check = NULL, skipped = skipped))
    }
    m1 <- named_value(measurements, x, "continuous")
    m2 <- named_value(measurements, y, "continuous")
    if (!identical(m1, m2)) {
      reason <- sprintf("Skipped because paired variables have different measurement levels (%s vs %s).", m1, m2)
      skipped <- paired_skipped_item(pair_label, sprintf("%s/%s", paired_measurement_label(m1), paired_measurement_label(m2)), "Nonparametric paired test", 0L, reason)
      return(list(result = skipped, scale = NULL, count = NULL, count_method = "", check = NULL, skipped = skipped))
    }
    nonparametric_paired_analyze_pair(data, x, y, m1, variable_info, labels, category_table, options)
  })
  result_tables <- lapply(items, `[[`, "result")
  all_columns <- unique(unlist(lapply(result_tables, names), use.names = FALSE))
  result_tables <- lapply(result_tables, function(table) {
    missing <- setdiff(all_columns, names(table))
    for (column in missing) table[[column]] <- ""
    table[, all_columns, drop = FALSE]
  })
  scale_tables <- Filter(Negate(is.null), lapply(items, `[[`, "scale"))
  count_tables <- Filter(Negate(is.null), lapply(items, `[[`, "count"))
  count_table <- if (length(count_tables) > 0) {
    columns <- unique(unlist(lapply(count_tables, names), use.names = FALSE))
    count_tables <- lapply(count_tables, function(table) {
      missing <- setdiff(columns, names(table))
      for (column in missing) table[[column]] <- ""
      table[, columns, drop = FALSE]
    })
    out <- do.call(rbind, count_tables)
    attr(out, "post_levels") <- unique(unlist(lapply(count_tables, function(table) attr(table, "post_levels", exact = TRUE)), use.names = FALSE))
    out
  } else {
    NULL
  }
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
    type = "nonparametric_paired",
    table = do.call(rbind, result_tables),
    scale_table = if (length(scale_tables) > 0) do.call(rbind, scale_tables) else NULL,
    count_table = count_table,
    count_methods = unique(as.character(unlist(lapply(items, function(item) item$count_method %||% character(0)), use.names = FALSE))),
    checks = NULL,
    skipped = skipped_table,
    warnings = warning_table,
    options = options
  )
}

prepare_nonparametric_paired_rm_single_result <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  variables <- as.character(variables %||% character(0))
  if (length(variables) < 3) {
    stop("Select three or more repeated-measures variables.", call. = FALSE)
  }
  if (!all(variables %in% names(data))) {
    missing <- setdiff(variables, names(data))
    stop(sprintf("Variable(s) were not found in the active data: %s.", paste(missing, collapse = ", ")), call. = FALSE)
  }
  measurements <- paired_measurement_lookup(variable_info)
  levels <- vapply(variables, function(name) named_value(measurements, name, "continuous"), character(1))
  if (length(unique(levels)) != 1) {
    stop(sprintf("Repeated-measures variables have different measurement levels: %s.", paste(sprintf("%s=%s", variables, levels), collapse = ", ")), call. = FALSE)
  }
  measurement <- levels[[1]]
  if (!measurement %in% c("continuous", "ordered", "binary")) {
    stop("Nonparametric paired test supports continuous, ordinal, or binary variables.", call. = FALSE)
  }
  values <- paired_rm_complete_matrix(data, variables, measurement)
  if (nrow(values) < 2) {
    stop("At least two complete repeated-measures cases are required.", call. = FALSE)
  }
  if (!paired_rm_has_within_subject_change(values)) {
    stop("All repeated measurements are identical within subjects; no repeated-measures test was performed.", call. = FALSE)
  }
  adjustment <- if (identical(options$posthoc_adjustment %||% "bonferroni", "holm")) "holm" else "bonferroni"

  if (measurement %in% c("continuous", "ordered")) {
    y <- as.matrix(values)
    test <- stats::friedman.test(y)
    main <- data.frame(Method = "Friedman test", N = nrow(y), Statistic = stat_chisq_label(FALSE), Value = format_decimal3(unname(as.numeric(test$statistic))), df1 = format_decimal3(unname(as.numeric(test$parameter))), df2 = "", p = format_p(test$p.value), stringsAsFactors = FALSE, check.names = FALSE)
    main[["Effect size"]] <- "Kendall's W"
    main[["ES"]] <- paired_effect_value(paired_rm_kendalls_w(unname(as.numeric(test$statistic)), nrow(y), ncol(y)))
    posthoc <- paired_rm_posthoc_scale(as.data.frame(y), "Wilcoxon signed-rank test", adjustment, variable_info, labels, category_table)
    result <- list(type = "nonparametric_paired_rm", measurement = if (identical(measurement, "ordered")) "Ordinal" else "Continuous", variables = variables, group_label = paired_rm_group_label(variables, variable_info, labels, category_table), table = main, summary = nonparametric_paired_summary_table(y, variable_info, labels, category_table, median_iqr = isTRUE(options$median_iqr)), posthoc = posthoc, assumption = NULL, options = options)
    result$display_table <- paired_rm_display_table(result)
    return(result)
  }

  y <- values
  test <- paired_rm_cochran_q(y)
  main <- data.frame(Method = "Cochran's Q test", N = nrow(y), Statistic = "Q", Value = format_decimal3(test$q), df1 = format_decimal3(test$df), df2 = "", p = format_p(test$p), stringsAsFactors = FALSE, check.names = FALSE)
  posthoc <- paired_rm_posthoc_binary(y, adjustment, variable_info, labels, category_table)
  result <- list(type = "nonparametric_paired_rm", measurement = "Binary", variables = variables, group_label = paired_rm_group_label(variables, variable_info, labels, category_table), table = main, summary = NULL, posthoc = posthoc, assumption = NULL, options = options)
  result$count_table <- paired_rm_binary_display_table(result, y, variable_info, labels, category_table)
  result
}

prepare_nonparametric_paired_rm_results <- function(data, variable_groups, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  groups <- lapply(variable_groups %||% list(), as.character)
  groups <- groups[lengths(groups) >= 3L]
  shiny::validate(shiny::need(length(groups) > 0, "Select one or more repeated-measures rows."))
  results <- lapply(groups, function(group) {
    tryCatch(
      prepare_nonparametric_paired_rm_single_result(data, group, variable_info, labels, category_table, options),
      error = function(e) {
        skipped <- paired_rm_skipped_result(group, variable_info, labels, category_table, options, conditionMessage(e))
        skipped$type <- "nonparametric_paired_rm"
        skipped
      }
    )
  })
  if (length(results) == 1L) return(results[[1]])
  skipped <- paired_rm_bind_rows(lapply(results, function(result) result$skipped))
  list(
    type = "nonparametric_paired_rm",
    measurement = paste(unique(vapply(results, `[[`, character(1), "measurement")), collapse = ", "),
    variables = lapply(results, `[[`, "variables"),
    group_label = paste(vapply(results, `[[`, character(1), "group_label"), collapse = "; "),
    table = paired_rm_bind_rows(lapply(results, function(result) paired_rm_tag_table(result$table, result$group_label))),
    display_table = paired_rm_bind_rows(lapply(results, function(result) result$display_table)),
    count_table = paired_rm_bind_rows(lapply(results, function(result) result$count_table)),
    summary = paired_rm_bind_rows(lapply(results, function(result) paired_rm_tag_table(result$summary, result$group_label))),
    posthoc = paired_rm_bind_rows(lapply(results, function(result) paired_rm_tag_table(result$posthoc, result$group_label))),
    assumption = NULL,
    skipped = skipped,
    options = options
  )
}

prepare_nonparametric_paired_unified_results <- function(data, variable_groups, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  groups <- lapply(variable_groups %||% list(), as.character)
  groups <- groups[lengths(groups) >= 2L]
  shiny::validate(shiny::need(length(groups) > 0, "Select one or more repeated-measures rows."))
  two_groups <- groups[lengths(groups) == 2L]
  rm_groups <- groups[lengths(groups) >= 3L]
  paired_result <- NULL
  paired_rm_result <- NULL
  if (length(two_groups) > 0) {
    paired_result <- prepare_nonparametric_paired_results(data, vapply(two_groups, `[[`, character(1), 1L), vapply(two_groups, `[[`, character(1), 2L), variable_info, labels, category_table, options)
  }
  if (length(rm_groups) > 0) {
    paired_rm_result <- prepare_nonparametric_paired_rm_results(data, rm_groups, variable_info, labels, category_table, options)
  }
  if (!is.null(paired_result) && !is.null(paired_rm_result)) {
    return(list(type = "nonparametric_paired_combined", paired = paired_result, paired_rm = paired_rm_result, options = options))
  }
  paired_result %||% paired_rm_result
}
