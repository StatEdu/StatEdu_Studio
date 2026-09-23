# Within-subject treatment x time repeated-measures ANOVA for WIDE and LONG data.

one_group_rm_abort <- function(key, ...) {
  args <- list(...)
  message <- do.call(sprintf, c(list(statedu_t(paste0("analysis.one_group.error.", key), "en")), args))
  stop(structure(list(message = message, call = NULL, key = key, args = args),
    class = c("one_group_rm_input_error", "error", "condition")))
}

one_group_rm_error_ui_text <- function(error, language) {
  if (!inherits(error, "one_group_rm_input_error")) return(conditionMessage(error))
  do.call(sprintf, c(list(statedu_t(paste0("analysis.one_group.error.", error$key), language)), error$args))
}

one_group_rm_continuous_candidates <- function(variable_names, variable_table = NULL) {
  analysis_allowed_variables(variable_names, variable_table, c("continuous", "ordered"))
}

one_group_rm_observed_levels <- function(values, sort_numeric = FALSE) {
  keep <- !is.na(values)
  if (is.character(values)) keep <- keep & nzchar(trimws(values))
  values <- values[keep]
  if (length(values) == 0L) return(character(0))
  if (is.factor(values)) {
    observed <- unique(as.character(values))
    return(intersect(levels(values), observed))
  }
  if (isTRUE(sort_numeric) && (is.numeric(values) || inherits(values, c("Date", "POSIXct", "POSIXlt")))) {
    return(as.character(sort(unique(values))))
  }
  unique(as.character(values))
}

one_group_rm_time_levels <- function(values) {
  one_group_rm_observed_levels(values, sort_numeric = TRUE)
}

one_group_rm_prepare_covariate_frame <- function(frame, covariates, variable_info = NULL) {
  covariates <- unique(as.character(covariates %||% character(0)))
  covariates <- covariates[nzchar(covariates)]
  if (length(covariates) == 0L) return(data.frame(row.names = seq_len(nrow(frame))))
  missing <- setdiff(covariates, names(frame))
  if (length(missing) > 0L) one_group_rm_abort("covariates_missing", paste(missing, collapse = ", "))
  measurements <- mixed_rm_measurement_lookup(variable_info)
  out <- as.data.frame(frame[, covariates, drop = FALSE], stringsAsFactors = FALSE)
  for (name in covariates) {
    measurement <- named_value(measurements, name, "continuous")
    out[[name]] <- if (measurement %in% c("binary", "category")) {
      mixed_rm_group_factor(out[[name]])
    } else {
      paired_numeric(out[[name]])
    }
  }
  out
}

one_group_rm_long_covariate_frame <- function(data, id_raw, subject_levels, covariates, variable_info = NULL) {
  covariates <- unique(as.character(covariates %||% character(0)))
  covariates <- covariates[nzchar(covariates)]
  if (length(covariates) == 0L) return(data.frame(row.names = seq_along(subject_levels)))
  collapsed <- data.frame(row.names = seq_along(subject_levels))
  id_text <- as.character(id_raw)
  for (name in covariates) {
    if (!name %in% names(data)) one_group_rm_abort("covariate_missing", name)
    collapsed[[name]] <- vapply(subject_levels, function(subject) {
      values <- data[[name]][id_text == subject & !is.na(id_raw)]
      if (is.character(values)) values <- values[nzchar(trimws(values))]
      values <- values[!is.na(values)]
      distinct <- unique(as.character(values))
      if (length(distinct) > 1L) {
        one_group_rm_abort("covariate_varies", name, subject)
      }
      if (length(distinct) == 0L) NA_character_ else distinct[[1L]]
    }, character(1))
  }
  one_group_rm_prepare_covariate_frame(collapsed, covariates, variable_info)
}

one_group_rm_long_input <- function(data, id_variable, group_variable, time_variable, outcome_variable, covariates = character(0), variable_info = NULL) {
  selected <- c(id_variable, group_variable, time_variable, outcome_variable, covariates)
  missing <- setdiff(selected, names(data))
  if (length(missing) > 0L) {
    one_group_rm_abort("long_missing", paste(missing, collapse = ", "))
  }
  id_raw <- data[[id_variable]]
  group_raw <- data[[group_variable]]
  time_raw <- data[[time_variable]]
  valid_key <- !is.na(id_raw) & !is.na(group_raw) & !is.na(time_raw)
  if (is.character(id_raw)) valid_key <- valid_key & nzchar(trimws(id_raw))
  if (is.character(group_raw)) valid_key <- valid_key & nzchar(trimws(group_raw))
  if (is.character(time_raw)) valid_key <- valid_key & nzchar(trimws(time_raw))
  if (!any(valid_key)) one_group_rm_abort("long_keys")

  id_text <- as.character(id_raw[valid_key])
  group_text <- as.character(group_raw[valid_key])
  time_text <- as.character(time_raw[valid_key])
  key_frame <- data.frame(id = id_text, group = group_text, time = time_text, stringsAsFactors = FALSE)
  duplicate_key <- duplicated(key_frame) | duplicated(key_frame, fromLast = TRUE)
  if (any(duplicate_key)) {
    examples <- unique(sprintf("%s / %s / %s", key_frame$id[duplicate_key], key_frame$group[duplicate_key], key_frame$time[duplicate_key]))
    one_group_rm_abort("duplicate_keys", paste(utils::head(examples, 5L), collapse = ", "))
  }

  subject_levels <- unique(id_text)
  treatment_levels <- one_group_rm_observed_levels(group_raw[valid_key])
  time_levels <- one_group_rm_time_levels(time_raw[valid_key])
  if (length(treatment_levels) != 2L) {
    one_group_rm_abort("two_groups")
  }
  if (length(time_levels) < 2L) one_group_rm_abort("two_times")

  outcome <- paired_numeric(data[[outcome_variable]][valid_key])
  values <- array(
    NA_real_,
    dim = c(length(subject_levels), length(treatment_levels), length(time_levels)),
    dimnames = list(subject_levels, treatment_levels, time_levels)
  )
  row_index <- match(id_text, subject_levels)
  group_index <- match(group_text, treatment_levels)
  time_index <- match(time_text, time_levels)
  assignable <- !is.na(row_index) & !is.na(group_index) & !is.na(time_index)
  values[cbind(row_index[assignable], group_index[assignable], time_index[assignable])] <- outcome[assignable]
  covariate_data <- one_group_rm_long_covariate_frame(data, id_raw, subject_levels, covariates, variable_info)

  list(
    values = values,
    treatment_labels = treatment_levels,
    time_labels = time_levels,
    total_subjects = length(subject_levels),
    source_rows = nrow(data),
    subject_ids = subject_levels,
    id_variable = id_variable,
    group_variable = group_variable,
    time_variable = time_variable,
    outcome_variable = outcome_variable,
    repeated_variables = outcome_variable,
    covariates = unique(as.character(covariates %||% character(0))),
    covariate_data = covariate_data
  )
}

one_group_rm_long_to_wide <- function(data, id_variable, group_variable, time_variable = NULL, outcome_variable = NULL) {
  if (is.null(outcome_variable)) one_group_rm_abort("select_group")
  normalized <- one_group_rm_long_input(data, id_variable, group_variable, time_variable, outcome_variable)
  flattened <- do.call(cbind, lapply(seq_along(normalized$treatment_labels), function(group_index) {
    normalized$values[, group_index, , drop = FALSE][, 1L, ]
  }))
  list(
    data = as.data.frame(flattened, check.names = FALSE),
    variables = paste(rep(normalized$treatment_labels, each = length(normalized$time_labels)), rep(normalized$time_labels, times = 2L), sep = " / "),
    treatment_labels = normalized$treatment_labels,
    time_labels = normalized$time_labels,
    total_subjects = normalized$total_subjects,
    source_rows = normalized$source_rows,
    id_variable = id_variable,
    group_variable = group_variable,
    time_variable = time_variable,
    outcome_variable = outcome_variable,
    values = normalized$values
  )
}

one_group_rm_wide_input <- function(
  data,
  experimental_variables,
  control_variables,
  covariates = character(0),
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  treatment_labels = c("Experimental", "Control"),
  time_labels = character(0)
) {
  experimental_variables <- unique(as.character(experimental_variables %||% character(0)))
  control_variables <- unique(as.character(control_variables %||% character(0)))
  experimental_variables <- experimental_variables[nzchar(experimental_variables)]
  control_variables <- control_variables[nzchar(control_variables)]
  if (length(experimental_variables) < 2L || length(control_variables) < 2L) {
    one_group_rm_abort("wide_times")
  }
  if (length(experimental_variables) != length(control_variables)) {
    one_group_rm_abort("wide_equal")
  }
  if (length(intersect(experimental_variables, control_variables)) > 0L) {
    one_group_rm_abort("wide_overlap")
  }
  covariates <- unique(as.character(covariates %||% character(0)))
  covariates <- covariates[nzchar(covariates)]
  if (length(intersect(selected <- c(experimental_variables, control_variables), covariates)) > 0L) {
    one_group_rm_abort("wide_covariate")
  }
  selected <- c(selected, covariates)
  missing <- setdiff(selected, names(data))
  if (length(missing) > 0L) one_group_rm_abort("wide_missing", paste(missing, collapse = ", "))

  experimental <- as.data.frame(data[, experimental_variables, drop = FALSE], check.names = FALSE)
  control <- as.data.frame(data[, control_variables, drop = FALSE], check.names = FALSE)
  experimental[] <- lapply(experimental, paired_numeric)
  control[] <- lapply(control, paired_numeric)
  if (length(time_labels) != length(experimental_variables) || any(!nzchar(trimws(as.character(time_labels))))) {
    time_labels <- vapply(
      experimental_variables,
      paired_display_name,
      character(1),
      variable_info = variable_info,
      labels = labels,
      category_table = category_table
    )
  }
  time_labels <- make.unique(as.character(time_labels), sep = " ")
  treatment_labels <- as.character(treatment_labels %||% c("Experimental", "Control"))
  if (length(treatment_labels) != 2L || any(!nzchar(trimws(treatment_labels)))) treatment_labels <- c("Experimental", "Control")
  treatment_labels <- make.unique(treatment_labels, sep = " ")

  values <- array(
    NA_real_,
    dim = c(nrow(data), 2L, length(time_labels)),
    dimnames = list(as.character(seq_len(nrow(data))), treatment_labels, time_labels)
  )
  values[, 1L, ] <- as.matrix(experimental)
  values[, 2L, ] <- as.matrix(control)
  covariate_data <- one_group_rm_prepare_covariate_frame(data, covariates, variable_info)
  list(
    values = values,
    treatment_labels = treatment_labels,
    time_labels = time_labels,
    total_subjects = nrow(data),
    source_rows = nrow(data),
    subject_ids = as.character(seq_len(nrow(data))),
    experimental_variables = experimental_variables,
    control_variables = control_variables,
    repeated_variables = c(experimental_variables, control_variables),
    covariates = covariates,
    covariate_data = covariate_data
  )
}

one_group_rm_effect_result <- function(matrix) {
  result <- paired_rm_anova(matrix)
  result$eta <- paired_rm_partial_eta_squared(result)
  result
}

one_group_rm_anova_row <- function(effect, result, n, k = 2L, sphericity = NULL) {
  correction <- mixed_rm_correction_cells(
    effect_key = effect,
    effect_label = effect,
    f_value = result$f,
    df1 = result$df1,
    df2 = result$df2,
    raw_p = result$p,
    n = n,
    k = k,
    sphericity = sphericity
  )
  data.frame(
    Effect = effect,
    `Mauchly W` = correction$w,
    p_sphericity = correction$sphericity_p,
    `epsilon(GG)` = correction$gg,
    `epsilon(HF)` = correction$hf,
    Correction = correction$correction,
    df1 = correction$df1,
    df2 = correction$df2,
    F = format_decimal3(result$f),
    p = correction$p,
    `partial eta2` = format_decimal3(result$eta),
    `post-hoc` = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

one_group_rm_anova_table <- function(values, treatment_label = "Treatment", time_label = "Time") {
  treatment_means <- cbind(rowMeans(values[, 1L, ]), rowMeans(values[, 2L, ]))
  time_means <- (values[, 1L, ] + values[, 2L, ]) / 2
  treatment_difference <- values[, 1L, ] - values[, 2L, ]
  treatment_result <- one_group_rm_effect_result(treatment_means)
  time_result <- one_group_rm_effect_result(time_means)
  interaction_result <- one_group_rm_effect_result(treatment_difference)
  time_sphericity <- suppressWarnings(paired_rm_sphericity(time_means))
  interaction_sphericity <- suppressWarnings(paired_rm_sphericity(treatment_difference))
  interaction_label <- paste(treatment_label, time_label, sep = " x ")
  out <- analysis_bind_rows(list(
    one_group_rm_anova_row(treatment_label, treatment_result, dim(values)[1], 2L, NULL),
    one_group_rm_anova_row(time_label, time_result, dim(values)[1], ncol(time_means), time_sphericity),
    one_group_rm_anova_row(interaction_label, interaction_result, dim(values)[1], ncol(treatment_difference), interaction_sphericity)
  ))
  attr(out, "time_sphericity") <- time_sphericity
  attr(out, "interaction_sphericity") <- interaction_sphericity
  out <- mixed_rm_finalize_anova_table(out)
  attr(out, "one_group_effect_keys") <- c("Treatment", "Time", "Treatment:Time")
  out
}

one_group_rm_covariate_anova_table <- function(values, covariate_data, covariate_labels = character(0)) {
  if (!requireNamespace("car", quietly = TRUE)) {
    one_group_rm_abort("car_required")
  }
  treatment_count <- dim(values)[2L]
  time_count <- dim(values)[3L]
  y <- do.call(cbind, lapply(seq_len(treatment_count), function(index) values[, index, , drop = FALSE][, 1L, ]))
  safe_covariates <- mixed_rm_reduced_covariates(covariate_data)
  if (ncol(safe_covariates) == 0L) return(one_group_rm_anova_table(values, "Treatment", "Time"))
  names(safe_covariates) <- paste0(".cov", seq_len(ncol(safe_covariates)))
  covariate_keys <- names(safe_covariates)
  if (length(covariate_labels) != length(covariate_keys)) covariate_labels <- covariate_keys
  covariate_labels <- stats::setNames(as.character(covariate_labels), covariate_keys)
  fit <- stats::lm(stats::as.formula(paste("y ~", paste(covariate_keys, collapse = " + "))), data = safe_covariates)
  idata <- data.frame(
    Treatment = factor(rep(seq_len(treatment_count), each = time_count)),
    Time = factor(rep(seq_len(time_count), times = treatment_count))
  )
  model <- car::Anova(fit, idata = idata, idesign = ~Treatment * Time, type = "II")
  summary_model <- suppressWarnings(summary(model, multivariate = FALSE))
  uni <- as.data.frame.matrix(summary_model$univariate.tests, stringsAsFactors = FALSE)
  effect_keys <- rownames(summary_model$univariate.tests)
  keep <- effect_keys != "(Intercept)"
  uni <- uni[keep, , drop = FALSE]
  effect_keys <- effect_keys[keep]
  p_adjust <- if (!is.null(summary_model$pval.adjustments)) as.data.frame.matrix(summary_model$pval.adjustments, stringsAsFactors = FALSE) else NULL
  effect_labels <- vapply(effect_keys, mixed_rm_effect_display, character(1), group_labels = c(Treatment = "Treatment"), covariate_labels = covariate_labels)
  corrections <- lapply(seq_along(effect_keys), function(index) {
    mixed_rm_correction_cells(
      effect_key = effect_keys[[index]],
      effect_label = effect_labels[[index]],
      f_value = uni$`F value`[[index]],
      df1 = uni$`num Df`[[index]],
      df2 = uni$`den Df`[[index]],
      raw_p = uni$`Pr(>F)`[[index]],
      n = nrow(y),
      k = time_count,
      sphericity = if (!is.null(summary_model$sphericity.tests)) as.data.frame.matrix(summary_model$sphericity.tests, stringsAsFactors = FALSE) else NULL,
      p_adjust = p_adjust
    )
  })
  out <- data.frame(
    Effect = effect_labels,
    `Mauchly W` = vapply(corrections, `[[`, character(1), "w"),
    p_sphericity = vapply(corrections, `[[`, character(1), "sphericity_p"),
    `epsilon(GG)` = vapply(corrections, `[[`, character(1), "gg"),
    `epsilon(HF)` = vapply(corrections, `[[`, character(1), "hf"),
    Correction = vapply(corrections, `[[`, character(1), "correction"),
    df1 = vapply(corrections, `[[`, character(1), "df1"),
    df2 = vapply(corrections, `[[`, character(1), "df2"),
    F = vapply(uni$`F value`, format_decimal3, character(1)),
    p = vapply(corrections, `[[`, character(1), "p"),
    `partial eta2` = vapply(mapply(mixed_rm_safe_ratio, uni$`Sum Sq`, uni$`Sum Sq` + uni$`Error SS`), format_decimal3, character(1)),
    `post-hoc` = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(out, "time_sphericity") <- suppressWarnings(paired_rm_sphericity((values[, 1L, ] + values[, 2L, ]) / 2))
  attr(out, "interaction_sphericity") <- suppressWarnings(paired_rm_sphericity(values[, 1L, ] - values[, 2L, ]))
  out <- mixed_rm_finalize_anova_table(out)
  attr(out, "one_group_effect_keys") <- effect_keys
  attr(out, "one_group_covariate_labels") <- covariate_labels
  out
}

one_group_rm_paired_test <- function(first, second, covariate_data = NULL) {
  if (!is.null(covariate_data) && ncol(covariate_data) > 0L) {
    return(mixed_rm_adjusted_pair_test(first - second, covariate_data))
  }
  test <- tryCatch(stats::t.test(first, second, paired = TRUE), error = function(e) NULL)
  list(
    statistic = if (!is.null(test)) unname(test$statistic) else NA_real_,
    df = if (!is.null(test)) unname(test$parameter) else NA_real_,
    p = if (!is.null(test)) test$p.value else NA_real_
  )
}

one_group_rm_posthoc_rows <- function(values, treatment_labels, time_labels, adjustment = statedu_multiple_correction_default(), covariate_data = NULL) {
  if (dim(values)[3] < 2L) return(data.frame())
  adjustment <- if (identical(adjustment, "bonferroni")) "bonferroni" else "holm"
  time_markers <- mixed_rm_time_markers(length(time_labels))
  time_pairs <- utils::combn(seq_along(time_labels), 2L, simplify = FALSE)
  rows <- list()
  method_label <- if (!is.null(covariate_data) && ncol(covariate_data) > 0L) "Covariate-adjusted paired contrast" else "Paired t-test"

  append_family <- function(family, stratum, contrasts, tests) {
    raw_p <- vapply(tests, `[[`, numeric(1), "p")
    adjusted_p <- stats::p.adjust(raw_p, method = adjustment)
    lapply(seq_along(tests), function(index) {
      data.frame(
        Family = family,
        Stratum = stratum[[index]],
        Contrast = contrasts[[index]],
        Method = method_label,
        Statistic = format_decimal3(tests[[index]]$statistic),
        df = format_decimal3(tests[[index]]$df),
        p = format_p(tests[[index]]$p),
        `p adjusted` = format_p(adjusted_p[[index]]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
  }

  overall_time <- (values[, 1L, ] + values[, 2L, ]) / 2
  overall_tests <- lapply(time_pairs, function(pair) one_group_rm_paired_test(overall_time[, pair[[1]]], overall_time[, pair[[2]]], covariate_data))
  rows <- c(rows, append_family(
    "Time comparison overall",
    rep("Overall", length(time_pairs)),
    vapply(time_pairs, function(pair) mixed_rm_pair_label(time_markers[[pair[[1]]]], time_markers[[pair[[2]]]]), character(1)),
    overall_tests
  ))

  treatment_tests <- lapply(seq_along(time_labels), function(index) one_group_rm_paired_test(values[, 1L, index], values[, 2L, index], covariate_data))
  rows <- c(rows, append_family(
    "Treatment comparison at each time",
    time_markers,
    rep(mixed_rm_pair_label(treatment_labels[[1]], treatment_labels[[2]]), length(time_labels)),
    treatment_tests
  ))

  for (group_index in seq_along(treatment_labels)) {
    treatment_matrix <- values[, group_index, ]
    tests <- lapply(time_pairs, function(pair) one_group_rm_paired_test(treatment_matrix[, pair[[1]]], treatment_matrix[, pair[[2]]], covariate_data))
    rows <- c(rows, append_family(
      "Time comparison within treatment",
      rep(treatment_labels[[group_index]], length(time_pairs)),
      vapply(time_pairs, function(pair) mixed_rm_pair_label(time_markers[[pair[[1]]]], time_markers[[pair[[2]]]]), character(1)),
      tests
    ))
  }
  out <- analysis_bind_rows(rows)
  attr(out, "time_markers") <- stats::setNames(time_markers, time_labels)
  mixed_rm_style_posthoc_table(out)
}

one_group_rm_summary_table <- function(values, treatment_labels, time_labels, posthoc, include_posthoc = TRUE, covariate_data = NULL) {
  time_markers <- mixed_rm_time_markers(length(time_labels))
  rows <- list()
  for (group_index in seq_along(treatment_labels)) {
    treatment_matrix <- values[, group_index, ]
    time_test <- if (!is.null(covariate_data) && ncol(covariate_data) > 0L) mixed_rm_adjusted_within_group_time_test(treatment_matrix, covariate_data) else one_group_rm_effect_result(treatment_matrix)
    row <- data.frame(Group = treatment_labels[[group_index]], N = nrow(treatment_matrix), stringsAsFactors = FALSE, check.names = FALSE)
    for (time_index in seq_along(time_labels)) {
      cell <- treatment_matrix[, time_index]
      row[[time_labels[[time_index]]]] <- paste0(format_decimal3(mean(cell)), " \u00b1 ", format_decimal3(stats::sd(cell)))
    }
    row$F <- format_decimal3(time_test$f)
    row$p <- format_p(time_test$p)
    if (isTRUE(include_posthoc) && length(time_labels) >= 3L) {
      family_rows <- posthoc[posthoc$Family == "Time comparison within treatment" & posthoc$Stratum == treatment_labels[[group_index]], , drop = FALSE]
      means <- stats::setNames(colMeans(treatment_matrix), time_markers)
      row$`post-hoc` <- if (mixed_rm_p_is_significant(time_test$p)) mixed_rm_significant_order_notation(time_markers, means, family_rows) else "n.s."
    }
    rows[[length(rows) + 1L]] <- row
  }

  comparison <- data.frame(Group = "Between treatments", N = "F(p)", stringsAsFactors = FALSE, check.names = FALSE)
  treatment_family <- posthoc[posthoc$Family == "Treatment comparison at each time", , drop = FALSE]
  for (time_index in seq_along(time_labels)) {
    matched <- treatment_family[treatment_family$Stratum == time_markers[[time_index]], , drop = FALSE]
    test <- one_group_rm_paired_test(values[, 1L, time_index], values[, 2L, time_index], covariate_data)
    p_text <- if (nrow(matched) > 0L) matched$p[[1]] else format_p(test$p)
    comparison[[time_labels[[time_index]]]] <- mixed_rm_between_group_stat_label(test$statistic ^ 2, mixed_rm_parse_p(p_text))
  }
  comparison$F <- ""
  comparison$p <- ""
  if (isTRUE(include_posthoc) && length(time_labels) >= 3L) comparison$`post-hoc` <- ""
  rows[[length(rows) + 1L]] <- comparison
  out <- analysis_bind_rows(rows)
  attr(out, "time_markers") <- stats::setNames(time_markers, time_labels)
  attr(out, "column_header_markers") <- data.frame(column = time_labels, marker = time_markers, stringsAsFactors = FALSE)
  out
}

one_group_rm_normality_table <- function(values, treatment_labels, time_labels) {
  rows <- lapply(seq_along(time_labels), function(time_index) {
    row <- data.frame(Time = time_labels[[time_index]], stringsAsFactors = FALSE, check.names = FALSE)
    for (group_index in seq_along(treatment_labels)) {
      cell <- values[, group_index, time_index]
      p <- if (length(cell) >= 3L && length(cell) <= 5000L && stats::sd(cell) > 0) tryCatch(stats::shapiro.test(cell)$p.value, error = function(e) NA_real_) else NA_real_
      row[[treatment_labels[[group_index]]]] <- if (is.finite(p)) format_p(p) else ""
    }
    row
  })
  out <- analysis_bind_rows(rows)
  attr(out, "normality_method") <- "Shapiro-Wilk"
  out
}

one_group_rm_assumption_table <- function(values, anova, total_n, excluded_n, treatment_labels, time_labels) {
  rows <- list(
    data.frame(Item = "Total cases", Result = as.character(total_n), Detail = "", stringsAsFactors = FALSE),
    data.frame(Item = "Excluded cases", Result = as.character(excluded_n), Detail = "Subjects missing any treatment-by-time cell are excluded listwise.", stringsAsFactors = FALSE),
    data.frame(Item = "Complete cases", Result = as.character(dim(values)[1]), Detail = "", stringsAsFactors = FALSE),
    data.frame(Item = "Treatment levels", Result = "2", Detail = paste(treatment_labels, collapse = ", "), stringsAsFactors = FALSE),
    data.frame(Item = "Time points", Result = as.character(length(time_labels)), Detail = paste(time_labels, collapse = ", "), stringsAsFactors = FALSE)
  )
  if (length(time_labels) >= 3L) {
    time_sphericity <- attr(anova, "time_sphericity", exact = TRUE) %||% list()
    interaction_sphericity <- attr(anova, "interaction_sphericity", exact = TRUE) %||% list()
    rows <- c(rows, list(
      data.frame(Item = "Sphericity: Time", Result = if (isTRUE(time_sphericity$satisfied)) "Satisfied" else "Not satisfied", Detail = paste0("W=", format_decimal3(time_sphericity$w), "; p=", format_p(time_sphericity$p), "; GG epsilon=", format_decimal3(time_sphericity$epsilon)), stringsAsFactors = FALSE),
      data.frame(Item = "Sphericity: Treatment x Time", Result = if (isTRUE(interaction_sphericity$satisfied)) "Satisfied" else "Not satisfied", Detail = paste0("W=", format_decimal3(interaction_sphericity$w), "; p=", format_p(interaction_sphericity$p), "; GG epsilon=", format_decimal3(interaction_sphericity$epsilon)), stringsAsFactors = FALSE)
    ))
  } else {
    rows <- c(rows, list(data.frame(Item = "Sphericity", Result = "Not required", Detail = "Only two repeated time points were selected.", stringsAsFactors = FALSE)))
  }
  analysis_bind_rows(rows)
}

one_group_rm_model_overview <- function(input_format, normalized, values, excluded_n, covariate_labels = character(0)) {
  fields <- if (identical(input_format, "long")) {
    paste(paste0("Subject ID: ", normalized$id_variable), paste0("Treatment group: ", normalized$group_variable), paste0("Time: ", normalized$time_variable), paste0("Outcome: ", normalized$outcome_variable), sep = "; ")
  } else {
    paste(paste0(normalized$treatment_labels[[1]], ": ", paste(normalized$experimental_variables, collapse = ", ")), paste0(normalized$treatment_labels[[2]], ": ", paste(normalized$control_variables, collapse = ", ")), sep = "; ")
  }
  overview <- data.frame(
    Item = c("Analysis", "Analysis population", "Primary effect", "Input format", "Input fields", "Repeated-measures variables", "Independent variables", "Covariates", "Time labels", "Complete N", "Groups", "Time points"),
    Value = c(
      "Within-subject treatment x time repeated-measures ANOVA",
      "PP / complete-case repeated-measures ANOVA",
      "Treatment x Time interaction",
      toupper(input_format), fields,
      paste(normalized$repeated_variables, collapse = ", "),
      "Treatment (within subject), Time (within subject)", if (length(covariate_labels) > 0L) paste(covariate_labels, collapse = ", ") else "None",
      paste(normalized$time_labels, collapse = ", "),
      as.character(dim(values)[1]),
      paste(sprintf("%s (n=%d paired)", normalized$treatment_labels, dim(values)[1]), collapse = ", "),
      as.character(length(normalized$time_labels))
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (identical(input_format, "long")) attr(overview, "one_group_input_fields") <- c(normalized$id_variable, normalized$group_variable, normalized$time_variable, normalized$outcome_variable)
  overview
}

one_group_rm_primary_rows <- function(anova) {
  keys <- attr(anova, "one_group_effect_keys", exact = TRUE)
  if (length(keys) == nrow(anova)) which(keys == "Treatment:Time") else which(anova$Effect == "Treatment x Time")
}

one_group_rm_recommendation_table <- function(anova, assumption, normality, excluded_n) {
  interaction <- anova[one_group_rm_primary_rows(anova), , drop = FALSE]
  primary_p <- if (nrow(interaction) > 0L) mixed_rm_parse_p(interaction$p[[1]]) else NA_real_
  normality_flag <- mixed_rm_normality_issue(normality)
  data.frame(
    Item = c("Recommended model", "Analysis population", "Primary effect", "Follow-up decision", "Assumption decision", "Data condition", "Normality decision", "Mixed-model decision"),
    Recommendation = c(
      "Two-factor within-subject repeated-measures ANOVA",
      "PP / complete-case repeated-measures ANOVA",
      if (nrow(interaction) > 0L) interaction$Effect[[1]] else "Treatment x Time",
      if (is.finite(primary_p) && primary_p < .05) "Prioritize treatment comparisons at each time and time comparisons within treatment." else if (is.finite(primary_p)) "Do not treat the treatment-specific change pattern as supported." else "Review the returned ANOVA table before interpretation.",
      if (nrow(assumption) > 0L) "Use the correction decision shown in the ANOVA table." else "Assumption review was not requested.",
      if (excluded_n > 0L) "Listwise deletion was applied." else "No selected subjects were excluded.",
      if (isTRUE(normality_flag)) "Normality flagged; review a robust or mixed-model sensitivity analysis." else "Normality review did not flag p < .05.",
      "Use a subject/site mixed model when treatment-by-time records are incomplete."
    ),
    Reason = c(
      "Both Treatment and Time are within-subject factors.",
      "Every included subject has both treatments at every selected time.",
      "The interaction tests whether experimental-control differences change over time.",
      if (is.finite(primary_p)) paste0("Primary p=", if (primary_p < .001) "<.001" else format_decimal3(primary_p), ".") else "Primary p was not available.",
      "Time and Treatment x Time have separate sphericity checks.",
      paste0("Excluded subjects: ", excluded_n, "."),
      if (isTRUE(normality_flag)) "At least one Shapiro-Wilk p < .05." else "Cell-level Shapiro-Wilk checks did not flag p < .05.",
      "The current result is a complete-case classical repeated-measures ANOVA."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

prepare_one_group_rm_anova_results <- function(
  data,
  input_format = "wide",
  experimental_variables = character(0),
  control_variables = character(0),
  id_variable = character(0),
  group_variable = character(0),
  time_variable = character(0),
  outcome_variable = character(0),
  covariates = character(0),
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  options = list(),
  repeated_variables = character(0)
) {
  if (length(attr(data, "statedu_scope_excluded"))) analysis_scope_prepare_variables(data, environment(), c("experimental_variables", "control_variables", "id_variable", "group_variable", "time_variable", "outcome_variable", "covariates", "repeated_variables"))
  input_format <- match.arg(tolower(as.character(input_format %||% "wide")[[1]]), c("wide", "long"))
  options$posthoc_adjustment <- if (identical(options$posthoc_adjustment %||% "holm", "bonferroni")) "bonferroni" else "holm"
  normalized <- if (identical(input_format, "long")) {
    required <- as.character(c(id_variable, group_variable, time_variable, outcome_variable) %||% character(0))
    covariates <- unique(as.character(covariates %||% character(0)))
    covariates <- covariates[nzchar(covariates)]
    if (length(required) != 4L || any(!nzchar(required))) one_group_rm_abort("long_roles")
    if (length(unique(c(required, covariates))) < length(c(required, covariates))) one_group_rm_abort("distinct_roles")
    one_group_rm_long_input(data, required[[1]], required[[2]], required[[3]], required[[4]], covariates, variable_info)
  } else {
    if (length(experimental_variables) == 0L && length(repeated_variables) > 0L) one_group_rm_abort("separate_blocks")
    one_group_rm_wide_input(
      data = data,
      experimental_variables = experimental_variables,
      control_variables = control_variables,
      covariates = covariates,
      variable_info = variable_info,
      labels = labels,
      category_table = category_table,
      treatment_labels = options$treatment_labels %||% c("Experimental", "Control"),
      time_labels = options$time_labels %||% character(0)
    )
  }

  covariate_data <- normalized$covariate_data %||% data.frame(row.names = seq_len(dim(normalized$values)[1L]))
  complete <- apply(normalized$values, 1L, function(subject) all(is.finite(subject)))
  if (ncol(covariate_data) > 0L) complete <- complete & stats::complete.cases(covariate_data)
  values <- normalized$values[complete, , , drop = FALSE]
  covariate_data <- covariate_data[complete, , drop = FALSE]
  excluded_n <- sum(!complete)
  if (dim(values)[1] < 3L) one_group_rm_abort("complete_subjects")
  if (!any(apply(values, 1L, function(subject) length(unique(as.numeric(subject))) > 1L))) one_group_rm_abort("identical_values")

  covariate_labels <- vapply(normalized$covariates %||% character(0), paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  anova <- if (ncol(covariate_data) > 0L) one_group_rm_covariate_anova_table(values, covariate_data, covariate_labels) else one_group_rm_anova_table(values, "Treatment", "Time")
  posthoc <- if (isTRUE(options$posthoc %||% TRUE)) one_group_rm_posthoc_rows(values, normalized$treatment_labels, normalized$time_labels, options$posthoc_adjustment, covariate_data) else data.frame()
  if (isTRUE(options$posthoc %||% TRUE) && "post-hoc" %in% names(anova)) {
    primary_rows <- one_group_rm_primary_rows(anova)
    primary_p <- if (length(primary_rows)) mixed_rm_parse_p(anova$p[primary_rows[[1L]]]) else NA_real_
    anova$`post-hoc`[primary_rows] <- if (is.finite(primary_p) && primary_p < .05) "See simple paired comparisons" else "n.s."
  }
  descriptives <- one_group_rm_summary_table(values, normalized$treatment_labels, normalized$time_labels, posthoc, isTRUE(options$posthoc %||% TRUE), covariate_data)
  time_markers <- mixed_rm_time_markers(length(normalized$time_labels))
  time_marker_note <- mixed_rm_time_marker_note(normalized$time_labels, time_markers)
  descriptives_note <- paste("Values are M \u00b1 SD. Within-treatment F/p tests time change in each treatment.", "Between-treatments row uses paired F(p) comparisons at each time.", time_marker_note)
  normality <- if (isTRUE(options$assumption_check %||% TRUE)) one_group_rm_normality_table(values, normalized$treatment_labels, normalized$time_labels) else data.frame()
  assumption <- if (isTRUE(options$assumption_check %||% TRUE)) one_group_rm_assumption_table(values, anova, normalized$total_subjects, excluded_n, normalized$treatment_labels, normalized$time_labels) else data.frame()
  method_note <- paste("Primary: Treatment x Time. Both factors are within subject.", if (length(covariate_labels) > 0L) paste0("Adjusted for: ", paste(covariate_labels, collapse = ", "), ".") else "", if (length(normalized$time_labels) >= 3L) "Sphericity: GG if epsilon(GG) < .75; HF otherwise." else "Sphericity correction is not required with two time points.", "1 ES = partial \u03b7\u00b2.")
  posthoc_note <- paste(if (length(covariate_labels) > 0L) "Treatment comparisons at each time and time comparisons within each treatment are covariate-adjusted paired contrasts." else "Treatment comparisons at each time and time comparisons within each treatment are paired tests.", time_marker_note, sprintf("Adjusted p values use %s correction within each comparison family.", if (identical(options$posthoc_adjustment, "bonferroni")) "Bonferroni" else "Holm-Bonferroni"))

  list(
    type = "one_group_rm_anova", input_format = input_format,
    group_variable = if (identical(input_format, "long")) normalized$group_variable else "Treatment",
    repeated_variables = normalized$repeated_variables, covariates = normalized$covariates %||% character(0),
    treatment_labels = normalized$treatment_labels, time_labels = normalized$time_labels,
    options = options, analysis_population = "pp",
    overview = one_group_rm_model_overview(input_format, normalized, values, excluded_n, covariate_labels),
    recommendation = one_group_rm_recommendation_table(anova, assumption, normality, excluded_n),
    observed_descriptives = data.frame(), observed_descriptives_note = "",
    adjusted_descriptives = data.frame(), adjusted_descriptives_note = "",
    descriptives = descriptives, descriptives_note = descriptives_note,
    anova = anova, assumption = assumption, normality = normality,
    mixed_model_overview = data.frame(), mixed_model_coefficients = data.frame(), mixed_model_note = "",
    posthoc = posthoc, posthoc_note = posthoc_note, method_note = method_note
  )
}
