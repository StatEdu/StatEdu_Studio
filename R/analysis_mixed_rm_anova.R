# Mixed repeated-measures ANOVA: between-subject group x within-subject time.

mixed_rm_measurement_lookup <- function(variable_info = NULL) {
  paired_measurement_lookup(variable_info)
}

mixed_rm_factor_candidates <- function(variable_names, variable_table) {
  variable_names <- as.character(variable_names %||% character(0))
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(variable_names)
  }
  measurements <- mixed_rm_measurement_lookup(variable_table)
  variable_names[variable_names %in% names(measurements) & measurements[variable_names] %in% c("binary", "category", "ordered")]
}

mixed_rm_continuous_candidates <- function(variable_names, variable_table) {
  variable_names <- as.character(variable_names %||% character(0))
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(variable_names)
  }
  measurements <- mixed_rm_measurement_lookup(variable_table)
  variable_names[variable_names %in% names(measurements) & measurements[variable_names] %in% c("continuous", "ordered")]
}

mixed_rm_covariate_candidates <- function(variable_names, variable_table) {
  variable_names <- as.character(variable_names %||% character(0))
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(variable_names)
  }
  measurements <- mixed_rm_measurement_lookup(variable_table)
  variable_names[variable_names %in% names(measurements) & measurements[variable_names] %in% c("binary", "category", "ordered", "continuous")]
}

mixed_rm_time_labels <- function(variables, options = list(), variable_info = NULL, labels = character(0), category_table = NULL) {
  variables <- as.character(variables %||% character(0))
  defaults <- paired_rm_time_header_labels(length(variables))
  option_labels <- as.character(options$time_labels %||% character(0))
  if (length(option_labels) < length(variables)) {
    option_labels <- c(option_labels, defaults[seq.int(length(option_labels) + 1L, length(variables))])
  }
  out <- ifelse(
    nzchar(trimws(option_labels[seq_along(variables)])),
    trimws(option_labels[seq_along(variables)]),
    defaults
  )
  if (length(out) == 0) {
    out <- vapply(variables, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  }
  out
}

mixed_rm_group_factor <- function(values) {
  if (is.factor(values)) {
    out <- droplevels(values)
  } else {
    text <- as.character(values)
    text[!nzchar(trimws(text))] <- NA_character_
    out <- factor(text)
  }
  droplevels(out)
}

mixed_rm_interaction_group <- function(group_data) {
  if (!is.data.frame(group_data) || ncol(group_data) == 0) return(factor())
  if (ncol(group_data) == 1L) return(droplevels(group_data[[1]]))
  droplevels(do.call(interaction, c(group_data, list(drop = TRUE, sep = " / "))))
}

mixed_rm_complete_frame <- function(data, group_variable, repeated_variables, covariates = character(0), covariate_measurements = character(0)) {
  group_variables <- unique(as.character(group_variable %||% character(0)))
  group_variables <- group_variables[nzchar(group_variables)]
  covariates <- as.character(covariates %||% character(0))
  variables <- unique(c(group_variables, repeated_variables, covariates))
  total_n <- nrow(data)
  missing <- setdiff(variables, names(data))
  if (length(missing) > 0) {
    stop("Selected variables were not found in the dataset: ", paste(missing, collapse = ", "))
  }
  group_data <- data.frame(row.names = seq_len(nrow(data)))
  for (name in group_variables) {
    group_data[[name]] <- mixed_rm_group_factor(data[[name]])
  }
  y <- as.data.frame(data[repeated_variables], stringsAsFactors = FALSE)
  y[] <- lapply(y, paired_numeric)
  covariate_data <- data.frame()
  if (length(covariates) > 0) {
    covariate_data <- as.data.frame(data[covariates], stringsAsFactors = FALSE)
    for (name in covariates) {
      measurement <- named_value(covariate_measurements, name, "continuous")
      covariate_data[[name]] <- if (measurement %in% c("binary", "category")) {
        mixed_rm_group_factor(covariate_data[[name]])
      } else {
        paired_numeric(covariate_data[[name]])
      }
    }
  }
  keep <- stats::complete.cases(group_data) & stats::complete.cases(y)
  if (ncol(covariate_data) > 0) {
    keep <- keep & stats::complete.cases(covariate_data)
  }
  if (sum(keep) < 3L) {
    stop("At least three complete cases are required for mixed repeated-measures ANOVA.")
  }
  group_data <- group_data[keep, , drop = FALSE]
  group_data[] <- lapply(group_data, droplevels)
  group <- mixed_rm_interaction_group(group_data)
  y <- y[keep, , drop = FALSE]
  covariate_data <- covariate_data[keep, , drop = FALSE]
  invalid_group_variables <- names(group_data)[vapply(group_data, nlevels, integer(1)) < 2L]
  if (length(invalid_group_variables) > 0) {
    stop("Each independent variable must have at least two non-empty levels: ", paste(invalid_group_variables, collapse = ", "))
  }
  if (nlevels(group) < 2L) {
    stop("Independent variables must define at least two non-empty groups.")
  }
  if (any(table(group) < 2L)) {
    stop("Each independent-variable cell must have at least two complete cases.")
  }
  if (ncol(y) < 2L) {
    stop("Select at least two repeated-measures variables.")
  }
  y_matrix <- as.matrix(y)
  storage.mode(y_matrix) <- "double"
  if (!paired_rm_has_within_subject_change(y_matrix)) {
    stop("Repeated-measures variables do not vary within subjects.")
  }
  list(group = group, group_data = group_data, y = y_matrix, covariates = covariate_data, n = nrow(y_matrix), total_n = total_n, excluded_n = total_n - nrow(y_matrix))
}

mixed_rm_safe_ratio <- function(num, den) {
  if (!is.finite(num) || !is.finite(den) || den <= 0) return(NA_real_)
  num / den
}

mixed_rm_p_value <- function(f_value, df1, df2) {
  if (!is.finite(f_value) || !is.finite(df1) || !is.finite(df2) || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  stats::pf(f_value, df1, df2, lower.tail = FALSE)
}

mixed_rm_hf_epsilon <- function(gg_epsilon, n, k) {
  gg_epsilon <- as.numeric(gg_epsilon %||% NA_real_)
  n <- as.numeric(n %||% NA_real_)
  k <- as.numeric(k %||% NA_real_)
  if (!is.finite(gg_epsilon) || !is.finite(n) || !is.finite(k) || n <= 1 || k <= 2) {
    return(NA_real_)
  }
  numerator <- n * (k - 1) * gg_epsilon - 2
  denominator <- (k - 1) * (n - 1 - (k - 1) * gg_epsilon)
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  max(1 / (k - 1), min(1, numerator / denominator))
}

mixed_rm_sphericity_row <- function(effect_key, sphericity = NULL, p_adjust = NULL, n = NA_integer_, k = NA_integer_) {
  effect_key <- as.character(effect_key %||% "")
  out <- list(w = NA_real_, p = NA_real_, gg = NA_real_, hf = NA_real_, gg_p = NA_real_, hf_p = NA_real_)
  if (is.data.frame(sphericity)) {
    if (effect_key %in% rownames(sphericity)) {
      if ("Test statistic" %in% colnames(sphericity)) out$w <- as.numeric(sphericity[effect_key, "Test statistic"])
      if ("p-value" %in% colnames(sphericity)) out$p <- as.numeric(sphericity[effect_key, "p-value"])
    }
  } else if (is.list(sphericity)) {
    out$w <- as.numeric(sphericity$w %||% NA_real_)
    out$p <- as.numeric(sphericity$p %||% NA_real_)
    out$gg <- as.numeric(sphericity$epsilon %||% NA_real_)
  }
  if (is.data.frame(p_adjust) && effect_key %in% rownames(p_adjust)) {
    if ("GG eps" %in% colnames(p_adjust)) out$gg <- as.numeric(p_adjust[effect_key, "GG eps"])
    if ("HF eps" %in% colnames(p_adjust)) out$hf <- as.numeric(p_adjust[effect_key, "HF eps"])
    if ("Pr(>F[GG])" %in% colnames(p_adjust)) out$gg_p <- as.numeric(p_adjust[effect_key, "Pr(>F[GG])"])
    if ("Pr(>F[HF])" %in% colnames(p_adjust)) out$hf_p <- as.numeric(p_adjust[effect_key, "Pr(>F[HF])"])
  }
  if (!is.finite(out$hf)) {
    out$hf <- mixed_rm_hf_epsilon(out$gg, n, k)
  }
  if (is.finite(out$gg)) {
    out$gg <- max(1 / (as.numeric(k) - 1), min(1, out$gg))
  }
  if (is.finite(out$hf)) {
    capped_hf <- max(1 / (as.numeric(k) - 1), min(1, out$hf))
    if (!isTRUE(all.equal(capped_hf, out$hf))) out$hf_p <- NA_real_
    out$hf <- capped_hf
  }
  out
}

mixed_rm_correction_cells <- function(effect_key, effect_label, f_value, df1, df2, raw_p, n, k, sphericity = NULL, p_adjust = NULL) {
  within_effect <- identical(effect_label, "Time") || grepl(" x Time$", effect_label)
  if (!isTRUE(within_effect)) {
    return(list(
      w = "", sphericity_p = "", gg = "", hf = "", correction = "",
      df1 = format_decimal3(df1), df2 = format_decimal3(df2), p = format_p(raw_p)
    ))
  }
  if (!is.finite(k) || k < 3) {
    return(list(
      w = "", sphericity_p = "", gg = "", hf = "", correction = "Not required",
      df1 = format_decimal3(df1), df2 = format_decimal3(df2), p = format_p(raw_p)
    ))
  }
  sph <- mixed_rm_sphericity_row(effect_key, sphericity, p_adjust, n, k)
  gg_p <- if (is.finite(sph$gg_p)) sph$gg_p else mixed_rm_p_value(f_value, df1 * sph$gg, df2 * sph$gg)
  hf_p <- if (is.finite(sph$hf_p)) sph$hf_p else mixed_rm_p_value(f_value, df1 * sph$hf, df2 * sph$hf)
  satisfied <- is.finite(sph$p) && sph$p >= .05
  if (isTRUE(satisfied)) {
    correction <- "Sphericity assumed"
    final_df1 <- df1
    final_df2 <- df2
    final_p <- raw_p
  } else if (is.finite(sph$gg) && sph$gg < .75) {
    correction <- "Greenhouse-Geisser"
    final_df1 <- df1 * sph$gg
    final_df2 <- df2 * sph$gg
    final_p <- gg_p
  } else if (is.finite(sph$hf)) {
    correction <- "Huynh-Feldt"
    final_df1 <- df1 * sph$hf
    final_df2 <- df2 * sph$hf
    final_p <- hf_p
  } else {
    correction <- "Sphericity assumed"
    final_df1 <- df1
    final_df2 <- df2
    final_p <- raw_p
  }
  list(
    w = format_decimal3(sph$w),
    sphericity_p = format_p(sph$p),
    gg = format_decimal3(sph$gg),
    hf = format_decimal3(sph$hf),
    correction = correction,
    df1 = format_decimal3(final_df1),
    df2 = format_decimal3(final_df2),
    p = format_p(final_p)
  )
}

mixed_rm_reference_grid <- function(group_level, group, covariate_data) {
  rows <- list(.group = factor(group_level, levels = levels(group)))
  if (!is.null(covariate_data) && ncol(covariate_data) > 0) {
    for (name in names(covariate_data)) {
      values <- covariate_data[[name]]
      if (is.factor(values)) {
        rows[[name]] <- factor(levels(values), levels = levels(values))
      } else {
        rows[[name]] <- mean(as.numeric(values), na.rm = TRUE)
      }
    }
  }
  grid <- expand.grid(rows, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$.group <- factor(grid$.group, levels = levels(group))
  for (name in names(covariate_data %||% data.frame())) {
    if (is.factor(covariate_data[[name]])) {
      grid[[name]] <- factor(grid[[name]], levels = levels(covariate_data[[name]]))
    }
  }
  grid
}

mixed_rm_adjusted_cell <- function(response, group, covariate_data, group_level) {
  safe_covariates <- covariate_data
  names(safe_covariates) <- paste0(".cov", seq_len(ncol(safe_covariates)))
  model_data <- data.frame(.response = response, .group = group, safe_covariates, check.names = FALSE)
  predictors <- c(".group", names(safe_covariates))
  fit <- stats::lm(stats::as.formula(paste(".response ~", paste(predictors, collapse = " + "))), data = model_data)
  newdata <- mixed_rm_reference_grid(group_level, group, safe_covariates)
  terms_obj <- stats::delete.response(stats::terms(fit))
  design <- stats::model.matrix(terms_obj, newdata)
  beta <- stats::coef(fit)
  cov_beta <- stats::vcov(fit)
  common <- intersect(colnames(design), names(beta))
  estimable <- common[is.finite(beta[common]) & is.finite(diag(cov_beta)[common])]
  if (length(estimable) == 0) return("")
  linear <- colMeans(design[, estimable, drop = FALSE])
  estimate <- as.numeric(sum(linear * beta[estimable]))
  se <- sqrt(as.numeric(t(linear) %*% cov_beta[estimable, estimable, drop = FALSE] %*% linear))
  paste0(format_decimal3(estimate), " \u00b1 ", format_decimal3(se))
}

mixed_rm_ss_table <- function(y, group) {
  group <- droplevels(group)
  group_levels <- levels(group)
  n <- nrow(y)
  k <- ncol(y)
  g_count <- length(group_levels)
  grand <- mean(y)
  subject_means <- rowMeans(y)
  time_means <- colMeans(y)
  group_means <- vapply(group_levels, function(level) mean(y[group == level, , drop = FALSE]), numeric(1))
  group_n <- as.numeric(table(group)[group_levels])
  group_by_time <- t(vapply(group_levels, function(level) colMeans(y[group == level, , drop = FALSE]), numeric(k)))

  ss_total <- sum((y - grand)^2)
  ss_group <- k * sum(group_n * (group_means - grand)^2)
  ss_subjects_within <- k * sum(vapply(seq_len(n), function(index) {
    level <- as.character(group[[index]])
    (subject_means[[index]] - group_means[[level]])^2
  }, numeric(1)))
  ss_time <- n * sum((time_means - grand)^2)
  ss_group_time <- sum(vapply(seq_along(group_levels), function(index) {
    level <- group_levels[[index]]
    group_n[[index]] * sum((group_by_time[index, ] - group_means[[level]] - time_means + grand)^2)
  }, numeric(1)))
  ss_error_time <- ss_total - ss_group - ss_subjects_within - ss_time - ss_group_time
  if (is.finite(ss_error_time) && ss_error_time < 0 && abs(ss_error_time) < 1e-8) {
    ss_error_time <- 0
  }

  df_group <- g_count - 1L
  df_subjects_within <- n - g_count
  df_time <- k - 1L
  df_group_time <- df_group * df_time
  df_error_time <- df_subjects_within * df_time

  ms_group <- mixed_rm_safe_ratio(ss_group, df_group)
  ms_subjects_within <- mixed_rm_safe_ratio(ss_subjects_within, df_subjects_within)
  ms_time <- mixed_rm_safe_ratio(ss_time, df_time)
  ms_group_time <- mixed_rm_safe_ratio(ss_group_time, df_group_time)
  ms_error_time <- mixed_rm_safe_ratio(ss_error_time, df_error_time)

  effects <- data.frame(
    Effect = c("Group", "Time", "Group x Time"),
    SS = c(ss_group, ss_time, ss_group_time),
    df1 = c(df_group, df_time, df_group_time),
    df2 = c(df_subjects_within, df_error_time, df_error_time),
    MS = c(ms_group, ms_time, ms_group_time),
    Error = c("Subjects within Group", "Time x Subjects within Group", "Time x Subjects within Group"),
    F = c(
      mixed_rm_safe_ratio(ms_group, ms_subjects_within),
      mixed_rm_safe_ratio(ms_time, ms_error_time),
      mixed_rm_safe_ratio(ms_group_time, ms_error_time)
    ),
    `partial eta2` = c(
      mixed_rm_safe_ratio(ss_group, ss_group + ss_subjects_within),
      mixed_rm_safe_ratio(ss_time, ss_time + ss_error_time),
      mixed_rm_safe_ratio(ss_group_time, ss_group_time + ss_error_time)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  effects$p <- mapply(mixed_rm_p_value, effects$F, effects$df1, effects$df2)
  list(
    effects = effects,
    components = data.frame(
      Source = c("Subjects within Group", "Time x Subjects within Group", "Total"),
      SS = c(ss_subjects_within, ss_error_time, ss_total),
      df = c(df_subjects_within, df_error_time, n * k - 1L),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
}

mixed_rm_formatted_anova <- function(y, group) {
  stats <- mixed_rm_ss_table(y, group)
  table <- stats$effects
  sphericity <- if (ncol(y) >= 3L) paired_rm_sphericity(y) else list(epsilon = NA_real_, p = NA_real_)
  corrections <- lapply(seq_len(nrow(table)), function(index) {
    mixed_rm_correction_cells(
      effect_key = table$Effect[[index]],
      effect_label = table$Effect[[index]],
      f_value = table$F[[index]],
      df1 = table$df1[[index]],
      df2 = table$df2[[index]],
      raw_p = table$p[[index]],
      n = nrow(y),
      k = ncol(y),
      sphericity = sphericity
    )
  })
  out <- data.frame(
    Effect = table$Effect,
    `Mauchly W` = vapply(corrections, `[[`, character(1), "w"),
    `p_sphericity` = vapply(corrections, `[[`, character(1), "sphericity_p"),
    `epsilon(GG)` = vapply(corrections, `[[`, character(1), "gg"),
    `epsilon(HF)` = vapply(corrections, `[[`, character(1), "hf"),
    Correction = vapply(corrections, `[[`, character(1), "correction"),
    df1 = vapply(corrections, `[[`, character(1), "df1"),
    df2 = vapply(corrections, `[[`, character(1), "df2"),
    F = vapply(table$F, format_decimal3, character(1)),
    p = vapply(corrections, `[[`, character(1), "p"),
    `partial eta2` = vapply(table$`partial eta2`, format_decimal3, character(1)),
    `post-hoc` = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(out, "raw") <- table
  attr(out, "sphericity") <- sphericity
  out
}

mixed_rm_effect_display <- function(effect, group_labels = character(0), covariate_labels = character(0)) {
  effect <- as.character(effect %||% "")
  for (key in names(covariate_labels)) {
    label <- covariate_labels[[key]]
    if (identical(effect, key)) return(label)
  }
  tokens <- strsplit(effect, ":", fixed = TRUE)[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) return(effect)
  mapped <- vapply(tokens, function(token) {
    if (identical(token, "Time")) return("Time")
    if (token %in% names(group_labels)) return(as.character(group_labels[[token]]))
    if (token %in% names(covariate_labels)) return(as.character(covariate_labels[[token]]))
    token
  }, character(1))
  if ("Time" %in% mapped && length(mapped) > 1L) {
    mapped <- c(mapped[mapped != "Time"], "Time")
  }
  paste(mapped, collapse = " x ")
}

mixed_rm_car_anova <- function(y, group_data, covariate_data, group_labels = character(0), covariate_labels = character(0)) {
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Covariate-adjusted repeated-measures ANOVA requires the car package.")
  }
  if (!is.data.frame(group_data)) {
    group_data <- data.frame(.group = group_data, stringsAsFactors = FALSE)
  }
  model_data <- data.frame(row.names = seq_len(nrow(y)))
  for (index in seq_len(ncol(group_data))) {
    model_data[[paste0(".iv", index)]] <- mixed_rm_group_factor(group_data[[index]])
  }
  for (index in seq_len(ncol(covariate_data))) {
    model_data[[paste0(".cov", index)]] <- covariate_data[[index]]
  }
  group_keys <- if (ncol(group_data) > 0L) paste0(".iv", seq_len(ncol(group_data))) else character(0)
  if (length(group_labels) != length(group_keys)) {
    group_labels <- stats::setNames(if (length(group_keys) == 1L) "Group" else group_keys, group_keys)
  } else {
    names(group_labels) <- group_keys
  }
  covariate_keys <- if (ncol(covariate_data) > 0L) paste0(".cov", seq_len(ncol(covariate_data))) else character(0)
  if (length(covariate_labels) != length(covariate_keys)) {
    covariate_labels <- stats::setNames(covariate_keys, covariate_keys)
  } else {
    names(covariate_labels) <- covariate_keys
  }
  group_term <- if (length(group_keys) == 1L) group_keys[[1]] else paste0("(", paste(group_keys, collapse = " * "), ")")
  predictors <- c(group_term, covariate_keys)
  fit <- stats::lm(stats::as.formula(paste("y ~", paste(predictors, collapse = " + "))), data = model_data)
  idata <- data.frame(Time = factor(seq_len(ncol(y)), levels = seq_len(ncol(y))))
  model <- car::Anova(fit, idata = idata, idesign = ~Time, type = "II")
  summary_model <- suppressWarnings(summary(model, multivariate = FALSE))
  uni <- as.data.frame.matrix(summary_model$univariate.tests, stringsAsFactors = FALSE)
  effect_names <- rownames(summary_model$univariate.tests)
  keep <- effect_names != "(Intercept)"
  uni <- uni[keep, , drop = FALSE]
  effect_names <- effect_names[keep]
  p_adjust <- if (!is.null(summary_model$pval.adjustments)) {
    as.data.frame.matrix(summary_model$pval.adjustments, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  corrections <- lapply(seq_along(effect_names), function(index) {
    mixed_rm_correction_cells(
      effect_key = effect_names[[index]],
      effect_label = mixed_rm_effect_display(effect_names[[index]], group_labels, covariate_labels),
      f_value = uni$`F value`[[index]],
      df1 = uni$`num Df`[[index]],
      df2 = uni$`den Df`[[index]],
      raw_p = uni$`Pr(>F)`[[index]],
      n = nrow(y),
      k = ncol(y),
      sphericity = if (!is.null(summary_model$sphericity.tests)) as.data.frame.matrix(summary_model$sphericity.tests, stringsAsFactors = FALSE) else NULL,
      p_adjust = p_adjust
    )
  })
  table <- data.frame(
    Effect = vapply(effect_names, mixed_rm_effect_display, character(1), group_labels = group_labels, covariate_labels = covariate_labels),
    `Mauchly W` = vapply(corrections, `[[`, character(1), "w"),
    `p_sphericity` = vapply(corrections, `[[`, character(1), "sphericity_p"),
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
  sphericity <- if (!is.null(summary_model$sphericity.tests)) {
    as.data.frame.matrix(summary_model$sphericity.tests, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  attr(table, "sphericity") <- sphericity
  attr(table, "p_adjustments") <- p_adjust
  table
}

mixed_rm_time_markers <- function(n) {
  if (n <= length(letters)) letters[seq_len(n)] else paste0("t", seq_len(n))
}

mixed_rm_group_time_posthoc <- function(y, time_markers, adjustment = statedu_multiple_correction_default()) {
  if (!is.matrix(y) && !is.data.frame(y)) return("")
  y <- as.matrix(y)
  if (nrow(y) < 2 || ncol(y) < 3) return("")
  time_pairs <- utils::combn(seq_len(ncol(y)), 2, simplify = FALSE)
  p_values <- numeric(length(time_pairs))
  for (pair_index in seq_along(time_pairs)) {
    pair <- time_pairs[[pair_index]]
    test <- tryCatch(stats::t.test(y[, pair[[1]]], y[, pair[[2]]], paired = TRUE), error = function(e) NULL)
    p_values[[pair_index]] <- if (!is.null(test)) test$p.value else NA_real_
  }
  adjusted <- stats::p.adjust(p_values, method = adjustment)
  rows <- list()
  for (pair_index in seq_along(time_pairs)) {
    pair <- time_pairs[[pair_index]]
    rows[[length(rows) + 1L]] <- data.frame(
      Contrast = mixed_rm_pair_label(time_markers[[pair[[1]]]], time_markers[[pair[[2]]]]),
      `p adjusted` = format_p(adjusted[[pair_index]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  means <- stats::setNames(colMeans(y), time_markers)
  mixed_rm_significant_order_notation(time_markers, means, if (length(rows) == 0) data.frame() else do.call(rbind, rows))
}

mixed_rm_covariate_reference_grid <- function(covariate_data) {
  if (is.null(covariate_data) || ncol(covariate_data) == 0) {
    return(data.frame(.intercept = 1)[0, , drop = FALSE])
  }
  rows <- list()
  for (name in names(covariate_data)) {
    values <- covariate_data[[name]]
    if (is.factor(values)) {
      rows[[name]] <- levels(droplevels(values))
    } else {
      rows[[name]] <- mean(as.numeric(values), na.rm = TRUE)
    }
  }
  grid <- expand.grid(rows, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (name in names(covariate_data)) {
    if (is.factor(covariate_data[[name]])) {
      grid[[name]] <- factor(grid[[name]], levels = levels(droplevels(covariate_data[[name]])))
    }
  }
  grid
}

mixed_rm_reduced_covariates <- function(covariate_data) {
  if (is.null(covariate_data) || ncol(covariate_data) == 0) return(data.frame())
  out <- covariate_data
  keep <- vapply(names(out), function(name) {
    values <- out[[name]]
    if (is.factor(values)) {
      out[[name]] <<- droplevels(values)
      return(nlevels(out[[name]]) >= 2L)
    }
    numeric_values <- suppressWarnings(as.numeric(values))
    length(unique(numeric_values[is.finite(numeric_values)])) >= 2L
  }, logical(1))
  out[, keep, drop = FALSE]
}

mixed_rm_adjusted_time_estimate <- function(response, covariate_data) {
  response <- as.numeric(response)
  covariate_data <- mixed_rm_reduced_covariates(covariate_data)
  if (ncol(covariate_data) == 0) {
    return(mean(response, na.rm = TRUE))
  }
  safe_covariates <- covariate_data
  names(safe_covariates) <- paste0(".cov", seq_len(ncol(safe_covariates)))
  model_data <- data.frame(.response = response, safe_covariates, check.names = FALSE)
  predictors <- names(safe_covariates)
  fit <- tryCatch(stats::lm(stats::as.formula(paste(".response ~", paste(predictors, collapse = " + "))), data = model_data), error = function(e) NULL)
  if (is.null(fit)) return(mean(response, na.rm = TRUE))
  newdata <- mixed_rm_covariate_reference_grid(safe_covariates)
  if (nrow(newdata) == 0) return(mean(response, na.rm = TRUE))
  terms_obj <- stats::delete.response(stats::terms(fit))
  design <- stats::model.matrix(terms_obj, newdata)
  beta <- stats::coef(fit)
  common <- intersect(colnames(design), names(beta))
  if (length(common) == 0) return(mean(response, na.rm = TRUE))
  as.numeric(sum(colMeans(design[, common, drop = FALSE]) * beta[common], na.rm = TRUE))
}

mixed_rm_adjusted_pair_p <- function(diff, covariate_data) {
  test <- mixed_rm_adjusted_pair_test(diff, covariate_data)
  test$p
}

mixed_rm_adjusted_pair_test <- function(diff, covariate_data) {
  diff <- as.numeric(diff)
  covariate_data <- mixed_rm_reduced_covariates(covariate_data)
  if (ncol(covariate_data) == 0) {
    test <- tryCatch(stats::t.test(diff), error = function(e) NULL)
    return(list(
      statistic = if (!is.null(test)) unname(test$statistic) else NA_real_,
      df = if (!is.null(test)) unname(test$parameter) else NA_real_,
      p = if (!is.null(test)) test$p.value else NA_real_
    ))
  }
  safe_covariates <- covariate_data
  names(safe_covariates) <- paste0(".cov", seq_len(ncol(safe_covariates)))
  model_data <- data.frame(.diff = diff, safe_covariates, check.names = FALSE)
  fit <- tryCatch(stats::lm(stats::as.formula(paste(".diff ~", paste(names(safe_covariates), collapse = " + "))), data = model_data), error = function(e) NULL)
  if (is.null(fit)) return(list(statistic = NA_real_, df = NA_real_, p = NA_real_))
  newdata <- mixed_rm_covariate_reference_grid(safe_covariates)
  terms_obj <- stats::delete.response(stats::terms(fit))
  design <- stats::model.matrix(terms_obj, newdata)
  beta <- stats::coef(fit)
  cov_beta <- stats::vcov(fit)
  common <- intersect(colnames(design), names(beta))
  estimable <- common[is.finite(beta[common]) & is.finite(diag(cov_beta)[common])]
  if (length(estimable) == 0) return(list(statistic = NA_real_, df = NA_real_, p = NA_real_))
  linear <- colMeans(design[, estimable, drop = FALSE])
  estimate <- as.numeric(sum(linear * beta[estimable]))
  se <- sqrt(as.numeric(t(linear) %*% cov_beta[estimable, estimable, drop = FALSE] %*% linear))
  df <- stats::df.residual(fit)
  if (!is.finite(se) || se <= 0 || !is.finite(df) || df <= 0) return(list(statistic = NA_real_, df = NA_real_, p = NA_real_))
  statistic <- estimate / se
  list(statistic = statistic, df = df, p = stats::pt(abs(statistic), df = df, lower.tail = FALSE) * 2)
}

mixed_rm_adjusted_group_time_posthoc <- function(y, covariate_data, time_markers, adjustment = statedu_multiple_correction_default()) {
  if (!is.matrix(y) && !is.data.frame(y)) return("")
  y <- as.matrix(y)
  if (nrow(y) < 2 || ncol(y) < 3) return("")
  covariate_data <- mixed_rm_reduced_covariates(covariate_data)
  time_pairs <- utils::combn(seq_len(ncol(y)), 2, simplify = FALSE)
  p_values <- vapply(time_pairs, function(pair) {
    mixed_rm_adjusted_pair_p(y[, pair[[2]]] - y[, pair[[1]]], covariate_data)
  }, numeric(1))
  adjusted <- stats::p.adjust(p_values, method = adjustment)
  rows <- data.frame(
    Contrast = vapply(time_pairs, function(pair) mixed_rm_pair_label(time_markers[[pair[[1]]]], time_markers[[pair[[2]]]]), character(1)),
    `p adjusted` = vapply(adjusted, format_p, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  means <- stats::setNames(vapply(seq_len(ncol(y)), function(index) mixed_rm_adjusted_time_estimate(y[, index], covariate_data), numeric(1)), time_markers)
  mixed_rm_significant_order_notation(time_markers, means, rows)
}

mixed_rm_within_group_time_test <- function(y) {
  y <- as.matrix(y)
  if (nrow(y) < 2 || ncol(y) < 2) return(list(f = NA_real_, p = NA_real_))
  test <- tryCatch(paired_rm_anova(y), error = function(e) NULL)
  if (is.null(test)) return(list(f = NA_real_, p = NA_real_))
  list(f = test$f, p = test$p)
}

mixed_rm_adjusted_within_group_time_test <- function(y, covariate_data) {
  y <- as.matrix(y)
  if (nrow(y) < 3 || ncol(y) < 2) return(list(f = NA_real_, p = NA_real_, p_text = ""))
  covariate_data <- mixed_rm_reduced_covariates(covariate_data)
  if (ncol(covariate_data) == 0 || !requireNamespace("car", quietly = TRUE)) {
    raw <- mixed_rm_within_group_time_test(y)
    raw$p_text <- format_p(raw$p)
    return(raw)
  }
  safe_covariates <- covariate_data
  names(safe_covariates) <- paste0(".cov", seq_len(ncol(safe_covariates)))
  fit <- tryCatch(stats::lm(y ~ ., data = safe_covariates), error = function(e) NULL)
  if (is.null(fit)) {
    raw <- mixed_rm_within_group_time_test(y)
    raw$p_text <- format_p(raw$p)
    return(raw)
  }
  idata <- data.frame(Time = factor(seq_len(ncol(y)), levels = seq_len(ncol(y))))
  model <- tryCatch(car::Anova(fit, idata = idata, idesign = ~Time, type = "II"), error = function(e) NULL)
  summary_model <- if (!is.null(model)) tryCatch(suppressWarnings(summary(model, multivariate = FALSE)), error = function(e) NULL) else NULL
  if (is.null(summary_model) || is.null(summary_model$univariate.tests)) {
    raw <- mixed_rm_within_group_time_test(y)
    raw$p_text <- format_p(raw$p)
    return(raw)
  }
  uni <- as.data.frame.matrix(summary_model$univariate.tests, stringsAsFactors = FALSE)
  effect_names <- rownames(summary_model$univariate.tests)
  matched <- effect_names[effect_names %in% c("Time", "(Intercept):Time")]
  if (length(matched) == 0) matched <- effect_names[grepl("(^|:)Time$", effect_names)]
  if (length(matched) == 0) return(list(f = NA_real_, p = NA_real_, p_text = ""))
  index <- match(matched[[1]], effect_names)
  p_adjust <- if (!is.null(summary_model$pval.adjustments)) {
    as.data.frame.matrix(summary_model$pval.adjustments, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  correction <- mixed_rm_correction_cells(
    effect_key = matched[[1]],
    effect_label = "Time",
    f_value = uni$`F value`[[index]],
    df1 = uni$`num Df`[[index]],
    df2 = uni$`den Df`[[index]],
    raw_p = uni$`Pr(>F)`[[index]],
    n = nrow(y),
    k = ncol(y),
    sphericity = if (!is.null(summary_model$sphericity.tests)) as.data.frame.matrix(summary_model$sphericity.tests, stringsAsFactors = FALSE) else NULL,
    p_adjust = p_adjust
  )
  list(f = uni$`F value`[[index]], p = mixed_rm_parse_p(correction$p), p_text = correction$p)
}

mixed_rm_between_group_stat_label <- function(f_value, p_value) {
  paste0(format_decimal3(f_value), "(", format_p(p_value), ")")
}

mixed_rm_simple_between_group_cell <- function(response, group, adjustment = statedu_multiple_correction_default()) {
  group <- droplevels(group)
  levels <- levels(group)
  means <- stats::setNames(vapply(levels, function(level) mean(response[group == level], na.rm = TRUE), numeric(1)), levels)
  if (length(levels) == 2L) {
    test <- tryCatch(stats::t.test(response ~ group), error = function(e) NULL)
    f_value <- if (!is.null(test)) unname(test$statistic) ^ 2 else NA_real_
    p_value <- if (!is.null(test)) test$p.value else NA_real_
    notation <- ""
  } else {
    fit <- tryCatch(stats::aov(response ~ group), error = function(e) NULL)
    summary_fit <- if (!is.null(fit)) summary(fit)[[1]] else NULL
    f_value <- if (!is.null(summary_fit) && nrow(summary_fit) > 0) summary_fit$`F value`[[1]] else NA_real_
    p_value <- if (!is.null(summary_fit) && nrow(summary_fit) > 0) summary_fit$`Pr(>F)`[[1]] else NA_real_
    pairs <- utils::combn(levels, 2, simplify = FALSE)
    p_values <- vapply(pairs, function(pair) {
      keep <- group %in% pair
      test <- tryCatch(stats::t.test(response[keep] ~ droplevels(group[keep])), error = function(e) NULL)
      if (!is.null(test)) test$p.value else NA_real_
    }, numeric(1))
    adjusted <- stats::p.adjust(p_values, method = adjustment)
    rows <- data.frame(
      Contrast = vapply(pairs, function(pair) mixed_rm_pair_label(pair[[1]], pair[[2]]), character(1)),
      `p adjusted` = vapply(adjusted, format_p, character(1)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    notation <- mixed_rm_significant_order_notation(levels, means, rows)
  }
  list(stat = mixed_rm_between_group_stat_label(f_value, p_value), posthoc = notation)
}

mixed_rm_adjusted_group_fit <- function(response, group, covariate_data) {
  safe_covariates <- covariate_data
  names(safe_covariates) <- paste0(".cov", seq_len(ncol(safe_covariates)))
  model_data <- data.frame(.response = response, .group = group, safe_covariates, check.names = FALSE)
  predictors <- c(".group", names(safe_covariates))
  full <- stats::lm(stats::as.formula(paste(".response ~", paste(predictors, collapse = " + "))), data = model_data)
  reduced_formula <- if (length(names(safe_covariates)) > 0) {
    stats::as.formula(paste(".response ~", paste(names(safe_covariates), collapse = " + ")))
  } else {
    .response ~ 1
  }
  reduced <- stats::lm(reduced_formula, data = model_data)
  list(full = full, reduced = reduced, safe_covariates = safe_covariates)
}

mixed_rm_adjusted_group_estimates <- function(response, group, covariate_data) {
  fit <- mixed_rm_adjusted_group_fit(response, group, covariate_data)
  terms_obj <- stats::delete.response(stats::terms(fit$full))
  beta <- stats::coef(fit$full)
  cov_beta <- stats::vcov(fit$full)
  group_levels <- levels(group)
  linear_map <- list()
  estimates <- numeric(length(group_levels))
  names(estimates) <- group_levels
  for (level in group_levels) {
    grid <- mixed_rm_reference_grid(level, group, fit$safe_covariates)
    design <- stats::model.matrix(terms_obj, grid)
    linear <- stats::setNames(rep(0, length(beta)), names(beta))
    common <- intersect(colnames(design), names(beta))
    linear[common] <- colMeans(design[, common, drop = FALSE])
    linear_map[[level]] <- linear
    estimates[[level]] <- sum(linear * beta, na.rm = TRUE)
  }
  list(estimates = estimates, linear_map = linear_map, beta = beta, cov_beta = cov_beta, df = stats::df.residual(fit$full), fit = fit)
}

mixed_rm_estimated_between_group_cell <- function(response, group, covariate_data, adjustment = statedu_multiple_correction_default()) {
  if (is.null(covariate_data) || ncol(covariate_data) == 0) {
    return(mixed_rm_simple_between_group_cell(response, group, adjustment))
  }
  group <- droplevels(group)
  group_levels <- levels(group)
  estimated <- tryCatch(mixed_rm_adjusted_group_estimates(response, group, covariate_data), error = function(e) NULL)
  if (is.null(estimated)) return("")
  comparison <- tryCatch(stats::anova(estimated$fit$reduced, estimated$fit$full), error = function(e) NULL)
  f_value <- if (!is.null(comparison) && nrow(comparison) >= 2L) comparison$F[[2]] else NA_real_
  p_value <- if (!is.null(comparison) && nrow(comparison) >= 2L) comparison$`Pr(>F)`[[2]] else NA_real_
  notation <- ""
  if (length(group_levels) >= 3L) {
    pairs <- utils::combn(group_levels, 2, simplify = FALSE)
    p_values <- vapply(pairs, function(pair) {
      contrast <- estimated$linear_map[[pair[[1]]]] - estimated$linear_map[[pair[[2]]]]
      se <- sqrt(as.numeric(t(contrast) %*% estimated$cov_beta %*% contrast))
      estimate <- sum(contrast * estimated$beta, na.rm = TRUE)
      if (!is.finite(se) || se <= 0 || !is.finite(estimated$df)) return(NA_real_)
      stats::pt(abs(estimate / se), df = estimated$df, lower.tail = FALSE) * 2
    }, numeric(1))
    adjusted <- stats::p.adjust(p_values, method = adjustment)
    rows <- data.frame(
      Contrast = vapply(pairs, function(pair) mixed_rm_pair_label(pair[[1]], pair[[2]]), character(1)),
      `p adjusted` = vapply(adjusted, format_p, character(1)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    notation <- mixed_rm_significant_order_notation(group_levels, estimated$estimates, rows)
  }
  list(stat = mixed_rm_between_group_stat_label(f_value, p_value), posthoc = notation)
}

mixed_rm_between_group_summary_row <- function(y, group, time_labels, covariate_data = NULL, method = "simple", adjustment = statedu_multiple_correction_default(), include_within = TRUE, include_posthoc = TRUE) {
  cells <- lapply(seq_len(ncol(y)), function(index) {
    if (identical(method, "estimated")) {
      mixed_rm_estimated_between_group_cell(y[, index], group, covariate_data, adjustment)
    } else {
      mixed_rm_simple_between_group_cell(y[, index], group, adjustment)
    }
  })
  row <- data.frame(Group = "Between groups", N = "F(p)", stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(time_labels)) {
    row[[time_labels[[index]]]] <- cells[[index]]$stat %||% ""
  }
  if (isTRUE(include_within)) {
    row[["F"]] <- ""
    row[["p"]] <- ""
    if (isTRUE(include_posthoc) && ncol(y) >= 3L) row[["post-hoc"]] <- ""
  }
  rows <- list(row)
  posthoc_values <- vapply(cells, function(cell) as.character(cell$posthoc %||% ""), character(1))
  if (length(levels(droplevels(group))) >= 3L) {
    posthoc_row <- data.frame(Group = "post-hoc", N = "", stringsAsFactors = FALSE, check.names = FALSE)
    for (index in seq_along(time_labels)) {
      posthoc_row[[time_labels[[index]]]] <- if (nzchar(posthoc_values[[index]])) posthoc_values[[index]] else "n.s."
    }
    if (isTRUE(include_within)) {
      posthoc_row[["F"]] <- ""
      posthoc_row[["p"]] <- ""
      if (isTRUE(include_posthoc) && ncol(y) >= 3L) posthoc_row[["post-hoc"]] <- ""
    }
    rows[[length(rows) + 1L]] <- posthoc_row
  }
  rows
}

mixed_rm_descriptives <- function(
  y,
  group,
  time_labels,
  covariate_data = NULL,
  adjustment = statedu_multiple_correction_default(),
  include_within = TRUE,
  include_between = FALSE,
  between_method = "simple",
  include_posthoc = TRUE,
  adjust = !is.null(covariate_data) && ncol(covariate_data) > 0
) {
  rows <- list()
  adjusted <- isTRUE(adjust) && !is.null(covariate_data) && ncol(covariate_data) > 0
  include_within_tests <- isTRUE(include_within)
  adjustment <- as.character(adjustment %||% statedu_multiple_correction_default())
  if (!adjustment %in% c("holm", "bonferroni")) adjustment <- statedu_multiple_correction_default()
  time_markers <- mixed_rm_time_markers(ncol(y))
  for (level in levels(group)) {
    subset <- y[group == level, , drop = FALSE]
    row <- data.frame(Group = level, N = nrow(subset), stringsAsFactors = FALSE, check.names = FALSE)
    for (index in seq_len(ncol(y))) {
      values <- subset[, index]
      row[[time_labels[[index]]]] <- if (isTRUE(adjusted)) {
        tryCatch(mixed_rm_adjusted_cell(y[, index], group, covariate_data, level), error = function(e) "")
      } else {
        paste0(format_decimal3(mean(values, na.rm = TRUE)), " \u00b1 ", format_decimal3(stats::sd(values, na.rm = TRUE)))
      }
    }
    if (isTRUE(include_within_tests)) {
      within_test <- if (isTRUE(adjusted)) {
        mixed_rm_adjusted_within_group_time_test(subset, covariate_data[group == level, , drop = FALSE])
      } else {
        mixed_rm_within_group_time_test(subset)
      }
      row[["F"]] <- format_decimal3(within_test$f)
      row[["p"]] <- within_test$p_text %||% format_p(within_test$p)
      if (isTRUE(include_posthoc) && ncol(y) >= 3L) {
        row[["post-hoc"]] <- if (mixed_rm_p_is_significant(within_test$p)) {
          if (isTRUE(adjusted)) {
            mixed_rm_adjusted_group_time_posthoc(subset, covariate_data[group == level, , drop = FALSE], time_markers, adjustment)
          } else {
            mixed_rm_group_time_posthoc(subset, time_markers, adjustment)
          }
        } else {
          "n.s."
        }
      }
    }
    rows[[length(rows) + 1L]] <- row
  }
  if (isTRUE(include_between)) {
    rows <- c(rows, mixed_rm_between_group_summary_row(
      y,
      group,
      time_labels,
      covariate_data,
      method = between_method,
      adjustment = adjustment,
      include_within = include_within_tests,
      include_posthoc = include_posthoc
    ))
  }
  out <- do.call(rbind, rows)
  attr(out, "adjusted") <- adjusted
  attr(out, "time_markers") <- stats::setNames(time_markers, time_labels)
  attr(out, "column_header_markers") <- data.frame(
    column = time_labels,
    marker = time_markers,
    stringsAsFactors = FALSE
  )
  out
}

mixed_rm_summary_note <- function(adjusted = FALSE, covariates = character(0), include_within = TRUE, include_between = FALSE, between_method = "simple", time_marker_note = "") {
  note <- if (isTRUE(adjusted)) {
    "Values are estimated marginal means \u00b1 SE."
  } else {
    "Values are M \u00b1 SD."
  }
  if (isTRUE(include_within) && !isTRUE(adjusted)) {
    note <- paste(note, "Within-group F/p tests time change in each group.")
  }
  if (isTRUE(include_within) && isTRUE(adjusted)) {
    note <- paste(note, "Within-group F/p uses covariate-adjusted time comparisons.")
  }
  if (isTRUE(include_between)) {
    note <- paste(
      note,
      sprintf(
        "Between-groups row uses %s.",
        if (identical(between_method, "estimated")) "estimated marginal mean differences" else "simple mean differences"
      )
    )
  }
  if (nzchar(time_marker_note)) {
    note <- paste(note, time_marker_note)
  }
  note
}

mixed_rm_split_group_summary_columns <- function(table, group_data, group_labels) {
  if (!is.data.frame(table) || nrow(table) == 0 || !"Group" %in% names(table)) return(table)
  if (!is.data.frame(group_data) || ncol(group_data) <= 1L) return(table)
  group_labels <- as.character(group_labels %||% names(group_data))
  if (length(group_labels) < ncol(group_data)) {
    group_labels <- c(group_labels, names(group_data)[seq.int(length(group_labels) + 1L, ncol(group_data))])
  }
  group_labels <- make.unique(group_labels[seq_len(ncol(group_data))], sep = " ")
  combo <- mixed_rm_interaction_group(group_data)
  levels <- levels(combo)
  parts <- strsplit(levels, " / ", fixed = TRUE)
  map <- data.frame(Group = levels, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(group_labels)) {
    map[[group_labels[[index]]]] <- vapply(parts, function(value) if (length(value) >= index) value[[index]] else "", character(1))
  }
  original <- table
  for (label in group_labels) {
    original[[label]] <- ""
  }
  matched <- match(as.character(original$Group), map$Group)
  for (label in group_labels) {
    has_match <- !is.na(matched)
    original[[label]][has_match] <- map[[label]][matched[has_match]]
  }
  group_col <- match("Group", names(original))
  insert <- original[group_labels]
  original[group_labels] <- NULL
  original$Group <- NULL
  before <- original[seq_len(max(0L, group_col - 1L))]
  after <- original[seq.int(group_col, ncol(original))]
  out <- cbind(before, insert, after, stringsAsFactors = FALSE)
  attrs <- attributes(table)
  keep_attrs <- setdiff(names(attrs), c("names", "row.names", "class"))
  for (name in keep_attrs) attr(out, name) <- attrs[[name]]
  out
}

mixed_rm_normality_table <- function(y, group, time_labels) {
  if (!is.matrix(y) && !is.data.frame(y)) return(data.frame())
  y <- as.matrix(y)
  rows <- list()
  group_levels <- levels(group)
  for (index in seq_len(ncol(y))) {
    row <- data.frame(Time = time_labels[[index]], stringsAsFactors = FALSE, check.names = FALSE)
    for (level in group_levels) {
      values <- y[group == level, index]
      p <- if (sum(is.finite(values)) >= 3L && sum(is.finite(values)) <= 5000L && length(unique(values[is.finite(values)])) > 1L) {
        tryCatch(stats::shapiro.test(values)$p.value, error = function(e) NA_real_)
      } else {
        NA_real_
      }
      row[[level]] <- if (is.finite(p)) format_p(p) else ""
    }
    rows[[length(rows) + 1L]] <- row
  }
  out <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  attr(out, "normality_method") <- "Shapiro-Wilk"
  out
}

mixed_rm_levene_p <- function(values, group) {
  values <- as.numeric(values)
  group <- droplevels(as.factor(group))
  keep <- is.finite(values) & !is.na(group)
  values <- values[keep]
  group <- droplevels(group[keep])
  if (length(values) < 3L || nlevels(group) < 2L) return(NA_real_)
  if (any(stats::setNames(tabulate(as.integer(group), nbins = nlevels(group)), levels(group)) < 2L)) return(NA_real_)
  centered <- ave(values, group, FUN = function(x) stats::median(x, na.rm = TRUE))
  test_data <- data.frame(deviation = abs(values - centered), group = group)
  fit <- tryCatch(stats::aov(deviation ~ group, data = test_data), error = function(e) NULL)
  summary_fit <- if (!is.null(fit)) tryCatch(summary(fit)[[1]], error = function(e) NULL) else NULL
  if (is.null(summary_fit) || !"Pr(>F)" %in% colnames(summary_fit) || nrow(summary_fit) < 1L) return(NA_real_)
  suppressWarnings(as.numeric(summary_fit[["Pr(>F)"]][[1]]))
}

mixed_rm_adjusted_residuals_for_time <- function(response, group, covariates = NULL) {
  response <- as.numeric(response)
  covariates <- mixed_rm_reduced_covariates(covariates)
  if (is.null(covariates) || ncol(covariates) == 0) return(response)
  safe_covariates <- covariates
  names(safe_covariates) <- paste0(".cov", seq_len(ncol(safe_covariates)))
  model_data <- data.frame(.response = response, .group = droplevels(as.factor(group)), safe_covariates, check.names = FALSE)
  predictors <- c(".group", names(safe_covariates))
  fit <- tryCatch(
    stats::lm(
      stats::as.formula(paste(".response ~", paste(predictors, collapse = " + "))),
      data = model_data,
      na.action = stats::na.exclude
    ),
    error = function(e) NULL
  )
  residuals <- if (!is.null(fit)) tryCatch(stats::residuals(fit), error = function(e) NULL) else NULL
  if (is.null(residuals) || length(residuals) != length(response)) return(response)
  as.numeric(residuals)
}

mixed_rm_levene_by_time <- function(y, group, covariates = NULL, time_labels = NULL) {
  if (!is.matrix(y) && !is.data.frame(y)) return(data.frame())
  y <- as.matrix(y)
  if (nrow(y) == 0 || ncol(y) == 0) return(data.frame())
  time_labels <- as.character(time_labels %||% colnames(y) %||% paste0("Time ", seq_len(ncol(y))))
  if (length(time_labels) < ncol(y)) {
    time_labels <- c(time_labels, paste0("Time ", seq.int(length(time_labels) + 1L, ncol(y))))
  }
  has_covariates <- !is.null(covariates) && ncol(mixed_rm_reduced_covariates(covariates)) > 0L
  p_values <- vapply(seq_len(ncol(y)), function(index) {
    values <- if (has_covariates) {
      mixed_rm_adjusted_residuals_for_time(y[, index], group, covariates)
    } else {
      y[, index]
    }
    mixed_rm_levene_p(values, group)
  }, numeric(1))
  finite <- is.finite(p_values)
  result <- if (!any(finite)) {
    "Not testable"
  } else if (any(p_values[finite] < .05)) {
    "Potential violation"
  } else {
    "Satisfied"
  }
  p_text <- vapply(p_values, function(p) if (is.finite(p)) format_p(p) else "not testable", character(1))
  detail <- paste(sprintf("%s=%s", time_labels[seq_len(ncol(y))], p_text), collapse = "; ")
  scale_note <- if (has_covariates) {
    "adjusted residuals (group + covariates)."
  } else {
    "raw values by group."
  }
  data.frame(Item = "Levene homogeneity", Result = result, Detail = paste0(detail, "; ", scale_note), stringsAsFactors = FALSE)
}

mixed_rm_assumption_table <- function(y, group, options = list(), covariates = NULL, adjusted_anova = NULL, total_n = nrow(y), excluded_n = 0L, time_labels = NULL) {
  rows <- list(
    data.frame(Item = "Total cases", Result = as.character(total_n), Detail = "", stringsAsFactors = FALSE),
    data.frame(Item = "Excluded cases", Result = as.character(excluded_n), Detail = "Rows with missing values in selected variables are excluded listwise.", stringsAsFactors = FALSE),
    data.frame(Item = "Complete cases", Result = as.character(nrow(y)), Detail = "", stringsAsFactors = FALSE),
    data.frame(Item = "Groups", Result = as.character(nlevels(group)), Detail = paste(levels(group), collapse = ", "), stringsAsFactors = FALSE),
    data.frame(Item = "Time points", Result = as.character(ncol(y)), Detail = "", stringsAsFactors = FALSE)
  )
  if (ncol(y) >= 3L) {
    if (!is.null(adjusted_anova) && !is.null(attr(adjusted_anova, "sphericity", exact = TRUE))) {
      sphericity_table <- attr(adjusted_anova, "sphericity", exact = TRUE)
      p_adjust <- attr(adjusted_anova, "p_adjustments", exact = TRUE)
      time_row <- if (is.data.frame(sphericity_table)) {
        row_names <- rownames(sphericity_table)
        matched <- row_names[row_names %in% c("Time", "(Intercept):Time")]
        if (length(matched) == 0) matched <- row_names[grepl("(^|:)Time$", row_names)]
        if (length(matched) > 0) matched[[1]] else NA_character_
      } else {
        NA_character_
      }
      sphericity <- list(
        w = if (is.data.frame(sphericity_table) && !is.na(time_row)) sphericity_table[time_row, "Test statistic"] else as.numeric(sphericity_table$w %||% NA_real_),
        p = if (is.data.frame(sphericity_table) && !is.na(time_row)) sphericity_table[time_row, "p-value"] else as.numeric(sphericity_table$p %||% NA_real_),
        epsilon = if (is.data.frame(p_adjust) && !is.na(time_row) && time_row %in% rownames(p_adjust) && "GG eps" %in% colnames(p_adjust)) p_adjust[time_row, "GG eps"] else as.numeric(sphericity_table$epsilon %||% NA_real_)
      )
      sphericity$satisfied <- is.finite(sphericity$p) && sphericity$p >= .05
    } else {
      sphericity <- paired_rm_sphericity(y)
    }
    rows <- c(rows, list(
      data.frame(Item = "Sphericity", Result = if (isTRUE(sphericity$satisfied)) "Satisfied" else "Not satisfied", Detail = paste0("W=", format_decimal3(sphericity$w), "; p=", format_p(sphericity$p)), stringsAsFactors = FALSE),
      data.frame(Item = "GG epsilon", Result = format_decimal3(sphericity$epsilon), Detail = "Used for corrected within-subject p values.", stringsAsFactors = FALSE)
    ))
  } else {
    rows <- c(rows, list(
      data.frame(Item = "Sphericity", Result = "Not required", Detail = "Only two repeated measurements were selected.", stringsAsFactors = FALSE)
    ))
  }
  rows <- c(rows, list(mixed_rm_levene_by_time(y, group, covariates, time_labels)))
  if (!is.null(covariates) && ncol(covariates) > 0) {
    rows <- c(rows, list(
      data.frame(Item = "Covariates", Result = as.character(ncol(covariates)), Detail = paste(names(covariates), collapse = ", "), stringsAsFactors = FALSE)
    ))
  }
  do.call(rbind, rows)
}

mixed_rm_pair_label <- function(a, b) {
  paste(a, b, sep = "-")
}

mixed_rm_time_marker_note <- function(time_labels, time_markers) {
  if (length(time_labels) == 0 || length(time_markers) == 0) return("")
  pairs <- sprintf("%s = %s", time_markers[seq_along(time_labels)], time_labels)
  paste0("Time markers: ", paste(pairs, collapse = "; "), ".")
}

mixed_rm_posthoc_table <- function(y, group, time_labels, adjustment = statedu_multiple_correction_default()) {
  adjustment <- as.character(adjustment %||% statedu_multiple_correction_default())
  if (!adjustment %in% c("holm", "bonferroni")) adjustment <- statedu_multiple_correction_default()
  rows <- list()
  group_levels <- levels(group)
  time_markers <- mixed_rm_time_markers(ncol(y))
  time_pairs <- utils::combn(seq_len(ncol(y)), 2, simplify = FALSE)

  p_values <- numeric(length(time_pairs))
  statistics <- rep(NA_real_, length(time_pairs))
  dfs <- rep(NA_real_, length(time_pairs))
  for (pair_index in seq_along(time_pairs)) {
    pair <- time_pairs[[pair_index]]
    test <- tryCatch(stats::t.test(y[, pair[[1]]], y[, pair[[2]]], paired = TRUE), error = function(e) NULL)
    if (!is.null(test)) {
      statistics[[pair_index]] <- unname(test$statistic)
      dfs[[pair_index]] <- unname(test$parameter)
      p_values[[pair_index]] <- test$p.value
    } else {
      p_values[[pair_index]] <- NA_real_
    }
  }
  adjusted <- stats::p.adjust(p_values, method = adjustment)
  for (pair_index in seq_along(time_pairs)) {
    pair <- time_pairs[[pair_index]]
    rows[[length(rows) + 1L]] <- data.frame(
      Family = "Time comparison overall",
      Stratum = "Overall",
      Contrast = mixed_rm_pair_label(time_markers[[pair[[1]]]], time_markers[[pair[[2]]]]),
      Method = "Paired t-test",
      Statistic = format_decimal3(statistics[[pair_index]]),
      df = format_decimal3(dfs[[pair_index]]),
      p = format_p(p_values[[pair_index]]),
      `p adjusted` = format_p(adjusted[[pair_index]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  for (index in seq_len(ncol(y))) {
    values <- y[, index]
    if (length(group_levels) == 2L) {
      test <- tryCatch(stats::t.test(values ~ group), error = function(e) NULL)
      rows[[length(rows) + 1L]] <- data.frame(
        Family = "Group comparison at each time",
        Stratum = time_markers[[index]],
        Contrast = mixed_rm_pair_label(group_levels[[1]], group_levels[[2]]),
        Method = "Independent t-test",
        Statistic = if (!is.null(test)) format_decimal3(unname(test$statistic)) else "",
        df = if (!is.null(test)) format_decimal3(unname(test$parameter)) else "",
        p = if (!is.null(test)) format_p(test$p.value) else "",
        `p adjusted` = if (!is.null(test)) format_p(test$p.value) else "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      pairs <- utils::combn(group_levels, 2, simplify = FALSE)
      p_values <- numeric(length(pairs))
      statistics <- rep(NA_real_, length(pairs))
      dfs <- rep(NA_real_, length(pairs))
      for (pair_index in seq_along(pairs)) {
        pair <- pairs[[pair_index]]
        keep <- group %in% pair
        test <- tryCatch(stats::t.test(values[keep] ~ droplevels(group[keep])), error = function(e) NULL)
        if (!is.null(test)) {
          statistics[[pair_index]] <- unname(test$statistic)
          dfs[[pair_index]] <- unname(test$parameter)
          p_values[[pair_index]] <- test$p.value
        } else {
          p_values[[pair_index]] <- NA_real_
        }
      }
      adjusted <- stats::p.adjust(p_values, method = adjustment)
      for (pair_index in seq_along(pairs)) {
        pair <- pairs[[pair_index]]
        rows[[length(rows) + 1L]] <- data.frame(
          Family = "Group comparison at each time",
          Stratum = time_markers[[index]],
          Contrast = mixed_rm_pair_label(pair[[1]], pair[[2]]),
          Method = "Independent t-test",
          Statistic = format_decimal3(statistics[[pair_index]]),
          df = format_decimal3(dfs[[pair_index]]),
          p = format_p(p_values[[pair_index]]),
          `p adjusted` = format_p(adjusted[[pair_index]]),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
  }
  out <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  if (is.data.frame(out) && nrow(out) > 0) {
    attr(out, "time_markers") <- stats::setNames(time_markers, time_labels)
    attr(out, "overall_time_means") <- stats::setNames(colMeans(y), time_markers)
    attr(out, "group_levels") <- group_levels
    attr(out, "group_means_by_time") <- stats::setNames(lapply(seq_len(ncol(y)), function(index) {
      stats::setNames(vapply(group_levels, function(level) {
        mean(y[group == level, index], na.rm = TRUE)
      }, numeric(1)), group_levels)
    }), time_markers)
    attr(out, "column_display_labels") <- c(
      Family = "Post-hoc family",
      Stratum = "Group / time",
      `p adjusted` = "p\nadjusted"
    )
  }
  out
}

mixed_rm_within_group_posthoc_table <- function(y, group, time_labels, covariate_data = NULL, adjustment = statedu_multiple_correction_default(), adjusted = FALSE) {
  if (!is.matrix(y) && !is.data.frame(y)) return(data.frame())
  y <- as.matrix(y)
  group <- droplevels(group)
  if (nrow(y) < 2 || ncol(y) < 3 || nlevels(group) < 1L) return(data.frame())
  adjustment <- as.character(adjustment %||% statedu_multiple_correction_default())
  if (!adjustment %in% c("holm", "bonferroni")) adjustment <- statedu_multiple_correction_default()
  time_markers <- mixed_rm_time_markers(ncol(y))
  time_pairs <- utils::combn(seq_len(ncol(y)), 2, simplify = FALSE)
  rows <- list()
  for (level in levels(group)) {
    keep <- group == level
    subset <- y[keep, , drop = FALSE]
    subset_covariates <- if (!is.null(covariate_data) && ncol(covariate_data) > 0) covariate_data[keep, , drop = FALSE] else data.frame()
    tests <- lapply(time_pairs, function(pair) {
      if (isTRUE(adjusted)) {
        mixed_rm_adjusted_pair_test(subset[, pair[[2]]] - subset[, pair[[1]]], subset_covariates)
      } else {
        test <- tryCatch(stats::t.test(subset[, pair[[1]]], subset[, pair[[2]]], paired = TRUE), error = function(e) NULL)
        list(
          statistic = if (!is.null(test)) unname(test$statistic) else NA_real_,
          df = if (!is.null(test)) unname(test$parameter) else NA_real_,
          p = if (!is.null(test)) test$p.value else NA_real_
        )
      }
    })
    p_values <- vapply(tests, `[[`, numeric(1), "p")
    adjusted_p <- stats::p.adjust(p_values, method = adjustment)
    for (pair_index in seq_along(time_pairs)) {
      pair <- time_pairs[[pair_index]]
      rows[[length(rows) + 1L]] <- data.frame(
        Family = if (isTRUE(adjusted)) "Adjusted within group" else "Observed within group",
        Stratum = level,
        Contrast = mixed_rm_pair_label(time_markers[[pair[[1]]]], time_markers[[pair[[2]]]]),
        Method = if (isTRUE(adjusted)) "Covariate-adjusted paired difference" else "Paired t-test",
        Statistic = format_decimal3(tests[[pair_index]]$statistic),
        df = format_decimal3(tests[[pair_index]]$df),
        p = format_p(tests[[pair_index]]$p),
        `p adjusted` = format_p(adjusted_p[[pair_index]]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  out <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  if (is.data.frame(out) && nrow(out) > 0) {
    attr(out, "time_markers") <- stats::setNames(time_markers, time_labels)
    attr(out, "column_display_labels") <- c(
      Family = "Post-hoc family",
      Stratum = "Group / time",
      `p adjusted` = "p\nadjusted"
    )
  }
  out
}

mixed_rm_time_posthoc_table <- function(y, time_labels, covariate_data = NULL, adjustment = statedu_multiple_correction_default(), adjusted = FALSE, family = NULL, stratum = "Overall") {
  if (!is.matrix(y) && !is.data.frame(y)) return(data.frame())
  y <- as.matrix(y)
  if (nrow(y) < 2 || ncol(y) < 3) return(data.frame())
  adjustment <- as.character(adjustment %||% statedu_multiple_correction_default())
  if (!adjustment %in% c("holm", "bonferroni")) adjustment <- statedu_multiple_correction_default()
  time_markers <- mixed_rm_time_markers(ncol(y))
  time_pairs <- utils::combn(seq_len(ncol(y)), 2, simplify = FALSE)
  tests <- lapply(time_pairs, function(pair) {
    if (isTRUE(adjusted)) {
      mixed_rm_adjusted_pair_test(y[, pair[[2]]] - y[, pair[[1]]], covariate_data %||% data.frame())
    } else {
      test <- tryCatch(stats::t.test(y[, pair[[1]]], y[, pair[[2]]], paired = TRUE), error = function(e) NULL)
      list(
        statistic = if (!is.null(test)) unname(test$statistic) else NA_real_,
        df = if (!is.null(test)) unname(test$parameter) else NA_real_,
        p = if (!is.null(test)) test$p.value else NA_real_
      )
    }
  })
  p_values <- vapply(tests, `[[`, numeric(1), "p")
  adjusted_p <- stats::p.adjust(p_values, method = adjustment)
  rows <- lapply(seq_along(time_pairs), function(pair_index) {
    pair <- time_pairs[[pair_index]]
    data.frame(
      Family = family %||% if (isTRUE(adjusted)) "Adjusted overall" else "Observed overall",
      Stratum = stratum,
      Contrast = mixed_rm_pair_label(time_markers[[pair[[1]]]], time_markers[[pair[[2]]]]),
      Method = if (isTRUE(adjusted)) "Covariate-adjusted paired difference" else "Paired t-test",
      Statistic = format_decimal3(tests[[pair_index]]$statistic),
      df = format_decimal3(tests[[pair_index]]$df),
      p = format_p(tests[[pair_index]]$p),
      `p adjusted` = format_p(adjusted_p[[pair_index]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  if (is.data.frame(out) && nrow(out) > 0) {
    attr(out, "time_markers") <- stats::setNames(time_markers, time_labels)
    attr(out, "column_display_labels") <- c(
      Family = "Post-hoc family",
      Stratum = "Group / time",
      `p adjusted` = "p\nadjusted"
    )
  }
  out
}

mixed_rm_style_posthoc_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0 || !"Family" %in% names(table)) return(table)
  attr(table, "column_display_labels") <- c(
    Family = "Post-hoc family",
    Stratum = "Group / time",
    `p adjusted` = "p\nadjusted"
  )
  family <- as.character(table$Family)
  previous_family <- c(family[[1]], family[-length(family)])
  separator_rows <- which(family != previous_family)
  separator_rows <- separator_rows[separator_rows > 1L]
  if (length(separator_rows) > 0) {
    attr(table, "cell_styles") <- data.frame(
      row = rep(separator_rows, each = ncol(table)),
      column = rep(names(table), times = length(separator_rows)),
      style = "border-top:2px solid #1f2937 !important;",
      stringsAsFactors = FALSE
    )
  }
  table
}

mixed_rm_parse_p_text <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(NA_real_)
  value <- sub("^<", "", value)
  value <- sub("^\\.", "0.", value)
  suppressWarnings(as.numeric(value))
}

mixed_rm_significant_order_notation <- function(levels, means, rows, alpha = .05) {
  levels <- as.character(levels %||% character(0))
  means <- as.numeric(means[levels])
  names(means) <- levels
  if (length(levels) < 2 || !is.data.frame(rows) || nrow(rows) == 0 || !"Contrast" %in% names(rows) || !"p adjusted" %in% names(rows)) {
    return("")
  }
  sig <- matrix(FALSE, nrow = length(levels), ncol = length(levels), dimnames = list(levels, levels))
  for (i in seq_along(levels)) {
    if (i >= length(levels)) next
    for (j in seq.int(i + 1L, length(levels))) {
      first <- levels[[i]]
      second <- levels[[j]]
      contrasts <- c(mixed_rm_pair_label(first, second), mixed_rm_pair_label(second, first))
      matched <- rows[as.character(rows$Contrast) %in% contrasts, , drop = FALSE]
      if (nrow(matched) == 0) next
      p_value <- mixed_rm_parse_p_text(matched$`p adjusted`[[1]])
      if (!is.finite(p_value) || p_value >= alpha) next
      first_mean <- means[[first]]
      second_mean <- means[[second]]
      higher <- if (is.finite(first_mean) && is.finite(second_mean) && first_mean < second_mean) second else first
      lower <- if (identical(higher, first)) second else first
      sig[higher, lower] <- TRUE
    }
  }
  if (!any(sig)) return("n.s.")
  ordered <- levels[order(-means, levels)]

  # Levels with the same significant relations to every other level are shown as one ordered tier.
  relation_profiles <- vapply(levels, function(level) {
    lower <- sort(levels[sig[level, levels]])
    higher <- sort(levels[sig[levels, level]])
    paste(paste(higher, collapse = ","), paste(lower, collapse = ","), sep = "|")
  }, character(1))
  profile_groups <- split(levels, relation_profiles)
  profile_groups <- lapply(profile_groups, function(group_levels) {
    group_levels[order(-means[group_levels], group_levels)]
  })
  group_order <- vapply(profile_groups, function(group_levels) {
    min(match(group_levels, ordered))
  }, numeric(1))
  profile_groups <- profile_groups[order(group_order)]
  node_labels <- vapply(profile_groups, paste, character(1), collapse = ",")
  node_count <- length(profile_groups)
  node_sig <- matrix(FALSE, nrow = node_count, ncol = node_count, dimnames = list(node_labels, node_labels))
  if (node_count >= 2L) {
    for (i in seq_len(node_count)) {
      for (j in seq_len(node_count)) {
        if (i == j) next
        node_sig[i, j] <- all(sig[profile_groups[[i]], profile_groups[[j]], drop = FALSE])
      }
    }
  }

  covered <- matrix(FALSE, nrow = length(levels), ncol = length(levels), dimnames = list(levels, levels))
  statements <- character(0)
  for (start_index in seq_len(node_count)) {
    chain <- start_index
    current <- start_index
    repeat {
      lower_candidates <- which(seq_len(node_count) %in% setdiff(seq_len(node_count), chain) & node_sig[current, ])
      lower_candidates <- lower_candidates[vapply(lower_candidates, function(candidate) {
        higher_levels <- profile_groups[[current]]
        lower_levels <- profile_groups[[candidate]]
        any(sig[higher_levels, lower_levels, drop = FALSE] & !covered[higher_levels, lower_levels, drop = FALSE])
      }, logical(1))]
      if (length(lower_candidates) == 0) break
      next_value <- lower_candidates[[1]]
      chain <- c(chain, next_value)
      current <- next_value
    }
    if (length(chain) <= 1L) next
    statements <- c(statements, paste(node_labels[chain], collapse = ">"))
    for (i in seq_along(chain)) {
      if (i >= length(chain)) next
      for (j in seq.int(i + 1L, length(chain))) {
        higher_levels <- profile_groups[[chain[[i]]]]
        lower_levels <- profile_groups[[chain[[j]]]]
        covered[higher_levels, lower_levels] <- sig[higher_levels, lower_levels, drop = FALSE]
      }
    }
  }
  for (left in ordered) {
    for (right in ordered) {
      if (!isTRUE(sig[left, right]) || isTRUE(covered[left, right])) next
      statements <- c(statements, sprintf("%s>%s", left, right))
    }
  }
  paste(unique(statements), collapse = "; ")
}

mixed_rm_posthoc_p_label <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return("")
  if (startsWith(value, "<")) paste0("p_adj", value) else paste0("p_adj=", value)
}

mixed_rm_interaction_posthoc_inline <- function(y, group, adjustment = statedu_multiple_correction_default(), max_items = 4L) {
  if (!is.matrix(y) && !is.data.frame(y)) return("")
  y <- as.matrix(y)
  group <- droplevels(group)
  if (ncol(y) < 3L || nlevels(group) < 2L) return("")
  adjustment <- as.character(adjustment %||% statedu_multiple_correction_default())
  if (!adjustment %in% c("holm", "bonferroni")) adjustment <- statedu_multiple_correction_default()
  time_markers <- mixed_rm_time_markers(ncol(y))
  time_pairs <- utils::combn(seq_len(ncol(y)), 2, simplify = FALSE)
  group_levels <- levels(group)
  labels <- character(0)
  if (length(group_levels) == 2L) {
    p_values <- vapply(time_pairs, function(pair) {
      change <- y[, pair[[2]]] - y[, pair[[1]]]
      test <- tryCatch(stats::t.test(change ~ group), error = function(e) NULL)
      if (!is.null(test)) test$p.value else NA_real_
    }, numeric(1))
    adjusted <- stats::p.adjust(p_values, method = adjustment)
    for (pair_index in seq_along(time_pairs)) {
      p_value <- adjusted[[pair_index]]
      if (!is.finite(p_value) || p_value >= .05) next
      pair <- time_pairs[[pair_index]]
      change <- y[, pair[[2]]] - y[, pair[[1]]]
      means <- stats::setNames(vapply(group_levels, function(level) mean(change[group == level], na.rm = TRUE), numeric(1)), group_levels)
      higher <- if (means[[group_levels[[1]]]] >= means[[group_levels[[2]]]]) group_levels[[1]] else group_levels[[2]]
      lower <- if (identical(higher, group_levels[[1]])) group_levels[[2]] else group_levels[[1]]
      labels <- c(labels, sprintf("%s-%s: %s>%s", time_markers[[pair[[2]]]], time_markers[[pair[[1]]]], higher, lower))
    }
  } else {
    for (pair in time_pairs) {
      change <- y[, pair[[2]]] - y[, pair[[1]]]
      pairs <- utils::combn(group_levels, 2, simplify = FALSE)
      p_values <- vapply(pairs, function(group_pair) {
        keep <- group %in% group_pair
        test <- tryCatch(stats::t.test(change[keep] ~ droplevels(group[keep])), error = function(e) NULL)
        if (!is.null(test)) test$p.value else NA_real_
      }, numeric(1))
      adjusted <- stats::p.adjust(p_values, method = adjustment)
      rows <- data.frame(
        Contrast = vapply(pairs, function(group_pair) mixed_rm_pair_label(group_pair[[1]], group_pair[[2]]), character(1)),
        `p adjusted` = vapply(adjusted, format_p, character(1)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      means <- stats::setNames(vapply(group_levels, function(level) mean(change[group == level], na.rm = TRUE), numeric(1)), group_levels)
      notation <- mixed_rm_significant_order_notation(group_levels, means, rows)
      if (nzchar(notation) && !identical(notation, "n.s.")) {
        labels <- c(labels, sprintf("%s-%s: %s", time_markers[[pair[[2]]]], time_markers[[pair[[1]]]], notation))
      }
    }
  }
  if (length(labels) == 0) return("n.s.")
  if (length(labels) > max_items) {
    labels <- c(labels[seq_len(max_items)], sprintf("+%d more", length(labels) - max_items))
  }
  paste(labels, collapse = "; ")
}

mixed_rm_posthoc_inline <- function(posthoc, family, max_items = 4L) {
  if (!is.data.frame(posthoc) || nrow(posthoc) == 0 || !"Family" %in% names(posthoc)) {
    return("")
  }
  rows <- posthoc[posthoc$Family == family, , drop = FALSE]
  if (nrow(rows) == 0) return("")
  if (identical(family, "Time comparison overall")) {
    time_markers <- unname(attr(posthoc, "time_markers", exact = TRUE) %||% character(0))
    means <- attr(posthoc, "overall_time_means", exact = TRUE)
    return(mixed_rm_significant_order_notation(time_markers, means, rows))
  }
  if (identical(family, "Group comparison at each time")) {
    group_levels <- attr(posthoc, "group_levels", exact = TRUE) %||% character(0)
    group_means <- attr(posthoc, "group_means_by_time", exact = TRUE) %||% list()
    labels <- character(0)
    for (stratum in unique(as.character(rows$Stratum))) {
      stratum_rows <- rows[as.character(rows$Stratum) == stratum, , drop = FALSE]
      notation <- mixed_rm_significant_order_notation(group_levels, group_means[[stratum]], stratum_rows)
      if (nzchar(notation) && !identical(notation, "n.s.")) {
        labels <- c(labels, sprintf("Time %s: %s", stratum, notation))
      }
    }
    if (length(labels) == 0) return("n.s.")
  } else {
    return("n.s.")
  }
  if (length(labels) > max_items) {
    labels <- c(labels[seq_len(max_items)], sprintf("+%d more", length(labels) - max_items))
  }
  paste(labels, collapse = "; ")
}

mixed_rm_finalize_anova_table <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) return(table)
  if ("partial eta2" %in% names(table)) {
    names(table)[names(table) == "partial eta2"] <- "ES"
  }
  display_labels <- c(
    `epsilon(GG)` = "epsilon\n(GG)",
    `epsilon(HF)` = "epsilon\n(HF)"
  )
  attr(table, "column_display_labels") <- display_labels
  if ("ES" %in% names(table)) {
    es_rows <- which(nzchar(as.character(table$ES %||% "")))
    if (length(es_rows) > 0) {
      attr(table, "note_markers") <- data.frame(
        row = es_rows,
        column = "ES",
        marker = "1",
        stringsAsFactors = FALSE
      )
    }
  }
  table
}

mixed_rm_primary_effect_label <- function(group_labels) {
  group_labels <- as.character(group_labels %||% character(0))
  if (length(group_labels) <= 1L) return("Group x Time")
  paste(c(group_labels, "Time"), collapse = " x ")
}

mixed_rm_resolve_analysis_population <- function(value) {
  value <- as.character(value %||% "pp")[[1]]
  if (identical(value, "itt")) "itt" else "pp"
}

mixed_rm_analysis_population_label <- function(value) {
  switch(
    mixed_rm_resolve_analysis_population(value),
    itt = "ITT / available repeated-measures mixed model",
    "PP / complete-case repeated-measures ANOVA"
  )
}

mixed_rm_overview_table <- function(group_variable, repeated_variables, y, group, time_labels, variable_info = NULL, labels = character(0), category_table = NULL, covariates = character(0), analysis_population = "pp") {
  group_variable <- as.character(group_variable %||% character(0))
  group_labels <- if (length(group_variable) > 0) {
    vapply(group_variable, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  } else {
    character(0)
  }
  repeated_labels <- vapply(repeated_variables, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  covariate_labels <- if (length(covariates) > 0) {
    vapply(covariates, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  } else {
    character(0)
  }
  analysis_label <- if (length(covariates) > 0 && length(group_variable) > 1L) {
    "Covariate-adjusted factorial mixed repeated-measures ANOVA"
  } else if (length(covariates) > 0) {
    "Covariate-adjusted mixed repeated-measures ANOVA"
  } else if (length(group_variable) > 1L) {
    "Factorial mixed repeated-measures ANOVA"
  } else {
    "Mixed repeated-measures ANOVA"
  }
  data.frame(
    Item = c("Analysis", "Analysis population", "Primary effect", "Repeated-measures variables", "Independent variables", "Covariates", "Time labels", "Complete N", "Groups", "Time points"),
    Value = c(
      analysis_label,
      mixed_rm_analysis_population_label(analysis_population),
      paste0(mixed_rm_primary_effect_label(group_labels), " interaction"),
      paste(repeated_labels, collapse = ", "),
      paste(group_labels, collapse = ", "),
      if (length(covariate_labels) > 0) paste(covariate_labels, collapse = ", ") else "None",
      paste(time_labels, collapse = ", "),
      as.character(nrow(y)),
      paste(sprintf("%s (n=%d)", levels(group), as.numeric(table(group)[levels(group)])), collapse = ", "),
      as.character(ncol(y))
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mixed_rm_itt_available_counts <- function(data, group_variable, repeated_variables, covariates = character(0), covariate_measurements = character(0)) {
  y <- as.data.frame(data[repeated_variables], stringsAsFactors = FALSE)
  y[] <- lapply(y, paired_numeric)
  group_data <- data.frame(row.names = seq_len(nrow(data)))
  for (name in group_variable) {
    group_data[[name]] <- mixed_rm_group_factor(data[[name]])
  }
  covariate_data <- data.frame(row.names = seq_len(nrow(data)))
  for (name in covariates) {
    measurement <- named_value(covariate_measurements, name, "continuous")
    covariate_data[[name]] <- if (measurement %in% c("binary", "category")) {
      mixed_rm_group_factor(data[[name]])
    } else {
      paired_numeric(data[[name]])
    }
  }
  complete_model_vars <- stats::complete.cases(group_data)
  if (ncol(covariate_data) > 0) complete_model_vars <- complete_model_vars & stats::complete.cases(covariate_data)
  has_outcome <- rowSums(!is.na(y)) > 0
  keep <- complete_model_vars & has_outcome
  group <- if (any(keep)) mixed_rm_interaction_group(group_data[keep, , drop = FALSE]) else factor()
  list(
    total_n = nrow(data),
    available_subjects = sum(keep),
    available_records = sum(!is.na(as.matrix(y[keep, , drop = FALSE]))),
    groups = if (length(group) > 0) paste(sprintf("%s (n=%d)", levels(group), as.numeric(table(group)[levels(group)])), collapse = ", ") else ""
  )
}

mixed_rm_itt_overview_table <- function(data, group_variable, repeated_variables, time_labels, variable_info = NULL, labels = character(0), category_table = NULL, covariates = character(0), covariate_measurements = character(0)) {
  group_labels <- vapply(group_variable, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  repeated_labels <- vapply(repeated_variables, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  covariate_labels <- if (length(covariates) > 0) vapply(covariates, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table) else character(0)
  counts <- mixed_rm_itt_available_counts(data, group_variable, repeated_variables, covariates, covariate_measurements)
  data.frame(
    Item = c("Analysis", "Analysis population", "Primary effect", "Repeated-measures variables", "Independent variables", "Covariates", "Time labels", "Available subjects", "Available repeated records", "Groups", "Time points"),
    Value = c(
      "Available-record repeated-measures mixed model",
      mixed_rm_analysis_population_label("itt"),
      paste0(mixed_rm_primary_effect_label(group_labels), " interaction"),
      paste(repeated_labels, collapse = ", "),
      paste(group_labels, collapse = ", "),
      if (length(covariate_labels) > 0) paste(covariate_labels, collapse = ", ") else "None",
      paste(time_labels, collapse = ", "),
      as.character(counts$available_subjects),
      as.character(counts$available_records),
      counts$groups,
      as.character(length(repeated_variables))
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mixed_rm_itt_assumption_table <- function(data, group_variable, repeated_variables, covariates = character(0), covariate_measurements = character(0), frame_error = "") {
  counts <- mixed_rm_itt_available_counts(data, group_variable, repeated_variables, covariates, covariate_measurements)
  data.frame(
    Item = c("Total cases", "Available subjects", "Available repeated records", "Complete-case RM ANOVA"),
    Result = c(as.character(counts$total_n), as.character(counts$available_subjects), as.character(counts$available_records), "Not available"),
    Detail = c("", "Subjects with complete group/covariate values and at least one observed repeated outcome.", "Long-format observed outcome rows used by the ITT mixed-model path.", frame_error),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mixed_rm_itt_only_recommendation_table <- function(group_labels, covariates = character(0), covariate_labels = character(0), frame_error = "", outcome_decision = NULL, mixed_alternative = NULL) {
  outcome_decision <- outcome_decision %||% list(label = "mixed model", reason = "")
  mixed_fitted <- is.list(mixed_alternative) && is.data.frame(mixed_alternative$coefficients) && nrow(mixed_alternative$coefficients) > 0
  data.frame(
    Item = c("Recommended model", "Analysis population", "Primary effect", "PP RM ANOVA status", "Mixed-model decision"),
    Recommendation = c(
      if (length(covariates) > 0) "Use a covariate-adjusted available-record mixed model." else "Use an available-record mixed model.",
      mixed_rm_analysis_population_label("itt"),
      paste0(mixed_rm_primary_effect_label(group_labels), " interaction"),
      "Complete-case RM ANOVA was not produced.",
      if (mixed_fitted) paste0("Use the fitted ", outcome_decision$label, " as the ITT result.") else paste0("Use an ", outcome_decision$label, " path for ITT; automatic fitting was not available.")
    ),
    Reason = c(
      "Complete-case RM ANOVA could not be computed for the selected variables.",
      "ITT keeps available repeated records through a mixed-model path.",
      if (length(group_labels) > 1L) "More than one independent variable was selected." else "One independent variable was selected.",
      frame_error,
      if (mixed_fitted) "A long-format subject-random-intercept model was fitted from the RM selections." else outcome_decision$reason
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mixed_rm_parse_p <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(NA_real_)
  if (startsWith(value, "<")) return(suppressWarnings(as.numeric(sub("^<", "", value))))
  suppressWarnings(as.numeric(value))
}

mixed_rm_p_is_significant <- function(value, alpha = .05) {
  if (length(value) == 0 || is.null(value[[1]])) return(FALSE)
  value <- if (is.numeric(value)) suppressWarnings(as.numeric(value[[1]])) else mixed_rm_parse_p(value)
  is.finite(value) && value < alpha
}

mixed_rm_anova_effect_is_significant <- function(anova, effect, alpha = .05) {
  if (!is.data.frame(anova) || !"Effect" %in% names(anova) || !"p" %in% names(anova)) return(FALSE)
  row <- anova[as.character(anova$Effect) == effect, , drop = FALSE]
  if (nrow(row) == 0) return(FALSE)
  mixed_rm_p_is_significant(row$p[[1]], alpha)
}

mixed_rm_normality_issue <- function(normality) {
  if (!is.data.frame(normality) || nrow(normality) == 0) return(FALSE)
  values <- unlist(normality[setdiff(names(normality), "Time")], use.names = FALSE)
  p <- vapply(values, mixed_rm_parse_p, numeric(1))
  any(is.finite(p) & p < .05)
}

mixed_rm_outcome_family_recommendation <- function(data, repeated_variables, repeated_measurements = NULL) {
  repeated_measurements <- as.character(repeated_measurements %||% character(0))
  if (any(repeated_measurements %in% c("ordered", "ordinal"))) {
    return(list(model_type = "ordinal", family = "ordinal", label = "ordinal mixed model", fit = FALSE, reason = "At least one repeated-measures variable is ordinal."))
  }
  values <- suppressWarnings(as.numeric(unlist(data[repeated_variables], use.names = FALSE)))
  values <- values[is.finite(values)]
  if (exists("longitudinal_count_outcome_candidate", mode = "function") && longitudinal_count_outcome_candidate(values)) {
    return(list(model_type = "glmm", family = "count", label = "count GLMM", fit = TRUE, reason = "The stacked repeated outcome is non-negative integer-like count data."))
  }
  if (exists("longitudinal_gamma_outcome_candidate", mode = "function") && longitudinal_gamma_outcome_candidate(values)) {
    return(list(model_type = "glmm", family = "gamma", label = "Gamma GLMM", fit = TRUE, reason = "The stacked repeated outcome is positive and strongly right-skewed."))
  }
  list(model_type = "lmm", family = "gaussian", label = "LMM", fit = TRUE, reason = "The repeated outcome is treated as continuous Gaussian; mixed modeling handles unbalanced repeated records.")
}

mixed_rm_long_data <- function(data, group_variable, repeated_variables, covariates, time_labels, outcome_measurement = "continuous") {
  n <- nrow(data)
  k <- length(repeated_variables)
  model_time_labels <- make.unique(as.character(time_labels), sep = "_")
  long <- data.frame(
    .statedu_rm_subject_id = factor(rep(seq_len(n), times = k)),
    .statedu_rm_time = factor(rep(model_time_labels, each = n), levels = model_time_labels),
    .statedu_rm_outcome = suppressWarnings(as.numeric(unlist(data[repeated_variables], use.names = FALSE))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  copied <- unique(c(group_variable, covariates))
  for (name in copied) {
    long[[name]] <- rep(data[[name]], times = k)
  }
  long
}

mixed_rm_fixed_formula <- function(outcome, time, group_variable, covariates, id = NULL, include_covariate_time = FALSE) {
  time_term <- longitudinal_formula_variable(time)
  group_variable <- as.character(group_variable %||% character(0))
  covariates <- as.character(covariates %||% character(0))
  group_term <- if (length(group_variable) == 0) {
    "1"
  } else if (length(group_variable) == 1L) {
    longitudinal_formula_variable(group_variable[[1]])
  } else {
    paste(vapply(group_variable, longitudinal_formula_variable, character(1)), collapse = " * ")
  }
  fixed <- paste0(time_term, " * (", group_term, ")")
  if (length(covariates) > 0) {
    covariate_terms <- vapply(covariates, longitudinal_formula_variable, character(1))
    covariate_time_terms <- if (isTRUE(include_covariate_time)) paste0(time_term, ":", covariate_terms) else character(0)
    fixed <- paste(c(fixed, covariate_terms, covariate_time_terms), collapse = " + ")
  }
  random <- if (length(id) == 1 && nzchar(id)) sprintf(" + (1 | %s)", longitudinal_formula_variable(id)) else ""
  stats::as.formula(sprintf("%s ~ %s%s", longitudinal_formula_variable(outcome), fixed, random))
}

mixed_rm_mixed_reference_summary <- function(time_labels = character(0), model_time_labels = character(0), factor_maps = list(), variable_labels = character(0)) {
  time_labels <- as.character(time_labels %||% character(0))
  model_time_labels <- as.character(model_time_labels %||% character(0))
  time_reference <- if (length(time_labels) > 0) time_labels[[1]] else if (length(model_time_labels) > 0) model_time_labels[[1]] else ""
  parts <- if (nzchar(time_reference)) sprintf("Time = %s", time_reference) else character(0)
  for (name in names(factor_maps)) {
    levels <- factor_maps[[name]]
    if (length(levels) == 0) next
    parts <- c(parts, sprintf("%s = %s", named_value(variable_labels, name, name), levels[[1]]))
  }
  if (length(parts) == 0) "" else paste(parts, collapse = "; ")
}

mixed_rm_mixed_term_component_label <- function(component, time_reference, time_display, factor_maps, variable_labels) {
  component <- as.character(component %||% "")[[1]]
  if (identical(component, "(Intercept)")) {
    return("Intercept (reference cell)")
  }
  time_prefix <- ".statedu_rm_time"
  if (startsWith(component, time_prefix)) {
    level <- substring(component, nchar(time_prefix) + 1L)
    label <- named_value(time_display, level, level)
    return(sprintf("Time: %s vs %s", label, time_reference))
  }
  factor_names <- names(factor_maps)
  if (length(factor_names) > 0) {
    factor_names <- factor_names[order(nchar(factor_names), decreasing = TRUE)]
    for (name in factor_names) {
      if (!startsWith(component, name)) next
      level <- substring(component, nchar(name) + 1L)
      level_map <- factor_maps[[name]]
      if (!nzchar(level) || !level %in% names(level_map)) next
      variable_label <- named_value(variable_labels, name, name)
      return(sprintf("%s: %s vs %s", variable_label, level_map[[level]], level_map[[1]]))
    }
  }
  named_value(variable_labels, component, component)
}

mixed_rm_mixed_term_label <- function(term, time_reference, time_display, factor_maps, variable_labels) {
  term <- as.character(term %||% "")[[1]]
  if (!nzchar(term)) return(term)
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  labels <- vapply(parts, mixed_rm_mixed_term_component_label, character(1), time_reference = time_reference, time_display = time_display, factor_maps = factor_maps, variable_labels = variable_labels)
  paste(labels, collapse = " x ")
}

mixed_rm_format_mixed_coef_table <- function(table, time_labels = character(0), model_time_labels = character(0), factor_maps = list(), variable_labels = character(0)) {
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  out <- table
  if ("Term" %in% names(out)) {
    model_time_labels <- as.character(model_time_labels %||% character(0))
    time_labels <- as.character(time_labels %||% model_time_labels)
    if (length(time_labels) < length(model_time_labels)) {
      time_labels <- c(time_labels, model_time_labels[seq.int(length(time_labels) + 1L, length(model_time_labels))])
    }
    time_display <- stats::setNames(time_labels[seq_along(model_time_labels)], model_time_labels)
    time_reference <- if (length(model_time_labels) > 0) time_display[[model_time_labels[[1]]]] else "reference time"
    out$Term <- vapply(out$Term, mixed_rm_mixed_term_label, character(1), time_reference = time_reference, time_display = time_display, factor_maps = factor_maps, variable_labels = variable_labels)
  }
  numeric_cols <- intersect(c("B", "SE", "Statistic", "LLCI", "ULCI", "exp(B)", "exp(LLCI)", "exp(ULCI)"), names(out))
  for (col in numeric_cols) {
    values <- suppressWarnings(as.numeric(out[[col]]))
    values[is.finite(values) & abs(values) < 0.5 * 10 ^ (-statedu_output_decimal_digits())] <- 0
    out[[col]] <- vapply(values, format_decimal3, character(1))
  }
  if ("p" %in% names(out)) {
    out$p <- vapply(out$p, format_p, character(1))
  }
  out
}

mixed_rm_fit_mixed_alternative <- function(data, group_variable, repeated_variables, covariates, time_labels, variable_info = NULL, labels = character(0), category_table = NULL, repeated_measurements = NULL, analysis_population = "pp", normality_issue = FALSE) {
  family_decision <- mixed_rm_outcome_family_recommendation(data, repeated_variables, repeated_measurements)
  should_fit <- identical(mixed_rm_resolve_analysis_population(analysis_population), "itt")
  if (!isTRUE(should_fit)) {
    return(list(decision = family_decision, overview = data.frame(), coefficients = data.frame(), note = "Mixed-model alternative was not fitted because PP / complete-case RM ANOVA was selected."))
  }
  if (!isTRUE(family_decision$fit)) {
    return(list(
      decision = family_decision,
      overview = data.frame(
        Item = c("Recommended alternative", "Status", "Reason"),
        Value = c(family_decision$label, "Not fitted", family_decision$reason),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      coefficients = data.frame(),
      note = "Ordinal mixed model is recommended, but automatic ordinal mixed-model fitting is not implemented in this RM module."
    ))
  }
  if (identical(family_decision$model_type, "lmm") && !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("LMM alternative requires the lmerTest package.")
  }
  if (identical(family_decision$model_type, "glmm") && !requireNamespace("lme4", quietly = TRUE)) {
    stop("GLMM alternative requires the lme4 package.")
  }
  long <- mixed_rm_long_data(data, group_variable, repeated_variables, covariates, time_labels, repeated_measurements[[1]] %||% "continuous")
  model_time_labels <- levels(long$.statedu_rm_time)
  variables <- unique(c(".statedu_rm_subject_id", ".statedu_rm_time", ".statedu_rm_outcome", group_variable, covariates))
  keep <- stats::complete.cases(long[, variables, drop = FALSE])
  analyzed <- long[keep, , drop = FALSE]
  analyzed$.statedu_rm_subject_id <- factor(analyzed$.statedu_rm_subject_id)
  analyzed$.statedu_rm_time <- factor(analyzed$.statedu_rm_time, levels = model_time_labels)
  for (name in group_variable) analyzed[[name]] <- mixed_rm_group_factor(analyzed[[name]])
  covariate_measurements <- mixed_rm_measurement_lookup(variable_info)
  for (name in covariates) {
    measurement <- named_value(covariate_measurements, name, "")
    if (measurement %in% c("binary", "category") || is.factor(analyzed[[name]]) || is.character(analyzed[[name]])) {
      analyzed[[name]] <- mixed_rm_group_factor(analyzed[[name]])
    }
  }
  factor_variables <- unique(c(group_variable, covariates[vapply(covariates, function(name) is.factor(analyzed[[name]]), logical(1))]))
  factor_maps <- lapply(factor_variables, function(name) {
    stats::setNames(levels(droplevels(analyzed[[name]])), levels(droplevels(analyzed[[name]])))
  })
  names(factor_maps) <- factor_variables
  variable_labels <- stats::setNames(
    vapply(unique(c(group_variable, covariates)), paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    unique(c(group_variable, covariates))
  )
  reference_summary <- mixed_rm_mixed_reference_summary(time_labels, model_time_labels, factor_maps, variable_labels)
  include_covariate_time <- length(covariates) > 0
  formula <- mixed_rm_fixed_formula(".statedu_rm_outcome", ".statedu_rm_time", group_variable, covariates, ".statedu_rm_subject_id", include_covariate_time = include_covariate_time)
  model_type <- family_decision$model_type
  model_family <- family_decision$family
  fit <- if (identical(model_type, "lmm")) {
    lmerTest::lmer(formula, data = analyzed, REML = FALSE)
  } else if (identical(model_family, "count")) {
    fixed_formula <- mixed_rm_fixed_formula(".statedu_rm_outcome", ".statedu_rm_time", group_variable, covariates, include_covariate_time = include_covariate_time)
    count_selection <- longitudinal_count_family_selection(
      analyzed,
      fixed_formula,
      "glmm",
      ".statedu_rm_subject_id",
      ".statedu_rm_time"
    )
    model_family <- count_selection$family
    if (identical(model_family, "negative_binomial")) {
      lme4::glmer.nb(formula, data = analyzed, control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    } else {
      lme4::glmer(formula, data = analyzed, family = stats::poisson(link = "log"), control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    }
  } else {
    lme4::glmer(formula, data = analyzed, family = longitudinal_family_object(model_family), control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
  }
  coef_table <- if (identical(model_type, "lmm")) longitudinal_lmm_coef_table(fit) else longitudinal_glmm_coef_table(fit, exponentiate = model_family %in% longitudinal_log_ratio_families())
  overview <- data.frame(
    Item = c("Recommended alternative", "Analysis population", "Family", "Reference levels", "Analyzed rows", "Subjects", "Formula", "Reason"),
    Value = c(
      if (identical(model_type, "lmm")) "Linear mixed model" else sprintf("Generalized linear mixed model (%s)", model_family),
      mixed_rm_analysis_population_label(analysis_population),
      model_family,
      reference_summary,
      as.character(nrow(analyzed)),
      as.character(length(unique(analyzed$.statedu_rm_subject_id))),
      paste(deparse(formula), collapse = " "),
      paste(if (isTRUE(normality_issue)) paste(family_decision$reason, "Normality was flagged in at least one RM cell.") else family_decision$reason, if (isTRUE(include_covariate_time)) "Covariate-by-time terms were included because covariates were selected." else "")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(
    decision = modifyList(family_decision, list(family = model_family)),
    overview = overview,
    coefficients = mixed_rm_format_mixed_coef_table(coef_table, time_labels, model_time_labels, factor_maps, variable_labels),
    note = paste(
      "Coefficients are treatment-coded contrasts against the reference levels shown above.",
      "The mixed-model alternative uses long-format observed repeated records with a subject random intercept.",
      if (isTRUE(include_covariate_time)) "Covariate-by-time coefficients check whether covariate effects vary across repeated time points." else "",
      "It is the recommended ITT path when missing repeated outcomes make complete-case RM ANOVA a PP analysis."
    )
  )
}

mixed_rm_effect_line <- function(row) {
  if (!is.data.frame(row) || nrow(row) == 0) return("")
  parts <- c(
    if ("F" %in% names(row)) paste0("F=", row$F[[1]]) else "",
    if ("p" %in% names(row)) paste0("p=", row$p[[1]]) else "",
    if ("ES" %in% names(row) && nzchar(as.character(row$ES[[1]]))) paste0("ES=", row$ES[[1]]) else ""
  )
  paste(parts[nzchar(parts)], collapse = ", ")
}

mixed_rm_covariate_time_rows <- function(anova, covariate_labels = character(0)) {
  if (!is.data.frame(anova) || nrow(anova) == 0 || length(covariate_labels) == 0 || !"Effect" %in% names(anova) || !"p" %in% names(anova)) {
    return(data.frame())
  }
  targets <- paste0(as.character(covariate_labels), " x Time")
  anova[as.character(anova$Effect) %in% targets, , drop = FALSE]
}

mixed_rm_covariate_time_issue <- function(anova, covariate_labels = character(0), alpha = .05) {
  rows <- mixed_rm_covariate_time_rows(anova, covariate_labels)
  if (nrow(rows) == 0) return(FALSE)
  p_values <- vapply(rows$p, mixed_rm_parse_p, numeric(1))
  any(is.finite(p_values) & p_values < alpha)
}

mixed_rm_covariate_time_recommendation <- function(anova, covariate_labels = character(0)) {
  rows <- mixed_rm_covariate_time_rows(anova, covariate_labels)
  if (nrow(rows) == 0) return(NULL)
  p_values <- vapply(rows$p, mixed_rm_parse_p, numeric(1))
  significant <- is.finite(p_values) & p_values < .05
  data.frame(
    Item = "Covariate x Time",
    Recommendation = if (any(significant)) {
      "Covariate effects vary over time; interpret adjusted means and Time effects with caution."
    } else {
      "No covariate-by-time effect was flagged in the fitted RM ANCOVA table."
    },
    Reason = paste(sprintf("%s p=%s", rows$Effect, rows$p), collapse = "; "),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mixed_rm_recommendation_table <- function(anova, assumption, normality, group_labels, covariates = character(0), covariate_labels = character(0), excluded_n = 0L, analysis_population = "pp", outcome_decision = NULL, mixed_alternative = NULL) {
  if (!is.data.frame(anova) || nrow(anova) == 0) return(data.frame())
  group_labels <- as.character(group_labels %||% character(0))
  covariate_labels <- as.character(covariate_labels %||% covariates %||% character(0))
  primary_effect <- mixed_rm_primary_effect_label(group_labels)
  primary_row <- anova[anova$Effect == primary_effect, , drop = FALSE]
  if (nrow(primary_row) == 0 && length(group_labels) <= 1L) {
    primary_row <- anova[anova$Effect == "Group x Time", , drop = FALSE]
  }
  if (nrow(primary_row) == 0) {
    time_interactions <- anova[grepl(" x Time$", anova$Effect), , drop = FALSE]
    if (nrow(time_interactions) > 0) primary_row <- time_interactions[nrow(time_interactions), , drop = FALSE]
  }
  primary_p <- if (nrow(primary_row) > 0 && "p" %in% names(primary_row)) mixed_rm_parse_p(primary_row$p[[1]]) else NA_real_
  primary_detail <- if (nrow(primary_row) > 0) mixed_rm_effect_line(primary_row) else "No matching time interaction was returned by the model."
  model_label <- if (length(covariates) > 0 && length(group_labels) > 1L) {
    "Use a covariate-adjusted factorial repeated-measures model."
  } else if (length(covariates) > 0) {
    "Use a covariate-adjusted mixed repeated-measures model."
  } else if (length(group_labels) > 1L) {
    "Use factorial mixed repeated-measures ANOVA."
  } else {
    "Use mixed repeated-measures ANOVA."
  }
  correction_rows <- anova[anova$Effect %in% c(primary_effect, "Time", "Group x Time"), , drop = FALSE]
  correction_value <- ""
  if (nrow(correction_rows) > 0 && "Correction" %in% names(correction_rows)) {
    correction_value <- correction_rows$Correction[nzchar(as.character(correction_rows$Correction))]
    correction_value <- if (length(correction_value) > 0) correction_value[[1]] else ""
  }
  assumption_available <- is.data.frame(assumption) && nrow(assumption) > 0
  sphericity_row <- if (assumption_available) assumption[assumption$Item %in% c("Mauchly sphericity", "Sphericity"), , drop = FALSE] else data.frame()
  assumption_detail <- if (nrow(sphericity_row) > 0) {
    paste0("Sphericity: ", sphericity_row$Result[[1]], " (", gsub("; ", ", ", sphericity_row$Detail[[1]], fixed = TRUE), ").")
  } else if (!isTRUE(assumption_available)) {
    "Assumption review was not requested."
  } else {
    "Assumption review was not available."
  }
  if (nzchar(correction_value)) {
    assumption_detail <- paste(assumption_detail, paste0("p column: ", correction_value, "."))
  }
  levene_row <- if (assumption_available) assumption[assumption$Item %in% c("Levene homogeneity", "Levene homogeneity by time"), , drop = FALSE] else data.frame()
  levene_flag <- nrow(levene_row) > 0 && identical(as.character(levene_row$Result[[1]]), "Potential violation")
  if (nrow(levene_row) > 0) {
    assumption_detail <- paste(
      assumption_detail,
      paste0("Levene: ", tolower(levene_row$Result[[1]]), " (", levene_row$Detail[[1]], ")")
    )
  }
  normality_flag <- mixed_rm_normality_issue(normality)
  normality_detail <- if (is.data.frame(normality) && nrow(normality) > 0) {
    if (isTRUE(normality_flag)) "At least one Shapiro-Wilk p < .05; treat this as a sensitivity-analysis cue." else "Cell-level Shapiro-Wilk checks did not flag p < .05."
  } else {
    "Assumption review was not requested."
  }
  outcome_decision <- outcome_decision %||% list(model_type = "lmm", family = "gaussian", label = "LMM", reason = "Continuous repeated outcome.")
  population_key <- mixed_rm_resolve_analysis_population(analysis_population)
  mixed_fitted <- is.list(mixed_alternative) && is.data.frame(mixed_alternative$coefficients) && nrow(mixed_alternative$coefficients) > 0
  alternative_recommendation <- if (identical(population_key, "itt")) {
    if (mixed_fitted) {
      paste0("Use the fitted ", outcome_decision$label, " as the ITT mixed-model result.")
    } else {
      paste0("Use an ", outcome_decision$label, " path for ITT; automatic fitting was not available.")
    }
  } else if (isTRUE(normality_flag)) {
    if (identical(outcome_decision$model_type, "glmm")) {
      paste0("Consider ", outcome_decision$label, " as the main or sensitivity analysis.")
    } else if (identical(outcome_decision$model_type, "ordinal")) {
      "Consider ordinal mixed model as the main or sensitivity analysis."
    } else {
      "Consider LMM with robust/bootstrap inference as a sensitivity analysis."
    }
  } else {
    "Mixed-model alternative is not required by the current checks."
  }
  primary_recommendation <- if (is.finite(primary_p) && primary_p < .05) {
    "Prioritize follow-up comparisons for this interaction."
  } else if (is.finite(primary_p)) {
    "Do not treat the group-specific change pattern as supported; review Time and between-subject effects as secondary."
  } else {
    "Review the returned ANOVA table and assumptions before interpretation."
  }
  normality_reason <- if (is.data.frame(normality) && nrow(normality) > 0) {
    paste(normality_detail, outcome_decision$reason)
  } else {
    normality_detail
  }
  assumption_recommendation_parts <- c(
    if (!isTRUE(assumption_available)) {
      "Assumption review was not requested."
    } else if (grepl("Not required", assumption_detail, fixed = TRUE)) {
      "Sphericity correction is not required."
    } else {
      "Use the correction decision shown in the ANOVA table."
    },
    if (isTRUE(levene_flag)) "Between-group homogeneity was flagged; interpret between-subject effects with caution." else ""
  )
  assumption_recommendation <- paste(assumption_recommendation_parts[nzchar(assumption_recommendation_parts)], collapse = " ")
  out <- data.frame(
    Item = c("Recommended model", "Analysis population", "Primary effect", "Follow-up decision", "Assumption decision", "Data condition", "Normality decision", "Mixed-model decision"),
    Recommendation = c(
      model_label,
      mixed_rm_analysis_population_label(analysis_population),
      if (nrow(primary_row) > 0) primary_row$Effect[[1]] else primary_effect,
      primary_recommendation,
      assumption_recommendation,
      if (excluded_n > 0) "Listwise deletion was applied." else "No selected rows were excluded.",
      if (isTRUE(normality_flag)) "Normality flagged; review LMM/robust sensitivity if distributional mismatch is meaningful." else normality_detail,
      alternative_recommendation
    ),
    Reason = c(
      if (length(group_labels) > 1L) "More than one independent variable was selected, so between-subject main effects and their interactions are modeled." else "One independent variable was selected.",
      if (identical(population_key, "itt")) "ITT keeps available repeated records through a mixed-model path; complete-case RM ANOVA remains a PP reference." else "PP uses subjects with complete selected repeated-measures and model variables.",
      primary_detail,
      if (is.finite(primary_p)) paste0("Primary p=", if (primary_p < .001) "<.001" else format_decimal3(primary_p), ".") else "Primary p was not available.",
      assumption_detail,
      paste0("Excluded cases: ", excluded_n, "."),
      normality_reason,
      if (mixed_fitted) "A long-format subject-random-intercept model was fitted from the RM selections." else outcome_decision$reason
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  covariate_time <- mixed_rm_covariate_time_recommendation(anova, covariate_labels)
  if (!is.null(covariate_time)) {
    out <- analysis_bind_rows(list(out, covariate_time))
  }
  out
}

prepare_mixed_rm_anova_results <- function(data, group_variable, repeated_variables, covariates = character(0), variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  group_variable <- as.character(group_variable %||% character(0))
  repeated_variables <- as.character(repeated_variables %||% character(0))
  covariates <- as.character(covariates %||% character(0))
  group_variable <- unique(group_variable[nzchar(group_variable)])
  repeated_variables <- repeated_variables[nzchar(repeated_variables)]
  covariates <- setdiff(covariates[nzchar(covariates)], c(group_variable, repeated_variables))
  if (length(group_variable) < 1L) {
    stop("Select at least one independent variable.")
  }
  if (length(repeated_variables) < 2L) {
    stop("Select at least two repeated-measures variables.")
  }
  measurements <- mixed_rm_measurement_lookup(variable_info)
  repeated_measurements <- vapply(repeated_variables, function(name) named_value(measurements, name, "continuous"), character(1))
  if (any(!repeated_measurements %in% c("continuous", "ordered"))) {
    stop("Repeated-measures variables must be continuous or ordinal-coded numeric variables.")
  }
  covariate_measurements <- vapply(covariates, function(name) named_value(measurements, name, "continuous"), character(1))
  if (length(covariate_measurements) > 0 && any(!covariate_measurements %in% c("binary", "category", "ordered", "continuous"))) {
    stop("Covariates must be continuous, binary, nominal, or ordinal variables.")
  }
  missing <- setdiff(unique(c(group_variable, repeated_variables, covariates)), names(data))
  if (length(missing) > 0) {
    stop("Selected variables were not found in the dataset: ", paste(missing, collapse = ", "))
  }
  analysis_population <- mixed_rm_resolve_analysis_population(options$analysis_population %||% "pp")
  assumption_review <- isTRUE(options$assumption_check %||% TRUE)
  time_labels <- mixed_rm_time_labels(repeated_variables, options, variable_info, labels, category_table)
  group_labels <- vapply(group_variable, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  include_within <- isTRUE(options$within_group_comparison %||% TRUE)
  include_between <- isTRUE(options$between_time_group_comparison %||% FALSE)
  observed_between_method <- "simple"
  adjusted_between_method <- if (length(covariates) > 0) "estimated" else "simple"
  include_posthoc <- isTRUE(options$posthoc %||% TRUE)
  covariate_labels <- if (length(covariates) > 0) {
    vapply(covariates, paired_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  } else {
    character(0)
  }
  frame_result <- tryCatch(
    list(frame = mixed_rm_complete_frame(data, group_variable, repeated_variables, covariates, covariate_measurements), error = NULL),
    error = function(e) list(frame = NULL, error = e)
  )
  frame <- frame_result$frame
  frame_error <- if (!is.null(frame_result$error)) conditionMessage(frame_result$error) else ""
  if (is.null(frame) && !identical(analysis_population, "itt")) {
    stop(frame_error)
  }
  outcome_decision <- mixed_rm_outcome_family_recommendation(data, repeated_variables, repeated_measurements)
  normality <- if (!is.null(frame) && isTRUE(assumption_review)) {
    mixed_rm_normality_table(frame$y, frame$group, time_labels)
  } else {
    data.frame()
  }
  normality_flag <- mixed_rm_normality_issue(normality)
  mixed_alternative <- tryCatch(
    mixed_rm_fit_mixed_alternative(
      data = data,
      group_variable = group_variable,
      repeated_variables = repeated_variables,
      covariates = covariates,
      time_labels = time_labels,
      variable_info = variable_info,
      labels = labels,
      category_table = category_table,
      repeated_measurements = repeated_measurements,
      analysis_population = analysis_population,
      normality_issue = normality_flag
    ),
    error = function(e) list(
      decision = outcome_decision,
      overview = data.frame(
        Item = c("Recommended alternative", "Status", "Reason"),
        Value = c(outcome_decision$label, "Fit failed", conditionMessage(e)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      coefficients = data.frame(),
      note = paste("Mixed-model alternative did not fit:", conditionMessage(e))
    )
  )
  if (is.null(frame)) {
    assumption <- if (isTRUE(assumption_review)) {
      mixed_rm_itt_assumption_table(data, group_variable, repeated_variables, covariates, covariate_measurements, frame_error)
    } else {
      data.frame()
    }
    recommendation <- mixed_rm_itt_only_recommendation_table(group_labels, covariates, covariate_labels, frame_error, outcome_decision, mixed_alternative)
    return(list(
      type = "mixed_rm_anova",
      group_variable = group_variable,
      repeated_variables = repeated_variables,
      covariates = covariates,
      options = options,
      analysis_population = analysis_population,
      overview = mixed_rm_itt_overview_table(data, group_variable, repeated_variables, time_labels, variable_info, labels, category_table, covariates, covariate_measurements),
      recommendation = recommendation,
      observed_descriptives = data.frame(),
      observed_descriptives_note = "",
      adjusted_descriptives = data.frame(),
      adjusted_descriptives_note = "",
      anova = data.frame(),
      descriptives = data.frame(),
      descriptives_note = "",
      assumption = assumption,
      normality = normality,
      posthoc = data.frame(),
      posthoc_note = "",
      method_note = "Complete-case RM ANOVA was not produced; interpret the ITT mixed-model alternative.",
      mixed_model_overview = mixed_alternative$overview %||% data.frame(),
      mixed_model_coefficients = mixed_alternative$coefficients %||% data.frame(),
      mixed_model_note = mixed_alternative$note %||% ""
    ))
  }
  anova <- if (length(covariates) > 0 || length(group_variable) > 1L) {
    mixed_rm_car_anova(frame$y, frame$group_data, frame$covariates, if (length(group_labels) == 1L) "Group" else group_labels, covariate_labels)
  } else {
    mixed_rm_formatted_anova(frame$y, frame$group)
  }
  assumption <- if (isTRUE(assumption_review)) {
    mixed_rm_assumption_table(frame$y, frame$group, options, frame$covariates, anova, frame$total_n, frame$excluded_n, time_labels)
  } else {
    data.frame()
  }
  adjusted_summary <- length(covariates) > 0
  anova_posthoc_allowed <- !isTRUE(adjusted_summary) && length(group_variable) <= 1L
  adjusted_posthoc_covariates <- if (isTRUE(adjusted_summary)) {
    data.frame(frame$group_data, frame$covariates, check.names = FALSE)
  } else {
    data.frame()
  }
  posthoc <- data.frame()
  if (isTRUE(include_posthoc) && ncol(frame$y) >= 3L) {
    posthoc <- if (isTRUE(anova_posthoc_allowed)) {
      mixed_rm_posthoc_table(frame$y, frame$group, time_labels, options$posthoc_adjustment %||% statedu_multiple_correction_default())
    } else {
      mixed_rm_style_posthoc_table(analysis_bind_rows(list(
        mixed_rm_time_posthoc_table(frame$y, time_labels, adjustment = options$posthoc_adjustment %||% statedu_multiple_correction_default(), adjusted = FALSE),
        if (length(covariates) > 0) mixed_rm_time_posthoc_table(frame$y, time_labels, adjusted_posthoc_covariates, adjustment = options$posthoc_adjustment %||% statedu_multiple_correction_default(), adjusted = TRUE) else data.frame(),
        if (isTRUE(include_within)) mixed_rm_within_group_posthoc_table(frame$y, frame$group, time_labels, adjustment = options$posthoc_adjustment %||% statedu_multiple_correction_default(), adjusted = FALSE) else data.frame(),
        if (length(covariates) > 0 && isTRUE(include_within)) mixed_rm_within_group_posthoc_table(frame$y, frame$group, time_labels, frame$covariates, adjustment = options$posthoc_adjustment %||% statedu_multiple_correction_default(), adjusted = TRUE) else data.frame()
      )))
    }
  }
  if (isTRUE(include_posthoc) && isTRUE(anova_posthoc_allowed) && ncol(frame$y) >= 3L && is.data.frame(anova) && "post-hoc" %in% colnames(anova)) {
    anova$`post-hoc`[anova$Effect == "Time"] <- if (mixed_rm_anova_effect_is_significant(anova, "Time")) {
      mixed_rm_posthoc_inline(posthoc, "Time comparison overall")
    } else {
      "n.s."
    }
    primary_effect <- mixed_rm_primary_effect_label(group_labels)
    if (length(group_labels) <= 1L) {
      interaction_effect <- if (any(anova$Effect == primary_effect)) primary_effect else "Group x Time"
      anova$`post-hoc`[anova$Effect == primary_effect | anova$Effect == "Group x Time"] <- if (mixed_rm_anova_effect_is_significant(anova, interaction_effect)) {
        mixed_rm_interaction_posthoc_inline(frame$y, frame$group, options$posthoc_adjustment %||% statedu_multiple_correction_default())
      } else {
        "n.s."
      }
    }
  } else if (isTRUE(include_posthoc) && isTRUE(adjusted_summary) && ncol(frame$y) >= 3L && is.data.frame(anova) && "post-hoc" %in% colnames(anova)) {
    time_markers_for_posthoc <- mixed_rm_time_markers(ncol(frame$y))
    anova$`post-hoc`[anova$Effect == "Time"] <- if (mixed_rm_anova_effect_is_significant(anova, "Time")) {
      mixed_rm_adjusted_group_time_posthoc(frame$y, adjusted_posthoc_covariates, time_markers_for_posthoc, options$posthoc_adjustment %||% statedu_multiple_correction_default())
    } else {
      "n.s."
    }
  }
  anova <- mixed_rm_finalize_anova_table(anova)
  sphericity <- attr(anova, "sphericity", exact = TRUE)
  epsilon <- if (is.list(sphericity) && !is.data.frame(sphericity)) as.numeric(sphericity$epsilon %||% NA_real_) else NA_real_
  primary_effect_label <- mixed_rm_primary_effect_label(group_labels)
  method_note <- if (length(covariates) > 0 || length(group_variable) > 1L) {
    paste0(
      "Primary: ", primary_effect_label, ". Between-subject factors modeled.",
      if (length(covariates) > 0) " Covariates additive." else "",
      " Sphericity: GG if epsilon(GG) < .75; HF otherwise."
    )
  } else if (ncol(frame$y) >= 3L && is.finite(epsilon)) {
    "Primary: Group x Time. Sphericity: GG if epsilon(GG) < .75; HF otherwise."
  } else {
    "Primary: Group x Time. Sphericity correction is not required with two repeated measurements."
  }
  method_note <- paste(method_note, "1 ES = partial \u03b7\u00b2.")
  posthoc_adjustment <- options$posthoc_adjustment %||% statedu_multiple_correction_default()
  observed_descriptives <- mixed_rm_descriptives(
    frame$y,
    frame$group,
    time_labels,
    NULL,
    posthoc_adjustment,
    include_within = include_within,
    include_between = include_between,
    between_method = observed_between_method,
    include_posthoc = include_posthoc,
    adjust = FALSE
  )
  observed_descriptives <- mixed_rm_split_group_summary_columns(observed_descriptives, frame$group_data, group_labels)
  adjusted_descriptives <- if (length(covariates) > 0) {
    adjusted_table <- mixed_rm_descriptives(
      frame$y,
      frame$group,
      time_labels,
      frame$covariates,
      posthoc_adjustment,
      include_within = include_within,
      include_between = include_between,
      between_method = adjusted_between_method,
      include_posthoc = include_posthoc,
      adjust = TRUE
    )
    mixed_rm_split_group_summary_columns(adjusted_table, frame$group_data, group_labels)
  } else {
    data.frame()
  }
  descriptives <- if (length(covariates) > 0) adjusted_descriptives else observed_descriptives
  time_markers <- attr(observed_descriptives, "time_markers", exact = TRUE)
  time_marker_note <- mixed_rm_time_marker_note(time_labels, unname(time_markers))
  observed_descriptives_note <- mixed_rm_summary_note(FALSE, character(0), include_within, include_between, observed_between_method, time_marker_note)
  adjusted_descriptives_note <- if (length(covariates) > 0) mixed_rm_summary_note(TRUE, covariates, include_within, include_between, adjusted_between_method, time_marker_note) else ""
  covariate_time_issue <- mixed_rm_covariate_time_issue(anova, covariate_labels)
  covariate_time_note <- if (isTRUE(covariate_time_issue)) {
    "Covariate x Time significant; adjusted estimates are time-specific."
  } else {
    ""
  }
  if (nzchar(covariate_time_note) && nzchar(adjusted_descriptives_note)) {
    adjusted_descriptives_note <- paste(adjusted_descriptives_note, covariate_time_note)
  }
  descriptives_note <- if (length(covariates) > 0) adjusted_descriptives_note else observed_descriptives_note
  posthoc_note_parts <- c(
    if (isTRUE(anova_posthoc_allowed)) "ANOVA Time post-hoc compares overall repeated means." else "Adjusted model: Time post-hoc uses adjusted overall time comparisons.",
    if (!isTRUE(anova_posthoc_allowed) && isTRUE(include_within)) "Within-group rows report observed/adjusted paired time comparisons.",
    if (nzchar(covariate_time_note)) "Covariate x Time is not a post-hoc family." else "",
    "Summary cells require a significant parent omnibus effect.",
    if (length(group_labels) > 1L) "Higher-order factor x Time post-hoc is not auto-filled." else "",
    time_marker_note,
    sprintf("Adjusted p values use %s correction.", if (identical(posthoc_adjustment, "bonferroni")) "Bonferroni" else "Holm-Bonferroni")
  )
  posthoc_note <- paste(posthoc_note_parts[nzchar(posthoc_note_parts)], collapse = " ")
  if (nzchar(covariate_time_note)) {
    method_note <- paste(method_note, covariate_time_note)
  }
  recommendation <- mixed_rm_recommendation_table(anova, assumption, normality, group_labels, covariates, covariate_labels, frame$excluded_n, analysis_population, outcome_decision, mixed_alternative)
  list(
    type = "mixed_rm_anova",
    group_variable = group_variable,
    repeated_variables = repeated_variables,
    covariates = covariates,
    options = options,
    analysis_population = analysis_population,
    overview = mixed_rm_overview_table(group_variable, repeated_variables, frame$y, frame$group, time_labels, variable_info, labels, category_table, covariates, analysis_population),
    recommendation = recommendation,
    observed_descriptives = observed_descriptives,
    observed_descriptives_note = observed_descriptives_note,
    adjusted_descriptives = adjusted_descriptives,
    adjusted_descriptives_note = adjusted_descriptives_note,
    anova = anova,
    descriptives = descriptives,
    descriptives_note = descriptives_note,
    assumption = assumption,
    normality = normality,
    mixed_model_overview = mixed_alternative$overview %||% data.frame(),
    mixed_model_coefficients = mixed_alternative$coefficients %||% data.frame(),
    mixed_model_note = mixed_alternative$note %||% "",
    posthoc = posthoc,
    posthoc_note = posthoc_note,
    method_note = method_note
  )
}
