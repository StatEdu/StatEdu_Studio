# ANCOVA analysis helpers.

ancova_measurement_lookup <- function(variable_info = NULL) {
  if (is.null(variable_info) || !all(c("name", "measurement") %in% names(variable_info))) {
    return(character(0))
  }
  stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
}

ancova_continuous_candidates <- function(variable_names, variable_info = NULL) {
  variable_names <- as.character(variable_names %||% character(0))
  measurement <- ancova_measurement_lookup(variable_info)
  measurement[measurement == "ordinal"] <- "ordered"
  if (length(measurement) == 0) {
    return(variable_names)
  }
  variable_names[variable_names %in% names(measurement) & measurement[variable_names] %in% c("continuous", "ordered")]
}

ancova_covariate_candidates <- function(variable_names, variable_info = NULL) {
  variable_names <- as.character(variable_names %||% character(0))
  measurement <- ancova_measurement_lookup(variable_info)
  measurement[measurement == "ordinal"] <- "ordered"
  measurement[measurement == "nominal"] <- "category"
  if (length(measurement) == 0) {
    return(variable_names)
  }
  variable_names[variable_names %in% names(measurement) & measurement[variable_names] %in% c("continuous", "binary", "category", "ordered")]
}

ancova_factor_candidates <- function(variable_names, variable_info = NULL) {
  variable_names <- as.character(variable_names %||% character(0))
  measurement <- ancova_measurement_lookup(variable_info)
  measurement[measurement == "ordinal"] <- "ordered"
  if (length(measurement) == 0) {
    return(variable_names)
  }
  variable_names[variable_names %in% names(measurement) & measurement[variable_names] %in% c("binary", "category", "ordered")]
}

ancova_format_f <- function(value) format_decimal3(value)

ancova_statistic_df_text <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) return("")
  if (abs(value - round(value)) < 1e-8) {
    return(as.character(as.integer(round(value))))
  }
  format_decimal3(value)
}

ancova_format_f_with_df <- function(value, df1, df2) {
  f_text <- format_decimal3(value)
  if (!nzchar(f_text)) return("")
  df <- suppressWarnings(as.numeric(c(df1, df2)))
  df <- df[is.finite(df)]
  if (length(df) < 2L) return(f_text)
  paste0(f_text, "(", paste(vapply(df[1:2], ancova_statistic_df_text, character(1)), collapse = ","), ")")
}

ancova_format_decimal3_vec <- function(values) {
  vapply(values, format_decimal3, character(1))
}

ancova_format_p_vec <- function(values) {
  vapply(values, format_p, character(1))
}

ancova_partial_eta2 <- function(f_value, df1, df2) {
  if (!is.finite(f_value) || !is.finite(df1) || !is.finite(df2) || f_value < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  (f_value * df1) / (f_value * df1 + df2)
}

ancova_rank_data <- function(data, variables) {
  ranked <- data
  for (variable in variables) {
    if (!is.numeric(ranked[[variable]])) next
    ranked[[variable]] <- rank(ranked[[variable]], ties.method = "average", na.last = "keep")
  }
  ranked
}

ancova_variable_measurement <- function(name, variable_info = NULL) {
  measurement <- ancova_measurement_lookup(variable_info)
  measurement <- tolower(as.character(measurement[[name]] %||% ""))
  if (identical(measurement, "ordinal")) measurement <- "ordered"
  if (identical(measurement, "nominal")) measurement <- "category"
  measurement
}

ancova_clean_data <- function(data, dependent, factor, covariates, variable_info = NULL, category_table = NULL) {
  columns <- unique(c(dependent, factor, covariates))
  cleaned <- data[, columns, drop = FALSE]
  cleaned[[dependent]] <- suppressWarnings(as.numeric(cleaned[[dependent]]))
  reference_values <- regression_reference_values_static(category_table)
  cleaned <- prepare_regression_model_data_static(
    cleaned,
    variables = unique(c(factor, covariates)),
    variable_info = variable_info,
    reference_values = reference_values
  )
  cleaned[[factor]] <- as.factor(cleaned[[factor]])
  factor_reference <- trimws(named_value(reference_values, factor, ""))
  if (nzchar(factor_reference) && factor_reference %in% levels(cleaned[[factor]])) {
    cleaned[[factor]] <- stats::relevel(cleaned[[factor]], ref = factor_reference)
  }
  for (covariate in covariates) {
    measurement <- ancova_variable_measurement(covariate, variable_info)
    if (identical(measurement, "continuous")) {
      cleaned[[covariate]] <- suppressWarnings(as.numeric(cleaned[[covariate]]))
    } else {
      cleaned[[covariate]] <- as.factor(cleaned[[covariate]])
      reference <- trimws(named_value(reference_values, covariate, ""))
      if (nzchar(reference) && reference %in% levels(cleaned[[covariate]])) {
        cleaned[[covariate]] <- stats::relevel(cleaned[[covariate]], ref = reference)
      }
    }
  }
  stats::na.omit(cleaned)
}

ancova_formula <- function(dependent, factor, covariates, interaction = FALSE) {
  quote_var <- function(value) sprintf("`%s`", value)
  rhs <- c(quote_var(factor), quote_var(covariates))
  if (isTRUE(interaction) && length(covariates) > 0) {
    rhs <- c(rhs, paste0(quote_var(factor), ":", quote_var(covariates)))
  }
  stats::as.formula(sprintf("`%s` ~ %s", dependent, paste(rhs, collapse = " + ")))
}

ancova_hc3_group_test <- function(model, factor) {
  coefficient_names <- names(stats::coef(model))
  normalized <- gsub("`", "", coefficient_names, fixed = TRUE)
  group_terms <- coefficient_names[startsWith(normalized, factor)]
  group_terms <- group_terms[!grepl(":", group_terms, fixed = TRUE)]
  if (length(group_terms) == 0) {
    return(list(f = NA_real_, df1 = NA_real_, df2 = stats::df.residual(model), p = NA_real_))
  }
  vcov_matrix <- tryCatch(sandwich::vcovHC(model, type = "HC3"), error = function(e) NULL)
  if (is.null(vcov_matrix)) {
    return(list(f = NA_real_, df1 = length(group_terms), df2 = stats::df.residual(model), p = NA_real_))
  }
  beta <- stats::coef(model)[group_terms]
  covariance <- vcov_matrix[group_terms, group_terms, drop = FALSE]
  inverse_covariance <- tryCatch(solve(covariance), error = function(e) NULL)
  df1 <- length(beta)
  df2 <- stats::df.residual(model)
  if (is.null(inverse_covariance) || df1 <= 0 || df2 <= 0) {
    return(list(f = NA_real_, df1 = df1, df2 = df2, p = NA_real_))
  }
  f_value <- as.numeric(t(beta) %*% inverse_covariance %*% beta / df1)
  list(f = f_value, df1 = df1, df2 = df2, p = stats::pf(f_value, df1, df2, lower.tail = FALSE))
}

ancova_anova_table <- function(model, method, factor) {
  anova_table <- as.data.frame(stats::anova(model))
  terms <- rownames(anova_table)
  out <- data.frame(
    Source = terms,
    df = ifelse(is.na(anova_table[["Df"]]), "", as.character(as.integer(anova_table[["Df"]]))),
    `Sum Sq` = ancova_format_decimal3_vec(anova_table[["Sum Sq"]]),
    `Mean Sq` = ancova_format_decimal3_vec(anova_table[["Mean Sq"]]),
    F = vapply(
      seq_len(nrow(anova_table)),
      function(index) ancova_format_f_with_df(anova_table[["F value"]][[index]], anova_table[["Df"]][[index]], stats::df.residual(model)),
      character(1)
    ),
    p = ancova_format_p_vec(anova_table[["Pr(>F)"]]),
    `partial eta2` = ancova_format_decimal3_vec(mapply(
      ancova_partial_eta2,
      anova_table[["F value"]],
      anova_table[["Df"]],
      stats::df.residual(model)
    )),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (identical(method, "Robust ANCOVA (HC3)")) {
    robust <- ancova_hc3_group_test(model, factor)
    row_index <- match(factor, out$Source)
    if (!is.na(row_index)) {
      out$F[[row_index]] <- ancova_format_f_with_df(robust$f, robust$df1, robust$df2)
      out$p[[row_index]] <- format_p(robust$p)
      out$`partial eta2`[[row_index]] <- format_decimal3(ancova_partial_eta2(robust$f, robust$df1, robust$df2))
    }
  }
  out
}

ancova_group_stat <- function(model, method, factor) {
  if (identical(method, "Robust ANCOVA (HC3)")) {
    robust <- ancova_hc3_group_test(model, factor)
    return(list(
      f = robust$f,
      df1 = robust$df1,
      df2 = robust$df2,
      p = robust$p,
      effect_size = ancova_partial_eta2(robust$f, robust$df1, robust$df2)
    ))
  }

  anova_table <- as.data.frame(stats::anova(model))
  row_index <- match(factor, rownames(anova_table))
  if (is.na(row_index)) {
    matches <- grep(sprintf("^`?%s`?$", factor), rownames(anova_table))
    row_index <- if (length(matches) > 0) matches[[1]] else NA_integer_
  }
  if (is.na(row_index)) {
    return(list(f = NA_real_, df1 = NA_real_, df2 = stats::df.residual(model), p = NA_real_, effect_size = NA_real_))
  }
  f_value <- suppressWarnings(as.numeric(anova_table[["F value"]][[row_index]]))
  df1 <- suppressWarnings(as.numeric(anova_table[["Df"]][[row_index]]))
  df2 <- stats::df.residual(model)
  p_value <- suppressWarnings(as.numeric(anova_table[["Pr(>F)"]][[row_index]]))
  list(
    f = f_value,
    df1 = df1,
    df2 = df2,
    p = p_value,
    effect_size = ancova_partial_eta2(f_value, df1, df2)
  )
}

ancova_prediction_grid <- function(fit_data, factor, covariates) {
  factor_levels <- levels(fit_data[[factor]])
  if (is.null(factor_levels) || length(factor_levels) == 0) {
    factor_levels <- unique(as.character(fit_data[[factor]]))
  }
  grid <- data.frame(stats::setNames(list(factor(factor_levels, levels = factor_levels)), factor), check.names = FALSE)
  for (covariate in covariates) {
    values <- fit_data[[covariate]]
    if (is.numeric(values)) {
      grid[[covariate]] <- mean(values, na.rm = TRUE)
    } else {
      value_levels <- if (is.factor(values)) levels(values) else unique(as.character(stats::na.omit(values)))
      reference <- if (length(value_levels) > 0) value_levels[[1]] else NA_character_
      grid[[covariate]] <- factor(rep(reference, nrow(grid)), levels = value_levels)
    }
  }
  grid
}

ancova_adjusted_means <- function(model, fit_data, factor, covariates) {
  grid <- ancova_prediction_grid(fit_data, factor, covariates)
  prediction <- stats::predict(model, newdata = grid, se.fit = TRUE)
  data.frame(
    Level = as.character(grid[[factor]]),
    Estimate = as.numeric(prediction$fit),
    SE = as.numeric(prediction$se.fit),
    stringsAsFactors = FALSE
  )
}

ancova_pairwise_p_matrix <- function(model, newdata, factor, robust = FALSE, adjustment = "bonferroni") {
  levels <- as.character(newdata[[factor]])
  if (length(levels) < 3L) {
    return(NULL)
  }
  terms_object <- stats::delete.response(stats::terms(model))
  x_matrix <- stats::model.matrix(terms_object, newdata, contrasts.arg = model$contrasts)
  coefficients <- stats::coef(model)
  keep <- names(coefficients)[!is.na(coefficients)]
  keep <- keep[keep %in% colnames(x_matrix)]
  if (length(keep) == 0) {
    return(NULL)
  }
  x_matrix <- x_matrix[, keep, drop = FALSE]
  coefficients <- coefficients[keep]
  covariance <- if (isTRUE(robust)) {
    tryCatch(sandwich::vcovHC(model, type = "HC3"), error = function(e) NULL)
  } else {
    stats::vcov(model)
  }
  if (is.null(covariance)) {
    return(NULL)
  }
  covariance <- covariance[keep, keep, drop = FALSE]
  p_matrix <- matrix(NA_real_, nrow = length(levels), ncol = length(levels), dimnames = list(levels, levels))
  pair_indices <- utils::combn(seq_along(levels), 2L, simplify = FALSE)
  raw_p <- numeric(length(pair_indices))
  for (index in seq_along(pair_indices)) {
    pair <- pair_indices[[index]]
    contrast <- x_matrix[pair[[1]], , drop = TRUE] - x_matrix[pair[[2]], , drop = TRUE]
    estimate <- as.numeric(sum(contrast * coefficients))
    se <- sqrt(as.numeric(t(contrast) %*% covariance %*% contrast))
    raw_p[[index]] <- if (is.finite(se) && se > 0) {
      2 * stats::pt(abs(estimate / se), df = stats::df.residual(model), lower.tail = FALSE)
    } else {
      NA_real_
    }
  }
  adjustment <- as.character(adjustment %||% "bonferroni")
  if (!adjustment %in% c("bonferroni", "holm")) {
    adjustment <- "bonferroni"
  }
  adjusted_p <- stats::p.adjust(raw_p, method = adjustment)
  for (index in seq_along(pair_indices)) {
    pair <- pair_indices[[index]]
    p_matrix[pair[[1]], pair[[2]]] <- adjusted_p[[index]]
    p_matrix[pair[[2]], pair[[1]]] <- adjusted_p[[index]]
  }
  diag(p_matrix) <- 1
  p_matrix
}

ancova_result_table <- function(model, fit_data, dependent, factor, covariates, method, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  adjusted <- ancova_adjusted_means(model, fit_data, factor, covariates)
  group_stat <- ancova_group_stat(model, method, factor)
  level_labels <- ttest_display_levels(factor, adjusted$Level, category_table)
  rows <- data.frame(
    Variable = "",
    Label = level_labels,
    `Adjusted mean` = ancova_format_decimal3_vec(adjusted$Estimate),
    SE = ancova_format_decimal3_vec(adjusted$SE),
    F = "",
    p = "",
    `Effect size` = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rows$Variable[[1]] <- ttest_display_variable(factor, variable_info, labels, category_table)
  rows$F[[1]] <- if (isTRUE(options$show_df)) {
    ancova_format_f_with_df(group_stat$f, group_stat$df1, group_stat$df2)
  } else {
    format_decimal3(group_stat$f)
  }
  rows$p[[1]] <- format_p(group_stat$p)
  rows$`Effect size`[[1]] <- format_decimal3(group_stat$effect_size)
  if (isTRUE(options$mean_se)) {
    rows[["M \u00B1 SE"]] <- ifelse(
      nzchar(rows[["Adjusted mean"]]) & nzchar(rows$SE),
      paste0(rows[["Adjusted mean"]], "\u00A0\u00B1\u00A0", rows$SE),
      ""
    )
    rows <- rows[, c("Variable", "Label", "M \u00B1 SE", "F", "p", "Effect size"), drop = FALSE]
  }

  if (nrow(rows) >= 3L) {
    grid <- ancova_prediction_grid(fit_data, factor, covariates)
    posthoc_method <- as.character(options$posthoc_method %||% "bonferroni")
    if (!posthoc_method %in% c("bonferroni", "holm")) posthoc_method <- "bonferroni"
    p_matrix <- ancova_pairwise_p_matrix(
      model,
      grid,
      factor,
      robust = identical(method, "Robust ANCOVA (HC3)"),
      adjustment = posthoc_method
    )
    rows[["post-hoc"]] <- ""
    if (isTRUE(options$ordered_significance) && !is.null(p_matrix)) {
      level_label_map <- stats::setNames(ttest_display_levels(factor, adjusted$Level, category_table), adjusted$Level)
      notation <- ttest_ordered_significance_notation(
        adjusted$Estimate,
        adjusted$Level,
        p_matrix,
        labels = level_label_map
      )
      rows <- ttest_distribute_ordered_posthoc(rows, notation)
    } else {
      letters <- ttest_group_letters(adjusted$Estimate, adjusted$Level, p_matrix, ordered = FALSE)
      rows[["post-hoc"]] <- ttest_lookup_letters(letters, adjusted$Level)
    }
  }
  rows
}
ancova_residual_normality_test <- function(residuals, method = "lillie") {
  residuals <- residuals[is.finite(residuals)]
  method <- as.character(method %||% "lillie")
  if (identical(method, "shapiro") && length(residuals) >= 3L && length(residuals) <= 5000L) {
    test <- tryCatch(stats::shapiro.test(residuals), error = function(e) NULL)
    return(list(method = "Shapiro-Wilk", p.value = if (!is.null(test)) test$p.value else NA_real_))
  }
  test <- tryCatch(nortest::lillie.test(residuals), error = function(e) NULL)
  list(method = "Lilliefors (K-S)", p.value = if (!is.null(test)) test$p.value else NA_real_)
}

ancova_assumptions <- function(model, clean_data, dependent, factor, covariates, options = list()) {
  residuals <- stats::residuals(model)
  normality <- ancova_residual_normality_test(residuals, options$normality_method %||% "lillie")
  homogeneity <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
  interaction_p <- NA_real_
  if (length(covariates) > 0) {
    interaction_model <- tryCatch(stats::lm(ancova_formula(dependent, factor, covariates, interaction = TRUE), data = clean_data), error = function(e) NULL)
    if (!is.null(interaction_model)) {
      interaction_terms <- grep(":", rownames(stats::anova(interaction_model)), fixed = TRUE, value = TRUE)
      interaction_rows <- match(interaction_terms, rownames(stats::anova(interaction_model)))
      p_values <- stats::anova(interaction_model)[["Pr(>F)"]][interaction_rows]
      interaction_p <- suppressWarnings(min(p_values, na.rm = TRUE))
      if (!is.finite(interaction_p)) interaction_p <- NA_real_
    }
  }
  list(
    normality_method = normality$method %||% "Lilliefors (K-S)",
    normality_p = normality$p.value %||% NA_real_,
    homogeneity_p = if (!is.null(homogeneity)) homogeneity$p.value else NA_real_,
    slope_p = interaction_p
  )
}

ancova_choose_method <- function(assumptions, options = list()) {
  if (isTRUE(options$force_ranked)) {
    return("Ranked ANCOVA")
  }
  if (is.finite(assumptions$slope_p) && assumptions$slope_p < 0.05) {
    return("Interaction ANCOVA")
  }
  if (!identical(options$normality_enabled, FALSE) && is.finite(assumptions$normality_p) && assumptions$normality_p < 0.05) {
    return("Ranked ANCOVA")
  }
  if (is.finite(assumptions$homogeneity_p) && assumptions$homogeneity_p < 0.05) {
    return("Robust ANCOVA (HC3)")
  }
  "ANCOVA"
}

ancova_method_reason <- function(method, assumptions) {
  if (identical(method, "Interaction ANCOVA")) {
    return("homogeneity of regression slopes not satisfied")
  }
  if (identical(method, "Ranked ANCOVA")) {
    return("residual normality not satisfied or ranked analysis selected")
  }
  if (identical(method, "Robust ANCOVA (HC3)")) {
    return("homogeneity of variance not satisfied")
  }
  "residual normality, homogeneity of variance, and homogeneity of regression slopes satisfied"
}
ancova_single_result <- function(data, dependent, factor, covariates, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  clean <- ancova_clean_data(data, dependent, factor, covariates, variable_info, category_table)
  if (nrow(clean) < 4L) {
    stop("ANCOVA requires at least four complete cases.")
  }
  if (nlevels(clean[[factor]]) < 2L) {
    stop("Grouping variable must have at least two observed levels.")
  }
  if (length(covariates) == 0) {
    stop("Select at least one covariate.")
  }
  base_model <- stats::lm(ancova_formula(dependent, factor, covariates), data = clean)
  assumptions <- ancova_assumptions(base_model, clean, dependent, factor, covariates, options)
  method <- ancova_choose_method(assumptions, options)
  fit_data <- clean
  interaction <- identical(method, "Interaction ANCOVA")
  if (identical(method, "Ranked ANCOVA")) {
    fit_data <- ancova_rank_data(clean, c(dependent, covariates))
  }
  model <- stats::lm(ancova_formula(dependent, factor, covariates, interaction = interaction), data = fit_data)
  table <- ancova_result_table(model, fit_data, dependent, factor, covariates, method, variable_info, labels, category_table, options)
  posthoc_note <- if (nlevels(fit_data[[factor]]) >= 3L) {
    posthoc_method <- as.character(options$posthoc_method %||% "bonferroni")
    if (!posthoc_method %in% c("bonferroni", "holm")) posthoc_method <- "bonferroni"
    adjustment_note <- if (identical(posthoc_method, "holm")) {
      "Holm-Bonferroni-adjusted"
    } else {
      "Bonferroni-corrected"
    }
    covariance_note <- if (identical(method, "Robust ANCOVA (HC3)")) {
      "using HC3 robust covariance"
    } else {
      "using the fitted model covariance"
    }
    if (isTRUE(options$ordered_significance)) {
      sprintf("Post-hoc: %s pairwise model contrasts of adjusted means, %s; displayed with ordered significance notation.", adjustment_note, covariance_note)
    } else {
      sprintf("Post-hoc: %s pairwise model contrasts of adjusted means, %s; displayed with compact letter notation.", adjustment_note, covariance_note)
    }
  } else {
    NULL
  }
  list(
    dependent = dependent,
    factor = factor,
    covariates = covariates,
    n = stats::nobs(model),
    method = method,
    reason = ancova_method_reason(method, assumptions),
    assumptions = assumptions,
    table = table,
    model = model,
    clean_data = clean,
    fit_data = fit_data,
    options = options,
    note = paste(
      "Analysis method:",
      method,
      "Effect size = partial eta squared.",
      posthoc_note,
      if (identical(method, "Robust ANCOVA (HC3)")) "Group effect F and p use HC3 robust covariance." else NULL,
      if (identical(method, "Ranked ANCOVA")) "Ranked ANCOVA uses rank-transformed dependent variable and continuous covariates; categorical covariates are dummy-coded." else NULL,
      if (identical(method, "Interaction ANCOVA")) "Group effects should be interpreted with group x covariate interactions." else NULL
    )
  )
}

prepare_ancova_results <- function(data, dependents, factor, covariates, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  if (!is.data.frame(data)) {
    stop("No data frame is available for ANCOVA.")
  }
  dependents <- intersect(as.character(dependents %||% character(0)), names(data))
  factor <- intersect(as.character(factor %||% character(0)), names(data))
  covariates <- intersect(as.character(covariates %||% character(0)), names(data))
  if (length(factor) > 1L) factor <- factor[[1]]
  results <- list()
  skipped <- list()
  for (dependent in dependents) {
    item <- tryCatch(
      ancova_single_result(data, dependent, factor, covariates, variable_info, labels, category_table, options),
      error = function(e) {
        data.frame(
          Type = "Skipped",
          `Dependent variable` = display_variable_name_static(dependent, variable_info, labels, label_only = TRUE),
          `Independent variable` = display_variable_name_static(factor, variable_info, labels, label_only = TRUE),
          N = "",
          Message = conditionMessage(e),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    )
    if (is.data.frame(item)) {
      skipped[[length(skipped) + 1L]] <- item
    } else {
      results[[length(results) + 1L]] <- item
    }
  }
  list(
    type = "ancova",
    dependents = dependents,
    factor = factor,
    covariates = covariates,
    results = results,
    skipped = ttest_bind_result_rows(skipped),
    options = options
  )
}
