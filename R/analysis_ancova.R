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

ancova_sum_of_squares_type <- function(options = list(), method = NULL) {
  value <- tolower(as.character(options$sum_of_squares %||% "type2"))
  if (!value %in% c("type1", "type2", "type3")) value <- "type2"
  if (identical(method, "Interaction ANCOVA") && is.null(options$sum_of_squares)) value <- "type3"
  value
}

ancova_sum_of_squares_label <- function(type) {
  switch(
    tolower(as.character(type %||% "type2")),
    type1 = "Type I SS",
    type3 = "Type III SS",
    "Type II SS"
  )
}

ancova_sum_of_squares_note <- function(type) {
  switch(
    tolower(as.character(type %||% "type2")),
    type1 = "Sum of squares: Type I. Tests are sequential and depend on model term order.",
    type3 = "Sum of squares: Type III. Effects are tested conditional on all other model terms.",
    "Sum of squares: Type II. Group effect is tested after adjustment for covariates."
  )
}

ancova_fit_lm <- function(formula, data, sum_of_squares = "type2") {
  if (identical(tolower(as.character(sum_of_squares)), "type3")) {
    old_contrasts <- getOption("contrasts")
    on.exit(options(contrasts = old_contrasts), add = TRUE)
    options(contrasts = c("contr.sum", "contr.poly"))
  }
  stats::lm(formula, data = data)
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

ancova_linearity_diagnostics <- function(clean_data, dependent, factor, covariates) {
  numeric_covariates <- covariates[vapply(covariates, function(name) is.numeric(clean_data[[name]]), logical(1))]
  if (length(numeric_covariates) == 0) {
    return(list(rows = data.frame(), min_p = NA_real_, flagged = character(0), summary = "No continuous covariate"))
  }
  rows <- lapply(numeric_covariates, function(covariate) {
    values <- suppressWarnings(as.numeric(clean_data[[covariate]]))
    if (sum(is.finite(values)) < 4L || stats::sd(values, na.rm = TRUE) <= 0) {
      return(data.frame(Covariate = covariate, p = NA_real_, Status = "not testable", stringsAsFactors = FALSE))
    }
    squared_name <- paste0(covariate, "__squared")
    diagnostic_data <- clean_data
    centered <- values - mean(values, na.rm = TRUE)
    diagnostic_data[[squared_name]] <- centered^2
    base_model <- tryCatch(stats::lm(ancova_formula(dependent, factor, covariates), data = diagnostic_data), error = function(e) NULL)
    extended_model <- tryCatch(stats::lm(ancova_formula(dependent, factor, c(covariates, squared_name)), data = diagnostic_data), error = function(e) NULL)
    comparison <- if (!is.null(base_model) && !is.null(extended_model)) {
      tryCatch(stats::anova(base_model, extended_model), error = function(e) NULL)
    } else {
      NULL
    }
    p_value <- if (!is.null(comparison) && "Pr(>F)" %in% names(comparison) && nrow(comparison) >= 2L) {
      suppressWarnings(as.numeric(comparison[["Pr(>F)"]][[2]]))
    } else {
      NA_real_
    }
    data.frame(
      Covariate = covariate,
      p = p_value,
      Status = if (is.finite(p_value) && p_value < 0.05) "possible nonlinearity" else if (is.finite(p_value)) "no clear nonlinearity" else "not testable",
      stringsAsFactors = FALSE
    )
  })
  rows <- ttest_bind_result_rows(rows)
  flagged <- if (is.data.frame(rows) && nrow(rows) > 0 && "p" %in% names(rows)) {
    rows$Covariate[is.finite(rows$p) & rows$p < 0.05]
  } else {
    character(0)
  }
  min_p <- if (is.data.frame(rows) && nrow(rows) > 0 && any(is.finite(rows$p))) min(rows$p[is.finite(rows$p)]) else NA_real_
  summary <- if (length(flagged) > 0) {
    paste("Possible nonlinearity:", paste(flagged, collapse = ", "))
  } else {
    "No clear nonlinearity"
  }
  list(rows = rows, min_p = min_p, flagged = flagged, summary = summary)
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

ancova_effect_table <- function(model, sum_of_squares = "type2", robust = FALSE) {
  sum_of_squares <- ancova_sum_of_squares_type(list(sum_of_squares = sum_of_squares))
  if (identical(sum_of_squares, "type1")) {
    anova_table <- as.data.frame(stats::anova(model))
    return(data.frame(
      Source = rownames(anova_table),
      df = suppressWarnings(as.numeric(anova_table[["Df"]])),
      sum_sq = suppressWarnings(as.numeric(anova_table[["Sum Sq"]])),
      mean_sq = suppressWarnings(as.numeric(anova_table[["Mean Sq"]])),
      f = suppressWarnings(as.numeric(anova_table[["F value"]])),
      p = suppressWarnings(as.numeric(anova_table[["Pr(>F)"]])) ,
      stringsAsFactors = FALSE
    ))
  }
  type_number <- if (identical(sum_of_squares, "type3")) 3L else 2L
  anova_table <- tryCatch(
    suppressMessages(as.data.frame(car::Anova(
      model,
      type = type_number,
      white.adjust = if (isTRUE(robust)) "hc3" else FALSE
    ))),
    error = function(e) NULL
  )
  if (is.null(anova_table)) {
    return(ancova_effect_table(model, "type1", robust = FALSE))
  }
  terms <- rownames(anova_table)
  keep_terms <- !terms %in% c("(Intercept)", "Residuals", "Error")
  anova_table <- anova_table[keep_terms, , drop = FALSE]
  sum_sq_column <- intersect(c("Sum Sq", "Sum Sq."), names(anova_table))
  f_column <- intersect(c("F value", "F"), names(anova_table))
  p_column <- intersect(c("Pr(>F)", "Pr(>Chisq)", "Pr(>Chisq.)"), names(anova_table))
  data.frame(
    Source = rownames(anova_table),
    df = suppressWarnings(as.numeric(anova_table[["Df"]])),
    sum_sq = if (length(sum_sq_column) > 0) suppressWarnings(as.numeric(anova_table[[sum_sq_column[[1]]]])) else NA_real_,
    mean_sq = NA_real_,
    f = if (length(f_column) > 0) suppressWarnings(as.numeric(anova_table[[f_column[[1]]]])) else NA_real_,
    p = if (length(p_column) > 0) suppressWarnings(as.numeric(anova_table[[p_column[[1]]]])) else NA_real_,
    stringsAsFactors = FALSE
  )
}

ancova_source_row <- function(effect_table, term) {
  row_index <- match(term, effect_table$Source)
  if (is.na(row_index)) {
    matches <- grep(sprintf("^`?%s`?$", term), effect_table$Source)
    row_index <- if (length(matches) > 0) matches[[1]] else NA_integer_
  }
  row_index
}

ancova_anova_table <- function(model, method, factor, sum_of_squares = "type2") {
  anova_table <- ancova_effect_table(model, sum_of_squares)
  df2 <- stats::df.residual(model)
  if (nrow(anova_table) == 0) {
    return(data.frame())
  }
  mean_sq <- anova_table$mean_sq
  missing_mean_sq <- !is.finite(mean_sq) & is.finite(anova_table$sum_sq) & is.finite(anova_table$df) & anova_table$df > 0
  mean_sq[missing_mean_sq] <- anova_table$sum_sq[missing_mean_sq] / anova_table$df[missing_mean_sq]
  out <- data.frame(
    Source = anova_table$Source,
    df = ifelse(is.na(anova_table$df), "", as.character(as.integer(anova_table$df))),
    `Sum Sq` = ancova_format_decimal3_vec(anova_table$sum_sq),
    `Mean Sq` = ancova_format_decimal3_vec(mean_sq),
    F = vapply(
      seq_len(nrow(anova_table)),
      function(index) ancova_format_f_with_df(anova_table$f[[index]], anova_table$df[[index]], df2),
      character(1)
    ),
    p = ancova_format_p_vec(anova_table$p),
    `partial eta2` = ancova_format_decimal3_vec(mapply(
      ancova_partial_eta2,
      anova_table$f,
      anova_table$df,
      df2
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

ancova_group_stat <- function(model, method, factor, sum_of_squares = "type2") {
  if (identical(method, "Robust ANCOVA (HC3)")) {
    if (!identical(sum_of_squares, "type1")) {
      robust_table <- ancova_effect_table(model, sum_of_squares, robust = TRUE)
      row_index <- ancova_source_row(robust_table, factor)
      if (!is.na(row_index)) {
        f_value <- suppressWarnings(as.numeric(robust_table$f[[row_index]]))
        df1 <- suppressWarnings(as.numeric(robust_table$df[[row_index]]))
        df2 <- stats::df.residual(model)
        p_value <- suppressWarnings(as.numeric(robust_table$p[[row_index]]))
        return(list(
          f = f_value,
          df1 = df1,
          df2 = df2,
          p = p_value,
          effect_size = ancova_partial_eta2(f_value, df1, df2)
        ))
      }
    }
    robust <- ancova_hc3_group_test(model, factor)
    return(list(
      f = robust$f,
      df1 = robust$df1,
      df2 = robust$df2,
      p = robust$p,
      effect_size = ancova_partial_eta2(robust$f, robust$df1, robust$df2)
    ))
  }

  anova_table <- ancova_effect_table(model, sum_of_squares)
  row_index <- ancova_source_row(anova_table, factor)
  if (is.na(row_index)) {
    return(list(f = NA_real_, df1 = NA_real_, df2 = stats::df.residual(model), p = NA_real_, effect_size = NA_real_))
  }
  f_value <- suppressWarnings(as.numeric(anova_table$f[[row_index]]))
  df1 <- suppressWarnings(as.numeric(anova_table$df[[row_index]]))
  df2 <- stats::df.residual(model)
  p_value <- suppressWarnings(as.numeric(anova_table$p[[row_index]]))
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
  prediction_warning <- ""
  prediction <- withCallingHandlers(
    stats::predict(model, newdata = grid, se.fit = TRUE),
    warning = function(w) {
      message <- conditionMessage(w)
      if (grepl("rank-deficient|non-estim", message, ignore.case = TRUE)) {
        prediction_warning <<- message
        invokeRestart("muffleWarning")
      }
    }
  )
  non_estim <- attr(prediction$fit, "non-estim", exact = TRUE)
  if (is.null(non_estim)) {
    non_estim <- attr(prediction, "non-estim", exact = TRUE)
  }
  fit <- as.numeric(prediction$fit)
  se <- as.numeric(prediction$se.fit)
  non_estim_index <- suppressWarnings(as.integer(non_estim))
  non_estim_index <- non_estim_index[is.finite(non_estim_index) & non_estim_index >= 1L & non_estim_index <= length(fit)]
  if (length(non_estim_index) > 0L) {
    fit[non_estim_index] <- NA_real_
    se[non_estim_index] <- NA_real_
  }
  out <- data.frame(
    Level = as.character(grid[[factor]]),
    Estimate = fit,
    SE = se,
    stringsAsFactors = FALSE
  )
  if (nzchar(prediction_warning) || length(non_estim_index) > 0L) {
    attr(out, "warning") <- "Some adjusted means could not be estimated because the fitted ANCOVA model is rank-deficient."
  }
  out
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

ancova_interaction_terms_table <- function(model, sum_of_squares = "type3") {
  effect_table <- ancova_effect_table(model, sum_of_squares)
  if (!is.data.frame(effect_table) || nrow(effect_table) == 0) {
    return(data.frame())
  }
  rows <- grepl(":", effect_table$Source, fixed = TRUE)
  if (!any(rows)) {
    return(data.frame())
  }
  interactions <- effect_table[rows, , drop = FALSE]
  data.frame(
    Term = interactions$Source,
    df = ancova_format_decimal3_vec(interactions$df),
    F = ancova_format_decimal3_vec(interactions$f),
    p = ancova_format_p_vec(interactions$p),
    `partial eta2` = ancova_format_decimal3_vec(mapply(
      ancova_partial_eta2,
      interactions$f,
      interactions$df,
      stats::df.residual(model)
    )),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

ancova_conditional_grid <- function(fit_data, factor, covariates, overrides = list()) {
  grid <- ancova_prediction_grid(fit_data, factor, covariates)
  if (length(overrides) > 0) {
    for (name in names(overrides)) {
      if (!name %in% names(grid)) next
      values <- fit_data[[name]]
      if (is.numeric(values)) {
        grid[[name]] <- as.numeric(overrides[[name]])
      } else {
        value_levels <- if (is.factor(values)) levels(values) else unique(as.character(stats::na.omit(values)))
        grid[[name]] <- factor(rep(as.character(overrides[[name]]), nrow(grid)), levels = value_levels)
      }
    }
  }
  grid
}

ancova_pairwise_contrasts <- function(model, newdata, factor) {
  levels <- as.character(newdata[[factor]])
  if (length(levels) < 2L) {
    return(data.frame())
  }
  terms_object <- stats::delete.response(stats::terms(model))
  x_matrix <- stats::model.matrix(terms_object, newdata, contrasts.arg = model$contrasts)
  coefficients <- stats::coef(model)
  keep <- names(coefficients)[!is.na(coefficients)]
  keep <- keep[keep %in% colnames(x_matrix)]
  if (length(keep) == 0) {
    return(data.frame())
  }
  x_matrix <- x_matrix[, keep, drop = FALSE]
  coefficients <- coefficients[keep]
  covariance <- stats::vcov(model)
  covariance <- covariance[keep, keep, drop = FALSE]
  pairs <- utils::combn(seq_along(levels), 2L, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    contrast <- x_matrix[pair[[1]], , drop = TRUE] - x_matrix[pair[[2]], , drop = TRUE]
    estimate <- as.numeric(sum(contrast * coefficients))
    se <- sqrt(as.numeric(t(contrast) %*% covariance %*% contrast))
    t_value <- if (is.finite(se) && se > 0) estimate / se else NA_real_
    p_value <- if (is.finite(t_value)) 2 * stats::pt(abs(t_value), df = stats::df.residual(model), lower.tail = FALSE) else NA_real_
    data.frame(
      Contrast = paste0(levels[[pair[[1]]]], " - ", levels[[pair[[2]]]]),
      Estimate = estimate,
      SE = se,
      t = t_value,
      p = p_value,
      stringsAsFactors = FALSE
    )
  })
  ttest_bind_result_rows(rows)
}

ancova_simple_effects_table <- function(model, fit_data, factor, covariates) {
  numeric_covariates <- covariates[vapply(covariates, function(name) is.numeric(fit_data[[name]]), logical(1))]
  if (length(numeric_covariates) == 0) {
    return(data.frame())
  }
  rows <- list()
  for (covariate in numeric_covariates) {
    values <- suppressWarnings(as.numeric(fit_data[[covariate]]))
    mean_value <- mean(values, na.rm = TRUE)
    sd_value <- stats::sd(values, na.rm = TRUE)
    if (!is.finite(mean_value) || !is.finite(sd_value) || sd_value <= 0) next
    probes <- data.frame(
      `Covariate value` = c("Mean - 1 SD", "Mean", "Mean + 1 SD"),
      value = c(mean_value - sd_value, mean_value, mean_value + sd_value),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    for (index in seq_len(nrow(probes))) {
      newdata <- ancova_conditional_grid(fit_data, factor, covariates, stats::setNames(list(probes$value[[index]]), covariate))
      contrasts <- ancova_pairwise_contrasts(model, newdata, factor)
      if (!is.data.frame(contrasts) || nrow(contrasts) == 0) next
      rows[[length(rows) + 1L]] <- data.frame(
        Covariate = covariate,
        `Covariate value` = probes$`Covariate value`[[index]],
        Value = format_decimal3(probes$value[[index]]),
        Contrast = contrasts$Contrast,
        Estimate = ancova_format_decimal3_vec(contrasts$Estimate),
        SE = ancova_format_decimal3_vec(contrasts$SE),
        t = ancova_format_decimal3_vec(contrasts$t),
        p = ancova_format_p_vec(contrasts$p),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  ttest_bind_result_rows(rows)
}

ancova_collinearity_diagnostics <- function(model) {
  model_matrix <- stats::model.matrix(model)
  rank <- qr(model_matrix)$rank
  rank_deficient <- rank < ncol(model_matrix)
  collinearity <- coefficient_collinearity(model_matrix)
  terms <- setdiff(colnames(model_matrix), "(Intercept)")
  if (length(terms) == 0) {
    return(list(table = data.frame(), max_vif = NA_real_, summary = "No predictors", rank_deficient = rank_deficient))
  }
  vif_values <- suppressWarnings(as.numeric(collinearity$vif[terms]))
  tolerance_values <- suppressWarnings(as.numeric(collinearity$tolerance[terms]))
  status <- ifelse(
    is.infinite(vif_values) | isTRUE(rank_deficient),
    "severe collinearity",
    ifelse(
      is.finite(vif_values) & vif_values > 10,
      "VIF > 10",
      ifelse(is.finite(vif_values) & vif_values > 5, "VIF > 5", "acceptable")
    )
  )
  table <- data.frame(
    Term = terms,
    Tolerance = tolerance_values,
    VIF = vif_values,
    Status = status,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  finite_vif <- vif_values[is.finite(vif_values)]
  max_vif <- if (any(is.infinite(vif_values))) Inf else if (length(finite_vif) > 0) max(finite_vif, na.rm = TRUE) else NA_real_
  summary <- if (isTRUE(rank_deficient)) {
    "Rank deficient"
  } else if (is.infinite(max_vif) || (is.finite(max_vif) && max_vif > 10)) {
    sprintf("High collinearity (max VIF=%s)", format_decimal3(max_vif))
  } else if (is.finite(max_vif) && max_vif > 5) {
    sprintf("Moderate collinearity (max VIF=%s)", format_decimal3(max_vif))
  } else if (is.finite(max_vif)) {
    sprintf("Acceptable (max VIF=%s)", format_decimal3(max_vif))
  } else {
    "Not testable"
  }
  list(table = table, max_vif = max_vif, summary = summary, rank_deficient = rank_deficient)
}

ancova_influence_diagnostics <- function(model) {
  n <- stats::nobs(model)
  p <- length(stats::coef(model)[!is.na(stats::coef(model))])
  if (!is.finite(n) || n <= 0 || !is.finite(p) || p <= 0) {
    return(list(table = data.frame(), summary = "Not testable", flagged_n = NA_integer_))
  }
  studentized <- tryCatch(stats::rstudent(model), error = function(e) rep(NA_real_, n))
  leverage <- tryCatch(stats::hatvalues(model), error = function(e) rep(NA_real_, n))
  cooks <- tryCatch(stats::cooks.distance(model), error = function(e) rep(NA_real_, n))
  row_ids <- names(studentized)
  if (is.null(row_ids) || length(row_ids) != length(studentized)) {
    row_ids <- as.character(seq_along(studentized))
  }
  leverage_cutoff <- 2 * p / n
  cooks_cutoff <- 4 / n
  flagged <- (is.finite(studentized) & abs(studentized) > 3) |
    (is.finite(leverage) & leverage > leverage_cutoff) |
    (is.finite(cooks) & cooks > cooks_cutoff)
  table <- data.frame(
    Case = row_ids,
    `Studentized residual` = studentized,
    Leverage = leverage,
    `Cook's D` = cooks,
    Flag = ifelse(flagged, "flagged", ""),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  max_abs_studentized <- if (any(is.finite(studentized))) max(abs(studentized), na.rm = TRUE) else NA_real_
  max_leverage <- if (any(is.finite(leverage))) max(leverage, na.rm = TRUE) else NA_real_
  max_cooks <- if (any(is.finite(cooks))) max(cooks, na.rm = TRUE) else NA_real_
  flagged_n <- sum(flagged, na.rm = TRUE)
  summary <- if (flagged_n > 0) {
    sprintf("Flagged cases=%d; max Cook's D=%s", flagged_n, format_decimal3(max_cooks))
  } else {
    "No influential cases flagged"
  }
  list(
    table = table,
    flagged_n = flagged_n,
    max_abs_studentized = max_abs_studentized,
    max_leverage = max_leverage,
    max_cooks = max_cooks,
    leverage_cutoff = leverage_cutoff,
    cooks_cutoff = cooks_cutoff,
    summary = summary
  )
}

ancova_result_table <- function(model, fit_data, dependent, factor, covariates, method, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  adjusted <- ancova_adjusted_means(model, fit_data, factor, covariates)
  adjusted_warning <- attr(adjusted, "warning", exact = TRUE)
  sum_of_squares <- ancova_sum_of_squares_type(options, method)
  group_stat <- ancova_group_stat(model, method, factor, sum_of_squares)
  level_labels <- ttest_display_levels(factor, adjusted$Level, category_table)
  rows <- data.frame(
    Variable = "",
    Label = level_labels,
    SE = ancova_format_decimal3_vec(adjusted$SE),
    F = "",
    p = "",
    `Effect size` = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  estimate_column <- if (identical(method, "Ranked ANCOVA")) "Adjusted rank mean" else "Adjusted mean"
  rows[[estimate_column]] <- ancova_format_decimal3_vec(adjusted$Estimate)
  rows <- rows[, c("Variable", "Label", estimate_column, "SE", "F", "p", "Effect size"), drop = FALSE]
  rows$Variable[[1]] <- ttest_display_variable(factor, variable_info, labels, category_table)
  rows$F[[1]] <- if (isTRUE(options$show_df)) {
    ancova_format_f_with_df(group_stat$f, group_stat$df1, group_stat$df2)
  } else {
    format_decimal3(group_stat$f)
  }
  rows$p[[1]] <- format_p(group_stat$p)
  rows$`Effect size`[[1]] <- format_decimal3(group_stat$effect_size)
  if (isTRUE(options$mean_se)) {
    combined_column <- if (identical(method, "Ranked ANCOVA")) "Rank M \u00B1 SE" else "M \u00B1 SE"
    rows[[combined_column]] <- ifelse(
      nzchar(rows[[estimate_column]]) & nzchar(rows$SE),
      paste0(rows[[estimate_column]], "\u00A0\u00B1\u00A0", rows$SE),
      ""
    )
    rows <- rows[, c("Variable", "Label", combined_column, "F", "p", "Effect size"), drop = FALSE]
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
    if (any(!is.finite(adjusted$Estimate))) {
      rows[["post-hoc"]] <- ""
    } else if (isTRUE(options$ordered_significance) && !is.null(p_matrix)) {
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
  if (nzchar(adjusted_warning %||% "")) {
    attr(rows, "warning") <- adjusted_warning
  }
  rows
}
ancova_normality_test <- function(values, method = "lillie") {
  residuals <- values[is.finite(values)]
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
  normality <- ancova_normality_test(residuals, options$normality_method %||% "lillie")
  outcome_normality <- ancova_normality_test(clean_data[[dependent]], options$normality_method %||% "lillie")
  homogeneity <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
  linearity <- ancova_linearity_diagnostics(clean_data, dependent, factor, covariates)
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
    outcome_normality_method = outcome_normality$method %||% "Lilliefors (K-S)",
    outcome_normality_p = outcome_normality$p.value %||% NA_real_,
    homogeneity_p = if (!is.null(homogeneity)) homogeneity$p.value else NA_real_,
    slope_p = interaction_p,
    linearity_p = linearity$min_p,
    linearity_summary = linearity$summary,
    linearity_flagged = linearity$flagged,
    linearity_table = linearity$rows
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
  sum_of_squares <- ancova_sum_of_squares_type(options, method)
  fit_data <- clean
  interaction <- identical(method, "Interaction ANCOVA")
  if (identical(method, "Ranked ANCOVA")) {
    fit_data <- ancova_rank_data(clean, c(dependent, covariates))
  }
  model <- ancova_fit_lm(ancova_formula(dependent, factor, covariates, interaction = interaction), fit_data, sum_of_squares)
  table <- ancova_result_table(model, fit_data, dependent, factor, covariates, method, variable_info, labels, category_table, options)
  interaction_terms <- if (identical(method, "Interaction ANCOVA")) ancova_interaction_terms_table(model, sum_of_squares) else data.frame()
  simple_effects <- if (identical(method, "Interaction ANCOVA")) ancova_simple_effects_table(model, fit_data, factor, covariates) else data.frame()
  collinearity <- ancova_collinearity_diagnostics(model)
  influence <- ancova_influence_diagnostics(model)
  adjusted_mean_warning <- attr(table, "warning", exact = TRUE)
  posthoc_note <- if (nlevels(fit_data[[factor]]) >= 3L && nzchar(adjusted_mean_warning %||% "")) {
    "Post-hoc comparisons were omitted because at least one adjusted mean was not estimable."
  } else if (nlevels(fit_data[[factor]]) >= 3L) {
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
    contrast_target <- if (identical(method, "Ranked ANCOVA")) "adjusted rank means" else "adjusted means"
    if (isTRUE(options$ordered_significance)) {
      sprintf("Post-hoc: %s pairwise model contrasts of %s, %s; displayed with ordered significance notation.", adjustment_note, contrast_target, covariance_note)
    } else {
      sprintf("Post-hoc: %s pairwise model contrasts of %s, %s; displayed with compact letter notation.", adjustment_note, contrast_target, covariance_note)
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
    sum_of_squares = sum_of_squares,
    reason = ancova_method_reason(method, assumptions),
    assumptions = assumptions,
    table = table,
    interaction_terms = interaction_terms,
    simple_effects = simple_effects,
    collinearity = collinearity,
    influence = influence,
    model = model,
    clean_data = clean,
    fit_data = fit_data,
    options = options,
    note = paste(
      "Analysis method:",
      method,
      "Effect size = partial eta squared.",
      ancova_sum_of_squares_note(sum_of_squares),
      posthoc_note,
      if (identical(method, "Robust ANCOVA (HC3)") && identical(sum_of_squares, "type1")) "Group effect F and p use an HC3 robust coefficient Wald test; Type I sequential sums of squares are not used for the robust group test." else NULL,
      if (identical(method, "Robust ANCOVA (HC3)") && !identical(sum_of_squares, "type1")) "Group effect F and p use HC3 robust covariance with the selected Type II/III test." else NULL,
      if (identical(method, "Ranked ANCOVA")) "Ranked ANCOVA uses rank-transformed dependent variable and continuous covariates; adjusted means are reported on the rank scale, not the original outcome scale." else NULL,
      if (identical(method, "Interaction ANCOVA")) "Group x covariate interaction was detected. Interpret group differences conditionally across covariate values rather than as a single adjusted mean difference." else NULL,
      if (identical(method, "Interaction ANCOVA") && is.data.frame(simple_effects) && nrow(simple_effects) > 0) "Simple group effects are reported at covariate mean and mean +/- 1 SD." else NULL,
      if (identical(method, "Interaction ANCOVA") && !identical(sum_of_squares, "type3")) "Type III SS is recommended for interaction models." else NULL,
      if (length(assumptions$linearity_flagged %||% character(0)) > 0) paste("Possible covariate-outcome nonlinearity detected:", paste(assumptions$linearity_flagged, collapse = ", "), ".") else NULL,
      if (isTRUE(collinearity$rank_deficient)) "Model matrix is rank deficient; one or more coefficients may be aliased because of perfect multicollinearity." else NULL,
      if (!isTRUE(collinearity$rank_deficient) && is.finite(collinearity$max_vif) && collinearity$max_vif > 10) sprintf("Multicollinearity warning: VIF exceeds 10 (max VIF = %s).", format_decimal3(collinearity$max_vif)) else NULL,
      if (!isTRUE(collinearity$rank_deficient) && is.finite(collinearity$max_vif) && collinearity$max_vif > 5 && collinearity$max_vif <= 10) sprintf("Multicollinearity caution: VIF exceeds 5 (max VIF = %s).", format_decimal3(collinearity$max_vif)) else NULL,
      if (is.finite(influence$flagged_n) && influence$flagged_n > 0) sprintf("Influence diagnostics flagged %d case(s) by studentized residual, leverage, or Cook's D.", influence$flagged_n) else NULL,
      adjusted_mean_warning
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
