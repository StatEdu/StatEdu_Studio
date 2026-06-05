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

ancova_influence_sensitivity <- function(model, clean_data, fit_data, dependent, factor, covariates, method, sum_of_squares, influence) {
  if (!is.data.frame(influence$table) || nrow(influence$table) == 0 || !"Flag" %in% names(influence$table)) {
    return(data.frame())
  }
  flagged_cases <- as.character(influence$table$Case[nzchar(as.character(influence$table$Flag))])
  if (length(flagged_cases) == 0) {
    return(data.frame())
  }
  full_stat <- ancova_group_stat(model, method, factor, sum_of_squares)
  row_ids <- rownames(fit_data)
  if (is.null(row_ids) || length(row_ids) != nrow(fit_data)) {
    row_ids <- as.character(seq_len(nrow(fit_data)))
  }
  keep <- !row_ids %in% flagged_cases
  reduced_clean <- clean_data[keep, , drop = FALSE]
  if (nrow(reduced_clean) < 4L || length(unique(stats::na.omit(reduced_clean[[factor]]))) < 2L) {
    return(data.frame(
      Model = "Full model",
      N = stats::nobs(model),
      `Excluded flagged cases` = 0L,
      F = full_stat$f,
      df1 = full_stat$df1,
      df2 = full_stat$df2,
      p = full_stat$p,
      `partial eta2` = full_stat$effect_size,
      Note = "Sensitivity model not estimable after excluding flagged cases.",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  reduced_fit_data <- reduced_clean
  if (identical(method, "Ranked ANCOVA")) {
    reduced_fit_data <- ancova_rank_data(reduced_clean, c(dependent, covariates))
  }
  interaction <- identical(method, "Interaction ANCOVA")
  reduced_model <- tryCatch(
    ancova_fit_lm(ancova_formula(dependent, factor, covariates, interaction = interaction), reduced_fit_data, sum_of_squares),
    error = function(e) NULL
  )
  if (is.null(reduced_model)) {
    return(data.frame(
      Model = "Full model",
      N = stats::nobs(model),
      `Excluded flagged cases` = 0L,
      F = full_stat$f,
      df1 = full_stat$df1,
      df2 = full_stat$df2,
      p = full_stat$p,
      `partial eta2` = full_stat$effect_size,
      Note = "Sensitivity model not estimable after excluding flagged cases.",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  reduced_stat <- ancova_group_stat(reduced_model, method, factor, sum_of_squares)
  data.frame(
    Model = c("Full model", "Excluding flagged cases"),
    N = c(stats::nobs(model), stats::nobs(reduced_model)),
    `Excluded flagged cases` = c(0L, sum(!keep)),
    F = c(full_stat$f, reduced_stat$f),
    df1 = c(full_stat$df1, reduced_stat$df1),
    df2 = c(full_stat$df2, reduced_stat$df2),
    p = c(full_stat$p, reduced_stat$p),
    `partial eta2` = c(full_stat$effect_size, reduced_stat$effect_size),
    Note = c("", "Exploratory sensitivity analysis; primary result remains the full model."),
    check.names = FALSE,
    stringsAsFactors = FALSE
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
      rows <- analysis_apply_ordered_posthoc_markers(
        rows,
        estimates = adjusted$Estimate,
        levels = adjusted$Level,
        p_matrix = p_matrix,
        label_column = "Label"
      )
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

ancova_original_scale_display_table <- function(rank_table, original_model, clean_data, factor, covariates) {
  if (!is.data.frame(rank_table) || nrow(rank_table) == 0) return(rank_table)
  adjusted <- ancova_adjusted_means(original_model, clean_data, factor, covariates)
  estimate_values <- ancova_format_decimal3_vec(adjusted$Estimate)
  se_values <- ancova_format_decimal3_vec(adjusted$SE)
  out <- rank_table
  if ("Adjusted rank mean" %in% names(out)) {
    out[["Adjusted rank mean"]] <- estimate_values
    names(out)[names(out) == "Adjusted rank mean"] <- "Adjusted mean"
  } else if ("Rank M \u00B1 SE" %in% names(out)) {
    out[["Rank M \u00B1 SE"]] <- ifelse(
      nzchar(estimate_values) & nzchar(se_values),
      paste0(estimate_values, "\u00A0\u00B1\u00A0", se_values),
      ""
    )
    names(out)[names(out) == "Rank M \u00B1 SE"] <- "M \u00B1 SE"
  }
  if ("SE" %in% names(out)) {
    out$SE <- se_values
  }
  out
}
ancova_min_group_n <- function(clean_data, factor) {
  if (is.null(clean_data) || !factor %in% names(clean_data)) return(NA_integer_)
  counts <- table(clean_data[[factor]], useNA = "no")
  if (length(counts) == 0L) return(NA_integer_)
  suppressWarnings(as.integer(min(counts)))
}

ancova_resolve_normality_method <- function(method = "auto", min_group_n = NA_integer_) {
  method <- as.character(method %||% "auto")
  if (!method %in% c("auto", "lillie", "shapiro")) method <- "auto"
  if (identical(method, "auto")) {
    if (is.finite(min_group_n) && min_group_n <= 50L) "shapiro" else "lillie"
  } else {
    method
  }
}

ancova_normality_test <- function(values, method = "auto", min_group_n = NA_integer_) {
  residuals <- values[is.finite(values)]
  method <- ancova_resolve_normality_method(method, min_group_n)
  if (identical(method, "shapiro") && length(residuals) >= 3L && length(residuals) <= 5000L) {
    test <- tryCatch(stats::shapiro.test(residuals), error = function(e) NULL)
    return(list(method = "Shapiro-Wilk", p.value = if (!is.null(test)) test$p.value else NA_real_))
  }
  test <- tryCatch(nortest::lillie.test(residuals), error = function(e) NULL)
  list(method = "Lilliefors (K-S)", p.value = if (!is.null(test)) test$p.value else NA_real_)
}

ancova_homogeneity_method_label <- function(method) {
  switch(
    as.character(method %||% "levene"),
    brown_forsythe = "Brown-Forsythe",
    breusch_pagan = "Breusch-Pagan",
    white = "White test",
    "Levene"
  )
}

ancova_white_test <- function(model) {
  residuals <- stats::residuals(model)
  model_matrix <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  if (is.null(model_matrix) || length(residuals) != nrow(model_matrix)) return(NA_real_)
  keep_rows <- is.finite(residuals) & apply(model_matrix, 1L, function(row) all(is.finite(row)))
  residuals <- residuals[keep_rows]
  model_matrix <- model_matrix[keep_rows, , drop = FALSE]
  if (length(residuals) < 4L || ncol(model_matrix) < 2L) return(NA_real_)

  predictor_names <- setdiff(colnames(model_matrix), "(Intercept)")
  predictors <- model_matrix[, predictor_names, drop = FALSE]
  if (ncol(predictors) == 0L) return(NA_real_)

  auxiliary <- predictors
  for (i in seq_len(ncol(predictors))) {
    for (j in i:ncol(predictors)) {
      auxiliary <- cbind(auxiliary, predictors[, i] * predictors[, j])
    }
  }
  auxiliary <- auxiliary[, apply(auxiliary, 2L, function(column) {
    all(is.finite(column)) && stats::var(column) > 0
  }), drop = FALSE]
  if (ncol(auxiliary) == 0L) return(NA_real_)

  qr_auxiliary <- qr(auxiliary)
  rank <- qr_auxiliary$rank
  if (rank < 1L || length(residuals) <= rank + 1L) return(NA_real_)
  auxiliary <- auxiliary[, qr_auxiliary$pivot[seq_len(rank)], drop = FALSE]
  colnames(auxiliary) <- paste0("x", seq_len(ncol(auxiliary)))

  white_data <- data.frame(squared_residual = residuals^2, auxiliary, check.names = FALSE)
  test_model <- tryCatch(stats::lm(squared_residual ~ ., data = white_data), error = function(e) NULL)
  if (is.null(test_model)) return(NA_real_)
  r_squared <- tryCatch(summary(test_model)$r.squared, error = function(e) NA_real_)
  if (!is.finite(r_squared)) return(NA_real_)
  statistic <- length(residuals) * r_squared
  stats::pchisq(statistic, df = rank, lower.tail = FALSE)
}

ancova_homogeneity_test <- function(model, clean_data, factor, method = "levene") {
  method <- as.character(method %||% "levene")
  if (!method %in% c("levene", "brown_forsythe", "breusch_pagan", "white")) method <- "levene"
  label <- ancova_homogeneity_method_label(method)
  if (identical(method, "breusch_pagan")) {
    test <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
    return(list(method = label, p.value = if (!is.null(test)) test$p.value else NA_real_))
  }
  if (identical(method, "white")) {
    return(list(method = label, p.value = ancova_white_test(model)))
  }
  residual_data <- data.frame(
    residual = stats::residuals(model),
    group = clean_data[[factor]]
  )
  center <- if (identical(method, "brown_forsythe")) median else mean
  test <- tryCatch(suppressWarnings(car::leveneTest(residual ~ group, data = residual_data, center = center)), error = function(e) NULL)
  p_value <- if (!is.null(test) && "Pr(>F)" %in% names(test) && nrow(test) > 0L) {
    suppressWarnings(as.numeric(test[["Pr(>F)"]][[1]]))
  } else {
    NA_real_
  }
  list(method = label, p.value = p_value)
}

ancova_slope_homogeneity_diagnostics <- function(clean_data, dependent, factor, covariates) {
  if (length(covariates) == 0) {
    return(list(rows = data.frame(), min_p = NA_real_, summary = "No covariates"))
  }
  interaction_model <- tryCatch(stats::lm(ancova_formula(dependent, factor, covariates, interaction = TRUE), data = clean_data), error = function(e) NULL)
  if (is.null(interaction_model)) {
    return(list(rows = data.frame(), min_p = NA_real_, summary = "Not testable"))
  }
  anova_table <- tryCatch(stats::anova(interaction_model), error = function(e) NULL)
  if (is.null(anova_table) || !"Pr(>F)" %in% names(anova_table)) {
    return(list(rows = data.frame(), min_p = NA_real_, summary = "Not testable"))
  }
  interaction_rows <- grepl(":", rownames(anova_table), fixed = TRUE)
  if (!any(interaction_rows)) {
    return(list(rows = data.frame(), min_p = NA_real_, summary = "No interaction terms"))
  }
  rows <- data.frame(
    Term = rownames(anova_table)[interaction_rows],
    df = suppressWarnings(as.numeric(anova_table[["Df"]][interaction_rows])),
    F = suppressWarnings(as.numeric(anova_table[["F value"]][interaction_rows])),
    p = suppressWarnings(as.numeric(anova_table[["Pr(>F)"]][interaction_rows])),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rows$Status <- ifelse(is.finite(rows$p) & rows$p < 0.05, "slope heterogeneity", ifelse(is.finite(rows$p), "no clear slope heterogeneity", "not testable"))
  min_p <- if (any(is.finite(rows$p))) min(rows$p[is.finite(rows$p)]) else NA_real_
  summary <- if (is.finite(min_p) && min_p < 0.05) {
    "Regression slopes differ by group"
  } else if (is.finite(min_p)) {
    "No clear slope heterogeneity"
  } else {
    "Not testable"
  }
  list(rows = rows, min_p = min_p, summary = summary)
}

ancova_decision_alpha <- function(options = list()) {
  alpha <- suppressWarnings(as.numeric(options$decision_alpha %||% 0.05))
  if (!is.finite(alpha) || alpha < 0.01 || alpha > 0.20) alpha <- 0.05
  alpha
}

ancova_assumption_flags <- function(assumptions, options = list()) {
  alpha <- ancova_decision_alpha(options)
  list(
    slope = is.finite(assumptions$slope_p) && assumptions$slope_p < alpha,
    normality = !identical(options$normality_enabled, FALSE) && is.finite(assumptions$normality_p) && assumptions$normality_p < alpha,
    homogeneity = is.finite(assumptions$homogeneity_p) && assumptions$homogeneity_p < alpha
  )
}

ancova_assumptions <- function(model, clean_data, dependent, factor, covariates, options = list()) {
  residuals <- stats::residuals(model)
  min_group_n <- ancova_min_group_n(clean_data, factor)
  normality <- ancova_normality_test(residuals, options$normality_method %||% "auto", min_group_n)
  outcome_normality <- ancova_normality_test(clean_data[[dependent]], options$normality_method %||% "auto", min_group_n)
  homogeneity <- ancova_homogeneity_test(model, clean_data, factor, options$homogeneity_method %||% "levene")
  linearity <- ancova_linearity_diagnostics(clean_data, dependent, factor, covariates)
  slope <- ancova_slope_homogeneity_diagnostics(clean_data, dependent, factor, covariates)
  list(
    normality_method = normality$method %||% "Lilliefors (K-S)",
    normality_p = normality$p.value %||% NA_real_,
    outcome_normality_method = outcome_normality$method %||% "Lilliefors (K-S)",
    outcome_normality_p = outcome_normality$p.value %||% NA_real_,
    homogeneity_method = homogeneity$method %||% "Levene",
    homogeneity_p = homogeneity$p.value %||% NA_real_,
    slope_p = slope$min_p,
    slope_summary = slope$summary,
    slope_table = slope$rows,
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
  auto_method <- as.character(options$auto_method %||% "auto")
  if (!auto_method %in% c("auto", "warn")) auto_method <- "auto"
  if (identical(auto_method, "warn")) {
    return("ANCOVA")
  }
  flags <- ancova_assumption_flags(assumptions, options)
  if (isTRUE(flags$slope)) {
    return("Interaction ANCOVA")
  }
  if (isTRUE(flags$normality)) {
    return("Ranked ANCOVA")
  }
  if (isTRUE(flags$homogeneity)) {
    return("Robust ANCOVA (HC3)")
  }
  "ANCOVA"
}

ancova_method_reason <- function(method, assumptions, options = list()) {
  flags <- ancova_assumption_flags(assumptions, options)
  if (identical(method, "ANCOVA") && identical(as.character(options$auto_method %||% "auto"), "warn") && any(unlist(flags))) {
    return("assumption warning mode selected; standard ANCOVA retained")
  }
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

ancova_assumption_warning_note <- function(assumptions, options = list()) {
  flags <- ancova_assumption_flags(assumptions, options)
  alpha <- ancova_decision_alpha(options)
  warnings <- character(0)
  if (isTRUE(flags$slope)) warnings <- c(warnings, "regression slope homogeneity")
  if (isTRUE(flags$normality)) warnings <- c(warnings, "residual normality")
  if (isTRUE(flags$homogeneity)) warnings <- c(warnings, "homogeneity of variance")
  if (length(warnings) == 0) return(NULL)
  sprintf("Assumption warning: %s flagged at alpha = %s.", paste(warnings, collapse = ", "), format_decimal3(alpha))
}

ancova_complete_case_note <- function(raw_n, complete_n, excluded_n) {
  sprintf(
    "Complete-case analysis: %s of %s cases used; %s case(s) excluded for missing model variables.",
    as.integer(complete_n),
    as.integer(raw_n),
    as.integer(excluded_n)
  )
}

ancova_decision_rule_note <- function(assumptions, options = list()) {
  mode <- if (identical(as.character(options$auto_method %||% "auto"), "warn")) "Warn only" else "Auto switch"
  sprintf(
    "Assumption decision rule: %s at alpha = %s; normality test = %s, homogeneity test = %s.",
    mode,
    format_decimal3(ancova_decision_alpha(options)),
    assumptions$normality_method %||% "Lilliefors (K-S)",
    assumptions$homogeneity_method %||% "Levene"
  )
}

ancova_method_interpretation_note <- function(method, sum_of_squares = "type2", simple_effects = data.frame()) {
  if (identical(method, "Robust ANCOVA (HC3)")) {
    if (identical(sum_of_squares, "type1")) {
      return("Robust ANCOVA (HC3): group effect F and p use an HC3 robust coefficient Wald test; Type I sequential sums of squares are not used for the robust group test.")
    }
    return("Robust ANCOVA (HC3): group effect F, p, and post-hoc contrasts use HC3 robust covariance with the selected Type II/III test.")
  }
  if (identical(method, "Ranked ANCOVA")) {
    return("Ranked ANCOVA: the dependent variable and continuous covariates are rank-transformed; adjusted means and post-hoc contrasts are interpreted on the rank scale, not the original outcome scale.")
  }
  if (identical(method, "Interaction ANCOVA")) {
    extra <- if (is.data.frame(simple_effects) && nrow(simple_effects) > 0) {
      " Simple group effects are reported at covariate mean and mean +/- 1 SD."
    } else {
      ""
    }
    return(paste0("Interaction ANCOVA: group x covariate interaction was detected; interpret group differences conditionally across covariate values rather than as a single adjusted mean difference.", extra))
  }
  "ANCOVA: group differences are reported as covariate-adjusted mean differences under the fitted linear model."
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
  display_table <- if (identical(method, "Ranked ANCOVA")) {
    ancova_original_scale_display_table(table, base_model, clean, factor, covariates)
  } else {
    table
  }
  interaction_terms <- if (identical(method, "Interaction ANCOVA")) ancova_interaction_terms_table(model, sum_of_squares) else data.frame()
  simple_effects <- if (identical(method, "Interaction ANCOVA")) ancova_simple_effects_table(model, fit_data, factor, covariates) else data.frame()
  collinearity <- ancova_collinearity_diagnostics(model)
  influence <- ancova_influence_diagnostics(model)
  influence_sensitivity <- if (isTRUE(options$influence_sensitivity)) {
    ancova_influence_sensitivity(model, clean, fit_data, dependent, factor, covariates, method, sum_of_squares, influence)
  } else {
    data.frame()
  }
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
    raw_n = nrow(data),
    complete_n = nrow(clean),
    excluded_n = max(0L, nrow(data) - nrow(clean)),
    n = stats::nobs(model),
    method = method,
    sum_of_squares = sum_of_squares,
    reason = ancova_method_reason(method, assumptions, options),
    assumptions = assumptions,
    table = table,
    display_table = display_table,
    interaction_terms = interaction_terms,
    simple_effects = simple_effects,
    collinearity = collinearity,
    influence = influence,
    influence_sensitivity = influence_sensitivity,
    model = model,
    clean_data = clean,
    fit_data = fit_data,
    options = options,
    note = paste(
      "Analysis method:",
      method,
      "ES = effect size (partial eta squared).",
      ancova_complete_case_note(nrow(data), nrow(clean), max(0L, nrow(data) - nrow(clean))),
      ancova_decision_rule_note(assumptions, options),
      ancova_sum_of_squares_note(sum_of_squares),
      posthoc_note,
      ancova_method_interpretation_note(method, sum_of_squares, simple_effects),
      if (identical(method, "Interaction ANCOVA") && !identical(sum_of_squares, "type3")) "Type III SS is recommended for interaction models." else NULL,
      if (identical(as.character(options$auto_method %||% "auto"), "warn") && !isTRUE(options$force_ranked)) "Automatic method switching is disabled; assumption checks are reported as warnings while the standard ANCOVA model is retained." else NULL,
      ancova_assumption_warning_note(assumptions, options),
      if (length(assumptions$linearity_flagged %||% character(0)) > 0) paste("Possible covariate-outcome nonlinearity detected:", paste(assumptions$linearity_flagged, collapse = ", "), ".") else NULL,
      if (isTRUE(collinearity$rank_deficient)) "Model matrix is rank deficient; one or more coefficients may be aliased because of perfect multicollinearity." else NULL,
      if (!isTRUE(collinearity$rank_deficient) && is.finite(collinearity$max_vif) && collinearity$max_vif > 10) sprintf("Multicollinearity warning: VIF exceeds 10 (max VIF = %s).", format_decimal3(collinearity$max_vif)) else NULL,
      if (!isTRUE(collinearity$rank_deficient) && is.finite(collinearity$max_vif) && collinearity$max_vif > 5 && collinearity$max_vif <= 10) sprintf("Multicollinearity caution: VIF exceeds 5 (max VIF = %s).", format_decimal3(collinearity$max_vif)) else NULL,
      if (is.finite(influence$flagged_n) && influence$flagged_n > 0) sprintf("Influence diagnostics flagged %d case(s) by studentized residual, leverage, or Cook's D.", influence$flagged_n) else NULL,
      if (isTRUE(options$influence_sensitivity) && is.data.frame(influence_sensitivity) && nrow(influence_sensitivity) > 0) "Influence sensitivity analysis compares the full model with a model excluding flagged cases; use it as a diagnostic, not as automatic case deletion." else NULL,
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
