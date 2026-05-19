# Logistic regression analysis helpers.

logistic_reference_values_static <- function(category_table) {
  regression_reference_values_static(category_table)
}

compact_analysis_blocks <- function(block1, block2 = character(0), block3 = character(0)) {
  blocks <- list(
    unique(as.character(block1 %||% character(0))),
    unique(as.character(block2 %||% character(0))),
    unique(as.character(block3 %||% character(0)))
  )
  compacted <- Filter(function(block) length(block) > 0, blocks)
  compacted <- c(compacted, rep(list(character(0)), 3L - length(compacted)))
  list(block1 = compacted[[1]], block2 = compacted[[2]], block3 = compacted[[3]])
}

logistic_measurement_for <- function(name, variable_info = NULL) {
  info <- normalize_regression_variable_info_static(variable_info)
  if (is.null(info) || !name %in% info$name) {
    return("")
  }
  measurement <- tolower(as.character(info$measurement[match(name, info$name)] %||% ""))
  if (identical(measurement, "ordinal")) measurement <- "ordered"
  if (identical(measurement, "nominal")) measurement <- "category"
  measurement
}

logistic_prepare_data <- function(data, variables, variable_info = NULL, reference_values = character(0)) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  raw_n <- nrow(data)
  prepared <- prepare_regression_model_data_static(
    data[, variables, drop = FALSE],
    variables,
    variable_info = variable_info,
    reference_values = reference_values
  )
  complete <- stats::complete.cases(prepared)
  list(data = prepared[complete, , drop = FALSE], n = sum(complete), excluded = raw_n - sum(complete))
}

logistic_auto_reference_notes <- function(data, variables, variable_info = NULL, reference_values = character(0)) {
  info <- normalize_regression_variable_info_static(variable_info)
  if (is.null(info)) return(list(reference_values = reference_values, notes = character(0)))
  notes <- character(0)
  variables <- intersect(as.character(variables), as.character(info$name))
  for (name in variables) {
    measurement <- logistic_measurement_for(name, info)
    if (!measurement %in% c("binary", "category")) next
    if (nzchar(trimws(named_value(reference_values, name, "")))) next
    values <- sort(unique(stats::na.omit(as.character(data[[name]]))))
    if (length(values) == 0) next
    reference_values <- c(reference_values, stats::setNames(values[[1]], name))
    notes <- c(notes, sprintf("Reference for %s was not set; minimum value %s was used.", name, values[[1]]))
  }
  list(reference_values = reference_values, notes = notes)
}

logistic_sparse_cell_check <- function(data, dependent, predictors, variable_info = NULL) {
  info <- normalize_regression_variable_info_static(variable_info)
  zero_notes <- character(0)
  sparse_notes <- character(0)
  if (is.null(info)) return(list(exclude = FALSE, notes = character(0), warnings = character(0)))
  for (predictor in predictors) {
    measurement <- logistic_measurement_for(predictor, info)
    if (!measurement %in% c("binary", "ordered", "category")) next
    tab <- table(data[[dependent]], data[[predictor]], useNA = "no")
    if (length(tab) == 0) next
    if (any(tab == 0)) {
      zero_notes <- c(zero_notes, sprintf("Zero cell found for %s by %s.", dependent, predictor))
    } else if (mean(tab < 5) >= .2) {
      sparse_notes <- c(sparse_notes, sprintf("Sparse cells found for %s by %s.", dependent, predictor))
    }
  }
  list(exclude = length(zero_notes) > 0, notes = zero_notes, warnings = sparse_notes)
}

logistic_pseudo_r2 <- function(model, null_model, n) {
  ll_full <- as.numeric(stats::logLik(model))
  ll_null <- as.numeric(stats::logLik(null_model))
  cox <- 1 - exp((2 / n) * (ll_null - ll_full))
  max_cox <- 1 - exp((2 / n) * ll_null)
  nagelkerke <- if (max_cox <= 0) NA_real_ else cox / max_cox
  mcfadden <- if (ll_null == 0) NA_real_ else 1 - (ll_full / ll_null)
  c(nagelkerke = nagelkerke, cox_snell = cox, mcfadden = mcfadden)
}

logistic_fit_stats <- function(model, null_model, n) {
  ll_full <- as.numeric(stats::logLik(model))
  ll_null <- as.numeric(stats::logLik(null_model))
  df <- attr(stats::logLik(model), "df") - attr(stats::logLik(null_model), "df")
  chisq <- 2 * (ll_full - ll_null)
  list(
    chisq = chisq,
    df = df,
    p = if (df > 0) stats::pchisq(chisq, df = df, lower.tail = FALSE) else NA_real_,
    r2 = logistic_pseudo_r2(model, null_model, n),
    aic = stats::AIC(model),
    bic = stats::BIC(model)
  )
}

logistic_vif_summary <- function(formula, data) {
  mm <- tryCatch(stats::model.matrix(formula, data = data), error = function(e) NULL)
  if (is.null(mm)) return(NA_real_)
  vif <- coefficient_collinearity(mm)$vif
  vif <- vif[is.finite(vif)]
  if (length(vif) == 0) NA_real_ else max(vif, na.rm = TRUE)
}

logistic_vif_by_predictor <- function(formula, data, predictors) {
  mm <- tryCatch(stats::model.matrix(formula, data = data), error = function(e) NULL)
  predictors <- as.character(predictors %||% character(0))
  values <- stats::setNames(rep(NA_real_, length(predictors)), predictors)
  if (is.null(mm) || length(predictors) == 0) {
    return(values)
  }
  vif <- coefficient_collinearity(mm)$vif
  vif <- vif[setdiff(names(vif), "(Intercept)")]
  if (length(vif) == 0) {
    return(values)
  }
  for (predictor in predictors) {
    prefixes <- unique(c(predictor, make.names(predictor)))
    matched <- unlist(lapply(prefixes, function(prefix) {
      names(vif)[names(vif) == prefix | startsWith(names(vif), prefix)]
    }), use.names = FALSE)
    matched <- unique(matched)
    matched <- matched[is.finite(vif[matched])]
    if (length(matched) > 0) {
      values[[predictor]] <- max(vif[matched], na.rm = TRUE)
    }
  }
  values
}

logistic_epv_warning <- function(y, coefficients) {
  tab <- table(y)
  events <- min(tab)
  epv <- events / max(1, coefficients)
  if (epv < 5) {
    sprintf("EPV is %.1f (<5); estimates may be unstable.", epv)
  } else if (epv < 10) {
    sprintf("EPV is %.1f (<10); interpret estimates cautiously.", epv)
  } else {
    NA_character_
  }
}

logistic_binary_coef_table <- function(model) {
  coef <- summary(model)$coefficients
  data.frame(
    Outcome = "",
    Term = rownames(coef),
    B = coef[, 1],
    SE = coef[, 2],
    p = coef[, 4],
    OR = exp(coef[, 1]),
    LLCI = exp(coef[, 1] - 1.96 * coef[, 2]),
    ULCI = exp(coef[, 1] + 1.96 * coef[, 2]),
    row.names = NULL,
    check.names = FALSE
  )
}

logistic_polr_coef_table <- function(model) {
  coef <- coef(summary(model))
  term_names <- names(stats::coef(model))
  coef <- coef[term_names, , drop = FALSE]
  p <- 2 * stats::pnorm(abs(coef[, "t value"]), lower.tail = FALSE)
  data.frame(
    Outcome = "",
    Term = rownames(coef),
    B = coef[, "Value"],
    SE = coef[, "Std. Error"],
    p = p,
    OR = exp(coef[, "Value"]),
    LLCI = exp(coef[, "Value"] - 1.96 * coef[, "Std. Error"]),
    ULCI = exp(coef[, "Value"] + 1.96 * coef[, "Std. Error"]),
    row.names = NULL,
    check.names = FALSE
  )
}

logistic_multinom_coef_table <- function(model) {
  sm <- summary(model)
  coef <- sm$coefficients
  se <- sm$standard.errors
  if (is.null(dim(coef))) {
    coef <- matrix(coef, nrow = 1, dimnames = list(names(coef)[[1]] %||% "", names(coef)))
    se <- matrix(se, nrow = 1, dimnames = dimnames(coef))
  }
  rows <- do.call(rbind, lapply(seq_len(nrow(coef)), function(index) {
    z <- coef[index, ] / se[index, ]
    data.frame(
      Outcome = rownames(coef)[[index]],
      Term = colnames(coef),
      B = coef[index, ],
      SE = se[index, ],
      p = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
      OR = exp(coef[index, ]),
      LLCI = exp(coef[index, ] - 1.96 * se[index, ]),
      ULCI = exp(coef[index, ] + 1.96 * se[index, ]),
      row.names = NULL,
      check.names = FALSE
    )
  }))
  rows
}

fit_logistic_model <- function(data, dependent, predictors, measurement) {
  model_data <- data
  form <- make_formula(dependent, predictors)
  null_formula <- stats::reformulate("1", response = dependent)
  if (identical(measurement, "binary")) {
    model <- stats::glm(form, data = model_data, family = stats::binomial())
    null_model <- stats::glm(null_formula, data = model_data, family = stats::binomial())
    model$call$formula <- form
    null_model$call$formula <- null_formula
    model$call$data <- model_data
    null_model$call$data <- model_data
    return(list(model = model, null_model = null_model, method = "Binary logistic regression", coef_table = logistic_binary_coef_table(model), parallel = NULL))
  }
  if (identical(measurement, "ordered")) {
    ordinal <- MASS::polr(form, data = model_data, Hess = TRUE)
    ordinal_null <- MASS::polr(null_formula, data = model_data, Hess = TRUE)
    multi <- nnet::multinom(form, data = model_data, trace = FALSE)
    multi_null <- nnet::multinom(null_formula, data = model_data, trace = FALSE)
    ordinal$call$formula <- form
    ordinal_null$call$formula <- null_formula
    multi$call$formula <- form
    multi_null$call$formula <- null_formula
    ordinal$call$data <- model_data
    ordinal_null$call$data <- model_data
    multi$call$data <- model_data
    multi_null$call$data <- model_data
    chisq <- 2 * (as.numeric(stats::logLik(multi)) - as.numeric(stats::logLik(ordinal)))
    df <- attr(stats::logLik(multi), "df") - attr(stats::logLik(ordinal), "df")
    p <- if (df > 0) stats::pchisq(chisq, df = df, lower.tail = FALSE) else NA_real_
    parallel <- list(chisq = chisq, df = df, p = p)
    if (!is.na(p) && p <= .05) {
      return(list(model = multi, null_model = multi_null, method = "Multinomial logistic regression", coef_table = logistic_multinom_coef_table(multi), parallel = parallel, ordinal_fallback = TRUE))
    }
    return(list(model = ordinal, null_model = ordinal_null, method = "Ordinal logistic regression", coef_table = logistic_polr_coef_table(ordinal), parallel = parallel, ordinal_fallback = FALSE))
  }
  model <- nnet::multinom(form, data = model_data, trace = FALSE)
  null_model <- nnet::multinom(null_formula, data = model_data, trace = FALSE)
  model$call$formula <- form
  null_model$call$formula <- null_formula
  model$call$data <- model_data
  null_model$call$data <- model_data
  list(model = model, null_model = null_model, method = "Multinomial logistic regression", coef_table = logistic_multinom_coef_table(model), parallel = NULL)
}

prepare_logistic_analysis_results <- function(
  data,
  dependents,
  block1,
  block2 = character(0),
  block3 = character(0),
  variable_info = NULL,
  reference_values = character(0)
) {
  data_names <- names(data)
  dependents <- intersect(unique(as.character(dependents %||% character(0))), data_names)
  block1 <- intersect(unique(as.character(block1 %||% character(0))), data_names)
  block2 <- intersect(unique(as.character(block2 %||% character(0))), data_names)
  block3 <- intersect(unique(as.character(block3 %||% character(0))), data_names)
  compacted <- compact_analysis_blocks(block1, block2, block3)
  block1 <- compacted$block1
  block2 <- compacted$block2
  block3 <- compacted$block3
  shiny::validate(shiny::need(length(dependents) > 0, "Select at least one dependent variable."))
  shiny::validate(shiny::need(length(block1) > 0, "Select at least one Block 1 variable."))

  steps <- list(list(name = "Model 1", predictors = block1, blocks = "Block 1"))
  if (length(block2) > 0) steps <- c(steps, list(list(name = "Model 2", predictors = unique(c(block1, block2)), blocks = "Block 1 + Block 2")))
  if (length(block3) > 0) steps <- c(steps, list(list(name = "Model 3", predictors = unique(c(block1, block2, block3)), blocks = "Block 1 + Block 2 + Block 3")))

  refs <- logistic_auto_reference_notes(data, unique(c(dependents, block1, block2, block3)), variable_info, reference_values)
  reference_values <- refs$reference_values
  results <- list()
  for (dependent in dependents) {
    measurement <- logistic_measurement_for(dependent, variable_info)
    if (!measurement %in% logistic_dependent_measurements()) next
    previous <- NULL
    for (step_index in seq_along(steps)) {
      predictors <- setdiff(steps[[step_index]]$predictors, dependent)
      if (length(predictors) == 0) next
      prep <- logistic_prepare_data(data, unique(c(dependent, predictors)), variable_info, reference_values)
      event_note <- NA_character_
      if (identical(measurement, "binary") && is.factor(prep$data[[dependent]]) && length(levels(prep$data[[dependent]])) >= 2) {
        event_note <- sprintf("Binary event for %s is %s; reference is %s.", dependent, levels(prep$data[[dependent]])[[2]], levels(prep$data[[dependent]])[[1]])
      }
      sparse <- logistic_sparse_cell_check(prep$data, dependent, predictors, variable_info)
      if (isTRUE(sparse$exclude)) {
        results[[length(results) + 1L]] <- list(
          dependent = dependent,
          predictors = predictors,
          formula = make_formula(dependent, predictors),
          method = "Excluded",
          excluded = TRUE,
          notes = unique(c(refs$notes, event_note, sparse$notes)),
          hierarchical_step = steps[[step_index]]$name,
          hierarchical_step_index = step_index,
          hierarchical_blocks = steps[[step_index]]$blocks
        )
        next
      }
      fit <- tryCatch(fit_logistic_model(prep$data, dependent, predictors, measurement), error = function(e) e)
      if (inherits(fit, "error")) {
        results[[length(results) + 1L]] <- list(
          dependent = dependent,
          predictors = predictors,
          formula = make_formula(dependent, predictors),
          method = "Failed",
          excluded = TRUE,
          notes = unique(c(refs$notes, event_note, sparse$warnings, conditionMessage(fit))),
          hierarchical_step = steps[[step_index]]$name,
          hierarchical_step_index = step_index,
          hierarchical_blocks = steps[[step_index]]$blocks
        )
        next
      }
      stats <- logistic_fit_stats(fit$model, fit$null_model, prep$n)
      coef_count <- nrow(fit$coef_table)
      epv_note <- logistic_epv_warning(prep$data[[dependent]], coef_count)
      result <- list(
        dependent = dependent,
        predictors = predictors,
        dependent_levels = if (is.factor(prep$data[[dependent]])) levels(prep$data[[dependent]]) else character(0),
        predictor_levels = lapply(stats::setNames(predictors, predictors), function(name) {
          if (is.factor(prep$data[[name]])) levels(prep$data[[name]]) else character(0)
        }),
        formula = make_formula(dependent, predictors),
        n = prep$n,
        missing_excluded = prep$excluded,
        method = fit$method,
        coef_table = fit$coef_table,
        fit = stats,
        max_vif = logistic_vif_summary(make_formula(dependent, predictors), prep$data),
        predictor_vif = logistic_vif_by_predictor(make_formula(dependent, predictors), prep$data, predictors),
        parallel = fit$parallel,
        ordinal_fallback = isTRUE(fit$ordinal_fallback),
        notes = unique(stats::na.omit(c(refs$notes, event_note, sparse$warnings, epv_note))),
        hierarchical_step = steps[[step_index]]$name,
        hierarchical_step_index = step_index,
        hierarchical_blocks = steps[[step_index]]$blocks
      )
      if (!is.null(previous)) {
        result$delta_r2 <- stats$r2[["nagelkerke"]] - previous$fit$r2[["nagelkerke"]]
        result$delta_chisq <- stats$chisq - previous$fit$chisq
        result$delta_df <- stats$df - previous$fit$df
        result$delta_p <- if (result$delta_df > 0) stats::pchisq(result$delta_chisq, result$delta_df, lower.tail = FALSE) else NA_real_
      }
      previous <- result
      results[[length(results) + 1L]] <- result
    }
  }
  shiny::validate(shiny::need(length(results) > 0, "No logistic regression model could be prepared."))
  results
}
