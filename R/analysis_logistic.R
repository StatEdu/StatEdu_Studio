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

logistic_prepare_data <- function(data, variables, variable_info = NULL, reference_values = character(0), category_table = NULL) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  raw_n <- nrow(data)
  prepared <- prepare_regression_model_data_static(
    data[, variables, drop = FALSE],
    variables,
    variable_info = variable_info,
    reference_values = reference_values
  )
  ordered_lookup <- category_value_label_lookup_static(category_table)
  for (name in variables) {
    if (!identical(logistic_measurement_for(name, variable_info), "ordered")) next
    declared_levels <- names(ordered_lookup[[name]] %||% character(0))
    declared_levels <- declared_levels[nzchar(declared_levels)]
    if (length(declared_levels) == 0L) next
    observed <- unique(as.character(prepared[[name]]))
    observed <- observed[!is.na(observed) & nzchar(observed)]
    prepared[[name]] <- ordered(as.character(prepared[[name]]), levels = unique(c(declared_levels, setdiff(observed, declared_levels))))
  }
  complete <- stats::complete.cases(prepared)
  analysis_data <- droplevels(prepared[complete, , drop = FALSE])
  list(data = analysis_data, n = sum(complete), excluded = raw_n - sum(complete))
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
      zero_notes <- c(zero_notes, sprintf("Zero cell found for %s by %s; separation is possible.", dependent, predictor))
    } else if (mean(tab < 5) >= .2) {
      sparse_notes <- c(sparse_notes, sprintf("Sparse cells found for %s by %s.", dependent, predictor))
    }
  }
  list(exclude = FALSE, notes = character(0), warnings = c(zero_notes, sparse_notes))
}

logistic_guard_row <- function(dependent, predictors, reason, n = NA_integer_, variable_info = NULL, type = "Skipped") {
  data.frame(
    Type = type,
    `Dependent variable` = display_variable_name_static(dependent, variable_info, character(0), label_only = TRUE),
    `Independent variables` = paste(vapply(predictors, display_variable_name_static, character(1), table = variable_info, labels = character(0), label_only = TRUE), collapse = ", "),
    N = if (is.na(n)) "" else as.character(n),
    Message = reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

logistic_bind_guard_rows <- function(rows) {
  analysis_bind_rows(rows)
}

logistic_constant_predictors <- function(data, predictors) {
  predictors <- intersect(as.character(predictors %||% character(0)), names(data))
  predictors[vapply(predictors, function(name) {
    values <- data[[name]]
    if (is.factor(values)) {
      values <- droplevels(values)
      return(nlevels(values) < 2)
    }
    values <- values[!is.na(values)]
    length(unique(values)) < 2
  }, logical(1))]
}

logistic_preflight <- function(data, dependent, predictors, measurement, variable_info = NULL) {
  n <- nrow(data)
  if (n < 3) {
    return(list(ok = FALSE, skipped = logistic_guard_row(dependent, predictors, "At least 3 complete cases are required.", n, variable_info)))
  }
  y <- data[[dependent]]
  y_levels <- if (is.factor(y)) levels(droplevels(y)) else unique(stats::na.omit(y))
  if (length(y_levels) < 2) {
    return(list(ok = FALSE, skipped = logistic_guard_row(dependent, predictors, "The dependent variable has fewer than two observed outcome levels after complete-case filtering.", n, variable_info)))
  }
  if (identical(measurement, "binary") && length(y_levels) != 2) {
    return(list(ok = FALSE, skipped = logistic_guard_row(dependent, predictors, "Binary logistic regression requires exactly two observed outcome levels.", n, variable_info)))
  }
  constant_predictors <- logistic_constant_predictors(data, predictors)
  if (length(constant_predictors) > 0) {
    return(list(ok = FALSE, skipped = logistic_guard_row(
      dependent,
      predictors,
      sprintf("Constant predictor(s) after complete-case filtering: %s.", paste(constant_predictors, collapse = ", ")),
      n,
      variable_info
    )))
  }
  form <- make_formula(dependent, predictors)
  mm <- tryCatch(stats::model.matrix(form, data = data), error = function(e) e)
  if (inherits(mm, "error")) {
    return(list(ok = FALSE, skipped = logistic_guard_row(dependent, predictors, conditionMessage(mm), n, variable_info)))
  }
  rank <- qr(mm)$rank
  residual_df <- n - rank
  if (residual_df < 1) {
    return(list(ok = FALSE, skipped = logistic_guard_row(
      dependent,
      predictors,
      sprintf("Residual degrees of freedom are insufficient (N=%d, model rank=%d).", n, rank),
      n,
      variable_info
    )))
  }
  if (rank < ncol(mm)) {
    return(list(ok = FALSE, skipped = logistic_guard_row(
      dependent,
      predictors,
      "Model matrix is rank deficient; coefficients are not uniquely estimable because of perfect multicollinearity.",
      n,
      variable_info
    )))
  }
  warnings <- list()
  if (identical(measurement, "binary")) {
    tab <- table(y)
    rare <- min(tab) / sum(tab)
    if (is.finite(rare) && rare < .05) {
      warnings[[length(warnings) + 1L]] <- logistic_guard_row(
        dependent,
        predictors,
        sprintf("Rare event warning: the smaller outcome class is %.1f%% of complete cases.", rare * 100),
        n,
        variable_info,
        type = "Warning"
      )
    }
  }
  list(ok = TRUE, rank = rank, residual_df = residual_df, warnings = logistic_bind_guard_rows(warnings))
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

logistic_epv_diagnostic <- function(y, parameter_df) {
  tab <- table(y)
  smallest_class <- if (length(tab) > 0L) min(tab) else NA_real_
  parameter_df <- max(1, as.numeric(parameter_df %||% 1))
  epv <- smallest_class / parameter_df
  list(
    smallest_class = as.numeric(smallest_class),
    parameter_df = parameter_df,
    epv = as.numeric(epv)
  )
}

logistic_epv_warning <- function(y, parameter_df) {
  diagnostic <- logistic_epv_diagnostic(y, parameter_df)
  epv <- diagnostic$epv
  if (!is.finite(epv)) return(NA_character_)
  if (epv < 5) {
    sprintf("EPV screening: approximate observations in the smallest outcome class per predictor parameter is %.1f (<5); estimates may be unstable.", epv)
  } else if (epv < 10) {
    sprintf("EPV screening: approximate observations in the smallest outcome class per predictor parameter is %.1f (<10); interpret estimates cautiously.", epv)
  } else {
    NA_character_
  }
}

logistic_vif_warning <- function(max_vif) {
  max_vif <- suppressWarnings(as.numeric(max_vif))
  if (length(max_vif) == 0 || is.na(max_vif) || max_vif <= 5) return(NA_character_)
  if (max_vif > 10) {
    return(sprintf("Multicollinearity warning: VIF exceeds 10 (max VIF = %s). Consider reducing predictors or using penalized logistic regression.", format_decimal3(max_vif)))
  }
  sprintf("Multicollinearity caution: VIF exceeds 5 (max VIF = %s). Interpret individual coefficients cautiously.", format_decimal3(max_vif))
}

logistic_separation_warning <- function(fit) {
  model <- fit$model
  if (!inherits(model, "glm")) return(NA_character_)
  fitted <- tryCatch(stats::fitted(model), error = function(e) numeric(0))
  coef_values <- tryCatch(stats::coef(model), error = function(e) numeric(0))
  if (any(!is.finite(coef_values)) || any(fitted < 1e-6 | fitted > 1 - 1e-6, na.rm = TRUE)) {
    return("Complete or quasi-complete separation is possible. Consider collapsing sparse categories, reducing predictors, or using Firth/penalized logistic regression.")
  }
  NA_character_
}

logistic_binary_coef_table <- function(model) {
  coef <- summary(model)$coefficients
  critical <- stats::qnorm(0.975)
  data.frame(
    Outcome = "",
    Term = rownames(coef),
    B = coef[, 1],
    SE = coef[, 2],
    p = coef[, 4],
    OR = exp(coef[, 1]),
    LLCI = exp(coef[, 1] - critical * coef[, 2]),
    ULCI = exp(coef[, 1] + critical * coef[, 2]),
    row.names = NULL,
    check.names = FALSE
  )
}

logistic_polr_coef_table <- function(model) {
  coef <- coef(summary(model))
  critical <- stats::qnorm(0.975)
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
    LLCI = exp(coef[, "Value"] - critical * coef[, "Std. Error"]),
    ULCI = exp(coef[, "Value"] + critical * coef[, "Std. Error"]),
    row.names = NULL,
    check.names = FALSE
  )
}

logistic_clm_coef_table <- function(model) {
  beta <- as.numeric(model$beta)
  names(beta) <- names(model$beta)
  covariance <- stats::vcov(model)
  se <- sqrt(diag(covariance))[names(beta)]
  z <- beta / se
  critical <- stats::qnorm(0.975)
  data.frame(
    Outcome = "",
    Term = names(beta),
    B = beta,
    SE = se,
    p = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    OR = exp(beta),
    LLCI = exp(beta - critical * se),
    ULCI = exp(beta + critical * se),
    row.names = NULL,
    check.names = FALSE
  )
}

logistic_multinom_coef_table <- function(model) {
  sm <- summary(model)
  coef <- sm$coefficients
  se <- sm$standard.errors
  critical <- stats::qnorm(0.975)
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
      LLCI = exp(coef[index, ] - critical * se[index, ]),
      ULCI = exp(coef[index, ] + critical * se[index, ]),
      row.names = NULL,
      check.names = FALSE
    )
  }))
  rows
}

logistic_parallel_odds_test <- function(data, dependent, predictors, model = NULL) {
  form <- make_formula(dependent, predictors)
  if (is.null(model)) {
    model <- ordinal::clm(form, data = data, link = "logit", Hess = TRUE)
  }
  nominal_formula <- stats::reformulate(predictors)
  alternative <- tryCatch(
    ordinal::clm(form, nominal = nominal_formula, data = data, link = "logit", Hess = TRUE),
    error = function(e) e
  )
  if (inherits(alternative, "error")) {
    return(list(
      chisq = NA_real_, df = NA_real_, p = NA_real_, available = FALSE,
      method = "ordinal::clm nominal-effects LR test",
      message = conditionMessage(alternative)
    ))
  }
  base_convergence <- logistic_model_convergence(model)
  alternative_convergence <- logistic_model_convergence(alternative)
  if (!isTRUE(base_convergence$ok) || !isTRUE(alternative_convergence$ok)) {
    return(list(
      chisq = NA_real_, df = NA_real_, p = NA_real_, available = FALSE,
      method = "ordinal::clm nominal-effects LR test",
      message = paste("Convergence failure:", paste(c(base_convergence$message, alternative_convergence$message), collapse = "; "))
    ))
  }
  ll_null <- as.numeric(stats::logLik(model))
  ll_alt <- as.numeric(stats::logLik(alternative))
  df <- attr(stats::logLik(alternative), "df") - attr(stats::logLik(model), "df")
  chisq <- max(0, 2 * (ll_alt - ll_null))
  list(
    chisq = chisq,
    df = df,
    p = if (is.finite(df) && df > 0) stats::pchisq(chisq, df, lower.tail = FALSE) else NA_real_,
    available = is.finite(df) && df > 0,
    method = "ordinal::clm nominal-effects LR test",
    message = ""
  )
}

logistic_model_convergence <- function(model) {
  if (inherits(model, "glm")) {
    return(list(ok = isTRUE(model$converged), message = if (isTRUE(model$converged)) "Converged" else "GLM did not converge"))
  }
  if (inherits(model, "multinom")) {
    code <- suppressWarnings(as.integer(model$convergence %||% NA_integer_))
    return(list(ok = identical(code, 0L), message = if (identical(code, 0L)) "Converged" else sprintf("multinom convergence code %s", code)))
  }
  if (inherits(model, "clm")) {
    code <- suppressWarnings(as.integer(model$convergence$code %||% NA_integer_))
    message <- as.character(model$convergence$messages %||% model$message %||% "")[[1L]]
    return(list(ok = identical(code, 0L), message = if (identical(code, 0L)) "Converged" else message))
  }
  list(ok = FALSE, message = "Unknown model convergence status")
}

logistic_fit_convergence <- function(fit) {
  full <- logistic_model_convergence(fit$model)
  null <- logistic_model_convergence(fit$null_model)
  list(
    ok = isTRUE(full$ok) && isTRUE(null$ok),
    message = paste(c(full$message, if (!isTRUE(null$ok)) paste("Null model:", null$message) else NULL), collapse = "; ")
  )
}

logistic_probability_matrix <- function(model, data, levels) {
  response <- tryCatch(all.vars(stats::formula(model))[[1L]], error = function(e) "")
  newdata <- data[, setdiff(names(data), response), drop = FALSE]
  probabilities <- if (inherits(model, "glm")) {
    event <- as.numeric(stats::predict(model, newdata = newdata, type = "response"))
    cbind(1 - event, event)
  } else if (inherits(model, "clm")) {
    as.matrix(stats::predict(model, newdata = newdata, type = "prob")$fit)
  } else {
    as.matrix(stats::predict(model, newdata = newdata, type = "probs"))
  }
  if (ncol(probabilities) == length(levels)) colnames(probabilities) <- levels
  probabilities
}

logistic_apparent_performance <- function(model, data, dependent, method) {
  outcome <- droplevels(as.factor(data[[dependent]]))
  levels <- levels(outcome)
  probabilities <- tryCatch(logistic_probability_matrix(model, data, levels), error = function(e) NULL)
  if (is.null(probabilities) || nrow(probabilities) != length(outcome) || ncol(probabilities) != length(levels)) return(NULL)
  probabilities <- pmin(pmax(probabilities, 1e-15), 1 - 1e-15)
  observed_index <- match(as.character(outcome), levels)
  observed_probability <- probabilities[cbind(seq_along(observed_index), observed_index)]
  log_loss <- -mean(log(observed_probability))
  if (identical(method, "Binary logistic regression")) {
    event <- as.integer(observed_index == 2L)
    event_probability <- probabilities[, 2L]
    n_event <- sum(event == 1L)
    n_nonevent <- sum(event == 0L)
    auc <- if (n_event > 0L && n_nonevent > 0L) {
      (sum(rank(event_probability, ties.method = "average")[event == 1L]) - n_event * (n_event + 1) / 2) / (n_event * n_nonevent)
    } else NA_real_
    return(c(
      `AUC (apparent)` = auc,
      `Brier score (apparent)` = mean((event - event_probability)^2),
      `Tjur R² (apparent)` = mean(event_probability[event == 1L]) - mean(event_probability[event == 0L]),
      `Log loss (apparent)` = log_loss
    ))
  }
  predicted <- max.col(probabilities, ties.method = "first")
  metrics <- c(
    `Accuracy (apparent)` = mean(predicted == observed_index),
    `Log loss (apparent)` = log_loss
  )
  if (identical(method, "Ordinal logistic regression")) {
    cumulative_probability <- t(apply(probabilities, 1L, cumsum))[, -length(levels), drop = FALSE]
    cumulative_observed <- vapply(seq_len(length(levels) - 1L), function(index) as.numeric(observed_index <= index), numeric(length(outcome)))
    metrics <- c(metrics, `Ranked probability score (apparent)` = mean(rowSums((cumulative_probability - cumulative_observed)^2) / (length(levels) - 1L)))
  } else {
    observed_matrix <- matrix(0, nrow(probabilities), ncol(probabilities))
    observed_matrix[cbind(seq_along(observed_index), observed_index)] <- 1
    metrics <- c(metrics, `Multiclass Brier score (apparent)` = mean(rowSums((probabilities - observed_matrix)^2)))
  }
  metrics
}

fit_logistic_model <- function(data, dependent, predictors, measurement, ordinal_mode = c("auto", "ordinal", "multinomial"), parallel = NULL) {
  ordinal_mode <- match.arg(ordinal_mode)
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
    if (identical(ordinal_mode, "multinomial")) {
      multi <- nnet::multinom(form, data = model_data, trace = FALSE)
      multi_null <- nnet::multinom(null_formula, data = model_data, trace = FALSE)
      multi$call$formula <- form
      multi_null$call$formula <- null_formula
      multi$call$data <- model_data
      multi_null$call$data <- model_data
      return(list(model = multi, null_model = multi_null, method = "Multinomial logistic regression", coef_table = logistic_multinom_coef_table(multi), parallel = parallel, ordinal_fallback = TRUE))
    }
    ordinal <- ordinal::clm(form, data = model_data, link = "logit", Hess = TRUE)
    ordinal_null <- ordinal::clm(null_formula, data = model_data, link = "logit", Hess = TRUE)
    if (is.null(parallel)) parallel <- logistic_parallel_odds_test(model_data, dependent, predictors, ordinal)
    use_multinomial <- identical(ordinal_mode, "auto") && !is.na(parallel$p) && parallel$p <= .05
    if (isTRUE(use_multinomial)) {
      multi <- nnet::multinom(form, data = model_data, trace = FALSE)
      multi_null <- nnet::multinom(null_formula, data = model_data, trace = FALSE)
      multi$call$formula <- form
      multi_null$call$formula <- null_formula
      multi$call$data <- model_data
      multi_null$call$data <- model_data
      return(list(model = multi, null_model = multi_null, method = "Multinomial logistic regression", coef_table = logistic_multinom_coef_table(multi), parallel = parallel, ordinal_fallback = TRUE))
    }
    return(list(model = ordinal, null_model = ordinal_null, method = "Ordinal logistic regression", coef_table = logistic_clm_coef_table(ordinal), parallel = parallel, ordinal_fallback = FALSE))
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
  reference_values = character(0),
  category_table = NULL
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
  warning_rows <- list()
  skipped_rows <- list()
  hierarchical_note <- "Hierarchical models were fitted on the complete cases of the final model (listwise across all blocks); all steps share the same N."
  for (dependent in dependents) {
    measurement <- logistic_measurement_for(dependent, variable_info)
    if (!measurement %in% logistic_dependent_measurements()) {
      skipped_rows[[length(skipped_rows) + 1L]] <- logistic_guard_row(dependent, character(0), "Unsupported logistic dependent measurement level.", NA_integer_, variable_info)
      next
    }
    all_vars <- unique(c(dependent, block1, block2, block3))
    full_prep <- logistic_prepare_data(data, all_vars, variable_info, reference_values, category_table)
    model_data <- full_prep$data
    observed_outcome_levels <- if (is.factor(model_data[[dependent]])) levels(droplevels(model_data[[dependent]])) else unique(stats::na.omit(model_data[[dependent]]))
    analysis_measurement <- if (length(observed_outcome_levels) == 2L) "binary" else measurement
    binary_level_note <- if (!identical(measurement, "binary") && identical(analysis_measurement, "binary")) {
      "Two outcome levels remained in the final complete-case sample; binary logistic regression was used."
    } else NULL
    final_predictors <- setdiff(steps[[length(steps)]]$predictors, dependent)
    ordinal_mode <- "auto"
    ordinal_parallel <- NULL
    ordinal_basis_note <- NULL
    if (identical(analysis_measurement, "ordered") && length(final_predictors) > 0L) {
      final_preflight <- logistic_preflight(model_data, dependent, final_predictors, analysis_measurement, variable_info)
      if (isTRUE(final_preflight$ok)) {
        ordinal_parallel <- tryCatch(
          logistic_parallel_odds_test(model_data, dependent, final_predictors),
          error = function(e) list(
            chisq = NA_real_, df = NA_real_, p = NA_real_, available = FALSE,
            method = "ordinal::clm nominal-effects LR test", message = conditionMessage(e)
          )
        )
        ordinal_mode <- if (!is.na(ordinal_parallel$p) && ordinal_parallel$p <= .05) "multinomial" else "ordinal"
        ordinal_parallel$basis <- if (length(steps) > 1L) "Final hierarchical model" else "Specified model"
        ordinal_basis_note <- if (isTRUE(ordinal_parallel$available)) {
          sprintf("The proportional-odds decision used an ordinal::clm nominal-effects likelihood-ratio test based on the %s; the same model family was used for every hierarchical step.", tolower(ordinal_parallel$basis))
        } else {
          sprintf("The proportional-odds nominal-effects test was unavailable (%s); the cumulative logit model was retained and this assumption requires external review.", ordinal_parallel$message %||% "unknown reason")
        }
      }
    }
    previous <- NULL
    for (step_index in seq_along(steps)) {
      predictors <- setdiff(steps[[step_index]]$predictors, dependent)
      if (length(predictors) == 0) next
      prep <- logistic_prepare_data(model_data, unique(c(dependent, predictors)), variable_info, reference_values, category_table)
      event_note <- NA_character_
      if (identical(analysis_measurement, "binary") && is.factor(prep$data[[dependent]]) && length(levels(prep$data[[dependent]])) >= 2) {
        event_note <- sprintf("Binary event for %s is %s; reference is %s.", dependent, levels(prep$data[[dependent]])[[2]], levels(prep$data[[dependent]])[[1]])
      }
      sparse <- logistic_sparse_cell_check(prep$data, dependent, predictors, variable_info)
      preflight <- logistic_preflight(prep$data, dependent, predictors, analysis_measurement, variable_info)
      if (is.data.frame(preflight$warnings) && nrow(preflight$warnings) > 0) {
        warning_rows[[length(warning_rows) + 1L]] <- preflight$warnings
      }
      if (!isTRUE(preflight$ok)) {
        skipped_rows[[length(skipped_rows) + 1L]] <- preflight$skipped
        next
      }
      fit <- tryCatch(
        fit_logistic_model(prep$data, dependent, predictors, analysis_measurement, ordinal_mode = ordinal_mode, parallel = ordinal_parallel),
        error = function(e) e
      )
      if (inherits(fit, "error")) {
        skipped_rows[[length(skipped_rows) + 1L]] <- logistic_guard_row(dependent, predictors, conditionMessage(fit), prep$n, variable_info)
        break
      }
      convergence <- logistic_fit_convergence(fit)
      if (!isTRUE(convergence$ok)) {
        skipped_rows[[length(skipped_rows) + 1L]] <- logistic_guard_row(
          dependent, predictors, paste("Model convergence gate failed:", convergence$message), prep$n, variable_info
        )
        break
      }
      stats <- logistic_fit_stats(fit$model, fit$null_model, prep$n)
      predictor_parameter_df <- max(1, preflight$rank - 1L)
      epv <- logistic_epv_diagnostic(prep$data[[dependent]], predictor_parameter_df)
      epv_note <- logistic_epv_warning(prep$data[[dependent]], predictor_parameter_df)
      max_vif <- logistic_vif_summary(make_formula(dependent, predictors), prep$data)
      predictor_vif <- logistic_vif_by_predictor(make_formula(dependent, predictors), prep$data, predictors)
      vif_note <- logistic_vif_warning(max_vif)
      separation_note <- logistic_separation_warning(fit)
      continuous_predictors <- predictors[vapply(predictors, function(name) identical(logistic_measurement_for(name, variable_info), "continuous"), logical(1))]
      functional_form_note <- if (length(continuous_predictors) > 0L) {
        sprintf("Linearity in the logit is not established automatically for continuous predictor(s): %s. Inspect nonlinear terms or spline sensitivity analyses when scientifically plausible.", paste(continuous_predictors, collapse = ", "))
      } else NA_character_
      iia_note <- if (identical(fit$method, "Multinomial logistic regression")) {
        "The independence of irrelevant alternatives (IIA) is not established automatically; justify the outcome categories and consider sensitivity analyses if alternatives may be substitutable."
      } else NA_character_
      result <- list(
        dependent = dependent,
        predictors = predictors,
        dependent_levels = if (is.factor(prep$data[[dependent]])) levels(prep$data[[dependent]]) else character(0),
        predictor_levels = lapply(stats::setNames(predictors, predictors), function(name) {
          if (is.factor(prep$data[[name]])) levels(prep$data[[name]]) else character(0)
        }),
        formula = make_formula(dependent, predictors),
        n = prep$n,
        missing_excluded = nrow(data) - prep$n,
        method = fit$method,
        coef_table = fit$coef_table,
        fit = stats,
        convergence = convergence,
        performance = logistic_apparent_performance(fit$model, prep$data, dependent, fit$method),
        epv = epv,
        max_vif = max_vif,
        predictor_vif = predictor_vif,
        parallel = fit$parallel,
        ordinal_fallback = isTRUE(fit$ordinal_fallback),
        notes = unique(stats::na.omit(c(refs$notes, if (length(steps) > 1L) hierarchical_note else NULL, binary_level_note, event_note, ordinal_basis_note, sparse$warnings, epv_note, vif_note, separation_note, functional_form_note, iia_note, "Odds-ratio confidence intervals use the large-sample Wald method.", "Performance statistics are apparent (in-sample) diagnostics and do not establish external predictive validity."))),
        hierarchical_step = steps[[step_index]]$name,
        hierarchical_step_index = step_index,
        hierarchical_blocks = steps[[step_index]]$blocks,
        hierarchical_note = if (length(steps) > 1L) hierarchical_note else NULL
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
  skipped <- logistic_bind_guard_rows(skipped_rows)
  shiny::validate(shiny::need(length(results) > 0 || is.data.frame(skipped) && nrow(skipped) > 0, "No logistic regression model could be prepared."))
  attr(results, "warnings") <- logistic_bind_guard_rows(warning_rows)
  attr(results, "skipped") <- skipped
  results
}
