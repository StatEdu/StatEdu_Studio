# Generalized linear model analysis helpers.

generalized_family_labels <- function() {
  c(
    auto = "Auto",
    gaussian = "Linear Gaussian / identity",
    binomial = "Binary logistic / logit",
    gamma = "Gamma / log",
    count = "Count: Poisson or negative binomial / log"
  )
}

generalized_family_choices <- function() {
  stats::setNames(names(generalized_family_labels()), unname(generalized_family_labels()))
}

generalized_resolve_family <- function(family) {
  family <- as.character(family %||% "auto")[[1]]
  if (family %in% names(generalized_family_labels())) family else "auto"
}

generalized_link_choices <- function(family = "auto") {
  family <- generalized_resolve_family(family)
  switch(
    family,
    gaussian = c("Default for family" = "default", "identity" = "identity", "log" = "log", "inverse" = "inverse"),
    binomial = c("Default for family" = "default", "logit" = "logit"),
    gamma = c("Default for family" = "default", "log" = "log", "inverse" = "inverse", "identity" = "identity"),
    count = c("Default for family" = "default", "log" = "log"),
    c("Default for family" = "default")
  )
}

generalized_missing_strategy_choices <- function() {
  c(
    "Complete-case: row-wise" = "complete",
    "Multiple imputation (MI)" = "mi",
    "Inverse probability weighting (IPW)" = "ipw"
  )
}

generalized_resolve_missing_strategy <- function(strategy) {
  strategy <- as.character(strategy %||% "complete")[[1]]
  choices <- unname(generalized_missing_strategy_choices())
  if (strategy %in% choices) return(strategy)
  "complete"
}

generalized_missing_strategy_label <- function(strategy) {
  strategy <- generalized_resolve_missing_strategy(strategy)
  labels <- names(generalized_missing_strategy_choices())
  values <- unname(generalized_missing_strategy_choices())
  labels[match(strategy, values)] %||% "Complete-case: row-wise"
}

generalized_missing_strategy_detail <- function(strategy) {
  switch(
    generalized_resolve_missing_strategy(strategy),
    mi = "Uses standard mice-based multiple imputation for incomplete selected GLM variables, including the dependent variable when it is missing. The selected GLM is fitted in each imputed dataset and pooled using Rubin-style total variance. Treat this as a prespecified primary or sensitivity option when missingness is plausibly MAR.",
    ipw = "Estimates the probability of being a complete case from fully observed predictors and fits the selected GLM with inverse-probability weights. Use this as a sensitivity option when complete-case inclusion may depend on observed covariates, and review positivity/weight stability and the observation model.",
    "Drops rows with missing selected model variables before fitting the GLM. This is transparent, but it is strongest when missingness is minimal or plausibly MCAR."
  )
}

generalized_mi_outcome_choices <- function() {
  c(
    "Use rows with observed dependent variable (recommended)" = "observed",
    "Impute missing dependent variable for sensitivity analysis" = "impute"
  )
}

generalized_resolve_mi_outcome <- function(value) {
  value <- as.character(value %||% "observed")[[1]]
  if (value %in% unname(generalized_mi_outcome_choices())) value else "observed"
}

generalized_mi_outcome_label <- function(value) {
  value <- generalized_resolve_mi_outcome(value)
  labels <- names(generalized_mi_outcome_choices())
  values <- unname(generalized_mi_outcome_choices())
  labels[match(value, values)] %||% "Use rows with observed dependent variable (recommended)"
}

generalized_resolve_mi_count <- function(value, default = 5L, minimum = 1L, maximum = 50L) {
  value <- suppressWarnings(as.integer(value %||% default))
  if (!is.finite(value)) value <- default
  max(minimum, min(maximum, value))
}

generalized_se_type_choices <- function() {
  c(
    "Model-based" = "model",
    "Robust sandwich HC0" = "HC0",
    "Robust sandwich HC1" = "HC1",
    "Robust sandwich HC2" = "HC2",
    "Robust sandwich HC3" = "HC3"
  )
}

generalized_resolve_se_type <- function(se_type = NULL, robust = TRUE) {
  if (is.null(se_type) || length(se_type) == 0 || !nzchar(as.character(se_type)[[1]])) {
    return(if (isTRUE(robust)) "HC1" else "model")
  }
  se_type <- as.character(se_type)[[1]]
  if (identical(tolower(se_type), "model")) return("model")
  se_type <- toupper(se_type)
  choices <- unname(generalized_se_type_choices())
  if (se_type %in% choices) se_type else if (isTRUE(robust)) "HC1" else "model"
}

generalized_se_type_label <- function(se_type) {
  se_type <- generalized_resolve_se_type(se_type, robust = FALSE)
  labels <- names(generalized_se_type_choices())
  values <- unname(generalized_se_type_choices())
  labels[match(se_type, values)] %||% "Model-based"
}

generalized_measurement_for <- function(name, variable_info = NULL) {
  logistic_measurement_for(name, variable_info)
}

generalized_outcome_allowed <- function(name, variable_info = NULL) {
  measurement <- generalized_measurement_for(name, variable_info)
  !measurement %in% c("category", "ordered")
}

generalized_offset_allowed <- function(name, variable_info = NULL) {
  measurement <- generalized_measurement_for(name, variable_info)
  !measurement %in% c("binary", "category", "ordered")
}

generalized_link_is_allowed <- function(family, link) {
  link <- as.character(link %||% "default")[[1]]
  link %in% unname(generalized_link_choices(family))
}

generalized_resolve_link <- function(family, link) {
  link <- as.character(link %||% "default")[[1]]
  if (generalized_link_is_allowed(family, link)) link else "default"
}

generalized_formula_variable <- function(name) {
  name <- as.character(name %||% "")[[1]]
  if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", name)) {
    return(name)
  }
  paste0("`", gsub("`", "\\\\`", name), "`")
}

generalized_formula <- function(outcome, predictors, offset = character(0)) {
  predictors <- unique(as.character(predictors %||% character(0)))
  offset <- utils::head(as.character(offset %||% character(0)), 1)
  rhs <- if (length(predictors) == 0) {
    "1"
  } else {
    paste(vapply(predictors, generalized_formula_variable, character(1)), collapse = " + ")
  }
  if (length(offset) == 1 && nzchar(offset)) {
    rhs <- paste(rhs, sprintf("offset(log(%s))", generalized_formula_variable(offset)), sep = " + ")
  }
  stats::as.formula(sprintf("%s ~ %s", generalized_formula_variable(outcome), rhs))
}

generalized_is_integerish <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  length(x) > 0 && all(abs(x - round(x)) < .Machine$double.eps^0.5)
}

generalized_skewness <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) < 3 || stats::sd(x) == 0) return(NA_real_)
  mean((x - mean(x))^3) / stats::sd(x)^3
}

generalized_detect_family <- function(data, outcome, requested_family, variable_info = NULL) {
  requested_family <- generalized_resolve_family(requested_family)
  if (!identical(requested_family, "auto")) {
    return(requested_family)
  }

  measurement <- generalized_measurement_for(outcome, variable_info)
  if (measurement %in% c("category", "ordered")) {
    stop("GLM supports continuous or binary dependent variables. Use the logistic regression menu for categorical or ordinal dependent variables.", call. = FALSE)
  }
  y <- data[[outcome]]
  observed <- y[!is.na(y)]
  numeric_y <- suppressWarnings(as.numeric(observed))

  if (identical(measurement, "binary")) {
    return("binomial")
  }
  if (is.numeric(y) || is.integer(y)) {
    if (length(numeric_y) > 0 && all(is.finite(numeric_y)) && all(numeric_y >= 0) && generalized_is_integerish(numeric_y)) {
      return("count")
    }
    skew <- generalized_skewness(numeric_y)
    if (length(numeric_y) > 0 && all(is.finite(numeric_y)) && all(numeric_y > 0) && is.finite(skew) && skew >= 1) {
      return("gamma")
    }
  }
  "gaussian"
}

generalized_prepare_data <- function(
  data,
  variables,
  outcome,
  family,
  variable_info = NULL,
  reference_values = character(0),
  drop_complete = TRUE
) {
  variables <- unique(as.character(variables %||% character(0)))
  prepared <- data[, variables, drop = FALSE]
  raw_n <- nrow(prepared)
  missing_table <- data.frame(
    Variable = variables,
    Missing = vapply(prepared, function(values) sum(is.na(values)), integer(1)),
    `Missing %` = vapply(prepared, function(values) {
      if (length(values) == 0) return(NA_real_)
      100 * mean(is.na(values))
    }, numeric(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  prepared <- prepare_regression_model_data_static(
    prepared,
    variables,
    variable_info = variable_info,
    reference_values = reference_values
  )
  coding_levels <- lapply(stats::setNames(variables, variables), function(name) {
    values <- prepared[[name]]
    measurement <- generalized_measurement_for(name, variable_info)
    if (is.factor(values)) {
      return(levels(droplevels(values)))
    }
    if (!measurement %in% c("binary", "category", "ordered") && !is.character(values) && !is.logical(values)) {
      return(character(0))
    }
    values <- unique(stats::na.omit(values))
    values <- values[order(values)]
    as.character(values)
  })

  if (identical(family, "binomial")) {
    y <- prepared[[outcome]]
    if (is.factor(y) || is.character(y) || is.logical(y)) {
      f <- as.factor(y)
      if (nlevels(f) != 2) {
        stop("Binary logistic GLM requires exactly two observed outcome levels.", call. = FALSE)
      }
      prepared[[outcome]] <- as.integer(f == levels(f)[[2]])
    } else {
      vals <- sort(unique(stats::na.omit(y)))
      if (length(vals) != 2) {
        stop("Binary logistic GLM requires exactly two observed outcome values.", call. = FALSE)
      }
      prepared[[outcome]] <- as.integer(y == vals[[2]])
    }
  } else {
    prepared[[outcome]] <- suppressWarnings(as.numeric(prepared[[outcome]]))
  }

  complete <- stats::complete.cases(prepared)
  if (isTRUE(drop_complete)) {
    prepared <- prepared[complete, , drop = FALSE]
  }
  if (isTRUE(drop_complete) && nrow(prepared) < 3) {
    stop("At least 3 complete cases are required.", call. = FALSE)
  }
  attr(prepared, "raw_n") <- raw_n
  attr(prepared, "excluded") <- sum(!complete)
  attr(prepared, "complete_index") <- complete
  attr(prepared, "missing_table") <- missing_table
  attr(prepared, "coding_levels") <- coding_levels
  prepared
}

generalized_missing_pattern_summary <- function(raw_prepared, complete_index, outcome, predictors, exposure = character(0)) {
  if (!is.data.frame(raw_prepared) || nrow(raw_prepared) == 0) {
    return(data.frame(Item = character(0), Value = character(0), stringsAsFactors = FALSE, check.names = FALSE))
  }
  complete_index <- as.logical(complete_index %||% stats::complete.cases(raw_prepared))
  complete_index[is.na(complete_index)] <- FALSE
  outcome <- utils::head(intersect(as.character(outcome %||% character(0)), names(raw_prepared)), 1)
  predictors <- intersect(as.character(predictors %||% character(0)), names(raw_prepared))
  exposure <- utils::head(intersect(as.character(exposure %||% character(0)), names(raw_prepared)), 1)
  missing_matrix <- is.na(raw_prepared)
  pattern <- apply(missing_matrix, 1, function(row) {
    missing_names <- names(raw_prepared)[row]
    if (length(missing_names) == 0) "Complete" else paste(missing_names, collapse = ", ")
  })
  pattern_counts <- sort(table(pattern), decreasing = TRUE)
  most_common_pattern <- if (length(pattern_counts) > 0) {
    sprintf("%s (n=%s)", names(pattern_counts)[[1]], as.integer(pattern_counts[[1]]))
  } else {
    ""
  }
  outcome_missing <- if (length(outcome) == 1) sum(is.na(raw_prepared[[outcome]])) else NA_integer_
  predictor_missing <- if (length(predictors) > 0) sum(rowSums(missing_matrix[, predictors, drop = FALSE]) > 0) else 0L
  exposure_missing <- if (length(exposure) == 1) sum(is.na(raw_prepared[[exposure]])) else 0L
  data.frame(
    Item = c(
      "Raw rows",
      "Complete model rows",
      "Rows excluded by complete-case screen",
      "Rows with missing dependent variable",
      "Rows with any missing independent variable",
      "Rows with missing exposure / offset",
      "Distinct missingness patterns",
      "Most common missingness pattern"
    ),
    Value = c(
      as.character(nrow(raw_prepared)),
      as.character(sum(complete_index)),
      as.character(sum(!complete_index)),
      if (is.na(outcome_missing)) "" else as.character(outcome_missing),
      as.character(predictor_missing),
      as.character(exposure_missing),
      as.character(length(pattern_counts)),
      most_common_pattern
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_validate_outcome <- function(data, outcome, family) {
  y <- data[[outcome]]
  if (identical(family, "gamma") && any(y <= 0, na.rm = TRUE)) {
    stop("Gamma GLM requires strictly positive outcome values.", call. = FALSE)
  }
  if (identical(family, "count") && (any(y < 0, na.rm = TRUE) || !generalized_is_integerish(y))) {
    stop("Count GLM requires non-negative integer outcome values.", call. = FALSE)
  }
  if (identical(family, "binomial") && length(unique(stats::na.omit(y))) != 2) {
    stop("Binary logistic GLM requires two observed outcome levels.", call. = FALSE)
  }
  invisible(TRUE)
}

generalized_validate_offset <- function(data, offset = character(0)) {
  offset <- utils::head(as.character(offset %||% character(0)), 1)
  if (length(offset) == 0 || !nzchar(offset)) return(invisible(TRUE))
  values <- suppressWarnings(as.numeric(data[[offset]]))
  if (any(!is.na(values) & values <= 0)) {
    stop("Exposure / offset values must be strictly positive because GLM uses offset(log(exposure)).", call. = FALSE)
  }
  invisible(TRUE)
}

generalized_family_object <- function(family, link = "default") {
  link <- as.character(link %||% "default")[[1]]
  if (!generalized_link_is_allowed(family, link)) {
    stop(sprintf("The selected link function is not supported for the fitted GLM family: %s.", family), call. = FALSE)
  }
  if (identical(link, "default")) {
    link <- switch(
      family,
      gaussian = "identity",
      binomial = "logit",
      gamma = "log",
      count = "log",
      "identity"
    )
  }
  switch(
    family,
    gaussian = stats::gaussian(link = link),
    binomial = stats::binomial(link = link),
    gamma = stats::Gamma(link = link),
    count = stats::poisson(link = link),
    stats::gaussian(link = "identity")
  )
}

generalized_overdispersion_ratio <- function(model) {
  rdf <- stats::df.residual(model)
  if (is.null(rdf) || is.na(rdf) || rdf <= 0) return(NA_real_)
  pearson <- stats::residuals(model, type = "pearson")
  sum(pearson^2, na.rm = TRUE) / rdf
}

generalized_zero_screen <- function(model, data, outcome) {
  observed <- suppressWarnings(as.numeric(data[[outcome]]))
  observed <- observed[is.finite(observed)]
  fitted <- tryCatch(stats::fitted(model), error = function(e) numeric(0))
  fitted <- suppressWarnings(as.numeric(fitted))
  fitted <- fitted[is.finite(fitted) & fitted >= 0]
  if (length(observed) == 0 || length(fitted) == 0) {
    return(list(observed = NA_real_, expected = NA_real_, ratio = NA_real_, flag = FALSE))
  }
  observed_zero <- mean(observed == 0)
  expected_zero <- mean(exp(-fitted))
  ratio <- if (is.finite(expected_zero) && expected_zero > 0) observed_zero / expected_zero else NA_real_
  flag <- is.finite(ratio) && ratio > 1.5 && is.finite(observed_zero - expected_zero) && (observed_zero - expected_zero) > 0.10
  list(observed = observed_zero, expected = expected_zero, ratio = ratio, flag = flag)
}

generalized_metric <- function(model, metric) {
  tryCatch(
    switch(metric, aic = stats::AIC(model), bic = stats::BIC(model), NA_real_),
    error = function(e) NA_real_
  )
}

generalized_log_likelihood <- function(model) {
  tryCatch(as.numeric(stats::logLik(model)), error = function(e) NA_real_)
}

generalized_coef_table <- function(model, robust = TRUE, exponentiate = FALSE, se_type = NULL) {
  se_type <- generalized_resolve_se_type(se_type, robust = robust)
  coef_matrix <- NULL
  robust_used <- FALSE
  if (!identical(se_type, "model") && requireNamespace("sandwich", quietly = TRUE) && requireNamespace("lmtest", quietly = TRUE)) {
    coef_matrix <- tryCatch(
      lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = se_type)),
      error = function(e) NULL
    )
    robust_used <- !is.null(coef_matrix)
  }
  if (is.null(coef_matrix)) {
    coef_matrix <- summary(model)$coefficients
  }
  table <- longitudinal_coef_table_from_matrix(coef_matrix, exponentiate = exponentiate)
  attr(table, "robust_used") <- robust_used
  attr(table, "se_type_requested") <- se_type
  attr(table, "se_type_used") <- if (isTRUE(robust_used)) se_type else "model"
  table
}

generalized_fit_model <- function(data, formula, family, link, robust, exponentiate, overdispersion_check, weights = NULL, se_type = NULL) {
  fit_note <- character(0)
  requested_family <- family
  fitted_family <- family
  count_details <- data.frame()
  weights <- if (is.null(weights)) NULL else suppressWarnings(as.numeric(weights))
  if (!is.null(weights) && length(weights) != nrow(data)) {
    stop("Analysis weights must match the analyzed GLM rows.", call. = FALSE)
  }
  if (!is.null(weights)) {
    data$.statedu_glm_weights <- weights
  }

  if (identical(family, "count")) {
    poisson <- if (is.null(weights)) {
      stats::glm(formula, data = data, family = generalized_family_object("count", link))
    } else {
      stats::glm(formula, data = data, family = generalized_family_object("count", link), weights = .statedu_glm_weights)
    }
    dispersion <- generalized_overdispersion_ratio(poisson)
    outcome <- all.vars(formula)[[1]]
    zero_screen <- generalized_zero_screen(poisson, data, outcome)
    nb <- if (requireNamespace("MASS", quietly = TRUE)) {
      tryCatch(
        if (is.null(weights)) {
          MASS::glm.nb(formula, data = data, link = "log")
        } else {
          MASS::glm.nb(formula, data = data, weights = .statedu_glm_weights, link = "log")
        },
        error = function(e) NULL
      )
    } else {
      NULL
    }
    model <- poisson
    threshold <- 1.5
    nb_ok <- !is.null(nb)
    poisson_loglik <- generalized_log_likelihood(poisson)
    nb_loglik <- if (nb_ok) generalized_log_likelihood(nb) else NA_real_
    lr_stat <- if (is.finite(poisson_loglik) && is.finite(nb_loglik)) {
      max(0, 2 * (nb_loglik - poisson_loglik))
    } else {
      NA_real_
    }
    if (isTRUE(overdispersion_check) && is.finite(dispersion) && dispersion > threshold) {
      if (!is.null(nb)) {
        model <- nb
        fitted_family <- "negative_binomial"
        fit_note <- c(fit_note, "Poisson overdispersion exceeded the prespecified screening threshold of 1.5; negative binomial GLM was fitted.")
      } else {
        fit_note <- c(fit_note, "Poisson overdispersion exceeded the prespecified screening threshold of 1.5, but negative binomial GLM did not converge; Poisson GLM was retained.")
      }
    }
    decision <- if (identical(fitted_family, "negative_binomial")) {
      "Poisson dispersion exceeded the prespecified screening threshold and negative-binomial fit was available."
    } else if (!is.finite(dispersion)) {
      "Poisson dispersion could not be computed; Poisson GLM was retained."
    } else if (dispersion > threshold && !nb_ok) {
      "Poisson dispersion exceeded the prespecified screening threshold, but negative-binomial fit was unavailable; Poisson GLM was retained."
    } else {
      "Poisson dispersion did not exceed the prespecified screening threshold; Poisson GLM was retained."
    }
    count_details <- data.frame(
      Item = c(
        "Requested family",
        "Selected family",
        "Selection rule",
        "Poisson dispersion ratio",
        "Overdispersion threshold",
        "Observed zero proportion",
        "Poisson expected zero proportion",
        "Zero ratio",
        "Zero screen",
        "Poisson AIC",
        "Poisson BIC",
        "Poisson logLik",
        "Negative binomial AIC",
        "Negative binomial BIC",
        "Negative binomial logLik",
        "Poisson vs NB LR statistic",
        "LL comparison note",
        "Decision rule"
      ),
      Value = c(
        "Count: Poisson or negative binomial / log",
        if (identical(fitted_family, "negative_binomial")) "Negative binomial / log" else "Poisson / log",
        "Dispersion-threshold screening selects Poisson versus negative binomial; AIC/BIC are reported as supplementary fit diagnostics, not as the automatic selection rule.",
        format_decimal3(dispersion),
        format_decimal3(threshold),
        format_decimal3(zero_screen$observed),
        format_decimal3(zero_screen$expected),
        format_decimal3(zero_screen$ratio),
        if (isTRUE(zero_screen$flag)) "Possible excess zeros; consider zero-inflated or hurdle sensitivity analysis." else "No excess-zero flag by simple Poisson zero screen.",
        format_decimal3(generalized_metric(poisson, "aic")),
        format_decimal3(generalized_metric(poisson, "bic")),
        format_decimal3(poisson_loglik),
        if (nb_ok) format_decimal3(generalized_metric(nb, "aic")) else "",
        if (nb_ok) format_decimal3(generalized_metric(nb, "bic")) else "",
        if (nb_ok) format_decimal3(nb_loglik) else "",
        if (is.finite(lr_stat)) format_decimal3(lr_stat) else "",
        "LL, AIC, and BIC are supplementary diagnostics; the automatic count-family decision uses the prespecified overdispersion screening rule.",
        decision
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    coef_table <- generalized_coef_table(model, robust = robust, exponentiate = exponentiate, se_type = se_type)
    return(list(
      model = model,
      requested_family = requested_family,
      fitted_family = fitted_family,
      dispersion = dispersion,
      coef_table = coef_table,
      robust_used = isTRUE(attr(coef_table, "robust_used")),
      se_type_requested = attr(coef_table, "se_type_requested") %||% "model",
      se_type_used = attr(coef_table, "se_type_used") %||% "model",
      fit_note = fit_note,
      count_details = count_details
    ))
  }

  model <- if (is.null(weights)) {
    stats::glm(formula, data = data, family = generalized_family_object(family, link))
  } else {
    stats::glm(formula, data = data, family = generalized_family_object(family, link), weights = .statedu_glm_weights)
  }
  coef_table <- generalized_coef_table(model, robust = robust, exponentiate = exponentiate, se_type = se_type)
  list(
    model = model,
    requested_family = requested_family,
    fitted_family = family,
    dispersion = generalized_overdispersion_ratio(model),
    coef_table = coef_table,
    robust_used = isTRUE(attr(coef_table, "robust_used")),
    se_type_requested = attr(coef_table, "se_type_requested") %||% "model",
    se_type_used = attr(coef_table, "se_type_used") %||% "model",
    fit_note = fit_note,
    count_details = count_details
  )
}

generalized_pool_coef_tables <- function(tables, exponentiate = FALSE) {
  tables <- tables[vapply(tables, function(table) is.data.frame(table) && nrow(table) > 0 && all(c("Term", "B", "SE") %in% names(table)), logical(1))]
  if (length(tables) == 0) {
    stop("No fitted imputed GLM coefficient tables were available for pooling.", call. = FALSE)
  }
  terms <- Reduce(intersect, lapply(tables, function(table) as.character(table$Term)))
  if (length(terms) == 0) {
    stop("No common coefficient terms were available for MI pooling.", call. = FALSE)
  }
  rows <- lapply(terms, function(term) {
    estimates <- vapply(tables, function(table) {
      suppressWarnings(as.numeric(table$B[match(term, table$Term)]))
    }, numeric(1))
    variances <- vapply(tables, function(table) {
      se <- suppressWarnings(as.numeric(table$SE[match(term, table$Term)]))
      se^2
    }, numeric(1))
    ok <- is.finite(estimates) & is.finite(variances)
    estimates <- estimates[ok]
    variances <- variances[ok]
    if (length(estimates) == 0) {
      return(data.frame())
    }
    m <- length(estimates)
    qbar <- mean(estimates)
    ubar <- mean(variances)
    bvar <- if (m > 1) stats::var(estimates) else 0
    total <- ubar + (1 + 1 / max(m, 1)) * bvar
    se <- sqrt(max(total, 0))
    statistic <- if (is.finite(se) && se > 0) qbar / se else NA_real_
    p <- if (is.finite(statistic)) 2 * stats::pnorm(abs(statistic), lower.tail = FALSE) else NA_real_
    row <- data.frame(
      Term = term,
      B = qbar,
      SE = se,
      Statistic = statistic,
      p = p,
      LLCI = qbar - 1.96 * se,
      ULCI = qbar + 1.96 * se,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (isTRUE(exponentiate)) {
      row$`exp(B)` <- exp(row$B)
      row$`exp(LLCI)` <- exp(row$LLCI)
      row$`exp(ULCI)` <- exp(row$ULCI)
    }
    row
  })
  pooled <- analysis_bind_rows(rows)
  if (!is.data.frame(pooled) || nrow(pooled) == 0) {
    stop("No finite coefficient estimates were available for MI pooling.", call. = FALSE)
  }
  attr(pooled, "robust_used") <- all(vapply(tables, function(table) isTRUE(attr(table, "robust_used")), logical(1)))
  requested <- unique(vapply(tables, function(table) attr(table, "se_type_requested") %||% "model", character(1)))
  used <- unique(vapply(tables, function(table) attr(table, "se_type_used") %||% "model", character(1)))
  attr(pooled, "se_type_requested") <- if (length(requested) == 1) requested else paste(requested, collapse = ", ")
  attr(pooled, "se_type_used") <- if (length(used) == 1) used else paste(used, collapse = ", ")
  pooled
}

generalized_mice_method <- function(data) {
  method <- mice::make.method(data)
  for (name in names(data)) {
    values <- data[[name]]
    if (is.factor(values) && nlevels(values) <= 2) method[[name]] <- "logreg"
    if (is.factor(values) && nlevels(values) > 2) method[[name]] <- "polyreg"
  }
  method
}

generalized_fit_mi <- function(
  raw_prepared,
  formula,
  family,
  link,
  robust,
  exponentiate,
  overdispersion_check,
  imputations = 5L,
  iterations = 5L,
  mi_outcome = "observed",
  offset = character(0),
  se_type = NULL,
  seed = 20260618L
) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("The mice package is required for GLM multiple imputation.", call. = FALSE)
  }
  if (!anyNA(raw_prepared)) {
    stop("No missing values were present in selected GLM variables; MI is not needed.", call. = FALSE)
  }
  outcome <- all.vars(formula)[[1]]
  mi_outcome <- generalized_resolve_mi_outcome(mi_outcome)
  observed_outcome <- if (outcome %in% names(raw_prepared)) !is.na(raw_prepared[[outcome]]) else rep(TRUE, nrow(raw_prepared))
  imputed <- mice::mice(
    raw_prepared,
    m = generalized_resolve_mi_count(imputations, minimum = 2L),
    maxit = generalized_resolve_mi_count(iterations, minimum = 1L),
    method = generalized_mice_method(raw_prepared),
    printFlag = FALSE,
    seed = seed
  )
  tables <- list()
  first_fit <- NULL
  failures <- character(0)
  for (index in seq_len(imputed$m)) {
    completed <- tryCatch(mice::complete(imputed, action = index), error = function(e) e)
    if (inherits(completed, "error")) {
      failures <- c(failures, sprintf("imputation %s completion failed: %s", index, conditionMessage(completed)))
      next
    }
    if (identical(family, "binomial")) {
      completed[[outcome]] <- pmin(1, pmax(0, round(suppressWarnings(as.numeric(completed[[outcome]])))))
    } else if (identical(family, "count")) {
      completed[[outcome]] <- pmax(0, round(suppressWarnings(as.numeric(completed[[outcome]]))))
    } else if (identical(family, "gamma")) {
      y <- suppressWarnings(as.numeric(completed[[outcome]]))
      positive_min <- suppressWarnings(min(y[y > 0], na.rm = TRUE))
      if (!is.finite(positive_min)) positive_min <- .Machine$double.eps
      y[!is.finite(y) | y <= 0] <- positive_min / 2
      completed[[outcome]] <- y
    }
    offset <- utils::head(as.character(offset %||% character(0)), 1)
    if (length(offset) == 1 && nzchar(offset) && offset %in% names(completed)) {
      exposure <- suppressWarnings(as.numeric(completed[[offset]]))
      positive_min <- suppressWarnings(min(exposure[exposure > 0], na.rm = TRUE))
      if (!is.finite(positive_min)) positive_min <- .Machine$double.eps
      exposure[!is.finite(exposure) | exposure <= 0] <- positive_min / 2
      completed[[offset]] <- exposure
    }
    if (identical(mi_outcome, "observed")) {
      completed <- completed[observed_outcome, , drop = FALSE]
    }
    completed <- completed[stats::complete.cases(completed), , drop = FALSE]
    fit <- tryCatch(
      generalized_fit_model(completed, formula, family, link, robust, exponentiate, overdispersion_check, se_type = se_type),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      failures <- c(failures, sprintf("imputation %s fit failed: %s", index, conditionMessage(fit)))
      next
    }
    if (is.null(first_fit)) first_fit <- fit
    tables[[length(tables) + 1L]] <- fit$coef_table
  }
  if (length(tables) == 0 || is.null(first_fit)) {
    stop(paste(failures, collapse = "; "), call. = FALSE)
  }
  pooled <- generalized_pool_coef_tables(tables, exponentiate = exponentiate)
  first_fit$coef_table <- pooled
  first_fit$robust_used <- isTRUE(attr(pooled, "robust_used"))
  first_fit$se_type_requested <- attr(pooled, "se_type_requested") %||% "model"
  first_fit$se_type_used <- attr(pooled, "se_type_used") %||% "model"
  outcome_note <- if (identical(mi_outcome, "observed")) {
    "Rows with originally missing dependent-variable values were excluded from each fitted imputed GLM."
  } else {
    "Rows with imputed dependent-variable values were included as a sensitivity analysis."
  }
  first_fit$fit_note <- unique(c(
    first_fit$fit_note,
    sprintf("Standard mice-based multiple imputation used %d fitted dataset(s); coefficients were pooled using Rubin-style total variance. %s", length(tables), outcome_note),
    if (length(failures) > 0) sprintf("MI warnings: %s", paste(failures, collapse = "; ")) else character(0)
  ))
  first_fit
}

generalized_ipw_diagnostics <- function(probability, weights, clipped_probability = NULL, raw_weights = NULL, clipped_weights = NULL, model_terms = character(0), fallback = "") {
  probability <- suppressWarnings(as.numeric(probability))
  probability <- probability[is.finite(probability)]
  weights <- suppressWarnings(as.numeric(weights))
  weights <- weights[is.finite(weights) & weights > 0]
  clipped_probability <- suppressWarnings(as.numeric(clipped_probability %||% probability))
  clipped_probability <- clipped_probability[is.finite(clipped_probability)]
  clipped_weights <- suppressWarnings(as.numeric(clipped_weights %||% weights))
  clipped_weights <- clipped_weights[is.finite(clipped_weights) & clipped_weights > 0]
  raw_weights <- suppressWarnings(as.numeric(raw_weights %||% weights))
  raw_weights <- raw_weights[is.finite(raw_weights) & raw_weights > 0]
  probability_clipped <- if (length(probability) > 0 && length(clipped_probability) == length(probability)) {
    sum(abs(clipped_probability - probability) > .Machine$double.eps^0.5, na.rm = TRUE)
  } else {
    NA_integer_
  }
  weight_clipped <- if (length(clipped_weights) > 0 && length(raw_weights) == length(clipped_weights)) {
    sum(abs(clipped_weights - raw_weights) > .Machine$double.eps^0.5, na.rm = TRUE)
  } else {
    NA_integer_
  }
  data.frame(
    Item = c(
      "Observation model variables",
      "Predicted observation probability: min",
      "Predicted observation probability: median",
      "Predicted observation probability: max",
      "Probability clipping count",
      "Final weight summary",
      "Effective sample size",
      "Weight clipping count",
      "Diagnostic note"
    ),
    Value = c(
      if (length(model_terms) > 0) paste(model_terms, collapse = ", ") else "Intercept only",
      if (length(probability) > 0) format_decimal3(min(probability)) else "",
      if (length(probability) > 0) format_decimal3(stats::median(probability)) else "",
      if (length(probability) > 0) format_decimal3(max(probability)) else "",
      if (is.finite(probability_clipped)) as.character(probability_clipped) else "",
      if (length(weights) > 0) sprintf("min=%s; median=%s; max=%s", format_decimal3(min(weights)), format_decimal3(stats::median(weights)), format_decimal3(max(weights))) else "",
      if (length(weights) > 0) format_decimal3(sum(weights)^2 / sum(weights^2)) else "",
      if (is.finite(weight_clipped)) as.character(weight_clipped) else "",
      if (nzchar(fallback)) fallback else "Review positivity and weight stability; report the observation model and any clipping."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_ipw_weights <- function(raw_prepared, analyzed_data, variables, predictors, auxiliary = character(0)) {
  observed <- stats::complete.cases(raw_prepared[, variables, drop = FALSE])
  analyzed_index <- match(rownames(analyzed_data), rownames(raw_prepared))
  if (length(unique(observed)) < 2 || anyNA(analyzed_index)) {
    weights <- rep(1, nrow(analyzed_data))
    return(list(
      weights = weights,
      diagnostics = generalized_ipw_diagnostics(rep(1, nrow(analyzed_data)), weights, fallback = "Observation status had no variation; unit weights were used."),
      note = "Observation status had no variation; unit weights were used."
    ))
  }
  auxiliary <- setdiff(as.character(auxiliary %||% character(0)), variables)
  candidate_terms <- intersect(unique(c(predictors, auxiliary)), names(raw_prepared))
  candidate_terms <- candidate_terms[vapply(candidate_terms, function(name) {
    values <- raw_prepared[[name]]
    !anyNA(values) && length(unique(values)) > 1
  }, logical(1))]
  weight_data <- raw_prepared
  weight_data$.statedu_observed <- as.integer(observed)
  if (length(candidate_terms) == 0) {
    probability <- mean(observed)
    weights <- rep(1 / probability, nrow(analyzed_data))
    weights <- weights / mean(weights)
    return(list(
      weights = weights,
      diagnostics = generalized_ipw_diagnostics(rep(probability, nrow(analyzed_data)), weights, model_terms = character(0), fallback = "No fully observed predictors were available for the observation model; intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis."),
      note = "No fully observed predictors were available for the observation model; intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis."
    ))
  }
  formula <- stats::reformulate(candidate_terms, response = ".statedu_observed")
  fit <- tryCatch(stats::glm(formula, data = weight_data, family = stats::binomial()), error = function(e) e)
  if (inherits(fit, "error")) {
    probability <- mean(observed)
    weights <- rep(1 / probability, nrow(analyzed_data))
    weights <- weights / mean(weights)
    note <- sprintf("Observation model failed (%s); intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis.", conditionMessage(fit))
    return(list(
      weights = weights,
      diagnostics = generalized_ipw_diagnostics(rep(probability, nrow(analyzed_data)), weights, model_terms = character(0), fallback = note),
      note = note
    ))
  }
  raw_probability <- suppressWarnings(stats::predict(fit, newdata = raw_prepared[analyzed_index, , drop = FALSE], type = "response"))
  probability <- pmin(pmax(raw_probability, 0.05), 0.99)
  raw_weights <- 1 / probability
  clipped_weights <- pmin(raw_weights, stats::quantile(raw_weights, 0.99, na.rm = TRUE))
  weights <- clipped_weights / mean(clipped_weights, na.rm = TRUE)
  list(
    weights = weights,
    diagnostics = generalized_ipw_diagnostics(raw_probability, weights, probability, raw_weights, clipped_weights, candidate_terms),
    note = sprintf("Observation model: %s; IPW clipped at the 99th percentile and normalized to mean 1. Report the observation model and review positivity/weight stability.", paste(candidate_terms, collapse = ", "))
  )
}

generalized_vif_table <- function(formula, data) {
  mm <- tryCatch(stats::model.matrix(formula, data = data), error = function(e) NULL)
  if (is.null(mm)) return(data.frame())
  vif <- coefficient_collinearity(mm)$vif
  vif <- vif[setdiff(names(vif), "(Intercept)")]
  if (length(vif) == 0) return(data.frame())
  data.frame(
    Term = names(vif),
    VIF = as.numeric(vif),
    Tolerance = ifelse(is.finite(vif) & vif != 0, 1 / as.numeric(vif), NA_real_),
    stringsAsFactors = FALSE
  )
}

generalized_logistic_sparse_summary <- function(data, outcome, predictors, variable_info = NULL) {
  notes <- character(0)
  for (predictor in predictors) {
    measurement <- generalized_measurement_for(predictor, variable_info)
    values <- data[[predictor]]
    if (!measurement %in% c("binary", "ordered", "category") && !is.factor(values)) next
    tab <- table(data[[outcome]], values, useNA = "no")
    if (length(tab) == 0) next
    if (any(tab == 0)) {
      notes <- c(notes, sprintf("Zero cell for outcome by %s", predictor))
    } else if (mean(tab < 5) >= 0.20) {
      notes <- c(notes, sprintf("Sparse cells for outcome by %s", predictor))
    }
  }
  unique(notes)
}

generalized_influence_summary <- function(model) {
  n <- tryCatch(stats::nobs(model), error = function(e) NA_real_)
  p <- length(stats::coef(model))
  cooks <- tryCatch(stats::cooks.distance(model), error = function(e) numeric(0))
  hat <- tryCatch(stats::hatvalues(model), error = function(e) numeric(0))
  cooks <- suppressWarnings(as.numeric(cooks))
  hat <- suppressWarnings(as.numeric(hat))
  cook_threshold <- if (is.finite(n) && n > 0) 4 / n else NA_real_
  leverage_threshold <- if (is.finite(n) && n > 0) 2 * p / n else NA_real_
  list(
    max_cook = if (length(cooks) > 0) max(cooks, na.rm = TRUE) else NA_real_,
    high_cook = if (is.finite(cook_threshold)) sum(cooks > cook_threshold, na.rm = TRUE) else NA_integer_,
    cook_threshold = cook_threshold,
    max_leverage = if (length(hat) > 0) max(hat, na.rm = TRUE) else NA_real_,
    high_leverage = if (is.finite(leverage_threshold)) sum(hat > leverage_threshold, na.rm = TRUE) else NA_integer_,
    leverage_threshold = leverage_threshold
  )
}

generalized_assumption_checks <- function(model, family, dispersion, data, formula, predictors = character(0), variable_info = NULL, show_vif = FALSE) {
  rows <- list()
  add <- function(check, result, statistic = NA_real_, p = NA_real_, interpretation = "", recommendation = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      Check = check,
      Result = result,
      Statistic = statistic,
      p = p,
      Interpretation = interpretation,
      Recommendation = recommendation,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  add(
    "Family / link",
    "Review",
    NA_real_,
    NA_real_,
    sprintf("Fitted family: %s, link: %s.", family, model$family$link %||% "log"),
    "Confirm that the outcome scale and scientific estimand match the selected GLM family."
  )

  add(
    "Independent observations",
    "Review",
    NA_real_,
    NA_real_,
    "Standard GLM assumes independent observations after conditioning on predictors.",
    "If observations are repeated, clustered, matched, or panel data, use GEE, LMM/GLMM, or cluster-robust/design-based analysis instead of ordinary GLM."
  )

  if (family %in% c("count", "negative_binomial")) {
    issue <- is.finite(dispersion) && dispersion > 1.5
    add(
      "Poisson dispersion screening",
      if (issue) "Flag" else "OK",
      dispersion,
      NA_real_,
      if (is.finite(dispersion)) sprintf("Pearson dispersion ratio = %.3f.", dispersion) else "Dispersion ratio could not be estimated.",
      if (issue) "Use negative binomial for count outcomes or robust standard errors; investigate model misspecification." else "No major overdispersion signal by the screening threshold."
    )
  }

  if (family %in% c("gaussian", "gamma")) {
    residuals <- stats::residuals(model, type = "deviance")
    if (length(residuals) >= 3 && length(residuals) <= 5000) {
      sw <- tryCatch(stats::shapiro.test(residuals), error = function(e) NULL)
      if (!is.null(sw)) {
        add(
          "Residual diagnostics",
          if (is.finite(sw$p.value) && sw$p.value < 0.05) "Flag" else "OK",
          unname(sw$statistic),
          sw$p.value,
          "Shapiro-Wilk screening was applied to deviance residuals; this is a diagnostic screen, not a normality requirement for non-Gaussian GLMs.",
          if (is.finite(sw$p.value) && sw$p.value < 0.05) "Inspect residual plots and consider robust inference or a more appropriate family/link." else "Residual diagnostic screening did not flag a major issue."
        )
      }
    }
  }

  if (identical(family, "binomial")) {
    y <- model.response(stats::model.frame(model))
    events <- suppressWarnings(min(table(y)))
    coefficient_count <- max(1, length(stats::coef(model)) - 1)
    epv <- suppressWarnings(as.numeric(events) / coefficient_count)
    add(
      "Events per variable",
      if (is.finite(epv) && epv < 10) "Flag" else "OK",
      epv,
      NA_real_,
      if (is.finite(epv)) sprintf("EPV = %.1f using the smaller outcome class and %d non-intercept coefficient(s).", epv, coefficient_count) else "EPV could not be estimated.",
      if (is.finite(epv) && epv < 5) "Estimates may be unstable; reduce predictors, collapse categories, or use penalized/Firth logistic regression." else if (is.finite(epv) && epv < 10) "Interpret coefficients cautiously and consider sensitivity analysis." else "Event count appears adequate by the common EPV screening rule."
    )
    fitted <- tryCatch(stats::fitted(model), error = function(e) numeric(0))
    coef_values <- tryCatch(stats::coef(model), error = function(e) numeric(0))
    separation <- any(!is.finite(coef_values)) || any(fitted < 1e-6 | fitted > 1 - 1e-6, na.rm = TRUE)
    add(
      "Separation risk",
      if (isTRUE(separation)) "Flag" else "OK",
      NA_real_,
      NA_real_,
      if (isTRUE(separation)) "Fitted probabilities or coefficients suggest possible complete/quasi-complete separation." else "No fitted-probability separation flag was detected.",
      if (isTRUE(separation)) "Collapse sparse categories, reduce predictors, or use Firth/penalized logistic regression." else "Continue to inspect sparse categories and confidence interval width."
    )
    sparse <- generalized_logistic_sparse_summary(data, all.vars(formula)[[1]], predictors, variable_info)
    add(
      "Sparse categorical cells",
      if (length(sparse) > 0) "Flag" else "OK",
      length(sparse),
      NA_real_,
      if (length(sparse) > 0) paste(sparse, collapse = "; ") else "No zero/sparse categorical predictor cell flag was detected.",
      if (length(sparse) > 0) "Collapse sparse levels or reduce categorical predictors before interpreting logistic coefficients." else "No sparse-cell action is suggested by this screen."
    )
  }

  influence <- generalized_influence_summary(model)
  influence_issue <- (is.finite(influence$high_cook) && influence$high_cook > 0) ||
    (is.finite(influence$high_leverage) && influence$high_leverage > 0)
  add(
    "Influential observations",
    if (isTRUE(influence_issue)) "Review" else "OK",
    influence$max_cook,
    NA_real_,
    sprintf(
      "Max Cook's D = %s; high Cook's D count = %s; max leverage = %s; high leverage count = %s.",
      format_decimal3(influence$max_cook),
      as.character(influence$high_cook),
      format_decimal3(influence$max_leverage),
      as.character(influence$high_leverage)
    ),
    if (isTRUE(influence_issue)) "Inspect influential records and report sensitivity analysis if conclusions change." else "No major influence flag by simple Cook's D and leverage screening rules."
  )

  if (isTRUE(show_vif)) {
    vifs <- generalized_vif_table(formula, data)
    max_vif <- if (nrow(vifs) > 0) max(vifs$VIF, na.rm = TRUE) else NA_real_
    issue <- is.finite(max_vif) && max_vif > 5
    add(
      "Collinearity",
      if (issue) "Flag" else "OK",
      max_vif,
      NA_real_,
      if (is.finite(max_vif)) sprintf("Maximum model-matrix VIF = %.3f.", max_vif) else "VIF could not be estimated.",
      if (issue) "Review correlated predictors or consider penalized regression for prediction-focused models." else "No major VIF signal by the screening threshold."
    )
  }

  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

generalized_fit_stats <- function(model, null_model = NULL, se_type = "model") {
  family <- model$family$family %||% ""
  n <- stats::nobs(model)
  residual_df <- stats::df.residual(model)
  log_likelihood <- generalized_log_likelihood(model)
  gaussian_r2 <- NA_real_
  gaussian_adjusted_r2 <- NA_real_
  pseudo_r2 <- NA_real_
  if (identical(family, "gaussian")) {
    deviance <- suppressWarnings(as.numeric(stats::deviance(model)))
    null_deviance <- suppressWarnings(as.numeric(model$null.deviance %||% NA_real_))
    if (is.finite(deviance) && is.finite(null_deviance) && null_deviance > 0) {
      gaussian_r2 <- 1 - deviance / null_deviance
      if (is.finite(n) && is.finite(residual_df) && residual_df > 0) {
        gaussian_adjusted_r2 <- 1 - (1 - gaussian_r2) * (n - 1) / residual_df
      }
    }
  } else if (!is.null(null_model)) {
    ll <- tryCatch(as.numeric(stats::logLik(model)), error = function(e) NA_real_)
    ll0 <- tryCatch(as.numeric(stats::logLik(null_model)), error = function(e) NA_real_)
    if (is.finite(ll) && is.finite(ll0) && ll0 != 0) {
      pseudo_r2 <- 1 - ll / ll0
    }
  }
  r2_items <- if (identical(family, "gaussian")) c("R2", "Adjusted R2") else "McFadden pseudo R2"
  r2_values <- if (identical(family, "gaussian")) {
    c(
      if (is.finite(gaussian_r2)) format_decimal3(gaussian_r2) else "",
      if (is.finite(gaussian_adjusted_r2)) format_decimal3(gaussian_adjusted_r2) else ""
    )
  } else {
    if (is.finite(pseudo_r2)) format_decimal3(pseudo_r2) else ""
  }
  data.frame(
    Item = c("N", "Log likelihood", "AIC", "BIC", "Residual df", "Dispersion", r2_items, "Standard errors"),
    Value = c(
      as.character(n),
      if (is.finite(log_likelihood)) format_decimal3(log_likelihood) else "",
      format_decimal3(stats::AIC(model)),
      format_decimal3(stats::BIC(model)),
      as.character(residual_df),
      format_decimal3(generalized_overdispersion_ratio(model)),
      r2_values,
      generalized_se_type_label(se_type)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_family_label <- function(family) {
  switch(
    as.character(family %||% "")[[1]],
    auto = "Auto",
    gaussian = "Linear Gaussian / identity",
    binomial = "Binary logistic / logit",
    gamma = "Gamma / log",
    count = "Count: Poisson or negative binomial / log",
    negative_binomial = "Negative binomial / log",
    as.character(family %||% "")[[1]]
  )
}

generalized_flag_summary <- function(checks) {
  if (!is.data.frame(checks) || nrow(checks) == 0) {
    return("Not run")
  }
  results <- as.character(checks$Result %||% character(0))
  flag_count <- sum(results %in% c("Flag", "Review"), na.rm = TRUE)
  ok_count <- sum(results == "OK", na.rm = TRUE)
  if (flag_count > 0) {
    flagged <- as.character(checks$Check[results %in% c("Flag", "Review")])
    return(sprintf("%d item(s) flagged/review: %s", flag_count, paste(flagged, collapse = "; ")))
  }
  sprintf("%d check(s) OK", ok_count)
}

generalized_family_selection_reason <- function(requested_family, detected_family, fitted_family) {
  if (!identical(requested_family, "auto")) {
    return(sprintf("User selected %s; fitted as %s.", generalized_family_label(requested_family), generalized_family_label(fitted_family)))
  }
  switch(
    detected_family,
    binomial = "Auto selected binary logistic GLM because the dependent variable is marked binary or has two observed outcome levels.",
    count = if (identical(fitted_family, "negative_binomial")) {
      "Auto selected the count-family workflow; Poisson overdispersion screening selected negative binomial as the final model."
    } else {
      "Auto selected the count-family workflow because the dependent variable is non-negative integer; Poisson was retained unless screening indicated otherwise."
    },
    gamma = "Auto selected Gamma GLM because the dependent variable is strictly positive and right-skewed.",
    gaussian = "Auto selected Gaussian GLM because the dependent variable is continuous without count/gamma/binary screening flags.",
    sprintf("Auto selected %s.", generalized_family_label(fitted_family))
  )
}

generalized_recommendation_text <- function(fitted_family, assumption_checks, count_details) {
  flags <- if (is.data.frame(assumption_checks) && nrow(assumption_checks) > 0) {
    as.character(assumption_checks$Check[as.character(assumption_checks$Result) %in% c("Flag", "Review")])
  } else {
    character(0)
  }
  if (identical(fitted_family, "negative_binomial")) {
    return("Use the negative-binomial GLM as the primary count model and report the Poisson overdispersion screening.")
  }
  if (identical(fitted_family, "count")) {
    zero_flag <- FALSE
    if (is.data.frame(count_details) && nrow(count_details) > 0 && "Zero screen" %in% count_details$Item) {
      zero_flag <- grepl("excess zeros", count_details$Value[match("Zero screen", count_details$Item)], ignore.case = TRUE)
    }
    if (isTRUE(zero_flag)) {
      return("Report Poisson GLM with caution and consider zero-inflated or hurdle sensitivity analysis.")
    }
    return("Use Poisson GLM if count screening remains acceptable; report dispersion and zero-screen diagnostics.")
  }
  if (length(flags) > 0) {
    return("Use the fitted GLM with caution and report flagged diagnostics; consider sensitivity analysis if conclusions depend on flagged items.")
  }
  "Use the fitted GLM as the primary analysis and report family, link, standard errors, missing-data strategy, and diagnostics."
}

generalized_decision_summary <- function(
  method,
  requested_family,
  detected_family,
  fitted_family,
  link,
  se_type_used,
  missing_strategy,
  missing_method,
  raw_n,
  analyzed_n,
  complete_case_n,
  assumption_checks,
  count_details
) {
  data.frame(
    Item = c(
      "Primary analysis",
      "Family selection",
      "Link",
      "Standard errors",
      "Missing-data handling",
      "Assumption check summary",
      "Recommended reporting"
    ),
    Value = c(
      method,
      generalized_family_selection_reason(requested_family, detected_family, fitted_family),
      as.character(link %||% ""),
      generalized_se_type_label(se_type_used),
      sprintf(
        "%s; analyzed %s of %s rows%s.",
        missing_method,
        as.character(analyzed_n %||% ""),
        as.character(raw_n %||% ""),
        if (!is.null(complete_case_n) && !identical(as.character(complete_case_n), as.character(analyzed_n))) {
          sprintf("; complete-case rows before missing-data engine: %s", as.character(complete_case_n))
        } else {
          ""
        }
      ),
      generalized_flag_summary(assumption_checks),
      generalized_recommendation_text(fitted_family, assumption_checks, count_details)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_value_display <- function(variable, value, category_table = NULL) {
  value <- as.character(value %||% "")
  if (!nzchar(value)) return("")
  value_labels <- category_value_label_lookup_static(category_table)
  label <- named_value(value_labels[[variable]], value, "")
  if (nzchar(label)) sprintf("%s (%s)", label, value) else value
}

generalized_variable_coding_summary <- function(
  outcome,
  predictors,
  exposure = character(0),
  family,
  link,
  coding_levels = list(),
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  reference_values = character(0)
) {
  row <- function(variable, role, coding) {
    data.frame(
      Variable = display_variable_name_static(variable, variable_info, labels, label_only = TRUE),
      Role = role,
      Measurement = generalized_measurement_for(variable, variable_info),
      Coding = coding,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  coding_for_predictor <- function(variable) {
    measurement <- generalized_measurement_for(variable, variable_info)
    levels <- as.character(coding_levels[[variable]] %||% character(0))
    if (!nzchar(measurement) && length(levels) > 0) {
      measurement <- if (length(levels) == 2) "binary" else "category"
    }
    reference <- trimws(named_value(reference_values, variable, ""))
    if (!nzchar(reference) && length(levels) > 0) reference <- levels[[1]]
    if (measurement %in% c("binary", "category")) {
      if (nzchar(reference)) {
        return(sprintf(
          "Factor with treatment contrasts; reference = %s.",
          generalized_value_display(variable, reference, category_table)
        ))
      }
      return("Factor with treatment contrasts; reference level could not be determined.")
    }
    if (identical(measurement, "ordered")) {
      if (length(levels) > 0) {
        return(sprintf(
          "Ordered factor; levels = %s. R ordered-factor contrasts are used unless the variable is recoded as numeric or nominal.",
          paste(vapply(levels, function(level) generalized_value_display(variable, level, category_table), character(1)), collapse = " < ")
        ))
      }
      return("Ordered factor; R ordered-factor contrasts are used unless the variable is recoded as numeric or nominal.")
    }
    "Continuous predictor; coefficient is per one-unit increase."
  }

  rows <- list()
  outcome_levels <- as.character(coding_levels[[outcome]] %||% character(0))
  outcome_coding <- switch(
    as.character(family %||% "")[[1]],
    binomial = if (length(outcome_levels) >= 2) {
      sprintf(
        "Binary outcome coded as event = %s, reference = %s.",
        generalized_value_display(outcome, outcome_levels[[2]], category_table),
        generalized_value_display(outcome, outcome_levels[[1]], category_table)
      )
    } else {
      "Binary outcome coded as event = second observed level and reference = first observed level."
    },
    gamma = sprintf("Strictly positive continuous outcome; %s link.", as.character(link %||% "")),
    count = sprintf("Non-negative integer count outcome; %s link.", as.character(link %||% "")),
    negative_binomial = sprintf("Non-negative integer count outcome; negative-binomial model with %s link.", as.character(link %||% "")),
    gaussian = sprintf("Continuous outcome; %s link.", as.character(link %||% "")),
    sprintf("Outcome modeled using %s family.", generalized_family_label(family))
  )
  rows[[length(rows) + 1L]] <- row(outcome, "Dependent variable", outcome_coding)
  exposure <- utils::head(as.character(exposure %||% character(0)), 1)
  if (length(exposure) == 1 && nzchar(exposure)) {
    rows[[length(rows) + 1L]] <- row(exposure, "Exposure / offset", "Offset applied as log(exposure); exposure values must be strictly positive.")
  }
  for (predictor in as.character(predictors %||% character(0))) {
    rows[[length(rows) + 1L]] <- row(predictor, "Independent variable", coding_for_predictor(predictor))
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

generalized_software_versions <- function(fitted_family, se_type_used = "model", missing_strategy = "complete") {
  package_row <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) return(NULL)
    data.frame(
      Software = package,
      Version = as.character(utils::packageVersion(package)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  rows <- list(data.frame(
    Software = "R",
    Version = paste(R.version$major, R.version$minor, sep = "."),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
  packages <- "stats"
  if (!identical(generalized_resolve_se_type(se_type_used, robust = FALSE), "model")) {
    packages <- c(packages, "sandwich", "lmtest")
  }
  if (identical(fitted_family, "negative_binomial")) {
    packages <- c(packages, "MASS")
  }
  if (identical(generalized_resolve_missing_strategy(missing_strategy), "mi")) {
    packages <- c(packages, "mice")
  }
  for (package in unique(packages)) {
    row <- package_row(package)
    if (!is.null(row)) rows[[length(rows) + 1L]] <- row
  }
  do.call(rbind, rows)
}

generalized_publication_notes <- function(result) {
  family_note <- switch(
    as.character(result$family %||% "")[[1]],
    gaussian = "Gaussian identity-link coefficients are mean differences on the original outcome scale.",
    binomial = "Binomial logit-link coefficients are log odds ratios; exp(B) is interpreted as an odds ratio.",
    gamma = "Gamma log-link coefficients are log mean ratios; exp(B) is interpreted as a mean ratio.",
    count = "Poisson log-link coefficients are log rate or mean ratios; RR denotes the exponentiated coefficient and is interpreted as a rate ratio when an exposure offset is used.",
    negative_binomial = "Negative-binomial log-link coefficients are log rate or mean ratios; RR denotes the exponentiated coefficient and accounts for overdispersion relative to Poisson.",
    "GLM coefficients are reported on the selected model link scale."
  )
  se_note <- sprintf("Standard errors are reported as %s.", generalized_se_type_label(result$se_type_used %||% "model"))
  missing_note <- sprintf(
    "Missing data were handled using %s; %s of %s row(s) were analyzed.",
    result$missing_method %||% "Complete-case",
    as.character(result$n %||% ""),
    as.character(result$raw_n %||% "")
  )
  offset_note <- if (length(result$exposure %||% character(0)) == 1) {
    sprintf("The exposure variable %s was included as offset(log(exposure)).", as.character(result$exposure))
  } else {
    "No exposure offset was included."
  }
  count_note <- if (is.data.frame(result$count_details) && nrow(result$count_details) > 0) {
    "For count outcomes, Poisson versus negative binomial selection used a prespecified dispersion-threshold screening rule; AIC/BIC were treated as supplementary fit diagnostics."
  } else {
    "Count-family Poisson/NB screening was not applicable."
  }
  data.frame(
    Note = c(
      sprintf("Primary analysis: %s.", result$method %||% "Generalized linear model"),
      "CI = confidence interval; SE = standard error.",
      family_note,
      se_note,
      missing_note,
      offset_note,
      count_note,
      "Variable coding, reference levels, family/link choice, diagnostics, and software versions should be reported with the coefficient table."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_reporting_checklist <- function(result) {
  assumption_count <- if (is.data.frame(result$assumption_checks)) nrow(result$assumption_checks) else 0L
  count_screening <- is.data.frame(result$count_details) && nrow(result$count_details) > 0
  data.frame(
    Item = c(
      "Model rationale",
      "Family and link reported",
      "Missing data described",
      "Variable coding described",
      "Effect estimate and 95% CI reported",
      "Standard error method reported",
      "Assumptions checked",
      "Count screening reported",
      "Publication table notes generated",
      "Software/package version reported",
      "Manuscript-ready text generated"
    ),
    Status = c(
      if (nzchar(result$model_rationale %||% "")) "Ready" else "Needs review",
      if (nzchar(result$family %||% "") && nzchar(result$link %||% "")) "Ready" else "Needs review",
      if (is.data.frame(result$missing_table) && nrow(result$missing_table) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$coding_summary) && nrow(result$coding_summary) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$coef_table) && all(c("LLCI", "ULCI") %in% names(result$coef_table))) "Ready" else "Needs review",
      if (nzchar(result$se_type_used %||% "")) "Ready" else "Needs review",
      if (assumption_count > 0) "Ready" else "Not selected",
      if (count_screening) "Ready" else "Not applicable",
      if (is.data.frame(result$publication_notes) && nrow(result$publication_notes) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$software_versions) && nrow(result$software_versions) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$manuscript_text) && nrow(result$manuscript_text) > 0) "Ready" else "Needs review"
    ),
    Details = c(
      result$model_rationale %||% "",
      sprintf("%s with %s link.", generalized_family_label(result$family), result$link %||% ""),
      sprintf("%s; analyzed %s of %s row(s).", result$missing_method %||% "", result$n %||% "", result$raw_n %||% ""),
      if (is.data.frame(result$coding_summary)) sprintf("%s coding row(s) generated.", nrow(result$coding_summary)) else "No coding summary was generated.",
      "Coefficient table includes B, SE, p-value, and 95% CI; exp(B) is added for logit/log models when selected.",
      generalized_se_type_label(result$se_type_used %||% "model"),
      sprintf("%s assumption check item(s) reported.", assumption_count),
      if (count_screening) "Poisson dispersion, zero screen, and supplementary AIC/BIC are reported." else "The selected outcome was not fitted through the Count workflow.",
      "Footnotes for family/link, standard errors, missing data, offset, and count screening are provided.",
      if (is.data.frame(result$software_versions)) paste(sprintf("%s %s", result$software_versions$Software, result$software_versions$Version), collapse = "; ") else "",
      "Suggested Methods, Results, Assumptions, and Software text is provided for manuscript drafting."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generalized_manuscript_text <- function(result) {
  non_intercept <- if (is.data.frame(result$coef_table) && "Term" %in% names(result$coef_table)) {
    result$coef_table[!grepl("^\\(Intercept\\)$", result$coef_table$Term), , drop = FALSE]
  } else {
    data.frame()
  }
  effect_sentence <- if (is.data.frame(non_intercept) && nrow(non_intercept) > 0) {
    terms <- utils::head(as.character(non_intercept$Term), 3)
    suffix <- if (nrow(non_intercept) > 3) " among other terms" else ""
    sprintf("Regression estimates were reported for %s%s with standard errors, p-values, and 95%% confidence intervals.", paste(terms, collapse = ", "), suffix)
  } else {
    "Regression estimates were reported with standard errors, p-values, and 95% confidence intervals."
  }
  exp_sentence <- if (isTRUE(result$exponentiate)) {
    "Exponentiated coefficients were additionally reported for logit/log-link effects."
  } else {
    ""
  }
  assumption_sentence <- if (is.data.frame(result$assumption_checks) && nrow(result$assumption_checks) > 0) {
    issue_rows <- result$assumption_checks[as.character(result$assumption_checks$Result) %in% c("Flag", "Review"), , drop = FALSE]
    if (nrow(issue_rows) > 0) {
      sprintf("Assumption screening flagged %s; recommended reporting cautions or sensitivity analyses were generated accordingly.", paste(unique(issue_rows$Check), collapse = ", "))
    } else {
      "Assumption screening did not flag a major issue among the selected GLM checks."
    }
  } else {
    "Assumption screening was not requested or no checks were selected."
  }
  count_sentence <- if (is.data.frame(result$count_details) && nrow(result$count_details) > 0) {
    "For count outcomes, Poisson versus negative binomial selection was based on the prespecified dispersion-threshold screening rule, with AIC/BIC reported as supplementary diagnostics."
  } else {
    ""
  }
  software_summary <- if (is.data.frame(result$software_versions) && nrow(result$software_versions) > 0) {
    sprintf("Analyses were performed using %s.", paste(sprintf("%s %s", result$software_versions$Software, result$software_versions$Version), collapse = ", "))
  } else {
    "Software and package versions should be reported."
  }
  data.frame(
    Section = c("Methods", "Results", "Assumptions", "Software"),
    SuggestedText = c(
      sprintf(
        "%s Missing data were handled using %s, analyzing %s of %s row(s). Standard errors were reported as %s.",
        result$model_rationale %||% "",
        result$missing_method %||% "complete-case analysis",
        result$n %||% "",
        result$raw_n %||% "",
        generalized_se_type_label(result$se_type_used %||% "model")
      ),
      trimws(paste(effect_sentence, exp_sentence, count_sentence)),
      assumption_sentence,
      software_summary
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

prepare_generalized_analysis_result <- function(
  data,
  outcome,
  predictors,
  exposure = character(0),
  family = "auto",
  link = "default",
  robust = TRUE,
  se_type = NULL,
  overdispersion = TRUE,
  assumption_checks = TRUE,
  show_vif = FALSE,
  exponentiate = TRUE,
  missing_strategy = "complete",
  missing_imputations = 5L,
  missing_iterations = 5L,
  mi_outcome = "observed",
  ipw_auxiliary = character(0),
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  reference_values = character(0)
) {
  outcome <- as.character(outcome %||% character(0))
  predictors <- as.character(predictors %||% character(0))
  exposure <- utils::head(as.character(exposure %||% character(0)), 1)
  outcome <- outcome[nzchar(outcome)]
  predictors <- predictors[nzchar(predictors)]
  exposure <- exposure[nzchar(exposure)]
  if (length(outcome) != 1) {
    stop("Select exactly one outcome variable for GLM.", call. = FALSE)
  }
  if (!isTRUE(generalized_outcome_allowed(outcome, variable_info))) {
    stop("GLM supports continuous or binary dependent variables. Use the logistic regression menu for categorical or ordinal dependent variables.", call. = FALSE)
  }
  if (length(exposure) == 1 && !isTRUE(generalized_offset_allowed(exposure, variable_info))) {
    stop("Exposure / offset must be a continuous positive variable.", call. = FALSE)
  }
  predictors <- setdiff(predictors, unique(c(outcome, exposure)))
  if (length(predictors) == 0) {
    stop("Select at least one predictor variable for GLM.", call. = FALSE)
  }

  requested_family <- as.character(family %||% "auto")[[1]]
  detected_family <- generalized_detect_family(data, outcome, requested_family, variable_info)
  link <- generalized_resolve_link(detected_family, link)
  se_type <- generalized_resolve_se_type(se_type, robust = robust)
  robust <- !identical(se_type, "model")
  variables <- unique(c(outcome, predictors, exposure))
  ipw_auxiliary <- setdiff(intersect(as.character(ipw_auxiliary %||% character(0)), names(data)), variables)
  missing_strategy <- generalized_resolve_missing_strategy(missing_strategy)
  mi_outcome <- generalized_resolve_mi_outcome(mi_outcome)
  if (length(reference_values) == 0) {
    reference_values <- regression_reference_values_static(category_table)
  }
  raw_prepared <- generalized_prepare_data(data, variables, outcome, detected_family, variable_info, reference_values, drop_complete = FALSE)
  ipw_prepared <- if (length(ipw_auxiliary) > 0) {
    generalized_prepare_data(data, unique(c(variables, ipw_auxiliary)), outcome, detected_family, variable_info, reference_values, drop_complete = FALSE)
  } else {
    raw_prepared
  }
  complete_index <- attr(raw_prepared, "complete_index") %||% stats::complete.cases(raw_prepared)
  coding_levels <- attr(raw_prepared, "coding_levels") %||% list()
  prepared <- raw_prepared[complete_index, , drop = FALSE]
  if (nrow(prepared) < 3 && !identical(missing_strategy, "mi")) {
    stop("At least 3 complete cases are required.", call. = FALSE)
  }
  raw_n <- attr(raw_prepared, "raw_n") %||% nrow(raw_prepared)
  complete_excluded_n <- attr(raw_prepared, "excluded") %||% 0L
  missing_table <- attr(raw_prepared, "missing_table") %||% data.frame()
  missing_pattern <- generalized_missing_pattern_summary(raw_prepared, complete_index, outcome, predictors, exposure)
  generalized_validate_outcome(raw_prepared, outcome, detected_family)
  generalized_validate_offset(raw_prepared, exposure)

  formula <- generalized_formula(outcome, predictors, offset = exposure)
  null_formula <- generalized_formula(outcome, character(0), offset = exposure)
  exponentiate <- isTRUE(exponentiate) && detected_family %in% c("binomial", "gamma", "count")
  missing_note <- character(0)
  missing_details <- data.frame(
    Item = character(0),
    Value = character(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (identical(missing_strategy, "mi") && anyNA(raw_prepared)) {
    missing_imputations <- generalized_resolve_mi_count(missing_imputations, default = 5L, minimum = 2L)
    missing_iterations <- generalized_resolve_mi_count(missing_iterations, default = 5L, minimum = 1L)
    fit <- generalized_fit_mi(
      raw_prepared,
      formula,
      detected_family,
      link,
      robust,
      exponentiate,
      overdispersion,
      imputations = missing_imputations,
      iterations = missing_iterations,
      mi_outcome = mi_outcome,
      offset = exposure,
      se_type = se_type
    )
    missing_note <- sprintf("Standard mice-based multiple imputation was selected for GLM missing-data handling (m = %d, iterations = %d).", missing_imputations, missing_iterations)
    missing_details <- data.frame(
      Item = c("Selected strategy", "MI datasets", "MI iterations", "Complete-case rows before MI", "Dependent-variable handling"),
      Value = c(
        generalized_missing_strategy_label(missing_strategy),
        as.character(missing_imputations),
        as.character(missing_iterations),
        sprintf("%d of %d", nrow(prepared), raw_n),
        if (identical(mi_outcome, "observed")) {
          "Rows with originally missing dependent-variable values are excluded from each fitted imputed GLM."
        } else if (anyNA(raw_prepared[[outcome]])) {
          "Rows with imputed dependent-variable values are included as a sensitivity analysis; report this explicitly."
        } else {
          "The dependent variable has no missing values in the selected GLM variables."
        }
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else if (identical(missing_strategy, "ipw") && complete_excluded_n > 0) {
    ipw <- generalized_ipw_weights(ipw_prepared, prepared, variables, predictors, ipw_auxiliary)
    fit <- generalized_fit_model(prepared, formula, detected_family, link, robust, exponentiate, overdispersion, weights = ipw$weights, se_type = se_type)
    missing_note <- "Inverse-probability weighting was selected for GLM missing-data handling."
    missing_details <- data.frame(
      Item = c("Selected strategy", "Complete-case rows", "Selected auxiliary variables", "IPW summary"),
      Value = c(
        generalized_missing_strategy_label(missing_strategy),
        sprintf("%d of %d", nrow(prepared), raw_n),
        if (length(ipw_auxiliary) > 0) paste(ipw_auxiliary, collapse = ", ") else "None selected",
        ipw$note
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    missing_details <- rbind(missing_details, ipw$diagnostics)
  } else {
    fit <- generalized_fit_model(prepared, formula, detected_family, link, robust, exponentiate, overdispersion, se_type = se_type)
    if (identical(missing_strategy, "mi") && !anyNA(raw_prepared)) {
      missing_note <- "MI was selected, but no missing values were present in selected GLM variables; complete data were fitted."
    } else if (identical(missing_strategy, "ipw") && complete_excluded_n == 0) {
      missing_note <- "IPW was selected, but no complete-case exclusion occurred; unweighted complete data were fitted."
    }
    missing_details <- data.frame(
      Item = c("Selected strategy", "Complete-case rows"),
      Value = c(generalized_missing_strategy_label(missing_strategy), sprintf("%d of %d", nrow(prepared), raw_n)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  null_model <- tryCatch(stats::glm(null_formula, data = prepared, family = fit$model$family), error = function(e) NULL)
  analyzed_n <- stats::nobs(fit$model)
  excluded_n <- max(0L, as.integer(raw_n) - as.integer(analyzed_n))
  assumption_checks <- if (isTRUE(assumption_checks)) {
    generalized_assumption_checks(
      fit$model,
      fit$fitted_family,
      fit$dispersion,
      prepared,
      formula,
      predictors = predictors,
      variable_info = variable_info,
      show_vif = show_vif
    )
  } else {
    data.frame()
  }

  outcome_label <- display_variable_name_static(outcome, variable_info, labels, label_only = TRUE)
  predictor_labels <- paste(vapply(predictors, display_variable_name_static, character(1), table = variable_info, labels = labels, label_only = TRUE), collapse = ", ")
  method <- switch(
    fit$fitted_family,
    gaussian = "General linear model",
    binomial = "Binary logistic GLM",
    gamma = "Gamma GLM",
    count = "Poisson GLM",
    negative_binomial = "Negative binomial GLM",
    "Generalized linear model"
  )
  rationale <- sprintf(
    "%s was fitted for dependent variable %s with independent variables %s. Requested family: %s; fitted family: %s.",
    method,
    outcome_label,
    predictor_labels,
    generalized_family_labels()[[requested_family]] %||% requested_family,
    fit$fitted_family
  )
  decision_summary <- generalized_decision_summary(
    method = method,
    requested_family = requested_family,
    detected_family = detected_family,
    fitted_family = fit$fitted_family,
    link = fit$model$family$link,
    se_type_used = fit$se_type_used %||% "model",
    missing_strategy = missing_strategy,
    missing_method = generalized_missing_strategy_label(missing_strategy),
    raw_n = raw_n,
    analyzed_n = analyzed_n,
    complete_case_n = nrow(prepared),
    assumption_checks = assumption_checks,
    count_details = fit$count_details
  )
  coding_summary <- generalized_variable_coding_summary(
    outcome = outcome,
    predictors = predictors,
    exposure = exposure,
    family = fit$fitted_family,
    link = fit$model$family$link,
    coding_levels = coding_levels,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table,
    reference_values = reference_values
  )

  result <- list(
    method = method,
    outcome = outcome,
    predictors = predictors,
    exposure = exposure,
    requested_family = requested_family,
    family = fit$fitted_family,
    link = fit$model$family$link,
    formula = formula,
    n = analyzed_n,
    raw_n = raw_n,
    excluded_n = excluded_n,
    complete_case_n = nrow(prepared),
    model = fit$model,
    coef_table = fit$coef_table,
    fit_stats = generalized_fit_stats(fit$model, null_model, se_type = fit$se_type_used %||% "model"),
    decision_summary = decision_summary,
    coding_summary = coding_summary,
    missing_table = missing_table,
    missing_pattern = missing_pattern,
    missing_method = generalized_missing_strategy_label(missing_strategy),
    missing_strategy = missing_strategy,
    mi_outcome = mi_outcome,
    ipw_auxiliary = ipw_auxiliary,
    missing_details = missing_details,
    count_details = fit$count_details,
    assumption_checks = assumption_checks,
    vif_table = if (isTRUE(show_vif)) generalized_vif_table(formula, prepared) else data.frame(),
    software_versions = generalized_software_versions(fit$fitted_family, fit$se_type_used %||% "model", missing_strategy),
    robust = isTRUE(fit$robust_used),
    se_type_requested = fit$se_type_requested %||% se_type,
    se_type_used = fit$se_type_used %||% "model",
    exponentiate = exponentiate,
    overdispersion = fit$dispersion,
    notes = unique(c(
      fit$fit_note,
      missing_note,
      if (excluded_n > 0 && identical(missing_strategy, "complete")) {
        sprintf(
          "Complete-case GLM excluded %d of %d rows with missing values in selected analysis variables.",
          as.integer(excluded_n),
          as.integer(raw_n)
        )
      } else if (!identical(missing_strategy, "complete") && complete_excluded_n > 0) {
        sprintf("Complete-case rows before missing-data engine: %d of %d.", nrow(prepared), as.integer(raw_n))
      } else {
        "No rows were excluded for missing values in selected analysis variables."
      },
      if (isTRUE(fit$robust_used)) {
        sprintf("Coefficient standard errors use %s sandwich robust covariance.", fit$se_type_used %||% se_type)
      } else if (!identical(se_type, "model")) {
        sprintf("Robust standard errors (%s) were requested, but robust covariance could not be computed; model-based covariance is shown.", se_type)
      } else {
        "Coefficient standard errors use model-based covariance."
      },
      if (identical(fit$fitted_family, "negative_binomial")) "Poisson and negative binomial were treated as one count-family workflow; overdispersion screening selected the final fitted model." else character(0),
      if (length(exposure) == 1) sprintf("Exposure offset applied as log(%s).", display_variable_name_static(exposure, variable_info, labels, label_only = TRUE)) else character(0)
    )),
    model_rationale = rationale
  )
  result$publication_notes <- generalized_publication_notes(result)
  result$manuscript_text <- generalized_manuscript_text(result)
  result$reporting_checklist <- generalized_reporting_checklist(result)
  result
}
