# Longitudinal, clustered, and panel model helpers.

longitudinal_model_choices <- function() {
  c(
    "GEE: population-averaged model" = "gee",
    "LMM: linear mixed model" = "lmm",
    "GLMM: generalized linear mixed model" = "glmm",
    "Panel fixed effects" = "panel_fe",
    "Panel random effects" = "panel_re"
  )
}

longitudinal_family_choices <- function() {
  c(
    "Auto" = "auto",
    "Linear: Gaussian / identity" = "gaussian",
    "Binary: logistic / logit" = "binomial",
    "Gamma: positive skewed continuous / log" = "gamma",
    "Count: Poisson or negative binomial / log" = "count"
  )
}

longitudinal_correlation_choices <- function() {
  c(
    "Exchangeable" = "exchangeable",
    "AR(1)" = "ar1",
    "Independence" = "independence",
    "Unstructured" = "unstructured"
  )
}

longitudinal_weight_type_choices <- function(model_type = NULL, has_weight = TRUE) {
  model_type <- as.character(model_type %||% "")[[1]]
  if (model_type %in% c("lmm", "glmm")) {
    return(c("No weights" = "none"))
  }
  has_weight <- isTRUE(has_weight)
  if (!has_weight) {
    return(c("No weights" = "none"))
  }
  weighted_choices <- c(
    "Sampling / baseline longitudinal weight" = "sampling",
    "Time-varying longitudinal weight" = "longitudinal",
    "Analysis weight x generated IPW" = "combined"
  )
  weighted_choices
}

longitudinal_resolve_weight_type <- function(weight_type) {
  weight_type <- as.character(weight_type %||% "none")[[1]]
  if (weight_type %in% c("none", "sampling", "longitudinal", "ipw", "combined")) {
    return(weight_type)
  }
  "none"
}

longitudinal_default_weight_type <- function(model_type = NULL, has_weight = FALSE) {
  model_type <- as.character(model_type %||% "")[[1]]
  if (!isTRUE(has_weight) || model_type %in% c("lmm", "glmm")) {
    return("none")
  }
  "longitudinal"
}

longitudinal_resolve_context_weight_type <- function(weight_type, model_type = NULL, has_weight = FALSE) {
  model_type <- as.character(model_type %||% "")[[1]]
  if (model_type %in% c("lmm", "glmm")) {
    return("none")
  }
  choices <- longitudinal_weight_type_choices(model_type, has_weight)
  weight_type <- longitudinal_resolve_weight_type(weight_type)
  if (weight_type %in% unname(choices)) {
    return(weight_type)
  }
  longitudinal_default_weight_type(model_type, has_weight)
}

longitudinal_weight_type_label <- function(weight_type) {
  choices <- c(
    "No weights" = "none",
    "Sampling / baseline longitudinal weight" = "sampling",
    "Time-varying longitudinal weight" = "longitudinal",
    "Generated IPW for dropout" = "ipw",
    "Analysis weight x generated IPW" = "combined"
  )
  weight_type <- longitudinal_resolve_weight_type(weight_type)
  names(choices)[match(weight_type, choices)]
}

longitudinal_weight_type_detail <- function(weight_type, model_type = NULL) {
  weight_type <- longitudinal_resolve_weight_type(weight_type)
  model_type <- as.character(model_type %||% "")[[1]]
  if (model_type %in% c("lmm", "glmm")) {
    return("No analysis weights are applied. LMM / GLMM weighted likelihood is not recommended as a routine default in this module because its interpretation depends strongly on the sampling design, target estimand, and software-specific likelihood definition. Use GEE for weighted marginal longitudinal inference, or report LMM / GLMM weights only as a separate design-justified sensitivity analysis outside the primary model.")
  }
  caution <- if (model_type %in% c("lmm", "glmm") && !identical(weight_type, "none")) {
    " For LMM / GLMM, weighted likelihood is less standardized than GEE or survey-weighted marginal models; report the rationale and treat this as design/sensitivity analysis unless the target estimand requires weights."
  } else {
    ""
  }
  paste0(
    switch(
      weight_type,
      none = "No analysis weights are applied. This is the default for LMM / GLMM unless the study design requires sampling or longitudinal weights.",
      sampling = "Uses the selected variable as a baseline sampling/design weight. Use this when the data were sampled with unequal probabilities or require survey/design weighting.",
      longitudinal = "Uses the selected variable as a time-varying longitudinal analysis weight. This is usually the primary weighted option for GEE when weights vary by visit or person-period.",
      combined = "Multiplies the selected analysis weight by generated inverse-probability weights for missingness/dropout sensitivity. Use this when both sampling/design weights and observation-process weights are needed.",
      "No analysis weights are applied."
    ),
    caution
  )
}

longitudinal_weight_trim_choices <- function() {
  c(
    "None" = "none",
    "1st-99th percentile" = "p01_99",
    "5th-95th percentile" = "p05_95"
  )
}

longitudinal_resolve_weight_trim <- function(trim) {
  trim <- as.character(trim %||% "none")[[1]]
  if (trim %in% unname(longitudinal_weight_trim_choices())) {
    return(trim)
  }
  "none"
}

longitudinal_weight_trim_label <- function(trim) {
  choices <- longitudinal_weight_trim_choices()
  trim <- longitudinal_resolve_weight_trim(trim)
  names(choices)[match(trim, choices)]
}

longitudinal_weight_trim_detail <- function(trim) {
  switch(
    longitudinal_resolve_weight_trim(trim),
    p01_99 = "Winsorizes extreme final weights below the 1st percentile and above the 99th percentile. This is a mild stability option for occasional extreme weights.",
    p05_95 = "Winsorizes final weights below the 5th percentile and above the 95th percentile. This is stronger and should be reported as a sensitivity choice because it can change the target weighted estimand.",
    "No trimming is applied. Use this when weights are stable or when preserving the original design weights is more important than variance stabilization."
  )
}

longitudinal_missing_method_choices <- function(model_type = NULL) {
  c(
    "Complete-case: row-wise" = "row_complete",
    "Likelihood-based MAR: available repeated measures" = "available",
    "Complete-subject analysis" = "subject_complete"
  )
}

longitudinal_default_missing_method <- function(model_type = NULL) {
  model_type <- as.character(model_type %||% "")[[1]]
  if (model_type %in% c("lmm", "glmm")) {
    return("available")
  }
  "row_complete"
}

longitudinal_missing_strategy_choices <- function(model_type = NULL) {
  model_type <- as.character(model_type %||% "")[[1]]
  if (identical(model_type, "gee")) {
    return(c(
      "Complete-case: row-wise" = "row_complete",
      "Complete-subject analysis" = "subject_complete",
      "Multiple imputation (MI)" = "mi",
      "Inverse probability weighting (IPW)" = "ipw",
      "Weighted GEE (WGEE)" = "wgee"
    ))
  }
  if (model_type %in% c("lmm", "glmm")) {
    return(c(
      "Likelihood-based MAR: available repeated measures" = "available",
      "Complete-case: row-wise" = "row_complete",
      "Complete-subject analysis" = "subject_complete",
      "Multiple imputation (MI)" = "mi",
      "Inverse probability weighting (IPW)" = "ipw"
    ))
  }
  if (model_type %in% c("panel_fe", "panel_re")) {
    return(c(
      "Complete-case: row-wise" = "row_complete",
      "Complete-subject analysis" = "subject_complete",
      "Multiple imputation (MI)" = "mi",
      "Inverse probability weighting (IPW)" = "ipw"
    ))
  }
  c(
    "Complete-case: row-wise" = "row_complete",
    "Complete-subject analysis" = "subject_complete",
    "Multiple imputation (MI)" = "mi",
    "Inverse probability weighting (IPW)" = "ipw"
  )
}

longitudinal_default_missing_strategy <- function(model_type = NULL) {
  longitudinal_default_missing_method(model_type)
}

longitudinal_resolve_missing_strategy <- function(strategy, model_type = NULL) {
  strategy <- as.character(strategy %||% longitudinal_default_missing_strategy(model_type))[[1]]
  choices <- longitudinal_missing_strategy_choices(model_type)
  if (strategy %in% unname(choices)) {
    return(strategy)
  }
  longitudinal_default_missing_strategy(model_type)
}

longitudinal_missing_strategy_method <- function(strategy, model_type = NULL) {
  strategy <- longitudinal_resolve_missing_strategy(strategy, model_type)
  if (strategy %in% c("row_complete", "available", "subject_complete")) {
    return(strategy)
  }
  longitudinal_default_missing_method(model_type)
}

longitudinal_missing_strategy_engines <- function(strategy, model_type = NULL) {
  strategy <- longitudinal_resolve_missing_strategy(strategy, model_type)
  if (strategy %in% c("mi", "ipw", "wgee")) {
    return(strategy)
  }
  character(0)
}

longitudinal_resolve_missing_method <- function(method) {
  method <- as.character(method %||% "row_complete")[[1]]
  if (method %in% unname(longitudinal_missing_method_choices())) {
    return(method)
  }
  "row_complete"
}

longitudinal_missing_method_label <- function(method) {
  switch(
    longitudinal_resolve_missing_method(method),
    available = "likelihood-based mixed-model analysis using available repeated measures under a MAR assumption",
    subject_complete = "complete-subject analysis",
    "complete-case analysis using row-wise complete observations"
  )
}

longitudinal_missing_method_detail <- function(method, model_type = NULL) {
  method <- longitudinal_resolve_missing_method(method)
  model_type <- as.character(model_type %||% "")[[1]]
  switch(
    method,
    available = if (model_type %in% c("lmm", "glmm")) {
      "Fits the unbalanced LMM / GLMM likelihood using observed repeated-measure records with complete selected model variables. A subject is not removed only because an outcome is missing at another visit, but rows with missing outcome, covariates, ID, or time for the fitted model are not imputed. This is appropriate as the primary mixed-model analysis when the MAR assumption is defensible; add MI or IPW sensitivity when covariate missingness or dropout mechanisms are important."
    } else if (identical(model_type, "gee")) {
      "Plain GEE is not likelihood-based. Available model rows can be fitted, but MAR dropout should be supported with MI, IPW, or WGEE sensitivity analysis."
    } else {
      "Uses rows with observed selected model variables. Report whether the panel is balanced, and add MI or weighting sensitivity when missingness may depend on observed history."
    },
    subject_complete = "Keeps only subjects / clusters with complete records on selected analysis variables. This is conservative and easy to report, but can reduce power and introduce bias when dropout is related to observed outcomes or covariates.",
    "Drops rows with missing selected analysis variables before fitting the model. This is transparent, but it is strongest when missingness is plausibly MCAR or when missingness is minimal."
  )
}

longitudinal_resolve_missing_strategies <- function(strategies, model_type = NULL) {
  strategies <- unique(as.character(strategies %||% character(0)))
  allowed <- if (identical(as.character(model_type %||% "")[[1]], "gee")) c("mi", "ipw", "wgee") else c("mi", "ipw")
  intersect(strategies, allowed)
}

longitudinal_resolve_mi_count <- function(value, default = 5L, minimum = 2L, maximum = 50L) {
  value <- suppressWarnings(as.integer(value %||% default))
  if (length(value) == 0 || is.na(value)) {
    value <- default
  }
  max(minimum, min(maximum, value[[1]]))
}

longitudinal_missing_strategy_labels <- function(strategies, model_type = NULL) {
  choices <- longitudinal_missing_strategy_choices(model_type)
  keys <- longitudinal_resolve_missing_strategies(strategies, model_type)
  unname(names(choices)[match(keys, choices)])
}

longitudinal_missing_strategy_details <- function(strategies, model_type = NULL) {
  keys <- longitudinal_resolve_missing_strategies(strategies, model_type)
  if (length(keys) == 0) {
    return(character(0))
  }
  unname(vapply(keys, function(key) {
    switch(
      key,
      mi = "MI: standard mice-based missing-data sensitivity analysis. It imputes incomplete selected model variables, including the dependent variable when missing, fits the selected model in each imputed dataset, and pools estimates using Rubin's rules; it is not a dedicated multilevel MI engine and should usually be reported as sensitivity for LMM / GLMM.",
      ipw = "IPW: sensitivity analysis that estimates complete-observation probabilities from observed predictors and refits the selected model with inverse-probability weights. Positivity and weight stability should be reviewed.",
      wgee = "WGEE: GEE-only sensitivity analysis that refits GEE with inverse-probability observation weights for MAR dropout sensitivity. Positivity and weight construction should be reported.",
      ""
    )
  }, character(1)))
}

longitudinal_missing_strategy_detail <- function(strategy, model_type = NULL) {
  strategy <- longitudinal_resolve_missing_strategy(strategy, model_type)
  if (strategy %in% c("row_complete", "available", "subject_complete")) {
    return(longitudinal_missing_method_detail(strategy, model_type))
  }
  details <- longitudinal_missing_strategy_details(strategy, model_type)
  if (length(details) > 0) {
    return(details[[1]])
  }
  ""
}

longitudinal_mi_outcome_choices <- function() {
  c(
    "Use rows with observed dependent variable (recommended)" = "observed",
    "Impute missing dependent variable for sensitivity analysis" = "impute"
  )
}

longitudinal_resolve_mi_outcome <- function(value) {
  value <- as.character(value %||% "observed")[[1]]
  if (value %in% unname(longitudinal_mi_outcome_choices())) value else "observed"
}

longitudinal_measurement_for <- function(name, variable_info = NULL) {
  info <- normalize_regression_variable_info_static(variable_info)
  if (is.null(info) || !name %in% info$name) {
    return("")
  }
  measurement <- tolower(as.character(info$measurement[match(name, info$name)] %||% ""))
  if (identical(measurement, "ordinal")) measurement <- "ordered"
  if (identical(measurement, "nominal")) measurement <- "category"
  measurement
}

longitudinal_binary_outcome_numeric <- function(values) {
  if (is.factor(values) || is.character(values) || is.logical(values)) {
    factor_values <- droplevels(as.factor(values))
    if (nlevels(factor_values) != 2) {
      stop("Binary longitudinal models require exactly two observed dependent-variable levels.", call. = FALSE)
    }
    return(as.integer(factor_values == levels(factor_values)[[2]]))
  }
  observed <- stats::na.omit(values)
  levels <- sort(unique(observed))
  if (length(levels) != 2) {
    stop("Binary longitudinal models require exactly two observed dependent-variable values.", call. = FALSE)
  }
  as.integer(values == levels[[2]])
}

longitudinal_auto_family <- function(data, outcome, variable_info = NULL, family = "auto") {
  family <- as.character(family %||% "auto")[[1]]
  if (!identical(family, "auto")) {
    return(family)
  }
  measurement <- longitudinal_measurement_for(outcome, variable_info)
  if (identical(measurement, "binary")) {
    return("binomial")
  }
  if (measurement %in% c("ordered", "category")) {
    return("gaussian")
  }
  if (identical(measurement, "continuous")) {
    values <- data[[outcome]]
    numeric_values <- suppressWarnings(as.numeric(values))
    numeric_values <- numeric_values[!is.na(numeric_values)]
    if (longitudinal_count_outcome_candidate(numeric_values)) {
      return("count")
    }
    if (longitudinal_gamma_outcome_candidate(numeric_values)) {
      return("gamma")
    }
    return("gaussian")
  }
  values <- data[[outcome]]
  numeric_values <- suppressWarnings(as.numeric(values))
  numeric_values <- numeric_values[!is.na(numeric_values)]
  if (longitudinal_count_outcome_candidate(numeric_values)) {
    return("count")
  }
  if (longitudinal_gamma_outcome_candidate(numeric_values)) {
    return("gamma")
  }
  "gaussian"
}

longitudinal_is_integer_like <- function(values) {
  length(values) > 0 && all(is.finite(values)) && all(abs(values - round(values)) < .Machine$double.eps^0.5)
}

longitudinal_count_outcome_candidate <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (!longitudinal_is_integer_like(values) || any(values < 0)) {
    return(FALSE)
  }
  unique_count <- length(unique(values))
  max_value <- max(values)
  has_zero <- any(values == 0)
  # Avoid classifying short bounded score scales such as 1-5 or 1-10 as counts.
  is_bounded_score <- !has_zero && max_value <= 10 && unique_count <= 10
  if (is_bounded_score) {
    return(FALSE)
  }
  has_zero || max_value > 10 || unique_count > 10
}

longitudinal_gamma_outcome_candidate <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (length(values) < 8 || any(values <= 0) || longitudinal_is_integer_like(values)) {
    return(FALSE)
  }
  sd_value <- stats::sd(values)
  if (!is.finite(sd_value) || sd_value <= 0) {
    return(FALSE)
  }
  skewness <- mean(((values - mean(values)) / sd_value)^3)
  is.finite(skewness) && skewness > 1
}

longitudinal_negative_binomial_theta <- function(formula, data, weights = NULL) {
  theta <- tryCatch(
    {
      fit <- if (is.null(weights)) {
        MASS::glm.nb(formula, data = data, link = "log")
      } else {
        MASS::glm.nb(formula, data = data, weights = weights, link = "log")
      }
      suppressWarnings(as.numeric(fit$theta))
    },
    error = function(e) NA_real_
  )
  if (length(theta) == 1 && is.finite(theta) && theta > 0) {
    return(theta)
  }
  1
}

longitudinal_family_object <- function(family, formula = NULL, data = NULL, weights = NULL) {
  switch(
    as.character(family %||% "gaussian")[[1]],
    binomial = stats::binomial(link = "logit"),
    count = stats::poisson(link = "log"),
    poisson = stats::poisson(link = "log"),
    negative_binomial = MASS::negative.binomial(
      theta = longitudinal_negative_binomial_theta(formula, data, weights),
      link = "log"
    ),
    gamma = stats::Gamma(link = "log"),
    stats::gaussian(link = "identity")
  )
}

longitudinal_log_ratio_families <- function() {
  c("binomial", "poisson", "negative_binomial", "gamma")
}

longitudinal_offset_families <- function() {
  c("count", "poisson", "negative_binomial")
}

longitudinal_family_outcome_issue <- function(data, outcome, family) {
  values <- suppressWarnings(as.numeric(data[[outcome]]))
  values <- values[!is.na(values)]
  family <- as.character(family %||% "gaussian")[[1]]
  if (family %in% c("count", "poisson", "negative_binomial")) {
    if (length(values) == 0 || any(values < 0) || any(abs(values - round(values)) > .Machine$double.eps^0.5)) {
      return("Count models require a non-negative integer outcome.")
    }
  }
  if (identical(family, "gamma")) {
    if (length(values) == 0 || any(values <= 0)) {
      return("Gamma models require a strictly positive continuous outcome.")
    }
  }
  ""
}

longitudinal_screening_metric <- function(model, metric) {
  value <- tryCatch(
    switch(metric, aic = stats::AIC(model), bic = stats::BIC(model), NA_real_),
    error = function(e) NA_real_
  )
  suppressWarnings(as.numeric(value))[1]
}

longitudinal_count_dispersion_ratio <- function(model) {
  residuals <- tryCatch(stats::residuals(model, type = "pearson"), error = function(e) numeric(0))
  residuals <- suppressWarnings(as.numeric(residuals))
  residuals <- residuals[is.finite(residuals)]
  df <- tryCatch(stats::df.residual(model), error = function(e) NA_real_)
  if (!is.finite(df) || df <= 0 || length(residuals) == 0) {
    return(NA_real_)
  }
  sum(residuals^2, na.rm = TRUE) / df
}

longitudinal_count_zero_screen <- function(model, data, outcome) {
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

longitudinal_fit_count_screen_model <- function(
  data,
  formula,
  model_type,
  family,
  id,
  time,
  cluster = character(0),
  random_slope = FALSE,
  weights = NULL
) {
  data <- as.data.frame(data)
  if (!is.null(weights)) {
    data$.statedu_weights <- suppressWarnings(as.numeric(weights))
  }
  if (identical(model_type, "glmm")) {
    random <- longitudinal_mixed_random_terms(id, time, cluster, random_slope)
    mixed_formula <- stats::as.formula(paste(deparse(formula), "+", paste(random, collapse = " + ")))
    return(tryCatch({
      if (identical(family, "negative_binomial")) {
        if (is.null(weights)) {
          lme4::glmer.nb(mixed_formula, data = data, control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
        } else {
          lme4::glmer.nb(mixed_formula, data = data, weights = .statedu_weights, control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
        }
      } else if (is.null(weights)) {
        lme4::glmer(mixed_formula, data = data, family = stats::poisson(link = "log"), control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
      } else {
        lme4::glmer(mixed_formula, data = data, weights = .statedu_weights, family = stats::poisson(link = "log"), control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
      }
    }, error = function(e) e))
  }
  tryCatch({
    if (identical(family, "negative_binomial")) {
      if (is.null(weights)) {
        MASS::glm.nb(formula, data = data, link = "log")
      } else {
        MASS::glm.nb(formula, data = data, weights = .statedu_weights, link = "log")
      }
    } else if (is.null(weights)) {
      stats::glm(formula, data = data, family = stats::poisson(link = "log"))
    } else {
      stats::glm(formula, data = data, weights = .statedu_weights, family = stats::poisson(link = "log"))
    }
  }, error = function(e) e)
}

longitudinal_count_family_selection <- function(
  data,
  formula,
  model_type,
  id,
  time,
  cluster = character(0),
  random_slope = FALSE,
  weights = NULL
) {
  threshold <- 1.5
  poisson_fit <- longitudinal_fit_count_screen_model(data, formula, model_type, "poisson", id, time, cluster, random_slope, weights)
  poisson_ok <- !inherits(poisson_fit, "error")
  dispersion <- if (poisson_ok) longitudinal_count_dispersion_ratio(poisson_fit) else NA_real_
  outcome_name <- all.vars(formula)[[1]]
  zero_screen <- if (poisson_ok) longitudinal_count_zero_screen(poisson_fit, data, outcome_name) else list(observed = NA_real_, expected = NA_real_, ratio = NA_real_, flag = FALSE)
  poisson_aic <- if (poisson_ok) longitudinal_screening_metric(poisson_fit, "aic") else NA_real_
  poisson_bic <- if (poisson_ok) longitudinal_screening_metric(poisson_fit, "bic") else NA_real_
  nb_fit <- if (poisson_ok && is.finite(dispersion) && dispersion > threshold) {
    longitudinal_fit_count_screen_model(data, formula, model_type, "negative_binomial", id, time, cluster, random_slope, weights)
  } else {
    NULL
  }
  nb_ok <- !is.null(nb_fit) && !inherits(nb_fit, "error")
  nb_aic <- if (nb_ok) longitudinal_screening_metric(nb_fit, "aic") else NA_real_
  nb_bic <- if (nb_ok) longitudinal_screening_metric(nb_fit, "bic") else NA_real_
  selected <- if (is.finite(dispersion) && dispersion > threshold && nb_ok) "negative_binomial" else "poisson"
  decision <- if (identical(selected, "negative_binomial")) {
    "Poisson overdispersion exceeded the prespecified screening threshold and negative binomial fit was available."
  } else if (!poisson_ok) {
    "Poisson screening failed; Poisson family is retained for fitting and model errors will be reported if fitting fails."
  } else if (!is.finite(dispersion)) {
    "Poisson dispersion could not be computed; Poisson family is retained."
  } else if (dispersion > threshold) {
    "Poisson overdispersion exceeded the prespecified threshold, but negative binomial screening did not fit; Poisson family is retained with overdispersion warning."
  } else {
    "Poisson dispersion did not exceed the prespecified screening threshold."
  }
  details <- data.frame(
    Item = c(
      "Requested family",
      "Count decision",
      "Selection rule",
      "Poisson dispersion ratio",
      "Overdispersion threshold",
      "Observed zero proportion",
      "Poisson expected zero proportion",
      "Zero-inflation ratio",
      "Zero-inflation screen",
      "Poisson screening AIC",
      "Poisson screening BIC",
      "Negative binomial screening AIC",
      "Negative binomial screening BIC",
      "Decision rule"
    ),
    Value = c(
      "Count: Poisson or negative binomial / log",
      switch(selected, poisson = "Poisson / log", negative_binomial = "Negative binomial / log", selected),
      "Dispersion-threshold screening selects Poisson versus negative binomial; AIC/BIC are reported as supplementary fit diagnostics, not as the automatic selection rule.",
      longitudinal_format_number_static(dispersion),
      longitudinal_format_number_static(threshold),
      longitudinal_format_number_static(zero_screen$observed),
      longitudinal_format_number_static(zero_screen$expected),
      longitudinal_format_number_static(zero_screen$ratio),
      if (isTRUE(zero_screen$flag)) "Possible excess zeros; consider zero-inflated or hurdle sensitivity analysis." else "No excess-zero flag by the simple Poisson zero screen.",
      longitudinal_format_number_static(poisson_aic),
      longitudinal_format_number_static(poisson_bic),
      longitudinal_format_number_static(nb_aic),
      longitudinal_format_number_static(nb_bic),
      decision
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(family = selected, details = details, note = decision)
}

longitudinal_model_label <- function(model_type, family = NULL) {
  family <- as.character(family %||% "auto")[[1]]
  switch(
    as.character(model_type %||% "")[[1]],
    gee = if (identical(family, "negative_binomial")) "Marginal negative binomial GLM (subject-cluster robust SE)" else sprintf("GEE (%s)", family),
    lmm = "Linear mixed model",
    glmm = sprintf("Generalized linear mixed model (%s)", family),
    panel_fe = "Panel fixed-effects regression",
    panel_re = "Panel random-effects regression",
    "Longitudinal model"
  )
}

longitudinal_required_package <- function(model_type) {
  switch(
    as.character(model_type %||% "")[[1]],
    gee = "geepack",
    lmm = "lmerTest",
    glmm = "lme4",
    panel_fe = "plm",
    panel_re = "plm",
    ""
  )
}

longitudinal_guard_row <- function(outcome, id, time, predictors, reason, n = NA_integer_, variable_info = NULL, type = "Skipped") {
  data.frame(
    Type = type,
    `Dependent variable` = display_variable_name_static(outcome, variable_info, character(0), label_only = TRUE),
    ID = display_variable_name_static(id, variable_info, character(0), label_only = TRUE),
    Time = display_variable_name_static(time, variable_info, character(0), label_only = TRUE),
    `Independent variables` = paste(vapply(predictors, display_variable_name_static, character(1), table = variable_info, labels = character(0), label_only = TRUE), collapse = ", "),
    N = if (is.na(n)) "" else as.character(n),
    Message = reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_bind_guard_rows <- function(rows) {
  analysis_bind_rows(rows)
}

longitudinal_missing_pattern_summary <- function(raw_prepared, complete, keep, id, time, outcome, terms, cluster = character(0), offset = character(0)) {
  if (!is.data.frame(raw_prepared) || nrow(raw_prepared) == 0) {
    return(data.frame(Item = character(0), Value = character(0), stringsAsFactors = FALSE, check.names = FALSE))
  }
  complete <- as.logical(complete %||% stats::complete.cases(raw_prepared))
  keep <- as.logical(keep %||% complete)
  complete[is.na(complete)] <- FALSE
  keep[is.na(keep)] <- FALSE
  variables <- names(raw_prepared)
  missing_matrix <- is.na(raw_prepared)
  pattern <- apply(missing_matrix, 1, function(row) {
    missing_names <- variables[row]
    if (length(missing_names) == 0) "Complete" else paste(missing_names, collapse = ", ")
  })
  pattern_counts <- sort(table(pattern), decreasing = TRUE)
  most_common_pattern <- if (length(pattern_counts) > 0) {
    sprintf("%s (n=%s)", names(pattern_counts)[[1]], as.integer(pattern_counts[[1]]))
  } else {
    ""
  }
  id <- utils::head(intersect(as.character(id %||% character(0)), variables), 1)
  time <- utils::head(intersect(as.character(time %||% character(0)), variables), 1)
  outcome <- utils::head(intersect(as.character(outcome %||% character(0)), variables), 1)
  terms <- intersect(as.character(terms %||% character(0)), variables)
  cluster <- utils::head(intersect(as.character(cluster %||% character(0)), variables), 1)
  offset <- utils::head(intersect(as.character(offset %||% character(0)), variables), 1)
  subject_values <- if (length(id) == 1) raw_prepared[[id]] else NULL
  subject_observed <- if (!is.null(subject_values)) !is.na(subject_values) else rep(FALSE, nrow(raw_prepared))
  subject_count <- if (!is.null(subject_values)) length(unique(subject_values[subject_observed])) else NA_integer_
  incomplete_subject_count <- if (!is.null(subject_values) && any(subject_observed)) {
    by_subject <- tapply(!complete[subject_observed], subject_values[subject_observed], any)
    sum(by_subject, na.rm = TRUE)
  } else {
    NA_integer_
  }
  retained_subject_count <- if (!is.null(subject_values) && any(subject_observed & keep)) {
    length(unique(subject_values[subject_observed & keep]))
  } else {
    NA_integer_
  }
  predictor_missing <- if (length(terms) > 0) sum(rowSums(missing_matrix[, terms, drop = FALSE]) > 0) else 0L
  id_time_missing <- sum(rowSums(missing_matrix[, intersect(c(id, time), variables), drop = FALSE]) > 0)
  data.frame(
    Item = c(
      "Raw rows",
      "Complete model rows",
      "Rows retained by selected missing-data method",
      "Rows excluded by selected missing-data method",
      "Subjects / clusters in raw data",
      "Subjects / clusters retained",
      "Subjects / clusters with any incomplete selected row",
      "Rows with missing dependent variable",
      "Rows with any missing model term",
      "Rows with missing ID or time",
      "Rows with missing exposure / offset",
      "Rows with missing higher-level cluster ID",
      "Distinct missingness patterns",
      "Most common missingness pattern"
    ),
    Value = c(
      as.character(nrow(raw_prepared)),
      as.character(sum(complete)),
      as.character(sum(keep)),
      as.character(sum(!keep)),
      if (is.na(subject_count)) "" else as.character(subject_count),
      if (is.na(retained_subject_count)) "" else as.character(retained_subject_count),
      if (is.na(incomplete_subject_count)) "" else as.character(incomplete_subject_count),
      if (length(outcome) == 1) as.character(sum(is.na(raw_prepared[[outcome]]))) else "",
      as.character(predictor_missing),
      as.character(id_time_missing),
      if (length(offset) == 1) as.character(sum(is.na(raw_prepared[[offset]]))) else "0",
      if (length(cluster) == 1) as.character(sum(is.na(raw_prepared[[cluster]]))) else "0",
      as.character(length(pattern_counts)),
      most_common_pattern
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_missing_by_time_summary <- function(raw_prepared, time, outcome, variables) {
  if (!is.data.frame(raw_prepared) || nrow(raw_prepared) == 0) {
    return(data.frame(Time = character(0), Rows = integer(0), `Complete rows` = integer(0), `Missing dependent variable` = integer(0), `Any missing selected variable` = integer(0), `Any missing %` = numeric(0), stringsAsFactors = FALSE, check.names = FALSE))
  }
  time <- utils::head(intersect(as.character(time %||% character(0)), names(raw_prepared)), 1)
  if (length(time) != 1) {
    return(data.frame(Time = character(0), Rows = integer(0), `Complete rows` = integer(0), `Missing dependent variable` = integer(0), `Any missing selected variable` = integer(0), `Any missing %` = numeric(0), stringsAsFactors = FALSE, check.names = FALSE))
  }
  variables <- intersect(as.character(variables %||% names(raw_prepared)), names(raw_prepared))
  outcome <- utils::head(intersect(as.character(outcome %||% character(0)), names(raw_prepared)), 1)
  time_values <- raw_prepared[[time]]
  levels <- unique(stats::na.omit(time_values))
  levels <- levels[order(levels)]
  rows <- lapply(levels, function(level) {
    index <- which(time_values == level)
    missing_selected <- rowSums(is.na(raw_prepared[index, variables, drop = FALSE])) > 0
    data.frame(
      Time = as.character(level),
      Rows = length(index),
      `Complete rows` = sum(!missing_selected),
      `Missing dependent variable` = if (length(outcome) == 1) sum(is.na(raw_prepared[[outcome]][index])) else NA_integer_,
      `Any missing selected variable` = sum(missing_selected),
      `Any missing %` = if (length(index) > 0) 100 * mean(missing_selected) else NA_real_,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  analysis_bind_rows(rows)
}

longitudinal_prepare_data <- function(
  data,
  outcome,
  id,
  time,
  exposure = character(0),
  predictors,
  covariates,
  cluster = character(0),
  weight = character(0),
  variable_info = NULL,
  reference_values = character(0),
  missing_method = "row_complete"
) {
  missing_method <- longitudinal_resolve_missing_method(missing_method)
  variables <- unique(c(outcome, id, cluster, time, exposure, predictors, covariates, weight))
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  raw_n <- nrow(data)
  raw_subset <- data[, variables, drop = FALSE]
  missing_table <- data.frame(
    Variable = variables,
    Missing = vapply(raw_subset, function(values) sum(is.na(values)), integer(1)),
    MissingPercent = vapply(raw_subset, function(values) {
      if (length(values) == 0) return(NA_real_)
      100 * mean(is.na(values))
    }, numeric(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  prepared <- raw_subset
  prepared <- prepare_regression_model_data_static(
    prepared,
    variables,
    variable_info = variable_info,
    reference_values = reference_values
  )
  if (identical(longitudinal_measurement_for(outcome, variable_info), "binary") && outcome %in% names(prepared)) {
    prepared[[outcome]] <- longitudinal_binary_outcome_numeric(prepared[[outcome]])
  }
  raw_prepared <- prepared
  complete <- stats::complete.cases(prepared)
  excluded_subjects <- 0L
  if (identical(missing_method, "subject_complete") && id %in% names(prepared)) {
    incomplete_ids <- unique(prepared[[id]][!complete & !is.na(prepared[[id]])])
    excluded_subjects <- length(incomplete_ids)
    keep <- complete & !prepared[[id]] %in% incomplete_ids
  } else {
    keep <- complete
  }
  missing_pattern <- longitudinal_missing_pattern_summary(raw_prepared, complete, keep, id, time, outcome, unique(c(time, predictors, covariates)), cluster, exposure)
  missing_by_time <- longitudinal_missing_by_time_summary(raw_prepared, time, outcome, variables)
  prepared <- prepared[keep, , drop = FALSE]
  prepared[[id]] <- as.factor(prepared[[id]])
  if (length(cluster) == 1 && cluster %in% names(prepared)) {
    prepared[[cluster]] <- as.factor(prepared[[cluster]])
  }
  list(
    data = prepared,
    n = nrow(prepared),
    excluded = raw_n - nrow(prepared),
    raw_n = raw_n,
    missing_table = missing_table,
    missing_method = missing_method,
    missing_method_label = longitudinal_missing_method_label(missing_method),
    excluded_subjects = excluded_subjects,
    raw_prepared = raw_prepared,
    missing_pattern = missing_pattern,
    missing_by_time = missing_by_time,
    complete_indicator = complete,
    keep_indicator = keep
  )
}

longitudinal_terms <- function(time, predictors, covariates, include_time = TRUE) {
  unique(c(if (isTRUE(include_time)) time else character(0), predictors, covariates))
}

longitudinal_formula <- function(outcome, terms, offset = character(0)) {
  terms <- unique(as.character(terms %||% character(0)))
  offset <- utils::head(as.character(offset %||% character(0)), 1)
  rhs <- if (length(terms) == 0) {
    "1"
  } else {
    paste(vapply(terms, longitudinal_formula_variable, character(1)), collapse = " + ")
  }
  if (length(offset) == 1 && nzchar(offset)) {
    rhs <- paste(rhs, sprintf("offset(log(%s))", longitudinal_formula_variable(offset)), sep = " + ")
  }
  stats::as.formula(sprintf("%s ~ %s", longitudinal_formula_variable(outcome), rhs))
}

longitudinal_formula_without_offset <- function(outcome, terms) {
  terms <- unique(as.character(terms %||% character(0)))
  if (length(terms) == 0) {
    stats::as.formula(sprintf("%s ~ 1", outcome))
  } else {
    stats::reformulate(terms, response = outcome)
  }
}

longitudinal_formula_variable <- function(name) {
  name <- as.character(name %||% "")[[1]]
  if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", name)) {
    return(name)
  }
  paste0("`", gsub("`", "\\\\`", name), "`")
}

longitudinal_mixed_random_terms <- function(id, time, cluster = character(0), random_slope = FALSE) {
  id <- as.character(id %||% character(0))[[1]]
  time <- as.character(time %||% character(0))[[1]]
  cluster <- utils::head(as.character(cluster %||% character(0)), 1)
  subject_term <- if (isTRUE(random_slope)) {
    sprintf("(%s | %s)", longitudinal_formula_variable(time), longitudinal_formula_variable(id))
  } else {
    sprintf("(1 | %s)", longitudinal_formula_variable(id))
  }
  cluster <- setdiff(cluster, id)
  if (length(cluster) == 1 && nzchar(cluster)) {
    c(subject_term, sprintf("(1 | %s)", longitudinal_formula_variable(cluster)))
  } else {
    subject_term
  }
}

longitudinal_model_frame_preflight <- function(data, formula, outcome, id, time, predictors, variable_info = NULL) {
  n <- nrow(data)
  if (n < 3) {
    return(list(ok = FALSE, skipped = longitudinal_guard_row(outcome, id, time, predictors, "At least 3 complete cases are required.", n, variable_info)))
  }
  cluster_count <- length(unique(data[[id]]))
  if (cluster_count < 2) {
    return(list(ok = FALSE, skipped = longitudinal_guard_row(outcome, id, time, predictors, "At least two subjects or clusters are required.", n, variable_info)))
  }
  y <- data[[outcome]]
  if (length(unique(stats::na.omit(y))) < 2) {
    return(list(ok = FALSE, skipped = longitudinal_guard_row(outcome, id, time, predictors, "The outcome has fewer than two observed values after complete-case filtering.", n, variable_info)))
  }
  frame <- tryCatch(stats::model.frame(formula, data = data, na.action = stats::na.omit), error = function(e) e)
  if (inherits(frame, "error")) {
    return(list(ok = FALSE, skipped = longitudinal_guard_row(outcome, id, time, predictors, conditionMessage(frame), n, variable_info)))
  }
  mm <- tryCatch(stats::model.matrix(formula, data = frame), error = function(e) e)
  if (inherits(mm, "error")) {
    return(list(ok = FALSE, skipped = longitudinal_guard_row(outcome, id, time, predictors, conditionMessage(mm), n, variable_info)))
  }
  rank <- qr(mm)$rank
  warnings <- list()
  if (rank < ncol(mm)) {
    warnings[[length(warnings) + 1L]] <- longitudinal_guard_row(
      outcome,
      id,
      time,
      predictors,
      "Model matrix is rank deficient; one or more coefficients may be aliased.",
      n,
      variable_info,
      type = "Warning"
    )
  }
  list(ok = TRUE, n = n, clusters = cluster_count, time_points = length(unique(data[[time]])), warnings = longitudinal_bind_guard_rows(warnings))
}

longitudinal_coef_table_from_matrix <- function(coef_matrix, exponentiate = FALSE) {
  if (!is.matrix(coef_matrix) && !is.data.frame(coef_matrix)) {
    return(data.frame())
  }
  if (is.data.frame(coef_matrix) && !inherits(coef_matrix, "coeftest")) {
    term_names <- rownames(coef_matrix)
    coef_matrix <- as.data.frame(coef_matrix, check.names = FALSE)
  } else {
    coef_matrix <- unclass(coef_matrix)
    if (is.null(dim(coef_matrix))) {
      coef_matrix <- matrix(coef_matrix, nrow = 1)
    }
    term_names <- rownames(coef_matrix)
    coef_names <- colnames(coef_matrix)
    coef_matrix <- as.data.frame(coef_matrix, check.names = FALSE)
    names(coef_matrix) <- coef_names
  }
  estimate_candidates <- intersect(c("Estimate", "Value"), names(coef_matrix))
  se_candidates <- intersect(c("Std. Error", "Std.err", "Robust S.E.", "Std. Error"), names(coef_matrix))
  if (length(estimate_candidates) == 0 || length(se_candidates) == 0) {
    return(data.frame())
  }
  estimate_col <- estimate_candidates[[1]]
  se_col <- se_candidates[[1]]
  statistic_col <- intersect(c("t value", "z value", "Wald", "Statistic"), names(coef_matrix))
  p_col <- grep("^Pr\\(|^p$|P\\(>|p-value", names(coef_matrix), value = TRUE)
  estimate <- suppressWarnings(as.numeric(coef_matrix[[estimate_col]]))
  se <- suppressWarnings(as.numeric(coef_matrix[[se_col]]))
  statistic <- if (length(statistic_col) > 0) suppressWarnings(as.numeric(coef_matrix[[statistic_col[[1]]]])) else estimate / se
  p <- if (length(p_col) > 0) suppressWarnings(as.numeric(coef_matrix[[p_col[[1]]]])) else 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  table <- data.frame(
    Term = term_names,
    B = estimate,
    SE = se,
    Statistic = statistic,
    p = p,
    LLCI = estimate - 1.96 * se,
    ULCI = estimate + 1.96 * se,
    row.names = NULL,
    check.names = FALSE
  )
  if (isTRUE(exponentiate)) {
    table$`exp(B)` <- exp(table$B)
    table$`exp(LLCI)` <- exp(table$LLCI)
    table$`exp(ULCI)` <- exp(table$ULCI)
  }
  table
}

longitudinal_lmm_coef_table <- function(model) {
  longitudinal_coef_table_from_matrix(summary(model)$coefficients, exponentiate = FALSE)
}

longitudinal_gee_coef_table <- function(model, exponentiate = FALSE) {
  coef_matrix <- summary(model)$coefficients
  longitudinal_coef_table_from_matrix(coef_matrix, exponentiate = exponentiate)
}

longitudinal_cluster_robust_coef_table <- function(model, cluster, exponentiate = FALSE) {
  test <- tryCatch(
    lmtest::coeftest(model, vcov. = sandwich::vcovCL(model, cluster = cluster, type = "HC1")),
    error = function(e) NULL
  )
  if (is.null(test)) {
    return(longitudinal_coef_table_from_matrix(summary(model)$coefficients, exponentiate = exponentiate))
  }
  longitudinal_coef_table_from_matrix(test, exponentiate = exponentiate)
}

longitudinal_glmm_coef_table <- function(model, exponentiate = FALSE) {
  longitudinal_coef_table_from_matrix(summary(model)$coefficients, exponentiate = exponentiate)
}

longitudinal_panel_coef_table <- function(model) {
  test <- lmtest::coeftest(model, vcov. = plm::vcovHC(model, type = "HC1", cluster = "group"))
  longitudinal_coef_table_from_matrix(test, exponentiate = FALSE)
}

longitudinal_panel_driscoll_kraay_summary <- function(model) {
  hc1 <- lmtest::coeftest(model, vcov. = plm::vcovHC(model, type = "HC1", cluster = "group"))
  dk <- lmtest::coeftest(model, vcov. = plm::vcovSCC(model, type = "HC1"))
  hc1_se <- suppressWarnings(as.numeric(hc1[, 2]))
  dk_se <- suppressWarnings(as.numeric(dk[, 2]))
  names(hc1_se) <- rownames(hc1)
  names(dk_se) <- rownames(dk)
  shared <- intersect(names(hc1_se), names(dk_se))
  ratios <- dk_se[shared] / hc1_se[shared]
  ratios <- ratios[is.finite(ratios) & ratios > 0]
  if (length(ratios) == 0) {
    stop("No comparable coefficient standard errors were returned.")
  }
  sprintf(
    "max DK/HC1 SE ratio=%s; median=%s; terms=%s",
    longitudinal_format_number_static(max(ratios)),
    longitudinal_format_number_static(stats::median(ratios)),
    length(ratios)
  )
}

longitudinal_fit_model <- function(data, outcome, id, time, terms, model_type, family, corstr, random_slope = FALSE, exponentiate = FALSE, weights = NULL, cluster = character(0), offset = character(0)) {
  formula <- longitudinal_formula(outcome, terms, offset = offset)
  if (!is.null(weights)) {
    weights <- suppressWarnings(as.numeric(weights))
    if (length(weights) != nrow(data) || any(!is.finite(weights)) || any(weights <= 0)) {
      stop("Model weights must be positive finite values with one value per analyzed row.")
    }
    data$.statedu_weights <- weights
  }
  if (identical(model_type, "gee")) {
    if (id %in% names(data) && time %in% names(data)) {
      data <- data[order(data[[id]], data[[time]], na.last = TRUE), , drop = FALSE]
    }
    if (identical(family, "negative_binomial")) {
      data$.statedu_gee_id <- data[[id]]
      model <- if (is.null(weights)) {
        MASS::glm.nb(formula, data = data, link = "log")
      } else {
        MASS::glm.nb(formula, data = data, weights = .statedu_weights, link = "log")
      }
      return(list(
        model = model,
        formula = formula,
        coef_table = longitudinal_cluster_robust_coef_table(model, data$.statedu_gee_id, exponentiate),
        aic = stats::AIC(model),
        bic = stats::BIC(model),
        fit_note = "Negative binomial count outcomes are fitted as a marginal negative binomial GLM with subject-cluster robust standard errors because geepack does not support a negative binomial working variance. Do not report this row as a native negative-binomial GEE."
      ))
    }
    data$.statedu_gee_id <- data[[id]]
    data$.statedu_gee_waves <- data[[time]]
    model <- if (is.null(weights)) {
      geepack::geeglm(
        formula,
        id = .statedu_gee_id,
        waves = .statedu_gee_waves,
        data = data,
        family = longitudinal_family_object(family, formula, data, weights),
        corstr = corstr
      )
    } else {
      geepack::geeglm(
        formula,
        id = .statedu_gee_id,
        waves = .statedu_gee_waves,
        data = data,
        weights = .statedu_weights,
        family = longitudinal_family_object(family, formula, data, weights),
        corstr = corstr
      )
    }
    return(list(model = model, formula = formula, coef_table = longitudinal_gee_coef_table(model, exponentiate), aic = NA_real_, bic = NA_real_, fit_note = NULL))
  }
  if (identical(model_type, "lmm")) {
    random <- longitudinal_mixed_random_terms(id, time, cluster, random_slope)
    mixed_formula <- stats::as.formula(paste(deparse(formula), "+", paste(random, collapse = " + ")))
    model <- if (is.null(weights)) {
      lmerTest::lmer(mixed_formula, data = data, REML = FALSE)
    } else {
      lmerTest::lmer(mixed_formula, data = data, weights = .statedu_weights, REML = FALSE)
    }
    return(list(model = model, formula = mixed_formula, coef_table = longitudinal_lmm_coef_table(model), aic = stats::AIC(model), bic = stats::BIC(model), fit_note = NULL))
  }
  if (identical(model_type, "glmm")) {
    random <- longitudinal_mixed_random_terms(id, time, cluster, random_slope)
    mixed_formula <- stats::as.formula(paste(deparse(formula), "+", paste(random, collapse = " + ")))
    if (identical(family, "negative_binomial")) {
      model <- if (is.null(weights)) {
        lme4::glmer.nb(
          mixed_formula,
          data = data,
          control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
        )
      } else {
        lme4::glmer.nb(
          mixed_formula,
          data = data,
          weights = .statedu_weights,
          control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
        )
      }
    } else {
      model <- if (is.null(weights)) {
        lme4::glmer(
          mixed_formula,
          data = data,
          family = longitudinal_family_object(family),
          control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
        )
      } else {
        lme4::glmer(
          mixed_formula,
          data = data,
          weights = .statedu_weights,
          family = longitudinal_family_object(family),
          control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
        )
      }
    }
    return(list(model = model, formula = mixed_formula, coef_table = longitudinal_glmm_coef_table(model, exponentiate), aic = stats::AIC(model), bic = stats::BIC(model), fit_note = NULL))
  }
  if (model_type %in% c("panel_fe", "panel_re")) {
    panel_model <- if (identical(model_type, "panel_fe")) "within" else "random"
    model <- if (is.null(weights)) {
      plm::plm(formula, data = data, index = c(id, time), model = panel_model, effect = "individual")
    } else {
      plm::plm(formula, data = data, weights = .statedu_weights, index = c(id, time), model = panel_model, effect = "individual")
    }
    return(list(model = model, formula = formula, coef_table = longitudinal_panel_coef_table(model), aic = NA_real_, bic = NA_real_, fit_note = NULL))
  }
  stop("Unknown longitudinal model type.")
}

longitudinal_model_notes <- function(model_type, family, corstr, random_slope, exponentiate, id = NULL, time = NULL, cluster = NULL, variable_info = NULL) {
  notes <- character(0)
  id_label <- display_variable_name_static(id, variable_info, character(0), label_only = TRUE)
  time_label <- display_variable_name_static(time, variable_info, character(0), label_only = TRUE)
  cluster_label <- display_variable_name_static(cluster, variable_info, character(0), label_only = TRUE)
  if (identical(model_type, "gee")) {
    notes <- c(notes, "GEE estimates population-averaged effects with robust sandwich standard errors.")
    notes <- c(notes, sprintf("Working correlation structure: %s.", corstr))
  }
  if (identical(model_type, "lmm")) {
    notes <- c(notes, "LMM estimates subject-specific fixed effects while allowing cluster-level random effects.")
    notes <- c(notes, sprintf("Random intercept grouping variable: %s.", id_label))
  }
  if (identical(model_type, "glmm")) {
    notes <- c(notes, "GLMM estimates subject-specific effects on the model link scale.")
    notes <- c(notes, sprintf("Random intercept grouping variable: %s.", id_label))
  }
  if (model_type %in% c("panel_fe", "panel_re")) {
    notes <- c(notes, "Panel regression uses the selected ID and time variables as panel indexes.")
    notes <- c(notes, "Panel coefficient standard errors use group-clustered HC1 robust covariance.")
  }
  if (isTRUE(random_slope) && model_type %in% c("lmm", "glmm")) {
    notes <- c(notes, sprintf("A random slope for the selected time variable (%s) was included.", time_label))
  }
  if (model_type %in% c("lmm", "glmm") && length(cluster) == 1 && nzchar(cluster) && !identical(cluster, id)) {
    notes <- c(notes, sprintf("Additional cluster-level random intercept grouping variable: %s.", cluster_label))
  }
  if (isTRUE(exponentiate) && family %in% longitudinal_log_ratio_families()) {
    notes <- c(notes, "Exponentiated coefficients are reported as OR for binomial models, rate ratios for count models, and mean ratios for Gamma log-link models.")
  }
  notes
}

longitudinal_assumption_row <- function(check, result, statistic = NA_real_, p = NA_real_, interpretation = "", recommendation = "", issue = FALSE) {
  data.frame(
    Check = check,
    Result = result,
    Statistic = suppressWarnings(as.numeric(statistic)),
    p = suppressWarnings(as.numeric(p)),
    Interpretation = interpretation,
    Recommendation = recommendation,
    Issue = isTRUE(issue),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_residual_values <- function(model, model_type) {
  values <- tryCatch(
    {
      if (model_type %in% c("gee", "glmm")) {
        stats::residuals(model, type = "pearson")
      } else {
        stats::residuals(model)
      }
    },
    error = function(e) numeric(0)
  )
  values <- suppressWarnings(as.numeric(values))
  values[is.finite(values)]
}

longitudinal_check_normality <- function(residuals, family) {
  if (!identical(family, "gaussian")) {
    return(longitudinal_assumption_row(
      "Residual normality",
      "Not primary",
      interpretation = "Normal residuals are not expected for GLM-family link-scale models.",
      recommendation = "Assess whether the selected outcome family and link match the data instead of relying on normal residuals."
    ))
  }
  if (length(residuals) < 3) {
    return(longitudinal_assumption_row(
      "Residual normality",
      "Not checked",
      interpretation = "At least 3 residuals are required for Shapiro-Wilk screening.",
      recommendation = "Use graphical residual review when more observations are available."
    ))
  }
  tested <- residuals
  if (length(tested) > 5000) {
    tested <- tested[seq_len(5000)]
  }
  test <- tryCatch(stats::shapiro.test(tested), error = function(e) NULL)
  if (is.null(test)) {
    return(longitudinal_assumption_row(
      "Residual normality",
      "Not checked",
      interpretation = "The normality screening test could not be computed.",
      recommendation = "Review Q-Q plots or use bootstrap / robust inference if residual normality is doubtful."
    ))
  }
  issue <- is.finite(test$p.value) && test$p.value < 0.05
  longitudinal_assumption_row(
    "Residual normality",
    if (issue) "Potential violation" else "No evidence of violation",
    statistic = unname(test$statistic),
    p = test$p.value,
    interpretation = if (issue) "Residuals deviate from normality by Shapiro-Wilk screening." else "Shapiro-Wilk screening did not detect a normality problem.",
    recommendation = if (issue) "Use robust or bootstrap confidence intervals; for clearly non-Gaussian outcomes, switch to GEE / GLMM with the appropriate family." else "Continue with the selected Gaussian model; still review residual plots for shape and outliers.",
    issue = issue
  )
}

longitudinal_check_response_family <- function(family, model_type) {
  label <- switch(
    as.character(family %||% "gaussian")[[1]],
    gaussian = "Gaussian / identity",
    binomial = "Binomial / logit",
    count = "Count: Poisson or negative binomial / log",
    poisson = "Poisson / log",
    negative_binomial = "Negative binomial / log",
    gamma = "Gamma / log",
    as.character(family %||% "auto")[[1]]
  )
  longitudinal_assumption_row(
    "Outcome family / link",
    "Reviewed",
    interpretation = sprintf("The fitted %s uses %s.", longitudinal_model_label(model_type, family), label),
    recommendation = "Confirm that the selected family matches the outcome scale; for count outcomes, review the Poisson dispersion-threshold screening and treat AIC/BIC as supplementary diagnostics."
  )
}

longitudinal_check_working_correlation <- function(corstr) {
  longitudinal_assumption_row(
    "GEE working correlation",
    "Reviewed",
    interpretation = sprintf("The selected working correlation is %s.", corstr),
    recommendation = "Compare exchangeable, AR(1), and independence structures when the repeated-measures pattern supports them."
  )
}

longitudinal_check_random_effect_group <- function(id, time = NULL, random_slope = FALSE, cluster = NULL, variable_info = NULL) {
  id_label <- display_variable_name_static(id, variable_info, character(0), label_only = TRUE)
  time_label <- display_variable_name_static(time, variable_info, character(0), label_only = TRUE)
  cluster <- utils::head(as.character(cluster %||% character(0)), 1)
  cluster_label <- display_variable_name_static(cluster, variable_info, character(0), label_only = TRUE)
  cluster_text <- if (length(cluster) == 1 && nzchar(cluster) && !identical(cluster, id)) {
    sprintf(" An additional cluster-level random intercept is grouped by %s.", cluster_label)
  } else {
    ""
  }
  longitudinal_assumption_row(
    "Random-effects structure",
    "Reviewed",
    interpretation = if (isTRUE(random_slope)) {
      sprintf("Subject-level random intercepts are grouped by %s, with a random slope for %s.%s", id_label, time_label, cluster_text)
    } else {
      sprintf("Subject-level random intercepts are grouped by %s.%s", id_label, cluster_text)
    },
    recommendation = if (isTRUE(random_slope)) {
      "Check convergence and simplify the random-effects structure if the model is singular or unstable."
    } else {
      "Add a random slope only when subject-specific time trends are substantively expected and supported by the data."
    }
  )
}

longitudinal_check_mixed_convergence <- function(model) {
  singular <- tryCatch(lme4::isSingular(model, tol = 1e-4), error = function(e) NA)
  messages <- tryCatch(model@optinfo$conv$lme4$messages, error = function(e) NULL)
  issue <- isTRUE(singular) || length(messages) > 0
  detail <- character(0)
  if (isTRUE(singular)) detail <- c(detail, "singular random-effects fit")
  if (length(messages) > 0) detail <- c(detail, paste(messages, collapse = "; "))
  longitudinal_assumption_row(
    "Convergence / singular fit",
    if (issue) "Potential violation" else "No evidence of violation",
    interpretation = if (issue) {
      sprintf("Mixed model fit warning: %s.", paste(detail, collapse = "; "))
    } else {
      "No convergence message or singular-fit flag was detected."
    },
    recommendation = if (issue) {
      "Simplify the random-effects structure, review sparse clusters, or refit with an alternative optimizer before final interpretation."
    } else {
      "Continue with the selected mixed model; still review cluster sizes and random-effect estimates."
    },
    issue = issue
  )
}

longitudinal_check_random_effect_normality <- function(model) {
  values <- tryCatch({
    effects <- lme4::ranef(model)
    unlist(lapply(effects, function(item) {
      if (!is.data.frame(item) || nrow(item) == 0) return(numeric(0))
      suppressWarnings(as.numeric(item[[1]]))
    }), use.names = FALSE)
  }, error = function(e) numeric(0))
  values <- values[is.finite(values)]
  if (length(values) < 3) {
    return(longitudinal_assumption_row(
      "Random-effect normality",
      "Not checked",
      interpretation = "At least 3 estimated random effects are required for screening.",
      recommendation = "Review the random-effects distribution graphically when enough clusters are available."
    ))
  }
  tested <- if (length(values) > 5000) values[seq_len(5000)] else values
  test <- tryCatch(stats::shapiro.test(tested), error = function(e) NULL)
  if (is.null(test)) {
    return(longitudinal_assumption_row(
      "Random-effect normality",
      "Not checked",
      interpretation = "Random-effect normality screening could not be computed.",
      recommendation = "Review random-effect quantile plots if random-effect distribution is important."
    ))
  }
  issue <- is.finite(test$p.value) && test$p.value < 0.05
  longitudinal_assumption_row(
    "Random-effect normality",
    if (issue) "Potential violation" else "No evidence of violation",
    statistic = unname(test$statistic),
    p = test$p.value,
    interpretation = if (issue) "Estimated random effects deviate from normality by Shapiro-Wilk screening." else "Random-effect normality screening was not significant.",
    recommendation = if (issue) "Use graphical review and sensitivity analysis; consider GEE if population-averaged inference is the primary target." else "Random-effect normality looks acceptable by this screening test.",
    issue = issue
  )
}

longitudinal_check_panel_exogeneity <- function(model_type) {
  longitudinal_assumption_row(
    "Strict exogeneity / omitted confounding",
    "Design review required",
    interpretation = if (identical(model_type, "panel_fe")) {
      "Fixed effects remove time-invariant unit confounding, but time-varying omitted confounding and reverse causation can still bias estimates."
    } else {
      "Random effects additionally require unit-specific unobserved factors to be independent of included predictors."
    },
    recommendation = if (identical(model_type, "panel_fe")) {
      "Add measured time-varying confounders and time fixed effects when plausible; consider lagged predictors, IV, or dynamic panel methods for reverse causation."
    } else {
      "If unit effects may correlate with predictors, prefer fixed effects; consider IV or dynamic panel methods when reverse causation is plausible."
    }
  )
}

longitudinal_check_overdispersion <- function(model, residuals) {
  df <- tryCatch(stats::df.residual(model), error = function(e) NA_real_)
  if (!is.finite(df) || df <= 0 || length(residuals) == 0) {
    return(longitudinal_assumption_row(
      "Overdispersion",
      "Not checked",
      interpretation = "Residual degrees of freedom were not available for overdispersion screening.",
      recommendation = "For count or binary clustered outcomes, review dispersion and sparse cells before final interpretation."
    ))
  }
  ratio <- sum(residuals^2, na.rm = TRUE) / df
  issue <- is.finite(ratio) && ratio > 1.5
  longitudinal_assumption_row(
    "Overdispersion",
    if (issue) "Potential violation" else "No evidence of violation",
    statistic = ratio,
    interpretation = if (issue) "Pearson residual dispersion is elevated." else "Pearson residual dispersion is not elevated by this screening rule.",
    recommendation = if (issue) "Consider robust sandwich inference, alternative family, or subject-level random effects depending on the selected model." else "No overdispersion adjustment is suggested by this screening rule.",
    issue = issue
  )
}

longitudinal_check_heteroskedasticity <- function(data, formula, family) {
  if (!identical(family, "gaussian")) {
    return(longitudinal_assumption_row(
      "Heteroskedasticity",
      "Not primary",
      interpretation = "Breusch-Pagan screening is mainly intended for Gaussian mean models.",
      recommendation = "For GEE / GLMM, rely on robust sandwich inference and check model family fit."
    ))
  }
  if (!requireNamespace("lmtest", quietly = TRUE)) {
    return(longitudinal_assumption_row(
      "Heteroskedasticity",
      "Not checked",
      interpretation = "The lmtest package is not available.",
      recommendation = "Install lmtest or use residual-vs-fitted plots."
    ))
  }
  test <- tryCatch(lmtest::bptest(formula, data = data), error = function(e) NULL)
  if (is.null(test)) {
    return(longitudinal_assumption_row(
      "Heteroskedasticity",
      "Not checked",
      interpretation = "Breusch-Pagan screening could not be computed for this model frame.",
      recommendation = "Use cluster-robust standard errors if heteroskedasticity is plausible."
    ))
  }
  issue <- is.finite(test$p.value) && test$p.value < 0.05
  longitudinal_assumption_row(
    "Heteroskedasticity",
    if (issue) "Potential violation" else "No evidence of violation",
    statistic = unname(test$statistic),
    p = test$p.value,
    interpretation = if (issue) "Residual variance appears non-constant." else "Breusch-Pagan screening did not detect non-constant variance.",
    recommendation = if (issue) "Use heteroskedasticity-robust or subject-clustered standard errors." else "Conventional variance assumptions look acceptable by this screening test.",
    issue = issue
  )
}

longitudinal_lagged_residual_pairs <- function(data, residuals, id, time) {
  if (length(residuals) != nrow(data)) {
    return(data.frame())
  }
  frame <- data.frame(
    id = data[[id]],
    time = data[[time]],
    residual = residuals,
    stringsAsFactors = FALSE
  )
  frame <- frame[is.finite(frame$residual), , drop = FALSE]
  rows <- lapply(split(frame, frame$id), function(unit) {
    if (nrow(unit) < 2) return(NULL)
    unit <- unit[order(unit$time), , drop = FALSE]
    data.frame(
      residual = unit$residual[-1],
      lag_residual = unit$residual[-nrow(unit)],
      stringsAsFactors = FALSE
    )
  })
  analysis_bind_rows(rows)
}

longitudinal_check_serial_correlation <- function(model, model_type, data, residuals, id, time) {
  if (model_type %in% c("panel_fe", "panel_re") && requireNamespace("plm", quietly = TRUE)) {
    test <- tryCatch(plm::pbgtest(model), error = function(e) NULL)
    if (!is.null(test)) {
      issue <- is.finite(test$p.value) && test$p.value < 0.05
      return(longitudinal_assumption_row(
        "Within-subject serial correlation",
        if (issue) "Potential violation" else "No evidence of violation",
        statistic = unname(test$statistic),
        p = test$p.value,
        interpretation = if (issue) "Panel residuals show evidence of serial correlation." else "Panel serial correlation screening was not significant.",
        recommendation = if (issue) "Use subject-clustered or Driscoll-Kraay standard errors; consider adding time fixed effects or a dynamic structure." else "No additional serial-correlation adjustment is suggested by this test.",
        issue = issue
      ))
    }
  }
  pairs <- longitudinal_lagged_residual_pairs(data, residuals, id, time)
  if (!is.data.frame(pairs) || nrow(pairs) < 4 || stats::sd(pairs$residual) == 0 || stats::sd(pairs$lag_residual) == 0) {
    return(longitudinal_assumption_row(
      "Within-subject serial correlation",
      "Not checked",
      interpretation = "There were not enough within-subject residual pairs for screening.",
      recommendation = "If repeated measures are dense, consider GEE correlation structures or cluster-robust inference."
    ))
  }
  test <- tryCatch(stats::cor.test(pairs$residual, pairs$lag_residual), error = function(e) NULL)
  if (is.null(test)) {
    return(longitudinal_assumption_row(
      "Within-subject serial correlation",
      "Not checked",
      interpretation = "Lagged residual correlation screening could not be computed.",
      recommendation = "Use subject-clustered inference when serial correlation is plausible."
    ))
  }
  issue <- is.finite(test$p.value) && test$p.value < 0.05
  longitudinal_assumption_row(
    "Within-subject serial correlation",
    if (issue) "Potential violation" else "No evidence of violation",
    statistic = unname(test$estimate),
    p = test$p.value,
    interpretation = if (issue) "Lag-1 residual correlation within subjects was detected." else "Lag-1 residual correlation screening was not significant.",
    recommendation = if (issue) "Use subject-clustered standard errors; for GEE consider AR(1) or exchangeable working correlation." else "No additional serial-correlation adjustment is suggested by this screening test.",
    issue = issue
  )
}

longitudinal_check_cross_section_dependence <- function(model, model_type) {
  if (!model_type %in% c("panel_fe", "panel_re")) {
    return(longitudinal_assumption_row(
      "Cross-sectional dependence",
      "Not checked",
      interpretation = "Cross-sectional dependence screening is available for panel FE / RE models.",
      recommendation = "If common shocks are likely, include time fixed effects or use panel-robust inference."
    ))
  }
  if (!requireNamespace("plm", quietly = TRUE)) {
    return(longitudinal_assumption_row(
      "Cross-sectional dependence",
      "Not checked",
      interpretation = "The plm package is not available.",
      recommendation = "Use time fixed effects when common period shocks are plausible."
    ))
  }
  test <- tryCatch(plm::pcdtest(model, test = "cd"), error = function(e) NULL)
  if (is.null(test)) {
    return(longitudinal_assumption_row(
      "Cross-sectional dependence",
      "Not checked",
      interpretation = "Pesaran CD screening could not be computed for this panel structure.",
      recommendation = "Use time fixed effects or Driscoll-Kraay standard errors if common shocks are plausible."
    ))
  }
  issue <- is.finite(test$p.value) && test$p.value < 0.05
  longitudinal_assumption_row(
    "Cross-sectional dependence",
    if (issue) "Potential violation" else "No evidence of violation",
    statistic = unname(test$statistic),
    p = test$p.value,
    interpretation = if (issue) "Residuals may be correlated across subjects or clusters at the same time point." else "Pesaran CD screening did not detect cross-sectional dependence.",
    recommendation = if (issue) "Add time fixed effects and consider Driscoll-Kraay standard errors for panel regression." else "No cross-sectional dependence adjustment is suggested by this test.",
    issue = issue
  )
}

longitudinal_check_hausman <- function(data, formula, id, time, model_type) {
  if (!model_type %in% c("panel_fe", "panel_re")) {
    return(longitudinal_assumption_row(
      "FE vs RE assumption",
      "Not applicable",
      interpretation = "Hausman screening applies to panel fixed-effects versus random-effects selection.",
      recommendation = "Use research design and estimand to choose among GEE, LMM / GLMM, and panel models."
    ))
  }
  if (!requireNamespace("plm", quietly = TRUE)) {
    return(longitudinal_assumption_row(
      "FE vs RE assumption",
      "Not checked",
      interpretation = "The plm package is not available.",
      recommendation = "Prefer fixed effects when unit-specific unobserved factors may correlate with predictors."
    ))
  }
  test <- tryCatch({
    fixed <- plm::plm(formula, data = data, index = c(id, time), model = "within", effect = "individual")
    random <- plm::plm(formula, data = data, index = c(id, time), model = "random", effect = "individual")
    plm::phtest(fixed, random)
  }, error = function(e) NULL)
  if (is.null(test)) {
    return(longitudinal_assumption_row(
      "FE vs RE assumption",
      "Not checked",
      interpretation = "Hausman screening could not be computed.",
      recommendation = "Prefer fixed effects when the random-effects independence assumption is doubtful."
    ))
  }
  issue <- is.finite(test$p.value) && test$p.value < 0.05
  longitudinal_assumption_row(
    "FE vs RE assumption",
    if (issue) "RE assumption doubtful" else "RE not rejected",
    statistic = unname(test$statistic),
    p = test$p.value,
    interpretation = if (issue) "The random-effects independence assumption is not supported by Hausman screening." else "Hausman screening did not reject the random-effects independence assumption.",
    recommendation = if (issue) "Prefer panel fixed effects over random effects for coefficient interpretation." else "Panel random effects can be considered if it matches the study question.",
    issue = issue && identical(model_type, "panel_re")
  )
}

longitudinal_check_enabled <- function(check_options, key) {
  if (is.null(check_options)) {
    return(TRUE)
  }
  if (!is.list(check_options)) {
    check_options <- as.list(check_options)
  }
  if (is.null(names(check_options)) || !key %in% names(check_options)) {
    return(TRUE)
  }
  isTRUE(check_options[[key]])
}

longitudinal_assumption_checks <- function(
  data,
  model,
  model_type,
  family,
  formula,
  id,
  time,
  corstr,
  cluster = character(0),
  random_slope = FALSE,
  variable_info = NULL,
  check_options = NULL
) {
  residuals <- longitudinal_residual_values(model, model_type)
  checks <- switch(
    as.character(model_type %||% "gee")[[1]],
    gee = list(
      if (longitudinal_check_enabled(check_options, "family")) longitudinal_check_response_family(family, model_type) else NULL,
      if (longitudinal_check_enabled(check_options, "working_correlation")) longitudinal_check_working_correlation(corstr) else NULL,
      if (longitudinal_check_enabled(check_options, "serial_correlation")) longitudinal_check_serial_correlation(model, model_type, data, residuals, id, time) else NULL,
      if (longitudinal_check_enabled(check_options, "overdispersion") && family %in% c("binomial", "poisson", "negative_binomial")) longitudinal_check_overdispersion(model, residuals) else NULL
    ),
    lmm = list(
      if (longitudinal_check_enabled(check_options, "mixed_convergence")) longitudinal_check_mixed_convergence(model) else NULL,
      if (longitudinal_check_enabled(check_options, "random_effects")) longitudinal_check_random_effect_group(id, time, random_slope, cluster, variable_info) else NULL,
      if (longitudinal_check_enabled(check_options, "random_effect_normality")) longitudinal_check_random_effect_normality(model) else NULL,
      if (longitudinal_check_enabled(check_options, "residual_normality")) longitudinal_check_normality(residuals, family) else NULL,
      if (longitudinal_check_enabled(check_options, "heteroskedasticity")) longitudinal_check_heteroskedasticity(data, formula, family) else NULL,
      if (longitudinal_check_enabled(check_options, "serial_correlation")) longitudinal_check_serial_correlation(model, model_type, data, residuals, id, time) else NULL
    ),
    glmm = list(
      if (longitudinal_check_enabled(check_options, "family")) longitudinal_check_response_family(family, model_type) else NULL,
      if (longitudinal_check_enabled(check_options, "mixed_convergence")) longitudinal_check_mixed_convergence(model) else NULL,
      if (longitudinal_check_enabled(check_options, "random_effects")) longitudinal_check_random_effect_group(id, time, random_slope, cluster, variable_info) else NULL,
      if (longitudinal_check_enabled(check_options, "serial_correlation")) longitudinal_check_serial_correlation(model, model_type, data, residuals, id, time) else NULL,
      if (longitudinal_check_enabled(check_options, "overdispersion") && family %in% c("binomial", "poisson", "negative_binomial")) longitudinal_check_overdispersion(model, residuals) else NULL
    ),
    panel_fe = list(
      if (longitudinal_check_enabled(check_options, "exogeneity")) longitudinal_check_panel_exogeneity(model_type) else NULL,
      if (longitudinal_check_enabled(check_options, "heteroskedasticity")) longitudinal_check_heteroskedasticity(data, formula, family) else NULL,
      if (longitudinal_check_enabled(check_options, "serial_correlation")) longitudinal_check_serial_correlation(model, model_type, data, residuals, id, time) else NULL,
      if (longitudinal_check_enabled(check_options, "cross_section")) longitudinal_check_cross_section_dependence(model, model_type) else NULL,
      if (longitudinal_check_enabled(check_options, "hausman")) longitudinal_check_hausman(data, formula, id, time, model_type) else NULL
    ),
    panel_re = list(
      if (longitudinal_check_enabled(check_options, "exogeneity")) longitudinal_check_panel_exogeneity(model_type) else NULL,
      if (longitudinal_check_enabled(check_options, "hausman")) longitudinal_check_hausman(data, formula, id, time, model_type) else NULL,
      if (longitudinal_check_enabled(check_options, "heteroskedasticity")) longitudinal_check_heteroskedasticity(data, formula, family) else NULL,
      if (longitudinal_check_enabled(check_options, "serial_correlation")) longitudinal_check_serial_correlation(model, model_type, data, residuals, id, time) else NULL,
      if (longitudinal_check_enabled(check_options, "cross_section")) longitudinal_check_cross_section_dependence(model, model_type) else NULL
    ),
    list(
      if (longitudinal_check_enabled(check_options, "serial_correlation")) longitudinal_check_serial_correlation(model, model_type, data, residuals, id, time) else NULL
    )
  )
  checks <- Filter(Negate(is.null), checks)
  checks <- analysis_bind_rows(checks)
  if (!is.data.frame(checks) || nrow(checks) == 0) {
    return(list(
      checks = data.frame(),
      recommendations = "No individual assumption checks were selected."
    ))
  }
  recommendations <- unique(checks$Recommendation[(checks$Issue %in% TRUE) | checks$Result %in% c("Potential violation", "RE assumption doubtful", "Design review required")])
  recommendations <- recommendations[nzchar(recommendations)]
  if (identical(model_type, "gee")) {
    recommendations <- c(recommendations, sprintf("For GEE, compare working correlation structures when clinically plausible; current structure is %s.", corstr))
  }
  if (length(recommendations) == 0) {
    recommendations <- "No major assumption issue was detected by the selected screening checks. Continue with the selected model and report the repeated-measures structure."
  }
  list(checks = checks, recommendations = unique(recommendations))
}

longitudinal_data_structure_summary <- function(
  prepared_data,
  raw_n,
  complete_n,
  excluded,
  id,
  time,
  cluster = character(0),
  missing_table = NULL,
  missing_method_label = "row-wise complete observations",
  excluded_subjects = 0L
) {
  cluster_sizes <- if (id %in% names(prepared_data)) as.integer(table(prepared_data[[id]])) else integer(0)
  higher_cluster_sizes <- if (length(cluster) == 1 && cluster %in% names(prepared_data)) as.integer(table(prepared_data[[cluster]])) else integer(0)
  observed_times <- if (time %in% names(prepared_data)) length(unique(prepared_data[[time]])) else NA_integer_
  balanced <- length(cluster_sizes) > 0 && length(unique(cluster_sizes)) == 1L
  rows <- data.frame(
    Item = c(
      "Raw observations",
      "Analyzed observations",
      "Missing-data handling",
      "Excluded for missing analysis variables",
      "Subjects / clusters excluded for missingness",
      "Subjects",
      "Higher-level clusters",
      "Time points observed",
      "Balanced panel",
      "Observations per cluster: min",
      "Observations per cluster: median",
      "Observations per cluster: max"
    ),
    Value = c(
      as.character(raw_n),
      as.character(complete_n),
      as.character(missing_method_label %||% ""),
      sprintf("%s (%.1f%%)", excluded, if (raw_n > 0) 100 * excluded / raw_n else NA_real_),
      as.character(excluded_subjects %||% 0L),
      as.character(length(cluster_sizes)),
      if (length(higher_cluster_sizes) > 0) as.character(length(higher_cluster_sizes)) else "",
      as.character(observed_times),
      if (balanced) "Yes" else "No",
      if (length(cluster_sizes) > 0) as.character(min(cluster_sizes)) else "",
      if (length(cluster_sizes) > 0) longitudinal_format_number_static(stats::median(cluster_sizes)) else "",
      if (length(cluster_sizes) > 0) as.character(max(cluster_sizes)) else ""
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rows
}

longitudinal_format_number_static <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0 || is.na(value)) return("")
  if (!is.finite(value)) return(as.character(value))
  format_decimal3(value)
}

longitudinal_fit_details <- function(model, model_type) {
  details <- data.frame(Item = character(0), Value = character(0), stringsAsFactors = FALSE, check.names = FALSE)
  add_row <- function(item, value) {
    details <<- rbind(details, data.frame(Item = item, Value = as.character(value %||% ""), stringsAsFactors = FALSE, check.names = FALSE))
  }
  if (model_type %in% c("lmm", "glmm")) {
    singular <- tryCatch(lme4::isSingular(model, tol = 1e-4), error = function(e) NA)
    add_row("Singular fit", if (isTRUE(singular)) "Yes" else if (identical(singular, FALSE)) "No" else "Not available")
    variance <- tryCatch(as.data.frame(lme4::VarCorr(model)), error = function(e) data.frame())
    if (is.data.frame(variance) && nrow(variance) > 0 && all(c("grp", "vcov") %in% names(variance))) {
      random_variance <- sum(suppressWarnings(as.numeric(variance$vcov[variance$grp != "Residual"])), na.rm = TRUE)
      residual_variance <- sum(suppressWarnings(as.numeric(variance$vcov[variance$grp == "Residual"])), na.rm = TRUE)
      add_row("Random-effect variance", longitudinal_format_number_static(random_variance))
      if (is.finite(residual_variance) && residual_variance > 0) {
        add_row("Residual variance", longitudinal_format_number_static(residual_variance))
        add_row("Approximate ICC", longitudinal_format_number_static(random_variance / (random_variance + residual_variance)))
      }
    }
  }
  if (model_type %in% c("panel_fe", "panel_re")) {
    summary_model <- tryCatch(summary(model), error = function(e) NULL)
    r_squared <- tryCatch(summary_model$r.squared, error = function(e) NULL)
    if (!is.null(r_squared)) {
      for (name in names(r_squared)) {
        add_row(sprintf("R-squared (%s)", name), longitudinal_format_number_static(r_squared[[name]]))
      }
    }
  }
  details
}

longitudinal_model_rationale <- function(model_type, family, corstr, random_slope) {
  switch(
    as.character(model_type %||% "")[[1]],
    gee = sprintf("Use GEE when the target is a population-averaged longitudinal effect. Report the selected working correlation (%s) and robust sandwich inference.", corstr),
    lmm = if (isTRUE(random_slope)) {
      "Use LMM when the target is subject-specific change in a continuous outcome and subject-specific intercepts/slopes are scientifically plausible."
    } else {
      "Use LMM when the target is subject-specific change in a continuous outcome with cluster-level random intercepts."
    },
    glmm = sprintf("Use GLMM when the target is subject-specific inference for a non-Gaussian outcome using the %s family.", family),
    panel_fe = "Use panel fixed effects when time-invariant unit-level confounding must be controlled and within-unit change is the main source of identification.",
    panel_re = "Use panel random effects only when unit-specific unobserved effects are plausibly independent of included predictors; support this with Hausman screening and study design.",
    "Report why this longitudinal model matches the estimand and data structure."
  )
}

longitudinal_sensitivity_recommendations <- function(model_type, family, corstr, random_slope) {
  switch(
    as.character(model_type %||% "")[[1]],
    gee = c(
      sprintf("Compare the selected GEE working correlation (%s) with independence, exchangeable, and AR(1) when feasible.", corstr),
      "Repeat inference with robust sandwich standard errors and verify that conclusions are stable.",
      "For binary or count outcomes, compare family/link choices when outcome coding or distribution is uncertain."
    ),
    lmm = c(
      "Compare random-intercept and random-slope specifications when time trends may differ by subject.",
      "Repeat key conclusions with GEE if population-averaged inference is also relevant.",
      "Review conclusions after excluding influential subjects or sparse clusters."
    ),
    glmm = c(
      "Compare random-intercept and random-slope specifications when subject-specific time trends are plausible.",
      "Check whether GEE gives similar population-averaged conclusions.",
      "Review overdispersion and sparse outcome patterns; consider alternative family/link if unstable."
    ),
    panel_fe = c(
      "Compare results with and without time fixed effects.",
      "Report cluster-robust standard errors; consider Driscoll-Kraay standard errors when cross-sectional dependence is detected.",
      "Compare with random effects and explain the Hausman result; consider lagged predictors, IV, or dynamic panel methods when reverse causation is plausible."
    ),
    panel_re = c(
      "Compare random effects with fixed effects and report the Hausman result.",
      "Use cluster-robust standard errors and consider Driscoll-Kraay standard errors if common shocks are plausible.",
      "Prefer fixed effects or alternative causal methods if unit effects may correlate with predictors."
    ),
    "Run a sensitivity analysis that changes the plausible correlation, random-effect, or panel structure."
  )
}

longitudinal_sensitivity_row <- function(analysis, comparison, status, metric = "", value = "", note = "") {
  data.frame(
    Analysis = analysis,
    Comparison = comparison,
    Status = status,
    Metric = metric,
    Value = as.character(value %||% ""),
    Note = as.character(note %||% ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_sensitivity_metric_value <- function(aic = NA_real_, bic = NA_real_) {
  parts <- character(0)
  if (is.finite(aic)) parts <- c(parts, sprintf("AIC=%s", longitudinal_format_number_static(aic)))
  if (is.finite(bic)) parts <- c(parts, sprintf("BIC=%s", longitudinal_format_number_static(bic)))
  paste(parts, collapse = "; ")
}

longitudinal_sensitivity_analysis_results <- function(
  data,
  outcome,
  id,
  time,
  terms,
  model_type,
  family,
  corstr,
  random_slope = FALSE,
  exponentiate = FALSE,
  weights = NULL,
  cluster = character(0),
  offset = character(0)
) {
  rows <- list()
  add_row <- function(...) {
    rows[[length(rows) + 1L]] <<- longitudinal_sensitivity_row(...)
  }
  if (identical(model_type, "gee")) {
    correlation_structures <- unique(c(corstr, "independence", "exchangeable", "ar1"))
    for (candidate in correlation_structures) {
      fit <- tryCatch(
        longitudinal_fit_model(data, outcome, id, time, terms, "gee", family, candidate, random_slope = FALSE, exponentiate = exponentiate, weights = weights, cluster = cluster, offset = offset),
        error = function(e) e
      )
      if (inherits(fit, "error")) {
        add_row("GEE correlation sensitivity", candidate, "Failed", "QIC", "", conditionMessage(fit))
      } else {
        qic <- tryCatch(geepack::QIC(fit$model), error = function(e) NA)
        qic_value <- if (is.numeric(qic) && length(qic) > 0) qic[[1]] else NA_real_
        add_row(
          "GEE correlation sensitivity",
          candidate,
          if (identical(candidate, corstr)) "Fitted (selected)" else "Fitted",
          "QIC",
          if (is.finite(qic_value)) longitudinal_format_number_static(qic_value) else "",
          "Compare conclusions across plausible working correlation structures."
        )
      }
    }
  } else if (model_type %in% c("lmm", "glmm")) {
    slope_options <- unique(c(random_slope, !isTRUE(random_slope)))
    for (candidate_slope in slope_options) {
      label <- if (isTRUE(candidate_slope)) "Random intercept + random slope for time" else "Random intercept only"
      fit <- tryCatch(
        longitudinal_fit_model(data, outcome, id, time, terms, model_type, family, corstr, random_slope = candidate_slope, exponentiate = exponentiate, weights = weights, cluster = cluster, offset = offset),
        error = function(e) e
      )
      if (inherits(fit, "error")) {
        add_row("Random-effects sensitivity", label, "Failed", "AIC / BIC", "", conditionMessage(fit))
      } else {
        singular <- tryCatch(lme4::isSingular(fit$model, tol = 1e-4), error = function(e) NA)
        note <- if (isTRUE(singular)) "Singular fit detected; prefer simpler random-effects structure unless justified." else "Review AIC/BIC together with convergence and subject-matter plausibility."
        add_row(
          "Random-effects sensitivity",
          label,
          if (identical(candidate_slope, random_slope)) "Fitted (selected)" else "Fitted",
          "AIC / BIC",
          longitudinal_sensitivity_metric_value(fit$aic, fit$bic),
          note
        )
      }
    }
  } else if (model_type %in% c("panel_fe", "panel_re")) {
    for (candidate_type in c("panel_fe", "panel_re")) {
      fit <- tryCatch(
        longitudinal_fit_model(data, outcome, id, time, terms, candidate_type, family, corstr, random_slope = FALSE, exponentiate = FALSE, weights = weights, cluster = cluster, offset = offset),
        error = function(e) e
      )
      label <- if (identical(candidate_type, "panel_fe")) "Panel fixed effects" else "Panel random effects"
      if (inherits(fit, "error")) {
        add_row("Panel model sensitivity", label, "Failed", "Robust SE", "", conditionMessage(fit))
      } else {
        add_row(
          "Panel model sensitivity",
          label,
          if (identical(candidate_type, model_type)) "Fitted (selected)" else "Fitted",
          "Group-clustered HC1 SE",
          "Available",
          "Compare FE and RE estimates and interpret with the Hausman test and study design."
        )
        dk_summary <- tryCatch(longitudinal_panel_driscoll_kraay_summary(fit$model), error = function(e) e)
        if (inherits(dk_summary, "error")) {
          add_row(
            "Panel covariance sensitivity",
            sprintf("%s: Driscoll-Kraay SE", label),
            "Failed",
            "SE ratio vs HC1",
            "",
            conditionMessage(dk_summary)
          )
        } else {
          add_row(
            "Panel covariance sensitivity",
            sprintf("%s: Driscoll-Kraay SE", label),
            "Computed",
            "SE ratio vs HC1",
            dk_summary,
            "Use as sensitivity inference when cross-sectional dependence or common shocks are plausible; report the chosen covariance estimator explicitly."
          )
        }
      }
    }
    hausman <- tryCatch({
      formula <- longitudinal_formula(outcome, terms, offset = offset)
      fixed <- plm::plm(formula, data = data, index = c(id, time), model = "within", effect = "individual")
      random <- plm::plm(formula, data = data, index = c(id, time), model = "random", effect = "individual")
      plm::phtest(fixed, random)
    }, error = function(e) e)
    if (inherits(hausman, "error")) {
      add_row("Panel model sensitivity", "Hausman FE vs RE", "Failed", "p-value", "", conditionMessage(hausman))
    } else {
      add_row(
        "Panel model sensitivity",
        "Hausman FE vs RE",
        "Computed",
        "p-value",
        if (is.finite(hausman$p.value)) format_p(hausman$p.value) else "",
        if (is.finite(hausman$p.value) && hausman$p.value < 0.05) "Prefer fixed effects if consistent with the estimand." else "Random effects not rejected; still justify the RE independence assumption."
      )
    }
  }
  analysis_bind_rows(rows)
}

longitudinal_trim_weights <- function(weights, trim = "none") {
  trim <- longitudinal_resolve_weight_trim(trim)
  weights <- suppressWarnings(as.numeric(weights))
  if (!length(weights) || any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Weights must be positive finite values.")
  }
  if (identical(trim, "p01_99")) {
    limits <- stats::quantile(weights, c(0.01, 0.99), na.rm = TRUE, names = FALSE)
    weights <- pmin(pmax(weights, limits[[1]]), limits[[2]])
  } else if (identical(trim, "p05_95")) {
    limits <- stats::quantile(weights, c(0.05, 0.95), na.rm = TRUE, names = FALSE)
    weights <- pmin(pmax(weights, limits[[1]]), limits[[2]])
  }
  weights / mean(weights, na.rm = TRUE)
}

longitudinal_weight_stats_text <- function(weights) {
  weights <- suppressWarnings(as.numeric(weights))
  weights <- weights[is.finite(weights)]
  if (length(weights) == 0) {
    return("")
  }
  sprintf(
    "min=%s; median=%s; max=%s",
    longitudinal_format_number_static(min(weights)),
    longitudinal_format_number_static(stats::median(weights)),
    longitudinal_format_number_static(max(weights))
  )
}

longitudinal_effective_sample_size <- function(weights) {
  weights <- suppressWarnings(as.numeric(weights))
  weights <- weights[is.finite(weights) & weights > 0]
  if (length(weights) == 0) return(NA_real_)
  sum(weights)^2 / sum(weights^2)
}

longitudinal_ipw_diagnostics <- function(probability, weights, clipped_probability = NULL, raw_weights = NULL, clipped_weights = NULL, model_terms = character(0), fallback = "") {
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
      "IPW observation model variables",
      "Predicted observation probability: min",
      "Predicted observation probability: median",
      "Predicted observation probability: max",
      "Probability clipping count",
      "Generated IPW summary",
      "Generated IPW effective sample size",
      "Weight clipping count",
      "IPW diagnostic note"
    ),
    Value = c(
      if (length(model_terms) > 0) paste(model_terms, collapse = ", ") else "Intercept only",
      if (length(probability) > 0) longitudinal_format_number_static(min(probability)) else "",
      if (length(probability) > 0) longitudinal_format_number_static(stats::median(probability)) else "",
      if (length(probability) > 0) longitudinal_format_number_static(max(probability)) else "",
      if (is.finite(probability_clipped)) as.character(probability_clipped) else "",
      longitudinal_weight_stats_text(weights),
      if (length(weights) > 0) longitudinal_format_number_static(longitudinal_effective_sample_size(weights)) else "",
      if (is.finite(weight_clipped)) as.character(weight_clipped) else "",
      if (nzchar(fallback)) fallback else "Review positivity and generated-weight stability; report the observation model and clipping."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_weight_summary_table <- function(
  weight_variable = character(0),
  weight_type = "none",
  trim = "none",
  base_weights = NULL,
  ipw_weights = NULL,
  final_weights = NULL,
  ipw_diagnostics = NULL,
  note = ""
) {
  weight_type <- longitudinal_resolve_weight_type(weight_type)
  trim <- longitudinal_resolve_weight_trim(trim)
  has_weights <- !is.null(final_weights) && length(final_weights) > 0
  data.frame(
    Item = c(
      "Weight variable",
      "Weight type",
      "Trimming",
      "Normalization",
      "Base weight summary",
      "IPW summary",
      "Final weight summary",
      "Effective sample size",
      "Note"
    ),
    Value = c(
      paste(as.character(weight_variable %||% character(0)), collapse = ", "),
      longitudinal_weight_type_label(weight_type),
      longitudinal_weight_trim_label(trim),
      if (has_weights) "Mean normalized to 1" else "Not applied",
      longitudinal_weight_stats_text(base_weights),
      longitudinal_weight_stats_text(ipw_weights),
      longitudinal_weight_stats_text(final_weights),
      if (has_weights) longitudinal_format_number_static(longitudinal_effective_sample_size(final_weights)) else "",
      as.character(note %||% "")
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ) |>
    (function(table) {
      if (is.data.frame(ipw_diagnostics) && nrow(ipw_diagnostics) > 0) {
        rbind(table, ipw_diagnostics)
      } else {
        table
      }
    })()
}

longitudinal_prepare_analysis_weights <- function(
  raw_prepared,
  analyzed_data,
  outcome,
  id,
  time,
  terms,
  weight = character(0),
  weight_type = "none",
  trim = "none",
  ipw_auxiliary = character(0)
) {
  weight_type <- longitudinal_resolve_weight_type(weight_type)
  trim <- longitudinal_resolve_weight_trim(trim)
  weight <- utils::head(as.character(weight %||% character(0)), 1)
  if (identical(weight_type, "none")) {
    return(list(
      weights = NULL,
      base_weights = NULL,
      ipw_weights = NULL,
      ipw_diagnostics = data.frame(),
      summary = longitudinal_weight_summary_table(weight, weight_type, trim, note = "No analysis weights were applied."),
      note = "No analysis weights were applied."
    ))
  }

  analyzed_index <- match(rownames(analyzed_data), rownames(raw_prepared))
  if (anyNA(analyzed_index)) {
    stop("Could not align analyzed rows with raw rows for weight construction.")
  }

  needs_user_weight <- weight_type %in% c("sampling", "longitudinal", "combined")
  base_weights <- NULL
  if (needs_user_weight) {
    if (length(weight) != 1 || !nzchar(weight) || !weight %in% names(raw_prepared)) {
      stop("Select one positive numeric weight variable for the selected weight type.")
    }
    base_all <- suppressWarnings(as.numeric(raw_prepared[[weight]]))
    base_weights <- base_all[analyzed_index]
    if (any(!is.finite(base_weights)) || any(base_weights <= 0)) {
      stop("Selected weight variable must be positive and non-missing in analyzed rows.")
    }
  }

  ipw_weights <- NULL
  ipw_diagnostics <- data.frame()
  ipw_note <- ""
  if (weight_type %in% c("ipw", "combined")) {
    ipw <- longitudinal_ipw_weights(raw_prepared, analyzed_data, outcome, id, time, terms, ipw_auxiliary)
    ipw_weights <- ipw$weights
    ipw_diagnostics <- ipw$diagnostics
    ipw_note <- ipw$note
  }

  final_weights <- switch(
    weight_type,
    sampling = base_weights,
    longitudinal = base_weights,
    ipw = ipw_weights,
    combined = base_weights * ipw_weights,
    NULL
  )
  final_weights <- longitudinal_trim_weights(final_weights, trim)
  note <- c(
    switch(
      weight_type,
      sampling = "Selected sampling/baseline longitudinal weights were applied.",
      longitudinal = "Selected time-varying longitudinal weights were applied.",
      ipw = "Generated IPW weights were applied.",
      combined = "Selected analysis weights were multiplied by generated IPW weights.",
      ""
    ),
    ipw_note
  )
  note <- paste(note[nzchar(note)], collapse = " ")
  list(
    weights = final_weights,
    base_weights = base_weights,
    ipw_weights = ipw_weights,
    ipw_diagnostics = ipw_diagnostics,
    summary = longitudinal_weight_summary_table(weight, weight_type, trim, base_weights, ipw_weights, final_weights, ipw_diagnostics, note),
    note = note
  )
}

longitudinal_missing_sensitivity_failure <- function(strategy, message, status = "Failed") {
  data.frame(
    Strategy = strategy,
    Term = "",
    B = NA_real_,
    SE = NA_real_,
    Statistic = NA_real_,
    p = NA_real_,
    LLCI = NA_real_,
    ULCI = NA_real_,
    Status = status,
    Note = as.character(message %||% ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_pool_coef_tables <- function(tables, strategy, note = "", exponentiate = FALSE) {
  tables <- tables[vapply(tables, function(table) is.data.frame(table) && nrow(table) > 0 && all(c("Term", "B", "SE") %in% names(table)), logical(1))]
  if (length(tables) == 0) {
    return(longitudinal_missing_sensitivity_failure(strategy, "No coefficient table was available for pooling."))
  }
  terms <- Reduce(intersect, lapply(tables, function(table) as.character(table$Term)))
  if (length(terms) == 0) {
    return(longitudinal_missing_sensitivity_failure(strategy, "No common coefficient terms were available for pooling."))
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
    m <- length(estimates)
    if (m == 0) {
      return(longitudinal_missing_sensitivity_failure(strategy, sprintf("No finite estimates were available for %s.", term)))
    }
    qbar <- mean(estimates)
    ubar <- mean(variances)
    bvar <- if (m > 1) stats::var(estimates) else 0
    total <- ubar + (1 + 1 / max(m, 1)) * bvar
    se <- sqrt(max(total, 0))
    statistic <- if (is.finite(se) && se > 0) qbar / se else NA_real_
    p <- if (is.finite(statistic)) 2 * stats::pnorm(abs(statistic), lower.tail = FALSE) else NA_real_
    output <- data.frame(
      Strategy = strategy,
      Term = term,
      B = qbar,
      SE = se,
      Statistic = statistic,
      p = p,
      LLCI = qbar - 1.96 * se,
      ULCI = qbar + 1.96 * se,
      Status = "Fitted",
      Note = note,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (isTRUE(exponentiate)) {
      output$`exp(B)` <- exp(output$B)
      output$`exp(LLCI)` <- exp(output$LLCI)
      output$`exp(ULCI)` <- exp(output$ULCI)
    }
    output
  })
  analysis_bind_rows(rows)
}

longitudinal_mice_method <- function(data, id, time) {
  method <- mice::make.method(data)
  method[intersect(c(id, time), names(method))] <- ""
  method
}

longitudinal_mice_predictor_matrix <- function(data, id, time) {
  predictor_matrix <- mice::make.predictorMatrix(data)
  blocked <- intersect(id, colnames(predictor_matrix))
  if (length(blocked) > 0) {
    predictor_matrix[, blocked] <- 0
    predictor_matrix[blocked, ] <- 0
  }
  time_rows <- intersect(time, rownames(predictor_matrix))
  if (length(time_rows) > 0) {
    predictor_matrix[time_rows, ] <- 0
  }
  predictor_matrix
}

longitudinal_mi_sensitivity_results <- function(
  raw_prepared,
  outcome,
  id,
  time,
  terms,
  model_type,
  family,
  corstr,
  random_slope = FALSE,
  exponentiate = FALSE,
  weight = character(0),
  weight_type = "none",
  weight_trim = "none",
  m = 5L,
  maxit = 5L,
  mi_outcome = "observed",
  seed = 20260618L,
  cluster = character(0),
  offset = character(0)
) {
  strategy <- "Multiple imputation (MI)"
  if (!requireNamespace("mice", quietly = TRUE)) {
    return(longitudinal_missing_sensitivity_failure(strategy, "The mice package is required for MI sensitivity analysis."))
  }
  weight_type <- longitudinal_resolve_weight_type(weight_type)
  mi_outcome <- longitudinal_resolve_mi_outcome(mi_outcome)
  weight <- utils::head(as.character(weight %||% character(0)), 1)
  variables <- unique(c(outcome, id, time, cluster, offset, terms, if (weight_type %in% c("sampling", "longitudinal", "combined")) weight else character(0)))
  mi_data <- raw_prepared[, intersect(variables, names(raw_prepared)), drop = FALSE]
  mi_data <- mi_data[stats::complete.cases(mi_data[, intersect(c(id, time), names(mi_data)), drop = FALSE]), , drop = FALSE]
  if (nrow(mi_data) < 3) {
    return(longitudinal_missing_sensitivity_failure(strategy, "Too few rows with observed ID and time were available for MI."))
  }
  if (!anyNA(mi_data)) {
    return(longitudinal_missing_sensitivity_failure(strategy, "No missing values were present in selected model variables; MI was not needed.", status = "Not needed"))
  }
  observed_outcome <- if (outcome %in% names(mi_data)) !is.na(mi_data[[outcome]]) else rep(TRUE, nrow(mi_data))
  imputed <- tryCatch(
    mice::mice(
      mi_data,
      m = m,
      maxit = maxit,
      method = longitudinal_mice_method(mi_data, id, time),
      predictorMatrix = longitudinal_mice_predictor_matrix(mi_data, id, time),
      printFlag = FALSE,
      seed = seed
    ),
    error = function(e) e
  )
  if (inherits(imputed, "error")) {
    return(longitudinal_missing_sensitivity_failure(strategy, conditionMessage(imputed)))
  }
  tables <- list()
  failures <- character(0)
  for (index in seq_len(m)) {
    completed <- tryCatch(mice::complete(imputed, action = index), error = function(e) e)
    if (inherits(completed, "error")) {
      failures <- c(failures, sprintf("imputation %s: %s", index, conditionMessage(completed)))
      next
    }
    if (identical(mi_outcome, "observed")) {
      completed <- completed[observed_outcome, , drop = FALSE]
    }
    completed <- completed[stats::complete.cases(completed[, variables, drop = FALSE]), , drop = FALSE]
    if (id %in% names(completed)) completed[[id]] <- as.factor(completed[[id]])
    imputed_weights <- tryCatch(
      longitudinal_prepare_analysis_weights(raw_prepared, completed, outcome, id, time, terms, weight, weight_type, weight_trim),
      error = function(e) e
    )
    if (inherits(imputed_weights, "error")) {
      failures <- c(failures, sprintf("imputation %s weights: %s", index, conditionMessage(imputed_weights)))
      next
    }
    fit <- tryCatch(
      longitudinal_fit_model(completed, outcome, id, time, terms, model_type, family, corstr, random_slope, exponentiate, weights = imputed_weights$weights, cluster = cluster, offset = offset),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      failures <- c(failures, sprintf("imputation %s: %s", index, conditionMessage(fit)))
    } else {
      tables[[length(tables) + 1L]] <- fit$coef_table
    }
  }
  if (length(tables) == 0) {
    return(longitudinal_missing_sensitivity_failure(strategy, paste(failures, collapse = "; ")))
  }
  outcome_missing_note <- if (outcome %in% names(mi_data) && anyNA(mi_data[[outcome]])) {
    if (identical(mi_outcome, "observed")) {
      "The dependent variable had missing values; rows with originally missing dependent-variable values were excluded from each fitted imputed model."
    } else {
      "The dependent variable had missing values and imputed dependent-variable rows were included as a sensitivity analysis."
    }
  } else {
    "The dependent variable had no missing values in the selected MI data."
  }
  note <- sprintf(
    "Standard mice-based MI sensitivity; pooled across %s imputed dataset(s) using Rubin-style total variance. %s This is not a dedicated multilevel MI engine.",
    length(tables),
    outcome_missing_note
  )
  if (!identical(weight_type, "none")) {
    note <- paste(note, sprintf("Weights: %s.", longitudinal_weight_type_label(weight_type)))
  }
  if (length(failures) > 0) {
    note <- paste(note, sprintf("Failed fits: %s.", paste(failures, collapse = "; ")))
  }
  longitudinal_pool_coef_tables(tables, strategy, note, exponentiate)
}

longitudinal_ipw_weights <- function(raw_prepared, analyzed_data, outcome, id, time, terms, auxiliary = character(0)) {
  variables <- intersect(unique(c(outcome, id, time, terms)), names(raw_prepared))
  observed <- stats::complete.cases(raw_prepared[, variables, drop = FALSE])
  analyzed_index <- match(rownames(analyzed_data), rownames(raw_prepared))
  if (length(unique(observed)) < 2 || anyNA(analyzed_index)) {
    weights <- rep(1, nrow(analyzed_data))
    return(list(
      weights = weights,
      diagnostics = longitudinal_ipw_diagnostics(rep(1, nrow(analyzed_data)), weights, fallback = "Observation status had no variation; unit weights were used."),
      note = "Observation status had no variation; unit weights were used."
    ))
  }
  auxiliary <- setdiff(as.character(auxiliary %||% character(0)), variables)
  candidate_terms <- intersect(unique(c(time, setdiff(terms, outcome), auxiliary)), names(raw_prepared))
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
      diagnostics = longitudinal_ipw_diagnostics(rep(probability, nrow(analyzed_data)), weights, model_terms = character(0), fallback = "No fully observed predictors were available for the observation model; intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis."),
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
      diagnostics = longitudinal_ipw_diagnostics(rep(probability, nrow(analyzed_data)), weights, model_terms = character(0), fallback = note),
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
    diagnostics = longitudinal_ipw_diagnostics(raw_probability, weights, probability, raw_weights, clipped_weights, candidate_terms),
    note = sprintf("Observation model: %s; weights clipped to [%.3f, %.3f] and normalized to mean 1. Report these variables and review positivity/weight stability.", paste(candidate_terms, collapse = ", "), min(weights, na.rm = TRUE), max(weights, na.rm = TRUE))
  )
}

longitudinal_weighted_missing_sensitivity_results <- function(
  strategy_key,
  raw_prepared,
  analyzed_data,
  outcome,
  id,
  time,
  terms,
  model_type,
  family,
  corstr,
  random_slope = FALSE,
  exponentiate = FALSE,
  weight = character(0),
  weight_type = "ipw",
  weight_trim = "none",
  cluster = character(0),
  offset = character(0),
  ipw_auxiliary = character(0)
) {
  strategy <- if (identical(strategy_key, "wgee")) "Weighted GEE (WGEE)" else "Inverse probability weighting (IPW)"
  if (identical(strategy_key, "wgee") && !identical(model_type, "gee")) {
    return(longitudinal_missing_sensitivity_failure(strategy, "WGEE is only available for GEE models."))
  }
  weights <- tryCatch(
    longitudinal_prepare_analysis_weights(raw_prepared, analyzed_data, outcome, id, time, terms, weight, weight_type, weight_trim, ipw_auxiliary),
    error = function(e) e
  )
  if (inherits(weights, "error")) {
    return(longitudinal_missing_sensitivity_failure(strategy, conditionMessage(weights)))
  }
  fit <- tryCatch(
    longitudinal_fit_model(analyzed_data, outcome, id, time, terms, model_type, family, corstr, random_slope, exponentiate, weights = weights$weights, cluster = cluster, offset = offset),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(longitudinal_missing_sensitivity_failure(strategy, conditionMessage(fit)))
  }
  table <- fit$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(longitudinal_missing_sensitivity_failure(strategy, "Weighted model did not return a coefficient table."))
  }
  output <- data.frame(
    Strategy = strategy,
    Term = as.character(table$Term),
    B = suppressWarnings(as.numeric(table$B)),
    SE = suppressWarnings(as.numeric(table$SE)),
    Statistic = suppressWarnings(as.numeric(table$Statistic)),
    p = suppressWarnings(as.numeric(table$p)),
    LLCI = suppressWarnings(as.numeric(table$LLCI)),
    ULCI = suppressWarnings(as.numeric(table$ULCI)),
    Status = "Fitted",
    Note = weights$note,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (isTRUE(exponentiate) && all(c("exp(B)", "exp(LLCI)", "exp(ULCI)") %in% names(table))) {
    output$`exp(B)` <- suppressWarnings(as.numeric(table$`exp(B)`))
    output$`exp(LLCI)` <- suppressWarnings(as.numeric(table$`exp(LLCI)`))
    output$`exp(ULCI)` <- suppressWarnings(as.numeric(table$`exp(ULCI)`))
  }
  output
}

longitudinal_missing_sensitivity_results <- function(
  strategies,
  raw_prepared,
  analyzed_data,
  outcome,
  id,
  time,
  terms,
  model_type,
  family,
  corstr,
  random_slope = FALSE,
  exponentiate = FALSE,
  weight = character(0),
  weight_type = "none",
  weight_trim = "none",
  missing_imputations = 5L,
  missing_iterations = 5L,
  mi_outcome = "observed",
  cluster = character(0),
  offset = character(0),
  ipw_auxiliary = character(0),
  ipw_raw_prepared = raw_prepared
) {
  strategies <- longitudinal_resolve_missing_strategies(strategies, model_type)
  missing_imputations <- longitudinal_resolve_mi_count(missing_imputations, default = 5L, minimum = 2L, maximum = 50L)
  missing_iterations <- longitudinal_resolve_mi_count(missing_iterations, default = 5L, minimum = 1L, maximum = 50L)
  mi_outcome <- longitudinal_resolve_mi_outcome(mi_outcome)
  rows <- list()
  for (strategy in strategies) {
    result <- switch(
      strategy,
      mi = longitudinal_mi_sensitivity_results(raw_prepared, outcome, id, time, terms, model_type, family, corstr, random_slope, exponentiate, weight, weight_type, weight_trim, m = missing_imputations, maxit = missing_iterations, mi_outcome = mi_outcome, cluster = cluster, offset = offset),
      ipw = longitudinal_weighted_missing_sensitivity_results("ipw", ipw_raw_prepared, analyzed_data, outcome, id, time, terms, model_type, family, corstr, random_slope, exponentiate, weight, if (longitudinal_resolve_weight_type(weight_type) %in% c("sampling", "longitudinal", "combined")) "combined" else "ipw", weight_trim, cluster = cluster, offset = offset, ipw_auxiliary = ipw_auxiliary),
      wgee = longitudinal_weighted_missing_sensitivity_results("wgee", ipw_raw_prepared, analyzed_data, outcome, id, time, terms, model_type, family, corstr, random_slope, exponentiate, weight, if (longitudinal_resolve_weight_type(weight_type) %in% c("sampling", "longitudinal", "combined")) "combined" else "ipw", weight_trim, cluster = cluster, offset = offset, ipw_auxiliary = ipw_auxiliary),
      data.frame()
    )
    if (is.data.frame(result) && nrow(result) > 0) {
      rows[[length(rows) + 1L]] <- result
    }
  }
  analysis_bind_rows(rows)
}

longitudinal_software_versions <- function(model_type) {
  package <- longitudinal_required_package(model_type)
  rows <- data.frame(
    Software = "R",
    Version = paste(R.version$major, R.version$minor, sep = "."),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (nzchar(package) && requireNamespace(package, quietly = TRUE)) {
    rows <- rbind(
      rows,
      data.frame(Software = package, Version = as.character(utils::packageVersion(package)), stringsAsFactors = FALSE, check.names = FALSE)
    )
  }
  if (requireNamespace("mice", quietly = TRUE)) {
    rows <- rbind(
      rows,
      data.frame(Software = "mice", Version = as.character(utils::packageVersion("mice")), stringsAsFactors = FALSE, check.names = FALSE)
    )
  }
  rows
}

longitudinal_manuscript_text <- function(result) {
  coef_table <- result$coef_table
  non_intercept <- if (is.data.frame(coef_table) && "Term" %in% names(coef_table)) {
    coef_table[!grepl("^\\(Intercept\\)$", coef_table$Term), , drop = FALSE]
  } else {
    data.frame()
  }
  effect_sentence <- if (is.data.frame(non_intercept) && nrow(non_intercept) > 0) {
    terms <- utils::head(as.character(non_intercept$Term), 3)
    suffix <- if (nrow(non_intercept) > 3) " among other terms" else ""
    sprintf("Fixed-effect estimates were reported for %s%s with 95%% confidence intervals.", paste(terms, collapse = ", "), suffix)
  } else {
    "Fixed-effect estimates were reported with standard errors, p-values, and 95% confidence intervals."
  }
  exp_sentence <- if (isTRUE(result$exponentiate)) {
    "Exponentiated coefficients were additionally reported for the logit/log link model."
  } else {
    ""
  }
  assumption_sentence <- if (is.data.frame(result$assumption_checks) && nrow(result$assumption_checks) > 0) {
    issue_rows <- result$assumption_checks[result$assumption_checks$Result %in% c("Potential violation", "RE assumption doubtful", "Design review required") | result$assumption_checks$Issue %in% TRUE, , drop = FALSE]
    if (nrow(issue_rows) > 0) {
      sprintf("Assumption screening flagged %s; recommended alternative analyses or reporting cautions were generated accordingly.", paste(unique(issue_rows$Check), collapse = ", "))
    } else {
      "Assumption screening did not flag a major issue among the selected checks."
    }
  } else {
    "Assumption screening was not requested or no individual checks were selected."
  }
  sensitivity_result_sentence <- if (is.data.frame(result$sensitivity_results) && nrow(result$sensitivity_results) > 0) {
    sprintf("Automated sensitivity screening generated %s comparison row(s), including fitted alternatives and failed alternatives where applicable.", nrow(result$sensitivity_results))
  } else {
    "Automated sensitivity comparisons were not available for this model."
  }
  data_summary <- sprintf(
    "The analysis used %s, including %s observations from %s subjects/clusters across %s observed time points.",
    result$missing_method_label %||% "complete-case analysis using row-wise complete observations",
    result$n %||% "",
    result$clusters %||% "",
    result$time_points %||% ""
  )
  missing_summary <- if (result$model_type %in% c("lmm", "glmm") && identical(result$missing_method %||% "", "available")) {
    sprintf(
      "%s model row(s) were excluded because selected variables were missing in that row; subjects with other observed visits remained in the likelihood-based analysis under the MAR assumption.",
      result$missing_excluded %||% 0
    )
  } else {
    sprintf(
      "%s observations were excluded because of missing values in selected analysis variables.",
      result$missing_excluded %||% 0
    )
  }
  weight_sentence <- if (!identical(result$weight_type %||% "none", "none")) {
    ess <- if (is.data.frame(result$weight_summary)) result$weight_summary$Value[result$weight_summary$Item == "Effective sample size"] else ""
    sprintf("Analysis weights were applied as %s with effective sample size %s.", longitudinal_weight_type_label(result$weight_type), ess %||% "")
  } else {
    "No analysis weights were applied."
  }
  missing_engine_sentence <- if (is.data.frame(result$missing_sensitivity_results) && nrow(result$missing_sensitivity_results) > 0) {
    fitted <- sum(result$missing_sensitivity_results$Status %in% "Fitted", na.rm = TRUE)
    failed <- sum(result$missing_sensitivity_results$Status %in% "Failed", na.rm = TRUE)
    sprintf("Missing-data sensitivity engines were run for %s (%s fitted row(s), %s failed row(s)); MI/IPW/WGEE outputs should be reported as sensitivity analyses unless the missing-data model is prespecified as primary.", paste(result$missing_strategy_labels %||% character(0), collapse = ", "), fitted, failed)
  } else {
    "No MI/IPW/WGEE missing-data sensitivity engine was selected."
  }
  software_summary <- if (is.data.frame(result$software_versions) && nrow(result$software_versions) > 0) {
    sprintf("Analyses were performed using %s.", paste(sprintf("%s %s", result$software_versions$Software, result$software_versions$Version), collapse = ", "))
  } else {
    "Software and package versions should be reported."
  }
  data.frame(
    Section = c("Methods", "Results", "Assumptions", "Sensitivity", "Software"),
    SuggestedText = c(
      sprintf("%s %s %s %s %s", result$model_rationale %||% "", data_summary, missing_summary, weight_sentence, missing_engine_sentence),
      trimws(sprintf("%s %s", effect_sentence, exp_sentence)),
      assumption_sentence,
      paste(c(sensitivity_result_sentence, result$sensitivity_recommendations %||% "Sensitivity analyses should be reported when feasible."), collapse = " "),
      software_summary
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_publication_notes <- function(result) {
  estimand <- if (identical(result$model_type, "gee")) {
    "population-averaged effects"
  } else if (result$model_type %in% c("lmm", "glmm")) {
    "subject-specific effects"
  } else if (identical(result$model_type, "panel_fe")) {
    "within-unit fixed-effect estimates"
  } else if (identical(result$model_type, "panel_re")) {
    "random-effect panel estimates"
  } else {
    "longitudinal model estimates"
  }
  se_note <- if (identical(result$model_type, "gee")) {
    "GEE standard errors use robust sandwich inference."
  } else if (result$model_type %in% c("panel_fe", "panel_re")) {
    "Panel regression coefficient standard errors use group-clustered HC1 robust covariance."
  } else {
    "Mixed-model fixed-effect standard errors are reported on the model scale."
  }
  link_note <- if (isTRUE(result$exponentiate)) {
    if (identical(result$family, "binomial")) {
      "exp(B) is interpreted as an odds ratio."
    } else if (result$family %in% c("poisson", "negative_binomial")) {
      "exp(B) is interpreted as a rate ratio."
    } else if (identical(result$family, "gamma")) {
      "exp(B) is interpreted as a mean ratio for the Gamma log-link model."
    } else {
      "exp(B) is reported for the fitted log-link or logit-link model."
    }
  } else {
    "B is reported on the model scale."
  }
  missing_note <- if (result$model_type %in% c("lmm", "glmm") && identical(result$missing_method %||% "", "available")) {
    sprintf(
      "Missing data were handled using %s; %s model row(s) with missing selected variables were excluded, while subjects with other observed repeated measures remained in the likelihood.",
      result$missing_method_label %||% "likelihood-based mixed-model analysis using available repeated measures under a MAR assumption",
      result$missing_excluded %||% 0
    )
  } else {
    sprintf(
      "Missing data were handled using %s; %s observation(s) were excluded because of missing selected analysis variables.",
      result$missing_method_label %||% "complete-case analysis using row-wise complete observations",
      result$missing_excluded %||% 0
    )
  }
  weight_note <- if (!identical(result$weight_type %||% "none", "none")) {
    sprintf("Analysis weights: %s; %s.", longitudinal_weight_type_label(result$weight_type), result$weight_summary$Value[result$weight_summary$Item == "Final weight summary"] %||% "")
  } else {
    "No analysis weights were applied."
  }
  advanced_missing_note <- if (is.data.frame(result$missing_sensitivity_results) && nrow(result$missing_sensitivity_results) > 0) {
    sprintf("Missing-data sensitivity engines were run: %s. Interpret these as sensitivity analyses and report the imputation or weighting model assumptions.", paste(unique(result$missing_sensitivity_results$Strategy), collapse = ", "))
  } else {
    "Missing-data sensitivity analysis with MI/IPW/WGEE was not selected."
  }
  cluster_note <- sprintf(
    "The repeated-measures structure used %s subject/cluster units and %s observed time point(s).",
    result$clusters %||% "",
    result$time_points %||% ""
  )
  sensitivity_note <- if (is.data.frame(result$sensitivity_results) && nrow(result$sensitivity_results) > 0) {
    "Automated sensitivity comparisons are reported separately and should be interpreted with the study design."
  } else {
    "Sensitivity analyses should be reported when feasible."
  }
  data.frame(
    Note = c(
      sprintf("Estimates represent %s from the selected %s.", estimand, result$method %||% "longitudinal model"),
      "CI = confidence interval; SE = standard error.",
      se_note,
      link_note,
      cluster_note,
      missing_note,
      weight_note,
      advanced_missing_note,
      sensitivity_note
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_reporting_checklist <- function(result) {
  assumption_count <- if (is.data.frame(result$assumption_checks)) nrow(result$assumption_checks) else 0L
  data.frame(
    Item = c(
      "Model rationale",
      "Data structure summarized",
      "Missing data described",
      "Analysis weights described",
      "Missing-data sensitivity engine run",
      "Assumptions checked",
      "Recommended alternatives provided",
      "Effect estimate and 95% CI reported",
      "Sensitivity analysis suggested",
      "Automated sensitivity comparison reported",
      "Publication table notes generated",
      "Software/package version reported",
      "Manuscript-ready text generated"
    ),
    Status = c(
      if (nzchar(result$model_rationale %||% "")) "Ready" else "Needs review",
      if (is.data.frame(result$data_structure) && nrow(result$data_structure) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$missing_table) && nrow(result$missing_table) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$weight_summary) && nrow(result$weight_summary) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$missing_sensitivity_results) && nrow(result$missing_sensitivity_results) > 0) "Ready" else "Not selected",
      if (assumption_count > 0) "Ready" else "Not selected",
      if (length(result$recommendations %||% character(0)) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$coef_table) && all(c("LLCI", "ULCI") %in% names(result$coef_table))) "Ready" else "Needs review",
      if (length(result$sensitivity_recommendations %||% character(0)) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$sensitivity_results) && nrow(result$sensitivity_results) > 0) "Ready" else "Not available",
      if (is.data.frame(result$publication_notes) && nrow(result$publication_notes) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$software_versions) && nrow(result$software_versions) > 0) "Ready" else "Needs review",
      if (is.data.frame(result$manuscript_text) && nrow(result$manuscript_text) > 0) "Ready" else "Needs review"
    ),
    Details = c(
      result$model_rationale %||% "",
      "Subject count, time points, balanced/unbalanced status, and cluster size are summarized.",
      "Missing-data method, exclusions, and variable-level missingness are summarized.",
      if (is.data.frame(result$weight_summary) && nrow(result$weight_summary) > 0) paste(sprintf("%s: %s", result$weight_summary$Item, result$weight_summary$Value), collapse = " ") else "Analysis weight summary was not generated.",
      if (is.data.frame(result$missing_sensitivity_results) && nrow(result$missing_sensitivity_results) > 0) sprintf("%s missing-data sensitivity result row(s) generated.", nrow(result$missing_sensitivity_results)) else "MI/IPW/WGEE sensitivity engine was not selected.",
      sprintf("%s assumption check item(s) reported.", assumption_count),
      paste(result$recommendations %||% character(0), collapse = " "),
      "Coefficient table includes B, SE, p-value, and 95% CI; exp(B) is added for logit/log models.",
      paste(result$sensitivity_recommendations %||% character(0), collapse = " "),
      if (is.data.frame(result$sensitivity_results) && nrow(result$sensitivity_results) > 0) sprintf("%s automated sensitivity comparison row(s) generated.", nrow(result$sensitivity_results)) else "No automated sensitivity comparison was generated.",
      "Footnotes for estimand, standard errors, confidence intervals, missing data, and sensitivity interpretation are provided.",
      paste(sprintf("%s %s", result$software_versions$Software, result$software_versions$Version), collapse = "; "),
      "Suggested Methods, Results, Assumptions, Sensitivity, and Software text is provided for manuscript drafting."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

prepare_longitudinal_analysis_result <- function(
  data,
  outcome,
  id,
  time,
  exposure = character(0),
  cluster = character(0),
  predictors = character(0),
  covariates = character(0),
  weight = character(0),
  model_type = "gee",
  family = "auto",
  corstr = "exchangeable",
  random_slope = FALSE,
  include_time = TRUE,
  exponentiate = TRUE,
  assumption_checks = TRUE,
  check_options = NULL,
  missing_method = "row_complete",
  missing_strategies = character(0),
  missing_imputations = 5L,
  missing_iterations = 5L,
  mi_outcome = "observed",
  weight_type = "none",
  weight_trim = "none",
  ipw_auxiliary = character(0),
  variable_info = NULL,
  reference_values = character(0)
) {
  data_names <- names(data)
  outcome <- intersect(as.character(outcome %||% character(0)), data_names)
  id <- intersect(as.character(id %||% character(0)), data_names)
  cluster <- intersect(utils::head(as.character(cluster %||% character(0)), 1), data_names)
  time <- intersect(as.character(time %||% character(0)), data_names)
  exposure <- intersect(utils::head(as.character(exposure %||% character(0)), 1), data_names)
  predictors <- intersect(unique(as.character(predictors %||% character(0))), data_names)
  covariates <- intersect(unique(as.character(covariates %||% character(0))), data_names)
  weight <- intersect(utils::head(as.character(weight %||% character(0)), 1), data_names)
  ipw_auxiliary <- intersect(unique(as.character(ipw_auxiliary %||% character(0))), data_names)
  weight_type <- longitudinal_resolve_weight_type(weight_type)
  weight_trim <- longitudinal_resolve_weight_trim(weight_trim)
  missing_imputations <- longitudinal_resolve_mi_count(missing_imputations, default = 5L, minimum = 2L, maximum = 50L)
  missing_iterations <- longitudinal_resolve_mi_count(missing_iterations, default = 5L, minimum = 1L, maximum = 50L)
  mi_outcome <- longitudinal_resolve_mi_outcome(mi_outcome)
  shiny::validate(shiny::need(length(outcome) == 1, "Select one outcome variable."))
  shiny::validate(shiny::need(length(id) == 1, "Select one subject / cluster ID variable."))
  shiny::validate(shiny::need(length(time) == 1, "Select one time variable."))
  outcome <- outcome[[1]]
  id <- id[[1]]
  time <- time[[1]]
  cluster <- setdiff(cluster, c(outcome, id, time))
  exposure <- setdiff(exposure, c(outcome, id, cluster, time))
  effective_cluster <- if (model_type %in% c("lmm", "glmm")) cluster else character(0)
  ignored_cluster <- if (!model_type %in% c("lmm", "glmm") && length(cluster) == 1) cluster else character(0)
  weight <- setdiff(weight, c(outcome, id, cluster, time, exposure))
  weight_type <- longitudinal_resolve_context_weight_type(weight_type, model_type, length(weight) == 1)
  shiny::validate(shiny::need(!weight_type %in% c("sampling", "longitudinal", "combined") || length(weight) == 1, "Select one weight variable for the selected weight type."))
  active_weight <- if (weight_type %in% c("sampling", "longitudinal", "combined")) weight else character(0)
  predictors <- setdiff(predictors, c(outcome, id, cluster, time, exposure, active_weight))
  covariates <- setdiff(covariates, c(outcome, id, cluster, time, exposure, predictors, active_weight))
  ipw_auxiliary <- setdiff(ipw_auxiliary, c(outcome, id, cluster, time, exposure, predictors, covariates, weight))
  terms <- longitudinal_terms(time, predictors, covariates, include_time)
  shiny::validate(shiny::need(length(terms) > 0, "Select at least one predictor/covariate or include time as a fixed effect."))

  package <- longitudinal_required_package(model_type)
  if (nzchar(package) && !requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("The %s package is required for this model.", package))
  }

  prepared <- longitudinal_prepare_data(data, outcome, id, time, exposure, predictors, covariates, effective_cluster, weight, variable_info, reference_values, missing_method)
  ipw_raw_prepared <- prepared$raw_prepared
  if (length(ipw_auxiliary) > 0) {
    auxiliary_data <- data[, ipw_auxiliary, drop = FALSE]
    auxiliary_data <- prepare_regression_model_data_static(
      auxiliary_data,
      ipw_auxiliary,
      variable_info = variable_info,
      reference_values = reference_values
    )
    auxiliary_data <- auxiliary_data[, setdiff(names(auxiliary_data), names(ipw_raw_prepared)), drop = FALSE]
    if (ncol(auxiliary_data) > 0) {
      ipw_raw_prepared <- cbind(ipw_raw_prepared, auxiliary_data)
    }
  }
  requested_family <- longitudinal_auto_family(prepared$data, outcome, variable_info, family)
  model_family <- requested_family
  if (identical(model_type, "lmm")) {
    model_family <- "gaussian"
  }
  if (model_type %in% c("panel_fe", "panel_re")) {
    model_family <- "gaussian"
  }
  if (identical(model_type, "glmm") && identical(model_family, "gaussian")) {
    stop("GLMM requires a non-Gaussian family. Use LMM for Gaussian continuous outcomes.")
  }
  family_issue <- longitudinal_family_outcome_issue(prepared$data, outcome, model_family)
  if (nzchar(family_issue)) {
    results <- list()
    attr(results, "warnings") <- character(0)
    attr(results, "skipped") <- longitudinal_guard_row(outcome, id, time, terms, family_issue, prepared$n, variable_info)
    return(results)
  }
  offset_variable <- if (length(exposure) == 1 && model_family %in% longitudinal_offset_families()) exposure else character(0)
  formula <- longitudinal_formula(outcome, terms, offset = offset_variable)
  preflight <- longitudinal_model_frame_preflight(prepared$data, formula, outcome, id, time, terms, variable_info)
  if (!isTRUE(preflight$ok)) {
    results <- list()
    attr(results, "skipped") <- preflight$skipped
    return(results)
  }
  analysis_weights <- tryCatch(
    longitudinal_prepare_analysis_weights(prepared$raw_prepared, prepared$data, outcome, id, time, terms, weight, weight_type, weight_trim),
    error = function(e) e
  )
  if (inherits(analysis_weights, "error")) {
    results <- list()
    attr(results, "warnings") <- preflight$warnings
    attr(results, "skipped") <- longitudinal_guard_row(outcome, id, time, terms, conditionMessage(analysis_weights), prepared$n, variable_info)
    return(results)
  }
  count_selection <- NULL
  if (identical(model_family, "count")) {
    count_selection <- longitudinal_count_family_selection(
      prepared$data,
      formula,
      model_type,
      id,
      time,
      effective_cluster,
      random_slope,
      weights = analysis_weights$weights
    )
    model_family <- count_selection$family
  }
  fit <- tryCatch(
    longitudinal_fit_model(prepared$data, outcome, id, time, terms, model_type, model_family, corstr, random_slope, exponentiate && model_family %in% longitudinal_log_ratio_families(), weights = analysis_weights$weights, cluster = effective_cluster, offset = offset_variable),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    results <- list()
    attr(results, "warnings") <- preflight$warnings
    attr(results, "skipped") <- longitudinal_guard_row(outcome, id, time, terms, conditionMessage(fit), prepared$n, variable_info)
    return(results)
  }
  result <- list(
    model = fit$model,
    model_type = model_type,
    method = longitudinal_model_label(model_type, model_family),
    family = model_family,
    requested_family = requested_family,
    corstr = corstr,
    formula = fit$formula,
    outcome = outcome,
    id = id,
    cluster = effective_cluster,
    selected_cluster = cluster,
    ignored_cluster = ignored_cluster,
    time = time,
    exposure = exposure,
    offset_variable = offset_variable,
    predictors = predictors,
    covariates = covariates,
    reference_values = reference_values,
    weight = weight,
    terms = terms,
    n = prepared$n,
    missing_excluded = prepared$excluded,
    missing_method = prepared$missing_method,
    missing_method_label = prepared$missing_method_label,
    missing_method_detail = longitudinal_missing_method_detail(prepared$missing_method, model_type),
    missing_strategies = longitudinal_resolve_missing_strategies(missing_strategies, model_type),
    missing_strategy_labels = longitudinal_missing_strategy_labels(missing_strategies, model_type),
    missing_strategy_notes = longitudinal_missing_strategy_details(missing_strategies, model_type),
    missing_imputations = missing_imputations,
    missing_iterations = missing_iterations,
    mi_outcome = mi_outcome,
    ipw_auxiliary = ipw_auxiliary,
    missing_sensitivity_results = longitudinal_missing_sensitivity_results(
      missing_strategies,
      prepared$raw_prepared,
      prepared$data,
      outcome,
      id,
      time,
      terms,
      model_type,
      model_family,
      corstr,
      random_slope,
      exponentiate && model_family %in% longitudinal_log_ratio_families(),
      weight,
      weight_type,
      weight_trim,
      missing_imputations,
      missing_iterations,
      mi_outcome,
      cluster = effective_cluster,
      offset = offset_variable,
      ipw_auxiliary = ipw_auxiliary,
      ipw_raw_prepared = ipw_raw_prepared
    ),
    missing_excluded_subjects = prepared$excluded_subjects,
    missing_pattern = prepared$missing_pattern,
    missing_by_time = prepared$missing_by_time,
    weight_type = weight_type,
    weight_trim = weight_trim,
    analysis_weights = analysis_weights$weights,
    weight_summary = analysis_weights$summary,
    clusters = preflight$clusters,
    time_points = preflight$time_points,
    aic = fit$aic,
    bic = fit$bic,
    data_structure = longitudinal_data_structure_summary(
      prepared$data,
      prepared$raw_n,
      prepared$n,
      prepared$excluded,
      id,
      time,
      effective_cluster,
      prepared$missing_table,
      prepared$missing_method_label,
      prepared$excluded_subjects
    ),
    missing_table = prepared$missing_table,
    fit_details = analysis_bind_rows(list(longitudinal_fit_details(fit$model, model_type), count_selection$details %||% data.frame())),
    model_rationale = longitudinal_model_rationale(model_type, model_family, corstr, random_slope),
    sensitivity_recommendations = longitudinal_sensitivity_recommendations(model_type, model_family, corstr, random_slope),
    sensitivity_results = longitudinal_sensitivity_analysis_results(
      prepared$data,
      outcome,
      id,
      time,
      terms,
      model_type,
      model_family,
      corstr,
      random_slope,
      exponentiate && model_family %in% longitudinal_log_ratio_families(),
      weights = analysis_weights$weights,
      cluster = effective_cluster,
      offset = offset_variable
    ),
    software_versions = longitudinal_software_versions(model_type),
    coef_table = fit$coef_table,
    random_slope = random_slope,
    exponentiate = exponentiate && model_family %in% longitudinal_log_ratio_families(),
    notes = c(
      fit$fit_note %||% character(0),
      if (length(offset_variable) == 1) sprintf("Exposure offset applied as log(%s) for the fitted count/rate model.", display_variable_name_static(offset_variable, variable_info, character(0), label_only = TRUE)) else character(0),
      if (length(ignored_cluster) == 1) sprintf("Cluster ID %s was selected but is not used by the selected GEE/panel primary fit.", display_variable_name_static(ignored_cluster, variable_info, character(0), label_only = TRUE)) else character(0),
      longitudinal_model_notes(model_type, model_family, corstr, random_slope, exponentiate, id, time, effective_cluster, variable_info)
    )
  )
  if (isTRUE(assumption_checks)) {
    review <- longitudinal_assumption_checks(prepared$data, fit$model, model_type, model_family, formula, id, time, corstr, effective_cluster, random_slope, variable_info, check_options)
    result$assumption_checks <- review$checks
    result$recommendations <- review$recommendations
  } else {
    result$assumption_checks <- data.frame()
    result$recommendations <- "Assumption checks were not requested."
  }
  result$manuscript_text <- longitudinal_manuscript_text(result)
  result$publication_notes <- longitudinal_publication_notes(result)
  result$reporting_checklist <- longitudinal_reporting_checklist(result)
  results <- list(result)
  attr(results, "warnings") <- preflight$warnings
  attr(results, "skipped") <- NULL
  results
}
