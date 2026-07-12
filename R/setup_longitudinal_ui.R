# Longitudinal / panel model setup UI.

longitudinal_setup_state <- function(
  selected_names,
  variable_table,
  labels = character(0),
  outcome = character(0),
  id = character(0),
  cluster = character(0),
  time = character(0),
  exposure = character(0),
  predictors = character(0),
  weight = character(0),
  selected_available = NULL,
  selected_outcome = NULL,
  selected_id = NULL,
  selected_cluster = NULL,
  selected_time = NULL,
  selected_exposure = NULL,
  selected_predictors = NULL,
  selected_weight = NULL,
  model_type = "gee",
  family = "auto",
  corstr = "exchangeable",
  include_time = TRUE,
  random_slope = FALSE,
  exponentiate = TRUE,
  assumption_checks = TRUE,
  check_options = list(),
  missing_method = NULL,
  missing_strategies = character(0),
  missing_strategy = NULL,
  missing_imputations = 5L,
  missing_iterations = 5L,
  mi_outcome = "observed",
  ipw_auxiliary = character(0),
  weight_type = "none",
  weight_trim = "none",
  options_tab = "Model",
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  selected <- as.character(selected_names %||% character(0))
  selected_single <- function(value) {
    utils::head(intersect(as.character(value %||% character(0)), selected), 1)
  }
  outcome <- selected_single(outcome)
  id <- selected_single(id)
  cluster <- selected_single(cluster)
  time <- selected_single(time)
  exposure <- selected_single(intersect(as.character(exposure %||% character(0)), analysis_allowed_variables(selected, variable_table, "continuous")))
  weight <- selected_single(intersect(as.character(weight %||% character(0)), analysis_allowed_variables(selected, variable_table, "continuous")))
  predictors <- intersect(as.character(predictors %||% character(0)), selected)
  assigned <- unique(c(outcome, id, cluster, time, exposure, predictors, weight))
  available <- setdiff(selected, assigned)
  ipw_auxiliary_choices <- available
  ipw_auxiliary <- intersect(as.character(ipw_auxiliary %||% character(0)), ipw_auxiliary_choices)

  include_time <- isTRUE(include_time)
  terms_selected <- include_time || length(predictors) > 0

  current_model <- as.character(model_type %||% "gee")[[1]]
  resolved_missing_strategy <- if (!is.null(missing_strategy)) {
    longitudinal_resolve_missing_strategy(missing_strategy, current_model)
  } else if (length(missing_strategies) > 0) {
    longitudinal_resolve_missing_strategy(utils::head(missing_strategies, 1), current_model)
  } else if (is.null(missing_method)) {
    longitudinal_default_missing_strategy(current_model)
  } else {
    longitudinal_resolve_missing_strategy(missing_method, current_model)
  }
  resolved_missing_method <- longitudinal_missing_strategy_method(resolved_missing_strategy, current_model)
  resolved_missing_strategies <- longitudinal_missing_strategy_engines(resolved_missing_strategy, current_model)
  resolved_weight_type <- longitudinal_resolve_context_weight_type(weight_type, current_model, length(weight) == 1)
  resolved_check_options <- longitudinal_resolve_check_options(current_model, check_options)
  resolved_options_tab <- as.character(options_tab %||% "Model")[[1]]
  if (resolved_options_tab %in% c("Terms", "Output")) {
    resolved_options_tab <- "Model"
  }
  if (!resolved_options_tab %in% c("Model", "Weights", "Missing", "Checks")) {
    resolved_options_tab <- "Model"
  }

  list(
    selected = selected,
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    outcome = outcome,
    outcome_items = analysis_variable_items(outcome, variable_table, labels),
    outcome_selected = selected_order_items(selected_outcome, outcome),
    id = id,
    id_items = analysis_variable_items(id, variable_table, labels),
    id_selected = selected_order_items(selected_id, id),
    cluster = cluster,
    cluster_items = analysis_variable_items(cluster, variable_table, labels),
    cluster_selected = selected_order_items(selected_cluster, cluster),
    time = time,
    time_items = analysis_variable_items(time, variable_table, labels),
    time_selected = selected_order_items(selected_time, time),
    exposure = exposure,
    exposure_items = analysis_variable_items(exposure, variable_table, labels),
    exposure_selected = selected_order_items(selected_exposure, exposure),
    weight = weight,
    weight_items = analysis_variable_items(weight, variable_table, labels),
    weight_selected = selected_order_items(selected_weight, weight),
    weight_choices = longitudinal_weight_variable_choices(weight, selected, variable_table, labels),
    predictors = predictors,
    predictor_items = analysis_variable_items(predictors, variable_table, labels),
    predictor_selected = selected_order_items(selected_predictors, predictors),
    model_type = current_model,
    family = as.character(family %||% "auto")[[1]],
    corstr = as.character(corstr %||% "exchangeable")[[1]],
    include_time = include_time,
    random_slope = isTRUE(random_slope),
    exponentiate = isTRUE(exponentiate),
    assumption_checks = isTRUE(assumption_checks),
    check_options = resolved_check_options,
    missing_method = resolved_missing_method,
    missing_strategy = resolved_missing_strategy,
    missing_method_detail = longitudinal_missing_method_detail(resolved_missing_method, current_model),
    missing_strategy_detail = longitudinal_missing_strategy_detail(resolved_missing_strategy, current_model),
    missing_strategies = resolved_missing_strategies,
    missing_strategy_details = longitudinal_missing_strategy_details(resolved_missing_strategies, current_model),
    missing_imputations = longitudinal_resolve_mi_count(missing_imputations, default = 5L, minimum = 2L, maximum = 50L),
    missing_iterations = longitudinal_resolve_mi_count(missing_iterations, default = 5L, minimum = 1L, maximum = 50L),
    mi_outcome = longitudinal_resolve_mi_outcome(mi_outcome),
    ipw_auxiliary = ipw_auxiliary,
    ipw_auxiliary_choices = display_variable_choices_with_measurements(ipw_auxiliary_choices, variable_table, labels),
    weight_type = resolved_weight_type,
    weight_type_detail = longitudinal_weight_type_detail(resolved_weight_type, current_model),
    weight_trim = longitudinal_resolve_weight_trim(weight_trim),
    weight_trim_detail = longitudinal_weight_trim_detail(weight_trim),
    options_tab = resolved_options_tab,
    can_run = length(outcome) == 1 && length(id) == 1 && length(time) == 1 && terms_selected && (!resolved_weight_type %in% c("sampling", "longitudinal", "combined") || length(weight) == 1),
    move_disabled = length(selected) == 0,
    has_assignment = length(assigned) > 0,
    language = language
  )
}

longitudinal_ui_text <- function(text, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  key <- tolower(gsub("[^A-Za-z0-9]+", "_", as.character(text %||% "")))
  key <- gsub("^_+|_+$", "", key)
  translated <- statedu_t(paste0("longitudinal.ui.", key), language, fallback = "")
  if (nzchar(translated)) {
    return(translated)
  }
  if (!identical(language, "ko")) {
    return(text)
  }
  h <- statedu_utf8
  labels <- c(
    "Panel structure" = h("ed8ca8eb849020eab5aceca1b0"),
    "Subject ID" = h("eb8c80ec8381ec9e90204944"),
    "Cluster ID (optional)" = h("ed81b4eb9facec8aa4ed84b020494428ec84a0ed839d29"),
    "Time variable" = h("ec8b9ceab08420ebb380ec8898"),
    "Exposure / offset (optional)" = h("eb85b8ecb69c2fec98a4ed9484ec858b28ec84a0ed839d29"),
    "Model variables" = h("ebaaa8ed989520ebb380ec8898"),
    "Dependent variable" = h("eca285ec868debb380ec8898"),
    "Model" = h("ebaaa8ed9895"),
    "Weights" = h("eab080eca491ecb998"),
    "Missing" = h("eab2b0ecb8a1"),
    "Checks" = h("eca090eab280"),
    "Model type" = h("ebaaa8ed989520ec9ca0ed9895"),
    "Outcome family" = h("eab2b0eab3bcebb380ec889820ebb684ed8fac"),
    "GEE correlation" = h("47454520ec8381eab480eab5aceca1b0"),
    "Terms" = h("ebaaa8ed989520ec84a4eca095"),
    "Include time as fixed effect" = h("ec8b9ceab084ec9d8420eab3a0eca095ed9aa8eab3bceba19c20ed8faced95a8"),
    "Random effects" = h("ebacb4ec9e91ec9c84ed9aa8eab3bc"),
    "Random slope for selected time variable" = h("ec84a0ed839ded959c20ec8b9ceab08420ebb380ec8898ec9d9820ebacb4ec9e91ec9c8420eab8b0ec9ab8eab8b0"),
    "Reporting" = h("eab2b0eab3bc20ed919cec8b9c"),
    "Report exp(B) for logit / log models" = h("eba19ceca7932feba19ceab7b820ebaaa8ed9895ec9790ec849c2065787028422920ebb3b4eab3a0"),
    "Weight variable" = h("eab080eca491ecb99820ebb380ec8898"),
    "Weight type" = h("eab080eca491ecb99820ec9ca0ed9895"),
    "Trim extreme weights" = h("eab7b9eb8ba820eab080eca491ecb99820eca088eb8ba8"),
    "Missing-data strategy" = h("eab2b0ecb8a1ec9e90eba38c20ecb298eba6ac"),
    "Multiple imputation settings" = h("eb8ba4eca491eb8c80ecb2b420ec84a4eca095"),
    "Dependent-variable handling" = h("eca285ec868debb380ec889820ecb298eba6ac"),
    "MI datasets" = h("eb8c80ecb2b420ec9e90eba38c20ec8898"),
    "MI iterations" = h("eb8c80ecb2b420ebb098ebb3b520ec8898"),
    "IPW observation model" = h("49505720eab480ecb0b020ebaaa8ed9895"),
    "Auxiliary variables" = h("ebb3b4eca1b020ebb380ec8898"),
    "Assumption review" = h("eab080eca09520eca090eab280"),
    "Run assumption checks and recommendations" = h("eab080eca09520eca090eab28020ebb08f20eab68ceab3a020ec8ba4ed9689"),
    "Checks for selected model" = h("ec84a0ed839ded959c20ebaaa8ed9895ec9d9820eca090eab28020ed95adebaaa9"),
    "Run model" = h("ebaaa8ed989520ec8ba4ed9689"),
    "Reset setting" = h("ec84a4eca09520ecb488eab8b0ed9994"),
    "Up" = h("ec9c84eba19c"),
    "Down" = h("ec9584eb9e98eba19c")
  )
  if (text %in% names(labels)) labels[[text]] else text
}

longitudinal_independent_variables_label <- function(n, language = statedu_initial_language()) {
  if (identical(normalize_app_language(language), "ko")) {
    return(sprintf("%s (%s)", statedu_utf8("eb8f85eba6bdebb380ec8898"), n))
  }
  sprintf("Independent variables (%s)", n)
}

longitudinal_ui_choices <- function(choices, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  h <- statedu_utf8
  labels <- c(
    "GEE: population-averaged model" = h("4745453a20ebaaa8eca791eb8ba820ed8f89eab7a020ebaaa8ed9895"),
    "LMM: linear mixed model" = h("4c4d4d3a20ec84a0ed989520ed98bced95a9ebaaa8ed9895"),
    "GLMM: generalized linear mixed model" = h("474c4d4d3a20ec9dbcebb098ed999420ec84a0ed989520ed98bced95a9ebaaa8ed9895"),
    "Panel fixed effects" = h("ed8ca8eb849020eab3a0eca095ed9aa8eab3bc"),
    "Panel random effects" = h("ed8ca8eb849020ebacb4ec9e91ec9c84ed9aa8eab3bc"),
    "Auto" = h("ec9e90eb8f99"),
    "Linear: Gaussian / identity" = h("ec84a0ed98953a20476175737369616e202f206964656e74697479"),
    "Binary: logistic / logit" = h("ec9db4ebb684ed98953a206c6f676973746963202f206c6f676974"),
    "Gamma: positive skewed continuous / log" = h("eab090eba7883a20ec9691ec9d9820ec999ceb8f8420ec97b0ec868ded9895202f206c6f67"),
    "Count: Poisson or negative binomial / log" = h("eab09cec88983a20506f6973736f6e20eb9890eb8a94206e656761746976652062696e6f6d69616c202f206c6f67"),
    "Exchangeable" = h("eab590ed9998eab080eb8aa5"),
    "Independence" = h("eb8f85eba6bd"),
    "Unstructured" = h("ebb984eab5aceca1b0ed9994"),
    "No weights" = h("eab080eca491ecb99820ec9786ec9d8c"),
    "Sampling / baseline longitudinal weight" = h("ed919cebb3b82feab8b0eca08020eca285eb8ba820eab080eca491ecb998"),
    "Time-varying longitudinal weight" = h("ec8b9ceab084ebb380ed999420eca285eb8ba820eab080eca491ecb998"),
    "Analysis weight x generated IPW" = h("ebb684ec849d20eab080eca491ecb998207820ec839dec84b120495057"),
    "No weight variable" = h("eab080eca491ecb99820ebb380ec889820ec9786ec9d8c"),
    "None" = h("eca088eb8ba820ec9786ec9d8c"),
    "1st-99th percentile" = h("3125202f20393925"),
    "5th-95th percentile" = h("3525202f20393525"),
    "Complete-case: row-wise" = h("ed968920eb8ba8ec9c8420ec9984eca084ec82aceba180"),
    "Likelihood-based MAR: available repeated measures" = h("eab080ec9aa9ec82aceba180"),
    "Complete-subject analysis" = h("eb8c80ec8381ec9e9020eb8ba8ec9c8420ec9984eca084ec82aceba180"),
    "Multiple imputation (MI)" = h("eb8ba4eca491eb8c80ecb2b4"),
    "Inverse probability weighting (IPW)" = h("495057"),
    "Weighted GEE (WGEE)" = h("eab080eca49120474545"),
    "Observed outcome only" = h("eab480ecb8a1eab09220ec82acec9aa9"),
    "Exclude outcome from imputation model" = h("4d49ec9790ec849c20eca285ec868debb380ec889820eca09cec99b8"),
    "Include outcome in imputation model" = h("4d49ec979020eca285ec868debb380ec889820ed8faced95a8"),
    "Use rows with observed dependent variable (recommended)" = h("eab480ecb8a1eab09220ec82acec9aa9"),
    "Impute missing dependent variable for sensitivity analysis" = h("4d49ec979020eca285ec868debb380ec889820ed8faced95a8")
  )
  translated <- choices
  names(translated) <- vapply(names(choices), function(name) {
    key <- tolower(gsub("[^A-Za-z0-9]+", "_", as.character(name %||% "")))
    key <- gsub("^_+|_+$", "", key)
    value <- statedu_t(paste0("longitudinal.choice.", key), language, fallback = "")
    if (nzchar(value)) {
      value
    } else if (identical(language, "ko") && name %in% names(labels)) {
      labels[[name]]
    } else {
      name
    }
  }, character(1))
  translated
}

longitudinal_check_label <- function(label, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  key <- tolower(gsub("[^A-Za-z0-9]+", "_", as.character(label %||% "")))
  key <- gsub("^_+|_+$", "", key)
  translated <- statedu_t(paste0("longitudinal.check.", key), language, fallback = "")
  if (nzchar(translated)) {
    return(translated)
  }
  if (!identical(language, "ko")) {
    return(label)
  }
  h <- statedu_utf8
  labels <- c(
    "Outcome family / link" = h("eab2b0eab3bcebb380ec889820ebb684ed8fac2feba781ed81ac"),
    "Working correlation" = h("ec9e91ec978520ec8381eab480eab5aceca1b0"),
    "Within-cluster correlation" = h("eab5b0eca79120eb82b420ec8381eab480"),
    "Overdispersion" = h("eab3bcec82b0ed8fac"),
    "Convergence / singular fit" = h("ec8898eba0b42fed8ab9ec9db420eca081ed95a9"),
    "Random-effects structure" = h("ebacb4ec9e91ec9c84ed9aa8eab3bc20eab5aceca1b0"),
    "Random-effect normality" = h("ebacb4ec9e91ec9c84ed9aa8eab3bc20eca095eab79cec84b1"),
    "Residual normality" = h("ec9e94ecb0a820eca095eab79cec84b1"),
    "Residual variance" = h("ec9e94ecb0a820ebb684ec82b0"),
    "Within-subject serial correlation" = h("eb8c80ec8381ec9e9020eb82b420ec9e90eab8b0ec8381eab480"),
    "Within-subject correlation" = h("eb8c80ec8381ec9e9020eb82b420ec8381eab480"),
    "Strict exogeneity / confounding" = h("ec9784eab2a920ec99b8ec839dec84b12feab590eb9e80"),
    "Heteroskedasticity" = h("ec9db4ebb684ec82b0ec84b1"),
    "Serial correlation" = h("ec9e90eab8b0ec8381eab480"),
    "Cross-sectional dependence" = h("ed9aa1eb8ba8eba9b420ec9d98eca1b4ec84b1"),
    "Hausman FE vs RE" = h("486175736d616e20eab3a0eca095ed9aa8eab3bc20767320ebacb4ec9e91ec9c84ed9aa8eab3bc"),
    "Residual dependence" = h("ec9e94ecb0a820ec9d98eca1b4ec84b1"),
    "Design review" = h("ec97b0eab5acec84a4eab38420eab280ed86a0")
  )
  if (label %in% names(labels)) labels[[label]] else label
}

longitudinal_weight_type_detail_ui <- function(weight_type, model_type = NULL, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  h <- statedu_utf8
  weight_type <- longitudinal_resolve_weight_type(weight_type)
  model_type <- as.character(model_type %||% "")[[1]]
  if (model_type %in% c("lmm", "glmm")) {
    translated <- statedu_t("longitudinal.detail.weight_type.lmm_glmm", language, fallback = "")
    if (nzchar(translated)) {
      return(translated)
    }
    return(h("ebb684ec849d20eab080eca491ecb998eba5bc20eca081ec9aa9ed9598eca78020ec958aec8ab5eb8b88eb8ba42e204c4d4d2f474c4d4dec9d9820eab080eca49120ec9ab0eb8f84eb8a9420ed919cebb3b8ec84a4eab3842c20ebaaa9ed919c20ecb694eca095eb9f892c20ec868ced9484ed8ab8ec9ba8ec96b4ebb38420ec9ab0eb8f8420eca095ec9d98ec979020eb94b0eb9dbc20ed95b4ec849dec9db420ed81aceab28c20eb8baceb9dbceca780ebaf80eba19c20ec9db420ebaaa8eb9388ec9d9820eab8b0ebb3b820ebb684ec849dec9cbceba19c20eab68cec9ea5ed9598eca78020ec958aec8ab5eb8b88eb8ba42e20eab080eca49120eca3bcebb38020eca285eb8ba820ecb694eba1a0ec9db420ed9584ec9a94ed9598eba9b420474545eba5bc20ec82acec9aa9ed9598eab1b0eb82982c204c4d4d2f474c4d4d20eab080eca49120eab2b0eab3bceb8a9420ebb384eb8f8420ec84a4eab38420eab7bceab1b0eab08020ec9e88eb8a9420ebafbceab090eb8f8420ebb684ec849dec9cbceba19c20ebb3b4eab3a0ed9598ec84b8ec9a942e"))
  }
  translated <- statedu_t(paste0("longitudinal.detail.weight_type.", weight_type), language, fallback = "")
  if (nzchar(translated)) {
    return(translated)
  }
  if (!identical(language, "ko")) {
    return(longitudinal_weight_type_detail(weight_type, model_type))
  }
  switch(
    weight_type,
    none = h("ebb684ec849d20eab080eca491ecb998eba5bc20eca081ec9aa9ed9598eca78020ec958aec8ab5eb8b88eb8ba42e20ec97b0eab5acec84a4eab384ec838120ed919cebb3b82feca285eb8ba820eab080eca491ecb998eab08020ed9584ec9a94ed9598eca78020ec958aec9cbceba9b420eab8b0ebb3b820ec84a0ed839dec9e85eb8b88eb8ba42e"),
    sampling = h("ec84a0ed839ded959c20ebb380ec8898eba5bc20eab8b0eca08020ed919cebb3b82fec84a4eab38420eab080eca491ecb998eba19c20ec82acec9aa9ed95a9eb8b88eb8ba42e20ed919cebb3b8ecb694ecb69c20ed9995eba5a0ec9db420eab099eca78020ec958aeab1b0eb829820eca1b0ec82ac2fec84a4eab38420eab080eca491ec9db420ed9584ec9a94ed959c20ec9e90eba38cec979020ec82acec9aa9ed95a9eb8b88eb8ba42e"),
    longitudinal = h("ec84a0ed839ded959c20ebb380ec8898eba5bc20ec8b9ceab084ebb380ed999420eca285eb8ba8ebb684ec849d20eab080eca491ecb998eba19c20ec82acec9aa9ed95a9eb8b88eb8ba42e20ebb0a9ebacb820ec8b9ceca090ec9db4eb829820eab09cec9db82dec8b9ceca090ebb38420eab080eca491ecb998eab08020eb8baceb9dbceca780eb8a9420474545ec9790ec849c20eca3bceba19c20ec82acec9aa9ed9598eb8a9420eab080eca49120ec98b5ec8598ec9e85eb8b88eb8ba42e"),
    combined = h("ec84a0ed839ded959c20ebb684ec849d20eab080eca491ecb998ec979020eab2b0ecb8a12fed8388eb9dbd20ebafbceab090eb8f8420ebb684ec849dec9d8420ec9c84ed95b420ec839dec84b1ed959c20ec97aded9995eba5a020eab080eca491ecb998eba5bc20eab3b1ed95a9eb8b88eb8ba42e20ed919cebb3b82fec84a4eab38420eab080eca491ecb998ec998020eab480ecb0b0eab3bceca09520eab080eca491ecb998eab08020ebaaa8eb919020ed9584ec9a94ed95a020eb958c20ec82acec9aa9ed95a9eb8b88eb8ba42e"),
    h("eab080eca491ecb998eba5bc20eca081ec9aa9ed9598eca78020ec958aec8ab5eb8b88eb8ba42e")
  )
}

longitudinal_weight_trim_detail_ui <- function(trim, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  h <- statedu_utf8
  trim <- longitudinal_resolve_weight_trim(trim)
  translated <- statedu_t(paste0("longitudinal.detail.weight_trim.", trim), language, fallback = "")
  if (nzchar(translated)) {
    return(translated)
  }
  if (!identical(language, "ko")) {
    return(longitudinal_weight_trim_detail(trim))
  }
  switch(
    trim,
    p01_99 = h("ecb59ceca28520eab080eca491ecb998ec9d982031ebb0b1ebb684ec9c84ec889820ebafb8eba78ceab3bc203939ebb0b1ebb684ec9c84ec889820ecb488eab3bc20eab092ec9d8420ec9c88eca080ed9994ed95a9eb8b88eb8ba42e20eab084ed9790eca081ec9db820eab7b9eb8ba820eab080eca491ecb998ec979020eb8c80ed959c20ec9984eba78ced959c20ec9588eca095ed999420ec98b5ec8598ec9e85eb8b88eb8ba42e"),
    p05_95 = h("ecb59ceca28520eab080eca491ecb998ec9d982035ebb0b1ebb684ec9c84ec889820ebafb8eba78ceab3bc203935ebb0b1ebb684ec9c84ec889820ecb488eab3bc20eab092ec9d8420ec9c88eca080ed9994ed95a9eb8b88eb8ba42e20eb8d9420eab095ed959c20ecb298eba6acec9db4ebaf80eba19c20ebaaa9ed919c20eab080eca49120ecb694eca095eb9f89ec9d8420ebb094eabf8020ec889820ec9e88eb8a9420ebafbceab090eb8f8420ec84a0ed839dec9cbceba19c20ebb3b4eab3a0ed95b4ec95bc20ed95a9eb8b88eb8ba42e"),
    h("eca088eb8ba8ec9d8420eca081ec9aa9ed9598eca78020ec958aec8ab5eb8b88eb8ba42e20eab080eca491ecb998eab08020ec9588eca095eca081ec9db4eab1b0eb829820ec9b90eb9e9820ec84a4eab38420eab080eca491ecb998eba5bc20ebb3b4eca1b4ed9598eb8a9420eab283ec9db420ebb684ec82b020ec9588eca095ed9994ebb3b4eb8ba420eca491ec9a94ed95a020eb958c20ec82acec9aa9ed95a9eb8b88eb8ba42e")
  )
}

longitudinal_missing_strategy_detail_ui <- function(strategy, model_type = NULL, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  h <- statedu_utf8
  strategy <- longitudinal_resolve_missing_strategy(strategy, model_type)
  model_type <- as.character(model_type %||% "")[[1]]
  if (identical(strategy, "available")) {
    if (model_type %in% c("lmm", "glmm")) {
      translated <- statedu_t("longitudinal.detail.missing_strategy.available_lmm_glmm", language, fallback = "")
      if (nzchar(translated)) {
        return(translated)
      }
      return(h("ebb688eab7a0ed9895204c4d4d2f474c4d4d20ec9ab0eb8f84eb8a9420ec84a0ed839ded959c20ebaaa8ed989520ebb380ec8898eb93a4ec9db420ec9984eca084ed959c20eab480ecb8a120eab8b0eba19dec9d8420ec82acec9aa9ed95b420eca081ed95a9ed95a9eb8b88eb8ba42e20eb8ba4eba5b820ebb0a9ebacb8ec9790ec849c20eab2b0eab3bcebb380ec8898eab08020eab2b0ecb8a1ec9db4eb9dbceb8a9420ec9db4ec9ca0eba78cec9cbceba19c20eb8c80ec8381ec9e9020eca084ecb2b4eba5bc20eca09ceab1b0ed9598eca780eb8a9420ec958aeca780eba78c2c20eca081ed95a920ebaaa8ed9895ec9d9820eab2b0eab3bcebb380ec88982c20eab3b5ebb380eb9f892c2049442c20ec8b9ceab08420ebb380ec8898ec979020eab2b0ecb8a1ec9db420ec9e88eb8a9420ed9689ec9d8020eb8c80ecb2b4ed9598eca78020ec958aec8ab5eb8b88eb8ba42e204d415220eab080eca095ec9db420ebb0a9ec96b420eab080eb8aa5ed95a020eb958c20eab8b0ebb3b820ed98bced95a9ebaaa8ed989520ebb684ec849dec9cbceba19c20eca081eca088ed9598eba9b02c20eab3b5ebb380eb9f8920eab2b0ecb8a1ec9db4eb829820ed8388eb9dbd20eab8b0eca09ceab08020eca491ec9a94ed9598eba9b4204d4920eb9890eb8a942049505720ebafbceab090eb8f8420ebb684ec849dec9d8420ecb694eab080ed9598ec84b8ec9a942e"))
    }
    if (identical(model_type, "gee")) {
      translated <- statedu_t("longitudinal.detail.missing_strategy.available_gee", language, fallback = "")
      if (nzchar(translated)) {
        return(translated)
      }
      return(h("ec9dbcebb09820474545eb8a9420ec9ab0eb8f8420eab8b0ebb09820ebaaa8ed9895ec9db420ec9584eb8b99eb8b88eb8ba42e20ec82acec9aa920eab080eb8aa5ed959c20ebaaa8ed989520ed9689ec9d8020eca081ed95a9ed95a020ec889820ec9e88eca780eba78c2c204d415220ed8388eb9dbdec9d80204d492c2049505720eb9890eb8a94205747454520ebafbceab090eb8f8420ebb684ec849dec9cbceba19c20ebb3b4ec9984ed95b4ec95bc20ed95a9eb8b88eb8ba42e"))
    }
    translated <- statedu_t("longitudinal.detail.missing_strategy.available_other", language, fallback = "")
    if (nzchar(translated)) {
      return(translated)
    }
    return(h("eab480ecb8a1eb909c20ec84a0ed839d20ebaaa8ed989520ebb380ec889820ed9689ec9d8420ec82acec9aa9ed95a9eb8b88eb8ba42e20ed8ca8eb849020eab7a0ed989520ec97acebb680eba5bc20ebb3b4eab3a0ed9598eab3a02c20eab2b0ecb8a1ec9db420eab480ecb8a120ec9db4eba0a5ec979020ec9d98eca1b4ed95a020eab080eb8aa5ec84b1ec9db420ec9e88ec9cbceba9b4204d4920eb9890eb8a9420eab080eca49120ebafbceab090eb8f8420ebb684ec849dec9d8420ecb694eab080ed9598ec84b8ec9a942e"))
  }
  translated <- statedu_t(paste0("longitudinal.detail.missing_strategy.", strategy), language, fallback = "")
  if (nzchar(translated)) {
    return(translated)
  }
  if (!identical(language, "ko")) {
    return(longitudinal_missing_strategy_detail(strategy, model_type))
  }
  switch(
    strategy,
    subject_complete = h("ec84a0ed839ded959c20ebb684ec849d20ebb380ec8898ec979020eb8c80ed95b420ec9984eca084ed959c20eab8b0eba19dec9d8420eab080eca78420eb8c80ec8381ec9e902feab5b0eca791eba78c20ec9ca0eca780ed95a9eb8b88eb8ba42e20ebb3b4ec8898eca081ec9db4eab3a020ebb3b4eab3a0ed9598eab8b020ec89bdeca780eba78c2c20ed8388eb9dbdec9db420eab480ecb8a120eab2b0eab3bceb829820eab3b5ebb380eb9f89eab3bc20eab480eba0a8eb9098eba9b420eab280eca095eba0a5ec9db420eab090ec868ced9598eab3a020ed8eb8ed96a5ec9db420ec839deab8b820ec889820ec9e88ec8ab5eb8b88eb8ba42e"),
    mi = h("4d493a206d69636520eab8b0ebb09820eab2b0ecb8a1ec9e90eba38c20ebafbceab090eb8f8420ebb684ec849dec9e85eb8b88eb8ba42e20eab2b0ecb8a1eb909c20ec84a0ed839d20ebaaa8ed989520ebb380ec8898eba5bc20eb8c80ecb2b4ed9598eab3a02c20ec84a0ed839dec979020eb94b0eb9dbc20eab2b0ecb8a120eab2b0eab3bcebb380ec8898eb8f8420eb8c80ecb2b4ed959c20eb92a420eab08120eb8c80ecb2b420ec9e90eba38cec9790ec849c20ebaaa8ed9895ec9d8420eca081ed95a9ed9598eab3a020527562696e20eab79cecb999ec9cbceba19c20ecb694eca095ecb998eba5bc20eab2b0ed95a9ed95a9eb8b88eb8ba42e20eca084ec9aa920eb8ba4ec8898eca480204d4920ec9794eca784ec9d8020ec9584eb8b88ebaf80eba19c204c4d4d2f474c4d4dec9790ec849ceb8a9420ebb3b4ed86b520ebafbceab090eb8f8420ebb684ec849dec9cbceba19c20ebb3b4eab3a0ed95a9eb8b88eb8ba42e"),
    ipw = h("4950573a20eab480ecb8a120ec9888ecb8a1ebb380ec8898eba19c20ec9984eca084eab480ecb8a120ed9995eba5a0ec9d8420ecb694eca095ed9598eab3a020ec97aded9995eba5a020eab080eca491ecb998eba19c20ec84a0ed839d20ebaaa8ed9895ec9d8420eb8ba4ec8b9c20eca081ed95a9ed9598eb8a9420ebafbceab090eb8f8420ebb684ec849dec9e85eb8b88eb8ba42e20ec9691ec84b1ec84b1eab3bc20eab080eca491ecb99820ec9588eca095ec84b1ec9d8420eab280ed86a0ed95b4ec95bc20ed95a9eb8b88eb8ba42e"),
    wgee = h("574745453a204d415220ed8388eb9dbd20ebafbceab090eb8f8420ebb684ec849dec9d8420ec9c84ed95b420ec97aded9995eba5a020eab480ecb0b020eab080eca491ecb998eba19c20474545eba5bc20eb8ba4ec8b9c20eca081ed95a9ed9598eb8a942047454520eca084ec9aa920ebafbceab090eb8f8420ebb684ec849dec9e85eb8b88eb8ba42e20ec9691ec84b1ec84b1eab3bc20eab080eca491ecb99820eab5acec84b120ebb0a9ec8b9dec9d8420ebb3b4eab3a0ed95b4ec95bc20ed95a9eb8b88eb8ba42e"),
    h("ebaaa8ed989520eca081ed95a920eca084ec979020ec84a0ed839ded959c20ebb684ec849d20ebb380ec8898ec979020eab2b0ecb8a1ec9db420ec9e88eb8a9420ed9689ec9d8420eca09ceab1b0ed95a9eb8b88eb8ba42e20ed88acebaa85ed959c20ebb0a9ebb295ec9db4eca780eba78c2c20eab2b0ecb8a1ec9db4204d434152ec9dbc20eab080eb8aa5ec84b1ec9db420eb8692eab1b0eb829820eab2b0ecb8a1ec9db420eba7a4ec9ab020eca081ec9d8420eb958c20eab080ec9ea520ed8380eb8bb9ed95a9eb8b88eb8ba42e")
  )
}

longitudinal_weight_variable_choices <- function(weight, available, variable_table = NULL, labels = character(0)) {
  candidates <- unique(c(as.character(weight %||% character(0)), as.character(available %||% character(0))))
  candidates <- analysis_allowed_variables(candidates, variable_table, "continuous")
  if (length(candidates) > 1) {
    weight_score <- function(name) {
      label <- ""
      if (length(labels) > 0 && name %in% names(labels)) {
        label <- labels[[name]] %||% ""
      }
      if (is.data.frame(variable_table) && all(c("name", "var_label") %in% names(variable_table))) {
        table_label <- variable_table$var_label[match(name, variable_table$name)]
        if (length(table_label) == 1 && !is.na(table_label)) {
          label <- table_label
        }
      }
      text <- tolower(paste(name, label))
      score <- 0L
      if (grepl("(^|[_ .-])(weight|weights|wt|wgt|ipw|iptw|sampling|survey|longitudinal|panel)([_ .-]|$)", text)) score <- score + 8L
      if (grepl("weight|weights", text)) score <- score + 6L
      if (grepl("ipw|iptw|inverse probability|probability weight", text)) score <- score + 5L
      if (grepl("sampling|survey|design|longitudinal|panel|baseline|time.varying|time varying", text)) score <- score + 3L
      if (grepl("score|scale|total|sum|outcome|quality|age|time|id|subject", text)) score <- score - 2L
      score
    }
    scores <- vapply(candidates, weight_score, integer(1))
    selected_weight <- intersect(as.character(weight %||% character(0)), candidates)
    likely <- candidates[scores > 0]
    likely <- setdiff(likely[order(-scores[match(likely, candidates)], match(likely, candidates))], selected_weight)
    remaining <- setdiff(candidates, c(selected_weight, likely))
    candidates <- c(selected_weight, likely, remaining)
  }
  items <- analysis_variable_items(candidates, variable_table, labels)
  choices <- vapply(items, `[[`, character(1), "value")
  names(choices) <- vapply(items, `[[`, character(1), "label")
  c("No weight variable" = "", choices)
}

longitudinal_target_field <- function(
  input_id,
  title,
  items,
  selected,
  size,
  allowed_measurements = NULL,
  move_up_id = NULL,
  move_down_id = NULL,
  field_class = "",
  language = statedu_initial_language()
) {
  div(
    class = paste(
      "longitudinal-target-field",
      field_class
    ),
    div(
      class = "longitudinal-field-body",
      analysis_field_label_tag(longitudinal_ui_text(title, language), allowed_measurements),
      analysis_transfer_listbox_input(input_id, items = items, selected = selected, size = size),
      if (!is.null(move_up_id) && !is.null(move_down_id)) {
        div(
          class = "hierarchical-order-actions longitudinal-order-actions",
          actionButton(move_up_id, longitudinal_ui_text("Up", language), class = "btn-default btn-sm"),
          actionButton(move_down_id, longitudinal_ui_text("Down", language), class = "btn-default btn-sm")
        )
      }
    )
  )
}

longitudinal_move_button <- function(input_id, disabled = FALSE) {
  actionButton(
    input_id,
    ">",
    class = "btn btn-default analysis-move-button",
    disabled = if (isTRUE(disabled)) "disabled" else NULL
  )
}

longitudinal_target_block <- function(title, ..., block_class = "", language = statedu_initial_language()) {
  div(
    class = paste("analysis-transfer-column analysis-transfer-panel longitudinal-target-block", block_class),
    div(class = "analysis-option-title longitudinal-block-title", longitudinal_ui_text(title, language)),
    div(class = "longitudinal-target-fields", ...)
  )
}

longitudinal_check_catalog <- function(model_type) {
  rows <- switch(
    as.character(model_type %||% "gee")[[1]],
    gee = list(
      c("family", "Outcome family / link"),
      c("working_correlation", "Working correlation"),
      c("serial_correlation", "Within-cluster correlation"),
      c("overdispersion", "Overdispersion")
    ),
    lmm = list(
      c("mixed_convergence", "Convergence / singular fit"),
      c("random_effects", "Random-effects structure"),
      c("random_effect_normality", "Random-effect normality"),
      c("residual_normality", "Residual normality"),
      c("heteroskedasticity", "Residual variance"),
      c("serial_correlation", "Within-subject serial correlation")
    ),
    glmm = list(
      c("family", "Outcome family / link"),
      c("mixed_convergence", "Convergence / singular fit"),
      c("random_effects", "Random-effects structure"),
      c("serial_correlation", "Within-subject correlation"),
      c("overdispersion", "Overdispersion")
    ),
    panel_fe = list(
      c("exogeneity", "Strict exogeneity / confounding"),
      c("heteroskedasticity", "Heteroskedasticity"),
      c("serial_correlation", "Serial correlation"),
      c("cross_section", "Cross-sectional dependence"),
      c("hausman", "Hausman FE vs RE")
    ),
    panel_re = list(
      c("exogeneity", "Strict exogeneity / confounding"),
      c("hausman", "Hausman FE vs RE"),
      c("heteroskedasticity", "Heteroskedasticity"),
      c("serial_correlation", "Serial correlation"),
      c("cross_section", "Cross-sectional dependence")
    ),
    list(
      c("serial_correlation", "Residual dependence"),
      c("exogeneity", "Design review")
    )
  )
  data.frame(
    key = vapply(rows, function(row) row[[1]], character(1)),
    label = vapply(rows, function(row) row[[2]], character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_all_check_keys <- function() {
  unique(unlist(lapply(c("gee", "lmm", "glmm", "panel_fe", "panel_re"), function(model_type) {
    longitudinal_check_catalog(model_type)$key
  }), use.names = FALSE))
}

longitudinal_all_check_input_ids <- function() {
  paste0("longitudinal_check_", longitudinal_all_check_keys())
}

longitudinal_resolve_check_options <- function(model_type, check_options = list()) {
  catalog <- longitudinal_check_catalog(model_type)
  values <- stats::setNames(rep(TRUE, nrow(catalog)), catalog$key)
  if (is.null(check_options)) {
    return(values)
  }
  provided <- check_options
  if (!is.list(provided)) {
    provided <- as.list(provided)
  }
  provided_names <- names(provided)
  if (is.null(provided_names)) {
    return(values)
  }
  for (key in intersect(catalog$key, provided_names)) {
    values[[key]] <- isTRUE(provided[[key]])
  }
  values
}

longitudinal_checks_tab_content <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  catalog <- longitudinal_check_catalog(state$model_type)
  values <- longitudinal_resolve_check_options(state$model_type, state$check_options)
  div(
    class = "factor-options-tab-content longitudinal-options-tab-content longitudinal-checks-options",
    analysis_option_group(
      longitudinal_ui_text("Assumption review", language),
      list(
        list(id = "longitudinal_assumption_checks", label = longitudinal_ui_text("Run assumption checks and recommendations", language), value = isTRUE(state$assumption_checks))
      )
    ),
    div(class = "analysis-option-subtitle", longitudinal_ui_text("Checks for selected model", language)),
    div(
      class = paste("longitudinal-check-option-list", if (!isTRUE(state$assumption_checks)) "longitudinal-check-options-muted" else ""),
      lapply(seq_len(nrow(catalog)), function(index) {
        key <- catalog$key[[index]]
        checkboxInput(
          paste0("longitudinal_check_", key),
          longitudinal_check_label(catalog$label[[index]], language),
          value = isTRUE(values[[key]])
        )
      })
    )
  )
}

longitudinal_disabled_select_input <- function(input_id, label, choices, selected) {
  htmltools::tagQuery(
    selectInput(input_id, label, choices = choices, selected = selected, selectize = FALSE)
  )$find("select")$addAttrs(disabled = "disabled")$allTags()
}

longitudinal_weights_tab_content <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  weights_disabled <- state$model_type %in% c("lmm", "glmm")
  has_active_weight_type <- !identical(state$weight_type, "none") && !isTRUE(weights_disabled)
  div(
    class = paste(
      "factor-options-tab-content longitudinal-options-tab-content longitudinal-weight-options-tab-content",
      if (isTRUE(weights_disabled)) "longitudinal-weight-options-disabled" else ""
    ),
    div(
      class = "analysis-option-group",
      if (isTRUE(weights_disabled)) {
        div(
          class = "longitudinal-disabled-notice",
          tags$strong(if (identical(language, "ko")) statedu_utf8("ec9dbcecb0a8204c4d4d202f20474c4d4dec9790ec849ceb8a9420eab080eca491ecb998eab08020ebb984ed999cec84b1ed9994eb90a9eb8b88eb8ba42e") else "Weights are disabled for primary LMM / GLMM."),
          tags$p(if (identical(language, "ko")) statedu_utf8("eab080eca49120ed98bced95a9ebaaa8ed989520ec9ab0eb8f84eb8a9420ec9db420ebaaa8eb9388ec9d9820eab8b0ebb3b820ebb684ec849dec9cbceba19c20eab68cec9ea5ed9598eca78020ec958aec8ab5eb8b88eb8ba42e20ec97b0eab5aceca788ebacb8ec979020eba79eeb8a9420eab080eca49120eca3bcebb38020eca285eb8ba820ecb694eba1a0ec9790eb8a9420474545eba5bc20ec82acec9aa9ed9598ec84b8ec9a942e") else "Weighted mixed-model likelihood is not recommended as a routine default in this module. Use GEE for weighted marginal longitudinal inference when it matches the research question.")
        )
      },
      div(
        class = "regression-field",
        if (isTRUE(weights_disabled)) {
          longitudinal_disabled_select_input(
            "longitudinal_weight_choice",
            label = tagList(longitudinal_ui_text("Weight variable", language), span(class = "analysis-allowed-measurements", measurement_symbol_tag("continuous"))),
            choices = longitudinal_ui_choices(state$weight_choices, language),
            selected = if (length(state$weight) == 1) state$weight else ""
          )
        } else {
          selectInput(
            "longitudinal_weight_choice",
            label = tagList(longitudinal_ui_text("Weight variable", language), span(class = "analysis-allowed-measurements", measurement_symbol_tag("continuous"))),
            choices = longitudinal_ui_choices(state$weight_choices, language),
            selected = if (length(state$weight) == 1) state$weight else "",
            selectize = FALSE
          )
        }
      ),
      div(
        class = "regression-field",
        if (isTRUE(weights_disabled)) {
          longitudinal_disabled_select_input(
            "longitudinal_weight_type",
            longitudinal_ui_text("Weight type", language),
            choices = longitudinal_ui_choices(longitudinal_weight_type_choices(state$model_type, length(state$weight) == 1), language),
            selected = "none"
          )
        } else {
          selectInput(
            "longitudinal_weight_type",
            longitudinal_ui_text("Weight type", language),
            choices = longitudinal_ui_choices(longitudinal_weight_type_choices(state$model_type, length(state$weight) == 1), language),
            selected = state$weight_type,
            selectize = FALSE
          )
        }
      ),
      if (!isTRUE(weights_disabled)) {
        div(class = "longitudinal-option-help longitudinal-weight-type-help", longitudinal_weight_type_detail_ui(state$weight_type, state$model_type, language))
      },
      if (isTRUE(has_active_weight_type)) {
        tagList(
          div(
            class = "regression-field",
            selectInput(
              "longitudinal_weight_trim",
              longitudinal_ui_text("Trim extreme weights", language),
              choices = longitudinal_ui_choices(longitudinal_weight_trim_choices(), language),
              selected = state$weight_trim,
              selectize = FALSE
            )
          ),
          div(class = "longitudinal-option-help longitudinal-weight-trim-help", longitudinal_weight_trim_detail_ui(state$weight_trim, language))
        )
      },
      if (!isTRUE(weights_disabled)) {
        div(
          class = "longitudinal-missing-detail",
          if (length(state$weight) == 0) {
            if (identical(language, "ko")) statedu_utf8("eab080eca491ecb99820ebb380ec8898eab08020ec84a0ed839deb9098eca78020ec958aec9598ec8ab5eb8b88eb8ba42e20eab080eca491ecb99820ec9786ec9db420ebaaa8ed9895ec9d8420ec8ba4ed9689ed95a9eb8b88eb8ba42e") else "No weight variable is selected; the model will run without analysis weights."
          } else if (identical(state$weight_type, "none")) {
            if (identical(language, "ko")) statedu_utf8("eab080eca491ecb99820ebb380ec8898eab08020ec84a0ed839deb9098ec9788eca780eba78c20eab080eca491ecb99820ec9ca0ed9895ec9db420ec9786ec9d8cec9cbceba19c20eb9098ec96b420ec9e88ec8ab5eb8b88eb8ba42e20ec9dbcecb0a820ebaaa8ed9895ec9d8020ebb984eab080eca491ec9cbceba19c20ec8ba4ed9689eb90a9eb8b88eb8ba42e") else "A weight variable is selected, but the selected weight type is No weights; the primary model will be unweighted."
          } else if (identical(state$model_type, "gee")) {
            if (identical(language, "ko")) statedu_utf8("474545eb8a9420ec84a0ed839ded959c20eab080eca491ecb998eba5bc20eca781eca09120ec82acec9aa9ed95a9eb8b88eb8ba42e20ebaaa9ed919c20ebaaa8eca791eb8ba8eab3bc20eab080eca491ecb99820eab5acec84b120ebb0a9ec8b9dec9d8420ebaa85ec8b9ceca081ec9cbceba19c20ed95b4ec849ded9598ec84b8ec9a942e") else "GEE uses the selected weight directly; interpret the target population and weight construction explicitly."
          } else if (state$model_type %in% c("panel_fe", "panel_re")) {
            if (identical(language, "ko")) statedu_utf8("ed8ca8eb849020ebaaa8ed9895ec9d8020eab080eca49120ed8ca8eb849020ecb694eca095ec9d8420ec82acec9aa9ed95a9eb8b88eb8ba42e20ebaaa9ed919c20ebaaa8eca791eb8ba8eab3bc20eab080eca491ecb99820eab5acec84b120ebb0a9ec8b9dec9d8420ebaa85ec8b9ceca081ec9cbceba19c20ed95b4ec849ded9598ec84b8ec9a942e") else "Panel models use weighted panel estimation; interpret the target population and weight construction explicitly."
          } else {
            if (identical(language, "ko")) statedu_utf8("ebaaa9ed919c20ebaaa8eca791eb8ba8eab3bc20eab080eca491ecb99820eab5acec84b120ebb0a9ec8b9dec9d8420ebaa85ec8b9ceca081ec9cbceba19c20ed95b4ec849ded9598ec84b8ec9a942e") else "Interpret the target population and weight construction explicitly."
          }
        )
      }
    )
  )
}

longitudinal_setup_panel <- function(state, status_message = NULL) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  if (length(state$selected) == 0) {
    return(setup_empty_message("Complete Step 2 in the Data tab before setting up longitudinal / panel models.", language = language))
  }
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "longitudinal-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel longitudinal-available-panel",
        analysis_field_label_tag("Variables", language = language),
        analysis_transfer_listbox_input(
          "longitudinal_available",
          items = state$available_items,
          selected = state$available_selected,
          size = 17
        )
      ),
      div(
        class = "analysis-transfer-controls longitudinal-transfer-controls longitudinal-panel-transfer-controls",
        longitudinal_move_button("longitudinal_id_move", state$move_disabled && length(state$id) == 0),
        longitudinal_move_button("longitudinal_cluster_move", state$move_disabled && length(state$cluster) == 0),
        longitudinal_move_button("longitudinal_time_move", state$move_disabled && length(state$time) == 0),
        longitudinal_move_button("longitudinal_exposure_move", state$move_disabled && length(state$exposure) == 0)
      ),
      longitudinal_target_block(
        "Panel structure",
        language = language,
        block_class = "longitudinal-core-block",
        longitudinal_target_field(
          "longitudinal_id",
          title = "Subject ID",
          items = state$id_items,
          selected = state$id_selected,
          size = 1,
          allowed_measurements = analysis_allowed_measurements_all(),
          field_class = "longitudinal-id-field",
          language = language
        ),
        longitudinal_target_field(
          "longitudinal_cluster",
          title = "Cluster ID (optional)",
          items = state$cluster_items,
          selected = state$cluster_selected,
          size = 1,
          allowed_measurements = analysis_allowed_measurements_all(),
          field_class = "longitudinal-cluster-field",
          language = language
        ),
        longitudinal_target_field(
          "longitudinal_time",
          title = "Time variable",
          items = state$time_items,
          selected = state$time_selected,
          size = 1,
          allowed_measurements = c("ordered", "continuous"),
          field_class = "longitudinal-time-field",
          language = language
        ),
        longitudinal_target_field(
          "longitudinal_exposure",
          title = "Exposure / offset (optional)",
          items = state$exposure_items,
          selected = state$exposure_selected,
          size = 1,
          allowed_measurements = c("continuous"),
          field_class = "longitudinal-exposure-field",
          language = language
        )
      ),
      div(
        class = "analysis-transfer-controls longitudinal-transfer-controls longitudinal-model-transfer-controls",
        longitudinal_move_button("longitudinal_outcome_move", state$move_disabled && length(state$outcome) == 0),
        longitudinal_move_button("longitudinal_predictors_move", state$move_disabled && length(state$predictors) == 0)
      ),
      longitudinal_target_block(
        "Model variables",
        language = language,
        block_class = "longitudinal-predictors-block",
        longitudinal_target_field(
          "longitudinal_outcome",
          title = "Dependent variable",
          items = state$outcome_items,
          selected = state$outcome_selected,
          size = 1,
          allowed_measurements = analysis_allowed_measurements_all(),
          field_class = "longitudinal-outcome-field",
          language = language
        ),
        longitudinal_target_field(
          "longitudinal_predictors",
          title = longitudinal_independent_variables_label(length(state$predictors), language),
          items = state$predictor_items,
          selected = state$predictor_selected,
          size = 13,
          allowed_measurements = analysis_allowed_measurements_all(),
          move_up_id = "move_longitudinal_predictors_up",
          move_down_id = "move_longitudinal_predictors_down",
          field_class = "longitudinal-predictors-field",
          language = language
        )
      ),
      analysis_options_tabs_panel(
        id = "longitudinal_options_tab",
        selected = state$options_tab,
        class = "analysis-options-column longitudinal-options",
        tabPanel(
            longitudinal_ui_text("Model", language),
            value = "Model",
            div(
              class = "factor-options-tab-content longitudinal-options-tab-content longitudinal-model-options-tab-content",
              div(
                class = "analysis-option-group",
                div(
                  class = "regression-field",
                  selectInput("longitudinal_model_type", longitudinal_ui_text("Model type", language), choices = longitudinal_ui_choices(longitudinal_model_choices(), language), selected = state$model_type, selectize = FALSE)
                ),
                if (state$model_type %in% c("gee", "glmm")) {
                  div(
                    class = "regression-field",
                    selectInput("longitudinal_family", longitudinal_ui_text("Outcome family", language), choices = longitudinal_ui_choices(longitudinal_family_choices(), language), selected = state$family, selectize = FALSE)
                  )
                },
                if (identical(state$model_type, "gee")) {
                  div(
                    class = "regression-field",
                    selectInput("longitudinal_corstr", longitudinal_ui_text("GEE correlation", language), choices = longitudinal_ui_choices(longitudinal_correlation_choices(), language), selected = state$corstr, selectize = FALSE)
                  )
                }
              ),
              analysis_option_group(
                longitudinal_ui_text("Terms", language),
                list(
                  list(id = "longitudinal_include_time", label = longitudinal_ui_text("Include time as fixed effect", language), value = isTRUE(state$include_time))
                )
              ),
              if (state$model_type %in% c("lmm", "glmm")) {
                analysis_option_group(
                  longitudinal_ui_text("Random effects", language),
                  list(
                    list(id = "longitudinal_random_slope", label = longitudinal_ui_text("Random slope for selected time variable", language), value = isTRUE(state$random_slope))
                  )
                )
              },
              if (state$model_type %in% c("gee", "glmm")) {
                analysis_option_group(
                  longitudinal_ui_text("Reporting", language),
                  list(
                    list(id = "longitudinal_exponentiate", label = longitudinal_ui_text("Report exp(B) for logit / log models", language), value = isTRUE(state$exponentiate))
                  )
                )
              }
            )
          ),
          tabPanel(
            longitudinal_ui_text("Weights", language),
            value = "Weights",
            longitudinal_weights_tab_content(state)
          ),
          tabPanel(
            longitudinal_ui_text("Missing", language),
            value = "Missing",
            div(
              class = "factor-options-tab-content longitudinal-options-tab-content longitudinal-missing-options-tab-content",
              div(
                class = "analysis-option-group",
                div(
                  class = "regression-field",
                  selectInput(
                    "longitudinal_missing_strategy",
                    longitudinal_ui_text("Missing-data strategy", language),
                    choices = longitudinal_ui_choices(longitudinal_missing_strategy_choices(state$model_type), language),
                    selected = state$missing_strategy,
                    selectize = FALSE
                  )
                ),
                div(class = "longitudinal-missing-detail", longitudinal_missing_strategy_detail_ui(state$missing_strategy, state$model_type, language)),
                if (identical(state$missing_strategy, "mi")) {
                  div(
                    class = "longitudinal-mi-settings",
                    div(class = "analysis-option-subtitle", longitudinal_ui_text("Multiple imputation settings", language)),
                    div(
                      class = "regression-field",
                      selectInput(
                        "longitudinal_mi_outcome",
                        longitudinal_ui_text("Dependent-variable handling", language),
                        choices = longitudinal_ui_choices(longitudinal_mi_outcome_choices(), language),
                        selected = state$mi_outcome,
                        selectize = FALSE
                      )
                    ),
                    div(
                      class = "regression-field",
                      numericInput("longitudinal_missing_imputations", longitudinal_ui_text("MI datasets", language), value = state$missing_imputations, min = 2, max = 50, step = 1)
                    ),
                    div(
                      class = "regression-field",
                      numericInput("longitudinal_missing_iterations", longitudinal_ui_text("MI iterations", language), value = state$missing_iterations, min = 1, max = 50, step = 1)
                    )
                  )
                },
                if (state$missing_strategy %in% c("ipw", "wgee")) {
                  div(
                    class = "longitudinal-ipw-settings",
                    div(class = "analysis-option-subtitle", longitudinal_ui_text("IPW observation model", language)),
                    div(
                      class = "regression-field",
                      selectizeInput(
                        "longitudinal_ipw_auxiliary",
                        longitudinal_ui_text("Auxiliary variables", language),
                        choices = state$ipw_auxiliary_choices,
                        selected = state$ipw_auxiliary,
                        multiple = TRUE,
                        options = list(plugins = list("remove_button"))
                      )
                    ),
                    div(
                      class = "longitudinal-missing-detail",
                      if (identical(language, "ko")) statedu_utf8("eab2b0ecb8a12fed8388eb9dbd20eab480ecb0b0ebaaa8ed9895ec9790ec849c20ec9984eca084ed9e8820eab480ecb8a1eb909c20ebb3b4eca1b020ebb380ec8898eba78c20ec82acec9aa9ed95a9eb8b88eb8ba42e20ec9db420ebb380ec8898eb93a4ec9d8020eab3a0eca095ed9aa8eab3bc20ec9888ecb8a1ebb380ec8898eba19c20eb93a4ec96b4eab080eca78020ec958aec8ab5eb8b88eb8ba42e") else "Auxiliary variables are used only in the missingness/dropout observation model when fully observed; they do not become fixed-effect predictors."
                    )
                  )
                }
              )
            )
          ),
          tabPanel(
            longitudinal_ui_text("Checks", language),
            value = "Checks",
            longitudinal_checks_tab_content(state)
          )
      )
    ),
    analysis_three_block_action_row(
      class = "longitudinal-action-row",
      run_button = actionButton("run_longitudinal", longitudinal_ui_text("Run model", language), class = "btn btn-primary", disabled = if (!isTRUE(state$can_run)) "disabled" else NULL),
      reset_control = tags$button(
        id = "reset_longitudinal",
        type = "button",
        class = "btn action-button btn-default analysis-reset-button",
        disabled = if (!isTRUE(state$has_assignment)) "disabled" else NULL,
        longitudinal_ui_text("Reset setting", language)
      ),
      save_control = uiOutput("longitudinal_save_control")
    )
  )
}
