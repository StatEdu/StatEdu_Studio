# Design-based categorical logistic models. Keep captured output on the shared
# table/export path; fitting is never repeated by an export operation.
complex_sample_logistic_choices <- function(key, language) {
  values <- switch(key,
    logistic_model = c("binary", "multinomial", "ordinal"),
    logistic_reference = c("metadata", "first", "last"),
    logistic_order = c("forward", "reverse"))
  stats::setNames(values, vapply(values, function(value) {
    statedu_t(paste0("complex_sample.", key, "_", value), language, fallback = value)
  }, character(1)))
}

complex_sample_logistic_outcome <- function(values, outcome, category_table, options) {
  values[!is.na(values) & !nzchar(trimws(as.character(values)))] <- NA
  observed <- frequency_value_order(unique(as.character(values[!is.na(values)])))
  declared <- names(category_value_label_lookup_static(category_table)[[outcome]] %||% character(0))
  declared <- declared[nzchar(declared)]
  if (!length(declared) && is.factor(values)) declared <- levels(values)
  category_levels <- unique(c(intersect(declared, observed), setdiff(observed, declared)))
  shiny::validate(shiny::need(length(category_levels) >= 3L,
    statedu_t("complex_sample.logistic_error.categories", result_appendix_table_language(), "Multinomial and ordinal logistic regression require at least three observed outcome categories.")))
  if (options$logistic_model == "ordinal") {
    if (options$logistic_order == "reverse") category_levels <- rev(category_levels)
    return(ordered(as.character(values), levels = category_levels))
  }
  reference <- switch(options$logistic_reference,
    first = category_levels[[1]], last = tail(category_levels, 1),
    metadata = trimws(named_value(regression_reference_values_static(category_table), outcome, "")))
  if (!nzchar(reference)) reference <- category_levels[[1]]
  shiny::validate(shiny::need(reference %in% category_levels,
    statedu_t("complex_sample.logistic_error.reference", result_appendix_table_language(), "The selected outcome reference category has no observations in the analysis sample.")))
  stats::relevel(factor(as.character(values), levels = category_levels), ref = reference)
}

complex_sample_categorical_logistic_fit <- function(data, outcome, predictors, input, prefix,
                                                   variable_info = NULL, category_table = NULL) {
  options <- complex_sample_analysis_options(input, prefix, "logistic")
  built <- complex_sample_build_design(data, input, prefix, unique(c(outcome, predictors)))
  design <- built$design
  eligible_n <- nrow(design$variables)
  safe <- stats::setNames(paste0("..x", seq_along(predictors), ".."), predictors)
  design$variables$`..outcome..` <- complex_sample_logistic_outcome(design$variables[[outcome]], outcome, category_table, options)
  original_levels <- levels(design$variables$`..outcome..`)
  for (predictor in predictors) {
    design$variables[[safe[[predictor]]]] <- complex_sample_regression_predictor_values(
      design$variables[[predictor]], predictor, variable_info, category_table)
  }
  missing <- data.frame(Variable = c(outcome, predictors),
    `Missing n` = vapply(c("..outcome..", unname(safe)), function(name) sum(is.na(design$variables[[name]])), integer(1)), check.names = FALSE)
  design$variables$`..model_keep..` <- stats::complete.cases(design$variables[, c("..outcome..", unname(safe)), drop = FALSE])
  outcome_missing <- is.na(design$variables$`..outcome..`)
  predictor_missing <- !stats::complete.cases(design$variables[, unname(safe), drop = FALSE])
  missing_partition <- c(outcome_only = sum(outcome_missing & !predictor_missing),
    predictor_only = sum(!outcome_missing & predictor_missing), both = sum(outcome_missing & predictor_missing))
  shiny::validate(shiny::need(any(design$variables$`..model_keep..`), statedu_t("complex_sample.logistic_error.complete", result_appendix_table_language(), "No complete cases are available for the selected regression variables.")))
  design <- subset(design, `..model_keep..`)
  # Do not silently change the outcome definition or its reference after deleting
  # incomplete rows. An absent outcome category requires the user to revise it.
  counts <- table(design$variables$`..outcome..`)
  shiny::validate(shiny::need(all(counts > 0),
    statedu_t("complex_sample.logistic_error.empty_category", result_appendix_table_language(), "An outcome category has no complete cases. Revise the outcome categories or predictors.")))
  usable <- predictors[vapply(unname(safe), function(name) complex_sample_regression_has_variation(design$variables[[name]]), logical(1))]
  shiny::validate(shiny::need(length(usable) > 0,
    statedu_t("complex_sample.logistic_error.variation", result_appendix_table_language(), "No independent variable has at least two usable values after applying the survey design and subpopulation.")))
  for (name in unname(safe[usable])) {
    if (is.factor(design$variables[[name]])) {
      design$variables[[name]] <- droplevels(design$variables[[name]])
      contrasts(design$variables[[name]]) <- stats::contr.treatment(nlevels(design$variables[[name]]))
    }
  }
  formula <- stats::reformulate(unname(safe[usable]), response = "..outcome..", env = baseenv())
  mm <- stats::model.matrix(formula, design$variables)
  shiny::validate(shiny::need(qr(mm)$rank == ncol(mm),
    statedu_t("complex_sample.logistic_error.rank", result_appendix_table_language(), "The predictor model matrix is rank deficient. Remove redundant predictors.")))
  df <- as.numeric(survey::degf(design))
  shiny::validate(shiny::need(is.finite(df) && df > 0,
    statedu_t("complex_sample.logistic_error.df", result_appendix_table_language(), "Positive survey design degrees of freedom are required for categorical logistic inference.")))
  warnings <- character(0)
  fit <- withCallingHandlers({
    if (options$logistic_model == "multinomial") {
      shiny::validate(shiny::need(requireNamespace("svyVGAM", quietly = TRUE) && requireNamespace("VGAM", quietly = TRUE),
        statedu_t("complex_sample.logistic_error.packages", result_appendix_table_language(), "Install VGAM and svyVGAM to run complex-sample multinomial logistic regression.")))
      result <- svyVGAM::svy_vglm(formula, family = VGAM::multinomial(refLevel = 1), design = design,
        control = VGAM::vglm.control(maxit = 100, epsilon = 1e-8))
      shiny::validate(shiny::need(result$fit@iter < 100,
        statedu_t("complex_sample.logistic_error.convergence", result_appendix_table_language(), "The categorical logistic model did not converge. Estimates are not reported.")))
      result
    } else {
      result <- survey::svyolr(formula, design = design, method = "logistic")
      shiny::validate(shiny::need(is.null(result$convergence) || result$convergence == 0,
        statedu_t("complex_sample.logistic_error.convergence", result_appendix_table_language(), "The categorical logistic model did not converge. Estimates are not reported.")))
      result
    }
  }, warning = function(w) {
    warnings <<- unique(c(warnings, conditionMessage(w)))
    invokeRestart("muffleWarning")
  })
  beta <- stats::coef(fit)
  covariance <- stats::vcov(fit)
  shiny::validate(shiny::need(all(is.finite(beta)) && all(is.finite(covariance)) && all(diag(covariance) > 0),
    statedu_t("complex_sample.logistic_error.covariance", result_appendix_table_language(), "The categorical logistic model has non-finite estimates or an invalid covariance matrix.")))
  se <- sqrt(diag(covariance))
  ci <- cbind(beta - stats::qt(.975, df) * se, beta + stats::qt(.975, df) * se)
  rownames(ci) <- names(beta)
  raw <- data.frame(Term = names(beta), Estimate = as.numeric(beta), SE = as.numeric(se),
    Statistic = as.numeric(beta / se), p = 2 * stats::pt(-abs(beta / se), df), stringsAsFactors = FALSE)
  slope_names <- if (options$logistic_model == "ordinal") names(fit$coefficients) else names(beta)[!grepl("^\\(Intercept\\):", names(beta))]
  list(fit = fit, built = built, design = design, options = options, raw = raw, ci = ci, df = df,
    slope_names = slope_names, covariance = covariance, levels = original_levels,
    predictors = usable, safe = safe, eligible_n = eligible_n,
    excluded_n = eligible_n - nrow(design$variables), missing = missing,
    missing_partition = missing_partition, warnings = warnings)
}

complex_sample_categorical_wald <- function(model) {
  b <- stats::coef(model$fit)[model$slope_names]
  v <- model$covariance[model$slope_names, model$slope_names, drop = FALSE]
  if (!length(b) || qr(v)$rank < length(b)) return(NULL)
  statistic <- as.numeric(crossprod(b, solve(v, b))) / length(b)
  if (!is.finite(statistic) || statistic < 0) return(NULL)
  list(F = statistic, df1 = length(b), df2 = model$df,
    p = stats::pf(statistic, length(b), model$df, lower.tail = FALSE))
}

complex_sample_categorical_logistic_result <- function(data, outcome, predictors, input, prefix,
                                                       variable_info = NULL, labels = character(0), category_table = NULL, language = NULL) {
  model <- complex_sample_categorical_logistic_fit(data, outcome, predictors, input, prefix, variable_info, category_table)
  options <- model$options
  ordinal <- options$logistic_model == "ordinal"
  ui_language <- "en"
  title <- if (ordinal) "Complex-sample ordinal logistic regression" else "Complex-sample multinomial logistic regression"
  outcome_label <- frequency_variable_display_name(outcome, variable_info, labels, category_table)
  category_labels <- frequency_value_display_labels(outcome, model$levels, category_table)
  predictor_labels <- vapply(model$predictors, frequency_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  overview <- data.frame(Item = c("Analysis", "Outcome", "Unweighted N", "Eligible design N", "Complete-case excluded N",
    if (ordinal) "Category order (low to high)" else "Reference category"),
    Value = c(title, outcome_label, nrow(model$design$variables), model$eligible_n, model$excluded_n,
      if (ordinal) paste(category_labels, collapse = " < ") else category_labels[[1]]), check.names = FALSE)
  if (isTRUE(options$show_weighted_n)) overview[nrow(overview) + 1L, ] <- c("Weighted N", complex_sample_weighted_n_text(model$design))
  if (isTRUE(options$show_df)) overview[nrow(overview) + 1L, ] <- c("Design df", complex_sample_num(model$df, 0))
  if (isTRUE(options$show_model_fit)) {
    wald <- complex_sample_categorical_wald(model)
    if (!is.null(wald)) {
      overview[nrow(overview) + 1L, ] <- c("Model Wald/F statistic", complex_sample_num(wald$F, 3))
      if (isTRUE(options$show_df)) overview[nrow(overview) + 1L, ] <- c("Model Wald/F df", paste(wald$df1, wald$df2, sep = ", "))
      overview[nrow(overview) + 1L, ] <- c("Model Wald/F p", complex_sample_p_value(wald$p))
    } else {
      model$warnings <- c(model$warnings, "The joint Wald test is unavailable because the slope covariance matrix is singular.")
    }
  }
  attr(overview, "result_user_cells") <- cbind(c(2L, 6L), 2L)
  overview[nrow(overview) + 1L, ] <- c("Predictors", paste(predictor_labels, collapse = ", "))
  attr(overview, "result_user_cells") <- rbind(attr(overview, "result_user_cells"), c(nrow(overview), 2L))
  # svyVGAM numbers the non-reference equations in outcome-level order.
  equations <- if (ordinal) 1L else seq_len(length(model$levels) - 1L)
  tables <- lapply(equations, function(equation) {
    rows <- if (ordinal) model$raw$Term %in% model$slope_names else endsWith(model$raw$Term, paste0(":", equation))
    raw <- model$raw[rows, , drop = FALSE]
    ci <- model$ci[raw$Term, , drop = FALSE]
    if (!ordinal) raw$Term <- sub(paste0(":", equation, "$"), "", raw$Term)
    rownames(ci) <- raw$Term
    table <- complex_sample_regression_coef_display_table(raw, model$fit, model$predictors, model$safe,
      model$design, logistic = TRUE, variable_info = variable_info, labels = labels, category_table = category_table,
      options = options, inference = list(ci = ci, df = model$df))
    adjusted <- length(model$predictors) > 1L
    names(table)[names(table) == "OR"] <- if (adjusted) "aOR" else "OR"
    names(table)[names(table) == "CI"] <- "95% CI"
    names(table)[names(table) == "Statistic"] <- "Wald F"
    reference <- if (ordinal) paste0("Category order (low to high): ", paste(category_labels, collapse = " < "),
      ". Odds ratios describe higher versus lower cumulative outcome categories; proportional odds are assumed") else
      paste0("Outcome comparison: ", category_labels[[equation + 1L]], " versus ", category_labels[[1]], " (reference)")
    note <- complex_sample_main_note(
      abbreviations = c(if (adjusted) "aOR = adjusted odds ratio" else "OR = odds ratio",
        if (isTRUE(options$show_ci)) "95% CI = 95% confidence interval"),
      estimation = paste0("Survey-weighted ", if (ordinal) "proportional-odds" else "multinomial", " logistic regression. ",
        "Coefficient p-values use F(1, ", model$df, ")", if (isTRUE(options$show_ci)) "; confidence intervals use the same design df" else "",
        if (isTRUE(options$show_model_fit)) ". The model Wald F jointly tests all slopes, excluding intercepts and thresholds" else ""),
      reference = c(reference, paste0("Analysis N = ", nrow(model$design$variables),
        "; complete-case excluded N = ", model$excluded_n),
        paste0(if (adjusted) "Mutually adjusted predictors: " else "Predictor: ", paste(predictor_labels, collapse = ", ")),
        "Categorical predictors use the displayed reference category"))
    list(main = shiny::div(class = "result-section regression-result-panel logistic-result-panel",
      shiny::h3(paste0(title, ": ", outcome_label, if (!ordinal) paste0(" — ", category_labels[[equation + 1L]], " vs ", category_labels[[1]]) else "")),
      coefficient_html_table(complex_sample_table_data(table[, setdiff(names(table), c("B", "SE", "Z", "Wald χ²")), drop = FALSE], "main", "en"), compact = TRUE,
        compact_font_size = 12, compact_width = 72, compact_first_width = 128, compact_min_width = 680,
        note_line = note, table_role = "main", table_language = "en", sheet_orientation = "portrait")),
      appendix = complex_sample_logistic_coefficient_appendix(table,
        paste0(outcome_label, if (!ordinal) paste0(" — ", category_labels[[equation + 1L]], " vs ", category_labels[[1]]) else ""),
        ui_language, df = model$df))
  })
  threshold_section <- NULL
  if (ordinal) {
    raw <- model$raw[!model$raw$Term %in% model$slope_names, , drop = FALSE]
    thresholds <- data.frame(Threshold = paste(head(category_labels, -1), tail(category_labels, -1), sep = " | "),
      B = vapply(raw$Estimate, complex_sample_num, character(1), digits = 3),
      SE = vapply(raw$SE, complex_sample_num, character(1), digits = 3),
      Z = vapply(raw$Estimate / raw$SE, complex_sample_num, character(1), digits = 3),
      `Wald χ²` = vapply((raw$Estimate / raw$SE)^2, complex_sample_num, character(1), digits = 3),
      p = vapply(raw$p, complex_sample_p_value, character(1)), check.names = FALSE)
    thresholds <- complex_sample_logistic_inference_columns(thresholds, model$df)
    attr(thresholds, "result_user_columns") <- "Threshold"
    threshold_section <- analysis_result_table_section(statedu_t("complex_sample.ordinal_thresholds", ui_language),
      complex_sample_table_data(thresholds, "appendix", ui_language),
      class = "result-section regression-result-panel", table_fn = function(x) coefficient_html_table(x,
        table_role = "appendix", table_language = "en", sheet_orientation = "portrait",
        note_line = complex_sample_logistic_coefficient_note(model$df)))
  }
  shiny::tagList(
    analysis_result_table_section(statedu_t("complex_sample.categorical_logistic_overview", ui_language),
      complex_sample_table_data(overview, "appendix", ui_language),
      class = "result-section regression-result-panel", table_fn = model_overview_html_table),
    shiny::tagList(lapply(tables, `[[`, "main")),
    shiny::tagList(lapply(tables, `[[`, "appendix")), threshold_section,
    complex_sample_logistic_reporting_sections(model, outcome, variable_info, labels, category_table, ui_language),
    complex_sample_appendix_diagnostics_section(
      items = c("Survey design", rep("Model warning", length(model$warnings))),
      details = c(complex_sample_design_note(model$built, role = "appendix", language = ui_language), model$warnings), language = ui_language),
    complex_sample_logistic_language_guide(language))
}
