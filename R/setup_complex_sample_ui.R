# Complex-sample analysis setup screens.

complex_sample_ui_text <- function(key, language = statedu_initial_language()) {
  key <- as.character(key %||% "")
  translated <- statedu_t(paste0("complex_sample.", key), language, fallback = "")
  if (nzchar(translated)) {
    return(translated)
  }
  labels <- list(
    menu = c(en = "Complex Samples", ko = "\uBCF5\uD569\uD45C\uBCF8\uBD84\uC11D"),
    frequencies = c(en = "Complex Samples Frequencies / Descriptives", ko = "\uBCF5\uD569\uD45C\uBCF8 \uBE48\uB3C4\uBD84\uC11D / \uAE30\uC220\uD1B5\uACC4\uBD84\uC11D"),
    crosstabs = c(en = "Complex Samples Cross-tabulation", ko = "\uBCF5\uD569\uD45C\uBCF8 \uAD50\uCC28\uBD84\uC11D"),
    ttest_anova = c(en = "Complex Samples t-test / ANOVA", ko = "\uBCF5\uD569\uD45C\uBCF8 t-test / ANOVA"),
    correlation = c(en = "Complex Samples Correlation", ko = "\uBCF5\uD569\uD45C\uBCF8 \uC0C1\uAD00\uBD84\uC11D"),
    regression = c(en = "Complex Samples Regression", ko = "\uBCF5\uD569\uD45C\uBCF8 \uD68C\uADC0\uBD84\uC11D"),
    logistic = c(en = "Complex Samples Logistic Regression", ko = "\uBCF5\uD569\uD45C\uBCF8 \uB85C\uC9C0\uC2A4\uD2F1 \uD68C\uADC0\uBD84\uC11D"),
    custom_model = c(en = "Complex Samples Mediation / Moderation", ko = "\uBCF5\uD569\uD45C\uBCF8 \uB9E4\uAC1C\u00B7\uC870\uC808\uD6A8\uACFC"),
    design_menu = c(en = "Complex Samples Design Variables", ko = "\uBCF5\uD569\uD45C\uBCF8 \uC124\uACC4\uBCC0\uC218"),
    design_subtitle = c(en = "Set the complex-sample design once and reuse it automatically in every complex-sample analysis.", ko = "\uBCF5\uD569\uD45C\uBCF8 \uC124\uACC4\uBCC0\uC218\uB97C \uD55C \uBC88 \uC124\uC815\uD558\uACE0 \uBAA8\uB4E0 \uBCF5\uD569\uD45C\uBCF8 \uBD84\uC11D\uC5D0 \uC790\uB3D9\uC73C\uB85C \uC801\uC6A9\uD569\uB2C8\uB2E4."),
    design_input_block = c(en = "Design variables", ko = "\uC124\uACC4\uBCC0\uC218 \uC785\uB825"),
        design_options_block = c(en = "Design options", ko = "\uC124\uACC4 \uC635\uC158"),
        save_design_settings = c(en = "Save design", ko = "\uC124\uACC4 \uC800\uC7A5"),
        load_design_settings = c(en = "Load design", ko = "\uC124\uACC4 \uBD88\uB7EC\uC624\uAE30"),
        post_hoc_options = c(en = "Post-hoc", ko = "\uC0AC\uD6C4\uBD84\uC11D"),
        estimate_options = c(en = "Estimates", ko = "\uCD94\uC815\uAC12"),
        design_stat_options = c(en = "Design statistics", ko = "\uC124\uACC4 \uD1B5\uACC4"),
        table_output_options = c(en = "Table output", ko = "\uD45C \uCD9C\uB825"),
    subtitle = c(en = "Move variables into the analysis lists and assign the complex-sample design variables.", ko = "\uBCC0\uC218\uB97C \uBD84\uC11D \uBAA9\uB85D\uC73C\uB85C \uC62E\uAE30\uACE0 \uBCF5\uD569\uD45C\uBCF8 \uC124\uACC4 \uBCC0\uC218\uB97C \uC9C0\uC815\uD558\uC138\uC694."),
    design = c(en = "Complex-sample design variables", ko = "\uBCF5\uD569\uD45C\uBCF8 \uC124\uACC4 \uBCC0\uC218"),
    strata = c(en = "Strata variable", ko = "\uCE35\uD654\uBCC0\uC218"),
    cluster = c(en = "Cluster / PSU variable", ko = "\uC9D1\uB77D / PSU \uBCC0\uC218"),
    weight = c(en = "Weight variable", ko = "\uAC00\uC911\uCE58 \uBCC0\uC218"),
    fpc = c(en = "Finite Population Correction (FPC) variable", ko = "\uC720\uD55C\uBAA8\uC9D1\uB2E8 \uBCF4\uC815(FPC) \uBCC0\uC218"),
    replicate_weights = c(en = "Replicate-weight variables", ko = "\uBCF5\uC81C \uAC00\uC911\uCE58 \uBCC0\uC218"),
    replicate_type = c(en = "Replicate-weight type", ko = "\uBCF5\uC81C \uAC00\uC911\uCE58 \uC720\uD615"),
    use_replicate_weights = c(en = "Use replicate weights", ko = "\uBCF5\uC81C \uAC00\uC911\uCE58 \uC0AC\uC6A9"),
    replicate_combined_weights = c(en = "Replicate weights include sampling weights", ko = "\uBCF5\uC81C\uAC00\uC911\uCE58\uC5D0 \uD45C\uBCF8\uAC00\uC911\uCE58 \uD3EC\uD568"),
    design_tab = c(en = "Design variables", ko = "\uC124\uACC4\uBCC0\uC218"),
        weight_tab = c(en = "Replicate weights / FPC", ko = "\uBCF5\uC81C\uAC00\uC911\uCE58/FPC"),
        options_tab = c(en = "Options", ko = "\uC635\uC158"),
        show_ci = c(en = "95% CI", ko = "95% CI"),
        show_weighted_n = c(en = "Weighted N", ko = "\uAC00\uC911 N"),
        show_missing = c(en = "Missing N", ko = "\uACB0\uCE21 N"),
        show_df = c(en = "Design df", ko = "\uC124\uACC4 df"),
        show_precision = c(en = "Design effect / CV", ko = "\uC124\uACC4\uD6A8\uACFC / CV"),
        show_median = c(en = "Median", ko = "\uC911\uC704\uC218"),
        show_effect_size = c(en = "Effect size", ko = "\uD6A8\uACFC\uD06C\uAE30"),
        show_model_fit = c(en = "Model fit statistics", ko = "\uBAA8\uD615 \uC801\uD569 \uD1B5\uACC4\uB7C9"),
        show_wald = c(en = "Wald statistic", ko = "Wald \uD1B5\uACC4\uB7C9"),
        post_hoc = c(en = "Post-hoc analysis", ko = "\uC0AC\uD6C4\uBD84\uC11D"),
        post_hoc_correction = c(en = "Post-hoc correction", ko = "\uC0AC\uD6C4\uBD84\uC11D \uBCF4\uC815\uBC95"),
        ordered_significance = c(en = "Mean-order significance notation", ko = "\uD3C9\uADE0\uC21C \uC720\uC758\uC131 \uD45C\uAE30"),
        mean_sd = c(en = "M \u00B1 SD", ko = "M \u00B1 SD"),
        crosstab_percent_basis = c(en = "Percent basis", ko = "\uD37C\uC13C\uD2B8 \uAE30\uC900"),
        crosstab_test_method = c(en = "Test statistic", ko = "\uAC80\uC815 \uD1B5\uACC4\uB7C9"),
        correlation_method = c(en = "Correlation method", ko = "\uC0C1\uAD00 \uBC29\uBC95"),
        correlation_p_adjust = c(en = "P-value adjustment", ko = "p\uAC12 \uBCF4\uC815"),
        correlation_matrix = c(en = "Correlation matrix", ko = "\uC0C1\uAD00\uD589\uB82C"),
        show_percent_ci = c(en = "Weighted % 95% CI", ko = "\uAC00\uC911 % 95% CI"),
        trend_analysis = c(en = "p for trend", ko = "\uCD94\uC138\uAC80\uC815"),
        variance_method = c(en = "Variance estimation method", ko = "\uBD84\uC0B0\uCD94\uC815 \uBC29\uBC95"),
        lonely_psu = c(en = "Single-PSU strata handling", ko = "\uB2E8\uC77C PSU \uCE35 \uCC98\uB9AC"),
        subpopulation = c(en = "Subpopulation variable", ko = "\uBD80-\uBAA8\uC9D1\uB2E8 \uBCC0\uC218"),
        subpopulation_condition = c(en = "Subpopulation condition", ko = "\uBD80-\uBAA8\uC9D1\uB2E8 \uC870\uAC74"),
        subpopulation_condition_type = c(en = "Subpopulation condition", ko = "\uBD84\uC11D \uB300\uC0C1\uC9D1\uB2E8 \uC870\uAC74"),
        subpopulation_value = c(en = "Value", ko = "\uAC12"),
        subpopulation_formula = c(en = "Formula", ko = "\uC218\uC2DD"),
        value_match = c(en = "Value match", ko = "\uAC12 \uC77C\uCE58"),
        value_not_match = c(en = "Value not match", ko = "\uAC12 \uBD88\uC77C\uCE58"),
        custom_formula = c(en = "User formula", ko = "\uC0AC\uC6A9\uC790 \uC218\uC2DD"),
        non_missing = c(en = "Non-missing", ko = "\uACB0\uCE21 \uC544\uB2D8"),
        greater_than = c(en = "Greater than", ko = "\uCD08\uACFC"),
        greater_equal = c(en = "Greater than or equal", ko = "\uC774\uC0C1"),
        less_than = c(en = "Less than", ko = "\uBBF8\uB9CC"),
        less_equal = c(en = "Less than or equal", ko = "\uC774\uD558"),
        no_variable = c(en = "No variable", ko = "\uBCC0\uC218 \uC5C6\uC74C"),
    setup_note = c(en = "Complex-sample design variables are applied to the analysis through the survey engine.", ko = "\uBCF5\uD569\uD45C\uBCF8 \uC124\uACC4 \uBCC0\uC218\uB97C survey \uC5D4\uC9C4\uC73C\uB85C \uBD84\uC11D\uC5D0 \uC801\uC6A9\uD569\uB2C8\uB2E4."),
    replicate_note = c(en = "If the data do not include replicate weights, leave this empty and use Auto or Taylor linearization.", ko = "\uBCF5\uC81C \uAC00\uC911\uCE58\uAC00 \uC5C6\uB294 \uC790\uB8CC\uB294 \uBE44\uC6CC\uB450\uACE0 Auto \uB610\uB294 Taylor linearization\uC744 \uC0AC\uC6A9\uD558\uC138\uC694."),
    design_candidate_note = c(en = "Design-variable candidates are prefilled from the selected variables.", ko = "\uC120\uD0DD\uB41C \uBCC0\uC218 \uC911\uC5D0\uC11C \uC124\uACC4\uBCC0\uC218 \uD6C4\uBCF4\uB97C \uC790\uB3D9\uC73C\uB85C \uB123\uC2B5\uB2C8\uB2E4."),
    subpopulation_placeholder = c(en = "Example: == 1, %in% c(1, 2), or > 0", ko = "\uC608: == 1, %in% c(1, 2), \uB610\uB294 > 0"),
    selected = c(en = "Selected Variables", ko = "\uC120\uD0DD \uBCC0\uC218"),
    column = c(en = "Column variable", ko = "\uC5F4 \uBCC0\uC218"),
    row = c(en = "Row variable", ko = "\uD589 \uBCC0\uC218"),
    dependent = c(en = "Dependent variables", ko = "\uC885\uC18D\uBCC0\uC218"),
    independent = c(en = "Independent variables", ko = "\uB3C5\uB9BD\uBCC0\uC218"),
    outcome = c(en = "Dependent variable", ko = "\uC885\uC18D\uBCC0\uC218"),
    predictors = c(en = "Predictors", ko = "\uC608\uCE21\uBCC0\uC218"),
    run = c(en = "Run analysis", ko = "\uBD84\uC11D \uC2E4\uD589"),
    section = c(en = "Section", ko = "\uAD6C\uBD84"),
    variables = c(en = "Variables", ko = "\uBCC0\uC218"),
    design_summary = c(en = "Design variable summary", ko = "\uC124\uACC4 \uBCC0\uC218 \uC694\uC57D")
  )
  value <- labels[[key]]
  if (is.null(value)) {
    return(key)
  }
  language <- normalize_app_language(language)
  if (identical(language, "ko")) value[["ko"]] else value[["en"]]
}

complex_sample_text_pair <- function(language, en, ko) {
  language <- normalize_app_language(language)
  if (identical(language, "ko")) ko else en
}

complex_sample_yes_no <- function(value, language = statedu_initial_language()) {
  complex_sample_text_pair(language, if (isTRUE(value)) "Yes" else "No", if (isTRUE(value)) "\uC608" else "\uC544\uB2C8\uC624")
}

complex_sample_choice_values <- function(names, variable_table = NULL, labels = character(0), allowed_measurements = character(0), language = statedu_initial_language()) {
  variables <- analysis_allowed_variables(names, variable_table, allowed_measurements)
  choices <- stats::setNames(variables, vapply(variables, display_variable_name_static, character(1), table = variable_table, labels = labels))
  c(stats::setNames("", complex_sample_ui_text("no_variable", language)), choices)
}

complex_sample_design_choices <- function(selected_names, all_names = selected_names, variable_table = NULL, labels = character(0), allowed_measurements = character(0), language = statedu_initial_language()) {
  selected_names <- as.character(selected_names %||% character(0))
  all_names <- as.character(all_names %||% selected_names)
  selected <- analysis_allowed_variables(intersect(selected_names, all_names), variable_table, allowed_measurements)
  other <- analysis_allowed_variables(setdiff(all_names, selected), variable_table, allowed_measurements)
  selected_choices <- stats::setNames(selected, vapply(selected, display_variable_name_static, character(1), table = variable_table, labels = labels))
  other_choices <- stats::setNames(other, vapply(other, display_variable_name_static, character(1), table = variable_table, labels = labels))
  c(stats::setNames("", complex_sample_ui_text("no_variable", language)), selected_choices, other_choices)
}

complex_sample_display_names <- function(names, variable_table = NULL, labels = character(0)) {
  names <- as.character(names %||% character(0))
  names <- names[nzchar(names)]
  if (length(names) == 0) {
    return("")
  }
  paste(vapply(names, display_variable_name_static, character(1), table = variable_table, labels = labels), collapse = ", ")
}

complex_sample_candidate_text <- function(name, variable_table = NULL, labels = character(0)) {
  name <- as.character(name %||% "")
  row_label <- ""
  if (is.data.frame(variable_table) && all(c("name", "var_label") %in% names(variable_table))) {
    row_index <- match(name, as.character(variable_table$name))
    if (!is.na(row_index)) {
      row_label <- as.character(variable_table$var_label[[row_index]] %||% "")
    }
  }
  override_label <- named_value(labels, name, "")
  paste(tolower(c(name, row_label, override_label)), collapse = " ")
}

complex_sample_candidate_variable <- function(selected_names, variable_table = NULL, labels = character(0), patterns, allowed_measurements = analysis_allowed_measurements_all(), exclude_patterns = character(0)) {
  candidates <- analysis_allowed_variables(selected_names, variable_table, allowed_measurements)
  if (length(candidates) == 0) {
    return("")
  }
  for (name in candidates) {
    text <- complex_sample_candidate_text(name, variable_table, labels)
    if (length(exclude_patterns) > 0 && any(vapply(exclude_patterns, grepl, logical(1), x = text, ignore.case = TRUE, perl = TRUE))) {
      next
    }
    if (any(vapply(patterns, grepl, logical(1), x = text, ignore.case = TRUE, perl = TRUE))) {
      return(name)
    }
  }
  ""
}

complex_sample_design_role_spec <- function(role) {
  switch(
    as.character(role %||% ""),
    strata = list(
      patterns = c("strata|stratum|stratification|kstrata|층화|層化|分层|estrato|strate|schicht|tầng"),
      allowed_measurements = analysis_allowed_measurements_all(),
      exclude_patterns = character(0)
    ),
    cluster = list(
      patterns = c("cluster|psu|primary sampling unit|집락|クラスター|聚类|群集|conglomerado|grappe|cụm"),
      allowed_measurements = analysis_allowed_measurements_all(),
      exclude_patterns = character(0)
    ),
    weight = list(
      patterns = c("weight|\\bwt\\b|^wt[_\\. -]?|[_\\. -]wt[_\\. -]?|wgt|가중|重み|权重|權重|peso|poids|gewicht|trọng số"),
      allowed_measurements = "continuous",
      exclude_patterns = c("replicate|rep[_ -]?weight|repwt|brr|fay|jackknife|jk|bootstrap")
    ),
    NULL
  )
}

complex_sample_design_candidate_variables <- function(role, names, variable_table = NULL, labels = character(0)) {
  spec <- complex_sample_design_role_spec(role)
  if (is.null(spec)) {
    return(character(0))
  }
  candidates <- analysis_allowed_variables(names, variable_table, spec$allowed_measurements)
  matched <- vapply(candidates, function(name) {
    text <- complex_sample_candidate_text(name, variable_table, labels)
    excluded <- length(spec$exclude_patterns) > 0 && any(vapply(
      spec$exclude_patterns,
      grepl,
      logical(1),
      x = text,
      ignore.case = TRUE,
      perl = TRUE
    ))
    !excluded && any(vapply(spec$patterns, grepl, logical(1), x = text, ignore.case = TRUE, perl = TRUE))
  }, logical(1))
  candidates[matched]
}

complex_sample_design_role_choices <- function(role, selected_names, all_names = selected_names, variable_table = NULL, labels = character(0), language = statedu_initial_language()) {
  spec <- complex_sample_design_role_spec(role)
  if (is.null(spec)) {
    return(complex_sample_design_choices(selected_names, all_names, variable_table, labels, language = language))
  }
  selected_names <- as.character(selected_names %||% character(0))
  all_names <- as.character(all_names %||% selected_names)
  selected <- analysis_allowed_variables(intersect(selected_names, all_names), variable_table, spec$allowed_measurements)
  other <- analysis_allowed_variables(setdiff(all_names, selected), variable_table, spec$allowed_measurements)
  variables <- unique(c(selected, other))
  candidates <- complex_sample_design_candidate_variables(role, variables, variable_table, labels)
  ordered <- unique(c(candidates, variables))
  choices <- stats::setNames(ordered, vapply(ordered, display_variable_name_static, character(1), table = variable_table, labels = labels))
  c(stats::setNames("", complex_sample_ui_text("no_variable", language)), choices)
}

complex_sample_candidate_replicates <- function(selected_names, variable_table = NULL, labels = character(0)) {
  candidates <- analysis_allowed_variables(selected_names, variable_table, "continuous")
  if (length(candidates) == 0) {
    return(character(0))
  }
  matched <- vapply(candidates, function(name) {
    text <- complex_sample_candidate_text(name, variable_table, labels)
    grepl("replicate|rep[_ -]?weight|repwt|brr|fay|jackknife|jk[0-9a-z_]*|bootstrap|^rw[0-9]+$|^wgt[0-9]+$", text, ignore.case = TRUE, perl = TRUE)
  }, logical(1))
  candidates[matched]
}

complex_sample_filter_candidate <- function(selected_names, all_names = selected_names, variable_table = NULL, labels = character(0)) {
  names <- unique(c(as.character(selected_names %||% character(0)), as.character(all_names %||% character(0))))
  candidates <- analysis_allowed_variables(names, variable_table, analysis_allowed_measurements_all())
  if (length(candidates) == 0) {
    return("")
  }
  exact <- candidates[tolower(candidates) == "filter"]
  if (length(exact) > 0) {
    return(exact[[1]])
  }
  for (name in candidates) {
    text <- complex_sample_candidate_text(name, variable_table, labels)
    if (grepl("(^|[^[:alnum:]_])filter([^[:alnum:]_]|$)", text, ignore.case = TRUE, perl = TRUE)) {
      return(name)
    }
  }
  ""
}

complex_sample_design_defaults <- function(prefix, selected_names, variable_table = NULL, labels = character(0)) {
  stats::setNames(
    list(
      c(complex_sample_design_candidate_variables("strata", selected_names, variable_table, labels), "")[[1]],
      c(complex_sample_design_candidate_variables("cluster", selected_names, variable_table, labels), "")[[1]],
      c(complex_sample_design_candidate_variables("weight", selected_names, variable_table, labels), "")[[1]],
      complex_sample_candidate_replicates(selected_names, variable_table, labels),
      complex_sample_filter_candidate(selected_names, selected_names, variable_table, labels),
      "equals",
      "1"
    ),
    paste0(prefix, c("_strata", "_cluster", "_weight", "_replicate_weights", "_subpopulation", "_subpopulation_condition_type", "_subpopulation_condition_value"))
  )
}

complex_sample_subpopulation_choices <- function(selected_names, all_names = selected_names, variable_table = NULL, labels = character(0), language = statedu_initial_language()) {
  selected_names <- as.character(selected_names %||% character(0))
  all_names <- as.character(all_names %||% selected_names)
  filter <- complex_sample_filter_candidate(selected_names, all_names, variable_table, labels)
  selected <- analysis_allowed_variables(intersect(selected_names, all_names), variable_table, analysis_allowed_measurements_all())
  other <- analysis_allowed_variables(setdiff(all_names, selected), variable_table, analysis_allowed_measurements_all())
  ordered <- unique(c(filter[nzchar(filter)], selected, other))
  ordered <- ordered[nzchar(ordered)]
  ordered <- ordered[!ordered %in% filter]
  filter_choices <- if (nzchar(filter)) {
    stats::setNames(filter, display_variable_name_static(filter, variable_table, labels))
  } else {
    NULL
  }
  variable_choices <- stats::setNames(ordered, vapply(ordered, display_variable_name_static, character(1), table = variable_table, labels = labels))
  c(filter_choices, stats::setNames("", complex_sample_ui_text("no_variable", language)), variable_choices)
}

complex_sample_selected_value <- function(selected, id, default = "") {
  value <- selected[[id]]
  if (is.null(value) || length(value) == 0) {
    return(default)
  }
  value
}

complex_sample_selected_tab <- function(selected, id, default, choices) {
  value <- as.character(complex_sample_selected_value(selected, id, default))
  if (length(value) == 0 || !value[[1]] %in% choices) {
    return(default)
  }
  value[[1]]
}

complex_sample_design_assigned_variables <- function(prefix, selected_names, variable_table = NULL, labels = character(0), selected = list()) {
  defaults <- complex_sample_design_defaults(prefix, selected_names, variable_table, labels)
  ids <- paste0(prefix, c("_strata", "_cluster", "_weight", "_subpopulation"))
  values <- vapply(ids, function(id) {
    complex_sample_selected_value(selected, id, defaults[[id]] %||% "")
  }, character(1))
  intersect(values[nzchar(values)], selected_names)
}

complex_sample_option_checkbox <- function(prefix, key, selected, language, default = FALSE) {
  id <- paste0(prefix, "_", key)
  checkboxInput(
    id,
    complex_sample_ui_text(key, language),
    value = isTRUE(complex_sample_selected_value(selected, id, default))
  )
}

complex_sample_option_select <- function(prefix, key, selected, language, choices, default) {
  id <- paste0(prefix, "_", key)
  selectInput(
    id,
    complex_sample_ui_text(key, language),
    choices = choices,
    selected = complex_sample_selected_value(selected, id, default),
    selectize = FALSE
  )
}

complex_sample_crosstab_percent_basis_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("row", "column", "total"),
    c(
      complex_sample_text_pair(language, "Row %", "\uD589 %"),
      complex_sample_text_pair(language, "Column %", "\uC5F4 %"),
      complex_sample_text_pair(language, "Total %", "\uC804\uCCB4 %")
    )
  )
}

complex_sample_crosstab_test_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("F", "Chisq", "Wald", "adjWald", "saddlepoint"),
    c("Rao-Scott F", "Rao-Scott \u03C7\u00B2", "Wald", "Adjusted Wald", "Saddlepoint")
  )
}

complex_sample_correlation_method_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("pearson", "spearman"),
    c(
      complex_sample_text_pair(language, "Pearson correlation", "Pearson \uC0C1\uAD00"),
      complex_sample_text_pair(language, "Spearman rank correlation", "Spearman \uC21C\uC704\uC0C1\uAD00")
    )
  )
}

complex_sample_post_hoc_correction_choices <- function(language = statedu_initial_language()) {
  labels <- c("Holm Bonferroni", "Bonferroni correction")
  stats::setNames(
    c("holm", "bonferroni"),
    vapply(labels, analysis_ui_text, character(1), language = language)
  )
}

complex_sample_correlation_p_adjust_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("holm", "bonferroni", "BH", "none"),
    c(
      analysis_ui_text("Holm Bonferroni", language),
      analysis_ui_text("Bonferroni correction", language),
      "FDR (Benjamini-Hochberg)",
      complex_sample_text_pair(language, "No adjustment", "\uBCF4\uC815 \uC5C6\uC74C")
    )
  )
}

complex_sample_option_control <- function(prefix, key, selected, language, default = FALSE) {
  if (identical(key, "crosstab_percent_basis")) {
    return(complex_sample_option_select(prefix, key, selected, language, complex_sample_crosstab_percent_basis_choices(language), default))
  }
  if (identical(key, "crosstab_test_method")) {
    return(complex_sample_option_select(prefix, key, selected, language, complex_sample_crosstab_test_choices(language), default))
  }
  if (identical(key, "post_hoc_correction")) {
    return(complex_sample_option_select(prefix, key, selected, language, complex_sample_post_hoc_correction_choices(language), default))
  }
  if (identical(key, "correlation_method")) {
    return(complex_sample_option_select(prefix, key, selected, language, complex_sample_correlation_method_choices(language), default))
  }
  if (identical(key, "correlation_p_adjust")) {
    return(complex_sample_option_select(prefix, key, selected, language, complex_sample_correlation_p_adjust_choices(language), default))
  }
  complex_sample_option_checkbox(prefix, key, selected, language, default)
}

complex_sample_option_keys <- function(analysis_type = NULL) {
  analysis_type <- as.character(analysis_type %||% "")
  switch(
    analysis_type,
    frequencies = c("show_ci", "show_weighted_n", "show_missing", "show_precision", "show_median"),
    crosstabs = c("crosstab_percent_basis", "crosstab_test_method", "show_weighted_n", "show_df", "show_percent_ci", "trend_analysis"),
    ttest_anova = c("post_hoc", "post_hoc_correction", "ordered_significance", "mean_sd", "trend_analysis", "show_ci", "show_weighted_n", "show_df", "show_precision", "show_effect_size"),
    correlation = c("correlation_method", "correlation_p_adjust", "correlation_matrix", "show_ci", "show_weighted_n", "show_missing", "show_df"),
    regression = c("show_ci", "show_weighted_n", "show_df", "show_model_fit"),
    logistic = c("show_ci", "show_weighted_n", "show_df", "show_model_fit", "show_wald"),
    c("show_ci", "show_weighted_n", "show_df", "show_precision")
  )
}

complex_sample_option_groups <- function(analysis_type = NULL) {
  analysis_type <- as.character(analysis_type %||% "")
  if (identical(analysis_type, "ttest_anova")) {
    return(list(
      post_hoc_options = c("post_hoc", "post_hoc_correction", "ordered_significance"),
      estimate_options = c("mean_sd", "show_ci", "show_effect_size"),
      design_stat_options = c("show_weighted_n", "show_df", "show_precision", "trend_analysis")
    ))
  }
  list(table_output_options = complex_sample_option_keys(analysis_type))
}

complex_sample_option_defaults <- function() {
  list(
    show_ci = TRUE,
    show_weighted_n = FALSE,
    show_missing = TRUE,
    show_df = TRUE,
    show_precision = FALSE,
    show_median = FALSE,
    show_effect_size = TRUE,
    show_model_fit = TRUE,
    show_wald = TRUE,
    post_hoc = FALSE,
    post_hoc_correction = statedu_multiple_correction_default(),
    ordered_significance = FALSE,
    mean_sd = FALSE,
    crosstab_percent_basis = "row",
    crosstab_test_method = "F",
    correlation_method = "pearson",
    correlation_p_adjust = statedu_multiple_correction_default(),
    correlation_matrix = TRUE,
    show_percent_ci = FALSE,
    trend_analysis = FALSE
  )
}

complex_sample_options_tab_content <- function(prefix, analysis_type = NULL, selected = list(), language = statedu_initial_language()) {
  option_groups <- complex_sample_option_groups(analysis_type)
  defaults <- complex_sample_option_defaults()
  div(
    class = "factor-options-tab-content ttest-anova-options-tab-content complex-sample-options-tab-content",
    lapply(names(option_groups), function(group_key) {
      div(
        class = "complex-sample-option-group",
        div(class = "analysis-option-title complex-sample-option-group-title", complex_sample_ui_text(group_key, language)),
        lapply(option_groups[[group_key]], function(key) {
          complex_sample_option_control(prefix, key, selected, language, defaults[[key]] %||% FALSE)
        })
      )
    })
  )
}

complex_sample_analysis_options_panel <- function(prefix, analysis_type = NULL, selected = list(), language = statedu_initial_language()) {
  div(
    class = "analysis-options-column ttest-anova-options-column complex-sample-design-column complex-sample-analysis-options-column",
    div(
      class = "analysis-options-panel ttest-anova-options regression-options complex-sample-design-panel complex-sample-analysis-options-panel",
      complex_sample_options_tab_content(prefix, analysis_type, selected, language)
    )
  )
}

complex_sample_analysis_options <- function(input, prefix, analysis_type = NULL) {
  defaults <- complex_sample_option_defaults()
  option_keys <- complex_sample_option_keys(analysis_type)
  options <- defaults
  for (key in names(options)) {
    if (key %in% option_keys) {
      value <- input[[paste0(prefix, "_", key)]] %||% defaults[[key]]
      if (identical(key, "crosstab_percent_basis")) {
        value <- as.character(value %||% defaults[[key]])
        if (!value %in% c("row", "column", "total")) value <- defaults[[key]]
        options[[key]] <- value
      } else if (identical(key, "crosstab_test_method")) {
        value <- as.character(value %||% defaults[[key]])
        if (!value %in% c("F", "Chisq", "Wald", "adjWald", "saddlepoint")) value <- defaults[[key]]
        options[[key]] <- value
      } else if (identical(key, "post_hoc_correction")) {
        value <- as.character(value %||% defaults[[key]])
        if (!value %in% c("holm", "bonferroni")) value <- defaults[[key]]
        options[[key]] <- value
      } else if (identical(key, "correlation_method")) {
        value <- as.character(value %||% defaults[[key]])
        if (!value %in% c("pearson", "spearman")) value <- defaults[[key]]
        options[[key]] <- value
      } else if (identical(key, "correlation_p_adjust")) {
        value <- as.character(value %||% defaults[[key]])
        if (!value %in% c("none", "holm", "bonferroni", "BH")) value <- defaults[[key]]
        options[[key]] <- value
      } else {
        options[[key]] <- isTRUE(value)
      }
    } else {
      options[[key]] <- FALSE
    }
  }
  options
}

complex_sample_options_are_default <- function(input, prefix, analysis_type = NULL) {
  defaults <- complex_sample_option_defaults()
  options <- complex_sample_analysis_options(input, prefix, analysis_type)
  option_keys <- complex_sample_option_keys(analysis_type)
  all(vapply(option_keys, function(key) {
    if (is.logical(defaults[[key]])) {
      return(identical(isTRUE(options[[key]]), isTRUE(defaults[[key]])))
    }
    identical(as.character(options[[key]]), as.character(defaults[[key]]))
  }, logical(1)))
}

complex_sample_design_panel <- function(prefix, selected_names, all_names = selected_names, variable_table = NULL, labels = character(0), language = statedu_initial_language(), selected = list(), analysis_type = NULL, include_analysis_options = TRUE) {
  selected_names <- as.character(selected_names %||% character(0))
  all_names <- as.character(all_names %||% selected_names)
  defaults <- complex_sample_design_defaults(prefix, selected_names, variable_table, labels)
  strata_id <- paste0(prefix, "_strata")
  cluster_id <- paste0(prefix, "_cluster")
  weight_id <- paste0(prefix, "_weight")
  fpc_id <- paste0(prefix, "_fpc")
  use_replicate_weights_id <- paste0(prefix, "_use_replicate_weights")
  replicate_weights_id <- paste0(prefix, "_replicate_weights")
  replicate_type_id <- paste0(prefix, "_replicate_type")
  replicate_combined_weights_id <- paste0(prefix, "_replicate_combined_weights")
  variance_method_id <- paste0(prefix, "_variance_method")
  lonely_psu_id <- paste0(prefix, "_lonely_psu")
  subpopulation_id <- paste0(prefix, "_subpopulation")
  subpopulation_condition_id <- paste0(prefix, "_subpopulation_condition")
  subpopulation_condition_type_id <- paste0(prefix, "_subpopulation_condition_type")
  subpopulation_condition_value_id <- paste0(prefix, "_subpopulation_condition_value")
  subpopulation_default <- complex_sample_filter_candidate(selected_names, all_names, variable_table, labels)
  subpopulation_selected <- complex_sample_selected_value(selected, subpopulation_id, subpopulation_default)
  subpopulation_condition_type_default <- if (nzchar(subpopulation_default)) "equals" else "equals"
  subpopulation_condition_value_default <- if (nzchar(subpopulation_default)) "1" else ""
  design_tab_label <- complex_sample_ui_text("design_tab", language)
  weight_tab_label <- complex_sample_ui_text("weight_tab", language)
  options_tab_label <- complex_sample_ui_text("options_tab", language)
  design_tab_choices <- c(
    design_tab_label,
    weight_tab_label,
    if (isTRUE(include_analysis_options)) options_tab_label else character(0)
  )
  div(
    class = "analysis-options-column ttest-anova-options-column complex-sample-design-column",
    analysis_options_tabs_panel(
      id = paste0(prefix, "_design_options_tab"),
      selected = complex_sample_selected_tab(
        selected,
        paste0(prefix, "_design_options_tab"),
        design_tab_label,
        design_tab_choices
      ),
      class = "ttest-anova-options regression-options complex-sample-design-panel",
      tabPanel(
        design_tab_label,
        value = design_tab_label,
        div(
          class = "factor-options-tab-content ttest-anova-options-tab-content complex-sample-design-tab-content",
          selectInput(
            strata_id,
            complex_sample_ui_text("strata", language),
            choices = complex_sample_design_role_choices("strata", selected_names, all_names, variable_table, labels, language),
            selected = complex_sample_selected_value(selected, strata_id, defaults[[strata_id]] %||% ""),
            selectize = FALSE
          ),
          selectInput(
            cluster_id,
            complex_sample_ui_text("cluster", language),
            choices = complex_sample_design_role_choices("cluster", selected_names, all_names, variable_table, labels, language),
            selected = complex_sample_selected_value(selected, cluster_id, defaults[[cluster_id]] %||% ""),
            selectize = FALSE
          ),
          selectInput(
            weight_id,
            complex_sample_ui_text("weight", language),
            choices = complex_sample_design_role_choices("weight", selected_names, all_names, variable_table, labels, language),
            selected = complex_sample_selected_value(selected, weight_id, defaults[[weight_id]] %||% ""),
            selectize = FALSE
          ),
          selectInput(
            variance_method_id,
            complex_sample_ui_text("variance_method", language),
            choices = c(
              "Auto" = "auto",
              "Taylor linearization" = "taylor",
              "BRR" = "brr",
              "Fay BRR" = "fay",
              "JK1" = "jk1",
              "JKn" = "jkn",
              "JK2" = "jk2",
              "Bootstrap" = "bootstrap"
            ),
            selected = selected[[variance_method_id]] %||% "auto",
            selectize = FALSE
          ),
          selectInput(
            subpopulation_id,
            complex_sample_ui_text("subpopulation", language),
            choices = complex_sample_subpopulation_choices(selected_names, all_names, variable_table, labels, language),
            selected = subpopulation_selected,
            selectize = FALSE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] && input['%s'] !== ''", subpopulation_id, subpopulation_id),
            div(
              class = "complex-sample-subpopulation-condition",
              selectInput(
                subpopulation_condition_type_id,
                complex_sample_ui_text("subpopulation_condition_type", language),
                choices = c(
                  stats::setNames("equals", complex_sample_ui_text("value_match", language)),
                  stats::setNames("not_equals", complex_sample_ui_text("value_not_match", language)),
                  stats::setNames("not_missing", complex_sample_ui_text("non_missing", language)),
                  stats::setNames("greater", complex_sample_ui_text("greater_than", language)),
                  stats::setNames("greater_equal", complex_sample_ui_text("greater_equal", language)),
                  stats::setNames("less", complex_sample_ui_text("less_than", language)),
                  stats::setNames("less_equal", complex_sample_ui_text("less_equal", language)),
                  stats::setNames("custom_formula", complex_sample_ui_text("custom_formula", language))
                ),
                selected = complex_sample_selected_value(selected, subpopulation_condition_type_id, subpopulation_condition_type_default),
                selectize = FALSE
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] !== 'not_missing' && input['%s'] !== 'custom_formula'", subpopulation_condition_type_id, subpopulation_condition_type_id),
                textInput(
                  subpopulation_condition_value_id,
                  complex_sample_ui_text("subpopulation_value", language),
                  value = complex_sample_selected_value(selected, subpopulation_condition_value_id, complex_sample_selected_value(selected, subpopulation_condition_id, subpopulation_condition_value_default)),
                  width = "100%"
                )
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] === 'custom_formula'", subpopulation_condition_type_id),
                textInput(
                  subpopulation_condition_id,
                  complex_sample_ui_text("subpopulation_formula", language),
                  value = complex_sample_selected_value(selected, subpopulation_condition_id, ""),
                  placeholder = complex_sample_ui_text("subpopulation_placeholder", language),
                  width = "100%"
                )
              )
            )
          )
        )
      ),
      tabPanel(
        weight_tab_label,
        value = weight_tab_label,
        div(
          class = "factor-options-tab-content ttest-anova-options-tab-content complex-sample-replicate-tab-content",
          selectInput(
            fpc_id,
            complex_sample_ui_text("fpc", language),
            choices = complex_sample_design_choices(selected_names, all_names, variable_table, labels, "continuous", language),
            selected = complex_sample_selected_value(selected, fpc_id, ""),
            selectize = FALSE
          ),
          selectInput(
            lonely_psu_id,
            complex_sample_ui_text("lonely_psu", language),
            choices = c(
              "Adjust" = "adjust",
              "Average" = "average",
              "Certainty" = "certainty",
              "Remove" = "remove",
              "Fail" = "fail"
            ),
            selected = selected[[lonely_psu_id]] %||% "adjust",
            selectize = FALSE
          ),
          checkboxInput(
            use_replicate_weights_id,
            complex_sample_ui_text("use_replicate_weights", language),
            value = isTRUE(complex_sample_selected_value(selected, use_replicate_weights_id, FALSE))
          ),
          div(class = "analysis-option-note complex-sample-replicate-note", complex_sample_ui_text("replicate_note", language)),
          conditionalPanel(
            condition = sprintf("input['%s'] === true", use_replicate_weights_id),
            selectInput(
              replicate_weights_id,
              complex_sample_ui_text("replicate_weights", language),
              choices = complex_sample_design_choices(selected_names, all_names, variable_table, labels, "continuous", language)[-1],
              selected = complex_sample_selected_value(selected, replicate_weights_id, defaults[[replicate_weights_id]] %||% character(0)),
              multiple = TRUE,
              selectize = FALSE
            ),
            selectInput(
              replicate_type_id,
              complex_sample_ui_text("replicate_type", language),
              choices = c(
                "Auto" = "auto",
                "BRR" = "brr",
                "Fay BRR" = "fay",
                "JK1" = "jk1",
                "JKn" = "jkn",
                "JK2" = "jk2",
                "Bootstrap" = "bootstrap"
              ),
              selected = selected[[replicate_type_id]] %||% "auto",
              selectize = FALSE
            ),
            checkboxInput(
              replicate_combined_weights_id,
              complex_sample_ui_text("replicate_combined_weights", language),
              value = isTRUE(complex_sample_selected_value(selected, replicate_combined_weights_id, FALSE))
            )
          )
        )
      ),
      if (isTRUE(include_analysis_options)) {
        tabPanel(
          options_tab_label,
          value = options_tab_label,
          complex_sample_options_tab_content(prefix, analysis_type, selected, language)
        )
      }
    )
  )
}

complex_sample_design_core_panel <- function(prefix, selected_names, all_names = selected_names, variable_table = NULL, labels = character(0), language = statedu_initial_language(), selected = list()) {
  selected_names <- as.character(selected_names %||% character(0))
  all_names <- as.character(all_names %||% selected_names)
  defaults <- complex_sample_design_defaults(prefix, selected_names, variable_table, labels)
  subpopulation_id <- paste0(prefix, "_subpopulation")
  subpopulation_condition_id <- paste0(prefix, "_subpopulation_condition")
  subpopulation_condition_type_id <- paste0(prefix, "_subpopulation_condition_type")
  subpopulation_condition_value_id <- paste0(prefix, "_subpopulation_condition_value")
  subpopulation_default <- complex_sample_filter_candidate(selected_names, all_names, variable_table, labels)
  subpopulation_selected <- complex_sample_selected_value(selected, subpopulation_id, subpopulation_default)
  subpopulation_condition_value_default <- if (nzchar(subpopulation_default)) "1" else ""

  div(
    class = "complex-sample-design-column complex-sample-design-core-column",
    div(
      class = "analysis-options-panel ttest-anova-options regression-options complex-sample-design-panel",
      div(class = "analysis-options-title", complex_sample_ui_text("design_input_block", language)),
      div(
        class = "factor-options-tab-content ttest-anova-options-tab-content complex-sample-design-tab-content",
        selectInput(
          paste0(prefix, "_strata"),
          complex_sample_ui_text("strata", language),
          choices = complex_sample_design_role_choices("strata", selected_names, all_names, variable_table, labels, language),
          selected = complex_sample_selected_value(selected, paste0(prefix, "_strata"), defaults[[paste0(prefix, "_strata")]] %||% ""),
          selectize = FALSE
        ),
        selectInput(
          paste0(prefix, "_cluster"),
          complex_sample_ui_text("cluster", language),
          choices = complex_sample_design_role_choices("cluster", selected_names, all_names, variable_table, labels, language),
          selected = complex_sample_selected_value(selected, paste0(prefix, "_cluster"), defaults[[paste0(prefix, "_cluster")]] %||% ""),
          selectize = FALSE
        ),
        selectInput(
          paste0(prefix, "_weight"),
          complex_sample_ui_text("weight", language),
          choices = complex_sample_design_role_choices("weight", selected_names, all_names, variable_table, labels, language),
          selected = complex_sample_selected_value(selected, paste0(prefix, "_weight"), defaults[[paste0(prefix, "_weight")]] %||% ""),
          selectize = FALSE
        ),
        selectInput(
          subpopulation_id,
          complex_sample_ui_text("subpopulation", language),
          choices = complex_sample_subpopulation_choices(selected_names, all_names, variable_table, labels, language),
          selected = subpopulation_selected,
          selectize = FALSE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] && input['%s'] !== ''", subpopulation_id, subpopulation_id),
          div(
            class = "complex-sample-subpopulation-condition",
            selectInput(
              subpopulation_condition_type_id,
              complex_sample_ui_text("subpopulation_condition_type", language),
              choices = c(
                stats::setNames("equals", complex_sample_ui_text("value_match", language)),
                stats::setNames("not_equals", complex_sample_ui_text("value_not_match", language)),
                stats::setNames("not_missing", complex_sample_ui_text("non_missing", language)),
                stats::setNames("greater", complex_sample_ui_text("greater_than", language)),
                stats::setNames("greater_equal", complex_sample_ui_text("greater_equal", language)),
                stats::setNames("less", complex_sample_ui_text("less_than", language)),
                stats::setNames("less_equal", complex_sample_ui_text("less_equal", language)),
                stats::setNames("custom_formula", complex_sample_ui_text("custom_formula", language))
              ),
              selected = complex_sample_selected_value(selected, subpopulation_condition_type_id, "equals"),
              selectize = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] !== 'not_missing' && input['%s'] !== 'custom_formula'", subpopulation_condition_type_id, subpopulation_condition_type_id),
              textInput(
                subpopulation_condition_value_id,
                complex_sample_ui_text("subpopulation_value", language),
                value = complex_sample_selected_value(selected, subpopulation_condition_value_id, complex_sample_selected_value(selected, subpopulation_condition_id, subpopulation_condition_value_default)),
                width = "100%"
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'custom_formula'", subpopulation_condition_type_id),
              textInput(
                subpopulation_condition_id,
                complex_sample_ui_text("subpopulation_formula", language),
                value = complex_sample_selected_value(selected, subpopulation_condition_id, ""),
                placeholder = complex_sample_ui_text("subpopulation_placeholder", language),
                width = "100%"
              )
            )
          )
        )
      )
    )
  )
}

complex_sample_design_options_panel <- function(prefix, selected_names, all_names = selected_names, variable_table = NULL, labels = character(0), language = statedu_initial_language(), selected = list(), include_settings_buttons = FALSE) {
  selected_names <- as.character(selected_names %||% character(0))
  all_names <- as.character(all_names %||% selected_names)
  defaults <- complex_sample_design_defaults(prefix, selected_names, variable_table, labels)
  div(
    class = "complex-sample-design-column complex-sample-design-options-column",
    div(
      class = "analysis-options-panel ttest-anova-options regression-options complex-sample-design-panel",
      div(class = "analysis-options-title", complex_sample_ui_text("design_options_block", language)),
      div(
        class = "factor-options-tab-content ttest-anova-options-tab-content complex-sample-replicate-tab-content",
        selectInput(
          paste0(prefix, "_variance_method"),
          complex_sample_ui_text("variance_method", language),
          choices = c(
            "Auto" = "auto",
            "Taylor linearization" = "taylor",
            "BRR" = "brr",
            "Fay BRR" = "fay",
            "JK1" = "jk1",
            "JKn" = "jkn",
            "JK2" = "jk2",
            "Bootstrap" = "bootstrap"
          ),
          selected = selected[[paste0(prefix, "_variance_method")]] %||% "auto",
          selectize = FALSE
        ),
        selectInput(
          paste0(prefix, "_fpc"),
          complex_sample_ui_text("fpc", language),
          choices = complex_sample_design_choices(selected_names, all_names, variable_table, labels, "continuous", language),
          selected = complex_sample_selected_value(selected, paste0(prefix, "_fpc"), ""),
          selectize = FALSE
        ),
        selectInput(
          paste0(prefix, "_lonely_psu"),
          complex_sample_ui_text("lonely_psu", language),
          choices = c("Adjust" = "adjust", "Average" = "average", "Certainty" = "certainty", "Remove" = "remove", "Fail" = "fail"),
          selected = selected[[paste0(prefix, "_lonely_psu")]] %||% "adjust",
          selectize = FALSE
        ),
        checkboxInput(
          paste0(prefix, "_use_replicate_weights"),
          complex_sample_ui_text("use_replicate_weights", language),
          value = isTRUE(complex_sample_selected_value(selected, paste0(prefix, "_use_replicate_weights"), FALSE))
        ),
        div(class = "analysis-option-note complex-sample-replicate-note", complex_sample_ui_text("replicate_note", language)),
        conditionalPanel(
          condition = sprintf("input['%s'] === true", paste0(prefix, "_use_replicate_weights")),
          selectInput(
            paste0(prefix, "_replicate_weights"),
            complex_sample_ui_text("replicate_weights", language),
            choices = complex_sample_design_choices(selected_names, all_names, variable_table, labels, "continuous", language)[-1],
            selected = complex_sample_selected_value(selected, paste0(prefix, "_replicate_weights"), defaults[[paste0(prefix, "_replicate_weights")]] %||% character(0)),
            multiple = TRUE,
            selectize = FALSE
          ),
          selectInput(
            paste0(prefix, "_replicate_type"),
            complex_sample_ui_text("replicate_type", language),
            choices = c("Auto" = "auto", "BRR" = "brr", "Fay BRR" = "fay", "JK1" = "jk1", "JKn" = "jkn", "JK2" = "jk2", "Bootstrap" = "bootstrap"),
            selected = selected[[paste0(prefix, "_replicate_type")]] %||% "auto",
            selectize = FALSE
          ),
          checkboxInput(
            paste0(prefix, "_replicate_combined_weights"),
            complex_sample_ui_text("replicate_combined_weights", language),
            value = isTRUE(complex_sample_selected_value(selected, paste0(prefix, "_replicate_combined_weights"), FALSE))
          )
        ),
        if (isTRUE(include_settings_buttons)) {
          div(
            class = "complex-sample-design-settings-actions",
            actionButton("complex_design_load_settings", complex_sample_ui_text("load_design_settings", language), class = "btn btn-default"),
            actionButton("complex_design_save_settings", complex_sample_ui_text("save_design_settings", language), class = "btn btn-primary")
          )
        }
      )
    )
  )
}

complex_sample_setup_grid_class <- function(type) {
  switch(
    as.character(type %||% ""),
    frequencies = "frequencies-setup-grid complex-sample-setup-grid complex-sample-frequencies-setup-grid",
    correlation = "frequencies-setup-grid complex-sample-setup-grid complex-sample-correlation-setup-grid",
    crosstabs = "regression-setup-grid crosstab-setup-grid complex-sample-setup-grid complex-sample-crosstab-setup-grid",
    ttest_anova = "ttest-anova-setup-grid complex-sample-setup-grid complex-sample-ttest-setup-grid",
    "regression-setup-grid complex-sample-setup-grid complex-sample-regression-setup-grid"
  )
}

complex_sample_available_panel_class <- function(type) {
  switch(
    as.character(type %||% ""),
    crosstabs = "analysis-transfer-column analysis-transfer-panel regression-available-panel crosstab-available-panel complex-sample-available-panel",
    regression = "analysis-transfer-column analysis-transfer-panel regression-available-panel complex-sample-available-panel",
    logistic = "analysis-transfer-column analysis-transfer-panel regression-available-panel complex-sample-available-panel",
    "analysis-transfer-column analysis-transfer-panel complex-sample-available-panel"
  )
}

complex_sample_available_measurements <- function(type) {
  switch(
    as.character(type %||% ""),
    frequencies = analysis_allowed_measurements_all(),
    correlation = c("continuous", "ordered"),
    crosstabs = crosstab_allowed_measurements(),
    character(0)
  )
}

complex_sample_transfer_controls_class <- function(type) {
  switch(
    as.character(type %||% ""),
    crosstabs = "analysis-transfer-controls regression-transfer-controls crosstab-transfer-controls complex-sample-transfer-controls",
    ttest_anova = "analysis-transfer-controls ttest-anova-transfer-controls complex-sample-transfer-controls",
    frequencies = "analysis-transfer-controls complex-sample-transfer-controls",
    "analysis-transfer-controls regression-transfer-controls complex-sample-transfer-controls"
  )
}

complex_sample_target_column_class <- function(type) {
  switch(
    as.character(type %||% ""),
    crosstabs = "regression-target-column crosstab-target-column complex-sample-target-column",
    ttest_anova = "ttest-anova-target-column complex-sample-target-column",
    "regression-target-column complex-sample-target-column"
  )
}

complex_sample_target_panel_class <- function(type, key) {
  type <- as.character(type %||% "")
  key <- as.character(key %||% "")
  if (identical(type, "frequencies")) {
    return("analysis-transfer-column analysis-transfer-panel complex-sample-target-panel complex-sample-frequency-selected-panel")
  }
  if (identical(type, "correlation")) {
    return("analysis-transfer-column analysis-transfer-panel complex-sample-target-panel complex-sample-correlation-selected-panel complex-sample-frequency-selected-panel")
  }
  if (identical(type, "crosstabs")) {
    if (identical(key, "column")) {
      return("analysis-transfer-column analysis-transfer-panel regression-dependent-panel crosstab-column-panel complex-sample-target-panel")
    }
    return("analysis-transfer-column analysis-transfer-panel regression-independent-panel crosstab-row-panel complex-sample-target-panel")
  }
  if (identical(type, "ttest_anova")) {
    if (identical(key, "dependent")) {
      return("analysis-transfer-column analysis-transfer-panel ttest-anova-dependent-panel complex-sample-target-panel")
    }
    return("analysis-transfer-column analysis-transfer-panel ttest-anova-factor-panel complex-sample-target-panel")
  }
  if (identical(key, "outcome")) {
    return("analysis-transfer-column analysis-transfer-panel regression-dependent-panel complex-sample-target-panel")
  }
  "analysis-transfer-column analysis-transfer-panel regression-independent-panel complex-sample-target-panel"
}

complex_sample_assign_button_class <- function(type, key) {
  extra <- switch(
    paste(as.character(type %||% ""), as.character(key %||% ""), sep = ":"),
    "crosstabs:column" = " crosstab-assign-col-button",
    "crosstabs:row" = " crosstab-assign-row-button",
    ""
  )
  paste0("btn btn-default analysis-move-button", extra)
}

complex_sample_order_actions_class <- function(type, key) {
  type <- as.character(type %||% "")
  key <- as.character(key %||% "")
  if (identical(type, "frequencies")) return("analysis-order-actions frequency-order-actions")
  if (identical(type, "crosstabs")) return("analysis-order-actions crosstab-order-actions")
  if (identical(type, "ttest_anova")) return("analysis-order-actions ttest-anova-order-actions")
  if (identical(key, "outcome")) return("dependent-order-actions")
  "predictor-order-actions"
}

complex_sample_target_panel <- function(prefix, type, spec, values, variable_table = NULL, labels = character(0), language = statedu_initial_language(), selected = list()) {
  key <- spec$key
  input_id <- paste0(prefix, "_", key)
  allowed <- analysis_allowed_variables(values, variable_table, spec$measurements %||% analysis_allowed_measurements_all())
  div(
    class = complex_sample_target_panel_class(type, key),
    analysis_field_label_tag(spec$label, spec$measurements %||% analysis_allowed_measurements_all(), language = language),
    analysis_transfer_listbox_input(
      input_id,
      analysis_variable_items(allowed, variable_table, labels),
      selected = selected[[input_id]] %||% character(0),
      size = spec$size %||% 5
    ),
    div(
      class = complex_sample_order_actions_class(type, key),
      actionButton(paste0(prefix, "_", key, "_up"), analysis_ui_text("Up", language), class = "btn-default btn-sm"),
      actionButton(paste0(prefix, "_", key, "_down"), analysis_ui_text("Down", language), class = "btn-default btn-sm")
    )
  )
}

complex_sample_available_variables <- function(selected_names, target_specs, variable_table = NULL) {
  measurements <- unique(unlist(lapply(target_specs, function(spec) {
    spec$measurements %||% analysis_allowed_measurements_all()
  }), use.names = FALSE))
  if (length(measurements) == 0) {
    return(selected_names)
  }
  analysis_allowed_variables(selected_names, variable_table, measurements)
}

complex_sample_setup_panel <- function(prefix, selected_names, all_names = selected_names, target_specs, target_values, variable_table = NULL, labels = character(0), language = statedu_initial_language(), selected = list(), analysis_type = NULL, show_design_tabs = FALSE) {
  selected_names <- as.character(selected_names %||% character(0))
  all_names <- as.character(all_names %||% selected_names)
  analysis_type <- as.character(analysis_type %||% "")
  target_values <- target_values %||% list()
  assigned <- unique(unlist(target_values, use.names = FALSE))
  design_assigned <- complex_sample_design_assigned_variables(prefix, selected_names, variable_table, labels, selected)
  available_scope <- complex_sample_available_variables(selected_names, target_specs, variable_table)
  available <- setdiff(available_scope, unique(c(assigned, design_assigned)))
  available_id <- paste0(prefix, "_available")

  target_panels <- lapply(target_specs, function(spec) {
    key <- spec$key
    values <- intersect(as.character(target_values[[key]] %||% character(0)), selected_names)
    complex_sample_target_panel(prefix, analysis_type, spec, values, variable_table, labels, language, selected)
  })
  target_column <- if (length(target_panels) == 1 && analysis_type %in% c("frequencies", "correlation")) {
    target_panels[[1]]
  } else {
    div(class = complex_sample_target_column_class(analysis_type), target_panels)
  }

  div(
    class = complex_sample_setup_grid_class(analysis_type),
    div(
      class = complex_sample_available_panel_class(analysis_type),
      analysis_field_label_tag("Variables", complex_sample_available_measurements(analysis_type), language = language),
      analysis_transfer_listbox_input(
        available_id,
        analysis_variable_items(available, variable_table, labels),
        selected = selected[[available_id]] %||% character(0),
        size = 17
      )
    ),
    div(
      class = complex_sample_transfer_controls_class(analysis_type),
      lapply(target_specs, function(spec) {
        actionButton(paste0(prefix, "_assign_", spec$key), ">", class = complex_sample_assign_button_class(analysis_type, spec$key))
      })
    ),
    target_column,
    if (isTRUE(show_design_tabs)) {
      complex_sample_design_panel(prefix, selected_names, all_names, variable_table, labels, language, selected, analysis_type)
    } else {
      tagList(
        div(
          class = "complex-sample-hidden-design-inputs",
          style = "display:none;",
          complex_sample_design_panel(prefix, selected_names, all_names, variable_table, labels, language, selected, analysis_type, include_analysis_options = FALSE)
        ),
        complex_sample_analysis_options_panel(prefix, analysis_type, selected, language)
      )
    }
  )
}

complex_sample_design_setup_panel <- function(prefix, variable_names, variable_table = NULL, labels = character(0), language = statedu_initial_language(), selected = list()) {
  variable_names <- as.character(variable_names %||% character(0))
  tagList(
    div(
      class = "complex-sample-design-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel complex-sample-design-variable-panel",
        analysis_field_label_tag("Variables", analysis_allowed_measurements_all(), language = language),
        analysis_transfer_listbox_input(
          paste0(prefix, "_variables"),
          analysis_variable_items(variable_names, variable_table, labels),
          selected = character(0),
          size = 17
        )
      ),
      complex_sample_design_core_panel(prefix, variable_names, variable_names, variable_table, labels, language, selected),
      complex_sample_design_options_panel(prefix, variable_names, variable_names, variable_table, labels, language, selected, include_settings_buttons = FALSE)
    ),
    div(
      class = "complex-sample-design-action-row",
      div(
        class = "complex-sample-design-settings-actions",
        actionButton("complex_design_load_settings", complex_sample_ui_text("load_design_settings", language), class = "btn btn-default"),
        actionButton("complex_design_save_settings", complex_sample_ui_text("save_design_settings", language), class = "btn btn-primary")
      )
    )
  )
}

complex_sample_design_tab_panel <- function(language = statedu_initial_language()) {
  title <- complex_sample_ui_text("design_menu", language)
  tabPanel(
    title,
    value = "analysis_complex_design",
    div(
      class = "page-shell complex-sample-page-shell",
      div(
        class = "app-heading",
        h1(title),
        div(complex_sample_ui_text("design_subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel complex-sample-workspace-panel complex-sample-design-workspace-panel",
        div(
          class = "analysis-workspace-heading",
          h3(title)
        ),
        uiOutput("complex_design_setup")
      )
    )
  )
}

complex_sample_tab_panel <- function(title_key, value, prefix, setup_output_id, results_output_id, reset_output_id, language = statedu_initial_language()) {
  title <- complex_sample_ui_text(title_key, language)
  tabPanel(
    title,
    value = value,
    div(
      class = "page-shell complex-sample-page-shell",
      div(
        class = "app-heading",
        h1(title),
        div(complex_sample_ui_text("subtitle", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel complex-sample-workspace-panel",
        div(
          class = "analysis-workspace-heading",
          h3(title),
          conditionalPanel(
            condition = sprintf("output.%s_view_mode !== 'viewer'", prefix),
            analysis_data_viewer_button(paste0(prefix, "_view_data"), language)
          )
        ),
        analysis_workspace_body(
          prefix,
          uiOutput(setup_output_id),
          analysis_three_block_action_row(
            class = "frequencies-action-row complex-sample-action-row",
            run_button = actionButton(paste0(prefix, "_run"), complex_sample_ui_text("run", language), class = "btn btn-primary"),
            reset_control = uiOutput(reset_output_id)
          ),
          uiOutput(results_output_id)
        )
      )
    )
  )
}

complex_sample_frequencies_tab_panel <- function(language = statedu_initial_language()) {
  complex_sample_tab_panel("frequencies", "analysis_complex_frequencies", "complex_freq", "complex_freq_setup", "complex_freq_results", "complex_freq_reset_control", language)
}

complex_sample_crosstabs_tab_panel <- function(language = statedu_initial_language()) {
  complex_sample_tab_panel("crosstabs", "analysis_complex_crosstabs", "complex_crosstab", "complex_crosstab_setup", "complex_crosstab_results", "complex_crosstab_reset_control", language)
}

complex_sample_ttest_anova_tab_panel <- function(language = statedu_initial_language()) {
  complex_sample_tab_panel("ttest_anova", "analysis_complex_ttest_anova", "complex_ttest", "complex_ttest_setup", "complex_ttest_results", "complex_ttest_reset_control", language)
}

complex_sample_correlation_tab_panel <- function(language = statedu_initial_language()) {
  complex_sample_tab_panel("correlation", "analysis_complex_correlation", "complex_correlation", "complex_correlation_setup", "complex_correlation_results", "complex_correlation_reset_control", language)
}

complex_sample_regression_tab_panel <- function(language = statedu_initial_language()) {
  complex_sample_tab_panel("regression", "analysis_complex_regression", "complex_regression", "complex_regression_setup", "complex_regression_results", "complex_regression_reset_control", language)
}

complex_sample_logistic_tab_panel <- function(language = statedu_initial_language()) {
  complex_sample_tab_panel("logistic", "analysis_complex_logistic", "complex_logistic", "complex_logistic_setup", "complex_logistic_results", "complex_logistic_reset_control", language)
}

complex_sample_p_value <- function(value) {
  if (exists("format_p", mode = "function")) {
    return(format_p(value))
  }
  ifelse(is.na(value), NA_character_, formatC(value, format = "f", digits = 3))
}

complex_sample_num <- function(value, digits = 3) {
  if (length(value) == 0 || is.na(value[[1]])) {
    return(NA_character_)
  }
  formatC(as.numeric(value[[1]]), format = "f", digits = digits)
}

complex_sample_num_vec <- function(value, digits = 3) {
  vapply(value, complex_sample_num, character(1), digits = digits)
}

complex_sample_ci_range_text <- function(fit, scale = 1, digits = 2) {
  ci <- tryCatch(stats::confint(fit), error = function(e) NULL)
  if (is.null(ci) || length(ci) < 2) {
    return("")
  }
  values <- as.numeric(ci[1, 1:2]) * scale
  if (anyNA(values)) {
    return("")
  }
  paste0(complex_sample_num(values[[1]], digits), "-", complex_sample_num(values[[2]], digits))
}

complex_sample_design_df_text <- function(design_or_fit) {
  value <- tryCatch(survey::degf(design_or_fit), error = function(e) NA_real_)
  if (!is.finite(value)) {
    value <- tryCatch(stats::df.residual(design_or_fit), error = function(e) NA_real_)
  }
  if (!is.finite(value)) {
    return("")
  }
  complex_sample_num(value, 1)
}

complex_sample_design_df_value <- function(design_or_fit) {
  value <- tryCatch(survey::degf(design_or_fit), error = function(e) NA_real_)
  if (!is.finite(value)) {
    value <- tryCatch(stats::df.residual(design_or_fit), error = function(e) NA_real_)
  }
  suppressWarnings(as.numeric(value))
}

complex_sample_test_df_text <- function(test) {
  if (is.null(test)) {
    return("")
  }
  parameters <- suppressWarnings(as.numeric(test$parameter %||% numeric(0)))
  parameter_names <- names(test$parameter %||% numeric(0))
  if (length(parameters) > 0 && any(is.finite(parameters))) {
    values <- parameters[is.finite(parameters)]
    names(values) <- parameter_names[seq_along(parameters)][is.finite(parameters)]
    if (length(values) == 1L) {
      return(complex_sample_num(values[[1]], 1))
    }
    labels <- if (length(names(values)) == length(values) && any(nzchar(names(values)))) {
      paste0(names(values), "=", complex_sample_num_vec(values, 1))
    } else {
      complex_sample_num_vec(values, 1)
    }
    return(paste(labels, collapse = ", "))
  }
  df_values <- c(test$df, test$ddf, test$df.residual)
  df_values <- suppressWarnings(as.numeric(df_values))
  df_values <- df_values[is.finite(df_values)]
  if (length(df_values) == 0) {
    return("")
  }
  paste(complex_sample_num_vec(df_values, 1), collapse = ", ")
}

complex_sample_test_df_compact_text <- function(test) {
  if (is.null(test)) {
    return("")
  }
  values <- suppressWarnings(as.numeric(test$parameter %||% numeric(0)))
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    values <- suppressWarnings(as.numeric(c(test$df, test$ddf, test$df.residual)))
    values <- values[is.finite(values)]
  }
  if (length(values) == 0) {
    return("")
  }
  formatted <- complex_sample_num_vec(values, 1)
  formatted <- sub("\\.0$", "", formatted)
  paste(formatted, collapse = ",")
}

complex_sample_weighted_n_text <- function(design) {
  value <- tryCatch(sum(stats::weights(design), na.rm = TRUE), error = function(e) NA_real_)
  if (!is.finite(value)) {
    return("")
  }
  complex_sample_num(value, 2)
}

complex_sample_cv_text <- function(fit) {
  estimate <- tryCatch(as.numeric(fit)[[1]], error = function(e) NA_real_)
  se <- tryCatch(as.numeric(survey::SE(fit))[[1]], error = function(e) NA_real_)
  value <- if (is.finite(estimate) && estimate != 0 && is.finite(se)) abs(se / estimate) * 100 else NA_real_
  if (!is.finite(value)) {
    return("")
  }
  paste0(complex_sample_num(value, 1), "%")
}

complex_sample_deff_text <- function(fit) {
  value <- tryCatch(as.numeric(survey::deff(fit))[[1]], error = function(e) NA_real_)
  if (!is.finite(value)) {
    return("")
  }
  complex_sample_num(value, 2)
}

complex_sample_frequency_summary_column <- function() {
  "unweighted n(weighted %)"
}

complex_sample_estimated_summary_column <- function(mean_sd = FALSE) {
  if (isTRUE(mean_sd)) "M \u00B1 SD" else "M \u00B1 SE"
}

complex_sample_svyquantile_text <- function(formula, design, quantile = 0.5, digits = 2) {
  fit <- tryCatch(
    survey::svyquantile(formula, design, quantiles = quantile, na.rm = TRUE, ci = FALSE),
    error = function(e) NULL
  )
  value <- tryCatch(as.numeric(fit)[[1]], error = function(e) NA_real_)
  if (!is.finite(value)) {
    return("")
  }
  complex_sample_num(value, digits)
}

complex_sample_count_percent_text <- function(count, percent, weighted_n = NULL, percent_ci = NULL) {
  count <- if (length(count) == 0 || is.na(count)) "" else as.character(count)
  if (length(percent) == 0 || is.na(percent)) {
    return(count)
  }
  percent_text <- format_frequency_percent(percent, pad_under_10 = TRUE)
  value <- paste0(count, "(", percent_text, ")")
  percent_ci <- as.character(percent_ci %||% "")
  if (nzchar(percent_ci)) {
    value <- paste0(value, "; 95% CI=", percent_ci)
  }
  weighted_n <- suppressWarnings(as.numeric(weighted_n %||% NA_real_))
  if (is.finite(weighted_n)) {
    value <- paste0(value, "; wN=", complex_sample_num(weighted_n, 1))
  }
  value
}

complex_sample_estimate_se_text <- function(estimate, se, digits = 2, se_digits = 3) {
  paste0(complex_sample_num(estimate, digits), "\u00A0\u00B1\u00A0", complex_sample_num(se, se_digits))
}

complex_sample_estimate_sd_text <- function(estimate, sd, digits = 2, sd_digits = 2) {
  paste0(complex_sample_num(estimate, digits), "\u00A0\u00B1\u00A0", complex_sample_num(sd, sd_digits))
}

complex_sample_effect_size_text <- function(level_stats, test = NULL) {
  if (!is.data.frame(level_stats) || nrow(level_stats) < 2) {
    return("")
  }
  if (nrow(level_stats) == 2 && all(c("mean", "sd") %in% names(level_stats))) {
    means <- suppressWarnings(as.numeric(level_stats$mean))
    sds <- suppressWarnings(as.numeric(level_stats$sd))
    pooled <- sqrt(mean(sds^2, na.rm = TRUE))
    value <- if (is.finite(pooled) && pooled > 0) abs(diff(means)) / pooled else NA_real_
    if (is.finite(value)) {
      return(complex_sample_num(value, 3))
    }
  }
  statistic <- suppressWarnings(as.numeric(test$statistic %||% test$Ftest %||% NA_real_)[[1]])
  parameters <- suppressWarnings(as.numeric(test$parameter %||% numeric(0)))
  parameters <- parameters[is.finite(parameters)]
  if (length(parameters) >= 2 && is.finite(statistic)) {
    eta <- (statistic * parameters[[1]]) / (statistic * parameters[[1]] + parameters[[2]])
    if (is.finite(eta)) {
      return(paste0("\u03B7\u00B2=", complex_sample_num(eta, 3)))
    }
  }
  ""
}

complex_sample_pairwise_p_matrix <- function(design, dependent, factor_var, levels, adjust_method = "holm") {
  levels <- as.character(levels %||% character(0))
  levels <- levels[nzchar(levels)]
  matrix_out <- matrix(NA_real_, nrow = length(levels), ncol = length(levels), dimnames = list(levels, levels))
  if (length(levels) < 2L) {
    return(matrix_out)
  }
  pair_rows <- list()
  pair_index <- 0L
  for (i in seq_len(length(levels) - 1L)) {
    for (j in seq.int(i + 1L, length(levels))) {
      pair_index <- pair_index + 1L
      first <- levels[[i]]
      second <- levels[[j]]
      temp_design <- design
      group_values <- as.character(temp_design$variables[[factor_var]])
      temp_design$variables$`..pair_keep..` <- !is.na(temp_design$variables[[dependent]]) & !is.na(group_values) & group_values %in% c(first, second)
      pair_design <- tryCatch(subset(temp_design, `..pair_keep..`), error = function(e) NULL)
      p_value <- NA_real_
      if (!is.null(pair_design)) {
        pair_design$variables$`..pair_y..` <- suppressWarnings(as.numeric(pair_design$variables[[dependent]]))
        pair_design$variables$`..pair_group..` <- factor(as.character(pair_design$variables[[factor_var]]), levels = c(first, second))
        test <- tryCatch(survey::svyttest(`..pair_y..` ~ `..pair_group..`, pair_design), error = function(e) NULL)
        if (!is.null(test)) {
          p_value <- suppressWarnings(as.numeric((test$p.value %||% test$p %||% NA_real_)[[1]]))
        }
      }
      pair_rows[[length(pair_rows) + 1L]] <- data.frame(
        first = first,
        second = second,
        p = p_value,
        stringsAsFactors = FALSE
      )
    }
  }
  pair_table <- do.call(rbind, pair_rows)
  valid <- is.finite(pair_table$p)
  adjusted <- rep(NA_real_, nrow(pair_table))
  if (any(valid)) {
    adjusted[valid] <- stats::p.adjust(pair_table$p[valid], method = adjust_method)
  }
  for (row_index in seq_len(nrow(pair_table))) {
    first <- pair_table$first[[row_index]]
    second <- pair_table$second[[row_index]]
    matrix_out[first, second] <- adjusted[[row_index]]
    matrix_out[second, first] <- adjusted[[row_index]]
  }
  diag(matrix_out) <- NA_real_
  matrix_out
}

complex_sample_post_hoc_correction_label <- function(method = "holm") {
  method <- as.character(method %||% "holm")
  if (identical(method, "bonferroni")) {
    return("Bonferroni correction")
  }
  "Holm Bonferroni"
}

complex_sample_post_hoc_adjustment_note <- function(method = "holm") {
  method <- as.character(method %||% "holm")
  if (identical(method, "bonferroni")) {
    return("Bonferroni-corrected")
  }
  "Holm-Bonferroni-adjusted"
}

complex_sample_p_adjustment_note <- function(method = "holm") {
  method <- as.character(method %||% "holm")[[1]]
  switch(
    method,
    none = "unadjusted",
    bonferroni = "Bonferroni-corrected",
    BH = "FDR-adjusted (Benjamini-Hochberg)",
    "Holm-Bonferroni-adjusted"
  )
}

complex_sample_trend_test <- function(design, dependent, factor_var, levels) {
  levels <- as.character(levels %||% character(0))
  levels <- levels[nzchar(levels)]
  if (length(levels) < 3L) {
    return(NULL)
  }
  temp_design <- design
  group_values <- as.character(temp_design$variables[[factor_var]])
  temp_design$variables$`..trend_keep..` <- !is.na(temp_design$variables[[dependent]]) & !is.na(group_values) & group_values %in% levels
  trend_design <- tryCatch(subset(temp_design, `..trend_keep..`), error = function(e) NULL)
  if (is.null(trend_design)) {
    return(NULL)
  }
  trend_design$variables$`..trend_y..` <- suppressWarnings(as.numeric(trend_design$variables[[dependent]]))
  trend_design$variables$`..trend_score..` <- match(as.character(trend_design$variables[[factor_var]]), levels)
  fit <- tryCatch(survey::svyglm(`..trend_y..` ~ `..trend_score..`, design = trend_design), error = function(e) NULL)
  if (is.null(fit)) {
    return(NULL)
  }
  coefficients <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  if (is.null(coefficients) || !"..trend_score.." %in% rownames(coefficients)) {
    return(NULL)
  }
  p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(coefficients))
  p_value <- if (length(p_col) > 0) suppressWarnings(as.numeric(coefficients["..trend_score..", p_col[[1]]])) else NA_real_
  statistic_col <- intersect(c("t value", "z value"), colnames(coefficients))
  statistic <- if (length(statistic_col) > 0) suppressWarnings(as.numeric(coefficients["..trend_score..", statistic_col[[1]]])) else NA_real_
  list(method = "survey-weighted linear score trend test", statistic = statistic, p = p_value)
}

complex_sample_model_wald_text <- function(fit, terms) {
  terms <- as.character(terms %||% character(0))
  terms <- terms[nzchar(terms)]
  if (length(terms) == 0) {
    return(c(statistic = "", p = ""))
  }
  test <- tryCatch(
    survey::regTermTest(fit, stats::as.formula(paste("~", paste(terms, collapse = " + ")))),
    error = function(e) NULL
  )
  if (is.null(test)) {
    return(c(statistic = "", p = ""))
  }
  statistic <- suppressWarnings(as.numeric(test$Ftest %||% test$statistic %||% NA_real_)[[1]])
  p_value <- suppressWarnings(as.numeric(test$p %||% test$p.value %||% NA_real_)[[1]])
  c(
    statistic = if (is.finite(statistic)) complex_sample_num(statistic, 3) else "",
    df = complex_sample_test_df_text(test),
    p = if (is.finite(p_value)) complex_sample_p_value(p_value) else ""
  )
}

complex_sample_weighted_r_squared <- function(fit, design, response = NULL) {
  y <- suppressWarnings(as.numeric(response %||% numeric(0)))
  if (length(y) == 0) {
    y <- tryCatch(as.numeric(stats::model.response(stats::model.frame(fit))), error = function(e) numeric(0))
  }
  fitted <- tryCatch(as.numeric(stats::fitted(fit)), error = function(e) numeric(0))
  weights <- tryCatch(as.numeric(stats::weights(design)), error = function(e) numeric(0))
  keep <- is.finite(y) & is.finite(fitted) & is.finite(weights) & weights > 0
  if (!any(keep)) {
    return(c(r2 = NA_real_, adj_r2 = NA_real_))
  }
  y <- y[keep]
  fitted <- fitted[keep]
  weights <- weights[keep]
  y_bar <- stats::weighted.mean(y, weights)
  sse <- sum(weights * (y - fitted)^2)
  sst <- sum(weights * (y - y_bar)^2)
  r2 <- if (is.finite(sst) && sst > 0) 1 - sse / sst else NA_real_
  n <- length(y)
  p <- length(stats::coef(fit)) - 1L
  adj <- if (is.finite(r2) && n > p + 1L) 1 - (1 - r2) * (n - 1L) / (n - p - 1L) else NA_real_
  c(r2 = r2, adj_r2 = adj)
}

complex_sample_logistic_pseudo_r_squared <- function(fit, design, response = NULL) {
  y <- suppressWarnings(as.numeric(response %||% numeric(0)))
  if (length(y) == 0) {
    y <- tryCatch(as.numeric(stats::model.response(stats::model.frame(fit))), error = function(e) numeric(0))
  }
  fitted <- tryCatch(as.numeric(stats::fitted(fit)), error = function(e) numeric(0))
  weights <- tryCatch(as.numeric(stats::weights(design)), error = function(e) numeric(0))
  keep <- is.finite(y) & is.finite(fitted) & is.finite(weights) & weights > 0
  if (!any(keep)) {
    return(c(mcfadden = NA_real_, nagelkerke = NA_real_))
  }
  y <- y[keep]
  fitted <- pmin(pmax(fitted[keep], 1e-8), 1 - 1e-8)
  weights <- weights[keep]
  p_null <- pmin(pmax(stats::weighted.mean(y, weights), 1e-8), 1 - 1e-8)
  ll_full <- sum(weights * (y * log(fitted) + (1 - y) * log(1 - fitted)))
  ll_null <- sum(weights * (y * log(p_null) + (1 - y) * log(1 - p_null)))
  n <- sum(weights)
  mcfadden <- if (is.finite(ll_null) && ll_null != 0) 1 - ll_full / ll_null else NA_real_
  cox_snell <- if (is.finite(n) && n > 0) 1 - exp((2 / n) * (ll_null - ll_full)) else NA_real_
  max_cox_snell <- if (is.finite(n) && n > 0) 1 - exp((2 / n) * ll_null) else NA_real_
  nagelkerke <- if (is.finite(max_cox_snell) && max_cox_snell > 0) cox_snell / max_cox_snell else NA_real_
  c(mcfadden = mcfadden, nagelkerke = nagelkerke)
}

complex_sample_backtick <- function(name) {
  paste0("`", gsub("`", "\\`", as.character(name), fixed = TRUE), "`")
}

complex_sample_formula_text <- function(response, predictors) {
  paste(
    complex_sample_backtick(response),
    "~",
    paste(vapply(predictors, complex_sample_backtick, character(1)), collapse = " + ")
  )
}

complex_sample_variance_type <- function(method, fallback = "JK1") {
  switch(
    tolower(as.character(method %||% "")),
    brr = "BRR",
    fay = "Fay",
    jk1 = "JK1",
    jkn = "JKn",
    jk2 = "JK2",
    bootstrap = "bootstrap",
    fallback
  )
}

complex_sample_variance_method_label <- function(method = "auto") {
  method <- tolower(as.character(method %||% "auto")[[1]])
  switch(
    method,
    auto = "Auto",
    taylor = "Taylor linearization",
    brr = "BRR",
    fay = "Fay BRR",
    jk1 = "JK1",
    jkn = "JKn",
    jk2 = "JK2",
    bootstrap = "Bootstrap",
    method
  )
}

complex_sample_validate_replicate_conversion <- function(type, design) {
  type <- as.character(type %||% "")
  strata <- as.character(design$strata %||% "")[[1]]
  if (identical(type, "JK1") && nzchar(strata)) {
    shiny::validate(shiny::need(
      FALSE,
      "JK1 is only valid for unstratified jackknife conversion. Use JKn for a stratified complex-sample design."
    ))
  }
}

complex_sample_shared_design_defaults <- function() {
  list(
    strata = "",
    cluster = "",
    weight = "",
    fpc = "",
    variance_method = "auto",
    lonely_psu = "adjust",
    use_replicate_weights = FALSE,
    replicate_weights = character(0),
    replicate_type = "auto",
    replicate_combined_weights = FALSE,
    subpopulation = "",
    subpopulation_condition = "",
    subpopulation_condition_type = "equals",
    subpopulation_condition_value = ""
  )
}

complex_sample_normalize_design_state <- function(values = NULL) {
  defaults <- complex_sample_shared_design_defaults()
  values <- values %||% list()
  for (name in names(defaults)) {
    if (!name %in% names(values) || is.null(values[[name]])) {
      values[[name]] <- defaults[[name]]
    }
  }
  values$strata <- as.character(values$strata %||% "")[[1]]
  values$cluster <- as.character(values$cluster %||% "")[[1]]
  values$weight <- as.character(values$weight %||% "")[[1]]
  values$fpc <- as.character(values$fpc %||% "")[[1]]
  values$variance_method <- as.character(values$variance_method %||% "auto")[[1]]
  values$lonely_psu <- complex_sample_lonely_psu_value(values$lonely_psu)
  values$use_replicate_weights <- isTRUE(values$use_replicate_weights)
  values$replicate_weights <- as.character(values$replicate_weights %||% character(0))
  values$replicate_type <- as.character(values$replicate_type %||% "auto")[[1]]
  values$replicate_combined_weights <- isTRUE(values$replicate_combined_weights)
  values$subpopulation <- as.character(values$subpopulation %||% "")[[1]]
  values$subpopulation_condition <- as.character(values$subpopulation_condition %||% "")[[1]]
  values$subpopulation_condition_type <- as.character(values$subpopulation_condition_type %||% "equals")[[1]]
  values$subpopulation_condition_value <- as.character(values$subpopulation_condition_value %||% "")[[1]]
  values[names(defaults)]
}

complex_sample_design_input_ids <- function(prefix) {
  c(
    strata = paste0(prefix, "_strata"),
    cluster = paste0(prefix, "_cluster"),
    weight = paste0(prefix, "_weight"),
    fpc = paste0(prefix, "_fpc"),
    variance_method = paste0(prefix, "_variance_method"),
    lonely_psu = paste0(prefix, "_lonely_psu"),
    use_replicate_weights = paste0(prefix, "_use_replicate_weights"),
    replicate_weights = paste0(prefix, "_replicate_weights"),
    replicate_type = paste0(prefix, "_replicate_type"),
    replicate_combined_weights = paste0(prefix, "_replicate_combined_weights"),
    subpopulation = paste0(prefix, "_subpopulation"),
    subpopulation_condition = paste0(prefix, "_subpopulation_condition"),
    subpopulation_condition_type = paste0(prefix, "_subpopulation_condition_type"),
    subpopulation_condition_value = paste0(prefix, "_subpopulation_condition_value")
  )
}

complex_sample_read_design_inputs <- function(input, prefix) {
  complex_sample_normalize_design_state(list(
    strata = input[[paste0(prefix, "_strata")]] %||% "",
    cluster = input[[paste0(prefix, "_cluster")]] %||% "",
    weight = input[[paste0(prefix, "_weight")]] %||% "",
    fpc = input[[paste0(prefix, "_fpc")]] %||% "",
    variance_method = input[[paste0(prefix, "_variance_method")]] %||% "auto",
    lonely_psu = input[[paste0(prefix, "_lonely_psu")]] %||% "adjust",
    use_replicate_weights = isTRUE(input[[paste0(prefix, "_use_replicate_weights")]] %||% FALSE),
    replicate_weights = input[[paste0(prefix, "_replicate_weights")]] %||% character(0),
    replicate_type = input[[paste0(prefix, "_replicate_type")]] %||% "auto",
    replicate_combined_weights = isTRUE(input[[paste0(prefix, "_replicate_combined_weights")]] %||% FALSE),
    subpopulation = input[[paste0(prefix, "_subpopulation")]] %||% "",
    subpopulation_condition = input[[paste0(prefix, "_subpopulation_condition")]] %||% "",
    subpopulation_condition_type = input[[paste0(prefix, "_subpopulation_condition_type")]] %||% "equals",
    subpopulation_condition_value = input[[paste0(prefix, "_subpopulation_condition_value")]] %||% ""
  ))
}

complex_sample_update_design_inputs <- function(session, prefix, values) {
  values <- complex_sample_normalize_design_state(values)
  updateSelectInput(session, paste0(prefix, "_strata"), selected = values$strata)
  updateSelectInput(session, paste0(prefix, "_cluster"), selected = values$cluster)
  updateSelectInput(session, paste0(prefix, "_weight"), selected = values$weight)
  updateSelectInput(session, paste0(prefix, "_fpc"), selected = values$fpc)
  updateSelectInput(session, paste0(prefix, "_variance_method"), selected = values$variance_method)
  updateSelectInput(session, paste0(prefix, "_lonely_psu"), selected = values$lonely_psu)
  updateCheckboxInput(session, paste0(prefix, "_use_replicate_weights"), value = values$use_replicate_weights)
  updateSelectInput(session, paste0(prefix, "_replicate_weights"), selected = values$replicate_weights)
  updateSelectInput(session, paste0(prefix, "_replicate_type"), selected = values$replicate_type)
  updateCheckboxInput(session, paste0(prefix, "_replicate_combined_weights"), value = values$replicate_combined_weights)
  updateSelectInput(session, paste0(prefix, "_subpopulation"), selected = values$subpopulation)
  updateSelectInput(session, paste0(prefix, "_subpopulation_condition_type"), selected = values$subpopulation_condition_type)
  updateTextInput(session, paste0(prefix, "_subpopulation_condition"), value = values$subpopulation_condition)
  updateTextInput(session, paste0(prefix, "_subpopulation_condition_value"), value = values$subpopulation_condition_value)
}

complex_sample_design_error <- function(error) {
  message <- conditionMessage(error)
  shiny::validate(shiny::need(FALSE, paste("Complex-sample design could not be created:", message)))
}

complex_sample_design_inputs <- function(input, prefix) {
  list(
    strata = as.character(input[[paste0(prefix, "_strata")]] %||% ""),
    cluster = as.character(input[[paste0(prefix, "_cluster")]] %||% ""),
    weight = as.character(input[[paste0(prefix, "_weight")]] %||% ""),
    fpc = as.character(input[[paste0(prefix, "_fpc")]] %||% ""),
    variance_method = as.character(input[[paste0(prefix, "_variance_method")]] %||% "auto"),
    lonely_psu = as.character(input[[paste0(prefix, "_lonely_psu")]] %||% "adjust"),
    use_replicate_weights = isTRUE(input[[paste0(prefix, "_use_replicate_weights")]] %||% FALSE),
    replicate_weights = as.character(input[[paste0(prefix, "_replicate_weights")]] %||% character(0)),
    replicate_type = as.character(input[[paste0(prefix, "_replicate_type")]] %||% "auto"),
    replicate_combined_weights = isTRUE(input[[paste0(prefix, "_replicate_combined_weights")]] %||% FALSE),
    subpopulation = as.character(input[[paste0(prefix, "_subpopulation")]] %||% ""),
    subpopulation_condition = as.character(input[[paste0(prefix, "_subpopulation_condition")]] %||% ""),
    subpopulation_condition_type = as.character(input[[paste0(prefix, "_subpopulation_condition_type")]] %||% "equals"),
    subpopulation_condition_value = as.character(input[[paste0(prefix, "_subpopulation_condition_value")]] %||% "")
  )
}

complex_sample_lonely_psu_value <- function(value = "adjust") {
  value <- as.character(value %||% "adjust")[[1]]
  if (!value %in% c("adjust", "average", "certainty", "remove", "fail")) {
    value <- "adjust"
  }
  value
}

complex_sample_lonely_psu_label <- function(value = "adjust") {
  value <- complex_sample_lonely_psu_value(value)
  switch(
    value,
    adjust = "adjust",
    average = "average",
    certainty = "certainty",
    remove = "remove",
    fail = "fail",
    value
  )
}

complex_sample_evaluate_subpopulation_formula <- function(data, variable, formula_text) {
  formula_text <- trimws(as.character(formula_text %||% ""))
  shiny::validate(shiny::need(nzchar(formula_text), "Enter a user formula for the subpopulation condition."))
  value <- data[[variable]]
  expression_text <- formula_text
  if (grepl("^(==|!=|>=|<=|>|<|%in%|&|\\|)", expression_text)) {
    expression_text <- paste(".value", expression_text)
  }
  expr <- tryCatch(parse(text = expression_text)[[1]], error = function(e) e)
  if (inherits(expr, "error")) {
    shiny::validate(shiny::need(FALSE, paste("Subpopulation formula could not be parsed:", conditionMessage(expr))))
  }
  eval_env <- list2env(as.list(data), parent = baseenv())
  eval_env$value <- value
  eval_env$.value <- value
  result <- tryCatch(eval(expr, eval_env), error = function(e) e)
  if (inherits(result, "error")) {
    shiny::validate(shiny::need(FALSE, paste("Subpopulation formula could not be evaluated:", conditionMessage(result))))
  }
  shiny::validate(
    shiny::need(is.logical(result) || is.numeric(result), "Subpopulation formula must return TRUE/FALSE values."),
    shiny::need(length(result) %in% c(1L, nrow(data)), "Subpopulation formula must return one value or one value per row.")
  )
  if (length(result) == 1L) {
    result <- rep(result, nrow(data))
  }
  if (is.numeric(result)) {
    result <- result != 0
  }
  result %in% TRUE
}

complex_sample_subpopulation_keep <- function(data, variable, condition = "", condition_type = "equals", condition_value = "") {
  variable <- as.character(variable %||% "")
  if (!nzchar(variable) || !variable %in% names(data)) {
    return(rep(TRUE, nrow(data)))
  }
  values <- data[[variable]]
  condition <- trimws(as.character(condition %||% ""))
  condition_type <- as.character(condition_type %||% "equals")[[1]]
  condition_value <- trimws(as.character(condition_value %||% ""))
  if (identical(condition_type, "custom_formula")) {
    keep <- complex_sample_evaluate_subpopulation_formula(data, variable, condition)
    shiny::validate(shiny::need(any(keep, na.rm = TRUE), "Subpopulation formula selected no rows."))
    return(keep)
  }
  if (nzchar(condition) && !nzchar(condition_value)) {
    condition_value <- condition
  }
  if (!nzchar(condition_value) && !identical(condition_type, "not_missing")) {
    keep <- !is.na(values)
    if (is.logical(values)) {
      return(keep & values)
    }
    if (is.numeric(values) || is.integer(values)) {
      return(keep & values == 1)
    }
    return(keep & nzchar(as.character(values)))
  }

  keep <- switch(
    condition_type,
    not_missing = !is.na(values),
    not_equals = !is.na(values) & as.character(values) != condition_value,
    greater = {
      threshold <- suppressWarnings(as.numeric(condition_value))
      shiny::validate(shiny::need(is.finite(threshold), "Subpopulation value must be numeric for this condition."))
      suppressWarnings(as.numeric(values)) > threshold
    },
    greater_equal = {
      threshold <- suppressWarnings(as.numeric(condition_value))
      shiny::validate(shiny::need(is.finite(threshold), "Subpopulation value must be numeric for this condition."))
      suppressWarnings(as.numeric(values)) >= threshold
    },
    less = {
      threshold <- suppressWarnings(as.numeric(condition_value))
      shiny::validate(shiny::need(is.finite(threshold), "Subpopulation value must be numeric for this condition."))
      suppressWarnings(as.numeric(values)) < threshold
    },
    less_equal = {
      threshold <- suppressWarnings(as.numeric(condition_value))
      shiny::validate(shiny::need(is.finite(threshold), "Subpopulation value must be numeric for this condition."))
      suppressWarnings(as.numeric(values)) <= threshold
    },
    {
      shiny::validate(shiny::need(nzchar(condition_value), "Enter a subpopulation value."))
      !is.na(values) & as.character(values) == condition_value
    }
  )
  shiny::validate(
    shiny::need(is.logical(keep), "Subpopulation condition must return TRUE/FALSE values."),
    shiny::need(length(keep) == nrow(data), "Subpopulation condition must return one TRUE/FALSE value per row."),
    shiny::need(any(keep, na.rm = TRUE), "Subpopulation condition selected no rows.")
  )
  keep %in% TRUE
}

complex_sample_formula <- function(variables) {
  variables <- as.character(variables %||% character(0))
  if (length(variables) == 0 || !nzchar(variables[[1]])) {
    return(NULL)
  }
  stats::reformulate(variables)
}

complex_sample_design_formula <- function(variable, frame) {
  variable <- as.character(variable %||% "")
  if (length(variable) == 0 || !nzchar(variable[[1]]) || !variable[[1]] %in% names(frame)) {
    return(NULL)
  }
  complex_sample_formula(variable[[1]])
}

complex_sample_cluster_formula <- function(variable, frame) {
  formula <- complex_sample_design_formula(variable, frame)
  if (is.null(formula)) {
    return(~1)
  }
  formula
}

complex_sample_design_missing_values <- function(values) {
  if (is.factor(values)) {
    values <- as.character(values)
  }
  if (is.character(values)) {
    return(is.na(values) | !nzchar(trimws(values)))
  }
  is.na(values)
}

complex_sample_design_preprocess <- function(frame, design) {
  n <- nrow(frame)
  design_missing <- rep(FALSE, n)
  selected_design_variables <- intersect(
    c(design$strata, design$cluster, if (!isTRUE(design$use_replicate_weights)) design$fpc else ""),
    names(frame)
  )
  for (variable in selected_design_variables) {
    design_missing <- design_missing | complex_sample_design_missing_values(frame[[variable]])
  }

  weight_invalid <- rep(FALSE, n)
  if (nzchar(design$weight) && design$weight %in% names(frame)) {
    weight_values <- suppressWarnings(as.numeric(frame[[design$weight]]))
    weight_invalid <- is.na(weight_values) | !is.finite(weight_values) | weight_values <= 0
  }

  fpc_invalid <- rep(FALSE, n)
  if (!isTRUE(design$use_replicate_weights) && nzchar(design$fpc) && design$fpc %in% names(frame)) {
    fpc_values <- suppressWarnings(as.numeric(frame[[design$fpc]]))
    fpc_invalid <- !is.na(fpc_values) & (!is.finite(fpc_values) | fpc_values <= 0)
  }

  replicate_invalid <- rep(FALSE, n)
  if (isTRUE(design$use_replicate_weights)) {
    replicate_variables <- intersect(design$replicate_weights, names(frame))
    for (variable in replicate_variables) {
      values <- suppressWarnings(as.numeric(frame[[variable]]))
      replicate_invalid <- replicate_invalid | is.na(values) | !is.finite(values)
    }
  }

  keep <- !(design_missing | weight_invalid | fpc_invalid | replicate_invalid)
  list(
    frame = frame[keep, , drop = FALSE],
    keep = keep,
    meta = list(
      original_n = n,
      design_missing_n = sum(design_missing, na.rm = TRUE),
      invalid_weight_n = sum(weight_invalid, na.rm = TRUE),
      invalid_fpc_n = sum(fpc_invalid, na.rm = TRUE),
      invalid_replicate_weight_n = sum(replicate_invalid, na.rm = TRUE),
      design_excluded_n = sum(!keep, na.rm = TRUE),
      design_n = sum(keep, na.rm = TRUE)
    )
  )
}

complex_sample_subpopulation_missing_n <- function(data, variable) {
  variable <- as.character(variable %||% "")
  if (!nzchar(variable) || !variable %in% names(data)) {
    return(0L)
  }
  sum(is.na(data[[variable]]), na.rm = TRUE)
}

complex_sample_subpopulation_missing_mask <- function(data, variable) {
  variable <- as.character(variable %||% "")
  if (!nzchar(variable) || !variable %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }
  is.na(data[[variable]])
}

complex_sample_design_note <- function(built) {
  meta <- built$meta %||% list()
  notes <- character(0)
  original_n <- suppressWarnings(as.integer(meta$original_n %||% NA_integer_))
  design_n <- suppressWarnings(as.integer(meta$design_n %||% NA_integer_))
  analysis_n <- suppressWarnings(as.integer(meta$analysis_n %||% NA_integer_))
  design_excluded_n <- suppressWarnings(as.integer(meta$design_excluded_n %||% 0L))
  subpopulation_excluded_n <- suppressWarnings(as.integer(meta$subpopulation_excluded_n %||% 0L))
  subpopulation_missing_n <- suppressWarnings(as.integer(meta$subpopulation_missing_n %||% 0L))
  lonely_psu <- as.character(meta$lonely_psu %||% "adjust")[[1]]
  design_type <- as.character(meta$design_type %||% "taylor")[[1]]
  variance_method <- as.character(meta$variance_method %||% "auto")[[1]]
  replicate_count <- suppressWarnings(as.integer(meta$replicate_count %||% 0L))
  replicate_combined <- isTRUE(meta$replicate_combined_weights %||% FALSE)
  fpc_selected <- isTRUE(meta$fpc_selected %||% FALSE)
  fpc_used <- isTRUE(meta$fpc_used %||% FALSE)

  if (is.finite(original_n) && is.finite(analysis_n)) {
    notes <- c(notes, sprintf("Survey design was constructed from %s rows; final design N after exclusions/subpopulation was %s.", original_n, analysis_n))
  }
  if (identical(design_type, "replicate")) {
    notes <- c(notes, sprintf(
      "Variance estimation used replicate weights (%s; %s replicate variables; combined.weights=%s).",
      complex_sample_variance_method_label(variance_method),
      replicate_count,
      if (isTRUE(replicate_combined)) "TRUE" else "FALSE"
    ))
  } else if (identical(design_type, "converted_replicate")) {
    notes <- c(notes, sprintf("Variance estimation used %s replicate weights converted from the Taylor design.", complex_sample_variance_method_label(variance_method)))
  } else {
    notes <- c(notes, "Variance estimation used Taylor linearization.")
  }
  if (isTRUE(fpc_used)) {
    notes <- c(notes, "Finite population correction (FPC) was applied to the Taylor design.")
  } else if (isTRUE(fpc_selected) && identical(design_type, "replicate")) {
    notes <- c(notes, "Finite population correction (FPC) was not applied because explicit replicate weights were used.")
  }
  notes <- c(notes, sprintf("Single-PSU strata were handled using survey.lonely.psu = '%s'.", complex_sample_lonely_psu_label(lonely_psu)))
  if (is.finite(design_excluded_n) && design_excluded_n > 0) {
    notes <- c(notes, sprintf("Rows with missing strata/cluster/FPC variables, missing weights, non-positive weights, invalid FPC values, or invalid replicate weights were excluded before constructing the survey design (n=%s).", design_excluded_n))
  }
  if (is.finite(subpopulation_excluded_n) && subpopulation_excluded_n > 0) {
    notes <- c(notes, sprintf("Subpopulation analyses used the survey design subset method; rows outside the subpopulation were excluded from the analytic subset (n=%s).", subpopulation_excluded_n))
  }
  if (is.finite(subpopulation_missing_n) && subpopulation_missing_n > 0) {
    notes <- c(notes, sprintf("Rows with missing subpopulation variable values were not included in the subpopulation condition (n=%s).", subpopulation_missing_n))
  }
  paste(notes[nzchar(notes)], collapse = " ")
}

complex_sample_build_design_from_spec <- function(data, design, variables = character(0), strict = FALSE) {
  shiny::validate(shiny::need(requireNamespace("survey", quietly = TRUE), "Install the survey package to run complex-sample analyses."))
  design <- complex_sample_normalize_design_state(design)
  analysis_variables <- unique(as.character(variables %||% character(0)))
  design_variables <- unique(c(
    design$strata,
    design$cluster,
    design$weight,
    if (!isTRUE(design$use_replicate_weights)) design$fpc else "",
    if (isTRUE(design$use_replicate_weights)) design$replicate_weights else character(0),
    design$subpopulation
  ))
  requested_variables <- unique(c(analysis_variables, design_variables))
  requested_variables <- requested_variables[nzchar(requested_variables)]
  if (isTRUE(strict)) {
    missing_variables <- setdiff(requested_variables, names(data))
    shiny::validate(shiny::need(
      length(missing_variables) == 0L,
      paste("Variables saved in the model or complex-sample design are missing from the current data:", paste(missing_variables, collapse = ", "))
    ))
    if (isTRUE(design$use_replicate_weights)) {
      shiny::validate(shiny::need(length(design$replicate_weights) > 0L, "Select at least one replicate-weight variable."))
    }
  }
  variables <- requested_variables
  variables <- intersect(variables[nzchar(variables)], names(data))
  shiny::validate(shiny::need(length(variables) > 0, "No usable variables were selected."))
  frame <- as.data.frame(data[, variables, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)
  preprocessing <- complex_sample_design_preprocess(frame, design)
  frame <- preprocessing$frame
  preprocessing$meta$lonely_psu <- complex_sample_lonely_psu_value(design$lonely_psu)
  preprocessing$meta$design_type <- "taylor"
  preprocessing$meta$variance_method <- "taylor"
  preprocessing$meta$replicate_count <- 0L
  preprocessing$meta$replicate_combined_weights <- FALSE
  preprocessing$meta$fpc_selected <- nzchar(design$fpc) && design$fpc %in% names(frame)
  preprocessing$meta$fpc_used <- !isTRUE(design$use_replicate_weights) && nzchar(design$fpc) && design$fpc %in% names(frame)
  shiny::validate(shiny::need(nrow(frame) > 0, "No rows remain after excluding missing design variables or invalid weights."))

  if (isTRUE(design$use_replicate_weights) && length(intersect(design$replicate_weights, names(frame))) > 0) {
    repweights <- frame[, intersect(design$replicate_weights, names(frame)), drop = FALSE]
    type <- if (identical(tolower(design$replicate_type %||% "auto"), "auto")) {
      complex_sample_variance_type(design$variance_method, "JK1")
    } else {
      complex_sample_variance_type(design$replicate_type, "JK1")
    }
    preprocessing$meta$design_type <- "replicate"
    preprocessing$meta$variance_method <- type
    preprocessing$meta$replicate_count <- ncol(repweights)
    preprocessing$meta$replicate_combined_weights <- isTRUE(design$replicate_combined_weights)
    preprocessing$meta$fpc_used <- FALSE
    sample_design <- tryCatch(
      survey::svrepdesign(
        data = frame,
        weights = complex_sample_design_formula(design$weight, frame),
        repweights = repweights,
        type = type,
        combined.weights = isTRUE(design$replicate_combined_weights)
      ),
      error = complex_sample_design_error
    )
  } else {
    sample_design <- tryCatch(
      survey::svydesign(
        ids = complex_sample_cluster_formula(design$cluster, frame),
        strata = complex_sample_design_formula(design$strata, frame),
        weights = complex_sample_design_formula(design$weight, frame),
        fpc = complex_sample_design_formula(design$fpc, frame),
        data = frame,
        nest = TRUE
      ),
      error = complex_sample_design_error
    )
    if (!tolower(design$variance_method %||% "auto") %in% c("auto", "taylor")) {
      type <- complex_sample_variance_type(design$variance_method, "JK1")
      complex_sample_validate_replicate_conversion(type, design)
      preprocessing$meta$design_type <- "converted_replicate"
      preprocessing$meta$variance_method <- type
      sample_design <- tryCatch(
        survey::as.svrepdesign(sample_design, type = type),
        error = complex_sample_design_error
      )
    }
  }

  keep <- complex_sample_subpopulation_keep(
    frame,
    design$subpopulation,
    design$subpopulation_condition,
    design$subpopulation_condition_type,
    design$subpopulation_condition_value
  )
  subpopulation_missing <- complex_sample_subpopulation_missing_mask(frame, design$subpopulation)
  subpopulation_missing_n <- sum(subpopulation_missing, na.rm = TRUE)
  subpopulation_excluded_n <- sum(!keep & !subpopulation_missing, na.rm = TRUE)
  if (!all(keep)) {
    sample_design <- subset(sample_design, keep)
  }
  meta <- preprocessing$meta
  meta$subpopulation_missing_n <- subpopulation_missing_n
  meta$subpopulation_excluded_n <- subpopulation_excluded_n
  meta$analysis_n <- nrow(as.data.frame(sample_design$variables, stringsAsFactors = FALSE, check.names = FALSE))
  list(
    design = sample_design,
    data = as.data.frame(sample_design$variables, stringsAsFactors = FALSE, check.names = FALSE),
    spec = design,
    meta = meta
  )
}

complex_sample_build_design <- function(data, input, prefix, variables = character(0)) {
  complex_sample_build_design_from_spec(
    data = data,
    design = complex_sample_design_inputs(input, prefix),
    variables = variables,
    strict = FALSE
  )
}

complex_sample_weighted_frequency <- function(design, data, variable, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  values <- data[[variable]]
  eligible_n <- length(values)
  missing_n <- sum(is.na(values))
  levels <- frequency_value_order(unique(as.character(values[!is.na(values)])))
  if (length(levels) == 0) {
    return(data.frame())
  }
  rows <- lapply(levels, function(level) {
    indicator <- ifelse(is.na(values), NA_real_, as.numeric(as.character(values) == level))
    temp_design <- design
    temp_design$variables$`..indicator..` <- indicator
    total <- survey::svytotal(~`..indicator..`, temp_design, na.rm = TRUE)
    weighted_n <- as.numeric(total)
    prop <- tryCatch(survey::svymean(~`..indicator..`, temp_design, na.rm = TRUE, deff = TRUE), error = function(e) NULL)
    unweighted <- sum(indicator == 1, na.rm = TRUE)
    data.frame(
      Name = variable,
      Variable = frequency_variable_display_name(variable, variable_info, labels, category_table),
      Value = frequency_value_display_labels(variable, level, category_table),
      N = unweighted,
      `Eligible N` = eligible_n,
      `Missing N` = missing_n,
      `Weighted N raw` = weighted_n,
      `Weighted N` = complex_sample_num(weighted_n, 2),
      SE = complex_sample_num(survey::SE(total), 3),
      `95% CI` = if (isTRUE(options$show_ci) && !is.null(prop)) complex_sample_ci_range_text(prop, scale = 100, digits = 1) else "",
      CV = if (isTRUE(options$show_precision) && !is.null(prop)) complex_sample_cv_text(prop) else "",
      Deff = if (isTRUE(options$show_precision) && !is.null(prop)) complex_sample_deff_text(prop) else "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  weighted_total <- suppressWarnings(sum(as.numeric(out[["Weighted N raw"]]), na.rm = TRUE))
  weighted_percent <- if (is.finite(weighted_total) && weighted_total > 0) {
    as.numeric(out[["Weighted N raw"]]) / weighted_total * 100
  } else {
    rep(NA_real_, nrow(out))
  }
  out$Percent <- vapply(weighted_percent, format_frequency_percent, character(1))
  out[[complex_sample_frequency_summary_column()]] <- mapply(
    complex_sample_count_percent_text,
    out$N,
    weighted_percent,
    USE.NAMES = FALSE
  )
  out
}

complex_sample_weighted_descriptive <- function(design, data, variable, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  temp_design <- design
  temp_design$variables$`..y..` <- suppressWarnings(as.numeric(data[[variable]]))
  values <- temp_design$variables$`..y..`
  observed <- values[!is.na(values)]
  if (length(observed) == 0) {
    return(data.frame())
  }
  mean_fit <- survey::svymean(~`..y..`, temp_design, na.rm = TRUE, deff = TRUE)
  var_fit <- survey::svyvar(~`..y..`, temp_design, na.rm = TRUE)
  eligible_n <- length(values)
  missing_n <- sum(is.na(values))
  estimate <- as.numeric(mean_fit)
  se <- survey::SE(mean_fit)
  out <- data.frame(
    Name = variable,
    Variable = frequency_variable_display_name(variable, variable_info, labels, category_table),
    N = length(observed),
    `Eligible N` = eligible_n,
    `Missing N` = missing_n,
    Mean = complex_sample_num(estimate, 2),
    SE = complex_sample_num(se, 3),
    Estimated = complex_sample_estimate_se_text(estimate, se),
    Median = if (isTRUE(options$show_median)) complex_sample_svyquantile_text(~`..y..`, temp_design, 0.5, 2) else "",
    `Weighted N` = complex_sample_weighted_n_text(subset(temp_design, !is.na(`..y..`))),
    `95% CI` = if (isTRUE(options$show_ci)) complex_sample_ci_range_text(mean_fit, digits = 2) else "",
    CV = if (isTRUE(options$show_precision)) complex_sample_cv_text(mean_fit) else "",
    Deff = if (isTRUE(options$show_precision)) complex_sample_deff_text(mean_fit) else "",
    SD = complex_sample_num(sqrt(as.numeric(var_fit)), 2),
    Min = complex_sample_num(min(observed, na.rm = TRUE), 2),
    Max = complex_sample_num(max(observed, na.rm = TRUE), 2),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out)[names(out) == "Estimated"] <- complex_sample_estimated_summary_column()
  out
}

complex_sample_frequency_display_table <- function(variables, categorical_tables, descriptive_table, categorical, continuous, options = list()) {
  categorical_by_name <- list()
  categorical_tables <- Filter(function(table) is.data.frame(table) && nrow(table) > 0, categorical_tables)
  if (length(categorical_tables) > 0) {
    categorical_by_name <- stats::setNames(categorical_tables, vapply(categorical_tables, function(table) {
      as.character(table$Name[[1]] %||% "")
    }, character(1)))
  }

  rows <- list()
  for (variable in variables) {
    if (variable %in% continuous && is.data.frame(descriptive_table) && nrow(descriptive_table) > 0) {
      matched <- descriptive_table[as.character(descriptive_table$Name) == variable, , drop = FALSE]
      if (nrow(matched) > 0) {
        rows[[length(rows) + 1L]] <- data.frame(
          Variable = matched$Variable[[1]],
          Value = "",
          Frequency = "",
          Estimated = matched[[complex_sample_estimated_summary_column()]][[1]],
          Median = if (isTRUE(options$show_median)) matched[["Median"]][[1]] else "",
          `Missing N` = if (isTRUE(options$show_missing)) matched[["Missing N"]][[1]] else "",
          `Weighted N` = if (isTRUE(options$show_weighted_n)) matched[["Weighted N"]][[1]] else "",
          `95% CI` = if (isTRUE(options$show_ci)) matched[["95% CI"]][[1]] else "",
          CV = if (isTRUE(options$show_precision)) matched[["CV"]][[1]] else "",
          Deff = if (isTRUE(options$show_precision)) matched[["Deff"]][[1]] else "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
      next
    }

    table <- categorical_by_name[[variable]]
    if (is.data.frame(table) && nrow(table) > 0) {
      for (row_index in seq_len(nrow(table))) {
        rows[[length(rows) + 1L]] <- data.frame(
          Variable = if (row_index == 1L) table$Variable[[row_index]] else "",
          Value = table$Value[[row_index]],
          Frequency = table[[complex_sample_frequency_summary_column()]][[row_index]],
          Estimated = "",
          Median = "",
          `Missing N` = if (isTRUE(options$show_missing) && row_index == 1L) table[["Missing N"]][[row_index]] else "",
          `Weighted N` = if (isTRUE(options$show_weighted_n)) table[["Weighted N"]][[row_index]] else "",
          `95% CI` = if (isTRUE(options$show_ci)) table[["95% CI"]][[row_index]] else "",
          CV = if (isTRUE(options$show_precision)) table[["CV"]][[row_index]] else "",
          Deff = if (isTRUE(options$show_precision)) table[["Deff"]][[row_index]] else "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(NULL)
  }
  table <- do.call(rbind, rows)
  names(table)[names(table) == "Frequency"] <- complex_sample_frequency_summary_column()
  names(table)[names(table) == "Estimated"] <- complex_sample_estimated_summary_column()
  columns <- c("Variable", "Value")
  if (length(categorical) > 0) columns <- c(columns, complex_sample_frequency_summary_column())
  if (length(continuous) > 0) columns <- c(columns, complex_sample_estimated_summary_column())
  if (length(continuous) > 0 && isTRUE(options$show_median)) columns <- c(columns, "Median")
  if (isTRUE(options$show_missing)) columns <- c(columns, "Missing N")
  if (isTRUE(options$show_weighted_n)) columns <- c(columns, "Weighted N")
  if (isTRUE(options$show_ci)) columns <- c(columns, "95% CI")
  if (isTRUE(options$show_precision)) columns <- c(columns, "CV", "Deff")
  table[, columns, drop = FALSE]
}

complex_sample_frequency_result <- function(data, variables, input, prefix, variable_info = NULL, labels = character(0), category_table = NULL) {
  built <- complex_sample_build_design(data, input, prefix, variables)
  options <- complex_sample_analysis_options(input, prefix, "frequencies")
  measurements <- ttest_measurement_lookup(variable_info)
  continuous <- variables[variables %in% names(measurements) & measurements[variables] == "continuous"]
  categorical <- setdiff(variables, continuous)
  categorical_tables <- stats::setNames(
    lapply(categorical, complex_sample_weighted_frequency, design = built$design, data = built$data, variable_info = variable_info, labels = labels, category_table = category_table, options = options),
    categorical
  )
  categorical_displayed <- names(Filter(function(table) is.data.frame(table) && nrow(table) > 0, categorical_tables))
  descriptive_tables <- list()
  descriptive_table <- if (length(continuous) > 0) {
    descriptive_tables <- stats::setNames(
      lapply(continuous, complex_sample_weighted_descriptive, design = built$design, data = built$data, variable_info = variable_info, labels = labels, category_table = category_table, options = options),
      continuous
    )
    descriptive_tables <- Filter(function(table) is.data.frame(table) && nrow(table) > 0, descriptive_tables)
    if (length(descriptive_tables) > 0) do.call(rbind, descriptive_tables) else NULL
  } else {
    NULL
  }
  continuous_displayed <- names(descriptive_tables)
  displayed_variables <- c(categorical_displayed, continuous_displayed)
  skipped_variables <- variables[!variables %in% displayed_variables]
  table <- complex_sample_frequency_display_table(variables, categorical_tables, descriptive_table, categorical_displayed, continuous_displayed, options)
  shiny::validate(shiny::need(is.data.frame(table) && nrow(table) > 0, "No non-missing values are available for the selected frequency/descriptive variables."))
  design_note <- complex_sample_design_note(built)
  missing_note <- if (isTRUE(options$show_missing)) {
    "Missing N is the unweighted number of rows with missing values for the displayed variable after survey design and subpopulation filtering."
  } else {
    ""
  }
  skipped_note <- if (length(skipped_variables) > 0) {
    sprintf(
      "Variables with no usable non-missing values after survey design/subpopulation filtering were not displayed: %s.",
      paste(vapply(skipped_variables, frequency_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table), collapse = ", ")
    )
  } else {
    ""
  }
  shiny::div(
    class = "result-section frequencies-result-section regression-result-panel",
    shiny::h3("Complex-sample frequencies / descriptives"),
    shiny::div(
      class = "frequency-table-wrap",
      coefficient_html_table(table, compact = TRUE, compact_font_size = 13, compact_width = 58, compact_first_width = 130, compact_min_width = 480)
    ),
    if (nzchar(paste(c(missing_note, skipped_note, design_note), collapse = ""))) {
      shiny::div(
        class = "analysis-result-notes",
        if (nzchar(missing_note)) shiny::tags$p(missing_note),
        if (nzchar(skipped_note)) shiny::tags$p(skipped_note),
        if (nzchar(design_note)) shiny::tags$p(design_note)
      )
    }
  )
}

complex_sample_crosstab_align_matrix <- function(tab, col_levels, fill = NULL) {
  tab <- as.matrix(tab)
  if (is.null(fill)) {
    fill <- if (is.character(tab)) "" else 0
  }
  out <- matrix(fill, nrow = nrow(tab), ncol = length(col_levels), dimnames = list(rownames(tab), col_levels))
  matched <- intersect(colnames(tab), col_levels)
  if (length(matched) > 0) {
    out[, matched] <- tab[, matched, drop = FALSE]
  }
  out
}

complex_sample_crosstab_percent_matrix <- function(weighted_tab, basis = "row") {
  weighted_tab <- as.matrix(weighted_tab)
  basis <- as.character(basis %||% "row")
  if (identical(basis, "column")) {
    totals <- colSums(weighted_tab, na.rm = TRUE)
    out <- sweep(weighted_tab, 2, ifelse(totals == 0, NA_real_, totals), "/") * 100
  } else if (identical(basis, "total")) {
    total <- sum(weighted_tab, na.rm = TRUE)
    out <- weighted_tab / ifelse(total == 0, NA_real_, total) * 100
  } else {
    totals <- rowSums(weighted_tab, na.rm = TRUE)
    out <- sweep(weighted_tab, 1, ifelse(totals == 0, NA_real_, totals), "/") * 100
  }
  out[!is.finite(out)] <- NA_real_
  out
}

complex_sample_crosstab_percent_basis_label <- function(basis = "row") {
  switch(
    as.character(basis %||% "row"),
    column = "column %",
    total = "total %",
    "row %"
  )
}

complex_sample_crosstab_test_label <- function(method = "F") {
  switch(
    as.character(method %||% "F"),
    Chisq = "Rao-Scott adjusted chi-square",
    Wald = "Wald test",
    adjWald = "adjusted Wald test",
    saddlepoint = "saddlepoint-adjusted test",
    "Rao-Scott adjusted F test"
  )
}

complex_sample_crosstab_ci_cell <- function(design, row_var, col_var, row_level, col_level, basis = "row") {
  temp_design <- design
  row_values <- as.character(temp_design$variables[[row_var]])
  col_values <- as.character(temp_design$variables[[col_var]])
  complete <- !is.na(row_values) & !is.na(col_values)
  basis <- as.character(basis %||% "row")
  denominator <- if (identical(basis, "column")) {
    complete & col_values == col_level
  } else if (identical(basis, "total")) {
    complete
  } else {
    complete & row_values == row_level
  }
  temp_design$variables$`..crosstab_denominator..` <- denominator
  temp_design$variables$`..crosstab_cell..` <- as.numeric(complete & row_values == row_level & col_values == col_level)
  subset_design <- tryCatch(subset(temp_design, `..crosstab_denominator..`), error = function(e) NULL)
  if (is.null(subset_design)) {
    return("")
  }
  fit <- tryCatch(survey::svymean(~`..crosstab_cell..`, subset_design, na.rm = TRUE), error = function(e) NULL)
  if (is.null(fit)) {
    return("")
  }
  complex_sample_ci_range_text(fit, scale = 100, digits = 1)
}

complex_sample_crosstab_ci_matrix <- function(design, row_var, col_var, row_levels, col_levels, basis = "row") {
  out <- matrix("", nrow = length(row_levels), ncol = length(col_levels), dimnames = list(row_levels, col_levels))
  for (row_index in seq_along(row_levels)) {
    for (col_index in seq_along(col_levels)) {
      out[row_index, col_index] <- complex_sample_crosstab_ci_cell(design, row_var, col_var, row_levels[[row_index]], col_levels[[col_index]], basis)
    }
  }
  out
}

complex_sample_crosstab_score <- function(values, levels) {
  matched <- match(as.character(values), as.character(levels))
  ifelse(is.na(matched), NA_real_, as.numeric(matched))
}

complex_sample_crosstab_trend_test <- function(design, row_var, col_var, row_levels, col_levels, variable_info = NULL) {
  row_measure <- crosstab_measurement(row_var, variable_info)
  col_measure <- crosstab_measurement(col_var, variable_info)
  row_ordered <- identical(row_measure, "ordered")
  col_ordered <- identical(col_measure, "ordered")
  if (!(row_ordered || col_ordered)) {
    return(NULL)
  }

  temp_design <- design
  row_values <- as.character(temp_design$variables[[row_var]])
  col_values <- as.character(temp_design$variables[[col_var]])
  complete <- !is.na(row_values) & !is.na(col_values)
  temp_design$variables$`..trend_keep..` <- complete
  subset_design <- tryCatch(subset(temp_design, `..trend_keep..`), error = function(e) NULL)
  if (is.null(subset_design)) {
    return(NULL)
  }
  row_values <- as.character(subset_design$variables[[row_var]])
  col_values <- as.character(subset_design$variables[[col_var]])

  if (length(col_levels) == 2 && row_ordered) {
    subset_design$variables$`..trend_y..` <- as.numeric(col_values == col_levels[[2]])
    subset_design$variables$`..trend_x..` <- complex_sample_crosstab_score(row_values, row_levels)
    method <- "survey-weighted logistic trend test"
    family <- stats::quasibinomial()
  } else if (length(row_levels) == 2 && col_ordered) {
    subset_design$variables$`..trend_y..` <- as.numeric(row_values == row_levels[[2]])
    subset_design$variables$`..trend_x..` <- complex_sample_crosstab_score(col_values, col_levels)
    method <- "survey-weighted logistic trend test"
    family <- stats::quasibinomial()
  } else if (row_ordered && col_ordered) {
    subset_design$variables$`..trend_y..` <- complex_sample_crosstab_score(row_values, row_levels)
    subset_design$variables$`..trend_x..` <- complex_sample_crosstab_score(col_values, col_levels)
    method <- "survey-weighted linear score trend test"
    family <- stats::gaussian()
  } else {
    return(NULL)
  }

  fit <- tryCatch(survey::svyglm(`..trend_y..` ~ `..trend_x..`, design = subset_design, family = family), error = function(e) NULL)
  if (is.null(fit)) {
    return(NULL)
  }
  coefficients <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  if (is.null(coefficients) || !"..trend_x.." %in% rownames(coefficients)) {
    return(NULL)
  }
  p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(coefficients))
  p_value <- if (length(p_col) > 0) suppressWarnings(as.numeric(coefficients["..trend_x..", p_col[[1]]])) else NA_real_
  statistic_col <- intersect(c("t value", "z value"), colnames(coefficients))
  statistic <- if (length(statistic_col) > 0) suppressWarnings(as.numeric(coefficients["..trend_x..", statistic_col[[1]]])) else NA_real_
  list(method = method, statistic = statistic, p = p_value)
}

complex_sample_crosstab_note <- function(options = list()) {
  basis <- complex_sample_crosstab_percent_basis_label(options$crosstab_percent_basis %||% "row")
  value_note <- if (isTRUE(options$show_weighted_n)) {
    paste0("Values are unweighted n(weighted ", basis, "; weighted N).")
  } else {
    paste0("Values are unweighted n(weighted ", basis, ").")
  }
  statistic_note <- paste0("Statistic is the ", complex_sample_crosstab_test_label(options$crosstab_test_method), " for the complex-sample design.")
  ci_note <- if (isTRUE(options$show_percent_ci)) "Cell 95% CIs are design-based confidence intervals for the weighted percentage." else ""
  trend_note <- if (isTRUE(options$trend_analysis)) "p for trend is reported when an ordered variable is available." else ""
  paste(
    c(value_note, statistic_note, ci_note, trend_note)[nzchar(c(value_note, statistic_note, ci_note, trend_note))],
    collapse = " "
  )
}

complex_sample_crosstab_stat_cells <- function(row_index, row_count, statistic, df_text = "", extra_class = "") {
  row_index <- as.integer(row_index %||% 1L)
  row_count <- as.integer(row_count %||% 1L)
  statistic <- as.character(statistic %||% "")
  df_text <- as.character(df_text %||% "")
  extra_class <- as.character(extra_class %||% "")
  cell_class <- trimws(paste("crosstab-stat-cell", extra_class))
  if (!nzchar(df_text)) {
    if (row_index == 1L) {
      return(shiny::tags$td(class = cell_class, rowspan = row_count, statistic))
    }
    return(NULL)
  }
  if (row_index == 1L) {
    return(shiny::tags$td(class = cell_class, statistic))
  }
  if (row_index == 2L) {
    return(shiny::tags$td(class = cell_class, paste0("(", df_text, ")")))
  }
  shiny::tags$td(class = cell_class, "")
}

complex_sample_crosstab_stat_header <- function(options = list()) {
  method <- as.character(options$crosstab_test_method %||% "F")
  label <- switch(
    method,
    F = "F",
    Chisq = "\u03C7\u00B2",
    Wald = "Wald",
    adjWald = "Adjusted Wald",
    saddlepoint = "Saddlepoint",
    "Statistic"
  )
  if (isTRUE(options$show_df) && method %in% c("F", "Chisq", "Wald", "adjWald")) {
    return(paste0(label, "(df)"))
  }
  label
}

complex_sample_crosstab_panel_class <- function(min_width, col_levels = character(0), show_trend = FALSE) {
  # Wider crosstabs need the landscape result style before they overflow a portrait page.
  wide <- is.finite(min_width) && min_width > 700
  many_columns <- length(col_levels %||% character(0)) >= 3L
  paste(
    "result-section regression-result-panel crosstab-result-section",
    if (isTRUE(wide) || isTRUE(many_columns) || isTRUE(show_trend)) "landscape-table-panel" else ""
  )
}

complex_sample_crosstab_group_display <- function(items, col_var, variable_info = NULL, labels = character(0), category_table = NULL, options = list(), skipped_notes = character(0)) {
  items <- Filter(Negate(is.null), items)
  skipped_notes <- unique(as.character(skipped_notes %||% character(0)))
  skipped_notes <- skipped_notes[nzchar(skipped_notes)]
  col_label <- frequency_variable_display_name(col_var, variable_info, labels, category_table)
  if (length(items) == 0) {
    if (length(skipped_notes) == 0) {
      return(NULL)
    }
    return(shiny::div(
      class = "result-section regression-result-panel crosstab-result-section",
      shiny::h3("Complex-sample cross-tabulation"),
      shiny::div(
        class = "analysis-result-notes crosstab-notes",
        shiny::tags$p(sprintf("No cross-tabulation table was computed for %s.", col_label)),
        lapply(skipped_notes, function(note) shiny::tags$p(note))
      )
    ))
  }
  col_measure <- crosstab_measurement(col_var, variable_info)
  col_levels <- crosstab_order_values(unique(unlist(lapply(items, function(item) colnames(item$weighted_tab)), use.names = FALSE)), col_measure)
  col_labels <- frequency_value_display_labels(col_var, col_levels, category_table)
  show_trend <- isTRUE(options$trend_analysis) && any(vapply(items, function(item) !is.null(item$trend), logical(1)))
  min_width <- max(640, 180 + length(col_levels) * 118 + 150 + if (isTRUE(show_trend)) 92 else 0)

  body_rows <- unlist(lapply(items, function(item) {
    weighted_tab <- complex_sample_crosstab_align_matrix(item$weighted_tab, col_levels)
    unweighted_tab <- complex_sample_crosstab_align_matrix(item$unweighted_tab, col_levels)
    percent_ci <- if (isTRUE(options$show_percent_ci) && !is.null(item$percent_ci)) {
      complex_sample_crosstab_align_matrix(item$percent_ci, col_levels, fill = "")
    } else {
      matrix("", nrow = nrow(weighted_tab), ncol = ncol(weighted_tab), dimnames = dimnames(weighted_tab))
    }
    row_levels <- rownames(weighted_tab)
    row_labels <- frequency_value_display_labels(item$row_var, row_levels, category_table)
    row_label <- frequency_variable_display_name(item$row_var, variable_info, labels, category_table)
    percent <- complex_sample_crosstab_percent_matrix(weighted_tab, options$crosstab_percent_basis %||% "row")
    statistic <- if (is.null(item$test)) "" else complex_sample_num(unname(item$test$statistic), 3)
    df_text <- if (isTRUE(options$show_df)) complex_sample_test_df_compact_text(item$test) else ""
    p_value <- if (is.null(item$test)) "" else complex_sample_p_value(item$test$p.value)
    trend_p <- if (!is.null(item$trend)) complex_sample_p_value(item$trend$p) else ""

    lapply(seq_len(nrow(weighted_tab)), function(row_index) {
      shiny::tags$tr(
        class = if (row_index == 1L) "crosstab-row-block-start" else NULL,
        if (row_index == 1L) {
          shiny::tags$td(class = "crosstab-row-variable", rowspan = nrow(weighted_tab), row_label)
        },
        shiny::tags$td(class = "crosstab-row-label", row_labels[[row_index]]),
        lapply(seq_len(ncol(weighted_tab)), function(col_index) {
          shiny::tags$td(
            class = "crosstab-count-cell",
                complex_sample_count_percent_text(
                  unweighted_tab[row_index, col_index],
                  percent[row_index, col_index],
                  if (isTRUE(options$show_weighted_n)) weighted_tab[row_index, col_index] else NULL,
                  percent_ci[row_index, col_index]
                )
              )
            }),
        complex_sample_crosstab_stat_cells(row_index, nrow(weighted_tab), statistic, df_text),
        if (row_index == 1L) {
          list(
            shiny::tags$td(class = "crosstab-stat-cell", rowspan = nrow(weighted_tab), p_value),
            if (isTRUE(show_trend)) shiny::tags$td(class = "crosstab-stat-cell crosstab-trend-p-cell", rowspan = nrow(weighted_tab), trend_p) else NULL
          )
        }
      )
    })
  }), recursive = FALSE)

  shiny::div(
    class = complex_sample_crosstab_panel_class(min_width, col_levels, show_trend),
    shiny::h3("Complex-sample cross-tabulation"),
    shiny::div(
      class = "frequency-table-wrap crosstab-table-wrap",
      shiny::tags$table(
        class = "coefficient-table crosstab-main-table",
        style = result_table_style(font_size = 13, min_width = min_width),
        shiny::tags$colgroup(
          shiny::tags$col(class = "crosstab-row-variable-col"),
          shiny::tags$col(class = "crosstab-row-label-col"),
          lapply(seq_along(col_levels), function(index) shiny::tags$col(class = "crosstab-count-col")),
          shiny::tags$col(class = "crosstab-stat-col"),
          shiny::tags$col(class = "crosstab-stat-col"),
          if (isTRUE(show_trend)) shiny::tags$col(class = "crosstab-stat-col crosstab-trend-p-col")
        ),
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th(class = "crosstab-row-head", rowspan = 2, ""),
            shiny::tags$th(class = "crosstab-level-label-head", rowspan = 2, ""),
            shiny::tags$th(class = "crosstab-col-head", colspan = length(col_labels), col_label),
            shiny::tags$th(class = "crosstab-stat-head", rowspan = 2, complex_sample_crosstab_stat_header(options)),
            shiny::tags$th(class = "crosstab-stat-head", rowspan = 2, "p"),
            if (isTRUE(show_trend)) shiny::tags$th(class = "crosstab-stat-head crosstab-trend-p-head", rowspan = 2, "p for trend")
          ),
          shiny::tags$tr(
            lapply(col_labels, function(label) shiny::tags$th(class = "crosstab-level-head", label))
          )
        ),
        shiny::tags$tbody(body_rows)
      )
    ),
    shiny::div(
      class = "analysis-result-notes crosstab-notes",
      shiny::tags$p(complex_sample_crosstab_note(options)),
      lapply(unique(vapply(items, function(item) as.character(item$missing_note %||% ""), character(1))), function(note) {
        if (nzchar(note)) shiny::tags$p(note)
      }),
      lapply(skipped_notes, function(note) shiny::tags$p(note)),
      lapply(unique(vapply(items, function(item) as.character(item$design_note %||% ""), character(1))), function(note) {
        if (nzchar(note)) shiny::tags$p(note)
      })
    )
  )
}

complex_sample_crosstab_display <- function(weighted_tab, unweighted_tab, row_var, col_var, test, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  weighted_tab <- as.matrix(weighted_tab)
  unweighted_tab <- as.matrix(unweighted_tab)
  row_levels <- rownames(weighted_tab)
  col_levels <- colnames(weighted_tab)
  row_labels <- frequency_value_display_labels(row_var, row_levels, category_table)
  col_labels <- frequency_value_display_labels(col_var, col_levels, category_table)
  row_label <- frequency_variable_display_name(row_var, variable_info, labels, category_table)
  col_label <- frequency_variable_display_name(col_var, variable_info, labels, category_table)
  row_totals <- rowSums(weighted_tab, na.rm = TRUE)
  percent <- sweep(weighted_tab, 1, row_totals, "/") * 100
  percent[!is.finite(percent)] <- NA_real_
  statistic <- if (is.null(test)) "" else complex_sample_num(unname(test$statistic), 3)
  df_text <- if (isTRUE(options$show_df)) complex_sample_test_df_compact_text(test) else ""
  p_value <- if (is.null(test)) "" else complex_sample_p_value(test$p.value)
  min_width <- max(640, 180 + length(col_levels) * 100 + 150)

  shiny::div(
    class = complex_sample_crosstab_panel_class(min_width, col_levels, FALSE),
    shiny::h3("Complex-sample cross-tabulation"),
    shiny::div(
      class = "frequency-table-wrap crosstab-table-wrap",
      shiny::tags$table(
        class = "coefficient-table crosstab-main-table",
        style = result_table_style(font_size = 13, min_width = min_width),
        shiny::tags$colgroup(
          shiny::tags$col(class = "crosstab-row-variable-col"),
          shiny::tags$col(class = "crosstab-row-label-col"),
          lapply(seq_along(col_levels), function(index) shiny::tags$col(class = "crosstab-count-col")),
          shiny::tags$col(class = "crosstab-stat-col"),
          shiny::tags$col(class = "crosstab-stat-col")
        ),
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th(class = "crosstab-row-head", rowspan = 2, ""),
            shiny::tags$th(class = "crosstab-level-label-head", rowspan = 2, ""),
            shiny::tags$th(class = "crosstab-col-head", colspan = length(col_labels), col_label),
            shiny::tags$th(class = "crosstab-stat-head", rowspan = 2, complex_sample_crosstab_stat_header(options)),
            shiny::tags$th(class = "crosstab-stat-head", rowspan = 2, "p")
          ),
          shiny::tags$tr(
            lapply(col_labels, function(label) shiny::tags$th(class = "crosstab-level-head", label))
          )
        ),
        shiny::tags$tbody(
          lapply(seq_len(nrow(weighted_tab)), function(row_index) {
            shiny::tags$tr(
              shiny::tags$td(class = "crosstab-row-variable", if (row_index == 1L) row_label else ""),
              shiny::tags$td(class = "crosstab-row-label", row_labels[[row_index]]),
              lapply(seq_len(ncol(weighted_tab)), function(col_index) {
                shiny::tags$td(
                  class = "crosstab-count-cell",
                  complex_sample_count_percent_text(
                    unweighted_tab[row_index, col_index],
                    percent[row_index, col_index],
                    if (isTRUE(options$show_weighted_n)) weighted_tab[row_index, col_index] else NULL
                  )
                )
              }),
              complex_sample_crosstab_stat_cells(row_index, nrow(weighted_tab), statistic, df_text),
              if (row_index == 1L) {
                list(
                  shiny::tags$td(class = "crosstab-stat-cell", rowspan = nrow(weighted_tab), p_value)
                )
              }
            )
          })
        )
      )
    ),
    shiny::div(
      class = "analysis-result-notes crosstab-notes",
      shiny::tags$p(complex_sample_crosstab_note(options))
    )
  )
}

complex_sample_crosstab_result <- function(data, row_var, col_var, input, prefix, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  built <- complex_sample_build_design(data, input, prefix, c(row_var, col_var))
  design_data <- as.data.frame(built$design$variables, stringsAsFactors = FALSE, check.names = FALSE)
  row_values <- as.character(design_data[[row_var]])
  col_values <- as.character(design_data[[col_var]])
  complete <- !is.na(row_values) & !is.na(col_values)
  complete_excluded_n <- sum(!complete, na.rm = TRUE)
  shiny::validate(shiny::need(any(complete), "No complete cases are available for the selected cross-tabulation variables."))
  analysis_design <- built$design
  analysis_design$variables$..complex_crosstab_complete.. <- complete
  analysis_design <- subset(analysis_design, ..complex_crosstab_complete..)
  formula <- stats::reformulate(c(row_var, col_var))
  weighted_tab <- survey::svytable(formula, analysis_design)
  row_levels <- rownames(weighted_tab)
  col_levels <- colnames(weighted_tab)
  shiny::validate(shiny::need(length(row_levels) > 0 && length(col_levels) > 0, "No complete cases are available for the selected cross-tabulation variables."))
  design_data <- as.data.frame(analysis_design$variables, stringsAsFactors = FALSE, check.names = FALSE)
  row_values <- as.character(design_data[[row_var]])
  col_values <- as.character(design_data[[col_var]])
  unweighted_tab <- table(
    factor(row_values, levels = row_levels),
    factor(col_values, levels = col_levels),
    useNA = "no"
  )
  test_method <- as.character(options$crosstab_test_method %||% "F")
  if (!test_method %in% c("F", "Chisq", "Wald", "adjWald", "saddlepoint")) {
    test_method <- "F"
  }
  test <- tryCatch(survey::svychisq(formula, analysis_design, statistic = test_method), error = function(e) NULL)
  percent_ci <- if (isTRUE(options$show_percent_ci)) {
    complex_sample_crosstab_ci_matrix(analysis_design, row_var, col_var, row_levels, col_levels, options$crosstab_percent_basis %||% "row")
  } else {
    NULL
  }
  trend <- if (isTRUE(options$trend_analysis)) {
    complex_sample_crosstab_trend_test(analysis_design, row_var, col_var, row_levels, col_levels, variable_info)
  } else {
    NULL
  }
  row_label <- frequency_variable_display_name(row_var, variable_info, labels, category_table)
  col_label <- frequency_variable_display_name(col_var, variable_info, labels, category_table)
  missing_note <- if (complete_excluded_n > 0) {
    sprintf(
      "%s by %s excluded %s row(s) with missing row or column values after survey design/subpopulation filtering.",
      row_label,
      col_label,
      complete_excluded_n
    )
  } else {
    ""
  }
  list(
    row_var = row_var,
    col_var = col_var,
    weighted_tab = as.matrix(weighted_tab),
    unweighted_tab = as.matrix(unweighted_tab),
    test = test,
    percent_ci = percent_ci,
    trend = trend,
    missing_note = missing_note,
    design_note = complex_sample_design_note(built)
  )
}

complex_sample_crosstab_results <- function(data, row_vars, col_vars, input, prefix, variable_info = NULL, labels = character(0), category_table = NULL) {
  row_vars <- as.character(row_vars %||% character(0))
  col_vars <- as.character(col_vars %||% character(0))
  sections <- list()
  options <- complex_sample_analysis_options(input, prefix, "crosstabs")
  for (col_var in col_vars) {
    items <- list()
    skipped_notes <- character(0)
    for (row_var in row_vars) {
      item <- tryCatch(
        complex_sample_crosstab_result(data, row_var, col_var, input, prefix, variable_info, labels, category_table, options),
        error = function(error) {
          message <- conditionMessage(error)
          if (!nzchar(message)) {
            message <- as.character(error)
          }
          message <- sub("^Error:\\s*", "", message)
          skipped_notes <<- c(skipped_notes, sprintf(
            "%s by %s was not computed: %s",
            frequency_variable_display_name(row_var, variable_info, labels, category_table),
            frequency_variable_display_name(col_var, variable_info, labels, category_table),
            message
          ))
          NULL
        }
      )
      items[[length(items) + 1L]] <- item
    }
    sections[[length(sections) + 1L]] <- complex_sample_crosstab_group_display(items, col_var, variable_info, labels, category_table, options, skipped_notes)
  }
  do.call(shiny::tagList, sections)
}

complex_sample_group_stat_label <- function(level_count, test = NULL) {
  # svyttest reports t, while survey ANOVA/regTermTest reports F.
  if (!is.null(test)) {
    statistic_names <- names(test$statistic %||% numeric(0))
    if (length(statistic_names) > 0) {
      first_name <- tolower(as.character(statistic_names[[1]]))
      if (identical(first_name, "t")) return("t")
      if (identical(first_name, "f")) return("F")
    }
    if (!is.null(test$Ftest)) {
      return("F")
    }
  }
  if (as.integer(level_count %||% 0L) == 2L) "t" else "F"
}

complex_sample_group_stat_column <- function(stat_labels, show_df = TRUE) {
  # A dependent-variable table can mix 2-level t-tests and 3+-level ANOVAs.
  stat_labels <- unique(as.character(stat_labels %||% character(0)))
  stat_labels <- stat_labels[nzchar(stat_labels)]
  label <- if (length(stat_labels) == 1L) {
    stat_labels[[1]]
  } else if (all(c("t", "F") %in% stat_labels)) {
    "t/F"
  } else {
    "Statistic"
  }
  if (isTRUE(show_df) && label %in% c("t", "F", "t/F")) {
    paste0(label, "(df)")
  } else {
    label
  }
}

complex_sample_group_stat_df_text <- function(statistic, df_text = "") {
  # Keep the header as t(df)/F(df); group rows put df in the next table row.
  statistic <- as.character(statistic %||% "")
  df_text <- as.character(df_text %||% "")
  if (!nzchar(statistic)) {
    return("")
  }
  if (!nzchar(df_text)) {
    return(statistic)
  }
  paste0(statistic, "\n(", df_text, ")")
}

complex_sample_group_column_widths <- function(columns) {
  columns <- as.character(columns %||% character(0))
  if (length(columns) == 0L) {
    return(numeric(0))
  }
  keys <- result_column_key(columns)
  weights <- vapply(keys, function(key) {
    switch(
      key,
      variable = 15,
      value = 20,
      mse = 18,
      msd = 18,
      `95ci` = 17,
      es = 8,
      effectsize = 8,
      tdf = 12,
      fdf = 12,
      tfdf = 12,
      p = 10,
      pfortrend = 10,
      posthoc = 13,
      weightedn = 8,
      cv = 7,
      deff = 7,
      10
    )
  }, numeric(1))
  weights / sum(weights) * 100
}

complex_sample_group_result <- function(data, dependents, factor_vars, input, prefix, variable_info = NULL, labels = character(0), category_table = NULL) {
  factor_vars <- as.character(factor_vars %||% character(0))
  built <- complex_sample_build_design(data, input, prefix, c(dependents, factor_vars))
  options <- complex_sample_analysis_options(input, prefix, "ttest_anova")
  sections <- list()
  for (dependent in dependents) {
    rows <- list()
    posthoc_tables <- list()
    ordered_marker_rows <- list()
    trend_used <- FALSE
    posthoc_used <- FALSE
    stat_labels <- character(0)
    missing_notes <- character(0)
    skipped_notes <- character(0)
    for (factor_var in factor_vars) {
      analysis_design <- built$design
      dependent_values <- suppressWarnings(as.numeric(analysis_design$variables[[dependent]]))
      factor_values <- analysis_design$variables[[factor_var]]
      complete <- !is.na(dependent_values) & !is.na(factor_values)
      complete_excluded_n <- sum(!complete, na.rm = TRUE)
      dependent_label <- ttest_display_variable(dependent, variable_info, labels, category_table)
      factor_label <- ttest_display_variable(factor_var, variable_info, labels, category_table)
      if (!any(complete)) {
        skipped_notes <- c(skipped_notes, sprintf(
          "%s by %s was not computed because no complete cases were available after survey design/subpopulation filtering.",
          dependent_label,
          factor_label
        ))
        next
      }
      if (complete_excluded_n > 0) {
        missing_notes <- c(missing_notes, sprintf(
          "%s by %s excluded %s row(s) with missing dependent or group values after survey design/subpopulation filtering.",
          dependent_label,
          factor_label,
          complete_excluded_n
        ))
      }
      analysis_design$variables$`..group_analysis_keep..` <- complete
      analysis_design$variables$`..y..` <- dependent_values
      analysis_design <- subset(analysis_design, `..group_analysis_keep..`)
      factor_values <- analysis_design$variables[[factor_var]]
      levels <- frequency_value_order(unique(as.character(factor_values[!is.na(factor_values)])))
      if (length(levels) < 2) {
        skipped_notes <- c(skipped_notes, sprintf(
          "%s by %s was not computed because the group variable had fewer than two usable groups after complete-case filtering.",
          dependent_label,
          factor_label
        ))
        next
      }
      factor_measure <- named_value(ttest_measurement_lookup(variable_info), factor_var, "")
      formula <- stats::reformulate(factor_var, response = "..y..")
      test <- if (length(levels) == 2) {
        tryCatch(survey::svyttest(formula, analysis_design), error = function(e) NULL)
      } else {
        fit <- tryCatch(survey::svyglm(formula, analysis_design), error = function(e) NULL)
        if (is.null(fit)) NULL else tryCatch(survey::regTermTest(fit, factor_var), error = function(e) NULL)
      }
      raw_p_value <- if (is.null(test)) {
        NA_real_
      } else {
        raw_p <- test$p.value %||% test$p
        if (length(raw_p) == 0) NA_real_ else suppressWarnings(as.numeric(raw_p[[1]]))
      }
      statistic <- if (is.null(test)) "" else complex_sample_num(unname(test$statistic %||% test$Ftest), 3)
      stat_label <- complex_sample_group_stat_label(length(levels), test)
      stat_labels <- c(stat_labels, stat_label)
      p_value <- if (is.null(test)) "" else complex_sample_p_value(raw_p_value)
      df_value <- if (isTRUE(options$show_df)) complex_sample_test_df_compact_text(test) else ""
      trend <- if (isTRUE(options$trend_analysis) && identical(factor_measure, "ordered")) {
        complex_sample_trend_test(analysis_design, "..y..", factor_var, levels)
      } else {
        NULL
      }
      trend_p <- if (!is.null(trend)) complex_sample_p_value(trend$p) else ""
      if (nzchar(trend_p)) trend_used <- TRUE
      level_stats <- list()
      row_start <- length(rows) + 1L
      stat_row_values <- rep("", length(levels))
      if (nzchar(statistic)) {
        stat_row_values[[1L]] <- statistic
        if (nzchar(df_value) && length(levels) >= 2L) {
          stat_row_values[[2L]] <- paste0("(", df_value, ")")
        }
      }
      for (level in levels) {
        level_index <- match(level, levels)
        temp_design <- analysis_design
        temp_design$variables$`..group_keep..` <- !is.na(temp_design$variables[[factor_var]]) & as.character(temp_design$variables[[factor_var]]) == level
        subset_design <- subset(temp_design, `..group_keep..`)
        fit <- survey::svymean(~`..y..`, subset_design, na.rm = TRUE, deff = TRUE)
        variance <- tryCatch(survey::svyvar(~`..y..`, subset_design, na.rm = TRUE), error = function(e) NA_real_)
        mean_value <- as.numeric(fit)
        sd_value <- sqrt(as.numeric(variance))
        level_stats[[length(level_stats) + 1L]] <- data.frame(
          level = level,
          mean = mean_value,
          sd = sd_value,
          stringsAsFactors = FALSE
        )
        rows[[length(rows) + 1L]] <- data.frame(
          Variable = if (identical(level, levels[[1]])) ttest_display_variable(factor_var, variable_info, labels, category_table) else "",
          Value = frequency_value_display_labels(factor_var, level, category_table),
          Estimated = if (isTRUE(options$mean_sd)) complex_sample_estimate_sd_text(mean_value, sd_value) else complex_sample_estimate_se_text(mean_value, survey::SE(fit)),
          `Weighted N` = if (isTRUE(options$show_weighted_n)) complex_sample_weighted_n_text(subset_design) else "",
          `95% CI` = if (isTRUE(options$show_ci)) complex_sample_ci_range_text(fit, digits = 2) else "",
          CV = if (isTRUE(options$show_precision)) complex_sample_cv_text(fit) else "",
          Deff = if (isTRUE(options$show_precision)) complex_sample_deff_text(fit) else "",
          `Effect size` = "",
          `Stat(df)` = stat_row_values[[level_index]],
          `p for trend` = if (identical(level, levels[[1]])) trend_p else "",
          p = if (identical(level, levels[[1]])) p_value else "",
          `post-hoc` = "",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
      if (isTRUE(options$show_effect_size) && length(level_stats) > 0) {
        rows[[length(rows) - length(levels) + 1L]][["Effect size"]] <- complex_sample_effect_size_text(do.call(rbind, level_stats), test)
      }
      if (ttest_should_run_posthoc(options, raw_p_value, length(levels))) {
        level_stats_table <- do.call(rbind, level_stats)
        correction <- as.character(options$post_hoc_correction %||% "holm")
        if (!correction %in% c("holm", "bonferroni")) correction <- "holm"
        correction_label <- complex_sample_post_hoc_correction_label(correction)
        p_matrix <- complex_sample_pairwise_p_matrix(analysis_design, "..y..", factor_var, levels, adjust_method = correction)
        posthoc_label <- paste0("Design-based pairwise t-test (", correction_label, ")")
        posthoc_table <- ttest_posthoc_table(factor_var, levels, p_matrix, posthoc_label, variable_info, labels, category_table)
        if (is.data.frame(posthoc_table) && nrow(posthoc_table) > 0) {
          posthoc_tables[[length(posthoc_tables) + 1L]] <- posthoc_table
          posthoc_used <- TRUE
          row_indices <- seq.int(row_start, length(rows))
          if (isTRUE(options$ordered_significance)) {
            factor_rows <- do.call(rbind, rows[row_indices])
            factor_rows <- analysis_apply_ordered_posthoc_markers(
              factor_rows,
              estimates = level_stats_table$mean,
              levels = level_stats_table$level,
              p_matrix = p_matrix,
              label_column = "Value"
            )
            markers <- attr(factor_rows, "note_markers", exact = TRUE)
            if (is.data.frame(markers) && nrow(markers) > 0) {
              markers$row <- row_indices[markers$row]
              ordered_marker_rows[[length(ordered_marker_rows) + 1L]] <- markers
            }
            for (offset in seq_along(row_indices)) {
              rows[[row_indices[[offset]]]] <- factor_rows[offset, , drop = FALSE]
            }
          } else {
            letters <- ttest_group_letters(level_stats_table$mean, level_stats_table$level, p_matrix, ordered = FALSE)
            posthoc_letters <- ttest_lookup_letters(letters, level_stats_table$level)
            for (offset in seq_along(row_indices)) {
              rows[[row_indices[[offset]]]][["post-hoc"]] <- posthoc_letters[[offset]]
            }
          }
        }
      }
    }
    if (length(rows) == 0) {
      design_note <- complex_sample_design_note(built)
      note <- paste(c(
        "Complex-sample univariable analysis was fitted with the survey design.",
        paste(unique(skipped_notes[nzchar(skipped_notes)]), collapse = " "),
        design_note
      )[nzchar(c(
        "Complex-sample univariable analysis was fitted with the survey design.",
        paste(unique(skipped_notes[nzchar(skipped_notes)]), collapse = " "),
        design_note
      ))], collapse = " ")
      sections[[length(sections) + 1L]] <- shiny::div(
        class = "result-section regression-result-panel ttest-anova-result-panel",
        shiny::h3(ttest_display_variable(dependent, variable_info, labels, category_table)),
        shiny::div(class = "analysis-result-notes", shiny::tags$p(note))
      )
      next
    }
    table <- do.call(rbind, rows)
    summary_column <- complex_sample_estimated_summary_column(options$mean_sd)
    stat_column <- complex_sample_group_stat_column(stat_labels, isTRUE(options$show_df))
    names(table)[names(table) == "Estimated"] <- summary_column
    names(table)[names(table) == "Effect size"] <- "ES"
    names(table)[names(table) == "Stat(df)"] <- stat_column
    columns <- c("Variable", "Value", summary_column)
    if (isTRUE(options$show_weighted_n)) columns <- c(columns, "Weighted N")
    if (isTRUE(options$show_ci)) columns <- c(columns, "95% CI")
    if (isTRUE(options$show_precision)) columns <- c(columns, "CV", "Deff")
    if (isTRUE(options$show_effect_size)) columns <- c(columns, "ES")
    columns <- c(columns, stat_column)
    if (isTRUE(trend_used)) columns <- c(columns, "p for trend")
    columns <- c(columns, "p")
    if (isTRUE(posthoc_used)) columns <- c(columns, "post-hoc")
    table <- table[, intersect(columns, names(table)), drop = FALSE]
    attr(table, "show_df") <- isTRUE(options$show_df)
    attr(table, "mean_sd") <- isTRUE(options$mean_sd)
    attr(table, "trend_analysis") <- isTRUE(trend_used)
    attr(table, "complex_sample_group_table") <- TRUE
    attr(table, "compact_column_widths") <- complex_sample_group_column_widths(names(table))
    if (length(ordered_marker_rows) > 0) {
      attr(table, "note_markers") <- do.call(rbind, ordered_marker_rows)
    }
    posthoc_table <- ttest_bind_result_rows(posthoc_tables)
    design_note <- complex_sample_design_note(built)
    mean_note <- if (isTRUE(options$mean_sd)) "M \u00B1 SD uses the design-weighted mean and design-based within-group SD." else "M \u00B1 SE uses the design-weighted mean and its survey standard error."
    posthoc_note <- if (isTRUE(posthoc_used)) {
      paste0("Post-hoc comparisons are design-based pairwise t-tests with ", complex_sample_post_hoc_adjustment_note(options$post_hoc_correction %||% "holm"), " p-values and are computed only for significant omnibus ANOVA results.")
    } else {
      ""
    }
    trend_note <- if (isTRUE(trend_used)) "p for trend is from a survey-weighted linear score trend test for ordered group variables." else ""
    effect_note <- if (isTRUE(options$show_effect_size)) "Effect sizes are design-weighted descriptive effect sizes and should be interpreted with the survey design note." else ""
    missing_note <- paste(unique(missing_notes[nzchar(missing_notes)]), collapse = " ")
    skipped_note <- paste(unique(skipped_notes[nzchar(skipped_notes)]), collapse = " ")
    note <- paste(c("Complex-sample univariable analysis was fitted with the survey design.", mean_note, posthoc_note, trend_note, effect_note, missing_note, skipped_note, design_note)[nzchar(c("Complex-sample univariable analysis was fitted with the survey design.", mean_note, posthoc_note, trend_note, effect_note, missing_note, skipped_note, design_note))], collapse = " ")
    sections[[length(sections) + 1L]] <- shiny::div(
      class = "result-section regression-result-panel ttest-anova-result-panel",
      shiny::h3(ttest_display_variable(dependent, variable_info, labels, category_table)),
      coefficient_html_table(table, compact = TRUE, compact_font_size = 13, compact_width = 66, compact_first_width = 130, compact_min_width = if (isTRUE(posthoc_used) || isTRUE(trend_used)) 720 else 560, note_line = note),
      if (is.data.frame(posthoc_table) && nrow(posthoc_table) > 0) {
        shiny::div(
          class = "ttest-anova-posthoc-section",
          shiny::h4("Post-hoc"),
          coefficient_html_table(posthoc_table, compact = TRUE, compact_font_size = 13, compact_width = 72, compact_first_width = 130, compact_min_width = 560)
        )
      }
    )
  }
  do.call(shiny::tagList, sections)
}

complex_sample_term_display <- function(term, term_labels = NULL) {
  term <- as.character(term %||% character(0))
  if (length(term) == 0 || is.null(term_labels) || length(term_labels) == 0) {
    return(term)
  }
  vapply(term, function(value) {
    if (identical(value, "(Intercept)")) {
      return(value)
    }
    for (safe in names(term_labels)) {
      if (startsWith(value, safe)) {
        suffix <- substring(value, nchar(safe) + 1L)
        return(paste0(term_labels[[safe]], suffix))
      }
    }
    value
  }, character(1), USE.NAMES = FALSE)
}

complex_sample_relevel_factor <- function(values, variable, category_table = NULL) {
  values <- factor(as.character(values))
  reference_values <- regression_reference_values_static(category_table)
  reference <- trimws(named_value(reference_values, variable, ""))
  if (nzchar(reference) && reference %in% levels(values)) {
    values <- stats::relevel(values, ref = reference)
  }
  values
}

complex_sample_regression_predictor_values <- function(values, variable, variable_info = NULL, category_table = NULL) {
  measurement <- named_value(ttest_measurement_lookup(variable_info), variable, "")
  if (measurement %in% c("binary", "category", "ordered")) {
    return(complex_sample_relevel_factor(values, variable, category_table))
  }
  if (is.numeric(values) || is.integer(values)) {
    return(suppressWarnings(as.numeric(values)))
  }
  complex_sample_relevel_factor(values, variable, category_table)
}

complex_sample_regression_has_variation <- function(values) {
  values <- values[!is.na(values)]
  if (is.factor(values)) {
    return(nlevels(droplevels(values)) >= 2L)
  }
  length(unique(values)) >= 2L
}

complex_sample_svyglm_raw_table <- function(fit) {
  table <- as.data.frame(summary(fit)$coefficients, stringsAsFactors = FALSE, check.names = FALSE)
  term <- rownames(table)
  p_col <- grep("Pr\\(", names(table), value = TRUE)[1] %||% names(table)[ncol(table)]
  stat_col <- grep("t value|z value", names(table), value = TRUE)[1] %||% names(table)[3]
  data.frame(
    Term = term,
    Estimate = as.numeric(table[[1]]),
    SE = as.numeric(table[[2]]),
    Statistic = as.numeric(table[[stat_col]]),
    p = as.numeric(table[[p_col]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

complex_sample_ci_text <- function(lower, upper, exponentiate = FALSE) {
  if (length(lower) == 0 || length(upper) == 0 || is.na(lower) || is.na(upper)) {
    return("")
  }
  if (isTRUE(exponentiate)) {
    lower <- exp(lower)
    upper <- exp(upper)
  }
  paste0(complex_sample_num(lower, 3), "-", complex_sample_num(upper, 3))
}

complex_sample_coef_row <- function(raw, term, ci, exponentiate = FALSE, design_df = NA_real_) {
  row <- raw[raw$Term == term, , drop = FALSE]
  if (nrow(row) == 0) {
    return(NULL)
  }
  lower <- if (!is.null(ci) && term %in% rownames(ci)) ci[term, 1] else NA_real_
  upper <- if (!is.null(ci) && term %in% rownames(ci)) ci[term, 2] else NA_real_
  estimate <- row$Estimate[[1]]
  statistic <- suppressWarnings(as.numeric(row$Statistic[[1]]))
  display_statistic <- if (isTRUE(exponentiate) && is.finite(statistic)) statistic^2 else statistic
  p_value <- if (isTRUE(exponentiate) && is.finite(display_statistic)) {
    df <- suppressWarnings(as.numeric(design_df %||% NA_real_))
    if (is.finite(df) && df > 0) {
      stats::pf(display_statistic, df1 = 1, df2 = df, lower.tail = FALSE)
    } else {
      stats::pchisq(display_statistic, df = 1, lower.tail = FALSE)
    }
  } else {
    row$p[[1]]
  }
  data.frame(
    B = complex_sample_num(estimate, 3),
    SE = complex_sample_num(row$SE[[1]], 3),
    OR = if (isTRUE(exponentiate)) complex_sample_num(exp(estimate), 3) else "",
    CI = complex_sample_ci_text(lower, upper, exponentiate),
    Statistic = complex_sample_num(display_statistic, 3),
    p = complex_sample_p_value(p_value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

complex_sample_regression_coef_display_table <- function(raw, fit, predictors, safe_predictors, model_design, logistic = FALSE, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  ci <- tryCatch(stats::confint(fit), error = function(e) NULL)
  design_df <- complex_sample_design_df_value(fit)
  rows <- list()
  used_terms <- "(Intercept)"
  intercept <- complex_sample_coef_row(raw, "(Intercept)", ci, exponentiate = isTRUE(logistic), design_df = design_df)
  if (!is.null(intercept)) {
    rows[[length(rows) + 1L]] <- data.frame(
      Variable = "Constant",
      Category = "",
      intercept,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  for (predictor in predictors) {
    safe <- safe_predictors[[predictor]]
    predictor_label <- frequency_variable_display_name(predictor, variable_info, labels, category_table)
    values <- model_design$variables[[safe]]
    if (is.factor(values)) {
      levels <- levels(droplevels(values))
      if (length(levels) == 0) next
      rows[[length(rows) + 1L]] <- data.frame(
        Variable = predictor_label,
        Category = frequency_value_display_labels(predictor, levels[[1]], category_table),
        B = if (isTRUE(logistic)) "" else "reference",
        SE = "",
        OR = if (isTRUE(logistic)) "reference" else "",
        CI = "",
        Statistic = "",
        p = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      predictor_terms <- raw$Term[startsWith(raw$Term, safe)]
      predictor_terms <- setdiff(predictor_terms, used_terms)
      for (level_index in seq_along(levels[-1])) {
        level <- levels[-1][[level_index]]
        term <- predictor_terms[[level_index]] %||% ""
        coef <- complex_sample_coef_row(raw, term, ci, exponentiate = isTRUE(logistic), design_df = design_df)
        if (is.null(coef)) next
        used_terms <- c(used_terms, term)
        rows[[length(rows) + 1L]] <- data.frame(
          Variable = "",
          Category = frequency_value_display_labels(predictor, level, category_table),
          coef,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    } else {
      term <- safe
      coef <- complex_sample_coef_row(raw, term, ci, exponentiate = isTRUE(logistic), design_df = design_df)
      if (is.null(coef)) next
      used_terms <- c(used_terms, term)
      rows[[length(rows) + 1L]] <- data.frame(
        Variable = predictor_label,
        Category = "",
        coef,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  remaining_terms <- setdiff(raw$Term, used_terms)
  for (term in remaining_terms) {
    coef <- complex_sample_coef_row(raw, term, ci, exponentiate = isTRUE(logistic), design_df = design_df)
    if (is.null(coef)) next
    rows[[length(rows) + 1L]] <- data.frame(
      Variable = complex_sample_term_display(term),
      Category = "",
      coef,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  table <- if (length(rows) > 0) do.call(rbind, rows) else data.frame(stringsAsFactors = FALSE)
  if (isTRUE(logistic)) {
    columns <- c("Variable", "Category", "B", "SE", "OR")
    if (isTRUE(options$show_ci)) columns <- c(columns, "CI")
    if (isTRUE(options$show_wald)) columns <- c(columns, "Statistic")
    columns <- c(columns, "p")
    table[, intersect(columns, names(table)), drop = FALSE]
  } else {
    columns <- c("Variable", "Category", "B", "SE")
    if (isTRUE(options$show_ci)) columns <- c(columns, "CI")
    columns <- c(columns, "Statistic", "p")
    table[, intersect(columns, names(table)), drop = FALSE]
  }
}

complex_sample_single_regression_result <- function(data, outcome, predictors, input, prefix, logistic = FALSE, variable_info = NULL, labels = character(0), category_table = NULL) {
  variables <- unique(c(outcome, predictors))
  built <- complex_sample_build_design(data, input, prefix, variables)
  options <- complex_sample_analysis_options(input, prefix, if (isTRUE(logistic)) "logistic" else "regression")
  family <- if (isTRUE(logistic)) stats::quasibinomial() else stats::gaussian()
  model_design <- built$design
  model_design$variables$`..outcome..` <- if (isTRUE(logistic)) {
    NULL
  } else {
    suppressWarnings(as.numeric(model_design$variables[[outcome]]))
  }
  if (isTRUE(logistic)) {
    y <- model_design$variables[[outcome]]
    y_values <- frequency_value_order(unique(as.character(y[!is.na(y)])))
    shiny::validate(shiny::need(length(y_values) == 2, "Logistic regression requires a binary dependent variable."))
    model_design$variables$`..outcome..` <- as.numeric(!is.na(y) & as.character(y) == y_values[[2]])
    event_category <- y_values[[2]]
  } else {
    event_category <- ""
  }
  shiny::validate(shiny::need(any(!is.na(model_design$variables$`..outcome..`)), "Dependent variable has no usable non-missing values."))

  safe_predictors <- stats::setNames(paste0("..x", seq_along(predictors), ".."), predictors)
  for (predictor in predictors) {
    model_design$variables[[safe_predictors[[predictor]]]] <- complex_sample_regression_predictor_values(
      model_design$variables[[predictor]],
      predictor,
      variable_info,
      category_table
    )
  }
  model_names <- c("..outcome..", unname(safe_predictors))
  complete <- stats::complete.cases(as.data.frame(model_design$variables[, model_names, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE))
  shiny::validate(shiny::need(any(complete), "No complete cases are available for the selected regression variables."))
  eligible_n <- nrow(as.data.frame(model_design$variables, stringsAsFactors = FALSE, check.names = FALSE))
  complete_n <- sum(complete, na.rm = TRUE)
  complete_excluded_n <- max(eligible_n - complete_n, 0L)
  model_design$variables$`..model_keep..` <- complete
  model_design <- subset(model_design, `..model_keep..`)

  usable_predictors <- predictors[vapply(unname(safe_predictors), function(name) {
    complex_sample_regression_has_variation(model_design$variables[[name]])
  }, logical(1))]
  shiny::validate(shiny::need(length(usable_predictors) > 0, "No independent variable has at least two usable values after applying the survey design and subpopulation."))
  for (name in unname(safe_predictors[usable_predictors])) {
    if (is.factor(model_design$variables[[name]])) {
      model_design$variables[[name]] <- droplevels(model_design$variables[[name]])
    }
  }
  formula <- stats::as.formula(
    paste("..outcome.. ~", paste(unname(safe_predictors[usable_predictors]), collapse = " + ")),
    env = baseenv()
  )
  fit <- tryCatch(
    survey::svyglm(formula, design = model_design, family = family, na.action = stats::na.omit),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    shiny::validate(shiny::need(FALSE, paste("Complex-sample regression could not be computed:", conditionMessage(fit))))
  }
  raw_table <- tryCatch(
    complex_sample_svyglm_raw_table(fit),
    error = function(e) e
  )
  if (inherits(raw_table, "error")) {
    shiny::validate(shiny::need(FALSE, paste("Complex-sample regression coefficients could not be summarized:", conditionMessage(raw_table))))
  }
  coef_table <- complex_sample_regression_coef_display_table(
    raw_table,
    fit,
    usable_predictors,
    safe_predictors,
    model_design,
    logistic = logistic,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table,
    options = options
  )
  if (isTRUE(logistic) && "OR" %in% names(coef_table)) {
    names(coef_table)[names(coef_table) == "OR"] <- "Odds ratio"
  }
  if (isTRUE(logistic) && "Statistic" %in% names(coef_table)) {
    names(coef_table)[names(coef_table) == "Statistic"] <- "Wald"
  }
  if ("CI" %in% names(coef_table)) {
    names(coef_table)[names(coef_table) == "CI"] <- "95% CI"
  }
  overview_items <- c("Unweighted N", "Eligible design N", "Complete-case excluded N")
  overview_values <- c(stats::nobs(fit), eligible_n, complete_excluded_n)
  if (isTRUE(options$show_weighted_n)) {
    overview_items <- c(overview_items, "Weighted N")
    overview_values <- c(overview_values, complex_sample_weighted_n_text(model_design))
  }
  if (isTRUE(options$show_df)) {
    overview_items <- c(overview_items, "Design df")
    overview_values <- c(overview_values, complex_sample_design_df_text(model_design))
  }
  if (isTRUE(logistic)) {
    overview_items <- c(overview_items, "Event category")
    overview_values <- c(overview_values, frequency_value_display_labels(outcome, event_category, category_table))
  }
  if (isTRUE(options$show_model_fit)) {
    wald <- complex_sample_model_wald_text(fit, unname(safe_predictors[usable_predictors]))
    if (isTRUE(logistic)) {
      pseudo <- complex_sample_logistic_pseudo_r_squared(fit, model_design, model_design$variables$`..outcome..`)
      overview_items <- c(overview_items, "McFadden pseudo R-squared", "Nagelkerke pseudo R-squared")
      overview_values <- c(
        overview_values,
        if (is.finite(pseudo[["mcfadden"]])) complex_sample_num(pseudo[["mcfadden"]], 3) else "",
        if (is.finite(pseudo[["nagelkerke"]])) complex_sample_num(pseudo[["nagelkerke"]], 3) else ""
      )
    } else {
      r2 <- complex_sample_weighted_r_squared(fit, model_design, model_design$variables$`..outcome..`)
      overview_items <- c(overview_items, "R-squared", "Adjusted R-squared")
      overview_values <- c(
        overview_values,
        if (is.finite(r2[["r2"]])) complex_sample_num(r2[["r2"]], 3) else "",
        if (is.finite(r2[["adj_r2"]])) complex_sample_num(r2[["adj_r2"]], 3) else ""
      )
    }
    overview_items <- c(overview_items, "Model Wald/F statistic")
    overview_values <- c(overview_values, wald[["statistic"]])
    if (isTRUE(options$show_df)) {
      overview_items <- c(overview_items, "Model Wald/F df")
      overview_values <- c(overview_values, wald[["df"]])
    }
    overview_items <- c(overview_items, "Model Wald/F p")
    overview_values <- c(overview_values, wald[["p"]])
  }
  overview_items <- c(overview_items, "Analysis", "Outcome", "Predictors")
  overview_values <- c(
    overview_values,
    if (isTRUE(logistic)) "Complex-sample logistic regression" else "Complex-sample linear regression",
    frequency_variable_display_name(outcome, variable_info, labels, category_table),
    paste(vapply(usable_predictors, frequency_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table), collapse = ", ")
  )
  overview <- data.frame(
    Item = overview_items,
    Value = overview_values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  note_parts <- c(
    if (isTRUE(logistic)) {
      "Coefficients are from survey-weighted logistic regression; Wald is the squared coefficient test statistic with design-df F p-values when design df is available, and odds ratios are exponentiated coefficients."
    } else {
      "Coefficients are from survey-weighted linear regression; model statistics use the design-based Wald/F test."
    },
    if (isTRUE(logistic) && isTRUE(options$show_model_fit)) {
      "Pseudo R-squared values are approximate descriptive fit indices for the survey-weighted logistic model."
    } else {
      ""
    },
    "Categorical predictors are displayed with the reference category followed by estimated categories.",
    if (complete_excluded_n > 0) sprintf("Complete-case modeling excluded %s row(s) with missing outcome or predictor values after survey design/subpopulation filtering.", complete_excluded_n) else "",
    complex_sample_design_note(built)
  )
  note_line <- paste(note_parts[nzchar(note_parts)], collapse = " ")
  shiny::tagList(
    shiny::div(
      class = "result-section regression-result-panel logistic-result-panel",
      shiny::h3(sprintf(
        "%s: %s",
        if (isTRUE(logistic)) "Complex-sample logistic regression" else "Complex-sample regression",
        frequency_variable_display_name(outcome, variable_info, labels, category_table)
      )),
      model_overview_html_table(overview),
      coefficient_html_table(
        coef_table,
        compact = TRUE,
        compact_font_size = 13,
        compact_width = 72,
        compact_first_width = 128,
        compact_min_width = if (isTRUE(logistic)) 680 else 560,
        note_line = note_line
      )
    )
  )
}

complex_sample_regression_failure_section <- function(outcome, error, logistic = FALSE, variable_info = NULL, labels = character(0), category_table = NULL) {
  message <- conditionMessage(error)
  if (!nzchar(message)) {
    message <- as.character(error)
  }
  message <- sub("^Error:\\s*", "", message)
  shiny::div(
    class = "result-section regression-result-panel logistic-result-panel",
    shiny::h3(sprintf(
      "%s: %s",
      if (isTRUE(logistic)) "Complex-sample logistic regression" else "Complex-sample regression",
      frequency_variable_display_name(outcome, variable_info, labels, category_table)
    )),
    shiny::div(
      class = "analysis-result-notes",
      shiny::tags$p(sprintf(
        "%s was not computed: %s",
        if (isTRUE(logistic)) "Complex-sample logistic regression" else "Complex-sample regression",
        message
      ))
    )
  )
}

complex_sample_regression_results <- function(data, outcomes, predictors, input, prefix, logistic = FALSE, variable_info = NULL, labels = character(0), category_table = NULL) {
  sections <- lapply(outcomes, function(outcome) {
    tryCatch(
      complex_sample_single_regression_result(data, outcome, predictors, input, prefix, logistic, variable_info, labels, category_table),
      error = function(error) {
        complex_sample_regression_failure_section(outcome, error, logistic, variable_info, labels, category_table)
      }
    )
  })
  do.call(shiny::tagList, sections)
}

complex_sample_correlation_score <- function(values, method = "pearson", measurement = "continuous") {
  method <- as.character(method %||% "pearson")[[1]]
  measurement <- as.character(measurement %||% "continuous")[[1]]
  if (!measurement %in% c("continuous", "ordered")) {
    measurement <- "continuous"
  }
  numeric_values <- suppressWarnings(as.numeric(correlation_analysis_vector(values, measurement)))
  if (identical(method, "spearman")) {
    return(rank(numeric_values, ties.method = "average", na.last = "keep"))
  }
  numeric_values
}

complex_sample_correlation_pair <- function(design, x_name, y_name, method = "pearson", options = list(), variable_info = NULL, labels = character(0), category_table = NULL) {
  method <- as.character(method %||% "pearson")[[1]]
  x_measure <- correlation_measurement(x_name, variable_info)
  y_measure <- correlation_measurement(y_name, variable_info)
  temp_design <- design
  temp_design$variables$`..corr_x..` <- complex_sample_correlation_score(temp_design$variables[[x_name]], method, x_measure)
  temp_design$variables$`..corr_y..` <- complex_sample_correlation_score(temp_design$variables[[y_name]], method, y_measure)
  complete <- stats::complete.cases(temp_design$variables[, c("..corr_x..", "..corr_y.."), drop = FALSE])
  missing_n <- sum(!complete, na.rm = TRUE)
  temp_design$variables$`..corr_keep..` <- complete
  pair_design <- tryCatch(subset(temp_design, `..corr_keep..`), error = function(e) NULL)
  if (is.null(pair_design)) {
    stop("Pairwise complete-case subset could not be constructed.", call. = FALSE)
  }
  pair_data <- as.data.frame(pair_design$variables, stringsAsFactors = FALSE, check.names = FALSE)
  n_unweighted <- nrow(pair_data)
  if (n_unweighted < 3) {
    stop("Fewer than three pairwise complete observations were available.", call. = FALSE)
  }
  if (length(unique(pair_data$`..corr_x..`[!is.na(pair_data$`..corr_x..`)])) < 2 ||
      length(unique(pair_data$`..corr_y..`[!is.na(pair_data$`..corr_y..`)])) < 2) {
    stop("At least one variable had fewer than two unique pairwise complete values.", call. = FALSE)
  }
  fit <- tryCatch(survey::svyvar(~`..corr_x..` + `..corr_y..`, pair_design, na.rm = TRUE), error = function(e) NULL)
  if (is.null(fit)) {
    stop("Design-based covariance could not be estimated.", call. = FALSE)
  }
  cov_matrix <- tryCatch(as.matrix(stats::coef(fit)), error = function(e) NULL)
  if (is.null(cov_matrix) || nrow(cov_matrix) < 2 || ncol(cov_matrix) < 2) {
    stop("Design-based covariance matrix was not available.", call. = FALSE)
  }
  var_x <- suppressWarnings(as.numeric(cov_matrix[1, 1]))
  cov_xy <- suppressWarnings(as.numeric(cov_matrix[2, 1]))
  var_y <- suppressWarnings(as.numeric(cov_matrix[2, 2]))
  if (!is.finite(var_x) || !is.finite(var_y) || var_x <= 0 || var_y <= 0 || !is.finite(cov_xy)) {
    stop("Design-based correlation could not be estimated because variance was zero or invalid.", call. = FALSE)
  }
  r <- cov_xy / sqrt(var_x * var_y)
  r <- max(min(r, 1), -1)
  fit_vcov <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  se <- NA_real_
  if (!is.null(fit_vcov) && all(dim(fit_vcov) >= c(4, 4))) {
    parameter_vcov <- fit_vcov[c(1, 2, 4), c(1, 2, 4), drop = FALSE]
    gradient <- c(
      -0.5 * cov_xy / (var_x^(1.5) * sqrt(var_y)),
      1 / sqrt(var_x * var_y),
      -0.5 * cov_xy / (sqrt(var_x) * var_y^(1.5))
    )
    variance <- suppressWarnings(as.numeric(t(gradient) %*% parameter_vcov %*% gradient))
    if (is.finite(variance) && variance >= 0) {
      se <- sqrt(variance)
    }
  }
  df <- suppressWarnings(as.numeric(survey::degf(pair_design)))
  statistic <- if (is.finite(se) && se > 0) r / se else NA_real_
  p_value <- if (is.finite(statistic)) {
    if (is.finite(df) && df > 0) 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE) else 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  } else {
    NA_real_
  }
  ci_text <- ""
  if (isTRUE(options$show_ci) && is.finite(se) && se >= 0) {
    critical <- if (is.finite(df) && df > 0) stats::qt(0.975, df = df) else stats::qnorm(0.975)
    lower <- max(-1, r - critical * se)
    upper <- min(1, r + critical * se)
    ci_text <- complex_sample_ci_text(lower, upper)
  }
  uses_ordered_score <- any(c(x_measure, y_measure) == "ordered")
  method_label <- if (identical(method, "spearman")) {
    "Spearman rank"
  } else if (isTRUE(uses_ordered_score)) {
    "Pearson (ordinal scores)"
  } else {
    "Pearson"
  }
  data.frame(
    `..x_name..` = x_name,
    `..y_name..` = y_name,
    Variable1 = correlation_variable_display_name(x_name, variable_info, labels, category_table),
    Variable2 = correlation_variable_display_name(y_name, variable_info, labels, category_table),
    Method = method_label,
    `Unweighted N` = n_unweighted,
    `Weighted N` = if (isTRUE(options$show_weighted_n)) complex_sample_weighted_n_text(pair_design) else "",
    `Missing N` = if (isTRUE(options$show_missing)) missing_n else "",
    r = complex_sample_num(r, 3),
    `..r_raw..` = r,
    SE = complex_sample_num(se, 3),
    `95% CI` = ci_text,
    df = if (isTRUE(options$show_df)) complex_sample_num(df, 1) else "",
    t = complex_sample_num(statistic, 3),
    p = complex_sample_p_value(p_value),
    `..p_raw..` = p_value,
    Note = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

complex_sample_correlation_failure_row <- function(x_name, y_name, message, variable_info = NULL, labels = character(0), category_table = NULL) {
  data.frame(
    `..x_name..` = x_name,
    `..y_name..` = y_name,
    Variable1 = correlation_variable_display_name(x_name, variable_info, labels, category_table),
    Variable2 = correlation_variable_display_name(y_name, variable_info, labels, category_table),
    Method = "",
    `Unweighted N` = "",
    `Weighted N` = "",
    `Missing N` = "",
    r = "",
    `..r_raw..` = NA_real_,
    SE = "",
    `95% CI` = "",
    df = "",
    t = "",
    p = "",
    `..p_raw..` = NA_real_,
    Note = message,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

complex_sample_correlation_matrix_table <- function(table, variables, p_adjust_method = "holm", variable_info = NULL, labels = character(0), category_table = NULL) {
  variables <- as.character(variables %||% character(0))
  variables <- variables[nzchar(variables)]
  if (!is.data.frame(table) || length(variables) < 2L || !all(c("..x_name..", "..y_name..", "..r_raw..") %in% names(table))) {
    return(NULL)
  }
  display_names <- stats::setNames(
    vapply(variables, correlation_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    variables
  )
  out <- matrix("", nrow = length(variables), ncol = length(variables), dimnames = list(unname(display_names), unname(display_names)))
  p_column <- if (!identical(p_adjust_method, "none") && "..p_adjusted_raw.." %in% names(table)) "..p_adjusted_raw.." else "..p_raw.."
  for (row_index in seq_len(nrow(table))) {
    x_name <- as.character(table[["..x_name.."]][[row_index]] %||% "")
    y_name <- as.character(table[["..y_name.."]][[row_index]] %||% "")
    i <- match(x_name, variables)
    j <- match(y_name, variables)
    r_value <- suppressWarnings(as.numeric(table[["..r_raw.."]][[row_index]]))
    if (is.na(i) || is.na(j) || !is.finite(r_value)) {
      next
    }
    p_value <- if (p_column %in% names(table)) suppressWarnings(as.numeric(table[[p_column]][[row_index]])) else NA_real_
    cell <- paste0(complex_sample_num(r_value, 3), if (is.finite(p_value)) correlation_sig(p_value) else "")
    out[i, j] <- cell
    out[j, i] <- cell
  }
  lower <- matrix("", nrow = nrow(out), ncol = ncol(out), dimnames = dimnames(out))
  for (row in seq_len(nrow(out))) {
    for (col in seq_len(ncol(out))) {
      if (row > col) {
        lower[row, col] <- out[row, col]
      }
    }
  }
  result <- data.frame(Variable = rownames(lower), as.data.frame(lower, check.names = FALSE), check.names = FALSE)
  if (nrow(result) >= 5L) {
    labels_short <- paste0("x", seq_len(nrow(result)))
    full_labels <- rownames(lower)
    result$Variable <- labels_short
    names(result)[-1L] <- labels_short
    attr(result, "compact_column_widths") <- c(8, rep((92 / max(1L, ncol(result) - 1L)), ncol(result) - 1L))
    attr(result, "matrix_variable_note") <- paste(sprintf("%s = %s", labels_short, full_labels), collapse = "; ")
  }
  result
}

complex_sample_correlation_result <- function(data, variables, input, prefix, variable_info = NULL, labels = character(0), category_table = NULL) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  shiny::validate(shiny::need(length(variables) >= 2, "Select at least two variables."))
  built <- complex_sample_build_design(data, input, prefix, variables)
  options <- complex_sample_analysis_options(input, prefix, "correlation")
  method <- as.character(options$correlation_method %||% "pearson")[[1]]
  if (!method %in% c("pearson", "spearman")) method <- "pearson"
  selected_measurements <- vapply(variables, correlation_measurement, character(1), variable_info = variable_info)
  ordered_included <- any(selected_measurements == "ordered")
  rows <- list()
  for (i in seq_len(length(variables) - 1L)) {
    for (j in seq.int(i + 1L, length(variables))) {
      x_name <- variables[[i]]
      y_name <- variables[[j]]
      rows[[length(rows) + 1L]] <- tryCatch(
        complex_sample_correlation_pair(built$design, x_name, y_name, method, options, variable_info, labels, category_table),
        error = function(error) complex_sample_correlation_failure_row(x_name, y_name, conditionMessage(error), variable_info, labels, category_table)
      )
    }
  }
  table <- do.call(rbind, rows)
  if (!"Note" %in% names(table)) {
    table$Note <- ""
  }
  p_adjust_method <- as.character(options$correlation_p_adjust %||% "holm")[[1]]
  if (!p_adjust_method %in% c("none", "holm", "bonferroni", "BH")) {
    p_adjust_method <- "holm"
  }
  if (!identical(p_adjust_method, "none") && "..p_raw.." %in% names(table)) {
    p_raw <- suppressWarnings(as.numeric(table[["..p_raw.."]]))
    p_adjusted <- rep("", length(p_raw))
    p_adjusted_raw <- rep(NA_real_, length(p_raw))
    valid <- is.finite(p_raw)
    if (any(valid)) {
      p_adjusted_raw[valid] <- stats::p.adjust(p_raw[valid], method = p_adjust_method)
      p_adjusted[valid] <- vapply(p_adjusted_raw[valid], complex_sample_p_value, character(1))
    }
    table[["..p_adjusted_raw.."]] <- p_adjusted_raw
    table[["p adjusted"]] <- p_adjusted
  }
  if ("p adjusted" %in% names(table)) {
    base_columns <- setdiff(names(table), "p adjusted")
    p_index <- match("p", base_columns)
    if (!is.na(p_index)) {
      table <- table[, append(base_columns, "p adjusted", after = p_index), drop = FALSE]
    }
  }
  matrix_table <- if (isTRUE(options$correlation_matrix)) {
    complex_sample_correlation_matrix_table(table, variables, p_adjust_method, variable_info, labels, category_table)
  } else {
    NULL
  }
  hidden_columns <- intersect(c("..x_name..", "..y_name..", "..r_raw..", "..p_raw..", "..p_adjusted_raw.."), names(table))
  if (length(hidden_columns) > 0) {
    table <- table[, setdiff(names(table), hidden_columns), drop = FALSE]
  }
  if (!any(nzchar(as.character(table$Note %||% "")))) {
    table$Note <- NULL
  }
  method_definition_note <- if (identical(method, "spearman")) {
    "Spearman rank correlation rank-transforms each variable before design-based covariance estimation."
  } else {
    ""
  }
  detail_note <- paste(c(
    sprintf("Complex-sample %s correlation was estimated with design-based covariance and delta-method standard errors.", if (identical(method, "spearman")) "Spearman rank" else "Pearson"),
    method_definition_note,
    if (isTRUE(ordered_included)) "Ordered variables were converted to ordinal scores before design-based covariance estimation." else NULL,
    if (!identical(p_adjust_method, "none")) sprintf("The p adjusted column uses %s p-values across the displayed variable pairs.", complex_sample_p_adjustment_note(p_adjust_method)) else NULL,
    "Each row uses pairwise complete observations for the two variables.",
    if (isTRUE(options$show_missing)) "Missing N is the unweighted number of rows excluded from each pair because either variable was missing after survey design/subpopulation filtering." else NULL,
    complex_sample_design_note(built)
  ), collapse = " ")
  matrix_note <- if (is.data.frame(matrix_table) && nrow(matrix_table) > 0) {
    paste(c(
      "Lower triangle shows design-based correlation coefficients.",
      method_definition_note,
      sprintf("Significance markers use %s p-values: * p < .05; ** p < .01; *** p < .001.", complex_sample_p_adjustment_note(p_adjust_method)),
      attr(matrix_table, "matrix_variable_note", exact = TRUE) %||% ""
    )[nzchar(c(
      "Lower triangle shows design-based correlation coefficients.",
      sprintf("Significance markers use %s p-values: * p < .05; ** p < .01; *** p < .001.", complex_sample_p_adjustment_note(p_adjust_method)),
      attr(matrix_table, "matrix_variable_note", exact = TRUE) %||% ""
    ))], collapse = " ")
  } else {
    ""
  }
  meta <- built$meta %||% list()
  overview_items <- c(
    "Analysis",
    "Method",
    "Variables",
    "Displayed variable pairs",
    "P-value adjustment",
    "Correlation matrix"
  )
  overview_values <- c(
    "Complex-sample correlation",
    if (identical(method, "spearman")) "Spearman rank correlation" else "Pearson correlation",
    length(variables),
    nrow(table),
    complex_sample_p_adjustment_note(p_adjust_method),
    if (isTRUE(options$correlation_matrix)) "Shown" else "Not shown"
  )
  original_n <- suppressWarnings(as.integer(meta$original_n %||% NA_integer_))
  analysis_n <- suppressWarnings(as.integer(meta$analysis_n %||% NA_integer_))
  if (is.finite(original_n)) {
    overview_items <- c(overview_items, "Original N")
    overview_values <- c(overview_values, original_n)
  }
  if (is.finite(analysis_n)) {
    overview_items <- c(overview_items, "Survey design N")
    overview_values <- c(overview_values, analysis_n)
  }
  if (isTRUE(options$show_df)) {
    overview_items <- c(overview_items, "Design df")
    overview_values <- c(overview_values, complex_sample_design_df_text(built$design))
  }
  overview <- data.frame(
    Item = overview_items,
    Value = overview_values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  shiny::tagList(
    div(
      class = "result-section regression-result-panel complex-sample-correlation-result-panel",
      h3("Complex-sample correlation overview"),
      model_overview_html_table(overview)
    ),
    if (is.data.frame(matrix_table) && nrow(matrix_table) > 0) {
      div(
        class = "result-section regression-result-panel complex-sample-correlation-result-panel",
        h3("Complex-sample correlation matrix"),
        div(
          class = "frequency-table-wrap",
          coefficient_html_table(matrix_table, compact = TRUE, compact_font_size = 13, compact_width = 58, compact_first_width = 120, compact_min_width = 560, note_line = matrix_note)
        )
      )
    },
    div(
      class = "result-section regression-result-panel complex-sample-correlation-result-panel",
      h3("Complex-sample correlation details"),
      div(
        class = "frequency-table-wrap",
        coefficient_html_table(table, compact = TRUE, compact_font_size = 13, compact_width = 70, compact_first_width = 130, compact_min_width = 720, note_line = detail_note)
      )
    )
  )
}

complex_sample_analysis_output <- function(type, data, target_values, input, prefix, variable_table = NULL, labels = character(0), category_table = NULL) {
  type <- as.character(type %||% "")
  design <- complex_sample_design_inputs(input, prefix)
  old_options <- options(survey.lonely.psu = complex_sample_lonely_psu_value(design$lonely_psu))
  on.exit(options(old_options), add = TRUE)
  if (identical(type, "frequencies")) {
    variables <- intersect(as.character(target_values$selected %||% character(0)), names(data))
    shiny::validate(shiny::need(length(variables) > 0, "Select at least one variable."))
    return(complex_sample_frequency_result(data, variables, input, prefix, variable_table, labels, category_table))
  }
  if (identical(type, "crosstabs")) {
    row_vars <- intersect(as.character(target_values$row %||% character(0)), names(data))
    col_vars <- intersect(as.character(target_values$column %||% character(0)), names(data))
    shiny::validate(shiny::need(length(row_vars) > 0 && length(col_vars) > 0, "Select row and column variables."))
    return(complex_sample_crosstab_results(data, row_vars, col_vars, input, prefix, variable_table, labels, category_table))
  }
  if (identical(type, "ttest_anova")) {
    dependents <- intersect(as.character(target_values$dependent %||% character(0)), names(data))
    factor_vars <- intersect(as.character(target_values$independent %||% character(0)), names(data))
    shiny::validate(shiny::need(length(dependents) > 0 && length(factor_vars) > 0, "Select dependent and independent variables."))
    return(complex_sample_group_result(data, dependents, factor_vars, input, prefix, variable_table, labels, category_table))
  }
  if (identical(type, "correlation")) {
    variables <- intersect(as.character(target_values$selected %||% character(0)), names(data))
    shiny::validate(shiny::need(length(variables) >= 2, "Select at least two variables."))
    return(complex_sample_correlation_result(data, variables, input, prefix, variable_table, labels, category_table))
  }
  if (identical(type, "regression") || identical(type, "logistic")) {
    outcomes <- intersect(as.character(target_values$outcome %||% character(0)), names(data))
    predictors <- intersect(as.character(target_values$predictors %||% character(0)), names(data))
    shiny::validate(shiny::need(length(outcomes) > 0 && length(predictors) > 0, "Select outcome and predictor variables."))
    return(complex_sample_regression_results(data, outcomes, predictors, input, prefix, logistic = identical(type, "logistic"), variable_info = variable_table, labels = labels, category_table = category_table))
  }
  NULL
}

complex_sample_result_panel <- function(prefix, target_specs, target_values, input, data = NULL, analysis_type = NULL, variable_table = NULL, labels = character(0), category_table = NULL, language = statedu_initial_language()) {
  run_value <- input[[paste0(prefix, "_run")]]
  if (is.null(run_value) || run_value == 0) {
    return(NULL)
  }
  result_language <- "en"
  variable_rows <- do.call(rbind, lapply(target_specs, function(spec) {
    data.frame(
      Section = complex_sample_ui_text(spec$key, result_language),
      Variables = complex_sample_display_names(target_values[[spec$key]], variable_table, labels),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
  names(variable_rows) <- c(complex_sample_ui_text("section", result_language), complex_sample_ui_text("variables", result_language))

  design_values <- c(
    strata = input[[paste0(prefix, "_strata")]] %||% "",
    cluster = input[[paste0(prefix, "_cluster")]] %||% "",
    weight = input[[paste0(prefix, "_weight")]] %||% "",
    fpc = input[[paste0(prefix, "_fpc")]] %||% "",
    variance_method = input[[paste0(prefix, "_variance_method")]] %||% "auto",
    lonely_psu = input[[paste0(prefix, "_lonely_psu")]] %||% "adjust",
    replicate_type = input[[paste0(prefix, "_replicate_type")]] %||% "auto",
    replicate_combined_weights = isTRUE(input[[paste0(prefix, "_replicate_combined_weights")]] %||% FALSE),
    subpopulation = input[[paste0(prefix, "_subpopulation")]] %||% ""
  )
  use_replicate_weights <- isTRUE(input[[paste0(prefix, "_use_replicate_weights")]] %||% FALSE)
  replicate_weights <- if (isTRUE(use_replicate_weights)) {
    as.character(input[[paste0(prefix, "_replicate_weights")]] %||% character(0))
  } else {
    character(0)
  }
  subpopulation_condition_type <- as.character(input[[paste0(prefix, "_subpopulation_condition_type")]] %||% "equals")
  subpopulation_condition <- if (identical(subpopulation_condition_type, "custom_formula")) {
    as.character(input[[paste0(prefix, "_subpopulation_condition")]] %||% "")
  } else {
    as.character(input[[paste0(prefix, "_subpopulation_condition_value")]] %||% input[[paste0(prefix, "_subpopulation_condition")]] %||% "")
  }
  design_rows <- data.frame(
    Variable = c(
      complex_sample_ui_text("strata", result_language),
      complex_sample_ui_text("cluster", result_language),
      complex_sample_ui_text("weight", result_language),
      complex_sample_ui_text("fpc", result_language),
      complex_sample_ui_text("variance_method", result_language),
      complex_sample_ui_text("lonely_psu", result_language),
      complex_sample_ui_text("use_replicate_weights", result_language),
      complex_sample_ui_text("replicate_weights", result_language),
      complex_sample_ui_text("replicate_type", result_language),
      complex_sample_ui_text("replicate_combined_weights", result_language),
      complex_sample_ui_text("subpopulation", result_language),
      complex_sample_ui_text("subpopulation_condition", result_language)
    ),
    Selection = c(
      complex_sample_display_names(design_values[["strata"]], variable_table, labels),
      complex_sample_display_names(design_values[["cluster"]], variable_table, labels),
      complex_sample_display_names(design_values[["weight"]], variable_table, labels),
      complex_sample_display_names(design_values[["fpc"]], variable_table, labels),
      as.character(design_values[["variance_method"]] %||% "auto"),
      as.character(design_values[["lonely_psu"]] %||% "adjust"),
      complex_sample_yes_no(use_replicate_weights, result_language),
      complex_sample_display_names(replicate_weights, variable_table, labels),
      if (isTRUE(use_replicate_weights)) as.character(design_values[["replicate_type"]] %||% "auto") else "",
      if (isTRUE(use_replicate_weights)) complex_sample_yes_no(design_values[["replicate_combined_weights"]], result_language) else "",
      complex_sample_display_names(design_values[["subpopulation"]], variable_table, labels),
      subpopulation_condition
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(design_rows) <- c(complex_sample_ui_text("variables", result_language), complex_sample_ui_text("design_summary", result_language))

  analysis_output <- NULL
  if (!is.null(data) && nzchar(as.character(analysis_type %||% ""))) {
    analysis_output <- complex_sample_analysis_output(
      analysis_type,
      data,
      target_values,
      input,
      prefix,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table
    )
  }

  shiny::tagList(
    div(class = "step-summary complex-sample-run-note", complex_sample_ui_text("setup_note", result_language)),
    analysis_result_table_section(
      complex_sample_ui_text("variables", result_language),
      variable_rows,
      class = "result-section regression-result-panel complex-sample-summary-section",
      table_fn = model_overview_html_table
    ),
    analysis_result_table_section(
      complex_sample_ui_text("design_summary", result_language),
      design_rows,
      class = "result-section regression-result-panel complex-sample-design-summary-section",
      table_fn = model_overview_html_table
    ),
    analysis_output
  )
}

register_complex_sample_handlers <- function(
  input,
  output,
  session,
  prefix,
  target_specs,
  target_values,
  selected_names_fn,
  all_variable_names_fn = selected_names_fn,
  variable_table_fn,
      design_variable_table_fn = variable_table_fn,
      dataset_fn = NULL,
      analysis_type = NULL,
  category_table_fn = NULL,
  labels_fn,
  mark_settings_dirty = NULL,
  app_language_fn = NULL,
  design_state = NULL
) {
  active_list <- reactiveVal(paste0(prefix, "_available"))
  available_id <- paste0(prefix, "_available")
  target_ids <- stats::setNames(paste0(prefix, "_", vapply(target_specs, `[[`, character(1), "key")), vapply(target_specs, `[[`, character(1), "key"))
  if (is.null(design_state)) {
    design_state <- reactiveVal(NULL)
  }
  result_cache <- reactiveVal(NULL)

  all_target_values <- function() {
    stats::setNames(lapply(names(target_ids), function(key) {
      target_values[[key]]()
    }), names(target_ids))
  }

  selected_target_variables <- function() {
    unique(as.character(unlist(lapply(target_values, function(value) value()), use.names = FALSE)))
  }

  selected_design_variables <- function() {
    design <- tryCatch(
      complex_sample_read_design_inputs(input, prefix),
      error = function(e) complex_sample_normalize_design_state(design_state())
    )
    if (is.null(design)) {
      design <- complex_sample_shared_design_defaults()
    }
    unique(as.character(c(
      design$strata,
      design$cluster,
      design$weight,
      design$fpc,
      if (isTRUE(design$use_replicate_weights)) design$replicate_weights else character(0),
      design$subpopulation
    )))
  }

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = prefix,
    title = paste0(complex_sample_ui_text(analysis_type %||% "frequencies", statedu_initial_language()), " Data Viewer"),
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = selected_target_variables,
    extra_variables_fn = selected_design_variables,
    variable_table_fn = design_variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  selected_for_render <- function() {
    ids <- c(
      available_id,
      unname(target_ids),
      paste0(prefix, "_strata"),
      paste0(prefix, "_cluster"),
      paste0(prefix, "_weight"),
      paste0(prefix, "_fpc"),
      paste0(prefix, "_variance_method"),
      paste0(prefix, "_lonely_psu"),
      paste0(prefix, "_use_replicate_weights"),
      paste0(prefix, "_replicate_weights"),
      paste0(prefix, "_replicate_type"),
      paste0(prefix, "_replicate_combined_weights"),
      paste0(prefix, "_subpopulation"),
      paste0(prefix, "_subpopulation_condition"),
      paste0(prefix, "_subpopulation_condition_type"),
      paste0(prefix, "_subpopulation_condition_value"),
      paste0(prefix, "_show_ci"),
      paste0(prefix, "_show_weighted_n"),
      paste0(prefix, "_show_missing"),
      paste0(prefix, "_show_df"),
      paste0(prefix, "_show_precision"),
      paste0(prefix, "_show_median"),
      paste0(prefix, "_show_effect_size"),
      paste0(prefix, "_show_model_fit"),
      paste0(prefix, "_show_wald"),
      paste0(prefix, "_post_hoc"),
      paste0(prefix, "_post_hoc_correction"),
      paste0(prefix, "_ordered_significance"),
      paste0(prefix, "_mean_sd"),
      paste0(prefix, "_crosstab_percent_basis"),
      paste0(prefix, "_crosstab_test_method"),
      paste0(prefix, "_correlation_method"),
      paste0(prefix, "_correlation_p_adjust"),
      paste0(prefix, "_correlation_matrix"),
      paste0(prefix, "_show_percent_ci"),
      paste0(prefix, "_trend_analysis"),
      paste0(prefix, "_design_options_tab")
    )
    selected <- stats::setNames(lapply(ids, function(id) isolate(input[[id]]) %||% character(0)), ids)
    shared_raw <- isolate(design_state())
    if (!is.null(shared_raw)) {
      shared_design <- complex_sample_normalize_design_state(shared_raw)
      design_ids <- complex_sample_design_input_ids(prefix)
      for (field in names(design_ids)) {
        id <- design_ids[[field]]
        if (!id %in% names(selected) || is.null(isolate(input[[id]]))) {
          selected[[id]] <- shared_design[[field]]
        }
      }
    }
    selected
  }

  design_inputs_for_render <- function() {
    ids <- c(
      paste0(prefix, "_strata"),
      paste0(prefix, "_cluster"),
      paste0(prefix, "_weight"),
      paste0(prefix, "_subpopulation"),
      paste0(prefix, "_subpopulation_condition"),
      paste0(prefix, "_subpopulation_condition_type"),
      paste0(prefix, "_subpopulation_condition_value")
    )
    stats::setNames(lapply(ids, function(id) input[[id]] %||% character(0)), ids)
  }

  allowed_for_target <- function(key) {
    spec <- target_specs[[match(key, vapply(target_specs, `[[`, character(1), "key"))]]
    analysis_allowed_variables(selected_names_fn(), variable_table_fn(), spec$measurements %||% analysis_allowed_measurements_all())
  }

  clean_targets <- function() {
    changed <- FALSE
    for (key in names(target_ids)) {
      current <- intersect(as.character(target_values[[key]]() %||% character(0)), allowed_for_target(key))
      if (!identical(current, target_values[[key]]())) {
        target_values[[key]](current)
        changed <- TRUE
      }
    }
    changed
  }

  output[[paste0(prefix, "_setup")]] <- renderUI({
    language <- statedu_current_language(app_language_fn)
    selected <- as.character(selected_names_fn() %||% character(0))
    all_names <- as.character(all_variable_names_fn() %||% selected)
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up regression.", language = language))
    }
    design_inputs_for_render()
    clean_targets()
        complex_sample_setup_panel(
      prefix = prefix,
      selected_names = selected,
      all_names = all_names,
      target_specs = target_specs,
      target_values = all_target_values(),
      variable_table = design_variable_table_fn(),
      labels = labels_fn(),
      language = language,
      selected = selected_for_render(),
      analysis_type = analysis_type,
      show_design_tabs = FALSE
    )
  })

  output[[paste0(prefix, "_reset_control")]] <- renderUI({
    design_changed <- !identical(
      complex_sample_read_design_inputs(input, prefix),
      complex_sample_shared_design_defaults()
    )
    enabled <- any(vapply(target_values, function(value) length(value()) > 0, logical(1))) ||
      isTRUE(design_changed) ||
      !isTRUE(complex_sample_options_are_default(input, prefix, analysis_type))
    analysis_reset_button(paste0(prefix, "_reset"), enabled = enabled, language = statedu_current_language(app_language_fn))
  })

  observeEvent(input[[paste0(prefix, "_run")]], {
    result_cache(complex_sample_result_panel(
      prefix,
      target_specs,
      isolate(all_target_values()),
      input,
      data = if (is.null(dataset_fn)) NULL else isolate(dataset_fn()),
      analysis_type = analysis_type,
      variable_table = isolate(design_variable_table_fn()),
      labels = isolate(labels_fn()),
      category_table = if (is.null(category_table_fn)) NULL else isolate(category_table_fn()),
      language = "en"
    ))
  }, ignoreInit = TRUE)

  output[[paste0(prefix, "_results")]] <- renderUI({
    result_cache()
  })

  observeEvent({
    option_ids <- paste0(prefix, "_", complex_sample_option_keys(analysis_type))
    lapply(option_ids, function(id) input[[id]])
  }, {
    if (!is.null(result_cache())) {
      result_cache(NULL)
    }
    if (!is.null(mark_settings_dirty)) mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input$main_menu, {
    if (identical(analysis_type, "crosstabs") && !identical(input$main_menu %||% "", "analysis_complex_crosstabs")) {
      result_cache(NULL)
    }
  }, ignoreInit = TRUE)

  observe({
    shared_raw <- design_state()
    if (is.null(shared_raw)) return()
    shared_design <- complex_sample_normalize_design_state(shared_raw)
    complex_sample_update_design_inputs(session, prefix, shared_design)
  })

  observeEvent({
    ids <- complex_sample_design_input_ids(prefix)
    lapply(unname(ids), function(id) input[[id]])
  }, {
    current <- complex_sample_read_design_inputs(input, prefix)
    previous <- complex_sample_normalize_design_state(design_state())
    if (!identical(current, previous)) {
      design_state(current)
      if (!is.null(mark_settings_dirty)) mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input[[paste0(available_id, "_active")]], active_list(available_id), ignoreInit = TRUE)
  for (key in names(target_ids)) {
    local({
      target_key <- key
      target_id <- target_ids[[target_key]]
      observeEvent(input[[paste0(target_id, "_active")]], active_list(target_id), ignoreInit = TRUE)
      observe({
        label <- if (identical(active_list(), target_id) && length(input[[target_id]] %||% character(0)) > 0) "<" else ">"
        updateActionButton(session, paste0(prefix, "_assign_", target_key), label = label)
      })
      observeEvent(input[[paste0(prefix, "_assign_", target_key)]], {
        if (identical(active_list(), target_id) && length(input[[target_id]] %||% character(0)) > 0) {
          selected <- intersect(as.character(input[[target_id]] %||% character(0)), target_values[[target_key]]())
          if (length(selected) == 0) return()
          target_values[[target_key]](setdiff(target_values[[target_key]](), selected))
          active_list(available_id)
        } else {
          selected <- intersect(as.character(input[[available_id]] %||% character(0)), allowed_for_target(target_key))
          if (length(selected) == 0) return()
          for (other_key in names(target_ids)) {
            target_values[[other_key]](setdiff(target_values[[other_key]](), selected))
          }
          target_values[[target_key]](unique(c(target_values[[target_key]](), selected)))
          active_list(target_id)
        }
        if (!is.null(mark_settings_dirty)) mark_settings_dirty()
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0(target_id, "_doubleclick")]], {
        value <- as.character(input[[paste0(target_id, "_doubleclick")]]$value %||% "")
        if (!nzchar(value)) return()
        target_values[[target_key]](setdiff(target_values[[target_key]](), value))
        active_list(available_id)
        if (!is.null(mark_settings_dirty)) mark_settings_dirty()
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0(prefix, "_", target_key, "_up")]], {
        updated <- move_order_item(target_values[[target_key]](), input[[target_id]], "up")
        if (isTRUE(updated$changed)) {
          target_values[[target_key]](updated$order)
          updateSelectInput(session, target_id, selected = updated$selected)
          if (!is.null(mark_settings_dirty)) mark_settings_dirty()
        }
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0(prefix, "_", target_key, "_down")]], {
        updated <- move_order_item(target_values[[target_key]](), input[[target_id]], "down")
        if (isTRUE(updated$changed)) {
          target_values[[target_key]](updated$order)
          updateSelectInput(session, target_id, selected = updated$selected)
          if (!is.null(mark_settings_dirty)) mark_settings_dirty()
        }
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    ids <- c(available_id, unname(target_ids))
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- unique(as.character(drop$values %||% character(0)))
    values <- values[nzchar(values)]
    if (!source %in% ids || !target %in% ids || identical(source, target) || length(values) == 0) {
      return()
    }
    if (identical(target, available_id)) {
      for (key in names(target_ids)) {
        target_values[[key]](setdiff(target_values[[key]](), values))
      }
      active_list(available_id)
    } else {
      target_key <- names(target_ids)[match(target, unname(target_ids))]
      values <- intersect(values, allowed_for_target(target_key))
      if (length(values) == 0) return()
      for (key in names(target_ids)) {
        target_values[[key]](setdiff(target_values[[key]](), values))
      }
      target_values[[target_key]](unique(c(target_values[[target_key]](), values)))
      active_list(target)
    }
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = ids))
    if (!is.null(mark_settings_dirty)) mark_settings_dirty()
  }, ignoreInit = TRUE)

  observeEvent(input[[paste0(prefix, "_reset")]], {
    result_cache(NULL)
    for (key in names(target_ids)) {
      target_values[[key]](character(0))
    }
    updateSelectInput(session, paste0(prefix, "_strata"), selected = "")
    updateSelectInput(session, paste0(prefix, "_cluster"), selected = "")
    updateSelectInput(session, paste0(prefix, "_weight"), selected = "")
    updateSelectInput(session, paste0(prefix, "_fpc"), selected = "")
    updateSelectInput(session, paste0(prefix, "_variance_method"), selected = "auto")
    updateSelectInput(session, paste0(prefix, "_lonely_psu"), selected = "adjust")
    updateCheckboxInput(session, paste0(prefix, "_use_replicate_weights"), value = FALSE)
    updateSelectInput(session, paste0(prefix, "_replicate_weights"), selected = character(0))
    updateSelectInput(session, paste0(prefix, "_replicate_type"), selected = "auto")
    updateCheckboxInput(session, paste0(prefix, "_replicate_combined_weights"), value = FALSE)
    updateSelectInput(session, paste0(prefix, "_subpopulation"), selected = "")
    updateSelectInput(session, paste0(prefix, "_subpopulation_condition_type"), selected = "equals")
    updateTextInput(session, paste0(prefix, "_subpopulation_condition"), value = "")
    updateTextInput(session, paste0(prefix, "_subpopulation_condition_value"), value = "")
    design_state(complex_sample_shared_design_defaults())
    updateCheckboxInput(session, paste0(prefix, "_show_ci"), value = TRUE)
    updateCheckboxInput(session, paste0(prefix, "_show_weighted_n"), value = FALSE)
    updateCheckboxInput(session, paste0(prefix, "_show_missing"), value = TRUE)
    updateCheckboxInput(session, paste0(prefix, "_show_df"), value = TRUE)
    updateCheckboxInput(session, paste0(prefix, "_show_precision"), value = FALSE)
    updateCheckboxInput(session, paste0(prefix, "_show_median"), value = FALSE)
    updateCheckboxInput(session, paste0(prefix, "_show_effect_size"), value = TRUE)
    updateCheckboxInput(session, paste0(prefix, "_show_model_fit"), value = TRUE)
    updateCheckboxInput(session, paste0(prefix, "_show_wald"), value = TRUE)
    updateCheckboxInput(session, paste0(prefix, "_post_hoc"), value = FALSE)
    updateSelectInput(session, paste0(prefix, "_post_hoc_correction"), selected = statedu_multiple_correction_default())
    updateCheckboxInput(session, paste0(prefix, "_ordered_significance"), value = FALSE)
    updateCheckboxInput(session, paste0(prefix, "_mean_sd"), value = FALSE)
    updateSelectInput(session, paste0(prefix, "_crosstab_percent_basis"), selected = "row")
    updateSelectInput(session, paste0(prefix, "_crosstab_test_method"), selected = "F")
    updateSelectInput(session, paste0(prefix, "_correlation_method"), selected = "pearson")
    updateSelectInput(session, paste0(prefix, "_correlation_p_adjust"), selected = statedu_multiple_correction_default())
    updateCheckboxInput(session, paste0(prefix, "_correlation_matrix"), value = TRUE)
    updateCheckboxInput(session, paste0(prefix, "_show_percent_ci"), value = FALSE)
    updateCheckboxInput(session, paste0(prefix, "_trend_analysis"), value = FALSE)
    updateTabsetPanel(session, paste0(prefix, "_design_options_tab"), selected = complex_sample_ui_text("design_tab", statedu_current_language(app_language_fn)))
    session$sendCustomMessage(
      "easyflow-clear-transfer-selection",
      list(inputIds = c(available_id, unname(target_ids)))
    )
    if (!is.null(mark_settings_dirty)) mark_settings_dirty()
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

register_complex_sample_design_handlers <- function(
  input,
  output,
  session,
  prefix = "complex_design",
  variable_names_fn,
  variable_table_fn,
  labels_fn,
  design_state,
  mark_settings_dirty = NULL,
  app_language_fn = NULL
) {
  if (is.null(design_state)) {
    design_state <- reactiveVal(NULL)
  }

  selected_for_render <- function() {
    ids <- unname(complex_sample_design_input_ids(prefix))
    selected <- stats::setNames(lapply(ids, function(id) isolate(input[[id]]) %||% character(0)), ids)
    shared_design <- complex_sample_normalize_design_state(isolate(design_state()))
    design_ids <- complex_sample_design_input_ids(prefix)
    for (field in names(design_ids)) {
      selected[[design_ids[[field]]]] <- shared_design[[field]]
    }
    selected
  }

  output[[paste0(prefix, "_setup")]] <- renderUI({
    language <- statedu_current_language(app_language_fn)
    names <- as.character(variable_names_fn() %||% character(0))
    if (length(names) == 0) {
      return(setup_empty_message("Complete Step 1 in the Data tab before setting complex-sample design variables.", language = language))
    }
    complex_sample_design_setup_panel(
      prefix = prefix,
      variable_names = names,
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      language = language,
      selected = selected_for_render()
    )
  })

  observe({
    shared_design <- complex_sample_normalize_design_state(design_state())
    complex_sample_update_design_inputs(session, prefix, shared_design)
  })

  observeEvent({
    ids <- complex_sample_design_input_ids(prefix)
    lapply(unname(ids), function(id) input[[id]])
  }, {
    current <- complex_sample_read_design_inputs(input, prefix)
    previous <- complex_sample_normalize_design_state(design_state())
    if (!identical(current, previous)) {
      design_state(current)
      if (!is.null(mark_settings_dirty)) mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  invisible(TRUE)
}

complex_sample_target_specs <- function(type) {
  switch(
    type,
    frequencies = list(
      list(key = "selected", label = "Selected Variables", measurements = analysis_allowed_measurements_all(), size = 17)
    ),
    correlation = list(
      list(key = "selected", label = "Selected Variables", measurements = c("continuous", "ordered"), size = 17)
    ),
    crosstabs = list(
      list(key = "column", label = "Column variable", measurements = crosstab_allowed_measurements(), size = 4),
      list(key = "row", label = "Row variable", measurements = crosstab_allowed_measurements(), size = 8)
    ),
    ttest_anova = list(
      list(key = "dependent", label = "Dependent variables", measurements = c("ordered", "continuous"), size = 4),
      list(key = "independent", label = "Independent variables", measurements = c("binary", "category", "ordered"), size = 8)
    ),
    regression = list(
      list(key = "outcome", label = "Dependent variable", measurements = "continuous", size = 4),
      list(key = "predictors", label = "Independent variables", measurements = analysis_allowed_measurements_all(), size = 8)
    ),
    logistic = list(
      list(key = "outcome", label = "Dependent variable", measurements = c("binary", "category", "ordered"), size = 4),
      list(key = "predictors", label = "Independent variables", measurements = analysis_allowed_measurements_all(), size = 8)
    )
  )
}
