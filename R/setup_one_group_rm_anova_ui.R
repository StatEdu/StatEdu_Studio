# Setup UI for within-subject treatment repeated-measures ANOVA.

one_group_rm_ui_text <- function(key, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  fallback <- switch(
    as.character(key %||% "")[[1]],
    title = "Within-subject treatment repeated-measures ANOVA",
    subtitle = "Compare experimental and control treatment measured in the same subjects across time.",
    input_format = "Input data format",
    wide = "WIDE",
    long = "LONG",
    wide_help = "Assign the same number of time-ordered columns to Experimental and Control treatment, plus optional subject-level covariates.",
    long_help = "Assign Dependent variable, Subject ID, Time, Independent variable (treatment group), and optional Covariates. Each subject must receive both treatments.",
    subject_id = "Subject ID",
    group_variable = "Independent variable (treatment group)",
    time_variable = "Time variable",
    outcome_variable = "Dependent variable",
    experimental_variables = "Experimental-treatment variables (time order)",
    control_variables = "Control-treatment variables (same time order)",
    options = "Options",
    check_assumptions = "Check normality and sphericity",
    posthoc = "Post-hoc pairwise comparisons",
    mean_sd = "Show M \u00B1 SD",
    key
  )
  statedu_t(paste0("one_group_rm_anova.ui.", key), language, fallback = fallback)
}

one_group_rm_anova_setup_state <- function(
  selected_names,
  input_format = "wide",
  experimental_variables = character(0),
  control_variables = character(0),
  id_variable = character(0),
  group_variable = character(0),
  time_variable = character(0),
  outcome_variable = character(0),
  covariates = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_experimental = NULL,
  selected_control = NULL,
  selected_id = NULL,
  selected_group = NULL,
  selected_time = NULL,
  selected_outcome = NULL,
  selected_covariates = NULL,
  assumption_check = TRUE,
  posthoc = TRUE,
  adjustment = "holm",
  mean_sd = TRUE,
  language = statedu_initial_language()
) {
  selected <- as.character(selected_names %||% character(0))
  input_format <- if (identical(input_format, "long")) "long" else "wide"

  covariates <- intersect(as.character(covariates %||% character(0)), selected)
  experimental_variables <- setdiff(intersect(as.character(experimental_variables %||% character(0)), selected), covariates)
  control_variables <- setdiff(intersect(as.character(control_variables %||% character(0)), selected), c(experimental_variables, covariates))
  wide_available <- setdiff(selected, c(experimental_variables, control_variables, covariates))

  id_variable <- head(intersect(as.character(id_variable %||% character(0)), selected), 1L)
  group_variable <- head(setdiff(intersect(as.character(group_variable %||% character(0)), selected), id_variable), 1L)
  time_variable <- head(setdiff(intersect(as.character(time_variable %||% character(0)), selected), c(id_variable, group_variable)), 1L)
  outcome_variable <- head(setdiff(intersect(as.character(outcome_variable %||% character(0)), selected), c(id_variable, group_variable, time_variable)), 1L)
  covariates <- setdiff(covariates, c(id_variable, group_variable, time_variable, outcome_variable))
  long_available <- setdiff(selected, c(id_variable, group_variable, time_variable, outcome_variable, covariates))

  available <- if (identical(input_format, "long")) long_available else wide_available
  list(
    selected_names = selected,
    input_format = input_format,
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    experimental_variables = experimental_variables,
    experimental_items = analysis_variable_items(experimental_variables, variable_table, labels),
    experimental_selected = selected_order_items(selected_experimental, experimental_variables),
    control_variables = control_variables,
    control_items = analysis_variable_items(control_variables, variable_table, labels),
    control_selected = selected_order_items(selected_control, control_variables),
    id_variable = id_variable,
    id_items = analysis_variable_items(id_variable, variable_table, labels),
    id_selected = selected_order_items(selected_id, id_variable),
    group_variable = group_variable,
    group_items = analysis_variable_items(group_variable, variable_table, labels),
    group_selected = selected_order_items(selected_group, group_variable),
    time_variable = time_variable,
    time_items = analysis_variable_items(time_variable, variable_table, labels),
    time_selected = selected_order_items(selected_time, time_variable),
    outcome_variable = outcome_variable,
    outcome_items = analysis_variable_items(outcome_variable, variable_table, labels),
    outcome_selected = selected_order_items(selected_outcome, outcome_variable),
    covariates = covariates,
    covariate_items = analysis_variable_items(covariates, variable_table, labels),
    covariate_selected = selected_order_items(selected_covariates, covariates),
    assumption_check = isTRUE(assumption_check),
    posthoc = isTRUE(posthoc),
    adjustment = if (identical(adjustment, "bonferroni")) "bonferroni" else "holm",
    mean_sd = isTRUE(mean_sd),
    move_disabled = length(selected) == 0L,
    language = normalize_app_language(language)
  )
}

one_group_rm_options_panel <- function(state) {
  language <- state$language
  div(
    class = "analysis-options-panel ttest-anova-options one-group-rm-options-block",
    analysis_option_group(
      one_group_rm_ui_text("options", language),
      list(
        list(id = "one_group_rm_assumption_check", label = one_group_rm_ui_text("check_assumptions", language), value = state$assumption_check),
        list(id = "one_group_rm_posthoc", label = one_group_rm_ui_text("posthoc", language), value = state$posthoc),
        list(id = "one_group_rm_mean_sd", label = one_group_rm_ui_text("mean_sd", language), value = state$mean_sd)
      ),
      language = language
    ),
    analysis_radio_group(
      analysis_ui_text("Post-hoc correction", language),
      "one_group_rm_adjustment",
      choices = analysis_ui_choices(c("Holm Bonferroni" = "holm", "Bonferroni correction" = "bonferroni"), language),
      selected = state$adjustment
    )
  )
}

one_group_rm_wide_setup_grid <- function(state) {
  language <- state$language
  div(
    class = "ttest-anova-setup-grid one-group-rm-transfer-grid one-group-rm-wide-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input("one_group_rm_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls one-group-rm-transfer-controls one-group-rm-wide-controls",
      div(class = "one-group-rm-transfer-button-row", actionButton("one_group_rm_experimental_move", ">", class = "btn btn-default analysis-move-button")),
      div(class = "one-group-rm-transfer-button-row", actionButton("one_group_rm_control_move", ">", class = "btn btn-default analysis-move-button")),
      div(class = "one-group-rm-transfer-button-row", actionButton("one_group_rm_covariates_move", ">", class = "btn btn-default analysis-move-button"))
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel one-group-rm-target-panel",
      div(
        class = "one-group-rm-target-section one-group-rm-wide-target-section",
        analysis_field_label_tag(one_group_rm_ui_text("experimental_variables", language), c("ordered", "continuous"), language = language),
        analysis_transfer_listbox_input("one_group_rm_experimental_variables", state$experimental_items, selected = state$experimental_selected, size = 5, important_height = TRUE, min_size = 3),
        div(class = "analysis-order-actions", actionButton("one_group_rm_experimental_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"), actionButton("one_group_rm_experimental_down", analysis_ui_text("Down", language), class = "btn-default btn-sm"))
      ),
      div(
        class = "one-group-rm-target-section one-group-rm-wide-target-section",
        analysis_field_label_tag(one_group_rm_ui_text("control_variables", language), c("ordered", "continuous"), language = language),
        analysis_transfer_listbox_input("one_group_rm_control_variables", state$control_items, selected = state$control_selected, size = 5, important_height = TRUE, min_size = 3),
        div(class = "analysis-order-actions", actionButton("one_group_rm_control_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"), actionButton("one_group_rm_control_down", analysis_ui_text("Down", language), class = "btn-default btn-sm"))
      ),
      div(
        class = "one-group-rm-target-section one-group-rm-wide-covariate-section",
        analysis_field_label_tag(analysis_ui_text("Covariates", language), c("binary", "category", "ordered", "continuous"), language = language),
        analysis_transfer_listbox_input("one_group_rm_covariates", state$covariate_items, selected = state$covariate_selected, size = 3, important_height = TRUE, min_size = 2)
      )
    ),
    div(class = "ttest-anova-options-column", one_group_rm_options_panel(state))
  )
}

one_group_rm_long_setup_grid <- function(state) {
  language <- state$language
  role_section <- function(label, measurements, input_id, items, selected, size = 2L, height_offset = 0L, role_class = "") {
    div(
      class = paste("one-group-rm-target-section one-group-rm-long-target-section", role_class),
      analysis_field_label_tag(label, measurements, language = language),
      analysis_transfer_listbox_input(input_id, items, selected = selected, size = size, important_height = TRUE, height_offset = height_offset, min_size = 2)
    )
  }
  div(
    class = "ttest-anova-setup-grid one-group-rm-transfer-grid one-group-rm-long-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input("one_group_rm_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls one-group-rm-transfer-controls one-group-rm-long-controls",
      div(class = "one-group-rm-transfer-button-row", actionButton("one_group_rm_outcome_move", ">", class = "btn btn-default analysis-move-button")),
      div(class = "one-group-rm-transfer-button-row", actionButton("one_group_rm_id_move", ">", class = "btn btn-default analysis-move-button")),
      div(class = "one-group-rm-transfer-button-row", actionButton("one_group_rm_time_move", ">", class = "btn btn-default analysis-move-button")),
      div(class = "one-group-rm-transfer-button-row", actionButton("one_group_rm_group_move", ">", class = "btn btn-default analysis-move-button")),
      div(class = "one-group-rm-transfer-button-row", actionButton("one_group_rm_covariates_move", ">", class = "btn btn-default analysis-move-button"))
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel one-group-rm-target-panel one-group-rm-long-target-panel",
      role_section(one_group_rm_ui_text("outcome_variable", language), c("ordered", "continuous"), "one_group_rm_outcome_variable", state$outcome_items, state$outcome_selected, height_offset = 12L, role_class = "one-group-rm-outcome-section"),
      role_section(one_group_rm_ui_text("subject_id", language), c("binary", "category", "ordered", "continuous"), "one_group_rm_id_variable", state$id_items, state$id_selected, role_class = "one-group-rm-id-section"),
      role_section(one_group_rm_ui_text("time_variable", language), c("binary", "category", "ordered", "continuous"), "one_group_rm_time_variable", state$time_items, state$time_selected, height_offset = 12L, role_class = "one-group-rm-time-section"),
      role_section(one_group_rm_ui_text("group_variable", language), c("binary", "category", "ordered"), "one_group_rm_group_variable", state$group_items, state$group_selected, height_offset = 12L, role_class = "one-group-rm-group-section"),
      role_section(analysis_ui_text("Covariates", language), c("binary", "category", "ordered", "continuous"), "one_group_rm_covariates", state$covariate_items, state$covariate_selected, size = 3L, role_class = "one-group-rm-covariate-section")
    ),
    div(class = "ttest-anova-options-column", one_group_rm_options_panel(state))
  )
}

one_group_rm_anova_setup_panel <- function(state) {
  language <- state$language
  div(
    class = "one-group-rm-anova-setup",
    div(
      class = "analysis-option-group one-group-rm-format",
      div(class = "analysis-option-title", one_group_rm_ui_text("input_format", language)),
      radioButtons(
        "one_group_rm_input_format",
        label = NULL,
        choices = stats::setNames(c("wide", "long"), c(one_group_rm_ui_text("wide", language), one_group_rm_ui_text("long", language))),
        selected = state$input_format,
        inline = TRUE
      ),
      div(
        class = "analysis-option-subtitle one-group-rm-format-help",
        if (identical(state$input_format, "long")) one_group_rm_ui_text("long_help", language) else one_group_rm_ui_text("wide_help", language)
      )
    ),
    if (identical(state$input_format, "long")) one_group_rm_long_setup_grid(state) else one_group_rm_wide_setup_grid(state)
  )
}
