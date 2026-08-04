# Mixed repeated-measures ANOVA setup UI.

mixed_rm_anova_setup_state <- function(
  selected_names,
  group_variable = character(0),
  repeated_variables = character(0),
  covariates = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_group = NULL,
  selected_repeated = NULL,
  selected_covariates = NULL,
  options_tab = "Checks",
  assumption_check = TRUE,
  posthoc = TRUE,
  within_group_comparison = TRUE,
  between_time_group_comparison = FALSE,
  analysis_population = "pp",
  adjustment = statedu_multiple_correction_default(),
  time_labels = NULL,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  selected <- as.character(selected_names %||% character(0))
  group_variable <- intersect(as.character(group_variable %||% character(0)), selected)
  repeated_variables <- intersect(as.character(repeated_variables %||% character(0)), selected)
  repeated_variables <- setdiff(repeated_variables, group_variable)
  covariates <- intersect(as.character(covariates %||% character(0)), selected)
  covariates <- setdiff(covariates, unique(c(group_variable, repeated_variables)))
  available <- setdiff(selected, unique(c(group_variable, repeated_variables, covariates)))
  time_count <- max(2L, length(repeated_variables))
  defaults <- paired_rm_time_header_labels(time_count)
  time_labels <- as.character(time_labels %||% defaults)
  if (length(time_labels) < time_count) {
    time_labels <- c(time_labels, defaults[seq.int(length(time_labels) + 1L, time_count)])
  }
  time_labels <- trimws(time_labels[seq_len(time_count)])
  time_labels <- ifelse(nzchar(time_labels), time_labels, defaults)
  options_tab <- as.character(options_tab %||% "Checks")[[1]]
  if (options_tab %in% c("Post-hoc")) {
    options_tab <- "Checks"
  } else if (options_tab %in% c("Summary", "Between")) {
    options_tab <- "Output"
  }
  if (!options_tab %in% c("Checks", "Output", "Labels")) {
    options_tab <- "Checks"
  }
  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    group_variable = group_variable,
    group_items = analysis_variable_items(group_variable, variable_table, labels),
    group_selected = selected_order_items(selected_group, group_variable),
    repeated_variables = repeated_variables,
    repeated_items = analysis_variable_items(repeated_variables, variable_table, labels),
    repeated_selected = selected_order_items(selected_repeated, repeated_variables),
    covariates = covariates,
    covariate_items = analysis_variable_items(covariates, variable_table, labels),
    covariate_selected = selected_order_items(selected_covariates, covariates),
    options_tab = options_tab,
    assumption_check = isTRUE(assumption_check),
    posthoc = isTRUE(posthoc),
    within_group_comparison = isTRUE(within_group_comparison),
    between_time_group_comparison = isTRUE(between_time_group_comparison),
    analysis_population = if (identical(analysis_population, "itt")) "itt" else "pp",
    adjustment = if (identical(adjustment, "bonferroni")) "bonferroni" else "holm",
    time_labels = time_labels,
    move_disabled = length(selected) == 0,
    language = language
  )
}

mixed_rm_anova_time_label_inputs <- function(time_labels, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  labels <- as.character(time_labels %||% paired_rm_time_header_labels(2L))
  div(
    class = "paired-rm-time-labels mixed-rm-time-labels",
    div(class = "analysis-option-title", analysis_ui_text("Repeated variable labels", language)),
    lapply(seq_along(labels), function(index) {
      textInput(
        inputId = paste0("mixed_rm_anova_time_label_", index),
        label = paste0(index, if (index == 1L) "st" else if (index == 2L) "nd" else if (index == 3L) "rd" else "th"),
        value = labels[[index]],
        width = "100%"
      )
    })
  )
}

mixed_rm_anova_ui_text <- function(key, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  key <- as.character(key %||% "")[[1]]
  if (identical(language, "ko")) {
    return(switch(
      key,
      labels = "\ub77c\ubca8",
      summary_options = "\uc694\uc57d \uc635\uc158",
      within_group_comparison = "\uad70\ub0b4 \uc2dc\uc810 \ube44\uad50",
      between_group_comparison = "\uc2dc\uc810\ubcc4 \uad70\uac04 \ube44\uad50",
      assumption_review = "\uac00\uc815\uac80\ud1a0",
      assumption_review_check = "\uac00\uc815\uac80\ud1a0",
      analysis_population = "\ubd84\uc11d \ub300\uc0c1",
      per_protocol = "PP / \uc644\uc804 \uc0ac\ub840",
      itt_available = "ITT / \uad00\uce21 \uc790\ub8cc \ud63c\ud569\ubaa8\ud615",
      key
    ))
  }
  switch(
    key,
    labels = "Labels",
    summary_options = "Summary options",
    within_group_comparison = "Within-group time comparison",
    between_group_comparison = "Between-group comparison",
    assumption_review = "Assumption review",
    assumption_review_check = "Check assumptions",
    analysis_population = "Analysis population",
    per_protocol = "PP / complete case",
    itt_available = "ITT / available mixed model",
    key
  )
}

mixed_rm_anova_setup_panel <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  options_content <- analysis_options_tabs_panel(
    id = "mixed_rm_anova_options_tab",
    selected = state$options_tab,
    class = "ttest-anova-options paired-options mixed-rm-options",
    tabPanel(
      analysis_ui_text("Checks", language),
      value = "Checks",
      div(
        class = "factor-options-tab-content ttest-anova-options-tab-content paired-options-tab-content mixed-rm-options-tab-content",
        analysis_option_group(
          mixed_rm_anova_ui_text("assumption_review", language),
          list(list(id = "mixed_rm_anova_assumption_check", label = mixed_rm_anova_ui_text("assumption_review_check", language), value = state$assumption_check)),
          language = language
        ),
        analysis_radio_group(
          mixed_rm_anova_ui_text("analysis_population", language),
          "mixed_rm_anova_analysis_population",
          choices = stats::setNames(
            c("pp", "itt"),
            c(mixed_rm_anova_ui_text("per_protocol", language), mixed_rm_anova_ui_text("itt_available", language))
          ),
          selected = state$analysis_population
        ),
        analysis_option_group(
          "Post-hoc",
          list(list(id = "mixed_rm_anova_posthoc", label = "Post-hoc", value = state$posthoc)),
          language = language
        ),
        analysis_radio_group(
          analysis_ui_text("Post-hoc correction", language),
          "mixed_rm_anova_adjustment",
          choices = analysis_ui_choices(c("Holm Bonferroni" = "holm", "Bonferroni correction" = "bonferroni"), language),
          selected = state$adjustment
        )
      )
    ),
    tabPanel(
      analysis_ui_text("Output", language),
      value = "Output",
      div(
        class = "factor-options-tab-content ttest-anova-options-tab-content paired-options-tab-content mixed-rm-options-tab-content",
        analysis_option_group(
          mixed_rm_anova_ui_text("summary_options", language),
          list(list(id = "mixed_rm_anova_within_group_comparison", label = mixed_rm_anova_ui_text("within_group_comparison", language), value = state$within_group_comparison)),
          language = language
        ),
        analysis_option_group(
          mixed_rm_anova_ui_text("between_group_comparison", language),
          list(list(id = "mixed_rm_anova_between_time_group_comparison", label = mixed_rm_anova_ui_text("between_group_comparison", language), value = state$between_time_group_comparison)),
          language = language
        )
      )
    ),
    tabPanel(
      mixed_rm_anova_ui_text("labels", language),
      value = "Labels",
      div(
        class = "factor-options-tab-content ttest-anova-options-tab-content paired-options-tab-content mixed-rm-options-tab-content",
        mixed_rm_anova_time_label_inputs(state$time_labels, language)
      )
    )
  )
  div(
    class = "ttest-anova-setup-grid paired-setup-grid paired-rm-setup-grid mixed-rm-anova-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input("mixed_rm_anova_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls paired-transfer-controls mixed-rm-transfer-controls",
      div(
        class = "mixed-rm-transfer-button-row mixed-rm-repeated-button-row",
        actionButton("mixed_rm_anova_repeated_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && length(state$repeated_variables) == 0) "disabled" else NULL)
      ),
      div(
        class = "mixed-rm-transfer-button-row mixed-rm-independent-button-row",
        actionButton("mixed_rm_anova_group_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && length(state$group_variable) == 0) "disabled" else NULL)
      ),
      div(
        class = "mixed-rm-transfer-button-row mixed-rm-covariate-button-row",
        actionButton("mixed_rm_anova_covariate_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && length(state$covariates) == 0) "disabled" else NULL)
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel mixed-rm-target-panel",
      div(
        class = "mixed-rm-target-section mixed-rm-repeated-section",
        analysis_field_label_tag("Repeated-measures variables", c("ordered", "continuous"), language = language),
        analysis_transfer_listbox_input("mixed_rm_anova_repeated", state$repeated_items, selected = state$repeated_selected, size = 6, important_height = TRUE, height_offset = 16, min_size = 2),
        div(class = "analysis-order-actions paired-order-actions", actionButton("mixed_rm_anova_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"), actionButton("mixed_rm_anova_down", analysis_ui_text("Down", language), class = "btn-default btn-sm"))
      ),
      div(
        class = "mixed-rm-target-section mixed-rm-independent-section",
        analysis_field_label_tag("Independent variables", c("binary", "category", "ordered"), language = language),
        analysis_transfer_listbox_input("mixed_rm_anova_group", state$group_items, selected = state$group_selected, size = 3, important_height = TRUE, min_size = 2)
      ),
      div(
        class = "mixed-rm-target-section mixed-rm-covariate-section",
        analysis_field_label_tag("Covariates", c("binary", "category", "ordered", "continuous"), language = language),
        analysis_transfer_listbox_input("mixed_rm_anova_covariates", state$covariate_items, selected = state$covariate_selected, size = 3, important_height = TRUE, min_size = 2)
      )
    ),
    div(
      class = "ttest-anova-options-column",
      options_content
    )
  )
}
