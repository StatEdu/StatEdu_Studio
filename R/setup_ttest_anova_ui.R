# t-test / ANOVA setup UI state and panel.

ttest_anova_method_choices <- function() {
  c(
    "Independent samples t-test" = "independent_t",
    "One-way ANOVA" = "anova",
    "Mann-Whitney U (Wilcoxon rank sum)" = "mann_whitney",
    "Kruskal-Wallis" = "kruskal_wallis"
  )
}

ttest_anova_setup_state <- function(
  selected_names,
  dependent_variables = character(0),
  factor_variables = character(0),
  variable_table = NULL,
  labels = character(0),
  method_value = NULL,
  selected_available = NULL,
  selected_dependent = NULL,
  selected_factor = NULL,
  normality_enabled = TRUE,
  normality_study_type = NULL,
  normality_method = NULL,
  normality_survey_method = NULL,
  normality_experimental_method = NULL,
  normality_skew_kurtosis_cutoff = NULL,
  normality_skew_kurtosis = FALSE,
  normality_ks = FALSE,
  trend_analysis = FALSE,
  post_hoc_method = NULL,
  nonparametric_post_hoc_method = NULL,
  post_hoc = FALSE,
  ordered_significance = FALSE,
  effect_size = TRUE,
  show_df = FALSE,
  mean_sd = FALSE,
  options_tab = "Normality",
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  selected <- as.character(selected_names %||% character(0))
  dependent_variables <- intersect(as.character(dependent_variables %||% character(0)), selected)
  factor_variables <- intersect(as.character(factor_variables %||% character(0)), selected)
  assigned <- unique(c(dependent_variables, factor_variables))
  available <- setdiff(selected, assigned)
  dependent_allowed <- analysis_allowed_variables(selected, variable_table, c("ordered", "continuous"))
  factor_allowed <- analysis_allowed_variables(selected, variable_table, c("binary", "category", "ordered"))
  method_choices <- ttest_anova_method_choices()
  current_method <- as.character(method_value %||% unname(method_choices)[[1]])
  if (!current_method %in% unname(method_choices)) {
    current_method <- unname(method_choices)[[1]]
  }
  normality_study_choices <- c(
    "Survey study" = "survey",
    "Experimental study" = "experimental"
  )
  current_normality_study <- as.character(normality_study_type %||% "survey")
  if (!current_normality_study %in% unname(normality_study_choices)) {
    current_normality_study <- "survey"
  }
  survey_normality_choices <- c(
    "Skewness, Kurtosis" = "skew_kurtosis",
    "Shapiro-Wilk" = "sw",
    "Kolmogorov-Smirnov" = "ks"
  )
  experimental_normality_choices <- c(
    "Shapiro-Wilk" = "sw",
    "Kolmogorov-Smirnov" = "ks"
  )
  skew_kurtosis_cutoff_choices <- c(
    "2/5 : conservative" = "2_5",
    "2/7 : Standard" = "2_7",
    "3/7 : lenient" = "3_7"
  )
  current_normality <- as.character(
    normality_method %||% if (isTRUE(normality_ks)) "ks" else "skew_kurtosis"
  )
  if (!current_normality %in% unname(survey_normality_choices)) {
    current_normality <- "skew_kurtosis"
  }
  current_survey_normality <- as.character(normality_survey_method %||% current_normality)
  if (!current_survey_normality %in% unname(survey_normality_choices)) {
    current_survey_normality <- "skew_kurtosis"
  }
  current_experimental_normality <- as.character(normality_experimental_method %||% current_normality)
  if (!current_experimental_normality %in% unname(experimental_normality_choices)) {
    current_experimental_normality <- "sw"
  }
  current_skew_kurtosis_cutoff <- as.character(normality_skew_kurtosis_cutoff %||% "2_7")
  if (!current_skew_kurtosis_cutoff %in% unname(skew_kurtosis_cutoff_choices)) {
    current_skew_kurtosis_cutoff <- "2_7"
  }
  post_hoc_choices <- c(
    "Tukey HSD" = "tukey",
    "Duncan multiple range test" = "duncan",
    "Scheffe post-hoc test" = "scheffe",
    "Bonferroni post-hoc test" = "bonferroni"
  )
  current_post_hoc <- as.character(post_hoc_method %||% "scheffe")
  if (!current_post_hoc %in% unname(post_hoc_choices)) {
    current_post_hoc <- "scheffe"
  }
  nonparametric_post_hoc_choices <- c(
    "Bonferroni correction" = "bonferroni",
    "Holm Bonferroni" = "holm"
  )
  current_nonparametric_post_hoc <- as.character(nonparametric_post_hoc_method %||% "bonferroni")
  if (!current_nonparametric_post_hoc %in% unname(nonparametric_post_hoc_choices)) {
    current_nonparametric_post_hoc <- "bonferroni"
  }

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    dependent_allowed = dependent_allowed,
    factor_allowed = factor_allowed,
    dependent_variables = dependent_variables,
    dependent_items = analysis_variable_items(dependent_variables, variable_table, labels),
    dependent_selected = selected_order_items(selected_dependent, dependent_variables),
    factor_variables = factor_variables,
    factor_items = analysis_variable_items(factor_variables, variable_table, labels),
    factor_selected = selected_order_items(selected_factor, factor_variables),
    method_choices = method_choices,
    current_method = current_method,
    normality_enabled = isTRUE(normality_enabled),
    language = language,
    normality_study_choices = analysis_ui_choices(normality_study_choices, language),
    normality_study_type = current_normality_study,
    survey_normality_choices = analysis_ui_choices(survey_normality_choices, language),
    experimental_normality_choices = analysis_ui_choices(experimental_normality_choices, language),
    skew_kurtosis_cutoff_choices = analysis_ui_choices(skew_kurtosis_cutoff_choices, language),
    normality_method = current_normality,
    survey_normality_method = current_survey_normality,
    experimental_normality_method = current_experimental_normality,
    skew_kurtosis_cutoff = current_skew_kurtosis_cutoff,
    normality_skew_kurtosis = isTRUE(normality_skew_kurtosis),
    normality_ks = isTRUE(normality_ks),
    trend_analysis = isTRUE(trend_analysis),
    post_hoc_choices = post_hoc_choices,
    post_hoc_method = current_post_hoc,
    nonparametric_post_hoc_choices = nonparametric_post_hoc_choices,
    nonparametric_post_hoc_method = current_nonparametric_post_hoc,
    post_hoc = isTRUE(post_hoc),
    ordered_significance = isTRUE(ordered_significance),
    effect_size = isTRUE(effect_size),
    show_df = isTRUE(show_df),
    mean_sd = isTRUE(mean_sd),
    options_tab = if (as.character(options_tab %||% "Normality") %in% c("Normality", "Post-hoc", "Output")) {
      as.character(options_tab %||% "Normality")
    } else {
      "Normality"
    },
    move_disabled = length(selected) == 0
  )
}

ttest_anova_setup_panel <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  normality_on <- isTRUE(state$normality_enabled)
  survey_active <- normality_on && identical(state$normality_study_type, "survey")
  experimental_active <- normality_on && identical(state$normality_study_type, "experimental")

  normality_study_radio <- function(value, label) {
    tags$label(
      class = "radio ttest-normality-study-choice",
      tags$input(
        type = "radio",
        name = "ttest_anova_normality_study_type",
        value = value,
        checked = if (identical(state$normality_study_type, value)) "checked" else NULL
      ),
      tags$span(label)
    )
  }

  normality_method_radio_group <- function(id, choices, selected) {
    choice_values <- unname(choices)
    choice_labels <- names(choices)
    if (is.null(choice_labels)) {
      choice_labels <- choice_values
    }

    choice_items <- Map(function(choice_value, choice_label) {
      tags$label(
        class = "radio ttest-normality-method-choice",
        tags$input(
          type = "radio",
          name = id,
          value = choice_value,
          checked = if (identical(as.character(selected), as.character(choice_value))) "checked" else NULL
        ),
        tags$span(choice_label)
      )
    }, choice_values, choice_labels)

    div(
      id = id,
      class = "form-group shiny-input-radiogroup shiny-input-container ttest-normality-method-group",
      div(class = "shiny-options-group", choice_items)
    )
  }

  normality_survey_method_radio_group <- function() {
    choice_values <- unname(state$survey_normality_choices)
    choice_labels <- names(state$survey_normality_choices)
    if (is.null(choice_labels)) {
      choice_labels <- choice_values
    }

    choice_items <- list()
    for (choice_index in seq_along(choice_values)) {
      choice_value <- choice_values[[choice_index]]
      choice_label <- choice_labels[[choice_index]]
      choice_items <- c(
        choice_items,
        list(
          tags$label(
            class = "radio ttest-normality-method-choice",
            tags$input(
              type = "radio",
              name = "ttest_anova_survey_normality_method",
              value = choice_value,
              checked = if (identical(as.character(state$survey_normality_method), as.character(choice_value))) "checked" else NULL
            ),
            tags$span(choice_label)
          )
        )
      )
      if (identical(choice_value, "skew_kurtosis")) {
        choice_items <- c(
          choice_items,
          list(
            div(
              class = paste(
                "ttest-skew-kurtosis-cutoff-options",
                if (!identical(state$survey_normality_method, "skew_kurtosis")) "ttest-normality-disabled" else ""
              ),
              div(class = "ttest-skew-kurtosis-cutoff-title", analysis_ui_text("Skewness/kurtosis cutoff", language)),
              normality_method_radio_group(
                "ttest_anova_skew_kurtosis_cutoff",
                state$skew_kurtosis_cutoff_choices,
                state$skew_kurtosis_cutoff
              )
            )
          )
        )
      }
    }

    div(
      id = "ttest_anova_survey_normality_method",
      class = "form-group shiny-input-radiogroup shiny-input-container ttest-normality-method-group",
      div(class = "shiny-options-group", choice_items)
    )
  }

  div(
    class = "ttest-anova-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input("ttest_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls",
      actionButton(
        "ttest_dependent_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (isTRUE(state$move_disabled) && length(state$dependent_variables) == 0) "disabled" else NULL
      ),
      actionButton(
        "ttest_factor_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (isTRUE(state$move_disabled) && length(state$factor_variables) == 0) "disabled" else NULL
      )
    ),
    div(
      class = "ttest-anova-target-column",
      div(
        class = "analysis-transfer-column analysis-transfer-panel ttest-anova-dependent-panel",
        analysis_field_label_tag("Dependent variables", c("ordered", "continuous"), language = language),
        analysis_transfer_listbox_input("ttest_dependents", state$dependent_items, selected = state$dependent_selected, size = 3),
        div(
          class = "analysis-order-actions ttest-anova-order-actions",
          actionButton("ttest_dependent_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
          actionButton("ttest_dependent_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
        )
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel ttest-anova-factor-panel",
        analysis_field_label_tag("Independent variables", c("binary", "category", "ordered"), language = language),
        analysis_transfer_listbox_input("ttest_factors", state$factor_items, selected = state$factor_selected, size = 9),
        div(
          class = "analysis-order-actions ttest-anova-order-actions",
          actionButton("ttest_factor_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
          actionButton("ttest_factor_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
        )
      )
    ),
    div(
      class = "ttest-anova-options-column",
      analysis_options_tabs_panel(
        id = "ttest_anova_options_tab",
        selected = state$options_tab,
        class = "ttest-anova-options",
          tabPanel(
            analysis_ui_text("Normality", language),
            value = "Normality",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content",
              div(
                class = "analysis-option-group ttest-normality-options",
                div(class = "analysis-option-title", analysis_ui_text("Normality", language)),
                div(
                  class = "ttest-normality-level ttest-normality-level-2",
                  checkboxInput(
                    "ttest_anova_normality_enabled",
                    analysis_ui_text("Normality", language),
                    value = state$normality_enabled
                  )
                ),
                div(
                  id = "ttest_anova_normality_study_type",
                  class = paste(
                    "form-group shiny-input-radiogroup shiny-input-container analysis-option-subgroup ttest-normality-subgroup ttest-normality-study-options",
                    if (!normality_on) "ttest-normality-disabled" else ""
                  ),
                  div(
                    class = "shiny-options-group ttest-normality-study-options-group",
                    div(
                      class = "ttest-normality-study-block ttest-normality-survey-study",
                      div(
                        class = "ttest-normality-level ttest-normality-level-2",
                        normality_study_radio("survey", analysis_ui_text("Survey study", language))
                      ),
                      div(
                        class = paste(
                          "ttest-normality-branch ttest-normality-survey-branch",
                          if (!survey_active) "ttest-normality-disabled" else ""
                        ),
                        div(
                          class = "ttest-normality-level ttest-normality-level-3",
                          normality_survey_method_radio_group()
                        )
                      )
                    ),
                    div(
                      class = "ttest-normality-study-block ttest-normality-experimental-study",
                      div(
                        class = "ttest-normality-level ttest-normality-level-2",
                        normality_study_radio("experimental", analysis_ui_text("Experimental study", language))
                      ),
                      div(
                        class = paste(
                          "ttest-normality-branch ttest-normality-experimental-branch",
                          if (!experimental_active) "ttest-normality-disabled" else ""
                        ),
                        div(
                          class = "ttest-normality-level ttest-normality-level-3",
                          normality_method_radio_group(
                            "ttest_anova_experimental_normality_method",
                            state$experimental_normality_choices,
                            state$experimental_normality_method
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            analysis_ui_text("Post-hoc", language),
            value = "Post-hoc",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content",
              div(
                class = "analysis-option-group analysis-radio-group",
                div(class = "analysis-option-title", analysis_ui_text("Post-hoc", language)),
                div(
                  class = "ttest-anova-ordered-significance-option",
                  checkboxInput(
                    "ttest_anova_ordered_significance",
                    analysis_ui_text("Ordered significance notation", language),
                    value = state$ordered_significance
                  )
                ),
                div(class = "analysis-option-subtitle", "ANOVA"),
                radioButtons(
                  "ttest_anova_post_hoc_method",
                  label = NULL,
                  choices = state$post_hoc_choices,
                  selected = state$post_hoc_method
                ),
                div(class = "analysis-option-subtitle", analysis_ui_text("Nonparametric", language)),
                radioButtons(
                  "ttest_anova_nonparametric_post_hoc_method",
                  label = NULL,
                  choices = state$nonparametric_post_hoc_choices,
                  selected = state$nonparametric_post_hoc_method
                )
              )
            )
          ),
          tabPanel(
            analysis_ui_text("Output", language),
            value = "Output",
            div(
              class = "factor-options-tab-content ttest-anova-options-tab-content",
              analysis_option_group(
                "Trend analysis",
                list(
                  list(id = "ttest_anova_trend_analysis", label = "Trend analysis", value = state$trend_analysis)
                ),
                language = language
              ),
              analysis_option_group(
                "Effect size",
                list(
                  list(id = "ttest_anova_effect_size", label = "Effect size", value = state$effect_size)
                ),
                language = language
              ),
              analysis_option_group(
                "Statistic",
                list(
                  list(id = "ttest_anova_show_df", label = "Degrees of freedom", value = state$show_df),
                  list(id = "ttest_anova_mean_sd", label = "M \u00B1 SD", value = state$mean_sd)
                ),
                language = language
              )
            )
          )
      )
    )
  )
}
