# Survival analysis setup UI.

survival_km_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    survival_ui_text("Kaplan-Meier", language),
    value = "analysis_survival_km",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(survival_ui_text("Kaplan-Meier", language)),
        div("Time-to-event curve, log-rank test, median survival, and number at risk.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel survival-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading(survival_ui_text("Kaplan-Meier", language), "survival_km", language),
        analysis_workspace_body(
          "survival_km",
          uiOutput("survival_km_setup"),
          analysis_three_block_action_row(
            class = "survival-action-row",
            run_button = actionButton("run_survival_km", survival_ui_text("Run Kaplan-Meier", language), class = "btn btn-primary"),
            reset_control = uiOutput("survival_km_reset_control"),
            save_control = uiOutput("survival_km_save_control")
          ),
          uiOutput("survival_km_results")
        )
      )
    )
  )
}

survival_cox_tab_panel <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    survival_ui_text("Cox Regression", language),
    value = "analysis_survival_cox",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(survival_ui_text("Cox Regression", language)),
        div("Hazard ratios with 95% CI and proportional hazards assumption checks.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel survival-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading(survival_ui_text("Cox Regression", language), "survival_cox", language),
        analysis_workspace_body(
          "survival_cox",
          uiOutput("survival_cox_setup"),
          analysis_three_block_action_row(
            class = "survival-action-row",
            run_button = actionButton("run_survival_cox", survival_ui_text("Run Cox regression", language), class = "btn btn-primary"),
            reset_control = uiOutput("survival_cox_reset_control"),
            save_control = uiOutput("survival_cox_save_control")
          ),
          uiOutput("survival_cox_results")
        )
      )
    )
  )
}

survival_target_panel <- function(
  title,
  input_id,
  items,
  selected,
  size = 3,
  class = "",
  language = statedu_initial_language(),
  allowed_measurements = analysis_allowed_measurements_all(),
  extra_content = NULL
) {
  language <- normalize_app_language(language)
  div(
    class = paste("analysis-transfer-column analysis-transfer-panel survival-target-panel", class),
    analysis_field_label_tag(title, allowed_measurements, language = language),
    analysis_transfer_listbox_input(input_id, items = items, selected = selected, size = size, important_height = TRUE, min_size = size),
    extra_content
  )
}

survival_km_setup_panel <- function(
  selected_names,
  time = "",
  event = "",
  group = "",
  event_value = "1",
  rate_times = "",
  analysis_method = "km",
  test_method = "logrank",
  output_tables = c("survival_table", "survival_time"),
  plot_types = c("survival", "event", "cumhaz", "log_survival"),
  plot_versions = "color",
  show_ci = TRUE,
  show_censor = TRUE,
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_time = NULL,
  selected_event = NULL,
  selected_group = NULL,
  option_tab = "analysis",
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  option_tab <- as.character(option_tab %||% "analysis")[[1]]
  if (!option_tab %in% c("analysis", "tables", "plots")) option_tab <- "analysis"
  selected <- survival_selected_names(selected_names)
  assigned <- unique(c(time, event, group))
  assigned <- assigned[nzchar(assigned)]
  available <- setdiff(selected, assigned)
  method_choices <- stats::setNames(c("km", "life_table"), c(
    survival_ui_text("Kaplan-Meier", language),
    survival_ui_text("Life table", language)
  ))
  test_choices <- c("Log-rank" = "logrank", "Breslow" = "breslow", "Tarone-Ware" = "tarone_ware")
  output_choices <- stats::setNames(c("survival_table", "survival_time"), c(
    survival_ui_text("Survival table", language),
    survival_ui_text("Mean / median / quartiles", language)
  ))
  plot_choices <- stats::setNames(c("survival", "event", "cumhaz", "log_survival"), c(
    survival_ui_text("Survival function", language),
    survival_ui_text("1 - Survival function", language),
    survival_ui_text("Cumulative hazard", language),
    survival_ui_text("Log survival", language)
  ))
  plot_version_choices <- stats::setNames(c("color", "bw"), c(
    survival_ui_text("Color", language),
    survival_ui_text("Black and white", language)
  ))
  div(
    class = "ttest-anova-setup-grid survival-setup-grid analysis-three-block-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel survival-available-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input(
        "survival_km_available",
        analysis_variable_items(available, variable_table, labels),
        selected = selected_order_items(selected_available, available),
        size = 17
      )
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls survival-transfer-controls",
      actionButton("survival_km_time_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("survival_km_event_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("survival_km_group_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = "ttest-anova-target-column survival-target-column",
      survival_target_panel(
        survival_ui_text("Time variable", language),
        "survival_km_time",
        analysis_variable_items(time, variable_table, labels),
        selected_order_items(selected_time, time),
        size = 1,
        class = "survival-time-panel",
        language = language,
        allowed_measurements = c("ordered", "continuous"),
        extra_content = div(
          class = "survival-target-setting",
          textInput("survival_km_rate_times", survival_ui_text("Time-point rates", language), value = rate_times, placeholder = "12, 36, 60")
        )
      ),
      survival_target_panel(
        survival_ui_text("Event variable", language),
        "survival_km_event",
        analysis_variable_items(event, variable_table, labels),
        selected_order_items(selected_event, event),
        size = 1,
        class = "survival-event-panel",
        language = language,
        allowed_measurements = c("binary", "category", "ordered", "continuous"),
        extra_content = div(
          class = "survival-target-setting",
          textInput("survival_km_event_value", survival_ui_text("Event value", language), value = event_value, placeholder = survival_ui_text("Event value placeholder", language))
        )
      ),
      survival_target_panel(
        survival_ui_text("Group variable", language),
        "survival_km_group",
        analysis_variable_items(group, variable_table, labels),
        selected_order_items(selected_group, group),
        size = 8,
        class = "survival-group-panel",
        language = language,
        allowed_measurements = c("binary", "category", "ordered")
      )
    ),
    div(
      class = "ttest-anova-options-column analysis-options-panel survival-options-column",
      tabsetPanel(
        id = "survival_km_option_tabs",
        type = "tabs",
        selected = option_tab,
        tabPanel(
          survival_ui_text("Analysis", language),
          value = "analysis",
          div(
            class = "survival-options-tab-content",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Analysis method", language)),
              selectInput("survival_km_analysis_method", NULL, choices = method_choices, selected = analysis_method)
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Group comparison", language)),
              selectInput("survival_km_test_method", NULL, choices = test_choices, selected = test_method)
            )
          )
        ),
        tabPanel(
          survival_ui_text("Tables", language),
          value = "tables",
          div(
            class = "survival-options-tab-content",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", analysis_ui_text("Output", language)),
              checkboxGroupInput("survival_km_output_tables", NULL, choices = output_choices, selected = output_tables)
            )
          )
        ),
        tabPanel(
          survival_ui_text("Plots", language),
          value = "plots",
          div(
            class = "survival-options-tab-content",
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Plot type", language)),
              checkboxGroupInput("survival_km_plot_types", NULL, choices = plot_choices, selected = plot_types)
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Plot version", language)),
              checkboxGroupInput("survival_km_plot_versions", NULL, choices = plot_version_choices, selected = plot_versions)
            ),
            div(class = "analysis-option-group",
              div(class = "analysis-option-title", survival_ui_text("Display", language)),
              checkboxInput("survival_km_show_ci", survival_ui_text("Show confidence interval", language), value = isTRUE(show_ci)),
              checkboxInput("survival_km_show_censor", survival_ui_text("Show censor marks", language), value = isTRUE(show_censor))
            )
          )
        )
      )
    )
  )
}

survival_cox_setup_panel <- function(
  selected_names,
  time = "",
  event = "",
  covariates = character(0),
  event_value = "1",
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_time = NULL,
  selected_event = NULL,
  selected_covariates = NULL,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  selected <- survival_selected_names(selected_names)
  assigned <- unique(c(time, event, covariates))
  assigned <- assigned[nzchar(assigned)]
  available <- setdiff(selected, assigned)
  div(
    class = "ttest-anova-setup-grid survival-setup-grid analysis-three-block-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel survival-available-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input(
        "survival_cox_available",
        analysis_variable_items(available, variable_table, labels),
        selected = selected_order_items(selected_available, available),
        size = 17
      )
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls survival-transfer-controls",
      actionButton("survival_cox_time_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("survival_cox_event_move", ">", class = "btn btn-default analysis-move-button"),
      actionButton("survival_cox_covariates_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = "ttest-anova-target-column survival-target-column",
      survival_target_panel(
        survival_ui_text("Time variable", language),
        "survival_cox_time",
        analysis_variable_items(time, variable_table, labels),
        selected_order_items(selected_time, time),
        size = 1,
        class = "survival-time-panel",
        language = language,
        allowed_measurements = c("ordered", "continuous")
      ),
      survival_target_panel(
        survival_ui_text("Event variable", language),
        "survival_cox_event",
        analysis_variable_items(event, variable_table, labels),
        selected_order_items(selected_event, event),
        size = 1,
        class = "survival-event-panel",
        language = language,
        allowed_measurements = c("binary", "category", "ordered", "continuous"),
        extra_content = div(
          class = "survival-target-setting",
          textInput("survival_cox_event_value", survival_ui_text("Event value", language), value = event_value, placeholder = survival_ui_text("Event value placeholder", language))
        )
      ),
      survival_target_panel(
        survival_ui_text("Covariates", language),
        "survival_cox_covariates",
        analysis_variable_items(covariates, variable_table, labels),
        selected_order_items(selected_covariates, covariates),
        size = 8,
        class = "survival-covariates-panel",
        language = language,
        allowed_measurements = analysis_allowed_measurements_all()
      )
    ),
    div(
      class = "ttest-anova-options-column analysis-options-panel survival-options-column",
      div(class = "analysis-option-group",
        div(class = "analysis-option-title", survival_ui_text("PH assumption", language)),
        div(class = "analysis-option-subtitle", "Schoenfeld residual test")
      )
    )
  )
}
