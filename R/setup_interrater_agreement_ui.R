# Inter-rater agreement setup UI.

interrater_agreement_setup_state <- function(
  selected_names,
  variable_table = NULL,
  labels = character(0),
  rater_variables = character(0),
  selected_available = character(0),
  selected_selected = character(0),
  weight = "quadratic",
  normality = TRUE,
  bootstrap_ci = TRUE,
  bootstrap_resamples = 1000,
  icc_model = "icc2",
  icc_type = "agreement",
  icc_unit = "single",
  seed = 20260720,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  selected <- as.character(selected_names %||% character(0))
  rater_variables <- intersect(as.character(rater_variables %||% character(0)), selected)
  available <- setdiff(selected, rater_variables)
  list(
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    selected_items = analysis_variable_items(rater_variables, variable_table, labels),
    selected_selected = selected_order_items(selected_selected, rater_variables),
    rater_variables = rater_variables,
    weight = if (weight %in% c("linear", "quadratic")) weight else "quadratic",
    normality = isTRUE(normality),
    bootstrap_ci = isTRUE(bootstrap_ci),
    bootstrap_resamples = as.integer(bootstrap_resamples %||% 1000L),
    icc_model = if (icc_model %in% c("icc1", "icc2", "icc3")) icc_model else "icc2",
    icc_type = if (icc_type %in% c("agreement", "consistency")) icc_type else "agreement",
    icc_unit = if (icc_unit %in% c("single", "average")) icc_unit else "single",
    seed = as.integer(seed %||% 20260720L),
    language = language
  )
}

interrater_agreement_setup_panel <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  div(
    class = "frequencies-setup-grid reliability-setup-grid interrater-agreement-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input("interrater_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls",
      actionButton("interrater_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel reliability-target-panel",
      analysis_field_label_tag("Rater variables", c("category", "ordered", "continuous"), language = language),
      analysis_transfer_listbox_input(
        "interrater_selected",
        state$selected_items,
        selected = state$selected_selected,
        size = 16,
        important_height = TRUE
      ),
      div(
        class = "analysis-order-actions reliability-order-actions",
        actionButton("interrater_move_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
        actionButton("interrater_move_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel interrater-options-panel",
      analysis_option_group(
        "Categorical / ordinal",
        list(
          list(
            id = "interrater_weight_quadratic",
            label = "Quadratic weights",
            value = identical(state$weight, "quadratic"),
            tooltip = "Use quadratic weights for weighted kappa and Gwet's AC2. Turn off to use linear weights."
          )
        ),
        language = language
      ),
      analysis_option_group(
        "ICC",
        list(
          list(
            id = "interrater_normality",
            label = "Normality",
            value = state$normality,
            tooltip = "Report skewness and kurtosis diagnostics for continuous rater scores."
          ),
          list(
            id = "interrater_bootstrap_ci",
            label = "Bootstrap 95% CI",
            value = state$bootstrap_ci,
            tooltip = "Use a percentile bootstrap CI for ICC when normality or outlier influence is a concern."
          )
        ),
        language = language
      ),
      selectInput(
        "interrater_icc_model",
        analysis_ui_text("ICC model", language),
        choices = stats::setNames(c("icc1", "icc2", "icc3"), c("ICC(1): one-way random", "ICC(2): two-way random", "ICC(3): two-way mixed")),
        selected = state$icc_model,
        selectize = FALSE
      ),
      selectInput(
        "interrater_icc_type",
        analysis_ui_text("ICC type", language),
        choices = stats::setNames(c("agreement", "consistency"), c("Absolute agreement", "Consistency")),
        selected = state$icc_type,
        selectize = FALSE
      ),
      selectInput(
        "interrater_icc_unit",
        analysis_ui_text("ICC unit", language),
        choices = stats::setNames(c("single", "average"), c("Single measure", "Average measure")),
        selected = state$icc_unit,
        selectize = FALSE
      ),
      numericInput(
        "interrater_bootstrap_resamples",
        analysis_ui_text("Bootstrap samples", language),
        value = state$bootstrap_resamples,
        min = 100,
        max = 50000,
        step = 100,
        width = "160px"
      ),
      numericInput(
        "interrater_seed",
        analysis_ui_text("Seed number", language),
        value = state$seed,
        min = 1,
        step = 1,
        width = "160px"
      )
    )
  )
}

interrater_agreement_tab_panel <- function(title = "Inter-rater Agreement", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "analysis_interrater_agreement",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Inter-rater Agreement", language)),
        div(statedu_t("analysis.subtitle_interrater_agreement", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel reliability-workspace-panel interrater-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Inter-rater Agreement", "interrater", language),
        analysis_workspace_body(
          "interrater",
          uiOutput("interrater_setup"),
          analysis_three_block_action_row(
            class = "reliability-action-row interrater-action-row",
            run_button = actionButton("run_interrater", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("interrater_reset_control"),
            save_control = uiOutput("interrater_save_control")
          ),
          uiOutput("interrater_results")
        )
      )
    )
  )
}
