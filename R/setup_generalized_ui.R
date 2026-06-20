# Generalized linear model setup UI state and panel.

generalized_setup_state <- function(
  selected_names,
  outcome,
  exposure = character(0),
  predictors,
  variable_table,
  labels = character(0),
  selected_available = NULL,
  selected_outcome = NULL,
  selected_exposure = NULL,
  selected_predictor = NULL,
  family = "auto",
  link = "default",
  robust = TRUE,
  se_type = NULL,
  overdispersion = TRUE,
  assumption_checks = TRUE,
  exponentiate = TRUE,
  show_vif = FALSE,
  missing_strategy = "complete",
  missing_imputations = 5L,
  missing_iterations = 5L,
  mi_outcome = "observed",
  ipw_auxiliary = character(0),
  options_tab = "Model"
) {
  selected_names <- as.character(selected_names %||% character(0))
  outcome <- utils::head(intersect(as.character(outcome %||% character(0)), selected_names), 1)
  if (length(outcome) == 1 && !isTRUE(generalized_outcome_allowed(outcome, variable_table))) {
    outcome <- character(0)
  }
  exposure <- utils::head(intersect(as.character(exposure %||% character(0)), selected_names), 1)
  if (length(exposure) == 1 && !isTRUE(generalized_offset_allowed(exposure, variable_table))) {
    exposure <- character(0)
  }
  predictors <- setdiff(intersect(as.character(predictors %||% character(0)), selected_names), unique(c(outcome, exposure)))
  available <- setdiff(selected_names, unique(c(outcome, exposure, predictors)))
  ipw_auxiliary_choices <- available
  ipw_auxiliary <- intersect(as.character(ipw_auxiliary %||% character(0)), ipw_auxiliary_choices)
  family <- generalized_resolve_family(family)
  link <- generalized_resolve_link(family, link)
  se_type <- generalized_resolve_se_type(se_type, robust = robust)

  list(
    available = available,
    available_items = variable_choice_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    outcome = outcome,
    outcome_items = variable_choice_items(outcome, variable_table, labels),
    outcome_selected = selected_order_items(selected_outcome, outcome),
    exposure = exposure,
    exposure_items = variable_choice_items(exposure, variable_table, labels),
    exposure_selected = selected_order_items(selected_exposure, exposure),
    predictors = predictors,
    predictor_items = variable_choice_items(predictors, variable_table, labels),
    predictor_selected = selected_order_items(selected_predictor, predictors),
    family = family,
    link = link,
    robust = !identical(se_type, "model"),
    se_type = se_type,
    overdispersion = setup_option_checked(overdispersion, default = TRUE),
    assumption_checks = setup_option_checked(assumption_checks, default = TRUE),
    exponentiate = setup_option_checked(exponentiate, default = TRUE),
    report_b_se = !setup_option_checked(exponentiate, default = TRUE),
    show_vif = setup_option_checked(show_vif, default = FALSE),
    missing_strategy = generalized_resolve_missing_strategy(missing_strategy),
    missing_strategy_detail = generalized_missing_strategy_detail(missing_strategy),
    missing_imputations = generalized_resolve_mi_count(missing_imputations, default = 5L, minimum = 2L),
    missing_iterations = generalized_resolve_mi_count(missing_iterations, default = 5L, minimum = 1L),
    mi_outcome = generalized_resolve_mi_outcome(mi_outcome),
    ipw_auxiliary = ipw_auxiliary,
    ipw_auxiliary_choices = display_variable_choices_with_measurements(ipw_auxiliary_choices, variable_table, labels),
    options_tab = as.character(options_tab %||% "Model")[[1]],
    run_disabled = length(outcome) != 1 || length(predictors) == 0
  )
}

generalized_setup_panel <- function(state, status_message = NULL) {
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "regression-setup-grid generalized-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-available-panel",
        analysis_field_label_tag("Variables"),
        analysis_transfer_listbox_input(
          "generalized_available",
          items = state$available_items,
          selected = state$available_selected,
          size = 17
        )
      ),
      div(
        class = "analysis-transfer-controls regression-transfer-controls generalized-transfer-controls",
        actionButton("generalized_outcome_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("generalized_exposure_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("generalized_predictors_move", ">", class = "btn btn-default analysis-move-button")
      ),
      div(
        class = "regression-target-column",
        div(
          class = "analysis-transfer-column analysis-transfer-panel generalized-model-block",
          div(class = "analysis-option-title longitudinal-block-title", "Model variables"),
          div(
            class = "generalized-model-fields",
            div(
              class = "generalized-target-field generalized-outcome-field",
              analysis_field_label_tag("Dependent variable", c("continuous", "binary")),
              analysis_transfer_listbox_input(
                "generalized_outcome",
                items = state$outcome_items,
                selected = state$outcome_selected,
                size = 1
              )
            ),
            div(
              class = "generalized-target-field generalized-exposure-field",
              analysis_field_label_tag("Exposure / offset (optional)", "continuous"),
              analysis_transfer_listbox_input(
                "generalized_exposure",
                items = state$exposure_items,
                selected = state$exposure_selected,
                size = 1
              )
            )
          ),
          div(
            class = "generalized-target-field generalized-predictors-field",
            analysis_field_label_tag("Independent variables", c("binary", "category", "ordered", "continuous")),
            analysis_transfer_listbox_input(
              "generalized_predictors",
              items = state$predictor_items,
              selected = state$predictor_selected,
              size = 8
            ),
            div(
              class = "predictor-order-actions",
              actionButton("generalized_predictor_up", "Up", class = "btn-default btn-sm"),
              actionButton("generalized_predictor_down", "Down", class = "btn-default btn-sm")
            )
          )
        )
      ),
      div(
        class = "analysis-options-column analysis-options-panel regression-options generalized-options",
        div("Options", class = "analysis-option-title"),
        tabsetPanel(
          id = "generalized_options_tab",
          selected = state$options_tab,
          tabPanel(
            "Model",
            div(
              class = "generalized-options-tab-content",
              selectInput(
                "generalized_family",
                "Outcome family",
                choices = generalized_family_choices(),
                selected = state$family,
                selectize = FALSE
              ),
              selectInput(
                "generalized_link",
                "Link function",
                choices = generalized_link_choices(state$family),
                selected = state$link,
                selectize = FALSE
              ),
              div("Inference", class = "analysis-option-title generalized-section-title"),
              selectInput(
                "generalized_se_type",
                "Standard errors",
                choices = generalized_se_type_choices(),
                selected = state$se_type,
                selectize = FALSE
              ),
              checkboxInput("generalized_report_b_se", "Report B and SE instead of OR / ratio and 95% CI", value = state$report_b_se)
            )
          ),
          tabPanel(
            "Missing",
            div(
              class = "generalized-options-tab-content generalized-missing-options-tab-content",
              selectInput(
                "generalized_missing_strategy",
                "Missing-data strategy",
                choices = generalized_missing_strategy_choices(),
                selected = state$missing_strategy,
                selectize = FALSE
              ),
              div(state$missing_strategy_detail, class = "generalized-help-text generalized-missing-detail"),
              if (identical(state$missing_strategy, "mi")) {
                div(
                  class = "generalized-mi-settings",
                  div(class = "analysis-option-subtitle", "Multiple imputation settings"),
                  selectInput(
                    "generalized_mi_outcome",
                    "Dependent-variable handling",
                    choices = generalized_mi_outcome_choices(),
                    selected = state$mi_outcome,
                    selectize = FALSE
                  ),
                  numericInput("generalized_missing_imputations", "MI datasets", value = state$missing_imputations, min = 2, max = 50, step = 1),
                  numericInput("generalized_missing_iterations", "MI iterations", value = state$missing_iterations, min = 1, max = 50, step = 1)
                )
              },
              if (identical(state$missing_strategy, "ipw")) {
                div(
                  class = "generalized-ipw-settings",
                  div(class = "analysis-option-subtitle", "IPW observation model"),
                  selectizeInput(
                    "generalized_ipw_auxiliary",
                    "Auxiliary variables",
                    choices = state$ipw_auxiliary_choices,
                    selected = state$ipw_auxiliary,
                    multiple = TRUE,
                    options = list(plugins = list("remove_button"))
                  ),
                  div(
                    "Auxiliary variables are used only in the complete-case observation model when fully observed; they do not become GLM predictors.",
                    class = "generalized-help-text"
                  )
                )
              }
            )
          ),
          tabPanel(
            "Checks",
            div(
              class = "generalized-options-tab-content",
              checkboxInput("generalized_assumption_checks", "Run assumption checks and recommendations", value = state$assumption_checks),
              tags$fieldset(
                disabled = if (isTRUE(state$assumption_checks)) NULL else "disabled",
                class = if (isTRUE(state$assumption_checks)) "generalized-check-options" else "generalized-check-options generalized-check-options-disabled",
                checkboxInput("generalized_overdispersion", "Poisson / negative-binomial screening", value = state$overdispersion),
                checkboxInput("generalized_show_vif", "Collinearity diagnostics (VIF)", value = state$show_vif)
              )
            )
          )
        )
      )
    ),
    div(
      class = "regression-actions generalized-action-row",
      actionButton(
        "run_generalized",
        "Run GLM",
        class = "btn btn-primary",
        disabled = if (isTRUE(state$run_disabled)) "disabled" else NULL
      ),
      uiOutput("generalized_reset_control"),
      uiOutput("generalized_save_control")
    )
  )
}
