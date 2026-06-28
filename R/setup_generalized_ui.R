# Generalized linear model setup UI state and panel.

generalized_missing_strategy_ui_detail <- function(strategy, language = statedu_initial_language()) {
  switch(
    generalized_resolve_missing_strategy(strategy),
    mi = statedu_text(
      language,
      "Uses standard mice-based multiple imputation for incomplete selected GLM variables, including the dependent variable when it is missing. The selected GLM is fitted in each imputed dataset and pooled using Rubin-style total variance. Treat this as a prespecified primary or sensitivity option when missingness is plausibly MAR.",
      statedu_utf8("ebb688ec9984eca084ed959c20474c4d20ebb380ec8898ec979020eb8c80ed95b4206d69636520eab8b0ebb09820eb8ba4eca491eb8c80ecb2b4eba5bc20ec8898ed9689ed9598eab3a020527562696e20ebb0a9ec8b9dec9cbceba19c20ecb694eca095ecb998eba5bc20ed86b5ed95a9ed95a9eb8b88eb8ba42e20eab2b0ecb8a1ec9db4204d4152eba19c20ebb3bc20ec889820ec9e88ec9d8420eb958c20ec82aceca08420eca780eca095ed959c20eca3bcebb684ec849d20eb9890eb8a9420ebafbceab090eb8f8420ebb684ec849dec9cbceba19c20ec82acec9aa9ed95a9eb8b88eb8ba42e")
    ),
    ipw = statedu_text(
      language,
      "Estimates the probability of being a complete case from fully observed predictors and fits the selected GLM with inverse-probability weights. Use this as a sensitivity option when complete-case inclusion may depend on observed covariates, and review positivity/weight stability and the observation model.",
      statedu_utf8("ec9984eca084ec82aceba180eab08020eb90a020ed9995eba5a0ec9d8420eab480ecb8a1eb909c20eab3b5ebb380eb9f89ec9cbceba19c20ecb694eca095ed959c20eb92a420ec97aded9995eba5a0eab080eca49120474c4dec9d8420eca081ed95a9ed95a9eb8b88eb8ba42e20ec9984eca084ec82aceba18020ed8faced95a820ec97acebb680eab08020eab480ecb8a120eab3b5ebb380eb9f89ec979020ec9d98eca1b4ed95a020ec889820ec9e88ec9d8420eb958c20ebafbceab090eb8f8420ebb684ec849dec9cbceba19c20ec82acec9aa9ed95a9eb8b88eb8ba42e")
    ),
    statedu_text(
      language,
      "Drops rows with missing selected model variables before fitting the GLM. This is transparent, but it is strongest when missingness is minimal or plausibly MCAR.",
      statedu_utf8("ec84a0ed839ded959c20474c4d20ebaaa8ed989520ebb380ec8898ec979020eab2b0ecb8a1ec9db420ec9e88eb8a9420ed9689ec9d8420ebb684ec849d20eca084ec979020eca09cec99b8ed95a9eb8b88eb8ba42e20eab2b0ecb8a1ec9db420eca081eab1b0eb8298204d434152eba19c20ebb3bc20ec889820ec9e88ec9d8420eb958c20eab080ec9ea520ed88acebaa85ed959c20ebb0a9ebb295ec9e85eb8b88eb8ba42e")
    )
  )
}

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
  options_tab = "Model",
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  options(statedu.app_language = language)
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
    missing_strategy_detail = generalized_missing_strategy_ui_detail(missing_strategy, language),
    missing_imputations = generalized_resolve_mi_count(missing_imputations, default = 5L, minimum = 2L),
    missing_iterations = generalized_resolve_mi_count(missing_iterations, default = 5L, minimum = 1L),
    mi_outcome = generalized_resolve_mi_outcome(mi_outcome),
    ipw_auxiliary = ipw_auxiliary,
    ipw_auxiliary_choices = display_variable_choices_with_measurements(ipw_auxiliary_choices, variable_table, labels),
    options_tab = as.character(options_tab %||% "Model")[[1]],
    language = language,
    run_disabled = length(outcome) != 1 || length(predictors) == 0
  )
}

generalized_setup_panel <- function(state, status_message = NULL) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  options(statedu.app_language = language)
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "regression-setup-grid generalized-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-available-panel",
        analysis_field_label_tag("Variables", language = language),
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
          div(class = "analysis-option-title longitudinal-block-title", analysis_ui_text("Model variables", language)),
          div(
            class = "generalized-model-fields",
            div(
              class = "generalized-target-field generalized-outcome-field",
              analysis_field_label_tag("Dependent variable", c("continuous", "binary"), language = language),
              analysis_transfer_listbox_input(
                "generalized_outcome",
                items = state$outcome_items,
                selected = state$outcome_selected,
                size = 1
              )
            ),
            div(
              class = "generalized-target-field generalized-exposure-field",
              analysis_field_label_tag("Exposure / offset (optional)", "continuous", language = language),
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
            analysis_field_label_tag("Independent variables", c("binary", "category", "ordered", "continuous"), language = language),
            analysis_transfer_listbox_input(
              "generalized_predictors",
              items = state$predictor_items,
              selected = state$predictor_selected,
              size = 8
            ),
            div(
              class = "predictor-order-actions",
              actionButton("generalized_predictor_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
              actionButton("generalized_predictor_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
            )
          )
        )
      ),
      analysis_options_tabs_panel(
        id = "generalized_options_tab",
        selected = state$options_tab,
        class = "analysis-options-column regression-options generalized-options",
        tabPanel(
            analysis_ui_text("Model", language),
            value = "Model",
            div(
              class = "generalized-options-tab-content",
              selectInput(
                "generalized_family",
                analysis_ui_text("Outcome family", language),
                choices = analysis_ui_choices(generalized_family_choices(), language),
                selected = state$family,
                selectize = FALSE
              ),
              selectInput(
                "generalized_link",
                analysis_ui_text("Link function", language),
                choices = analysis_ui_choices(generalized_link_choices(state$family), language),
                selected = state$link,
                selectize = FALSE
              ),
              div(analysis_ui_text("Inference", language), class = "analysis-option-title generalized-section-title"),
              selectInput(
                "generalized_se_type",
                analysis_ui_text("Standard errors", language),
                choices = analysis_ui_choices(generalized_se_type_choices(), language),
                selected = state$se_type,
                selectize = FALSE
              ),
              checkboxInput("generalized_report_b_se", analysis_ui_text("Report B and SE instead of OR / ratio and 95% CI", language), value = state$report_b_se)
            )
          ),
          tabPanel(
            analysis_ui_text("Missing", language),
            value = "Missing",
            div(
              class = "generalized-options-tab-content generalized-missing-options-tab-content",
              selectInput(
                "generalized_missing_strategy",
                analysis_ui_text("Missing-data strategy", language),
                choices = analysis_ui_choices(generalized_missing_strategy_choices(), language),
                selected = state$missing_strategy,
                selectize = FALSE
              ),
              div(state$missing_strategy_detail, class = "generalized-help-text generalized-missing-detail"),
              if (identical(state$missing_strategy, "mi")) {
                div(
                  class = "generalized-mi-settings",
                  div(class = "analysis-option-subtitle", analysis_ui_text("Multiple imputation settings", language)),
                  selectInput(
                    "generalized_mi_outcome",
                    analysis_ui_text("Dependent-variable handling", language),
                    choices = analysis_ui_choices(generalized_mi_outcome_choices(), language),
                    selected = state$mi_outcome,
                    selectize = FALSE
                  ),
                  numericInput("generalized_missing_imputations", analysis_ui_text("MI datasets", language), value = state$missing_imputations, min = 2, max = 50, step = 1),
                  numericInput("generalized_missing_iterations", analysis_ui_text("MI iterations", language), value = state$missing_iterations, min = 1, max = 50, step = 1)
                )
              },
              if (identical(state$missing_strategy, "ipw")) {
                div(
                  class = "generalized-ipw-settings",
                  div(class = "analysis-option-subtitle", analysis_ui_text("IPW observation model", language)),
                  selectizeInput(
                    "generalized_ipw_auxiliary",
                    analysis_ui_text("Auxiliary variables", language),
                    choices = state$ipw_auxiliary_choices,
                    selected = state$ipw_auxiliary,
                    multiple = TRUE,
                    options = list(plugins = list("remove_button"))
                  ),
                  div(
                    statedu_text(
                      language,
                      "Auxiliary variables are used only in the complete-case observation model when fully observed; they do not become GLM predictors.",
                      statedu_utf8("ebb3b4eca1b0ebb380ec8898eb8a9420ec9984eca084ed9e8820eab480ecb8a1eb909c20eab2bdec9ab020ec9984eca084ec82aceba18020eab480ecb8a1ebaaa8ed9895ec9790eba78c20ec82acec9aa9eb9098eba9b020474c4d20ec9888ecb8a1ebb380ec8898eba19c20ed8faced95a8eb9098eca78020ec958aec8ab5eb8b88eb8ba42e")
                    ),
                    class = "generalized-help-text"
                  )
                )
              }
            )
          ),
          tabPanel(
            analysis_ui_text("Checks", language),
            value = "Checks",
            div(
              class = "generalized-options-tab-content",
              checkboxInput("generalized_assumption_checks", analysis_ui_text("Run assumption checks and recommendations", language), value = state$assumption_checks),
              tags$fieldset(
                disabled = if (isTRUE(state$assumption_checks)) NULL else "disabled",
                class = if (isTRUE(state$assumption_checks)) "generalized-check-options" else "generalized-check-options generalized-check-options-disabled",
                checkboxInput("generalized_overdispersion", analysis_ui_text("Poisson / negative-binomial screening", language), value = state$overdispersion),
                checkboxInput("generalized_show_vif", paste0(analysis_ui_text("Collinearity diagnostics", language), " (VIF)"), value = state$show_vif)
              )
            )
          )
      )
    ),
    div(
      class = "regression-actions generalized-action-row",
      actionButton(
        "run_generalized",
        analysis_ui_text("Run GLM", language),
        class = "btn btn-primary",
        disabled = if (isTRUE(state$run_disabled)) "disabled" else NULL
      ),
      uiOutput("generalized_reset_control"),
      uiOutput("generalized_save_control")
    )
  )
}
