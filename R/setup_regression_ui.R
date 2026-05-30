# Regression setup UI state and panel.

regression_setup_state <- function(
  ordered_dependents,
  ordered_predictors,
  available_predictors,
  variable_table,
  labels = character(0),
  selected_available = NULL,
  selected_dependent = NULL,
  selected_predictor = NULL,
  bootstrap_value = NULL,
  seed_value = NULL,
  show_sr2 = FALSE,
  show_f2 = TRUE,
  show_vif = TRUE
) {
  bootstrap_choices <- bootstrap_resample_choices()
  available <- setdiff(
    as.character(available_predictors %||% character(0)),
    unique(c(as.character(ordered_dependents %||% character(0)), as.character(ordered_predictors %||% character(0))))
  )
  available_selected <- selected_order_items(selected_available, available)
  dependent_selected <- selected_order_items(selected_dependent, ordered_dependents)
  predictor_selected <- selected_order_items(selected_predictor, ordered_predictors)

  list(
    available_predictors = available,
    available_choices = display_variable_choices_with_measurements(available, variable_table, labels),
    available_items = variable_choice_items(available, variable_table, labels),
    available_selected = available_selected,
    available_list_size = 17,
    add_dependent_disabled = length(available) == 0,
    add_predictor_disabled = length(available) == 0,
    remove_dependent_disabled = length(ordered_dependents) == 0,
    remove_disabled = length(ordered_predictors) == 0,
    dependent_choices = display_variable_choices_with_measurements(ordered_dependents, variable_table, labels),
    dependent_items = variable_choice_items(ordered_dependents, variable_table, labels),
    dependent_selected = dependent_selected,
    dependent_list_size = 3,
    predictor_choices = display_variable_choices_with_measurements(ordered_predictors, variable_table, labels),
    predictor_items = variable_choice_items(ordered_predictors, variable_table, labels),
    ordered_predictors = ordered_predictors,
    predictor_selected = predictor_selected,
    predictor_list_size = 9,
    bootstrap_choices = bootstrap_choices,
    current_bootstrap = normalized_bootstrap_resamples(bootstrap_value, bootstrap_choices),
    current_seed = seed_value %||% default_seed(),
    show_sr2 = setup_option_checked(show_sr2, default = FALSE),
    show_f2 = setup_option_checked(show_f2, default = TRUE),
    show_vif = setup_option_checked(show_vif, default = TRUE)
  )
}

regression_setup_panel_from_state <- function(setup, status_message) {
  regression_setup_panel(
    status_message = status_message,
    available_predictors = setup$available_predictors,
    available_choices = setup$available_choices,
    available_items = setup$available_items,
    available_selected = setup$available_selected,
    available_list_size = setup$available_list_size,
    add_dependent_disabled = setup$add_dependent_disabled,
    add_predictor_disabled = setup$add_predictor_disabled,
    remove_dependent_disabled = setup$remove_dependent_disabled,
    remove_disabled = setup$remove_disabled,
    dependent_choices = setup$dependent_choices,
    dependent_items = setup$dependent_items,
    dependent_selected = setup$dependent_selected,
    dependent_list_size = setup$dependent_list_size,
    predictor_choices = setup$predictor_choices,
    predictor_items = setup$predictor_items,
    ordered_predictors = setup$ordered_predictors,
    predictor_selected = setup$predictor_selected,
    predictor_list_size = setup$predictor_list_size,
    bootstrap_choices = setup$bootstrap_choices,
    current_bootstrap = setup$current_bootstrap,
    current_seed = setup$current_seed,
    show_sr2 = setup$show_sr2,
    show_f2 = setup$show_f2,
    show_vif = setup$show_vif
  )
}



regression_setup_panel <- function(
  status_message,
  available_predictors,
  available_choices,
  available_items,
  available_selected,
  available_list_size,
  add_dependent_disabled,
  add_predictor_disabled,
  remove_dependent_disabled,
  remove_disabled,
  dependent_choices,
  dependent_items,
  dependent_selected,
  dependent_list_size,
  predictor_choices,
  predictor_items,
  ordered_predictors,
  predictor_selected,
  predictor_list_size,
  bootstrap_choices,
  current_bootstrap,
  current_seed,
  show_sr2 = FALSE,
  show_f2 = TRUE,
  show_vif = TRUE
) {
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "regression-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-available-panel",
        analysis_field_label_tag("Variables"),
        analysis_transfer_listbox_input(
          "available_predictors",
          items = available_items,
          selected = available_selected,
          size = available_list_size
        )
      ),
      div(
        class = "analysis-transfer-controls regression-transfer-controls",
        actionButton(
          "add_dependent_from_variables",
          ">",
          class = "btn btn-default analysis-move-button",
          disabled = if (add_dependent_disabled && remove_dependent_disabled) "disabled" else NULL
        ),
        actionButton(
          "add_predictor_from_variables",
          ">",
          class = "btn btn-default analysis-move-button",
          disabled = if (add_predictor_disabled && remove_disabled) "disabled" else NULL
        )
      ),
      div(
        class = "regression-target-column",
        div(
          class = "analysis-transfer-column analysis-transfer-panel regression-dependent-panel",
          analysis_field_label_tag("Dependent Variables", "continuous"),
          analysis_transfer_listbox_input(
          "y",
          items = dependent_items,
          selected = dependent_selected,
          size = 3
          ),
          div(
            class = "dependent-order-actions",
            actionButton("move_dependent_up", "Up", class = "btn-default btn-sm"),
            actionButton("move_dependent_down", "Down", class = "btn-default btn-sm")
          )
        ),
        div(
          class = "analysis-transfer-column analysis-transfer-panel regression-independent-panel",
          analysis_field_label_tag("Independent Variables", c("binary", "category", "ordered", "continuous")),
          analysis_transfer_listbox_input(
            "predictor_order",
            items = predictor_items,
            selected = predictor_selected,
            size = 9
          ),
          div(
            class = "predictor-order-actions",
            actionButton("move_predictor_up", "Up", class = "btn-default btn-sm"),
            actionButton("move_predictor_down", "Down", class = "btn-default btn-sm")
          )
        )
      ),
      div(
        class = "analysis-options-column analysis-options-panel regression-options",
        div(
          class = "analysis-option-group",
          div(class = "analysis-option-title", "Bootstrap"),
          div(
            class = "regression-field",
            selectInput(
              "boot_r",
              "Number of bootstrap samples",
              choices = bootstrap_choices,
              selected = current_bootstrap,
              selectize = FALSE
            )
          ),
          div(
            class = "regression-field",
            numericInput("seed", "Seed number", value = current_seed, min = 1, step = 1)
          )
        ),
        analysis_option_group(
          "Effect size",
          list(
            list(id = "show_sr2", label = "sr\u00B2", value = isTRUE(show_sr2)),
            list(id = "show_f2", label = "f\u00B2", value = isTRUE(show_f2))
          )
        ),
        analysis_option_group(
          "Collinearity diagnostics",
          list(
            list(id = "show_vif", label = "VIF", value = isTRUE(show_vif))
          )
        )
      )
    ),
    div(
      class = "analysis-action-row regression-action-row",
      if (!is.null(status_message)) {
        tags$button("Run regression", type = "button", class = "btn btn-primary", disabled = "disabled")
      } else {
        actionButton("run", "Run regression", class = "btn-primary")
      },
      uiOutput("regression_reset_control"),
      uiOutput("penalized_regression_control"),
      uiOutput("regression_save_control")
    )
  )
}

