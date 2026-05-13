# Regression setup UI state and panel.

regression_setup_state <- function(
  ordered_dependents,
  ordered_predictors,
  available_predictors,
  variable_table,
  labels = character(0),
  selected_dependent = NULL,
  bootstrap_value = NULL,
  seed_value = NULL
) {
  bootstrap_choices <- bootstrap_resample_choices()
  dependent_selected <- selected_order_item(selected_dependent, ordered_dependents)

  list(
    available_predictors = available_predictors,
    available_choices = display_variable_choices_static(available_predictors, variable_table, labels),
    available_list_size = 18,
    add_disabled = length(setdiff(available_predictors, ordered_predictors)) == 0,
    remove_disabled = length(ordered_predictors) == 0,
    dependent_choices = display_variable_choices_static(ordered_dependents, variable_table, labels),
    dependent_selected = dependent_selected,
    dependent_list_size = setup_list_size(ordered_dependents),
    predictor_choices = display_variable_choices_static(ordered_predictors, variable_table, labels),
    ordered_predictors = ordered_predictors,
    predictor_list_size = setup_list_size(ordered_predictors),
    bootstrap_choices = bootstrap_choices,
    current_bootstrap = normalized_bootstrap_resamples(bootstrap_value, bootstrap_choices),
    current_seed = seed_value %||% default_seed()
  )
}

regression_setup_panel_from_state <- function(setup, status_message) {
  regression_setup_panel(
    status_message = status_message,
    available_predictors = setup$available_predictors,
    available_choices = setup$available_choices,
    available_list_size = setup$available_list_size,
    add_disabled = setup$add_disabled,
    remove_disabled = setup$remove_disabled,
    dependent_choices = setup$dependent_choices,
    dependent_selected = setup$dependent_selected,
    dependent_list_size = setup$dependent_list_size,
    predictor_choices = setup$predictor_choices,
    ordered_predictors = setup$ordered_predictors,
    predictor_list_size = setup$predictor_list_size,
    bootstrap_choices = setup$bootstrap_choices,
    current_bootstrap = setup$current_bootstrap,
    current_seed = setup$current_seed
  )
}



regression_setup_panel <- function(
  status_message,
  available_predictors,
  available_choices,
  available_list_size,
  add_disabled,
  remove_disabled,
  dependent_choices,
  dependent_selected,
  dependent_list_size,
  predictor_choices,
  ordered_predictors,
  predictor_list_size,
  bootstrap_choices,
  current_bootstrap,
  current_seed
) {
  setup_variable_list <- div(
    class = "regression-setup-variable-box",
    div("Variables", class = "regression-setup-variable-title"),
    if (length(available_predictors) == 0) {
      div("No predictor variables selected.", class = "regression-variable-empty")
    } else {
      selectInput(
        "available_predictors",
        label = NULL,
        choices = available_choices,
        selected = utils::head(available_predictors, 1),
        multiple = FALSE,
        selectize = FALSE,
        size = available_list_size
      )
    }
  )

  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "regression-fields",
      div(
        class = "regression-variables-panel",
        setup_variable_list
      ),
      div(
        class = "variable-transfer-actions",
        actionButton(
          "add_predictor_from_variables",
          ">",
          class = "btn btn-default btn-sm variable-transfer-button",
          disabled = if (add_disabled) "disabled" else NULL
        ),
        actionButton(
          "remove_predictor_to_variables",
          "<",
          class = "btn btn-default btn-sm variable-transfer-button",
          disabled = if (remove_disabled) "disabled" else NULL
        )
      ),
      div(
        class = "regression-field",
        tags$label("Dependent Variables", `for` = "y", class = "control-label"),
        selectInput(
          "y",
          label = NULL,
          choices = dependent_choices,
          selected = dependent_selected,
          multiple = FALSE,
          selectize = FALSE,
          size = dependent_list_size
        ),
        div(
          class = "dependent-order-actions",
          actionButton("move_dependent_up", "Up", class = "btn-default btn-sm"),
          actionButton("move_dependent_down", "Down", class = "btn-default btn-sm")
        )
      ),
      div(
        class = "regression-field",
        tags$label("Independent Variables", class = "control-label"),
        selectInput(
          "predictor_order",
          label = NULL,
          choices = predictor_choices,
          selected = utils::head(ordered_predictors, 1),
          multiple = FALSE,
          selectize = FALSE,
          size = predictor_list_size
        ),
        div(
          class = "predictor-order-actions",
          actionButton("move_predictor_up", "Up", class = "btn-default btn-sm"),
          actionButton("move_predictor_down", "Down", class = "btn-default btn-sm")
        )
      ),
      div(
        class = "regression-options",
        div(
          class = "regression-field",
          selectInput(
            "boot_r",
            "Bootstrap resamples",
            choices = bootstrap_choices,
            selected = current_bootstrap,
            selectize = FALSE
          )
        ),
        div(
          class = "regression-field",
          numericInput("seed", "Seed number", value = current_seed, min = 1, step = 1)
        ),
        div(
          class = "regression-effect-options",
          checkboxInput("show_sr2", "effect size sr\u00B2", value = FALSE),
          checkboxInput("show_f2", "effect size f\u00B2", value = FALSE),
          checkboxInput("show_vif", "Collinearity diagnostics(VIF)", value = FALSE)
        )
      )
    ),
    div(
      class = "regression-actions",
      if (!is.null(status_message)) {
        tags$button("Run regression", type = "button", class = "btn btn-primary", disabled = "disabled")
      } else {
        actionButton("run", "Run regression", class = "btn-primary")
      },
      uiOutput("penalized_regression_control"),
      uiOutput("regression_save_control")
    )
  )
}

