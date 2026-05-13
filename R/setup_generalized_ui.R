# Generalized regression setup UI state and panel.

generalized_setup_state <- function(
  ordered_dependents,
  ordered_predictors,
  available_predictors,
  variable_table,
  labels = character(0)
) {
  list(
    available_predictors = available_predictors,
    available_choices = display_variable_choices_static(available_predictors, variable_table, labels),
    available_list_size = 18,
    dependent_choices = display_variable_choices_static(ordered_dependents, variable_table, labels),
    ordered_dependents = ordered_dependents,
    dependent_list_size = setup_list_size(ordered_dependents),
    predictor_choices = display_variable_choices_static(ordered_predictors, variable_table, labels),
    ordered_predictors = ordered_predictors,
    predictor_list_size = setup_list_size(ordered_predictors),
    family_choices = generalized_family_choices(),
    link_choices = generalized_link_choices()
  )
}

generalized_setup_panel_from_state <- function(setup, status_message) {
  generalized_setup_panel(
    status_message = status_message,
    available_predictors = setup$available_predictors,
    available_choices = setup$available_choices,
    available_list_size = setup$available_list_size,
    dependent_choices = setup$dependent_choices,
    ordered_dependents = setup$ordered_dependents,
    dependent_list_size = setup$dependent_list_size,
    predictor_choices = setup$predictor_choices,
    ordered_predictors = setup$ordered_predictors,
    predictor_list_size = setup$predictor_list_size,
    family_choices = setup$family_choices,
    link_choices = setup$link_choices
  )
}



generalized_setup_panel <- function(
  status_message,
  available_predictors,
  available_choices,
  available_list_size,
  dependent_choices,
  ordered_dependents,
  dependent_list_size,
  predictor_choices,
  ordered_predictors,
  predictor_list_size,
  family_choices,
  link_choices
) {
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "regression-fields",
      div(
        class = "regression-variables-panel",
        div(
          class = "regression-setup-variable-box",
          div("Variables", class = "regression-setup-variable-title"),
          if (length(available_predictors) == 0) {
            div("No predictor variables selected.", class = "regression-variable-empty")
          } else {
            selectInput(
              "generalized_available_predictors",
              label = NULL,
              choices = available_choices,
              selected = utils::head(available_predictors, 1),
              multiple = FALSE,
              selectize = FALSE,
              size = available_list_size
            )
          }
        )
      ),
      div(
        class = "variable-transfer-actions",
        tags$button(">", type = "button", class = "btn btn-default btn-sm variable-transfer-button", disabled = "disabled"),
        tags$button("<", type = "button", class = "btn btn-default btn-sm variable-transfer-button", disabled = "disabled")
      ),
      div(
        class = "regression-field",
        tags$label("Dependent Variables", `for` = "generalized_y", class = "control-label"),
        selectInput(
          "generalized_y",
          label = NULL,
          choices = dependent_choices,
          selected = utils::head(ordered_dependents, 1),
          multiple = FALSE,
          selectize = FALSE,
          size = dependent_list_size
        ),
        div(
          class = "dependent-order-actions",
          tags$button("Up", type = "button", class = "btn btn-default btn-sm", disabled = "disabled"),
          tags$button("Down", type = "button", class = "btn btn-default btn-sm", disabled = "disabled")
        )
      ),
      div(
        class = "regression-field",
        tags$label("Independent Variables", class = "control-label"),
        selectInput(
          "generalized_predictor_order",
          label = NULL,
          choices = predictor_choices,
          selected = utils::head(ordered_predictors, 1),
          multiple = FALSE,
          selectize = FALSE,
          size = predictor_list_size
        ),
        div(
          class = "predictor-order-actions",
          tags$button("Up", type = "button", class = "btn btn-default btn-sm", disabled = "disabled"),
          tags$button("Down", type = "button", class = "btn btn-default btn-sm", disabled = "disabled")
        )
      ),
      div(
        class = "regression-options generalized-options",
        div(
          class = "regression-field generalized-option-field",
          selectInput(
            "generalized_family",
            "Outcome model",
            choices = family_choices,
            selected = "count",
            selectize = FALSE
          )
        ),
        div(
          class = "regression-field generalized-option-field",
          selectInput(
            "generalized_link",
            "Link function",
            choices = link_choices,
            selected = "default",
            selectize = FALSE
          )
        ),
        div(
          class = "regression-effect-options",
          checkboxInput("generalized_exponentiate", "Report exp(B): IRR / ratio", value = TRUE),
          checkboxInput("generalized_robust_se", "Robust standard errors", value = TRUE),
          checkboxInput("generalized_overdispersion", "Overdispersion check", value = TRUE),
          checkboxInput("generalized_show_vif", "Collinearity diagnostics(VIF)", value = FALSE)
        )
      )
    ),
    div(
      class = "regression-actions",
      tags$button("Run generalized", type = "button", class = "btn btn-primary", disabled = "disabled")
    )
  )
}
