# Factor analysis setup UI state and panel.

factor_analysis_setup_state <- function(
  selected_names,
  factor_variables = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = character(0),
  selected_selected = character(0),
  normality = FALSE,
  normality_method = "skew_kurt",
  method = "pa",
  rotation = "varimax",
  criterion = "eigen",
  n_factors = 1
) {
  selected <- as.character(selected_names %||% character(0))
  allowed <- analysis_allowed_variables(selected, variable_table, c("ordered", "continuous"))
  factor_variables <- intersect(as.character(factor_variables %||% character(0)), allowed)
  available <- setdiff(allowed, factor_variables)

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    factor_variables = factor_variables,
    selected_items = analysis_variable_items(factor_variables, variable_table, labels),
    selected_selected = selected_order_items(selected_selected, factor_variables),
    move_disabled = length(allowed) == 0,
    normality = isTRUE(normality),
    normality_method = normality_method,
    method = method,
    rotation = rotation,
    criterion = criterion,
    n_factors = max(1L, as.integer(n_factors %||% 1L))
  )
}

factor_analysis_setup_panel <- function(state) {
  div(
    class = "correlation-setup-grid factor-analysis-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", c("ordered", "continuous")),
      analysis_transfer_listbox_input("factor_available", state$available_items, selected = state$available_selected, size = 19)
    ),
    div(
      class = "analysis-transfer-controls correlation-transfer-controls",
      actionButton(
        "factor_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (isTRUE(state$move_disabled)) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Selected Variables", c("ordered", "continuous")),
      analysis_transfer_listbox_input("factor_selected", state$selected_items, selected = state$selected_selected, size = 19),
      div(
        class = "analysis-order-actions correlation-order-actions",
        actionButton("factor_move_up", "Up", class = "btn-default btn-sm"),
        actionButton("factor_move_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "correlation-options-column factor-analysis-options-column",
      div(
        class = "analysis-options-panel correlation-options factor-analysis-options",
        analysis_option_group(
          "Assumption",
          list(
            list(id = "factor_normality", label = "normality test", value = state$normality)
          )
        ),
        div(
          class = paste(
            "analysis-option-group analysis-radio-group factor-normality-method-group",
            if (!isTRUE(state$normality)) "ttest-normality-disabled" else ""
          ),
          div(class = "analysis-option-title", "Normality method"),
          radioButtons(
            "factor_normality_method",
            label = NULL,
            choices = factor_analysis_normality_method_choices(),
            selected = state$normality_method
          )
        ),
        analysis_radio_group(
          "Method",
          "factor_method",
          factor_analysis_method_choices(),
          selected = state$method
        ),
        analysis_radio_group(
          "Rotation",
          "factor_rotation",
          factor_analysis_rotation_choices(),
          selected = state$rotation
        ),
        div(
          class = "analysis-option-group analysis-radio-group factor-selection-group",
          div(class = "analysis-option-title", "Factor selection"),
          radioButtons(
            "factor_criterion",
            label = NULL,
            choices = factor_analysis_criterion_choices(),
            selected = state$criterion
          ),
          div(
            class = "factor-fixed-number-input",
            numericInput("factor_n_factors", "Number of factors", value = state$n_factors, min = 1, step = 1)
          )
        )
      )
    )
  )
}
