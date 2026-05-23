# Principal component analysis setup UI state and panel.

pca_setup_state <- function(
  selected_names,
  pca_variables = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = character(0),
  selected_selected = character(0),
  matrix_type = "correlation",
  rotation = "none",
  criterion = "eigen",
  n_components = 1,
  cumulative_variance = 70,
  scree_plot = TRUE,
  component_plot = TRUE
) {
  selected <- as.character(selected_names %||% character(0))
  allowed <- analysis_allowed_variables(selected, variable_table, c("ordered", "continuous"))
  pca_variables <- intersect(as.character(pca_variables %||% character(0)), allowed)
  available <- setdiff(allowed, pca_variables)

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    pca_variables = pca_variables,
    selected_items = analysis_variable_items(pca_variables, variable_table, labels),
    selected_selected = selected_order_items(selected_selected, pca_variables),
    move_disabled = length(allowed) == 0,
    matrix_type = matrix_type,
    rotation = rotation,
    criterion = criterion,
    n_components = max(1L, as.integer(n_components %||% 1L)),
    cumulative_variance = min(max(as.numeric(cumulative_variance %||% 70), 1), 100),
    scree_plot = isTRUE(scree_plot),
    component_plot = isTRUE(component_plot)
  )
}

pca_setup_panel <- function(state) {
  div(
    class = "correlation-setup-grid pca-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", c("ordered", "continuous")),
      analysis_transfer_listbox_input("pca_available", state$available_items, selected = state$available_selected, size = 19)
    ),
    div(
      class = "analysis-transfer-controls correlation-transfer-controls",
      actionButton(
        "pca_move",
        ">",
        class = "btn btn-default analysis-move-button",
        disabled = if (isTRUE(state$move_disabled)) "disabled" else NULL
      )
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Selected Variables", c("ordered", "continuous")),
      analysis_transfer_listbox_input("pca_selected", state$selected_items, selected = state$selected_selected, size = 19),
      div(
        class = "analysis-order-actions correlation-order-actions",
        actionButton("pca_move_up", "Up", class = "btn-default btn-sm"),
        actionButton("pca_move_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "correlation-options-column pca-options-column",
      div(
        class = "analysis-options-panel correlation-options pca-options",
        analysis_radio_group("Matrix", "pca_matrix_type", pca_matrix_choices(), selected = state$matrix_type),
        analysis_radio_group("Rotation", "pca_rotation", pca_rotation_choices(), selected = state$rotation),
        div(
          class = "analysis-option-group analysis-radio-group pca-selection-group",
          div(class = "analysis-option-title", "Component selection"),
          radioButtons(
            "pca_criterion",
            label = NULL,
            choices = pca_criterion_choices(),
            selected = state$criterion
          ),
          div(
            class = "factor-fixed-number-input pca-fixed-number-input",
            numericInput("pca_n_components", "Number of components", value = state$n_components, min = 1, step = 1)
          ),
          div(
            class = "factor-fixed-number-input pca-cumulative-input",
            numericInput("pca_cumulative_variance", "Cumulative variance (%)", value = state$cumulative_variance, min = 1, max = 100, step = 1)
          )
        ),
        analysis_option_group(
          "Plot",
          list(
            list(id = "pca_scree_plot", label = "scree plot", value = state$scree_plot),
            list(id = "pca_component_plot", label = "component plot", value = state$component_plot)
          )
        )
      )
    )
  )
}
