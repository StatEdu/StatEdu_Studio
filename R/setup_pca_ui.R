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
  sort_loadings = TRUE,
  hide_small_loadings = TRUE,
  highlight_problem_values = TRUE,
  scree_plot = TRUE,
  biplot = TRUE,
  save_component_scores = FALSE,
  save_component_base_name = "PCA"
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
    sort_loadings = isTRUE(sort_loadings),
    hide_small_loadings = isTRUE(hide_small_loadings),
    highlight_problem_values = isTRUE(highlight_problem_values),
    scree_plot = isTRUE(scree_plot),
    biplot = isTRUE(biplot),
    save_component_scores = isTRUE(save_component_scores),
    save_component_base_name = trimws(as.character(save_component_base_name %||% "PCA"))
  )
}

pca_save_name_row <- function(value) {
  div(
    class = "factor-save-name-row pca-save-name-row",
    tags$label(`for` = "pca_save_component_base_name", "Variable name"),
    tags$input(
      id = "pca_save_component_base_name",
      type = "text",
      class = "form-control factor-save-name-input pca-save-name-input",
      value = value
    )
  )
}

pca_save_score_row <- function(id, label, value) {
  div(
    class = "factor-save-score-row pca-save-score-row",
    checkboxInput(id, label, value = value, width = NULL)
  )
}

pca_selection_controls <- function(state) {
  choices <- pca_criterion_choices()
  selected <- as.character(state$criterion %||% "eigen")
  if (!selected %in% unname(choices)) {
    selected <- "eigen"
  }
  radio_input <- function(label, value, extra = NULL, class = "radio", style = NULL, label_style = NULL) {
    div(
      class = class,
      style = style,
      tags$label(
        style = label_style,
        tags$input(
          type = "radio",
          name = "pca_criterion",
          value = value,
          checked = if (identical(selected, value)) "checked" else NULL
        ),
        span(label)
      ),
      extra
    )
  }
  div(
    id = "pca_criterion",
    class = "form-group shiny-input-radiogroup shiny-input-container",
    div(
      class = "shiny-options-group",
      radio_input("Eigenvalue >= 1.0", "eigen"),
      radio_input(
        "Fixed number of components",
        "fixed",
        div(
          class = "factor-fixed-number-input pca-fixed-number-input",
          tags$input(
            id = "pca_n_components",
            type = "number",
            class = "form-control",
            value = state$n_components,
            min = 1,
            step = 1
          )
        ),
        class = "radio factor-fixed-radio-row pca-selection-input-row pca-fixed-radio-row"
      ),
      radio_input(
        "Cumulative variance >=",
        "cumulative",
        div(
          class = "factor-fixed-number-input pca-cumulative-input",
          tags$input(
            id = "pca_cumulative_variance",
            type = "number",
            class = "form-control",
            value = state$cumulative_variance,
            min = 1,
            max = 100,
            step = 1
          ),
          span("%")
        ),
        class = "radio factor-fixed-radio-row pca-selection-input-row pca-cumulative-radio-row"
      )
    )
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
          pca_selection_controls(state)
        ),
        analysis_option_group(
          "Output",
          list(
            list(id = "pca_sort_loadings", label = "Sort loadings by size", value = state$sort_loadings),
            list(id = "pca_hide_small_loadings", label = "Show loadings >= .30 only", value = state$hide_small_loadings),
            list(id = "pca_highlight_problem_values", label = "Highlight problem values", value = state$highlight_problem_values),
            list(id = "pca_scree_plot", label = "Scree plot", value = state$scree_plot),
            list(id = "pca_biplot", label = "Biplot", value = state$biplot)
          )
        ),
        div(
          class = "analysis-option-group factor-save-score-group pca-save-score-group",
          div(class = "analysis-option-title", "Save scores"),
          pca_save_name_row(state$save_component_base_name),
          pca_save_score_row("pca_save_component_scores", "Component scores", state$save_component_scores)
        )
      )
    )
  )
}
