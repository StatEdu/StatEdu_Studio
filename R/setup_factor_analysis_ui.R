# Factor analysis setup UI state and panel.

factor_analysis_setup_state <- function(
  selected_names,
  factor_variables = character(0),
  variable_table = NULL,
  labels = character(0),
  selected_available = character(0),
  selected_selected = character(0),
  matrix_type = "pearson",
  normality = FALSE,
  normality_method = "skew_kurt",
  method = "pa",
  rotation = "varimax",
  criterion = "eigen",
  n_factors = 1,
  sort_loadings = TRUE,
  hide_small_loadings = TRUE,
  highlight_problem_values = TRUE,
  subfactor_reliability = TRUE,
  save_factor_means = FALSE,
  save_factor_sums = FALSE,
  save_factor_scores = FALSE,
  save_factor_base_name = "FA",
  options_tab = "Model"
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
    matrix_type = if (matrix_type %in% unname(factor_analysis_matrix_choices())) matrix_type else "pearson",
    normality = isTRUE(normality),
    normality_method = normality_method,
    method = method,
    rotation = rotation,
    criterion = criterion,
    n_factors = max(1L, as.integer(n_factors %||% 1L)),
    sort_loadings = isTRUE(sort_loadings),
    hide_small_loadings = isTRUE(hide_small_loadings),
    highlight_problem_values = isTRUE(highlight_problem_values),
    subfactor_reliability = isTRUE(subfactor_reliability),
    save_factor_means = isTRUE(save_factor_means),
    save_factor_sums = isTRUE(save_factor_sums),
    save_factor_scores = isTRUE(save_factor_scores),
    save_factor_base_name = trimws(as.character(save_factor_base_name %||% "FA")),
    options_tab = if (options_tab %in% c("Model", "Output", "Scores")) options_tab else "Model"
  )
}

factor_analysis_save_name_row <- function(value) {
  div(
    class = "factor-save-name-row",
    tags$label(`for` = "factor_save_factor_base_name", "Variable name"),
    tags$input(
      id = "factor_save_factor_base_name",
      type = "text",
      class = "form-control factor-save-name-input",
      value = value
    )
  )
}

factor_analysis_save_score_row <- function(id, label, value) {
  div(
    class = "factor-save-score-row",
    checkboxInput(id, label, value = value, width = NULL)
  )
}

factor_analysis_selection_controls <- function(state) {
  choices <- factor_analysis_criterion_choices()
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
          name = "factor_criterion",
          value = value,
          checked = if (identical(selected, value)) "checked" else NULL
        ),
        span(label)
      ),
      extra
    )
  }
  div(
    id = "factor_criterion",
    class = "form-group shiny-input-radiogroup shiny-input-container factor-selection-compact",
    div(
      class = "shiny-options-group",
      radio_input("Eigenvalue >= 1.0", "eigen"),
      radio_input(
        "Fixed number of factors",
        "fixed",
        div(
          class = "factor-fixed-number-input",
          style = "display:inline-block;flex:0 0 56px;margin:0;width:56px;",
          tags$input(
            id = "factor_n_factors",
            type = "number",
            class = "form-control",
            value = state$n_factors,
            min = 1,
            step = 1,
            style = "display:inline-block;height:30px;min-width:0;padding:3px 6px;width:56px;"
          )
        ),
        class = "radio factor-fixed-radio-row",
        style = "align-items:center;display:flex;flex-wrap:nowrap;gap:6px;margin-left:0;width:252px;",
        label_style = "flex:0 0 auto;margin-bottom:0;white-space:nowrap;"
      )
    )
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
        div(class = "analysis-option-title factor-options-title", "Options"),
        tabsetPanel(
          id = "factor_options_tab",
          type = "tabs",
          selected = state$options_tab,
          tabPanel(
            "Model",
            div(
              class = "factor-options-tab-content",
              analysis_radio_group(
                "Matrix",
                "factor_matrix_type",
                factor_analysis_matrix_choices(),
                selected = state$matrix_type
              ),
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
              div(
                class = paste(
                  "analysis-option-group analysis-radio-group factor-method-group",
                  if (isTRUE(state$normality)) "ttest-normality-disabled" else ""
                ),
                div(class = "analysis-option-title", "Method"),
                radioButtons(
                  "factor_method",
                  label = NULL,
                  choices = factor_analysis_method_choices(),
                  selected = state$method
                )
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
                factor_analysis_selection_controls(state)
              )
            )
          ),
          tabPanel(
            "Output",
            div(
              class = "factor-options-tab-content",
              analysis_option_group(
                "Output",
                list(
                  list(id = "factor_sort_loadings", label = "Sort loadings by size", value = state$sort_loadings),
                  list(id = "factor_hide_small_loadings", label = "Show loadings >= .30 only", value = state$hide_small_loadings),
                  list(id = "factor_highlight_problem_values", label = "Highlight problem values", value = state$highlight_problem_values),
                  list(id = "factor_subfactor_reliability", label = "Subfactor reliability", value = state$subfactor_reliability)
                )
              )
            )
          ),
          tabPanel(
            "Scores",
            div(
              class = "factor-options-tab-content",
              div(
                class = "analysis-option-group factor-save-score-group",
                div(class = "analysis-option-title", "Save scores"),
                factor_analysis_save_name_row(state$save_factor_base_name),
                factor_analysis_save_score_row("factor_save_factor_means", "Factor item means", state$save_factor_means),
                factor_analysis_save_score_row("factor_save_factor_sums", "Factor item sums", state$save_factor_sums),
                factor_analysis_save_score_row("factor_save_factor_scores", "Factor scores", state$save_factor_scores)
              )
            )
          )
        )
      )
    )
  )
}
