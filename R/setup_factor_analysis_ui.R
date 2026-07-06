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
  options_tab = "Model",
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  selected <- as.character(selected_names %||% character(0))
  allowed <- analysis_allowed_variables(selected, variable_table, c("ordered", "continuous"))
  factor_variables <- intersect(as.character(factor_variables %||% character(0)), allowed)
  available <- setdiff(allowed, factor_variables)
  normality_method <- as.character(normality_method %||% "skew_kurt")
  if (!normality_method %in% unname(factor_analysis_normality_method_choices())) {
    normality_method <- "skew_kurt"
  }
  assumption <- if (isTRUE(normality)) normality_method else "none"

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
    assumption = assumption,
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
    options_tab = if (options_tab %in% c("Model", "Output", "Scores")) options_tab else "Model",
    language = language
  )
}

factor_analysis_save_name_row <- function(value, language = statedu_initial_language()) {
  div(
    class = "factor-save-name-row",
    tags$label(`for` = "factor_save_factor_base_name", analysis_ui_text("Variable name", language)),
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

factor_analysis_select_group <- function(title, id, choices, selected, extra_class = "", disabled = FALSE, language = statedu_initial_language()) {
  selected <- as.character(selected %||% unname(choices)[[1]])
  if (!selected %in% unname(choices)) {
    selected <- unname(choices)[[1]]
  }
  div(
    class = paste(
      "analysis-option-group factor-select-group",
      extra_class,
      if (isTRUE(disabled)) "ttest-normality-disabled" else ""
    ),
    div(class = "analysis-option-title", analysis_ui_text(title, language)),
    selectInput(
      id,
      label = NULL,
      choices = analysis_ui_choices(choices, language = language),
      selected = selected,
      width = "100%",
      selectize = FALSE
    )
  )
}

factor_analysis_selection_controls <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
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
      radio_input(analysis_ui_text("Eigenvalue >= 1.0", language), "eigen"),
      radio_input(
        analysis_ui_text("Fixed number of factors", language),
        "fixed",
        div(
          class = "factor-fixed-number-input",
          tags$input(
            id = "factor_n_factors",
            type = "number",
            class = "form-control",
            value = state$n_factors,
            min = 1,
            step = 1
          )
        ),
        class = "radio factor-fixed-radio-row"
      )
    )
  )
}

factor_analysis_setup_panel <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  div(
    class = "correlation-setup-grid factor-analysis-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", c("ordered", "continuous"), language = language),
      analysis_transfer_listbox_input("factor_available", state$available_items, selected = state$available_selected, size = 17)
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
      analysis_field_label_tag("Selected Variables", c("ordered", "continuous"), language = language),
      analysis_transfer_listbox_input("factor_selected", state$selected_items, selected = state$selected_selected, size = 17),
      div(
        class = "analysis-order-actions correlation-order-actions",
        actionButton("factor_move_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"),
        actionButton("factor_move_down", analysis_ui_text("Down", language), class = "btn-default btn-sm")
      )
    ),
    div(
      class = "correlation-options-column factor-analysis-options-column",
      analysis_options_tabs_panel(
        id = "factor_options_tab",
        selected = state$options_tab,
        class = "correlation-options factor-analysis-options",
          tabPanel(
            analysis_ui_text("Model", language),
            value = "Model",
            div(
              class = "factor-options-tab-content",
              factor_analysis_select_group(
                "Matrix",
                "factor_matrix_type",
                factor_analysis_matrix_choices(),
                selected = state$matrix_type,
                language = language
              ),
              factor_analysis_select_group(
                "Normality",
                "factor_assumption",
                factor_analysis_assumption_choices(),
                selected = state$assumption,
                language = language
              ),
              factor_analysis_select_group(
                "Method",
                "factor_method",
                factor_analysis_method_choices(),
                selected = state$method,
                extra_class = "factor-method-group",
                disabled = isTRUE(state$normality),
                language = language
              ),
              factor_analysis_select_group(
                "Rotation",
                "factor_rotation",
                factor_analysis_rotation_choices(),
                selected = state$rotation,
                language = language
              ),
              div(
                class = "analysis-option-group analysis-radio-group factor-selection-group",
                div(class = "analysis-option-title", analysis_ui_text("Factor selection", language)),
                factor_analysis_selection_controls(state)
              )
            )
          ),
          tabPanel(
            analysis_ui_text("Output", language),
            value = "Output",
            div(
              class = "factor-options-tab-content",
              analysis_option_group(
                analysis_ui_text("Output", language),
                list(
                  list(id = "factor_sort_loadings", label = analysis_ui_text("Sort loadings by size", language), value = state$sort_loadings),
                  list(id = "factor_hide_small_loadings", label = analysis_ui_text("Show loadings >= .30 only", language), value = state$hide_small_loadings),
                  list(id = "factor_highlight_problem_values", label = analysis_ui_text("Highlight problem values", language), value = state$highlight_problem_values),
                  list(id = "factor_subfactor_reliability", label = analysis_ui_text("Subfactor reliability", language), value = state$subfactor_reliability)
                )
              )
            )
          ),
          tabPanel(
            analysis_ui_text("Save scores", language),
            value = "Scores",
            div(
              class = "factor-options-tab-content",
              div(
                class = "analysis-option-group factor-save-score-group",
                div(class = "analysis-option-title", analysis_ui_text("Save scores", language)),
                factor_analysis_save_name_row(state$save_factor_base_name, language),
                factor_analysis_save_score_row("factor_save_factor_means", analysis_ui_text("Factor item means", language), state$save_factor_means),
                factor_analysis_save_score_row("factor_save_factor_sums", analysis_ui_text("Factor item sums", language), state$save_factor_sums),
                factor_analysis_save_score_row("factor_save_factor_scores", analysis_ui_text("Factor scores", language), state$save_factor_scores)
              )
            )
          )
      )
    )
  )
}
