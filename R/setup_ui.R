reliability_tab_panel <- function(title = "Reliability", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Reliability", language)),
        div(statedu_t("analysis.subtitle_reliability", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel reliability-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Reliability", "reliability", language),
        analysis_workspace_body(
          "reliability",
          uiOutput("reliability_setup"),
          analysis_three_block_action_row(
            class = "reliability-action-row",
            run_button = actionButton("run_reliability", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("reliability_reset_control"),
            save_control = uiOutput("reliability_save_control")
          ),
          uiOutput("reliability_results")
        )
      )
    )
  )
}

frequencies_tab_panel <- function(title = "Frequencies", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Frequencies / Descriptives", language)),
        div(statedu_t("analysis.subtitle_frequencies", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Frequencies / Descriptives", "frequencies", language),
        analysis_workspace_body(
          "frequencies",
          uiOutput("frequencies_setup"),
          analysis_three_block_action_row(
            class = "frequencies-action-row",
            run_button = actionButton("run_frequencies", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("frequencies_reset_control"),
            save_control = uiOutput("frequencies_save_control")
          ),
          uiOutput("frequencies_results")
        )
      )
    )
  )
}

paired_tab_panel <- function(title = "Paired test", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "paired",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Paired test", language)),
        div(statedu_t("analysis.subtitle_paired", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel paired-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Paired test", "paired", language),
        analysis_workspace_body(
          "paired",
          uiOutput("paired_setup"),
          analysis_three_block_action_row(
            class = "paired-action-row",
            run_button = actionButton("run_paired", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("paired_reset_control"),
            save_control = uiOutput("paired_save_control")
          ),
          uiOutput("paired_results")
        )
      )
    )
  )
}

nonparametric_paired_tab_panel <- function(title = "Nonparametric Paired", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "nonparametric_paired",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Nonparametric Paired Test", language)),
        div(statedu_t("analysis.subtitle_nonparametric_paired", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel paired-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Nonparametric paired test", "nonparametric_paired", language),
        analysis_workspace_body(
          "nonparametric_paired",
          uiOutput("nonparametric_paired_setup"),
          analysis_three_block_action_row(
            class = "paired-action-row",
            run_button = actionButton("run_nonparametric_paired", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("nonparametric_paired_reset_control"),
            save_control = uiOutput("nonparametric_paired_save_control")
          ),
          uiOutput("nonparametric_paired_results")
        )
      )
    )
  )
}

paired_rm_tab_panel <- function(title = "Paired test (3+)", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "paired_rm",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Paired test (3+)", language)),
        div(statedu_t("analysis.subtitle_paired_rm", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel paired-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Paired test (3+)", "paired_rm", language),
        analysis_workspace_body(
          "paired_rm",
          uiOutput("paired_rm_setup"),
          analysis_three_block_action_row(
            class = "paired-action-row",
            run_button = actionButton("run_paired_rm", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("paired_rm_reset_control"),
            save_control = uiOutput("paired_rm_save_control")
          ),
          uiOutput("paired_rm_results")
        )
      )
    )
  )
}

ttest_anova_tab_panel <- function(title = "t-test / ANOVA", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("t-test / ANOVA"),
        div(statedu_t("analysis.subtitle_ttest_anova", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel ttest-anova-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("t-test / ANOVA", "ttest_anova", language),
        analysis_workspace_body(
          "ttest_anova",
          uiOutput("ttest_anova_setup"),
          analysis_three_block_action_row(
            class = "analysis-action-row ttest-anova-action-row",
            run_button = actionButton("run_ttest_anova", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("ttest_anova_reset_control"),
            save_control = uiOutput("ttest_anova_save_control")
          ),
          uiOutput("ttest_anova_results")
        )
      )
    )
  )
}

ancova_tab_panel <- function(title = "ANCOVA", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("ANCOVA"),
        div(statedu_t("analysis.subtitle_ancova", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel ttest-anova-workspace-panel ancova-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("ANCOVA", "ancova", language),
        analysis_workspace_body(
          "ancova",
          uiOutput("ancova_setup"),
          analysis_three_block_action_row(
            class = "ttest-anova-action-row ancova-action-row",
            run_button = actionButton("run_ancova", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("ancova_reset_control"),
            save_control = uiOutput("ancova_save_control")
          ),
          uiOutput("ancova_results")
        )
      )
    )
  )
}

nonparametric_tab_panel <- function(title = "Nonparametric Tests", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Nonparametric Tests", language)),
        div(statedu_t("analysis.subtitle_nonparametric", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel ttest-anova-workspace-panel nonparametric-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Nonparametric tests", "nonparametric", language),
        analysis_workspace_body(
          "nonparametric",
          uiOutput("nonparametric_setup"),
          analysis_three_block_action_row(
            class = "ttest-anova-action-row nonparametric-action-row",
            run_button = actionButton("run_nonparametric", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("nonparametric_reset_control"),
            save_control = uiOutput("nonparametric_save_control")
          ),
          uiOutput("nonparametric_results")
        )
      )
    )
  )
}

correlation_tab_panel <- function(title = "Correlation", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Correlation", language)),
        div(statedu_t("analysis.subtitle_correlation", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel correlation-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Correlation", "correlation", language),
        analysis_workspace_body(
          "correlation",
          uiOutput("correlation_setup"),
          analysis_three_block_action_row(
            class = "correlation-action-row",
            run_button = actionButton("run_correlation", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("correlation_reset_control"),
            save_control = uiOutput("correlation_save_control")
          ),
          uiOutput("correlation_results")
        )
      )
    )
  )
}

factor_analysis_tab_panel <- function(title = "Factor Analysis", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Factor Analysis", language)),
        div(statedu_t("analysis.subtitle_factor", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel correlation-workspace-panel factor-analysis-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Factor Analysis", "factor", language),
        analysis_workspace_body(
          "factor",
          uiOutput("factor_analysis_setup"),
          analysis_three_block_action_row(
            class = "correlation-action-row factor-analysis-action-row",
            run_button = actionButton("run_factor_analysis", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("factor_analysis_reset_control"),
            save_control = uiOutput("factor_analysis_save_control")
          ),
          uiOutput("factor_analysis_results")
        )
      )
    )
  )
}

pca_tab_panel <- function(title = "Principal Components", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Principal Component Analysis", language)),
        div(statedu_t("analysis.subtitle_pca", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel correlation-workspace-panel pca-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Principal Component Analysis", "pca", language),
        analysis_workspace_body(
          "pca",
          uiOutput("pca_setup"),
          analysis_three_block_action_row(
            class = "correlation-action-row pca-action-row",
            run_button = actionButton("run_pca", statedu_ui_label("run_analysis", language), class = "btn btn-primary"),
            reset_control = uiOutput("pca_reset_control"),
            save_control = uiOutput("pca_save_control")
          ),
          uiOutput("pca_results")
        )
      )
    )
  )
}

regression_tab_panel <- function(title = "Regression", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Regression", language)),
        div(statedu_t("analysis.subtitle_regression", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel regression-workspace-panel analysis-three-block-workspace",
        analysis_workspace_heading("Regression", "regression", language),
        analysis_workspace_body(
          "regression",
          uiOutput("regression_setup"),
          div(
            class = "bootstrap-progress-slot",
            uiOutput("bootstrap_progress"),
            uiOutput("bootstrap_stop_control")
          ),
          uiOutput("regression_results")
        )
      )
    )
  )
}

hierarchical_tab_panel <- function(title = "Regression", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Regression", language)),
        div(statedu_t("analysis.subtitle_regression", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel hierarchical-workspace-panel analysis-three-block-workspace",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Regression", "hierarchical", language),
        analysis_workspace_body(
          "hierarchical",
          uiOutput("hierarchical_setup"),
          div(
            class = "bootstrap-progress-slot",
            uiOutput("hierarchical_bootstrap_progress"),
            uiOutput("hierarchical_bootstrap_stop_control")
          ),
          uiOutput("hierarchical_results")
        )
      )
    )
  )
}

generalized_tab_panel <- function(title = "GLM", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "analysis_generalized",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Generalized Linear Model (GLM)", language)),
        div(statedu_t("analysis.subtitle_generalized", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel generalized-workspace-panel analysis-three-block-workspace",
        analysis_workspace_heading("Generalized Linear Model (GLM)", "generalized", language),
        analysis_workspace_body(
          "generalized",
          uiOutput("generalized_setup"),
          uiOutput("generalized_results")
        )
      )
    )
  )
}

longitudinal_tab_panel <- function(title = "Longitudinal / Panel Models", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "analysis_longitudinal",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(analysis_ui_text("Longitudinal / Panel Models", language)),
        div(statedu_t("analysis.subtitle_longitudinal", language), class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel longitudinal-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        analysis_workspace_heading("Longitudinal / Panel Models", "longitudinal", language),
        analysis_workspace_body(
          "longitudinal",
          uiOutput("longitudinal_setup"),
          uiOutput("longitudinal_results")
        )
      )
    )
  )
}

setup_empty_message <- function(message, language = statedu_initial_language()) {
  message <- analysis_ui_text(message, language)
  div(
    class = "frequencies-setup-grid easyflow-empty-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
      tags$select(class = "analysis-transfer-listbox form-control", multiple = NA, size = 19)
    ),
    div(
      class = "analysis-transfer-controls",
      tags$button(type = "button", class = "btn btn-default analysis-move-button", disabled = NA, ">")
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Selected Variables", language = language),
      tags$select(class = "analysis-transfer-listbox form-control", multiple = NA, size = 19),
      div(
        class = "dependent-order-actions",
        tags$button(type = "button", class = "btn btn-default btn-sm", disabled = NA, analysis_ui_text("Up", language)),
        tags$button(type = "button", class = "btn btn-default btn-sm", disabled = NA, analysis_ui_text("Down", language))
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel",
      div(class = "analysis-option-title", analysis_ui_text("Options", language)),
      div(class = "empty-setup-message", message)
    )
  )
}

setup_status_message <- function(selection_applied, roles_applied) {
  if (!isTRUE(selection_applied)) {
    return(statedu_t("analysis.status_step2_not_applied", statedu_initial_language()))
  }
  if (!isTRUE(roles_applied)) {
    return(statedu_t("analysis.status_step3_not_applied", statedu_initial_language()))
  }
  NULL
}

bootstrap_resample_choices <- function(language = statedu_initial_language()) {
  c(
    stats::setNames("1000", sprintf("1000 (%s)", analysis_ui_text("test", language))),
    "5000" = "5000",
    "10000" = "10000",
    "20000" = "20000",
    stats::setNames("50000", sprintf("50000 (%s)", analysis_ui_text("recommended", language)))
  )
}

normalized_bootstrap_resamples <- function(value, choices = bootstrap_resample_choices()) {
  current <- as.character(value %||% character(0))
  current <- current[!is.na(current) & nzchar(current)]
  if (length(current) == 0) {
    current <- "1000"
  } else {
    current <- current[[1]]
  }
  if (!current %in% unname(choices)) {
    return("1000")
  }
  current
}

reset_setup_inputs <- function(session) {
  updateCheckboxInput(session, "header", value = TRUE)
  updateSelectInput(session, "dat_delimiter", selected = "whitespace")
  updateCheckboxInput(session, "dat_has_names", value = FALSE)
  updateSelectInput(session, "id_var", selected = "")
  updateSelectInput(session, "filter_var", selected = "")
  updateTextInput(session, "filter_condition", value = "")
  updateSelectInput(session, "y", selected = character(0))
  updateSelectizeInput(session, "xs", selected = character(0))
  updateSelectizeInput(session, "covariates", selected = character(0))
  updateSelectInput(session, "boot_r", selected = "1000")
  updateNumericInput(session, "seed", value = default_seed())
}

restore_setup_inputs <- function(session, settings) {
  if (!is.null(settings$bootstrap_resamples)) {
    updateSelectInput(session, "boot_r", selected = as.character(settings$bootstrap_resamples))
  }
  if (!is.null(settings$seed)) {
    updateNumericInput(session, "seed", value = settings$seed)
  }
}

setup_list_size <- function(items, min_size = 4, max_size = 10) {
  min(max(length(items), min_size), max_size)
}

measurement_icon_label <- function(measurement) {
  measurement <- tolower(as.character(measurement %||% ""))
  switch(
    measurement,
    continuous = "\u223F",
    binary = "\u25D0",
    category = "\u25C6",
    ordered = "\u2582\u2585\u2588",
    "\u25CB"
  )
}

display_variable_choices_with_measurements <- function(names, table = NULL, labels = character(0)) {
  choices <- display_variable_choices_static(names, table, labels)
  variable_names <- as.character(names %||% character(0))
  if (is.null(table) || !all(c("name", "measurement") %in% names(table)) || length(variable_names) == 0) {
    return(choices)
  }

  measurements <- stats::setNames(as.character(table$measurement), as.character(table$name))
  choice_values <- unname(choices)
  labels_by_value <- stats::setNames(names(choices), choice_values)
  icon_labels <- vapply(variable_names, function(name) {
    paste(measurement_icon_label(named_value(measurements, name, "")), named_value(labels_by_value, name, name))
  }, character(1))
  stats::setNames(choice_values, icon_labels)
}

variable_choice_items <- function(names, table = NULL, labels = character(0)) {
  variable_names <- as.character(names %||% character(0))
  if (length(variable_names) == 0) {
    return(list())
  }

  measurements <- character(0)
  if (!is.null(table) && all(c("name", "measurement") %in% names(table))) {
    measurements <- stats::setNames(as.character(table$measurement), as.character(table$name))
  }

  lapply(variable_names, function(name) {
    list(
      value = name,
      label = display_variable_name_static(name, table, labels),
      measurement = named_value(measurements, name, "")
    )
  })
}

measurement_symbol_tag <- function(measurement) {
  measurement <- tolower(as.character(measurement %||% ""))
  measurement_label <- switch(
    measurement,
    category = "nominal",
    ordered = "ordinal",
    measurement
  )
  span(
    class = paste("measurement-symbol", paste0("measurement-", measurement)),
    title = measurement_label,
    `aria-label` = measurement_label
  )
}

variable_icon_listbox_input <- function(input_id, items, selected = NULL, size = 8) {
  values <- vapply(items, `[[`, character(1), "value")
  labels <- vapply(items, `[[`, character(1), "label")
  selected <- selected_order_item(selected, values)
  height_px <- max(4, as.integer(size %||% 8)) * 24

  tagList(
    tags$select(
      id = input_id,
      class = "easyflow-hidden-select",
      style = "display:none;",
      lapply(seq_along(values), function(index) {
        tags$option(
          value = values[[index]],
          selected = if (identical(values[[index]], selected)) "selected" else NULL,
          labels[[index]]
        )
      })
    ),
    div(
      class = "variable-icon-listbox",
      role = "listbox",
      tabindex = "0",
      `data-input-id` = input_id,
      style = paste0("height:", height_px, "px;"),
      lapply(items, function(item) {
        value <- as.character(item$value)
        div(
          class = paste("variable-icon-option", if (identical(value, selected)) "is-selected" else ""),
          role = "option",
          `data-value` = value,
          onclick = "window.easyflowSelectVariableIconOption && window.easyflowSelectVariableIconOption(this);",
          measurement_symbol_tag(item$measurement),
          span(item$label, class = "variable-icon-option-label")
        )
      })
    )
  )
}

regression_role_variable_list <- function(
  table,
  selected = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  labels = character(0)
) {
  if (is.null(table) || nrow(table) == 0 || length(selected) == 0) {
    return(div(
      class = "empty-message",
      div("Select variables and apply roles in the Data tab.")
    ))
  }

  variable_block <- function(title, names) {
    names <- as.character(names)
    names <- names[nzchar(names)]
    rows <- table[match(names, table$name), , drop = FALSE]
    rows <- rows[!is.na(rows$name), , drop = FALSE]

    div(
      class = "regression-variable-block",
      div(
        class = "regression-variable-block-title",
        span(title)
      ),
      if (length(names) == 0) {
        div("No variables selected.", class = "regression-variable-empty")
      } else {
        div(
          class = "regression-variable-listbox",
          lapply(seq_len(nrow(rows)), function(index) {
            row <- rows[index, , drop = FALSE]
            name <- as.character(row$name)
            display_name <- display_variable_name_static(name, rows, labels)
            measurement <- as.character(row$measurement %||% "")
            div(
              class = "regression-variable-option",
              measurement_symbol_tag(measurement),
              span(display_name, class = "regression-variable-option-name")
            )
          })
        )
      }
    )
  }

  tagList(
    variable_block("Dependent variables", intersect(dependent, selected)),
    variable_block("Independent variables", intersect(independent, selected)),
    variable_block("Covariates", intersect(controls, selected))
  )
}
