frequencies_tab_panel <- function() {
  tabPanel(
    "Frequencies",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Frequencies / Descriptives"),
        div("Move variables into the analysis list and select summary options.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3("Frequencies / Descriptives"),
        uiOutput("frequencies_setup"),
        div(
          class = "analysis-action-row frequencies-action-row",
          actionButton("run_frequencies", "Run analysis", class = "btn btn-primary"),
          uiOutput("frequencies_save_control")
        ),
        uiOutput("frequencies_results")
      )
    )
  )
}

ttest_anova_tab_panel <- function() {
  tabPanel(
    "t-test / ANOVA",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("t-test / ANOVA"),
        div("Move variables into the analysis lists and select test options.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel ttest-anova-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3("t-test / ANOVA"),
        uiOutput("ttest_anova_setup"),
        div(
          class = "analysis-action-row ttest-anova-action-row",
          actionButton("run_ttest_anova", "Run analysis", class = "btn btn-primary")
        ),
        uiOutput("ttest_anova_results")
      )
    )
  )
}

correlation_tab_panel <- function() {
  tabPanel(
    "Correlation",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Correlation"),
        div("Move variables into the analysis list and select correlation options.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel correlation-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3("Correlation"),
        uiOutput("correlation_setup"),
        div(
          class = "analysis-action-row correlation-action-row",
          actionButton("run_correlation", "Run analysis", class = "btn btn-primary")
        ),
        uiOutput("correlation_results")
      )
    )
  )
}

regression_tab_panel <- function() {
  tabPanel(
    "Regression",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Regression"),
        div("Review selected variables and run regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel regression-workspace-panel",
        h3("Regression"),
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
}

hierarchical_tab_panel <- function() {
  tabPanel(
    "Hierarchical",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Hierarchical"),
        div("Review selected variables and prepare hierarchical regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel hierarchical-workspace-panel",
        style = "min-width:980px;overflow-x:auto;",
        h3("Hierarchical"),
        uiOutput("hierarchical_setup"),
        uiOutput("hierarchical_results")
      )
    )
  )
}

generalized_tab_panel <- function() {
  tabPanel(
    "Generalized",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("Generalized"),
        div("Review selected variables and prepare generalized regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "regression-layout",
        div(
          class = "side-panel",
          uiOutput("generalized_variable_list")
        ),
        div(
          class = "workspace-panel",
          h3("Generalized"),
          uiOutput("generalized_setup"),
          div(
            class = "empty-message regression-results-empty",
            "Generalized regression models are not implemented yet."
          )
        )
      )
    )
  )
}

setup_empty_message <- function(message) {
  tagList(
    div(
      class = "empty-message",
      div(message)
    )
  )
}

setup_status_message <- function(selection_applied, roles_applied) {
  if (!isTRUE(selection_applied)) {
    return("Step 2 variable selection has not been applied yet.")
  }
  if (!isTRUE(roles_applied)) {
    return("Step 3 role assignment has not been applied yet.")
  }
  NULL
}

bootstrap_resample_choices <- function() {
  c(
    "1000 (test)" = "1000",
    "5000" = "5000",
    "10000" = "10000",
    "20000" = "20000",
    "50000 (recommended)" = "50000"
  )
}

normalized_bootstrap_resamples <- function(value, choices = bootstrap_resample_choices()) {
  current <- as.character(value %||% "1000")
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

generalized_family_choices <- function() {
  c(
    "Poisson / Negative binomial / Zero-inflated (count)" = "count",
    "Gamma (positive continuous)" = "gamma"
  )
}

generalized_link_choices <- function() {
  c(
    "Default for family" = "default",
    "log" = "log",
    "identity" = "identity",
    "inverse" = "inverse"
  )
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
    paste(measurement_icon_label(measurements[[name]]), named_value(labels_by_value, name, name))
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
    variable_block("Dependent Variables", intersect(dependent, selected)),
    variable_block("Independent Variables", intersect(independent, selected)),
    variable_block("Covariates", intersect(controls, selected))
  )
}

