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
        class = "regression-layout",
        div(
          class = "side-panel",
          uiOutput("regression_variable_list")
        ),
        div(
          class = "workspace-panel",
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
        class = "regression-layout",
        div(
          class = "side-panel",
          uiOutput("hierarchical_variable_list")
        ),
        div(
          class = "workspace-panel",
          h3("Hierarchical"),
          uiOutput("hierarchical_setup"),
          div(
            class = "empty-message regression-results-empty",
            "Hierarchical regression models are not implemented yet."
          )
        )
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

hierarchical_setup_state <- function(
  ordered_dependents,
  block1,
  block2,
  block3,
  variable_table,
  labels = character(0),
  seed_value = NULL
) {
  independent <- c(block2, block3)
  list(
    dependent_choices = display_variable_choices_static(ordered_dependents, variable_table, labels),
    ordered_dependents = ordered_dependents,
    block1_choices = display_variable_choices_static(block1, variable_table, labels),
    block1 = block1,
    block2_choices = display_variable_choices_static(block2, variable_table, labels),
    block2 = block2,
    block3_choices = display_variable_choices_static(block3, variable_table, labels),
    block3 = block3,
    block_list_size = setup_list_size(independent),
    current_seed = seed_value %||% default_seed()
  )
}

hierarchical_setup_panel_from_state <- function(setup, status_message) {
  hierarchical_setup_panel(
    status_message = status_message,
    dependent_choices = setup$dependent_choices,
    ordered_dependents = setup$ordered_dependents,
    block1_choices = setup$block1_choices,
    block1 = setup$block1,
    block2_choices = setup$block2_choices,
    block2 = setup$block2,
    block3_choices = setup$block3_choices,
    block3 = setup$block3,
    block_list_size = setup$block_list_size,
    current_seed = setup$current_seed
  )
}

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

measurement_icon_label <- function(measurement) {
  measurement <- tolower(as.character(measurement %||% ""))
  switch(
    measurement,
    continuous = "C",
    binary = "B",
    category = "Cat",
    ordered = "Ord",
    "?"
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
              span(display_name, class = "regression-variable-option-name"),
              span(
                measurement_icon_label(measurement),
                class = paste("measurement-icon", paste0("measurement-", tolower(measurement))),
                title = measurement
              )
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

hierarchical_setup_panel <- function(
  status_message,
  dependent_choices,
  ordered_dependents,
  block1_choices,
  block1,
  block2_choices,
  block2,
  block3_choices,
  block3,
  block_list_size,
  current_seed
) {
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "regression-fields hierarchical-fields",
      div(
        class = "regression-field hierarchical-dependent-field",
        tags$label("Dependent Variable", `for` = "hierarchical_y", class = "control-label"),
        selectInput(
          "hierarchical_y",
          label = NULL,
          choices = dependent_choices,
          selected = utils::head(ordered_dependents, 1),
          multiple = FALSE,
          selectize = TRUE
        )
      ),
      div(
        class = "regression-field",
        tags$label("Block 1: Covariates", class = "control-label"),
        selectInput(
          "hierarchical_block1",
          label = NULL,
          choices = block1_choices,
          selected = utils::head(block1, 1),
          multiple = FALSE,
          selectize = FALSE,
          size = min(max(length(block1), 4), 10)
        )
      ),
      div(
        class = "regression-field",
        tags$label("Block 2: Independent variables", class = "control-label"),
        selectInput(
          "hierarchical_block2",
          label = NULL,
          choices = block2_choices,
          selected = utils::head(block2, 1),
          multiple = FALSE,
          selectize = FALSE,
          size = block_list_size
        )
      ),
      div(
        class = "variable-transfer-actions hierarchical-block-transfer",
        actionButton("move_hierarchical_block2_to_block3", ">", class = "btn btn-default btn-sm variable-transfer-button"),
        actionButton("move_hierarchical_block3_to_block2", "<", class = "btn btn-default btn-sm variable-transfer-button")
      ),
      div(
        class = "regression-field",
        tags$label("Block 3: Additional predictors", class = "control-label"),
        selectInput(
          "hierarchical_block3",
          label = NULL,
          choices = block3_choices,
          selected = utils::head(block3, 1),
          multiple = FALSE,
          selectize = FALSE,
          size = block_list_size
        )
      ),
      div(
        class = "regression-options",
        div(
          class = "regression-field",
          numericInput("hierarchical_seed", "Seed number", value = current_seed, min = 1, step = 1)
        ),
        div(
          class = "regression-effect-options",
          checkboxInput("hierarchical_show_sr2", "effect size sr\u00B2", value = FALSE),
          checkboxInput("hierarchical_show_f2", "effect size f\u00B2", value = FALSE),
          checkboxInput("hierarchical_show_vif", "Collinearity diagnostics(VIF)", value = FALSE)
        )
      )
    ),
    div(
      class = "regression-actions",
      tags$button("Run hierarchical", type = "button", class = "btn btn-primary", disabled = "disabled")
    )
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
