# Hierarchical regression setup UI state and panel.

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

