# Hierarchical regression setup UI state and panel.

hierarchical_setup_state <- function(
  selected_names,
  ordered_dependents,
  block1,
  block2,
  block3,
  variable_table,
  labels = character(0),
  bootstrap_value = NULL,
  seed_value = NULL
) {
  selected <- as.character(selected_names %||% character(0))
  ordered_dependents <- intersect(as.character(ordered_dependents %||% character(0)), selected)
  block1 <- intersect(as.character(block1 %||% character(0)), selected)
  block2 <- intersect(as.character(block2 %||% character(0)), selected)
  block3 <- intersect(as.character(block3 %||% character(0)), selected)
  assigned <- unique(c(ordered_dependents, block1, block2, block3))
  available <- setdiff(selected, assigned)
  bootstrap_choices <- bootstrap_resample_choices()

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    ordered_dependents = ordered_dependents,
    dependent_items = analysis_variable_items(ordered_dependents, variable_table, labels),
    block1 = block1,
    block1_items = analysis_variable_items(block1, variable_table, labels),
    block2 = block2,
    block2_items = analysis_variable_items(block2, variable_table, labels),
    block3 = block3,
    block3_items = analysis_variable_items(block3, variable_table, labels),
    bootstrap_choices = bootstrap_choices,
    current_bootstrap = normalized_bootstrap_resamples(bootstrap_value, bootstrap_choices),
    current_seed = seed_value %||% default_seed(),
    move_disabled = length(selected) == 0
  )
}

hierarchical_setup_panel_from_state <- function(setup, status_message) {
  hierarchical_setup_panel(setup, status_message)
}

hierarchical_target_panel <- function(title, input_id, items, size, move_up_id, move_down_id) {
  div(
    class = "analysis-transfer-column analysis-transfer-panel hierarchical-target-panel",
    div(class = "analysis-field-label", title),
    analysis_transfer_listbox_input(input_id, items = items, selected = utils::head(vapply(items, `[[`, character(1), "value"), 1), size = size),
    div(
      class = "hierarchical-order-actions",
      actionButton(move_up_id, "Up", class = "btn-default btn-sm"),
      actionButton(move_down_id, "Down", class = "btn-default btn-sm")
    )
  )
}

hierarchical_transfer_button_group <- function(add_id, remove_id, add_disabled, remove_disabled) {
  div(
    class = "hierarchical-transfer-button-group",
    actionButton(
      add_id,
      ">",
      class = "btn btn-default analysis-move-button",
      disabled = if (isTRUE(add_disabled)) "disabled" else NULL
    ),
    actionButton(
      remove_id,
      "<",
      class = "btn btn-default analysis-move-button",
      disabled = if (isTRUE(remove_disabled)) "disabled" else NULL
    )
  )
}

hierarchical_setup_panel <- function(setup, status_message) {
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "hierarchical-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel",
        div(class = "analysis-field-label", "Variables"),
        analysis_transfer_listbox_input(
          "hierarchical_available",
          items = setup$available_items,
          selected = utils::head(setup$available, 1),
          size = 26
        )
      ),
      div(
        class = "analysis-transfer-controls hierarchical-transfer-controls",
        hierarchical_transfer_button_group(
          "hierarchical_add_dependent",
          "hierarchical_remove_dependent",
          setup$move_disabled,
          length(setup$ordered_dependents) == 0
        ),
        hierarchical_transfer_button_group(
          "hierarchical_add_block1",
          "hierarchical_remove_block1",
          setup$move_disabled,
          length(setup$block1) == 0
        ),
        hierarchical_transfer_button_group(
          "hierarchical_add_block2",
          "hierarchical_remove_block2",
          setup$move_disabled,
          length(setup$block2) == 0
        ),
        hierarchical_transfer_button_group(
          "hierarchical_add_block3",
          "hierarchical_remove_block3",
          setup$move_disabled,
          length(setup$block3) == 0
        )
      ),
      div(
        class = "hierarchical-target-column",
        hierarchical_target_panel(
          "Dependent Variables",
          "hierarchical_y",
          setup$dependent_items,
          5,
          "move_hierarchical_dependent_up",
          "move_hierarchical_dependent_down"
        ),
        hierarchical_target_panel(
          "Block 1: Covariates",
          "hierarchical_block1",
          setup$block1_items,
          8,
          "move_hierarchical_block1_up",
          "move_hierarchical_block1_down"
        ),
        hierarchical_target_panel(
          "Block 2: Predictors",
          "hierarchical_block2",
          setup$block2_items,
          8,
          "move_hierarchical_block2_up",
          "move_hierarchical_block2_down"
        ),
        hierarchical_target_panel(
          "Block 3: Additional predictors",
          "hierarchical_block3",
          setup$block3_items,
          8,
          "move_hierarchical_block3_up",
          "move_hierarchical_block3_down"
        )
      ),
      div(
        class = "analysis-options-column analysis-options-panel hierarchical-options",
        div(
          class = "analysis-option-group",
          div(class = "analysis-option-title", "Bootstrap"),
          div(
            class = "regression-field",
            selectInput(
              "hierarchical_boot_r",
              "Number of bootstrap samples",
              choices = setup$bootstrap_choices,
              selected = setup$current_bootstrap,
              selectize = FALSE
            )
          ),
          div(
            class = "regression-field",
            numericInput("hierarchical_seed", "Seed number", value = setup$current_seed, min = 1, step = 1)
          )
        ),
        analysis_option_group(
          "Effect size",
          list(
            list(id = "hierarchical_show_sr2", label = "sr\u00B2", value = FALSE),
            list(id = "hierarchical_show_f2", label = "f\u00B2", value = FALSE)
          )
        ),
        analysis_option_group(
          "Collinearity diagnostics",
          list(
            list(id = "hierarchical_show_vif", label = "VIF", value = FALSE)
          )
        )
      )
    ),
    div(
      class = "analysis-action-row hierarchical-action-row",
      tags$button("Run hierarchical", type = "button", class = "btn btn-primary", disabled = "disabled")
    )
  )
}
