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
  seed_value = NULL,
  selected_available = NULL,
  selected_dependent = NULL,
  selected_block1 = NULL,
  selected_block2 = NULL,
  selected_block3 = NULL,
  active_block = "block1",
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE
) {
  selected <- as.character(selected_names %||% character(0))
  ordered_dependents <- intersect(as.character(ordered_dependents %||% character(0)), selected)
  block1 <- intersect(as.character(block1 %||% character(0)), selected)
  block2 <- intersect(as.character(block2 %||% character(0)), selected)
  block3 <- intersect(as.character(block3 %||% character(0)), selected)
  assigned <- unique(c(ordered_dependents, block1, block2, block3))
  available <- setdiff(selected, assigned)
  bootstrap_choices <- bootstrap_resample_choices()
  available_selected <- selected_order_items(selected_available, available)
  dependent_selected <- selected_order_items(selected_dependent, ordered_dependents)
  block1_selected <- selected_order_items(selected_block1, block1)
  block2_selected <- selected_order_items(selected_block2, block2)
  block3_selected <- selected_order_items(selected_block3, block3)
  active_block <- as.character(active_block %||% "block1")[[1]]
  if (!active_block %in% c("block1", "block2", "block3")) {
    active_block <- "block1"
  }

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = available_selected,
    ordered_dependents = ordered_dependents,
    dependent_items = analysis_variable_items(ordered_dependents, variable_table, labels),
    dependent_selected = dependent_selected,
    block1 = block1,
    block1_items = analysis_variable_items(block1, variable_table, labels),
    block1_selected = block1_selected,
    block2 = block2,
    block2_items = analysis_variable_items(block2, variable_table, labels),
    block2_selected = block2_selected,
    block3 = block3,
    block3_items = analysis_variable_items(block3, variable_table, labels),
    block3_selected = block3_selected,
    active_block = active_block,
    bootstrap_choices = bootstrap_choices,
    current_bootstrap = normalized_bootstrap_resamples(bootstrap_value, bootstrap_choices),
    current_seed = seed_value %||% default_seed(),
    show_sr2 = isTRUE(show_sr2),
    show_f2 = isTRUE(show_f2),
    show_vif = isTRUE(show_vif),
    move_disabled = length(selected) == 0
  )
}

hierarchical_setup_panel_from_state <- function(setup, status_message) {
  hierarchical_setup_panel(setup, status_message)
}

hierarchical_active_block_setup <- function(setup) {
  block <- setup$active_block %||% "block1"
  switch(
    block,
    block2 = list(
      index = 2L,
      name = "block2",
      title = sprintf("Block 2: Predictors (%s)", length(setup$block2)),
      input_id = "hierarchical_block2",
      items = setup$block2_items,
      selected = setup$block2_selected,
      move_id = "hierarchical_block2_move",
      move_up_id = "move_hierarchical_block2_up",
      move_down_id = "move_hierarchical_block2_down"
    ),
    block3 = list(
      index = 3L,
      name = "block3",
      title = sprintf("Block 3: Predictors (%s)", length(setup$block3)),
      input_id = "hierarchical_block3",
      items = setup$block3_items,
      selected = setup$block3_selected,
      move_id = "hierarchical_block3_move",
      move_up_id = "move_hierarchical_block3_up",
      move_down_id = "move_hierarchical_block3_down"
    ),
    list(
      index = 1L,
      name = "block1",
      title = sprintf("Block 1: Covariates (%s)", length(setup$block1)),
      input_id = "hierarchical_block1",
      items = setup$block1_items,
      selected = setup$block1_selected,
      move_id = "hierarchical_block1_move",
      move_up_id = "move_hierarchical_block1_up",
      move_down_id = "move_hierarchical_block1_down"
    )
  )
}

hierarchical_block_title_tag <- function(block) {
  div(
    class = "hierarchical-block-title-row",
    div(
      class = "analysis-field-label analysis-field-label-with-icons hierarchical-block-title",
      span(block$title),
      span(
        class = "analysis-allowed-measurements",
        lapply(analysis_allowed_measurements_all(), measurement_symbol_tag)
      )
    ),
    div(
      class = "hierarchical-block-nav",
      if (block$index > 1L) {
        actionButton("hierarchical_block_prev", "\u2039", class = "btn-default btn-sm hierarchical-block-nav-button", title = "Previous block")
      },
      if (block$index < 3L) {
        actionButton("hierarchical_block_next", "\u203a", class = "btn-default btn-sm hierarchical-block-nav-button", title = "Next block")
      }
    )
  )
}

hierarchical_target_panel <- function(
  title,
  input_id,
  items,
  selected,
  size,
  move_up_id,
  move_down_id,
  allowed_measurements = NULL
) {
  panel_class <- switch(
    input_id,
    hierarchical_y = "hierarchical-dependent-panel",
    hierarchical_block1 = "hierarchical-block1-panel",
    hierarchical_block2 = "hierarchical-block2-panel",
    hierarchical_block3 = "hierarchical-block3-panel",
    ""
  )
  size_class <- paste0("hierarchical-list-size-", max(1, as.integer(size %||% 1)))

  div(
    class = paste("analysis-transfer-column analysis-transfer-panel hierarchical-target-panel", panel_class, size_class),
    if (inherits(title, "shiny.tag") || inherits(title, "shiny.tag.list")) title else analysis_field_label_tag(title, allowed_measurements),
    analysis_transfer_listbox_input(input_id, items = items, selected = selected, size = size),
    div(
      class = "hierarchical-order-actions",
      actionButton(move_up_id, "Up", class = "btn-default btn-sm"),
      actionButton(move_down_id, "Down", class = "btn-default btn-sm")
    )
  )
}

hierarchical_setup_panel <- function(setup, status_message) {
  can_run <- length(setup$ordered_dependents) > 0 && length(setup$block1) > 0
  active_block <- hierarchical_active_block_setup(setup)
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "hierarchical-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel",
        analysis_field_label_tag("Variables"),
        analysis_transfer_listbox_input(
          "hierarchical_available",
          items = setup$available_items,
          selected = setup$available_selected,
          size = 19
        )
      ),
      div(
        class = "hierarchical-target-stack hierarchical-target-stack-compact",
        div(
          class = "hierarchical-target-row hierarchical-dependent-row",
          div(
            class = "hierarchical-target-move-cell",
            actionButton(
              "hierarchical_dependent_move",
              ">",
              class = "btn btn-default analysis-move-button",
              disabled = if (setup$move_disabled && length(setup$ordered_dependents) == 0) "disabled" else NULL
            )
          ),
          hierarchical_target_panel(
            sprintf("Dependent Variables (%s)", length(setup$ordered_dependents)),
            "hierarchical_y",
            setup$dependent_items,
            setup$dependent_selected,
            4,
            "move_hierarchical_dependent_up",
            "move_hierarchical_dependent_down",
            "continuous"
          )
        ),
        div(
          class = paste("hierarchical-target-row hierarchical-active-block-row", paste0("hierarchical-active-", active_block$name)),
          div(
            class = "hierarchical-target-move-cell",
            actionButton(
              active_block$move_id,
              ">",
              class = "btn btn-default analysis-move-button",
              disabled = if (setup$move_disabled && length(setup[[active_block$name]]) == 0) "disabled" else NULL
            )
          ),
          hierarchical_target_panel(
            hierarchical_block_title_tag(active_block),
            active_block$input_id,
            active_block$items,
            active_block$selected,
            8,
            active_block$move_up_id,
            active_block$move_down_id,
            analysis_allowed_measurements_all()
          )
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
            list(id = "hierarchical_show_sr2", label = "sr\u00B2", value = isTRUE(setup$show_sr2)),
            list(id = "hierarchical_show_f2", label = "f\u00B2", value = isTRUE(setup$show_f2))
          )
        ),
        analysis_option_group(
          "Collinearity diagnostics",
          list(
            list(id = "hierarchical_show_vif", label = "VIF", value = isTRUE(setup$show_vif))
          )
        )
      )
    ),
    div(
      class = "analysis-action-row hierarchical-action-row",
      actionButton(
        "run_hierarchical",
        "Run hierarchical",
        class = "btn-primary",
        disabled = if (!can_run) "disabled" else NULL
      ),
      uiOutput("hierarchical_save_control")
    )
  )
}
