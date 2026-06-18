# Logistic regression setup UI state and panel.

logistic_dependent_measurements <- function() {
  c("binary", "ordered", "category")
}

logistic_predictor_measurements <- function() {
  analysis_allowed_measurements_all()
}

logistic_variable_measurements <- function(variable_table = NULL) {
  if (!is.data.frame(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(character(0))
  }
  values <- tolower(as.character(variable_table$measurement))
  values[values == "ordinal"] <- "ordered"
  values[values == "nominal"] <- "category"
  stats::setNames(values, as.character(variable_table$name))
}

logistic_dependent_candidates <- function(selected_names, variable_table = NULL) {
  selected <- as.character(selected_names %||% character(0))
  measurements <- logistic_variable_measurements(variable_table)
  selected[vapply(selected, function(name) named_value(measurements, name, "") %in% logistic_dependent_measurements(), logical(1))]
}

logistic_model_family_label <- function(dependents, variable_table = NULL) {
  dependents <- as.character(dependents %||% character(0))
  dependents <- dependents[nzchar(dependents)]
  measurements <- logistic_variable_measurements(variable_table)
  dependent_measurements <- unique(vapply(dependents, function(name) named_value(measurements, name, ""), character(1)))
  dependent_measurements <- dependent_measurements[nzchar(dependent_measurements)]
  if (length(dependent_measurements) == 0) {
    return("Select a binary, ordered, or categorical dependent variable.")
  }
  labels <- vapply(dependent_measurements, function(measurement) {
    switch(
      measurement,
      binary = "Binary logistic regression",
      ordered = "Ordinal logistic regression",
      category = "Multinomial logistic regression",
      ""
    )
  }, character(1))
  paste(labels[nzchar(labels)], collapse = "; ")
}

logistic_setup_state <- function(
  selected_names,
  dependents,
  block1,
  block2,
  block3,
  variable_table,
  labels = character(0),
  selected_available = NULL,
  selected_dependent = NULL,
  selected_block1 = NULL,
  selected_block2 = NULL,
  selected_block3 = NULL,
  active_block = "block1",
  show_b = FALSE,
  show_se = FALSE,
  show_mcfadden = TRUE,
  show_cox_snell = TRUE,
  show_b_se = NULL,
  show_extra_r2 = TRUE,
  split_ci = FALSE
) {
  selected <- as.character(selected_names %||% character(0))
  dependents <- intersect(as.character(dependents %||% character(0)), selected)
  block1 <- intersect(as.character(block1 %||% character(0)), selected)
  block2 <- intersect(as.character(block2 %||% character(0)), selected)
  block3 <- intersect(as.character(block3 %||% character(0)), selected)
  assigned <- unique(c(dependents, block1, block2, block3))
  available <- setdiff(selected, assigned)
  active_block <- as.character(active_block %||% "block1")[[1]]
  if (!active_block %in% c("block1", "block2", "block3")) {
    active_block <- "block1"
  }

  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    dependents = dependents,
    dependent_items = analysis_variable_items(dependents, variable_table, labels),
    dependent_selected = selected_order_items(selected_dependent, dependents),
    block1 = block1,
    block1_items = analysis_variable_items(block1, variable_table, labels),
    block1_selected = selected_order_items(selected_block1, block1),
    block2 = block2,
    block2_items = analysis_variable_items(block2, variable_table, labels),
    block2_selected = selected_order_items(selected_block2, block2),
    block3 = block3,
    block3_items = analysis_variable_items(block3, variable_table, labels),
    block3_selected = selected_order_items(selected_block3, block3),
    active_block = active_block,
    model_family = logistic_model_family_label(dependents, variable_table),
    show_b_se = if (is.null(show_b_se)) isTRUE(show_b) || isTRUE(show_se) else isTRUE(show_b_se),
    show_extra_r2 = if (is.null(show_extra_r2)) isTRUE(show_mcfadden) || isTRUE(show_cox_snell) else isTRUE(show_extra_r2),
    split_ci = isTRUE(split_ci),
    can_run = length(dependents) > 0 && length(unique(c(block1, block2, block3))) > 0,
    move_disabled = length(selected) == 0
  )
}

logistic_active_block_setup <- function(setup) {
  block <- setup$active_block %||% "block1"
  switch(
    block,
    block2 = list(
      index = 2L,
      name = "block2",
      title = sprintf("Block 2: Independent variables (%s)", length(setup$block2)),
      input_id = "logistic_block2",
      items = setup$block2_items,
      selected = setup$block2_selected,
      move_id = "logistic_block2_move",
      move_up_id = "move_logistic_block2_up",
      move_down_id = "move_logistic_block2_down"
    ),
    block3 = list(
      index = 3L,
      name = "block3",
      title = sprintf("Block 3: Independent variables (%s)", length(setup$block3)),
      input_id = "logistic_block3",
      items = setup$block3_items,
      selected = setup$block3_selected,
      move_id = "logistic_block3_move",
      move_up_id = "move_logistic_block3_up",
      move_down_id = "move_logistic_block3_down"
    ),
    list(
      index = 1L,
      name = "block1",
      title = sprintf("Block 1 (%s)", length(setup$block1)),
      input_id = "logistic_block1",
      items = setup$block1_items,
      selected = setup$block1_selected,
      move_id = "logistic_block1_move",
      move_up_id = "move_logistic_block1_up",
      move_down_id = "move_logistic_block1_down"
    )
  )
}

logistic_block_title_tag <- function(block, can_next = FALSE) {
  div(
    class = "hierarchical-block-title-row logistic-block-title-row",
    div(
      class = "analysis-field-label analysis-field-label-with-icons hierarchical-block-title",
      span(block$title),
      span(
        class = "analysis-allowed-measurements",
        lapply(logistic_predictor_measurements(), measurement_symbol_tag)
      )
    ),
    div(
      class = "hierarchical-block-nav logistic-block-nav",
      if (block$index > 1L) {
        actionButton("logistic_block_prev", "\u2039", class = "btn-default btn-sm hierarchical-block-nav-button", title = "Previous block")
      },
      if (block$index < 3L && isTRUE(can_next)) {
        actionButton("logistic_block_next", "\u203a", class = "btn-default btn-sm hierarchical-block-nav-button", title = "Next block")
      }
    )
  )
}

logistic_target_panel <- function(title, input_id, items, selected, size, move_up_id, move_down_id, allowed_measurements = NULL) {
  panel_class <- switch(
    input_id,
    logistic_y = "hierarchical-dependent-panel logistic-dependent-panel",
    logistic_block1 = "hierarchical-block1-panel logistic-block1-panel",
    logistic_block2 = "hierarchical-block2-panel logistic-block2-panel",
    logistic_block3 = "hierarchical-block3-panel logistic-block3-panel",
    ""
  )
  size_class <- paste0("hierarchical-list-size-", max(1, as.integer(size %||% 1)))
  div(
    class = paste("analysis-transfer-column analysis-transfer-panel hierarchical-target-panel logistic-target-panel", panel_class, size_class),
    if (inherits(title, "shiny.tag") || inherits(title, "shiny.tag.list")) title else analysis_field_label_tag(title, allowed_measurements),
    analysis_transfer_listbox_input(input_id, items = items, selected = selected, size = size),
    div(
      class = "hierarchical-order-actions logistic-order-actions",
      actionButton(move_up_id, "Up", class = "btn-default btn-sm"),
      actionButton(move_down_id, "Down", class = "btn-default btn-sm")
    )
  )
}

logistic_setup_panel <- function(setup, status_message = NULL) {
  active_block <- logistic_active_block_setup(setup)
  tagList(
    if (!is.null(status_message)) {
      div(status_message, class = "regression-warning")
    },
    div(
      class = "hierarchical-setup-grid logistic-setup-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel logistic-available-panel",
        analysis_field_label_tag("Variables"),
        analysis_transfer_listbox_input("logistic_available", items = setup$available_items, selected = setup$available_selected, size = 17)
      ),
      div(
        class = "hierarchical-target-stack hierarchical-target-stack-compact logistic-target-stack",
        div(
          class = "hierarchical-target-row hierarchical-dependent-row logistic-dependent-row",
          div(
            class = "hierarchical-target-move-cell",
            actionButton(
              "logistic_dependent_move",
              ">",
              class = "btn btn-default analysis-move-button",
              disabled = if (setup$move_disabled && length(setup$dependents) == 0) "disabled" else NULL
            )
          ),
          logistic_target_panel(
            sprintf("Dependent variables (%s)", length(setup$dependents)),
            "logistic_y",
            setup$dependent_items,
            setup$dependent_selected,
            3,
            "move_logistic_dependent_up",
            "move_logistic_dependent_down",
            logistic_dependent_measurements()
          )
        ),
        div(
          class = paste("hierarchical-target-row hierarchical-active-block-row logistic-active-block-row", paste0("logistic-active-", active_block$name)),
          div(
            class = "hierarchical-target-move-cell",
            actionButton(
              active_block$move_id,
              ">",
              class = "btn btn-default analysis-move-button",
              disabled = if (setup$move_disabled && length(setup[[active_block$name]]) == 0) "disabled" else NULL
            )
          ),
          logistic_target_panel(
            logistic_block_title_tag(active_block, can_next = active_block$index == 1L || length(setup[[active_block$name]]) > 0),
            active_block$input_id,
            active_block$items,
            active_block$selected,
            7,
            active_block$move_up_id,
            active_block$move_down_id,
            logistic_predictor_measurements()
          )
        )
      ),
      div(
        class = "analysis-options-column analysis-options-panel hierarchical-options logistic-options",
        div(
          class = "analysis-option-group",
          div(class = "analysis-option-title", "Output"),
          checkboxInput("logistic_show_b_se", "B, SE", value = isTRUE(setup$show_b_se)),
          checkboxInput("logistic_show_extra_r2", "McFadden R\u00b2, Cox & Snell R\u00b2", value = isTRUE(setup$show_extra_r2)),
          checkboxInput("logistic_split_ci", "95% CI (LLCI, ULCI)", value = isTRUE(setup$split_ci))
        )
      )
    ),
    div(
      class = "analysis-action-row hierarchical-action-row logistic-action-row",
      actionButton("run_logistic", "Run logistic", class = "btn btn-primary", disabled = if (!isTRUE(setup$can_run)) "disabled" else NULL),
      tags$button(
        id = "reset_logistic_block2",
        type = "button",
        class = "btn action-button btn-default analysis-reset-button logistic-reset-button",
        disabled = if (length(unique(c(setup$dependents, setup$block1, setup$block2, setup$block3))) == 0) "disabled" else NULL,
        "Reset setting"
      ),
      uiOutput("logistic_save_control")
    )
  )
}
