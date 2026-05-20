# Reliability setup UI state and panel.

reliability_setup_state <- function(
  selected_names,
  variable_table = NULL,
  labels = character(0),
  selected_variables = character(0),
  factor_blocks = NULL,
  active_factor = 1L,
  selected_available = character(0),
  selected_selected = character(0),
  normality = TRUE,
  ordinal = TRUE,
  subfactor_enabled = FALSE,
  reliability_if_deleted = TRUE,
  item_total_correlation = TRUE
) {
  selected <- as.character(selected_names %||% character(0))
  if (is.null(factor_blocks)) {
    factor_blocks <- list(as.character(selected_variables %||% character(0)))
  }
  factor_blocks <- lapply(factor_blocks %||% list(character(0)), function(block) intersect(as.character(block), selected))
  if (length(factor_blocks) == 0) factor_blocks <- list(character(0))
  active_factor <- as.integer(active_factor %||% 1L)
  if (!is.finite(active_factor) || active_factor < 1L) active_factor <- 1L
  if (active_factor > length(factor_blocks)) active_factor <- length(factor_blocks)
  assigned <- unique(unlist(factor_blocks, use.names = FALSE))
  active_variables <- factor_blocks[[active_factor]]
  available <- setdiff(selected, assigned)
  list(
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    selected_items = analysis_variable_items(active_variables, variable_table, labels),
    selected_selected = selected_order_items(selected_selected, active_variables),
    factor_blocks = factor_blocks,
    active_factor = active_factor,
    subfactor_enabled = isTRUE(subfactor_enabled),
    normality = isTRUE(normality),
    ordinal = isTRUE(ordinal),
    reliability_if_deleted = isTRUE(reliability_if_deleted),
    item_total_correlation = isTRUE(item_total_correlation)
  )
}

reliability_factor_title_tag <- function(state) {
  div(
    class = "reliability-factor-title-row",
    analysis_field_label_tag(
      if (isTRUE(state$subfactor_enabled)) {
        sprintf("Items - Subfactor %s", state$active_factor)
      } else {
        "Items"
      },
      c("binary", "ordered", "continuous")
    ),
    checkboxInput("reliability_subfactor_enabled", "Subfactor", value = state$subfactor_enabled),
    div(
      class = "reliability-factor-nav",
      if (isTRUE(state$subfactor_enabled) && state$active_factor > 1L) {
        actionButton("reliability_factor_prev", "Previous subfactor", class = "btn-default btn-sm reliability-factor-nav-button")
      },
      if (isTRUE(state$subfactor_enabled)) {
        actionButton("reliability_factor_next", "Next subfactor", class = "btn-default btn-sm reliability-factor-nav-button")
      }
    )
  )
}

reliability_setup_panel <- function(state) {
  div(
    class = "frequencies-setup-grid reliability-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("reliability_available", state$available_items, selected = state$available_selected, size = 19)
    ),
    div(
      class = "analysis-transfer-controls",
      actionButton("reliability_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = paste(
        "analysis-transfer-column analysis-transfer-panel reliability-target-panel",
        if (isTRUE(state$subfactor_enabled)) "reliability-subfactor-on" else "reliability-subfactor-off"
      ),
      reliability_factor_title_tag(state),
      analysis_transfer_listbox_input(
        "reliability_selected",
        state$selected_items,
        selected = state$selected_selected,
        size = if (isTRUE(state$subfactor_enabled)) 17 else 18,
        important_height = TRUE,
        height_offset = if (isTRUE(state$subfactor_enabled)) -7 else 0
      ),
      div(
        class = "analysis-order-actions reliability-order-actions",
        actionButton("reliability_move_up", "Up", class = "btn-default btn-sm"),
        actionButton("reliability_move_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel",
      analysis_option_group(
        "Method",
        list(
          list(id = "reliability_normality", label = "Normality", value = state$normality),
          list(id = "reliability_ordinal", label = "Ordinal alpha / Ordinal omega", value = state$ordinal)
        )
      ),
      analysis_option_group(
        "Item diagnostics",
        list(
          list(id = "reliability_if_deleted", label = "Reliability if item deleted", value = state$reliability_if_deleted),
          list(id = "reliability_item_total_correlation", label = "Item-total correlation", value = state$item_total_correlation)
        )
      )
    )
  )
}
