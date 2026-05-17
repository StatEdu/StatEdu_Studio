# Reliability setup UI state and panel.

reliability_setup_state <- function(
  selected_names,
  variable_table = NULL,
  labels = character(0),
  selected_variables = character(0),
  selected_available = character(0),
  selected_selected = character(0),
  normality = FALSE,
  ordinal = FALSE,
  reliability_if_deleted = FALSE,
  item_total_correlation = FALSE
) {
  selected <- as.character(selected_names %||% character(0))
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), selected)
  available <- setdiff(selected, selected_variables)
  list(
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    selected_items = analysis_variable_items(selected_variables, variable_table, labels),
    selected_selected = selected_order_items(selected_selected, selected_variables),
    normality = isTRUE(normality),
    ordinal = isTRUE(ordinal),
    reliability_if_deleted = isTRUE(reliability_if_deleted),
    item_total_correlation = isTRUE(item_total_correlation)
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
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Items", c("binary", "ordered", "continuous")),
      analysis_transfer_listbox_input("reliability_selected", state$selected_items, selected = state$selected_selected, size = 19),
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
