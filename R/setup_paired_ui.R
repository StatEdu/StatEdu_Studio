# Paired test setup UI.

paired_pair_separator <- "\u001F"

paired_pair_values <- function(first, second) {
  paste(as.character(first %||% character(0)), as.character(second %||% character(0)), sep = paired_pair_separator)
}

paired_pair_from_values <- function(values) {
  values <- as.character(values %||% character(0))
  parts <- strsplit(values, paired_pair_separator, fixed = TRUE)
  valid <- lengths(parts) == 2L
  if (!any(valid)) {
    return(data.frame(first = character(0), second = character(0), stringsAsFactors = FALSE))
  }
  data.frame(
    first = vapply(parts[valid], `[[`, character(1), 1L),
    second = vapply(parts[valid], `[[`, character(1), 2L),
    stringsAsFactors = FALSE
  )
}

paired_pair_items <- function(pairs, variable_table = NULL, labels = character(0)) {
  pairs <- pairs %||% data.frame(first = character(0), second = character(0), stringsAsFactors = FALSE)
  if (nrow(pairs) == 0) return(list())
  measurements <- paired_measurement_lookup(variable_table)
  lapply(seq_len(nrow(pairs)), function(index) {
    first <- as.character(pairs$first[[index]])
    second <- as.character(pairs$second[[index]])
    first_label <- paired_display_name(first, variable_table, labels)
    second_label <- paired_display_name(second, variable_table, labels)
    measurement <- named_value(measurements, first, "continuous")
    list(
      value = paired_pair_values(first, second),
      label = sprintf("%s - %s", first_label, second_label),
      measurement = measurement
    )
  })
}

paired_setup_state <- function(
  selected_names,
  paired_pairs = data.frame(first = character(0), second = character(0), stringsAsFactors = FALSE),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_pairs = NULL,
  assumption_check = FALSE,
  bowker = FALSE,
  effect_size = FALSE,
  cohen_d = FALSE
) {
  selected <- as.character(selected_names %||% character(0))
  paired_pairs <- paired_pairs %||% data.frame(first = character(0), second = character(0), stringsAsFactors = FALSE)
  if (!all(c("first", "second") %in% names(paired_pairs))) {
    paired_pairs <- data.frame(first = character(0), second = character(0), stringsAsFactors = FALSE)
  }
  paired_pairs$first <- as.character(paired_pairs$first)
  paired_pairs$second <- as.character(paired_pairs$second)
  paired_pairs <- paired_pairs[paired_pairs$first %in% selected & paired_pairs$second %in% selected, , drop = FALSE]
  pair_values <- paired_pair_values(paired_pairs$first, paired_pairs$second)
  available <- setdiff(selected, unique(c(paired_pairs$first, paired_pairs$second)))
  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    paired_pairs = paired_pairs,
    pair_values = pair_values,
    pair_items = paired_pair_items(paired_pairs, variable_table, labels),
    pair_selected = selected_order_items(selected_pairs, pair_values),
    assumption_check = isTRUE(assumption_check),
    bowker = isTRUE(bowker),
    effect_size = isTRUE(effect_size),
    cohen_d = isTRUE(cohen_d),
    move_disabled = length(selected) == 0
  )
}

paired_setup_panel <- function(state) {
  div(
    class = "ttest-anova-setup-grid paired-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("paired_available", state$available_items, selected = state$available_selected, size = 20)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls paired-transfer-controls",
      actionButton("paired_pair_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && nrow(state$paired_pairs) == 0) "disabled" else NULL)
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel paired-target-panel",
      analysis_field_label_tag("Paired variables", c("binary", "category", "ordered", "continuous")),
      analysis_transfer_listbox_input("paired_pairs", state$pair_items, selected = state$pair_selected, size = 18),
      div(class = "analysis-order-actions paired-order-actions", actionButton("paired_pair_up", "Up", class = "btn-default btn-sm"), actionButton("paired_pair_down", "Down", class = "btn-default btn-sm"))
    ),
    div(
      class = "ttest-anova-options-column",
      div(
        class = "analysis-options-panel ttest-anova-options paired-options",
        analysis_option_group(
          "Assumption",
          list(list(id = "paired_assumption_check", label = "Check assumptions", value = state$assumption_check))
        ),
        analysis_option_group(
          "Categorical",
          list(list(id = "paired_bowker", label = "Bowker symmetry test", value = state$bowker))
        ),
        analysis_option_group(
          "Effect size",
          list(
            list(id = "paired_effect_size", label = "Effect size", value = state$effect_size),
            list(id = "paired_cohen_d", label = "Cohen's d for paired t-test", value = state$cohen_d)
          )
        )
      )
    )
  )
}
