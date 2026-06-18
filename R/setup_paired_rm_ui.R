# Paired test (three or more repeated measurements) setup UI.

paired_rm_group_separator <- "\u001F"
paired_rm_variable_separator <- "\u001E"

paired_rm_group_values <- function(groups) {
  groups <- groups %||% list()
  vapply(groups, function(group) paste(as.character(group), collapse = paired_rm_variable_separator), character(1))
}

paired_rm_group_from_values <- function(values) {
  values <- as.character(values %||% character(0))
  groups <- strsplit(values, paired_rm_variable_separator, fixed = TRUE)
  groups[lengths(groups) >= 3L]
}

paired_rm_group_items <- function(groups, variable_table = NULL, labels = character(0)) {
  groups <- groups %||% list()
  if (length(groups) == 0) return(list())
  measurements <- paired_measurement_lookup(variable_table)
  lapply(groups, function(group) {
    group <- as.character(group)
    group_labels <- vapply(group, paired_display_name, character(1), variable_info = variable_table, labels = labels)
    measurement <- named_value(measurements, group[[1]], "continuous")
    list(
      value = paired_rm_group_values(list(group)),
      label = paste(group_labels, collapse = " - "),
      measurement = measurement
    )
  })
}

paired_rm_setup_state <- function(
  selected_names,
  repeated_groups = list(),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_repeated = NULL,
  assumption_check = FALSE,
  adjustment = "holm",
  time_labels = NULL
) {
  selected <- as.character(selected_names %||% character(0))
  repeated_groups <- lapply(repeated_groups %||% list(), paired_keep_selected_order, selected = selected)
  repeated_groups <- repeated_groups[lengths(repeated_groups) >= 3L]
  group_values <- paired_rm_group_values(repeated_groups)
  grouped_variables <- unique(unlist(repeated_groups, use.names = FALSE))
  available <- setdiff(selected, grouped_variables)
  time_count <- if (length(repeated_groups) > 0) max(lengths(repeated_groups)) else 3L
  default_time_labels <- paired_rm_time_header_labels(time_count)
  time_labels <- as.character(time_labels %||% default_time_labels)
  time_labels <- trimws(time_labels)
  if (length(time_labels) < time_count) {
    time_labels <- c(time_labels, default_time_labels[seq.int(length(time_labels) + 1L, time_count)])
  }
  time_labels <- ifelse(nzchar(time_labels[seq_len(time_count)]), time_labels[seq_len(time_count)], default_time_labels)
  list(
    available = available,
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    repeated_groups = repeated_groups,
    group_values = group_values,
    repeated_items = paired_rm_group_items(repeated_groups, variable_table, labels),
    repeated_selected = selected_order_items(selected_repeated, group_values),
    assumption_check = isTRUE(assumption_check),
    adjustment = if (identical(adjustment, "bonferroni")) "bonferroni" else "holm",
    time_labels = time_labels,
    move_disabled = length(selected) == 0
  )
}

paired_rm_time_label_inputs <- function(time_labels) {
  labels <- as.character(time_labels %||% paired_rm_time_header_labels(3L))
  div(
    class = "paired-rm-time-labels",
    div(class = "analysis-option-title", "Repeated variable labels"),
    lapply(seq_along(labels), function(index) {
      textInput(
        inputId = paste0("paired_rm_time_label_", index),
        label = paste0(index, if (index == 1L) "st" else if (index == 2L) "nd" else if (index == 3L) "rd" else "th"),
        value = labels[[index]],
        width = "100%"
      )
    })
  )
}

paired_rm_setup_panel <- function(state) {
  div(
    class = "ttest-anova-setup-grid paired-setup-grid paired-rm-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("paired_rm_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls paired-transfer-controls",
      actionButton("paired_rm_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && length(state$repeated_groups) == 0) "disabled" else NULL)
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel paired-target-panel paired-rm-target-panel",
      analysis_field_label_tag("Repeated-measures variables", c("binary", "ordered", "continuous")),
      analysis_transfer_listbox_input("paired_rm_repeated", state$repeated_items, selected = state$repeated_selected, size = 17),
      div(class = "analysis-order-actions paired-order-actions", actionButton("paired_rm_up", "Up", class = "btn-default btn-sm"), actionButton("paired_rm_down", "Down", class = "btn-default btn-sm"))
    ),
    div(
      class = "ttest-anova-options-column",
      div(
        class = "analysis-options-panel ttest-anova-options paired-options",
        analysis_option_group(
          "Assumption",
          list(list(id = "paired_rm_assumption_check", label = "Check assumptions", value = state$assumption_check))
        ),
        analysis_radio_group(
          "Post-hoc correction",
          "paired_rm_adjustment",
          choices = c("Holm Bonferroni" = "holm", "Bonferroni correction" = "bonferroni"),
          selected = state$adjustment
        ),
        paired_rm_time_label_inputs(state$time_labels)
      )
    )
  )
}
