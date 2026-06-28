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

paired_variable_separator <- "\u001E"

paired_group_values <- function(groups) {
  groups <- groups %||% list()
  vapply(groups, function(group) paste(as.character(group), collapse = paired_variable_separator), character(1))
}

paired_group_from_values <- function(values) {
  values <- as.character(values %||% character(0))
  groups <- strsplit(values, paired_variable_separator, fixed = TRUE)
  groups[lengths(groups) >= 2L]
}

paired_keep_selected_order <- function(group, selected) {
  group <- as.character(group %||% character(0))
  selected <- as.character(selected %||% character(0))
  group[group %in% selected]
}

paired_transfer_selection_order <- function(values, order_values = NULL, selected = NULL) {
  values <- as.character(values %||% character(0))
  order_values <- as.character(order_values %||% character(0))
  selected <- as.character(selected %||% character(0))
  if (length(selected) > 0) {
    values <- values[values %in% selected]
    order_values <- order_values[order_values %in% selected]
  }
  ordered <- order_values[order_values %in% values]
  c(ordered, values[!values %in% ordered])
}

paired_group_items <- function(groups, variable_table = NULL, labels = character(0)) {
  groups <- groups %||% list()
  if (length(groups) == 0) return(list())
  measurements <- paired_measurement_lookup(variable_table)
  lapply(groups, function(group) {
    group <- as.character(group)
    group_labels <- vapply(group, paired_display_name, character(1), variable_info = variable_table, labels = labels)
    measurement <- named_value(measurements, group[[1]], "continuous")
    list(
      value = paired_group_values(list(group)),
      label = paste(group_labels, collapse = " - "),
      measurement = measurement
    )
  })
}

paired_setup_state <- function(
  selected_names,
  repeated_groups = list(),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_repeated = NULL,
  assumption_check = TRUE,
  bowker = TRUE,
  effect_size = TRUE,
  cohen_d = TRUE,
  mean_sd = FALSE,
  median_iqr = FALSE,
  adjustment = "bonferroni",
  time_labels = NULL,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  selected <- as.character(selected_names %||% character(0))
  repeated_groups <- lapply(repeated_groups %||% list(), paired_keep_selected_order, selected = selected)
  repeated_groups <- repeated_groups[lengths(repeated_groups) >= 2L]
  group_values <- paired_group_values(repeated_groups)
  grouped_variables <- unique(unlist(repeated_groups, use.names = FALSE))
  available <- setdiff(selected, grouped_variables)
  time_count <- if (length(repeated_groups) > 0) max(3L, max(lengths(repeated_groups))) else 3L
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
    repeated_items = paired_group_items(repeated_groups, variable_table, labels),
    repeated_selected = selected_order_items(selected_repeated, group_values),
    assumption_check = isTRUE(assumption_check),
    bowker = isTRUE(bowker),
    effect_size = isTRUE(effect_size),
    cohen_d = isTRUE(cohen_d),
    mean_sd = isTRUE(mean_sd),
    median_iqr = isTRUE(median_iqr),
    adjustment = if (identical(adjustment, "bonferroni")) "bonferroni" else "holm",
    time_labels = time_labels,
    has_two = any(lengths(repeated_groups) == 2L),
    has_three_plus = any(lengths(repeated_groups) >= 3L),
    move_disabled = length(selected) == 0,
    language = language
  )
}

paired_time_label_inputs <- function(time_labels, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  labels <- as.character(time_labels %||% paired_rm_time_header_labels(3L))
  div(
    class = "paired-rm-time-labels",
    div(class = "analysis-option-title", analysis_ui_text("Repeated variable labels", language)),
    lapply(seq_along(labels), function(index) {
      textInput(
        inputId = paste0("paired_time_label_", index),
        label = paste0(index, if (index == 1L) "st" else if (index == 2L) "nd" else if (index == 3L) "rd" else "th"),
        value = labels[[index]],
        width = "100%"
      )
    })
  )
}

paired_setup_panel <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  primary_options <- tagList(
    analysis_option_group(
      "Assumption",
      list(list(id = "paired_assumption_check", label = "Check assumptions", value = state$assumption_check)),
      language = language
    ),
    analysis_option_group(
      "Categorical",
      list(list(id = "paired_bowker", label = "Bowker symmetry test", value = state$bowker)),
      language = language
    ),
    analysis_option_group(
      "Effect size",
      list(
        list(id = "paired_effect_size", label = "Effect size", value = state$effect_size),
        list(id = "paired_cohen_d", label = "Cohen's d for paired t-test", value = state$cohen_d)
      ),
      language = language
    ),
    analysis_option_group(
      "Summary",
      list(
        list(id = "paired_mean_sd", label = "M \u00B1 SD", value = state$mean_sd),
        list(id = "paired_median_iqr", label = "Median(Q1~Q3)", value = state$median_iqr)
      ),
      language = language
    )
  )
  repeated_options <- tagList(
    div(
      class = "analysis-option-group analysis-radio-group paired-posthoc-group",
      div(class = "analysis-option-title", analysis_ui_text("Post-hoc correction", language)),
      radioButtons(
        "paired_adjustment",
        label = NULL,
        choices = c("Bonferroni correction" = "bonferroni", "Holm Bonferroni" = "holm"),
        selected = state$adjustment
      )
    ),
    paired_time_label_inputs(state$time_labels, language)
  )
  repeated_tab_title <- if (isTRUE(state$has_three_plus)) {
    analysis_ui_text("Repeated", language)
  } else {
    tags$span(class = "paired-options-disabled-tab", analysis_ui_text("Repeated", language))
  }
  repeated_tab_content <- if (isTRUE(state$has_three_plus)) {
    repeated_options
  } else {
    div(class = "paired-options-disabled-content")
  }
  options_content <- analysis_options_tabs_panel(
    id = "paired_options_tabs",
    class = "ttest-anova-options paired-options",
    tabPanel(
      analysis_ui_text("Options", language),
      value = "Options",
      div(class = "factor-options-tab-content ttest-anova-options-tab-content paired-options-tab-content", primary_options)
    ),
    tabPanel(
      repeated_tab_title,
      value = "Repeated",
      div(class = "factor-options-tab-content ttest-anova-options-tab-content paired-options-tab-content", repeated_tab_content)
    )
  )
  div(
    class = "ttest-anova-setup-grid paired-setup-grid paired-rm-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input("paired_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls paired-transfer-controls",
      actionButton("paired_pair_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && length(state$repeated_groups) == 0) "disabled" else NULL)
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel paired-target-panel paired-rm-target-panel",
      analysis_field_label_tag("Repeated-measures variables", c("binary", "category", "ordered", "continuous"), language = language),
      analysis_transfer_listbox_input("paired_pairs", state$repeated_items, selected = state$repeated_selected, size = 16, important_height = TRUE),
      div(class = "analysis-order-actions paired-order-actions", actionButton("paired_pair_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"), actionButton("paired_pair_down", analysis_ui_text("Down", language), class = "btn-default btn-sm"))
    ),
    div(
      class = "ttest-anova-options-column",
      options_content
    )
  )
}
