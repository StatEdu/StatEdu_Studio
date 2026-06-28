# Nonparametric paired test setup UI.

nonparametric_paired_setup_state <- function(
  selected_names,
  repeated_groups = list(),
  variable_table = NULL,
  labels = character(0),
  selected_available = NULL,
  selected_repeated = NULL,
  effect_size = TRUE,
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
  time_labels <- trimws(as.character(time_labels %||% default_time_labels))
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
    effect_size = isTRUE(effect_size),
    median_iqr = isTRUE(median_iqr),
    adjustment = if (identical(adjustment, "bonferroni")) "bonferroni" else "holm",
    time_labels = time_labels,
    has_three_plus = any(lengths(repeated_groups) >= 3L),
    move_disabled = length(selected) == 0,
    language = language
  )
}

nonparametric_paired_time_label_inputs <- function(time_labels, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  labels <- as.character(time_labels %||% paired_rm_time_header_labels(3L))
  div(
    class = "paired-rm-time-labels",
    div(class = "analysis-option-title", analysis_ui_text("Repeated variable labels", language)),
    lapply(seq_along(labels), function(index) {
      textInput(
        inputId = paste0("nonparametric_paired_time_label_", index),
        label = paste0(index, if (index == 1L) "st" else if (index == 2L) "nd" else if (index == 3L) "rd" else "th"),
        value = labels[[index]],
        width = "100%"
      )
    })
  )
}

nonparametric_paired_setup_panel <- function(state) {
  language <- normalize_app_language(state$language %||% statedu_initial_language())
  div(
    class = "ttest-anova-setup-grid paired-setup-grid paired-rm-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables", language = language),
      analysis_transfer_listbox_input("nonparametric_paired_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls ttest-anova-transfer-controls paired-transfer-controls",
      actionButton("nonparametric_paired_move", ">", class = "btn btn-default analysis-move-button", disabled = if (isTRUE(state$move_disabled) && length(state$repeated_groups) == 0) "disabled" else NULL)
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel paired-target-panel paired-rm-target-panel",
      analysis_field_label_tag("Repeated-measures variables", c("binary", "category", "ordered", "continuous"), language = language),
      analysis_transfer_listbox_input("nonparametric_paired_repeated", state$repeated_items, selected = state$repeated_selected, size = 16, important_height = TRUE),
      div(class = "analysis-order-actions paired-order-actions", actionButton("nonparametric_paired_up", analysis_ui_text("Up", language), class = "btn-default btn-sm"), actionButton("nonparametric_paired_down", analysis_ui_text("Down", language), class = "btn-default btn-sm"))
    ),
    div(
      class = "ttest-anova-options-column",
      div(
        class = "analysis-options-panel ttest-anova-options paired-options",
        if (isTRUE(state$has_three_plus)) {
          tagList(
            div(
              class = "analysis-option-group analysis-radio-group paired-posthoc-group",
              div(class = "analysis-option-title", analysis_ui_text("Post-hoc correction", language)),
              radioButtons(
                "nonparametric_paired_adjustment",
                label = NULL,
                choices = c("Bonferroni correction" = "bonferroni", "Holm Bonferroni" = "holm"),
                selected = state$adjustment
              )
            ),
            nonparametric_paired_time_label_inputs(state$time_labels, language)
          )
        },
        analysis_option_group(
          "Options",
          list(
            list(id = "nonparametric_paired_effect_size", label = "Effect size", value = state$effect_size),
            list(id = "nonparametric_paired_median_iqr", label = "Median(Q1~Q3)", value = state$median_iqr)
          ),
          language = language
        )
      )
    )
  )
}
