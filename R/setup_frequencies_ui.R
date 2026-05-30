# Frequency/descriptive setup UI state and panel.

frequencies_setup_state <- function(
  selected_names,
  variable_table = NULL,
  labels = character(0),
  selected_variables = character(0),
  selected_available = character(0),
  selected_selected = character(0),
  table_summary = FALSE,
  stat_min_max = TRUE,
  stat_skew_kurtosis = TRUE,
  stat_median_iqr = TRUE,
  plot_pie = FALSE,
  plot_bar = FALSE,
  plot_histogram = FALSE,
  plot_box = FALSE,
  plot_violin = FALSE
) {
  selected <- as.character(selected_names %||% character(0))
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), selected)
  available <- setdiff(selected, selected_variables)
  list(
    available_items = analysis_variable_items(available, variable_table, labels),
    available_selected = selected_order_items(selected_available, available),
    selected_items = analysis_variable_items(selected_variables, variable_table, labels),
    selected_selected = selected_order_items(selected_selected, selected_variables),
    move_disabled = length(selected) == 0,
    table_summary = isTRUE(table_summary),
    stat_min_max = isTRUE(stat_min_max),
    stat_skew_kurtosis = isTRUE(stat_skew_kurtosis),
    stat_median_iqr = isTRUE(stat_median_iqr),
    plot_pie = isTRUE(plot_pie),
    plot_bar = isTRUE(plot_bar),
    plot_histogram = isTRUE(plot_histogram),
    plot_box = isTRUE(plot_box),
    plot_violin = isTRUE(plot_violin)
  )
}

frequencies_setup_panel <- function(state) {
  div(
    class = "frequencies-setup-grid",
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Variables"),
      analysis_transfer_listbox_input("frequency_available", state$available_items, selected = state$available_selected, size = 17)
    ),
    div(
      class = "analysis-transfer-controls",
      actionButton("frequency_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      analysis_field_label_tag("Selected Variables", analysis_allowed_measurements_all()),
      analysis_transfer_listbox_input("frequency_selected", state$selected_items, selected = state$selected_selected, size = 17),
      div(
        class = "analysis-order-actions frequency-order-actions",
        actionButton("frequency_move_up", "Up", class = "btn-default btn-sm"),
        actionButton("frequency_move_down", "Down", class = "btn-default btn-sm")
      )
    ),
    div(
      class = "analysis-options-column analysis-options-panel",
      analysis_option_group(
        "Table",
        list(
          list(id = "frequency_table_summary", label = "n(%) or M \u00b1 SD", value = state$table_summary)
        )
      ),
      analysis_option_group(
        "Statistics",
        list(
          list(id = "frequency_stat_min_max", label = "Min, Max", value = state$stat_min_max),
          list(id = "frequency_stat_skew_kurtosis", label = "Skewness, Kurtosis", value = state$stat_skew_kurtosis),
          list(id = "frequency_stat_median_iqr", label = "Median, IQR(Q1~Q3)", value = state$stat_median_iqr)
        )
      ),
      analysis_option_group(
        "Plots",
        list(
          list(id = "frequency_plot_pie", label = "Pie chart", value = state$plot_pie),
          list(id = "frequency_plot_bar", label = "Bar chart", value = state$plot_bar),
          list(id = "frequency_plot_histogram", label = "Histogram", value = state$plot_histogram),
          list(id = "frequency_plot_box", label = "Box plot", value = state$plot_box),
          list(id = "frequency_plot_violin", label = "Violin plot", value = state$plot_violin)
        )
      )
    )
  )
}
