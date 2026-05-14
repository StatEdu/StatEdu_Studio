# Frequency/descriptive setup UI state and panel.

frequencies_setup_state <- function(
  selected_names,
  variable_table = NULL,
  labels = character(0),
  selected_variables = character(0)
) {
  selected <- as.character(selected_names %||% character(0))
  selected_variables <- intersect(as.character(selected_variables %||% character(0)), selected)
  available <- setdiff(selected, selected_variables)
  list(
    available_items = analysis_variable_items(available, variable_table, labels),
    selected_items = analysis_variable_items(selected_variables, variable_table, labels),
    move_disabled = length(selected) == 0
  )
}

frequencies_setup_panel <- function(state) {
  div(
    class = "frequencies-setup-grid",
    style = paste(
      "display:grid;",
      "grid-template-columns:326px 50px 326px 310px;",
      "gap:18px;",
      "align-items:start;",
      "width:1138px;",
      "min-width:1138px;",
      "margin-top:18px;",
      "padding:18px;",
      "background:#f4f8fb;",
      "border:1px solid #e1eaf2;",
      "border-radius:8px;",
      "box-sizing:border-box;"
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      style = "background:#edf4f8;border:1px solid #d5e2ec;border-radius:8px;padding:12px;box-sizing:border-box;",
      div(class = "analysis-field-label", "Variables"),
      analysis_transfer_listbox_input("frequency_available", state$available_items, size = 20)
    ),
    div(
      class = "analysis-transfer-controls",
      actionButton("frequency_move", ">", class = "btn btn-default analysis-move-button")
    ),
    div(
      class = "analysis-transfer-column analysis-transfer-panel",
      style = "background:#edf4f8;border:1px solid #d5e2ec;border-radius:8px;padding:12px;box-sizing:border-box;",
      div(class = "analysis-field-label", "Selected Variables"),
      analysis_transfer_listbox_input("frequency_selected", state$selected_items, size = 20)
    ),
    div(
      class = "analysis-options-column analysis-options-panel",
      style = "background:#edf4f8;border:1px solid #d5e2ec;border-radius:8px;padding:12px;box-sizing:border-box;min-height:330px;",
      analysis_option_group(
        "Table",
        list(
          list(id = "frequency_table_summary", label = "n(%) or M \u00b1 SD", value = FALSE)
        )
      ),
      analysis_option_group(
        "Statistics",
        list(
          list(id = "frequency_stat_min_max", label = "Min, Max", value = FALSE),
          list(id = "frequency_stat_skew_kurtosis", label = "Skewness, Kurtosis", value = FALSE),
          list(id = "frequency_stat_median_iqr", label = "Median, IQR(Q1~Q3)", value = FALSE)
        )
      ),
      analysis_option_group(
        "Plots",
        list(
          list(id = "frequency_plot_pie", label = "Pie chart", value = FALSE),
          list(id = "frequency_plot_bar", label = "Bar chart", value = FALSE),
          list(id = "frequency_plot_histogram", label = "Histogram", value = FALSE),
          list(id = "frequency_plot_box", label = "Box plot", value = FALSE),
          list(id = "frequency_plot_violin", label = "Violin plot", value = FALSE)
        )
      )
    )
  )
}
