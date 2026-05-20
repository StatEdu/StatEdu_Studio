# Frequency/descriptive result UI.

frequency_format_value <- function(value) {
  if (length(value) == 0 || is.na(value)) {
    return("")
  }
  as.character(value)
}

frequency_compact_summary <- function(row, is_continuous) {
  if (isTRUE(is_continuous)) {
    return(frequency_format_value(row[["M \u00b1 SD"]]))
  }
  percent <- suppressWarnings(as.numeric(row[["Percent"]]))
  percent_display <- if (length(percent) > 0 && !is.na(percent)) {
    format_frequency_percent(percent, pad_under_10 = TRUE)
  } else {
    frequency_format_value(row[["Percent"]])
  }
  paste0(frequency_format_value(row[["N"]]), "(", percent_display, ")")
}

frequency_summary_column <- function() {
  "n(%) or M \u00b1 SD"
}

frequency_combined_table <- function(result, options) {
  variables <- as.character(result$variables %||% character(0))
  descriptive <- result$descriptive_table
  categorical_tables <- result$categorical_tables %||% list()
  categorical_by_name <- list()
  if (length(categorical_tables) > 0) {
    categorical_by_name <- stats::setNames(categorical_tables, vapply(categorical_tables, function(table) {
      if (!is.null(table) && nrow(table) > 0 && "Name" %in% names(table)) {
        as.character(table$Name[[1]])
      } else {
        ""
      }
    }, character(1)))
  }

  rows <- list()
  for (name in variables) {
    continuous_row <- NULL
    if (!is.null(descriptive) && nrow(descriptive) > 0 && "Name" %in% names(descriptive)) {
      matched <- descriptive[as.character(descriptive$Name) == name, , drop = FALSE]
      if (nrow(matched) > 0) {
        continuous_row <- matched[1, , drop = FALSE]
      }
    }

    if (!is.null(continuous_row)) {
      row <- continuous_row[1, , drop = TRUE]
      output_row <- list(
        Variable = frequency_format_value(row[["Variable"]]),
        Value = "",
        n = "",
        `%` = "",
        M = frequency_format_value(row[["Mean"]]),
        SD = frequency_format_value(row[["SD"]]),
        Min = frequency_format_value(row[["Min"]]),
        Max = frequency_format_value(row[["Max"]]),
        Median = frequency_format_value(row[["Median"]]),
        `IQR(Q1~Q3)` = frequency_format_value(row[["IQR(Q1~Q3)"]]),
        Skewness = frequency_format_value(row[["Skewness"]]),
        Kurtosis = frequency_format_value(row[["Kurtosis"]])
      )
      output_row[[frequency_summary_column()]] <- frequency_compact_summary(row, TRUE)
      rows[[length(rows) + 1]] <- output_row
      next
    }

    categorical <- categorical_by_name[[name]]
    if (!is.null(categorical) && nrow(categorical) > 0) {
      for (row_index in seq_len(nrow(categorical))) {
        row <- categorical[row_index, , drop = TRUE]
        output_row <- list(
          Variable = if (row_index == 1) frequency_format_value(row[["Variable"]]) else "",
          Value = frequency_format_value(row[["Value"]]),
          n = frequency_format_value(row[["N"]]),
          `%` = frequency_format_value(row[["Percent"]]),
          M = "",
          SD = "",
          Min = "",
          Max = "",
          Median = "",
          `IQR(Q1~Q3)` = "",
          Skewness = "",
          Kurtosis = ""
        )
        output_row[[frequency_summary_column()]] <- frequency_compact_summary(row, FALSE)
        rows[[length(rows) + 1]] <- output_row
      }
    }
  }

  if (length(rows) == 0) {
    return(NULL)
  }

  table <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE, check.names = FALSE))
  columns <- if (isTRUE(options$n_percent) && isTRUE(options$mean_sd)) {
    c("Variable", "Value", frequency_summary_column())
  } else {
    c("Variable", "Value", "n", "%", "M", "SD")
  }
  if (isTRUE(options$min_max)) {
    columns <- c(columns, "Min", "Max")
  }
  if (isTRUE(options$median_iqr)) {
    columns <- c(columns, "Median", "IQR(Q1~Q3)")
  }
  if (isTRUE(options$skew_kurtosis)) {
    columns <- c(columns, "Skewness", "Kurtosis")
  }
  table[, intersect(columns, names(table)), drop = FALSE]
}

frequencies_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  options <- result$options %||% list(n_percent = TRUE, mean_sd = TRUE)
  table <- frequency_combined_table(result, options)
  if (is.null(table) || nrow(table) == 0) {
    return(NULL)
  }
  div(
    class = "result-section frequencies-result-section regression-result-panel",
    h3("Frequencies / Descriptives"),
    div(class = "frequency-table-wrap", coefficient_html_table(table)),
    frequency_plot_blocks(result, options)
  )
}

frequency_plot_output_id <- function(type, name) {
  paste0("frequency_plot_", type, "_", make.names(name))
}

frequency_plot_blocks <- function(result, options) {
  categorical <- as.character(result$categorical %||% character(0))
  continuous <- as.character(result$continuous %||% character(0))
  plot_sections <- list()

  if (isTRUE(options$pie) || isTRUE(options$bar)) {
    categorical_blocks <- unlist(lapply(categorical, function(name) {
      variable_label <- frequency_variable_display_name(name, result$variable_info, result$labels, result$category_table)
      c(
        if (isTRUE(options$pie)) {
          list(div(
            class = "frequency-plot-card",
            h4(sprintf("Pie chart(%s)", variable_label)),
            plotOutput(frequency_plot_output_id("pie", name), height = "320px")
          ))
        },
        if (isTRUE(options$bar)) {
          list(div(
            class = "frequency-plot-card",
            h4(sprintf("Bar chart(%s)", variable_label)),
            plotOutput(frequency_plot_output_id("bar", name), height = "320px")
          ))
        }
      )
    }), recursive = FALSE)
    if (length(categorical_blocks) > 0) {
      plot_sections[[length(plot_sections) + 1]] <- div(class = "frequency-plot-grid", categorical_blocks)
    }
  }

  if (isTRUE(options$histogram) || isTRUE(options$box) || isTRUE(options$violin)) {
    continuous_rows <- lapply(continuous, function(name) {
      variable_label <- frequency_variable_display_name(name, result$variable_info, result$labels, result$category_table)
      plot_blocks <- c(
        if (isTRUE(options$histogram)) {
          list(div(
            class = "frequency-plot-card",
            h4(sprintf("Histogram(%s)", variable_label)),
            plotOutput(frequency_plot_output_id("histogram", name), height = "320px")
          ))
        },
        if (isTRUE(options$box)) {
          list(div(
            class = "frequency-plot-card",
            h4(sprintf("Box plot(%s)", variable_label)),
            plotOutput(frequency_plot_output_id("box", name), height = "320px")
          ))
        },
        if (isTRUE(options$violin)) {
          list(div(
            class = "frequency-plot-card",
            h4(sprintf("Violin plot(%s)", variable_label)),
            plotOutput(frequency_plot_output_id("violin", name), height = "320px")
          ))
        }
      )
      if (length(plot_blocks) == 0) {
        return(NULL)
      }
      div(
        class = sprintf("frequency-plot-row frequency-plot-row-%d", min(length(plot_blocks), 3)),
        plot_blocks
      )
    })
    continuous_rows <- Filter(Negate(is.null), continuous_rows)
    if (length(continuous_rows) > 0) {
      plot_sections <- c(plot_sections, continuous_rows)
    }
  }

  if (length(plot_sections) == 0) {
    return(NULL)
  }

  div(
    class = "frequency-plots-section",
    h3("Plots"),
    tagList(plot_sections)
  )
}
