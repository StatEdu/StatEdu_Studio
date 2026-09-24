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

frequency_summary_columns <- function(result, options) {
  has_categorical <- length(as.character(result$categorical %||% character(0))) > 0
  has_continuous <- length(as.character(result$continuous %||% character(0))) > 0
  columns <- c("Variable", "Value")

  if (isTRUE(options$n_percent) && isTRUE(options$mean_sd)) {
    if (isTRUE(has_categorical) && isTRUE(has_continuous)) {
      return(c(columns, frequency_summary_column()))
    }
    if (isTRUE(has_categorical)) {
      return(c(columns, "n (%)"))
    }
    if (isTRUE(has_continuous)) {
      return(c(columns, "M \u00b1 SD"))
    }
    return(columns)
  }

  if (isTRUE(has_categorical)) {
    columns <- c(columns, "n", "%")
  }
  if (isTRUE(has_continuous)) {
    columns <- c(columns, "M", "SD")
  }
  columns
}

frequency_combined_table <- function(result, options) {
  variables <- as.character(result$variables %||% character(0))
  has_continuous <- length(as.character(result$continuous %||% character(0))) > 0
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
      output_row[["M \u00b1 SD"]] <- frequency_compact_summary(row, TRUE)
      output_row[["n (%)"]] <- ""
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
        output_row[["M \u00b1 SD"]] <- ""
        output_row[["n (%)"]] <- frequency_compact_summary(row, FALSE)
        rows[[length(rows) + 1]] <- output_row
      }
    }
  }

  if (length(rows) == 0) {
    return(NULL)
  }

  table <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE, check.names = FALSE))
  columns <- frequency_summary_columns(result, options)
  if (isTRUE(has_continuous) && isTRUE(options$min_max)) {
    columns <- c(columns, "Min", "Max")
  }
  if (isTRUE(has_continuous) && isTRUE(options$median_iqr)) {
    columns <- c(columns, "Median", "IQR(Q1~Q3)")
  }
  if (isTRUE(has_continuous) && isTRUE(options$skew_kurtosis)) {
    columns <- c(columns, "Skewness", "Kurtosis")
  }
  table[, intersect(columns, names(table)), drop = FALSE]
}

frequency_categorical_main_table <- function(result) {
  categorical_names <- as.character(result$categorical %||% character(0))
  categorical_tables <- result$categorical_tables %||% list()
  if (length(categorical_names) == 0L || length(categorical_tables) == 0L) {
    return(NULL)
  }

  tables_by_name <- stats::setNames(categorical_tables, vapply(categorical_tables, function(table) {
    if (is.data.frame(table) && nrow(table) > 0L && "Name" %in% names(table)) {
      as.character(table$Name[[1L]])
    } else {
      ""
    }
  }, character(1)))
  rows <- lapply(categorical_names, function(name) {
    table <- tables_by_name[[name]]
    if (!is.data.frame(table) || nrow(table) == 0L) {
      return(NULL)
    }
    variable <- as.character(table$Variable)
    if (length(variable) > 1L) {
      variable[-1L] <- ""
    }
    data.frame(
      Variable = variable,
      Value = as.character(table$Value),
      n = as.character(table$N),
      `%` = as.character(table$Percent),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(NULL)
  }
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  attr(table, "result_table_role") <- "main"
  attr(table, "result_table_language") <- "en"
  table
}

frequency_continuous_main_table <- function(result, options) {
  continuous_names <- as.character(result$continuous %||% character(0))
  descriptive <- result$descriptive_table
  if (length(continuous_names) == 0L || !is.data.frame(descriptive) || nrow(descriptive) == 0L) {
    return(NULL)
  }

  order_index <- match(continuous_names, as.character(descriptive$Name))
  order_index <- order_index[!is.na(order_index)]
  if (length(order_index) == 0L) {
    return(NULL)
  }
  descriptive <- descriptive[order_index, , drop = FALSE]
  table <- data.frame(
    Variable = as.character(descriptive$Variable),
    n = as.character(descriptive$N),
    M = as.character(descriptive$Mean),
    SD = as.character(descriptive$SD),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (isTRUE(options$mean_sd)) {
    table <- table[c("Variable", "n")]
    table[["M ± SD"]] <- as.character(descriptive[["M ± SD"]])
  }
  if (isTRUE(options$min_max)) {
    table$Min <- as.character(descriptive$Min)
    table$Max <- as.character(descriptive$Max)
  }
  if (isTRUE(options$median_iqr)) {
    table$Median <- as.character(descriptive$Median)
    table[["IQR (Q1–Q3)"]] <- as.character(descriptive[["IQR(Q1~Q3)"]])
  }
  if (isTRUE(options$skew_kurtosis)) {
    table$Skewness <- as.character(descriptive$Skewness)
    table$Kurtosis <- as.character(descriptive$Kurtosis)
  }
  attr(table, "result_table_orientation") <- "portrait"
  # Budget the portrait sheet for complete numeric tokens, including 10.00,
  # and a complete quartile interval on the second line.
  widths <- c(Variable = 64, n = 32, M = 44, SD = 44, `M ± SD` = 88, Min = 44, Max = 44,
              Median = 50, `IQR (Q1–Q3)` = 100, Skewness = 65, Kurtosis = 60)
  selected_widths <- unname(widths[names(table)])
  attr(table, "compact_column_widths") <- 100 * selected_widths / sum(selected_widths)
  attr(table, "compact_cell_padding") <- "5px 3px"
  attr(table, "column_display_labels") <- c(`IQR (Q1–Q3)` = "IQR\n(q1-q3)")
  attr(table, "nowrap_columns") <- setdiff(names(table), c("Variable", "IQR (Q1–Q3)"))
  attr(table, "right_align_columns") <- "n"
  attr(table, "result_table_role") <- "main"
  attr(table, "result_table_language") <- "en"
  table
}

frequency_main_table_section <- function(title, table, note = "") {
  if (!is.data.frame(table) || nrow(table) == 0L) {
    return(NULL)
  }
  contract <- result_table_contract(table, role = "main", language = "en",
                                    orientation = attr(table, "result_table_orientation", exact = TRUE) %||% "auto")
  div(
    class = paste(
      "result-section frequencies-result-section regression-result-panel",
      "result-table-sheet-section",
      paste0("result-table-sheet-section--", contract$role),
      paste0("result-table-sheet-section--", contract$orientation)
    ),
    lang = "en",
    h3(title),
    div(
      class = "frequency-table-wrap",
      coefficient_html_table(
        table,
        compact = TRUE,
        compact_width = 58,
        compact_first_width = 130,
        compact_min_width = 480,
        note_line = note,
        table_role = "main",
        table_language = "en"
      )
    )
  )
}

frequency_main_table_sections <- function(result, options = NULL) {
  if (is.null(result)) {
    return(NULL)
  }
  options <- options %||% result$options %||% list(n_percent = TRUE, mean_sd = TRUE)
  categorical_table <- frequency_categorical_main_table(result)
  continuous_table <- frequency_continuous_main_table(result, options)
  continuous_abbreviations <- c("M = mean", "SD = standard deviation")
  if (isTRUE(options$median_iqr)) {
    continuous_abbreviations <- c(continuous_abbreviations, "IQR = interquartile range")
  }
  tagList(
    frequency_main_table_section(
      "Categorical Frequencies",
      categorical_table,
      result_sci_note_text(format = "Values are counts and percentages")
    ),
    frequency_main_table_section(
      "Continuous Descriptive Statistics",
      continuous_table,
      result_sci_note_text(abbreviations = paste(continuous_abbreviations, collapse = "; "))
    )
  )
}

frequencies_results_ui <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  options <- result$options %||% list(n_percent = TRUE, mean_sd = TRUE)
  categorical_table <- frequency_categorical_main_table(result)
  continuous_table <- frequency_continuous_main_table(result, options)
  if ((!is.data.frame(categorical_table) || nrow(categorical_table) == 0L) &&
      (!is.data.frame(continuous_table) || nrow(continuous_table) == 0L)) {
    return(NULL)
  }
  tagList(
    frequency_main_table_sections(result, options),
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
