# Survival analysis result UI.

survival_cell_note_marker <- function(table, row_index, column) {
  markers <- attr(table, "note_markers", exact = TRUE)
  if (!is.data.frame(markers) || nrow(markers) == 0) return("")
  matched <- markers[markers$row == row_index & markers$column == column, , drop = FALSE]
  if (nrow(matched) == 0) "" else as.character(matched$marker[[1]])
}

survival_cell_content <- function(value, marker = "") {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value) || !nzchar(marker)) return(value)
  tagList(
    value,
    tags$sup(style = "margin-left:2px;font-size:75%;vertical-align:super;", marker)
  )
}

survival_header_content <- function(name) {
  name <- as.character(name)
  if (identical(name, "Chi-square")) {
    return(tagList(HTML("&chi;"), tags$sup("2")))
  }
  if (identical(name, "Chi-square (df)")) {
    return(tagList(HTML("&chi;"), tags$sup("2"), "(df)"))
  }
  if (identical(name, "Median (95% CI)")) {
    return(tagList("Median", tags$br(), "(95% CI)"))
  }
  name
}

survival_simple_table <- function(table, class = "survival-result-table") {
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  tags$table(
    class = paste("coefficient-table", class),
    style = result_table_style(font_size = 12, min_width = 0),
    tags$thead(tags$tr(lapply(names(table), function(name) tags$th(style = result_header_cell_style(), survival_header_content(name))))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(table), function(col_index) {
        column <- names(table)[[col_index]]
        value <- table[[col_index]][[row_index]]
        if (is.numeric(value)) value <- survival_format_number(value)
        marker <- survival_cell_note_marker(table, row_index, column)
        tags$td(style = result_body_cell_style(col_index == 1L, row_index == nrow(table)), survival_cell_content(value, marker))
      }))
    }))
  )
}

survival_table_note <- function(text) {
  text <- as.character(text %||% "")
  if (!nzchar(text)) return(NULL)
  div(class = "survival-table-note", text)
}

survival_km_overview_table <- function(result) {
  group_variables <- if (identical(result$type, "km_multi")) {
    labels <- vapply(result$analyses %||% list(), survival_km_group_label, character(1))
    paste(labels[nzchar(labels)], collapse = ", ")
  } else {
    survival_km_group_label(result)
  }
  group_variables <- if (nzchar(group_variables) && !identical(group_variables, "All")) group_variables else "None"
  data.frame(
    Item = c("Method", "Time variable", "Event variable", "Event value", "Group variables", "N", "Events", "Censored", "Package"),
    Value = c(
      if (identical(result$analysis_method, "life_table")) "Life table" else "Kaplan-Meier",
      result$time %||% "",
      result$event %||% "",
      result$event_value %||% "",
      group_variables,
      result$n,
      result$events,
      result$censored,
      result$packages
    ),
    stringsAsFactors = FALSE
  )
}

survival_km_group_label <- function(result) {
  group <- as.character(result$group %||% "")
  if (nzchar(group)) group else "All"
}

survival_km_level_label <- function(value, group = "") {
  value <- trimws(as.character(value %||% ""))
  group <- trimws(as.character(group %||% ""))
  if (!nzchar(value) || !nzchar(group) || identical(group, "All")) {
    return(value)
  }
  prefix <- paste0(group, "=")
  if (startsWith(value, prefix)) {
    return(substr(value, nchar(prefix) + 1L, nchar(value)))
  }
  value
}

survival_median_ci_text <- function(median, lower, upper) {
  median_num <- suppressWarnings(as.numeric(median))
  median_text <- if (length(median_num) > 0 && is.finite(median_num)) survival_format_number(median_num) else "NR"
  ci_text <- survival_format_ci(lower, upper)
  if (!nzchar(ci_text)) {
    return(median_text)
  }
  if (identical(median_text, "NR") && identical(ci_text, "(NE-NE)")) {
    return("NR")
  }
  paste0(median_text, "\u00A0", ci_text)
}

survival_km_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  row_offset <- 0L
  marker_rows <- list()
  rows <- lapply(items, function(item) {
    table <- item$median_table
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    group_label <- survival_km_group_label(item)
    item_rows <- data.frame(
      Variables = group_label,
      Level = vapply(table$Strata, survival_km_level_label, character(1), group = group_label),
      N = table$Records,
      Events = table$Events,
      Censored = table$Records - table$Events,
      MeanSE = sprintf("%s \u00B1 %s", vapply(table$Mean, survival_format_number, character(1)), vapply(table$`Mean SE`, survival_format_number, character(1))),
      Q1 = vapply(table$Q1, survival_format_number, character(1)),
      `Median (95% CI)` = mapply(function(median, lower, upper) {
        survival_median_ci_text(median, lower, upper)
      }, table$Median, table$`Median 95% LCL`, table$`Median 95% UCL`),
      Q3 = vapply(table$Q3, survival_format_number, character(1)),
      `Chi-square (df)` = "",
      p = "",
      `post-hoc` = "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(item_rows)[names(item_rows) == "MeanSE"] <- "M \u00B1 SE"
    logrank <- item$logrank
    if (!is.null(logrank) && nrow(item_rows) > 0) {
      item_rows$`Chi-square (df)`[[1]] <- sprintf("%s (%s)", survival_format_number(logrank$chisq), as.character(logrank$df))
      item_rows$p[[1]] <- survival_p(logrank$p)
    }
    item_rows <- survival_km_apply_ordered_posthoc(item_rows, item)
    if (nrow(item_rows) > 1L) {
      item_rows$Variables[-1L] <- ""
      item_rows$`Chi-square (df)`[-1L] <- ""
      item_rows$p[-1L] <- ""
    }
    markers <- attr(item_rows, "note_markers", exact = TRUE)
    if (is.data.frame(markers) && nrow(markers) > 0) {
      markers$row <- as.integer(markers$row) + row_offset
      marker_rows[[length(marker_rows) + 1L]] <<- markers
    }
    row_offset <<- row_offset + nrow(item_rows)
    item_rows
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  rows_without_attrs <- lapply(rows, function(row) {
    attr(row, "note_markers") <- NULL
    row
  })
  out <- do.call(rbind, rows_without_attrs)
  if (length(marker_rows) > 0) {
    attr(out, "note_markers") <- do.call(rbind, marker_rows)
  }
  out
}

survival_km_posthoc_p_matrix <- function(item) {
  posthoc <- item$posthoc
  group <- survival_km_group_label(item)
  strata <- vapply(as.character(item$median_table$Strata %||% character(0)), survival_km_level_label, character(1), group = group)
  strata <- strata[nzchar(strata)]
  if (!is.data.frame(posthoc) || nrow(posthoc) == 0 || length(strata) < 2L) {
    return(NULL)
  }
  p_matrix <- matrix(NA_real_, nrow = length(strata), ncol = length(strata), dimnames = list(strata, strata))
  diag(p_matrix) <- 1
  display_level <- function(level) {
    level <- trimws(as.character(level %||% ""))
    if (!nzchar(level)) return(level)
    survival_km_level_label(level, group)
  }
  has_direct_groups <- all(c("Group1", "Group2") %in% names(posthoc))
  for (row_index in seq_len(nrow(posthoc))) {
    first <- if (has_direct_groups) display_level(posthoc$Group1[[row_index]]) else character(0)
    second <- if (has_direct_groups) display_level(posthoc$Group2[[row_index]]) else character(0)
    if (!nzchar(first) || !nzchar(second)) {
      parts <- trimws(strsplit(as.character(posthoc$Comparison[[row_index]] %||% ""), "\\s+vs\\s+", perl = TRUE)[[1]])
      if (length(parts) != 2L) next
      first <- display_level(parts[[1]])
      second <- display_level(parts[[2]])
    }
    if (!all(c(first, second) %in% strata)) next
    p_value <- suppressWarnings(as.numeric(posthoc$p_adjusted[[row_index]] %||% posthoc$p[[row_index]]))
    if (!is.finite(p_value)) next
    p_matrix[first, second] <- p_value
    p_matrix[second, first] <- p_value
  }
  p_matrix
}

survival_km_apply_ordered_posthoc <- function(rows, item, alpha = .05) {
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"post-hoc" %in% names(rows)) {
    return(rows)
  }
  p_matrix <- survival_km_posthoc_p_matrix(item)
  if (is.null(p_matrix)) return(rows)
  estimates <- suppressWarnings(as.numeric(item$median_table$Median))
  fallback <- suppressWarnings(as.numeric(item$median_table$Mean))
  estimates[!is.finite(estimates)] <- fallback[!is.finite(estimates)]
  if (exists("analysis_apply_ordered_posthoc_markers", mode = "function")) {
    return(analysis_apply_ordered_posthoc_markers(
      rows,
      estimates = estimates,
      levels = rows$Level,
      p_matrix = p_matrix,
      label_column = "Level",
      alpha = alpha
    ))
  }
  rows
}

survival_km_test_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    logrank <- item$logrank
    if (is.null(logrank)) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      Test = logrank$label %||% item$test_label %||% "Log-rank",
      Chisq = survival_format_number(logrank$chisq),
      df = logrank$df,
      p = survival_p(logrank$p),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_posthoc_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    table <- survival_km_posthoc_display_table(item)
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_rate_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    table <- survival_km_rate_table(item)
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_life_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    table <- survival_life_table_display(item)
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_logrank_table <- function(result) {
  logrank <- result$logrank
  if (is.null(logrank)) return(data.frame())
  data.frame(
    Test = logrank$label %||% result$test_label %||% "Log-rank",
    Chisq = survival_format_number(logrank$chisq),
    df = logrank$df,
    p = survival_p(logrank$p),
    stringsAsFactors = FALSE
  )
}

survival_km_rate_table <- function(result) {
  summary_at <- result$summary_at
  if (is.null(summary_at)) return(data.frame())
  strata <- as.character(summary_at$strata %||% "")
  if (length(strata) == 0) strata <- rep("All", length(summary_at$time))
  data.frame(
    Strata = strata,
    Time = summary_at$time,
    `Number at risk` = summary_at$n.risk,
    Survival = survival_format_number(summary_at$surv),
    `95% CI` = mapply(survival_format_ci, summary_at$lower, summary_at$upper),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_life_table_display <- function(result) {
  table <- result$life_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  for (name in names(table)) {
    if (is.numeric(table[[name]])) {
      table[[name]] <- vapply(table[[name]], survival_format_number, character(1))
    }
  }
  table
}

survival_km_posthoc_display_table <- function(result) {
  table <- result$posthoc
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Comparison = table$Comparison,
    Chisq = vapply(table$Chisq, survival_format_number, character(1)),
    df = table$df,
    p = vapply(table$p, survival_p, character(1)),
    `p adjusted` = vapply(table$p_adjusted, survival_p, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_plot_type_label <- function(type, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  switch(as.character(type %||% "survival")[[1]],
    event = survival_ui_text("1 - Survival function", language),
    cumhaz = survival_ui_text("Cumulative hazard", language),
    log_survival = survival_ui_text("Log survival", language),
    survival_ui_text("Survival function", language)
  )
}

survival_plot_version_label <- function(version, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  switch(as.character(version %||% "color")[[1]],
    bw = survival_ui_text("Black and white", language),
    survival_ui_text("Color", language)
  )
}

survival_km_result_items <- function(result) {
  if (identical(result$type, "km_multi")) {
    return(result$analyses %||% list())
  }
  list(result)
}

survival_km_result_panel <- function(result, plot_output_ids = "survival_km_plot", title = NULL, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  output_tables <- as.character(result$output_tables %||% c("survival_table", "survival_time"))
  plot_types <- as.character(result$plot_types %||% "survival")
  plot_versions <- as.character(result$plot_versions %||% "color")
  plot_specs <- expand.grid(
    plot_type = plot_types,
    plot_version = plot_versions,
    stringsAsFactors = FALSE
  )
  if (length(plot_output_ids) < nrow(plot_specs)) {
    plot_output_ids <- paste0("survival_km_plot_", seq_len(nrow(plot_specs)))
  }
  div(
    class = "survival-km-result-block",
    if (nzchar(as.character(title %||% ""))) h2(title),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3(survival_ui_text("Model overview")),
      survival_simple_table(survival_km_overview_table(result))
    ),
    lapply(seq_len(nrow(plot_specs)), function(index) {
      plot_type <- plot_specs$plot_type[[index]]
      plot_version <- plot_specs$plot_version[[index]]
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(paste(survival_plot_type_label(plot_type, language), survival_plot_version_label(plot_version, language), sep = " - ")),
        plotOutput(plot_output_ids[[index]], height = "420px")
      )
    }),
    if ("survival_time" %in% output_tables) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(survival_ui_text("Kaplan-Meier survival time summary", language)),
        survival_simple_table(result$median_table)
      )
    },
    if ("survival_table" %in% output_tables) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(survival_ui_text("Survival table", language)),
        survival_simple_table(survival_km_rate_table(result))
      )
    },
    if (identical(result$analysis_method, "life_table") && "survival_table" %in% output_tables) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(survival_ui_text("Life table", language)),
        survival_simple_table(survival_life_table_display(result))
      )
    },
    if (!is.null(result$logrank)) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(survival_ui_text("Log-rank test")),
        survival_simple_table(survival_logrank_table(result))
      )
    },
    if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(survival_ui_text("Post-hoc pairwise comparison", language)),
        survival_simple_table(survival_km_posthoc_display_table(result))
      )
    }
  )
}

survival_km_results_panel <- function(result, plot_output_ids = "survival_km_plot", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  items <- survival_km_result_items(result)
  if (length(items) == 0) return(NULL)
  if (length(plot_output_ids) < length(items)) {
    plot_output_ids <- paste0("survival_km_plot_", seq_along(items))
  }
  output_tables <- as.character(result$output_tables %||% c("survival_table", "survival_time"))
  km_summary <- survival_km_summary_table(result)
  rate_summary <- survival_km_rate_summary_table(result)
  life_summary <- survival_life_summary_table(result)
  div(
    class = "survival-results",
    div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
      h3(survival_ui_text("Model overview", language)),
      survival_simple_table(survival_km_overview_table(result))
    ),
    div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
      h3(survival_ui_text("Kaplan-Meier survival time summary", language)),
      survival_simple_table(km_summary),
      survival_table_note("M = mean; SE = standard error; CI = confidence interval; NE = not estimable; NR = not reached.")
    ),
    if ("survival_table" %in% output_tables && nrow(rate_summary) > 0) {
      div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
        h3(survival_ui_text("Survival table", language)),
        survival_simple_table(rate_summary)
      )
    },
    if (identical(result$analysis_method, "life_table") && "survival_table" %in% output_tables && nrow(life_summary) > 0) {
      div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
        h3(survival_ui_text("Life table", language)),
        survival_simple_table(life_summary)
      )
    },
    div(class = "survival-plot-results",
      lapply(seq_along(items), function(index) {
        item <- items[[index]]
        item_plot_ids <- plot_output_ids[[index]]
        plot_types <- as.character(item$plot_types %||% "survival")
        plot_versions <- as.character(item$plot_versions %||% "color")
        plot_specs <- expand.grid(
          plot_type = plot_types,
          plot_version = plot_versions,
          stringsAsFactors = FALSE
        )
        if (is.null(item_plot_ids) || length(item_plot_ids) < nrow(plot_specs)) {
          item_plot_ids <- paste0("survival_km_plot_", index, "_", seq_len(nrow(plot_specs)))
        }
        lapply(seq_len(nrow(plot_specs)), function(plot_index) {
          plot_type <- plot_specs$plot_type[[plot_index]]
          plot_version <- plot_specs$plot_version[[plot_index]]
          title <- paste(survival_plot_type_label(plot_type, language), survival_plot_version_label(plot_version, language), sep = " - ")
          if (identical(result$type, "km_multi")) {
            title <- paste0(survival_km_group_label(item), ": ", title)
          }
          div(class = "result-section regression-result-panel survival-result-panel survival-plot-result-panel",
            h3(title),
            plotOutput(item_plot_ids[[plot_index]], height = "520px")
          )
        })
      })
    )
  )
}

survival_cox_overview_table <- function(result) {
  concordance <- result$concordance
  c_value <- if (length(concordance) >= 1L) suppressWarnings(as.numeric(concordance[[1]])) else NA_real_
  c_se <- if (length(concordance) >= 2L) suppressWarnings(as.numeric(concordance[[2]])) else NA_real_
  c_index <- if (is.finite(c_value) && is.finite(c_se)) {
    lower <- max(0, c_value - 1.96 * c_se)
    upper <- min(1, c_value + 1.96 * c_se)
    sprintf("%s %s", survival_format_number(c_value), survival_format_ci(lower, upper))
  } else if (is.finite(c_value)) {
    survival_format_number(c_value)
  } else {
    ""
  }
  model_tests <- result$model_tests
  likelihood <- if (is.data.frame(model_tests) && nrow(model_tests) > 0) {
    model_tests[model_tests$Test == "Likelihood ratio", , drop = FALSE]
  } else {
    data.frame()
  }
  lr_chisq <- if (nrow(likelihood) > 0) sprintf("%s (%s)", survival_format_number(likelihood$Chisq[[1]]), as.character(likelihood$df[[1]])) else ""
  lr_p <- if (nrow(likelihood) > 0) survival_p(likelihood$p[[1]]) else ""
  data.frame(
    Item = c("N", "Events", "LR chi-square (df)", "LR p", "Concordance (95% CI)", "Package"),
    Value = c(result$n, result$events, lr_chisq, lr_p, c_index, result$packages),
    stringsAsFactors = FALSE
  )
}

survival_cox_model_test_table <- function(result) {
  table <- result$model_tests
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Test = table$Test,
    `Chi-square (df)` = sprintf("%s (%s)", vapply(table$Chisq, survival_format_number, character(1)), as.character(table$df)),
    p = vapply(table$p, survival_p, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_coef_table <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = table$Term,
    B = vapply(table$B, survival_format_number, character(1)),
    SE = vapply(table$SE, survival_format_number, character(1)),
    HR = vapply(table$HR, survival_format_number, character(1)),
    `95% CI` = mapply(survival_format_ci, table$LLCI, table$ULCI),
    z = vapply(table$z, survival_format_number, character(1)),
    p = vapply(table$p, survival_p, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_ph_table <- function(result) {
  table <- result$ph_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  names(table) <- sub("^p$", "p", names(table))
  for (name in names(table)) {
    if (is.numeric(table[[name]])) {
      table[[name]] <- if (identical(name, "p")) {
        vapply(table[[name]], survival_p, character(1))
      } else {
        vapply(table[[name]], survival_format_number, character(1))
      }
    }
  }
  table
}

survival_cox_results_panel <- function(result) {
  div(
    class = "survival-results",
    div(class = "result-section regression-result-panel survival-result-panel",
      h3(survival_ui_text("Model overview")),
      survival_simple_table(survival_cox_overview_table(result))
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3(survival_ui_text("Cox Regression")),
      survival_simple_table(survival_cox_coef_table(result))
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("Model tests"),
      survival_simple_table(survival_cox_model_test_table(result))
    ),
    div(class = "result-section regression-result-panel survival-result-panel survival-plot-result-panel",
      h3("Hazard ratio forest plot"),
      plotOutput("survival_cox_forest_plot", height = "420px")
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3(survival_ui_text("PH assumption")),
      survival_simple_table(survival_ph_table(result)),
      div("PH assumption is assessed with Schoenfeld residuals. A small p-value suggests time-varying effects.", class = "result-note")
    )
  )
}
