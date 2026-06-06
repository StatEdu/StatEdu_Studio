# Nonparametric paired test result UI.

nonparametric_paired_posthoc_display_table <- function(result) {
  table <- result$posthoc
  if (!is.data.frame(table) || isTRUE(result$options$effect_size)) {
    return(table)
  }
  table[, setdiff(names(table), c("Effect size", "ES")), drop = FALSE]
}

nonparametric_paired_rm_table <- function(result, type = c("scale", "count")) {
  type <- match.arg(type)
  table <- if (identical(type, "count")) result$count_table else result$display_table
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  if (identical(type, "scale")) {
    attr(table, "median_iqr") <- isTRUE(result$options$median_iqr)
    if (!isTRUE(result$options$effect_size)) {
      drop_columns <- c(
        "ES_overall", "ES_overall_label", "PairwiseEffectSizeLabel", "EffectSizeLabel",
        grep("^ES_[0-9]+_[0-9]+(_label)?$", names(table), value = TRUE)
      )
      table <- table[, setdiff(names(table), drop_columns), drop = FALSE]
    }
  }
  paired_rm_grouped_table(table, type)
}

nonparametric_paired_rm_note_table <- function(result) {
  table <- result$display_table
  if (!is.data.frame(table) || isTRUE(result$options$effect_size)) return(table)
  drop_columns <- c(
    "ES_overall", "ES_overall_label", "PairwiseEffectSizeLabel", "EffectSizeLabel",
    grep("^ES_[0-9]+_[0-9]+(_label)?$", names(table), value = TRUE)
  )
  table[, setdiff(names(table), drop_columns), drop = FALSE]
}

nonparametric_paired_reason <- function(row) {
  level <- as.character(row$Level[[1]] %||% "")
  method <- as.character(row$Method[[1]] %||% "")
  if (level %in% c("Binary", "Categorical")) {
    return(level)
  }
  if (identical(method, "Wilcoxon signed-rank test")) {
    return("\ube44\ubaa8\uc218 paired test")
  }
  "\ube44\ubaa8\uc218 \ubc29\ubc95"
}

nonparametric_paired_model_overview_table <- function(result) {
  tables <- Filter(
    function(x) is.data.frame(x) && nrow(x) > 0,
    list(result$scale_table, result$count_table)
  )
  if (length(tables) == 0) return(NULL)
  rows <- list()
  for (table in tables) {
    if (!"Pair" %in% names(table)) next
    for (index in seq_len(nrow(table))) {
      item <- table[index, , drop = FALSE]
      row <- data.frame(
        Pair = as.character(item$Pair[[1]] %||% ""),
        N = as.character(item$N[[1]] %||% ""),
        Analysis = paired_short_method(item$Method[[1]] %||% ""),
        Reason = nonparametric_paired_reason(item),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(row) <- c("Pair", "N", "\ubd84\uc11d \ubc29\ubc95", "\uc774\uc720")
      rows[[length(rows) + 1L]] <- row
    }
  }
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

nonparametric_paired_rm_model_overview_table <- function(result) {
  table <- paired_rm_overview_source(result)
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  group_column <- if ("Repeated variables" %in% names(table)) "Repeated variables" else names(table)[[1]]
  rows <- list()
  for (index in seq_len(nrow(table))) {
    item <- table[index, , drop = FALSE]
    measurement <- as.character(result$measurement %||% "")
    reason <- if (nzchar(measurement)) {
      paste(measurement, "\ube44\ubaa8\uc218 repeated-measures test")
    } else {
      "\ube44\ubaa8\uc218 repeated-measures test"
    }
    row <- data.frame(
      `Repeated variables` = as.character(item[[group_column]][[1]] %||% ""),
      N = as.character(item$N[[1]] %||% ""),
      Analysis = paired_rm_short_method(item$Method[[1]] %||% ""),
      Reason = reason,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(row) <- c("Repeated variables", "N", "\ubd84\uc11d \ubc29\ubc95", "\uc774\uc720")
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

nonparametric_paired_results_ui <- function(result) {
  if (is.null(result)) return(NULL)
  if (is.list(result) && !is.null(result$error)) return(empty_message(result$error))
  if (identical(result$type, "nonparametric_paired_combined")) {
    return(tags$div(
      class = "regression-results paired-results nonparametric-paired-results",
      nonparametric_paired_results_ui(result$paired),
      nonparametric_paired_results_ui(result$paired_rm)
    ))
  }
  if (identical(result$type, "nonparametric_paired_rm")) {
    overview_table <- nonparametric_paired_rm_model_overview_table(result)
    return(tags$div(
      class = "regression-results paired-results paired-rm-results nonparametric-paired-results",
      if (is.data.frame(overview_table) && nrow(overview_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel",
          tags$h3("Model overview"),
          coefficient_html_table(overview_table)
        )
      },
      if (is.data.frame(result$display_table) && nrow(result$display_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel landscape-table-panel",
          tags$h3("Nonparametric paired test: continuous / ordinal"),
          result_table_with_notes(
            nonparametric_paired_rm_table(result, "scale"),
            result_note_tag(paired_rm_table_method_note(nonparametric_paired_rm_note_table(result))),
            class = "result-table-with-note paired-fit-table-wrap"
          )
        )
      },
      if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel landscape-table-panel",
          tags$h3("Nonparametric paired test: binary"),
          result_table_with_notes(
            nonparametric_paired_rm_table(result, "count"),
            result_note_tag(paired_rm_table_method_note(result$count_table)),
            class = "result-table-with-note paired-fit-table-wrap"
          )
        )
      },
      if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel landscape-table-panel",
          tags$h3("Post-hoc pairwise comparisons"),
          coefficient_html_table(nonparametric_paired_posthoc_display_table(result), note_line = paired_rm_posthoc_note(result))
        )
      },
      if (is.data.frame(result$skipped) && nrow(result$skipped) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel landscape-table-panel paired-diagnostics-panel",
          tags$h3("Skipped repeated-measures rows"),
          coefficient_html_table(result$skipped)
        )
      }
    ))
  }
  if (is.data.frame(result$scale_table)) {
    attr(result$scale_table, "median_iqr") <- isTRUE(result$options$median_iqr)
  }
  overview_table <- nonparametric_paired_model_overview_table(result)
  tags$div(
    class = "regression-results paired-results nonparametric-paired-results",
    if (is.data.frame(overview_table) && nrow(overview_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Model overview"),
        coefficient_html_table(overview_table)
      )
    },
    if (is.data.frame(result$scale_table) && nrow(result$scale_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Nonparametric paired test: continuous / ordinal"),
        result_table_with_notes(
          paired_grouped_table(result$scale_table, "scale", show_effect_size = isTRUE(result$options$effect_size)),
          result_note_tag(paired_method_note(result$scale_table, show_effect_size = isTRUE(result$options$effect_size))),
          class = "result-table-with-note paired-fit-table-wrap"
        )
      )
    },
    if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Nonparametric paired test: binary / categorical"),
        result_table_with_notes(
          paired_grouped_table(result$count_table, "count", show_effect_size = isTRUE(result$options$effect_size)),
          result_note_tag(paired_count_method_note(result, show_effect_size = isTRUE(result$options$effect_size))),
          class = "result-table-with-note paired-fit-table-wrap"
        )
      )
    },
    if (is.data.frame(result$warnings) && nrow(result$warnings) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel paired-diagnostics-panel",
        tags$h3("Warnings"),
        coefficient_html_table(result$warnings)
      )
    },
    if (is.data.frame(result$skipped) && nrow(result$skipped) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel paired-diagnostics-panel",
        tags$h3("Skipped pairs"),
        coefficient_html_table(result$skipped)
      )
    }
  )
}
