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
    return(tags$div(
      class = "regression-results paired-results paired-rm-results nonparametric-paired-results",
      if (is.data.frame(result$display_table) && nrow(result$display_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel landscape-table-panel",
          tags$h3("Nonparametric paired test: continuous / ordinal"),
          result_table_with_notes(
            nonparametric_paired_rm_table(result, "scale"),
            result_note_tag(paired_rm_table_method_note(nonparametric_paired_rm_note_table(result)))
          )
        )
      },
      if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
        tags$div(
          class = "result-section paired-result-section regression-result-panel landscape-table-panel",
          tags$h3("Nonparametric paired test: binary"),
          result_table_with_notes(
            nonparametric_paired_rm_table(result, "count"),
            result_note_tag(paired_rm_table_method_note(result$count_table))
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
          class = "result-section paired-result-section regression-result-panel landscape-table-panel",
          tags$h3("Skipped repeated-measures rows"),
          coefficient_html_table(result$skipped)
        )
      }
    ))
  }
  if (is.data.frame(result$scale_table)) {
    attr(result$scale_table, "median_iqr") <- isTRUE(result$options$median_iqr)
  }
  tags$div(
    class = "regression-results paired-results nonparametric-paired-results",
    if (is.data.frame(result$scale_table) && nrow(result$scale_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Nonparametric paired test: continuous / ordinal"),
        result_table_with_notes(
          paired_grouped_table(result$scale_table, "scale", show_effect_size = isTRUE(result$options$effect_size)),
          result_note_tag(paired_method_note(result$scale_table, show_effect_size = isTRUE(result$options$effect_size)))
        )
      )
    },
    if (is.data.frame(result$count_table) && nrow(result$count_table) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Nonparametric paired test: binary / categorical"),
        result_table_with_notes(
          paired_grouped_table(result$count_table, "count", show_effect_size = isTRUE(result$options$effect_size)),
          result_note_tag(paired_count_method_note(result, show_effect_size = isTRUE(result$options$effect_size)))
        )
      )
    },
    if (is.data.frame(result$warnings) && nrow(result$warnings) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Warnings"),
        coefficient_html_table(result$warnings)
      )
    },
    if (is.data.frame(result$skipped) && nrow(result$skipped) > 0) {
      tags$div(
        class = "result-section paired-result-section regression-result-panel",
        tags$h3("Skipped pairs"),
        coefficient_html_table(result$skipped)
      )
    }
  )
}
