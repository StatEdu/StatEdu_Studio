# Mixed repeated-measures ANOVA result UI.

mixed_rm_anova_results_ui <- function(result) {
  if (is.null(result)) return(NULL)
  if (is.list(result) && !is.null(result$error)) return(empty_message(result$error))
  tags$div(
    class = "regression-results mixed-rm-anova-results",
    if (is.data.frame(result$overview) && nrow(result$overview) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Model overview"),
        model_overview_html_table(result$overview)
      )
    },
    if (is.data.frame(result$recommendation) && nrow(result$recommendation) > 0) {
      tags$div(
        class = "result-section regression-result-panel landscape-table-panel",
        tags$h3("Recommended interpretation"),
        coefficient_html_table(result$recommendation)
      )
    },
    if (is.data.frame(result$observed_descriptives) && nrow(result$observed_descriptives) > 0 && is.data.frame(result$adjusted_descriptives) && nrow(result$adjusted_descriptives) > 0) {
      tags$div(
        class = "result-section regression-result-panel landscape-table-panel",
        tags$h3("Observed mean summary"),
        coefficient_html_table(result$observed_descriptives, note_line = result$observed_descriptives_note %||% "")
      )
    },
    if (is.data.frame(result$adjusted_descriptives) && nrow(result$adjusted_descriptives) > 0) {
      tags$div(
        class = "result-section regression-result-panel landscape-table-panel",
        tags$h3("Adjusted mean summary"),
        coefficient_html_table(result$adjusted_descriptives, note_line = result$adjusted_descriptives_note %||% "")
      )
    },
    if ((!is.data.frame(result$adjusted_descriptives) || nrow(result$adjusted_descriptives) == 0) && is.data.frame(result$descriptives) && nrow(result$descriptives) > 0) {
      tags$div(
        class = "result-section regression-result-panel landscape-table-panel",
        tags$h3("Group x time summary"),
        coefficient_html_table(result$descriptives, note_line = result$descriptives_note %||% "")
      )
    },
    if (is.data.frame(result$anova) && nrow(result$anova) > 0) {
      tags$div(
        class = "result-section regression-result-panel landscape-table-panel",
        tags$h3("Repeated-measures ANOVA"),
        coefficient_html_table(result$anova, note_line = result$method_note %||% "")
      )
    },
    if (is.data.frame(result$mixed_model_overview) && nrow(result$mixed_model_overview) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Mixed-model alternative"),
        model_overview_html_table(result$mixed_model_overview)
      )
    },
    if (is.data.frame(result$mixed_model_coefficients) && nrow(result$mixed_model_coefficients) > 0) {
      tags$div(
        class = "result-section regression-result-panel landscape-table-panel",
        tags$h3("Mixed-model coefficients"),
        coefficient_html_table(result$mixed_model_coefficients, note_line = result$mixed_model_note %||% "")
      )
    },
    if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
      tags$div(
        class = "result-section regression-result-panel landscape-table-panel",
        tags$h3("Post-hoc comparisons"),
        coefficient_html_table(result$posthoc, note_line = result$posthoc_note %||% "")
      )
    },
    if (is.data.frame(result$assumption) && nrow(result$assumption) > 0) {
      tags$div(
        class = "result-section regression-result-panel",
        tags$h3("Assumption review"),
        model_overview_html_table(result$assumption)
      )
    },
    if (is.data.frame(result$normality) && nrow(result$normality) > 0) {
      tags$div(
        class = "result-section regression-result-panel landscape-table-panel",
        tags$h3("Normality review"),
        coefficient_html_table(result$normality, note_line = paste0("Normality method: ", attr(result$normality, "normality_method", exact = TRUE) %||% "Shapiro-Wilk", "."))
      )
    }
  )
}

saved_mixed_rm_anova_results_html <- function(result, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  html <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title("StatEdu Studio Repeated-Measures ANOVA"),
      tags$link(rel = "stylesheet", type = "text/css", href = css_path)
    ),
    tags$body(
      class = if (isTRUE(report_mode)) "print-report" else NULL,
      tags$div(class = "regression-results", mixed_rm_anova_results_ui(result))
    )
  )
  paste("<!DOCTYPE html>", tags_to_html(html), sep = "\n")
}

write_mixed_rm_anova_results_html <- function(result, file) {
  writeLines(saved_mixed_rm_anova_results_html(result), file, useBytes = TRUE)
  invisible(file)
}

write_mixed_rm_anova_results_pdf <- function(result, file) {
  write_pdf_from_html(saved_mixed_rm_anova_results_html(result, report_mode = TRUE), file)
  invisible(file)
}

save_mixed_rm_anova_excel_file <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  if (is.data.frame(result$overview) && nrow(result$overview) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Model overview", result$overview, used_sheets, title = "Model overview")
  }
  if (is.data.frame(result$recommendation) && nrow(result$recommendation) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Recommendation", result$recommendation, "", used_sheets, title = "Recommended interpretation")
  }
  if (is.data.frame(result$observed_descriptives) && nrow(result$observed_descriptives) > 0 && is.data.frame(result$adjusted_descriptives) && nrow(result$adjusted_descriptives) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Observed summary", result$observed_descriptives, result$observed_descriptives_note %||% "", used_sheets, title = "Observed mean summary")
  }
  if (is.data.frame(result$adjusted_descriptives) && nrow(result$adjusted_descriptives) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Adjusted summary", result$adjusted_descriptives, result$adjusted_descriptives_note %||% "", used_sheets, title = "Adjusted mean summary")
  }
  if ((!is.data.frame(result$adjusted_descriptives) || nrow(result$adjusted_descriptives) == 0) && is.data.frame(result$descriptives) && nrow(result$descriptives) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Summary", result$descriptives, result$descriptives_note %||% "", used_sheets, title = "Group x time summary")
  }
  if (is.data.frame(result$anova) && nrow(result$anova) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "ANOVA", result$anova, result$method_note %||% "", used_sheets, title = "Repeated-measures ANOVA")
  }
  if (is.data.frame(result$mixed_model_overview) && nrow(result$mixed_model_overview) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Mixed model overview", result$mixed_model_overview, used_sheets, title = "Mixed-model alternative")
  }
  if (is.data.frame(result$mixed_model_coefficients) && nrow(result$mixed_model_coefficients) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Mixed model coefficients", result$mixed_model_coefficients, result$mixed_model_note %||% "", used_sheets, title = "Mixed-model coefficients")
  }
  if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Posthoc", result$posthoc, result$posthoc_note %||% "", used_sheets, title = "Post-hoc comparisons")
  }
  if (is.data.frame(result$assumption) && nrow(result$assumption) > 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Assumptions", result$assumption, used_sheets, title = "Assumption review")
  }
  if (is.data.frame(result$normality) && nrow(result$normality) > 0) {
    used_sheets <- add_ttest_anova_result_sheet(workbook, "Normality", result$normality, paste0("Normality method: ", attr(result$normality, "normality_method", exact = TRUE) %||% "Shapiro-Wilk", "."), used_sheets, title = "Normality review")
  }
  if (length(used_sheets) == 0) {
    used_sheets <- add_excel_table_sheet(workbook, "Results", data.frame(Message = "No data", stringsAsFactors = FALSE), used_sheets, title = "Results")
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}
