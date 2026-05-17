# Saved analysis HTML output.

saved_results_inline_css <- function(max_width = 1280) {
  paste(
    "body { background: #ffffff !important; color: #2f3a46; font-family: Arial, Helvetica, sans-serif; font-size: 16px; margin: 0; }",
    sprintf(".page-shell { max-width: %dpx; margin: 24px auto; padding: 0 18px; }", max_width),
    ".saved-results-meta { color: #52606d; margin: 4px 0 18px; font-size: 13px; }",
    ".regression-results { border-top: 0 !important; padding-top: 0 !important; }",
    ".regression-result-panel { background: #ffffff; border: 1px solid #d9e2ec; border-radius: 6px; padding: 18px 20px; margin-bottom: 22px; break-inside: avoid; page-break-inside: avoid; }",
    ".result-section.regression-result-panel { width: max-content; max-width: 100%; overflow-x: auto; box-sizing: border-box; }",
    ".regression-result-panel h3 { color: #15233a; font-size: 22px; font-weight: 700; margin: 0 0 12px; }",
    ".regression-result-panel table { width: auto; min-width: 440px; border-collapse: collapse !important; border-spacing: 0 !important; border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; color: #2f3a46; font-size: 16px; background: transparent; }",
    ".regression-result-panel table th, .regression-result-panel table td { padding: 9px 16px; line-height: 1.45; border-left: 0 !important; border-right: 0 !important; border-bottom: 1px solid #d7dde5; vertical-align: middle; background: transparent; }",
    ".regression-result-panel table thead th { border-bottom: 2px solid #1f2937 !important; font-weight: 700; }",
    ".regression-result-panel table tbody tr:last-child td, .regression-result-panel table tbody tr:last-child th { border-bottom: 0 !important; }",
    ".regression-result-panel table th:first-child, .regression-result-panel table td:first-child { text-align: left !important; }",
    ".regression-result-panel table th:not(:first-child), .regression-result-panel table td:not(:first-child) { text-align: right !important; }",
    ".coefficient-table { table-layout: auto; }",
    ".coefficient-table th:first-child, .coefficient-table td:first-child { text-align: left !important; }",
    ".coefficient-table th:not(:first-child) { text-align: right !important; }",
    ".coefficient-table thead th { border-top: 2px solid #1f2937 !important; border-bottom: 2px solid #1f2937 !important; font-weight: 700; }",
    ".coefficient-table tbody td:not(:first-child), .coefficient-table tfoot td:not(:first-child) { text-align: right !important; }",
    ".coefficient-table tfoot { border-bottom: 2px solid #1f2937 !important; }",
    ".coefficient-table .coefficient-fit-row td { text-align: center !important; border-top: 2px solid #1f2937 !important; border-bottom: 0 !important; font-weight: 500; }",
    ".hierarchical-table-wrap { display: block; max-width: 100%; }",
    ".hierarchical-table-scroll { max-width: 100%; overflow-x: auto; }",
    ".hierarchical-coefficient-table { border-top: 0 !important; table-layout: fixed !important; width: auto; }",
    ".hierarchical-coefficient-table thead tr:first-child th { border-top: 2px solid #1f2937 !important; border-bottom: 0 !important; }",
    ".hierarchical-coefficient-table thead tr:first-child th:first-child { border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table .hierarchical-model-header { text-align: center !important; font-weight: 700; border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table thead tr:last-child th { border-top: 0 !important; border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table thead tr:last-child th:not(.hierarchical-model-separator) { text-align: right !important; }",
    ".hierarchical-coefficient-table tbody td:not(:first-child):not(.hierarchical-model-separator) { text-align: right !important; }",
    ".hierarchical-coefficient-table thead tr:first-child th:first-child, .hierarchical-coefficient-table tbody td:first-child, .hierarchical-coefficient-table tfoot td:first-child { width: 240px; min-width: 240px; max-width: 240px; white-space: normal; overflow-wrap: break-word; word-break: keep-all; }",
    ".hierarchical-coefficient-table .hierarchical-term-col { width: 240px; }",
    ".hierarchical-coefficient-table .hierarchical-stat-col { width: 78px; }",
    ".hierarchical-coefficient-table .hierarchical-separator-col { width: 10px; }",
    ".hierarchical-coefficient-table thead tr:last-child th:not(.hierarchical-model-separator), .hierarchical-coefficient-table td:not(:first-child):not(.hierarchical-model-separator) { width: 78px; min-width: 78px; max-width: 78px; padding-left: 8px; padding-right: 8px; overflow-wrap: normal; white-space: nowrap; }",
    ".hierarchical-coefficient-table .hierarchical-model-separator { width: 10px; min-width: 10px; max-width: 10px; padding: 0 !important; background: transparent !important; }",
    ".hierarchical-coefficient-table .hierarchical-model-header-separator { border-bottom: 0 !important; }",
    ".hierarchical-coefficient-table .hierarchical-model-subheader-separator { border-bottom: 2px solid #1f2937 !important; }",
    ".hierarchical-coefficient-table tfoot .coefficient-fit-row td { border-top: 1px solid #d7dde5 !important; border-bottom: 0 !important; }",
    ".hierarchical-coefficient-table tfoot tr:first-child td { border-top: 2px solid #1f2937 !important; }",
    ".coefficient-note { color: #52606d; font-size: 11px; line-height: 1.4; margin-top: 4px; padding-top: 4px; text-align: left; white-space: normal; max-width: 100%; overflow-wrap: break-word; }",
    ".residual-diagnostic-plots img { display: block; width: 420px; height: 420px; }",
    ".frequency-plot-grid, .frequency-plot-row, .correlation-plot-grid { display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start; }",
    ".frequency-plot-card, .correlation-plot-card, .residual-plot-card { border: 1px solid #d9e2ec; border-radius: 6px; padding: 12px; background: #ffffff; }",
    ".frequency-plot-card h4, .correlation-plot-card h4, .residual-plot-card h4 { margin: 0 0 8px; font-size: 15px; color: #15233a; }",
    ".frequency-plot-card img { display: block; width: 420px; height: 320px; }",
    ".correlation-plot-card img { display: block; width: 720px; height: 520px; max-width: 100%; }",
    sep = "\n"
  )
}

saved_results_document <- function(title, content, max_width = 1280, css_path = file.path("www", "style.css")) {
  css <- if (file.exists(css_path)) paste(readLines(css_path, warn = FALSE), collapse = "\n") else ""
  document <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title(title),
      tags$style(htmltools::HTML(css)),
      tags$style(htmltools::HTML(saved_results_inline_css(max_width)))
    ),
    tags$body(
      div(
        class = "page-shell",
        h1(title),
        div(
          class = "saved-results-meta",
          sprintf("Saved: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        ),
        content
      )
    )
  )
  paste0("<!DOCTYPE html>\n", tags_to_html(document))
}

saved_analysis_results_html <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  css_path = file.path("www", "style.css")
) {
  saved_results_document(
    "easyflow_statistics Results",
    div(
      class = "regression-results",
      div(
        class = "regression-result-panel model-overview-panel",
        h3("Model overview"),
        model_overview_html_table(model_overview_data_frame(results, variable_table, labels))
      ),
      lapply(seq_along(results), function(index) {
        regression_coefficient_result_block(
          results[[index]],
          variable_table,
          labels,
          category_table,
          refs,
          value_labels,
          show_sr2,
          show_f2,
          show_vif
        )
      }),
      effect_size_reference_panel(show_sr2, show_f2),
      lapply(seq_along(results), function(index) {
        result <- results[[index]]
        dependent <- all.vars(result$formula)[[1]]
        dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
        saved_plot_result_block(result, dependent_label)
      }),
      regression_durbin_watson_result_block(results, variable_table, labels)
    )
    ,
    max_width = 1280,
    css_path = css_path
  )
}

saved_hierarchical_results_html <- function(
  results,
  variable_table = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  show_sr2 = FALSE,
  show_f2 = FALSE,
  show_vif = FALSE,
  css_path = file.path("www", "style.css")
) {
  saved_results_document(
    "easyflow_statistics Hierarchical Results",
    hierarchical_results_panel(
      results = results,
      variable_table = variable_table,
      labels = labels,
      category_table = category_table,
      refs = refs,
      value_labels = value_labels,
      show_sr2 = show_sr2,
      show_f2 = show_f2,
      show_vif = show_vif,
      plot_blocks = lapply(seq_along(results), function(index) {
        result <- results[[index]]
        dependent <- all.vars(result$formula)[[1]]
        dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
        saved_plot_result_block(result, dependent_label)
      })
    ),
    max_width = 1500,
    css_path = css_path
  )
}

saved_frequency_plot_blocks <- function(result, options) {
  plot_block <- function(type, name) {
    variable_label <- frequency_variable_display_name(name, result$variable_info, result$labels, result$category_table)
    title <- sprintf("%s(%s)", frequency_plot_label(type), variable_label)
    tags$div(
      class = "frequency-plot-card",
      tags$h4(title),
      tags$img(
        src = plot_data_uri(function(plot_result) draw_frequency_plot(plot_result, type, name), result, width = 420, height = 320),
        width = "420",
        height = "320",
        alt = title
      )
    )
  }
  blocks <- list()
  categorical <- as.character(result$categorical %||% character(0))
  continuous <- as.character(result$continuous %||% character(0))
  for (name in categorical) {
    if (isTRUE(options$pie)) blocks <- c(blocks, list(plot_block("pie", name)))
    if (isTRUE(options$bar)) blocks <- c(blocks, list(plot_block("bar", name)))
  }
  for (name in continuous) {
    if (isTRUE(options$histogram)) blocks <- c(blocks, list(plot_block("histogram", name)))
    if (isTRUE(options$box)) blocks <- c(blocks, list(plot_block("box", name)))
    if (isTRUE(options$violin)) blocks <- c(blocks, list(plot_block("violin", name)))
  }
  if (length(blocks) == 0) {
    return(NULL)
  }
  tags$div(
    class = "regression-result-panel frequency-plots-section",
    tags$h3("Plots"),
    tags$div(class = "frequency-plot-grid", blocks)
  )
}

saved_frequencies_results_html <- function(result, css_path = file.path("www", "style.css")) {
  options <- result$options %||% list(n_percent = TRUE, mean_sd = TRUE)
  table <- frequency_combined_table(result, options)
  saved_results_document(
    "easyflow_statistics Frequencies Results",
    tags$div(
      class = "regression-results",
      tags$div(
        class = "result-section frequencies-result-section regression-result-panel",
        tags$h3("Frequencies / Descriptives"),
        tags$div(class = "frequency-table-wrap", coefficient_html_table(table))
      ),
      saved_frequency_plot_blocks(result, options)
    ),
    max_width = 1500,
    css_path = css_path
  )
}

saved_reliability_results_html <- function(result, css_path = file.path("www", "style.css")) {
  saved_results_document(
    "easyflow_statistics Reliability Results",
    tags$div(class = "regression-results", reliability_results_ui(result)),
    max_width = 1500,
    css_path = css_path
  )
}

saved_ttest_anova_results_html <- function(result, css_path = file.path("www", "style.css")) {
  saved_results_document(
    "easyflow_statistics t-test / ANOVA Results",
    tags$div(class = "regression-results", ttest_anova_results_ui(result)),
    max_width = 1500,
    css_path = css_path
  )
}

saved_paired_results_html <- function(result, css_path = file.path("www", "style.css")) {
  saved_results_document(
    "easyflow_statistics Paired Test Results",
    tags$div(class = "regression-results", paired_results_ui(result)),
    max_width = 1500,
    css_path = css_path
  )
}

saved_correlation_results_html <- function(result, css_path = file.path("www", "style.css")) {
  options <- result$options %||% list()
  normality_table <- correlation_normality_display_table(result)
  saved_results_document(
    "easyflow_statistics Correlation Results",
    tags$div(
      class = "correlation-results regression-results",
      correlation_matrix_set_ui(result),
      if (is.list(result$latent)) {
        correlation_matrix_set_ui(result, source = result$latent, title_prefix = "Latent-variable ")
      },
      if (isTRUE(options$normality) && is.data.frame(normality_table) && nrow(normality_table) > 0) {
        tags$div(
          class = "result-section correlation-result-section regression-result-panel",
          tags$h3("Normality"),
          coefficient_html_table(normality_table)
        )
      },
      if (isTRUE(options$scatter_plot)) {
        tags$div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          tags$h3("Scatter plot matrix"),
          tags$div(
            class = "correlation-plot-card",
            tags$img(
              src = plot_data_uri(draw_correlation_scatter_plot, result, width = 1480, height = 1480),
              width = "1480",
              height = "1480",
              alt = "Scatter plot matrix"
            )
          )
        )
      },
      if (isTRUE(options$matrix_plot)) {
        tags$div(
          class = "result-section correlation-result-section correlation-plot-section regression-result-panel",
          tags$h3("Correlation matrix heatmap"),
          tags$div(
            class = "correlation-plot-card",
            tags$img(
              src = plot_data_uri(draw_correlation_heatmap, result, width = 1360, height = 1360),
              width = "1360",
              height = "1360",
              alt = "Correlation matrix heatmap"
            )
          )
        )
      }
    ),
    max_width = 1500,
    css_path = css_path
  )
}
