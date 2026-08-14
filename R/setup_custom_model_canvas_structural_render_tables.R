# Shared structural canvas result table rendering helpers.

structural_canvas_numeric_display_cell <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(FALSE)
  grepl("^(?:[<>]=?)?-?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?(?:[%†‡¶*]+)?$", value)
}

structural_canvas_html_cell <- function(value, header = FALSE) {
  value <- as.character(value %||% "")
  if (isTRUE(header)) return(tags$th(class = "structural-table-header-cell", value))
  if (grepl("^\\((?:N/A[†‡¶]?|(?:[<>]=?)?-?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?[†‡¶]?)\\)$", trimws(value))) {
    return(tags$td(class = "text-center structural-parenthetical-cell", value))
  }
  if (structural_canvas_numeric_display_cell(value)) {
    return(tags$td(class = "text-center structural-numeric-cell", tags$span(class = "structural-numeric-value", value)))
  }
  tags$td(value)
}

structural_canvas_basic_html_table <- function(table, class = "table table-striped table-bordered") {
  if (!is.data.frame(table) || !nrow(table) || !ncol(table)) return(NULL)
  table_class <- paste(unique(c(strsplit(class, "\\s+")[[1]], "structural-result-table")), collapse = " ")
  tags$div(class = "table-responsive", tags$table(class = table_class,
    tags$thead(tags$tr(lapply(names(table), structural_canvas_html_cell, header = TRUE))),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), structural_canvas_html_cell))))
  ))
}

structural_canvas_measurement_html_table <- function(table) {
  required <- c("Latent", "Indicator", "B", "SE", "beta", "z", "p", "R²")
  if (!is.data.frame(table) || !all(required %in% names(table))) {
    return(structural_canvas_basic_html_table(table))
  }
  body_values <- table[, required, drop = FALSE]
  tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-measurement-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", "Latent"),
        tags$th(class = "structural-table-header-cell", "Indicator"),
        tags$th(class = "structural-table-header-cell", "B"),
        tags$th(class = "structural-table-header-cell", "SE"),
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "z"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", HTML("R<sup>2</sup>"))
      )
    ),
    tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
      tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
    }))
  ))
}

structural_canvas_measurement_ci_html_table <- function(table) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  column_named <- function(pattern) {
    matches <- names(table)[grepl(pattern, names(table))]
    if (length(matches)) matches[[1L]] else ""
  }
  columns <- c(
    "Latent",
    "Indicator",
    "B 95% CI lower",
    "B 95% CI upper",
    "beta 95% CI lower",
    "beta 95% CI upper",
    column_named("^R.*95% CI lower$"),
    column_named("^R.*95% CI upper$")
  )
  if (any(!nzchar(columns)) || !all(columns %in% names(table))) {
    return(structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-measurement-ci-table"))
  }
  body_values <- table[, columns, drop = FALSE]
  tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-measurement-ci-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Latent"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Indicator"),
        tags$th(class = "structural-table-header-cell", colspan = "2", "B 95% CI"),
        tags$th(class = "structural-table-header-cell", colspan = "2", "beta 95% CI"),
        tags$th(class = "structural-table-header-cell", colspan = "2", HTML("R<sup>2</sup> 95% CI"))
      ),
      tags$tr(
        tags$th(class = "structural-table-header-cell", "lower"),
        tags$th(class = "structural-table-header-cell", "upper"),
        tags$th(class = "structural-table-header-cell", "lower"),
        tags$th(class = "structural-table-header-cell", "upper"),
        tags$th(class = "structural-table-header-cell", "lower"),
        tags$th(class = "structural-table-header-cell", "upper")
      )
    ),
    tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
      tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
    }))
  ))
}

structural_canvas_effect_summary_html_table <- function(table, ci = FALSE) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  if (isTRUE(ci)) {
    required <- c("Outcome", "Predictor", "Direct beta 95% CI", "Indirect beta 95% CI", "Total beta 95% CI")
    if (!all(required %in% names(table))) return(structural_canvas_basic_html_table(table))
    body_values <- table[, required, drop = FALSE]
    return(tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-effect-ci-table",
      tags$thead(
        tags$tr(
          tags$th(class = "structural-table-header-cell", rowspan = "2", "Outcome"),
          tags$th(class = "structural-table-header-cell", rowspan = "2", "Predictor"),
          tags$th(class = "structural-table-header-cell", colspan = "3", "beta 95% CI")
        ),
        tags$tr(
          tags$th(class = "structural-table-header-cell", "Direct effect"),
          tags$th(class = "structural-table-header-cell", "Indirect effect"),
          tags$th(class = "structural-table-header-cell", "Total effect")
        )
      ),
      tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
        tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
      }))
    )))
  }
  required <- c("Outcome", "Predictor", "Direct beta", "Direct p", "Indirect beta", "Indirect p", "Total beta", "Total p")
  if (!all(required %in% names(table))) return(structural_canvas_basic_html_table(table))
  body_values <- table[, required, drop = FALSE]
  tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-effect-summary-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Outcome"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Predictor"),
        tags$th(class = "structural-table-header-cell", colspan = "2", "Direct effect"),
        tags$th(class = "structural-table-header-cell", colspan = "2", "Indirect effect"),
        tags$th(class = "structural-table-header-cell", colspan = "2", "Total effect")
      ),
      tags$tr(
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p")
      )
    ),
    tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
      tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
    }))
  ))
}

structural_canvas_abbreviation_footnotes <- function(table, context = "general") {
  if (identical(context, "fit")) {
    return(tags$p(
      class = "structural-result-note",
      HTML("&chi;<sup>2</sup> = chi-square; df = degrees of freedom; CFI = Comparative Fit Index; TLI = Tucker-Lewis Index; SRMR = Standardized Root Mean Square Residual; RMSEA = Root Mean Square Error of Approximation; CI = confidence interval; LLCI/ULCI = lower/upper limit of the confidence interval.")
    ))
  }
  if (identical(context, "validity")) {
    return(tags$p(
      class = "structural-result-note",
      HTML("AVE = Average Variance Extracted; CR = Composite Reliability; &alpha; = Cronbach's alpha; &omega; total = McDonald's omega total.")
    ))
  }
  if (identical(context, "measurement")) {
    return(tags$p(
      class = "structural-result-note",
      HTML("B = unstandardized loading; SE = standard error; beta = standardized loading; R<sup>2</sup> = coefficient of determination; z = z statistic; p = p value.")
    ))
  }
  NULL
}

structural_canvas_symbol_footnotes <- function(table) {
  values <- as.character(unlist(table, use.names = FALSE))
  values <- values[!is.na(values)]
  notes <- list()
  if (any(grepl("\\*", values))) {
    notes <- c(notes, list(tags$p(class = "structural-result-note", "* Fixed reference loading.")))
  }
  if (any(grepl("\u2020", values, fixed = TRUE))) {
    notes <- c(notes, list(tags$p(class = "structural-result-note", "\u2020 Coefficient is outside the conventional admissible range; interpret cautiously.")))
  }
  if (any(grepl("\u2021", values, fixed = TRUE))) {
    notes <- c(notes, list(tags$p(class = "structural-result-note", "\u2021 Single-indicator construct without a constrained error variance; reliability and AVE are not estimated.")))
  }
  if (any(grepl("\u00b6", values, fixed = TRUE))) {
    notes <- c(notes, list(tags$p(class = "structural-result-note", "\u00b6 Single-indicator construct with constrained error variance; interpret reliability and validity descriptively.")))
  }
  tagList(notes)
}
