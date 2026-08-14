# Shared structural canvas result table rendering helpers.

structural_canvas_numeric_display_cell <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(FALSE)
  grepl("^(?:[<>]=?)?-?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?(?:[%†‡¶*]+)?$", value)
}

structural_canvas_html_cell <- function(value, header = FALSE) {
  value <- as.character(value %||% "")
  if (isTRUE(header)) return(tags$th(value))
  if (structural_canvas_numeric_display_cell(value)) {
    return(tags$td(class = "text-center structural-numeric-cell", style = "text-align: center;", value))
  }
  tags$td(value)
}

structural_canvas_basic_html_table <- function(table, class = "table table-striped table-bordered") {
  if (!is.data.frame(table) || !nrow(table) || !ncol(table)) return(NULL)
  tags$div(class = "table-responsive", tags$table(class = class,
    tags$thead(tags$tr(lapply(names(table), structural_canvas_html_cell, header = TRUE))),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), structural_canvas_html_cell))))
  ))
}

structural_canvas_measurement_html_table <- function(table) {
  required <- c("Latent", "Indicator", "B", "B 95% CI lower", "B 95% CI upper", "SE", "beta", "R²", "z", "p")
  if (!is.data.frame(table) || !all(required %in% names(table))) {
    return(structural_canvas_basic_html_table(table))
  }
  body_values <- table[, required, drop = FALSE]
  tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-measurement-table",
    tags$thead(
      tags$tr(
        tags$th(rowspan = "2", "Latent"),
        tags$th(rowspan = "2", "Indicator"),
        tags$th(rowspan = "2", "B"),
        tags$th(colspan = "2", "B 95% CI"),
        tags$th(rowspan = "2", "SE"),
        tags$th(rowspan = "2", "beta"),
        tags$th(rowspan = "2", HTML("R<sup>2</sup>")),
        tags$th(rowspan = "2", "z"),
        tags$th(rowspan = "2", "p")
      ),
      tags$tr(tags$th("lower"), tags$th("upper"))
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
      HTML("B = unstandardized loading; CI = confidence interval; SE = standard error; beta = standardized loading; R<sup>2</sup> = coefficient of determination; z = z statistic; p = p value.")
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
