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

structural_canvas_path_display_table <- function(table, path_label = "Path") {
  if (!is.data.frame(table) || !all(c("Outcome", "Predictor") %in% names(table))) return(table)
  path <- paste(table$Predictor, "→", table$Outcome)
  remaining <- table[, setdiff(names(table), c("Outcome", "Predictor")), drop = FALSE]
  cbind(stats::setNames(data.frame(path, stringsAsFactors = FALSE), path_label), remaining)
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

structural_canvas_effect_summary_html_table <- function(table, ci = FALSE, language = NULL) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  path_label <- if (identical(normalize_app_language(language), "ko")) "경로" else "Path"
  if (isTRUE(ci)) {
    required <- c("Outcome", "Predictor", "Direct beta 95% CI", "Direct CI source", "Indirect beta 95% CI", "Indirect CI source", "Total beta 95% CI", "Total CI source")
    if (!all(required %in% names(table))) return(structural_canvas_basic_html_table(table))
    body_values <- cbind(
      stats::setNames(data.frame(paste(table$Predictor, "→", table$Outcome), stringsAsFactors = FALSE), path_label),
      table[, c("Direct beta 95% CI", "Direct CI source", "Indirect beta 95% CI", "Indirect CI source", "Total beta 95% CI", "Total CI source"), drop = FALSE]
    )
    return(tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-effect-ci-table",
      tags$thead(
        tags$tr(
          tags$th(class = "structural-table-header-cell", rowspan = "2", path_label),
          tags$th(class = "structural-table-header-cell", colspan = "2", "Direct effect"),
          tags$th(class = "structural-table-header-cell", colspan = "2", "Indirect effect"),
          tags$th(class = "structural-table-header-cell", colspan = "2", "Total effect")
        ),
        tags$tr(
          tags$th(class = "structural-table-header-cell", "beta 95% CI"), tags$th(class = "structural-table-header-cell", if (identical(normalize_app_language(language), "ko")) "산출 근거" else "Source"),
          tags$th(class = "structural-table-header-cell", "beta 95% CI"), tags$th(class = "structural-table-header-cell", if (identical(normalize_app_language(language), "ko")) "산출 근거" else "Source"),
          tags$th(class = "structural-table-header-cell", "beta 95% CI"), tags$th(class = "structural-table-header-cell", if (identical(normalize_app_language(language), "ko")) "산출 근거" else "Source")
        )
      ),
      tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
        tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
      }))
    )))
  }
  required <- c("Outcome", "Predictor", "Direct beta", "Direct p", "Direct BH-adjusted p", "Indirect beta", "Indirect p", "Indirect BH-adjusted p", "Total beta", "Total p", "Total BH-adjusted p")
  if (!all(required %in% names(table))) return(structural_canvas_basic_html_table(table))
  body_values <- cbind(
    stats::setNames(data.frame(paste(table$Predictor, "→", table$Outcome), stringsAsFactors = FALSE), path_label),
    table[, required[-c(1L, 2L)], drop = FALSE]
  )
  tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-effect-summary-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", rowspan = "2", path_label),
        tags$th(class = "structural-table-header-cell", colspan = "3", "Direct effect"),
        tags$th(class = "structural-table-header-cell", colspan = "3", "Indirect effect"),
        tags$th(class = "structural-table-header-cell", colspan = "3", "Total effect")
      ),
      tags$tr(
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", "BH p"),
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", "BH p"),
        tags$th(class = "structural-table-header-cell", "beta"),
        tags$th(class = "structural-table-header-cell", "p"),
        tags$th(class = "structural-table-header-cell", "BH p")
      )
    ),
    tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
      tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
    }))
  ))
}

structural_canvas_specific_indirect_html_table <- function(table) {
  required <- c("Path", "B", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "p")
  if (!is.data.frame(table) || !nrow(table) || !all(required %in% names(table))) {
    return(structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-specific-indirect-table"))
  }
  body_values <- table[, required, drop = FALSE]
  tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-specific-indirect-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Path"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "B"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Boot SE"),
        tags$th(class = "structural-table-header-cell", colspan = "2", "Boot 95% CI"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "p")
      ),
      tags$tr(
        tags$th(class = "structural-table-header-cell", "Lower"),
        tags$th(class = "structural-table-header-cell", "Upper")
      )
    ),
    tags$tbody(lapply(seq_len(nrow(body_values)), function(index) {
      tags$tr(lapply(as.character(body_values[index, ]), structural_canvas_html_cell))
    }))
  ))
}

structural_canvas_pls_measurement_main_html_table <- function(table) {
  required <- c("Construct", "Construct type", "Indicator", "loading/weight", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "Boot t", "Boot p", "Boot BH-adjusted p", "Item VIF", "Mode")
  if (!is.data.frame(table) || !nrow(table) || !all(required %in% names(table))) {
    return(structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-pls-measurement-main-table"))
  }
  marker <- ifelse(table[["Construct type"]] == "Common factor", "†", ifelse(table[["Construct type"]] == "Composite", "‡", "¶"))
  body <- data.frame(
    Construct = paste0(table$Construct, marker), Indicator = table$Indicator,
    `loading/weight` = table[["loading/weight"]], `Boot SE` = table[["Boot SE"]],
    `Boot 95% CI lower` = table[["Boot 95% CI lower"]], `Boot 95% CI upper` = table[["Boot 95% CI upper"]],
    `Boot t` = table[["Boot t"]], `Boot p` = table[["Boot p"]], `Boot BH-adjusted p` = table[["Boot BH-adjusted p"]], VIF = table[["Item VIF"]],
    check.names = FALSE, stringsAsFactors = FALSE
  )
  body$Construct[duplicated(as.character(table$Construct))] <- ""
  tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-result-table structural-pls-measurement-main-table",
    tags$thead(
      tags$tr(
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Construct"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Indicator"),
        tags$th(class = "structural-table-header-cell structural-loading-weight-header", rowspan = "2", HTML("loading/<br>weight")),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Boot SE"),
        tags$th(class = "structural-table-header-cell", colspan = "2", "Boot 95% CI"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Boot t"),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "Boot p"),
        tags$th(class = "structural-table-header-cell structural-boot-bh-header", rowspan = "2", HTML("Boot BH<br>adj p")),
        tags$th(class = "structural-table-header-cell", rowspan = "2", "VIF")
      ),
      tags$tr(tags$th(class = "structural-table-header-cell", "Lower"), tags$th(class = "structural-table-header-cell", "Upper"))
    ),
    tags$tbody(lapply(seq_len(nrow(body)), function(index) tags$tr(lapply(as.character(body[index, ]), structural_canvas_html_cell))))
  ))
}

structural_canvas_effect_ci_source_note <- function(table, language = NULL) {
  source_columns <- c("Direct CI source", "Indirect CI source", "Total CI source")
  if (!is.data.frame(table) || !all(source_columns %in% names(table))) return(NULL)
  labels <- if (identical(normalize_app_language(language), "ko")) {
    c("Direct CI source" = "직접효과", "Indirect CI source" = "간접효과", "Total CI source" = "총효과")
  } else {
    c("Direct CI source" = "direct effects", "Indirect CI source" = "indirect effects", "Total CI source" = "total effects")
  }
  parts <- unlist(lapply(source_columns, function(column) {
    values <- unique(trimws(as.character(table[[column]] %||% "")))
    values <- values[nzchar(values)]
    if (!length(values)) return(character(0))
    paste0(labels[[column]], ": ", paste(values, collapse = "; "))
  }), use.names = FALSE)
  if (!length(parts)) return(NULL)
  prefix <- if (identical(normalize_app_language(language), "ko")) "주. 신뢰구간 산출 근거 - " else "Note. Confidence-interval sources - "
  tags$p(class = "structural-result-note structural-effect-ci-source-note", paste0(prefix, paste(parts, collapse = "; "), "."))
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
