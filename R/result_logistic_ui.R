# Logistic regression result UI.

logistic_format_ci <- function(lower, upper) {
  sprintf("%s-%s", logistic_format_number(lower), logistic_format_number(upper))
}

logistic_format_number <- function(x, threshold = 1e6) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) {
    return("")
  }
  if (!is.finite(x)) {
    return(as.character(x))
  }
  if (abs(x) >= threshold) {
    return(formatC(x, format = "e", digits = 2))
  }
  format_decimal3(x)
}

logistic_format_ci_parenthetical <- function(lower, upper) {
  if (is.na(lower) || is.na(upper)) {
    return("")
  }
  sprintf("(%s~%s)", logistic_format_number(lower), logistic_format_number(upper))
}

logistic_result_notes <- function(result) {
  notes <- as.character(result$notes %||% character(0))
  if (isTRUE(result$ordinal_fallback)) {
    notes <- c(notes, "The proportional odds assumption was not met; multinomial logistic regression was fitted instead.")
  }
  instability_note <- logistic_instability_note(result)
  if (nzchar(instability_note)) {
    notes <- c(notes, instability_note)
  }
  unique(notes[!is.na(notes) & nzchar(notes)])
}

logistic_instability_note <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return("")
  }
  numeric_column <- function(name) suppressWarnings(as.numeric(table[[name]] %||% NA_real_))
  large_or <- any(abs(numeric_column("OR")) >= 1e6, na.rm = TRUE)
  large_ci <- any(abs(c(numeric_column("LLCI"), numeric_column("ULCI"))) >= 1e6, na.rm = TRUE)
  large_se <- any(abs(numeric_column("SE")) >= 10, na.rm = TRUE)
  vif_values <- suppressWarnings(as.numeric(result$predictor_vif %||% NA_real_))
  large_vif <- any(vif_values >= 10, na.rm = TRUE)
  if (!large_or && !large_ci && !large_se && !large_vif) {
    return("")
  }
  "Very large OR, confidence interval, SE, or VIF indicates an unstable logistic model, commonly caused by sparse cells, quasi/complete separation, or multicollinearity. Interpret these coefficients with caution and consider collapsing categories, reducing predictors, or using penalized/exact logistic regression."
}

logistic_term_candidates <- function(variable, level = "") {
  unique(c(
    variable,
    make.names(variable),
    paste0(variable, level),
    paste0(make.names(variable), make.names(level)),
    paste0(variable, make.names(level))
  ))
}

logistic_lookup_coefficient <- function(table, outcome, candidates) {
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  terms <- as.character(table$Term)
  keep <- terms %in% candidates
  if ("Outcome" %in% names(table) && any(nzchar(as.character(table$Outcome)))) {
    keep <- keep & as.character(table$Outcome) == as.character(outcome %||% "")
  }
  index <- which(keep)
  if (length(index) == 0) return(NULL)
  table[index[[1]], , drop = FALSE]
}

logistic_cell <- function(value = "") {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) "" else as.character(value[[1]])
}

logistic_header_style <- function(first = FALSE, bottom = TRUE, center = FALSE, compact = FALSE) {
  paste0(
    result_header_cell_style(first, compact = compact, compact_width = 56, compact_first_width = 112),
    "width:auto;min-width:0;max-width:none;",
    if (!isTRUE(bottom)) "border-bottom:0;" else "",
    if (isTRUE(center)) "text-align:center;" else ""
  )
}

logistic_body_style <- function(first = FALSE, last = FALSE, center = FALSE, compact = FALSE) {
  paste0(
    result_body_cell_style(first, last, compact = compact, compact_width = 56, compact_first_width = 112),
    "width:auto;min-width:0;max-width:none;",
    if (isTRUE(center)) "text-align:center;" else ""
  )
}

logistic_body_separator_style <- function(first = FALSE, last = FALSE, center = FALSE, separator = FALSE, compact = FALSE) {
  paste0(
    logistic_body_style(first, last, center, compact = compact),
    if (isTRUE(separator)) "border-top:2px solid #1f2937;" else ""
  )
}

logistic_fit_body_style <- function(last = FALSE, top = "") {
  paste0(
    "padding:7px 10px;",
    "border-left:0;",
    "border-right:0;",
    "border-bottom:", if (isTRUE(last)) "0" else "1px solid #d7dde5", ";",
    "text-align:center;",
    "font-weight:500;",
    "line-height:1.45;",
    "white-space:normal;",
    "min-width:0;",
    "overflow-wrap:normal;",
    top
  )
}

logistic_coef_cells <- function(row, show_b = FALSE, show_se = FALSE, split_ci = TRUE, reference = FALSE) {
  cells <- character(0)
  if (isTRUE(show_b)) {
    cells <- c(cells, if (isTRUE(reference)) "reference" else logistic_format_number(row$B))
  }
  if (isTRUE(show_se)) {
    cells <- c(cells, if (isTRUE(reference)) "" else logistic_format_number(row$SE))
  }
  cells <- c(
    cells,
    if (isTRUE(reference)) {
      if (isTRUE(show_b)) "" else "reference"
    } else {
      logistic_format_number(row$OR)
    }
  )
  if (isTRUE(split_ci)) {
    cells <- c(
      cells,
      if (isTRUE(reference)) "" else logistic_format_number(row$LLCI),
      if (isTRUE(reference)) "" else logistic_format_number(row$ULCI)
    )
  } else {
    cells <- c(cells, if (isTRUE(reference)) "" else logistic_format_ci_parenthetical(row$LLCI, row$ULCI))
  }
  cells <- c(
    cells,
    if (isTRUE(reference)) "" else format_p(row$p)
  )
  cells
}

logistic_vif_cell <- function(result, predictor) {
  values <- result$predictor_vif %||% numeric(0)
  value <- suppressWarnings(as.numeric(named_value(values, predictor, NA_real_)))
  if (is.na(value)) "" else logistic_format_number(value)
}

logistic_value_label <- function(variable, value, category_table = NULL) {
  value <- as.character(value %||% "")
  value_labels <- category_value_label_lookup_static(category_table)
  labels <- value_labels[[variable]]
  if (is.null(labels) || length(labels) == 0) {
    return(value)
  }
  label <- named_value(labels, value, "")
  if (nzchar(label)) label else value
}

logistic_binary_event_reference_label <- function(result, category_table = NULL) {
  dependent <- as.character(result$dependent %||% "")
  if (!nzchar(dependent)) {
    return("")
  }
  levels <- result$dependent_levels %||% character(0)
  levels <- as.character(levels)
  if (length(levels) < 2) {
    return("")
  }
  reference <- levels[[1]]
  event <- levels[[2]]
  sprintf(
    "%s vs %s",
    logistic_value_label(dependent, event, category_table),
    logistic_value_label(dependent, reference, category_table)
  )
}

logistic_outcome_comparison_label <- function(result, outcome, category_table = NULL) {
  dependent <- as.character(result$dependent %||% "")
  levels <- as.character(result$dependent_levels %||% character(0))
  outcome <- as.character(outcome %||% "")
  if (!nzchar(dependent) || length(levels) < 2 || !nzchar(outcome)) {
    return(sprintf("Outcome: %s", outcome))
  }
  reference <- levels[[1]]
  sprintf(
    "(%s vs %s)",
    logistic_value_label(dependent, outcome, category_table),
    logistic_value_label(dependent, reference, category_table)
  )
}

logistic_dependent_title_label <- function(name, variable_table = NULL, labels = character(0), category_table = NULL) {
  label <- display_variable_name_static(name, variable_table, labels, label_only = TRUE)
  if (!identical(label, name) || !is.data.frame(category_table) || !all(c("name", "var_label") %in% names(category_table))) {
    return(label)
  }
  row_index <- match(as.character(name), as.character(category_table$name))
  if (is.na(row_index)) {
    return(label)
  }
  category_label <- trimws(as.character(category_table$var_label[[row_index]] %||% ""))
  if (nzchar(category_label)) category_label else label
}

logistic_intercept_rows <- function(table, outcome, show_b = FALSE, show_se = FALSE, split_ci = TRUE) {
  if (!isTRUE(show_b) && !isTRUE(show_se)) {
    return(list())
  }
  coef <- logistic_lookup_coefficient(table, outcome, "(Intercept)")
  if (is.null(coef)) {
    return(list())
  }
  list(list(type = "coef", values = c("Constant", "", logistic_coef_cells(coef, show_b, show_se, split_ci), "")))
}

logistic_method_label <- function(result) {
  method <- as.character(result$method %||% "")
  if (identical(method, "Ordinal logistic regression")) {
    return("Ordinal logistic regression (cumulative logit)")
  }
  method
}

logistic_coefficient_rows <- function(result, variable_table = NULL, labels = character(0), category_table = NULL, show_b = FALSE, show_se = FALSE, split_ci = TRUE) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) return(list())
  coefficient_width <- length(logistic_coef_headers(show_b, show_se, split_ci))
  blank_coefficient_values <- function() rep("", coefficient_width)
  row_values <- function(first, second, coefficient_values = blank_coefficient_values(), vif = "") {
    values <- c(first, second, coefficient_values)
    length(values) <- 2L + coefficient_width
    values[is.na(values)] <- ""
    values
  }
  outcomes <- if ("Outcome" %in% names(table) && any(nzchar(as.character(table$Outcome)))) unique(as.character(table$Outcome)) else ""
  rows <- list()
  for (outcome in outcomes) {
    if (nzchar(outcome)) {
      rows[[length(rows) + 1L]] <- list(type = "outcome", values = row_values(logistic_outcome_comparison_label(result, outcome, category_table), ""))
    }
    rows <- c(rows, logistic_intercept_rows(table, outcome, show_b, show_se, split_ci))
    for (predictor in result$predictors) {
      predictor_label <- display_variable_name_static(predictor, variable_table, labels, label_only = TRUE)
      vif_cell <- logistic_vif_cell(result, predictor)
      levels <- as.character(result$predictor_levels[[predictor]] %||% character(0))
      if (length(levels) > 0) {
        ref_label <- logistic_value_label(predictor, levels[[1]], category_table)
        rows[[length(rows) + 1L]] <- list(
          type = "coef",
          values = row_values(
            predictor_label,
            ref_label,
            c(logistic_coef_cells(data.frame(B = NA, SE = NA, OR = NA, LLCI = NA, ULCI = NA, p = NA), show_b, show_se, split_ci, reference = TRUE), vif_cell)
          )
        )
        for (level in levels[-1]) {
          coef <- logistic_lookup_coefficient(table, outcome, logistic_term_candidates(predictor, level))
          if (is.null(coef)) next
          value_label <- logistic_value_label(predictor, level, category_table)
          rows[[length(rows) + 1L]] <- list(
            type = "coef",
            values = row_values("", value_label, c(logistic_coef_cells(coef, show_b, show_se, split_ci), vif_cell))
          )
        }
      } else {
        coef <- logistic_lookup_coefficient(table, outcome, logistic_term_candidates(predictor))
        if (is.null(coef)) next
        rows[[length(rows) + 1L]] <- list(
          type = "coef",
          values = row_values(predictor_label, "", c(logistic_coef_cells(coef, show_b, show_se, split_ci), vif_cell))
        )
      }
    }
  }
  if (length(rows) == 0) {
    remaining <- table[as.character(table$Term) != "(Intercept)", , drop = FALSE]
    for (index in seq_len(nrow(remaining))) {
      rows[[length(rows) + 1L]] <- list(
        type = "coef",
        values = row_values(as.character(remaining$Term[[index]]), "", c(logistic_coef_cells(remaining[index, , drop = FALSE], show_b, show_se, split_ci), ""))
      )
    }
  }
  rows
}

logistic_sup <- function(text, sup) {
  tags$span(style = "white-space:nowrap;", text, tags$sup(sup))
}

logistic_x2_label <- function(prefix = "") {
  tags$span(style = "white-space:nowrap;", prefix, "x", tags$sup("2"), "(p)")
}

logistic_stat_line <- function(label, value) {
  tags$span(style = "display:inline-block;white-space:nowrap;", label, " = ", value)
}

logistic_fit_rows <- function(result, show_mcfadden = FALSE, show_cox_snell = FALSE) {
  if (isTRUE(result$excluded) || is.null(result$fit)) {
    return(list(list(type = "fit", values = list(logistic_stat_line("Status", result$method %||% "Excluded")))))
  }
  rows <- list(
    list(type = "fit", values = list(logistic_stat_line(logistic_x2_label(), sprintf("%s (%s)", format_decimal3(result$fit$chisq), format_p(result$fit$p)))))
  )
  r2_values <- c(sprintf("Nagelkerke R%s = %s", "\u00b2", format_decimal3(result$fit$r2[["nagelkerke"]])))
  if (isTRUE(show_mcfadden) || isTRUE(show_cox_snell)) {
    r2_values <- c(
      r2_values,
      sprintf("McFadden R%s = %s", "\u00b2", format_decimal3(result$fit$r2[["mcfadden"]])),
      sprintf("Cox & Snell R%s = %s", "\u00b2", format_decimal3(result$fit$r2[["cox_snell"]]))
    )
  }
  rows[[length(rows) + 1L]] <- list(type = "fit", values = list(tags$span(style = "display:inline-block;white-space:normal;", paste(r2_values, collapse = "; "))))
  if (!is.null(result$delta_r2)) {
    rows[[length(rows) + 1L]] <- list(type = "fit", values = list(logistic_stat_line(logistic_sup("Delta R", "2"), format_decimal3(result$delta_r2))))
    rows[[length(rows) + 1L]] <- list(type = "fit", values = list(logistic_stat_line(logistic_x2_label("\u0394"), sprintf("%s (%s)", format_decimal3(result$delta_chisq), format_p(result$delta_p)))))
  }
  rows[[length(rows) + 1L]] <- list(type = "fit", values = list(tags$span(style = "display:inline-block;white-space:normal;", sprintf("AIC=%s, BIC=%s", format_decimal3(result$fit$aic), format_decimal3(result$fit$bic)))))
  if (!is.null(result$parallel)) {
    rows[[length(rows) + 1L]] <- list(type = "fit", values = list(logistic_stat_line(tagList("Parallel lines ", logistic_x2_label()), sprintf("%s (%s)", format_decimal3(result$parallel$chisq), format_p(result$parallel$p)))))
  }
  rows
}

logistic_coef_headers <- function(show_b = FALSE, show_se = FALSE, split_ci = TRUE) {
  headers <- c()
  if (isTRUE(show_b)) headers <- c(headers, "B")
  if (isTRUE(show_se)) headers <- c(headers, "SE")
  c(headers, "OR", if (isTRUE(split_ci)) c("LLCI", "ULCI") else "95% CI", "p", "VIF")
}

logistic_single_colgroup <- function(headers) {
  keys <- result_column_key(headers)
  widths <- rep(8, length(headers))
  widths[seq_along(headers) == 1L] <- 19
  widths[seq_along(headers) == 2L] <- 18
  widths[keys == "b"] <- 9
  widths[keys == "se"] <- 9
  widths[keys == "or"] <- 12
  widths[keys %in% c("95ci")] <- 22
  widths[keys %in% c("llci", "ulci")] <- 12
  widths[keys == "p"] <- 9
  widths[keys == "vif"] <- 9
  widths <- widths / sum(widths, na.rm = TRUE) * 100
  tags$colgroup(lapply(widths, function(width) {
    tags$col(style = sprintf("width:%.4f%%;", width))
  }))
}

logistic_hierarchical_column_widths <- function(headers) {
  keys <- result_column_key(headers)
  widths <- rep(8, length(headers))
  widths[keys == "b"] <- 8
  widths[keys == "se"] <- 8
  widths[keys == "or"] <- 11
  widths[keys == "95ci"] <- 24
  widths[keys %in% c("llci", "ulci")] <- 12
  widths[keys == "p"] <- 10
  widths[keys == "vif"] <- 9
  widths
}

logistic_hierarchical_colgroup <- function(model_columns, fit_width = FALSE) {
  if (!is.list(model_columns) || length(model_columns) == 0) {
    model_columns <- list(character(0))
  }
  widths <- c(90, 90)
  if (isTRUE(fit_width)) {
    widths <- c(70, 58)
  }
  for (index in seq_along(model_columns)) {
    model_widths <- logistic_hierarchical_column_widths(model_columns[[index]])
    if (isTRUE(fit_width)) {
      keys <- result_column_key(model_columns[[index]])
      model_widths <- rep(34, length(model_columns[[index]]))
      model_widths[keys == "b"] <- 48
      model_widths[keys == "se"] <- 27
      model_widths[keys == "or"] <- 38
      model_widths[keys == "95ci"] <- 64
      model_widths[keys %in% c("llci", "ulci")] <- 34
      model_widths[keys == "p"] <- 24
      model_widths[keys == "vif"] <- 26
    }
    widths <- c(widths, model_widths)
    if (index < length(model_columns)) {
      widths <- c(widths, if (isTRUE(fit_width)) 4 else 10)
    }
  }
  if (isTRUE(fit_width)) {
    percent_widths <- widths / sum(widths, na.rm = TRUE) * 100
    return(tags$colgroup(lapply(percent_widths, function(width) {
      tags$col(style = sprintf("width:%.4f%%;", width))
    })))
  }
  tags$colgroup(lapply(widths, function(width) {
    tags$col(style = sprintf("width:%dpx;", as.integer(width * 7)))
  }))
}

logistic_result_html_table <- function(result, variable_table = NULL, labels = character(0), category_table = NULL, show_b = FALSE, show_se = FALSE, show_mcfadden = FALSE, show_cox_snell = FALSE, split_ci = TRUE) {
  coef_headers <- logistic_coef_headers(show_b, show_se, split_ci)
  headers <- c("", "", coef_headers)
  rows <- c(
    logistic_coefficient_rows(result, variable_table, labels, category_table, show_b, show_se, split_ci),
    logistic_fit_rows(result, show_mcfadden, show_cox_snell)
  )
  tags$table(
    class = "coefficient-table logistic-result-table",
    style = paste0(result_table_style(font_size = 12, min_width = 0), "width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;"),
    logistic_single_colgroup(headers),
    tags$thead(
      if (isTRUE(split_ci)) {
        tagList(
          tags$tr(
            tags$th(style = logistic_header_style(TRUE, bottom = FALSE), ""),
            tags$th(style = logistic_header_style(FALSE, bottom = FALSE), ""),
            if (isTRUE(show_b)) tags$th(style = logistic_header_style(FALSE, bottom = FALSE), ""),
            if (isTRUE(show_se)) tags$th(style = logistic_header_style(FALSE, bottom = FALSE), ""),
            tags$th(style = logistic_header_style(FALSE, bottom = FALSE), ""),
            tags$th(style = logistic_header_style(FALSE, bottom = TRUE, center = TRUE), colspan = 2, "95% CI"),
            tags$th(style = logistic_header_style(FALSE, bottom = FALSE), ""),
            tags$th(style = logistic_header_style(FALSE, bottom = FALSE), "")
          ),
          tags$tr(lapply(seq_along(headers), function(index) {
            tags$th(style = logistic_header_style(index == 1), headers[[index]])
          }))
        )
      } else {
        tags$tr(lapply(seq_along(headers), function(index) {
          tags$th(style = logistic_header_style(index == 1), headers[[index]])
        }))
      }
    ),
    tags$tbody(lapply(seq_along(rows), function(row_index) {
      row <- rows[[row_index]]
      if (identical(row$type, "fit")) {
        first_fit <- row_index == 1L || !identical(rows[[row_index - 1L]]$type, "fit")
        fit_top <- if (isTRUE(first_fit)) "border-top:2px solid #1f2937;" else "border-top:1px solid #d7dde5;"
        return(tags$tr(
          class = "coefficient-fit-row logistic-fit-row",
          tags$td(style = logistic_fit_body_style(row_index == length(rows), fit_top), colspan = length(headers), row$values[[1]])
        ))
      }
      values <- row$values
      length(values) <- length(headers)
      values[is.na(values)] <- ""
      separator <- identical(row$type, "outcome") && row_index > 1L
      tags$tr(lapply(seq_along(values), function(index) {
        tags$td(style = logistic_body_separator_style(index %in% c(1L, 2L), row_index == length(rows), separator = separator), values[[index]])
      }))
    }))
  )
}

logistic_model_label <- function(result, index = NULL) {
  label <- as.character(result$hierarchical_step %||% "")
  if (nzchar(label)) {
    return(label)
  }
  if (!is.null(index)) {
    return(sprintf("Model %s", index))
  }
  "Model"
}

logistic_result_title <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  dependent_label <- logistic_dependent_title_label(result$dependent, variable_table, labels, category_table)
  title <- sprintf("Logistic regression: %s", dependent_label)
  event_reference <- if (identical(result$method, "Binary logistic regression")) logistic_binary_event_reference_label(result, category_table) else ""
  if (nzchar(event_reference)) {
    title <- sprintf("Logistic regression: %s(%s)", dependent_label, event_reference)
  }
  title
}

logistic_fit_summary_values <- function(result, show_mcfadden = FALSE, show_cox_snell = FALSE) {
  if (isTRUE(result$excluded) || is.null(result$fit)) {
    return(list(status = result$method %||% "Excluded"))
  }
  r2_values <- c(sprintf("Nagelkerke R\u00b2 = %s", format_decimal3(result$fit$r2[["nagelkerke"]])))
  if (isTRUE(show_mcfadden) || isTRUE(show_cox_snell)) {
    r2_values <- c(
      r2_values,
      sprintf("McFadden R\u00b2 = %s", format_decimal3(result$fit$r2[["mcfadden"]])),
      sprintf("Cox & Snell R\u00b2 = %s", format_decimal3(result$fit$r2[["cox_snell"]]))
    )
  }
  r2_value <- if (length(r2_values) > 1L) {
    tagList(lapply(r2_values, function(value) tags$div(style = "white-space:nowrap;", value)))
  } else {
    r2_values[[1]]
  }
  values <- list(
    x2 = sprintf("x\u00b2(p) = %s (%s)", format_decimal3(result$fit$chisq), format_p(result$fit$p)),
    r2 = r2_value,
    delta_r2 = if (!is.null(result$delta_r2)) sprintf("\u0394R\u00b2 = %s", format_decimal3(result$delta_r2)) else "",
    delta_x2 = if (!is.null(result$delta_chisq)) sprintf("\u0394x\u00b2(p) = %s (%s)", format_decimal3(result$delta_chisq), format_p(result$delta_p)) else "",
    aic = sprintf("AIC=%s, BIC=%s", format_decimal3(result$fit$aic), format_decimal3(result$fit$bic)),
    parallel = if (!is.null(result$parallel)) sprintf("Parallel lines x\u00b2(p) = %s (%s)", format_decimal3(result$parallel$chisq), format_p(result$parallel$p)) else ""
  )
  keep <- vapply(values, function(value) {
    if (inherits(value, "shiny.tag") || inherits(value, "shiny.tag.list")) {
      return(TRUE)
    }
    nzchar(as.character(value %||% ""))
  }, logical(1))
  values[keep]
}

logistic_overview_column_label <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  dependent_label <- logistic_dependent_title_label(result$dependent, variable_table, labels, category_table)
  step <- as.character(result$hierarchical_step %||% "")
  if (nzchar(step)) {
    sprintf("%s %s", dependent_label, step)
  } else {
    dependent_label
  }
}

logistic_package_label <- function(result) {
  method <- as.character(result$method %||% "")
  packages <- c("stats")
  if (grepl("Ordinal", method)) {
    packages <- c(packages, "MASS")
  }
  if (grepl("Multinomial", method)) {
    packages <- c(packages, "nnet")
  }
  paste(vapply(unique(packages), package_version_label, character(1)), collapse = "; ")
}

logistic_model_overview_data_frame <- function(results, variable_table = NULL, labels = character(0), category_table = NULL) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  rows <- c("N", "\ubd84\uc11d", "\uc0ac\uc720")
  logistic_overview_reason <- function(result) {
    notes <- logistic_result_notes(result)
    parts <- character(0)
    if (!is.null(result$parallel)) {
      parts <- c(parts, if (!is.na(result$parallel$p) && result$parallel$p > .05) "\ube44\ub840\uc624\uc988 \ub9cc\uc871" else "\ube44\ub840\uc624\uc988 \ubd88\ub9cc\uc871")
    }
    if (nzchar(logistic_note_match(notes, "EPV|Sparse|Zero cell|Rare event"))) {
      parts <- c(parts, "EPV/sparse \uc8fc\uc758")
    }
    if (nzchar(logistic_note_match(notes, "separation"))) {
      parts <- c(parts, "\ubd84\ub9ac \uc8fc\uc758")
    }
    max_vif <- suppressWarnings(as.numeric(result$max_vif %||% NA_real_))
    if (!is.na(max_vif) && max_vif > 10) {
      parts <- c(parts, "\ub2e4\uc911\uacf5\uc120\uc131 \uc704\ud5d8")
    } else if (!is.na(max_vif) && max_vif > 5) {
      parts <- c(parts, "\ub2e4\uc911\uacf5\uc120\uc131 \uc8fc\uc758")
    }
    if (length(parts) == 0) {
      parts <- "\ud2b9\uc774 \uc0ac\ud56d \uc5c6\uc74c"
    }
    paste(parts[nzchar(parts)], collapse = "\n")
  }
  values <- lapply(results, function(result) {
    c(
      N = as.character(result$n),
      "\ubd84\uc11d" = as.character(result$method %||% ""),
      "\uc0ac\uc720" = logistic_overview_reason(result)
    )
  })
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(results)) {
    table[[logistic_overview_column_label(results[[index]], variable_table, labels, category_table)]] <- unname(values[[index]][rows])
  }
  table
}

logistic_note_match <- function(notes, pattern) {
  notes <- as.character(notes %||% character(0))
  matched <- notes[grepl(pattern, notes, ignore.case = TRUE)]
  paste(matched[nzchar(matched)], collapse = "\n")
}

logistic_assumption_note_match <- function(notes, pattern) {
  notes <- as.character(notes %||% character(0))
  matched <- notes[grepl(pattern, notes, ignore.case = TRUE)]
  matched <- matched[!grepl("^Very large OR", matched, ignore.case = TRUE)]
  paste(matched[nzchar(matched)], collapse = "\n")
}

logistic_instability_summary <- function(notes) {
  notes <- as.character(notes %||% character(0))
  if (any(grepl("^Very large OR", notes, ignore.case = TRUE))) {
    return("Very large OR/CI/SE/VIF; unstable model warning.")
  }
  ""
}

logistic_parallel_summary <- function(result) {
  if (is.null(result$parallel)) {
    return("")
  }
  p <- result$parallel$p
  decision <- if (!is.na(p) && p > .05) "\ube44\ub840\uc624\uc988 \uac00\uc815 \ub9cc\uc871" else "\ube44\ub840\uc624\uc988 \uac00\uc815 \ubd88\ub9cc\uc871"
  sprintf("x\u00b2=%s(%s)\n%s", format_decimal3(result$parallel$chisq), format_p(p), decision)
}

logistic_vif_summary_text <- function(result) {
  value <- suppressWarnings(as.numeric(result$max_vif %||% NA_real_))
  if (is.na(value)) {
    return("")
  }
  decision <- if (value <= 5) {
    "\ub2e4\uc911\uacf5\uc120\uc131 \ubb38\uc81c \uc5c6\uc74c"
  } else if (value <= 10) {
    "\ub2e4\uc911\uacf5\uc120\uc131 \uc8fc\uc758"
  } else {
    "\ub2e4\uc911\uacf5\uc120\uc131 \uc704\ud5d8"
  }
  sprintf("max VIF=%s\n%s", format_decimal3(value), decision)
}

logistic_assumption_review_data_frame <- function(results, variable_table = NULL, labels = character(0), category_table = NULL) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  rows <- c("\ube44\ub840\uc624\uc988", "EPV / sparse", "\ubd84\ub9ac", "VIF", "\ud328\ud0a4\uc9c0")
  values <- lapply(results, function(result) {
    notes <- logistic_result_notes(result)
    epv_sparse <- paste(c(
      logistic_assumption_note_match(notes, "EPV|Sparse|Zero cell|Rare event"),
      logistic_instability_summary(notes)
    ), collapse = "\n")
    c(
      "\ube44\ub840\uc624\uc988" = logistic_parallel_summary(result),
      "EPV / sparse" = epv_sparse,
      "\ubd84\ub9ac" = logistic_assumption_note_match(notes, "separation"),
      VIF = logistic_vif_summary_text(result),
      "\ud328\ud0a4\uc9c0" = logistic_package_label(result)
    )
  })
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(results)) {
    table[[logistic_overview_column_label(results[[index]], variable_table, labels, category_table)]] <- unname(values[[index]][rows])
  }
  table
}

logistic_model_overview_block <- function(results, variable_table = NULL, labels = character(0), category_table = NULL) {
  table <- logistic_model_overview_data_frame(results, variable_table, labels, category_table)
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  div(
    class = "regression-result-panel logistic-result-panel model-overview-panel",
    h3("Model overview"),
    model_overview_html_table(table)
  )
}

logistic_assumption_review_block <- function(results, variable_table = NULL, labels = character(0), category_table = NULL) {
  table <- logistic_assumption_review_data_frame(results, variable_table, labels, category_table)
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(NULL)
  }
  div(
    class = "regression-result-panel logistic-result-panel assumption-review-panel",
    h3("\uac00\uc815 \uac80\ud1a0"),
    model_overview_html_table(table)
  )
}

logistic_hierarchical_footer_row <- function(label, values, model_columns, first = FALSE) {
  cells <- list(tags$td(
    colspan = 2,
    style = paste0(
      "padding:7px 10px;line-height:1.35;border-left:0;border-right:0;",
      "border-top:", if (isTRUE(first)) "2px solid #1f2937" else "1px solid #d7dde5", ";",
      "border-bottom:0;text-align:left;font-weight:500;white-space:nowrap;"
    ),
    label
  ))
  for (index in seq_along(model_columns)) {
    cells <- c(cells, list(tags$td(
      colspan = length(model_columns[[index]]),
      style = paste0(
        "padding:7px 10px;line-height:1.35;border-left:0;border-right:0;",
        "border-top:", if (isTRUE(first)) "2px solid #1f2937" else "1px solid #d7dde5", ";",
        "border-bottom:0;text-align:center;font-weight:500;white-space:normal;"
      ),
      values[[index]] %||% ""
    )))
    if (index < length(model_columns)) {
      cells <- c(cells, list(hierarchical_separator_cell(
        border_top = if (isTRUE(first)) "2px solid #1f2937" else "1px solid #d7dde5",
        border_bottom = "0"
      )))
    }
  }
  do.call(tags$tr, cells)
}

logistic_hierarchical_result_table <- function(group, variable_table = NULL, labels = character(0), category_table = NULL, show_b = FALSE, show_se = FALSE, show_mcfadden = FALSE, show_cox_snell = FALSE, split_ci = TRUE) {
  coef_headers <- logistic_coef_headers(show_b, show_se, split_ci)
  model_columns <- lapply(seq_along(group), function(index) {
    if (index < length(group)) {
      coef_headers[coef_headers != "VIF"]
    } else {
      coef_headers
    }
  })
  fit_width <- length(group) <= 3L
  table_min_width <- 250 + sum(vapply(model_columns, function(headers) {
    sum(logistic_hierarchical_column_widths(headers)) * 7
  }, numeric(1))) + max(0, length(group) - 1L) * 10
  table_style <- if (isTRUE(fit_width)) {
    paste0(
      result_table_style(font_size = if (length(group) >= 3L) 9 else 10, min_width = 0),
      "width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;"
    )
  } else {
    sprintf(
      "%s width:auto; min-width:%dpx; table-layout:fixed;",
      result_table_style(font_size = 13),
      table_min_width
    )
  }
  model_rows <- lapply(group, logistic_coefficient_rows,
    variable_table = variable_table,
    labels = labels,
    category_table = category_table,
    show_b = show_b,
    show_se = show_se,
    split_ci = split_ci
  )
  row_key <- function(row, context = "") {
    paste(context, row$values[[1]] %||% "", row$values[[2]] %||% "", sep = "\r")
  }
  model_row_keys <- lapply(model_rows, function(rows) {
    context <- ""
    vapply(rows, function(row) {
      if (identical(row$type, "outcome")) {
        context <<- row$values[[1]] %||% ""
      }
      row_key(row, context)
    }, character(1))
  })
  row_keys <- unique(unlist(lapply(rev(model_rows), function(rows) {
    context <- ""
    vapply(rows, function(row) {
      if (identical(row$type, "outcome")) {
        context <<- row$values[[1]] %||% ""
      }
      row_key(row, context)
    }, character(1))
  }), use.names = FALSE))

  header_groups <- list(
    tags$th(rowspan = 2, style = logistic_header_style(TRUE, compact = TRUE), ""),
    tags$th(rowspan = 2, style = logistic_header_style(TRUE, compact = TRUE), "")
  )
  for (index in seq_along(group)) {
    header_groups <- c(header_groups, list(tags$th(
      class = "hierarchical-model-header",
      style = paste0(logistic_header_style(FALSE, center = TRUE, compact = TRUE), "border-top:2px solid #1f2937;"),
      colspan = length(model_columns[[index]]),
      logistic_model_label(group[[index]], index)
    )))
    if (index < length(group)) {
      header_groups <- c(header_groups, list(hierarchical_header_separator_cell()))
    }
  }

  sub_headers <- list()
  for (index in seq_along(group)) {
    sub_headers <- c(sub_headers, lapply(model_columns[[index]], function(column) {
      tags$th(style = logistic_header_style(FALSE, compact = TRUE), column)
    }))
    if (index < length(group)) {
      sub_headers <- c(sub_headers, list(hierarchical_header_separator_cell("hierarchical-model-subheader-separator")))
    }
  }

  body_rows <- lapply(seq_along(row_keys), function(row_index) {
    key <- row_keys[[row_index]]
    display_row <- NULL
    for (model_index in seq_along(model_rows)) {
      matched <- match(key, model_row_keys[[model_index]])
      if (!is.na(matched)) {
        display_row <- model_rows[[model_index]][[matched]]
        break
      }
    }
    display_values <- (display_row$values %||% c("", ""))[seq_len(2)]
    display_values[is.na(display_values)] <- ""
    is_last <- identical(row_index, length(row_keys))
    is_outcome_row <- any(vapply(seq_along(model_rows), function(model_index) {
      rows <- model_rows[[model_index]]
      matched <- match(key, model_row_keys[[model_index]])
      !is.na(matched) && identical(rows[[matched]]$type, "outcome")
    }, logical(1)))
    separator <- is_outcome_row && row_index > 1L
    cells <- list(
      tags$td(style = logistic_body_separator_style(TRUE, is_last, separator = separator, compact = TRUE), display_values[[1]]),
      tags$td(style = logistic_body_separator_style(TRUE, is_last, separator = separator, compact = TRUE), display_values[[2]])
    )
    for (model_index in seq_along(model_rows)) {
      rows <- model_rows[[model_index]]
      model_headers <- model_columns[[model_index]]
      match_index <- match(key, model_row_keys[[model_index]])
      if (is.na(match_index)) {
        cells <- c(cells, lapply(model_headers, function(column) tags$td(style = logistic_body_separator_style(FALSE, is_last, separator = separator, compact = TRUE), "")))
      } else {
        values <- rows[[match_index]]$values
        model_values <- values[seq.int(3L, length.out = length(coef_headers))]
        keep_indices <- match(model_headers, coef_headers)
        model_values <- model_values[keep_indices[!is.na(keep_indices)]]
        model_values[is.na(model_values)] <- ""
        cells <- c(cells, lapply(model_values, function(value) tags$td(style = logistic_body_separator_style(FALSE, is_last, separator = separator, compact = TRUE), value)))
      }
      if (model_index < length(model_rows)) {
        cells <- c(cells, list(hierarchical_separator_cell(
          border_top = if (isTRUE(separator)) "2px solid #1f2937" else "0"
        )))
      }
    }
    do.call(tags$tr, cells)
  })

  summaries <- lapply(group, logistic_fit_summary_values, show_mcfadden = show_mcfadden, show_cox_snell = show_cox_snell)
  footer_labels <- unique(unlist(lapply(summaries, names), use.names = FALSE))
  footer_label_text <- c(
    x2 = "x\u00b2(p)",
    r2 = "R\u00b2",
    delta_r2 = "\u0394R\u00b2",
    delta_x2 = "\u0394x\u00b2(p)",
    aic = "AIC, BIC",
    parallel = "Parallel lines x\u00b2(p)",
    status = "Status"
  )
  footer_rows <- lapply(seq_along(footer_labels), function(index) {
    label <- footer_labels[[index]]
    logistic_hierarchical_footer_row(
      footer_label_text[[label]] %||% label,
      lapply(summaries, function(summary) summary[[label]] %||% ""),
      model_columns,
      first = index == 1L
    )
  })

  table <- tags$table(
    class = paste(
      "coefficient-table hierarchical-coefficient-table logistic-hierarchical-table",
      if (isTRUE(fit_width)) "fit-width-hierarchical-table" else ""
    ),
    style = table_style,
    logistic_hierarchical_colgroup(model_columns, fit_width = fit_width),
    tags$thead(
      do.call(tags$tr, header_groups),
      do.call(tags$tr, sub_headers)
    ),
    tags$tbody(body_rows),
    tags$tfoot(footer_rows)
  )
  div(class = "result-table-with-note hierarchical-table-wrap", div(class = "hierarchical-table-scroll", table))
}

logistic_result_block <- function(result, variable_table = NULL, labels = character(0), category_table = NULL, show_b = FALSE, show_se = FALSE, show_mcfadden = FALSE, show_cox_snell = FALSE, split_ci = TRUE) {
  title <- logistic_result_title(result, variable_table, labels, category_table)
  if (!is.null(result$hierarchical_step_index) && result$hierarchical_step_index > 1L && !is.null(result$hierarchical_step) && nzchar(result$hierarchical_step)) {
    title <- sprintf("%s - %s", title, result$hierarchical_step)
  }
  div(
    class = "result-section regression-result-panel logistic-result-panel",
    h3(title),
    logistic_result_html_table(result, variable_table, labels, category_table, show_b, show_se, show_mcfadden, show_cox_snell, split_ci),
    div(logistic_method_label(result), class = "result-note logistic-result-method"),
    lapply(logistic_result_notes(result), function(note) {
      div(note, class = "result-note coefficient-warning")
    })
  )
}

logistic_hierarchical_result_block <- function(group, variable_table = NULL, labels = character(0), category_table = NULL, show_b = FALSE, show_se = FALSE, show_mcfadden = FALSE, show_cox_snell = FALSE, split_ci = TRUE) {
  first_result <- group[[1]]
  notes <- unique(unlist(lapply(group, logistic_result_notes), use.names = FALSE))
  methods <- unique(vapply(group, logistic_method_label, character(1)))
  div(
    class = paste(
      "result-section regression-result-panel logistic-result-panel logistic-hierarchical-result-panel",
      if (length(group) >= 3L) "landscape-table-panel" else ""
    ),
    h3(logistic_result_title(first_result, variable_table, labels, category_table)),
    logistic_hierarchical_result_table(group, variable_table, labels, category_table, show_b, show_se, show_mcfadden, show_cox_snell, split_ci),
    lapply(methods, function(method) div(method, class = "result-note logistic-result-method")),
    lapply(notes, function(note) div(note, class = "result-note coefficient-warning"))
  )
}

logistic_result_groups <- function(results) {
  if (!is.list(results) || length(results) == 0) {
    return(list())
  }
  keys <- vapply(results, function(result) as.character(result$dependent %||% ""), character(1))
  groups <- split(results, keys)
  lapply(groups, function(group) {
    order_index <- vapply(group, function(result) as.integer(result$hierarchical_step_index %||% 999L), integer(1))
    group[order(order_index)]
  })
}

logistic_results_panel <- function(results, variable_table = NULL, labels = character(0), category_table = NULL, show_b = FALSE, show_se = FALSE, show_mcfadden = FALSE, show_cox_snell = FALSE, split_ci = TRUE) {
  groups <- logistic_result_groups(results)
  warnings <- attr(results, "warnings")
  skipped <- attr(results, "skipped")
  div(
    class = "logistic-results",
    logistic_model_overview_block(results, variable_table, labels, category_table),
    lapply(groups, function(group) {
      if (length(group) > 1L) {
        logistic_hierarchical_result_block(group, variable_table, labels, category_table, show_b, show_se, show_mcfadden, show_cox_snell, split_ci)
      } else {
        logistic_result_block(group[[1]], variable_table, labels, category_table, show_b, show_se, show_mcfadden, show_cox_snell, split_ci)
      }
    }),
    logistic_assumption_review_block(results, variable_table, labels, category_table),
    analysis_diagnostics_section(
      warnings,
      skipped,
      title = "Warnings / skipped models",
      class = "regression-result-panel logistic-result-panel logistic-diagnostics-panel"
    )
  )
}
