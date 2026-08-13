# Structural core fit result rendering.

structural_canvas_fit_table_result_ui <- function(bundle, values) {
  shiny::req(nrow(values) > 0)
  if (!inherits(bundle$fit, "lavaan")) {
    return(tagList(
      tags$table(
        class = "table table-striped table-bordered structural-fit-table",
        tags$thead(tags$tr(lapply(names(values), tags$th))),
        tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, , drop = TRUE]), tags$td))))
      ),
      tags$p(class = "structural-result-note", "PLS-SEM reports path coefficients and explained variance (R^2/adjusted R^2) rather than covariance-based global fit indices.")
    ))
  }
  ci_percent <- round(100 * as.numeric(bundle$rmsea_ci %||% .90))
  comparison_fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, bundle$fit) else list(bundle$fit)
  fit_selections <- structural_canvas_common_fit_measures(comparison_fits, bundle$estimator %||% "ML", bundle$rmsea_ci %||% .90)
  selection <- fit_selections[[length(fit_selections)]]
  fit_labels <- selection$labels
  baseline_selection <- if (length(fit_selections) > 1L) fit_selections[[1L]] else NULL
  same_measure_keys <- is.null(baseline_selection) || identical(selection$keys, baseline_selection$keys)
  if (!same_measure_keys) fit_labels[c(5L, 6L, 8L)] <- c("Adjusted CFI", "Adjusted TLI", "Adjusted RMSEA")
  tagList(tags$table(
    class = "table table-striped table-bordered structural-fit-table",
    tags$thead(
      tags$tr(
        tags$th(rowspan = "2", "Model"),
        tags$th(rowspan = "2", if (!same_measure_keys) HTML("Adjusted &chi;<sup>2</sup>*") else if (grepl("Scaled", fit_labels[[1L]], fixed = TRUE)) HTML("Scaled &chi;<sup>2</sup>*") else HTML("&chi;<sup>2</sup>")), tags$th(rowspan = "2", "df"), tags$th(rowspan = "2", "p"), tags$th(rowspan = "2", "Q"),
        tags$th(rowspan = "2", paste0(fit_labels[[5L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", paste0(fit_labels[[6L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", "SRMR"), tags$th(rowspan = "2", paste0(fit_labels[[8L]], if (selection$adjusted) "*" else "")),
        tags$th(colspan = "2", paste0(ci_percent, "% CI"))
      ),
      tags$tr(tags$th("LLCI"), tags$th("ULCI"))
    ),
    tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, , drop = TRUE]), tags$td))))
  ), if (selection$adjusted)
    tags$p(class = "structural-result-note", if (is.null(baseline_selection) || same_measure_keys) {
      paste0("* Reported lavaan measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
    } else {
      paste0("* Original-model measures: ", paste(unname(baseline_selection$keys), collapse = ", "), "; modified-model measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
    }),
    if ((!is.null(baseline_selection) && baseline_selection$values[[2L]] == 0) || selection$values[[2L]] == 0)
      tags$p(class = "structural-result-note", "Q (chi-square/df) and some fit indices are not interpretable for a saturated model with df = 0.")
  )

}

structural_canvas_identification_result_ui <- function(bundle) {
  issues <- bundle$identification %||% data.frame()
  if (!nrow(issues)) return(tags$p(class = "structural-result-note", "Pre-fit structural identification check: no rule-based issues detected."))
  tags$div(class = "structural-identification-result",
    tags$h5("Pre-fit identification diagnostics"),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(issues), tags$th))),
      tags$tbody(lapply(seq_len(nrow(issues)), function(index) tags$tr(lapply(as.character(issues[index, ]), tags$td))))
    ),
    tags$p(class = "structural-result-note", "This rule-based screen does not prove mathematical identification; lavaan estimation, degrees of freedom, information-matrix checks, and solution admissibility remain decisive.")
  )

}

structural_canvas_fit_difference_result_ui <- function(bundle, language) {
  if (!identical(bundle$comparison_type %||% "", "mi") || is.null(bundle$baseline_fit)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  report <- structural_canvas_model_difference_report(bundle)
  if (!nrow(report) || !isTRUE(report$Available[[1L]])) {
    reason <- if (nrow(report)) as.character(report$Reason[[1L]]) else "eligibility was not established"
    return(tags$p(class = "structural-result-note", if (ko) paste0("공식 모형 차이 검정을 표시하지 않았습니다: ", reason) else paste0("A formal model-difference test was suppressed: ", reason)))
  }
  tags$div(
    class = "structural-fit-difference",
    tags$h5(if (ko) "기존 모형 vs 탐색적 수정 모형" else "Original vs exploratory modified model"),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th("Δχ²"), tags$th("Δdf"), tags$th("p"))),
      tags$tbody(tags$tr(
        tags$td(format_decimal3(report$`Delta chi-square`[[1L]])),
        tags$td(format_decimal3(report$`Delta df`[[1L]])),
        tags$td(format_p(report$p[[1L]]))
      ))
    ),
    tags$p(class = "structural-result-note", report$Method[[1L]]),
    tags$p(class = "structural-result-note", if (ko) "이 수정은 동일 자료의 MI를 보고 선택했으므로, 이 차이 검정은 탐색적 결과이며 확인적 근거로 해석하면 안 됩니다." else "Because the modification was selected using MI from the same data, this difference test is exploratory and should not be treated as confirmatory evidence.")
  )

}
