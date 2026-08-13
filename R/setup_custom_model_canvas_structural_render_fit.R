structural_canvas_register_fit_diagnostic_outputs <- function(output, prefix, analysis_type, fit_result, result_table, dataset_fn, app_language_fn) {
output[[paste0(prefix, "_result_fit")]] <- renderUI({
  values <- result_table("fit")
  shiny::req(nrow(values) > 0)
  ci_percent <- round(100 * as.numeric(fit_result()$rmsea_ci %||% .90))
  comparison_fits <- if (isTRUE(fit_result()$modified_from_baseline) && !is.null(fit_result()$baseline_fit)) list(fit_result()$baseline_fit, fit_result()$fit) else list(fit_result()$fit)
  fit_selections <- structural_canvas_common_fit_measures(comparison_fits, fit_result()$estimator %||% "ML", fit_result()$rmsea_ci %||% .90)
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
})
output[[paste0(prefix, "_result_identification")]] <- renderUI({
  bundle <- fit_result()
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
})
output[[paste0(prefix, "_result_normality")]] <- renderUI({
  structural_canvas_normality_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_risk_diagnostics")]] <- renderUI({
  structural_canvas_risk_diagnostics_result_ui(fit_result(), dataset_fn(), analysis_type)
})
output[[paste0(prefix, "_result_missing_outliers")]] <- renderUI({
  structural_canvas_missing_outliers_result_ui(fit_result(), dataset_fn(), analysis_type)
})
output[[paste0(prefix, "_result_fit_difference")]] <- renderUI({
  bundle <- fit_result()
  if (!identical(bundle$comparison_type %||% "", "mi") || is.null(bundle$baseline_fit)) return(NULL)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
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
})
output[[paste0(prefix, "_result_invariance")]] <- renderUI({
  structural_canvas_invariance_result_ui(fit_result())
})
output[[paste0(prefix, "_result_fit_guidance")]] <- renderUI({
  structural_canvas_fit_guidance_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_rmsea_tests")]] <- renderUI({
  structural_canvas_rmsea_tests_result_ui(fit_result())
})
output[[paste0(prefix, "_result_information_criteria")]] <- renderUI({
  structural_canvas_information_criteria_result_ui(fit_result())
})
output[[paste0(prefix, "_result_bollen_stine")]] <- renderUI({
  structural_canvas_bollen_stine_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_heywood")]] <- renderUI({
  structural_canvas_heywood_result_ui(fit_result(), dataset_fn(), prefix, analysis_type)
})
  invisible(TRUE)
}
