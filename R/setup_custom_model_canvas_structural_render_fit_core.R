# Structural core fit result rendering.

structural_canvas_fit_table_result_ui <- function(bundle, values) {
  shiny::req(nrow(values) > 0)
  if (!inherits(bundle$fit, "lavaan")) {
    return(tagList(
      tags$table(
        class = "table table-striped table-bordered structural-fit-table",
        tags$thead(tags$tr(lapply(names(values), tags$th))),
        tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, , drop = TRUE]), structural_canvas_html_cell))))
      ),
      tags$p(class = "structural-result-note", "PLS-SEM reports structural effects, path coefficients, f^2/q^2 effect-size labels, Q^2 predictive relevance, and explained variance (R^2/adjusted R^2) rather than covariance-based global fit indices."),
      if (as.integer(bundle$pls_bootstrap %||% 0L) > 0L) tags$p(class = "structural-result-note", paste0("Bootstrap columns use seminr percentile intervals from ", as.integer(bundle$pls_bootstrap %||% 0L), " requested resamples; failed bootstrap iterations are excluded by seminr."))
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
  model_header <- names(values)[[1L]] %||% "Model"
  tagList(tags$table(
    class = "table table-striped table-bordered structural-fit-table",
    tags$thead(
      tags$tr(
        tags$th(rowspan = "2", model_header),
        tags$th(rowspan = "2", if (!same_measure_keys) HTML("Adjusted &chi;<sup>2</sup>*") else if (grepl("Scaled", fit_labels[[1L]], fixed = TRUE)) HTML("Scaled &chi;<sup>2</sup>*") else HTML("&chi;<sup>2</sup>")), tags$th(rowspan = "2", "df"), tags$th(rowspan = "2", "p"), tags$th(rowspan = "2", HTML("&chi;<sup>2</sup>/df")),
        tags$th(rowspan = "2", paste0(fit_labels[[5L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", paste0(fit_labels[[6L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", "SRMR"), tags$th(rowspan = "2", paste0(fit_labels[[8L]], if (selection$adjusted) "*" else "")),
        tags$th(colspan = "2", paste0(ci_percent, "% CI"))
      ),
      tags$tr(tags$th("LLCI"), tags$th("ULCI"))
    ),
    tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, , drop = TRUE]), structural_canvas_html_cell))))
  ),
    tags$p(class = "structural-result-note", HTML("&chi;<sup>2</sup> = chi-square; df = degrees of freedom; CFI = Comparative Fit Index; TLI = Tucker-Lewis Index; SRMR = Standardized Root Mean Square Residual; RMSEA = Root Mean Square Error of Approximation; CI = confidence interval; LLCI/ULCI = lower/upper limit of the confidence interval.")),
    if (selection$adjusted)
    tags$p(class = "structural-result-note", if (is.null(baseline_selection) || same_measure_keys) {
      paste0("* Reported lavaan measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
    } else {
      paste0("* Research-model measures: ", paste(unname(baseline_selection$keys), collapse = ", "), "; modified-model measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
    }),
    if ((!is.null(baseline_selection) && baseline_selection$values[[2L]] == 0) || selection$values[[2L]] == 0)
      tags$p(class = "structural-result-note", "Chi-square/df and some fit indices are not interpretable for a saturated model with df = 0.")
  )

}

structural_canvas_identification_result_ui <- function(bundle, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  issues <- bundle$identification %||% data.frame()
  snapshot <- bundle$snapshot %||% list(nodes = list(), edges = list())
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latent_n <- sum(vapply(nodes, function(node) identical(node$role, "latent"), logical(1)))
  indicator_n <- sum(vapply(nodes, function(node) identical(node$role, "indicator"), logical(1)))
  structural_n <- sum(vapply(edges, function(edge) {
    if (identical(edge$kind, "covariance") || identical(as.character(edge$pathType %||% ""), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")
  }, logical(1)))
  fitted <- inherits(bundle$fit, "lavaan")
  model_df <- if (fitted) tryCatch(as.numeric(lavaan::fitMeasures(bundle$fit, "df")[[1L]]), error = function(error) NA_real_) else NA_real_
  free_parameters <- if (fitted) tryCatch(as.numeric(lavaan::lavInspect(bundle$fit, "npar")), error = function(error) NA_real_) else NA_real_
  inventory <- data.frame(
    Item = c("Latent constructs", "Observed indicators", "Structural paths", "Latent scaling", "Model df", "Free parameters", "A-priori power basis"),
    Value = c(
      latent_n, indicator_n, structural_n,
      if (isTRUE(bundle$std_lv)) "Latent variance = 1" else "Marker loading = 1",
      if (is.finite(model_df)) format(model_df, trim = TRUE) else "Not available",
      if (is.finite(free_parameters)) format(free_parameters, trim = TRUE) else "Not available",
      if (identical(bundle$power_basis %||% "not_recorded", "not_recorded")) "Not recorded; not inferred from fitted data" else paste0(bundle$power_basis, if (nzchar(bundle$power_details %||% "")) paste0(": ", bundle$power_details) else ": details not recorded")
    ),
    stringsAsFactors = FALSE
  )
  if (ko) {
    inventory$Item <- c("잠재 구성개념", "관측 지표", "구조경로", "잠재변수 척도", "모형 자유도", "자유모수", "사전 검정력 근거")
    inventory$Value[inventory$Value == "Latent variance = 1"] <- "잠재분산 = 1"
    inventory$Value[inventory$Value == "Marker loading = 1"] <- "표지 지표 적재량 = 1"
    inventory$Value[inventory$Value == "Not available"] <- "산출 불가"
    inventory$Value[inventory$Value == "Not recorded; not inferred from fitted data"] <- "기록되지 않음; 적합된 자료에서 역산하지 않음"
  }
  display_issues <- issues
  if (ko && ncol(display_issues)) {
    issue_name_map <- c(Element = "요소", Message = "메시지", Severity = "심각도")
    names(display_issues) <- ifelse(names(display_issues) %in% names(issue_name_map), unname(issue_name_map[names(display_issues)]), names(display_issues))
  }
  tags$div(class = "structural-identification-result",
    tags$h5(if (ko) "식별성 및 검정력 사전점검" else "Identification and power preflight"),
    if (!is.null(bundle$method_recommendation)) structural_canvas_method_recommendation_ui(
      bundle$method_recommendation,
      bundle$selected_method %||% structural_canvas_selected_method_label(if (inherits(bundle$fit, "pls_model")) "plssem" else "cbsem", bundle$estimator),
      language
    ),
    tags$table(class = "table table-striped table-bordered structural-identification-inventory",
      tags$thead(tags$tr(lapply(names(inventory), tags$th))),
      tags$tbody(lapply(seq_len(nrow(inventory)), function(index) tags$tr(lapply(as.character(inventory[index, ]), tags$td))))
    ),
    if (!nrow(display_issues)) tags$p(class = "structural-result-note", if (ko) "규칙 기반 사전 식별성 문제는 발견되지 않았습니다." else "No rule-based pre-fit identification issues were detected."),
    if (nrow(display_issues)) tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display_issues), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display_issues)), function(index) tags$tr(lapply(as.character(display_issues[index, ]), tags$td))))
    ),
    tags$p(
      class = "structural-result-note",
      if (ko) "이 규칙 기반 점검만으로 수학적 식별성이 증명되지는 않습니다. lavaan 추정, 자유도, 정보행렬 점검, 해의 허용성이 최종 판단 기준입니다." else "This rule-based screen does not prove mathematical identification; lavaan estimation, degrees of freedom, information-matrix checks, and solution admissibility remain decisive."
    ),
    tags$p(class = "structural-result-note", if (ko) "검정력은 적합된 표본의 유의확률에서 역산하지 않습니다. 연구 전 RMSEA 검정력 또는 명시한 모수·모형을 이용한 시뮬레이션 근거를 별도로 기록하십시오." else "Power is not back-calculated from fitted-sample p values. Record an a-priori RMSEA-power analysis or a simulation based on explicitly specified parameters and model structure.")
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
    tags$h5(if (ko) "연구모형 vs 탐색적 수정모형" else "Research model vs exploratory modified model"),
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
