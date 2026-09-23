# Structural core fit result rendering.

structural_canvas_fit_table_result_ui <- function(bundle, values) {
  shiny::req(nrow(values) > 0)
  if (!inherits(bundle$fit, "lavaan")) {
    pls_header <- function(value) {
      display <- switch(value,
        f2 = "f<sup>2</sup>", R2 = "R<sup>2</sup>", AdjR2 = "Adj R<sup>2</sup>",
        R2AdjR2 = "R<sup>2</sup>(adj R<sup>2</sup>)",
        beta = "&beta;",
        `Inner VIF` = "VIF", value
      )
      tags$th(HTML(display))
    }
    pls_cell <- function(value, column) {
      if (!identical(column, "Path")) return(structural_canvas_html_cell(value))
      parts <- strsplit(as.character(value %||% ""), "→", fixed = TRUE)[[1L]]
      if (length(parts) != 2L) return(tags$td(class = "structural-pls-path-cell", value))
      tags$td(class = "structural-pls-path-cell",
        tags$div(class = "structural-pls-path-layout",
          tags$span(class = "structural-pls-path-from", trimws(parts[[1L]])),
          tags$span(class = "structural-pls-path-arrow", "→"),
          tags$span(class = "structural-pls-path-to", trimws(parts[[2L]]))
        )
      )
    }
    return(structural_canvas_table_sheet(tagList(
      tags$table(
        class = "table table-striped table-bordered structural-fit-table structural-pls-main-effects-table",
        tags$thead(tags$tr(lapply(names(values), pls_header))),
        tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(Map(pls_cell, as.character(values[index, , drop = TRUE]), names(values)))))
      ),
      structural_canvas_abbreviation_footnotes(values, "pls-path"),
      result_note_paragraph(class = "structural-result-note", "All tests are two-sided."),
      if (as.integer(bundle$pls_bootstrap %||% 0L) > 0L) {
        bootstrap <- bundle$pls_bootstrap_result %||% list()
        valid <- suppressWarnings(as.integer(bootstrap$nboot %||% 0L))
        requested <- suppressWarnings(as.integer(bootstrap$requested_nboot %||% bundle$pls_bootstrap %||% 0L))
        minimum_ratio <- suppressWarnings(as.numeric(bootstrap$minimum_valid_ratio %||% .80))
        bootstrap_status <- as.character(bootstrap$bootstrap_status %||% "Not recorded")[[1L]]
        retained_nonpd <- suppressWarnings(as.integer(bootstrap$retained_nonpositive_definite_plsc_draws %||% 0L))
        if (!is.finite(retained_nonpd) || retained_nonpd < 0L) retained_nonpd <- 0L
        status_key <- tolower(trimws(bootstrap_status))
        failure_values <- as.character(bootstrap$failure_message %||% "")
        failure_message <- if (length(failure_values)) trimws(failure_values[[1L]]) else ""
        unavailable_reason <- if (isTRUE(bootstrap$inference_available)) {
          ""
        } else if (identical(status_key, "pending")) {
          "Bootstrap is still in progress; inferential fields remain suppressed until completion."
        } else if (identical(status_key, "failed")) {
          "Bootstrap execution failed; point estimates are retained and inferential fields are suppressed."
        } else if (identical(status_key, "canceled")) {
          "Bootstrap was canceled; point estimates are retained and inferential fields are suppressed."
        } else if (identical(status_key, "insufficient")) {
          paste0("Fewer than ", formatC(100 * minimum_ratio, format = "fg", digits = 3), "% of requested resamples passed the whole-draw contract; inferential fields are suppressed.")
        } else {
          paste0(
            "Bootstrap inference is unavailable (status: ", bootstrap_status,
            "); point estimates are retained. Bootstrap SE, CI, t, and p values are suppressed."
          )
        }
        if (nzchar(unavailable_reason) && nzchar(failure_message)) unavailable_reason <- paste0(unavailable_reason, " Detail: ", failure_message)
        result_note_paragraph(
          class = paste("structural-result-note structural-main-note structural-main-note-2", if (!isTRUE(bootstrap$inference_available)) "structural-result-warning" else ""),
          paste0(
            "Bootstrap inference uses type-7 percentile CIs and plus-one two-sided empirical sign p values under a common valid-draw contract. Valid resamples = ",
            valid, "/", requested, "; minimum required = ", formatC(100 * minimum_ratio, format = "fg", digits = 3), "%; status = ", bootstrap_status, ". ",
            if (retained_nonpd > 0L) paste0(
              "Retained globally non-positive-definite PLSc resamples after all local-equation and downstream checks = ",
              retained_nonpd, ". "
            ) else "",
            unavailable_reason
          )
        )
      },
      result_note_paragraph(
        class = "structural-result-note structural-main-note structural-main-note-3",
        "Indirect and total effects and extended diagnostics are reported in the supplementary tables."
      ),
      if (length(bundle$diagnostics$ignored_covariances %||% character(0))) result_note_paragraph(
        class = "structural-result-note structural-main-note structural-main-note-symbol",
        paste0("Covariance paths omitted by PLS-SEM: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), ".")
      )
    ), table = values, role = "main", orientation = "landscape"))
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
  structural_canvas_table_sheet(tagList(tags$table(
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
    result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-1", HTML("<em>Note.</em> &chi;<sup>2</sup> = chi-square; df = degrees of freedom; CFI = Comparative Fit Index; TLI = Tucker-Lewis Index; SRMR = Standardized Root Mean Square Residual; RMSEA = Root Mean Square Error of Approximation; CI = confidence interval; LLCI/ULCI = lower/upper confidence limits.")),
    if (selection$adjusted)
    result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-2", if (is.null(baseline_selection) || same_measure_keys) {
      paste0("* Reported lavaan measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
    } else {
      paste0("* Research-model measures: ", paste(unname(baseline_selection$keys), collapse = ", "), "; modified-model measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
    }),
    if ((!is.null(baseline_selection) && baseline_selection$values[[2L]] == 0) || selection$values[[2L]] == 0)
      result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-3", "Chi-square/df and some fit indices are not interpretable for a saturated model with df = 0.")
  ), table = values, role = "main", orientation = "landscape")

}

structural_canvas_identification_result_ui <- function(bundle, language = statedu_initial_language()) {
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
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
  power_labels <- c(rmsea_power = "RMSEA fit-test power", model_monte_carlo = "Model-specific Monte Carlo", target_effect_precision = "Target effect/precision", prior_evidence = "Prior evidence", other_documented = "Other documented basis")
  power_ko <- c(rmsea_power = "RMSEA 적합도 검정력", model_monte_carlo = "모형별 Monte Carlo", target_effect_precision = "목표 효과·정밀도", prior_evidence = "선행연구 근거", other_documented = "기타 문서화된 근거")
  basis <- bundle$power_basis %||% "not_recorded"
  basis_label <- if (basis %in% names(power_labels)) tr(unname(power_labels[[basis]]), unname(power_ko[[basis]])) else basis
  power_text <- if (identical(basis, "not_recorded")) tr("Not recorded; not inferred from fitted data", "기록되지 않음; 적합된 자료에서 역산하지 않음") else if (nzchar(bundle$power_details %||% "")) paste0(basis_label, ": ", bundle$power_details) else sprintf(tr("%s: details not recorded", "%s: 상세 기록 없음"), basis_label)
  inventory <- data.frame(
    Item = c("Latent constructs", "Observed indicators", "Structural paths", "Latent scaling", "Model df", "Free parameters", "A-priori power basis"),
    Value = c(
      latent_n, indicator_n, structural_n,
      if (isTRUE(bundle$std_lv)) "Latent variance = 1" else "Marker loading = 1",
      if (is.finite(model_df)) format(model_df, trim = TRUE) else "Not available",
      if (is.finite(free_parameters)) format(free_parameters, trim = TRUE) else "Not available",
      power_text
    ),
    stringsAsFactors = FALSE
  )
  inventory_labels <- c(
    "Latent constructs" = "잠재 구성개념",
    "Observed indicators" = "관측 지표",
    "Structural paths" = "구조경로",
    "Latent scaling" = "잠재변수 척도",
    "Model df" = "모형 자유도",
    "Free parameters" = "자유모수",
    "A-priori power basis" = "사전 검정력 근거",
    "Latent variance = 1" = "잠재분산 = 1",
    "Marker loading = 1" = "표지 지표 적재량 = 1",
    "Not recorded; not inferred from fitted data" = "기록되지 않음; 적합된 자료에서 역산하지 않음",
    "Not available" = "산출 불가")
  translate_inventory <- function(value) {
    if (value %in% names(inventory_labels)) tr(value, unname(inventory_labels[[value]])) else value
  }
  inventory$Item <- vapply(inventory$Item, translate_inventory, character(1), USE.NAMES = FALSE)
  inventory$Value[seq_len(6L)] <- vapply(inventory$Value[seq_len(6L)], translate_inventory, character(1), USE.NAMES = FALSE)
  attr(inventory, "result_user_columns") <- seq_along(inventory)
  display_issues <- issues
  if (all(c("Code", "Message") %in% names(display_issues)) && nrow(display_issues)) display_issues$Message <- mapply(
    structural_canvas_identification_issue_message, display_issues$Code, display_issues$Message,
    MoreArgs = list(language = language), USE.NAMES = FALSE)
  if ("Severity" %in% names(display_issues)) display_issues$Severity <- vapply(display_issues$Severity, function(value) {
    if (identical(value, "Error")) tr("Error", "오류") else if (identical(value, "Warning")) tr("Warning", "경고") else value
  }, character(1), USE.NAMES = FALSE)
  attr(display_issues, "result_user_columns") <- seq_along(display_issues)
  issue_headers <- c(Element="요소", Message="메시지", Severity="심각도", Code="코드")
  names(display_issues) <- vapply(names(display_issues), function(value) {
    if (value %in% names(issue_headers)) tr(value, unname(issue_headers[[value]])) else value
  }, character(1), USE.NAMES = FALSE)
  tags$div(class = "structural-identification-result",
    tags$h5(tr("Identification and power preflight", "식별성 및 검정력 사전점검")),
    if (!is.null(bundle$method_recommendation)) structural_canvas_method_recommendation_ui(
      bundle$method_recommendation,
      bundle$selected_method %||% structural_canvas_selected_method_label(if (inherits(bundle$fit, "pls_model")) "plssem" else "cbsem", bundle$estimator),
      language
    ),
    structural_canvas_basic_html_table(
      inventory,
      class = "table table-striped table-bordered structural-identification-inventory",
      role = "appendix",
      orientation = "portrait",
      language = language
    ),
    if (!nrow(display_issues)) result_note_paragraph(class = "structural-result-note", tr("No rule-based pre-fit identification issues were detected.", "규칙 기반 사전 식별성 문제는 발견되지 않았습니다.")),
    if (nrow(display_issues)) structural_canvas_basic_html_table(display_issues, role = "appendix", orientation = "auto", language = language),
    result_note_paragraph(
      class = "structural-result-note",
      tr("This rule-based screen does not prove mathematical identification; lavaan estimation, degrees of freedom, information-matrix checks, and solution admissibility remain decisive.", "이 규칙 기반 점검만으로 수학적 식별성이 증명되지는 않습니다. lavaan 추정, 자유도, 정보행렬 점검, 해의 허용성이 최종 판단 기준입니다.")
    ),
    result_note_paragraph(class = "structural-result-note", tr("Power is not back-calculated from fitted-sample p values. Record an a-priori RMSEA-power analysis or a simulation based on explicitly specified parameters and model structure.", "검정력은 적합된 표본의 유의확률에서 역산하지 않습니다. 연구 전 RMSEA 검정력 또는 명시한 모수·모형을 이용한 시뮬레이션 근거를 별도로 기록하십시오."))
  )

}

structural_canvas_difference_text <- function(value, language) {
  if (identical(normalize_app_language(language), "en")) return(value)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  labels <- c(
    "eligibility was not established" = "비교 가능 조건이 확인되지 않았습니다",
    "Models use different sample sizes." = "모형의 표본 크기가 다릅니다.",
    "Models use different observed variables." = "모형의 관측변수가 다릅니다.",
    "Models do not use the same analyzed observations and values." = "모형이 동일한 분석 관측치와 값을 사용하지 않습니다.",
    "Models use different group structures." = "모형의 집단 구조가 다릅니다.",
    "Models use incompatible estimator families." = "모형의 추정량 계열이 호환되지 않습니다.",
    "Models use different ML likelihood conventions." = "모형의 ML 우도 규약이 다릅니다.",
    "Models do not have different finite degrees of freedom." = "모형의 자유도가 유한하면서 서로 다른 조건을 충족하지 않습니다.",
    "A strict free-parameter nesting relation was not verified." = "자유모수의 엄격한 내포 관계가 확인되지 않았습니다.",
    "Nesting was verified, but lavaan did not return a usable difference test." = "내포 관계는 확인했지만 lavaan이 사용 가능한 차이검정을 반환하지 않았습니다.",
    "One or both models are inadmissible." = "하나 이상의 모형이 허용 불가능한 해를 산출했습니다.",
    "Chi-Squared Difference Test" = "카이제곱 차이검정",
    "Likelihood-ratio difference test" = "우도비 차이검정")
  normalized <- trimws(gsub("[[:space:]]+", " ", value))
  if (normalized %in% names(labels)) return(tr(normalized, unname(labels[[normalized]])))
  prefix <- "One or both models are inadmissible"
  if (startsWith(value, paste0(prefix, " ("))) return(paste0(tr(paste0(prefix, "."), unname(labels[[paste0(prefix, ".")]])), " ", substring(value, nchar(prefix) + 2L)))
  robust <- 'Scaled Chi-Squared Difference Test (method = "satorra.bentler.2001") lavaan->lavTestLRT(): lavaan NOTE: The "Chisq" column contains standard test statistics, not the robust test that should be reported per model. A robust difference test is a function of two standard (not robust) statistics.'
  if (identical(normalized, robust)) return(sprintf(tr("Scaled chi-square difference test (method: %s). The Chisq column contains standard test statistics, not the robust statistics to report for each model. The robust difference test is a function of two standard statistics.", "척도 보정 카이제곱 차이검정(방법: %s). Chisq 열에는 각 모형에 보고할 강건 통계량이 아닌 일반 검정통계량이 들어 있습니다. 강건 차이검정은 두 일반 통계량의 함수입니다."), "satorra.bentler.2001"))
  value
}

structural_canvas_fit_difference_result_ui <- function(bundle, language) {
  if (!identical(bundle$comparison_type %||% "", "mi") || is.null(bundle$baseline_fit)) return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  report <- structural_canvas_model_difference_report(bundle)
  if (!nrow(report) || !isTRUE(report$Available[[1L]])) {
    reason <- if (nrow(report)) as.character(report$Reason[[1L]]) else "eligibility was not established"
    return(result_note_paragraph(class = "structural-result-note", sprintf(tr("A formal model-difference test was suppressed: %s", "공식 모형 차이 검정을 표시하지 않았습니다: %s"), structural_canvas_difference_text(reason, language))))
  }
  tags$div(
    class = "structural-fit-difference",
    tags$h5(tr("Research model vs exploratory modified model", "연구모형 vs 탐색적 수정모형")),
    structural_canvas_basic_html_table(
      data.frame(
        `Δχ²` = format_decimal3(report$`Delta chi-square`[[1L]]),
        `Δdf` = format_decimal3(report$`Delta df`[[1L]]),
        p = format_p(report$p[[1L]]),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      role = "appendix",
      orientation = "portrait",
      language = language
    ),
    result_note_paragraph(class = "structural-result-note", structural_canvas_difference_text(report$Method[[1L]], language)),
    result_note_paragraph(class = "structural-result-note", tr("Because the modification was selected using MI from the same data, this difference test is exploratory and should not be treated as confirmatory evidence.", "이 수정은 동일 자료의 MI를 보고 선택했으므로, 이 차이 검정은 탐색적 결과이며 확인적 근거로 해석하면 안 됩니다."))
  )

}
