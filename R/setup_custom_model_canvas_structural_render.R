structural_canvas_reporting_package_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) return("not available")
  as.character(utils::packageVersion(package))
}

structural_canvas_reporting_sample_size <- function(bundle, analysis_type) {
  if (is.null(bundle)) return("")
  if (identical(analysis_type, "plssem")) {
    return(as.character(bundle$diagnostics$n %||% bundle$n %||% ""))
  }
  if (!is.null(bundle$fit) && requireNamespace("lavaan", quietly = TRUE)) {
    inspected <- tryCatch(lavaan::lavInspect(bundle$fit, "ntotal"), error = function(error) NULL)
    if (length(inspected)) {
      inspected <- as.integer(inspected)
      group_names <- names(inspected)
      if (length(inspected) > 1L) {
        if (is.null(group_names) || any(!nzchar(group_names))) group_names <- paste0("group", seq_along(inspected))
        return(paste0(sum(inspected, na.rm = TRUE), " (", paste(paste0(group_names, "=", inspected), collapse = ", "), ")"))
      }
      return(as.character(inspected[[1L]]))
    }
  }
  as.character(bundle$diagnostics$n %||% bundle$n %||% "")
}

structural_canvas_reporting_lavaan_option <- function(bundle, name, fallback = "") {
  if (is.null(bundle$fit) || !requireNamespace("lavaan", quietly = TRUE)) return(fallback)
  options <- tryCatch(lavaan::lavInspect(bundle$fit, "options"), error = function(error) list())
  as.character(options[[name]] %||% fallback)
}

structural_canvas_reporting_bootstrap_label <- function(bundle, analysis_type) {
  requested <- character(0)
  if (identical(analysis_type, "plssem")) {
    pls_r <- suppressWarnings(as.integer(bundle$pls_bootstrap %||% 0L))
    if (is.finite(pls_r) && pls_r > 0L) {
      requested <- c(requested, paste0("PLS bootstrap R=", pls_r, ", seed=", bundle$pls_seed %||% "not recorded"))
    }
  } else {
    rel_r <- suppressWarnings(as.integer(bundle$reliability_bootstrap %||% 0L))
    htmt_r <- suppressWarnings(as.integer(bundle$htmt_bootstrap %||% 0L))
    bs_r <- suppressWarnings(as.integer(bundle$bollen_stine_bootstrap %||% 0L))
    if (is.finite(rel_r) && rel_r > 0L) {
      requested <- c(requested, paste0("Reliability/AVE R=", rel_r, ", CI=", bundle$reliability_ci_method %||% "percentile", ", seed=", bundle$reliability_seed %||% "not recorded"))
    }
    if (is.finite(htmt_r) && htmt_r > 0L) {
      requested <- c(requested, paste0("HTMT R=", htmt_r, ", CI=", bundle$htmt_ci_method %||% "percentile", ", seed=", bundle$htmt_seed %||% "not recorded"))
    }
    if (is.finite(bs_r) && bs_r > 0L) {
      requested <- c(requested, paste0("Bollen-Stine R=", bs_r, ", seed=", bundle$bollen_stine_seed %||% "not recorded"))
    }
  }
  if (!length(requested)) "Not requested" else paste(requested, collapse = "; ")
}

structural_canvas_reporting_predict_label <- function(bundle, analysis_type) {
  if (!identical(analysis_type, "plssem")) return("Not applicable")
  result <- bundle$pls_predict_result
  if (is.list(result)) {
    return(paste0("Executed: folds=", result$folds %||% "", ", reps=", result$reps %||% ""))
  }
  folds <- suppressWarnings(as.integer(bundle$pls_predict_folds %||% 0L))
  reps <- suppressWarnings(as.integer(bundle$pls_predict_reps %||% 0L))
  if (is.finite(folds) && folds > 1L) return(paste0("Requested: folds=", folds, ", reps=", reps))
  "Not requested"
}

structural_canvas_reporting_group_label <- function(bundle) {
  if (is.list(bundle$invariance_result)) {
    group <- as.character(bundle$invariance_group %||% "")
    label <- as.character(bundle$invariance_result$type %||% "group analysis")
    if (nzchar(group)) paste0(label, " by ", group) else label
  } else if (isTRUE(bundle$invariance_enabled)) {
    paste0("Requested by ", as.character(bundle$invariance_group %||% "selected group"))
  } else {
    "Not enabled"
  }
}

structural_canvas_reporting_holdout_label <- function(bundle) {
  if (is.list(bundle$mi_holdout_result)) return("Executed")
  if (isTRUE(bundle$mi_holdout_enabled)) {
    return(paste0("Requested: fraction=", bundle$mi_holdout_fraction %||% "", ", seed=", bundle$mi_holdout_seed %||% "not recorded"))
  }
  "Not enabled"
}

structural_canvas_reporting_admissibility_label <- function(bundle) {
  converged <- bundle$converged %||% bundle$diagnostics$converged %||% NA
  admissible <- bundle$admissible %||% bundle$diagnostics$admissible %||% NA
  paste0(
    "converged=", if (is.na(converged)) "not recorded" else as.character(isTRUE(converged)),
    "; admissible=", if (is.na(admissible)) "not recorded" else as.character(isTRUE(admissible))
  )
}

structural_canvas_reporting_context_rows <- function(bundle, analysis_type) {
  if (is.null(bundle)) return(data.frame(Item = character(0), Value = character(0), stringsAsFactors = FALSE))
  engine <- if (identical(analysis_type, "plssem")) {
    paste0("seminr ", structural_canvas_reporting_package_version("seminr"))
  } else {
    paste0("lavaan ", structural_canvas_reporting_package_version("lavaan"))
  }
  estimator <- if (identical(analysis_type, "plssem")) {
    "PLS path modeling"
  } else {
    bundle$estimator %||% structural_canvas_reporting_lavaan_option(bundle, "estimator", "")
  }
  missing <- if (identical(analysis_type, "plssem")) {
    "Valid rows used by seminr; no FIML/pairwise option"
  } else {
    bundle$missing %||% structural_canvas_reporting_lavaan_option(bundle, "missing", "")
  }
  ordered <- as.character(bundle$ordered %||% character(0))
  ordered_label <- if (length(ordered)) paste(ordered, collapse = ", ") else "None recorded"
  scaling <- if (identical(analysis_type, "plssem")) {
    "Composite scores"
  } else if (isTRUE(bundle$std_lv)) {
    "std.lv = TRUE"
  } else {
    "Marker loading scaling"
  }
  context <- if (isTRUE(bundle$modified_model) || length(bundle$mi_history %||% list())) {
    "Exploratory modified model"
  } else {
    "Original/prespecified model"
  }
  syntax_label <- if (nzchar(as.character(bundle$syntax %||% ""))) "Available in analysis bundle" else "Not recorded"
  data.frame(
    Item = c(
      "Analysis context",
      "Analysis engine",
      "Estimator or algorithm",
      "Missing-data handling",
      "Analyzed N",
      "Ordered indicators",
      "Latent scaling",
      "Bootstrap settings",
      "PLSpredict setting",
      "Group analysis",
      "MI holdout",
      "Syntax availability",
      "Admissibility and convergence"
    ),
    Value = c(
      context,
      engine,
      estimator,
      missing,
      structural_canvas_reporting_sample_size(bundle, analysis_type),
      ordered_label,
      scaling,
      structural_canvas_reporting_bootstrap_label(bundle, analysis_type),
      structural_canvas_reporting_predict_label(bundle, analysis_type),
      structural_canvas_reporting_group_label(bundle),
      structural_canvas_reporting_holdout_label(bundle),
      syntax_label,
      structural_canvas_reporting_admissibility_label(bundle)
    ),
    stringsAsFactors = FALSE
  )
}

structural_canvas_reporting_context_result_ui <- function(bundle, analysis_type, language = statedu_initial_language()) {
  rows <- structural_canvas_reporting_context_rows(bundle, analysis_type)
  if (!nrow(rows)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  div(
    class = "result-section regression-result-panel structural-reporting-context",
    h4(if (ko) "Reporting checklist" else "Reporting checklist"),
    structural_canvas_basic_html_table(rows),
    tags$p(
      class = "structural-result-note",
      if (ko) {
        "Use this block to report reproducibility conditions: estimator, missing-data handling, analyzed N, ordered indicators, bootstrap settings, seeds, group analysis, and whether MI-based changes are exploratory."
      } else {
        "Use this block to report reproducibility conditions: estimator, missing-data handling, analyzed N, ordered indicators, bootstrap settings, seeds, group analysis, and whether MI-based changes are exploratory."
      }
    )
  )
}

structural_canvas_register_result_outputs <- function(input, output, prefix, canvas_output, analysis_type, selected_names_fn, variable_table_fn, labels_fn, app_language_fn, fit_result, result_table) {
output[[canvas_output]] <- renderUI({
  structural_equation_workspace(selected_names_fn(), variable_table_fn(), labels_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_results")]] <- renderUI({
  shiny::req(!is.null(fit_result()))
  bundle <- fit_result()
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  div(
    class = "structural-analysis-results regression-results",
    h3(if (ko) "분석 결과" else "Analysis Results"),
    if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_reproducibility"), if (ko) "분석 기록 다운로드" else "Download analysis record", class = "btn btn-default btn-sm"),
    if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_tables"), if (ko) "결과표 Excel 다운로드" else "Download result tables", class = "btn btn-default btn-sm"),
    div(class = "result-section regression-result-panel", h4(if (ko) "1. 모형 개요" else "1. Model overview"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview")))),
    uiOutput(paste0(prefix, "_result_reporting_context")),
    uiOutput(paste0(prefix, "_result_identification")),
    uiOutput(paste0(prefix, "_result_normality")),
    uiOutput(paste0(prefix, "_result_missing_outliers")),
    uiOutput(paste0(prefix, "_result_risk_diagnostics")),
    uiOutput(paste0(prefix, "_result_heywood")),
    div(class = "result-section regression-result-panel", h4(if (identical(analysis_type, "plssem")) {
      if (ko) "2. PLS 구조모형 효과" else "2. PLS structural model effects"
    } else {
      if (ko) "2. 모형 적합도" else "2. Model fit"
    }), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit"))), if (identical(analysis_type, "plssem")) tagList(
      tags$p(class = "structural-result-note", if (ko) "PLS-SEM 구조모형 출력은 공분산 기반 전역 적합도 지수 대신 경로계수, R2, 수정 R2, f2, inner VIF, 총효과와 간접효과를 표시합니다." else "PLS-SEM structural output reports path coefficients, R2, adjusted R2, f2, inner VIF, and total/indirect effects rather than covariance-based global fit indices."),
      tags$p(class = "structural-result-note", if (ko) "Inner VIF는 seminr에서 내생 구성개념의 선행 예측자 정보를 반환할 때 직접 구조경로에 대해 표시됩니다." else "Inner VIF is reported for direct structural paths when an endogenous construct has antecedent predictors available from seminr."),
      if (length(bundle$diagnostics$ignored_covariances %||% character(0))) tags$p(class = "structural-result-note", if (ko) paste0("PLS-SEM에서는 공분산 경로를 추정하지 않으므로 다음 캔버스 공분산 경로를 제외했습니다: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), ".") else paste0("PLS-SEM does not estimate covariance paths, so these canvas covariance paths were excluded: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), "."))
    ), if (!identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_lavaan_quality")), if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_pls_quality")), uiOutput(paste0(prefix, "_result_pls_predict")), uiOutput(paste0(prefix, "_result_fit_guidance")), uiOutput(paste0(prefix, "_result_rmsea_tests")), uiOutput(paste0(prefix, "_result_information_criteria")), uiOutput(paste0(prefix, "_result_bollen_stine")), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit_difference")))),
    if (analysis_type %in% c("cbsem", "sem")) div(
      class = "result-section regression-result-panel structural-path-result",
      h4(if (ko) "3. 구조모형 경로" else "3. Structural model paths"),
      div(class = "table-responsive", tableOutput(paste0(prefix, "_result_structural"))),
      tags$p(class = "structural-result-note", if (ko) "구조경로 표는 lavaan의 회귀경로(~)에 대한 비표준화 계수, 표준화 계수, 신뢰구간, R², z 및 p 값을 표시합니다." else "Structural paths are lavaan regression paths (~), reported with unstandardized and standardized coefficients, confidence intervals, R², z, and p values."),
      tags$p(class = "structural-result-note", if (ko) "매개 경로가 정의되면 간접효과와 총효과 행도 함께 표시되며, lavaan이 반환하는 경우 표준화 효과의 95% 신뢰구간을 포함합니다." else "When mediation paths are defined, indirect and total effect rows are included with standardized-effect 95% confidence intervals when lavaan returns them.")
    ),
    uiOutput(paste0(prefix, "_result_invariance")),
    div(class = "result-section regression-result-panel", h4(if (ko) "3. 잠재 구성개념 상관, 신뢰도 및 수렴·판별타당도" else "3. Latent construct correlations, reliability, and convergent/discriminant validity"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_validity"))), uiOutput(paste0(prefix, "_result_latent_correlation_ci")), uiOutput(paste0(prefix, "_result_validity_note")), uiOutput(paste0(prefix, "_result_reliability_bootstrap")), uiOutput(paste0(prefix, "_result_factor_scores")), uiOutput(paste0(prefix, "_result_htmt"))),
    div(
      class = "result-section regression-result-panel structural-measurement-result",
      h4(if (ko) "4. 측정모형" else "4. Measurement model"),
      div(class = "table-responsive", tableOutput(paste0(prefix, "_result_measurement"))),
      if (identical(analysis_type, "plssem")) tagList(
        tags$p(class = "structural-result-note", if (ko) "PLS-SEM 측정모형 출력은 outer loading, outer weight, item VIF, 교차적재 요약, reflective/formative 측정모드를 표시합니다." else "PLS-SEM measurement output reports outer loadings, outer weights, item VIF, cross-loading summaries, and the reflective/formative measurement mode."),
        tags$p(class = "structural-result-note", if (ko) "Reflective 구성개념은 outer loading과 교차적재를 확인하고, formative 구성개념은 outer weight, item VIF, 지표의 이론적 포괄성을 우선 확인합니다." else "For reflective constructs, review outer loadings and cross-loadings. For formative constructs, prioritize outer weights, item VIF, and substantive indicator coverage."),
        tags$p(class = "structural-result-note", if (ko) "PLS bootstrap을 요청한 경우 loading과 weight의 CI/p 열은 사용 가능한 seminr percentile bootstrap 요약을 사용합니다." else "When PLS bootstrap is requested, loading and weight CI/p columns use seminr percentile bootstrap summaries when available.")
      ) else tagList(
        tags$p(class = "structural-result-note", "* Fixed reference loading; its unstandardized SE, z, and p are not estimated. The standardized loading remains a derived estimate and therefore has a confidence interval."),
        tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized loadings. A fixed reference loading has a degenerate B interval at its fixed value."),
        tags$p(class = "structural-result-note", "Standardized-loading confidence intervals are 95% delta-method intervals from lavaan; robust fitted models use the fitted model's robust covariance information."),
        tags$p(class = "structural-result-note", "R² confidence intervals are obtained by complementing the standardized residual-variance interval (lower = 1 − residual upper; upper = 1 − residual lower)."),
        tags$p(class = "structural-result-note", "Std. residual variance is the standardized diagonal residual variance for each indicator. † marks an unavailable residual value, a residual outside [0, 1], or an R² interval extending beyond [0, 1], including a negative-residual Heywood case."),
        tags$p(class = "structural-result-note", "Guidance prioritizes inadmissible residual variance, then cross-loading, a standardized-loading CI containing 0, and |β| < .40. The .40 value is a descriptive review guideline rather than a universal item-retention rule."),
        tags$p(class = "structural-result-note", "Cross-loaded indicators require theory-based interpretation; simple-structure reliability and discriminant-validity summaries, especially HTMT, may be unavailable or require caution.")
      )
    ),
    uiOutput(paste0(prefix, "_result_higher_order")),
    uiOutput(paste0(prefix, "_result_residuals")),
    uiOutput(paste0(prefix, "_result_mi_holdout")),
    uiOutput(paste0(prefix, "_result_mi_history")),
    if (analysis_type != "plssem") div(class = "result-section regression-result-panel structural-mi-result", h4(if (ko) "6. 수정지수(MI)" else "6. Modification indices (MI)"), uiOutput(paste0(prefix, "_result_mi")))
  )
})
  output[[paste0(prefix, "_result_reporting_context")]] <- renderUI({
    structural_canvas_reporting_context_result_ui(fit_result(), analysis_type, statedu_current_language(app_language_fn))
  })
  structural_canvas_register_fit_diagnostic_outputs(
    output, prefix, analysis_type, fit_result, result_table, dataset_fn, app_language_fn
  )
  structural_canvas_register_validity_outputs(
    output, prefix, analysis_type, fit_result, result_table, app_language_fn
  )
  if (analysis_type != "plssem") structural_canvas_register_local_fit_outputs(
    output, prefix, fit_result, app_language_fn
  )
for (kind in c("overview", "validity", "measurement", "structural")) local({
  result_kind <- kind
  output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
})
  structural_canvas_register_mi_render_outputs(
    output, prefix, fit_result, result_table, app_language_fn
  )
  invisible(TRUE)
}
