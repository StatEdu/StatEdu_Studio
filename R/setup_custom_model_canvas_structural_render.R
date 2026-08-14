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

structural_canvas_reporting_context_display_rows <- function(rows, ko = FALSE) {
  if (!isTRUE(ko) || !nrow(rows)) return(rows)
  item_map <- c(
    "Analysis context" = "분석 맥락",
    "Analysis engine" = "분석 엔진",
    "Estimator or algorithm" = "추정량/알고리즘",
    "Missing-data handling" = "결측 처리",
    "Analyzed N" = "분석 N",
    "Ordered indicators" = "순서형 지표",
    "Latent scaling" = "잠재변수 척도화",
    "Bootstrap settings" = "부트스트랩 설정",
    "PLSpredict setting" = "PLSpredict 설정",
    "Group analysis" = "집단 분석",
    "MI holdout" = "MI 홀드아웃",
    "Syntax availability" = "구문 제공",
    "Admissibility and convergence" = "해의 허용성 및 수렴"
  )
  value_map <- c(
    "Original/prespecified model" = "연구모형",
    "Exploratory modified model" = "탐색적 수정모형",
    "Not requested" = "요청하지 않음",
    "Not applicable" = "해당 없음",
    "Not enabled" = "사용 안 함",
    "None recorded" = "기록 없음",
    "Not recorded" = "기록 없음",
    "Available in analysis bundle" = "분석 객체에 포함됨",
    "PLS path modeling" = "PLS 경로모형",
    "Valid rows used by seminr; no FIML/pairwise option" = "seminr 유효 행 사용(FIML/pairwise 옵션 없음)",
    "Composite scores" = "합성점수",
    "Marker loading scaling" = "기준 적재량 고정",
    "std.lv = TRUE" = "잠재변수 분산 = 1",
    "Executed" = "실행됨"
  )
  rows$Item <- ifelse(rows$Item %in% names(item_map), unname(item_map[rows$Item]), rows$Item)
  rows$Value <- vapply(as.character(rows$Value), function(value) {
    if (value %in% names(value_map)) return(unname(value_map[value]))
    value <- gsub("^converged=TRUE; admissible=TRUE$", "수렴=예; 허용 가능=예", value)
    value <- gsub("^converged=TRUE; admissible=FALSE$", "수렴=예; 허용 가능=아니오", value)
    value <- gsub("^converged=FALSE; admissible=TRUE$", "수렴=아니오; 허용 가능=예", value)
    value <- gsub("^converged=FALSE; admissible=FALSE$", "수렴=아니오; 허용 가능=아니오", value)
    value <- gsub("not recorded", "기록 없음", value, fixed = TRUE)
    value <- gsub("Requested", "요청됨", value, fixed = TRUE)
    value <- gsub("Executed", "실행됨", value, fixed = TRUE)
    value
  }, character(1))
  names(rows) <- c("항목", "값")
  rows
}
structural_canvas_reporting_context_result_ui <- function(bundle, analysis_type, language = statedu_initial_language()) {
  rows <- structural_canvas_reporting_context_rows(bundle, analysis_type)
  if (!nrow(rows)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  display_rows <- structural_canvas_reporting_context_display_rows(rows, ko)
  div(
    class = "result-section regression-result-panel structural-reporting-context",
    h4(if (ko) "보고 체크리스트" else "Reporting checklist"),
    structural_canvas_basic_html_table(display_rows),
    tags$p(
      class = "structural-result-note",
      if (ko) {
        "이 블록은 재현성 보고에 필요한 조건을 요약합니다: 추정량, 결측 처리, 분석 N, 순서형 지표, 부트스트랩 설정과 seed, 집단 분석, MI 기반 수정 여부."
      } else {
        "Use this block to report reproducibility conditions: estimator, missing-data handling, analyzed N, ordered indicators, bootstrap settings, seeds, group analysis, and whether MI-based changes are exploratory."
      }
    )
  )
}

structural_canvas_register_result_outputs <- function(input, output, prefix, canvas_output, analysis_type, selected_names_fn, variable_table_fn, dataset_fn, labels_fn, app_language_fn, fit_result, result_table) {
manuscript_result_table <- function(kind) {
  structural_canvas_result_table(kind, fit_result, analysis_type, labels_fn, function() "en")
}
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
    div(class = "result-section regression-result-panel", h4("1. Model overview"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview")))),
    div(class = "result-section regression-result-panel", h4(if (identical(analysis_type, "plssem")) {
      "2. PLS structural model effects"
    } else {
      "2. Model fit"
    }), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit"))), if (identical(analysis_type, "plssem")) tagList(
      tags$p(class = "structural-result-note", if (ko) "PLS-SEM 구조모형 출력은 공분산 기반 전역 적합도 지수 대신 경로계수, R2, 수정 R2, f2, Q2, q2, inner VIF, 총효과와 간접효과를 표시합니다." else "PLS-SEM structural output reports path coefficients, R2, adjusted R2, f2, Q2, q2, inner VIF, and total/indirect effects rather than covariance-based global fit indices."),
      tags$p(class = "structural-result-note", if (ko) "f2와 q2 해석 등급은 .02/.15/.35 기준의 descriptive small/medium/large 안내이며, 이론과 연구 맥락을 대체하지 않습니다." else "f2 and q2 size labels use the descriptive .02/.15/.35 small/medium/large anchors and do not replace theory or study context."),
      tags$p(class = "structural-result-note", if (ko) "Inner VIF는 seminr에서 내생 구성개념의 선행 예측자 정보를 반환할 때 직접 구조경로에 대해 표시됩니다." else "Inner VIF is reported for direct structural paths when an endogenous construct has antecedent predictors available from seminr."),
      if (length(bundle$diagnostics$ignored_covariances %||% character(0))) tags$p(class = "structural-result-note", if (ko) paste0("PLS-SEM에서는 공분산 경로를 추정하지 않으므로 다음 캔버스 공분산 경로를 제외했습니다: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), ".") else paste0("PLS-SEM does not estimate covariance paths, so these canvas covariance paths were excluded: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), "."))
    )),
    if (analysis_type %in% c("cbsem", "sem")) div(
      class = "result-section regression-result-panel structural-path-result",
      h4("3. Structural model paths"),
      div(class = "table-responsive", tableOutput(paste0(prefix, "_result_structural"))),
      tags$p(class = "structural-result-note", if (ko) "구조경로 표는 lavaan의 회귀경로(~)에 대한 비표준화 계수, 표준화 계수, 신뢰구간, R², z 및 p 값을 표시합니다." else "Structural paths are lavaan regression paths (~), reported with unstandardized and standardized coefficients, confidence intervals, R², z, and p values."),
      tags$p(class = "structural-result-note", if (ko) "매개 경로가 정의되면 간접효과와 총효과 행도 함께 표시되며, lavaan이 반환하는 경우 표준화 효과의 95% 신뢰구간을 포함합니다." else "When mediation paths are defined, indirect and total effect rows are included with standardized-effect 95% confidence intervals when lavaan returns them.")
    ),
    uiOutput(paste0(prefix, "_result_moderation_jn")),
    div(class = "result-section regression-result-panel structural-validity-result", h4("3. Latent construct correlations, reliability, and convergent/discriminant validity"), uiOutput(paste0(prefix, "_result_validity")), uiOutput(paste0(prefix, "_result_htmt"))),
    div(
      class = "result-section regression-result-panel structural-measurement-result",
      h4("4. Measurement model"),
      uiOutput(paste0(prefix, "_result_measurement")),
      if (identical(analysis_type, "plssem")) tagList(
        tags$p(class = "structural-result-note", if (ko) "PLS-SEM 측정모형 출력은 outer loading, outer weight, item VIF, 교차적재 요약, reflective/formative 측정모드를 표시합니다." else "PLS-SEM measurement output reports outer loadings, outer weights, item VIF, cross-loading summaries, and the reflective/formative measurement mode."),
        tags$p(class = "structural-result-note", if (ko) "Reflective 구성개념은 outer loading과 교차적재를 확인하고, formative 구성개념은 outer weight, item VIF, 지표의 이론적 포괄성을 우선 확인합니다." else "For reflective constructs, review outer loadings and cross-loadings. For formative constructs, prioritize outer weights, item VIF, and substantive indicator coverage."),
        tags$p(class = "structural-result-note", if (ko) "PLS bootstrap을 요청한 경우 loading과 weight의 CI/p 열은 사용 가능한 seminr percentile bootstrap 요약을 사용합니다." else "When PLS bootstrap is requested, loading and weight CI/p columns use seminr percentile bootstrap summaries when available.")
      ) else structural_canvas_symbol_footnotes(manuscript_result_table("measurement"))
    ),
    uiOutput(paste0(prefix, "_result_mi_section")),
    uiOutput(paste0(prefix, "_result_residuals")),
    div(class = "result-section regression-result-panel structural-supplementary-result",
      h4(if (ko) "보조 결과 및 진단" else "Supplementary results and diagnostics"),
      uiOutput(paste0(prefix, "_result_reporting_context")),
      uiOutput(paste0(prefix, "_result_identification")),
      uiOutput(paste0(prefix, "_result_normality")),
      uiOutput(paste0(prefix, "_result_missing_outliers")),
      uiOutput(paste0(prefix, "_result_risk_diagnostics")),
      uiOutput(paste0(prefix, "_result_heywood")),
      if (!identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_lavaan_quality")),
      if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_pls_quality")),
      uiOutput(paste0(prefix, "_result_pls_predict")),
      uiOutput(paste0(prefix, "_result_fit_guidance")),
      uiOutput(paste0(prefix, "_result_rmsea_tests")),
      uiOutput(paste0(prefix, "_result_information_criteria")),
      uiOutput(paste0(prefix, "_result_bollen_stine")),
      div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit_difference"))),
      uiOutput(paste0(prefix, "_result_invariance")),
      uiOutput(paste0(prefix, "_result_htmt_details")),
      uiOutput(paste0(prefix, "_result_latent_correlation_ci")),
      uiOutput(paste0(prefix, "_result_validity_note")),
      uiOutput(paste0(prefix, "_result_reliability_bootstrap")),
      uiOutput(paste0(prefix, "_result_factor_scores")),
      uiOutput(paste0(prefix, "_result_measurement_diagnostics"))
    ),
    uiOutput(paste0(prefix, "_result_higher_order")),
    uiOutput(paste0(prefix, "_result_mi_holdout")),
    uiOutput(paste0(prefix, "_result_mi_history"))
  )
})
  output[[paste0(prefix, "_result_reporting_context")]] <- renderUI({
    structural_canvas_reporting_context_result_ui(fit_result(), analysis_type, statedu_current_language(app_language_fn))
  })
  structural_canvas_register_fit_diagnostic_outputs(
    output, prefix, analysis_type, fit_result, manuscript_result_table, dataset_fn, app_language_fn
  )
  structural_canvas_register_validity_outputs(
    output, prefix, analysis_type, fit_result, manuscript_result_table, app_language_fn
  )
  if (analysis_type != "plssem") structural_canvas_register_local_fit_outputs(
    output, prefix, fit_result, app_language_fn
  )
  if (analysis_type %in% c("cbsem", "sem")) structural_canvas_register_moderation_outputs(
    output, prefix, fit_result, app_language_fn
  )
for (kind in c("overview", "structural")) local({
  result_kind <- kind
  output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(manuscript_result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
})
  output[[paste0(prefix, "_result_validity")]] <- renderUI({
    table <- manuscript_result_table("validity")
    tagList(
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-validity-table"),
      tags$p(class = "structural-result-note", "\u03b1 = Cronbach's alpha; \u03c9 total = omega total."),
      structural_canvas_symbol_footnotes(table)
    )
  })
  output[[paste0(prefix, "_result_measurement")]] <- renderUI({
    if (identical(analysis_type, "plssem")) {
      structural_canvas_basic_html_table(manuscript_result_table("measurement"))
    } else {
      structural_canvas_measurement_html_table(manuscript_result_table("measurement"))
    }
  })
  output[[paste0(prefix, "_result_measurement_diagnostics")]] <- renderUI({
    if (identical(analysis_type, "plssem")) return(NULL)
    diagnostics <- result_table("measurement_diagnostics")
    if (!nrow(diagnostics)) return(NULL)
    tagList(
      tags$h5(if (identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")) "표 4 가이드: 보조 측정 진단" else "Guide for Table 4: Supplementary measurement diagnostics"),
      structural_canvas_basic_html_table(diagnostics)
    )
  })
  output[[paste0(prefix, "_result_mi_section")]] <- renderUI({
    if (identical(analysis_type, "plssem")) return(NULL)
    table <- result_table("mi")
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    div(class = "result-section regression-result-panel structural-mi-result",
      h4("Modification indices (MI)"),
      uiOutput(paste0(prefix, "_result_mi"))
    )
  })
  structural_canvas_register_mi_render_outputs(
    output, prefix, fit_result, manuscript_result_table, app_language_fn
  )
  invisible(TRUE)
}
