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
      pls_algorithm <- bundle$diagnostics$estimator %||% bundle$estimator %||% "PLS"
      requested <- c(requested, paste0(pls_algorithm, " bootstrap R=", pls_r, ", seed=", bundle$pls_seed %||% "not recorded"))
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

structural_canvas_reporting_common_method_label <- function(bundle) {
  if (isTRUE(bundle$common_method_enabled)) {
    methods <- as.character(bundle$common_method_methods %||% character(0))
    if (!length(methods)) methods <- "requested"
    return(paste0("Executed: ", paste(methods, collapse = ", ")))
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
    algorithm <- bundle$diagnostics$estimator %||% bundle$estimator %||% "PLS"
    if (identical(toupper(as.character(algorithm)), "PLSC")) "PLSc path modeling" else "PLS path modeling"
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
    if (identical(toupper(as.character(bundle$estimator %||% "PLS")), "PLSC")) "PLSc consistency-corrected scores" else "Composite scores"
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
      "Analysis context", "Analysis engine", "Estimator or algorithm", "Missing-data handling",
      "Analyzed N", "Ordered indicators", "Latent scaling", "Bootstrap settings",
      "PLSpredict setting", "Group analysis", "Common method diagnostics", "MI holdout", "Syntax availability",
      "Admissibility and convergence"
    ),
    Value = c(
      context, engine, estimator, missing, structural_canvas_reporting_sample_size(bundle, analysis_type),
      ordered_label, scaling, structural_canvas_reporting_bootstrap_label(bundle, analysis_type),
      structural_canvas_reporting_predict_label(bundle, analysis_type), structural_canvas_reporting_group_label(bundle),
      structural_canvas_reporting_common_method_label(bundle), structural_canvas_reporting_holdout_label(bundle),
      syntax_label, structural_canvas_reporting_admissibility_label(bundle)
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
    "Admissibility and convergence" = "해의 허용성과 수렴"
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
    "PLSc path modeling" = "PLSc 경로모형",
    "Valid rows used by seminr; no FIML/pairwise option" = "seminr 유효 행 사용(FIML/pairwise 옵션 없음)",
    "Composite scores" = "합성점수",
    "PLSc consistency-corrected scores" = "PLSc 일관성 보정 점수",
    "Marker loading scaling" = "기준 적재량 고정",
    "std.lv = TRUE" = "잠재변수 분산 = 1",
    "Executed" = "실행함"
  )
  rows$Item <- ifelse(rows$Item %in% names(item_map), unname(item_map[rows$Item]), rows$Item)
  rows$Value <- vapply(as.character(rows$Value), function(value) {
    if (value %in% names(value_map)) return(unname(value_map[value]))
    value <- gsub("^converged=TRUE; admissible=TRUE$", "수렴=예; 허용 가능=예", value)
    value <- gsub("^converged=TRUE; admissible=FALSE$", "수렴=예; 허용 가능=아니오", value)
    value <- gsub("^converged=FALSE; admissible=TRUE$", "수렴=아니오; 허용 가능=예", value)
    value <- gsub("^converged=FALSE; admissible=FALSE$", "수렴=아니오; 허용 가능=아니오", value)
    value <- gsub("not recorded", "기록 없음", value, fixed = TRUE)
    value <- gsub("Requested", "요청함", value, fixed = TRUE)
    value <- gsub("Executed", "실행함", value, fixed = TRUE)
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
  table_number <- function(kind) {
    has_structural <- analysis_type %in% c("cbsem", "sem")
    switch(kind,
      overview = "1",
      fit = "2",
      structural = if (has_structural) "3" else NA_character_,
      validity = if (has_structural) "4" else "3",
      measurement = if (has_structural) "5" else "4",
      localfit = if (has_structural) "6" else "5",
      NA_character_
    )
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
      downloadButton(paste0(prefix, "_download_audit"), if (ko) "Audit JSON 다운로드" else "Download audit JSON", class = "btn btn-default btn-sm"),
      if (analysis_type %in% c("cfa", "cbsem", "sem")) downloadButton(paste0(prefix, "_download_reproducibility"), if (ko) "분석 기록 다운로드" else "Download analysis record", class = "btn btn-default btn-sm"),
      if (analysis_type %in% c("cfa", "cbsem", "sem")) downloadButton(paste0(prefix, "_download_tables"), if (ko) "결과표 Excel 다운로드" else "Download result tables", class = "btn btn-default btn-sm"),
      div(
        class = "result-section regression-result-panel",
        h4("1. Model overview"),
        div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview"))),
        if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_pls_fit_diagnostics"))
      ),
      div(
        class = "result-section regression-result-panel",
        h4(if (identical(analysis_type, "plssem")) "2. PLS structural model effects" else "2. Model fit"),
        div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit"))),
        if (identical(analysis_type, "plssem")) tagList(
          tags$p(class = "structural-result-note", "PLS-SEM structural effects are reported as path beta, indirect effect, total effect, R2, adjusted R2, and Q2. Effect-size labels, VIF, and bootstrap inference are reported in supplementary tables."),
          if (length(bundle$diagnostics$ignored_covariances %||% character(0))) tags$p(class = "structural-result-note", if (ko) paste0("PLS-SEM은 공분산 경로를 추정하지 않으므로 다음 캔버스 공분산 경로를 제외했습니다: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), ".") else paste0("PLS-SEM does not estimate covariance paths, so these canvas covariance paths were excluded: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), "."))
        )
      ),
      if (analysis_type %in% c("cbsem", "sem")) div(
        class = "result-section regression-result-panel structural-path-result",
        h4("3. Structural model paths"),
        uiOutput(paste0(prefix, "_result_structural")),
        tags$p(class = "structural-result-note", "Structural paths are lavaan regression paths (~), reported with unstandardized and standardized coefficients, R², z, and p values."),
        tags$p(class = "structural-result-note", "Indirect and total effects are reported separately when mediation paths are defined."),
        uiOutput(paste0(prefix, "_result_structural_effects")),
        uiOutput(paste0(prefix, "_result_structural_effect_ci"))
      ),
      uiOutput(paste0(prefix, "_result_moderation_jn")),
      div(
        class = "result-section regression-result-panel structural-validity-result",
        h4(paste0(table_number("validity"), ". Latent construct correlations, reliability, and convergent/discriminant validity")),
        uiOutput(paste0(prefix, "_result_validity")),
        uiOutput(paste0(prefix, "_result_htmt"))
      ),
      div(
        class = "result-section regression-result-panel structural-measurement-result",
        h4(paste0(table_number("measurement"), ". Measurement model")),
        uiOutput(paste0(prefix, "_result_measurement")),
        if (identical(analysis_type, "plssem")) tagList(
          tags$p(class = "structural-result-note", "loading/weight reports the outer loading for reflective indicators and the outer weight for formative indicators. VIF, cross-loading, and bootstrap inference are reported in supplementary tables.")
        ) else tagList(
          structural_canvas_abbreviation_footnotes(manuscript_result_table("measurement"), "measurement"),
          structural_canvas_symbol_footnotes(manuscript_result_table("measurement"))
        )
      ),
      uiOutput(paste0(prefix, "_result_mi_section")),
      uiOutput(paste0(prefix, "_result_residuals")),
      div(class = "result-section regression-result-panel landscape-table-panel structural-supplementary-result",
        h4(if (ko) "보조 결과 및 진단" else "Supplementary results and diagnostics"),
        uiOutput(paste0(prefix, "_result_reporting_context")),
        uiOutput(paste0(prefix, "_result_structural_effect_plan")),
        uiOutput(paste0(prefix, "_result_identification")),
        uiOutput(paste0(prefix, "_result_normality")),
        uiOutput(paste0(prefix, "_result_missing_outliers")),
        if (!identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_common_method")),
        uiOutput(paste0(prefix, "_result_risk_diagnostics")),
        uiOutput(paste0(prefix, "_result_heywood")),
        if (!identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_lavaan_quality")),
        if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_pls_quality")),
        uiOutput(paste0(prefix, "_result_pls_predict")),
        uiOutput(paste0(prefix, "_result_fit_guidance")),
        if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_fit_bootstrap")),
        uiOutput(paste0(prefix, "_result_rmsea_tests")),
        uiOutput(paste0(prefix, "_result_information_criteria")),
        uiOutput(paste0(prefix, "_result_bollen_stine")),
        div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit_difference"))),
        uiOutput(paste0(prefix, "_result_invariance")),
        uiOutput(paste0(prefix, "_result_htmt_details")),
        if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_validity_guide")),
        uiOutput(paste0(prefix, "_result_latent_correlation_ci")),
        uiOutput(paste0(prefix, "_result_validity_note")),
        uiOutput(paste0(prefix, "_result_reliability_bootstrap")),
        uiOutput(paste0(prefix, "_result_factor_scores")),
        uiOutput(paste0(prefix, "_result_measurement_ci")),
        uiOutput(paste0(prefix, "_result_measurement_diagnostics")),
        if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_redundancy")),
        if (identical(analysis_type, "cfa")) uiOutput(paste0(prefix, "_result_parcel_plan"))
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

  for (kind in c("overview")) local({
    result_kind <- kind
    output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(manuscript_result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
  })
  output[[paste0(prefix, "_result_structural")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    structural_canvas_basic_html_table(manuscript_result_table("structural"), class = "table table-striped table-bordered structural-path-table")
  })
  output[[paste0(prefix, "_result_structural_effects")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- result_table("structural_effects")
    if (!nrow(table)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(
      class = "structural-effect-summary-block",
      tags$h5(if (ko) "표 3 보조: 직접효과, 간접효과, 총효과" else "Supplementary Table 3: Direct, indirect, and total effects"),
      structural_canvas_effect_summary_html_table(table, ci = FALSE)
    )
  })
  output[[paste0(prefix, "_result_structural_effect_ci")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- result_table("structural_effect_ci")
    if (!nrow(table)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(
      class = "structural-effect-summary-block",
      tags$h5(if (ko) "표 3 보조: 효과 beta 95% 신뢰구간" else "Supplementary Table 3: Effect beta 95% confidence intervals"),
      structural_canvas_effect_summary_html_table(table, ci = TRUE)
    )
  })
  output[[paste0(prefix, "_result_validity")]] <- renderUI({
    table <- manuscript_result_table("validity")
    tagList(
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-validity-table"),
      structural_canvas_abbreviation_footnotes(table, "validity"),
      structural_canvas_symbol_footnotes(table)
    )
  })
  output[[paste0(prefix, "_result_measurement")]] <- renderUI({
    if (identical(analysis_type, "plssem")) {
      structural_canvas_basic_html_table(manuscript_result_table("measurement"), class = "table table-striped table-bordered structural-pls-measurement-main-table")
    } else {
      structural_canvas_measurement_html_table(manuscript_result_table("measurement"))
    }
  })
  output[[paste0(prefix, "_result_measurement_diagnostics")]] <- renderUI({
    if (identical(analysis_type, "plssem")) {
      diagnostics <- result_table("measurement_guide")
      if (!is.data.frame(diagnostics) || !nrow(diagnostics)) return(NULL)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      return(tagList(
        tags$h5(if (ko) paste0("표 ", table_number("measurement"), " 보조: PLS 측정 진단") else paste0("Supplementary Table ", table_number("measurement"), ": PLS measurement diagnostics")),
        structural_canvas_basic_html_table(diagnostics, class = "table table-striped table-bordered structural-pls-measurement-guide-table"),
        tags$p(class = "structural-result-note", if (ko) "반영지표는 outer loading과 교차적재를, 형성지표는 outer weight와 item VIF를 우선 검토합니다." else "For reflective indicators, review outer loadings and cross-loadings; for formative indicators, prioritize outer weights and item VIF.")
      ))
    }
    diagnostics <- result_table("measurement_diagnostics")
    if (!nrow(diagnostics)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    tagList(
      tags$h5(if (ko) paste0("표 ", table_number("measurement"), " 가이드: 보조 측정 진단") else paste0("Guide for Table ", table_number("measurement"), ": Supplementary measurement diagnostics")),
      structural_canvas_basic_html_table(diagnostics)
    )
  })
  output[[paste0(prefix, "_result_structural_effect_plan")]] <- renderUI({
    bundle <- fit_result()
    plan <- bundle$structural_effect_plan %||% bundle$diagnostics$structural_effect_plan %||% data.frame()
    if (!nrow(plan)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(
      class = "result-section structural-effect-capability-result",
      tags$h5(if (ko) "구조효과 지원 범위" else "Structural-effect capability plan"),
      tags$p(class = "structural-result-note", if (ko) "요청한 효과가 선택한 엔진에서 실제로 추정되는지 확인하는 사전 기록입니다." else "This preflight record confirms whether requested effects are actually estimated by the selected engine."),
      structural_canvas_basic_html_table(plan, class = "table table-striped table-bordered")
    )
  })
  output[[paste0(prefix, "_result_redundancy")]] <- renderUI({
    if (!identical(analysis_type, "plssem")) return(NULL)
    result <- fit_result()$redundancy_result %||% list(available = FALSE, reason = "Redundancy analysis was not requested.")
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    if (!isTRUE(result$available)) return(tagList(
      tags$h5(if (ko) "형성형 Redundancy analysis" else "Formative redundancy analysis"),
      tags$p(class = "structural-result-note", if (ko) paste0("미평가: ", result$reason %||% "전역 기준변수를 선택하지 않았습니다.") else paste0("Not assessed: ", result$reason %||% "no global criterion was selected."))
    ))
    table <- data.frame(
      Construct = result$construct,
      Criterion = result$criterion,
      N = result$n,
      Loading = format_decimal3(result$loading),
      `95% CI lower` = format_decimal3(result$ci_lower),
      `95% CI upper` = format_decimal3(result$ci_upper),
      R2 = format_decimal3(result$r2),
      Guidance = result$guidance,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (ko) names(table) <- c("합성변수", "전역 기준변수", "N", "적재량", "95% CI 하한", "95% CI 상한", "R²", "해석")
    tagList(
      tags$h5(if (ko) "형성형 Redundancy analysis" else "Formative redundancy analysis"),
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-redundancy-table"),
      tags$p(class = "structural-result-note", if (ko) ".70은 설명용 참고값이며 자동 합격선이 아닙니다. 기준변수가 동일 개념을 충분히 포괄하고 형성지표와 독립적으로 측정되었는지 함께 검토하십시오." else ".70 is a descriptive reference, not an automatic pass rule. Also verify that the criterion adequately covers the same concept and was measured independently of the formative indicators.")
    )
  })
  output[[paste0(prefix, "_result_parcel_plan")]] <- renderUI({
    if (!identical(analysis_type, "cfa")) return(NULL)
    result <- fit_result()$parcel_result %||% list(enabled = FALSE)
    if (!isTRUE(result$enabled)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    if (!isTRUE(result$available)) return(tagList(
      tags$h5(if (ko) "Parcel 계획 안전성 점검" else "Parcel-plan safety review"),
      tags$p(class = "structural-result-note", if (ko) paste0("미리보기 생성 불가: ", result$reason) else paste0("Preview unavailable: ", result$reason))
    ))
    allocation <- result$allocation
    summary <- result$summary
    allocation$Loading <- vapply(allocation$Loading, format_decimal3, character(1))
    summary[["Mean absolute loading"]] <- vapply(summary[["Mean absolute loading"]], format_decimal3, character(1))
    if (ko) {
      names(allocation) <- c("Parcel", "문항", "표준화 적재량")
      names(summary) <- c("Parcel", "평균 절대 적재량", "문항")
    }
    tagList(
      tags$h5(if (ko) "Parcel 계획 안전성 점검" else "Parcel-plan safety review"),
      tags$p(tags$b(if (ko) "기록된 목적: " else "Recorded purpose: "), result$purpose),
      tags$p(class = "structural-result-note", paste0(if (ko) "상태: " else "Status: ", result$status, ". ", result$warning)),
      tags$p(class = "structural-result-note", paste0(if (ko) "문항수준 최소 |적재량| = " else "Item-level minimum |loading| = ", format_decimal3(result$min_loading), if (ko) "; 최대 절대 잔차상관 = " else "; maximum absolute residual correlation = ", format_decimal3(result$max_residual_correlation), ".")),
      tags$h6(if (ko) "배정 미리보기" else "Allocation preview"),
      structural_canvas_basic_html_table(allocation, class = "table table-striped table-bordered structural-parcel-allocation-table"),
      tags$h6(if (ko) "Parcel 균형 요약" else "Parcel balance summary"),
      structural_canvas_basic_html_table(summary, class = "table table-striped table-bordered structural-parcel-summary-table"),
      tags$p(class = "structural-result-note", if (ko) "데이터셋에 parcel 변수는 생성되지 않았습니다. 실제 적용 전 이론적 동질성, 국소의존, 다른 배정 방식에 대한 민감도와 item-level 결과를 함께 검토해야 합니다." else "No parcel variables were created. Before implementation, review substantive item homogeneity, local dependence, sensitivity to alternative allocations, and the item-level results.")
    )
  })
  output[[paste0(prefix, "_result_measurement_ci")]] <- renderUI({
    if (identical(analysis_type, "plssem")) {
      ci_table <- result_table("measurement_bootstrap")
      if (!is.data.frame(ci_table) || !nrow(ci_table)) return(NULL)
      value_columns <- setdiff(names(ci_table), c("Construct", "Construct type", "Indicator", "Mode"))
      if (!length(value_columns) || !any(nzchar(as.character(unlist(ci_table[value_columns], use.names = FALSE))))) return(NULL)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      return(tagList(
        tags$h5(if (ko) paste0("표 ", table_number("measurement"), " 보조: PLS 측정모형 부트스트랩") else paste0("Supplementary Table ", table_number("measurement"), ": PLS measurement bootstrap")),
        structural_canvas_basic_html_table(ci_table, class = "table table-striped table-bordered structural-pls-measurement-bootstrap-table"),
        tags$p(class = "structural-result-note", if (ko) "outer loading과 outer weight의 percentile bootstrap CI, t, p 값입니다. seminr가 반환한 항목만 표시됩니다." else "Percentile bootstrap CI, t, and p values for outer loadings and outer weights are shown when seminr returns them.")
      ))
    }
    ci_table <- result_table("measurement_ci")
    if (!nrow(ci_table)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    tagList(
      tags$h5(if (ko) paste0("표 ", table_number("measurement"), " 보조: 측정모형 95% 신뢰구간") else paste0("Supplementary Table ", table_number("measurement"), ": Measurement model 95% confidence intervals")),
      structural_canvas_measurement_ci_html_table(ci_table)
    )
  })
  output[[paste0(prefix, "_result_common_method")]] <- renderUI({
    if (identical(analysis_type, "plssem")) return(NULL)
    bundle <- fit_result()
    if (!isTRUE(bundle$common_method_enabled)) return(NULL)
    table <- result_table("common_method")
    common_method <- bundle$common_method_result %||% list()
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    if (!is.data.frame(table) || !nrow(table)) {
      return(tagList(
        tags$h5(if (ko) "동일방법편의 진단" else "Common method bias diagnostics"),
        tags$p(
          class = "structural-result-note",
          if (ko) {
            "동일방법편의 진단을 요청했지만 현재 모형에서 표시할 결과를 계산하지 못했습니다. 모형 수렴, 식별성, 관측변수 수를 확인하십시오."
          } else {
            "Common method bias diagnostics were requested, but no displayable result could be computed for the current model."
          }
        )
      ))
    }
    fit_table <- as.data.frame(common_method$fit %||% data.frame(), check.names = FALSE)
    comparison_table <- as.data.frame(common_method$comparison %||% data.frame(), check.names = FALSE)
    loading_change <- as.data.frame(common_method$loading_change %||% data.frame(), check.names = FALSE)
    conclusion_table <- structural_canvas_common_method_conclusion(common_method, statedu_current_language(app_language_fn))
    if (ko && nrow(conclusion_table)) {
      names(conclusion_table) <- c(Status = "판정", Guidance = "근거")[names(conclusion_table)]
    }
    translate_values <- function(values, labels) {
      values <- as.character(values %||% character(0))
      translated <- unname(labels[values])
      ifelse(is.na(translated), values, translated)
    }
    if (nrow(comparison_table)) {
      numeric_columns <- names(comparison_table)[vapply(comparison_table, is.numeric, logical(1))]
      for (column in numeric_columns) comparison_table[[column]] <- vapply(comparison_table[[column]], format_decimal3, character(1))
      if ("Delta p" %in% names(comparison_table)) {
        comparison_table[["Delta p"]] <- vapply(suppressWarnings(as.numeric(comparison_table[["Delta p"]])), format_p, character(1))
      }
      if (ko) {
        if ("Comparison" %in% names(comparison_table)) {
          comparison_table$Comparison <- translate_values(comparison_table$Comparison, c(
            `Single_factor_CFA vs Research_model` = "단일요인 CFA vs 연구모형",
            `Common_latent_factor vs Research_model` = "공통잠재요인 vs 연구모형"
          ))
        }
        if ("Note" %in% names(comparison_table)) {
          comparison_table$Note <- translate_values(comparison_table$Note, c(
            `Single-factor CFA is a diagnostic alternative model; use differences as screening evidence, not as a strict nested-model test.` = "단일요인 CFA는 진단용 대안모형입니다. 차이값은 엄격한 중첩모형 검정이 아니라 점검 근거로 해석하십시오.",
            `Common latent factor comparison screens whether fit and loadings change after adding the method factor.` = "공통잠재요인 비교는 방법요인 추가 후 적합도와 적재량이 얼마나 바뀌는지 점검합니다."
          ))
        }
        translated_names <- c(
          Comparison = "비교",
          `Delta chisq` = "Δχ²",
          `Delta df` = "Δdf",
          `Delta p` = "Δp",
          `Delta CFI` = "ΔCFI",
          `Delta RMSEA` = "ΔRMSEA",
          `Delta SRMR` = "ΔSRMR",
          Note = "해석 주의"
        )[names(comparison_table)]
        names(comparison_table) <- ifelse(is.na(translated_names), names(comparison_table), unname(translated_names))
      }
    }
    if (nrow(fit_table)) {
      numeric_columns <- names(fit_table)[vapply(fit_table, is.numeric, logical(1))]
      for (column in numeric_columns) fit_table[[column]] <- vapply(fit_table[[column]], format_decimal3, character(1))
      if ("p" %in% names(fit_table)) fit_table$p <- vapply(suppressWarnings(as.numeric(fit_table$p)), format_p, character(1))
      if (ko && "Model" %in% names(fit_table)) {
        fit_table$Model <- translate_values(fit_table$Model, c(
          Research_model = "연구모형",
          Single_factor_CFA = "단일요인 CFA",
          Common_latent_factor = "공통잠재요인"
        ))
        names(fit_table)[names(fit_table) == "Model"] <- "모형"
      }
    }
    if (nrow(loading_change)) {
      numeric_columns <- names(loading_change)[vapply(loading_change, is.numeric, logical(1))]
      for (column in numeric_columns) loading_change[[column]] <- vapply(loading_change[[column]], format_decimal3, character(1))
      if (ko) {
        translated_names <- c(
          Latent = "잠재변수",
          Indicator = "관측변수",
          `Baseline beta` = "기준 beta",
          `Method-adjusted beta` = "방법보정 beta",
          `Absolute change` = "절대 변화량",
          `Method factor beta` = "방법요인 beta"
        )[names(loading_change)] %||% names(loading_change)
        names(loading_change) <- ifelse(is.na(translated_names), names(loading_change), unname(translated_names))
      }
    }
    tagList(
      tags$h5(if (ko) "동일방법편의 진단" else "Common method bias diagnostics"),
      if (nrow(conclusion_table)) tagList(
        tags$h6(if (ko) "판정 요약" else "Conclusion"),
        structural_canvas_basic_html_table(conclusion_table, class = "table table-striped table-bordered structural-common-method-conclusion-table")
      ),
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-common-method-table"),
      if (nrow(fit_table)) tagList(
        tags$h6(if (ko) "모형 적합도 비교" else "Model fit comparison"),
        structural_canvas_basic_html_table(fit_table, class = "table table-striped table-bordered structural-common-method-fit-table")
      ),
      if (nrow(comparison_table)) tagList(
        tags$h6(if (ko) "모형 차이 비교" else "Model difference comparison"),
        structural_canvas_basic_html_table(comparison_table, class = "table table-striped table-bordered structural-common-method-comparison-table")
      ),
      if (nrow(loading_change)) tagList(
        tags$h6(if (ko) "공통잠재요인 적재량 변화" else "Common latent factor loading changes"),
        structural_canvas_basic_html_table(loading_change, class = "table table-striped table-bordered structural-common-method-loading-table")
      ),
      tags$p(
        class = "structural-result-note",
        if (ko) {
          "동일방법편의 진단은 편의 가능성을 점검하는 증거입니다. 편의가 없다는 증명으로 해석하지 말고, 심각한 동일방법 집중 여부를 판단하는 보조 근거로 보고하십시오."
        } else {
          "These diagnostics screen for common method bias. They should be reported as evidence for or against serious common-method concentration, not as proof that common method bias is absent."
        }
      )
    )
  })
  output[[paste0(prefix, "_result_mi_section")]] <- renderUI({
    if (identical(analysis_type, "plssem")) return(NULL)
    table <- result_table("mi")
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    div(class = "result-section regression-result-panel landscape-table-panel structural-mi-result",
      h4("Modification indices (MI)"),
      uiOutput(paste0(prefix, "_result_mi"))
    )
  })
  structural_canvas_register_mi_render_outputs(
    output, prefix, fit_result, manuscript_result_table, app_language_fn
  )
  invisible(TRUE)
}
