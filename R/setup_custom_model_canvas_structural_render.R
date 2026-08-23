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

structural_canvas_ml_likelihood_label <- function(bundle) {
  estimator <- toupper(as.character(bundle$estimator %||% structural_canvas_reporting_lavaan_option(bundle, "estimator", "")))
  if (!identical(estimator, "ML")) return("Not applicable to the selected estimator")
  convention <- tolower(as.character(bundle$ml_likelihood %||% structural_canvas_reporting_lavaan_option(bundle, "likelihood", "normal")))
  if (identical(convention, "wishart")) {
    "Wishart ML (unbiased covariance; N-1 chi-square multiplier; AMOS/LISREL/EQS compatible)"
  } else {
    "Normal ML (biased covariance; N chi-square multiplier; lavaan default)"
  }
}

structural_canvas_reporting_bootstrap_label <- function(bundle, analysis_type) {
  requested <- character(0)
  if (identical(analysis_type, "plssem")) {
    pls_r <- suppressWarnings(as.integer(bundle$pls_bootstrap %||% 0L))
    if (is.finite(pls_r) && pls_r > 0L) {
      pls_algorithm <- bundle$diagnostics$estimator %||% bundle$estimator %||% "PLS"
      bootstrap <- bundle$pls_bootstrap_result %||% list()
      actual <- suppressWarnings(as.integer(bootstrap$nboot %||% 0L))
      valid_ratio <- suppressWarnings(as.numeric(bootstrap$valid_ratio %||% NA_real_))
      status <- as.character(bootstrap$bootstrap_status %||% if (is.finite(valid_ratio) && valid_ratio >= .80) "Adequate" else "Not recorded")
      rng <- as.character(bootstrap$rng %||% "L'Ecuyer-CMRG independent stream per requested position")
      actual_label <- paste0(", valid=", if (is.finite(actual)) actual else "NA", "/", pls_r, " (", if (is.finite(valid_ratio)) formatC(100 * valid_ratio, format = "fg", digits = 4) else "NA", "%), status=", status)
      requested <- c(requested, paste0(pls_algorithm, " bootstrap R=", pls_r, ", seed=", bundle$pls_seed %||% "not recorded", ", RNG=", rng, actual_label, ", whole-draw minimum=80%"))
    }
  } else {
    rel_r <- suppressWarnings(as.integer(bundle$reliability_bootstrap %||% 0L))
    htmt_r <- suppressWarnings(as.integer(bundle$htmt_bootstrap %||% 0L))
    bs_r <- suppressWarnings(as.integer(bundle$bollen_stine_bootstrap %||% 0L))
    if (is.finite(rel_r) && rel_r > 0L) {
      requested <- c(requested, paste0("Reliability/AVE R=", rel_r, ", CI=", bundle$reliability_ci_method %||% "bias_corrected", ", quantile=R type ", structural_canvas_bootstrap_quantile_type(bundle$reliability_ci_method %||% "bias_corrected", "reliability"), ", seed=", bundle$reliability_seed %||% "not recorded"))
    }
    if (is.finite(htmt_r) && htmt_r > 0L) {
      requested <- c(requested, paste0("HTMT R=", htmt_r, ", CI=", bundle$htmt_ci_method %||% "bias_corrected", ", quantile=R type ", structural_canvas_bootstrap_quantile_type(bundle$htmt_ci_method %||% "bias_corrected", "htmt"), ", seed=", bundle$htmt_seed %||% "not recorded"))
    }
    if (is.finite(bs_r) && bs_r > 0L) {
      requested <- c(requested, paste0("Bollen-Stine R=", bs_r, ", seed=", bundle$bollen_stine_seed %||% "not recorded"))
    }
    effect_r <- suppressWarnings(as.integer(bundle$effect_bootstrap %||% 0L))
    if (is.finite(effect_r) && effect_r > 0L) {
      requested <- c(requested, paste0("Path/indirect/total-effect R=", effect_r, ", CI=", bundle$effect_bootstrap_ci_method %||% "bias_corrected", ", quantile=R type ", structural_canvas_bootstrap_quantile_type(bundle$effect_bootstrap_ci_method %||% "bias_corrected", "structural_effects"), ", seed=", bundle$effect_bootstrap_seed %||% "not recorded"))
    }
  }
  if (!length(requested)) "Not requested" else paste(requested, collapse = "; ")
}

structural_canvas_reporting_predict_label <- function(bundle, analysis_type) {
  if (!identical(analysis_type, "plssem")) return("Not applicable")
  result <- bundle$pls_predict_result
  if (is.list(result)) {
    return(paste0("Executed: folds=", result$folds %||% "", ", reps=", result$reps %||% "", ", seed=", result$seed %||% bundle$pls_predict_seed %||% "not recorded"))
  }
  folds <- suppressWarnings(as.integer(bundle$pls_predict_folds %||% 0L))
  reps <- suppressWarnings(as.integer(bundle$pls_predict_reps %||% 0L))
  if (is.finite(folds) && folds > 1L) return(paste0("Requested: folds=", folds, ", reps=", reps, ", seed=", bundle$pls_predict_seed %||% "not recorded"))
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
    label <- if (identical(toupper(as.character(algorithm)), "PLSC")) "PLSc path modeling" else "PLS path modeling"
    requested <- toupper(as.character(bundle$diagnostics$estimator_requested %||% bundle$estimator_requested %||% ""))
    mode <- as.character(bundle$diagnostics$estimator_selection_mode %||% bundle$estimator_selection_mode %||% "")
    if (nzchar(mode)) paste0(label, " (", if (identical(requested, "AUTO")) "rule-based recommendation accepted; " else "", mode, ")") else label
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
    if (identical(toupper(as.character(bundle$estimator %||% "PLS")), "PLSC")) {
      mode <- as.character(bundle$diagnostics$estimator_selection_mode %||% bundle$estimator_selection_mode %||% "")
      if (grepl("Mixed model", mode, fixed = TRUE)) "Mixed PLSc: common-factor blocks corrected; composite blocks uncorrected" else "PLSc consistency-corrected common-factor scores"
    } else "Composite scores"
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
      "Analysis context", "Sampling design", "Analysis engine", "Estimator or algorithm", "ML likelihood convention", "Missing-data handling", "Missing-data sensitivity",
      "Analyzed N", "Ordered indicators", "Latent scaling", "Bootstrap settings",
      "PLSpredict setting", "Group analysis", "Common method diagnostics", "MI holdout", "Syntax availability",
      "Admissibility and convergence"
    ),
    Value = c(
      context, bundle$sampling_design_gate$label %||% "Not recorded", engine, estimator,
      if (identical(analysis_type, "plssem")) "Not applicable" else structural_canvas_ml_likelihood_label(bundle), missing,
      if (identical(analysis_type, "plssem")) "Not applicable to the current complete-row PLS workflow" else paste0(structural_canvas_missing_sensitivity_rows(bundle)$`Sensitivity assessment`[[1L]], "; status=", structural_canvas_missing_sensitivity_rows(bundle)$Status[[1L]]),
      structural_canvas_reporting_sample_size(bundle, analysis_type),
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
    "Sampling design" = "표본·관측 구조",
    "Analysis engine" = "분석 엔진",
    "Estimator or algorithm" = "추정량/알고리즘",
    "Missing-data handling" = "결측 처리",
    "Missing-data sensitivity" = "결측 민감도",
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
    "Independent cross-sectional observations" = "독립 관측 횡단자료",
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

structural_canvas_construct_reporting_rows <- function(bundle, analysis_type, ko = FALSE) {
  display_names <- if (ko) {
    c("구성개념", "선언 유형", "측정 방향", "요청 가중", "실제 가중", "엔진 표현", "추정대상", "마이그레이션")
  } else {
    c("Construct", "Declared type", "Measurement direction", "Requested weighting", "Effective weighting", "Engine representation", "Estimand", "Migration")
  }
  rows <- bundle$resolved_construct_specification %||% structural_canvas_resolve_construct_specification(
    bundle$snapshot %||% list(), analysis_type, bundle$estimator %||% NULL
  )
  if (!is.data.frame(rows) || !nrow(rows)) {
    empty <- as.data.frame(stats::setNames(rep(list(character(0)), length(display_names)), display_names), check.names = FALSE)
    return(empty)
  }
  defaults <- list(
    weighting_mode = "auto", effective_weighting = "Not recorded", engine_representation = "Not recorded",
    estimand = "Not recorded", specification_migration = ""
  )
  for (column in names(defaults)) if (!column %in% names(rows)) rows[[column]] <- defaults[[column]]
  display <- rows[, c(
    "name", "construct_type", "measurement_mode", "weighting_mode", "effective_weighting",
    "engine_representation", "estimand", "specification_migration"
  ), drop = FALSE]
  display$specification_migration[!nzchar(display$specification_migration)] <- if (ko) "없음" else "None"
  names(display) <- display_names
  display
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
    tags$h5(if (ko) "구성개념 명세와 실제 계산 표현" else "Construct specification and computational representation"),
    structural_canvas_basic_html_table(structural_canvas_construct_reporting_rows(bundle, analysis_type, ko)),
    tags$p(
      class = "structural-result-note",
      if (ko) {
        "이 블록은 재현성 보고에 필요한 조건과 구성개념별 선언 유형, 실제 가중법, 엔진 표현, 추정대상 및 저장 모형 마이그레이션 이력을 요약합니다."
      } else {
        "Use this block to report reproducibility conditions and, for every construct, the declared type, effective weighting, engine representation, estimand, and saved-model migration history."
      }
    )
  )
}

structural_canvas_register_result_outputs <- function(input, output, prefix, canvas_output, analysis_type, selected_names_fn, variable_table_fn, dataset_fn, labels_fn, app_language_fn, fit_result, result_table) {
  supplementary_ready <- reactiveVal(FALSE)
  observeEvent(fit_result(), {
    supplementary_ready(FALSE)
    later::later(function() supplementary_ready(TRUE), delay = 0.20)
  }, ignoreNULL = TRUE)
  manuscript_result_table <- function(kind) {
    result_table(kind, "en")
  }
  table_number <- function(kind) {
    has_structural <- analysis_type %in% c("cbsem", "sem")
    if (identical(analysis_type, "plssem")) {
      return(switch(kind,
        overview = "1",
        fit_diagnostics = "2",
        fit = "3",
        structural = NA_character_,
        validity = "4",
        measurement = "5",
        localfit = "6",
        NA_character_
      ))
    }
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
        div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview")))
      ),
      if (identical(analysis_type, "plssem")) div(
        class = "result-section regression-result-panel structural-pls-fit-diagnostics-result",
        h4("2. PLS/PLSc model fit diagnostics"),
        uiOutput(paste0(prefix, "_result_pls_fit_diagnostics"))
      ),
      div(
        class = "result-section regression-result-panel",
        h4(if (identical(analysis_type, "plssem")) "3. PLS structural model effects" else "2. Model fit"),
        div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit"))),
        if (identical(analysis_type, "plssem")) tagList(
          tags$p(class = "structural-result-note", "The main table combines direct path estimates, bootstrap inference, local effect size, destination-construct explanatory/predictive indices, and inner VIF. Indirect and total effects, confidence intervals, effect-size labels, and BH-adjusted p values remain in supplementary tables; prespecified primary hypotheses should still be distinguished from exploratory tests."),
          if (length(bundle$diagnostics$ignored_covariances %||% character(0))) tags$p(class = "structural-result-note", if (ko) paste0("PLS-SEM은 공분산 경로를 추정하지 않으므로 다음 캔버스 공분산 경로를 제외했습니다: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), ".") else paste0("PLS-SEM does not estimate covariance paths, so these canvas covariance paths were excluded: ", paste(bundle$diagnostics$ignored_covariances, collapse = ", "), "."))
        )
      ),
      if (analysis_type %in% c("cfa", "cbsem", "sem")) uiOutput(paste0(prefix, "_result_covariate_section")),
      if (analysis_type %in% c("cbsem", "sem")) div(
        class = "result-section regression-result-panel structural-path-result",
        h4(if (ko) "표 3. 구조모형 경로" else "Table 3. Structural model paths"),
        uiOutput(paste0(prefix, "_result_structural"))
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
        if (identical(analysis_type, "plssem")) NULL else tagList(
          structural_canvas_abbreviation_footnotes(manuscript_result_table("measurement"), "measurement"),
          structural_canvas_symbol_footnotes(manuscript_result_table("measurement"))
        )
      ),
      if (analysis_type %in% c("cbsem", "sem")) uiOutput(paste0(prefix, "_result_specific_indirect")),
      uiOutput(paste0(prefix, "_result_mi_section")),
      uiOutput(paste0(prefix, "_result_residuals")),
      uiOutput(paste0(prefix, "_result_supplementary_container")),
      uiOutput(paste0(prefix, "_result_higher_order")),
      uiOutput(paste0(prefix, "_result_mi_holdout")),
      uiOutput(paste0(prefix, "_result_mi_history"))
    )
  })

  output[[paste0(prefix, "_result_reporting_context")]] <- renderUI({
    structural_canvas_reporting_context_result_ui(fit_result(), analysis_type, statedu_current_language(app_language_fn))
  })

  output[[paste0(prefix, "_result_covariate_section")]] <- renderUI({
    bundle <- fit_result()
    covariates <- bundle$covariates %||% character(0)
    if (!length(covariates)) return(NULL)
    labels <- labels_fn() %||% character(0)
    display_name <- structural_canvas_display_name_resolver(
      snapshot = bundle$snapshot %||% list(),
      variable_table = variable_table_fn(),
      labels = labels,
      moderation_definitions = bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list(),
      language = statedu_current_language(app_language_fn)
    )
    effects <- structural_canvas_covariate_effect_table(bundle$fit, covariates, display_name)
    comparison <- bundle$covariate_fit_comparison %||% data.frame()
    format_table <- function(table) {
      if (!nrow(table)) return(table)
      for (name in names(table)) if (is.numeric(table[[name]])) {
        table[[name]] <- if (identical(name, "p")) vapply(table[[name]], format_p, character(1)) else vapply(table[[name]], format_decimal3, character(1))
      }
      table
    }
    if (!nrow(effects) && !nrow(comparison)) return(NULL)
    div(
      class = "result-section regression-result-panel structural-covariate-result",
      tags$h4("Covariate-adjusted model"),
      if (nrow(effects)) tagList(tags$h5("Covariate effects"), structural_canvas_basic_html_table(format_table(effects), class = "table table-striped table-bordered structural-covariate-effect-table")),
      if (nrow(comparison)) tagList(tags$h5("Research-model and covariate-adjusted-model fit comparison"), structural_canvas_basic_html_table(format_table(comparison), class = "table table-striped table-bordered structural-covariate-fit-table")),
      tags$p(class = "structural-result-note", "For robust estimators, model rows use scaled chi-square/p and robust CFI, TLI, and RMSEA when available; SRMR remains the standard residual index. Delta chi-square, delta df, and p come from the nested-model likelihood-ratio difference test, while delta CFI, TLI, RMSEA, and SRMR are covariate-adjusted model minus research model.")
    )
  })

  output[[paste0(prefix, "_result_supplementary_container")]] <- renderUI({
    shiny::req(supplementary_ready())
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(class = "result-section regression-result-panel landscape-table-panel structural-supplementary-result",
      h4(if (ko) "보조 결과 및 진단" else "Supplementary results and diagnostics"),
      if (analysis_type %in% c("cbsem", "sem")) tagList(
        uiOutput(paste0(prefix, "_result_structural_effects")),
        uiOutput(paste0(prefix, "_result_structural_effect_ci"))
      ),
      uiOutput(paste0(prefix, "_result_reporting_context")),
      uiOutput(paste0(prefix, "_result_causal_interpretation")),
      uiOutput(paste0(prefix, "_result_structural_effect_plan")),
      if (analysis_type %in% c("cbsem", "sem")) uiOutput(paste0(prefix, "_result_effect_bootstrap")),
      uiOutput(paste0(prefix, "_result_identification")), uiOutput(paste0(prefix, "_result_normality")),
      uiOutput(paste0(prefix, "_result_missing_outliers")),
      if (!identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_common_method")),
      uiOutput(paste0(prefix, "_result_risk_diagnostics")), uiOutput(paste0(prefix, "_result_heywood")),
      if (!identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_lavaan_quality")),
      if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_pls_quality")),
      uiOutput(paste0(prefix, "_result_pls_predict")), uiOutput(paste0(prefix, "_result_fit_guidance")),
      if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_fit_bootstrap")),
      uiOutput(paste0(prefix, "_result_rmsea_tests")), uiOutput(paste0(prefix, "_result_information_criteria")),
      uiOutput(paste0(prefix, "_result_bollen_stine")),
      div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit_difference"))),
      uiOutput(paste0(prefix, "_result_invariance")), uiOutput(paste0(prefix, "_result_htmt_details")),
      if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_validity_guide")),
      uiOutput(paste0(prefix, "_result_latent_correlation_ci")), uiOutput(paste0(prefix, "_result_validity_note")),
      uiOutput(paste0(prefix, "_result_reliability_bootstrap")), uiOutput(paste0(prefix, "_result_factor_scores")),
      uiOutput(paste0(prefix, "_result_measurement_ci")), uiOutput(paste0(prefix, "_result_measurement_diagnostics")),
      if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_redundancy")),
      if (identical(analysis_type, "cfa")) uiOutput(paste0(prefix, "_result_parcel_plan"))
    )
  })
  output[[paste0(prefix, "_result_causal_interpretation")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem", "plssem")) return(NULL)
    bundle <- fit_result()
    interpretation <- structural_canvas_causal_interpretation(bundle$snapshot %||% list(), analysis_type)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    rows <- interpretation$rows
    if (ko && nrow(rows)) {
      names(rows) <- c("인과식별 가정", "현재 기록", "보고상 제한")
    }
    div(
      class = "result-section structural-causal-interpretation-result",
      tags$h5(if (ko) "인과해석 경계" else "Causal-interpretation boundary"),
      tags$p(class = "structural-result-note", if (ko) {
        "현재 분석은 인과식별을 확립하지 않으므로 경로계수와 간접효과를 이론에 의해 방향을 정한 통계적 연관으로 해석하십시오. 유의한 계수, bootstrap 신뢰구간 또는 좋은 적합도만으로 인과성이나 시간적 선행성이 성립하지 않습니다."
      } else {
        "This analysis does not establish causal identification. Interpret path coefficients and indirect effects as theory-directed statistical associations. Significant coefficients, bootstrap intervals, or good fit alone do not establish causality or temporal precedence."
      }),
      structural_canvas_basic_html_table(rows, class = "table table-striped table-bordered")
    )
  })
  structural_canvas_register_fit_diagnostic_outputs(
    output, prefix, analysis_type, fit_result, manuscript_result_table, dataset_fn, app_language_fn
  )
  structural_canvas_register_validity_outputs(
    output, prefix, analysis_type, fit_result, manuscript_result_table, app_language_fn,
    variable_table_fn, labels_fn
  )
  if (analysis_type != "plssem") structural_canvas_register_local_fit_outputs(
    output, prefix, fit_result, app_language_fn, variable_table_fn, labels_fn
  )
  if (analysis_type %in% c("cbsem", "sem")) structural_canvas_register_moderation_outputs(
    output, prefix, fit_result, app_language_fn, variable_table_fn, labels_fn
  )

  for (kind in c("overview")) local({
    result_kind <- kind
    output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(manuscript_result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
  })
  output[[paste0(prefix, "_result_structural")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- structural_canvas_path_display_table(manuscript_result_table("structural"), "Path")
    tagList(
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-path-table"),
      tags$p(class = "structural-result-note", HTML("<em>Note.</em> B = unstandardized coefficient; SE = standard error; beta = standardized coefficient; R<sup>2</sup> = explained variance in the outcome. BH-adjusted <em>p</em> values control the false-discovery rate across the reported structural paths. All tests are two-sided."))
    )
  })
  output[[paste0(prefix, "_result_structural_effects")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- result_table("structural_effects")
    if (!nrow(table)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(
      class = "structural-effect-summary-block",
      tags$h5(if (ko) "표 3 보조: 직접효과, 간접효과, 총효과" else "Supplementary Table 3: Direct, indirect, and total effects"),
      structural_canvas_effect_summary_html_table(table, ci = FALSE, language = statedu_current_language(app_language_fn)),
      tags$p(class = "structural-result-note", if (ko) "매개경로가 정의된 경우 간접효과와 총효과를 본표의 직접 구조경로와 구분하여 보고합니다." else "Indirect and total effects are reported separately when mediation paths are defined.")
    )
  })
  output[[paste0(prefix, "_result_specific_indirect")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- result_table("structural_specific_indirect")
    if (!nrow(table)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(
      class = "result-section regression-result-panel structural-specific-indirect-result",
      tags$h5(if (ko) "표 6. 경로별 특정 간접효과" else "Table 6. Specific indirect effects"),
      structural_canvas_specific_indirect_html_table(table),
      tags$p(class = "structural-result-note", if (ko) "각 행은 하나의 매개경로입니다. Boot SE와 Boot 95% CI는 요청한 경로·간접·총효과 재표집을 사용하며, 유효 반복이 부족하면 구간을 비워 둡니다." else "Each row represents one mediation path. Boot SE and Boot 95% CI use path/indirect/total-effect resampling when requested; intervals are blank when valid replicates are insufficient.")
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
      structural_canvas_effect_summary_html_table(table, ci = TRUE, language = statedu_current_language(app_language_fn)),
      structural_canvas_effect_ci_source_note(table, statedu_current_language(app_language_fn)),
      tags$p(class = "structural-result-note", if (ko) "Bootstrap을 요청한 효과는 유효 반복이 부족해도 모형기반 정규이론 CI로 자동 대체하지 않습니다. 빈 구간은 위 주석의 산출 근거와 함께 해석하십시오." else "When bootstrap inference was requested, an interval with insufficient valid replicates is not silently replaced by a model-based normal-theory CI. Interpret blank intervals with the source note above.")
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
      tagList(
        structural_canvas_pls_measurement_main_html_table(manuscript_result_table("measurement")),
        tags$p(class = "structural-result-note", HTML("<em>Note.</em> † Common factor; ‡ composite; ¶ unspecified construct type. loading/weight reports the outer loading for reflective indicators and the outer weight for formative indicators. Boot = bootstrap; SE = standard error; CI = confidence interval; BH = Benjamini–Hochberg; adj <em>p</em> = multiplicity-adjusted <em>p</em> value; VIF = variance inflation factor. VIF is especially relevant when evaluating collinearity among formative indicators. Data-driven indicator deletion must not be presented as a prespecified confirmatory procedure; report any deletion and its rationale transparently."))
      )
    } else {
      structural_canvas_measurement_html_table(manuscript_result_table("measurement"))
    }
  })
  output[[paste0(prefix, "_result_measurement_diagnostics")]] <- renderUI({
    if (identical(analysis_type, "plssem")) {
      diagnostics <- result_table("measurement_guide")
      if (!is.data.frame(diagnostics) || !nrow(diagnostics)) return(NULL)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      bundle <- fit_result()
      formative_evidence <- structural_canvas_formative_content_validity_rows(
        bundle$snapshot %||% list(), bundle$redundancy_result %||% NULL, bundle$redundancy_construct %||% NULL
      )
      return(tagList(
        tags$h5(if (ko) paste0("표 ", table_number("measurement"), " 보조: PLS 측정 진단") else paste0("Supplementary Table ", table_number("measurement"), ": PLS measurement diagnostics")),
        structural_canvas_basic_html_table(diagnostics, class = "table table-striped table-bordered structural-pls-measurement-guide-table"),
        tags$p(class = "structural-result-note", if (ko) "반영지표는 outer loading과 교차적재를, 형성지표는 outer weight와 item VIF를 우선 검토합니다." else "For reflective indicators, review outer loadings and cross-loadings; for formative indicators, prioritize outer weights and item VIF."),
        if (nrow(formative_evidence)) tags$h5(if (ko) "형성형 합성변수의 내용타당도 근거" else "Formative-composite content-validity evidence"),
        if (nrow(formative_evidence)) structural_canvas_basic_html_table(formative_evidence, class = "table table-striped table-bordered structural-formative-evidence-table")
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
  output[[paste0(prefix, "_result_effect_bootstrap")]] <- renderUI({
    bundle <- fit_result()
    result <- bundle$effect_bootstrap_result %||% NULL
    requested <- as.integer(bundle$effect_bootstrap %||% 0L)
    if (requested <= 0L) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    if (isTRUE(bundle$effect_bootstrap_pending)) {
      return(tags$p(class = "structural-result-note", if (ko) "경로·간접·총효과 bootstrap CI/p를 계산하고 있습니다. 완료되면 이 표가 자동으로 갱신됩니다." else "Path, indirect, and total-effect bootstrap CIs and p values are still running. This section will update automatically when they finish."))
    }
    if (isTRUE(bundle$effect_bootstrap_canceled)) {
      return(tags$p(class = "structural-result-note", if (ko) "경로·간접·총효과 부트스트랩이 사용자 요청으로 중단되었습니다. 기본 분석 결과와 점추정값은 유지됩니다." else "The path, indirect, and total-effect bootstrap was stopped by the user. Base-model results and point estimates remain available."))
    }
    if (nzchar(as.character(bundle$effect_bootstrap_error %||% ""))) {
      return(tags$p(class = "structural-result-note", if (ko) paste0("경로·간접·총효과 bootstrap CI/p 계산 실패: ", bundle$effect_bootstrap_error) else paste0("Path, indirect, and total-effect bootstrap failed: ", bundle$effect_bootstrap_error)))
    }
    if (is.null(result) || !nrow(result)) return(tags$p(class = "structural-result-note", if (ko) "경로·간접·총효과 bootstrap에서 사용 가능한 반복 적합을 얻지 못했습니다." else "No usable replicate fits were obtained for the path, indirect, and total-effect bootstrap."))
    interval_label <- if (identical(as.character(bundle$effect_bootstrap_ci_method %||% "bias_corrected"), "percentile")) "percentile" else "bias-corrected (BC)"
    quantile_type <- structural_canvas_bootstrap_quantile_type(bundle$effect_bootstrap_ci_method %||% "bias_corrected", "structural_effects")
    if (!"beta_status" %in% names(result)) result$beta_status <- ifelse(result$op == "modmed", "Not reported: product-indicator index is scale-dependent", "Not available")
    if (!"quantile_type" %in% names(result)) result$quantile_type <- quantile_type
    diagnostics <- unique(result[, c("ci_method", "quantile_type", "valid", "requested", "valid_percent", "status"), drop = FALSE])
    names(diagnostics) <- c("CI method", "Quantile type", "Valid replicates", "Requested replicates", "Valid %", "Status")
    diagnostics[["Quantile type"]] <- paste0("R type ", diagnostics[["Quantile type"]])
    moderated <- result[result$op == "modmed", c("lhs", "rhs", "estimate", "lower", "upper", "p", "beta_status", "valid", "requested", "valid_percent", "status"), drop = FALSE]
    if (nrow(moderated)) names(moderated) <- c("Indirect path", "Moderator", "Index", "95% CI lower", "95% CI upper", "Bootstrap p", "Standardized index", "Valid replicates", "Requested replicates", "Valid %", "Status")
    div(
      class = "result-section structural-effect-bootstrap-result",
      tags$h5(if (ko) "경로·간접·총효과 bootstrap 진단" else "Path, indirect, and total-effect bootstrap diagnostics"),
      structural_canvas_basic_html_table(diagnostics, class = "table table-striped table-bordered"),
      if (nrow(moderated)) tagList(
        tags$h5(if (ko) "조절된 매개효과 index" else "Index of moderated mediation"),
        structural_canvas_basic_html_table(moderated, class = "table table-striped table-bordered")
      ),
      tags$p(class = "structural-result-note", if (ko) paste0("사례 재표집 ", interval_label, " 95% CI(R quantile type ", quantile_type, "); seed = ", bundle$effect_bootstrap_seed, ". 직접효과, 특정 간접효과, 간접효과와 총효과의 B와 beta 구간은 각 유효 반복에서 다시 계산됩니다. product-indicator 잠재조절모형의 조절된 매개효과 index는 척도 의존적이며 유일한 표준화 정의가 없으므로 비표준화 index와 bootstrap CI를 주 결과로 보고하고 표준화 index는 보고하지 않습니다. 부적합·미수렴 반복은 제외하며 유효율이 80% 미만이면 주의가 필요합니다.") else paste0("Case-resampling ", interval_label, " 95% CIs (R quantile type ", quantile_type, "); seed = ", bundle$effect_bootstrap_seed, ". B and beta intervals for direct effects, specific indirect effects, indirect effects, and total effects are recomputed in every valid replicate. The moderated-mediation index in a product-indicator latent-moderation model is scale-dependent and has no unique standardization, so the unstandardized index with its bootstrap CI is the primary result and a standardized index is not reported. Inadmissible or nonconverged replicates are excluded; valid rates below 80% require caution."))
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
      tags$p(tags$b(if (ko) "기록된 목적: " else "Recorded purpose: "), if (nzchar(result$purpose %||% "")) result$purpose else if (ko) "기록 없음" else "Not recorded"),
      tags$p(class = "structural-result-note", paste0(if (ko) "상태: " else "Status: ", result$status, ". ", result$warning)),
      if (isTRUE(result$applied)) tags$p(class = "structural-result-note", if (ko) paste0("결과 모형은 ", result$construct, "를 상위요인으로 두고 ", paste(result$item_level_constructs %||% character(0), collapse = ", "), " 하위 item-level 요인을 적합했습니다.") else paste0("The result model fitted ", result$construct, " as a higher-order factor with lower-order item-level factors: ", paste(result$item_level_constructs %||% character(0), collapse = ", "), ".")),
      if (identical(result$applied, FALSE) && nzchar(result$fit_error %||% "")) tags$p(class = "structural-result-note structural-result-warning", if (ko) paste0("item-level 하위요인 모형 적합 실패: ", result$fit_error) else paste0("Item-level lower-order model fit failed: ", result$fit_error)),
      tags$p(class = "structural-result-note", paste0(if (ko) "문항수준 최소 |적재량| = " else "Item-level minimum |loading| = ", format_decimal3(result$min_loading), if (ko) "; 최대 절대 잔차상관 = " else "; maximum absolute residual correlation = ", format_decimal3(result$max_residual_correlation), ".")),
      tags$h6(if (ko) "배정 미리보기" else "Allocation preview"),
      structural_canvas_basic_html_table(allocation, class = "table table-striped table-bordered structural-parcel-allocation-table"),
      tags$h6(if (ko) "Parcel 균형 요약" else "Parcel balance summary"),
      structural_canvas_basic_html_table(summary, class = "table table-striped table-bordered structural-parcel-summary-table"),
      tags$p(class = "structural-result-note", if (ko) "데이터셋에 parcel 변수는 생성되지 않았습니다. 하위요인은 원문항을 그대로 지표로 사용하는 item-level 표현입니다. 해석 전 이론적 동질성, 국소의존, 다른 배정 방식에 대한 민감도를 함께 검토해야 합니다." else "No parcel variables were created. The lower-order factors remain item-level representations using the original indicators. Review substantive item homogeneity, local dependence, and sensitivity to alternative allocations before interpretation.")
    )
  })
  output[[paste0(prefix, "_result_measurement_ci")]] <- renderUI({
    if (identical(analysis_type, "plssem")) {
      ci_table <- result_table("measurement_bootstrap")
      if (!is.data.frame(ci_table) || !nrow(ci_table)) return(NULL)
      value_columns <- setdiff(names(ci_table), c("Construct", "Construct type", "Indicator", "Mode"))
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      bundle <- fit_result()
      bootstrap <- bundle$pls_bootstrap_result %||% list()
      inference_available <- isTRUE(bootstrap$inference_available)
      valid_n <- suppressWarnings(as.integer(bootstrap$nboot %||% 0L))
      requested_n <- suppressWarnings(as.integer(bootstrap$requested_nboot %||% bundle$pls_bootstrap %||% 0L))
      minimum_ratio <- suppressWarnings(as.numeric(bootstrap$minimum_valid_ratio %||% .80))
      bootstrap_status <- as.character(bootstrap$bootstrap_status %||% "Not recorded")[[1L]]
      failure_message <- as.character(bootstrap$failure_message %||% "")
      failure_message <- if (length(failure_message)) trimws(failure_message[[1L]]) else ""
      if (!is.finite(requested_n) || requested_n <= 0L) return(NULL)
      has_values <- length(value_columns) && any(nzchar(as.character(unlist(ci_table[value_columns], use.names = FALSE))))
      if (!has_values && inference_available) return(NULL)
      return(tagList(
        tags$h5(if (ko) paste0("표 ", table_number("measurement"), " 보조: PLS 측정모형 부트스트랩") else paste0("Supplementary Table ", table_number("measurement"), ": PLS measurement bootstrap")),
        if (has_values) structural_canvas_basic_html_table(ci_table, class = "table table-striped table-bordered structural-pls-measurement-bootstrap-table"),
        if (!inference_available) tags$p(class = "structural-result-note structural-result-warning", if (ko) paste0("PLS bootstrap 상태: ", bootstrap_status, ". 유효 재표집은 ", valid_n, "/", requested_n, "회로 최소 ", formatC(100 * minimum_ratio, format = "fg", digits = 3), "% 기준에 미달하여 outer loading·weight의 bootstrap SE, CI, t, p를 보고하지 않습니다.", if (nzchar(failure_message)) paste0(" 상세: ", failure_message) else "") else paste0("PLS bootstrap status: ", bootstrap_status, ". Only ", valid_n, "/", requested_n, " resamples were valid, below the ", formatC(100 * minimum_ratio, format = "fg", digits = 3), "% minimum; outer-loading and weight bootstrap SE, CI, t, and p values are not reported.", if (nzchar(failure_message)) paste0(" Detail: ", failure_message) else "")),
        if (inference_available) tags$p(class = "structural-result-note", if (ko) "outer loading과 outer weight의 percentile bootstrap CI, t, p 값과 각 검정군별 BH 보정 p값입니다. 전체 통계량 계약을 통과한 반복만 사용합니다." else "Percentile bootstrap CI, t, raw p values, and family-specific BH-adjusted p values for outer loadings and outer weights use only whole-draw contract-valid resamples.")
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
