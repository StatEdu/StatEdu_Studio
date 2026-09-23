# Longitudinal / panel model result UI and export helpers.

longitudinal_format_number <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0 || is.na(value)) return("")
  if (!is.finite(value)) return(as.character(value))
  format_decimal3(value)
}

longitudinal_mi_failure_text <- function(text, language) {
  parts <- strsplit(text, "; (?=imputation [0-9]+(?: weights)?: )", perl = TRUE)[[1L]]
  translated <- character(0)
  for (part in parts) {
    matched <- regmatches(part, regexec("^imputation ([0-9]+)( weights)?: (.+)$", part, perl = TRUE))[[1L]]
    if (!length(matched)) return(text)
    detail <- longitudinal_input_error_text(simpleError(matched[[4L]]), language)
    # Do not reinterpret punctuation or numbered fragments in external errors.
    if (identical(detail, matched[[4L]])) return(text)
    key <- if (nzchar(matched[[3L]])) "weights" else "fit"
    translated <- c(translated, sprintf(statedu_t(paste0("longitudinal.mi_failure.", key), language), matched[[2L]], detail))
  }
  paste(translated, collapse = "; ")
}

longitudinal_appendix_text <- function(text, language = NULL) {
  language <- result_appendix_table_language(language)
  text <- as.character(text %||% "")
  if (identical(language, "en") || is.na(text) || !nzchar(text)) return(text)
  warning_prefix <- "Mixed model fit warning: "
  if (startsWith(text, warning_prefix) && endsWith(text, ".")) {
    detail <- substring(text, nchar(warning_prefix) + 1L, nchar(text) - 1L)
    # Translate the standalone application marker only. Compound optimizer
    # messages are opaque; their words may themselves resemble this marker.
    singular <- "singular random-effects fit"
    if (identical(detail, singular)) detail <- statedu_t("longitudinal.mixed_message.singular", language)
    return(sprintf(statedu_t("longitudinal.mixed_message.warning", language), detail))
  }
  dk_comparison <- regmatches(text, regexec("^(Panel fixed effects|Panel random effects): Driscoll-Kraay SE$", text))[[1L]]
  if (length(dk_comparison)) return(paste0(longitudinal_appendix_text(dk_comparison[[2L]], language), ": Driscoll-Kraay SE"))
  correlation_formats <- c(
    "Working correlation structure: %s." = "작업상관 구조: %s.",
    "The selected working correlation is %s." = "선택한 작업상관은 %s입니다.",
    "Report the selected working correlation (%s) and robust sandwich inference." = "선택한 작업상관(%s)과 강건 샌드위치 추론을 보고하십시오.",
    "Compare the selected GEE working correlation (%s) with independence, exchangeable, and AR(1) when feasible." = "가능하면 선택한 GEE 작업상관(%s)을 독립, 교환가능 및 AR(1) 구조와 비교하십시오.",
    "GEE (%s)" = "GEE(%s)"
  )
  for (identifier in c("exchangeable", "ar1", "independence", "unstructured", "unstructured_adjusted")) {
    for (format in names(correlation_formats)) {
      if (identical(text, sprintf(format, identifier))) return(sprintf(
        statedu_localized_text(language, format, unname(correlation_formats[[format]])),
        statedu_t(paste0("longitudinal.identifier.", identifier), language)))
    }
    rationale_prefix <- "Use GEE when the target is a population-averaged longitudinal effect."
    rationale_tail <- sprintf("Report the selected working correlation (%s) and robust sandwich inference.", identifier)
    if (identical(text, paste(rationale_prefix, rationale_tail))) return(paste(
      statedu_localized_text(language, rationale_prefix, "모집단 평균 종단효과가 목표이면 GEE를 사용합니다."),
      longitudinal_appendix_text(rationale_tail, language)))
  }
  correlation <- regmatches(text, regexec("^Working correlation: (exchangeable|ar1|independence|unstructured|unstructured_adjusted)$", text))[[1L]]
  if (length(correlation)) return(sprintf(statedu_localized_text(language,
    "Working correlation: %s", "작업상관: %s"),
    statedu_t(paste0("longitudinal.identifier.", correlation[[2L]]), language)))
  if (startsWith(text, "Weight variable:")) return(text)
  number_pattern <- "([+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)(?:[eE][+-]?[0-9]+)?)"
  weight_stats <- regmatches(text, regexec(paste0("^min=", number_pattern,
    "; median=", number_pattern, "; max=", number_pattern, "$"), text, perl = TRUE))[[1L]]
  if (length(weight_stats)) return(sprintf(statedu_t("longitudinal.weight_stats", language),
    weight_stats[[2L]], weight_stats[[3L]], weight_stats[[4L]]))
  # Parse the complete application sentence before splitting lines: user
  # variable names can themselves contain line breaks and punctuation.
  observation <- regmatches(text, regexec(paste0("^Observation model: ([\\s\\S]+); weights clipped to \\[",
    number_pattern, ", ", number_pattern,
    "\\] and normalized to mean 1\\. Report these variables and review positivity/weight stability\\.$"), text, perl = TRUE))[[1L]]
  if (length(observation)) return(sprintf(statedu_localized_text(language,
    "Observation model: %s; weights clipped to [%s, %s] and normalized to mean 1. Report these variables and review positivity/weight stability.",
    "관측모형: %s; 가중치를 [%s, %s] 범위로 절단하고 평균 1로 정규화했습니다. 이 변수들을 보고하고 양성성/가중치 안정성을 검토하십시오."),
    observation[[2L]], observation[[3L]], observation[[4L]]))
  # Translate the known leading sentence without splitting variable names in
  # the observation-model description that follows it.
  for (kind in c("sampling", "longitudinal", "ipw", "combined")) {
    key <- paste0("longitudinal.weight_note.", kind)
    prefix <- statedu_t(key, "en")
    if (identical(text, prefix)) return(statedu_t(key, language))
    if (startsWith(text, paste0(prefix, " "))) return(paste(statedu_t(key, language),
      longitudinal_appendix_text(substring(text, nchar(prefix) + 2L), language)))
  }
  gee_compare <- regmatches(text, regexec("^For GEE, compare working correlation structures when clinically plausible; current structure is ([\\s\\S]+)\\.$", text, perl = TRUE))[[1L]]
  if (length(gee_compare)) return(sprintf(statedu_t("longitudinal.checklist.gee_compare", language), gee_compare[[2L]]))
  glmm_family <- regmatches(text, regexec("^Use GLMM when the target is subject-specific inference for a non-Gaussian outcome using the ([A-Za-z0-9_]+) family\\.$", text, perl = TRUE))[[1L]]
  if (length(glmm_family)) {
    family <- glmm_family[[2L]]
    display_family <- if (family %in% c("gaussian", "binomial", "poisson", "negative_binomial", "gamma")) {
      longitudinal_appendix_text(family, language)
    } else family
    return(sprintf(statedu_t("longitudinal.glmm_checklist.rationale", language), display_family))
  }
  condition_keys <- c(paste0("longitudinal.sensitivity_metric.", c("robust", "hc1", "ratio", "p")),
    paste0("longitudinal.sensitivity_table.", c("gee", "random", "panel", "covariance", "selected", "fitted", "computed", "intercept", "slope", "hausman", "re_note", "dk_note")),
    paste0("longitudinal.identifier.", c("auto", "count", "poisson", "negative_binomial",
    "exchangeable", "ar1", "independence", "unstructured", "unstructured_adjusted")),
    paste0("longitudinal.family_name.", c("gaussian", "binomial", "gamma")),
    paste0("longitudinal.fit_details.", c("singular", "random_variance")),
    paste0("longitudinal.count_failure.", c("poisson", "dispersion", "nb")),
    paste0("longitudinal.auto_count.", c("poisson_reason", "nb_label", "nb_reason", "decision", "zero_ratio", "zero_screen", "zero_clear", "poisson_aic", "poisson_bic", "nb_aic", "nb_bic")),
    paste0("longitudinal.glmm_checklist.", c("cluster", "no_issue")),
    paste0("longitudinal.panel_recommendation.", c("confounding", "dependence", "unit_effects", "singular")),
    paste0("longitudinal.checklist.", c("no_weights", "review", "structure", "structure_note", "missing_note", "weights", "engine", "alternatives", "coefficient_note", "sensitivity", "comparison", "footnotes", "manuscript")),
    paste0("longitudinal.weight_value.", c("normalized", "review", "p01_99", "p05_95")),
    paste0("longitudinal.weight_item.", c("trim", "normalize", "base", "variables", "generated", "effective", "diagnostic")),
    paste0("longitudinal.mi_condition.", c("rows", "unneeded", "wgee")),
    paste0("longitudinal.pool_error.", c("tables", "terms")),
    paste0("longitudinal.sensitivity_error.", c("package", "coefficients")),
    "analysis.ui.observation_status_had_no_variation_unit_weights_were_used",
    "analysis.ui.no_fully_observed_predictors_were_available_for_the_observation_model_intercept_only_ipw_was_used_treat_this_as_a_weak_ipw_sensitivity_analysis")
  condition_index <- match(text, vapply(condition_keys, function(key) statedu_t(key, "en"), character(1)))
  if (!is.na(condition_index)) return(statedu_t(condition_keys[[condition_index]], language))
  pooling_term <- regmatches(text, regexec("^No finite estimates were available for ([\\s\\S]+)\\.$", text, perl = TRUE))[[1L]]
  if (length(pooling_term)) return(sprintf(statedu_t("longitudinal.pool_error.finite", language), pooling_term[[2L]]))
  if (startsWith(text, "imputation ")) return(longitudinal_mi_failure_text(text, language))
  if (text %in% c("Warning", "Skipped"))
    return(statedu_t(paste0("longitudinal.warning.", tolower(text)), language))
  if (startsWith(text, "R package warning: "))
    return(paste0(statedu_t("longitudinal.warning.package_prefix", language),
      substring(text, nchar("R package warning: ") + 1L)))
  error_text <- longitudinal_input_error_text(simpleError(text), language)
  if (!identical(error_text, text)) return(error_text)
  # Assumption summaries contain application check labels, not user identifiers.
  flagged <- regmatches(text, regexec("^Assumption screening flagged (.+); recommended alternative analyses or reporting cautions were generated accordingly\\.$", text, perl = TRUE))[[1L]]
  if (length(flagged)) {
    checks <- strsplit(flagged[[2L]], ", ", fixed = TRUE)[[1L]]
    checks <- paste(vapply(checks, longitudinal_appendix_text, character(1), language = language), collapse = ", ")
    return(sprintf(statedu_localized_text(language,
      "Assumption screening flagged %s; recommended alternative analyses or reporting cautions were generated accordingly.",
      "가정 선별에서 %s 항목이 표시되어 대안 분석 또는 보고상 주의사항을 제시했습니다."), checks))
  }
  reml <- regmatches(text, regexec("^Repeated-measures marginal linear model with (.+) residual covariance, REML estimation and Satterthwaite coefficient inference\\.$", text, perl = TRUE))[[1L]]
  if (length(reml)) return(sprintf(statedu_localized_text(language,
    "Repeated-measures marginal linear model with %s residual covariance, REML estimation and Satterthwaite coefficient inference.",
    "%s 잔차 공분산, REML 추정 및 Satterthwaite 계수 추론을 사용하는 반복측정 주변 선형모형입니다."), reml[[2L]]))
  # MI appends optional weight and failure fields after a known complete note.
  # Parse those fields as a whole, preserving punctuation in failure details.
  mi_parts <- regmatches(text, regexec(
    "^(Standard mice-based MI sensitivity;.+ This is not a dedicated multilevel MI engine\\.)(?: Weights: (.+?)\\.)?(?: Failed fits: (.+)\\.)?$",
    text, perl = TRUE))[[1L]]
  if (length(mi_parts) && any(nzchar(mi_parts[3:4]))) {
    pieces <- longitudinal_appendix_text(mi_parts[[2L]], language)
    if (nzchar(mi_parts[[3L]])) pieces <- c(pieces, sprintf(statedu_localized_text(language,
      "Weights: %s.", "가중치: %s."), longitudinal_appendix_text(mi_parts[[3L]], language)))
    if (nzchar(mi_parts[[4L]])) pieces <- c(pieces, sprintf(statedu_localized_text(language,
      "Failed fits: %s.", "적합 실패: %s."), longitudinal_mi_failure_text(mi_parts[[4L]], language)))
    return(paste(pieces, collapse = " "))
  }
  if (!identical(language, "ko")) {
    formats <- c(
      "^Compare the selected GEE working correlation \\((.+)\\) with independence, exchangeable, and AR\\(1\\) when feasible\\.$" = "Compare the selected GEE working correlation (%s) with independence, exchangeable, and AR(1) when feasible.",
      "^([0-9]+) model row\\(s\\) were excluded because selected variables were missing in that row; subjects with other observed visits remained in the likelihood-based analysis under the MAR assumption\\.$" = "%s model row(s) were excluded because selected variables were missing in that row; subjects with other observed visits remained in the likelihood-based analysis under the MAR assumption.",
      "^The analysis used (.+), including ([^ ]+) observations from ([^ ]+) subjects/clusters across ([^ ]+) observed time points\\.$" = "The analysis used %s, including %s observations from %s subjects/clusters across %s observed time points.",
      "^Analysis weights were applied as (.+) with effective sample size (.+)\\.$" = "Analysis weights were applied as %s with effective sample size %s.",
      "^Fixed-effect estimates were reported for (.+) with 95% confidence intervals\\.$" = "Fixed-effect estimates were reported for %s with 95%% confidence intervals.",
      "^Automated sensitivity screening generated ([0-9]+) comparison row\\(s\\), including fitted alternatives and failed alternatives where applicable\\.$" = "Automated sensitivity screening generated %s comparison row(s), including fitted alternatives and failed alternatives where applicable.",
      "^([0-9]+) missing-data sensitivity result row\\(s\\) generated\\.$" = "%s missing-data sensitivity result row(s) generated.",
      "^Observation model: (.+); weights clipped to \\[(.+), (.+)\\] and normalized to mean 1\\. Report these variables and review positivity/weight stability\\.$" = "Observation model: %s; weights clipped to [%s, %s] and normalized to mean 1. Report these variables and review positivity/weight stability.",
      "^Observation model failed \\((.+)\\); intercept-only IPW was used\\. Treat this as a weak IPW sensitivity analysis\\.$" = "Observation model failed (%s); intercept-only IPW was used. Treat this as a weak IPW sensitivity analysis.",
      "^Standard mice-based MI sensitivity; pooled across ([0-9]+) imputed dataset\\(s\\) using Rubin-style total variance\\. (.+) This is not a dedicated multilevel MI engine\\.$" = "Standard mice-based MI sensitivity; pooled across %s imputed dataset(s) using Rubin-style total variance. %s This is not a dedicated multilevel MI engine.",
      "^Missing-data sensitivity engines were run for (.+) \\(([0-9]+) fitted row\\(s\\), ([0-9]+) failed row\\(s\\)\\); MI/IPW/WGEE outputs should be reported as sensitivity analyses unless the missing-data model is prespecified as primary\\.$" = "Missing-data sensitivity engines were run for %s (%s fitted row(s), %s failed row(s)); MI/IPW/WGEE outputs should be reported as sensitivity analyses unless the missing-data model is prespecified as primary.",
      "^Missing-data sensitivity engines were run: (.+)\\. Interpret these as sensitivity analyses and report the imputation or weighting model assumptions\\.$" = "Missing-data sensitivity engines were run: %s. Interpret these as sensitivity analyses and report the imputation or weighting model assumptions.",
      "^Working correlation: (.+)$" = "Working correlation: %s",
      "^Working correlation structure: (.+)\\.$" = "Working correlation structure: %s.",
      "^The selected working correlation is (.+)\\.$" = "The selected working correlation is %s.",
      "^Report the selected working correlation \\((.+)\\) and robust sandwich inference\\.$" = "Report the selected working correlation (%s) and robust sandwich inference.",
      "^Random intercept grouping variable: (.+)\\.$" = "Random intercept grouping variable: %s.",
      "^A random slope for the selected time variable \\((.+)\\) was included\\.$" = "A random slope for the selected time variable (%s) was included.",
      "^([0-9]+) observations were excluded because of missing values in selected analysis variables\\.$" = "%s observations were excluded because of missing values in selected analysis variables.",
      "^Analyses were performed using (.+)\\.$" = "Analyses were performed using %s.",
      "^([0-9]+) assumption check item\\(s\\) reported\\.$" = "%s assumption check item(s) reported.",
      "^([0-9]+) automated sensitivity comparison row\\(s\\) generated\\.$" = "%s automated sensitivity comparison row(s) generated."
    )
    translate_line <- function(value) {
      for (pattern in names(formats)) {
        match <- regmatches(value, regexec(pattern, value, perl = TRUE))[[1L]]
        if (length(match)) {
          args <- as.list(match[-1L])
          if (startsWith(value, "The analysis used ") || startsWith(value, "Analysis weights were applied as "))
            args[[1L]] <- longitudinal_appendix_text(args[[1L]], language)
          if (startsWith(value, "Standard mice-based MI sensitivity;"))
            args[[2L]] <- longitudinal_appendix_text(args[[2L]], language)
          if (startsWith(value, "Observation model failed ("))
            args[[1L]] <- longitudinal_input_error_text(simpleError(args[[1L]]), language)
          if (startsWith(value, "Missing-data sensitivity engines were run")) {
            strategies <- strsplit(args[[1L]], ", ", fixed = TRUE)[[1L]]
            args[[1L]] <- paste(vapply(strategies, function(x) statedu_localized_text(language, x), character(1)), collapse = ", ")
          }
          return(do.call(sprintf, c(list(statedu_localized_text(language, unname(formats[[pattern]]))), args)))
        }
      }
      # This builder joins two known messages; do not split arbitrary prose at
      # periods because user identifiers and software versions can contain them.
      prefix <- "Use GEE when the target is a population-averaged longitudinal effect."
      if (startsWith(value, paste0(prefix, " Report the selected working correlation ("))) {
        return(paste(statedu_localized_text(language, prefix),
          longitudinal_appendix_text(substring(value, nchar(prefix) + 2L), language)))
      }
      result_appendix_ui_text(value, language)
    }
    return(paste(vapply(strsplit(text, "\n", fixed = TRUE)[[1L]], translate_line, character(1)), collapse = "\n"))
  }

  exact <- c(
    "R package warnings" = "R 패키지 경고",
    "Experimental SPSS compatibility mode: custom GEE estimator, not the geepack estimator. This mode is not the default analysis." =
      "실험적 SPSS 호환 모드: geepack 추정기가 아닌 자체 GEE 추정기를 사용합니다. 기본 분석 모드가 아닙니다.",
    "Coefficients (detailed)" = "상세 계수",
    "Model rationale" = "모형 선택 근거",
    "Data structure" = "자료 구조",
    "Analysis weights" = "분석 가중치",
    "Missing data" = "결측 자료",
    "Missing-data pattern" = "결측자료 패턴",
    "Missing data by time" = "시점별 결측자료",
    "Missing-data sensitivity results" = "결측자료 민감도 분석 결과",
    "Model fit details" = "모형 적합 세부정보",
    "Assumption checks" = "가정 검토",
    "Recommended analysis" = "권고 분석",
    "Sensitivity analysis suggestions" = "민감도 분석 제안",
    "Sensitivity analysis results" = "민감도 분석 결과",
    "Suggested manuscript text" = "원고 문장 제안",
    "SCI reporting checklist" = "SCI 보고 점검표",
    "Software versions" = "소프트웨어 버전",
    "Model overview" = "모형 개요",
    "Model interpretation guide" = "모형 해석 안내",
    "Warnings / skipped models" = "경고 / 제외된 모형",
    "Notes" = "참고사항",
    "N" = "N", "Clusters" = "군집 수", "Time points" = "시점 수",
    "Outcome" = "결과변수", "ID" = "ID", "Time" = "시점",
    "Exposure / offset" = "노출량 / 오프셋", "Weight" = "가중치",
    "Method" = "방법", "Requested family" = "요청 분포",
    "Fitted family" = "적합 분포", "Formula" = "모형식",
    "Estimand" = "추정대상", "Correlation / random effect" = "상관 / 확률효과",
    "Package" = "패키지", "Population-averaged" = "모집단 평균",
    "Subject-specific" = "대상자 특이적", "Panel unit effect" = "패널 단위 효과",
    "Raw observations" = "원자료 관측치",
    "Analyzed observations" = "분석 관측치",
    "Missing-data handling" = "결측자료 처리",
    "Excluded for missing analysis variables" = "분석변수 결측으로 제외",
    "Subjects / clusters excluded for missingness" = "결측으로 제외된 대상자 / 군집",
    "Subjects" = "대상자 수", "Higher-level clusters" = "상위수준 군집 수",
    "Time points observed" = "관측 시점 수", "Balanced panel" = "균형 패널",
    "Observations per cluster: min" = "군집당 관측치: 최솟값",
    "Observations per cluster: median" = "군집당 관측치: 중앙값",
    "Observations per cluster: max" = "군집당 관측치: 최댓값",
    "Raw rows" = "원자료 행",
    "Complete model rows" = "모형 완전관측 행",
    "Rows retained by selected missing-data method" = "선택한 결측자료 방법으로 유지된 행",
    "Rows excluded by selected missing-data method" = "선택한 결측자료 방법으로 제외된 행",
    "Subjects / clusters in raw data" = "원자료의 대상자 / 군집",
    "Subjects / clusters retained" = "유지된 대상자 / 군집",
    "Subjects / clusters with any incomplete selected row" = "선택 행 중 결측이 있는 대상자 / 군집",
    "Rows with missing dependent variable" = "종속변수가 결측인 행",
    "Rows with any missing model term" = "모형 항에 결측이 있는 행",
    "Rows with missing ID or time" = "ID 또는 시점이 결측인 행",
    "Rows with missing exposure / offset" = "노출량 / 오프셋이 결측인 행",
    "Rows with missing higher-level cluster ID" = "상위수준 군집 ID가 결측인 행",
    "Distinct missingness patterns" = "서로 다른 결측 패턴 수",
    "Most common missingness pattern" = "가장 흔한 결측 패턴",
    "Complete" = "완전관측",
    "Weight variable" = "가중치 변수", "Weight type" = "가중치 유형",
    "Trimming" = "절단", "Normalization" = "정규화",
    "Base weight summary" = "기본 가중치 요약", "IPW summary" = "IPW 요약",
    "Final weight summary" = "최종 가중치 요약", "Effective sample size" = "유효표본크기",
    "IPW observation model variables" = "IPW 관측모형 변수",
    "Predicted observation probability: min" = "예측 관측확률: 최솟값",
    "Predicted observation probability: median" = "예측 관측확률: 중앙값",
    "Predicted observation probability: max" = "예측 관측확률: 최댓값",
    "Probability clipping count" = "확률 절단 건수",
    "Generated IPW summary" = "생성된 IPW 요약",
    "Generated IPW effective sample size" = "생성된 IPW 유효표본크기",
    "Weight clipping count" = "가중치 절단 건수", "IPW diagnostic note" = "IPW 진단 참고",
    "Mean normalized to 1" = "평균 1로 정규화", "Not applied" = "적용하지 않음",
    "No weights" = "가중치 없음",
    "Sampling / baseline longitudinal weight" = "표본 / 기저시점 종단 가중치",
    "Time-varying longitudinal weight" = "시간가변 종단 가중치",
    "Generated IPW for dropout" = "탈락 보정을 위한 생성 IPW",
    "Analysis weight x generated IPW" = "분석 가중치 × 생성 IPW",
    "None" = "없음", "Intercept only" = "절편만 사용",
    "Complete-case: row-wise" = "완전사례: 행 단위",
    "Likelihood-based MAR: available repeated measures" = "우도 기반 MAR: 이용 가능한 반복측정값",
    "Complete-subject analysis" = "완전 대상자 분석",
    "complete-case analysis using row-wise complete observations" = "행 단위 완전사례 분석",
    "likelihood-based mixed-model analysis using available repeated measures under a MAR assumption" =
      "MAR 가정하에 이용 가능한 반복측정값을 사용하는 우도 기반 혼합모형 분석",
    "complete-subject analysis" = "완전 대상자 분석",
    "Multiple imputation (MI)" = "다중대치(MI)",
    "Inverse probability weighting (IPW)" = "역확률가중(IPW)",
    "Weighted GEE (WGEE)" = "가중 GEE(WGEE)",
    "Strategy" = "방법", "Term" = "항", "Statistic" = "통계량",
    "Status" = "상태", "Note" = "참고", "Check" = "검토 항목",
    "Result" = "결과", "Interpretation" = "해석", "Recommendation" = "권고",
    "Analysis" = "분석", "Comparison" = "비교", "Metric" = "지표",
    "Value" = "값", "Item" = "항목", "Variable" = "변수",
    "Missing" = "결측", "Missing %" = "결측 %", "Rows" = "행 수",
    "Complete rows" = "완전관측 행", "Missing dependent variable" = "종속변수 결측",
    "Any missing selected variable" = "선택 변수 중 하나 이상 결측",
    "Any missing %" = "하나 이상 결측 %", "Software" = "소프트웨어",
    "Version" = "버전", "Section" = "구분", "SuggestedText" = "제안 문장",
    "Details" = "세부내용", "Direction" = "방향",
    "Methods" = "방법", "Results" = "결과", "Assumptions" = "가정",
    "Sensitivity" = "민감도 분석",
    "Ready" = "준비됨", "Needs review" = "검토 필요",
    "Not selected" = "선택하지 않음", "Not available" = "사용 불가",
    "Fitted" = "적합됨", "Fitted (selected)" = "적합됨(선택 모형)",
    "Failed" = "실패", "Computed" = "계산됨", "Available" = "사용 가능",
    "Not needed" = "필요하지 않음", "Reviewed" = "검토됨",
    "Potential violation" = "잠재적 위반", "No evidence of violation" = "위반 근거 없음",
    "Not primary" = "주요 검토 아님", "Not checked" = "검토하지 않음",
    "Design review required" = "연구설계 검토 필요", "RE assumption doubtful" = "확률효과 가정 의심",
    "Model rationale" = "모형 선택 근거",
    "Data structure summarized" = "자료 구조 요약",
    "Missing data described" = "결측자료 기술",
    "Analysis weights described" = "분석 가중치 기술",
    "Missing-data sensitivity engine run" = "결측자료 민감도 분석 실행",
    "Assumptions checked" = "가정 검토",
    "Recommended alternatives provided" = "대안 분석 제시",
    "Effect estimate and 95% CI reported" = "효과 추정치와 95% CI 보고",
    "Sensitivity analysis suggested" = "민감도 분석 제안",
    "Automated sensitivity comparison reported" = "자동 민감도 비교 보고",
    "Publication table notes generated" = "출판표 주석 생성",
    "Software/package version reported" = "소프트웨어/패키지 버전 보고",
    "Manuscript-ready text generated" = "원고용 문장 생성",
    "Residual normality" = "잔차 정규성", "Outcome family / link" = "결과변수 분포 / 링크",
    "GEE working correlation" = "GEE 작업상관", "Random-effects structure" = "확률효과 구조",
    "Convergence / singular fit" = "수렴 / 특이 적합", "Random-effect normality" = "확률효과 정규성",
    "Strict exogeneity / omitted confounding" = "강외생성 / 누락 교란",
    "Overdispersion" = "과산포", "Heteroskedasticity" = "이분산성",
    "Within-subject serial correlation" = "대상자 내 자기상관",
    "Cross-sectional dependence" = "횡단면 의존성", "FE vs RE assumption" = "고정효과 대 확률효과 가정",
    "GEE correlation sensitivity" = "GEE 상관구조 민감도",
    "Random-effects sensitivity" = "확률효과 민감도",
    "Panel model sensitivity" = "패널 모형 민감도",
    "Panel covariance sensitivity" = "패널 공분산 민감도",
    "Panel fixed effects" = "패널 고정효과", "Panel random effects" = "패널 확률효과",
    "Linear mixed model" = "선형 혼합모형", "Generalized linear mixed model" = "일반화 선형 혼합모형",
    "gaussian" = "가우시안", "binomial" = "이항", "poisson" = "Poisson",
    "negative_binomial" = "음이항", "gamma" = "감마",
    "exchangeable" = "교환가능", "independence" = "독립", "ar1" = "AR(1)", "unstructured" = "비구조화",
    "Hausman FE vs RE" = "Hausman 고정효과 대 확률효과",
    "Random intercept only" = "확률절편만 포함",
    "Random intercept + random slope for time" = "확률절편 + 시점 확률기울기",
    "No analysis weights were applied." = "분석 가중치를 적용하지 않았습니다.",
    "Use GEE when the target is a population-averaged longitudinal effect." =
      "모집단 평균 종단효과가 목표이면 GEE를 사용합니다.",
    "Use LMM when the target is subject-specific change in a continuous outcome and subject-specific intercepts/slopes are scientifically plausible." =
      "연속형 결과의 대상자 특이적 변화가 목표이고 대상자별 절편과 기울기가 과학적으로 타당하면 LMM을 사용합니다.",
    "Use LMM when the target is subject-specific change in a continuous outcome with cluster-level random intercepts." =
      "군집수준 확률절편을 포함한 연속형 결과의 대상자 특이적 변화가 목표이면 LMM을 사용합니다.",
    "Use panel fixed effects when time-invariant unit-level confounding must be controlled and within-unit change is the main source of identification." =
      "시간불변 단위수준 교란을 통제해야 하고 단위 내 변화가 주요 식별 근거이면 패널 고정효과를 사용합니다.",
    "Use panel random effects only when unit-specific unobserved effects are plausibly independent of included predictors; support this with Hausman screening and study design." =
      "단위별 미관측 효과가 포함된 예측변수와 독립이라는 가정이 타당할 때만 패널 확률효과를 사용하고 Hausman 선별과 연구설계로 이를 뒷받침하십시오.",
    "Report why this longitudinal model matches the estimand and data structure." =
      "이 종단모형이 추정대상과 자료 구조에 부합하는 이유를 보고하십시오.",
    "Assumption checks were not requested." = "가정 검토를 요청하지 않았습니다.",
    "No individual assumption checks were selected." = "개별 가정 검토 항목을 선택하지 않았습니다.",
    "No major assumption issue was detected by the selected screening checks. Continue with the selected model and report the repeated-measures structure." =
      "선택한 선별 검토에서 주요 가정 문제가 발견되지 않았습니다. 선택한 모형을 유지하고 반복측정 구조를 보고하십시오.",
    "Sensitivity analyses should be reported when feasible." = "가능하면 민감도 분석을 보고하십시오.",
    "Automated sensitivity comparisons were not available for this model." = "이 모형에서는 자동 민감도 비교를 사용할 수 없습니다.",
    "Software and package versions should be reported." = "소프트웨어와 패키지 버전을 보고해야 합니다.",
    "Assumption screening did not flag a major issue among the selected checks." = "선택한 검토 항목에서 주요 가정 문제가 발견되지 않았습니다.",
    "Assumption screening was not requested or no individual checks were selected." = "가정 선별을 요청하지 않았거나 개별 검토 항목을 선택하지 않았습니다.",
    "No MI/IPW/WGEE missing-data sensitivity engine was selected." = "MI/IPW/WGEE 결측자료 민감도 분석 방법을 선택하지 않았습니다.",
    "Missing-data sensitivity analysis with MI/IPW/WGEE was not selected." = "MI/IPW/WGEE 결측자료 민감도 분석을 선택하지 않았습니다.",
    "No automated sensitivity comparison was generated." = "자동 민감도 비교를 생성하지 않았습니다.",
    "MI/IPW/WGEE sensitivity engine was not selected." = "MI/IPW/WGEE 민감도 분석 방법을 선택하지 않았습니다.",
    "Analysis weight summary was not generated." = "분석 가중치 요약을 생성하지 않았습니다.",
    "Coefficient table includes B, SE, p-value, and 95% CI; exp(B) is added for logit/log models." =
      "계수표에는 B, SE, p값 및 95% CI가 포함되며 로짓/로그 모형에는 exp(B)를 추가합니다.",
    "Footnotes for estimand, standard errors, confidence intervals, missing data, and sensitivity interpretation are provided." =
      "추정대상, 표준오차, 신뢰구간, 결측자료 및 민감도 해석에 대한 주석을 제공합니다.",
    "Suggested Methods, Results, Assumptions, Sensitivity, and Software text is provided for manuscript drafting." =
      "원고 작성을 위한 방법, 결과, 가정, 민감도 분석 및 소프트웨어 문장을 제공합니다.",
    "Subject count, time points, balanced/unbalanced status, and cluster size are summarized." =
      "대상자 수, 시점 수, 균형/불균형 상태 및 군집 크기를 요약했습니다.",
    "Missing-data method, exclusions, and variable-level missingness are summarized." =
      "결측자료 처리 방법, 제외 사례 및 변수별 결측을 요약했습니다.",
    "Fixed-effect estimates were reported with standard errors, p-values, and 95% confidence intervals." =
      "고정효과 추정치를 표준오차, p값 및 95% 신뢰구간과 함께 보고했습니다.",
    "Exponentiated coefficients were additionally reported for the logit/log link model." =
      "로짓/로그 링크 모형에는 지수화 계수를 추가로 보고했습니다.",
    "No convergence message or singular-fit flag was detected." = "수렴 메시지나 특이 적합 표시가 발견되지 않았습니다.",
    "Random-effect normality screening was not significant." = "확률효과 정규성 선별검정은 유의하지 않았습니다.",
    "Random-effect normality looks acceptable by this screening test." = "이 선별검정에서 확률효과 정규성은 수용 가능한 것으로 보입니다.",
    "Repeat inference with robust sandwich standard errors and verify that conclusions are stable." =
      "강건 샌드위치 표준오차로 추론을 반복하고 결론의 안정성을 확인하십시오.",
    "For binary or count outcomes, compare family/link choices when outcome coding or distribution is uncertain." =
      "이분형 또는 카운트 결과의 코딩이나 분포가 불확실하면 분포/링크 선택을 비교하십시오.",
    "Compare random-intercept and random-slope specifications when time trends may differ by subject." =
      "대상자별 시점 추세가 다를 수 있으면 확률절편 모형과 확률기울기 모형을 비교하십시오.",
    "Repeat key conclusions with GEE if population-averaged inference is also relevant." =
      "모집단 평균 추론도 중요하면 GEE로 주요 결론을 다시 확인하십시오.",
    "Review conclusions after excluding influential subjects or sparse clusters." =
      "영향력이 큰 대상자 또는 희소 군집을 제외한 뒤 결론을 재검토하십시오.",
    "Compare results with and without time fixed effects." = "시점 고정효과 포함 여부에 따른 결과를 비교하십시오.",
    "Compare random effects with fixed effects and report the Hausman result." =
      "확률효과와 고정효과를 비교하고 Hausman 검정 결과를 보고하십시오.",
    "Singular fit" = "특이 적합", "Random-effect variance" = "확률효과 분산",
    "Residual variance" = "잔차 분산", "Approximate ICC" = "근사 ICC",
    "Count decision" = "카운트 분포 판정", "Selection rule" = "선택 규칙",
    "Poisson dispersion ratio" = "Poisson 산포비", "Overdispersion threshold" = "과산포 임계값",
    "Observed zero proportion" = "관측 영값 비율", "Poisson expected zero proportion" = "Poisson 기대 영값 비율",
    "Zero-inflation ratio" = "영과잉 비율", "Zero-inflation screen" = "영과잉 선별",
    "Poisson screening AIC" = "Poisson 선별 AIC", "Poisson screening BIC" = "Poisson 선별 BIC",
    "Negative binomial screening AIC" = "음이항 선별 AIC", "Negative binomial screening BIC" = "음이항 선별 BIC",
    "Decision rule" = "판정 규칙",
    "Normal residuals are not expected for GLM-family link-scale models." =
      "GLM 계열의 링크 척도 모형에서는 정규 잔차를 기대하지 않습니다.",
    "Assess whether the selected outcome family and link match the data instead of relying on normal residuals." =
      "잔차 정규성에 의존하지 말고 선택한 결과분포와 링크가 자료에 부합하는지 평가하십시오.",
    "At least 3 residuals are required for Shapiro-Wilk screening." = "Shapiro-Wilk 선별에는 잔차가 3개 이상 필요합니다.",
    "Use graphical residual review when more observations are available." = "관측치가 더 확보되면 잔차를 그래프로 검토하십시오.",
    "The normality screening test could not be computed." = "정규성 선별검정을 계산하지 못했습니다.",
    "Review Q-Q plots or use bootstrap / robust inference if residual normality is doubtful." =
      "잔차 정규성이 의심되면 Q-Q 도표를 검토하거나 부트스트랩/강건 추론을 사용하십시오.",
    "Residuals deviate from normality by Shapiro-Wilk screening." = "Shapiro-Wilk 선별 결과 잔차가 정규성에서 벗어났습니다.",
    "Shapiro-Wilk screening did not detect a normality problem." = "Shapiro-Wilk 선별에서 정규성 문제가 발견되지 않았습니다.",
    "Use robust or bootstrap confidence intervals; for clearly non-Gaussian outcomes, switch to GEE / GLMM with the appropriate family." =
      "강건 또는 부트스트랩 신뢰구간을 사용하고 명확히 비가우시안인 결과는 적절한 분포의 GEE/GLMM으로 전환하십시오.",
    "Continue with the selected Gaussian model; still review residual plots for shape and outliers." =
      "선택한 가우시안 모형을 유지하되 잔차 도표의 형태와 이상치를 검토하십시오.",
    "Confirm that the selected family matches the outcome scale; for count outcomes, review the Poisson dispersion-threshold screening and treat AIC/BIC as supplementary diagnostics." =
      "선택한 분포가 결과척도에 부합하는지 확인하고 카운트 결과는 Poisson 산포 임계값 선별을 검토하며 AIC/BIC는 보조 진단으로 해석하십시오.",
    "Compare exchangeable, AR(1), and independence structures when the repeated-measures pattern supports them." =
      "반복측정 패턴이 허용하면 교환가능, AR(1), 독립 구조를 비교하십시오.",
    "Check convergence and simplify the random-effects structure if the model is singular or unstable." =
      "수렴을 확인하고 모형이 특이하거나 불안정하면 확률효과 구조를 단순화하십시오.",
    "Add a random slope only when subject-specific time trends are substantively expected and supported by the data." =
      "대상자별 시점 추세가 실질적으로 예상되고 자료가 뒷받침할 때만 확률기울기를 추가하십시오.",
    "Simplify the random-effects structure, review sparse clusters, or refit with an alternative optimizer before final interpretation." =
      "최종 해석 전에 확률효과 구조를 단순화하고 희소 군집을 검토하거나 다른 최적화 방법으로 다시 적합하십시오.",
    "Continue with the selected mixed model; still review cluster sizes and random-effect estimates." =
      "선택한 혼합모형을 유지하되 군집 크기와 확률효과 추정치를 검토하십시오.",
    "At least 3 estimated random effects are required for screening." = "선별에는 추정된 확률효과가 3개 이상 필요합니다.",
    "Review the random-effects distribution graphically when enough clusters are available." =
      "군집 수가 충분하면 확률효과 분포를 그래프로 검토하십시오.",
    "Random-effect normality screening could not be computed." = "확률효과 정규성 선별검정을 계산하지 못했습니다.",
    "Review random-effect quantile plots if random-effect distribution is important." = "확률효과 분포가 중요하면 분위수 도표를 검토하십시오.",
    "Estimated random effects deviate from normality by Shapiro-Wilk screening." =
      "Shapiro-Wilk 선별 결과 추정된 확률효과가 정규성에서 벗어났습니다.",
    "Use graphical review and sensitivity analysis; consider GEE if population-averaged inference is the primary target." =
      "그래프 검토와 민감도 분석을 수행하고 모집단 평균 추론이 주 목표이면 GEE를 고려하십시오.",
    "Fixed effects remove time-invariant unit confounding, but time-varying omitted confounding and reverse causation can still bias estimates." =
      "고정효과는 시간불변 단위 교란을 제거하지만 시간가변 누락 교란과 역인과는 여전히 추정치를 편향시킬 수 있습니다.",
    "Random effects additionally require unit-specific unobserved factors to be independent of included predictors." =
      "확률효과는 단위별 미관측 요인이 포함된 예측변수와 독립이라는 가정을 추가로 요구합니다.",
    "Residual degrees of freedom were not available for overdispersion screening." = "과산포 선별에 필요한 잔차 자유도를 사용할 수 없습니다.",
    "For count or binary clustered outcomes, review dispersion and sparse cells before final interpretation." =
      "카운트 또는 이분형 군집 결과는 최종 해석 전에 산포와 희소 셀을 검토하십시오.",
    "Pearson residual dispersion is elevated." = "Pearson 잔차 산포가 높습니다.",
    "Pearson residual dispersion is not elevated by this screening rule." = "이 선별 규칙에서는 Pearson 잔차 산포가 높지 않습니다.",
    "Breusch-Pagan screening is mainly intended for Gaussian mean models." = "Breusch-Pagan 선별은 주로 가우시안 평균 모형을 대상으로 합니다.",
    "The lmtest package is not available." = "lmtest 패키지를 사용할 수 없습니다.",
    "Breusch-Pagan screening could not be computed for this model frame." = "이 모형 프레임에서 Breusch-Pagan 선별을 계산하지 못했습니다.",
    "Residual variance appears non-constant." = "잔차 분산이 일정하지 않은 것으로 보입니다.",
    "Breusch-Pagan screening did not detect non-constant variance." = "Breusch-Pagan 선별에서 비일정 분산이 발견되지 않았습니다.",
    "Panel residuals show evidence of serial correlation." = "패널 잔차에서 자기상관 근거가 나타났습니다.",
    "Panel serial correlation screening was not significant." = "패널 자기상관 선별검정은 유의하지 않았습니다.",
    "There were not enough within-subject residual pairs for screening." = "선별에 필요한 대상자 내 잔차 쌍이 충분하지 않았습니다.",
    "Lagged residual correlation screening could not be computed." = "시차 잔차 상관 선별을 계산하지 못했습니다.",
    "Lag-1 residual correlation within subjects was detected." = "대상자 내 1차 시차 잔차 상관이 발견되었습니다.",
    "Lag-1 residual correlation screening was not significant." = "1차 시차 잔차 상관 선별검정은 유의하지 않았습니다.",
    "No additional serial-correlation adjustment is suggested by this screening test." =
      "이 선별검정에서는 추가 자기상관 보정이 필요하지 않습니다.",
    "Cross-sectional dependence screening is available for panel FE / RE models." = "횡단면 의존성 선별은 패널 고정효과/확률효과 모형에서 사용할 수 있습니다.",
    "The plm package is not available." = "plm 패키지를 사용할 수 없습니다.",
    "Pesaran CD screening could not be computed for this panel structure." = "이 패널 구조에서 Pesaran CD 선별을 계산하지 못했습니다.",
    "Residuals may be correlated across subjects or clusters at the same time point." = "같은 시점에서 대상자 또는 군집 간 잔차가 상관될 수 있습니다.",
    "Pesaran CD screening did not detect cross-sectional dependence." = "Pesaran CD 선별에서 횡단면 의존성이 발견되지 않았습니다.",
    "Hausman screening applies to panel fixed-effects versus random-effects selection." = "Hausman 선별은 패널 고정효과와 확률효과 선택에 적용됩니다.",
    "Hausman screening could not be computed." = "Hausman 선별을 계산하지 못했습니다.",
    "The random-effects independence assumption is not supported by Hausman screening." = "Hausman 선별에서 확률효과 독립성 가정이 지지되지 않았습니다.",
    "Hausman screening did not reject the random-effects independence assumption." = "Hausman 선별에서 확률효과 독립성 가정을 기각하지 않았습니다.",
    "Prefer panel fixed effects over random effects for coefficient interpretation." = "계수 해석에는 패널 확률효과보다 고정효과를 우선 고려하십시오.",
    "Panel random effects can be considered if it matches the study question." = "연구질문에 부합하면 패널 확률효과를 고려할 수 있습니다.",
    "Dispersion-threshold screening selects Poisson versus negative binomial; AIC/BIC are reported as supplementary fit diagnostics, not as the automatic selection rule." =
      "산포 임계값 선별로 Poisson과 음이항을 선택하며 AIC/BIC는 자동 선택 규칙이 아니라 보조 적합 진단으로 보고합니다.",
    "Possible excess zeros; consider zero-inflated or hurdle sensitivity analysis." = "영과잉 가능성이 있으므로 영과잉 또는 허들 민감도 분석을 고려하십시오.",
    "No excess-zero flag by the simple Poisson zero screen." = "단순 Poisson 영값 선별에서 영과잉 표시가 없습니다.",
    "Compare conclusions across plausible working correlation structures." = "타당한 작업상관 구조 간 결론을 비교하십시오.",
    "Singular fit detected; prefer simpler random-effects structure unless justified." = "특이 적합이 발견되었습니다. 정당한 근거가 없으면 더 단순한 확률효과 구조를 우선하십시오.",
    "Review AIC/BIC together with convergence and subject-matter plausibility." = "AIC/BIC를 수렴 여부 및 연구분야 타당성과 함께 검토하십시오.",
    "Compare FE and RE estimates and interpret with the Hausman test and study design." = "고정효과와 확률효과 추정치를 비교하고 Hausman 검정 및 연구설계와 함께 해석하십시오.",
    "Use as sensitivity inference when cross-sectional dependence or common shocks are plausible; report the chosen covariance estimator explicitly." =
      "횡단면 의존성 또는 공통 충격이 타당하면 민감도 추론으로 사용하고 선택한 공분산 추정량을 명시하십시오.",
    "GEE estimates population-averaged effects with robust sandwich standard errors." =
      "GEE는 강건 샌드위치 표준오차로 모집단 평균 효과를 추정합니다.",
    "LMM estimates subject-specific fixed effects while allowing cluster-level random effects." =
      "LMM은 군집수준 확률효과를 허용하면서 대상자 특이적 고정효과를 추정합니다.",
    "GLMM estimates subject-specific effects on the model link scale." =
      "GLMM은 모형 링크 척도에서 대상자 특이적 효과를 추정합니다.",
    "Panel regression uses the selected ID and time variables as panel indexes." =
      "패널 회귀는 선택한 ID와 시점 변수를 패널 인덱스로 사용합니다.",
    "Panel coefficient standard errors use group-clustered HC1 robust covariance." =
      "패널 계수의 표준오차는 집단 군집화 HC1 강건 공분산을 사용합니다.",
    "Exponentiated coefficients are reported as OR for binomial models, rate ratios for count models, and mean ratios for Gamma log-link models." =
      "지수화 계수는 이분형 모형에서 OR, 카운트 모형에서 발생률비, 감마 로그링크 모형에서 평균비로 보고합니다."
  )
  translate_one <- function(value) {
    value <- trimws(as.character(value %||% ""))
    if (!nzchar(value)) return(value)
    if (value %in% names(exact)) return(unname(exact[[value]]))
    # Legacy flattened text has no reliable boundary between labels and user
    # names. New checklist tables carry structured source cells instead.
    if (startsWith(value, "Weight variable:")) return(value)
    common <- result_appendix_ui_text(value, language)
    if (!identical(common, value)) return(common)
    capture <- function(pattern) {
      match <- regmatches(value, regexec(pattern, value, perl = TRUE))[[1L]]
      if (length(match) > 0L) match else character(0)
    }
    matched <- capture("^Complete \\(n=([0-9]+)\\)$")
    if (length(matched)) return(sprintf("완전관측 (n=%s)", matched[[2L]]))
    matched <- capture("^Model ([0-9]+)$")
    if (length(matched)) return(sprintf("모형 %s", matched[[2L]]))
    matched <- capture("^Working correlation: (.+)$")
    if (length(matched)) return(sprintf("작업상관: %s", matched[[2L]]))
    matched <- capture("^Working correlation structure: (.+)\\.$")
    if (length(matched)) return(sprintf("작업상관 구조: %s.", matched[[2L]]))
    matched <- capture("^Random intercept grouping variable: (.+)\\.$")
    if (length(matched)) return(sprintf("확률절편 집단변수: %s.", matched[[2L]]))
    matched <- capture("^A random slope for the selected time variable \\((.+)\\) was included\\.$")
    if (length(matched)) return(sprintf("선택한 시점 변수(%s)의 확률기울기를 포함했습니다.", matched[[2L]]))
    matched <- capture("^Additional cluster-level random intercept grouping variable: (.+)\\.$")
    if (length(matched)) return(sprintf("추가 군집수준 확률절편 집단변수: %s.", matched[[2L]]))
    matched <- capture("^Random intercept by (.+); random slope for (.+)$")
    if (length(matched)) return(sprintf("%s별 확률절편; %s의 확률기울기", matched[[2L]], matched[[3L]]))
    matched <- capture("^Random intercept by (.+)$")
    if (length(matched)) return(sprintf("%s별 확률절편", matched[[2L]]))
    matched <- capture("^R-squared \\((.+)\\)$")
    if (length(matched)) return(sprintf("R² (%s)", matched[[2L]]))
    matched <- capture("^The fitted (.+) uses (.+)\\.$")
    if (length(matched)) return(sprintf("적합한 %s은(는) %s을(를) 사용합니다.", longitudinal_appendix_text(matched[[2L]], language), longitudinal_appendix_text(matched[[3L]], language)))
    matched <- capture("^GEE \\((.+)\\)$")
    if (length(matched)) return(sprintf("GEE(%s)", longitudinal_appendix_text(matched[[2L]], language)))
    matched <- capture("^The selected working correlation is (.+)\\.$")
    if (length(matched)) return(sprintf("선택한 작업상관은 %s입니다.", matched[[2L]]))
    matched <- capture("^Subject-level random intercepts are grouped by ([^.]+), with a random slope for ([^.]+)\\.(.*)$")
    if (length(matched)) return(sprintf("대상자수준 확률절편은 %s별로 묶고 %s의 확률기울기를 포함합니다. %s", matched[[2L]], matched[[3L]], matched[[4L]]))
    matched <- capture("^Subject-level random intercepts are grouped by ([^.]+)\\.(.*)$")
    if (length(matched)) return(sprintf("대상자수준 확률절편은 %s별로 묶습니다. %s", matched[[2L]], matched[[3L]]))
    matched <- capture("^An additional cluster-level random intercept is grouped by (.+)\\.$")
    if (length(matched)) return(sprintf("추가 군집수준 확률절편은 %s별로 묶습니다.", matched[[2L]]))
    matched <- capture("^Mixed model fit warning: (.+)\\.$")
    if (length(matched)) return(sprintf("혼합모형 적합 경고: %s.", matched[[2L]]))
    matched <- capture("^Use GEE when the target is a population-averaged longitudinal effect\\. Report the selected working correlation \\((.+)\\) and robust sandwich inference\\.$")
    if (length(matched)) return(sprintf("모집단 평균 종단효과가 목표이면 GEE를 사용합니다. 선택한 작업상관(%s)과 강건 샌드위치 추론을 보고하십시오.", matched[[2L]]))
    matched <- capture("^Report the selected working correlation \\((.+)\\) and robust sandwich inference\\.$")
    if (length(matched)) return(sprintf("선택한 작업상관(%s)과 강건 샌드위치 추론을 보고하십시오.", matched[[2L]]))
    matched <- capture("^Use GLMM when the target is subject-specific inference for a non-Gaussian outcome using the (.+) family\\.$")
    if (length(matched)) return(sprintf("%s 분포를 사용하는 비가우시안 결과에서 대상자 특이적 추론이 목표이면 GLMM을 사용합니다.", matched[[2L]]))
    matched <- capture("^Compare the selected GEE working correlation \\((.+)\\) with independence, exchangeable, and AR\\(1\\) when feasible\\.$")
    if (length(matched)) return(sprintf("가능하면 선택한 GEE 작업상관(%s)을 독립, 교환가능 및 AR(1) 구조와 비교하십시오.", matched[[2L]]))
    matched <- capture("^The analysis used (.+), including ([^ ]+) observations from ([^ ]+) subjects/clusters across ([^ ]+) observed time points\\.$")
    if (length(matched)) return(sprintf("분석에는 %s을(를) 사용했으며 %s개 대상자/군집의 %s개 관측치를 %s개 시점에 걸쳐 포함했습니다.", longitudinal_appendix_text(matched[[2L]], language), matched[[4L]], matched[[3L]], matched[[5L]]))
    matched <- capture("^([0-9]+) observations were excluded because of missing values in selected analysis variables\\.$")
    if (length(matched)) return(sprintf("선택한 분석변수의 결측값으로 관측치 %s개를 제외했습니다.", matched[[2L]]))
    matched <- capture("^([0-9]+) model row\\(s\\) were excluded because selected variables were missing in that row; subjects with other observed visits remained in the likelihood-based analysis under the MAR assumption\\.$")
    if (length(matched)) return(sprintf("해당 행의 선택 변수 결측으로 모형 행 %s개를 제외했지만 다른 시점 관측값이 있는 대상자는 MAR 가정의 우도 기반 분석에 유지했습니다.", matched[[2L]]))
    matched <- capture("^Analysis weights were applied as (.+) with effective sample size (.+)\\.$")
    if (length(matched)) return(sprintf("%s 방식으로 분석 가중치를 적용했으며 유효표본크기는 %s입니다.", longitudinal_appendix_text(matched[[2L]], language), matched[[3L]]))
    matched <- capture("^Missing-data sensitivity engines were run for (.+) \\(([0-9]+) fitted row\\(s\\), ([0-9]+) failed row\\(s\\)\\); MI/IPW/WGEE outputs should be reported as sensitivity analyses unless the missing-data model is prespecified as primary\\.$")
    if (length(matched)) return(sprintf("%s 결측자료 민감도 분석을 실행했습니다(적합 행 %s개, 실패 행 %s개). 결측자료 모형을 사전에 주 분석으로 지정하지 않았다면 MI/IPW/WGEE 결과는 민감도 분석으로 보고하십시오.", matched[[2L]], matched[[3L]], matched[[4L]]))
    matched <- capture("^Analyses were performed using (.+)\\.$")
    if (length(matched)) return(sprintf("분석에는 %s를 사용했습니다.", matched[[2L]]))
    matched <- capture("^Fixed-effect estimates were reported for (.+) with 95% confidence intervals\\.$")
    if (length(matched)) return(sprintf("%s의 고정효과 추정치를 95%% 신뢰구간과 함께 보고했습니다.", matched[[2L]]))
    matched <- capture("^Assumption screening flagged (.+); recommended alternative analyses or reporting cautions were generated accordingly\\.$")
    if (length(matched)) return(sprintf("가정 선별에서 %s 항목이 표시되어 대안 분석 또는 보고상 주의사항을 제시했습니다.", matched[[2L]]))
    matched <- capture("^Automated sensitivity screening generated ([0-9]+) comparison row\\(s\\), including fitted alternatives and failed alternatives where applicable\\.$")
    if (length(matched)) return(sprintf("자동 민감도 선별에서 비교 행 %s개를 생성했으며 해당되는 경우 적합된 대안과 실패한 대안을 모두 포함했습니다.", matched[[2L]]))
    matched <- capture("^([0-9]+) assumption check item\\(s\\) reported\\.$")
    if (length(matched)) return(sprintf("가정 검토 항목 %s개를 보고했습니다.", matched[[2L]]))
    matched <- capture("^([0-9]+) missing-data sensitivity result row\\(s\\) generated\\.$")
    if (length(matched)) return(sprintf("결측자료 민감도 분석 결과 행 %s개를 생성했습니다.", matched[[2L]]))
    matched <- capture("^([0-9]+) automated sensitivity comparison row\\(s\\) generated\\.$")
    if (length(matched)) return(sprintf("자동 민감도 비교 행 %s개를 생성했습니다.", matched[[2L]]))
    matched <- capture("^For GEE, compare working correlation structures when clinically plausible; current structure is (.+)\\.$")
    if (length(matched)) return(sprintf("임상적으로 타당하면 GEE 작업상관 구조를 비교하십시오. 현재 구조는 %s입니다.", matched[[2L]]))
    matched <- capture("^Observation model: (.+); weights clipped to \\[(.+), (.+)\\] and normalized to mean 1\\. Report these variables and review positivity/weight stability\\.$")
    if (length(matched)) return(sprintf("관측모형: %s; 가중치를 [%s, %s] 범위로 절단하고 평균 1로 정규화했습니다. 이 변수들을 보고하고 양성성/가중치 안정성을 검토하십시오.", matched[[2L]], matched[[3L]], matched[[4L]]))
    matched <- capture("^Standard mice-based MI sensitivity; pooled across ([0-9]+) imputed dataset\\(s\\) using Rubin-style total variance\\. (.+) This is not a dedicated multilevel MI engine\\.$")
    if (length(matched)) return(sprintf("표준 mice 기반 MI 민감도 분석으로 대치자료 %s개를 Rubin 방식 총분산으로 통합했습니다. %s 전용 다층 MI 방법은 아닙니다.", matched[[2L]], longitudinal_appendix_text(matched[[3L]], language)))
    matched <- capture("^Observation model failed \\((.+)\\); intercept-only IPW was used\\. Treat this as a weak IPW sensitivity analysis\\.$")
    if (length(matched)) return(sprintf("관측모형 적합에 실패하여(%s) 절편만 포함한 IPW를 사용했습니다. 제한적인 IPW 민감도 분석으로 해석하십시오.", longitudinal_input_error_text(simpleError(matched[[2L]]), language)))
    value
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  translated_lines <- vapply(lines, function(line) {
    translated <- translate_one(line)
    if (!identical(translated, trimws(line)) || !grepl("(?<=[.!?])\\s+", line, perl = TRUE)) return(translated)
    sentences <- strsplit(line, "(?<=[.!?])\\s+", perl = TRUE)[[1L]]
    paste(vapply(sentences, translate_one, character(1)), collapse = " ")
  }, character(1))
  paste(translated_lines, collapse = "\n")
}

longitudinal_appendix_table <- function(table, language = NULL) {
  if (!is.data.frame(table)) return(table)
  language <- result_appendix_table_language(language)
  localized <- result_appendix_localize_table(table, language)
  if (identical(language, "en")) return(localized)
  original_names <- names(table)
  names(localized) <- vapply(original_names, longitudinal_appendix_text, character(1), language = language)
  for (index in seq_along(localized)) {
    if (!is.character(localized[[index]]) && !is.factor(localized[[index]])) next
    localized[[index]] <- vapply(as.character(table[[index]]), longitudinal_appendix_text, character(1), language = language)
  }
  structure_messages <- attr(table, "longitudinal_structure_messages", exact = TRUE)
  if (is.list(structure_messages) && "Interpretation" %in% original_names) {
    column <- match("Interpretation", original_names)
    for (entry in structure_messages) {
      rows <- which(table[[column]] == entry$source)
      if (!length(rows)) next
      text <- if (isTRUE(entry$slope)) sprintf(statedu_t("longitudinal.mixed_message.slope", language), entry$id, entry$time)
        else sprintf(statedu_t("longitudinal.mixed_message.intercept", language), entry$id)
      if (length(entry$cluster)) text <- paste(text,
        sprintf(statedu_t("longitudinal.mixed_message.cluster", language), entry$cluster))
      localized[[column]][rows] <- text
    }
  }
  parts <- attr(table, "longitudinal_manuscript_parts", exact = TRUE)
  weight_details <- attr(table, "longitudinal_weight_details", exact = TRUE)
  detail_column <- match("Details", original_names)
  checklist_messages <- attr(table, "longitudinal_checklist_messages", exact = TRUE)
  if (!is.na(detail_column) && "Item" %in% original_names && is.list(checklist_messages)) {
    for (entry in checklist_messages) {
      row <- which(table$Item == entry$item)
      if (length(row) == 1L && is.character(entry$messages) &&
          identical(table[[detail_column]][row], paste(entry$messages, collapse = " "))) {
        localized[[detail_column]][row] <- paste(vapply(entry$messages, longitudinal_appendix_text, character(1), language = language), collapse = " ")
      }
    }
  }
  weight_row <- if ("Item" %in% original_names) which(table$Item == "Analysis weights described") else integer(0)
  if (length(weight_row) == 1L && !is.na(detail_column) && is.list(weight_details) &&
      identical(table[[detail_column]][weight_row], weight_details$source) && is.data.frame(weight_details$table)) {
    weight_table <- longitudinal_appendix_table(longitudinal_display_weight_summary_table(list(weight_summary = weight_details$table)), language)
    localized[[detail_column]][weight_row] <- paste(sprintf("%s: %s", weight_table[[1L]], weight_table[[2L]]), collapse = " ")
  }
  text_column <- match("SuggestedText", original_names)
  if (!is.na(text_column) && is.list(parts) && identical(parts$source, table[[text_column]]) &&
      is.list(parts$rows) && length(parts$rows) == nrow(table)) {
    localized[[text_column]] <- vapply(parts$rows, function(messages) {
      messages <- messages[!is.na(messages) & nzchar(messages)]
      paste(vapply(messages, longitudinal_appendix_text, character(1), language = language), collapse = " ")
    }, character(1))
  }
  attr(localized, "result_table_role") <- "appendix"
  attr(localized, "result_table_language") <- language
  guide_parts <- attr(table, "longitudinal_guide_structure", exact = TRUE)
  if (is.list(guide_parts)) for (entry in guide_parts) {
    if (identical(table[[entry$column]][entry$row], entry$source)) {
      localized[[entry$column]][entry$row] <- do.call(sprintf,
        c(list(statedu_t(paste0("longitudinal.guide.", entry$key), language)), entry$args))
    }
  }
  preserved <- result_appendix_preserve_data(localized, table)
  if (isTRUE(attr(table, "longitudinal_sensitivity_comparison", exact = TRUE))) {
    comparison <- match("Comparison", names(table))
    if (!is.na(comparison)) preserved[[comparison]] <- localized[[comparison]]
    if (all(c("Status", "Note") %in% names(table))) {
      failed <- which(table$Status == "Failed")
      note <- match("Note", names(table))
      # Only registered application errors are translated; external details
      # can contain variable names, line breaks, and diagnostic-like words.
      preserved[[note]][failed] <- vapply(table$Note[failed], function(message)
        longitudinal_input_error_text(simpleError(message), language), character(1))
    }
  }
  overview_rows <- attr(table, "longitudinal_overview_app_rows", exact = TRUE)
  # Outcome labels may themselves be identity-column names (e.g. Outcome).
  # Only the explicitly marked application rows may override column protection.
  if (is.numeric(overview_rows) && ncol(table) > 1L) {
    overview_rows <- overview_rows[is.finite(overview_rows) & overview_rows >= 1L & overview_rows <= nrow(table)]
    for (column in seq.int(2L, ncol(table))) preserved[[column]][overview_rows] <- localized[[column]][overview_rows]
  }
  preserved
}

longitudinal_diagnostics_section <- function(warnings, skipped, title = "Warnings / skipped models", class = "result-section regression-result-panel") {
  rows <- Filter(Negate(is.null), list(
    analysis_diagnostics_row(warnings, "Warning"),
    analysis_diagnostics_row(skipped, "Skipped")
  ))
  if (length(rows) == 0L) return(NULL)
  table <- do.call(rbind, rows)
  if (all(!nzchar(table$N))) table$N <- NULL
  table <- longitudinal_appendix_table(table)
  analysis_result_table_section(
    longitudinal_appendix_text(title),
    table,
    class = class,
    table_fn = analysis_diagnostics_html_table
  )
}

longitudinal_role_table <- function(table, role = "main") {
  if (!is.data.frame(table)) return(table)
  role <- result_table_role(role)
  if (identical(role, "appendix")) {
    return(longitudinal_appendix_table(table))
  }
  attr(table, "result_table_role") <- "main"
  attr(table, "result_table_language") <- result_main_table_language()
  table
}

longitudinal_table_section <- function(title, table, role = "appendix", table_fn = model_overview_html_table, note_line = "") {
  if (!is.data.frame(table) || nrow(table) == 0L) return(NULL)
  role <- result_table_role(role)
  marked <- longitudinal_role_table(table, role)
  heading <- if (identical(role, "appendix")) longitudinal_appendix_text(title) else title
  content <- if (identical(table_fn, coefficient_html_table)) {
    table_fn(marked, note_line = note_line, table_role = role)
  } else if (identical(table_fn, longitudinal_coef_html_table)) {
    table_fn(marked, table_role = role, note_line = note_line)
  } else {
    table_fn(marked)
  }
  div(
    class = paste("result-section regression-result-panel longitudinal-result-panel", paste0("longitudinal-result-panel--", role)),
    h3(heading),
    content
  )
}

longitudinal_coefficient_predictors <- function(result) {
  terms <- as.character(result$terms %||% character(0))
  if (length(terms) == 0) {
    terms <- unique(c(
      if (isTRUE(result$include_time %||% TRUE)) result$time else character(0),
      result$predictors,
      result$covariates
    ))
  }
  terms[nzchar(terms)]
}

longitudinal_reference_values <- function(result, category_table = NULL) {
  refs <- regression_reference_values_static(category_table)
  result_refs <- result$reference_values %||% character(0)
  if (length(result_refs) > 0 && !is.null(names(result_refs))) {
    keep <- nzchar(names(result_refs))
    refs[names(result_refs)[keep]] <- as.character(result_refs[keep])
  }
  refs
}

longitudinal_display_coef_table <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  output <- data.frame(
    Term = as.character(table$Term),
    B = table$B,
    SE = table$SE,
    Statistic = table$Statistic,
    p = table$p,
    LLCI = table$LLCI,
    ULCI = table$ULCI,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (isTRUE(result$exponentiate) && all(c("exp(B)", "exp(LLCI)", "exp(ULCI)") %in% names(table))) {
    output$`exp(B)` <- table$`exp(B)`
    output$`exp(LLCI)` <- table$`exp(LLCI)`
    output$`exp(ULCI)` <- table$`exp(ULCI)`
  }
  if ("df" %in% names(table)) output$df <- table$df
  formatted <- coefficient_output_table_with_context(
    output,
    predictors = longitudinal_coefficient_predictors(result),
    variable_info = variable_table,
    refs = longitudinal_reference_values(result, category_table),
    value_labels = category_value_label_lookup_static(category_table),
    labels = labels,
    category_table = category_table
  )
  rownames(formatted) <- NULL
  formatted
}

longitudinal_publication_estimate_table <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  estimate_ci <- sprintf(
    "%s (%s to %s)",
    vapply(table$B, longitudinal_format_number, character(1)),
    vapply(table$LLCI, longitudinal_format_number, character(1)),
    vapply(table$ULCI, longitudinal_format_number, character(1))
  )
  direction <- ifelse(
    suppressWarnings(as.numeric(table$B)) > 0,
    "Positive",
    ifelse(suppressWarnings(as.numeric(table$B)) < 0, "Negative", "Neutral")
  )
  output <- data.frame(
    Term = as.character(table$Term),
    `B (95% CI)` = estimate_ci,
    SE = vapply(table$SE, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    Direction = direction,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (isTRUE(result$exponentiate) && all(c("exp(B)", "exp(LLCI)", "exp(ULCI)") %in% names(table))) {
    output$`exp(B) (95% CI)` <- sprintf(
      "%s (%s to %s)",
      vapply(table$`exp(B)`, longitudinal_format_number, character(1)),
      vapply(table$`exp(LLCI)`, longitudinal_format_number, character(1)),
      vapply(table$`exp(ULCI)`, longitudinal_format_number, character(1))
    )
  }
  output
}

longitudinal_result_title <- function(result, variable_table = NULL, labels = character(0)) {
  outcome <- display_variable_name_static(result$outcome, variable_table, labels, label_only = TRUE)
  sprintf("%s: %s", result$method, outcome)
}

longitudinal_display_assumption_table <- function(result) {
  table <- result$assumption_checks
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  output <- data.frame(
    Check = as.character(table$Check),
    Result = as.character(table$Result),
    Statistic = vapply(table$Statistic, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    Interpretation = as.character(table$Interpretation),
    Recommendation = as.character(table$Recommendation),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(output, "longitudinal_structure_messages") <- attr(table, "longitudinal_structure_messages", exact = TRUE)
  output
}

longitudinal_display_missing_table <- function(result) {
  table <- result$missing_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  data.frame(
    Variable = as.character(table$Variable),
    Missing = as.character(table$Missing),
    `Missing %` = vapply(table$MissingPercent, longitudinal_format_number, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_display_missing_pattern_table <- function(result) {
  table <- result$missing_pattern
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

longitudinal_display_missing_by_time_table <- function(result) {
  table <- result$missing_by_time
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  output <- table
  attr(output, "result_user_columns") <- unique(c(attr(output, "result_user_columns", exact = TRUE), "Time"))
  if ("Any missing %" %in% names(output)) {
    output$`Any missing %` <- vapply(output$`Any missing %`, longitudinal_format_number, character(1))
  }
  output
}

longitudinal_display_missing_sensitivity_table <- function(result) {
  table <- result$missing_sensitivity_results
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  output <- data.frame(
    Strategy = as.character(table$Strategy),
    Term = as.character(table$Term),
    B = vapply(table$B, longitudinal_format_number, character(1)),
    SE = vapply(table$SE, longitudinal_format_number, character(1)),
    Statistic = vapply(table$Statistic, longitudinal_format_number, character(1)),
    p = vapply(table$p, format_p, character(1)),
    LLCI = vapply(table$LLCI, longitudinal_format_number, character(1)),
    ULCI = vapply(table$ULCI, longitudinal_format_number, character(1)),
    Status = as.character(table$Status),
    Note = as.character(table$Note),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (all(c("exp(B)", "exp(LLCI)", "exp(ULCI)") %in% names(table))) {
    output$`exp(B)` <- vapply(table$`exp(B)`, longitudinal_format_number, character(1))
    output$`exp(LLCI)` <- vapply(table$`exp(LLCI)`, longitudinal_format_number, character(1))
    output$`exp(ULCI)` <- vapply(table$`exp(ULCI)`, longitudinal_format_number, character(1))
  }
  output
}

longitudinal_display_weight_summary_table <- function(result) {
  table <- result$weight_summary
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  output <- data.frame(
    Item = as.character(table$Item),
    Value = as.character(table$Value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rows <- which(table$Item == "Weight variable")
  attr(output, "result_user_cells") <- rbind(attr(table, "result_user_cells", exact = TRUE),
    cbind(as.numeric(rows), rep(2, length(rows))))
  output
}

longitudinal_model_overview_table <- function(results, variable_table = NULL, labels = character(0)) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  rows <- c("N", "Clusters", "Time points", "Outcome", "ID", "Time", "Exposure / offset", "Weight", "Method", "Requested family", "Fitted family", "Formula", "AIC", "BIC")
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(results)) {
    result <- results[[index]]
    outcome <- display_variable_name_static(result$outcome, variable_table, labels, label_only = TRUE)
    id <- display_variable_name_static(result$id, variable_table, labels, label_only = TRUE)
    time <- display_variable_name_static(result$time, variable_table, labels, label_only = TRUE)
    offset <- display_variable_name_static(result$offset_variable %||% character(0), variable_table, labels, label_only = TRUE)
    column <- outcome
    if (column %in% names(table)) {
      column <- paste(column, index)
    }
    table[[column]] <- c(
      as.character(result$n),
      as.character(result$clusters),
      as.character(result$time_points),
      outcome,
      id,
      time,
      offset,
      paste(as.character(result$weight %||% character(0)), collapse = ", "),
      result$method,
      result$requested_family %||% result$family,
      result$family,
      paste(deparse(result$formula), collapse = " "),
      longitudinal_format_number(result$aic),
      longitudinal_format_number(result$bic)
    )
  }
  user_rows <- which(rows %in% c("Outcome", "ID", "Time", "Exposure / offset", "Weight", "Formula"))
  attr(table, "result_user_headers") <- seq.int(2L, ncol(table))
  attr(table, "result_user_cells") <- as.matrix(expand.grid(user_rows, seq.int(2L, ncol(table))))
  attr(table, "longitudinal_overview_app_rows") <- setdiff(seq_along(rows), user_rows)
  table
}

longitudinal_assumption_review_table <- function(results) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  rows <- c("Estimand", "Correlation / random effect", "Package")
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  guide_parts <- list()
  for (index in seq_along(results)) {
    result <- results[[index]]
    repeated_reml <- identical(result$model_type, "lmm") && result$corstr %in% c("reml_un", "reml_ar1")
    estimand <- if (isTRUE(repeated_reml) || identical(result$model_type, "gee")) {
      "Population-averaged"
    } else if (result$model_type %in% c("lmm", "glmm")) {
      "Subject-specific"
    } else {
      "Panel unit effect"
    }
    structure <- if (isTRUE(repeated_reml)) {
      sprintf("REML residual covariance: %s; no random effects", if (identical(result$corstr, "reml_un")) "UN" else "AR(1)")
    } else if (identical(result$model_type, "gee")) {
      sprintf("Working correlation: %s", result$corstr)
    } else if (result$model_type %in% c("lmm", "glmm")) {
      id_label <- display_variable_name_static(result$id, NULL, character(0), label_only = TRUE)
      time_label <- display_variable_name_static(result$time, NULL, character(0), label_only = TRUE)
      if (isTRUE(result$random_slope)) {
        sprintf("Random intercept by %s; random slope for %s", id_label, time_label)
      } else {
        sprintf("Random intercept by %s", id_label)
      }
    } else {
      if (identical(result$model_type, "panel_fe")) "Fixed effects" else "Random effects"
    }
    package <- if (isTRUE(repeated_reml)) "mmrm" else longitudinal_required_package(result$model_type)
    structure_key <- if (isTRUE(repeated_reml)) "reml" else if (result$model_type %in% c("lmm", "glmm")) {
      if (isTRUE(result$random_slope)) "slope" else "intercept"
    } else if (identical(result$model_type, "panel_fe")) "fixed" else if (identical(result$model_type, "panel_re")) "random" else NULL
    if (!is.null(structure_key)) {
      structure_args <- switch(structure_key,
        reml = list(if (identical(result$corstr, "reml_un")) "UN" else "AR(1)"),
        intercept = list(id_label), slope = list(id_label, time_label), list())
      guide_parts[[length(guide_parts) + 1L]] <- list(row = 2L, column = index + 1L,
        source = structure, key = structure_key, args = structure_args)
    }
    table[[sprintf("Model %s", index)]] <- c(
      estimand,
      structure,
      if (nzchar(package)) package_version_label(package) else ""
    )
  }
  attr(table, "longitudinal_guide_structure") <- guide_parts
  table
}

longitudinal_coef_html_table <- function(table, table_role = NULL, note_line = "") {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(div(class = "empty-message", "No coefficient table was returned."))
  }
  table_tag <- tags$table(
    class = "coefficient-table longitudinal-coefficient-table",
    style = paste0(result_table_style(font_size = 12, min_width = 0), "width:100% !important;table-layout:fixed;"),
    tags$thead(tags$tr(lapply(seq_along(names(table)), function(index) {
      tags$th(style = result_header_cell_style(first = index == 1L), names(table)[[index]])
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(table), function(col_index) {
        tags$td(style = result_body_cell_style(first = col_index == 1L, last = row_index == nrow(table)), as.character(table[[col_index]][[row_index]] %||% ""))
      }))
    }))
  )
  contract <- result_table_contract(
    table,
    role = table_role,
    intrinsic_width = result_table_intrinsic_width(table, min_width = 480)
  )
  result_table_with_notes(
    result_table_apply_contract(table_tag, contract),
    result_note_tag(note_line)
  )
}

longitudinal_result_block <- function(result, variable_table = NULL, labels = character(0), category_table = NULL) {
  assumption_table <- longitudinal_display_assumption_table(result)
  publication_table <- longitudinal_publication_estimate_table(result)
  recommendations <- as.character(result$recommendations %||% character(0))
  recommendations <- recommendations[nzchar(recommendations)]
  sensitivity <- as.character(result$sensitivity_recommendations %||% character(0))
  sensitivity <- sensitivity[nzchar(sensitivity)]
  publication_note <- if (is.data.frame(result$publication_notes) && nrow(result$publication_notes) > 0 && "Note" %in% names(result$publication_notes)) {
    notes <- as.character(result$publication_notes$Note)
    result_sci_note_text(
      abbreviations = notes[intersect(2L, seq_along(notes))],
      estimation = notes[intersect(c(1L, 3L), seq_along(notes))],
      reference = notes[intersect(4L, seq_along(notes))]
    )
  } else {
    result_sci_note_text(estimation = result$method %||% "")
  }
  detailed_table <- longitudinal_display_coef_table(result, variable_table, labels, category_table)
  main_table <- if (is.data.frame(publication_table) && nrow(publication_table) > 0L) publication_table else detailed_table
  has_publication_table <- is.data.frame(publication_table) && nrow(publication_table) > 0L
  main_note <- if (isTRUE(has_publication_table)) publication_note else result_sci_note_text(estimation = result$method %||% "")
  tagList(
    if (isTRUE(has_publication_table)) {
      div(
        `data-note-source` = "Publication table notes",
        longitudinal_table_section(
          sprintf("Publication-ready estimates: %s", longitudinal_result_title(result, variable_table, labels)),
          main_table,
          role = "main",
          table_fn = coefficient_html_table,
          note_line = main_note
        )
      )
    } else {
      longitudinal_table_section(
        longitudinal_result_title(result, variable_table, labels),
        main_table,
        role = "main",
        table_fn = coefficient_html_table,
        note_line = main_note
      )
    },
    if (isTRUE(has_publication_table)) {
      longitudinal_table_section("Coefficients (detailed)", detailed_table, role = "appendix", table_fn = longitudinal_coef_html_table)
    },
    if (nzchar(result$model_rationale %||% "")) {
      longitudinal_table_section("Model rationale",
        data.frame(Note = longitudinal_appendix_text(result$model_rationale), stringsAsFactors = FALSE), role = "appendix")
    },
    longitudinal_table_section("Data structure", result$data_structure, role = "appendix"),
    longitudinal_table_section("Analysis weights", longitudinal_display_weight_summary_table(result), role = "appendix"),
    longitudinal_table_section("Missing data", longitudinal_display_missing_table(result), role = "appendix"),
    longitudinal_table_section("Missing-data pattern", longitudinal_display_missing_pattern_table(result), role = "appendix"),
    longitudinal_table_section("Missing data by time", longitudinal_display_missing_by_time_table(result), role = "appendix"),
    longitudinal_table_section("Missing-data sensitivity results", longitudinal_display_missing_sensitivity_table(result), role = "appendix"),
    longitudinal_table_section("Model fit details", result$fit_details, role = "appendix"),
    longitudinal_table_section("Assumption checks", assumption_table, role = "appendix"),
    if (length(recommendations) > 0L) {
      longitudinal_table_section("Recommended analysis",
        data.frame(Note = vapply(recommendations, longitudinal_appendix_text, character(1)), stringsAsFactors = FALSE), role = "appendix")
    },
    if (length(sensitivity) > 0L) {
      longitudinal_table_section("Sensitivity analysis suggestions",
        data.frame(Note = vapply(sensitivity, longitudinal_appendix_text, character(1)), stringsAsFactors = FALSE), role = "appendix")
    },
    longitudinal_table_section("Sensitivity analysis results", result$sensitivity_results, role = "appendix"),
    longitudinal_table_section("Suggested manuscript text", result$manuscript_text, role = "appendix"),
    longitudinal_table_section("SCI reporting checklist", result$reporting_checklist, role = "appendix"),
    longitudinal_table_section("Software versions", result$software_versions, role = "appendix"),
    longitudinal_table_section("R package warnings", result$package_warnings, role = "appendix"),
    if (length(result$notes %||% character(0)) > 0L) {
      display_notes <- if (is.data.frame(result$package_warnings) && nrow(result$package_warnings) > 0L)
        result$notes[!startsWith(result$notes, "R package warning: ")] else result$notes
      longitudinal_table_section("Notes",
        data.frame(Note = vapply(display_notes, longitudinal_appendix_text, character(1)), stringsAsFactors = FALSE), role = "appendix")
    }
  )
}

longitudinal_results_panel <- function(results, variable_table = NULL, labels = character(0), category_table = NULL) {
  warnings <- attr(results, "warnings")
  skipped <- attr(results, "skipped")
  div(
    class = "regression-results longitudinal-results",
    if (is.list(results) && length(results) > 0) {
      longitudinal_table_section(
        "Model overview",
        longitudinal_model_overview_table(results, variable_table, labels),
        role = "appendix"
      )
    },
    lapply(results, longitudinal_result_block, variable_table = variable_table, labels = labels, category_table = category_table),
    if (is.list(results) && length(results) > 0) {
      longitudinal_table_section(
        "Model interpretation guide",
        longitudinal_assumption_review_table(results),
        role = "appendix"
      )
    },
    longitudinal_diagnostics_section(warnings, skipped, title = "Warnings / skipped models", class = "regression-result-panel longitudinal-diagnostics-panel")
  )
}

saved_longitudinal_results_html <- function(results, variable_table = NULL, labels = character(0), category_table = NULL, css_path = file.path("www", "style.css"), report_mode = FALSE) {
  content <- div(
    class = "page-shell",
    div(
      class = "app-heading",
      h1("Longitudinal / Panel Models"),
      div("Longitudinal, clustered, and panel model results.", class = "app-subtitle")
    ),
    longitudinal_results_panel(results, variable_table, labels, category_table)
  )
  saved_results_document(
    title = "Longitudinal / Panel Models",
    content = content,
    css_path = css_path,
    report_mode = report_mode
  )
}

write_longitudinal_results_html <- function(results, file, variable_table = NULL, labels = character(0), category_table = NULL) {
  write_result_html_document(saved_longitudinal_results_html(results, variable_table, labels, category_table), file, useBytes = TRUE)
  invisible(file)
}

write_longitudinal_results_pdf <- function(results, file, variable_table = NULL, labels = character(0), category_table = NULL) {
  write_pdf_from_html(saved_longitudinal_results_html(results, variable_table, labels, category_table, report_mode = TRUE), file)
}

save_longitudinal_excel_file <- function (results, file, variable_table = NULL, labels = character(0), category_table = NULL)
{
    save_screen_excel_file(saved_longitudinal_results_html(results = results, variable_table = variable_table,
        labels = labels, category_table = category_table), file)
}
