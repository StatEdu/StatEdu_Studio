# Structural fit summary result rendering.

structural_canvas_lavaan_quality_number <- function(values, direction = "single") {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (!length(values)) return("")
  value <- switch(
    direction,
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    mean = mean(values, na.rm = TRUE),
    values[[1L]]
  )
  format_decimal3(value)
}

structural_canvas_quality_full_collinearity_vif <- function(scores) {
  if (is.null(scores)) return(numeric(0))
  scores <- as.data.frame(scores, check.names = FALSE)
  numeric_columns <- names(scores)[vapply(scores, is.numeric, logical(1))]
  scores <- scores[numeric_columns]
  if (ncol(scores) < 2L) return(numeric(0))
  scores <- scores[stats::complete.cases(scores), , drop = FALSE]
  if (nrow(scores) <= ncol(scores) + 1L) return(numeric(0))
  stats::setNames(vapply(names(scores), function(outcome) {
    predictors <- setdiff(names(scores), outcome)
    y <- scores[[outcome]]
    x <- as.matrix(scores[predictors])
    design <- cbind(`(Intercept)` = 1, x)
    fit <- tryCatch(stats::lm.fit(design, y), error = function(error) NULL)
    if (is.null(fit)) return(NA_real_)
    residual <- fit$residuals
    tss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
    rss <- sum(residual^2, na.rm = TRUE)
    if (!is.finite(tss) || tss <= 0 || !is.finite(rss)) return(NA_real_)
    r2 <- max(0, min(1, 1 - rss / tss))
    if (r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1)), names(scores))
}

structural_canvas_lavaan_harman_first_factor_percent <- function(fit) {
  observed <- tryCatch(lavaan::lavNames(fit, "ov"), error = function(error) character(0))
  data <- tryCatch(as.data.frame(lavaan::lavInspect(fit, "data"), check.names = FALSE), error = function(error) data.frame())
  variables <- intersect(observed, names(data))
  variables <- variables[vapply(data[variables], is.numeric, logical(1))]
  if (length(variables) < 2L) return(NA_real_)
  values <- data[variables]
  values <- values[stats::complete.cases(values), , drop = FALSE]
  if (nrow(values) <= length(variables) + 1L) return(NA_real_)
  pca <- tryCatch(stats::prcomp(values, center = TRUE, scale. = TRUE), error = function(error) NULL)
  if (is.null(pca) || is.null(pca$sdev) || !length(pca$sdev)) return(NA_real_)
  variance <- pca$sdev^2
  100 * variance[[1L]] / sum(variance)
}

structural_canvas_lavaan_full_collinearity_vif <- function(fit) {
  scores <- tryCatch(lavaan::lavPredict(fit), error = function(error) NULL)
  structural_canvas_quality_full_collinearity_vif(scores)
}

structural_canvas_lavaan_sample_ratio <- function(fit) {
  n <- tryCatch(sum(as.integer(lavaan::lavInspect(fit, "ntotal")), na.rm = TRUE), error = function(error) NA_integer_)
  npar <- tryCatch(as.numeric(lavaan::lavInspect(fit, "npar")), error = function(error) NA_real_)
  if (!is.finite(n) || !is.finite(npar) || npar <= 0) NA_real_ else n / npar
}

structural_canvas_lavaan_quality_loadings <- function(fit) {
  standardized <- tryCatch(lavaan::standardizedSolution(fit), error = function(error) data.frame())
  observed <- tryCatch(lavaan::lavNames(fit, "ov"), error = function(error) character(0))
  loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs", "est.std"), drop = FALSE]
  loadings$est.std <- suppressWarnings(as.numeric(loadings$est.std))
  loadings
}

structural_canvas_lavaan_quality_validity <- function(fit) {
  loadings <- structural_canvas_lavaan_quality_loadings(fit)
  if (!nrow(loadings)) return(list(ave = numeric(0), cr = numeric(0)))
  observed <- tryCatch(lavaan::lavNames(fit, "ov"), error = function(error) character(0))
  standardized <- tryCatch(lavaan::standardizedSolution(fit), error = function(error) data.frame())
  theta <- matrix(0, nrow = length(observed), ncol = length(observed), dimnames = list(observed, observed))
  theta_rows <- standardized$op == "~~" & standardized$lhs %in% observed & standardized$rhs %in% observed
  for (index in which(theta_rows)) {
    lhs <- standardized$lhs[[index]]
    rhs <- standardized$rhs[[index]]
    theta[lhs, rhs] <- standardized$est.std[[index]]
    theta[rhs, lhs] <- standardized$est.std[[index]]
  }
  latent_names <- unique(loadings$lhs)
  ave <- stats::setNames(vapply(latent_names, function(name) {
    lambda <- loadings$est.std[loadings$lhs == name]
    mean(lambda^2, na.rm = TRUE)
  }, numeric(1)), latent_names)
  cr <- stats::setNames(vapply(latent_names, function(name) {
    lambda <- loadings$est.std[loadings$lhs == name]
    indicators <- loadings$rhs[loadings$lhs == name]
    common <- sum(lambda, na.rm = TRUE)^2
    residual <- sum(theta[indicators, indicators, drop = FALSE], na.rm = TRUE)
    denominator <- common + residual
    if (is.finite(denominator) && denominator > 0) common / denominator else NA_real_
  }, numeric(1)), latent_names)
  list(ave = ave, cr = cr)
}

structural_canvas_lavaan_quality_max_latent_correlation <- function(fit) {
  correlations <- tryCatch(as.matrix(lavaan::lavInspect(fit, "cor.lv")), error = function(error) matrix(numeric(0), 0L, 0L))
  if (!length(correlations) || nrow(correlations) < 2L || ncol(correlations) < 2L) return(NA_real_)
  correlations[upper.tri(correlations, diag = TRUE)] <- NA_real_
  values <- suppressWarnings(abs(as.numeric(correlations)))
  if (!any(is.finite(values))) NA_real_ else max(values, na.rm = TRUE)
}

structural_canvas_lavaan_quality_structural <- function(fit) {
  standardized <- tryCatch(lavaan::standardizedSolution(fit), error = function(error) data.frame())
  structural <- standardized[standardized$op == "~", , drop = FALSE]
  beta <- suppressWarnings(abs(as.numeric(structural$est.std %||% numeric(0))))
  r2_values <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(error) numeric(0))
  outcomes <- unique(as.character(structural$lhs %||% character(0)))
  r2 <- suppressWarnings(as.numeric(r2_values[outcomes]))
  list(path_count = nrow(structural), max_beta = beta, r2 = r2)
}

structural_canvas_lavaan_quality_status <- function(item, value, analysis_type = "cfa") {
  numeric_value <- suppressWarnings(as.numeric(value))
  unavailable <- !nzchar(as.character(value %||% "")) || identical(value, "not recorded")
  if (unavailable) return("Not assessed")
  if (identical(item, "Converged")) return(if (identical(value, "TRUE")) "OK" else "Review")
  if (identical(item, "Admissible solution")) return(if (identical(value, "TRUE")) "OK" else "Review")
  if (identical(item, "Model df")) return(if (is.finite(numeric_value) && numeric_value > 0) "OK" else "Review")
  if (identical(item, "Chi-square/df")) return(if (is.finite(numeric_value) && numeric_value <= 5) "Reference only" else "Review")
  if (identical(item, "Fit statistic source")) return("OK")
  if (identical(item, "N/free parameter ratio")) return(if (is.finite(numeric_value)) "Reference only" else "Not assessed")
  if (item %in% c("Harman first-factor %", "Max full collinearity VIF")) return(if (is.finite(numeric_value)) "Screen only" else "Not assessed")
  if (identical(item, "CFI")) return(if (is.finite(numeric_value) && numeric_value >= .90) "Reference only" else "Review")
  if (identical(item, "TLI")) return(if (is.finite(numeric_value) && numeric_value >= .90) "Reference only" else "Review")
  if (identical(item, "RMSEA")) return(if (is.finite(numeric_value) && numeric_value <= .08) "Reference only" else "Review")
  if (identical(item, "SRMR")) return(if (is.finite(numeric_value) && numeric_value <= .10) "Reference only" else "Review")
  if (identical(item, "Min standardized loading")) return(if (is.finite(numeric_value) && numeric_value >= .40) "Reference only" else "Review")
  if (identical(item, "Min CR")) return(if (is.finite(numeric_value) && numeric_value >= .70) "Reference only" else "Review")
  if (identical(item, "Min AVE")) return(if (is.finite(numeric_value) && numeric_value >= .50) "Reference only" else "Review")
  if (identical(item, "Max latent correlation")) return(if (is.finite(numeric_value) && numeric_value < .85) "Reference only" else "Review")
  if (identical(item, "Structural path count")) {
    if (!analysis_type %in% c("sem", "cbsem")) return("Not assessed")
    return(if (is.finite(numeric_value) && numeric_value > 0) "OK" else "Review")
  }
  if (identical(item, "Max structural beta")) {
    if (!analysis_type %in% c("sem", "cbsem")) return("Not assessed")
    return(if (is.finite(numeric_value) && numeric_value < 1) "OK" else "Review")
  }
  if (identical(item, "Min endogenous R2")) {
    if (!analysis_type %in% c("sem", "cbsem")) return("Not assessed")
    return(if (is.finite(numeric_value)) "Descriptive only" else "Not assessed")
  }
  if (identical(item, "Model status")) return(if (identical(value, "Original/prespecified model")) "OK" else "Review")
  "Not assessed"
}

structural_canvas_lavaan_quality_rows <- function(bundle, analysis_type = "cfa") {
  if (is.null(bundle) || is.null(bundle$fit) || !inherits(bundle$fit, "lavaan")) {
    return(data.frame(Item = character(0), Value = character(0), Status = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  fit <- bundle$fit
  selection <- structural_canvas_fit_measures(fit, bundle$estimator %||% "ML", bundle$rmsea_ci %||% .90)
  values <- selection$values
  source_label <- paste(unname(selection$keys), collapse = ", ")
  sample_ratio <- structural_canvas_lavaan_sample_ratio(fit)
  harman_percent <- structural_canvas_lavaan_harman_first_factor_percent(fit)
  full_collinearity_vif <- structural_canvas_lavaan_full_collinearity_vif(fit)
  loadings <- structural_canvas_lavaan_quality_loadings(fit)
  validity <- structural_canvas_lavaan_quality_validity(fit)
  structural <- structural_canvas_lavaan_quality_structural(fit)
  converged <- bundle$converged %||% bundle$diagnostics$converged %||% tryCatch(lavaan::lavInspect(fit, "converged"), error = function(error) NA)
  admissible <- bundle$admissible %||% bundle$diagnostics$admissible %||% tryCatch(structural_canvas_fit_admissibility(fit)$admissible, error = function(error) NA)
  modified <- if (isTRUE(bundle$modified_model) || isTRUE(bundle$modified_from_baseline) || length(bundle$mi_history %||% list())) "Exploratory modified model" else "Original/prespecified model"
  items <- c(
    "Converged",
    "Admissible solution",
    "Model df",
    "Chi-square/df",
    "Fit statistic source",
    "N/free parameter ratio",
    "Harman first-factor %",
    "Max full collinearity VIF",
    "CFI",
    "TLI",
    "RMSEA",
    "SRMR",
    "Min standardized loading",
    "Min CR",
    "Min AVE",
    "Max latent correlation",
    "Structural path count",
    "Max structural beta",
    "Min endogenous R2",
    "Model status"
  )
  displayed_values <- c(
    if (is.na(converged)) "not recorded" else as.character(isTRUE(converged)),
    if (is.na(admissible)) "not recorded" else as.character(isTRUE(admissible)),
    structural_canvas_lavaan_quality_number(values[[2L]]),
    structural_canvas_lavaan_quality_number(values[[4L]]),
    source_label,
    structural_canvas_lavaan_quality_number(sample_ratio),
    structural_canvas_lavaan_quality_number(harman_percent),
    structural_canvas_lavaan_quality_number(full_collinearity_vif, "max"),
    structural_canvas_lavaan_quality_number(values[[5L]]),
    structural_canvas_lavaan_quality_number(values[[6L]]),
    structural_canvas_lavaan_quality_number(values[[8L]]),
    structural_canvas_lavaan_quality_number(values[[7L]]),
    structural_canvas_lavaan_quality_number(abs(loadings$est.std), "min"),
    structural_canvas_lavaan_quality_number(validity$cr, "min"),
    structural_canvas_lavaan_quality_number(validity$ave, "min"),
    structural_canvas_lavaan_quality_number(structural_canvas_lavaan_quality_max_latent_correlation(fit)),
    as.character(structural$path_count),
    structural_canvas_lavaan_quality_number(structural$max_beta, "max"),
    structural_canvas_lavaan_quality_number(structural$r2, "min"),
    modified
  )
  data.frame(
    Item = items,
    Value = displayed_values,
    Status = mapply(structural_canvas_lavaan_quality_status, items, displayed_values, MoreArgs = list(analysis_type = analysis_type), USE.NAMES = FALSE),
    Guidance = c(
      "Must be TRUE before interpreting estimates.",
      "Must be TRUE before reporting fit, reliability, validity, or structural paths as final.",
      "df = 0 indicates a saturated model; approximate fit is not substantively diagnostic.",
      "Descriptive ratio only; conventional ranges are not a model-acceptance test. Large values flag possible misfit for investigation.",
      "Records whether lavaan robust/scaled or conventional fit statistics were selected.",
      "Descriptive sample-size ratio only; no universal N-to-parameter cutoff establishes adequate power or stable estimation.",
      "Exploratory Harman screen only; values above 50% may flag concentration, while lower values do not rule out common-method bias.",
      "Exploratory full-collinearity screen only; values above 3.3 are nonspecific and lower values do not rule out common-method bias.",
      "Common .90/.95 values are descriptive references, not universal acceptance rules; interpret with residuals, model complexity, and theory.",
      "Common .90/.95 values are descriptive references, not universal acceptance rules; interpret with residuals, model complexity, and theory.",
      "Common .08/.06 values are descriptive references; interpret the estimate and CI with df, sample size, residuals, and theory.",
      "Common .10/.08 values are descriptive references, not universal acceptance rules; inspect the residual pattern and substantive size.",
      "Loading values are descriptive evidence, not automatic item-retention rules. Review low values with content validity, residuals, cross-loadings, uncertainty, and prespecified theory.",
      "The .70 value is a descriptive reliability reference, not a universal scale-acceptance rule; report the estimate, formula, and uncertainty.",
      "The .50 value is a descriptive convergent-validity reference, not an automatic construct-acceptance or item-deletion rule.",
      "Latent-correlation references do not establish discriminant validity; interpret with HTMT intervals, cross-loadings, local fit, theory, and competing measurement models.",
      "Report whether the model includes structural regressions.",
      "Inspect coefficients near or above |1| for inadmissibility or suppression.",
      "Report R2 descriptively for each endogenous construct; no universal value establishes adequate explanation or causal importance.",
      "MI-based modifications are exploratory unless validated in independent data."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

structural_canvas_lavaan_quality_status_summary <- function(rows) {
  if (!nrow(rows) || !"Status" %in% names(rows)) return("Quality status: not assessed.")
  counts <- table(factor(rows$Status, levels = c("OK", "Review", "Reference only", "Screen only", "Not assessed")))
  paste0(
    "Quality status: OK=", counts[["OK"]],
    "; Review=", counts[["Review"]],
    "; Reference only=", counts[["Reference only"]],
    "; Screen only=", counts[["Screen only"]],
    "; Not assessed=", counts[["Not assessed"]],
    "."
  )
}

structural_canvas_lavaan_quality_priority <- function(items) {
  critical <- c("Converged", "Admissible solution")
  major <- c("Model df", "Chi-square/df", "N/free parameter ratio", "Harman first-factor %", "Max full collinearity VIF", "CFI", "TLI", "RMSEA", "SRMR", "Min standardized loading", "Min CR", "Min AVE", "Max latent correlation", "Max structural beta")
  ifelse(items %in% critical, "Critical", ifelse(items %in% major, "Major", "Advisory"))
}

structural_canvas_lavaan_quality_action <- function(priority) {
  ifelse(
    priority == "Critical", "Resolve before reporting",
    ifelse(priority == "Major", "Resolve or justify", "Document limitation")
  )
}

structural_canvas_lavaan_quality_review_rows <- function(rows) {
  if (!nrow(rows) || !"Status" %in% names(rows)) {
    return(data.frame(Priority = character(0), Action = character(0), Item = character(0), Value = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  review <- rows[rows$Status == "Review", c("Item", "Value", "Guidance"), drop = FALSE]
  review$Priority <- structural_canvas_lavaan_quality_priority(review$Item)
  review$Action <- structural_canvas_lavaan_quality_action(review$Priority)
  priority_order <- c(Critical = 1L, Major = 2L, Advisory = 3L)
  review <- review[order(priority_order[review$Priority], review$Item), c("Priority", "Action", "Item", "Value", "Guidance"), drop = FALSE]
  rownames(review) <- NULL
  review
}

structural_canvas_lavaan_quality_reporting_readiness <- function(rows) {
  review <- structural_canvas_lavaan_quality_review_rows(rows)
  if (!nrow(review)) return("Reporting readiness: no quality-review blockers detected.")
  critical_n <- sum(review$Priority == "Critical")
  major_n <- sum(review$Priority == "Major")
  if (critical_n > 0L) {
    return(paste0("Reporting readiness: blocked until ", critical_n, " critical review item(s) are resolved."))
  }
  if (major_n > 0L) {
    return(paste0("Reporting readiness: requires resolution or explicit justification for ", major_n, " major review item(s)."))
  }
  "Reporting readiness: advisory review item(s) should be documented."
}

structural_canvas_quality_display_value <- function(value) {
  value <- as.character(value %||% "")
  value_map <- c(
    "TRUE" = "예",
    "FALSE" = "아니오",
    "not recorded" = "기록 없음",
    "Not executed" = "실행 안 함",
    "Original/prespecified model" = "연구모형",
    "Exploratory modified model" = "탐색적 수정모형",
    "mean_replacement" = "평균 대체"
  )
  if (value %in% names(value_map)) return(unname(value_map[value]))
  if (grepl("^[0-9]+/[0-9]+ indicator metrics favor PLS over LM$", value)) {
    return(sub(" indicator metrics favor PLS over LM", "개 지표 오차에서 PLS가 LM보다 낮음", value))
  }
  value
}

structural_canvas_quality_display_guidance <- function(item, guidance) {
  guidance_map <- c(
    "Converged" = "추정이 정상 수렴해야 계수를 해석할 수 있습니다.",
    "Admissible solution" = "적합도, 신뢰도, 타당도, 구조경로를 최종 보고하기 전 반드시 예여야 합니다.",
    "Model df" = "df = 0이면 포화모형이므로 근사 적합도 해석이 제한됩니다.",
    "Chi-square/df" = "카이제곱을 자유도로 나눈 값입니다. 3 또는 5 초과는 근거를 제시하십시오.",
    "Fit statistic source" = "lavaan의 일반/robust/scaled 적합도 중 어떤 지표를 선택했는지 기록합니다.",
    "N/free parameter ratio" = "표본 크기 점검입니다. 5 미만이면 주의 또는 근거가 필요합니다.",
    "Harman first-factor %" = "탐색적 점검값일 뿐입니다. 50% 초과는 집중 가능성을 표시하지만 낮은 값도 동일방법편향의 부재를 입증하지 않습니다.",
    "Max full collinearity VIF" = "비특이적인 탐색적 점검값입니다. 3.3 초과는 검토 신호일 수 있지만 낮은 값도 동일방법편향의 부재를 입증하지 않습니다.",
    "CFI" = ".90 미만은 검토가 필요하며 .95는 흔히 쓰는 기술적 목표입니다.",
    "TLI" = ".90 미만은 검토가 필요하며 .95는 흔히 쓰는 기술적 목표입니다.",
    "RMSEA" = ".08 초과는 검토가 필요하며 .06은 흔히 쓰는 기술적 목표입니다.",
    "SRMR" = ".10 초과는 검토가 필요하며 .08은 흔히 쓰는 기술적 목표입니다.",
    "Min standardized loading" = "적재량은 문항 유지의 자동 규칙이 아닙니다. 낮은 값은 내용타당도, 잔차, 교차적재, 불확실성과 사전 이론을 함께 검토하십시오.",
    "Min CR" = ".70은 기술적 신뢰도 참고값이며 보편적 척도 합격선이 아닙니다. 추정값, 계산식과 불확실성을 보고하십시오.",
    "Min AVE" = ".50은 기술적 수렴타당도 참고값이며 구성개념 채택이나 문항 삭제의 자동 기준이 아닙니다.",
    "Max latent correlation" = "잠재상관 기준만으로 판별타당도가 확립되지는 않습니다. HTMT 구간, 교차적재, 국소적합, 이론과 경쟁 측정모형을 함께 검토하십시오.",
    "Structural path count" = "구조 회귀경로 포함 여부를 보고하십시오.",
    "Max structural beta" = "|1|에 가깝거나 넘는 계수는 부적절해 또는 억제효과 가능성을 점검하십시오.",
    "Min endogenous R2" = "내생 구성개념별 R²를 기술적으로 보고하십시오. 보편적인 값 하나가 충분한 설명력이나 인과적 중요성을 확립하지 않습니다.",
    "Model status" = "MI 기반 수정은 독립 자료에서 검증하지 않는 한 탐색적 결과입니다.",
    "PLS algorithm iterations" = "반복 횟수가 비정상적으로 크면 알고리즘 수렴을 점검하십시오.",
    "Final weight difference" = "값이 작을수록 외부가중치 수렴이 안정적입니다.",
    "Missing-data method" = "PLS는 lavaan의 FIML/pairwise 옵션을 사용하지 않으므로 결측 처리 방식을 보고하십시오.",
    "Approx PLS SRMR" = "로컬에서 재구성한 반영형 측정모형 SRMR입니다. 수용·기각 절단값 없이 기술적으로만 보고하십시오.",
    "Approx d_G" = "로컬에서 재구성한 측지 불일치입니다. 보편 절단값이 없으므로 기술적으로만 보고하십시오.",
    "Approx d_ULS" = "로컬에서 재구성한 제곱 유클리드 불일치입니다. 보편 절단값이 없으므로 기술적으로만 보고하십시오.",
    "Approx NFI" = "독립 상관 기준선으로 로컬에서 재구성한 NFI입니다. 수용·기각 절단값 없이 기술적으로만 보고하십시오.",
    "10-times rule margin" = "역사적으로 부정확한 10배 규칙은 표본크기 정당화에 사용할 수 없습니다. 사전 검정력 또는 모형별 시뮬레이션을 우선하십시오.",
    "Min outer loading" = "외부적재량은 자동 삭제 기준이 아닙니다. 낮은 값은 내용타당도, 교차적재, 불확실성과 사전 이론을 함께 검토하십시오.",
    "Min rhoC" = ".70은 기술적 신뢰도 참고값이며 보편적 척도 합격선이 아닙니다.",
    "Max HTMT" = "HTMT 기준 미만은 판별타당도 확정 판정이 아닙니다. 신뢰구간, 잠재상관, 교차적재, 이론과 경쟁 측정모형을 함께 검토하십시오.",
    "Max item VIF" = "VIF 절단값은 기술적 참고입니다. 높은 값은 중복 지표나 형성가중치 불안정성을 검토하고, 낮은 값도 측정 품질을 확립하지 않습니다.",
    "Max inner VIF" = "VIF 절단값은 기술적 참고입니다. 예측변수 간 이론적 중복, 계수 부호·크기 변화, 억제효과와 bootstrap 불안정성을 함께 검토하십시오.",
    "PLSpredict summary" = "PLS와 LM의 반복 표본외 오차 차이를 기술적으로 비교합니다. 일부 지표의 PLS 우세만으로 예측타당도 통과 판정을 내리지 마십시오.",
    "Max f2" = "f²의 .02/.15/.35 구간은 기술적 표지이며 분야 맥락, 불확실성과 실질적 중요성을 대체하지 않습니다."
  )
  if (item %in% names(guidance_map)) unname(guidance_map[item]) else guidance
}

structural_canvas_quality_display_rows <- function(rows, ko = FALSE) {
  if (!isTRUE(ko) || !nrow(rows)) return(rows)
  original_items <- if ("Item" %in% names(rows)) as.character(rows$Item) else rep("", nrow(rows))
  item_map <- c(
    "Converged" = "수렴",
    "Admissible solution" = "허용 가능한 해",
    "Model df" = "모형 자유도",
    "Chi-square/df" = "χ²/df",
    "Fit statistic source" = "적합도 지표 출처",
    "N/free parameter ratio" = "N/자유모수 비율",
    "Harman first-factor %" = "Harman 1요인 %",
    "Max full collinearity VIF" = "최대 full collinearity VIF",
    "Min standardized loading" = "최소 표준화 적재량",
    "Min CR" = "최소 CR",
    "Min AVE" = "최소 AVE",
    "Max latent correlation" = "최대 잠재상관",
    "Structural path count" = "구조경로 수",
    "Max structural beta" = "최대 구조 β",
    "Min endogenous R2" = "최소 내생 R²",
    "Model status" = "모형 상태",
    "PLS algorithm iterations" = "PLS 알고리즘 반복 수",
    "Final weight difference" = "최종 가중치 차이",
    "Missing-data method" = "결측 처리 방식",
    "Approx PLS SRMR" = "근사 PLS SRMR",
    "Approx d_ULS" = "근사 d_ULS",
    "Approx NFI" = "근사 NFI",
    "10-times rule margin" = "10배 규칙 여유",
    "Min outer loading" = "최소 외부적재량",
    "Min rhoC" = "최소 rhoC",
    "Max HTMT" = "최대 HTMT",
    "Max item VIF" = "최대 지표 VIF",
    "Max inner VIF" = "최대 inner VIF",
    "Max f2" = "최대 f²",
    "PLSpredict summary" = "PLSpredict 요약"
  )
  status_map <- c("OK" = "양호", "Review" = "검토", "Reference only" = "기준 참고", "Descriptive only" = "기술적 참고", "Screen only" = "탐색 점검", "Not assessed" = "미평가")
  priority_map <- c("Critical" = "치명", "Major" = "주요", "Advisory" = "참고")
  action_map <- c("Resolve before reporting" = "보고 전 해결", "Resolve or justify" = "해결 또는 근거 제시", "Document limitation" = "한계로 명시")
  if ("Item" %in% names(rows)) rows$Item <- ifelse(rows$Item %in% names(item_map), unname(item_map[rows$Item]), rows$Item)
  if ("Value" %in% names(rows)) rows$Value <- vapply(rows$Value, structural_canvas_quality_display_value, character(1))
  if ("Status" %in% names(rows)) rows$Status <- ifelse(rows$Status %in% names(status_map), unname(status_map[rows$Status]), rows$Status)
  if ("Priority" %in% names(rows)) rows$Priority <- ifelse(rows$Priority %in% names(priority_map), unname(priority_map[rows$Priority]), rows$Priority)
  if ("Action" %in% names(rows)) rows$Action <- ifelse(rows$Action %in% names(action_map), unname(action_map[rows$Action]), rows$Action)
  if ("Guidance" %in% names(rows)) rows$Guidance <- mapply(structural_canvas_quality_display_guidance, original_items, rows$Guidance, USE.NAMES = FALSE)
  name_map <- c(Priority = "우선순위", Action = "조치", Item = "항목", Value = "값", Status = "상태", Guidance = "안내")
  names(rows) <- ifelse(names(rows) %in% names(name_map), unname(name_map[names(rows)]), names(rows))
  rows
}

structural_canvas_quality_status_summary_display <- function(rows, ko = FALSE) {
  if (!isTRUE(ko)) {
    if ("PLS algorithm iterations" %in% rows$Item) return(structural_canvas_pls_quality_status_summary(rows))
    return(structural_canvas_lavaan_quality_status_summary(rows))
  }
  if (!nrow(rows) || !"Status" %in% names(rows)) return("품질 상태: 미평가.")
  status_levels <- c("OK", "Review", "Reference only", "Descriptive only", "Screen only", "Not assessed")
  counts <- table(factor(rows$Status, levels = status_levels))
  paste0("품질 상태: 양호=", counts[["OK"]], "; 검토=", counts[["Review"]], "; 기준 참고=", counts[["Reference only"]], "; 기술적 참고=", counts[["Descriptive only"]], "; 탐색 점검=", counts[["Screen only"]], "; 미평가=", counts[["Not assessed"]], ".")
}

structural_canvas_quality_reporting_readiness_display <- function(review_rows, rows, ko = FALSE) {
  if (!isTRUE(ko)) {
    if ("PLS algorithm iterations" %in% rows$Item) return(structural_canvas_pls_quality_reporting_readiness(rows))
    return(structural_canvas_lavaan_quality_reporting_readiness(rows))
  }
  if (!nrow(review_rows)) return("보고 준비도: 품질 검토 차단 항목이 없습니다.")
  critical_n <- sum(review_rows$Priority == "Critical")
  major_n <- sum(review_rows$Priority == "Major")
  if (critical_n > 0L) return(paste0("보고 준비도: 치명 검토 항목 ", critical_n, "개를 해결해야 합니다."))
  if (major_n > 0L) return(paste0("보고 준비도: 주요 검토 항목 ", major_n, "개는 해결하거나 근거를 명시해야 합니다."))
  "보고 준비도: 참고 검토 항목은 한계로 명시하십시오."
}

structural_canvas_lavaan_quality_result_ui <- function(bundle, analysis_type = "cfa", language = statedu_initial_language()) {
  rows <- structural_canvas_lavaan_quality_rows(bundle, analysis_type)
  if (!nrow(rows)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  review_rows <- structural_canvas_lavaan_quality_review_rows(rows)
  summary <- structural_canvas_quality_status_summary_display(rows, ko)
  readiness <- structural_canvas_quality_reporting_readiness_display(review_rows, rows, ko)
  display_rows <- structural_canvas_quality_display_rows(rows, ko)
  display_review_rows <- structural_canvas_quality_display_rows(review_rows, ko)
  div(
    class = "result-section regression-result-panel structural-lavaan-quality-result",
    h4(if (ko) "SEM 품질 체크리스트" else "SEM quality checklist"),
    tags$p(class = "structural-result-note structural-quality-status-summary", summary),
    tags$p(class = "structural-result-note structural-quality-reporting-readiness", readiness),
    tags$h5(if (ko) "검토 필요 항목" else "Review focus"),
    if (nrow(display_review_rows)) structural_canvas_basic_html_table(display_review_rows) else tags$p(class = "structural-result-note", if (ko) "품질 체크리스트에 검토 항목이 없습니다." else "No Review rows in the quality checklist."),
    structural_canvas_basic_html_table(display_rows),
    if (any(rows$Status == "Review")) tags$p(
      class = "structural-result-note",
      if (ko) "검토로 표시된 행은 확인적 보고 전 해결하거나 명시적으로 근거를 제시해야 합니다." else "Rows marked Review should be resolved or explicitly justified before confirmatory reporting."
    ),
    tags$p(
      class = "structural-result-note",
      if (ko) {
        "이 체크리스트는 lavaan SEM/CFA의 수렴, 해의 허용성, 표본 적절성, 공통방법편향 점검, χ²/df, robust/scaled 적합도 출처, 전역 적합도, 측정 품질, 판별타당도 위험, 구조모형 설명력 조건을 요약합니다."
      } else {
        "This checklist summarizes lavaan SEM/CFA convergence, admissibility, sample adequacy, common-method-bias screens, chi-square/df, robust/scaled fit source, global fit, measurement quality, discriminant-validity risk, and structural explanatory-power conditions for reporting and review."
      }
    )
  )
}

structural_canvas_additional_fit_indices_table <- function(fits, labels, selections) {
  rows <- lapply(seq_along(selections), function(index) {
    selection <- selections[[index]]
    measures <- selection$measures
    selected_chisq <- as.character(selection$keys[["chisq"]] %||% "")
    displayed_keys <- unique(c(
      unname(selection$keys),
      "df", "srmr",
      if (identical(selected_chisq, "chisq.scaled")) "pvalue.scaled" else "pvalue"
    ))
    values <- measures[setdiff(names(measures), displayed_keys)]
    values <- values[is.finite(values)]
    if (!length(values)) return(NULL)
    data.frame(
      Model = labels[[index]],
      Metric = names(values),
      Value = vapply(values, format_decimal3, character(1)),
      check.names = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

structural_canvas_additional_fit_metric_family <- function(metric) {
  metric <- tolower(metric %||% "")
  if (grepl("^baseline\\.", metric)) return("baseline")
  if (metric %in% c("npar", "fmin", "ntotal") || grepl("^(chisq|df|pvalue)(\\.|$)|scaling", metric)) {
    return("model")
  }
  if (grepl("^(cfi|tli|nnfi|rfi|nfi|pnfi|ifi|rni)(\\.|$)", metric)) return("incremental")
  if (grepl("^rmsea(\\.|$)", metric)) return("rmsea")
  if (grepl("^(srmr|crmr|rmr|wrmr|gfi|agfi|pgfi|mfi)(\\.|_|$)", metric)) return("residual")
  if (grepl("^(logl|unrestricted\\.logl|aic|bic|bic2|ecvi)(\\.|$)", metric)) return("likelihood")
  "other"
}

structural_canvas_additional_fit_family_label <- function(family, ko = FALSE) {
  labels <- if (ko) {
    c(
      model = "모형 기본 통계",
      baseline = "기저모형 통계",
      incremental = "증분 적합도",
      rmsea = "RMSEA 계열",
      residual = "잔차/절대 적합도",
      likelihood = "우도/정보기준",
      other = "기타 지표"
    )
  } else {
    c(
      model = "Model statistics",
      baseline = "Baseline model",
      incremental = "Incremental fit",
      rmsea = "RMSEA family",
      residual = "Residual and absolute fit",
      likelihood = "Likelihood and information",
      other = "Other indices"
    )
  }
  if (family %in% names(labels)) {
    return(unname(labels[[family]]))
  }
  family
}

structural_canvas_additional_fit_metric_label <- function(metric) {
  metric <- as.character(metric %||% "")
  labels <- c(
    npar = "npar",
    fmin = "fmin",
    chisq = "χ²",
    pvalue = "p",
    df.scaled = "scaled df",
    chisq.scaling.factor = "χ² scale",
    baseline.chisq = "base χ²",
    baseline.df = "base df",
    baseline.pvalue = "base p",
    baseline.chisq.scaled = "base scaled χ²",
    baseline.df.scaled = "base scaled df",
    baseline.pvalue.scaled = "base scaled p",
    unrestricted.logl = "unrestricted logL",
    logl = "logL",
    bic2 = "adj BIC",
    rmsea.ci.level = "CI level",
    rmsea.pvalue = "RMSEA p",
    rmsea.close.h0 = "close H0",
    rmsea.notclose.h0 = "not-close H0",
    rmsea.robust = "robust RMSEA",
    rmsea.ci.lower = "CI lower",
    rmsea.ci.upper = "CI upper",
    rmsea.ci.lower.robust = "robust CI lower",
    rmsea.ci.upper.robust = "robust CI upper",
    rmsea.pvalue.robust = "robust p",
    rmsea.notclose.pvalue.robust = "robust not-close p",
    srmr_bentler = "SRMR Bentler",
    srmr_bentler_nomean = "SRMR Bentler no mean",
    crmr_nomean = "CRMR no mean",
    srmr_mplus = "SRMR Mplus",
    srmr_mplus_nomean = "SRMR Mplus no mean"
  )
  if (metric %in% names(labels)) {
    return(unname(labels[[metric]]))
  }
  toupper(gsub(".", " ", metric, fixed = TRUE))
}

structural_canvas_additional_fit_metric_order <- function(metrics, family) {
  metrics <- as.character(metrics %||% character(0))
  if (!length(metrics)) return(metrics)
  if (identical(family, "residual")) {
    priority <- c("gfi", "agfi", "pgfi", "mfi")
    return(c(intersect(priority, metrics), setdiff(metrics, priority)))
  }
  metrics
}

structural_canvas_additional_fit_indices_wide_tables <- function(additional_fit) {
  if (!nrow(additional_fit)) return(list())
  order <- c("model", "baseline", "incremental", "rmsea", "residual", "likelihood", "other")
  additional_fit$Family <- vapply(additional_fit$Metric, structural_canvas_additional_fit_metric_family, character(1))
  families <- order[order %in% unique(additional_fit$Family)]
  result <- lapply(families, function(family) {
    subset <- additional_fit[additional_fit$Family == family, , drop = FALSE]
    models <- unique(subset$Model)
    metrics <- structural_canvas_additional_fit_metric_order(unique(subset$Metric), family)
    table <- data.frame(Model = models, check.names = FALSE)
    for (metric in metrics) {
      table[[structural_canvas_additional_fit_metric_label(metric)]] <- vapply(models, function(model) {
        match <- subset$Model == model & subset$Metric == metric
        if (!any(match)) return("")
        subset$Value[which(match)[[1L]]]
      }, character(1))
    }
    table
  })
  names(result) <- families
  result
}

structural_canvas_additional_fit_indices_ui <- function(additional_fit, ko = FALSE) {
  tables <- structural_canvas_additional_fit_indices_wide_tables(additional_fit)
  if (!length(tables)) return(NULL)
  tagList(lapply(names(tables), function(family) {
    tags$div(
      class = "structural-additional-fit-family",
      tags$h6(structural_canvas_additional_fit_family_label(family, ko)),
      tags$div(
        class = "table-responsive structural-landscape-table-wrap",
        structural_canvas_basic_html_table(
          tables[[family]],
          class = "table table-striped table-bordered structural-landscape-table structural-additional-fit-indices-table"
        )
      )
    )
  }))
}

structural_canvas_fit_guidance_result_ui <- function(bundle, language) {
  ko <- identical(normalize_app_language(language), "ko")
  comparison_fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, bundle$fit) else list(bundle$fit)
  selections <- structural_canvas_common_fit_measures(comparison_fits, bundle$estimator %||% "ML", bundle$rmsea_ci %||% .90)
  labels <- if (length(selections) > 1L) c(if (ko) "연구모형" else "Research model", if (ko) "탐색적 수정모형" else bundle$comparison_label %||% "Modified model") else if (ko) "연구모형" else "Research model"
  additional_fit <- structural_canvas_additional_fit_indices_table(comparison_fits, labels, selections)
  tables <- lapply(seq_along(selections), function(index) {
    guidance <- structural_canvas_fit_guidance(selections[[index]]$values)
    guidance$Model <- labels[[index]]
    guidance[, c("Model", "Metric", "Value", "Guidance", "Reference"), drop = FALSE]
  })
  table <- do.call(rbind, tables)
  table$Value <- vapply(table$Value, format_decimal3, character(1))
  severity <- c("Reference only" = 1L, "Review" = 2L, "Not assessed" = 3L)
  summaries <- vapply(split(table$Guidance, table$Model), function(values) {
    if (all(values == "Not assessed")) return("Not assessed")
    score <- max(vapply(values[values != "Not assessed"], function(value) severity[[value]], integer(1)))
    names(severity)[match(score, severity)]
  }, character(1))
  div(class = "structural-fit-guidance-result",
    tags$h5(if (ko) "표 2 가이드: 기준 기반 적합도 안내" else "Guide for Table 2: Reference-based fit guidance"),
    tags$div(class = "table-responsive structural-landscape-table-wrap", tags$table(class = "table table-striped table-bordered structural-result-table structural-landscape-table structural-fit-guidance-table",
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
    )),
    tags$p(paste(vapply(names(summaries), function(name) paste0(name, ": ", summaries[[name]]), character(1)), collapse = " | ")),
    tags$p(class = "structural-result-note", if (ko) "기준 참고/검토 표시는 흔히 쓰는 근사 범위와의 위치를 설명할 뿐 모형의 채택·기각 판정이 아닙니다. 모형 식별, 잔차 진단, 모수 타당성, 이론, 표본 특성, 대안 모형 비교를 함께 검토하십시오." else "Reference-only/review labels locate estimates relative to common approximate ranges; they do not accept or reject the model. Also inspect identification, residuals, parameter plausibility, theory, sample characteristics, and plausible alternative models."),
    tags$p(class = "structural-result-note", if (ko) "흔한 참고점: CFI/TLI .90/.95, RMSEA .08/.06, SRMR .10/.08. 이 값은 맥락 의존적 참고점이며 보편적 절단값이 아닙니다." else "Common reference points: CFI/TLI .90/.95, RMSEA .08/.06, and SRMR .10/.08. These are context-dependent reference points, not universal cutoffs."),
    if (any(table$Guidance == "Not assessed")) tags$p(class = "structural-result-note", if (ko) "포화모형(df = 0)이거나 적합도 지수가 없으면 적합도 안내를 평가하지 않습니다." else "Fit guidance is not assessed for saturated models (df = 0) or unavailable fit indices."),
    if (nrow(additional_fit)) tagList(
      tags$h5(if (ko) "표 2 가이드: 표 2에 직접 표시하지 않은 추가 적합도 통계량" else "Guide for Table 2: Additional fit indices not shown in Table 2"),
      structural_canvas_additional_fit_indices_ui(additional_fit, ko)
    )
  )

}

structural_canvas_rmsea_tests_result_ui <- function(bundle, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  table <- structural_canvas_rmsea_hypothesis_tests(bundle)
  display <- table
  for (column in c("RMSEA", "Close-fit H0", "Not-close H0")) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
  for (column in c("Close-fit p", "Not-close p")) display[[column]] <- vapply(display[[column]], format_p, character(1))
  tagList(
    tags$h5(if (ko) "표 2 가이드: RMSEA 가설검정" else "Guide for Table 2: RMSEA hypothesis tests"),
    tags$div(class = "table-responsive structural-landscape-table-wrap", tags$table(class = "table table-striped table-bordered structural-result-table structural-landscape-table structural-rmsea-tests-table",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) "Close-fit 검정의 귀무가설은 모집단 RMSEA <= .05입니다. 작은 p값은 close fit을 기각합니다. Not-close 검정의 귀무가설은 모집단 RMSEA >= .08입니다. 작은 p값은 poor approximate fit을 기각합니다. 두 검정은 RMSEA 추정값 및 신뢰구간과 함께 해석하십시오." else "Close-fit tests H0: population RMSEA <= .05; a small p value rejects close fit. Not-close tests H0: population RMSEA >= .08; a small p value rejects poor approximate fit. Interpret both with the RMSEA estimate and confidence interval."),
    tags$p(class = "structural-result-note", if (ko) "가능한 경우 보고된 RMSEA와 맞는 robust 또는 scaled p값을 선택합니다. 이 가설검정은 표본크기에 민감하며 독립적인 모형 채택 규칙이 아닙니다." else "Robust or scaled p values are selected to match the reported RMSEA when available. These hypothesis tests are sample-size sensitive and are not standalone model-acceptance rules.")
  )

}

structural_canvas_information_criteria_result_ui <- function(bundle, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  table <- structural_canvas_information_criteria(bundle)
  if (!any(is.finite(table$AIC)) && !any(is.finite(table$BIC))) return(NULL)
  display <- table
  display_columns <- intersect(c(
    "Model", "N", "Estimator", "Admissible", "LogLik", "Free parameters",
    "AIC", "BIC", "Adjusted BIC", "Comparison status",
    "Delta AIC", "Delta BIC", "Delta Adjusted BIC"
  ), names(display))
  display <- display[, display_columns, drop = FALSE]
  numeric_columns <- names(display)[vapply(display, is.numeric, logical(1))]
  for (column in numeric_columns) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
  names(display) <- vapply(names(display), function(name) switch(
    name,
    "Estimator" = "Estimator",
    "Admissible" = "Admissible",
    "LogLik" = "logL",
    "Free parameters" = "Params",
    "Adjusted BIC" = "Adj BIC",
    "Comparison status" = "Comparison",
    "Delta AIC" = "Δ AIC",
    "Delta BIC" = "Δ BIC",
    "Delta Adjusted BIC" = "Δ Adj BIC",
    name
  ), character(1))
  tagList(
    tags$h5(if (ko) "표 2 가이드: 우도 기반 정보기준" else "Guide for Table 2: Likelihood-based information criteria"),
    tags$div(class = "table-responsive structural-landscape-table-wrap", tags$table(class = "table table-striped table-bordered structural-result-table structural-landscape-table structural-information-criteria-table",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) "AIC, BIC, adjusted BIC가 낮을수록 복잡도 벌점을 반영한 상대적 기대 적합이 더 좋다는 뜻입니다. Delta 값은 이 표에 표시된 모형 중 가장 작은 기준값을 기준으로 계산합니다." else "Lower AIC, BIC, and adjusted BIC indicate better relative expected fit after penalizing complexity; delta values are relative to the smallest criterion in this displayed set."),
    if (any(grepl("^Not comparable", table$`Comparison status`))) tags$p(class = "structural-result-note", if (ko) "분석 관측치, 관측변수, 추정량 계열 또는 모형 허용성이 모형 간에 달라 delta 값을 숨겼습니다." else "Delta values were suppressed because analyzed observations, observed variables, estimator family, or model admissibility differed across models."),
    tags$p(class = "structural-result-note", if (ko) "정보기준은 같은 관측치와 관측변수, 같은 likelihood 및 추정량 계열로 적합한 모형끼리만 비교하십시오. 이는 절대적 적합도 검정이 아니며, 비교 가능한 likelihood가 없는 WLSMV 모형에는 보고하지 않습니다." else "Compare information criteria only for models fitted to the same observations and observed variables with the same likelihood and estimator family. They are not absolute goodness-of-fit tests and are not reported for WLSMV models without a comparable likelihood.")
  )

}

structural_canvas_bollen_stine_result_ui <- function(bundle, language) {
  result <- bundle$bollen_stine_result %||% NULL
  if (is.null(result) || !nrow(result)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  display <- result
  display[["Observed chi-square"]] <- vapply(display[["Observed chi-square"]], format_decimal3, character(1))
  display[["Bootstrap p"]] <- vapply(display[["Bootstrap p"]], format_p, character(1))
  for (column in c("Monte Carlo SE", "Monte Carlo 95% lower", "Monte Carlo 95% upper")) {
    display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
  }
  display[["Valid %"]] <- paste0(vapply(display[["Valid %"]], format_decimal3, character(1)), "%")
  tagList(
    tags$h5(if (ko) "Bollen-Stine 부트스트랩 전반적 적합도 검정" else "Bollen-Stine bootstrap global-fit test"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) "부트스트랩 p값은 plus-one 보정식 (1 + 관측값 이상인 부트스트랩 χ² 수) / (1 + 유효 반복 수)을 사용합니다. 유효 반복은 주 CFA와 같은 수렴, 분산, 공분산행렬, 자유도, 잠재상관 허용성 점검을 통과한 반복입니다. 작은 p값은 exact-fit 귀무가설에서 전반적 모형 부적합을 의미합니다." else "The bootstrap p value uses the plus-one correction: (1 + bootstrap chi-square values at least as large as observed) / (1 + valid replicates). Valid replicates pass the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA. A small p value indicates global model misfit under the exact-fit null."),
    tags$p(class = "structural-result-note", if (ko) "Monte Carlo SE와 95% Wilson 구간은 유효 부트스트랩 반복 수가 유한하기 때문에 생기는 시뮬레이션 오차를 나타냅니다. 모집단 모형 모수에 대한 신뢰구간이 아닙니다." else "Monte Carlo SE and the 95% Wilson interval quantify simulation error from the finite number of valid bootstrap replicates; they are not a confidence interval for a population model parameter."),
    if (any(result$Status != "Adequate")) tags$p(class = "structural-result-note", if (ko) "요청한 재표집의 80% 미만만 수렴하고 허용 가능한 통계량을 만들었습니다. 부트스트랩 p값과 Monte Carlo 구간을 불안정한 값으로 보고, 보고 전에 수렴 또는 허용성 문제를 해결하십시오." else "Fewer than 80% of requested resamples produced a converged admissible statistic. Treat the bootstrap p value and Monte Carlo interval as unstable; resolve convergence or admissibility problems before reporting."),
    if (isTRUE(bundle$modified_from_baseline)) tags$p(class = "structural-result-note", if (ko) "이 모형은 분석 자료를 보고 수정한 탐색적 수정 모형입니다. Bollen-Stine 결과는 탐색적 참고값이며 자료 기반 수정에 대한 확인적 근거를 제공하지 않습니다." else "This model was modified using the analyzed data. Its Bollen-Stine result is exploratory and does not provide confirmatory evidence for the data-driven modification."),
    tags$p(class = "structural-result-note", if (ko) "이 transformed-data 검정은 결측이 없는 연속형 단일집단 ML CFA에서만 사용할 수 있으며, 근사 적합도 지수, 잔차 진단, 실질적 모형 평가를 대체하지 않습니다." else "This transformed-data test is available only for complete continuous single-group ML CFA and does not replace approximate fit indices, residual diagnostics, or substantive model evaluation.")
  )

}
