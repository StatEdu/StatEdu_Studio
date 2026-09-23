# Descriptive top-level HTMT from observed subscale means. Never use the
# higher-order model-implied latent correlations to assess its own separation.
structural_canvas_higher_htmt_data <- function(fit, data = NULL) {
  pt <- lavaan::parameterTable(fit)
  ov <- lavaan::lavNames(fit, "ov")
  lv <- lavaan::lavNames(fit, "lv")
  measurement <- unique(pt[pt$op == "=~", c("lhs", "rhs")])
  higher <- measurement[measurement$rhs %in% lv, , drop = FALSE]
  if (!nrow(higher)) return(NULL)
  unavailable <- function(reason) list(available = FALSE, reason = reason)
  if (lavaan::lavInspect(fit, "ngroups") != 1L) return(unavailable("Multigroup higher-order HTMT is not supported."))
  roots <- setdiff(unique(measurement$lhs), higher$rhs)
  if (length(roots) < 2L) return(unavailable("At least two top-level constructs are required."))
  if (is.null(data) || !is.data.frame(data)) return(unavailable("The analyzed raw data are unavailable."))
  case_rows <- lavaan::lavInspect(fit, "case.idx")
  if (is.list(case_rows)) case_rows <- case_rows[[1L]]
  if (!length(case_rows) || any(case_rows > nrow(data))) return(unavailable("Analyzed case indices are unavailable."))
  data <- data[case_rows, , drop = FALSE]
  specs <- list()
  source_sets <- list()
  mapping <- list()
  for (root in roots) {
    children <- measurement$rhs[measurement$lhs == root]
    latent_children <- children %in% lv
    if (any(latent_children) && !all(latent_children)) return(unavailable("Mixed observed and latent indicators of one construct are not supported."))
    blocks <- if (all(latent_children)) lapply(children, function(child) measurement$rhs[measurement$lhs == child]) else as.list(children)
    if (any(lengths(blocks) < 1L) || any(!unlist(blocks) %in% ov)) return(unavailable("Only second-order models with observed first-order indicators are supported."))
    ids <- character(length(blocks))
    for (i in seq_along(blocks)) {
      id <- paste0("htmt_indicator_", length(specs) + 1L)
      specs[[id]] <- unique(blocks[[i]])
      ids[[i]] <- id
      mapping[[length(mapping) + 1L]] <- data.frame(Construct = root, Indicator = children[[i]],
        Scoring = if (all(latent_children)) "Unit-weighted item mean" else "Observed indicator (unchanged)",
        Items = paste(blocks[[i]], collapse = ", "), check.names = FALSE)
    }
    source_sets[[root]] <- list(ids = ids, items = unlist(blocks, use.names = FALSE))
  }
  variables <- unique(unlist(specs, use.names = FALSE))
  if (!all(variables %in% names(data))) return(unavailable("Some indicator columns are unavailable."))
  numeric_data <- lapply(data[variables], function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) x <- suppressWarnings(as.numeric(x))
    if (!is.numeric(x)) return(NULL)
    x
  })
  if (any(vapply(numeric_data, is.null, logical(1)))) return(unavailable("Subscale scoring requires numeric item values."))
  # Do not turn non-numeric labels into an arbitrary ordinal coding.
  if (any(vapply(variables, function(v) any(!is.na(data[[v]]) & is.na(numeric_data[[v]])), logical(1)))) return(unavailable("Subscale scoring requires numeric item values; recode category labels first."))
  numeric_data <- as.data.frame(numeric_data)
  values <- as.data.frame(lapply(specs, function(items) rowMeans(numeric_data[items], na.rm = FALSE)))
  keep <- apply(values, 1L, function(x) all(is.finite(x)))
  values <- values[keep, , drop = FALSE]
  if (nrow(values) < 3L) return(unavailable("Fewer than three complete cases are available for top-level indicators."))
  indicators <- lapply(source_sets, `[[`, "ids")
  correlations <- suppressWarnings(stats::cor(values))
  result <- structural_canvas_htmt(correlations, indicators)
  # Shared source items invalidate disjoint HTMT even when mean columns differ.
  for (i in seq_len(nrow(result$pairs))) {
    a <- result$pairs$Factor1[[i]]; b <- result$pairs$Factor2[[i]]
    source_a <- source_sets[[a]]$items; source_b <- source_sets[[b]]$items
    reason <- if (anyDuplicated(source_a) || anyDuplicated(source_b) || length(intersect(source_a, source_b))) {
      "Overlapping source items prevent standard HTMT calculation"
    } else if (any(!is.finite(correlations[c(indicators[[a]], indicators[[b]]), c(indicators[[a]], indicators[[b]])]))) {
      "Constant or unavailable indicator correlations"
    } else ""
    if (nzchar(reason)) {
      result$matrix[a, b] <- result$matrix[b, a] <- NA_real_
      result$pairs$HTMT[[i]] <- NA_real_
      result$pairs$Reason[[i]] <- reason
      result$pairs$Criterion[[i]] <- "Not assessed"
    }
  }
  raw_data <- numeric_data[keep, , drop = FALSE]
  raw_indicators <- lapply(source_sets, function(x) unique(x$items))
  raw_result <- structural_canvas_htmt(suppressWarnings(stats::cor(raw_data)), raw_indicators)
  for (i in seq_len(nrow(raw_result$pairs))) {
    a <- raw_result$pairs$Factor1[[i]]; b <- raw_result$pairs$Factor2[[i]]
    selected <- c(raw_indicators[[a]], raw_indicators[[b]])
    if (any(vapply(raw_data[selected], function(x) !is.finite(stats::sd(x)) || stats::sd(x) == 0, logical(1)))) {
      raw_result$matrix[a, b] <- raw_result$matrix[b, a] <- NA_real_
      raw_result$pairs$HTMT[[i]] <- NA_real_
      raw_result$pairs$Criterion[[i]] <- "Not assessed"
      raw_result$pairs$Reason[[i]] <- "Constant or unavailable indicator correlations"
    }
  }
  list(available = TRUE, data = values, indicators = indicators, result = result,
       raw_data = raw_data, raw_indicators = raw_indicators, raw_result = raw_result,
       mapping = do.call(rbind, mapping), n = nrow(values), excluded = sum(!keep))
}

structural_canvas_higher_htmt_result <- function(bundle) {
  result <- structural_canvas_higher_htmt_data(bundle$fit, bundle$analysis_data)
  if (isTRUE(result$available)) {
    threshold <- as.numeric(bundle$htmt_threshold %||% .85)
    result$result$pairs$Criterion <- ifelse(is.finite(result$result$pairs$HTMT),
      ifelse(result$result$pairs$HTMT < threshold, "Below reference", "Review needed"), "Not assessed")
    result$result$threshold <- threshold
    result$raw_result$pairs$Criterion <- ifelse(is.finite(result$raw_result$pairs$HTMT),
      ifelse(result$raw_result$pairs$HTMT < threshold, "Below reference", "Review needed"), "Not assessed")
    result$raw_result$threshold <- threshold
  }
  result
}

structural_canvas_htmt_bootstrap_with_higher <- function(fit, data, indicators, reps,
    confidence = .95, seed = default_seed(), ordered = character(0), threshold = .85,
    ci_method = "percentile", progress = NULL, cancel = NULL) {
  higher <- structural_canvas_higher_htmt_data(fit, data)
  has_higher <- isTRUE(higher$available)
  first_total <- reps + if (identical(structural_canvas_bootstrap_ci_method(ci_method), "bca")) nrow(data) else 0L
  second_total <- if (has_higher) reps + if (identical(structural_canvas_bootstrap_ci_method(ci_method), "bca")) higher$n else 0L else 0L
  last_valid <- 0L
  first_progress <- if (is.function(progress)) function(done, total, valid) {
    last_valid <<- valid
    progress(done, first_total + 2L * second_total, valid)
  } else NULL
  value <- structural_canvas_htmt_bootstrap(data, indicators, reps, confidence, seed,
    ordered, threshold, ci_method, progress = first_progress, cancel = cancel)
  if (has_higher) {
    second_valid <- 0L
    second_progress <- if (is.function(progress)) function(done, total, valid) {
      second_valid <<- valid
      progress(first_total + done, first_total + 2L * second_total, last_valid + valid)
    } else NULL
    higher_ci <- structural_canvas_htmt_bootstrap(higher$data, higher$indicators,
      reps, confidence, seed, character(0), threshold, ci_method,
      progress = second_progress, cancel = cancel, strict_correlations = TRUE)
    if (is.data.frame(higher_ci)) {
      blocked <- !is.finite(higher$result$pairs$HTMT)
      for (column in c("Lower", "Upper", "One-sided upper")) higher_ci[blocked, column] <- NA_real_
      for (column in c("Upper < threshold", "Upper < 1")) higher_ci[blocked, column] <- "Not assessed"
      higher_ci$Status[blocked] <- "Not assessed"
      higher_ci[["Valid replicates"]][blocked] <- 0L
      higher_ci[["Valid %"]][blocked] <- 0
    }
    attr(value, "higher_order") <- higher_ci
    raw_progress <- if (is.function(progress)) function(done, total, valid) progress(first_total + second_total + done, first_total + 2L * second_total, last_valid + second_valid + valid) else NULL
    raw_ci <- structural_canvas_htmt_bootstrap(higher$raw_data, higher$raw_indicators,
      reps, confidence, seed, character(0), threshold, ci_method, progress = raw_progress, cancel = cancel, strict_correlations = TRUE)
    if (is.data.frame(raw_ci)) {
      blocked <- !is.finite(higher$raw_result$pairs$HTMT)
      for (column in c("Lower", "Upper", "One-sided upper")) raw_ci[blocked, column] <- NA_real_
      for (column in c("Upper < threshold", "Upper < 1")) raw_ci[blocked, column] <- "Not assessed"
      raw_ci$Status[blocked] <- "Not assessed"
      raw_ci[["Valid replicates"]][blocked] <- 0L
      raw_ci[["Valid %"]][blocked] <- 0
    }
    attr(value, "higher_order_raw") <- raw_ci
  }
  value
}

structural_canvas_higher_validity_estimates <- function(bundle) {
  fit <- bundle$fit
  if (lavaan::lavInspect(fit, "ngroups") != 1L) return(data.frame())
  p <- lavaan::parameterEstimates(fit)
  lv <- lavaan::lavNames(fit, "lv")
  ov <- lavaan::lavNames(fit, "ov")
  measurement <- p[p$op == "=~", , drop = FALSE]
  higher <- measurement[measurement$rhs %in% lv, , drop = FALSE]
  if (!nrow(higher)) return(data.frame())
  roots <- setdiff(unique(measurement$lhs), higher$rhs)
  covariance <- as.matrix(lavaan::lavInspect(fit, "cov.lv"))
  observed_covariance <- as.matrix(lavaan::fitted(fit)$cov)
  raw <- identical(bundle$validity_formula %||% "standardized", "model_implied")
  admissible <- isTRUE(lavaan::lavInspect(fit, "converged")) &&
    isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check")))
  rows <- lapply(roots, function(root) {
    row <- data.frame(Factor = root, AVE = NA_real_, CR = NA_real_, Reason = "", check.names = FALSE)
    reject <- function(reason) { row$AVE <- row$CR <- NA_real_; row$Reason <- reason; row }
    loading <- measurement[measurement$lhs == root, , drop = FALSE]
    children <- loading$rhs
    if (!admissible) return(reject("The CFA solution is not converged or admissible"))
    if (length(children) < 2L) return(reject("At least two direct indicators are required"))
    if (!(all(children %in% ov) || all(children %in% lv))) return(reject("Mixed observed and latent indicators are not supported"))
    if (any(table(measurement$rhs)[children] > 1L) || any(p$op == "~" & p$lhs %in% children)) {
      return(reject("Cross-loaded or structurally regressed direct indicators are not supported"))
    }
    if (all(children %in% lv) && any(measurement$lhs %in% children & !measurement$rhs %in% ov)) {
      return(reject("Only second-order measurement hierarchies are supported"))
    }
    residual <- matrix(0, length(children), length(children), dimnames = list(children, children))
    rr <- p[p$op == "~~" & p$lhs %in% children & p$rhs %in% children, , drop = FALSE]
    if (!all(children %in% rr$lhs[rr$lhs == rr$rhs])) return(reject("Direct-indicator residual variances are unavailable"))
    for (i in seq_len(nrow(rr))) {
      residual[rr$lhs[[i]], rr$rhs[[i]]] <- residual[rr$rhs[[i]], rr$lhs[[i]]] <- rr$est[[i]]
    }
    lambda <- loading$est
    variance <- covariance[root, root]
    if (!raw) {
      direct_covariance <- if (all(children %in% lv)) covariance else observed_covariance
      variances <- diag(direct_covariance)[children]
      if (any(!is.finite(variances)) || any(variances <= 0) || !is.finite(variance) || variance <= 0) {
        return(reject("Direct-indicator or factor variances are invalid"))
      }
      # Use total indicator SDs, not standardized residual correlations.
      residual <- residual / sqrt(outer(variances, variances))
      lambda <- lambda * sqrt(variance / variances)
      variance <- 1
    }
    if (any(!is.finite(c(lambda, residual, variance))) || variance <= 0 ||
        any(diag(residual) < 0) || min(eigen(residual, symmetric = TRUE, only.values = TRUE)$values) < -1e-8) {
      return(reject("Invalid loadings or direct-indicator residual covariance"))
    }
    # The higher-order calculation treats lower-order factors as indicators.
    # Off-diagonal disturbances belong in CR's denominator, not in AVE.
    signal <- sum(lambda^2) * variance
    common <- sum(lambda)^2 * variance
    row$AVE <- if (raw) signal / (signal + sum(diag(residual))) else mean(lambda^2)
    row$CR <- if (common + sum(residual) > 0) common / (common + sum(residual)) else NA_real_
    if (any(!is.finite(c(row$AVE, row$CR))) || any(c(row$AVE, row$CR) < 0 | c(row$AVE, row$CR) > 1 + 1e-8)) {
      return(reject("AVE or CR is outside its admissible range"))
    }
    row
  })
  do.call(rbind, rows)
}

structural_canvas_higher_validity_note <- function(bundle, estimates) {
  paste0("AVE = average variance extracted; CR = composite reliability. ",
    if (identical(bundle$validity_formula %||% "standardized", "model_implied"))
      "AVE and CR use unstandardized loadings, total factor variances and residual covariances. " else
      "AVE and CR use fully standardized loadings and residual covariances. ",
    "For higher-order constructs, direct lower-order factors are the indicators; for first-order constructs, original items are the indicators. CR includes residual covariances. These CFA estimates are not reliability estimates of the pooled-item or subscale-mean score. ",
    if (any(nzchar(estimates$Reason))) paste0("N/A: ", paste(paste0(estimates$Factor[nzchar(estimates$Reason)], " (", estimates$Reason[nzchar(estimates$Reason)], ")"), collapse = "; "), ". ") else "")
}

structural_canvas_higher_validity_html <- function(bundle) {
  estimates <- structural_canvas_higher_validity_estimates(bundle)
  if (!nrow(estimates)) return(NULL)
  table <- estimates[c("Factor", "AVE", "CR")]
  for (column in c("AVE", "CR")) table[[column]] <- vapply(table[[column]],
    function(x) if (is.finite(x)) format_decimal3(x) else "N/A", character(1))
  structural_canvas_basic_html_table(table, role = "main", orientation = "portrait",
    class = "table table-striped table-bordered structural-higher-validity",
    title = "Top-level reliability and convergent validity",
    note = structural_canvas_higher_validity_note(bundle, estimates))
}

structural_canvas_htmt_ci_html <- function(bundle, pairs, ci = NULL, title = "HTMT", display_map = NULL) {
  normalize_pairs <- function(x) {
    names(x)[names(x) == "Factor1"] <- "Factor 1"
    names(x)[names(x) == "Factor2"] <- "Factor 2"
    x
  }
  pairs <- normalize_pairs(pairs)
  if (!nrow(pairs)) return(NULL)
  reps <- as.integer(bundle$htmt_bootstrap %||% 0L)
  state <- if (reps <= 0L) "Not requested" else if (isTRUE(bundle$cfa_bootstrap_pending)) "Pending" else
    if (isTRUE(bundle$cfa_bootstrap_canceled)) "Canceled" else "Unavailable"
  table <- pairs[c("Factor 1", "Factor 2", "HTMT")]
  table[["95% CI lower"]] <- "—"
  table[["95% CI upper"]] <- "—"
  table[["Valid replicates"]] <- "—"
  table$Status <- state
  if (reps > 0L && is.data.frame(ci) && nrow(ci)) {
    ci <- normalize_pairs(ci)
    key <- function(x) vapply(seq_len(nrow(x)), function(i)
      paste(sort(c(x[["Factor 1"]][i], x[["Factor 2"]][i])), collapse = "\r"), character(1))
    matched <- match(key(pairs), key(ci))
    for (column in c("Lower", "Upper")) {
      values <- ci[[column]][matched]
      table[[if (column == "Lower") "95% CI lower" else "95% CI upper"]] <- vapply(values,
        function(x) if (is.finite(x)) format_decimal3(x) else "—", character(1))
    }
    found <- !is.na(matched)
    table[["Valid replicates"]][found] <- as.character(ci[["Valid replicates"]][matched[found]])
    table$Status[found] <- as.character(ci$Status[matched[found]])
  }
  table$HTMT <- vapply(table$HTMT, function(x) if (is.finite(x)) format_decimal3(x) else "—", character(1))
  if (!is.null(display_map)) for (column in c("Factor 1", "Factor 2")) {
    mapped <- unname(display_map[table[[column]]])
    table[[column]][!is.na(mapped)] <- mapped[!is.na(mapped)]
  }
  method <- structural_canvas_bootstrap_ci_method(bundle$htmt_ci_method %||% "bias_corrected")
  label <- switch(method, bca = "BCa", bias_corrected = "bias-corrected (BC)", "percentile")
  note <- paste0("HTMT = heterotrait-monotrait ratio; CI = confidence interval; LLCI = lower confidence limit; ULCI = upper confidence limit. ",
    "Two-sided 95% bootstrap confidence intervals", if (reps > 0L) paste0(" (", label, "; ", reps,
      " requested resamples; seed = ", as.integer(bundle$htmt_seed %||% default_seed()), ")") else "", ". ",
    if (reps <= 0L) "Select HTMT bootstrap CI in the analysis options to estimate intervals. " else "",
    "An em dash indicates an unavailable limit or count. Pending, canceled or unavailable intervals are not evidence of discriminant validity. Valid replicates and status describe estimation quality; they are not validity decisions.")
  structural_canvas_basic_html_table(table, role = "main", orientation = "portrait",
    class = "table table-striped table-bordered structural-htmt-ci-main",
    title = paste0(title, ": 95% confidence intervals"), note = note)
}

structural_canvas_higher_htmt_note <- function(result, raw = FALSE) {
  paste0("HTMT = heterotrait-monotrait ratio. ", if (raw) "Original items are pooled within each top-level construct; subdimensions can affect this ratio. " else "Each lower-order factor is represented by its unit-weighted item mean; observed indicators of first-order constructs are unchanged. ",
    "Both top-level versions use Pearson correlations on the same ", result$n,
    " complete cases, excluding ", result$excluded, " analyzed cases. Items require prior scoring in the intended direction. These descriptive observed-score ratios are not higher-order latent correlations or polychoric HTMT. Interpret alongside CFA and first-order HTMT; .85/.90 are reference values, not validity decisions.")
}

structural_canvas_higher_htmt_html <- function(bundle, details = FALSE, language = "en", raw = FALSE) {
  ko <- identical(normalize_app_language(language), "ko")
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  higher <- structural_canvas_higher_htmt_result(bundle)
  if (is.null(higher)) return(NULL)
  if (!isTRUE(higher$available)) {
    if (!details) return(NULL)
    reasons <- c(
      "Multigroup higher-order HTMT is not supported."="다집단 고차요인 HTMT는 지원하지 않습니다.",
      "At least two top-level constructs are required."="상위 구성개념이 최소 2개 필요합니다.",
      "The analyzed raw data are unavailable."="분석한 원자료를 사용할 수 없습니다.",
      "Analyzed case indices are unavailable."="분석 대상 사례 인덱스를 사용할 수 없습니다.",
      "Mixed observed and latent indicators of one construct are not supported."="하나의 구성개념에 관측·잠재 지표를 혼합하는 방식은 지원하지 않습니다.",
      "Only second-order models with observed first-order indicators are supported."="관측된 일차 지표가 있는 이차 모형만 지원합니다.",
      "Some indicator columns are unavailable."="일부 지표 열을 사용할 수 없습니다.",
      "Subscale scoring requires numeric item values."="하위척도 점수 계산에는 숫자 문항값이 필요합니다.",
      "Subscale scoring requires numeric item values; recode category labels first."="하위척도 점수 계산에는 숫자 문항값이 필요합니다. 범주 라벨을 먼저 재코딩하십시오.",
      "Fewer than three complete cases are available for top-level indicators."="상위 지표의 완전사례가 3개 미만입니다.")
    reason <- higher$reason
    if (reason %in% names(reasons)) reason <- tr(reason, unname(reasons[[reason]]))
    title <- if (raw) tr("Top-level HTMT (original items)", "상위 수준 HTMT (원문항)") else tr("Top-level HTMT (subscale scores)", "상위 수준 HTMT (하위요인 문항 평균)")
    return(result_note_paragraph(class = "structural-result-note", sprintf(tr("%s: not assessed. %s", "%s: 평가하지 않음. %s"), title, reason)))
  }
  result <- if (raw) higher$raw_result else higher$result
  title <- if (raw) "Top-level HTMT (original items)" else "Top-level HTMT (subscale scores)"
  if (!details) {
    mat <- result$matrix
    values <- matrix("", nrow(mat), ncol(mat), dimnames = dimnames(mat))
    diag(values) <- "—"
    values[lower.tri(values)] <- vapply(mat[lower.tri(mat)], format_decimal3, character(1))
    table <- data.frame(Factor = rownames(mat), values, check.names = FALSE, row.names = NULL)
    return(shiny::tagList(
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-htmt-matrix structural-higher-htmt-matrix",
        role = "main", orientation = if (nrow(mat) <= 9L) "portrait" else "landscape",
        note = structural_canvas_higher_htmt_note(higher, raw), title = title),
      structural_canvas_htmt_ci_html(bundle, result$pairs,
        attr(bundle$htmt_bootstrap_result, if (raw) "higher_order_raw" else "higher_order"), title)
    ))
  }
  pairs <- result$pairs
  names(pairs)[names(pairs) == "Factor1"] <- "Factor 1"
  names(pairs)[names(pairs) == "Factor2"] <- "Factor 2"
  pairs$HTMT <- vapply(pairs$HTMT, format_decimal3, character(1))
  pairs$Reason[!nzchar(pairs$Reason)] <- "—"
  mapping <- higher$mapping
  localize_details <- function(table) {
    labels <- c("Unit-weighted item mean"="문항의 동일가중 평균", "Observed indicator (unchanged)"="입력된 관측지표 그대로 사용",
      "Overlapping source items prevent standard HTMT calculation"="원문항이 중복되어 표준 HTMT 계산 불가",
      "Cross-loaded indicators prevent standard HTMT calculation"="교차적재 지표로 표준 HTMT 계산 불가",
      "Constant or unavailable indicator correlations"="상수 지표 또는 계산할 수 없는 지표 상관",
      "At least two indicators per factor are required"="각 요인에 최소 2개의 지표 필요",
      "Within-factor correlations are insufficient"="요인 내부 상관이 불충분함",
      "Below reference"="참고값 미만", "Review needed"="검토 필요", "Not assessed"="평가하지 않음",
      "Adequate"="충분", "Caution"="주의", "Unreliable"="신뢰 불가", "Yes"="예", "No"="아니요",
      "BCa unavailable"="BCa 사용 불가", "Percentile"="백분위수", "Bias-corrected (BC)"="편향보정 (BC)")
    for (column in intersect(c("Scoring", "Reason", "Criterion", "Status", "CI method", "Upper < threshold", "Upper < 1"), names(table))) {
      table[[column]] <- vapply(as.character(table[[column]]), function(x) if (x %in% names(labels)) tr(x, unname(labels[[x]])) else x, character(1), USE.NAMES=FALSE)
    }
    headers <- c("Construct"="구성개념", "Indicator"="지표", "Scoring"="점수 구성", "Items"="원문항", "Criterion"="기준", "CI method"="CI 산출법")
    names(table) <- vapply(names(table), function(x) if(x %in% names(headers)) tr(x, unname(headers[[x]])) else x, character(1), USE.NAMES=FALSE)
    attr(table, "result_user_columns") <- seq_len(ncol(table))
    table
  }
  mapping <- localize_details(mapping)
  pairs <- localize_details(pairs)
  ci <- attr(bundle$htmt_bootstrap_result, if (raw) "higher_order_raw" else "higher_order")
  ci_note <- tr("95% CI = 95% confidence interval. Bootstrap intervals resample the same complete cases used for the point estimates; Pearson correlations are recomputed for the displayed indicator construction. No factor scores are estimated. 'Upper < threshold' uses the one-sided 95% upper limit; 'Upper < 1' uses the two-sided 95% upper limit.", "95% CI는 95% 신뢰구간입니다. 부트스트랩은 점추정에 사용한 동일한 완전사례를 재표집하며, 표시된 지표 구성에 대해 Pearson 상관을 다시 계산합니다. 요인점수는 추정하지 않습니다. 상한 < 기준은 단측 95% 상한을, 상한 < 1은 양측 95% 상한을 사용합니다.")
  if (is.data.frame(ci)) {
    names(ci)[names(ci) == "Lower"] <- "95% CI lower"
    names(ci)[names(ci) == "Upper"] <- "95% CI upper"
    names(ci)[names(ci) == "One-sided upper"] <- "One-sided 95% upper"
    for (column in c("95% CI lower", "95% CI upper", "One-sided 95% upper", "Valid %")) ci[[column]] <- vapply(ci[[column]], format_decimal3, character(1))
  }
  if (is.data.frame(ci)) ci <- localize_details(ci)
  requested <- as.integer(bundle$htmt_bootstrap %||% 0L) > 0L
  state_note <- if (!requested) tr("Select HTMT bootstrap CI in the analysis options for intervals.", "구간추정이 필요하면 분석 옵션에서 HTMT 부트스트랩 CI를 선택하십시오.") else if (isTRUE(bundle$cfa_bootstrap_pending)) {
    tr("Top-level HTMT bootstrap intervals are being computed in the background.", "상위 수준 HTMT 부트스트랩 구간을 백그라운드에서 계산하고 있습니다.")
  } else if (isTRUE(bundle$cfa_bootstrap_canceled)) tr("Top-level HTMT bootstrap was canceled; point estimates remain available.", "상위 수준 HTMT 부트스트랩이 중단되었습니다. 점추정값은 유지됩니다.") else tr("Top-level HTMT bootstrap intervals could not be estimated.", "상위 수준 HTMT 부트스트랩 구간을 추정하지 못했습니다.")
  detail_title <- if (raw) tr("Top-level HTMT (original items)", "상위 수준 HTMT (원문항)") else tr("Top-level HTMT (subscale scores)", "상위 수준 HTMT (하위요인 문항 평균)")
  shiny::tagList(
    if (!raw) structural_canvas_basic_html_table(mapping, role = "appendix", language = language, title = sprintf(tr("%s: indicator construction", "%s: 지표 구성"), detail_title)),
    structural_canvas_basic_html_table(pairs, role = "appendix", language = language, title = sprintf(tr("%s: assessment", "%s: 세부 판단"), detail_title)),
    if (is.data.frame(ci)) structural_canvas_basic_html_table(ci, role = "appendix", language = language, note = ci_note, title = sprintf(tr("%s: bootstrap intervals", "%s: 부트스트랩 신뢰구간"), detail_title))
      else result_note_paragraph(class = "structural-result-note", state_note)
  )
}
