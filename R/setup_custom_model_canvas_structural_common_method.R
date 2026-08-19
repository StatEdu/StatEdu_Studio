# Common-method-bias diagnostics for lavaan-based CFA/CB-SEM models.

structural_canvas_common_method_methods <- function(value = NULL) {
  allowed <- c("harman", "single_factor_cfa", "common_latent_factor")
  value <- as.character(value %||% character(0))
  value <- intersect(value, allowed)
  if (!length(value)) c("harman", "single_factor_cfa") else value
}

structural_canvas_common_method_lookup_label <- function(value, labels) {
  value <- as.character(value %||% "")
  matched <- unname(labels[value])
  ifelse(is.na(matched), value, matched)
}

structural_canvas_common_method_method_label <- function(value, language = NULL) {
  ko <- identical(normalize_app_language(language %||% "en"), "ko")
  labels <- if (ko) {
    c(
      "Harman single-factor screen" = "Harman 단일요인 점검",
      "Single-factor CFA comparison" = "단일요인 CFA 비교",
      "Common latent factor screen" = "공통잠재요인 점검",
      "Common method diagnostics" = "동일방법편의 진단"
    )
  } else {
    c()
  }
  structural_canvas_common_method_lookup_label(value, labels)
}

structural_canvas_common_method_statistic_label <- function(value, language = NULL) {
  ko <- identical(normalize_app_language(language %||% "en"), "ko")
  labels <- if (ko) {
    c(
      "First factor percent" = "첫 단일요인 설명분산(%)",
      "One-factor model fit" = "단일요인 모형 적합도",
      "Max loading change" = "최대 적재량 변화",
      "Mean absolute method loading" = "평균 절대 방법요인 적재량",
      "Requested diagnostics" = "요청한 진단"
    )
  } else {
    c()
  }
  structural_canvas_common_method_lookup_label(value, labels)
}

structural_canvas_common_method_status_label <- function(value, language = NULL) {
  ko <- identical(normalize_app_language(language %||% "en"), "ko")
  labels <- if (ko) {
    c("OK" = "양호", "Review" = "검토 필요", "Screen only" = "탐색 점검", "Screen flag" = "탐색 신호", "Not available" = "계산 불가")
  } else {
    c()
  }
  structural_canvas_common_method_lookup_label(value, labels)
}

structural_canvas_common_method_guidance_label <- function(value, language = NULL) {
  ko <- identical(normalize_app_language(language %||% "en"), "ko")
  labels <- if (ko) {
    c(
      "Harman PCA requires at least two numeric indicators." = "Harman PCA에는 숫자형 관측변수가 최소 2개 필요합니다.",
      "Harman PCA requires more complete rows than indicators." = "Harman PCA에는 관측변수 수보다 많은 완전 응답 행이 필요합니다.",
      "Harman PCA failed for the selected indicators." = "선택된 관측변수로 Harman PCA를 계산하지 못했습니다.",
      "Exploratory first-component screen only; values above 50% may flag concentration, but no value establishes presence or absence of common-method bias." = "탐색적 첫 성분 점검값입니다. 50% 초과는 집중 가능성을 표시할 수 있지만 어떤 값도 동일방법편향의 존재나 부재를 확정하지 않습니다.",
      "First unrotated principal component percent; values above 50% suggest possible common-method concentration." = "회전하지 않은 첫 번째 주성분의 설명분산 비율입니다. 50%를 넘으면 동일방법편의 집중 가능성을 검토합니다.",
      "A single-factor CFA fit the indicators reasonably well; review common-method concentration." = "단일요인 CFA가 비교적 양호하게 적합했습니다. 동일방법편의 집중 가능성을 검토하십시오.",
      "The single-factor CFA did not show a strong one-factor fit pattern." = "단일요인 CFA에서 강한 단일요인 적합 패턴은 보이지 않았습니다.",
      "Common latent factor screening requires at least three indicators." = "공통잠재요인 점검에는 관측변수가 최소 3개 필요합니다.",
      "Equal-loading common latent factor screen; loading changes above .20 suggest possible method effects." = "동일 적재 공통잠재요인 점검입니다. 적재량 변화가 .20을 넘으면 방법효과 가능성을 검토합니다.",
      "Descriptive size of the common method factor loadings." = "공통방법요인 적재량의 기술적 크기입니다.",
      "Mean absolute method factor loading above .50 suggests substantial method-factor concentration." = "평균 절대 방법요인 적재량이 .50을 넘으면 방법요인 집중 가능성이 큽니다.",
      "No common method diagnostics could be computed for the current model." = "현재 모형에서 동일방법편의 진단 결과를 계산하지 못했습니다."
    )
  } else {
    c()
  }
  value <- as.character(value %||% "")
  translated <- structural_canvas_common_method_lookup_label(value, labels)
  translated <- sub(
    "^Single-factor CFA failed:",
    "단일요인 CFA 계산 실패:",
    translated
  )
  translated <- sub(
    "^Common latent factor model failed:",
    "공통잠재요인 모형 계산 실패:",
    translated
  )
  translated <- sub(
    "^The single-factor CFA did not show a strong one-factor fit pattern\\.",
    "단일요인 CFA에서 강한 단일요인 적합 패턴은 보이지 않았습니다.",
    translated
  )
  translated <- sub(
    "^A single-factor CFA fit the indicators reasonably well; review common-method concentration\\.",
    "단일요인 CFA가 비교적 양호하게 적합했습니다. 동일방법편의 집중 가능성을 검토하십시오.",
    translated
  )
  translated
}

structural_canvas_common_method_unavailable_result <- function(methods = NULL, reason = NULL) {
  methods <- structural_canvas_common_method_methods(methods)
  guidance <- reason %||% "No common method diagnostics could be computed for the current model."
  list(
    enabled = TRUE,
    methods = methods,
    indicators = character(0),
    summary = data.frame(
      Method = "Common method diagnostics",
      Statistic = "Requested diagnostics",
      Value = NA_real_,
      Status = "Not available",
      Guidance = guidance,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    fit = data.frame(),
    comparison = data.frame(),
    loading_change = data.frame()
  )
}

structural_canvas_common_method_indicators <- function(fit) {
  parameters <- tryCatch(lavaan::parameterTable(fit), error = function(error) data.frame())
  indicators <- unique(as.character(parameters$rhs[parameters$op == "=~"] %||% character(0)))
  indicators <- indicators[nzchar(indicators)]
  if (length(indicators)) return(indicators)
  observed <- tryCatch(lavaan::lavNames(fit, "ov"), error = function(error) character(0))
  unique(as.character(observed[nzchar(observed)]))
}

structural_canvas_common_method_harman <- function(data, indicators) {
  variables <- intersect(indicators, names(data))
  variables <- variables[vapply(data[variables], is.numeric, logical(1))]
  if (length(variables) < 2L) {
    return(list(value = NA_real_, status = "Not available", note = "Harman PCA requires at least two numeric indicators."))
  }
  values <- data[variables]
  values <- values[stats::complete.cases(values), , drop = FALSE]
  if (nrow(values) <= length(variables) + 1L) {
    return(list(value = NA_real_, status = "Not available", note = "Harman PCA requires more complete rows than indicators."))
  }
  pca <- tryCatch(stats::prcomp(values, center = TRUE, scale. = TRUE), error = function(error) NULL)
  if (is.null(pca) || is.null(pca$sdev) || !length(pca$sdev)) {
    return(list(value = NA_real_, status = "Not available", note = "Harman PCA failed for the selected indicators."))
  }
  variance <- pca$sdev^2
  percent <- 100 * variance[[1L]] / sum(variance)
  list(
    value = percent,
    status = if (is.finite(percent) && percent <= 50) "Screen only" else "Screen flag",
    note = "Exploratory first-component screen only; values above 50% may flag concentration, but no value establishes presence or absence of common-method bias."
  )
}

structural_canvas_common_method_fit_values <- function(fit) {
  keys <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")
  values <- tryCatch(lavaan::fitMeasures(fit, keys), error = function(error) stats::setNames(rep(NA_real_, length(keys)), keys))
  values[keys]
}

structural_canvas_common_method_fit_summary <- function(fit) {
  values <- structural_canvas_common_method_fit_values(fit)
  data.frame(
    chisq = as.numeric(values[["chisq"]]),
    df = as.numeric(values[["df"]]),
    p = as.numeric(values[["pvalue"]]),
    CFI = as.numeric(values[["cfi"]]),
    TLI = as.numeric(values[["tli"]]),
    RMSEA = as.numeric(values[["rmsea"]]),
    SRMR = as.numeric(values[["srmr"]]),
    check.names = FALSE
  )
}

structural_canvas_common_method_fit_label <- function(fit) {
  values <- structural_canvas_common_method_fit_summary(fit)
  paste0(
    "CFI=", format_decimal3(values$CFI), ", TLI=", format_decimal3(values$TLI),
    ", RMSEA=", format_decimal3(values$RMSEA), ", SRMR=", format_decimal3(values$SRMR)
  )
}

structural_canvas_common_method_fit_comparison <- function(fit_table) {
  if (!is.data.frame(fit_table) || !nrow(fit_table) || !"Model" %in% names(fit_table)) return(data.frame())
  baseline <- fit_table[fit_table$Model == "Research_model", , drop = FALSE]
  candidates <- fit_table[fit_table$Model != "Research_model", , drop = FALSE]
  if (!nrow(baseline) || !nrow(candidates)) return(data.frame())
  rows <- lapply(seq_len(nrow(candidates)), function(index) {
    row <- candidates[index, , drop = FALSE]
    delta_chisq <- suppressWarnings(as.numeric(row$chisq) - as.numeric(baseline$chisq))
    delta_df <- suppressWarnings(as.numeric(row$df) - as.numeric(baseline$df))
    delta_cfi <- suppressWarnings(as.numeric(row$CFI) - as.numeric(baseline$CFI))
    delta_rmsea <- suppressWarnings(as.numeric(row$RMSEA) - as.numeric(baseline$RMSEA))
    delta_srmr <- suppressWarnings(as.numeric(row$SRMR) - as.numeric(baseline$SRMR))
    delta_p <- if (is.finite(delta_chisq) && is.finite(delta_df) && abs(delta_df) > 0) {
      stats::pchisq(abs(delta_chisq), df = abs(delta_df), lower.tail = FALSE)
    } else {
      NA_real_
    }
    data.frame(
      Comparison = paste0(row$Model, " vs Research_model"),
      `Delta chisq` = delta_chisq,
      `Delta df` = delta_df,
      `Delta p` = delta_p,
      `Delta CFI` = delta_cfi,
      `Delta RMSEA` = delta_rmsea,
      `Delta SRMR` = delta_srmr,
      Note = if (row$Model == "Single_factor_CFA") {
        "Single-factor CFA is a diagnostic alternative model; use differences as screening evidence, not as a strict nested-model test."
      } else {
        "Common latent factor comparison screens whether fit and loadings change after adding the method factor."
      },
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  table
}

structural_canvas_common_method_conclusion <- function(common_method, language = NULL) {
  summary <- common_method$summary %||% data.frame()
  if (!is.data.frame(summary) || !nrow(summary)) {
    status <- "Not available"
    guidance <- "No common method diagnostics could be computed for the current model."
  } else if (any(summary$Status == "Review", na.rm = TRUE)) {
    status <- "Review"
    clf <- summary[summary$Method == "Common latent factor screen" & summary$Statistic == "Max loading change", , drop = FALSE]
    clf_loading <- summary[summary$Method == "Common latent factor screen" & summary$Statistic == "Mean absolute method loading", , drop = FALSE]
    single <- summary[summary$Method == "Single-factor CFA comparison", , drop = FALSE]
    harman <- summary[summary$Method == "Harman single-factor screen", , drop = FALSE]
    evidence <- character(0)
    if (nrow(clf) && identical(clf$Status[[1L]], "Review")) {
      evidence <- c(evidence, "Common latent factor loading changes exceed the .20 screening threshold.")
    }
    if (nrow(clf_loading) && identical(clf_loading$Status[[1L]], "Review")) {
      evidence <- c(evidence, "Mean common method factor loading exceeds the .50 screening threshold.")
    }
    if (nrow(single) && identical(single$Status[[1L]], "Review")) {
      evidence <- c(evidence, "The single-factor CFA is comparatively close to conventional fit references and requires review.")
    }
    if (nrow(harman) && identical(harman$Status[[1L]], "Review")) {
      evidence <- c(evidence, "Harman's first factor exceeds 50%.")
    }
    guidance <- paste(evidence, collapse = " ")
    if (!nzchar(guidance)) guidance <- "At least one common method diagnostic crossed its review threshold."
  } else if (any(summary$Status == "Screen flag", na.rm = TRUE)) {
    status <- "Screen flag"
    guidance <- "An exploratory screen crossed its heuristic threshold; this is neither a diagnosis nor evidence that common-method bias caused the results."
  } else if (any(summary$Status == "Not available", na.rm = TRUE)) {
    status <- "Not available"
    guidance <- "Some common method diagnostics could not be computed; interpret the available checks only."
  } else {
    status <- if (any(summary$Status == "Screen only", na.rm = TRUE)) "Screen only" else "OK"
    guidance <- "No selected screen crossed its heuristic threshold; this does not establish the absence of common-method bias."
  }
  ko <- identical(normalize_app_language(language %||% "en"), "ko")
  data.frame(
    Status = if (ko) structural_canvas_common_method_status_label(status, language) else status,
    Guidance = if (ko) structural_canvas_common_method_conclusion_guidance_label(guidance) else guidance,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

structural_canvas_common_method_conclusion_guidance_label <- function(value) {
  labels <- c(
    "No common method diagnostics could be computed for the current model." = "현재 모형에서 동일방법편의 진단 결과를 계산하지 못했습니다.",
    "Common latent factor loading changes exceed the .20 screening threshold." = "공통잠재요인 점검에서 적재량 변화가 .20 기준을 초과했습니다.",
    "Mean common method factor loading exceeds the .50 screening threshold." = "평균 공통방법요인 적재량이 .50 기준을 초과했습니다.",
    "The single-factor CFA is comparatively close to conventional fit references and requires review." = "단일요인 CFA가 관행적 적합도 참고값에 비교적 가까워 추가 검토가 필요합니다.",
    "Harman's first factor exceeds 50%." = "Harman 첫 요인의 설명분산이 50%를 초과했습니다.",
    "At least one common method diagnostic crossed its review threshold." = "하나 이상의 동일방법편의 진단 지표가 검토 기준을 넘었습니다.",
    "Some common method diagnostics could not be computed; interpret the available checks only." = "일부 동일방법편의 진단을 계산하지 못했습니다. 계산된 지표만 제한적으로 해석하십시오.",
    "No selected common method diagnostic crossed its screening threshold." = "선택한 동일방법편의 진단에서 검토 기준을 넘는 신호는 없습니다.",
    "An exploratory screen crossed its heuristic threshold; this is neither a diagnosis nor evidence that common-method bias caused the results." = "탐색적 점검값이 휴리스틱 기준을 넘었지만 이는 진단도 아니고 동일방법편향이 결과를 유발했다는 증거도 아닙니다.",
    "No selected screen crossed its heuristic threshold; this does not establish the absence of common-method bias." = "선택한 점검값이 휴리스틱 기준을 넘지 않았지만 동일방법편향의 부재를 입증하지는 않습니다."
  )
  parts <- strsplit(as.character(value %||% ""), " (?=Mean common|The single-factor|Harman's|At least|Some common|No selected)", perl = TRUE)[[1L]]
  translated <- structural_canvas_common_method_lookup_label(parts, labels)
  paste(translated, collapse = " ")
}

structural_canvas_common_method_single_factor <- function(data, indicators, estimator, missing, std_lv, ordered) {
  indicators <- intersect(indicators, names(data))
  if (length(indicators) < 3L) {
    return(list(fit = NULL, status = "Not available", note = "Single-factor CFA requires at least three indicators."))
  }
  syntax <- paste("CMB_SINGLE =~", paste(indicators, collapse = " + "))
  ordered <- intersect(as.character(ordered %||% character(0)), indicators)
  parameterization <- if (length(ordered)) "theta" else "delta"
  fit <- tryCatch(
    lavaan::cfa(syntax, data = data, estimator = estimator, missing = missing, std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE, parameterization = parameterization),
    error = function(error) error
  )
  if (inherits(fit, "error")) {
    return(list(fit = NULL, status = "Not available", note = paste("Single-factor CFA failed:", conditionMessage(fit))))
  }
  values <- structural_canvas_common_method_fit_summary(fit)
  good_fit <- is.finite(values$CFI) && is.finite(values$TLI) && is.finite(values$RMSEA) && is.finite(values$SRMR) &&
    values$CFI >= .90 && values$TLI >= .90 && values$RMSEA <= .08 && values$SRMR <= .08
  list(
    fit = fit,
    status = if (good_fit) "Review" else "OK",
    note = if (good_fit) {
      "A single-factor CFA fit the indicators reasonably well; review common-method concentration."
    } else {
      "The single-factor CFA did not show a strong one-factor fit pattern."
    }
  )
}

structural_canvas_common_method_loading_change <- function(original_fit, method_fit, method_factor) {
  baseline <- tryCatch(lavaan::standardizedSolution(original_fit), error = function(error) data.frame())
  adjusted <- tryCatch(lavaan::standardizedSolution(method_fit), error = function(error) data.frame())
  if (!nrow(baseline) || !nrow(adjusted)) {
    return(list(max_change = NA_real_, mean_method_loading = NA_real_, table = data.frame()))
  }
  baseline <- baseline[baseline$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
  adjusted_measurement <- adjusted[adjusted$op == "=~" & adjusted$lhs != method_factor, c("lhs", "rhs", "est.std"), drop = FALSE]
  method_loadings <- adjusted[adjusted$op == "=~" & adjusted$lhs == method_factor, c("rhs", "est.std"), drop = FALSE]
  key <- paste(baseline$lhs, baseline$rhs, sep = "\r")
  adjusted_key <- paste(adjusted_measurement$lhs, adjusted_measurement$rhs, sep = "\r")
  matched <- match(key, adjusted_key)
  rows <- data.frame(
    Latent = baseline$lhs,
    Indicator = baseline$rhs,
    `Baseline beta` = suppressWarnings(as.numeric(baseline$est.std)),
    `Method-adjusted beta` = suppressWarnings(as.numeric(adjusted_measurement$est.std[matched])),
    check.names = FALSE
  )
  rows$`Absolute change` <- abs(rows$`Method-adjusted beta` - rows$`Baseline beta`)
  method_match <- match(rows$Indicator, method_loadings$rhs)
  rows$`Method factor beta` <- suppressWarnings(as.numeric(method_loadings$est.std[method_match]))
  max_change <- if (any(is.finite(rows$`Absolute change`))) max(rows$`Absolute change`, na.rm = TRUE) else NA_real_
  mean_method_loading <- if (any(is.finite(rows$`Method factor beta`))) mean(abs(rows$`Method factor beta`), na.rm = TRUE) else NA_real_
  list(max_change = max_change, mean_method_loading = mean_method_loading, table = rows)
}

structural_canvas_common_method_clf <- function(original_fit, syntax, data, indicators, estimator, missing, std_lv, ordered) {
  indicators <- intersect(indicators, names(data))
  if (length(indicators) < 3L) {
    return(list(fit = NULL, status = "Not available", note = "Common latent factor screening requires at least three indicators."))
  }
  latent_names <- tryCatch(lavaan::lavNames(original_fit, "lv"), error = function(error) character(0))
  method_factor <- "STATEDU_CMV"
  while (method_factor %in% c(latent_names, names(data))) method_factor <- paste0(method_factor, "_X")
  method_line <- paste(method_factor, "=~", paste(paste0("cmv_loading*", indicators), collapse = " + "))
  orthogonal_lines <- if (length(latent_names)) paste(method_factor, "~~ 0*", latent_names) else character(0)
  method_syntax <- paste(c(syntax, method_line, paste(method_factor, "~~ 1*", method_factor), orthogonal_lines), collapse = "\n")
  ordered <- intersect(as.character(ordered %||% character(0)), indicators)
  parameterization <- if (length(ordered)) "theta" else "delta"
  fit <- tryCatch(
    lavaan::sem(method_syntax, data = data, estimator = estimator, missing = missing, std.lv = isTRUE(std_lv), ordered = ordered, auto.cov.lv.x = FALSE, parameterization = parameterization),
    error = function(error) error
  )
  if (inherits(fit, "error")) {
    return(list(fit = NULL, status = "Not available", note = paste("Common latent factor model failed:", conditionMessage(fit))))
  }
  change <- structural_canvas_common_method_loading_change(original_fit, fit, method_factor)
  max_status <- if (is.finite(change$max_change)) {
    if (change$max_change <= .20) "OK" else "Review"
  } else {
    "Not available"
  }
  method_loading_status <- if (is.finite(change$mean_method_loading)) {
    if (change$mean_method_loading <= .50) "OK" else "Review"
  } else {
    "Not available"
  }
  status <- if ("Review" %in% c(max_status, method_loading_status)) {
    "Review"
  } else if ("Not available" %in% c(max_status, method_loading_status)) {
    "Not available"
  } else {
    "OK"
  }
  list(
    fit = fit,
    status = status,
    max_status = max_status,
    method_loading_status = method_loading_status,
    note = "Equal-loading common latent factor screen; loading changes above .20 suggest possible method effects.",
    method_factor = method_factor,
    max_change = change$max_change,
    mean_method_loading = change$mean_method_loading,
    loading_change = change$table
  )
}

structural_canvas_run_common_method_diagnostics <- function(
  result, data, analysis_type, estimator, missing, std_lv, ordered, methods = NULL
) {
  if (is.null(result$fit) || !inherits(result$fit, "lavaan") || !analysis_type %in% c("cfa", "cbsem", "sem")) {
    return(NULL)
  }
  methods <- structural_canvas_common_method_methods(methods)
  indicators <- structural_canvas_common_method_indicators(result$fit)
  rows <- list()
  single_fit <- NULL
  clf <- list(fit = NULL, loading_change = data.frame())
  if ("harman" %in% methods) {
    harman <- tryCatch(
      structural_canvas_common_method_harman(data, indicators),
      error = function(error) list(value = NA_real_, status = "Not available", note = conditionMessage(error))
    )
    rows[[length(rows) + 1L]] <- data.frame(
      Method = "Harman single-factor screen",
      Statistic = "First factor percent",
      Value = harman$value,
      Status = harman$status,
      Guidance = harman$note,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  if ("single_factor_cfa" %in% methods) {
    single <- tryCatch(
      structural_canvas_common_method_single_factor(data, indicators, estimator, missing, std_lv, ordered),
      error = function(error) list(fit = NULL, status = "Not available", note = conditionMessage(error))
    )
    single_fit <- single$fit
    rows[[length(rows) + 1L]] <- data.frame(
      Method = "Single-factor CFA comparison",
      Statistic = "One-factor model fit",
      Value = if (is.null(single$fit)) NA_real_ else NA_real_,
      Status = single$status,
      Guidance = if (is.null(single$fit)) single$note else paste(single$note, structural_canvas_common_method_fit_label(single$fit)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  if ("common_latent_factor" %in% methods) {
    clf <- tryCatch(
      structural_canvas_common_method_clf(result$fit, result$syntax, data, indicators, estimator, missing, std_lv, ordered),
      error = function(error) list(fit = NULL, status = "Not available", note = conditionMessage(error), loading_change = data.frame())
    )
    rows[[length(rows) + 1L]] <- data.frame(
      Method = "Common latent factor screen",
      Statistic = "Max loading change",
      Value = clf$max_change %||% NA_real_,
      Status = clf$max_status %||% clf$status,
      Guidance = clf$note,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (is.finite(clf$mean_method_loading %||% NA_real_)) {
      rows[[length(rows) + 1L]] <- data.frame(
        Method = "Common latent factor screen",
        Statistic = "Mean absolute method loading",
        Value = clf$mean_method_loading,
        Status = clf$method_loading_status %||% clf$status,
        Guidance = "Mean absolute method factor loading above .50 suggests substantial method-factor concentration.",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }
  summary <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (!nrow(summary)) {
    return(structural_canvas_common_method_unavailable_result(methods))
  }
  fit_rows <- list(Research_model = result$fit)
  if (!is.null(single_fit)) fit_rows$Single_factor_CFA <- single_fit
  if (!is.null(clf$fit)) fit_rows$Common_latent_factor <- clf$fit
  fit_table <- do.call(rbind, lapply(names(fit_rows), function(name) {
    row <- structural_canvas_common_method_fit_summary(fit_rows[[name]])
    data.frame(Model = name, row, check.names = FALSE)
  }))
  rownames(fit_table) <- NULL
  comparison_table <- structural_canvas_common_method_fit_comparison(fit_table)
  list(
    enabled = TRUE,
    methods = methods,
    indicators = indicators,
    summary = summary,
    fit = fit_table,
    comparison = comparison_table,
    loading_change = clf$loading_change %||% data.frame()
  )
}

structural_canvas_common_method_display_table <- function(common_method, format_values = TRUE, language = NULL) {
  table <- common_method$summary %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  if (isTRUE(format_values) && "Value" %in% names(table)) {
    table$Value <- vapply(table$Value, function(value) {
      value <- suppressWarnings(as.numeric(value))
      if (!is.finite(value)) "" else format_decimal3(value)
    }, character(1))
  }
  ko <- identical(normalize_app_language(language %||% "en"), "ko")
  if (ko) {
    if ("Method" %in% names(table)) {
      table$Method <- vapply(table$Method, structural_canvas_common_method_method_label, character(1), language = language)
    }
    if ("Statistic" %in% names(table)) {
      table$Statistic <- vapply(table$Statistic, structural_canvas_common_method_statistic_label, character(1), language = language)
    }
    if ("Status" %in% names(table)) {
      table$Status <- vapply(table$Status, structural_canvas_common_method_status_label, character(1), language = language)
    }
    if ("Guidance" %in% names(table)) {
      table$Guidance <- vapply(table$Guidance, structural_canvas_common_method_guidance_label, character(1), language = language)
    }
    translated_names <- c(
      Method = "방법",
      Statistic = "통계량",
      Value = "값",
      Status = "상태",
      Guidance = "해석"
    )[names(table)]
    names(table) <- ifelse(is.na(translated_names), names(table), unname(translated_names))
  }
  table
}
