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
    "Indicator mean replacement (seminr-compatible)"
  } else {
    bundle$missing %||% structural_canvas_reporting_lavaan_option(bundle, "missing", "")
  }
  missing_sensitivity <- if (identical(analysis_type, "plssem")) {
    missing_diagnostics <- bundle$missing_diagnostics %||% list()
    if (isTRUE(missing_diagnostics$available) && as.integer(missing_diagnostics$imputed_cell_n %||% 0L) > 0L) {
      "Review missingness mechanism and sensitivity to a justified alternative"
    } else {
      "Not required - no missing indicator cells"
    }
  } else {
    paste0(structural_canvas_missing_sensitivity_rows(bundle)$`Sensitivity assessment`[[1L]], "; status=", structural_canvas_missing_sensitivity_rows(bundle)$Status[[1L]])
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
      missing_sensitivity,
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
    "Indicator mean replacement (seminr-compatible)" = "지표 평균 대체(seminr 호환)",
    "Review missingness mechanism and sensitivity to a justified alternative" = "결측기전과 정당화된 대안 처리의 민감도 검토 필요",
    "Not required - no missing indicator cells" = "불필요 - 지표 결측 셀 없음",
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

structural_canvas_reporting_text <- function(text, language) {
  if (identical(normalize_app_language(language), "en")) return(text)
  key <- paste0("analysis.ui.", gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(text)))))
  row <- statedu_translation_table()[[key]]
  translated <- row[normalize_app_language(language)]
  if (length(translated) && !is.na(translated) && nzchar(translated)) unname(translated) else statedu_localized_text(language, text)
}

structural_canvas_reporting_settings_display <- function(bundle, analysis_type, language) {
  # Build translated labels from typed settings. Never replace substrings inside
  # user group names, RNG identifiers, or other stored parameter values.
  tr <- function(text) structural_canvas_reporting_text(text, language)
  field <- function(label, value) paste0(tr(label), "=", value)
  seed <- function(value) if (is.null(value)) tr("Not recorded") else as.character(value)
  requested <- character(0)
  if (identical(analysis_type, "plssem")) {
    r <- suppressWarnings(as.integer(bundle$pls_bootstrap %||% 0L))
    if (is.finite(r) && r > 0L) {
      bs <- bundle$pls_bootstrap_result %||% list()
      n <- suppressWarnings(as.integer(bs$nboot %||% 0L))
      ratio <- suppressWarnings(as.numeric(bs$valid_ratio %||% NA_real_))
      status <- as.character(bs$bootstrap_status %||% if (is.finite(ratio) && ratio >= .80) "Adequate" else "Not recorded")
      rng <- bs$rng %||% tr("L'Ecuyer-CMRG independent stream per requested position")
      requested <- paste0(bundle$diagnostics$estimator %||% bundle$estimator %||% "PLS", " ", tr("Bootstrap"), " R=", r, ", ",
        field("Seed", seed(bundle$pls_seed)), ", RNG=", rng, ", ",
        field("Valid", paste0(if (is.finite(n)) n else "NA", "/", r, " (", if (is.finite(ratio)) formatC(100 * ratio, format = "fg", digits = 4) else "NA", "%)")), ", ",
        field("Status", tr(status)), ", ", field("Whole-draw minimum", "80%"))
    }
  } else {
    specs <- list(
      list("reliability", "Reliability/AVE", "reliability_ci_method", "reliability_seed", "reliability"),
      list("htmt", "HTMT", "htmt_ci_method", "htmt_seed", "htmt"),
      list("bollen_stine", "Bollen-Stine", NULL, "bollen_stine_seed", NULL),
      list("effect", "Path/indirect/total-effect", "effect_bootstrap_ci_method", "effect_bootstrap_seed", "structural_effects"))
    for (spec in specs) {
      r <- suppressWarnings(as.integer(bundle[[paste0(spec[[1L]], "_bootstrap")]] %||% 0L))
      if (!is.finite(r) || r <= 0L) next
      value <- paste0(tr(spec[[2L]]), " R=", r)
      if (!is.null(spec[[3L]])) {
        method <- bundle[[spec[[3L]]]] %||% "bias_corrected"
        value <- paste0(value, ", CI=", tr(method), ", ", field("Quantile", paste0("R ", tr("Type"), " ", structural_canvas_bootstrap_quantile_type(method, spec[[5L]]))))
      }
      requested <- c(requested, paste0(value, ", ", field("Seed", seed(bundle[[spec[[4L]]]]))))
    }
  }
  prediction <- tr("Not applicable")
  if (identical(analysis_type, "plssem")) {
    result <- bundle$pls_predict_result
    folds <- suppressWarnings(as.integer(bundle$pls_predict_folds %||% 0L))
    prediction <- tr("Not requested")
    if (is.list(result)) {
      prediction <- paste0(tr("Executed"), ": ", field("Folds", result$folds %||% ""), ", ", field("Repetitions", result$reps %||% ""), ", ", field("Seed", seed(result$seed %||% bundle$pls_predict_seed)))
    } else if (is.finite(folds) && folds > 1L) {
      prediction <- paste0(tr("Requested"), ": ", field("Folds", folds), ", ", field("Repetitions", bundle$pls_predict_reps %||% 0L), ", ", field("Seed", seed(bundle$pls_predict_seed)))
    }
  }
  group <- tr("Not enabled")
  if (is.list(bundle$invariance_result)) {
    type <- as.character(bundle$invariance_result$type %||% "Group analysis")
    aliases <- c(pls_micom = "MICOM", structural_path_comparison = "Structural path comparison")
    if (type %in% names(aliases)) type <- unname(aliases[type])
    group <- tr(type)
    name <- as.character(bundle$invariance_group %||% "")
    if (nzchar(name)) group <- paste0(group, "; ", field("Group variable", name))
  } else if (isTRUE(bundle$invariance_enabled)) group <- paste0(tr("Requested"), "; ", field("Group variable", bundle$invariance_group %||% tr("Selected group")))
  holdout <- if (is.list(bundle$mi_holdout_result)) tr("Executed") else if (isTRUE(bundle$mi_holdout_enabled)) {
    paste0(tr("Requested"), ": ", field("Fraction", bundle$mi_holdout_fraction %||% ""), ", ", field("Seed", seed(bundle$mi_holdout_seed)))
  } else tr("Not enabled")
  common <- tr("Not enabled")
  if (isTRUE(bundle$common_method_enabled)) {
    methods <- as.character(bundle$common_method_methods %||% character(0))
    aliases <- c(harman = "Harman single-factor screen", single_factor_cfa = "Single-factor CFA comparison", common_latent_factor = "Common latent factor screen")
    methods <- vapply(methods, function(method) tr(if (method %in% names(aliases)) unname(aliases[method]) else method), character(1))
    common <- paste0(tr("Executed"), ": ", if (length(methods)) paste(methods, collapse = ", ") else tr("Requested"))
  }
  flag <- function(value) if (is.na(value)) tr("Not recorded") else if (isTRUE(value)) tr("Yes") else tr("No")
  c("Bootstrap settings" = if (length(requested)) paste(requested, collapse = "; ") else tr("Not requested"),
    "PLSpredict setting" = prediction, "Group analysis" = group, "MI holdout" = holdout, "Common method diagnostics" = common,
    "Admissibility and convergence" = paste0(field("Converged", flag(bundle$converged %||% bundle$diagnostics$converged %||% NA)), "; ", field("Admissible solution", flag(bundle$admissible %||% bundle$diagnostics$admissible %||% NA))))
}

structural_canvas_reporting_context_result_ui <- function(bundle, analysis_type, language = statedu_initial_language()) {
  rows <- structural_canvas_reporting_context_rows(bundle, analysis_type)
  if (!nrow(rows)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  tr <- function(en, ko = en) statedu_localized_text(language, en, ko)
  display_rows <- structural_canvas_reporting_context_display_rows(rows, ko)
  if (!identical(normalize_app_language(language), "en")) {
    report_tr <- function(text) structural_canvas_reporting_text(text, language)
    display_rows[[1L]] <- vapply(rows$Item, report_tr, character(1))
    display_rows[[2L]] <- vapply(rows$Value, report_tr, character(1))
    # Package identifiers, sample counts and authored variable names are literal.
    literal <- rows$Item %in% c("Analysis engine", "Analyzed N")
    display_rows[literal, 2L] <- rows$Value[literal]
    if (identical(analysis_type, "plssem")) {
      algorithm <- bundle$diagnostics$estimator %||% bundle$estimator %||% "PLS"
      label <- report_tr(if (identical(toupper(as.character(algorithm)), "PLSC")) "PLSc path modeling" else "PLS path modeling")
      mode <- as.character(bundle$diagnostics$estimator_selection_mode %||% bundle$estimator_selection_mode %||% "")
      requested <- toupper(as.character(bundle$diagnostics$estimator_requested %||% bundle$estimator_requested %||% ""))
      if (nzchar(mode)) label <- paste0(label, " (", if (identical(requested, "AUTO")) paste0(report_tr("Rule-based recommendation accepted"), "; ") else "", report_tr(mode), ")")
      display_rows[rows$Item == "Estimator or algorithm", 2L] <- label
    } else {
      sensitivity <- structural_canvas_missing_sensitivity_rows(bundle)
      display_rows[rows$Item == "Missing-data sensitivity", 2L] <- paste0(report_tr(sensitivity$`Sensitivity assessment`[[1L]]), "; ", report_tr("Status"), "=", report_tr(sensitivity$Status[[1L]]))
    }
    settings <- structural_canvas_reporting_settings_display(bundle, analysis_type, language)
    for (item in names(settings)) display_rows[rows$Item == item, 2L] <- settings[[item]]
    if (length(bundle$ordered %||% character(0))) display_rows[rows$Item == "Ordered indicators", 2L] <- rows$Value[rows$Item == "Ordered indicators"]
    attr(display_rows, "result_user_columns") <- c(1L, 2L)
  }
  construct_rows <- structural_canvas_construct_reporting_rows(bundle, analysis_type, FALSE)
  if (!identical(normalize_app_language(language), "en")) {
    aliases <- c(commonFactor = "Common factor", composite = "Composite", unspecified = "Unspecified", reflective = "Reflective", formative = "Formative", auto = "Automatic", modeA = "Mode A", modeB = "Mode B", sum = "Equal weights", predefined = "Predefined weights")
    for (column in seq.int(2L, ncol(construct_rows))) construct_rows[[column]] <- vapply(construct_rows[[column]], function(value) {
      # Migration audit identifiers are retained, rather than rewritten as prose.
      if (column == 8L && value != "None") return(value)
      if (value %in% names(aliases)) value <- unname(aliases[value])
      report_tr(value)
    }, character(1))
    names(construct_rows) <- vapply(names(construct_rows), report_tr, character(1))
  }
  construct_name <- names(construct_rows)[[1L]]
  compact_constructs <- structural_canvas_compact_common_display_columns(
    construct_rows,
    construct_name
  )
  attr(compact_constructs$table, "result_user_columns") <- seq_len(ncol(compact_constructs$table))
  common_construct_note <- if (length(compact_constructs$common)) {
    paste0(
      if (identical(normalize_app_language(language), "en")) "Shared specification for all constructs: " else paste0(report_tr("Shared specification for all constructs"), ": "),
      paste0(names(compact_constructs$common), " = ", unname(compact_constructs$common), collapse = "; "),
      "."
    )
  } else ""
  div(
    class = "result-section regression-result-panel structural-reporting-context",
    h4(tr("Reporting checklist", "보고 체크리스트")),
    structural_canvas_basic_html_table(display_rows, language = language),
    tags$h5(tr("Construct specification and computational representation", "구성개념 명세와 실제 계산 표현")),
    structural_canvas_basic_html_table(compact_constructs$table, language = language),
    if (nzchar(common_construct_note)) result_note_paragraph(class = "structural-result-note", common_construct_note),
    result_note_paragraph(
      class = "structural-result-note",
      tr("Only construct-specific differences remain in the table; values shared by every construct are reported once above.", "구성개념마다 다른 값만 표에 남기고, 모든 구성개념에 같은 값은 위 공통 명세에 한 번만 표시합니다.")
    )
  )
}

structural_canvas_measurement_diagnostics_ui <- function(diagnostics, bundle, analysis_type, table_number, language) {
  if (!is.data.frame(diagnostics) || !nrow(diagnostics)) return(NULL)
  tr <- function(en, ko = en) statedu_localized_text(language, en, ko)
  header_map <- if (identical(analysis_type, "plssem")) c(Loading = "Outer loading", Weight = "Outer weight", Mode = "Measurement mode") else c(Latent = "Latent factor")
  for (key in intersect(names(header_map), names(diagnostics))) names(diagnostics)[names(diagnostics) == key] <- unname(header_map[key])
  if (!identical(analysis_type, "plssem")) return(tagList(
    tags$h5(gsub("{number}", as.character(table_number), tr("Guide for Table {number}: Supplementary measurement diagnostics", "표 {number} 가이드: 보조 측정 진단"), fixed = TRUE)),
    structural_canvas_basic_html_table(diagnostics, language = language)
  ))
  formative <- structural_canvas_formative_content_validity_rows(
    bundle$snapshot %||% list(), bundle$redundancy_result %||% NULL, bundle$redundancy_construct %||% NULL)
  # These are authored descriptions, even when they equal a UI dictionary term.
  attr(formative, "result_user_columns") <- c("Construct", "Domain definition", "Indicator inclusion rationale", "Content-validity procedure/source")
  if (nrow(formative)) formative$Guidance <- vapply(seq_len(nrow(formative)), function(i) {
    if (formative$Status[i] == "Documented") tr("Report these design-based grounds with weight, collinearity, and redundancy results.", "이 설계상의 근거를 가중치, 공선성 및 중복성 결과와 함께 보고하세요.") else
      tr("Document construct-domain coverage, indicator inclusion grounds, content-validation procedure/source, and available redundancy evidence before confirmatory reporting.", "확인적 보고 전에 구성개념 영역의 포괄성, 지표 포함 근거, 내용타당도 검증 절차/출처 및 이용 가능한 중복성 근거를 기록하세요.")
  }, character(1))
  tagList(
    tags$h5(gsub("{number}", as.character(table_number), tr("Supplementary Table {number}: PLS measurement diagnostics", "표 {number} 보조: PLS 측정 진단"), fixed = TRUE)),
    structural_canvas_basic_html_table(diagnostics, class = "table table-striped table-bordered structural-pls-measurement-guide-table", language = language),
    result_note_paragraph(class = "structural-result-note", tr("For reflective indicators, review outer loadings and cross-loadings; for formative indicators, prioritize outer weights and item VIF.", "반영지표는 outer loading과 교차적재를, 형성지표는 outer weight와 item VIF를 우선 검토합니다.")),
    if (nrow(formative)) tags$h5(tr("Formative-composite content-validity evidence", "형성형 합성변수의 내용타당도 근거")),
    if (nrow(formative)) structural_canvas_basic_html_table(formative, class = "table table-striped table-bordered structural-formative-evidence-table", language = language)
  )
}

structural_canvas_effect_plan_ui <- function(plan, language = statedu_initial_language()) {
  if (!is.data.frame(plan) || !nrow(plan)) return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  labels <- c(
    "Effect" = "효과",
    "Limitation" = "제한 사항",
    "Direct effects" = "직접효과",
    "Mediation" = "매개효과",
    "Moderation" = "조절효과",
    "Moderated mediation" = "조절된 매개효과",
    "Supported" = "지원됨",
    "Available when an indirect chain contains the moderated path" = "간접효과 경로에 조절된 경로가 포함되면 사용 가능",
    "lavaan structural regression" = "lavaan 구조회귀",
    "Defined indirect and total effects" = "정의된 간접효과와 총효과",
    "Unconstrained all-pairs product indicators with double-mean-centering" = "이중 평균중심화를 적용한 비제약 전체 쌍 곱지표",
    "Unconstrained matched-pair product indicators with double-mean-centering" = "이중 평균중심화를 적용한 비제약 대응 쌍 곱지표",
    "Unconstrained all-pairs product indicators with indicator mean-centering (legacy)" = "지표 평균중심화를 적용한 비제약 전체 쌍 곱지표(기존 방식)",
    "Conditional indirect effect and index of moderated mediation" = "조건부 간접효과와 조절된 매개효과 지수",
    "Interpret only theory-specified directed paths." = "이론으로 명시한 방향성 경로만 해석하세요.",
    "Use bootstrap confidence intervals when requested; significance of component paths alone is not a mediation test." = "요청한 경우 부트스트랩 신뢰구간을 사용하세요. 구성 경로의 유의성만으로 매개효과를 검정할 수 없습니다.",
    "Johnson-Neyman regions require a continuous observed moderator or a latent moderator represented on its factor-score scale." = "Johnson-Neyman 영역에는 연속형 관측 조절변수 또는 요인점수 척도로 표현된 잠재 조절변수가 필요합니다.",
    "Inference uses the fitted product-indicator parameterization and the observed moderator range." = "추론에는 적합된 곱지표 모수화와 관측된 조절변수 범위를 사용합니다.",
    "PLS path coefficients" = "PLS 경로계수",
    "Bootstrap indirect and total effects" = "부트스트랩 간접효과와 총효과",
    "seminr two-stage interaction" = "seminr 2단계 상호작용",
    "seminr product-indicator interaction" = "seminr 곱지표 상호작용",
    "seminr orthogonalized interaction" = "seminr 직교화 상호작용",
    "PLS conditional path components; moderated-mediation inference is compiled separately" = "PLS 조건부 경로 구성요소; 조절된 매개효과 추론은 별도로 구성",
    "Requires PLS bootstrap inference; no covariance-model global fit claim." = "PLS 부트스트랩 추론이 필요하며, 공분산 모형의 전역 적합도를 주장하지 않습니다.",
    "Simple slopes are reported on the standardized construct-score scale." = "단순기울기는 표준화된 구성개념 점수 척도로 보고합니다.",
    "The interaction is re-estimated in every accepted whole-draw bootstrap sample; PLSpredict remains unavailable for interaction models." = "허용된 전체 통계량 부트스트랩 표본마다 상호작용을 재추정합니다. 상호작용 모형에서는 PLSpredict를 사용할 수 없습니다.",
    "The interaction is an uncorrected composite; PLSc consistency correction remains selective for declared common-factor constructs." = "상호작용은 보정하지 않은 합성변수이며, PLSc 일치성 보정은 공통요인으로 선언한 구성개념에만 선택적으로 적용됩니다.",
    "Status" = "상태",
    "Method" = "방법",
    "Not requested" = "요청하지 않음")
  translate <- function(value) {
    if (value %in% names(labels)) tr(value, unname(labels[[value]])) else value
  }
  for (column in intersect(c("Effect", "Status", "Method", "Limitation"), names(plan))) {
    plan[[column]] <- vapply(as.character(plan[[column]]), translate, character(1), USE.NAMES = FALSE)
  }
  names(plan) <- vapply(names(plan), translate, character(1), USE.NAMES = FALSE)
  attr(plan, "result_user_columns") <- seq_along(plan)
  div(class = "result-section structural-effect-capability-result",
    tags$h5(tr("Structural-effect capability plan", "구조효과 지원 범위")),
    result_note_paragraph(class = "structural-result-note", tr("This preflight record confirms whether requested effects are actually estimated by the selected engine.", "요청한 효과가 선택한 엔진에서 실제로 추정되는지 확인하는 사전 기록입니다.")),
    structural_canvas_basic_html_table(plan, class = "table table-striped table-bordered", language = language)
  )
}

structural_canvas_causal_boundary_ui <- function(interpretation, language = statedu_initial_language()) {
  if (!isTRUE(interpretation$applicable)) return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  labels <- c(
    "Assumption" = "인과식별 가정",
    "Recorded" = "현재 기록",
    "Consequence" = "보고상 제한",
    "Temporal ordering / study design" = "시간적 순서 / 연구 설계",
    "No unmeasured exposure-outcome confounding" = "측정하지 않은 노출-결과 교란이 없음",
    "Causal treatment/exposure identification" = "처치·노출의 인과효과 식별",
    "Sequential ignorability for mediation" = "매개효과의 순차적 무교란 가정",
    "Not collected by this workflow" = "이 분석 절차에서 수집하지 않음",
    "Not established by SEM fit" = "SEM 적합도로 확립되지 않음",
    "Not established by directed paths" = "방향성 경로로 확립되지 않음",
    "Not established; indirect chain detected" = "확립되지 않음; 간접 경로가 확인됨",
    "Not assessed; no indirect chain detected" = "평가하지 않음; 간접 경로가 확인되지 않음",
    "Do not infer temporal or causal direction from path arrows alone." = "경로 화살표만으로 시간적·인과적 방향을 추론하지 마십시오.",
    "Fit indices, bootstrap intervals, and significant coefficients do not remove confounding bias." = "적합도 지수, 부트스트랩 구간과 유의한 계수가 교란 편향을 제거하지는 않습니다.",
    "Report coefficients as theory-directed associations unless identification is justified externally." = "외부 근거로 인과식별을 정당화하지 않는 한 계수는 이론에 의해 방향을 정한 연관으로 보고하십시오.",
    "Describe the indirect effect as an associational decomposition unless temporal order and mediator/outcome confounding assumptions are justified." = "시간적 순서와 매개변수·결과의 교란 가정을 정당화하지 않는 한 간접효과는 연관의 분해로 설명하십시오.",
    "No mediation-specific claim is indicated by the current path graph." = "현재 경로도에는 매개효과에 관한 별도 주장을 뒷받침하는 경로가 없습니다.")
  translate <- function(value) {
    if (value %in% names(labels)) tr(value, unname(labels[[value]])) else value
  }
  display <- interpretation$rows
  for (column in intersect(c("Assumption", "Recorded", "Consequence"), names(display))) {
    display[[column]] <- vapply(as.character(display[[column]]), translate, character(1), USE.NAMES = FALSE)
  }
  names(display) <- vapply(names(display), translate, character(1), USE.NAMES = FALSE)
  attr(display, "result_user_columns") <- seq_along(display)
  div(class = "result-section structural-causal-interpretation-result",
    tags$h5(tr("Causal-interpretation boundary", "인과해석 경계")),
    result_note_paragraph(class = "structural-result-note", tr("This analysis does not establish causal identification. Interpret path coefficients and indirect effects as theory-directed statistical associations. Significant coefficients, bootstrap intervals, or good fit alone do not establish causality or temporal precedence.", "현재 분석은 인과식별을 확립하지 않으므로 경로계수와 간접효과를 이론에 의해 방향을 정한 통계적 연관으로 해석하십시오. 유의한 계수, bootstrap 신뢰구간 또는 좋은 적합도만으로 인과성이나 시간적 선행성이 성립하지 않습니다.")),
    structural_canvas_basic_html_table(display, class = "table table-striped table-bordered", language = language)
  )
}

structural_canvas_register_result_outputs <- function(input, output, prefix, canvas_output, analysis_type, selected_names_fn, variable_table_fn, dataset_fn, labels_fn, app_language_fn, fit_result, result_table) {
  supplementary_ready <- reactiveVal(FALSE)
  observeEvent(fit_result(), {
    supplementary_ready(FALSE)
    later::later(function() supplementary_ready(TRUE), delay = 0.20)
  }, ignoreNULL = TRUE)
  ui_language <- function() normalize_app_language(statedu_current_language(app_language_fn))
  manuscript_result_table <- function(kind) {
    # Journal-facing screen tables have a stable English reporting contract,
    # independent of the application UI language.
    result_table(kind, "en")
  }
  appendix_result_table <- function(kind) result_table(kind, ui_language())
  table_number <- function(kind) {
    bundle <- fit_result()
    if (identical(analysis_type, "plssem")) {
      sequence <- c("overview", "fit_diagnostics", "fit")
      if (length(bundle$diagnostics$moderation_definitions %||% list())) {
        sequence <- c(sequence, "pls_moderation")
      }
      sequence <- c(sequence, "validity", "measurement")
    } else {
      sequence <- c("overview", "fit")
      if (length(bundle$covariates %||% character(0))) sequence <- c(sequence, "covariate")
      if (analysis_type %in% c("cbsem", "sem")) sequence <- c(sequence, "structural")
      invariance_result <- bundle$invariance_result %||% NULL
      if (
        analysis_type %in% c("cbsem", "sem") &&
          identical(invariance_result$type %||% "", "structural_path_comparison")
      ) {
        has_rows <- function(value) is.data.frame(value) && nrow(value) > 0L
        if (has_rows(invariance_result$measurement_invariance$table %||% data.frame())) {
          sequence <- c(sequence, "mg_measurement_gate")
        }
        if (has_rows(invariance_result$table %||% data.frame())) {
          sequence <- c(sequence, "mg_structural_models")
        }
        if (has_rows(invariance_result$path_estimates %||% data.frame())) {
          sequence <- c(sequence, "mg_group_paths")
        }
        if (has_rows(invariance_result$formal_path_tests %||% data.frame())) {
          sequence <- c(sequence, "mg_formal_tests")
        }
        if (has_rows(invariance_result$path_differences %||% data.frame())) {
          sequence <- c(sequence, "mg_pairwise")
        }
        if (has_rows(invariance_result$interaction_group_estimates %||% data.frame())) {
          sequence <- c(sequence, "mg_interaction_estimates")
        }
        if (has_rows(invariance_result$interaction_omnibus_tests %||% data.frame())) {
          sequence <- c(sequence, "mg_interaction_omnibus")
        }
        if (has_rows(invariance_result$interaction_pairwise_differences %||% data.frame())) {
          sequence <- c(sequence, "mg_interaction_pairwise")
        }
        if (has_rows(invariance_result$moderated_mediation_group_indices %||% data.frame())) {
          sequence <- c(sequence, "mg_modmed_indices")
        }
        if (has_rows(invariance_result$moderated_mediation_delta_tests %||% data.frame())) {
          sequence <- c(sequence, "mg_modmed_delta")
        }
        if (has_rows(invariance_result$moderated_mediation_pairwise_differences %||% data.frame())) {
          sequence <- c(sequence, "mg_modmed_pairwise")
        }
      }
      if (length(bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list())) {
        sequence <- c(sequence, "moderation_jn")
      }
      sequence <- c(sequence, "validity", "measurement")
      if (analysis_type %in% c("cbsem", "sem")) {
        specific_indirect <- tryCatch(
          manuscript_result_table("structural_specific_indirect"),
          error = function(error) data.frame()
        )
        if (is.data.frame(specific_indirect) && nrow(specific_indirect)) {
          sequence <- c(sequence, "specific_indirect")
        }
        if (nrow(manuscript_result_table("structural_effects"))) sequence <- c(sequence, "effects_b_p", "effects_beta_p", "effects_b_ci", "effects_beta_ci")
      }
      sequence <- c(sequence, "localfit")
    }
    index <- match(kind, sequence)
    if (is.na(index)) NA_character_ else as.character(index)
  }
  table_heading <- function(kind, ko_title, en_title) {
    number <- table_number(kind)
    title <- en_title
    if (!is.character(number) || !length(number) || is.na(number) || !nzchar(number)) return(title)
    paste0("Table ", number, ". ", title)
  }

  option_ids <- structural_canvas_option_ids(structural_analysis_options_panel(analysis_type, "en"))
  option_values <- reactiveVal(list())
  observe({
    current <- lapply(option_ids, function(id) input[[id]])
    names(current) <- option_ids
    saved <- isolate(option_values())
    for (id in option_ids) if (!is.null(current[[id]])) saved[id] <- current[id]
    option_values(saved)
  })
  output[[canvas_output]] <- renderUI({
    structural_equation_workspace(selected_names_fn(), variable_table_fn(), labels_fn(), analysis_type, statedu_current_language(app_language_fn), isolate(option_values()))
  })
  shiny::outputOptions(output, canvas_output, suspendWhenHidden = FALSE)

  output[[paste0(prefix, "_save_control")]] <- renderUI({
    shiny::req(!is.null(fit_result()))
    controls <- analysis_save_buttons(
      html_button_id = paste0(prefix, "_save_html"),
      pdf_button_id = paste0(prefix, "_save_pdf"),
      figure_button_id = paste0(prefix, "_save_figure"),
      excel_button_id = paste0(prefix, "_save_excel"),
      add_result_button_id = paste0(prefix, "_add_result"),
      language = statedu_current_language(app_language_fn))
    for (i in seq_along(controls$children)) {
      button <- controls$children[[i]]
      if (!inherits(button, "shiny.tag")) next
      id <- button$attribs$id %||% ""
      if (identical(id, paste0(prefix, "_save_figure"))) button$attribs$onclick <-
        "this.closest('.custom-model-canvas-root').querySelector('[data-action=export]').click()"
      controls$children[[i]] <- button
    }
    controls
  })

  output[[paste0(prefix, "_results")]] <- renderUI({
    shiny::req(!is.null(fit_result()))
    bundle <- fit_result()
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(
      class = "structural-analysis-results regression-results",
      h3(statedu_localized_text(ui_language(), "Analysis Results", "분석 결과")),
      div(
        class = "result-section regression-result-panel structural-main-result-panel",
        h4(table_heading("overview", "모형 개요", "Model overview")),
        uiOutput(paste0(prefix, "_result_overview"))
      ),
      if (identical(analysis_type, "plssem")) div(
        class = "result-section regression-result-panel structural-main-result-panel structural-pls-fit-diagnostics-result",
        h4(table_heading("fit_diagnostics", "PLS/PLSc 모형 적합 진단", "PLS/PLSc model fit diagnostics")),
        uiOutput(paste0(prefix, "_result_pls_fit_diagnostics"))
      ),
      div(
        class = "result-section regression-result-panel structural-main-result-panel",
        h4(if (identical(analysis_type, "plssem")) {
          table_heading("fit", "PLS 구조모형 효과", "PLS structural model effects")
        } else {
          table_heading("fit", "모형 적합도", "Model fit")
        }),
        div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit")))
      ),
      if (analysis_type %in% c("cfa", "cbsem", "sem")) uiOutput(paste0(prefix, "_result_covariate_section")),
      if (analysis_type %in% c("cbsem", "sem")) div(
        class = "result-section regression-result-panel structural-main-result-panel structural-path-result landscape-table-panel",
        h4(table_heading("structural", "구조모형 경로", "Structural model paths")),
        uiOutput(paste0(prefix, "_result_structural"))
      ),
      uiOutput(paste0(prefix, "_result_invariance")),
      if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_pls_moderation")),
      uiOutput(paste0(prefix, "_result_moderation_jn")),
      div(
        class = "result-section regression-result-panel structural-main-result-panel structural-validity-result",
        h4(table_heading("validity", "잠재구성개념 상관, 신뢰도 및 수렴·판별타당도", "Latent construct correlations, reliability, and convergent/discriminant validity")),
        uiOutput(paste0(prefix, "_result_validity")),
        uiOutput(paste0(prefix, "_result_htmt"))
      ),
      div(
        class = "result-section regression-result-panel structural-main-result-panel structural-measurement-result",
        h4(table_heading("measurement", "측정모형", "Measurement model")),
        uiOutput(paste0(prefix, "_result_measurement"))
      ),
      if (analysis_type %in% c("cbsem", "sem")) uiOutput(paste0(prefix, "_result_specific_indirect")),
      if (analysis_type %in% c("cbsem", "sem")) uiOutput(paste0(prefix, "_result_effect_main")),
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
    ko <- FALSE
    div(
      class = "result-section regression-result-panel structural-main-result-panel structural-covariate-result",
      tags$h4(table_heading("covariate", "공변량 보정모형", "Covariate-adjusted model")),
      if (nrow(effects)) tagList(
        tags$h5("Covariate effects"),
        structural_canvas_basic_html_table(
          format_table(effects),
          class = "table table-striped table-bordered structural-covariate-effect-table",
          role = "main",
          orientation = "auto",
          note = "Note. Robust rows use scaled chi-square and robust CFI, TLI, and RMSEA when available; SRMR is uncorrected.",
          note_class = "structural-result-note structural-main-note structural-main-note-1"
        )
      ),
      if (nrow(comparison)) tagList(
        tags$h5("Research-model and covariate-adjusted-model fit comparison"),
        structural_canvas_basic_html_table(
          format_table(comparison),
          class = "table table-striped table-bordered structural-covariate-fit-table",
          role = "main",
          orientation = "auto",
          note = "Note. Model differences are covariate-adjusted minus research-model values.",
          note_class = "structural-result-note structural-main-note structural-main-note-1"
        )
      )
    )
  })

  output[[paste0(prefix, "_result_supplementary_container")]] <- renderUI({
    shiny::req(supplementary_ready())
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    div(class = "result-section regression-result-panel structural-appendix-result-panel structural-supplementary-result landscape-table-panel",
      h4(statedu_localized_text(ui_language(), "Supplementary results and diagnostics", "보조 결과 및 진단")),
      if (analysis_type %in% c("cbsem", "sem")) tagList(
        uiOutput(paste0(prefix, "_result_effect_inference_details"))
      ),
      uiOutput(paste0(prefix, "_result_reporting_context")),
      uiOutput(paste0(prefix, "_result_causal_interpretation")),
      uiOutput(paste0(prefix, "_result_structural_effect_plan")),
      uiOutput(paste0(prefix, "_result_invariance_appendix")),
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
      uiOutput(paste0(prefix, "_result_htmt_details")),
      if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_validity_guide")),
      uiOutput(paste0(prefix, "_result_latent_correlation_ci")), uiOutput(paste0(prefix, "_result_validity_note")),
      uiOutput(paste0(prefix, "_result_reliability_bootstrap")), uiOutput(paste0(prefix, "_result_factor_scores")),
      uiOutput(paste0(prefix, "_result_measurement_ci")), uiOutput(paste0(prefix, "_result_measurement_diagnostics")),
      if (!identical(analysis_type, "plssem")) result_note_paragraph(
        class = "structural-result-note structural-export-contents-note",
        statedu_localized_text(ui_language(), "Excel export preserves the current screen's table order, variable labels, displayed values, and notes.", "Excel 저장은 현재 화면의 표 순서, 변수명, 표시값과 주석을 유지합니다.")
      ),
      if (identical(analysis_type, "plssem")) uiOutput(paste0(prefix, "_result_redundancy")),
      if (identical(analysis_type, "cfa")) uiOutput(paste0(prefix, "_result_parcel_plan"))
    )
  })
  output[[paste0(prefix, "_result_causal_interpretation")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem", "plssem")) return(NULL)
    bundle <- fit_result()
    interpretation <- structural_canvas_causal_interpretation(bundle$snapshot %||% list(), analysis_type)
    structural_canvas_causal_boundary_ui(interpretation, ui_language())
  })
  structural_canvas_register_fit_diagnostic_outputs(
    output, prefix, analysis_type, fit_result, manuscript_result_table, dataset_fn, app_language_fn,
    variable_table_fn, labels_fn, table_number, appendix_result_table
  )
  structural_canvas_register_validity_outputs(
    output, prefix, analysis_type, fit_result, manuscript_result_table, app_language_fn,
    variable_table_fn, labels_fn, table_number, appendix_result_table
  )
  if (analysis_type != "plssem") structural_canvas_register_local_fit_outputs(
    output, prefix, fit_result, app_language_fn, variable_table_fn, labels_fn, table_number
  )
  if (analysis_type %in% c("cbsem", "sem")) structural_canvas_register_moderation_outputs(
    output, prefix, fit_result, app_language_fn, variable_table_fn, labels_fn, table_number
  )

  for (kind in c("overview")) local({
    result_kind <- kind
    output[[paste0(prefix, "_result_", result_kind)]] <- renderUI({
      tagList(structural_canvas_basic_html_table(
        manuscript_result_table(result_kind),
        class = "table table-striped table-bordered structural-overview-table",
        role = "main",
        orientation = "portrait"
      ), if(any(vapply(fit_result()$snapshot$nodes %||% list(),function(n)!is.null(n$scoreDesign),logical(1))))
        canvas_score_audit_ui(fit_result()$snapshot, statedu_current_language(app_language_fn)))
    })
  })
  output[[paste0(prefix, "_result_structural")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- structural_canvas_path_display_table(manuscript_result_table("structural"), "Path")
    uses_bootstrap_inference <- "Inference source" %in% names(table) && any(
      grepl("^Bootstrap", as.character(table[["Inference source"]])),
      na.rm = TRUE
    )
    table <- structural_canvas_statistics_only(table)
    structural_canvas_basic_html_table(
      table,
      class = "table table-striped table-bordered structural-path-table",
      role = "main",
      orientation = "landscape",
      note = result_note_paragraph(
        class = "structural-result-note structural-main-note structural-main-note-1",
        HTML(paste0(
          "<em>Note.</em> B = unstandardized coefficient; SE = standard error; &beta; = standardized coefficient; R<sup>2</sup> = explained variance in the outcome; BH = Benjamini-Hochberg. All tests are two-sided.",
          if (uses_bootstrap_inference) " z = B / bootstrap SE (Wald approximation); bootstrap p values are empirical two-sided sign-count values, not normal-theory p values derived from z." else ""
        ))
      )
    )
  })
  output[[paste0(prefix, "_result_pls_moderation")]] <- renderUI({
    if (!identical(analysis_type, "plssem")) return(NULL)
    bundle <- fit_result()
    definitions <- bundle$diagnostics$moderation_definitions %||%
      bundle$fit$statedu_moderation_definitions %||% list()
    if (!length(definitions)) return(NULL)
    ko <- FALSE
    localize <- function(table) {
      if (!is.data.frame(table) || !nrow(table) || !ko) return(table)
      value_map <- c(
        two_stage = "2단계법", product_indicator = "곱지표법", orthogonal = "직교화법",
        `-1 SD` = "평균 - 1 SD", Mean = "평균", `+1 SD` = "평균 + 1 SD",
        "TRUE" = "예", "FALSE" = "아니요", Adequate = "충분", Insufficient = "불충분"
      )
      for (column in names(table)) {
        values <- as.character(table[[column]])
        matched <- match(values, names(value_map))
        replace <- !is.na(matched)
        values[replace] <- unname(value_map[matched[replace]])
        table[[column]] <- values
      }
      column_map <- c(
        Predictor = "예측변수", Moderator = "조절변수", Outcome = "결과변수",
        Interaction = "상호작용항", Method = "산출 방법", Estimate = "추정값",
        `Predictor main effect` = "예측변수 주효과", `Moderator main effect` = "조절변수 주효과",
        `Moderator main effect auto-added` = "조절변수 주효과 자동 추가",
        `Moderator level` = "조절변수 수준", `Moderator value` = "조절변수 값",
        `Direct effect` = "직접효과", `Interaction effect` = "상호작용효과",
        `Simple slope` = "단순기울기", `Bootstrap mean` = "부트스트랩 평균",
        `Bootstrap Mean` = "부트스트랩 평균", `Bootstrap SE` = "부트스트랩 SE",
        `95% CI lower` = "95% CI 하한", `95% CI upper` = "95% CI 상한",
        `2.5% CI` = "95% CI 하한", `97.5% CI` = "95% CI 상한",
        p = "p", `Bootstrap P Val` = "부트스트랩 p", `BH-adjusted p` = "BH 보정 p",
        `Valid replicates` = "유효 재표집", `Requested replicates` = "요청 재표집",
        `Valid ratio` = "유효 비율", `Valid N` = "유효 재표집", `Requested N` = "요청 재표집",
        `Valid Ratio` = "유효 비율", `Inference available` = "추론 가능",
        `PLSc interaction correction` = "PLSc 상호작용 보정",
        Path = "경로", `Downstream Path` = "후속 경로",
        `Moderator Level` = "조절변수 수준", `Moderator Position` = "조절변수 위치",
        `Bootstrap Status` = "부트스트랩 상태", `Inference Source` = "추론 근거"
      )
      names(table) <- ifelse(names(table) %in% names(column_map), unname(column_map[names(table)]), names(table))
      table
    }
    moderation <- localize(manuscript_result_table("pls_moderation"))
    slopes <- localize(manuscript_result_table("pls_simple_slopes"))
    modmed <- localize(manuscript_result_table("pls_moderated_mediation"))
    conditional <- localize(manuscript_result_table("pls_conditional_indirect"))
    bootstrap <- bundle$pls_bootstrap_result %||% list()
    pending <- identical(tolower(as.character(bootstrap$bootstrap_status %||% "")), "pending")
    bootstrap_requested <- suppressWarnings(as.integer(bundle$pls_bootstrap %||% 0L)) > 0L
    error_text <- trimws(as.character(bundle$pls_modmed_error %||% "")[[1L]])
    status_notes <- function() tagList(
      if (pending) result_note_paragraph(
        class = "structural-result-note structural-main-note structural-main-note-status",
        "Bootstrap inference is pending; the inferential columns will update when resampling completes."
      ),
      if (!isTRUE(bootstrap_requested)) result_note_paragraph(
        class = "structural-result-note structural-main-note structural-main-note-status structural-result-warning",
        "Bootstrap resampling was not run; inferential values are point estimates only."
      ),
      if (nzchar(error_text)) result_note_paragraph(
        class = "structural-result-note structural-main-note structural-main-note-status structural-result-warning",
        paste0("Moderated-mediation results could not be compiled: ", error_text)
      )
    )
    section <- function(title_ko, title_en, table, note_text) {
      if (!is.data.frame(table) || !nrow(table)) return(NULL)
      tagList(
        tags$h5(if (ko) title_ko else title_en),
        structural_canvas_basic_html_table(
          table,
          class = "table table-striped table-bordered structural-pls-moderation-table",
          role = "main",
          orientation = "auto",
          note = tagList(
            result_note_paragraph(class = "structural-result-note structural-main-note structural-main-note-1", note_text),
            status_notes()
          )
        )
      )
    }
    div(
      class = "result-section regression-result-panel structural-main-result-panel structural-pls-moderation-result",
      h4(table_heading("pls_moderation", "PLS 잠재 조절효과와 조절된 매개효과", "PLS latent moderation and moderated mediation")),
      section(
        "상호작용효과", "Interaction effects", moderation,
        "Note. Predictor and moderator main effects are retained under strong hierarchy; auto-added moderator paths are identified. PLSc does not correct interaction terms. All tests are two-sided."
      ),
      section(
        "단순기울기", "Simple slopes", slopes,
        "Note. Moderator values are the mean and mean ± 1 SD of standardized construct scores. Bootstrap inference is suppressed when fewer than 80% of resamples are valid."
      ),
      section(
        "조절된 매개효과 지수", "Indices of moderated mediation", modmed,
        "Note. Indices are unstandardized and bootstrap-based. Bootstrap inference is suppressed when fewer than 80% of resamples are valid; PLSc does not correct interaction terms."
      ),
      section(
        "조건부 간접효과", "Conditional indirect effects", conditional,
        "Note. Moderator values are the mean and mean ± 1 SD of standardized construct scores. Bootstrap inference is suppressed when fewer than 80% of resamples are valid; PLSc does not correct interaction terms."
      )
    )
  })
  output[[paste0(prefix, "_result_effect_main")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- manuscript_result_table("structural_effects")
    if (!nrow(table)) return(NULL)
    keys <- c("effects_b_p", "effects_beta_p", "effects_b_ci", "effects_beta_ci")
    tagList(lapply(seq_along(keys),function(index) div(
      class="result-section regression-result-panel structural-main-result-panel structural-effect-main-result",
      h4(table_heading(keys[[index]], "", paste0("Direct, indirect, and total effects (", if(index %% 2L) "B" else "β", "; ", if(index > 2L) "95% CI" else "p", ")"))),
      structural_canvas_effect_main_html_table(table, standardized=index %% 2L == 0L, ci=index > 2L)
    )))
  })
  output[[paste0(prefix, "_result_effect_inference_details")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    language <- ui_language()
    tr <- function(en, ko) statedu_localized_text(language, en, ko)
    tagList(lapply(c("structural", "structural_specific_indirect", "structural_effects"),function(kind) {
      table <- manuscript_result_table(kind)
      if (!nrow(table)) return(NULL)
      columns <- c(intersect(c("Path", "Predictor", "Outcome", "Effect"),names(table)),structural_canvas_inference_columns(table))
      details <- structural_canvas_localize_reporting_metadata(table[,columns,drop=FALSE], ui_language())
      attr(details, "result_user_columns") <- seq_along(details)
      title <- switch(kind,
        structural = tr("Inference and bootstrap details: Structural paths", "산출 근거 및 부트스트랩 진단: 구조경로"),
        structural_specific_indirect = tr("Inference and bootstrap details: Specific indirect effects", "산출 근거 및 부트스트랩 진단: 특정 간접효과"),
        tr("Inference and bootstrap details: Direct, indirect, total effects", "산출 근거 및 부트스트랩 진단: 직접·간접·총효과"))
      div(h5(title), structural_canvas_basic_html_table(details, role="appendix", orientation="landscape", language=language))
    }))
  })
  output[[paste0(prefix, "_result_structural_effects")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- appendix_result_table("structural_effects")
    if (!nrow(table)) return(NULL)
    tr <- function(en, ko) statedu_localized_text(ui_language(), en, ko)
    div(
      class = "structural-effect-summary-block",
      tags$h5(tr("Structural-path supplement: Direct, indirect, and total effects", "구조모형 경로 보조표: 직접효과, 간접효과 및 총효과")),
      structural_canvas_effect_summary_html_table(table, ci = FALSE, language = statedu_current_language(app_language_fn)),
      result_note_paragraph(class = "structural-result-note", tr("Indirect and total effects are reported separately when mediation paths are defined.", "매개경로가 정의된 경우 간접효과와 총효과를 본표의 직접 구조경로와 구분하여 보고합니다."))
    )
  })
  output[[paste0(prefix, "_result_specific_indirect")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- manuscript_result_table("structural_specific_indirect")
    if (!nrow(table)) return(NULL)
    ko <- FALSE
    bundle <- fit_result()
    bootstrap_requested <- suppressWarnings(as.integer(bundle$effect_bootstrap %||% 0L)) > 0L
    inference_note <- if (isTRUE(bootstrap_requested)) {
      if (ko) {
        "각 행은 하나의 매개경로입니다. 요청한 경로·간접·총효과 재표집에서 산출한 bootstrap SE·95% CI·p값을 사용합니다. 유효 반복이 부족하면 해당 추론값을 비워 두며 모형기반 값으로 자동 대체하지 않습니다."
      } else {
        "Each row represents one mediation path. z = B / bootstrap SE (Wald approximation); p is the empirical two-sided bootstrap p value, not a normal-theory p value derived from z. Inference is blank when too few replicates are valid; details are provided in the supplement."
      }
    } else if (ko) {
      "각 행은 하나의 매개경로입니다. 부트스트랩을 실행하지 않았으므로 모형기반 SE·95% CI·p값을 제시합니다."
    } else {
      "Each row represents one mediation path. Because bootstrap resampling was not run, model-based SEs, 95% CIs, and p values are reported."
    }
    div(
      class = "result-section regression-result-panel structural-main-result-panel structural-specific-indirect-result",
      tags$h4(table_heading("specific_indirect", "경로별 특정 간접효과", "Specific indirect effects by path")),
      structural_canvas_specific_indirect_html_table(
        table,
        "en",
        note = result_note_paragraph(
          class = "structural-result-note structural-main-note structural-main-note-1",
          paste0("Note. ", inference_note)
        )
      )
    )
  })
  output[[paste0(prefix, "_result_structural_effect_ci")]] <- renderUI({
    if (!analysis_type %in% c("cbsem", "sem")) return(NULL)
    table <- appendix_result_table("structural_effect_ci")
    if (!nrow(table)) return(NULL)
    tr <- function(en, ko) statedu_localized_text(ui_language(), en, ko)
    div(
      class = "structural-effect-summary-block",
      tags$h5(tr("Structural-path supplement: Effect beta 95% confidence intervals", "구조모형 경로 보조표: 효과 beta 95% 신뢰구간")),
      structural_canvas_effect_summary_html_table(table, ci = TRUE, language = statedu_current_language(app_language_fn)),
      structural_canvas_effect_ci_source_note(table, statedu_current_language(app_language_fn)),
      result_note_paragraph(class = "structural-result-note", tr("When bootstrap inference was requested, an interval with insufficient valid replicates is not silently replaced by a model-based normal-theory CI. Interpret blank intervals with the source note above.", "Bootstrap을 요청한 효과는 유효 반복이 부족해도 모형기반 정규이론 CI로 자동 대체하지 않습니다. 빈 구간은 위 주석의 산출 근거와 함께 해석하십시오."))
    )
  })
  output[[paste0(prefix, "_result_validity")]] <- renderUI({
    table <- manuscript_result_table("validity")
    structural_canvas_basic_html_table(
      table,
      class = "table table-striped table-bordered structural-validity-table",
      role = "main",
      orientation = "auto",
      note = tagList(
        structural_canvas_abbreviation_footnotes(table, "validity"),
        structural_canvas_symbol_footnotes(table)
      )
    )
  })
  output[[paste0(prefix, "_result_measurement")]] <- renderUI({
    table <- manuscript_result_table("measurement")
    if (identical(analysis_type, "plssem")) {
      structural_canvas_pls_measurement_main_html_table(
        table,
        note = result_note_paragraph(
          class = "structural-result-note structural-main-note structural-main-note-1",
          HTML("<em>Note.</em> † Common factor; ‡ composite; ¶ unspecified construct type. Loading/weight denotes an outer loading for reflective indicators and an outer weight for formative indicators. For a single-indicator construct, loading/weight is fixed at 1 and bootstrap inference is not applicable (—). Boot = bootstrap; SE = standard error; CI = confidence interval; BH = Benjamini-Hochberg; VIF = variance inflation factor. All tests are two-sided.")
        )
      )
    } else {
      structural_canvas_measurement_html_table(
        table,
        note = tagList(
          structural_canvas_abbreviation_footnotes(table, "measurement"),
          structural_canvas_symbol_footnotes(table)
        )
      )
    }
  })
  output[[paste0(prefix, "_result_measurement_diagnostics")]] <- renderUI({
    diagnostics <- appendix_result_table(if (identical(analysis_type, "plssem")) "measurement_guide" else "measurement_diagnostics")
    structural_canvas_measurement_diagnostics_ui(diagnostics, fit_result(), analysis_type, table_number("measurement"), ui_language())
  })
  output[[paste0(prefix, "_result_structural_effect_plan")]] <- renderUI({
    bundle <- fit_result()
    plan <- bundle$structural_effect_plan %||% bundle$diagnostics$structural_effect_plan %||% data.frame()
    structural_canvas_effect_plan_ui(plan, ui_language())
  })
  output[[paste0(prefix, "_result_effect_bootstrap")]] <- renderUI({
    bundle <- fit_result()
    result <- bundle$effect_bootstrap_result %||% NULL
    requested <- as.integer(bundle$effect_bootstrap %||% 0L)
    if (requested <= 0L) return(NULL)
    language <- ui_language()
    tr <- function(en, ko = en) statedu_localized_text(language, en, ko)
    if (isTRUE(bundle$effect_bootstrap_pending)) {
      return(result_note_paragraph(class = "structural-result-note", tr("Path, indirect, and total-effect bootstrap CIs and p values are still running. This section will update automatically when they finish.", "경로·간접·총효과 bootstrap CI/p를 계산하고 있습니다. 완료되면 이 표가 자동으로 갱신됩니다.")))
    }
    if (isTRUE(bundle$effect_bootstrap_canceled)) {
      return(result_note_paragraph(class = "structural-result-note", tr("The path, indirect, and total-effect bootstrap was stopped by the user. Base-model results and point estimates remain available.", "경로·간접·총효과 부트스트랩이 사용자 요청으로 중단되었습니다. 기본 분석 결과와 점추정값은 유지됩니다.")))
    }
    blocked_reason <- trimws(as.character(bundle$effect_bootstrap_blocked_reason %||% "")[[1L]])
    if (nzchar(blocked_reason)) {
      return(result_note_paragraph(
        class = "structural-result-note structural-result-warning",
        structural_canvas_effect_bootstrap_blocked_text(blocked_reason, language)
      ))
    }
    if (nzchar(as.character(bundle$effect_bootstrap_error %||% ""))) {
      return(result_note_paragraph(class = "structural-result-note", sprintf(tr("Path, indirect, and total-effect bootstrap failed: %s", "경로·간접·총효과 bootstrap CI/p 계산 실패: %s"), bundle$effect_bootstrap_error)))
    }
    if (is.null(result) || !nrow(result)) return(result_note_paragraph(class = "structural-result-note", tr("No usable replicate fits were obtained for the path, indirect, and total-effect bootstrap.", "경로·간접·총효과 bootstrap에서 사용 가능한 반복 적합을 얻지 못했습니다.")))
    interval_label <- if (identical(as.character(bundle$effect_bootstrap_ci_method %||% "bias_corrected"), "percentile")) "percentile" else "bias-corrected (BC)"
    quantile_type <- structural_canvas_bootstrap_quantile_type(bundle$effect_bootstrap_ci_method %||% "bias_corrected", "structural_effects")
    if (!"beta_status" %in% names(result)) result$beta_status <- ifelse(result$op == "modmed", "Not reported: product-indicator index is scale-dependent", "Not available")
    if (!"quantile_type" %in% names(result)) result$quantile_type <- quantile_type
    diagnostics <- unique(result[, c("ci_method", "quantile_type", "valid", "requested", "valid_percent", "status"), drop = FALSE])
    names(diagnostics) <- c("CI method", "Quantile type", "Valid replicates", "Requested replicates", "Valid %", "Status")
    diagnostics[["Quantile type"]] <- paste0("R type ", diagnostics[["Quantile type"]])
    moderated <- result[result$op == "modmed", c("lhs", "rhs", "estimate", "lower", "upper", "p", "beta_status", "valid", "requested", "valid_percent", "status"), drop = FALSE]
    if (nrow(moderated)) names(moderated) <- c("Indirect path", "Moderator", "Index", "95% CI lower", "95% CI upper", "Bootstrap p", "Standardized index", "Valid replicates", "Requested replicates", "Valid %", "Status")
    program_labels <- c(
      percentile = "백분위", bias_corrected = "편향보정(BC)",
      Adequate = "적정", Caution = "주의", Unreliable = "신뢰 불가",
      Estimated = "추정됨", `Not available` = "산출 불가",
      `Not available - insufficient valid bootstrap replicates` = "산출 불가 - 유효 부트스트랩 반복 부족",
      `Not reported: product-indicator index is scale-dependent` = "보고하지 않음: 곱지표 지수는 척도 의존적임"
    )
    localize_value <- function(values) vapply(as.character(values), function(value) {
      if (is.na(value) || !value %in% names(program_labels)) return(value)
      key <- if (identical(value, "bias_corrected")) "bias-corrected (BC)" else value
      tr(key, unname(program_labels[[value]]))
    }, character(1), USE.NAMES = FALSE)
    diagnostics[["CI method"]] <- localize_value(diagnostics[["CI method"]])
    diagnostics$Status <- localize_value(diagnostics$Status)
    if (nrow(moderated)) {
      moderated$Status <- localize_value(moderated$Status)
      moderated[["Standardized index"]] <- localize_value(moderated[["Standardized index"]])
    }
    header_labels <- c(
      `CI method` = "CI 방법", `Quantile type` = "분위수 계산 방식",
      `Valid replicates` = "유효 반복", `Requested replicates` = "요청 반복", `Valid %` = "유효 %", Status = "상태",
      `Indirect path` = "간접 경로", Moderator = "조절변수", Index = "지수",
      `95% CI lower` = "95% CI 하한", `95% CI upper` = "95% CI 상한",
      `Bootstrap p` = "부트스트랩 p", `Standardized index` = "표준화 지수"
    )
    localize_headers <- function(headers) vapply(headers, function(header) {
      if (!header %in% names(header_labels)) return(header)
      tr(header, unname(header_labels[[header]]))
    }, character(1), USE.NAMES = FALSE)
    names(diagnostics) <- localize_headers(names(diagnostics))
    names(moderated) <- localize_headers(names(moderated))
    attr(diagnostics, "result_user_columns") <- seq_along(diagnostics)
    attr(moderated, "result_user_columns") <- seq_along(moderated)
    div(
      class = "result-section structural-effect-bootstrap-result",
      tags$h5(tr("Path, indirect, and total-effect bootstrap diagnostics", "경로·간접·총효과 bootstrap 진단")),
      structural_canvas_basic_html_table(diagnostics, class = "table table-striped table-bordered", language = ui_language()),
      if (nrow(moderated)) tagList(
        tags$h5(tr("Index of moderated mediation", "조절된 매개효과 index")),
        structural_canvas_basic_html_table(moderated, class = "table table-striped table-bordered", language = ui_language())
      ),
      result_note_paragraph(class = "structural-result-note", sprintf(tr(
        "Case-resampling %s 95%% CIs (R quantile type %s); seed = %s. B and beta intervals for direct effects, specific indirect effects, indirect effects, and total effects are recomputed in every valid replicate. The moderated-mediation index in a product-indicator latent-moderation model is scale-dependent and has no unique standardization, so the unstandardized index with its bootstrap CI is the primary result and a standardized index is not reported. Inadmissible or nonconverged replicates are excluded; valid rates below 80%% require caution.",
        "사례 재표집 %s 95%% CI(R quantile type %s); seed = %s. 직접효과, 특정 간접효과, 간접효과와 총효과의 B와 beta 구간은 각 유효 반복에서 다시 계산됩니다. product-indicator 잠재조절모형의 조절된 매개효과 index는 척도 의존적이며 유일한 표준화 정의가 없으므로 비표준화 index와 bootstrap CI를 주 결과로 보고하고 표준화 index는 보고하지 않습니다. 부적합·미수렴 반복은 제외하며 유효율이 80%% 미만이면 주의가 필요합니다."), tr(interval_label), quantile_type, bundle$effect_bootstrap_seed))
    )
  })
  output[[paste0(prefix, "_result_redundancy")]] <- renderUI({
    if (!identical(analysis_type, "plssem")) return(NULL)
    result <- fit_result()$redundancy_result %||% list(available = FALSE, reason = "Redundancy analysis was not requested.")
    language <- ui_language()
    tr <- function(en, ko = en) statedu_localized_text(language, en, ko)
    if (!isTRUE(result$available)) return(tagList(
      tags$h5(tr("Formative redundancy analysis", "형성형 중복성 분석")),
      result_note_paragraph(class = "structural-result-note", sprintf(tr("Not assessed: %s", "미평가: %s"), structural_canvas_redundancy_text(result$reason %||% "no global criterion was selected.", language)))
    ))
    table <- data.frame(
      Construct = result$construct,
      Criterion = result$criterion,
      N = result$n,
      Loading = format_decimal3(result$loading),
      `95% CI lower` = format_decimal3(result$ci_lower),
      `95% CI upper` = format_decimal3(result$ci_upper),
      R2 = format_decimal3(result$r2),
      Guidance = structural_canvas_redundancy_text(result$guidance, language),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(table) <- c(tr("Construct", "합성변수"), tr("Global criterion variable", "전역 기준변수"), "N", tr("Loading", "적재량"), tr("95% CI lower", "95% CI 하한"), tr("95% CI upper", "95% CI 상한"), "R²", tr("Guidance", "해석"))
    attr(table, "result_user_columns") <- seq_along(table)
    tagList(
      tags$h5(tr("Formative redundancy analysis", "형성형 중복성 분석")),
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-redundancy-table", language = ui_language()),
      result_note_paragraph(class = "structural-result-note", tr(".70 is a descriptive reference, not an automatic pass rule. Also verify that the criterion adequately covers the same concept and was measured independently of the formative indicators.", ".70은 설명용 참고값이며 자동 합격선이 아닙니다. 기준변수가 동일 개념을 충분히 포괄하고 형성지표와 독립적으로 측정되었는지 함께 검토하십시오."))
    )
  })
  output[[paste0(prefix, "_result_parcel_plan")]] <- renderUI({
    if (!identical(analysis_type, "cfa")) return(NULL)
    result <- fit_result()$parcel_result %||% list(enabled = FALSE)
    if (!isTRUE(result$enabled)) return(NULL)
    language <- ui_language()
    tr <- function(en, ko = en) statedu_localized_text(language, en, ko)
    if (!isTRUE(result$available)) return(tagList(
      tags$h5(tr("Parcel-plan safety review", "Parcel 계획 안전성 점검")),
      result_note_paragraph(class = "structural-result-note", sprintf(tr("Preview unavailable: %s", "미리보기 생성 불가: %s"), structural_canvas_parcel_text(result$reason, language)))
    ))
    allocation <- result$allocation
    summary <- result$summary
    allocation$Loading <- vapply(allocation$Loading, format_decimal3, character(1))
    summary[["Mean absolute loading"]] <- vapply(summary[["Mean absolute loading"]], format_decimal3, character(1))
    names(allocation) <- c(tr("Parcel", "Parcel"), tr("Indicator", "문항"), tr("Standardized loading", "표준화 적재량"))
    names(summary) <- c(tr("Parcel", "Parcel"), tr("Mean absolute loading", "평균 절대 적재량"), tr("Items", "문항"))
    attr(allocation, "result_user_columns") <- seq_along(allocation)
    attr(summary, "result_user_columns") <- seq_along(summary)
    tagList(
      tags$h5(tr("Parcel-plan safety review", "Parcel 계획 안전성 점검")),
      tags$p(tags$b(tr("Recorded purpose: ", "기록된 목적: ")), if (nzchar(result$purpose %||% "")) result$purpose else tr("Not recorded", "기록 없음")),
      result_note_paragraph(class = "structural-result-note", sprintf(tr("Status: %s. %s", "상태: %s. %s"), structural_canvas_parcel_text(result$status, language), structural_canvas_parcel_warning(result$warning, language))),
      if (isTRUE(result$applied)) result_note_paragraph(class = "structural-result-note", sprintf(tr("The result model fitted %s as a higher-order factor with lower-order item-level factors: %s.", "결과 모형은 %s를 상위요인으로 두고 %s 하위 item-level 요인을 적합했습니다."), result$construct, paste(result$item_level_constructs %||% character(0), collapse = ", "))),
      if (identical(result$applied, FALSE) && nzchar(result$fit_error %||% "")) result_note_paragraph(class = "structural-result-note structural-result-warning", sprintf(tr("Item-level lower-order model fit failed: %s", "item-level 하위요인 모형 적합 실패: %s"), result$fit_error)),
      result_note_paragraph(class = "structural-result-note", sprintf(tr("Item-level minimum |loading| = %s; maximum absolute residual correlation = %s.", "문항수준 최소 |적재량| = %s; 최대 절대 잔차상관 = %s."), format_decimal3(result$min_loading), format_decimal3(result$max_residual_correlation))),
      tags$h6(tr("Allocation preview", "배정 미리보기")),
      structural_canvas_basic_html_table(allocation, class = "table table-striped table-bordered structural-parcel-allocation-table", language = ui_language()),
      tags$h6(tr("Parcel balance summary", "Parcel 균형 요약")),
      structural_canvas_basic_html_table(summary, class = "table table-striped table-bordered structural-parcel-summary-table", language = ui_language()),
      result_note_paragraph(class = "structural-result-note", tr("No parcel variables were created. The lower-order factors remain item-level representations using the original indicators. Review substantive item homogeneity, local dependence, and sensitivity to alternative allocations before interpretation.", "데이터셋에 parcel 변수는 생성되지 않았습니다. 하위요인은 원문항을 그대로 지표로 사용하는 item-level 표현입니다. 해석 전 이론적 동질성, 국소의존, 다른 배정 방식에 대한 민감도를 함께 검토해야 합니다."))
    )
  })
  output[[paste0(prefix, "_result_measurement_ci")]] <- renderUI({
    if (identical(analysis_type, "plssem")) {
      ci_table <- appendix_result_table("measurement_bootstrap")
      if (!is.data.frame(ci_table) || !nrow(ci_table)) return(NULL)
      value_columns <- setdiff(names(ci_table), c("Construct", "Construct type", "Indicator", "Mode"))
      language <- ui_language()
      tr <- function(en, ko) statedu_localized_text(language, en, ko)
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
      status_labels <- c("Adequate"="충분", "Insufficient"="불충분", "Pending"="진행 중", "Failed"="실패", "Canceled"="중단", "Not recorded"="기록 없음")
      display_status <- if (bootstrap_status %in% names(status_labels)) tr(bootstrap_status, unname(status_labels[[bootstrap_status]])) else bootstrap_status
      labels <- c("Common factor"="공통요인", Composite="합성변수", Unspecified="미지정", Reflective="반영형", Formative="형성형")
      for (column in intersect(c("Construct type", "Mode"), names(ci_table))) ci_table[[column]] <- vapply(as.character(ci_table[[column]]), function(value) {
        if (value %in% names(labels)) tr(value, unname(labels[[value]])) else value
      }, character(1), USE.NAMES = FALSE)
      if (!identical(normalize_app_language(language), "en")) names(ci_table) <- vapply(names(ci_table), function(header) {
        if (!grepl("^(Loading|Weight) ", header)) return(header)
        loading <- startsWith(header, "Loading ")
        prefix <- if (loading) tr("Outer loading", "외부 적재량") else tr("Outer weight", "외부 가중치")
        suffix <- sub("^(Loading|Weight) ", "", header)
        if (identical(suffix, "BH-adjusted p")) suffix <- tr("BH-adjusted p", "BH 보정 p")
        paste(prefix, suffix)
      }, character(1), USE.NAMES = FALSE)
      attr(ci_table, "result_user_columns") <- seq_along(ci_table)
      return(tagList(
        tags$h5(sprintf(tr("Supplementary Table %s: PLS measurement bootstrap", "표 %s 보조: PLS 측정모형 부트스트랩"), table_number("measurement"))),
        if (has_values) structural_canvas_basic_html_table(ci_table, class = "table table-striped table-bordered structural-pls-measurement-bootstrap-table", language = ui_language()),
        if (!inference_available) result_note_paragraph(class = "structural-result-note structural-result-warning",
          sprintf(tr("PLS bootstrap inference is unavailable (status: %s). Valid resamples: %s/%s; the minimum reporting ratio is %s%%. Outer-loading and weight bootstrap SE, CI, t, and p values are not reported.", "PLS 부트스트랩 추론을 사용할 수 없습니다(상태: %s). 유효 재표집: %s/%s회; 최소 보고 기준은 %s%%입니다. 외부 적재량·가중치의 부트스트랩 SE, CI, t, p를 보고하지 않습니다."), display_status, valid_n, requested_n, formatC(100 * minimum_ratio, format = "fg", digits = 3)),
          if (nzchar(failure_message)) paste0(" ", sprintf(tr("Detail: %s", "상세: %s"), failure_message))),
        if (inference_available) result_note_paragraph(class = "structural-result-note", tr("Percentile bootstrap CI, t, raw p values, and family-specific BH-adjusted p values for outer loadings and outer weights use only whole-draw contract-valid resamples.", "외부 적재량과 외부 가중치의 백분위수 부트스트랩 CI, t, 원 p값과 검정군별 BH 보정 p값은 전체 통계량 요건을 통과한 재표본만 사용합니다."))
      ))
    }

    ci_table <- appendix_result_table("measurement_ci")
    if (!nrow(ci_table)) return(NULL)
    language <- ui_language()
    tagList(
      tags$h5(sprintf(statedu_localized_text(language, "Supplementary Table %s: Measurement model 95%% confidence intervals", "표 %s 보조: 측정모형 95%% 신뢰구간"), table_number("measurement"))),
      structural_canvas_measurement_ci_html_table(ci_table, language)
    )
  })
  output[[paste0(prefix, "_result_common_method")]] <- renderUI({
    if (identical(analysis_type, "plssem")) return(NULL)
    bundle <- fit_result()
    if (!isTRUE(bundle$common_method_enabled)) return(NULL)
    table <- appendix_result_table("common_method")
    common_method <- bundle$common_method_result %||% list()
    language <- normalize_app_language(statedu_current_language(app_language_fn))
    ko <- identical(language, "ko")
    tr <- function(en, ko = en) statedu_localized_text(language, en, ko)
    if (!is.data.frame(table) || !nrow(table)) {
      return(tagList(
        tags$h5(tr("Common method bias diagnostics", "동일방법편의 진단")),
        result_note_paragraph(
          class = "structural-result-note",
          tr("Common method bias diagnostics were requested, but no displayable result could be computed for the current model.", "동일방법편의 진단을 요청했지만 현재 모형에서 표시할 결과를 계산하지 못했습니다. 모형 수렴, 식별성, 관측변수 수를 확인하십시오.")
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
    attr(conclusion_table, "result_user_columns") <- seq_along(conclusion_table)
    translate_values <- function(values, labels) {
      structural_canvas_common_method_lookup_label(values, labels, language)
    }
    if (nrow(comparison_table)) {
      numeric_columns <- names(comparison_table)[vapply(comparison_table, is.numeric, logical(1))]
      for (column in numeric_columns) comparison_table[[column]] <- vapply(comparison_table[[column]], format_decimal3, character(1))
      if ("Delta p" %in% names(comparison_table)) {
        comparison_table[["Delta p"]] <- vapply(suppressWarnings(as.numeric(comparison_table[["Delta p"]])), format_p, character(1))
      }
      {
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
        header_labels <- c(
          Comparison = "비교",
          `Delta chisq` = "Δχ²",
          `Delta df` = "Δdf",
          `Delta p` = "Δp",
          `Delta CFI` = "ΔCFI",
          `Delta RMSEA` = "ΔRMSEA",
          `Delta SRMR` = "ΔSRMR",
          Note = "해석 주의"
        )
        names(comparison_table) <- translate_values(names(comparison_table), header_labels)
      }
    }
    if (nrow(fit_table)) {
      numeric_columns <- names(fit_table)[vapply(fit_table, is.numeric, logical(1))]
      for (column in numeric_columns) fit_table[[column]] <- vapply(fit_table[[column]], format_decimal3, character(1))
      if ("p" %in% names(fit_table)) fit_table$p <- vapply(suppressWarnings(as.numeric(fit_table$p)), format_p, character(1))
      if ("Model" %in% names(fit_table)) {
        fit_table$Model <- translate_values(fit_table$Model, c(
          Research_model = "연구모형",
          Single_factor_CFA = "단일요인 CFA",
          Common_latent_factor = "공통잠재요인"
        ))
        names(fit_table)[names(fit_table) == "Model"] <- tr("Model", "모형")
      }
    }
    if (nrow(loading_change)) {
      numeric_columns <- names(loading_change)[vapply(loading_change, is.numeric, logical(1))]
      for (column in numeric_columns) loading_change[[column]] <- vapply(loading_change[[column]], format_decimal3, character(1))
      {
        header_labels <- c(
          Latent = "잠재변수",
          Indicator = "관측변수",
          `Baseline beta` = "기준 beta",
          `Method-adjusted beta` = "방법보정 beta",
          `Absolute change` = "절대 변화량",
          `Method factor beta` = "방법요인 beta"
        )
        names(loading_change) <- translate_values(names(loading_change), header_labels)
      }
    }
    attr(fit_table, "result_user_columns") <- seq_along(fit_table)
    attr(comparison_table, "result_user_columns") <- seq_along(comparison_table)
    attr(loading_change, "result_user_columns") <- seq_along(loading_change)
    tagList(
      tags$h5(tr("Common method bias diagnostics", "동일방법편의 진단")),
      if (nrow(conclusion_table)) tagList(
        tags$h6(tr("Conclusion", "판정 요약")),
        structural_canvas_basic_html_table(conclusion_table, class = "table table-striped table-bordered structural-common-method-conclusion-table", language = ui_language())
      ),
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-common-method-table", language = ui_language()),
      if (nrow(fit_table)) tagList(
        tags$h6(tr("Model fit comparison", "모형 적합도 비교")),
        structural_canvas_basic_html_table(fit_table, class = "table table-striped table-bordered structural-common-method-fit-table", language = ui_language())
      ),
      if (nrow(comparison_table)) tagList(
        tags$h6(tr("Model difference comparison", "모형 차이 비교")),
        structural_canvas_basic_html_table(comparison_table, class = "table table-striped table-bordered structural-common-method-comparison-table", language = ui_language())
      ),
      if (nrow(loading_change)) tagList(
        tags$h6(tr("Common latent factor loading changes", "공통잠재요인 적재량 변화")),
        structural_canvas_basic_html_table(loading_change, class = "table table-striped table-bordered structural-common-method-loading-table", language = ui_language())
      ),
      result_note_paragraph(
        class = "structural-result-note",
        tr("These diagnostics screen for common method bias. They should be reported as evidence for or against serious common-method concentration, not as proof that common method bias is absent.", "동일방법편의 진단은 편의 가능성을 점검하는 증거입니다. 편의가 없다는 증명으로 해석하지 말고, 심각한 동일방법 집중 여부를 판단하는 보조 근거로 보고하십시오.")
      )
    )
  })
  output[[paste0(prefix, "_result_mi_section")]] <- renderUI({
    if (identical(analysis_type, "plssem")) return(NULL)
    table <- appendix_result_table("mi")
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    div(class = "result-section regression-result-panel structural-appendix-result-panel structural-mi-result landscape-table-panel",
      h4(statedu_localized_text(ui_language(), "Modification indices (MI)", "수정지수(MI)")),
      uiOutput(paste0(prefix, "_result_mi"))
    )
  })
  structural_canvas_register_mi_render_outputs(
    output, prefix, fit_result, manuscript_result_table, app_language_fn
  )
  invisible(TRUE)
}
