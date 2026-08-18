structural_canvas_pls_quality_number <- function(values, direction = "single") {
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

structural_canvas_pls_diagnostic_number <- function(value) {
  formatted <- structural_canvas_pls_quality_number(value)
  if (nzchar(formatted)) formatted else "N/A"
}

structural_canvas_pls_quality_predictive_label <- function(bundle) {
  if (is.null(bundle$pls_predict_result)) return("Not executed")
  tables <- structural_canvas_pls_predict_tables(bundle$pls_predict_result)
  items <- tables$items
  if (!nrow(items) || !"Assessment" %in% names(items)) return("Executed; no comparable indicator metrics")
  assessments <- as.character(items$Assessment %||% character(0))
  paste0(
    sum(assessments == "PLS lower error", na.rm = TRUE),
    "/",
    length(assessments),
    " indicator metrics favor PLS over LM"
  )
}

structural_canvas_pls_ten_times_margin <- function(bundle) {
  fit <- bundle$fit %||% NULL
  diagnostics <- bundle$diagnostics %||% list()
  n <- suppressWarnings(as.numeric(diagnostics$n %||% nrow(fit$data %||% data.frame())))
  mm_variables <- fit$mmVariables %||% matrix(character(0), 0L, 0L)
  mm_variables <- as.matrix(mm_variables)
  indicator_max <- if (length(mm_variables)) max(colSums(!is.na(mm_variables) & nzchar(mm_variables)), na.rm = TRUE) else 0
  path_specs <- structural_canvas_pls_path_specs(diagnostics)
  antecedent_max <- if (nrow(path_specs)) max(table(path_specs$outcome), na.rm = TRUE) else 0
  denominator <- 10 * max(1, indicator_max, antecedent_max)
  if (!is.finite(n) || denominator <= 0) NA_real_ else n / denominator
}

structural_canvas_pls_approximate_fit_indices <- function(bundle, summary_fit = NULL) {
  fit <- bundle$fit %||% NULL
  if (is.null(fit) || is.null(fit$data) || is.null(fit$construct_scores)) {
    return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
  }
  snapshot <- bundle$snapshot %||% list()
  latent_nodes <- Filter(function(node) identical(node$role %||% "", "latent"), snapshot$nodes %||% list())
  reflective_constructs <- vapply(Filter(
    function(node) !identical(node$measurementMode %||% "reflective", "formative"),
    latent_nodes
  ), structural_canvas_name, character(1))
  summary_fit <- summary_fit %||% tryCatch(summary(fit), error = function(error) NULL)
  if (is.null(summary_fit)) return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
  mm <- as.data.frame(fit$mmMatrix %||% data.frame(), stringsAsFactors = FALSE)
  if (!all(c("construct", "measurement") %in% names(mm))) return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
  if (length(latent_nodes)) mm <- mm[as.character(mm$construct) %in% reflective_constructs, , drop = FALSE]
  loadings <- suppressWarnings(as.matrix(summary_fit$loadings %||% matrix(numeric(0), 0L, 0L)))
  scores <- suppressWarnings(as.matrix(fit$construct_scores))
  indicators <- intersect(as.character(mm$measurement), rownames(loadings))
  indicators <- intersect(indicators, names(fit$data))
  constructs <- intersect(as.character(mm$construct), colnames(loadings))
  constructs <- intersect(constructs, colnames(scores))
  mm <- mm[mm$measurement %in% indicators & mm$construct %in% constructs, , drop = FALSE]
  indicators <- unique(as.character(mm$measurement))
  if (length(indicators) < 2L || nrow(mm) < 2L) return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
  observed <- tryCatch(stats::cor(fit$data[indicators], use = "pairwise.complete.obs"), error = function(error) NULL)
  construct_cor <- tryCatch(stats::cor(scores[, constructs, drop = FALSE], use = "pairwise.complete.obs"), error = function(error) NULL)
  if (is.null(observed) || is.null(construct_cor)) return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
  implied <- diag(1, length(indicators), length(indicators))
  dimnames(implied) <- list(indicators, indicators)
  construct_for <- stats::setNames(as.character(mm$construct), as.character(mm$measurement))
  for (i in seq_along(indicators)) {
    for (j in seq_along(indicators)) {
      if (i == j) next
      first <- indicators[[i]]
      second <- indicators[[j]]
      first_construct <- construct_for[[first]]
      second_construct <- construct_for[[second]]
      first_loading <- structural_canvas_pls_matrix_cell(loadings, first, first_construct)
      second_loading <- structural_canvas_pls_matrix_cell(loadings, second, second_construct)
      phi <- if (identical(first_construct, second_construct)) 1 else structural_canvas_pls_matrix_cell(construct_cor, first_construct, second_construct)
      implied[first, second] <- if (all(is.finite(c(first_loading, second_loading, phi)))) first_loading * second_loading * phi else NA_real_
    }
  }
  lower <- lower.tri(observed)
  residuals <- suppressWarnings(as.numeric(observed[lower] - implied[lower]))
  residuals <- residuals[is.finite(residuals)]
  null_residuals <- suppressWarnings(as.numeric(observed[lower]))
  null_residuals <- null_residuals[is.finite(null_residuals)]
  if (!length(residuals)) return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
  d_uls <- sum(residuals^2, na.rm = TRUE)
  d_null <- sum(null_residuals^2, na.rm = TRUE)
  d_g <- tryCatch({
    observed_pd <- observed[indicators, indicators, drop = FALSE]
    implied_pd <- implied[indicators, indicators, drop = FALSE]
    eigen_values <- Re(eigen(solve(implied_pd, observed_pd), only.values = TRUE)$values)
    if (any(!is.finite(eigen_values) | eigen_values <= 0)) NA_real_ else sqrt(sum(log(eigen_values)^2))
  }, error = function(error) NA_real_)
  c(
    srmr = sqrt(mean(residuals^2, na.rm = TRUE)),
    d_g = d_g,
    d_uls = d_uls,
    nfi = if (is.finite(d_null) && d_null > 0) 1 - d_uls / d_null else NA_real_
  )
}

structural_canvas_pls_diagnostic_fit <- function(bundle, estimator) {
  if (is.null(bundle) || is.null(bundle$fit) || !inherits(bundle$fit, "pls_model")) return(NULL)
  estimator <- toupper(as.character(estimator %||% "PLS"))
  current_estimator <- toupper(as.character(bundle$diagnostics$estimator %||% bundle$estimator %||% "PLS"))
  if (identical(current_estimator, "PLSC")) current_estimator <- "PLSC"
  if (identical(estimator, current_estimator)) return(bundle$fit)
  if (identical(estimator, "PLSC")) {
    return(tryCatch(seminr::PLSc(bundle$fit), error = function(error) NULL))
  }
  snapshot <- bundle$snapshot %||% list()
  data <- bundle$analysis_data %||% bundle$fit$rawdata %||% bundle$fit$data %||% NULL
  if (is.null(data)) return(NULL)
  latents <- Filter(function(node) identical(node$role %||% "", "latent"), snapshot$nodes %||% list())
  edges <- snapshot$edges %||% list()
  tryCatch(
    structural_canvas_run_pls_analysis(snapshot, as.data.frame(data, check.names = FALSE), latents, edges, estimator = "PLS")$fit,
    error = function(error) NULL
  )
}

structural_canvas_pls_fit_diagnostics_table <- function(bundle) {
  if (is.null(bundle) || is.null(bundle$fit) || !inherits(bundle$fit, "pls_model")) return(data.frame())
  specification <- structural_canvas_construct_specification(bundle$snapshot %||% list())
  has_formative <- nrow(specification) && any(specification$measurement_mode == "formative")
  rows <- lapply(c("PLS", "PLSC"), function(estimator) {
    applicable <- !(identical(estimator, "PLSC") && has_formative)
    fit <- if (applicable) structural_canvas_pls_diagnostic_fit(bundle, estimator) else NULL
    diagnostic_bundle <- bundle
    diagnostic_bundle$fit <- fit
    diagnostic_bundle$estimator <- estimator
    summary_fit <- tryCatch(summary(fit), error = function(error) NULL)
    values <- structural_canvas_pls_approximate_fit_indices(diagnostic_bundle, summary_fit)
    data.frame(
      Model = if (identical(estimator, "PLSC")) "plsc" else "pls",
      srmr = structural_canvas_pls_diagnostic_number(values[["srmr"]]),
      d_G = structural_canvas_pls_diagnostic_number(values[["d_g"]]),
      d_ULS = structural_canvas_pls_diagnostic_number(values[["d_uls"]]),
      Basis = if (!applicable) "Not applicable: formative construct present" else if (has_formative) "Reflective measurement subset" else "All indicators",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_pls_quality_status <- function(item, value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  unavailable <- !nzchar(as.character(value %||% "")) || identical(value, "Not executed")
  if (unavailable) return("Not assessed")
  if (identical(item, "PLS algorithm iterations")) return(if (is.finite(numeric_value) && numeric_value <= 300) "OK" else "Review")
  if (identical(item, "Final weight difference")) return(if (is.finite(numeric_value) && numeric_value <= 1e-6) "OK" else "Review")
  if (identical(item, "Missing-data method")) return("OK")
  if (item %in% c("Approx PLS SRMR", "Approx d_G", "Approx d_ULS", "Approx NFI", "10-times rule margin")) {
    return(if (is.finite(numeric_value)) "Descriptive only" else "Not assessed")
  }
  if (identical(item, "Min outer loading")) return(if (is.finite(numeric_value) && numeric_value >= .40) "Reference only" else "Review")
  if (identical(item, "Min rhoC")) return(if (is.finite(numeric_value) && numeric_value >= .70) "Reference only" else "Review")
  if (identical(item, "Min AVE")) return(if (is.finite(numeric_value) && numeric_value >= .50) "Reference only" else "Review")
  if (identical(item, "Max HTMT")) return(if (is.finite(numeric_value) && numeric_value < .85) "Reference only" else "Review")
  if (identical(item, "Max item VIF")) return(if (is.finite(numeric_value) && numeric_value <= 5) "Reference only" else "Review")
  if (identical(item, "Max inner VIF")) return(if (is.finite(numeric_value) && numeric_value <= 5) "Reference only" else "Review")
  if (identical(item, "Max full collinearity VIF")) return(if (is.finite(numeric_value)) "Screen only" else "Not assessed")
  if (identical(item, "Min endogenous R2")) return(if (is.finite(numeric_value)) "Descriptive only" else "Not assessed")
  if (identical(item, "Max f2")) return(if (is.finite(numeric_value)) "Descriptive only" else "Not assessed")
  if (identical(item, "Min Q2")) return(if (!is.finite(numeric_value)) "Not assessed" else if (numeric_value > 0) "Descriptive only" else "Review")
  if (identical(item, "PLSpredict summary")) {
    matches <- regmatches(value, regexec("^([0-9]+)/([0-9]+)", value))[[1L]]
    if (length(matches) == 3L) {
      favored <- suppressWarnings(as.integer(matches[[2L]]))
      total <- suppressWarnings(as.integer(matches[[3L]]))
      return(if (is.finite(favored) && is.finite(total) && total > 0L && favored > 0L) "OK" else "Review")
    }
    return("Not assessed")
  }
  "Not assessed"
}

structural_canvas_pls_quality_rows <- function(bundle) {
  if (is.null(bundle) || is.null(bundle$fit) || !inherits(bundle$fit, "pls_model")) {
    return(data.frame(Item = character(0), Value = character(0), Status = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  summary_fit <- tryCatch(summary(bundle$fit), error = function(error) NULL)
  if (is.null(summary_fit)) {
    return(data.frame(Item = character(0), Value = character(0), Status = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  reliability <- as.data.frame(summary_fit$reliability %||% data.frame(), check.names = FALSE)
  loadings <- suppressWarnings(abs(as.matrix(summary_fit$loadings %||% matrix(numeric(0), 0L, 0L))))
  if (length(loadings)) loadings[loadings == 0] <- NA_real_
  assigned_loadings <- if (length(loadings)) apply(loadings, 1L, function(row) {
    row <- row[is.finite(row)]
    if (!length(row)) NA_real_ else max(row, na.rm = TRUE)
  }) else numeric(0)
  item_vif <- unlist(summary_fit$validity$vif_items %||% list(), use.names = FALSE)
  inner_vif <- unlist(summary_fit$vif_antecedents %||% list(), use.names = FALSE)
  htmt <- suppressWarnings(as.numeric(as.matrix(summary_fit$validity$htmt %||% matrix(numeric(0), 0L, 0L))))
  htmt <- htmt[is.finite(htmt)]
  paths <- suppressWarnings(as.matrix(summary_fit$paths %||% matrix(numeric(0), 0L, 0L)))
  r2 <- if (length(paths) && "R^2" %in% rownames(paths)) suppressWarnings(as.numeric(paths["R^2", ])) else numeric(0)
  f_square <- suppressWarnings(as.numeric(as.matrix(summary_fit$fSquare %||% matrix(numeric(0), 0L, 0L))))
  f_square <- f_square[is.finite(f_square) & f_square > 0]
  q2 <- suppressWarnings(as.numeric((bundle$diagnostics$q2 %||% data.frame())$Q2))
  q2 <- q2[is.finite(q2)]
  full_collinearity_vif <- structural_canvas_quality_full_collinearity_vif(bundle$fit$construct_scores %||% NULL)
  ten_times_margin <- structural_canvas_pls_ten_times_margin(bundle)
  approximate_fit <- structural_canvas_pls_approximate_fit_indices(bundle, summary_fit)
  missing <- summary_fit$missing_data$method %||% "not recorded"
  weight_diff <- bundle$fit$weightDiff %||% NA_real_
  items <- c(
    "PLS algorithm iterations",
    "Final weight difference",
    "Missing-data method",
    "Approx PLS SRMR",
    "Approx d_G",
    "Approx d_ULS",
    "Approx NFI",
    "10-times rule margin",
    "Min outer loading",
    "Min rhoC",
    "Min AVE",
    "Max HTMT",
    "Max item VIF",
    "Max inner VIF",
    "Max full collinearity VIF",
    "Min endogenous R2",
    "Max f2",
    "Min Q2",
    "PLSpredict summary"
  )
  displayed_values <- c(
    as.character(summary_fit$iterations %||% bundle$fit$iterations %||% ""),
    structural_canvas_pls_quality_number(weight_diff),
    as.character(missing),
    structural_canvas_pls_quality_number(approximate_fit[["srmr"]]),
    structural_canvas_pls_quality_number(approximate_fit[["d_g"]]),
    structural_canvas_pls_quality_number(approximate_fit[["d_uls"]]),
    structural_canvas_pls_quality_number(approximate_fit[["nfi"]]),
    structural_canvas_pls_quality_number(ten_times_margin),
    structural_canvas_pls_quality_number(assigned_loadings, "min"),
    structural_canvas_pls_quality_number(reliability$rhoC, "min"),
    structural_canvas_pls_quality_number(reliability$AVE, "min"),
    structural_canvas_pls_quality_number(htmt, "max"),
    structural_canvas_pls_quality_number(item_vif, "max"),
    structural_canvas_pls_quality_number(inner_vif, "max"),
    structural_canvas_pls_quality_number(full_collinearity_vif, "max"),
    structural_canvas_pls_quality_number(r2, "min"),
    structural_canvas_pls_quality_number(f_square, "max"),
    structural_canvas_pls_quality_number(q2, "min"),
    structural_canvas_pls_quality_predictive_label(bundle)
  )
  rows <- data.frame(
    Item = items,
    Value = displayed_values,
    Status = mapply(structural_canvas_pls_quality_status, items, displayed_values, USE.NAMES = FALSE),
    Guidance = c(
      "Algorithm diagnostic; inspect unusually high iteration counts.",
      "Smaller values indicate stable outer-weight convergence.",
      "Report this because PLS does not use lavaan FIML/pairwise options.",
      "Locally reconstructed reflective-measurement SRMR; report descriptively without an accept/reject cutoff.",
      "Locally reconstructed geodesic discrepancy; report descriptively because no universal cutoff applies.",
      "Locally reconstructed squared Euclidean discrepancy; report descriptively because no universal cutoff applies.",
      "Locally reconstructed NFI against an independence baseline; report descriptively without an accept/reject cutoff.",
      "Historically imprecise 10-times heuristic; do not use it to justify sample size. Prefer a priori power or model-specific simulation.",
      "Outer loadings are descriptive evidence, not automatic deletion rules; review low values with content validity, cross-loadings, uncertainty, and prespecified theory.",
      "The .70 rhoC value is a descriptive reliability reference, not a universal scale-acceptance rule.",
      "The .50 AVE value is a descriptive convergent-validity reference, not an automatic construct-acceptance or indicator-deletion rule.",
      "HTMT references do not establish discriminant validity; interpret with intervals, cross-loadings, construct correlations, theory, and competing measurement models.",
      "Item VIF cutoffs are descriptive references. High values flag redundant indicators or unstable formative weights; low values do not establish measurement quality.",
      "Inner VIF cutoffs are descriptive references. Review coefficient sign/magnitude changes, suppression, bootstrap instability, and theoretical overlap across predictors.",
      "Exploratory full-collinearity screen only; values above 3.3 are nonspecific and lower values do not rule out common-method bias.",
      "Report R2 descriptively for each endogenous construct; no universal value establishes adequate explanation or causal importance.",
      "The .02/.15/.35 f2 ranges are descriptive anchors, not universal importance thresholds.",
      "Q2 compares omission prediction with a baseline; values above 0 remain descriptive and do not establish strong out-of-sample prediction.",
      "Out-of-sample prediction is strongest when PLS error is lower than LM."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  formative <- structural_canvas_formative_content_validity_rows(
    bundle$snapshot %||% list(), bundle$redundancy_result %||% NULL, bundle$redundancy_construct %||% NULL
  )
  if (nrow(formative)) rows <- rbind(rows, data.frame(
    Item = paste0("Formative evidence: ", formative$Construct),
    Value = paste0(
      "domain=", ifelse(nzchar(formative$`Domain definition`), "recorded", "missing"),
      "; indicator rationale=", ifelse(nzchar(formative$`Indicator inclusion rationale`), "recorded", "missing"),
      "; content-validity procedure/source=", ifelse(nzchar(formative$`Content-validity procedure/source`), "recorded", "missing"),
      "; redundancy=", formative$`Redundancy evidence`
    ),
    Status = ifelse(formative$Status == "Documented", "OK", "Review"),
    Guidance = formative$Guidance,
    stringsAsFactors = FALSE, check.names = FALSE
  ))
  rows
}

structural_canvas_pls_quality_status_summary <- function(rows) {
  if (!nrow(rows) || !"Status" %in% names(rows)) return("Quality status: not assessed.")
  counts <- table(factor(rows$Status, levels = c("OK", "Review", "Reference only", "Descriptive only", "Screen only", "Not assessed")))
  paste0(
    "Quality status: OK=", counts[["OK"]],
    "; Review=", counts[["Review"]],
    "; Reference only=", counts[["Reference only"]],
    "; Descriptive only=", counts[["Descriptive only"]],
    "; Screen only=", counts[["Screen only"]],
    "; Not assessed=", counts[["Not assessed"]],
    "."
  )
}

structural_canvas_pls_quality_priority <- function(items) {
  critical <- c("PLS algorithm iterations", "Final weight difference")
  major <- c("Min outer loading", "Min rhoC", "Min AVE", "Max HTMT", "Max item VIF", "Max inner VIF", "Max full collinearity VIF", "Min Q2")
  ifelse(items %in% critical, "Critical", ifelse(items %in% major | grepl("^Formative evidence:", items), "Major", "Advisory"))
}

structural_canvas_pls_quality_action <- function(priority) {
  ifelse(
    priority == "Critical", "Resolve before reporting",
    ifelse(priority == "Major", "Resolve or justify", "Document limitation")
  )
}

structural_canvas_pls_quality_review_rows <- function(rows) {
  if (!nrow(rows) || !"Status" %in% names(rows)) {
    return(data.frame(Priority = character(0), Action = character(0), Item = character(0), Value = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  review <- rows[rows$Status == "Review", c("Item", "Value", "Guidance"), drop = FALSE]
  review$Priority <- structural_canvas_pls_quality_priority(review$Item)
  review$Action <- structural_canvas_pls_quality_action(review$Priority)
  priority_order <- c(Critical = 1L, Major = 2L, Advisory = 3L)
  review <- review[order(priority_order[review$Priority], review$Item), c("Priority", "Action", "Item", "Value", "Guidance"), drop = FALSE]
  rownames(review) <- NULL
  review
}

structural_canvas_pls_quality_reporting_readiness <- function(rows) {
  review <- structural_canvas_pls_quality_review_rows(rows)
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

structural_canvas_pls_quality_result_ui <- function(bundle, language = statedu_initial_language()) {
  rows <- structural_canvas_pls_quality_rows(bundle)
  if (!nrow(rows)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  review_rows <- structural_canvas_pls_quality_review_rows(rows)
  summary <- structural_canvas_quality_status_summary_display(rows, ko)
  readiness <- structural_canvas_quality_reporting_readiness_display(review_rows, rows, ko)
  display_rows <- structural_canvas_quality_display_rows(rows, ko)
  display_review_rows <- structural_canvas_quality_display_rows(review_rows, ko)
  div(
    class = "result-section regression-result-panel structural-pls-quality-result",
    h4(if (ko) "PLS-SEM 품질 체크리스트" else "PLS-SEM quality checklist"),
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
        "이 체크리스트는 PLS-SEM의 표본 적절성, 반영형 모형 근사 적합도 진단, 측정모형, 공선성, 공통방법편향 점검, 설명력, Q² 예측 관련성, 예측 품질의 보고 조건을 요약합니다."
      } else {
        "This checklist summarizes PLS-SEM sample adequacy, approximate reflective-model fit diagnostics, measurement, collinearity, common-method-bias screens, explanatory-power, Q2 predictive-relevance, and predictive-quality boundary conditions for reporting and review."
      }
    )
  )
}

structural_canvas_pls_predict_result_ui <- function(bundle, language = statedu_initial_language()) {
  if (is.null(bundle) || is.null(bundle$pls_predict_result)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  tables <- structural_canvas_pls_predict_tables(bundle$pls_predict_result)
  item_table <- tables$items
  construct_table <- tables$constructs
  for (table_name in c("item_table", "construct_table")) {
    table <- get(table_name)
    if (nrow(table)) {
      for (name in names(table)) {
        if (is.numeric(table[[name]])) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
      }
      assign(table_name, table)
    }
  }
  div(class = "result-section regression-result-panel structural-pls-predict-result",
    h4(if (ko) "PLSpredict 예측 진단" else "PLSpredict predictive assessment"),
    tags$p(class = "structural-result-note", if (ko) {
      paste0("Direct Antecedents 방식, ", bundle$pls_predict_result$folds, "-fold, 독립 반복 ", bundle$pls_predict_result$reps, "회(seed = ", bundle$pls_predict_result$seed, ") 기준입니다. PLS - LM은 반복 평균 차이, SD는 분할 간 변동성, PLS lower %는 선형모형보다 오차가 낮았던 반복 비율입니다. 단일 반복 결과는 예측력 결론에 충분하지 않습니다.")
    } else {
      paste0("Direct Antecedents scheme, ", bundle$pls_predict_result$folds, "-fold, ", bundle$pls_predict_result$reps, " independent repetitions (seed = ", bundle$pls_predict_result$seed, "). PLS - LM is the mean difference, SD reflects split-to-split variability, and PLS lower % is the proportion of repetitions favoring PLS over the linear-model benchmark. One repetition is insufficient for a predictive-performance claim.")
    }),
    if (nrow(item_table)) tagList(
      tags$h5(if (ko) "Indicator별 out-of-sample 예측오차" else "Indicator-level out-of-sample prediction error"),
      structural_canvas_basic_html_table(item_table)
    ),
    if (nrow(construct_table)) tagList(
      tags$h5(if (ko) "Construct-level prediction error" else "Construct-level prediction error"),
      structural_canvas_basic_html_table(construct_table)
    )
  )
}

structural_canvas_register_fit_diagnostic_outputs <- function(output, prefix, analysis_type, fit_result, result_table, dataset_fn, app_language_fn) {
output[[paste0(prefix, "_result_fit")]] <- renderUI({
  structural_canvas_fit_table_result_ui(fit_result(), result_table("fit"))
})
if (identical(analysis_type, "plssem")) {
  pls_fit_diagnostics_content <- function() {
    table <- structural_canvas_pls_fit_diagnostics_table(fit_result())
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    display <- table
    names(display)[names(display) == "Model"] <- ""
    tagList(
      structural_canvas_basic_html_table(display, class = "table table-striped table-bordered structural-pls-fit-diagnostics-table"),
      tags$p(class = "structural-result-note", if (ko) "SRMR, d_G, d_ULS는 관측 지표상관과 모형함의 지표상관 사이의 근사 불일치 진단입니다. 혼합모형에서는 반영형 측정 부분만 계산하며, 형성형 구성개념이 포함된 모형의 PLSc 행은 적용되지 않습니다." else "SRMR, d_G, and d_ULS are approximate discrepancy diagnostics between observed and model-implied indicator correlations. For mixed models they are calculated on the reflective measurement subset; PLSc is not applicable when a formative construct is present.")
    )
  }
  pls_fit_diagnostics_ui <- function() pls_fit_diagnostics_content()
  output[[paste0(prefix, "_result_pls_fit_diagnostics_section")]] <- renderUI({
    content <- pls_fit_diagnostics_content()
    if (is.null(content)) return(NULL)
    div(class = "result-section regression-result-panel structural-pls-fit-diagnostics-result", content)
  })
  output[[paste0(prefix, "_result_pls_fit_diagnostics")]] <- renderUI(pls_fit_diagnostics_ui())
  output[[paste0(prefix, "_result_pls_fit_diagnostics_inline")]] <- renderUI(pls_fit_diagnostics_ui())
  output[[paste0(prefix, "_result_fit_guidance")]] <- renderUI({
    table <- result_table("fit_guide")
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    tagList(
      tags$h5(if (ko) "표 3 보조: 구조효과 가이드 지표" else "Supplementary Table 3: Structural effect guide indices"),
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-pls-fit-guide-table"),
      tags$p(class = "structural-result-note", if (ko) "f²와 q² 크기 등급은 관행적 기술 표지이며 보편적 중요도 또는 예측력 합격선이 아닙니다. R²·Q²·f²는 분야 맥락, 불확실성, 실질적 중요성과 반복 PLSpredict 기준모형 비교를 함께 해석하십시오. Inner VIF는 단독 합격판정이 아니며 계수 부호·크기 변화, 억제효과와 bootstrap 안정성을 함께 점검하십시오." else "f2/q2 size labels are conventional descriptive anchors, not universal importance or predictive-performance thresholds. Interpret R2, Q2, and f2 with domain context, uncertainty, practical importance, and repeated PLSpredict benchmark comparisons. Inner VIF is not a standalone pass criterion; also inspect coefficient sign/magnitude changes, suppression, and bootstrap stability.")
    )
  })
  output[[paste0(prefix, "_result_fit_bootstrap")]] <- renderUI({
    table <- result_table("fit_bootstrap")
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    value_columns <- setdiff(names(table), c("Effect", "Outcome", "Predictor"))
    if (!length(value_columns) || !any(nzchar(as.character(unlist(table[value_columns], use.names = FALSE))))) return(NULL)
    ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
    bootstrap <- fit_result()$pls_bootstrap_result %||% list()
    valid_n <- suppressWarnings(as.integer(bootstrap$nboot %||% 0L))
    requested_n <- suppressWarnings(as.integer(bootstrap$requested_nboot %||% 0L))
    timeout_n <- suppressWarnings(as.integer(bootstrap$timeout_failures %||% 0L))
    estimation_n <- suppressWarnings(as.integer(bootstrap$estimation_failures %||% max(0L, requested_n - valid_n - timeout_n)))
    tagList(
      tags$h5(if (ko) "표 3 보조: PLS 부트스트랩 경로 추론" else "Supplementary Table 3: PLS bootstrap path inference"),
      structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-pls-fit-bootstrap-table"),
      if (requested_n > 0L) tags$p(class = "structural-result-note", if (ko) paste0("유효 재표집: ", valid_n, "/", requested_n, "회 (시간 제한 ", timeout_n, ", 추정 실패 ", estimation_n, ").") else paste0("Valid resamples: ", valid_n, "/", requested_n, " (timeouts ", timeout_n, ", estimation failures ", estimation_n, ").")),
      tags$p(class = "structural-result-note", if (ko) "직접경로, 간접효과, 총효과의 percentile bootstrap CI, t, p 값입니다. seminr가 반환한 항목만 표시됩니다." else "Percentile bootstrap CI, t, and p values are shown when seminr returns them.")
    )
  })
  output[[paste0(prefix, "_result_pls_quality")]] <- renderUI({
    structural_canvas_pls_quality_result_ui(fit_result(), statedu_current_language(app_language_fn))
  })
  output[[paste0(prefix, "_result_pls_predict")]] <- renderUI({
    structural_canvas_pls_predict_result_ui(fit_result(), statedu_current_language(app_language_fn))
  })
  return(invisible(TRUE))
}
output[[paste0(prefix, "_result_lavaan_quality")]] <- renderUI({
  structural_canvas_lavaan_quality_result_ui(fit_result(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_identification")]] <- renderUI({
  structural_canvas_identification_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_normality")]] <- renderUI({
  structural_canvas_normality_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_risk_diagnostics")]] <- renderUI({
  structural_canvas_risk_diagnostics_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_missing_outliers")]] <- renderUI({
  structural_canvas_missing_outliers_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_fit_difference")]] <- renderUI({
  structural_canvas_fit_difference_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_invariance")]] <- renderUI({
  structural_canvas_invariance_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_fit_guidance")]] <- renderUI({
  structural_canvas_fit_guidance_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_rmsea_tests")]] <- renderUI({
  structural_canvas_rmsea_tests_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_information_criteria")]] <- renderUI({
  structural_canvas_information_criteria_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_bollen_stine")]] <- renderUI({
  structural_canvas_bollen_stine_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_heywood")]] <- renderUI({
  structural_canvas_heywood_result_ui(fit_result(), dataset_fn(), prefix, analysis_type, statedu_current_language(app_language_fn))
})
  invisible(TRUE)
}
