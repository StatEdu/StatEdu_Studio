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

structural_canvas_pls_matrix_fit_indices <- function(observed, implied) {
  observed <- suppressWarnings(as.matrix(observed))
  implied <- suppressWarnings(as.matrix(implied))
  unavailable <- c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_)
  if (!length(observed) || !identical(dim(observed), dim(implied)) || nrow(observed) < 2L) return(unavailable)
  if (any(!is.finite(observed)) || any(!is.finite(implied))) return(unavailable)
  observed <- tryCatch(suppressWarnings(stats::cov2cor(observed)), error = function(error) NULL)
  implied <- tryCatch(suppressWarnings(stats::cov2cor(implied)), error = function(error) NULL)
  if (is.null(observed) || is.null(implied)) return(unavailable)
  if (any(!is.finite(observed)) || any(!is.finite(implied))) return(unavailable)
  difference <- observed - implied
  lower_diagonal <- lower.tri(difference, diag = TRUE)
  d_uls <- 0.5 * sum(difference^2)
  d_g <- tryCatch({
    eigen_values <- Re(eigen(solve(observed, implied), only.values = TRUE)$values)
    if (any(!is.finite(eigen_values) | eigen_values <= 0)) NA_real_ else 0.5 * sum(log10(eigen_values)^2)
  }, error = function(error) NA_real_)
  d_ml <- function(sample_matrix, fitted_matrix) {
    tryCatch({
      ratio <- solve(fitted_matrix, sample_matrix)
      determinant <- determinant(ratio, logarithm = TRUE)
      value <- sum(diag(ratio)) - as.numeric(determinant$modulus) - nrow(sample_matrix)
      if (determinant$sign <= 0 || !is.finite(value)) NA_real_ else value
    }, error = function(error) NA_real_)
  }
  fitted_ml <- d_ml(observed, implied)
  null_ml <- d_ml(observed, diag(nrow(observed)))
  c(
    srmr = sqrt(sum(difference[lower_diagonal]^2) / sum(lower_diagonal)),
    d_g = d_g,
    d_uls = d_uls,
    nfi = if (is.finite(null_ml) && null_ml > 0 && is.finite(fitted_ml)) (null_ml - fitted_ml) / null_ml else NA_real_
  )
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
  summary_fit <- summary_fit %||% tryCatch(
    if (inherits(fit, "pls_model")) structural_canvas_pls_summary(fit) else summary(fit),
    error = function(error) NULL
  )
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
  estimator <- toupper(as.character(bundle$estimator %||% bundle$diagnostics$estimator %||% "PLS"))
  if (identical(estimator, "PLSC")) {
    common_factors <- intersect(as.character(fit$statedu_common_factor_constructs %||% character(0)), constructs)
    rho_a <- tryCatch(seminr:::rho_A(fit, constructs), error = function(error) NULL)
    if (is.null(rho_a) || !length(common_factors)) return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
    rho_a <- suppressWarnings(as.numeric(rho_a[constructs, 1L]))
    names(rho_a) <- constructs
    rho_a[!constructs %in% common_factors] <- 1
    if (any(!is.finite(rho_a) | rho_a <= 0)) return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
    adjustment <- sqrt(outer(rho_a, rho_a))
    diag(adjustment) <- 1
    construct_cor <- construct_cor / adjustment
    if (any(!is.finite(construct_cor))) return(c(srmr = NA_real_, d_g = NA_real_, d_uls = NA_real_, nfi = NA_real_))
  }
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
  structural_canvas_pls_matrix_fit_indices(observed, implied)
}

structural_canvas_pls_diagnostic_fit <- function(bundle, estimator) {
  if (is.null(bundle) || is.null(bundle$fit) || !inherits(bundle$fit, "pls_model")) return(NULL)
  estimator <- toupper(as.character(estimator %||% "PLS"))
  current_estimator <- toupper(as.character(bundle$diagnostics$estimator %||% bundle$estimator %||% "PLS"))
  if (identical(current_estimator, "PLSC")) current_estimator <- "PLSC"
  if (identical(estimator, current_estimator)) return(bundle$fit)
  if (identical(estimator, "PLSC")) {
    selection <- structural_canvas_select_pls_estimator(bundle$snapshot %||% list(), "PLSC")
    return(tryCatch(structural_canvas_apply_plsc(bundle$fit, selection$common_factors), error = function(error) NULL))
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
  has_common_factor <- nrow(specification) && any(specification$construct_type == "commonFactor" & specification$measurement_mode == "reflective")
  has_composite <- nrow(specification) && any(specification$construct_type == "composite")
  rows <- lapply(c("PLS", "PLSC"), function(estimator) {
    applicable <- !(identical(estimator, "PLSC") && !has_common_factor)
    fit <- if (applicable) structural_canvas_pls_diagnostic_fit(bundle, estimator) else NULL
    diagnostic_bundle <- bundle
    diagnostic_bundle$fit <- fit
    diagnostic_bundle$estimator <- estimator
    summary_fit <- tryCatch(
      if (inherits(fit, "pls_model")) structural_canvas_pls_summary(fit) else summary(fit),
      error = function(error) NULL
    )
    values <- structural_canvas_pls_approximate_fit_indices(diagnostic_bundle, summary_fit)
    data.frame(
      Model = if (identical(estimator, "PLSC")) "plsc" else "pls",
      Fit = "saturated",
      srmr = structural_canvas_pls_diagnostic_number(values[["srmr"]]),
      d_G = structural_canvas_pls_diagnostic_number(values[["d_g"]]),
      d_ULS = structural_canvas_pls_diagnostic_number(values[["d_uls"]]),
      Basis = if (!applicable) {
        "Not applicable: no reflective common factor"
      } else if (identical(estimator, "PLSC") && has_composite) {
        "Mixed model: common factors corrected; composites uncorrected"
      } else if (has_formative) {
        "Reflective measurement subset"
      } else {
        "All indicators"
      },
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
  if (identical(item, "PLSpredict summary")) {
    matches <- regmatches(value, regexec("^([0-9]+)/([0-9]+)", value))[[1L]]
    if (length(matches) == 3L) {
      total <- suppressWarnings(as.integer(matches[[3L]]))
      return(if (is.finite(total) && total > 0L) "Descriptive only" else "Not assessed")
    }
    return("Not assessed")
  }
  "Not assessed"
}

structural_canvas_pls_quality_rows <- function(bundle) {
  if (is.null(bundle) || is.null(bundle$fit) || !inherits(bundle$fit, "pls_model")) {
    return(data.frame(Item = character(0), Value = character(0), Status = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  summary_fit <- tryCatch(structural_canvas_pls_summary(bundle$fit), error = function(error) NULL)
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
  f_square_result <- tryCatch(
    structural_canvas_pls_f_square_for_reporting(bundle, summary_fit),
    error = function(error) list(
      values = matrix(numeric(0), 0L, 0L),
      complete = FALSE,
      failures = data.frame(Predictor = "", Outcome = "", Reason = conditionMessage(error), stringsAsFactors = FALSE)
    )
  )
  f_square <- suppressWarnings(as.numeric(as.matrix(f_square_result$values %||% matrix(numeric(0), 0L, 0L))))
  f_square <- f_square[is.finite(f_square) & f_square > 0]
  if (!isTRUE(f_square_result$complete)) f_square <- numeric(0)
  f_square_failure_note <- if (isTRUE(f_square_result$complete)) "" else {
    reasons <- unique(as.character(f_square_result$failures$Reason %||% "estimator-consistent reduced-model fitting was incomplete"))
    reasons <- reasons[nzchar(reasons)]
    paste0(" f-squared is suppressed because ", paste(reasons, collapse = "; "), ".")
  }
  full_collinearity_vif <- structural_canvas_quality_full_collinearity_vif(bundle$fit$construct_scores %||% NULL)
  ten_times_margin <- structural_canvas_pls_ten_times_margin(bundle)
  approximate_fit <- structural_canvas_pls_approximate_fit_indices(bundle, summary_fit)
  missing <- bundle$missing_diagnostics$method %||% bundle$missing %||%
    summary_fit$missing_data$method %||% "not recorded"
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
    structural_canvas_pls_quality_predictive_label(bundle)
  )
  rows <- data.frame(
    Item = items,
    Value = displayed_values,
    Status = mapply(structural_canvas_pls_quality_status, items, displayed_values, USE.NAMES = FALSE),
    Guidance = c(
      "Algorithm diagnostic; inspect unusually high iteration counts.",
      "Smaller values indicate stable outer-weight convergence.",
      "Indicator-mean replacement is fixed for PLS/PLSc. Report the replaced rows/cells; bootstrap recomputes indicator means within each resample.",
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
      paste0("The .02/.15/.35 f2 ranges are descriptive anchors, not universal importance thresholds.", f_square_failure_note),
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
  attr(rows, "quality_formative_evidence") <- formative
  attr(rows, "quality_f_square_failures") <- if (isTRUE(f_square_result$complete)) character(0) else reasons
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
  major <- c("Min outer loading", "Min rhoC", "Min AVE", "Max HTMT", "Max item VIF", "Max inner VIF", "Max full collinearity VIF")
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

structural_canvas_pls_quality_display_rows <- function(rows, language, source = rows) {
  ko <- identical(normalize_app_language(language), "ko")
  display <- structural_canvas_quality_display_rows(rows, ko, language)
  if (identical(normalize_app_language(language), "en")) return(display)
  tr <- function(en, ko = en) statedu_localized_text(language, en, ko)
  formative <- attr(source, "quality_formative_evidence", exact = TRUE)
  if (is.data.frame(formative) && nrow(formative)) for (i in seq_len(nrow(formative))) {
    index <- which(rows$Item == paste0("Formative evidence: ", formative$Construct[i]))
    if (!length(index)) next
    display[index, match("Item", names(rows))] <- paste0(tr("Formative evidence", "형성형 근거"), ": ", formative$Construct[i])
    recorded <- function(value) if (nzchar(value)) tr("Recorded", "기록됨") else tr("Missing", "누락")
    display[index, match("Value", names(rows))] <- paste0(
      tr("Domain", "구성개념 영역"), "=", recorded(formative$`Domain definition`[i]), "; ",
      tr("Indicator rationale", "지표 포함 근거"), "=", recorded(formative$`Indicator inclusion rationale`[i]), "; ",
      tr("Content-validity procedure/source", "내용타당도 절차/출처"), "=", recorded(formative$`Content-validity procedure/source`[i]), "; ",
      tr("Redundancy evidence", "중복성 근거"), "=", if (formative$`Redundancy evidence`[i] == "Available") tr("Available", "있음") else tr("Not documented", "기록되지 않음"))
    display[index, match("Guidance", names(rows))] <- if (formative$Status[i] == "Documented") {
      tr("Report these design-based grounds with weight, collinearity, and redundancy results.", "이 설계상의 근거를 가중치, 공선성 및 중복성 결과와 함께 보고하세요.")
    } else tr("Document construct-domain coverage, indicator inclusion grounds, content-validation procedure/source, and available redundancy evidence before confirmatory reporting.", "확인적 보고 전에 구성개념 영역의 포괄성, 지표 포함 근거, 내용타당도 검증 절차/출처 및 이용 가능한 중복성 근거를 기록하세요.")
  }
  reasons <- attr(source, "quality_f_square_failures", exact = TRUE)
  index <- which(rows$Item == "Max f2")
  if (length(index) && length(reasons)) {
    localized <- vapply(reasons, function(reason) {
      prefix <- "reduced-model estimation failed: "
      if (startsWith(reason, prefix)) return(paste0(tr("Reduced-model estimation failed", "축소 모형 추정 실패"), ": ", substring(reason, nchar(prefix) + 1L)))
      korean <- c(
        "estimator-consistent reduced-model fitting was incomplete" = "동일한 추정법을 사용한 축소 모형 적합이 완료되지 않았습니다",
        "the full-model R-squared is unavailable or outside [0, 1)" = "전체 모형의 R²를 구할 수 없거나 [0, 1) 범위를 벗어났습니다",
        "the reduced model did not converge" = "축소 모형이 수렴하지 않았습니다",
        "the reduced model was numerically inadmissible" = "축소 모형의 해가 수치적으로 허용되지 않습니다",
        "the reduced model used a different estimator" = "축소 모형에 다른 추정법이 사용되었습니다",
        "the reduced model used a different PLSc common-factor specification" = "축소 모형에 다른 PLSc 공통요인 명세가 사용되었습니다",
        "the reduced-model R-squared is unavailable or outside [0, 1)" = "축소 모형의 R²를 구할 수 없거나 [0, 1) 범위를 벗어났습니다",
        "the f-squared formula produced a non-finite value" = "f² 계산 결과가 유한한 값이 아닙니다"
      )
      statedu_localized_text(language, reason, if (reason %in% names(korean)) unname(korean[reason]) else reason)
    }, character(1))
    template <- tr("f-squared is suppressed because {reason}.", "다음 사유로 f²를 표시하지 않습니다: {reason}.")
    display[index, match("Guidance", names(rows))] <- paste(
      tr("The .02/.15/.35 f2 ranges are descriptive anchors, not universal importance thresholds.", "f²의 .02/.15/.35는 기술적 참고값이며 보편적인 중요성 기준이 아닙니다."),
      gsub("{reason}", paste(localized, collapse = "; "), template, fixed = TRUE))
  }
  # These cells are already localized, including literal user construct names
  # and original engine error details. Do not translate their contents again.
  attr(display, "result_user_columns") <- match(c("Item", "Value", "Guidance"), names(rows))
  display
}

structural_canvas_pls_quality_result_ui <- function(bundle, language = statedu_initial_language()) {
  rows <- structural_canvas_pls_quality_rows(bundle)
  if (!nrow(rows)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  review_rows <- structural_canvas_pls_quality_review_rows(rows)
  summary <- structural_canvas_quality_status_summary_display(rows, ko, language)
  readiness <- structural_canvas_quality_reporting_readiness_display(review_rows, rows, ko, language)
  display_rows <- structural_canvas_pls_quality_display_rows(rows, language)
  display_review_rows <- structural_canvas_pls_quality_display_rows(review_rows, language, rows)
  div(
    class = "result-section regression-result-panel structural-pls-quality-result",
    h4(statedu_localized_text(language, "PLS-SEM quality checklist", "PLS-SEM 품질 체크리스트")),
    result_note_paragraph(class = "structural-result-note structural-quality-status-summary", summary),
    result_note_paragraph(class = "structural-result-note structural-quality-reporting-readiness", readiness),
    tags$h5(statedu_localized_text(language, "Review focus", "검토 필요 항목")),
    if (nrow(display_review_rows)) structural_canvas_basic_html_table(display_review_rows, language = language) else result_note_paragraph(class = "structural-result-note", statedu_localized_text(language, "No Review rows in the quality checklist.", "품질 체크리스트에 검토 항목이 없습니다.")),
    structural_canvas_basic_html_table(display_rows, language = language),
    if (any(rows$Status == "Review")) result_note_paragraph(
      class = "structural-result-note",
      statedu_localized_text(language, "Rows marked Review should be resolved or explicitly justified before confirmatory reporting.", "검토로 표시된 행은 확인적 보고 전 해결하거나 명시적으로 근거를 제시해야 합니다.")
    ),
    result_note_paragraph(
      class = "structural-result-note",
      statedu_localized_text(language, "This checklist summarizes PLS-SEM sample adequacy, approximate reflective-model fit diagnostics, measurement, collinearity, common-method-bias screens, explanatory power, and repeated PLSpredict boundary conditions for reporting and review.", "이 체크리스트는 PLS-SEM의 표본 적절성, 반영형 모형 근사 적합도 진단, 측정모형, 공선성, 공통방법편향 점검, 설명력과 반복 PLSpredict의 보고 조건을 요약합니다.")
    )
  )
}

structural_canvas_pls_predict_result_ui <- function(bundle, language = statedu_initial_language()) {
  if (is.null(bundle) || is.null(bundle$pls_predict_result)) return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  headers <- c(Indicator = "지표", Construct = "구성개념", Metric = "측정 지표", Assessment = "평가",
    "PLS out-of-sample" = "PLS 표본 외 오차", "LM benchmark" = "LM 기준 오차", "PLS lower %" = "PLS 오차 우세 비율(%)")
  assessments <- c("Not available" = "사용 불가", "PLS lower error" = "PLS 오차가 더 낮음", "LM lower error" = "LM 오차가 더 낮음", Tie = "동일")
  tables <- structural_canvas_pls_predict_tables(bundle$pls_predict_result)
  item_table <- tables$items
  construct_table <- tables$constructs
  for (table_name in c("item_table", "construct_table")) {
    table <- get(table_name)
    if (nrow(table)) {
      for (name in names(table)) {
        if (is.numeric(table[[name]])) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
      }
      if (identical(table_name, "item_table")) {
        table$Assessment <- vapply(table$Assessment, function(value) {
          if (value %in% names(assessments)) tr(value, unname(assessments[[value]])) else value
        }, character(1), USE.NAMES = FALSE)
      }
      names(table) <- vapply(names(table), function(header) {
        if (identical(header, "Metric")) return(tr("Error metric", "오차 지표"))
        if (header %in% names(headers)) tr(header, unname(headers[[header]])) else header
      }, character(1), USE.NAMES = FALSE)
      attr(table, "result_user_columns") <- seq_along(table)
      assign(table_name, table)
    }
  }
  div(class = "result-section regression-result-panel structural-pls-predict-result",
    h4(tr("PLSpredict predictive assessment", "PLSpredict 예측 진단")),
    result_note_paragraph(class = "structural-result-note", sprintf(tr("Direct Antecedents scheme, %s-fold, %s independent repetitions (seed = %s). PLS - LM is the mean difference, SD reflects split-to-split variability, and PLS lower %% is the proportion of repetitions favoring PLS over the linear-model benchmark. One repetition is insufficient for a predictive-performance claim.", "Direct Antecedents 방식, %s-fold, 독립 반복 %s회(seed = %s) 기준입니다. PLS - LM은 반복 평균 차이, SD는 분할 간 변동성, PLS 오차 우세 비율(%%)은 선형모형보다 오차가 낮았던 반복 비율입니다. 단일 반복 결과는 예측력 결론에 충분하지 않습니다."), bundle$pls_predict_result$folds, bundle$pls_predict_result$reps, bundle$pls_predict_result$seed)),
    if (nrow(item_table)) tagList(
      tags$h5(tr("Indicator-level out-of-sample prediction error", "지표별 표본 외 예측오차")),
      structural_canvas_basic_html_table(item_table, language = language)
    ),
    if (nrow(construct_table)) tagList(
      tags$h5(tr("Construct-level prediction error", "구성개념별 예측오차")),
      structural_canvas_basic_html_table(construct_table, language = language)
    )
  )
}

structural_canvas_pls_fit_guide_ui <- function(table, language = statedu_initial_language()) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  headers <- c(Outcome = "결과변수", Predictor = "예측변수")
  names(table) <- vapply(names(table), function(header) {
    if (header %in% names(headers)) tr(header, unname(headers[[header]])) else header
  }, character(1), USE.NAMES = FALSE)
  attr(table, "result_user_columns") <- seq_along(table)
  tagList(
    tags$h5(tr("Supplementary Table 3: Structural effect guide indices", "표 3 보조: 구조효과 가이드 지표")),
    structural_canvas_basic_html_table(table, class = "table table-striped table-bordered structural-pls-fit-guide-table", language = language),
    result_note_paragraph(class = "structural-result-note", tr("Descriptive f² references are .02 (small), .15 (medium), and .35 (large); f² is computed from an estimator-consistent reduced model. Inner VIF is not a standalone pass criterion.", "f²의 기술적 참고값은 .02(작음), .15(중간), .35(큼)이며, 표의 f²는 추정량과 일치하는 축소모형으로 계산합니다. Inner VIF도 단독 합격판정이 아닙니다."))
  )
}

structural_canvas_register_fit_diagnostic_outputs <- function(output, prefix, analysis_type, fit_result, result_table,
                                                             dataset_fn, app_language_fn,
                                                             variable_table_fn = function() NULL,
                                                             labels_fn = function() character(0),
                                                             table_number_fn = NULL,
                                                             appendix_result_table = result_table) {
output[[paste0(prefix, "_result_fit")]] <- renderUI({
  structural_canvas_fit_table_result_ui(fit_result(), result_table("fit"))
})
if (identical(analysis_type, "plssem")) {
  pls_fit_diagnostics_content <- function() {
    table <- structural_canvas_pls_fit_diagnostics_table(fit_result())
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    display <- table
    names(display)[names(display) == "Model"] <- ""
    structural_canvas_basic_html_table(
      display,
      class = "table table-striped table-bordered structural-pls-fit-diagnostics-table",
      role = "main",
      orientation = "portrait",
      note = "Note. SRMR, d_G, and d_ULS compare observed and model-implied indicator correlations under a saturated measurement-model approximation. SRMR < .08 is descriptive; no fixed cutoff is applied to d_G or d_ULS.",
      note_class = "structural-result-note structural-main-note structural-main-note-1"
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
    structural_canvas_pls_fit_guide_ui(appendix_result_table("fit_guide"), statedu_current_language(app_language_fn))
  })
  output[[paste0(prefix, "_result_fit_bootstrap")]] <- renderUI({
    table <- appendix_result_table("fit_bootstrap")
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    language <- statedu_current_language(app_language_fn)
    tr <- function(en, ko) statedu_localized_text(language, en, ko)
    bundle <- fit_result()
    bootstrap <- bundle$pls_bootstrap_result %||% list()
    valid_n <- suppressWarnings(as.integer(bootstrap$nboot %||% 0L))
    requested_n <- suppressWarnings(as.integer(bootstrap$requested_nboot %||% bundle$pls_bootstrap %||% 0L))
    timeout_n <- suppressWarnings(as.integer(bootstrap$timeout_failures %||% 0L))
    estimation_n <- suppressWarnings(as.integer(bootstrap$estimation_failures %||% max(0L, requested_n - valid_n - timeout_n)))
    nonconvergence_n <- suppressWarnings(as.integer(bootstrap$nonconvergence_failures %||% 0L))
    inadmissible_n <- suppressWarnings(as.integer(bootstrap$inadmissible_failures %||% 0L))
    retained_nonpd_n <- suppressWarnings(as.integer(bootstrap$retained_nonpositive_definite_plsc_draws %||% 0L))
    invalid_n <- suppressWarnings(as.integer(bootstrap$invalid_statistic_failures %||% 0L))
    execution_n <- suppressWarnings(as.integer(bootstrap$execution_failures %||% 0L))
    canceled_n <- suppressWarnings(as.integer(bootstrap$canceled_failures %||% 0L))
    inference_available <- isTRUE(bootstrap$inference_available)
    if (!is.finite(requested_n) || requested_n < 0L) requested_n <- 0L
    if (!is.finite(retained_nonpd_n) || retained_nonpd_n < 0L) retained_nonpd_n <- 0L
    minimum_ratio <- suppressWarnings(as.numeric(bootstrap$minimum_valid_ratio %||% .80))
    bootstrap_status <- as.character(bootstrap$bootstrap_status %||% "Not recorded")[[1L]]
    failure_message <- as.character(bootstrap$failure_message %||% "")
    failure_message <- if (length(failure_message)) trimws(failure_message[[1L]]) else ""
    status_key <- tolower(trimws(bootstrap_status))
    status_labels <- c("Adequate"="충분", "Insufficient"="불충분", "Pending"="진행 중", "Failed"="실패", "Canceled"="중단", "Not recorded"="기록 없음")
    display_status <- if (bootstrap_status %in% names(status_labels)) tr(bootstrap_status, unname(status_labels[[bootstrap_status]])) else bootstrap_status
    unavailable_note <- if (identical(status_key, "pending")) {
      tr("Bootstrap is still in progress. Point estimates are retained, but Boot SE, CI, t, and p are not reported until completion.", "부트스트랩이 진행 중입니다. 완료 전까지 점추정만 유지하고 Boot SE, CI, t, p를 보고하지 않습니다.")
    } else if (identical(status_key, "failed")) {
      tr("Bootstrap execution failed. Point estimates are retained, but Boot SE, CI, t, and p are not reported.", "부트스트랩 실행이 실패했습니다. 점추정은 유지하지만 Boot SE, CI, t, p를 보고하지 않습니다.")
    } else if (identical(status_key, "canceled")) {
      tr("Bootstrap was canceled by the user. Point estimates are retained, but Boot SE, CI, t, and p are not reported.", "사용자가 부트스트랩을 취소했습니다. 점추정은 유지하지만 Boot SE, CI, t, p를 보고하지 않습니다.")
    } else if (identical(status_key, "insufficient")) {
      sprintf(tr("Fewer than %s%% of requested resamples passed the whole-draw statistic contract, so Boot SE, CI, t, and p are not reported.", "전체 통계량이 유효한 반복이 요청 반복의 %s%% 미만이어서 Boot SE, CI, t, p를 보고하지 않습니다."), formatC(100 * minimum_ratio, format = "fg", digits = 3))
    } else {
      sprintf(tr("Bootstrap inference is unavailable (status: %s). Point estimates are retained.", "부트스트랩 추론을 사용할 수 없습니다(상태: %s). 점추정만 유지합니다."), display_status)
    }
    if (nzchar(failure_message)) unavailable_note <- paste0(unavailable_note, " ", sprintf(tr("Detail: %s", "상세: %s"), failure_message))
    effect_sections <- list(
      list(key = "Specific indirect", ko = "경로별 특정 간접효과", en = "Specific indirect effects by path"),
      list(key = "Total indirect", ko = "총간접효과", en = "Total indirect effects"),
      list(key = "Total", ko = "총효과", en = "Total effects")
    )
    section_ui <- lapply(effect_sections, function(section) {
      rows <- table[table$Effect == section$key, , drop = FALSE]
      if (!nrow(rows)) return(NULL)
      display <- structural_canvas_subset_columns(rows, c(
        "Path", "beta", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper",
        "t", "p", "BH-adjusted p"
      ))
      display <- structural_canvas_drop_empty_display_columns(
        display,
        c("Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "t", "p", "BH-adjusted p")
      )
      names(display)[names(display) == "beta"] <- "β"
      names(display)[names(display) == "BH-adjusted p"] <- tr("BH-adjusted p", "BH 보정 p")
      attr(display, "result_user_columns") <- seq_along(display)
      tagList(
        tags$h5(tr(section$en, section$ko)),
        structural_canvas_basic_html_table(display, class = "table table-striped table-bordered structural-pls-fit-bootstrap-table", language = language)
      )
    })
    section_ui <- Filter(Negate(is.null), section_ui)
    if (!length(section_ui)) return(NULL)
    tagList(
      section_ui,
      if (requested_n <= 0L) result_note_paragraph(class = "structural-result-note", tr("Bootstrap was not requested, so only the point estimate β is shown. Boot SE, 95% CI, t, p, and BH-adjusted p are reported after bootstrap.", "부트스트랩을 실행하지 않아 β 점추정값만 표시합니다. Boot SE, 95% CI, t, p 및 BH 보정 p는 부트스트랩 실행 후 보고됩니다.")),
      if (requested_n > 0L) result_note_paragraph(class = "structural-result-note", sprintf(tr("Status: %s. Valid resamples: %s/%s (timeouts %s, estimation failures %s, nonconvergence %s, inadmissible solutions %s, statistic-contract failures %s, execution failures %s, cancellations %s).", "상태: %s. 유효 재표집: %s/%s회 (시간 제한 %s, 추정 실패 %s, 비수렴 %s, 허용 불가 해 %s, 통계량 계약 실패 %s, 실행 실패 %s, 취소 %s)."), display_status, valid_n, requested_n, timeout_n, estimation_n, nonconvergence_n, inadmissible_n, invalid_n, execution_n, canceled_n)),
      if (requested_n > 0L && retained_nonpd_n > 0L) result_note_paragraph(class = "structural-result-note", sprintf(tr("Globally non-positive-definite PLSc resamples retained after all local-equation and downstream checks: %s.", "전역 비양정이지만 모든 국소 방정식과 후속 검사를 통과해 보존한 PLSc 재표본: %s회."), retained_nonpd_n)),
      if (requested_n > 0L && !inference_available) result_note_paragraph(class = "structural-result-note structural-result-warning", unavailable_note),
      if (requested_n > 0L && inference_available) result_note_paragraph(class = "structural-result-note", tr("Direct, specific indirect, total indirect, and total effects are distinct estimands. BH adjustment is calculated within each table using unique hypotheses only. Type-7 percentile CIs and plus-one two-sided empirical sign p values use only resamples that pass the whole-draw statistic contract.", "직접효과, 특정 간접효과, 총간접효과와 총효과는 서로 분리된 추정대상입니다. 각 표의 BH 보정은 해당 표 안의 고유한 가설만으로 계산합니다. type-7 percentile CI와 plus-one 양측 경험적 부호 p는 전체 통계량 계약을 통과한 반복만 사용합니다."))
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
  structural_canvas_invariance_result_ui(
    fit_result(), "en",
    variable_table_fn(), labels_fn() %||% character(0),
    table_number_fn = table_number_fn
  )
})
output[[paste0(prefix, "_result_invariance_appendix")]] <- renderUI({
  structural_canvas_invariance_appendix_ui(
    fit_result(), statedu_current_language(app_language_fn),
    variable_table_fn(), labels_fn() %||% character(0)
  )
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
