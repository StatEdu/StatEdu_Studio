# Survey-aware reporting and a parallel-slopes diagnostic. The latter fits the
# cumulative binary logits jointly as a stacked estimating equation; copies of
# each sampled unit retain its original survey IDs/replicate weights. This is
# not the independent-observation Brant test or a likelihood-ratio test.
complex_sample_logistic_wald <- function(beta, covariance, contrast, df) {
  delta <- as.numeric(contrast %*% beta)
  variance <- contrast %*% covariance %*% t(contrast)
  q <- nrow(contrast)
  if (!q || !is.finite(df) || df <= 0 || any(!is.finite(variance)) || qr(variance)$rank < q) return(NULL)
  statistic <- as.numeric(crossprod(delta, solve(variance, delta))) / q
  if (!is.finite(statistic) || statistic < 0) return(NULL)
  list(F = statistic, df1 = q, df2 = df, p = stats::pf(statistic, q, df, lower.tail = FALSE))
}

complex_sample_logistic_term_tests <- function(model, variable_info, labels, category_table) {
  formula <- stats::reformulate(unname(model$safe[model$predictors]), response = "..outcome..")
  matrix <- stats::model.matrix(formula, model$design$variables)
  assignment <- attr(matrix, "assign")
  beta <- stats::coef(model$fit)
  tests <- lapply(seq_along(model$predictors), function(i) {
    terms <- colnames(matrix)[assignment == i]
    names <- if (model$options$logistic_model == "ordinal") terms else
      as.vector(outer(terms, seq_len(length(model$levels) - 1L), paste, sep = ":"))
    indices <- match(names, names(beta))
    test <- if (anyNA(indices)) NULL else complex_sample_logistic_wald(beta, model$covariance,
      diag(length(beta))[indices, , drop = FALSE], model$df)
    data.frame(Variable = frequency_variable_display_name(model$predictors[[i]], variable_info, labels, category_table),
      `Wald F` = if (is.null(test)) "" else complex_sample_num(test$F),
      df1 = if (is.null(test)) "" else as.character(test$df1),
      df2 = as.character(model$df), p = if (is.null(test)) "" else complex_sample_p_value(test$p),
      Status = if (is.null(test)) "Not estimable" else "Estimated", check.names = FALSE)
  })
  do.call(rbind, tests)
}

complex_sample_logistic_parallel_slopes <- function(model) {
  tryCatch({
    design <- model$design
    n <- nrow(design$variables)
    cuts <- length(model$levels) - 1L
    formula <- stats::reformulate(unname(model$safe[model$predictors]), response = "..outcome..")
    X <- stats::model.matrix(formula, design$variables)
    p <- ncol(X)
    stacked <- design[rep(seq_len(n), cuts), ]
    y <- as.integer(design$variables$`..outcome..`)
    stacked$variables$`..po_response..` <- as.numeric(rep(y, cuts) > rep(seq_len(cuts), each = n))
    predictor_names <- paste0("..po", seq_len(p * cuts), "..")
    for (j in seq_len(cuts)) for (k in seq_len(p)) {
      values <- numeric(n * cuts)
      values[(j - 1L) * n + seq_len(n)] <- X[, k]
      stacked$variables[[predictor_names[[(j - 1L) * p + k]]]] <- values
    }
    # survey::svyglm.svyrep.design evaluates its temporary call named `g` inside
    # the data frame. Original columns such as `g` must not mask that call.
    stacked$variables <- stacked$variables[, c("..po_response..", predictor_names), drop = FALSE]
    warnings <- character()
    fit <- withCallingHandlers(survey::svyglm(stats::reformulate(predictor_names, response = "..po_response..", intercept = FALSE),
      design = stacked, family = stats::quasibinomial(), control = stats::glm.control(maxit = 100)),
      warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
    if (!isTRUE(fit$converged) || any(!is.finite(stats::coef(fit))) || any(!is.finite(stats::vcov(fit))) ||
        any(fit$fitted.values < 1e-8 | fit$fitted.values > 1 - 1e-8) || length(warnings)) {
      return(list(test = NULL, reason = "Parallel-slopes diagnostic unavailable: cumulative logits are unstable or did not converge.", details = warnings, fit = fit))
    }
    contrast <- matrix(0, (cuts - 1L) * (p - 1L), p * cuts)
    row <- 0L
    for (j in 2:cuts) for (k in 2:p) {
      row <- row + 1L
      contrast[row, k] <- -1
      contrast[row, (j - 1L) * p + k] <- 1
    }
    test <- complex_sample_logistic_wald(stats::coef(fit), stats::vcov(fit), contrast, model$df)
    list(test = test, fit = fit, contrast = contrast,
      reason = if (is.null(test)) "Parallel-slopes diagnostic unavailable: the contrast covariance is singular." else "")
  }, error = function(e) list(test = NULL, reason = "Parallel-slopes diagnostic unavailable: cumulative logits could not be estimated.", details = conditionMessage(e)))
}

complex_sample_weighted_matrix_gvif <- function(matrix, groups, weights) {
  # Predictor-geometry diagnostic only: not the survey sandwich covariance of a fit.
  matrix <- as.matrix(matrix)
  if (!ncol(matrix) || length(groups) != ncol(matrix) || length(weights) != nrow(matrix) ||
      any(!is.finite(matrix)) || any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0))
    stop("Invalid predictor matrix or sampling weights.")
  weights <- weights / max(weights)
  weights <- weights / sum(weights)
  centered <- sweep(matrix, 2, colSums(matrix * weights), "-")
  gram <- crossprod(centered, centered * weights)
  scales <- sqrt(diag(gram))
  if (any(!is.finite(scales)) || any(scales <= 0)) stop("A predictor column has zero weighted variance.")
  correlation <- gram / outer(scales, scales)
  spectrum <- eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
  if (min(spectrum) <= max(spectrum) * .Machine$double.eps * ncol(matrix) * 10)
    stop("The weighted predictor matrix is singular or numerically rank deficient.")
  logdet <- function(x) if (!nrow(x)) 0 else 2 * sum(log(diag(chol(x))))
  total <- logdet(correlation)
  terms <- unique(groups)
  gvif <- vapply(terms, function(term) {
    ii <- which(groups == term); jj <- which(groups != term)
    exp(max(0, logdet(correlation[ii, ii, drop = FALSE]) + logdet(correlation[jj, jj, drop = FALSE]) - total))
  }, numeric(1))
  df <- vapply(terms, function(term) sum(groups == term), integer(1))
  data.frame(Term = terms, df = df, GVIF = gvif, Adjusted = gvif^(1/(2*df)), check.names = FALSE)
}

complex_sample_logistic_collinearity <- function(model, variable_info = NULL, labels = character(), category_table = NULL) {
  display <- vapply(model$predictors, frequency_variable_display_name, character(1),
    variable_info = variable_info, labels = labels, category_table = category_table)
  result <- tryCatch({
    if (!requireNamespace("survey", quietly = TRUE)) stop("The survey package is unavailable.")
    X <- stats::model.matrix(stats::reformulate(unname(model$safe[model$predictors])), model$design$variables)
    groups <- attr(X, "assign")
    keep <- groups != 0L
    complex_sample_weighted_matrix_gvif(X[, keep, drop = FALSE], groups[keep],
      as.numeric(stats::weights(model$design, type = "sampling")))
  }, error = function(e) e)
  if (inherits(result, "error")) {
    table <- data.frame(Variable = display, df = "", GVIF = "", `GVIF^(1/(2df))` = "",
      Status = paste("Not estimable:", conditionMessage(result)), check.names = FALSE)
  } else {
    table <- data.frame(Variable = display[result$Term], df = as.character(result$df),
      GVIF = vapply(result$GVIF, complex_sample_num, character(1)),
      `GVIF^(1/(2df))` = vapply(result$Adjusted, complex_sample_num, character(1)),
      Status = "Estimated", check.names = FALSE)
  }
  attr(table, "result_user_columns") <- "Variable"
  table
}

complex_sample_logistic_sparse_summary <- function(model, variable_info = NULL, labels = character(), category_table = NULL) {
  rows <- lapply(model$predictors, function(predictor) {
    values <- model$design$variables[[model$safe[[predictor]]]]
    if (!is.factor(values)) return(NULL)
    counts <- table(model$design$variables$`..outcome..`, values)
    data.frame(Variable = frequency_variable_display_name(predictor, variable_info, labels, category_table),
      Cells = length(counts), `Minimum n` = min(counts), `Empty cells` = sum(counts == 0),
      `Cells 1-4` = sum(counts > 0 & counts < 5),
      Status = if (any(counts < 5)) "Flagged" else "No cells below 5", check.names = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  result <- if (length(rows)) do.call(rbind, rows) else data.frame(Variable = "None",
    Cells = "", `Minimum n` = "", `Empty cells` = "", `Cells 1-4` = "",
    Status = "Not applicable", check.names = FALSE)
  attr(result, "result_user_columns") <- "Variable"
  result
}

complex_sample_logistic_stability <- function(model) {
  rows <- list()
  add <- function(item, status, details) {
    rows[[length(rows) + 1L]] <<- data.frame(Item = item, Status = status,
      Details = details, check.names = FALSE)
  }
  number <- function(x) format(x, digits = 4, scientific = FALSE, trim = TRUE)
  fit <- model$fit
  probabilities <- NULL
  if (identical(model$options$logistic_model, "ordinal")) {
    code <- fit$convergence
    known <- length(code) == 1L && is.finite(code)
    add("Optimizer convergence", if (!known) "Unavailable" else if (code == 0) "Reported convergence" else "Review",
      if (known) paste("Optimizer code:", code) else "No optimizer convergence code is stored.")
    probabilities <- fit$fitted.values
  } else {
    engine <- fit$fit
    if (isS4(engine) && all(c("iter", "control") %in% methods::slotNames(engine))) {
      iteration <- methods::slot(engine, "iter")
      limit <- methods::slot(engine, "control")$maxit
      known <- length(iteration) == 1L && length(limit) == 1L && all(is.finite(c(iteration, limit)))
      add("IRLS iterations", if (!known) "Unavailable" else if (iteration >= limit) "Review" else "Recorded",
        if (known) paste0(iteration, " iterations; stored limit = ", limit,
          ". Iteration count alone does not certify convergence.") else "Iteration count or limit is not stored.")
      if ("fitted.values" %in% methods::slotNames(engine)) probabilities <- methods::slot(engine, "fitted.values")
    } else add("IRLS iterations", "Unavailable", "No supported engine iteration record is stored.")
  }
  warnings <- unique(model$warnings)
  add("Captured fit warnings", if (length(warnings)) "Review" else "None recorded",
    if (length(warnings)) paste(warnings, collapse = " | ") else "No warnings were captured; this is not proof of convergence.")
  raw <- model$raw
  invalid <- sum(!is.finite(raw$Estimate) | !is.finite(raw$SE) | raw$SE <= 0)
  add("Coefficients and SE", if (invalid) "Review" else "No flag",
    paste(invalid, "of", nrow(raw), "parameters have non-finite B/SE or non-positive SE (including intercepts/thresholds)."))
  slopes <- raw[raw$Term %in% model$slope_names, , drop = FALSE]
  if (nrow(slopes) && all(is.finite(slopes$Estimate)) && all(is.finite(slopes$SE))) {
    add("Slope magnitude", "Descriptive", paste0("Maximum |B| = ", number(max(abs(slopes$Estimate))),
      "; maximum SE = ", number(max(slopes$SE)), ". Values depend on predictor units and coding."))
    show_ci <- isTRUE(model$options$show_ci)
    limits <- exp(c(slopes$Estimate, if (show_ci) c(slopes$Estimate - stats::qt(.975, model$df)*slopes$SE,
      slopes$Estimate + stats::qt(.975, model$df)*slopes$SE)))
    bad <- sum(!is.finite(limits) | limits <= 0)
    add(if (show_ci) "Slope OR and CI range" else "Slope OR range", if (bad) "Review" else "Descriptive",
      if (bad) paste(bad, if (show_ci) "OR/CI" else "OR", "values overflowed, underflowed or were non-finite.") else
        paste0(if (show_ci) "Across slope ORs and 95% CI endpoints: " else "Across slope ORs: ", number(min(limits)), " to ", number(max(limits)), "."))
  } else add("Slope magnitude and OR", "Unavailable", "Finite slope estimates are unavailable.")
  V <- model$covariance
  valid <- is.matrix(V) && nrow(V) > 0 && nrow(V) == ncol(V) && all(is.finite(V)) &&
    all(diag(V) > 0) && isTRUE(isSymmetric(V, tol = 1e-8))
  if (valid) {
    R <- V / sqrt(outer(diag(V), diag(V)))
    ev <- eigen((R + t(R))/2, symmetric = TRUE, only.values = TRUE)$values
    tolerance <- max(abs(ev)) * nrow(V) * .Machine$double.eps * 10
    rank <- sum(ev > tolerance)
    add("Survey covariance", if (rank < nrow(V)) "Review" else "No flag",
      paste0("Standardized covariance rank = ", rank, "/", nrow(V), "; minimum eigenvalue = ",
        number(min(ev)), ". Includes intercepts/thresholds; this is not a Hessian check."))
  } else add("Survey covariance", "Review", "Covariance is missing, non-finite, asymmetric or has non-positive diagonal entries.")
  if (is.null(probabilities) || !length(probabilities)) {
    add("Fitted probabilities", "Unavailable", "Fitted category probabilities are not stored.")
  } else {
    probabilities <- as.matrix(probabilities)
    bad <- sum(!is.finite(probabilities) | probabilities < 0 | probabilities > 1)
    bad_sum <- sum(!is.finite(rowSums(probabilities)) | abs(rowSums(probabilities)-1) > 1e-7)
    edge <- sum(is.finite(probabilities) & (probabilities < 1e-8 | probabilities > 1-1e-8))
    add("Fitted probabilities", if (bad || bad_sum || edge) "Review" else "No flag",
      paste0(bad, " invalid entries; ", bad_sum, " rows not summing to 1; ", edge,
        " of ", length(probabilities), " category probabilities within 1e-8 of 0 or 1."))
  }
  do.call(rbind, rows)
}

complex_sample_logistic_language_guide <- function(language = NULL) {
  language <- result_appendix_table_language(language)
  keys <- c("estimates", "flow", "odds", "screening", "stability", "scope")
  table <- data.frame(Topic = c("Default estimates", "Analysis cases", "Odds-ratio interpretation",
    "Screening diagnostics", "Estimation stability", "Scope and export"),
    Explanation = vapply(keys, function(key) statedu_t(paste0("complex_sample.logistic_guide.", key), language), character(1)),
    check.names = FALSE)
  attr(table, "compact_column_widths") <- c(22, 78)
  shiny::div(class = "result-section regression-result-panel logistic-language-guide",
    `data-guide-language` = language, shiny::h3("Interpretation guide"),
    coefficient_html_table(table, table_role = "appendix", table_language = "en", sheet_orientation = "portrait"))
}

complex_sample_logistic_case_flow <- function(model) {
  meta <- model$built$meta
  required <- c("original_n", "design_excluded_n", "design_n", "subpopulation_missing_n", "subpopulation_excluded_n", "analysis_n")
  if (any(vapply(required, function(x) is.null(meta[[x]]) || length(meta[[x]]) != 1L || !is.finite(meta[[x]]), logical(1))))
    return(data.frame(Stage = "Case-flow metadata unavailable", `Excluded n` = "", `Remaining n` = "", check.names = FALSE))
  excluded <- c(0, meta$design_excluded_n, meta$subpopulation_missing_n, meta$subpopulation_excluded_n, model$excluded_n)
  remaining <- meta$original_n - cumsum(excluded)
  stopifnot(all(excluded >= 0), all(remaining >= 0), remaining[2] == meta$design_n,
    remaining[4] == model$eligible_n, meta$analysis_n == model$eligible_n,
    tail(remaining, 1) == nrow(model$design$variables))
  data.frame(Stage = c("Input to this analysis", "Design-variable or weight exclusions",
    "Missing subpopulation membership", "Outside selected subpopulation", "Incomplete outcome or predictors"),
    `Excluded n` = c("", as.character(excluded[-1])), `Remaining n` = remaining, check.names = FALSE)
}

complex_sample_logistic_missing_partition <- function(model) {
  counts <- model$missing_partition
  if (is.null(counts)) return(NULL)
  stopifnot(sum(counts) == model$excluded_n)
  data.frame(Reason = c("Outcome only", "One or more predictors only", "Both outcome and predictor(s)"),
    `Excluded n` = as.integer(counts[c("outcome_only", "predictor_only", "both")]), check.names = FALSE)
}

complex_sample_logistic_reporting <- function(model, outcome, variable_info, labels, category_table) {
  variables <- model$design$variables
  category_labels <- frequency_value_display_labels(outcome, model$levels, category_table)
  weights <- as.numeric(stats::weights(model$design, type = "sampling"))
  outcome_counts <- table(variables$`..outcome..`)
  distribution <- data.frame(Category = category_labels, `Unweighted n` = as.integer(outcome_counts),
    `Weighted %` = vapply(model$levels, function(level) complex_sample_num(100 * sum(weights[variables$`..outcome..` == level])/sum(weights), 1), character(1)),
    check.names = FALSE)
  attr(distribution, "result_user_columns") <- "Category"
  missing <- model$missing
  missing$Variable <- vapply(missing$Variable, frequency_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table)
  attr(missing, "result_user_columns") <- "Variable"
  sparse_rows <- list()
  for (predictor in model$predictors) {
    values <- variables[[model$safe[[predictor]]]]
    if (!is.factor(values)) next
    counts <- table(variables$`..outcome..`, values)
    sparse <- which(counts < 5, arr.ind = TRUE)
    for (i in seq_len(nrow(sparse))) {
      row <- sparse[i, 1]; col <- sparse[i, 2]
      sparse_rows[[length(sparse_rows) + 1L]] <- data.frame(
        Variable = frequency_variable_display_name(predictor, variable_info, labels, category_table),
        Category = frequency_value_display_labels(predictor, colnames(counts)[col], category_table),
        Outcome = category_labels[row], `Unweighted n` = as.integer(counts[row, col]), check.names = FALSE)
    }
  }
  list(distribution = distribution, missing = missing,
    sparse_summary = complex_sample_logistic_sparse_summary(model, variable_info, labels, category_table),
    sparse = if (length(sparse_rows)) do.call(rbind, sparse_rows) else NULL,
    terms = complex_sample_logistic_term_tests(model, variable_info, labels, category_table))
}

complex_sample_logistic_reporting_sections <- function(model, outcome, variable_info, labels, category_table, language) {
  report <- complex_sample_logistic_reporting(model, outcome, variable_info, labels, category_table)
  section <- function(title, table, note = NULL, role = "appendix") {
    if (is.null(table) || !nrow(table)) return(NULL)
    lang <- if (role == "main") "en" else language
    if (role == "appendix") title <- result_appendix_ui_text(title, language)
    shiny::div(class = "result-section regression-result-panel logistic-result-panel",
      shiny::h3(title), coefficient_html_table(complex_sample_table_data(table, role, lang),
        note_line = if (role == "appendix" && !is.null(note)) result_appendix_ui_text(note, language) else note,
        table_role = role, table_language = lang))
  }
  parallel <- NULL
  if (model$options$logistic_model == "ordinal" && isTRUE(model$options$logistic_parallel)) {
    diagnostic <- complex_sample_logistic_parallel_slopes(model)
    test <- diagnostic$test
    table <- data.frame(Test = "Parallel slopes", `Wald F` = if (is.null(test)) "" else complex_sample_num(test$F),
      df1 = if (is.null(test)) "" else as.character(test$df1), df2 = as.character(model$df),
      p = if (is.null(test)) "" else complex_sample_p_value(test$p),
      Status = if (is.null(test)) diagnostic$reason else if (test$p < .05) "Evidence against proportional odds" else "No evidence against proportional odds", check.names = FALSE)
    parallel <- section("Proportional-odds assumption diagnostic", table,
      "Survey-adjusted Wald test of equal slopes across cumulative binary logits. Cross-threshold covariance retains the original survey design. A non-significant result does not prove proportional odds; this is not the ordinary Brant test.")
  }
  if (!is.null(report$sparse)) attr(report$sparse, "result_user_columns") <- c("Variable", "Category", "Outcome")
  shiny::tagList(
    section("Analysis sample flow", complex_sample_logistic_case_flow(model),
      "n = unweighted case count. Input refers to the data supplied to this analysis after any active case selection or split; it is not necessarily the full loaded file. Exclusions are sequential and counted once. Design exclusions combine missing design values and invalid weights/FPC/replicate weights without double counting. Subpopulation counts are measured among design-eligible cases; membership-missing cases precede known cases outside the domain. The final remaining count is the complete-case analysis N. Domain selection uses survey-design subsetting."),
    section("Mutually exclusive complete-case exclusion reasons", complex_sample_logistic_missing_partition(model),
      "n = unweighted case count. Reasons are measured after survey-design and subpopulation filtering using the recoded model variables. The three reasons are mutually exclusive and sum to the final complete-case exclusions. Multiple missing predictors count as one case. Variable-specific missing counts in the separate table may overlap. No imputation is performed; this table does not establish that missingness is random or that complete-case estimates are unbiased."),
    section("Outcome distribution in the analysis sample", report$distribution,
      "Counts are unweighted; percentages use sampling weights among complete cases. Weighted percentages are not population totals."),
    if (isTRUE(model$options$show_model_fit)) section("Design-based tests of model terms", report$terms,
      "Wald F tests jointly assess all coefficients of each predictor, conditional on the other predictors. Multinomial tests include all non-reference outcome equations. A blank F or p means the test is not estimable."),
    section("Missing data by analysis variable", report$missing,
      "Missing counts are measured after design and subpopulation filtering, before complete-case exclusion. Missing counts may overlap across variables."),
    section("Convergence and estimation stability", complex_sample_logistic_stability(model),
      paste0("B = log-odds coefficient; SE = design-based standard error; OR = odds ratio; ",
        if (isTRUE(model$options$show_ci)) "CI = confidence interval; " else "",
        "IRLS = iteratively reweighted least squares. Diagnostics describe the stored fit without refitting. Slope magnitudes are descriptive, with no universal large-coefficient cutoff. Probability boundary flags use 1e-8 as a numerical screening tolerance, not a separation test. No flag does not establish model validity or exclude complete/quasi-complete separation. Ordinal convergence and multinomial iteration records are different forms of evidence; captured warnings must also be reviewed. Proportional odds and functional form require separate assessment.")),
    section("Sampling-weighted predictor collinearity", complex_sample_logistic_collinearity(model, variable_info, labels, category_table),
      "GVIF = generalized variance inflation factor; df = number of predictor columns for the term; GVIF^(1/(2df)) = dimension-adjusted GVIF. Calculated from the sampling-weighted, centered predictor matrix in the analyzed cases, with categorical columns grouped by variable. This describes predictor geometry, not inflation of the fitted logistic model's survey-sandwich variance. Clustering and stratification are not included in this geometry diagnostic. For df=1, GVIF is VIF and the adjusted value is its square root; do not apply raw VIF cutoffs to the adjusted column. No automatic variable deletion or model-validity decision is made. Low values do not exclude separation, misspecification or proportional-odds violations."),
    section("Sparse-cell screening summary", report$sparse_summary,
      "n = unweighted cell count. Each row summarizes the outcome-by-predictor cross-tabulation among analyzed cases. Empty cells and cells with 1-4 cases are counted separately. Counts below five are descriptive screening flags, not a validity cutoff. Not applicable means there are no categorical predictors. This bivariate screen does not test complete or quasi-complete separation and does not examine joint predictor patterns or continuous predictors. No flagged cells does not establish model validity."),
    section("Sparse outcome-predictor cells", report$sparse,
      "Unweighted cells below five are screening flags, not a formal validity threshold. Empty or sparse cells can cause unstable estimates or separation."),
    parallel)
}
