# Structural equation canvas reliability and measurement-quality helpers.

structural_canvas_cronbach_alpha <- function(covariance, indicators) {
  covariance <- as.matrix(covariance)
  indicators <- unique(as.character(indicators))
  if (length(indicators) < 2L || !all(indicators %in% rownames(covariance))) return(NA_real_)
  item_covariance <- covariance[indicators, indicators, drop = FALSE]
  total_variance <- sum(item_covariance, na.rm = TRUE)
  if (!is.finite(total_variance) || total_variance <= 0) return(NA_real_)
  k <- length(indicators)
  k / (k - 1) * (1 - sum(diag(item_covariance), na.rm = TRUE) / total_variance)
}

structural_canvas_reliability_estimates <- function(fit, formula_mode = "standardized") {
  standardized <- lavaan::standardizedSolution(fit)
  observed <- lavaan::lavNames(fit, "ov")
  loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs", "est.std"), drop = FALSE]
  factors <- unique(loadings$lhs)
  theta_raw <- as.matrix(lavaan::lavInspect(fit, "theta"))
  theta_standardized <- matrix(0, nrow = length(observed), ncol = length(observed), dimnames = list(observed, observed))
  theta_rows <- standardized$op == "~~" & standardized$lhs %in% observed & standardized$rhs %in% observed
  for (index in which(theta_rows)) {
    lhs <- standardized$lhs[[index]]
    rhs <- standardized$rhs[[index]]
    theta_standardized[lhs, rhs] <- standardized$est.std[[index]]
    theta_standardized[rhs, lhs] <- standardized$est.std[[index]]
  }
  sample_covariance <- lavaan::lavInspect(fit, "sampstat")$cov %||% NULL
  rows <- lapply(factors, function(factor) {
    factor_rows <- loadings$lhs == factor
    indicators <- loadings$rhs[factor_rows]
    lambda <- loadings$est.std[factor_rows]
    if (identical(formula_mode, "model_implied")) {
      parameters <- lavaan::parameterEstimates(fit)
      raw <- parameters$est[parameters$op == "=~" & parameters$lhs == factor & parameters$rhs %in% indicators]
      latent_variance <- diag(as.matrix(lavaan::lavInspect(fit, "cov.lv")))[[factor]]
      residual_variances <- diag(theta_raw)[indicators]
      ave_common <- sum(raw^2 * latent_variance, na.rm = TRUE)
      ave <- ave_common / (ave_common + sum(residual_variances, na.rm = TRUE))
      common <- sum(raw)^2 * latent_variance
    } else {
      ave <- mean(lambda^2, na.rm = TRUE)
      common <- sum(lambda)^2
    }
    theta <- if (identical(formula_mode, "model_implied")) theta_raw else theta_standardized
    error_total <- if (all(indicators %in% rownames(theta))) sum(theta[indicators, indicators, drop = FALSE], na.rm = TRUE) else NA_real_
    cr <- if (is.finite(error_total) && common + error_total > 0) common / (common + error_total) else NA_real_
    data.frame(
      Factor = factor, AVE = ave, CR = cr,
      Alpha = if (!is.null(sample_covariance)) structural_canvas_cronbach_alpha(sample_covariance, indicators) else NA_real_,
      Omega = cr, check.names = FALSE
    )
  })
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

# Bootstrap workers fit the same single-group CFA thousands of times. The
# public lavaan accessors below repeatedly validate the object and re-read
# package metadata on Windows. For the bundled lavaan 0.7-2 contract, obtain
# the same aligned standardized estimates and sample matrices directly from
# the fitted object. Any version/slot/internal mismatch falls back to the
# public implementation above.
structural_canvas_reliability_estimates_bootstrap_fast <- function(fit, formula_mode = "standardized") {
  if (!isTRUE(getOption("statedu.isolated_lavaan_bootstrap_worker", FALSE)) ||
      !inherits(fit, "lavaan") ||
      !identical(tryCatch(as.character(fit@version[[1L]]), error = function(error) ""), "0.7-2")) {
    return(structural_canvas_reliability_estimates(fit, formula_mode))
  }
  value <- tryCatch({
    namespace <- asNamespace("lavaan")
    standardize_all <- get("lav_standardize_all", envir = namespace, inherits = FALSE)
    inspect_theta <- get("lav_inspect_theta", envir = namespace, inherits = FALSE)
    inspect_cov_lv <- get("lav_inspect_cov_lv", envir = namespace, inherits = FALSE)
    object_vnames <- get("lav_object_vnames", envir = namespace, inherits = FALSE)
    parameter_table <- fit@ParTable
    standardized_all <- as.numeric(standardize_all(fit))
    if (!is.list(parameter_table) ||
        !all(c("lhs", "op", "rhs", "est") %in% names(parameter_table)) ||
        length(standardized_all) != length(parameter_table$est) ||
        !identical(as.integer(fit@Data@ngroups), 1L) ||
        !identical(as.integer(fit@Data@nlevels), 1L)) {
      stop("lavaan CFA bootstrap slot contract changed")
    }
    observed <- as.character(object_vnames(parameter_table, "ov"))
    standardized <- data.frame(
      lhs = as.character(parameter_table$lhs),
      op = as.character(parameter_table$op),
      rhs = as.character(parameter_table$rhs),
      est.std = standardized_all,
      stringsAsFactors = FALSE
    )
    loadings <- standardized[
      standardized$op == "=~" & standardized$rhs %in% observed,
      c("lhs", "rhs", "est.std"), drop = FALSE
    ]
    factors <- unique(loadings$lhs)
    theta_raw <- as.matrix(inspect_theta(
      fit, correlation_metric = FALSE, add_labels = TRUE,
      add_class = FALSE, drop_list_single_group = TRUE
    ))
    theta_standardized <- matrix(
      0, nrow = length(observed), ncol = length(observed),
      dimnames = list(observed, observed)
    )
    theta_rows <- standardized$op == "~~" &
      standardized$lhs %in% observed & standardized$rhs %in% observed
    for (index in which(theta_rows)) {
      lhs <- standardized$lhs[[index]]
      rhs <- standardized$rhs[[index]]
      theta_standardized[lhs, rhs] <- standardized$est.std[[index]]
      theta_standardized[rhs, lhs] <- standardized$est.std[[index]]
    }
    sample_covariance <- as.matrix(fit@SampleStats@cov[[1L]])
    if (is.null(rownames(sample_covariance)) && nrow(sample_covariance) == length(observed)) {
      dimnames(sample_covariance) <- list(observed, observed)
    }
    latent_covariance <- as.matrix(inspect_cov_lv(
      fit, correlation_metric = FALSE, add_labels = TRUE,
      add_class = FALSE, drop_list_single_group = TRUE
    ))
    rows <- lapply(factors, function(factor) {
      factor_rows <- loadings$lhs == factor
      indicators <- loadings$rhs[factor_rows]
      lambda <- loadings$est.std[factor_rows]
      if (identical(formula_mode, "model_implied")) {
        loading_rows <- parameter_table$op == "=~" &
          parameter_table$lhs == factor & parameter_table$rhs %in% indicators
        raw <- as.numeric(parameter_table$est[loading_rows])
        latent_variance <- diag(latent_covariance)[[factor]]
        residual_variances <- diag(theta_raw)[indicators]
        ave_common <- sum(raw^2 * latent_variance, na.rm = TRUE)
        ave <- ave_common / (ave_common + sum(residual_variances, na.rm = TRUE))
        common <- sum(raw)^2 * latent_variance
      } else {
        ave <- mean(lambda^2, na.rm = TRUE)
        common <- sum(lambda)^2
      }
      theta <- if (identical(formula_mode, "model_implied")) theta_raw else theta_standardized
      error_total <- if (all(indicators %in% rownames(theta))) {
        sum(theta[indicators, indicators, drop = FALSE], na.rm = TRUE)
      } else NA_real_
      cr <- if (is.finite(error_total) && common + error_total > 0) {
        common / (common + error_total)
      } else NA_real_
      data.frame(
        Factor = factor, AVE = ave, CR = cr,
        Alpha = if (length(sample_covariance)) {
          structural_canvas_cronbach_alpha(sample_covariance, indicators)
        } else NA_real_,
        Omega = cr, check.names = FALSE
      )
    })
    if (length(rows)) do.call(rbind, rows) else data.frame()
  }, error = function(error) NULL)
  if (is.data.frame(value)) value else structural_canvas_reliability_estimates(fit, formula_mode)
}

structural_canvas_group_reliability_estimates <- function(fit, formula_mode = "standardized") {
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label"))
  group_count <- as.integer(lavaan::lavInspect(fit, "ngroups"))
  if (!is.finite(group_count) || group_count < 2L) return(data.frame())
  if (length(group_labels) != group_count) group_labels <- paste("Group", seq_len(group_count))
  standardized_all <- lavaan::standardizedSolution(fit)
  parameters_all <- lavaan::parameterEstimates(fit)
  observed <- lavaan::lavNames(fit, "ov")
  theta_all <- lavaan::lavInspect(fit, "theta")
  latent_covariance_all <- lavaan::lavInspect(fit, "cov.lv")
  sample_statistics_all <- lavaan::lavInspect(fit, "sampstat")
  rows <- lapply(seq_len(group_count), function(group_index) {
    standardized <- standardized_all[standardized_all$group == group_index, , drop = FALSE]
    parameters <- parameters_all[parameters_all$group == group_index, , drop = FALSE]
    loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs", "est.std"), drop = FALSE]
    factors <- unique(loadings$lhs)
    if (!length(factors)) return(data.frame())
    theta_raw <- as.matrix(if (is.list(theta_all) && !is.null(theta_all[[group_index]])) theta_all[[group_index]] else theta_all)
    theta_standardized <- matrix(0, nrow = length(observed), ncol = length(observed), dimnames = list(observed, observed))
    theta_rows <- standardized$op == "~~" & standardized$lhs %in% observed & standardized$rhs %in% observed
    for (index in which(theta_rows)) {
      lhs <- standardized$lhs[[index]]
      rhs <- standardized$rhs[[index]]
      theta_standardized[lhs, rhs] <- standardized$est.std[[index]]
      theta_standardized[rhs, lhs] <- standardized$est.std[[index]]
    }
    sample_statistics <- if (is.list(sample_statistics_all) && !is.null(sample_statistics_all[[group_index]])) sample_statistics_all[[group_index]] else sample_statistics_all
    sample_covariance <- sample_statistics$cov %||% NULL
    latent_covariance <- as.matrix(if (is.list(latent_covariance_all) && !is.null(latent_covariance_all[[group_index]])) latent_covariance_all[[group_index]] else latent_covariance_all)
    group_rows <- lapply(factors, function(factor) {
      factor_rows <- loadings$lhs == factor
      indicators <- loadings$rhs[factor_rows]
      lambda <- loadings$est.std[factor_rows]
      if (identical(formula_mode, "model_implied")) {
        raw <- parameters$est[parameters$op == "=~" & parameters$lhs == factor & parameters$rhs %in% indicators]
        latent_variance <- diag(latent_covariance)[[factor]]
        residual_variances <- diag(theta_raw)[indicators]
        ave_common <- sum(raw^2 * latent_variance, na.rm = TRUE)
        ave <- ave_common / (ave_common + sum(residual_variances, na.rm = TRUE))
        common <- sum(raw)^2 * latent_variance
      } else {
        ave <- mean(lambda^2, na.rm = TRUE)
        common <- sum(lambda)^2
      }
      theta <- if (identical(formula_mode, "model_implied")) theta_raw else theta_standardized
      error_total <- if (all(indicators %in% rownames(theta))) sum(theta[indicators, indicators, drop = FALSE], na.rm = TRUE) else NA_real_
      cr <- if (is.finite(error_total) && common + error_total > 0) common / (common + error_total) else NA_real_
      data.frame(
        Group = group_labels[[group_index]], Factor = factor, k = length(indicators),
        AVE = ave, CR = cr,
        `Cronbach's alpha` = if (!is.null(sample_covariance)) structural_canvas_cronbach_alpha(sample_covariance, indicators) else NA_real_,
        `Omega total` = cr,
        check.names = FALSE
      )
    })
    do.call(rbind, group_rows)
  })
  rows <- Filter(function(value) is.data.frame(value) && nrow(value), rows)
  if (length(rows)) do.call(rbind, rows) else data.frame()
}
structural_canvas_group_htmt <- function(fit, threshold = .85) {
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label"))
  group_count <- as.integer(lavaan::lavInspect(fit, "ngroups"))
  if (!is.finite(group_count) || group_count < 2L) return(data.frame())
  if (length(group_labels) != group_count) group_labels <- paste("Group", seq_len(group_count))
  standardized <- lavaan::standardizedSolution(fit)
  observed <- lavaan::lavNames(fit, "ov")
  sample_statistics_all <- lavaan::lavInspect(fit, "sampstat")
  rows <- lapply(seq_len(group_count), function(group_index) {
    group_standardized <- standardized[standardized$group == group_index, , drop = FALSE]
    loadings <- group_standardized[group_standardized$op == "=~" & group_standardized$rhs %in% observed, c("lhs", "rhs"), drop = FALSE]
    factor_names <- unique(loadings$lhs)
    if (length(factor_names) < 2L) return(data.frame())
    indicators_by_factor <- stats::setNames(lapply(factor_names, function(name) unique(loadings$rhs[loadings$lhs == name])), factor_names)
    sample_statistics <- if (is.list(sample_statistics_all) && !is.null(sample_statistics_all[[group_index]])) sample_statistics_all[[group_index]] else sample_statistics_all
    covariance <- as.matrix(sample_statistics$cov %||% matrix(numeric(0), 0L, 0L))
    if (!nrow(covariance) || !all(observed %in% rownames(covariance))) return(data.frame())
    variances <- diag(covariance)
    correlations <- if (all(is.finite(variances)) && all(variances > 0)) stats::cov2cor(covariance) else matrix(NA_real_, nrow(covariance), ncol(covariance), dimnames = dimnames(covariance))
    htmt <- structural_canvas_htmt(correlations, indicators_by_factor, threshold = threshold)
    if (is.null(htmt) || !nrow(htmt$pairs)) return(data.frame())
    data.frame(Group = group_labels[[group_index]], htmt$pairs, check.names = FALSE)
  })
  rows <- Filter(function(value) is.data.frame(value) && nrow(value), rows)
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

