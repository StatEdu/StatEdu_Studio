# Structural equation canvas validity result tables.

structural_canvas_validity_result_table <- function(kind, bundle, snapshot, fit, ko, fmt, display_name) {
    if (identical(kind, "validity")) {
      standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
      loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
      observed_names <- lavaan::lavNames(fit, "ov")
      loadings <- loadings[loadings$rhs %in% observed_names, , drop = FALSE]
      latent_names <- unique(loadings$lhs)
      indicator_counts <- stats::setNames(vapply(latent_names, function(name) sum(loadings$lhs == name), integer(1)), latent_names)
      formula_mode <- bundle$validity_formula %||% "standardized"
      if (identical(formula_mode, "model_implied")) {
        parameters <- lavaan::parameterEstimates(fit)
        latent_variance <- diag(lavaan::lavInspect(fit, "cov.lv"))
        residuals <- parameters[parameters$op == "~~" & parameters$lhs == parameters$rhs, c("lhs", "est"), drop = FALSE]
        theta_matrix <- as.matrix(lavaan::lavInspect(fit, "theta"))
        ave <- stats::setNames(vapply(latent_names, function(name) {
          factor_loadings <- parameters$est[parameters$op == "=~" & parameters$lhs == name]
          indicators <- parameters$rhs[parameters$op == "=~" & parameters$lhs == name]
          theta <- residuals$est[match(indicators, residuals$lhs)]
          common <- sum(factor_loadings^2 * latent_variance[[name]], na.rm = TRUE)
          common / (common + sum(theta, na.rm = TRUE))
        }, numeric(1)), latent_names)
        cr <- stats::setNames(vapply(latent_names, function(name) {
          factor_loadings <- parameters$est[parameters$op == "=~" & parameters$lhs == name]
          indicators <- parameters$rhs[parameters$op == "=~" & parameters$lhs == name]
          common <- sum(factor_loadings)^2 * latent_variance[[name]]
          theta <- theta_matrix[indicators, indicators, drop = FALSE]
          denominator <- common + sum(theta, na.rm = TRUE)
          if (is.finite(denominator) && denominator > 0) common / denominator else NA_real_
        }, numeric(1)), latent_names)
      } else {
        standardized_parameters <- lavaan::standardizedSolution(fit)
        theta_matrix <- matrix(0, nrow = length(observed_names), ncol = length(observed_names), dimnames = list(observed_names, observed_names))
        theta_rows <- standardized_parameters$op == "~~" & standardized_parameters$lhs %in% observed_names & standardized_parameters$rhs %in% observed_names
        for (index in which(theta_rows)) {
          lhs <- standardized_parameters$lhs[[index]]
          rhs <- standardized_parameters$rhs[[index]]
          theta_matrix[lhs, rhs] <- standardized_parameters$est.std[[index]]
          theta_matrix[rhs, lhs] <- standardized_parameters$est.std[[index]]
        }
        ave <- stats::setNames(vapply(latent_names, function(name) mean(loadings$est.std[loadings$lhs == name]^2, na.rm = TRUE), numeric(1)), latent_names)
        cr <- stats::setNames(vapply(latent_names, function(name) {
          lambda <- loadings$est.std[loadings$lhs == name]
          indicators <- loadings$rhs[loadings$lhs == name]
          common <- sum(lambda)^2
          theta <- theta_matrix[indicators, indicators, drop = FALSE]
          denominator <- common + sum(theta, na.rm = TRUE)
          if (is.finite(denominator) && denominator > 0) common / denominator else NA_real_
        }, numeric(1)), latent_names)
      }
      correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
      if (!length(dim(correlations))) correlations <- matrix(correlations, nrow = 1L, ncol = 1L)
      if (is.null(rownames(correlations))) rownames(correlations) <- latent_names
      if (is.null(colnames(correlations))) colnames(correlations) <- latent_names
      missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
      fl <- structural_canvas_fornell_larcker(ave, correlations, indicator_counts, assessable = !length(missing_covariances))
      sample_statistics <- lavaan::lavInspect(fit, "sampstat")
      sample_covariance <- sample_statistics$cov %||% NULL
      alpha <- stats::setNames(vapply(latent_names, function(name) {
        if (is.null(sample_covariance)) return(NA_real_)
        structural_canvas_cronbach_alpha(sample_covariance, loadings$rhs[loadings$lhs == name])
      }, numeric(1)), latent_names)
      omega <- cr
      constrained_single_factors <- structural_canvas_constrained_single_indicators(snapshot)
      table <- matrix("", nrow = length(latent_names), ncol = length(latent_names) + 6L)
      colnames(table) <- c("Latent", vapply(latent_names, display_name, character(1)), "Max |r|", "AVE", "CR", "Cronbach's alpha", "Omega total")
      for (row in seq_along(latent_names)) {
        latent_name <- latent_names[[row]]
        single_indicator <- indicator_counts[[latent_name]] < 2L
        constrained_single <- single_indicator && latent_name %in% constrained_single_factors
        ave_value <- ave[[latent_name]]
        table[row, 1] <- display_name(latent_name)
        for (column in seq_along(latent_names)) {
          if (row == column) table[row, column + 1L] <- if (single_indicator && !constrained_single) "(N/A‡)" else if (!is.finite(ave_value) || ave_value < 0) "(N/A†)" else paste0("(", format_decimal3(sqrt(ave_value)), if (ave_value > 1) "†" else if (constrained_single) "¶" else "", ")")
          if (row > column) table[row, column + 1L] <- format_decimal3(correlations[latent_name, latent_names[[column]]])
        }
        table[row, ncol(table) - 4L] <- if (is.finite(fl$max_correlation[[latent_name]])) format_decimal3(fl$max_correlation[[latent_name]]) else "—"
        ave_marker <- if (!is.finite(ave_value) || ave_value < 0 || ave_value > 1) "†" else ""
        table[row, ncol(table) - 3L] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(ave_value), ave_marker, if (constrained_single && !nzchar(ave_marker)) "¶" else "")
        cr_value <- cr[[latent_name]]
        cr_marker <- if (!is.finite(cr_value) || cr_value < 0 || cr_value > 1) "†" else ""
        table[row, ncol(table) - 2L] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(cr_value), cr_marker, if (constrained_single && !nzchar(cr_marker)) "¶" else "")
        alpha_value <- alpha[[latent_name]]
        alpha_marker <- if (!is.finite(alpha_value) || alpha_value < 0 || alpha_value > 1) "†" else ""
        table[row, ncol(table) - 1L] <- if (single_indicator) if (constrained_single) "N/A¶" else "N/A‡" else paste0(format_decimal3(alpha_value), alpha_marker)
        omega_value <- omega[[latent_name]]
        omega_marker <- if (!is.finite(omega_value) || omega_value < 0 || omega_value > 1) "†" else ""
        table[row, ncol(table)] <- if (single_indicator && !constrained_single) "N/A‡" else paste0(format_decimal3(omega_value), omega_marker, if (constrained_single && !nzchar(omega_marker)) "¶" else "")
      }
      return(as.data.frame(table, check.names = FALSE))
    }
  NULL
}
