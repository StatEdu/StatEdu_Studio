# Structural equation canvas measurement loading result tables.

structural_canvas_measurement_result_table <- function(kind, fit, ko, fmt, display_name) {
  if (!kind %in% c("measurement", "measurement_ci", "measurement_diagnostics")) return(NULL)

  raw <- lavaan::parameterEstimates(fit)
  parameter_table <- lavaan::parameterTable(fit)
  standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
  rows <- raw$op == "=~"
  raw <- raw[rows, c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper"), drop = FALSE]
  loading_parameters <- parameter_table[parameter_table$op == "=~", c("lhs", "rhs", "free"), drop = FALSE]
  standardized_loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE]

  raw_key <- paste(raw$lhs, raw$rhs, sep = "\r")
  standardized_key <- paste(standardized_loadings$lhs, standardized_loadings$rhs, sep = "\r")
  standardized_match <- match(raw_key, standardized_key)
  beta <- standardized_loadings$est.std[standardized_match]
  beta_ci_lower <- standardized_loadings$ci.lower[standardized_match]
  beta_ci_upper <- standardized_loadings$ci.upper[standardized_match]

  standardized_residuals <- standardized[
    standardized$op == "~~" & standardized$lhs == standardized$rhs & standardized$lhs %in% raw$rhs,
    c("lhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE
  ]
  residual_match <- match(raw$rhs, standardized_residuals$lhs)
  residual_variance <- standardized_residuals$est.std[residual_match]
  r2_ci_lower <- 1 - standardized_residuals$ci.upper[residual_match]
  r2_ci_upper <- 1 - standardized_residuals$ci.lower[residual_match]
  r2_ci_abnormal <- !is.finite(r2_ci_lower) | !is.finite(r2_ci_upper) | r2_ci_lower < 0 | r2_ci_upper > 1
  r2_ci_lower_display <- paste0(fmt(r2_ci_lower), ifelse(r2_ci_abnormal, "†", ""))
  r2_ci_upper_display <- paste0(fmt(r2_ci_upper), ifelse(r2_ci_abnormal, "†", ""))
  residual_marker <- ifelse(!is.finite(residual_variance) | residual_variance < 0 | residual_variance > 1, "†", "")
  residual_display <- paste0(fmt(residual_variance), residual_marker)

  cross_loaded <- duplicated(raw$rhs) | duplicated(raw$rhs, fromLast = TRUE)
  loading_guidance <- mapply(
    structural_canvas_indicator_loading_guidance,
    beta, beta_ci_lower, beta_ci_upper, residual_variance, cross_loaded,
    USE.NAMES = FALSE
  )

  parameter_key <- paste(loading_parameters$lhs, loading_parameters$rhs, sep = "\r")
  parameter_match <- match(raw_key, parameter_key)
  fixed <- loading_parameters$free[parameter_match] == 0L
  fixed[is.na(fixed)] <- raw$se[is.na(fixed)] == 0 & is.na(raw$z[is.na(fixed)]) & is.na(raw$pvalue[is.na(fixed)])

  se <- fmt(raw$se)
  z <- fmt(raw$z)
  p <- vapply(raw$pvalue, format_p, character(1))
  r2_values <- lavaan::lavInspect(fit, "r2")
  r2 <- as.numeric(r2_values[raw$rhs])
  se[fixed] <- "—"
  z[fixed] <- "—"
  p[fixed] <- "—"

  table <- data.frame(
    vapply(raw$lhs, display_name, character(1)),
    vapply(raw$rhs, display_name, character(1)),
    B = fmt(raw$est),
    `B 95% CI lower` = fmt(raw$ci.lower),
    `B 95% CI upper` = fmt(raw$ci.upper),
    SE = se,
    beta = fmt(beta),
    `beta 95% CI lower` = fmt(beta_ci_lower),
    `beta 95% CI upper` = fmt(beta_ci_upper),
    R2 = fmt(r2),
    `R2 95% CI lower` = r2_ci_lower_display,
    `R2 95% CI upper` = r2_ci_upper_display,
    `Std. residual variance` = residual_display,
    `Cross-loading` = ifelse(cross_loaded, "Yes", "No"),
    Guidance = loading_guidance,
    z = z,
    p = p,
    check.names = FALSE
  )
  names(table)[1:2] <- c("Latent", "Indicator")
  names(table)[names(table) == "R2"] <- "R²"
  names(table)[names(table) == "R2 95% CI lower"] <- "R² 95% CI lower"
  names(table)[names(table) == "R2 95% CI upper"] <- "R² 95% CI upper"

  if (identical(kind, "measurement_ci")) {
    return(table[, c(
      "Latent", "Indicator",
      "B 95% CI lower", "B 95% CI upper",
      "beta 95% CI lower", "beta 95% CI upper",
      "R² 95% CI lower", "R² 95% CI upper"
    ), drop = FALSE])
  }

  if (identical(kind, "measurement_diagnostics")) {
    return(table[, c(
      "Latent", "Indicator",
      "Std. residual variance", "Cross-loading", "Guidance"
    ), drop = FALSE])
  }

  table[, c(
    "Latent", "Indicator", "B", "SE", "beta", "z", "p", "R²"
  ), drop = FALSE]
}
