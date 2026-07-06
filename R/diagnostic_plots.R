plot_residual_qq <- function(result) {
  residuals <- stats::residuals(result$model)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mar = c(4, 4, 1.5, 1), pty = "s", cex.axis = .8)
  stats::qqnorm(
    residuals,
    pch = 19,
    col = "#475569",
    cex = .75,
    main = "",
    xlab = "Theoretical quantiles",
    ylab = "Sample quantiles"
  )
  stats::qqline(residuals, col = "#1f2937", lwd = 1.2)
  box()
}

plot_residual_homoscedasticity <- function(result) {
  fitted_values <- stats::fitted(result$model)
  residuals <- stats::rstandard(result$model)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mar = c(4, 4, 1.5, 1), pty = "s", cex.axis = .8)
  plot(
    fitted_values,
    residuals,
    pch = 19,
    col = "#475569",
    cex = .75,
    main = "",
    xlab = "Fitted values",
    ylab = "Standardized residuals"
  )
  abline(h = 0, col = "#1f2937", lwd = 1.1)
  if (any(abs(residuals) > 3, na.rm = TRUE)) {
    abline(h = c(-3, 3), col = "#991b1b", lty = 2, lwd = 1)
  }
  if (length(fitted_values) > 2 && length(unique(fitted_values)) > 1) {
    lines(stats::lowess(fitted_values, residuals), col = "#2563eb", lwd = 1.2)
  }
  box()
}
