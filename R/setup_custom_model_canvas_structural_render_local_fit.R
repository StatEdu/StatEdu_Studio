structural_canvas_register_local_fit_outputs <- function(output, prefix, fit_result) {
output[[paste0(prefix, "_result_residuals")]] <- renderUI({
  bundle <- fit_result()
  diagnostics <- structural_canvas_residual_diagnostics(bundle$fit)
  if (!isTRUE(diagnostics$available)) return(NULL)
  matrix_table <- function(matrix_value, title) {
    values <- matrix("", nrow(matrix_value), ncol(matrix_value) + 1L)
    colnames(values) <- c("Indicator", colnames(matrix_value))
    for (row_index in seq_len(nrow(matrix_value))) {
      values[row_index, 1L] <- rownames(matrix_value)[[row_index]]
      for (column_index in seq_len(ncol(matrix_value))) {
        value <- matrix_value[row_index, column_index]
        if (is.finite(value)) values[row_index, column_index + 1L] <- format_decimal3(value)
      }
    }
    tagList(tags$h5(title), tags$table(class = "table table-striped table-bordered structural-residual-matrix",
      tags$thead(tags$tr(lapply(colnames(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    ))
  }
  largest <- diagnostics$largest
  if (nrow(largest)) {
    largest[["Standardized residual"]] <- vapply(largest[["Standardized residual"]], format_decimal3, character(1))
    largest[["Correlation residual"]] <- vapply(largest[["Correlation residual"]], format_decimal3, character(1))
  }
  div(class = "result-section regression-result-panel structural-residual-result",
    h4("5. Local fit diagnostics"),
    matrix_table(diagnostics$standardized, "Standardized residual matrix"),
    matrix_table(diagnostics$correlation, "Correlation residual matrix"),
    tags$h5(paste0("Large standardized residuals (|z| >= ", diagnostics$cutoff, ")")),
    if (!nrow(largest)) tags$p("No residuals exceeded the cutoff.") else tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(largest), tags$th))),
      tags$tbody(lapply(seq_len(nrow(largest)), function(index) tags$tr(lapply(as.character(largest[index, ]), tags$td))))
    ),
    tags$p(class = "structural-result-note", "Large standardized residuals identify local areas of model misfit and should be interpreted with theory rather than used as automatic modification instructions.")
  )
})
output[[paste0(prefix, "_result_higher_order")]] <- renderUI({
  bundle <- fit_result()
  higher <- structural_canvas_higher_order_results(bundle$snapshot %||% list(), bundle$fit)
  if (!isTRUE(higher$available)) return(NULL)
  table <- higher$table
  fixed <- !is.na(table$SE) & table$SE == 0 & is.na(table$z) & is.na(table$p)
  residual_abnormal <- !is.finite(table$ResidualVariance) | table$ResidualVariance < 0 | table$ResidualVariance > 1
  residual_display <- paste0(vapply(table$ResidualVariance, format_decimal3, character(1)), ifelse(residual_abnormal, "†", ""))
  r2_interval_abnormal <- !is.finite(table$R2CILower) | !is.finite(table$R2CIUpper) | table$R2CILower < 0 | table$R2CIUpper > 1
  loading_guidance <- vapply(table$Beta, structural_canvas_higher_order_loading_guidance, character(1))
  display <- data.frame(
    `Higher-order factor` = table$HigherOrderFactor,
    `Lower-order factor` = table$LowerOrderFactor,
    B = vapply(table$B, format_decimal3, character(1)),
    `B 95% CI lower` = vapply(table$BCILower, format_decimal3, character(1)),
    `B 95% CI upper` = vapply(table$BCIUpper, format_decimal3, character(1)),
    SE = vapply(table$SE, format_decimal3, character(1)),
    Beta = vapply(table$Beta, format_decimal3, character(1)),
    `β 95% CI lower` = vapply(table$BetaCILower, format_decimal3, character(1)),
    `β 95% CI upper` = vapply(table$BetaCIUpper, format_decimal3, character(1)),
    `R²` = vapply(table$R2, format_decimal3, character(1)),
    `R² 95% CI lower` = paste0(vapply(table$R2CILower, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
    `R² 95% CI upper` = paste0(vapply(table$R2CIUpper, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
    `Residual variance` = residual_display,
    Guidance = ifelse(residual_abnormal | r2_interval_abnormal, "Review residual/R² interval", loading_guidance),
    z = vapply(table$z, format_decimal3, character(1)),
    p = vapply(table$p, format_p, character(1)),
    check.names = FALSE
  )
  display$SE[fixed] <- "Fixed*"
  display$z[fixed] <- "—"
  display$p[fixed] <- "—"
  omega_h <- structural_canvas_omega_h(bundle$snapshot %||% list(), bundle$fit)
  div(class = "result-section regression-result-panel structural-higher-order-result",
    h4("Higher-order CFA results"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    if (isTRUE(omega_h$available)) tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th("Higher-order factor"), tags$th("Indicators"), tags$th("Hierarchical omega (ωh)"), tags$th("Guidance"))),
      tags$tbody(tags$tr(
        tags$td(omega_h$higher_order_factor), tags$td(omega_h$indicators),
        tags$td(paste0(format_decimal3(omega_h$omega_h), if (!is.finite(omega_h$omega_h) || omega_h$omega_h < 0 || omega_h$omega_h > 1) "†" else "")),
        tags$td(structural_canvas_omega_h_guidance(omega_h$omega_h))
      ))
    )) else tags$p(class = "structural-result-note", paste0("Hierarchical omega was not reported: ", omega_h$reason)),
    tags$p(class = "structural-result-note", "Lower-order R² is the variance explained by the higher-order factor. Residual variance is reported on the standardized latent-variable scale."),
    tags$p(class = "structural-result-note", "Lower-order R² intervals complement the standardized residual-variance intervals. † also marks an R² interval extending beyond [0, 1]."),
    tags$p(class = "structural-result-note", "Higher-order standardized-loading confidence intervals are 95% delta-method intervals from lavaan."),
    tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized higher-order loadings; a fixed reference loading has a degenerate interval at its fixed value."),
    tags$p(class = "structural-result-note", "ωh estimates the proportion of unit-weighted total-score variance attributable to one higher-order general factor under the fitted higher-order CFA model."),
    tags$p(class = "structural-result-note", "The .40 loading and .70 ωh values are descriptive review guidelines, not universal pass/fail rules. † marks an unavailable value or a coefficient/residual variance outside [0, 1]."),
    tags$p(class = "structural-result-note", "* Fixed reference loading; SE, z, and p are not estimated.")
  )
})
  invisible(TRUE)
}
