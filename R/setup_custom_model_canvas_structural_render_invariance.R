# Structural measurement invariance result rendering.

structural_canvas_invariance_result_ui <- function(bundle) {
  result <- bundle$invariance_result %||% NULL
  if (is.null(result)) return(NULL)
  table <- result$table
  group_table <- result$group_diagnostics
  group_reliability <- result$group_reliability %||% data.frame()
  group_htmt <- result$group_htmt %||% data.frame()
  group_residuals <- result$group_residuals %||% list()
  residual_summary <- if (isTRUE(group_residuals$available)) group_residuals$group_summary %||% data.frame() else data.frame()
  residual_largest <- if (isTRUE(group_residuals$available)) group_residuals$group_largest %||% data.frame() else data.frame()
  group_table[["Indicator missing %"]] <- paste0(vapply(group_table[["Indicator missing %"]], format_decimal3, character(1)), "%")
  if (nrow(group_reliability)) {
    for (name in intersect(c("AVE", "CR", "Cronbach's alpha", "Omega total"), names(group_reliability))) {
      group_reliability[[name]] <- vapply(group_reliability[[name]], format_decimal3, character(1))
    }
  }
  if (nrow(group_htmt)) {
    for (name in intersect(c("HTMT"), names(group_htmt))) {
      group_htmt[[name]] <- vapply(group_htmt[[name]], format_decimal3, character(1))
    }
  }
  if (nrow(residual_summary)) {
    residual_summary[["Max |standardized residual|"]] <- vapply(residual_summary[["Max |standardized residual|"]], format_decimal3, character(1))
  }
  if (nrow(residual_largest)) {
    residual_largest[["Standardized residual"]] <- vapply(residual_largest[["Standardized residual"]], format_decimal3, character(1))
    residual_largest[["Correlation residual"]] <- vapply(residual_largest[["Correlation residual"]], format_decimal3, character(1))
  }
  srmr_limit <- ifelse(table$Model == "Metric", .030, .010)
  table$Decision <- ifelse(
    !table$Admissible, "Inadmissible stage",
    ifelse(table$Model == "Configural", "Baseline stage",
    ifelse(table$Admissible & table$DeltaCFI >= -.010 & table$DeltaRMSEA <= .015 & table$DeltaSRMR <= srmr_limit, "Change criteria met", "Review noninvariance")
  ))
  reviewed_stages <- table$Model[table$Decision == "Review noninvariance"]
  score_tables <- (result$score_diagnostics %||% list())[intersect(reviewed_stages, names(result$score_diagnostics %||% list()))]
  score_tables <- Filter(function(value) nrow(value), score_tables)
  numeric_columns <- c("Chisq", "df", "CFI", "RMSEA", "SRMR", "DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf", "Residual min eigenvalue", "Latent min eigenvalue", "Parameter min eigenvalue")
  for (name in numeric_columns) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
  for (name in c("Residual condition number", "Latent condition number", "Parameter condition number")) table[[name]] <- vapply(table[[name]], function(value) if (is.finite(value)) format(value, scientific = TRUE, digits = 3) else "Inf", character(1))
  table$p <- vapply(table$p, format_p, character(1))
  table$DeltaP <- vapply(table$DeltaP, format_p, character(1))
  names(table)[names(table) == "DeltaCFI"] <- "ΔCFI"
  names(table)[names(table) == "DeltaRMSEA"] <- "ΔRMSEA"
  names(table)[names(table) == "DeltaSRMR"] <- "ΔSRMR"
  names(table)[names(table) == "DeltaChisq"] <- "Δχ²"
  names(table)[names(table) == "DeltaDf"] <- "Δdf"
  names(table)[names(table) == "DeltaP"] <- "Δp"
  div(class = "result-section regression-result-panel structural-invariance-result",
    h4(paste0("Measurement invariance by ", result$group)),
    tags$h5("Group-level data diagnostics"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(group_table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(group_table)), function(index) tags$tr(lapply(as.character(group_table[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", "Group N below 100 is flagged as a descriptive small-group warning, not a universal minimum. Adequacy depends on model complexity, indicator quality, estimator, missingness, category distribution, and effect size."),
    if (nrow(residual_summary)) tagList(
      tags$h5("Group-specific configural residual diagnostics"),
      tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(lapply(names(residual_summary), tags$th))),
        tags$tbody(lapply(seq_len(nrow(residual_summary)), function(index) tags$tr(lapply(as.character(residual_summary[index, ]), tags$td))))
      )),
      if (nrow(residual_largest)) tagList(
        tags$h5(paste0("Large group-specific residuals (|z| >= ", group_residuals$cutoff %||% 1.96, ")")),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(residual_largest), tags$th))),
          tags$tbody(lapply(seq_len(nrow(residual_largest)), function(index) tags$tr(lapply(as.character(residual_largest[index, ]), tags$td))))
        ))
      ) else tags$p(class = "structural-result-note", paste0("No group-specific configural residuals exceeded |z| >= ", group_residuals$cutoff %||% 1.96, ".")),
      tags$p(class = "structural-result-note", "These residual diagnostics are computed separately within each group from the configural model. For WLSMV ordered indicators they are approximate local-fit diagnostics on the fitted latent-response/polychoric scale; use them to locate candidate areas for review, not as automatic modification instructions.")
    ),
    if (nrow(group_reliability)) tagList(
      tags$h5("Group-specific reliability and convergent validity"),
      tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(lapply(names(group_reliability), tags$th))),
        tags$tbody(lapply(seq_len(nrow(group_reliability)), function(index) tags$tr(lapply(as.character(group_reliability[index, ]), tags$td))))
      )),
      tags$p(class = "structural-result-note", "Group-specific AVE, CR, alpha, and omega are computed from the configural model, before imposing equality constraints. Use them as descriptive group diagnostics, not as formal invariance tests.")
    ),
    if (nrow(group_htmt)) tagList(
      tags$h5("Group-specific HTMT"),
      tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(lapply(names(group_htmt), tags$th))),
        tags$tbody(lapply(seq_len(nrow(group_htmt)), function(index) tags$tr(lapply(as.character(group_htmt[index, ]), tags$td))))
      )),
      tags$p(class = "structural-result-note", "Group-specific HTMT uses each group's configural-model sample correlation matrix. Bootstrap intervals remain single-group in this release.")
    ),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
    )),
    if (any(!result$table$Admissible)) tags$p(class = "structural-result-note", "An inadmissible invariance stage failed the same full checks as the main CFA. Its change indices, formal difference test, and equality-constraint score diagnostics are suppressed; resolve the stage-specific variance, covariance-matrix, df, or latent-correlation problem before judging invariance."),
    tags$p(class = "structural-result-note", "Parameter boundary dimensions counts near-zero eigenvalues in the parameter covariance matrix. Boundary dimensions up to the number of explicit equality constraints are treated as constraint-induced; any excess is flagged as unexplained empirical underidentification."),
    tags$p(class = "structural-result-note", "Minimum eigenvalues report the worst group-specific residual and latent covariance eigenvalues and the parameter covariance minimum. Negative values beyond numerical tolerance indicate non-positive-definiteness; values near zero indicate a boundary or singular direction."),
    if (any(result$table$`Ill-conditioned warning`)) tags$p(class = "structural-result-note", "Ill-conditioned warning marks a residual, latent, or parameter covariance condition number above 1e8 (or an infinite value). This indicates numerical sensitivity but is not by itself proof of inadmissibility."),
    if (length(score_tables)) tagList(
      tags$h5("Largest equality-constraint score tests"),
      lapply(names(score_tables), function(stage) {
        score_table <- utils::head(score_tables[[stage]], 10L)
        score_table[["Score χ²"]] <- vapply(score_table[["Score χ²"]], format_decimal3, character(1))
        score_table[["Raw χ²"]] <- vapply(score_table[["Raw χ²"]], format_decimal3, character(1))
        score_table$p <- vapply(score_table$p, format_p, character(1))
        score_table[["BH-adjusted p"]] <- vapply(score_table[["BH-adjusted p"]], format_p, character(1))
        score_table[["Max |standardized EPC|"]] <- vapply(score_table[["Max |standardized EPC|"]], format_decimal3, character(1))
        score_table[["Raw p"]] <- vapply(score_table[["Raw p"]], format_p, character(1))
        score_table[["Raw BH-adjusted p"]] <- vapply(score_table[["Raw BH-adjusted p"]], format_p, character(1))
        tagList(tags$h6(stage), tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(score_table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(score_table)), function(index) tags$tr(lapply(as.character(score_table[index, ]), tags$td))))
        )))
      }),
      tags$p(class = "structural-result-note", "Score tests rank equality constraints that contribute to stage misfit. Group labels are the observed grouping-variable categories. Max |standardized EPC| is the largest absolute fully standardized expected parameter change among the parameters in that equality constraint and provides an effect-size-oriented diagnostic. BH-adjusted p values control the false-discovery rate within each invariance stage. These remain exploratory diagnostics and do not automatically establish partial invariance or justify freeing a constraint.")
    ),
    tags$p(class = "structural-result-note", if (isTRUE(result$ordinal)) "For ordered indicators, nested theta-parameterized stages constrain thresholds, then thresholds plus loadings (scalar/strong invariance), then residual variances (strict). A separate conventional metric-only stage is not reported because categorical identification depends jointly on thresholds, loadings, scales, and intercepts." else "Nested stages constrain loadings (metric), then intercepts (scalar), then residual variances (strict). Δ values compare each row with the immediately preceding stage; robust/scaled fit indices and difference tests are used when available."),
    if (isTRUE(result$ordinal)) tags$p(class = "structural-result-note", "Ordered-indicator models use WLSMV/DWLS, category thresholds, and theta parameterization. Scalar invariance therefore means invariant thresholds and loadings, not equality of observed-variable intercepts as in continuous CFA."),
    tags$p(class = "structural-result-note", "Descriptive change guidance flags ΔCFI < −.010, ΔRMSEA > .015, or ΔSRMR > .030 for metric and > .010 for scalar/strict invariance. These guidelines should be considered jointly with parameter changes, group sizes, theory, and model admissibility; Δχ² is sample-size sensitive."),
    tags$p(class = "structural-result-note", "Failure at a stage does not justify automatically freeing parameters. Partial invariance requires substantively defensible constraints and transparent reporting.")
  )

}
