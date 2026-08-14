# CFA/SEM canvas workbook assembly and Excel writing helpers.

structural_canvas_write_result_workbook <- function(sheets, file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("The openxlsx package is required to export CFA result tables.")
  sheets <- Filter(function(value) is.data.frame(value) || is.matrix(value), sheets)
  if (!length(sheets)) stop("At least one result table is required for Excel export.")
  sanitize_sheet_name <- function(name) {
    characters <- strsplit(as.character(name), "", fixed = TRUE)[[1L]]
    characters[characters %in% c("\\", "/", ":", "*", "?", "[", "]")] <- "_"
    value <- trimws(paste(characters, collapse = ""))
    if (!nzchar(value)) value <- "Sheet"
    substr(value, 1L, 31L)
  }
  candidates <- vapply(names(sheets), sanitize_sheet_name, character(1))
  valid_names <- character(length(candidates))
  used <- character(0)
  for (index in seq_along(candidates)) {
    base <- candidates[[index]]
    candidate <- base
    suffix_index <- 1L
    while (tolower(candidate) %in% tolower(used)) {
      suffix <- paste0("_", suffix_index)
      candidate <- paste0(substr(base, 1L, 31L - nchar(suffix)), suffix)
      suffix_index <- suffix_index + 1L
    }
    valid_names[[index]] <- candidate
    used <- c(used, candidate)
  }
  names(sheets) <- valid_names
  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")
  decimal_style <- openxlsx::createStyle(numFmt = "0.000")
  integer_style <- openxlsx::createStyle(numFmt = "0")
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  for (name in names(sheets)) {
    table <- as.data.frame(sheets[[name]], stringsAsFactors = FALSE, check.names = FALSE)
    openxlsx::addWorksheet(workbook, name)
    openxlsx::writeData(workbook, name, table, withFilter = nrow(table) > 0L)
    if (ncol(table)) {
      openxlsx::addStyle(workbook, name, header_style, rows = 1L, cols = seq_len(ncol(table)), gridExpand = TRUE)
      widths <- vapply(seq_len(ncol(table)), function(column) {
        values <- c(names(table)[[column]], as.character(table[[column]]))
        values <- values[!is.na(values)]
        observed <- if (length(values)) max(nchar(values, type = "width"), na.rm = TRUE) + 2L else 10L
        upper <- if (identical(name, "Notes") && identical(names(table)[[column]], "Note")) 80L else 40L
        min(max(observed, 10L), upper)
      }, numeric(1))
      openxlsx::setColWidths(workbook, name, cols = seq_len(ncol(table)), widths = widths)
      openxlsx::freezePane(workbook, name, firstRow = TRUE)
      if (nrow(table)) {
        body_rows <- seq.int(2L, nrow(table) + 1L)
        numeric_columns <- which(vapply(table, is.numeric, logical(1)))
        for (column in numeric_columns) {
          finite <- table[[column]][is.finite(table[[column]])]
          style <- if (length(finite) && all(finite == trunc(finite))) integer_style else decimal_style
          openxlsx::addStyle(workbook, name, style, rows = body_rows, cols = column, gridExpand = TRUE, stack = TRUE)
        }
        wrapped_columns <- which(vapply(table, function(values) {
          is.character(values) && any(nchar(values, type = "width") > 40L, na.rm = TRUE)
        }, logical(1)))
        if (length(wrapped_columns)) openxlsx::addStyle(
          workbook, name, wrap_style, rows = body_rows, cols = wrapped_columns,
          gridExpand = TRUE, stack = TRUE
        )
      }
    }
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

structural_canvas_workbook_contents <- function(sheet_names) {
  describe <- function(name) {
    if (identical(name, "Contents")) return("Workbook sheet index and interpretation guide.")
    if (identical(name, "Report_Summary")) return("Copy-ready analysis context and key model-fit values for report drafting.")
    if (name %in% c("Overview", "Fit", "Validity", "Measurement")) return("Formatted reporting table; use the corresponding numeric sheet for calculations where available.")
    if (identical(name, "Fit_Numeric")) return("Numeric model-fit statistics, confidence limits, and selected robust/scaled source keys.")
    if (identical(name, "Admissibility_Diagnostics")) return("Numeric eigenvalue, condition-number, boundary-dimension, and reason diagnostics for fitted and compared models.")
    if (identical(name, "RMSEA_Tests")) return("RMSEA close-fit and not-close hypothesis tests using estimator-matched robust/scaled p values when available.")
    if (identical(name, "Information_Criteria")) return("Log-likelihood, AIC, BIC, adjusted BIC, and within-export delta values for likelihood-based model comparison.")
    if (identical(name, "Parameter_Estimates")) return("Numeric unstandardized and standardized parameter estimates with confidence intervals and fixed-parameter flags.")
    if (identical(name, "Latent_Correlations")) return("Numeric latent-variable correlation matrix.")
    if (identical(name, "Reliability_Validity_Numeric")) return("Numeric AVE, sqrt(AVE), reliability, latent-correlation, and Fornell-Larcker results with assessment flags.")
    if (identical(name, "Sample_Descriptives")) return("Sample statistics used by the fitted model: variable means when estimated, variances, standard deviations, and model N.")
    if (identical(name, "Sample_Covariance")) return("Long-form covariance and correlation statistics used by the fitted model.")
    if (identical(name, "Thresholds")) return("Numeric ordered-indicator thresholds.")
    if (identical(name, "Bollen_Stine")) return("Bollen-Stine exact-fit bootstrap p value with Monte Carlo uncertainty and valid-replicate diagnostics.")
    if (identical(name, "Common_Method")) return("Common method bias diagnostic summary including Harman, single-factor CFA, and common latent factor screens when requested.")
    if (identical(name, "Common_Method_Fit")) return("Fit indices for the research model and requested common-method comparison models.")
    if (identical(name, "Common_Method_Comparison")) return("Difference statistics for requested common-method comparison models, including delta chi-square, delta df, delta CFI, delta RMSEA, and delta SRMR.")
    if (identical(name, "Common_Method_Loadings")) return("Standardized loading changes after adding the common latent method factor.")
    if (grepl("^Residual", name) || identical(name, "Large_Residuals")) return("Local-fit residual diagnostic output.")
    if (grepl("^MI_", name)) return("Exploratory modification-index or holdout-validation output; changes require theoretical justification.")
    if (identical(name, "Model_Difference")) return("Nested-model difference-test result or the explicit reason the formal test was suppressed.")
    if (grepl("^Inv", name)) return("Measurement-invariance fit, group, or equality-constraint diagnostic output.")
    if (grepl("_CI$", name)) return("Confidence-interval output; consult Notes and valid-replicate counts before interpretation.")
    if (identical(name, "Model_Syntax")) return("lavaan model syntax used for the fitted model.")
    if (identical(name, "Analysis_Record")) return("Reproducibility record containing analysis options and computational context.")
    if (identical(name, "Notes")) return("Statistical definitions, descriptive cutoffs, caveats, and model-specific warnings.")
    "Supplementary CFA result table."
  }
  data.frame(
    Sheet = sheet_names,
    Description = vapply(sheet_names, describe, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

structural_canvas_result_workbook_sheets <- function(bundle, table_fn) {
  sheets <- list(
    Overview = table_fn("overview"), Report_Summary = structural_canvas_report_summary(bundle), Fit = table_fn("fit"),
    Validity = table_fn("validity"), Measurement = table_fn("measurement")
  )
  if (!is.null(bundle$invariance_result) && nrow(bundle$invariance_result$table)) {
    sheets$Invariance <- bundle$invariance_result$table
    sheets$Invariance_Groups <- bundle$invariance_result$group_diagnostics
    if (!is.null(bundle$invariance_result$group_reliability) && nrow(bundle$invariance_result$group_reliability)) {
      sheets$Invariance_Reliability <- bundle$invariance_result$group_reliability
    }
    if (!is.null(bundle$invariance_result$group_htmt) && nrow(bundle$invariance_result$group_htmt)) {
      sheets$Invariance_HTMT <- bundle$invariance_result$group_htmt
    }
    group_residuals <- bundle$invariance_result$group_residuals %||% list()
    if (isTRUE(group_residuals$available)) {
      if (nrow(group_residuals$group_summary %||% data.frame())) sheets$Invariance_Residual_Summary <- group_residuals$group_summary
      if (nrow(group_residuals$group_pairs %||% data.frame())) sheets$Invariance_Residual_Pairs <- group_residuals$group_pairs
    }
    score_tables <- bundle$invariance_result$score_diagnostics %||% list()
    for (stage in names(score_tables)) if (nrow(score_tables[[stage]])) sheets[[paste0("Inv_Score_", stage)]] <- score_tables[[stage]]
  }
  if (!is.null(bundle$reliability_bootstrap_result) && nrow(bundle$reliability_bootstrap_result)) sheets$Reliability_CI <- bundle$reliability_bootstrap_result
  if (!is.null(bundle$bollen_stine_result) && nrow(bundle$bollen_stine_result)) {
    sheets$Bollen_Stine <- bundle$bollen_stine_result
    sheets$Bollen_Stine$`Model context` <- if (isTRUE(bundle$modified_from_baseline)) "Exploratory modified model" else "Prespecified/original model"
  }
  if (!is.null(bundle$htmt_bootstrap_result) && nrow(bundle$htmt_bootstrap_result)) sheets$HTMT_CI <- bundle$htmt_bootstrap_result
  common_method <- bundle$common_method_result %||% NULL
  if (!is.null(common_method)) {
    common_summary <- structural_canvas_common_method_display_table(common_method, format_values = FALSE)
    if (nrow(common_summary)) sheets$Common_Method <- common_summary
    if (nrow(common_method$fit %||% data.frame())) sheets$Common_Method_Fit <- common_method$fit
    if (nrow(common_method$comparison %||% data.frame())) sheets$Common_Method_Comparison <- common_method$comparison
    if (nrow(common_method$loading_change %||% data.frame())) sheets$Common_Method_Loadings <- common_method$loading_change
  }
  if (!is.null(bundle$mi_history) && nrow(bundle$mi_history)) sheets$MI_History <- bundle$mi_history[, setdiff(names(bundle$mi_history), "Signature"), drop = FALSE]
  if (!is.null(bundle$holdout_comparison)) {
    sheets$MI_Holdout_Fit <- bundle$holdout_comparison$table
    sheets$MI_Holdout_Change <- bundle$holdout_comparison$changes
  }
  if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) sheets$Model_Difference <- structural_canvas_model_difference_report(bundle)
  residuals <- structural_canvas_residual_diagnostics(bundle$fit)
  matrix_sheet <- function(value) {
    value <- as.matrix(value)
    data.frame(Indicator = rownames(value), value, check.names = FALSE)
  }
  if (isTRUE(residuals$available)) {
    sheets$Residual_Z <- matrix_sheet(residuals$standardized)
    sheets$Residual_Correlation <- matrix_sheet(residuals$correlation)
    if (nrow(residuals$largest)) sheets$Large_Residuals <- residuals$largest
  }
  latent_intervals <- structural_canvas_latent_correlation_intervals(bundle$fit, level = .95)
  if (nrow(latent_intervals)) sheets$Latent_Correlation_CI <- latent_intervals
  if (!is.null(bundle$mi) && nrow(bundle$mi)) {
    mi_columns <- intersect(c("step", "skipped_inadmissible", "skipped_details", "lhs", "op", "rhs", "mi", "MI p", "BH-adjusted p", "Multiplicity family size", "epc", "sepc.lv", "sepc.all", "Reason", "cfi_after", "tli_after", "rmsea_after", "srmr_after"), names(bundle$mi))
    sheets$MI_Candidates <- bundle$mi[, mi_columns, drop = FALSE]
  }
  sheets$Model_Syntax <- data.frame(Line = strsplit(as.character(bundle$syntax %||% ""), "\n", fixed = TRUE)[[1L]], check.names = FALSE)
  sheets$Analysis_Record <- data.frame(Record = strsplit(structural_canvas_reproducibility_record(bundle), "\n", fixed = TRUE)[[1L]], check.names = FALSE)
  sheets$Fit_Numeric <- structural_canvas_export_fit_estimates(bundle)
  sheets$Admissibility_Diagnostics <- structural_canvas_export_admissibility(bundle)
  sheets$RMSEA_Tests <- structural_canvas_rmsea_hypothesis_tests(bundle)
  information_criteria <- structural_canvas_information_criteria(bundle)
  if (any(is.finite(information_criteria$AIC)) || any(is.finite(information_criteria$BIC))) sheets$Information_Criteria <- information_criteria
  sheets$Parameter_Estimates <- structural_canvas_export_parameter_estimates(bundle$fit)
  sheets$Latent_Correlations <- structural_canvas_export_latent_correlations(bundle$fit)
  sheets$Reliability_Validity_Numeric <- structural_canvas_export_reliability_validity(bundle)
  sample_statistics <- structural_canvas_export_sample_statistics(bundle$fit)
  if (nrow(sample_statistics$Descriptives)) sheets$Sample_Descriptives <- sample_statistics$Descriptives
  if (nrow(sample_statistics$Covariance)) sheets$Sample_Covariance <- sample_statistics$Covariance
  if (nrow(sample_statistics$Thresholds)) sheets$Thresholds <- sample_statistics$Thresholds
  sheets$Notes <- structural_canvas_export_notes(bundle)
  c(list(Contents = structural_canvas_workbook_contents(c("Contents", names(sheets)))), sheets)
}
