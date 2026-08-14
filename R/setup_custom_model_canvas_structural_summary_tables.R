# Structural equation canvas overview and fit result tables.

structural_canvas_summary_result_table <- function(kind, bundle, fit, analysis_type, ko, fmt) {
  if (identical(kind, "overview")) {
    overview_df <- data.frame(
      Item = if (ko) {
        c("분석 방법", "추정 방법", "표본 크기(N)", "관측변수 수", "잠재변수 수", "자유 파라미터 수", "수렴 여부", "적합해 여부")
      } else {
        c("Analysis", "Estimator", "N", "Observed variables", "Latent variables", "Free parameters", "Converged", "Admissible solution")
      },
      Value = c(
        structural_analysis_title(analysis_type, "en"),
        lavaan::lavInspect(fit, "options")$estimator,
        lavaan::lavInspect(fit, "ntotal"),
        length(lavaan::lavNames(fit, "ov")),
        length(lavaan::lavNames(fit, "lv")),
        lavaan::lavInspect(fit, "npar"),
        if (isTRUE(lavaan::lavInspect(fit, "converged"))) "Yes" else "No",
        if (isTRUE(bundle$diagnostics$admissible %||% lavaan::lavInspect(fit, "post.check"))) "Yes" else "No"
      ),
      check.names = FALSE
    )
    names(overview_df) <- if (ko) c("항목", "값") else c("Item", "Value")
    overview_df <- rbind(
      overview_df[1L, , drop = FALSE],
      data.frame(overview_df[1L, , drop = FALSE], check.names = FALSE),
      overview_df[-1L, , drop = FALSE]
    )
    overview_df[2L, 1L] <- if (ko) "분석 맥락" else "Analysis context"
    overview_df[2L, 2L] <- structural_canvas_analysis_context(bundle)
    return(overview_df)
  }

  if (identical(kind, "fit")) {
    ci_level <- as.numeric(bundle$rmsea_ci %||% .90)
    comparison_fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) {
      list(bundle$baseline_fit, fit)
    } else {
      list(fit)
    }
    selections <- structural_canvas_common_fit_measures(comparison_fits, bundle$estimator %||% "ML", ci_level)
    row_labels <- if (ko) "연구모형" else "Research model"
    if (length(selections) > 1L) {
      modified_label <- bundle$comparison_label %||% if (ko) "수정 모형" else "Modified model"
      row_labels <- c(if (ko) "연구모형" else "Research model", modified_label)
    }
    values <- do.call(rbind, lapply(selections, function(item) item$values))
    ci_percent <- round(100 * ci_level)
    table <- data.frame(
      row_labels,
      `χ²` = fmt(values[, 1L]),
      df = fmt(values[, 2L]),
      p = vapply(values[, 3L], format_p, character(1)),
      `χ²/df` = fmt(values[, 4L]),
      CFI = fmt(values[, 5L]),
      TLI = fmt(values[, 6L]),
      SRMR = fmt(values[, 7L]),
      RMSEA = fmt(values[, 8L]),
      fmt(values[, 9L]),
      fmt(values[, 10L]),
      check.names = FALSE
    )
    names(table)[[1L]] <- if (ko) "모형" else "Model"
    names(table)[10:11] <- paste0(ci_percent, "% CI ", c("LLCI", "ULCI"))
    return(table)
  }

  NULL
}
