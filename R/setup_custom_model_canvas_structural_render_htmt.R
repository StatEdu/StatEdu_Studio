# Structural equation canvas HTMT render outputs.

structural_canvas_register_htmt_outputs <- function(output, prefix, fit_result, result_table = NULL,
                                                    app_language_fn = NULL, table_number_fn = NULL) {
  render_htmt_tables <- function(include_details = FALSE) {
    bundle <- fit_result()
    language <- statedu_current_language(app_language_fn)
    tr <- function(en, ko) statedu_localized_text(language, en, ko)
    fit <- bundle$fit
    standardized <- lavaan::standardizedSolution(fit)
    loadings <- standardized[standardized$op == "=~", c("lhs", "rhs"), drop = FALSE]
    loadings <- loadings[loadings$rhs %in% lavaan::lavNames(fit, "ov"), , drop = FALSE]
    factor_names <- unique(loadings$lhs)
    if (length(factor_names) < 2L) return(NULL)

    factor_display_names <- factor_names
    if (is.function(result_table)) {
      validity_table <- tryCatch(result_table("validity"), error = function(e) NULL)
      if (is.data.frame(validity_table) && ncol(validity_table) > length(factor_names)) {
        candidates <- names(validity_table)[seq.int(2L, length.out = length(factor_names))]
        candidates <- candidates[nzchar(candidates)]
        if (length(candidates) == length(factor_names)) factor_display_names <- candidates
      }
    }
    factor_display_map <- stats::setNames(factor_display_names, factor_names)
    indicators_by_factor <- stats::setNames(lapply(factor_names, function(name) unique(loadings$rhs[loadings$lhs == name])), factor_names)
    sample_statistics <- lavaan::lavInspect(fit, "sampstat")
    sample_covariance <- sample_statistics$cov %||% NULL
    if (is.null(sample_covariance)) {
      return(result_note_paragraph(class = "structural-result-note", tr("HTMT is not currently displayed for multigroup sample statistics.", "다집단 표본통계에 대한 HTMT는 현재 결과 화면에 표시하지 않습니다.")))
    }

    sample_correlations <- stats::cov2cor(as.matrix(sample_covariance))
    threshold <- as.numeric(bundle$htmt_threshold %||% .85)
    htmt <- structural_canvas_htmt(sample_correlations, indicators_by_factor, threshold)
    matrix_values <- matrix("", nrow = length(factor_names), ncol = length(factor_names) + 1L)
    colnames(matrix_values) <- c("Factor", factor_display_names)
    for (row in seq_along(factor_names)) {
      matrix_values[row, 1L] <- factor_display_names[[row]]
      for (column in seq_along(factor_names)) {
        if (row == column) matrix_values[row, column + 1L] <- "—"
        else if (row > column) matrix_values[row, column + 1L] <- format_decimal3(htmt$matrix[row, column])
      }
    }

    pair_table <- htmt$pairs
    names(pair_table)[names(pair_table) == "Factor1"] <- "Factor 1"
    names(pair_table)[names(pair_table) == "Factor2"] <- "Factor 2"
    if ("HTMT" %in% names(pair_table)) pair_table$HTMT <- vapply(pair_table$HTMT, format_decimal3, character(1))
    if ("Reason" %in% names(pair_table)) pair_table$Reason[!nzchar(pair_table$Reason)] <- "—"
    pair_columns <- intersect(c("Factor 1", "Factor 2", "HTMT", "Criterion", "Reason"), names(pair_table))

    bootstrap_reps <- as.integer(bundle$htmt_bootstrap %||% 0L)
    bootstrap_seed <- as.integer(bundle$htmt_seed %||% default_seed())
    htmt_ci_method <- structural_canvas_bootstrap_ci_method(bundle$htmt_ci_method %||% "bias_corrected")
    htmt_ci_label <- if (identical(htmt_ci_method, "bca")) "BCa" else if (identical(htmt_ci_method, "bias_corrected")) "bias-corrected (BC)" else "percentile"
    bootstrap_table <- NULL
    bootstrap_incomplete <- FALSE
    bootstrap_caution <- FALSE
    bootstrap_unreliable <- FALSE
    bootstrap_bca_unavailable <- FALSE
    bootstrap_canceled <- isTRUE(bundle$cfa_bootstrap_canceled)
    if (bootstrap_reps > 0L) {
      bootstrap_table <- bundle$htmt_bootstrap_result %||% NULL
      if (!is.null(bootstrap_table)) {
        bootstrap_incomplete <- any(bootstrap_table[["Valid replicates"]] < bootstrap_reps)
        bootstrap_caution <- any(bootstrap_table$Status == "Caution")
        bootstrap_unreliable <- any(bootstrap_table$Status == "Unreliable")
        bootstrap_bca_unavailable <- "CI method" %in% names(bootstrap_table) && any(bootstrap_table[["CI method"]] == "BCa unavailable")
        names(bootstrap_table)[names(bootstrap_table) == "Lower"] <- "95% CI lower"
        names(bootstrap_table)[names(bootstrap_table) == "Upper"] <- "95% CI upper"
        names(bootstrap_table)[names(bootstrap_table) == "One-sided upper"] <- "One-sided 95% upper"
        bootstrap_table[["95% CI lower"]] <- vapply(bootstrap_table[["95% CI lower"]], format_decimal3, character(1))
        bootstrap_table[["95% CI upper"]] <- vapply(bootstrap_table[["95% CI upper"]], format_decimal3, character(1))
        bootstrap_table[["One-sided 95% upper"]] <- vapply(bootstrap_table[["One-sided 95% upper"]], format_decimal3, character(1))
        bootstrap_table[["Valid %"]] <- paste0(vapply(bootstrap_table[["Valid %"]], format_decimal3, character(1)), "%")
      }
    }

    if (!isTRUE(include_details)) {
      return(tagList(
        tags$h5(paste0("HTMT (threshold = ", format(threshold, nsmall = 2L), ")")),
        structural_canvas_basic_html_table(
          as.data.frame(matrix_values, check.names = FALSE),
          class = "table table-striped table-bordered structural-htmt-matrix",
          role = "main",
          orientation = "auto"
        ),
        structural_canvas_htmt_ci_html(bundle, htmt$pairs, bundle$htmt_bootstrap_result,
          display_map = factor_display_map)
      ))
    }

    validity_number <- if (is.function(table_number_fn)) table_number_fn("validity") else NA_character_
    htmt_ci_label <- if (identical(htmt_ci_method, "bca")) "BCa" else if (identical(htmt_ci_method, "bias_corrected")) tr("Bias-corrected (BC)", "편향보정 (BC)") else tr("Percentile", "백분위수")
    validity_guide_title <- if (length(validity_number) && !is.na(validity_number) && nzchar(as.character(validity_number))) {
      sprintf(tr("Guide for Table %s: HTMT detailed criteria", "표 %s 가이드: HTMT 세부 판단"), validity_number)
    } else tr("Validity-table guide: HTMT detailed criteria", "타당도 표 가이드: HTMT 세부 판단")
    localize_details <- function(table) {
      for (column in intersect(c("Factor 1", "Factor 2"), names(table))) {
        mapped <- unname(factor_display_map[as.character(table[[column]])])
        keep <- !is.na(mapped) & nzchar(mapped)
        table[[column]][keep] <- mapped[keep]
      }
      labels <- c("Below reference"="참고값 미만", "Review needed"="검토 필요", "Not assessed"="평가하지 않음",
        "At least two indicators per factor are required"="요인마다 지표가 최소 2개 필요합니다",
        "Cross-loaded indicators prevent standard HTMT calculation"="교차적재 지표로 인해 표준 HTMT를 계산할 수 없습니다",
        "Indicator correlations are unavailable"="지표 상관을 사용할 수 없습니다",
        "Within-factor correlations are insufficient"="요인 내 상관이 충분하지 않습니다",
        "Adequate"="충분", "Caution"="주의", "Unreliable"="신뢰 불가", "Yes"="예", "No"="아니요",
        "BCa unavailable"="BCa 사용 불가", "Percentile"="백분위수", "Bias-corrected (BC)"="편향보정 (BC)")
      for (column in intersect(c("Criterion", "Reason", "Status", "CI method", "Upper < threshold", "Upper < 1"), names(table))) {
        table[[column]] <- vapply(as.character(table[[column]]), function(x) if(x %in% names(labels)) tr(x, unname(labels[[x]])) else x, character(1), USE.NAMES=FALSE)
      }
      names(table)[names(table) == "Criterion"] <- tr("Criterion", "기준")
      names(table)[names(table) == "CI method"] <- tr("CI method", "CI 산출법")
      attr(table, "result_user_columns") <- seq_len(ncol(table))
      table
    }
    if (!is.null(bootstrap_table)) bootstrap_table <- localize_details(bootstrap_table)

    tagList(
      tags$h5(validity_guide_title),
      if (length(pair_columns)) structural_canvas_basic_html_table(localize_details(pair_table[, pair_columns, drop = FALSE]), language = language, class = "table table-striped table-bordered structural-htmt-criterion"),
      if (!is.null(bootstrap_table)) tagList(
        tags$h5(sprintf(tr("HTMT %s bootstrap confidence intervals (%s resamples; seed = %s)", "HTMT %s 부트스트랩 신뢰구간 (%s회 재표집; seed = %s)"), htmt_ci_label, bootstrap_reps, bootstrap_seed)),
        structural_canvas_basic_html_table(bootstrap_table, language = language, class = "table table-striped table-bordered structural-htmt-bootstrap")
      ),
      result_note_paragraph(class = "structural-result-note", tr("HTMT uses absolute item correlations. Values below the selected threshold are marked 'Below reference', which is not a standalone discriminant-validity pass.", "HTMT는 문항 상관의 절댓값을 사용합니다. 선택 기준보다 낮은 값은 'Below reference'로 표시되며 판별타당도 합격을 뜻하지 않습니다.")),
      if (length(bundle$ordered %||% character(0))) result_note_paragraph(class = "structural-result-note", tr("For ordered indicators, HTMT uses lavaan's polychoric latent-response correlations.", "순서형 지표에서는 HTMT가 lavaan의 polychoric latent-response 상관을 사용합니다.")),
      if (bootstrap_reps <= 0L) result_note_paragraph(class = "structural-result-note", tr("HTMT point estimates are descriptive. Select HTMT bootstrap CI in the analysis options when interval estimates are required.", "HTMT 점추정값은 기술적 지표입니다. 구간추정이 필요하면 분석 옵션에서 HTMT 부트스트랩 CI를 선택하십시오.")),
      if (bootstrap_reps > 0L && is.null(bootstrap_table) && isTRUE(bundle$cfa_bootstrap_pending)) result_note_paragraph(class = "structural-result-note", statedu_localized_text(statedu_current_language(app_language_fn), "HTMT bootstrap intervals are being computed in the background. This result table will update automatically when complete.", "HTMT 부트스트랩을 백그라운드에서 계산하고 있습니다. 완료되면 이 결과표가 자동으로 갱신됩니다.")),
      if (bootstrap_reps > 0L && is.null(bootstrap_table) && bootstrap_canceled) result_note_paragraph(class = "structural-result-note", statedu_localized_text(statedu_current_language(app_language_fn), "The HTMT bootstrap was stopped by the user. Point estimates and base-model results remain available.", "HTMT 부트스트랩이 사용자 요청으로 중단되었습니다. 점추정값과 기본 분석 결과는 유지됩니다.")),
      if (bootstrap_reps > 0L && is.null(bootstrap_table) && !isTRUE(bundle$cfa_bootstrap_pending) && !bootstrap_canceled) result_note_paragraph(class = "structural-result-note", statedu_localized_text(statedu_current_language(app_language_fn), "HTMT bootstrap confidence intervals could not be estimated from the selected indicators.", "선택된 지표로부터 HTMT 부트스트랩 신뢰구간을 계산하지 못했습니다.")),
      if (bootstrap_incomplete) result_note_paragraph(class = "structural-result-note", statedu_localized_text(statedu_current_language(app_language_fn), "Some bootstrap resamples could not produce an admissible correlation matrix, commonly because an ordered category was absent or sparse. Interpret intervals with reduced valid-replicate counts cautiously.", "일부 부트스트랩 표본은 허용 가능한 상관행렬을 만들지 못했습니다. 순서형 범주가 없거나 희소한 경우가 흔한 원인입니다. 유효 반복 수가 줄어든 구간은 신중하게 해석하십시오.")),
      if (bootstrap_bca_unavailable) result_note_paragraph(class = "structural-result-note", statedu_localized_text(statedu_current_language(app_language_fn), "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that pair; increase valid replicates or use percentile CI for reporting.", "BCa unavailable은 해당 쌍의 편향보정 또는 잭나이프 가속도를 계산하지 못했다는 뜻입니다. 유효 반복 수를 늘리거나 보고에는 percentile CI 사용을 검토하십시오.")),
      if (bootstrap_caution) result_note_paragraph(class = "structural-result-note", statedu_localized_text(statedu_current_language(app_language_fn), "HTMT bootstrap status Caution means that 50% to less than 80% of requested resamples were valid. Treat the interval as unstable and report the valid-replicate count.", "HTMT 부트스트랩 상태 Caution은 요청 표본의 50% 이상 80% 미만만 유효했다는 뜻입니다. 구간은 불안정할 수 있으므로 유효 반복 수를 함께 보고하십시오.")),
      if (bootstrap_unreliable) result_note_paragraph(class = "structural-result-note", statedu_localized_text(statedu_current_language(app_language_fn), "HTMT bootstrap status Unreliable means that fewer than 50% of requested resamples were valid. Confidence limits and threshold decisions are not assessed and should not be used as discriminant-validity evidence.", "HTMT 부트스트랩 상태 Unreliable은 요청 표본의 50% 미만만 유효했다는 뜻입니다. 신뢰한계와 기준 판단은 평가하지 않으며, 변별타당도 근거로 사용하면 안 됩니다.")),
      if (!is.null(bootstrap_table)) tagList(
        result_note_paragraph(class = "structural-result-note", sprintf(tr("The %s interval is based on case resampling.", "%s 구간은 사례 재표집에 기반합니다."), htmt_ci_label)),
        if (identical(htmt_ci_method, "bca")) result_note_paragraph(class = "structural-result-note", tr("BCa uses leave-one-out jackknife acceleration.", "BCa는 leave-one-out 잭나이프 가속도를 사용합니다.")),
        if (length(bundle$ordered %||% character(0))) result_note_paragraph(class = "structural-result-note", tr("Polychoric correlations are re-estimated in each resample.", "각 재표본에서 polychoric 상관을 재추정합니다.")),
        result_note_paragraph(class = "structural-result-note", tr("'Upper < threshold' uses the one-sided 95% upper confidence limit for the selected .85/.90 criterion. 'Upper < 1' indicates whether the two-sided 95% interval excludes 1. These are reported separately from the point-estimate criterion.", "'상한 < 기준'은 선택한 .85/.90 기준에 대한 단측 95% 상한을 사용합니다. '상한 < 1'은 양측 95% 구간이 1을 제외하는지 나타냅니다. 이 판단은 점추정 기준과 분리해 보고합니다."))
      )
    )
  }
  output[[paste0(prefix, "_result_htmt")]] <- renderUI({
    tagList(structural_canvas_higher_validity_html(fit_result()),
      render_htmt_tables(include_details = FALSE),
      structural_canvas_higher_htmt_html(fit_result(), FALSE, statedu_current_language(app_language_fn), raw = TRUE),
      structural_canvas_higher_htmt_html(fit_result(), FALSE, statedu_current_language(app_language_fn)))
  })
  output[[paste0(prefix, "_result_htmt_details")]] <- renderUI({
    tagList(render_htmt_tables(include_details = TRUE),
      structural_canvas_higher_htmt_html(fit_result(), TRUE, statedu_current_language(app_language_fn), raw = TRUE),
      structural_canvas_higher_htmt_html(fit_result(), TRUE, statedu_current_language(app_language_fn)))
  })
  invisible(TRUE)
}
