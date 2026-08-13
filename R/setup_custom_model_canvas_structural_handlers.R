register_structural_equation_canvas_handlers <- function(input, output, session, dataset_fn, selected_names_fn, variable_table_fn, labels_fn, category_table_fn, mark_settings_dirty, app_language_fn = NULL) {
  lapply(c("cfa", "cbsem", "plssem"), function(analysis_type) {
    prefix <- structural_analysis_prefix(analysis_type)
    canvas_input <- paste0(prefix, "_canvas_state")
    canvas_output <- paste0(prefix, "_canvas_setup")
    run_input <- paste0(prefix, "_canvas_run_request")
    confirm_input <- paste0(prefix, "_canvas_run_confirm")
    advanced_input <- paste0(prefix, "_canvas_advanced_request")
    fit_result <- reactiveVal(NULL)
    pending_mi_rows <- reactiveVal(integer(0))
    pending_estimator_snapshot <- reactiveVal(NULL)
    if (identical(analysis_type, "cfa")) output[[paste0(prefix, "_download_reproducibility")]] <- downloadHandler(
      filename = function() paste0("cfa-analysis-record-", format(Sys.Date(), "%Y%m%d"), ".txt"),
      contentType = "text/plain; charset=utf-8",
      content = function(file) {
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        writeLines(structural_canvas_reproducibility_record(bundle), file, useBytes = TRUE)
      }
    )
    if (identical(analysis_type, "cfa")) output[[paste0(prefix, "_download_tables")]] <- downloadHandler(
      filename = function() paste0("cfa-result-tables-", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        shiny::req(requireNamespace("openxlsx", quietly = TRUE))
        bundle <- fit_result()
        shiny::req(!is.null(bundle))
        sheets <- structural_canvas_result_workbook_sheets(bundle, result_table)
        structural_canvas_write_result_workbook(sheets, file)
      }
    )
    if (identical(analysis_type, "cfa")) observe({
      data <- dataset_fn()
      choices <- names(data %||% data.frame())
      current <- as.character(input[[paste0(prefix, "_invariance_group")]] %||% "")
      updateSelectInput(session, paste0(prefix, "_invariance_group"), choices = choices, selected = if (current %in% choices) current else "")
    })
    result_table <- function(kind) {
      structural_canvas_result_table(kind, fit_result, analysis_type, labels_fn, app_language_fn)
    }
    output[[canvas_output]] <- renderUI({
      structural_equation_workspace(selected_names_fn(), variable_table_fn(), labels_fn(), analysis_type, statedu_current_language(app_language_fn))
    })
    output[[paste0(prefix, "_results")]] <- renderUI({
      shiny::req(!is.null(fit_result()))
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      div(
        class = "structural-analysis-results regression-results",
        h3(if (ko) "분석 결과" else "Analysis Results"),
        if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_reproducibility"), if (ko) "분석 기록 다운로드" else "Download analysis record", class = "btn btn-default btn-sm"),
        if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_tables"), if (ko) "결과표 Excel 다운로드" else "Download result tables", class = "btn btn-default btn-sm"),
        div(class = "result-section regression-result-panel", h4(if (ko) "1. 모형 개요" else "1. Model overview"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview")))),
        uiOutput(paste0(prefix, "_result_identification")),
        uiOutput(paste0(prefix, "_result_normality")),
        uiOutput(paste0(prefix, "_result_missing_outliers")),
        uiOutput(paste0(prefix, "_result_risk_diagnostics")),
        uiOutput(paste0(prefix, "_result_heywood")),
        div(class = "result-section regression-result-panel", h4(if (ko) "2. 모형 적합도" else "2. Model fit"), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit"))), uiOutput(paste0(prefix, "_result_fit_guidance")), uiOutput(paste0(prefix, "_result_rmsea_tests")), uiOutput(paste0(prefix, "_result_information_criteria")), uiOutput(paste0(prefix, "_result_bollen_stine")), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit_difference")))),
        uiOutput(paste0(prefix, "_result_invariance")),
        div(class = "result-section regression-result-panel", h4(if (ko) "3. 잠재 구성개념 상관, 신뢰도 및 수렴·판별타당도" else "3. Latent construct correlations, reliability, and convergent/discriminant validity"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_validity"))), uiOutput(paste0(prefix, "_result_latent_correlation_ci")), uiOutput(paste0(prefix, "_result_validity_note")), uiOutput(paste0(prefix, "_result_reliability_bootstrap")), uiOutput(paste0(prefix, "_result_factor_scores")), uiOutput(paste0(prefix, "_result_htmt"))),
        div(
          class = "result-section regression-result-panel structural-measurement-result",
          h4(if (ko) "4. 측정모형" else "4. Measurement model"),
          div(class = "table-responsive", tableOutput(paste0(prefix, "_result_measurement"))),
          tags$p(class = "structural-result-note", "* Fixed reference loading; its unstandardized SE, z, and p are not estimated. The standardized loading remains a derived estimate and therefore has a confidence interval."),
          tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized loadings. A fixed reference loading has a degenerate B interval at its fixed value."),
          tags$p(class = "structural-result-note", "Standardized-loading confidence intervals are 95% delta-method intervals from lavaan; robust fitted models use the fitted model's robust covariance information."),
          tags$p(class = "structural-result-note", "R² confidence intervals are obtained by complementing the standardized residual-variance interval (lower = 1 − residual upper; upper = 1 − residual lower)."),
          tags$p(class = "structural-result-note", "Std. residual variance is the standardized diagonal residual variance for each indicator. † marks an unavailable residual value, a residual outside [0, 1], or an R² interval extending beyond [0, 1], including a negative-residual Heywood case."),
          tags$p(class = "structural-result-note", "Guidance prioritizes inadmissible residual variance, then cross-loading, a standardized-loading CI containing 0, and |β| < .40. The .40 value is a descriptive review guideline rather than a universal item-retention rule."),
          tags$p(class = "structural-result-note", "Cross-loaded indicators require theory-based interpretation; simple-structure reliability and discriminant-validity summaries, especially HTMT, may be unavailable or require caution.")
        ),
        uiOutput(paste0(prefix, "_result_higher_order")),
        uiOutput(paste0(prefix, "_result_residuals")),
        uiOutput(paste0(prefix, "_result_mi_holdout")),
        uiOutput(paste0(prefix, "_result_mi_history")),
        if (analysis_type != "plssem") div(class = "result-section regression-result-panel structural-mi-result", h4(if (ko) "6. 수정지수(MI)" else "6. Modification indices (MI)"), uiOutput(paste0(prefix, "_result_mi")))
      )
    })
    output[[paste0(prefix, "_result_fit")]] <- renderUI({
      values <- result_table("fit")
      shiny::req(nrow(values) > 0)
      ci_percent <- round(100 * as.numeric(fit_result()$rmsea_ci %||% .90))
      comparison_fits <- if (isTRUE(fit_result()$modified_from_baseline) && !is.null(fit_result()$baseline_fit)) list(fit_result()$baseline_fit, fit_result()$fit) else list(fit_result()$fit)
      fit_selections <- structural_canvas_common_fit_measures(comparison_fits, fit_result()$estimator %||% "ML", fit_result()$rmsea_ci %||% .90)
      selection <- fit_selections[[length(fit_selections)]]
      fit_labels <- selection$labels
      baseline_selection <- if (length(fit_selections) > 1L) fit_selections[[1L]] else NULL
      same_measure_keys <- is.null(baseline_selection) || identical(selection$keys, baseline_selection$keys)
      if (!same_measure_keys) fit_labels[c(5L, 6L, 8L)] <- c("Adjusted CFI", "Adjusted TLI", "Adjusted RMSEA")
      tagList(tags$table(
        class = "table table-striped table-bordered structural-fit-table",
        tags$thead(
          tags$tr(
            tags$th(rowspan = "2", "Model"),
            tags$th(rowspan = "2", if (!same_measure_keys) HTML("Adjusted &chi;<sup>2</sup>*") else if (grepl("Scaled", fit_labels[[1L]], fixed = TRUE)) HTML("Scaled &chi;<sup>2</sup>*") else HTML("&chi;<sup>2</sup>")), tags$th(rowspan = "2", "df"), tags$th(rowspan = "2", "p"), tags$th(rowspan = "2", "Q"),
            tags$th(rowspan = "2", paste0(fit_labels[[5L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", paste0(fit_labels[[6L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", "SRMR"), tags$th(rowspan = "2", paste0(fit_labels[[8L]], if (selection$adjusted) "*" else "")),
            tags$th(colspan = "2", paste0(ci_percent, "% CI"))
          ),
          tags$tr(tags$th("LLCI"), tags$th("ULCI"))
        ),
        tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, , drop = TRUE]), tags$td))))
      ), if (selection$adjusted)
        tags$p(class = "structural-result-note", if (is.null(baseline_selection) || same_measure_keys) {
          paste0("* Reported lavaan measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
        } else {
          paste0("* Original-model measures: ", paste(unname(baseline_selection$keys), collapse = ", "), "; modified-model measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
        }),
        if ((!is.null(baseline_selection) && baseline_selection$values[[2L]] == 0) || selection$values[[2L]] == 0)
          tags$p(class = "structural-result-note", "Q (chi-square/df) and some fit indices are not interpretable for a saturated model with df = 0.")
      )
    })
    output[[paste0(prefix, "_result_identification")]] <- renderUI({
      bundle <- fit_result()
      issues <- bundle$identification %||% data.frame()
      if (!nrow(issues)) return(tags$p(class = "structural-result-note", "Pre-fit structural identification check: no rule-based issues detected."))
      tags$div(class = "structural-identification-result",
        tags$h5("Pre-fit identification diagnostics"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(issues), tags$th))),
          tags$tbody(lapply(seq_len(nrow(issues)), function(index) tags$tr(lapply(as.character(issues[index, ]), tags$td))))
        ),
        tags$p(class = "structural-result-note", "This rule-based screen does not prove mathematical identification; lavaan estimation, degrees of freedom, information-matrix checks, and solution admissibility remain decisive.")
      )
    })
    output[[paste0(prefix, "_result_normality")]] <- renderUI({
      bundle <- fit_result()
      if (analysis_type == "plssem" || length(bundle$ordered %||% character(0))) return(NULL)
      indicators <- lavaan::lavNames(bundle$fit, "ov")
      diagnosis <- structural_canvas_mardia(dataset_fn(), indicators)
      if (!isTRUE(diagnosis$available)) {
        return(div(class = "result-section regression-result-panel structural-normality-result",
          h4("Multivariate normality"), tags$p(class = "structural-result-note", diagnosis$reason)))
      }
      table <- data.frame(
        Test = c("Mardia skewness", "Mardia kurtosis"),
        Estimate = c(format_decimal3(diagnosis$skewness), format_decimal3(diagnosis$kurtosis)),
        Statistic = c(format_decimal3(diagnosis$skew_statistic), format_decimal3(diagnosis$kurtosis_z)),
        df = c(format_decimal3(diagnosis$skew_df), "—"),
        p = c(format_p(diagnosis$skew_p), format_p(diagnosis$kurtosis_p)),
        check.names = FALSE
      )
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      recommendation_label <- if (ko && identical(diagnosis$recommendation, "MLR recommended")) {
        "MLR 권장"
      } else if (ko && identical(diagnosis$recommendation, "ML is acceptable")) {
        "ML 사용 가능"
      } else {
        diagnosis$recommendation
      }
      div(class = "result-section regression-result-panel structural-normality-result",
        h4(if (ko) "다변량 정규성 및 추정량 안내" else "Multivariate normality and estimator guidance"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
        ),
        tags$p(class = "structural-result-note", if (ko) paste0("안내: ", recommendation_label, ". 이 권고는 진단용이며 추정량을 자동으로 변경하지 않습니다.") else paste0("Guidance: ", recommendation_label, ". This recommendation is diagnostic and does not automatically change the estimator.")),
        tags$p(class = "structural-result-note", if (ko) paste0("사용된 완전 사례: ", diagnosis$n, " / ", diagnosis$original_n, "; 지표 수: ", diagnosis$p, ".") else paste0("Complete cases used: ", diagnosis$n, " of ", diagnosis$original_n, "; indicators: ", diagnosis$p, ".")),
        if (isTRUE(diagnosis$sampled)) tags$p(class = "structural-result-note", if (ko) "계산 안정성을 위해 Mardia 통계량은 완전 사례 중 균등 간격 결정 표본 2,000건으로 계산했습니다." else "For computational stability, Mardia statistics used an evenly spaced deterministic subsample of 2,000 complete cases."),
        tags$p(class = "structural-result-note", if (ko) "Mardia 검정은 표본 수에 민감합니다. p 값과 함께 분포 형태, 이상치, 측정 가정을 함께 검토하십시오." else "Mardia tests are sample-size sensitive. Consider distribution shape, outliers, and substantive measurement assumptions alongside p-values.")
      )
    })
    output[[paste0(prefix, "_result_risk_diagnostics")]] <- renderUI({
      bundle <- fit_result()
      if (analysis_type == "plssem") return(NULL)
      factor_correlations <- structural_canvas_factor_correlation_diagnostics(bundle$fit)
      flagged_correlations <- factor_correlations[factor_correlations$Severity != "Acceptable", , drop = FALSE]
      if (nrow(flagged_correlations)) {
        flagged_correlations$Correlation <- vapply(flagged_correlations$Correlation, format_decimal3, character(1))
        flagged_correlations[["Absolute correlation"]] <- vapply(flagged_correlations[["Absolute correlation"]], format_decimal3, character(1))
      }
      categories <- structural_canvas_ordered_category_diagnostics(dataset_fn(), bundle$ordered %||% character(0))
      flagged_categories <- if (nrow(categories)) categories[categories$Status != "Adequate", , drop = FALSE] else categories
      if (nrow(flagged_categories)) flagged_categories$Percent <- paste0(vapply(flagged_categories$Percent, format_decimal3, character(1)), "%")
      ordered_pairs <- structural_canvas_ordered_pair_diagnostics(dataset_fn(), bundle$ordered %||% character(0))
      flagged_pairs <- if (nrow(ordered_pairs)) ordered_pairs[ordered_pairs$Status != "Adequate", , drop = FALSE] else ordered_pairs
      if (nrow(flagged_pairs)) flagged_pairs[["Empty %"]] <- paste0(vapply(flagged_pairs[["Empty %"]], format_decimal3, character(1)), "%")
      error_covariances <- structural_canvas_error_covariance_diagnostics(bundle$snapshot %||% list())
      if (!nrow(flagged_correlations) && !nrow(flagged_categories) && !nrow(flagged_pairs) && error_covariances$count == 0L) return(NULL)
      render_data_table <- function(table) tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(lapply(names(table), tags$th))),
        tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
      )
      div(class = "result-section regression-result-panel structural-risk-result",
        h4("Data and model risk diagnostics"),
        if (nrow(flagged_correlations)) tagList(tags$h5("High latent correlations"), render_data_table(flagged_correlations)),
        if (nrow(flagged_categories)) tagList(tags$h5("Sparse ordered categories"), render_data_table(flagged_categories)),
        if (nrow(flagged_pairs)) tagList(tags$h5("Sparse ordered-indicator cross-tabulations"), render_data_table(flagged_pairs)),
        if (error_covariances$count > 0L) tagList(
          tags$h5("Correlated measurement errors"),
          tags$p(paste0(error_covariances$count, " of ", error_covariances$possible, " possible indicator pairs (", format_decimal3(100 * error_covariances$ratio), "%): ", error_covariances$status, "."))
        ),
        tags$p(class = "structural-result-note", "Latent correlations of .85 or greater warrant discriminant-validity review; .90 or greater are high, and .95 or greater indicate severe construct overlap."),
        if (nrow(flagged_categories)) tags$p(class = "structural-result-note", "A category is flagged when empty, when its count is no greater than max(5, 1% of valid responses), or when it contains at least 95% of valid responses. Sparse or extremely dominant categories can destabilize thresholds and polychoric correlations."),
        if (nrow(flagged_pairs)) tags$p(class = "structural-result-note", "Each ordered-indicator pair is cross-tabulated over all observed or declared categories. Empty cells or nonempty cells with counts no greater than max(5, 1% of pairwise-valid responses) are flagged because they can destabilize polychoric correlations and the WLSMV weight matrix. Category collapsing requires substantive justification and must preserve order."),
        if (error_covariances$count > 0L) tags$p(class = "structural-result-note", "Several correlated errors can indicate item redundancy or data-driven overfitting. Each covariance requires substantive justification.")
      )
    })
    output[[paste0(prefix, "_result_missing_outliers")]] <- renderUI({
      bundle <- fit_result()
      if (analysis_type == "plssem") return(NULL)
      indicators <- lavaan::lavNames(bundle$fit, "ov")
      missing <- structural_canvas_missing_diagnostics(dataset_fn(), indicators)
      outliers <- if (!length(bundle$ordered %||% character(0))) structural_canvas_mahalanobis_diagnostics(dataset_fn(), indicators) else list(available = FALSE, reason = "Mahalanobis diagnostics are not reported for ordered indicators.")
      if (isTRUE(missing$available)) {
        missing_variables <- missing$variables[missing$variables$Missing > 0L, , drop = FALSE]
        if (nrow(missing_variables)) missing_variables$Percent <- paste0(vapply(missing_variables$Percent, format_decimal3, character(1)), "%")
      } else missing_variables <- data.frame()
      render_table <- function(table) tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(lapply(names(table), tags$th))),
        tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
      )
      div(class = "result-section regression-result-panel structural-missing-outlier-result",
        h4("Missing data and multivariate outliers"),
        if (nrow(missing_variables)) tagList(tags$h5("Variable-level missingness"), render_table(missing_variables)) else tags$p("No missing indicator values were detected."),
        if (isTRUE(missing$available)) tags$p(paste0("Complete cases: ", missing$complete_n, " of ", missing$n, "; incomplete cases: ", missing$incomplete_n, "; distinct missingness patterns: ", missing$pattern_count, ".")),
        if (isTRUE(missing$available) && missing$pattern_count > 1L) tagList(tags$h5("Missingness patterns"), render_table(utils::head(missing$patterns, 20L))),
        tags$p(class = "structural-result-note", if (length(bundle$ordered %||% character(0))) {
          paste0("Ordered-indicator estimation uses ", bundle$missing %||% "pairwise", " missing-data handling. Review sparse pairwise coverage alongside category frequencies.")
        } else if (identical(bundle$missing %||% "", "fiml")) {
          "FIML uses all available observations under the missing-at-random assumption; the complete-case count above applies only to diagnostics such as Mardia and Mahalanobis distance."
        } else {
          paste0("The fitted model used missing-data option: ", bundle$missing %||% "unspecified", ".")
        }),
        tags$h5("Mahalanobis outlier candidates"),
        if (!isTRUE(outliers$available)) tags$p(class = "structural-result-note", outliers$reason) else if (!nrow(outliers$table)) tags$p(paste0("No complete cases were flagged at p < ", outliers$alpha, ".")) else {
          outlier_table <- outliers$table
          outlier_table$Mahalanobis <- vapply(outlier_table$Mahalanobis, format_decimal3, character(1))
          outlier_table$p <- vapply(outlier_table$p, format_p, character(1))
          tagList(render_table(outlier_table), tags$p(paste0(outliers$flagged_n, " of ", outliers$n, " complete cases flagged at p < ", outliers$alpha, ".")))
        },
        tags$p(class = "structural-result-note", "Mahalanobis candidates should be investigated for data errors, unusual but valid cases, and influence. They are not removed automatically; use robust estimation or a documented sensitivity analysis when appropriate.")
      )
    })
    output[[paste0(prefix, "_result_fit_difference")]] <- renderUI({
      bundle <- fit_result()
      if (!identical(bundle$comparison_type %||% "", "mi") || is.null(bundle$baseline_fit)) return(NULL)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      report <- structural_canvas_model_difference_report(bundle)
      if (!nrow(report) || !isTRUE(report$Available[[1L]])) {
        reason <- if (nrow(report)) as.character(report$Reason[[1L]]) else "eligibility was not established"
        return(tags$p(class = "structural-result-note", if (ko) paste0("공식 모형 차이 검정을 표시하지 않았습니다: ", reason) else paste0("A formal model-difference test was suppressed: ", reason)))
      }
      tags$div(
        class = "structural-fit-difference",
        tags$h5(if (ko) "기존 모형 vs 탐색적 수정 모형" else "Original vs exploratory modified model"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(tags$th("Δχ²"), tags$th("Δdf"), tags$th("p"))),
          tags$tbody(tags$tr(
            tags$td(format_decimal3(report$`Delta chi-square`[[1L]])),
            tags$td(format_decimal3(report$`Delta df`[[1L]])),
            tags$td(format_p(report$p[[1L]]))
          ))
        ),
        tags$p(class = "structural-result-note", report$Method[[1L]]),
        tags$p(class = "structural-result-note", if (ko) "이 수정은 동일 자료의 MI를 보고 선택했으므로, 이 차이 검정은 탐색적 결과이며 확인적 근거로 해석하면 안 됩니다." else "Because the modification was selected using MI from the same data, this difference test is exploratory and should not be treated as confirmatory evidence.")
      )
    })
    output[[paste0(prefix, "_result_invariance")]] <- renderUI({
      bundle <- fit_result()
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
    })
    output[[paste0(prefix, "_result_fit_guidance")]] <- renderUI({
      bundle <- fit_result()
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      comparison_fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, bundle$fit) else list(bundle$fit)
      selections <- structural_canvas_common_fit_measures(comparison_fits, bundle$estimator %||% "ML", bundle$rmsea_ci %||% .90)
      labels <- if (length(selections) > 1L) c(if (ko) "기존 모형" else "Original model", if (ko) "탐색적 수정 모형" else bundle$comparison_label %||% "Modified model") else if (ko) "기존 모형" else "Original model"
      tables <- lapply(seq_along(selections), function(index) {
        guidance <- structural_canvas_fit_guidance(selections[[index]]$values)
        guidance$Model <- labels[[index]]
        guidance[, c("Model", "Metric", "Value", "Guidance", "Reference"), drop = FALSE]
      })
      table <- do.call(rbind, tables)
      table$Value <- vapply(table$Value, format_decimal3, character(1))
      severity <- c("Good" = 1L, "Marginal" = 2L, "Review" = 3L, "Not assessed" = 4L)
      summaries <- vapply(split(table$Guidance, table$Model), function(values) {
        if (all(values == "Not assessed")) return("Not assessed")
        score <- max(vapply(values[values != "Not assessed"], function(value) severity[[value]], integer(1)))
        names(severity)[match(score, severity)]
      }, character(1))
      div(class = "structural-fit-guidance-result",
        tags$h5(if (ko) "기준 기반 적합도 안내" else "Reference-based fit guidance"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
        ),
        tags$p(paste(vapply(names(summaries), function(name) paste0(name, ": ", summaries[[name]]), character(1)), collapse = " | ")),
        tags$p(class = "structural-result-note", if (ko) "Good/Marginal/Review 표시는 흔히 쓰는 근사 기준에 따른 설명용 안내입니다. 보편적 수용 규칙이 아니며 모형 식별, 잔차 진단, 모수 타당성, 이론, 표본 특성, 대안 모형 비교를 대체하지 않습니다." else "Good/Marginal/Review labels are descriptive reference guidance based on commonly used approximate cutoffs. They are not universal acceptance rules and do not replace model identification, residual diagnostics, parameter plausibility, theory, sample characteristics, or comparison with plausible alternatives."),
        tags$p(class = "structural-result-note", if (ko) "증분 적합도 안내: CFI/TLI >= .95 Good, >= .90 Marginal. 절대 적합도 안내: RMSEA <= .06 Good, <= .08 Marginal; SRMR <= .08 Good, <= .10 Marginal. 이 범위 밖 값은 Review로 표시합니다." else "Incremental-fit guidance: CFI/TLI >= .95 Good, >= .90 Marginal. Absolute-fit guidance: RMSEA <= .06 Good, <= .08 Marginal; SRMR <= .08 Good, <= .10 Marginal. Values outside these ranges are marked Review."),
        if (any(table$Guidance == "Not assessed")) tags$p(class = "structural-result-note", if (ko) "포화모형(df = 0)이거나 적합도 지수가 없으면 적합도 안내를 평가하지 않습니다." else "Fit guidance is not assessed for saturated models (df = 0) or unavailable fit indices.")
      )
    })
    output[[paste0(prefix, "_result_rmsea_tests")]] <- renderUI({
      bundle <- fit_result()
      table <- structural_canvas_rmsea_hypothesis_tests(bundle)
      display <- table
      for (column in c("RMSEA", "Close-fit H0", "Not-close H0")) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
      for (column in c("Close-fit p", "Not-close p")) display[[column]] <- vapply(display[[column]], format_p, character(1))
      tagList(
        tags$h5("RMSEA hypothesis tests"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Close-fit tests H0: population RMSEA <= .05; a small p value rejects close fit. Not-close tests H0: population RMSEA >= .08; a small p value rejects poor approximate fit. Interpret both with the RMSEA estimate and confidence interval."),
        tags$p(class = "structural-result-note", "Robust or scaled p values are selected to match the reported RMSEA when available. These hypothesis tests are sample-size sensitive and are not standalone model-acceptance rules.")
      )
    })
    output[[paste0(prefix, "_result_information_criteria")]] <- renderUI({
      bundle <- fit_result()
      table <- structural_canvas_information_criteria(bundle)
      if (!any(is.finite(table$AIC)) && !any(is.finite(table$BIC))) return(NULL)
      display <- table
      numeric_columns <- names(display)[vapply(display, is.numeric, logical(1))]
      for (column in numeric_columns) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
      tagList(
        tags$h5("Likelihood-based information criteria"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Lower AIC, BIC, and adjusted BIC indicate better relative expected fit after penalizing complexity; delta values are relative to the smallest criterion in this displayed set."),
        if (any(grepl("^Not comparable", table$`Comparison status`))) tags$p(class = "structural-result-note", "Delta values were suppressed because analyzed observations, observed variables, estimator family, or model admissibility differed across models."),
        tags$p(class = "structural-result-note", "Compare information criteria only for models fitted to the same observations and observed variables with the same likelihood and estimator family. They are not absolute goodness-of-fit tests and are not reported for WLSMV models without a comparable likelihood.")
      )
    })
    output[[paste0(prefix, "_result_bollen_stine")]] <- renderUI({
      bundle <- fit_result()
      result <- bundle$bollen_stine_result %||% NULL
      if (is.null(result) || !nrow(result)) return(NULL)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      display <- result
      display[["Observed chi-square"]] <- vapply(display[["Observed chi-square"]], format_decimal3, character(1))
      display[["Bootstrap p"]] <- vapply(display[["Bootstrap p"]], format_p, character(1))
      for (column in c("Monte Carlo SE", "Monte Carlo 95% lower", "Monte Carlo 95% upper")) {
        display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
      }
      display[["Valid %"]] <- paste0(vapply(display[["Valid %"]], format_decimal3, character(1)), "%")
      tagList(
        tags$h5("Bollen-Stine bootstrap global-fit test"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "The bootstrap p value uses the plus-one correction: (1 + bootstrap chi-square values at least as large as observed) / (1 + valid replicates). Valid replicates pass the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA. A small p value indicates global model misfit under the exact-fit null."),
        tags$p(class = "structural-result-note", "Monte Carlo SE and the 95% Wilson interval quantify simulation error from the finite number of valid bootstrap replicates; they are not a confidence interval for a population model parameter."),
        if (any(result$Status != "Adequate")) tags$p(class = "structural-result-note", "Fewer than 80% of requested resamples produced a converged admissible statistic. Treat the bootstrap p value and Monte Carlo interval as unstable; resolve convergence or admissibility problems before reporting."),
        if (isTRUE(bundle$modified_from_baseline)) tags$p(class = "structural-result-note", if (ko) "이 모형은 분석 자료를 보고 수정한 탐색적 수정 모형입니다. Bollen-Stine 결과는 탐색적 참고값이며 자료 기반 수정에 대한 확인적 근거를 제공하지 않습니다." else "This model was modified using the analyzed data. Its Bollen-Stine result is exploratory and does not provide confirmatory evidence for the data-driven modification."),
        tags$p(class = "structural-result-note", "This transformed-data test is available only for complete continuous single-group ML CFA and does not replace approximate fit indices, residual diagnostics, or substantive model evaluation.")
      )
    })
    output[[paste0(prefix, "_result_heywood")]] <- renderUI({
      bundle <- fit_result()
      diagnostics <- bundle$baseline_diagnostics %||% bundle$diagnostics %||% list()
      variables <- as.character(diagnostics$negative_residuals %||% character(0))
      latent_variables <- as.character(diagnostics$negative_latent_variances %||% character(0))
      theta_matrix_issue <- isTRUE(diagnostics$non_psd_theta) || isTRUE(diagnostics$near_singular_theta) || isTRUE(diagnostics$ill_conditioned_theta)
      latent_matrix_issue <- isTRUE(diagnostics$non_psd_latent_covariance) || isTRUE(diagnostics$near_singular_latent_covariance) || isTRUE(diagnostics$ill_conditioned_latent_covariance)
      parameter_matrix_issue <- isTRUE(diagnostics$non_psd_parameter_covariance) || isTRUE(diagnostics$near_singular_parameter_covariance) || isTRUE(diagnostics$ill_conditioned_parameter_covariance)
      matrix_issue <- theta_matrix_issue || latent_matrix_issue || parameter_matrix_issue
      if ((!length(variables) && !length(latent_variables) && !matrix_issue) || analysis_type == "plssem") return(NULL)
      diagnostic_fit <- bundle$baseline_fit %||% bundle$fit
      theta <- as.matrix(lavaan::lavInspect(diagnostic_fit, "theta"))
      standardized <- lavaan::standardizedSolution(diagnostic_fit)
      r2 <- lavaan::lavInspect(diagnostic_fit, "r2")
      loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
      residual_rows <- standardized$op == "~~" & standardized$lhs == standardized$rhs
      standardized_residuals <- stats::setNames(standardized$est.std[residual_rows], standardized$lhs[residual_rows])
      data <- dataset_fn()
      observed_variances <- vapply(variables, function(name) stats::var(data[[name]], na.rm = TRUE), numeric(1))
      fixed_values <- as.numeric((bundle$residual_variance_fixes %||% numeric(0))[variables])
      applied_percent <- 100 * fixed_values / observed_variances
      diagnostic_table <- data.frame(
        Variable = variables,
        Factor = vapply(variables, function(name) paste(unique(loadings$lhs[loadings$rhs == name]), collapse = ", "), character(1)),
        `Residual variance` = vapply(variables, function(name) theta[name, name], numeric(1)),
        `Standardized residual` = as.numeric(standardized_residuals[variables]),
        `R²` = as.numeric(r2[variables]),
        `Observed variance` = observed_variances,
        `Applied %` = applied_percent,
        `Fixed value` = fixed_values,
        Status = "Heywood",
        check.names = FALSE
      )
      latent_covariance <- as.matrix(lavaan::lavInspect(diagnostic_fit, "cov.lv"))
      latent_table <- if (length(latent_variables)) data.frame(
        `Latent factor` = latent_variables,
        Variance = vapply(latent_variables, function(name) latent_covariance[name, name], numeric(1)),
        Status = "Latent Heywood", check.names = FALSE
      ) else data.frame()
      matrix_table <- data.frame(
        Matrix = c(if (theta_matrix_issue) "Residual covariance (theta)", if (latent_matrix_issue) "Latent covariance", if (parameter_matrix_issue) "Parameter-estimate covariance (vcov)"),
        `Minimum eigenvalue` = c(if (theta_matrix_issue) diagnostics$theta_min_eigenvalue, if (latent_matrix_issue) diagnostics$latent_min_eigenvalue, if (parameter_matrix_issue) diagnostics$parameter_min_eigenvalue),
        `Condition number` = c(if (theta_matrix_issue) diagnostics$theta_condition_number, if (latent_matrix_issue) diagnostics$latent_condition_number, if (parameter_matrix_issue) diagnostics$parameter_condition_number),
        Status = c(
          if (theta_matrix_issue) if (isTRUE(diagnostics$non_psd_theta)) "Not positive semidefinite" else if (isTRUE(diagnostics$near_singular_theta)) "Near singular / boundary" else "Ill-conditioned",
          if (latent_matrix_issue) if (isTRUE(diagnostics$non_psd_latent_covariance)) "Not positive semidefinite" else if (isTRUE(diagnostics$near_singular_latent_covariance)) "Near singular / boundary" else "Ill-conditioned",
          if (parameter_matrix_issue) if (isTRUE(diagnostics$non_psd_parameter_covariance)) "Unreliable standard errors" else if (isTRUE(diagnostics$near_singular_parameter_covariance)) "Empirical identification boundary" else "Ill-conditioned standard errors"
        ), check.names = FALSE
      )
      can_refit <- length(variables) > 0L && toupper(as.character(bundle$estimator %||% "ML")) %in% c("ML", "MLR") && !length(bundle$ordered %||% character(0))
      tagList(
        div(class = "result-section regression-result-panel structural-heywood-result",
          h4("Heywood case diagnostics"),
          if (nrow(diagnostic_table)) tags$table(class = "table table-striped table-bordered",
            tags$thead(tags$tr(lapply(names(diagnostic_table), tags$th))),
            tags$tbody(lapply(seq_len(nrow(diagnostic_table)), function(index) tags$tr(lapply(c(
              diagnostic_table$Variable[[index]], diagnostic_table$Factor[[index]],
              format_decimal3(diagnostic_table[["Residual variance"]][[index]]),
              format_decimal3(diagnostic_table[["Standardized residual"]][[index]]),
              format_decimal3(diagnostic_table[["R²"]][[index]]),
              format_decimal3(diagnostic_table[["Observed variance"]][[index]]),
              if (is.finite(diagnostic_table[["Applied %"]][[index]])) paste0(format_decimal3(diagnostic_table[["Applied %"]][[index]]), "%") else "—",
              if (is.finite(diagnostic_table[["Fixed value"]][[index]])) format_decimal3(diagnostic_table[["Fixed value"]][[index]]) else "—", diagnostic_table$Status[[index]]
            ), tags$td))))
          ),
          if (nrow(latent_table)) tagList(
            tags$h5("Negative latent variances"),
            tags$table(class = "table table-striped table-bordered",
              tags$thead(tags$tr(lapply(names(latent_table), tags$th))),
              tags$tbody(lapply(seq_len(nrow(latent_table)), function(index) tags$tr(
                tags$td(latent_table[["Latent factor"]][[index]]),
                tags$td(format_decimal3(latent_table$Variance[[index]])),
                tags$td(latent_table$Status[[index]])
              )))
            ),
            tags$p(class = "structural-result-note", "A negative latent variance is a latent-variable Heywood case. The indicator residual-variance sensitivity button does not correct it; review factor specification, scaling, higher-order structure, correlations, and identification constraints.")
          ),
          if (nrow(matrix_table)) tagList(
            tags$h5("Covariance-matrix definiteness diagnostics"),
            tags$table(class = "table table-striped table-bordered",
              tags$thead(tags$tr(lapply(names(matrix_table), tags$th))),
              tags$tbody(lapply(seq_len(nrow(matrix_table)), function(index) tags$tr(
                tags$td(matrix_table$Matrix[[index]]),
                tags$td(format_decimal3(matrix_table[["Minimum eigenvalue"]][[index]])),
                tags$td(if (is.finite(matrix_table[["Condition number"]][[index]])) format(matrix_table[["Condition number"]][[index]], scientific = TRUE, digits = 3) else "Inf"),
                tags$td(matrix_table$Status[[index]])
              )))
            ),
            tags$p(class = "structural-result-note", "A negative minimum eigenvalue means the covariance matrix is not positive semidefinite. A value near zero indicates a singular boundary. For vcov, these findings mean standard errors, confidence intervals, z tests, and p values may be unreliable and can indicate empirical underidentification. A condition number above 1e8 flags severe numerical sensitivity; it is a warning rather than, by itself, proof of inadmissibility. Review excessive covariance paths, near-collinear factors, correlations, constraints, and identification.")
          ),
          if (can_refit) actionButton(paste0(prefix, "_heywood_refit"), "Constrained reanalysis", class = "btn-warning btn-sm"),
          if (length(variables) && !can_refit) tags$p(class = "structural-result-note", "Constrained residual-variance reanalysis is available only for continuous indicators estimated with ML or MLR."),
          tags$p(class = "structural-result-note", "A constrained reanalysis is a sensitivity analysis and does not resolve the source of the Heywood case.")
        )
      )
    })
    output[[paste0(prefix, "_result_latent_correlation_ci")]] <- renderUI({
      bundle <- fit_result()
      values <- structural_canvas_latent_correlation_intervals(bundle$fit, level = .95)
      if (!nrow(values)) return(NULL)
      values$r <- vapply(values$r, format_decimal3, character(1))
      values[["CI lower"]] <- vapply(values[["CI lower"]], format_decimal3, character(1))
      values[["CI upper"]] <- vapply(values[["CI upper"]], format_decimal3, character(1))
      values$p <- vapply(values$p, format_p, character(1))
      values$p[values$Type == "Fixed"] <- "—"
      tags$div(
        class = "structural-latent-correlation-ci",
        tags$h5("Latent correlation confidence intervals"),
        tags$div(class = "table-responsive", tags$table(
          class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Intervals are 95% delta-method intervals for explicitly estimated or fixed latent covariance paths. 'CI reaches |1|' flags an interval touching an inadmissible correlation boundary; implied correlations without an explicit covariance parameter are not assigned a delta-method interval here.")
      )
    })
    output[[paste0(prefix, "_result_validity_note")]] <- renderUI({
      bundle <- fit_result()
      validity_values <- result_table("validity")
      abnormal_reliability <- any(grepl("†", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
      single_indicator <- any(grepl("‡", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
      constrained_single_indicator <- any(grepl("¶", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
      orthogonal_not_assessed <- any(grepl("§", as.character(unlist(validity_values, use.names = FALSE)), fixed = TRUE))
      theta <- as.matrix(lavaan::lavInspect(bundle$fit, "theta"))
      correlated_errors <- length(theta) > 1L && any(abs(theta[row(theta) != col(theta)]) > sqrt(.Machine$double.eps), na.rm = TRUE)
      snapshot <- bundle$snapshot %||% list()
      missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
      has_higher_order <- any(vapply(snapshot$edges %||% list(), function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), logical(1)))
      validity_loadings <- lavaan::standardizedSolution(bundle$fit)
      validity_loadings <- validity_loadings[validity_loadings$op == "=~" & validity_loadings$rhs %in% lavaan::lavNames(bundle$fit, "ov"), c("lhs", "rhs"), drop = FALSE]
      cross_loaded_indicators <- unique(validity_loadings$rhs[duplicated(validity_loadings$rhs) | duplicated(validity_loadings$rhs, fromLast = TRUE)])
      tagList(
        tags$p(class = "structural-result-note", "Diagonal values in parentheses are sqrt(AVE); lower-triangle values are latent correlations. Max |r| is compared with sqrt(AVE). The remaining columns report the number of indicators (k), AVE, CR, Cronbach's alpha, and McDonald's omega total."),
        tags$p(class = "structural-result-note", "Fornell-Larcker is marked 'Criterion met' when sqrt(AVE) is greater than the factor's largest absolute correlation; otherwise it is marked 'Review needed'."),
        tags$p(class = "structural-result-note", "Guidance uses commonly cited descriptive cutoffs (AVE ≥ .50; CR, Cronbach's alpha, and omega total ≥ .70). These are heuristics rather than universal pass/fail rules and should be interpreted with construct breadth, item count, model admissibility, and study purpose."),
        if (length(missing_covariances)) tags$p(class = "structural-result-note", paste0("Caution: missing exogenous latent covariance paths (", paste(missing_covariances, collapse = ", "), ") are fixed to zero.")),
        if (correlated_errors) tags$p(class = "structural-result-note", "CR incorporates the estimated measurement-error covariances in the residual covariance matrix."),
        if (abnormal_reliability) tags$p(class = "structural-result-note", "† AVE or a reliability coefficient is unavailable or outside [0, 1], indicating unusual item covariances, an inadmissible solution, or an unidentified calculation."),
        if (single_indicator) tags$p(class = "structural-result-note", "‡ AVE and CR are not reported for a single-indicator factor without externally justified reliability constraints."),
        if (constrained_single_indicator) tags$p(class = "structural-result-note", "¶ The single-indicator factor uses an externally justified fixed residual variance. AVE, CR, and omega total reflect that imposed reliability constraint rather than independently estimated internal consistency; Cronbach's alpha and Fornell-Larcker are not assessed."),
        if (orthogonal_not_assessed) tags$p(class = "structural-result-note", "§ Fornell-Larcker was not assessed because one or more exogenous latent covariances were fixed to zero by omitted covariance paths."),
        if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, AVE, CR, and omega use the standardized latent-response solution; Cronbach's alpha uses lavaan's polychoric correlation matrix (ordinal alpha)."),
        if (!length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "Cronbach's alpha uses the analyzed indicators' sample covariance matrix. Omega is model-based and incorporates estimated residual covariances."),
        tags$p(class = "structural-result-note", "In this table, omega total uses the same model-implied loadings and residual covariance matrix as CR; under the current congeneric scoring specification it therefore equals CR. Both labels are retained for reporting clarity."),
        if (has_higher_order) tags$p(class = "structural-result-note", "AVE, CR, Fornell-Larcker, and HTMT are reported for first-order factors with observed indicators; higher-order factors are excluded from these reliability and discriminant-validity calculations."),
        if (length(cross_loaded_indicators)) tags$p(class = "structural-result-note", paste0("Caution: cross-loaded indicators (", paste(cross_loaded_indicators, collapse = ", "), ") appear in more than one factor. Factor-specific AVE/CR remain descriptive, while simple-structure discriminant-validity interpretations require particular caution.")),
        if (!isTRUE(bundle$diagnostics$admissible %||% TRUE)) tags$p(class = "structural-result-note", "Caution: the fitted solution failed one or more admissibility checks; AVE, CR, and discriminant-validity results should not be interpreted as final.")
      )
    })
    output[[paste0(prefix, "_result_factor_scores")]] <- renderUI({
      bundle <- fit_result()
      values <- structural_canvas_factor_score_quality(bundle$fit)
      if (!nrow(values)) return(NULL)
      values$Determinacy <- vapply(values$Determinacy, format_decimal3, character(1))
      values[["Score reliability"]] <- vapply(values[["Score reliability"]], format_decimal3, character(1))
      tagList(
        tags$h5("Factor-score quality"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", "Determinacy is the correlation between regression factor scores and the latent factor; score reliability is its square. Descriptive guidance uses .90 as strong and .80 as a cautious-use threshold. These indices concern estimated factor scores for downstream or individual-level use and are not substitutes for CR, omega, validity evidence, or model admissibility."),
        if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, factor-score quality is conditional on the fitted latent-response WLSMV model and category thresholds.")
      )
    })
    output[[paste0(prefix, "_result_reliability_bootstrap")]] <- renderUI({
      bundle <- fit_result()
      values <- bundle$reliability_bootstrap_result %||% NULL
      requested <- as.integer(bundle$reliability_bootstrap %||% 0L)
      if (requested <= 0L) return(tags$p(class = "structural-result-note", "AVE, CR, Cronbach's alpha, and omega are point estimates. Select AVE/reliability bootstrap CI in the analysis options when interval estimates are required."))
      if (is.null(values) || !nrow(values)) return(tags$p(class = "structural-result-note", "AVE/reliability bootstrap intervals could not be estimated because no resample produced usable estimates."))
      reliability_ci_method <- structural_canvas_bootstrap_ci_method(bundle$reliability_ci_method %||% "percentile")
      reliability_ci_label <- if (identical(reliability_ci_method, "bca")) "BCa" else "percentile"
      incomplete <- values[["Valid replicates"]] < values[["Requested replicates"]]
      caution_intervals <- values$Status == "Caution"
      unreliable_intervals <- values$Status == "Unreliable"
      boundary_interval <- !is.finite(values$Lower) | !is.finite(values$Upper) | values$Lower < 0 | values$Upper > 1
      if (!"CI method" %in% names(values)) values[["CI method"]] <- "Percentile"
      bca_unavailable <- "CI method" %in% names(values) && any(values[["CI method"]] == "BCa unavailable")
      values$Statistic[values$Statistic == "Alpha"] <- "Cronbach's α"
      values$Statistic[values$Statistic == "Omega"] <- "McDonald's ωtotal"
      values$Estimate <- paste0(vapply(values$Estimate, format_decimal3, character(1)), ifelse(!is.finite(values$Estimate) | values$Estimate < 0 | values$Estimate > 1, "†", ""))
      values$Lower <- paste0(vapply(values$Lower, format_decimal3, character(1)), ifelse(boundary_interval, "†", ""))
      values$Upper <- paste0(vapply(values$Upper, format_decimal3, character(1)), ifelse(boundary_interval, "†", ""))
      values[["Valid %"]] <- paste0(vapply(values[["Valid %"]], format_decimal3, character(1)), "%")
      names(values)[names(values) == "Lower"] <- "95% CI lower"
      names(values)[names(values) == "Upper"] <- "95% CI upper"
      tagList(
        tags$h5(paste0("AVE and reliability ", reliability_ci_label, " bootstrap intervals (", requested, " resamples; seed = ", bundle$reliability_seed, ")")),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
        )),
        tags$p(class = "structural-result-note", paste0("Intervals use case resampling", if (identical(reliability_ci_method, "bca")) " with leave-one-out jackknife acceleration" else " percentile bootstrap", " and the selected ", if (identical(bundle$validity_formula, "model_implied")) "model-implied parameter" else "standardized-loading", " AVE/CR formula. McDonald's omega total follows the same fitted congeneric scoring formula as CR in this output.")),
        if (bca_unavailable) tags$p(class = "structural-result-note", "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that statistic; increase valid replicates or use percentile CI for reporting."),
        if (any(boundary_interval)) tags$p(class = "structural-result-note", "† marks an estimate or interval extending outside the admissible [0, 1] coefficient range. Do not truncate the interval for reporting; investigate model admissibility, sample instability, item covariance structure, and failed resamples."),
        if (any(incomplete)) tags$p(class = "structural-result-note", "Some resamples failed the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA, or yielded unavailable statistics. Interpret intervals cautiously when the valid-replicate count is materially below the requested count."),
        if (any(caution_intervals)) tags$p(class = "structural-result-note", "Caution indicates that 50% to less than 80% of requested resamples yielded the statistic. Treat the percentile limits as unstable and report the valid-replicate count."),
        if (any(unreliable_intervals)) tags$p(class = "structural-result-note", "Unreliable indicates that fewer than 50% of requested resamples yielded the statistic. The displayed quantiles are diagnostic only and should not be reported as a defensible confidence interval; resolve convergence, admissibility, sparse-category, or specification problems first.")
      )
    })
    output[[paste0(prefix, "_result_htmt")]] <- renderUI({
      bundle <- fit_result()
      fit <- bundle$fit
      standardized <- lavaan::standardizedSolution(fit)
      loadings <- standardized[standardized$op == "=~", c("lhs", "rhs"), drop = FALSE]
      loadings <- loadings[loadings$rhs %in% lavaan::lavNames(fit, "ov"), , drop = FALSE]
      factor_names <- unique(loadings$lhs)
      if (length(factor_names) < 2L) return(NULL)
      indicators_by_factor <- stats::setNames(lapply(factor_names, function(name) unique(loadings$rhs[loadings$lhs == name])), factor_names)
      sample_statistics <- lavaan::lavInspect(fit, "sampstat")
      sample_covariance <- sample_statistics$cov %||% NULL
      if (is.null(sample_covariance)) return(tags$p(class = "structural-result-note", "HTMT is not currently displayed for multigroup sample statistics."))
      sample_correlations <- stats::cov2cor(as.matrix(sample_covariance))
      threshold <- as.numeric(bundle$htmt_threshold %||% .85)
      htmt <- structural_canvas_htmt(sample_correlations, indicators_by_factor, threshold)
      matrix_values <- matrix("", nrow = length(factor_names), ncol = length(factor_names) + 1L)
      colnames(matrix_values) <- c("Factor", factor_names)
      for (row in seq_along(factor_names)) {
        matrix_values[row, 1L] <- factor_names[[row]]
        for (column in seq_along(factor_names)) {
          if (row == column) matrix_values[row, column + 1L] <- "—"
          else if (row > column) matrix_values[row, column + 1L] <- format_decimal3(htmt$matrix[row, column])
        }
      }
      pair_table <- htmt$pairs
      pair_table$HTMT <- vapply(pair_table$HTMT, format_decimal3, character(1))
      pair_table$Reason[!nzchar(pair_table$Reason)] <- "—"
      bootstrap_reps <- as.integer(bundle$htmt_bootstrap %||% 0L)
      bootstrap_seed <- as.integer(bundle$htmt_seed %||% 12345L)
      htmt_ci_method <- structural_canvas_bootstrap_ci_method(bundle$htmt_ci_method %||% "percentile")
      htmt_ci_label <- if (identical(htmt_ci_method, "bca")) "BCa" else "percentile"
      bootstrap_table <- NULL
      bootstrap_incomplete <- FALSE
      bootstrap_caution <- FALSE
      bootstrap_unreliable <- FALSE
      bootstrap_bca_unavailable <- FALSE
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
      tagList(
        tags$h5(paste0("HTMT (threshold = ", format(threshold, nsmall = 2L), ")")),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-matrix",
          tags$thead(tags$tr(lapply(colnames(matrix_values), tags$th))),
          tags$tbody(lapply(seq_len(nrow(matrix_values)), function(index) tags$tr(lapply(as.character(matrix_values[index, ]), tags$td))))
        )),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-criterion",
          tags$thead(tags$tr(lapply(c("Factor 1", "Factor 2", "HTMT", "Criterion", "Reason"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(pair_table)), function(index) tags$tr(lapply(as.character(pair_table[index, ]), tags$td))))
        )),
        if (!is.null(bootstrap_table)) tagList(
          tags$h5(paste0("HTMT ", htmt_ci_label, " bootstrap confidence intervals (", bootstrap_reps, " resamples; seed = ", bootstrap_seed, ")")),
          tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered structural-htmt-bootstrap",
            tags$thead(tags$tr(lapply(names(bootstrap_table), tags$th))),
            tags$tbody(lapply(seq_len(nrow(bootstrap_table)), function(index) tags$tr(lapply(as.character(bootstrap_table[index, ]), tags$td))))
          ))
        ),
        tags$p(class = "structural-result-note", "HTMT uses absolute item correlations. Values below the selected threshold are marked 'Criterion met'."),
        if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, HTMT uses lavaan's polychoric latent-response correlations."),
        if (bootstrap_reps <= 0L) tags$p(class = "structural-result-note", "HTMT point estimates are descriptive. Select HTMT bootstrap CI in the analysis options when interval estimates are required."),
        if (bootstrap_reps > 0L && is.null(bootstrap_table)) tags$p(class = "structural-result-note", "HTMT bootstrap confidence intervals could not be estimated from the selected indicators."),
        if (bootstrap_incomplete) tags$p(class = "structural-result-note", "Some bootstrap resamples could not produce an admissible correlation matrix, commonly because an ordered category was absent or sparse. Interpret intervals with reduced valid-replicate counts cautiously."),
        if (bootstrap_bca_unavailable) tags$p(class = "structural-result-note", "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that pair; increase valid replicates or use percentile CI for reporting."),
        if (bootstrap_caution) tags$p(class = "structural-result-note", "HTMT bootstrap status Caution means that 50% to less than 80% of requested resamples were valid. Treat the interval as unstable and report the valid-replicate count."),
        if (bootstrap_unreliable) tags$p(class = "structural-result-note", "HTMT bootstrap status Unreliable means that fewer than 50% of requested resamples were valid. Confidence limits and threshold decisions are not assessed and should not be used as discriminant-validity evidence."),
        if (!is.null(bootstrap_table)) tags$p(class = "structural-result-note", paste0(
          "The ", htmt_ci_label, " interval is based on case resampling", if (identical(htmt_ci_method, "bca")) " with leave-one-out jackknife acceleration" else "", if (length(bundle$ordered %||% character(0))) " with polychoric correlations re-estimated in each resample" else "",
          ". 'Upper < threshold' uses the one-sided 95% upper confidence limit for the selected .85/.90 criterion. 'Upper < 1' indicates whether the two-sided 95% interval excludes 1. These are intentionally reported separately from the point-estimate criterion."
        ))
      )
    })
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
    for (kind in c("overview", "validity", "measurement")) local({
      result_kind <- kind
      output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
    })
    output[[paste0(prefix, "_result_mi")]] <- renderUI({
      table <- result_table("mi")
      if (!nrow(table)) return(NULL)
      theory_mi <- identical(fit_result()$mi_mode %||% "theory", "theory")
      tagList(tags$table(
        class = "table table-striped table-bordered structural-mi-table",
        tags$thead(tags$tr(
          lapply(names(table), tags$th),
          if (theory_mi) tags$th("Select")
        )),
        tags$tbody(lapply(seq_len(nrow(table)), function(index) {
          tags$tr(
            lapply(as.character(table[index, , drop = TRUE]), tags$td),
            if (theory_mi) tags$td(actionButton(
              paste0(prefix, "_mi_select_", index),
              "Select",
              class = "btn-sm structural-mi-select-button"
            ))
          )
        }))
      ),
      tags$p(class = "structural-result-note", "MI p treats each modification index as an unscaled asymptotic 1-df chi-square test. BH-adjusted p controls the false-discovery rate across all finite lavaan candidate modifications before the displayed MI and theory filters; MI tests reports that multiplicity-family size."),
      tags$p(class = "structural-result-note", "For MLR or WLSMV, these derived p values are not a separate robust/scaled score-test correction and should be treated as exploratory reference values."),
      tags$p(class = "structural-result-note", "EPC is the expected unstandardized parameter change if the fixed parameter is freed; Std. EPC is lavaan's fully standardized expected change (sepc.all). Consider direction and magnitude rather than MI rank alone."),
      if (theory_mi) tags$p(class = "structural-result-note", "Each Step is sequential: the displayed path is added, the model is refitted, and MI, multiplicity family, EPC, and cumulative fit for the next row are recomputed from that updated model. Rows are not simultaneous candidates from one unchanged model."),
      if (theory_mi) tags$p(class = "structural-result-note", "Skipped unsafe counts higher-ranked candidates rejected for nonconvergence, post.check failure, negative variance, a non-positive-definite or boundary residual/latent/parameter covariance matrix, invalid df, or |latent correlation| >= 1. Skipped details records each rejected path and diagnostic reason; a skipped candidate is not offered for automatic application."),
      tags$p(class = "structural-result-note", "Neither an unadjusted nor adjusted p value justifies a modification. Use effect size (EPC/standardized EPC), residual diagnostics, admissibility, theory, and preferably independent validation."))
    })
    output[[paste0(prefix, "_result_mi_history")]] <- renderUI({
      bundle <- fit_result()
      history <- bundle$mi_history %||% data.frame()
      if (!nrow(history)) return(NULL)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      display <- history[, setdiff(names(history), "Signature"), drop = FALSE]
      for (name in intersect(c("MI", "EPC", "CFI", "TLI", "RMSEA", "SRMR"), names(display))) {
        display[[name]] <- vapply(display[[name]], format_decimal3, character(1))
      }
      display$Justification[!nzchar(display$Justification)] <- if (ko) "제공되지 않음" else "Not provided"
      div(class = "result-section regression-result-panel structural-mi-history-result",
        h4(if (ko) "MI 수정 이력" else "MI modification history"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(display), tags$th))),
          tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
        ),
        tags$p(class = "structural-result-note", if (ko) "MI, EPC, 누적 적합도 값은 해당 경로를 선택한 시점의 값입니다. 근거란에는 각 모수를 자유화한 실질적 이유를 기록해야 합니다." else "MI, EPC, and cumulative fit values are those available when the path was selected. The justification should document the substantive reason for freeing each parameter."),
        tags$p(class = "structural-result-note", if (ko) "MI 기반 수정은 탐색적 수정 모형이며 독립 표본에서 교차검증해야 합니다." else "MI-driven modifications are exploratory and should be cross-validated in an independent sample.")
      )
    })
    output[[paste0(prefix, "_result_mi_holdout")]] <- renderUI({
      bundle <- fit_result()
      if (!isTRUE(bundle$mi_holdout_enabled)) return(NULL)
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      comparison <- bundle$holdout_comparison %||% NULL
      if (is.null(comparison)) return(tags$div(class = "result-section regression-result-panel",
        tags$h4(if (ko) "MI 홀드아웃 검증" else "MI holdout validation"),
        tags$p(paste0(if (ko) "탐색 표본 N = " else "Exploration N = ", nrow(bundle$analysis_data), if (ko) "; 예약 검증 표본 N = " else "; reserved validation N = ", nrow(bundle$validation_data), ".")),
        tags$p(class = "structural-result-note", if (ko) "MI 후보와 현재 표시된 CFA 추정값은 탐색 표본만 사용한 결과입니다. MI 경로를 적용한 뒤 검증 결과가 표시됩니다." else "MI candidates and all currently displayed CFA estimates are based only on the exploration sample. Validation results will appear after an MI path is applied.")
      ))
      table <- comparison$table
      for (name in c("Chisq", "df", "CFI", "TLI", "SRMR", "RMSEA")) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
      table$p <- vapply(table$p, format_p, character(1))
      changes <- comparison$changes
      for (name in names(changes)[vapply(changes, is.numeric, logical(1)) & names(changes) != "DeltaP"]) changes[[name]] <- vapply(changes[[name]], format_decimal3, character(1))
      changes$DeltaP <- vapply(changes$DeltaP, format_p, character(1))
      div(class = "result-section regression-result-panel structural-mi-holdout-result",
        h4(if (ko) "MI 홀드아웃 검증" else "MI holdout validation"),
        tags$p(paste0(if (ko) "탐색 행 수 = " else "Exploration rows = ", nrow(bundle$analysis_data), if (ko) "; 예약 검증 행 수 = " else "; reserved validation rows = ", comparison$validation_n_raw, if (ko) "; 사용된 검증 N = " else "; validation N used = ", paste(unique(comparison$validation_n_used), collapse = ", "), if (ko) "; 분할 seed = " else "; split seed = ", bundle$mi_holdout_seed, ".")),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
        )),
        tags$h5(if (ko) "검증표본 변화: 수정 모형 - 기존 모형" else "Validation-sample change: modified minus original"),
        tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(changes), tags$th))),
          tags$tbody(tags$tr(lapply(as.character(changes[1L, ]), tags$td)))
        )),
        if (any(!comparison$table$Admissible)) tags$p(class = "structural-result-note", if (ko) "검증표본의 한 모형 또는 두 모형이 주 CFA와 동일한 전체 admissibility 점검을 통과하지 못했습니다. 검증표본 변화 통계와 공식 차이 검정은 표시하지 않으며, 이 수정은 반복검증된 것으로 해석하면 안 됩니다." else "One or both validation-sample models failed the same full admissibility checks as the main CFA. Validation-sample change statistics and the formal difference test are suppressed; the modification must not be treated as replicated."),
        tags$p(class = "structural-result-note", if (ko) "MI 경로는 탐색 표본에서만 선택되었습니다. 위 표는 예약된 검증 표본에서 두 모형을 독립적으로 다시 적합한 결과입니다. 적합도 개선의 반복은 안정성을 뒷받침하지만 실질적 근거를 대체하지 않으며, 반복되지 않으면 표본 특이적 수정일 가능성이 큽니다." else "The MI path was selected only in the exploration sample. The table above refits both models independently in the reserved validation sample. Replication of improved fit supports stability but does not replace substantive justification; failure to replicate indicates likely sample-specific modification."),
        tags$p(class = "structural-result-note", if (ko) "검증 표본은 이제 공개되어 잠겼습니다. 이 분할에서는 추가 MI 변경을 비활성화합니다. 다른 수정 모형을 평가하려면 새 분할 seed로 새 분석을 시작하십시오." else "The validation sample is now unblinded and locked. Further MI changes are disabled for this split; start a new analysis with a newly chosen split seed to evaluate a different modified model.")
      )
    })
    execute_analysis <- function(snapshot, settings = NULL) {
      is_mi_refit <- !is.null(settings) && !is.null(settings$fit)
      settings <- settings %||% list()
      identification <- structural_canvas_identification_diagnostics(snapshot)
      identification_errors <- identification[identification$Severity == "Error", , drop = FALSE]
      if (nrow(identification_errors)) {
        stop(paste0("Model identification check failed: ", paste(paste0(identification_errors$Element, " — ", identification_errors$Message), collapse = "; ")))
      }
      identification_warnings <- identification[identification$Severity == "Warning", , drop = FALSE]
      if (nrow(identification_warnings)) {
        showNotification(paste0("Identification warning: ", paste(paste0(identification_warnings$Element, " — ", identification_warnings$Message), collapse = "; ")), type = "warning", duration = 12)
      }
      estimator <- settings$estimator %||% input[[paste0(prefix, "_estimator")]] %||% "ML"
      missing <- settings$missing %||% input[[paste0(prefix, "_missing")]] %||% "fiml"
      std_lv <- settings$std_lv %||% identical(input[[paste0(prefix, "_scale")]], "variance")
      mi_mode <- settings$mi_mode %||% input[[paste0(prefix, "_mi_mode")]] %||% "theory"
      rmsea_ci <- settings$rmsea_ci %||% as.numeric(input[[paste0(prefix, "_rmsea_ci")]] %||% .90)
      validity_formula <- settings$validity_formula %||% input[[paste0(prefix, "_validity_formula")]] %||% "standardized"
      reliability_bootstrap <- suppressWarnings(as.integer(settings$reliability_bootstrap %||% input[[paste0(prefix, "_reliability_bootstrap")]] %||% 0L))
      if (is.na(reliability_bootstrap) || !reliability_bootstrap %in% c(0L, 500L, 1000L, 2000L)) reliability_bootstrap <- 0L
      reliability_seed <- suppressWarnings(as.integer(settings$reliability_seed %||% input[[paste0(prefix, "_reliability_seed")]] %||% 24680L))
      if (is.na(reliability_seed) || reliability_seed < 1L) reliability_seed <- 24680L
      reliability_ci_method <- structural_canvas_bootstrap_ci_method(settings$reliability_ci_method %||% input[[paste0(prefix, "_reliability_ci_method")]] %||% "percentile")
      bollen_stine_bootstrap <- suppressWarnings(as.integer(settings$bollen_stine_bootstrap %||% input[[paste0(prefix, "_bollen_stine_bootstrap")]] %||% 0L))
      if (is.na(bollen_stine_bootstrap) || !bollen_stine_bootstrap %in% c(0L, 500L, 1000L, 2000L)) bollen_stine_bootstrap <- 0L
      bollen_stine_seed <- suppressWarnings(as.integer(settings$bollen_stine_seed %||% input[[paste0(prefix, "_bollen_stine_seed")]] %||% 97531L))
      if (is.na(bollen_stine_seed) || bollen_stine_seed < 1L) bollen_stine_seed <- 97531L
      htmt_threshold <- as.numeric(settings$htmt_threshold %||% input[[paste0(prefix, "_htmt_threshold")]] %||% .85)
      if (!is.finite(htmt_threshold) || !htmt_threshold %in% c(.85, .90)) htmt_threshold <- .85
      htmt_bootstrap <- suppressWarnings(as.integer(settings$htmt_bootstrap %||% input[[paste0(prefix, "_htmt_bootstrap")]] %||% 0L))
      if (is.na(htmt_bootstrap) || !htmt_bootstrap %in% c(0L, 500L, 1000L, 2000L)) htmt_bootstrap <- 0L
      htmt_seed <- suppressWarnings(as.integer(settings$htmt_seed %||% input[[paste0(prefix, "_htmt_seed")]] %||% 12345L))
      if (is.na(htmt_seed) || htmt_seed < 1L) htmt_seed <- 12345L
      htmt_ci_method <- structural_canvas_bootstrap_ci_method(settings$htmt_ci_method %||% input[[paste0(prefix, "_htmt_ci_method")]] %||% "percentile")
      invariance_enabled <- isTRUE(settings$invariance_enabled %||% input[[paste0(prefix, "_invariance_enabled")]] %||% FALSE)
      invariance_group <- as.character(settings$invariance_group %||% input[[paste0(prefix, "_invariance_group")]] %||% "")
      mi_holdout_enabled <- isTRUE(settings$mi_holdout_enabled %||% input[[paste0(prefix, "_mi_holdout_enabled")]] %||% FALSE)
      mi_holdout_fraction <- as.numeric(settings$mi_holdout_fraction %||% input[[paste0(prefix, "_mi_holdout_fraction")]] %||% .30)
      mi_holdout_seed <- suppressWarnings(as.integer(settings$mi_holdout_seed %||% input[[paste0(prefix, "_mi_holdout_seed")]] %||% 13579L))
      if (is.na(mi_holdout_seed) || mi_holdout_seed < 1L) mi_holdout_seed <- 13579L
      result_coefficient <- settings$result_coefficient %||% input[[paste0(prefix, "_result_coefficient")]] %||% "beta"
      residual_variance_fixes <- settings$residual_variance_fixes %||% numeric(0)
      full_data <- dataset_fn()
      variable_table <- variable_table_fn()
      nominal <- structural_canvas_nominal_indicators(snapshot, variable_table)
      if (length(nominal)) {
        stop(sprintf(
          "Nominal indicators are not supported by standard CFA/SEM: %s. Reclassify them as ordered only when their categories have a meaningful order.",
          paste(nominal, collapse = ", ")
        ))
      }
      ordered <- structural_canvas_ordered_indicators(snapshot, variable_table)
      if (length(ordered) > 0 || identical(toupper(estimator), "WLSMV")) {
        if (toupper(estimator) %in% c("ML", "MLR")) estimator <- "WLSMV"
        if (identical(missing, "fiml")) missing <- "pairwise"
      }
      structural_canvas_validate_holdout_options(
        mi_holdout_enabled, analysis_type, estimator, ordered,
        invariance_enabled, residual_variance_fixes
      )
      if (mi_holdout_enabled) {
        if (!is.null(settings$analysis_data) && !is.null(settings$validation_data)) {
          data <- settings$analysis_data
          validation_data <- settings$validation_data
          holdout_rows <- settings$holdout_rows %||% list()
        } else {
          split <- structural_canvas_holdout_split(full_data, mi_holdout_fraction, mi_holdout_seed)
          data <- split$exploration
          validation_data <- split$validation
          holdout_rows <- list(exploration = split$exploration_rows, validation = split$validation_rows)
        }
      } else {
        data <- full_data
        validation_data <- NULL
        holdout_rows <- list()
      }
      missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
      if (analysis_type %in% c("cfa", "cbsem") && length(missing_covariances)) {
        showNotification(paste0("Missing covariance paths between exogenous latent variables: ", paste(missing_covariances, collapse = ", "), ". These covariances will be fixed to zero."), type = "warning", duration = 10)
      }
      result <- run_structural_canvas_analysis(snapshot, data, analysis_type, estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, nominal = nominal, residual_variance_fixes = residual_variance_fixes)
      if (!isTRUE(result$admissible)) {
        details <- c(
          if (!isTRUE(result$converged)) "the model did not converge",
          if (!isTRUE(result$post_check)) "lavaan post-estimation checks failed",
          if (!isTRUE(result$identified)) paste0("model degrees of freedom are invalid (df = ", format_decimal3(result$df), ")"),
          if (length(result$negative_residuals)) paste0("negative residual variances: ", paste(result$negative_residuals, collapse = ", ")),
          if (length(result$negative_latent_variances)) paste0("negative latent variances: ", paste(result$negative_latent_variances, collapse = ", ")),
          if (isTRUE(result$non_psd_theta)) paste0("residual covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
          if (isTRUE(result$non_psd_latent_covariance)) paste0("latent covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
          if (isTRUE(result$near_singular_theta)) paste0("residual covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
          if (isTRUE(result$near_singular_latent_covariance)) paste0("latent covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
          if (isTRUE(result$non_psd_parameter_covariance)) paste0("parameter-estimate covariance matrix is not positive semidefinite (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
          if (isTRUE(result$near_singular_parameter_covariance)) paste0("parameter-estimate covariance matrix is near singular (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
          if (isTRUE(result$invalid_correlations)) "one or more absolute latent correlations are at least 1"
        )
        showNotification(paste0("Potentially inadmissible solution: ", paste(details, collapse = "; "), ". Interpret fit, AVE, CR, and validity results with caution."), type = "error", duration = NULL)
      }
      conditioning_details <- c(
        if (isTRUE(result$ill_conditioned_theta)) paste0("residual covariance condition number = ", format(result$theta_condition_number, scientific = TRUE, digits = 3)),
        if (isTRUE(result$ill_conditioned_latent_covariance)) paste0("latent covariance condition number = ", format(result$latent_condition_number, scientific = TRUE, digits = 3)),
        if (isTRUE(result$ill_conditioned_parameter_covariance)) paste0("parameter-estimate covariance condition number = ", format(result$parameter_condition_number, scientific = TRUE, digits = 3))
      )
      if (length(conditioning_details)) {
        showNotification(paste0("Numerically ill-conditioned solution: ", paste(conditioning_details, collapse = "; "), ". Small data or specification changes may produce unstable estimates."), type = "warning", duration = 12)
      }
      if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
        if (invariance_enabled) stop("Bollen-Stine bootstrap cannot be combined with measurement-invariance analysis; assess global fit within the appropriate group model instead of the pooled CFA.")
        eligibility <- structural_canvas_bollen_stine_eligibility(result$fit)
        if (!isTRUE(eligibility$available)) stop(eligibility$reason)
      }
      invariance_result <- NULL
      if (identical(analysis_type, "cfa") && invariance_enabled) {
        if (!length(ordered) && !toupper(estimator) %in% c("ML", "MLR")) stop("Continuous-indicator measurement invariance requires ML or MLR.")
        if (length(ordered) && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop("Ordered-indicator measurement invariance requires WLSMV or DWLS.")
        if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop("Select a valid grouping variable for measurement invariance analysis.")
        if (invariance_group %in% lavaan::lavNames(result$fit, "ov")) stop("The grouping variable cannot also be an indicator in the CFA model.")
        group_count <- length(unique(data[[invariance_group]][!is.na(data[[invariance_group]])]))
        if (group_count < 2L || group_count > 20L) stop("The grouping variable must contain between 2 and 20 non-empty groups.")
        invariance_result <- shiny::withProgress(message = "Estimating measurement-invariance models", value = 0, {
          shiny::incProgress(.15, detail = "Configural, metric, scalar, and strict models")
          value <- structural_canvas_measurement_invariance(result$syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered)
          shiny::incProgress(.85, detail = "Preparing robust comparisons")
          value
        })
      }
      reliability_bootstrap_result <- NULL
      if (identical(analysis_type, "cfa") && reliability_bootstrap > 0L) {
        structural_canvas_validate_model_based_bootstrap(result$fit, "AVE/reliability bootstrap")
        reliability_bootstrap_result <- shiny::withProgress(message = "Estimating AVE and reliability confidence intervals", value = 0, {
          shiny::incProgress(.05, detail = paste0(reliability_bootstrap, " case-resampling replicates"))
          value <- structural_canvas_reliability_bootstrap(
            result$syntax, data, reps = reliability_bootstrap, confidence = .95, seed = reliability_seed,
            estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, formula_mode = validity_formula,
            original_fit = result$fit,
            ci_method = reliability_ci_method,
            progress = function(done, total, valid) {
              shiny::setProgress(
                value = .05 + .90 * (as.numeric(done) / max(1, as.numeric(total))),
                detail = sprintf("Reliability bootstrap %s/%s; valid replicates %s", done, total, valid)
              )
            }
          )
          if (nrow(value)) {
            point <- structural_canvas_reliability_estimates(result$fit, validity_formula)
            statistic_column <- c(AVE = "AVE", CR = "CR", Alpha = "Alpha", Omega = "Omega")
            value$Estimate <- vapply(seq_len(nrow(value)), function(index) {
              row <- point[point$Factor == value$Factor[[index]], , drop = FALSE]
              column <- statistic_column[[value$Statistic[[index]]]]
              if (nrow(row) && column %in% names(row)) as.numeric(row[[column]][[1L]]) else NA_real_
            }, numeric(1))
            value$`Valid %` <- 100 * value[["Valid replicates"]] / value[["Requested replicates"]]
            value <- value[, c("Factor", "Statistic", "Estimate", "Lower", "Upper", "CI method", "Valid replicates", "Requested replicates", "Valid %", "Status"), drop = FALSE]
          }
          shiny::incProgress(.95, detail = paste0("Preparing ", if (identical(reliability_ci_method, "bca")) "BCa" else "percentile", " intervals"))
          value
        })
      }
      bollen_stine_result <- NULL
      if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
        bollen_stine_result <- shiny::withProgress(message = "Estimating Bollen-Stine global-fit p value", value = 0, {
          shiny::incProgress(.05, detail = paste0(bollen_stine_bootstrap, " transformed-data bootstrap replicates"))
          value <- structural_canvas_bollen_stine(result$fit, bollen_stine_bootstrap, bollen_stine_seed)
          shiny::incProgress(.95, detail = "Preparing bootstrap goodness-of-fit result")
          value
        })
      }
      mi <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_mi_refits(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = mi_mode, ordered = ordered) else NULL
      htmt_bootstrap_result <- NULL
      if (analysis_type %in% c("cfa", "cbsem") && htmt_bootstrap > 0L) {
        standardized_for_htmt <- lavaan::standardizedSolution(result$fit)
        observed_for_htmt <- lavaan::lavNames(result$fit, "ov")
        loadings_for_htmt <- standardized_for_htmt[
          standardized_for_htmt$op == "=~" & standardized_for_htmt$rhs %in% observed_for_htmt,
          c("lhs", "rhs"), drop = FALSE
        ]
        factor_names_for_htmt <- unique(loadings_for_htmt$lhs)
        if (length(factor_names_for_htmt) >= 2L) {
          indicators_for_htmt <- stats::setNames(lapply(factor_names_for_htmt, function(name) {
            unique(loadings_for_htmt$rhs[loadings_for_htmt$lhs == name])
          }), factor_names_for_htmt)
          htmt_bootstrap_result <- shiny::withProgress(
            message = "Estimating HTMT bootstrap confidence intervals",
            value = 0,
            {
              shiny::incProgress(.1, detail = paste0(htmt_bootstrap, " case-resampling replicates"))
              value <- structural_canvas_htmt_bootstrap(
                data, indicators_for_htmt, reps = htmt_bootstrap, confidence = .95,
                seed = htmt_seed, ordered = ordered, threshold = htmt_threshold,
                ci_method = htmt_ci_method,
                progress = function(done, total, valid) {
                  shiny::setProgress(
                    value = .10 + .80 * (as.numeric(done) / max(1, as.numeric(total))),
                    detail = sprintf("HTMT bootstrap %s/%s; valid replicates %s", done, total, valid)
                  )
                }
              )
              shiny::incProgress(.9, detail = "Preparing interval estimates")
              value
            }
          )
        }
      }
      baseline_fit <- if (is_mi_refit) settings$baseline_fit %||% settings$fit else result$fit
      baseline_diagnostics <- if (is_mi_refit) settings$baseline_diagnostics %||% settings$diagnostics else result
      baseline_syntax <- if (is_mi_refit) settings$baseline_syntax %||% settings$syntax else result$syntax
      holdout_comparison <- NULL
      if (mi_holdout_enabled && identical(settings$comparison_type %||% "", "mi") && !is.null(validation_data)) {
        holdout_comparison <- structural_canvas_holdout_model_comparison(
          baseline_syntax, result$syntax, validation_data,
          estimator = estimator, missing = missing, std_lv = std_lv, ci_level = rmsea_ci
        )
      }
      fit_result(list(
        fit = result$fit, syntax = result$syntax, snapshot = snapshot, mi = mi, mi_mode = mi_mode,
        rmsea_ci = rmsea_ci, validity_formula = validity_formula,
        reliability_bootstrap = reliability_bootstrap, reliability_seed = reliability_seed,
        reliability_ci_method = reliability_ci_method,
        reliability_bootstrap_result = reliability_bootstrap_result,
        bollen_stine_bootstrap = bollen_stine_bootstrap, bollen_stine_seed = bollen_stine_seed,
        bollen_stine_result = bollen_stine_result,
        htmt_threshold = htmt_threshold, htmt_bootstrap = htmt_bootstrap, htmt_seed = htmt_seed,
        htmt_ci_method = htmt_ci_method,
        htmt_bootstrap_result = htmt_bootstrap_result,
        invariance_enabled = invariance_enabled, invariance_group = invariance_group, invariance_result = invariance_result,
        mi_holdout_enabled = mi_holdout_enabled, mi_holdout_fraction = mi_holdout_fraction, mi_holdout_seed = mi_holdout_seed,
        analysis_data = data, validation_data = validation_data, holdout_rows = holdout_rows, holdout_comparison = holdout_comparison,
        estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered,
        result_coefficient = result_coefficient, diagnostics = result,
        baseline_fit = baseline_fit, modified_from_baseline = is_mi_refit || isTRUE(settings$modified_from_baseline),
        baseline_syntax = baseline_syntax,
        baseline_diagnostics = baseline_diagnostics,
        comparison_label = settings$comparison_label %||% NULL,
        comparison_type = settings$comparison_type %||% NULL,
        residual_variance_fixes = residual_variance_fixes,
        identification = identification,
        mi_history = settings$mi_history %||% data.frame()
      ))
      session$sendCustomMessage(
        "custom-model-canvas-result",
        list(
          rootId = paste0(prefix, "-canvas-root"),
          source = snapshot,
          result = structural_canvas_result_snapshot(snapshot, result$fit, result_coefficient),
          show = TRUE
        )
      )
      result
    }
    run_confirmed_analysis <- function(snapshot, settings = list()) {
      result <- execute_analysis(snapshot, settings)
      showNotification(
        if (identical(statedu_current_language(app_language_fn), "ko")) paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " 遺꾩꽍???꾨즺?섏뿀?듬땲??") else paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " analysis completed."),
        type = if (isTRUE(result$converged)) "message" else "warning"
      )
      result
    }
    observeEvent(input[[canvas_input]], mark_settings_dirty(), ignoreInit = TRUE)
    observeEvent(input[[confirm_input]], {
      package <- structural_analysis_package(analysis_type)
      if (!requireNamespace(package, quietly = TRUE)) {
        showNotification(sprintf("%s package is required.", package), type = "error")
      } else {
        tryCatch({
          snapshot <- input[[confirm_input]]
          recommendation <- structural_canvas_estimator_recommendation(
            snapshot, dataset_fn(), variable_table_fn(), analysis_type,
            input[[paste0(prefix, "_estimator")]] %||% "ML"
          )
          if (isTRUE(recommendation$recommend)) {
            pending_estimator_snapshot(snapshot)
            diagnosis <- recommendation$diagnosis
            bollen_requested <- identical(analysis_type, "cfa") && as.integer(input[[paste0(prefix, "_bollen_stine_bootstrap")]] %||% 0L) > 0L
            ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
            showModal(modalDialog(
              title = if (ko) "추정량 권고" else "Estimator recommendation",
              tags$p(if (ko) "Mardia 진단에서 연속형 지표의 비정규성이 확인되었습니다. 이 모형을 적합하기 전에 강건 MLR 사용을 권장합니다." else "Mardia diagnostics flagged nonnormal continuous indicators. Robust MLR is recommended before fitting this model."),
              tags$p(paste0(
                if (ko) "Mardia 왜도 p = " else "Mardia skewness p = ", format_p(diagnosis$skew_p),
                if (ko) "; 첨도 p = " else "; kurtosis p = ", format_p(diagnosis$kurtosis_p),
                if (ko) "; 완전 사례 = " else "; complete cases = ", diagnosis$n, if (ko) " / " else " of ", diagnosis$original_n, "."
              )),
              if (bollen_requested) tags$p(class = "structural-result-note", if (ko) "Bollen-Stine 부트스트랩은 ML에서만 사용할 수 있습니다. MLR을 선택하면 Bollen-Stine 부트스트랩 없이 모형을 실행합니다." else "Bollen-Stine bootstrap is available only for ML; choosing MLR will run the model without Bollen-Stine bootstrap."),
              footer = tagList(
                modalButton(if (ko) "취소" else "Cancel"),
                actionButton(paste0(prefix, "_run_with_ml"), if (ko) "ML로 실행" else "Run with ML", class = "btn-default"),
                actionButton(paste0(prefix, "_run_with_mlr"), if (ko) "MLR로 실행" else "Run with MLR", class = "btn-primary")
              ),
              easyClose = TRUE
            ))
            return()
          }
          run_confirmed_analysis(snapshot)
          return()
          result <- execute_analysis(snapshot)
          showNotification(
            if (identical(statedu_current_language(app_language_fn), "ko")) paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " 분석이 완료되었습니다.") else paste0(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), " analysis completed."),
            type = if (isTRUE(result$converged)) "message" else "warning"
          )
        }, error = function(error) {
          showNotification(conditionMessage(error), type = "error", duration = 8)
        })
      }
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_run_with_ml")]], {
      snapshot <- pending_estimator_snapshot()
      removeModal()
      shiny::req(!is.null(snapshot))
      tryCatch({
        run_confirmed_analysis(snapshot, list(estimator = "ML"))
      }, error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_run_with_mlr")]], {
      snapshot <- pending_estimator_snapshot()
      removeModal()
      shiny::req(!is.null(snapshot))
      tryCatch({
        settings <- list(estimator = "MLR")
        if (identical(analysis_type, "cfa")) settings$bollen_stine_bootstrap <- 0L
        run_confirmed_analysis(snapshot, settings)
      }, error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)
    lapply(seq_len(100L), function(index) local({
      row_index <- index
      observeEvent(input[[paste0(prefix, "_mi_select_", row_index)]], {
        bundle <- fit_result()
        shiny::req(!is.null(bundle), !is.null(bundle$mi), nrow(bundle$mi) >= row_index)
        reuse_error <- tryCatch({
          structural_canvas_validate_holdout_reuse(bundle$mi_holdout_enabled, !is.null(bundle$holdout_comparison))
          NULL
        }, error = identity)
        if (!is.null(reuse_error)) {
          showNotification(conditionMessage(reuse_error), type = "error", duration = 12)
          return()
        }
        selected_rows <- if (identical(bundle$mi_mode %||% "theory", "theory")) seq_len(row_index) else row_index
        existing <- bundle$mi_history %||% data.frame()
        existing_signatures <- if (nrow(existing)) existing$Signature else character(0)
        selected_rows <- selected_rows[!vapply(selected_rows, function(index) structural_canvas_mi_signature(bundle$mi$lhs[[index]], bundle$mi$op[[index]], bundle$mi$rhs[[index]]) %in% existing_signatures, logical(1))]
        if (!length(selected_rows)) {
          showNotification("All selected MI paths have already been applied.", type = "warning")
          return()
        }
        pending_mi_rows(selected_rows)
        parameters <- vapply(selected_rows, function(index) paste(bundle$mi$lhs[[index]], bundle$mi$op[[index]], bundle$mi$rhs[[index]]), character(1))
        ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
        showModal(modalDialog(
          title = if (ko) "MI 수정 기록" else "Document MI modification",
          tags$p(paste0(if (ko) "추가할 경로: " else "Paths to add: ", paste(parameters, collapse = ", "))),
          textAreaInput(paste0(prefix, "_mi_justification"), if (ko) "실질적 근거" else "Substantive justification", rows = 4, placeholder = if (ko) "이 모수를 자유화하는 것이 이론적으로 방어 가능한 이유를 기록하십시오." else "Explain why freeing these parameters is theoretically defensible."),
          footer = tagList(modalButton(if (ko) "취소" else "Cancel"), actionButton(paste0(prefix, "_mi_confirm_apply"), if (ko) "적용 후 재분석" else "Apply and reanalyze", class = "btn-primary")),
          easyClose = TRUE
        ))
      }, ignoreInit = TRUE)
    }))
    observeEvent(input[[paste0(prefix, "_mi_confirm_apply")]], {
      bundle <- fit_result()
      selected_rows <- pending_mi_rows()
      shiny::req(!is.null(bundle), length(selected_rows))
      tryCatch({
        structural_canvas_validate_holdout_reuse(bundle$mi_holdout_enabled, !is.null(bundle$holdout_comparison))
        snapshot <- bundle$snapshot
        for (selected_row in selected_rows) snapshot <- structural_canvas_apply_mi(snapshot, bundle$mi[selected_row, , drop = FALSE])
        settings <- bundle
        settings$comparison_type <- "mi"
        settings$comparison_label <- "Modified model"
        settings$mi_history <- structural_canvas_mi_history_rows(bundle$mi, selected_rows, bundle$mi_history %||% data.frame(), input[[paste0(prefix, "_mi_justification")]] %||% "")
        removeModal()
        pending_mi_rows(integer(0))
        result <- execute_analysis(snapshot, settings)
        ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
        showNotification(if (ko) "선택한 MI 경로를 추가하고 기록한 뒤 재분석했습니다." else "The selected MI paths were added, documented, and reanalyzed.", type = if (isTRUE(result$converged)) "message" else "warning")
      }, error = function(error) showNotification(conditionMessage(error), type = "error", duration = 8))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_heywood_refit")]], {
      ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
      showModal(modalDialog(
        title = if (ko) "Heywood 제약 재분석" else "Heywood-constrained reanalysis",
        tags$p(if (ko) "각 음수 잔차분산을 해당 변수 관측분산의 작은 양수 비율로 고정합니다." else "Fix each negative residual variance to a small positive percentage of that variable's observed variance."),
        numericInput(paste0(prefix, "_heywood_percent"), if (ko) "관측분산 비율" else "Observed-variance percentage", value = 0.1, min = 0.01, max = 5, step = 0.01),
        tags$p(class = "structural-result-note", if (ko) "권장 시작값: 0.1%. 이는 민감도 분석이며 모형 부적합을 자동으로 수정하는 절차가 아닙니다." else "Recommended starting value: 0.1%. This is a sensitivity analysis, not an automatic correction of model misspecification."),
        footer = tagList(modalButton(if (ko) "취소" else "Cancel"), actionButton(paste0(prefix, "_heywood_confirm"), if (ko) "제약 모형 실행" else "Run constrained model", class = "btn-warning")),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0(prefix, "_heywood_confirm")]], {
      bundle <- fit_result()
      shiny::req(!is.null(bundle))
      tryCatch({
        if (!toupper(as.character(bundle$estimator %||% "ML")) %in% c("ML", "MLR") || length(bundle$ordered %||% character(0))) {
          stop("Heywood-constrained reanalysis is available only for continuous indicators estimated with ML or MLR.")
        }
        variables <- as.character((bundle$baseline_diagnostics %||% bundle$diagnostics)$negative_residuals %||% character(0))
        if (!length(variables)) stop("No negative residual variances were found in the original model.")
        percent <- as.numeric(input[[paste0(prefix, "_heywood_percent")]] %||% 0.1)
        if (!is.finite(percent) || percent < 0.01 || percent > 5) stop("Enter a percentage between 0.01 and 5.")
        data <- dataset_fn()
        observed_variances <- vapply(variables, function(name) stats::var(data[[name]], na.rm = TRUE), numeric(1))
        if (any(!is.finite(observed_variances) | observed_variances <= 0)) stop("A positive observed variance is required for every Heywood indicator.")
        fixes <- observed_variances * percent / 100
        names(fixes) <- variables
        settings <- bundle
        settings$residual_variance_fixes <- fixes
        settings$comparison_label <- "Heywood-constrained model"
        settings$comparison_type <- "heywood"
        removeModal()
        result <- execute_analysis(bundle$snapshot, settings)
        showNotification(paste0("The constrained model fixed ", paste(variables, collapse = ", "), " to ", format(percent, trim = TRUE), "% of observed variance."), type = if (isTRUE(result$admissible)) "message" else "warning", duration = 10)
      }, error = function(error) {
        showNotification(conditionMessage(error), type = "error", duration = 10)
      })
    }, ignoreInit = TRUE)
    observeEvent(input[[advanced_input]], {
      request <- input[[advanced_input]] %||% list()
      action <- as.character(request$action %||% "")
      candidates <- as.character(selected_names_fn() %||% character(0))
      ko <- identical(statedu_current_language(app_language_fn), "ko")
      if (identical(action, "multiGroup")) {
        showModal(modalDialog(
          title = if (ko) "다집단 분석 설정" else "Multigroup Analysis",
          selectInput(paste0(prefix, "_group_variable"), if (ko) "집단변수" else "Grouping variable", choices = candidates),
          helpText(if (ko) "집단 간 측정모형과 구조경로의 차이를 검정합니다." else "Compare measurement models and structural paths across groups."),
          footer = modalButton(if (ko) "닫기" else "Close"),
          easyClose = TRUE
        ))
      } else if (identical(action, "moderator")) {
        showModal(modalDialog(
          title = if (ko) "조절효과 설정" else "Moderation Settings",
          selectInput(paste0(prefix, "_moderator_variable"), if (ko) "조절변수" else "Moderator", choices = candidates),
          selectInput(paste0(prefix, "_moderated_predictor"), if (ko) "독립변수" else "Predictor", choices = candidates),
          selectInput(paste0(prefix, "_moderated_outcome"), if (ko) "종속변수" else "Outcome", choices = candidates),
          helpText(if (ko) "선택한 경로에 조절효과를 지정합니다." else "Assign a moderation effect to the selected path."),
          footer = modalButton(if (ko) "닫기" else "Close"),
          easyClose = TRUE
        ))
      }
    }, ignoreInit = TRUE)
    register_analysis_data_viewer_handlers(
      input = input,
      output = output,
      prefix = prefix,
      title = paste(structural_analysis_title(analysis_type, statedu_current_language(app_language_fn)), if (identical(statedu_current_language(app_language_fn), "ko")) "데이터 보기" else "Data Viewer"),
      dataset_fn = dataset_fn,
      selected_names_fn = selected_names_fn,
      variables_fn = local({
        state_input <- canvas_input
        function() custom_model_canvas_viewer_variables(input[[state_input]] %||% list())
      }),
      variable_table_fn = variable_table_fn,
      labels_fn = labels_fn,
      category_table_fn = category_table_fn,
      language_fn = app_language_fn
    )
  })
  invisible(TRUE)
}
