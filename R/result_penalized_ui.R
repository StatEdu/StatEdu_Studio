# Penalized regression result UI.

penalized_cv_plot_output_id <- function() "penalized_cv_curve_plot"

penalized_path_plot_output_id <- function() "penalized_coefficient_path_plot"

penalized_has_plot_data <- function(result) {
  is.list(result) &&
    is.data.frame(result$cv_curves) &&
    nrow(result$cv_curves) > 0 &&
    is.data.frame(result$coefficient_paths) &&
    nrow(result$coefficient_paths) > 0
}

penalized_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 13)
    )
}

plot_penalized_cv_curve <- function(result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Package 'ggplot2' is required for penalized regression plots.")
    return(invisible(NULL))
  }
  curve <- result$cv_curves
  if (!is.data.frame(curve) || nrow(curve) == 0) {
    plot.new()
    text(0.5, 0.5, "No cross-validation curve data.")
    return(invisible(NULL))
  }
  reference <- unique(curve[, c("Outcome", "Method", "lambda_min", "lambda_1se"), drop = FALSE])
  reference$log_lambda_min <- log(reference$lambda_min)
  reference$log_lambda_1se <- log(reference$lambda_1se)
  p <- ggplot2::ggplot(curve, ggplot2::aes(x = log_lambda, y = `CV MSE`, color = Method)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = `CV MSE` - `CV SE`, ymax = `CV MSE` + `CV SE`, fill = Method),
      alpha = 0.12,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(size = 1.1, alpha = 0.7) +
    ggplot2::geom_vline(
      data = reference,
      ggplot2::aes(xintercept = log_lambda_min, color = Method),
      linetype = "dashed",
      linewidth = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      data = reference,
      ggplot2::aes(xintercept = log_lambda_1se, color = Method),
      linetype = "dotted",
      linewidth = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::facet_wrap(stats::as.formula("~ Outcome"), scales = "free_y") +
    ggplot2::labs(
      title = "Cross-validation curve",
      x = "log(lambda)",
      y = "Cross-validated MSE",
      color = "Method",
      fill = "Method"
    ) +
    penalized_plot_theme()
  print(p)
}

plot_penalized_coefficient_path <- function(result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Package 'ggplot2' is required for penalized regression plots.")
    return(invisible(NULL))
  }
  path <- result$coefficient_paths
  if (!is.data.frame(path) || nrow(path) == 0) {
    plot.new()
    text(0.5, 0.5, "No coefficient path data.")
    return(invisible(NULL))
  }
  reference <- unique(path[, c("Outcome", "Method", "lambda_min", "lambda_1se"), drop = FALSE])
  reference$log_lambda_min <- log(reference$lambda_min)
  reference$log_lambda_1se <- log(reference$lambda_1se)
  p <- ggplot2::ggplot(path, ggplot2::aes(x = log_lambda, y = Coefficient, group = Predictor, color = Predictor)) +
    ggplot2::geom_hline(yintercept = 0, color = "#888888", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.85) +
    ggplot2::geom_vline(
      data = reference,
      ggplot2::aes(xintercept = log_lambda_min),
      linetype = "dashed",
      color = "#333333",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = reference,
      ggplot2::aes(xintercept = log_lambda_1se),
      linetype = "dotted",
      color = "#333333",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(Outcome ~ Method, scales = "free_y") +
    ggplot2::labs(
      title = "Coefficient path",
      x = "log(lambda)",
      y = "Coefficient",
      color = "Predictor"
    ) +
    penalized_plot_theme()
  print(p)
}

penalized_plot_block <- function(result) {
  if (!penalized_has_plot_data(result)) {
    return(NULL)
  }
  div(
    class = "penalized-plot-section",
    h4("Figure 1. Cross-validation curve"),
    div(
      class = "penalized-plot-card",
      plotOutput(result$cv_plot_id %||% penalized_cv_plot_output_id(), height = "460px")
    ),
    h4("Figure 2. Coefficient path"),
    div(
      class = "penalized-plot-card",
      plotOutput(result$path_plot_id %||% penalized_path_plot_output_id(), height = "520px")
    ),
    result_note_tag(
      "Dashed vertical lines indicate lambda.min; dotted vertical lines indicate lambda.1se.",
      class = "coefficient-note penalized-plot-note"
    )
  )
}

penalized_result_block <- function(result) {
  if (!is.list(result) || !is.data.frame(result$summary)) {
    return(NULL)
  }
  appendix_language <- result_appendix_table_language()
  appendix_text <- function(en, ko) statedu_localized_text(appendix_language, en, ko)
  localize_appendix_table <- function(table) {
    original_table <- table
    original_names <- names(table)
    table <- result_appendix_localize_table(table, appendix_language)
    selected_column <- match("Selected predictors", original_names)
    if (!is.na(selected_column)) {
      # Names may themselves be "None" or another translated application phrase.
      table[[selected_column]] <- original_table[[selected_column]]
      selection_status <- attr(original_table, "penalized_selection_status", exact = TRUE)
      if (is.character(selection_status) && length(selection_status) == nrow(table)) {
        for (status in c("none", "all")) {
          rows <- which(selection_status == status)
          key <- if (status == "none") "analysis.ui.none" else "analysis.ui.all_predictors_retained"
          table[[selected_column]][rows] <- statedu_t(key, appendix_language)
        }
      }
    }
    predictor_column <- match("Predictor", original_names)
    intercept_rows <- attr(original_table, "penalized_intercept_rows", exact = TRUE)
    if (!is.na(predictor_column) && is.numeric(intercept_rows)) {
      intercept_rows <- intercept_rows[is.finite(intercept_rows) & intercept_rows >= 1L & intercept_rows <= nrow(table) & intercept_rows == floor(intercept_rows)]
      intercept_rows <- intercept_rows[as.character(original_table[[predictor_column]][intercept_rows]) == "(Intercept)"]
      table[[predictor_column]][intercept_rows] <- statedu_t("analysis.penalized.intercept", appendix_language)
    }
    family_column <- match("Family", original_names)
    if (!is.na(family_column)) {
      gaussian <- which(as.character(original_table[[family_column]]) == "Gaussian")
      table[[family_column]][gaussian] <- statedu_t("analysis.penalized.family_gaussian", appendix_language)
    }
    percent_column <- match("Selection frequency (%)", original_names)
    if (!is.na(percent_column)) names(table)[[percent_column]] <- statedu_t("analysis.penalized.selection_frequency_percent", appendix_language)
    status_column <- match("Status", original_names)
    if (!is.na(status_column)) {
      status_keys <- c("Tested"="tested", "No variables selected"="no_variables_selected",
        "Tuning failed"="tuning_failed", "Insufficient residual degrees of freedom or rank deficiency"="insufficient_residual_degrees_of_freedom_or_rank_deficiency",
        "Non-positive residual variance"="non_positive_residual_variance", "Non-finite test statistics"="non_finite_test_statistics")
      raw <- as.character(original_table[[status_column]])
      known <- which(raw %in% names(status_keys))
      table[[status_column]][known] <- vapply(raw[known],function(value) statedu_t(paste0("analysis.ui.",status_keys[[value]]),appendix_language),character(1))
    }
    if (!identical(appendix_language, "ko") || !is.data.frame(table)) return(table)
    labels <- c(
      "Outcome" = "결과변수", "Method" = "방법", "Family" = "분포",
      "Predictor" = "예측변수", "Coefficient" = "계수", "Selected" = "선택 여부",
      "Selected predictors" = "선택된 예측변수", "Selected predictors, n" = "선택 예측변수 수",
      "alpha" = "알파", "Alpha" = "알파", "Alpha searched" = "탐색한 알파",
      "Selected alpha" = "선택된 알파", "lambda" = "람다", "lambda rule" = "람다 규칙",
      "Lambda rules" = "람다 규칙", "CV folds" = "교차검증 폴드",
      "CV MSE" = "교차검증 MSE", "CV SE" = "교차검증 표준오차",
      "CV RMSE" = "교차검증 RMSE", "CV MAE" = "교차검증 MAE", "CV R²" = "교차검증 R²",
      "Apparent RMSE" = "표본내 RMSE", "Apparent MAE" = "표본내 MAE", "Apparent R²" = "표본내 R²",
      "N complete" = "완전 사례 수", "Rows removed" = "제외 행 수", "Model matrix p" = "모형행렬 변수 수",
      "Predictor standardization" = "예측변수 표준화",
      "Selection bootstrap resamples" = "선택 안정성 부트스트랩 반복수",
      "Selection frequency" = "선택 빈도", "Selection frequency (%)" = "선택 빈도 (%)",
      "Successful resamples" = "성공 반복수", "Selected in full model" = "전체 모형 선택 여부",
      "Metric" = "성능 지표", "Mean" = "평균", "SD" = "표준편차", "Minimum" = "최솟값", "Maximum" = "최댓값", "Repetitions" = "반복수",
      "Stability" = "안정성", "Status" = "처리 상태", "Splits" = "분할 수", "Random seed" = "난수 시드", "Outer CV folds" = "외부 교차검증 폴드"
    )
    names(table) <- vapply(seq_along(original_names), function(i) {
      column<-original_names[[i]]
      if (column %in% names(labels)) unname(labels[[column]]) else names(table)[[i]]
    }, character(1))
    if("처리 상태" %in% names(table)) {
      statuses<-c("Tested"="검정 완료","No variables selected"="선택된 변수 없음",
        "Tuning failed"="튜닝 실패","Insufficient residual degrees of freedom or rank deficiency"="잔차 자유도 부족 또는 완전 공선성",
        "Non-positive residual variance"="잔차 분산이 0이거나 유효하지 않음","Non-finite test statistics"="검정 통계량 산출 불가")
      table[["처리 상태"]]<-vapply(table[["처리 상태"]],function(x)if(x%in%names(statuses))unname(statuses[[x]])else x,character(1))
    }
    table
  }
  render_simple_table <- function(table, class_name, note = NULL, role = "main") {
    if (!is.data.frame(table) || nrow(table) == 0) {
      return(NULL)
    }
    role <- result_table_role(role)
    language <- result_table_language(role, if (identical(role, "appendix")) appendix_language else "en")
    if (identical(role, "appendix")) table <- localize_appendix_table(table)
    table_tag <- tags$table(
      class = paste("table shiny-table penalized-journal-table", class_name),
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
        tags$tr(lapply(table[row_index, , drop = TRUE], tags$td))
      }))
    )
    contract <- result_table_contract(
      table,
      role = role,
      language = language,
      intrinsic_width = result_table_intrinsic_width(table, first_width = 132, default_width = 76, min_width = 480)
    )
    result_table_with_notes(
      result_table_apply_contract(table_tag, contract),
      result_note_tag(note, class = "coefficient-note penalized-table-note")
    )
  }
  div(
    class = "regression-result-panel penalized-result-panel",
    h3(result$display_title %||% "Penalized Regression"),
    h4("Table 1. Penalized regression performance"),
    render_simple_table(
      result$publication_summary,
      "penalized-publication-summary-table",
      result_sci_note_text(
        abbreviations = "CV = cross-validation; RMSE = root mean square error; MAE = mean absolute error; alpha = penalty mixing parameter; lambda = penalty strength",
        estimation = if(isTRUE(result$nested_validation)) "Each repetition uses pooled held-out predictions from outer 5-fold nested CV; reported performance is the arithmetic mean of repetition-specific metrics. Alpha (Elastic Net), lambda and predictor standardization are fitted within training data. Alpha and lambda shown are final full-data tuning values. Repetition variability describes sensitivity to data splitting, not a confidence interval; see Appendix A8 when repetitions exceed one" else "Estimates use the parsimonious lambda.1se solution and tuning-CV predictions; these are not independent validation estimates"
      )
    ),
    h4(if(is.data.frame(result$publication_coefficients)) "Table 2. Penalized regression coefficients" else "Table 2. Predictors retained in the penalized model"),
    render_simple_table(
      result$publication_coefficients %||% result$publication_selected_predictors,
      "penalized-publication-selected-table",
      result_sci_note_text(abbreviations="B = unstandardized penalized regression coefficient",estimation = "Full-data lambda.1se coefficients are returned on the original predictor and outcome scales. Selection uses unrounded coefficients and is not a significance test. Ridge retains all predictors; the intercept is not subject to selection")
    ),
    if(is.data.frame(result$publication_stability)&&nrow(result$publication_stability))tagList(h4("Table 3. Bootstrap selection stability"),
    render_simple_table(
      result$publication_stability,
      "penalized-publication-stability-table",
      result_sci_note_text(
        abbreviations = "alpha = penalty mixing parameter",
        estimation = "Selection frequency is the proportion of successful bootstrap fits retaining a predictor at lambda.1se. Elastic Net re-tunes alpha over the configured grid and lambda within each resample, using identical CV folds across alpha candidates. Displayed alpha is the full-data estimate. A failed alpha candidate makes that resample unsuccessful. This selection stability measure is not a p-value or evidence of statistical significance",
        symbol = "Ridge is omitted because it does not perform variable selection"
      )
    )),
    if(is.data.frame(result$publication_inference))tagList(
      h4("Table 4. Post-selection inference: repeated sample splitting"),
      render_simple_table(result$publication_inference,"penalized-inference-table",
        result_sci_note_text(abbreviations="p = two-sided multi-split p-value adjusted across candidate model-matrix terms within each outcome and method",
          estimation="Each 50:50 split uses training-only tuning and lambda.1se screening, followed by independent OLS t-tests. Split p-values are multiplied by the number of selected terms; unselected or failed tests are assigned 1. The aggregate is twice the empirical median (fixed gamma = .5), capped at 1. No additional BH correction is applied",
          reference="Validity requires independent observations, a correctly specified linear model with Gaussian homoskedastic errors, and screening that retains the true active terms. These assumptions are not guaranteed by cross-validation. Categorical levels are tested as individual contrasts, not as an omnibus factor test; no aggregate coefficient or confidence interval is claimed",
          symbol="An em dash means no independent test was estimable for that term; it is not evidence of no effect. Internally, the prescribed p=1 contributions remain in aggregation. Status describes test coverage and selection variation, not inferential validity. Fewer than half tested refers to the fixed gamma=.5 aggregation threshold"))),
    if(is.data.frame(result$publication_factor_inference))tagList(
      h4("Table 5. Categorical variables: omnibus post-selection tests"),
      render_simple_table(result$publication_factor_inference,"penalized-factor-inference-table",
        result_sci_note_text(abbreviations="p = multi-split omnibus p-value; contrasts = number of model-matrix columns for the categorical variable",
          estimation="Within each split, selection of any contrast includes all contrasts of that variable in a separate test model. Independent partial F-tests compare this model with the same model excluding that entire variable. Split p-values are multiplied by the number of selected categorical variables; unselected or failed tests contribute 1. The aggregate is twice the fixed empirical .5 quantile, capped at 1",
          reference="The testing family comprises categorical variables within each outcome and method, separately from Table 4. This is not joint error control over both tables. The same linear-model, independent-error and screening assumptions apply. Only additive categorical terms are supported; no aggregate F statistic or confidence interval is claimed",
          symbol="An em dash indicates zero estimable tests. Missing categories or insufficient residual degrees of freedom can prevent full-factor testing. Status describes coverage, not inferential validity"))),
    h4(appendix_text("Appendix Table A1. Cross-validated tuning and model performance", "부록표 A1. 교차검증 튜닝 및 모형 성능")),
    render_simple_table(
      result$summary,
      "penalized-summary-table",
      appendix_text(
        "lambda.min minimizes tuning-CV MSE; lambda.1se selects the more parsimonious solution within one standard error. These tuning-CV and apparent indices are not independent validation performance; see Table 1 for nested CV.",
        "lambda.min은 튜닝 교차검증 MSE가 최소인 값이며, lambda.1se는 최소값의 1 표준오차 범위에서 더 간명한 해를 선택합니다. 튜닝 교차검증 및 표본내 지수는 독립적인 검증 성능이 아닙니다. 중첩 교차검증은 본표 1을 참조하세요."
      ),
      role = "appendix"
    ),
    h4(appendix_text("Appendix Table A2. OLS and penalized regression coefficients", "부록표 A2. OLS 및 규제 회귀 계수")),
    render_simple_table(
      result$coefficient_comparison,
      "penalized-coefficient-comparison-table",
      appendix_text(
        "Penalized coefficients use internal predictor standardization and are returned on the original response scale. Zero denotes exclusion by LASSO or Elastic Net.",
        "규제 적용 계수는 내부적으로 예측변수를 표준화해 추정한 뒤 원래 결과변수 척도로 제시합니다. LASSO 또는 Elastic Net의 0은 해당 예측변수가 선택되지 않았음을 뜻합니다."
      ),
      role = "appendix"
    ),
    h4(appendix_text("Appendix Table A3. Predictors retained by penalized regression", "부록표 A3. 규제 회귀에서 유지된 예측변수")),
    render_simple_table(
      result$selected_predictors,
      "penalized-selected-table",
      appendix_text(
        "Ridge regression retains all predictors because it does not perform variable selection.",
        "Ridge 회귀는 변수선택을 하지 않으므로 모든 예측변수를 유지합니다."
      ),
      role = "appendix"
    ),
    h4(appendix_text("Appendix Table A4. Cross-validation settings", "부록표 A4. 교차검증 설정")),
    render_simple_table(
      result$cv_settings,
      "penalized-settings-table",
      appendix_text(
        "Elastic Net selects alpha by the lowest cross-validated MSE. Bootstrap stability re-tunes alpha over the configured grid and lambda in each resample; identical CV folds are used across alpha candidates.",
        "Elastic Net은 교차검증 MSE가 가장 작은 알파를 선택합니다. 선택 안정성 부트스트랩에서는 각 재표본마다 설정된 탐색 범위에서 알파와 람다를 다시 선택하며, 알파 후보 간 동일한 교차검증 폴드를 사용합니다."
      ),
      role = "appendix"
    ),
    if(is.data.frame(result$selection_stability)&&nrow(result$selection_stability))tagList(h4(appendix_text("Appendix Table A5. Bootstrap selection stability", "부록표 A5. 부트스트랩 선택 안정성")),
    render_simple_table(
      result$selection_stability,
      "penalized-selection-stability-table",
      appendix_text(
        "Selection frequency is the proportion of successful resamples retaining each predictor. Elastic Net re-tunes alpha and lambda in each resample; displayed alpha is the full-data estimate. A failed alpha candidate makes the resample unsuccessful. Ridge is omitted; selection frequency is not a significance test.",
        "선택 빈도는 성공한 재표본 중 각 예측변수가 유지된 비율입니다. Elastic Net은 각 재표본에서 알파와 람다를 다시 선택하며, 표의 알파는 전체 표본 추정값입니다. 알파 후보 하나라도 추정에 실패하면 해당 재표본을 실패로 처리합니다. Ridge는 제외하며 선택 빈도는 유의성 검정이 아닙니다."
      ),
      role = "appendix"
    )),
    if(is.data.frame(result$inference_diagnostics))tagList(
      h4(appendix_text("Appendix Table A6. Sample-split diagnostics","부록표 A6. 표본 분할 검정 진단")),
      render_simple_table(result$inference_diagnostics,"penalized-inference-diagnostics",
        appendix_text("All requested splits remain in the aggregation denominator. Failed or non-estimable splits are not discarded or replaced.","요청한 모든 분할을 통합 분모에 포함합니다. 실패하거나 추정할 수 없는 분할은 제거하거나 다시 뽑지 않습니다."),role="appendix")),
    if(is.data.frame(result$factor_diagnostics))tagList(
      h4(appendix_text("Appendix Table A7. Omnibus test diagnostics","부록표 A7. 범주형 변수 전체 검정 진단")),
      render_simple_table(result$factor_diagnostics,"penalized-factor-diagnostics",
        appendix_text("Omnibus tests expand selected categorical terms to all contrasts. Failures remain in the requested split denominator; this test model can differ from the individual-contrast model in Table 4.","전체 검정은 선택된 범주형 변수의 모든 대비를 포함합니다. 실패 분할도 요청한 분할 수의 분모에 남깁니다. 검정 모형은 본표 4의 개별 대비 검정 모형과 다를 수 있습니다."),role="appendix")),
    if(is.data.frame(result$validation_variability))tagList(
      h4(appendix_text("Appendix Table A8. Repeated nested CV variability","부록표 A8. 반복 중첩 교차검증 변동성")),
      render_simple_table(result$validation_variability,"penalized-validation-variability",
        appendix_text("SD = sample standard deviation across repetitions. Mean, SD, minimum and maximum summarize repetition-specific pooled held-out metrics, not folds. Repetitions reuse the same observations and are not independent samples; the range is not a confidence interval. Failed repetitions stop the analysis and are not discarded. Repetition seeds start at the analysis seed + 30000 and increase by 100.",
          "SD는 반복 간 표본 표준편차입니다. 평균·표준편차·최솟값·최댓값은 폴드별 값이 아닌, 각 반복의 전체 검증 예측으로 계산한 성능을 요약합니다. 반복은 같은 관측치를 재사용하므로 독립 표본이 아니며 범위는 신뢰구간이 아닙니다. 반복 실패 시 분석을 중단하며 실패 반복을 제거하지 않습니다. 반복 시드는 분석 시드 + 30000에서 시작해 100씩 증가합니다."),role="appendix")),
    penalized_plot_block(result)
  )
}
