# Structural measurement invariance result rendering.

structural_canvas_structural_path_group_comparison_ui <- function(result, ko = FALSE) {
  table <- result$table %||% data.frame()
  group_table <- result$group_diagnostics %||% data.frame()
  path_estimates <- result$path_estimates %||% data.frame()
  path_differences <- result$path_differences %||% data.frame()
  if (nrow(group_table) && "Indicator missing %" %in% names(group_table)) {
    group_table[["Indicator missing %"]] <- paste0(vapply(group_table[["Indicator missing %"]], format_decimal3, character(1)), "%")
  }
  if (nrow(table)) {
    numeric_columns <- c("Chisq", "df", "CFI", "RMSEA", "SRMR", "DeltaCFI", "DeltaRMSEA", "DeltaSRMR", "DeltaChisq", "DeltaDf")
    for (name in intersect(numeric_columns, names(table))) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
    if ("p" %in% names(table)) table$p <- vapply(table$p, format_p, character(1))
    if ("DeltaP" %in% names(table)) table$DeltaP <- vapply(table$DeltaP, format_p, character(1))
    names(table)[names(table) == "DeltaCFI"] <- "ΔCFI"
    names(table)[names(table) == "DeltaRMSEA"] <- "ΔRMSEA"
    names(table)[names(table) == "DeltaSRMR"] <- "ΔSRMR"
    names(table)[names(table) == "DeltaChisq"] <- "Δχ²"
    names(table)[names(table) == "DeltaDf"] <- "Δdf"
    names(table)[names(table) == "DeltaP"] <- "Δp"
  }
  if (nrow(path_estimates)) {
    for (name in intersect(c("B", "SE", "z", "B 95% CI lower", "B 95% CI upper", "beta", "beta 95% CI lower", "beta 95% CI upper"), names(path_estimates))) {
      path_estimates[[name]] <- vapply(path_estimates[[name]], format_decimal3, character(1))
    }
    if ("p" %in% names(path_estimates)) path_estimates$p <- vapply(path_estimates$p, format_p, character(1))
  }
  if (nrow(path_differences)) {
    for (name in intersect(c("B difference", "SE", "z"), names(path_differences))) {
      path_differences[[name]] <- vapply(path_differences[[name]], format_decimal3, character(1))
    }
    for (name in intersect(c("p", "BH-adjusted p"), names(path_differences))) {
      path_differences[[name]] <- vapply(path_differences[[name]], format_p, character(1))
    }
  }
  measurement <- result$measurement_invariance %||% NULL
  gate <- result$measurement_gate %||% list(passed = FALSE, reason = "Measurement-invariance gate was not recorded.")
  div(class = "result-section regression-result-panel structural-invariance-result structural-path-group-result",
    h4(if (ko) paste0("구조경로 집단비교: ", result$group) else paste0("Structural path group comparison by ", result$group)),
    tags$h5(if (ko) "구조경로 비교 선행 측정불변성" else "Measurement-invariance gate before structural comparison"),
    if (!is.null(measurement) && nrow(measurement$table %||% data.frame())) structural_canvas_basic_html_table(measurement$table, class = "table table-striped table-bordered"),
    tags$p(class = "structural-result-note", if (ko) paste0("Metric gate: ", if (isTRUE(gate$passed)) "통과" else "실패", ". ", gate$reason %||% "") else paste0("Metric gate: ", if (isTRUE(gate$passed)) "passed" else "failed", ". ", gate$reason %||% "")),
    tags$h5(if (ko) "집단별 데이터 진단" else "Group-level data diagnostics"),
    structural_canvas_basic_html_table(group_table),
    tags$p(class = "structural-result-note", if (ko) "집단 크기와 결측률은 구조경로 차이 해석의 안정성을 판단하기 위한 기술 진단입니다." else "Group size and missingness diagnostics are descriptive checks for judging the stability of structural path comparisons."),
    tags$h5(if (ko) "자유 구조경로 모형과 동일 구조경로 모형 비교" else "Free versus equal structural-path models"),
    structural_canvas_basic_html_table(table),
    tags$p(class = "structural-result-note", if (ko) "동일 구조경로 모형은 집단 간 회귀경로(~)를 같게 제약합니다. Δχ²와 적합도 변화는 전체 구조경로 동일성의 omnibus 검정으로 해석하십시오." else "The equal structural-path model constrains regression paths (~) to equality across groups. Δχ² and fit-index changes are omnibus evidence for or against equality of the structural paths."),
    if (nrow(path_estimates)) tagList(
      tags$h5(if (ko) "집단별 구조경로 계수" else "Group-specific structural path estimates"),
      structural_canvas_basic_html_table(path_estimates)
    ),
    if (nrow(path_differences)) tagList(
      tags$h5(if (ko) "경로별 집단 간 차이" else "Pairwise group differences by path"),
      structural_canvas_basic_html_table(path_differences),
      tags$p(class = "structural-result-note", if (ko) "경로별 차이 z 검정은 집단별 추정치와 표준오차를 이용한 탐색적 pairwise 진단입니다. 최종 판단은 위의 동일 구조경로 모형 비교, 이론, 표본수와 함께 보고하십시오." else "Path-level z tests are exploratory pairwise diagnostics based on group-specific estimates and standard errors. Report final conclusions jointly with the equal-path model comparison, theory, and group sizes.")
    ) else tags$p(class = "structural-result-note", if (ko) "비교 가능한 구조경로가 충분하지 않아 경로별 집단 차이 표를 만들지 않았습니다." else "No path-level group-difference table was produced because comparable structural paths were unavailable.")
  )
}

structural_canvas_invariance_result_ui <- function(bundle, language = statedu_initial_language()) {
  result <- bundle$invariance_result %||% NULL
  if (is.null(result)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  if (identical(result$type %||% "", "structural_path_comparison")) {
    return(structural_canvas_structural_path_group_comparison_ui(result, ko))
  }
  if (identical(result$type %||% "", "pls_micom")) {
    gate <- result$measurement_gate %||% list(passed = FALSE, reason = "MICOM gate was not recorded.")
    table <- result$table %||% data.frame()
    mga <- result$mga_table %||% data.frame()
    for (name in intersect(c("Observed c", "5% permutation c", "Mean difference", "Variance difference"), names(table))) {
      table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
    }
    if (nrow(mga)) {
      if ("Path difference" %in% names(mga)) mga[["Path difference"]] <- vapply(mga[["Path difference"]], format_decimal3, character(1))
      for (name in intersect(c("Permutation p", "BH-adjusted p"), names(mga))) mga[[name]] <- vapply(mga[[name]], format_p, character(1))
    }
    return(div(class = "result-section regression-result-panel structural-invariance-result structural-micom-result",
      h4(if (ko) paste0("PLS MICOM 측정불변성: ", result$group) else paste0("PLS MICOM measurement invariance by ", result$group)),
      tags$p(class = "structural-result-note", if (ko) "1단계 구성불변성은 동일 지표, 자료처리, 알고리즘 설정으로 확보합니다. 2단계 합성점수 상관 permutation이 모든 구성개념에서 통과해야 최소 부분 측정불변성이 성립합니다. 평균·분산 동등성까지 통과하면 완전 측정불변성입니다." else "Step 1 configural invariance uses identical indicators, data treatment, and algorithm settings. Step 2 permutation tests of composite-score correlation must pass for every construct to establish at least partial measurement invariance. Equality of means and variances additionally establishes full measurement invariance."),
      structural_canvas_basic_html_table(table),
      tags$p(class = "structural-result-note", if (ko) paste0("MICOM gate: ", if (isTRUE(gate$passed)) "통과" else "실패", ". ", gate$reason, " 유효 permutation ", result$permutations_valid, "/", result$permutations_requested, ", seed=", result$seed, ".") else paste0("MICOM gate: ", if (isTRUE(gate$passed)) "passed" else "failed", ". ", gate$reason, " Valid permutations ", result$permutations_valid, "/", result$permutations_requested, ", seed=", result$seed, ".")),
      tags$h5(if (ko) "MICOM gate 이후 PLS-MGA 경로차이" else "PLS-MGA path differences after the MICOM gate"),
      if (nrow(mga)) structural_canvas_basic_html_table(mga, class = "table table-striped table-bordered structural-pls-mga-table") else tags$p(class = "structural-result-note", result$mga_status %||% if (ko) "MGA를 계산하지 않았습니다." else "MGA was not computed."),
      tags$p(class = "structural-result-note", if (ko) "경로차이 p값은 집단표지를 치환한 양측 permutation 검정이며 경로군 안에서 BH 보정합니다. MICOM 통과는 비교 가능성의 선행조건일 뿐 경로차이의 증거가 아니며, 유의한 차이도 이론·효과크기·집단 표본수와 함께 해석해야 합니다." else "Path-difference p values use a two-sided group-label permutation test and BH adjustment within the path family. Passing MICOM is a comparability prerequisite rather than evidence of path differences; significant differences must still be interpreted with theory, effect size, and group sample sizes.")
    ))
  }
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
    ifelse(table$Admissible & table$DeltaCFI >= -.010 & table$DeltaRMSEA <= .015 & table$DeltaSRMR <= srmr_limit, "Descriptive change guidance met", "Review descriptive change")
  ))
  reviewed_stages <- table$Model[table$Decision == "Review descriptive change"]
  score_tables <- (result$score_diagnostics %||% list())[intersect(reviewed_stages, names(result$score_diagnostics %||% list()))]
  score_tables <- Filter(function(value) nrow(value), score_tables)
  if (ko) {
    decision_labels <- c(
      "Inadmissible stage" = "허용 불가 단계",
      "Baseline stage" = "기준 단계",
      "Descriptive change guidance met" = "기술적 변화 기준 충족",
      "Review descriptive change" = "기술적 변화 검토"
    )
    table$Decision <- ifelse(table$Decision %in% names(decision_labels), decision_labels[table$Decision], table$Decision)
  }
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
    h4(if (ko) paste0("집단별 측정불변성: ", result$group) else paste0("Measurement invariance by ", result$group)),
    tags$h5(if (ko) "집단별 데이터 진단" else "Group-level data diagnostics"),
    structural_canvas_basic_html_table(group_table),
    tags$p(class = "structural-result-note", if (ko) "집단 N이 100 미만이면 기술적 소집단 경고로 표시합니다. 이는 보편적 최소 기준이 아니며, 적절성은 모형 복잡도, 지표 품질, 추정량, 결측, 범주 분포, 효과크기에 따라 달라집니다." else "Group N below 100 is flagged as a descriptive small-group warning, not a universal minimum. Adequacy depends on model complexity, indicator quality, estimator, missingness, category distribution, and effect size."),
    if (nrow(residual_summary)) tagList(
      tags$h5(if (ko) "집단별 형태모형 잔차 진단" else "Group-specific configural residual diagnostics"),
      structural_canvas_basic_html_table(residual_summary),
      if (nrow(residual_largest)) tagList(
        tags$h5(if (ko) paste0("큰 집단별 잔차(|z| >= ", group_residuals$cutoff %||% 1.96, ")") else paste0("Large group-specific residuals (|z| >= ", group_residuals$cutoff %||% 1.96, ")")),
        structural_canvas_basic_html_table(residual_largest)
      ) else tags$p(class = "structural-result-note", if (ko) paste0("|z| >= ", group_residuals$cutoff %||% 1.96, "을 초과한 집단별 형태모형 잔차가 없습니다.") else paste0("No group-specific configural residuals exceeded |z| >= ", group_residuals$cutoff %||% 1.96, ".")),
      tags$p(class = "structural-result-note", if (ko) "이 잔차 진단은 형태모형에서 집단별로 따로 계산합니다. WLSMV 순서형 지표에서는 적합된 잠재반응/polychoric 척도에서의 근사적 국소적합 진단입니다. 자동 수정 지시가 아니라 검토 후보 영역을 찾는 용도로 사용하십시오." else "These residual diagnostics are computed separately within each group from the configural model. For WLSMV ordered indicators they are approximate local-fit diagnostics on the fitted latent-response/polychoric scale; use them to locate candidate areas for review, not as automatic modification instructions.")
    ),
    if (nrow(group_reliability)) tagList(
      tags$h5(if (ko) "집단별 신뢰도 및 수렴타당도" else "Group-specific reliability and convergent validity"),
      structural_canvas_basic_html_table(group_reliability),
      tags$p(class = "structural-result-note", if (ko) "집단별 AVE, CR, alpha, omega는 동등성 제약을 부과하기 전 형태모형에서 계산합니다. 이는 공식 불변성 검정이 아니라 기술적 집단 진단으로 해석하십시오." else "Group-specific AVE, CR, alpha, and omega are computed from the configural model, before imposing equality constraints. Use them as descriptive group diagnostics, not as formal invariance tests.")
    ),
    if (nrow(group_htmt)) tagList(
      tags$h5(if (ko) "집단별 HTMT" else "Group-specific HTMT"),
      structural_canvas_basic_html_table(group_htmt),
      tags$p(class = "structural-result-note", if (ko) "집단별 HTMT는 각 집단의 형태모형 표본 상관행렬을 사용합니다. 이 릴리스에서 bootstrap 구간은 단일집단 기준으로 유지됩니다." else "Group-specific HTMT uses each group's configural-model sample correlation matrix. Bootstrap intervals remain single-group in this release.")
    ),
    structural_canvas_basic_html_table(table),
    if (any(!result$table$Admissible)) tags$p(class = "structural-result-note", if (ko) "허용 불가능한 불변성 단계는 주 CFA와 같은 전체 점검을 통과하지 못한 것입니다. 변화 지수, 공식 차이검정, 동등성 제약 score 진단은 숨깁니다. 불변성을 판단하기 전에 단계별 분산, 공분산행렬, 자유도, 잠재상관 문제를 먼저 해결하십시오." else "An inadmissible invariance stage failed the same full checks as the main CFA. Its change indices, formal difference test, and equality-constraint score diagnostics are suppressed; resolve the stage-specific variance, covariance-matrix, df, or latent-correlation problem before judging invariance."),
    tags$p(class = "structural-result-note", if (ko) "Parameter boundary dimensions는 모수 공분산행렬의 0에 가까운 고유값 수입니다. 명시적 동등성 제약 수 이내의 boundary dimension은 제약으로 인한 것으로 보고, 초과분은 설명되지 않은 경험적 과소식별로 표시합니다." else "Parameter boundary dimensions counts near-zero eigenvalues in the parameter covariance matrix. Boundary dimensions up to the number of explicit equality constraints are treated as constraint-induced; any excess is flagged as unexplained empirical underidentification."),
    tags$p(class = "structural-result-note", if (ko) "최소 고유값은 집단별 잔차 및 잠재 공분산 고유값 중 최악의 값과 모수 공분산 최소값을 보고합니다. 수치 허용오차를 넘는 음수는 비양정성, 0에 가까운 값은 경계 또는 특이 방향을 의미합니다." else "Minimum eigenvalues report the worst group-specific residual and latent covariance eigenvalues and the parameter covariance minimum. Negative values beyond numerical tolerance indicate non-positive-definiteness; values near zero indicate a boundary or singular direction."),
    if (any(result$table$`Ill-conditioned warning`)) tags$p(class = "structural-result-note", if (ko) "Ill-conditioned warning은 잔차, 잠재, 또는 모수 공분산 조건수가 1e8을 넘거나 무한대일 때 표시됩니다. 이는 수치 민감성을 뜻하지만 그 자체로 허용 불가능성을 입증하지는 않습니다." else "Ill-conditioned warning marks a residual, latent, or parameter covariance condition number above 1e8 (or an infinite value). This indicates numerical sensitivity but is not by itself proof of inadmissibility."),
    if (length(score_tables)) tagList(
      tags$h5(if (ko) "가장 큰 동등성 제약 score 검정" else "Largest equality-constraint score tests"),
      lapply(names(score_tables), function(stage) {
        score_table <- utils::head(score_tables[[stage]], 10L)
        score_table[["Score χ²"]] <- vapply(score_table[["Score χ²"]], format_decimal3, character(1))
        score_table[["Raw χ²"]] <- vapply(score_table[["Raw χ²"]], format_decimal3, character(1))
        score_table$p <- vapply(score_table$p, format_p, character(1))
        score_table[["BH-adjusted p"]] <- vapply(score_table[["BH-adjusted p"]], format_p, character(1))
        score_table[["Max |standardized EPC|"]] <- vapply(score_table[["Max |standardized EPC|"]], format_decimal3, character(1))
        score_table[["Raw p"]] <- vapply(score_table[["Raw p"]], format_p, character(1))
        score_table[["Raw BH-adjusted p"]] <- vapply(score_table[["Raw BH-adjusted p"]], format_p, character(1))
        tagList(tags$h6(stage), structural_canvas_basic_html_table(score_table))
      }),
      tags$p(class = "structural-result-note", if (ko) "Score 검정은 단계 부적합에 기여하는 동등성 제약의 순위를 보여줍니다. 집단 라벨은 관측된 집단변수 범주입니다. Max |standardized EPC|는 해당 동등성 제약에 포함된 모수 중 완전표준화 기대모수변화의 최대 절대값이며 효과크기 관점의 진단을 제공합니다. BH 보정 p값은 각 불변성 단계 안에서 false-discovery rate를 조절합니다. 이 결과는 탐색적 진단이며 부분불변성을 자동으로 확립하거나 제약 해제를 정당화하지 않습니다." else "Score tests rank equality constraints that contribute to stage misfit. Group labels are the observed grouping-variable categories. Max |standardized EPC| is the largest absolute fully standardized expected parameter change among the parameters in that equality constraint and provides an effect-size-oriented diagnostic. BH-adjusted p values control the false-discovery rate within each invariance stage. These remain exploratory diagnostics and do not automatically establish partial invariance or justify freeing a constraint.")
    ),
    tags$p(class = "structural-result-note", if (isTRUE(result$ordinal)) {
      if (ko) "순서형 지표에서는 중첩된 theta 파라미터화 단계가 thresholds, thresholds+loadings(scalar/strong invariance), residual variances(strict)를 차례로 제약합니다. 범주형 식별은 thresholds, loadings, scales, intercepts에 함께 의존하므로 별도의 전통적 metric-only 단계는 보고하지 않습니다." else "For ordered indicators, nested theta-parameterized stages constrain thresholds, then thresholds plus loadings (scalar/strong invariance), then residual variances (strict). A separate conventional metric-only stage is not reported because categorical identification depends jointly on thresholds, loadings, scales, and intercepts."
    } else {
      if (ko) "중첩 단계는 loadings(metric), intercepts(scalar), residual variances(strict)를 차례로 제약합니다. Δ 값은 각 행을 바로 이전 단계와 비교하며, 가능한 경우 robust/scaled 적합도 지수와 차이검정을 사용합니다." else "Nested stages constrain loadings (metric), then intercepts (scalar), then residual variances (strict). Δ values compare each row with the immediately preceding stage; robust/scaled fit indices and difference tests are used when available."
    }),
    if (isTRUE(result$ordinal)) tags$p(class = "structural-result-note", if (ko) "순서형 지표 모형은 WLSMV/DWLS, 범주 thresholds, theta 파라미터화를 사용합니다. 따라서 scalar invariance는 연속형 CFA의 관측변수 절편 동등성이 아니라 thresholds와 loadings의 불변성을 의미합니다." else "Ordered-indicator models use WLSMV/DWLS, category thresholds, and theta parameterization. Scalar invariance therefore means invariant thresholds and loadings, not equality of observed-variable intercepts as in continuous CFA."),
    tags$p(class = "structural-result-note", if (ko) "Chen(2007)에 근거한 기술적 변화 표시는 ΔCFI < -.010, ΔRMSEA > .015, 또는 ΔSRMR > .030(metric) 및 > .010(scalar/strict)을 사용합니다. Scalar/strict의 .010은 단계별 결과표의 보조 표시일 뿐 구조경로 집단비교 실행 gate에는 사용하지 않습니다. 구조경로 비교 gate는 metric 단계의 수렴·허용성과 ΔCFI ≥ -.010, ΔRMSEA ≤ .015, ΔSRMR ≤ .030만 사용합니다. 모든 변화 기준은 모수 변화, 집단 크기, 이론, 모형 허용성과 함께 판단해야 하며 Δχ²는 표본크기에 민감합니다." else "Descriptive change guidance based on Chen (2007) flags ΔCFI < −.010, ΔRMSEA > .015, or ΔSRMR > .030 for metric and > .010 for scalar/strict invariance. The .010 scalar/strict value is display-only stage guidance and is not part of the structural-path group-comparison gate. That gate uses only metric-stage convergence and admissibility with ΔCFI ≥ −.010, ΔRMSEA ≤ .015, and ΔSRMR ≤ .030. All change guidelines should be considered jointly with parameter changes, group sizes, theory, and model admissibility; Δχ² is sample-size sensitive."),
    tags$p(class = "structural-result-note", if (ko) "특정 단계의 실패가 모수 자동 해제를 정당화하지는 않습니다. 부분불변성은 실질적으로 방어 가능한 제약과 투명한 보고가 필요합니다." else "Failure at a stage does not justify automatically freeing parameters. Partial invariance requires substantively defensible constraints and transparent reporting.")
  )

}
