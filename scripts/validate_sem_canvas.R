source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

if (!requireNamespace("lavaan", quietly = TRUE)) {
  stop("lavaan is required for CB-SEM validation.")
}
if (!requireNamespace("seminr", quietly = TRUE)) {
  stop("seminr is required for PLS-SEM validation.")
}

set.seed(20260813)
n <- 180L
eta1 <- stats::rnorm(n)
eta2 <- 0.62 * eta1 + stats::rnorm(n, sd = 0.78)
data <- data.frame(
  x1 = 0.78 * eta1 + stats::rnorm(n, sd = 0.45),
  x2 = 0.72 * eta1 + stats::rnorm(n, sd = 0.50),
  x3 = 0.69 * eta1 + stats::rnorm(n, sd = 0.52),
  y1 = 0.82 * eta2 + stats::rnorm(n, sd = 0.42),
  y2 = 0.76 * eta2 + stats::rnorm(n, sd = 0.48),
  y3 = 0.70 * eta2 + stats::rnorm(n, sd = 0.55)
)

snapshot <- list(
  nodes = list(
    list(id = "lv1", role = "latent", name = "eta1", canvasLabel = "eta1", x = 120, y = 120, measurementMode = "reflective"),
    list(id = "lv2", role = "latent", name = "eta2", canvasLabel = "eta2", x = 420, y = 120, measurementMode = "reflective"),
    list(id = "x1", role = "indicator", name = "x1", variableId = "x1", canvasLabel = "x1", x = 120, y = 260),
    list(id = "x2", role = "indicator", name = "x2", variableId = "x2", canvasLabel = "x2", x = 120, y = 340),
    list(id = "x3", role = "indicator", name = "x3", variableId = "x3", canvasLabel = "x3", x = 120, y = 420),
    list(id = "y1", role = "indicator", name = "y1", variableId = "y1", canvasLabel = "y1", x = 420, y = 260),
    list(id = "y2", role = "indicator", name = "y2", variableId = "y2", canvasLabel = "y2", x = 420, y = 340),
    list(id = "y3", role = "indicator", name = "y3", variableId = "y3", canvasLabel = "y3", x = 420, y = 420)
  ),
  edges = list(
    list(id = "e1", from = "lv1", to = "x1"),
    list(id = "e2", from = "lv1", to = "x2"),
    list(id = "e3", from = "lv1", to = "x3"),
    list(id = "e4", from = "lv2", to = "y1"),
    list(id = "e5", from = "lv2", to = "y2"),
    list(id = "e6", from = "lv2", to = "y3"),
    list(id = "p1", from = "lv1", to = "lv2")
  )
)

labels_fn <- function() character(0)
language_fn <- function() "en"
ko_language_fn <- function() "ko"
variable_table <- data.frame(name = names(data), measurement = "scale", stringsAsFactors = FALSE)
notification_source <- readLines(file.path("R", "setup_custom_model_canvas_structural_execute_notifications.R"), warn = FALSE, encoding = "UTF-8")
pls_engine_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_pls_engine.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
handler_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_handlers.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
fit_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("PLS structural model effects", ui_source, fixed = TRUE),
  grepl("2. PLS/PLSc model fit diagnostics", ui_source, fixed = TRUE),
  grepl("3. PLS structural model effects", ui_source, fixed = TRUE),
  grepl("total and indirect effects", ui_source, fixed = TRUE),
  grepl("PLSpredict cross-validation", ui_source, fixed = TRUE),
  grepl('"AUTO", "PLS", "PLSC"', ui_source, fixed = TRUE),
  grepl("Rule-based recommendation (confirmation required)", ui_source, fixed = TRUE),
  grepl("estimator_recommendation_confirmed", ui_source, fixed = TRUE),
  grepl('"5,000 resamples" = "5000"', ui_source, fixed = TRUE),
  grepl("PLSpredict predictive assessment", ui_source, fixed = TRUE),
  grepl("PLS-SEM quality checklist", ui_source, fixed = TRUE),
  grepl("repeated PLSpredict boundary conditions", ui_source, fixed = TRUE),
  grepl("approximate reflective-model fit diagnostics", ui_source, fixed = TRUE),
  grepl("SEM quality checklist", ui_source, fixed = TRUE),
  grepl("sample adequacy, common-method-bias screens, chi-square/df", ui_source, fixed = TRUE),
  grepl("Reporting checklist", ui_source, fixed = TRUE),
  grepl("Estimator or algorithm", ui_source, fixed = TRUE),
  grepl("Admissibility and convergence", ui_source, fixed = TRUE),
  grepl("PLS lower % is the proportion of repetitions favoring PLS", ui_source, fixed = TRUE),
  grepl("main table combines direct path estimates, bootstrap inference", ui_source, fixed = TRUE),
  grepl("형성형 합성변수에는 내적일관성·AVE·HTMT를 적용하지 않습니다", ui_source, fixed = TRUE),
  grepl("Indirect and total effects, confidence intervals, effect-size labels", ui_source, fixed = TRUE),
  grepl("loading/weight reports the outer loading for reflective indicators and the outer weight for formative indicators", ui_source, fixed = TRUE),
  grepl("Supplementary Table 3: Structural effect guide indices", fit_source, fixed = TRUE),
  grepl("Supplementary Table 3: PLS bootstrap path inference", fit_source, fixed = TRUE),
  grepl("PLS measurement diagnostics", ui_source, fixed = TRUE),
  grepl("PLS measurement bootstrap", ui_source, fixed = TRUE),
  grepl("HTMT and Fornell-Larcker are computed only between reflective constructs", ui_source, fixed = TRUE),
  grepl("Indirect and total effects are reported separately", ui_source, fixed = TRUE),
  grepl("Table 6. Specific indirect effects", ui_source, fixed = TRUE),
  grepl("Supplementary Table 3: Effect beta 95% confidence intervals", ui_source, fixed = TRUE),
  grepl("표 3. 구조모형 경로", ui_source, fixed = TRUE),
  grepl("Data-driven indicator deletion must not be presented", ui_source, fixed = TRUE),
  grepl('"Direct CI source", "Indirect beta 95% CI", "Indirect CI source"', ui_source, fixed = TRUE),
  grepl("PLS-SEM does not estimate covariance paths", ui_source, fixed = TRUE),
  grepl("PLS-SEM does not estimate covariance paths; excluded:", ui_source, fixed = TRUE),
  grepl('analysis_type %in% c("cfa", "cbsem", "sem")) downloadButton(paste0(prefix, "_download_reproducibility")', ui_source, fixed = TRUE),
  grepl('analysis_type %in% c("cfa", "cbsem", "sem")) downloadButton(paste0(prefix, "_download_tables")', ui_source, fixed = TRUE),
  grepl("PLS-SEM은 공분산 경로를 추정하지", ui_source, fixed = TRUE),
  grepl("외생 잠재변수 사이의 공분산 경로가 없습니다", ui_source, fixed = TRUE),
  grepl("structural_canvas_show_notification <- function", ui_source, fixed = TRUE),
  grepl("Estimating PLS-SEM bootstrap intervals", pls_engine_source, fixed = TRUE),
  grepl("seminr bootstrap resamples", pls_engine_source, fixed = TRUE),
  grepl("seminr::PLSc", pls_engine_source, fixed = TRUE),
  grepl("parallel::makePSOCKcluster", pls_engine_source, fixed = TRUE),
  grepl("setTimeLimit(cpu = Inf, elapsed = 60", pls_engine_source, fixed = TRUE),
  grepl("align_bootstrap_signs", pls_engine_source, fixed = TRUE),
  grepl("current resample batch is slow; ETA paused", handler_source, fixed = TRUE),
  grepl("PLS-SEM bootstrap complete", pls_engine_source, fixed = TRUE),
  grepl("Estimating PLSpredict cross-validation", pls_engine_source, fixed = TRUE),
  grepl("seminr::predict_pls", pls_engine_source, fixed = TRUE),
  !grepl("structural_canvas_pls_predictive_relevance", pls_engine_source, fixed = TRUE),
  !grepl("q2 = 1 - press / tss", pls_engine_source, fixed = TRUE),
  grepl("structural_canvas_notify_missing_covariances(missing_covariances, analysis_type, statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  grepl("structural_canvas_notify_ignored_pls_covariances(result, analysis_type, statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  grepl("structural_canvas_notify_solution_diagnostics(result, statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  any(grepl("잠재적으로 허용 불가능한 해", notification_source, fixed = TRUE)),
  any(grepl("수치적으로 불안정한 해", notification_source, fixed = TRUE)),
  grepl("PLS-SEM은 공분산 경로를 추정하지", ui_source, fixed = TRUE),
  sum(grepl("showNotification(", notification_source, fixed = TRUE)) == 1L,
  grepl('analysis_type %in% c("cfa", "cbsem", "sem")) output[[paste0(prefix, "_download_reproducibility")]]', handler_source, fixed = TRUE),
  grepl('analysis_type %in% c("cfa", "cbsem", "sem")) output[[paste0(prefix, "_download_tables")]]', handler_source, fixed = TRUE),
  !grepl("Latent covariance, factor-score, HTMT, and lavaan delta-method diagnostics are not displayed", ui_source, fixed = TRUE)
)

cbsem <- run_structural_canvas_analysis(snapshot, data, "cbsem", estimator = "ML", missing = "fiml")
stopifnot(inherits(cbsem$fit, "lavaan"))
stopifnot(isTRUE(cbsem$converged))
stopifnot(grepl("eta2 ~", cbsem$syntax, fixed = TRUE), grepl("*eta1", cbsem$syntax, fixed = TRUE))
stopifnot(is.finite(cbsem$df))

cbsem_bundle <- list(
  fit = cbsem$fit,
  syntax = cbsem$syntax,
  snapshot = snapshot,
  diagnostics = cbsem,
  estimator = "ML",
  missing = "fiml",
  rmsea_ci = 0.90,
  validity_formula = "standardized"
)
cbsem_result <- function() cbsem_bundle
cbsem_reporting <- structural_canvas_reporting_context_rows(cbsem_bundle, "cbsem")
cbsem_construct_reporting <- structural_canvas_construct_reporting_rows(cbsem_bundle, "cbsem", FALSE)
stopifnot(nrow(cbsem_reporting) == 16L)
stopifnot(
  nrow(cbsem_construct_reporting) == 2L,
  all(c("Declared type", "Effective weighting", "Engine representation", "Estimand", "Migration") %in% names(cbsem_construct_reporting)),
  all(cbsem_construct_reporting$`Declared type` == "commonFactor"),
  all(cbsem_construct_reporting$`Effective weighting` == "Not applicable")
)
stopifnot(grepl("lavaan", cbsem_reporting$Value[cbsem_reporting$Item == "Analysis engine"], fixed = TRUE))
stopifnot(cbsem_reporting$Value[cbsem_reporting$Item == "Estimator or algorithm"] == "ML")
stopifnot(cbsem_reporting$Value[cbsem_reporting$Item == "Missing-data handling"] == "fiml")
stopifnot(cbsem_reporting$Value[cbsem_reporting$Item == "Analysis context"] == "Original/prespecified model")
stopifnot(cbsem_reporting$Value[cbsem_reporting$Item == "Common method diagnostics"] == "Not enabled")
stopifnot(grepl("converged=TRUE", cbsem_reporting$Value[cbsem_reporting$Item == "Admissibility and convergence"], fixed = TRUE))
cbsem_reporting_ko <- structural_canvas_reporting_context_display_rows(cbsem_reporting, TRUE)
stopifnot(
  identical(names(cbsem_reporting_ko), c("항목", "값")),
  "분석 맥락" %in% cbsem_reporting_ko$항목,
  "연구모형" %in% cbsem_reporting_ko$값,
  grepl("보고 체크리스트", paste(as.character(structural_canvas_reporting_context_result_ui(cbsem_bundle, "cbsem", "ko")), collapse = "\n"), fixed = TRUE)
)
cbsem_quality <- structural_canvas_lavaan_quality_rows(cbsem_bundle, "cbsem")
stopifnot(nrow(cbsem_quality) == 20L)
stopifnot(all(c("Item", "Value", "Status", "Guidance") %in% names(cbsem_quality)))
stopifnot(all(c("Converged", "Admissible solution", "Model df", "Chi-square/df", "Fit statistic source", "N/free parameter ratio", "Harman first-factor %", "Max full collinearity VIF", "CFI", "RMSEA", "SRMR", "Min standardized loading", "Min CR", "Min AVE", "Max latent correlation", "Structural path count", "Max structural beta", "Min endogenous R2", "Model status") %in% cbsem_quality$Item))
stopifnot(cbsem_quality$Value[cbsem_quality$Item == "Converged"] == "TRUE")
stopifnot(cbsem_quality$Value[cbsem_quality$Item == "Admissible solution"] == "TRUE")
stopifnot(cbsem_quality$Status[cbsem_quality$Item == "Converged"] == "OK")
stopifnot(cbsem_quality$Status[cbsem_quality$Item == "Admissible solution"] == "OK")
stopifnot(nzchar(cbsem_quality$Value[cbsem_quality$Item == "Chi-square/df"]))
stopifnot(cbsem_quality$Status[cbsem_quality$Item == "Fit statistic source"] == "OK")
stopifnot(nzchar(cbsem_quality$Value[cbsem_quality$Item == "N/free parameter ratio"]))
stopifnot(cbsem_quality$Status[cbsem_quality$Item == "N/free parameter ratio"] == "Reference only")
stopifnot(all(cbsem_quality$Status[cbsem_quality$Item %in% c("CFI", "TLI", "RMSEA", "SRMR")] %in% c("Reference only", "Review")))
stopifnot(nzchar(cbsem_quality$Value[cbsem_quality$Item == "Harman first-factor %"]))
stopifnot(nzchar(cbsem_quality$Value[cbsem_quality$Item == "Max full collinearity VIF"]))
stopifnot(all(cbsem_quality$Status[cbsem_quality$Item %in% c("Harman first-factor %", "Max full collinearity VIF")] == "Screen only"))
stopifnot(all(cbsem_quality$Status[cbsem_quality$Item %in% c("Min standardized loading", "Min CR", "Min AVE")] %in% c("Reference only", "Review")))
stopifnot(cbsem_quality$Status[cbsem_quality$Item == "Max latent correlation"] %in% c("Reference only", "Review"))
stopifnot(cbsem_quality$Status[cbsem_quality$Item == "Min endogenous R2"] == "Descriptive only")
stopifnot(cbsem_quality$Value[cbsem_quality$Item == "Structural path count"] == "1")
stopifnot(cbsem_quality$Status[cbsem_quality$Item == "Structural path count"] == "OK")
stopifnot(cbsem_quality$Value[cbsem_quality$Item == "Model status"] == "Original/prespecified model")
stopifnot(cbsem_quality$Status[cbsem_quality$Item == "Model status"] == "OK")
cbsem_quality_summary <- structural_canvas_lavaan_quality_status_summary(cbsem_quality)
stopifnot(grepl("Quality status: OK=", cbsem_quality_summary, fixed = TRUE))
stopifnot(grepl("; Review=", cbsem_quality_summary, fixed = TRUE))
stopifnot(grepl("; Reference only=", cbsem_quality_summary, fixed = TRUE))
cbsem_quality_review <- structural_canvas_lavaan_quality_review_rows(cbsem_quality)
stopifnot(all(c("Priority", "Action", "Item", "Value", "Guidance") %in% names(cbsem_quality_review)))
stopifnot(nrow(cbsem_quality_review) == sum(cbsem_quality$Status == "Review"))
stopifnot(all(cbsem_quality_review$Priority %in% c("Critical", "Major", "Advisory")))
stopifnot(all(cbsem_quality_review$Action %in% c("Resolve before reporting", "Resolve or justify", "Document limitation")))
cbsem_readiness <- structural_canvas_lavaan_quality_reporting_readiness(cbsem_quality)
stopifnot(grepl("Reporting readiness:", cbsem_readiness, fixed = TRUE))
cbsem_quality_ko <- structural_canvas_quality_display_rows(cbsem_quality, TRUE)
cbsem_quality_ui_ko <- paste(as.character(structural_canvas_lavaan_quality_result_ui(cbsem_bundle, "cbsem", "ko")), collapse = "\n")
stopifnot(
  "항목" %in% names(cbsem_quality_ko),
  "상태" %in% names(cbsem_quality_ko),
  "수렴" %in% cbsem_quality_ko$항목,
  "χ²/df" %in% cbsem_quality_ko$항목,
  "연구모형" %in% cbsem_quality_ko$값,
  grepl("SEM 품질 체크리스트", cbsem_quality_ui_ko, fixed = TRUE),
  grepl("보고 준비도:", cbsem_quality_ui_ko, fixed = TRUE)
)
stopifnot(nrow(structural_canvas_result_table("overview", cbsem_result, "cbsem", labels_fn, language_fn)) > 0L)
cbsem_fit_en <- structural_canvas_result_table("fit", cbsem_result, "cbsem", labels_fn, language_fn)
cbsem_fit_ko <- structural_canvas_result_table("fit", cbsem_result, "cbsem", labels_fn, ko_language_fn)
stopifnot(
  nrow(cbsem_fit_en) > 0L,
  nrow(cbsem_fit_ko) > 0L,
  "χ²/df" %in% names(cbsem_fit_ko),
  !"Q" %in% names(cbsem_fit_ko),
  names(cbsem_fit_ko)[[1L]] == "모형",
  cbsem_fit_ko[[1L]][[1L]] == "연구모형"
)
cbsem_validity <- structural_canvas_result_table("validity", cbsem_result, "cbsem", labels_fn, language_fn)
cbsem_measurement <- structural_canvas_result_table("measurement", cbsem_result, "cbsem", labels_fn, language_fn)
cbsem_measurement_ci <- structural_canvas_result_table("measurement_ci", cbsem_result, "cbsem", labels_fn, language_fn)
cbsem_measurement_diagnostics <- structural_canvas_result_table("measurement_diagnostics", cbsem_result, "cbsem", labels_fn, language_fn)
stopifnot(
  nrow(cbsem_validity) > 0L,
  identical(names(cbsem_validity), c("Latent", "eta1", "eta2", "Max |r|", "AVE", "CR", "α", "ω total")),
  !any(c("FL criterion", "k", "Guidance") %in% names(cbsem_validity)),
  nrow(cbsem_measurement) > 0L,
  identical(names(cbsem_measurement), c("Latent", "Indicator", "B", "SE", "beta", "z", "p", "R²")),
  !any(grepl("Fixed", cbsem_measurement$SE, fixed = TRUE)),
  !any(grepl("95% CI", names(cbsem_measurement), fixed = TRUE)),
  nrow(cbsem_measurement_ci) == nrow(cbsem_measurement),
  all(c("B 95% CI lower", "B 95% CI upper", "beta 95% CI lower", "beta 95% CI upper", "R² 95% CI lower", "R² 95% CI upper") %in% names(cbsem_measurement_ci)),
  nrow(cbsem_measurement_diagnostics) == nrow(cbsem_measurement),
  all(c("Std. residual variance", "Cross-loading", "Guidance") %in% names(cbsem_measurement_diagnostics)),
  !any(grepl("95% CI", names(cbsem_measurement_diagnostics), fixed = TRUE))
)
result_layout_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl('uiOutput(paste0(prefix, "_result_htmt"))', result_layout_source, fixed = TRUE),
  grepl('uiOutput(paste0(prefix, "_result_htmt_details"))', result_layout_source, fixed = TRUE),
  grepl('uiOutput(paste0(prefix, "_result_structural_effects"))', result_layout_source, fixed = TRUE),
  grepl('uiOutput(paste0(prefix, "_result_structural_effect_ci"))', result_layout_source, fixed = TRUE),
  grepl('uiOutput(paste0(prefix, "_result_measurement_ci"))', result_layout_source, fixed = TRUE),
  grepl("structural_canvas_measurement_html_table", result_layout_source, fixed = TRUE),
  grepl('h4("Modification indices (MI)")', result_layout_source, fixed = TRUE),
  regexpr('uiOutput\\(paste0\\(prefix, "_result_reporting_context"\\)\\)', result_layout_source) >
    regexpr('uiOutput\\(paste0\\(prefix, "_result_measurement"\\)\\)', result_layout_source),
  regexpr('uiOutput\\(paste0\\(prefix, "_result_mi_section"\\)\\)', result_layout_source) >
    regexpr('uiOutput\\(paste0\\(prefix, "_result_measurement"\\)\\)', result_layout_source),
  regexpr('uiOutput\\(paste0\\(prefix, "_result_mi_section"\\)\\)', result_layout_source) <
    regexpr('uiOutput\\(paste0\\(prefix, "_result_residuals"\\)\\)', result_layout_source),
  regexpr('uiOutput\\(paste0\\(prefix, "_result_residuals"\\)\\)', result_layout_source) >
    regexpr('uiOutput\\(paste0\\(prefix, "_result_measurement"\\)\\)', result_layout_source),
  regexpr('uiOutput\\(paste0\\(prefix, "_result_residuals"\\)\\)', result_layout_source) <
    regexpr('uiOutput\\(paste0\\(prefix, "_result_reporting_context"\\)\\)', result_layout_source),
  regexpr('uiOutput\\(paste0\\(prefix, "_result_htmt"\\)\\)', result_layout_source) <
    regexpr('uiOutput\\(paste0\\(prefix, "_result_measurement"\\)\\)', result_layout_source),
  grepl("structural-validity-table", result_layout_source, fixed = TRUE)
)
alignment_html <- paste(as.character(structural_canvas_basic_html_table(data.frame(Label = c("x", "y"), Value = c(".12", "No"), check.names = FALSE))), collapse = "\n")
stopifnot(
  grepl("structural-numeric-cell", alignment_html, fixed = TRUE),
  grepl("structural-numeric-value", alignment_html, fixed = TRUE),
  grepl("structural-table-header-cell", alignment_html, fixed = TRUE),
  grepl("structural-result-table", alignment_html, fixed = TRUE)
)
cbsem_structural <- structural_canvas_result_table("structural", cbsem_result, "cbsem", labels_fn, language_fn)
stopifnot(nrow(cbsem_structural) == 1L)
stopifnot(!"Effect" %in% names(cbsem_structural))
stopifnot(all(c("Outcome", "Predictor", "B", "beta", "z", "p", "BH-adjusted p", "R²") %in% names(cbsem_structural)))
stopifnot(identical(names(cbsem_structural)[[ncol(cbsem_structural)]], "R²"))
stopifnot(!any(grepl("95% CI", names(cbsem_structural), fixed = TRUE)))
cbsem_structural_ci <- structural_canvas_result_table("structural_ci", cbsem_result, "cbsem", labels_fn, language_fn)
stopifnot(nrow(cbsem_structural_ci) == nrow(cbsem_structural))
stopifnot(all(c("B 95% CI lower", "B 95% CI upper", "B CI source", "beta 95% CI lower", "beta 95% CI upper", "beta CI source") %in% names(cbsem_structural_ci)))

moderation_data <- data
moderation_data$W <- stats::rnorm(nrow(moderation_data))
moderation_data$y1 <- moderation_data$y1 + 0.55 * eta1 * moderation_data$W
moderation_data$y2 <- moderation_data$y2 + 0.55 * eta1 * moderation_data$W
moderation_data$y3 <- moderation_data$y3 + 0.55 * eta1 * moderation_data$W
moderation_snapshot <- snapshot
moderation_snapshot$nodes <- c(moderation_snapshot$nodes, list(list(id = "w", role = "moderator", name = "W", variableId = "W", canvasLabel = "W", x = 260, y = 20)))
moderation_snapshot$moderations <- list(list(id = "mod_eta1_eta2", from = "w", toEdge = "p1"))
cbsem_moderation <- run_structural_canvas_analysis(moderation_snapshot, moderation_data, "cbsem", estimator = "MLR", missing = "fiml")
cbsem_jn <- structural_canvas_moderation_jn_table(list(fit = cbsem_moderation$fit, diagnostics = cbsem_moderation))
cbsem_moderation_table <- structural_canvas_result_table(
  "structural",
  function() list(fit = cbsem_moderation$fit, snapshot = moderation_snapshot, diagnostics = cbsem_moderation),
  "cbsem",
  labels_fn,
  language_fn
)
cbsem_moderation_snapshot <- structural_canvas_result_snapshot(moderation_snapshot, cbsem_moderation$fit, "b_p")
stopifnot(
  length(cbsem_moderation$moderation_definitions) == 1L,
  grepl("statedu_int", cbsem_moderation$syntax, fixed = TRUE),
  any(cbsem_moderation_table$Predictor == "eta1*W"),
  !any(grepl("^statedu_int", cbsem_moderation_table$Predictor)),
  length(cbsem_moderation_snapshot$moderations) == 1L,
  isTRUE(cbsem_moderation_snapshot$moderations[[1L]]$resultMatched),
  nzchar(cbsem_moderation_snapshot$moderations[[1L]]$label),
  grepl("\\(", cbsem_moderation_snapshot$moderations[[1L]]$label),
  is.data.frame(cbsem_jn),
  all(c("Effect", "Path", "Moderator", "Moderator range", "Midpoint effect", "SE", "z", "p", "Significant") %in% names(cbsem_jn)),
  "Direct" %in% cbsem_jn$Effect
)
moderated_mediation_n <- 320L
moderated_x <- stats::rnorm(moderated_mediation_n)
moderated_w <- stats::rnorm(moderated_mediation_n)
moderated_m <- (0.45 + 0.80 * moderated_w) * moderated_x + stats::rnorm(moderated_mediation_n, sd = 0.55)
moderated_y <- 0.75 * moderated_m + stats::rnorm(moderated_mediation_n, sd = 0.55)
moderated_mediation_data <- data.frame(
  x1 = 0.82 * moderated_x + stats::rnorm(moderated_mediation_n, sd = 0.35),
  x2 = 0.78 * moderated_x + stats::rnorm(moderated_mediation_n, sd = 0.38),
  x3 = 0.75 * moderated_x + stats::rnorm(moderated_mediation_n, sd = 0.40),
  m1 = 0.84 * moderated_m + stats::rnorm(moderated_mediation_n, sd = 0.35),
  m2 = 0.80 * moderated_m + stats::rnorm(moderated_mediation_n, sd = 0.38),
  m3 = 0.76 * moderated_m + stats::rnorm(moderated_mediation_n, sd = 0.40),
  y1 = 0.86 * moderated_y + stats::rnorm(moderated_mediation_n, sd = 0.35),
  y2 = 0.81 * moderated_y + stats::rnorm(moderated_mediation_n, sd = 0.38),
  y3 = 0.77 * moderated_y + stats::rnorm(moderated_mediation_n, sd = 0.40),
  W = moderated_w
)
moderated_mediation_snapshot <- list(
  nodes = list(
    list(id = "mx", role = "latent", name = "eta1", canvasLabel = "eta1", x = 120, y = 120, measurementMode = "reflective"),
    list(id = "mm", role = "latent", name = "eta2", canvasLabel = "eta2", x = 420, y = 120, measurementMode = "reflective"),
    list(id = "my", role = "latent", name = "eta3", canvasLabel = "eta3", x = 720, y = 120, measurementMode = "reflective"),
    list(id = "mx1", role = "indicator", name = "x1", variableId = "x1", canvasLabel = "x1", x = 120, y = 250),
    list(id = "mx2", role = "indicator", name = "x2", variableId = "x2", canvasLabel = "x2", x = 120, y = 320),
    list(id = "mx3", role = "indicator", name = "x3", variableId = "x3", canvasLabel = "x3", x = 120, y = 390),
    list(id = "mm1", role = "indicator", name = "m1", variableId = "m1", canvasLabel = "m1", x = 420, y = 250),
    list(id = "mm2", role = "indicator", name = "m2", variableId = "m2", canvasLabel = "m2", x = 420, y = 320),
    list(id = "mm3", role = "indicator", name = "m3", variableId = "m3", canvasLabel = "m3", x = 420, y = 390),
    list(id = "my1", role = "indicator", name = "y1", variableId = "y1", canvasLabel = "y1", x = 720, y = 250),
    list(id = "my2", role = "indicator", name = "y2", variableId = "y2", canvasLabel = "y2", x = 720, y = 320),
    list(id = "my3", role = "indicator", name = "y3", variableId = "y3", canvasLabel = "y3", x = 720, y = 390),
    list(id = "mw", role = "moderator", name = "W", variableId = "W", canvasLabel = "W", x = 260, y = 20)
  ),
  edges = list(
    list(id = "me1", from = "mx", to = "mx1"),
    list(id = "me2", from = "mx", to = "mx2"),
    list(id = "me3", from = "mx", to = "mx3"),
    list(id = "me4", from = "mm", to = "mm1"),
    list(id = "me5", from = "mm", to = "mm2"),
    list(id = "me6", from = "mm", to = "mm3"),
    list(id = "me7", from = "my", to = "my1"),
    list(id = "me8", from = "my", to = "my2"),
    list(id = "me9", from = "my", to = "my3"),
    list(id = "mp1", from = "mx", to = "mm"),
    list(id = "mp2", from = "mm", to = "my")
  ),
  moderations = list(list(id = "mod_mediation", from = "mw", toEdge = "mp1"))
)
cbsem_moderated_mediation <- run_structural_canvas_analysis(moderated_mediation_snapshot, moderated_mediation_data, "cbsem", estimator = "MLR", missing = "fiml")
cbsem_moderated_index <- structural_canvas_moderated_mediation_indices(cbsem_moderated_mediation)
stopifnot(nrow(cbsem_moderated_index) >= 1L, all(cbsem_moderated_index$op == "modmed"), all(is.finite(cbsem_moderated_index$est)))
cbsem_moderated_bootstrap <- structural_canvas_effect_bootstrap(
  moderated_mediation_snapshot, moderated_mediation_data, "cbsem", "MLR", "fiml", FALSE,
  character(0), character(0), numeric(0), reps = 20L, seed = 20260820L
)
stopifnot(any(cbsem_moderated_bootstrap$op == "modmed"))
stopifnot(all(c("lower", "upper", "p", "beta_status", "valid_percent", "status") %in% names(cbsem_moderated_bootstrap)))
stopifnot(all(cbsem_moderated_bootstrap$beta_status[cbsem_moderated_bootstrap$op == "modmed"] == "Not reported: product-indicator index is scale-dependent"))
cbsem_moderated_mediation_jn <- structural_canvas_moderation_jn_table(list(fit = cbsem_moderated_mediation$fit, diagnostics = cbsem_moderated_mediation))
stopifnot(
  length(cbsem_moderated_mediation$effect_definitions) > 0L,
  is.data.frame(cbsem_moderated_mediation_jn),
  "Indirect" %in% cbsem_moderated_mediation_jn$Effect
)
latent_moderation_n <- 520L
latent_moderation_x <- stats::rnorm(latent_moderation_n)
latent_moderation_w <- stats::rnorm(latent_moderation_n)
latent_moderation_m <- (0.50 + 0.95 * latent_moderation_w) * latent_moderation_x + 0.25 * latent_moderation_w + stats::rnorm(latent_moderation_n, sd = 0.45)
latent_moderation_y <- 0.85 * latent_moderation_m + stats::rnorm(latent_moderation_n, sd = 0.45)
latent_moderation_data <- data.frame(
  x1 = 0.86 * latent_moderation_x + stats::rnorm(latent_moderation_n, sd = 0.30),
  x2 = 0.82 * latent_moderation_x + stats::rnorm(latent_moderation_n, sd = 0.32),
  x3 = 0.78 * latent_moderation_x + stats::rnorm(latent_moderation_n, sd = 0.34),
  w1 = 0.86 * latent_moderation_w + stats::rnorm(latent_moderation_n, sd = 0.30),
  w2 = 0.82 * latent_moderation_w + stats::rnorm(latent_moderation_n, sd = 0.32),
  w3 = 0.78 * latent_moderation_w + stats::rnorm(latent_moderation_n, sd = 0.34),
  m1 = 0.86 * latent_moderation_m + stats::rnorm(latent_moderation_n, sd = 0.30),
  m2 = 0.82 * latent_moderation_m + stats::rnorm(latent_moderation_n, sd = 0.32),
  m3 = 0.78 * latent_moderation_m + stats::rnorm(latent_moderation_n, sd = 0.34),
  y1 = 0.86 * latent_moderation_y + stats::rnorm(latent_moderation_n, sd = 0.30),
  y2 = 0.82 * latent_moderation_y + stats::rnorm(latent_moderation_n, sd = 0.32),
  y3 = 0.78 * latent_moderation_y + stats::rnorm(latent_moderation_n, sd = 0.34)
)
latent_moderation_snapshot <- list(
  nodes = list(
    list(id = "lmx", role = "latent", name = "etaX", canvasLabel = "etaX", measurementMode = "reflective"),
    list(id = "lmw", role = "latent", name = "etaW", canvasLabel = "etaW", measurementMode = "reflective"),
    list(id = "lmm", role = "latent", name = "etaM", canvasLabel = "etaM", measurementMode = "reflective"),
    list(id = "lmy", role = "latent", name = "etaY", canvasLabel = "etaY", measurementMode = "reflective"),
    list(id = "lmx1", role = "indicator", name = "x1", variableId = "x1", canvasLabel = "x1"),
    list(id = "lmx2", role = "indicator", name = "x2", variableId = "x2", canvasLabel = "x2"),
    list(id = "lmx3", role = "indicator", name = "x3", variableId = "x3", canvasLabel = "x3"),
    list(id = "lmw1", role = "indicator", name = "w1", variableId = "w1", canvasLabel = "w1"),
    list(id = "lmw2", role = "indicator", name = "w2", variableId = "w2", canvasLabel = "w2"),
    list(id = "lmw3", role = "indicator", name = "w3", variableId = "w3", canvasLabel = "w3"),
    list(id = "lmm1", role = "indicator", name = "m1", variableId = "m1", canvasLabel = "m1"),
    list(id = "lmm2", role = "indicator", name = "m2", variableId = "m2", canvasLabel = "m2"),
    list(id = "lmm3", role = "indicator", name = "m3", variableId = "m3", canvasLabel = "m3"),
    list(id = "lmy1", role = "indicator", name = "y1", variableId = "y1", canvasLabel = "y1"),
    list(id = "lmy2", role = "indicator", name = "y2", variableId = "y2", canvasLabel = "y2"),
    list(id = "lmy3", role = "indicator", name = "y3", variableId = "y3", canvasLabel = "y3")
  ),
  edges = list(
    list(id = "lme1", from = "lmx", to = "lmx1"),
    list(id = "lme2", from = "lmx", to = "lmx2"),
    list(id = "lme3", from = "lmx", to = "lmx3"),
    list(id = "lme4", from = "lmw", to = "lmw1"),
    list(id = "lme5", from = "lmw", to = "lmw2"),
    list(id = "lme6", from = "lmw", to = "lmw3"),
    list(id = "lme7", from = "lmm", to = "lmm1"),
    list(id = "lme8", from = "lmm", to = "lmm2"),
    list(id = "lme9", from = "lmm", to = "lmm3"),
    list(id = "lme10", from = "lmy", to = "lmy1"),
    list(id = "lme11", from = "lmy", to = "lmy2"),
    list(id = "lme12", from = "lmy", to = "lmy3"),
    list(id = "lmp1", from = "lmx", to = "lmm"),
    list(id = "lmp2", from = "lmm", to = "lmy")
  ),
  moderations = list(list(id = "latent_mod_mediation", from = "lmw", toEdge = "lmp1"))
)
cbsem_latent_moderation <- run_structural_canvas_analysis(latent_moderation_snapshot, latent_moderation_data, "cbsem", estimator = "MLR", missing = "fiml")
cbsem_latent_moderation_jn <- structural_canvas_moderation_jn_table(list(fit = cbsem_latent_moderation$fit, diagnostics = cbsem_latent_moderation))
stopifnot(
  isTRUE(cbsem_latent_moderation$converged),
  length(cbsem_latent_moderation$moderation_definitions) == 1L,
  identical(cbsem_latent_moderation$moderation_definitions[[1L]]$moderator_role, "latent"),
  identical(cbsem_latent_moderation$moderation_definitions[[1L]]$product_indicator_method, "all_pairs_dmc"),
  cbsem_latent_moderation$moderation_definitions[[1L]]$product_indicator_count == 9L,
  grepl("statedu_int", cbsem_latent_moderation$syntax, fixed = TRUE),
  grepl("statedu_pi_etaX_etaW_x1_w1", cbsem_latent_moderation$syntax, fixed = TRUE),
  is.data.frame(cbsem_latent_moderation_jn),
  all(c("Direct", "Indirect") %in% cbsem_latent_moderation_jn$Effect)
)
matched_latent_moderation_snapshot <- latent_moderation_snapshot
matched_latent_moderation_snapshot$moderationMethod <- "matched_pair_dmc"
cbsem_latent_moderation_matched <- run_structural_canvas_analysis(matched_latent_moderation_snapshot, latent_moderation_data, "cbsem", estimator = "MLR", missing = "fiml")
stopifnot(
  isTRUE(cbsem_latent_moderation_matched$converged),
  identical(cbsem_latent_moderation_matched$moderation_definitions[[1L]]$product_indicator_method, "matched_pair_dmc"),
  cbsem_latent_moderation_matched$moderation_definitions[[1L]]$product_indicator_count == 3L
)
stopifnot(cbsem_structural$Outcome[[1L]] == "eta2")
stopifnot(cbsem_structural$Predictor[[1L]] == "eta1")

cbsem_snapshot <- structural_canvas_result_snapshot(snapshot, cbsem$fit, "beta")
cbsem_labels <- vapply(cbsem_snapshot$edges, function(edge) as.character(edge$label %||% ""), character(1))
stopifnot(any(nzchar(cbsem_labels)))

sem <- run_structural_canvas_analysis(snapshot, data, "sem", estimator = "ML", missing = "fiml")
stopifnot(inherits(sem$fit, "lavaan"))
stopifnot(isTRUE(sem$converged))
stopifnot(grepl("eta2 ~", sem$syntax, fixed = TRUE), grepl("*eta1", sem$syntax, fixed = TRUE))
sem_bundle <- list(
  fit = sem$fit,
  syntax = sem$syntax,
  snapshot = snapshot,
  diagnostics = sem,
  estimator = "ML",
  rmsea_ci = 0.90,
  validity_formula = "standardized"
)
sem_result <- function() sem_bundle
stopifnot(nrow(structural_canvas_result_table("overview", sem_result, "sem", labels_fn, language_fn)) > 0L)
sem_structural <- structural_canvas_result_table("structural", sem_result, "sem", labels_fn, language_fn)
stopifnot(nrow(sem_structural) == 1L)
stopifnot(sem_structural$Effect[[1L]] == "Direct")
stopifnot(sem_structural$Outcome[[1L]] == "eta2")
stopifnot(sem_structural$Predictor[[1L]] == "eta1")
sem_recommendation <- structural_canvas_estimator_recommendation(snapshot, data, variable_table, "sem", "ML")
stopifnot(is.logical(sem_recommendation$recommend), !is.null(sem_recommendation$diagnosis))
sem_htmt_bootstrap <- structural_canvas_run_htmt_bootstrap("sem", 8L, sem, data, 20260818L, character(0), .85, "percentile")
stopifnot(is.data.frame(sem_htmt_bootstrap))
stopifnot(nrow(sem_htmt_bootstrap) == 1L)
stopifnot(sem_htmt_bootstrap[["Requested replicates"]][[1L]] == 8L)

set.seed(20260821)
group_n <- 220L
group_value <- rep(c("A", "B"), each = group_n / 2L)
group_eta1 <- stats::rnorm(group_n)
group_eta2 <- ifelse(group_value == "A", 0.35, 0.75) * group_eta1 + stats::rnorm(group_n, sd = 0.72)
group_data <- data.frame(
  x1 = 0.78 * group_eta1 + stats::rnorm(group_n, sd = 0.45),
  x2 = 0.72 * group_eta1 + stats::rnorm(group_n, sd = 0.50),
  x3 = 0.69 * group_eta1 + stats::rnorm(group_n, sd = 0.52),
  y1 = 0.82 * group_eta2 + stats::rnorm(group_n, sd = 0.42),
  y2 = 0.76 * group_eta2 + stats::rnorm(group_n, sd = 0.48),
  y3 = 0.70 * group_eta2 + stats::rnorm(group_n, sd = 0.55),
  group = group_value
)
cbsem_group_base <- run_structural_canvas_analysis(snapshot, group_data, "cbsem", estimator = "ML", missing = "fiml")
structural_group_comparison <- structural_canvas_structural_path_group_comparison(
  cbsem_group_base$syntax, group_data, "group", estimator = "ML", missing = "fiml"
)
stopifnot(
  identical(structural_group_comparison$type, "structural_path_comparison"),
  nrow(structural_group_comparison$table) == 2L,
  identical(structural_group_comparison$table$Model, c("Free structural paths", "Equal structural paths")),
  all(structural_group_comparison$table$Converged),
  all(structural_group_comparison$table$Admissible),
  nrow(structural_group_comparison$group_diagnostics) == 2L,
  nrow(structural_group_comparison$path_estimates) == 2L,
  all(c("Group", "Outcome", "Predictor", "B", "SE", "beta") %in% names(structural_group_comparison$path_estimates)),
  nrow(structural_group_comparison$path_differences) == 1L,
  all(c("Group 1", "Group 2", "B difference", "BH-adjusted p") %in% names(structural_group_comparison$path_differences)),
  is.finite(structural_group_comparison$table$DeltaP[[2L]]),
  structural_group_comparison$path_differences$Predictor[[1L]] == "eta1",
  structural_group_comparison$path_differences$Outcome[[1L]] == "eta2"
)

set.seed(20260814)
n_mediation <- 240L
eta_a <- stats::rnorm(n_mediation)
eta_b <- 0.55 * eta_a + stats::rnorm(n_mediation, sd = 0.80)
eta_c <- 0.35 * eta_a + 0.50 * eta_b + stats::rnorm(n_mediation, sd = 0.75)
mediation_data <- data.frame(
  a1 = 0.82 * eta_a + stats::rnorm(n_mediation, sd = 0.42),
  a2 = 0.75 * eta_a + stats::rnorm(n_mediation, sd = 0.50),
  a3 = 0.70 * eta_a + stats::rnorm(n_mediation, sd = 0.55),
  b1 = 0.80 * eta_b + stats::rnorm(n_mediation, sd = 0.45),
  b2 = 0.73 * eta_b + stats::rnorm(n_mediation, sd = 0.52),
  b3 = 0.68 * eta_b + stats::rnorm(n_mediation, sd = 0.58),
  c1 = 0.84 * eta_c + stats::rnorm(n_mediation, sd = 0.40),
  c2 = 0.77 * eta_c + stats::rnorm(n_mediation, sd = 0.48),
  c3 = 0.71 * eta_c + stats::rnorm(n_mediation, sd = 0.54)
)
mediation_snapshot <- list(
  nodes = c(
    list(
      list(id = "eta_a", role = "latent", name = "etaA", canvasLabel = "etaA", x = 100, y = 100),
      list(id = "eta_b", role = "latent", name = "etaB", canvasLabel = "etaB", x = 360, y = 100),
      list(id = "eta_c", role = "latent", name = "etaC", canvasLabel = "etaC", x = 620, y = 100)
    ),
    lapply(seq_len(3L), function(index) list(id = paste0("a", index), role = "indicator", name = paste0("a", index), variableId = paste0("a", index), canvasLabel = paste0("a", index), x = 100, y = 210 + index * 60)),
    lapply(seq_len(3L), function(index) list(id = paste0("b", index), role = "indicator", name = paste0("b", index), variableId = paste0("b", index), canvasLabel = paste0("b", index), x = 360, y = 210 + index * 60)),
    lapply(seq_len(3L), function(index) list(id = paste0("c", index), role = "indicator", name = paste0("c", index), variableId = paste0("c", index), canvasLabel = paste0("c", index), x = 620, y = 210 + index * 60))
  ),
  edges = c(
    lapply(seq_len(3L), function(index) list(id = paste0("ea", index), from = "eta_a", to = paste0("a", index))),
    lapply(seq_len(3L), function(index) list(id = paste0("eb", index), from = "eta_b", to = paste0("b", index))),
    lapply(seq_len(3L), function(index) list(id = paste0("ec", index), from = "eta_c", to = paste0("c", index))),
    list(
      list(id = "ab", from = "eta_a", to = "eta_b"),
      list(id = "bc", from = "eta_b", to = "eta_c"),
      list(id = "ac", from = "eta_a", to = "eta_c")
    )
  )
)
cbsem_mediation <- run_structural_canvas_analysis(mediation_snapshot, mediation_data, "cbsem", estimator = "ML", missing = "fiml")
stopifnot(isTRUE(cbsem_mediation$converged))
causal_boundary <- structural_canvas_causal_interpretation(mediation_snapshot, "cbsem")
stopifnot(
  isTRUE(causal_boundary$applicable),
  identical(causal_boundary$status, "Causal identification not established"),
  identical(causal_boundary$interpretation, "Associational structural parameters"),
  isTRUE(causal_boundary$indirect_chain_detected),
  nrow(causal_boundary$rows) == 4L,
  any(grepl("confounding", causal_boundary$rows$Assumption, ignore.case = TRUE)),
  any(grepl("associational", causal_boundary$rows$Consequence, ignore.case = TRUE))
)
cbsem_effect_bootstrap <- structural_canvas_effect_bootstrap(
  mediation_snapshot, mediation_data, "cbsem", "ML", "fiml", FALSE,
  character(0), character(0), numeric(0), reps = 30L, seed = 20260819L
)
stopifnot(is.data.frame(cbsem_effect_bootstrap))
stopifnot(any(cbsem_effect_bootstrap$op == ":="))
stopifnot(all(c("lower", "upper", "p", "beta_estimate", "beta_lower", "beta_upper", "beta_p", "beta_valid", "valid", "requested", "valid_percent", "status") %in% names(cbsem_effect_bootstrap)))
stopifnot(all(cbsem_effect_bootstrap$requested == 30L))
stopifnot(grepl(":=", cbsem_mediation$syntax, fixed = TRUE))
stopifnot(length(cbsem_mediation$effect_definitions) == 3L)
cbsem_mediation_bundle <- list(
  fit = cbsem_mediation$fit,
  syntax = cbsem_mediation$syntax,
  snapshot = mediation_snapshot,
  diagnostics = cbsem_mediation,
  estimator = "ML",
  rmsea_ci = 0.90,
  validity_formula = "standardized"
)
cbsem_mediation_bundle$effect_bootstrap_result <- cbsem_effect_bootstrap
cbsem_mediation_bundle$analysis_data <- mediation_data
cbsem_mediation_bundle$sampling_design <- "independent_cross_sectional"
cbsem_mediation_bundle$sampling_design_gate <- structural_canvas_sampling_design_gate("independent_cross_sectional")
cbsem_mediation_audit <- structural_canvas_audit_manifest(cbsem_mediation_bundle, "cbsem")
stopifnot(
  identical(cbsem_mediation_audit$schema$version, "1.5"),
  grepl("recorded seed", cbsem_mediation_audit$resampling$reproducibility_policy$requirement, fixed = TRUE),
  grepl("data and model fingerprints", cbsem_mediation_audit$resampling$reproducibility_policy$additional_conditions, fixed = TRUE),
  identical(cbsem_mediation_audit$decision$causal_interpretation$status, "Causal identification not established"),
  isTRUE(cbsem_mediation_audit$decision$causal_interpretation$indirect_chain_detected),
  identical(cbsem_mediation_audit$data_fingerprints$analysis$rows, nrow(mediation_data)),
  identical(cbsem_mediation_audit$data_fingerprints$analysis$columns, ncol(mediation_data)),
  nchar(cbsem_mediation_audit$data_fingerprints$analysis$content_sha256) == 64L,
  nchar(cbsem_mediation_audit$model$specification_sha256) == 64L,
  nchar(cbsem_mediation_audit$generated$analysis_code$sha256) == 64L,
  is.logical(cbsem_mediation_audit$generated$git$available),
  all(c("Category", "Severity", "Message") %in% names(cbsem_mediation_audit$warnings)),
  any(cbsem_mediation_audit$warnings$Category == "Causal interpretation"),
  isFALSE(cbsem_mediation_audit$privacy$raw_data_included)
)
audit_file <- tempfile(fileext = ".json")
on.exit(unlink(audit_file), add = TRUE)
structural_canvas_write_audit_manifest(cbsem_mediation_bundle, audit_file, "cbsem")
audit_roundtrip <- jsonlite::read_json(audit_file, simplifyVector = TRUE)
stopifnot(
  identical(audit_roundtrip$schema$version, "1.5"),
  grepl("recorded seed", audit_roundtrip$resampling$reproducibility_policy$requirement, fixed = TRUE),
  identical(audit_roundtrip$data_fingerprints$analysis$content_sha256, cbsem_mediation_audit$data_fingerprints$analysis$content_sha256)
)
cbsem_mediation_result <- function() cbsem_mediation_bundle
cbsem_mediation_structural <- structural_canvas_result_table("structural", cbsem_mediation_result, "cbsem", labels_fn, language_fn)
cbsem_mediation_effects <- structural_canvas_result_table("structural_effects", cbsem_mediation_result, "cbsem", labels_fn, language_fn)
cbsem_mediation_effect_ci <- structural_canvas_result_table("structural_effect_ci", cbsem_mediation_result, "cbsem", labels_fn, language_fn)
cbsem_specific_indirect <- structural_canvas_result_table("structural_specific_indirect", cbsem_mediation_result, "cbsem", labels_fn, language_fn)
stopifnot(nrow(cbsem_mediation_structural) == 3L)
stopifnot(!"Effect" %in% names(cbsem_mediation_structural))
stopifnot(any(cbsem_mediation_effects$Outcome == "etaC" & cbsem_mediation_effects$Predictor == "etaA"))
stopifnot(all(c("Direct beta", "Direct p", "Direct BH-adjusted p", "Indirect beta", "Indirect p", "Indirect BH-adjusted p", "Total beta", "Total p", "Total BH-adjusted p") %in% names(cbsem_mediation_effects)))
stopifnot(!"Specific indirect beta" %in% names(cbsem_mediation_effects))
stopifnot(any(nzchar(cbsem_mediation_effects[["Indirect beta"]][cbsem_mediation_effects$Outcome == "etaC" & cbsem_mediation_effects$Predictor == "etaA"])))
stopifnot(any(nzchar(cbsem_mediation_effects[["Total p"]][cbsem_mediation_effects$Outcome == "etaC" & cbsem_mediation_effects$Predictor == "etaA"])))
stopifnot(any(nzchar(cbsem_mediation_structural[["R²"]][cbsem_mediation_structural$Outcome == "etaC"])))
stopifnot(all(c("Direct beta 95% CI", "Direct CI source", "Indirect beta 95% CI", "Indirect CI source", "Total beta 95% CI", "Total CI source") %in% names(cbsem_mediation_effect_ci)))
stopifnot(!"Specific indirect beta 95% CI" %in% names(cbsem_mediation_effect_ci))
stopifnot(any(nzchar(cbsem_mediation_effect_ci[["Indirect beta 95% CI"]][cbsem_mediation_effect_ci$Outcome == "etaC" & cbsem_mediation_effect_ci$Predictor == "etaA"])))
stopifnot(nrow(cbsem_specific_indirect) == 1L)
stopifnot(all(c("Path", "B", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "z", "p", "BH-adjusted p") %in% names(cbsem_specific_indirect)))
stopifnot(grepl("etaA → etaB → etaC", cbsem_specific_indirect$Path[[1L]], fixed = TRUE))
stopifnot(nzchar(cbsem_specific_indirect[["Boot SE"]][[1L]]), nzchar(cbsem_specific_indirect[["Boot 95% CI lower"]][[1L]]), nzchar(cbsem_specific_indirect[["Boot 95% CI upper"]][[1L]]))

sem_mediation <- run_structural_canvas_analysis(mediation_snapshot, mediation_data, "sem", estimator = "ML", missing = "fiml")
stopifnot(isTRUE(sem_mediation$converged))
stopifnot(grepl(":=", sem_mediation$syntax, fixed = TRUE))
stopifnot(length(sem_mediation$effect_definitions) == 3L)
sem_mediation_bundle <- list(
  fit = sem_mediation$fit,
  syntax = sem_mediation$syntax,
  snapshot = mediation_snapshot,
  diagnostics = sem_mediation,
  estimator = "ML",
  rmsea_ci = 0.90,
  validity_formula = "standardized"
)
sem_mediation_result <- function() sem_mediation_bundle
sem_mediation_structural <- structural_canvas_result_table("structural", sem_mediation_result, "sem", labels_fn, language_fn)
sem_mediation_effects <- structural_canvas_result_table("structural_effects", sem_mediation_result, "sem", labels_fn, language_fn)
sem_specific_indirect <- structural_canvas_result_table("structural_specific_indirect", sem_mediation_result, "sem", labels_fn, language_fn)
stopifnot(!"Effect" %in% names(sem_mediation_structural))
stopifnot(any(sem_mediation_effects$Outcome == "etaC" & sem_mediation_effects$Predictor == "etaA"))
stopifnot(any(nzchar(sem_mediation_effects[["Indirect beta"]][sem_mediation_effects$Outcome == "etaC" & sem_mediation_effects$Predictor == "etaA"])))
stopifnot(any(nzchar(sem_mediation_effects[["Total beta"]][sem_mediation_effects$Outcome == "etaC" & sem_mediation_effects$Predictor == "etaA"])))
stopifnot(nrow(sem_specific_indirect) == 1L)
stopifnot(grepl("etaA → etaB → etaC", sem_specific_indirect$Path[[1L]], fixed = TRUE))

parallel_snapshot <- mediation_snapshot
parallel_snapshot$edges <- c(
  parallel_snapshot$edges[seq_len(9L)],
  list(
    list(id = "ac", from = "eta_a", to = "eta_c"),
    list(id = "bc", from = "eta_b", to = "eta_c"),
    list(id = "cov_ab", from = "eta_a", to = "eta_b", kind = "covariance")
  )
)
cbsem_parallel <- run_structural_canvas_analysis(parallel_snapshot, mediation_data, "cbsem", estimator = "ML", missing = "fiml")
stopifnot(isTRUE(cbsem_parallel$converged))
stopifnot(grepl("etaA ~~ etaB", cbsem_parallel$syntax, fixed = TRUE))
stopifnot(!length(structural_canvas_missing_exogenous_covariances(parallel_snapshot)))
cbsem_parallel_bundle <- list(
  fit = cbsem_parallel$fit,
  syntax = cbsem_parallel$syntax,
  snapshot = parallel_snapshot,
  diagnostics = cbsem_parallel,
  estimator = "ML",
  rmsea_ci = 0.90,
  validity_formula = "standardized"
)
cbsem_parallel_result <- function() cbsem_parallel_bundle
cbsem_parallel_structural <- structural_canvas_result_table("structural", cbsem_parallel_result, "cbsem", labels_fn, language_fn)
stopifnot(nrow(cbsem_parallel_structural) == 2L)
stopifnot(all(c("etaA", "etaB") %in% cbsem_parallel_structural$Predictor[cbsem_parallel_structural$Outcome == "etaC"]))

cycle_snapshot <- snapshot
cycle_snapshot$edges <- c(cycle_snapshot$edges, list(list(id = "p2", from = "lv2", to = "lv1")))
cycle_syntax <- structural_canvas_lavaan_syntax(
  cycle_snapshot, data, "cbsem",
  Filter(function(node) identical(node$role, "latent"), cycle_snapshot$nodes),
  cycle_snapshot$edges,
  character(0)
)
stopifnot(!grepl(":=", cycle_syntax$syntax, fixed = TRUE), !length(cycle_syntax$effect_definitions))

pls <- run_structural_canvas_analysis(snapshot, data, "plssem", estimator = "PLS")
stopifnot(inherits(pls$fit, "pls_model"))
stopifnot(isTRUE(pls$converged))
stopifnot(identical(pls$estimator, "PLS"))
stopifnot(grepl("eta2 ~ eta1", pls$syntax, fixed = TRUE))
stopifnot(length(pls$constructs) == 2L)
stopifnot(length(pls$observed) == 6L)

plsc <- run_structural_canvas_analysis(snapshot, data, "plssem", estimator = "PLSc")
stopifnot(inherits(plsc$fit, "pls_model"))
stopifnot(identical(plsc$estimator, "PLSc"))
automatic_plsc <- run_structural_canvas_analysis(snapshot, data, "plssem", estimator = "AUTO")
stopifnot(
  identical(automatic_plsc$estimator, "PLSc"),
  identical(automatic_plsc$estimator_requested, "AUTO"),
  setequal(automatic_plsc$plsc_corrected_constructs, c("eta1", "eta2"))
)
plsc_bundle <- list(
  fit = plsc$fit,
  syntax = plsc$syntax,
  snapshot = snapshot,
  diagnostics = plsc,
  estimator = "PLSc"
)
plsc_result <- function() plsc_bundle
plsc_overview <- structural_canvas_result_table("overview", plsc_result, "plssem", labels_fn, language_fn)
plsc_reporting <- structural_canvas_reporting_context_rows(plsc_bundle, "plssem")
plsc_fit_diagnostics <- structural_canvas_pls_fit_diagnostics_table(plsc_bundle)
stopifnot(plsc_overview$Value[plsc_overview$Item == "Estimator"] == "PLSc")
stopifnot(plsc_reporting$Value[plsc_reporting$Item == "Estimator or algorithm"] == "PLSc path modeling (Reflective common-factor model)")
stopifnot(identical(plsc_fit_diagnostics$Model, c("pls", "plsc")))
stopifnot(all(nzchar(plsc_fit_diagnostics$srmr)), all(nzchar(plsc_fit_diagnostics$d_G)), all(nzchar(plsc_fit_diagnostics$d_ULS)))

pls_bundle <- list(
  fit = pls$fit,
  syntax = pls$syntax,
  snapshot = snapshot,
  diagnostics = pls,
  estimator = "PLS"
)
pls_result <- function() pls_bundle
pls_reporting <- structural_canvas_reporting_context_rows(pls_bundle, "plssem")
stopifnot(nrow(pls_reporting) == 16L)
stopifnot(grepl("seminr", pls_reporting$Value[pls_reporting$Item == "Analysis engine"], fixed = TRUE))
stopifnot(pls_reporting$Value[pls_reporting$Item == "Estimator or algorithm"] == "PLS path modeling (Composite PLS model)")
stopifnot(pls_reporting$Value[pls_reporting$Item == "Missing-data handling"] == "Valid rows used by seminr; no FIML/pairwise option")
stopifnot(pls_reporting$Value[pls_reporting$Item == "Latent scaling"] == "Composite scores")
stopifnot(pls_reporting$Value[pls_reporting$Item == "Common method diagnostics"] == "Not enabled")
pls_quality <- structural_canvas_pls_quality_rows(pls_bundle)
stopifnot(nrow(pls_quality) == 18L)
stopifnot(all(c("Item", "Value", "Status", "Guidance") %in% names(pls_quality)))
stopifnot(all(c("PLS algorithm iterations", "Approx PLS SRMR", "Approx d_G", "Approx d_ULS", "Approx NFI", "10-times rule margin", "Min outer loading", "Min rhoC", "Min AVE", "Max HTMT", "Max item VIF", "Max inner VIF", "Max full collinearity VIF", "Min endogenous R2", "Max f2", "PLSpredict summary") %in% pls_quality$Item))
stopifnot(!"Min score-CV Q2" %in% pls_quality$Item)
stopifnot(nzchar(pls_quality$Value[pls_quality$Item == "PLS algorithm iterations"]))
stopifnot(pls_quality$Status[pls_quality$Item == "PLS algorithm iterations"] == "OK")
stopifnot(nzchar(pls_quality$Value[pls_quality$Item == "Approx PLS SRMR"]))
stopifnot(nzchar(pls_quality$Value[pls_quality$Item == "Approx d_G"]))
stopifnot(nzchar(pls_quality$Value[pls_quality$Item == "Approx d_ULS"]))
stopifnot(nzchar(pls_quality$Value[pls_quality$Item == "Approx NFI"]))
stopifnot(all(pls_quality$Status[pls_quality$Item %in% c("Approx PLS SRMR", "Approx d_G", "Approx d_ULS", "Approx NFI")] == "Descriptive only"))
stopifnot(nzchar(pls_quality$Value[pls_quality$Item == "Min outer loading"]))
stopifnot(nzchar(pls_quality$Value[pls_quality$Item == "10-times rule margin"]))
stopifnot(pls_quality$Status[pls_quality$Item == "10-times rule margin"] == "Descriptive only")
stopifnot(nzchar(pls_quality$Value[pls_quality$Item == "Max full collinearity VIF"]))
stopifnot(pls_quality$Status[pls_quality$Item == "Max full collinearity VIF"] == "Screen only")
stopifnot(all(pls_quality$Status[pls_quality$Item %in% c("Min outer loading", "Min rhoC", "Min AVE")] %in% c("Reference only", "Review")))
stopifnot(pls_quality$Status[pls_quality$Item == "Max HTMT"] %in% c("Reference only", "Review"))
stopifnot(all(pls_quality$Status[pls_quality$Item %in% c("Max item VIF", "Max inner VIF")] %in% c("Reference only", "Review", "Not assessed")))
stopifnot(all(pls_quality$Status[pls_quality$Item %in% c("Min endogenous R2", "Max f2")] == "Descriptive only"))
stopifnot(pls_quality$Value[pls_quality$Item == "PLSpredict summary"] == "Not executed")
stopifnot(pls_quality$Status[pls_quality$Item == "PLSpredict summary"] == "Not assessed")
pls_quality_summary <- structural_canvas_pls_quality_status_summary(pls_quality)
stopifnot(grepl("Quality status: OK=", pls_quality_summary, fixed = TRUE))
stopifnot(grepl("; Descriptive only=", pls_quality_summary, fixed = TRUE))
stopifnot(grepl("; Not assessed=", pls_quality_summary, fixed = TRUE))
pls_quality_review <- structural_canvas_pls_quality_review_rows(pls_quality)
stopifnot(all(c("Priority", "Action", "Item", "Value", "Guidance") %in% names(pls_quality_review)))
stopifnot(nrow(pls_quality_review) == sum(pls_quality$Status == "Review"))
stopifnot(all(pls_quality_review$Priority %in% c("Critical", "Major", "Advisory")))
stopifnot(all(pls_quality_review$Action %in% c("Resolve before reporting", "Resolve or justify", "Document limitation")))
pls_readiness <- structural_canvas_pls_quality_reporting_readiness(pls_quality)
stopifnot(grepl("Reporting readiness:", pls_readiness, fixed = TRUE))
pls_overview <- structural_canvas_result_table("overview", pls_result, "plssem", labels_fn, language_fn)
pls_fit_diagnostics <- structural_canvas_pls_fit_diagnostics_table(pls_bundle)
pls_fit <- structural_canvas_result_table("fit", pls_result, "plssem", labels_fn, language_fn)
pls_fit_guide <- structural_canvas_result_table("fit_guide", pls_result, "plssem", labels_fn, language_fn)
pls_validity <- structural_canvas_result_table("validity", pls_result, "plssem", labels_fn, language_fn)
pls_validity_guide <- structural_canvas_result_table("validity_guide", pls_result, "plssem", labels_fn, language_fn)
pls_measurement <- structural_canvas_result_table("measurement", pls_result, "plssem", labels_fn, language_fn)
pls_measurement_guide <- structural_canvas_result_table("measurement_guide", pls_result, "plssem", labels_fn, language_fn)
pls_mi <- structural_canvas_result_table("mi", pls_result, "plssem", labels_fn, language_fn)
stopifnot(nrow(pls_overview) == 7L)
stopifnot("Item" %in% names(pls_overview))
stopifnot(all(c("Model", "srmr", "d_G", "d_ULS") %in% names(pls_fit_diagnostics)))
stopifnot(identical(pls_fit_diagnostics$Model, c("pls", "plsc")))
stopifnot(all(nzchar(pls_fit_diagnostics$srmr)), all(nzchar(pls_fit_diagnostics$d_G)), all(nzchar(pls_fit_diagnostics$d_ULS)))
stopifnot(all(c("Path", "B", "Boot SE", "z", "p", "f2", "R2AdjR2", "Inner VIF") %in% names(pls_fit)))
stopifnot(!"Score-CV Q2" %in% names(pls_fit))
stopifnot(!any(c("Effect", "Outcome", "Predictor") %in% names(pls_fit)))
stopifnot(any(pls_fit$Path == "eta1 → eta2"))
stopifnot(identical(pls_fit$Path, sort(pls_fit$Path, method = "radix")))
stopifnot(any(nzchar(pls_fit[["B"]])))
stopifnot(all(c("Effect", "Outcome", "Predictor", "f2", "f2 size", "Inner VIF") %in% names(pls_fit_guide)))
stopifnot(!any(c("Score-CV q2", "Score-CV q2 size") %in% names(pls_fit_guide)))
stopifnot(nzchar(pls_fit_guide$f2[[1L]]))
stopifnot(nzchar(pls_fit_guide[["f2 size"]][[1L]]))
stopifnot(grepl("Descriptive:", pls_fit_guide[["f2 size"]][[1L]], fixed = TRUE))
stopifnot(all(c("Construct", "alpha", "rhoA", "rhoC", "AVE", "sqrt(AVE)", "Max HTMT") %in% names(pls_validity)))
stopifnot(!any(c("Construct type", "Mode", "Evidence role") %in% names(pls_validity)))
stopifnot(all(c("Construct", "Construct type", "Mode", "Evidence role", "Max HTMT CI lower", "Max HTMT CI upper", "Max HTMT p", "Fornell-Larcker") %in% names(pls_validity_guide)))
stopifnot(all(grepl("score-proxy", pls_validity_guide$`Evidence role`, fixed = TRUE)))
stopifnot(nrow(pls_validity) >= 2L)
stopifnot(all(c("Construct", "Indicator", "loading/weight", "Boot SE", "Boot 95% CI lower", "Boot 95% CI upper", "Boot t", "Boot p", "Item VIF", "Mode") %in% names(pls_measurement)))
stopifnot(nrow(pls_measurement) >= 6L)
pls_htmt <- structural_canvas_result_table("pls_htmt", pls_result, "plssem", labels_fn, language_fn)
stopifnot(nrow(pls_htmt) >= 2L)
stopifnot("Construct" %in% names(pls_htmt))
stopifnot(all(c("Construct", "Indicator", "Loading", "Weight", "Item VIF", "Max cross-loading", "Mode") %in% names(pls_measurement_guide)))
stopifnot(any(nzchar(pls_measurement_guide[["Item VIF"]])))
stopifnot(nrow(pls_mi) == 0L)

pls_snapshot <- structural_canvas_result_snapshot(snapshot, pls$fit, "beta")
stopifnot(any(vapply(pls_snapshot$nodes, function(node) {
  identical(node$role, "latent") && is.list(node$resultStatsValues) && length(node$resultStatsValues) > 0L
}, logical(1))))
pls_labels <- vapply(pls_snapshot$edges, function(edge) as.character(edge$label %||% ""), character(1))
stopifnot(any(nzchar(pls_labels)))
pls_label_positions <- stats::setNames(vapply(pls_snapshot$edges, function(edge) as.numeric(edge$labelPosition %||% NA_real_), numeric(1)), vapply(pls_snapshot$edges, function(edge) as.character(edge$id), character(1)))
stopifnot(all(pls_label_positions[paste0("e", 1:6)] == 38), pls_label_positions[["p1"]] == 50)

pls_covariance_snapshot <- snapshot
pls_covariance_snapshot$edges <- c(pls_covariance_snapshot$edges, list(list(id = "cov12", from = "lv1", to = "lv2", kind = "covariance")))
pls_covariance <- run_structural_canvas_analysis(pls_covariance_snapshot, data, "plssem", estimator = "PLS")
stopifnot(inherits(pls_covariance$fit, "pls_model"))
stopifnot(!grepl("~~", pls_covariance$syntax, fixed = TRUE))
stopifnot(identical(pls_covariance$ignored_covariances, "eta1 ~~ eta2"))
pls_covariance_bundle <- list(
  fit = pls_covariance$fit,
  syntax = pls_covariance$syntax,
  snapshot = pls_covariance_snapshot,
  diagnostics = pls_covariance,
  estimator = "PLS"
)
pls_covariance_result <- function() pls_covariance_bundle
pls_covariance_overview <- structural_canvas_result_table("overview", pls_covariance_result, "plssem", labels_fn, language_fn)
stopifnot(nrow(pls_covariance_overview) == 8L)
stopifnot(pls_covariance_overview$Value[pls_covariance_overview$Item == "Ignored covariance paths"] == "eta1 ~~ eta2")

pls_mediation <- run_structural_canvas_analysis(mediation_snapshot, mediation_data, "plssem", estimator = "PLS")
stopifnot(inherits(pls_mediation$fit, "pls_model"))
pls_mediation_bundle <- list(
  fit = pls_mediation$fit,
  syntax = pls_mediation$syntax,
  snapshot = mediation_snapshot,
  diagnostics = pls_mediation,
  estimator = "PLS"
)
pls_mediation_result <- function() pls_mediation_bundle
pls_mediation_fit <- structural_canvas_result_table("fit", pls_mediation_result, "plssem", labels_fn, language_fn)
pls_mediation_fit_guide <- structural_canvas_result_table("fit_guide", pls_mediation_result, "plssem", labels_fn, language_fn)
mediation_path_row <- pls_mediation_fit$Path == "etaA → etaC"
stopifnot(any(mediation_path_row))
stopifnot(any(nzchar(pls_mediation_fit[["B"]][mediation_path_row])))
stopifnot(any(nzchar(pls_mediation_fit_guide[["Inner VIF"]][pls_mediation_fit_guide$Effect == "Direct" & pls_mediation_fit_guide$Outcome == "etaC"])))

pls_options <- structural_canvas_execute_settings(
  settings = list(pls_bootstrap = 5000L, pls_seed = 13579L, pls_predict_folds = 5L, pls_predict_reps = 1L, sampling_design = "independent_cross_sectional"),
  input = list(),
  prefix = "structural_plssem"
)
stopifnot(pls_options$pls_bootstrap == 5000L)
stopifnot(pls_options$pls_seed == 13579L)
stopifnot(pls_options$pls_predict_folds == 5L)
stopifnot(pls_options$pls_predict_reps == 1L)
stopifnot(is.finite(pls_options$pls_predict_seed))
stopifnot(pls_options$sampling_design == "independent_cross_sectional")
undeclared_options <- structural_canvas_execute_settings(settings = list(), input = list(), prefix = "structural_plssem")
stopifnot(identical(undeclared_options$sampling_design, "not_declared"))
stopifnot(identical(undeclared_options$estimator_recommendation_confirmed, FALSE))
confirmed_options <- structural_canvas_execute_settings(settings = list(estimator_recommendation_confirmed = TRUE), input = list(), prefix = "structural_plssem")
stopifnot(identical(confirmed_options$estimator_recommendation_confirmed, TRUE))
cbsem_default_options <- structural_canvas_execute_settings(settings = list(), input = list(), prefix = "structural_cbsem")
stopifnot(identical(cbsem_default_options$sampling_design, "not_declared"))
stopifnot(identical(cbsem_default_options$effect_bootstrap, 5000L))
stopifnot(identical(cbsem_default_options$htmt_bootstrap, 5000L))
stopifnot(identical(cbsem_default_options$htmt_ci_method, "bias_corrected"))
sampling_gate <- structural_canvas_sampling_design_gate(pls_options$sampling_design)
stopifnot(isTRUE(sampling_gate$supported), grepl("independent-observation", sampling_gate$reason, fixed = TRUE))
for (unsupported_design in c("not_declared", "clustered", "complex_survey", "longitudinal_repeated")) {
  gate_error <- tryCatch(structural_canvas_sampling_design_gate(unsupported_design), error = identity)
  stopifnot(inherits(gate_error, "error"), grepl("Sampling-design gate blocked estimation", conditionMessage(gate_error), fixed = TRUE))
}

set.seed(1618L)
pls_predict_rng_before <- .Random.seed
pls_predict_kind_before <- RNGkind()
pls_predict <- structural_canvas_run_pls_predict("plssem", 5L, 5L, pls, 20260821L)
pls_predict_repeat <- structural_canvas_run_pls_predict("plssem", 5L, 5L, pls, 20260821L)
stopifnot(is.list(pls_predict))
stopifnot(pls_predict$folds == 5L)
stopifnot(
  pls_predict$reps == 5L, pls_predict$seed == 20260821L,
  identical(pls_predict$rng, "L'Ecuyer-CMRG independent streams"),
  length(pls_predict$repetition_summaries) == 5L,
  identical(pls_predict$repetition_summaries, pls_predict_repeat$repetition_summaries),
  identical(.Random.seed, pls_predict_rng_before),
  identical(RNGkind(), pls_predict_kind_before)
)
pls_predict_tables <- structural_canvas_pls_predict_tables(pls_predict)
stopifnot(nrow(pls_predict_tables$items) >= 2L)
stopifnot(all(c("Indicator", "Metric", "PLS out-of-sample", "LM benchmark", "PLS - LM", "PLS - LM SD", "PLS lower %", "Assessment") %in% names(pls_predict_tables$items)))
stopifnot(any(is.finite(pls_predict_tables$items[["PLS - LM SD"]])))
stopifnot(all(c("Construct", "IS_MSE", "IS_MAE", "OOS_MSE", "OOS_MAE", "overfit") %in% names(pls_predict_tables$constructs)))
pls_predict_bundle <- pls_bundle
pls_predict_bundle$pls_predict_result <- pls_predict
pls_predict_bundle$pls_predict_folds <- 5L
pls_predict_bundle$pls_predict_reps <- 5L
pls_predict_bundle$pls_predict_seed <- 20260821L
pls_predict_bundle$analysis_data <- data
pls_predict_bundle$sampling_design <- "independent_cross_sectional"
pls_predict_audit <- structural_canvas_audit_manifest(pls_predict_bundle, "plssem")
pls_predict_warning <- pls_predict_audit$warnings[pls_predict_audit$warnings$Category == "Prediction", , drop = FALSE]
pls_predict_four_bundle <- pls_predict_bundle
pls_predict_four_bundle$pls_predict_result$reps <- 4L
pls_predict_four_warning <- structural_canvas_audit_warnings(pls_predict_four_bundle, "plssem", pls_predict_four_bundle$diagnostics)
pls_predict_four_warning <- pls_predict_four_warning[pls_predict_four_warning$Category == "Prediction", , drop = FALSE]
pls_predict_ten_bundle <- pls_predict_bundle
pls_predict_ten_bundle$pls_predict_result$reps <- 10L
pls_predict_ten_warning <- structural_canvas_audit_warnings(pls_predict_ten_bundle, "plssem", pls_predict_ten_bundle$diagnostics)
pls_predict_ten_warning <- pls_predict_ten_warning[pls_predict_ten_warning$Category == "Prediction", , drop = FALSE]
stopifnot(
  identical(pls_predict_audit$resampling$pls_predict$rng, "L'Ecuyer-CMRG independent streams"),
  nrow(pls_predict_warning) == 1L, identical(pls_predict_warning$Severity[[1L]], "Advisory"),
  grepl("recommended stability setting of 10", pls_predict_warning$Message[[1L]], fixed = TRUE),
  nrow(pls_predict_four_warning) == 1L, identical(pls_predict_four_warning$Severity[[1L]], "Major"),
  nrow(pls_predict_ten_warning) == 0L
)
pls_predict_quality <- structural_canvas_pls_quality_rows(pls_predict_bundle)
stopifnot(grepl("indicator metrics favor PLS over LM", pls_predict_quality$Value[pls_predict_quality$Item == "PLSpredict summary"], fixed = TRUE))
stopifnot(pls_predict_quality$Status[pls_predict_quality$Item == "PLSpredict summary"] == "Descriptive only")
stopifnot(
  structural_canvas_pls_quality_status("PLSpredict summary", "0/6 indicator metrics favor PLS over LM") == "Descriptive only",
  structural_canvas_pls_quality_status("PLSpredict summary", "1/6 indicator metrics favor PLS over LM") == "Descriptive only",
  structural_canvas_pls_quality_status("PLSpredict summary", "6/6 indicator metrics favor PLS over LM") == "Descriptive only"
)

pls_bootstrap <- structural_canvas_run_pls_bootstrap("plssem", 30L, pls, 24680L)
stopifnot(is.list(pls_bootstrap))
stopifnot(length(pls_bootstrap$bootstrapped_paths) > 0L)
stopifnot(length(pls_bootstrap$bootstrapped_loadings) > 0L)
stopifnot(length(pls_bootstrap$bootstrapped_weights) > 0L)
set.seed(8128L)
pls_rng_before <- .Random.seed
pls_rng_kind_before <- RNGkind()
pls_rng_streams_first <- structural_canvas_rng_streams(4L, 24680L)
pls_rng_streams_second <- structural_canvas_rng_streams(4L, 24680L)
stopifnot(
  identical(pls_rng_streams_first, pls_rng_streams_second),
  length(unique(vapply(pls_rng_streams_first, paste, collapse = ":", FUN.VALUE = character(1)))) == 4L,
  all(vapply(pls_rng_streams_first, function(stream) length(stream) == 7L, logical(1))),
  identical(.Random.seed, pls_rng_before),
  identical(RNGkind(), pls_rng_kind_before)
)
set.seed(2718L)
plsc_rng_before <- .Random.seed
plsc_bootstrap <- structural_canvas_run_pls_bootstrap("plssem", 5L, plsc, 13579L)
plsc_bootstrap_repeat <- structural_canvas_run_pls_bootstrap("plssem", 5L, plsc, 13579L)
stopifnot(is.list(plsc_bootstrap))
stopifnot(identical(plsc_bootstrap$requested_nboot, 5L))
stopifnot(all(c("timeout_failures", "estimation_failures") %in% names(plsc_bootstrap)))
stopifnot(length(plsc_bootstrap$bootstrapped_paths) > 0L)
stopifnot(length(plsc_bootstrap$bootstrapped_loadings) > 0L)
stopifnot(length(plsc_bootstrap$bootstrapped_weights) > 0L)
stopifnot(
  identical(plsc_bootstrap$bootstrapped_paths, plsc_bootstrap_repeat$bootstrapped_paths),
  identical(plsc_bootstrap$bootstrapped_loadings, plsc_bootstrap_repeat$bootstrapped_loadings),
  identical(.Random.seed, plsc_rng_before)
)
pls_mediation_bootstrap <- structural_canvas_run_pls_bootstrap("plssem", 20L, pls_mediation, 13579L)
stopifnot("bootstrapped_total_indirect_paths" %in% names(pls_mediation_bootstrap))
pls_boot_bundle <- pls_bundle
pls_boot_bundle$pls_bootstrap <- 30L
pls_boot_bundle$pls_seed <- 24680L
pls_boot_bundle$pls_bootstrap_result <- pls_bootstrap
pls_boot_result <- function() pls_boot_bundle
pls_boot_reporting <- structural_canvas_reporting_context_rows(pls_boot_bundle, "plssem")
stopifnot(grepl("PLS bootstrap R=30", pls_boot_reporting$Value[pls_boot_reporting$Item == "Bootstrap settings"], fixed = TRUE))
stopifnot(grepl("seed=24680", pls_boot_reporting$Value[pls_boot_reporting$Item == "Bootstrap settings"], fixed = TRUE))
pls_boot_fit <- structural_canvas_result_table("fit", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_fit_bootstrap <- structural_canvas_result_table("fit_bootstrap", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_validity <- structural_canvas_result_table("validity", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_validity_guide <- structural_canvas_result_table("validity_guide", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_measurement <- structural_canvas_result_table("measurement", pls_boot_result, "plssem", labels_fn, language_fn)
pls_boot_measurement_bootstrap <- structural_canvas_result_table("measurement_bootstrap", pls_boot_result, "plssem", labels_fn, language_fn)
stopifnot(all(c("Indirect effect CI lower", "Indirect effect CI upper", "Indirect effect t", "Indirect effect p", "Indirect effect BH-adjusted p", "Total effect CI lower", "Total effect CI upper", "Total effect t", "Total effect p", "Total effect BH-adjusted p", "Boot CI lower", "Boot CI upper", "Boot t", "Boot p", "Boot BH-adjusted p") %in% names(pls_boot_fit_bootstrap)))
stopifnot(any(nzchar(pls_boot_fit_bootstrap[["Total effect p"]])))
stopifnot(any(nzchar(pls_boot_fit_bootstrap[["Boot t"]])))
stopifnot(any(nzchar(pls_boot_fit_bootstrap[["Boot p"]])))
pls_mediation_boot_bundle <- pls_mediation_bundle
pls_mediation_boot_bundle$pls_bootstrap_result <- pls_mediation_bootstrap
pls_mediation_boot_result <- function() pls_mediation_boot_bundle
pls_mediation_boot_fit <- structural_canvas_result_table("fit_bootstrap", pls_mediation_boot_result, "plssem", labels_fn, language_fn)
pls_mediation_indirect_row <- pls_mediation_boot_fit[pls_mediation_boot_fit$Effect == "Indirect" & pls_mediation_boot_fit$Outcome == "etaC" & pls_mediation_boot_fit$Predictor == "etaA", , drop = FALSE]
stopifnot(nrow(pls_mediation_indirect_row) == 1L)
stopifnot(nzchar(pls_mediation_indirect_row[["Indirect effect CI lower"]][[1L]]), nzchar(pls_mediation_indirect_row[["Indirect effect CI upper"]][[1L]]), nzchar(pls_mediation_indirect_row[["Indirect effect p"]][[1L]]))
stopifnot(all(c("Max HTMT CI lower", "Max HTMT CI upper", "Max HTMT p") %in% names(pls_boot_validity_guide)))
stopifnot(any(nzchar(pls_boot_validity_guide[["Max HTMT p"]])))
stopifnot(all(c("Loading CI lower", "Loading CI upper", "Loading t", "Loading p", "Loading BH-adjusted p", "Weight CI lower", "Weight CI upper", "Weight t", "Weight p", "Weight BH-adjusted p") %in% names(pls_boot_measurement_bootstrap)))
stopifnot(any(nzchar(pls_boot_measurement_bootstrap[["Loading t"]])))
stopifnot(any(nzchar(pls_boot_measurement_bootstrap[["Loading p"]])))

execution_state <- new.env(parent = emptyenv())
execution_state$value <- NULL
execution_state$message <- NULL
fit_result_state <- function(value) {
  if (missing(value)) return(execution_state$value)
  execution_state$value <- value
}
session <- list(sendCustomMessage = function(type, message) {
  execution_state$message <- list(type = type, message = message)
})
executed_cbsem <- structural_canvas_execute_analysis(
  snapshot,
  settings = list(estimator = "ML", missing = "fiml", sampling_design = "independent_cross_sectional", effect_bootstrap = 0L),
  input = list(),
  session = session,
  dataset_fn = function() data,
  variable_table_fn = function() variable_table,
  analysis_type = "cbsem",
  prefix = "structural_cbsem",
  fit_result = fit_result_state
)
stopifnot(inherits(executed_cbsem$fit, "lavaan"))
stopifnot(inherits(fit_result_state()$fit, "lavaan"))
stopifnot(identical(fit_result_state()$estimator, "ML"))
stopifnot(identical(execution_state$message$type, "custom-model-canvas-result"))
stopifnot(any(nzchar(vapply(execution_state$message$message$result$edges, function(edge) as.character(edge$label %||% ""), character(1)))))
execution_state$value <- NULL
execution_state$message <- NULL
executed_cbsem_group <- structural_canvas_execute_analysis(
  snapshot,
  settings = list(estimator = "ML", missing = "fiml", sampling_design = "independent_cross_sectional", effect_bootstrap = 0L, invariance_enabled = TRUE, invariance_group = "group"),
  input = list(),
  session = session,
  dataset_fn = function() group_data,
  variable_table_fn = function() data.frame(name = names(group_data), measurement = c(rep("scale", 6L), "nominal"), stringsAsFactors = FALSE),
  analysis_type = "cbsem",
  prefix = "structural_cbsem",
  fit_result = fit_result_state
)
stopifnot(inherits(executed_cbsem_group$fit, "lavaan"))
stopifnot(identical(fit_result_state()$invariance_result$type, "structural_path_comparison"))
stopifnot(nrow(fit_result_state()$invariance_result$path_differences) == 1L)
stopifnot(identical(execution_state$message$type, "custom-model-canvas-result"))
execution_state$value <- NULL
execution_state$message <- NULL
executed_sem <- structural_canvas_execute_analysis(
  snapshot,
  settings = list(estimator = "ML", missing = "fiml", sampling_design = "independent_cross_sectional", effect_bootstrap = 0L),
  input = list(),
  session = session,
  dataset_fn = function() data,
  variable_table_fn = function() variable_table,
  analysis_type = "sem",
  prefix = "structural_sem",
  fit_result = fit_result_state
)
stopifnot(inherits(executed_sem$fit, "lavaan"))
stopifnot(inherits(fit_result_state()$fit, "lavaan"))
stopifnot(identical(fit_result_state()$estimator, "ML"))
stopifnot(identical(execution_state$message$type, "custom-model-canvas-result"))
stopifnot(any(nzchar(vapply(execution_state$message$message$result$edges, function(edge) as.character(edge$label %||% ""), character(1)))))
execution_state$value <- NULL
execution_state$message <- NULL
executed <- structural_canvas_execute_analysis(
  snapshot,
  settings = list(estimator = "PLS", sampling_design = "independent_cross_sectional", pls_bootstrap = 0L, pls_predict_folds = 5L, pls_predict_reps = 1L),
  input = list(),
  session = session,
  dataset_fn = function() data,
  variable_table_fn = function() variable_table,
  analysis_type = "plssem",
  prefix = "structural_plssem",
  fit_result = fit_result_state
)
stopifnot(inherits(executed$fit, "pls_model"))
stopifnot(inherits(fit_result_state()$fit, "pls_model"))
stopifnot(is.list(fit_result_state()$pls_predict_result))
stopifnot(fit_result_state()$pls_predict_result$folds == 5L)
pls_predict_reporting <- structural_canvas_reporting_context_rows(fit_result_state(), "plssem")
stopifnot(grepl("Executed: folds=5, reps=1", pls_predict_reporting$Value[pls_predict_reporting$Item == "PLSpredict setting"], fixed = TRUE))
stopifnot(identical(execution_state$message$type, "custom-model-canvas-result"))
stopifnot(any(nzchar(vapply(execution_state$message$message$result$edges, function(edge) as.character(edge$label %||% ""), character(1)))))
execution_state$value <- NULL
execution_state$message <- NULL
executed_covariance <- structural_canvas_execute_analysis(
  pls_covariance_snapshot,
  settings = list(estimator = "PLS", sampling_design = "independent_cross_sectional", pls_bootstrap = 0L),
  input = list(),
  session = session,
  dataset_fn = function() data,
  variable_table_fn = function() variable_table,
  analysis_type = "plssem",
  prefix = "structural_plssem",
  fit_result = fit_result_state
)
stopifnot(inherits(executed_covariance$fit, "pls_model"))
stopifnot(identical(fit_result_state()$diagnostics$ignored_covariances, "eta1 ~~ eta2"))
stopifnot(identical(execution_state$message$type, "custom-model-canvas-result"))

formative_snapshot <- snapshot
formative_snapshot$nodes[[1]]$measurementMode <- "formative"
pls_formative <- run_structural_canvas_analysis(formative_snapshot, data, "plssem", estimator = "PLS")
stopifnot(inherits(pls_formative$fit, "pls_model"))
automatic_mixed <- run_structural_canvas_analysis(formative_snapshot, data, "plssem", estimator = "AUTO")
stopifnot(
  identical(automatic_mixed$estimator, "PLSc"),
  identical(automatic_mixed$plsc_corrected_constructs, "eta2"),
  identical(automatic_mixed$plsc_uncorrected_composites, "eta1")
)
stopifnot(grepl("eta1 <~", pls_formative$syntax, fixed = TRUE))
stopifnot(
  identical(pls_formative$resolved_construct_specification$effective_weighting[pls_formative$resolved_construct_specification$name == "eta1"], "Mode B"),
  all(pls_formative$fit$measurement_model$composite[c(FALSE, FALSE, TRUE)] == "B")
)
pls_formative_bundle <- list(
  fit = pls_formative$fit,
  syntax = pls_formative$syntax,
  snapshot = formative_snapshot,
  diagnostics = pls_formative,
  estimator = "PLS"
)
pls_formative_result <- function() pls_formative_bundle
pls_formative_quality <- structural_canvas_pls_quality_rows(pls_formative_bundle)
pls_formative_measurement <- structural_canvas_result_table("measurement", pls_formative_result, "plssem", labels_fn, language_fn)
pls_formative_measurement_guide <- structural_canvas_result_table("measurement_guide", pls_formative_result, "plssem", labels_fn, language_fn)
pls_formative_validity <- structural_canvas_result_table("validity", pls_formative_result, "plssem", labels_fn, language_fn)
pls_formative_validity_guide <- structural_canvas_result_table("validity_guide", pls_formative_result, "plssem", labels_fn, language_fn)
stopifnot(any(pls_formative_measurement$Construct == "eta1" & pls_formative_measurement$Mode == "Formative"))
stopifnot(any(grepl("^Formative evidence: eta1$", pls_formative_quality$Item) & pls_formative_quality$Status == "Review"))
stopifnot(any(pls_formative_measurement$Construct == "eta2" & pls_formative_measurement$Mode == "Reflective"))
stopifnot(any(nzchar(pls_formative_measurement[["loading/weight"]][pls_formative_measurement$Mode == "Formative"])))
stopifnot(any(nzchar(pls_formative_measurement_guide[["Item VIF"]][pls_formative_measurement_guide$Mode == "Formative"])))
stopifnot(!"Mode" %in% names(pls_formative_validity))
stopifnot(any(pls_formative_validity_guide$Construct == "eta1" & pls_formative_validity_guide$Mode == "Formative"))
formative_validity <- pls_formative_validity[pls_formative_validity$Construct == "eta1", , drop = FALSE]
stopifnot(all(formative_validity[, c("alpha", "rhoA", "rhoC", "AVE", "sqrt(AVE)", "Max HTMT")] == "N/A"))
formative_validity_guide <- pls_formative_validity_guide[pls_formative_validity_guide$Mode == "Formative", , drop = FALSE]
stopifnot(all(grepl("not applicable", formative_validity_guide$`Evidence role`, fixed = TRUE)))
stopifnot(all(formative_validity_guide$`Fornell-Larcker` == "N/A - formative"))
pls_formative_htmt <- structural_canvas_result_table("pls_htmt", pls_formative_result, "plssem", labels_fn, language_fn)
stopifnot(any(pls_formative_htmt == "N/A"))

reflective_composite_bundle <- pls_bundle
reflective_composite_bundle$snapshot$nodes[[1L]]$constructType <- "composite"
reflective_composite_result <- function() reflective_composite_bundle
reflective_composite_validity <- structural_canvas_result_table("validity", reflective_composite_result, "plssem", labels_fn, language_fn)
reflective_composite_validity_guide <- structural_canvas_result_table("validity_guide", reflective_composite_result, "plssem", labels_fn, language_fn)
reflective_composite_row <- reflective_composite_validity[reflective_composite_validity$Construct == "eta1", , drop = FALSE]
reflective_composite_guide_row <- reflective_composite_validity_guide[reflective_composite_validity_guide$Construct == "eta1", , drop = FALSE]
stopifnot(
  reflective_composite_guide_row$`Construct type` == "Composite",
  reflective_composite_guide_row$Mode == "Reflective",
  grepl("do not infer a latent common cause", reflective_composite_guide_row$`Evidence role`, fixed = TRUE),
  reflective_composite_row$AVE != "N/A"
)
pls_formative_fit_diagnostics <- structural_canvas_pls_fit_diagnostics_table(pls_formative_bundle)
stopifnot(
  nrow(pls_formative_fit_diagnostics) == 2L,
  identical(pls_formative_fit_diagnostics$Model, c("pls", "plsc")),
  grepl("common factors corrected", pls_formative_fit_diagnostics$Basis[[2L]], fixed = TRUE)
)

missing_latent_range <- structural_canvas_moderation_update_factor_score_ranges(
  list(list(moderator_role = "latent", moderator = "missing", moderator_min = -2, moderator_max = 2)),
  structure(list(), class = "not_lavaan")
)[[1L]]
stopifnot(isFALSE(missing_latent_range$moderator_range_available))
stopifnot(!is.finite(missing_latent_range$moderator_min))
stopifnot(!is.finite(missing_latent_range$moderator_max))

cat("SEM canvas validations passed.\n")
