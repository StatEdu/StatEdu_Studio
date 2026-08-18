source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("htmltools", quietly = TRUE))

canvas_js_source <- paste(readLines(file.path("www", "model-canvas", "canvas.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
bridge_js_source <- paste(readLines(file.path("www", "model-canvas", "shiny-bridge.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
nodes_js_source <- paste(readLines(file.path("www", "model-canvas", "nodes.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
state_js_source <- paste(readLines(file.path("www", "model-canvas", "state.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
dialogs_js_source <- paste(readLines(file.path("www", "model-canvas", "dialogs.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
layout_js_source <- paste(readLines(file.path("www", "model-canvas", "layout.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
toolbar_js_source <- paste(readLines(file.path("www", "model-canvas", "toolbar.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
edges_js_source <- paste(readLines(file.path("www", "model-canvas", "edges.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
canvas_css_source <- paste(readLines(file.path("www", "model-canvas", "canvas.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
structural_core_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_core.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
structural_notifications_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_execute_notifications.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
structural_toolbar_icons_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_toolbar_icons.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("Latent construct correlations, reliability, and convergent/discriminant validity", ui_source, fixed = TRUE),
  length(gregexpr('class = "table-responsive"', ui_source, fixed = TRUE)[[1L]]) >= 8L,
  grepl("STATEDU_CAPTURE_CFA_MODEL_FILE", ui_source, fixed = TRUE),
  grepl("data-initial-snapshot", ui_source, fixed = TRUE),
  grepl("data-initial-run", ui_source, fixed = TRUE),
  grepl("parseInitialSnapshot", canvas_js_source, fixed = TRUE),
  grepl("window.StatEduModelCanvas.state.restore(instance.state, initialSnapshot)", canvas_js_source, fixed = TRUE),
  grepl("instance.state.validation = window.StatEduModelCanvas.state.clone(instance.validation);", canvas_js_source, fixed = TRUE),
  grepl("window.Shiny && typeof window.Shiny.setInputValue === \"function\"", canvas_js_source, fixed = TRUE),
  grepl("function autoLayoutModel(instance)", canvas_js_source, fixed = TRUE),
  grepl("window.StatEduModelCanvas.layout.reflowRoleLayout(instance.state.nodes, instance.state.style)", canvas_js_source, fixed = TRUE),
  grepl("autoLayout: autoLayoutModel", canvas_js_source, fixed = TRUE),
  grepl("\"fromSide\", \"toSide\", \"fixedCenter\", \"directAnchors\", \"labelPosition\"", bridge_js_source, fixed = TRUE),
  grepl("incomingResult = syncVisualEdits(incomingResult, currentResult)", bridge_js_source, fixed = TRUE),
  grepl("resultStatsOffsetX", bridge_js_source, fixed = TRUE),
  grepl("autoLabelPosition", edges_js_source, fixed = TRUE),
  grepl("owner.labelManualPosition = true", edges_js_source, fixed = TRUE),
  grepl("custom-model-edge-label-hit", edges_js_source, fixed = TRUE),
  grepl("data-label-auto-shift-x", edges_js_source, fixed = TRUE),
  grepl("if (!matches.length) return initialLabel", canvas_js_source, fixed = TRUE),
  grepl("marqueeSelectionMode && !labelElement && !nodeElement", canvas_js_source, fixed = TRUE),
  grepl('if (window.StatEduModelCanvas.nodes.isViewingResult(instance))', canvas_js_source, fixed = TRUE),
  grepl('event.target.closest(".structural-latent-statistics")', canvas_js_source, fixed = TRUE),
  !grepl("instance.paper.setPointerCapture", nodes_js_source, fixed = TRUE),
  grepl('custom-model-node-" + node.role', nodes_js_source, fixed = TRUE),
  grepl('if (isViewingResult(instance)) {', nodes_js_source, fixed = TRUE),
  grepl("if (usesStructuralMovePolicy(instance)) {", nodes_js_source, fixed = TRUE),
  grepl('var singleLatent = structuralMovePolicy && initialPositions.length === 1 && item.node.role === "latent"', nodes_js_source, fixed = TRUE),
  grepl("window.StatEduModelCanvas.nodes.startDrag(instance, event, nodeElement);", canvas_js_source, fixed = TRUE),
  grepl("if (!isViewingResult(instance)) return", nodes_js_source, fixed = TRUE),
  grepl("instance.selectedLatentStatsNodeId = node.id", nodes_js_source, fixed = TRUE),
  grepl("instance.state.selectedNodeIds = []", nodes_js_source, fixed = TRUE),
  grepl("data-latent-stat", toolbar_js_source, fixed = TRUE),
  grepl("var validationItems = instance.validation && instance.validation.byNode", nodes_js_source, fixed = TRUE),
  grepl('constructType: "commonFactor"', nodes_js_source, fixed = TRUE),
  !grepl('constructType: "unspecified"', nodes_js_source, fixed = TRUE),
  grepl('latent.constructType = "commonFactor"', canvas_js_source, fixed = TRUE),
  grepl('fixedCenter: true, directAnchors: false', canvas_js_source, fixed = TRUE),
  grepl('edge.directAnchors = false', canvas_js_source, fixed = TRUE),
  grepl('node.constructType === "unspecified" && !node.advancedConstructSpecification', state_js_source, fixed = TRUE),
  grepl('migrations.push("default_construct_type_applied")', state_js_source, fixed = TRUE),
  grepl("state.latentStatsSelection", nodes_js_source, fixed = TRUE),
  grepl("if (window.showOpenFilePicker)", dialogs_js_source, fixed = TRUE),
  grepl("types: filePickerTypes()", dialogs_js_source, fixed = TRUE),
  grepl("multiple: false", dialogs_js_source, fixed = TRUE),
  grepl("instance.root.classList.remove(\"is-viewing-result\", \"has-result\");", dialogs_js_source, fixed = TRUE),
  grepl("window.StatEduModelCanvas.canvas.render(instance);\n    var width = Number", dialogs_js_source, fixed = TRUE),
  grepl('accept: {"application/json": [".stmodel", ".studio", ".json"]}', dialogs_js_source, fixed = TRUE),
  grepl("window.StatEduModelCanvas.dialogs", dialogs_js_source, fixed = TRUE),
  grepl('replace(/^\\uFEFF/, "")', dialogs_js_source, fixed = TRUE),
  grepl("openModelFileFallback(instance)", dialogs_js_source, fixed = TRUE),
  grepl("if (window.showSaveFilePicker)", dialogs_js_source, fixed = TRUE),
  grepl("downloadText(timestampName(\"model-canvas\", \"stmodel\")", dialogs_js_source, fixed = TRUE),
  grepl("validation: clone(state.validation || null)", state_js_source, fixed = TRUE),
  grepl("changed = alignModerators(nodes, style) || changed;", layout_js_source, fixed = TRUE),
  grepl("if (action === \"autoLayout\") applicable = hasCanvasContent;", toolbar_js_source, fixed = TRUE),
  grepl("if (action === \"autoLayout\") window.StatEduModelCanvas.canvas.autoLayout(instance);", toolbar_js_source, fixed = TRUE),
  grepl("_reliability_ci_method", ui_source, fixed = TRUE),
  grepl("_htmt_ci_method", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_validity_outputs(\n    output, prefix, analysis_type, fit_result, manuscript_result_table, app_language_fn", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_latent_correlation_outputs(output, prefix, fit_result, app_language_fn)", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_validity_note_outputs(output, prefix, fit_result, result_table, app_language_fn)", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_factor_score_outputs(output, prefix, fit_result, app_language_fn)", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_reliability_bootstrap_outputs(output, prefix, fit_result, app_language_fn)", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_htmt_outputs(output, prefix, fit_result, result_table, app_language_fn)", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_local_fit_outputs(\n    output, prefix, fit_result, app_language_fn", ui_source, fixed = TRUE),
  grepl("structural_canvas_invariance_result_ui(fit_result(), statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  grepl("Structural path group comparison", ui_source, fixed = TRUE),
  grepl("structural_canvas_structural_path_group_comparison_ui", ui_source, fixed = TRUE),
  grepl("Free versus equal structural-path models", ui_source, fixed = TRUE),
  grepl("Path-level z tests are exploratory pairwise diagnostics", ui_source, fixed = TRUE),
  grepl("structural_canvas_rmsea_tests_result_ui(fit_result(), statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  grepl("structural_canvas_information_criteria_result_ui(fit_result(), statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  grepl("manuscript_result_table <- function(kind)", ui_source, fixed = TRUE),
  grepl("structural-information-criteria-table", ui_source, fixed = TRUE),
  grepl("structural-landscape-table-wrap", ui_source, fixed = TRUE),
  grepl("structural-fit-guidance-table", ui_source, fixed = TRUE),
  grepl("structural-rmsea-tests-table", ui_source, fixed = TRUE),
  grepl("structural-landscape-table structural-mi-table", ui_source, fixed = TRUE),
  grepl("Guide for Table 2: Likelihood-based information criteria", ui_source, fixed = TRUE),
  grepl("Guide for Table 3: HTMT detailed criteria", ui_source, fixed = TRUE),
  grepl("_result_mi_section", ui_source, fixed = TRUE),
  grepl("structural_canvas_risk_diagnostics_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  grepl("structural_canvas_missing_outliers_result_ui(fit_result(), dataset_fn(), analysis_type, statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  grepl("FIML 대신 pairwise 결측 처리", ui_source, fixed = TRUE),
  grepl("pairwise missing-data handling instead of FIML", ui_source, fixed = TRUE),
  grepl("RMSEA 가설검정", ui_source, fixed = TRUE),
  grepl("우도 기반 정보기준", ui_source, fixed = TRUE),
  grepl("Bollen-Stine 부트스트랩 전반적 적합도 검정", ui_source, fixed = TRUE),
  grepl("데이터 및 모형 위험 진단", ui_source, fixed = TRUE),
  grepl("결측 자료 및 다변량 이상치", ui_source, fixed = TRUE),
  grepl("Mahalanobis 후보는 자료 오류", ui_source, fixed = TRUE),
  grepl("요인점수 품질", ui_source, fixed = TRUE),
  grepl("잠재상관 신뢰구간", ui_source, fixed = TRUE),
  grepl("MI p는 각 수정지수", ui_source, fixed = TRUE),
  grepl("Skipped unsafe는 더 높은 순위", ui_source, fixed = TRUE),
  grepl("structural-mi-table-scroll", ui_source, fixed = TRUE),
  grepl("structural-mi-theory-table", ui_source, fixed = TRUE),
  grepl("structural-mi-skipped-details-table", ui_source, fixed = TRUE),
  grepl("괄호 안 대각값은 sqrt(AVE)", ui_source, fixed = TRUE),
  grepl("순서형 지표에서는 AVE, CR, ω total", ui_source, fixed = TRUE),
  grepl("교차적재 지표", ui_source, fixed = TRUE),
  grepl("집단별 측정불변성", ui_source, fixed = TRUE),
  grepl("집단별 신뢰도 및 수렴타당도", ui_source, fixed = TRUE),
  grepl("특정 단계의 실패가 모수 자동 해제", ui_source, fixed = TRUE),
  grepl("AVE/신뢰도 부트스트랩 CI", ui_source, fixed = TRUE),
  grepl("다집단 표본통계에 대한 HTMT", ui_source, fixed = TRUE),
  grepl("Local fit diagnostics", ui_source, fixed = TRUE),
  grepl("Guide for Table 2: Additional fit indices not shown in Table 2", ui_source, fixed = TRUE),
  grepl("structural_canvas_additional_fit_indices_ui", ui_source, fixed = TRUE),
  grepl("structural-additional-fit-family", canvas_css_source, fixed = TRUE),
  grepl("structural-mi-table-scroll", canvas_css_source, fixed = TRUE),
  grepl("structural-landscape-table", canvas_css_source, fixed = TRUE),
  grepl("CFI = Comparative Fit Index", ui_source, fixed = TRUE),
  grepl("structural_canvas_measurement_html_table", ui_source, fixed = TRUE),
  grepl("고차요인 CFA 결과", ui_source, fixed = TRUE),
  grepl("omega-h는 적합된 고차요인 CFA 모형", ui_source, fixed = TRUE),
  grepl("Download analysis record", ui_source, fixed = TRUE),
  grepl("Download result tables", ui_source, fixed = TRUE),
  !grepl("ko <- FALSE", ui_source, fixed = TRUE),
  !grepl(intToUtf8(0xfffd), ui_source, fixed = TRUE)
)

old_capture_model <- Sys.getenv("STATEDU_CAPTURE_CFA_MODEL_FILE", unset = NA_character_)
old_structural_model <- Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_MODEL_FILE", unset = NA_character_)
old_capture_run <- Sys.getenv("STATEDU_CAPTURE_CFA_RUN", unset = NA_character_)
on.exit({
  if (is.na(old_capture_model)) Sys.unsetenv("STATEDU_CAPTURE_CFA_MODEL_FILE") else Sys.setenv(STATEDU_CAPTURE_CFA_MODEL_FILE = old_capture_model)
  if (is.na(old_structural_model)) Sys.unsetenv("STATEDU_CAPTURE_STRUCTURAL_MODEL_FILE") else Sys.setenv(STATEDU_CAPTURE_STRUCTURAL_MODEL_FILE = old_structural_model)
  if (is.na(old_capture_run)) Sys.unsetenv("STATEDU_CAPTURE_CFA_RUN") else Sys.setenv(STATEDU_CAPTURE_CFA_RUN = old_capture_run)
}, add = TRUE)
capture_model_file <- tempfile(fileext = ".stmodel")
jsonlite::write_json(
  list(nodes = list(list(id = "latent_1", role = "latent", name = "eta1")), edges = list()),
  capture_model_file,
  auto_unbox = TRUE,
  null = "null"
)
Sys.setenv(STATEDU_CAPTURE_CFA_MODEL_FILE = capture_model_file, STATEDU_CAPTURE_CFA_RUN = "yes")
capture_result <- structural_capture_initial_snapshot("cfa")
stopifnot(
  is.list(capture_result$snapshot),
  identical(capture_result$snapshot$nodes[[1L]]$id, "latent_1"),
  isTRUE(capture_result$auto_run),
  is.null(structural_capture_initial_snapshot("cbsem")$snapshot)
)

cfa_toolbar <- htmltools::renderTags(structural_equation_toolbar("cfa", "en"))$html
cfa_ids <- regmatches(cfa_toolbar, gregexpr('(^|[[:space:]])id="[^"]+"', cfa_toolbar, perl = TRUE))[[1L]]
cfa_ids <- sub('^.*id="([^"]+)".*$', "\\1", cfa_ids)
cfa_option_tab_links <- regmatches(
  cfa_toolbar,
  gregexpr('<a href="#[^"]+" data-toggle="tab"[^>]*data-value="[^"]+"[^>]*>', cfa_toolbar, perl = TRUE)
)[[1L]]
cfa_option_tab_values <- sub('^.*data-value="([^"]+)".*$', "\\1", cfa_option_tab_links)
cfa_option_tab_targets <- sub('^.*href="#([^"]+)".*$', "\\1", cfa_option_tab_links)
cfa_option_panes <- regmatches(
  cfa_toolbar,
  gregexpr('<div class="tab-pane[^"]*" data-value="[^"]+" id="[^"]+"', cfa_toolbar, perl = TRUE)
)[[1L]]
cfa_option_pane_values <- sub('^.*data-value="([^"]+)".*$', "\\1", cfa_option_panes)
cfa_option_pane_ids <- sub('^.*id="([^"]+)".*$', "\\1", cfa_option_panes)
stopifnot(
  !anyDuplicated(cfa_ids),
  grepl("structural-run-options-tabs", cfa_toolbar, fixed = TRUE),
  grepl('<ul class="nav nav-tabs"', cfa_toolbar, fixed = TRUE),
  grepl('<div class="tab-content"', cfa_toolbar, fixed = TRUE),
  identical(cfa_option_tab_values, c("Estimation", "Validity", "Diagnostics", "Common Method")),
  identical(cfa_option_pane_values, c("Estimation", "Validity", "Diagnostics", "Common Method")),
  identical(cfa_option_tab_targets, cfa_option_pane_ids),
  grepl('<li class="active">', cfa_toolbar, fixed = TRUE),
  grepl('<div class="tab-pane active" data-value="Estimation"', cfa_toolbar, fixed = TRUE),
  grepl("Estimation", cfa_toolbar, fixed = TRUE),
  grepl('selected = "independent_cross_sectional"', ui_source, fixed = TRUE),
  grepl("분석 전에 표집구조를 명시적으로 확인하십시오", ui_source, fixed = TRUE),
  grepl("Validity", cfa_toolbar, fixed = TRUE),
  grepl("Diagnostics", cfa_toolbar, fixed = TRUE),
  grepl("Common Method", cfa_toolbar, fixed = TRUE),
  grepl("Run common method bias diagnostics", cfa_toolbar, fixed = TRUE),
  grepl("Common method diagnostics", cfa_toolbar, fixed = TRUE),
  grepl("동일방법편의 진단", ui_source, fixed = TRUE),
  grepl("판정 요약", ui_source, fixed = TRUE),
  grepl("모형 차이 비교", ui_source, fixed = TRUE),
  grepl("Delta chisq", ui_source, fixed = TRUE),
  regexpr("모형 적합도 비교", ui_source, fixed = TRUE) < regexpr("모형 차이 비교", ui_source, fixed = TRUE),
  grepl("structural-result-coefficient-select", cfa_toolbar, fixed = TRUE),
  grepl("B(t)", cfa_toolbar, fixed = TRUE),
  grepl("beta(t)", cfa_toolbar, fixed = TRUE),
  grepl("B(Beta)", cfa_toolbar, fixed = TRUE),
  !grepl("Result diagram coefficient", cfa_toolbar, fixed = TRUE),
  grepl("structural_canvas_result_coefficient_mode", structural_core_source, fixed = TRUE),
  grepl("b_beta", structural_core_source, fixed = TRUE),
  grepl("structural_canvas_identification_issue_text", structural_notifications_source, fixed = TRUE),
  grepl("structural_canvas_error_message", structural_notifications_source, fixed = TRUE),
  grepl("Sampling-design gate blocked estimation", structural_notifications_source, fixed = TRUE),
  grepl("분석을 실행하려면 관측치와 표본설계 구조를 먼저 선택해야 합니다.", structural_notifications_source, fixed = TRUE),
  grepl("모형 식별성 점검 실패", structural_notifications_source, fixed = TRUE),
  grepl("현재 자동 식별 방식에서는 고차요인마다 하위요인이 최소 3개 필요합니다.", structural_notifications_source, fixed = TRUE),
  grepl("수정지수(MI)를 계산할 수 없습니다", structural_notifications_source, fixed = TRUE),
  grepl("structural_canvas_error_message(error, statedu_current_language(app_language_fn))", ui_source, fixed = TRUE),
  grepl("AVE/reliability bootstrap CI", cfa_toolbar, fixed = TRUE),
  grepl("structural_cfa_reliability_bootstrap", cfa_toolbar, fixed = TRUE),
  grepl("structural_cfa_htmt_bootstrap", cfa_toolbar, fixed = TRUE),
  grepl(as.character(default_seed()), cfa_toolbar, fixed = TRUE),
  grepl("structural_cfa_mi_mode", cfa_toolbar, fixed = TRUE),
  grepl("data-action=\"autoLayout\"", cfa_toolbar, fixed = TRUE),
  grepl("Auto layout", cfa_toolbar, fixed = TRUE),
  grepl("data-action=\"addHigherOrderLatent\"", cfa_toolbar, fixed = TRUE),
  grepl("Higher-order", cfa_toolbar, fixed = TRUE),
  grepl("structural-higher-order-svg", cfa_toolbar, fixed = TRUE),
  grepl("action === \"addLatent\" || action === \"addHigherOrderLatent\"", toolbar_js_source, fixed = TRUE),
  grepl("latent.constructType = \"higherOrder\"", toolbar_js_source, fixed = TRUE),
  grepl("if (otherLatent.constructType !== \"higherOrder\") return;", toolbar_js_source, fixed = TRUE),
  grepl("covariance.curveDirection = \"left\";", toolbar_js_source, fixed = TRUE),
  grepl("sem-higher-order-arrow", structural_toolbar_icons_source, fixed = TRUE),
  grepl("2차", structural_toolbar_icons_source, fixed = TRUE),
  grepl("height: 25px", canvas_css_source, fixed = TRUE),
  grepl('configuredArrowHead === "none" || configuredArrowHead === "line"', edges_js_source, fixed = TRUE),
  grepl("var ARROW_MARKER_CLEARANCE = 0;", edges_js_source, fixed = TRUE),
  grepl('if (structuralMovePolicy && item.node.role === "latent") item.node.manualPosition = true;', nodes_js_source, fixed = TRUE),
  grepl('latent.manualPosition = true;', canvas_js_source, fixed = TRUE),
  grepl('curveDirection: latentCovariance ? "left" : null', edges_js_source, fixed = TRUE),
  grepl("function isHigherOrderLatent", edges_js_source, fixed = TRUE),
  grepl('var arrowHead = (style && style.arrowHead) || "triangle";', edges_js_source, fixed = TRUE),
  grepl("function hasHigherOrderParent", edges_js_source, fixed = TRUE),
  grepl("return !hasHigherOrderParent(instance, fromNode.id) && !hasHigherOrderParent(instance, toNode.id);", edges_js_source, fixed = TRUE),
  grepl("isHigherOrderLatent(fromNode) ? \"higherOrder\" : \"regression\"", edges_js_source, fixed = TRUE),
  grepl("removeHigherOrderConflictingCovariances(instance, fromId, toId)", edges_js_source, fixed = TRUE),
  grepl("return !lowerOrderIds[edge.from] && !lowerOrderIds[edge.to];", edges_js_source, fixed = TRUE),
  !grepl("Math.abs(normalY) < 0.2", edges_js_source, fixed = TRUE),
  grepl("custom-model-node-higher-order", canvas_css_source, fixed = TRUE),
  !grepl("structural-covariate-toolbar-button", cfa_toolbar, fixed = TRUE),
  !grepl("data-action=\"structuralCovariateTargets\"", cfa_toolbar, fixed = TRUE)
)

cat("CFA UI validations passed.\n")
