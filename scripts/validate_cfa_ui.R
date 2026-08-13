source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("htmltools", quietly = TRUE))

canvas_js_source <- paste(readLines(file.path("www", "model-canvas", "canvas.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
bridge_js_source <- paste(readLines(file.path("www", "model-canvas", "shiny-bridge.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("Latent construct correlations, reliability, and convergent/discriminant validity", ui_source, fixed = TRUE),
  length(gregexpr('class = "table-responsive"', ui_source, fixed = TRUE)[[1L]]) >= 8L,
  grepl("STATEDU_CAPTURE_CFA_MODEL_FILE", ui_source, fixed = TRUE),
  grepl("data-initial-snapshot", ui_source, fixed = TRUE),
  grepl("data-initial-run", ui_source, fixed = TRUE),
  grepl("parseInitialSnapshot", canvas_js_source, fixed = TRUE),
  grepl("window.StatEduModelCanvas.state.restore(instance.state, initialSnapshot)", canvas_js_source, fixed = TRUE),
  grepl("window.Shiny && typeof window.Shiny.setInputValue === \"function\"", canvas_js_source, fixed = TRUE),
  grepl("\"fromSide\", \"toSide\", \"fixedCenter\", \"directAnchors\", \"labelPosition\"", bridge_js_source, fixed = TRUE),
  grepl("_reliability_ci_method", ui_source, fixed = TRUE),
  grepl("_htmt_ci_method", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_validity_outputs(\n    output, prefix, analysis_type, fit_result, result_table, app_language_fn", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_reliability_bootstrap_outputs(output, prefix, fit_result, app_language_fn)", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_htmt_outputs(output, prefix, fit_result, app_language_fn)", ui_source, fixed = TRUE),
  grepl("structural_canvas_register_local_fit_outputs(\n    output, prefix, fit_result, app_language_fn", ui_source, fixed = TRUE),
  grepl("AVE/신뢰도 부트스트랩 CI", ui_source, fixed = TRUE),
  grepl("다집단 표본통계에 대한 HTMT", ui_source, fixed = TRUE),
  grepl("국지적 적합도 진단", ui_source, fixed = TRUE),
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
  identical(cfa_option_tab_values, c("Estimation", "Validity", "Diagnostics")),
  identical(cfa_option_pane_values, c("Estimation", "Validity", "Diagnostics")),
  identical(cfa_option_tab_targets, cfa_option_pane_ids),
  grepl('<li class="active">', cfa_toolbar, fixed = TRUE),
  grepl('<div class="tab-pane active" data-value="Estimation"', cfa_toolbar, fixed = TRUE),
  grepl("Estimation", cfa_toolbar, fixed = TRUE),
  grepl("Validity", cfa_toolbar, fixed = TRUE),
  grepl("Diagnostics", cfa_toolbar, fixed = TRUE),
  grepl("AVE/reliability bootstrap CI", cfa_toolbar, fixed = TRUE),
  grepl("structural_cfa_reliability_bootstrap", cfa_toolbar, fixed = TRUE),
  grepl("structural_cfa_htmt_bootstrap", cfa_toolbar, fixed = TRUE),
  grepl("structural_cfa_mi_mode", cfa_toolbar, fixed = TRUE),
  !grepl("structural-covariate-toolbar-button", cfa_toolbar, fixed = TRUE),
  !grepl("data-action=\"structuralCovariateTargets\"", cfa_toolbar, fixed = TRUE)
)

cat("CFA UI validations passed.\n")
