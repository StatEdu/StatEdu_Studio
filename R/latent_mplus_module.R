# Integrated StatEdu Studio Latent Mplus module.

latent_mplus_module_root <- function() {
  normalizePath(file.path(getwd(), "modules", "latent_mplus", "app"), winslash = "/", mustWork = FALSE)
}

latent_mplus_enabled <- function() {
  isTRUE(statedu_feature_enabled("latent_mplus", FALSE)) &&
    file.exists(file.path(latent_mplus_module_root(), "R", "latent_ui.R")) &&
    file.exists(file.path(latent_mplus_module_root(), "R", "app_server.R"))
}

latent_mplus_env <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    root <- latent_mplus_module_root()
    env <- new.env(parent = globalenv())
    env$LATENT_MPLUS_APP_ROOT <- root
    sys.source(file.path(root, "R", "ui_helpers.R"), envir = env)
    env$latent_default_project_root <- function() {
      normalizePath(env$LATENT_MPLUS_APP_ROOT, winslash = "/", mustWork = FALSE)
    }
    env$latent_project_root_value <- function(value = NULL) {
      value <- trimws(as.character(value %||% ""))
      normalized <- gsub("\\\\", "/", value)
      if (!nzchar(normalized) || identical(tolower(normalized), "d:/latent")) {
        return(env$latent_default_project_root())
      }
      normalized
    }
    sys.source(file.path(root, "R", "latent_ui.R"), envir = env)
    sys.source(file.path(root, "R", "app_server.R"), envir = env)
    cache <<- env
    env
  }
})

latent_mplus_head_tags <- function(version) {
  tagList(
    tags$style(HTML("
      .latent-workflow {
        background: #eef3f8 !important;
        border: 1px solid #d9e2ec !important;
        border-radius: 8px !important;
        padding: 12px !important;
        --latent-b5-portrait-width: 688px !important;
        --latent-b5-landscape-width: 972px !important;
      }
      .latent-step-tabs {
        display: grid !important;
        grid-template-columns: repeat(3, minmax(0, 1fr)) !important;
        gap: 8px !important;
        margin-bottom: 12px !important;
      }
      .latent-step-tab {
        background: #ffffff !important;
        border: 1px solid #cbd5e1 !important;
        border-radius: 6px !important;
        color: #334e68 !important;
        font-weight: 700 !important;
        padding: 11px 14px !important;
        text-align: left !important;
      }
      .latent-step-tab.active {
        background: #0f766e !important;
        border-color: #0f766e !important;
        color: #ffffff !important;
      }
      .latent-step-panel,
      .latent-setup-panel {
        display: none !important;
      }
      .latent-step-panel.active,
      .latent-setup-panel.active {
        display: block !important;
      }
      .latent-block-grid,
      .latent-setup-workspace {
        align-items: stretch !important;
        display: grid !important;
        gap: 12px !important;
        grid-template-columns: 360px minmax(0, 1fr) !important;
      }
      .latent-results-workspace {
        display: block !important;
      }
      .latent-results-panel {
        min-height: 620px !important;
        max-width: none !important;
        overflow-x: hidden !important;
        width: 100% !important;
      }
      .latent-results-toolbar {
        align-items: center !important;
        display: flex !important;
        flex-wrap: wrap !important;
        gap: 8px !important;
        margin-bottom: 10px !important;
      }
      .latent-result-table-section {
        border-top: 1px solid #d9e2ec !important;
        box-sizing: border-box !important;
        margin-left: auto !important;
        margin-right: auto !important;
        margin-top: 18px !important;
        max-width: var(--latent-b5-portrait-width) !important;
        padding-top: 14px !important;
        width: min(100%, var(--latent-b5-portrait-width)) !important;
      }
      .latent-result-table-section:first-of-type {
        border-top: 0 !important;
        margin-top: 8px !important;
        padding-top: 0 !important;
      }
      .latent-result-table-section h4 {
        color: #102a43 !important;
        font-size: 15px !important;
        font-weight: 800 !important;
        margin: 0 0 8px !important;
      }
      .latent-result-overview-grid {
        display: grid !important;
        gap: 10px !important;
        grid-template-columns: repeat(4, minmax(140px, 1fr)) !important;
      }
      .latent-result-overview-item {
        border: 1px solid #d9e2ec !important;
        border-radius: 6px !important;
        padding: 10px 12px !important;
      }
      .latent-result-overview-item span {
        color: #62748a !important;
        display: block !important;
        font-size: 12px !important;
      }
      .latent-result-output-path {
        margin-top: 10px !important;
        word-break: break-all !important;
      }
      .latent-result-figure-grid {
        display: grid !important;
        gap: 14px !important;
        grid-template-columns: minmax(0, 1fr) !important;
      }
      .latent-result-figure-section {
        box-sizing: border-box !important;
        margin-left: auto !important;
        margin-right: auto !important;
        max-width: var(--latent-b5-landscape-width) !important;
        width: min(100%, var(--latent-b5-landscape-width)) !important;
      }
      .latent-result-figure-card {
        border: 1px solid #d9e2ec !important;
        border-radius: 6px !important;
        padding: 10px !important;
      }
      .latent-result-figure-card-header {
        align-items: center !important;
        display: flex !important;
        gap: 8px !important;
        justify-content: space-between !important;
        margin-bottom: 8px !important;
      }
      .latent-result-source-label {
        color: #62748a !important;
        font-size: 11px !important;
        white-space: nowrap !important;
      }
      .latent-result-figure-img {
        background: #fff !important;
        display: block !important;
        height: auto !important;
        max-width: 100% !important;
        width: 100% !important;
      }
      .latent-mplus-native-note {
        background: #f7fafc !important;
        border: 1px solid #d9e2ec !important;
        border-radius: 6px !important;
        color: #334e68 !important;
        padding: 10px !important;
        word-break: break-all !important;
      }
      .latent-result-overview-grid {
        grid-template-columns: minmax(170px, 1fr) minmax(320px, 1.7fr) minmax(110px, 0.55fr) minmax(110px, 0.55fr) !important;
      }
      .latent-result-overview-item strong {
        display: block !important;
        overflow-wrap: anywhere !important;
      }
      .latent-result-overview-analysis strong {
        font-size: 20px !important;
        line-height: 1.2 !important;
      }
      .latent-result-overview-panel {
        box-sizing: border-box !important;
        margin-left: auto !important;
        margin-right: auto !important;
        max-width: var(--latent-b5-portrait-width) !important;
        overflow-x: hidden !important;
        width: min(100%, var(--latent-b5-portrait-width)) !important;
      }
      .latent-result-output-path code {
        display: block !important;
        max-width: 100% !important;
        overflow-wrap: anywhere !important;
        white-space: normal !important;
        word-break: break-word !important;
      }
      .latent-result-table-section {
        overflow-x: hidden !important;
      }
      .latent-result-table-section.latent-result-table-landscape {
        max-width: var(--latent-b5-landscape-width) !important;
        width: min(100%, var(--latent-b5-landscape-width)) !important;
      }
      .latent-result-table-section .latent-excel-table-wrap,
      .latent-excel-table-wrap {
        max-width: 100% !important;
        overflow-x: auto !important;
      }
      .latent-result-table-landscape .latent-excel-table {
        font-size: 10.5px !important;
        min-width: 0 !important;
        width: 100% !important;
      }
      .latent-result-table-compact .latent-excel-table {
        font-size: 11px !important;
        min-width: 0 !important;
        width: 100% !important;
      }
      .latent-result-table-internal .latent-excel-table {
        font-size: 10px !important;
        min-width: 0 !important;
        width: 100% !important;
      }
      .latent-result-table-two-column .latent-excel-table {
        min-width: 0 !important;
        width: 100% !important;
      }
      .latent-result-table-nowrap-values .latent-excel-table {
        min-width: 0 !important;
        width: 100% !important;
      }
      .latent-result-table-landscape .latent-excel-table,
      .latent-result-table-compact .latent-excel-table,
      .latent-result-table-internal .latent-excel-table,
      .latent-result-table-nowrap-values .latent-excel-table {
        table-layout: fixed !important;
      }
      .latent-result-table-landscape .latent-excel-table th,
      .latent-result-table-landscape .latent-excel-table td,
      .latent-result-table-internal .latent-excel-table th,
      .latent-result-table-internal .latent-excel-table td,
      .latent-result-table-nowrap-values .latent-excel-table td:not(:first-child) {
        white-space: nowrap !important;
      }
      .latent-result-table-compact .latent-excel-table th,
      .latent-result-table-compact .latent-excel-table td {
        line-height: 1.25 !important;
        padding-left: 6px !important;
        padding-right: 6px !important;
      }
      .latent-result-table-compact .latent-excel-table td:not(:first-child),
      .latent-result-table-compact .latent-excel-table th:not(:first-child) {
        white-space: nowrap !important;
      }
      .latent-result-table-compact .latent-excel-table td:first-child,
      .latent-result-table-compact .latent-excel-table th:first-child,
      .latent-result-table-nowrap-values .latent-excel-table td:first-child,
      .latent-result-table-nowrap-values .latent-excel-table th:first-child {
        min-width: 0 !important;
        white-space: normal !important;
      }
      .latent-result-table-landscape .latent-excel-table th,
      .latent-result-table-compact .latent-excel-table th,
      .latent-result-table-internal .latent-excel-table th {
        overflow-wrap: anywhere !important;
        white-space: normal !important;
        word-break: normal !important;
      }
      .latent-result-table-internal .latent-excel-table td,
      .latent-result-table-internal .latent-excel-table th {
        line-height: 1.2 !important;
        overflow-wrap: anywhere !important;
        padding-left: 4px !important;
        padding-right: 4px !important;
        white-space: normal !important;
        word-break: break-word !important;
      }
      .latent-result-table-landscape .latent-excel-table th,
      .latent-result-table-compact .latent-excel-table th,
      .latent-result-table-nowrap-values .latent-excel-table th {
        overflow-wrap: anywhere !important;
        white-space: normal !important;
        word-break: normal !important;
      }
      .latent-result-table-compact .latent-excel-table td:first-child,
      .latent-result-table-compact .latent-excel-table th:first-child,
      .latent-result-table-nowrap-values .latent-excel-table td:first-child,
      .latent-result-table-nowrap-values .latent-excel-table th:first-child {
        width: 22% !important;
      }
      .latent-result-table-compact .latent-excel-table td:nth-child(2),
      .latent-result-table-compact .latent-excel-table th:nth-child(2) {
        width: 17% !important;
      }
      .latent-excel-profile-mean .latent-excel-table td:first-child,
      .latent-excel-profile-mean .latent-excel-table th:first-child,
      .latent-excel-appendix-center .latent-excel-table td:first-child,
      .latent-excel-appendix-center .latent-excel-table th:first-child {
        width: 22% !important;
      }
      .latent-result-table-landscape .latent-excel-title-row td,
      .latent-result-table-compact .latent-excel-title-row td,
      .latent-result-table-internal .latent-excel-title-row td {
        white-space: normal !important;
      }
      .latent-result-overview-panel,
      .latent-result-table-section,
      .latent-result-table-section.latent-result-table-landscape,
      .latent-result-figure-section {
        margin-left: 0 !important;
        margin-right: 0 !important;
        max-width: var(--latent-b5-portrait-width) !important;
        width: min(100%, var(--latent-b5-portrait-width)) !important;
      }
      .latent-result-overview-grid {
        grid-template-columns: minmax(135px, 1fr) minmax(240px, 1.6fr) minmax(84px, 0.65fr) minmax(84px, 0.65fr) !important;
        gap: 8px !important;
      }
      .latent-result-overview-item {
        min-width: 0 !important;
        padding: 9px 10px !important;
      }
      .latent-result-overview-analysis strong {
        font-size: 17px !important;
      }
      .latent-result-overview-count strong {
        font-size: 16px !important;
      }
      .latent-result-figure-card {
        overflow: hidden !important;
      }
      .latent-result-figure-img {
        max-width: var(--latent-b5-portrait-width) !important;
        width: 100% !important;
      }
      .latent-result-table-section .latent-excel-table-wrap,
      .latent-excel-table-wrap {
        max-width: 100% !important;
        overflow-x: hidden !important;
      }
      .latent-result-table-section .latent-excel-table {
        table-layout: fixed !important;
        width: 100% !important;
      }
      .latent-result-table-t1 .latent-excel-table td,
      .latent-result-table-t1 .latent-excel-table th {
        white-space: normal !important;
        word-break: break-word !important;
        overflow-wrap: anywhere !important;
      }
      .latent-result-table-t2 .latent-excel-table {
        font-size: 7.4px !important;
        line-height: 1.12 !important;
      }
      .latent-result-table-t2 .latent-excel-table th,
      .latent-result-table-t2 .latent-excel-table td {
        padding-left: 2px !important;
        padding-right: 2px !important;
        white-space: nowrap !important;
        word-break: normal !important;
        overflow-wrap: normal !important;
      }
      .latent-result-table-t2 .latent-excel-title-row td {
        font-size: 9px !important;
        white-space: normal !important;
      }
      .latent-result-table-t4 .latent-excel-table {
        font-size: 9.5px !important;
      }
      .latent-result-table-t4 .latent-excel-table th,
      .latent-result-table-t4 .latent-excel-table td {
        padding-left: 3px !important;
        padding-right: 3px !important;
      }
      .latent-result-table-t4 .latent-excel-table td:first-child,
      .latent-result-table-t4 .latent-excel-table th:first-child {
        width: 28% !important;
        white-space: normal !important;
        word-break: normal !important;
        overflow-wrap: break-word !important;
      }
      .latent-result-table-t4 .latent-excel-table td:not(:first-child),
      .latent-result-table-t4 .latent-excel-table th:not(:first-child) {
        text-align: center !important;
        white-space: nowrap !important;
      }
      .latent-control-panel,
      .latent-main-panel {
        align-self: stretch !important;
        min-height: 420px !important;
        min-width: 0 !important;
      }
      .latent-main-panel {
        overflow-x: auto !important;
      }
      .latent-control-panel h3,
      .latent-main-panel h3 {
        margin-bottom: 16px !important;
      }
      .latent-panel-note {
        color: #52606d !important;
        margin-bottom: 14px !important;
      }
      .latent-button-row,
      .latent-export-actions {
        align-items: center !important;
        display: flex !important;
        flex-wrap: wrap !important;
        gap: 8px !important;
      }
      .latent-button-column {
        display: grid !important;
        gap: 8px !important;
      }
      .latent-button-column .btn {
        text-align: left !important;
        width: 100% !important;
      }
      .latent-variable-role-panel .dataTables_wrapper {
        min-height: 100% !important;
      }
      .latent-variable-role-panel table.dataTable th:first-child,
      .latent-variable-role-panel table.dataTable td:first-child {
        text-align: center !important;
        width: 54px !important;
      }
      .latent-variable-role-panel table.dataTable select.latent-role-select {
        font-size: 13px !important;
        height: 32px !important;
        min-width: 140px !important;
        padding: 4px 8px !important;
      }
      .latent-variable-role-panel table.dataTable input.latent-role-checkbox {
        height: 16px !important;
        margin: 0 !important;
        width: 16px !important;
      }
      select[id$='_active_role'] {
        background: #ecfdf5 !important;
        border-color: #0f766e !important;
        border-width: 2px !important;
        color: #064e3b !important;
        font-weight: 700 !important;
      }
      select[id$='_active_role']:focus {
        border-color: #f59e0b !important;
        box-shadow: 0 0 0 3px rgba(245, 158, 11, 0.2) !important;
      }
      .latent-active-role-row td {
        background: #fff7ed !important;
        color: #7c2d12 !important;
        font-weight: 700 !important;
      }
      .latent-active-role-row td:first-child {
        border-left: 4px solid #f97316 !important;
      }
      .latent-options-grid {
        align-items: start !important;
        display: grid !important;
        gap: 12px !important;
        grid-template-columns: repeat(3, minmax(200px, 1fr)) !important;
      }
      .latent-option-card {
        background: #ffffff !important;
        border: 1px solid #e6edf5 !important;
        border-radius: 6px !important;
        min-height: 106px !important;
        min-width: 0 !important;
        overflow: visible !important;
        padding: 10px 12px 7px !important;
      }
      .latent-option-card .form-group,
      .latent-option-card .checkbox {
        margin-bottom: 0 !important;
      }
      .latent-option-card label {
        color: #102a43 !important;
        font-size: 13px !important;
        line-height: 1.25 !important;
      }
      .latent-option-card .form-control {
        height: 34px !important;
      }
      .latent-option-card .checkbox,
      .latent-option-card .checkbox label {
        align-items: center !important;
        display: flex !important;
        min-height: 34px !important;
      }
      .latent-option-card .checkbox label {
        margin: 0 !important;
      }
      .latent-option-card .checkbox input[type='checkbox'] {
        margin-top: 0 !important;
      }
      .latent-option-card .shiny-options-group {
        max-height: 54px !important;
        overflow-y: auto !important;
      }
      .latent-combined-control {
        align-items: end !important;
        display: grid !important;
        gap: 10px !important;
        grid-template-columns: minmax(0, 1fr) 112px !important;
      }
      .latent-combined-control .checkbox,
      .latent-combined-control .form-group {
        margin-bottom: 0 !important;
      }
      .latent-fixed-k-control input[type='number'] {
        width: 100% !important;
      }
      #mixture_model_structures .shiny-options-group {
        column-gap: 0 !important;
        display: flex !important;
        flex-wrap: wrap !important;
        gap: 8px 16px !important;
        grid-template-columns: none !important;
        max-height: none !important;
        overflow: visible !important;
        white-space: nowrap !important;
      }
      #mixture_model_structures .checkbox-inline {
        align-items: center !important;
        display: inline-flex !important;
        font-size: 13px !important;
        gap: 4px !important;
        line-height: 1.15 !important;
        margin: 0 !important;
        min-width: 66px !important;
        padding-left: 0 !important;
      }
      #mixture_model_structures input[type='checkbox'] {
        flex: 0 0 auto !important;
        margin: 0 !important;
        position: static !important;
      }
      .latent-option-scope {
        display: flex !important;
        flex-wrap: wrap !important;
        gap: 4px !important;
        margin-bottom: 7px !important;
        min-height: 22px !important;
      }
      .latent-analysis-badge {
        align-items: center !important;
        background: #f8fafc !important;
        border: 1px solid #cbd5e1 !important;
        border-radius: 999px !important;
        color: #334e68 !important;
        display: inline-flex !important;
        font-size: 11px !important;
        font-weight: 800 !important;
        line-height: 1 !important;
        min-height: 20px !important;
        padding: 2px 7px !important;
      }
      .latent-analysis-badge.scope-common {
        background: #f8fafc !important;
        border-color: #94a3b8 !important;
        color: #334155 !important;
      }
      .latent-analysis-badge.scope-lca {
        background: #ecfdf5 !important;
        border-color: #34d399 !important;
        color: #065f46 !important;
      }
      .latent-analysis-badge.scope-lpa {
        background: #eff6ff !important;
        border-color: #60a5fa !important;
        color: #1d4ed8 !important;
      }
      .latent-analysis-badge.scope-mixed {
        background: #fffbeb !important;
        border-color: #f59e0b !important;
        color: #92400e !important;
      }
      .latent-analysis-badge.scope-lmr,
      .latent-analysis-badge.scope-blrt {
        background: #faf5ff !important;
        border-color: #c084fc !important;
        color: #6b21a8 !important;
      }
      .latent-setup-topic-list {
        display: grid !important;
        gap: 8px !important;
      }
      .latent-setup-topic {
        background: #ffffff !important;
        border: 1px solid #cbd5e1 !important;
        border-radius: 6px !important;
        color: #102a43 !important;
        font-weight: 700 !important;
        min-height: 42px !important;
        padding: 9px 12px !important;
        text-align: left !important;
        width: 100% !important;
      }
      .latent-setup-topic.active {
        background: #0f766e !important;
        border-color: #0f766e !important;
        color: #ffffff !important;
      }
      .latent-setup-panel {
        background: #ffffff !important;
        border: 1px solid #e6edf5 !important;
        border-radius: 8px !important;
        padding: 14px 14px 4px !important;
      }
      .latent-setup-panel h4 {
        color: #0f766e !important;
        font-size: 16px !important;
        font-weight: 700 !important;
        margin: 0 0 12px !important;
      }
      .latent-run-footer {
        background: #ffffff !important;
        border: 1px solid #e6edf5 !important;
        border-radius: 8px !important;
        padding: 14px !important;
      }
      .latent-run-controls-grid {
        align-items: start !important;
        display: grid !important;
        gap: 12px !important;
        grid-template-columns: repeat(3, minmax(200px, 1fr)) !important;
      }
      .latent-run-field {
        min-width: 0 !important;
      }
      .latent-run-field .form-group {
        margin-bottom: 0 !important;
      }
      .latent-run-action-row {
        border-top: 1px solid #e6edf5 !important;
        display: flex !important;
        justify-content: flex-start !important;
        margin-top: 12px !important;
        padding-top: 12px !important;
      }
      .latent-run-action-row .btn {
        min-width: 180px !important;
      }
      .latent-control-panel .latent-run-footer {
        margin-top: 0 !important;
      }
      .latent-control-panel .latent-run-controls-grid {
        grid-template-columns: 1fr !important;
      }
      .latent-control-panel .latent-run-field {
        padding: 0 !important;
      }
      .latent-control-panel .latent-run-action-row .btn {
        width: 100% !important;
      }
      @media (max-width: 1100px) {
        .latent-block-grid,
        .latent-setup-workspace,
        .latent-options-grid,
        .latent-run-controls-grid {
          grid-template-columns: 1fr !important;
        }
      }
    ")),
    tags$script(HTML("
      $(document).on('click', '.latent-step-tab', function() {
        var target = $(this).data('target');
        var scope = $(this).closest('.latent-workflow');
        scope.find('.latent-step-tab').removeClass('active');
        $(this).addClass('active');
        scope.find('.latent-step-panel').removeClass('active');
        scope.find('#' + target).addClass('active');
      });
      $(document).on('click', '.latent-setup-topic', function() {
        var target = $(this).data('target');
        var scope = $(this).closest('.latent-setup-workspace');
        scope.find('.latent-setup-topic').removeClass('active');
        $(this).addClass('active');
        scope.find('.latent-setup-panel').removeClass('active');
        scope.find('#' + target).addClass('active');
      });
      Shiny.addCustomMessageHandler('latent-show-results', function(message) {
        var moduleId = message.module || '';
        var target = 'latent_' + moduleId + '_run';
        var panel = $('#' + target);
        if (!panel.length) return;
        var workflow = panel.closest('.latent-workflow');
        workflow.find('.latent-step-tab').removeClass('active');
        workflow.find('.latent-step-tab[data-target=\"' + target + '\"]').addClass('active');
        workflow.find('.latent-step-panel').removeClass('active');
        panel.addClass('active');
        window.setTimeout(function() {
          var top = Math.max(0, workflow.offset().top - 12);
          window.scrollTo(window.pageXOffset || 0, top);
        }, 0);
      });
      function latentActiveRole(moduleId) {
        var input = $('#' + moduleId + '_active_role');
        if (!input.length) return '';
        if (input[0].selectize) return input[0].selectize.getValue() || '';
        return input.val() || '';
      }
      $(document).on('change', '.latent-role-select', function() {
        if (!window.Shiny) return;
        var el = $(this);
        var moduleId = el.data('module');
        if (window.easyflowLatentRememberViewport) window.easyflowLatentRememberViewport(moduleId);
        if (window.easyflowLatentRememberPage) window.easyflowLatentRememberPage(moduleId);
        Shiny.setInputValue(moduleId + '_role_cell_update', {
          variable: el.data('variable'),
          role: el.val() || '',
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });
      $(document).on('change', '.latent-role-checkbox', function() {
        if (!window.Shiny) return;
        var el = $(this);
        var moduleId = el.data('module');
        var role = latentActiveRole(moduleId);
        if (!moduleId || !role) return;
        if (window.easyflowLatentRememberViewport) window.easyflowLatentRememberViewport(moduleId);
        if (window.easyflowLatentRememberPage) window.easyflowLatentRememberPage(moduleId);
        el.closest('tr').find('.latent-role-select').val(el.prop('checked') ? role : '');
        Shiny.setInputValue(moduleId + '_role_checkbox_update', {
          variable: el.data('variable'),
          role: el.prop('checked') ? role : '',
          active_role: role,
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('latent-role-clear', function(message) {
        var moduleId = message.module || '';
        var role = message.role || '';
        var all = !!message.all;
        var table = $('#' + moduleId + '_variable_preview table');
        table.find('.latent-role-select').each(function() {
          var select = $(this);
          if (all || select.val() === role) select.val('');
        });
        table.find('.latent-role-checkbox').prop('checked', false);
      });
      $(document).on('click', '.latent-select-current-page', function() {
        if (!window.Shiny) return;
        var moduleId = $(this).data('module');
        var role = $('#' + moduleId + '_active_role').val() || '';
        if (window.easyflowLatentRememberViewport) window.easyflowLatentRememberViewport(moduleId);
        if (window.easyflowLatentRememberPage) window.easyflowLatentRememberPage(moduleId);
        var dt = $('#' + moduleId + '_variable_preview table').DataTable();
        var variables = [];
        $(dt.rows({page: 'current'}).nodes()).find('.latent-role-checkbox').each(function() {
          var variable = $(this).data('variable');
          if (variable) variables.push(variable);
        });
        Shiny.setInputValue(moduleId + '_select_current_page', {
          variables: variables,
          role: role,
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });
    "))
  )
}

latent_menu_tab <- function() {
  if (!latent_mplus_enabled()) {
    return(NULL)
  }
  latent_mplus_env()$latent_menu_tab()
}

register_latent_mplus_server <- function(input,
                                         output,
                                         session,
                                         app_version,
                                         current_data_file,
                                         variable_info_table,
                                         active_data_file,
                                         reset_on_dataset_load,
                                         available_variable_names) {
  if (!latent_mplus_enabled()) {
    return(invisible(FALSE))
  }

  root <- latent_mplus_module_root()
  try(shiny::addResourcePath("latent_mplus_assets", file.path(root, "www")), silent = TRUE)
  app_output_root <- file.path(getwd(), "outputs")
  dir.create(app_output_root, recursive = TRUE, showWarnings = FALSE)
  try(shiny::addResourcePath("latent_outputs", normalizePath(app_output_root, winslash = "/", mustWork = FALSE)), silent = TRUE)

  env <- new.env(parent = latent_mplus_env())
  env$input <- input
  env$output <- output
  env$session <- session
  env$app_version <- app_version
  env$current_data_file <- current_data_file
  env$variable_info_table <- variable_info_table
  env$active_data_file <- active_data_file
  env$reset_on_dataset_load <- reset_on_dataset_load
  env$available_variable_names <- available_variable_names

  source_file <- file.path(root, "R", "app_server.R")
  lines <- readLines(source_file, warn = FALSE, encoding = "UTF-8")
  start <- grep("observeEvent\\(input\\$home_open_mixture", lines)[1]
  end <- grep("^\\s*output\\$result_library\\s*<-", lines)[1]
  if (is.na(start) || is.na(end)) {
    stop("Latent Mplus server block could not be located.", call. = FALSE)
  }
  depth <- 0L
  end_block <- NA_integer_
  for (i in seq.int(end, length(lines))) {
    depth <- depth + lengths(regmatches(lines[[i]], gregexpr("\\{", lines[[i]], fixed = FALSE)))
    depth <- depth - lengths(regmatches(lines[[i]], gregexpr("\\}", lines[[i]], fixed = FALSE)))
    if (i > end && depth <= 0L) {
      end_block <- i
      break
    }
  }
  if (is.na(end_block)) {
    end_block <- 1173L
  }

  block <- paste(lines[start:end_block], collapse = "\n")
  eval(parse(text = block), envir = env)
  invisible(TRUE)
}
