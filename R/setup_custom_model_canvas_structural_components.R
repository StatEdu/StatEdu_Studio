# Structural equation canvas UI components.

structural_equation_variable_panel <- function(items, language = statedu_initial_language()) {
  list_ui <- custom_model_canvas_variable_panel(items, language)
  if (length(items) > 0) list_ui$children[[2]] <- NULL
  tagList(
    list_ui,
    div(
      class = "structural-selection-settings",
      div(class = "structural-selection-settings-body", if (identical(normalize_app_language(language), "ko")) "캔버스의 변수를 선택하세요." else "Select a variable on the canvas.")
    )
  )
}

structural_equation_title <- function(language = statedu_initial_language()) {
  if (identical(normalize_app_language(language), "ko")) "구조방정식" else "Structural Equation Modeling"
}

structural_analysis_title <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  switch(
    analysis_type,
    cfa = if (ko) "확인적 요인분석" else "Confirmatory Factor Analysis",
    plssem = if (ko) "PLS 구조방정식" else "PLS Structural Equation Modeling",
    if (ko) "구조방정식" else "Structural Equation Modeling"
  )
}

structural_analysis_prefix <- function(analysis_type = "cbsem") {
  paste0("structural_", analysis_type)
}

structural_automation_title <- function(language = statedu_initial_language()) {
  if (identical(normalize_app_language(language), "ko")) "구조방정식 분석 추천" else "SEM Workflow Recommendation"
}

structural_automation_tab_panel <- function(language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  title <- structural_automation_title(language)
  tabPanel(
    title,
    value = "analysis_structural_automation",
    div(
      class = "page-shell",
      div(class = "app-heading", h1(title), div(if (ko) "연구 목적과 구성개념 명세에 맞는 구조방정식 분석 workflow를 추천합니다." else "Receive a structural-equation workflow recommendation based on the research objective and construct specification.", class = "app-subtitle")),
      div(
        class = "workspace-panel structural-automation-launcher",
        h3(if (ko) "연구설계 기반 분석 추천" else "Design-guided analysis recommendation"),
        tags$p(if (ko) "분석법 이름을 직접 고르는 대신 연구 목적과 구성개념을 선언하면 적합한 엔진으로 안내합니다. 구성개념의 이론적 유형은 자료만으로 자동 판정하지 않습니다." else "Declare the research objective and construct type instead of choosing a method name. The workflow routes to an appropriate engine; construct ontology is not inferred from data alone."),
        selectInput("structural_automation_objective", if (ko) "주요 목적" else "Primary objective", choices = stats::setNames(c("measurement", "theory", "prediction"), c(if (ko) "측정모형 검증" else "Validate a measurement model", if (ko) "구조관계·이론 검증" else "Test structural relations or theory", if (ko) "예측·구성개념 점수" else "Prediction or construct scores"))),
        selectInput("structural_automation_construct", if (ko) "구성개념 유형" else "Construct type", choices = stats::setNames(c("common_factor", "composite", "mixed"), c(if (ko) "반영형 공통요인" else "Reflective common factors", if (ko) "합성변수 포함" else "Includes composites", if (ko) "공통요인·합성변수 혼합" else "Mixed factors and composites"))),
        selectInput("structural_automation_indicator", if (ko) "지표 측정수준" else "Indicator scale", choices = stats::setNames(c("continuous", "ordered"), c(if (ko) "연속형" else "Continuous", if (ko) "순서형 포함" else "Includes ordered indicators"))),
        actionButton("structural_automation_start", if (ko) "권장 workflow 시작" else "Start recommended workflow", class = "btn-primary")
      )
    )
  )
}

structural_capture_truthy <- function(value) {
  tolower(trimws(as.character(value %||% ""))) %in% c("1", "true", "yes", "y", "run", "auto")
}

structural_capture_initial_snapshot <- function(analysis_type = "cbsem") {
  if (!identical(analysis_type, "cfa")) return(list(snapshot = NULL, auto_run = FALSE))
  candidates <- c(
    Sys.getenv("STATEDU_CAPTURE_CFA_MODEL_FILE", ""),
    Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_MODEL_FILE", "")
  )
  candidates <- candidates[nzchar(candidates)]
  path <- if (length(candidates)) candidates[[1L]] else ""
  if (!nzchar(path)) return(list(snapshot = NULL, auto_run = FALSE))
  if (!file.exists(path)) {
    warning(sprintf("CFA capture model file does not exist: %s", path), call. = FALSE)
    return(list(snapshot = NULL, auto_run = FALSE))
  }
  snapshot <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) {
      warning(sprintf("Failed to read CFA capture model file: %s", conditionMessage(error)), call. = FALSE)
      NULL
    }
  )
  if (!is.list(snapshot) || !is.list(snapshot$nodes) || !is.list(snapshot$edges)) {
    warning("CFA capture model file must contain model nodes and edges.", call. = FALSE)
    return(list(snapshot = NULL, auto_run = FALSE))
  }
  list(
    snapshot = snapshot,
    auto_run = structural_capture_truthy(Sys.getenv("STATEDU_CAPTURE_CFA_RUN", ""))
  )
}

structural_analysis_package <- function(analysis_type = "cbsem") {
  if (identical(analysis_type, "plssem")) "seminr" else "lavaan"
}

structural_equation_workspace <- function(selected_names, variable_table = NULL, labels = character(0), analysis_type = "cbsem", language = statedu_initial_language()) {
  prefix <- structural_analysis_prefix(analysis_type)
  items <- custom_model_canvas_variable_items(selected_names, variable_table, labels)
  variables_json <- htmltools::htmlEscape(jsonlite::toJSON(items, auto_unbox = TRUE, null = "null"), attribute = TRUE)
  labels_i18n <- custom_model_canvas_i18n(language)
  labels_i18n$role_latent <- if (identical(normalize_app_language(language), "ko")) "잠재변수" else "Latent variable"
  i18n_json <- htmltools::htmlEscape(jsonlite::toJSON(labels_i18n, auto_unbox = TRUE, null = "null"), attribute = TRUE)
  capture <- structural_capture_initial_snapshot(analysis_type)
  capture_attrs <- list()
  if (!is.null(capture$snapshot)) {
    capture_attrs[["data-initial-snapshot"]] <- htmltools::htmlEscape(
      jsonlite::toJSON(capture$snapshot, auto_unbox = TRUE, null = "null"),
      attribute = TRUE
    )
    if (isTRUE(capture$auto_run)) capture_attrs[["data-initial-run"]] <- "true"
  }
  canvas_spec <- list(width = "1123", height = "794", paper = "A4", orientation = "landscape", status = "A4 landscape")
  root <- div(
      id = paste0(prefix, "-canvas-root"),
      class = "custom-model-canvas-root structural-equation-canvas-root",
      `data-input-prefix` = paste0(prefix, "_canvas"),
      `data-analysis-type` = analysis_type,
      `data-analysis-package` = structural_analysis_package(analysis_type),
      `data-canvas-width` = canvas_spec$width,
      `data-canvas-height` = canvas_spec$height,
      `data-canvas-paper` = canvas_spec$paper,
      `data-canvas-orientation` = canvas_spec$orientation,
      `data-variables` = variables_json,
      `data-language` = normalize_app_language(language),
      `data-i18n` = i18n_json,
      div(
        class = "custom-model-variable-panel analysis-transfer-column analysis-transfer-panel",
        analysis_field_label_tag(if (identical(normalize_app_language(language), "ko")) "관측변수" else "Observed variables", language = language),
        structural_equation_variable_panel(items, language)
      ),
      div(
        class = "custom-model-diagram-panel",
        structural_equation_toolbar(analysis_type, language),
        div(class = "custom-model-statusbar",
            span(class = "custom-model-mode-status", custom_model_canvas_text(language, "Mode: Select", "모드: 선택")),
            span(class = "custom-model-paper-status", canvas_spec$status),
            span(class = "custom-model-covariate-status", ""),
            span(class = "structural-validation-status", "오류 0 · 경고 0")),
        div(class = "custom-model-canvas-scroll",
            div(class = "custom-model-paper-frame",
                div(class = "custom-model-paper is-grid-visible", `data-width` = canvas_spec$width, `data-height` = canvas_spec$height,
                    tags$svg(class = "custom-model-edge-layer", width = canvas_spec$width, height = canvas_spec$height),
                    div(class = "custom-model-node-layer"))))
      )
    )
  if (length(capture_attrs)) {
    root <- do.call(htmltools::tagAppendAttributes, c(list(root), capture_attrs))
  }
  tagList(
    root,
    uiOutput(paste0(prefix, "_results")),
    tags$script(HTML("window.StatEduModelCanvas && window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.initAll();"))
  )
}

structural_equation_tab_panel <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  prefix <- structural_analysis_prefix(analysis_type)
  title <- structural_analysis_title(analysis_type, language)
  tabPanel(
    title,
    value = paste0("analysis_", prefix),
    div(class = "page-shell",
        div(class = "app-heading", h1(title), div(if (identical(normalize_app_language(language), "ko")) "관측변수와 잠재변수를 배치하여 CFA와 SEM 모형을 작성합니다." else "Build CFA and SEM models with observed and latent variables.", class = "app-subtitle")),
        div(class = "workspace-panel frequencies-workspace-panel custom-model-workspace-panel structural-equation-workspace-panel", style = "min-width:1450px;overflow-x:auto;",
            div(
              class = paste("analysis-workspace-heading", paste0(prefix, "-workspace-heading")),
              div(class = "analysis-workspace-heading-main", h3(analysis_ui_text(title, language)))
            ),
            analysis_workspace_body(prefix, uiOutput(paste0(prefix, "_canvas_setup")), NULL, NULL)))
  )
}
