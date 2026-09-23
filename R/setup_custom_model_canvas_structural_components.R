# Structural equation canvas UI components.

structural_equation_variable_panel <- function(items, language = statedu_initial_language()) {
  tagList(
    custom_model_canvas_variable_panel(items, language),
    div(
      class = "structural-selection-settings",
      div(class = "structural-selection-settings-title",
        statedu_localized_text(language, "Variable settings", "환경 설정")),
      div(class = "structural-selection-settings-body",
        statedu_localized_text(language, "Select a variable on the canvas.", "캔버스의 변수를 선택하세요."))
    )
  )
}

structural_equation_title <- function(language = statedu_initial_language()) {
  statedu_localized_text(language, "Structural Equation Modeling", "구조방정식")
}

structural_analysis_title <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  switch(
    analysis_type,
    cfa = statedu_localized_text(language, "Confirmatory Factor Analysis", "확인적 요인분석"),
    plssem = statedu_localized_text(language, "PLS Structural Equation Modeling", "PLS 구조방정식"),
    structural_equation_title(language)
  )
}

structural_analysis_prefix <- function(analysis_type = "cbsem") {
  paste0("structural_", analysis_type)
}

structural_automation_title <- function(language = statedu_initial_language()) {
  statedu_localized_text(language, "SEM Workflow Recommendation", "구조방정식 분석 추천")
}

structural_automation_tab_panel <- function(language = statedu_initial_language(), selected = list()) {
  title <- structural_automation_title(language)
  tabPanel(
    title,
    value = "analysis_structural_automation",
    div(
      class = "page-shell",
      div(class = "app-heading", h1(title), div(statedu_localized_text(language, "Receive a structural-equation workflow recommendation based on the research objective and construct specification.", "연구 목적과 구성개념 명세에 맞는 구조방정식 분석 workflow를 추천합니다."), class = "app-subtitle")),
      div(
        class = "workspace-panel structural-automation-launcher",
        h3(statedu_localized_text(language, "Design-guided analysis recommendation", "연구설계 기반 분석 추천")),
        tags$p(statedu_localized_text(language, "Declare the research objective and construct type instead of choosing a method name. The workflow routes to an appropriate engine; construct ontology is not inferred from data alone.", "분석법 이름을 직접 고르는 대신 연구 목적과 구성개념을 선언하면 적합한 엔진으로 안내합니다. 구성개념의 이론적 유형은 자료만으로 자동 판정하지 않습니다.")),
        selectInput("structural_automation_objective", statedu_localized_text(language, "Primary objective", "주요 목적"), choices = stats::setNames(c("measurement", "theory", "prediction"), c(statedu_localized_text(language, "Validate a measurement model", "측정모형 검증"), statedu_localized_text(language, "Test structural relations or theory", "구조관계·이론 검증"), statedu_localized_text(language, "Prediction or construct scores", "예측·구성개념 점수"))), selected = selected$objective %||% "measurement"),
        selectInput("structural_automation_construct", statedu_localized_text(language, "Construct type", "구성개념 유형"), choices = stats::setNames(c("common_factor", "composite", "mixed"), c(statedu_localized_text(language, "Reflective common factors", "반영형 공통요인"), statedu_localized_text(language, "Includes composites", "합성변수 포함"), statedu_localized_text(language, "Mixed factors and composites", "공통요인·합성변수 혼합"))), selected = selected$construct %||% "common_factor"),
        selectInput("structural_automation_indicator", statedu_localized_text(language, "Indicator scale", "지표 측정수준"), choices = stats::setNames(c("continuous", "ordered"), c(statedu_localized_text(language, "Continuous", "연속형"), statedu_localized_text(language, "Includes ordered indicators", "순서형 포함"))), selected = selected$indicator %||% "continuous"),
        actionButton("structural_automation_start", statedu_localized_text(language, "Start recommended workflow", "권장 workflow 시작"), class = "btn-primary")
      )
    )
  )
}

structural_capture_truthy <- function(value) {
  tolower(trimws(as.character(value %||% ""))) %in% c("1", "true", "yes", "y", "run", "auto")
}

structural_capture_initial_snapshot <- function(analysis_type = "cbsem") {
  analysis_type <- as.character(analysis_type %||% "cbsem")
  candidates <- switch(
    analysis_type,
    cfa = c(Sys.getenv("STATEDU_CAPTURE_CFA_MODEL_FILE", ""), Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_MODEL_FILE", "")),
    cbsem = c(Sys.getenv("STATEDU_CAPTURE_SEM_MODEL_FILE", ""), Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_MODEL_FILE", "")),
    plssem = c(Sys.getenv("STATEDU_CAPTURE_PLS_MODEL_FILE", ""), Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_MODEL_FILE", "")),
    character(0)
  )
  candidates <- candidates[nzchar(candidates)]
  path <- if (length(candidates)) candidates[[1L]] else ""
  if (!nzchar(path)) return(list(snapshot = NULL, auto_run = FALSE))
  if (!file.exists(path)) {
    warning(sprintf("%s capture model file does not exist: %s", toupper(analysis_type), path), call. = FALSE)
    return(list(snapshot = NULL, auto_run = FALSE))
  }
  snapshot <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) {
      warning(sprintf("Failed to read %s capture model file: %s", toupper(analysis_type), conditionMessage(error)), call. = FALSE)
      NULL
    }
  )
  if (!is.list(snapshot) || !is.list(snapshot$nodes) || !is.list(snapshot$edges)) {
    warning(sprintf("%s capture model file must contain model nodes and edges.", toupper(analysis_type)), call. = FALSE)
    return(list(snapshot = NULL, auto_run = FALSE))
  }
  list(
    snapshot = snapshot,
    auto_run = structural_capture_truthy(switch(
      analysis_type,
      cfa = Sys.getenv("STATEDU_CAPTURE_CFA_RUN", Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_RUN", "")),
      cbsem = Sys.getenv("STATEDU_CAPTURE_SEM_RUN", Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_RUN", "")),
      plssem = Sys.getenv("STATEDU_CAPTURE_PLS_RUN", Sys.getenv("STATEDU_CAPTURE_STRUCTURAL_RUN", "")),
      ""
    ))
  )
}

structural_analysis_package <- function(analysis_type = "cbsem") {
  if (identical(analysis_type, "plssem")) "seminr" else "lavaan"
}

structural_equation_workspace <- function(selected_names, variable_table = NULL, labels = character(0), analysis_type = "cbsem", language = statedu_initial_language(), option_values = list()) {
  prefix <- structural_analysis_prefix(analysis_type)
  items <- custom_model_canvas_variable_items(selected_names, variable_table, labels)
  grouping_choices <- structural_canvas_grouping_variable_select_choices(selected_names, variable_table, labels)
  variables_json <- htmltools::htmlEscape(jsonlite::toJSON(items, auto_unbox = TRUE, null = "null"), attribute = TRUE)
  labels_i18n <- custom_model_canvas_i18n(language)
  labels_i18n$role_latent <- statedu_localized_text(language, "Latent variable", "잠재변수")
  labels_i18n$validation_counts <- statedu_localized_text(language, "Errors {errors} · Warnings {warnings}", "오류 {errors} · 경고 {warnings}")
  labels_i18n$mode_add_observed <- statedu_localized_text(language, "Mode: Click the canvas to place a measured variable", "모드: 캔버스를 클릭하여 측정변수 배치")
  labels_i18n$mode_add_latent <- statedu_localized_text(language, "Mode: Click the canvas to place a latent variable", "모드: 캔버스를 클릭하여 잠재변수 배치")
  labels_i18n$mode_add_higher_order <- statedu_localized_text(language, "Mode: Click the canvas to place a higher-order factor", "모드: 캔버스를 클릭하여 고차요인 배치")
  labels_i18n$mode_covariance <- statedu_localized_text(language, "Mode: Covariance", "모드: 공분산")
  labels_i18n$selected_count <- statedu_localized_text(language, "{count} selected", "{count}개 선택")
  labels_i18n$paper_change_title <- statedu_localized_text(language, "Change paper size/orientation", "용지 크기/방향 변경")
  labels_i18n$paper_change_aria <- statedu_localized_text(language, "Change paper size and orientation", "용지 크기와 방향 변경")
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
        analysis_field_label_tag(statedu_localized_text(language, "Observed variables", "관측변수"), language = language),
        structural_equation_variable_panel(items, language)
      ),
      div(
        class = "custom-model-diagram-panel",
        structural_equation_toolbar(analysis_type, language, grouping_choices, option_values),
        div(class = "custom-model-statusbar",
            span(class = "custom-model-mode-status", custom_model_canvas_text(language, "Mode: Select", "모드: 선택")),
            span(
              class = "custom-model-result-group-control",
              span(class = "custom-model-result-group-label", custom_model_canvas_text(language, "Model", "모형")),
              tags$select(class = "custom-model-result-group-select", `aria-label` = custom_model_canvas_text(language, "Result group", "결과 집단"))
            ),
            span(class = "custom-model-paper-status", canvas_spec$status),
            span(class = "custom-model-covariate-status", ""),
            span(class = "structural-validation-status", gsub("{warnings}", "0", gsub("{errors}", "0", labels_i18n$validation_counts, fixed = TRUE), fixed = TRUE))),
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
    analysis_canvas_sidebar(root, language, uiOutput(paste0(prefix, "_save_control"))),
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
        div(class = "app-heading", h1(title), div(statedu_localized_text(language, "Build CFA and SEM models with observed and latent variables.", "관측변수와 잠재변수를 배치하여 CFA와 SEM 모형을 작성합니다."), class = "app-subtitle")),
        div(class = "workspace-panel frequencies-workspace-panel custom-model-workspace-panel structural-equation-workspace-panel",
            div(
              class = paste("analysis-workspace-heading", paste0(prefix, "-workspace-heading")),
              div(class = "analysis-workspace-heading-main", h3(analysis_ui_text(title, language)))
            ),
            analysis_workspace_body(prefix, uiOutput(paste0(prefix, "_canvas_setup")), NULL, NULL)))
  )
}
