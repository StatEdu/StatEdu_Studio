# Structural equation canvas UI components.

structural_measurement_icon <- function(kind) {
  marker_id <- paste0("sem-arrow-", kind)
  svg <- function(...) tags$svg(class = "structural-measurement-svg", viewBox = "0 0 68 42", `aria-hidden` = "true", ...)
  marker <- tags$defs(tags$marker(id = marker_id, viewBox = "0 0 6 6", refX = "5", refY = "3", markerWidth = "5", markerHeight = "5", orient = "auto", tags$path(d = "M0,0 L6,3 L0,6 Z", fill = "currentColor")))
  line <- function(x1, y1, x2, y2, arrow = FALSE) tags$line(x1 = x1, y1 = y1, x2 = x2, y2 = y2, `marker-end` = if (arrow) paste0("url(#", marker_id, ")") else NULL)
  box <- function(x, y) tags$rect(x = x, y = y, width = "17", height = "7", rx = "1")
  latent <- function(cx, cy) tags$ellipse(cx = cx, cy = cy, rx = "10", ry = "7")
  if (kind == "left") return(svg(marker, latent(55, 21), box(3, 5), box(3, 18), box(3, 31), line(20, 8.5, 45, 18), line(20, 21.5, 45, 21), line(20, 34.5, 45, 24)))
  if (kind == "right") return(svg(marker, latent(13, 21), box(48, 5), box(48, 18), box(48, 31), line(23, 18, 48, 8.5), line(23, 21, 48, 21.5), line(23, 24, 48, 34.5)))
  if (kind == "top") return(svg(marker, latent(34, 34), box(3, 3), box(25.5, 3), box(48, 3), line(29, 28, 11.5, 10), line(34, 27, 34, 10), line(39, 28, 56.5, 10)))
  if (kind == "bottom") return(svg(marker, latent(34, 8), box(3, 32), box(25.5, 32), box(48, 32), line(29, 14, 11.5, 32), line(34, 15, 34, 32), line(39, 14, 56.5, 32)))
  reflective <- identical(kind, "reflective")
  svg(marker, latent(13, 21), box(48, 5), box(48, 18), box(48, 31),
      line(if (reflective) 23 else 48, if (reflective) 18 else 8.5, if (reflective) 48 else 23, if (reflective) 8.5 else 18, TRUE),
      line(if (reflective) 23 else 48, 21, if (reflective) 48 else 23, 21, TRUE),
      line(if (reflective) 23 else 48, if (reflective) 24 else 34.5, if (reflective) 48 else 23, if (reflective) 34.5 else 24, TRUE))
}

structural_file_icon <- function(kind) {
  svg <- function(...) tags$svg(
    class = "structural-common-toolbar-svg",
    viewBox = "0 0 24 24",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "1.8",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    `aria-hidden` = "true",
    ...
  )
  if (identical(kind, "load")) {
    return(svg(
      tags$path(d = "M3 7.5h6l2-2h4.5a2 2 0 0 1 2 2v1"),
      tags$path(d = "M3.5 9.5h17l-2.2 9H5.7z"),
      tags$path(d = "M12 17v-5"),
      tags$path(d = "m9.8 14.2 2.2-2.2 2.2 2.2")
    ))
  }
  if (identical(kind, "save")) {
    return(svg(
      tags$path(d = "M4 3.5h13l3 3V20H4z"),
      tags$path(d = "M8 3.5v6h8v-6"),
      tags$rect(x = "7", y = "13", width = "10", height = "7", rx = "1")
    ))
  }
  svg(
    tags$path(d = "M4 7h10"), tags$circle(cx = "17", cy = "7", r = "2"), tags$path(d = "M19 7h1"),
    tags$path(d = "M4 12h3"), tags$circle(cx = "10", cy = "12", r = "2"), tags$path(d = "M12 12h8"),
    tags$path(d = "M4 17h8"), tags$circle(cx = "15", cy = "17", r = "2"), tags$path(d = "M17 17h3")
  )
}

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

structural_equation_toolbar <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  div(
    class = "custom-model-toolbar",
    div(
      class = "custom-model-toolbar-panel is-active",
      `data-toolbar-panel` = "tools",
      div(
        class = "structural-primary-toolbar-tools",
      custom_model_canvas_button("load", if (ko) "모형 불러오기" else "Load model", title = if (ko) "저장한 모형 불러오기" else "Load a saved model", icon = structural_file_icon("load")),
      custom_model_canvas_button("save", if (ko) "모형 저장" else "Save model", title = if (ko) "현재 모형 저장하기" else "Save the current model", icon = structural_file_icon("save")),
      custom_model_canvas_button("export", if (ko) "모형 내보내기" else "Export model"),
      if (!identical(analysis_type, "cfa")) tagList(
        tags$button(
          type = "button",
          class = "custom-model-toolbar-button structural-covariate-toolbar-button",
          `data-role` = "covariate",
          title = if (ko) "선택한 관측변수를 공변량으로 지정" else "Assign selected variables as covariates",
          span(class = "custom-model-toolbar-icon", "C"),
          span(class = "custom-model-toolbar-label", if (ko) "공변량 지정" else "Assign covariate")
        ),
        custom_model_canvas_button("structuralCovariateTargets", if (ko) "공변량 설정" else "Covariate targets", title = if (ko) "공변량별 통제 대상 설정" else "Set control targets for each covariate", icon = structural_file_icon("settings"))
      ),
      custom_model_canvas_button("addLatent", if (ko) "잠재변수" else "Latent variable", extra_class = "structural-add-latent"),
      custom_model_canvas_button("select", custom_model_canvas_text(language, "Select", "선택"), mode = TRUE),
      if (identical(analysis_type, "cfa")) custom_model_canvas_button("flipCfa", if (ko) "좌우 반전" else "Flip sides", title = if (ko) "잠재변수와 측정변수 좌우 반전" else "Flip latent variables and indicators", mode = TRUE),
      custom_model_canvas_button("connect", custom_model_canvas_text(language, "Connect", "연결"), mode = TRUE),
      custom_model_canvas_button("covariance", if (ko) "공분산" else "Covariance", title = if (ko) "공분산 연결" else "Draw covariance", mode = TRUE),
      custom_model_canvas_button("properties", custom_model_canvas_text(language, "Properties", "속성"), mode = TRUE),
      custom_model_canvas_button("detachIndicator", if (ko) "지표 분리" else "Detach indicator", title = if (ko) "선택 측정변수를 잠재변수에서 분리" else "Detach selected indicator"),
      custom_model_canvas_button("indicatorUp", if (ko) "지표 앞으로" else "Indicator up", title = if (ko) "측정변수 순서를 앞으로" else "Move indicator earlier"),
      custom_model_canvas_button("indicatorDown", if (ko) "지표 뒤로" else "Indicator down", title = if (ko) "측정변수 순서를 뒤로" else "Move indicator later"),
      custom_model_canvas_button("alignLeft", if (ko) "왼쪽 정렬" else "Align left", title = if (ko) "선택 항목 왼쪽 정렬" else "Align selected left"),
      custom_model_canvas_button("alignTop", if (ko) "위 정렬" else "Align top", title = if (ko) "선택 항목 위 정렬" else "Align selected top"),
      custom_model_canvas_button("alignCenter", if (ko) "가운데 정렬" else "Center", title = if (ko) "선택 항목 가로 중앙 정렬" else "Align horizontal centers"),
      custom_model_canvas_button("alignMiddle", if (ko) "세로 중앙" else "Middle", title = if (ko) "선택 항목 세로 중앙 정렬" else "Align vertical centers"),
      custom_model_canvas_button("distributeH", if (ko) "가로 분배" else "Distribute", title = if (ko) "선택 항목 가로 균등 배치" else "Distribute horizontally"),
      custom_model_canvas_button("distributeV", if (ko) "세로 분배" else "Distribute vertical", title = if (ko) "선택 항목 세로 균등 배치" else "Distribute vertically"),
      custom_model_canvas_button("delete", custom_model_canvas_text(language, "Delete", "삭제"), mode = TRUE),
      custom_model_canvas_button("undo", custom_model_canvas_text(language, "Undo", "실행 취소")),
      custom_model_canvas_button("redo", custom_model_canvas_text(language, "Redo", "다시 실행")),
      custom_model_canvas_button("grid", custom_model_canvas_text(language, "Grid", "격자")),
      custom_model_canvas_button("zoomIn", if (ko) "모형 확대" else "Zoom in", title = if (ko) "캔버스 안의 모형 확대" else "Zoom model in"),
      custom_model_canvas_button("zoomOut", if (ko) "모형 축소" else "Zoom out", title = if (ko) "캔버스 안의 모형 축소" else "Zoom model out"),
      custom_model_canvas_button("fit", custom_model_canvas_text(language, "Fit", "화면 맞춤")),
      custom_model_canvas_button("reset", if (ko) "모형 초기화" else "Reset model", title = if (ko) "캔버스의 모형 전체 초기화" else "Clear the entire canvas model", extra_class = "custom-model-reset-button"),
      div(
        class = "custom-model-reset-confirm-popover",
        div(class = "custom-model-reset-confirm-title", if (ko) "모형 초기화" else "Reset model"),
        div(class = "custom-model-reset-confirm-message", if (ko) "모든 변수와 연결선을 초기화할까요?" else "Clear all variables and paths?"),
        div(
          class = "custom-model-reset-confirm-actions",
          tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "resetCancel", if (ko) "취소" else "Cancel"),
          tags$button(type = "button", class = "btn btn-warning btn-sm", `data-action` = "resetConfirm", if (ko) "초기화" else "Reset")
        )
      ),
      custom_model_canvas_button("run", if (ko) "분석 실행" else "Run analysis", title = if (ko) "현재 모형 분석 실행" else "Run the current model"),
      div(
        class = "custom-model-run-options-popover structural-run-options-popover",
        div(class = "custom-model-run-options-title", if (ko) "분석 옵션" else "Analysis options"),
        structural_analysis_options_panel(analysis_type, language),
        div(
          class = "custom-model-run-options-actions",
          tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "runCancel", if (ko) "취소" else "Cancel"),
          tags$button(type = "button", class = "btn btn-primary btn-sm", `data-action` = "runConfirm", if (ko) "실행" else "Run")
        )
      )
      ),
      if (!identical(analysis_type, "cfa")) div(
        class = "structural-advanced-analysis-tools",
        custom_model_canvas_button("multiGroup", if (ko) "다집단 분석" else "Multigroup", title = if (ko) "다집단 분석 설정" else "Multigroup analysis settings"),
        custom_model_canvas_button("moderator", if (ko) "조절변수" else "Moderator", title = if (ko) "조절효과 설정" else "Moderation settings")
      ),
      if (!identical(analysis_type, "cfa")) div(
        class = "structural-latent-tools",
        span(class = "structural-latent-tools-label", if (ko) "측정모형" else "Measurement"),
        custom_model_canvas_button("placementLeft", if (ko) "왼쪽" else "Left", title = if (ko) "측정변수를 왼쪽으로" else "Indicators left", icon = structural_measurement_icon("left")),
        custom_model_canvas_button("placementRight", if (ko) "오른쪽" else "Right", title = if (ko) "측정변수를 오른쪽으로" else "Indicators right", icon = structural_measurement_icon("right")),
        custom_model_canvas_button("placementTop", if (ko) "위" else "Top", title = if (ko) "측정변수를 위로" else "Indicators above", icon = structural_measurement_icon("top")),
        custom_model_canvas_button("placementBottom", if (ko) "아래" else "Bottom", title = if (ko) "측정변수를 아래로" else "Indicators below", icon = structural_measurement_icon("bottom")),
        if (identical(analysis_type, "plssem")) tagList(
          span(class = "structural-toolbar-separator"),
          custom_model_canvas_button("reflective", if (ko) "반영지표" else "Reflective", title = if (ko) "반영지표: 잠재변수 → 측정변수" else "Reflective measurement", icon = structural_measurement_icon("reflective")),
          custom_model_canvas_button("formative", if (ko) "형성지표" else "Formative", title = if (ko) "형성지표: 측정변수 → 잠재변수" else "Formative measurement", icon = structural_measurement_icon("formative"))
        )
      ),
      custom_model_canvas_edge_shape_tools(language),
      custom_model_canvas_edge_anchor_tools(language),
      div(
        class = "structural-result-tools",
        custom_model_canvas_button("resultView", if (ko) "결과 모형" else "Result diagram"),
        custom_model_canvas_button("resultEdit", if (ko) "결과 편집" else "Edit result", mode = TRUE),
        custom_model_canvas_button("dashNonsignificant", if (ko) "비유의 점선" else "Non-significant dashed", mode = TRUE),
        custom_model_canvas_button("style", if (ko) "스타일" else "Style")
      ),
      div(class = "structural-disturbance-toolbar", `aria-live` = "polite")
    )
  )
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
  root <- div(
      id = paste0(prefix, "-canvas-root"),
      class = "custom-model-canvas-root structural-equation-canvas-root",
      `data-input-prefix` = paste0(prefix, "_canvas"),
      `data-analysis-type` = analysis_type,
      `data-analysis-package` = structural_analysis_package(analysis_type),
      `data-canvas-width` = "1600",
      `data-canvas-height` = "1000",
      `data-canvas-paper` = "Large",
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
            span(class = "custom-model-paper-status", "Large 1600×1000"),
            span(class = "custom-model-covariate-status", ""),
            span(class = "structural-validation-status", "오류 0 · 경고 0")),
        div(class = "custom-model-canvas-scroll",
            div(class = "custom-model-paper is-grid-visible", `data-width` = "1600", `data-height` = "1000",
                tags$svg(class = "custom-model-edge-layer", width = "1600", height = "1000"),
                div(class = "custom-model-node-layer")))
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
