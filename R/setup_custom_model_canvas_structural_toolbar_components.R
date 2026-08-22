# Structural equation canvas toolbar components.

structural_equation_toolbar <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  prefix <- structural_analysis_prefix(analysis_type)
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
      if (analysis_type %in% c("cfa", "cbsem", "sem", "plssem")) tagList(
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
      if (analysis_type %in% c("cfa", "cbsem")) custom_model_canvas_button(
        "addHigherOrderLatent",
        if (ko) "고차요인" else "Higher-order",
        title = if (ko) "고차 잠재변수를 배치하고, 잠재변수로 연결하면 2차 CFA 적재로 자동 지정됩니다." else "Add a higher-order latent variable. Connections to latent variables become higher-order loadings.",
        extra_class = "structural-add-higher-order",
        icon = structural_higher_order_icon()
      ),
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
      custom_model_canvas_button("autoLayout", if (ko) "자동 정렬" else "Auto layout", title = if (ko) "역할 열과 측정모형을 자동으로 다시 배치" else "Automatically arrange role columns and measurement blocks"),
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
      div(
        class = "structural-secondary-toolbar-tools",
        if (!identical(analysis_type, "cfa")) div(
          class = "structural-advanced-analysis-tools",
          custom_model_canvas_button("multiGroup", if (ko) "다집단 분석" else "Multigroup", title = if (ko) "다집단 분석 설정" else "Multigroup analysis settings"),
          custom_model_canvas_button("moderator", if (ko) "조절변수" else "Moderator", title = if (ko) "조절효과 설정" else "Moderation settings")
        ),
        if (!identical(analysis_type, "cfa")) div(
          class = paste("structural-latent-tools", if (identical(analysis_type, "plssem")) "structural-latent-tools-pls" else "structural-latent-tools-basic"),
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
        div(
          class = "structural-result-tools",
          div(
            class = "structural-result-coefficient-control",
            tags$label(
              class = "structural-result-coefficient-label",
              `for` = paste0(prefix, "_result_coefficient"),
              if (ko) "계수" else "Coefficient"
            ),
            tags$select(
              id = paste0(prefix, "_result_coefficient"),
              class = "form-control input-sm structural-result-coefficient-select",
              lapply(names(structural_canvas_result_coefficient_choices(language, analysis_type)), function(label) {
                value <- structural_canvas_result_coefficient_choices(language, analysis_type)[[label]]
                default_value <- if (identical(analysis_type, "plssem")) "pls_p" else "beta_p"
                tags$option(value = value, selected = if (identical(value, default_value)) "selected" else NULL, label)
              })
            )
          ),
          if (identical(analysis_type, "plssem")) div(
            class = "structural-result-coefficient-control",
            tags$label(
              class = "structural-result-coefficient-label",
              `for` = paste0(prefix, "_result_measurement_coefficient"),
              if (ko) "측정경로" else "Measurement paths"
            ),
            tags$select(
              id = paste0(prefix, "_result_measurement_coefficient"),
              class = "form-control input-sm structural-result-coefficient-select",
              lapply(names(structural_canvas_measurement_coefficient_choices(language)), function(label) {
                value <- structural_canvas_measurement_coefficient_choices(language)[[label]]
                tags$option(value = value, selected = if (identical(value, "measurement_p")) "selected" else NULL, label)
              })
            )
          ),
          div(
            class = "structural-result-icon-grid",
            if (analysis_type %in% c("cbsem", "sem", "plssem")) tagList(
              custom_model_canvas_button("latentStats", if (ko) "잠재변수 통계" else "Latent statistics", title = if (ko) "표시할 잠재변수 통계량 선택" else "Choose latent-variable statistics to display", mode = TRUE),
              div(
                class = "structural-latent-stats-popover",
                div(class = "structural-latent-stats-title", if (ko) "표시할 통계량" else "Statistics to display"),
                Map(function(key, label) {
                  tags$label(class = "structural-latent-stats-option",
                    tags$input(type = "radio", name = paste0(prefix, "_latent_stat"), `data-latent-stat` = key, checked = if (identical(key, "r2")) "checked" else NULL),
                    as.character(label)
                  )
                }, c("r2", "ave", "cr", "none"), c("R²", "AVE", "CR", if (ko) "없음" else "None"))
              )
            ) else span(class = "structural-result-icon-placeholder", `aria-hidden` = "true"),
            custom_model_canvas_button("resultView", if (ko) "결과 모형" else "Result diagram"),
            custom_model_canvas_button("resultEdit", if (ko) "결과 편집" else "Edit result", mode = TRUE),
            custom_model_canvas_button("dashNonsignificant", if (ko) "비유의 점선" else "Non-significant dashed", mode = TRUE),
            custom_model_canvas_button("style", if (ko) "스타일" else "Style")
          )
        )
      ),
      div(class = "structural-disturbance-toolbar", `aria-live` = "polite")
    )
  )
}
