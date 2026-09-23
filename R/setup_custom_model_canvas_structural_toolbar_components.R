# Structural equation canvas toolbar components.

structural_equation_toolbar <- function(analysis_type = "cbsem", language = statedu_initial_language(), grouping_choices = character(0), option_values = list()) {
  prefix <- structural_analysis_prefix(analysis_type)
  div(
    class = "custom-model-toolbar",
    div(
      class = "custom-model-toolbar-panel is-active",
      `data-toolbar-panel` = "tools",
      div(
        class = "structural-primary-toolbar-tools",
      analysis_canvas_command_button(language),
      custom_model_canvas_button("load", statedu_localized_text(language, "Load model", "모형 불러오기"), title = statedu_localized_text(language, "Load a saved model", "저장한 모형 불러오기"), icon = structural_file_icon("open")),
      custom_model_canvas_button("save", statedu_localized_text(language, "Save model", "모형 저장"), title = statedu_localized_text(language, "Save the current model", "현재 모형 저장하기"), icon = structural_file_icon("save")),
      custom_model_canvas_button("resultLoad", statedu_localized_text(language, "Load result", "분석 결과 불러오기"), title = statedu_localized_text(language, "Load a saved analysis result", "저장한 분석 결과 불러오기"), icon = structural_file_icon("resultOpen")),
      custom_model_canvas_button("resultSave", statedu_localized_text(language, "Save result", "분석 결과 저장"), title = statedu_localized_text(language, "Save the current analysis result", "현재 분석 결과 저장하기"), icon = structural_file_icon("resultSave")),
      custom_model_canvas_button("export", statedu_localized_text(language, "Export PNG", "PNG 그림 내보내기")),
      if (analysis_type %in% c("cfa", "cbsem", "sem", "plssem")) tagList(
        tags$button(
          type = "button",
          class = "custom-model-toolbar-button structural-covariate-toolbar-button",
          `data-role` = "covariate",
          title = statedu_localized_text(language, "Assign selected variables as covariates", "선택한 관측변수를 공변량으로 지정"),
          span(class = "custom-model-toolbar-icon", "C"),
          span(class = "custom-model-toolbar-label", statedu_localized_text(language, "Assign covariate", "공변량 지정"))
        ),
        custom_model_canvas_button("structuralCovariateTargets", statedu_localized_text(language, "Covariate targets", "공변량 설정"), title = statedu_localized_text(language, "Set control targets for each covariate", "공변량별 통제 대상 설정"), icon = structural_file_icon("settings"))
      ),
      custom_model_canvas_button("addLatent", statedu_localized_text(language, "Latent variable", "잠재변수"), extra_class = "structural-add-latent"),
      if (analysis_type %in% c("cfa", "cbsem")) custom_model_canvas_button(
        "addHigherOrderLatent",
        statedu_localized_text(language, "Higher-order", "고차요인"),
        title = statedu_localized_text(language, "Add a higher-order latent variable. Connections to latent variables become higher-order loadings.", "고차 잠재변수를 배치하고, 잠재변수로 연결하면 2차 CFA 적재로 자동 지정됩니다."),
        extra_class = "structural-add-higher-order",
        icon = structural_higher_order_icon()
      ),
      custom_model_canvas_button("select", custom_model_canvas_text(language, "Select", "선택"), mode = TRUE),
      if (identical(analysis_type, "cfa")) custom_model_canvas_button("flipCfa", statedu_localized_text(language, "Flip sides", "좌우 반전"), title = statedu_localized_text(language, "Flip latent variables and indicators", "잠재변수와 측정변수 좌우 반전"), mode = TRUE),
      custom_model_canvas_button("connect", custom_model_canvas_text(language, "Connect", "연결"), mode = TRUE),
      custom_model_canvas_button("covariance", statedu_localized_text(language, "Covariance", "공분산"), title = statedu_localized_text(language, "Draw covariance", "공분산 연결"), mode = TRUE),
      custom_model_canvas_button("properties", custom_model_canvas_text(language, "Properties", "속성"), mode = TRUE),
      custom_model_canvas_button("detachIndicator", statedu_localized_text(language, "Detach indicator", "지표 분리"), title = statedu_localized_text(language, "Detach selected indicator", "선택 측정변수를 잠재변수에서 분리")),
      custom_model_canvas_button("indicatorUp", statedu_localized_text(language, "Indicator up", "지표 앞으로"), title = statedu_localized_text(language, "Move indicator earlier", "측정변수 순서를 앞으로")),
      custom_model_canvas_button("indicatorDown", statedu_localized_text(language, "Indicator down", "지표 뒤로"), title = statedu_localized_text(language, "Move indicator later", "측정변수 순서를 뒤로")),
      custom_model_canvas_button("alignIndicators", statedu_localized_text(language, "Align indicators", "측정변수 정렬"), title = statedu_localized_text(language, "Align selected measurement groups, or all groups when nothing is selected. Move errors together; preserve latent positions and existing spacing.", "선택한 측정변수 묶음을 정렬합니다. 선택이 없으면 전체 묶음을 정렬합니다. 오차항은 함께 이동하며 잠재변수 위치와 기존 간격은 유지합니다."),
        icon = tags$svg(viewBox = "0 0 24 24", width = "20", height = "20", fill = "none", stroke = "currentColor", `stroke-width` = "1.6", `aria-hidden` = "true",
          if (identical(analysis_type, "plssem")) tagList(
            tags$path(d = "M2 18 H22"),
            tags$rect(x = "2", y = "7", width = "5", height = "8", rx = "1"),
            tags$rect(x = "9.5", y = "7", width = "5", height = "8", rx = "1"),
            tags$rect(x = "17", y = "7", width = "5", height = "8", rx = "1")
          ) else tagList(
            tags$circle(cx = "4.5", cy = "4", r = "2.2"),
            tags$circle(cx = "12", cy = "4", r = "2.2"),
            tags$circle(cx = "19.5", cy = "4", r = "2.2"),
            tags$path(d = "M4.5 6.2 V11 M12 6.2 V11 M19.5 6.2 V11 M2 21 H22"),
            tags$rect(x = "2", y = "11", width = "5", height = "6", rx = "1"),
            tags$rect(x = "9.5", y = "11", width = "5", height = "6", rx = "1"),
            tags$rect(x = "17", y = "11", width = "5", height = "6", rx = "1")
          ))),
      custom_model_canvas_button("delete", custom_model_canvas_text(language, "Delete", "삭제"), mode = TRUE),
      custom_model_canvas_button("undo", custom_model_canvas_text(language, "Undo", "실행 취소")),
      custom_model_canvas_button("redo", custom_model_canvas_text(language, "Redo", "다시 실행")),
      custom_model_canvas_button("grid", custom_model_canvas_text(language, "Grid", "격자")),
      custom_model_canvas_button(
        "autoAlign",
        statedu_localized_text(language, "Snap alignment", "자동 맞춤"),
        title = statedu_localized_text(language, "Snap a moved construct to nearby latent-variable axes", "이동 중 가까운 잠재변수의 가로·세로 중심축에 자동으로 맞춤"),
        mode = TRUE
      ),
      custom_model_canvas_button("zoomIn", statedu_localized_text(language, "Zoom in", "모형 확대"), title = statedu_localized_text(language, "Zoom model in", "캔버스 안의 모형 확대")),
      custom_model_canvas_button("zoomOut", statedu_localized_text(language, "Zoom out", "모형 축소"), title = statedu_localized_text(language, "Zoom model out", "캔버스 안의 모형 축소")),
      custom_model_canvas_button(
        "fit",
        statedu_localized_text(language, "Center model", "모형 중앙 맞춤"),
        title = statedu_localized_text(language, "Center the entire model on the paper and fit the view", "모형 전체를 용지 중앙에 배치하고 화면에 맞춤")
      ),
      custom_model_canvas_button("reset", statedu_localized_text(language, "Reset model", "모형 초기화"), title = statedu_localized_text(language, "Clear the entire canvas model", "캔버스의 모형 전체 초기화"), extra_class = "custom-model-reset-button"),
      div(
        class = "custom-model-reset-confirm-popover",
        div(class = "custom-model-reset-confirm-title", statedu_localized_text(language, "Reset model", "모형 초기화")),
        div(class = "custom-model-reset-confirm-message", statedu_localized_text(language, "Clear all variables and paths?", "모든 변수와 연결선을 초기화할까요?")),
        div(
          class = "custom-model-reset-confirm-actions",
          tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "resetCancel", statedu_localized_text(language, "Cancel", "취소")),
          tags$button(type = "button", class = "btn btn-warning btn-sm", `data-action` = "resetConfirm", statedu_localized_text(language, "Reset", "초기화"))
        )
      ),
      custom_model_canvas_button("run", statedu_localized_text(language, "Run analysis", "분석 실행"), title = statedu_localized_text(language, "Run the current model", "현재 모형 분석 실행")),
      div(
        class = "custom-model-run-options-popover structural-run-options-popover",
        div(class = "custom-model-run-options-title", statedu_localized_text(language, "Analysis options", "분석 옵션")),
        structural_canvas_restore_options(structural_analysis_options_panel(analysis_type, language, grouping_choices), option_values),
        div(
          class = "custom-model-run-options-actions",
          tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "runCancel", statedu_localized_text(language, "Cancel", "취소")),
          tags$button(type = "button", class = "btn btn-primary btn-sm", `data-action` = "runConfirm", statedu_localized_text(language, "Run", "실행"))
        )
      )
      ),
      div(
        class = "structural-secondary-toolbar-tools",
        if (!identical(analysis_type, "cfa")) div(
          class = paste("structural-latent-tools", if (identical(analysis_type, "plssem")) "structural-latent-tools-pls" else "structural-latent-tools-basic"),
          custom_model_canvas_button("placementLeft", statedu_localized_text(language, "Left", "왼쪽"), title = statedu_localized_text(language, "Indicators left", "측정변수를 왼쪽으로"), icon = structural_measurement_icon("left")),
          custom_model_canvas_button("placementRight", statedu_localized_text(language, "Right", "오른쪽"), title = statedu_localized_text(language, "Indicators right", "측정변수를 오른쪽으로"), icon = structural_measurement_icon("right")),
          custom_model_canvas_button("placementTop", statedu_localized_text(language, "Top", "위"), title = statedu_localized_text(language, "Indicators above", "측정변수를 위로"), icon = structural_measurement_icon("top")),
          custom_model_canvas_button("placementBottom", statedu_localized_text(language, "Bottom", "아래"), title = statedu_localized_text(language, "Indicators below", "측정변수를 아래로"), icon = structural_measurement_icon("bottom")),
          if (identical(analysis_type, "plssem")) tagList(
            span(class = "structural-toolbar-separator"),
            custom_model_canvas_button("reflective", statedu_localized_text(language, "Reflective", "반영지표"), title = statedu_localized_text(language, "Reflective measurement", "반영지표: 잠재변수 → 측정변수"), icon = structural_measurement_icon("reflective")),
            custom_model_canvas_button("formative", statedu_localized_text(language, "Formative", "형성지표"), title = statedu_localized_text(language, "Formative measurement", "형성지표: 측정변수 → 잠재변수"), icon = structural_measurement_icon("formative"))
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
              statedu_localized_text(language, "Coefficient", "계수")
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
              statedu_localized_text(language, "Measurement paths", "측정경로")
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
              custom_model_canvas_button("latentStats", statedu_localized_text(language, "Latent statistics", "잠재변수 통계"), title = statedu_localized_text(language, "Choose latent-variable statistics to display", "표시할 잠재변수 통계량 선택"), mode = TRUE),
              div(
                class = "structural-latent-stats-popover",
                div(class = "structural-latent-stats-title", statedu_localized_text(language, "Statistics to display", "표시할 통계량")),
                Map(function(key, label) {
                  tags$label(class = "structural-latent-stats-option",
                    tags$input(type = "radio", name = paste0(prefix, "_latent_stat"), `data-latent-stat` = key, checked = if (identical(key, "r2")) "checked" else NULL),
                    as.character(label)
                  )
                }, c("r2", "ave", "cr", "none"), c("R²", "AVE", "CR", statedu_localized_text(language, "None", "없음")))
              )
            ) else span(class = "structural-result-icon-placeholder", `aria-hidden` = "true"),
            custom_model_canvas_button("resultEdit", statedu_localized_text(language, "Edit result", "결과 편집"), mode = TRUE),
            custom_model_canvas_button("dashNonsignificant", statedu_localized_text(language, "Non-significant dashed", "비유의 점선"), mode = TRUE),
            custom_model_canvas_button("style", statedu_localized_text(language, "Style", "스타일"))
          )
        )
      ),
      div(class = "structural-disturbance-toolbar", `aria-live` = "polite")
    )
  )
}
