# Custom model canvas toolbar UI components.

custom_model_canvas_button <- function(action, label, title = label, mode = FALSE, extra_class = "", icon = NULL) {
  if (action %in% c("select", "autoAlign")) {
    ko <- grepl("[가-힣]", label)
    label <- if (action == "select") {
      if (ko) "선택·이동" else "Select / move"
    } else {
      if (ko) "이동 시 정렬: 켬" else "Snap while moving: On"
    }
    title <- if (action == "select") {
      if (ko) "변수를 클릭하여 선택하거나 드래그하여 이동합니다." else "Click a variable to select it; drag to move it."
    } else {
      if (ko) "변수를 이동할 때 주변 변수의 정렬선에 맞춥니다. 클릭하면 켜거나 끕니다." else "Snap to nearby variable alignment guides while dragging. Click to turn on or off."
    }
    extra_class <- paste(extra_class, "custom-model-icon-tool")
    icon <- tags$svg(
      viewBox = "0 0 24 24", width = "20", height = "20", `aria-hidden` = "true",
      fill = "none", stroke = "currentColor", `stroke-width` = "1.7",
      `stroke-linecap` = "round", `stroke-linejoin` = "round",
      if (action == "select") {
        tags$path(d = "M5 3 L5 19 L9 15 L12 21 L15 19.5 L12 13.5 L18 13 Z", fill = "currentColor", stroke = "none")
      } else {
        tagList(
          tags$path(class = "custom-model-alignment-guide",
            d = "M8 3 H16 M8 8.5 H16 M1.5 8.5 V17 M8 8.5 V17",
            stroke = "#e53935", `stroke-width` = "1.4", `stroke-dasharray` = "1.6 1.8"),
          tags$rect(x = "16", y = "3", width = "6.5", height = "5.5", rx = ".5"),
          tags$rect(x = "1.5", y = "17", width = "6.5", height = "5.5", rx = ".5"),
          tags$rect(class = "custom-model-alignment-reference", x = "1.5", y = "3", width = "6.5", height = "5.5", rx = ".5",
            fill = "#8cc8ff", stroke = "#0754aa", `stroke-width` = "2")
        )
      }
    )
  }
  tags$button(
    type = "button",
    class = paste("custom-model-toolbar-button", extra_class, if (isTRUE(mode)) "is-mode-button" else ""),
    `data-action` = action,
    title = title,
    `aria-label` = label,
    span(class = "custom-model-toolbar-icon", icon),
    span(class = "custom-model-toolbar-label", label)
  )
}

custom_model_canvas_edge_shape_tools <- function(language = statedu_initial_language()) {
  div(
    class = "custom-model-edge-shape-tools",
    `data-edge-shape-tools` = "true",
    tags$button(type = "button", class = "custom-model-edge-shape-button", `data-edge-shape` = "straight", title = custom_model_canvas_text(language, "Straight", "\uc9c1\uc120"), "\u2500"),
    tags$button(type = "button", class = "custom-model-edge-shape-button", `data-edge-shape` = "curveUp", title = custom_model_canvas_text(language, "Curve up", "\uc704\ub85c \uace1\uc120"), "\u2322"),
    tags$button(type = "button", class = "custom-model-edge-shape-button", `data-edge-shape` = "curveDown", title = custom_model_canvas_text(language, "Curve down", "\uc544\ub798\ub85c \uace1\uc120"), "\u2323")
  )
}

custom_model_canvas_edge_anchor_tools <- function(language = statedu_initial_language()) {
  side_button <- function(endpoint, side, label, title) {
    tags$button(
      type = "button",
      class = "custom-model-edge-anchor-button",
      `data-edge-anchor-endpoint` = endpoint,
      `data-edge-anchor-side` = side,
      title = title,
      label
    )
  }
  div(
    class = "custom-model-edge-anchor-tools",
    `data-edge-anchor-tools` = "true",
    span(class = "custom-model-edge-anchor-label", custom_model_canvas_text(language, "Start", "\uc2dc\uc791")),
    side_button("from", "auto", "A", custom_model_canvas_text(language, "Start auto", "\uc2dc\uc791 \uc790\ub3d9")),
    side_button("from", "top", "\u2191", custom_model_canvas_text(language, "Start top", "\uc2dc\uc791 \uc704")),
    side_button("from", "right", "\u2192", custom_model_canvas_text(language, "Start right", "\uc2dc\uc791 \uc624\ub978\ucabd")),
    side_button("from", "bottom", "\u2193", custom_model_canvas_text(language, "Start bottom", "\uc2dc\uc791 \uc544\ub798")),
    side_button("from", "left", "\u2190", custom_model_canvas_text(language, "Start left", "\uc2dc\uc791 \uc67c\ucabd")),
    span(class = "custom-model-edge-anchor-label", custom_model_canvas_text(language, "End", "\ub05d")),
    side_button("to", "auto", "A", custom_model_canvas_text(language, "End auto", "\ub05d \uc790\ub3d9")),
    side_button("to", "top", "\u2191", custom_model_canvas_text(language, "End top", "\ub05d \uc704")),
    side_button("to", "right", "\u2192", custom_model_canvas_text(language, "End right", "\ub05d \uc624\ub978\ucabd")),
    side_button("to", "bottom", "\u2193", custom_model_canvas_text(language, "End bottom", "\ub05d \uc544\ub798")),
    side_button("to", "left", "\u2190", custom_model_canvas_text(language, "End left", "\ub05d \uc67c\ucabd"))
  )
}

custom_model_canvas_legacy_toolbar <- function(input = NULL, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  group_tab <- function(group, label, active = FALSE) {
    tags$button(
      type = "button",
      class = paste("custom-model-toolbar-tab", if (isTRUE(active)) "is-active" else ""),
      `data-toolbar-group` = group,
      label
    )
  }

  group_panel <- function(group, ..., active = FALSE) {
    div(
      class = paste("custom-model-toolbar-panel", if (isTRUE(active)) "is-active" else ""),
      `data-toolbar-panel` = group,
      ...
    )
  }

  div(
    class = "custom-model-toolbar",
    div(
      class = "custom-model-toolbar-tabs",
      group_tab("file", custom_model_canvas_text(language, "File", "\ud30c\uc77c"), active = TRUE),
      group_tab("analysis", custom_model_canvas_text(language, "Analysis", "\ubd84\uc11d")),
      group_tab("tools", custom_model_canvas_text(language, "Tools", "\ub3c4\uad6c")),
      group_tab("view", custom_model_canvas_text(language, "View", "\ubcf4\uae30")),
      group_tab("result", custom_model_canvas_text(language, "Result", "\uacb0\uacfc"))
    ),
    div(
      class = "custom-model-toolbar-panels",
      group_panel(
        "file",
        analysis_canvas_command_button(language),
        custom_model_canvas_button("load", custom_model_canvas_text(language, "Load", "\ubd88\ub7ec\uc624\uae30")),
        custom_model_canvas_button("save", custom_model_canvas_text(language, "Save", "\uc800\uc7a5")),
        custom_model_canvas_button("export", custom_model_canvas_text(language, "Export PNG", "PNG \uadf8\ub9bc \ub0b4\ubcf4\ub0b4\uae30")),
        active = TRUE
      ),
      group_panel(
        "analysis",
        custom_model_canvas_button("run", custom_model_canvas_text(language, "Run", "\uc2e4\ud589")),
        div(
          class = "custom-model-run-options-popover",
          div(class = "custom-model-run-options-title", custom_model_canvas_text(language, "Analysis options", "\ubd84\uc11d \uc635\uc158")),
          custom_model_canvas_analysis_options(input, language),
          div(
            class = "custom-model-run-options-actions",
            tags$button(type = "button", class = "btn btn-default btn-sm custom-model-run-options-cancel", `data-action` = "runCancel", custom_model_canvas_text(language, "Cancel", "\ucde8\uc18c")),
            tags$button(type = "button", class = "btn btn-primary btn-sm custom-model-run-options-confirm", `data-action` = "runConfirm", custom_model_canvas_text(language, "Run", "\uc2e4\ud589"))
          )
        )
      ),
      group_panel(
        "tools",
        custom_model_canvas_button("select", custom_model_canvas_text(language, "Select", "\uc120\ud0dd"), mode = TRUE),
        custom_model_canvas_button("connect", custom_model_canvas_text(language, "Connect", "\uc5f0\uacb0"), mode = TRUE),
        custom_model_canvas_button("delete", custom_model_canvas_text(language, "Delete", "\uc0ad\uc81c"), mode = TRUE, extra_class = "custom-model-delete-button"),
        custom_model_canvas_button("properties", custom_model_canvas_text(language, "Properties", "\uc18d\uc131"), mode = TRUE),
        custom_model_canvas_button("undo", custom_model_canvas_text(language, "Undo", "\uc2e4\ud589\ucde8\uc18c")),
        custom_model_canvas_button("redo", custom_model_canvas_text(language, "Redo", "\ub2e4\uc2dc\uc2e4\ud589")),
        custom_model_canvas_edge_shape_tools(language),
        custom_model_canvas_edge_anchor_tools(language)
      ),
      group_panel(
        "view",
        custom_model_canvas_button("zoomIn", custom_model_canvas_text(language, "Zoom in", "\ud655\ub300")),
        custom_model_canvas_button("zoomOut", custom_model_canvas_text(language, "Zoom out", "\ucd95\uc18c")),
        custom_model_canvas_button(
          "fit",
          custom_model_canvas_text(language, "Center model", "\ubaa8\ud615 \uc911\uc559 \ub9de\ucda4"),
          title = if (ko) "\ubaa8\ud615 \uc804\uccb4\ub97c \uc6a9\uc9c0 \uc911\uc559\uc5d0 \ubc30\uce58\ud558\uace0 \ud654\uba74\uc5d0 \ub9de\ucda4" else "Center the entire model on the paper and fit the view"
        ),
        custom_model_canvas_button("paper", custom_model_canvas_text(language, "Paper", "\uc6a9\uc9c0\uc124\uc815")),
        custom_model_canvas_button("grid", custom_model_canvas_text(language, "Grid", "\uaca9\uc790"), mode = TRUE),
        custom_model_canvas_button("autoAlign", custom_model_canvas_text(language, "Auto align", "\uc790\ub3d9 \ub9de\ucda4"), mode = TRUE),
        custom_model_canvas_button("style", custom_model_canvas_text(language, "Style", "\uc2a4\ud0c0\uc77c")),
        custom_model_canvas_button("reset", custom_model_canvas_text(language, "Reset", "\ucd08\uae30\ud654"), extra_class = "custom-model-reset-button"),
        div(
          class = "custom-model-reset-confirm-popover",
          div(class = "custom-model-reset-confirm-title", custom_model_canvas_text(language, "Reset model", "\ubaa8\ud615 \ucd08\uae30\ud654")),
          div(class = "custom-model-reset-confirm-message", custom_model_canvas_text(language, "Clear all boxes and arrows?", "\ubaa8\ub4e0 \ubc15\uc2a4\uc640 \ud654\uc0b4\ud45c\ub97c \ucd08\uae30\ud654\ud560\uae4c\uc694?")),
          div(
            class = "custom-model-reset-confirm-actions",
            tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "resetCancel", custom_model_canvas_text(language, "Cancel", "\ucde8\uc18c")),
            tags$button(type = "button", class = "btn btn-warning btn-sm", `data-action` = "resetConfirm", custom_model_canvas_text(language, "Reset", "\ucd08\uae30\ud654"))
          )
        )
      ),
      group_panel(
        "result",
        custom_model_canvas_button("resultEdit", custom_model_canvas_text(language, "Edit", "\ud3b8\uc9d1"), mode = TRUE),
        custom_model_canvas_button("dashNonsignificant", custom_model_canvas_text(language, "Non-significant dashed", "\uc720\uc758\ud558\uc9c0 \uc54a\uc740 \uacbd\ub85c \uc810\uc120"), mode = TRUE),
        custom_model_canvas_button("style", custom_model_canvas_text(language, "Style", "\uc2a4\ud0c0\uc77c")),
        custom_model_canvas_edge_shape_tools(language),
        custom_model_canvas_edge_anchor_tools(language)
      )
    )
  )
}

# SEM-style icon toolbar for the mediation/moderation custom model canvas.
custom_model_canvas_toolbar <- function(input = NULL, language = statedu_initial_language(), analysis_options_ui = NULL) {
  ko <- identical(normalize_app_language(language), "ko")
  canvas_text <- function(en, ko) custom_model_canvas_text(language, en, ko)
  if (is.null(analysis_options_ui)) {
    analysis_options_ui <- custom_model_canvas_analysis_options(input, language)
  }

  div(
    class = "custom-model-toolbar",
    div(
      class = "custom-model-toolbar-panel is-active mediation-moderation-toolbar-panel",
      `data-toolbar-panel` = "tools",
      div(
        class = "mediation-moderation-primary-tools",
        analysis_canvas_command_button(language),
        custom_model_canvas_button("load", canvas_text("Load model", "\ubaa8\ud615 \ubd88\ub7ec\uc624\uae30"), icon = structural_file_icon("open")),
        custom_model_canvas_button("save", canvas_text("Save model", "\ubaa8\ud615 \uc800\uc7a5"), icon = structural_file_icon("save")),
        custom_model_canvas_button("resultLoad", canvas_text("Load result", "\ubd84\uc11d \uacb0\uacfc \ubd88\ub7ec\uc624\uae30"), icon = structural_file_icon("resultOpen")),
        custom_model_canvas_button("resultSave", canvas_text("Save result", "\ubd84\uc11d \uacb0\uacfc \uc800\uc7a5"), icon = structural_file_icon("resultSave")),
        custom_model_canvas_button("export", canvas_text("Export PNG", "PNG \uadf8\ub9bc \ub0b4\ubcf4\ub0b4\uae30")),
        custom_model_canvas_button("select", canvas_text("Select", "\uc120\ud0dd"), mode = TRUE),
        custom_model_canvas_button("addObserved", canvas_text("Measured variable", "측정변수"), mode = TRUE,
          icon = tags$svg(viewBox = "0 0 24 24", width = "20", height = "20", `aria-hidden` = "true",
            tags$rect(x = "3", y = "5", width = "18", height = "14", rx = "1", fill = "none", stroke = "currentColor", `stroke-width` = "1.8"))),
        custom_model_canvas_button("covariates", canvas_text("Covariates", "공변량"), icon = tags$span("C")),
        custom_model_canvas_button("connect", canvas_text("Connect", "\uc5f0\uacb0"), mode = TRUE),
        custom_model_canvas_button("properties", canvas_text("Properties", "\uc18d\uc131"), mode = TRUE),
        custom_model_canvas_button("delete", canvas_text("Delete", "\uc0ad\uc81c"), mode = TRUE, extra_class = "custom-model-delete-button"),
        custom_model_canvas_button("autoAlign", canvas_text("Auto align", "\uc790\ub3d9 \ub9de\ucda4"), mode = TRUE),
        custom_model_canvas_button("run", canvas_text("Run", "\uc2e4\ud589")),
        div(
          class = "custom-model-run-options-popover",
          div(class = "custom-model-run-options-title", canvas_text("Analysis options", "\ubd84\uc11d \uc635\uc158")),
          analysis_options_ui,
          div(
            class = "custom-model-run-options-actions",
            tags$button(type = "button", class = "btn btn-default btn-sm custom-model-run-options-cancel", `data-action` = "runCancel", canvas_text("Cancel", "\ucde8\uc18c")),
            tags$button(type = "button", class = "btn btn-primary btn-sm custom-model-run-options-confirm", `data-action` = "runConfirm", canvas_text("Run", "\uc2e4\ud589"))
          )
        ),
        custom_model_canvas_button("undo", canvas_text("Undo", "\uc2e4\ud589\ucde8\uc18c")),
        custom_model_canvas_button("redo", canvas_text("Redo", "\ub2e4\uc2dc\uc2e4\ud589")),
        custom_model_canvas_button("grid", canvas_text("Grid", "\uaca9\uc790"), mode = TRUE),
        custom_model_canvas_button("zoomIn", canvas_text("Zoom in", "\ud655\ub300")),
        custom_model_canvas_button("zoomOut", canvas_text("Zoom out", "\ucd95\uc18c")),
        custom_model_canvas_button(
          "fit",
          canvas_text("Center model", "\ubaa8\ud615 \uc911\uc559 \ub9de\ucda4"),
          title = if (ko) "\ubaa8\ud615 \uc804\uccb4\ub97c \uc6a9\uc9c0 \uc911\uc559\uc5d0 \ubc30\uce58\ud558\uace0 \ud654\uba74\uc5d0 \ub9de\ucda4" else "Center the entire model on the paper and fit the view"
        ),
        custom_model_canvas_button("reset", canvas_text("Reset", "\ucd08\uae30\ud654"), extra_class = "custom-model-reset-button"),
        div(
          class = "custom-model-reset-confirm-popover",
          div(class = "custom-model-reset-confirm-title", canvas_text("Reset model", "\ubaa8\ud615 \ucd08\uae30\ud654")),
          div(class = "custom-model-reset-confirm-message", canvas_text("Clear all boxes and arrows?", "\ubaa8\ub4e0 \ubc15\uc2a4\uc640 \ud654\uc0b4\ud45c\ub97c \ucd08\uae30\ud654\ud560\uae4c\uc694?")),
          div(
            class = "custom-model-reset-confirm-actions",
            tags$button(type = "button", class = "btn btn-default btn-sm", `data-action` = "resetCancel", canvas_text("Cancel", "\ucde8\uc18c")),
            tags$button(type = "button", class = "btn btn-warning btn-sm", `data-action` = "resetConfirm", canvas_text("Reset", "\ucd08\uae30\ud654"))
          )
        )
      ),
      div(
        class = "mediation-moderation-secondary-tools",
        custom_model_canvas_edge_shape_tools(language),
        custom_model_canvas_edge_anchor_tools(language),
        div(
          class = "structural-result-icon-grid mediation-moderation-result-icons",
          span(class = "structural-result-icon-placeholder", `aria-hidden` = "true"),
          custom_model_canvas_button("resultEdit", canvas_text("Edit", "\ud3b8\uc9d1"), mode = TRUE),
          custom_model_canvas_button("dashNonsignificant", canvas_text("Non-significant dashed", "\uc720\uc758\ud558\uc9c0 \uc54a\uc740 \uacbd\ub85c \uc810\uc120"), mode = TRUE),
          custom_model_canvas_button("style", canvas_text("Style", "\uc2a4\ud0c0\uc77c"))
        )
      )
    )
  )
}
