# Custom mediation/moderation model canvas workspace UI.

custom_model_canvas_workspace <- function(selected_names, variable_table = NULL, labels = character(0), input = NULL, language = statedu_initial_language()) {
  items <- custom_model_canvas_variable_items(selected_names, variable_table, labels)
  variables_json <- htmltools::htmlEscape(
    jsonlite::toJSON(items, auto_unbox = TRUE, null = "null"),
    attribute = TRUE
  )
  i18n_json <- htmltools::htmlEscape(
    jsonlite::toJSON(custom_model_canvas_i18n(language), auto_unbox = TRUE, null = "null"),
    attribute = TRUE
  )

  tagList(
    div(
      id = "custom-model-canvas-root",
      class = "custom-model-canvas-root",
      `data-variables` = variables_json,
      `data-language` = normalize_app_language(language),
      `data-i18n` = i18n_json,
      div(
        class = "custom-model-variable-panel analysis-transfer-column analysis-transfer-panel",
        analysis_field_label_tag("Variables", language = language),
        custom_model_canvas_variable_panel(items, language)
      ),
      div(
        class = "custom-model-diagram-panel",
        custom_model_canvas_toolbar(input, language),
        div(
          class = "custom-model-statusbar",
          span(class = "custom-model-mode-status", custom_model_canvas_text(language, "Mode: Select", "\ubaa8\ub4dc: \uc120\ud0dd")),
          span(class = "custom-model-paper-status", "B5 landscape"),
          span(class = "custom-model-covariate-status", custom_model_canvas_text(language, "Covariates: none", "\uacf5\ubcc0\ub7c9: \uc5c6\uc74c"))
        ),
        div(
          class = "custom-model-canvas-scroll",
          div(
            class = "custom-model-paper-frame",
            div(
              class = "custom-model-paper is-grid-visible",
              `data-width` = "971",
              `data-height` = "688",
              tags$svg(class = "custom-model-edge-layer", width = "971", height = "688"),
              div(class = "custom-model-node-layer")
            )
          )
        )
      )
    ),
    tags$script(HTML("window.StatEduModelCanvas && window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.initAll();"))
  )
}

custom_model_canvas_tab_panel <- function(title = custom_model_canvas_title(), language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "analysis_custom_model_canvas",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(custom_model_canvas_title(language)),
        div(
          custom_model_canvas_text(
            language,
            "Draw a mediation/moderation model by placing variables on the canvas.",
            "\ubcc0\uc218\ub97c \uce94\ubc84\uc2a4\uc5d0 \ubc30\uce58\ud558\uc5ec \ub9e4\uac1c\u00b7\uc870\uc808 \uc0ac\uc6a9\uc790 \ubaa8\ub378\uc744 \uc791\uc131\ud569\ub2c8\ub2e4."
          ),
          class = "app-subtitle"
        )
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel custom-model-workspace-panel",
        style = "min-width:1450px;overflow-x:auto;",
        analysis_workspace_heading(custom_model_canvas_title(language), "custom_model_canvas", language),
        analysis_workspace_body(
          "custom_model_canvas",
          uiOutput("custom_model_canvas_setup"),
          uiOutput("custom_model_canvas_save_control"),
          uiOutput("custom_model_canvas_results")
        )
      )
    )
  )
}
