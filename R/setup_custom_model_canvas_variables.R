# Custom model canvas variable list UI helpers.

custom_model_canvas_display_label <- function(name, variable_table = NULL, labels = character(0)) {
  data_label <- display_variable_name_static(name, variable_table, labels, label_only = TRUE)
  if (!nzchar(data_label)) name else data_label
}

custom_model_canvas_variable_items <- function(selected_names, variable_table = NULL, labels = character(0)) {
  selected_names <- as.character(selected_names %||% character(0))
  selected_names <- selected_names[nzchar(selected_names)]
  if (length(selected_names) == 0) {
    return(list())
  }

  measurements <- character(0)
  if (!is.null(variable_table) && all(c("name", "measurement") %in% names(variable_table))) {
    measurements <- stats::setNames(as.character(variable_table$measurement), as.character(variable_table$name))
  }

  lapply(selected_names, function(name) {
    list(
      name = name,
      dataLabel = custom_model_canvas_display_label(name, variable_table, labels),
      measurement = named_value(measurements, name, "")
    )
  })
}

custom_model_canvas_variable_panel <- function(items, language = statedu_initial_language()) {
  if (length(items) == 0) {
    return(div(
      class = "custom-model-empty-variable-list",
      custom_model_canvas_text(
        language,
        "Complete Step 2 in the Data tab before drawing a model.",
        "\ub370\uc774\ud130 \ud0ed\uc758 2\ub2e8\uacc4\ub97c \uba3c\uc800 \uc801\uc6a9\ud55c \ub2e4\uc74c \ubaa8\ub378\uc744 \uadf8\ub9b4 \uc218 \uc788\uc2b5\ub2c8\ub2e4."
      )
    ))
  }

  div(
    class = "custom-model-variable-panel-body",
    div(
      class = "custom-model-variable-list analysis-transfer-listbox",
      role = "listbox",
      tabindex = "0",
      lapply(items, function(item) {
        div(
          class = "custom-model-variable-item analysis-transfer-option",
          role = "option",
          draggable = "true",
          `data-variable-name` = item$name,
          `data-value` = item$name,
          `data-data-label` = item$dataLabel,
          `data-measurement` = item$measurement,
          measurement_symbol_tag(item$measurement),
          span(class = "analysis-transfer-option-label custom-model-variable-name", item$dataLabel)
        )
      })
    ),
    div(
      class = "custom-model-role-actions",
      div(class = "custom-model-role-title", custom_model_canvas_text(language, "Role", "\uc5ed\ud560")),
      tags$button(type = "button", class = "custom-model-role-button", `data-role` = "dependent", custom_model_canvas_text(language, "Dependent", "\uc885\uc18d")),
      tags$button(type = "button", class = "custom-model-role-button", `data-role` = "independent", custom_model_canvas_text(language, "Independent", "\ub3c5\ub9bd")),
      tags$button(type = "button", class = "custom-model-role-button", `data-role` = "mediator", custom_model_canvas_text(language, "Mediator", "\ub9e4\uac1c")),
      tags$button(type = "button", class = "custom-model-role-button", `data-role` = "moderator", custom_model_canvas_text(language, "Moderator", "\uc870\uc808")),
      tags$button(type = "button", class = "custom-model-role-button custom-model-role-button-covariate", `data-role` = "covariate", custom_model_canvas_text(language, "Covariate", "\uacf5\ubcc0\ub7c9"))
    )
  )
}
