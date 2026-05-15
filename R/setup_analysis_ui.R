# Shared setup UI helpers for analysis modules.

analysis_variable_items <- function(names, table = NULL, labels = character(0)) {
  variable_choice_items(names, table, labels)
}

analysis_allowed_measurements_all <- function() {
  c("binary", "category", "ordered", "continuous")
}

analysis_allowed_variables <- function(names, variable_table = NULL, allowed_measurements = character(0)) {
  names <- as.character(names %||% character(0))
  allowed_measurements <- tolower(as.character(allowed_measurements %||% character(0)))
  allowed_measurements[allowed_measurements == "ordinal"] <- "ordered"
  allowed_measurements[allowed_measurements == "nominal"] <- "category"
  if (length(names) == 0 || length(allowed_measurements) == 0) {
    return(names)
  }
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) {
    return(names)
  }
  measurements <- stats::setNames(tolower(as.character(variable_table$measurement)), as.character(variable_table$name))
  measurements[measurements == "ordinal"] <- "ordered"
  names[names %in% names(measurements) & measurements[names] %in% allowed_measurements]
}

analysis_field_label_tag <- function(label, allowed_measurements = character(0)) {
  allowed_measurements <- as.character(allowed_measurements %||% character(0))
  div(
    class = "analysis-field-label analysis-field-label-with-icons",
    span(label),
    if (length(allowed_measurements) > 0) {
      span(
        class = "analysis-allowed-measurements",
        lapply(allowed_measurements, measurement_symbol_tag)
      )
    }
  )
}

analysis_transfer_listbox_input <- function(input_id, items, selected = character(0), size = 14) {
  values <- vapply(items, `[[`, character(1), "value")
  labels <- vapply(items, `[[`, character(1), "label")
  selected <- intersect(as.character(selected %||% character(0)), values)
  height_px <- max(4, as.integer(size %||% 14)) * 24
  listbox_style <- paste0(
    "height:", height_px, "px;",
    "width:300px;min-width:300px;max-width:300px;",
    "overflow-y:auto;background:#fff;",
    "border:1px solid #b8c8d6;border-radius:6px;",
    "box-sizing:border-box;padding:4px 0;"
  )

  tagList(
    tags$select(
      id = input_id,
      class = "easyflow-hidden-select analysis-transfer-hidden-select",
      multiple = "multiple",
      style = "display:none;",
      lapply(seq_along(values), function(index) {
        tags$option(
          value = values[[index]],
          selected = if (values[[index]] %in% selected) "selected" else NULL,
          labels[[index]]
        )
      })
    ),
    div(
      class = "analysis-transfer-listbox",
      role = "listbox",
      tabindex = "0",
      `aria-multiselectable` = "true",
      `data-input-id` = input_id,
      onkeydown = "return window.easyflowTransferListboxKeydown ? window.easyflowTransferListboxKeydown(event, this) : true;",
      style = listbox_style,
      lapply(items, function(item) {
        value <- as.character(item$value)
        div(
          class = paste("analysis-transfer-option", if (value %in% selected) "is-selected" else ""),
          role = "option",
          `aria-selected` = if (value %in% selected) "true" else "false",
          `data-value` = value,
          onmousedown = "event.preventDefault();",
          onclick = "window.easyflowTransferOptionClick && window.easyflowTransferOptionClick(event, this);",
          measurement_symbol_tag(item$measurement),
          span(item$label, class = "analysis-transfer-option-label")
        )
      })
    )
  )
}

analysis_option_group <- function(title, options) {
  div(
    class = "analysis-option-group",
    div(class = "analysis-option-title", title),
    lapply(options, function(option) {
      checkboxInput(option$id, option$label, value = isTRUE(option$value))
    })
  )
}

analysis_radio_group <- function(title, input_id, choices, selected = NULL) {
  values <- unname(choices)
  selected <- as.character(selected %||% values[[1]])
  if (!selected %in% values) {
    selected <- values[[1]]
  }
  div(
    class = "analysis-option-group analysis-radio-group",
    div(class = "analysis-option-title", title),
    radioButtons(input_id, label = NULL, choices = choices, selected = selected)
  )
}
