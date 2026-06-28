# Shared analysis data viewer helpers.

analysis_data_viewer_button <- function(id, language = statedu_initial_language()) {
  actionButton(
    id,
    statedu_text(language, "View selected data", statedu_utf8("ec84a0ed839d20eb8db0ec9db4ed84b020ebb3b4eab8b0")),
    class = "btn btn-default analysis-data-viewer-button"
  )
}

analysis_data_viewer_title <- function(title, language = statedu_initial_language()) {
  title <- as.character(title %||% "")
  base_title <- sub(" Data Viewer$", "", title)
  if (identical(base_title, title)) {
    return(analysis_ui_text(title, language))
  }
  paste(analysis_ui_text(base_title, language), analysis_ui_text("Data Viewer", language))
}

analysis_workspace_heading <- function(title, prefix, language = statedu_initial_language()) {
  div(
    class = paste("analysis-workspace-heading", paste0(prefix, "-workspace-heading")),
    h3(analysis_ui_text(title, language)),
    conditionalPanel(
      condition = sprintf("output.%s_view_mode !== 'viewer'", prefix),
      analysis_data_viewer_button(paste0(prefix, "_view_data"), language)
    )
  )
}

analysis_workspace_body <- function(prefix, ..., viewer_output_id = NULL) {
  viewer_output_id <- viewer_output_id %||% paste0(prefix, "_data_viewer")
  tagList(
    conditionalPanel(
      condition = sprintf("output.%s_view_mode !== 'viewer'", prefix),
      ...
    ),
    conditionalPanel(
      condition = sprintf("output.%s_view_mode === 'viewer'", prefix),
      uiOutput(viewer_output_id)
    )
  )
}

register_analysis_data_viewer_handlers <- function(
  input,
  output,
  prefix,
  title,
  dataset_fn,
  selected_names_fn,
  variables_fn,
  extra_variables_fn = NULL,
  variable_table_fn,
  labels_fn,
  category_table_fn,
  language_fn = NULL
) {
  visible <- reactiveVal(FALSE)
  label_mode <- reactiveVal(FALSE)
  view_input_id <- paste0(prefix, "_view_data")
  back_input_id <- paste0(prefix, "_back_to_analysis")
  scope_input_id <- paste0(prefix, "_viewer_all_step2")
  label_input_id <- paste0(prefix, "_value_label_toggle")
  mode_output_id <- paste0(prefix, "_view_mode")
  panel_output_id <- paste0(prefix, "_data_viewer")
  table_output_id <- paste0(prefix, "_data_viewer_table")
  viewer_language <- reactive({
    if (is.function(language_fn)) {
      return(normalize_app_language(language_fn()))
    }
    statedu_initial_language()
  })

  viewer_variables <- reactive({
    data <- dataset_fn()
    data_names <- names(data %||% data.frame())
    selected_variables <- intersect(as.character(selected_names_fn() %||% character(0)), data_names)
    if (isTRUE(input[[scope_input_id]])) {
      return(selected_variables)
    }
    variables <- as.character(variables_fn() %||% character(0))
    extra_variables <- if (is.function(extra_variables_fn)) {
      as.character(extra_variables_fn() %||% character(0))
    } else {
      character(0)
    }
    analysis_variables <- intersect(unique(c(variables, extra_variables)), data_names)
    if (length(analysis_variables) == 0) {
      return(selected_variables)
    }
    analysis_variables
  })

  output[[mode_output_id]] <- reactive({
    if (isTRUE(visible())) "viewer" else "analysis"
  })
  outputOptions(output, mode_output_id, suspendWhenHidden = FALSE)

  observeEvent(input[[view_input_id]], {
    visible(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input[[back_input_id]], {
    visible(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input[[label_input_id]], {
    label_mode(!isTRUE(label_mode()))
  }, ignoreInit = TRUE)

  output[[panel_output_id]] <- renderUI({
    analysis_data_viewer_panel(
      title = title,
      variables = viewer_variables(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      back_button_id = back_input_id,
      scope_input_id = scope_input_id,
      value_label_button_id = label_input_id,
      table_output_id = table_output_id,
      all_selected = isTRUE(input[[scope_input_id]]),
      label_mode = label_mode(),
      language = viewer_language()
    )
  })

  output[[table_output_id]] <- DT::renderDT({
    analysis_data_viewer_table(
      dataset_fn(),
      viewer_variables(),
      category_table = category_table_fn(),
      use_labels = label_mode(),
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      language = viewer_language()
    )
  })

  invisible(TRUE)
}

analysis_data_viewer_variable_label <- function(name, variable_table = NULL, labels = character(0)) {
  name <- as.character(name %||% "")
  label <- named_value(labels, name, "")
  if (!nzchar(label) && is.data.frame(variable_table) && all(c("name", "var_label") %in% names(variable_table))) {
    row_index <- match(name, as.character(variable_table$name))
    if (!is.na(row_index)) {
      label <- as.character(variable_table$var_label[[row_index]] %||% "")
    }
  }
  trimws(label)
}

analysis_data_viewer_variable_list <- function(variables, variable_table = NULL, labels = character(0), language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  variables <- as.character(variables %||% character(0))
  variables <- variables[nzchar(variables)]
  if (length(variables) == 0) {
    return(div(analysis_ui_text("No analysis variables are selected.", language), class = "analysis-data-viewer-empty"))
  }
  items <- analysis_variable_items(variables, variable_table, labels)
  div(
    class = "analysis-data-viewer-variable-list",
    lapply(items, function(item) {
      variable_label <- analysis_data_viewer_variable_label(item$value, variable_table, labels)
      div(
        class = "analysis-data-viewer-variable",
        span(item$value, class = "analysis-data-viewer-variable-name"),
        measurement_symbol_tag(item$measurement),
        if (nzchar(variable_label) && !identical(variable_label, item$value)) {
          span(sprintf("(%s)", variable_label), class = "analysis-data-viewer-variable-label")
        }
      )
    })
  )
}

analysis_data_viewer_panel <- function(
  title,
  variables,
  variable_table = NULL,
  labels = character(0),
  back_button_id,
  scope_input_id,
  value_label_button_id,
  table_output_id,
  all_selected = FALSE,
  label_mode = FALSE,
  language = statedu_initial_language()
) {
  language <- normalize_app_language(language)
  div(
    class = "analysis-data-viewer-panel",
    div(
      class = "analysis-data-viewer-header",
      div(
        h3(analysis_data_viewer_title(title, language)),
        div(analysis_ui_text("Read-only worksheet preview of the current data.", language), class = "analysis-data-viewer-subtitle")
      ),
      div(
        class = "analysis-data-viewer-controls",
        div(
          class = "analysis-data-viewer-button-stack",
          actionButton(back_button_id, analysis_ui_text("Back to analysis", language), class = "btn btn-primary analysis-data-viewer-action-button"),
          actionButton(
            value_label_button_id,
            analysis_ui_text("Value / Label", language),
            class = paste("btn btn-default btn-sm analysis-data-value-label-button analysis-data-viewer-action-button", if (isTRUE(label_mode)) "is-active" else "")
          )
        )
      )
    ),
    div(
      class = "analysis-data-viewer-table-wrap",
      DT::DTOutput(table_output_id)
    )
  )
}

analysis_data_viewer_labeled_data <- function(data, variables, category_table = NULL, use_labels = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  preview <- utils::head(data[, variables, drop = FALSE], 20)
  if (!isTRUE(use_labels)) {
    return(preview)
  }

  value_labels <- category_value_label_lookup_static(category_table)
  for (variable in variables) {
    labels <- value_labels[[variable]]
    if (length(labels) == 0) {
      next
    }
    values <- as.character(preview[[variable]])
    mapped <- unname(labels[values])
    preview[[variable]] <- ifelse(!is.na(mapped) & nzchar(mapped), mapped, values)
  }
  preview
}

analysis_data_viewer_column_headers <- function(variables, variable_table = NULL, labels = character(0), use_labels = FALSE) {
  variables <- as.character(variables %||% character(0))
  measurements <- character(0)
  if (is.data.frame(variable_table) && all(c("name", "measurement") %in% names(variable_table))) {
    measurements <- stats::setNames(as.character(variable_table$measurement), as.character(variable_table$name))
  }

  stats::setNames(
    vapply(variables, function(variable) {
      heading <- if (isTRUE(use_labels)) {
        label <- analysis_data_viewer_variable_label(variable, variable_table, labels)
        if (nzchar(label)) label else variable
      } else {
        variable
      }
      icon <- htmltools::renderTags(measurement_symbol_tag(named_value(measurements, variable, "")))$html
      sprintf(
        "<span class=\"analysis-data-viewer-column-heading\"><span>%s</span>%s</span>",
        htmltools::htmlEscape(heading),
        icon
      )
    }, character(1)),
    variables
  )
}

analysis_data_viewer_escape_cells <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  for (name in names(data)) {
    values <- as.character(data[[name]])
    data[[name]] <- ifelse(is.na(values), "", htmltools::htmlEscape(values))
  }
  data
}

analysis_data_viewer_table <- function(data, variables, category_table = NULL, use_labels = FALSE, variable_table = NULL, labels = character(0), language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  if (!is.data.frame(data) || nrow(data) == 0) {
    message_label <- analysis_ui_text("Message", language)
    empty_message <- analysis_ui_text("No data is loaded.", language)
    empty_data <- stats::setNames(data.frame(empty_message, stringsAsFactors = FALSE), message_label)
    return(DT::datatable(empty_data, rownames = FALSE, options = list(dom = "t")))
  }
  variables <- names(data)
  preview <- analysis_data_viewer_labeled_data(data, variables, category_table, use_labels)
  headers <- unname(analysis_data_viewer_column_headers(names(preview), variable_table, labels, use_labels))
  header_json <- jsonlite::toJSON(headers, auto_unbox = TRUE)
  DT::datatable(
    preview,
    colnames = names(preview),
    rownames = FALSE,
    callback = DT::JS(sprintf(
      "var easyflowHeaders = %s; table.columns().every(function(index) { $(this.header()).html(easyflowHeaders[index]); });",
      header_json
    )),
    options = list(
      dom = "t",
      paging = FALSE,
      autoWidth = FALSE
    )
  )
}
