# Data tab status text, view title, and step panel UI.

data_loaded_message_text <- function(
  has_file,
  file_name = "",
  file_restored = FALSE,
  data_n_variables = 0,
  data_n_rows = 0,
  restored_file_name = "",
  restored_n_variables = 0,
  has_restored_info = FALSE
) {
  if (!isTRUE(has_file)) {
    if (isTRUE(has_restored_info)) {
      return(sprintf(
        "Settings loaded for %s: %s variables saved. Reopen the data file before running analysis.",
        restored_file_name,
        restored_n_variables
      ))
    }
    return("No data file is open.")
  }

  if (isTRUE(file_restored)) {
    return(sprintf("Loaded %s from settings: %s variables, %s rows.", file_name, data_n_variables, data_n_rows))
  }
  sprintf("Loaded %s: %s variables, %s rows.", file_name, data_n_variables, data_n_rows)
}

data_loaded_message_state <- function(file = NULL, data = NULL, restored_info = NULL, restored_file_name = "") {
  has_file <- !is.null(file)
  list(
    has_file = has_file,
    file_name = if (has_file) file$name else "",
    file_restored = if (has_file) isTRUE(file$restored) else FALSE,
    data_n_variables = if (has_file && !is.null(data)) ncol(data) else 0,
    data_n_rows = if (has_file && !is.null(data)) nrow(data) else 0,
    restored_file_name = restored_file_name,
    restored_n_variables = if (!is.null(restored_info)) nrow(restored_info) else 0,
    has_restored_info = !is.null(restored_info)
  )
}

data_view_title_text <- function(
  has_file,
  has_restored_info,
  view,
  selection_applied,
  active_role,
  active_role_count,
  selected_count,
  available_count
) {
  if (!isTRUE(has_file) && !isTRUE(has_restored_info)) {
    return("Variable Info")
  }
  if (identical(view, "preview")) {
    if (isTRUE(selection_applied)) {
      return(sprintf("Selected Data Preview (%s variables)", selected_count))
    }
    return("Data Preview")
  }
  if (identical(view, "labels")) {
    return("Categorical Value Labels")
  }
  if (isTRUE(selection_applied)) {
    return(sprintf("Selected variables (%s)", selected_count))
  }
  sprintf("All variables (%s)", available_count)
}

data_view_title_state <- function(
  file = NULL,
  restored_info = NULL,
  view = "info",
  selection_applied = FALSE,
  active_role = "dependent",
  active_role_names = character(0),
  selected = character(0),
  available = character(0)
) {
  list(
    has_file = !is.null(file),
    has_restored_info = !is.null(restored_info),
    view = view,
    selection_applied = isTRUE(selection_applied),
    active_role = active_role,
    active_role_count = length(active_role_names),
    selected_count = length(selected),
    available_count = length(available)
  )
}

data_view_toggle_control <- function(view, step = "step1") {
  if (identical(view, "labels")) {
    return(actionButton("show_data_preview", "Data Preview", class = "view-toggle-button"))
  }
  if (identical(view, "preview")) {
    label <- if (identical(step, "step3")) "Variable Labels" else "Variable Info"
    return(actionButton("show_variable_info", label, class = "view-toggle-button"))
  }
  actionButton("show_data_preview", "Data Preview", class = "view-toggle-button")
}

apply_category_labels_inline_js <- function() {
  paste(
    "if (window.easyflowApplyCategoryLabels) return window.easyflowApplyCategoryLabels();",
    "if (window.Shiny) {",
    "  var root = document.getElementById('category_label_table') || document;",
    "  var categoryLabels = {};",
    "  var measurements = {};",
    "  var varLabels = {};",
    "  var measurementPairs = [];",
    "  var varLabelPairs = [];",
    "  root.querySelectorAll('input[data-name][data-field], select[data-name][data-field]').forEach(function(el) {",
    "    var name = el.getAttribute('data-name') || '';",
    "    var field = el.getAttribute('data-field') || '';",
    "    if (!name || !field) return;",
    "    var value = el.value || '';",
    "    if (field === 'measurement') {",
    "      measurements[name] = value;",
    "      measurementPairs.push({name: name, value: value});",
    "      return;",
    "    }",
    "    categoryLabels[name] = categoryLabels[name] || {};",
    "    categoryLabels[name][field] = value;",
    "    if (field === 'var_label') {",
    "      varLabels[name] = value;",
    "      varLabelPairs.push({name: name, value: value});",
    "    }",
    "  });",
    "  Shiny.setInputValue('apply_category_labels_request', {",
    "    category_labels: categoryLabels,",
    "    measurements: measurements,",
    "    measurement_pairs: measurementPairs,",
    "    var_labels: varLabels,",
    "    var_label_pairs: varLabelPairs,",
    "    nonce: Date.now() + Math.random()",
    "  }, {priority: 'event'});",
    "}",
    "return false;",
    sep = ""
  )
}

data_steps_state <- function(
  file = NULL,
  open_data = NULL,
  restored_info = NULL,
  step = "step1",
  applied = FALSE,
  role_applied = FALSE,
  role = "dependent",
  restored_data_file_name = "",
  selected = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  has_calculated_variables = FALSE
) {
  has_open_data <- !is.null(file)
  list(
    step = step,
    has_open_data = has_open_data,
    has_data = has_open_data || !is.null(restored_info),
    applied = isTRUE(applied),
    role_applied = isTRUE(role_applied),
    role = role,
    data_file_name = if (has_open_data) file$name else "",
    data_n_variables = if (has_open_data && !is.null(open_data)) ncol(open_data) else 0,
    data_n_rows = if (has_open_data && !is.null(open_data)) nrow(open_data) else 0,
    restored_data_file_name = restored_data_file_name,
    restored_n_variables = if (!is.null(restored_info)) nrow(restored_info) else 0,
    selected_count = length(selected),
    dependent_count = length(dependent),
    independent_count = length(independent),
    control_count = length(controls),
    has_calculated_variables = isTRUE(has_calculated_variables)
  )
}

data_steps_panel <- function(
  step,
  has_open_data,
  has_data,
  applied,
  role_applied,
  role,
  data_file_name,
  data_n_variables,
  data_n_rows,
  restored_data_file_name,
  restored_n_variables,
  selected_count,
  dependent_count,
  independent_count,
  control_count,
  has_calculated_variables = FALSE
) {
  step_class <- function(name, enabled = TRUE) {
    paste("step-block", if (identical(step, name)) "is-open" else "is-closed", if (!enabled) "is-disabled" else "")
  }

  tagList(
    div(
      class = step_class("step1"),
      h3(actionLink("go_step1", "Step 1. Load data file", class = "step-link")),
      if (has_data && !identical(step, "step1")) {
        div(
          class = "step-summary",
          div(if (has_open_data) data_file_name else restored_data_file_name, class = "step-summary-title"),
          div(
            if (has_open_data) {
              sprintf("%s variables, %s rows", data_n_variables, data_n_rows)
            } else {
              sprintf("%s variables saved in settings. Reopen the data file before running analysis.", restored_n_variables)
            },
            class = "step-summary-detail"
          ),
          if (!has_open_data) {
            actionButton("browse_data_file", "Reconnect data file")
          }
        )
      } else {
        tagList(
          actionButton("browse_data_file", "Open data file"),
          checkboxInput("header", "CSV first row contains variable names", TRUE),
          selectInput(
            "dat_delimiter",
            "DAT delimiter",
            choices = c("Whitespace" = "whitespace", "Comma" = "comma", "Tab" = "tab"),
            selected = "whitespace"
          ),
          checkboxInput("dat_has_names", "DAT first row contains variable names", FALSE)
        )
      }
    ),
    if (has_data) {
      div(
        class = step_class("step2", has_data),
        h3(actionLink("go_step2", "Step 2. Select analysis variables", class = "step-link")),
        if (!identical(step, "step2") && applied) {
          div(
            class = "step-summary",
            div(sprintf("%s variables selected", selected_count), class = "step-summary-title"),
            actionButton("modify_variable_selection", "Modify selection")
          )
        } else if (identical(step, "step2")) {
          tagList(
            div("Check variables to keep, then apply the selection.", class = "step-note"),
            div(
              class = "bulk-measurement-control",
              selectInput(
                "bulk_measurement_type",
                "Set selected variable type to",
                choices = c(
                  "binary" = "binary",
                  "category" = "category",
                  "ordinal" = "ordered",
                  "continuous" = "continuous"
                ),
                selected = "continuous"
              ),
              actionButton(
                "apply_bulk_measurement_type",
                "Apply type",
                class = "btn btn-default",
                onclick = "if(window.easyflowApplyBulkMeasurement){window.easyflowApplyBulkMeasurement(); return false;}"
              )
            ),
            actionButton(
              "apply_variable_selection",
              "Apply variable selection",
              class = "btn btn-primary",
              onmousedown = "if(window.easyflowFlushVariableTableState){window.easyflowFlushVariableTableState();}"
            )
          )
        } else {
          div("Select variables first.", class = "step-note")
        }
      )
    },
    if (has_data && applied) {
      div(
        class = step_class("step3", applied),
        h3(actionLink("go_step3", "Step 3. Variable labels", class = "step-link")),
        if (identical(step, "step3")) {
          tagList(
            div("Edit value labels for categorical variables. Click Apply labels to update the active session.", class = "step-note"),
            div(
              class = "step3-action-row",
              actionButton(
                "apply_category_labels_button",
                "Apply labels",
                class = "btn btn-primary",
                onmousedown = "if(window.easyflowApplyCategoryLabels){window.easyflowApplyCategoryLabels();}"
              ),
              if (isTRUE(has_calculated_variables)) {
                actionButton("save_current_data_file", "Save data", class = "btn btn-default")
              }
            )
          )
        } else {
          div(
            class = "step-summary",
            div("Categorical value labels", class = "step-summary-title"),
            div("Click Step 3 to review or edit labels.", class = "step-summary-detail")
          )
        }
      )
    },
    div(
      class = "step-block session-settings-block",
      h3("Session settings"),
      div(
        class = "session-settings-actions",
        actionButton("browse_settings_data", "Load settings", class = "session-settings-button"),
        actionButton("save_settings_data", "Save settings", class = "settings-save-button session-settings-button"),
        actionButton("reset_settings_data", "Reset settings", class = "reset-settings-button session-settings-button")
      )
    )
  )
}
