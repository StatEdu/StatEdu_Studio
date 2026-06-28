# Data tab status text, view title, and step panel UI.

data_loaded_message_text <- function(
  has_file,
  file_name = "",
  file_restored = FALSE,
  data_n_variables = 0,
  data_n_rows = 0,
  restored_file_name = "",
  restored_n_variables = 0,
  has_restored_info = FALSE,
  language = statedu_initial_language()
) {
  if (!isTRUE(has_file)) {
    if (isTRUE(has_restored_info)) {
      return(sprintf(
        statedu_text(language, "Settings loaded for %s: %s variables saved. Reopen the data file before running analysis.", statedu_utf8("257320ec84a4eca095ec9d8420ebb688eb9facec9994ec8ab5eb8b88eb8ba42e20eca080ec9ea5eb909c20ebb380ec8898eb8a94202573eab09cec9e85eb8b88eb8ba42e20ebb684ec849d20eca08420eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420eb8ba4ec8b9c20ec97b4ec96b420eca3bcec84b8ec9a942e")),
        restored_file_name,
        restored_n_variables
      ))
    }
    return(statedu_text(language, "No data file is open.", statedu_utf8("ec97b4eba6b020eb8db0ec9db4ed84b020ed8c8cec9dbcec9db420ec9786ec8ab5eb8b88eb8ba42e")))
  }

  if (isTRUE(file_restored)) {
    return(sprintf(statedu_text(language, "Loaded %s from settings: %s variables, %s rows.", statedu_utf8("ec84a4eca095ec9790ec849c202573ec9d8420ebb688eb9facec9994ec8ab5eb8b88eb8ba43a20ebb380ec8898202573eab09c2c20ed9689202573eab09c2e")), file_name, data_n_variables, data_n_rows))
  }
  sprintf(statedu_text(language, "Loaded %s: %s variables, %s rows.", statedu_utf8("257320ebb688eb9facec98b43a20ebb380ec8898202573eab09c2c20ed9689202573eab09c2e")), file_name, data_n_variables, data_n_rows)
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
  available_count,
  language = statedu_initial_language()
) {
  if (!isTRUE(has_file) && !isTRUE(has_restored_info)) {
    return(statedu_text(language, "Variable Info", statedu_utf8("ec84b8ec859820ec84a4eca095")))
  }
  if (identical(view, "preview")) {
    if (isTRUE(selection_applied)) {
      return(sprintf(statedu_text(language, "Selected Data Preview (%s variables)", statedu_utf8("ec84a0ed839d20eb8db0ec9db4ed84b020ebafb8eba6acebb3b4eab8b028ebb380ec8898202573eab09c29")), selected_count))
    }
    return(statedu_text(language, "Data Preview", statedu_utf8("eb8db0ec9db4ed84b020ebafb8eba6acebb3b4eab8b0")))
  }
  if (identical(view, "labels")) {
    return(statedu_text(language, "Variable Review", statedu_utf8("ec84b8ec859820ec84a4eca095")))
  }
  if (isTRUE(selection_applied)) {
    return(sprintf(statedu_text(language, "Selected variables (%s)", statedu_utf8("ec84a0ed839d20ebb380ec8898282573eab09c29")), selected_count))
  }
  sprintf(statedu_text(language, "All variables (%s)", statedu_utf8("eca084ecb2b420ebb380ec8898282573eab09c29")), available_count)
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

data_view_toggle_control <- function(view, step = "step1", language = statedu_initial_language()) {
  if (identical(view, "labels")) {
    return(actionButton("show_data_preview", statedu_text(language, "Data Preview", statedu_utf8("eb8db0ec9db4ed84b020ebafb8eba6acebb3b4eab8b0")), class = "view-toggle-button"))
  }
  if (identical(view, "preview")) {
    label <- if (identical(step, "step3")) statedu_text(language, "Variable Labels", statedu_utf8("ec84b8ec859820ec84a4eca095")) else statedu_text(language, "Variable Info", statedu_utf8("ec84b8ec859820ec84a4eca095"))
    return(actionButton("show_variable_info", label, class = "view-toggle-button"))
  }
  actionButton("show_data_preview", statedu_text(language, "Data Preview", statedu_utf8("eb8db0ec9db4ed84b020ebafb8eba6acebb3b4eab8b0")), class = "view-toggle-button")
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

apply_step3_review_inline_js <- function() {
  paste(
    "if (window.easyflowApplyStep3Review) return window.easyflowApplyStep3Review();",
    "if (window.Shiny) {",
    "  var categoryLabels = {};",
    "  var measurements = {};",
    "  var varLabels = {};",
    "  var selectedMap = {};",
    "  function putMeasurement(name, value) { if (name && value) measurements[name] = value; }",
    "  function putVarLabel(name, value) { if (!name) return; varLabels[name] = value || ''; categoryLabels[name] = categoryLabels[name] || {}; categoryLabels[name].var_label = value || ''; }",
    "  var labelRoot = document.getElementById('category_label_table') || document;",
    "  labelRoot.querySelectorAll('input[data-name][data-field]').forEach(function(input) {",
    "    var name = input.getAttribute('data-name') || '';",
    "    var field = input.getAttribute('data-field') || '';",
    "    if (!name || !field || input.disabled) return;",
    "    categoryLabels[name] = categoryLabels[name] || {};",
    "    categoryLabels[name][field] = input.value || '';",
    "    if (field === 'var_label') putVarLabel(name, input.value || '');",
    "  });",
    "  labelRoot.querySelectorAll('select.category-measurement-select[data-name]').forEach(function(select) {",
    "    putMeasurement(select.getAttribute('data-name') || '', select.value || '');",
    "  });",
    "  var selectedRoot = document.getElementById('selected_variable_edit_table') || document;",
    "  selectedRoot.querySelectorAll('input.variable-select[data-name]').forEach(function(input) {",
    "    var name = input.getAttribute('data-name') || '';",
    "    if (name && !input.disabled && input.checked) selectedMap[name] = true;",
    "  });",
    "  selectedRoot.querySelectorAll('select.measurement-select[data-name]').forEach(function(select) {",
    "    putMeasurement(select.getAttribute('data-name') || '', select.value || '');",
    "  });",
    "  selectedRoot.querySelectorAll('input.var-label-input[data-name], input[data-field=\"var_label\"][data-name]').forEach(function(input) {",
    "    putVarLabel(input.getAttribute('data-name') || '', input.value || '');",
    "  });",
    "  var measurementPairs = Object.keys(measurements).map(function(name) { return {name:name, value:measurements[name]}; });",
    "  var varLabelPairs = Object.keys(varLabels).map(function(name) { return {name:name, value:varLabels[name]}; });",
    "  var selected = Object.keys(selectedMap);",
    "  var nonce = Date.now() + Math.random();",
    "  Shiny.setInputValue('apply_selected_variable_review_request', {selected:selected, measurements:measurements, measurement_pairs:measurementPairs, var_labels:varLabels, nonce:nonce}, {priority:'event'});",
    "  Shiny.setInputValue('apply_category_labels_request', {category_labels:categoryLabels, measurements:measurements, measurement_pairs:measurementPairs, var_labels:varLabels, var_label_pairs:varLabelPairs, selected:selected, nonce:nonce + 1}, {priority:'event'});",
    "  Shiny.setInputValue('variable_measurement_snapshot', {values:measurements, measurement_pairs:measurementPairs, nonce:nonce + 2}, {priority:'event'});",
    "}",
    "return false;",
    sep = "\n"
  )
}

data_steps_state <- function(
  file = NULL,
  pending_file = NULL,
  excel_sheets = character(0),
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
  has_pending_excel <- valid_pending_excel_file_value(pending_file)
  list(
    step = step,
    has_open_data = has_open_data,
    has_pending_excel = has_pending_excel,
    has_data = has_open_data || !is.null(restored_info),
    applied = isTRUE(applied),
    role_applied = isTRUE(role_applied),
    role = role,
    data_file_name = if (has_open_data) file$name else "",
    pending_data_file_name = if (has_pending_excel) pending_file$name else "",
    excel_sheets = excel_sheets,
    excel_sheet = if (has_pending_excel) pending_file$excel_sheet %||% "" else "",
    excel_start_cell = if (has_pending_excel) pending_file$excel_start_cell %||% "A1" else "A1",
    excel_col_names = if (has_pending_excel) isTRUE(pending_file$excel_col_names %||% TRUE) else TRUE,
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
  has_pending_excel = FALSE,
  has_data,
  applied,
  role_applied,
  role,
  data_file_name,
  pending_data_file_name = "",
  excel_sheets = character(0),
  excel_sheet = "",
  excel_start_cell = "A1",
  excel_col_names = TRUE,
  data_n_variables,
  data_n_rows,
  restored_data_file_name,
  restored_n_variables,
  selected_count,
  dependent_count,
  independent_count,
  control_count,
  has_calculated_variables = FALSE,
  language = statedu_initial_language()
) {
  step_class <- function(name, enabled = TRUE) {
    paste("step-block", if (identical(step, name)) "is-open" else "is-closed", if (!enabled) "is-disabled" else "")
  }

  tagList(
    div(
      class = step_class("step1"),
      h3(actionLink("go_step1", statedu_text(language, "Step 1. Load data file", statedu_utf8("5374657020312e20eb8db0ec9db4ed84b020ed8c8cec9dbc20ec97b4eab8b0")), class = "step-link")),
      if (has_data && !identical(step, "step1")) {
        div(
          class = "step-summary",
          div(if (has_open_data) data_file_name else restored_data_file_name, class = "step-summary-title"),
          div(
            if (has_open_data) {
              sprintf(statedu_text(language, "%s variables, %s rows", statedu_utf8("ebb380ec8898202573eab09c2c20ed9689202573eab09c")), data_n_variables, data_n_rows)
            } else {
              sprintf(statedu_text(language, "%s variables saved in settings. Reopen the data file before running analysis.", statedu_utf8("ec84a4eca095ec979020ebb380ec8898202573eab09ceab08020eca080ec9ea5eb9098ec96b420ec9e88ec8ab5eb8b88eb8ba42e20ebb684ec849d20eca08420eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420eb8ba4ec8b9c20ec97b4ec96b420eca3bcec84b8ec9a942e")), restored_n_variables)
            },
            class = "step-summary-detail"
          ),
          if (!has_open_data) {
            actionButton("browse_data_file", statedu_text(language, "Reconnect data file", statedu_utf8("eb8db0ec9db4ed84b020ed8c8cec9dbc20eb8ba4ec8b9c20ec97b0eab2b0")))
          }
        )
      } else {
        if (isTRUE(has_pending_excel)) {
          tagList(
            div(pending_data_file_name, class = "step-summary-title"),
            div(statedu_text(language, "Choose Excel import options, review the data on the right, then import.", statedu_utf8("457863656c20ebb688eb9facec98a4eab8b020ec98b5ec8598ec9d8420ec84a0ed839ded9598eab3a020ec98a4eba5b8ecaabd20eb8db0ec9db4ed84b0eba5bc20eab280ed86a0ed959c20eb92a420ebb688eb9facec98a4ec84b8ec9a942e")), class = "step-note"),
            selectInput(
              "excel_import_sheet",
              statedu_text(language, "Sheet", statedu_utf8("eb9dbcebb2a8")),
              choices = excel_sheets,
              selected = if (nzchar(excel_sheet)) excel_sheet else if (length(excel_sheets) > 0) excel_sheets[[1]] else ""
            ),
            textInput("excel_import_start_cell", statedu_text(language, "Data starts at", statedu_utf8("eb8db0ec9db4ed84b020ec8b9cec9e9120ec8580")), value = excel_start_cell %||% "A1", placeholder = "A1 or B4"),
            checkboxInput("excel_import_col_names", statedu_text(language, "First row contains variable names", statedu_utf8("ecb2ab20ed9689ec979020ebb380ec8898ebaa85ec9db420ec9e88ec9d8c")), value = isTRUE(excel_col_names)),
            div(
              class = "excel-import-actions",
              actionButton("apply_excel_import", statedu_text(language, "Import", statedu_utf8("ebb688eb9facec98a4eab8b0")), class = "btn btn-primary"),
              actionButton("cancel_excel_import", statedu_text(language, "Cancel", statedu_utf8("eb9dbcebb2a8")), class = "btn btn-default")
            )
          )
        } else {
          tagList(
            actionButton("browse_data_file", statedu_ui_label("open_data_file", language)),
            checkboxInput("header", statedu_text(language, "CSV first row contains variable names", statedu_utf8("43535620ecb2ab20ed9689ec979020ebb380ec8898ebaa85ec9db420ec9e88ec9d8c")), TRUE),
            selectInput(
              "dat_delimiter",
              statedu_text(language, "DAT delimiter", statedu_utf8("44415420eab5acebb684ec9e90")),
              choices = c("Whitespace" = "whitespace", "Comma" = "comma", "Tab" = "tab"),
              selected = "whitespace"
            ),
            checkboxInput("dat_has_names", statedu_text(language, "DAT first row contains variable names", statedu_utf8("44415420ecb2ab20ed9689ec979020ebb380ec8898ebaa85ec9db420ec9e88ec9d8c")), FALSE)
          )
        }
      }
    ),
    if (has_data) {
      div(
        class = step_class("step2", has_data),
        h3(actionLink("go_step2", statedu_text(language, "Step 2. Select analysis variables", statedu_utf8("5374657020322e20ebb684ec849d20ebb380ec889820ec84a0ed839d")), class = "step-link")),
        if (!identical(step, "step2") && applied) {
          div(
            class = "step-summary",
            div(sprintf(statedu_text(language, "%s variables selected", statedu_utf8("ebb380ec8898202573eab09c20ec84a0ed839deb90a8")), selected_count), class = "step-summary-title"),
            actionButton("modify_variable_selection", statedu_text(language, "Modify selection", statedu_utf8("ec84a0ed839d20ec8898eca095")))
          )
        } else if (identical(step, "step2")) {
          tagList(
            div(statedu_text(language, "Check variables to keep, then apply the selection.", statedu_utf8("ec82acec9aa9ed95a020ebb380ec8898eba5bc20ecb2b4ed81aced959c20eb92a420ec84a0ed839dec9d8420eca081ec9aa9ed9598ec84b8ec9a942e")), class = "step-note"),
            div(
              class = "bulk-measurement-control",
              selectInput(
                "bulk_measurement_type",
                statedu_text(language, "Set selected variable type to", statedu_utf8("ec84a0ed839d20ebb380ec889820ec9ca0ed989520ec9dbceab48420eca780eca095")),
                choices = statedu_measurement_choices(language),
                selected = "continuous"
              ),
              actionButton("apply_bulk_measurement_type", statedu_text(language, "Apply type", statedu_utf8("ec9ca0ed989520eca081ec9aa9")), class = "btn btn-default", onclick = "if(window.easyflowApplyBulkMeasurement){window.easyflowApplyBulkMeasurement(); return false;}")
            ),
            div(
              class = "bulk-measurement-action",
              actionButton("apply_variable_selection", statedu_text(language, "Apply variable selection", statedu_utf8("ebb380ec889820ec84a0ed839d20eca081ec9aa9")), class = "btn btn-primary", onmousedown = "if(window.easyflowFlushVariableTableState){window.easyflowFlushVariableTableState();}", onclick = "if(window.easyflowApplyVariableSelection){return window.easyflowApplyVariableSelection();}")
            )
          )
        } else {
          div(statedu_text(language, "Select variables first.", statedu_utf8("eba8bceca08020ebb380ec8898eba5bc20ec84a0ed839ded9598ec84b8ec9a942e")), class = "step-note")
        }
      )
    },
    if (has_data && applied) {
      div(
        class = step_class("step3", applied),
        h3(actionLink("go_step3", statedu_text(language, "Step 3. Variable review", statedu_utf8("5374657020332e20ebb380ec889820eab280ed86a0")), class = "step-link")),
        if (identical(step, "step3")) {
          tagList(
            div(statedu_text(language, "Review selected variables or edit categorical value labels.", statedu_utf8("ec84a0ed839d20ebb380ec8898eba5bc20eab280ed86a0ed9598eab1b0eb829820ebb294eca3bceab09220eb9dbcebb2a8ec9d8420ed8eb8eca791ed9598ec84b8ec9a942e")), class = "step-note"),
            div(
              class = "step3-view-toggle",
              tags$button(
                type = "button",
                class = "step3-toggle-combined step3-control-button",
                onclick = paste0(
                  "(function(button){",
                  "var selected=button.querySelector('[data-step3-selected]');",
                  "var next=(selected&&selected.classList.contains('is-active'))?'labels':'variables';",
                  "var label=button.querySelector('[data-step3-label]');",
                  "if(label){label.classList.toggle('is-active',next==='labels');}",
                  "if(selected){selected.classList.toggle('is-active',next==='variables');}",
                  "document.querySelectorAll('.step3-labels-section').forEach(function(section){section.style.display=next==='labels'?'':'none';});",
                  "document.querySelectorAll('.step3-variables-section').forEach(function(section){section.style.display=next==='variables'?'':'none';});",
                  "window.easyflowStep3View=next;",
                  "if(window.Shiny){Shiny.setInputValue('step3_panel_view',next,{priority:'event'});}",
                  "})(this); return false;"
                ),
                tags$span(`data-step3-label` = TRUE, class = "is-active", statedu_text(language, "Labels", statedu_utf8("eb9dbcebb2a8"))),
                tags$span(class = "step3-toggle-divider", "/"),
                tags$span(`data-step3-selected` = TRUE, statedu_text(language, "Variables", statedu_utf8("ebb380ec8898")))
              )
            ),
            div(
              class = "step3-action-row",
              tags$button(id = "apply_step3_review", type = "button", statedu_text(language, "Apply", statedu_utf8("eca081ec9aa9")), class = "btn btn-primary step3-control-button", onclick = apply_step3_review_inline_js()),
              if (isTRUE(has_calculated_variables)) {
                actionButton("save_current_data_file", statedu_text(language, "Save data", statedu_utf8("eb8db0ec9db4ed84b020eca080ec9ea5")), class = "btn btn-default")
              }
            )
          )
        } else {
          div(
            class = "step-summary",
            div(statedu_text(language, "Variable review", statedu_utf8("ec84b8ec859820ec84a4eca095")), class = "step-summary-title"),
            div(statedu_text(language, "Click Step 3 to review selected variables or edit labels.", statedu_utf8("537465702033ec9d8420ed81b4eba6aded95b420ec84a0ed839d20ebb380ec8898eba5bc20eab280ed86a0ed9598eab1b0eb829820eb9dbcebb2a8ec9d8420ed8eb8eca791ed9598ec84b8ec9a942e")), class = "step-summary-detail")
          )
        }
      )
    },
    div(
      class = "step-block session-settings-block",
      h3(statedu_text(language, "Session settings", statedu_utf8("ec84b8ec859820ec84a4eca095"))),
      div(
        class = "session-settings-actions",
        actionButton("browse_settings_data", statedu_ui_label("load_settings", language), class = "session-settings-button"),
        actionButton("save_settings_data", statedu_ui_label("save_settings", language), class = "settings-save-button session-settings-button"),
        actionButton("reset_settings_data", statedu_ui_label("reset_settings", language), class = "reset-settings-button session-settings-button")
      )
    )
  )
}
