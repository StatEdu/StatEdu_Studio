# Data tab DT table renderers and callbacks.

empty_variable_table <- function() {
  DT::datatable(
    data.frame(Message = "Read a SAV, CSV, or DAT file to show variable information."),
    rownames = FALSE,
    options = list(dom = "t")
  )
}

empty_category_label_table <- function() {
  DT::datatable(
    data.frame(Message = "Read a SAV, CSV, or DAT file first.", check.names = FALSE),
    rownames = FALSE,
    options = list(dom = "t")
  )
}

empty_data_preview_table <- function() {
  DT::datatable(
    data.frame(Message = "Reopen the data file to preview data rows."),
    rownames = FALSE,
    options = list(dom = "t")
  )
}

data_preview_table_options <- function() {
  list(
    pageLength = 20,
    lengthMenu = c(10, 20, 50, 100),
    deferRender = TRUE,
    searchDelay = 250,
    scrollX = TRUE,
    autoWidth = TRUE
  )
}

data_preview_datatable <- function(data, n = 20) {
  DT::datatable(
    head(data, n),
    rownames = FALSE,
    filter = "top",
    options = data_preview_table_options()
  )
}

category_label_input_renderer <- function(field_name, unique_index, name_index, source_order_index) {
  DT::JS(
    "function(data, type, row, meta) {",
    "  if (type !== 'display') return data;",
    "  function esc(x) {",
    "    if (x === null || x === undefined) return '';",
    "    return String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\"/g, '&quot;');",
    "  }",
    sprintf("  var fieldName = '%s';", field_name),
    sprintf("  var uniqueIndex = %s;", unique_index),
    sprintf("  var nameIndex = %s;", name_index),
    sprintf("  var sourceOrderIndex = %s;", source_order_index),
    "  var pairMatch = fieldName.match(/^(value|label)_(\\d+)$/);",
    "  var cls = fieldName === 'var_label' ? 'var-label-input' : (pairMatch ? 'category-label-input value-label-input' : 'category-label-input category-meta-input');",
    "  var pairNumber = pairMatch ? parseInt(pairMatch[2], 10) : null;",
    "  var nUnique = uniqueIndex >= 0 ? parseInt(row[uniqueIndex], 10) : NaN;",
    "  var disabled = pairNumber !== null && isFinite(nUnique) && pairNumber > Math.max(1, Math.min(6, nUnique));",
    "  var disabledAttr = disabled ? ' disabled' : '';",
    "  var tabAttr = pairMatch && !disabled ? '' : ' tabindex=\"-1\"';",
    "  var name = nameIndex >= 0 ? row[nameIndex] : '';",
    "  var sourceOrder = sourceOrderIndex >= 0 ? row[sourceOrderIndex] : meta.row;",
    "  var inputId = fieldName === 'var_label' ? 'category_var_label_input_' + sourceOrder : 'category_' + fieldName + '_input_' + sourceOrder;",
    "  var value = data;",
    "  if (fieldName === 'var_label') {",
    "    window.easyflowVarLabels = window.easyflowVarLabels || {};",
    "    var hasClientValue = Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name);",
    "    if (data !== null && data !== undefined && String(data).length > 0 && (!hasClientValue || String(window.easyflowVarLabels[name]).length === 0)) window.easyflowVarLabels[name] = data;",
    "    if (Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name)) value = window.easyflowVarLabels[name];",
    "  }",
    "  var idAttr = inputId ? ' id=\"' + esc(inputId) + '\"' : '';",
    "  var directHandlers = ' onchange=\"if(window.Shiny){Shiny.setInputValue(&quot;category_label_cell_input&quot;,{name:this.getAttribute(&quot;data-name&quot;),field:this.getAttribute(&quot;data-field&quot;),value:this.value,nonce:Date.now()+Math.random()},{priority:&quot;event&quot;});}\"';",
    "  var listAttr = '';",
    "  var dataList = '';",
    "  if (fieldName === 'reference') {",
    "    var listId = 'reference_values_' + esc(sourceOrder);",
    "    listAttr = ' list=\"' + listId + '\"';",
    "    dataList = '<datalist id=\"' + listId + '\">';",
    "    for (var optionIndex = 1; optionIndex <= 6; optionIndex++) {",
    "      var valueColumn = 'value_' + optionIndex;",
    "      var columnIndex = meta.settings.aoColumns.findIndex(function(column) { return column.sTitle === valueColumn; });",
    "      if (columnIndex < 0) continue;",
    "      var optionValue = row[columnIndex];",
    "      if (optionValue === null || optionValue === undefined || String(optionValue).length === 0) continue;",
    "      dataList += '<option value=\"' + esc(optionValue) + '\"></option>';",
    "    }",
    "    dataList += '</datalist>';",
    "  }",
    "  return '<input type=\"text\"' + idAttr + listAttr + ' class=\"form-control input-sm ' + cls + '\" data-name=\"' + esc(name) + '\" data-field=\"' + fieldName + '\" value=\"' + esc(value) + '\"' + disabledAttr + tabAttr + directHandlers + '>' + dataList;",
    "}"
  )
}

category_label_measurement_renderer <- function(name_index, source_order_index) {
  DT::JS(
    "function(data, type, row, meta) {",
    "  if (type !== 'display') return data;",
    "  function esc(x) {",
    "    if (x === null || x === undefined) return '';",
    "    return String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\"/g, '&quot;');",
    "  }",
    sprintf("  var nameIndex = %s;", name_index),
    sprintf("  var sourceOrderIndex = %s;", source_order_index),
    "  var name = nameIndex >= 0 ? row[nameIndex] : '';",
    "  var sourceOrder = sourceOrderIndex >= 0 ? row[sourceOrderIndex] : meta.row;",
    "  var current = String(data || 'category');",
    "  window.easyflowMeasurements = window.easyflowMeasurements || {};",
    "  if (name && Object.prototype.hasOwnProperty.call(window.easyflowMeasurements, name)) current = String(window.easyflowMeasurements[name] || current);",
    "  var values = ['binary', 'category', 'ordered', 'continuous'];",
    "  var labels = ['binary', 'category', 'ordinal', 'continuous'];",
    "  var html = '<select id=\"category_measurement_input_' + esc(sourceOrder) + '\" class=\"measurement-select category-measurement-select\" data-name=\"' + esc(name) + '\" data-field=\"measurement\" onchange=\"if(window.Shiny){Shiny.setInputValue(&quot;variable_measurement_update&quot;,{name:this.getAttribute(&quot;data-name&quot;),value:this.value,nonce:Date.now()+Math.random()},{priority:&quot;event&quot;});}\">';",
    "  for (var i = 0; i < values.length; i++) {",
    "    var selected = values[i] === current ? ' selected' : '';",
    "    html += '<option value=\"' + values[i] + '\"' + selected + '>' + labels[i] + '</option>';",
    "  }",
    "  html += '</select>';",
    "  return html;",
    "}"
  )
}

category_label_column_defs <- function(table_data, edit_columns = category_label_edit_columns()) {
  unique_index <- match("n_unique", names(table_data)) - 1L
  source_order_index <- match("source_order", names(table_data)) - 1L
  name_index <- match("name", names(table_data)) - 1L
  selected_index <- match("selected", names(table_data)) - 1L
  column_defs <- list()
  var_label_index <- match("var_label", names(table_data)) - 1L
  if (!is.na(var_label_index) && var_label_index >= 0) {
    column_defs <- append(column_defs, list(list(targets = var_label_index, width = "170px")))
  }
  compact_value_indices <- which(grepl("^(reference|value_\\d+)$", names(table_data))) - 1L
  compact_value_indices <- compact_value_indices[compact_value_indices >= 0]
  if (length(compact_value_indices) > 0) {
    column_defs <- append(column_defs, list(list(targets = compact_value_indices, width = "72px")))
  }
  measurement_index <- match("measurement", names(table_data)) - 1L
  if (!is.na(measurement_index) && measurement_index >= 0) {
    column_defs <- append(
      column_defs,
      list(list(
        targets = measurement_index,
        render = category_label_measurement_renderer(name_index, source_order_index),
        orderable = FALSE,
        width = "130px"
      ))
    )
  }
  if (!is.na(selected_index) && selected_index >= 0) {
    column_defs <- append(column_defs, list(list(targets = selected_index, render = DT::JS(
      "function(data, type, row, meta) {",
      "  if (type === 'display') return '<input type=\"checkbox\" checked disabled>';",
      "  return data;",
      "}"
    ), orderable = FALSE)))
  }
  if (!is.na(source_order_index) && source_order_index >= 0) {
    column_defs <- append(column_defs, list(list(visible = FALSE, targets = source_order_index)))
  }
  if (!is.na(unique_index) && unique_index >= 0) {
    column_defs <- append(column_defs, list(list(visible = FALSE, targets = unique_index)))
  }
  for (field_name in edit_columns) {
    target_index <- match(field_name, names(table_data)) - 1L
    if (is.na(target_index)) {
      next
    }
    column_defs <- append(
      column_defs,
      list(list(
        targets = target_index,
        render = category_label_input_renderer(field_name, unique_index, name_index, source_order_index),
        orderable = FALSE
      ))
    )
  }
  column_defs
}

category_label_table_options <- function(column_defs) {
  list(
    dom = '<"variable-table-top"lfp>rt<"variable-table-bottom"ip>',
    pageLength = 20,
    lengthMenu = c(10, 20, 50, 100),
    deferRender = TRUE,
    searchDelay = 250,
    scrollX = TRUE,
    autoWidth = TRUE,
    order = list(list(0, "asc")),
    columnDefs = column_defs
  )
}

category_label_table_callback <- function() {
  DT::JS(
    "var wrapper = $(table.table().container());",
    "function saveCategoryInput(input) {",
    "  Shiny.setInputValue('category_label_cell_input', {",
    "    name: $(input).attr('data-name'),",
    "    field: $(input).attr('data-field'),",
    "    value: $(input).val(),",
    "    nonce: Date.now() + Math.random()",
    "  }, {priority: 'event'});",
    "}",
    "function saveVarLabelInput(input) {",
    "  var name = $(input).attr('data-name');",
    "  window.easyflowVarLabels = window.easyflowVarLabels || {};",
    "  if (name) window.easyflowVarLabels[name] = $(input).val() || '';",
    "  return name;",
    "}",
    "function commitVarLabelInput(input) {",
    "  var name = saveVarLabelInput(input);",
    "  Shiny.setInputValue('var_label_cell_input', {",
    "    name: name,",
    "    value: name ? window.easyflowVarLabels[name] : ($(input).val() || ''),",
    "    nonce: Date.now() + Math.random()",
    "  }, {priority: 'event'});",
    "}",
    "function rowInput(row, field) { return row.find('input[data-field=\"' + field + '\"]:not(:disabled)'); }",
    "function referenceLabelFor(row, referenceValue) {",
    "  var referenceText = String(referenceValue || '').trim();",
    "  if (!referenceText) return '';",
    "  for (var i = 1; i <= 6; i++) {",
    "    if (String(rowInput(row, 'value_' + i).val() || '').trim() === referenceText) return rowInput(row, 'label_' + i).val() || '';",
    "  }",
    "  return '';",
    "}",
    "wrapper.off('change.categoryLabelInput').on('change.categoryLabelInput', 'input.category-label-input', function() {",
    "  if ($(this).data('skipCategoryChange')) {",
    "    $(this).removeData('skipCategoryChange');",
    "    return;",
    "  }",
    "  saveCategoryInput(this);",
    "  if ($(this).attr('data-field') === 'reference') {",
    "    var row = $(this).closest('tr');",
    "    var labelInput = rowInput(row, 'reference_label');",
    "    labelInput.val(referenceLabelFor(row, $(this).val()));",
    "    saveCategoryInput(labelInput[0]);",
    "  }",
    "});",
    "wrapper.off('change.categoryMeasurementInput').on('change.categoryMeasurementInput', 'select.category-measurement-select', function() {",
    "  var name = $(this).attr('data-name');",
    "  var value = $(this).val();",
    "  window.easyflowMeasurements = window.easyflowMeasurements || {};",
    "  if (name) window.easyflowMeasurements[name] = value;",
    "  Shiny.setInputValue('variable_measurement_update', {",
    "    name: name,",
    "    value: value,",
    "    nonce: Date.now() + Math.random()",
    "  }, {priority: 'event'});",
    "});",
    "wrapper.off('input.varLabelInput').on('input.varLabelInput', 'input.var-label-input', function() {",
    "  saveVarLabelInput(this);",
    "});",
    "wrapper.off('change.varLabelInput focusout.varLabelInput blur.varLabelInput').on('change.varLabelInput focusout.varLabelInput blur.varLabelInput', 'input.var-label-input', function() {",
    "  commitVarLabelInput(this);",
    "  if ($(this).attr('data-field') === 'var_label') saveCategoryInput(this);",
    "});",
    "function rememberCategoryFocus(input) {",
    "  window.easyflowCategoryFocus = {",
    "    name: $(input).attr('data-name'),",
    "    field: $(input).attr('data-field')",
    "  };",
    "}",
    "function restoreCategoryFocus() {",
    "  var target = window.easyflowCategoryFocus;",
    "  if (!target) return;",
    "  var targetInput = wrapper.find('input.value-label-input:not(:disabled)').filter(function() {",
    "    return $(this).attr('data-name') === target.name && $(this).attr('data-field') === target.field;",
    "  }).first();",
    "  if (!targetInput.length) return;",
    "  window.easyflowCategoryFocus = null;",
    "  setTimeout(function() { targetInput.focus().select(); }, 20);",
    "}",
    "function moveCategoryFocus(input, event) {",
    "  var inputs = wrapper.find('input.value-label-input:not(:disabled)').toArray();",
    "  var index = inputs.indexOf(input);",
    "  if (index < 0 || inputs.length === 0) return false;",
    "  var nextIndex = index + (event.shiftKey ? -1 : 1);",
    "  if (nextIndex < 0) nextIndex = inputs.length - 1;",
    "  if (nextIndex >= inputs.length) nextIndex = 0;",
    "  var next = inputs[nextIndex];",
    "  event.preventDefault();",
    "  event.stopPropagation();",
    "  if (event.stopImmediatePropagation) event.stopImmediatePropagation();",
    "  rememberCategoryFocus(next);",
    "  $(input).data('skipCategoryChange', true);",
    "  saveCategoryInput(input);",
    "  setTimeout(function() { $(next).focus().select(); }, 0);",
    "  return true;",
    "}",
    "function bindCategoryTabHandlers() {",
    "  wrapper.find('input.value-label-input').each(function() {",
    "    this.onkeydown = function(event) {",
    "      event = event || window.event;",
    "      if (event.key !== 'Tab' && event.keyCode !== 9) return true;",
    "      return !moveCategoryFocus(this, event);",
    "    };",
    "  });",
    "  restoreCategoryFocus();",
    "}",
    "function bindShinyInputs() {",
    "  if (!window.Shiny) return;",
    "  try { Shiny.unbindAll(wrapper.get(0)); } catch (e) {}",
    "  try { Shiny.bindAll(wrapper.get(0)); } catch (e) {}",
    "}",
    "table.off('draw.dt.easyflowCategoryTab').on('draw.dt.easyflowCategoryTab', bindCategoryTabHandlers);",
    "table.off('draw.dt.easyflowCategoryBind').on('draw.dt.easyflowCategoryBind', bindShinyInputs);",
    "bindCategoryTabHandlers();",
    "bindShinyInputs();"
  )
}

variable_label_input_renderer <- function() {
  DT::JS(
    "function(data, type, row, meta) {",
    "  if (type !== 'display') return data;",
    "  function esc(x) {",
    "    if (x === null || x === undefined) return '';",
    "    return String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\"/g, '&quot;');",
    "  }",
    "  var name = row[2];",
    "  var inputId = 'var_label_input_' + row[1];",
    "  var value = data;",
    "  window.easyflowVarLabels = window.easyflowVarLabels || {};",
    "  var hasClientValue = Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name);",
    "  if (data !== null && data !== undefined && String(data).length > 0 && (!hasClientValue || String(window.easyflowVarLabels[name]).length === 0)) window.easyflowVarLabels[name] = data;",
    "  if (Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name)) value = window.easyflowVarLabels[name];",
    "  return '<input type=\"text\" id=\"' + esc(inputId) + '\" class=\"form-control input-sm var-label-input\" data-name=\"' + esc(name) + '\" value=\"' + esc(value) + '\" oninput=\"window.easyflowStoreVarLabel && window.easyflowStoreVarLabel(this)\" onchange=\"window.easyflowCommitVarLabel && window.easyflowCommitVarLabel(this)\">';",
    "}"
  )
}

variable_table_options <- function() {
  list(
    dom = '<"variable-table-top"lfp>rt<"variable-table-bottom"ip>',
    pageLength = 20,
    lengthMenu = c(10, 20, 50, 100),
    deferRender = TRUE,
    searchDelay = 250,
    scrollX = TRUE,
    autoWidth = TRUE,
    order = list(list(1, "asc")),
    columnDefs = list(
      list(orderable = FALSE, targets = 0),
      list(visible = FALSE, targets = 1),
      list(targets = 3, render = variable_label_input_renderer(), orderable = FALSE)
    )
  )
}

variable_table_callback <- function(
  script,
  selected_names = character(0),
  dependent_only = FALSE,
  single_select_role = FALSE
) {
  DT::JS(gsub(
    "__SINGLE_SELECT_ROLE__",
    jsonlite::toJSON(single_select_role, auto_unbox = TRUE),
    gsub(
      "__DEPENDENT_ONLY__",
      jsonlite::toJSON(dependent_only, auto_unbox = TRUE),
      gsub(
        "__SELECTED_NAMES__",
        jsonlite::toJSON(selected_names, auto_unbox = FALSE),
        script,
        fixed = TRUE
      ),
      fixed = TRUE
    ),
    fixed = TRUE
  ))
}

variable_table_callback_script <- function() {
  "
        var selected = __SELECTED_NAMES__;
        var dependentOnly = __DEPENDENT_ONLY__;
        var singleSelectRole = __SINGLE_SELECT_ROLE__;
        window.easyflowSelectedNames = {};
        selected.forEach(function(name) { window.easyflowSelectedNames[name] = true; });
        var selectedHeader = $(table.column(0).header());
        var nameHeader = $(table.column(2).header());
        var nameSortState = 'original';

        selectedHeader.html('<button type=\"button\" class=\"page-select-toggle is-off\">selected</button>');
        nameHeader.html('<button type=\"button\" class=\"name-sort-toggle\">name <span class=\"sort-mark\">original</span></button>');

        function rememberVariableTablePage() {
          if (window.easyflowVariableTableRestorePending) return;
          try {
            window.easyflowVariableTablePage = table.page.info().page || 0;
          } catch (e) {}
        }

        function requestVariableTablePageRestore() {
          try {
            window.easyflowVariableTablePage = table.page.info().page || 0;
            window.easyflowVariableTableRestorePending = window.easyflowVariableTablePage > 0;
          } catch (e) {
            window.easyflowVariableTableRestorePending = false;
          }
        }

        function restoreVariableTablePage() {
          if (!window.easyflowVariableTableRestorePending) return;
          var page = parseInt(window.easyflowVariableTablePage || 0, 10);
          if (!isFinite(page) || page <= 0) return;
          try {
            var info = table.page.info();
            var maxPage = Math.max(0, (info.pages || 1) - 1);
            var targetPage = Math.min(page, maxPage);
            if ((info.page || 0) !== targetPage) {
              window.easyflowVariableTableRestorePending = false;
              table.page(targetPage).draw(false);
            } else {
              window.easyflowVariableTableRestorePending = false;
            }
          } catch (e) {}
        }

        function scheduleVariableTablePageRestore() {
          [0, 50, 150, 300].forEach(function(delay) {
            window.setTimeout(restoreVariableTablePage, delay);
          });
        }

        function syncVariableSelection() {
          var state = currentTableState();
          Shiny.setInputValue('variable_table_state', {
            selected: state.selected,
            measurements: state.measurements,
            var_labels: state.var_labels,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }

        function syncVariableTableState() {
          var state = currentTableState();
          var measurementPairs = Object.keys(state.measurements || {}).map(function(name) {
            return {name: name, value: state.measurements[name]};
          });
          Shiny.setInputValue('variable_table_state', {
            selected: state.selected,
            measurements: state.measurements,
            measurement_pairs: measurementPairs,
            var_labels: state.var_labels,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }

        function rowNameFromInput(input) {
          var name = $(input).attr('data-name');
          if (name) return name;
          var data = table.row($(input).closest('tr')).data();
          return data && data[2] ? data[2] : '';
        }

        function currentPageNames() {
          var names = [];
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            if (data && data[2]) names.push(data[2]);
          });
          return names;
        }

        function selectablePageNames() {
          var names = [];
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            var checkbox = $(this.node()).find('input.variable-select');
            if (data && data[2] && !checkbox.prop('disabled')) names.push(data[2]);
          });
          return names;
        }

        function updatePageToggle() {
          var names = selectablePageNames();
          var allSelected = names.length > 0 && names.every(function(name) {
            return !!window.easyflowSelectedNames[name];
          });
          selectedHeader.find('.page-select-toggle')
            .toggleClass('is-on', allSelected)
            .toggleClass('is-off', !allSelected);
        }

        function refreshVariableChecks() {
          table.$('input.variable-select').each(function() {
            var name = $(this).data('name');
            if ($(this).prop('disabled')) {
              delete window.easyflowSelectedNames[name];
              $(this).prop('checked', false);
            } else {
              $(this).prop('checked', !!window.easyflowSelectedNames[name]);
            }
          });
          updatePageToggle();
        }

        function updateMeasurementAvailability(select) {
          if (!dependentOnly) return;
          var dropdown = $(select);
          var row = dropdown.closest('tr');
          var checkbox = row.find('input.variable-select');
          var name = checkbox.data('name');
          var isContinuous = dropdown.val() === 'continuous';
          checkbox.prop('disabled', !isContinuous);
          if (isContinuous) {
            checkbox.removeAttr('title');
          } else {
            checkbox.attr('title', 'Dependent variable must be continuous');
            delete window.easyflowSelectedNames[name];
            checkbox.prop('checked', false);
          }
          updatePageToggle();
        }

        function measurementName(select) {
          var name = $(select).attr('data-name') || $(select).data('name');
          if (name) return name;
          var data = table.row($(select).closest('tr')).data();
          return data && data[2] ? data[2] : '';
        }

        function rememberMeasurementSelect(select, notifyServer) {
          var name = measurementName(select);
          if (!name) return;
          window.easyflowMeasurements = window.easyflowMeasurements || {};
          window.easyflowMeasurements[name] = $(select).val() || '';
          if (notifyServer && window.Shiny) {
            Shiny.setInputValue('variable_measurement_update', {
              name: name,
              value: window.easyflowMeasurements[name],
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
          }
        }

        function restoreMeasurementSelects() {
          window.easyflowMeasurements = window.easyflowMeasurements || {};
          var restoreOne = function() {
            var name = measurementName(this);
            if (name && Object.prototype.hasOwnProperty.call(window.easyflowMeasurements, name)) {
              $(this).val(window.easyflowMeasurements[name]);
            }
            updateMeasurementAvailability(this);
          };
          $(table.table().container()).find('select.measurement-select').each(restoreOne);
        }

        function syncMeasurementSnapshot() {
          window.easyflowMeasurements = window.easyflowMeasurements || {};
          var values = Object.assign({}, window.easyflowMeasurements);
          $(table.table().container()).find('select.measurement-select').each(function() {
            var name = $(this).attr('data-name') || $(this).data('name');
            if (!name) return;
            values[name] = $(this).val();
            window.easyflowMeasurements[name] = values[name];
          });
          Shiny.setInputValue('variable_measurement_snapshot', {
            values: values,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }

        window.easyflowApplyBulkMeasurement = function() {
          var value = $('#bulk_measurement_type').val() || '';
          if (['binary', 'category', 'ordered', 'continuous'].indexOf(value) < 0) return false;
          requestVariableTablePageRestore();
          window.easyflowSelectedNames = window.easyflowSelectedNames || {};
          window.easyflowMeasurements = window.easyflowMeasurements || {};
          window.easyflowBulkMeasurementPairs = window.easyflowBulkMeasurementPairs || [];
          var names = [];
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            var checkbox = $(this.node()).find('input.variable-select');
            var isChecked = checkbox.length && checkbox.is(':checked');
            if (data && data[2] && (window.easyflowSelectedNames[data[2]] || isChecked)) {
              names.push(data[2]);
              window.easyflowSelectedNames[data[2]] = true;
            }
          });
          if (names.length === 0) {
            if (window.Shiny) {
              Shiny.setInputValue('variable_measurement_bulk_update', {
                names: [],
                value: value,
                nonce: Date.now() + Math.random()
              }, {priority: 'event'});
            }
            return false;
          }
          names.forEach(function(name) {
            window.easyflowMeasurements[name] = value;
          });
          window.easyflowBulkMeasurementPairs = names.map(function(name) {
            return {name: name, value: value};
          });
          $(table.table().container()).find('select.measurement-select').each(function() {
            var name = $(this).attr('data-name') || $(this).data('name');
            if (name && window.easyflowSelectedNames[name]) {
              $(this).val(value);
              rememberMeasurementSelect(this, true);
              updateMeasurementAvailability(this);
            }
          });
          refreshVariableChecks();
          syncVariableTableState();
          if (window.Shiny) {
            Shiny.setInputValue('variable_measurement_bulk_update', {
              names: names,
              value: value,
              measurements: window.easyflowMeasurements,
              measurement_pairs: window.easyflowBulkMeasurementPairs,
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
            Shiny.setInputValue('variable_measurement_snapshot', {
              values: window.easyflowMeasurements,
              measurement_pairs: window.easyflowBulkMeasurementPairs,
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
          }
          return false;
        };

        function storeVarLabelInput(input) {
          var name = rowNameFromInput(input);
          if (!name) return;
          window.easyflowVarLabels = window.easyflowVarLabels || {};
          window.easyflowVarLabels[name] = $(input).val() || '';
          return name;
        }

        function saveVarLabelInput(input) {
          var name = storeVarLabelInput(input);
          if (!name) return;
          Shiny.setInputValue('var_label_cell_input', {
            name: name,
            value: window.easyflowVarLabels[name],
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }

        function syncVisibleVarLabels() {
          table.$('input.var-label-input').each(function() {
            storeVarLabelInput(this);
          });
        }

        function currentTableState() {
          if (window.easyflowCollectTableState && window.isEasyflowVisibleElement && !window.isEasyflowVisibleElement(table.table().container())) {
            return window.easyflowCollectTableState();
          }
          window.easyflowMeasurements = window.easyflowMeasurements || {};
          var measurements = Object.assign({}, window.easyflowMeasurements);
          $(table.table().container()).find('select.measurement-select').each(function() {
            var name = $(this).attr('data-name') || $(this).data('name');
            if (name) {
              measurements[name] = $(this).val();
              window.easyflowMeasurements[name] = measurements[name];
            }
          });
          var varLabels = {};
          table.$('input.var-label-input, input[data-field=\"var_label\"]').each(function() {
            var name = rowNameFromInput(this);
            if (name) varLabels[name] = $(this).val() || '';
          });
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            var node = $(this.node());
            var input = node.find('td').eq(2).find('input[type=\"text\"]');
            if (data && data[2] && input.length) {
              varLabels[data[2]] = input.val() || '';
            }
          });
          window.easyflowVarLabels = Object.assign({}, window.easyflowVarLabels || {}, varLabels);
          return {
            selected: Object.keys(window.easyflowSelectedNames || {}),
            measurements: measurements,
            var_labels: window.easyflowVarLabels
          };
        }

        function bindShinyInputs() {
          if (!window.Shiny) return;
          try { Shiny.unbindAll($(table.table().container()).get(0)); } catch (e) {}
          try { Shiny.bindAll($(table.table().container()).get(0)); } catch (e) {}
        }

        function setCurrentPageSelection(checked) {
          if (singleSelectRole && checked) {
            var names = selectablePageNames();
            window.easyflowSelectedNames = {};
            if (names.length > 0) window.easyflowSelectedNames[names[0]] = true;
            refreshVariableChecks();
            syncVariableSelection();
            return;
          }
          table.rows({page: 'current'}).every(function() {
            var data = this.data();
            var checkbox = $(this.node()).find('input.variable-select');
            if (!data || !data[2] || checkbox.prop('disabled')) return;
            if (checked) {
              window.easyflowSelectedNames[data[2]] = true;
            } else {
              delete window.easyflowSelectedNames[data[2]];
            }
            checkbox.prop('checked', checked);
          });
          refreshVariableChecks();
          syncVariableSelection();
        }

        table.on('draw.dt', function() {
          restoreMeasurementSelects();
          refreshVariableChecks();
          bindShinyInputs();
          scheduleVariableTablePageRestore();
        });
        table.on('page.dt', rememberVariableTablePage);
        selectedHeader.off('click.easyflowPageToggle').on('click.easyflowPageToggle', '.page-select-toggle', function(e) {
          e.preventDefault();
          e.stopPropagation();
          setCurrentPageSelection(!$(this).hasClass('is-on'));
        });
        nameHeader.off('click.easyflowNameSort').on('click.easyflowNameSort', '.name-sort-toggle', function(e) {
          e.preventDefault();
          e.stopPropagation();
          if (nameSortState === 'original') {
            nameSortState = 'asc';
            table.order([2, 'asc']).draw();
            $(this).find('.sort-mark').text('asc');
          } else if (nameSortState === 'asc') {
            nameSortState = 'desc';
            table.order([2, 'desc']).draw();
            $(this).find('.sort-mark').text('desc');
          } else {
            nameSortState = 'original';
            table.order([1, 'asc']).draw();
            $(this).find('.sort-mark').text('original');
          }
        });
        table.on('change', 'input.variable-select', function() {
          if (window.getSelection) window.getSelection().removeAllRanges();
          $(this).closest('tr').removeClass('selected');
          if ($(this).prop('disabled')) return;
          var name = $(this).data('name');
          if ($(this).is(':checked')) {
            if (singleSelectRole) {
              window.easyflowSelectedNames = {};
            }
            window.easyflowSelectedNames[name] = true;
          } else {
            delete window.easyflowSelectedNames[name];
          }
          refreshVariableChecks();
          syncVariableSelection();
        });
        table.on('click', 'input.variable-select', function(e) {
          e.stopPropagation();
          $(this).closest('tr').removeClass('selected');
        });
        table.on('change', 'select.measurement-select', function(e) {
          e.stopPropagation();
          rememberMeasurementSelect(this, true);
          updateMeasurementAvailability(this);
          syncVariableTableState();
        });
        table.on('preDraw.dt page.dt order.dt search.dt', syncVisibleVarLabels);
        table.on('input keyup', 'input.var-label-input', function(e) {
          e.stopPropagation();
          storeVarLabelInput(this);
        });
        table.on('focusout blur', 'input.var-label-input', function(e) {
          e.stopPropagation();
          saveVarLabelInput(this);
        });
        table.on('change', 'input.var-label-input', function(e) {
          e.stopPropagation();
          saveVarLabelInput(this);
          syncVariableTableState();
        });
        $(document)
          .off('mousedown.easyflowMeasurementSnapshot', '#save_settings_data')
          .on('mousedown.easyflowMeasurementSnapshot', '#save_settings_data', syncMeasurementSnapshot);
        window.easyflowCurrentTableState = currentTableState;
        bindShinyInputs();
        restoreMeasurementSelects();
        refreshVariableChecks();
        scheduleVariableTablePageRestore();
        syncVariableSelection();
        "
}
