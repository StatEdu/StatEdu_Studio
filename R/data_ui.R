data_tab_panel <- function() {
  tabPanel(
    "Data",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1("EasyFlow Statistics"),
        div("SPSS SAV, CSV, DAT files can be loaded and summarized before regression analysis.", class = "app-subtitle")
      ),
      div(
        class = "data-layout",
        div(
          class = "side-panel",
          uiOutput("data_steps")
        ),
        div(
          class = "workspace-panel",
          div(class = "load-message", textOutput("data_loaded_message")),
          div(
            class = "workspace-header",
            h3(textOutput("data_view_title")),
            uiOutput("data_view_toggle")
          ),
          conditionalPanel(
            condition = "output.data_view === 'info'",
            DTOutput("variable_table")
          ),
          conditionalPanel(
            condition = "output.data_view === 'preview'",
            DTOutput("data_preview_table")
          ),
          conditionalPanel(
            condition = "output.data_view === 'labels'",
            DTOutput("category_label_table")
          )
        )
      )
    )
  )
}

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

category_label_value_columns <- function(max_pairs = 6) {
  as.vector(rbind(paste0("value_", seq_len(max_pairs)), paste0("label_", seq_len(max_pairs))))
}

category_label_edit_columns <- function(max_pairs = 6) {
  c("var_label", "reference", "reference_label", category_label_value_columns(max_pairs))
}

category_label_input_renderer <- function(field_name, unique_index) {
  DT::JS(
    "function(data, type, row, meta) {",
    "  if (type !== 'display') return data;",
    "  function esc(x) {",
    "    if (x === null || x === undefined) return '';",
    "    return String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\"/g, '&quot;');",
    "  }",
    sprintf("  var fieldName = '%s';", field_name),
    sprintf("  var uniqueIndex = %s;", unique_index),
    "  var pairMatch = fieldName.match(/^(value|label)_(\\d+)$/);",
    "  var cls = fieldName === 'var_label' ? 'var-label-input' : (pairMatch ? 'category-label-input value-label-input' : 'category-label-input category-meta-input');",
    "  var pairNumber = pairMatch ? parseInt(pairMatch[2], 10) : null;",
    "  var nUnique = uniqueIndex >= 0 ? parseInt(row[uniqueIndex], 10) : NaN;",
    "  var disabled = pairNumber !== null && isFinite(nUnique) && pairNumber > Math.max(1, Math.min(6, nUnique));",
    "  var disabledAttr = disabled ? ' disabled' : '';",
    "  var tabAttr = pairMatch && !disabled ? '' : ' tabindex=\"-1\"';",
    "  var name = row[2];",
    "  var inputId = (fieldName === 'var_label' ? 'category_var_label_input_' + row[0] : '');",
    "  var value = data;",
    "  if (fieldName === 'var_label') {",
    "    window.easyflowVarLabels = window.easyflowVarLabels || {};",
    "    var hasClientValue = Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name);",
    "    if (data !== null && data !== undefined && String(data).length > 0 && (!hasClientValue || String(window.easyflowVarLabels[name]).length === 0)) window.easyflowVarLabels[name] = data;",
    "    if (Object.prototype.hasOwnProperty.call(window.easyflowVarLabels, name)) value = window.easyflowVarLabels[name];",
    "  }",
    "  var idAttr = inputId ? ' id=\"' + esc(inputId) + '\"' : '';",
    "  var varLabelHandlers = fieldName === 'var_label' ? ' oninput=\"window.easyflowStoreVarLabel && window.easyflowStoreVarLabel(this)\" onchange=\"window.easyflowCommitVarLabel && window.easyflowCommitVarLabel(this)\"' : '';",
    "  return '<input type=\"text\"' + idAttr + ' class=\"form-control input-sm ' + cls + '\" data-name=\"' + esc(name) + '\" data-field=\"' + fieldName + '\" value=\"' + esc(value) + '\"' + disabledAttr + tabAttr + varLabelHandlers + '>';",
    "}"
  )
}

category_label_column_defs <- function(table_data, edit_columns = category_label_edit_columns()) {
  unique_index <- match("n_unique", names(table_data)) - 1L
  column_defs <- list(
    list(targets = 1, render = DT::JS(
      "function(data, type, row, meta) {",
      "  if (type === 'display') return '<input type=\"checkbox\" checked disabled>';",
      "  return data;",
      "}"
    ), orderable = FALSE),
    list(visible = FALSE, targets = 0),
    list(visible = FALSE, targets = unique_index)
  )
  for (field_name in edit_columns) {
    target_index <- match(field_name, names(table_data)) - 1L
    if (is.na(target_index)) {
      next
    }
    column_defs <- append(
      column_defs,
      list(list(
        targets = target_index,
        render = category_label_input_renderer(field_name, unique_index),
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
          Shiny.setInputValue('variable_table_state', {
            selected: state.selected,
            measurements: state.measurements,
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
        });
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
        syncVariableSelection();
        "
}
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
    role_label <- switch(
      active_role,
      independent = "independent variables",
      control = "control/covariates",
      "dependent variable"
    )
    return(sprintf("Select %s (%s of %s selected)", role_label, active_role_count, selected_count))
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

data_view_toggle_control <- function(view) {
  if (identical(view, "labels")) {
    return(NULL)
  }
  if (identical(view, "preview")) {
    return(actionButton("show_variable_info", "Variable Info", class = "view-toggle-button"))
  }
  actionButton("show_data_preview", "Data Preview", class = "view-toggle-button")
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
  controls = character(0)
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
    control_count = length(controls)
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
  control_count
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
            tags$button(
              id = "apply_variable_selection_button",
              "Apply variable selection",
              type = "button",
              class = "btn btn-primary",
              onmousedown = "window.easyflowFlushVariableTableState && window.easyflowFlushVariableTableState();",
              onclick = "return window.easyflowApplyVariableSelection ? window.easyflowApplyVariableSelection() : true;"
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
        h3(actionLink("go_step3", "Step 3. Assign variable roles", class = "step-link")),
        if (!identical(step, "step3") && role_applied) {
          div(
            class = "step-summary",
            div(sprintf(
              "Dependent %s, independent %s, covariates %s",
              dependent_count, independent_count, control_count
            ), class = "step-summary-title")
          )
        } else if (identical(step, "step3")) {
          tagList(
            div("Use the checkbox column in the variable table to select variables for the active role.", class = "step-note"),
            div(
              class = "role-actions",
              tags$button(
                id = "select_dependent_role_button",
                sprintf("Dependent (%s)", dependent_count),
                type = "button",
                class = paste("role-button", if (identical(role, "dependent")) "is-active" else ""),
                onmousedown = "window.easyflowFlushVariableTableState && window.easyflowFlushVariableTableState();",
                onclick = "return window.easyflowSelectRole ? window.easyflowSelectRole('dependent') : true;"
              ),
              tags$button(
                id = "select_independent_role_button",
                sprintf("Independent (%s)", independent_count),
                type = "button",
                class = paste("role-button", if (identical(role, "independent")) "is-active" else ""),
                onmousedown = "window.easyflowFlushVariableTableState && window.easyflowFlushVariableTableState();",
                onclick = "return window.easyflowSelectRole ? window.easyflowSelectRole('independent') : true;"
              ),
              tags$button(
                id = "select_control_role_button",
                sprintf("Control/Covariates (%s)", control_count),
                type = "button",
                class = paste("role-button", if (identical(role, "control")) "is-active" else ""),
                onmousedown = "window.easyflowFlushVariableTableState && window.easyflowFlushVariableTableState();",
                onclick = "return window.easyflowSelectRole ? window.easyflowSelectRole('control') : true;"
              )
            ),
            div(
              sprintf(
                "Selecting: %s",
                switch(role, independent = "Independent variables", control = "Control/Covariates", "Dependent variable")
              ),
              class = "step-summary-detail"
            ),
            tags$button(
              id = "apply_role_selection_button",
              "Apply variable roles",
              type = "button",
              class = "btn btn-primary",
              onmousedown = "window.easyflowFlushVariableTableState && window.easyflowFlushVariableTableState();",
              onclick = "return window.easyflowApplyRoleSelection ? window.easyflowApplyRoleSelection() : true;"
            )
          )
        }
      )
    },
    if (has_data && role_applied) {
      div(
        class = step_class("step4", role_applied),
        h3(actionLink("go_step4", "Step 4. Category labels", class = "step-link")),
        if (identical(step, "step4")) {
          div("Edit value labels for categorical variables. These labels are saved with the session settings.", class = "step-note")
        } else {
          div(
            class = "step-summary",
            div("Categorical value labels", class = "step-summary-title"),
            div("Click Step 4 to review or edit labels.", class = "step-summary-detail")
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
