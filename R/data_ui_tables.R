# Data tab DT table renderers and callbacks.

empty_variable_table <- function(language = statedu_initial_language()) {
  message_label <- statedu_text(language, "Message", statedu_utf8("eba994ec8b9ceca780"))
  DT::datatable(
    stats::setNames(
      data.frame(statedu_text(
        language,
        "Read a SAV, SAS, Stata, Excel, CSV, or DAT file to show variable information.",
        statedu_utf8("ebb380ec889820eca095ebb3b4eba5bc20ebb3b4eba0a4eba9b4205341562c205341532c2053746174612c20457863656c2c2043535620eb9890eb8a942044415420ed8c8cec9dbcec9d8420ec97acec84b8ec9a942e")
      ), check.names = FALSE),
      message_label
    ),
    rownames = FALSE,
    options = list(dom = "t")
  )
}

empty_category_label_table <- function(language = statedu_initial_language()) {
  message_label <- statedu_text(language, "Message", statedu_utf8("eba994ec8b9ceca780"))
  DT::datatable(
    stats::setNames(
      data.frame(statedu_text(
        language,
        "Read a SAV, SAS, Stata, Excel, CSV, or DAT file first.",
        statedu_utf8("eba8bceca080205341562c205341532c2053746174612c20457863656c2c2043535620eb9890eb8a942044415420ed8c8cec9dbcec9d8420ec97acec84b8ec9a942e")
      ), check.names = FALSE),
      message_label
    ),
    rownames = FALSE,
    options = list(dom = "t")
  )
}

empty_selected_variable_summary_table <- function(language = statedu_initial_language()) {
  message_label <- statedu_text(language, "Message", statedu_utf8("eba994ec8b9ceca780"))
  DT::datatable(
    stats::setNames(
      data.frame(statedu_text(
        language,
        "Apply Step 2 variable selection to review selected variables.",
        statedu_utf8("ec84a0ed839ded959c20ebb380ec8898eba5bc20eab280ed86a0ed9598eba0a4eba9b42053746570203220ebb380ec889820ec84a0ed839dec9d8420eca081ec9aa9ed9598ec84b8ec9a942e")
      ), check.names = FALSE),
      message_label
    ),
    rownames = FALSE,
    options = list(dom = "t")
  )
}

empty_data_preview_table <- function(language = statedu_initial_language()) {
  message_label <- statedu_text(language, "Message", statedu_utf8("eba994ec8b9ceca780"))
  DT::datatable(
    stats::setNames(
      data.frame(statedu_text(
        language,
        "Reopen the data file to preview data rows.",
        statedu_utf8("eb8db0ec9db4ed84b020ed9689ec9d8420ebafb8eba6ac20ebb3b4eba0a4eba9b420eb8db0ec9db4ed84b020ed8c8cec9dbcec9d8420eb8ba4ec8b9c20ec97acec84b8ec9a942e")
      ), check.names = FALSE),
      message_label
    ),
    rownames = FALSE,
    options = list(dom = "t")
  )
}

data_table_header_labels <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  h <- statedu_utf8
  labels <- c(
    "selected" = statedu_text(language, "selected", h("ec84a0ed839d")),
    "source_order" = statedu_text(language, "source_order", h("ec9b90eb9e9820ec889cec849c")),
    "name" = statedu_text(language, "name", h("ebb380ec8898ebaa85")),
    "var_label" = statedu_text(language, "var_label", h("ebb380ec889820eb9dbcebb2a8")),
    "role" = statedu_text(language, "role", h("ec97aded95a0")),
    "measurement" = statedu_text(language, "measurement", h("ecb8a1eca095ec8898eca480")),
    "storage_type" = statedu_text(language, "storage_type", h("eca080ec9ea520ed9895ec8b9d")),
    "n_unique" = statedu_text(language, "n_unique", h("eab3a0ec9ca0eab09220ec8898")),
    "n_missing" = statedu_text(language, "n_missing", h("eab2b0ecb8a1eab09220ec8898")),
    "min_value" = statedu_text(language, "min_value", h("ecb59cec869feab092")),
    "max_value" = statedu_text(language, "max_value", h("ecb59ceb8c93eab092")),
    "Variable" = statedu_text(language, "Variable", h("ebb380ec8898")),
    "Label" = statedu_text(language, "Label", h("eb9dbcebb2a8")),
    "Measurement" = statedu_text(language, "Measurement", h("ecb8a1eca095ec8898eca480")),
    "Reference" = statedu_text(language, "Reference", h("ecb0b8eca1b0eab092")),
    "Min" = statedu_text(language, "Min", h("ecb59cec869feab092")),
    "Max" = statedu_text(language, "Max", h("ecb59ceb8c93eab092")),
    "Missing" = statedu_text(language, "Missing", h("eab2b0ecb8a1eab09220ec8898")),
    "reference" = statedu_text(language, "reference", h("ecb0b8eca1b0eab092")),
    "reference_label" = statedu_text(language, "reference_label", h("ecb0b8eca1b020eb9dbcebb2a8"))
  )
  for (index in seq_len(11)) {
    labels[[paste0("value_", index)]] <- paste(statedu_text(language, "value", h("eab092")), index)
    labels[[paste0("label_", index)]] <- paste(statedu_text(language, "label", h("eab09220eb9dbcebb2a8")), index)
  }
  labels
}

data_table_colnames <- function(columns, language = statedu_initial_language()) {
  labels <- data_table_header_labels(language)
  vapply(as.character(columns), function(column) {
    if (column %in% names(labels)) labels[[column]] else column
  }, character(1), USE.NAMES = FALSE)
}

datatable_language_options <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  if (!identical(language, "ko")) {
    return(NULL)
  }
  list(
    search = statedu_utf8("eab280ec83893a"),
    lengthMenu = statedu_utf8("5f4d454e555feab09cec94a920ebb3b4eab8b0"),
    zeroRecords = statedu_utf8("ed919cec8b9ced95a020ec9e90eba38ceab08020ec9786ec8ab5eb8b88eb8ba42e"),
    emptyTable = statedu_utf8("ec9e90eba38ceab08020ec9786ec8ab5eb8b88eb8ba42e"),
    info = statedu_utf8("ecb49d205f544f54414c5feab09c20eca491205f53544152545f202d205f454e445f20ed919cec8b9c"),
    paginate = list(
      previous = statedu_utf8("ec9db4eca084"),
      `next` = statedu_utf8("eb8ba4ec9d8c")
    )
  )
}

with_datatable_language <- function(options, language = statedu_initial_language()) {
  language_options <- datatable_language_options(language)
  if (!is.null(language_options)) {
    options$language <- language_options
  }
  options
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

category_label_measurement_renderer <- function(name_index, source_order_index, language = statedu_initial_language()) {
  measurement_choices <- statedu_measurement_choices(language)
  measurement_values <- unname(measurement_choices)
  measurement_labels <- names(measurement_choices)
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
    "  if (current === 'ordinal') current = 'ordered';",
    "  if (current === 'nominal') current = 'category';",
    "  window.easyflowMeasurements = window.easyflowMeasurements || {};",
    "  if (name && Object.prototype.hasOwnProperty.call(window.easyflowMeasurements, name)) current = String(window.easyflowMeasurements[name] || current);",
    "  if (current === 'ordinal') current = 'ordered';",
    "  if (current === 'nominal') current = 'category';",
    sprintf("  var values = %s;", jsonlite::toJSON(measurement_values, auto_unbox = FALSE)),
    sprintf("  var labels = %s;", jsonlite::toJSON(measurement_labels, auto_unbox = FALSE)),
    "  var title = current === 'ordered' ? 'ordinal' : current;",
    "  for (var labelIndex = 0; labelIndex < values.length; labelIndex++) {",
    "    if (values[labelIndex] === current) title = labels[labelIndex];",
    "  }",
    "  var html = '<span class=\"measurement-control\"><span class=\"measurement-symbol measurement-' + esc(current) + '\" title=\"' + esc(title) + '\" aria-label=\"' + esc(title) + '\"></span>';",
    "  html += '<select id=\"category_measurement_input_' + esc(sourceOrder) + '\" class=\"measurement-select category-measurement-select\" data-name=\"' + esc(name) + '\" data-field=\"measurement\" onchange=\"if(window.Shiny){Shiny.setInputValue(&quot;variable_measurement_update&quot;,{name:this.getAttribute(&quot;data-name&quot;),value:this.value,nonce:Date.now()+Math.random()},{priority:&quot;event&quot;});} if(window.easyflowUpdateMeasurementControl){window.easyflowUpdateMeasurementControl(this);}\">';",
    "  for (var i = 0; i < values.length; i++) {",
    "    var selected = values[i] === current ? ' selected' : '';",
    "    html += '<option value=\"' + values[i] + '\"' + selected + '>' + labels[i] + '</option>';",
    "  }",
    "  html += '</select></span>';",
    "  return html;",
    "}"
  )
}

category_label_column_defs <- function(table_data, edit_columns = category_label_edit_columns(), language = statedu_initial_language()) {
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
        render = category_label_measurement_renderer(name_index, source_order_index, language),
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

selected_variable_summary_table_options <- function(language = statedu_initial_language()) {
  with_datatable_language(list(
    dom = '<"variable-table-top"lfp>rt<"variable-table-bottom"ip>',
    pageLength = 20,
    lengthMenu = c(10, 20, 50, 100),
    deferRender = TRUE,
    searchDelay = 250,
    scrollX = TRUE,
    autoWidth = TRUE,
    order = list(list(0, "asc"))
  ), language)
}

category_label_table_options <- function(column_defs, language = statedu_initial_language()) {
  with_datatable_language(list(
    dom = '<"variable-table-top"lfp>rt<"variable-table-bottom"ip>',
    pageLength = 20,
    lengthMenu = c(10, 20, 50, 100),
    deferRender = TRUE,
    searchDelay = 250,
    scrollX = TRUE,
    autoWidth = TRUE,
    order = list(list(0, "asc")),
    columnDefs = column_defs
  ), language)
}

category_label_table_callback <- function(language = statedu_initial_language()) {
  header_labels <- jsonlite::toJSON(as.list(data_table_header_labels(language)), auto_unbox = TRUE)
  DT::JS(
    "var wrapper = $(table.table().container());",
    sprintf("var easyflowHeaderLabels = %s;", header_labels),
    "function applyEasyflowHeaderLabels() {",
    "  table.columns().every(function(index) {",
    "    var column = table.settings()[0].aoColumns[index];",
    "    var title = column && column.sTitle ? String(column.sTitle) : '';",
    "    if (Object.prototype.hasOwnProperty.call(easyflowHeaderLabels, title)) {",
    "      $(table.column(index).header()).text(easyflowHeaderLabels[title]);",
    "    }",
    "  });",
    "}",
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
    "applyEasyflowHeaderLabels();",
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

variable_table_options <- function(language = statedu_initial_language()) {
  with_datatable_language(list(
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
  ), language)
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

variable_table_callback_script <- function(language = statedu_initial_language()) {
  header_labels <- jsonlite::toJSON(as.list(data_table_header_labels(language)), auto_unbox = TRUE)
  selected_label <- data_table_header_labels(language)[["selected"]]
  name_label <- data_table_header_labels(language)[["name"]]
  original_label <- statedu_text(language, "original", statedu_utf8("ec9b90ebb3b8"))
  asc_label <- statedu_text(language, "asc", statedu_utf8("ec98a4eba684ecb0a8ec889c"))
  desc_label <- statedu_text(language, "desc", statedu_utf8("eb82b4eba6bcecb0a8ec889c"))
  script <- "
        var selected = __SELECTED_NAMES__;
        var dependentOnly = __DEPENDENT_ONLY__;
        var singleSelectRole = __SINGLE_SELECT_ROLE__;
        var easyflowHeaderLabels = __HEADER_LABELS__;
        var easyflowSelectedLabel = __SELECTED_LABEL__;
        var easyflowNameLabel = __NAME_LABEL__;
        var easyflowOriginalLabel = __ORIGINAL_LABEL__;
        var easyflowAscLabel = __ASC_LABEL__;
        var easyflowDescLabel = __DESC_LABEL__;
        window.easyflowSelectedNames = {};
        selected.forEach(function(name) { window.easyflowSelectedNames[name] = true; });
        var selectedHeader = $(table.column(0).header());
        var nameHeader = $(table.column(2).header());
        var nameSortState = 'original';

        selectedHeader.html('<button type=\"button\" class=\"page-select-toggle is-off\">' + easyflowSelectedLabel + '</button>');
        nameHeader.html('<button type=\"button\" class=\"name-sort-toggle\">' + easyflowNameLabel + ' <span class=\"sort-mark\">' + easyflowOriginalLabel + '</span></button>');

        function applyEasyflowHeaderLabels() {
          table.columns().every(function(index) {
            if (index === 0 || index === 2) return;
            var column = table.settings()[0].aoColumns[index];
            var title = column && column.sTitle ? String(column.sTitle) : '';
            if (Object.prototype.hasOwnProperty.call(easyflowHeaderLabels, title)) {
              $(table.column(index).header()).text(easyflowHeaderLabels[title]);
            }
          });
        }

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
            $(this).find('.sort-mark').text(easyflowAscLabel);
          } else if (nameSortState === 'asc') {
            nameSortState = 'desc';
            table.order([2, 'desc']).draw();
            $(this).find('.sort-mark').text(easyflowDescLabel);
          } else {
            nameSortState = 'original';
            table.order([1, 'asc']).draw();
            $(this).find('.sort-mark').text(easyflowOriginalLabel);
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
        applyEasyflowHeaderLabels();
        bindShinyInputs();
        restoreMeasurementSelects();
        refreshVariableChecks();
        scheduleVariableTablePageRestore();
        syncVariableSelection();
        "
  script <- gsub("__HEADER_LABELS__", header_labels, script, fixed = TRUE)
  script <- gsub("__SELECTED_LABEL__", jsonlite::toJSON(selected_label, auto_unbox = TRUE), script, fixed = TRUE)
  script <- gsub("__NAME_LABEL__", jsonlite::toJSON(name_label, auto_unbox = TRUE), script, fixed = TRUE)
  script <- gsub("__ORIGINAL_LABEL__", jsonlite::toJSON(original_label, auto_unbox = TRUE), script, fixed = TRUE)
  script <- gsub("__ASC_LABEL__", jsonlite::toJSON(asc_label, auto_unbox = TRUE), script, fixed = TRUE)
  script <- gsub("__DESC_LABEL__", jsonlite::toJSON(desc_label, auto_unbox = TRUE), script, fixed = TRUE)
  script
}
