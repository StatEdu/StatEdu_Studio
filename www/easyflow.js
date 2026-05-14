      window.easyflowSettingsDirty = false;
      window.easyflowVarLabels = window.easyflowVarLabels || {};
      window.easyflowMeasurements = window.easyflowMeasurements || {};

      window.easyflowSelectVariableIconOption = function(option) {
        if (!option) return;
        var listbox = option.closest('.variable-icon-listbox');
        if (!listbox) return;
        var inputId = listbox.getAttribute('data-input-id');
        var value = option.getAttribute('data-value') || '';
        if (!inputId || !value) return;

        listbox.querySelectorAll('.variable-icon-option').forEach(function(item) {
          item.classList.toggle('is-selected', item === option);
          item.setAttribute('aria-selected', item === option ? 'true' : 'false');
        });

        var select = document.getElementById(inputId);
        if (select) {
          select.value = value;
          if (window.jQuery) {
            window.jQuery(select).trigger('change');
          } else {
            select.dispatchEvent(new Event('change', {bubbles: true}));
          }
        }
      };

      document.addEventListener('keydown', function(event) {
        var listbox = event.target && event.target.closest ? event.target.closest('.variable-icon-listbox') : null;
        if (!listbox) return;
        if (event.key !== 'ArrowDown' && event.key !== 'ArrowUp') return;
        var options = Array.prototype.slice.call(listbox.querySelectorAll('.variable-icon-option'));
        if (options.length === 0) return;
        var current = options.findIndex(function(item) { return item.classList.contains('is-selected'); });
        var next = event.key === 'ArrowDown'
          ? Math.min(options.length - 1, current + 1)
          : Math.max(0, current - 1);
        if (current < 0) next = 0;
        window.easyflowSelectVariableIconOption(options[next]);
        options[next].scrollIntoView({block: 'nearest'});
        event.preventDefault();
      });

      function captureEasyflowVarLabels() {
        function collectInput(input) {
          var name = input.getAttribute('data-name');
          if (!name) {
            var row = input.closest('tr');
            var cells = row ? row.querySelectorAll('td') : [];
            if (cells.length > 1) name = (cells[1].textContent || '').trim();
          }
          if (name) window.easyflowVarLabels[name] = input.value || '';
        }

        var selectors = [
          'input.var-label-input',
          'input[data-field="var_label"]',
          'table.dataTable tbody tr td:nth-child(3) input[type="text"]'
        ];
        document.querySelectorAll(selectors.join(',')).forEach(collectInput);

        document.querySelectorAll('table.dataTable').forEach(function(table) {
          var headers = Array.prototype.slice.call(table.querySelectorAll('thead tr:first-child th'));
          var labelIndex = headers.findIndex(function(th) {
            return (th.textContent || '').replace(/\s+/g, ' ').trim().indexOf('var_label') >= 0;
          });
          if (labelIndex < 0) return;
          table.querySelectorAll('tbody tr').forEach(function(row) {
            var cells = row.querySelectorAll('td');
            var input = cells[labelIndex] ? cells[labelIndex].querySelector('input[type="text"]') : null;
            if (input) collectInput(input);
          });
        });
        return window.easyflowVarLabels;
      }

      function storeEasyflowVarLabelInput(input) {
        if (!input || !input.matches || !input.matches('input.var-label-input, input[data-field="var_label"]')) return;
        var name = input.getAttribute('data-name');
        if (!name) {
          var row = input.closest('tr');
          var cells = row ? row.querySelectorAll('td') : [];
          if (cells.length > 1) name = (cells[1].textContent || '').trim();
        }
        if (!name) return;
        window.easyflowVarLabels = window.easyflowVarLabels || {};
        window.easyflowVarLabels[name] = input.value || '';
        return name;
      }

      function saveEasyflowVarLabelInput(input) {
        var name = storeEasyflowVarLabelInput(input);
        if (!name) return;
        if (window.Shiny) {
          Shiny.setInputValue('var_label_cell_input', {
            name: name,
            value: window.easyflowVarLabels[name],
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }
      window.easyflowStoreVarLabel = storeEasyflowVarLabelInput;
      window.easyflowCommitVarLabel = saveEasyflowVarLabelInput;

      function collectEasyflowTableState() {
        var selectedMap = Object.assign({}, window.easyflowSelectedNames || {});
        document.querySelectorAll('input.variable-select[data-name]').forEach(function(input) {
          var name = input.getAttribute('data-name');
          if (!name || input.disabled) return;
          if (input.checked) selectedMap[name] = true;
          else delete selectedMap[name];
        });

        var measurements = {};
        document.querySelectorAll('select').forEach(function(select) {
          var optionValues = Array.prototype.slice.call(select.options || []).map(function(option) { return option.value; });
          var isMeasurementSelect = select.classList.contains('measurement-select') ||
            (select.id || '').indexOf('measurement_input_') === 0 ||
            ['binary', 'category', 'ordered', 'continuous'].every(function(value) { return optionValues.indexOf(value) >= 0; });
          if (!isMeasurementSelect) return;

          var name = select.getAttribute('data-name') || '';
          if (!name) {
            var row = select.closest('tr');
            var cells = row ? row.querySelectorAll('td') : [];
            if (cells.length > 1) {
              name = (cells[1].textContent || '').trim();
            }
          }
          if (name) measurements[name] = select.value || '';
        });
        Object.assign(measurements, window.easyflowMeasurements || {});

        var varLabels = Object.assign({}, window.easyflowVarLabels || {}, captureEasyflowVarLabels());
        return {
          selected: Object.keys(selectedMap),
          measurements: measurements,
          var_labels: varLabels
        };
      }
      window.easyflowCollectTableState = collectEasyflowTableState;

      function rememberEasyflowMeasurement(select, measurements) {
        if (!select) return;
        var optionValues = Array.prototype.slice.call(select.options || []).map(function(option) { return option.value; });
        var isMeasurementSelect = select.classList.contains('measurement-select') ||
          (select.id || '').indexOf('measurement_input_') === 0 ||
          ['binary', 'category', 'ordered', 'continuous'].every(function(value) { return optionValues.indexOf(value) >= 0; });
        if (!isMeasurementSelect) return;

        var name = select.getAttribute('data-name') || '';
        if (!name) {
          var row = select.closest('tr');
          var cells = row ? row.querySelectorAll('td') : [];
          if (cells.length > 1) name = (cells[1].textContent || '').trim();
        }
        if (!name) return;
        measurements[name] = select.value || '';
        window.easyflowMeasurements = window.easyflowMeasurements || {};
        window.easyflowMeasurements[name] = select.value || '';
      }

      function collectEasyflowMeasurementsFromPage() {
        var measurements = {};
        document.querySelectorAll('select.measurement-select, select[id^="measurement_input_"]').forEach(function(select) {
          rememberEasyflowMeasurement(select, measurements);
        });
        return measurements;
      }

      function submitEasyflowTableState() {
        var state = null;
        if (window.easyflowCurrentTableState) {
          try {
            state = window.easyflowCurrentTableState();
          } catch (e) {
            state = null;
          }
        }
        if (!state) state = collectEasyflowTableState();
        state.selected = state.selected || [];
        var pageMeasurements = collectEasyflowMeasurementsFromPage();
        state.measurements = Object.assign({}, state.measurements || {}, window.easyflowMeasurements || {}, pageMeasurements);
        state.measurement_pairs = Object.keys(state.measurements || {}).map(function(name) {
          return {name: name, value: state.measurements[name]};
        });
        state.var_labels = Object.assign({}, window.easyflowVarLabels || {}, state.var_labels || {}, captureEasyflowVarLabels());
        state.debug_measurement_count = Object.keys(state.measurements || {}).length;
        return state;
      }
      window.easyflowSubmitTableState = submitEasyflowTableState;

      window.easyflowApplyVariableSelection = function() {
        if (!window.Shiny) return false;
        flushEasyflowInputs();
        var state = submitEasyflowTableState();
        state.nonce = Date.now() + Math.random();
        Shiny.setInputValue('apply_variable_request', state, {priority: 'event'});
        return false;
      };

      window.easyflowApplyRoleSelection = function() {
        if (!window.Shiny) return false;
        flushEasyflowInputs();
        var state = submitEasyflowTableState();
        state.nonce = Date.now() + Math.random();
        Shiny.setInputValue('apply_role_request', state, {priority: 'event'});
        return false;
      };

      window.easyflowSelectRole = function(role) {
        if (!window.Shiny) return true;
        flushEasyflowInputs();
        var state = submitEasyflowTableState();
        state.role = role || '';
        state.nonce = Date.now() + Math.random();
        Shiny.setInputValue('role_switch_request', state, {priority: 'event'});
        return false;
      };

      window.easyflowFlushVariableTableState = function() {
        if (!window.Shiny) return true;
        flushEasyflowInputs();
        var state = submitEasyflowTableState();
        Shiny.setInputValue('variable_table_state', {
          selected: state.selected,
          measurements: state.measurements || {},
          measurement_pairs: state.measurement_pairs || [],
          var_labels: state.var_labels || {},
          debug_measurement_count: state.debug_measurement_count || 0,
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
        return true;
      };

      function flushEasyflowInputs() {
        captureEasyflowVarLabels();
        document.querySelectorAll('input.category-label-input, input.var-label-input').forEach(function(input) {
          input.dispatchEvent(new Event('change', {bubbles: true}));
        });
        if (window.Shiny) {
          Shiny.setInputValue('var_label_snapshot', {
            values: window.easyflowVarLabels || {},
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }

      function registerEasyflowDirtyHandler() {
        if (!window.Shiny || window.easyflowDirtyHandlerRegistered) return;
        window.easyflowDirtyHandlerRegistered = true;
        Shiny.addCustomMessageHandler('easyflow-settings-dirty', function(value) {
          window.easyflowSettingsDirty = !!value;
        });
      }
      registerEasyflowDirtyHandler();
      document.addEventListener('shiny:connected', registerEasyflowDirtyHandler);
      window.setTimeout(registerEasyflowDirtyHandler, 0);

      window.addEventListener('beforeunload', function(event) {
        if (!window.easyflowSettingsDirty) return;
        event.preventDefault();
        event.returnValue = '';
      });

      window.addEventListener('error', function(event) {
        if (!window.Shiny) return;
        Shiny.setInputValue('client_js_error', {
          message: event.message || '',
          source: event.filename || '',
          line: event.lineno || '',
          column: event.colno || '',
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });

      window.addEventListener('unhandledrejection', function(event) {
        if (!window.Shiny) return;
        Shiny.setInputValue('client_js_error', {
          message: String(event.reason || ''),
          source: 'unhandledrejection',
          line: '',
          column: '',
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });

      document.addEventListener('click', function(event) {
        if (event.target.closest('.navbar-nav a')) {
          flushEasyflowInputs();
          if (window.Shiny) {
            Shiny.setInputValue('nav_flush_request', {
              var_labels: captureEasyflowVarLabels(),
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
          }
        }
      }, true);

      var easyflowTransferLastIndexByInput = {};
      var easyflowActiveTransferListbox = null;

      function easyflowTransferOptions(listbox) {
        return Array.prototype.slice.call(listbox.querySelectorAll('.analysis-transfer-option'));
      }

      function easyflowTransferSelectedValues(listbox) {
        return easyflowTransferOptions(listbox)
          .filter(function(option) { return option.classList.contains('is-selected'); })
          .map(function(option) { return option.getAttribute('data-value'); });
      }

      function easyflowTransferSetSelected(option, selected) {
        option.classList.toggle('is-selected', selected);
        option.setAttribute('aria-selected', selected ? 'true' : 'false');
      }

      function easyflowTransferClear(listbox) {
        easyflowTransferOptions(listbox).forEach(function(option) {
          easyflowTransferSetSelected(option, false);
        });
      }

      function easyflowTransferSync(listbox) {
        var inputId = listbox.getAttribute('data-input-id');
        var select = inputId ? document.getElementById(inputId) : null;
        var values = easyflowTransferSelectedValues(listbox);
        if (select) {
          Array.prototype.forEach.call(select.options, function(option) {
            option.selected = values.indexOf(option.value) >= 0;
          });
        }
        if (window.Shiny && inputId) {
          Shiny.setInputValue(inputId, values, {priority: 'event'});
        }
      }

      function easyflowTransferMarkActive(listbox) {
        var inputId = listbox ? listbox.getAttribute('data-input-id') : null;
        if (listbox) easyflowActiveTransferListbox = listbox;
        if (window.Shiny && inputId) {
          Shiny.setInputValue(inputId + '_active', Date.now() + Math.random(), {priority: 'event'});
        }
      }

      function easyflowTransferFocusListbox(listbox) {
        if (!listbox) return;
        easyflowTransferMarkActive(listbox);
        if (listbox.focus) {
          try {
            listbox.focus({preventScroll: true});
          } catch (error) {
            listbox.focus();
          }
        }
      }

      window.easyflowTransferOptionClick = function(event, option) {
        var listbox = option && option.closest ? option.closest('.analysis-transfer-listbox') : null;
        if (!listbox) return;
        easyflowTransferFocusListbox(listbox);
        var inputId = listbox.getAttribute('data-input-id') || '';
        var options = easyflowTransferOptions(listbox);
        var index = options.indexOf(option);
        var storedLastIndex = inputId && Object.prototype.hasOwnProperty.call(easyflowTransferLastIndexByInput, inputId)
          ? easyflowTransferLastIndexByInput[inputId]
          : listbox.getAttribute('data-last-index');
        var lastIndex = parseInt(storedLastIndex || '-1', 10);

        if (event.shiftKey && lastIndex >= 0) {
          var start = Math.min(lastIndex, index);
          var end = Math.max(lastIndex, index);
          if (!event.ctrlKey && !event.metaKey) easyflowTransferClear(listbox);
          options.slice(start, end + 1).forEach(function(item) {
            easyflowTransferSetSelected(item, true);
          });
        } else if (event.ctrlKey || event.metaKey) {
          easyflowTransferSetSelected(option, !option.classList.contains('is-selected'));
        } else {
          easyflowTransferClear(listbox);
          easyflowTransferSetSelected(option, true);
        }
        listbox.setAttribute('data-last-index', String(index));
        if (inputId) easyflowTransferLastIndexByInput[inputId] = index;
        easyflowTransferSync(listbox);
      };

      ['mousedown', 'focusin'].forEach(function(eventName) {
        document.addEventListener(eventName, function(event) {
          var listbox = event.target && event.target.closest ? event.target.closest('.analysis-transfer-listbox') : null;
          if (listbox) easyflowTransferMarkActive(listbox);
        }, true);
      });

      document.addEventListener('keydown', function(event) {
        var isSelectAll = (event.ctrlKey || event.metaKey) &&
          ((event.key || '').toLowerCase() === 'a' || event.code === 'KeyA');
        if (!isSelectAll) return;

        var targetListbox = event.target && event.target.closest ? event.target.closest('.analysis-transfer-listbox') : null;
        var activeListbox = easyflowActiveTransferListbox && document.body.contains(easyflowActiveTransferListbox)
          ? easyflowActiveTransferListbox
          : null;
        var listbox = targetListbox || activeListbox;
        if (!listbox) return;

        var options = easyflowTransferOptions(listbox);
        if (options.length === 0) return;

        event.preventDefault();
        event.stopPropagation();
        if (event.stopImmediatePropagation) event.stopImmediatePropagation();

        var allSelected = options.every(function(option) {
          return option.classList.contains('is-selected');
        });
        options.forEach(function(option) {
          easyflowTransferSetSelected(option, !allSelected);
        });
        easyflowTransferFocusListbox(listbox);
        easyflowTransferSync(listbox);
      }, true);

      function easyflowUpdateMoveButtonClasses() {
        document.querySelectorAll('.analysis-move-button').forEach(function(button) {
          var label = (button.textContent || '').trim();
          button.classList.toggle('analysis-move-forward', label === '>');
          button.classList.toggle('analysis-move-back', label === '<');
        });
      }

      function easyflowStartMoveButtonObserver() {
        easyflowUpdateMoveButtonClasses();
        if (!window.MutationObserver || !document.body) return;
        var observer = new MutationObserver(easyflowUpdateMoveButtonClasses);
        observer.observe(document.body, {
          childList: true,
          subtree: true,
          characterData: true
        });
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', easyflowStartMoveButtonObserver);
      } else {
        easyflowStartMoveButtonObserver();
      }

      document.addEventListener('input', function(event) {
        storeEasyflowVarLabelInput(event.target);
      }, true);
      ['change', 'focusout', 'blur'].forEach(function(eventName) {
        document.addEventListener(eventName, function(event) {
          saveEasyflowVarLabelInput(event.target);
        }, true);
      });

      document.addEventListener('change', function(event) {
        var select = event.target && event.target.matches && event.target.matches('select')
          ? event.target
          : null;
        if (!select) return;
        var optionValues = Array.prototype.slice.call(select.options || []).map(function(option) { return option.value; });
        var isMeasurementSelect = select.classList.contains('measurement-select') ||
          (select.id || '').indexOf('measurement_input_') === 0 ||
          ['binary', 'category', 'ordered', 'continuous'].every(function(value) { return optionValues.indexOf(value) >= 0; });
        if (!isMeasurementSelect) return;

        var name = select.getAttribute('data-name') || '';
        if (!name) {
          var row = select.closest('tr');
          var cells = row ? row.querySelectorAll('td') : [];
          if (cells.length > 1) {
            name = (cells[1].textContent || '').trim();
          }
        }
        if (!name) return;
        window.easyflowMeasurements = window.easyflowMeasurements || {};
        window.easyflowMeasurements[name] = select.value || '';
        if (window.Shiny) {
          Shiny.setInputValue('variable_measurement_update', {
            name: name,
            value: window.easyflowMeasurements[name],
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }, true);

      document.addEventListener('click', function(event) {
        var button = event.target.closest('.settings-save-button');
        if (!button || !window.Shiny) return;
        event.preventDefault();
        event.stopImmediatePropagation();

        flushEasyflowInputs();

        var state = submitEasyflowTableState();
        var varLabels = Object.assign({}, window.easyflowVarLabels || {}, state.var_labels || {}, captureEasyflowVarLabels());
        var categoryLabels = {};
        document.querySelectorAll('input.category-label-input').forEach(function(input) {
          var name = input.getAttribute('data-name');
          var field = input.getAttribute('data-field');
          if (!name || !field) return;
          categoryLabels[name] = categoryLabels[name] || {};
          categoryLabels[name][field] = input.value || '';
        });
        Shiny.setInputValue('save_settings_request', {
          selected: state.selected,
          measurements: state.measurements || {},
          measurement_pairs: state.measurement_pairs || [],
          var_labels: varLabels,
          category_labels: categoryLabels,
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      }, true);
