      window.easyflowSettingsDirty = false;
      window.easyflowVarLabels = window.easyflowVarLabels || {};
      window.easyflowMeasurements = window.easyflowMeasurements || {};
      window.easyflowCodingErrorFixValues = window.easyflowCodingErrorFixValues || {};

      window.easyflowRestoreCodingErrorFixInputs = function(root) {
        root = root || document;
        var values = window.easyflowCodingErrorFixValues || {};
        var inputs = root.querySelectorAll ? root.querySelectorAll('.coding-error-fix-input') : [];
        Array.prototype.forEach.call(inputs, function(input) {
          var index = input.getAttribute('data-coding-error-index');
          if (index && Object.prototype.hasOwnProperty.call(values, index)) {
            input.value = values[index];
          }
        });
      };

      function isEasyflowVisibleElement(element) {
        if (!element || !element.getClientRects) return false;
        if (element.closest && element.closest('[style*="display:none"], [style*="display: none"]')) return false;
        return element.getClientRects().length > 0;
      }
      window.isEasyflowVisibleElement = isEasyflowVisibleElement;

      window.easyflowTransferFallbackSync = function(listbox) {
        if (!listbox || !listbox.getAttribute) return;
        var inputId = listbox.getAttribute('data-input-id') || '';
        var values = Array.prototype.slice.call(listbox.querySelectorAll('.analysis-transfer-option.is-selected'))
          .map(function(option) { return option.getAttribute('data-value') || ''; })
          .filter(function(value) { return value !== ''; });
        var select = inputId ? document.getElementById(inputId) : null;
        if (select) {
          Array.prototype.forEach.call(select.options, function(option) {
            option.selected = values.indexOf(option.value) >= 0;
          });
          if (window.jQuery) {
            window.jQuery(select).trigger('change');
          } else {
            select.dispatchEvent(new Event('change', {bubbles: true}));
          }
        }
        if (window.Shiny && inputId) {
          Shiny.setInputValue(inputId, values, {priority: 'event'});
          Shiny.setInputValue(inputId + '_active', Date.now() + Math.random(), {priority: 'event'});
        }
      };

      window.easyflowTransferFallbackFocus = function(listbox) {
        if (!listbox) return;
        window.easyflowFallbackActiveTransferListbox = listbox;
        if (listbox.focus) {
          try {
            listbox.focus({preventScroll: true});
          } catch (error) {
            listbox.focus();
          }
        }
      };

      window.easyflowTransferOptionClickFallback = function(event, option) {
        var listbox = option && option.closest ? option.closest('.analysis-transfer-listbox') : null;
        if (!listbox) return;
        window.easyflowTransferFallbackFocus(listbox);
        var options = Array.prototype.slice.call(listbox.querySelectorAll('.analysis-transfer-option'));
        var index = options.indexOf(option);
        var lastIndex = parseInt(listbox.getAttribute('data-last-index') || '-1', 10);
        if (Number.isNaN(lastIndex)) lastIndex = -1;

        if (event && event.shiftKey && lastIndex >= 0) {
          var start = Math.min(lastIndex, index);
          var end = Math.max(lastIndex, index);
          if (!event.ctrlKey && !event.metaKey) {
            options.forEach(function(item) {
              item.classList.remove('is-selected');
              item.setAttribute('aria-selected', 'false');
            });
          }
          options.slice(start, end + 1).forEach(function(item) {
            item.classList.add('is-selected');
            item.setAttribute('aria-selected', 'true');
          });
        } else if (event && (event.ctrlKey || event.metaKey)) {
          var selected = !option.classList.contains('is-selected');
          option.classList.toggle('is-selected', selected);
          option.setAttribute('aria-selected', selected ? 'true' : 'false');
        } else {
          options.forEach(function(item) {
            item.classList.remove('is-selected');
            item.setAttribute('aria-selected', 'false');
          });
          option.classList.add('is-selected');
          option.setAttribute('aria-selected', 'true');
        }
        listbox.setAttribute('data-last-index', String(index));
        window.easyflowTransferFallbackSync(listbox);
      };

      window.easyflowTransferListboxKeydownFallback = function(event, listbox) {
        var isSelectAll = (event.ctrlKey || event.metaKey) &&
          ((event.key || '').toLowerCase() === 'a' || event.code === 'KeyA');
        if (!isSelectAll || !listbox) return true;
        event.preventDefault();
        event.stopPropagation();
        if (event.stopImmediatePropagation) event.stopImmediatePropagation();
        var options = Array.prototype.slice.call(listbox.querySelectorAll('.analysis-transfer-option'));
        var allSelected = options.length > 0 && options.every(function(option) {
          return option.classList.contains('is-selected');
        });
        options.forEach(function(option) {
          option.classList.toggle('is-selected', !allSelected);
          option.setAttribute('aria-selected', !allSelected ? 'true' : 'false');
        });
        window.easyflowTransferFallbackSync(listbox);
        return false;
      };

      document.addEventListener('keydown', function(event) {
        var isSelectAll = (event.ctrlKey || event.metaKey) &&
          ((event.key || '').toLowerCase() === 'a' || event.code === 'KeyA');
        if (!isSelectAll) return;
        var listbox = event.target && event.target.closest
          ? event.target.closest('.analysis-transfer-listbox')
          : null;
        if (!listbox && window.easyflowFallbackActiveTransferListbox && document.body.contains(window.easyflowFallbackActiveTransferListbox)) {
          listbox = window.easyflowFallbackActiveTransferListbox;
        }
        if (!listbox || !listbox.querySelector || !listbox.querySelector('.analysis-transfer-option')) return;
        window.easyflowTransferListboxKeydownFallback(event, listbox);
      }, true);

      function registerEasyflowNestedDropdownMenus() {
        if (!window.jQuery) return;
        function configureNestedDropdownToggles() {
          window.jQuery('.dropdown-menu .dropdown-toggle')
            .removeAttr('data-toggle')
            .removeAttr('data-bs-toggle')
            .attr('aria-haspopup', 'true');
        }

        configureNestedDropdownToggles();
        window.jQuery(configureNestedDropdownToggles);
        document.addEventListener('DOMContentLoaded', configureNestedDropdownToggles);
        document.addEventListener('shiny:connected', configureNestedDropdownToggles);
        window.setTimeout(configureNestedDropdownToggles, 0);

        if (window.easyflowNestedDropdownRegistered) return;
        window.easyflowNestedDropdownRegistered = true;

        window.jQuery(document)
          .on('click.easyflowNestedDropdown', '.dropdown-menu .dropdown-toggle', function(event) {
            event.preventDefault();
            event.stopPropagation();
            var item = window.jQuery(this).parent();
            item.toggleClass('open');
            item.siblings('.dropdown.open').removeClass('open');
          })
          .on('hidden.bs.dropdown.easyflowNestedDropdown', '.navbar-nav > .dropdown', function() {
            window.jQuery(this).find('.dropdown.open').removeClass('open');
          });
      }

      registerEasyflowNestedDropdownMenus();
      document.addEventListener('shiny:connected', registerEasyflowNestedDropdownMenus);
      window.setTimeout(registerEasyflowNestedDropdownMenus, 0);

      function registerEasyflowCodingErrorFixHandler() {
        if (!window.Shiny || !Shiny.addCustomMessageHandler || window.easyflowCodingErrorFixHandlerRegistered) {
          return;
        }
        window.easyflowCodingErrorFixHandlerRegistered = true;
        Shiny.addCustomMessageHandler('easyflow-clear-coding-error-fixes', function() {
          window.easyflowCodingErrorFixValues = {};
        });
      }

      registerEasyflowCodingErrorFixHandler();
      document.addEventListener('shiny:connected', registerEasyflowCodingErrorFixHandler);

      function clearEasyflowClientDataSession() {
        window.easyflowVarLabels = {};
        window.easyflowMeasurements = {};
        window.easyflowSelectedNames = {};
        window.easyflowCodingErrorFixValues = {};
        window.easyflowCurrentTableState = null;
      }

      function registerEasyflowDataSessionHandler() {
        if (!window.Shiny || !Shiny.addCustomMessageHandler || window.easyflowDataSessionHandlerRegistered) {
          return;
        }
        window.easyflowDataSessionHandlerRegistered = true;
        Shiny.addCustomMessageHandler('easyflow-clear-data-session', clearEasyflowClientDataSession);
      }

      registerEasyflowDataSessionHandler();
      document.addEventListener('shiny:connected', registerEasyflowDataSessionHandler);

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
        document.querySelectorAll(selectors.join(',')).forEach(function(input) {
          if (isEasyflowVisibleElement(input)) collectInput(input);
        });

        document.querySelectorAll('table.dataTable').forEach(function(table) {
          if (!isEasyflowVisibleElement(table)) return;
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
          if (!isEasyflowVisibleElement(select)) return;
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
        if (!isEasyflowVisibleElement(select)) return;
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
        document.querySelectorAll('select.measurement-select, select.category-measurement-select, select[id^="measurement_input_"]').forEach(function(select) {
          rememberEasyflowMeasurement(select, measurements);
        });
        return measurements;
      }

      function submitEasyflowTableState() {
        var state = null;
        var variableTable = document.getElementById('variable_table');
        if (window.easyflowCurrentTableState && isEasyflowVisibleElement(variableTable)) {
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

      function collectEasyflowCategoryLabelState() {
        var root = document.getElementById('category_label_table') || document;
        var categoryLabels = {};
        var varLabels = {};
        root.querySelectorAll('input[data-name][data-field]').forEach(function(input) {
          var name = input.getAttribute('data-name');
          var field = input.getAttribute('data-field');
          if (!name || !field || input.disabled) return;
          categoryLabels[name] = categoryLabels[name] || {};
          categoryLabels[name][field] = input.value || '';
          if (field === 'var_label') {
            varLabels[name] = input.value || '';
          }
        });

        var measurements = {};
        root.querySelectorAll('select.category-measurement-select[data-name]').forEach(function(select) {
          var name = select.getAttribute('data-name') || '';
          if (!name) return;
          measurements[name] = select.value || '';
          window.easyflowMeasurements = window.easyflowMeasurements || {};
          window.easyflowMeasurements[name] = select.value || '';
        });

        return {
          category_labels: categoryLabels,
          var_labels: varLabels,
          var_label_pairs: Object.keys(varLabels).map(function(name) {
            return {name: name, value: varLabels[name]};
          }),
          measurements: measurements,
          measurement_pairs: Object.keys(measurements).map(function(name) {
            return {name: name, value: measurements[name]};
          })
        };
      }

      window.easyflowFlushCategoryLabelState = function() {
        flushEasyflowInputs();
        return true;
      };

      window.easyflowApplyCategoryLabels = function() {
        if (!window.Shiny) return false;
        var state = collectEasyflowCategoryLabelState();
        state.nonce = Date.now() + Math.random();
        Shiny.setInputValue('apply_category_labels_request', state, {priority: 'event'});
        Shiny.setInputValue('variable_measurement_snapshot', {
          values: state.measurements || {},
          measurement_pairs: state.measurement_pairs || [],
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
        return false;
      };

      document.addEventListener('click', function(event) {
        var button = event.target && event.target.closest ? event.target.closest('#apply_category_labels_button') : null;
        if (!button) return;
        event.preventDefault();
        event.stopPropagation();
        if (event.stopImmediatePropagation) event.stopImmediatePropagation();
        window.easyflowApplyCategoryLabels();
      }, true);

      function flushEasyflowInputs() {
        captureEasyflowVarLabels();
        collectEasyflowMeasurementsFromPage();
        document.querySelectorAll('input.category-label-input, input.var-label-input').forEach(function(input) {
          if (!isEasyflowVisibleElement(input)) return;
          input.dispatchEvent(new Event('change', {bubbles: true}));
        });
        if (window.Shiny) {
          Shiny.setInputValue('var_label_snapshot', {
            values: window.easyflowVarLabels || {},
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }

      document.addEventListener('change', function(event) {
        var select = event.target && event.target.closest ? event.target.closest('select.category-measurement-select') : null;
        if (!select) return;
        var measurements = {};
        rememberEasyflowMeasurement(select, measurements);
        if (window.Shiny) {
          Shiny.setInputValue('variable_measurement_update', {
            name: select.getAttribute('data-name') || '',
            value: select.value || '',
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }, true);

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
        var navLink = event.target && event.target.closest ? event.target.closest('.navbar-nav a') : null;
        if (navLink) {
          var activeTopLink = document.querySelector('.navbar-nav > li.active > a');
          var activeTopValue = activeTopLink ? activeTopLink.getAttribute('data-value') : '';
          var targetValue = navLink.getAttribute('data-value') || '';
          if (activeTopValue !== 'Data' || targetValue === 'Data' || navLink.classList.contains('dropdown-toggle') || navLink.closest('.dropdown-menu')) {
            return;
          }
          flushEasyflowInputs();
          var state = submitEasyflowTableState();
          if (window.Shiny) {
            Shiny.setInputValue('nav_flush_request', {
              measurements: state.measurements || {},
              measurement_pairs: state.measurement_pairs || [],
              var_labels: Object.assign({}, window.easyflowVarLabels || {}, state.var_labels || {}, captureEasyflowVarLabels()),
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
          }
        }
      }, true);

      var easyflowTransferLastIndexByInput = {};
      var easyflowActiveTransferListbox = null;
      var easyflowActiveTransferInputId = null;

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
          if (window.jQuery) {
            window.jQuery(select).trigger('change');
          } else {
            select.dispatchEvent(new Event('change', {bubbles: true}));
          }
        }
        if (window.Shiny && inputId) {
          Shiny.setInputValue(inputId, values, {priority: 'event'});
        }
      }

      function easyflowTransferMarkActive(listbox) {
        var inputId = listbox ? listbox.getAttribute('data-input-id') : null;
        if (listbox) easyflowActiveTransferListbox = listbox;
        if (inputId) easyflowActiveTransferInputId = inputId;
        if (window.Shiny && inputId) {
          Shiny.setInputValue(inputId + '_active', Date.now() + Math.random(), {priority: 'event'});
        }
      }

      function easyflowFindTransferListboxByInputId(inputId) {
        if (!inputId) return null;
        var listboxes = document.querySelectorAll('.analysis-transfer-listbox');
        for (var i = 0; i < listboxes.length; i += 1) {
          if (listboxes[i].getAttribute('data-input-id') === inputId) return listboxes[i];
        }
        return null;
      }

      function easyflowResolveActiveTransferListbox(event) {
        var targetListbox = event && event.target && event.target.closest
          ? event.target.closest('.analysis-transfer-listbox')
          : null;
        if (targetListbox) return targetListbox;

        var focusedListbox = document.activeElement && document.activeElement.closest
          ? document.activeElement.closest('.analysis-transfer-listbox')
          : null;
        if (focusedListbox) return focusedListbox;

        if (easyflowActiveTransferListbox && document.body.contains(easyflowActiveTransferListbox)) {
          return easyflowActiveTransferListbox;
        }

        return easyflowFindTransferListboxByInputId(easyflowActiveTransferInputId);
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
        var lastIndex = (storedLastIndex === undefined || storedLastIndex === null || storedLastIndex === '')
          ? -1
          : parseInt(storedLastIndex, 10);
        if (Number.isNaN(lastIndex)) lastIndex = -1;

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

      window.easyflowTransferOptionDoubleClick = function(event, option) {
        var listbox = option && option.closest ? option.closest('.analysis-transfer-listbox') : null;
        if (!listbox) return;
        if (event) {
          event.preventDefault();
          event.stopPropagation();
        }
        easyflowTransferFocusListbox(listbox);
        easyflowTransferClear(listbox);
        easyflowTransferSetSelected(option, true);
        easyflowTransferSync(listbox);

        var inputId = listbox.getAttribute('data-input-id') || '';
        var value = option.getAttribute('data-value') || '';
        if (window.Shiny && inputId && value) {
          Shiny.setInputValue(inputId + '_doubleclick', {
            value: value,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      };

      ['mousedown', 'focusin'].forEach(function(eventName) {
        document.addEventListener(eventName, function(event) {
          var listbox = event.target && event.target.closest ? event.target.closest('.analysis-transfer-listbox') : null;
          if (listbox) easyflowTransferMarkActive(listbox);
        }, true);
      });

      document.addEventListener('pointerover', function(event) {
        var listbox = event.target && event.target.closest ? event.target.closest('.analysis-transfer-listbox') : null;
        if (listbox) {
          easyflowActiveTransferListbox = listbox;
          easyflowActiveTransferInputId = listbox.getAttribute('data-input-id') || easyflowActiveTransferInputId;
        }
      }, true);

      function easyflowTransferHandleSelectAll(event, listbox) {
        var isSelectAll = (event.ctrlKey || event.metaKey) &&
          ((event.key || '').toLowerCase() === 'a' || event.code === 'KeyA');
        if (!isSelectAll || !listbox) return true;

        if (!document.body.contains(listbox)) return true;

        var options = easyflowTransferOptions(listbox);
        event.preventDefault();
        event.stopPropagation();
        if (event.stopImmediatePropagation) event.stopImmediatePropagation();
        if (options.length === 0) return false;

        var allSelected = options.every(function(option) {
          return option.classList.contains('is-selected');
        });
        options.forEach(function(option) {
          easyflowTransferSetSelected(option, !allSelected);
        });
        easyflowTransferFocusListbox(listbox);
        easyflowTransferSync(listbox);
        return false;
      }

      window.easyflowTransferListboxKeydown = function(event, listbox) {
        easyflowTransferMarkActive(listbox);
        return easyflowTransferHandleSelectAll(event, listbox);
      };

      function easyflowTransferSelectAllListener(event) {
        var isSelectAll = (event.ctrlKey || event.metaKey) &&
          ((event.key || '').toLowerCase() === 'a' || event.code === 'KeyA');
        if (!isSelectAll) return;

        var listbox = easyflowResolveActiveTransferListbox(event);
        if (listbox) {
          easyflowTransferHandleSelectAll(event, listbox);
        }
      }

      window.addEventListener('keydown', easyflowTransferSelectAllListener, true);
      document.addEventListener('keydown', easyflowTransferSelectAllListener, true);

      function easyflowRegisterClearTransferSelectionHandler() {
        if (!window.Shiny || !Shiny.addCustomMessageHandler || window.easyflowClearTransferSelectionHandlerRegistered) {
          return;
        }
        window.easyflowClearTransferSelectionHandlerRegistered = true;
        Shiny.addCustomMessageHandler('easyflow-clear-transfer-selection', function(message) {
          var inputIds = [];
          if (message && Array.isArray(message.inputIds)) {
            inputIds = message.inputIds;
          } else if (message && message.inputId) {
            inputIds = [message.inputId];
          }

          inputIds.forEach(function(inputId) {
            var listbox = easyflowFindTransferListboxByInputId(inputId);
            if (listbox) {
              easyflowTransferClear(listbox);
              delete easyflowTransferLastIndexByInput[inputId];
              listbox.removeAttribute('data-last-index');
            }
            if (window.Shiny && inputId) {
              Shiny.setInputValue(inputId, [], {priority: 'event'});
            }
          });
        });
      }
      easyflowRegisterClearTransferSelectionHandler();
      document.addEventListener('shiny:connected', easyflowRegisterClearTransferSelectionHandler);

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

      (function() {
        function selectedRadioValue(name) {
          var checked = document.querySelector('input[name="' + name + '"]:checked');
          return checked ? checked.value : null;
        }

        function setTtestNormalityDisabled(selector, disabled) {
          document.querySelectorAll(selector).forEach(function(container) {
            container.classList.toggle('ttest-normality-disabled', disabled);
            container.setAttribute('aria-disabled', disabled ? 'true' : 'false');
            container.querySelectorAll('input').forEach(function(input) {
              input.disabled = disabled;
            });
          });
        }

        function setRadioValue(name, value) {
          var input = document.querySelector('input[name="' + name + '"][value="' + value + '"]');
          if (!input || input.checked) return;
          input.checked = true;
          input.dispatchEvent(new Event('change', {bubbles: true}));
          if (window.Shiny) {
            Shiny.setInputValue(name, value, {priority: 'event'});
          }
        }

        function resetTtestNormalityDefaults() {
          setRadioValue('ttest_anova_normality_study_type', 'survey');
          setRadioValue('ttest_anova_survey_normality_method', 'skew_kurtosis');
        }

        function updateTtestNormalityTree() {
          var normality = document.getElementById('ttest_anova_normality_enabled');
          if (!normality) return;
          var enabled = normality.checked;
          var studyType = selectedRadioValue('ttest_anova_normality_study_type') || 'survey';
          setTtestNormalityDisabled('.ttest-normality-study-options', !enabled);
          setTtestNormalityDisabled('.ttest-normality-survey-branch', !enabled || studyType !== 'survey');
          setTtestNormalityDisabled('.ttest-normality-experimental-branch', !enabled || studyType !== 'experimental');
        }

        function scheduleTtestNormalityTreeUpdate() {
          window.setTimeout(updateTtestNormalityTree, 0);
        }

        document.addEventListener('change', function(event) {
          var target = event.target;
          if (!target || !target.matches) return;
          if (
            target.matches('#ttest_anova_normality_enabled') ||
            target.matches('input[name="ttest_anova_normality_study_type"]')
          ) {
            if (target.matches('#ttest_anova_normality_enabled') && target.checked) {
              resetTtestNormalityDefaults();
            }
            scheduleTtestNormalityTreeUpdate();
          }
        }, true);

        document.addEventListener('shiny:value', scheduleTtestNormalityTreeUpdate);
        document.addEventListener('shiny:bound', scheduleTtestNormalityTreeUpdate);
        document.addEventListener('shiny:connected', scheduleTtestNormalityTreeUpdate);
        if (window.MutationObserver) {
          new MutationObserver(scheduleTtestNormalityTreeUpdate).observe(document.documentElement, {
            childList: true,
            subtree: true
          });
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', scheduleTtestNormalityTreeUpdate);
        } else {
          scheduleTtestNormalityTreeUpdate();
        }
        window.easyflowUpdateTtestNormalityTree = updateTtestNormalityTree;
      })();

      document.addEventListener('click', function(event) {
        var button = event.target && event.target.closest ? event.target.closest('.settings-save-button') : null;
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
