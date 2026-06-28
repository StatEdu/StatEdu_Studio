      window.easyflowSettingsDirty = false;
      window.easyflowVarLabels = window.easyflowVarLabels || {};
      window.easyflowMeasurements = window.easyflowMeasurements || {};
      window.easyflowCodingErrorFixValues = window.easyflowCodingErrorFixValues || {};
      window.easyflowTransferSelectionOrderByInput = window.easyflowTransferSelectionOrderByInput || {};

      function easyflowNormalizeLanguage(language) {
        var value = String(language || '').trim().toLowerCase();
        if (value === 'en' || value === 'english') return 'en';
        return 'ko';
      }

      function easyflowStoredLanguage() {
        try {
          var stored = window.localStorage.getItem('statedu_app_language') || '';
          return stored ? easyflowNormalizeLanguage(stored) : '';
        } catch (error) {
          return '';
        }
      }

      function easyflowHasUrlLanguage() {
        try {
          return !!(new URL(window.location.href).searchParams.get('lang') || '');
        } catch (error) {
          return false;
        }
      }

      function easyflowUrlLanguage() {
        var language = '';
        try {
          language = new URL(window.location.href).searchParams.get('lang') || '';
        } catch (error) {
          language = '';
        }
        if (language) return easyflowNormalizeLanguage(language);
        language = easyflowStoredLanguage();
        if (language) return language;
        var initial = document.getElementById('statedu_initial_language');
        if (initial && initial.value) return easyflowNormalizeLanguage(initial.value);
        return 'ko';
      }

      function easyflowSetCurrentLanguage(language) {
        language = easyflowNormalizeLanguage(language);
        window.easyflowAppLanguage = language;
        if (document.documentElement) {
          document.documentElement.setAttribute('lang', language);
        }
        return language;
      }

      function easyflowCurrentLanguage() {
        if (window.easyflowAppLanguage) {
          return easyflowNormalizeLanguage(window.easyflowAppLanguage);
        }
        var initial = document.getElementById('statedu_initial_language');
        if (initial && initial.value) {
          return easyflowSetCurrentLanguage(initial.value);
        }
        return easyflowSetCurrentLanguage(easyflowUrlLanguage());
      }

      function easyflowEnsureStoredLanguageUrl() {
        if (easyflowHasUrlLanguage()) return;
        var stored = easyflowStoredLanguage();
        if (!stored) return;
        var initial = document.getElementById('statedu_initial_language');
        var initialLanguage = initial && initial.value ? easyflowNormalizeLanguage(initial.value) : 'ko';
        if (stored !== initialLanguage) easyflowApplyLanguageValue(stored);
      }

      window.easyflowSendAppLanguage = function() {
        if (!easyflowCanSetInputValue()) return;
        Shiny.setInputValue('statedu_url_language', easyflowCurrentLanguage(), {priority: 'event'});
      };

      function easyflowScheduleAppLanguageSend() {
        [0, 100, 500, 1000].forEach(function(delay) {
          window.setTimeout(window.easyflowSendAppLanguage, delay);
        });
      }

      function easyflowApplyLanguageValue(language) {
        language = easyflowSetCurrentLanguage(language);
        var input = document.getElementById('app_language');
        if (input) {
          input.value = language;
          if (input.selectize && typeof input.selectize.setValue === 'function') {
            input.selectize.setValue(language, true);
          }
        }
        try {
          window.localStorage.setItem('statedu_app_language', language);
        } catch (error) {
          // Ignore storage failures; the query parameter is the active setting.
        }
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue('app_language', language, {priority: 'event'});
          Shiny.setInputValue('statedu_url_language', language, {priority: 'event'});
        }
        var href = window.location && window.location.href ? window.location.href : '';
        var url;
        try {
          url = new URL(href);
        } catch (error) {
          url = new URL('/', window.location.origin || 'http://127.0.0.1');
        }
        url.searchParams.set('lang', language);
        var nextHref = url.toString();
        if (nextHref === window.location.href) {
          window.location.reload();
        } else {
          window.location.replace(nextHref);
        }
        return false;
      }

      window.easyflowApplyAppLanguage = function() {
        var input = document.getElementById('app_language');
        var language = input && input.selectize && typeof input.selectize.getValue === 'function'
          ? input.selectize.getValue()
          : (input && input.value ? input.value : 'ko');
        return easyflowApplyLanguageValue(language);
      };

      easyflowSetCurrentLanguage(easyflowUrlLanguage());

      if (window.Shiny && typeof Shiny.addCustomMessageHandler === 'function') {
        window.easyflowLanguageHandlerBound = true;
        Shiny.addCustomMessageHandler('statedu-apply-language', easyflowApplyLanguageValue);
      } else {
        document.addEventListener('shiny:connected', function() {
          if (window.Shiny && typeof Shiny.addCustomMessageHandler === 'function' && !window.easyflowLanguageHandlerBound) {
            window.easyflowLanguageHandlerBound = true;
            Shiny.addCustomMessageHandler('statedu-apply-language', easyflowApplyLanguageValue);
          }
        });
      }

      function easyflowCanSetInputValue() {
        return !!(window.Shiny && typeof Shiny.setInputValue === 'function');
      }

      document.addEventListener('shiny:connected', easyflowScheduleAppLanguageSend);
      document.addEventListener('DOMContentLoaded', function() {
        easyflowEnsureStoredLanguageUrl();
        easyflowScheduleAppLanguageSend();
      });
      if (document.readyState !== 'loading') {
        easyflowEnsureStoredLanguageUrl();
        easyflowScheduleAppLanguageSend();
      }

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

      document.addEventListener('click', function(event) {
        if (!event.target || !event.target.closest) return;
        var disabledPairedTab = event.target.closest('.paired-options-disabled-tab');
        if (!disabledPairedTab) return;
        var tabLink = disabledPairedTab.closest('a');
        if (!tabLink) return;
        event.preventDefault();
        event.stopPropagation();
        if (event.stopImmediatePropagation) event.stopImmediatePropagation();
      }, true);

      function easyflowTransferOptionValues(options) {
        return Array.prototype.slice.call(options || [])
          .map(function(option) { return option.getAttribute('data-value') || ''; })
          .filter(function(value) { return value !== ''; });
      }

      document.addEventListener('click', function(event) {
        if (!event.target || !event.target.closest || !window.Shiny) return;
        var button = event.target.closest('#crosstab_assign_col, #crosstab_assign_row');
        if (!button) return;
        var label = (button.textContent || '').trim();
        var targetInputId = button.id === 'crosstab_assign_col' ? 'crosstab_col' : 'crosstab_row';
        var targetListbox = document.querySelector('.analysis-transfer-listbox[data-input-id="' + targetInputId + '"]');
        var selectedTargetOptions = targetListbox
          ? targetListbox.querySelectorAll('.analysis-transfer-option.is-selected')
          : [];
        if (label !== '<' && selectedTargetOptions.length === 0) return;
        event.preventDefault();
        event.stopPropagation();
        if (event.stopImmediatePropagation) event.stopImmediatePropagation();
        var values = easyflowTransferOptionValues(selectedTargetOptions);
        if (values.length === 0 && label === '<' && targetListbox) {
          values = easyflowTransferOptionValues(targetListbox.querySelectorAll('.analysis-transfer-option'));
        }
        Shiny.setInputValue(
          button.id === 'crosstab_assign_col' ? 'crosstab_assign_col_request' : 'crosstab_assign_row_request',
          {direction: 'back', values: values, nonce: Date.now() + Math.random()},
          {priority: 'event'}
        );
      }, true);

      function easyflowSkipsMathNode(node) {
        var parent = node && node.parentElement;
        while (parent) {
          var tagName = parent.tagName ? parent.tagName.toLowerCase() : '';
          if (tagName === 'script' || tagName === 'noscript' || tagName === 'style' ||
              tagName === 'textarea' || tagName === 'pre' || tagName === 'code' ||
              tagName === 'mjx-container') {
            return true;
          }
          if (parent.classList && parent.classList.contains('MathJax')) {
            return true;
          }
          parent = parent.parentElement;
        }
        return false;
      }

      async function easyflowReplaceMathInTextNode(node) {
        if (!node || !node.parentNode || !node.nodeValue) return false;
        var text = node.nodeValue;
        var pattern = /(\$\$([\s\S]+?)\$\$|\\\[([\s\S]+?)\\\]|\$([^$\n]+?)\$|\\\(([\s\S]+?)\\\))/g;
        var fragment = document.createDocumentFragment();
        var lastIndex = 0;
        var changed = false;
        var match;

        while ((match = pattern.exec(text)) !== null) {
          if (match.index > lastIndex) {
            fragment.appendChild(document.createTextNode(text.slice(lastIndex, match.index)));
          }

          var display = typeof match[2] !== 'undefined' || typeof match[3] !== 'undefined';
          var tex = (match[2] || match[3] || match[4] || match[5] || '').trim();
          if (!tex) {
            fragment.appendChild(document.createTextNode(match[0]));
          } else {
            try {
              var converter = window.MathJax.tex2svgPromise || window.MathJax.tex2chtmlPromise;
              fragment.appendChild(await converter.call(window.MathJax, tex, { display: display }));
              changed = true;
            } catch (error) {
              fragment.appendChild(document.createTextNode(match[0]));
              if (window.console && window.console.warn) {
                window.console.warn('MathJax conversion failed', tex, error);
              }
            }
          }

          lastIndex = pattern.lastIndex;
        }

        if (!changed) return false;
        if (lastIndex < text.length) {
          fragment.appendChild(document.createTextNode(text.slice(lastIndex)));
        }
        node.parentNode.replaceChild(fragment, node);
        return true;
      }

      function easyflowCollectMathTextNodes(root) {
        var nodes = [];
        var pattern = /(\$\$[\s\S]+?\$\$|\\\[[\s\S]+?\\\]|\$[^$\n]+?\$|\\\([\s\S]+?\\\))/;
        var walker = document.createTreeWalker(
          root,
          NodeFilter.SHOW_TEXT,
          {
            acceptNode: function(node) {
              if (!node.nodeValue || (node.nodeValue.indexOf('$') < 0 && node.nodeValue.indexOf('\\') < 0)) return NodeFilter.FILTER_REJECT;
              if (!pattern.test(node.nodeValue)) return NodeFilter.FILTER_REJECT;
              if (easyflowSkipsMathNode(node)) return NodeFilter.FILTER_REJECT;
              return NodeFilter.FILTER_ACCEPT;
            }
          }
        );
        var node;
        while ((node = walker.nextNode())) {
          nodes.push(node);
        }
        return nodes;
      }

      window.easyflowTypesetMath = function(root) {
        root = root || document;
        if (!root.querySelector || !root.querySelector('.about-markdown-document')) return;
        if (window.easyflowDecorateMeasurementTerms) {
          window.easyflowDecorateMeasurementTerms(root);
        }
        if (!window.MathJax || (!window.MathJax.tex2svgPromise && !window.MathJax.tex2chtmlPromise && !window.MathJax.typesetPromise)) {
          window.easyflowMathJaxPending = true;
          return;
        }
        if (!window.MathJax.tex2svgPromise && !window.MathJax.tex2chtmlPromise && window.MathJax.typesetPromise) {
          window.MathJax.typesetPromise([root]).catch(function(error) {
            if (window.console && window.console.warn) {
              window.console.warn('MathJax typeset failed', error);
            }
          });
          return;
        }
        if (window.easyflowMathJaxRendering) {
          window.easyflowMathJaxPending = true;
          return;
        }

        window.easyflowMathJaxRendering = true;
        (async function() {
          var documents = root.classList && root.classList.contains('about-markdown-document') ?
            [root] :
            Array.prototype.slice.call(root.querySelectorAll('.about-markdown-document'));
          for (var i = 0; i < documents.length; i += 1) {
            var nodes = easyflowCollectMathTextNodes(documents[i]);
            for (var j = 0; j < nodes.length; j += 1) {
              await easyflowReplaceMathInTextNode(nodes[j]);
            }
          }
          if (window.MathJax.startup && window.MathJax.startup.document &&
              window.MathJax.startup.document.updateDocument) {
            window.MathJax.startup.document.updateDocument();
          }
        })().finally(function() {
          window.easyflowMathJaxRendering = false;
          if (window.easyflowMathJaxPending) {
            window.easyflowMathJaxPending = false;
            scheduleEasyflowTypesetMath(root);
          }
        });
      };

      function scheduleEasyflowTypesetMath(root) {
        window.setTimeout(function() {
          window.easyflowTypesetMath(root || document);
        }, 0);
      }

      window.easyflowMathJaxReady = function() {
        window.easyflowMathJaxPending = false;
        scheduleEasyflowTypesetMath(document);
        window.setTimeout(function() { scheduleEasyflowTypesetMath(document); }, 250);
        window.setTimeout(function() { scheduleEasyflowTypesetMath(document); }, 1000);
      };

      window.easyflowStartMathJaxPolling = function() {
        if (window.easyflowMathJaxPollingStarted) return;
        window.easyflowMathJaxPollingStarted = true;
        var attempts = 0;
        var timer = window.setInterval(function() {
          attempts += 1;
          if (window.MathJax && (window.MathJax.tex2svgPromise || window.MathJax.tex2chtmlPromise || window.MathJax.typesetPromise)) {
            window.clearInterval(timer);
            window.easyflowMathJaxReady();
          } else if (attempts >= 80) {
            window.clearInterval(timer);
          }
        }, 250);
      };

      window.easyflowUpdateMeasurementControl = function(select) {
        if (!select) return;
        var value = String(select.value || '');
        if (value === 'ordinal') value = 'ordered';
        if (value === 'nominal') value = 'category';
        var label = value === 'ordered' ? 'ordinal' : value;
        var wrapper = select.closest ? select.closest('.measurement-control') : null;
        var symbol = wrapper && wrapper.querySelector ? wrapper.querySelector('.measurement-symbol') : null;
        if (!symbol) return;
        ['continuous', 'binary', 'category', 'ordered'].forEach(function(level) {
          symbol.classList.remove('measurement-' + level);
        });
        if (value) {
          symbol.classList.add('measurement-' + value);
        }
        symbol.setAttribute('title', label);
        symbol.setAttribute('aria-label', label);
      };

      window.easyflowRefreshMeasurementControls = function(root) {
        root = root || document;
        if (!root.querySelectorAll) return;
        root.querySelectorAll('.measurement-control select.measurement-select').forEach(function(select) {
          window.easyflowUpdateMeasurementControl(select);
        });
      };

      function easyflowMeasurementTermInfo(term) {
        var raw = String(term || '');
        var value = raw.toLowerCase();
        if (value === 'ordinal') value = 'ordered';
        if (value === 'nominal') value = 'category';
        if (['continuous', 'binary', 'category', 'ordered'].indexOf(value) < 0) return null;
        return {
          value: value,
          label: value === 'ordered' ? 'ordinal' : value,
          text: raw
        };
      }

      function easyflowMeasurementIconNode(info) {
        var symbol = document.createElement('span');
        symbol.className = 'measurement-symbol measurement-' + info.value;
        symbol.setAttribute('title', info.label);
        symbol.setAttribute('aria-label', info.label);
        return symbol;
      }

      function easyflowMeasurementTermNode(term, codeNode) {
        var info = easyflowMeasurementTermInfo(term);
        if (!info) return null;
        var wrapper = document.createElement('span');
        wrapper.className = 'measurement-term measurement-term-' + info.value;
        wrapper.appendChild(easyflowMeasurementIconNode(info));
        if (codeNode) {
          wrapper.appendChild(codeNode);
        } else {
          var text = document.createElement('span');
          text.className = 'measurement-term-text';
          text.textContent = info.text;
          wrapper.appendChild(text);
        }
        return wrapper;
      }

      function easyflowSkipMeasurementTermNode(node) {
        var parent = node && node.parentElement;
        while (parent) {
          var tagName = parent.tagName ? parent.tagName.toLowerCase() : '';
          if (tagName === 'script' || tagName === 'noscript' || tagName === 'style' ||
              tagName === 'textarea' || tagName === 'pre' || tagName === 'mjx-container') {
            return true;
          }
          if (parent.classList &&
              (parent.classList.contains('measurement-term') || parent.classList.contains('measurement-symbol'))) {
            return true;
          }
          parent = parent.parentElement;
        }
        return false;
      }

      window.easyflowDecorateMeasurementTerms = function(root) {
        root = root || document;
        if (!root.querySelectorAll) return;
        var documents = root.classList && root.classList.contains('about-markdown-document') ?
          [root] :
          Array.prototype.slice.call(root.querySelectorAll('.about-markdown-document'));
        var termPattern = /\b(continuous|binary|category|ordered|ordinal|nominal)\b/g;

        documents.forEach(function(doc) {
          doc.querySelectorAll('code').forEach(function(code) {
            if (!code.parentNode || code.closest('.measurement-term')) return;
            var text = (code.textContent || '').trim();
            if (!easyflowMeasurementTermInfo(text)) return;
            var clone = code.cloneNode(true);
            code.parentNode.replaceChild(easyflowMeasurementTermNode(text, clone), code);
          });

          var walker = document.createTreeWalker(
            doc,
            NodeFilter.SHOW_TEXT,
            {
              acceptNode: function(node) {
                if (!node.nodeValue || !termPattern.test(node.nodeValue)) {
                  termPattern.lastIndex = 0;
                  return NodeFilter.FILTER_REJECT;
                }
                termPattern.lastIndex = 0;
                if (easyflowSkipMeasurementTermNode(node)) return NodeFilter.FILTER_REJECT;
                return NodeFilter.FILTER_ACCEPT;
              }
            }
          );
          var nodes = [];
          var node;
          while ((node = walker.nextNode())) {
            nodes.push(node);
          }
          nodes.forEach(function(textNode) {
            var text = textNode.nodeValue;
            var fragment = document.createDocumentFragment();
            var lastIndex = 0;
            var match;
            termPattern.lastIndex = 0;
            while ((match = termPattern.exec(text)) !== null) {
              if (match.index > lastIndex) {
                fragment.appendChild(document.createTextNode(text.slice(lastIndex, match.index)));
              }
              fragment.appendChild(easyflowMeasurementTermNode(match[1]));
              lastIndex = termPattern.lastIndex;
            }
            if (lastIndex < text.length) {
              fragment.appendChild(document.createTextNode(text.slice(lastIndex)));
            }
            if (textNode.parentNode) {
              textNode.parentNode.replaceChild(fragment, textNode);
            }
          });
        });
      };

      window.easyflowTransferScrollTops = window.easyflowTransferScrollTops || {};
      window.easyflowTransferScrollAnchors = window.easyflowTransferScrollAnchors || {};
      window.easyflowTransferScrollRestoreUntil = window.easyflowTransferScrollRestoreUntil || 0;

      function easyflowVisibleTransferAnchor(listbox) {
        var options = Array.prototype.slice.call(listbox.querySelectorAll('.analysis-transfer-option'));
        var scrollTop = listbox.scrollTop || 0;
        for (var i = 0; i < options.length; i += 1) {
          var option = options[i];
          var optionTop = option.offsetTop || 0;
          var optionBottom = optionTop + (option.offsetHeight || 24);
          if (optionBottom > scrollTop) {
            return {
              value: option.getAttribute('data-value') || '',
              offset: optionTop - scrollTop
            };
          }
        }
        return null;
      }

      function easyflowRememberAllTransferScrolls() {
        document.querySelectorAll('.analysis-transfer-listbox[data-input-id]').forEach(function(listbox) {
          var inputId = listbox.getAttribute('data-input-id') || '';
          if (inputId) {
            window.easyflowTransferScrollTops[inputId] = listbox.scrollTop || 0;
            window.easyflowTransferScrollAnchors[inputId] = easyflowVisibleTransferAnchor(listbox);
          }
        });
        window.easyflowTransferScrollRestoreUntil = Date.now() + 1500;
      }
      window.easyflowRememberAllTransferScrolls = easyflowRememberAllTransferScrolls;

      function easyflowRestoreRememberedTransferScrolls() {
        if (Date.now() > (window.easyflowTransferScrollRestoreUntil || 0)) return;
        var values = window.easyflowTransferScrollTops || {};
        var anchors = window.easyflowTransferScrollAnchors || {};
        document.querySelectorAll('.analysis-transfer-listbox[data-input-id]').forEach(function(listbox) {
          var inputId = listbox.getAttribute('data-input-id') || '';
          if (!inputId) return;
          var anchor = anchors[inputId];
          if (anchor && anchor.value) {
            var options = Array.prototype.slice.call(listbox.querySelectorAll('.analysis-transfer-option'));
            var matched = options.find(function(option) {
              return (option.getAttribute('data-value') || '') === anchor.value;
            });
            if (matched) {
              listbox.scrollTop = Math.max(0, (matched.offsetTop || 0) - (anchor.offset || 0));
              return;
            }
          }
          if (Object.prototype.hasOwnProperty.call(values, inputId)) {
            listbox.scrollTop = Math.max(0, values[inputId] || 0);
          }
        });
      }

      function easyflowRestoreRememberedTransferScrollsSoon() {
        window.setTimeout(easyflowRestoreRememberedTransferScrolls, 0);
        window.setTimeout(easyflowRestoreRememberedTransferScrolls, 50);
        window.setTimeout(easyflowRestoreRememberedTransferScrolls, 150);
        window.setTimeout(easyflowRestoreRememberedTransferScrolls, 300);
        window.setTimeout(easyflowRestoreRememberedTransferScrolls, 600);
        window.setTimeout(easyflowRestoreRememberedTransferScrolls, 1000);
      }

      document.addEventListener('mousedown', function(event) {
        var button = event.target && event.target.closest
          ? event.target.closest('.analysis-move-button')
          : null;
        if (button) {
          document.querySelectorAll('.analysis-transfer-listbox[data-input-id]').forEach(function(listbox) {
            if (window.easyflowTransferFallbackSync) window.easyflowTransferFallbackSync(listbox);
          });
          easyflowRememberAllTransferScrolls();
        }
      }, true);

      document.addEventListener('click', function(event) {
        var button = event.target && event.target.closest
          ? event.target.closest('#paired_pair_move, #paired_rm_move, #nonparametric_paired_move')
          : null;
        if (!button || button.disabled) return;
        var inputMap = {
          paired_pair_move: 'paired_available',
          paired_rm_move: 'paired_rm_available',
          nonparametric_paired_move: 'nonparametric_paired_available'
        };
        var inputId = inputMap[button.id] || '';
        var availableListbox = inputId ? easyflowFindTransferListboxByInputId(inputId) : null;
        var values = availableListbox ? easyflowTransferSelectedValues(availableListbox) : [];
        if (values.length < 2) return;
        event.preventDefault();
        event.stopPropagation();
        if (event.stopImmediatePropagation) event.stopImmediatePropagation();
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(button.id + '_ordered', {
            values: values,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }, true);

      document.addEventListener('click', function(event) {
        var button = event.target && event.target.closest
          ? event.target.closest('#coding_error_move, #recode_different_move, #variable_calculation_move, #missing_values_move, #wide_long_move')
          : null;
        if (!button || button.disabled) return;
        var inputMap = {
          coding_error_move: ['coding_error_available', 'coding_error_selected'],
          recode_different_move: ['recode_different_available', 'recode_different_selected'],
          variable_calculation_move: ['variable_calculation_available', 'variable_calculation_selected'],
          missing_values_move: ['missing_values_available', 'missing_values_selected'],
          wide_long_move: ['wide_long_available', 'wide_long_selected']
        };
        var ids = inputMap[button.id] || [];
        var availableListbox = ids[0] ? document.querySelector('.analysis-transfer-listbox[data-input-id="' + ids[0] + '"]') : null;
        var selectedListbox = ids[1] ? document.querySelector('.analysis-transfer-listbox[data-input-id="' + ids[1] + '"]') : null;
        var selectedValues = selectedListbox ? easyflowTransferSelectedValues(selectedListbox) : [];
        var availableValues = availableListbox ? easyflowTransferSelectedValues(availableListbox) : [];
        var source = '';
        var values = [];
        if (selectedListbox && (easyflowActiveTransferListbox === selectedListbox || window.easyflowFallbackActiveTransferListbox === selectedListbox) && selectedValues.length > 0) {
          source = 'selected';
          values = selectedValues;
        } else if (availableValues.length > 0) {
          source = 'available';
          values = availableValues;
        } else if (selectedValues.length > 0) {
          source = 'selected';
          values = selectedValues;
        }
        if (!source || values.length === 0) return;
        event.preventDefault();
        event.stopPropagation();
        if (event.stopImmediatePropagation) event.stopImmediatePropagation();
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(button.id + '_direct', {
            source: source,
            values: values,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
      }, true);

      document.addEventListener('mousedown', function(event) {
        var button = event.target && event.target.closest
          ? event.target.closest('.variable-rename-apply-button')
          : null;
        if (button) easyflowRememberAllTransferScrolls();
      }, true);

      document.addEventListener('dblclick', function(event) {
        var option = event.target && event.target.closest
          ? event.target.closest('.analysis-transfer-option')
          : null;
        if (option) easyflowRememberAllTransferScrolls();
      }, true);

      document.addEventListener('shiny:bound', function() {
        easyflowRestoreRememberedTransferScrollsSoon();
      });

      document.addEventListener('shiny:value', function() {
        easyflowRestoreRememberedTransferScrollsSoon();
      });

      document.addEventListener('shiny:idle', function() {
        easyflowRestoreRememberedTransferScrollsSoon();
      });

      function easyflowRegisterTransferScrollRestoreObserver() {
        if (!window.MutationObserver || !document.body || window.easyflowTransferScrollRestoreObserver) return;
        window.easyflowTransferScrollRestoreObserver = new MutationObserver(function() {
          easyflowRestoreRememberedTransferScrollsSoon();
        });
        window.easyflowTransferScrollRestoreObserver.observe(document.body, {childList: true, subtree: true});
      }
      easyflowRegisterTransferScrollRestoreObserver();
      document.addEventListener('DOMContentLoaded', easyflowRegisterTransferScrollRestoreObserver);
      document.addEventListener('shiny:connected', easyflowRegisterTransferScrollRestoreObserver);
      window.setTimeout(easyflowRegisterTransferScrollRestoreObserver, 0);

      window.easyflowTransferFallbackSync = function(listbox) {
        if (!listbox || !listbox.getAttribute) return;
        var inputId = listbox.getAttribute('data-input-id') || '';
        var selectedOptions = Array.prototype.slice.call(listbox.querySelectorAll('.analysis-transfer-option.is-selected'));
        var values = selectedOptions
          .sort(function(a, b) {
            var aOrder = parseInt(a.getAttribute('data-selected-order') || '0', 10);
            var bOrder = parseInt(b.getAttribute('data-selected-order') || '0', 10);
            if (Number.isNaN(aOrder)) aOrder = 0;
            if (Number.isNaN(bOrder)) bOrder = 0;
            if (aOrder && bOrder && aOrder !== bOrder) return aOrder - bOrder;
            if (aOrder && !bOrder) return -1;
            if (!aOrder && bOrder) return 1;
            return 0;
          })
          .map(function(option) { return option.getAttribute('data-value') || ''; })
          .filter(function(value) { return value !== ''; });
        var hasExplicitOrder = selectedOptions.some(function(option) {
          var order = parseInt(option.getAttribute('data-selected-order') || '0', 10);
          return !Number.isNaN(order) && order > 0;
        });
        var storedOrder = inputId ? (window.easyflowTransferSelectionOrderByInput[inputId] || []) : [];
        if (!hasExplicitOrder && storedOrder.length > 0) {
          values = storedOrder
            .filter(function(value) { return values.indexOf(value) >= 0; })
            .concat(values.filter(function(value) { return storedOrder.indexOf(value) < 0; }));
        }
        if (inputId) {
          window.easyflowTransferSelectionOrderByInput[inputId] = values.slice();
          selectedOptions.forEach(function(option) {
            var value = option.getAttribute('data-value') || '';
            var index = values.indexOf(value);
            if (index >= 0) option.setAttribute('data-selected-order', String(index + 1));
          });
        }
        var select = inputId ? document.getElementById(inputId) : null;
        if (select) {
          easyflowTransferSyncHiddenSelect(select, values);
          if (window.jQuery) {
            window.jQuery(select).trigger('change');
          } else {
            select.dispatchEvent(new Event('change', {bubbles: true}));
          }
        }
        if (window.Shiny && inputId) {
          Shiny.setInputValue(inputId, values, {priority: 'event'});
          Shiny.setInputValue(inputId + '_selection_order', values, {priority: 'event'});
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
              item.removeAttribute('data-selected-order');
            });
          }
          var fallbackRange = options.slice(start, end + 1);
          if (index < lastIndex) fallbackRange.reverse();
          fallbackRange.forEach(function(item) {
            if (!item.classList.contains('is-selected')) {
              easyflowTransferSelectionCounter += 1;
              item.setAttribute('data-selected-order', String(easyflowTransferSelectionCounter));
            }
            item.classList.add('is-selected');
            item.setAttribute('aria-selected', 'true');
          });
        } else if (event && (event.ctrlKey || event.metaKey)) {
          var selected = !option.classList.contains('is-selected');
          option.classList.toggle('is-selected', selected);
          option.setAttribute('aria-selected', selected ? 'true' : 'false');
          if (selected) {
            easyflowTransferSelectionCounter += 1;
            option.setAttribute('data-selected-order', String(easyflowTransferSelectionCounter));
          } else {
            option.removeAttribute('data-selected-order');
          }
        } else {
          options.forEach(function(item) {
            item.classList.remove('is-selected');
            item.setAttribute('aria-selected', 'false');
            item.removeAttribute('data-selected-order');
          });
          option.classList.add('is-selected');
          option.setAttribute('aria-selected', 'true');
          easyflowTransferSelectionCounter += 1;
          option.setAttribute('data-selected-order', String(easyflowTransferSelectionCounter));
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
          if (!allSelected && !option.classList.contains('is-selected')) {
            easyflowTransferSelectionCounter += 1;
            option.setAttribute('data-selected-order', String(easyflowTransferSelectionCounter));
          } else if (allSelected) {
            option.removeAttribute('data-selected-order');
          }
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

        function easyflowNavbarText(anchor) {
          return anchor.clone().children().remove().end().text().trim();
        }

        function easyflowSetNavbarText(anchor, label) {
          if (!anchor.length || !label) return;
          var children = anchor.children().detach();
          anchor.empty().text(label);
          if (children.length) anchor.append(' ').append(children);
        }

        function easyflowMenuLabelDictionaries() {
          return {
            en: {
              top: {
                Data: 'Data',
                data_editor: 'Data Editor',
                calculator: 'Calculator',
                Analysis: 'Analysis',
                'Sample Size': 'Sample Size',
                'Effect Size': 'Effect Size',
                Result: 'Result',
                Help: 'Help',
                About: 'About'
              },
              items: {
                data_editor_coding_error_check: 'Auto coding error check',
                data_editor_likert: 'Auto Likert conversion',
                data_editor_missing_values: 'Auto missing values',
                data_editor_wide_long: 'Wide to Long',
                data_editor_recode_different: 'Auto reverse coding',
                data_editor_variable_calculation: 'Auto variable calculation',
                data_editor_variable_transformation: 'Variable transformation',
                data_editor_recode_same: 'Recode variable',
                data_editor_variable_rename: 'Rename variable',
                calculator_hint8: 'HINT8',
                calculator_eq5d: 'EQ-5D',
                calculator_metabolic: 'Metabolic syndrome',
                calculator_frs: 'Framingham risk score',
                calculator_ascvd10: 'ASCVD10',
                calculator_metabolic_severity: 'Metabolic severity',
                help_bug: 'Bug report',
                help_feature: 'Feature request',
                help_analysis_request: 'Analysis Method Request',
                help_qa: 'Q&A',
                about_update: 'Check for Updates',
                about_overview: 'Overview',
                about_user_guide: 'User Guide',
                about_analysis_methods: 'Analyses',
                about_method_notes: 'Method Notes',
                about_validation: 'Validation',
                about_version_history: 'Version History',
                about_source_license: 'Source & License',
                about_oss_licenses: 'Open Source Licenses',
                about: 'About'
              }
            },
            ko: {
              top: {
                Data: '\uB370\uC774\uD130',
                data_editor: '\uB370\uC774\uD130 \uD3B8\uC9D1',
                calculator: '\uACC4\uC0B0\uAE30',
                Analysis: '\uBD84\uC11D',
                'Sample Size': '\uD45C\uBCF8\uC218',
                'Effect Size': '\uD6A8\uACFC\uD06C\uAE30',
                Result: '\uACB0\uACFC',
                Help: '\uB3C4\uC6C0\uB9D0',
                About: '\uC815\uBCF4'
              },
              items: {
                data_editor_coding_error_check: '\uC790\uB3D9 \uCF54\uB529 \uC624\uB958 \uD655\uC778',
                data_editor_likert: 'Likert \uC790\uB3D9 \uBCC0\uD658',
                data_editor_missing_values: '\uACB0\uCE21\uAC12 \uC790\uB3D9\uCC98\uB9AC',
                data_editor_wide_long: '\uC640\uC774\uB4DC-\uB871 \uBCC0\uD658',
                data_editor_recode_different: '\uC5ED\uCF54\uB529 \uC790\uB3D9\uCC98\uB9AC',
                data_editor_variable_calculation: '\uBCC0\uC218 \uC790\uB3D9 \uACC4\uC0B0',
                data_editor_variable_transformation: '\uBCC0\uC218 \uBCC0\uD658',
                data_editor_recode_same: '\uBCC0\uC218 \uB9AC\uCF54\uB529',
                data_editor_variable_rename: '\uBCC0\uC218\uBA85 \uBCC0\uACBD',
                calculator_hint8: 'HINT8',
                calculator_eq5d: 'EQ-5D',
                calculator_metabolic: '\uB300\uC0AC\uC99D\uD6C4\uAD70',
                calculator_frs: 'Framingham \uC704\uD5D8\uB3C4',
                calculator_ascvd10: 'ASCVD10',
                calculator_metabolic_severity: '\uB300\uC0AC\uC99D\uD6C4\uAD70 \uC911\uC99D\uB3C4',
                help_bug: '\uBC84\uADF8 \uC2E0\uACE0',
                help_feature: '\uAE30\uB2A5 \uAC1C\uC120 \uC694\uCCAD',
                help_analysis_request: '\uBD84\uC11D\uAE30\uBC95 \uC694\uCCAD',
                help_qa: 'Q&A',
                about_update: '\uC5C5\uB370\uC774\uD2B8 \uD655\uC778',
                about_overview: '\uAC1C\uC694',
                about_user_guide: '\uC0AC\uC6A9\uC790 \uC548\uB0B4\uC11C',
                about_analysis_methods: '\uBD84\uC11D',
                about_method_notes: '\uBC29\uBC95\uB860 \uB178\uD2B8',
                about_validation: '\uAC80\uC99D',
                about_version_history: '\uBC84\uC804 \uC774\uB825',
                about_source_license: '\uC18C\uC2A4 \uBC0F \uB77C\uC774\uC120\uC2A4',
                about_oss_licenses: '\uC624\uD508\uC18C\uC2A4 \uB77C\uC774\uC120\uC2A4',
                about: '\uC815\uBCF4'
              }
            }
          };
        }

        function easyflowTopLabelLookup() {
          return {
            Data: 'Data',
            '\uB370\uC774\uD130': 'Data',
            data_editor: 'data_editor',
            'Data Editor': 'data_editor',
            '\uB370\uC774\uD130 \uD3B8\uC9D1': 'data_editor',
            calculator: 'calculator',
            Calculator: 'calculator',
            '\uACC4\uC0B0\uAE30': 'calculator',
            Analysis: 'Analysis',
            '\uBD84\uC11D': 'Analysis',
            'Sample Size': 'Sample Size',
            '\uD45C\uBCF8\uC218': 'Sample Size',
            'Effect Size': 'Effect Size',
            '\uD6A8\uACFC\uD06C\uAE30': 'Effect Size',
            Result: 'Result',
            '\uACB0\uACFC': 'Result',
            Help: 'Help',
            '\uB3C4\uC6C0\uB9D0': 'Help',
            About: 'About',
            '\uC815\uBCF4': 'About'
          };
        }

        function easyflowTranslateNavbarLabels() {
          var language = easyflowCurrentLanguage();
          var dictionaries = easyflowMenuLabelDictionaries();
          var dictionary = dictionaries[language] || dictionaries.ko;
          var topLookup = easyflowTopLabelLookup();

          window.jQuery('.navbar-nav > li > a').each(function() {
            var anchor = window.jQuery(this);
            var value = String(anchor.attr('data-value') || '');
            var key = topLookup[value] || topLookup[easyflowNavbarText(anchor)];
            if (key && dictionary.top[key]) {
              easyflowSetNavbarText(anchor, dictionary.top[key]);
            }
          });

          window.jQuery('.navbar-nav > li.dropdown > ul.dropdown-menu a[data-value]').each(function() {
            var anchor = window.jQuery(this);
            var value = String(anchor.attr('data-value') || '');
            if (dictionary.items[value]) {
              easyflowSetNavbarText(anchor, dictionary.items[value]);
            }
          });
        }

        function easyflowHelpRequestUrl(value) {
          var language = easyflowCurrentLanguage();
          var studioUrls = {
            help_bug: 'https://studio.statedu.com/help/bug/',
            help_feature: 'https://studio.statedu.com/help/feature/',
            help_analysis_request: 'https://studio.statedu.com/help/analysis/'
          };
          if (studioUrls[value]) return studioUrls[value];
          if (value === 'help_qa') {
            return language === 'ko'
              ? 'https://statedu.com/qna/?qna_action=write&qna_topic=StatEdu%20Studio'
              : 'https://statedu.com/en/qna/?qna_action=write&qna_topic=StatEdu%20Studio';
          }
          return '';
        }

        function easyflowOpenHelpRequestLink(event, navLink) {
          var navValue = navLink ? navLink.getAttribute('data-value') || '' : '';
          var url = easyflowHelpRequestUrl(navValue);
          if (!url) return false;
          if (event) {
            event.preventDefault();
            event.stopPropagation();
            if (typeof event.stopImmediatePropagation === 'function') {
              event.stopImmediatePropagation();
            }
          }
          if (window.jQuery) {
            window.jQuery(navLink).closest('.navbar-nav > li.dropdown').removeClass('open');
          }
          window.open(url, '_blank', 'noopener,noreferrer');
          return true;
        }

        document.addEventListener('click', function(event) {
          var navLink = event.target && event.target.closest ? event.target.closest('.navbar-nav a[data-value]') : null;
          if (!navLink || navLink.classList.contains('dropdown-toggle')) return;
          easyflowOpenHelpRequestLink(event, navLink);
        }, true);

        function easyflowGroupedMenuConfigs() {
          return [
            {
              menu: 'Analysis',
              menuLabels: ['Analysis', '\uBD84\uC11D'],
              marker: 'analysis',
              itemLabelsEn: {
                'Frequencies / Descriptives': 'Frequencies / Descriptives',
                analysis_crosstabs: 'Cross-tabulation Analysis',
                't-test / ANOVA': 't-test / ANOVA',
                'Paired test': 'Paired test',
                ANCOVA: 'ANCOVA',
                'Nonparametric Tests': 'Nonparametric Tests',
                'Nonparametric Paired': 'Nonparametric Paired',
                Correlation: 'Correlation',
                Reliability: 'Reliability',
                'Factor Analysis': 'Factor Analysis',
                'Principal Components': 'Principal Components',
                Regression: 'Regression',
                'Generalized Linear Model (GLM)': 'Generalized Linear Model (GLM)',
                analysis_logistic_regression: 'Logistic Regression',
                'Longitudinal / Panel Models': 'Longitudinal / Panel Models'
              },
              itemLabelsKo: {
                'Frequencies / Descriptives': '\uBE48\uB3C4\uBD84\uC11D / \uAE30\uC220\uD1B5\uACC4',
                analysis_crosstabs: '\uAD50\uCC28\uBD84\uC11D',
                't-test / ANOVA': 't-test / ANOVA',
                'Paired test': '\uB300\uC751\uD45C\uBCF8 \uAC80\uC815',
                ANCOVA: 'ANCOVA',
                'Nonparametric Tests': '\uBE44\uBAA8\uC218 \uAC80\uC815',
                'Nonparametric Paired': '\uB300\uC751 \uBE44\uBAA8\uC218 \uAC80\uC815',
                Correlation: '\uC0C1\uAD00\uBD84\uC11D',
                Reliability: '\uC2E0\uB8B0\uB3C4',
                'Factor Analysis': '\uC694\uC778\uBD84\uC11D',
                'Principal Components': '\uC8FC\uC131\uBD84\uBD84\uC11D',
                Regression: '\uD68C\uADC0\uBD84\uC11D',
                'Generalized Linear Model (GLM)': '\uC77C\uBC18\uD654 \uC120\uD615\uBAA8\uD615(GLM)',
                analysis_logistic_regression: '\uB85C\uC9C0\uC2A4\uD2F1 \uD68C\uADC0',
                'Longitudinal / Panel Models': '\uC885\uB2E8 / \uD328\uB110 \uBAA8\uD615'
              },
              groups: [
                {
                  title: 'Descriptives & Tables',
                  titleKo: '\uAE30\uC220\uD1B5\uACC4 / \uD45C',
                  values: ['Frequencies / Descriptives', 'analysis_crosstabs']
                },
                {
                  title: 'Group Comparisons',
                  titleKo: '\uC9D1\uB2E8 \uBE44\uAD50',
                  values: ['t-test / ANOVA', 'Paired test', 'ANCOVA']
                },
                {
                  title: 'Nonparametric Tests',
                  titleKo: '\uBE44\uBAA8\uC218 \uAC80\uC815',
                  values: ['Nonparametric Tests', 'Nonparametric Paired']
                },
                {
                  title: 'Association & Measurement',
                  titleKo: '\uC5F0\uAD00 / \uCE21\uC815',
                  values: ['Correlation', 'Reliability', 'Factor Analysis', 'Principal Components']
                },
                {
                  title: 'Regression & Models',
                  titleKo: '\uD68C\uADC0 / \uBAA8\uD615',
                  values: ['Regression', 'Generalized Linear Model (GLM)', 'analysis_logistic_regression']
                },
                {
                  title: 'Longitudinal / Panel',
                  titleKo: '\uC885\uB2E8 / \uD328\uB110',
                  values: ['Longitudinal / Panel Models']
                }
              ]
            },
            {
              menu: 'Sample Size',
              menuLabels: ['Sample Size', '\uD45C\uBCF8\uC218'],
              marker: 'sample-size',
              itemLabelsEn: {
                sample_size_proportion: 'Proportion',
                sample_size_chisquare: 'Chi-square',
                sample_size_mcnemar: 'McNemar',
                sample_size_ttest: 't-test',
                sample_size_anova: 'ANOVA',
                sample_size_ancova: 'ANCOVA / MANOVA',
                sample_size_nonparametric: 'Nonparametric',
                sample_size_correlation: 'Correlation',
                sample_size_reliability: 'Reliability / Agreement',
                sample_size_sem: 'SEM / CFA',
                sample_size_regression: 'Regression',
                sample_size_rates: 'Count / Rate Regression',
                sample_size_diagnostic: 'ROC AUC',
                sample_size_gee: 'GEE',
                sample_size_lmm: 'LMM',
                sample_size_survival: 'Survival / Cox',
                sample_size_equivalence: 'Equivalence / NI',
                sample_size_cluster: 'Cluster Trial',
                sample_size_precision: 'Precision / CI'
              },
              itemLabelsKo: {
                sample_size_proportion: '\uBE44\uC728',
                sample_size_chisquare: '\uCE74\uC774\uC81C\uACF1',
                sample_size_mcnemar: '\uB9E5\uB2C8\uB9C8',
                sample_size_ttest: 't-test',
                sample_size_anova: 'ANOVA',
                sample_size_ancova: 'ANCOVA / MANOVA',
                sample_size_nonparametric: '\uBE44\uBAA8\uC218',
                sample_size_correlation: '\uC0C1\uAD00',
                sample_size_reliability: '\uC2E0\uB8B0\uB3C4 / \uC77C\uCE58\uB3C4',
                sample_size_sem: '\uAD6C\uC870\uBC29\uC815\uC2DD/\uD655\uC778\uC801\uC694\uC778\uBD84\uC11D',
                sample_size_regression: '\uD68C\uADC0',
                sample_size_rates: '\uAC74\uC218 / \uBC1C\uC0DD\uB960 \uD68C\uADC0',
                sample_size_diagnostic: 'ROC AUC',
                sample_size_gee: '\uC77C\uBC18\uD654\uCD94\uC815\uBC29\uC815\uC2DD',
                sample_size_lmm: '\uC120\uD615\uD63C\uD569\uBAA8\uD615',
                sample_size_survival: '\uC0DD\uC874 / Cox',
                sample_size_equivalence: '\uB3D9\uB4F1\uC131 / \uBE44\uC5F4\uB4F1\uC131',
                sample_size_cluster: '\uAD70\uC9D1 \uC5F0\uAD6C',
                sample_size_precision: '\uC815\uBC00\uB3C4 / \uC2E0\uB8B0\uAD6C\uAC04'
              },
              groups: [
                {
                  title: 'Descriptives & Tables',
                  titleKo: '\uAE30\uC220\uD1B5\uACC4 / \uD45C',
                  values: ['sample_size_proportion', 'sample_size_chisquare', 'sample_size_mcnemar']
                },
                {
                  title: 'Group Comparisons',
                  titleKo: '\uC9D1\uB2E8 \uBE44\uAD50',
                  values: ['sample_size_ttest', 'sample_size_anova', 'sample_size_ancova']
                },
                {
                  title: 'Nonparametric Tests',
                  titleKo: '\uBE44\uBAA8\uC218 \uAC80\uC815',
                  values: ['sample_size_nonparametric']
                },
                {
                  title: 'Association & Measurement',
                  titleKo: '\uC5F0\uAD00 / \uCE21\uC815',
                  values: ['sample_size_correlation', 'sample_size_reliability', 'sample_size_sem', 'sample_size_diagnostic']
                },
                {
                  title: 'Regression & Models',
                  titleKo: '\uD68C\uADC0 / \uBAA8\uD615',
                  values: ['sample_size_regression', 'sample_size_rates', 'sample_size_survival']
                },
                {
                  title: 'Longitudinal / Panel',
                  titleKo: '\uC885\uB2E8 / \uD328\uB110',
                  values: ['sample_size_gee', 'sample_size_lmm']
                },
                {
                  title: 'Study Design & Precision',
                  titleKo: '\uC5F0\uAD6C \uC124\uACC4 / \uC815\uBC00\uB3C4',
                  values: ['sample_size_equivalence', 'sample_size_cluster', 'sample_size_precision']
                }
              ]
            },
            {
              menu: 'Effect Size',
              menuLabels: ['Effect Size', '\uD6A8\uACFC\uD06C\uAE30'],
              marker: 'effect-size',
              itemLabelsEn: {
                effect_size_proportion: 'Proportion',
                effect_size_chisquare: 'Chi-square',
                effect_size_mcnemar: 'McNemar',
                effect_size_ttest: 't-test',
                effect_size_anova: 'ANOVA',
                effect_size_ancova: 'ANCOVA / MANOVA',
                effect_size_nonparametric: 'Nonparametric',
                effect_size_correlation: 'Correlation',
                effect_size_regression: 'Regression',
                effect_size_rates: 'Count / Rate Regression',
                effect_size_diagnostic: 'ROC AUC',
                effect_size_gee: 'GEE',
                effect_size_lmm: 'LMM',
                effect_size_glmm: 'GLMM',
                effect_size_survival: 'Survival / Cox'
              },
              itemLabelsKo: {
                effect_size_proportion: '\uBE44\uC728',
                effect_size_chisquare: '\uCE74\uC774\uC81C\uACF1',
                effect_size_mcnemar: '\uB9E5\uB2C8\uB9C8',
                effect_size_ttest: 't-test',
                effect_size_anova: 'ANOVA',
                effect_size_ancova: 'ANCOVA / MANOVA',
                effect_size_nonparametric: '\uBE44\uBAA8\uC218',
                effect_size_correlation: '\uC0C1\uAD00',
                effect_size_regression: '\uD68C\uADC0',
                effect_size_rates: '\uAC74\uC218 / \uBC1C\uC0DD\uB960 \uD68C\uADC0',
                effect_size_diagnostic: 'ROC AUC',
                effect_size_gee: '\uC77C\uBC18\uD654\uCD94\uC815\uBC29\uC815\uC2DD',
                effect_size_lmm: '\uC120\uD615\uD63C\uD569\uBAA8\uD615',
                effect_size_glmm: '\uC77C\uBC18\uD654\uC120\uD615\uBAA8\uD615',
                effect_size_survival: '\uC0DD\uC874 / Cox'
              },
              groups: [
                {
                  title: 'Descriptives & Tables',
                  titleKo: '\uAE30\uC220\uD1B5\uACC4 / \uD45C',
                  values: ['effect_size_proportion', 'effect_size_chisquare', 'effect_size_mcnemar']
                },
                {
                  title: 'Group Comparisons',
                  titleKo: '\uC9D1\uB2E8 \uBE44\uAD50',
                  values: ['effect_size_ttest', 'effect_size_anova', 'effect_size_ancova']
                },
                {
                  title: 'Nonparametric Tests',
                  titleKo: '\uBE44\uBAA8\uC218 \uAC80\uC815',
                  values: ['effect_size_nonparametric']
                },
                {
                  title: 'Association & Measurement',
                  titleKo: '\uC5F0\uAD00 / \uCE21\uC815',
                  values: ['effect_size_correlation', 'effect_size_diagnostic']
                },
                {
                  title: 'Regression & Models',
                  titleKo: '\uD68C\uADC0 / \uBAA8\uD615',
                  values: ['effect_size_regression', 'effect_size_rates', 'effect_size_survival']
                },
                {
                  title: 'Longitudinal / Panel',
                  titleKo: '\uC885\uB2E8 / \uD328\uB110',
                  values: ['effect_size_gee', 'effect_size_lmm', 'effect_size_glmm']
                }
              ]
            }
          ];
        }

        function groupNavbarDropdownItems(config) {
          var menuLabels = config.menuLabels || [config.menu];
          var navItem = window.jQuery('.navbar-nav > li.dropdown > a.dropdown-toggle')
            .filter(function() {
              var label = window.jQuery(this).clone().children().remove().end().text().trim();
              return menuLabels.indexOf(label) >= 0;
            })
            .parent()
            .first();
          if (!navItem.length) return;
          var menuLanguage = easyflowCurrentLanguage();
          var useKorean = menuLanguage === 'ko';
          var itemLabelsEn = config.itemLabelsEn || {};
          var itemLabelsKo = config.itemLabelsKo || {};
          var menu = navItem.children('ul.dropdown-menu').first();
          if (!menu.length) return;
          if (menu.attr('data-easyflow-menu-grouped') === config.marker) {
            if (menu.attr('data-easyflow-menu-language') === menuLanguage) return;
            config.groups.forEach(function(group) {
              var firstValue = group.values[0];
              var groupLink = menu.find('a[data-value="' + firstValue + '"]').first();
              var groupNode = groupLink.closest('.analysis-menu-section');
              var groupTitleText = useKorean && group.titleKo ? group.titleKo : group.title;
              if (!groupNode.length) return;
              groupNode.children('.analysis-menu-section-title').first().text(groupTitleText);
              group.values.forEach(function(value) {
                var link = menu.find('a[data-value="' + value + '"]').first();
                if (!link.length) return;
                var itemLabel = useKorean ? itemLabelsKo[value] : itemLabelsEn[value];
                if (itemLabel) link.text(itemLabel);
              });
            });
            menu.attr('data-easyflow-menu-language', menuLanguage);
            return;
          }

          var existingItems = {};
          menu.children('li').each(function() {
            var item = window.jQuery(this);
            var link = item.children('a[data-value]').first();
            if (!link.length) return;
            existingItems[String(link.attr('data-value'))] = item.detach();
          });

          config.groups.forEach(function(group) {
            var groupItems = [];
            group.values.forEach(function(value) {
              if (existingItems[value]) {
                var itemLabel = useKorean ? itemLabelsKo[value] : itemLabelsEn[value];
                if (itemLabel) {
                  existingItems[value].children('a[data-value]').first().text(itemLabel);
                }
                groupItems.push(existingItems[value]);
                delete existingItems[value];
              }
            });
            if (groupItems.length === 0) return;
            var groupNode = window.jQuery('<li class="analysis-menu-section" role="presentation"></li>');
            var groupTitleText = useKorean && group.titleKo ? group.titleKo : group.title;
            var groupTitle = window.jQuery('<button type="button" class="analysis-menu-section-title" aria-expanded="false"></button>').text(groupTitleText);
            var groupList = window.jQuery('<ul class="analysis-menu-section-items" role="menu"></ul>');
            groupItems.forEach(function(item) {
              groupList.append(item);
            });
            groupNode.append(groupTitle, groupList);
            if (groupList.children('li.active').length) {
              groupNode.addClass('active');
              groupNode.addClass('open');
              groupTitle.attr('aria-expanded', 'true');
            }
            menu.append(groupNode);
          });

          Object.keys(existingItems).forEach(function(value) {
            menu.append(existingItems[value]);
          });
          if (!menu.children('.analysis-menu-section.active').length && !menu.children('li.active').not('.analysis-menu-section').length) {
            menu.children('.analysis-menu-section').first().addClass('open')
              .children('.analysis-menu-section-title').attr('aria-expanded', 'true');
          }
          menu.addClass('analysis-submenu')
            .attr('data-easyflow-menu-grouped', config.marker)
            .attr('data-easyflow-menu-language', menuLanguage)
            .attr('data-analysis-menu-grouped', 'true');
        }

        function groupAnalysisDropdownItems() {
          easyflowTranslateNavbarLabels();
          easyflowGroupedMenuConfigs().forEach(groupNavbarDropdownItems);
          easyflowTranslateNavbarLabels();
        }

        function configureNestedDropdownToggles() {
          groupAnalysisDropdownItems();
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

        function markNavbarDropdownActive(link) {
          var dropdown = link.closest('.navbar-nav > li.dropdown');
          if (!dropdown.length) return;
          dropdown.closest('.navbar-nav').children('li.active').removeClass('active');
          dropdown.addClass('active');
        }

        window.jQuery(document)
          .on('click.easyflowNestedDropdown', '.dropdown-menu .dropdown-toggle', function(event) {
            event.preventDefault();
            event.stopPropagation();
            var item = window.jQuery(this).parent();
            item.toggleClass('open');
            item.siblings('.dropdown.open').removeClass('open');
          })
          .on('mouseenter.easyflowAnalysisSubmenu focusin.easyflowAnalysisSubmenu click.easyflowAnalysisSubmenu', '.analysis-submenu .analysis-menu-section-title', function(event) {
            event.preventDefault();
            event.stopPropagation();
            var section = window.jQuery(this).closest('.analysis-menu-section');
            var menu = section.closest('.analysis-submenu');
            menu.children('.analysis-menu-section').removeClass('open')
              .children('.analysis-menu-section-title').attr('aria-expanded', 'false');
            section.addClass('open');
            window.jQuery(this).attr('aria-expanded', 'true');
          })
          .on('mouseenter.easyflowAnalysisDirectItem focusin.easyflowAnalysisDirectItem', '.analysis-submenu > li.analysis-menu-direct-item > a[data-value]', function() {
            var menu = window.jQuery(this).closest('.analysis-submenu');
            menu.children('.analysis-menu-section').removeClass('open')
              .children('.analysis-menu-section-title').attr('aria-expanded', 'false');
          })
          .on('click.easyflowAnalysisSubmenu', '.analysis-submenu .analysis-menu-section-items a[data-value]', function() {
            var link = window.jQuery(this);
            var menu = link.closest('.analysis-submenu');
            var markActive = function() {
              menu.find('li.active, .analysis-menu-section.active').removeClass('active');
              menu.children('.analysis-menu-section').removeClass('open');
              link.parent('li').addClass('active');
              link.closest('.analysis-menu-section').addClass('active open');
              menu.find('.analysis-menu-section-title').attr('aria-expanded', 'false');
              link.closest('.analysis-menu-section').children('.analysis-menu-section-title').attr('aria-expanded', 'true');
              markNavbarDropdownActive(link);
            };
            markActive();
            window.setTimeout(markActive, 0);
            link.closest('.navbar-nav > li.dropdown').removeClass('open');
          })
          .on('click.easyflowAnalysisDirectItem', '.analysis-submenu > li.analysis-menu-direct-item > a[data-value]', function() {
            var link = window.jQuery(this);
            var menu = link.closest('.analysis-submenu');
            var markActive = function() {
              menu.find('li.active, .analysis-menu-section.active').removeClass('active');
              menu.children('.analysis-menu-section').removeClass('open');
              menu.find('.analysis-menu-section-title').attr('aria-expanded', 'false');
              link.parent('li').addClass('active');
              markNavbarDropdownActive(link);
            };
            markActive();
            window.setTimeout(markActive, 0);
            link.closest('.navbar-nav > li.dropdown').removeClass('open');
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
        Shiny.addCustomMessageHandler('easyflow-clear-coding-error-fixes', function(message) {
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
        Shiny.addCustomMessageHandler('easyflow-clear-data-session', function(message) {
          clearEasyflowClientDataSession();
        });
      }

      registerEasyflowDataSessionHandler();
      document.addEventListener('shiny:connected', registerEasyflowDataSessionHandler);

      function registerEasyflowMeasurementUpdateHandler() {
        if (!window.Shiny || !Shiny.addCustomMessageHandler || window.easyflowMeasurementUpdateHandlerRegistered) {
          return;
        }
        window.easyflowMeasurementUpdateHandlerRegistered = true;
        Shiny.addCustomMessageHandler('easyflow-update-measurements', function(message) {
          window.easyflowMeasurements = window.easyflowMeasurements || {};
          Object.keys(message || {}).forEach(function(name) {
            var value = message[name] || '';
            if (!name || !value) return;
            window.easyflowMeasurements[name] = value;
            document.querySelectorAll('select.measurement-select[data-name], select.category-measurement-select[data-name]').forEach(function(select) {
              if (select.getAttribute('data-name') !== name) return;
              select.value = value;
              if (window.jQuery) {
                window.jQuery(select).trigger('change');
              } else {
                select.dispatchEvent(new Event('change', {bubbles: true}));
              }
            });
          });
        });
      }

      registerEasyflowMeasurementUpdateHandler();
      document.addEventListener('shiny:connected', registerEasyflowMeasurementUpdateHandler);

      function registerEasyflowLikertSelectionHandler() {
        if (!window.Shiny || !Shiny.addCustomMessageHandler || window.easyflowLikertSelectionHandlerRegistered) {
          return;
        }
        window.easyflowLikertSelectionHandlerRegistered = true;
        Shiny.addCustomMessageHandler('easyflow-clear-likert-selection', function(message) {
          window.easyflowLikertSelected = '';
        });
      }

      registerEasyflowLikertSelectionHandler();
      document.addEventListener('shiny:connected', registerEasyflowLikertSelectionHandler);

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
        (window.easyflowBulkMeasurementPairs || []).forEach(function(pair) {
          if (!pair || !pair.name) return;
          state.measurements[pair.name] = pair.value || '';
        });
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

      window.easyflowApplySelectedVariableReview = function() {
        if (!window.Shiny) return false;
        flushEasyflowInputs();
        var state = collectEasyflowSelectedVariableReviewState();
        state.nonce = Date.now() + Math.random();
        Shiny.setInputValue('apply_selected_variable_review_request', state, {priority: 'event'});
        return false;
      };

      function collectEasyflowSelectedVariableReviewState() {
        var root = document.getElementById('selected_variable_edit_table') || document;
        var measurements = Object.assign({}, window.easyflowMeasurements || {});
        var varLabels = Object.assign({}, window.easyflowVarLabels || {});
        var selectedMap = Object.assign({}, window.easyflowSelectedNames || {});

        if (!root || !root.querySelectorAll) {
          return submitEasyflowTableState();
        }

        root.querySelectorAll('input.variable-select[data-name]').forEach(function(input) {
          var name = input.getAttribute('data-name') || '';
          if (!name || input.disabled) return;
          selectedMap[name] = true;
        });

        root.querySelectorAll('select.measurement-select[data-name]').forEach(function(select) {
          var name = select.getAttribute('data-name') || '';
          if (!name) return;
          measurements[name] = select.value || '';
          window.easyflowMeasurements = window.easyflowMeasurements || {};
          window.easyflowMeasurements[name] = measurements[name];
        });

        root.querySelectorAll('input.var-label-input[data-name], input[data-field=\"var_label\"][data-name]').forEach(function(input) {
          var name = input.getAttribute('data-name') || '';
          if (!name) return;
          varLabels[name] = input.value || '';
          window.easyflowVarLabels = window.easyflowVarLabels || {};
          window.easyflowVarLabels[name] = varLabels[name];
        });

        return {
          selected: Object.keys(selectedMap),
          measurements: measurements,
          measurement_pairs: Object.keys(measurements).map(function(name) {
            return {name: name, value: measurements[name]};
          }),
          var_labels: varLabels
        };
      }
      window.easyflowCollectSelectedVariableReviewState = collectEasyflowSelectedVariableReviewState;

      function mergeEasyflowNamedValues(target, source) {
        Object.keys(source || {}).forEach(function(name) {
          if (!name) return;
          target[name] = source[name];
        });
        return target;
      }

      function buildEasyflowStep3ReviewState() {
        var categoryState = collectEasyflowCategoryLabelState();
        var selectedState = collectEasyflowSelectedVariableReviewState();
        var categoryLabels = categoryState.category_labels || {};
        var varLabels = mergeEasyflowNamedValues(
          Object.assign({}, categoryState.var_labels || {}),
          selectedState.var_labels || {}
        );
        var measurements = mergeEasyflowNamedValues(
          Object.assign({}, categoryState.measurements || {}),
          selectedState.measurements || {}
        );

        Object.keys(varLabels).forEach(function(name) {
          if (!name) return;
          categoryLabels[name] = categoryLabels[name] || {};
          categoryLabels[name].var_label = varLabels[name] || '';
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
          }),
          selected: selectedState.selected || []
        };
      }

      window.easyflowApplyStep3Review = function() {
        if (!window.Shiny) return false;
        flushEasyflowInputs();
        var state = buildEasyflowStep3ReviewState();
        state.nonce = Date.now() + Math.random();
        Shiny.setInputValue('apply_selected_variable_review_request', {
          selected: state.selected || [],
          measurements: state.measurements || {},
          measurement_pairs: state.measurement_pairs || [],
          var_labels: state.var_labels || {},
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
        Shiny.setInputValue('apply_category_labels_request', state, {priority: 'event'});
        Shiny.setInputValue('variable_measurement_snapshot', {
          values: state.measurements || {},
          measurement_pairs: state.measurement_pairs || [],
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
        return false;
      };

      function setEasyflowStep3ToggleLabel(button, view) {
        if (!button) return;
        var normalized = view === 'variables' ? 'variables' : 'labels';
        if (button.classList && button.classList.contains('step3-toggle-combined')) {
          var labelSpan = button.querySelector('[data-step3-label]');
          var selectedSpan = button.querySelector('[data-step3-selected]');
          if (labelSpan) labelSpan.classList.toggle('is-active', normalized === 'labels');
          if (selectedSpan) selectedSpan.classList.toggle('is-active', normalized === 'variables');
          return;
        }
        var language = easyflowCurrentLanguage();
        button.textContent = language === 'ko'
          ? (normalized === 'variables' ? '\uB77C\uBCA8' : '\uBCC0\uC218')
          : (normalized === 'variables' ? 'Labels' : 'Variables');
      }

      window.easyflowToggleStep3View = function(button) {
        window.easyflowStep3View = window.easyflowStep3View === 'variables' ? 'labels' : 'variables';
        document.querySelectorAll('.step3-toggle-button, .step3-toggle-combined').forEach(function(item) {
          setEasyflowStep3ToggleLabel(item, window.easyflowStep3View);
        });
        document.querySelectorAll('.step3-labels-section').forEach(function(section) {
          section.style.display = window.easyflowStep3View === 'labels' ? '' : 'none';
        });
        document.querySelectorAll('.step3-variables-section').forEach(function(section) {
          section.style.display = window.easyflowStep3View === 'variables' ? '' : 'none';
        });
        if (easyflowCanSetInputValue()) {
          Shiny.setInputValue('step3_panel_view', window.easyflowStep3View, {priority: 'event'});
        }
        return false;
      };

      function initializeEasyflowStep3View() {
        if (!window.easyflowStep3View) window.easyflowStep3View = 'labels';
        document.querySelectorAll('.step3-toggle-button, .step3-toggle-combined').forEach(function(button) {
          setEasyflowStep3ToggleLabel(button, window.easyflowStep3View);
        });
        document.querySelectorAll('.step3-labels-section').forEach(function(section) {
          section.style.display = window.easyflowStep3View === 'labels' ? '' : 'none';
        });
        document.querySelectorAll('.step3-variables-section').forEach(function(section) {
          section.style.display = window.easyflowStep3View === 'variables' ? '' : 'none';
        });
        if (easyflowCanSetInputValue()) {
          Shiny.setInputValue('step3_panel_view', window.easyflowStep3View, {priority: 'event'});
        }
      }

      document.addEventListener('DOMContentLoaded', initializeEasyflowStep3View);
      window.setTimeout(initializeEasyflowStep3View, 0);

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

      function easyflowBlobToDataUrl(blob) {
        return new Promise(function(resolve, reject) {
          var reader = new FileReader();
          reader.onload = function() { resolve(reader.result); };
          reader.onerror = function() { reject(reader.error); };
          reader.readAsDataURL(blob);
        });
      }

      async function easyflowInlineSnapshotImages(source, clone) {
        var sourceImages = source.querySelectorAll ? source.querySelectorAll('img') : [];
        var cloneImages = clone.querySelectorAll ? clone.querySelectorAll('img') : [];
        for (var i = 0; i < sourceImages.length && i < cloneImages.length; i += 1) {
          var sourceImage = sourceImages[i];
          var cloneImage = cloneImages[i];
          var src = sourceImage.currentSrc || sourceImage.src || cloneImage.getAttribute('src') || '';
          if (!src || src.indexOf('data:') === 0) continue;
          try {
            var response = await fetch(src, { credentials: 'same-origin' });
            if (!response.ok) continue;
            var dataUrl = await easyflowBlobToDataUrl(await response.blob());
            cloneImage.setAttribute('src', dataUrl);
            cloneImage.removeAttribute('srcset');
          } catch (error) {
            if (window.console && window.console.warn) {
              window.console.warn('StatEdu Studio result image snapshot failed', error);
            }
          }
        }
      }

      function easyflowInlineSnapshotCanvases(source, clone) {
        var sourceCanvases = source.querySelectorAll ? source.querySelectorAll('canvas') : [];
        var cloneCanvases = clone.querySelectorAll ? clone.querySelectorAll('canvas') : [];
        for (var i = 0; i < sourceCanvases.length && i < cloneCanvases.length; i += 1) {
          try {
            var dataUrl = sourceCanvases[i].toDataURL('image/png');
            var img = document.createElement('img');
            img.setAttribute('src', dataUrl);
            img.setAttribute('alt', cloneCanvases[i].getAttribute('aria-label') || 'Result figure');
            img.style.maxWidth = '100%';
            cloneCanvases[i].parentNode.replaceChild(img, cloneCanvases[i]);
          } catch (error) {
            if (window.console && window.console.warn) {
              window.console.warn('StatEdu Studio result canvas snapshot failed', error);
            }
          }
        }
      }

      async function easyflowSnapshotHtml(element) {
        var clone = element.cloneNode(true);
        easyflowInlineSnapshotCanvases(element, clone);
        await easyflowInlineSnapshotImages(element, clone);
        return clone.innerHTML || '';
      }

      function registerEasyflowResultSnapshotHandler() {
        if (!window.Shiny || window.easyflowResultSnapshotHandlerRegistered) return;
        window.easyflowResultSnapshotHandlerRegistered = true;
        Shiny.addCustomMessageHandler('easyflow-capture-result-snapshot', function(message) {
          (async function() {
            var inputId = message && message.inputId ? String(message.inputId) : '';
            var outputId = message && message.outputId ? String(message.outputId) : '';
            var element = outputId ? document.getElementById(outputId) : null;
            var payload = {
              outputId: outputId,
              html: '',
              text: '',
              error: '',
              nonce: Date.now() + Math.random()
            };
            if (!inputId) return;
            if (!element) {
              payload.error = 'Result output was not found.';
            } else {
              payload.text = (element.textContent || '').replace(/\s+/g, ' ').trim();
              payload.html = await easyflowSnapshotHtml(element);
              if (!payload.html || !payload.text) {
                payload.error = 'No analysis result is available to add.';
              }
            }
            Shiny.setInputValue(inputId, payload, { priority: 'event' });
          })();
        });
      }
      registerEasyflowResultSnapshotHandler();
      document.addEventListener('shiny:connected', registerEasyflowResultSnapshotHandler);
      window.setTimeout(registerEasyflowResultSnapshotHandler, 0);

      window.addEventListener('beforeunload', function(event) {
        if (!window.easyflowSettingsDirty) return;
        event.preventDefault();
        event.returnValue = '';
      });

      window.addEventListener('error', function(event) {
        if (!easyflowCanSetInputValue()) return;
        Shiny.setInputValue('client_js_error', {
          message: event.message || '',
          source: event.filename || '',
          line: event.lineno || '',
          column: event.colno || '',
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });

      window.addEventListener('unhandledrejection', function(event) {
        if (!easyflowCanSetInputValue()) return;
        Shiny.setInputValue('client_js_error', {
          message: String(event.reason || ''),
          source: 'unhandledrejection',
          line: '',
          column: '',
          nonce: Date.now() + Math.random()
        }, {priority: 'event'});
      });

      document.addEventListener('click', function(event) {
        var button = event.target && event.target.closest ? event.target.closest('button[id^="effect_size_"][id$="_calculate"]') : null;
        if (!button || !window.Shiny) return;
        Shiny.setInputValue(button.id, Date.now() + Math.random(), {priority: 'event'});
      }, true);

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

      function syncEasyflowTopNavbarActive(link) {
        if (!link || !window.jQuery) return;
        var item = window.jQuery(link).closest('.navbar-nav > li');
        if (!item.length) return;
        window.jQuery('.navbar-nav > li.active').not(item).removeClass('active');
        item.addClass('active');
      }

      document.addEventListener('click', function(event) {
        var navLink = event.target && event.target.closest ? event.target.closest('.navbar-nav a[data-value]') : null;
        if (!navLink || navLink.classList.contains('dropdown-toggle')) return;
        var navValue = navLink.getAttribute('data-value') || '';
        if (!navValue || !window.jQuery) return;
        window.setTimeout(function() {
          var link = window.jQuery(navLink);
          if (!link.length) return;
          var item = link.parent('li');
          var wasActive = item.hasClass('active');
          if (wasActive) {
            item.removeClass('active');
          }
          try {
            link.tab('show');
          } catch (error) {
            if (wasActive) {
              item.addClass('active');
            }
          }
          syncEasyflowTopNavbarActive(navLink);
          link.closest('.navbar-nav > li.dropdown').removeClass('open');
        }, 0);
      }, true);

      document.addEventListener('click', function(event) {
        var navLink = event.target && event.target.closest ? event.target.closest('.navbar-nav a[data-value="data_editor_wide_long"]') : null;
        if (!navLink || navLink.classList.contains('dropdown-toggle')) return;
        window.setTimeout(function() {
          if (window.jQuery) {
            var link = window.jQuery('.navbar-nav a[data-value="data_editor_wide_long"]').first();
            if (link.length && !link.parent().hasClass('active')) {
              link.tab('show');
            }
          }
          if (window.Shiny) {
            Shiny.setInputValue('wide_long_nav_request', {
              nonce: Date.now() + Math.random()
            }, {priority: 'event'});
          }
        }, 0);
      }, true);

      var easyflowTransferLastIndexByInput = {};
      var easyflowTransferSelectionCounter = 0;
      var easyflowActiveTransferListbox = null;
      var easyflowActiveTransferInputId = null;

      function easyflowTransferOptions(listbox) {
        return Array.prototype.slice.call(listbox.querySelectorAll('.analysis-transfer-option'));
      }

      function easyflowTransferSelectedValues(listbox) {
        var inputId = listbox ? (listbox.getAttribute('data-input-id') || '') : '';
        var selectedOptions = easyflowTransferOptions(listbox)
          .filter(function(option) { return option.classList.contains('is-selected'); })
        var hasExplicitOrder = selectedOptions.some(function(option) {
          var order = parseInt(option.getAttribute('data-selected-order') || '0', 10);
          return !Number.isNaN(order) && order > 0;
        });
        var values = selectedOptions.sort(function(a, b) {
            var aOrder = parseInt(a.getAttribute('data-selected-order') || '0', 10);
            var bOrder = parseInt(b.getAttribute('data-selected-order') || '0', 10);
            if (Number.isNaN(aOrder)) aOrder = 0;
            if (Number.isNaN(bOrder)) bOrder = 0;
            if (aOrder && bOrder && aOrder !== bOrder) return aOrder - bOrder;
            if (aOrder && !bOrder) return -1;
            if (!aOrder && bOrder) return 1;
            return 0;
          })
          .map(function(option) { return option.getAttribute('data-value'); });
        var storedOrder = inputId ? (window.easyflowTransferSelectionOrderByInput[inputId] || []) : [];
        if (!hasExplicitOrder && storedOrder.length > 0) {
          values = storedOrder
            .filter(function(value) { return values.indexOf(value) >= 0; })
            .concat(values.filter(function(value) { return storedOrder.indexOf(value) < 0; }));
        }
        return values;
      }

      function easyflowTransferSetSelected(option, selected) {
        var wasSelected = option.classList.contains('is-selected');
        option.classList.toggle('is-selected', selected);
        option.setAttribute('aria-selected', selected ? 'true' : 'false');
        if (selected && !wasSelected) {
          easyflowTransferSelectionCounter += 1;
          option.setAttribute('data-selected-order', String(easyflowTransferSelectionCounter));
        } else if (!selected) {
          option.removeAttribute('data-selected-order');
        }
      }

      function easyflowTransferSyncHiddenSelect(select, values) {
        if (!select) return;
        var valueOrder = {};
        values.forEach(function(value, index) {
          valueOrder[value] = index + 1;
        });
        var options = Array.prototype.slice.call(select.options);
        options
          .sort(function(a, b) {
            var aOrder = valueOrder[a.value] || 0;
            var bOrder = valueOrder[b.value] || 0;
            if (aOrder && bOrder && aOrder !== bOrder) return aOrder - bOrder;
            if (aOrder && !bOrder) return -1;
            if (!aOrder && bOrder) return 1;
            return options.indexOf(a) - options.indexOf(b);
          })
          .forEach(function(option) {
            select.appendChild(option);
          });
        Array.prototype.forEach.call(select.options, function(option) {
          option.selected = values.indexOf(option.value) >= 0;
        });
      }

      function easyflowTransferOptionValue(option) {
        return option ? (option.getAttribute('data-value') || '') : '';
      }

      function easyflowTransferDragValues(option, listbox) {
        var value = easyflowTransferOptionValue(option);
        var selected = easyflowTransferSelectedValues(listbox);
        if (value && selected.indexOf(value) >= 0) {
          return selected;
        }
        return value ? [value] : [];
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
          easyflowTransferSyncHiddenSelect(select, values);
          if (window.jQuery) {
            window.jQuery(select).trigger('change');
          } else {
            select.dispatchEvent(new Event('change', {bubbles: true}));
          }
        }
        if (window.Shiny && inputId) {
          window.easyflowTransferSelectionOrderByInput[inputId] = values.slice();
          Shiny.setInputValue(inputId, values, {priority: 'event'});
          Shiny.setInputValue(inputId + '_selection_order', values, {priority: 'event'});
        }
      }

      window.easyflowSyncTransferListbox = easyflowTransferSync;

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
        if (window.easyflowTransferSuppressClickUntil && Date.now() < window.easyflowTransferSuppressClickUntil) {
          if (event) {
            event.preventDefault();
            event.stopPropagation();
          }
          return;
        }
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
          var range = options.slice(start, end + 1);
          if (index < lastIndex) range.reverse();
          range.forEach(function(item) {
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

      function easyflowDecorateTransferDragOptions(root) {
        root = root || document;
        if (!root.querySelectorAll) return;
        root.querySelectorAll('.analysis-transfer-option').forEach(function(option) {
          option.setAttribute('draggable', 'false');
        });
      }

      function easyflowTransferDropPayload(source, target, values) {
        return {
          source: source ? (source.getAttribute('data-input-id') || '') : '',
          target: target ? (target.getAttribute('data-input-id') || '') : '',
          values: values || [],
          nonce: Date.now() + Math.random()
        };
      }

      function easyflowTransferListboxFromEvent(event) {
        var listbox = event.target && event.target.closest ? event.target.closest('.analysis-transfer-listbox') : null;
        if (listbox) return listbox;
        var panel = event.target && event.target.closest ? event.target.closest('.analysis-transfer-panel') : null;
        if (panel && panel.querySelector) {
          listbox = panel.querySelector('.analysis-transfer-listbox');
          if (listbox) return listbox;
        }
        if (typeof event.clientX === 'number' && typeof event.clientY === 'number' && document.elementFromPoint) {
          var element = document.elementFromPoint(event.clientX, event.clientY);
          if (element && element.closest) {
            listbox = element.closest('.analysis-transfer-listbox');
            if (listbox) return listbox;
            panel = element.closest('.analysis-transfer-panel');
            if (panel && panel.querySelector) {
              return panel.querySelector('.analysis-transfer-listbox');
            }
          }
        }
        return null;
      }

      function easyflowTransferCalculatorSelectFromEvent(event) {
        if (typeof event.clientX !== 'number' || typeof event.clientY !== 'number' || !document.elementFromPoint) {
          return null;
        }
        var element = document.elementFromPoint(event.clientX, event.clientY);
        if (!element || !element.closest) return null;
        var panel = element.closest('.hint8-target-panel, .metabolic-target-panel, .eq5d-target-panel, .frs-target-panel, .ascvd10-target-panel, .mbss-target-panel');
        if (!panel || element.closest('.eq5d-type-control')) return null;
        var select = element.closest('select');
        if (!select) {
          var group = element.closest('.form-group, .shiny-input-container');
          select = group && group.querySelector ? group.querySelector('select') : null;
        }
        if (!select || !panel.contains(select) || !select.id) return null;
        if (select.closest('.eq5d-type-control')) return null;
        return select;
      }

      var easyflowPointerTransfer = null;

      function easyflowTransferLabelForGhost(option, values) {
        var label = option ? (option.innerText || option.textContent || '') : '';
        label = label.replace(/\s+/g, ' ').trim();
        if (values && values.length > 1) return values.length + ' variables';
        return label || 'Move variable';
      }

      function easyflowTransferCreateGhost(option, values) {
        var ghost = document.createElement('div');
        ghost.className = 'analysis-transfer-drag-ghost';
        ghost.textContent = easyflowTransferLabelForGhost(option, values);
        document.body.appendChild(ghost);
        return ghost;
      }

      function easyflowTransferMoveGhost(ghost, x, y) {
        if (!ghost) return;
        ghost.style.transform = 'translate(' + (x + 12) + 'px, ' + (y + 12) + 'px)';
      }

      function easyflowTransferSetPointerTarget(target) {
        if (window.easyflowTransferDropTarget && window.easyflowTransferDropTarget !== target && window.easyflowTransferDropTarget.classList) {
          window.easyflowTransferDropTarget.classList.remove('is-drop-target');
        }
        window.easyflowTransferDropTarget = target || null;
        if (target && target.classList) target.classList.add('is-drop-target');
      }

      function easyflowTransferSelectHasValue(select, value) {
        return Array.prototype.some.call(select.options || [], function(option) {
          return option.value === value;
        });
      }

      function easyflowTransferCommitCalculatorSelect(select) {
        var state = window.easyflowTransferDragState;
        if (!select || !state || !state.values || state.values.length === 0 || window.easyflowTransferDropSent) return false;
        var value = state.values[0];
        if (!easyflowTransferSelectHasValue(select, value)) return false;
        window.easyflowTransferDropSent = true;
        select.value = value;
        if (window.jQuery) {
          var wrapped = window.jQuery(select);
          if (select.selectize && select.selectize.setValue) {
            select.selectize.setValue(value);
          } else {
            wrapped.trigger('change');
          }
        } else {
          select.dispatchEvent(new Event('input', {bubbles: true}));
          select.dispatchEvent(new Event('change', {bubbles: true}));
        }
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(select.id, value, {priority: 'event'});
        }
        return true;
      }

      function easyflowTransferStartPointerDrag(state, event) {
        state.dragging = true;
        easyflowTransferFocusListbox(state.listbox);
        window.easyflowTransferDragState = easyflowTransferDropPayload(state.listbox, null, state.values);
        window.easyflowTransferDropTarget = null;
        window.easyflowTransferDropSent = false;
        state.option.classList.add('is-dragging');
        state.listbox.classList.add('is-drag-source');
        state.ghost = easyflowTransferCreateGhost(state.option, state.values);
        easyflowTransferMoveGhost(state.ghost, event.clientX, event.clientY);
        document.body.classList.add('analysis-transfer-dragging');
      }

      function easyflowTransferCleanupPointerDrag() {
        document.querySelectorAll('.analysis-transfer-option.is-dragging').forEach(function(option) {
          option.classList.remove('is-dragging');
        });
        document.querySelectorAll('.analysis-transfer-listbox.is-drag-source, .analysis-transfer-listbox.is-drop-target').forEach(function(listbox) {
          listbox.classList.remove('is-drag-source');
          listbox.classList.remove('is-drop-target');
        });
        if (easyflowPointerTransfer && easyflowPointerTransfer.ghost && easyflowPointerTransfer.ghost.parentNode) {
          easyflowPointerTransfer.ghost.parentNode.removeChild(easyflowPointerTransfer.ghost);
        }
        document.body.classList.remove('analysis-transfer-dragging');
        easyflowPointerTransfer = null;
        window.easyflowTransferDragState = null;
        window.easyflowTransferDropTarget = null;
        window.easyflowTransferDropSent = false;
      }

      document.addEventListener('pointerdown', function(event) {
        if (event.button !== 0) return;
        var option = event.target && event.target.closest ? event.target.closest('.analysis-transfer-option') : null;
        var listbox = option && option.closest ? option.closest('.analysis-transfer-listbox') : null;
        if (!option || !listbox) return;
        var values = easyflowTransferDragValues(option, listbox);
        if (values.length === 0) return;
        easyflowPointerTransfer = {
          pointerId: event.pointerId,
          option: option,
          listbox: listbox,
          values: values,
          startX: event.clientX,
          startY: event.clientY,
          ghost: null,
          dragging: false
        };
      }, true);

      document.addEventListener('pointermove', function(event) {
        var state = easyflowPointerTransfer;
        if (!state || state.pointerId !== event.pointerId) return;
        var dx = event.clientX - state.startX;
        var dy = event.clientY - state.startY;
        if (!state.dragging && Math.sqrt(dx * dx + dy * dy) < 6) return;
        if (!state.dragging) {
          easyflowTransferStartPointerDrag(state, event);
        }
        event.preventDefault();
        event.stopPropagation();
        easyflowTransferMoveGhost(state.ghost, event.clientX, event.clientY);
        easyflowTransferSetPointerTarget(easyflowTransferListboxFromEvent(event) || easyflowTransferCalculatorSelectFromEvent(event));
      }, true);

      document.addEventListener('pointerup', function(event) {
        var state = easyflowPointerTransfer;
        if (!state || state.pointerId !== event.pointerId) return;
        if (state.dragging) {
          event.preventDefault();
          event.stopPropagation();
          var target = window.easyflowTransferDropTarget || easyflowTransferListboxFromEvent(event) || easyflowTransferCalculatorSelectFromEvent(event);
          if (target && target.tagName && target.tagName.toLowerCase() === 'select') {
            easyflowTransferCommitCalculatorSelect(target);
          } else {
            easyflowTransferCommitDrop(target);
          }
          window.easyflowTransferSuppressClickUntil = Date.now() + 500;
        }
        easyflowTransferCleanupPointerDrag();
      }, true);

      document.addEventListener('pointercancel', function(event) {
        var state = easyflowPointerTransfer;
        if (!state || state.pointerId !== event.pointerId) return;
        easyflowTransferCleanupPointerDrag();
      }, true);

      function easyflowTransferCommitDrop(target) {
        var state = window.easyflowTransferDragState;
        if (!target || !state || window.easyflowTransferDropSent) return false;
        target.classList.remove('is-drop-target');
        var source = easyflowFindTransferListboxByInputId(state.source);
        var payload = easyflowTransferDropPayload(source, target, state.values || []);
        if (!payload.source || !payload.target || payload.values.length === 0 || payload.source === payload.target) {
          return false;
        }
        window.easyflowTransferDropSent = true;
        easyflowTransferMarkActive(target);
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('analysis_transfer_drop', payload, {priority: 'event'});
        } else if (window.Shiny && Shiny.onInputChange) {
          Shiny.onInputChange('analysis_transfer_drop', payload);
        }
        return true;
      }

      window.easyflowTransferListboxDragOver = function(event, listbox) {
        listbox = listbox || easyflowTransferListboxFromEvent(event);
        if (!listbox) return true;
        if (event) {
          event.preventDefault();
          event.stopPropagation();
          if (event.dataTransfer) event.dataTransfer.dropEffect = 'move';
        }
        window.easyflowTransferDropTarget = listbox;
        listbox.classList.add('is-drop-target');
        return false;
      };

      window.easyflowTransferListboxDrop = function(event, listbox) {
        listbox = listbox || easyflowTransferListboxFromEvent(event) || window.easyflowTransferDropTarget;
        if (event) {
          event.preventDefault();
          event.stopPropagation();
        }
        if (!window.easyflowTransferDragState && event && event.dataTransfer) {
          try {
            var data = event.dataTransfer.getData('application/x-easyflow-transfer');
            if (data) window.easyflowTransferDragState = JSON.parse(data);
          } catch (error) {
            window.easyflowTransferDragState = null;
          }
        }
        if (!window.easyflowTransferDragState) return false;
        easyflowTransferCommitDrop(listbox);
        return false;
      };

      document.addEventListener('dragstart', function(event) {
        var option = event.target && event.target.closest ? event.target.closest('.analysis-transfer-option') : null;
        var listbox = option && option.closest ? option.closest('.analysis-transfer-listbox') : null;
        if (!option || !listbox) return;
        easyflowTransferFocusListbox(listbox);
        var values = easyflowTransferDragValues(option, listbox);
        if (values.length === 0) return;
        window.easyflowTransferDragState = easyflowTransferDropPayload(listbox, null, values);
        window.easyflowTransferDropTarget = null;
        window.easyflowTransferDropSent = false;
        if (event.dataTransfer) {
          event.dataTransfer.effectAllowed = 'move';
          event.dataTransfer.setData('text/plain', values.join('\n'));
          event.dataTransfer.setData('application/x-easyflow-transfer', JSON.stringify(window.easyflowTransferDragState));
        }
        option.classList.add('is-dragging');
        listbox.classList.add('is-drag-source');
      }, true);

      document.addEventListener('dragend', function(event) {
        document.querySelectorAll('.analysis-transfer-option.is-dragging').forEach(function(option) {
          option.classList.remove('is-dragging');
        });
        document.querySelectorAll('.analysis-transfer-listbox.is-drag-source, .analysis-transfer-listbox.is-drop-target').forEach(function(listbox) {
          listbox.classList.remove('is-drag-source');
          listbox.classList.remove('is-drop-target');
        });
        if (window.easyflowTransferDragState && !window.easyflowTransferDropSent && window.easyflowTransferDropTarget) {
          easyflowTransferCommitDrop(window.easyflowTransferDropTarget);
        }
        window.easyflowTransferDragState = null;
        window.easyflowTransferDropTarget = null;
        window.easyflowTransferDropSent = false;
      }, true);

      document.addEventListener('dragover', function(event) {
        var listbox = easyflowTransferListboxFromEvent(event);
        if (!listbox || !window.easyflowTransferDragState) return;
        window.easyflowTransferListboxDragOver(event, listbox);
      }, true);

      document.addEventListener('dragleave', function(event) {
        var listbox = easyflowTransferListboxFromEvent(event);
        if (!listbox) return;
        var related = event.relatedTarget;
        if (related && listbox.contains(related)) return;
        listbox.classList.remove('is-drop-target');
      }, true);

      document.addEventListener('drop', function(event) {
        var target = easyflowTransferListboxFromEvent(event) || window.easyflowTransferDropTarget;
        if (!target || !window.easyflowTransferDragState) return;
        window.easyflowTransferListboxDrop(event, target);
      }, true);

      ['mousedown', 'focusin'].forEach(function(eventName) {
        document.addEventListener(eventName, function(event) {
          var listbox = event.target && event.target.closest ? event.target.closest('.analysis-transfer-listbox') : null;
          if (listbox) easyflowTransferMarkActive(listbox);
        }, true);
      });

      easyflowDecorateTransferDragOptions(document);
      document.addEventListener('shiny:bound', function(event) {
        easyflowDecorateTransferDragOptions(event.target || document);
      });
      if (window.MutationObserver && document.body) {
        new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            mutation.addedNodes && Array.prototype.forEach.call(mutation.addedNodes, function(node) {
              if (node.nodeType === 1) easyflowDecorateTransferDragOptions(node);
            });
          });
        }).observe(document.body, {childList: true, subtree: true});
      }

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
        window.easyflowRefreshMeasurementControls(document);
        window.easyflowDecorateMeasurementTerms(document);
        if (!window.MutationObserver || !document.body) return;
        var observer = new MutationObserver(function() {
          easyflowUpdateMoveButtonClasses();
          window.easyflowRefreshMeasurementControls(document);
          window.easyflowDecorateMeasurementTerms(document);
        });
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
        window.easyflowUpdateMeasurementControl(select);

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
            container.querySelectorAll('input, select, textarea, button').forEach(function(input) {
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
          var surveyMethod = selectedRadioValue('ttest_anova_survey_normality_method') || 'skew_kurtosis';
          setTtestNormalityDisabled('.ttest-normality-study-options', !enabled);
          setTtestNormalityDisabled('.ttest-normality-survey-branch', !enabled || studyType !== 'survey');
          setTtestNormalityDisabled('.ttest-normality-experimental-branch', !enabled || studyType !== 'experimental');
          setTtestNormalityDisabled('.ttest-skew-kurtosis-cutoff-options', !enabled || studyType !== 'survey' || surveyMethod !== 'skew_kurtosis');
        }

        function scheduleTtestNormalityTreeUpdate() {
          window.setTimeout(updateTtestNormalityTree, 0);
        }

        function updateFactorNormalityOptions() {
          var assumption = document.getElementById('factor_assumption');
          if (!assumption) return;
          setTtestNormalityDisabled('.factor-method-group', assumption.value !== 'none');
        }

        function updateAncovaNormalityOptions() {
          var normality = document.getElementById('ancova_normality_enabled');
          var method = document.getElementById('ancova_normality_method');
          if (!normality || !method) return;
          setTtestNormalityDisabled('.ancova-normality-method-block', !normality.checked);
        }

        function scheduleFactorNormalityOptionsUpdate() {
          window.setTimeout(updateFactorNormalityOptions, 0);
        }

        function scheduleAncovaNormalityOptionsUpdate() {
          window.setTimeout(updateAncovaNormalityOptions, 0);
        }

        document.addEventListener('change', function(event) {
          var target = event.target;
          if (!target || !target.matches) return;
          if (
            target.matches('#ttest_anova_normality_enabled') ||
            target.matches('input[name="ttest_anova_normality_study_type"]') ||
            target.matches('input[name="ttest_anova_survey_normality_method"]')
          ) {
            if (target.matches('#ttest_anova_normality_enabled') && target.checked) {
              resetTtestNormalityDefaults();
            }
            scheduleTtestNormalityTreeUpdate();
          }
          if (target.matches('#factor_assumption')) {
            scheduleFactorNormalityOptionsUpdate();
          }
          if (target.matches('#ancova_normality_enabled')) {
            scheduleAncovaNormalityOptionsUpdate();
          }
        }, true);
        document.addEventListener('click', function(event) {
          var target = event.target;
          if (target && target.matches && target.matches('#ancova_normality_enabled')) {
            scheduleAncovaNormalityOptionsUpdate();
          }
        }, true);

        document.addEventListener('shiny:value', scheduleTtestNormalityTreeUpdate);
        document.addEventListener('shiny:bound', scheduleTtestNormalityTreeUpdate);
        document.addEventListener('shiny:connected', scheduleTtestNormalityTreeUpdate);
        document.addEventListener('shiny:value', function(event) {
          scheduleEasyflowTypesetMath(event.target || document);
        });
        document.addEventListener('shiny:bound', function(event) {
          scheduleEasyflowTypesetMath(event.target || document);
        });
        document.addEventListener('shiny:value', scheduleFactorNormalityOptionsUpdate);
        document.addEventListener('shiny:bound', scheduleFactorNormalityOptionsUpdate);
        document.addEventListener('shiny:connected', scheduleFactorNormalityOptionsUpdate);
        document.addEventListener('shiny:value', scheduleAncovaNormalityOptionsUpdate);
        document.addEventListener('shiny:bound', scheduleAncovaNormalityOptionsUpdate);
        document.addEventListener('shiny:connected', scheduleAncovaNormalityOptionsUpdate);
        if (window.MutationObserver) {
          new MutationObserver(function() {
            scheduleTtestNormalityTreeUpdate();
            scheduleFactorNormalityOptionsUpdate();
            scheduleAncovaNormalityOptionsUpdate();
            scheduleEasyflowTypesetMath(document);
          }).observe(document.documentElement, {
            childList: true,
            subtree: true
          });
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', function() {
            scheduleTtestNormalityTreeUpdate();
            scheduleFactorNormalityOptionsUpdate();
            scheduleAncovaNormalityOptionsUpdate();
          });
        } else {
          scheduleTtestNormalityTreeUpdate();
          scheduleFactorNormalityOptionsUpdate();
          scheduleAncovaNormalityOptionsUpdate();
        }
        scheduleEasyflowTypesetMath(document);
        window.easyflowStartMathJaxPolling();
        window.easyflowUpdateTtestNormalityTree = updateTtestNormalityTree;
        window.easyflowUpdateFactorNormalityOptions = updateFactorNormalityOptions;
        window.easyflowUpdateAncovaNormalityOptions = updateAncovaNormalityOptions;
      })();

      (function() {
        function parseSortValue(text, type) {
          var value = (text || '').replace(/\s+/g, ' ').trim();
          if (type !== 'numeric') return value.toLocaleLowerCase();
          var cleaned = value
            .replace(/[<>,]/g, '')
            .replace(/^\./, '0.')
            .replace(/^-\./, '-0.');
          var parsed = parseFloat(cleaned);
          return Number.isFinite(parsed) ? parsed : Number.NEGATIVE_INFINITY;
        }

        function updateSortIndicators(table, activeButton, direction) {
          table.querySelectorAll('.ancova-sort-button').forEach(function(button) {
            var indicator = button.querySelector('.ancova-sort-indicator');
            var isActive = button === activeButton;
            button.classList.toggle('is-active', isActive);
            button.setAttribute('aria-sort', isActive ? (direction === 'asc' ? 'ascending' : 'descending') : 'none');
            if (indicator) {
              indicator.textContent = isActive ? (direction === 'asc' ? '\u25b4' : '\u25be') : '\u25be';
            }
          });
        }

        function sortAncovaDiagnosticsTable(button) {
          var table = button.closest('table');
          var tbody = table ? table.querySelector('tbody') : null;
          if (!tbody) return;
          var column = parseInt(button.getAttribute('data-sort-column') || '0', 10) - 1;
          if (column < 0) return;
          var type = button.getAttribute('data-sort-type') || 'text';
          var previousColumn = table.getAttribute('data-sort-column') || '';
          var previousDirection = table.getAttribute('data-sort-direction') || '';
          var defaultDirection = button.getAttribute('data-sort-default') || 'asc';
          var direction = previousColumn === String(column) && previousDirection === defaultDirection
            ? (defaultDirection === 'asc' ? 'desc' : 'asc')
            : defaultDirection;
          var rows = Array.prototype.slice.call(tbody.querySelectorAll('tr'));
          rows.sort(function(a, b) {
            var aCell = a.children[column];
            var bCell = b.children[column];
            var aValue = parseSortValue(aCell ? aCell.textContent : '', type);
            var bValue = parseSortValue(bCell ? bCell.textContent : '', type);
            if (aValue < bValue) return direction === 'asc' ? -1 : 1;
            if (aValue > bValue) return direction === 'asc' ? 1 : -1;
            return 0;
          });
          rows.forEach(function(row) {
            tbody.appendChild(row);
          });
          table.setAttribute('data-sort-column', String(column));
          table.setAttribute('data-sort-direction', direction);
          updateSortIndicators(table, button, direction);
        }
        window.easyflowSortAncovaDiagnosticsTable = sortAncovaDiagnosticsTable;

        document.addEventListener('click', function(event) {
          var button = event.target && event.target.closest ? event.target.closest('.ancova-sort-button') : null;
          if (!button) return;
          event.preventDefault();
          sortAncovaDiagnosticsTable(button);
        });
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
