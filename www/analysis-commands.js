(function () {
  'use strict';
  window.stateduOpenCommand = function (id, button) {
    var root = button.closest('.custom-model-canvas-root') || button.closest('.workspace-panel') || button.closest('.tab-pane');
    if (!root || !window.Shiny || !window.jQuery) return;
    var options = Array.from(root.querySelectorAll('.shiny-bound-input')).filter(function (el) {
      var binding = window.jQuery(el).data('shiny-input-binding');
      return el.id && binding && typeof binding.receiveMessage === 'function' &&
        !el.matches('button, a, input[type=file], .shiny-input-file') &&
        !el.closest('.analysis-data-viewer, .dataTables_wrapper') &&
        !/(?:_available|_selected|_active|_selection|_request|_move|_reset|_save|_download)(?:_|$)/.test(el.id);
    }).map(function (el) { return el.id; });
    Shiny.setInputValue(id + '_open_regression_syntax', { options: options, nonce: Date.now() }, { priority: 'event' });
  };
  function register() {
    if (!window.Shiny || !window.jQuery) return;
    Shiny.addCustomMessageHandler('statedu-command-canvas', function (message) {
      var root = document.getElementById(message.rootId);
      var instance = root && root.__stateduModelCanvas;
      var api = window.StatEduModelCanvas;
      if (!instance || !api || !message.snapshot) return;
      api.bridge.clearInstanceContext(root);
      api.state.restore(instance.state, message.snapshot);
      instance.sourceSnapshot = api.state.clone(message.snapshot);
      api.bridge.cacheInstance(instance, instance.sourceSnapshot);
      api.canvas.render(instance);
    });
    Shiny.addCustomMessageHandler('statedu-command-options', async function (message) {
      var errors = [];
      for (var id of Object.keys(message.options || {})) {
        var el = document.getElementById(id);
        var binding = el && window.jQuery(el).data('shiny-input-binding');
        if (!binding || typeof binding.receiveMessage !== 'function') {
          errors.push(id);
          continue;
        }
        var value = message.options[id];
        try {
          await binding.receiveMessage(el, { value: value, selected: value });
        } catch (_) { errors.push(id); }
      }
      Shiny.setInputValue(message.id + '_command_ack', {
        token: message.token, errors: errors, nonce: Date.now()
      }, { priority: 'event' });
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', register);
  else register();
})();
