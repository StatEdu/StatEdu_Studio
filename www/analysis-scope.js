(function () {
  'use strict';
  var snapshots = window.stateduSplitSnapshots = Object.create(null);
  var running = false;
  var canvasScopes = window.stateduCanvasScopes = Object.create(null);
  ['click','pointerdown','keydown','change'].forEach(function(type) {
    document.addEventListener(type, function(event) {
      if (!running || !event.isTrusted || event.target.closest('#statedu-analysis-scope-status')) return;
      if (event.target.closest('input,select,textarea,button,a,.custom-model-paper,.selectize-control')) {
        event.preventDefault(); event.stopImmediatePropagation();
      }
    }, true);
  });
  document.addEventListener('click', function(event) {
    var button = event.target.closest('button[id], a[id]');
    if (!button || !/^save_/.test(button.id)) return;
    var format = button.id.match(/_(html|pdf|excel|word|hwpx|figures?)_dialog$/);
    if (!format) return;
    var panel = button.closest('.tab-pane');
    var id = Object.keys(snapshots).find(function(key) { var root = document.getElementById(key); return root && panel && panel.contains(root); });
    if (!id) return;
    event.preventDefault(); event.stopImmediatePropagation();
    Shiny.setInputValue('scope_save', {outputId:id,format:/^figure/.test(format[1])?'figures':format[1],nonce:Date.now()}, {priority:'event'});
  }, true);
  function mount(id) {
    var root = document.getElementById(id);
    if (!root || !snapshots[id]) return;
    root.innerHTML = snapshots[id];
  }
  function register() {
    if (!window.Shiny || window.stateduScopeRegistered) return;
    window.stateduScopeRegistered = true;
    Shiny.addCustomMessageHandler('statedu-analysis-scope-start', function (message) {
      delete snapshots[message.outputId];
      if (message.canvasRootId) {
        canvasScopes[message.canvasRootId] = {entries: [], current: message.label ? {key: 'cases', label: message.label} : null};
      }
      running = !!message.running;
      var stop = document.getElementById('statedu-scope-stop'); if (stop) stop.hidden = !running;
    });
    Shiny.addCustomMessageHandler('statedu-analysis-scope-group', function (message) {
      if (!message.canvasRootId) return;
      var scope = canvasScopes[message.canvasRootId] || (canvasScopes[message.canvasRootId] = {entries: []});
      scope.current = {key: message.key, label: message.label};
    });
    Shiny.addCustomMessageHandler('statedu-analysis-scope-finish', function (message) {
      snapshots[message.outputId] = message.html;
      running = false;
      var stop = document.getElementById('statedu-scope-stop'); if (stop) stop.hidden = true;
      mount(message.outputId);
      var scope = canvasScopes[message.canvasRootId];
      if (scope) {
        scope.current = null;
        var api = window.StatEduModelCanvas;
        if (scope.entries.length && api && api.bridge) {
          api.bridge.applyResult({rootId: message.canvasRootId, source: scope.source,
            results: scope.entries, result: scope.entries[0].result,
            activeResultGroupKey: scope.entries[0].key, show: true});
        }
      }
    });
    Shiny.addCustomMessageHandler('statedu-analysis-scope-status', function (message) {
      var status = document.getElementById('statedu-analysis-scope-status');
      if (!status) {
        status = document.createElement('div'); status.id = 'statedu-analysis-scope-status';
        status.setAttribute('role', 'status');
        status.style.cssText = 'padding:6px 20px;background:#eef6fc;color:#183650;';
        var nav = document.querySelector('.navbar');
        if (nav) nav.insertAdjacentElement('afterend', status);
        else document.body.prepend(status);
      }
      status.replaceChildren(document.createTextNode(message.text || ''));
      status.hidden = !message.active && !running;
      var stop = document.createElement('button'); stop.id = 'statedu-scope-stop'; stop.hidden = !running;
      stop.textContent = /[가-힣]/.test(message.text || '') ? '현재 집단 완료 후 중단' : 'Stop after current group';
      stop.style.marginLeft = '12px';
      stop.onclick = function() { Shiny.setInputValue('scope_cancel',Date.now(),{priority:'event'}); };
      status.appendChild(stop);
    });
    if (window.jQuery) jQuery(document).on('shiny:value.stateduScope', function (event) {
      var outputId = event.name;
      window.setTimeout(function () {
        var updated = document.getElementById(outputId);
        Object.keys(snapshots).forEach(function (id) {
          var root = document.getElementById(id);
          if (id === outputId || (updated && root && updated.contains(root))) mount(id);
        });
      }, 0);
    });
  }
  document.addEventListener('shiny:connected', register); register();
}());
