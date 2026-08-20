(function() {
  "use strict";

  function copyFields(target, source, fields) {
    fields.forEach(function(field) {
      if (Object.prototype.hasOwnProperty.call(source, field)) {
        target[field] = window.StatEduModelCanvas.state.clone(source[field]);
      }
    });
  }

  function syncCollectionMembership(targetItems, editedItems) {
    var targets = {};
    (targetItems || []).forEach(function(item) { targets[item.id] = item; });
    return (editedItems || []).map(function(edited) {
      return targets[edited.id] || window.StatEduModelCanvas.state.clone(edited);
    });
  }

  function syncVisualEdits(targetSnapshot, editedSnapshot) {
    if (!targetSnapshot || !editedSnapshot) return targetSnapshot;
    var source = window.StatEduModelCanvas.state.clone(targetSnapshot);
    source.canvas = window.StatEduModelCanvas.state.clone(editedSnapshot.canvas || source.canvas);
    source.style = window.StatEduModelCanvas.state.clone(editedSnapshot.style || source.style);
    source.gridVisible = editedSnapshot.gridVisible !== false;
    source.dashNonsignificant = editedSnapshot.dashNonsignificant !== false;
    source.latentStatsSelection = window.StatEduModelCanvas.state.clone(editedSnapshot.latentStatsSelection || ["r2"]);
    source.showLatentStats = editedSnapshot.showLatentStats !== false;

    // The source and result views are two presentations of one model. Keep
    // their topology in sync while retaining result-only values on matching
    // objects (for example coefficient labels).
    source.nodes = syncCollectionMembership(source.nodes, editedSnapshot.nodes);
    source.edges = syncCollectionMembership(source.edges, editedSnapshot.edges);
    source.moderations = syncCollectionMembership(source.moderations, editedSnapshot.moderations);

    var resultNodes = {};
    (editedSnapshot.nodes || []).forEach(function(node) { resultNodes[node.id] = node; });
    (source.nodes || []).forEach(function(node) {
      var edited = resultNodes[node.id];
      if (!edited) return;
      copyFields(node, edited, [
        "x", "y", "width", "height", "canvasLabel", "fontSize", "customFontSize",
        "resultStatsOffsetX", "resultStatsOffsetY"
      ]);
    });

    var resultEdges = {};
    (editedSnapshot.edges || []).forEach(function(edge) { resultEdges[edge.id] = edge; });
    (source.edges || []).forEach(function(edge) {
      var edited = resultEdges[edge.id];
      if (!edited) return;
      copyFields(edge, edited, [
        "shape", "curveDirection", "curveOffset", "controlPoint",
        "fromSide", "toSide", "fixedCenter", "directAnchors", "labelPosition",
        "labelOffsetX", "labelOffsetY", "labelManualPosition", "labelFontSize", "labelTextAnchor"
      ]);
    });

    var resultModerations = {};
    (editedSnapshot.moderations || []).forEach(function(item) { resultModerations[item.id] = item; });
    (source.moderations || []).forEach(function(item) {
      var edited = resultModerations[item.id];
      if (!edited) return;
      copyFields(item, edited, ["edgePosition", "labelPosition", "labelOffsetX", "labelOffsetY", "labelManualPosition", "labelFontSize"]);
    });
    return source;
  }

  function sendState(instance) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var payload = window.StatEduModelCanvas.state.snapshot(instance.state);
    if (window.StatEduModelCanvas.nodes && window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
      instance.resultSnapshot = window.StatEduModelCanvas.state.clone(payload);
      instance.sourceSnapshot = syncVisualEdits(instance.sourceSnapshot, payload);
      payload = window.StatEduModelCanvas.state.clone(instance.sourceSnapshot || payload);
    } else if (instance.resultSnapshot) {
      instance.sourceSnapshot = window.StatEduModelCanvas.state.clone(payload);
      instance.resultSnapshot = syncVisualEdits(instance.resultSnapshot, payload);
    }
    payload.nonce = Date.now() + Math.random();
    Shiny.setInputValue((instance.root.getAttribute("data-input-prefix") || "custom_model_canvas") + "_state", payload, {priority: "event"});
  }

  function run(instance) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var payload = window.StatEduModelCanvas.state.snapshot(instance.state);
    payload.nonce = Date.now() + Math.random();
    Shiny.setInputValue((instance.root.getAttribute("data-input-prefix") || "custom_model_canvas") + "_run_request", payload, {priority: "event"});
  }

  function runConfirm(instance) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var payload = window.StatEduModelCanvas.state.snapshot(instance.state);
    payload.nonce = Date.now() + Math.random();
    Shiny.setInputValue((instance.root.getAttribute("data-input-prefix") || "custom_model_canvas") + "_run_confirm", payload, {priority: "event"});
  }

  function applyResult(message) {
    var root = message && message.rootId
      ? document.getElementById(message.rootId)
      : document.querySelector(".custom-model-canvas-root");
    if (!root || !window.StatEduModelCanvas || !window.StatEduModelCanvas.canvas) return;
    var instance = window.StatEduModelCanvas.canvas.init(root);
    if (!instance) return;
    var currentResult = null;
    if (window.StatEduModelCanvas.nodes && window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
      currentResult = window.StatEduModelCanvas.state.snapshot(instance.state);
    } else if (instance.resultSnapshot) {
      currentResult = window.StatEduModelCanvas.state.clone(instance.resultSnapshot);
    }
    var incomingSource = message && message.source ? window.StatEduModelCanvas.state.clone(message.source) : null;
    var incomingResult = message && message.result ? window.StatEduModelCanvas.state.clone(message.result) : null;
    if (incomingSource && instance.sourceSnapshot) incomingSource = syncVisualEdits(incomingSource, instance.sourceSnapshot);
    if (incomingResult && currentResult) incomingResult = syncVisualEdits(incomingResult, currentResult);
    instance.sourceSnapshot = incomingSource;
    instance.resultSnapshot = incomingResult;
    root.classList.toggle("has-result", !!instance.resultSnapshot);
    if (window.StatEduModelCanvas.toolbar && window.StatEduModelCanvas.toolbar.updateButtons) {
      window.StatEduModelCanvas.toolbar.updateButtons(instance);
    }
    if (message && message.show && instance.resultSnapshot) {
      if (root.classList.contains("structural-equation-canvas-root")) {
        window.StatEduModelCanvas.canvas.showResult(instance);
      } else if (window.StatEduModelCanvas.toolbar && window.StatEduModelCanvas.toolbar.setActiveGroup) {
        window.StatEduModelCanvas.toolbar.setActiveGroup(instance, "result");
      } else {
        window.StatEduModelCanvas.canvas.showResult(instance);
      }
    }
  }

  function bindHandlers() {
    if (!window.Shiny || typeof Shiny.addCustomMessageHandler !== "function" || window.StatEduModelCanvas.resultHandlerBound) return;
    window.StatEduModelCanvas.resultHandlerBound = true;
    Shiny.addCustomMessageHandler("custom-model-canvas-result", applyResult);
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.bridge = {
    sendState: sendState,
    syncVisualEdits: syncVisualEdits,
    run: run,
    runConfirm: runConfirm,
    applyResult: applyResult
  };

  bindHandlers();
  document.addEventListener("shiny:connected", bindHandlers);
})();
