(function() {
  "use strict";

  var mountCache = window.StatEduModelCanvasMountCache = window.StatEduModelCanvasMountCache || {};

  function rootCacheKey(root) {
    if (!root) return "";
    return root.id || root.getAttribute("data-input-prefix") || "custom_model_canvas";
  }

  function cacheInstance(instance, sourceSnapshot) {
    if (!instance || !instance.root || !window.StatEduModelCanvas || !window.StatEduModelCanvas.state) return;
    var key = rootCacheKey(instance.root);
    if (!key) return;
    var source = sourceSnapshot || instance.sourceSnapshot || window.StatEduModelCanvas.state.snapshot(instance.state);
    mountCache[key] = {
      source: source ? window.StatEduModelCanvas.state.clone(source) : null,
      result: instance.resultSnapshot ? window.StatEduModelCanvas.state.clone(instance.resultSnapshot) : null,
      results: instance.resultSnapshots ? window.StatEduModelCanvas.state.clone(instance.resultSnapshots) : null,
      activeResultGroupKey: instance.activeResultGroupKey || "overall",
      viewingResult: !!(window.StatEduModelCanvas.nodes && window.StatEduModelCanvas.nodes.isViewingResult(instance))
    };
  }

  function cachedStateForRoot(root) {
    var cached = mountCache[rootCacheKey(root)];
    if (!cached || !window.StatEduModelCanvas || !window.StatEduModelCanvas.state) return null;
    return window.StatEduModelCanvas.state.clone(cached);
  }

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

  function syncVisualEdits(targetSnapshot, editedSnapshot, preserveTopology) {
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
    if (!preserveTopology) {
      source.nodes = syncCollectionMembership(source.nodes, editedSnapshot.nodes);
      source.edges = syncCollectionMembership(source.edges, editedSnapshot.edges);
      source.moderations = syncCollectionMembership(source.moderations, editedSnapshot.moderations);
    }

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

  function activeResultEntry(instance) {
    var entries = Array.isArray(instance && instance.resultSnapshots) ? instance.resultSnapshots : [];
    var key = String(instance && instance.activeResultGroupKey || "overall");
    return entries.find(function(entry) { return String(entry.key || "") === key; }) || entries[0] || null;
  }

  function syncAllResultSnapshots(instance, editedSnapshot) {
    if (!instance || !editedSnapshot || !Array.isArray(instance.resultSnapshots)) return;
    instance.resultSnapshots = instance.resultSnapshots.map(function(entry) {
      var next = window.StatEduModelCanvas.state.clone(entry);
      next.result = syncVisualEdits(next.result, editedSnapshot);
      return next;
    });
    var active = activeResultEntry(instance);
    instance.resultSnapshot = active && active.result ? window.StatEduModelCanvas.state.clone(active.result) : instance.resultSnapshot;
  }

  function updateResultGroupControl(instance) {
    if (!instance || !instance.root) return;
    var control = instance.root.querySelector(".custom-model-result-group-control");
    var select = instance.root.querySelector(".custom-model-result-group-select");
    if (!control || !select) return;
    var entries = Array.isArray(instance.resultSnapshots) ? instance.resultSnapshots : [];
    control.classList.toggle("is-available", entries.length > 1 || entries.some(function(entry) { return !!entry.scopeGroup; }));
    select.textContent = "";
    entries.forEach(function(entry) {
      var option = document.createElement("option");
      option.value = String(entry.key || "");
      option.textContent = String(entry.label || entry.key || "");
      option.selected = option.value === String(instance.activeResultGroupKey || "overall");
      select.appendChild(option);
    });
    if (!select.__stateduResultGroupBound) {
      select.__stateduResultGroupBound = true;
      select.addEventListener("change", function() {
        setActiveResultGroup(instance.root.__stateduModelCanvas || instance, select.value);
      });
    }
  }

  function setActiveResultGroup(instance, key) {
    if (!instance) return;
    if (window.StatEduModelCanvas.nodes && window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
      syncAllResultSnapshots(instance, window.StatEduModelCanvas.state.snapshot(instance.state));
    }
    var entries = Array.isArray(instance.resultSnapshots) ? instance.resultSnapshots : [];
    var selected = entries.find(function(entry) { return String(entry.key || "") === String(key || ""); });
    if (!selected || !selected.result) return;
    instance.activeResultGroupKey = String(selected.key || "overall");
    instance.resultSnapshot = window.StatEduModelCanvas.state.clone(selected.result);
    if (window.StatEduModelCanvas.nodes && window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
      window.StatEduModelCanvas.state.restore(instance.state, instance.resultSnapshot);
      instance.state.mode = "select";
      instance.state.selectedNodeId = null;
      instance.state.selectedNodeIds = [];
      instance.state.selectedEdgeId = null;
      instance.state.selectedModerationId = null;
      window.StatEduModelCanvas.canvas.render(instance);
    }
    updateResultGroupControl(instance);
    cacheInstance(instance, instance.sourceSnapshot);
  }

  function invalidateResultAfterModelEdit(instance) {
    if (!instance || !instance.resultSnapshot) return;
    var api = window.StatEduModelCanvas;
    var edited = api.state.snapshot(instance.state);
    var source = syncVisualEdits(instance.sourceSnapshot, edited) || edited;
    // Undo restores model topology, without reviving coefficients from the old fit.
    if (instance.sourceSnapshot) {
      instance.state.history = instance.state.history.map(function(snap) { return syncVisualEdits(instance.sourceSnapshot, snap); });
      instance.state.redoStack = instance.state.redoStack.map(function(snap) { return syncVisualEdits(instance.sourceSnapshot, snap); });
    }
    api.state.restore(instance.state, source);
    instance.state.mode = "select";
    instance.viewingResult = false;
    instance.resultSnapshot = null;
    instance.resultSnapshots = [];
    instance.sourceSnapshot = api.state.snapshot(instance.state);
    instance.root.classList.remove("is-viewing-result", "has-result");
    updateResultGroupControl(instance);
    cacheInstance(instance, instance.sourceSnapshot);
  }

  function sendState(instance) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    if (window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.stripPlsResidualNodes) {
      window.StatEduModelCanvas.canvas.stripPlsResidualNodes(instance);
    }
    var payload = window.StatEduModelCanvas.state.snapshot(instance.state);
    if (window.StatEduModelCanvas.nodes && window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
      syncAllResultSnapshots(instance, payload);
      instance.resultSnapshot = window.StatEduModelCanvas.state.clone(payload);
      instance.sourceSnapshot = syncVisualEdits(instance.sourceSnapshot, payload);
      payload = window.StatEduModelCanvas.state.clone(instance.sourceSnapshot || payload);
    } else if (instance.resultSnapshot) {
      instance.sourceSnapshot = window.StatEduModelCanvas.state.clone(payload);
      syncAllResultSnapshots(instance, payload);
      instance.resultSnapshot = syncVisualEdits(instance.resultSnapshot, payload);
    }
    cacheInstance(instance, payload);
    payload.nonce = Date.now() + Math.random();
    Shiny.setInputValue((instance.root.getAttribute("data-input-prefix") || "custom_model_canvas") + "_state", payload, {priority: "event"});
  }

  function run(instance) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    if (window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.stripPlsResidualNodes) {
      window.StatEduModelCanvas.canvas.stripPlsResidualNodes(instance);
    }
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

  function normalizedAnalysisType(instance) {
    var value = String(instance && instance.analysisType || "").toLowerCase();
    if (value === "sem") value = "cbsem";
    return value || "custom_mm";
  }

  function resultFileInfo(instance) {
    var type = normalizedAnalysisType(instance);
    var info = {
      custom_mm: {extension: "stmmr", prefix: "mediation-moderation-result"},
      cfa: {extension: "stcfar", prefix: "cfa-result"},
      cbsem: {extension: "stsemr", prefix: "sem-result"},
      plssem: {extension: "stplsr", prefix: "pls-sem-result"}
    }[type] || {extension: "stresult", prefix: "analysis-result"};
    var now = new Date();
    var stamp = [
      now.getFullYear(),
      String(now.getMonth() + 1).padStart(2, "0"),
      String(now.getDate()).padStart(2, "0"),
      "-",
      String(now.getHours()).padStart(2, "0"),
      String(now.getMinutes()).padStart(2, "0")
    ].join("");
    info.suggestedName = info.prefix + "-" + stamp + "." + info.extension;
    return info;
  }

  function resultSnapshots(instance) {
    var current = window.StatEduModelCanvas.state.snapshot(instance.state);
    var viewing = window.StatEduModelCanvas.nodes && window.StatEduModelCanvas.nodes.isViewingResult(instance);
    if (viewing) syncAllResultSnapshots(instance, current);
    var snapshots = {
      source: window.StatEduModelCanvas.state.clone(viewing ? (instance.sourceSnapshot || current) : current),
      result: instance.resultSnapshot ? window.StatEduModelCanvas.state.clone(viewing ? current : instance.resultSnapshot) : null,
      results: instance.resultSnapshots ? window.StatEduModelCanvas.state.clone(instance.resultSnapshots) : null,
      activeResultGroupKey: instance.activeResultGroupKey || "overall"
    };
    if (window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.sanitizePlsSnapshot) {
      snapshots.source = window.StatEduModelCanvas.canvas.sanitizePlsSnapshot(instance, snapshots.source);
      snapshots.result = window.StatEduModelCanvas.canvas.sanitizePlsSnapshot(instance, snapshots.result);
    }
    return snapshots;
  }

  async function requestResultFile(instance, action) {
    if (!instance || !instance.root || !window.Shiny || typeof Shiny.setInputValue !== "function") return;
    if (action === "save" && !instance.resultSnapshot) return;
    var info = resultFileInfo(instance);
    var path = "";
    var desktop = window.stateduDesktopFiles;
    try {
      if (desktop) {
        var selected = action === "save" && typeof desktop.chooseResultSave === "function"
          ? await desktop.chooseResultSave({
              title: instance.language === "ko" ? "분석 결과 저장" : "Save analysis result",
              description: "StatEdu Analysis Result",
              extensions: [info.extension],
              suggestedName: info.suggestedName
            })
          : action === "load" && typeof desktop.chooseResultOpen === "function"
            ? await desktop.chooseResultOpen({
                title: instance.language === "ko" ? "분석 결과 불러오기" : "Open analysis result",
                description: "StatEdu Analysis Result",
                extensions: [info.extension]
              })
            : null;
        if (selected && selected.canceled) return;
        path = selected && selected.filePath ? selected.filePath : "";
      }
    } catch (error) {
      window.alert(error && error.message ? error.message : String(error));
      return;
    }
    var payload = {path: path, nonce: Date.now() + Math.random()};
    if (action === "save") {
      var snapshots = resultSnapshots(instance);
      payload.source = snapshots.source;
      payload.result = snapshots.result;
      payload.results = snapshots.results;
      payload.activeResultGroupKey = snapshots.activeResultGroupKey;
    }
    Shiny.setInputValue(
      (instance.root.getAttribute("data-input-prefix") || "custom_model_canvas") + "_result_" + action + "_request",
      payload,
      {priority: "event"}
    );
  }

  function notifyModelReplaced(instance, reason) {
    if (!instance || !instance.root || !window.Shiny || typeof Shiny.setInputValue !== "function") return;
    Shiny.setInputValue(
      (instance.root.getAttribute("data-input-prefix") || "custom_model_canvas") + "_model_replaced",
      {reason: reason || "replace", nonce: Date.now() + Math.random()},
      {priority: "event"}
    );
  }

  function clearInstanceContext(root) {
    if (!root) return;
    var key = rootCacheKey(root);
    if (key && Object.prototype.hasOwnProperty.call(mountCache, key)) delete mountCache[key];
    var instance = root.__stateduModelCanvas;
    if (!instance || !instance.state) return;
    instance.sourceSnapshot = null;
    instance.resultSnapshot = null;
    instance.resultSnapshots = null;
    instance.activeResultGroupKey = "overall";
    instance.viewingResult = false;
    root.classList.remove("is-viewing-result", "has-result");
    instance.state.nodes = [];
    instance.state.edges = [];
    instance.state.moderations = [];
    instance.state.covariates = [];
    instance.state.covariateTypes = {};
    instance.state.covariateTargets = {};
    instance.state.selectedNodeId = null;
    instance.state.selectedNodeIds = [];
    instance.state.selectedEdgeId = null;
    instance.state.selectedModerationId = null;
    instance.state.history = [];
    instance.state.redoStack = [];
    instance.state.mode = "select";
    if (window.StatEduModelCanvas.canvas) window.StatEduModelCanvas.canvas.render(instance);
  }

  function clearContext(message) {
    var rootIds = message && Array.isArray(message.rootIds) ? message.rootIds : [];
    rootIds.forEach(function(rootId) {
      var root = document.getElementById(rootId);
      if (root) clearInstanceContext(root);
      else if (Object.prototype.hasOwnProperty.call(mountCache, rootId)) delete mountCache[rootId];
    });
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
    var incomingResults = message && Array.isArray(message.results)
      ? window.StatEduModelCanvas.state.clone(message.results)
      : [];
    if (window.StatEduModelCanvas.canvas.sanitizePlsSnapshot) {
      incomingSource = window.StatEduModelCanvas.canvas.sanitizePlsSnapshot(instance, incomingSource);
      incomingResult = window.StatEduModelCanvas.canvas.sanitizePlsSnapshot(instance, incomingResult);
      incomingResults.forEach(function(entry) {
        entry.result = window.StatEduModelCanvas.canvas.sanitizePlsSnapshot(instance, entry.result);
      });
    }
    // A newly fitted group owns its topology. The previous (possibly transient
    // empty) view supplies layout only, not node/path additions or deletions.
    if (incomingSource && instance.sourceSnapshot) incomingSource = syncVisualEdits(incomingSource, instance.sourceSnapshot, true);
    if (incomingResult && currentResult) incomingResult = syncVisualEdits(incomingResult, currentResult, true);
    if (!incomingResults.length && incomingResult) {
      incomingResults = [{key: "overall", label: instance.language === "ko" ? "전체" : "Overall", result: incomingResult}];
    }
    var scope = window.stateduCanvasScopes && window.stateduCanvasScopes[root.id];
    if (scope && scope.current) {
      incomingResults.forEach(function(entry) {
        var innerKey = String(entry.key || "overall");
        entry.key = scope.current.key + ":" + innerKey;
        entry.label = scope.current.label + (innerKey === "overall" ? "" : " · " + entry.label);
        entry.scopeGroup = true;
      });
    }
    if (currentResult) {
      incomingResults = incomingResults.map(function(entry) {
        entry.result = syncVisualEdits(entry.result, currentResult, true);
        return entry;
      });
    }
    if (scope && scope.current) {
      var groupPrefix = scope.current.key + ":";
      scope.entries = (scope.entries || []).filter(function(entry) { return String(entry.key).indexOf(groupPrefix) !== 0; }).concat(incomingResults);
      scope.source = incomingSource;
    }
    var requestedKey = message && message.activeResultGroupKey ? message.activeResultGroupKey : null;
    var previousKey = String(requestedKey || instance.activeResultGroupKey || "overall");
    instance.sourceSnapshot = incomingSource;
    instance.resultSnapshots = incomingResults;
    instance.activeResultGroupKey = incomingResults.some(function(entry) { return String(entry.key || "") === previousKey; })
      ? previousKey : (incomingResults[0] ? incomingResults[0].key : "overall");
    var active = activeResultEntry(instance);
    instance.resultSnapshot = active && active.result ? window.StatEduModelCanvas.state.clone(active.result) : incomingResult;
    cacheInstance(instance, incomingSource);
    root.classList.toggle("has-result", !!instance.resultSnapshot);
    updateResultGroupControl(instance);
    if (window.StatEduModelCanvas.toolbar && window.StatEduModelCanvas.toolbar.updateButtons) {
      window.StatEduModelCanvas.toolbar.updateButtons(instance);
    }
    if (message && message.show && instance.resultSnapshot) {
      if (root.classList.contains("structural-equation-canvas-root")) {
        window.StatEduModelCanvas.canvas.showResult(instance);
      } else if (
        root.querySelector('.custom-model-toolbar-panel[data-toolbar-panel="result"]') &&
        window.StatEduModelCanvas.toolbar &&
        window.StatEduModelCanvas.toolbar.setActiveGroup
      ) {
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
    Shiny.addCustomMessageHandler("custom-model-canvas-reset-context", clearContext);
    Shiny.addCustomMessageHandler("custom-model-canvas-score", function(message) {
      var root = document.getElementById(message.rootId);
      var instance = root && root.__stateduModelCanvas;
      var api = window.StatEduModelCanvas;
      if (!instance || !instance.scoreRequest || instance.scoreRequest.token !== message.token) return;
      if (instance.scoreRequest.base !== JSON.stringify(api.state.snapshot(instance.state))) {
        window.alert(api.state.label(instance, "score_model_changed", "The model changed. Reopen the original-item editor."));
        instance.scoreRequest = null;
        return;
      }
      api.state.pushHistory(instance);
      api.state.restore(instance.state, message.snapshot);
      instance.resultSnapshot = null;
      instance.resultSnapshots = [];
      instance.viewingResult = false;
      if (message.layoutTarget) api.canvas.reflowMeasurements(instance, [message.layoutTarget]);
      instance.sourceSnapshot = api.state.snapshot(instance.state);
      instance.scoreRequest = null;
      root.classList.remove("has-result", "is-viewing-result");
      api.canvas.render(instance);
      sendState(instance);
    });
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.bridge = {
    sendState: sendState,
    invalidateResultAfterModelEdit: invalidateResultAfterModelEdit,
    cacheInstance: cacheInstance,
    cachedStateForRoot: cachedStateForRoot,
    syncVisualEdits: syncVisualEdits,
    syncAllResultSnapshots: syncAllResultSnapshots,
    updateResultGroupControl: updateResultGroupControl,
    setActiveResultGroup: setActiveResultGroup,
    run: run,
    runConfirm: runConfirm,
    requestResultFile: requestResultFile,
    notifyModelReplaced: notifyModelReplaced,
    clearInstanceContext: clearInstanceContext,
    applyResult: applyResult
  };

  bindHandlers();
  document.addEventListener("shiny:connected", bindHandlers);
})();
