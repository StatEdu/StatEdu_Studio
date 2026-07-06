(function() {
  "use strict";

  function parseVariables(root) {
    var raw = root.getAttribute("data-variables") || "[]";
    try {
      return JSON.parse(raw);
    } catch (error) {
      var textarea = document.createElement("textarea");
      textarea.innerHTML = raw;
      try {
        return JSON.parse(textarea.value || "[]");
      } catch (innerError) {
        return [];
      }
    }
  }

  function parseI18n(root) {
    var raw = root.getAttribute("data-i18n") || "{}";
    try {
      return JSON.parse(raw);
    } catch (error) {
      var textarea = document.createElement("textarea");
      textarea.innerHTML = raw;
      try {
        return JSON.parse(textarea.value || "{}");
      } catch (innerError) {
        return {};
      }
    }
  }

  function canvasPoint(instance, event) {
    var rect = instance.paper.getBoundingClientRect();
    return {
      x: (event.clientX - rect.left) / instance.state.canvas.zoom,
      y: (event.clientY - rect.top) / instance.state.canvas.zoom
    };
  }

  function variableByName(instance, name) {
    return instance.state.variables.find(function(variable) {
      return variable.name === name;
    });
  }

  function edgeIdFromEvent(instance, event, tolerance) {
    var edgeElement = event.target && event.target.closest ? event.target.closest(".custom-model-edge, .custom-model-edge-hit, .custom-model-edge-control") : null;
    if (edgeElement) return edgeElement.getAttribute("data-edge-id");
    var nearest = window.StatEduModelCanvas.edges.nearestEdgeAt(instance, canvasPoint(instance, event), tolerance || 18);
    return nearest && nearest.edge ? nearest.edge.id : null;
  }

  function render(instance) {
    var paperWidth = Number(instance.state.canvas.widthPx || 0);
    var paperHeight = Number(instance.state.canvas.heightPx || 0);
    var diagramColumnWidth = Math.max(720, paperWidth + 48);
    instance.root.style.setProperty("--custom-model-paper-width", paperWidth + "px");
    instance.root.style.setProperty("--custom-model-paper-height", paperHeight + "px");
    instance.root.style.setProperty("--custom-model-diagram-column-width", diagramColumnWidth + "px");
    instance.paper.style.width = instance.state.canvas.widthPx + "px";
    instance.paper.style.height = instance.state.canvas.heightPx + "px";
    instance.paper.style.transform = "scale(" + instance.state.canvas.zoom + ")";
    instance.paper.style.transformOrigin = "0 0";
    instance.edgeLayer.setAttribute("width", instance.state.canvas.widthPx);
    instance.edgeLayer.setAttribute("height", instance.state.canvas.heightPx);
    window.StatEduModelCanvas.nodes.render(instance);
    window.StatEduModelCanvas.edges.render(instance);
    window.StatEduModelCanvas.toolbar.updateStatus(instance);
    window.StatEduModelCanvas.toolbar.updateButtons(instance);
  }

  function selectedVariableNames(instance) {
    var names = instance.state.selectedVariables || [];
    if (names.length > 0) return names.slice();
    return instance.state.selectedVariable ? [instance.state.selectedVariable] : [];
  }

  function variableItems(instance) {
    return Array.from(instance.root.querySelectorAll(".custom-model-variable-item"));
  }

  function selectVariable(instance, item, event) {
    var name = item ? item.getAttribute("data-variable-name") : "";
    if (!name) return;
    var items = variableItems(instance);
    var index = items.indexOf(item);
    var selected = instance.state.selectedVariables || [];

    if (event && event.shiftKey && instance.state.lastSelectedVariableIndex !== null) {
      var start = Math.min(instance.state.lastSelectedVariableIndex, index);
      var end = Math.max(instance.state.lastSelectedVariableIndex, index);
      selected = items.slice(start, end + 1).map(function(listItem) {
        return listItem.getAttribute("data-variable-name") || "";
      }).filter(Boolean);
    } else if (event && (event.ctrlKey || event.metaKey)) {
      selected = selected.slice();
      var selectedIndex = selected.indexOf(name);
      if (selectedIndex >= 0) {
        selected.splice(selectedIndex, 1);
      } else {
        selected.push(name);
      }
      instance.state.lastSelectedVariableIndex = index;
    } else {
      selected = [name];
      instance.state.lastSelectedVariableIndex = index;
    }

    instance.state.selectedVariables = selected;
    instance.state.selectedVariable = selected.length ? selected[selected.length - 1] : null;
    window.StatEduModelCanvas.nodes.setVariableUsage(instance);
  }

  function existingNodeByVariable(instance, name) {
    return instance.state.nodes.find(function(node) {
      return node.variableId === name;
    });
  }

  function currentVariableRole(instance, name) {
    var node = existingNodeByVariable(instance, name);
    if (node) return node.role || "independent";
    if (instance.state.covariates.indexOf(name) >= 0) return "covariate";
    return "";
  }

  function roleCount(instance, role, excludingName) {
    if (role === "covariate") {
      return instance.state.covariates.filter(function(name) {
        return name !== excludingName;
      }).length;
    }
    return instance.state.nodes.filter(function(node) {
      return node.variableId !== excludingName && node.role === role;
    }).length;
  }

  function canAssignRole(instance, role, variableName) {
    var limit = window.StatEduModelCanvas.state.ROLE_LIMITS[role];
    if (!Number.isFinite(limit)) return true;
    if (roleCount(instance, role, variableName) < limit) return true;
    window.alert((window.StatEduModelCanvas.state.ROLE_LABELS_KO[role] || role) + "\uc740(\ub294) \ud604\uc7ac 1\uac1c\ub9cc \uc120\ud0dd\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.");
    return false;
  }

  function removeVariableRole(instance, name) {
    var node = existingNodeByVariable(instance, name);
    if (node) {
      window.StatEduModelCanvas.nodes.deleteNode(instance, node.id);
    }
    instance.state.covariates = instance.state.covariates.filter(function(item) {
      return item !== name;
    });
  }

  function applyRoleToVariable(instance, name, role) {
    var variable = variableByName(instance, name);
    if (!variable) return false;
    var currentRole = currentVariableRole(instance, name);

    if (currentRole === role) {
      removeVariableRole(instance, name);
      return true;
    }

    if (!canAssignRole(instance, role, name)) return false;

    if (role === "covariate") {
      removeVariableRole(instance, name);
      if (instance.state.covariates.indexOf(name) < 0) {
        instance.state.covariates.push(name);
      }
      return true;
    }

    var existing = existingNodeByVariable(instance, name);
    if (existing) {
      existing.role = role;
      if (role === "dependent") {
        var dependentPosition = window.StatEduModelCanvas.layout.nextRolePosition(
          instance.state.nodes.filter(function(node) { return node.id !== existing.id; }),
          role,
          instance.state.style
        );
        existing.x = dependentPosition.x;
        existing.y = dependentPosition.y;
      }
      window.StatEduModelCanvas.layout.alignDependentToMediators(instance.state.nodes);
      return true;
    }

    var position = window.StatEduModelCanvas.layout.nextRolePosition(instance.state.nodes, role, instance.state.style);
    instance.state.covariates = instance.state.covariates.filter(function(item) {
      return item !== name;
    });
    instance.state.nodes.push(window.StatEduModelCanvas.nodes.createNodeFromVariable(instance, variable, position.x, position.y, role));
    window.StatEduModelCanvas.layout.alignDependentToMediators(instance.state.nodes);
    return true;
  }

  function addSelectedVariableAsRole(instance, role) {
    var names = selectedVariableNames(instance);
    if (names.length === 0) {
      window.alert(window.StatEduModelCanvas.state.label(instance, "select_variable_first", "\ubcc0\uc218\ub97c \uba3c\uc800 \uc120\ud0dd\ud558\uc138\uc694."));
      return;
    }
    window.StatEduModelCanvas.state.pushHistory(instance);
    var changed = false;
    names.forEach(function(name) {
      changed = applyRoleToVariable(instance, name, role) || changed;
    });
    if (!changed) {
      instance.state.history.pop();
      return;
    }
    instance.state.selectedVariables = names;
    instance.state.selectedVariable = names[names.length - 1] || null;
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function bindVariableSelection(instance) {
    instance.root.querySelectorAll(".custom-model-variable-item").forEach(function(item) {
      item.addEventListener("click", function(event) {
        event.preventDefault();
        selectVariable(instance, item, event);
      });
    });
    instance.root.querySelectorAll(".custom-model-role-button").forEach(function(button) {
      button.addEventListener("click", function(event) {
        event.preventDefault();
        addSelectedVariableAsRole(instance, button.getAttribute("data-role") || "independent");
      });
    });
  }

  function bindVariableDrag(instance) {
    instance.root.querySelectorAll(".custom-model-variable-item").forEach(function(item) {
      item.addEventListener("dragstart", function(event) {
        if (item.classList.contains("is-used")) {
          event.preventDefault();
          return;
        }
        event.dataTransfer.setData("text/plain", item.getAttribute("data-variable-name") || "");
        event.dataTransfer.effectAllowed = "copy";
      });
    });

    instance.paper.addEventListener("dragover", function(event) {
      event.preventDefault();
      event.dataTransfer.dropEffect = "copy";
    });

    instance.paper.addEventListener("drop", function(event) {
      event.preventDefault();
      var name = event.dataTransfer.getData("text/plain");
      var variable = variableByName(instance, name);
      if (!variable || window.StatEduModelCanvas.nodes.variableUsed(instance, name)) return;
      window.StatEduModelCanvas.activeInstance = instance;
      var role = window.StatEduModelCanvas.dialogs.chooseRole("independent");
      if (!role) return;
      if (!canAssignRole(instance, role, name)) return;
      var point = canvasPoint(instance, event);
      window.StatEduModelCanvas.state.pushHistory(instance);
      instance.state.nodes.push(window.StatEduModelCanvas.nodes.createNodeFromVariable(instance, variable, point.x, point.y, role));
      instance.state.selectedVariable = name;
      instance.state.selectedVariables = [name];
      render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    });
  }

  function bindCanvasPointer(instance) {
    instance.paper.addEventListener("pointerdown", function(event) {
      var propertyPanel = event.target.closest ? event.target.closest(".custom-model-property-popover") : null;
      if (propertyPanel) return;

      var labelElement = event.target.closest ? event.target.closest(".custom-model-edge-label") : null;
      var nodeElement = event.target.closest ? event.target.closest(".custom-model-node") : null;
      var edgeControlElement = event.target.closest ? event.target.closest(".custom-model-edge-control") : null;
      var edgeElement = event.target.closest ? event.target.closest(".custom-model-edge, .custom-model-edge-hit") : null;
      var moderationElement = event.target.closest ? event.target.closest(".custom-model-moderation") : null;
      var edgeId = edgeIdFromEvent(instance, event, 18);

      if (labelElement) {
        if (instance.state.mode === "properties") {
          event.preventDefault();
          window.StatEduModelCanvas.edges.showLabelProperties(
            instance,
            labelElement.getAttribute("data-label-type"),
            labelElement.getAttribute("data-label-id")
          );
          return;
        }
        window.StatEduModelCanvas.edges.startLabelDrag(
          instance,
          event,
          labelElement.getAttribute("data-label-type"),
          labelElement.getAttribute("data-label-id")
        );
        return;
      }

      if (instance.state.mode === "properties" && edgeControlElement) {
        window.StatEduModelCanvas.edges.startControlDrag(instance, event, edgeControlElement.getAttribute("data-edge-id"));
        return;
      }

      if (instance.state.mode === "delete") {
        if (nodeElement) {
          window.StatEduModelCanvas.state.pushHistory(instance);
          window.StatEduModelCanvas.nodes.deleteNode(instance, nodeElement.getAttribute("data-node-id"));
          render(instance);
          window.StatEduModelCanvas.bridge.sendState(instance);
          return;
        }
        if (moderationElement) {
          window.StatEduModelCanvas.state.pushHistory(instance);
          window.StatEduModelCanvas.edges.deleteModeration(instance, moderationElement.getAttribute("data-moderation-id"));
          render(instance);
          window.StatEduModelCanvas.bridge.sendState(instance);
          return;
        }
        if (edgeId) {
          window.StatEduModelCanvas.state.pushHistory(instance);
          window.StatEduModelCanvas.edges.deleteEdge(instance, edgeId);
          render(instance);
          window.StatEduModelCanvas.bridge.sendState(instance);
        }
        return;
      }

      if (instance.state.mode === "connect" && nodeElement) {
        handleConnectPointer(instance, event, nodeElement);
        return;
      }

      if (instance.state.mode === "properties") {
        if (nodeElement) {
          event.preventDefault();
          window.StatEduModelCanvas.nodes.showProperties(instance, nodeElement.getAttribute("data-node-id"));
          return;
        }
        if (edgeId) {
          event.preventDefault();
          window.StatEduModelCanvas.nodes.showEdgeProperties(instance, edgeId);
          return;
        }
        if (!moderationElement) {
          instance.state.selectedNodeId = null;
          instance.state.selectedEdgeId = null;
          window.StatEduModelCanvas.nodes.hideProperties(instance);
          render(instance);
        }
        return;
      }

      if (nodeElement) {
        instance.state.selectedNodeId = nodeElement.getAttribute("data-node-id");
        instance.state.selectedEdgeId = null;
        window.StatEduModelCanvas.nodes.startDrag(instance, event, nodeElement);
      }
    }, true);

    instance.paper.addEventListener("dblclick", function(event) {
      var propertyPanel = event.target.closest ? event.target.closest(".custom-model-property-popover") : null;
      if (propertyPanel) return;

      var labelElement = event.target.closest ? event.target.closest(".custom-model-edge-label") : null;
      var nodeElement = event.target.closest ? event.target.closest(".custom-model-node") : null;
      var edgeId = edgeIdFromEvent(instance, event, 18);
      if (labelElement) {
        if (instance.state.mode !== "properties") window.StatEduModelCanvas.toolbar.setMode(instance, "properties");
        window.StatEduModelCanvas.edges.showLabelProperties(
          instance,
          labelElement.getAttribute("data-label-type"),
          labelElement.getAttribute("data-label-id")
        );
      } else if (nodeElement) {
        if (instance.state.mode !== "properties") window.StatEduModelCanvas.toolbar.setMode(instance, "properties");
        window.StatEduModelCanvas.nodes.editLabel(instance, nodeElement.getAttribute("data-node-id"));
      } else if (edgeId) {
        if (instance.state.mode !== "properties") window.StatEduModelCanvas.toolbar.setMode(instance, "properties");
        window.StatEduModelCanvas.nodes.showEdgeProperties(instance, edgeId);
      }
    });
  }

  function handleConnectPointer(instance, event, nodeElement) {
    var fromId = nodeElement.getAttribute("data-node-id");
    var fromNode = window.StatEduModelCanvas.nodes.nodeById(instance, fromId);
    if (!fromNode) return;
    event.preventDefault();

    var fromCenter = window.StatEduModelCanvas.layout.nodeCenter(fromNode, instance.state.style);
    var point = canvasPoint(instance, event);
    instance.state.dragPreview = {x1: fromCenter.x, y1: fromCenter.y, x2: point.x, y2: point.y};
    window.StatEduModelCanvas.edges.render(instance);

    function move(moveEvent) {
      var next = canvasPoint(instance, moveEvent);
      instance.state.dragPreview = {x1: fromCenter.x, y1: fromCenter.y, x2: next.x, y2: next.y};
      window.StatEduModelCanvas.edges.render(instance);
    }

    function up(upEvent) {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      document.removeEventListener("pointercancel", cancel, true);
      instance.state.dragPreview = null;

      var targetNodeElement = upEvent.target && upEvent.target.closest ? upEvent.target.closest(".custom-model-node") : null;
      var changed = false;
      window.StatEduModelCanvas.state.pushHistory(instance);
      if (targetNodeElement) {
        changed = window.StatEduModelCanvas.edges.createEdge(instance, fromId, targetNodeElement.getAttribute("data-node-id"));
      } else {
        var targetPoint = canvasPoint(instance, upEvent);
        var nearest = window.StatEduModelCanvas.edges.nearestEdgeAt(instance, targetPoint, 24);
        if (nearest) {
          changed = window.StatEduModelCanvas.edges.createModeration(instance, fromId, nearest.edge.id, nearest.percent);
        }
      }
      if (!changed) {
        instance.state.history.pop();
      }
      render(instance);
      if (changed) window.StatEduModelCanvas.bridge.sendState(instance);
    }

    function cancel() {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      document.removeEventListener("pointercancel", cancel, true);
      instance.state.dragPreview = null;
      window.StatEduModelCanvas.edges.render(instance);
    }

    document.addEventListener("pointermove", move, true);
    document.addEventListener("pointerup", up, true);
    document.addEventListener("pointercancel", cancel, true);
  }

  function zoom(instance, factor) {
    var next = Math.max(0.5, Math.min(2, instance.state.canvas.zoom * factor));
    instance.state.canvas.zoom = next;
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function fit(instance) {
    instance.state.canvas.zoom = 1;
    render(instance);
    instance.root.querySelector(".custom-model-canvas-scroll").scrollTo({left: 0, top: 0});
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function showResult(instance) {
    if (!instance || !instance.resultSnapshot) {
      window.alert(window.StatEduModelCanvas.state.label(instance, "result_unavailable", "\uacb0\uacfc \uadf8\ub9bc\uc740 \ubd84\uc11d \uc2e4\ud589 \ud6c4\uc5d0 \ud655\uc778\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4."));
      return;
    }
    window.StatEduModelCanvas.state.restore(instance.state, instance.resultSnapshot);
    instance.state.mode = "properties";
    instance.viewingResult = true;
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function showSource(instance) {
    if (!instance || !instance.sourceSnapshot || !instance.viewingResult) return;
    window.StatEduModelCanvas.state.restore(instance.state, instance.sourceSnapshot);
    instance.state.mode = "select";
    instance.viewingResult = false;
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function init(root) {
    if (!root || root.__stateduModelCanvas) return root && root.__stateduModelCanvas;
    var instance = {
      root: root,
      state: window.StatEduModelCanvas.state.create(),
      paper: root.querySelector(".custom-model-paper"),
      edgeLayer: root.querySelector(".custom-model-edge-layer"),
      nodeLayer: root.querySelector(".custom-model-node-layer")
    };
    instance.state.variables = parseVariables(root);
    instance.language = root.getAttribute("data-language") || "ko";
    instance.i18n = parseI18n(root);
    root.__stateduModelCanvas = instance;
    window.StatEduModelCanvas.toolbar.bind(instance);
    bindVariableSelection(instance);
    bindVariableDrag(instance);
    bindCanvasPointer(instance);
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    return instance;
  }

  function initAll() {
    document.querySelectorAll(".custom-model-canvas-root").forEach(init);
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.canvas = {
    init: init,
    initAll: initAll,
    render: render,
    canvasPoint: canvasPoint,
    showResult: showResult,
    showSource: showSource,
    zoom: zoom,
    fit: fit
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initAll);
  } else {
    initAll();
  }
  document.addEventListener("shiny:value", function(event) {
    if (
      event.target &&
      (
        (event.target.matches && event.target.matches(".custom-model-canvas-root")) ||
        (event.target.querySelector && event.target.querySelector(".custom-model-canvas-root"))
      )
    ) {
      window.setTimeout(initAll, 0);
    }
  });
  if (window.MutationObserver && document.documentElement) {
    new MutationObserver(function(mutations) {
      var shouldInit = mutations.some(function(mutation) {
        return Array.prototype.some.call(mutation.addedNodes || [], function(node) {
          return node.nodeType === 1 && (
            (node.matches && node.matches(".custom-model-canvas-root")) ||
            (node.querySelector && node.querySelector(".custom-model-canvas-root"))
          );
        });
      });
      if (shouldInit) window.setTimeout(initAll, 0);
    }).observe(document.documentElement, {childList: true, subtree: true});
  }
})();
