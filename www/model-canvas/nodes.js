(function() {
  "use strict";

  function variableUsed(instance, name) {
    return instance.state.nodes.some(function(node) {
      return node.variableId === name;
    });
  }

  function setVariableUsage(instance) {
    var used = {};
    var covariates = {};
    instance.state.nodes.forEach(function(node) {
      used[node.variableId] = node.role || "independent";
    });
    instance.state.covariates.forEach(function(name) {
      covariates[name] = true;
    });
    instance.root.querySelectorAll(".custom-model-variable-item").forEach(function(item) {
      var name = item.getAttribute("data-variable-name") || "";
      var role = used[name] || (covariates[name] ? "covariate" : "");
      var isAssigned = !!role;
      var isSelected = (instance.state.selectedVariables || []).indexOf(name) >= 0 || instance.state.selectedVariable === name;
      item.classList.toggle("is-used", isAssigned);
      item.classList.toggle("is-covariate", role === "covariate");
      item.classList.toggle("is-selected", isSelected);
      item.setAttribute("draggable", isAssigned ? "false" : "true");
      item.setAttribute("aria-disabled", isAssigned ? "true" : "false");
      item.setAttribute("aria-selected", isSelected ? "true" : "false");
      if (role) {
        item.setAttribute("data-role", role);
        item.setAttribute("data-role-label", window.StatEduModelCanvas.state.roleLabel(instance, role));
      } else {
        item.removeAttribute("data-role");
        item.removeAttribute("data-role-label");
      }
    });
  }

  function createNodeFromVariable(instance, variable, x, y, role) {
    var style = instance.state.style;
    var box = {
      id: "node_" + variable.name.replace(/[^A-Za-z0-9_]/g, "_") + "_" + Date.now(),
      variableId: variable.name,
      name: variable.name,
      dataLabel: variable.dataLabel || variable.name,
      canvasLabel: "",
      role: role || "independent",
      x: x,
      y: y,
      width: style.boxWidth,
      height: style.boxHeight,
      fontSize: style.fontSize,
      customFontSize: false,
      fontFamily: style.fontFamily
    };
    if (instance.state.autoAlign === false) return box;
    return window.StatEduModelCanvas.layout.roleAutoAlignPosition(box, instance.state.nodes, window.StatEduModelCanvas.layout.AUTO_ALIGN_THRESHOLD);
  }

  function renderNodes(instance) {
    var layer = instance.nodeLayer;
    layer.innerHTML = "";
    var selectedNodeIds = instance.state.selectedNodeIds || [];
    instance.state.nodes.forEach(function(node) {
      var element = document.createElement("div");
      element.className = "custom-model-node custom-model-node-" + node.role;
      if (node.id === instance.state.selectedNodeId || selectedNodeIds.indexOf(node.id) >= 0) {
        element.className += " is-selected";
      }
      element.setAttribute("data-node-id", node.id);
      element.setAttribute("title", window.StatEduModelCanvas.layout.displayText(node));
      element.style.left = node.x + "px";
      element.style.top = node.y + "px";
      element.style.width = (node.width || instance.state.style.boxWidth) + "px";
      element.style.height = (node.height || instance.state.style.boxHeight) + "px";
      element.style.fontSize = (node.customFontSize ? node.fontSize : instance.state.style.fontSize) + "px";
      element.style.fontFamily = node.fontFamily || instance.state.style.fontFamily;
      element.style.borderColor = instance.state.style.boxStrokeColor || "#000000";
      element.style.borderWidth = Number(instance.state.style.boxStrokeWidth || 1.5) + "px";

      var label = document.createElement("div");
      label.className = "custom-model-node-label";
      label.textContent = window.StatEduModelCanvas.layout.displayText(node);
      element.appendChild(label);
      layer.appendChild(element);
    });
    setVariableUsage(instance);
  }

  function nodeById(instance, id) {
    return instance.state.nodes.find(function(node) {
      return node.id === id;
    });
  }

  function deleteNode(instance, nodeId) {
    var removedEdgeIds = instance.state.edges
      .filter(function(edge) { return edge.from === nodeId || edge.to === nodeId; })
      .map(function(edge) { return edge.id; });
    instance.state.nodes = instance.state.nodes.filter(function(node) {
      return node.id !== nodeId;
    });
    instance.state.edges = instance.state.edges.filter(function(edge) {
      return edge.from !== nodeId && edge.to !== nodeId;
    });
    instance.state.moderations = instance.state.moderations.filter(function(moderation) {
      return moderation.from !== nodeId && removedEdgeIds.indexOf(moderation.toEdge) < 0;
    });
    if (instance.state.selectedNodeId === nodeId) instance.state.selectedNodeId = null;
    hideProperties(instance);
  }

  function startNodeDrag(instance, event, element) {
    if (instance.state.mode !== "select") return;
    var node = nodeById(instance, element.getAttribute("data-node-id"));
    if (!node) return;
    event.preventDefault();
    window.StatEduModelCanvas.state.pushHistory(instance);
    var startX = event.clientX;
    var startY = event.clientY;
    var selectedIds = (instance.state.selectedNodeIds || []).indexOf(node.id) >= 0 ?
      (instance.state.selectedNodeIds || []).slice() :
      [node.id];
    var draggedNodes = instance.state.nodes.filter(function(item) {
      return selectedIds.indexOf(item.id) >= 0;
    });
    var initialPositions = draggedNodes.map(function(item) {
      return {node: item, x: Number(item.x || 0), y: Number(item.y || 0)};
    });

    function move(moveEvent) {
      var dx = (moveEvent.clientX - startX) / instance.state.canvas.zoom;
      var dy = (moveEvent.clientY - startY) / instance.state.canvas.zoom;
      initialPositions.forEach(function(item) {
        item.node.x = item.x + dx;
        item.node.y = item.y + dy;
      });
      renderNodes(instance);
      window.StatEduModelCanvas.edges.render(instance);
    }

    function up() {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      if (instance.state.autoAlign !== false && initialPositions.length === 1) {
        window.StatEduModelCanvas.layout.roleAutoAlignPosition(node, instance.state.nodes, window.StatEduModelCanvas.layout.AUTO_ALIGN_THRESHOLD, node.id);
        renderNodes(instance);
        window.StatEduModelCanvas.edges.render(instance);
      }
      window.StatEduModelCanvas.bridge.sendState(instance);
    }

    document.addEventListener("pointermove", move, true);
    document.addEventListener("pointerup", up, true);
  }

  function hideProperties(instance) {
    if (!instance || !instance.paper) return;
    var panel = instance.paper.querySelector(".custom-model-property-popover");
    if (panel) panel.remove();
  }

  function clampPanelPosition(instance, x, y, width, height) {
    var canvasWidth = Number(instance.state.canvas.widthPx || 0);
    var canvasHeight = Number(instance.state.canvas.heightPx || 0);
    return {
      x: Math.max(8, Math.min(canvasWidth - width - 8, x)),
      y: Math.max(8, Math.min(canvasHeight - height - 8, y))
    };
  }

  function option(value, label) {
    var item = document.createElement("option");
    item.value = value;
    item.textContent = label;
    return item;
  }

  function showNodeProperties(instance, nodeId) {
    var node = nodeById(instance, nodeId);
    if (!node) return;
    hideProperties(instance);
    instance.state.selectedNodeId = node.id;
    instance.state.selectedEdgeId = null;
    instance.state.selectedModerationId = null;
    window.StatEduModelCanvas.edges.render(instance);
    renderNodes(instance);

    var panel = document.createElement("div");
    panel.className = "custom-model-property-popover";
    var t = function(key, fallback) {
      return window.StatEduModelCanvas.state.label(instance, key, fallback);
    };
    panel.innerHTML = [
      '<div class="custom-model-property-title">' + t("properties", "\uc18d\uc131") + '</div>',
      '<label class="custom-model-property-label">' + t("variable_name", "\ubcc0\uc218\uba85") + '</label>',
      '<input class="form-control custom-model-property-variable" type="text" readonly>',
      '<label class="custom-model-property-label">' + t("label", "\ub77c\ubca8") + '</label>',
      '<input class="form-control custom-model-property-label-input" type="text">',
      '<label class="custom-model-property-label">' + t("role", "\uc5ed\ud560") + '</label>',
      '<select class="form-control custom-model-property-role"></select>',
      '<label class="custom-model-property-label">' + t("font_size", "\ud3f0\ud2b8 \ud06c\uae30") + '</label>',
      '<input class="form-control custom-model-property-font-size" type="number" min="8" max="32" step="1">',
      '<div class="custom-model-property-actions">',
      '<button type="button" class="btn btn-primary btn-sm custom-model-property-apply">' + t("apply", "\uc801\uc6a9") + '</button>',
      '<button type="button" class="btn btn-default btn-sm custom-model-property-close">' + t("close", "\ub2eb\uae30") + '</button>',
      '</div>'
    ].join("");

    var roleSelect = panel.querySelector(".custom-model-property-role");
    roleSelect.appendChild(option("independent", window.StatEduModelCanvas.state.roleLabel(instance, "independent")));
    roleSelect.appendChild(option("mediator", window.StatEduModelCanvas.state.roleLabel(instance, "mediator")));
    roleSelect.appendChild(option("moderator", window.StatEduModelCanvas.state.roleLabel(instance, "moderator")));
    roleSelect.appendChild(option("dependent", window.StatEduModelCanvas.state.roleLabel(instance, "dependent")));

    panel.querySelector(".custom-model-property-variable").value = node.name || node.variableId || "";
    panel.querySelector(".custom-model-property-label-input").value = node.canvasLabel || node.dataLabel || "";
    panel.querySelector(".custom-model-property-font-size").value = node.customFontSize ? (node.fontSize || instance.state.style.fontSize) : instance.state.style.fontSize;
    roleSelect.value = node.role || "independent";

    var position = clampPanelPosition(
      instance,
      Number(node.x || 0) + Number(node.width || instance.state.style.boxWidth) + 10,
      Number(node.y || 0),
      230,
      230
    );
    panel.style.left = position.x + "px";
    panel.style.top = position.y + "px";

    panel.querySelector(".custom-model-property-close").addEventListener("click", function() {
      instance.state.selectedNodeId = null;
      hideProperties(instance);
      renderNodes(instance);
    });
    panel.querySelector(".custom-model-property-apply").addEventListener("click", function() {
      window.StatEduModelCanvas.state.pushHistory(instance);
      node.canvasLabel = String(panel.querySelector(".custom-model-property-label-input").value || "").trim();
      var nextRole = roleSelect.value || "independent";
      var roleLimit = window.StatEduModelCanvas.state.ROLE_LIMITS[nextRole];
      var assignedCount = instance.state.nodes.filter(function(item) {
        return item.id !== node.id && item.role === nextRole;
      }).length;
      if (Number.isFinite(roleLimit) && assignedCount >= roleLimit) {
        window.alert(window.StatEduModelCanvas.state.formatLabel(instance, "role_limit", "%s can currently be selected only once.", window.StatEduModelCanvas.state.roleLabel(instance, nextRole)));
        instance.state.history.pop();
        return;
      }
      node.role = nextRole;
      if (nextRole === "dependent") {
        var dependentPosition = window.StatEduModelCanvas.layout.nextRolePosition(
          instance.state.nodes.filter(function(item) { return item.id !== node.id; }),
          nextRole,
          instance.state.style
        );
        node.x = dependentPosition.x;
        node.y = dependentPosition.y;
      }
      if (nextRole !== "moderator") {
        window.StatEduModelCanvas.layout.reflowRoleLayout(instance.state.nodes, instance.state.style);
      }
      var fontSize = Number(panel.querySelector(".custom-model-property-font-size").value || instance.state.style.fontSize);
      node.fontSize = Math.max(8, Math.min(32, fontSize));
      node.customFontSize = node.fontSize !== Number(instance.state.style.fontSize || 13);
      hideProperties(instance);
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    });

    instance.paper.appendChild(panel);
  }

  function showEdgeProperties(instance, edgeId) {
    var edge = window.StatEduModelCanvas.edges.edgeById(instance, edgeId);
    if (!edge) return;
    hideProperties(instance);
    instance.state.selectedNodeId = null;
    instance.state.selectedEdgeId = edge.id;
    instance.state.selectedModerationId = null;
    renderNodes(instance);
    window.StatEduModelCanvas.edges.render(instance);
    window.StatEduModelCanvas.toolbar.updateButtons(instance);
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.nodes = {
    variableUsed: variableUsed,
    setVariableUsage: setVariableUsage,
    createNodeFromVariable: createNodeFromVariable,
    render: renderNodes,
    nodeById: nodeById,
    deleteNode: deleteNode,
    startDrag: startNodeDrag,
    editLabel: showNodeProperties,
    showProperties: showNodeProperties,
    showEdgeProperties: showEdgeProperties,
    hideProperties: hideProperties
  };
})();
