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
        var roleLabel = window.StatEduModelCanvas.state.roleLabel(instance, role);
        if (role === "covariate") {
          var covariateType = (instance.state.covariateTypes || {})[name];
          if (covariateType && covariateType.encoding) roleLabel += " · " + covariateType.encoding;
        }
        item.setAttribute("data-role-label", roleLabel);
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

  function createLatentNode(instance, x, y) {
    var style = instance.state.style;
    var sequence = instance.state.nodes.filter(function(node) { return node.role === "latent"; }).length + 1;
    return {
      id: "latent_" + Date.now() + "_" + sequence,
      variableId: "",
      name: "eta" + sequence,
      dataLabel: "잠재변수 " + sequence,
      canvasLabel: "",
      role: "latent",
      nodeType: "latent",
      constructType: "commonFactor",
      weightingMode: "auto",
      x: x,
      y: y,
      width: 90,
      height: 44,
      fontSize: 13,
      customFontSize: true,
      fontFamily: style.fontFamily
    };
  }

  function createIndicatorNode(instance, variable, x, y) {
    var node = createNodeFromVariable(instance, variable, x, y, "indicator");
    node.nodeType = "indicator";
    node.width = 82;
    node.height = 30;
    node.fontSize = 12;
    node.customFontSize = true;
    return node;
  }

  function createErrorNode(instance, indicator, x, y, sequence) {
    return {
      id: "error_" + indicator.id + "_" + sequence,
      variableId: "",
      name: "e" + sequence,
      dataLabel: "e" + sequence,
      canvasLabel: "",
      autoErrorNotation: true,
      role: "error",
      nodeType: "error",
      x: x,
      y: y,
      width: 26,
      height: 26,
      fontSize: 12,
      customFontSize: true,
      fontFamily: instance.state.style.fontFamily
    };
  }

  function createDisturbanceNode(instance, latent, sequence) {
    return {
      id: "disturbance_" + latent.id,
      variableId: "",
      name: "zeta" + sequence,
      dataLabel: "ζ" + sequence,
      canvasLabel: "",
      role: "disturbance",
      nodeType: "disturbance",
      x: Number(latent.x || 0),
      y: Number(latent.y || 0),
      width: 26,
      height: 26,
      fontSize: 12,
      customFontSize: true,
      fontFamily: instance.state.style.fontFamily
    };
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
      var validationItems = instance.validation && instance.validation.byNode ? (instance.validation.byNode[node.id] || []) : [];
      if (validationItems.length) {
        var hasError = validationItems.some(function(item) { return item.level === "error"; });
        element.className += hasError ? " has-validation-error" : " has-validation-warning";
        element.setAttribute("data-validation-message", validationItems.map(function(item) { return item.message; }).join("\n"));
      }
      element.setAttribute("data-node-id", node.id);
      element.setAttribute("title", window.StatEduModelCanvas.layout.displayText(node));
      element.style.left = node.x + "px";
      element.style.top = node.y + "px";
      element.style.width = (node.width || instance.state.style.boxWidth) + "px";
      element.style.height = (node.height || instance.state.style.boxHeight) + "px";
      element.style.fontSize = (node.customFontSize ? node.fontSize : instance.state.style.fontSize) + "px";
      element.style.fontFamily = node.fontFamily || instance.state.style.fontFamily;
      element.style.backgroundColor = node.fillColor && node.fillColor !== "transparent" ? node.fillColor : "transparent";
      element.style.borderColor = node.strokeColor || instance.state.style.boxStrokeColor || "#000000";
      element.style.borderWidth = Number(instance.state.style.boxStrokeWidth || 1.5) + "px";

      var label = document.createElement("div");
      label.className = "custom-model-node-label";
      label.textContent = window.StatEduModelCanvas.layout.displayText(node);
      element.appendChild(label);
      if (validationItems.length) {
        var badge = document.createElement("span");
        badge.className = "structural-validation-badge";
        badge.textContent = validationItems.some(function(item) { return item.level === "error"; }) ? "!" : "△";
        badge.title = validationItems.map(function(item) { return item.message; }).join("\n");
        element.appendChild(badge);
      }
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
    var target = nodeById(instance, nodeId);
    if (!target) return;
    var removeIds = [nodeId];
    if (target.role === "indicator") {
      instance.state.edges.forEach(function(edge) {
        var source = nodeById(instance, edge.from);
        if (edge.to === target.id && source && source.role === "error") removeIds.push(source.id);
      });
    }
    if (target.role === "latent") {
      var indicators = instance.state.edges.map(function(edge) {
        var from = nodeById(instance, edge.from);
        var to = nodeById(instance, edge.to);
        if (from && from.id === target.id && to && to.role === "indicator") return to;
        if (to && to.id === target.id && from && from.role === "indicator") return from;
        return null;
      }).filter(Boolean);
      indicators.forEach(function(indicator) {
        removeIds.push(indicator.id);
        instance.state.edges.forEach(function(edge) {
          var source = nodeById(instance, edge.from);
          if (edge.to === indicator.id && source && source.role === "error") removeIds.push(source.id);
        });
      });
      instance.state.edges.forEach(function(edge) {
        var source = nodeById(instance, edge.from);
        if (edge.to === target.id && source && source.role === "disturbance") removeIds.push(source.id);
      });
    }
    removeIds = removeIds.filter(function(id, index) { return removeIds.indexOf(id) === index; });
    var removedEdgeIds = instance.state.edges
      .filter(function(edge) { return removeIds.indexOf(edge.from) >= 0 || removeIds.indexOf(edge.to) >= 0; })
      .map(function(edge) { return edge.id; });
    instance.state.nodes = instance.state.nodes.filter(function(node) {
      return removeIds.indexOf(node.id) < 0;
    });
    instance.state.edges = instance.state.edges.filter(function(edge) {
      return removeIds.indexOf(edge.from) < 0 && removeIds.indexOf(edge.to) < 0;
    });
    instance.state.moderations = instance.state.moderations.filter(function(moderation) {
      return moderation.from !== nodeId && removedEdgeIds.indexOf(moderation.toEdge) < 0;
    });
    instance.state.selectedNodeIds = (instance.state.selectedNodeIds || []).filter(function(id) { return removeIds.indexOf(id) < 0; });
    if (removeIds.indexOf(instance.state.selectedNodeId) >= 0) instance.state.selectedNodeId = null;
    hideProperties(instance);
    if (window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.reflowMeasurements) {
      window.StatEduModelCanvas.canvas.reflowMeasurements(instance);
    }
  }

  function errorPlacement(instance, errorNode) {
    if (!errorNode || errorNode.role !== "error") return null;
    var errorEdge = instance.state.edges.find(function(edge) { return edge.from === errorNode.id; });
    var indicator = errorEdge ? nodeById(instance, errorEdge.to) : null;
    if (!indicator) return null;
    var measurementEdge = instance.state.edges.find(function(edge) {
      if (edge.from !== indicator.id && edge.to !== indicator.id) return false;
      var other = nodeById(instance, edge.from === indicator.id ? edge.to : edge.from);
      return other && other.role === "latent";
    });
    var latent = measurementEdge ? nodeById(instance, measurementEdge.from === indicator.id ? measurementEdge.to : measurementEdge.from) : null;
    if (!latent) return null;
    return {
      indicator: indicator,
      placement: latent.effectiveMeasurementPlacement || latent.measurementPlacement || "left"
    };
  }

  function errorMovementAxis(instance, errorNode) {
    var placementInfo = errorPlacement(instance, errorNode);
    if (!placementInfo) return null;
    return placementInfo.placement === "left" || placementInfo.placement === "right" ? "x" : "y";
  }

  function captureErrorOffset(instance, errorNode) {
    var placementInfo = errorPlacement(instance, errorNode);
    if (!placementInfo) return false;
    var indicator = placementInfo.indicator;
    var placement = placementInfo.placement;
    var iw = Number(indicator.width || instance.state.style.boxWidth || 110);
    var ih = Number(indicator.height || instance.state.style.boxHeight || 38);
    var ew = Number(errorNode.width || 26);
    var eh = Number(errorNode.height || 26);
    var defaultPosition;
    if (placement === "left") {
      defaultPosition = Number(indicator.x || 0) - ew - 28;
      errorNode.manualOffset = defaultPosition - Number(errorNode.x || 0);
    } else if (placement === "right") {
      defaultPosition = Number(indicator.x || 0) + iw + 28;
      errorNode.manualOffset = Number(errorNode.x || 0) - defaultPosition;
    } else if (placement === "top") {
      defaultPosition = Number(indicator.y || 0) - eh - 28;
      errorNode.manualOffset = defaultPosition - Number(errorNode.y || 0);
    } else {
      defaultPosition = Number(indicator.y || 0) + ih + 28;
      errorNode.manualOffset = Number(errorNode.y || 0) - defaultPosition;
    }
    errorNode.manualPosition = true;
    return true;
  }

  function disturbanceMovement(instance, disturbanceNode) {
    if (!disturbanceNode || disturbanceNode.role !== "disturbance") return null;
    var edge = instance.state.edges.find(function(item) { return item.from === disturbanceNode.id; });
    var latent = edge ? nodeById(instance, edge.to) : null;
    if (!latent) return null;
    var placement = disturbanceNode.disturbancePosition || "auto";
    if (placement === "auto") {
      var measurementPlacement = latent.effectiveMeasurementPlacement || latent.measurementPlacement || "left";
      placement = measurementPlacement === "top" ? "s" : "n";
    }
    var diagonal = Math.sqrt(0.5);
    var vectors = {
      nw: {x: -diagonal, y: -diagonal}, n: {x: 0, y: -1}, ne: {x: diagonal, y: -diagonal},
      w: {x: -1, y: 0}, e: {x: 1, y: 0},
      sw: {x: -diagonal, y: diagonal}, s: {x: 0, y: 1}, se: {x: diagonal, y: diagonal}
    };
    var vector = vectors[placement] || vectors.n;
    var latentX = Number(latent.x || 0);
    var latentY = Number(latent.y || 0);
    var latentWidth = Number(latent.width || 90);
    var latentHeight = Number(latent.height || 44);
    var width = Number(disturbanceNode.width || 26);
    var height = Number(disturbanceNode.height || 26);
    var bases = {
      nw: {x: latentX - width - 38, y: latentY - height - 38},
      n: {x: latentX + latentWidth / 2 - width / 2, y: latentY - height - 38},
      ne: {x: latentX + latentWidth + 38, y: latentY - height - 38},
      e: {x: latentX + latentWidth + 38, y: latentY + latentHeight / 2 - height / 2},
      se: {x: latentX + latentWidth + 38, y: latentY + latentHeight + 38},
      s: {x: latentX + latentWidth / 2 - width / 2, y: latentY + latentHeight + 38},
      sw: {x: latentX - width - 38, y: latentY + latentHeight + 38},
      w: {x: latentX - width - 38, y: latentY + latentHeight / 2 - height / 2}
    };
    return {placement: placement, vector: vector, base: bases[placement] || bases.n};
  }

  function captureDisturbanceOffset(instance, disturbanceNode) {
    var movement = disturbanceMovement(instance, disturbanceNode);
    if (!movement) return false;
    disturbanceNode.manualOffset =
      (Number(disturbanceNode.x || 0) - movement.base.x) * movement.vector.x +
      (Number(disturbanceNode.y || 0) - movement.base.y) * movement.vector.y;
    return true;
  }

  function startNodeDrag(instance, event, element) {
    if (instance.state.mode !== "select") return;
    var node = nodeById(instance, element.getAttribute("data-node-id"));
    if (!node) return;
    event.preventDefault();
    window.StatEduModelCanvas.state.pushHistory(instance);
    var startX = event.clientX;
    var startY = event.clientY;
    var moved = false;
    var dragThreshold = 4;
    var selectedIds = (instance.state.selectedNodeIds || []).indexOf(node.id) >= 0 ?
      (instance.state.selectedNodeIds || []).slice() :
      [node.id];
    var entireResultSelection = instance.viewingResult &&
      selectedIds.length > 0 && selectedIds.length === instance.state.nodes.length;
    var draggedNodes = instance.state.nodes.filter(function(item) {
      return selectedIds.indexOf(item.id) >= 0;
    });
    var movingEntireResultModel = entireResultSelection &&
      draggedNodes.length === instance.state.nodes.length;
    var initialPositions = draggedNodes.map(function(item) {
      return {node: item, x: Number(item.x || 0), y: Number(item.y || 0)};
    });

    function move(moveEvent) {
      var dragScale = instance.analysisType ? Number(instance.state.canvas.modelZoom || 1) : Number(instance.state.canvas.zoom || 1);
      var dx = (moveEvent.clientX - startX) / dragScale;
      var dy = (moveEvent.clientY - startY) / dragScale;
      if (!moved && Math.sqrt(dx * dx + dy * dy) < dragThreshold) return;
      moved = true;
      initialPositions.forEach(function(item) {
        var axis = instance.viewingResult ? (movingEntireResultModel ? null : "x") : errorMovementAxis(instance, item.node);
        var disturbanceMove = instance.viewingResult ? null : disturbanceMovement(instance, item.node);
        if (disturbanceMove) {
          var projected = dx * disturbanceMove.vector.x + dy * disturbanceMove.vector.y;
          item.node.x = item.x + projected * disturbanceMove.vector.x;
          item.node.y = item.y + projected * disturbanceMove.vector.y;
        } else {
          item.node.x = item.x + (axis === "y" ? 0 : dx);
          item.node.y = item.y + (axis === "x" ? 0 : dy);
        }
      });
      renderNodes(instance);
      window.StatEduModelCanvas.edges.render(instance);
    }

    function up(upEvent) {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      if (!moved) {
        instance.state.history.pop();
        return;
      }
      if (instance.viewingResult) {
        window.StatEduModelCanvas.bridge.sendState(instance);
        return;
      }
      var movedError = false;
      var movedDisturbance = false;
      initialPositions.forEach(function(item) {
        if (captureErrorOffset(instance, item.node)) movedError = true;
        if (captureDisturbanceOffset(instance, item.node)) movedDisturbance = true;
      });
      if (node.role === "indicator" && initialPositions.length === 1 && upEvent && upEvent.target && upEvent.target.closest) {
        var targetElement = upEvent.target.closest(".custom-model-node-latent");
        var targetLatent = targetElement ? nodeById(instance, targetElement.getAttribute("data-node-id")) : null;
        if (targetLatent) {
          instance.state.edges = instance.state.edges.filter(function(edge) {
            var from = nodeById(instance, edge.from);
            var to = nodeById(instance, edge.to);
            return !((from && from.role === "latent" && to && to.id === node.id) || (to && to.role === "latent" && from && from.id === node.id));
          });
          var formative = (targetLatent.measurementMode || "reflective") === "formative";
          window.StatEduModelCanvas.edges.createEdge(instance, formative ? node.id : targetLatent.id, formative ? targetLatent.id : node.id);
          if (window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.reflowMeasurements) window.StatEduModelCanvas.canvas.reflowMeasurements(instance);
          renderNodes(instance);
          window.StatEduModelCanvas.edges.render(instance);
          window.StatEduModelCanvas.bridge.sendState(instance);
          return;
        }
      }
      if (instance.state.autoAlign !== false && initialPositions.length === 1 && node.role !== "error" && node.role !== "disturbance") {
        window.StatEduModelCanvas.layout.roleAutoAlignPosition(node, instance.state.nodes, window.StatEduModelCanvas.layout.AUTO_ALIGN_THRESHOLD, node.id);
        renderNodes(instance);
        window.StatEduModelCanvas.edges.render(instance);
      }
      if (node.role === "latent" && window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.reflowMeasurements) {
        window.StatEduModelCanvas.canvas.reflowMeasurements(instance);
        renderNodes(instance);
        window.StatEduModelCanvas.edges.render(instance);
      }
      if ((movedError || movedDisturbance) && window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.reflowMeasurements) {
        window.StatEduModelCanvas.canvas.reflowMeasurements(instance);
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
    if (["latent", "indicator", "error"].indexOf(node.role) >= 0) {
      roleSelect.appendChild(option(node.role, window.StatEduModelCanvas.state.roleLabel(instance, node.role)));
      roleSelect.disabled = true;
    }
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
      var fixedRole = ["latent", "indicator", "error"].indexOf(node.role) >= 0;
      var nextRole = fixedRole ? node.role : (roleSelect.value || "independent");
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
      node.customFontSize = node.fontSize !== Number(instance.state.style.fontSize || 11);
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
    var ko = instance.language === "ko";
    var panel = document.createElement("div");
    panel.className = "custom-model-property-popover structural-edge-property-popover";
    panel.innerHTML = [
      '<div class="custom-model-property-title">' + (ko ? (edge.kind === "covariance" ? "공분산 속성" : "경로 속성") : "Path properties") + '</div>',
      '<label class="custom-model-property-label">' + (ko ? "표시 라벨" : "Display label") + '</label>',
      '<input class="form-control edge-property-label" type="text">',
      '<label class="custom-model-property-label">' + (ko ? "모수 이름" : "Parameter name") + '</label>',
      '<input class="form-control edge-property-name" type="text">',
      '<label class="custom-model-property-check"><input class="edge-property-free" type="checkbox"> ' + (ko ? "자유모수" : "Free parameter") + '</label>',
      '<label class="custom-model-property-label">' + (ko ? "고정값" : "Fixed value") + '</label>',
      '<input class="form-control edge-property-fixed" type="number" step="any">',
      '<label class="custom-model-property-label">' + (ko ? "시작값" : "Starting value") + '</label>',
      '<input class="form-control edge-property-start" type="number" step="any">',
      '<label class="custom-model-property-label">' + (ko ? "등가제약 라벨" : "Equality label") + '</label>',
      '<input class="form-control edge-property-equality" type="text">',
      '<div class="custom-model-property-actions">',
      '<button type="button" class="btn btn-primary btn-sm edge-property-apply">' + (ko ? "적용" : "Apply") + '</button>',
      '<button type="button" class="btn btn-default btn-sm edge-property-close">' + (ko ? "닫기" : "Close") + '</button>',
      '</div>'
    ].join("");
    panel.querySelector(".edge-property-label").value = edge.label || "";
    panel.querySelector(".edge-property-name").value = edge.parameterName || "";
    panel.querySelector(".edge-property-free").checked = edge.free !== false;
    panel.querySelector(".edge-property-fixed").value = edge.fixedValue === undefined ? "" : edge.fixedValue;
    panel.querySelector(".edge-property-start").value = edge.startValue === undefined ? "" : edge.startValue;
    panel.querySelector(".edge-property-equality").value = edge.equalityLabel || "";
    panel.querySelector(".edge-property-fixed").disabled = edge.free !== false;
    panel.querySelector(".edge-property-free").addEventListener("change", function(event) {
      panel.querySelector(".edge-property-fixed").disabled = event.target.checked;
    });
    panel.querySelector(".edge-property-close").addEventListener("click", function() { hideProperties(instance); });
    panel.querySelector(".edge-property-apply").addEventListener("click", function() {
      window.StatEduModelCanvas.state.pushHistory(instance);
      edge.label = String(panel.querySelector(".edge-property-label").value || "").trim();
      edge.parameterName = String(panel.querySelector(".edge-property-name").value || "").trim();
      edge.free = panel.querySelector(".edge-property-free").checked;
      var fixed = panel.querySelector(".edge-property-fixed").value;
      var start = panel.querySelector(".edge-property-start").value;
      edge.fixedValue = fixed === "" ? null : Number(fixed);
      edge.startValue = start === "" ? null : Number(start);
      edge.equalityLabel = String(panel.querySelector(".edge-property-equality").value || "").trim();
      hideProperties(instance);
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    });
    panel.style.left = "24px";
    panel.style.top = "24px";
    instance.paper.appendChild(panel);
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.nodes = {
    variableUsed: variableUsed,
    setVariableUsage: setVariableUsage,
    createNodeFromVariable: createNodeFromVariable,
    createLatentNode: createLatentNode,
    createIndicatorNode: createIndicatorNode,
    createErrorNode: createErrorNode,
    createDisturbanceNode: createDisturbanceNode,
    errorMovementAxis: errorMovementAxis,
    captureErrorOffset: captureErrorOffset,
    disturbanceMovement: disturbanceMovement,
    captureDisturbanceOffset: captureDisturbanceOffset,
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
