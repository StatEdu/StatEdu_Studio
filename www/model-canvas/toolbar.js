(function() {
  "use strict";

  function modeLabel(instance, mode) {
    if (mode === "addObserved") return window.StatEduModelCanvas.state.label(instance, "mode_add_observed", instance.language === "ko" ? "모드: 캔버스를 클릭하여 측정변수 배치" : "Mode: Click the canvas to place a measured variable");
    if (mode === "addLatent") return window.StatEduModelCanvas.state.label(instance, "mode_add_latent", instance.language === "ko" ? "모드: 캔버스를 클릭하여 잠재변수 배치" : "Mode: Click the canvas to place a latent variable");
    if (mode === "addHigherOrderLatent") return window.StatEduModelCanvas.state.label(instance, "mode_add_higher_order", instance.language === "ko" ? "모드: 캔버스를 클릭하여 고차요인 배치" : "Mode: Click the canvas to place a higher-order factor");
    if (mode === "connect") return window.StatEduModelCanvas.state.label(instance, "mode_connect", "Mode: Connect");
    if (mode === "covariance") return window.StatEduModelCanvas.state.label(instance, "mode_covariance", instance.language === "ko" ? "모드: 공분산" : "Mode: Covariance");
    if (mode === "delete") return window.StatEduModelCanvas.state.label(instance, "mode_delete", "Mode: Delete");
    if (mode === "properties") return window.StatEduModelCanvas.state.label(instance, "mode_properties", "Mode: Properties");
    return window.StatEduModelCanvas.state.label(instance, "mode_select", "Mode: Select");
  }

  function setMode(instance, mode) {
    if (instance.state.mode === mode && mode !== "select") {
      mode = "select";
    }
    instance.state.mode = mode;
    instance.state.connectFrom = null;
    instance.state.dragPreview = null;
    instance.state.selectedEdgeId = null;
    instance.state.selectedModerationId = null;
    if (mode !== "properties") {
      instance.state.selectedNodeId = null;
      instance.state.selectedNodeIds = [];
      if (window.StatEduModelCanvas.nodes) {
        window.StatEduModelCanvas.nodes.hideProperties(instance);
      }
    }
    updateStatus(instance);
    updateButtons(instance);
    window.StatEduModelCanvas.edges.render(instance);
    window.StatEduModelCanvas.nodes.render(instance);
  }

  function updateButtons(instance) {
    var nodeIds = instance.state.selectedNodeIds || [];
    var selectedNodes = instance.state.nodes.filter(function(node) { return nodeIds.indexOf(node.id) >= 0; });
    var selectedIndicators = selectedNodes.filter(function(node) { return node.role === "indicator"; });
    var hasCanvasContent = instance.state.nodes.length > 0 || instance.state.edges.length > 0 ||
      instance.state.moderations.length > 0 || instance.state.covariates.length > 0;
    var hasResult = !!instance.resultSnapshot && instance.root.classList.contains("has-result");
    instance.root.querySelectorAll(".custom-model-toolbar-button").forEach(function(button) {
      var action = button.getAttribute("data-action") || "";
      var latentStatsVisible = instance.state.showLatentStats !== false &&
        Array.isArray(instance.state.latentStatsSelection) &&
        instance.state.latentStatsSelection.length > 0;
      var active = action === instance.state.mode ||
        (action === "grid" && instance.state.gridVisible) ||
        (action === "autoAlign" && instance.state.autoAlign !== false) ||
        (action === "latentStats" && latentStatsVisible) ||
        (action === "resultEdit" && hasResult && instance.state.mode === "properties") ||
        (action === "dashNonsignificant" && hasResult && instance.state.dashNonsignificant !== false);
      button.classList.toggle("is-active", active);
      if (action === "select" || action === "autoAlign") {
        button.setAttribute("aria-pressed", active ? "true" : "false");
        if (action === "autoAlign") {
          var label = button.querySelector(".custom-model-toolbar-label");
          if (label) label.textContent = instance.language === "ko"
            ? "이동 시 정렬: " + (active ? "켬" : "끔")
            : "Snap while moving: " + (active ? "On" : "Off");
          if (label) {
            button.setAttribute("aria-label", label.textContent);
            button.title = label.textContent + (instance.language === "ko"
              ? " — 이동할 때 주변 변수의 정렬선에 맞춥니다."
              : " — Snap to nearby alignment guides while dragging.");
          }
        }
      }

      var applicable = true;
      if (action === "covariates") {
        applicable = (instance.state.selectedVariables || []).length > 0;
        button.disabled = !applicable;
        button.setAttribute("aria-disabled", applicable ? "false" : "true");
      }
      if (action === "save") applicable = hasCanvasContent;
      if (action === "resultSave") applicable = hasResult;
      if (action === "run") applicable = instance.state.nodes.length > 0;
      if (action === "connect" || action === "covariance") applicable = instance.state.nodes.length >= 2;
      if (action === "properties" || action === "delete") applicable = hasCanvasContent;
      if (action === "detachIndicator" || action === "indicatorUp" || action === "indicatorDown") applicable = selectedIndicators.length > 0;
      if (["alignLeft", "alignTop", "alignCenter", "alignMiddle"].indexOf(action) >= 0) applicable = selectedNodes.length >= 2;
      if (action === "distributeH" || action === "distributeV") applicable = selectedNodes.length >= 3;
      if (action === "alignIndicators") applicable = window.StatEduModelCanvas.canvas.indicatorAlignmentGroups(instance).length > 0;
      if (action === "autoLayout") applicable = hasCanvasContent;
      if (action === "undo") applicable = instance.state.history.length > 0;
      if (action === "redo") applicable = instance.state.redoStack.length > 0;
      if (["resultEdit", "dashNonsignificant", "style"].indexOf(action) >= 0) applicable = hasResult;
      if (["save", "resultSave", "run", "connect", "covariance", "properties", "delete", "detachIndicator", "indicatorUp", "indicatorDown", "alignLeft", "alignTop", "alignCenter", "alignMiddle", "distributeH", "distributeV", "alignIndicators", "autoLayout", "undo", "redo", "resultEdit", "dashNonsignificant", "style"].indexOf(action) >= 0) {
        button.disabled = !applicable;
        button.setAttribute("aria-disabled", applicable ? "false" : "true");
      }
      if (action === "structuralCovariateTargets") {
        var hasCovariates = Array.isArray(instance.state.covariates) && instance.state.covariates.length > 0;
        button.disabled = !hasCovariates;
        button.setAttribute("aria-disabled", hasCovariates ? "false" : "true");
      }
    });
    var assignCovariate = instance.root.querySelector(".structural-covariate-toolbar-button");
    if (assignCovariate) {
      var canAssignCovariate = (instance.state.selectedVariables || []).length > 0;
      assignCovariate.disabled = !canAssignCovariate;
      assignCovariate.setAttribute("aria-disabled", canAssignCovariate ? "false" : "true");
    }
    instance.paper.classList.toggle("is-delete-mode", instance.state.mode === "delete");
    instance.paper.classList.toggle("is-connect-mode", instance.state.mode === "connect" || instance.state.mode === "covariance");
    instance.paper.classList.toggle("is-place-latent-mode", ["addObserved", "addLatent", "addHigherOrderLatent"].indexOf(instance.state.mode) >= 0);
    instance.paper.classList.toggle("is-grid-visible", instance.state.gridVisible);
    var latentTools = instance.root.querySelector(".structural-latent-tools");
    var selectedLatents = window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.selectedLatents ? window.StatEduModelCanvas.canvas.selectedLatents(instance) : [];
    if (latentTools) {
      latentTools.classList.add("is-visible");
      var selectedLatent = selectedLatents[0] || null;
      var placement = selectedLatent ? (selectedLatent.measurementPlacement || selectedLatent.effectiveMeasurementPlacement || "") : "";
      var measurementMode = selectedLatent ? (selectedLatent.measurementMode || "reflective") : "";
      latentTools.querySelectorAll(".custom-model-toolbar-button").forEach(function(button) {
        var action = button.getAttribute("data-action") || "";
        var active = action === "placement" + (placement ? placement.charAt(0).toUpperCase() + placement.slice(1) : "") || action === measurementMode;
        button.classList.toggle("is-active", active);
        button.disabled = selectedLatents.length === 0;
        button.setAttribute("aria-disabled", selectedLatents.length === 0 ? "true" : "false");
      });
    }
    var selectedEdge = instance.state.selectedEdgeId ? window.StatEduModelCanvas.edges.edgeById(instance, instance.state.selectedEdgeId) : null;
    instance.root.querySelectorAll(".custom-model-edge-shape-tools").forEach(function(shapeTools) {
      var visible = !!selectedEdge && (instance.state.mode === "properties" ||
        (instance.state.mode === "select" && window.StatEduModelCanvas.edges.curveEditable(instance, selectedEdge)));
      shapeTools.classList.toggle("is-visible", visible);
      shapeTools.querySelectorAll(".custom-model-edge-shape-button").forEach(function(button) {
        var shape = button.getAttribute("data-edge-shape") || "straight";
        var active = selectedEdge && window.StatEduModelCanvas.edges.edgeShape(selectedEdge) === shape;
        button.classList.toggle("is-active", !!active);
      });
    });
    instance.root.querySelectorAll(".custom-model-edge-anchor-tools").forEach(function(anchorTools) {
      var visible = instance.state.mode === "properties" && !!selectedEdge;
      anchorTools.classList.toggle("is-visible", visible);
      anchorTools.querySelectorAll(".custom-model-edge-anchor-button").forEach(function(button) {
        var endpoint = button.getAttribute("data-edge-anchor-endpoint") || "from";
        var side = button.getAttribute("data-edge-anchor-side") || "auto";
        var key = endpoint === "to" ? "toSide" : "fromSide";
        var activeSide = selectedEdge && selectedEdge[key] ? selectedEdge[key] : "auto";
        button.classList.toggle("is-active", !!selectedEdge && activeSide === side);
      });
    });
    instance.root.querySelectorAll("[data-latent-stat]").forEach(function(input) {
      var selectedStats = instance.state.latentStatsSelection || ["r2"];
      var key = input.getAttribute("data-latent-stat") || "r2";
      input.checked = key === "none" ?
        instance.state.showLatentStats === false || selectedStats.length === 0 :
        instance.state.showLatentStats !== false && selectedStats.indexOf(key) >= 0;
    });
  }

  function updateStatus(instance) {
    var mode = instance.root.querySelector(".custom-model-mode-status");
    if (mode) {
      var selectedCount = (instance.state.selectedNodeIds || []).length;
      var selectionText = selectedCount > 0 ? " · " + window.StatEduModelCanvas.state.label(instance, "selected_count", instance.language === "ko" ? "{count}개 선택" : "{count} selected").replace("{count}", selectedCount) : "";
      mode.textContent = modeLabel(instance, instance.state.mode) + selectionText;
    }
    var paper = instance.root.querySelector(".custom-model-paper-status");
    if (paper) {
      paper.textContent = (instance.state.canvas.paper || "B5") + " " + (instance.state.canvas.orientation || "landscape");
      paper.setAttribute("role", "button");
      paper.setAttribute("tabindex", "0");
      paper.setAttribute("title", window.StatEduModelCanvas.state.label(instance, "paper_change_title", instance.language === "ko" ? "용지 크기/방향 변경" : "Change paper size/orientation"));
      paper.setAttribute("aria-label", window.StatEduModelCanvas.state.label(instance, "paper_change_aria", instance.language === "ko" ? "용지 크기와 방향 변경" : "Change paper size and orientation"));
    }
    var covariates = instance.root.querySelector(".custom-model-covariate-status");
    if (covariates) {
      var prefix = window.StatEduModelCanvas.state.label(instance, "covariates", "Covariates");
      var none = window.StatEduModelCanvas.state.label(instance, "none", "none");
      var covariateText = instance.state.covariates.map(function(name) {
        var type = (instance.state.covariateTypes || {})[name];
        return name + (type && type.encoding ? " (" + type.encoding + ")" : "");
      });
      covariates.textContent = prefix + ": " + (covariateText.length ? covariateText.join(", ") : none);
    }
  }

  function hidePopover(instance, selector) {
    var popover = instance.root.querySelector(selector);
    if (popover) popover.classList.remove("is-visible");
  }

  function positionPopoverNearButton(instance, popover, button) {
    if (!popover || !button) return;
    var panel = button.closest(".custom-model-toolbar-panel") || instance.root.querySelector(".custom-model-toolbar-panel.is-active");
    if (!panel) return;
    var panelRect = panel.getBoundingClientRect();
    var buttonRect = button.getBoundingClientRect();
    popover.style.left = Math.max(0, buttonRect.right - panelRect.left + 8) + "px";
    var popoverTop = Math.max(0, buttonRect.top - panelRect.top);
    popover.style.top = popoverTop + "px";
    if (popover.classList.contains("structural-run-options-popover")) {
      var availableHeight = Math.max(320, window.innerHeight - (panelRect.top + popoverTop) - 12);
      popover.style.setProperty("--structural-run-options-height", Math.min(590, availableHeight) + "px");
    }
    if (panel.classList.contains("custom-model-sidebar-actions")) {
      popover.style.setProperty("--sidebar-options-height", Math.max(120, buttonRect.bottom - 2) + "px");
    }
    popover.classList.add("is-visible");

    var popoverRect = popover.getBoundingClientRect();
    var panelWidth = panel.clientWidth || panelRect.width;
    var nextLeft = buttonRect.right - panelRect.left + 8;
    if (!panel.classList.contains("custom-model-sidebar-actions") && nextLeft + popoverRect.width > panelWidth) {
      nextLeft = Math.max(0, buttonRect.left - panelRect.left - popoverRect.width - 8);
    }
    if (panelRect.left + nextLeft + popoverRect.width > window.innerWidth - 12) {
      nextLeft = Math.max(0, window.innerWidth - panelRect.left - popoverRect.width - 12);
    }
    popover.style.left = nextLeft + "px";
    if (panel.classList.contains("custom-model-sidebar-actions")) {
      var confirm = popover.querySelector('[data-action="runConfirm"]');
      var footerRect = confirm ? confirm.getBoundingClientRect() : popoverRect;
      var alignedTop = popoverTop + buttonRect.top + buttonRect.height / 2 - footerRect.top - footerRect.height / 2;
      popover.style.top = Math.max(12 - panelRect.top, alignedTop) + "px";
    }
  }

  function placeLatentAt(instance, x, y, higherOrder) {
    if (!instance || !instance.state || !instance.paper || window.StatEduModelCanvas.nodes.isViewingResult(instance)) return false;
    window.StatEduModelCanvas.state.pushHistory(instance);
    var isCfa = instance.analysisType === "cfa";
    var latent = window.StatEduModelCanvas.nodes.createLatentNode(instance, 0, 0);
    if (higherOrder) {
      var higherOrderCount = instance.state.nodes.filter(function(node) {
        return node.role === "latent" && node.constructType === "higherOrder";
      }).length + 1;
      latent.constructType = "higherOrder";
      latent.name = "HO" + higherOrderCount;
      latent.dataLabel = instance.language === "ko" ? "\uace0\ucc28\uc694\uc778 " + higherOrderCount : "Higher-order " + higherOrderCount;
      latent.canvasLabel = latent.dataLabel;
      latent.measurementPlacement = "left";
      latent.width = 104;
    } else if (isCfa) {
      latent.measurementPlacement = "right";
    }
    var margin = 8;
    var width = Number(latent.width || 90);
    var height = Number(latent.height || 44);
    var paperWidth = Number(instance.state.canvas.widthPx || instance.paper.offsetWidth || 0);
    var paperHeight = Number(instance.state.canvas.heightPx || instance.paper.offsetHeight || 0);
    latent.x = Math.max(margin, Math.min(paperWidth - width - margin, Number(x || 0) - width / 2));
    latent.y = Math.max(margin, Math.min(paperHeight - height - margin, Number(y || 0) - height / 2));

    var existingLatents = instance.state.nodes.filter(function(node) { return node.role === "latent"; });
    instance.state.nodes.push(latent);
    if (isCfa && !higherOrder) {
      existingLatents.forEach(function(otherLatent) {
        if (otherLatent.constructType === "higherOrder") return;
        if (!window.StatEduModelCanvas.edges.createCovariance(instance, otherLatent.id, latent.id)) return;
        var covariance = instance.state.edges[instance.state.edges.length - 1];
        covariance.curveDirection = "left";
        covariance.curveOffset = window.StatEduModelCanvas.edges.latentCovarianceCurveOffset(otherLatent, latent);
        covariance.fromSide = "left";
        covariance.toSide = "left";
        covariance.fixedCenter = true;
      });
    } else if (isCfa && higherOrder) {
      existingLatents.forEach(function(otherLatent) {
        if (otherLatent.constructType !== "higherOrder") return;
        if (!window.StatEduModelCanvas.edges.createCovariance(instance, otherLatent.id, latent.id)) return;
        var covariance = instance.state.edges[instance.state.edges.length - 1];
        covariance.curveDirection = "left";
        covariance.curveOffset = window.StatEduModelCanvas.edges.latentCovarianceCurveOffset(otherLatent, latent, {minOffset: 70, maxOffset: 500, factor: 0.58, knee: 120});
        covariance.fromSide = "left";
        covariance.toSide = "left";
        covariance.fixedCenter = true;
      });
    }
    instance.state.mode = higherOrder ? "addHigherOrderLatent" : "addLatent";
    instance.state.selectedNodeId = latent.id;
    instance.state.selectedNodeIds = [latent.id];
    window.StatEduModelCanvas.canvas.render(instance);
    updateStatus(instance);
    updateButtons(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    return true;
  }

  function handleAction(instance, action, button) {
    if (action === "addObserved") { setMode(instance, action); return; }
    if (action === "connect" && ["cfa", "cbsem", "sem", "plssem"].indexOf(instance.analysisType) < 0) {
      instance.state.nodes.forEach(function(node) { node.autoRole = true; });
    }
    if (action === "addLatent" || action === "addHigherOrderLatent") {
      setMode(instance, action);
      return;
    }
    if (action === "flipCfa" && instance.analysisType === "cfa") {
      var cfaLatents = instance.state.nodes.filter(function(node) { return node.role === "latent"; });
      if (cfaLatents.length) {
        window.StatEduModelCanvas.state.pushHistory(instance);
        var nextPlacement = cfaLatents.some(function(node) {
          return (node.measurementPlacement || node.effectiveMeasurementPlacement || "right") === "right";
        }) ? "left" : "right";
        cfaLatents.forEach(function(node) { node.measurementPlacement = nextPlacement; });
        var covarianceSide = nextPlacement === "left" ? "right" : "left";
        instance.state.edges.forEach(function(edge) {
          if (edge.kind !== "covariance" && edge.type !== "covariance") return;
          var fromNode = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
          var toNode = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
          if (!fromNode || !toNode || fromNode.role !== "latent" || toNode.role !== "latent") return;
          edge.curveDirection = covarianceSide;
          edge.fromSide = covarianceSide;
          edge.toSide = covarianceSide;
          delete edge.controlPoint;
        });
        window.StatEduModelCanvas.canvas.reflowMeasurements(instance);
        window.StatEduModelCanvas.canvas.render(instance);
        window.StatEduModelCanvas.bridge.sendState(instance);
      }
    }
    var validation = instance.root.querySelector(".structural-validation-status");
    if (validation && instance.validation) {
      var errorCount = instance.validation.errors.length;
      var warningCount = instance.validation.warnings.length;
      validation.textContent = window.StatEduModelCanvas.state.label(instance, "validation_counts", instance.language === "ko" ? "오류 {errors} · 경고 {warnings}" : "Errors {errors} · Warnings {warnings}").replace("{errors}", errorCount).replace("{warnings}", warningCount);
      validation.classList.toggle("has-errors", errorCount > 0);
    }
    if (action === "placementLeft") window.StatEduModelCanvas.canvas.setMeasurementPlacement(instance, "left");
    if (action === "placementRight") window.StatEduModelCanvas.canvas.setMeasurementPlacement(instance, "right");
    if (action === "placementTop") window.StatEduModelCanvas.canvas.setMeasurementPlacement(instance, "top");
    if (action === "placementBottom") window.StatEduModelCanvas.canvas.setMeasurementPlacement(instance, "bottom");
    if (action === "reflective") window.StatEduModelCanvas.canvas.setMeasurementMode(instance, "reflective");
    if (action === "formative") window.StatEduModelCanvas.canvas.setMeasurementMode(instance, "formative");
    if (action === "select") setMode(instance, "select");
    if (action === "connect") setMode(instance, "connect");
    if (action === "covariance") setMode(instance, "covariance");
    if (action === "delete") setMode(instance, "delete");
    if (action === "properties") setMode(instance, "properties");
    if (action === "detachIndicator") window.StatEduModelCanvas.canvas.detachIndicators(instance);
    if (action === "indicatorUp") window.StatEduModelCanvas.canvas.moveIndicator(instance, -1);
    if (action === "indicatorDown") window.StatEduModelCanvas.canvas.moveIndicator(instance, 1);
    if (action === "alignIndicators") window.StatEduModelCanvas.canvas.alignIndicators(instance);
    if (["alignLeft", "alignTop", "alignCenter", "alignMiddle", "distributeH", "distributeV"].indexOf(action) >= 0) window.StatEduModelCanvas.canvas.alignSelected(instance, action);
    if (action === "autoLayout") window.StatEduModelCanvas.canvas.autoLayout(instance);
    if (action === "grid") {
      instance.state.gridVisible = !instance.state.gridVisible;
      updateButtons(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }
    if (action === "autoAlign") {
      instance.state.autoAlign = instance.state.autoAlign === false;
      updateButtons(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }
    if (action === "undo" && window.StatEduModelCanvas.state.undo(instance)) {
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }
    if (action === "redo" && window.StatEduModelCanvas.state.redo(instance)) {
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }
    if (action === "reset") {
      hidePopover(instance, ".custom-model-run-options-popover");
      var resetPopover = instance.root.querySelector(".custom-model-reset-confirm-popover");
      positionPopoverNearButton(instance, resetPopover, button);
    }
    if (action === "resetCancel") {
      hidePopover(instance, ".custom-model-reset-confirm-popover");
    }
    if (action === "resetConfirm") {
      hidePopover(instance, ".custom-model-reset-confirm-popover");
      window.StatEduModelCanvas.dialogs.reset(instance, true);
    }
    if (action === "style") window.StatEduModelCanvas.dialogs.style(instance);
    if (action === "covariates") window.StatEduModelCanvas.canvas.addSelectedVariableAsRole(instance, "covariate");
    if (action === "structuralCovariateTargets") window.StatEduModelCanvas.dialogs.structuralCovariateTargets(instance);
    if (action === "paper") window.StatEduModelCanvas.dialogs.paper(instance);
    if (action === "save") window.StatEduModelCanvas.dialogs.save(instance);
    if (action === "load") window.StatEduModelCanvas.dialogs.load(instance);
    if (action === "resultSave") window.StatEduModelCanvas.bridge.requestResultFile(instance, "save");
    if (action === "resultLoad") window.StatEduModelCanvas.bridge.requestResultFile(instance, "load");
    if (action === "run") {
      hidePopover(instance, ".custom-model-reset-confirm-popover");
      window.StatEduModelCanvas.dialogs.run(instance);
      positionPopoverNearButton(instance, instance.root.querySelector(".custom-model-run-options-popover"), button);
    }
    if (action === "runCancel") {
      hidePopover(instance, ".custom-model-run-options-popover");
    }
    if (action === "runConfirm") {
      hidePopover(instance, ".custom-model-run-options-popover");
      window.StatEduModelCanvas.bridge.sendState(instance);
      window.StatEduModelCanvas.bridge.runConfirm(instance);
    }
    if (action === "resultEdit") {
      if (!window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
        window.StatEduModelCanvas.canvas.showResult(instance);
      }
      setMode(instance, "properties");
    }
    if (action === "latentStats") {
      var statsPopover = instance.root.querySelector(".structural-latent-stats-popover");
      if (statsPopover) statsPopover.classList.toggle("is-visible");
    }
    if (action === "dashNonsignificant") {
      instance.state.dashNonsignificant = instance.state.dashNonsignificant === false;
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }
    if (action === "zoomIn") window.StatEduModelCanvas.canvas.zoom(instance, 1.1);
    if (action === "zoomOut") window.StatEduModelCanvas.canvas.zoom(instance, 1 / 1.1);
    if (action === "fit") window.StatEduModelCanvas.canvas.fit(instance);
    if (action === "export") window.StatEduModelCanvas.dialogs.exportModel(instance);
  }

  function setActiveGroup(instance, group) {
    if (!instance || !group) return;
    instance.root.querySelectorAll(".custom-model-toolbar-tab").forEach(function(item) {
      item.classList.toggle("is-active", item.getAttribute("data-toolbar-group") === group);
    });
    instance.root.querySelectorAll(".custom-model-toolbar .custom-model-toolbar-panel").forEach(function(panel) {
      panel.classList.toggle("is-active", panel.getAttribute("data-toolbar-panel") === group);
    });
    if (group === "result") {
      window.StatEduModelCanvas.canvas.showResult(instance);
    } else {
      window.StatEduModelCanvas.canvas.showSource(instance);
    }
  }

  function bind(instance) {
    instance.root.querySelectorAll(".custom-model-toolbar-tab").forEach(function(tab) {
      tab.addEventListener("click", function(event) {
        event.preventDefault();
        var group = tab.getAttribute("data-toolbar-group") || "";
        setActiveGroup(instance, group);
      });
    });
    instance.root.querySelectorAll(".custom-model-toolbar-button").forEach(function(button) {
      button.addEventListener("click", function(event) {
        event.preventDefault();
        handleAction(instance, button.getAttribute("data-action") || "", button);
      });
    });
    instance.root.querySelectorAll(".custom-model-paper-status").forEach(function(paperStatus) {
      paperStatus.addEventListener("click", function(event) {
        event.preventDefault();
        window.StatEduModelCanvas.dialogs.paper(instance);
      });
      paperStatus.addEventListener("keydown", function(event) {
        if (event.key !== "Enter" && event.key !== " ") return;
        event.preventDefault();
        window.StatEduModelCanvas.dialogs.paper(instance);
      });
    });
    instance.root.querySelectorAll(".custom-model-run-options-popover [data-action], .custom-model-reset-confirm-popover [data-action]").forEach(function(button) {
      button.addEventListener("click", function(event) {
        event.preventDefault();
        handleAction(instance, button.getAttribute("data-action") || "", button);
      });
    });
    instance.root.querySelectorAll("[data-latent-stat]").forEach(function(input) {
      var selectedStats = instance.state.latentStatsSelection || ["r2"];
      var key = input.getAttribute("data-latent-stat") || "r2";
      input.checked = key === "none" ?
        instance.state.showLatentStats === false || selectedStats.length === 0 :
        instance.state.showLatentStats !== false && selectedStats.indexOf(key) >= 0;
    });
    instance.root.querySelectorAll("[data-latent-stat]").forEach(function(input) {
      input.addEventListener("change", function() {
        var key = input.getAttribute("data-latent-stat") || "r2";
        if (key === "none") {
          instance.state.latentStatsSelection = [];
          instance.state.showLatentStats = false;
        } else {
          instance.state.latentStatsSelection = [key];
          instance.state.showLatentStats = true;
        }
        hidePopover(instance, ".structural-latent-stats-popover");
        window.StatEduModelCanvas.canvas.render(instance);
        updateButtons(instance);
        window.StatEduModelCanvas.bridge.sendState(instance);
      });
    });
    instance.root.querySelectorAll(".custom-model-edge-shape-button").forEach(function(button) {
      button.addEventListener("click", function(event) {
        event.preventDefault();
        var edgeId = instance.state.selectedEdgeId;
        if (!edgeId) return;
        window.StatEduModelCanvas.state.pushHistory(instance);
        var changed = window.StatEduModelCanvas.edges.setEdgeShape(instance, edgeId, button.getAttribute("data-edge-shape") || "straight");
        if (!changed) {
          instance.state.history.pop();
          return;
        }
        window.StatEduModelCanvas.canvas.render(instance);
        window.StatEduModelCanvas.bridge.sendState(instance);
      });
    });
    instance.root.querySelectorAll(".custom-model-edge-anchor-button").forEach(function(button) {
      button.addEventListener("click", function(event) {
        event.preventDefault();
        var edgeId = instance.state.selectedEdgeId;
        if (!edgeId) return;
        window.StatEduModelCanvas.state.pushHistory(instance);
        var changed = window.StatEduModelCanvas.edges.setEdgeAnchorSide(
          instance,
          edgeId,
          button.getAttribute("data-edge-anchor-endpoint") || "from",
          button.getAttribute("data-edge-anchor-side") || "auto"
        );
        if (!changed) {
          instance.state.history.pop();
          return;
        }
        window.StatEduModelCanvas.canvas.render(instance);
        window.StatEduModelCanvas.bridge.sendState(instance);
      });
    });
    updateStatus(instance);
    updateButtons(instance);
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.toolbar = {
    bind: bind,
    placeLatentAt: placeLatentAt,
    setMode: setMode,
    setActiveGroup: setActiveGroup,
    updateStatus: updateStatus,
    updateButtons: updateButtons
  };
})();
