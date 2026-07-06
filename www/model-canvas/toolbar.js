(function() {
  "use strict";

  function modeLabel(instance, mode) {
    if (mode === "connect") return window.StatEduModelCanvas.state.label(instance, "mode_connect", "Mode: Connect");
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
    if (mode !== "properties") {
      instance.state.selectedNodeId = null;
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
    instance.root.querySelectorAll(".custom-model-toolbar-button").forEach(function(button) {
      var action = button.getAttribute("data-action") || "";
      var active = action === instance.state.mode ||
        (action === "grid" && instance.state.gridVisible) ||
        (action === "autoAlign" && instance.state.autoAlign !== false) ||
        (action === "resultEdit" && instance.state.mode === "properties") ||
        (action === "dashNonsignificant" && instance.state.dashNonsignificant !== false);
      button.classList.toggle("is-active", active);
    });
    instance.paper.classList.toggle("is-delete-mode", instance.state.mode === "delete");
    instance.paper.classList.toggle("is-connect-mode", instance.state.mode === "connect");
    instance.paper.classList.toggle("is-grid-visible", instance.state.gridVisible);
    var selectedEdge = instance.state.selectedEdgeId ? window.StatEduModelCanvas.edges.edgeById(instance, instance.state.selectedEdgeId) : null;
    instance.root.querySelectorAll(".custom-model-edge-shape-tools").forEach(function(shapeTools) {
      var visible = instance.state.mode === "properties" && !!selectedEdge;
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
  }

  function updateStatus(instance) {
    var mode = instance.root.querySelector(".custom-model-mode-status");
    if (mode) mode.textContent = modeLabel(instance, instance.state.mode);
    var paper = instance.root.querySelector(".custom-model-paper-status");
    if (paper) {
      paper.textContent = (instance.state.canvas.paper || "B5") + " " + (instance.state.canvas.orientation || "landscape");
    }
    var covariates = instance.root.querySelector(".custom-model-covariate-status");
    if (covariates) {
      var prefix = window.StatEduModelCanvas.state.label(instance, "covariates", "Covariates");
      var none = window.StatEduModelCanvas.state.label(instance, "none", "none");
      covariates.textContent = prefix + ": " + (instance.state.covariates.length ? instance.state.covariates.join(", ") : none);
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
    popover.style.top = Math.max(0, buttonRect.top - panelRect.top) + "px";
    popover.classList.add("is-visible");

    var popoverRect = popover.getBoundingClientRect();
    var panelWidth = panel.clientWidth || panelRect.width;
    var nextLeft = buttonRect.right - panelRect.left + 8;
    if (nextLeft + popoverRect.width > panelWidth) {
      nextLeft = Math.max(0, buttonRect.left - panelRect.left - popoverRect.width - 8);
    }
    if (panelRect.left + nextLeft + popoverRect.width > window.innerWidth - 12) {
      nextLeft = Math.max(0, window.innerWidth - panelRect.left - popoverRect.width - 12);
    }
    popover.style.left = nextLeft + "px";
  }

  function handleAction(instance, action, button) {
    if (action === "select") setMode(instance, "select");
    if (action === "connect") setMode(instance, "connect");
    if (action === "delete") setMode(instance, "delete");
    if (action === "properties") setMode(instance, "properties");
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
    if (action === "covariates") window.StatEduModelCanvas.dialogs.covariates(instance);
    if (action === "paper") window.StatEduModelCanvas.dialogs.paper(instance);
    if (action === "save") window.StatEduModelCanvas.dialogs.save(instance);
    if (action === "load") window.StatEduModelCanvas.dialogs.load(instance);
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
    if (action === "resultView") window.StatEduModelCanvas.canvas.showResult(instance);
    if (action === "resultEdit") setMode(instance, "properties");
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
    instance.root.querySelectorAll(".custom-model-toolbar-panel").forEach(function(panel) {
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
    instance.root.querySelectorAll(".custom-model-run-options-popover [data-action], .custom-model-reset-confirm-popover [data-action]").forEach(function(button) {
      button.addEventListener("click", function(event) {
        event.preventDefault();
        handleAction(instance, button.getAttribute("data-action") || "", button);
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
    setMode: setMode,
    setActiveGroup: setActiveGroup,
    updateStatus: updateStatus,
    updateButtons: updateButtons
  };
})();
