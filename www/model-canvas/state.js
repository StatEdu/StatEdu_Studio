(function() {
  "use strict";

  var DEFAULT_STATE = {
    canvas: {
      paper: "B5",
      orientation: "landscape",
      widthMm: 257,
      heightMm: 182,
      widthPx: 971,
      heightPx: 688,
      zoom: 1
    },
    style: {
      boxWidth: 110,
      boxHeight: 38,
      fontSize: 13,
      fontFamily: "Arial",
      boxStrokeColor: "#000000",
      boxStrokeWidth: 1.5,
      edgeStrokeColor: "#000000",
      edgeStrokeWidth: 1.8,
      arrowHead: "triangle",
      labelFontSize: 12
    },
    nodes: [],
    edges: [],
    moderations: [],
    covariates: [],
    covariateApplyTo: "all",
    dashNonsignificant: true,
    autoAlign: true,
    mode: "select",
    gridVisible: true
  };
  var ROLE_LIMITS = {
    independent: Infinity,
    mediator: Infinity,
    moderator: Infinity,
    dependent: Infinity,
    covariate: Infinity
  };
  var ROLE_LABELS_KO = {
    independent: "\ub3c5\ub9bd",
    mediator: "\ub9e4\uac1c",
    moderator: "\uc870\uc808",
    dependent: "\uc885\uc18d",
    covariate: "\uacf5\ubcc0\ub7c9"
  };

  function clone(value) {
    return JSON.parse(JSON.stringify(value));
  }

  function createState() {
    var state = clone(DEFAULT_STATE);
    state.history = [];
    state.redoStack = [];
    state.variables = [];
    state.connectFrom = null;
    state.dragPreview = null;
    state.selectedVariable = null;
    state.selectedVariables = [];
    state.lastSelectedVariableIndex = null;
    state.selectedNodeId = null;
    state.selectedNodeIds = [];
    state.selectedEdgeId = null;
    state.selectedModerationId = null;
    return state;
  }

  function snapshot(state) {
    return {
      canvas: clone(state.canvas),
      style: clone(state.style),
      nodes: clone(state.nodes),
      edges: clone(state.edges),
      moderations: clone(state.moderations),
      covariates: clone(state.covariates),
      covariateApplyTo: state.covariateApplyTo,
      dashNonsignificant: state.dashNonsignificant !== false,
      autoAlign: state.autoAlign !== false,
      gridVisible: state.gridVisible
    };
  }

  function restore(state, snap) {
    state.canvas = clone(snap.canvas || DEFAULT_STATE.canvas);
    state.style = Object.assign(clone(DEFAULT_STATE.style), clone(snap.style || {}));
    state.nodes = clone(snap.nodes || []);
    state.edges = clone(snap.edges || []);
    state.moderations = clone(snap.moderations || []);
    state.covariates = clone(snap.covariates || []);
    state.covariateApplyTo = snap.covariateApplyTo || "all";
    state.dashNonsignificant = snap.dashNonsignificant !== false;
    state.autoAlign = snap.autoAlign !== false;
    state.gridVisible = snap.gridVisible !== false;
    state.connectFrom = null;
    state.dragPreview = null;
    state.selectedNodeId = null;
    state.selectedNodeIds = [];
    state.selectedEdgeId = null;
    state.selectedModerationId = null;
  }

  function pushHistory(instance) {
    instance.state.history.push(snapshot(instance.state));
    if (instance.state.history.length > 100) {
      instance.state.history.shift();
    }
    instance.state.redoStack = [];
  }

  function undo(instance) {
    var state = instance.state;
    if (state.history.length === 0) return false;
    state.redoStack.push(snapshot(state));
    restore(state, state.history.pop());
    return true;
  }

  function redo(instance) {
    var state = instance.state;
    if (state.redoStack.length === 0) return false;
    state.history.push(snapshot(state));
    restore(state, state.redoStack.pop());
    return true;
  }

  function label(instance, key, fallback) {
    var labels = instance && instance.i18n ? instance.i18n : {};
    var value = labels[key];
    return value === undefined || value === null || value === "" ? fallback : String(value);
  }

  function formatLabel(instance, key, fallback) {
    var text = label(instance, key, fallback);
    Array.prototype.slice.call(arguments, 3).forEach(function(value) {
      text = text.replace("%s", value);
    });
    return text;
  }

  function roleLabel(instance, role) {
    return label(instance, "role_" + role, ROLE_LABELS_KO[role] || role);
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.state = {
    ROLE_LIMITS: ROLE_LIMITS,
    ROLE_LABELS_KO: ROLE_LABELS_KO,
    create: createState,
    snapshot: snapshot,
    restore: restore,
    pushHistory: pushHistory,
    undo: undo,
    redo: redo,
    label: label,
    formatLabel: formatLabel,
    roleLabel: roleLabel,
    clone: clone
  };
})();
