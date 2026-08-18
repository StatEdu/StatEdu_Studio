(function() {
  "use strict";

  var DEFAULT_STATE = {
    modelSchemaVersion: 6,
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
      fontSize: 11,
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
    covariateTypes: {},
    covariateTargets: {},
    covariateApplyTo: "all",
    dashNonsignificant: true,
    latentStatsSelection: ["r2"],
    showLatentStats: true,
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
      modelSchemaVersion: Number(state.modelSchemaVersion || DEFAULT_STATE.modelSchemaVersion),
      canvas: clone(state.canvas),
      style: clone(state.style),
      nodes: clone(state.nodes),
      edges: clone(state.edges),
      moderations: clone(state.moderations),
      covariates: clone(state.covariates),
      covariateTypes: clone(state.covariateTypes || {}),
      covariateTargets: clone(state.covariateTargets || {}),
      covariateApplyTo: state.covariateApplyTo,
      dashNonsignificant: state.dashNonsignificant !== false,
      latentStatsSelection: clone(state.latentStatsSelection || ["r2"]),
      showLatentStats: state.showLatentStats !== false,
      autoAlign: state.autoAlign !== false,
      gridVisible: state.gridVisible
    };
  }

  function normalizeEdge(edge) {
    edge = clone(edge || {});
    if (edge.kind !== "covariance" && edge.pathType !== "higherOrder" && edge.pathType !== "regression") edge.pathType = null;
    if (edge.pathType === "higherOrder" || edge.pathType === "regression") edge.pathType = edge.pathType;
    edge.free = edge.free !== false;
    edge.fixedValue = edge.fixedValue === null || edge.fixedValue === undefined || edge.fixedValue === "" ? null : Number(edge.fixedValue);
    edge.startValue = edge.startValue === null || edge.startValue === undefined || edge.startValue === "" ? null : Number(edge.startValue);
    if (!Number.isFinite(edge.fixedValue)) edge.fixedValue = null;
    if (!Number.isFinite(edge.startValue)) edge.startValue = null;
    edge.parameterName = String(edge.parameterName || "").trim();
    edge.equalityLabel = String(edge.equalityLabel || "").trim();
    return edge;
  }

  function normalizeNode(node) {
    node = clone(node || {});
    if (node.role !== "latent") return node;

    var migrations = [];
    var hadConstructType = Object.prototype.hasOwnProperty.call(node, "constructType");
    var hadMeasurementMode = Object.prototype.hasOwnProperty.call(node, "measurementMode");
    var hadWeightingMode = Object.prototype.hasOwnProperty.call(node, "weightingMode");
    var hadAdvancedFlag = Object.prototype.hasOwnProperty.call(node, "advancedConstructSpecification");

    if (!hadMeasurementMode || !node.measurementMode) {
      node.measurementMode = "reflective";
      migrations.push("measurement_mode_defaulted");
    }
    if (!hadConstructType || !node.constructType) {
      node.constructType = node.measurementMode === "formative" ? "composite" : "commonFactor";
      migrations.push("construct_type_inferred_from_measurement_mode");
    }
    if (node.constructType === "unspecified" && !node.advancedConstructSpecification) {
      node.constructType = node.measurementMode === "formative" ? "composite" : "commonFactor";
      migrations.push("default_construct_type_applied");
    }
    if (!hadWeightingMode || !node.weightingMode) {
      node.weightingMode = "auto";
      migrations.push("weighting_mode_defaulted");
    }

    var needsAdvancedReview =
      (node.constructType === "composite" && node.measurementMode === "reflective") ||
      (node.constructType === "commonFactor" && node.measurementMode === "formative") ||
      ["", "auto"].indexOf(node.weightingMode) < 0;
    if (!hadAdvancedFlag) {
      node.advancedConstructSpecification = needsAdvancedReview;
      if (needsAdvancedReview) migrations.push("advanced_view_enabled_for_review");
    } else {
      node.advancedConstructSpecification = !!node.advancedConstructSpecification;
    }

    if (migrations.length && !node.constructSpecificationMigration) {
      node.constructSpecificationMigration = migrations.join(";");
    }
    return node;
  }

  function restore(state, snap) {
    var sourceVersion = Number(snap && snap.modelSchemaVersion || 2);
    state.modelSchemaVersion = DEFAULT_STATE.modelSchemaVersion;
    state.canvas = clone(snap.canvas || DEFAULT_STATE.canvas);
    state.style = Object.assign(clone(DEFAULT_STATE.style), clone(snap.style || {}));
    if (sourceVersion < 5 && state.style.arrowHead === "line") state.style.arrowHead = "triangle";
    state.nodes = clone(snap.nodes || []).map(normalizeNode);
    state.edges = clone(snap.edges || []).map(normalizeEdge);
    var nodeRoles = {};
    state.nodes.forEach(function(node) { nodeRoles[node.id] = node.role; });
    state.edges.forEach(function(edge) {
      if (edge.kind !== "covariance" && nodeRoles[edge.from] === "latent" && nodeRoles[edge.to] === "latent" && !edge.pathType) edge.pathType = "regression";
    });
    state.moderations = clone(snap.moderations || []);
    state.covariates = clone(snap.covariates || []);
    state.covariateTypes = clone(snap.covariateTypes || {});
    state.covariateTargets = clone(snap.covariateTargets || {});
    state.covariateApplyTo = snap.covariateApplyTo || "all";
    state.dashNonsignificant = snap.dashNonsignificant !== false;
    state.latentStatsSelection = clone(snap.latentStatsSelection || ["r2"]);
    state.showLatentStats = snap.showLatentStats !== false;
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
    clone: clone,
    normalizeNode: normalizeNode
  };
})();
