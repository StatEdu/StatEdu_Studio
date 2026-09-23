"use strict";

global.window = {
  addEventListener: function() {},
  setTimeout: function() {},
  StatEduModelCanvas: {}
};
global.document = {
  readyState: "loading",
  documentElement: null,
  addEventListener: function() {},
  querySelectorAll: function() { return []; }
};

require("../www/model-canvas/state.js");

window.StatEduModelCanvas.nodes = {
  nodeById: function(instance, id) {
    return instance.state.nodes.find(function(node) { return node.id === id; });
  },
  isViewingResult: function(instance) { return !!instance.viewingResult; }
};
window.StatEduModelCanvas.edges = {
  createEdge: function() { throw new Error("unexpected edge creation"); }
};
window.StatEduModelCanvas.bridge = { sendState: function() {} };
window.StatEduModelCanvas.toolbar = { updateButtons: function() {} };

require("../www/model-canvas/canvas.js");

function instanceFor(placement, scale, includeError) {
  var latent = {
    id: "latent",
    role: "latent",
    x: 100,
    y: 100,
    width: 90,
    height: 44,
    measurementMode: "reflective",
    measurementPlacement: placement,
    measurementDistanceScale: scale
  };
  var indicator = {id: "indicator", role: "indicator", x: 0, y: 0, width: 82, height: 30};
  var nodes = [latent, indicator];
  var edges = [{id: "measurement", from: "latent", to: "indicator", kind: "path"}];
  if (includeError) {
    nodes.push({id: "error", role: "error", x: 0, y: 0, width: 26, height: 26, manualOffset: 0});
    edges.push({id: "error-edge", from: "error", to: "indicator", kind: "path"});
  }
  return {
    analysisType: includeError ? "cfa" : "plssem",
    root: {classList: {contains: function(name) { return name === "structural-equation-canvas-root"; } }},
    state: {
      nodes: nodes,
      edges: edges,
      moderations: [],
      selectedNodeIds: [],
      selectedNodeId: null,
      selectedEdgeId: null,
      style: {boxWidth: 110, boxHeight: 38, latentHeight: 58}
    }
  };
}

function reflow(placement, scale, includeError) {
  var instance = instanceFor(placement, scale, includeError);
  window.StatEduModelCanvas.canvas.reflowMeasurements(instance);
  return Object.fromEntries(instance.state.nodes.map(function(node) { return [node.id, node]; }));
}

var right1 = reflow("right", 1, false);
var right15 = reflow("right", 1.5, false);
var right17 = reflow("right", 1.7, false);
var right2 = reflow("right", 2, false);
if (right1.indicator.x !== 240) throw new Error("1.0x right distance is incorrect");
if (right15.indicator.x !== 265) throw new Error("1.5x right distance is incorrect");
if (right17.indicator.x !== 275) throw new Error("1.7x right distance is incorrect");
if (right2.indicator.x !== 290) throw new Error("2.0x right distance is incorrect");

var left2 = reflow("left", 2, false);
var top2 = reflow("top", 2, false);
var bottom2 = reflow("bottom", 2, false);
if (left2.indicator.x !== -82) throw new Error("2.0x left distance is incorrect");
if (top2.indicator.y !== -30) throw new Error("2.0x top distance is incorrect");
if (bottom2.indicator.y !== 244) throw new Error("2.0x bottom distance is incorrect");

var withError15 = reflow("right", 1.5, true);
var withError2 = reflow("right", 2, true);
if (withError15.error.x !== 391 || withError2.error.x !== 416) {
  throw new Error("residual node did not move with its indicator");
}

var resultView = instanceFor("right", 1, false);
resultView.viewingResult = true;
resultView.state.selectedNodeIds = ["latent"];
if (window.StatEduModelCanvas.canvas.setMeasurementDistanceScale(resultView, 2) !== false ||
    resultView.state.nodes[0].measurementDistanceScale !== 1) {
  throw new Error("result-view distance editing was not blocked");
}

var stateApi = window.StatEduModelCanvas.state;
var restored = stateApi.create();
stateApi.restore(restored, {
  nodes: [
    {id: "legacy", role: "latent"},
    {id: "custom", role: "latent", measurementDistanceScale: 2},
    {id: "invalid", role: "latent", measurementDistanceScale: 9}
  ]
});
var byId = Object.fromEntries(restored.nodes.map(function(node) { return [node.id, node]; }));
if (byId.legacy.measurementDistanceScale !== 1) throw new Error("legacy distance did not default to 1.0x");
if (byId.custom.measurementDistanceScale !== 2) throw new Error("custom distance was not restored");
if (byId.invalid.measurementDistanceScale !== 1) throw new Error("invalid distance was not normalized");

var roundTrip = stateApi.create();
stateApi.restore(roundTrip, stateApi.snapshot(restored));
if (roundTrip.nodes.find(function(node) { return node.id === "custom"; }).measurementDistanceScale !== 2) {
  throw new Error("distance scale did not survive the save/load round trip");
}

console.log("SEM measurement-distance validation passed.");
