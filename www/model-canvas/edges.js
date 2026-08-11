(function() {
  "use strict";

  var SVG_NS = "http://www.w3.org/2000/svg";
  var SNAP_POINTS = [20, 30, 40, 50, 60, 70, 80];
  var ANCHOR_GAP = 2.5;
  var MODERATION_ANCHOR_GAP = 4;
  var EDGE_SIDES = ["top", "right", "bottom", "left"];

  function edgeById(instance, id) {
    return instance.state.edges.find(function(edge) {
      return edge.id === id;
    });
  }

  function validDirectedEdge(fromNode, toNode) {
    var fromRole = fromNode && fromNode.role;
    var toRole = toNode && toNode.role;
    if ((fromRole === "latent" && (toRole === "indicator" || toRole === "latent")) ||
        (fromRole === "indicator" && toRole === "latent") ||
        (fromRole === "disturbance" && toRole === "latent") ||
        (fromRole === "error" && toRole === "indicator")) return true;
    return (
      (fromRole === "independent" && toRole === "mediator") ||
      (fromRole === "mediator" && toRole === "mediator") ||
      (fromRole === "mediator" && toRole === "dependent") ||
      (fromRole === "independent" && toRole === "dependent")
    );
  }

  function measurementParentId(instance, errorNode) {
    var errorEdge = instance.state.edges.find(function(edge) { return edge.from === errorNode.id; });
    var indicatorId = errorEdge ? errorEdge.to : "";
    var measurementEdge = instance.state.edges.find(function(edge) {
      if (edge.kind === "covariance") return false;
      var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
      var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
      return (from && from.role === "latent" && to && to.id === indicatorId) || (to && to.role === "latent" && from && from.id === indicatorId);
    });
    if (!measurementEdge) return "";
    var from = window.StatEduModelCanvas.nodes.nodeById(instance, measurementEdge.from);
    return from && from.role === "latent" ? from.id : measurementEdge.to;
  }

  function validCovarianceEdge(instance, fromNode, toNode) {
    if (!fromNode || !toNode || fromNode.id === toNode.id) return false;
    if (fromNode.role === "latent" && toNode.role === "latent") return true;
    if (fromNode.role === "error" && toNode.role === "error") {
      var fromParent = measurementParentId(instance, fromNode);
      return !!fromParent && fromParent === measurementParentId(instance, toNode);
    }
    if (fromNode.role === "disturbance" && toNode.role === "disturbance") return true;
    var observed = ["independent", "mediator", "dependent", "indicator"];
    return observed.indexOf(fromNode.role) >= 0 && observed.indexOf(toNode.role) >= 0;
  }

  function validModerationSource(node) {
    return node && node.role === "moderator";
  }

  function validModeration(instance, moderation) {
    var fromNode = moderation ? window.StatEduModelCanvas.nodes.nodeById(instance, moderation.from) : null;
    var edge = moderation ? edgeById(instance, moderation.toEdge) : null;
    return !!(validModerationSource(fromNode) && edge);
  }

  function edgeShape(edge) {
    if (!edge || !edge.shape || edge.shape === "straight") return "straight";
    if (edge.shape === "oval") return "curveUp";
    if (edge.shape === "curveDown") return "curveDown";
    return "curveUp";
  }

  function anchorOffset(size, count, index, gap) {
    size = Number(size || 0);
    var center = size / 2;
    if (count <= 1) return center;
    var maxSpread = Math.max(0, size - 12);
    var slotGap = Math.min(Number(gap || ANCHOR_GAP), maxSpread / Math.max(1, count - 1));
    return center + (index - ((count - 1) / 2)) * slotGap;
  }

  function validSide(side) {
    return EDGE_SIDES.indexOf(side) >= 0;
  }

  function sideOverride(edge, endpoint) {
    var side = edge ? edge[endpoint === "from" ? "fromSide" : "toSide"] : "";
    return validSide(side) ? side : null;
  }

  function edgeSide(instance, edge) {
    var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
    var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
    if (!from || !to) return null;
    var fromCenter = window.StatEduModelCanvas.layout.nodeCenter(from, instance.state.style);
    var toCenter = window.StatEduModelCanvas.layout.nodeCenter(to, instance.state.style);
    var fromWidth = Number(from.width || instance.state.style.boxWidth);
    var toWidth = Number(to.width || instance.state.style.boxWidth);
    var separatedHorizontally =
      Number(from.x || 0) + fromWidth <= Number(to.x || 0) ||
      Number(to.x || 0) + toWidth <= Number(from.x || 0);
    var dx = Math.abs(toCenter.x - fromCenter.x);
    var dy = Math.abs(toCenter.y - fromCenter.y);
    var sides;
    if (!separatedHorizontally && dy > dx) {
      var topToBottom = toCenter.y >= fromCenter.y;
      sides = {
        fromSide: topToBottom ? "bottom" : "top",
        toSide: topToBottom ? "top" : "bottom"
      };
    } else {
      var leftToRight = toCenter.x >= fromCenter.x;
      sides = {
        fromSide: leftToRight ? "right" : "left",
        toSide: leftToRight ? "left" : "right"
      };
    }

    var fromOverride = sideOverride(edge, "from");
    var toOverride = sideOverride(edge, "to");
    if (fromOverride) sides.fromSide = fromOverride;
    if (toOverride) sides.toSide = toOverride;
    return sides;
  }

  function setEdgeAnchorSide(instance, edgeId, endpoint, side) {
    var edge = edgeById(instance, edgeId);
    if (!edge || (endpoint !== "from" && endpoint !== "to")) return false;
    var key = endpoint === "from" ? "fromSide" : "toSide";
    if (!side || side === "auto") {
      if (!edge[key]) return false;
      delete edge[key];
      return true;
    }
    if (!validSide(side)) return false;
    if (edge[key] === side) return false;
    edge[key] = side;
    return true;
  }

  function anchorSlot(instance, edge, endpoint, side) {
    if (edge && edge.fixedCenter) return {count: 1, index: 0};
    var nodeId = endpoint === "from" ? edge.from : edge.to;
    var peers = instance.state.edges.filter(function(item) {
      var itemSide = edgeSide(instance, item);
      if (!itemSide) return false;
      if (endpoint === "from") {
        return item.from === nodeId && itemSide.fromSide === side;
      }
      return item.to === nodeId && itemSide.toSide === side;
    });

    peers.sort(function(a, b) {
      var aOther = endpoint === "from" ? a.to : a.from;
      var bOther = endpoint === "from" ? b.to : b.from;
      var aNode = window.StatEduModelCanvas.nodes.nodeById(instance, aOther);
      var bNode = window.StatEduModelCanvas.nodes.nodeById(instance, bOther);
      var aCenter = aNode ? window.StatEduModelCanvas.layout.nodeCenter(aNode, instance.state.style) : {y: 0};
      var bCenter = bNode ? window.StatEduModelCanvas.layout.nodeCenter(bNode, instance.state.style) : {y: 0};
      if (side === "top" || side === "bottom") {
        if (aCenter.x !== bCenter.x) return aCenter.x - bCenter.x;
      } else if (aCenter.y !== bCenter.y) {
        return aCenter.y - bCenter.y;
      }
      return String(a.id).localeCompare(String(b.id));
    });

    return {
      count: peers.length,
      index: Math.max(0, peers.findIndex(function(item) { return item.id === edge.id; }))
    };
  }

  function nodeAnchor(instance, node, side, slot, gap) {
    var width = Number(node.width || instance.state.style.boxWidth);
    var height = Number(node.height || instance.state.style.boxHeight);
    var x = 0;
    var y = 0;
    if (side === "top" || side === "bottom") {
      x = Number(node.x || 0) + anchorOffset(width, slot.count, slot.index, gap);
      y = side === "bottom" ? Number(node.y || 0) + height : Number(node.y || 0);
      return {x: x, y: y};
    }
    y = Number(node.y || 0) + anchorOffset(height, slot.count, slot.index, gap);
    x = side === "right" ? Number(node.x || 0) + width : Number(node.x || 0);
    return {x: x, y: y};
  }

  function moderationTargetPoint(instance, moderation) {
    var edge = edgeById(instance, moderation.toEdge);
    var endpoints = edge ? edgeEndpoints(instance, edge) : null;
    if (!endpoints) return null;
    return pointOnRenderedEdge(edge, endpoints, moderation.edgePosition || 50);
  }

  function moderationSourceSide(instance, moderation, node) {
    var target = moderationTargetPoint(instance, moderation);
    if (!target) return "bottom";
    var center = window.StatEduModelCanvas.layout.nodeCenter(node, instance.state.style);
    return center.y <= target.y ? "bottom" : "top";
  }

  function moderationAnchorSlot(instance, moderation, side) {
    var peers = instance.state.moderations.filter(function(item) {
      if (item.from !== moderation.from) return false;
      var node = window.StatEduModelCanvas.nodes.nodeById(instance, item.from);
      if (!node) return false;
      return moderationSourceSide(instance, item, node) === side;
    });

    peers.sort(function(a, b) {
      var aPoint = moderationTargetPoint(instance, a) || {y: 0};
      var bPoint = moderationTargetPoint(instance, b) || {y: 0};
      if (aPoint.x !== bPoint.x) return aPoint.x - bPoint.x;
      if (aPoint.y !== bPoint.y) return aPoint.y - bPoint.y;
      return String(a.id).localeCompare(String(b.id));
    });

    return {
      count: peers.length,
      index: Math.max(0, peers.findIndex(function(item) { return item.id === moderation.id; }))
    };
  }

  function edgeEndpoints(instance, edge) {
    var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
    var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
    var sides = edgeSide(instance, edge);
    if (!from || !to || !sides) return null;
    return {
      from: nodeAnchor(instance, from, sides.fromSide, anchorSlot(instance, edge, "from", sides.fromSide)),
      to: nodeAnchor(instance, to, sides.toSide, anchorSlot(instance, edge, "to", sides.toSide))
    };
  }

  function pointOnEdge(start, end, percent) {
    var amount = Number(percent || 50) / 100;
    return {
      x: start.x + (end.x - start.x) * amount,
      y: start.y + (end.y - start.y) * amount
    };
  }

  function defaultCurveControlPoint(start, end, shape) {
    var midX = (start.x + end.x) / 2;
    var midY = (start.y + end.y) / 2;
    var dx = end.x - start.x;
    var dy = end.y - start.y;
    var distance = Math.sqrt(dx * dx + dy * dy) || 1;
    var offset = Math.max(40, Math.min(130, distance * 0.22));
    var normalX = -dy / distance;
    var normalY = dx / distance;
    if (shape === "curveUp" && normalY > 0) {
      normalX = -normalX;
      normalY = -normalY;
    }
    if (shape === "curveDown" && normalY < 0) {
      normalX = -normalX;
      normalY = -normalY;
    }
    if (Math.abs(normalY) < 0.2) {
      normalY = shape === "curveDown" ? 1 : -1;
      normalX = 0;
    }
    return {
      x: midX + normalX * offset,
      y: midY + normalY * offset
    };
  }

  function curveControlPoint(edge, start, end) {
    if (
      edge &&
      edge.controlPoint &&
      Number.isFinite(Number(edge.controlPoint.x)) &&
      Number.isFinite(Number(edge.controlPoint.y))
    ) {
      return {x: Number(edge.controlPoint.x), y: Number(edge.controlPoint.y)};
    }
    return defaultCurveControlPoint(start, end, edgeShape(edge));
  }

  function controlPointFromHandle(start, end, handle) {
    return {
      x: 2 * handle.x - 0.5 * (start.x + end.x),
      y: 2 * handle.y - 0.5 * (start.y + end.y)
    };
  }

  function pointOnBezier(start, control, end, percent) {
    var t = Number(percent || 50) / 100;
    var inverse = 1 - t;
    return {
      x: inverse * inverse * start.x + 2 * inverse * t * control.x + t * t * end.x,
      y: inverse * inverse * start.y + 2 * inverse * t * control.y + t * t * end.y
    };
  }

  function pointOnRenderedEdge(edge, endpoints, percent) {
    var shape = edgeShape(edge);
    if (shape !== "straight") {
      return pointOnBezier(endpoints.from, curveControlPoint(edge, endpoints.from, endpoints.to), endpoints.to, percent);
    }
    return pointOnEdge(endpoints.from, endpoints.to, percent);
  }

  function labelOwner(instance, type, id) {
    if (type === "moderation") {
      return instance.state.moderations.find(function(moderation) {
        return moderation.id === id;
      });
    }
    return edgeById(instance, id);
  }

  function labelBox(label, x, y, fontSize) {
    var width = Math.max(28, String(label || "").length * Number(fontSize || 12) * 0.58 + 10);
    var height = Math.max(16, Number(fontSize || 12) + 8);
    return {
      left: x - width / 2,
      right: x + width / 2,
      top: y - height / 2,
      bottom: y + height / 2
    };
  }

  function boxesOverlap(a, b) {
    return a && b && a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top;
  }

  function autoLabelPosition(label, x, y, fontSize, placedLabels) {
    var candidates = [
      {x: 0, y: 0},
      {x: 0, y: -18},
      {x: 0, y: 18},
      {x: 26, y: -10},
      {x: -26, y: -10},
      {x: 26, y: 10},
      {x: -26, y: 10},
      {x: 0, y: -36},
      {x: 0, y: 36},
      {x: 52, y: 0},
      {x: -52, y: 0}
    ];
    for (var i = 0; i < candidates.length; i += 1) {
      var candidate = candidates[i];
      var box = labelBox(label, x + candidate.x, y + candidate.y, fontSize);
      var collides = (placedLabels || []).some(function(existing) {
        return boxesOverlap(box, existing);
      });
      if (!collides) {
        return {x: x + candidate.x, y: y + candidate.y, box: box};
      }
    }
    var fallback = labelBox(label, x, y, fontSize);
    return {x: x, y: y, box: fallback};
  }

  function addEdgeLabelElement(instance, svg, owner, type, id, point, placedLabels) {
    var automaticLabel = owner && (owner.equalityLabel || owner.parameterName) ? (owner.equalityLabel || owner.parameterName) :
      owner && owner.free === false && owner.fixedValue !== null && owner.fixedValue !== undefined ? String(owner.fixedValue) : "";
    var label = String(owner && owner.label ? owner.label : automaticLabel).trim();
    if (!label) return;
    var x = Number(point.x || 0) + Number(owner.labelOffsetX || 0);
    var y = Number(point.y || 0) + Number(owner.labelOffsetY || -10);
    var fontSize = Number(owner.labelFontSize || instance.state.style.labelFontSize || instance.state.style.fontSize || 12);
    if (Number(owner.labelOffsetX || 0) === 0 && Number(owner.labelOffsetY || -10) === -10) {
      var positioned = autoLabelPosition(label, x, y, fontSize, placedLabels);
      x = positioned.x;
      y = positioned.y;
      if (placedLabels) placedLabels.push(positioned.box);
    } else if (placedLabels) {
      placedLabels.push(labelBox(label, x, y, fontSize));
    }
    var group = document.createElementNS(SVG_NS, "g");
    group.setAttribute("class", "custom-model-edge-label");
    group.setAttribute("data-label-type", type);
    group.setAttribute("data-label-id", id);
    group.setAttribute("transform", "translate(" + x + " " + y + ")");

    var halo = document.createElementNS(SVG_NS, "text");
    halo.setAttribute("class", "custom-model-edge-label-halo");
    halo.setAttribute("text-anchor", "middle");
    halo.setAttribute("dominant-baseline", "middle");
    halo.setAttribute("font-size", fontSize);
    halo.textContent = label;

    var text = document.createElementNS(SVG_NS, "text");
    text.setAttribute("class", "custom-model-edge-label-text");
    text.setAttribute("text-anchor", "middle");
    text.setAttribute("dominant-baseline", "middle");
    text.setAttribute("font-size", fontSize);
    text.textContent = label;

    group.appendChild(halo);
    group.appendChild(text);
    svg.appendChild(group);
  }

  function curveHandlePoint(edge, start, end) {
    return pointOnBezier(start, curveControlPoint(edge, start, end), end, 50);
  }

  function percentOnEdge(start, end, point) {
    var dx = end.x - start.x;
    var dy = end.y - start.y;
    var lengthSquared = dx * dx + dy * dy;
    if (lengthSquared <= 0) return 50;
    var raw = ((point.x - start.x) * dx + (point.y - start.y) * dy) / lengthSquared;
    return Math.max(0, Math.min(1, raw)) * 100;
  }

  function snapPercent(percent) {
    return SNAP_POINTS.reduce(function(best, value) {
      return Math.abs(value - percent) < Math.abs(best - percent) ? value : best;
    }, SNAP_POINTS[0]);
  }

  function distanceToEdge(start, end, point) {
    var percent = percentOnEdge(start, end, point);
    var projected = pointOnEdge(start, end, percent);
    var dx = point.x - projected.x;
    var dy = point.y - projected.y;
    return Math.sqrt(dx * dx + dy * dy);
  }

  function distanceToRenderedEdge(edge, endpoints, point) {
    if (edgeShape(edge) === "straight") {
      return {
        distance: distanceToEdge(endpoints.from, endpoints.to, point),
        percent: percentOnEdge(endpoints.from, endpoints.to, point)
      };
    }
    var best = {distance: Infinity, percent: 50};
    for (var percent = 0; percent <= 100; percent += 2) {
      var sample = pointOnRenderedEdge(edge, endpoints, percent);
      var dx = point.x - sample.x;
      var dy = point.y - sample.y;
      var distance = Math.sqrt(dx * dx + dy * dy);
      if (distance < best.distance) {
        best = {distance: distance, percent: percent};
      }
    }
    return best;
  }

  function nearestEdgeAt(instance, point, maxDistance) {
    var best = null;
    instance.state.edges.forEach(function(edge) {
      var endpoints = edgeEndpoints(instance, edge);
      if (!endpoints) return;
      var nearest = distanceToRenderedEdge(edge, endpoints, point);
      if (nearest.distance <= maxDistance && (!best || nearest.distance < best.distance)) {
        best = {
          edge: edge,
          distance: nearest.distance,
          percent: snapPercent(nearest.percent)
        };
      }
    });
    return best;
  }

  function appendArrowMarker(defs, id, arrowHead, color) {
    var marker = document.createElementNS(SVG_NS, "marker");
    marker.setAttribute("id", id);
    marker.setAttribute("markerWidth", arrowHead === "circle" ? "8" : (arrowHead === "line" ? "10" : "10"));
    marker.setAttribute("markerHeight", arrowHead === "circle" ? "8" : (arrowHead === "line" ? "8" : "10"));
    marker.setAttribute("refX", arrowHead === "circle" ? "6" : (arrowHead === "line" ? "8" : "8"));
    marker.setAttribute("refY", arrowHead === "circle" ? "4" : (arrowHead === "line" ? "4" : "3"));
    marker.setAttribute("viewBox", arrowHead === "line" ? "0 0 10 8" : "0 0 10 10");
    marker.setAttribute("orient", "auto-start-reverse");
    marker.setAttribute("markerUnits", arrowHead === "line" ? "userSpaceOnUse" : "strokeWidth");
    marker.setAttribute("overflow", "visible");
    if (arrowHead === "line") {
      var lineArrow = document.createElementNS(SVG_NS, "path");
      lineArrow.setAttribute("d", "M1,1 L8,4 L1,7");
      lineArrow.setAttribute("class", "custom-model-arrow-head custom-model-arrow-head-line");
      lineArrow.setAttribute("fill", "none");
      lineArrow.style.stroke = color;
      lineArrow.setAttribute("stroke-width", "2.4");
      lineArrow.setAttribute("stroke-linecap", "round");
      lineArrow.setAttribute("stroke-linejoin", "round");
      marker.appendChild(lineArrow);
    } else if (arrowHead === "open") {
      var open = document.createElementNS(SVG_NS, "path");
      open.setAttribute("d", "M0,0 L8,3 L0,6 z");
      open.setAttribute("class", "custom-model-arrow-head custom-model-arrow-head-open");
      open.style.fill = "#ffffff";
      open.style.stroke = color;
      open.setAttribute("stroke-width", "1.6");
      open.setAttribute("stroke-linecap", "round");
      open.setAttribute("stroke-linejoin", "round");
      marker.appendChild(open);
    } else if (arrowHead === "circle") {
      var circle = document.createElementNS(SVG_NS, "circle");
      circle.setAttribute("cx", "4");
      circle.setAttribute("cy", "4");
      circle.setAttribute("r", "3");
      circle.setAttribute("class", "custom-model-arrow-head");
      circle.style.fill = color;
      marker.appendChild(circle);
    } else {
      var path = document.createElementNS(SVG_NS, "path");
      path.setAttribute("d", "M0,0 L0,6 L9,3 z");
      path.setAttribute("class", "custom-model-arrow-head");
      path.style.fill = color;
      marker.appendChild(path);
    }
    defs.appendChild(marker);
  }

  function ensureDefs(svg, style) {
    var defs = svg.querySelector("defs");
    if (defs) defs.remove();
    defs = document.createElementNS(SVG_NS, "defs");
    var color = (style && style.edgeStrokeColor) || "#000000";
    var arrowHead = (style && style.arrowHead) || "triangle";
    if (arrowHead === "none") {
      svg.appendChild(defs);
      return defs;
    }
    appendArrowMarker(defs, "custom-model-arrow", arrowHead, color);
    appendArrowMarker(defs, "custom-model-arrow-selected", arrowHead, "#2563eb");
    appendArrowMarker(defs, "custom-model-moderation-arrow", arrowHead, "#7c3aed");
    appendArrowMarker(defs, "custom-model-moderation-arrow-selected", arrowHead, "#2563eb");
    svg.appendChild(defs);
    return defs;
  }

  function lineElement(className, x1, y1, x2, y2) {
    var line = document.createElementNS(SVG_NS, "line");
    line.setAttribute("class", className);
    line.setAttribute("x1", x1);
    line.setAttribute("y1", y1);
    line.setAttribute("x2", x2);
    line.setAttribute("y2", y2);
    return line;
  }

  function pathElement(className, d) {
    var path = document.createElementNS(SVG_NS, "path");
    path.setAttribute("class", className);
    path.setAttribute("d", d);
    path.setAttribute("fill", "none");
    return path;
  }

  function curvePath(edge, start, end) {
    var control = curveControlPoint(edge, start, end);
    return "M " + start.x + " " + start.y + " Q " + control.x + " " + control.y + " " + end.x + " " + end.y;
  }

  function addEdgeHitElement(svg, edge, endpoints) {
    var shape = edgeShape(edge);
    var hit = shape === "straight"
      ? lineElement("custom-model-edge-hit", endpoints.from.x, endpoints.from.y, endpoints.to.x, endpoints.to.y)
      : pathElement("custom-model-edge-hit", curvePath(edge, endpoints.from, endpoints.to));
    hit.setAttribute("data-edge-id", edge.id);
    svg.appendChild(hit);
  }

  function addModerationHitElement(svg, moderation, from, to) {
    var hit = lineElement("custom-model-moderation-hit", from.x, from.y, to.x, to.y);
    hit.setAttribute("data-moderation-id", moderation.id);
    svg.appendChild(hit);
  }

  function addEdgeControlElement(instance, svg, edge, endpoints) {
    if (instance.state.mode !== "properties") return;
    if (edge.id !== instance.state.selectedEdgeId) return;
    if (edgeShape(edge) === "straight") return;
    var control = curveHandlePoint(edge, endpoints.from, endpoints.to);
    var circle = document.createElementNS(SVG_NS, "circle");
    circle.setAttribute("class", "custom-model-edge-control");
    circle.setAttribute("data-edge-id", edge.id);
    circle.setAttribute("cx", control.x);
    circle.setAttribute("cy", control.y);
    circle.setAttribute("r", "5");
    svg.appendChild(circle);
  }

  function selectLabelOwner(instance, type, id) {
    if (type === "moderation") {
      var moderation = labelOwner(instance, type, id);
      if (!moderation) return false;
      instance.state.selectedNodeId = null;
      instance.state.selectedEdgeId = null;
      instance.state.selectedModerationId = moderation.id;
      window.StatEduModelCanvas.nodes.hideProperties(instance);
      renderEdges(instance);
      window.StatEduModelCanvas.nodes.render(instance);
      window.StatEduModelCanvas.toolbar.updateButtons(instance);
      return true;
    }
    var edge = labelOwner(instance, type, id);
    if (!edge) return false;
    instance.state.selectedNodeId = null;
    instance.state.selectedEdgeId = edge.id;
    instance.state.selectedModerationId = null;
    window.StatEduModelCanvas.nodes.hideProperties(instance);
    renderEdges(instance);
    window.StatEduModelCanvas.nodes.render(instance);
    window.StatEduModelCanvas.toolbar.updateButtons(instance);
    return true;
  }

  function renderEdges(instance) {
    var svg = instance.edgeLayer;
    svg.innerHTML = "";
    ensureDefs(svg, instance.state.style);
    var edgeColor = instance.state.style.edgeStrokeColor || "#000000";
    var edgeWidth = Number(instance.state.style.edgeStrokeWidth || 1.8);
    var arrowHead = instance.state.style.arrowHead || "triangle";
    var placedLabels = [];

    function applyEdgeStyle(element, selected, widthOffset, item) {
      element.style.stroke = selected ? "#2563eb" : edgeColor;
      element.style.strokeWidth = edgeWidth + Number(widthOffset || 0);
      if (instance.state.dashNonsignificant !== false && item && item.significant === false) {
        element.setAttribute("stroke-dasharray", "7 5");
      } else {
        element.removeAttribute("stroke-dasharray");
      }
      if (item && item.kind === "covariance") {
        element.setAttribute("marker-start", selected ? "url(#custom-model-arrow-selected)" : "url(#custom-model-arrow)");
        element.setAttribute("marker-end", selected ? "url(#custom-model-arrow-selected)" : "url(#custom-model-arrow)");
      } else if (arrowHead !== "none") {
        element.setAttribute("marker-end", selected ? "url(#custom-model-arrow-selected)" : "url(#custom-model-arrow)");
      }
    }

    instance.state.edges.forEach(function(edge) {
      var endpoints = edgeEndpoints(instance, edge);
      if (!endpoints) return;
      var isSelected = edge.id === instance.state.selectedEdgeId;
      var selected = isSelected ? " is-selected" : "";
      var shape = edgeShape(edge);
      if (shape !== "straight") {
        var path = pathElement("custom-model-edge custom-model-edge-main" + selected, curvePath(edge, endpoints.from, endpoints.to));
        path.setAttribute("data-edge-id", edge.id);
        applyEdgeStyle(path, isSelected, isSelected ? 0.4 : 0, edge);
        svg.appendChild(path);
      } else {
        var line = lineElement("custom-model-edge" + selected, endpoints.from.x, endpoints.from.y, endpoints.to.x, endpoints.to.y);
        line.setAttribute("data-edge-id", edge.id);
        applyEdgeStyle(line, isSelected, isSelected ? 0.4 : 0, edge);
        svg.appendChild(line);
      }
      addEdgeHitElement(svg, edge, endpoints);
      addEdgeControlElement(instance, svg, edge, endpoints);
      addEdgeLabelElement(instance, svg, edge, "edge", edge.id, pointOnRenderedEdge(edge, endpoints, edge.labelPosition || 50), placedLabels);
    });

    instance.state.moderations.forEach(function(moderation) {
      var edge = edgeById(instance, moderation.toEdge);
      var fromNode = window.StatEduModelCanvas.nodes.nodeById(instance, moderation.from);
      var endpoints = edge ? edgeEndpoints(instance, edge) : null;
      if (!fromNode || !endpoints || !validModerationSource(fromNode)) return;
      var side = moderationSourceSide(instance, moderation, fromNode);
      var from = nodeAnchor(instance, fromNode, side, moderationAnchorSlot(instance, moderation, side), MODERATION_ANCHOR_GAP);
      var to = pointOnRenderedEdge(edge, endpoints, moderation.edgePosition || 50);
      var isSelected = moderation.id === instance.state.selectedModerationId;
      var line = lineElement("custom-model-moderation" + (isSelected ? " is-selected" : ""), from.x, from.y, to.x, to.y);
      line.setAttribute("data-moderation-id", moderation.id);
      if (isSelected) {
        line.style.stroke = "#2563eb";
        line.style.strokeWidth = "2.2";
      }
      if (Object.prototype.hasOwnProperty.call(moderation, "significant")) {
        if (instance.state.dashNonsignificant !== false && moderation.significant === false) {
          line.style.strokeDasharray = "5 4";
        } else {
          line.style.strokeDasharray = "none";
        }
      }
      if (arrowHead !== "none") {
        line.setAttribute("marker-end", isSelected ? "url(#custom-model-moderation-arrow-selected)" : "url(#custom-model-moderation-arrow)");
      }
      svg.appendChild(line);
      addModerationHitElement(svg, moderation, from, to);
      addEdgeLabelElement(instance, svg, moderation, "moderation", moderation.id, pointOnEdge(from, to, 50), placedLabels);
    });

    if (instance.state.dragPreview) {
      var preview = instance.state.dragPreview;
      svg.appendChild(lineElement("custom-model-drag-preview", preview.x1, preview.y1, preview.x2, preview.y2));
    }
  }

  function createEdge(instance, fromId, toId) {
    if (!fromId || !toId || fromId === toId) return false;
    var fromNode = window.StatEduModelCanvas.nodes.nodeById(instance, fromId);
    var toNode = window.StatEduModelCanvas.nodes.nodeById(instance, toId);
    if (!validDirectedEdge(fromNode, toNode)) return false;
    var exists = instance.state.edges.some(function(edge) {
      return edge.from === fromId && edge.to === toId;
    });
    if (exists) return false;
    instance.state.edges.push({
      id: "edge_" + fromId + "_" + toId + "_" + Date.now(),
      from: fromId,
      to: toId,
      label: "",
      shape: "straight"
    });
    return true;
  }

  function createCovariance(instance, fromId, toId) {
    if (!fromId || !toId || fromId === toId) return false;
    var fromNode = window.StatEduModelCanvas.nodes.nodeById(instance, fromId);
    var toNode = window.StatEduModelCanvas.nodes.nodeById(instance, toId);
    if (!validCovarianceEdge(instance, fromNode, toNode)) return false;
    var exists = instance.state.edges.some(function(edge) {
      return edge.kind === "covariance" && ((edge.from === fromId && edge.to === toId) || (edge.from === toId && edge.to === fromId));
    });
    if (exists) return false;
    instance.state.edges.push({
      id: "covariance_" + fromId + "_" + toId + "_" + Date.now(),
      from: fromId,
      to: toId,
      kind: "covariance",
      label: "",
      shape: "curveUp",
      free: true,
      parameterName: "",
      equalityLabel: ""
    });
    return true;
  }

  function createModeration(instance, fromId, edgeId, edgePosition) {
    var fromNode = window.StatEduModelCanvas.nodes.nodeById(instance, fromId);
    if (!validModerationSource(fromNode)) return false;
    if (!edgeById(instance, edgeId)) return false;
    var exists = instance.state.moderations.some(function(moderation) {
      return moderation.from === fromId && moderation.toEdge === edgeId;
    });
    if (exists) return false;
    instance.state.moderations.push({
      id: "moderation_" + fromId + "_" + edgeId + "_" + Date.now(),
      type: "moderation",
      from: fromId,
      toEdge: edgeId,
      edgePosition: edgePosition
    });
    return true;
  }

  function deleteEdge(instance, edgeId) {
    instance.state.edges = instance.state.edges.filter(function(edge) {
      return edge.id !== edgeId;
    });
    instance.state.moderations = instance.state.moderations.filter(function(moderation) {
      return moderation.toEdge !== edgeId;
    });
    if (instance.state.selectedEdgeId === edgeId) instance.state.selectedEdgeId = null;
    if (window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.reflowMeasurements) {
      window.StatEduModelCanvas.canvas.reflowMeasurements(instance);
    }
  }

  function deleteModeration(instance, moderationId) {
    instance.state.moderations = instance.state.moderations.filter(function(moderation) {
      return moderation.id !== moderationId;
    });
  }

  function moderationById(instance, moderationId) {
    return instance.state.moderations.find(function(moderation) {
      return moderation.id === moderationId;
    });
  }

  function setEdgeShape(instance, edgeId, shape) {
    var edge = edgeById(instance, edgeId);
    if (!edge) return false;
    edge.shape = shape || "straight";
    if (edgeShape(edge) === "straight") {
      delete edge.controlPoint;
    } else {
      var endpoints = edgeEndpoints(instance, edge);
      if (endpoints) {
        edge.controlPoint = defaultCurveControlPoint(endpoints.from, endpoints.to, edgeShape(edge));
      }
    }
    return true;
  }

  function startControlDrag(instance, event, edgeId) {
    var edge = edgeById(instance, edgeId);
    if (!edge || edgeShape(edge) === "straight") return false;
    event.preventDefault();
    event.stopPropagation();
    instance.state.selectedEdgeId = edge.id;
    window.StatEduModelCanvas.state.pushHistory(instance);

    function move(moveEvent) {
      var point = window.StatEduModelCanvas.canvas.canvasPoint(instance, moveEvent);
      var endpoints = edgeEndpoints(instance, edge);
      if (!endpoints) return;
      edge.controlPoint = controlPointFromHandle(endpoints.from, endpoints.to, point);
      renderEdges(instance);
    }

    function up() {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      document.removeEventListener("pointercancel", up, true);
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }

    document.addEventListener("pointermove", move, true);
    document.addEventListener("pointerup", up, true);
    document.addEventListener("pointercancel", up, true);
    return true;
  }

  function startModerationDrag(instance, event, moderationId) {
    var moderation = moderationById(instance, moderationId);
    var edge = moderation ? edgeById(instance, moderation.toEdge) : null;
    if (!moderation || !edge) return false;
    event.preventDefault();
    event.stopPropagation();
    instance.state.selectedNodeId = null;
    instance.state.selectedNodeIds = [];
    instance.state.selectedEdgeId = null;
    instance.state.selectedModerationId = moderation.id;
    window.StatEduModelCanvas.nodes.hideProperties(instance);
    window.StatEduModelCanvas.state.pushHistory(instance);

    function move(moveEvent) {
      var point = window.StatEduModelCanvas.canvas.canvasPoint(instance, moveEvent);
      var endpoints = edgeEndpoints(instance, edge);
      if (!endpoints) return;
      var nearest = distanceToRenderedEdge(edge, endpoints, point);
      moderation.edgePosition = snapPercent(nearest.percent);
      renderEdges(instance);
      window.StatEduModelCanvas.nodes.render(instance);
    }

    function up() {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      document.removeEventListener("pointercancel", up, true);
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }

    document.addEventListener("pointermove", move, true);
    document.addEventListener("pointerup", up, true);
    document.addEventListener("pointercancel", up, true);
    move(event);
    return true;
  }

  function selectModeration(instance, moderationId) {
    var moderation = moderationById(instance, moderationId);
    if (!moderation) return false;
    instance.state.selectedNodeId = null;
    instance.state.selectedNodeIds = [];
    instance.state.selectedEdgeId = null;
    instance.state.selectedModerationId = moderation.id;
    window.StatEduModelCanvas.nodes.hideProperties(instance);
    renderEdges(instance);
    window.StatEduModelCanvas.nodes.render(instance);
    window.StatEduModelCanvas.toolbar.updateButtons(instance);
    return true;
  }

  function startLabelDrag(instance, event, type, id) {
    var owner = labelOwner(instance, type, id);
    if (!owner) return false;
    event.preventDefault();
    event.stopPropagation();
    selectLabelOwner(instance, type, id);
    window.StatEduModelCanvas.state.pushHistory(instance);
    var start = window.StatEduModelCanvas.canvas.canvasPoint(instance, event);
    var startX = Number(owner.labelOffsetX || 0);
    var startY = Number(owner.labelOffsetY || -10);

    function move(moveEvent) {
      var point = window.StatEduModelCanvas.canvas.canvasPoint(instance, moveEvent);
      owner.labelOffsetX = startX + point.x - start.x;
      owner.labelOffsetY = startY + point.y - start.y;
      renderEdges(instance);
    }

    function up() {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      document.removeEventListener("pointercancel", up, true);
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }

    document.addEventListener("pointermove", move, true);
    document.addEventListener("pointerup", up, true);
    document.addEventListener("pointercancel", up, true);
    return true;
  }

  function showLabelProperties(instance, type, id) {
    var owner = labelOwner(instance, type, id);
    if (!owner) return;
    window.StatEduModelCanvas.nodes.hideProperties(instance);
    var panel = document.createElement("div");
    panel.className = "custom-model-property-popover custom-model-label-property-popover";
    var t = function(key, fallback) {
      return window.StatEduModelCanvas.state.label(instance, key, fallback);
    };
    panel.innerHTML = [
      '<div class="custom-model-property-title">B(p)</div>',
      '<label class="custom-model-property-label">' + t("label", "\ub77c\ubca8") + '</label>',
      '<input class="form-control custom-model-property-label-input" type="text" readonly>',
      '<label class="custom-model-property-label">' + t("font_size", "\ud3f0\ud2b8 \ud06c\uae30") + '</label>',
      '<input class="form-control custom-model-property-font-size" type="number" min="8" max="32" step="1">',
      '<div class="custom-model-property-actions">',
      '<button type="button" class="btn btn-primary btn-sm custom-model-property-apply">' + t("apply", "\uc801\uc6a9") + '</button>',
      '<button type="button" class="btn btn-default btn-sm custom-model-property-close">' + t("close", "\ub2eb\uae30") + '</button>',
      '</div>'
    ].join("");
    panel.querySelector(".custom-model-property-label-input").value = owner.label || "";
    panel.querySelector(".custom-model-property-font-size").value = owner.labelFontSize || instance.state.style.labelFontSize || 12;
    panel.style.left = Math.max(8, Math.min(Number(instance.state.canvas.widthPx || 0) - 250, Number(owner.labelOffsetX || 0) + 360)) + "px";
    panel.style.top = "24px";
    panel.querySelector(".custom-model-property-close").addEventListener("click", function() {
      panel.remove();
    });
    panel.querySelector(".custom-model-property-apply").addEventListener("click", function() {
      window.StatEduModelCanvas.state.pushHistory(instance);
      var fontSize = Number(panel.querySelector(".custom-model-property-font-size").value || 12);
      owner.labelFontSize = Math.max(8, Math.min(32, Number.isFinite(fontSize) ? fontSize : 12));
      panel.remove();
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    });
    instance.paper.appendChild(panel);
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.edges = {
    render: renderEdges,
    createEdge: createEdge,
    createCovariance: createCovariance,
    createModeration: createModeration,
    deleteEdge: deleteEdge,
    deleteModeration: deleteModeration,
    validDirectedEdge: validDirectedEdge,
    validModeration: validModeration,
    setEdgeShape: setEdgeShape,
    setEdgeAnchorSide: setEdgeAnchorSide,
    startControlDrag: startControlDrag,
    startModerationDrag: startModerationDrag,
    selectModeration: selectModeration,
    startLabelDrag: startLabelDrag,
    showLabelProperties: showLabelProperties,
    selectLabelOwner: selectLabelOwner,
    edgeById: edgeById,
    edgeShape: edgeShape,
    edgeEndpoints: edgeEndpoints,
    nearestEdgeAt: nearestEdgeAt,
    pointOnEdge: pointOnEdge,
    pointOnRenderedEdge: pointOnRenderedEdge,
    percentOnEdge: percentOnEdge,
    snapPercent: snapPercent
  };
})();
