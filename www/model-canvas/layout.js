(function() {
  "use strict";

  var ROLE_X = {
    independent: 90,
    mediator: 330,
    moderator: 570,
    dependent: 570
  };
  var AUTO_ALIGN_THRESHOLD = 48;
  var ROLE_GAP_Y = 70;
  var MEDIATOR_MULTI_GAP_Y = ROLE_GAP_Y * 1.5;

  var ROLE_LABELS = {
    independent: "Independent",
    mediator: "Mediator",
    moderator: "Moderator",
    dependent: "Dependent"
  };

  function displayText(node) {
    var canvasLabel = String(node.canvasLabel || "").trim();
    if (canvasLabel) return canvasLabel;
    var dataLabel = String(node.dataLabel || "").trim();
    if (dataLabel) return dataLabel;
    return String(node.name || "");
  }

  function nodeCenter(node, style) {
    return {
      x: Number(node.x || 0) + Number((node.width || style.boxWidth) / 2),
      y: Number(node.y || 0) + Number((node.height || style.boxHeight) / 2)
    };
  }

  function rangesOverlap(a1, a2, b1, b2) {
    return Math.max(a1, b1) <= Math.min(a2, b2);
  }

  function snapDropPosition(box, nodes, threshold) {
    return autoAlignPosition(box, nodes, threshold);
  }

  function filteredNodes(nodes, excludeId, roles) {
    return nodes.filter(function(node) {
      if (excludeId && node.id === excludeId) return false;
      if (roles === undefined || roles === null) return true;
      if (!roles.length) return false;
      return roles.indexOf(node.role || "independent") >= 0;
    });
  }

  function numericXs(nodes) {
    return nodes.map(function(node) {
      return Number(node.x || 0);
    }).filter(function(value) {
      return Number.isFinite(value);
    }).sort(function(a, b) {
      return a - b;
    });
  }

  function median(values) {
    if (!values.length) return NaN;
    var middle = Math.floor(values.length / 2);
    if (values.length % 2 === 1) return values[middle];
    return (values[middle - 1] + values[middle]) / 2;
  }

  function independentMediatorGap(nodes, excludeId) {
    var independentXs = numericXs(filteredNodes(nodes, excludeId, ["independent"]));
    var mediatorXs = numericXs(filteredNodes(nodes, excludeId, ["mediator"]));
    var gap = median(mediatorXs) - median(independentXs);
    if (Number.isFinite(gap) && Math.abs(gap) >= 40) return gap;
    return ROLE_X.mediator - ROLE_X.independent;
  }

  function targetXValues(nodes, excludeId, roles, offset) {
    return numericXs(filteredNodes(nodes, excludeId, roles)).map(function(x) {
      return x + offset;
    });
  }

  function autoAlignPosition(box, nodes, threshold, excludeId, options) {
    threshold = Number(threshold || AUTO_ALIGN_THRESHOLD);
    options = options || {};
    var bestX = null;
    var bestY = null;
    var candidates = filteredNodes(nodes, excludeId, options.roles);
    var hasRolesX = Object.prototype.hasOwnProperty.call(options, "rolesX");
    var hasRolesY = Object.prototype.hasOwnProperty.call(options, "rolesY");
    var xCandidates = hasRolesX ? filteredNodes(nodes, excludeId, options.rolesX) : candidates;
    var yCandidates = hasRolesY ? filteredNodes(nodes, excludeId, options.rolesY) : candidates;
    var xValues = (options.xValues || []).filter(function(value) {
      return Number.isFinite(Number(value));
    }).map(Number);
    var snapX = options.snapX !== false;
    var snapY = options.snapY !== false;

    xCandidates.forEach(function(node) {
      xValues.push(Number(node.x || 0));
    });

    xValues.forEach(function(value) {
      var dx = Math.abs(box.x - value);
      if (snapX && dx <= threshold) {
        if (!bestX || dx < bestX.distance) bestX = {value: value, distance: dx};
      }
    });

    yCandidates.forEach(function(node) {
      var dy = Math.abs(box.y - node.y);
      if (snapY && dy <= threshold) {
        if (!bestY || dy < bestY.distance) bestY = {value: node.y, distance: dy};
      }
    });

    if (bestX) box.x = bestX.value;
    if (bestY) box.y = bestY.value;
    return box;
  }

  function roleAutoAlignPosition(box, nodes, threshold, excludeId) {
    var role = box && box.role ? box.role : "independent";
    var gap = independentMediatorGap(nodes, excludeId);
    if (role === "mediator") {
      return autoAlignPosition(box, nodes, threshold, excludeId, {
        xValues: targetXValues(nodes, excludeId, ["independent"], gap),
        rolesX: [],
        rolesY: ["independent"]
      });
    }
    if (role === "independent") {
      return autoAlignPosition(box, nodes, threshold, excludeId, {
        xValues: targetXValues(nodes, excludeId, ["mediator"], -gap),
        rolesX: [],
        rolesY: ["mediator"]
      });
    }
    if (role === "dependent") {
      return autoAlignPosition(box, nodes, threshold, excludeId, {
        roles: ["independent", "mediator"],
        snapX: false,
        snapY: true
      });
    }
    return autoAlignPosition(box, nodes, threshold, excludeId);
  }

  function roleColumnX(nodes, role, excludeId) {
    var peers = nodes.filter(function(node) {
      return node.role === role && (!excludeId || node.id !== excludeId);
    }).sort(function(a, b) {
      return Number(a.y || 0) - Number(b.y || 0);
    });
    if (peers.length) return Number(peers[0].x || 0);
    return ROLE_X[role] || ROLE_X.independent;
  }

  function roleGapY(role, count) {
    if (role === "mediator" && Number(count || 0) >= 2) {
      return MEDIATOR_MULTI_GAP_Y;
    }
    return ROLE_GAP_Y;
  }

  function alignNodeToRoleColumn(node, nodes) {
    if (!node) return false;
    var role = node.role || "independent";
    if (role !== "independent" && role !== "mediator") return false;
    var x = roleColumnX(nodes, role, node.id);
    if (Number(node.x || 0) === x) return false;
    node.x = x;
    return true;
  }

  function nextRolePosition(nodes, role, style) {
    var count = nodes.filter(function(node) {
      return node.role === role;
    }).length;
    var gapY = roleGapY(role, count + 1);
    var y = 120 + count * gapY;
    if (role === "dependent") {
      y = dependentYFromMediators(nodes, y);
    }
    return {
      x: (role === "independent" || role === "mediator") ? roleColumnX(nodes, role) : (ROLE_X[role] || ROLE_X.independent),
      y: y,
      width: style.boxWidth,
      height: style.boxHeight
    };
  }

  function dependentYFromMediators(nodes, fallbackY) {
    var mediators = nodes.filter(function(node) {
      return node.role === "mediator";
    });
    if (!mediators.length) return fallbackY;
    var ys = mediators.map(function(node) {
      return Number(node.y || 0);
    }).sort(function(a, b) {
      return a - b;
    });
    var middle = Math.floor(ys.length / 2);
    if (ys.length % 2 === 1) return ys[middle];
    return Math.round((ys[middle - 1] + ys[middle]) / 2);
  }

  function alignDependentToMediators(nodes) {
    var dependent = nodes.find(function(node) {
      return node.role === "dependent";
    });
    if (!dependent) return false;
    var y = dependentYFromMediators(nodes, dependent.y);
    var changed = false;
    if (Number(dependent.x || 0) !== ROLE_X.dependent) {
      dependent.x = ROLE_X.dependent;
      changed = true;
    }
    if (Number(dependent.y || 0) !== y) {
      dependent.y = y;
      changed = true;
    }
    return changed;
  }

  function autoLayoutVariables(variables, roles, style) {
    var counts = {};
    var nodes = [];
    var startY = 120;

    Object.keys(roles).forEach(function(role) {
      var names = roles[role] || [];
      var gapY = roleGapY(role, names.length);
      counts[role] = counts[role] || 0;
      names.forEach(function(name) {
        var variable = variables.find(function(item) { return item.name === name; });
        if (!variable) return;
        var index = counts[role]++;
        var y = startY + index * gapY;
        if (role === "dependent") {
          var mediatorCount = (roles.mediator || []).length;
          if (mediatorCount > 0) {
            var mediatorGapY = roleGapY("mediator", mediatorCount);
            y = startY + Math.floor(mediatorCount / 2) * mediatorGapY;
            if (mediatorCount % 2 === 0) y = startY + (mediatorCount - 1) * mediatorGapY / 2;
          }
        }
        nodes.push({
          id: "node_" + name.replace(/[^A-Za-z0-9_]/g, "_") + "_" + Date.now() + "_" + index,
          variableId: name,
          name: name,
          dataLabel: variable.dataLabel || name,
          canvasLabel: "",
          role: role,
          x: ROLE_X[role] || 90,
          y: y,
          width: style.boxWidth,
          height: style.boxHeight,
          fontSize: style.fontSize,
          fontFamily: style.fontFamily
        });
      });
    });

    return nodes;
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.layout = {
    ROLE_X: ROLE_X,
    AUTO_ALIGN_THRESHOLD: AUTO_ALIGN_THRESHOLD,
    ROLE_LABELS: ROLE_LABELS,
    displayText: displayText,
    nodeCenter: nodeCenter,
    rangesOverlap: rangesOverlap,
    snapDropPosition: snapDropPosition,
    autoAlignPosition: autoAlignPosition,
    roleColumnX: roleColumnX,
    alignNodeToRoleColumn: alignNodeToRoleColumn,
    nextRolePosition: nextRolePosition,
    alignDependentToMediators: alignDependentToMediators,
    roleAutoAlignPosition: roleAutoAlignPosition,
    autoLayoutVariables: autoLayoutVariables
  };
})();
