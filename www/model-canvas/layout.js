(function() {
  "use strict";

  var ROLE_X = {
    independent: 100,
    mediator: 360,
    moderator: 230,
    dependent: 620
  };
  var AUTO_ALIGN_THRESHOLD = 48;
  var ROLE_GAP_Y = 70;
  var MEDIATOR_MULTI_GAP_Y = ROLE_GAP_Y * 1.5;
  var ROLE_STACK_TOP_Y = 168;
  var ROLE_STACK_BOTTOM_Y = 348;
  var ROLE_STACK_STEP_Y = Math.round((ROLE_STACK_BOTTOM_Y - ROLE_STACK_TOP_Y) / 2);
  var STACKED_ROLES = ["independent", "mediator", "dependent"];

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
    if (role === "dependent") return box;
    if (role === "moderator") {
      return box;
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

  function roleStackY(index, count) {
    index = Math.max(0, Number(index || 0));
    count = Math.max(1, Number(count || 1));
    if (count === 1) return Math.round((ROLE_STACK_TOP_Y + ROLE_STACK_BOTTOM_Y) / 2);
    if (count <= 3) {
      return Math.round(ROLE_STACK_TOP_Y + (ROLE_STACK_BOTTOM_Y - ROLE_STACK_TOP_Y) * index / (count - 1));
    }
    return ROLE_STACK_TOP_Y + index * ROLE_STACK_STEP_Y;
  }

  function mediatorRangeForCount(count) {
    count = Math.max(1, Number(count || 1));
    return {
      first: roleStackY(0, count),
      last: roleStackY(count - 1, count)
    };
  }

  function roleStackYForCounts(role, index, count, mediatorCount) {
    index = Math.max(0, Number(index || 0));
    count = Math.max(1, Number(count || 1));
    mediatorCount = Math.max(0, Number(mediatorCount || 0));
    if ((role === "independent" || role === "dependent") && count >= 2 && mediatorCount > count) {
      var mediatorRange = mediatorRangeForCount(mediatorCount);
      return Math.round(mediatorRange.first + (mediatorRange.last - mediatorRange.first) * index / (count - 1));
    }
    return roleStackY(index, count);
  }

  function roleStackYForNodes(nodes, role, index, count) {
    var mediatorCount = orderedRoleNodes(nodes, "mediator").length;
    return roleStackYForCounts(role, index, count, mediatorCount);
  }

  function orderedRoleNodes(nodes, role) {
    return nodes.map(function(node, index) {
      return {node: node, index: index};
    }).filter(function(item) {
      return item.node.role === role;
    }).sort(function(a, b) {
      var dy = Number(a.node.y || 0) - Number(b.node.y || 0);
      if (dy !== 0) return dy;
      return a.index - b.index;
    }).map(function(item) {
      return item.node;
    });
  }

  function alignRoleStack(nodes, role) {
    if (STACKED_ROLES.indexOf(role) < 0) return false;
    var roleNodes = orderedRoleNodes(nodes, role);
    var changed = false;
    var x = roleColumnX(nodes, role);
    roleNodes.forEach(function(node, index) {
      var y = roleStackYForNodes(nodes, role, index, roleNodes.length);
      if (Number(node.x || 0) !== x) {
        node.x = x;
        changed = true;
      }
      if (Number(node.y || 0) !== y) {
        node.y = y;
        changed = true;
      }
    });
    return changed;
  }

  function alignSingleIndependentMediator(nodes) {
    var independents = orderedRoleNodes(nodes, "independent");
    var mediators = orderedRoleNodes(nodes, "mediator");
    if (independents.length !== 1 || mediators.length !== 1) return false;
    var targetY = Math.max(16, Number(independents[0].y || 0) - ROLE_GAP_Y);
    if (Number(mediators[0].y || 0) === targetY) return false;
    mediators[0].y = targetY;
    return true;
  }

  function moderatorY(index, style) {
    var boxHeight = Number((style && style.boxHeight) || 38);
    return Math.max(16, ROLE_STACK_TOP_Y - boxHeight * 3);
  }

  function moderatorX(index, style) {
    var boxWidth = Number((style && style.boxWidth) || 110);
    return ROLE_X.moderator + Math.max(0, Number(index || 0)) * (boxWidth + 20);
  }

  function alignModerators(nodes, style) {
    var roleNodes = orderedRoleNodes(nodes, "moderator");
    var changed = false;
    roleNodes.forEach(function(node, index) {
      var x = moderatorX(index, style);
      var y = moderatorY(index, style);
      if (Number(node.x || 0) !== x) {
        node.x = x;
        changed = true;
      }
      if (Number(node.y || 0) !== y) {
        node.y = y;
        changed = true;
      }
    });
    return changed;
  }

  function alignNodeToRoleColumn(node, nodes) {
    if (!node) return false;
    var role = node.role || "independent";
    if (STACKED_ROLES.indexOf(role) < 0) return false;
    var x = roleColumnX(nodes, role, node.id);
    if (Number(node.x || 0) === x) return false;
    node.x = x;
    return true;
  }

  function nextRolePosition(nodes, role, style) {
    var count = nodes.filter(function(node) {
      return node.role === role;
    }).length;
    var nextCount = count + 1;
    var y = STACKED_ROLES.indexOf(role) >= 0 ? roleStackYForNodes(nodes, role, count, nextCount) : 120 + count * roleGapY(role, nextCount);
    if (role === "moderator") {
      y = moderatorY(count, style);
    }
    return {
      x: role === "moderator" ? moderatorX(count, style) : (STACKED_ROLES.indexOf(role) >= 0 ? roleColumnX(nodes, role) : (ROLE_X[role] || ROLE_X.independent)),
      y: y,
      width: style.boxWidth,
      height: style.boxHeight
    };
  }

  function alignDependents(nodes) {
    return alignRoleStack(nodes, "dependent");
  }

  function reflowRoleLayout(nodes, style) {
    var changed = false;
    STACKED_ROLES.forEach(function(role) {
      changed = alignRoleStack(nodes, role) || changed;
    });
    changed = alignSingleIndependentMediator(nodes) || changed;
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
        var mediatorCount = (roles.mediator || []).length;
        var y = STACKED_ROLES.indexOf(role) >= 0 ? roleStackYForCounts(role, index, names.length, mediatorCount) : startY + index * gapY;
        if (role === "moderator") {
          y = moderatorY(index, style);
        }
        nodes.push({
          id: "node_" + name.replace(/[^A-Za-z0-9_]/g, "_") + "_" + Date.now() + "_" + index,
          variableId: name,
          name: name,
          dataLabel: variable.dataLabel || name,
          canvasLabel: "",
          role: role,
          x: role === "moderator" ? moderatorX(index, style) : (ROLE_X[role] || 90),
          y: y,
          width: style.boxWidth,
          height: style.boxHeight,
          fontSize: style.fontSize,
          fontFamily: style.fontFamily
        });
      });
    });

    alignSingleIndependentMediator(nodes);
    alignDependents(nodes);
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
    reflowRoleLayout: reflowRoleLayout,
    roleColumnX: roleColumnX,
    alignNodeToRoleColumn: alignNodeToRoleColumn,
    nextRolePosition: nextRolePosition,
    alignDependents: alignDependents,
    roleAutoAlignPosition: roleAutoAlignPosition,
    autoLayoutVariables: autoLayoutVariables
  };
})();
