(function() {
  "use strict";

  function isStructuralCanvas(instance) {
    return !!(instance && instance.root && instance.root.classList.contains("structural-equation-canvas-root"));
  }

  function parseVariables(root) {
    var raw = root.getAttribute("data-variables") || "[]";
    try {
      return JSON.parse(raw);
    } catch (error) {
      var textarea = document.createElement("textarea");
      textarea.innerHTML = raw;
      try {
        return JSON.parse(textarea.value || "[]");
      } catch (innerError) {
        return [];
      }
    }
  }

  function parseI18n(root) {
    var raw = root.getAttribute("data-i18n") || "{}";
    try {
      return JSON.parse(raw);
    } catch (error) {
      var textarea = document.createElement("textarea");
      textarea.innerHTML = raw;
      try {
        return JSON.parse(textarea.value || "{}");
      } catch (innerError) {
        return {};
      }
    }
  }

  function parseInitialSnapshot(root) {
    var raw = root.getAttribute("data-initial-snapshot") || "";
    if (!raw) return null;
    try {
      return JSON.parse(raw);
    } catch (error) {
      var textarea = document.createElement("textarea");
      textarea.innerHTML = raw;
      try {
        return JSON.parse(textarea.value || "null");
      } catch (innerError) {
        return null;
      }
    }
  }

  function canvasPoint(instance, event) {
    var rect = instance.paper.getBoundingClientRect();
    var structural = isStructuralCanvas(instance);
    var viewZoom = paperViewZoom(instance);
    var localX = (event.clientX - rect.left) / viewZoom;
    var localY = (event.clientY - rect.top) / viewZoom;
    if (structural) {
      var scale = Number(instance.state.canvas.modelZoom || 1);
      var centerX = Number(instance.state.canvas.widthPx || rect.width) / 2;
      var centerY = Number(instance.state.canvas.heightPx || rect.height) / 2;
      return {
        x: (localX - centerX) / scale + centerX,
        y: (localY - centerY) / scale + centerY
      };
    }
    return {
      x: localX,
      y: localY
    };
  }

  function boundedZoom(value, fallback) {
    var number = Number(value);
    if (!Number.isFinite(number) || number <= 0) number = fallback;
    return Math.max(0.25, Math.min(2, number));
  }

  function paperViewZoom(instance) {
    if (isStructuralCanvas(instance)) return boundedZoom(instance.state.canvas.viewZoom, 1);
    return boundedZoom(instance.state.canvas.zoom, 1);
  }

  function setPaperViewZoom(instance, zoom) {
    var nextZoom = boundedZoom(zoom, 1);
    if (isStructuralCanvas(instance)) {
      instance.state.canvas.viewZoom = nextZoom;
    } else {
      instance.state.canvas.zoom = nextZoom;
    }
  }

  function ensurePaperFrame(instance) {
    if (!instance || !instance.paper) return null;
    if (instance.paperFrame && instance.paperFrame.contains(instance.paper)) return instance.paperFrame;
    var parent = instance.paper.parentNode;
    if (parent && parent.classList && parent.classList.contains("custom-model-paper-frame")) {
      instance.paperFrame = parent;
      return parent;
    }
    if (!parent) return null;
    var frame = document.createElement("div");
    frame.className = "custom-model-paper-frame";
    parent.insertBefore(frame, instance.paper);
    frame.appendChild(instance.paper);
    instance.paperFrame = frame;
    return frame;
  }

  function variableByName(instance, name) {
    return instance.state.variables.find(function(variable) {
      return variable.name === name;
    });
  }

  function edgeIdFromEvent(instance, event, tolerance) {
    var edgeElement = event.target && event.target.closest ? event.target.closest(".custom-model-edge, .custom-model-edge-hit, .custom-model-edge-control") : null;
    if (edgeElement) return edgeElement.getAttribute("data-edge-id");
    var nearest = window.StatEduModelCanvas.edges.nearestEdgeAt(instance, canvasPoint(instance, event), tolerance || 18);
    return nearest && nearest.edge ? nearest.edge.id : null;
  }

  function nodeIdAtEvent(instance, event, requiredRole) {
    var direct = event.target && event.target.closest ? event.target.closest(".custom-model-node") : null;
    var candidates = direct ? [direct] : (document.elementsFromPoint ? document.elementsFromPoint(event.clientX, event.clientY) : []);
    for (var index = 0; index < candidates.length; index += 1) {
      var element = candidates[index] && candidates[index].closest ? candidates[index].closest(".custom-model-node") : null;
      if (!element || !instance.nodeLayer.contains(element)) continue;
      var nodeId = element.getAttribute("data-node-id");
      var node = window.StatEduModelCanvas.nodes.nodeById(instance, nodeId);
      if (node && (!requiredRole || node.role === requiredRole)) return nodeId;
    }
    return null;
  }

  function selectableLabelFromEvent(instance, event, initialLabel) {
    if (!instance.edgeLayer) return initialLabel;
    var x = Number(event.clientX);
    var y = Number(event.clientY);
    var matches = Array.prototype.filter.call(instance.edgeLayer.querySelectorAll(".custom-model-edge-label"), function(label) {
      var rect = label.getBoundingClientRect();
      return x >= rect.left - 3 && x <= rect.right + 3 && y >= rect.top - 3 && y <= rect.bottom + 3;
    });
    if (!matches.length) return initialLabel;
    if (matches.length === 1) return matches[0];
    var selectedId = instance.state.selectedEdgeId;
    var selectedIndex = matches.findIndex(function(label) {
      return label.getAttribute("data-label-type") === "edge" && label.getAttribute("data-label-id") === selectedId;
    });
    return matches[(selectedIndex + 1 + matches.length) % matches.length];
  }

  function render(instance) {
    ensurePaperFrame(instance);
    var paperWidth = Number(instance.state.canvas.widthPx || 0);
    var paperHeight = Number(instance.state.canvas.heightPx || 0);
    var viewZoom = paperViewZoom(instance);
    var displayWidth = Math.ceil(paperWidth * viewZoom);
    var displayHeight = Math.ceil(paperHeight * viewZoom);
    var diagramColumnWidth = Math.max(720, displayWidth + 48);
    instance.root.style.setProperty("--custom-model-paper-width", paperWidth + "px");
    instance.root.style.setProperty("--custom-model-paper-height", paperHeight + "px");
    instance.root.style.setProperty("--custom-model-paper-display-width", displayWidth + "px");
    instance.root.style.setProperty("--custom-model-paper-display-height", displayHeight + "px");
    instance.root.style.setProperty("--custom-model-diagram-column-width", diagramColumnWidth + "px");
    if (instance.paperFrame) {
      instance.paperFrame.style.width = displayWidth + "px";
      instance.paperFrame.style.height = displayHeight + "px";
    }
    instance.paper.style.width = instance.state.canvas.widthPx + "px";
    instance.paper.style.height = instance.state.canvas.heightPx + "px";
    instance.paper.style.transform = viewZoom === 1 ? "none" : "scale(" + viewZoom + ")";
    instance.paper.style.transformOrigin = "0 0";
    if (isStructuralCanvas(instance)) {
      var modelZoom = Number(instance.state.canvas.modelZoom || 1);
      instance.nodeLayer.style.transform = "scale(" + modelZoom + ")";
      instance.nodeLayer.style.transformOrigin = "50% 50%";
      instance.edgeLayer.style.transform = "scale(" + modelZoom + ")";
      instance.edgeLayer.style.transformOrigin = "50% 50%";
    } else {
      instance.nodeLayer.style.transform = "";
      instance.edgeLayer.style.transform = "";
    }
    instance.edgeLayer.setAttribute("width", instance.state.canvas.widthPx);
    instance.edgeLayer.setAttribute("height", instance.state.canvas.heightPx);
    instance.validation = validateStructuralModel(instance);
    instance.state.validation = window.StatEduModelCanvas.state.clone(instance.validation);
    window.StatEduModelCanvas.nodes.render(instance);
    window.StatEduModelCanvas.edges.render(instance);
    window.StatEduModelCanvas.toolbar.updateStatus(instance);
    window.StatEduModelCanvas.toolbar.updateButtons(instance);
    updateSelectionSettings(instance);
  }

  function validateStructuralModel(instance) {
    var result = {errors: [], warnings: [], byNode: {}};
    if (!isStructuralCanvas(instance)) return result;
    var ko = instance.language === "ko";
    function add(level, nodeId, code, message) {
      var item = {level: level, nodeId: nodeId, code: code, message: message};
      result[level === "error" ? "errors" : "warnings"].push(item);
      if (nodeId) {
        result.byNode[nodeId] = result.byNode[nodeId] || [];
        result.byNode[nodeId].push(item);
      }
    }
    var latents = instance.state.nodes.filter(function(node) { return node.role === "latent"; });
    var structural = instance.state.edges.filter(function(edge) {
      if (edge.kind === "covariance") return false;
      if (edge.pathType === "higherOrder") return false;
      var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
      var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
      return from && to && from.role === "latent" && to.role === "latent";
    });
    latents.forEach(function(latent) {
      var indicators = instance.state.edges.map(function(edge) {
        var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
        var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
        if (from && from.id === latent.id && to && to.role === "indicator") return to;
        if (to && to.id === latent.id && from && from.role === "indicator") return from;
        return null;
      }).filter(Boolean);
      var lowerOrderFactors = instance.state.edges.map(function(edge) {
        if (edge.kind === "covariance" || edge.pathType !== "higherOrder" || edge.from !== latent.id) return null;
        var target = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
        return target && target.role === "latent" ? target : null;
      }).filter(Boolean);
      indicators = indicators.concat(lowerOrderFactors);
      if (!indicators.length) add("error", latent.id, "latent_without_indicators", "측정변수가 없는 잠재변수");
      else if (indicators.length < 3) add("warning", latent.id, "few_indicators", "측정변수가 3개 미만");
      var measurementMode = latent.measurementMode || "reflective";
      var constructType = latent.constructType || (measurementMode === "formative" ? "composite" : "commonFactor");
      if (constructType === "unspecified") {
        add("error", latent.id, "construct_type_unspecified", ko ? "실행 전 공통요인 또는 합성변수로 명세해야 함" : "Specify common factor or composite before estimation");
      }
      if (constructType === "commonFactor" && measurementMode === "formative") {
        add("error", latent.id, "formative_common_factor", ko ? "형성형 지표 관계는 공통요인 명세와 양립하지 않음" : "A formative indicator relationship is incompatible with a common-factor specification");
      }
      if (["cfa", "cbsem", "sem"].indexOf(instance.analysisType) >= 0 && constructType === "composite") {
        add("error", latent.id, "composite_in_covariance_engine", ko ? "현재 CFA/CB-SEM 엔진은 합성변수를 추정하지 않음" : "The current CFA/CB-SEM engine does not estimate composite constructs");
      }
      var weightingMode = latent.weightingMode || "auto";
      if (["sum", "predefined"].indexOf(weightingMode) >= 0) {
        add("error", latent.id, "weighting_not_implemented", ko ? "동일가중·사용자 지정 가중은 현재 엔진에 구현되지 않음" : "Equal and predefined weights are not implemented in the current engine");
      }
      if (measurementMode === "reflective" && weightingMode === "modeB") {
        add("error", latent.id, "reflective_mode_b", ko ? "현재 반영형 블록은 Mode A만 지원함" : "Reflective blocks require Mode A in the current engine");
      }
      if (measurementMode === "formative" && weightingMode === "modeA") {
        add("error", latent.id, "formative_mode_a", ko ? "현재 형성형 합성변수는 Mode B만 지원함" : "Formative composites require Mode B in the current engine");
      }
      if (constructType === "composite" && measurementMode === "formative") {
        if (!String(latent.compositeDomainDefinition || "").trim()) add("warning", latent.id, "formative_domain_missing", ko ? "합성변수의 개념 영역 정의가 기록되지 않음" : "Composite domain definition is not recorded");
        if (!String(latent.compositeIndicatorRationale || "").trim()) add("warning", latent.id, "formative_indicator_rationale_missing", ko ? "형성지표 포함 근거가 기록되지 않음" : "Formative-indicator inclusion rationale is not recorded");
        if (!String(latent.compositeContentValidityEvidence || "").trim()) add("warning", latent.id, "formative_content_evidence_missing", ko ? "내용타당도 절차·출처가 기록되지 않음" : "Content-validity procedure or source is not recorded");
      }
      var connected = structural.some(function(edge) { return edge.from === latent.id || edge.to === latent.id; });
      if (structural.length > 0 && latents.length > 1 && !connected) add("warning", latent.id, "disconnected_latent", "구조모형에 연결되지 않은 잠재변수");
    });
    var variableNames = instance.state.variables.map(function(variable) { return variable.name; });
    instance.state.nodes.filter(function(node) { return node.role === "indicator"; }).forEach(function(node) {
      if (node.variableId && variableNames.indexOf(node.variableId) < 0) add("error", node.id, "missing_variable", "현재 데이터에 없는 변수");
    });
    var indicatorUsage = {};
    instance.state.nodes.filter(function(node) { return node.role === "indicator" && node.variableId; }).forEach(function(node) {
      indicatorUsage[node.variableId] = indicatorUsage[node.variableId] || [];
      indicatorUsage[node.variableId].push(node);
    });
    Object.keys(indicatorUsage).forEach(function(name) {
      if (indicatorUsage[name].length > 1) indicatorUsage[name].forEach(function(node) { add("error", node.id, "duplicate_indicator", "중복 배정된 측정변수"); });
    });
    var visiting = {}, visited = {};
    function visit(id) {
      if (visiting[id]) return true;
      if (visited[id]) return false;
      visiting[id] = true;
      var cycle = structural.filter(function(edge) { return edge.from === id; }).some(function(edge) { return visit(edge.to); });
      delete visiting[id];
      visited[id] = true;
      return cycle;
    }
    latents.forEach(function(latent) {
      if (visit(latent.id)) add("error", latent.id, "structural_cycle", "순환 구조경로");
    });
    return result;
  }

  function updateSelectionSettings(instance) {
    var container = instance.root.querySelector(".structural-selection-settings-body");
    if (!container) return;
    var ids = instance.state.selectedNodeIds || [];
    var nodes = instance.state.nodes.filter(function(node) { return ids.indexOf(node.id) >= 0; });
    var selectionKey = ids.slice().sort().join("|");
    var selectionChanged = instance.settingsSelectionKey !== selectionKey;
    instance.settingsSelectionKey = selectionKey;
    var ko = instance.language === "ko";
    var single = nodes.length === 1 ? nodes[0] : null;
    updateDisturbancePositionToolbar(instance, single, ko);
    container.innerHTML = "";
    if (!nodes.length) {
      container.textContent = ko ? "캔버스의 변수를 선택하세요." : "Select a variable on the canvas.";
      return;
    }
    var heading = document.createElement("div");
    heading.className = "structural-selection-summary";
    heading.textContent = single ? (single.role === "latent" ? (ko ? "잠재변수" : "Latent variable") : single.role === "indicator" ? (ko ? "측정변수" : "Indicator") : single.role === "disturbance" ? (ko ? "구조오차" : "Structural disturbance") : (ko ? "오차변수" : "Error variable")) : (ko ? nodes.length + "개 선택" : nodes.length + " selected");
    container.appendChild(heading);

    function field(labelText, input) {
      var label = document.createElement("label");
      label.className = "structural-setting-field";
      var caption = document.createElement("span");
      caption.textContent = labelText;
      label.appendChild(caption);
      label.appendChild(input);
      container.appendChild(label);
    }
    function commit(change) {
      window.StatEduModelCanvas.state.pushHistory(instance);
      change();
      render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    }
    function colorPalette(colors, selectedColor, applyColor, includeTransparent) {
      var palette = document.createElement("div");
      palette.className = "structural-color-palette";
      var values = includeTransparent ? ["transparent"].concat(colors) : colors;
      values.forEach(function(color) {
        var button = document.createElement("button");
        button.type = "button";
        button.className = "structural-color-swatch" + (color === "transparent" ? " is-transparent" : "");
        button.title = color === "transparent" ? (ko ? "투명" : "Transparent") : color;
        button.setAttribute("aria-label", button.title);
        if (color !== "transparent") button.style.backgroundColor = color;
        button.classList.toggle("is-selected", selectedColor === color || (color === "transparent" && (!selectedColor || selectedColor === "transparent")));
        button.addEventListener("click", function() { applyColor(color); });
        palette.appendChild(button);
      });
      return palette;
    }

    if (single) {
      var nameInput = document.createElement("input");
      nameInput.type = "text";
      nameInput.className = "form-control input-sm";
      nameInput.value = single.role === "indicator" ? (single.variableId || single.name || "") : (single.name || "");
      nameInput.setAttribute("data-setting-field", "name");
      nameInput.setAttribute("data-node-id", single.id);
      var nameEditable = single.role === "latent" || single.role === "error";
      nameInput.readOnly = !nameEditable;
      if (nameEditable) {
        nameInput.addEventListener("focus", function() { instance.settingsPreferredField = "name"; });
        nameInput.addEventListener("input", function() { instance.settingsPreferredField = "name"; });
        nameInput.addEventListener("change", function() {
          var value = String(nameInput.value || "").trim();
          if (!value) return;
          instance.settingsPreferredField = "name";
          commit(function() {
            single.name = value;
            if (single.role === "latent") single.dataLabel = value;
            if (single.role === "error") single.autoErrorNotation = false;
          });
        });
      }
      field(ko ? "이름" : "Name", nameInput);

      var labelInput = document.createElement("input");
      labelInput.type = "text";
      labelInput.className = "form-control input-sm";
      labelInput.value = single.canvasLabel || single.dataLabel || single.name || "";
      labelInput.setAttribute("data-setting-field", "label");
      labelInput.setAttribute("data-node-id", single.id);
      labelInput.addEventListener("focus", function() { instance.settingsPreferredField = "label"; });
      labelInput.addEventListener("input", function() { instance.settingsPreferredField = "label"; });
      labelInput.addEventListener("change", function() {
        instance.settingsPreferredField = "label";
        commit(function() { single.canvasLabel = String(labelInput.value || "").trim(); });
      });
      field(ko ? "라벨" : "Label", labelInput);
      if (single.role === "latent") {
        var advancedInput = document.createElement("input");
        advancedInput.type = "checkbox";
        advancedInput.checked = !!single.advancedConstructSpecification;
        advancedInput.addEventListener("change", function() {
          commit(function() { single.advancedConstructSpecification = !!advancedInput.checked; });
        });
        field(ko ? "고급 구성개념 명세" : "Advanced construct specification", advancedInput);
        var advancedConstruct = !!single.advancedConstructSpecification;
        var constructSelect = document.createElement("select");
        constructSelect.className = "form-control input-sm";
        var constructChoices = single.constructType === "higherOrder" ?
          [["higherOrder", ko ? "고차 공통요인" : "Higher-order common factor"]] :
          [["commonFactor", ko ? "잠재특성·공통요인" : "Latent trait / common factor"], ["composite", ko ? "구성지수·합성변수" : "Constructed index / composite"]];
        constructChoices.forEach(function(item) {
          var option = document.createElement("option"); option.value = item[0]; option.textContent = item[1];
          if (["cfa", "cbsem", "sem"].indexOf(instance.analysisType) >= 0 && item[0] === "composite") option.disabled = true;
          constructSelect.appendChild(option);
        });
        constructSelect.value = single.constructType || ((single.measurementMode || "reflective") === "formative" ? "composite" : "commonFactor");
        if (single.constructType === "higherOrder") constructSelect.disabled = true;
        constructSelect.addEventListener("change", function() {
          var selectedType = constructSelect.value;
          commit(function() {
            single.constructType = selectedType;
            if (!advancedConstruct && selectedType !== "unspecified") single.weightingMode = "auto";
          });
          if (!advancedConstruct && selectedType !== "unspecified") setMeasurementMode(instance, selectedType === "composite" ? "formative" : "reflective");
        });
        field(advancedConstruct ? (ko ? "구성개념 유형" : "Construct ontology") : (ko ? "구성개념 모형" : "Construct model"), constructSelect);
        if (!advancedConstruct) {
          var constructNote = document.createElement("div");
          constructNote.className = "structural-option-note";
          var covarianceEngine = ["cfa", "cbsem", "sem"].indexOf(instance.analysisType) >= 0;
          constructNote.textContent = covarianceEngine ? (ko ? "현재 엔진은 반영형 공통요인만 지원합니다." : "The current engine supports reflective common factors only.") : (ko ? "공통요인은 반영형 Mode A로, 합성변수는 형성형 Mode B로 자동 설정됩니다." : "Common factors default to reflective Mode A blocks; composites default to formative Mode B blocks.");
          field("", constructNote);
        }
        if (advancedConstruct && instance.analysisType === "plssem") {
          var weightSelect = document.createElement("select");
          weightSelect.className = "form-control input-sm";
          [["auto", ko ? "자동" : "Automatic"], ["modeA", "Mode A"], ["modeB", "Mode B"]].forEach(function(item) {
            var option = document.createElement("option"); option.value = item[0]; option.textContent = item[1]; weightSelect.appendChild(option);
          });
          weightSelect.value = single.weightingMode || "auto";
          weightSelect.addEventListener("change", function() { commit(function() { single.weightingMode = weightSelect.value; }); });
          field(ko ? "PLS 가중 방식" : "PLS weighting", weightSelect);
        }
        if (instance.analysisType === "plssem" && constructSelect.value === "composite" && (single.measurementMode || "reflective") === "formative") {
          function constructEvidenceField(label, property, placeholder) {
            var textarea = document.createElement("textarea");
            textarea.className = "form-control input-sm";
            textarea.rows = 3;
            textarea.value = single[property] || "";
            textarea.placeholder = placeholder;
            textarea.addEventListener("change", function() {
              commit(function() { single[property] = String(textarea.value || "").trim(); });
            });
            field(label, textarea);
          }
          constructEvidenceField(ko ? "개념 영역 정의" : "Domain definition", "compositeDomainDefinition", ko ? "합성변수가 포괄해야 하는 개념 영역" : "Conceptual domain the composite must cover");
          constructEvidenceField(ko ? "지표 포함 근거" : "Indicator inclusion rationale", "compositeIndicatorRationale", ko ? "각 지표가 포함된 이론적·내용적 근거" : "Theoretical and content rationale for indicator inclusion");
          constructEvidenceField(ko ? "내용타당도 절차·출처" : "Content-validity procedure/source", "compositeContentValidityEvidence", ko ? "전문가 검토, 문헌, 척도개발 절차 또는 출처" : "Expert review, literature, scale-development procedure, or source");
        }
      }
      if (selectionChanged) {
        var preferredInput = instance.settingsPreferredField === "name" && nameEditable ? nameInput :
          instance.settingsPreferredField === "label" ? labelInput :
          nameEditable ? nameInput : labelInput;
        window.setTimeout(function() {
          if (!preferredInput || !preferredInput.isConnected) return;
          preferredInput.focus();
          if (typeof preferredInput.select === "function") preferredInput.select();
        }, 0);
      }
    }

    var sizeInput = document.createElement("input");
    sizeInput.type = "number";
    sizeInput.min = "8";
    sizeInput.max = "32";
    sizeInput.step = "1";
    sizeInput.className = "form-control input-sm";
    sizeInput.value = single ? Number(single.fontSize || instance.state.style.fontSize) : Number(nodes[0].fontSize || instance.state.style.fontSize);
    sizeInput.addEventListener("change", function() {
      var size = Math.max(8, Math.min(32, Number(sizeInput.value || 11)));
      commit(function() { nodes.forEach(function(node) { node.fontSize = size; node.customFontSize = true; }); });
    });
    field(ko ? "폰트 크기" : "Font size", sizeInput);

    var familyInput = document.createElement("select");
    familyInput.className = "form-control input-sm";
    ["Arial", "Noto Sans KR", "Times New Roman"].forEach(function(family) {
      var option = document.createElement("option");
      option.value = family;
      option.textContent = family;
      familyInput.appendChild(option);
    });
    familyInput.value = single ? (single.fontFamily || instance.state.style.fontFamily) : (nodes[0].fontFamily || instance.state.style.fontFamily);
    familyInput.addEventListener("change", function() {
      commit(function() { nodes.forEach(function(node) { node.fontFamily = familyInput.value; }); });
    });
    field(ko ? "폰트" : "Font", familyInput);

    var fillRow = document.createElement("div");
    fillRow.className = "structural-color-control";
    var fillInput = document.createElement("input");
    fillInput.type = "color";
    fillInput.value = single && single.fillColor && single.fillColor !== "transparent" ? single.fillColor : "#ffffff";
    var transparentLabel = document.createElement("label");
    var transparentInput = document.createElement("input");
    transparentInput.type = "checkbox";
    transparentInput.checked = nodes.every(function(node) { return !node.fillColor || node.fillColor === "transparent"; });
    transparentLabel.appendChild(transparentInput);
    transparentLabel.appendChild(document.createTextNode(ko ? " 투명" : " Transparent"));
    fillRow.appendChild(fillInput);
    fillRow.appendChild(transparentLabel);
    var currentFill = single ? (single.fillColor || "transparent") : (nodes[0].fillColor || "transparent");
    fillRow.appendChild(colorPalette(["#ffffff", "#f1f5f9", "#dbeafe", "#dcfce7", "#fef3c7", "#fee2e2", "#f3e8ff"], currentFill, function(color) {
      commit(function() { nodes.forEach(function(node) { node.fillColor = color; }); });
    }, true));
    fillInput.addEventListener("change", function() {
      transparentInput.checked = false;
      commit(function() { nodes.forEach(function(node) { node.fillColor = fillInput.value; }); });
    });
    transparentInput.addEventListener("change", function() {
      commit(function() { nodes.forEach(function(node) { node.fillColor = transparentInput.checked ? "transparent" : fillInput.value; }); });
    });
    field(ko ? "내부 색상" : "Fill", fillRow);

    var strokeRow = document.createElement("div");
    strokeRow.className = "structural-color-control";
    var strokeInput = document.createElement("input");
    strokeInput.type = "color";
    strokeInput.value = single && single.strokeColor ? single.strokeColor : "#000000";
    strokeInput.addEventListener("change", function() {
      commit(function() { nodes.forEach(function(node) { node.strokeColor = strokeInput.value; }); });
    });
    strokeRow.appendChild(strokeInput);
    var currentStroke = single ? (single.strokeColor || "#000000") : (nodes[0].strokeColor || "#000000");
    strokeRow.appendChild(colorPalette(["#000000", "#475569", "#2563eb", "#16a34a", "#dc2626", "#7c3aed", "#ea580c"], currentStroke, function(color) {
      commit(function() { nodes.forEach(function(node) { node.strokeColor = color; }); });
    }, false));
    field(ko ? "외곽선 색상" : "Outline", strokeRow);
  }

  function updateDisturbancePositionToolbar(instance, node, ko) {
    var toolbar = instance.root.querySelector(".structural-disturbance-toolbar");
    if (!toolbar) return;
    toolbar.innerHTML = "";
    toolbar.classList.toggle("is-visible", !!node && node.role === "disturbance");
    if (!node || node.role !== "disturbance") return;

    var label = document.createElement("span");
    label.className = "structural-disturbance-toolbar-label";
    label.textContent = ko ? "구조오차 위치" : "Disturbance";
    toolbar.appendChild(label);

    var positionGrid = document.createElement("div");
    positionGrid.className = "structural-disturbance-position-grid";
    [
      ["nw", "↖", ko ? "왼쪽 위" : "Top left"], ["n", "↑", ko ? "위" : "Top"], ["ne", "↗", ko ? "오른쪽 위" : "Top right"],
      ["w", "←", ko ? "왼쪽" : "Left"], ["auto", "A", ko ? "자동 배치" : "Automatic"], ["e", "→", ko ? "오른쪽" : "Right"],
      ["sw", "↙", ko ? "왼쪽 아래" : "Bottom left"], ["s", "↓", ko ? "아래" : "Bottom"], ["se", "↘", ko ? "오른쪽 아래" : "Bottom right"]
    ].forEach(function(item) {
      var button = document.createElement("button");
      button.type = "button";
      button.className = "structural-disturbance-position-button";
      button.textContent = item[1];
      button.title = item[2];
      button.setAttribute("aria-label", item[2]);
      button.classList.toggle("is-selected", (node.disturbancePosition || "auto") === item[0]);
      button.addEventListener("click", function() {
        window.StatEduModelCanvas.state.pushHistory(instance);
        node.disturbancePosition = item[0];
        reflowMeasurementModel(instance);
        render(instance);
        window.StatEduModelCanvas.bridge.sendState(instance);
      });
      positionGrid.appendChild(button);
    });
    toolbar.appendChild(positionGrid);
  }

  function flushActiveSetting(instance) {
    var active = document.activeElement;
    if (!active || !instance.root.contains(active)) return;
    var fieldName = active.getAttribute && active.getAttribute("data-setting-field");
    var nodeId = active.getAttribute && active.getAttribute("data-node-id");
    if (!fieldName || !nodeId) return;
    var node = window.StatEduModelCanvas.nodes.nodeById(instance, nodeId);
    if (!node) return;
    var value = String(active.value || "").trim();
    if (!value && fieldName === "name") return;
    var current = fieldName === "name" ? String(node.name || "") : String(node.canvasLabel || node.dataLabel || node.name || "");
    instance.settingsPreferredField = fieldName;
    if (value === current) return;
    window.StatEduModelCanvas.state.pushHistory(instance);
    if (fieldName === "name" && (node.role === "latent" || node.role === "error")) {
      node.name = value;
      if (node.role === "latent") node.dataLabel = value;
      if (node.role === "error") node.autoErrorNotation = false;
    } else if (fieldName === "label") {
      node.canvasLabel = value;
    }
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function selectedVariableNames(instance) {
    var names = instance.state.selectedVariables || [];
    if (names.length > 0) return names.slice();
    return instance.state.selectedVariable ? [instance.state.selectedVariable] : [];
  }

  function variableItems(instance) {
    return Array.from(instance.root.querySelectorAll(".custom-model-variable-item"));
  }

  function reflowMeasurementModel(instance) {
    if (!isStructuralCanvas(instance)) return;
    var structuralEdges = instance.state.edges.filter(function(edge) {
      if (edge.kind === "covariance") return false;
      var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
      var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
      return from && to && from.role === "latent" && to.role === "latent";
    });
    var errorNotationCounters = {delta: 0, epsilon: 0};
    function measurementEdgesForLatent(latent) {
      return instance.state.edges.filter(function(edge) {
        if (edge.kind === "covariance") return false;
        var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
        var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
        return (from && from.id === latent.id && to && to.role === "indicator") ||
          (to && to.id === latent.id && from && from.role === "indicator");
      });
    }
    function avoidVerticalMeasurementOverlap(latents) {
      var style = instance.state.style || {};
      var indicatorHeight = Number(style.boxHeight || 38);
      var latentHeightFallback = Number(style.latentHeight || 58);
      var rowGap = Math.max(48, indicatorHeight + 10);
      var groupGap = rowGap;
      var groups = latents.map(function(latent) {
        var edges = measurementEdgesForLatent(latent);
        if (!edges.length) return null;
        var count = edges.length;
        var latentY = Number(latent.y || 0);
        var latentHeight = Number(latent.height || latentHeightFallback);
        var latentMiddle = latentY + latentHeight / 2;
        var top = latentMiddle - indicatorHeight / 2 - (count - 1) * rowGap / 2;
        var bottom = top + indicatorHeight + (count - 1) * rowGap;
        return {
          latent: latent,
          top: Math.min(top, latentY) - 6,
          bottom: Math.max(bottom, latentY + latentHeight) + 6
        };
      }).filter(Boolean).sort(function(a, b) {
        return Number(a.latent.y || 0) - Number(b.latent.y || 0);
      });
      var nextTop = null;
      groups.forEach(function(group) {
        if (nextTop !== null && group.top < nextTop) {
          var shift = nextTop - group.top;
          group.latent.y = Number(group.latent.y || 0) + shift;
          group.top += shift;
          group.bottom += shift;
        }
        nextTop = group.bottom + groupGap;
      });
    }
    if (instance.analysisType === "cfa") {
      var cfaLatents = instance.state.nodes.filter(function(node) { return node.role === "latent"; }).sort(function(a, b) {
        return Number(a.y || 0) - Number(b.y || 0);
      });
      cfaLatents.forEach(function(latent) {
        var cfaEdges = measurementEdgesForLatent(latent);
        if (!cfaEdges.length) return;
        latent.manualPosition = true;
      });
      avoidVerticalMeasurementOverlap(cfaLatents);
    }
    instance.state.nodes.filter(function(node) { return node.role === "latent"; }).forEach(function(latent) {
      var hasIncoming = structuralEdges.some(function(edge) { return edge.to === latent.id; });
      var disturbanceEdge = instance.state.edges.find(function(edge) {
        var source = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
        return edge.to === latent.id && source && source.role === "disturbance";
      });
      var disturbance = disturbanceEdge ? window.StatEduModelCanvas.nodes.nodeById(instance, disturbanceEdge.from) : null;
      if (hasIncoming && !disturbance) {
        disturbance = window.StatEduModelCanvas.nodes.createDisturbanceNode(instance, latent, instance.state.nodes.filter(function(node) { return node.role === "disturbance"; }).length + 1);
        instance.state.nodes.push(disturbance);
        window.StatEduModelCanvas.edges.createEdge(instance, disturbance.id, latent.id);
        disturbanceEdge = instance.state.edges.find(function(edge) { return edge.from === disturbance.id && edge.to === latent.id; });
        if (disturbanceEdge) disturbanceEdge.type = "structuralError";
      } else if (!hasIncoming && disturbance) {
        instance.state.edges = instance.state.edges.filter(function(edge) { return edge.from !== disturbance.id && edge.to !== disturbance.id; });
        instance.state.nodes = instance.state.nodes.filter(function(node) { return node.id !== disturbance.id; });
      }
    });
    instance.state.nodes.filter(function(node) { return node.role === "latent"; }).forEach(function(latent) {
      var incoming = structuralEdges.some(function(edge) { return edge.to === latent.id; });
      var outgoing = structuralEdges.some(function(edge) { return edge.from === latent.id; });
      var placement = latent.measurementPlacement || (incoming && outgoing ? "top" : (incoming ? "right" : "left"));
      latent.effectiveMeasurementPlacement = placement;
      var measurementMode = latent.measurementMode || "reflective";
      var disturbanceEdge = instance.state.edges.find(function(edge) {
        var source = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
        return edge.to === latent.id && source && source.role === "disturbance";
      });
      var disturbance = disturbanceEdge ? window.StatEduModelCanvas.nodes.nodeById(instance, disturbanceEdge.from) : null;
      if (disturbance) {
        var disturbancePosition = disturbance.disturbancePosition || "auto";
        if (disturbancePosition === "auto") disturbancePosition = placement === "top" ? "s" : "n";
        var latentX = Number(latent.x || 0);
        var latentY = Number(latent.y || 0);
        var latentWidth = Number(latent.width || 90);
        var latentHeight = Number(latent.height || 44);
        var disturbanceWidth = Number(disturbance.width || 26);
        var disturbanceHeight = Number(disturbance.height || 26);
        var horizontalGap = 38;
        var verticalGap = 38;
        var positionMap = {
          nw: {x: latentX - disturbanceWidth - horizontalGap, y: latentY - disturbanceHeight - verticalGap, fromSide: "right", toSide: "left"},
          n: {x: latentX + latentWidth / 2 - disturbanceWidth / 2, y: latentY - disturbanceHeight - verticalGap, fromSide: "bottom", toSide: "top"},
          ne: {x: latentX + latentWidth + horizontalGap, y: latentY - disturbanceHeight - verticalGap, fromSide: "left", toSide: "right"},
          e: {x: latentX + latentWidth + horizontalGap, y: latentY + latentHeight / 2 - disturbanceHeight / 2, fromSide: "left", toSide: "right"},
          se: {x: latentX + latentWidth + horizontalGap, y: latentY + latentHeight + verticalGap, fromSide: "left", toSide: "right"},
          s: {x: latentX + latentWidth / 2 - disturbanceWidth / 2, y: latentY + latentHeight + verticalGap, fromSide: "top", toSide: "bottom"},
          sw: {x: latentX - disturbanceWidth - horizontalGap, y: latentY + latentHeight + verticalGap, fromSide: "right", toSide: "left"},
          w: {x: latentX - disturbanceWidth - horizontalGap, y: latentY + latentHeight / 2 - disturbanceHeight / 2, fromSide: "right", toSide: "left"}
        };
        var disturbanceLayout = positionMap[disturbancePosition] || positionMap.n;
        var disturbanceOffset = Number(disturbance.manualOffset || 0);
        var diagonal = Math.sqrt(0.5);
        var disturbanceVectors = {
          nw: {x: -diagonal, y: -diagonal}, n: {x: 0, y: -1}, ne: {x: diagonal, y: -diagonal},
          w: {x: -1, y: 0}, e: {x: 1, y: 0},
          sw: {x: -diagonal, y: diagonal}, s: {x: 0, y: 1}, se: {x: diagonal, y: diagonal}
        };
        var disturbanceVector = disturbanceVectors[disturbancePosition] || disturbanceVectors.n;
        disturbance.x = disturbanceLayout.x + disturbanceVector.x * disturbanceOffset;
        disturbance.y = disturbanceLayout.y + disturbanceVector.y * disturbanceOffset;
        Object.assign(disturbanceEdge, {
          fromSide: disturbanceLayout.fromSide,
          toSide: disturbanceLayout.toSide,
          fixedCenter: true,
          directAnchors: true
        });
      }
      var measurementEdges = measurementEdgesForLatent(latent);
      var indicators = measurementEdges.map(function(edge) {
        var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
        return from && from.role === "indicator" ? from : window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
      });
      var count = indicators.length;
      indicators.forEach(function(indicator, index) {
        var measurementEdge = measurementEdges.find(function(edge) { return edge.from === indicator.id || edge.to === indicator.id; });
        var errorEdge = instance.state.edges.find(function(edge) {
          var source = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
          return edge.to === indicator.id && source && source.role === "error";
        });
        var errorNode = errorEdge ? window.StatEduModelCanvas.nodes.nodeById(instance, errorEdge.from) : null;
        if (errorNode) {
          instance.state.edges.forEach(function(edge) {
            if (edge.kind !== "covariance" || (edge.from !== errorNode.id && edge.to !== errorNode.id)) return;
            var otherError = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from === errorNode.id ? edge.to : edge.from);
            if (otherError && otherError.role === "error") {
              edge.curveDirection = placement;
              edge.fromSide = placement;
              edge.toSide = placement;
              edge.fixedCenter = true;
            }
          });
        }
        if (errorNode && errorNode.autoErrorNotation !== false) {
          var errorKind = incoming ? "epsilon" : "delta";
          var errorSymbol = incoming ? "ε" : "δ";
          errorNotationCounters[errorKind] += 1;
          errorNode.name = errorKind + errorNotationCounters[errorKind];
          errorNode.dataLabel = errorSymbol + errorNotationCounters[errorKind];
        }
        var iw = Number(indicator.width || instance.state.style.boxWidth || 110);
        var ih = Number(indicator.height || instance.state.style.boxHeight || 38);
        var lw = Number(latent.width || 120);
        var lh = Number(latent.height || 58);
        if (placement === "left") {
          indicator.x = Number(latent.x || 0) - iw - 50;
          indicator.y = Number(latent.y || 0) + lh / 2 - ih / 2 + (index - (count - 1) / 2) * 48;
          if (errorNode) {
            var leftErrorOffset = Number(errorNode.manualOffset || 0);
            errorNode.x = indicator.x - Number(errorNode.width || 26) - 28 - leftErrorOffset;
            errorNode.y = indicator.y + ih / 2 - Number(errorNode.height || 26) / 2;
          }
          if (measurementEdge) Object.assign(measurementEdge, {fromSide: measurementMode === "formative" ? "right" : "left", toSide: measurementMode === "formative" ? "left" : "right", fixedCenter: measurementAnchorFixedCenter(measurementMode), directAnchors: false});
          if (errorEdge) Object.assign(errorEdge, {fromSide: "right", toSide: "left", fixedCenter: true});
        } else if (placement === "right") {
          indicator.x = Number(latent.x || 0) + lw + 50;
          indicator.y = Number(latent.y || 0) + lh / 2 - ih / 2 + (index - (count - 1) / 2) * 48;
          if (errorNode) {
            var rightErrorOffset = Number(errorNode.manualOffset || 0);
            errorNode.x = indicator.x + iw + 28 + rightErrorOffset;
            errorNode.y = indicator.y + ih / 2 - Number(errorNode.height || 26) / 2;
          }
          if (measurementEdge) Object.assign(measurementEdge, {fromSide: measurementMode === "formative" ? "left" : "right", toSide: measurementMode === "formative" ? "right" : "left", fixedCenter: measurementAnchorFixedCenter(measurementMode), directAnchors: false});
          if (errorEdge) Object.assign(errorEdge, {fromSide: "left", toSide: "right", fixedCenter: true});
        } else if (placement === "top") {
          var gap = iw + 24;
          indicator.x = Number(latent.x || 0) + lw / 2 - iw / 2 + (index - (count - 1) / 2) * gap;
          indicator.y = Number(latent.y || 0) - ih - 50;
          if (errorNode) {
            var topErrorOffset = Number(errorNode.manualOffset || 0);
            errorNode.x = indicator.x + iw / 2 - Number(errorNode.width || 26) / 2;
            errorNode.y = indicator.y - Number(errorNode.height || 26) - 28 - topErrorOffset;
          }
          if (measurementEdge) Object.assign(measurementEdge, {fromSide: measurementMode === "formative" ? "bottom" : "top", toSide: measurementMode === "formative" ? "top" : "bottom", fixedCenter: measurementAnchorFixedCenter(measurementMode), directAnchors: false});
          if (errorEdge) Object.assign(errorEdge, {fromSide: "bottom", toSide: "top", fixedCenter: true});
        } else {
          var bottomGap = iw + 24;
          indicator.x = Number(latent.x || 0) + lw / 2 - iw / 2 + (index - (count - 1) / 2) * bottomGap;
          indicator.y = Number(latent.y || 0) + lh + 50;
          if (errorNode) {
            var bottomErrorOffset = Number(errorNode.manualOffset || 0);
            errorNode.x = indicator.x + iw / 2 - Number(errorNode.width || 26) / 2;
            errorNode.y = indicator.y + ih + 28 + bottomErrorOffset;
          }
          if (measurementEdge) Object.assign(measurementEdge, {fromSide: measurementMode === "formative" ? "top" : "bottom", toSide: measurementMode === "formative" ? "bottom" : "top", fixedCenter: measurementAnchorFixedCenter(measurementMode), directAnchors: false});
          if (errorEdge) Object.assign(errorEdge, {fromSide: "top", toSide: "bottom", fixedCenter: true});
        }
      });
    });
  }

  function selectedLatentNodes(instance) {
    var ids = instance.state.selectedNodeIds || [];
    return instance.state.nodes.filter(function(node) { return node.role === "latent" && ids.indexOf(node.id) >= 0; });
  }

  function selectedIndicatorNodes(instance) {
    var ids = instance.state.selectedNodeIds || [];
    return instance.state.nodes.filter(function(node) { return node.role === "indicator" && ids.indexOf(node.id) >= 0; });
  }

  function detachSelectedIndicators(instance) {
    var indicators = selectedIndicatorNodes(instance);
    if (!indicators.length) return false;
    var ids = indicators.map(function(node) { return node.id; });
    window.StatEduModelCanvas.state.pushHistory(instance);
    instance.state.edges = instance.state.edges.filter(function(edge) {
      var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
      var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
      return !((from && from.role === "latent" && to && ids.indexOf(to.id) >= 0) || (to && to.role === "latent" && from && ids.indexOf(from.id) >= 0));
    });
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    return true;
  }

  function moveSelectedIndicator(instance, direction) {
    var indicator = selectedIndicatorNodes(instance)[0];
    if (!indicator) return false;
    var edge = instance.state.edges.find(function(item) {
      var from = window.StatEduModelCanvas.nodes.nodeById(instance, item.from);
      var to = window.StatEduModelCanvas.nodes.nodeById(instance, item.to);
      return (from && from.role === "latent" && to && to.id === indicator.id) || (to && to.role === "latent" && from && from.id === indicator.id);
    });
    if (!edge) return false;
    var latentId = edge.from === indicator.id ? edge.to : edge.from;
    var peers = instance.state.edges.filter(function(item) {
      if (item.kind === "covariance") return false;
      var from = window.StatEduModelCanvas.nodes.nodeById(instance, item.from);
      var to = window.StatEduModelCanvas.nodes.nodeById(instance, item.to);
      return (from && from.id === latentId && to && to.role === "indicator") || (to && to.id === latentId && from && from.role === "indicator");
    });
    var index = peers.indexOf(edge);
    var swap = direction < 0 ? index - 1 : index + 1;
    if (index < 0 || swap < 0 || swap >= peers.length) return false;
    window.StatEduModelCanvas.state.pushHistory(instance);
    var firstIndex = instance.state.edges.indexOf(peers[index]);
    var secondIndex = instance.state.edges.indexOf(peers[swap]);
    var temp = instance.state.edges[firstIndex]; instance.state.edges[firstIndex] = instance.state.edges[secondIndex]; instance.state.edges[secondIndex] = temp;
    reflowMeasurementModel(instance);
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    return true;
  }

  function nextErrorSequence(instance) {
    var values = instance.state.nodes.filter(function(node) { return node.role === "error"; }).map(function(node) {
      var match = String(node.name || "").match(/(\d+)$/);
      return match ? Number(match[1]) : 0;
    });
    return (values.length ? Math.max.apply(Math, values) : 0) + 1;
  }

  function latentHasIncomingStructuralPath(instance, latent) {
    return instance.state.edges.some(function(edge) {
      if (edge.kind === "covariance" || edge.to !== latent.id) return false;
      var source = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
      return source && source.role === "latent";
    });
  }

  function setMeasurementPlacement(instance, placement) {
    var latents = selectedLatentNodes(instance);
    if (!latents.length || ["left", "right", "top", "bottom"].indexOf(placement) < 0) return false;
    window.StatEduModelCanvas.state.pushHistory(instance);
    latents.forEach(function(latent) { latent.measurementPlacement = placement; });
    reflowMeasurementModel(instance);
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    return true;
  }

  function placeErrorNearIndicator(instance, errorNode, indicator, placement) {
    var iw = Number(indicator.width || instance.state.style.boxWidth || 110);
    var ih = Number(indicator.height || instance.state.style.boxHeight || 38);
    var ew = Number(errorNode.width || 26);
    var eh = Number(errorNode.height || 26);
    if (placement === "left") {
      errorNode.x = Number(indicator.x || 0) - ew - 28;
      errorNode.y = Number(indicator.y || 0) + ih / 2 - eh / 2;
    } else if (placement === "right") {
      errorNode.x = Number(indicator.x || 0) + iw + 28;
      errorNode.y = Number(indicator.y || 0) + ih / 2 - eh / 2;
    } else if (placement === "top") {
      errorNode.x = Number(indicator.x || 0) + iw / 2 - ew / 2;
      errorNode.y = Number(indicator.y || 0) - eh - 28;
    } else {
      errorNode.x = Number(indicator.x || 0) + iw / 2 - ew / 2;
      errorNode.y = Number(indicator.y || 0) + ih + 28;
    }
  }

  function applyAnalysisCanvasDefaults(instance) {
    if (!instance || instance.analysisType !== "cfa") return;
    Object.assign(instance.state.canvas, {
      paper: "B5",
      orientation: "portrait",
      widthMm: 182,
      heightMm: 257,
      widthPx: 688,
      heightPx: 971
    });
  }

  function applyMeasurementAnchors(edge, mode, placement) {
    edge.type = "measurement";
    edge.directAnchors = false;
    edge.fixedCenter = measurementAnchorFixedCenter(mode);
    if (placement === "left") {
      Object.assign(edge, {fromSide: mode === "formative" ? "right" : "left", toSide: mode === "formative" ? "left" : "right"});
    } else if (placement === "right") {
      Object.assign(edge, {fromSide: mode === "formative" ? "left" : "right", toSide: mode === "formative" ? "right" : "left"});
    } else if (placement === "top") {
      Object.assign(edge, {fromSide: mode === "formative" ? "bottom" : "top", toSide: mode === "formative" ? "top" : "bottom"});
    } else {
      Object.assign(edge, {fromSide: mode === "formative" ? "top" : "bottom", toSide: mode === "formative" ? "bottom" : "top"});
    }
  }

  function measurementAnchorFixedCenter(mode) {
    return mode !== "formative";
  }

  function measurementPlacementForLatent(instance, latent, relatedEdges) {
    if (["left", "right", "top", "bottom"].indexOf(latent.measurementPlacement) >= 0) return latent.measurementPlacement;
    if (["left", "right", "top", "bottom"].indexOf(latent.effectiveMeasurementPlacement) >= 0) return latent.effectiveMeasurementPlacement;
    var indicators = (relatedEdges || []).map(function(edge) {
      var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
      return from && from.role === "indicator" ? from : window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
    }).filter(Boolean);
    if (!indicators.length) return "right";
    var style = instance.state.style || {};
    var latentCenter = window.StatEduModelCanvas.layout.nodeCenter(latent, style);
    var average = indicators.reduce(function(total, indicator) {
      var center = window.StatEduModelCanvas.layout.nodeCenter(indicator, style);
      total.x += Number(center.x || 0);
      total.y += Number(center.y || 0);
      return total;
    }, {x: 0, y: 0});
    average.x /= indicators.length;
    average.y /= indicators.length;
    var dx = average.x - Number(latentCenter.x || 0);
    var dy = average.y - Number(latentCenter.y || 0);
    if (Math.abs(dx) >= Math.abs(dy)) return dx < 0 ? "left" : "right";
    return dy < 0 ? "top" : "bottom";
  }

  function applyErrorAnchors(edge, placement) {
    edge.fixedCenter = true;
    if (placement === "left") Object.assign(edge, {fromSide: "right", toSide: "left"});
    else if (placement === "right") Object.assign(edge, {fromSide: "left", toSide: "right"});
    else if (placement === "top") Object.assign(edge, {fromSide: "bottom", toSide: "top"});
    else Object.assign(edge, {fromSide: "top", toSide: "bottom"});
  }

  function setMeasurementMode(instance, mode) {
    var latents = selectedLatentNodes(instance);
    if (!latents.length || ["reflective", "formative"].indexOf(mode) < 0) return false;
    if (mode === "formative" && ["cfa", "cbsem", "sem"].indexOf(instance.analysisType) >= 0) return false;
    window.StatEduModelCanvas.state.pushHistory(instance);
    latents.forEach(function(latent) {
      latent.measurementMode = mode;
      if (!latent.advancedConstructSpecification) {
        latent.constructType = mode === "formative" ? "composite" : "commonFactor";
        latent.weightingMode = "auto";
      }
      var relatedEdges = instance.state.edges.filter(function(edge) {
        var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
        var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
        return (from && from.id === latent.id && to && to.role === "indicator") || (to && to.id === latent.id && from && from.role === "indicator");
      });
      var placement = measurementPlacementForLatent(instance, latent, relatedEdges);
      relatedEdges.forEach(function(edge) {
        var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
        var indicator = from && from.role === "indicator" ? from : window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
        if (!indicator) return;
        edge.from = mode === "formative" ? indicator.id : latent.id;
        edge.to = mode === "formative" ? latent.id : indicator.id;
        applyMeasurementAnchors(edge, mode, placement);
        if (mode === "formative") {
          var errorIds = instance.state.edges.filter(function(item) { return item.to === indicator.id; }).map(function(item) {
            var source = window.StatEduModelCanvas.nodes.nodeById(instance, item.from);
            return source && source.role === "error" ? source.id : null;
          }).filter(Boolean);
          instance.state.edges = instance.state.edges.filter(function(item) {
            return errorIds.indexOf(item.from) < 0 && errorIds.indexOf(item.to) < 0;
          });
          instance.state.nodes = instance.state.nodes.filter(function(node) { return errorIds.indexOf(node.id) < 0; });
        } else {
          var hasError = instance.state.edges.some(function(item) {
            var source = window.StatEduModelCanvas.nodes.nodeById(instance, item.from);
            return item.to === indicator.id && source && source.role === "error";
          });
          if (!hasError) {
            var sequence = nextErrorSequence(instance);
            var errorNode = window.StatEduModelCanvas.nodes.createErrorNode(instance, indicator, indicator.x, indicator.y, sequence);
            if (errorNode.autoErrorNotation !== false) {
              var endogenous = latentHasIncomingStructuralPath(instance, latent);
              errorNode.name = (endogenous ? "epsilon" : "delta") + sequence;
              errorNode.dataLabel = (endogenous ? "ε" : "δ") + sequence;
            }
            placeErrorNearIndicator(instance, errorNode, indicator, placement);
            instance.state.nodes.push(errorNode);
            window.StatEduModelCanvas.edges.createEdge(instance, errorNode.id, indicator.id);
            var newErrorEdge = instance.state.edges.find(function(item) { return item.from === errorNode.id && item.to === indicator.id; });
            if (newErrorEdge) applyErrorAnchors(newErrorEdge, placement);
          } else {
            instance.state.edges.forEach(function(item) {
              var source = window.StatEduModelCanvas.nodes.nodeById(instance, item.from);
              if (item.to === indicator.id && source && source.role === "error") applyErrorAnchors(item, placement);
            });
          }
        }
      });
    });
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    return true;
  }

  function selectVariable(instance, item, event) {
    var name = item ? item.getAttribute("data-variable-name") : "";
    if (!name) return;
    var items = variableItems(instance);
    var index = items.indexOf(item);
    var selected = instance.state.selectedVariables || [];

    if (event && event.shiftKey && instance.state.lastSelectedVariableIndex !== null) {
      var start = Math.min(instance.state.lastSelectedVariableIndex, index);
      var end = Math.max(instance.state.lastSelectedVariableIndex, index);
      selected = items.slice(start, end + 1).map(function(listItem) {
        return listItem.getAttribute("data-variable-name") || "";
      }).filter(Boolean);
    } else if (event && (event.ctrlKey || event.metaKey)) {
      selected = selected.slice();
      var selectedIndex = selected.indexOf(name);
      if (selectedIndex >= 0) {
        selected.splice(selectedIndex, 1);
      } else {
        selected.push(name);
      }
      instance.state.lastSelectedVariableIndex = index;
    } else {
      selected = [name];
      instance.state.lastSelectedVariableIndex = index;
    }

    instance.state.selectedVariables = selected;
    instance.state.selectedVariable = selected.length ? selected[selected.length - 1] : null;
    window.StatEduModelCanvas.nodes.setVariableUsage(instance);
    window.StatEduModelCanvas.toolbar.updateButtons(instance);
  }

  function existingNodeByVariable(instance, name) {
    return instance.state.nodes.find(function(node) {
      return node.variableId === name;
    });
  }

  function currentVariableRole(instance, name) {
    var node = existingNodeByVariable(instance, name);
    if (node) return node.role || "independent";
    if (instance.state.covariates.indexOf(name) >= 0) return "covariate";
    return "";
  }

  function roleCount(instance, role, excludingName) {
    if (role === "covariate") {
      return instance.state.covariates.filter(function(name) {
        return name !== excludingName;
      }).length;
    }
    return instance.state.nodes.filter(function(node) {
      return node.variableId !== excludingName && node.role === role;
    }).length;
  }

  function canAssignRole(instance, role, variableName) {
    var limit = window.StatEduModelCanvas.state.ROLE_LIMITS[role];
    if (!Number.isFinite(limit)) return true;
    if (roleCount(instance, role, variableName) < limit) return true;
    window.alert((window.StatEduModelCanvas.state.ROLE_LABELS_KO[role] || role) + "\uc740(\ub294) \ud604\uc7ac 1\uac1c\ub9cc \uc120\ud0dd\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4.");
    return false;
  }

  function removeVariableRole(instance, name) {
    var node = existingNodeByVariable(instance, name);
    if (node) {
      window.StatEduModelCanvas.nodes.deleteNode(instance, node.id);
    }
    instance.state.covariates = instance.state.covariates.filter(function(item) {
      return item !== name;
    });
    if (instance.state.covariateTypes) delete instance.state.covariateTypes[name];
    if (instance.state.covariateTargets) delete instance.state.covariateTargets[name];
  }

  function covariateTypeForVariable(variable) {
    var measurement = String(variable && variable.measurement || "").toLowerCase();
    var factorMeasurements = ["binary", "category", "categorical", "factor", "nominal", "ordinal", "ordered"];
    return {
      measurement: measurement || "continuous",
      encoding: factorMeasurements.indexOf(measurement) >= 0 ? "factor" : "continuous"
    };
  }

  function applyRoleToVariable(instance, name, role) {
    var variable = variableByName(instance, name);
    if (!variable) return false;
    var currentRole = currentVariableRole(instance, name);

    if (currentRole === role) {
      removeVariableRole(instance, name);
      return true;
    }

    if (!canAssignRole(instance, role, name)) return false;

    if (role === "covariate") {
      removeVariableRole(instance, name);
      if (instance.state.covariates.indexOf(name) < 0) {
        instance.state.covariates.push(name);
      }
      instance.state.covariateTypes = instance.state.covariateTypes || {};
      instance.state.covariateTypes[name] = covariateTypeForVariable(variable);
      instance.state.covariateTargets = instance.state.covariateTargets || {};
      if (!instance.state.covariateTargets[name]) instance.state.covariateTargets[name] = [];
      if (isStructuralCanvas(instance)) {
        instance.state.covariateApplyTo = "finalEndogenous";
      }
      return true;
    }

    var existing = existingNodeByVariable(instance, name);
    if (existing) {
      existing.role = role;
      if (role === "dependent") {
        var dependentPosition = window.StatEduModelCanvas.layout.nextRolePosition(
          instance.state.nodes.filter(function(node) { return node.id !== existing.id; }),
          role,
          instance.state.style
        );
        existing.x = dependentPosition.x;
        existing.y = dependentPosition.y;
      }
      window.StatEduModelCanvas.layout.reflowRoleLayout(instance.state.nodes, instance.state.style);
      return true;
    }

    var position = window.StatEduModelCanvas.layout.nextRolePosition(instance.state.nodes, role, instance.state.style);
    instance.state.covariates = instance.state.covariates.filter(function(item) {
      return item !== name;
    });
    instance.state.nodes.push(window.StatEduModelCanvas.nodes.createNodeFromVariable(instance, variable, position.x, position.y, role));
    window.StatEduModelCanvas.layout.reflowRoleLayout(instance.state.nodes, instance.state.style);
    return true;
  }

  function addSelectedVariableAsRole(instance, role) {
    var names = selectedVariableNames(instance);
    if (names.length === 0) {
      window.alert(window.StatEduModelCanvas.state.label(instance, "select_variable_first", "\ubcc0\uc218\ub97c \uba3c\uc800 \uc120\ud0dd\ud558\uc138\uc694."));
      return;
    }
    window.StatEduModelCanvas.state.pushHistory(instance);
    var changed = false;
    names.forEach(function(name) {
      changed = applyRoleToVariable(instance, name, role) || changed;
    });
    if (!changed) {
      instance.state.history.pop();
      return;
    }
    if (role !== "covariate" && role !== "moderator") {
      window.StatEduModelCanvas.layout.reflowRoleLayout(instance.state.nodes, instance.state.style);
    }
    instance.state.selectedVariables = names;
    instance.state.selectedVariable = names[names.length - 1] || null;
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function bindVariableSelection(instance) {
    instance.root.querySelectorAll(".custom-model-variable-item").forEach(function(item) {
      item.addEventListener("click", function(event) {
        event.preventDefault();
        selectVariable(instance, item, event);
      });
    });
    instance.root.querySelectorAll(".custom-model-role-button, .structural-covariate-toolbar-button").forEach(function(button) {
      button.addEventListener("click", function(event) {
        event.preventDefault();
        addSelectedVariableAsRole(instance, button.getAttribute("data-role") || "independent");
      });
    });
  }

  function bindVariableDrag(instance) {
    instance.root.querySelectorAll(".custom-model-variable-item").forEach(function(item) {
      item.addEventListener("dragstart", function(event) {
        if (item.classList.contains("is-used")) {
          event.preventDefault();
          return;
        }
        event.dataTransfer.setData("text/plain", item.getAttribute("data-variable-name") || "");
        var draggedName = item.getAttribute("data-variable-name") || "";
        var selected = selectedVariableNames(instance);
        if (selected.indexOf(draggedName) < 0) selected = [draggedName];
        event.dataTransfer.setData("application/x-statedu-variables", JSON.stringify(selected));
        event.dataTransfer.effectAllowed = "copy";
      });
    });

    instance.paper.addEventListener("dragover", function(event) {
      event.preventDefault();
      event.dataTransfer.dropEffect = "copy";
    });

    instance.paper.addEventListener("drop", function(event) {
      event.preventDefault();
      var name = event.dataTransfer.getData("text/plain");
      var targetElement = event.target && event.target.closest ? event.target.closest(".custom-model-node-latent") : null;
      var isStructural = isStructuralCanvas(instance);
      if (isStructural && targetElement) {
        var latent = window.StatEduModelCanvas.nodes.nodeById(instance, targetElement.getAttribute("data-node-id"));
        if (!latent) return;
        var names;
        try {
          names = JSON.parse(event.dataTransfer.getData("application/x-statedu-variables") || "[]");
        } catch (error) {
          names = [name];
        }
        names = names.filter(function(itemName, index) {
          return itemName && names.indexOf(itemName) === index && !window.StatEduModelCanvas.nodes.variableUsed(instance, itemName);
        });
        if (!names.length) return;
        window.StatEduModelCanvas.state.pushHistory(instance);
        var errorBase = nextErrorSequence(instance) - 1;
        names.forEach(function(itemName, index) {
          var variable = variableByName(instance, itemName);
          if (!variable) return;
          var indicator = window.StatEduModelCanvas.nodes.createIndicatorNode(instance, variable, Number(latent.x || 0), Number(latent.y || 0));
          var errorNode = window.StatEduModelCanvas.nodes.createErrorNode(instance, indicator, Number(latent.x || 0), Number(latent.y || 0), errorBase + index + 1);
          instance.state.nodes.push(indicator, errorNode);
          window.StatEduModelCanvas.edges.createEdge(instance, latent.id, indicator.id);
          window.StatEduModelCanvas.edges.createEdge(instance, errorNode.id, indicator.id);
        });
        reflowMeasurementModel(instance);
        instance.state.selectedVariables = names;
        instance.state.selectedVariable = names[names.length - 1] || null;
        render(instance);
        window.StatEduModelCanvas.bridge.sendState(instance);
        return;
      }
      var variable = variableByName(instance, name);
      if (!variable || window.StatEduModelCanvas.nodes.variableUsed(instance, name)) return;
      window.StatEduModelCanvas.activeInstance = instance;
      var role = window.StatEduModelCanvas.dialogs.chooseRole("independent");
      if (!role) return;
      if (!canAssignRole(instance, role, name)) return;
      var point = canvasPoint(instance, event);
      window.StatEduModelCanvas.state.pushHistory(instance);
      instance.state.nodes.push(window.StatEduModelCanvas.nodes.createNodeFromVariable(instance, variable, point.x, point.y, role));
    if (role !== "moderator") {
      window.StatEduModelCanvas.layout.reflowRoleLayout(instance.state.nodes, instance.state.style);
    }
      instance.state.selectedVariable = name;
      instance.state.selectedVariables = [name];
      render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    });
  }

  function bindCanvasPointer(instance) {
    instance.paper.addEventListener("pointerdown", function(event) {
      window.StatEduModelCanvas.activeInstance = instance;
      flushActiveSetting(instance);
      var propertyPanel = event.target.closest ? event.target.closest(".custom-model-property-popover") : null;
      if (propertyPanel) return;
      var latentStatsElement = event.target.closest ? event.target.closest(".structural-latent-statistics") : null;
      if (latentStatsElement) return;
      instance.selectedLatentStatsNodeId = null;

      var labelElement = event.target.closest ? event.target.closest(".custom-model-edge-label") : null;
      labelElement = selectableLabelFromEvent(instance, event, labelElement);
      var nodeElement = event.target.closest ? event.target.closest(".custom-model-node") : null;
      var edgeControlElement = event.target.closest ? event.target.closest(".custom-model-edge-control") : null;
      var edgeElement = event.target.closest ? event.target.closest(".custom-model-edge, .custom-model-edge-hit") : null;
      var moderationElement = event.target.closest ? event.target.closest(".custom-model-moderation, .custom-model-moderation-hit") : null;
      var edgeId = edgeIdFromEvent(instance, event, 18);

      var viewingResult = window.StatEduModelCanvas.nodes.isViewingResult(instance);
      var resultSelectionMode = viewingResult && instance.state.mode === "properties";
      var marqueeSelectionMode = (!viewingResult && (event.shiftKey || instance.state.mode === "select")) || resultSelectionMode;
      if (marqueeSelectionMode && !labelElement && !nodeElement && !edgeId && !moderationElement && !propertyPanel) {
        event.preventDefault();
        var start = canvasPoint(instance, event);
        var additiveSelection = event.shiftKey;
        var initialSelection = additiveSelection ? (instance.state.selectedNodeIds || []).slice() : [];
        var marquee = document.createElement("div");
        marquee.className = "custom-model-selection-marquee";
        instance.paper.appendChild(marquee);
        function moveMarquee(moveEvent) {
          var point = canvasPoint(instance, moveEvent);
          var left = Math.min(start.x, point.x);
          var top = Math.min(start.y, point.y);
          marquee.style.left = left + "px";
          marquee.style.top = top + "px";
          marquee.style.width = Math.abs(point.x - start.x) + "px";
          marquee.style.height = Math.abs(point.y - start.y) + "px";
        }
        function finishMarquee(upEvent) {
          document.removeEventListener("pointermove", moveMarquee, true);
          document.removeEventListener("pointerup", finishMarquee, true);
          document.removeEventListener("pointercancel", cancelMarquee, true);
          var end = canvasPoint(instance, upEvent);
          var left = Math.min(start.x, end.x);
          var right = Math.max(start.x, end.x);
          var top = Math.min(start.y, end.y);
          var bottom = Math.max(start.y, end.y);
          var matched = instance.state.nodes.filter(function(node) {
            var nodeLeft = Number(node.x || 0);
            var nodeTop = Number(node.y || 0);
            var nodeRight = nodeLeft + Number(node.width || instance.state.style.boxWidth);
            var nodeBottom = nodeTop + Number(node.height || instance.state.style.boxHeight);
            return nodeRight >= left && nodeLeft <= right && nodeBottom >= top && nodeTop <= bottom;
          }).map(function(node) { return node.id; });
          var combined = initialSelection.concat(matched).filter(function(id, index, values) { return values.indexOf(id) === index; });
          instance.state.selectedNodeIds = combined;
          instance.state.selectedNodeId = combined.length ? combined[combined.length - 1] : null;
          instance.state.selectedEdgeId = null;
          if (marquee.parentNode) marquee.parentNode.removeChild(marquee);
          render(instance);
        }
        function cancelMarquee() {
          document.removeEventListener("pointermove", moveMarquee, true);
          document.removeEventListener("pointerup", finishMarquee, true);
          document.removeEventListener("pointercancel", cancelMarquee, true);
          if (marquee.parentNode) marquee.parentNode.removeChild(marquee);
        }
        moveMarquee(event);
        document.addEventListener("pointermove", moveMarquee, true);
        document.addEventListener("pointerup", finishMarquee, true);
        document.addEventListener("pointercancel", cancelMarquee, true);
        return;
      }

      if (labelElement) {
        event.preventDefault();
        if (window.StatEduModelCanvas.edges.selectLabelOwner) {
          window.StatEduModelCanvas.edges.selectLabelOwner(
            instance,
            labelElement.getAttribute("data-label-type"),
            labelElement.getAttribute("data-label-id")
          );
        }
        if (instance.state.mode === "properties") {
          window.StatEduModelCanvas.edges.startLabelDrag(
            instance,
            event,
            labelElement.getAttribute("data-label-type"),
            labelElement.getAttribute("data-label-id")
          );
        }
        return;
      }

      if (edgeControlElement) {
        var controlEdge = window.StatEduModelCanvas.edges.edgeById(instance, edgeControlElement.getAttribute("data-edge-id"));
        if (instance.state.mode === "properties" || (!viewingResult && instance.state.mode === "select" && window.StatEduModelCanvas.edges.curveEditable(instance, controlEdge))) {
          window.StatEduModelCanvas.edges.startControlDrag(instance, event, controlEdge.id);
          return;
        }
      }

      if (((!viewingResult && instance.state.mode === "select") || instance.state.mode === "properties") && moderationElement) {
        window.StatEduModelCanvas.edges.startModerationDrag(instance, event, moderationElement.getAttribute("data-moderation-id"));
        return;
      }

      if (instance.state.mode === "delete") {
        if (nodeElement) {
          window.StatEduModelCanvas.state.pushHistory(instance);
          window.StatEduModelCanvas.nodes.deleteNode(instance, nodeElement.getAttribute("data-node-id"));
          render(instance);
          window.StatEduModelCanvas.bridge.sendState(instance);
          return;
        }
        if (moderationElement) {
          window.StatEduModelCanvas.state.pushHistory(instance);
          window.StatEduModelCanvas.edges.deleteModeration(instance, moderationElement.getAttribute("data-moderation-id"));
          render(instance);
          window.StatEduModelCanvas.bridge.sendState(instance);
          return;
        }
        if (edgeId) {
          window.StatEduModelCanvas.state.pushHistory(instance);
          window.StatEduModelCanvas.edges.deleteEdge(instance, edgeId);
          render(instance);
          window.StatEduModelCanvas.bridge.sendState(instance);
        }
        return;
      }

      if ((instance.state.mode === "connect" || instance.state.mode === "covariance") && nodeElement) {
        handleConnectPointer(instance, event, nodeElement, instance.state.mode);
        return;
      }

      if (instance.state.mode === "properties") {
        if (nodeElement) {
          if (window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
            var resultNodeId = nodeElement.getAttribute("data-node-id");
            var resultSelectedIds = instance.state.selectedNodeIds || [];
            if (event.shiftKey || event.ctrlKey || event.metaKey) {
              event.preventDefault();
              resultSelectedIds = resultSelectedIds.slice();
              var resultSelectedIndex = resultSelectedIds.indexOf(resultNodeId);
              if (resultSelectedIndex >= 0) resultSelectedIds.splice(resultSelectedIndex, 1);
              else resultSelectedIds.push(resultNodeId);
              instance.state.selectedNodeIds = resultSelectedIds;
              instance.state.selectedNodeId = resultSelectedIds.length ? resultSelectedIds[resultSelectedIds.length - 1] : null;
              instance.state.selectedEdgeId = null;
              render(instance);
            } else {
              var resultAllSelected = window.StatEduModelCanvas.nodes.structuralSelectionAllowsY(instance, resultSelectedIds);
              if (!resultAllSelected && resultSelectedIds.indexOf(resultNodeId) < 0) {
                instance.state.selectedNodeIds = [resultNodeId];
              }
              instance.state.selectedNodeId = resultNodeId;
              instance.state.selectedEdgeId = null;
              window.StatEduModelCanvas.nodes.startDrag(instance, event, nodeElement);
              render(instance);
            }
          } else {
            event.preventDefault();
            window.StatEduModelCanvas.nodes.showProperties(instance, nodeElement.getAttribute("data-node-id"));
          }
          return;
        }
        if (edgeId) {
          event.preventDefault();
          if (window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
            instance.state.selectedNodeId = null;
            instance.state.selectedNodeIds = [];
            instance.state.selectedEdgeId = edgeId;
            instance.state.selectedModerationId = null;
            window.StatEduModelCanvas.nodes.hideProperties(instance);
            render(instance);
          } else {
            window.StatEduModelCanvas.nodes.showEdgeProperties(instance, edgeId);
          }
          return;
        }
        if (!moderationElement) {
          instance.state.selectedNodeId = null;
          instance.state.selectedNodeIds = [];
          instance.state.selectedEdgeId = null;
          window.StatEduModelCanvas.nodes.hideProperties(instance);
          render(instance);
        }
        return;
      }

      if (viewingResult) {
        instance.state.selectedNodeId = null;
        instance.state.selectedNodeIds = [];
        instance.state.selectedEdgeId = null;
        instance.state.selectedModerationId = null;
        render(instance);
        return;
      }

      if (nodeElement) {
        var nodeId = nodeElement.getAttribute("data-node-id");
        var selectedIds = instance.state.selectedNodeIds || [];
        if (event.shiftKey || event.ctrlKey || event.metaKey) {
          event.preventDefault();
          selectedIds = selectedIds.slice();
          var selectedIndex = selectedIds.indexOf(nodeId);
          if (selectedIndex >= 0) {
            selectedIds.splice(selectedIndex, 1);
          } else {
            selectedIds.push(nodeId);
          }
          instance.state.selectedNodeIds = selectedIds;
          instance.state.selectedNodeId = selectedIds.length ? selectedIds[selectedIds.length - 1] : null;
          instance.state.selectedEdgeId = null;
          render(instance);
          return;
        } else if (selectedIds.indexOf(nodeId) < 0) {
          instance.state.selectedNodeIds = [nodeId];
          instance.state.selectedNodeId = nodeId;
        } else {
          instance.state.selectedNodeId = nodeId;
        }
        instance.state.selectedEdgeId = null;
        window.StatEduModelCanvas.nodes.startDrag(instance, event, nodeElement);
        render(instance);
      } else if (edgeId) {
        var selectedEdge = window.StatEduModelCanvas.edges.edgeById(instance, edgeId);
        if (window.StatEduModelCanvas.edges.curveEditable(instance, selectedEdge)) {
          event.preventDefault();
          instance.state.selectedNodeId = null;
          instance.state.selectedNodeIds = [];
          instance.state.selectedEdgeId = selectedEdge.id;
          instance.state.selectedModerationId = null;
          render(instance);
        }
      } else if (!edgeId && !moderationElement) {
        instance.state.selectedNodeId = null;
        instance.state.selectedNodeIds = [];
        instance.state.selectedEdgeId = null;
        render(instance);
      }
    }, true);

    instance.paper.addEventListener("dblclick", function(event) {
      var propertyPanel = event.target.closest ? event.target.closest(".custom-model-property-popover") : null;
      if (propertyPanel) return;
      if (window.StatEduModelCanvas.nodes.isViewingResult(instance)) return;

      var labelElement = event.target.closest ? event.target.closest(".custom-model-edge-label") : null;
      var nodeElement = event.target.closest ? event.target.closest(".custom-model-node") : null;
      var moderationElement = event.target.closest ? event.target.closest(".custom-model-moderation, .custom-model-moderation-hit") : null;
      var edgeId = edgeIdFromEvent(instance, event, 18);
      if (labelElement) {
        if (instance.state.mode !== "properties") window.StatEduModelCanvas.toolbar.setMode(instance, "properties");
        window.StatEduModelCanvas.edges.showLabelProperties(
          instance,
          labelElement.getAttribute("data-label-type"),
          labelElement.getAttribute("data-label-id")
        );
      } else if (nodeElement) {
        if (instance.state.mode !== "properties") window.StatEduModelCanvas.toolbar.setMode(instance, "properties");
        window.StatEduModelCanvas.nodes.editLabel(instance, nodeElement.getAttribute("data-node-id"));
      } else if (moderationElement) {
        if (instance.state.mode !== "properties") window.StatEduModelCanvas.toolbar.setMode(instance, "properties");
        window.StatEduModelCanvas.edges.selectModeration(instance, moderationElement.getAttribute("data-moderation-id"));
      } else if (edgeId) {
        if (instance.state.mode !== "properties") window.StatEduModelCanvas.toolbar.setMode(instance, "properties");
        window.StatEduModelCanvas.nodes.showEdgeProperties(instance, edgeId);
      }
    });
  }

  function selectedNodes(instance) {
    var ids = instance.state.selectedNodeIds || [];
    return instance.state.nodes.filter(function(node) { return ids.indexOf(node.id) >= 0; });
  }

  function alignSelected(instance, action) {
    var nodes = selectedNodes(instance);
    if (nodes.length < 2) return false;
    window.StatEduModelCanvas.state.pushHistory(instance);

    // Indicators and their errors form one measurement layout.  When those
    // items are selected, use the parent latent as the alignment reference.
    var measurementSelection = nodes.every(function(node) {
      return node.role === "indicator" || node.role === "error";
    });
    if (measurementSelection && ["alignCenter", "alignMiddle", "distributeH", "distributeV"].indexOf(action) >= 0) {
      var selectedIndicatorIds = [];
      nodes.forEach(function(node) {
        if (node.role === "indicator") selectedIndicatorIds.push(node.id);
        if (node.role !== "error") return;
        var errorEdge = instance.state.edges.find(function(edge) {
          return edge.from === node.id && window.StatEduModelCanvas.nodes.nodeById(instance, edge.to) &&
            window.StatEduModelCanvas.nodes.nodeById(instance, edge.to).role === "indicator";
        });
        if (errorEdge) selectedIndicatorIds.push(errorEdge.to);
      });
      selectedIndicatorIds = selectedIndicatorIds.filter(function(id, index, ids) {
        return ids.indexOf(id) === index;
      });
      var parentLatentIds = selectedIndicatorIds.map(function(indicatorId) {
        var measurementEdge = instance.state.edges.find(function(edge) {
          var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
          var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
          return (from && from.role === "latent" && edge.to === indicatorId) ||
            (to && to.role === "latent" && edge.from === indicatorId);
        });
        if (!measurementEdge) return null;
        var from = window.StatEduModelCanvas.nodes.nodeById(instance, measurementEdge.from);
        return from && from.role === "latent" ? measurementEdge.from : measurementEdge.to;
      }).filter(function(id, index, ids) {
        return id && ids.indexOf(id) === index;
      });
      if (parentLatentIds.length) {
        reflowMeasurementModel(instance);
        render(instance);
        window.StatEduModelCanvas.bridge.sendState(instance);
        return true;
      }
    }

    if (action === "alignLeft") { var left = Math.min.apply(Math, nodes.map(function(n) { return Number(n.x || 0); })); nodes.forEach(function(n) { n.x = left; }); }
    if (action === "alignTop") { var top = Math.min.apply(Math, nodes.map(function(n) { return Number(n.y || 0); })); nodes.forEach(function(n) { n.y = top; }); }
    if (action === "alignCenter") { var center = nodes.reduce(function(sum,n) { return sum + Number(n.x || 0) + Number(n.width || 0)/2; },0)/nodes.length; nodes.forEach(function(n) { n.x = center - Number(n.width || 0)/2; }); }
    if (action === "alignMiddle") { var middle = nodes.reduce(function(sum,n) { return sum + Number(n.y || 0) + Number(n.height || 0)/2; },0)/nodes.length; nodes.forEach(function(n) { n.y = middle - Number(n.height || 0)/2; }); }
    if (action === "distributeH" && nodes.length >= 3) {
      nodes.sort(function(a,b){ return a.x-b.x; }); var minX=nodes[0].x, maxX=nodes[nodes.length-1].x; nodes.forEach(function(n,i){ n.x=minX+(maxX-minX)*i/(nodes.length-1); });
    }
    if (action === "distributeV" && nodes.length >= 3) {
      nodes.sort(function(a,b){ return a.y-b.y; }); var minY=nodes[0].y, maxY=nodes[nodes.length-1].y; nodes.forEach(function(n,i){ n.y=minY+(maxY-minY)*i/(nodes.length-1); });
    }
    reflowMeasurementModel(instance); render(instance); window.StatEduModelCanvas.bridge.sendState(instance); return true;
  }

  function autoLayoutModel(instance) {
    if (!instance || !instance.state || !instance.state.nodes.length) return false;
    window.StatEduModelCanvas.state.pushHistory(instance);
    window.StatEduModelCanvas.layout.reflowRoleLayout(instance.state.nodes, instance.state.style);
    reflowMeasurementModel(instance);
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    return true;
  }

  function selectedStructuralEdge(instance) {
    var edge = instance.state.selectedEdgeId && window.StatEduModelCanvas.edges.edgeById(instance, instance.state.selectedEdgeId);
    if (!edge || edge.kind === "covariance") return null;
    var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
    var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
    if (!from || !to || from.role !== "latent" || to.role !== "latent") return null;
    return {edge: edge, from: from, to: to};
  }

  function addContinuousModeratorBlock(instance) {
    if (["cbsem", "sem", "plssem"].indexOf(instance.analysisType) < 0) return false;
    var target = selectedStructuralEdge(instance);
    var names = selectedVariableNames(instance).filter(function(name) {
      var variable = variableByName(instance, name);
      var measurement = String(variable && variable.measurement || "").toLowerCase();
      var continuous = !measurement || ["scale", "continuous", "numeric"].indexOf(measurement) >= 0;
      return variable && continuous && !window.StatEduModelCanvas.nodes.variableUsed(instance, name);
    });
    if (!target || !names.length) return false;
    window.StatEduModelCanvas.state.pushHistory(instance);
    var style = instance.state.style || {};
    var fromCenter = window.StatEduModelCanvas.layout.nodeCenter(target.from, style);
    var toCenter = window.StatEduModelCanvas.layout.nodeCenter(target.to, style);
    var baseX = (fromCenter.x + toCenter.x) / 2 - 45;
    var baseY = Math.min(fromCenter.y, toCenter.y) - 145;
    var selectedIds = [];
    var errorBase = nextErrorSequence(instance) - 1;
    names.forEach(function(name, index) {
      var variable = variableByName(instance, name);
      if (!variable) return;
      var latent = window.StatEduModelCanvas.nodes.createLatentNode(instance, baseX + index * 150, baseY);
      latent.name = String(variable.name || "moderator") + "_latent";
      latent.dataLabel = variable.dataLabel || variable.name;
      latent.canvasLabel = variable.dataLabel || variable.name;
      latent.measurementPlacement = "top";
      latent.measurementMode = "reflective";
      latent.constructType = "commonFactor";
      var indicator = window.StatEduModelCanvas.nodes.createIndicatorNode(instance, variable, latent.x, latent.y - 80);
      var errorNode = window.StatEduModelCanvas.nodes.createErrorNode(instance, indicator, indicator.x, indicator.y - 46, errorBase + index + 1);
      instance.state.nodes.push(latent, indicator, errorNode);
      window.StatEduModelCanvas.edges.createEdge(instance, latent.id, indicator.id);
      window.StatEduModelCanvas.edges.createEdge(instance, errorNode.id, indicator.id);
      window.StatEduModelCanvas.edges.createModeration(instance, latent.id, target.edge.id, 50);
      selectedIds.push(latent.id, indicator.id);
    });
    if (!selectedIds.length) {
      instance.state.history.pop();
      return false;
    }
    reflowMeasurementModel(instance);
    instance.state.selectedNodeIds = selectedIds;
    instance.state.selectedNodeId = selectedIds[0];
    instance.state.selectedEdgeId = null;
    instance.state.selectedModerationId = null;
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    return true;
  }

  function copySelection(instance) {
    var nodes = selectedNodes(instance);
    if (!nodes.length) return false;
    var ids = nodes.map(function(node){ return node.id; });
    instance.nodeClipboard = {nodes: JSON.parse(JSON.stringify(nodes)), edges: JSON.parse(JSON.stringify(instance.state.edges.filter(function(edge){ return ids.indexOf(edge.from)>=0 && ids.indexOf(edge.to)>=0; })))};
    return true;
  }

  function pasteSelection(instance) {
    if (!instance.nodeClipboard || !instance.nodeClipboard.nodes.length) return false;
    window.StatEduModelCanvas.state.pushHistory(instance);
    var map = {}, stamp = Date.now();
    var copies = instance.nodeClipboard.nodes.map(function(node,index){ var copy=JSON.parse(JSON.stringify(node)); map[node.id]="copy_"+stamp+"_"+index; copy.id=map[node.id]; copy.x=Number(copy.x||0)+28; copy.y=Number(copy.y||0)+28; return copy; });
    var edges = instance.nodeClipboard.edges.map(function(edge,index){ var copy=JSON.parse(JSON.stringify(edge)); copy.id="copy_edge_"+stamp+"_"+index; copy.from=map[edge.from]; copy.to=map[edge.to]; return copy; });
    instance.state.nodes.push.apply(instance.state.nodes,copies); instance.state.edges.push.apply(instance.state.edges,edges);
    instance.state.selectedNodeIds=copies.map(function(node){return node.id;}); instance.state.selectedNodeId=instance.state.selectedNodeIds[instance.state.selectedNodeIds.length-1];
    render(instance); window.StatEduModelCanvas.bridge.sendState(instance); return true;
  }

  function bindCanvasKeyboard(instance) {
    document.addEventListener("keydown", function(event) {
      var active = document.activeElement;
      var tagName = active && active.tagName ? active.tagName.toLowerCase() : "";
      if (tagName === "input" || tagName === "textarea" || tagName === "select" || (active && active.isContentEditable)) return;
      if (window.StatEduModelCanvas.activeInstance && window.StatEduModelCanvas.activeInstance !== instance) return;
      if (!instance.root.contains(active) && window.StatEduModelCanvas.activeInstance !== instance) return;
      var key = String(event.key || "").toLowerCase();
      var ctrl = event.ctrlKey || event.metaKey;
      if (ctrl && key === "c") { event.preventDefault(); copySelection(instance); return; }
      if (ctrl && key === "v") { event.preventDefault(); pasteSelection(instance); return; }
      if (ctrl && key === "d") { event.preventDefault(); if (copySelection(instance)) pasteSelection(instance); return; }
      if (["arrowleft","arrowright","arrowup","arrowdown"].indexOf(key) >= 0) {
        var moving=selectedNodes(instance); if(!moving.length)return; event.preventDefault(); window.StatEduModelCanvas.state.pushHistory(instance); var step=event.shiftKey?10:1;
        var selectedIdsForMove = instance.state.selectedNodeIds || [];
        var structuralSelectionAxis = window.StatEduModelCanvas.nodes.structuralSelectionMovementAxis(instance, selectedIdsForMove);
        moving.forEach(function(node){
          var structuralMovePolicy=window.StatEduModelCanvas.nodes.usesStructuralMovePolicy(instance);
          var errorAxis=!structuralMovePolicy && node.role==="error" ? window.StatEduModelCanvas.nodes.errorMovementAxis(instance,node) : null;
          var disturbanceMove=!structuralMovePolicy && node.role==="disturbance" ? window.StatEduModelCanvas.nodes.disturbanceMovement(instance,node) : null;
          var keyDx=key==="arrowleft"?-step:key==="arrowright"?step:0;
          var keyDy=key==="arrowup"?-step:key==="arrowdown"?step:0;
          if(structuralMovePolicy && (structuralSelectionAxis==="y" || structuralSelectionAxis==="none")) keyDx=0;
          if(structuralMovePolicy && (structuralSelectionAxis==="x" || structuralSelectionAxis==="none")) keyDy=0;
          if(disturbanceMove){
            var projected=keyDx*disturbanceMove.vector.x+keyDy*disturbanceMove.vector.y;
            node.x+=projected*disturbanceMove.vector.x; node.y+=projected*disturbanceMove.vector.y;
          } else {
            if(keyDx && errorAxis!=="y")node.x+=keyDx; if(keyDy && errorAxis!=="x")node.y+=keyDy;
          }
          if(node.role==="error") {
            window.StatEduModelCanvas.nodes.captureErrorOffset(instance,node);
          }
          if(node.role==="disturbance") window.StatEduModelCanvas.nodes.captureDisturbanceOffset(instance,node);
        });
        if(!window.StatEduModelCanvas.nodes.usesStructuralMovePolicy(instance)) reflowMeasurementModel(instance);
        render(instance); window.StatEduModelCanvas.bridge.sendState(instance); return;
      }
      if (!ctrl || key !== "a") return;
      event.preventDefault();
      var allIds = instance.state.nodes.map(function(node) { return node.id; });
      var selectedIds = instance.state.selectedNodeIds || [];
      if (allIds.length > 0 && selectedIds.length === allIds.length) {
        instance.state.selectedNodeIds = [];
        instance.state.selectedNodeId = null;
      } else {
        instance.state.selectedNodeIds = allIds;
        instance.state.selectedNodeId = allIds.length ? allIds[allIds.length - 1] : null;
      }
      instance.state.selectedEdgeId = null;
      render(instance);
    }, true);
  }

  function handleConnectPointer(instance, event, nodeElement, connectMode) {
    var fromId = nodeElement.getAttribute("data-node-id");
    var fromNode = window.StatEduModelCanvas.nodes.nodeById(instance, fromId);
    if (!fromNode) return;
    event.preventDefault();

    var fromCenter = window.StatEduModelCanvas.layout.nodeCenter(fromNode, instance.state.style);
    var point = canvasPoint(instance, event);
    instance.state.dragPreview = {x1: fromCenter.x, y1: fromCenter.y, x2: point.x, y2: point.y};
    window.StatEduModelCanvas.edges.render(instance);

    function move(moveEvent) {
      var next = canvasPoint(instance, moveEvent);
      instance.state.dragPreview = {x1: fromCenter.x, y1: fromCenter.y, x2: next.x, y2: next.y};
      window.StatEduModelCanvas.edges.render(instance);
    }

    function up(upEvent) {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      document.removeEventListener("pointercancel", cancel, true);
      instance.state.dragPreview = null;

      var targetNodeId = nodeIdAtEvent(instance, upEvent);
      var changed = false;
      window.StatEduModelCanvas.state.pushHistory(instance);
      if (targetNodeId) {
        changed = connectMode === "covariance" ?
          window.StatEduModelCanvas.edges.createCovariance(instance, fromId, targetNodeId) :
          window.StatEduModelCanvas.edges.createEdge(instance, fromId, targetNodeId);
      } else {
        var targetPoint = canvasPoint(instance, upEvent);
        var nearest = window.StatEduModelCanvas.edges.nearestEdgeAt(instance, targetPoint, 24);
        if (nearest) {
          changed = window.StatEduModelCanvas.edges.createModeration(instance, fromId, nearest.edge.id, nearest.percent);
        }
      }
      if (!changed) {
        instance.state.history.pop();
      }
      if (changed) reflowMeasurementModel(instance);
      render(instance);
      if (changed) window.StatEduModelCanvas.bridge.sendState(instance);
    }

    function cancel() {
      document.removeEventListener("pointermove", move, true);
      document.removeEventListener("pointerup", up, true);
      document.removeEventListener("pointercancel", cancel, true);
      instance.state.dragPreview = null;
      window.StatEduModelCanvas.edges.render(instance);
    }

    document.addEventListener("pointermove", move, true);
    document.addEventListener("pointerup", up, true);
    document.addEventListener("pointercancel", cancel, true);
  }

  function zoom(instance, factor) {
    if (isStructuralCanvas(instance)) {
      instance.state.canvas.modelZoom = Math.max(0.5, Math.min(2, Number(instance.state.canvas.modelZoom || 1) * factor));
    } else {
      instance.state.canvas.zoom = Math.max(0.5, Math.min(2, instance.state.canvas.zoom * factor));
    }
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function fit(instance) {
    if (isStructuralCanvas(instance)) {
      instance.state.canvas.modelZoom = 1;
    }
    fitPaperToViewport(instance);
    render(instance);
    var scroll = instance.root.querySelector(".custom-model-canvas-scroll");
    if (scroll) scroll.scrollTo({left: 0, top: 0});
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function fitPaperToViewport(instance) {
    if (!instance || !instance.root || instance.root.offsetParent === null) return false;
    var scroll = instance.root.querySelector(".custom-model-canvas-scroll");
    if (!scroll) {
      setPaperViewZoom(instance, 1);
      instance.paperAutoFitApplied = true;
      return false;
    }
    var paperWidth = Number(instance.state.canvas.widthPx || 0);
    var paperHeight = Number(instance.state.canvas.heightPx || 0);
    if (!(paperWidth > 0) || !(paperHeight > 0)) {
      setPaperViewZoom(instance, 1);
      instance.paperAutoFitApplied = true;
      return false;
    }
    var computed = window.getComputedStyle(scroll);
    var paddingX = Number(parseFloat(computed.paddingLeft) || 0) + Number(parseFloat(computed.paddingRight) || 0);
    var paddingY = Number(parseFloat(computed.paddingTop) || 0) + Number(parseFloat(computed.paddingBottom) || 0);
    var availableWidth = Math.max(120, scroll.clientWidth - paddingX - 4);
    var availableHeight = Math.max(120, scroll.clientHeight - paddingY - 4);
    var nextZoom = Math.min(1, availableWidth / paperWidth, availableHeight / paperHeight);
    nextZoom = boundedZoom(nextZoom, 1);
    var currentZoom = paperViewZoom(instance);
    setPaperViewZoom(instance, nextZoom);
    instance.paperAutoFitApplied = true;
    return Math.abs(currentZoom - nextZoom) > 0.001;
  }

  function resizeCanvasToViewport(instance) {
    if (!instance || !instance.root || !isStructuralCanvas(instance)) return;
    if (instance.root.offsetParent === null) return;
    var scroll = instance.root.querySelector(".custom-model-canvas-scroll");
    var diagram = instance.root.querySelector(".custom-model-diagram-panel");
    if (!scroll || !diagram) return;
    var toolbar = diagram.querySelector(".custom-model-toolbar");
    var statusbar = diagram.querySelector(".custom-model-statusbar");
    var rootTop = instance.root.getBoundingClientRect().top;
    var chromeHeight = Number(toolbar ? toolbar.offsetHeight : 0) + Number(statusbar ? statusbar.offsetHeight : 0);
    var available = window.innerHeight - rootTop - chromeHeight - 20;
    var scrollHeight = Math.max(280, Math.min(730, available));
    scroll.style.height = Math.round(scrollHeight) + "px";
    instance.root.style.setProperty("--custom-model-visible-height", Math.round(scrollHeight + chromeHeight) + "px");
  }

  function showResult(instance) {
    if (!instance || !instance.resultSnapshot) {
      window.alert(window.StatEduModelCanvas.state.label(instance, "result_unavailable", "\uacb0\uacfc \uadf8\ub9bc\uc740 \ubd84\uc11d \uc2e4\ud589 \ud6c4\uc5d0 \ud655\uc778\ud560 \uc218 \uc788\uc2b5\ub2c8\ub2e4."));
      return;
    }
    window.StatEduModelCanvas.state.restore(instance.state, instance.resultSnapshot);
    instance.state.mode = "select";
    instance.state.selectedNodeId = null;
    instance.state.selectedNodeIds = [];
    instance.state.selectedEdgeId = null;
    instance.state.selectedModerationId = null;
    instance.selectedLatentStatsNodeId = null;
    instance.viewingResult = true;
    instance.root.classList.add("is-viewing-result");
    render(instance);
  }

  function showSource(instance) {
    if (!instance || !instance.sourceSnapshot || !window.StatEduModelCanvas.nodes.isViewingResult(instance)) return;
    window.StatEduModelCanvas.state.restore(instance.state, instance.sourceSnapshot);
    instance.state.mode = "select";
    instance.viewingResult = false;
    instance.root.classList.remove("is-viewing-result");
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
  }

  function init(root) {
    if (!root || root.__stateduModelCanvas) return root && root.__stateduModelCanvas;
    var instance = {
      root: root,
      state: window.StatEduModelCanvas.state.create(),
      analysisType: root.getAttribute("data-analysis-type") || "",
      paper: root.querySelector(".custom-model-paper"),
      paperFrame: root.querySelector(".custom-model-paper-frame"),
      edgeLayer: root.querySelector(".custom-model-edge-layer"),
      nodeLayer: root.querySelector(".custom-model-node-layer")
    };
    ensurePaperFrame(instance);
    instance.state.variables = parseVariables(root);
    applyAnalysisCanvasDefaults(instance);
    var configuredWidth = Number(root.getAttribute("data-canvas-width") || 0);
    var configuredHeight = Number(root.getAttribute("data-canvas-height") || 0);
    if (configuredWidth > 0 && configuredHeight > 0) {
      instance.state.canvas.widthPx = configuredWidth;
      instance.state.canvas.heightPx = configuredHeight;
      instance.state.canvas.paper = root.getAttribute("data-canvas-paper") || "Custom";
      instance.state.canvas.orientation = root.getAttribute("data-canvas-orientation") ||
        (configuredHeight > configuredWidth ? "portrait" : "landscape");
    }
    var initialSnapshot = parseInitialSnapshot(root);
    if (
      initialSnapshot &&
      typeof initialSnapshot === "object" &&
      Array.isArray(initialSnapshot.nodes) &&
      Array.isArray(initialSnapshot.edges)
    ) {
      window.StatEduModelCanvas.state.restore(instance.state, initialSnapshot);
    }
    instance.language = root.getAttribute("data-language") || "ko";
    instance.i18n = parseI18n(root);
    root.__stateduModelCanvas = instance;
    window.StatEduModelCanvas.toolbar.bind(instance);
    bindVariableSelection(instance);
    bindVariableDrag(instance);
    bindCanvasPointer(instance);
    bindCanvasKeyboard(instance);
    resizeCanvasToViewport(instance);
    fitPaperToViewport(instance);
    render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    if (root.getAttribute("data-initial-run") === "true") {
      var attempts = 0;
      var runInitial = function() {
        attempts += 1;
        if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
          window.StatEduModelCanvas.bridge.runConfirm(instance);
        } else if (attempts < 20) {
          window.setTimeout(runInitial, 250);
        }
      };
      window.setTimeout(runInitial, 500);
    }
    return instance;
  }

  function initAll() {
    document.querySelectorAll(".custom-model-canvas-root").forEach(function(root) {
      var instance = init(root);
      resizeCanvasToViewport(instance);
      if (instance && !instance.paperAutoFitApplied && fitPaperToViewport(instance)) render(instance);
    });
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.canvas = {
    init: init,
    initAll: initAll,
    render: render,
    canvasPoint: canvasPoint,
    nodeIdAtEvent: nodeIdAtEvent,
    showResult: showResult,
    showSource: showSource,
    zoom: zoom,
    fit: fit,
    fitPaperToViewport: fitPaperToViewport,
    resizeToViewport: resizeCanvasToViewport,
    reflowMeasurements: reflowMeasurementModel,
    setMeasurementPlacement: setMeasurementPlacement,
    setMeasurementMode: setMeasurementMode,
    addContinuousModeratorBlock: addContinuousModeratorBlock,
    selectedLatents: selectedLatentNodes,
    detachIndicators: detachSelectedIndicators,
    moveIndicator: moveSelectedIndicator,
    alignSelected: alignSelected,
    autoLayout: autoLayoutModel
  };

  var canvasResizeFrame = 0;
  window.addEventListener("resize", function() {
    if (canvasResizeFrame) window.cancelAnimationFrame(canvasResizeFrame);
    canvasResizeFrame = window.requestAnimationFrame(function() {
      canvasResizeFrame = 0;
      document.querySelectorAll(".custom-model-canvas-root").forEach(function(root) {
        var instance = root.__stateduModelCanvas;
        resizeCanvasToViewport(instance);
        if (instance && fitPaperToViewport(instance)) render(instance);
      });
    });
  });

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initAll);
  } else {
    initAll();
  }
  document.addEventListener("shiny:value", function(event) {
    if (
      event.target &&
      (
        (event.target.matches && event.target.matches(".custom-model-canvas-root")) ||
        (event.target.querySelector && event.target.querySelector(".custom-model-canvas-root"))
      )
    ) {
      window.setTimeout(initAll, 0);
    }
  });
  if (window.MutationObserver && document.documentElement) {
    new MutationObserver(function(mutations) {
      var shouldInit = mutations.some(function(mutation) {
        return Array.prototype.some.call(mutation.addedNodes || [], function(node) {
          return node.nodeType === 1 && (
            (node.matches && node.matches(".custom-model-canvas-root")) ||
            (node.querySelector && node.querySelector(".custom-model-canvas-root"))
          );
        });
      });
      if (shouldInit) window.setTimeout(initAll, 0);
    }).observe(document.documentElement, {childList: true, subtree: true});
  }
})();
