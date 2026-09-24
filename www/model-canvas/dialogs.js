(function() {
  "use strict";

  var ROLE_ORDER = ["independent", "mediator", "moderator", "dependent"];
  var PAPER_SIZES = {
    B5: {widthMm: 257, heightMm: 182},
    A4: {widthMm: 297, heightMm: 210},
    Large: {widthMm: 529.1667, heightMm: 264.5833, widthPx: 2000, heightPx: 1000}
  };
  var COLOR_PRESETS = [
    {value: "#000000", label: "\uac80\uc815", key: "color_black"},
    {value: "#1f2937", label: "\ud68c\uac80\uc815", key: "color_dark_gray"},
    {value: "#2563eb", label: "\ud30c\ub791", key: "color_blue"},
    {value: "#16a34a", label: "\ucd08\ub85d", key: "color_green"},
    {value: "#dc2626", label: "\ube68\uac15", key: "color_red"},
    {value: "#7c3aed", label: "\ubcf4\ub77c", key: "color_purple"},
    {value: "#f59e0b", label: "\uc8fc\ud669", key: "color_orange"},
    {value: "custom", label: "Custom", key: "color_custom"}
  ];

  function pxFromMm(mm) {
    return Math.round(Number(mm || 0) * 96 / 25.4);
  }

  function removeModal() {
    var modal = document.querySelector(".custom-model-modal-backdrop");
    if (modal) modal.remove();
  }

  function modalShell(title) {
    removeModal();
    var backdrop = document.createElement("div");
    backdrop.className = "custom-model-modal-backdrop";
    var modal = document.createElement("div");
    modal.className = "custom-model-modal";
    var header = document.createElement("div");
    header.className = "custom-model-modal-title";
    header.textContent = title;
    var body = document.createElement("div");
    body.className = "custom-model-modal-body";
    var footer = document.createElement("div");
    footer.className = "custom-model-modal-footer";
    var cancel = document.createElement("button");
    cancel.type = "button";
    cancel.className = "btn btn-default custom-model-modal-cancel";
    cancel.textContent = window.StatEduModelCanvas.state.label(window.StatEduModelCanvas.activeInstance, "cancel", "\ucde8\uc18c");
    cancel.addEventListener("click", removeModal);
    var apply = document.createElement("button");
    apply.type = "button";
    apply.className = "btn btn-primary custom-model-modal-apply";
    apply.textContent = window.StatEduModelCanvas.state.label(window.StatEduModelCanvas.activeInstance, "apply", "\uc801\uc6a9");
    footer.appendChild(cancel);
    footer.appendChild(apply);
    modal.appendChild(header);
    modal.appendChild(body);
    modal.appendChild(footer);
    backdrop.appendChild(modal);
    document.body.appendChild(backdrop);
    return {backdrop: backdrop, modal: modal, body: body, apply: apply};
  }

  function chooseRole(defaultRole) {
    var instance = window.StatEduModelCanvas.activeInstance || null;
    var raw = window.prompt(
      window.StatEduModelCanvas.state.label(instance, "role_prompt", "Role: independent, mediator, moderator, dependent"),
      defaultRole || "independent"
    );
    if (raw === null) return null;
    raw = String(raw || "").trim().toLowerCase();
    if (ROLE_ORDER.indexOf(raw) < 0) {
      raw = "independent";
    }
    return raw;
  }

  function measuredVariable(instance, applyVariable) {
    window.StatEduModelCanvas.activeInstance = instance;
    var ko = instance.language === "ko";
    var shell = modalShell(ko ? "측정변수 설정" : "Measured variable settings");
    var variables = document.createElement("select");
    variables.className = "form-control mm-measured-variable";
    instance.state.variables.filter(function(variable) {
      return !window.StatEduModelCanvas.nodes.variableUsed(instance, variable.name) && instance.state.covariates.indexOf(variable.name) < 0;
    }).forEach(function(variable) {
      var option = document.createElement("option");
      option.value = variable.name;
      option.textContent = variable.dataLabel || variable.name;
      variables.appendChild(option);
    });
    if (Array.from(variables.options).some(function(option) { return option.value === instance.state.selectedVariable; })) variables.value = instance.state.selectedVariable;
    var label = document.createElement("label");
    label.textContent = ko ? "데이터 변수" : "Data variable";
    label.appendChild(variables);
    shell.body.appendChild(label);
    var message = document.createElement("p");
    message.textContent = variables.options.length ? (ko ? "역할은 화살표 연결에 따라 자동으로 결정됩니다." : "Arrows automatically determine the variable's role.") : (ko ? "배치할 미사용 변수가 없습니다." : "No unused variables are available.");
    shell.body.appendChild(message);
    shell.apply.disabled = !variables.options.length;
    shell.apply.addEventListener("click", function() {
      if (applyVariable(variables.value)) removeModal();
    });
    variables.focus();
  }

  function covariates(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    var selected = {};
    instance.state.covariates.forEach(function(name) {
      selected[name] = true;
    });
    var shell = modalShell(window.StatEduModelCanvas.state.label(instance, "covariate_settings", "\uacf5\ubcc0\ub7c9 \uc124\uc815"));
    var list = document.createElement("div");
    list.className = "custom-model-covariate-list";
    instance.state.variables.forEach(function(variable) {
      var label = document.createElement("label");
      label.className = "custom-model-check-row";
      var input = document.createElement("input");
      input.type = "checkbox";
      input.value = variable.name;
      input.checked = !!selected[variable.name];
      input.disabled = window.StatEduModelCanvas.nodes.variableUsed(instance, variable.name);
      var text = document.createElement("span");
      text.textContent = (variable.dataLabel || variable.name) + (variable.dataLabel && variable.dataLabel !== variable.name ? " (" + variable.name + ")" : "");
      label.appendChild(input);
      label.appendChild(text);
      list.appendChild(label);
    });
    shell.body.appendChild(list);
    shell.apply.addEventListener("click", function() {
      var names = Array.from(shell.body.querySelectorAll("input[type='checkbox']:checked")).map(function(input) {
        return input.value;
      });
      window.StatEduModelCanvas.state.pushHistory(instance);
      instance.state.covariates = names.filter(function(name) {
        return !window.StatEduModelCanvas.nodes.variableUsed(instance, name);
      });
      var types = {}, targets = {};
      instance.state.covariates.forEach(function(name) {
        var variable = instance.state.variables.find(function(item) { return item.name === name; });
        types[name] = window.StatEduModelCanvas.canvas.covariateTypeForVariable(variable);
        targets[name] = (instance.state.covariateTargets || {})[name] || [];
      });
      instance.state.covariateTypes = types;
      instance.state.covariateTargets = targets;
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
      removeModal();
    });
  }

  function structuralCovariateTargets(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    var ko = instance.language === "ko";
    if (!instance.state.covariates.length) {
      window.alert(ko ? "먼저 왼쪽 변수 목록에서 공변량을 설정하세요." : "Select covariates from the variable list first.");
      return;
    }
    var latents = instance.state.nodes.filter(function(node) { return node.role === "latent"; });
    if (!latents.length) {
      window.alert(ko ? "통제할 잠재변수를 먼저 만드세요." : "Create latent variables to control first.");
      return;
    }
    var paths = instance.state.edges.filter(function(edge) {
      if (edge.kind === "covariance") return false;
      var from = window.StatEduModelCanvas.nodes.nodeById(instance, edge.from);
      var to = window.StatEduModelCanvas.nodes.nodeById(instance, edge.to);
      return from && to && from.role === "latent" && to.role === "latent";
    });
    var finalIds = latents.filter(function(latent) {
      return paths.some(function(edge) { return edge.to === latent.id; }) &&
        !paths.some(function(edge) { return edge.from === latent.id; });
    }).map(function(latent) { return latent.id; });
    if (instance.analysisType === "cfa") {
      finalIds = latents.map(function(latent) { return latent.id; });
    }
    var shell = modalShell(ko ? "공변량 통제 대상" : "Covariate control targets");
    shell.modal.classList.add("structural-covariate-target-modal");
    var note = document.createElement("div");
    note.className = "structural-covariate-target-note";
    note.textContent = ko ? "최종 종속변수는 기본으로 통제됩니다. 공변량별로 추가 통제할 잠재변수를 선택하세요." : "Final dependent variables are controlled by default. Select additional targets for each covariate.";
    shell.body.appendChild(note);
    var table = document.createElement("div");
    table.className = "structural-covariate-target-table";
    table.style.setProperty("--target-columns", String(latents.length));
    var header = document.createElement("div");
    header.className = "structural-covariate-target-row is-header";
    var first = document.createElement("div");
    first.textContent = ko ? "공변량" : "Covariate";
    header.appendChild(first);
    latents.forEach(function(latent) {
      var cell = document.createElement("div");
      cell.textContent = window.StatEduModelCanvas.layout.displayText(latent);
      header.appendChild(cell);
    });
    table.appendChild(header);
    instance.state.covariates.forEach(function(covariate) {
      var row = document.createElement("div");
      row.className = "structural-covariate-target-row";
      var nameCell = document.createElement("div");
      nameCell.className = "structural-covariate-target-name";
      nameCell.textContent = covariate;
      row.appendChild(nameCell);
      var selected = (instance.state.covariateTargets || {})[covariate] || [];
      latents.forEach(function(latent) {
        var cell = document.createElement("label");
        cell.className = "structural-covariate-target-cell";
        var input = document.createElement("input");
        input.type = "checkbox";
        input.setAttribute("data-covariate", covariate);
        input.value = latent.id;
        input.checked = finalIds.indexOf(latent.id) >= 0 || selected.indexOf(latent.id) >= 0;
        input.disabled = finalIds.indexOf(latent.id) >= 0;
        cell.appendChild(input);
        row.appendChild(cell);
      });
      table.appendChild(row);
    });
    shell.body.appendChild(table);
    shell.apply.addEventListener("click", function() {
      window.StatEduModelCanvas.state.pushHistory(instance);
      instance.state.covariateTargets = {};
      instance.state.covariates.forEach(function(covariate) {
        instance.state.covariateTargets[covariate] = Array.from(shell.body.querySelectorAll("input[data-covariate]:checked:not(:disabled)")).filter(function(input) {
          return input.getAttribute("data-covariate") === covariate;
        }).map(function(input) { return input.value; });
      });
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
      removeModal();
    });
  }

  function paper(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    var t = function(key, fallback) { return window.StatEduModelCanvas.state.label(instance, key, fallback); };
    var shell = modalShell(t("paper_settings", "\uc6a9\uc9c0 \uc124\uc815"));
    var form = document.createElement("div");
    form.className = "custom-model-paper-form";
    form.innerHTML = [
      '<label class="custom-model-field-label">' + t("paper", "\uc6a9\uc9c0") + '</label>',
      '<select class="form-control custom-model-paper-size">',
      '<option value="B5">B5</option>',
      '<option value="A4">A4</option>',
      '<option value="Large">Large</option>',
      '</select>',
      '<label class="custom-model-field-label">' + t("orientation", "\ubc29\ud5a5") + '</label>',
      '<select class="form-control custom-model-paper-orientation">',
      '<option value="landscape">' + t("landscape", "Landscape") + '</option>',
      '<option value="portrait">' + t("portrait", "Portrait") + '</option>',
      '</select>',
      '<label class="custom-model-field-label">' + t("paper_view_zoom", "\uc6a9\uc9c0 \ubcf4\uae30 \ubc30\uc728") + '</label>',
      '<select class="form-control custom-model-paper-view-zoom">',
      '<option value="width">' + t("fit_width", "\ud3ed \ub9de\ucda4") + '</option>',
      '<option value="fit">' + t("fit_to_screen", "\uc804\uccb4 \ubcf4\uae30") + '</option>',
      '<option value="1.25">125%</option>',
      '<option value="1">100%</option>',
      '<option value="0.75">75%</option>',
      '<option value="0.5">50%</option>',
      '</select>'
    ].join("");
    shell.body.appendChild(form);
    form.querySelector(".custom-model-paper-size").value = instance.state.canvas.paper || "B5";
    form.querySelector(".custom-model-paper-orientation").value = instance.state.canvas.orientation || "landscape";
    var currentPaperZoom = window.StatEduModelCanvas.canvas.paperViewZoom ?
      window.StatEduModelCanvas.canvas.paperViewZoom(instance) :
      (instance.state.canvas.viewZoom || instance.state.canvas.zoom || 1);
    var currentViewMode = instance.state.canvas.paperViewMode || "width";
    form.querySelector(".custom-model-paper-view-zoom").value = currentViewMode === "fit" || currentViewMode === "width" ? currentViewMode : String(currentPaperZoom);
    shell.apply.addEventListener("click", function() {
      var paperName = form.querySelector(".custom-model-paper-size").value || "B5";
      var orientation = form.querySelector(".custom-model-paper-orientation").value || "landscape";
      var paperViewZoom = form.querySelector(".custom-model-paper-view-zoom").value || "width";
      var size = PAPER_SIZES[paperName] || PAPER_SIZES.B5;
      var widthMm = size.widthMm;
      var heightMm = size.heightMm;
      var widthPx = Number(size.widthPx || pxFromMm(widthMm));
      var heightPx = Number(size.heightPx || pxFromMm(heightMm));
      if (orientation === "portrait") {
        widthMm = size.heightMm;
        heightMm = size.widthMm;
        widthPx = Number(size.heightPx || pxFromMm(widthMm));
        heightPx = Number(size.widthPx || pxFromMm(heightMm));
      }
      window.StatEduModelCanvas.state.pushHistory(instance);
      instance.state.canvas.paper = paperName;
      instance.state.canvas.orientation = orientation;
      instance.state.canvas.widthMm = widthMm;
      instance.state.canvas.heightMm = heightMm;
      instance.state.canvas.widthPx = widthPx;
      instance.state.canvas.heightPx = heightPx;
      window.StatEduModelCanvas.canvas.resizeToViewport(instance);
      if (paperViewZoom === "fit" || paperViewZoom === "width") {
        instance.state.canvas.paperViewMode = paperViewZoom;
        window.StatEduModelCanvas.canvas.fitPaperToViewport(instance, true);
      } else {
        window.StatEduModelCanvas.canvas.setPaperViewZoom(instance, Number(paperViewZoom), "manual");
      }
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
      removeModal();
    });
  }

  function boundedNumber(value, fallback, min, max) {
    var number = Number(value);
    if (!Number.isFinite(number)) number = fallback;
    return Math.max(min, Math.min(max, number));
  }

  function downloadText(filename, text, type) {
    var blob = new Blob([text], {type: type || "application/json"});
    downloadBlob(filename, blob);
  }

  function downloadBlob(filename, blob) {
    var url = URL.createObjectURL(blob);
    var link = document.createElement("a");
    link.href = url;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    link.remove();
    window.setTimeout(function() {
      URL.revokeObjectURL(url);
    }, 0);
  }

  function timestampName(prefix, extension) {
    var now = new Date();
    var stamp = [
      now.getFullYear(),
      String(now.getMonth() + 1).padStart(2, "0"),
      String(now.getDate()).padStart(2, "0"),
      "-",
      String(now.getHours()).padStart(2, "0"),
      String(now.getMinutes()).padStart(2, "0")
    ].join("");
    return prefix + "-" + stamp + "." + extension;
  }

  function modelCanvasFileExtension(instance) {
    return {
      cfa: "stcfa",
      cbsem: "stsem",
      sem: "stsem",
      plssem: "stpls"
    }[String(instance && instance.analysisType || "")] || "streg";
  }

  function filePickerTypes(instance) {
    var extension = modelCanvasFileExtension(instance);
    return [{
      description: "StatEdu Model Canvas",
      accept: {"application/octet-stream": ["." + extension]}
    }];
  }

  function modelCanvasFilePrefix(instance) {
    return {
      cfa: "cfa-canvas",
      cbsem: "sem-canvas",
      sem: "sem-canvas",
      plssem: "pls-sem-canvas"
    }[String(instance && instance.analysisType || "")] || "regression-canvas";
  }

  function normalizedAnalysisType(value) {
    value = String(value || "").toLowerCase();
    return value === "sem" ? "cbsem" : value;
  }

  function pngFilePickerTypes() {
    return [{
      description: "PNG image",
      accept: {"image/png": [".png"]}
    }];
  }

  function desktopFilesApi() {
    var api = window.stateduDesktopFiles;
    return api && typeof api.openText === "function" && typeof api.save === "function" ? api : null;
  }

  function parseSnapshotText(raw) {
    var normalized = String(raw || "").replace(/^\uFEFF/, "").trim();
    if (!normalized) throw new Error("Empty model file");
    var snap = JSON.parse(normalized);
    if (snap && snap.snapshot && typeof snap.snapshot === "object") snap = snap.snapshot;
    if (snap && snap.state && typeof snap.state === "object") snap = snap.state;
    if (!snap || typeof snap !== "object" || !Array.isArray(snap.nodes) || !Array.isArray(snap.edges)) throw new Error("Invalid model structure");
    return snap;
  }

  function applySnapshotText(instance, raw) {
    if (!raw) return;
    try {
      var snap = parseSnapshotText(raw);
      var savedAnalysisType = normalizedAnalysisType(snap.analysisType || snap.analysis_type);
      var currentAnalysisType = normalizedAnalysisType(instance.analysisType);
      if (savedAnalysisType && currentAnalysisType && savedAnalysisType !== currentAnalysisType) {
        throw new Error("Model canvas analysis type does not match the current menu");
      }
      window.StatEduModelCanvas.state.pushHistory(instance);
      window.StatEduModelCanvas.state.restore(instance.state, snap);
      if (window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.stripPlsResidualNodes) {
        window.StatEduModelCanvas.canvas.stripPlsResidualNodes(instance);
      }
      instance.state.mode = "select";
      instance.sourceSnapshot = null;
      instance.resultSnapshot = null;
      instance.viewingResult = false;
      instance.root.classList.remove("is-viewing-result", "has-result");
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
      if (window.StatEduModelCanvas.bridge.notifyModelReplaced) {
        window.StatEduModelCanvas.bridge.notifyModelReplaced(instance, "load");
      }
    } catch (error) {
      var active = window.StatEduModelCanvas.activeInstance || null;
      window.alert(window.StatEduModelCanvas.state.label(active, "invalid_json", "Invalid model canvas file."));
    }
  }

  function openModelFileFallback(instance) {
    var input = document.createElement("input");
    input.type = "file";
    input.accept = "." + modelCanvasFileExtension(instance);
    input.style.display = "none";
    input.addEventListener("change", function() {
      var file = input.files && input.files[0];
      if (!file) {
        input.remove();
        return;
      }
      file.text().then(function(raw) {
        applySnapshotText(instance, raw);
      }).finally(function() {
        input.remove();
      });
    });
    document.body.appendChild(input);
    input.click();
  }

  function colorFieldHtml(classPrefix, label) {
    var active = window.StatEduModelCanvas.activeInstance || null;
    var options = COLOR_PRESETS.map(function(item) {
      var itemLabel = item.key ? window.StatEduModelCanvas.state.label(active, item.key, item.label) : item.label;
      return '<option value="' + item.value + '">' + itemLabel + '</option>';
    }).join("");
    return [
      '<label class="custom-model-field-label">' + label + '</label>',
      '<div class="custom-model-color-row">',
      '<select class="form-control ' + classPrefix + '-preset">' + options + '</select>',
      '<input class="form-control ' + classPrefix + '-custom" type="color">',
      '</div>'
    ].join("");
  }

  function setupColorField(form, classPrefix, value) {
    var preset = form.querySelector("." + classPrefix + "-preset");
    var custom = form.querySelector("." + classPrefix + "-custom");
    var normalized = String(value || "#000000").toLowerCase();
    var presetValues = COLOR_PRESETS.map(function(item) { return item.value; });
    preset.value = presetValues.indexOf(normalized) >= 0 ? normalized : "custom";
    custom.value = normalized;
    custom.disabled = preset.value !== "custom";
    preset.addEventListener("change", function() {
      custom.disabled = preset.value !== "custom";
      if (preset.value !== "custom") custom.value = preset.value;
    });
  }

  function colorFieldValue(form, classPrefix, fallback) {
    var preset = form.querySelector("." + classPrefix + "-preset").value;
    var custom = form.querySelector("." + classPrefix + "-custom").value;
    return preset === "custom" ? (custom || fallback) : (preset || fallback);
  }

  function escapeXml(value) {
    return String(value || "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function inlineComputedStyles(source, clone) {
    if (!source || !clone || source.nodeType !== 1 || clone.nodeType !== 1) return;
    var computed = window.getComputedStyle(source);
    var css = [];
    for (var index = 0; index < computed.length; index += 1) {
      var name = computed[index];
      css.push(name + ":" + computed.getPropertyValue(name) + ";");
    }
    clone.setAttribute("style", css.join(""));
    var sourceChildren = Array.prototype.filter.call(source.childNodes || [], function(node) { return node.nodeType === 1; });
    var cloneChildren = Array.prototype.filter.call(clone.childNodes || [], function(node) { return node.nodeType === 1; });
    sourceChildren.forEach(function(sourceChild, index) {
      inlineComputedStyles(sourceChild, cloneChildren[index]);
    });
  }

  // Validation badges are editor feedback, not part of the saved model figure.
  // Remove them on the clone only; computed styles have already captured the
  // warning/error border, so restore the model's configured node stroke too.
  function stripExportValidation(instance, clone) {
    clone.querySelectorAll(".structural-validation-badge").forEach(function(element) {
      element.remove();
    });
    clone.querySelectorAll(".custom-model-node.has-validation-error, .custom-model-node.has-validation-warning").forEach(function(element) {
      var nodeId = element.getAttribute("data-node-id");
      var node = (instance.state.nodes || []).find(function(item) { return item.id === nodeId; });
      element.classList.remove("has-validation-error", "has-validation-warning");
      element.removeAttribute("data-validation-message");
      var stroke = (node && node.strokeColor) || instance.state.style.boxStrokeColor || "#000000";
      ["border-color", "border-top-color", "border-right-color", "border-bottom-color", "border-left-color",
        "border-block-start-color", "border-block-end-color", "border-inline-start-color", "border-inline-end-color"].forEach(function(property) {
        element.style.setProperty(property, stroke);
      });
    });
  }

  function prepareExportClone(instance) {
    var paper = instance && instance.paper ? instance.paper : null;
    if (!paper) return null;
    var clone = paper.cloneNode(true);
    inlineComputedStyles(paper, clone);
    clone.setAttribute("xmlns", "http://www.w3.org/1999/xhtml");
    clone.querySelectorAll(".custom-model-edge-hit, .custom-model-moderation-hit, .custom-model-edge-control, .custom-model-drag-preview, .structural-alignment-guide-layer, .custom-model-score-shortcut").forEach(function(element) {
      element.remove();
    });
    stripExportValidation(instance, clone);
    clone.querySelectorAll(".custom-model-node.is-selected, .structural-latent-statistics.is-selected").forEach(function(element) {
      element.classList.remove("is-selected");
    });
    clone.classList.remove("is-grid-visible", "is-delete-mode", "is-connect-mode");
    clone.style.backgroundImage = "none";
    clone.style.backgroundSize = "auto";
    clone.style.position = "relative";
    clone.style.left = "0";
    clone.style.top = "0";
    clone.style.margin = "0";
    clone.style.boxShadow = "none";
    clone.style.transform = "none";
    clone.querySelectorAll(".custom-model-edge-layer, .custom-model-node-layer").forEach(function(layer) {
      layer.style.transform = "none";
    });
    return clone;
  }

  var SVG_NS = "http://www.w3.org/2000/svg";
  var SVG_STYLE_PROPERTIES = [
    "fill", "fill-opacity", "stroke", "stroke-opacity", "stroke-width", "stroke-dasharray",
    "stroke-linecap", "stroke-linejoin", "opacity", "color", "font-family", "font-size",
    "font-weight", "font-style", "text-anchor", "dominant-baseline", "paint-order"
  ];

  function svgElement(name, attributes) {
    var element = document.createElementNS(SVG_NS, name);
    Object.keys(attributes || {}).forEach(function(key) {
      if (attributes[key] !== null && attributes[key] !== undefined && attributes[key] !== "") {
        element.setAttribute(key, String(attributes[key]));
      }
    });
    return element;
  }

  function nodeVariableNames(node) {
    return [node.name, node.variable, node.variableId, node.dataLabel].filter(Boolean);
  }

  function covariateNodeIds(instance) {
    var names = instance.state.covariates || [];
    return new Set((instance.state.nodes || []).filter(function(node) {
      return node.role === "covariate" || nodeVariableNames(node).some(function(name) { return names.indexOf(name) >= 0; });
    }).map(function(node) { return node.id; }));
  }

  function modelFigureVariants(instance) {
    return (instance.state.covariates || []).length || covariateNodeIds(instance).size ?
      ["with_covariates", "without_covariates"] : ["model"];
  }

  function exportControlEffects(instance, variant) {
    if (variant === "without_covariates" || !window.StatEduModelCanvas.nodes.isViewingResult(instance)) return [];
    var names = instance.state.covariates || [];
    var drawn = (instance.state.nodes || []).reduce(function(names, node) { return names.concat(nodeVariableNames(node)); }, []);
    return (instance.state.covariateEffects || []).filter(function(effect) {
      return names.indexOf(effect.variable) >= 0 && drawn.indexOf(effect.variable) < 0;
    });
  }

  function exportGeometry(instance, variant) {
    var width = Number(instance.state.canvas.widthPx || instance.paper.offsetWidth || 0);
    var height = Number(instance.state.canvas.heightPx || instance.paper.offsetHeight || 0);
    var effects = exportControlEffects(instance, variant);
    var controlTop = Math.max.apply(null, [0].concat((instance.state.nodes || []).map(function(node) {
      return Number(node.y || 0) + Number(node.height || instance.state.style.boxHeight || 50);
    }))) + 70;
    return {width: width, height: effects.length ? Math.max(height, controlTop + effects.length * 30 + 24) : height,
      controlTop: controlTop, effects: effects};
  }

  // Filter copied elements by their owning model IDs, including coefficient
  // groups. Never remove correlations from the fitted model or live editor.
  function stripExportCovariates(instance, clone, variant) {
    var covariates = covariateNodeIds(instance);
    var hiddenNodes = new Set(variant === "without_covariates" ? covariates : []);
    var nodes = instance.state.nodes || [];
    var edges = instance.state.edges || [];
    nodes.forEach(function(node) {
      if (["error", "disturbance"].indexOf(node.role) >= 0 && edges.some(function(edge) {
        return edge.from === node.id && covariates.has(edge.to);
      })) hiddenNodes.add(node.id);
    });
    var hiddenEdges = new Set(edges.filter(function(edge) {
      if (hiddenNodes.has(edge.from) || hiddenNodes.has(edge.to)) return true;
      if (!covariates.has(edge.from) && !covariates.has(edge.to)) return false;
      return edge.kind === "covariance" || !covariates.has(edge.from) || covariates.has(edge.to);
    }).map(function(edge) { return edge.id; }));
    var hiddenModerations = new Set((instance.state.moderations || []).filter(function(item) {
      return hiddenNodes.has(item.from) || hiddenEdges.has(item.toEdge);
    }).map(function(item) { return item.id; }));
    clone.querySelectorAll("[data-node-id], [data-edge-id], [data-moderation-id], [data-label-id]").forEach(function(element) {
      var labelId = element.getAttribute("data-label-id");
      if (hiddenNodes.has(element.getAttribute("data-node-id")) ||
          hiddenEdges.has(element.getAttribute("data-edge-id")) ||
          hiddenModerations.has(element.getAttribute("data-moderation-id")) ||
          (element.getAttribute("data-label-type") === "edge" && hiddenEdges.has(labelId)) ||
          (element.getAttribute("data-label-type") === "moderation" && hiddenModerations.has(labelId))) element.remove();
    });
    clone.querySelectorAll(".custom-model-node").forEach(function(element) {
      if (covariates.has(element.getAttribute("data-node-id"))) {
        element.querySelectorAll(".structural-latent-statistics").forEach(function(stats) { stats.remove(); });
      }
    });
  }

  // Serialize the displayed DOM, including browser text wrapping and SVG paths.
  // Export never changes selection, recomputes layout, or redraws the model.
  function exportSvg(instance, variant) {
    var geometry = exportGeometry(instance, variant);
    var width = geometry.width;
    var height = geometry.height;
    var clone = instance.paper.cloneNode(true);
    var sources = [instance.paper].concat(Array.from(instance.paper.querySelectorAll("*")));
    var copies = [clone].concat(Array.from(clone.querySelectorAll("*")));
    sources.forEach(function(source, index) {
      var computed = window.getComputedStyle(source);
      var target = copies[index];
      for (var i = 0; i < computed.length; i++) {
        var property = computed[i];
        var value = computed.getPropertyValue(property);
        // Computed SVG marker URLs are absolute; keep copied marker references local.
        value = value.replace(/url\([\"\']?[^)\"\']*#([^)\"\']+)[\"\']?\)/g, "url(#$1)");
        target.style.setProperty(property, value);
      }
    });
    stripExportValidation(instance, clone);
    stripExportCovariates(instance, clone, variant);
    clone.querySelectorAll(".custom-model-edge-hit, .custom-model-edge-label-hit, .custom-model-moderation-hit, .custom-model-edge-control, .custom-model-drag-preview, .custom-model-node-resize-handle, .structural-alignment-guide-layer, .custom-model-score-shortcut").forEach(function(element) { element.remove(); });
    clone.querySelectorAll(".custom-model-edge-label, .custom-model-edge-label-bg").forEach(function(element) {
      element.style.setProperty("stroke", "none", "important");
      element.style.setProperty("border", "0", "important");
      element.style.setProperty("border-block", "0", "important");
      element.style.setProperty("border-inline", "0", "important");
      element.style.setProperty("outline", "none", "important");
      element.style.setProperty("box-shadow", "none", "important");
    });
    clone.style.transform = "none";
    clone.style.margin = "0";
    clone.style.position = "relative";
    clone.style.left = "0";
    clone.style.top = "0";
    clone.style.width = width + "px";
    clone.style.height = height + "px";
    clone.style.backgroundImage = "none";
    clone.style.backgroundColor = "transparent";
    clone.style.boxShadow = "none";
    clone.style.border = "0";
    clone.style.borderBlock = "0";
    clone.style.borderInline = "0";
    clone.style.outline = "none";
    clone.setAttribute("xmlns", "http://www.w3.org/1999/xhtml");
    var svg = svgElement("svg", {xmlns: SVG_NS, width: width, height: height, viewBox: "0 0 " + width + " " + height});
    var content = svgElement("foreignObject", {x: 0, y: 0, width: width, height: height});
    content.appendChild(clone);
    svg.appendChild(content);
    // List-only controls have no live diagram nodes. Export their already fitted
    // effects as clear rows, without generating covariance lines or refitting.
    geometry.effects.forEach(function(effect, index) {
      var target = (instance.state.nodes || []).find(function(node) {
        return nodeVariableNames(node).indexOf(effect.target) >= 0;
      });
      var text = svgElement("text", {x: 24, y: geometry.controlTop + index * 30,
        "font-family": instance.state.style.fontFamily || "Arial", "font-size": 15, fill: "#000000",
        "data-export-control-effect": effect.variable});
      text.textContent = effect.variable + " → " + (target ? window.StatEduModelCanvas.layout.displayText(target) : effect.target) + "  " + effect.label;
      svg.appendChild(text);
    });
    return new XMLSerializer().serializeToString(svg);
  }

  // Canvas encoders default to 96dpi. Write pHYs as well as the correct pixels.
  async function pngWithDpi(blob, dpi) {
    var bytes = new Uint8Array(await blob.arrayBuffer());
    var chunk = new Uint8Array(21);
    var view = new DataView(chunk.buffer);
    view.setUint32(0, 9);
    chunk.set([112, 72, 89, 115], 4);
    var ppm = Math.round(dpi / 0.0254);
    view.setUint32(8, ppm); view.setUint32(12, ppm); chunk[16] = 1;
    var crc = 0xffffffff;
    for (var i = 4; i < 17; i++) {
      crc ^= chunk[i];
      for (var bit = 0; bit < 8; bit++) crc = (crc >>> 1) ^ ((crc & 1) ? 0xedb88320 : 0);
    }
    view.setUint32(17, (crc ^ 0xffffffff) >>> 0);
    var parts = [bytes.slice(0, 8)];
    var data = new DataView(bytes.buffer);
    for (var offset = 8; offset < bytes.length;) {
      var size = data.getUint32(offset) + 12;
      var type = String.fromCharCode.apply(null, bytes.slice(offset + 4, offset + 8));
      if (type !== 'pHYs') parts.push(bytes.slice(offset, offset + size));
      if (type === 'IHDR') parts.push(chunk);
      offset += size;
    }
    return new Blob(parts, {type: 'image/png'});
  }

  async function exportReportFigure(instance) {
    var figures = [];
    for (var variant of modelFigureVariants(instance)) {
      figures.push(await exportReportFigureVariant(instance, variant));
    }
    return figures.join("");
  }

  async function exportReportFigureVariant(instance, variant) {
    if (!window.StatEduModelCanvas.nodes.isViewingResult(instance)) {
      throw new Error(instance.language === 'ko' ? '결과 모형을 표시한 뒤 저장해 주세요.' : 'Display the result model before saving.');
    }
    var blob = await exportPng(instance, variant);
    var pngHeader = new DataView(await blob.slice(0, 24).arrayBuffer());
    var dataUrl = await new Promise(function(resolve, reject) {
      var reader = new FileReader();
      reader.onload = function() { resolve(reader.result); };
      reader.onerror = reject;
      reader.readAsDataURL(blob);
    });
    var section = document.createElement('div');
    section.className = 'model-canvas-report-figure';
    section.setAttribute('data-result-table-sheet', 'true');
    section.setAttribute('data-result-table-orientation', instance.paper.offsetWidth > instance.paper.offsetHeight ? 'landscape' : 'portrait');
    section.style.cssText = 'break-before:page;break-inside:avoid;background:white;width:100%;max-width:100%;';
    var title = document.createElement('h3');
    title.textContent = window.StatEduModelCanvas.state.label(instance, 'score_model_results', 'Model results');
    if (variant !== 'model') title.textContent += ' (' + window.StatEduModelCanvas.state.label(instance,
      variant === 'with_covariates' ? 'score_with_covariates' : 'score_without_covariates',
      variant === 'with_covariates' ? 'with covariates' : 'without covariates') + ')';
    section.setAttribute('data-model-figure-variant', variant);
    var image = document.createElement('img');
    image.className = 'analysis-plot-image';
    image.width = pngHeader.getUint32(16);
    image.height = pngHeader.getUint32(20);
    image.src = dataUrl; image.alt = title.textContent;
    image.style.cssText = 'display:block;width:100%;height:auto;max-width:100%;';
    section.appendChild(title); section.appendChild(image);
    return section.outerHTML;
  }

  function exportPng(instance, variant) {
    var geometry = exportGeometry(instance, variant);
    var width = geometry.width;
    var height = geometry.height;
    var dpi = Number(instance.root.getAttribute("data-export-dpi")) === 600 ? 600 : 300;
    var scale = dpi / 96;
    var url = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(exportSvg(instance, variant));
    return new Promise(function(resolve, reject) {
      var image = new Image();
      image.onload = function() {
        try {
          var canvas = document.createElement("canvas");
          canvas.width = Math.max(1, Math.round(width * scale));
          canvas.height = Math.max(1, Math.round(height * scale));
          var context = canvas.getContext("2d");
          context.scale(scale, scale);
          context.drawImage(image, 0, 0, width, height);
          // Crop the rendered pixels, not node boxes: curved paths, arrowheads,
          // labels and error terms can extend beyond the variable rectangles.
          var pixels = context.getImageData(0, 0, canvas.width, canvas.height).data;
          var left = canvas.width, top = canvas.height, right = -1, bottom = -1;
          for (var y = 0; y < canvas.height; y++) {
            for (var x = 0; x < canvas.width; x++) {
              if (pixels[(y * canvas.width + x) * 4 + 3] !== 0) {
                left = Math.min(left, x); right = Math.max(right, x);
                top = Math.min(top, y); bottom = y;
              }
            }
          }
          var cropped = canvas;
          if (right >= left && bottom >= top) {
            var padding = Math.ceil(12 * scale);
            cropped = document.createElement("canvas");
            cropped.width = right - left + 1 + padding * 2;
            cropped.height = bottom - top + 1 + padding * 2;
            cropped.getContext("2d").drawImage(canvas, left, top, right - left + 1, bottom - top + 1,
              padding, padding, right - left + 1, bottom - top + 1);
          }
          cropped.toBlob(function(blob) {
            URL.revokeObjectURL(url);
            cropped.width = cropped.height = 1;
            canvas.width = canvas.height = 1;
            if (blob) pngWithDpi(blob, dpi).then(resolve, reject);
            else reject(new Error("PNG export failed"));
          }, "image/png");
        } catch (error) {
          URL.revokeObjectURL(url);
          reject(error);
        }
      };
      image.onerror = function() {
        URL.revokeObjectURL(url);
        reject(new Error("PNG export failed"));
      };
      image.src = url;
    });
  }

  function style(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    var t = function(key, fallback) { return window.StatEduModelCanvas.state.label(instance, key, fallback); };
    var shell = modalShell(t("style_settings", "\uc2a4\ud0c0\uc77c \uc124\uc815"));
    var current = instance.state.style;
    var form = document.createElement("div");
    form.className = "custom-model-style-form";
    form.innerHTML = [
      colorFieldHtml("custom-model-style-box-color", t("box_line_color", "\ubc15\uc2a4 \uc120 \uc0c9")),
      '<label class="custom-model-field-label">' + t("box_line_width", "\ubc15\uc2a4 \uc120 \uad75\uae30") + '</label>',
      '<input class="form-control custom-model-style-box-width" type="number" min="0.5" max="8" step="0.5">',
      colorFieldHtml("custom-model-style-edge-color", t("arrow_line_color", "\ud654\uc0b4\ud45c \uc120 \uc0c9")),
      '<label class="custom-model-field-label">' + t("arrow_line_width", "\ud654\uc0b4\ud45c \uc120 \uad75\uae30") + '</label>',
      '<input class="form-control custom-model-style-edge-width" type="number" min="0.5" max="8" step="0.5">',
      '<label class="custom-model-field-label">' + t("arrow_head", "\ud654\uc0b4\ud45c \ub05d") + '</label>',
      '<select class="form-control custom-model-style-arrow-head">',
      '<option value="triangle">\u25b6 ' + t("triangle", "\uc0bc\uac01\ud615") + '</option>',
      '<option value="line">--&gt; ' + t("arrow", "\ud654\uc0b4\ud45c") + '</option>',
      '<option value="open">\u25b7 ' + t("open_triangle", "\uc5f4\ub9b0 \uc0bc\uac01\ud615") + '</option>',
      '<option value="circle">\u25cf ' + t("circle", "\uc6d0\ud615") + '</option>',
      '<option value="none">' + t("none", "\uc5c6\uc74c") + '</option>',
      '</select>',
      '<label class="custom-model-field-label">' + t("font_size", "\ud3f0\ud2b8 \ud06c\uae30") + '</label>',
      '<input class="form-control custom-model-style-font-size" type="number" min="8" max="32" step="1">',
      '<label class="custom-model-field-label">' + t("b_p_font", "B(p) \ud3f0\ud2b8") + '</label>',
      '<input class="form-control custom-model-style-label-font-size" type="number" min="8" max="32" step="1">'
    ].join("");
    shell.body.appendChild(form);

    setupColorField(form, "custom-model-style-box-color", current.boxStrokeColor || "#000000");
    form.querySelector(".custom-model-style-box-width").value = current.boxStrokeWidth || 1.5;
    setupColorField(form, "custom-model-style-edge-color", current.edgeStrokeColor || "#000000");
    form.querySelector(".custom-model-style-edge-width").value = current.edgeStrokeWidth || 1.8;
    form.querySelector(".custom-model-style-arrow-head").value = current.arrowHead || "triangle";
    form.querySelector(".custom-model-style-font-size").value = current.fontSize || 11;
    form.querySelector(".custom-model-style-label-font-size").value = current.labelFontSize || 12;

    shell.apply.addEventListener("click", function() {
      window.StatEduModelCanvas.state.pushHistory(instance);
      current.boxStrokeColor = colorFieldValue(form, "custom-model-style-box-color", "#000000");
      current.boxStrokeWidth = boundedNumber(form.querySelector(".custom-model-style-box-width").value, 1.5, 0.5, 8);
      current.edgeStrokeColor = colorFieldValue(form, "custom-model-style-edge-color", "#000000");
      current.edgeStrokeWidth = boundedNumber(form.querySelector(".custom-model-style-edge-width").value, 1.8, 0.5, 8);
      current.arrowHead = form.querySelector(".custom-model-style-arrow-head").value || "triangle";
      current.fontSize = boundedNumber(form.querySelector(".custom-model-style-font-size").value, 11, 8, 32);
      current.labelFontSize = boundedNumber(form.querySelector(".custom-model-style-label-font-size").value, 12, 8, 32);
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
      removeModal();
    });
  }

  function reset(instance, includeCovariates) {
    window.StatEduModelCanvas.state.pushHistory(instance);
    instance.sourceSnapshot = null;
    instance.resultSnapshot = null;
    instance.viewingResult = false;
    instance.root.classList.remove("is-viewing-result", "has-result");
    instance.state.mode = "select";
    instance.state.selectedNodeId = null;
    instance.state.selectedNodeIds = [];
    instance.state.selectedEdgeId = null;
    instance.state.selectedModerationId = null;
    instance.state.nodes = [];
    instance.state.edges = [];
    instance.state.moderations = [];
    if (includeCovariates !== false) {
      instance.state.covariates = [];
      instance.state.covariateTypes = {};
      instance.state.covariateTargets = {};
    }
    window.StatEduModelCanvas.canvas.render(instance);
    window.StatEduModelCanvas.bridge.sendState(instance);
    if (window.StatEduModelCanvas.bridge.notifyModelReplaced) {
      window.StatEduModelCanvas.bridge.notifyModelReplaced(instance, "reset");
    }
  }

  async function save(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    var snapshot = window.StatEduModelCanvas.nodes.isViewingResult(instance) && instance.sourceSnapshot
      ? window.StatEduModelCanvas.state.clone(instance.sourceSnapshot)
      : window.StatEduModelCanvas.state.snapshot(instance.state);
    if (window.StatEduModelCanvas.canvas && window.StatEduModelCanvas.canvas.sanitizePlsSnapshot) {
      snapshot = window.StatEduModelCanvas.canvas.sanitizePlsSnapshot(instance, snapshot);
    }
    if (instance.analysisType) snapshot.analysisType = normalizedAnalysisType(instance.analysisType);
    var payload = JSON.stringify(snapshot, null, 2);
    var filename = timestampName(modelCanvasFilePrefix(instance), modelCanvasFileExtension(instance));
    var desktop = desktopFilesApi();
    if (desktop) {
      try {
        await desktop.save({
          suggestedName: filename,
          description: "StatEdu Model Canvas",
          extensions: [modelCanvasFileExtension(instance)],
          text: payload,
          binary: false
        });
        return;
      } catch (error) {
        // Fall through to the browser picker when the desktop bridge is unavailable.
      }
    }
    if (window.showSaveFilePicker) {
      try {
        var handle = await window.showSaveFilePicker({
          suggestedName: filename,
          types: filePickerTypes(instance)
        });
        var writable = await handle.createWritable();
        await writable.write(payload);
        await writable.close();
        return;
      } catch (error) {
        if (error && error.name === "AbortError") return;
      }
    }
    downloadText(filename, payload, "application/octet-stream");
  }

  async function load(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    var desktop = desktopFilesApi();
    if (desktop) {
      try {
        var result = await desktop.openText({
          description: "StatEdu Model Canvas",
          extensions: [modelCanvasFileExtension(instance)]
        });
        if (!result || result.canceled) return;
        applySnapshotText(instance, result.text);
        return;
      } catch (error) {
        // Fall through to the browser picker when the desktop bridge is unavailable.
      }
    }
    if (window.showOpenFilePicker) {
      try {
        var handles = await window.showOpenFilePicker({
          types: filePickerTypes(instance),
          multiple: false
        });
        if (!handles || !handles.length) return;
        var file = await handles[0].getFile();
        applySnapshotText(instance, await file.text());
        return;
      } catch (error) {
        if (error && error.name === "AbortError") return;
      }
    }
    openModelFileFallback(instance);
  }

  async function collectMediationFigures(instance, modelBlob) {
    var files = [];
    var dataUrl = await new Promise(function(resolve, reject) {
      var reader = new FileReader();
      reader.onload = function() { resolve(reader.result); };
      reader.onerror = reject;
      reader.readAsDataURL(modelBlob);
    });
    var variants = modelFigureVariants(instance);
    files.push({name: variants[0] === 'model' ? 'model.png' : 'model_' + variants[0] + '.png', data: dataUrl});
    if (variants.length > 1) {
      var without = await exportPng(instance, 'without_covariates');
      var withoutUrl = await new Promise(function(resolve, reject) {
        var reader = new FileReader();
        reader.onload = function() { resolve(reader.result); };
        reader.onerror = reject;
        reader.readAsDataURL(without);
      });
      files.push({name: 'model_without_covariates.png', data: withoutUrl});
    }
    var prefix = instance.root.getAttribute('data-input-prefix') || 'custom_model_canvas';
    var output = document.getElementById(prefix + '_results') ||
      document.getElementById(prefix.replace(/_canvas$/, '') + '_results');
    if (output) {
      var plots = Array.from(output.querySelectorAll('img.analysis-plot-image'));
      for (var index = 0; index < plots.length; index++) {
        var plot = plots[index];
        var source = plot.currentSrc || plot.src;
        if (!/^data:image\/png;base64,/i.test(source)) throw new Error('The displayed plot is not a PNG snapshot.');
        // currentSrc may encode R's base64 line breaks as %0A after image loading.
        source = 'data:image/png;base64,' + decodeURIComponent(source.slice(source.indexOf(',') + 1)).replace(/[\t\n\f\r ]/g, '');
        var kind = plot.getAttribute('data-plot-kind') || 'conditional_effect';
        var label = kind.indexOf('johnson_neyman') >= 0 ? 'johnson_neyman' :
          (instance.root.classList.contains('mediation-moderation-canvas-root') ? 'conditional_effect' : 'figure');
        files.push({name: label + '_' + String(index + 1).padStart(2, '0') + '.png', data: source});
      }
    }
    return files;
  }

  async function exportModel(instance) {
    var scopePrefix = instance.root.getAttribute('data-input-prefix') || 'custom_model_canvas';
    var scopeOutput = [scopePrefix + '_results', scopePrefix.replace(/_canvas$/, '') + '_results'].find(function(id) {
      return window.stateduSplitSnapshots && window.stateduSplitSnapshots[id];
    });
    if (scopeOutput && window.Shiny && typeof window.Shiny.setInputValue === 'function') {
      window.Shiny.setInputValue('scope_save', {outputId: scopeOutput, format: 'figures', nonce: Date.now()}, {priority: 'event'});
      return;
    }
    window.StatEduModelCanvas.activeInstance = instance;
    var filename = timestampName("model-canvas", "png");
    var payload;
    try {
      payload = await exportPng(instance);
      if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
        var files = await collectMediationFigures(instance, payload);
        var prefix = instance.root.getAttribute('data-input-prefix') || 'custom_model_canvas';
        window.Shiny.setInputValue(prefix + '_figures_snapshot', {files: files, nonce: Date.now()}, {priority: 'event'});
        return;
      }
    } catch (error) {
      window.alert(instance.language === "ko" ?
        "PNG 그림을 만들지 못했습니다. 다시 시도해 주세요." :
        "Could not create the PNG image. Please try again.");
      return;
    }
    var variants = modelFigureVariants(instance);
    for (var index = 0; index < variants.length; index++) {
      var variant = variants[index];
      var variantName = variant === 'model' ? filename : filename.replace(/\.png$/, '_' + variant + '.png');
      var variantPayload = index === 0 ? payload : await exportPng(instance, variant);
      if (!await savePngPayload(instance, variantName, variantPayload)) return;
    }
  }

  async function savePngPayload(instance, filename, payload) {
    var desktop = desktopFilesApi();
    if (desktop) {
      try {
        var desktopResult = await desktop.save({
          suggestedName: filename,
          description: "PNG image",
          extensions: ["png"],
          data: new Uint8Array(await payload.arrayBuffer()),
          binary: true
        });
        if (!desktopResult || desktopResult.canceled) return false;
        return true;
      } catch (error) {
        window.alert(instance.language === "ko" ?
          "PNG 저장 창을 열거나 파일을 저장하지 못했습니다." :
          "Could not open the PNG save dialog or save the file.");
        return false;
      }
    }
    if (window.showSaveFilePicker) {
      try {
        var handle = await window.showSaveFilePicker({
          suggestedName: filename,
          types: pngFilePickerTypes()
        });
        var writable = await handle.createWritable();
        await writable.write(payload);
        await writable.close();
        return true;
      } catch (error) {
        if (error && error.name === "AbortError") return false;
      }
    }
    downloadBlob(filename, payload);
    return true;
  }

  function run(instance) {
    window.StatEduModelCanvas.bridge.sendState(instance);
    window.StatEduModelCanvas.bridge.run(instance);
    var popover = instance.root.querySelector(".custom-model-run-options-popover");
    if (!popover) return;
    popover.classList.add("is-visible");
    var firstInput = popover.querySelector("select, input, button");
    if (firstInput && typeof firstInput.focus === "function") firstInput.focus();
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.dialogs = {
    measuredVariable: measuredVariable,
    chooseRole: chooseRole,
    covariates: covariates,
    structuralCovariateTargets: structuralCovariateTargets,
    paper: paper,
    style: style,
    reset: reset,
    save: save,
    load: load,
    exportModel: exportModel,
    collectMediationFigures: collectMediationFigures,
    exportPng: exportPng,
    exportReportFigure: exportReportFigure,
    parseSnapshotText: parseSnapshotText,
    run: run
  };
})();
