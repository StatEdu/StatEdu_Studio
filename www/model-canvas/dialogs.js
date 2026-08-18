(function() {
  "use strict";

  var ROLE_ORDER = ["independent", "mediator", "moderator", "dependent"];
  var PAPER_SIZES = {
    B5: {widthMm: 257, heightMm: 182},
    A4: {widthMm: 297, heightMm: 210}
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
      if (orientation === "portrait") {
        widthMm = size.heightMm;
        heightMm = size.widthMm;
      }
      window.StatEduModelCanvas.state.pushHistory(instance);
      instance.state.canvas.paper = paperName;
      instance.state.canvas.orientation = orientation;
      instance.state.canvas.widthMm = widthMm;
      instance.state.canvas.heightMm = heightMm;
      instance.state.canvas.widthPx = pxFromMm(widthMm);
      instance.state.canvas.heightPx = pxFromMm(heightMm);
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

  function filePickerTypes() {
    return [{
      description: "StatEdu Model Canvas",
      accept: {"application/json": [".stmodel", ".studio", ".json"]}
    }];
  }

  function svgFilePickerTypes() {
    return [{
      description: "SVG image",
      accept: {"image/svg+xml": [".svg"]}
    }];
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
      window.StatEduModelCanvas.state.pushHistory(instance);
      window.StatEduModelCanvas.state.restore(instance.state, snap);
      instance.sourceSnapshot = null;
      instance.resultSnapshot = null;
      instance.viewingResult = false;
      instance.root.classList.remove("is-viewing-result", "has-result");
      window.StatEduModelCanvas.canvas.render(instance);
      window.StatEduModelCanvas.bridge.sendState(instance);
    } catch (error) {
      var active = window.StatEduModelCanvas.activeInstance || null;
      window.alert(window.StatEduModelCanvas.state.label(active, "invalid_json", "Invalid model JSON."));
    }
  }

  function openModelFileFallback(instance) {
    var input = document.createElement("input");
    input.type = "file";
    input.accept = ".stmodel,.studio,.json,application/json";
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

  function prepareExportClone(instance) {
    var paper = instance && instance.paper ? instance.paper : null;
    if (!paper) return null;
    var clone = paper.cloneNode(true);
    inlineComputedStyles(paper, clone);
    clone.setAttribute("xmlns", "http://www.w3.org/1999/xhtml");
    clone.querySelectorAll(".custom-model-edge-hit, .custom-model-moderation-hit, .custom-model-edge-control, .custom-model-drag-preview").forEach(function(element) {
      element.remove();
    });
    clone.querySelectorAll(".structural-validation-badge").forEach(function(element) {
      element.remove();
    });
    clone.querySelectorAll(".custom-model-node.is-selected, .structural-latent-statistics.is-selected").forEach(function(element) {
      element.classList.remove("is-selected");
    });
    clone.classList.remove("is-grid-visible", "is-delete-mode", "is-connect-mode");
    clone.style.backgroundImage = "none";
    clone.style.backgroundSize = "auto";
    clone.querySelectorAll(".custom-model-node.has-validation-error, .custom-model-node.has-validation-warning").forEach(function(element) {
      element.classList.remove("has-validation-error", "has-validation-warning");
      element.removeAttribute("data-validation-message");
    });
    clone.style.position = "relative";
    clone.style.left = "0";
    clone.style.top = "0";
    clone.style.margin = "0";
    clone.style.boxShadow = "none";
    clone.style.transform = "none";
    return clone;
  }

  function exportSvg(instance) {
    window.StatEduModelCanvas.canvas.render(instance);
    var width = Number(instance.state.canvas.widthPx || instance.paper.offsetWidth || 0);
    var height = Number(instance.state.canvas.heightPx || instance.paper.offsetHeight || 0);
    var svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
    svg.setAttribute("width", width);
    svg.setAttribute("height", height);
    svg.setAttribute("viewBox", "0 0 " + width + " " + height);
    var foreignObject = document.createElementNS("http://www.w3.org/2000/svg", "foreignObject");
    foreignObject.setAttribute("x", "0");
    foreignObject.setAttribute("y", "0");
    foreignObject.setAttribute("width", width);
    foreignObject.setAttribute("height", height);
    foreignObject.appendChild(prepareExportClone(instance));
    svg.appendChild(foreignObject);
    return '<?xml version="1.0" encoding="UTF-8"?>\n' + new XMLSerializer().serializeToString(svg);
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
  }

  async function save(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    var payload = JSON.stringify(window.StatEduModelCanvas.state.snapshot(instance.state), null, 2);
    if (window.showSaveFilePicker) {
      try {
        var handle = await window.showSaveFilePicker({
          suggestedName: timestampName("model-canvas", "stmodel"),
          types: filePickerTypes()
        });
        var writable = await handle.createWritable();
        await writable.write(payload);
        await writable.close();
        return;
      } catch (error) {
        if (error && error.name === "AbortError") return;
      }
    }
    downloadText(timestampName("model-canvas", "stmodel"), payload, "application/json");
  }

  async function load(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    if (window.showOpenFilePicker) {
      try {
        var handles = await window.showOpenFilePicker({
          types: filePickerTypes(),
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

  async function exportModel(instance) {
    window.StatEduModelCanvas.activeInstance = instance;
    var payload = exportSvg(instance);
    if (window.showSaveFilePicker) {
      try {
        var handle = await window.showSaveFilePicker({
          suggestedName: timestampName("model-canvas", "svg"),
          types: svgFilePickerTypes()
        });
        var writable = await handle.createWritable();
        await writable.write(payload);
        await writable.close();
        return;
      } catch (error) {
        if (error && error.name === "AbortError") return;
      }
    }
    downloadText(timestampName("model-canvas", "svg"), payload, "image/svg+xml");
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
    chooseRole: chooseRole,
    covariates: covariates,
    structuralCovariateTargets: structuralCovariateTargets,
    paper: paper,
    style: style,
    reset: reset,
    save: save,
    load: load,
    exportModel: exportModel,
    parseSnapshotText: parseSnapshotText,
    run: run
  };
})();
