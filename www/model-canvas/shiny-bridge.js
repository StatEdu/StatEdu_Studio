(function() {
  "use strict";

  function sendState(instance) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var payload = window.StatEduModelCanvas.state.snapshot(instance.state);
    payload.nonce = Date.now() + Math.random();
    Shiny.setInputValue("custom_model_canvas_state", payload, {priority: "event"});
  }

  function run(instance) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var payload = window.StatEduModelCanvas.state.snapshot(instance.state);
    payload.nonce = Date.now() + Math.random();
    Shiny.setInputValue("custom_model_canvas_run_request", payload, {priority: "event"});
  }

  function runConfirm(instance) {
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var payload = window.StatEduModelCanvas.state.snapshot(instance.state);
    payload.nonce = Date.now() + Math.random();
    Shiny.setInputValue("custom_model_canvas_run_confirm", payload, {priority: "event"});
  }

  function applyResult(message) {
    var root = document.querySelector(".custom-model-canvas-root");
    if (!root || !window.StatEduModelCanvas || !window.StatEduModelCanvas.canvas) return;
    var instance = window.StatEduModelCanvas.canvas.init(root);
    if (!instance) return;
    instance.sourceSnapshot = message && message.source ? window.StatEduModelCanvas.state.clone(message.source) : null;
    instance.resultSnapshot = message && message.result ? window.StatEduModelCanvas.state.clone(message.result) : null;
    root.classList.toggle("has-result", !!instance.resultSnapshot);
    if (message && message.show && instance.resultSnapshot) {
      if (window.StatEduModelCanvas.toolbar && window.StatEduModelCanvas.toolbar.setActiveGroup) {
        window.StatEduModelCanvas.toolbar.setActiveGroup(instance, "result");
      } else {
        window.StatEduModelCanvas.canvas.showResult(instance);
      }
    }
  }

  function bindHandlers() {
    if (!window.Shiny || typeof Shiny.addCustomMessageHandler !== "function" || window.StatEduModelCanvas.resultHandlerBound) return;
    window.StatEduModelCanvas.resultHandlerBound = true;
    Shiny.addCustomMessageHandler("custom-model-canvas-result", applyResult);
  }

  window.StatEduModelCanvas = window.StatEduModelCanvas || {};
  window.StatEduModelCanvas.bridge = {
    sendState: sendState,
    run: run,
    runConfirm: runConfirm,
    applyResult: applyResult
  };

  bindHandlers();
  document.addEventListener("shiny:connected", bindHandlers);
})();
