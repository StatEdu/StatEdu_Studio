const { contextBridge, ipcRenderer, webUtils } = require("electron");

contextBridge.exposeInMainWorld("stateduDesktopFiles", {
  setDataDirectory(directory) {
    ipcRenderer.send("statedu:data-directory", directory);
  },
  dataFilePath() {
    const input = document.getElementById("file");
    return input && input.files && input.files.length ? webUtils.getPathForFile(input.files[0]) : "";
  },
  openText(options) {
    return ipcRenderer.invoke("statedu:canvas-open-text", options || {});
  },
  save(options) {
    return ipcRenderer.invoke("statedu:canvas-save", options || {});
  },
  chooseResultOpen(options) {
    return ipcRenderer.invoke("statedu:result-choose-open", options || {});
  },
  chooseResultSave(options) {
    return ipcRenderer.invoke("statedu:result-choose-save", options || {});
  }
});

window.addEventListener("DOMContentLoaded", () => {
  document.addEventListener("change", (event) => {
    const input = event.target;
    if (!input || input.id !== "file" || input.type !== "file" || !input.files || !input.files.length) return;
    const filePath = webUtils.getPathForFile(input.files[0]);
    if (filePath) ipcRenderer.send("statedu:data-file-path", filePath);
  }, true);
});
