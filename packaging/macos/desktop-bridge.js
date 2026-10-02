'use strict';
const fs = require('node:fs');
const path = require('node:path');

// A private per-launch queue also works while R is blocked in a file selector.
function createDesktopBridge({app, BrowserWindow, dialog, window, directory}) {
  let busy = false;
  const timer = setInterval(async () => {
    if (busy) return;
    const name = fs.readdirSync(directory).find(name => /^[a-f0-9]+\.request\.json$/.test(name));
    if (!name) return;
    busy = true;
    const requestPath = path.join(directory, name);
    const responsePath = requestPath.replace('.request.json', '.response.json');
    let response;
    try {
      const request = JSON.parse(fs.readFileSync(requestPath, 'utf8'));
      const options = {title: String(request.title || 'StatEdu Studio'), defaultPath: String(request.defaultPath || app.getPath('documents'))};
      const extensions = (request.extensions || []).filter(value => typeof value === 'string' && /^(\*|[a-z0-9]+)$/.test(value));
      if (extensions.length) options.filters = [{name: String(request.description || 'Files'), extensions}];
      if (request.operation === 'pdf') {
        const pdfWindow = new BrowserWindow({show:false, webPreferences:{sandbox:true, contextIsolation:true, nodeIntegration:false, javascript:false}});
        try {
          pdfWindow.webContents.setWindowOpenHandler(() => ({action:'deny'}));
          await pdfWindow.loadFile(request.htmlPath);
          const pdf = await pdfWindow.webContents.printToPDF({printBackground:true, preferCSSPageSize:true, displayHeaderFooter:false});
          fs.writeFileSync(request.pdfPath, pdf);
          response = {canceled:false, filePath:request.pdfPath};
        } finally { pdfWindow.destroy(); }
      } else if (request.operation === 'save') {
        const result = await dialog.showSaveDialog(window(), options);
        response = {canceled:result.canceled, filePath:result.filePath || ''};
      } else if (request.operation === 'open' || request.operation === 'directory') {
        options.properties = request.operation === 'directory' ? ['openDirectory','createDirectory'] : ['openFile'];
        const result = await dialog.showOpenDialog(window(), options);
        response = {canceled:result.canceled, filePath:result.filePaths[0] || ''};
      } else throw new Error('Unknown desktop operation');
    } catch (error) { response = {error:error.message}; }
    finally {
      const temporary = responsePath + '.tmp';
      fs.writeFileSync(temporary, JSON.stringify(response), {mode:0o600});
      fs.renameSync(temporary, responsePath);
      fs.rmSync(requestPath, {force:true});
      busy = false;
    }
  }, 80);
  timer.unref();
  return () => clearInterval(timer);
}
module.exports = {createDesktopBridge};
