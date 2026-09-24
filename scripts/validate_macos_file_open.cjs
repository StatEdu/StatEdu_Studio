const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const source = fs.readFileSync(path.join(__dirname, '../packaging/macos/main.js'), 'utf8');
const functions = source.slice(source.indexOf('async function reloadStudioFile('), source.indexOf('app.on("open-file"'));
const noop = () => {};
function setup() {
  const starts = [], loads = [], errors = [];
  let stops = 0;
  const context = {
    mainWindow: null, isReloadingStudioFile: false, pendingStudioFile: '', launchStudioFile: '', isQuitting: false,
    normalizeStudioFileArg: file => file.endsWith('.studio') ? file : '',
    logStartup: noop, logStartupEnvironment: noop, appDisplayName: () => 'Studio',
    windowTitle: () => 'Studio', configureDownloadSavePath: noop, installRendererDiagnostics: noop,
    focusMainWindow: noop, stopShiny: () => { stops++; },
    dialog: {showErrorBox: (_, text) => errors.push(text)},
    app: {setName: noop, quit: () => { context.isQuitting = true; }},
    path, __dirname, shell: {openExternal: noop}, setTimeout,
    formatStartupError: e => e.message,
    startShiny: () => new Promise((resolve, reject) => starts.push({file: context.launchStudioFile, resolve, reject})),
    BrowserWindow: class {
      constructor() { this.webContents = {setWindowOpenHandler: noop, on: noop}; }
      on() {}
      async loadURL(url) { loads.push(url); }
    }
  };
  vm.createContext(context);
  vm.runInContext(functions, context);
  return {context, starts, loads, errors, stops: () => stops};
}
const tick = () => new Promise(resolve => setImmediate(resolve));
(async () => {
  const t = setup(), c = t.context;
  await c.reloadStudioFile('/한글 space/early.studio');
  assert.equal(c.launchStudioFile, '/한글 space/early.studio');
  assert.equal(t.starts.length, 0);
  const creation = c.createWindow();
  assert.equal(t.starts.length, 1);
  await c.reloadStudioFile('/during-start.studio');
  assert.equal(t.starts.length, 1, 'initial startup must not overlap reload');
  t.starts[0].resolve('initial');
  await tick();
  assert.equal(t.starts.length, 2);
  assert.equal(t.starts[1].file, '/during-start.studio');
  t.starts[1].resolve('queued');
  await creation;
  assert.equal(c.isReloadingStudioFile, false);
  const opening = c.reloadStudioFile('/A.studio');
  await tick();
  await c.reloadStudioFile('/B.studio');
  await c.reloadStudioFile('/C.studio');
  assert.equal(t.starts.length, 3);
  t.starts[2].resolve('A');
  await tick();
  assert.equal(t.starts[3].file, '/C.studio');
  t.starts[3].resolve('C');
  await opening;
  assert.equal(t.loads.at(-1), 'C');
  assert.ok(!t.loads.includes('A'));
  const failed = c.reloadStudioFile('/failure.studio');
  await tick();
  t.starts[4].reject(Error('startup failed'));
  await failed;
  assert.equal(c.isReloadingStudioFile, false);
  assert.deepEqual(t.errors, ['startup failed']);
  await c.reloadStudioFile('/ignored.txt');
  c.isQuitting = true;
  await c.reloadStudioFile('/after-quit.studio');
  assert.equal(t.starts.length, 5);
  // A late exit from the previous R must not clear the current process.
  const old = {}, current = {};
  const exitContext = {shinyProcess: current, startedProcess: old, code: 0, signal: null, logStartup: noop};
  const handler = source.match(/shinyProcess\.on\("exit", \(code, signal\) => \{([\s\S]*?)\n  \}\);/)[1];
  vm.runInNewContext(handler, exitContext);
  assert.equal(exitContext.shinyProcess, current);
  exitContext.startedProcess = current;
  vm.runInNewContext(handler, exitContext);
  assert.equal(exitContext.shinyProcess, null);
  const pkg = JSON.parse(fs.readFileSync(path.join(__dirname, '../packaging/macos/package.json')));
  assert.equal(pkg.build.mac.fileAssociations[0].ext, 'studio');
  assert.equal(pkg.build.mac.fileAssociations[0].rank, 'Alternate');
  console.log('PASS: early/startup/rapid file requests, failure recovery, quit, stale R exit and Mac association');
})().catch(error => { console.error(error); process.exitCode = 1; });
