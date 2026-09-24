const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
for (const [folder, platform, api, root, expected] of [
  ['electron', 'win32', path.win32, 'C:\\Studio', 'C:\\Studio\\runtime\\R-4.5.3\\bin\\x64\\Rscript.exe'],
  ['macos', 'darwin', path.posix, '/Applications/StatEdu Studio.app/Contents/Resources/app.asar.unpacked', '/Applications/StatEdu Studio.app/Contents/Resources/app.asar.unpacked/runtime/R.framework/Resources/bin/Rscript']
]) {
  const source = fs.readFileSync(path.join(__dirname, `../packaging/${folder}/main.js`), 'utf8');
  const start = platform === 'win32' ? 'function bundledRscriptPath()' : 'function bundledRHomePath()';
  const functions = source.slice(source.indexOf(start), source.indexOf('function shinyStartupTimeoutMs()'));
  const context = {process: {platform}, path: api, appBaseDir: () => root};
  vm.createContext(context);
  vm.runInContext(functions, context);
  assert.equal(context.bundledRscriptPath(), expected);
  assert.equal(context.bundledRLibraryPath(), platform === 'win32'
    ? api.join(root, 'runtime', 'R-4.5.3', 'library')
    : api.join(context.bundledRHomePath(), 'library'));
  if (platform === 'win32') {
    assert.ok(!source.includes('R.framework'));
    assert.ok(!source.includes('bundledRHomePath'));
    assert.match(source, /PATH: `\$\{bundledRBinPath\(\)\};/);
  } else {
    assert.ok(!source.includes('Rscript.exe'));
    assert.match(source, /PATH: `\$\{bundledRBinPath\(\)\}\$\{path.delimiter\}/);
  }
}
console.log('PASS: independent Windows and macOS launchers, R paths and PATH separators');
