const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

const source = fs.readFileSync(path.join(__dirname, '../packaging/macos/main.js'), 'utf8');
const helper = source.slice(source.indexOf('function macPdfBrowserPath()'), source.indexOf('async function startShiny()'));
const chrome = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const edge = '/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge';
const userChrome = '/Users/한글 user/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
function detect(entries, override = '') {
  const context = {
    process: {env: {STATEDU_CHROME: override}},
    path: path.posix,
    app: {getPath: name => { assert.equal(name, 'home'); return '/Users/한글 user'; }},
    fs: {
      constants: {X_OK: 1},
      statSync: file => { if (!entries[file]) throw Error('missing'); return {isFile: () => entries[file] !== 'directory'}; },
      accessSync: (file, flag) => { assert.equal(flag, 1); if (entries[file] !== 'executable') throw Error('not executable'); }
    }
  };
  vm.createContext(context);
  vm.runInContext(helper, context);
  return context.macPdfBrowserPath();
}
assert.equal(detect({}, '/custom space/Chrome'), '/custom space/Chrome');
assert.equal(detect({[chrome]: 'executable', [edge]: 'executable'}), chrome);
assert.equal(detect({[chrome]: 'directory', [edge]: 'executable'}), edge);
assert.equal(detect({[chrome]: 'unexecutable', [userChrome]: 'executable'}), userChrome);
assert.equal(detect({[userChrome]: 'executable'}, '  '), userChrome);
assert.equal(detect({}), '');
assert.match(source, /STATEDU_CHROME: macPdfBrowserPath\(\)/);
console.log('PASS: Mac PDF browser override, system/user apps, spaces/Korean, non-executable and missing browser');
