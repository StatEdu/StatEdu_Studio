const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const source = fs.readFileSync(path.join(__dirname, '../packaging/macos/main.js'), 'utf8');
const functions = source.slice(source.indexOf('function macREnvironment()'), source.indexOf('function formatStartupError('));
const home = '/Applications/한글 Studio.app/Contents/Resources/app.asar.unpacked/runtime/R.framework/Resources';
const inherited = {
  R_HOME: '/external/R', R_LIBS: '/external/packages', R_LIBS_SITE: '/external/site',
  R_LIBS_USER: '/external/user', R_PROFILE_USER: '/private/.Rprofile', R_ENVIRON: '/private/Renviron',
  R_SHARE_DIR: '/old/share', R_ARCH: '/x86_64', DYLD_LIBRARY_PATH: '/opt/R',
  DYLD_INSERT_LIBRARIES: '/external/inject', LD_LIBRARY_PATH: '/external', LD_PRELOAD: '/external/lib',
  PATH: '/opt/homebrew/bin:/external/bin', HOME: '/Users/한글 user', LANG: 'ko_KR.UTF-8',
  TMPDIR: '/tmp/app', STATEDU_CHROME: '/custom/Chrome', STATEDU_APP_LANGUAGE: 'ko'
};
const original = {...inherited};
let probe;
const context = {
  process: {env: inherited}, path: path.posix,
  bundledRHomePath: () => home, bundledRLibraryPath: () => home + '/library', bundledRBinPath: () => home + '/bin',
  spawnSync: (exe, args, options) => { probe = options; return {status: 0, stdout: 'R version'}; }, logStartup: () => {}
};
vm.createContext(context);
vm.runInContext(functions, context);
const env = context.macREnvironment();
assert.deepEqual(inherited, original, 'caller environment is unchanged');
for (const name of ['R_PROFILE_USER', 'R_ENVIRON', 'R_ARCH', 'DYLD_LIBRARY_PATH', 'DYLD_INSERT_LIBRARIES', 'LD_LIBRARY_PATH', 'LD_PRELOAD']) {
  assert.ok(!(name in env), name);
}
for (const name of ['R_LIBS', 'R_LIBS_USER', 'R_LIBS_SITE']) assert.equal(env[name], home + '/library');
for (const [key, directory] of [['R_SHARE_DIR', 'share'], ['R_INCLUDE_DIR', 'include'], ['R_DOC_DIR', 'doc']]) {
  assert.equal(env[key], home + '/' + directory);
}
for (const key of ['HOME', 'LANG', 'TMPDIR', 'STATEDU_CHROME', 'STATEDU_APP_LANGUAGE']) assert.equal(env[key], inherited[key]);
assert.equal(env.PATH, home + '/bin:/usr/bin:/bin:/usr/sbin:/sbin');
context.runRscriptProbe(home + '/bin/Rscript', '/app');
assert.equal(probe.env.R_LIBS_SITE, home + '/library');
assert.match(source, /spawn\(rscript, \["--vanilla", "run_app.R"\]/);
assert.match(source, /const env = \{\s*\.\.\.macREnvironment\(\),/);
console.log('PASS: Mac R library/startup/loader isolation, probe parity, caller and app settings preserved');
