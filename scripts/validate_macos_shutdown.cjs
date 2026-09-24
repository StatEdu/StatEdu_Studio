const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const {EventEmitter} = require('node:events');
const source = fs.readFileSync(path.join(__dirname, '../packaging/macos/main.js'), 'utf8');
const helper = source.slice(source.indexOf('function stopShiny()'), source.indexOf('function focusMainWindow()'));
function setup() {
  const child = new EventEmitter();
  child.pid = 123;
  child.exitCode = null;
  child.signalCode = null;
  child.killed = true; // Sent a signal earlier; this does not mean it exited.
  const signals = [], timers = new Map();
  let nextTimer = 0;
  child.kill = signal => { signals.push(signal); return true; };
  const context = {
    shinyProcess: child, stoppingShiny: null, logStartup: () => {},
    setTimeout: callback => { timers.set(++nextTimer, callback); return nextTimer; },
    clearTimeout: id => timers.delete(id)
  };
  vm.createContext(context);
  vm.runInContext(helper, context);
  const fire = () => { const [id, fn] = timers.entries().next().value; timers.delete(id); fn(); };
  return {child, signals, timers, context, fire};
}
const tick = () => new Promise(resolve => setImmediate(resolve));
(async () => {
  let t = setup();
  let promise = t.context.stopShiny();
  assert.equal(t.context.stopShiny(), promise, 'concurrent callers share one stop');
  assert.deepEqual(t.signals, ['SIGTERM']);
  assert.equal(t.context.shinyProcess, t.child, 'retain handle until exit');
  t.child.emit('exit', 0, null);
  await promise;
  assert.equal(t.context.shinyProcess, null);
  assert.equal(t.timers.size, 0);
  t = setup();
  promise = t.context.stopShiny();
  t.fire();
  assert.deepEqual(t.signals, ['SIGTERM', 'SIGKILL']);
  t.child.emit('exit', null, 'SIGKILL');
  await promise;
  assert.equal(t.timers.size, 0);
  t = setup();
  promise = t.context.stopShiny();
  const rejected = assert.rejects(promise, /exit could not be confirmed/);
  t.fire(); t.fire();
  await rejected;
  assert.equal(t.context.shinyProcess, t.child, 'failed stop must preserve process for retry');
  assert.equal(t.context.stoppingShiny, null);
  assert.equal(t.child.listenerCount('exit'), 0);
  t = setup();
  t.child.exitCode = 0;
  await t.context.stopShiny();
  assert.deepEqual(t.signals, []);
  t = setup();
  t.child.kill = () => { throw Error('permission denied'); };
  await assert.rejects(t.context.stopShiny(), /permission denied/);
  assert.equal(t.timers.size, 0);
  // Quitting must remain blocked until R exit is confirmed, and permit retry on failure.
  const handlers = {}, errors = [];
  let resolveStop, rejectStop, quits = 0;
  const context = {shutdownComplete: false, shutdownPending: false, isQuitting: false,
    pendingStudioFile: 'queued.studio', app: {on: (name, fn) => { handlers[name] = fn; }, quit: () => { quits++; }},
    stopShiny: () => new Promise((resolve, reject) => { resolveStop = resolve; rejectStop = reject; }),
    logStartup: () => {}, appDisplayName: () => 'Studio', dialog: {showErrorBox: (_, text) => errors.push(text)}};
  vm.runInNewContext(source.slice(source.indexOf('app.on("before-quit"')), context);
  let prevented = 0;
  const event = {preventDefault: () => { prevented++; }};
  handlers['before-quit'](event);
  assert.equal(quits, 0);
  assert.equal(context.isQuitting, true);
  assert.equal(context.pendingStudioFile, '');
  rejectStop(Error('cannot stop'));
  await tick();
  assert.equal(context.isQuitting, false);
  assert.equal(context.shutdownPending, false);
  assert.deepEqual(errors, ['cannot stop']);
  handlers['before-quit'](event);
  resolveStop();
  await tick();
  assert.equal(context.shutdownComplete, true);
  assert.equal(quits, 1);
  handlers['before-quit'](event);
  assert.equal(prevented, 2);
  console.log('PASS: confirmed exit, TERM/KILL escalation, timeout, retry, deduplication and quit gate');
})().catch(error => { console.error(error); process.exitCode = 1; });
