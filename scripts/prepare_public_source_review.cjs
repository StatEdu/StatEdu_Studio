/* Extract a local, review-only snapshot from the distributed Windows package. */
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const { createRequire } = require('node:module');
const root = path.resolve(__dirname, '..');
const req = createRequire(path.join(root, 'packaging/electron/package.json'));
const asar = req('@electron/asar');
const bundled = path.join(root, 'dist/electron/win-unpacked/resources');
const app = path.join(bundled, 'app.asar.unpacked/app');
const out = path.join(root, 'output/microsoft-store/StatEdu_Studio_1.3.0_source-review');
const sha = data => crypto.createHash('sha256').update(data).digest('hex').toUpperCase();
const installer = fs.readFileSync(path.join(root, 'dist/electron/StatEdu_Studio_Setup_1.3.0.exe'));
const expected = 'DEFE844B8A46CCD416553322818FE724441A81681C26FAA9C4EB02A2092C5E10';
if (sha(installer) !== expected) throw new Error('Installer changed; review release provenance first.');
if (fs.readFileSync(path.join(app, 'VERSION'), 'utf8').trim() !== '1.3.0') throw new Error('Unexpected version');
if (fs.existsSync(out)) throw new Error('Review directory already exists; preserve it and choose a new reviewed destination.');
const records = [];
const alerts = [];
function put(relative, bytes, origin) {
  const name = relative.replaceAll('\\', '/');
  if (/(^|\/)(\.git|\.env|node_modules)(\/|$)/i.test(name) || /^app\/(runtime|output)\//i.test(name)) throw new Error(`Unexpected path: ${name}`);
  const target = path.join(out, relative);
  fs.mkdirSync(path.dirname(target), { recursive: true });
  fs.writeFileSync(target, bytes);
  if (sha(fs.readFileSync(target)) !== sha(bytes)) throw new Error(`Copy mismatch: ${name}`);
  records.push({ path: name, bytes: bytes.length, sha256: sha(bytes), origin });
  if (/\.(R|js|cjs|ps1|py|json|csv|md|txt|ya?ml|html)$/i.test(name)) {
    const text = bytes.toString('utf8');
    if (/-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----|ghp_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{40,}|AKIA[A-Z0-9]{16}|sk-proj-[A-Za-z0-9_-]{30,}/.test(text)) alerts.push({ path: name, reason: 'credential-pattern-review-required' });
  }
}
function walk(dir, prefix = '') {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true }).sort((a,b)=>a.name.localeCompare(b.name))) {
    const rel = path.join(prefix, entry.name);
    if (entry.isSymbolicLink()) throw new Error(`Unexpected symlink: ${rel}`);
    if (entry.isDirectory()) walk(path.join(dir, entry.name), rel);
    else put(path.join('app', rel), fs.readFileSync(path.join(dir, entry.name)), 'installed-app');
  }
}
walk(app);
for (const file of ['main.js', 'preload.js', 'package.json']) {
  put(path.join('electron', file), asar.extractFile(path.join(bundled, 'app.asar'), file), 'installed-asar');
}
put('electron/package-lock.json', fs.readFileSync(path.join(root, 'packaging/electron/package-lock.json')), 'supplemental-working-tree');
for (const file of fs.readdirSync(path.join(root, 'packaging/electron/build')).filter(x=>/\.ico$/.test(x))) {
  put(path.join('electron/build', file), fs.readFileSync(path.join(root, 'packaging/electron/build', file)), 'supplemental-working-tree');
}
const readme = `# StatEdu Studio 1.3.0 source review candidate\n\nLOCAL REVIEW ONLY — not uploaded, not a complete corresponding-source or reproducible-build certification.\n\n## Provenance\n\nInstaller SHA-256: ${expected}\nPublic release: https://github.com/StatEdu/StatEdu_Studio/releases/tag/v1.3.0\n\nThe app/ directory preserves every file from the packaged app.asar.unpacked/app directory without edits. electron/main.js, preload.js and package.json are extracted from app.asar. The lockfile and icons are supplemental working-tree files, identified separately in MANIFEST.json; they were not extracted from the installer. R binaries, Electron binaries and node_modules are omitted. Their versions and upstream references are in app/license_report.csv and app/THIRD-PARTY-NOTICES.txt.\n\n## Source inspection and execution\n\nStart source review with app/VERSION, app/app.R and app/run_app.R. The shipped README describes source execution. An R 4.5.3 environment and the listed packages are prerequisites. Read the scripts before running; startup may install missing R packages unless STATEDU_NO_PACKAGE_INSTALL=true. To preserve the public edition set STATEDU_PUBLIC_RELEASE=1. No execution or clean build from this archive has been verified.\n\nFor Electron packaging, the expected layout is electron/app containing the app/ tree and electron/runtime/R-4.5.3 containing the separately prepared runtime. The shipped package.json identifies the Electron and builder versions. Its output path is inherited from the original repository; review it before invoking packaging. The original app/scripts build scripts expect the full repository layout, additional validation scripts and private SmartPLS comparison evidence; they are retained as shipped and must not be represented as a standalone successful build recipe.\n\n## Required before public release\n\n- Reconcile this extracted snapshot with preferred editable source and verify all build inputs, including missing validation scripts and any native components.\n- Establish reproducible source/runtime preparation without publishing private benchmark evidence. Keep release validation enabled.\n- Review supplemental lockfile/icons and bundled sample/fixture permissions.\n- Review all content for secrets and personal data. Automated patterns are limited and are not a full privacy review.\n- Resolve third-party source availability for exact distributed versions.\n- Correct the source offer: app/SOURCE-OFFER.txt is deliberately retained byte-for-byte and still refers to the inaccessible _dev address. Do not publish it as a corrected notice.\n- The existing GitHub v1.3.0 tag points at an older 1.2.0 commit. This archive does not move tags or fix GitHub assets.\n\nContact for review: Stat@statedu.com\n`;
fs.writeFileSync(path.join(out, 'REVIEW-README.md'), readme);
fs.writeFileSync(path.join(out, 'MANIFEST.json'), JSON.stringify({ version:'1.3.0', installerSha256:expected, records }, null, 2));
fs.writeFileSync(path.join(out, 'REVIEW-CHECKS.json'), JSON.stringify({ copiedFiles:records.length, installedAppFiles:records.filter(x=>x.origin==='installed-app').length, credentialPatternAlerts:alerts, scanLimit:'Selected text extensions and high-confidence credential patterns only', cleanBuildVerified:false, readyToPublish:false }, null, 2));
console.log(JSON.stringify({ directory:out, files:records.length, credentialPatternAlerts:alerts.length, readyToPublish:false }, null, 2));
if (alerts.length) process.exitCode = 2;
