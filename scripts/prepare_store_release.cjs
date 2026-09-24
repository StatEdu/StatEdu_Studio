/* Readiness audit and Store configuration preparation; never uploads or installs. */
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const { createRequire } = require('node:module');
const root = path.resolve(__dirname, '..');
const electron = path.join(root, 'packaging/electron');
const localRequire = createRequire(path.join(electron, 'package.json'));
const readJson = p => JSON.parse(fs.readFileSync(p, 'utf8').replace(/^\uFEFF/, ''));
const hash = p => crypto.createHash('sha256').update(fs.readFileSync(p)).digest('hex').toUpperCase();
const output = path.join(root, 'output/microsoft-store');
const args = process.argv.slice(2);
if (args.length && !(args.length === 2 && args[0] === '--identity')) {
  throw new Error('Usage: node scripts/prepare_store_release.cjs [--identity <Partner Center identity JSON>]');
}
async function main() {
  const pkg = readJson(path.join(electron, 'package.json'));
  const version = fs.readFileSync(path.join(root, 'VERSION'), 'utf8').trim();
  const checks = [];
  const add = (name, passed, detail) => checks.push({ name, status: passed ? 'pass' : 'pending', detail });
  add('publicVersion', /^\d+\.\d+\.\d+$/.test(version) && version === pkg.version, version);
  const bundle = path.join(root, 'dist/electron/win-unpacked');
  const unpacked = path.join(bundle, 'resources/app.asar.unpacked');
  const packagedVersion = path.join(unpacked, 'app/VERSION');
  add('packagedPublicVersion', fs.existsSync(packagedVersion) && fs.readFileSync(packagedVersion, 'utf8').trim() === version, packagedVersion);
  const runtime = path.join(unpacked, 'runtime/R-4.5.3/bin/Rscript.exe');
  const runtime64 = path.join(unpacked, 'runtime/R-4.5.3/bin/x64/Rscript.exe');
  add('bundledR', fs.existsSync(runtime) || fs.existsSync(runtime64), 'Bundled R must be tested after Store installation.');
  const verificationFile = path.join(root, 'dist/electron/packaged-source-verification.json');
  const differences = [];
  if (fs.existsSync(verificationFile)) {
    const verification = readJson(verificationFile);
    for (const entry of verification.files || []) {
      for (const [kind, base] of [['source', root], ['packaged', path.join(unpacked, 'app')]]) {
        const file = path.join(base, entry.path);
        if (!fs.existsSync(file) || hash(file) !== entry.sha256.toUpperCase()) differences.push(`${kind}: ${entry.path}`);
      }
    }
    add('verifiedSourceSnapshot', verification.version === version && verification.files?.length > 0 && !differences.length,
      { recordedFileCount: verification.files?.length || 0, differences });
    const installer = path.join(root, 'dist/electron', verification.installer);
    add('existingInstallerHash', fs.existsSync(installer) && hash(installer) === verification.sha256.toUpperCase(), verification.installer);
  } else add('verifiedSourceSnapshot', false, 'Run the existing public release verification first.');
  const asarPath = path.join(bundle, 'resources/app.asar');
  const launcherDifferences = [];
  const launcherLineEndingDifferences = [];
  if (fs.existsSync(asarPath)) {
    const asar = localRequire('@electron/asar');
    for (const name of ['main.js', 'preload.js']) {
      try {
        const packed = asar.extractFile(asarPath, name);
        const source = fs.readFileSync(path.join(electron, name));
        if (!packed.equals(source)) {
          if (packed.toString('utf8').replace(/\r\n/g, '\n') === source.toString('utf8').replace(/\r\n/g, '\n')) {
            launcherLineEndingDifferences.push(name);
          } else launcherDifferences.push(name);
        }
      } catch (error) { launcherDifferences.push(`${name}: ${error.message}`); }
    }
  } else launcherDifferences.push('app.asar missing');
  add('launcherSource', !launcherDifferences.length, { differences: launcherDifferences, lineEndingOnlyDifferences: launcherLineEndingDifferences });
  const builderVersion = localRequire('app-builder-lib/package.json').version;
  add('appxTarget', fs.existsSync(path.join(electron, 'node_modules/app-builder-lib/out/targets/AppxTarget.js')), builderVersion);
  const assets = [['StoreLogo.png',50,50],['Square150x150Logo.png',150,150],['Square44x44Logo.png',44,44],['Wide310x150Logo.png',310,150]];
  for (const [name,w,h] of assets) {
    const file = path.join(electron, 'build/appx', name);
    const data = fs.existsSync(file) ? fs.readFileSync(file) : null;
    add(`asset:${name}`, !!data && data.length >= 24 && data.subarray(0,8).equals(Buffer.from('89504e470d0a1a0a','hex')) && data.readUInt32BE(16) === w && data.readUInt32BE(20) === h, `${w}x${h} PNG required; do not ship default sample logos.`);
  }
  const identity = args.length ? readJson(path.resolve(args[1])) : null;
  const validIdentity = !!identity && /^[A-Za-z0-9.-]{3,50}$/.test(identity.identityName || '') && /^CN=.+/.test(identity.publisher || '') && !!identity.publisherDisplayName?.trim();
  add('partnerCenterIdentity', validIdentity, 'Copy Identity.Name, Identity.Publisher and PublisherDisplayName exactly from Partner Center after name reservation.');
  let configPath = null;
  if (validIdentity) {
    const config = JSON.parse(JSON.stringify(pkg.build));
    config.directories = { ...config.directories, output: path.join(root, 'dist/store'), buildResources: path.join(electron, 'build') };
    config.win = { ...config.win, target: [{ target: 'appx', arch: ['x64'] }], artifactName: 'StatEdu_Studio_Store_${version}_${arch}.${ext}' };
    delete config.nsis;
    config.appx = { applicationId: 'StatEduStudio', identityName: identity.identityName, publisher: identity.publisher,
      publisherDisplayName: identity.publisherDisplayName, displayName: 'StatEdu Studio', languages: ['ko-KR','en-US','ja-JP','zh-CN','de-DE','es-ES','fr-FR','vi-VN'],
      minVersion: '10.0.19041.0', setBuildNumber: false, addAutoLaunchExtension: false, capabilities: ['runFullTrust'] };
    const { validateConfiguration } = localRequire('app-builder-lib/out/util/config/config');
    const { DebugLogger } = localRequire('builder-util');
    await validateConfiguration(config, new DebugLogger(false));
    fs.mkdirSync(output, { recursive: true });
    // A draft filename is intentional: configuration validity is not Store certification.
    configPath = path.join(output, 'electron-builder.store.draft.json');
    fs.writeFileSync(configPath, JSON.stringify(config, null, 2) + '\n');
  }
  const report = { generatedAt: new Date().toISOString(), version, builderVersion, target: 'appx',
    storeReady: false, configPath, checks,
    manualGates: ['Partner Center account verification and app name reservation', 'Branded assets and real public-build screenshots',
      'Installed-package startup, localhost R child process, file associations, exports and update tests',
      'Public privacy policy and exact-version source/license availability', 'Store listing and certification review'] };
  fs.mkdirSync(output, { recursive: true });
  fs.writeFileSync(path.join(output, 'readiness.json'), JSON.stringify(report, null, 2) + '\n');
  console.log(JSON.stringify({ version, checks: checks.map(c => `${c.status}: ${c.name}`), configPath, report: path.join(output,'readiness.json') }, null, 2));
  if (identity && !validIdentity) process.exitCode = 2;
}
main().catch(error => { console.error(error.message); process.exitCode = 1; });
