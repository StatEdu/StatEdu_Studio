'use strict';
const fs = require('node:fs');
const path = require('node:path');
const {execFileSync} = require('node:child_process');
const {downloadArtifact} = require('@electron/get');

// electron-builder extracts only Electron.app from the vendor archive. Preserve
// the accompanying Electron and Chromium notices inside the distributed app.
module.exports = async context => {
  const version = JSON.parse(fs.readFileSync(path.join(context.packager.projectDir, 'package.json'))).devDependencies.electron;
  const archive = await downloadArtifact({version, artifactName:'electron', platform:'darwin', arch:'arm64'});
  const resources = path.join(context.appOutDir, context.packager.appInfo.productFilename+'.app', 'Contents/Resources');
  for (const [entry, name] of [['LICENSE','LICENSE.electron.txt'], ['LICENSES.chromium.html','LICENSES.chromium.html']]) {
    const bytes = execFileSync('/usr/bin/unzip', ['-p',archive,entry], {maxBuffer:32*1024*1024});
    if (!bytes.length) throw new Error('Missing vendor notice: '+entry);
    fs.writeFileSync(path.join(resources,name),bytes);
  }
};
