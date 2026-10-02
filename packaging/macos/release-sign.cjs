'use strict';
const fs = require('node:fs');
const path = require('node:path');
const {execFileSync} = require('node:child_process');

// Sign every native R component before the containing Electron bundle is signed.
module.exports = async context => {
  await require('./bundle-notices.cjs')(context);
  const identity = process.env.STATEDU_MAC_SIGN_IDENTITY;
  if (!identity) throw new Error('STATEDU_MAC_SIGN_IDENTITY is required for a signed release');
  const framework = path.join(context.appOutDir, context.packager.appInfo.productFilename+'.app',
    'Contents/Resources/app.asar.unpacked/runtime/R.framework');
  const magic = new Set(['feedface','cefaedfe','feedfacf','cffaedfe','cafebabe','bebafeca','cafebabf','bfbafeca']);
  const binaries = [];
  function walk(directory) {
    for (const entry of fs.readdirSync(directory, {withFileTypes:true})) {
      const file = path.join(directory, entry.name);
      if (entry.isSymbolicLink() || entry.name.endsWith('.dSYM') || entry.name.endsWith('.class')) continue;
      if (entry.isDirectory()) walk(file);
      else if (entry.isFile()) {
        const fd = fs.openSync(file,'r'), header=Buffer.alloc(4);
        try { fs.readSync(fd,header,0,4,0); } finally { fs.closeSync(fd); }
        if (magic.has(header.toString('hex'))) binaries.push(file);
      }
    }
  }
  walk(framework);
  for (const binary of binaries) {
    execFileSync('codesign',['--force','--timestamp','--options','runtime','--sign',identity,binary]);
    execFileSync('codesign',['--verify','--strict',binary]);
  }
};
