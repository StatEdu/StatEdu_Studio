'use strict';
// Local sandbox validation uses Developer ID. This artifact is never submitted
// as the App Store distribution package.
const path = require('node:path');
module.exports = async function(app, stage) {
  const {signAsync} = require('@electron/osx-sign');
  await signAsync({
    app, platform:'mas', identity:process.env.STATEDU_MAC_SIGN_IDENTITY,
    identityValidation:false, preAutoEntitlements:false,
    optionsForFile: file => ({
      entitlements:path.join(stage,'build',file===app ? 'entitlements.mas.local.plist' : 'entitlements.mas.inherit.plist'),
      hardenedRuntime:false
    }),
    ignore:file=>/\/Contents\/Resources\/app\.asar\.unpacked\/(app|runtime)(\/|$)/.test(file)
  });
};
if (require.main === module) module.exports(process.argv[2],process.argv[3]).catch(error=>{console.error(error);process.exitCode=1;});
