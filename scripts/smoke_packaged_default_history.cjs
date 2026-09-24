const { _electron } = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
const fs = require('fs');
const path = require('path');
const assert = require('node:assert/strict');
const root = path.resolve(__dirname, '..');
const output = fs.mkdtempSync(path.join(root, 'tmp', 'default-history-'));
const profile = path.join(output, 'profile');
const history = path.join(profile, 'data', 'StatEdu_Studio_results.json');
const entries = [{id:'upgrade-preserved', title:'Upgrade preservation', saved_at:'2026-09-15', html:'<!doctype html><html><body><p>Stored estimate: 0.123</p></body></html>'}];
fs.mkdirSync(path.dirname(history), {recursive:true});
fs.writeFileSync(history, JSON.stringify({type:'easyflow_result_history',version:1,entries}));
const original = fs.readFileSync(history);
const env = {...process.env, APPDATA:path.join(output,'appdata'), LOCALAPPDATA:path.join(output,'localappdata')};
delete env.STATEDU_RESULT_STORE;
delete env.STATEDU_USER_DATA_DIR;
let app;
(async () => {
  try {
    app = await _electron.launch({executablePath:path.join(root,'dist/electron/win-unpacked/StatEdu Studio.exe'),args:[`--user-data-dir=${profile}`],env,timeout:120000});
    const actualProfile = await app.evaluate(({app}) => app.getPath('userData'));
    assert.equal(path.resolve(actualProfile).toLowerCase(), profile.toLowerCase());
    const win = await app.firstWindow();
    win.on('dialog', dialog=>dialog.accept().catch(()=>{}));
    await win.waitForURL(/^http:\/\/127\.0\.0\.1:/,{timeout:120000});
    await win.waitForFunction(()=>window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState===1,null,{timeout:120000});
    await win.locator('a[data-value="result"]').evaluate(e=>e.click());
    await win.waitForFunction(()=>document.querySelectorAll('#saved_results_list .saved-result-entry').length===1,null,{timeout:60000});
    const actual = await win.locator('#saved_results_list .saved-result-entry').evaluateAll(nodes=>nodes.map(n=>({id:n.dataset.resultEntryId,html:n.querySelector('iframe').getAttribute('srcdoc')})));
    assert.deepEqual(actual,entries.map(({id,html})=>({id,html})));
    assert.deepEqual(fs.readFileSync(history),original);
    const closed=app.waitForEvent('close',{timeout:20000});
    await app.evaluate(({BrowserWindow})=>setTimeout(()=>BrowserWindow.getAllWindows().forEach(w=>w.close()),0));
    await closed; app=null;
    const result={passed:true,explicitStoreOverride:false,profile,history,restoredIdAndHtmlExact:true,historyBytesUnchanged:true};
    fs.writeFileSync(path.join(output,'result.json'),JSON.stringify(result,null,2));
    console.log(JSON.stringify(result));
  } finally { if(app) await app.close(); }
})().catch(e=>{console.error(e);process.exitCode=1;});
