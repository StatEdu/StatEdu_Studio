const { _electron } = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
const fs = require('fs');
const path = require('path');
const assert = require('assert');
const root = path.resolve(__dirname, '..');
const restoreArg=process.argv.find(value=>value.startsWith('--restore='));
const output = restoreArg ? path.resolve(restoreArg.slice('--restore='.length)) : fs.mkdtempSync(path.join(root, 'tmp', 'packaged-smoke-'));
assert(output.toLowerCase().startsWith(path.join(root,'tmp','packaged-smoke-').toLowerCase()),'Only isolated smoke profiles may be restored');
const profile = path.join(output, 'profile');
const manage = process.argv.includes('--manage');
const exportsRequested = process.argv.includes('--exports') || manage;
(async () => {
  let app;
  const started = Date.now();
  try {
    app = await _electron.launch({
      executablePath: path.join(root, 'dist/electron/win-unpacked/StatEdu Studio.exe'),
      args: [`--user-data-dir=${profile}`],
      env: { ...process.env, APPDATA: path.join(output, 'appdata'), LOCALAPPDATA: path.join(output, 'localappdata'), STATEDU_APP_LANGUAGE: 'ko', STATEDU_RESULT_STORE:path.join(output,'saved-results.json') },
      timeout: 120000
    });
    const dataDir = await app.evaluate(({ app }) => app.getPath('userData'));
    assert(path.resolve(dataDir).toLowerCase().startsWith(output.toLowerCase() + path.sep), `Profile was not isolated: ${dataDir}`);
    const window = await app.firstWindow();
    window.on('dialog', dialog => dialog.accept().catch(() => {}));
    await window.waitForURL(/^http:\/\/127\.0\.0\.1:/, { timeout: 120000 });
    await window.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState === 1, null, { timeout: 120000 });
    await window.locator('#data_steps input[type="file"]').waitFor({state:'attached',timeout:120000});
    await window.waitForFunction(() => !document.querySelector('#data_steps.recalculating'), null, {timeout:120000});
    await window.locator('body').screenshot({ path: path.join(output, restoreArg?'restored-startup.png':'startup.png') });
    const readyMs = Date.now() - started;
    const errors=[];window.on('pageerror',e=>errors.push(e.message));
    await window.locator('#file').setInputFiles(path.join(root,'scripts/fixtures/survival_validation.csv'));
    await window.locator('#apply_all_variable_selection').waitFor({state:'visible'});
    await window.locator('#apply_all_variable_selection').click();
    const open = async value => {
      await window.locator(`a[data-value="${value}"]`).evaluate(el=>el.click());
    };
    const move = async (prefix,variable,role) => {
      await window.locator(`[data-input-id="${prefix}_available"] [data-value="${variable}"]`).click();
      await window.locator(`#${prefix}_${role}_move`).click();
      await window.locator(`[data-input-id="${prefix}_${role}"] [data-value="${variable}"]`).waitFor();
    };
    const records=[];
    const capture = async (id,run,add,required) => {
      const t=Date.now();await window.locator(`#${run}`).click();
      await window.locator(`#${id} table`).first().waitFor({timeout:90000});
      const plots=window.locator(`#${id} .shiny-plot-output`);
      const plotCount=await plots.count();
      for(let i=0;i<plotCount;i++) {
        const plot=plots.nth(i);
        await plot.scrollIntoViewIfNeeded();
        await plot.locator('img').waitFor({state:'attached',timeout:90000});
        await window.waitForFunction(plotId=>{
          const e=document.getElementById(plotId),img=e.querySelector('img');
          return !e.classList.contains('recalculating')&&img&&img.complete&&img.naturalWidth>0;
        },await plot.getAttribute('id'),{timeout:90000});
        await plot.screenshot({path:path.join(output,`${id}-plot-${i+1}.png`)});
      }
      await window.waitForFunction(id=>{
        const e=document.getElementById(id);
        return !e.classList.contains('recalculating')&&[...e.querySelectorAll('img')].every(i=>i.complete&&i.naturalWidth>0);
      },id,{timeout:90000});
      const result=window.locator(`#${id}`),text=await result.innerText();
      for(const name of required)assert(text.includes(name),`${id} missing ${name}`);
      assert(!await result.locator('.shiny-output-error').count());
      fs.writeFileSync(path.join(output,`${id}.html`),await result.innerHTML());
      await result.screenshot({path:path.join(output,`${id}.png`)});
      const tables=await result.locator('table').count();
      await window.locator(`#${add}`).click();
      await window.waitForFunction(n=>document.querySelectorAll('#saved_results_list .saved-result-entry').length===n,records.length+1);
      const saved=await window.locator('#saved_results_list .saved-result-frame').last().getAttribute('srcdoc');
      for(const name of required)assert(saved.includes(name));
      fs.writeFileSync(path.join(output,`${id}-saved.html`),saved);
      records.push({id,tables,plots:plotCount,elapsedMs:Date.now()-t});
    };
    await open('Correlation');
    for(const variable of ['time','age']) {
      await window.locator(`[data-input-id="correlation_available"] [data-value="${variable}"]`).click();
      await window.locator('#correlation_move').click();
      await window.locator(`[data-input-id="correlation_selected"] [data-value="${variable}"]`).waitFor();
    }
    await capture('correlation_results','run_correlation','add_correlation_result',['time','age']);
    await open('analysis_survival_km');
    await move('survival_km','time','time');
    await move('survival_km','status','event');
    await move('survival_km','sex','group');
    await window.locator('#survival_km_event_value').fill('1');
    await capture('survival_km_results','run_survival_km','add_survival_km_result',['time','status']);
    await open('analysis_survival_cox');
    await move('survival_cox','time','time');
    await move('survival_cox','status','event');
    await move('survival_cox','age','covariates');
    await window.locator('#survival_cox_event_value').fill('1');
    await capture('survival_cox_results','run_survival_cox','add_survival_cox_result',['age']);
    assert.deepStrictEqual(errors,[]);
    const closed=app.waitForEvent('close',{timeout:20000});
    await app.evaluate(({BrowserWindow})=>setTimeout(()=>BrowserWindow.getAllWindows().forEach(w=>w.close()),0));
    await closed;app=null;
    const log=path.join(dataDir,'logs/startup.log');
    for(let i=0;i<40&&!fs.readFileSync(log,'utf8').includes('R process exited');i++)await new Promise(r=>setTimeout(r,250));
    assert(fs.readFileSync(log,'utf8').includes('R process exited'));
    const result={status:'passed',readyMs,records,errors,shutdownConfirmed:true,output};
    fs.writeFileSync(path.join(output,'result.json'),JSON.stringify(result,null,2));console.log(JSON.stringify(result));
  } catch(error) {
    if(app) { const w=await app.firstWindow();await w.screenshot({path:path.join(output,'failure.png')}).catch(()=>{});fs.writeFileSync(path.join(output,'failure.html'),await w.content().catch(()=>'')); }
    throw error;
  } finally {
    if(app) { const closed=app.waitForEvent('close',{timeout:20000});await app.evaluate(({BrowserWindow})=>setTimeout(()=>BrowserWindow.getAllWindows().forEach(w=>w.close()),0)).catch(()=>{});await closed; }
  }
})().catch(error=>{console.error(error);process.exitCode=1;});
