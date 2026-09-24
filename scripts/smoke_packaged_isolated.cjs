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
const development = process.argv.includes('--development');
(async () => {
  let app;
  const started = Date.now();
  try {
    app = await _electron.launch({
      executablePath: path.join(root, 'dist/electron/win-unpacked', development ? 'StatEdu Studio Dev.exe' : 'StatEdu Studio.exe'),
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
    if (development) {
      assert.equal(await app.evaluate(({app}) => app.getVersion()), fs.readFileSync(path.join(root, 'VERSION_DEV'), 'utf8').trim());
      assert.equal(await window.locator('.navbar-nav a[data-value="analysis_meta"]').count(), 1);
      assert.equal(await window.locator('.navbar-nav a[data-value="One-group repeated-measures ANOVA"]').count(), 1);
      console.log('PASS packaged development version and retained experimental menus');
    }
    if(process.argv.includes('--public')) {
      assert.equal(await app.evaluate(({app})=>app.getVersion()),fs.readFileSync(path.join(root,'VERSION'),'utf8').trim());
      for(const language of ['en','ja','zh','es','fr','de','vi','ko']) {
        await window.locator('.navbar-nav a[data-value="about_preferences"]').evaluate(e=>e.click());
        await window.locator('#app_language').selectOption(language);
        await window.waitForTimeout(500);
        assert.equal(await window.locator('.navbar-nav a[data-value="analysis_meta"]').count(),0);
        assert.equal(await window.locator('.navbar-nav a[data-value="One-group repeated-measures ANOVA"]').count(),0);
        assert.equal(await window.locator('.navbar-nav a[data-value="Repeated-measures ANOVA"]').count(),1);
        assert.equal(await window.locator('.navbar-nav a[data-value="Paired test"]').count(),1);
        assert.equal(await window.evaluate(()=>Shiny.shinyapp.$socket.readyState),1);
        console.log('PASS packaged public menu exclusions:',language);
      }
    }
    await window.locator('body').screenshot({ path: path.join(output, restoreArg?'restored-startup.png':'startup.png') });
    const readyMs = Date.now() - started;
    const analyses = [];
    let restored = null;
    if(restoreArg) {
      const saved=JSON.parse(fs.readFileSync(path.join(output,'saved-results.json'),'utf8').replace(/^\uFEFF/,''));
      const expected=saved.entries.map(entry=>({id:entry.id,html:entry.html}));
      assert.equal(expected.length,3);
      await window.locator('a[data-value="result"]').evaluate(element=>element.click());
      await window.locator('#open_result_history_dialog').waitFor({state:'visible'});
      await window.waitForFunction(count=>document.querySelectorAll('#saved_results_list .saved-result-entry').length===count,expected.length,{timeout:30000});
      const actual=await window.locator('#saved_results_list .saved-result-entry').evaluateAll(nodes=>nodes.map(node=>({id:node.dataset.resultEntryId,html:node.querySelector('iframe').getAttribute('srcdoc')})));
      assert.deepStrictEqual(actual,expected);
      restored={automaticRestore:true,persistedEntries:expected.length,orderAndHtmlIdentical:true};
    }
    if (process.argv.includes('--analysis') || exportsRequested) {
      const csv = path.join(output, 'synthetic.csv');
      fs.writeFileSync(csv, 'y,x,z\n' + Array.from({length:80}, (_,i) => {
        const x=Math.sin(i*1.73), z=Math.cos(i*.91), y=3*x-z+Math.sin(i*2.17)*.6;
        return `${y},${x},${z}`;
      }).join('\n'));
      await window.locator('#file').setInputFiles(csv);
      await window.locator('#apply_all_variable_selection').waitFor({state:'visible',timeout:30000});
      await window.locator('#apply_all_variable_selection').click();
      const menu = window.locator('a[data-value="analysis_penalized"]');
      await menu.evaluate(element => element.click());
      const prefix='penalized_regularized';
      await window.locator(`#${prefix}_setup`).waitFor({state:'visible',timeout:30000});
      for(const [variable,role] of [['y','y'],['x','x'],['z','x']]) {
        await window.locator(`[data-input-id="${prefix}_available"] [data-value="${variable}"]`).click();
        await window.locator(`#${prefix}_move_${role}`).click();
        await window.locator(`[data-input-id="${prefix}_${role}"] [data-value="${variable}"]`).waitFor();
      }
      await window.locator(`#${prefix}_bootstrap`).fill('2');
      await window.locator(`#${prefix}_validation_repeats`).fill('2');
      for (const [method,title] of [['ridge','릿지 회귀'],['lasso','라소 회귀'],['elastic_net','엘라스틱넷 회귀']]) {
        await window.locator(`input[name="${prefix}_method"][value="${method}"]`).check();
        if (method!=='ridge') {
          await window.locator(`#${prefix}_post_selection`).check();
          await window.locator(`#${prefix}_inference_splits`).fill('20');
        }
        const start=Date.now();
        await window.locator(`#run_${prefix}`).click();
        await window.locator(`#${prefix}_results h3`).filter({hasText:title}).waitFor({timeout:90000});
        await window.locator(`#${prefix}_results .penalized-validation-variability`).waitFor();
        await window.waitForFunction(() => Array.from(document.querySelectorAll('#penalized_regularized_results img')).length===2 && Array.from(document.querySelectorAll('#penalized_regularized_results img')).every(i=>i.complete&&i.naturalWidth>0));
        const result = window.locator(`#${prefix}_results`);
        const html=await result.innerHTML();
        assert(html.includes('Nested CV RMSE'));
        if(method!=='ridge')assert(html.includes('Table 4. Post-selection inference'));
        fs.writeFileSync(path.join(output,`${method}.html`),html);
        await result.locator('.penalized-publication-summary-table').screenshot({path:path.join(output,`${method}.png`)});
        analyses.push({method,elapsedMs:Date.now()-start});
        if(exportsRequested) {
          await window.locator(`#add_${prefix}_result`).click();
          await window.waitForFunction(count=>document.querySelectorAll('#saved_results_list .saved-result-entry').length===count,analyses.length,{timeout:30000});
          const frames=window.locator('#saved_results_list .saved-result-frame');
          const captured=await frames.last().getAttribute('srcdoc');
          assert(captured.includes('data:image/png;base64,'));
          assert(captured.includes('Nested CV RMSE'));
          fs.writeFileSync(path.join(output,`${method}-saved.html`),captured);
          if(method!=='elastic_net')await menu.evaluate(element=>element.click());
        }
      }
    }
    let management = null;
    if(manage) {
      const entries=window.locator('#saved_results_list .saved-result-entry');
      const read=()=>entries.evaluateAll(nodes=>nodes.map(node=>({id:node.dataset.resultEntryId,html:node.querySelector('iframe').getAttribute('srcdoc')})));
      const initial=await read();assert.equal(initial.length,3);
      const waitOrder=ids=>window.waitForFunction(ids=>JSON.stringify(Array.from(document.querySelectorAll('#saved_results_list .saved-result-entry')).map(n=>n.dataset.resultEntryId))===JSON.stringify(ids),ids);
      await entries.first().locator('.saved-result-entry-down').click();
      const reordered=[initial[1],initial[0],initial[2]];
      await waitOrder(reordered.map(e=>e.id));assert.deepStrictEqual(await read(),reordered);
      await entries.nth(1).locator('.saved-result-entry-delete').click();
      await waitOrder([initial[1].id,initial[2].id]);assert.deepStrictEqual(await read(),[initial[1],initial[2]]);
      await window.locator('#undo_saved_result_edit').click();
      await waitOrder(reordered.map(e=>e.id));assert.deepStrictEqual(await read(),reordered);
      await entries.nth(1).locator('.saved-result-entry-up').click();
      await waitOrder(initial.map(e=>e.id));assert.deepStrictEqual(await read(),initial);
      const persisted=JSON.parse(fs.readFileSync(path.join(output,'saved-results.json'),'utf8').replace(/^\uFEFF/,''));
      fs.writeFileSync(path.join(output,'management-store.json'),JSON.stringify(persisted,null,2));
      management={move:true,deleteWholeEntry:true,undo:true,originalOrderRestored:true,allSnapshotHtmlUnchanged:true};
    }
    const pid = app.process().pid;
    const closed = app.waitForEvent('close', { timeout: 20000 });
    // Exercise the window's close action, as the user would with the X button.
    await app.evaluate(({ BrowserWindow }) => {
      setTimeout(() => BrowserWindow.getAllWindows().forEach(window => window.close()), 0);
    });
    await closed; app = null;
    const log = path.join(dataDir, 'logs/startup.log');
    let text = '';
    for (let i=0;i<40;i++) {
      text = fs.existsSync(log) ? fs.readFileSync(log,'utf8') : '';
      if (text.includes('R process exited')) break;
      await new Promise(resolve => setTimeout(resolve,250));
    }
    assert(text.includes('R process exited'), 'No bundled R shutdown confirmation');
    const result = { status:'passed', executable:'dist/electron/win-unpacked/' + (development ? 'StatEdu Studio Dev.exe' : 'StatEdu Studio.exe'), isolatedUserData:dataDir, readyMs, analyses, management, restored, pid, shutdownConfirmed:true, output };
    fs.writeFileSync(path.join(output,restoreArg?'restore-result.json':'result.json'),JSON.stringify(result,null,2));
    console.log(JSON.stringify(result));
  } finally { if (app) {
    const closed=app.waitForEvent('close',{timeout:20000});
    await app.evaluate(({ BrowserWindow }) => setTimeout(()=>BrowserWindow.getAllWindows().forEach(w=>w.close()),0)).catch(()=>{});
    await closed;
  } }
})().catch(error => { console.error(error); process.exitCode=1; });
