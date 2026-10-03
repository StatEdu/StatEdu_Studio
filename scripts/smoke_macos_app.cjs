const { _electron } = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
const fs = require('fs');
const path = require('path');
const assert = require('assert');
const root = path.resolve(__dirname, '..');
assert.equal(process.platform,'darwin','Run on a Mac');
const executableArg=process.argv.find(value=>value.startsWith('--executable='));
assert(executableArg,'Supply --executable=/path/to/App.app/Contents/MacOS/App');
const executable=path.resolve(executableArg.slice('--executable='.length));
const output = fs.mkdtempSync(path.join(root, 'tmp', 'packaged-smoke-'));
assert(output.toLowerCase().startsWith(path.join(root,'tmp','packaged-smoke-').toLowerCase()),'Only isolated smoke profiles may be restored');
const profile = path.join(output, 'profile');
const sourceData=path.join(root,'scripts/fixtures/survival_validation.csv');
const documentSpecs=JSON.parse(fs.readFileSync(path.join(root,'docs/i18n/document_specs.json'),'utf8').replace(/^\uFEFF/,''));
(async () => {
  let app;
  const started = Date.now();
  try {
    app = await _electron.launch({
      executablePath: executable,
      args: [`--user-data-dir=${profile}`],
      env: { ...process.env, STATEDU_APP_LANGUAGE: 'ko', STATEDU_RESULT_STORE:path.join(output,'saved-results.json') },
      timeout: 120000
    });
    const dataDir = await app.evaluate(({ app }) => app.getPath('userData'));
    assert(path.resolve(dataDir).toLowerCase().startsWith(output.toLowerCase() + path.sep), `Profile was not isolated: ${dataDir}`);
    await app.evaluate(({dialog},{output,sourceData})=>{
      globalThis.macSmokeDialogs=[];
      dialog.showMessageBoxSync=()=>1;
      dialog.showSaveDialog=async (_window,options)=>{
        globalThis.macSmokeDialogs.push({operation:'save',options});
        const ext=options.filters[0].extensions[0];
        return {canceled:false,filePath:output+'/검증 결과.'+ext};
      };
      dialog.showOpenDialog=async (_window,options)=>{
        globalThis.macSmokeDialogs.push({operation:'open',options});
        const isData=options.filters&&options.filters.some(f=>f.extensions.includes('csv'));
        return {canceled:false,filePaths:[isData?sourceData:output+'/검증 결과.studio']};
      };
    },{output,sourceData});
    const window = await app.firstWindow();
    await window.waitForURL(/^http:\/\/127\.0\.0\.1:/, { timeout: 120000 });
    await window.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState === 1, null, { timeout: 120000 });
    await window.locator('#data_steps input[type="file"]').waitFor({state:'attached',timeout:120000});
    await window.waitForFunction(() => !document.querySelector('#data_steps.recalculating'), null, {timeout:120000});
    assert.equal(await app.evaluate(({app})=>app.getVersion()), '1.3.1');
    await window.locator('.navbar-nav a[data-value="about"]').evaluate(e=>e.click());
    const about = window.locator('.about-application-document');
    await about.waitFor({state:'visible'});
    const aboutText = await about.innerText();
    for (const value of ['v1.3.1','2026-10-02','10.22934/statedu.studio']) {
      assert(aboutText.includes(value), `Packaged About page missing public release metadata: ${value}`);
    }
    await about.screenshot({path:path.join(output,'about-1.3.1.png')});
    for(const language of ['en','ja','zh','es','fr','de','vi','ko']) {
      await window.locator('.navbar-nav a[data-value="about_preferences"]').evaluate(e=>e.click());
      await window.locator('#app_language').selectOption(language);
      await window.waitForTimeout(500);
      assert.equal(await window.locator('.navbar-nav a[data-value="analysis_meta"]').count(),0);
      assert.equal(await window.locator('.navbar-nav a[data-value="One-group repeated-measures ANOVA"]').count(),0);
      assert.equal(await window.locator('.navbar-nav a[data-value="Repeated-measures ANOVA"]').count(),1);
      await window.locator('.navbar-nav a[data-value="about_user_guide"]').evaluate(e=>e.click());
      const guide=window.locator('#lazy_about_user_guide');
      await guide.waitFor({state:'visible'});
      await window.waitForFunction(title=>document.querySelector('#lazy_about_user_guide h1')?.textContent===title,
        documentSpecs[language].user_guide.title,{timeout:30000});
      const guideText=await guide.innerText();
      assert(guideText.includes('StatEdu Studio 1.3.1'),`Public Mac guide missing: ${language}`);
      assert(!guideText.includes('StatEdu Studio Dev'),`Developer installer instructions in public guide: ${language}`);
      await guide.screenshot({path:path.join(output,`guide-${language}.png`)});
    }
    await window.locator('#main_menu > li > a').first().evaluate(e=>e.click());
    await window.locator('body').screenshot({ path: path.join(output,'startup.png') });
    const readyMs = Date.now() - started;
    const errors=[];window.on('pageerror',e=>errors.push(e.message));
    await window.evaluate(()=>Shiny.setInputValue('browse_data_file',1,{priority:'event'}));
    await window.locator('#apply_all_variable_selection').waitFor({state:'visible'});
    await window.locator('#apply_all_variable_selection').click();
    const open = async value => {
      await window.locator(`a[data-value="${value}"]`).evaluate(el=>el.click());
      await window.waitForTimeout(500);
    };
    const move = async (prefix,variable,role) => {
      const option=window.locator(`[data-input-id="${prefix}_available"] [data-value="${variable}"]`);
      for(let attempt=0;attempt<3;attempt++) {
        await option.click();
        await window.waitForTimeout(300);
        if(await option.getAttribute('aria-selected')==='true') break;
      }
      assert.equal(await option.getAttribute('aria-selected'),'true');
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
    await open('Correlation');
    // Exercise real R -> private queue -> Electron -> PDF and file writers.
    for(const extension of ['html','pdf','excel']) {
      await window.locator(`#save_correlation_${extension}_dialog`).click();
      const ext=extension==='excel'?'xlsx':extension;
      const file=path.join(output,'검증 결과.'+ext);
      for(let i=0;i<240&&(!fs.existsSync(file)||fs.statSync(file).size<100);i++)await new Promise(r=>setTimeout(r,250));
      assert(fs.existsSync(file),`${ext} export did not finish`);
      await window.waitForTimeout(500);
      assert(fs.statSync(file).size>100,`Empty ${ext} export`);
      if(ext==='pdf')assert.equal(fs.readFileSync(file).subarray(0,5).toString(),'%PDF-');
    }
    await window.evaluate(()=>Shiny.setInputValue('save_settings_request',{}, {priority:'event'}));
    const project=path.join(output,'검증 결과.studio');
    for(let i=0;i<120&&!fs.existsSync(project);i++)await new Promise(r=>setTimeout(r,250));
    assert(fs.existsSync(project),'Project save failed');
    const projectData=JSON.parse(fs.readFileSync(project,'utf8').replace(/^\uFEFF/,''));
    assert(Object.keys(projectData).length>0);
    assert.equal(fs.realpathSync(projectData.data_file_path),fs.realpathSync(sourceData),'Project must retain its original data path');
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
    for(const [format,extension] of [['word','docx'],['hwpx','hwpx']]) {
      await window.locator(`#save_result_collection_${format}_dialog`).click();
      await window.locator('#confirm_document_export').waitFor({state:'visible'});
      await window.locator('#result_document_select_all').click();
      await window.locator('#confirm_document_export').click();
      const file=path.join(output,'검증 결과.'+extension);
      for(let i=0;i<240&&!fs.existsSync(file);i++)await new Promise(r=>setTimeout(r,250));
      assert(fs.existsSync(file)&&fs.statSync(file).size>100,`${extension} collection export failed`);
    }
    assert.deepStrictEqual(errors,[]);
    const dialogs=await app.evaluate(()=>globalThis.macSmokeDialogs);
    assert(dialogs.length>=4,'Native request dispatch was not exercised');
    fs.writeFileSync(path.join(output,'native-dialog-dispatch.json'),JSON.stringify(dialogs,null,2));
    // Electron owns the native unsaved-change prompt. Clear the test's dirty
    // flag to avoid Playwright/CDP also handling the same beforeunload dialog;
    // cancellation/discard are exercised separately through native UI.
    await window.evaluate(() => { window.easyflowSettingsDirty = false; });
    const closed=app.waitForEvent('close',{timeout:20000});
    await app.evaluate(({BrowserWindow})=>setTimeout(()=>BrowserWindow.getAllWindows().forEach(w=>w.close()),0));
    await closed;app=null;
    const log=path.join(dataDir,'logs/startup.log');
    for(let i=0;i<40&&!fs.readFileSync(log,'utf8').includes('R process exited');i++)await new Promise(r=>setTimeout(r,250));
    assert(fs.readFileSync(log,'utf8').includes('R process exited'));
    assert.equal(JSON.parse(fs.readFileSync(path.join(output,'saved-results.json'),'utf8')).entries.length,3);
    // Result history restores when resuming the application. In the 1.3.1
    // contract, opening a project clears the previous analysis/result context.
    app=await _electron.launch({executablePath:executable,args:[`--user-data-dir=${profile}`],
      env:{...process.env,STATEDU_APP_LANGUAGE:'ko',STATEDU_RESULT_STORE:path.join(output,'saved-results.json')},timeout:120000});
    let restored=await app.firstWindow();
    await restored.waitForURL(/^http:\/\/127\.0\.0\.1:/,{timeout:120000});
    await restored.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState===1,null,{timeout:120000});
    await restored.locator('a[data-value="result"]').evaluate(e=>e.click());
    await restored.waitForFunction(()=>document.querySelectorAll('#saved_results_list .saved-result-entry').length===3,null,{timeout:30000});
    await restored.screenshot({path:path.join(output,'restored-results.png')});
    await restored.evaluate(() => { window.easyflowSettingsDirty = false; });
    let restoredClosed=app.waitForEvent('close',{timeout:20000});
    await app.evaluate(({app})=>setTimeout(()=>app.quit(),0));
    await restoredClosed;app=null;
    app=await _electron.launch({executablePath:executable,args:[`--user-data-dir=${profile}`,project],
      env:{...process.env,STATEDU_APP_LANGUAGE:'ko',STATEDU_RESULT_STORE:path.join(output,'saved-results.json')},timeout:120000});
    restored=await app.firstWindow();
    await restored.waitForURL(/^http:\/\/127\.0\.0\.1:/,{timeout:120000});
    await restored.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState===1,null,{timeout:120000});
    await restored.waitForFunction(() => {
      const steps = document.querySelector('#data_steps');
      return steps && !steps.classList.contains('recalculating') && steps.innerText.includes('survival_validation.csv');
    }, null, {timeout:60000});
    await restored.screenshot({path:path.join(output,'restored-project.png')});
    await restored.evaluate(() => { window.easyflowSettingsDirty = false; });
    restoredClosed=app.waitForEvent('close',{timeout:20000});
    await app.evaluate(({app})=>setTimeout(()=>app.quit(),0));
    await restoredClosed;app=null;
    const result={version:'1.3.1',aboutPublicReleaseMetadata:true,publicLanguages:8,localizedPublicGuides:8,projectRestore:true,resultHistoryRestore:true,nativeDialogSelections:'automated Electron dialog responses; visual panel check separate',exports:['html','pdf','xlsx','docx','hwpx','studio'],status:'passed',readyMs,records,errors,shutdownConfirmed:true,output};
    fs.writeFileSync(path.join(output,'result.json'),JSON.stringify(result,null,2));console.log(JSON.stringify(result));
  } catch(error) {
    console.error('Primary validation failure:',error);
    if(app) { const w=await app.firstWindow();await w.screenshot({path:path.join(output,'failure.png')}).catch(()=>{});fs.writeFileSync(path.join(output,'failure.html'),await w.content().catch(()=>'')); }
    throw error;
  } finally {
    if(app) { await app.evaluate(({BrowserWindow}) => Promise.all(BrowserWindow.getAllWindows().map(w => w.webContents.executeJavaScript('window.easyflowSettingsDirty = false').catch(() => {})))).catch(()=>{}); const closed=app.waitForEvent('close',{timeout:20000});await app.evaluate(({BrowserWindow})=>setTimeout(()=>BrowserWindow.getAllWindows().forEach(w=>w.close()),0)).catch(()=>{});await closed; }
  }
})().catch(error=>{console.error(error);process.exitCode=1;});
