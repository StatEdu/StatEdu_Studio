const {chromium}=require('playwright'),fs=require('node:fs'),assert=require('node:assert/strict');
fs.mkdirSync('tmp/scope-canvas-live',{recursive:true});
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage({viewport:{width:1550,height:1050}});const errors=[];page.on('pageerror',e=>{errors.push(e.message);console.log('PAGE ERROR',e.message)});page.on('response',r=>{if(r.status()>=400)console.log(r.status(),r.url())});
 await page.goto('http://127.0.0.1:3878');await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1);
 await page.locator('[data-input-id="scope_cases_available"] [data-value="q7"]').dblclick();
 await page.locator('input[name="scope_cases_values"]').first().check();await page.locator('#scope_cases_apply').click();
 await page.locator('a[data-value="analysis_custom_model_canvas"]').click();
 await page.waitForFunction(()=>document.querySelector('.custom-model-canvas-root')?.__stateduModelCanvas);
 await page.evaluate(()=>{const a=window.StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas,s=a.state.snapshot(i.state);
 s.nodes=[{id:'x',name:'X',variableId:'X',role:'independent',x:100,y:300},{id:'m',name:'M',variableId:'M',role:'mediator',x:380,y:120},{id:'y',name:'Y',variableId:'Y',role:'dependent',x:650,y:300}];s.edges=[{id:'xm',from:'x',to:'m'},{id:'xy',from:'x',to:'y'},{id:'my',from:'m',to:'y'}];s.covariates=['C'];s.moderations=[];
 window.testSource=s;a.state.restore(i.state,s);a.canvas.render(i);a.bridge.sendState(i);
 Shiny.setInputValue('custom_mm_boot_r',20);Shiny.setInputValue('custom_mm_residual_diagnostics',false);Shiny.setInputValue('custom_mm_auto_method',false);
 });
 await page.evaluate(()=>{window.qaNotifications=[];new MutationObserver(()=>{document.querySelectorAll('.shiny-notification').forEach(n=>{if(!window.qaNotifications.includes(n.textContent))window.qaNotifications.push(n.textContent)})}).observe(document.body,{childList:true,subtree:true})});await page.waitForTimeout(300);
 await page.evaluate(()=>Shiny.setInputValue('custom_model_canvas_run_confirm',{...window.testSource,nonce:Date.now()},{priority:'event'}));
 await page.waitForFunction(()=>document.querySelector('.custom-model-canvas-root')?.__stateduModelCanvas?.resultSnapshots?.[0]?.scopeGroup,null,{timeout:90000}).catch(async e=>{console.log((await page.locator('body').innerText()).slice(-7000));console.log(await page.evaluate(()=>({notifications:window.qaNotifications,scope:window.stateduCanvasScopes,state:document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state})));throw e});
 await page.waitForFunction(()=>document.querySelectorAll('.custom-model-node').length>=3);
 assert.equal(await page.locator('.custom-model-result-group-select option').count(),1);
 assert((await page.locator('.custom-model-result-group-select').innerText()).includes('일반병동'));
 await page.locator('.custom-model-diagram-panel').screenshot({path:'tmp/scope-canvas-live/cases.png'});
 const cases=await page.evaluate(async()=>{const a=StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;return {nodes:i.state.nodes.length,covariates:i.state.covariates,html:await a.dialogs.exportReportFigure(i)}});
 assert.deepEqual(cases.covariates,['C']);assert.equal(cases.nodes,3);fs.writeFileSync('tmp/scope-canvas-live/cases.html',cases.html);
 await page.locator('a[data-value="Cases"]').click();await page.locator('#scope_cases_clear').click();
 await page.locator('a[data-value="Split"]').click();await page.locator('[data-input-id="scope_split_available"] [data-value="q7"]').dblclick();await page.locator('#scope_split_apply').click();
 await page.locator('a[data-value="analysis_custom_model_canvas"]').click();
 await page.evaluate(()=>Shiny.setInputValue('custom_model_canvas_run_confirm',{...window.testSource,nonce:Date.now()},{priority:'event'}));
 await page.waitForFunction(()=>window.stateduSplitSnapshots?.custom_model_canvas_results,null,{timeout:120000});
 await page.waitForFunction(()=>document.querySelectorAll('.custom-model-result-group-select option').length===2);
 const keys=await page.locator('.custom-model-result-group-select option').evaluateAll(o=>o.map(x=>x.value));
 const images=[];const labels=[];
 for(const key of keys){await page.locator('.custom-model-result-group-select').selectOption(key);const r=await page.evaluate(async()=>{const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;return {nodes:i.state.nodes.length,edges:i.state.edges,html:await StatEduModelCanvas.dialogs.exportReportFigure(i)}});assert.equal(r.nodes,3);images.push(r.html);labels.push(JSON.stringify(r.edges));}
 assert.notEqual(labels[0],labels[1]);assert.notEqual(images[0],images[1]);
 await page.locator('.custom-model-diagram-panel').screenshot({path:'tmp/scope-canvas-live/split.png'});
 fs.writeFileSync('tmp/scope-canvas-live/split.html',await page.evaluate(()=>window.stateduSplitSnapshots.custom_model_canvas_results));
 fs.writeFileSync('tmp/scope-canvas-live/group1.html',images[0]);fs.writeFileSync('tmp/scope-canvas-live/group2.html',images[1]);
 assert.deepEqual(errors,[]);console.log('PASS: real scoped worker, singleton covariate, case-only label, two split groups, selector changes coefficients and exported PNGs');
}finally{await browser.close()}})().catch(e=>{console.error(e);process.exit(1)});




