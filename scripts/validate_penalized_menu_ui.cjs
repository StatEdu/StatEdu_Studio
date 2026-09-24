const assert=require('node:assert/strict'),fs=require('node:fs');const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE);
(async()=>{const b=await chromium.launch({channel:'chrome',headless:true});try{const p=await b.newPage({viewport:{width:1550,height:1100}});await p.goto('http://127.0.0.1:3869');await p.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1);
const prefix='penalized_regularized';
for(const [v,role] of [['y','y'],['x','x'],['z','x'],['sex','x'],['group','x']]){await p.locator(`[data-input-id="${prefix}_available"] [data-value="${v}"]`).click();await p.locator(`#${prefix}_move_${role}`).click();await p.locator(`[data-input-id="${prefix}_${role}"] [data-value="${v}"]`).waitFor();}
await p.locator(`[data-input-id="${prefix}_x"] [data-value="z"]`).click();
await p.waitForFunction(()=>document.getElementById('penalized_regularized_move_x').textContent.trim()==='<');
await p.locator(`#${prefix}_x_up`).click();
await p.waitForFunction(()=>document.querySelector('[data-input-id="penalized_regularized_x"] .analysis-transfer-option')?.dataset.value==='z');
await p.locator(`#${prefix}_x_down`).click();
await p.waitForFunction(()=>document.querySelector('[data-input-id="penalized_regularized_x"] .analysis-transfer-option')?.dataset.value==='x');
await p.locator(`#${prefix}_move_x`).click();
await p.locator(`[data-input-id="${prefix}_available"] [data-value="z"]`).waitFor();
await p.locator(`[data-input-id="${prefix}_available"] [data-value="z"]`).click();
await p.waitForFunction(()=>document.getElementById('penalized_regularized_move_x').textContent.trim()==='>');
await p.locator(`#${prefix}_move_x`).click();
await p.locator(`[data-input-id="${prefix}_x"] [data-value="z"]`).waitFor();
await p.locator(`#${prefix}_bootstrap`).fill('2');await p.locator(`#${prefix}_setup`).screenshot({path:'tmp/penalized-menu/setup.png'});
let snapshots=[];
for(const [key,title] of [['ridge','릿지 회귀'],['lasso','라소 회귀'],['elastic_net','엘라스틱넷 회귀']]){
 await p.locator(`input[name="${prefix}_method"][value="${key}"]`).check();if(key!=='ridge'){await p.locator(`#${prefix}_post_selection`).check();await p.locator(`#${prefix}_inference_splits`).fill('20');}
 await p.locator(`#run_${prefix}`).click();
 await p.locator(`#${prefix}_results h3`).filter({hasText:title}).waitFor({timeout:90000});
 await p.waitForFunction(()=>!document.querySelector('.recalculating')&&document.querySelectorAll('#penalized_regularized_results img').length===2&&Array.from(document.querySelectorAll('#penalized_regularized_results img')).every(i=>i.complete&&i.naturalWidth>0));
 await p.evaluate(()=>{window.qaSnapshot=null;const original=Shiny.setInputValue;if(!window.qaOriginal){window.qaOriginal=original;Shiny.setInputValue=function(id,value,opts){if(id==='save_penalized_regularized_html_dialog_snapshot'){window.qaSnapshot=value;return;}return window.qaOriginal(id,value,opts);};}});
 await p.locator(`#save_${prefix}_html_dialog`).click();await p.waitForFunction(()=>window.qaSnapshot?.html);
 const snap=await p.evaluate(()=>window.qaSnapshot);assert(!snap.error,snap.error);assert(snap.html.includes(title));assert(snap.html.includes('data:image/png;base64,'));assert(snap.html.includes('Nested CV RMSE'));assert(snap.html.includes('Table 2. Penalized regression coefficients'));
 if(key==='ridge')assert(!snap.html.includes('Table 3. Bootstrap selection stability'));else {assert(snap.html.includes('Table 4. Post-selection inference'));assert(snap.html.includes('Tested in all splits'));assert(snap.html.includes('An em dash means no independent test'));assert(snap.html.includes('Table 5. Categorical variables'));assert(snap.html.includes('omnibus'));}
 if(key==='lasso')await p.locator(`#${prefix}_results`).screenshot({path:'tmp/penalized-menu/inference-results.png'});
 snapshots.push(snap.html);
}
fs.writeFileSync('tmp/penalized-menu/results.html',snapshots.join('\n'));console.log('PASS: unified menu, 3 method runs, live tables and 2 captured figures per method');
}finally{await b.close();}})().catch(e=>{console.error(e);process.exit(1)});
