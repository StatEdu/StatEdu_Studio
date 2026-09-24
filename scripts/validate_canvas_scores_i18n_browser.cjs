const fs=require('node:fs'),assert=require('node:assert/strict');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
const key=s=>'canvas.score.'+s.toLowerCase().replace(/[^a-z0-9]+/g,'_').replace(/^_+|_+$/g,'')+(s.endsWith(':')?'_colon':'');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  for(const lang of ['ko','en','ja','zh','es','fr','de','vi']) {
   const translations=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
   const page=await browser.newPage({viewport:{width:1550,height:1100}}),errors=[];
   page.on('pageerror',e=>errors.push(e.message));
   await page.goto(`http://127.0.0.1:43991/?lang=${lang}`);
   await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('.custom-model-canvas-root')?.__stateduModelCanvas);
   const source=JSON.parse(fs.readFileSync('tmp/canvas-scores/fixture.json','utf8')).source;
   await page.evaluate(source=>{
    const a=window.StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
    const s=a.state.snapshot(i.state);s.nodes=source.nodes;s.edges=source.edges;s.covariates=[];
    s.nodes.find(n=>n.id==='F').measurementPlacement='left';
    s.nodes.find(n=>n.id==='F').effectiveMeasurementPlacement='left';
    Object.assign(s.nodes.find(n=>n.id==='e'),{width:26,height:26});
    s.selectedNodeIds=['F'];s.selectedNodeId='F';
    a.state.restore(i.state,s);a.canvas.render(i);a.nodes.openScoreEditor(i,'F');
   },source);
   await page.locator('.canvas-score-panel').waitFor();
   assert.ok((await page.locator('.canvas-score-panel h4').first().textContent()).includes(translations[key('Original items / reliability / parcels:')]));
   assert.equal(await page.locator('input[name="structural_cbsem_score_scoring"]:checked').inputValue(),'mean');
   assert.equal(await page.locator('input[name="structural_cbsem_score_method"]:checked').inputValue(),'omega');
   await page.evaluate(()=>{
    const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
    i.state.selectedVariables=Array.from({length:12},(_,k)=>'q'+(k+1));
   });
   await page.locator('.canvas-score-add-selected').click();
   await page.evaluate(()=>$('#structural_cbsem_score_mode')[0].selectize.setValue('parcels'));
   await page.locator('#structural_cbsem_score_allocation .canvas-parcel-balanced').waitFor();
   assert.equal(await page.locator('#structural_cbsem_score_allocation h4').textContent(),translations[key('Loading-balanced parcel allocation')]);
   await page.locator('#structural_cbsem_score_rationale').fill('User rationale: Mean is my scale name.');
   await page.locator('#structural_cbsem_score_reviewed').check();
   await page.locator('#structural_cbsem_score_calculate').click();
   await page.locator('.canvas-score-preview-ready').waitFor();
   const preview=await page.locator('.canvas-score-preview-ready').textContent();
   assert.ok(preview.includes('User rationale: Mean is my scale name.'));
   assert.ok(preview.includes(translations[key('Original items · reliability constraints / parcel definitions')]));
   if(lang!=='ko')assert.ok(!/[가-힣]/.test(await page.locator('.canvas-score-panel').textContent()));
   await page.locator('.canvas-score-panel').evaluate(e=>e.scrollTop=0);
   await page.screenshot({path:`tmp/canvas-score-i18n/${lang}-panel.png`,fullPage:true});
   assert.deepEqual(errors,[]);
   console.log('PASS live score panel and preview:',lang);
   await page.close();
  }
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1});
