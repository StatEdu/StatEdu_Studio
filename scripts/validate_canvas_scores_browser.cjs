const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage({viewport:{width:1500,height:1100}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.goto('http://127.0.0.1:43991');
  await page.waitForFunction(()=>document.querySelector('.custom-model-canvas-root')?.__stateduModelCanvas && window.Shiny?.shinyapp?.$socket?.readyState===1).catch(async e=>{
    console.error(errors,await page.evaluate(()=>({root:!!document.querySelector('.custom-model-canvas-root'),api:Object.keys(window.StatEduModelCanvas||{}),scripts:[...document.scripts].map(s=>s.src),shiny:!!window.Shiny,socket:window.Shiny?.shinyapp?.$socket?.readyState})));
    await page.screenshot({path:'tmp/canvas-scores/startup-error.png'});throw e;
  });
  const source=JSON.parse(fs.readFileSync('tmp/canvas-scores/fixture.json','utf8')).source;
  await page.evaluate(source=>{
   const api=window.StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
   const s=api.state.snapshot(i.state);s.nodes=source.nodes;s.edges=source.edges;s.covariates=[];
   s.nodes.find(n=>n.id==='F').measurementPlacement='left';
   s.nodes.find(n=>n.id==='F').effectiveMeasurementPlacement='left';
   Object.assign(s.nodes.find(n=>n.id==='e'),{width:26,height:26});
   api.state.restore(i.state,s);api.canvas.render(i);api.bridge.sendState(i);
  },source);
  await page.locator('.custom-model-node[data-node-id="G"]').click();
  assert.equal(await page.locator(".structural-score-editor").count(),0);
  const fieldTop=await page.locator(".structural-setting-field").first().evaluate(e=>e.getBoundingClientRect().top);
  assert.equal(await page.locator('.custom-model-score-shortcut').count(),0);
  await page.locator('.custom-model-node[data-node-id="F"]').click({position:{x:10,y:15}});
  await page.locator('.custom-model-score-shortcut').waitFor();
  assert.equal(await page.locator('.structural-selection-summary .structural-score-editor').count(),1);
  assert.equal(await page.locator(".structural-setting-field").first().evaluate(e=>e.getBoundingClientRect().top),fieldTop);
  const exportClean=await page.evaluate(async()=>{
    const api=window.StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
    const before=JSON.stringify(api.state.snapshot(i.state));
    const withButton=new Uint8Array(await (await api.dialogs.exportPng(i)).arrayBuffer());
    document.querySelector('.custom-model-score-shortcut').remove();
    const withoutButton=new Uint8Array(await (await api.dialogs.exportPng(i)).arrayBuffer());
    api.canvas.render(i);
    return {same:withButton.length===withoutButton.length && withButton.every((v,k)=>v===withoutButton[k]),state:before===JSON.stringify(api.state.snapshot(i.state))};
  });
  assert.equal(exportClean.same,true);assert.equal(exportClean.state,true);
  await page.screenshot({path:'tmp/canvas-scores/shortcut.png',fullPage:true});
  await page.locator('.custom-model-score-shortcut').click();
  await page.locator('.canvas-score-panel').waitFor();
  assert.equal(await page.locator('.modal-backdrop').count(),0);
  await page.locator('.custom-model-variable-item[data-variable-name="q1"]').click();
  await page.locator('.custom-model-variable-item[data-variable-name="q12"]').click({modifiers:['Shift']});
  await page.locator('.custom-model-variable-item[data-variable-name="q1"]').dragTo(page.locator('.canvas-score-item-drop'));
  await page.waitForFunction(()=>document.querySelectorAll('.canvas-score-panel .analysis-transfer-option').length===12);
  const reorder = async (from,to) => {
    const a=await page.locator('.canvas-score-panel .analysis-transfer-option[data-value="'+from+'"]').boundingBox();
    const b=await page.locator('.canvas-score-panel .analysis-transfer-option[data-value="'+to+'"]').boundingBox();
    await page.mouse.move(a.x+40,a.y+a.height/2);await page.mouse.down();
    await page.mouse.move(b.x+40,b.y+2,{steps:12});await page.mouse.up();
  };
  await reorder('q2','q1');
  await page.waitForFunction(()=>document.querySelector('.canvas-score-panel .analysis-transfer-option')?.getAttribute('data-value')==='q2');
  await reorder('q1','q2');
  await page.waitForFunction(()=>document.querySelector('.canvas-score-panel .analysis-transfer-option')?.getAttribute('data-value')==='q1');
  await page.locator('.canvas-score-panel .analysis-transfer-option[data-value="q12"]').click();
  await page.locator('#structural_cbsem_score_remove_items').click();
  await page.waitForFunction(()=>document.querySelectorAll('.canvas-score-panel .analysis-transfer-option').length===11);
  await page.locator('.custom-model-variable-item[data-variable-name="q12"]').click();
  await page.locator('.canvas-score-add-selected').click();
  await page.waitForFunction(()=>document.querySelectorAll('.canvas-score-panel .analysis-transfer-option').length===12);
  await page.locator('input[name="structural_cbsem_score_scoring"][value="mean"]').check();
  assert.equal(await page.locator('input[name="structural_cbsem_score_min_response"]:checked').inputValue(),'0.8');
  await page.locator('input[name="structural_cbsem_score_min_response"][value="1"]').check();
  await page.locator('input[name="structural_cbsem_score_min_response"][value="0.8"]').check();
  await page.locator('input[name="structural_cbsem_score_scoring"][value="sum"]').check();
  await page.locator('input[name="structural_cbsem_score_method"][value="omega"]').check();
  await page.locator('input[name="structural_cbsem_score_method"][value="alpha"]').check();
  await page.screenshot({path:'tmp/canvas-scores/item-panel.png',fullPage:true});
  await page.locator('#structural_cbsem_score_calculate').click();
  await page.locator('.canvas-score-preview-ready').waitFor();
  await page.screenshot({path:'tmp/canvas-scores/single-preview.png',fullPage:true});
  await page.locator('#structural_cbsem_score_apply').click();
  await page.waitForFunction(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.some(n=>n.scoreDesign?.mode==='single'));
  const result=await page.evaluate(()=>{
   const api=window.StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
   const s=JSON.parse(JSON.stringify(api.state.snapshot(i.state)));api.state.restore(i.state,s);
   const d=i.state.nodes.find(n=>n.scoreDesign).scoreDesign;
   const e=i.state.edges.find(e=>e.id==='F__score1_residual');
   api.nodes.showProperties(i,'F');return {d,e};
  });
  assert.equal(result.e.free,false);assert.equal(result.e.fixedValue,result.d.calculation.residual);
  await page.locator('.custom-model-score-editor').click();
  await page.waitForFunction(()=>!!document.querySelector('#structural_cbsem_score_mode')?.selectize);
  await page.evaluate(()=>$('#structural_cbsem_score_mode')[0].selectize.setValue('parcels'));
  await page.locator('#structural_cbsem_score_allocation .canvas-parcel-balanced').waitFor();
  await page.locator('#structural_cbsem_score_rationale').fill('One-factor fixture; balanced item groups');
  await page.locator('#structural_cbsem_score_reviewed').check();
  await page.locator('#structural_cbsem_score_calculate').click();
  await page.locator('#structural_cbsem_score_preview .canvas-parcel-balanced').waitFor();
  await page.locator('#structural_cbsem_score_apply').click();
  await page.waitForFunction(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.filter(n=>n.scoreSpec).length===3);
  const parcel=await page.evaluate(()=>{
   const api=window.StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
   const before=api.state.snapshot(i.state);api.state.undo(i);const undo=api.state.snapshot(i.state);api.state.redo(i);
   api.canvas.render(i);
   return {before,undo,redo:api.state.snapshot(i.state),errors:i.validation.errors};
  });
  assert.equal(parcel.before.nodes.find(n=>n.scoreDesign).scoreDesign.allocation.method,'loading_balance');
  const geometry=await page.evaluate(()=>{
    const a=window.StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
    const original=a.state.snapshot(i.state),checks=[];
    for(const side of ['left','right','top','bottom']) {
      a.state.restore(i.state,original);i.state.nodes.find(n=>n.id==='F').measurementPlacement=side;
      const others=JSON.stringify(i.state.nodes.filter(n=>n.id!=='F'&&!n.id.startsWith('F__score')));
      a.canvas.reflowMeasurements(i,['F']);
      const pick=()=>({nodes:i.state.nodes.filter(n=>n.id.startsWith('F__score')).map(n=>[n.id,n.x,n.y]),edges:i.state.edges.filter(e=>e.id.startsWith('F__score')).map(e=>[e.id,e.fromSide,e.toSide,e.fixedCenter,e.directAnchors])});
      const targeted=pick(),unchanged=others===JSON.stringify(i.state.nodes.filter(n=>n.id!=='F'&&!n.id.startsWith('F__score')));
      a.canvas.reflowMeasurements(i);checks.push({targeted,standard:pick(),unchanged});
    }
    a.state.restore(i.state,original);a.canvas.render(i);return checks;
  });
  geometry.forEach(g=>{assert.deepEqual(g.targeted,g.standard);assert.equal(g.unchanged,true)});
  assert.equal(parcel.undo.nodes.find(n=>n.scoreDesign).scoreDesign.mode,'single');
  assert.deepEqual(parcel.before.nodes,parcel.redo.nodes);
  assert.deepEqual(parcel.errors,[]);
  await page.locator('.canvas-score-panel').waitFor({state:'detached'});
  await page.locator('.custom-model-node[data-node-id="F"]').click();
  assert.equal(await page.locator('.custom-model-score-shortcut').count(),0);
  await page.screenshot({path:'tmp/canvas-scores/parcel-model.png',fullPage:true});
  await page.evaluate(()=>{
   const api=window.StatEduModelCanvas,i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
   i.viewingResult=true;api.nodes.render(i);
  });
  assert.equal(await page.locator('.custom-model-score-shortcut').count(),0);
  assert.deepEqual(errors,[]);console.log('PASS: live Shiny original-item selector, preview, apply, re-open, actual parcel replacement, metadata roundtrip and undo/redo');
 }finally{await browser.close()}
})().catch(e=>{console.error(e);process.exitCode=1});
