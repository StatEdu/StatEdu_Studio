const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1700,height:1100}});
  const errors=[]; page.on('pageerror', e=>errors.push(e.message));
  await page.goto(pathToFileURL(path.resolve('tmp/canvas-export-validation/mm.html')).href);
  for(const css of [fs.readFileSync('tmp/canvas-export-validation/bootstrap-path.txt','utf8').trim(),'www/style.css','www/model-canvas/canvas.css']) await page.addStyleTag({path:css});
  for(const script of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas']) await page.addScriptTag({path:`www/model-canvas/${script}.js`});
  await page.evaluate(()=>{
   const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
   i.state.variables=['x','m','y','w','c','d'].map(name=>({name,dataLabel:name,measurement:'continuous'}));
   i.state.autoAlign=false;
   window.StatEduModelCanvas.canvas.render(i);
  });
  assert.equal(await page.locator('.custom-model-role-button').count(),0);
  assert.equal(await page.locator('[data-action="addObserved"] svg rect').count(),1);
  await page.locator('[data-action="addObserved"]').click();
  for(const [name,x,y] of [['x',100,230],['m',370,230],['y',640,230],['w',300,60]]) {
   await page.locator('.custom-model-paper').click({position:{x,y}});
   assert.equal(await page.locator('.custom-model-modal-backdrop').count(),0);
   const target=page.locator('.custom-model-node').last();
   await page.locator(`.custom-model-variable-item[data-variable-name="${name}"]`).dragTo(target);
   assert.equal(await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.at(-1).variableId),name);
   if(name==='w') {
    await page.locator('[data-action="undo"]').click();
    assert.equal(await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.at(-1).unassigned),true);
    await page.locator('[data-action="redo"]').click();
    assert.equal(await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.at(-1).variableId),'w');
   }
  }
  const roles=()=>page.evaluate(()=>Object.fromEntries(document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.map(n=>[n.variableId,n.role])));
  assert.deepEqual(await roles(),{x:'observed',m:'observed',y:'observed',w:'observed'});
  async function nodeCenter(name) {
   const id=await page.evaluate(name=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.find(n=>n.variableId===name).id,name);
   const box=await page.locator(`[data-node-id="${id}"].custom-model-node`).boundingBox();
   return {x:box.x+box.width/2,y:box.y+box.height/2};
  }
  async function drag(from,to) {await page.mouse.move(from.x,from.y);await page.mouse.down();await page.mouse.move(to.x,to.y,{steps:8});await page.mouse.up();}
  await page.locator('[data-action="connect"]').click();
  await drag(await nodeCenter('x'),await nodeCenter('m'));
  await drag(await nodeCenter('m'),await nodeCenter('y'));
  assert.deepEqual(await roles(),{x:'independent',m:'mediator',y:'dependent',w:'observed'});
  // The same pointer handler used by SEM: drag the moderator onto a path.
  const a=await nodeCenter('x'), b=await nodeCenter('m');
  await drag(await nodeCenter('w'),{x:(a.x+b.x)/2,y:(a.y+b.y)/2});
  assert.equal((await roles()).w,'moderator');
  await page.locator('[data-action="undo"]').click();
  assert.equal((await roles()).w,'observed');
  await page.locator('[data-action="redo"]').click();
  assert.equal((await roles()).w,'moderator');
  await page.locator('.custom-model-variable-item[data-variable-name="c"]').click();
  await page.locator('.custom-model-variable-item[data-variable-name="d"]').click({modifiers:['Control']});
  await page.locator('[data-action="covariates"]').click();
  assert.equal(await page.locator('.custom-model-modal-backdrop').count(),0);
  assert.deepEqual(await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.covariates),['c','d']);
  await page.locator('.custom-model-variable-item[data-variable-name="d"]').click();
  await page.locator('[data-action="covariates"]').click();
  const snapshot=await page.evaluate(()=>{
   const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas, api=window.StatEduModelCanvas;
   const x=i.state.nodes.find(n=>n.variableId==='x'), y=i.state.nodes.find(n=>n.variableId==='y');
   if(api.edges.createEdge(i,y.id,x.id)) throw Error('cycle accepted');
   return api.state.snapshot(i.state);
  });
  assert.deepEqual(snapshot.covariates,['c']);
  const restored=await page.evaluate(snapshot=>{
   const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas, api=window.StatEduModelCanvas;
   api.state.restore(i.state,JSON.parse(JSON.stringify(snapshot)));
   api.canvas.render(i);
   const saved=api.state.snapshot(i.state);
   api.edges.deleteEdge(i,i.state.edges[0].id); api.canvas.render(i);
   if(i.state.nodes.find(n=>n.variableId==='x').role!=='observed'||i.state.moderations.length) throw Error('delete failed to recompute roles');
   api.state.restore(i.state,saved); api.canvas.render(i);
   return saved;
  },snapshot);
  assert.deepEqual(restored.nodes,snapshot.nodes);
  assert.deepEqual(restored.moderations,snapshot.moderations);
  assert.equal(restored.covariateTypes.c.encoding,'continuous');
  fs.writeFileSync('tmp/canvas-export-validation/mm-arrow-snapshot.json',JSON.stringify(snapshot));
  await page.locator('.custom-model-diagram-panel').screenshot({path:'tmp/canvas-export-validation/mm-arrow-toolbar.png'});
  assert.deepEqual(errors,[]);
  console.log('PASS: placement without roles, SEM pointer connections, mediation/moderation inference, undo/redo, covariates and cycle rejection');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1)});
