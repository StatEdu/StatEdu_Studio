const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
 const browser = await chromium.launch({channel:'chrome',headless:true});
 try {
  const page = await browser.newPage({viewport:{width:1500,height:1050}});
  await page.goto(pathToFileURL(path.resolve('tmp/canvas-export-validation/mm.html')).href);
  for (const css of [fs.readFileSync('tmp/canvas-export-validation/bootstrap-path.txt','utf8').trim(),'www/style.css','www/model-canvas/canvas.css']) await page.addStyleTag({path:css});
  for (const file of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas']) await page.addScriptTag({path:`www/model-canvas/${file}.js`});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  const emptySettings=await page.locator('.structural-selection-settings').evaluate(e=>({height:e.getBoundingClientRect().height,max:getComputedStyle(e).maxHeight}));
  assert.ok(emptySettings.height>=72&&emptySettings.height<110,'initial hint stays compact');
  assert.equal(emptySettings.max,'none','settings have no maximum height');
  const seed=async(role,zoom=1,enabled=true)=>page.evaluate(({role,zoom,enabled})=>{
   const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
   Object.assign(i.state,{nodes:[{id:'x',name:'Independent',role:'independent',x:150,y:250,width:110,height:38},{id:'y',name:'Dependent',role,x:550,y:290,width:110,height:38}],edges:[],moderations:[],covariates:[],selectedNodeIds:['y'],selectedNodeId:'y',selectedEdgeId:null,mode:'select',autoAlign:enabled,history:[],redoStack:[]});
   i.state.canvas.zoom=zoom;i.state.canvas.modelZoom=1;i.state.canvas.paperViewMode='manual';window.StatEduModelCanvas.canvas.render(i);
  },{role,zoom,enabled});
  const position=()=>page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.find(n=>n.id==='y'));
  for(const role of ['observed','dependent','moderator','mediator','independent','covariate'])for(const zoom of [1,.7]){
   await seed(role,zoom);
   const box=await page.locator('.custom-model-node[data-node-id="y"]').boundingBox();
   await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
   await page.mouse.move(box.x+box.width/2+20*zoom,box.y+box.height/2-34*zoom,{steps:5});
   assert.equal((await position()).y,250,`${role} at zoom ${zoom}`);
   assert.equal(await page.locator('.structural-alignment-guide-layer.is-visible .is-horizontal').count(),2);
   const guideBox=await page.locator('.structural-alignment-guide.is-horizontal').first().boundingBox();
   const alignedBox=await page.locator('.custom-model-node[data-node-id="x"]').boundingBox();
   assert.ok(Math.abs(guideBox.y-alignedBox.y)<2,'guide follows node at every zoom');
   if(role==='dependent'&&zoom===1){fs.mkdirSync('tmp/mediation-alignment',{recursive:true});await page.screenshot({path:'tmp/mediation-alignment/guides.png'});}
   await page.mouse.up();assert.equal((await position()).y,250);
   assert.equal(await page.locator('.structural-alignment-guide-layer.is-visible').count(),0);
   await page.locator('[data-action="undo"]').click();assert.equal((await position()).y,290);
  }
  await seed('dependent',1,false);
  let box=await page.locator('.custom-model-node[data-node-id="y"]').boundingBox();
  await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();await page.mouse.move(box.x+box.width/2+20,box.y+box.height/2-34);await page.mouse.up();
  assert.equal((await position()).y,256,'disabled snapping preserves free position');
  await seed('dependent');
  box=await page.locator('.custom-model-node[data-node-id="y"]').boundingBox();
  await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();await page.mouse.move(box.x+box.width/2-394,box.y+box.height/2+70);
  assert.equal((await position()).x,150,'column snap');await page.mouse.up();assert.equal((await position()).x,150);
  const settings=await page.locator('.structural-selection-settings').evaluate(e=>({height:e.getBoundingClientRect().height,font:getComputedStyle(e.querySelector('.structural-selection-settings-title')).fontSize,body:getComputedStyle(e.querySelector('.structural-selection-settings-body')).fontSize}));
  assert.ok(settings.height>=72);assert.equal(settings.font,'13px');assert.equal(settings.body,'13px');
  assert.ok(settings.height>emptySettings.height+150,'selected settings expand with their content');
  assert.equal(await page.locator('.structural-setting-field .form-control').first().evaluate(e=>getComputedStyle(e).fontSize),'12px');
  await page.locator('[data-action="autoAlign"]').click();
  assert.equal(await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.autoAlign),false);
  await page.locator('[data-action="autoAlign"]').click();
  assert.equal(await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.autoAlign),true);
  await page.screenshot({path:'tmp/mediation-alignment/settings.png'});
  assert.deepEqual(errors,[]);
  console.log('PASS: all observed roles; row/column snap; zoom; disabled mode; undo; transient red guides; visible +1px settings');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
