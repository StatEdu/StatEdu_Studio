const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const {pathToFileURL}=require('node:url');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage({viewport:{width:1500,height:1050}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  for(const menu of ['mm','cfa','sem','pls']){
   await page.goto(pathToFileURL(path.resolve(`tmp/canvas-export-validation/${menu}.html`)).href);
   for(const css of [fs.readFileSync('tmp/canvas-export-validation/bootstrap-path.txt','utf8').trim(),'www/style.css','www/model-canvas/canvas.css'])await page.addStyleTag({path:css});
   for(const file of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas'])await page.addScriptTag({path:`www/model-canvas/${file}.js`});
   for(const historySize of [0,100])for(const ending of ['click','pointercancel','Escape']){
    const before=await page.evaluate(({menu,historySize})=>{
     const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
     Object.assign(i.state,{nodes:[{id:'a',role:menu==='mm'?'observed':'latent',name:'A',x:250,y:250,width:110,height:50},{id:'b',role:menu==='mm'?'dependent':'latent',name:'B',x:550,y:290,width:110,height:50}],edges:[],moderations:[],covariates:[],mode:'select',selectedNodeIds:['a','b'],selectedNodeId:'a',history:[],redoStack:[]});
     Object.assign(i.state.canvas,{zoom:1,viewZoom:1,modelZoom:1,paperViewMode:'manual'});
     const snapshot=window.StatEduModelCanvas.state.snapshot(i.state);
     i.state.history=Array.from({length:historySize},()=>JSON.parse(JSON.stringify(snapshot)));
     i.state.redoStack=[JSON.parse(JSON.stringify(snapshot))];
     window.StatEduModelCanvas.canvas.render(i);
     return JSON.parse(JSON.stringify({nodes:i.state.nodes,edges:i.state.edges,history:i.state.history,redo:i.state.redoStack}));
    },{menu,historySize});
    const box=await page.locator('.custom-model-node[data-node-id="a"]').boundingBox();
    await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
    if(ending!=='click'){
     await page.mouse.move(box.x+box.width/2+53,box.y+box.height/2+41,{steps:4});
     assert.notEqual(await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes[0].x),before.nodes[0].x);
     if(ending==='Escape')await page.keyboard.press('Escape');
     else await page.evaluate(()=>document.dispatchEvent(new PointerEvent('pointercancel',{bubbles:true,pointerId:1})));
    }
    await page.mouse.up();
    // Ensure detached listeners do not keep moving the nodes after cancellation.
    await page.mouse.move(box.x+box.width/2+75,box.y+box.height/2+60);
    const after=await page.evaluate(()=>{const s=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state;return {nodes:s.nodes,edges:s.edges,history:s.history,redo:s.redoStack};});
    assert.deepEqual(after,before,`${menu}/${historySize}/${ending}`);
    assert.equal(await page.locator('[data-action="redo"]').isEnabled(),true);
    assert.equal(await page.locator('.structural-alignment-guide-layer.is-visible').count(),0);
   }
  }
  assert.deepEqual(errors,[]);
  console.log('PASS: 24 cancel/click cases; positions, edges, 100-entry history, redo, cleared guides and listeners preserved');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
