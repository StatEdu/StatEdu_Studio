const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const {pathToFileURL}=require('node:url');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage({viewport:{width:1600,height:1100}});
  for(const menu of ['cfa','sem','pls']){
   await page.goto(pathToFileURL(path.resolve(`tmp/canvas-export-validation/${menu}.html`)).href);
   for(const css of [fs.readFileSync('tmp/canvas-export-validation/bootstrap-path.txt','utf8').trim(),'www/style.css','www/model-canvas/canvas.css'])await page.addStyleTag({path:css});
   for(const file of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas'])await page.addScriptTag({path:`www/model-canvas/${file}.js`});
   for(const [viewZoom,modelZoom] of [[.6,1],[.7,1.4],[1.2,.8]])for(const autoAlign of [true,false]){
    await page.evaluate(({viewZoom,modelZoom,autoAlign})=>{
     const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
     Object.assign(i.state,{nodes:[{id:'a',role:'latent',name:'A',x:150,y:250,width:100,height:50},{id:'b',role:'latent',name:'B',x:550,y:290,width:100,height:50}],edges:[],moderations:[],covariates:[],selectedNodeIds:['b'],selectedNodeId:'b',selectedEdgeId:null,mode:'select',autoAlign,history:[],redoStack:[]});
     Object.assign(i.state.canvas,{viewZoom,modelZoom,paperViewMode:'manual'});window.StatEduModelCanvas.canvas.render(i);
    },{viewZoom,modelZoom,autoAlign});
    const scale=viewZoom*modelZoom;
    const box=await page.locator('.custom-model-node[data-node-id="b"]').boundingBox();
    await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
    await page.mouse.move(box.x+box.width/2+20*scale,box.y+box.height/2-34*scale,{steps:5});
    const node=await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.find(n=>n.id==='b'));
    assert.ok(Math.abs(node.x-570)<.001,`${menu}: horizontal drag ${viewZoom}/${modelZoom}: ${node.x}`);
    assert.ok(Math.abs(node.y-(autoAlign?250:256))<.001,`${menu}: vertical drag/snap ${viewZoom}/${modelZoom}: ${node.y}`);
    const guide=await page.locator('.structural-alignment-guide.is-horizontal').first().boundingBox();
    const target=await page.locator('.custom-model-node[data-node-id="a"]').boundingBox();
    assert.ok(Math.abs(guide.y-target.y)<2,'guide matches the target on screen');
    await page.mouse.up();assert.equal(await page.locator('.structural-alignment-guide-layer.is-visible').count(),0);
    await page.locator('[data-action="undo"]').click();
    const restored=await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.find(n=>n.id==='b'));
    assert.equal(restored.x,550);assert.equal(restored.y,290);
   }
  }
  console.log('PASS: CFA/SEM/PLS, combined paper/model zoom, free drag, snapping, guide positions, undo');
 }finally{await browser.close()}
})().catch(e=>{console.error(e);process.exit(1)});
