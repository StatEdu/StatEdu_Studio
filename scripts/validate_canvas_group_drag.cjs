const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const {pathToFileURL}=require('node:url');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
const close=(a,b)=>assert.ok(Math.abs(a-b)<.002,`${a} != ${b}`);
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage({viewport:{width:1600,height:1100}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  for(const menu of ['mm','cfa','sem','pls']){
   await page.goto(pathToFileURL(path.resolve(`tmp/canvas-export-validation/${menu}.html`)).href);
   for(const css of [fs.readFileSync('tmp/canvas-export-validation/bootstrap-path.txt','utf8').trim(),'www/style.css','www/model-canvas/canvas.css'])await page.addStyleTag({path:css});
   for(const file of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas'])await page.addScriptTag({path:`www/model-canvas/${file}.js`});
   for(const zoom of [.65,1])for(const selection of ['all','subset']){
    const before=await page.evaluate(({menu,zoom,selection})=>{
     const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
     const mm=menu==='mm';
     const nodes=mm?[
      {id:'a',role:'independent',name:'X',x:250,y:250,width:110,height:38},
      {id:'i',role:'mediator',name:'M',x:430,y:180,width:110,height:38},
      {id:'b',role:'dependent',name:'Y',x:650,y:400,width:110,height:38}
     ]:[
      {id:'a',role:'latent',name:'A',x:300,y:250,width:100,height:50,measurementPlacement:'left'},
      {id:'b',role:'latent',name:'B',x:650,y:450,width:100,height:50},
      {id:'i',role:'indicator',name:'Item',x:150,y:250,width:100,height:30}
     ];
     const edges=mm?[{id:'am',from:'a',to:'i',kind:'path'},{id:'mb',from:'i',to:'b',kind:'path'}]:[{id:'ai',from:'a',to:'i',kind:'measurement'}];
     if(!mm&&menu!=='pls'){nodes.push({id:'e',role:'error',name:'e',x:90,y:250,width:26,height:26});edges.push({id:'ei',from:'e',to:'i',kind:'path'});}
     Object.assign(i.state,{nodes,edges,moderations:[],covariates:[],mode:'select',autoAlign:true,selectedNodeIds:nodes.filter(n=>selection==='all'||n.id!=='b').map(n=>n.id),selectedNodeId:'a',selectedEdgeId:null,history:[],redoStack:[]});
     Object.assign(i.state.canvas,{zoom,viewZoom:zoom,modelZoom:mm?1:1.2,paperViewMode:'manual'});
     window.StatEduModelCanvas.canvas.render(i);
     return JSON.parse(JSON.stringify({nodes:i.state.nodes,edges:i.state.edges,selected:i.state.selectedNodeIds}));
    },{menu,zoom,selection});
    const scale=zoom*(menu==='mm'?1:1.2);
    const box=await page.locator('.custom-model-node[data-node-id="a"]').boundingBox();
    await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
    await page.mouse.move(box.x+box.width/2+42*scale,box.y+box.height/2+31*scale,{steps:6});await page.mouse.up();
    const after=await page.evaluate(()=>{const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;return {nodes:i.state.nodes,edges:i.state.edges}});
    for(const old of before.nodes){const now=after.nodes.find(n=>n.id===old.id);assert.ok(now);const moved=before.selected.includes(old.id);close(now.x,old.x+(moved?42:0));close(now.y,old.y+(moved?31:0));}
    assert.deepEqual(after.edges,before.edges,'connections and edge settings preserved');
    assert.equal(await page.locator('.structural-alignment-guide-layer.is-visible').count(),0);
    await page.locator('[data-action="undo"]').click();
    const undone=await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes);
    for(const old of before.nodes){const now=undone.find(n=>n.id===old.id);close(now.x,old.x);close(now.y,old.y);}
    await page.locator('[data-action="redo"]').click();
    const redone=await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes);
    for(const old of after.nodes){const now=redone.find(n=>n.id===old.id);close(now.x,old.x);close(now.y,old.y);}
   }
  }
  assert.deepEqual(errors,[]);
  console.log('PASS: 16 group-drag cases across MM/CFA/SEM/PLS; combined zoom; relative spacing; unselected nodes; connections; undo/redo');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
