const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path');
const {pathToFileURL}=require('node:url');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage({viewport:{width:1600,height:1100}});
  for(const menu of ['mm','cfa','sem','pls']){
   await page.goto(pathToFileURL(path.resolve(`tmp/canvas-export-validation/${menu}.html`)).href);
   for(const css of [fs.readFileSync('tmp/canvas-export-validation/bootstrap-path.txt','utf8').trim(),'www/style.css','www/model-canvas/canvas.css'])await page.addStyleTag({path:css});
   for(const file of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas'])await page.addScriptTag({path:`www/model-canvas/${file}.js`});
   for(const axis of ['x','y'])for(const autoAlign of [true,false]){
    const scale=await page.evaluate(({menu,axis,autoAlign})=>{
     const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas,mm=menu==='mm';
     const node=(id,x,y)=>({id,name:id,role:mm?'observed':'latent',x,y,width:100,height:50});
     const nodes=axis==='x'?[node('a',150,400),node('b',650,400),node('m',350,200)]:[node('a',650,100),node('b',650,500),node('m',350,250)];
     const edges=[];
     if(!mm){nodes.push({id:'item',name:'Item',role:'indicator',x:350,y:100,width:100,height:30});edges.push({id:'mi',from:'m',to:'item',kind:'measurement'});}
     Object.assign(i.state,{nodes,edges,moderations:[],covariates:[],selectedNodeIds:mm?['m']:['m','item'],selectedNodeId:'m',selectedEdgeId:null,mode:'select',autoAlign,history:[],redoStack:[]});
     Object.assign(i.state.canvas,{zoom:.7,viewZoom:.7,modelZoom:mm?1:1.2,paperViewMode:'manual'});
     window.StatEduModelCanvas.canvas.render(i);return .7*(mm?1:1.2);
    },{menu,axis,autoAlign});
    const box=await page.locator('[data-node-id="m"].custom-model-node').boundingBox();
    await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
    await page.mouse.move(box.x+box.width/2+(axis==='x'?46:0)*scale,box.y+box.height/2+(axis==='y'?46:0)*scale,{steps:5});
    const guide=page.locator('.structural-alignment-guide.is-midpoint');assert.equal(await guide.count(),1);
    if(axis==='x'&&autoAlign){
     const exported=await page.evaluate(async()=>{
      const serialize=XMLSerializer.prototype.serializeToString;let markup='';
      XMLSerializer.prototype.serializeToString=function(value){markup=serialize.call(this,value);return markup;};
      try{await window.StatEduModelCanvas.dialogs.exportPng(document.querySelector('.custom-model-canvas-root').__stateduModelCanvas,300);return markup;}
      finally{XMLSerializer.prototype.serializeToString=serialize;}
     });
     assert.ok(exported.includes('<svg'));assert.ok(!exported.includes('structural-alignment-guide'));
    }
    const g=await guide.boundingBox(),a=await page.locator('[data-node-id="a"].custom-model-node').boundingBox(),b=await page.locator('[data-node-id="b"].custom-model-node').boundingBox();
    const size=axis==='x'?'width':'height';assert.ok(Math.abs(g[axis]-(a[axis]+a[size]/2+b[axis]+b[size]/2)/2)<2);
    const nodes=await page.evaluate(()=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes);
    assert.ok(Math.abs(nodes.find(n=>n.id==='m')[axis]-((axis==='x'?350:250)+(autoAlign?50:46)))<.001);
    if(menu!=='mm')assert.ok(Math.abs(nodes.find(n=>n.id==='item')[axis]-((axis==='x'?350:100)+(autoAlign?50:46)))<.001);
    await page.mouse.up();assert.equal(await page.locator('.structural-alignment-guide-layer.is-visible').count(),0);
    await page.locator('[data-action="undo"]').click();
    assert.equal(await page.evaluate(axis=>document.querySelector('.custom-model-canvas-root').__stateduModelCanvas.state.nodes.find(n=>n.id==='m')[axis],axis),axis==='x'?350:250);
   }
   console.log('PASS:',menu,'horizontal/vertical midpoint, group center, zoom, snap toggle, cleanup, undo');
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
