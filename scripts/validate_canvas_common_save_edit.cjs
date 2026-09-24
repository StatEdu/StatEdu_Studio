const assert=require('node:assert/strict'), fs=require('node:fs'), path=require('node:path');
const {pathToFileURL}=require('node:url');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1600,height:1000}});
  for(const menu of ['mm','cfa','sem','pls']) {
   await page.goto(pathToFileURL(path.resolve(`tmp/canvas-export-validation/${menu}.html`)).href);
   for(const css of ['www/style.css','www/model-canvas/canvas.css']) await page.addStyleTag({path:css});
   for(const file of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas']) await page.addScriptTag({path:`www/model-canvas/${file}.js`});
   const result=await page.evaluate(async()=>{
    const api=window.StatEduModelCanvas, i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
    i.state.nodes=[{id:'x',name:'X',role:'independent',x:100,y:100},{id:'m',name:'M',role:'mediator',x:350,y:100},{id:'y',name:'Y',role:'dependent',x:600,y:100}];
    i.state.edges=[{id:'xm',from:'x',to:'m'},{id:'my',from:'m',to:'y'}];
    i.sourceSnapshot=api.state.snapshot(i.state);
    i.state.edges.forEach(e=>{e.label='.75(.001)';e.resultMatched=true;});
    i.resultSnapshot=api.state.snapshot(i.state); i.viewingResult=true;i.root.classList.add('is-viewing-result','has-result');
    api.canvas.render(i);
    // Layout-only edits retain the result and coefficients.
    window.Shiny={setInputValue:(name,payload)=>{window.lastInput={name,payload};}};
    i.state.nodes[0].x=120;api.bridge.sendState(i);
    const keptResult=!!i.resultSnapshot;
    api.state.pushHistory(i);api.edges.deleteEdge(i,'xm');api.canvas.render(i);api.bridge.sendState(i);
    const after={viewing:api.nodes.isViewingResult(i),mode:i.state.mode,result:!!i.resultSnapshot,edges:i.state.edges.length,label:i.state.edges[0].label||'',x:i.state.nodes[0].x};
    api.state.undo(i);
    const undoClean=i.state.edges.length===2 && i.state.edges.every(e=>!e.label);
    api.state.redo(i);api.canvas.render(i);api.bridge.sendState(i);
    await api.dialogs.exportModel(i);
    const figure=window.lastInput;
    window.stateduDesktopFiles={openText:async()=>{},save:async options=>{window.savedModel=options;return {canceled:false};}};
    await api.dialogs.save(i);
    return {keptResult,undoClean,after,figure:{name:figure.name,count:figure.payload.files.length,data:figure.payload.files[0].data},model:window.savedModel};
   });
   assert(result.keptResult,menu+': visual edits retain results');
   assert(result.undoClean,menu+': undo does not revive old coefficients');
   assert.deepEqual(result.after,{viewing:false,mode:'select',result:false,edges:1,label:'',x:120},menu);
   assert(result.figure.name.endsWith('_figures_snapshot'));
   assert.equal(result.figure.count,1);
   assert(Buffer.from(result.figure.data.split(',')[1],'base64').subarray(0,8).equals(Buffer.from([137,80,78,71,13,10,26,10])));
   const saved=JSON.parse(result.model.text);
   assert.equal(saved.edges.length,1);assert.equal(saved.edges[0].id,'my');assert(!saved.edges[0].label);
   assert.equal(result.model.binary,false);
   fs.writeFileSync(`tmp/mm-figure-snapshot/${menu}-common-payload.json`,JSON.stringify({files:[{name:'model.png',data:result.figure.data}]}));
   console.log('PASS:',menu,'result invalidation, preserved layout, common PNG export, edited model save');
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
