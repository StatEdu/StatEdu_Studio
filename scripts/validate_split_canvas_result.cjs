const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path');
const {pathToFileURL}=require('node:url');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1500,height:1000}});
  await page.goto(pathToFileURL(path.resolve('tmp/canvas-export-validation/mm.html')).href);
  await page.evaluate(()=>{window.scopeHandlers={};window.Shiny={addCustomMessageHandler:(id,fn)=>window.scopeHandlers[id]=fn,setInputValue:()=>{}};});
  await page.addScriptTag({path:'www/analysis-scope.js'});
  for(const f of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas'])await page.addScriptTag({path:`www/model-canvas/${f}.js`});
  const result=await page.evaluate(async()=>{
   const api=window.StatEduModelCanvas,root=document.querySelector('.custom-model-canvas-root'),i=api.canvas.init(root);
   const source=api.state.snapshot(i.state);
   source.nodes=[{id:'x',name:'X',role:'independent',x:100,y:300},{id:'m',name:'M',role:'mediator',x:300,y:100},{id:'y',name:'Y',role:'dependent',x:500,y:300}];
   source.edges=[{id:'xm',from:'x',to:'m'},{id:'my',from:'m',to:'y'},{id:'xy',from:'x',to:'y'}];
   source.moderations=[];
   const first=api.state.clone(source);first.edges[0].label='.5';
   // A transient empty/stale view must not replace a new group's fitted topology.
   i.sourceSnapshot=api.state.snapshot(i.state);i.resultSnapshot=api.state.snapshot(i.state);
   api.bridge.applyResult({rootId:root.id,source,result:first,show:true});
   const firstCount=i.state.nodes.length;
   const firstFigure=await api.dialogs.exportReportFigure(i);
   const second=api.state.clone(source);second.edges[0].label='.8';
   i.state.nodes[0]&&(i.state.nodes[0].x=140);
   api.bridge.applyResult({rootId:root.id,source,result:second,show:true});
   const current=api.state.snapshot(i.state);
   const secondFigure=await api.dialogs.exportReportFigure(i);
   const output=document.createElement('div');output.id='custom_model_canvas_results';document.body.appendChild(output);
   const html='<section><h3>q7: 0</h3>'+firstFigure+'</section><section><h3>q7: 1</h3>'+secondFigure+'</section>';
   window.scopeHandlers['statedu-analysis-scope-finish']({outputId:output.id,html});
   // Direct model edits must continue to synchronize membership.
   const deleted=api.state.clone(source);deleted.nodes=deleted.nodes.slice(0,2);deleted.edges=deleted.edges.slice(0,1);
   const synced=api.bridge.syncVisualEdits(source,deleted);
   return {firstCount,nodes:current.nodes.length,edges:current.edges.length,x:current.nodes[0]?.x,label:i.resultSnapshot.edges[0]?.label,deleted:synced.nodes.length,images:output.querySelectorAll('img').length,afterFinish:i.state.nodes.length,html};
  });
  assert.equal(result.firstCount,3);assert.equal(result.nodes,3);assert.equal(result.edges,3);
  assert.equal(result.x,140);assert.equal(result.label,'.8');assert.equal(result.deleted,2);
  assert.equal(result.images,2);assert.equal(result.afterFinish,3);
  fs.mkdirSync('tmp/split-canvas-result',{recursive:true});fs.writeFileSync('tmp/split-canvas-result/split.html',result.html);
  console.log('PASS: successive group results preserve topology, layout and new coefficients; explicit deletion still propagates');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
