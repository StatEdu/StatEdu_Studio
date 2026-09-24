const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path');
const {pathToFileURL}=require('node:url');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try {
  const page=await browser.newPage({viewport:{width:1600,height:1000}});
  await page.goto(pathToFileURL(path.resolve('tmp/canvas-export-validation/mm.html')).href);
  for(const file of ['www/style.css','www/model-canvas/canvas.css']) await page.addStyleTag({path:file});
  for(const script of ['state','layout','shiny-bridge','edges','nodes','dialogs','toolbar','canvas']) await page.addScriptTag({path:`www/model-canvas/${script}.js`});
  await page.evaluate(html=>{
   const output=document.createElement('div');output.id='custom_model_canvas_results';output.innerHTML=html;document.body.appendChild(output);
   const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
   i.state.nodes=[{id:'x',name:'X',role:'independent',x:100,y:100},{id:'y',name:'Y',role:'dependent',x:400,y:100}];
   i.state.edges=[{id:'xy',from:'x',to:'y'}];window.StatEduModelCanvas.canvas.render(i);
   window.Shiny={setInputValue:(name,payload)=>{if(name==='custom_model_canvas_figures_snapshot') window.savedFigures=payload;}};
  },fs.readFileSync('tmp/mm-figure-snapshot/plots.html','utf8'));
  // Reproduce the loaded browser URL, including percent-encoded base64 line breaks.
  await page.locator('#custom_model_canvas_results img').evaluateAll(imgs=>Promise.all(imgs.map(img=>img.decode())));
  const result=await page.evaluate(async()=>{
   const i=document.querySelector('.custom-model-canvas-root').__stateduModelCanvas;
   await window.StatEduModelCanvas.dialogs.exportModel(i);
   return {payload:window.savedFigures,original:[...document.querySelectorAll('#custom_model_canvas_results img')].map(img=>img.src)};
  });
  assert.equal(result.payload.files.length,result.original.length+1);
  const pngBytes=uri=>Buffer.from(decodeURIComponent(uri.slice(uri.indexOf(',')+1)).replace(/\s/g,''),'base64');
  result.original.forEach((uri,index)=>assert.deepEqual(pngBytes(result.payload.files[index+1].data),pngBytes(uri)));
  assert(result.original.some(uri=>uri.includes('%0A')),'fixture must exercise the reported encoded line-break failure');
  assert(result.payload.files.some(file=>file.name.startsWith('johnson_neyman')));
  fs.writeFileSync('tmp/mm-figure-snapshot/payload.json',JSON.stringify(result.payload));
  const browserPayload=JSON.parse(JSON.stringify(result.payload));
  browserPayload.files.slice(1).forEach((file,index)=>{file.data=result.original[index];});
  fs.writeFileSync('tmp/mm-figure-snapshot/browser-payload.json',JSON.stringify(browserPayload));
  console.log('PASS: actual export action captures model plus all displayed moderation/JN plots without rerendering');
 }finally {await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
