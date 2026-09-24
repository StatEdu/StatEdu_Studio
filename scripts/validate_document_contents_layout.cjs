const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage({viewport:{width:1500,height:1000}}),errors=[];
 page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43989/?lang=en');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&!document.documentElement.classList.contains('shiny-busy'));
 await page.evaluate(()=>Shiny.setInputValue('fixture_result',Date.now(),{priority:'event'}));
 async function visit(v){await page.locator(`.navbar-nav a[data-value="${v}"]`).first().evaluate(e=>e.click());}
 for(const language of ['en','ko','ja','zh','es','fr','de','vi']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(language);await page.waitForTimeout(600);
  await visit('result');
  await page.locator('#save_result_collection_word_dialog').click();
  await page.locator('#result_document_contents').waitFor();await page.waitForTimeout(200);
  const inspect=()=>page.locator('#result_document_contents .checkbox-inline').evaluateAll(labels=>labels.map(e=>{let r=e.getBoundingClientRect();return{x:r.x,y:r.y,width:r.width};}));
  const rects=await inspect();assert.equal(rects.length,4);
  const same=(a,b)=>Math.abs(a-b)<1;
  if(rects.every(r=>same(r.y,rects[0].y))) console.log('PASS',language,'one row');
  else {
   assert.ok(same(rects[0].y,rects[1].y)&&same(rects[2].y,rects[3].y)&&rects[2].y>rects[0].y);
   assert.ok(same(rects[0].x,rects[2].x)&&same(rects[1].x,rects[3].x));
   console.log('PASS',language,'aligned 2 x 2');
  }
  assert.equal(await page.locator('#result_document_contents input:checked').count(),1);
  await page.locator('#result_document_select_all').click();await page.waitForFunction(()=>document.querySelectorAll('#result_document_contents input:checked').length===4);
  if(['en','ko'].includes(language)){
   fs.mkdirSync('tmp/document-contents-layout',{recursive:true});
   await page.locator('#shiny-modal .modal-content').screenshot({path:`tmp/document-contents-layout/${language}.png`});
  }
  if(language==='en'){
   await page.locator('#shiny-modal .modal-dialog').evaluate(e=>e.style.width='700px');await page.waitForTimeout(150);
   const wide=await inspect();assert.ok(wide.every(r=>same(r.y,wide[0].y)));
   await page.locator('#shiny-modal .modal-dialog').evaluate(e=>e.style.width='300px');await page.waitForTimeout(150);
   const narrow=await inspect();assert.ok(narrow[2].y>narrow[0].y&&same(narrow[0].x,narrow[2].x));
   console.log('PASS resize: one row when wide, 2 x 2 when narrow');
  }
  await page.locator('#shiny-modal [data-dismiss="modal"]').click();await page.locator('#result_document_contents').waitFor({state:'detached'});
  if(language==='ko'){
   await page.locator('#save_result_collection_hwpx_dialog').click();await page.locator('#result_document_contents').waitFor();
   await page.waitForTimeout(150);const hwpx=await inspect();assert.ok(hwpx.every(r=>same(r.y,hwpx[0].y)));
   await page.locator('#shiny-modal [data-dismiss="modal"]').click();await page.locator('#result_document_contents').waitFor({state:'detached'});
   console.log('PASS Korean HWPX modal shares alignment');
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
