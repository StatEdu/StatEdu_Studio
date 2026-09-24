const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43875');await page.waitForSelector('#progress-fixture');
 const phases={starting:'Starting worker',preparing:'Preparing models',resampling:'Resampling',finalizing:'Computing bootstrap summaries',serializing:'Saving results',complete:'Complete'};
 for(const language of ['ja','zh','es','fr','de','vi','en','ko']){
  await page.locator('#language').evaluate((el,v)=>el.selectize.setValue(v),language);
  for(const [phase,source] of Object.entries(phases)){
   await page.locator('#phase').evaluate((el,v)=>el.selectize.setValue(v),phase);
   await page.waitForSelector(`#progress-fixture[data-language="${language}"][data-phase="${phase}"]`);
   const root=page.locator('#progress-fixture'),text=await root.innerText();
   if(!['en','ko'].includes(language)){
    const dictionary=JSON.parse(fs.readFileSync(`i18n/${language}.json`,'utf8')).translations;
    const displayedSource=phase==='complete'?'Analysis complete; preparing the result view':source;
    assert.ok(text.includes(dictionary['analysis.ui.'+displayedSource.toLowerCase().replace(/[^a-z0-9]+/g,'_')]), language+' '+phase+' '+JSON.stringify(text));
    assert.ok(!text.includes('Custom mediation / moderation bootstrap progress'));
   }
   if(phase==='resampling')assert.ok(text.includes('Review 사용자 <&>')&&text.includes('500/1,000'));
   assert.equal(await root.locator('#custom_model_canvas_bootstrap_stop').count(),1);
  }
  console.log('PASS:',language,'six browser phases, actual card rendering, literal focal variable');
 }
 await page.locator('#custom_model_canvas_bootstrap_stop').click();
 await page.waitForFunction(()=>Number(document.querySelector('#stop_count').textContent)>=1);
 assert.deepEqual(errors,[]);console.log('PASS: stop event delivered; no browser script errors');
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});

