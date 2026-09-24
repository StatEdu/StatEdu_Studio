const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const fixture=process.env.STATEDU_RECOVERY_FIXTURE||'tmp/survival-error-recovery';
 const cases=JSON.parse(fs.readFileSync(fixture+'.json','utf8'));
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);await page.locator('#file').setInputFiles(fixture+'.csv');await page.locator('#apply_all_variable_selection').click();
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(900);}
 async function set(id,value){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else if(e.type==='checkbox'){e.checked=v;e.dispatchEvent(new Event('change',{bubbles:true}));}else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(300);}
 async function run(){await page.locator('#run_survival_design').click();await page.waitForTimeout(700);}
 await visit('analysis_survival_setup');await page.waitForTimeout(1500);
 for(const [name,test]of Object.entries(cases)){
  for(const [id,value]of Object.entries(test.config)){
   if(id==='survival_contract_time'&&test.config.survival_design_shape==='start_stop')continue;
   await set(id,value);
  }
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
   await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_setup');
   if(lang!=='ja')for(const [id,value]of Object.entries(test.preserve||{}))assert.equal(await page.locator('#'+id).inputValue(),value,`${name}/${lang}/preserve user text`);
   for(const id of Object.keys(test.fix))await set(id,test.config[id]);
   await run();
   const text=await page.locator('#survival_design_recommendation').innerText();
   assert.ok(text.includes(test.languages[lang]),`${name}/${lang}: missing ${test.languages[lang]}; actual=${text}`);
   assert.equal(await page.locator('#open_recommended_survival_analysis').count(),0,`${name}/${lang}/blocked`);
   for(const [id,value]of Object.entries(test.fix))await set(id,value);
   await run();await page.locator('#open_recommended_survival_analysis').waitFor({state:'visible'});
   const recovered=await page.locator('#survival_design_recommendation').innerText();
   assert.ok(!recovered.includes(test.languages[lang]),`${name}/${lang}/stale error after correction`);
   console.log('PASS',name,lang,'translated error; corrected input restores recommendation and navigation');
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
