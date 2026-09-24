const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const cases=JSON.parse(fs.readFileSync('tmp/survival-restriction-branches.json','utf8'));
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);
 await page.locator('#file').setInputFiles({name:'restrictions.csv',mimeType:'text/csv',buffer:Buffer.from('time,event,entry,start,stop,id,x\n5,0,0,0,5,1,2\n7,1,1,0,7,2,3\n9,2,0,0,9,3,4\n10,1,2,0,10,4,5')});await page.locator('#apply_all_variable_selection').click();
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1000);}
 async function set(id,value){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else if(e.type==='checkbox'){e.checked=v;e.dispatchEvent(new Event('change',{bubbles:true}));}else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(350);}
 await visit('analysis_survival_setup');await page.waitForTimeout(1500);
 for(const [name,test]of Object.entries(cases)){
  const c=test.config;
  for(const [id,value]of Object.entries({survival_design_objective:c.objective,survival_design_shape:c.data_shape,survival_design_events:c.event_structure,survival_design_time_dependent:c.time_dependent,survival_contract_origin:'Review 사용자 <&> %s',survival_contract_unit:'day',survival_contract_event:'event'}))await set(id,value);
  if(c.data_shape==='start_stop'){
   for(const [field,value]of Object.entries({start:'start',stop:'stop',subject_id:'id'}))await set('survival_contract_'+field,value);
  }else if(c.data_shape!=='interval_censored')await set('survival_contract_time','time');
  if(c.data_shape==='entry_exit')await set('survival_contract_entry','entry');
  await set('survival_event_role_3',c.event_structure==='competing'?'competing_event':'censored');
  await set('survival_event_map_confirmed',true);
  await page.locator('#run_survival_design').click();await page.waitForTimeout(800);
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
   await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_setup');
   const text=await page.locator('#survival_design_recommendation').innerText();
   for(const phrase of test.languages[lang])assert.ok(text.includes(phrase),`${name}/${lang}: missing ${phrase}; actual=${text}`);
   assert.equal(await page.locator('#open_recommended_survival_analysis').count(),0,`${name}/${lang}/no navigation`);
   assert.equal(await page.locator('#survival_contract_origin').inputValue(),'Review 사용자 <&> %s');
   console.log('PASS',name,lang,'restriction translated, navigation absent, user text retained');
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
