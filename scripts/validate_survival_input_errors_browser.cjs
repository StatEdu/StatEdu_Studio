const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const cases=JSON.parse(fs.readFileSync('tmp/survival-input-errors.json','utf8'));
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);await page.locator('#file').setInputFiles('tmp/survival-input-errors.csv');await page.locator('#apply_all_variable_selection').click();
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1000);}
 async function set(id,value){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else if(e.type==='checkbox'){e.checked=v;e.dispatchEvent(new Event('change',{bubbles:true}));}else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(350);}
 await visit('analysis_survival_setup');await page.waitForTimeout(1500);
 for(const [name,test]of Object.entries(cases)){
  const c=test.config;
  for(const [id,value]of Object.entries({survival_design_objective:c.objective,survival_design_shape:c.data_shape,survival_design_events:c.event_structure,survival_contract_origin:c.time_origin,survival_contract_unit:c.time_unit,survival_contract_event:c.event}))await set(id,value);
  if(c.data_shape==='start_stop')for(const field of ['start','stop','subject_id'])await set('survival_contract_'+field,c[field]);
  else await set('survival_contract_time',c.time);
  if(c.data_shape==='entry_exit')await set('survival_contract_entry',c.entry);
  if(c.event){await set('survival_event_role_3',c.role3);await set('survival_event_label_3','Review 사용자 <&> %s');await set('survival_event_map_confirmed',c.event_map_confirmed);}
  await page.locator('#run_survival_design').click();await page.waitForTimeout(800);
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
   await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_setup');
   const text=await page.locator('#survival_design_recommendation').innerText();
   assert.ok(text.includes(test.languages[lang]),`${name}/${lang}: missing ${test.languages[lang]}; actual=${text}`);
   assert.equal(await page.locator('#open_recommended_survival_analysis').count(),0,`${name}/${lang}/no navigation`);
   if(c.event)assert.equal(await page.locator('#survival_event_label_3').inputValue(),'Review 사용자 <&> %s');
   console.log('PASS',name,lang,'translated input error and blocked navigation');
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
