const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const expected=JSON.parse(fs.readFileSync('tmp/survival-remaining-recommendations.json','utf8'));
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);
 await page.locator('#file').setInputFiles({name:'competing.csv',mimeType:'text/csv',buffer:Buffer.from('time,event,x,group\n1,0,2,1\n2,1,3,2\n3,2,4,1\n4,1,5,2')});await page.locator('#apply_all_variable_selection').click();
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1000);}
 async function set(id,value){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else if(e.type==='checkbox'){e.checked=v;e.dispatchEvent(new Event('change',{bubbles:true}));}else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(350);}
 await visit('analysis_survival_setup');await page.waitForTimeout(1500);
 for(const [id,value]of Object.entries({survival_design_objective:'competing',survival_design_events:'competing',survival_contract_origin:'Review 사용자 <&> %s',survival_contract_unit:'day',survival_contract_time:'time',survival_contract_event:'event',survival_contract_group:'group',survival_contract_covariates:['x']}))await set(id,value);
 await set('survival_event_role_3','competing_event');await set('survival_event_map_confirmed',true);
 const scenarios=[['cause_specific','competing','cause_specific','cause_specific'],['fine_gray','competing','cumulative_incidence','fine_gray'],['gray','group_comparison','both','none'],['confirmation','competing','',null],['unsupported','prediction','both',null]];
 for(const [name,objective,estimand,regression]of scenarios){
  await visit('analysis_survival_setup');await set('survival_design_objective',objective);await set('survival_design_estimand',estimand);
  await page.locator('#run_survival_design').click();await page.waitForTimeout(800);
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
   await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_setup');
   const text=await page.locator('#survival_design_recommendation').innerText();
   for(const phrase of expected[name].languages[lang])assert.ok(text.includes(phrase),`${name}/${lang}: missing ${phrase}`);
   assert.equal(await page.locator('#open_recommended_survival_analysis').count(),regression===null?0:1,`${name}/${lang}/navigation`);
   console.log('PASS',name,lang,'translated recommendation and navigation eligibility');
  }
  if(regression!==null){await page.locator('#open_recommended_survival_analysis').click();await page.waitForTimeout(1400);
   for(const [field,value]of Object.entries({regression,time:'time',event:'event',group:'group',event_values:'2'}))assert.equal(await page.locator('#survival_competing_'+field).inputValue(),value,`${name}/${field}`);
   assert.deepEqual(await page.locator('#survival_competing_covariates').evaluate(e=>e.selectize.getValue()),['x']);
   console.log('PASS',name,'actual target options and variables');
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
