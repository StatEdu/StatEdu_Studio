const assert=require('node:assert/strict');const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
await page.goto('http://127.0.0.1:43873/?lang=ko');
await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
await page.waitForTimeout(1200);
await page.locator('#file').setInputFiles({name:'design.csv',mimeType:'text/csv',buffer:Buffer.from('time,event,x,status,group\n1,0,2,7,1\n2,1,3,9,2\n3,2,4,9,1\n4,1,5,7,2')});await page.locator('#apply_all_variable_selection').click();
async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1200);}
async function set(id,value){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else if(e.type==='checkbox'){e.checked=v;e.dispatchEvent(new Event('change',{bubbles:true}));}else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(500);}
await visit('analysis_survival_setup');await page.waitForTimeout(1500);
const fields={survival_design_objective:'competing',survival_design_events:'competing',survival_design_estimand:'both',survival_contract_origin:'수술일 Review <&> %s',survival_contract_unit:'day',survival_contract_time:'time',survival_contract_event:'event'};
for(const [id,v]of Object.entries(fields))await set(id,v);
await set('survival_event_role_3','competing_event');await set('survival_event_label_3','사용자 Normality <&> %s');await set('survival_event_map_confirmed',true);
Object.assign(fields,{survival_event_role_3:'competing_event',survival_event_label_3:'사용자 Normality <&> %s',survival_event_map_confirmed:true});
async function check(lang){for(const [id,v]of Object.entries(fields)){const actual=await page.locator('#'+id).evaluate(e=>e.type==='checkbox'?e.checked:e.value);assert.equal(actual,v,lang+'/'+id);}}
await check('initial');await page.locator('#run_survival_design').click();await page.waitForTimeout(900);await page.locator('#open_recommended_survival_analysis').waitFor();
await page.locator('#open_recommended_survival_analysis').click();await page.waitForTimeout(1800);
for(const [field,value]of Object.entries({time:'time',event:'event',regression:'both',event_values:'2'}))assert.equal(await page.locator('#survival_competing_'+field).inputValue(),value,'transfer/'+field);
await set('survival_competing_regression','fine_gray');
await set('survival_competing_event_values','2, 3');
for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
 await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_competing');
 for(const [field,value]of Object.entries({time:'time',event:'event',regression:'fine_gray',event_values:'2, 3'}))assert.equal(await page.locator('#survival_competing_'+field).inputValue(),value,lang+'/'+field);
 console.log('PASS',lang,'recommendation transfer followed by user edits retained');
}
for(const [type,objective]of [['km','group_comparison'],['cox','association']]){
 await visit('analysis_survival_setup');
 for(const [id,value]of Object.entries({survival_design_objective:objective,survival_design_events:'single',survival_contract_event:'status',survival_contract_group:'group',survival_contract_covariates:['x']}))await set(id,value);
 await set('survival_event_role_1','censored');await set('survival_event_role_2','event_of_interest');await set('survival_event_map_confirmed',true);
 await page.locator('#run_survival_design').click();await page.waitForTimeout(900);
 await page.locator('#open_recommended_survival_analysis').click();await page.waitForTimeout(1800);
 const expected={time:['time'],event:['status'],[type==='km'?'group':'covariates']:[type==='km'?'group':'x']};
 async function checkTarget(lang){
  for(const [field,value]of Object.entries(expected))assert.deepEqual(await page.locator('#survival_'+type+'_'+field+' option').evaluateAll(es=>es.map(e=>e.value)),value,lang+'/'+type+'/'+field);
  assert.equal(await page.locator('#survival_'+type+'_event_value').inputValue(),'9',lang+'/'+type+'/event_value');
 }
 await checkTarget('initial');
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_'+type);await checkTarget(lang);
  console.log('PASS',lang,type,'recommendation role assignments and custom event code');
 }
}
assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
