const assert=require('node:assert/strict');const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
await page.goto('http://127.0.0.1:43873/?lang=ko');
await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
await page.waitForTimeout(1200);
await page.locator('#file').setInputFiles({name:'design.csv',mimeType:'text/csv',buffer:Buffer.from('time,event,x\n1,0,2\n2,1,3\n3,2,4\n4,1,5')});await page.locator('#apply_all_variable_selection').click();
async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1200);}
async function set(id,value){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else if(e.type==='checkbox'){e.checked=v;e.dispatchEvent(new Event('change',{bubbles:true}));}else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(500);}
await visit('analysis_survival_setup');await page.waitForTimeout(1500);
const fields={survival_design_objective:'competing',survival_design_events:'competing',survival_design_estimand:'both',survival_contract_origin:'수술일 Review <&> %s',survival_contract_unit:'day',survival_contract_time:'time',survival_contract_event:'event'};
for(const [id,v]of Object.entries(fields))await set(id,v);
await set('survival_event_role_3','competing_event');await set('survival_event_label_3','사용자 Normality <&> %s');await set('survival_event_map_confirmed',true);
Object.assign(fields,{survival_event_role_3:'competing_event',survival_event_label_3:'사용자 Normality <&> %s',survival_event_map_confirmed:true});
async function check(lang){for(const [id,v]of Object.entries(fields)){const actual=await page.locator('#'+id).evaluate(e=>e.type==='checkbox'?e.checked:e.value);assert.equal(actual,v,lang+'/'+id);}}
await check('initial');await page.locator('#run_survival_design').click();await page.waitForTimeout(900);await page.locator('#open_recommended_survival_analysis').waitFor();
for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_setup');await check(lang);await page.locator('#open_recommended_survival_analysis').waitFor();console.log('PASS',lang,'design inputs, event mapping, user text, confirmation and recommendation retained');}
await set('survival_contract_event','x');await page.waitForTimeout(800);
assert.equal(await page.locator('#survival_event_map_confirmed').isChecked(),false);
assert.equal(await page.locator('#survival_event_label_3').inputValue(),'4');
assert.equal(await page.locator('#survival_event_role_3').inputValue(),'unknown');
console.log('PASS changed event variable resets mapping and confirmation');
assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
