const assert=require('node:assert/strict');const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));await page.goto('http://127.0.0.1:43874');await page.locator('#survival_design_objective').waitFor({state:'attached'});await page.waitForTimeout(1200);
const fields={survival_design_objective:'competing',survival_design_events:'competing',survival_design_estimand:'both',survival_contract_origin:'Review 수술일 <&> %s',survival_contract_unit:'other',survival_contract_custom_unit:'사용자 주기 <&> %s',survival_contract_time:'time',survival_contract_event:'event',survival_event_role_3:'competing_event',survival_event_label_3:'사용자 Review <&> %s'};
async function check(){for(const [id,v]of Object.entries(fields))assert.equal(await page.locator('#'+id).inputValue(),v,id);assert.equal(await page.locator('#survival_event_map_confirmed').isChecked(),true);}
await page.locator('#restore').click();await page.waitForTimeout(1800);await check();
for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
 await page.locator('#language').evaluate((e,v)=>e.selectize.setValue(v),lang);await page.waitForTimeout(800);await check();
 await page.locator('#survival_contract_origin').fill('edited');await page.locator('#survival_contract_origin').dispatchEvent('change');await page.waitForTimeout(350);
 await page.locator('#restore').click();await page.waitForTimeout(1000);await check();console.log('PASS',lang,'saved design restored in rendered inputs and retained across language change');
}
await page.locator('#change_data').click();await page.waitForTimeout(900);assert.equal(await page.locator('#survival_event_map_confirmed').isChecked(),false);
await page.locator('#restore').click();await page.waitForTimeout(1200);assert.equal(await page.locator('#survival_event_map_confirmed').isChecked(),false);assert.equal(await page.locator('#survival_event_role_3').inputValue(),'unknown');console.log('PASS changed data rejects saved mapping confirmation');assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
