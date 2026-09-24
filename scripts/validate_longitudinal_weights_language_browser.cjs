const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try {
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43921');await page.waitForSelector('#longitudinal_model_type.shiny-bound-input');
 async function tab(value){await page.locator(`#longitudinal_options_tab a[data-value="${value}"]`).click();await page.waitForTimeout(200);}
 async function select(id,value){await page.locator('#'+id).selectOption(value);await page.waitForTimeout(450);}
 await tab('Weights');await select('longitudinal_weight_choice','wt');
 for(const type of ['sampling','longitudinal','combined'])for(const strategy of ['ipw','wgee']) {
  await tab('Weights');await select('longitudinal_weight_type',type);await select('longitudinal_weight_trim','p05_95');
  await tab('Missing');await select('longitudinal_missing_strategy',strategy);
  await page.locator('#longitudinal_ipw_auxiliary').evaluate(e=>e.selectize.setValue(['aux']));await page.waitForTimeout(400);
  const snapshot=JSON.parse(await page.locator('#test_settings').innerText());
  assert.equal(snapshot.options.weight_type,type);assert.equal(snapshot.options.missing_strategy,strategy);
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']) {
   await page.locator('#test_language').evaluate((e,value)=>e.selectize.setValue(value),lang);await page.waitForTimeout(650);
   assert.equal(await page.locator('#longitudinal_options_tab li.active a').getAttribute('data-value'),'Missing');
   for(const [id,value] of Object.entries({longitudinal_weight_choice:'wt',longitudinal_weight_type:type,longitudinal_weight_trim:'p05_95',longitudinal_missing_strategy:strategy}))assert.equal(await page.locator('#'+id).inputValue(),value);
   assert.deepEqual(await page.locator('#longitudinal_ipw_auxiliary').evaluate(e=>e.selectize.getValue()),['aux']);
   assert.deepEqual(JSON.parse(await page.locator('#test_settings').innerText()),snapshot,`${type}/${strategy}/${lang}`);
   const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
   assert.ok((await page.locator('body').innerText()).includes(dict['longitudinal.help.auxiliary']));
   console.log('PASS weighted language switch',type,strategy,lang);
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
