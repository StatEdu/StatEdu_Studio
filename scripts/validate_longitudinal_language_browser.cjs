const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try {
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43921');
 await page.waitForSelector('#longitudinal_model_type.shiny-bound-input');
 async function tab(value){await page.locator(`#longitudinal_options_tab a[data-value="${value}"]`).click();await page.waitForTimeout(200);}
 async function number(id,value){await page.locator('#'+id).fill(value);await page.locator('#'+id).press('Tab');await page.waitForTimeout(300);}
 await tab('Model');await page.locator('#longitudinal_corstr').selectOption('ar1');await page.waitForTimeout(500);
 await tab('Missing');await number('longitudinal_missing_imputations','9');await number('longitudinal_missing_iterations','7');
 await page.locator('#longitudinal_mi_outcome').selectOption('impute');await page.waitForTimeout(500);
 const snapshot=JSON.parse(await page.locator('#test_settings').innerText());
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']) {
  await page.locator('#test_language').evaluate((e,value)=>e.selectize.setValue(value),lang);await page.waitForTimeout(700);
  assert.equal(await page.locator('#longitudinal_options_tab li.active a').getAttribute('data-value'),'Missing');
  assert.equal(await page.locator('#longitudinal_missing_imputations').inputValue(),'9');
  assert.equal(await page.locator('#longitudinal_missing_iterations').inputValue(),'7');
  assert.equal(await page.locator('#longitudinal_mi_outcome').inputValue(),'impute');
  assert.deepEqual(JSON.parse(await page.locator('#test_settings').innerText()),snapshot,lang);
  const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
  assert.ok((await page.locator('body').innerText()).includes(dict['longitudinal.ui.independent_variables_count'].replace('%s','1')));
  console.log('PASS language switch',lang,'MI values, active tab, model settings and variable assignments');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
