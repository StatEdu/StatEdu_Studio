const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try {
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43921');await page.waitForSelector('#longitudinal_model_type.shiny-bound-input');
 async function tab(value){await page.locator(`#longitudinal_options_tab a[data-value="${value}"]`).click();await page.waitForTimeout(250);}
 await tab('Model');await page.locator('#longitudinal_model_type').selectOption('lmm');await page.waitForTimeout(600);
 for(const structure of ['exchangeable','reml_un','reml_ar1']) {
  await tab('Model');await page.locator('#longitudinal_corstr').selectOption(structure);await page.waitForTimeout(500);
  await page.locator('#longitudinal_include_time').uncheck();
  if(structure==='exchangeable')await page.locator('#longitudinal_random_slope').check();
  await tab('Checks');await page.locator('#longitudinal_check_residual_normality').uncheck();await page.waitForTimeout(500);
  const snapshot=JSON.parse(await page.locator('#test_settings').innerText());
  assert.equal(snapshot.options.corstr,structure);
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']) {
   await page.locator('#test_language').evaluate((e,value)=>e.selectize.setValue(value),lang);await page.waitForTimeout(600);
   assert.equal(await page.locator('#longitudinal_options_tab li.active a').getAttribute('data-value'),'Checks');
   assert.equal(await page.locator('#longitudinal_check_residual_normality').isChecked(),false);
   assert.equal(await page.locator('#longitudinal_include_time').isChecked(),false);
   assert.equal(await page.locator('#longitudinal_corstr').inputValue(),structure);
   if(structure==='exchangeable')assert.equal(await page.locator('#longitudinal_random_slope').isChecked(),true);
   else assert.equal(await page.locator('#longitudinal_random_slope').count(),0);
   assert.deepEqual(JSON.parse(await page.locator('#test_settings').innerText()),snapshot,`${structure}/${lang}`);
   const key={exchangeable:'random_effects_ml',reml_un:'repeated_un_reml',reml_ar1:'repeated_ar_1_reml'}[structure];
   const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
   assert.equal(await page.locator('#longitudinal_corstr option:checked').textContent(),dict['longitudinal.choice.'+key]);
   console.log('PASS LMM language switch',structure,lang);
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
