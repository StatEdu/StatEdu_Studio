const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try {
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43921');await page.waitForSelector('#longitudinal_model_type.shiny-bound-input');
 async function tab(value){await page.locator(`#longitudinal_options_tab a[data-value="${value}"]`).click();await page.waitForTimeout(200);}
 async function select(id,value){await page.locator('#'+id).selectOption(value);await page.waitForTimeout(450);}
 await tab('Missing');await select('longitudinal_missing_strategy','ipw');
 for(const weight of ['wt','']) {
  await tab('Weights');await select('longitudinal_weight_choice',weight);
  if(weight)await select('longitudinal_weight_type','combined');
  for(const auxiliary of [['aux','aux2'],['aux2'],[]]) {
   await tab('Missing');await page.locator('#longitudinal_ipw_auxiliary').evaluate((e,v)=>e.selectize.setValue(v),auxiliary);await page.waitForTimeout(450);
   const snapshot=JSON.parse(await page.locator('#test_settings').innerText());
   for(const lang of ['ja','zh','es','fr','de','vi','en','ko']) {
    await page.locator('#test_language').evaluate((e,v)=>e.selectize.setValue(v),lang);await page.waitForTimeout(600);
    assert.deepEqual(await page.locator('#longitudinal_ipw_auxiliary').evaluate(e=>e.selectize.getValue()),auxiliary);
    assert.equal(await page.locator('#longitudinal_weight_choice').inputValue(),weight);
    assert.equal(await page.locator('#longitudinal_options_tab li.active a').getAttribute('data-value'),'Missing');
    assert.deepEqual(JSON.parse(await page.locator('#test_settings').innerText()),snapshot);
    if(!weight) {
     assert.equal(await page.locator('#longitudinal_weight_type').inputValue(),'none');
     assert.equal(await page.locator('#longitudinal_weight_trim').count(),0);
     const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
     assert.ok((await page.locator('#longitudinal_setup').textContent()).includes(dict['longitudinal.help.empty']));
    }
    console.log('PASS clear/restore language',weight||'unweighted',auxiliary.join(',')||'no auxiliary',lang);
   }
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
