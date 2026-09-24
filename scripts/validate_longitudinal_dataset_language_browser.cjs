const assert=require('node:assert/strict');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43921');await page.waitForSelector('#longitudinal_model_type.shiny-bound-input');
 async function choose(id,value){await page.locator('#'+id).evaluate((e,v)=>e.selectize?e.selectize.setValue(v):(e.value=v,e.dispatchEvent(new Event('change',{bubbles:true}))),value);await page.waitForTimeout(700);}
 await page.locator('#longitudinal_options_tab a[data-value="Missing"]').click();
 await choose('longitudinal_missing_strategy','ipw');await choose('longitudinal_ipw_auxiliary',['aux','aux2']);
 await page.locator('#longitudinal_options_tab a[data-value="Weights"]').click();await choose('longitudinal_weight_choice','wt');
 for(const dataset of ['reduced','empty','full']){
  await choose('test_dataset',dataset);
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
   await choose('test_language',lang);
   const snapshot=JSON.parse(await page.locator('#test_settings').innerText());
   console.log(dataset,lang,JSON.stringify(snapshot.variables),JSON.stringify(snapshot.options.ipw_auxiliary));
   const asArray=v=>Array.isArray(v)?v:(v?[v]:[]);
   assert.deepEqual(asArray(snapshot.variables.predictors),[]);
   assert.deepEqual(asArray(snapshot.variables.weight),[]);
   assert.deepEqual(asArray(snapshot.options.ipw_auxiliary),dataset==='reduced'?['aux2']:[]);
   for(const key of ['outcome','id','time'])assert.deepEqual(asArray(snapshot.variables[key]),dataset==='reduced'?[{outcome:'y',id:'id',time:'time'}[key]]:[]);
   if(dataset==='empty')assert.equal(await page.locator('#longitudinal_model_type').count(),0);
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
