const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1000);
 await page.locator('#file').setInputFiles({name:'survival.csv',mimeType:'text/csv',buffer:Buffer.from('time,event,entry,x\n5,1,0,2\n7,0,1,3\n9,2,0,4\n10,1,2,5')});
 await page.locator('#apply_all_variable_selection').click();
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(800);}
 async function set(id,value){await page.locator('#'+id).evaluate((el,value)=>{if(el.selectize)el.selectize.setValue(value);else{el.value=value;el.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(350);}
 const configs={km:{data_shape:'entry_exit',entry:'entry',rate_times:'12, 36',rmst_tau:'365'},cox:{data_shape:'entry_exit',entry:'entry',ties_method:'exact',spline_df:'5',time_varying_times:'1, 5, 10'},competing:{regression:'both',censoring_group:'x',event_values:'2, 3'}};
 // Input IDs whose suffix differs from saved option names are kept explicit.
 for(const [type,settings] of Object.entries(configs)){
  await visit('analysis_survival_'+type);
  await page.waitForTimeout(1800);
  for(const [field,value] of Object.entries(settings))await set('survival_'+type+'_'+field,value);
  for(const [field,value] of Object.entries(settings))assert.equal(await page.locator('#survival_'+type+'_'+field).inputValue(),value,`initial/${type}/${field}`);
 }
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(700);
  for(const [type,settings] of Object.entries(configs)){
   await visit('analysis_survival_'+type);
   for(const [field,value] of Object.entries(settings))assert.equal(await page.locator('#survival_'+type+'_'+field).inputValue(),value,`${lang}/${type}/${field}`);
   if(type==='km')await page.locator('#survival_km_option_tabs a[data-value="analysis"]').click();
   if(type==='cox')await page.locator('#survival_cox_option_tabs a[data-value="structure"]').click();
   const target=type==='competing'?'survival_competing_censoring_group':'survival_'+type+'_entry';
   assert.equal(await page.locator('#'+target).evaluate(el=>!!el.closest('.shiny-input-container')?.getClientRects().length),true,`${lang}/${type}/conditional`);
   if(lang!=='en'){
    const key=type==='competing'?'regression_estimand':type==='cox'?'cox_data_structure':'entry_time';
    const id=type==='competing'?'survival_competing_regression':type==='cox'?'survival_cox_data_shape':'survival_km_entry';
    const expected=lang==='ko'?{regression_estimand:'회귀 추정량',cox_data_structure:'Cox 자료 구조',entry_time:'진입 시간'}[key]:JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations['analysis.ui.'+key];
    assert.equal(await page.locator(`#${id}-label`).textContent(),expected);
   }
   console.log('PASS',lang,type,'session option restoration, translated label, visible conditional option');
  }
 }
 await visit('analysis_survival_cox');await set('survival_cox_data_shape','start_stop');
 assert.equal(await page.locator('#survival_cox_entry').evaluate(el=>!!el.closest('.shiny-input-container').getClientRects().length),false);
 assert.equal(await page.locator('#survival_cox_start').evaluate(el=>!!el.closest('.shiny-input-container').getClientRects().length),true);
 await visit('analysis_survival_competing');await set('survival_competing_regression','none');
 assert.equal(await page.locator('#survival_competing_censoring_group').evaluate(el=>!!el.closest('.shiny-input-container').getClientRects().length),false);
 assert.deepEqual(errors,[]);console.log('PASS conditional hide/show; no page errors');
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
