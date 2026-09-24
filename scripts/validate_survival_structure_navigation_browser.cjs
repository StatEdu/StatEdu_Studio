const assert = require('node:assert/strict');
const {chromium} = require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async () => {
 const browser = await chromium.launch({channel:'chrome', headless:true});
 try {
  const page = await browser.newPage(), errors = [];
  page.on('pageerror', e => errors.push(e.message));
  await page.goto('http://127.0.0.1:43873/?lang=ko');
  await page.waitForFunction(() => window.Shiny?.shinyapp?.$socket?.readyState === 1 && document.querySelector('#file.shiny-bound-input') && !document.documentElement.classList.contains('shiny-busy'));
  await page.waitForTimeout(1200);
  await page.locator('#file').setInputFiles({name:'intervals.csv', mimeType:'text/csv', buffer:Buffer.from('time,status,entry,start,stop,id,x,group\n5,7,0,0,5,1,2,1\n7,9,1,5,7,1,3,1\n9,7,0,0,9,2,4,2\n10,9,2,9,10,2,5,2')});
  await page.locator('#apply_all_variable_selection').click();
  async function visit(id) {await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1200);}
  async function set(id,value) {
   await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else if(e.type==='checkbox'){e.checked=v;e.dispatchEvent(new Event('change',{bubbles:true}));}else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);
   await page.waitForTimeout(500);
  }
  for (const [type,shape] of [['km','entry_exit'],['cox','entry_exit'],['cox','start_stop']]) {
   await visit('analysis_survival_setup');await page.waitForTimeout(1500);
   const config = {
    survival_design_objective:type==='km'?'group_comparison':'association',survival_design_shape:shape,
    survival_design_events:'single',survival_design_time_dependent:shape==='start_stop',
    survival_contract_origin:'Review 사용자 <&> %s',survival_contract_unit:'day',survival_contract_event:'status',
    survival_contract_group:'group',survival_contract_covariates:['x'],
    ...(shape==='entry_exit'?{survival_contract_time:'time',survival_contract_entry:'entry'}:{survival_contract_start:'start',survival_contract_stop:'stop',survival_contract_subject_id:'id'})
   };
   for (const [id,value] of Object.entries(config)) await set(id,value);
   await set('survival_event_role_1','censored');await set('survival_event_role_2','event_of_interest');await set('survival_event_map_confirmed',true);
   await page.locator('#run_survival_design').click();await page.waitForTimeout(900);
   await page.locator('#open_recommended_survival_analysis').click();await page.waitForTimeout(1800);
   const inputs={data_shape:shape,event_value:'9',...(shape==='entry_exit'?{entry:'entry'}:{start:'start',stop:'stop',subject_id:'id'})};
   async function check(lang) {
    for(const [field,value] of Object.entries(inputs))assert.equal(await page.locator('#survival_'+type+'_'+field).inputValue(),value,`${lang}/${type}/${shape}/${field}`);
    for(const [field,value] of Object.entries({time:[shape==='start_stop'?'stop':'time'],event:['status'],[type==='km'?'group':'covariates']:[type==='km'?'group':'x']}))assert.deepEqual(await page.locator('#survival_'+type+'_'+field+' option').evaluateAll(es=>es.map(e=>e.value)),value,`${lang}/${type}/${shape}/${field}`);
   }
   await check('initial');
   // Verify user edits after the recommendation are not replaced by the transfer.
   if(shape==='entry_exit'){await set('survival_'+type+'_entry','start');inputs.entry='start';}
   else{await set('survival_cox_subject_id','group');inputs.subject_id='group';}
   for(const lang of ['ja','zh','es','fr','de','vi','en','ko']) {
    await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_'+type);
    await check(lang);console.log('PASS',lang,type,shape,'role transfer and subsequent edit retained');
   }
  }
  assert.deepEqual(errors,[]);
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
