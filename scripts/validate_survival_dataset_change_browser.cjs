const assert=require('node:assert/strict');const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));await page.waitForTimeout(1200);
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1000);}
 async function set(id,v){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else if(e.type==='checkbox'){e.checked=v;e.dispatchEvent(new Event('change',{bubbles:true}));}else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},v);await page.waitForTimeout(400);}
 async function upload(csv){if(!await page.locator('#file').count()){await page.locator('#go_step1').click();await page.waitForTimeout(700);}await page.locator('#file').setInputFiles({name:'same-name.csv',mimeType:'text/csv',buffer:Buffer.from(csv)});await page.waitForTimeout(1200);await page.locator('#apply_all_variable_selection').click();await visit('analysis_survival_setup');await page.waitForTimeout(1500);}
 await upload('time,event,x\n1,0,2\n2,1,3\n3,2,4\n4,1,5');
 for(const [id,v]of Object.entries({survival_contract_origin:'Review 사용자 <&> %s',survival_contract_unit:'day',survival_contract_time:'time',survival_contract_event:'event',survival_event_role_3:'censored',survival_event_label_3:'Old label Review',survival_event_map_confirmed:true}))await set(id,v);
 await page.locator('#run_survival_design').click();await page.waitForTimeout(800);await page.locator('#open_recommended_survival_analysis').waitFor();
 await visit('about_preferences');await page.locator('#app_language').selectOption('ja');await visit('analysis_survival_setup');assert.equal(await page.locator('#survival_event_map_confirmed').isChecked(),true);
 await page.locator('.navbar-nav a[data-value]').first().evaluate(e=>e.click());await page.waitForTimeout(1000);await upload('time,event,x\n11,0,2\n12,1,3\n13,2,4\n14,1,5');
 assert.equal(await page.locator('#open_recommended_survival_analysis').count(),0,'old recommendation cleared');
 await set('survival_contract_event','event');
 assert.equal(await page.locator('#survival_event_map_confirmed').isChecked(),false,'same columns/codes in different data must require reconfirmation');
 assert.equal(await page.locator('#survival_event_role_3').inputValue(),'unknown');assert.equal(await page.locator('#survival_event_label_3').inputValue(),'2');
 for(const lang of ['zh','es','fr','de','vi','en','ko','ja']){await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('analysis_survival_setup');assert.equal(await page.locator('#survival_event_map_confirmed').isChecked(),false);console.log('PASS',lang,'new dataset mapping reset and old recommendation cleared');}
 for(const [id,v]of Object.entries({survival_contract_origin:'Review 사용자 <&> %s',survival_contract_unit:'day',survival_contract_time:'time',survival_event_role_3:'censored',survival_event_map_confirmed:true}))await set(id,v);await page.locator('#run_survival_design').click();await page.locator('#open_recommended_survival_analysis').waitFor();console.log('PASS new data can be confirmed and recommended');assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
