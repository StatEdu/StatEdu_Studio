const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto(process.env.STATEDU_UI_TEST_URL||'http://127.0.0.1:43923/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 const rows=['id,time,y,x,aux,wt'];for(let i=0;i<30;i++)rows.push([Math.floor(i/3)+1,i%3,i+1,i+31,i+61,1+i/100].join(','));
 await page.locator('#file').setInputFiles({name:'longitudinal-navigation.csv',mimeType:'text/csv',buffer:Buffer.from(rows.join('\n'))});
 await page.locator('#apply_all_variable_selection').click();
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1000);}
 async function set(id,value){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(500);}
 async function tab(value){await page.locator(`#longitudinal_options_tab a[data-value="${value}"]`).click();await page.waitForTimeout(300);}
 await visit('Longitudinal / Panel Models');
 for(const [role,variable]of [['outcome','y'],['id','id'],['time','time'],['predictors','x']]){
  await page.locator(`[data-input-id="longitudinal_available"] [data-value="${variable}"]`).click();
  await page.locator(`#longitudinal_${role}_move`).click();
  await page.locator(`[data-input-id="longitudinal_${role}"] [data-value="${variable}"]`).waitFor();
 }
 await set('longitudinal_corstr','ar1');await tab('Missing');await set('longitudinal_missing_strategy','ipw');await set('longitudinal_ipw_auxiliary',['aux']);
 await tab('Weights');await set('longitudinal_weight_choice','wt');await set('longitudinal_weight_type','combined');await set('longitudinal_weight_trim','p05_95');await tab('Missing');
 console.log('BASELINE',await page.evaluate(()=>({trim:document.querySelector('#longitudinal_weight_trim')?.value,input:Shiny.shinyapp.$inputValues.longitudinal_weight_trim})));
 assert.equal(await page.locator('#longitudinal_weight_trim').inputValue(),'p05_95');
 await page.evaluate(()=>window.__longitudinalNavigation='same-session');
 for(const lang of ['ko','ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(900);
  await visit('Longitudinal / Panel Models');
  assert.equal(await page.evaluate(()=>window.__longitudinalNavigation),'same-session');
  for(const [role,variable]of [['outcome','y'],['id','id'],['time','time'],['predictors','x']])assert.deepEqual(await page.locator(`[data-input-id="longitudinal_${role}"] [data-value]`).evaluateAll(es=>es.map(e=>e.dataset.value)),[variable],lang+'/'+role);
  for(const [id,value]of Object.entries({corstr:'ar1',weight_choice:'wt',weight_type:'combined',weight_trim:'p05_95',missing_strategy:'ipw'}))assert.equal(await page.locator('#longitudinal_'+id).inputValue(),value,lang+'/'+id);
  assert.deepEqual(await page.locator('#longitudinal_ipw_auxiliary').evaluate(e=>e.selectize.getValue()),['aux']);
  assert.equal(await page.locator('#longitudinal_options_tab li.active a').getAttribute('data-value'),'Missing');
  const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
  assert.ok((await page.locator('#longitudinal_setup').textContent()).includes(dict['longitudinal.ui.independent_variables_count'].replace('%s','1')));
  console.log('PASS full-app navigation',lang,'roles, options, active tab and translated caption');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
