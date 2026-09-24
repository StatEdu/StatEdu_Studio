const assert=require('node:assert/strict');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input'));
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(800);}
 async function upload(offset){
  await page.locator('.navbar-nav a[data-value]').first().evaluate(e=>e.click());await page.waitForTimeout(400);
  if(!await page.locator('#file').count()){await page.locator('#go_step1').click();await page.waitForTimeout(500);}
  await page.locator('#file').setInputFiles({name:'same.csv',mimeType:'text/csv',buffer:Buffer.from(`id,x1,x2\n1,${10+offset},${30+offset}\n2,${20+offset},${40+offset}`)});
  await page.locator('#apply_all_variable_selection').click();await visit('data_editor_wide_long');
 }
 async function set(id,v){await page.locator('#'+id).evaluate((e,v)=>{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));},v);await page.waitForTimeout(250);}
 async function configure(offset){
  for(const v of ['x1','x2']){await page.locator(`[data-input-id="wide_long_available"] [data-value="${v}"]`).dblclick();await page.waitForTimeout(350);}
  await set('wide_long_value_name','사용자_값');await set('wide_long_index_name','시점');await set('wide_long_index_values','Normality,morning');
  await page.locator('#wide_long_set_spec').click();await page.waitForTimeout(500);await page.locator('#preview_wide_long').click();await page.waitForTimeout(500);
  const rows=await page.locator('#wide_long_preview table.dataTable[id]').evaluate(e=>window.jQuery(e).DataTable().rows().data().toArray());
  assert.deepEqual(rows.map(r=>JSON.stringify([Number(r[0]),r[1],Number(r[2])])).sort(),[[1,'Normality',10+offset],[2,'Normality',20+offset],[1,'morning',30+offset],[2,'morning',40+offset]].map(r=>JSON.stringify(r)).sort());
 }
 await upload(0);await configure(0);
 for(const [i,lang] of ['ja','zh','es','fr','de','vi','en','ko'].entries()){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_wide_long');
  assert.equal(await page.locator('[data-input-id="wide_long_configured"] .analysis-transfer-option').count(),1);
  await upload((i+1)*100);
  assert.equal(await page.locator('[data-input-id="wide_long_configured"] .analysis-transfer-option').count(),0);
  assert.equal((await page.locator('#wide_long_message').innerText()).trim(),'');
  assert.equal(await page.locator('#wide_long_preview table.dataTable[id]').count(),0);
  await configure((i+1)*100);
  console.log('PASS',lang,'language preserves group; same-name replacement clears stale group/result; new preview uses new values');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
