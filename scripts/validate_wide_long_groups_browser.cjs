const assert=require('node:assert/strict');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input'));
 const csv='id,x1,x2,x3,x4,y1,y2,y3,y4\n1,11,12,13,14,101,102,103,104\n2,21,22,23,24,201,202,203,204';
 await page.locator('#file').setInputFiles({name:'groups.csv',mimeType:'text/csv',buffer:Buffer.from(csv)});await page.locator('#apply_all_variable_selection').click();
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(800);}
 async function set(id,value){await page.locator('#'+id).evaluate((e,v)=>{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));},value);await page.waitForTimeout(300);}
 async function add(prefix,label){
  for(let i=1;i<=4;i++){await page.locator(`[data-input-id="wide_long_available"] [data-value="${prefix}${i}"]`).dblclick();await page.waitForTimeout(350);}
  await page.locator('#wide_long_unit_type input[value="same"]').check();
  await set('wide_long_group_count','2');await set('wide_long_time_count','2');await set('wide_long_group_name','사용자_집단');await set('wide_long_time_name','사용자_시점');await set('wide_long_value_name',label);
  await page.locator('#wide_long_set_spec').click();await page.waitForTimeout(650);
 }
 async function names(){return page.locator('[data-input-id="wide_long_configured"] .analysis-transfer-option').allTextContents();}
 async function selectB(){await page.locator('[data-input-id="wide_long_configured"] .analysis-transfer-option').filter({hasText:'측정B'}).click();await page.waitForTimeout(200);}
 async function verify(order){
  await page.locator('#preview_wide_long').click();await page.waitForTimeout(500);
  const result=await page.locator('#wide_long_preview table.dataTable[id]').evaluate(e=>{const api=window.jQuery(e).DataTable();return {rows:api.rows().data().toArray(),columns:api.columns().header().toArray().map(h=>h.textContent)};});
  assert.deepEqual(result.columns,['id','사용자_집단','사용자_시점',...order]);
  const expected=[];for(let id=1;id<=2;id++)for(let group=1;group<=2;group++)for(let time=1;time<=2;time++){
   const k=(group-1)*2+time;expected.push([id,group,time,...order.map(v=>v==='측정A'?id*10+k:id*100+k)]);
  }
  assert.deepEqual(result.rows.map(r=>JSON.stringify(r.map(Number))).sort(),expected.map(r=>JSON.stringify(r)).sort());
 }
 await visit('data_editor_wide_long');await add('x','측정A');await add('y','측정B');
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_wide_long');
  assert.deepEqual((await names()).map(v=>v.trim()),['측정A (2,2)','측정B (2,2)']);
  await selectB();await page.locator('#wide_long_configured_up').click();await page.waitForTimeout(400);await verify(['측정B','측정A']);
  await selectB();await page.locator('#wide_long_configured_down').click();await page.waitForTimeout(400);await verify(['측정A','측정B']);
  await selectB();await page.locator('#wide_long_remove_spec').click();await page.waitForTimeout(500);
  assert.equal(await page.locator('[data-input-id="wide_long_configured"] .analysis-transfer-option').count(),1);
  for(let i=1;i<=4;i++)assert.equal(await page.locator(`[data-input-id="wide_long_available"] [data-value="y${i}"]`).count(),1);
  await add('y','측정B');await verify(['측정A','측정B']);
  console.log('PASS',lang,'two same-object groups: reorder, delete, recreate and exact group/time values');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
