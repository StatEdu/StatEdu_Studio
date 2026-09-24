const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const templates=JSON.parse(fs.readFileSync('tmp/wide-long-group-update-templates.json','utf8'));
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input'));
 await page.locator('#file').setInputFiles({name:'update.csv',mimeType:'text/csv',buffer:Buffer.from('id,x1,x2,y1,y2\n1,10,30,110,130\n2,20,40,120,140')});await page.locator('#apply_all_variable_selection').click();
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(800);}
 async function set(id,v){await page.locator('#'+id).evaluate((e,v)=>{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));},v);await page.waitForTimeout(250);}
 async function configure(prefix){
  for(let i=1;i<=2;i++){await page.locator(`[data-input-id="wide_long_available"] [data-value="${prefix}${i}"]`).dblclick();await page.waitForTimeout(350);}
  await set('wide_long_value_name','사용자_값');await set('wide_long_index_name','시점');await set('wide_long_index_values','Normality,morning');await page.locator('#wide_long_set_spec').click();await page.waitForTimeout(550);
 }
 const group=()=>page.locator('[data-input-id="wide_long_configured"] .analysis-transfer-option');
 await visit('data_editor_wide_long');await configure('x');const originalId=await group().getAttribute('data-value');
 await page.locator('#wide_long_options_tab a[data-value="ID / Fixed"]').evaluate(e=>e.click());await page.locator('#wide_long_fixed_mode input[value="selected"]').check();
 await page.locator('#wide_long_options_tab a[data-value="Reshape"]').evaluate(e=>e.click());
 for(const [i,lang] of ['ja','zh','es','fr','de','vi','en','ko'].entries()){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_wide_long');
  const source=i%2===0?'y':'x',previous=source==='x'?'y':'x',offset=source==='y'?100:0;
  await configure(source);
  assert.equal(await group().count(),1);assert.equal(await group().getAttribute('data-value'),originalId);
  assert.ok((await page.locator('#wide_long_message').innerText()).includes(templates[lang].updated.replace('%s','사용자_값 (2)')));
  assert.equal(await page.locator('#wide_long_preview table.dataTable[id]').count(),0);
  for(let j=1;j<=2;j++){assert.equal(await page.locator(`[data-input-id="wide_long_available"] [data-value="${previous}${j}"]`).count(),1);assert.equal(await page.locator(`[data-input-id="wide_long_available"] [data-value="${source}${j}"]`).count(),0);}
  await page.locator('#preview_wide_long').click();await page.waitForTimeout(500);
  const rows=await page.locator('#wide_long_preview table.dataTable[id]').evaluate(e=>window.jQuery(e).DataTable().rows().data().toArray());
  assert.deepEqual(rows.map(r=>JSON.stringify([Number(r[0]),r[1],Number(r[2])])).sort(),[[1,'Normality',offset+10],[2,'Normality',offset+20],[1,'morning',offset+30],[2,'morning',offset+40]].map(r=>JSON.stringify(r)).sort());
  console.log('PASS',lang,'same-name update: stable ID, one group, old sources released, new values and localized notice');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
