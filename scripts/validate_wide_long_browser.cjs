const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input'));
 await page.locator('#file').setInputFiles({name:'wide.csv',mimeType:'text/csv',buffer:Buffer.from('id,x1,x2,group\n1,10,30,A\n2,20,40,B')});await page.locator('#apply_all_variable_selection').click();
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(900);}
 async function set(id,value){await page.locator('#'+id).evaluate((e,value)=>{if(e.selectize)e.selectize.setValue(value);else{e.value=value;e.dispatchEvent(new Event('change',{bubbles:true}));}},value);await page.waitForTimeout(350);}
 await visit('data_editor_wide_long');
 for(const variable of ['x1','x2']){await page.locator(`[data-input-id="wide_long_available"] [data-value="${variable}"]`).dblclick();await page.waitForTimeout(500);}
 await set('wide_long_value_name','사용자_값');await set('wide_long_index_name','사용자_시점');await set('wide_long_index_values','Normality,morning');
 await page.locator('#wide_long_set_spec').click();await page.waitForTimeout(700);
 const fields={wide_long_value_name:'다음_값',wide_long_index_name:'다음_시점',wide_long_index_values:'return\nNormality',wide_long_group_name:'사용자_집단',wide_long_time_name:'사용자_시간',wide_long_generated_id:'새_ID',wide_long_group_count:'2',wide_long_time_count:'3'};
 for(const [id,v] of Object.entries(fields))await set(id,v);
 await page.locator('#wide_long_options_tab a[data-value="ID / Fixed"]').evaluate(e=>e.click());await page.waitForTimeout(300);
 await page.locator('#wide_long_fixed_mode input[value="selected"]').check();await set('wide_long_fixed_variables',['group']);
 await page.locator('#preview_wide_long').click();await page.waitForTimeout(700);
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_wide_long');
  for(const [id,v] of Object.entries(fields))assert.equal(await page.locator('#'+id).inputValue(),v,`${lang}/${id}`);
  assert.equal(await page.locator('#wide_long_options_tab li.active a').getAttribute('data-value'),'ID / Fixed');
  assert.equal(await page.locator('#wide_long_fixed_mode input[value="selected"]').isChecked(),true);
  assert.deepEqual(await page.locator('#wide_long_fixed_variables').evaluate(e=>e.selectize.getValue()),['group']);
  assert.ok((await page.locator('[data-input-id="wide_long_configured"]').innerText()).includes('사용자_값'));
  const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
  const table=await page.locator('#wide_long_preview table.dataTable[id]').evaluate(e=>{const api=window.jQuery(e).DataTable();return {rows:api.rows().data().toArray(),search:api.settings()[0].oLanguage.sSearch};});
  const actual=table.rows.map(row=>JSON.stringify([Number(row[0]),row[1],Number(row[2]),row[3]])).sort();
  assert.deepEqual(actual,[[1,'Normality',10,'A'],[1,'morning',30,'A'],[2,'Normality',20,'B'],[2,'morning',40,'B']].map(row=>JSON.stringify(row)).sort());
  assert.equal(table.search,lang==='ko'?'검색:':lang==='en'?'Search:':dict['analysis.ui.search']);
  const template=JSON.parse(fs.readFileSync('tmp/wide-long-status-templates.json','utf8'))[lang];
  assert.ok((await page.locator('#wide_long_message').innerText()).includes(template.replace('%s','4').replace('%s','4').replace('%s','1')));
  console.log('PASS',lang,'input state, configured group, original indicators, preview and localized status/controls');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
