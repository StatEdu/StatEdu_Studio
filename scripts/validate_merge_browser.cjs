const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&!document.documentElement.classList.contains('shiny-busy'));
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1100);}
 async function set(id,v){await page.locator('#'+id).evaluate((e,v)=>{if(e.selectize)e.selectize.setValue(v);else{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));}},v);await page.waitForTimeout(350);}
 await visit('data_editor_merge');
 await page.locator('#merge_files').setInputFiles([
  {name:'사용자.csv',mimeType:'text/csv',buffer:Buffer.from('id,Review,Normality\n1,10,100\n2,20,200')},
  {name:'second.csv',mimeType:'text/csv',buffer:Buffer.from('id,Review,Normality\n2,30,300\n3,40,400')}
 ]);await page.waitForTimeout(1400);
 await set('merge_dat_delimiter','comma');await page.locator('#merge_dat_has_names').check();
 await page.locator('#merge_join_type input[value="full"]').check();
 await set('merge_indicator_name','사용자_시점');await set('merge_indicator_values','시작,종료');
 await page.locator('#merge_mode a[data-value="cases"]').evaluate(e=>e.click());await page.waitForTimeout(500);
 await set('merge_case_variables',['id','Review']);
 await page.locator('#preview_merge_data').click();await page.waitForTimeout(1000);
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_merge');
  const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
  const menu=(await page.locator('.navbar-nav a[data-value="data_editor_merge"]').first().innerText()).trim();
  if(lang==='en'||lang==='ko')assert.equal(menu,dict['merge.ui.title']);
  else {assert.notEqual(menu,'Merge');assert.ok(!/[가-힣]/.test(menu));}
  assert.equal((await page.locator('#lazy_data_editor_merge h1').innerText()).trim(),dict['merge.ui.title']);
  assert.equal(await page.locator('#merge_dat_delimiter').inputValue(),'comma');
  assert.equal(await page.locator('#merge_dat_has_names').isChecked(),true);
  assert.equal(await page.locator('#merge_csv_header').isChecked(),true);
  assert.equal(await page.locator('#merge_join_type input[value="full"]').isChecked(),true);
  assert.equal(await page.locator('#merge_id_variable').inputValue(),'id');
  assert.equal(await page.locator('#merge_indicator_name').inputValue(),'사용자_시점');
  assert.equal(await page.locator('#merge_indicator_values').inputValue(),'시작,종료');
  assert.equal(await page.locator('#merge_mode li.active a').getAttribute('data-value'),'cases');
  assert.deepEqual(await page.locator('#merge_case_variables').evaluate(e=>e.selectize.getValue()),['id','Review']);
  assert.ok((await page.locator('#merge_files').evaluate(e=>e.closest('.shiny-input-container').querySelector('input[type="text"]').placeholder)).includes('사용자.csv'));
  assert.equal(await page.locator('#merge_dat_delimiter option[value="comma"]').innerText(),dict['merge.ui.comma']);
  const table=page.locator('#merge_data_preview table.dataTable[id]');
  const data=await table.evaluate(e=>{const api=window.jQuery(e).DataTable();return {rows:api.rows().data().toArray(),language:api.settings()[0].oLanguage};});
  assert.deepEqual(data.rows.map(r=>[Number(r[0]),Number(r[1]),r[2]]),[[1,10,'시작'],[2,20,'시작'],[2,30,'종료'],[3,40,'종료']]);
  assert.equal(data.language.sSearch,lang==='ko'?'검색:':lang==='en'?'Search:':dict['analysis.ui.search']);
  const message=dict['merge.ui.preview'].replace('%s','4').replace('%s','3');
  assert.ok((await page.locator('#merge_data_message').innerText()).includes(message));
  // A fresh preview also succeeds after the file input was recreated.
  await page.locator('#preview_merge_data').click();await page.waitForTimeout(600);
  assert.ok((await page.locator('#merge_data_message').innerText()).includes(message));
  console.log('PASS',lang,'uploaded files, settings, selected variables, preview values and localized controls/status');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
