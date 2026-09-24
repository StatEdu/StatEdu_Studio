const assert=require('node:assert/strict'),fs=require('node:fs');const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));await page.goto('http://127.0.0.1:43873/?lang=ko');await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));await page.waitForTimeout(1200);
const csv='id,Review\n'+Array.from({length:24},(_,i)=>`${i%12+1},${i+1}`).join('\n');await page.locator('#file').setInputFiles({name:'id-preview.csv',mimeType:'text/csv',buffer:Buffer.from(csv)});await page.locator('#apply_all_variable_selection').click();
async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1100);}
async function set(id,v){await page.locator('#'+id).evaluate((e,v)=>{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));},v);await page.waitForTimeout(400);}
await visit('data_editor_id_aggregate');await page.waitForTimeout(1200);
const fields={id_aggregate_id:'id',id_aggregate_value:'Review',id_aggregate_condition:'Review > 0',id_aggregate_stat:'mean',id_aggregate_empty:'9',id_aggregate_output_name:'사용자_결과'};for(const [id,v]of Object.entries(fields))await set(id,v);
await page.locator('#preview_id_aggregate').click();await page.waitForTimeout(1200);
for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
 await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_id_aggregate');
 for(const [id,v]of Object.entries(fields))assert.equal(await page.locator('#'+id).inputValue(),v,lang+'/'+id);
 const table=page.locator('#id_aggregate_preview');await table.locator('tbody tr').first().waitFor();
 const expected=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
 const settings=await table.locator('table.dataTable[id]').evaluate(e=>{const api=window.jQuery(e).DataTable();return {language:api.settings()[0].oLanguage,rows:api.rows().data().toArray(),count:api.page.info().recordsTotal};});
 assert.equal(settings.count,12);assert.ok((await table.innerText()).includes('사용자_결과'));
 assert.deepEqual(settings.rows.map(row=>row.map(Number)),Array.from({length:10},(_,i)=>[i+1,i+7]));
 const labels=lang==='ko'?['검색:','다음','이전']:lang==='en'?['Search:','Next','Previous']:['search','next','previous'].map(k=>expected['analysis.ui.'+k]);
 assert.equal(settings.language.sSearch,labels[0],lang+'/search');
 assert.equal(settings.language.oPaginate.sNext,labels[1],lang+'/next');
 assert.equal(settings.language.oPaginate.sPrevious,labels[2],lang+'/previous');
 assert.ok((await page.locator('#id_aggregate_message').innerText()).includes(expected['id_aggregate.preview_created'].replace('%s','12')));
 if(lang==='ja'){
  await table.locator('.paginate_button.next').click();await page.waitForTimeout(500);assert.equal(await table.locator('table[id] tbody tr').count(),2);
  await table.locator('.paginate_button.previous').click();await page.waitForTimeout(500);
  await table.locator('input[type="search"]').fill('no-such-ID');await page.waitForTimeout(700);assert.equal(await table.locator('table.dataTable[id]').evaluate(e=>window.jQuery(e).DataTable().page.info().recordsDisplay),0);
  await table.locator('input[type="search"]').fill('');await page.waitForTimeout(700);
 }
 console.log('PASS',lang,'input values, preview data, localized table controls and status');
}
assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
