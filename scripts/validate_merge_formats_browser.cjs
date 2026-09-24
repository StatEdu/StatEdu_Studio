const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const manifest=JSON.parse(fs.readFileSync('tmp/merge-format-fixtures/manifest.json','utf8'));
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&!document.documentElement.classList.contains('shiny-busy'));
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(800);}
 async function rows(){return page.locator('#merge_data_preview table.dataTable[id]').evaluate(e=>window.jQuery(e).DataTable().rows().data().toArray());}
 await visit('data_editor_merge');await page.locator('#merge_dat_has_names').check();await page.locator('#merge_join_type input[value="full"]').check();
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']) {
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_merge');
  const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
  assert.equal(await page.locator('#merge_dat_has_names').isChecked(),true);
  assert.equal(await page.locator('#merge_join_type input[value="full"]').isChecked(),true);
  for(const [format,fixture] of Object.entries(manifest)) {
   await page.locator('#merge_dat_delimiter').selectOption(fixture.delimiter);
   await page.locator('#merge_files').setInputFiles(fixture.paths.map((path,i)=>({name:fixture.names[i],mimeType:'application/octet-stream',buffer:fs.readFileSync(path)})));
   await page.waitForTimeout(750);
   assert.equal((await page.locator('#merge_data_message').innerText()).trim(),'');
   assert.deepEqual((await rows()).map(r=>[r[0],Number(r[1]),Number(r[2])]),[[fixture.names[0],2,3],[fixture.names[1],2,3]],`${lang}/${format}/summary`);
   await page.locator('#preview_merge_data').click();await page.waitForTimeout(500);
   const actual=(await rows()).map(r=>r.map((v,i)=>v===null||v===''?null:[0,1,3].includes(i)?Number(v):v)).sort((a,b)=>a[0]-b[0]);
   assert.deepEqual(actual,[[1,10,'Review',null,null],[2,20,'Normality',30,'Normality'],[3,null,null,40,'Review']],`${lang}/${format}`);
   const message=dict['merge.ui.preview'].replace('%s','3').replace('%s','5');
   assert.ok((await page.locator('#merge_data_message').innerText()).includes(message),`${lang}/${format}/status`);
  }
  console.log('PASS',lang,'8 format/delimiter paths: uploaded summary, full merge values, user strings and localized status');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
