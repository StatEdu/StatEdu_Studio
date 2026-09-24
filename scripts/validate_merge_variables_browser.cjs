const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&!document.documentElement.classList.contains('shiny-busy'));
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(900);}
 async function upload(offset){await page.locator('#merge_files').setInputFiles([
  {name:'사용자.csv',mimeType:'text/csv',buffer:Buffer.from(`사용자_ID,Review\n1,${10+offset}\n2,${20+offset}`)},
  {name:'second.csv',mimeType:'text/csv',buffer:Buffer.from(`사용자_ID,Review\n2,${30+offset}\n3,${40+offset}`)}
 ]);await page.waitForTimeout(1000);}
 async function preview(){await page.locator('#preview_merge_data').click();await page.waitForTimeout(600);}
 async function rows(){return page.locator('#merge_data_preview table.dataTable[id]').evaluate(e=>window.jQuery(e).DataTable().rows().data().toArray());}
 function numeric(data){return data.map(row=>row.map(v=>v===null||v===''?null:Number(v))).sort((a,b)=>a[0]-b[0]);}
 await visit('data_editor_merge');await upload(0);
 await page.locator('#merge_id_variable').fill('사용자_ID');await page.locator('#merge_id_variable').blur();await page.waitForTimeout(400);
 let previous,offset=0;
 for(const [index,lang] of ['ja','zh','es','fr','de','vi','en','ko'].entries()){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_merge');
  const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
  assert.equal(await page.locator('#merge_id_variable').inputValue(),'사용자_ID');
  assert.equal(await page.locator('#merge_mode li.active a').getAttribute('data-value'),'variables');
  if(previous){assert.deepEqual(numeric(await rows()),previous);assert.equal(await page.locator('#merge_join_type input[value="full"]').isChecked(),true);}
  offset=(index+1)*100;await upload(offset);
  assert.equal((await page.locator('#merge_data_message').innerText()).trim(),'');
  assert.deepEqual((await rows()).map(r=>[r[0],Number(r[1]),Number(r[2])]),[['사용자.csv',2,2],['second.csv',2,2]]);
  for(const mode of ['left','inner','full']){
   await page.locator(`#merge_join_type input[value="${mode}"]`).check();await page.waitForTimeout(250);await preview();
   const full=[[1,offset+10,null],[2,offset+20,offset+30],[3,null,offset+40]];
   const expected=mode==='left'?full.slice(0,2):mode==='inner'?full.slice(1,2):full;
   assert.deepEqual(numeric(await rows()),expected,`${lang}/${mode}`);
   const header=await page.locator('#merge_data_preview').innerText();assert.ok(header.includes('사용자_ID'));assert.ok(header.includes('Review_1'));
   assert.ok((await page.locator('#merge_data_message').innerText()).includes(dict['merge.ui.preview'].replace('%s',String(expected.length)).replace('%s','3')));
   if(mode==='full')previous=expected;
  }
  console.log('PASS',lang,'left/inner/full values, same-name file refresh, custom ID and language persistence');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
