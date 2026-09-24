const assert=require('node:assert/strict');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input'));
 const columns=Array.from({length:20},(_,i)=>'x'+(i+1));
 const csv=[columns.join(','),columns.map((_,i)=>i+1).join(','),columns.map((_,i)=>i+101).join(',')].join('\n');
 await page.locator('#file').setInputFiles({name:'long-list.csv',mimeType:'text/csv',buffer:Buffer.from(csv)});await page.locator('#apply_all_variable_selection').click();
 async function visit(id){await page.locator(`.navbar-nav a[data-value="${id}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(800);}
 async function set(id,v){await page.locator('#'+id).evaluate((e,v)=>{e.value=v;e.dispatchEvent(new Event('change',{bubbles:true}));},v);await page.waitForTimeout(250);}
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await visit('data_editor_wide_long');
  const available=page.locator('[data-input-id="wide_long_available"]');
  await available.focus();await page.keyboard.press('Control+a');await page.locator('#wide_long_move').click();await page.waitForTimeout(600);
  const list=page.locator('[data-input-id="wide_long_selected"]');
  await list.locator('[data-value="x1"]').click();await list.locator('[data-value="x2"]').click({modifiers:['Control']});
  assert.equal(await list.locator('[aria-selected="true"]').count(),2);
  const item=await list.locator('[data-value="x1"]').boundingBox(),box=await list.boundingBox();
  await page.mouse.move(item.x+80,item.y+item.height/2);await page.mouse.down();
  await page.mouse.move(box.x+80,box.y+box.height-6,{steps:12});await page.waitForTimeout(1800);
  const scroll=await list.evaluate(e=>({top:e.scrollTop,max:e.scrollHeight-e.clientHeight}));
  assert.ok(scroll.max>0&&scroll.top>=scroll.max-2,`${lang}/automatic scrolling`);
  await page.mouse.up();await page.waitForTimeout(600);
  const expected=[...columns.slice(2),...columns.slice(0,2)];
  assert.deepEqual(await page.locator('#wide_long_selected option').evaluateAll(opts=>opts.map(o=>o.value)),expected);
  assert.deepEqual(await list.locator('[aria-selected="true"]').evaluateAll(opts=>opts.map(o=>o.dataset.value)),['x1','x2']);
  await set('wide_long_value_name','사용자_값');await set('wide_long_index_name','시점');
  await page.locator('#wide_long_set_spec').click();await page.waitForTimeout(450);await page.locator('#preview_wide_long').click();await page.waitForTimeout(500);
  const rows=await page.locator('#wide_long_preview table.dataTable[id]').evaluate(e=>window.jQuery(e).DataTable().rows().data().toArray());
  const desired=[];for(let id=1;id<=2;id++)for(let time=1;time<=20;time++)desired.push([id,time,Number(expected[time-1].slice(1))+(id-1)*100]);
  assert.deepEqual(rows.map(r=>JSON.stringify(r.map(Number))).sort(),desired.map(r=>JSON.stringify(r)).sort());
  await page.locator('[data-input-id="wide_long_configured"] .analysis-transfer-option').click();await page.locator('#wide_long_remove_spec').click();await page.waitForTimeout(400);
  console.log('PASS',lang,'20-column list, two-item pointer drag, real auto-scroll, selection and 40 transformed rows');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
