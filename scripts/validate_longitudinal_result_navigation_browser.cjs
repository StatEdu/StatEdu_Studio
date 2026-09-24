const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto(process.env.STATEDU_UI_TEST_URL||'http://127.0.0.1:43923/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);
 const split=process.env.STATEDU_TEST_SPLIT==='1';
 const strictNavigation=process.env.STATEDU_TEST_STRICT_NAV==='1';
 const rows=['id,time,y,x,group'];for(let i=0;i<120;i++){const id=Math.floor(i/4)+1,t=i%4,x=Math.sin(i*1.13);rows.push([id,t,4+.3*t+.7*x+Math.cos(id*.7)+Math.sin(i*2.3)*.4,x,id%2?'A':'B'].join(','));}
 await page.locator('#file').setInputFiles({name:'longitudinal-result.csv',mimeType:'text/csv',buffer:Buffer.from(rows.join('\n'))});await page.locator('#apply_all_variable_selection').click();
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(900);}
 if(split){
  await visit('data_editor_split');await page.locator('[data-input-id="scope_split_available"] [data-value="group"]').dblclick();
  for(const value of ['A','B'])await page.locator(`#scope_split_values input[value="${value}"]`).check();
  await page.locator('#scope_split_apply').click();await page.waitForTimeout(500);
 }
 await visit('Longitudinal / Panel Models');
 for(const [role,variable]of [['outcome','y'],['id','id'],['time','time'],['predictors','x']]){await page.locator(`[data-input-id="longitudinal_available"] [data-value="${variable}"]`).click();await page.locator(`#longitudinal_${role}_move`).click();await page.locator(`[data-input-id="longitudinal_${role}"] [data-value="${variable}"]`).waitFor();}
 await page.locator('#longitudinal_family').selectOption('gaussian');await page.waitForTimeout(500);
 await page.locator('#run_longitudinal').click();
 const main='#longitudinal_results table[data-result-table-role="main"]';
 await page.locator(main).first().waitFor({timeout:90000});await page.waitForTimeout(1200);
 if(split)await page.waitForFunction(()=>!!window.stateduSplitSnapshots?.longitudinal_results&&document.querySelector('#statedu-scope-stop')?.hidden,null,{timeout:90000});
 const cells=()=>page.locator(main).evaluateAll(ts=>ts.map(t=>[...t.querySelectorAll('th,td')].map(c=>c.textContent.trim())));
 const baseline=await cells();assert.ok(baseline.length>0);assert.ok(!/[가-힣]/.test(JSON.stringify(baseline)));
 if(split)assert.equal(baseline.length,2);
 await page.locator('#add_longitudinal_result').click();await page.waitForTimeout(2000);await visit('result');await page.waitForTimeout(1000);
 const frames='#saved_results_list .saved-result-entry iframe';await page.locator(frames).first().waitFor();
 const snapshots=()=>page.locator(frames).evaluateAll(es=>es.map(e=>e.getAttribute('srcdoc')));
 const saved=await snapshots();assert.ok(saved.length>0);const captured={};
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');if(!strictNavigation&&!await page.locator('#app_language').isVisible())await visit('about_preferences');
  await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(strictNavigation?700:2000);await visit('Longitudinal / Panel Models');
  for(let attempt=0;!strictNavigation&&attempt<4&&!await page.locator(main).first().isVisible();attempt++)await visit('Longitudinal / Panel Models');
  await page.locator(main).first().waitFor();assert.deepEqual(await cells(),baseline,lang+'/main');
  const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
  await page.waitForFunction(title=>document.querySelector('#longitudinal_results')?.textContent.includes(title),dict['analysis.ui.assumption_checks']);
  assert.ok((await page.locator('#longitudinal_results').textContent()).includes(dict['analysis.ui.assumption_checks']),lang+'/appendix');
  const headings=await page.locator('#longitudinal_results h3').allTextContents();
  for(const key of ['coefficients_detailed','recommended_analysis'])assert.ok(headings.includes(dict['analysis.ui.'+key]),lang+'/'+key);
  if(split)for(const key of ['assumption_checks','coefficients_detailed','recommended_analysis'])assert.equal(headings.filter(x=>x===dict['analysis.ui.'+key]).length,2,lang+'/'+key+'/both groups');
  captured[lang]=await page.locator('#longitudinal_results').innerHTML();
  await visit('result');assert.deepEqual(await snapshots(),saved,lang+'/saved snapshots');
  console.log('PASS result navigation',lang,'English main cells, localized appendix, unchanged saved snapshot');
 }
 assert.deepEqual(errors,[]);
 const out=split?'tmp/longitudinal-split-navigation':'tmp/longitudinal-result-navigation';
 fs.mkdirSync(out,{recursive:true});fs.writeFileSync(out+'/captured.json',JSON.stringify(captured));
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
