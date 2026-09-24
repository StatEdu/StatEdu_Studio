const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage({viewport:{width:1488,height:1000}}),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);
 const selectedCases=process.env.STATEDU_TEST_CASE_SELECTION==='1';
 const rows=[selectedCases?'sex,site,x,z,y':'site,x,z,y'];for(let i=0;i<80;i++){const x=Math.sin(i*.8),z=Math.cos(i*1.3),values=[i%2?'B':'A',x,z,2*x+.3*z+Math.sin(i*2.7)];if(selectedCases)values.unshift(i%4<2?'Male':'Female');rows.push(values.join(','));}
 await page.locator('#file').setInputFiles({name:'split-regression.csv',mimeType:'text/csv',buffer:Buffer.from(rows.join('\n'))});await page.locator('#apply_all_variable_selection').click();
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(el=>el.click());await page.waitForTimeout(700);}
 if(selectedCases){
  await visit('data_editor_cases');await page.locator('[data-input-id="scope_cases_available"] [data-value="sex"]').dblclick();
  await page.locator('#scope_cases_values input[value="Male"]').check();await page.locator('#scope_cases_apply').click();
  await page.waitForFunction(()=>document.querySelector('#statedu-analysis-scope-status')?.textContent.includes('40/80'));
 }
 await visit('data_editor_split');await page.locator('[data-input-id="scope_split_available"] [data-value="site"]').dblclick();
 for(const v of ['A','B'])await page.locator(`#scope_split_values input[value="${v}"]`).check();await page.locator('#scope_split_apply').click();
 await visit('Regression');
 for(const [v,button] of [['y','hierarchical_dependent_move'],['x','hierarchical_block1_move'],['z','hierarchical_block1_move']]){
  await page.locator(`[data-input-id="hierarchical_available"] [data-value="${v}"]`).click();await page.locator(`#${button}`).click();await page.waitForTimeout(350);
 }
 await page.locator('#hierarchical_auto_method').uncheck();await page.waitForTimeout(400);
 await page.locator('#run_hierarchical').click();
 await page.waitForFunction(()=>!!window.stateduSplitSnapshots?.hierarchical_results&&document.querySelector('#statedu-scope-stop')?.hidden,null,{timeout:120000});
 await page.waitForFunction(()=>document.querySelectorAll('#hierarchical_results table[data-result-table-role="main"]').length>=2&&!document.documentElement.classList.contains('shiny-busy'),null,{timeout:60000});
 const main=()=>page.locator('#hierarchical_results table[data-result-table-role="main"]').evaluateAll(ts=>ts.map(t=>[...t.querySelectorAll('th,td')].map(c=>c.textContent.trim().replace(/\s+/g,' '))));
 const images=()=>page.locator('#hierarchical_results img').evaluateAll(ns=>ns.map(n=>n.getAttribute('src')));
 const baseline=await main(),pictures=await images();assert.ok(baseline.length>=2);assert.ok(pictures.length>=4);assert.ok(pictures.every(src=>src.startsWith('data:image/')));
 for(const language of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');if(!await page.locator('#app_language').isVisible())await visit('about_preferences');
  await page.locator('#app_language').selectOption(language);await visit('Regression');
  await page.waitForFunction(lang=>{const ts=[...document.querySelectorAll('#hierarchical_results table[data-result-table-role="appendix"]')];return ts.length>0&&ts.every(t=>t.dataset.resultTableLanguage===lang);},language);
  assert.deepEqual(await main(),baseline);assert.deepEqual(await images(),pictures);
  await page.waitForFunction(()=>[...document.querySelectorAll('#hierarchical_results img')].every(img=>img.complete&&img.naturalWidth>0&&img.naturalHeight>0));
  if(selectedCases){
   const status=await page.locator('#statedu-analysis-scope-status').innerText();
   assert.match(status,/40\/80/);assert.match(status,/site/);
   const dictionary=JSON.parse(fs.readFileSync(`i18n/${language}.json`,'utf8')).translations;
   const template=dictionary['analysis.scope.split_status']||'Split variable: %s · Total groups: %s';
   assert.ok(status.includes(template.replace('%s','site').replace('%s','2')));
  }
  if(language==='ja'){fs.mkdirSync('tmp/split-regression-localized',{recursive:true});fs.writeFileSync('tmp/split-regression-localized/entries.json',JSON.stringify([{id:'regression-split',title:'Split regression',html:await page.evaluate(()=>window.stateduSplitSnapshots.hierarchical_results)}]));}
  console.log(`PASS: ${language} two-group regression appendix language; main cells and ${pictures.length} residual images unchanged`);
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
