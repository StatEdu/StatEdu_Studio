const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage({viewport:{width:1488,height:1000}}),errors=[],captured=[];
 page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);
 const rows=['site,x,y,binary'];for(let i=0;i<120;i++){const x=Math.sin(i*.8);rows.push([i%2?'B':'A',x,2*x+Math.sin(i*2.7),i%7<3?1:0].join(','));}
 await page.locator('#file').setInputFiles({name:'split-glm-logistic.csv',mimeType:'text/csv',buffer:Buffer.from(rows.join('\n'))});await page.locator('#apply_all_variable_selection').click();
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(el=>el.click());await page.waitForTimeout(700);}
 await visit('data_editor_split');await page.locator('[data-input-id="scope_split_available"] [data-value="site"]').dblclick();
 for(const v of ['A','B'])await page.locator(`#scope_split_values input[value="${v}"]`).check();await page.locator('#scope_split_apply').click();
 await page.waitForFunction(()=>document.querySelector('#statedu-analysis-scope-status')?.textContent.includes('site'));
 for(const config of [
  {id:'generalized',tab:'Generalized Linear Model (GLM)',moves:[['y','outcome'],['x','predictors']]},
  {id:'logistic',tab:'analysis_logistic_regression',moves:[['binary','dependent'],['x','block1']]}
 ]){
  await visit(config.tab);
  for(const [v,role] of config.moves){await page.locator(`[data-input-id="${config.id}_available"] [data-value="${v}"]`).click();await page.locator(`#${config.id}_${role}_move`).click();await page.waitForTimeout(450);}
  await page.locator(`#run_${config.id}`).click();
  const output=config.id+'_results';
  await page.waitForFunction(id=>!!window.stateduSplitSnapshots?.[id]&&document.querySelector('#statedu-scope-stop')?.hidden,output,{timeout:120000});
  await page.waitForFunction(id=>document.querySelectorAll(`#${id} table[data-result-table-role="main"]`).length>=2&&!document.documentElement.classList.contains('shiny-busy'),output,{timeout:60000});
  const main=()=>page.locator(`#${output} table[data-result-table-role="main"]`).evaluateAll(ts=>ts.map(t=>[...t.querySelectorAll('th,td')].map(c=>c.textContent.trim().replace(/\s+/g,' '))));
  const baseline=await main();
  for(const language of ['ja','zh','es','fr','de','vi','en','ko']){
   await visit('about_preferences');if(!await page.locator('#app_language').isVisible())await visit('about_preferences');
   await page.locator('#app_language').selectOption(language);await page.waitForTimeout(1000);await visit(config.tab);
   await page.waitForFunction(({id,lang})=>{const ts=[...document.querySelectorAll(`#${id} table[data-result-table-role="appendix"]`)];return ts.length>0&&ts.every(t=>t.dataset.resultTableLanguage===lang);},{id:output,lang:language});
   assert.deepEqual(await main(),baseline);
   if(language==='ja')captured.push(await page.evaluate(id=>window.stateduSplitSnapshots[id],output));
   console.log(`PASS: ${config.id} ${language}; two-group appendix language; main cells unchanged`);
  }
 }
 assert.deepEqual(errors,[]);
 fs.mkdirSync('tmp/split-glm-logistic-localized',{recursive:true});fs.writeFileSync('tmp/split-glm-logistic-localized/entries.json',JSON.stringify([{id:'glm-logistic-split',title:'Split GLM and logistic regression',html:captured.join('\n')}]));
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
