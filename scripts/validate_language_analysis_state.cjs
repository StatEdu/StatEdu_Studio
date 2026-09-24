const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE || 'playwright');
(async()=>{
 const browser=await chromium.launch({channel:'chrome',headless:true});
 try{
  const page=await browser.newPage({viewport:{width:1488,height:1000}});const errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  await page.goto(process.env.STATEDU_UI_TEST_URL || 'http://127.0.0.1:43873/?lang=ko');
  await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1 && document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
  await page.waitForTimeout(1200);
  const rows=['sex,site,x1,x2,x3,x4'];
  for(let i=0;i<60;i++)rows.push([i%4<2?'Male':'Female',i%2?'B':'A',...Array.from({length:4},(_,j)=>(10+i/10+j*.3+Math.sin(i*1.7+j*2.3)).toFixed(4))].join(','));
  await page.locator('#file').setInputFiles({name:'language-state.csv',mimeType:'text/csv',buffer:Buffer.from(rows.join('\n'))});
  await page.locator('#apply_all_variable_selection').click();
  async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(el=>el.click());await page.waitForTimeout(700);}
  await visit('data_editor_cases');
  await page.locator('[data-input-id="scope_cases_available"] [data-value="sex"]').dblclick();
  await page.locator('#scope_cases_values input[value="Male"]').check();await page.locator('#scope_cases_apply').click();
  await page.waitForFunction(()=>document.querySelector('#statedu-analysis-scope-status')?.textContent.includes('30/60'));
  await visit('data_editor_split');
  await page.locator('[data-input-id="scope_split_available"] [data-value="site"]').dblclick();
  for(const value of ['A','B'])await page.locator(`#scope_split_values input[value="${value}"]`).check();
  await page.locator('#scope_split_apply').click();
  await page.waitForFunction(()=>document.querySelector('#statedu-analysis-scope-status')?.textContent.includes('site'));
  await visit('One-group repeated-measures ANOVA');
  for(const [variable,role] of [['x1','experimental'],['x2','experimental'],['x3','control'],['x4','control']]){
   await page.locator(`[data-input-id="one_group_rm_available"] [data-value="${variable}"]`).click();
   await page.locator(`#one_group_rm_${role}_move`).click();
   await page.locator(`[data-input-id="one_group_rm_${role}_variables"] [data-value="${variable}"]`).waitFor();
  }
  await page.waitForFunction(()=>document.querySelector('#one_group_rm_posthoc')?.classList.contains('shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
  await page.waitForTimeout(500);
  await page.locator('#one_group_rm_posthoc').uncheck();
  await page.locator('input[name="one_group_rm_adjustment"][value="bonferroni"]').check();
  await page.waitForTimeout(700);
  assert.equal(await page.locator('#one_group_rm_posthoc').isChecked(),false);
  console.log('OPTIONS BEFORE RUN',await page.evaluate(()=>({posthoc:Shiny.shinyapp.$inputValues.one_group_rm_posthoc,adjustment:Shiny.shinyapp.$inputValues.one_group_rm_adjustment})));
  await page.locator('#run_one_group_rm_anova').click();
  await page.waitForFunction(()=>!!window.stateduSplitSnapshots?.one_group_rm_anova_results && document.querySelector('#statedu-scope-stop')?.hidden,null,{timeout:90000});
  await page.waitForFunction(()=>document.querySelectorAll('#one_group_rm_anova_results table[data-result-table-role="main"]').length>=4 && !document.documentElement.classList.contains('shiny-busy'),null,{timeout:60000});
  await page.waitForTimeout(1500);
  console.log('OPTIONS AFTER RUN',await page.evaluate(()=>({posthoc:document.querySelector('#one_group_rm_posthoc')?.checked,input:Shiny.shinyapp.$inputValues.one_group_rm_posthoc})));
  const mainText=()=>page.locator('#one_group_rm_anova_results table[data-result-table-role="main"]').evaluateAll(tables=>tables.map(t=>[...t.querySelectorAll('th,td')].map(c=>c.textContent.trim().replace(/\s+/g,' '))));
  const baseline=await mainText();assert.ok(baseline.length>=4);
  fs.mkdirSync('tmp/language-state-i18n',{recursive:true});
  fs.writeFileSync('tmp/language-state-i18n/entries.json',JSON.stringify([{id:'split-language-state',title:'Split repeated measures',html:await page.evaluate(()=>window.stateduSplitSnapshots.one_group_rm_anova_results)}]));
  await page.locator('#add_one_group_rm_anova_result').click();await visit('result');
  await page.locator('#saved_results_list .saved-result-entry iframe').first().waitFor();
  const snapshots=()=>page.locator('#saved_results_list .saved-result-entry iframe').evaluateAll(frames=>frames.map(f=>f.getAttribute('srcdoc')));
  const saved=await snapshots();
  await page.evaluate(()=>window.__analysisStateSentinel='same-session');
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
   await visit('about_preferences');
   if(!await page.locator('#app_language').isVisible()){
    console.log('Navigation retry: preferences after split snapshot');
    await visit('about_preferences');
   }
   console.log('PREFERENCES',await page.evaluate(()=>({active:document.querySelector('.navbar-nav li.active>a')?.dataset.value,languageInputs:document.querySelectorAll('#app_language').length,busy:document.documentElement.classList.contains('shiny-busy'),modals:[...document.querySelectorAll('.modal.in')].map(n=>n.textContent)})));
   await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(1200);
   await visit('One-group repeated-measures ANOVA');
   await page.waitForFunction(()=>document.querySelectorAll('[data-input-id="one_group_rm_experimental_variables"] [data-value]').length===2 && document.querySelectorAll('[data-input-id="one_group_rm_control_variables"] [data-value]').length===2);
   console.log('OPTIONS AFTER LANGUAGE',lang,await page.evaluate(()=>({posthoc:document.querySelector('#one_group_rm_posthoc')?.checked,input:Shiny.shinyapp.$inputValues.one_group_rm_posthoc})));
   assert.equal(await page.evaluate(()=>window.__analysisStateSentinel),'same-session');
   for(const [role,values] of [['experimental',['x1','x2']],['control',['x3','x4']]]){
    assert.deepEqual(await page.locator(`[data-input-id="one_group_rm_${role}_variables"] [data-value]`).evaluateAll(nodes=>nodes.map(n=>n.dataset.value)),values);
   }
   assert.equal(await page.locator('#one_group_rm_posthoc').isChecked(),false);
   assert.equal(await page.locator('input[name="one_group_rm_adjustment"][value="bonferroni"]').isChecked(),true);
   assert.match(await page.locator('#statedu-analysis-scope-status').textContent(),/30\/60/);
   assert.match(await page.locator('#statedu-analysis-scope-status').textContent(),/site/);
   const current=await mainText();
   console.log('RESULT TABLE COUNTS',lang,{before:baseline.length,after:current.length,snapshot:await page.evaluate(()=>{const d=new DOMParser().parseFromString(window.stateduSplitSnapshots.one_group_rm_anova_results,'text/html');return d.querySelectorAll('table[data-result-table-role="main"]').length;})});
   assert.deepEqual(current,baseline);
   await page.waitForFunction(language=>{
    const tables=[...document.querySelectorAll('#one_group_rm_anova_results table[data-result-table-role="appendix"]')];
    return tables.length>0 && tables.every(t=>t.dataset.resultTableLanguage===language);
   },lang);
   if(lang==='ja'){
    fs.mkdirSync('tmp/language-state-localized',{recursive:true});
    fs.writeFileSync('tmp/language-state-localized/entries.json',JSON.stringify([{id:'localized-split',title:'Split repeated measures',html:await page.evaluate(()=>window.stateduSplitSnapshots.one_group_rm_anova_results)}]));
   }
   await visit('result');assert.deepEqual(await snapshots(),saved);
   console.log(`PASS: ${lang} assignments/options, 30/60 cases, two-group split main tables and captured result unchanged`);
  }
  assert.deepEqual(errors,[]);
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
