const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto(process.env.STATEDU_UI_TEST_URL||'http://127.0.0.1:43923/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 const rows=['id,time,y,x'];for(let i=0;i<30;i++)rows.push([Math.floor(i/3)+1,i%3,i+1,i+31].join(','));
 await page.locator('#file').setInputFiles({name:'longitudinal-model-navigation.csv',mimeType:'text/csv',buffer:Buffer.from(rows.join('\n'))});
 await page.locator('#apply_all_variable_selection').click();
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(800);}
 async function set(key,value){await page.locator('#longitudinal_'+key).selectOption(value);await page.waitForTimeout(500);}
 async function tab(value){await page.locator(`#longitudinal_options_tab a[data-value="${value}"]`).click();await page.waitForTimeout(200);}
 await visit('Longitudinal / Panel Models');
 const roles=[['outcome','y'],['id','id'],['time','time'],['predictors','x']];
 for(const [role,variable]of roles){await page.locator(`[data-input-id="longitudinal_available"] [data-value="${variable}"]`).click();await page.locator(`#longitudinal_${role}_move`).click();await page.locator(`[data-input-id="longitudinal_${role}"] [data-value="${variable}"]`).waitFor();}
 for(const [model,structure,family,check]of [
  ['lmm','exchangeable',null,'residual_normality'],['lmm','reml_un',null,'residual_normality'],['lmm','reml_ar1',null,'residual_normality'],
  ['glmm',null,'binomial','overdispersion'],['glmm',null,'gamma','overdispersion'],['glmm',null,'count','overdispersion'],
  ['panel_fe',null,null,'cross_section'],['panel_re',null,null,'hausman']]){
  await tab('Model');await set('model_type',model);
  if(structure)await set('corstr',structure);if(family)await set('family',family);
  await page.locator('#longitudinal_include_time').uncheck();
  const slope=await page.locator('#longitudinal_random_slope').count();if(slope)await page.locator('#longitudinal_random_slope').check();
  if(family)await page.locator('#longitudinal_exponentiate').uncheck();
  await tab('Checks');await page.locator('#longitudinal_check_'+check).uncheck();await page.waitForTimeout(400);
  for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
   await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(650);await visit('Longitudinal / Panel Models');
   assert.equal(await page.locator('#longitudinal_model_type').inputValue(),model);
   if(structure)assert.equal(await page.locator('#longitudinal_corstr').inputValue(),structure);
   if(family){assert.equal(await page.locator('#longitudinal_family').inputValue(),family);assert.equal(await page.locator('#longitudinal_exponentiate').isChecked(),false);}
   assert.equal(await page.locator('#longitudinal_include_time').isChecked(),false);
   assert.equal(await page.locator('#longitudinal_random_slope').count(),slope);if(slope)assert.equal(await page.locator('#longitudinal_random_slope').isChecked(),true);
   assert.equal(await page.locator('#longitudinal_check_'+check).isChecked(),false);
   assert.equal(await page.locator('#longitudinal_options_tab li.active a').getAttribute('data-value'),'Checks');
   for(const [role,variable]of roles)assert.deepEqual(await page.locator(`[data-input-id="longitudinal_${role}"] [data-value]`).evaluateAll(es=>es.map(e=>e.dataset.value)),[variable]);
   const dict=JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations;
   assert.ok((await page.locator('#longitudinal_setup').textContent()).includes(dict['longitudinal.ui.independent_variables_count'].replace('%s','1')));
   console.log('PASS model navigation',model,structure||family||'',lang);
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
