const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&!document.documentElement.classList.contains('shiny-busy'));
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1500);}
 async function set(field,value){
  const box=page.locator('#structural_automation_'+field).locator('..');
  await box.locator('.selectize-input').click();
  await box.locator(`.selectize-dropdown [data-value="${value}"]`).click();
  await page.waitForTimeout(250);
 }
 await visit('analysis_structural_automation');
 await set('objective','theory');await set('construct','mixed');await set('indicator','ordered');
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(900);await visit('analysis_structural_automation');
  const title=lang==='en'?'SEM Workflow Recommendation':lang==='ko'?'구조방정식 분석 추천':JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations['analysis.ui.sem_workflow_recommendation'];
  await page.waitForFunction(title=>document.querySelector('.structural-automation-launcher')?.parentElement.querySelector('h1')?.textContent===title,title);
  for(const [field,value] of [['objective','theory'],['construct','mixed'],['indicator','ordered']])assert.equal(await page.locator('#structural_automation_'+field).inputValue(),value,`${lang}: ${field} lost on language switch`);
  await page.locator('#structural_automation_start').click();
  await page.waitForSelector('.shiny-notification');
  const warning=lang==='en'?'The current engine does not support ordered indicators combined with composite constructs. Review the construct specification.':lang==='ko'?'순서형 지표와 합성변수를 함께 추정하는 엔진은 현재 지원하지 않습니다. 구성개념 명세를 다시 확인하십시오.':JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations['analysis.ui.the_current_engine_does_not_support_ordered_indicators_combined_with_composite_constructs_review_the_construct_specification'];
  assert.ok((await page.locator('.shiny-notification').allTextContents()).some(t=>t.includes(warning)),`${lang}: warning`);
  assert.ok(await page.locator('.structural-automation-launcher').isVisible());
  for(const [objective,construct,target] of [['measurement','common_factor','cfa'],['theory','common_factor','cbsem'],['prediction','composite','plssem']]){
   await set('objective',objective);await set('construct',construct);await set('indicator','continuous');
   await page.locator('#structural_automation_start').click();
   await page.waitForFunction(target=>document.querySelector(`.tab-pane.active[data-value="analysis_structural_${target}"]`),target);
   await visit('analysis_structural_automation');
   assert.equal(await page.locator('#structural_automation_objective').inputValue(),objective);
  }
  await set('objective','theory');await set('construct','mixed');await set('indicator','ordered');
  console.log('PASS:',lang,'browser choices preserved, translated warning, CFA/SEM/PLS navigation and return');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
