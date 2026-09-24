const assert=require('node:assert/strict');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto(process.env.STATEDU_UI_TEST_URL||'http://127.0.0.1:43923/?lang=ko');
 await page.waitForSelector('#file.shiny-bound-input');await page.waitForTimeout(1500);
 for(const lang of ['ko','ja','zh','es','fr','de','vi','en','ko']){
  for(const value of ['about_preferences','Longitudinal / Panel Models','result','data_editor_cases','Longitudinal / Panel Models']){
   await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(900);
   const state=await page.evaluate(()=>({value:Shiny.shinyapp.$inputValues.main_menu,active:[...document.querySelectorAll('.navbar-nav li:not(.dropdown).active > a')].map(e=>({value:e.dataset.value,toggle:e.dataset.toggle}))}));
   console.log(lang,value,JSON.stringify(state));assert.equal(state.value,value);assert.equal(state.active.length,1);
   if(value==='about_preferences'){await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(700);}
  }
 }
 assert.deepEqual(errors,[]);
 console.log('PASS all 45 first-click menu transitions; no JavaScript page errors');
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
