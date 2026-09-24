const assert=require('node:assert/strict');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);
 await page.locator('#file').setInputFiles({name:'design.csv',mimeType:'text/csv',buffer:Buffer.from('stratum,psu,weight,x\n1,1,1.1,2\n1,2,1.2,3\n2,3,1.3,4\n2,4,1.4,5')});
 await page.locator('#apply_all_variable_selection').click();
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(700);}
 await visit('analysis_complex_design');
 for(const [field,value] of [['strata','stratum'],['cluster','psu'],['weight','weight'],['variance_method','taylor'],['lonely_psu','average']]){
  await page.locator('#complex_design_'+field).selectOption(value);await page.waitForTimeout(350);
 }
 const labels={ja:'Taylor線形化',zh:'Taylor线性化',es:'Linealización de Taylor',fr:'Linéarisation de Taylor',de:'Taylor-Linearisierung',vi:'Tuyến tính hóa Taylor',en:'Taylor linearization',ko:'Taylor 선형화'};
 console.log('BEFORE',await page.evaluate(()=>Object.fromEntries(['strata','cluster','weight','variance_method','lonely_psu'].map(f=>[f,[document.querySelector('#complex_design_'+f)?.value,Shiny.shinyapp.$inputValues['complex_design_'+f]]]))));
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');if(!await page.locator('#app_language').isVisible())await visit('about_preferences');
  await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(1000);await visit('analysis_complex_design');
  await page.waitForFunction(label=>document.querySelector('#complex_design_variance_method option[value="taylor"]')?.textContent===label,labels[lang]);
  for(const [field,value] of [['strata','stratum'],['cluster','psu'],['weight','weight'],['variance_method','taylor'],['lonely_psu','average']])assert.equal(await page.locator('#complex_design_'+field).inputValue(),value);
  console.log('PASS:',lang,'actual browser language switch preserves design choices');
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
