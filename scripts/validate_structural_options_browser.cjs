const assert=require('node:assert/strict'),fs=require('node:fs');
const {chromium}=require(process.env.STATEDU_PLAYWRIGHT_MODULE||'playwright');
(async()=>{const browser=await chromium.launch({channel:'chrome',headless:true});try{
 const page=await browser.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.goto('http://127.0.0.1:43873/?lang=ko');
 await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1&&document.querySelector('#file.shiny-bound-input')&&!document.documentElement.classList.contains('shiny-busy'));
 await page.waitForTimeout(1200);
 await page.locator('#file').setInputFiles({name:'structural.csv',mimeType:'text/csv',buffer:Buffer.from('x1,x2,x3,g\n1,2,3,1\n2,4,5,1\n3,3,4,2\n4,5,6,2\n5,4,6,1\n6,6,7,2')});
 await page.locator('#apply_all_variable_selection').click();
 async function visit(value){await page.locator(`.navbar-nav a[data-value="${value}"]`).first().evaluate(e=>e.click());await page.waitForTimeout(1500);}
 const settings={cfa:{estimator:'MLR',missing:'listwise',reliability_bootstrap:'1000',scale:'variance',mi_holdout_enabled:true,common_method_enabled:true,power_details:'사용자 메모 Normality'},cbsem:{estimator:'MLR',effect_bootstrap:'1000',scale:'variance',invariance_enabled:true,invariance_group:'g',invariance_path_scope:'selected',power_details:'사용자 메모 Normality'},plssem:{estimator:'PLS',pls_bootstrap:'1000',objective:'predictive',power_basis:'other_documented',pls_predict_folds:'5',pls_predict_reps:'20',power_details:'사용자 메모 Normality'}};
 for(const [type,values] of Object.entries(settings)){
  await visit('analysis_structural_'+type);
  await page.waitForSelector('#structural_'+type+'-canvas-root');
  await page.evaluate(({type,values})=>{
   for(const [field,value] of Object.entries(values)){
    const id='structural_'+type+'_'+field,el=document.getElementById(id);
    if(!el)throw new Error('Missing '+id);
    if(el.selectize)el.selectize.setValue(value);
    else if(el.type==='checkbox'){el.checked=value;el.dispatchEvent(new Event('change',{bubbles:true}));}
    else if(el.classList.contains('shiny-input-radiogroup')){const radio=el.querySelector(`input[value="${value}"]`);radio.checked=true;radio.dispatchEvent(new Event('change',{bubbles:true}));}
    else {el.value=value;el.dispatchEvent(new Event('change',{bubbles:true}));}
   }
  },{type,values});await page.waitForTimeout(700);
  console.log('INITIAL',type,await page.evaluate(type=>[document.getElementById('structural_'+type+'_estimator').value,Shiny.shinyapp.$inputValues['structural_'+type+'_estimator']],type));
 }
 for(const lang of ['ja','zh','es','fr','de','vi','en','ko']){
  await visit('about_preferences');await page.locator('#app_language').selectOption(lang);await page.waitForTimeout(900);
  for(const [type,values] of Object.entries(settings)){
   await visit('analysis_structural_'+type);
   const root=page.locator('#structural_'+type+'-canvas-root');
   await page.waitForFunction(({type,lang})=>document.getElementById('structural_'+type+'-canvas-root')?.dataset.language===lang,{type,lang});
   for(const [field,value] of Object.entries(values)){
    const actual=await page.locator('#structural_'+type+'_'+field).evaluate(el=>el.type==='checkbox'?el.checked:el.classList.contains('shiny-input-radiogroup')?el.querySelector('input:checked')?.value:el.value);
    assert.equal(actual,value,`${lang}/${type}/${field}`);
   }
   const label=lang==='en'?'Analysis options':lang==='ko'?'분석 옵션':JSON.parse(fs.readFileSync(`i18n/${lang}.json`,'utf8')).translations['analysis.ui.analysis_options'];
   assert.equal(await root.locator('.custom-model-run-options-title').textContent(),label);
   console.log('PASS:',lang,type,'browser option values, checkbox/radio choices, user text and translated options panel');
  }
 }
 assert.deepEqual(errors,[]);
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
